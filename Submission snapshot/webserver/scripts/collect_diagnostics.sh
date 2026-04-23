#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

HOST="${HOST:-127.0.0.1}"
OUT_BASE="${OUT_BASE:-bench/diagnostics}"
EVENTLOG_DURATION="${EVENTLOG_DURATION:-60}"
PROFILE_DURATION="${PROFILE_DURATION:-30}"
TS="$(date +%Y%m%d_%H%M%S)"
OUT_DIR="$ROOT/$OUT_BASE/$TS"
EVENTLOG_DIR="$OUT_DIR/eventlogs"
PROFILE_DIR="$OUT_DIR/profiles"
RUNS_COPY_DIR="$OUT_DIR/benchmark_runs"
LOG_DIR="$OUT_DIR/logs"
CSV_DIR="$OUT_DIR/csv"

EVENTLOG_FILE1M_PORT="${EVENTLOG_FILE1M_PORT:-18080}"
EVENTLOG_JSON_PORT="${EVENTLOG_JSON_PORT:-18081}"
PROFILE_FILE1M_PORT="${PROFILE_FILE1M_PORT:-18082}"
PROFILE_JSON_PORT="${PROFILE_JSON_PORT:-18083}"

mkdir -p "$EVENTLOG_DIR" "$PROFILE_DIR" "$RUNS_COPY_DIR" "$LOG_DIR" "$CSV_DIR"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Error: required command not found: $1" >&2
    exit 2
  fi
}

require_cmd stack
require_cmd wrk2
require_cmd awk
require_cmd find
require_cmd grep
require_cmd python3

SERVER_PID=""

log() {
  printf '\n[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

cleanup_server() {
  if [[ -n "$SERVER_PID" ]]; then
    if kill -0 "$SERVER_PID" 2>/dev/null; then
      kill "$SERVER_PID" 2>/dev/null || true
      wait "$SERVER_PID" 2>/dev/null || true
    fi
    SERVER_PID=""
  fi
}
trap cleanup_server EXIT

port_is_listening() {
  local host="$1"
  local port="$2"
  python3 - <<PY >/dev/null 2>&1
import socket
s = socket.socket()
s.settimeout(0.5)
try:
    s.connect(("$host", int("$port")))
    ok = True
except OSError:
    ok = False
finally:
    s.close()
raise SystemExit(0 if ok else 1)
PY
}

assert_port_free() {
  local host="$1"
  local port="$2"
  if port_is_listening "$host" "$port"; then
    echo "Error: $host:$port is already in use before launch. Pick a different port or stop the existing server." >&2
    exit 3
  fi
}

wait_for_server_ready() {
  local pid="$1"
  local host="$2"
  local port="$3"
  local stderr_log="$4"
  local timeout_s="${5:-20}"
  local start
  start="$(date +%s)"

  while true; do
    if [[ -f "$stderr_log" ]] && grep -qiE 'address already in use|resource busy|bind:' "$stderr_log"; then
      echo "Error: server failed to bind $host:$port. See $stderr_log" >&2
      return 1
    fi

    if ! kill -0 "$pid" 2>/dev/null; then
      echo "Error: server process exited before becoming ready. See $stderr_log" >&2
      return 1
    fi

    if port_is_listening "$host" "$port"; then
      return 0
    fi

    if (( $(date +%s) - start >= timeout_s )); then
      echo "Error: timed out waiting for server on $host:$port. See $stderr_log" >&2
      return 1
    fi
    sleep 0.25
  done
}

stop_server() {
  cleanup_server
  sleep 1
}

find_one_file() {
  local dir="$1"
  local pattern="$2"
  find "$dir" -maxdepth 1 -type f -name "$pattern" | head -n 1
}

copy_bench_run_dir_from_csv() {
  local csv="$1"
  local dest_root="$2"
  [[ -f "$csv" ]] || return 0
  local run_dir
  run_dir="$(tail -n 1 "$csv" | awk -F',' '{print $NF}')"
  [[ -n "$run_dir" ]] || return 0
  if [[ -d "$run_dir" ]]; then
    cp -a "$run_dir" "$dest_root/"
  fi
}

build_eventlog_binary() {
  log "Building eventlog-capable executable"
  stack build webserver --ghc-options='-O2'
}

build_profile_binary() {
  log "Building profiled executable"
  stack build webserver \
    --profile \
    --library-profiling \
    --executable-profiling \
    --ghc-options='-fprof-auto'

  if ! stack exec --profile webserver-exe -- +RTS --info -RTS >/dev/null 2>&1; then
    echo "Error: 'stack exec --profile webserver-exe' is not working on this setup." >&2
    exit 4
  fi
}

start_eventlog_server() {
  local case_dir="$1"
  local port="$2"
  local stdout_log="$3"
  local stderr_log="$4"

  assert_port_free "$HOST" "$port"
  (
    cd "$case_dir"
    PORT="$port" PROFILE_STAGES=0 stack exec webserver-exe -- +RTS -N -l-au -RTS \
      > "$stdout_log" 2> "$stderr_log"
  ) &
  SERVER_PID=$!
  wait_for_server_ready "$SERVER_PID" "$HOST" "$port" "$stderr_log" 20
}

start_profile_server() {
  local case_dir="$1"
  local port="$2"
  local stdout_log="$3"
  local stderr_log="$4"

  assert_port_free "$HOST" "$port"
  (
    cd "$case_dir"
    PORT="$port" PROFILE_STAGES=0 stack exec --profile webserver-exe -- +RTS -N -p -hc -i0.01 -RTS \
      > "$stdout_log" 2> "$stderr_log"
  ) &
  SERVER_PID=$!
  wait_for_server_ready "$SERVER_PID" "$HOST" "$port" "$stderr_log" 25
}

convert_hp_outputs() {
  local case_dir="$1"
  local hp_file="$2"
  if [[ -z "$hp_file" || ! -f "$hp_file" ]]; then
    return 0
  fi
  if command -v hp2ps >/dev/null 2>&1; then
    (cd "$case_dir" && hp2ps -c "$(basename "$hp_file")") || true
  fi
  local ps_file
  ps_file="$(find_one_file "$case_dir" '*.ps')"
  if [[ -n "$ps_file" && -f "$ps_file" ]] && command -v ps2pdf >/dev/null 2>&1; then
    ps2pdf "$ps_file" "${ps_file%.ps}.pdf" || true
  fi
}

run_eventlog_case() {
  local case_name="$1"
  local endpoint="$2"
  local rate="$3"
  local conns="$4"
  local threads="$5"
  local duration="$6"
  local port="$7"

  local case_dir="$EVENTLOG_DIR/$case_name"
  local csv="$CSV_DIR/eventlog_raw_runs.csv"
  mkdir -p "$case_dir"

  log "Starting eventlog server for $case_name on port $port"
  start_eventlog_server \
    "$case_dir" \
    "$port" \
    "$LOG_DIR/${case_name}_eventlog_server.stdout.log" \
    "$LOG_DIR/${case_name}_eventlog_server.stderr.log"

  log "Running benchmark for $case_name"
  ./scripts/bench.sh \
    --url "http://$HOST:$port$endpoint" \
    --rate "$rate" \
    --duration "$duration" \
    --threads "$threads" \
    --conns "$conns" \
    --name "lean_${case_name}_eventlog" \
    --run-index 1 \
    --raw-csv "$csv"

  log "Stopping eventlog server for $case_name"
  stop_server

  local ev
  ev="$(find_one_file "$case_dir" '*.eventlog')"
  if [[ -z "$ev" ]]; then
    echo "Warning: no .eventlog file found for $case_name in $case_dir" >&2
  else
    mv "$ev" "$case_dir/${case_name}.eventlog"
  fi

  copy_bench_run_dir_from_csv "$csv" "$RUNS_COPY_DIR"
}

run_profile_case() {
  local case_name="$1"
  local endpoint="$2"
  local rate="$3"
  local conns="$4"
  local threads="$5"
  local duration="$6"
  local port="$7"

  local case_dir="$PROFILE_DIR/$case_name"
  local csv="$CSV_DIR/profile_raw_runs.csv"
  mkdir -p "$case_dir"

  log "Starting profiled server for $case_name on port $port"
  start_profile_server \
    "$case_dir" \
    "$port" \
    "$LOG_DIR/${case_name}_profile_server.stdout.log" \
    "$LOG_DIR/${case_name}_profile_server.stderr.log"

  log "Running benchmark for $case_name"
  ./scripts/bench.sh \
    --url "http://$HOST:$port$endpoint" \
    --rate "$rate" \
    --duration "$duration" \
    --threads "$threads" \
    --conns "$conns" \
    --name "lean_${case_name}_profile" \
    --run-index 1 \
    --raw-csv "$csv"

  log "Stopping profiled server for $case_name"
  stop_server

  local prof hp
  prof="$(find_one_file "$case_dir" '*.prof')"
  hp="$(find_one_file "$case_dir" '*.hp')"

  if [[ -n "$prof" && -f "$prof" ]]; then
    mv "$prof" "$case_dir/${case_name}.prof"
  else
    echo "Warning: no .prof file found for $case_name in $case_dir" >&2
  fi

  if [[ -n "$hp" && -f "$hp" ]]; then
    mv "$hp" "$case_dir/${case_name}.hp"
    hp="$case_dir/${case_name}.hp"
  else
    echo "Warning: no .hp file found for $case_name in $case_dir" >&2
    hp=""
  fi

  convert_hp_outputs "$case_dir" "$hp"
  copy_bench_run_dir_from_csv "$csv" "$RUNS_COPY_DIR"
}

write_manifest() {
  cat > "$OUT_DIR/README.txt" <<TXT
Diagnostics run created: $TS
Project root: $ROOT

Ports used:
- file1m eventlog: $EVENTLOG_FILE1M_PORT
- json eventlog:   $EVENTLOG_JSON_PORT
- file1m profile:  $PROFILE_FILE1M_PORT
- json profile:    $PROFILE_JSON_PORT

Contents:
- eventlogs/: eventlog benchmark runs and .eventlog files
- profiles/: profiled runs and .prof/.hp/.ps/.pdf files
- benchmark_runs/: copies of the wrk2 run directories produced by scripts/bench.sh
- csv/: raw CSV summaries for eventlog and profile passes
- logs/: stdout/stderr for server launches
TXT
}

log "Output directory: $OUT_DIR"
write_manifest
build_eventlog_binary
run_eventlog_case file1m /file1m 50 50 2 "$EVENTLOG_DURATION" "$EVENTLOG_FILE1M_PORT"
run_eventlog_case json   /json   2000 100 2 "$EVENTLOG_DURATION" "$EVENTLOG_JSON_PORT"
build_profile_binary
run_profile_case file1m /file1m 50 50 2 "$PROFILE_DURATION" "$PROFILE_FILE1M_PORT"
run_profile_case json   /json   2000 100 2 "$PROFILE_DURATION" "$PROFILE_JSON_PORT"

log "Done. All collected artefacts are under: $OUT_DIR"
log "Inspect eventlogs with: threadscope $EVENTLOG_DIR/file1m/file1m.eventlog"
