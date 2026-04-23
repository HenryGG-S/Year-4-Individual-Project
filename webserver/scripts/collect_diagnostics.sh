#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

HOST="${HOST:-127.0.0.1}"
OUT_BASE="${OUT_BASE:-bench/diagnostics}"
EVENTLOG_DURATION="${EVENTLOG_DURATION:-60}"
PROFILE_DURATION="${PROFILE_DURATION:-30}"
EVENTLOG_EXIT_GRACE="${EVENTLOG_EXIT_GRACE:-10}"
PROFILE_EXIT_GRACE="${PROFILE_EXIT_GRACE:-10}"
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
NORMAL_EXE=""
PROFILE_EXE=""

log() {
  printf '\n[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

cleanup_server() {
  if [[ -n "${SERVER_PID:-}" ]]; then
    if kill -0 "$SERVER_PID" 2>/dev/null; then
      kill -TERM "$SERVER_PID" 2>/dev/null || true
      for _ in {1..12}; do
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
          break
        fi
        sleep 0.25
      done
      if kill -0 "$SERVER_PID" 2>/dev/null; then
        kill -KILL "$SERVER_PID" 2>/dev/null || true
      fi
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

wait_for_server_exit() {
  local pid="$1"
  local timeout_s="$2"
  local start
  start="$(date +%s)"

  while kill -0 "$pid" 2>/dev/null; do
    if (( $(date +%s) - start >= timeout_s )); then
      return 1
    fi
    sleep 0.25
  done

  wait "$pid" 2>/dev/null || true
  return 0
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

resolve_normal_exe() {
  stack exec -- which webserver-exe | tail -n 1
}

resolve_profile_exe() {
  stack exec --profile -- which webserver-exe | tail -n 1
}

build_eventlog_binary() {
  log "Building eventlog-capable executable"
  stack build webserver --ghc-options='-O2'
  NORMAL_EXE="$(resolve_normal_exe)"
  [[ -n "$NORMAL_EXE" && -x "$NORMAL_EXE" ]] || {
    echo "Error: could not resolve normal executable path" >&2
    exit 5
  }
}

build_profile_binary() {
  log "Building profiled executable"
  stack build webserver \
    --profile \
    --library-profiling \
    --executable-profiling \
    --ghc-options='-fprof-auto'
  PROFILE_EXE="$(resolve_profile_exe)"
  [[ -n "$PROFILE_EXE" && -x "$PROFILE_EXE" ]] || {
    echo "Error: could not resolve profiled executable path" >&2
    exit 6
  }
}

start_eventlog_server() {
  local case_dir="$1"
  local port="$2"
  local stdout_log="$3"
  local stderr_log="$4"
  local exit_after="$5"
  local stem="$6"

  assert_port_free "$HOST" "$port"
  (
    cd "$case_dir"
    PORT="$port" \
    DIAG_EXIT_AFTER_SECONDS="$exit_after" \
    PROFILE_STAGES=0 \
    "$NORMAL_EXE" +RTS -N -l -ol"${stem}.eventlog" --eventlog-flush-interval=1 -RTS \
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
  local exit_after="$5"
  local stem="$6"

  assert_port_free "$HOST" "$port"
  (
    cd "$case_dir"
    PORT="$port" \
    DIAG_EXIT_AFTER_SECONDS="$exit_after" \
    PROFILE_STAGES=0 \
    "$PROFILE_EXE" +RTS -N -p -po"${stem}" -hc -i0.01 -RTS \
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

collect_eventlog_outputs() {
  local case_dir="$1"
  local stem="$2"

  local ev
  ev="$(find_one_file "$case_dir" '*.eventlog')"
  if [[ -z "$ev" ]]; then
    echo "Warning: no .eventlog file found in $case_dir" >&2
  elif [[ "$(basename "$ev")" != "${stem}.eventlog" ]]; then
    mv "$ev" "$case_dir/${stem}.eventlog"
  fi
}

collect_profile_outputs() {
  local case_dir="$1"
  local stem="$2"

  local prof hp
  prof="$(find_one_file "$case_dir" '*.prof')"
  hp="$(find_one_file "$case_dir" '*.hp')"

  if [[ -n "$prof" && -f "$prof" ]]; then
    if [[ ! -s "$prof" ]]; then
      echo "Warning: .prof file exists but is empty in $case_dir" >&2
    fi
    if [[ "$(basename "$prof")" != "${stem}.prof" ]]; then
      mv "$prof" "$case_dir/${stem}.prof"
    fi
  else
    echo "Warning: no .prof file found in $case_dir" >&2
  fi

  if [[ -n "$hp" && -f "$hp" ]]; then
    if [[ "$(basename "$hp")" != "${stem}.hp" ]]; then
      mv "$hp" "$case_dir/${stem}.hp"
      hp="$case_dir/${stem}.hp"
    fi
  else
    echo "Warning: no .hp file found in $case_dir" >&2
    hp=""
  fi

  convert_hp_outputs "$case_dir" "$hp"
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
  local exit_after=$(( duration + EVENTLOG_EXIT_GRACE ))
  mkdir -p "$case_dir"

  log "Starting eventlog server for $case_name on port $port"
  start_eventlog_server \
    "$case_dir" \
    "$port" \
    "$LOG_DIR/${case_name}_eventlog_server.stdout.log" \
    "$LOG_DIR/${case_name}_eventlog_server.stderr.log" \
    "$exit_after" \
    "$case_name"

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

  log "Waiting for eventlog server for $case_name to exit normally"
  if ! wait_for_server_exit "$SERVER_PID" $(( EVENTLOG_EXIT_GRACE + 15 )); then
    echo "Warning: server did not exit on its own for $case_name; forcing shutdown" >&2
    cleanup_server
  else
    SERVER_PID=""
  fi

  collect_eventlog_outputs "$case_dir" "$case_name"
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
  local exit_after=$(( duration + PROFILE_EXIT_GRACE ))
  mkdir -p "$case_dir"

  log "Starting profiled server for $case_name on port $port"
  start_profile_server \
    "$case_dir" \
    "$port" \
    "$LOG_DIR/${case_name}_profile_server.stdout.log" \
    "$LOG_DIR/${case_name}_profile_server.stderr.log" \
    "$exit_after" \
    "$case_name"

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

  log "Waiting for profiled server for $case_name to exit normally"
  if ! wait_for_server_exit "$SERVER_PID" $(( PROFILE_EXIT_GRACE + 15 )); then
    echo "Warning: server did not exit on its own for $case_name; forcing shutdown" >&2
    cleanup_server
  else
    SERVER_PID=""
  fi

  collect_profile_outputs "$case_dir" "$case_name"
  copy_bench_run_dir_from_csv "$csv" "$RUNS_COPY_DIR"
}

write_manifest() {
  cat > "$OUT_DIR/README.txt" <<TXT
Diagnostics run created: $TS
Project root: $ROOT

Executables:
- normal:   $NORMAL_EXE
- profiled: $PROFILE_EXE

Ports used:
- file1m eventlog: $EVENTLOG_FILE1M_PORT
- json eventlog:   $EVENTLOG_JSON_PORT
- file1m profile:  $PROFILE_FILE1M_PORT
- json profile:    $PROFILE_JSON_PORT

Exit timers:
- eventlog cases: duration + ${EVENTLOG_EXIT_GRACE}s
- profile cases:  duration + ${PROFILE_EXIT_GRACE}s

Contents:
- eventlogs/: eventlog benchmark runs and .eventlog files
- profiles/: profiled runs and .prof/.hp/.ps/.pdf files
- benchmark_runs/: copies of the wrk2 run directories produced by scripts/bench.sh
- csv/: raw CSV summaries for eventlog and profile passes
- logs/: stdout/stderr for server launches
TXT
}

log "Output directory: $OUT_DIR"
build_eventlog_binary
build_profile_binary
write_manifest
run_eventlog_case file1m /file1m 50 50 2 "$EVENTLOG_DURATION" "$EVENTLOG_FILE1M_PORT"
run_eventlog_case json   /json   2000 100 2 "$EVENTLOG_DURATION" "$EVENTLOG_JSON_PORT"
run_profile_case file1m /file1m 50 50 2 "$PROFILE_DURATION" "$PROFILE_FILE1M_PORT"
run_profile_case json   /json   2000 100 2 "$PROFILE_DURATION" "$PROFILE_JSON_PORT"

log "Done. All collected artefacts are under: $OUT_DIR"
log "Inspect eventlogs with: threadscope $EVENTLOG_DIR/file1m/file1m.eventlog"
