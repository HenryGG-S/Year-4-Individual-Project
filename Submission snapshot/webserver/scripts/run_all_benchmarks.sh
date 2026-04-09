#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WEBSERVER_DIR="$ROOT"
BASELINES_DIR="$ROOT/comparison_baselines"
LOG_DIR="$ROOT/bench/server_logs"
SESSIONS_DIR="$ROOT/bench/sessions"
mkdir -p "$LOG_DIR" "$SESSIONS_DIR"

LEAN_URL="http://127.0.0.1:8080"
WARP_URL="http://127.0.0.1:8081"
FLASK_URL="http://127.0.0.1:8082"
NGINX_URL="http://127.0.0.1:8083"
GO_URL="http://127.0.0.1:8084"

LEAN_LOG="$LOG_DIR/lean_server.log"
WARP_LOG="$LOG_DIR/warp_server.log"
FLASK_LOG="$LOG_DIR/flask_gunicorn.log"
NGINX_LOG="$LOG_DIR/nginx.log"
GO_LOG="$LOG_DIR/go_server.log"
BUILD_LOG="$LOG_DIR/build.log"
INIT_LOG="$LOG_DIR/init.log"
FLASK_PROVISION_LOG="$LOG_DIR/flask_provision.log"

RUN_FLASK=1
RUN_NGINX=1
RUN_GO=1
BUILD_ONLY=0
REPEATS=10
TRIM_COUNT=2
HEARTBEAT_SECS=15

JSON_RATE=2000
FILE50K_RATE=300
FILE1M_RATE=50
DURATION=60
THREADS=2
CONNS_JSON=100
CONNS_FILE50K=100
CONNS_FILE1M=50

usage() {
  cat <<USAGE
Usage: $0 [options]

Options:
  --build-only         Build/check everything, but do not start servers or run benchmarks
  --skip-flask         Skip Flask + Gunicorn baseline
  --skip-nginx         Skip nginx baseline
  --skip-go            Skip Go baseline
  --repeats N          Number of repetitions per benchmark condition (default: $REPEATS)
  --trim-count N       Number of low/high values to trim per metric (default: $TRIM_COUNT)
  --json-rate N        Requests/sec for /json    (default: $JSON_RATE)
  --file50k-rate N     Requests/sec for /file50k (default: $FILE50K_RATE)
  --file1m-rate N      Requests/sec for /file1m  (default: $FILE1M_RATE)
  --duration S         Benchmark duration in seconds (default: $DURATION)
  --threads N          wrk2 threads (default: $THREADS)
  --heartbeat-secs N   Progress heartbeat interval (default: $HEARTBEAT_SECS)
  --help               Show this help
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --build-only) BUILD_ONLY=1; shift ;;
    --skip-flask) RUN_FLASK=0; shift ;;
    --skip-nginx) RUN_NGINX=0; shift ;;
    --skip-go) RUN_GO=0; shift ;;
    --repeats) REPEATS="$2"; shift 2 ;;
    --trim-count) TRIM_COUNT="$2"; shift 2 ;;
    --json-rate) JSON_RATE="$2"; shift 2 ;;
    --file50k-rate) FILE50K_RATE="$2"; shift 2 ;;
    --file1m-rate) FILE1M_RATE="$2"; shift 2 ;;
    --duration) DURATION="$2"; shift 2 ;;
    --threads) THREADS="$2"; shift 2 ;;
    --heartbeat-secs) HEARTBEAT_SECS="$2"; shift 2 ;;
    --help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

die() { echo "Error: $*" >&2; exit 1; }
need_cmd() { command -v "$1" >/dev/null 2>&1 || die "$1 not found"; }

timestamp() { date '+%Y-%m-%d %H:%M:%S'; }
log_msg() { printf '%s %s\n' "[$(timestamp)]" "$*"; }

fmt_duration() {
  local total="$1"
  local h=$(( total / 3600 ))
  local m=$(( (total % 3600) / 60 ))
  local s=$(( total % 60 ))
  if (( h > 0 )); then
    printf '%02dh:%02dm:%02ds' "$h" "$m" "$s"
  elif (( m > 0 )); then
    printf '%02dm:%02ds' "$m" "$s"
  else
    printf '%02ds' "$s"
  fi
}

sanitize_line() {
  tr '\r' ' ' | sed 's/[[:space:]]\+/ /g' | sed 's/^ //; s/ $//' | cut -c1-180
}

run_logged_command() {
  local label="$1"
  local log_file="$2"
  shift 2

  mkdir -p "$(dirname "$log_file")"
  : > "$log_file"

  log_msg "[$label] started; log: $log_file"
  "$@" >>"$log_file" 2>&1 &
  local pid=$!
  local started=$SECONDS

  while kill -0 "$pid" >/dev/null 2>&1; do
    sleep "$HEARTBEAT_SECS"
    if ! kill -0 "$pid" >/dev/null 2>&1; then
      break
    fi

    local elapsed=$((SECONDS - started))
    local lines=0
    local tail_msg=""
    if [[ -f "$log_file" ]]; then
      lines=$(wc -l < "$log_file" | tr -d ' ')
    fi
    if [[ -s "$log_file" ]]; then
      tail_msg=$(tail -n 1 "$log_file" | sanitize_line)
    fi

    if [[ -n "$tail_msg" ]]; then
      log_msg "[$label] still running after $(fmt_duration "$elapsed"); log lines=$lines; last: $tail_msg"
    else
      log_msg "[$label] still running after $(fmt_duration "$elapsed"); log lines=$lines"
    fi
  done

  local rc=0
  wait "$pid" || rc=$?
  local elapsed=$((SECONDS - started))

  if (( rc != 0 )); then
    log_msg "[$label] failed after $(fmt_duration "$elapsed"); showing log tail"
    tail -n 40 "$log_file" >&2 || true
    return "$rc"
  fi

  log_msg "[$label] completed in $(fmt_duration "$elapsed")"
}

corpus_matches() {
  local main_dir="$WEBSERVER_DIR/bench_files"
  local baseline_dir="$BASELINES_DIR/bench_files"
  local files=(json1k.json file50k.bin file1m.bin)

  for f in "${files[@]}"; do
    [[ -f "$main_dir/$f" ]] || return 1
    [[ -f "$baseline_dir/$f" ]] || return 1
  done

  if ! command -v sha256sum >/dev/null 2>&1; then
    return 0
  fi

  for f in "${files[@]}"; do
    local main_hash baseline_hash
    main_hash="$(sha256sum "$main_dir/$f" | awk '{print $1}')"
    baseline_hash="$(sha256sum "$baseline_dir/$f" | awk '{print $1}')"
    [[ "$main_hash" == "$baseline_hash" ]] || return 1
  done
}

verify_bench_corpus() {
  local main_dir="$WEBSERVER_DIR/bench_files"
  local baseline_dir="$BASELINES_DIR/bench_files"
  local files=(json1k.json file50k.bin file1m.bin)

  for f in "${files[@]}"; do
    [[ -f "$main_dir/$f" ]] || die "Missing benchmark corpus file: $main_dir/$f"
    [[ -f "$baseline_dir/$f" ]] || die "Missing benchmark corpus file: $baseline_dir/$f"
  done

  if ! command -v sha256sum >/dev/null 2>&1; then
    log_msg "[corpus] sha256sum not available; skipping corpus hash verification"
    return 0
  fi

  for f in "${files[@]}"; do
    local main_hash baseline_hash
    main_hash="$(sha256sum "$main_dir/$f" | awk '{print $1}')"
    baseline_hash="$(sha256sum "$baseline_dir/$f" | awk '{print $1}')"
    [[ "$main_hash" == "$baseline_hash" ]] || die "Benchmark corpus mismatch for $f"
    log_msg "[corpus] $f $main_hash"
  done
}

ensure_baselines_ready() {
  local create_args=()
  [[ -x "$ROOT/create_comparison_baselines.sh" ]] || die "create_comparison_baselines.sh not executable"

  if [[ "$RUN_FLASK" -eq 0 ]]; then
    create_args+=(--no-provision-flask)
  fi

  local need_regen=0
  [[ -d "$BASELINES_DIR" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/flask/app.py" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/flask/run_gunicorn.sh" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/flask/requirements.txt" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/nginx/nginx.conf" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/go/main.go" ]] || need_regen=1
  [[ -f "$BASELINES_DIR/scripts/render_nginx_conf.sh" ]] || need_regen=1

  if (( need_regen == 1 )); then
    run_logged_command "init" "$INIT_LOG" "$ROOT/create_comparison_baselines.sh" "${create_args[@]}" "$BASELINES_DIR"
  elif ! corpus_matches; then
    log_msg "[init] benchmark corpus out of sync; regenerating baselines"
    run_logged_command "init" "$INIT_LOG" "$ROOT/create_comparison_baselines.sh" "${create_args[@]}" "$BASELINES_DIR"
  else
    log_msg "[init] comparison baselines already present and corpus is in sync"
  fi

  if [[ "$RUN_FLASK" -eq 1 ]]; then
    run_logged_command "flask-provision" "$FLASK_PROVISION_LOG" "$BASELINES_DIR/flask/run_gunicorn.sh" --ensure-only
  fi
}

wait_for_http() {
  local name="$1"
  local url="$2"
  local log_file="$3"
  local pid="$4"

  local started=$SECONDS
  local attempts=0

  while (( attempts < 120 )); do
    if curl -fsS "$url" >/dev/null 2>&1; then
      log_msg "[$name] ready after $(fmt_duration $((SECONDS - started)))"
      return 0
    fi

    if ! kill -0 "$pid" >/dev/null 2>&1; then
      log_msg "[$name] process exited before becoming ready; showing log tail"
      tail -n 40 "$log_file" >&2 || true
      return 1
    fi

    attempts=$((attempts + 1))
    if (( attempts % 20 == 0 )); then
      local tail_msg=""
      if [[ -s "$log_file" ]]; then
        tail_msg=$(tail -n 1 "$log_file" | sanitize_line)
      fi
      if [[ -n "$tail_msg" ]]; then
        log_msg "[$name] waiting for readiness after $(fmt_duration $((SECONDS - started))); last log line: $tail_msg"
      else
        log_msg "[$name] waiting for readiness after $(fmt_duration $((SECONDS - started)))"
      fi
    fi
    sleep 0.25
  done

  log_msg "[$name] failed to become ready within timeout; showing log tail"
  tail -n 40 "$log_file" >&2 || true
  return 1
}

capture_environment() {
  local out="$1"
  {
    echo "session_started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "stack_version=$(stack --version | head -n 1)"
    echo "python3_version=$(python3 --version 2>&1)"
    echo "wrk2_path=$(command -v wrk2)"
    if [[ "$RUN_NGINX" -eq 1 ]]; then
      echo "nginx_version=$(nginx -v 2>&1)"
    fi
    if [[ "$RUN_GO" -eq 1 ]]; then
      echo "go_version=$(go version 2>&1)"
    fi
    if [[ "$RUN_FLASK" -eq 1 ]]; then
      echo "flask_python=$($BASELINES_DIR/flask/.venv/bin/python --version 2>&1)"
      echo "flask_packages=$($BASELINES_DIR/flask/.venv/bin/python -m pip freeze | tr '\n' ' ')"
    fi
  } > "$out"
}

cleanup() {
  set +e
  echo
  log_msg "[cleanup] stopping benchmark servers"

  if [[ -n "${NGINX_PID:-}" ]]; then
    (
      cd "$BASELINES_DIR/nginx" &&
      nginx -p "$PWD" -c nginx.rendered.conf -s stop
    ) >/dev/null 2>&1 || true
  fi

  [[ -n "${GO_PID:-}" ]] && kill "$GO_PID" >/dev/null 2>&1 || true
  [[ -n "${FLASK_PID:-}" ]] && kill "$FLASK_PID" >/dev/null 2>&1 || true
  [[ -n "${WARP_PID:-}" ]] && kill "$WARP_PID" >/dev/null 2>&1 || true
  [[ -n "${LEAN_PID:-}" ]] && kill "$LEAN_PID" >/dev/null 2>&1 || true

  wait >/dev/null 2>&1 || true
  log_msg "[cleanup] logs saved under: $LOG_DIR"
}
trap cleanup EXIT INT TERM

need_cmd curl
need_cmd stack
need_cmd wrk2
need_cmd python3
[[ -x "$ROOT/scripts/bench.sh" ]] || die "scripts/bench.sh not executable (chmod +x scripts/bench.sh)"
[[ -f "$ROOT/scripts/summarize_trimmed.py" ]] || die "scripts/summarize_trimmed.py missing"

if [[ "$RUN_NGINX" -eq 1 ]]; then
  need_cmd nginx
fi

if [[ "$RUN_GO" -eq 1 ]]; then
  need_cmd go
fi

log_msg "[0/8] preparing comparison baselines"
ensure_baselines_ready

log_msg "[1/8] building Haskell executables"
run_logged_command "build" "$BUILD_LOG" bash -lc "cd '$WEBSERVER_DIR' && stack setup && stack --hpack-force build :webserver-exe :warp-baseline"

if [[ "$BUILD_ONLY" -eq 1 ]]; then
  verify_bench_corpus
  log_msg "Build/check completed successfully."
  exit 0
fi

SESSION_ID="$(date +%Y%m%d_%H%M%S)"
SESSION_DIR="$SESSIONS_DIR/$SESSION_ID"
mkdir -p "$SESSION_DIR"
RAW_CSV="$SESSION_DIR/raw_runs.csv"
SUMMARY_CSV="$SESSION_DIR/trimmed_summary.csv"
ENV_TXT="$SESSION_DIR/environment.txt"
BENCH_LOG_DIR="$SESSION_DIR/bench_logs"
mkdir -p "$BENCH_LOG_DIR"

capture_environment "$ENV_TXT"
log_msg "[session] directory: $SESSION_DIR"
log_msg "[session] environment: $ENV_TXT"

log_msg "[2/8] starting custom server"
: > "$LEAN_LOG"
( cd "$WEBSERVER_DIR" && exec stack exec -- webserver-exe ) >>"$LEAN_LOG" 2>&1 &
LEAN_PID=$!

log_msg "[2/8] starting Warp baseline"
: > "$WARP_LOG"
( cd "$WEBSERVER_DIR" && exec stack exec -- warp-baseline ) >>"$WARP_LOG" 2>&1 &
WARP_PID=$!

if [[ "$RUN_FLASK" -eq 1 ]]; then
  log_msg "[2/8] starting Flask + Gunicorn baseline"
  : > "$FLASK_LOG"
  ( cd "$BASELINES_DIR/flask" && exec ./run_gunicorn.sh ) >>"$FLASK_LOG" 2>&1 &
  FLASK_PID=$!
fi

if [[ "$RUN_NGINX" -eq 1 ]]; then
  log_msg "[2/8] rendering and starting nginx baseline"
  : > "$NGINX_LOG"
  (
    cd "$BASELINES_DIR/nginx"
    ../scripts/render_nginx_conf.sh
    exec nginx -p "$PWD" -c nginx.rendered.conf -g 'daemon off;'
  ) >>"$NGINX_LOG" 2>&1 &
  NGINX_PID=$!
fi

if [[ "$RUN_GO" -eq 1 ]]; then
  log_msg "[2/8] starting Go net/http baseline"
  : > "$GO_LOG"
  ( cd "$BASELINES_DIR/go" && exec go run . ) >>"$GO_LOG" 2>&1 &
  GO_PID=$!
fi

log_msg "[3/8] waiting for servers to become ready"
wait_for_http "custom" "$LEAN_URL/health" "$LEAN_LOG" "$LEAN_PID" || die "Custom server did not become ready"
wait_for_http "warp" "$WARP_URL/health" "$WARP_LOG" "$WARP_PID" || die "Warp baseline did not become ready"
if [[ "$RUN_FLASK" -eq 1 ]]; then
  wait_for_http "flask" "$FLASK_URL/health" "$FLASK_LOG" "$FLASK_PID" || die "Flask baseline did not become ready"
fi
if [[ "$RUN_NGINX" -eq 1 ]]; then
  wait_for_http "nginx" "$NGINX_URL/health" "$NGINX_LOG" "$NGINX_PID" || die "nginx baseline did not become ready"
fi
if [[ "$RUN_GO" -eq 1 ]]; then
  wait_for_http "go" "$GO_URL/health" "$GO_LOG" "$GO_PID" || die "Go baseline did not become ready"
fi

log_msg "[3b/8] verifying benchmark corpus"
verify_bench_corpus

build_cases_for_rep() {
  local rep="$1"
  local cases=()
  local total_cases
  local offset

  cases+=("lean_json|$LEAN_URL/json|$JSON_RATE|$CONNS_JSON")
  cases+=("warp_json|$WARP_URL/json|$JSON_RATE|$CONNS_JSON")
  [[ "$RUN_FLASK" -eq 1 ]] && cases+=("flask_json|$FLASK_URL/json|$JSON_RATE|$CONNS_JSON")
  [[ "$RUN_NGINX" -eq 1 ]] && cases+=("nginx_json|$NGINX_URL/json|$JSON_RATE|$CONNS_JSON")
  [[ "$RUN_GO" -eq 1 ]] && cases+=("go_json|$GO_URL/json|$JSON_RATE|$CONNS_JSON")

  cases+=("lean_file50k|$LEAN_URL/file50k|$FILE50K_RATE|$CONNS_FILE50K")
  cases+=("warp_file50k|$WARP_URL/file50k|$FILE50K_RATE|$CONNS_FILE50K")
  [[ "$RUN_FLASK" -eq 1 ]] && cases+=("flask_file50k|$FLASK_URL/file50k|$FILE50K_RATE|$CONNS_FILE50K")
  [[ "$RUN_NGINX" -eq 1 ]] && cases+=("nginx_file50k|$NGINX_URL/file50k|$FILE50K_RATE|$CONNS_FILE50K")
  [[ "$RUN_GO" -eq 1 ]] && cases+=("go_file50k|$GO_URL/file50k|$FILE50K_RATE|$CONNS_FILE50K")

  cases+=("lean_file1m|$LEAN_URL/file1m|$FILE1M_RATE|$CONNS_FILE1M")
  cases+=("warp_file1m|$WARP_URL/file1m|$FILE1M_RATE|$CONNS_FILE1M")
  [[ "$RUN_FLASK" -eq 1 ]] && cases+=("flask_file1m|$FLASK_URL/file1m|$FILE1M_RATE|$CONNS_FILE1M")
  [[ "$RUN_NGINX" -eq 1 ]] && cases+=("nginx_file1m|$NGINX_URL/file1m|$FILE1M_RATE|$CONNS_FILE1M")
  [[ "$RUN_GO" -eq 1 ]] && cases+=("go_file1m|$GO_URL/file1m|$FILE1M_RATE|$CONNS_FILE1M")

  total_cases=${#cases[@]}
  offset=$(( (rep - 1) % total_cases ))

  for ((i=0; i<total_cases; i++)); do
    echo "${cases[$(((i + offset) % total_cases))]}"
  done
}

count_cases_per_rep() {
  local count=0
  count=$((count + 2))
  [[ "$RUN_FLASK" -eq 1 ]] && count=$((count + 1))
  [[ "$RUN_NGINX" -eq 1 ]] && count=$((count + 1))
  [[ "$RUN_GO" -eq 1 ]] && count=$((count + 1))
  count=$((count * 3))
  echo "$count"
}

run_bench() {
  local bench_index="$1"
  local bench_total="$2"
  local name="$3"
  local url="$4"
  local rate="$5"
  local conns="$6"
  local run_index="$7"
  local log_file="$BENCH_LOG_DIR/$(printf '%03d' "$bench_index")_${name}_rep${run_index}.log"

  log_msg "[bench $bench_index/$bench_total] starting $name (rep $run_index/$REPEATS, rate=$rate, conns=$conns, duration=${DURATION}s)"
  run_logged_command "bench $bench_index/$bench_total $name rep $run_index" "$log_file" \
    "$ROOT/scripts/bench.sh" \
      --name "$name" \
      --url "$url" \
      --rate "$rate" \
      --duration "$DURATION" \
      --threads "$THREADS" \
      --conns "$conns" \
      --run-index "$run_index" \
      --raw-csv "$RAW_CSV"
}

CASES_PER_REP="$(count_cases_per_rep)"
TOTAL_BENCH_RUNS=$((CASES_PER_REP * REPEATS))
ESTIMATED_SECONDS=$((TOTAL_BENCH_RUNS * DURATION))
log_msg "[4/8] running benchmarks: $TOTAL_BENCH_RUNS cases total across $REPEATS repetitions"
log_msg "[4/8] lower-bound benchmark time is about $(fmt_duration "$ESTIMATED_SECONDS") plus setup/summary"

BENCH_INDEX=0
for ((rep=1; rep<=REPEATS; rep++)); do
  log_msg "[rep $rep/$REPEATS] beginning"
  while IFS='|' read -r name url rate conns; do
    [[ -z "$name" ]] && continue
    BENCH_INDEX=$((BENCH_INDEX + 1))
    run_bench "$BENCH_INDEX" "$TOTAL_BENCH_RUNS" "$name" "$url" "$rate" "$conns" "$rep"
  done < <(build_cases_for_rep "$rep")
done

log_msg "[5/8] computing trimmed summary"
SUMMARY_LOG="$SESSION_DIR/summary.log"
run_logged_command "summary" "$SUMMARY_LOG" python3 "$ROOT/scripts/summarize_trimmed.py" \
  --input "$RAW_CSV" \
  --output "$SUMMARY_CSV" \
  --trim-count "$TRIM_COUNT"

cp "$RAW_CSV" "$ROOT/bench/raw_runs_latest.csv"
cp "$SUMMARY_CSV" "$ROOT/bench/trimmed_summary_latest.csv"
cp "$ENV_TXT" "$ROOT/bench/environment_latest.txt"

log_msg "[6/8] done"
log_msg "Session directory: $SESSION_DIR"
log_msg "Raw runs CSV:     $RAW_CSV"
log_msg "Trimmed summary:  $SUMMARY_CSV"
log_msg "Environment:      $ENV_TXT"
log_msg "Latest raw copy:  $ROOT/bench/raw_runs_latest.csv"
log_msg "Latest summary:   $ROOT/bench/trimmed_summary_latest.csv"
log_msg "Latest env copy:  $ROOT/bench/environment_latest.txt"
