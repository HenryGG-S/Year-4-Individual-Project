#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WEBSERVER_DIR="$ROOT"
BASELINES_DIR="$ROOT/comparison_baselines"
LOG_DIR="$ROOT/bench/server_logs"
mkdir -p "$LOG_DIR"

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

RUN_FLASK=1
RUN_NGINX=1
RUN_GO=1
BUILD_ONLY=0

JSON_RATE=2000
FILE50K_RATE=300
FILE1M_RATE=50
DURATION=60
THREADS=2
CONNS_JSON=100
CONNS_FILE50K=100
CONNS_FILE1M=50

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  --build-only       Build/check everything, but do not start servers or run benchmarks
  --skip-flask       Skip Flask + Gunicorn baseline
  --skip-nginx       Skip nginx baseline
  --skip-go          Skip Go baseline
  --json-rate N      Requests/sec for /json    (default: $JSON_RATE)
  --file50k-rate N   Requests/sec for /file50k (default: $FILE50K_RATE)
  --file1m-rate N    Requests/sec for /file1m  (default: $FILE1M_RATE)
  --duration S       Benchmark duration in seconds (default: $DURATION)
  --threads N        wrk2 threads (default: $THREADS)
  --help             Show this help
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --build-only) BUILD_ONLY=1; shift ;;
    --skip-flask) RUN_FLASK=0; shift ;;
    --skip-nginx) RUN_NGINX=0; shift ;;
    --skip-go) RUN_GO=0; shift ;;
    --json-rate) JSON_RATE="$2"; shift 2 ;;
    --file50k-rate) FILE50K_RATE="$2"; shift 2 ;;
    --file1m-rate) FILE1M_RATE="$2"; shift 2 ;;
    --duration) DURATION="$2"; shift 2 ;;
    --threads) THREADS="$2"; shift 2 ;;
    --help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

die() { echo "Error: $*" >&2; exit 1; }
need_cmd() { command -v "$1" >/dev/null 2>&1 || die "$1 not found"; }

wait_for_http() {
  local name="$1"
  local url="$2"
  local log="$3"

  for _ in {1..120}; do
    if curl -fsS "$url" >/dev/null 2>&1; then
      echo "[$name] ready"
      return 0
    fi
    sleep 0.25
  done

  echo "[$name] failed to become ready. Log tail:" >&2
  tail -n 40 "$log" >&2 || true
  return 1
}

cleanup() {
  set +e
  echo
  echo "[cleanup] Stopping benchmark servers..."

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
  echo "[cleanup] Logs saved under: $LOG_DIR"
}
trap cleanup EXIT INT TERM

need_cmd curl
need_cmd stack
need_cmd wrk2
[[ -x "$ROOT/scripts/bench.sh" ]] || die "scripts/bench.sh not executable (chmod +x scripts/bench.sh)"

[[ -d "$BASELINES_DIR" ]] || die "Missing comparison_baselines/. Run ./create_comparison_baselines.sh first."
[[ -f "$BASELINES_DIR/flask/app.py" ]] || die "Missing Flask baseline app"
[[ -f "$BASELINES_DIR/nginx/nginx.conf" ]] || die "Missing nginx config template"
[[ -f "$BASELINES_DIR/go/main.go" ]] || die "Missing Go baseline"

if [[ "$RUN_FLASK" -eq 1 ]]; then
  [[ -x "$BASELINES_DIR/flask/run_gunicorn.sh" ]] || die "Flask runner is not executable"
  [[ -x "$BASELINES_DIR/flask/.venv/bin/gunicorn" ]] || die "Missing Flask virtualenv gunicorn. Run:
cd comparison_baselines/flask
python3 -m venv .venv
source .venv/bin/activate
pip install flask gunicorn"
fi

if [[ "$RUN_NGINX" -eq 1 ]]; then
  need_cmd nginx
fi

if [[ "$RUN_GO" -eq 1 ]]; then
  need_cmd go
fi

echo "[1/7] Building Haskell executables..."
( cd "$WEBSERVER_DIR" && stack --hpack-force build :webserver-exe :warp-baseline ) >/dev/null

if [[ "$BUILD_ONLY" -eq 1 ]]; then
  echo "Build/check completed successfully."
  exit 0
fi

echo "[2/7] Starting custom server..."
: > "$LEAN_LOG"
( cd "$WEBSERVER_DIR" && stack exec -- webserver-exe ) >>"$LEAN_LOG" 2>&1 &
LEAN_PID=$!

echo "[2/7] Starting Warp baseline..."
: > "$WARP_LOG"
( cd "$WEBSERVER_DIR" && stack exec -- warp-baseline ) >>"$WARP_LOG" 2>&1 &
WARP_PID=$!

if [[ "$RUN_FLASK" -eq 1 ]]; then
  echo "[2/7] Starting Flask + Gunicorn baseline..."
  : > "$FLASK_LOG"
  (
    cd "$BASELINES_DIR/flask"
    export PATH="$PWD/.venv/bin:$PATH"
    exec ./run_gunicorn.sh
  ) >>"$FLASK_LOG" 2>&1 &
  FLASK_PID=$!
fi

if [[ "$RUN_NGINX" -eq 1 ]]; then
  echo "[2/7] Rendering and starting nginx baseline..."
  : > "$NGINX_LOG"
  (
    cd "$BASELINES_DIR/nginx"
    ../scripts/render_nginx_conf.sh
    exec nginx -p "$PWD" -c nginx.rendered.conf -g 'daemon off;'
  ) >>"$NGINX_LOG" 2>&1 &
  NGINX_PID=$!
fi

if [[ "$RUN_GO" -eq 1 ]]; then
  echo "[2/7] Starting Go net/http baseline..."
  : > "$GO_LOG"
  (
    cd "$BASELINES_DIR/go"
    exec go run .
  ) >>"$GO_LOG" 2>&1 &
  GO_PID=$!
fi

echo "[3/7] Waiting for servers to become ready..."
wait_for_http "custom" "$LEAN_URL/health" "$LEAN_LOG" || die "Custom server did not become ready"
wait_for_http "warp" "$WARP_URL/health" "$WARP_LOG" || die "Warp baseline did not become ready"

if [[ "$RUN_FLASK" -eq 1 ]]; then
  wait_for_http "flask" "$FLASK_URL/health" "$FLASK_LOG" || die "Flask baseline did not become ready"
fi

if [[ "$RUN_NGINX" -eq 1 ]]; then
  wait_for_http "nginx" "$NGINX_URL/health" "$NGINX_LOG" || die "nginx baseline did not become ready"
fi

if [[ "$RUN_GO" -eq 1 ]]; then
  wait_for_http "go" "$GO_URL/health" "$GO_LOG" || die "Go baseline did not become ready"
fi

run_bench() {
  local name="$1"
  local url="$2"
  local rate="$3"
  local conns="$4"

  "$ROOT/scripts/bench.sh" \
    --name "$name" \
    --url "$url" \
    --rate "$rate" \
    --duration "$DURATION" \
    --threads "$THREADS" \
    --conns "$conns"
}

echo "[4/7] Running /json benchmarks..."
run_bench lean_json  "$LEAN_URL/json"  "$JSON_RATE" "$CONNS_JSON"
run_bench warp_json  "$WARP_URL/json"  "$JSON_RATE" "$CONNS_JSON"
[[ "$RUN_FLASK" -eq 1 ]] && run_bench flask_json "$FLASK_URL/json" "$JSON_RATE" "$CONNS_JSON"
[[ "$RUN_NGINX" -eq 1 ]] && run_bench nginx_json "$NGINX_URL/json" "$JSON_RATE" "$CONNS_JSON"
[[ "$RUN_GO" -eq 1 ]] && run_bench go_json    "$GO_URL/json"    "$JSON_RATE" "$CONNS_JSON"

echo "[5/7] Running /file50k benchmarks..."
run_bench lean_file50k  "$LEAN_URL/file50k"  "$FILE50K_RATE" "$CONNS_FILE50K"
run_bench warp_file50k  "$WARP_URL/file50k"  "$FILE50K_RATE" "$CONNS_FILE50K"
[[ "$RUN_FLASK" -eq 1 ]] && run_bench flask_file50k "$FLASK_URL/file50k" "$FILE50K_RATE" "$CONNS_FILE50K"
[[ "$RUN_NGINX" -eq 1 ]] && run_bench nginx_file50k "$NGINX_URL/file50k" "$FILE50K_RATE" "$CONNS_FILE50K"
[[ "$RUN_GO" -eq 1 ]] && run_bench go_file50k    "$GO_URL/file50k"    "$FILE50K_RATE" "$CONNS_FILE50K"

echo "[6/7] Running /file1m benchmarks..."
run_bench lean_file1m  "$LEAN_URL/file1m"  "$FILE1M_RATE" "$CONNS_FILE1M"
run_bench warp_file1m  "$WARP_URL/file1m"  "$FILE1M_RATE" "$CONNS_FILE1M"
[[ "$RUN_FLASK" -eq 1 ]] && run_bench flask_file1m "$FLASK_URL/file1m" "$FILE1M_RATE" "$CONNS_FILE1M"
[[ "$RUN_NGINX" -eq 1 ]] && run_bench nginx_file1m "$NGINX_URL/file1m" "$FILE1M_RATE" "$CONNS_FILE1M"
[[ "$RUN_GO" -eq 1 ]] && run_bench go_file1m    "$GO_URL/file1m"    "$FILE1M_RATE" "$CONNS_FILE1M"

echo "[7/7] Done."
echo "Summary CSV: $ROOT/bench/summary.csv"
