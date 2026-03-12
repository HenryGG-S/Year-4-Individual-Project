#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WEBSERVER_DIR="$ROOT"
LOG_DIR="$ROOT/bench/server_logs"
mkdir -p "$LOG_DIR"

LEAN_URL="http://127.0.0.1:8080"
WARP_URL="http://127.0.0.1:8081"

LEAN_LOG="$LOG_DIR/lean_server.log"
WARP_LOG="$LOG_DIR/warp_server.log"

die() { echo "Error: $*" >&2; exit 1; }

command -v curl >/dev/null 2>&1 || die "curl not found"
[[ -x "$ROOT/scripts/bench.sh" ]] || die "scripts/bench.sh not executable (chmod +x scripts/bench.sh)"

# Build once so you don't compile during benchmarks
echo "[1/5] Building..."
( cd "$WEBSERVER_DIR" && stack --hpack-force build ) >/dev/null

cleanup() {
  echo
  echo "[cleanup] Stopping servers..."
  [[ -n "${LEAN_PID:-}" ]] && kill "$LEAN_PID" >/dev/null 2>&1 || true
  [[ -n "${WARP_PID:-}" ]] && kill "$WARP_PID" >/dev/null 2>&1 || true
  wait >/dev/null 2>&1 || true
  echo "[cleanup] Logs: $LEAN_LOG , $WARP_LOG"
}
trap cleanup EXIT INT TERM

echo "[2/5] Starting lean server..."
: > "$LEAN_LOG"
( cd "$WEBSERVER_DIR" && stack run webserver-exe ) >>"$LEAN_LOG" 2>&1 &
LEAN_PID=$!

echo "[2/5] Starting warp baseline..."
: > "$WARP_LOG"
( cd "$WEBSERVER_DIR" && stack run warp-baseline ) >>"$WARP_LOG" 2>&1 &
WARP_PID=$!

echo "[3/5] Waiting for servers to become ready..."
for i in {1..60}; do
  if curl -fsS "$LEAN_URL/health" >/dev/null 2>&1 && curl -fsS "$WARP_URL/health" >/dev/null 2>&1; then
    echo "Servers are up."
    break
  fi
  sleep 0.25
  if [[ "$i" -eq 60 ]]; then
    echo "Lean log tail:"; tail -n 30 "$LEAN_LOG" || true
    echo "Warp log tail:"; tail -n 30 "$WARP_LOG" || true
    die "Servers did not become ready (ports 8080/8081)."
  fi
done

echo "[4/5] Running benchmarks..."
# Adjust rates as you like. Keep them small at first, then sweep.
"$ROOT/scripts/bench.sh" --name lean_json    --url "$LEAN_URL/json"    --rate 2000 --duration 60 --threads 2 --conns 100
"$ROOT/scripts/bench.sh" --name warp_json    --url "$WARP_URL/json"    --rate 2000 --duration 60 --threads 2 --conns 100

"$ROOT/scripts/bench.sh" --name lean_file50k --url "$LEAN_URL/file50k" --rate 300  --duration 60 --threads 2 --conns 100
"$ROOT/scripts/bench.sh" --name warp_file50k --url "$WARP_URL/file50k" --rate 300  --duration 60 --threads 2 --conns 100

"$ROOT/scripts/bench.sh" --name lean_file1m  --url "$LEAN_URL/file1m"  --rate 50   --duration 60 --threads 2 --conns 50
"$ROOT/scripts/bench.sh" --name warp_file1m  --url "$WARP_URL/file1m"  --rate 50   --duration 60 --threads 2 --conns 50

echo "[5/5] Done."
echo "Summary CSV: $ROOT/bench/summary.csv"
