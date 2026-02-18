#!/usr/bin/env bash
set -euo pipefail

URL=""
RATE=""
DURATION="60"     # seconds
THREADS="2"
CONNS="100"
NAME=""

usage() {
  echo "Usage: $0 --url URL --rate RPS [--duration S] [--threads N] [--conns N] [--name NAME]"
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --url) URL="$2"; shift 2;;
    --rate) RATE="$2"; shift 2;;
    --duration) DURATION="$2"; shift 2;;
    --threads) THREADS="$2"; shift 2;;
    --conns) CONNS="$2"; shift 2;;
    --name) NAME="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -z "$URL" || -z "$RATE" ]] && usage
[[ -z "$NAME" ]] && NAME="$(echo "$URL" | sed 's#http://##; s#https://##; s#[/:]#_#g')"

# Detect wrk2 command name:
WRK=""
if command -v wrk2 >/dev/null 2>&1; then
  WRK="wrk2"
elif command -v wrk >/dev/null 2>&1; then
  WRK="wrk"
else
  echo "Error: neither 'wrk2' nor 'wrk' found in PATH."
  exit 2
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNS_DIR="$ROOT/bench/runs"
TS="$(date +%Y%m%d_%H%M%S)"
OUT_DIR="$RUNS_DIR/${TS}_${NAME}_R${RATE}_c${CONNS}_t${THREADS}_d${DURATION}s"
mkdir -p "$OUT_DIR"

# Metadata
{
  echo "timestamp=$TS"
  echo "name=$NAME"
  echo "url=$URL"
  echo "rate_rps=$RATE"
  echo "duration_s=$DURATION"
  echo "threads=$THREADS"
  echo "connections=$CONNS"
  echo "wrk_cmd=$WRK"
  echo "git_commit=$(git -C "$ROOT" rev-parse HEAD 2>/dev/null || echo unknown)"
  echo "uname=$(uname -a)"
  echo "ulimit_n=$(ulimit -n || true)"
  echo "stack_version=$(stack --version 2>/dev/null || true)"
  echo "ghc_version=$(stack ghc -- --version 2>/dev/null || true)"
  echo "wrk_version=$($WRK --version 2>/dev/null || true)"
} > "$OUT_DIR/meta.txt"

# Run
set +e
"$WRK" -t"$THREADS" -c"$CONNS" -d"${DURATION}s" -R"$RATE" \
  --latency \
  -s "$ROOT/scripts/wrk2_metrics.lua" \
  "$URL" | tee "$OUT_DIR/wrk2.out"
RC=${PIPESTATUS[0]}
set -e

echo "$RC" > "$OUT_DIR/exit_code.txt"

# Extract metrics block
awk '
  /--- metrics ---/ {inblk=1; next}
  /--- end ---/ {inblk=0}
  inblk==1 {print}
' "$OUT_DIR/wrk2.out" > "$OUT_DIR/metrics.txt" || true

# Build CSV row
# shellcheck disable=SC1090
source <(sed 's/^/export /' "$OUT_DIR/metrics.txt" 2>/dev/null || true)

CSV="$ROOT/bench/summary.csv"
mkdir -p "$ROOT/bench"
if [[ ! -f "$CSV" ]]; then
  echo "timestamp,name,url,rate_rps,duration_s,threads,connections,requests,achieved_rps,p50_ms,p95_ms,p99_ms,p999_ms,err_connect,err_read,err_write,err_status,err_timeout,exit_code" > "$CSV"
fi

echo "${TS},${NAME},${URL},${RATE},${DURATION},${THREADS},${CONNS},${requests:-},${rps:-},${p50_ms:-},${p95_ms:-},${p99_ms:-},${p999_ms:-},${err_connect:-},${err_read:-},${err_write:-},${err_status:-},${err_timeout:-},${RC}" >> "$CSV"

echo "Run saved to: $OUT_DIR"
echo "Summary appended to: $CSV"
exit "$RC"

