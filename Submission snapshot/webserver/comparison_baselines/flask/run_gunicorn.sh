#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV="$SCRIPT_DIR/.venv"
PYTHON_BIN="$VENV/bin/python"
REQ_FILE="$SCRIPT_DIR/requirements.txt"
ENSURE_ONLY=0

if [[ "${1:-}" == "--ensure-only" ]]; then
  ENSURE_ONLY=1
fi

ensure_flask_env() {
  if [[ ! -x "$PYTHON_BIN" ]]; then
    python3 -m venv "$VENV"
  fi

  if ! "$PYTHON_BIN" - <<'PY' >/dev/null 2>&1
import flask  # noqa: F401
import gunicorn  # noqa: F401
PY
  then
    "$PYTHON_BIN" -m pip install --upgrade pip >/dev/null
    "$PYTHON_BIN" -m pip install -r "$REQ_FILE"
  fi
}

cd "$SCRIPT_DIR"
ensure_flask_env

if [[ "$ENSURE_ONLY" -eq 1 ]]; then
  exit 0
fi

exec "$PYTHON_BIN" -m gunicorn \
  --bind 127.0.0.1:8082 \
  --workers 1 \
  --worker-class sync \
  --threads 1 \
  --preload \
  --access-logfile - \
  app:app
