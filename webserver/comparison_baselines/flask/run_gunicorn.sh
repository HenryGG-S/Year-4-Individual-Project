#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
exec gunicorn \
  --bind 127.0.0.1:8082 \
  --workers 1 \
  --worker-class sync \
  --threads 1 \
  --preload \
  --access-logfile - \
  app:app
