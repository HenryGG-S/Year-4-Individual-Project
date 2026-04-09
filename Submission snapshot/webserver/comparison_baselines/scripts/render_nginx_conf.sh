#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
STATIC_ROOT="$ROOT/bench_files"
TEMPLATE="$ROOT/nginx/nginx.conf"
OUTPUT="$ROOT/nginx/nginx.rendered.conf"

ESCAPED_STATIC_ROOT=$(printf '%s\n' "$STATIC_ROOT" | sed 's/[\/&]/\\&/g')
sed "s#__STATIC_ROOT__#$ESCAPED_STATIC_ROOT#g" "$TEMPLATE" > "$OUTPUT"

echo "Rendered: $OUTPUT"
