#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="${1:-}"
if [[ -z "$PROJECT_ROOT" ]]; then
  echo "Usage: $0 /path/to/webserver"
  exit 1
fi

cd "$PROJECT_ROOT"

./scripts/bench.sh --url http://127.0.0.1:8080/json   --rate 2000 --name custom_json
./scripts/bench.sh --url http://127.0.0.1:8081/json   --rate 2000 --name warp_json
./scripts/bench.sh --url http://127.0.0.1:8082/json   --rate 2000 --name flask_json
./scripts/bench.sh --url http://127.0.0.1:8083/json   --rate 2000 --name nginx_json
./scripts/bench.sh --url http://127.0.0.1:8084/json   --rate 2000 --name go_json

./scripts/bench.sh --url http://127.0.0.1:8080/file1m --rate 200 --conns 50 --name custom_file1m
./scripts/bench.sh --url http://127.0.0.1:8081/file1m --rate 200 --conns 50 --name warp_file1m
./scripts/bench.sh --url http://127.0.0.1:8082/file1m --rate 200 --conns 50 --name flask_file1m
./scripts/bench.sh --url http://127.0.0.1:8083/file1m --rate 200 --conns 50 --name nginx_file1m
./scripts/bench.sh --url http://127.0.0.1:8084/file1m --rate 200 --conns 50 --name go_file1m
