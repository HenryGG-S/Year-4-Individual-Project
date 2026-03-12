#!/usr/bin/env bash
set -euo pipefail

OUT_DIR="${1:-comparison_baselines}"

mkdir -p "$OUT_DIR"/{flask,nginx,go,scripts,bench_files}

# ------------------------------------------------------------------
# Generate payload files to match the Haskell project exactly
# ------------------------------------------------------------------
python3 - <<'PY' "$OUT_DIR/bench_files"
from pathlib import Path
import os
import sys

bench = Path(sys.argv[1])
bench.mkdir(parents=True, exist_ok=True)

len_json1k = 1024
len_file50k = 50 * 1024
len_file1m = 1024 * 1024

prefix = b'{"ok":true,"pad":"'
suffix = b'"}\n'
pad_len = len_json1k - (len(prefix) + len(suffix))
json1k = prefix + (b'a' * pad_len) + suffix

assert len(json1k) == len_json1k

(bench / "json1k.json").write_bytes(json1k)
(bench / "file50k.bin").write_bytes(os.urandom(len_file50k))
(bench / "file1m.bin").write_bytes(os.urandom(len_file1m))
PY

# ------------------------------------------------------------------
# Flask baseline
# ------------------------------------------------------------------
cat > "$OUT_DIR/flask/app.py" <<'PY'
from pathlib import Path
from flask import Flask, Response

ROOT = Path(__file__).resolve().parent.parent
BENCH = ROOT / "bench_files"

app = Flask(__name__)

def _read(name: str) -> bytes:
    return (BENCH / name).read_bytes()

JSON1K = _read("json1k.json")
FILE50K = _read("file50k.bin")
FILE1M = _read("file1m.bin")

@app.get("/")
def index() -> Response:
    return Response(b"ok\n", mimetype="text/plain")

@app.get("/health")
def health() -> Response:
    return Response(b"healthy\n", mimetype="text/plain")

@app.get("/json")
def json1k() -> Response:
    return Response(JSON1K, mimetype="application/json")

@app.get("/file50k")
def file50k() -> Response:
    return Response(FILE50K, mimetype="application/octet-stream")

@app.get("/file1m")
def file1m() -> Response:
    return Response(FILE1M, mimetype="application/octet-stream")

@app.errorhandler(404)
def not_found(_err):
    return Response(b"not found\n", status=404, mimetype="text/plain")

if __name__ == "__main__":
    # Development only. Do not use this for dissertation benchmarks.
    app.run(host="127.0.0.1", port=8082)
PY

cat > "$OUT_DIR/flask/run_gunicorn.sh" <<'SH'
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
SH
chmod +x "$OUT_DIR/flask/run_gunicorn.sh"

# ------------------------------------------------------------------
# nginx baseline
# Self-contained config: no external mime.types dependency
# ------------------------------------------------------------------
cat > "$OUT_DIR/nginx/nginx.conf" <<'NGINX'
worker_processes 1;

events {
    worker_connections 1024;
}

http {
    default_type application/octet-stream;
    sendfile on;
    keepalive_timeout 65;

    server {
        listen 8083;
        server_name localhost;

        location = / {
            default_type text/plain;
            return 200 "ok\n";
        }

        location = /health {
            default_type text/plain;
            return 200 "healthy\n";
        }

        location = /json {
            alias __STATIC_ROOT__/json1k.json;
            default_type application/json;
        }

        location = /file50k {
            alias __STATIC_ROOT__/file50k.bin;
            default_type application/octet-stream;
        }

        location = /file1m {
            alias __STATIC_ROOT__/file1m.bin;
            default_type application/octet-stream;
        }

        location / {
            default_type text/plain;
            return 404 "not found\n";
        }
    }
}
NGINX

cat > "$OUT_DIR/scripts/render_nginx_conf.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
STATIC_ROOT="$ROOT/bench_files"
TEMPLATE="$ROOT/nginx/nginx.conf"
OUTPUT="$ROOT/nginx/nginx.rendered.conf"

# Escape slashes for sed replacement
ESCAPED_STATIC_ROOT=$(printf '%s\n' "$STATIC_ROOT" | sed 's/[\/&]/\\&/g')
sed "s#__STATIC_ROOT__#$ESCAPED_STATIC_ROOT#g" "$TEMPLATE" > "$OUTPUT"

echo "Rendered: $OUTPUT"
SH
chmod +x "$OUT_DIR/scripts/render_nginx_conf.sh"

# ------------------------------------------------------------------
# Go baseline (optional)
# ------------------------------------------------------------------
cat > "$OUT_DIR/go/go.mod" <<'GO'
module comparison-baseline-go

go 1.22
GO

cat > "$OUT_DIR/go/main.go" <<'GO'
package main

import (
	"log"
	"net/http"
	"os"
	"path/filepath"
)

func mustRead(path string) []byte {
	b, err := os.ReadFile(path)
	if err != nil {
		log.Fatalf("read %s: %v", path, err)
	}
	return b
}

func main() {
	root, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	bench := filepath.Join(filepath.Dir(root), "bench_files")
	json1k := mustRead(filepath.Join(bench, "json1k.json"))
	file50k := mustRead(filepath.Join(bench, "file50k.bin"))
	file1m := mustRead(filepath.Join(bench, "file1m.bin"))

	mux := http.NewServeMux()

	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		_, _ = w.Write([]byte("ok\n"))
	})

	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		_, _ = w.Write([]byte("healthy\n"))
	})

	mux.HandleFunc("/json", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write(json1k)
	})

	mux.HandleFunc("/file50k", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		_, _ = w.Write(file50k)
	})

	mux.HandleFunc("/file1m", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		_, _ = w.Write(file1m)
	})

	log.Println("Go baseline on :8084")
	log.Fatal(http.ListenAndServe("127.0.0.1:8084", mux))
}
GO

# ------------------------------------------------------------------
# Bench helper
# ------------------------------------------------------------------
cat > "$OUT_DIR/scripts/benchmark_matrix.sh" <<'SH'
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
SH
chmod +x "$OUT_DIR/scripts/benchmark_matrix.sh"

# ------------------------------------------------------------------
# README
# ------------------------------------------------------------------
cat > "$OUT_DIR/README.md" <<'MD'
# Comparison baselines for the dissertation

This bundle provides minimal comparison servers aligned with the current Haskell project routes and payload sizes.

Routes:
- /
- /health
- /json
- /file50k
- /file1m

Ports:
- Custom Haskell server: 8080
- Warp baseline: 8081
- Flask + Gunicorn: 8082
- nginx static baseline: 8083
- Go net/http baseline: 8084

## Flask
cd flask
python3 -m venv .venv
source .venv/bin/activate
pip install flask gunicorn
./run_gunicorn.sh

## nginx
../scripts/render_nginx_conf.sh
nginx -p "$PWD" -c nginx.rendered.conf

Stop:
nginx -p "$PWD" -c nginx.rendered.conf -s stop

## Go
cd go
go run .

## Method note
- Benchmark Flask through Gunicorn, not Flask's dev server.
- nginx is a valid baseline, but it is not a like-for-like application framework comparison.
- Go net/http is optional but useful as another application-server baseline.
MD

echo "Created comparison baselines in: $OUT_DIR"
