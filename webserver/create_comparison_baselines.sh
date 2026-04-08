#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="$ROOT/comparison_baselines"
PROVISION_FLASK=1

usage() {
  cat <<'USAGE'
Usage: ./create_comparison_baselines.sh [options] [out_dir]

Options:
  --no-provision-flask   Generate files only; do not create/update the Flask virtualenv
  --help                 Show this help

Arguments:
  out_dir                Target directory for generated baselines
                         (default: ./comparison_baselines)
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --no-provision-flask) PROVISION_FLASK=0; shift ;;
    --help) usage; exit 0 ;;
    -*) echo "Unknown option: $1" >&2; usage; exit 1 ;;
    *) OUT_DIR="$1"; shift ;;
  esac
done

need_cmd() { command -v "$1" >/dev/null 2>&1 || { echo "Error: $1 not found" >&2; exit 1; }; }
die() { echo "Error: $*" >&2; exit 1; }

need_cmd python3

MAIN_BENCH_DIR="$ROOT/bench_files"
[[ -d "$MAIN_BENCH_DIR" ]] || die "Missing $MAIN_BENCH_DIR. The main benchmark corpus must exist before generating baselines."

mkdir -p "$OUT_DIR"/{flask,nginx,go,scripts,bench_files}

copy_bench_corpus() {
  local src="$MAIN_BENCH_DIR"
  local dst="$OUT_DIR/bench_files"
  local files=(json1k.json file50k.bin file1m.bin)

  for f in "${files[@]}"; do
    [[ -f "$src/$f" ]] || die "Missing benchmark corpus file: $src/$f"
    cp "$src/$f" "$dst/$f"
  done
}

write_flask_files() {
  cat > "$OUT_DIR/flask/app.py" <<'PYAPP'
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
    app.run(host="127.0.0.1", port=8082)
PYAPP

  cat > "$OUT_DIR/flask/requirements.txt" <<'REQ'
flask
gunicorn
REQ

  cat > "$OUT_DIR/flask/run_gunicorn.sh" <<'GUNICORN'
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
GUNICORN
  chmod +x "$OUT_DIR/flask/run_gunicorn.sh"
}

write_nginx_files() {
  cat > "$OUT_DIR/nginx/nginx.conf" <<'NGINX'
worker_processes 1;
pid nginx.pid;
error_log stderr notice;

events {
    worker_connections 1024;
}

http {
    default_type application/octet-stream;
    sendfile on;
    keepalive_timeout 65;

    access_log off;

    client_body_temp_path client_temp;
    proxy_temp_path proxy_temp;
    fastcgi_temp_path fastcgi_temp;
    uwsgi_temp_path uwsgi_temp;
    scgi_temp_path scgi_temp;

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

  cat > "$OUT_DIR/scripts/render_nginx_conf.sh" <<'RENDER'
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
RENDER
  chmod +x "$OUT_DIR/scripts/render_nginx_conf.sh"
}

write_go_files() {
  cat > "$OUT_DIR/go/go.mod" <<'GOMOD'
module comparison-baseline-go

go 1.22
GOMOD

  cat > "$OUT_DIR/go/main.go" <<'GOMAIN'
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
GOMAIN
}

write_readme() {
  cat > "$OUT_DIR/README.md" <<'README'
# Comparison baselines for the dissertation

This directory is generated by `./create_comparison_baselines.sh` and is intended to be driven through `./scripts/run_all_benchmarks.sh`.

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

## Notes
- The benchmark corpus is copied from the main project's `bench_files/` so all baselines serve byte-identical payloads.
- The Flask runner bootstraps its own virtualenv on first use.
- `nginx`, `go`, `wrk2`, `stack`, `curl`, and `python3` remain external system prerequisites.
- Preferred entrypoint: `./scripts/run_all_benchmarks.sh`
README
}

provision_flask() {
  echo "[init] Provisioning Flask baseline virtualenv..."
  "$OUT_DIR/flask/run_gunicorn.sh" --ensure-only
}

copy_bench_corpus
write_flask_files
write_nginx_files
write_go_files
write_readme

if [[ "$PROVISION_FLASK" -eq 1 ]]; then
  provision_flask
else
  echo "[init] Skipping Flask virtualenv provisioning"
fi

echo "Created comparison baselines in: $OUT_DIR"
