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
