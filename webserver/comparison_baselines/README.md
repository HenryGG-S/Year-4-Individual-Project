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
