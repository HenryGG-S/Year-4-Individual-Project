# webserver

This directory contains the main Stack project for the dissertation implementation and benchmark harness.

## Contents

- `src/`
  - `Server.hs` — main custom HTTP/1.1 server
  - `Http/Parse.hs` — request-head parsing and target validation
  - `Http/Framing.hs` — request body framing decisions
  - `Http/Body.hs` — fixed-length and chunked body reading
  - `Http/Response.hs` — response construction
  - `Workloads.hs` — shared benchmark payload loading and generation
- `app/` — custom server executable entry point
- `app-warp/` — Warp baseline entry point
- `test/` — socket-level end-to-end tests
- `scripts/` — benchmark scripts
- `bench/` — benchmark outputs
- `bench_files/` — shared benchmark corpus
- `comparison_baselines/` — Flask, nginx, and Go baseline servers

## Build

```bash
stack setup
stack build
```

## Run

Custom Haskell server:

```bash
stack exec -- webserver-exe
```

Warp baseline:

```bash
stack exec -- warp-baseline
```

## Test

Run the full end-to-end test suite:

```bash
stack test
```

## Benchmark workflow

Run a single benchmark:

```bash
./scripts/bench.sh \
  --name lean_json \
  --url http://127.0.0.1:8080/json \
  --rate 2000 \
  --duration 60 \
  --threads 2 \
  --conns 100
```

Run the full benchmark session:

```bash
./scripts/run_all_benchmarks.sh
```

Run only the build / environment checks:

```bash
./scripts/run_all_benchmarks.sh --build-only
```

## Comparison baselines

The comparison bundle includes:

- Warp baseline on port `8081`
- Flask + Gunicorn on port `8082`
- nginx static baseline on port `8083`
- Go `net/http` baseline on port `8084`

The baseline README is here:

```bash
cat comparison_baselines/README.md
```

## Benchmark corpus

The Haskell side uses files under:

```text
bench_files/
```

The comparison bundle mirrors the same corpus under:

```text
comparison_baselines/bench_files/
```

These payloads are meant to stay byte-identical across implementations.

## Output locations

Benchmark outputs are written under:

```text
bench/runs/
bench/sessions/
bench/server_logs/
```

Typical outputs include:
- raw `wrk2` output
- extracted metrics
- per-run metadata
- trimmed summaries

## Dissertation framing

This project is about tail-latency behaviour under controlled load, with Haskell as the implementation language under study. The code is therefore organised to make protocol choices, benchmarking, and cross-implementation comparison easy to inspect and defend.
