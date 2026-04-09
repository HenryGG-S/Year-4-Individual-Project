# Year 4 Individual Project — Haskell HTTP/1.1 Server Tail-Latency Study

This repository contains the implementation and benchmarking code for a dissertation project on the tail latency of a lean, standards-aware HTTP/1.1 server written in Haskell.

## Repository layout

- `webserver/` — main Stack project
  - `src/` — server, parser, framing, response, and workload code
  - `app/` — custom server executable
  - `app-warp/` — Warp baseline executable
  - `test/` — socket-level end-to-end tests
  - `scripts/` — benchmark orchestration and summarisation scripts
  - `bench/` — benchmark outputs, sessions, and logs
  - `bench_files/` — shared benchmark payload corpus used by the Haskell side
  - `comparison_baselines/` — Flask + Gunicorn, nginx, and Go baselines plus mirrored payloads

## What is included

- a custom HTTP/1.1 server in Haskell
- a Warp baseline implemented in the same Stack project
- filesystem-backed resource routes used for protocol and cache-validator testing
- socket-level tests for parsing, framing, conditional requests, and method semantics
- open-loop benchmarking via `wrk2`
- comparison baselines for:
  - Warp
  - Flask + Gunicorn
  - nginx static serving
  - Go `net/http`

## Quick start

```bash
cd webserver

stack setup
stack build
stack test
```

Run the custom server:

```bash
stack exec -- webserver-exe
```

Run the Warp baseline:

```bash
stack exec -- warp-baseline
```

## Benchmarking

Run one benchmark with fixed-rate load:

```bash
./scripts/bench.sh \
  --name lean_json \
  --url http://127.0.0.1:8080/json \
  --rate 2000 \
  --duration 60 \
  --threads 2 \
  --conns 100
```

Run the full benchmark session across the configured baselines:

```bash
./scripts/run_all_benchmarks.sh
```

If you need to regenerate or refresh the comparison baseline bundle:

```bash
./create_comparison_baselines.sh
```

## Notes on the baselines

- Warp is the closest Haskell baseline.
- Flask is benchmarked through Gunicorn, not Flask’s development server.
- nginx is a valid and useful static-file baseline, but it is not a like-for-like application-server comparison.
- Go `net/http` is included as another application-server baseline.

## Reproducibility

The benchmark scripts record run metadata including:
- code revision
- runtime and toolchain details
- load parameters
- per-run outputs and summaries

The benchmark payload corpus is intentionally fixed-size and shared across implementations to keep comparisons fair.

## Dissertation focus

The core research focus is tail latency under controlled load, not protocol completeness for its own sake. The implementation therefore aims to be lean, explicit in scope, and strong on the HTTP/1.1 behaviours that are directly relevant to the study.
