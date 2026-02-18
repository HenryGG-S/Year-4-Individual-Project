# Year 4 Individual Project — Haskell HTTP/1.1 Server (Tail Latency Study)

This repository contains my Year 4 dissertation project: implementing a lean, standards-aware HTTP/1.1 server in Haskell and benchmarking its tail-latency behaviour (p95–p99.9) under open-loop load, with comparisons against Warp.

## Repository layout

- `webserver/` — Haskell server implementation (Stack project)
- (Other folders may be added later: report, benchmarks, plots, etc.)

> If you’re looking for implementation details, start in `webserver/`.

---

## Quick start (Linux)

### 1) Install system dependencies (Debian/Ubuntu)

Stack’s docs list the common build dependencies required for GHC/tooling on Linux. On Debian/Ubuntu, this is usually enough: :contentReference[oaicite:1]{index=1}

```bash
sudo apt-get update
sudo apt-get install -y \
  g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev \
  git gnupg netbase

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source ~/.bashrc   # or ~/.zshrc
stack --version
ghc --version

git clone https://github.com/HenryGG-S/Year-4-Individual-Project.git
cd Year-4-Individual-Project/webserver

# Install the right GHC for the resolver (if needed)
stack setup

# Build
stack build

# Run (runs the first executable in the project)
stack run

#running with rts stats - useful later in the project
stack run -- +RTS -N -s -RTS

-N uses all cores

-s prints GC/runtime stats on exit

(Stack run behaviour is documented here: stack run builds and runs a project executable.)

Troubleshooting
“stack: command not found”

Your PATH isn’t set up. If you installed via GHCup, Stack is typically in:

~/.ghcup/bin

or ~/.local/bin

Add one (or both) to PATH in ~/.bashrc:

export PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"


Restart your terminal or run source ~/.bashrc.

Stack can’t install GHC / complains about missing libraries

You’re usually missing system packages like libgmp, libffi, zlib, or xz. Re-run the Debian/Ubuntu dependency install shown above.

“No project config file found” or Stack isn’t using the right stack.yaml

Make sure you’re inside the Stack project directory:

cd Year-4-Individual-Project/webserver
ls


You should see a stack.yaml in that directory.

Builds are failing in strange ways after changes

Try a clean rebuild:

stack clean
stack build


If it’s really stuck (rare), delete .stack-work/ in the project directory:

rm -rf .stack-work
stack build

Downloads are failing (TLS / proxy / network)

Ensure system clock is correct

Try a different network

If you’re behind a corporate proxy, you may need to configure Stack’s network settings

Notes

This project is in active development.

Benchmarking harness + workloads + Warp comparison tooling will be added as the implementation stabilises.

