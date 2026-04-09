#!/usr/bin/env python3
import argparse
import csv
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Tuple

NUMERIC_FIELDS = [
    "requests",
    "achieved_rps",
    "p50_ms",
    "p95_ms",
    "p99_ms",
    "p999_ms",
    "err_connect",
    "err_read",
    "err_write",
    "err_status",
    "err_timeout",
]

GROUP_FIELDS = [
    "name",
    "url",
    "rate_rps",
    "duration_s",
    "threads",
    "connections",
]


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--input", required=True, help="raw_runs.csv")
    p.add_argument("--output", required=True, help="trimmed_summary.csv")
    p.add_argument("--trim-count", type=int, default=2)
    return p.parse_args()


def to_float(row: dict, key: str) -> float:
    value = row.get(key, "")
    if value == "":
        raise ValueError(f"Missing numeric field {key} in row {row}")
    return float(value)


def trimmed_mean(values: List[float], trim_count: int) -> float:
    if len(values) < (2 * trim_count + 1):
        raise ValueError(
            f"Need at least {2 * trim_count + 1} values for trim_count={trim_count}, got {len(values)}"
        )
    vals = sorted(values)
    trimmed = vals[trim_count: len(vals) - trim_count]
    return sum(trimmed) / len(trimmed)


def main() -> None:
    args = parse_args()
    input_path = Path(args.input)
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with input_path.open(newline="") as f:
      rows = list(csv.DictReader(f))

    if not rows:
      raise SystemExit("Input CSV has no rows")

    bad_rows = [r for r in rows if r.get("exit_code", "") not in ("0", 0)]
    if bad_rows:
      raise SystemExit(f"Found {len(bad_rows)} rows with non-zero exit_code; refusing to summarize")

    groups: Dict[Tuple[str, ...], List[dict]] = defaultdict(list)
    for row in rows:
      key = tuple(row[field] for field in GROUP_FIELDS)
      groups[key].append(row)

    fieldnames = GROUP_FIELDS + [
      "runs_total",
      "trim_low",
      "trim_high",
      "requests_trimmed_mean",
      "achieved_rps_trimmed_mean",
      "p50_ms_trimmed_mean",
      "p95_ms_trimmed_mean",
      "p99_ms_trimmed_mean",
      "p999_ms_trimmed_mean",
      "err_connect_trimmed_mean",
      "err_read_trimmed_mean",
      "err_write_trimmed_mean",
      "err_status_trimmed_mean",
      "err_timeout_trimmed_mean",
    ]

    out_rows = []
    for key, group_rows in sorted(groups.items()):
      out = {field: value for field, value in zip(GROUP_FIELDS, key)}
      out["runs_total"] = len(group_rows)
      out["trim_low"] = args.trim_count
      out["trim_high"] = args.trim_count

      for field in NUMERIC_FIELDS:
        vals = [to_float(r, field) for r in group_rows]
        out[f"{field}_trimmed_mean"] = f"{trimmed_mean(vals, args.trim_count):.6f}"

      out_rows.append(out)

    with output_path.open("w", newline="") as f:
      writer = csv.DictWriter(f, fieldnames=fieldnames)
      writer.writeheader()
      writer.writerows(out_rows)

    print(f"Wrote trimmed summary: {output_path}")


if __name__ == "__main__":
    main()
