-- scripts/wrk2_metrics.lua
done = function(summary, latency, requests)
  local function ms(us) return us / 1000.0 end  -- wrk2 latency values are microseconds :contentReference[oaicite:6]{index=6}

  io.write("\n--- metrics ---\n")
  io.write(string.format("requests=%d\n", summary.requests))
  io.write(string.format("duration_s=%.6f\n", summary.duration / 1000000.0))
  io.write(string.format("bytes=%d\n", summary.bytes))
  io.write(string.format("rps=%.2f\n", summary.requests / (summary.duration / 1000000.0)))

  io.write(string.format("p50_ms=%.3f\n", ms(latency:percentile(50.0))))
  io.write(string.format("p95_ms=%.3f\n", ms(latency:percentile(95.0))))
  io.write(string.format("p99_ms=%.3f\n", ms(latency:percentile(99.0))))
  io.write(string.format("p999_ms=%.3f\n", ms(latency:percentile(99.9))))

  io.write(string.format("err_connect=%d\n", summary.errors.connect))
  io.write(string.format("err_read=%d\n", summary.errors.read))
  io.write(string.format("err_write=%d\n", summary.errors.write))
  io.write(string.format("err_status=%d\n", summary.errors.status))
  io.write(string.format("err_timeout=%d\n", summary.errors.timeout))
  io.write("--- end ---\n")
end

