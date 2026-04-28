Diagnostics run created: 20260421_190653
Project root: /home/henry/Documents/Year-4-Individual-Project/webserver

Executables:
- normal:   /home/henry/Documents/Year-4-Individual-Project/webserver/.stack-work/install/x86_64-linux/f3c222d765ddb5230766e72eb788d89297c913f0212f4721a77c7fd116884bf5/9.10.3/bin/webserver-exe
- profiled: /home/henry/Documents/Year-4-Individual-Project/webserver/.stack-work/install/x86_64-linux/57521459828ea74c27981b23cfe6e64be9eaa05794a3a90642873d925606d7f9/9.10.3/bin/webserver-exe

Ports used:
- file1m eventlog: 18080
- json eventlog:   18081
- file1m profile:  18082
- json profile:    18083

Exit timers:
- eventlog cases: duration + 10s
- profile cases:  duration + 10s

Contents:
- eventlogs/: eventlog benchmark runs and .eventlog files
- profiles/: profiled runs and .prof/.hp/.ps/.pdf files
- benchmark_runs/: copies of the wrk2 run directories produced by scripts/bench.sh
- csv/: raw CSV summaries for eventlog and profile passes

