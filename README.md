# rOut
Scala library for building Finagle HTTP services.
This is a fork of https://github.com/finagle/finch which will aim to make it possible to use regular Filter and Services.

Initial benchmark results:

finagle

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.40ms    2.40ms  48.97ms   91.99%
    Req/Sec     7.24k     1.13k   10.95k    76.83%
  864507 requests in 30.00s, 81.62MB read
Requests/sec:  28812.74
Transfer/sec:      2.72MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.33ms    2.22ms  31.46ms   92.06%
    Req/Sec     7.46k     0.90k   10.56k    72.75%
  890550 requests in 30.01s, 84.08MB read
Requests/sec:  29679.16
Transfer/sec:      2.80MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.33ms    2.15ms  25.60ms   91.81%
    Req/Sec     7.44k   736.71    10.79k    71.25%
  888090 requests in 30.01s, 83.85MB read
Requests/sec:  29596.28
Transfer/sec:      2.79MB

finch

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.42ms    2.23ms  44.66ms   92.13%
    Req/Sec     6.54k     1.06k   25.18k    79.02%
  781214 requests in 30.10s, 73.76MB read
Requests/sec:  25954.21
Transfer/sec:      2.45MB
teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.33ms    1.99ms  32.53ms   93.04%
    Req/Sec     6.54k     0.95k    9.48k    74.42%
  780690 requests in 30.00s, 73.71MB read
Requests/sec:  26021.64
Transfer/sec:      2.46MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s wrk.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.31ms    1.90ms  29.08ms   93.08%
    Req/Sec     6.51k   799.65     9.48k    72.17%
  777632 requests in 30.01s, 73.42MB read
Requests/sec:  25916.59
Transfer/sec:      2.45MB

rOut - raw

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s scripts/rOut-raw.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.37ms    1.94ms  34.23ms   93.02%
    Req/Sec     6.08k     1.04k    8.91k    72.25%
  725695 requests in 30.00s, 62.29MB read
Requests/sec:  24186.81
Transfer/sec:      2.08MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s scripts/rOut-raw.lua http://localhost:8081/
Running 30s test @ http://localhost:8081/
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.45ms    2.14ms  32.59ms   92.33%
    Req/Sec     6.10k   845.34    21.24k    79.68%
  729373 requests in 30.10s, 62.60MB read
Requests/sec:  24232.44
Transfer/sec:      2.08MB
teodimoff@:wrk >

rOut -- with authentication 

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s scripts/rOut-raw.lua http://localhost:8081/auth
Running 30s test @ http://localhost:8081/auth
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.70ms    2.70ms  54.17ms   90.66%
    Req/Sec     5.99k     0.94k   22.68k    77.60%
  716243 requests in 30.10s, 61.48MB read
Requests/sec:  23795.81
Transfer/sec:      2.04MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s scripts/rOut-raw.lua http://localhost:8081/auth
Running 30s test @ http://localhost:8081/auth
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.68ms    2.55ms  33.78ms   90.07%
    Req/Sec     5.96k   798.80     8.43k    69.00%
  711177 requests in 30.01s, 61.04MB read
Requests/sec:  23701.54
Transfer/sec:      2.03MB

teodimoff@:wrk >./wrk -t4 -c24 -d30s -s scripts/rOut-raw.lua http://localhost:8081/auth
Running 30s test @ http://localhost:8081/auth
  4 threads and 24 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.54ms    2.30ms  32.02ms   91.85%
    Req/Sec     5.97k   844.64    21.95k    79.93%
  713018 requests in 30.10s, 61.20MB read
Requests/sec:  23688.66
Transfer/sec:      2.03MB
teodimoff@:wrk >





