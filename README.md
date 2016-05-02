# rOut

[![Join the chat at https://gitter.im/teodimoff/rOut](https://badges.gitter.im/teodimoff/rOut.svg)](https://gitter.im/teodimoff/rOut?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
Scala library for building Finagle HTTP services.
This is a fork of https://github.com/finagle/finch which will aim to make it possible to use regular Filter and Services.

Initial benchmark results:

| Benchmark         | Run 1          | Run 2          | Run 3          |
|-------------------|----------------|----------------|----------------|
| Finagle + Jackson | 28812.74 req/s | 29679.16 req/s | 29596.28 req/s |
| Finch + Circe     | 25954.25 req/s | 26021.75 req/s | 25916.58 req/s |
| rOut params	    | 24186.81 req/s | 24232.44 req/s | 24334.53 req/s |
| rOut params + auth| 23795.24 req/s | 23701.54 req/s | 23688.66 req/s |
| rOut + Circe      | 26089.80 req/s | 26184.95	req/s | 26248.69 req/s |
| rOut Circe + auth | 21729.74 req/s | 21668.75	req/s | 21681.65 req/s |