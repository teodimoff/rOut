# rOut
Scala library for building Finagle HTTP services.
This is a fork of https://github.com/finagle/finch which will aim to make it possible to use regular Filter and Services.

Initial benchmark results:

| Benchmark         | Run 1          | Run 2          | Run 3          |
|-------------------|----------------|----------------|----------------|
| Finagle + Jackson | 28812.74 req/s | 29679.16 req/s | 29596.28 req/s |
| Finch + Circe     | 25954.25 req/s | 26021.75 req/s | 25916.58 req/s |
| rOut params	    | 24186.81 req/s | 24232.44 req/s | 24334.53 req/s |
| rOut params + auth| 23795.24 req/s | 23701.54 req/s | 23688.66 req/s |
| rOut circe soon   | 		     |		      |		       |
| rOut circe + auth |		     | 		      | 	       |