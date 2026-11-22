TPC-H sf=1, median of 5 runs after a warm-up run

- duckdb: v2.0.0
- sqlite: 3.51.2
- postgres: 16.15 (Ubuntu 16.15-0ubuntu0.24.04.1)
- duckdb-sqlite: v2.0.0
- duckdb-postgres: v2.0.0

| query | rows | duckdb | sqlite | postgres | duckdb-sqlite | duckdb-postgres |
|---|---:|---:|---:|---:|---:|---:|
| a1-monthly-trend | 80 | 16.36ms | 3.13s | 142.49ms | 71.53ms | 156.65ms |
| a2-top3-per-nation | 75 | 56.51ms | 4.15s | 2.92s | 148.18ms | 223.44ms |
| a3-median-price | 5 | 32.12ms | 12s | 1.06s | 114.07ms | 199.42ms |
| a4-pivot-shipmode | 7 | 58.23ms | 12.55s | 711.31ms | 473.83ms | 816.02ms |
| a5-rollup-region | 31 | 42.35ms | 3.49s | 1.25s | 123.34ms | 185.86ms |
| a6-top-order-per-priority | 5 | 15.97ms | 1.99s | 416.84ms | 80.12ms | 201.95ms |
| a7-distinct-customers | 35 | 47.92ms | 4.61s | 2.02s | 189.27ms | 205.05ms |
| q01-pricing-summary | 4 | 33.90ms | 17.05s | 1.24s | 587.57ms | 1.27s |
| q03-shipping-priority | 10 | 27.71ms | 1.38s | 550.74ms | 477.13ms | 718.70ms |
| q05-local-supplier | 5 | 34.57ms | 1.03s | 322.13ms | 420.82ms | 903.50ms |
| q06-forecast-revenue | 1 | 10.36ms | 260.68ms | 70.84ms | 342.14ms | 198.47ms |
| q10-returned-item | 20 | 55.45ms | 778.96ms | 269.68ms | 492.32ms | 465.40ms |
| q12-shipping-modes | 2 | 22.25ms | 1.81s | 320.64ms | 525.69ms | 390.77ms |
| q14-promotion-effect | 1 | 21.95ms | 256.61ms | 178.94ms | 421.56ms | 220.53ms |
| q18-large-volume | 57 | 70.57ms | 3.31s | 4.53s | 388.01ms | 725.13ms |
| p1-point-lookups | 10000 | 6.43s | 18.44ms | 676.03ms | n/a | n/a |
| p2-single-inserts | 10000 | 8.64s | 8.25ms | 607.71ms | n/a | n/a |

^ single run (more than 30 seconds), * result differs from the first engine

Same run with dbbench compiled with -dNOSQLITE3STATIC (Ubuntu libsqlite3 3.45.1):

| query | rows | sqlite |
|---|---:|---:|
| a1-monthly-trend | 80 | 2.31s |
| a2-top3-per-nation | 75 | 3.29s |
| a3-median-price | 5 | 6.13s |
| a4-pivot-shipmode | 7 | 6.21s |
| a5-rollup-region | 31 | 2.86s |
| a6-top-order-per-priority | 5 | 1.17s |
| a7-distinct-customers | 35 | 3.91s |
| q01-pricing-summary | 4 | 7s |
| q03-shipping-priority | 10 | 1.19s |
| q05-local-supplier | 5 | 883.85ms |
| q06-forecast-revenue | 1 | 52ms |
| q10-returned-item | 20 | 589.13ms |
| q12-shipping-modes | 2 | 843.61ms |
| q14-promotion-effect | 1 | 198.15ms |
| q18-large-volume | 57 | 873.14ms |
| p1-point-lookups | 10000 | 15.97ms |
| p2-single-inserts | 10000 | 6.82ms |

^ single run (more than 30 seconds), * result differs from the first engine
