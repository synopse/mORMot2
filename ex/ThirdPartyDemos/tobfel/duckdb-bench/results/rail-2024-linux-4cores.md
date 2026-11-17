rail-2024, median of 5 runs after a warm-up run

- duckdb: v2.0.0
- sqlite: 3.51.2
- postgres: 16.15 (Ubuntu 16.15-0ubuntu0.24.04.1)
- duckdb-sqlite: v2.0.0
- duckdb-postgres: v2.0.0

| query | rows | duckdb | sqlite | postgres | duckdb-sqlite | duckdb-postgres |
|---|---:|---:|---:|---:|---:|---:|
| r1-punctuality-month | 158 | 486.91ms | 32.91s^ | 2.46s | 1.33s | 2.56s |
| r2-worst-stations | 10 | 41.79ms | 8.13s | 435.73ms | 836.41ms | 1.64s |
| r3-delay-by-hour | 217 | 409.07ms | 36.69s^ | 7.76s | 1.63s | 3.89s |
| r4-delay-percentiles | 18 | 770.11ms | 2m25^ | 9.88s | 1.45s | 2.15s |
| r5-cancellations-weekday | 7 | 10.74ms | 2.47s | 165.83ms | 94.92ms | 173.21ms |
| r6-most-delayed-trips | 20 | 48.14ms | 688.02ms | 102.16ms | 1.68s | 2.22s |
| r7-station-daily-trend | 366 | 62.05ms | 993.05ms | 1.21s | 1.32s | 702.98ms |
| r8-platform-changes | 10 | 31.89ms | 7.40s | 440.31ms | 818.82ms | 1.68s |

^ single run (more than 30 seconds), * result differs from the first engine

Same run with dbbench compiled with -dNOSQLITE3STATIC (Ubuntu libsqlite3 3.45.1):

| query | rows | sqlite |
|---|---:|---:|
| r1-punctuality-month | 158 | 18.40s |
| r2-worst-stations | 10 | 1.44s |
| r3-delay-by-hour | 217 | 23.55s |
| r4-delay-percentiles | 18 | 1m14^ |
| r5-cancellations-weekday | 7 | 1.12s |
| r6-most-delayed-trips | 20 | 134.14ms |
| r7-station-daily-trend | 366 | 670.88ms |
| r8-platform-changes | 10 | 1.38s |

^ single run (more than 30 seconds), * result differs from the first engine
