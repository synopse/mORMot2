# duckdbcli - SQL command line client for DuckDB

A small `sqlite3`-like console client, using the mORMot parameters parser
(`Executable.Command`), for the three DuckDB flavors of the framework:

| mode | option | notes |
|------|--------|-------|
| embedded engine | `-d file` or `file` (default `:memory:`) | needs `duckdb.dll` / `libduckdb.so` (64-bit) |
| mormot.db.proxy | `-p host:port --root name -u user -w pwd` | any database published by a `TSqlDBServerSockets` |
| Quack | `-q quack:host[:port] -w token` | native client, no library needed; token may be set by `DUCKDB_TOKEN` |

```
duckdbcli my.duckdb -c "select count(*) from t"
duckdbcli -q quack:localhost -w secret
duckdbcli -p 127.0.0.1:8899 --root duck -u user -w pass -m json -c "select * from t"
duckdbcli -f script.sql -m csv --noheader
```

Interactive statements end with `;`. Dot commands: `.tables`, `.schema [table]`,
`.mode table|csv|json|line`, `.timer on|off`, `.version`, `.help`, `.quit`.
Run `duckdbcli -h` for all options.

Build with FPC or Delphi: `fpc -Mdelphi duckdbcli.dpr` (with the mORMot `src`
sub-folders in the unit path).

## Benchmark against the official `duckdb` CLI

Scripts are in the [`bench`](bench/README.md) folder. Dataset: the public
[`train_services.parquet`](https://blobs.duckdb.org/train_services.parquet)
(380,959 rows, 8 columns), queried through a view-only catalog database as
described in <https://duckdb.org/2026/10/07/view-only-mode>.

### Execution environment

- Cloud VM, Linux 6.18 x86_64, 4 vCPU Intel Xeon @ 2.80 GHz, 15 GB RAM
- Official `duckdb` CLI v1.5.6 and the same `libduckdb.so` v1.5.6 for `duckdbcli`
- `duckdbcli` built with FPC 3.2.2 (`-O3`), Quack client using protocol version 1
  (the one of the released DuckDB 1.5 `quack` extension)
- Everything on localhost: Quack and `mormot.db.proxy` servers on the same VM,
  Parquet file read from the local disk (no network involved)
- CSV output redirected to `/dev/null`; median of 9 runs (5 for the multi-statement
  scenarios) after one warm-up run; wall-clock time including process start-up
- "official Quack" runs `SELECT * FROM quack_query(...)` in the official CLI,
  which loads the `httpfs` and `quack` extensions on first use

### One process per query (median, milliseconds)

| Query | official embedded | duckdbcli embedded | official Quack | duckdbcli Quack | duckdbcli proxy |
|---|---:|---:|---:|---:|---:|
| `count(*)` | 22 | 50 | 137 | **7** | 7 |
| top 10 stations (group by) | 28 | 56 | 138 | **11** | 12 |
| group by type, min/max | 33 | 63 | 142 | **17** | 20 |
| filter on `train_number` | 32 | 67 | 146 | **18** | 18 |
| `select *` (380,959 rows) | 809 | 847 | 1265 | **901** | 4764 |

### Several statements in one process (median, milliseconds)

| Scenario | official embedded | duckdbcli embedded | official Quack | duckdbcli Quack | duckdbcli proxy |
|---|---:|---:|---:|---:|---:|
| top 10 stations x 20 | 126 | 145 | 312 | **122** | 95 |
| `select *` x 5 | 2794 | 3653 | 4396 | **3943** | 24061 |

### Remarks

- The Quack client is a pure Pascal implementation, with no DuckDB library to
  load: small queries are dominated by the start-up time, where it wins
  against the official CLI which has to load two extensions.
- For large results, `duckdbcli` Quack is about 30% faster than the official
  CLI over Quack, but slower than the embedded engine.
- `duckdbcli` embedded loses about 30 ms at start-up (dynamic loading of
  `libduckdb.so`), and 5-30% on large results because every cell is read
  as text via the generic `mormot.db.sql` column API.
- `mormot.db.proxy` is fast for small queries, but its JSON/binary result
  encoding is much slower for large result sets.
- Results are identical, but the quoting of non-ASCII text in CSV, the row
  order of ties, and the timestamp format of the proxy.
- These numbers come from a single VM and are not a rigorous benchmark:
  re-run `bench/bench.py` and `bench/bench2.py` on your own hardware.
