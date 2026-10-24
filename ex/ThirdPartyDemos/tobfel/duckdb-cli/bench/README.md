# duckdbcli vs the official duckdb CLI

Setup, in a folder holding `duckdb` (official CLI), `libduckdb.so` (same version),
the compiled `duckdbcli` and `proxysrv`:

```
curl -O https://blobs.duckdb.org/train_services.parquet
duckdb rail_catalog.duckdb -c "CREATE VIEW services AS SELECT * FROM read_parquet('$PWD/train_services.parquet')"
python3 quacksrv.py 3600 &     # Quack server (token tok1234, port 9500), catalog read-only
./proxysrv &                   # mormot.db.proxy server (port 8898), catalog read-only
python3 bench.py 9             # one process per query
python3 bench2.py              # several statements in a single process
```

The catalog is a view-only database as in
https://duckdb.org/2026/10/07/view-only-mode (here over a local copy of the
Parquet file, to not measure the network).
