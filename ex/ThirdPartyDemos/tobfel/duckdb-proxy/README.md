# duckdbproxy - DuckDB from 32-bit clients via a local Quack server

DuckDB is only available as a 64-bit library. Since DuckDB 2.0, the `quack`
extension can serve a database over HTTP with the
[Quack protocol](https://duckdb.org/2026/05/12/quack-remote-protocol), and
`mormot.db.sql.duckdb` includes `TSqlDBQuackConnectionProperties`, a pure
Pascal Quack client which also works on Win32 and Delphi 7.

So no Pascal proxy is needed: `duckdbproxy.dpr` launches the official 64-bit
`duckdb` CLI with `TExternalProcess`, sends it `CALL quack_serve(...)` via
stdin, then uses the database through the regular `TSqlDBConnectionProperties`
API.

```
duckdbproxy [options]
  --cli duckdb.exe      the 64-bit duckdb CLI executable
  -d, --db <file>       the DuckDB database file - default is in-memory
  -p, --port 9494       the local TCP port of the Quack server
```

## How it works

```pascal
server := TDuckDBQuackProcess.Create('duckdb.exe', 'data.duckdb', 9494);
try
  props := server.NewClient; // TSqlDBQuackConnectionProperties
  try
    // ... props.Execute() / props.ExecuteNoResult() as usual
  finally
    props.Free; // release the client (and its interfaces) first
  end;
finally
  server.Free; // stdin EOF: the CLI checkpoints the database and exits
end;
```

- The server only listens on `127.0.0.1`, with a random 128-bit token sent via
  stdin, so it does not appear on the process command line.
- `TExternalProcess` is started with `roCloseChildren`: the CLI is killed if
  the client process ends without stopping it.
- Other Quack clients, e.g. another DuckDB with `ATTACH 'quack:127.0.0.1:9494'`
  or any language, can connect to the same server with the token.
- The `quack` extension is downloaded by DuckDB on first use, unless it was
  installed before (`INSTALL quack`).

Tested with DuckDB 2.0.0 from Delphi 7, Delphi 13.1 (Win32 and Win64) and
FPC 3.2.3 (Win64) clients.
