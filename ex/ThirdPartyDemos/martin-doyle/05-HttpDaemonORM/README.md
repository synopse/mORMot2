# 05-HttpDaemonORM

HTTP server as daemon/service.

## Build

```bash
# Free Pascal
cd src
lazbuild Project05HttpDaemon.lpi
lazbuild Project05HttpClient.lpi
```

## Run

Start daemon in foreground:
```bash
#Linux
./bin/fpc/aarch64-linux/Project05HttpDaemon --run
```

Start client (new terminal):
```bash
# Linux
./bin/fpc/aarch64-linux/Project05HttpClient
```

Use `--help` for all daemon options (install, start, stop, etc.).
