# 02-HttpClientServerORM

Client/Server architecture via HTTP.

## Build

```bash
# Free Pascal
cd src
lazbuild Project02Server.lpi
lazbuild Project02Client.lpi
```

## Run

1. Start server:
```bash
# Linux
./bin/fpc/aarch64-linux/Project02Server
```

2. Start client (new terminal):
```bash
# Linux
./bin/fpc/aarch64-linux/Project02Client
```

Server listens on port **11111**.
