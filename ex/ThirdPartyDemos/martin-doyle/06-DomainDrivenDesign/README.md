# 06-DomainDrivenDesign

DDD with repository pattern.

## Build

```bash
# Free Pascal
cd src
lazbuild Project06DDDServer.lpi
lazbuild Project06DDDClient.lpi
```

## Run

1. Start server:
```bash
# Linux
./bin/fpc/aarch64-linux/Project06DDDServer
```

2. Start client (new terminal):
```bash
# Linux
./bin/fpc/aarch64-linux/Project06DDDClient
```

Server listens on port **11111**.
