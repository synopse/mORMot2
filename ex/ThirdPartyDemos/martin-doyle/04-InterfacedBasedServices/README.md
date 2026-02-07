# 04-InterfacedBasedServices

Interface-based services (SOA).

## Build

```bash
# Free Pascal
cd src
lazbuild Project04InterfaceBasedServer.lpi
lazbuild Project04InterfaceBasedClient.lpi
```

## Run

1. Start server:
```bash
# Linux
./bin/fpc/aarch64-linux/Project04InterfaceBasedServer
```

2. Start client (new terminal):
```bash
# Linux
./bin/fpc/aarch64-linux/Project04InterfaceBasedClient
```

Server listens on port **11111**.
