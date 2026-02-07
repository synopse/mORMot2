# 03-MethodBasedServices

Server methods as REST endpoints.

## Build

```bash
# Free Pascal
cd src
lazbuild Project03MethodBasedServer.lpi
lazbuild Project03MethodBasedClient.lpi
```

## Run

1. Start server:
```bash
# Linux
./bin/fpc/aarch64-linux/Project03MethodBasedServer
```

2. Start client (new terminal):
```bash
# Linux
./bin/fpc/aarch64-linux/Project03MethodBasedClient
```

Server listens on port **11111**.
