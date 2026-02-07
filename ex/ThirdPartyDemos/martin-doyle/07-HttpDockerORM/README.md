# 07-HttpDockerORM

Docker container deployment.

## Build

```bash
# Free Pascal
cd src
lazbuild Project07HttpDaemon.lpi
lazbuild Project07HttpClient.lpi
```

## Docker

Build image:
```bash
cd bin/fpc/x86_64-linux
cp ../../../docker/Dockerfile .
docker build -t project07daemon .
```

Run container:
```bash
mkdir -p test/data test/log
docker run -d -p 11111:11111 --name project07daemon \
  -v "$(pwd)"/test/data:/app/data \
  -v "$(pwd)"/test/log:/app/log \
  project07daemon
```

See `docker/DockerCommands.md` for more commands.
