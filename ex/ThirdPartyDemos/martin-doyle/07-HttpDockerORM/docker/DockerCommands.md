# Run mORMot daemon in Docker

## Go to the folder where your executable is

## Build Docker image

```bash
sudo docker build -t project07daemon .
```

## Create log and data folders

```bash
mkdir test
cd test
mkdir data
mkdir log
```

## Run Docker container

```bash
sudo docker run -d -p 11111:11111 --name project07daemon -v "$(pwd)"/test/data:/app/data -v "$(pwd)"/test/log:/app/log project07daemon
```

## Check if Docker container runs

```bash
sudo docker container ls -a
```

## Test Daemon with Client

```bash
Project07Client
```

## Stop Docker container

```bash
sudo docker container stop project07daemon
```

## Remove Docker container

```bash
sudo docker container rm project07daemon
```

## Check Docker images

```bash
sudo docker image ls
```

## Remove Docker image

```bash
sudo docker image rm project07daemon
```
