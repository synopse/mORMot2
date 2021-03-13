# Run mORMot daemon in Docker

## Go to the folder where your executable is

## Build Docker image

sudo docker build -t project07daemon .

## Create log and data folders

mkdir test
cd test
mkdir data
mkdir log

## Run Docker container

sudo docker run -d -p 11111:11111 --name project07daemon -v "$(pwd)"/test/data:/app/data -v "$(pwd)"/test/log:/app/log project07daemon

## Check if Docker container runs

sudo docker container ls -a

## Test Daemon with Client

Project07Client

## Stop Docker container

sudo docker container stop project07daemon

## Remove Docker container

sudo docker container rm project07daemon

## Check Docker images

sudo docker image ls

## Remove Docker image

sudo docker image rm project07daemon
