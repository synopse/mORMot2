FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libsqlite3-0 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN mkdir data log

COPY ./Project07Daemon /app/Project07Daemon

VOLUME ["/app/data", "/app/log"]

EXPOSE 11111

CMD ["/app/Project07Daemon", "--run"]
