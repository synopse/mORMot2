FROM alpine:latest

RUN apk add --no-cache libc6-compat sqlite-libs

WORKDIR /app


RUN mkdir data
RUN mkdir log

COPY /Project07Daemon /app/Project07Daemon

VOLUME ["/app/data","app/log"]

EXPOSE 11111

CMD ["/app/Project07Daemon", "--run"]
