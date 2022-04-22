# mORMot Network Communication Layer

## Folder Content

This folder gives access to the *Network Communication Layer* used by the *mORMot* Open Source framework, version 2.

## Network Communication

All `mormot.net.*.pas` units define client/server communication as used by our framework, especially for its REST features, over sockets, HTTP and WebSockets. 

## Units Presentation

### mormot.net.sock

Cross-Platform Raw Sockets API Definition
- Socket Process High-Level Encapsulation
- MAC and IP Addresses Support
- TLS / HTTPS Encryption Abstract Layer
- Efficient Multiple Sockets Polling
- `TUri` parsing/generating URL wrapper
- `TCrtSocket` Buffered Socket Read/Write Class

The Low-Level Sockets API is encapsultated into a single set of functions, and wrapped around a `TNetSocket` abstract helper, and never made public.

### mormot.net.http

HTTP/HTTPS Abstract Process Classes and Definitions
- Shared HTTP Constants and Functions
- `THttpSocket` Implementing HTTP over plain sockets
- Abstract Server-Side Types used e.g. for Client-Server Protocol

### mormot.net.client

HTTP Client Classes
- `THttpMultiPartStream` for multipart/formdata HTTP POST
- `THttpClientSocket` Implementing HTTP client over plain sockets
- `THttpRequest` Abstract HTTP client class
- `TWinHttp` `TWinINet` `TWinHttpWebSocketClient` `TCurlHTTP`
- `TSimpleHttpClient` Wrapper Class
- Cached HTTP Connection to a Remote Server
- Send Email using the `SMTP` Protocol
- `DNS` Resolution Cache for `mormot.net.sock` `NewSocket()`

### mormot.net.server

HTTP Server Classes
- Shared Server-Side HTTP Process
- `THttpServerSocket`/`THttpServer` HTTP/1.1 Server
- `THttpApiServer` HTTP/1.1 Server Over Windows `http.sys` Module
- `THttpApiWebSocketServer` Over Windows `http.sys` Module

### mormot.net.asynch

Event-Driven Network Classes and Functions
- Low-Level Non-blocking Connections
- Client or Server Asynchronous Process

Used e.g. by both `mormot.net.relay` and `mormot.net.rtsphttp` to handle thousands on concurrent streams, with minimal resources, in a cross-platform way.

### mormot.net.ws.core

WebSockets Abstract Processing for Client and Server
- WebSockets Frames Definitions
- WebSockets Protocols Implementation
- WebSockets Client and Server Shared Process
- `TWebSocketProtocolChat` Simple Protocol

### mormot.net.ws.client

WebSockets Bidirectional Client
- `TWebSocketProcessClient` Processing Class
- `THttpClientWebSockets` Bidirectional REST Client

### mormot.net.ws.server

WebSockets Bidirectional Server
- `TWebSocketProcessServer` Processing Class
- `TWebSocketServerSocket` Bidirectional REST Server

### mormot.net.ws.async

Asynchronous WebSockets Bidirectional Server
- `TWebSocketAsyncServer` Event-Driven HTTP/WebSockets Server
- `TWebSocketAsyncServerRest` Bidirectional REST Server

### mormot.net.relay

Secured Tunneling over WebSockets
- Low-level Shared Definitions
- Low-level WebSockets Relaying Protocols
- Public and Private relay process

### mormot.net.rtsphttp

RTSP Stream Tunnelling over HTTP as defined by Apple at https://goo.gl/CX6VA3
- Low-level HTTP and RTSP Connections
- RTSP over HTTP Tunnelling 

Encapsulate a RTSP TCP/IP duplex video stream into two HTTP links, one POST for upgoing commands, and one GET for downloaded video.

### mormot.net.tftp.client

TFTP Protocol and Client with RFC 1350/2347/2348/2349/7440 Support
- TFTP Protocol Definitions

Current limitation: no Client code is defined yet - only the raw TFTP protocol.

### mormot.net.tftp.server;

TFTP Server Processing with RFC 1350/2347/2348/2349/7440 Support
- Abstract UDP Server
- TFTP Connection Thread and State Machine
- `TTftpServerThread` Server Class

Current limitation: only RRQ requests are supported/tested yet.
