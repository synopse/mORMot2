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
- `TSocketStream` Socket Wrapper
- Windows IOCP sockets support
- `TUri` parsing/generating URL wrapper
- `TCrtSocket` Buffered Socket Read/Write Class
- NTP / SNTP Protocol Client

The Low-Level Sockets API, which is complex and inconsistent among OS, is not made public and shouldn't be used in end-user code. This unit encapsultates all Sockets features into a single set of functions, e.g. around the TNetSocket abstract wrapper.


### mormot.net.http

HTTP/HTTPS Abstract Process Classes and Definitions
- Shared HTTP Constants and Functions
- Reusable HTTP State Machine
- `THttpSocket` Implementing HTTP over plain sockets
- Abstract Server-Side Types used e.g. for Client-Server Protocol
- HTTP Server Logging/Monitoring Processors

### mormot.net.client

HTTP Client Classes
- `THttpMultiPartStream` for multipart/formdata HTTP POST
- `THttpClientSocket` Implementing HTTP client over plain sockets
- Additional Client Protocols Support (e.g. 'file://')
- `THttpRequest` Abstract HTTP client class
- `TWinHttp` `TWinINet` `TCurlHTTP` classes
- `TSimpleHttpClient` Wrapper Class
- Cached HTTP Connection to a Remote Server
- Send Email using the `SMTP` Protocol
- `DNS` Resolution Cache for `mormot.net.sock` `NewSocket()`

### mormot.net.server

HTTP/UDP Server Classes
- Abstract UDP Server
- Custom URI Routing using an efficient Radix Tree
- Shared Server-Side HTTP Process
- `THttpServerSocket`/`THttpServer` HTTP/1.1 Server
- `THttpPeerCache` Local Peer-to-peer Cache
- `THttpApiServer` HTTP/1.1 Server Over Windows `http.sys` Module
- `THttpApiWebSocketServer` Over Windows `http.sys` Module

### mormot.net.asynch

Event-Driven Network Classes and Functions
- Low-Level Non-blocking Connections
- Client or Server Asynchronous Process
- `THttpAsyncServer` Event-Driven HTTP Server

Used e.g. by both `mormot.net.relay` and `mormot.net.rtsphttp` to handle thousands on concurrent streams, with minimal resources, in a cross-platform way.

### mormot.net.ws.core

WebSockets Abstract Processing for Client and Server
- WebSockets Frames Definitions
- WebSockets Protocols Implementation
- WebSockets Asynchronous Frames Parsing
- WebSockets Client and Server Shared Process
- `TWebSocketProtocolChat` Simple Protocol
- Socket.IO / Engine.IO Raw Protocols

### mormot.net.ws.client

WebSockets Bidirectional Client
- `TWebSocketProcessClient` Processing Class
- `THttpClientWebSockets` Bidirectional REST Client
- Socket.IO / Engine.IO Client Protocol over WebSockets

### mormot.net.ws.server

WebSockets Bidirectional Server
- `TWebSocketProcessServer` Processing Class
- `TWebSocketServerSocket` Bidirectional REST Server
- Socket.IO / Engine.IO Server Protocol over WebSockets

### mormot.net.ws.async

Asynchronous WebSockets Bidirectional Server
- `TWebSocketAsyncServer` Event-Driven HTTP/WebSockets Server
- `TWebSocketAsyncServerRest` Bidirectional REST Server

### mormot.net.relay

Secured Tunneling over WebSockets
- Low-level Shared Definitions
- Low-level WebSockets Relaying Protocols
- Public and Private relay process

It will encapsulate any WebSockets duplex stream over a public server, allowing any remote client to connect to a local server behind a firewall, using a public server (e.g. a simple Linux box) as relay.

A Private Relay client should connect to a Public Relay Server, probably behind a firewall. By definition, only a single Private Relay client instance could connect at the same time to the Public Relay server.

### mormot.net.rtsphttp

RTSP Stream Tunnelling over HTTP as defined by Apple at https://goo.gl/CX6VA3
- Low-level HTTP and RTSP Connections
- RTSP over HTTP Tunnelling 

Encapsulate a RTSP TCP/IP duplex video stream into two HTTP links, one POST for upgoing commands, and one GET for downloaded video.

### mormot.net.tftp.client

TFTP Protocol and Client with RFC 1350/2347/2348/2349/7440 Support
- TFTP Protocol Definitions

Current limitation: no Client code is defined yet - only the raw TFTP protocol.

### mormot.net.tftp.server

TFTP Server Processing with RFC 1350/2347/2348/2349/7440 Support
- TFTP Connection Thread and State Machine
- `TTftpServerThread` Server Class

Current limitation: only RRQ requests are supported/tested yet.

### mormot.net.tunnel

TCP/UDP Port Forwarding and Tunnelling
- Abstract Definitions for Port Forwarding
- Local NAT Client/Server to Tunnel TCP Streams
- WebSockets stand-alone Relay Server

### mormot.net.acme

Automatic Certificate Management Environment (ACME v2) Client
- JWS HTTP-client implementation
- ACME client implementation
- Let's Encrypt TLS / HTTPS Encryption Certificates Support
- HTTP-01 Let's Encrypt Challenges HTTP Server on port 80

### mormot.net.ldap

Simple LDAP Protocol Client
- CLDAP Client Functions
- LDIF Data Interchange Format
- LDAP Protocol Definitions
- LDAP Attributes Definitions
- LDAP Response Storage
- Main `TLdapClient` Class
- Dedicated `TLdapCheckMember` Class
- HTTP BASIC Authentication via LDAP or Kerberos

### mormot.net.dns

Simple DNS Protocol Client
- Low-Level DNS Protocol Definitions
- High-Level DNS Query

### mormot.net.openapi

OpenAPI Language-agnostic Interface to HTTP APIs
- OpenAPI Document Wrappers
- FPC/Delphi Pascal Client Code Generation

See https://blog.synopse.info/?post/2024/09/06/Swagger/OpenAPI-Client-Generator-for-Delphi-and-FPC
