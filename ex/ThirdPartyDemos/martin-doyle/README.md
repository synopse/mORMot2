# mORMot2 Tutorial Demos

Step-by-step examples for learning the mORMot2 framework - from basic ORM to real-world applications.

## Projects

| # | Project | Description |
|---|---------|-------------|
| 01 | [StandAloneORM](01-StandAloneORM/) | GUI application with local SQLite ORM |
| 02 | [HttpClientServerORM](02-HttpClientServerORM/) | Client/Server architecture via HTTP |
| 03 | [MethodBasedServices](03-MethodBasedServices/) | Server methods as REST endpoints |
| 04 | [InterfacedBasedServices](04-InterfacedBasedServices/) | Interface-based services (SOA) |
| 05 | [HttpDaemonORM](05-HttpDaemonORM/) | HTTP server as daemon/service |
| 06 | [DomainDrivenDesign](06-DomainDrivenDesign/) | DDD with repository pattern |
| 07 | [HttpDockerORM](07-HttpDockerORM/) | Docker container deployment |
| 10 | [InvoiceExample](10-InvoiceExample/) | Invoice management with service layer and DTOs |

## Requirements

- **Delphi 7+** or **Free Pascal 3.2 / Lazarus**
- mORMot2 Framework

## Build

```bash
# Free Pascal
cd <project>/src && lazbuild <project>.lpi

# Delphi
# Use the provided .dpr/.dproj files
```
