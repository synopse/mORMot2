# mORMot 2 Software Architecture Design

## Complete Index

This index provides a complete reference to all chapters of the mORMot 2 Software Architecture Design documentation.

---

## Document Information

| Item | Value |
|------|-------|
| **Framework** | Synopse mORMot 2 |
| **Document** | Software Architecture Design (SAD) |
| **Version** | 2.0 |
| **Last Updated** | December 2025 |
| **Chapters** | 26 (+ Foreword) |
| **Based On** | mORMot 1.18 SAD by Arnaud Bouchez |

---

## Table of Contents

### Foreword

- [**Foreword**](mORMot2-SAD-Foreword.md) - Document purpose, resources, quick start guide

---

### Part I: Introduction and Core Concepts

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 1 | [mORMot 2 Overview](mORMot2-SAD-Chapter-01.md) | `mORMot2-SAD-Chapter-01.md` | Framework introduction, Client-Server ORM/SOA, highlights, benefits, FAQ |
| 2 | [Architecture Principles](mORMot2-SAD-Chapter-02.md) | `mORMot2-SAD-Chapter-02.md` | MVC, Multi-tier, SOA, ORM, NoSQL/ODM, DDD patterns |
| 3 | [Meet mORMot 2 - Unit Structure](mORMot2-SAD-Chapter-03.md) | `mORMot2-SAD-Chapter-03.md` | Source organization, layers, unit naming, dependencies |
| 4 | [Core Units](mORMot2-SAD-Chapter-04.md) | `mORMot2-SAD-Chapter-04.md` | mormot.core.*, Unicode, JSON, TDocVariant, threading |

---

### Part II: Object-Relational Mapping

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 5 | [Object-Relational Mapping](mORMot2-SAD-Chapter-05.md) | `mORMot2-SAD-Chapter-05.md` | TOrm, TOrmModel, field types, relationships, validation |
| 6 | [Daily ORM](mORMot2-SAD-Chapter-06.md) | `mORMot2-SAD-Chapter-06.md` | Practical patterns, multi-tier thinking, ORM selection |

---

### Part III: Database Access

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 7 | [Database Layer](mORMot2-SAD-Chapter-07.md) | `mORMot2-SAD-Chapter-07.md` | SQLite3, virtual tables, FTS, ACID, backup |
| 8 | [External SQL Database Access](mORMot2-SAD-Chapter-08.md) | `mORMot2-SAD-Chapter-08.md` | mormot.db.sql.*, ODBC, OleDB, Oracle, PostgreSQL, ZEOS |
| 9 | [External NoSQL Database Access](mORMot2-SAD-Chapter-09.md) | `mORMot2-SAD-Chapter-09.md` | MongoDB client, ODM integration, BSON |

---

### Part IV: REST and Client-Server

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 10 | [JSON and RESTful Fundamentals](mORMot2-SAD-Chapter-10.md) | `mORMot2-SAD-Chapter-10.md` | JSON serialization, REST principles, encoding |
| 11 | [Client-Server Architecture](mORMot2-SAD-Chapter-11.md) | `mORMot2-SAD-Chapter-11.md` | Protocols, TRest classes, HTTP servers, threading |
| 12 | [Client-Server ORM Operations](mORMot2-SAD-Chapter-12.md) | `mORMot2-SAD-Chapter-12.md` | BATCH mode, caching, synchronization |
| 13 | [Server-Side ORM Processing](mORMot2-SAD-Chapter-13.md) | `mORMot2-SAD-Chapter-13.md` | Performance optimization, stored procedures, services |

---

### Part V: Services and SOA

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 14 | [Client-Server Services via Methods](mORMot2-SAD-Chapter-14.md) | `mORMot2-SAD-Chapter-14.md` | Published methods, parameters, file serving, JWT |
| 15 | [Interfaces and SOLID Design](mORMot2-SAD-Chapter-15.md) | `mORMot2-SAD-Chapter-15.md` | SOLID principles, dependency injection, mocking |
| 16 | [Client-Server Services via Interfaces](mORMot2-SAD-Chapter-16.md) | `mORMot2-SAD-Chapter-16.md` | Interface-based SOA, WebSockets, callbacks |
| 17 | [Cross-Platform Clients](mORMot2-SAD-Chapter-17.md) | `mORMot2-SAD-Chapter-17.md` | FPC, FMX, mobile, code generators |

---

### Part VI: Web Applications

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 18 | [The MVC Pattern](mORMot2-SAD-Chapter-18.md) | `mORMot2-SAD-Chapter-18.md` | Model-View-Controller, Mustache templates |
| 19 | [MVC/MVVM Web Applications](mORMot2-SAD-Chapter-19.md) | `mORMot2-SAD-Chapter-19.md` | Web framework, sessions, controllers, views |
| 20 | [Hosting and Deployment](mORMot2-SAD-Chapter-20.md) | `mORMot2-SAD-Chapter-20.md` | Linux, Docker, nginx, scaling, CDN |

---

### Part VII: Security and Advanced Topics

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 21 | [Security](mORMot2-SAD-Chapter-21.md) | `mORMot2-SAD-Chapter-21.md` | Authentication, authorization, SSPI, JWT |
| 22 | [Scripting Engine](mORMot2-SAD-Chapter-22.md) | `mORMot2-SAD-Chapter-22.md` | QuickJS JavaScript integration |
| 23 | [Asymmetric Encryption](mORMot2-SAD-Chapter-23.md) | `mORMot2-SAD-Chapter-23.md` | ECC, certificates, encryption, licensing |
| 24 | [Domain-Driven Design](mORMot2-SAD-Chapter-24.md) | `mORMot2-SAD-Chapter-24.md` | DDD patterns, CQRS, repositories, aggregates |
| 25 | [Testing and Logging](mORMot2-SAD-Chapter-25.md) | `mORMot2-SAD-Chapter-25.md` | TSynTest, TSynLog, debugging, profiling |

---

### Part VIII: Reference

| # | Chapter | File | Topics |
|---|---------|------|--------|
| 26 | [Source Code](mORMot2-SAD-Chapter-26.md) | `mORMot2-SAD-Chapter-26.md` | Licensing, installation, Delphi/FPC setup |

---

### Obsolete Chapters (from mORMot 1.18)

| # | Original Title | Status | Notes |
|---|---------------|--------|-------|
| 27 | mORMot Framework Source | Superseded | See Chapters 3, 4, 26 |
| 28 | SynFile Application | Obsolete | Deprecated UI components |
| 29 | Main SynFile Demo Source | Obsolete | SynFile not ported |
| 30 | SWRS Implications | Obsolete | Outdated references |

See [Chapters 27-30 Status](mORMot2-SAD-Chapters-27-30-Obsolete.md) for details.

---

## Quick Reference by Topic

### Getting Started
- Installation: [Chapter 26](mORMot2-SAD-Chapter-26.md)
- Unit organization: [Chapter 3](mORMot2-SAD-Chapter-03.md)
- Core utilities: [Chapter 4](mORMot2-SAD-Chapter-04.md)

### ORM/Database
- TOrm basics: [Chapter 5](mORMot2-SAD-Chapter-05.md)
- Daily patterns: [Chapter 6](mORMot2-SAD-Chapter-06.md)
- SQLite3: [Chapter 7](mORMot2-SAD-Chapter-07.md)
- External databases: [Chapter 8](mORMot2-SAD-Chapter-08.md)
- MongoDB: [Chapter 9](mORMot2-SAD-Chapter-09.md)

### Client-Server
- REST/JSON: [Chapter 10](mORMot2-SAD-Chapter-10.md)
- Architecture: [Chapter 11](mORMot2-SAD-Chapter-11.md)
- Operations: [Chapter 12](mORMot2-SAD-Chapter-12.md)
- Server processing: [Chapter 13](mORMot2-SAD-Chapter-13.md)

### Services/SOA
- Method services: [Chapter 14](mORMot2-SAD-Chapter-14.md)
- Interfaces/SOLID: [Chapter 15](mORMot2-SAD-Chapter-15.md)
- Interface services: [Chapter 16](mORMot2-SAD-Chapter-16.md)
- Cross-platform: [Chapter 17](mORMot2-SAD-Chapter-17.md)

### Web Development
- MVC pattern: [Chapter 18](mORMot2-SAD-Chapter-18.md)
- Web apps: [Chapter 19](mORMot2-SAD-Chapter-19.md)
- Hosting: [Chapter 20](mORMot2-SAD-Chapter-20.md)

### Security & Advanced
- Security: [Chapter 21](mORMot2-SAD-Chapter-21.md)
- Scripting: [Chapter 22](mORMot2-SAD-Chapter-22.md)
- Encryption: [Chapter 23](mORMot2-SAD-Chapter-23.md)
- DDD: [Chapter 24](mORMot2-SAD-Chapter-24.md)
- Testing/Logging: [Chapter 25](mORMot2-SAD-Chapter-25.md)

---

## Key Classes Reference

| mORMot 2 Class | Purpose | Chapter |
|----------------|---------|---------|
| `TOrm` | Base ORM class | 5 |
| `TOrmModel` | Data model definition | 5 |
| `TRest` | REST base class | 11 |
| `TRestServerDB` | SQLite3 REST server | 7, 11 |
| `TRestHttpServer` | HTTP server wrapper | 11 |
| `TRestHttpClient` | HTTP client | 11 |
| `IInvokable` | Service interface base | 16 |
| `TSynLog` | Logging framework | 25 |
| `TSynTest` | Testing framework | 25 |
| `TDocVariant` | JSON/variant documents | 4 |
| `TMVCApplication` | MVC controller base | 19 |

---

## Key Units Reference

| Unit Pattern | Purpose | Chapter |
|--------------|---------|---------|
| `mormot.core.*` | Core utilities | 4 |
| `mormot.orm.*` | ORM layer | 5, 6 |
| `mormot.db.sql.*` | SQL database access | 8 |
| `mormot.db.nosql.*` | NoSQL database access | 9 |
| `mormot.rest.*` | REST client/server | 11-13 |
| `mormot.soa.*` | Service-oriented architecture | 16 |
| `mormot.net.*` | Network layer | 11 |
| `mormot.crypt.*` | Cryptography | 21, 23 |
| `mormot.app.*` | Application helpers | 20 |
| `mormot.script.*` | Scripting support | 22 |

---

## Migration from mORMot 1.18

| mORMot 1 | mORMot 2 | Notes |
|----------|----------|-------|
| `TSQLRecord` | `TOrm` | See Chapter 5 |
| `TSQLRest` | `TRest` | See Chapter 11 |
| `TSQLModel` | `TOrmModel` | See Chapter 5 |
| `SynCommons.pas` | `mormot.core.*.pas` | See Chapter 3 |
| `mORMot.pas` | Multiple units | See Chapter 3 |
| `SynDB*.pas` | `mormot.db.*.pas` | See Chapter 8 |
| SpiderMonkey | QuickJS | See Chapter 22 |
| `mORMotUI*` | (deprecated) | Use MVC/web |

---

## External Resources

- **GitHub**: https://github.com/synopse/mORMot2
- **Forum**: https://synopse.info/forum/viewforum.php?id=24
- **Documentation**: https://synopse.info/files/doc/mORMot2.html
- **Blog**: https://blog.synopse.info
- **Samples**: `ex/` folder in repository

---

*Last updated: December 2025*
