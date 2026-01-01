# mORMot 2 Software Architecture Design

**Foreword**

---

## About This Document

| Item | Description |
|------|-------------|
| **Project Name** | Synopse mORMot 2 Framework |
| **Document Name** | Software Architecture Design (SAD) |
| **Document Version** | 2.0 |
| **Date** | December 2025 |
| **Author** | Based on original work by Arnaud Bouchez |
| **Adapted for mORMot 2** | Claude AI with human oversight |

---

## Document Purpose

The *Software Architecture Design* document describes the architecture, design patterns, and implementation guidelines for the *Synopse mORMot 2 Framework*. It serves as both a learning resource for new developers and a reference guide for experienced users.

This document has been adapted from the original mORMot 1.18 SAD to reflect the complete rewrite that is mORMot 2, including:
- New unit organization and naming
- Updated class names and APIs
- New features and capabilities
- Removed deprecated functionality

---

## Document License

**Synopse mORMot 2 Framework Documentation**
Copyright (C) 2008-2026 Arnaud Bouchez
Synopse Informatique - https://synopse.info

The *Synopse mORMot 2 Framework Source Code* is licensed under GPL / LGPL / MPL licensing terms, free to be included in any application.

The *Synopse mORMot 2 Framework Documentation* is a free document, released under a GPL 3.0 License, distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

---

## Trademark Notice

Rather than indicating every occurrence of a trademarked name as such, this document uses the names only in an editorial fashion and to the benefit of the trademark owner with no intention of infringement of the trademark.

---

## Resources

| Resource | URL |
|----------|-----|
| **Overview** | [README.md](https://github.com/synopse/mORMot2/blob/master/README.md) |
| **Download** | [GitHub Releases](https://github.com/synopse/mORMot2/releases) |
| **API Reference** | [Official Documentation](https://synopse.info/files/doc/mORMot2.html) |
| **Forum** | [Synopse Forum](https://synopse.info/forum/viewforum.php?id=24) |
| **Blog** | [Synopse Blog](https://blog.synopse.info) |
| **Telegram** | [mORMot Telegram Group](https://t.me/synopse_mormot) |
| **Discord** | [mORMot Discord Server](https://discord.gg/BcmcpY6afj) |
| **Donate** | [Sponsor mORMot 2](https://github.com/synopse/mORMot2/blob/master/DONATE.md) |
| **License Terms** | [LICENCE.md](https://github.com/synopse/mORMot2/blob/master/LICENCE.md) |

---

## Table of Contents

### Part I: Introduction and Core Concepts

| Chapter | Title | Description |
|---------|-------|-------------|
| 1 | mORMot 2 Overview | Framework introduction, features, and benefits |
| 2 | Architecture Principles | Design patterns: MVC, SOA, ORM, DDD |
| 3 | Meet mORMot 2 - Unit Structure | Source code organization and layers |
| 4 | Core Units | mormot.core.* functionality |

### Part II: Object-Relational Mapping

| Chapter | Title | Description |
|---------|-------|-------------|
| 5 | Object-Relational Mapping | TOrm, TOrmModel, field types |
| 6 | Daily ORM | Practical ORM patterns and best practices |

### Part III: Database Access

| Chapter | Title | Description |
|---------|-------|-------------|
| 7 | Database Layer | SQLite3, virtual tables, FTS |
| 8 | External SQL Database Access | mormot.db.sql.* - RDBMS integration |
| 9 | External NoSQL Database Access | MongoDB ODM integration |

### Part IV: REST and Client-Server

| Chapter | Title | Description |
|---------|-------|-------------|
| 10 | JSON and RESTful Fundamentals | JSON serialization, REST concepts |
| 11 | Client-Server Architecture | Protocols, TSQLRest classes |
| 12 | Client-Server ORM Operations | BATCH, caching, synchronization |
| 13 | Server-Side ORM Processing | Performance, stored procedures |

### Part V: Services and SOA

| Chapter | Title | Description |
|---------|-------|-------------|
| 14 | Client-Server Services via Methods | Published methods, parameters |
| 15 | Interfaces and SOLID Design | SOLID principles, DI, mocking |
| 16 | Client-Server Services via Interfaces | Interface-based SOA |
| 17 | Cross-Platform Clients | FPC, mobile, JavaScript clients |

### Part VI: Web Applications

| Chapter | Title | Description |
|---------|-------|-------------|
| 18 | The MVC Pattern | Model-View-Controller, Mustache |
| 19 | MVC/MVVM Web Applications | Web framework implementation |
| 20 | Hosting and Deployment | Production deployment strategies |

### Part VII: Security and Advanced Topics

| Chapter | Title | Description |
|---------|-------|-------------|
| 21 | Security | Authentication, authorization, sessions |
| 22 | Scripting Engine | QuickJS JavaScript integration |
| 23 | Asymmetric Encryption | ECC, certificates, encryption |
| 24 | Domain-Driven Design | DDD patterns with mORMot 2 |
| 25 | Testing and Logging | TSynTest, TSynLog, debugging |

### Part VIII: Reference

| Chapter | Title | Description |
|---------|-------|-------------|
| 26 | Source Code | Installation, licensing, setup |

---

## What's New in mORMot 2

mORMot 2 is a complete rewrite of the framework. Key changes include:

### Naming Changes

| mORMot 1 | mORMot 2 | Notes |
|----------|----------|-------|
| `TSQLRecord` | `TOrm` | Base ORM class |
| `TSQLRest` | `TRest` | REST client/server base |
| `TSQLModel` | `TOrmModel` | Data model definition |
| `SynCommons.pas` | `mormot.core.*.pas` | Split into ~24 units |
| `mORMot.pas` | `mormot.orm.*` + `mormot.rest.*` | Separated layers |
| `SynDB*.pas` | `mormot.db.*.pas` | Database access |

### New Features

- **OpenSSL Integration**: Full OpenSSL 1.1/3.0 support for cryptography
- **Async Servers**: Non-blocking HTTP and WebSocket servers
- **QuickJS Scripting**: Modern JavaScript engine (replaces SpiderMonkey)
- **Let's Encrypt**: Automatic HTTPS certificate management
- **libdeflate**: Faster compression alternative
- **Modern Syntax**: Optional generics and enumerators

### Deprecated Features

The following mORMot 1 features are not available in mORMot 2:

- **RTTI-based UI Generation**: mORMotUI*, mORMotToolBar, mORMotUIEdit
- **BigTable**: Large binary storage
- **LVCL**: Light VCL replacement
- **Kylix/CrossKylix**: Linux via Kylix compiler
- **Delphi 5-6**: Minimum is now Delphi 7
- **SpiderMonkey**: Replaced by QuickJS
- **SynFile Demo**: No equivalent demo application

---

## Quick Start

### Installation Summary

1. **Clone repository**: `git clone https://github.com/synopse/mORMot2.git`
2. **Download static files**: Extract `mormot2static.7z` to `static/` folder
3. **Configure IDE**:
   - **Lazarus**: Open `packages/lazarus/mormot2.lpk`
   - **Delphi**: Add library paths via `$(mormot2)` environment variable
4. **Verify**: Compile and run `test/mormot2tests.dpr`

### First Steps

```pascal
uses
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.sqlite3;

type
  TOrmPerson = class(TOrm)
  private
    fName: RawUtf8;
    fAge: Integer;
  published
    property Name: RawUtf8 read fName write fName;
    property Age: Integer read fAge write fAge;
  end;

var
  Model: TOrmModel;
  Server: TRestServerDB;
  Person: TOrmPerson;
begin
  Model := TOrmModel.Create([TOrmPerson]);
  Server := TRestServerDB.Create(Model, 'test.db');
  try
    Server.CreateMissingTables;

    Person := TOrmPerson.Create;
    try
      Person.Name := 'John';
      Person.Age := 30;
      Server.Add(Person, true);
    finally
      Person.Free;
    end;
  finally
    Server.Free;
    Model.Free;
  end;
end.
```

---

## Learning Path

For those new to mORMot 2, we recommend the following reading order:

1. **Chapters 1-2**: Understand the framework philosophy
2. **Chapters 3-4**: Learn the unit structure and core utilities
3. **Chapters 5-6**: Master the ORM layer
4. **Chapters 10-12**: Understand client-server architecture
5. **Chapters 14-16**: Explore service-oriented features
6. **Chapter 26**: Set up your development environment

Then, based on your needs:
- **Database Integration**: Chapters 7-9
- **Web Development**: Chapters 18-20
- **Security**: Chapter 21
- **Advanced Patterns**: Chapters 22-25

---

## Sample Applications

The `ex/` folder contains many examples:

| Sample | Description |
|--------|-------------|
| `ex/ThirdPartyDemos/tbo/` | Thomas Tutorials - step-by-step learning |
| `ex/ThirdPartyDemos/martin-doyle/` | Various feature demonstrations |
| `ex/mvc-blog/` | MVC web application example |

---

## Acknowledgments

- **Arnaud Bouchez**: Original framework author and architect
- **All Contributors**: See [CONTRIBUTORS.md](https://github.com/synopse/mORMot2/blob/master/CONTRIBUTORS.md)
- **The Community**: Forum members, bug reporters, and documentation contributors

---

*Adopt a mORMot!*

---

**Continue to [Chapter 1: mORMot 2 Overview](mORMot2-SAD-Chapter-01.md)**

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| - | [Index](mORMot2-SAD-Index.md) | [Chapter 1: mORMot 2 Overview](mORMot2-SAD-Chapter-01.md) |
