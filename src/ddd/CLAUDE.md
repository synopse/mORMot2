# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This directory contains conceptual documentation for **Domain-Driven Design (DDD)** patterns as implemented using the mORMot framework. While this folder itself has minimal implementation, mORMot's existing ORM, SOA, and REST layers fully support DDD architectural patterns.

**📖 Comprehensive DDD Documentation**: See [SAD Chapter 24: Domain-Driven Design](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-24.md) for complete coverage including:
- Value Objects, Entities, and Aggregates
- Repository Pattern with `IRestOrm`
- Domain Services and Application Services
- Unit of Work with `TRestBatch`
- Event-Driven Design patterns
- Clean Architecture layer mapping
- Testing DDD code

## Domain-Driven-Design vs Kingdom-Driven-Design

From the README:

- **DDD (Domain-Driven-Design)**: A set of paranoid architecture and design rules for writing highly-maintainable applications
- **KDD (Kingdom-Driven-Design)**: mORMot's own less paranoid set of design rules, leveraging framework abstractions like ORM and SOA

## Directory Status

This folder currently contains:
- ✅ README.md with conceptual overview
- ✅ Patterns documented in SAD Chapter 24
- ❌ No Pascal source files (`.pas`) - patterns implemented in other modules

## Relevant mORMot Features for DDD/KDD

While this directory is empty, mORMot2 provides DDD/KDD-oriented features elsewhere:

1. **Domain Layer** - Use `TOrm` classes in `src/orm` for domain entities
2. **Service Layer** - Use `IInvokable` interfaces in `src/soa` for application services
3. **Repository Pattern** - Use `IRestOrm` interface for data access abstraction
4. **CQRS Support** - Use `TRestBatch` for command batching
5. **Event Sourcing** - Implement via `TOrm` with JSON document storage
6. **Aggregates** - Model using `TOrm` inheritance and `TRecordReference` fields

## Typical DDD Architecture with mORMot

```
Domain Layer (Pure Pascal)
  └─> TOrm entities with business logic

Application Layer (Services)
  └─> IInvokable interfaces (src/soa)

Infrastructure Layer
  └─> IRestOrm repositories (src/orm)
  └─> TSqlDBConnection (src/db)

Presentation Layer
  └─> TRestHttpServer (src/rest)
  └─> MVC views (src/core/mormot.core.mvc.pas)
```

## Related Documentation

- [ORM Layer](../orm/CLAUDE.md) - Domain entities and repositories
- [SOA Layer](../soa/CLAUDE.md) - Application services
- [REST Layer](../rest/CLAUDE.md) - HTTP/API infrastructure
- [Core Interfaces](../core/CLAUDE.md) - See `mormot.core.interfaces.pas` for IoC/DI support

## Note for Developers

If implementing DDD patterns in your mORMot application, use the existing framework components rather than creating new abstractions in this folder. The mORMot architecture already aligns well with DDD principles through its ORM, SOA, and REST layers.
