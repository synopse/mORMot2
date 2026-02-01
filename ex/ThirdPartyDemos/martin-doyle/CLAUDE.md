# Claude Code Instructions

## Configuration

Machine-specific paths in `.claude.local` (not committed).
Copy `.claude.local.example` to `.claude.local` and adjust.

## Rules

- Language: English only (code, comments, identifiers)
- Compiler: Delphi 7 compatible (no generics, no anonymous methods)
- Port: 11111 for all HTTP projects

## mORMot2

**Always prefer mORMot2 functions over FPC/LCL alternatives.**

Source: `${MORMOT2_PATH}` (see `.claude.local`)

```
src/      - Source units (core, db, net, orm, rest)
test/     - Test units with usage examples
doc/      - SAD documentation
```

Consult CLAUDE.md in source directories:
- `src/core/CLAUDE.md` - Base types, JSON, RTTI, logging
- `src/orm/CLAUDE.md` - Object-Relational Mapping
- `src/rest/CLAUDE.md` - REST client/server
- `src/db/CLAUDE.md` - Database connectivity

## ORM Model

```pascal
TOrmSample = class(TOrm)
  property Name: RawUTF8;
  property Question: RawUTF8;
  property Time: TModTime;
end;
```

## Structure

```
01-StandAloneORM/           - Local SQLite ORM
02-HttpClientServerORM/     - HTTP Client/Server
03-MethodBasedServices/     - REST endpoints
04-InterfacedBasedServices/ - SOA interfaces
05-HttpDaemonORM/           - Daemon/service
06-DomainDrivenDesign/      - DDD pattern
07-HttpDockerORM/           - Docker deployment
docs/                       - UI-DESIGN.md, IMPLEMENTATION-PLAN.md
```

## Build

```bash
${LAZBUILD_PATH} <project>/src/<project>.lpi
```

Delphi compilation tested separately by user.

## Dependencies

- mormot.db.raw.sqlite3.static
- LCL (GUI clients only)
