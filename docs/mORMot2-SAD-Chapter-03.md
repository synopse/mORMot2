# 3. Meet mORMot 2 - Unit Structure

*Enter the new territory*

mORMot 2 represents a complete restructuring of the original framework. The monolithic units (`SynCommons.pas`, `mORMot.pas`) have been split into focused, layered units following SOLID principles.

This chapter introduces the new unit organization and helps you understand which units to include for your specific needs.

---

## 3.1. Layered Architecture Overview

mORMot 2 organizes its ~130 units into 6 dependency layers:

```
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 5: Application & Tools                                       │
│  ┌─────┬─────┬────────┬──────┬───────┬─────┐                        │
│  │ app │ ui  │ script │ misc │ tools │ ddd │                        │
│  └─────┴─────┴────────┴──────┴───────┴─────┘                        │
└─────────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 4: ORM / REST / SOA                                          │
│  ┌─────┬──────┬─────┐                                               │
│  │ orm │ rest │ soa │                                               │
│  └─────┴──────┴─────┘                                               │
└─────────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 3: Database Access                                           │
│  ┌────┐                                                             │
│  │ db │                                                             │
│  └────┘                                                             │
└─────────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 2: Networking                                                │
│  ┌─────┐                                                            │
│  │ net │                                                            │
│  └─────┘                                                            │
└─────────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 1: System Libraries & Cryptography                           │
│  ┌─────┬────────┐                                                   │
│  │ lib │ crypt  │                                                   │
│  └─────┴────────┘                                                   │
└─────────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────────┐
│  Layer 0: Foundation (RTL-Only)                                     │
│  ┌──────┐                                                           │
│  │ core │  (24 units - no mORMot dependencies)                      │
│  └──────┘                                                           │
└─────────────────────────────────────────────────────────────────────┘
```

**Key Principle**: Lower layers never depend on higher ones. This ensures:
- Incremental compilation
- Selective deployment
- Modular testing
- Clear separation of concerns

---

## 3.2. Unit Naming Convention

All units follow a consistent naming pattern:

```
mormot.<layer>.<feature>.pas
```

Examples:
- `mormot.core.json.pas` - Core layer, JSON feature
- `mormot.orm.core.pas` - ORM layer, core functionality
- `mormot.db.sql.postgres.pas` - Database layer, PostgreSQL SQL connector

### Location Rules

Units are located in folders matching their layer:
- `src/core/mormot.core.*.pas`
- `src/orm/mormot.orm.*.pas`
- `src/db/mormot.db.*.pas`
- etc.

---

## 3.3. Layer 0: Core Foundation (mormot.core.*)

**24 units** providing RTL-level functionality with **zero mORMot dependencies**.

### Dependency Chain

```
mormot.core.base (RTL types, ASM stubs, no dependencies)
  └─► mormot.core.os (OS abstraction, threading)
      └─► mormot.core.unicode (charset/encoding)
          └─► mormot.core.text (text parsing, formatting)
              └─► mormot.core.datetime (ISO-8601, TTimeLog)
                  └─► mormot.core.rtti (RTTI wrapper)
                      └─► mormot.core.buffers (compression, base64)
                          └─► mormot.core.data (TDynArray, serialization)
                              └─► mormot.core.json (JSON parsing)
                                  └─► [higher units: variants, log, etc.]
```

### Key Core Units

| Unit | Purpose | Key Types |
|------|---------|-----------|
| `mormot.core.base` | Foundation types, ASM | `RawUtf8`, `PtrInt`, `TDynArray` basics |
| `mormot.core.os` | OS abstraction | `TSynLocker`, `GetTickCount64` |
| `mormot.core.os.security` | Security-related OS functions | OS security abstractions |
| `mormot.core.os.mac` | macOS platform support | macOS-specific implementations |
| `mormot.core.unicode` | Charset conversion | `Utf8ToWideString`, `WinAnsiToUtf8` |
| `mormot.core.text` | Text processing | `FormatUtf8`, CSV parsing |
| `mormot.core.rtti` | RTTI abstraction | `TRttiCustom`, `PRttiInfo` |
| `mormot.core.json` | JSON handling | `TJsonWriter`, `GetJsonField` |
| `mormot.core.data` | Data structures | `TDynArray`, `TDynArrayHashed` |
| `mormot.core.collections` | Advanced collection types | Collection algorithms |
| `mormot.core.variants` | Dynamic documents | `TDocVariant`, `IDocDict` |
| `mormot.core.log` | Logging framework | `TSynLog`, `ISynLog` |
| `mormot.core.perf` | Performance monitoring | Performance counters |
| `mormot.core.search` | Search and indexing | Search functionality |
| `mormot.core.threads` | Threading utilities | `TSynBackgroundThread` |
| `mormot.core.zip` | ZIP archive support | ZIP compression |
| `mormot.core.test` | Testing framework | `TSynTestCase` |
| `mormot.core.mustache` | Template engine | `TSynMustache` |
| `mormot.core.mvc` | MVC pattern support | MVC architecture types |
| `mormot.core.interfaces` | Interface support | DI/IoC container |
| `mormot.core.fpclibcmm` | FPC libc memory manager | FPC memory management |
| `mormot.core.fpcx64mm` | FPC x64 memory manager | FPC x64 memory management |

### Minimal Core Usage

```pascal
uses
  mormot.core.base,    // Foundation types
  mormot.core.os,      // OS abstraction
  mormot.core.text,    // Text utilities
  mormot.core.json;    // JSON support

var
  doc: TDocVariantData;
begin
  doc.InitJson('{"name":"John","age":30}');
  WriteLn(doc.U['name']);  // Output: John
end;
```

---

## 3.4. Layer 1: Libraries & Cryptography

### External Libraries (mormot.lib.*)

**14 units** wrapping external libraries:

| Unit | External Library |
|------|------------------|
| `mormot.lib.z` | zlib/libdeflate compression |
| `mormot.lib.openssl11` | OpenSSL 1.1/3.x |
| `mormot.lib.curl` | libcurl HTTP client |
| `mormot.lib.sspi` | Windows SSPI (Kerberos) |
| `mormot.lib.gssapi` | POSIX GSSAPI |
| `mormot.lib.quickjs` | QuickJS JavaScript engine |
| `mormot.lib.winhttp` | Windows WinHTTP |
| `mormot.lib.pkcs11` | Hardware security modules |
| `mormot.lib.static` | Static library utilities |
| `mormot.lib.gdiplus` | Windows GDI+ wrapper |
| `mormot.lib.uniscribe` | Windows Uniscribe text rendering |
| `mormot.lib.win7zip` | 7-Zip library wrapper |
| `mormot.lib.lizard` | Lizard compression |
| `mormot.lib.zstd` | Zstandard compression |

### Cryptography (mormot.crypt.*)

**10 units** for cryptographic operations:

| Unit | Purpose |
|------|---------|
| `mormot.crypt.core` | AES, SHA-2, SHA-3, HMAC, PBKDF2 |
| `mormot.crypt.secure` | High-level factories, password hashing |
| `mormot.crypt.ecc` | Elliptic Curve Cryptography |
| `mormot.crypt.ecc256r1` | secp256r1 curve implementation |
| `mormot.crypt.rsa` | RSA encryption/signatures |
| `mormot.crypt.jwt` | JSON Web Tokens |
| `mormot.crypt.x509` | X.509 certificates |
| `mormot.crypt.openssl` | OpenSSL wrapper |
| `mormot.crypt.other` | Additional cryptographic utilities |

### Cryptography Factory Pattern

```pascal
uses
  mormot.crypt.secure;

// High-level factories (recommended)
var
  h: TCryptHasher;
  c: ICryptCipher;
begin
  // Hashing - use Hasher() to get TCryptHasher with Full() method
  h := Hasher('sha256');
  digest := h.Full(pointer(data), length(data));

  // Encryption
  c := Cipher('aes-256-ctr', @key[1], {encrypt:}true);
  c.Process(plain, encrypted, '');
end;
```

---

## 3.5. Layer 2: Networking (mormot.net.*)

**18 units** for network communication:

### Socket & HTTP

| Unit | Purpose |
|------|---------|
| `mormot.net.sock` | Cross-platform socket abstraction |
| `mormot.net.http` | HTTP protocol state machine |
| `mormot.net.client` | HTTP clients (Socket, WinHTTP, curl) |
| `mormot.net.server` | HTTP servers (threaded, http.sys) |
| `mormot.net.async` | Event-driven async I/O |

### WebSockets

| Unit | Purpose |
|------|---------|
| `mormot.net.ws.core` | WebSocket protocol core |
| `mormot.net.ws.client` | WebSocket client |
| `mormot.net.ws.server` | WebSocket server |
| `mormot.net.ws.async` | Async WebSocket server |

### Specialized Protocols

| Unit | Purpose |
|------|---------|
| `mormot.net.dns` | DNS resolution |
| `mormot.net.ldap` | LDAP client |
| `mormot.net.acme` | ACME/Let's Encrypt |
| `mormot.net.relay` | Firewall traversal |
| `mormot.net.openapi` | OpenAPI client generator |
| `mormot.net.tftp.client` | TFTP client implementation |
| `mormot.net.tftp.server` | TFTP server implementation |
| `mormot.net.rtsphttp` | RTSP over HTTP streaming |
| `mormot.net.tunnel` | Network tunneling |

---

## 3.6. Layer 3: Database Access (mormot.db.*)

**27 units** for SQL and NoSQL database access:

### Core Database

| Unit | Purpose |
|------|---------|
| `mormot.db.core` | Shared database types |
| `mormot.db.sql` | Abstract SQL classes |
| `mormot.db.proxy` | Remote database proxy |

### Raw Database APIs

| Unit | Database |
|------|----------|
| `mormot.db.raw.sqlite3` | SQLite3 native API |
| `mormot.db.raw.sqlite3.static` | Static SQLite3 library |
| `mormot.db.raw.postgres` | PostgreSQL API |
| `mormot.db.raw.oracle` | Oracle OCI |
| `mormot.db.raw.odbc` | ODBC API |
| `mormot.db.raw.oledb` | OLE DB API |

### SQL Connectors

| Unit | Database |
|------|----------|
| `mormot.db.sql.sqlite3` | SQLite3 |
| `mormot.db.sql.postgres` | PostgreSQL |
| `mormot.db.sql.oracle` | Oracle |
| `mormot.db.sql.odbc` | Any ODBC source |
| `mormot.db.sql.oledb` | Any OLE DB source |
| `mormot.db.sql.zeos` | Via ZDBC (cross-database) |

### NoSQL

| Unit | Database |
|------|----------|
| `mormot.db.nosql.bson` | BSON encoding |
| `mormot.db.nosql.mongodb` | MongoDB client |

### RAD Adapters

| Unit | Purpose |
|------|---------|
| `mormot.db.rad` | Base RAD adapter class |
| `mormot.db.rad.bde` | Borland Database Engine adapter |
| `mormot.db.rad.firedac` | FireDAC integration |
| `mormot.db.rad.nexusdb` | NexusDB adapter |
| `mormot.db.rad.unidac` | UniDAC integration |
| `mormot.db.rad.ui` | Base UI adapter |
| `mormot.db.rad.ui.cds` | ClientDataSet UI adapter |
| `mormot.db.rad.ui.sql` | SQL-aware UI adapter |
| `mormot.db.rad.ui.orm` | ORM-aware TDataSet |

---

## 3.7. Layer 4: ORM / REST / SOA

### ORM Layer (mormot.orm.*)

**9 units** for Object-Relational Mapping:

| Unit | Purpose |
|------|---------|
| `mormot.orm.base` | Low-level ORM types |
| `mormot.orm.core` | TOrm, TOrmModel, IRestOrm |
| `mormot.orm.rest` | REST-based ORM base |
| `mormot.orm.client` | ORM client |
| `mormot.orm.server` | ORM server |
| `mormot.orm.storage` | Storage engine abstraction |
| `mormot.orm.sql` | SQL-based storage |
| `mormot.orm.sqlite3` | SQLite3 ORM |
| `mormot.orm.mongodb` | MongoDB ODM |

### REST Layer (mormot.rest.*)

**8 units** for RESTful services:

| Unit | Purpose |
|------|---------|
| `mormot.rest.core` | TRest base class |
| `mormot.rest.client` | REST client |
| `mormot.rest.server` | REST server |
| `mormot.rest.http.client` | HTTP REST client |
| `mormot.rest.http.server` | HTTP REST server |
| `mormot.rest.mvc` | MVC/MVVM support |
| `mormot.rest.sqlite3` | SQLite3 REST server |
| `mormot.rest.memserver` | In-memory REST server |

### SOA Layer (mormot.soa.*)

**4 units** for Service-Oriented Architecture:

| Unit | Purpose |
|------|---------|
| `mormot.soa.core` | Interface-based SOA types |
| `mormot.soa.client` | Client service stubs |
| `mormot.soa.server` | Server service implementation |
| `mormot.soa.codegen` | Service code generation |

---

## 3.8. Layer 5: Application & Tools

### Application (mormot.app.*)

| Unit | Purpose |
|------|---------|
| `mormot.app.console` | Console application support |
| `mormot.app.daemon` | Daemon/service support |
| `mormot.app.agl` | Application Graphics Layer |

### UI Components (mormot.ui.*)

| Unit | Purpose |
|------|---------|
| `mormot.ui.core` | VCL/LCL compatibility |
| `mormot.ui.controls` | Custom controls |
| `mormot.ui.gdiplus` | GDI+ UI integration |
| `mormot.ui.grid.orm` | ORM-aware grids |
| `mormot.ui.report` | Reporting engine |
| `mormot.ui.pdf` | PDF generation |

### Scripting (mormot.script.*)

| Unit | Purpose |
|------|---------|
| `mormot.script.core` | Core scripting infrastructure |
| `mormot.script.quickjs` | QuickJS JavaScript engine |

### Miscellaneous (mormot.misc.*)

| Unit | Purpose |
|------|---------|
| `mormot.misc.iso` | ISO file format handling |
| `mormot.misc.pecoff` | PE/COFF executable format |

### Tools (mormot.tools.*)

Command-line utilities (not library units):
- `mormot.tools.ecc` - ECC certificate tool
- `mormot.tools.mget` - HTTP downloader utility

### DDD (Domain-Driven Design)

> **Note**: The `src/ddd/` folder contains conceptual documentation only. DDD patterns are implemented through existing ORM/SOA/REST layers (see Chapter 24).

---

## 3.9. Common Usage Patterns

### Minimal HTTP Server with ORM

```pascal
uses
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.orm.sqlite3,
  mormot.rest.sqlite3,
  mormot.rest.http.server;

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
  HttpServer: TRestHttpServer;
begin
  Model := TOrmModel.Create([TOrmPerson]);
  Server := TRestServerDB.Create(Model, 'data.db3');
  Server.Server.CreateMissingTables;

  HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);
  try
    WriteLn('Server running...');
    ReadLn;
  finally
    HttpServer.Free;
    Server.Free;
    Model.Free;
  end;
end.
```

### Interface-Based Service

```pascal
uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.server;

type
  ICalculator = interface(IInvokable)
    ['{...GUID...}']
    function Add(A, B: Integer): Integer;
  end;

  TCalculator = class(TInjectableObjectRest, ICalculator)
  public
    function Add(A, B: Integer): Integer;
  end;

function TCalculator.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Registration
Server.ServiceDefine(TCalculator, [ICalculator], sicShared);
```

### Database-Only (No REST)

```pascal
uses
  mormot.core.base,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.sql.postgres;

var
  Props: TSqlDBPostgresConnectionProperties;
  Conn: TSqlDBConnection;
  Stmt: TSqlDBStatement;
begin
  Props := TSqlDBPostgresConnectionProperties.Create(
    'localhost:5432', 'mydb', 'user', 'pass');
  Conn := Props.ThreadSafeConnection;

  Stmt := Conn.NewStatementPrepared('SELECT * FROM users WHERE id=?', true);
  try
    Stmt.Bind(1, 42);
    Stmt.ExecutePrepared;
    while Stmt.Step do
      WriteLn(Stmt.ColumnUtf8(0));
  finally
    Stmt.Free;
  end;
end;
```

---

## 3.10. Migration from mORMot 1

### Unit Mapping Summary

| mORMot 1 Unit | mORMot 2 Units |
|---------------|----------------|
| `SynCommons.pas` | `mormot.core.*` (24 units) |
| `mORMot.pas` | `mormot.orm.*` + `mormot.rest.*` |
| `SynDB*.pas` | `mormot.db.*` |
| `SynCrypto.pas` | `mormot.crypt.*` |
| `SynCrtSock.pas` | `mormot.net.*` |
| `SynEcc.pas` | `mormot.crypt.ecc*` |

### Type Mapping Summary

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLRecord` | `TOrm` |
| `TSQLModel` | `TOrmModel` |
| `TSQLRest` | `TRest` |
| `TSQLRestServer` | `TRestServer` |
| `TSQLRestClient` | `TRestClient` |
| `TSQLRestServerDB` | `TRestServerDB` |
| `TSQLHttpServer` | `TRestHttpServer` |
| `RawUTF8` | `RawUtf8` |

### Backward Compatibility

By default, mORMot 2 provides type aliases for compatibility:
```pascal
type
  TSQLRecord = TOrm;
  TSQLModel = TOrmModel;
  // etc.
```

Define `PUREMORMOT2` to disable these and use only new names:
```pascal
{$DEFINE PUREMORMOT2}
```

---

*Next Chapter: Core Units (mormot.core.*)*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 2: Architecture Principles](mORMot2-SAD-Chapter-02.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 4: Core Units](mORMot2-SAD-Chapter-04.md) |
