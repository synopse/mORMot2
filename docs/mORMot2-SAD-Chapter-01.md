# 1. mORMot2 Overview

*Meet the mORMot*

**Synopse mORMot 2** is an Open Source Client-Server ORM/SOA/MVC framework for *Delphi* and *Free Pascal*, targeting Windows, Linux, BSD, and macOS for the server, and virtually any platform for clients (including mobile and AJAX).

This is **mORMot 2**, a complete rewrite of the original mORMot framework with improved architecture, cleaner code organization, and modern Object Pascal patterns.

## Main Features

The main features of *mORMot 2* are:

- **ORM/ODM**: Object persistence on almost any database (SQL or NoSQL)
- **SOA**: Organize your business logic into RESTful JSON services
- **Clients**: Consume your data or services from any platform, via ORM classes or SOA interfaces
- **Web MVC**: Publish your ORM/SOA processes as responsive Web Applications

All features work with local or remote access, via an auto-configuring Client-Server REST design.

```
┌────────────────────────────────────────────────────────────────────────┐
│                         mORMot2 Architecture                           │
├────────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  SQL Databases              NoSQL Databases        Services            │
│  ─────────────              ────────────────       ────────            │
│  · SQLite3                  · MongoDB              · Method-based      │
│  · PostgreSQL               · In-Memory            · Interface-based   │
│  · MySQL/MariaDB            · Files                · Asynchronous      │
│  · MS SQL Server                                   · Remote (SaaS)     │
│  · Oracle                        ↓                                     │
│  · Firebird                     ODM                      ↓             │
│  · DB2                           ↓                      SOA            │
│  · Informix                      │                       │             │
│        ↓                         │                       │             │
│       ORM ───────────────────────┴───────────────────────┘             │
│        │                                                               │
│        └─────────────────────────┬────────────────────────             │
│                                  ↓                                     │
│                            REST Server                                 │
│                        ┌────────┴────────┐                             │
│                        │    MVC/MVVM     │                             │
│                        │   Web Server    │                             │
│                        └────────┬────────┘                             │
│                                 ↓                                      │
│  ┌──────────────────────────────┴───────────────────────────────────┐  │
│  │                       REST Clients                               │  │
│  │   · Delphi Desktop/Mobile  · AJAX  · Any HTTP Client             │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                        │
│  Cross-Cutting Features:                                               │
│  · User Management & Security   · Sessions & Replication               │
│  · Unit Testing & Mocks/Stubs   · Logging & Profiling                  │
│  · http.sys & WebSockets        · Templates (Mustache)                 │
│  · JSON & Cryptography          · PDF & Reporting                      │
└────────────────────────────────────────────────────────────────────────┘
```

*mORMot 2* offers all features needed for building any kind of modern software project, with state-of-the-art integrated software components, designed for both completeness and complementarity, offering *convention over configuration* solutions, and implemented for speed and efficiency.

## Quick Start Examples

**For storing data**, you define a class, and the framework handles everything: routing, JSON marshalling, table creation, SQL generation, validation.

```pascal
type
  TOrmCustomer = class(TOrm)
  published
    property Name: RawUtf8 read fName write fName;
    property Email: RawUtf8 read fEmail write fEmail;
    property Balance: Currency read fBalance write fBalance;
  end;

// Create model and server
Model := TOrmModel.Create([TOrmCustomer]);
Server := TRestServerDB.Create(Model, 'customers.db3');
Server.Server.CreateMissingTables;

// Add a customer
Customer := TOrmCustomer.Create;
Customer.Name := 'John Doe';
Customer.Email := 'john@example.com';
Server.Orm.Add(Customer, true);
```

**For creating a service**, you define an interface and a class:

```pascal
type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
  end;

  TCalculator = class(TInjectableObjectRest, ICalculator)
  public
    function Add(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
  end;

// Register on server
Server.ServiceDefine(TCalculator, [ICalculator], sicShared);

// Call from client
var Calc: ICalculator;
if Client.Services.Resolve(ICalculator, Calc) then
  Result := Calc.Add(10, 20);
```

**For building a MVC web site**, write a Controller class in Delphi, then HTML Views using *Mustache* templates, leveraging the same ORM/SOA methods as Model.

## What's New in mORMot 2

mORMot 2 is a complete rewrite of the original mORMot framework. Key improvements include:

### Cleaner Architecture
- **Renamed types**: `TSQLRecord` → `TOrm`, `TSQLRest` → `TRest`, `TSQLModel` → `TOrmModel`
- **Split units**: Large `SynCommons.pas` split into ~24 focused `mormot.core.*` units
- **SOLID principles**: Composition over inheritance, interface-based design

### Unit Organization
```
mormot.core.*   - Foundation (text, JSON, RTTI, logging, threading)
mormot.crypt.*  - Cryptography (AES, SHA, ECC, RSA, JWT, X.509)
mormot.net.*    - Networking (HTTP, WebSockets, async I/O)
mormot.db.*     - Database access (SQL and NoSQL)
mormot.orm.*    - Object-Relational Mapping
mormot.soa.*    - Service-Oriented Architecture
mormot.rest.*   - RESTful client/server
mormot.app.*    - Application utilities (console, daemon)
mormot.ui.*     - VCL/LCL components
```

### New Features in mORMot 2
- **Async HTTP Server** (`useHttpAsync`) - Event-driven for massive concurrency
- **Factory-based Cryptography** - `Hash()`, `Sign()`, `Cipher()`, `Asym()`, `Cert()` factories
- **ACME/Let's Encrypt** - Automatic TLS certificate management
- **Native X.509/RSA** - Pure Pascal implementation without OpenSSL dependency
- **OpenAPI Generator** - Generate Delphi clients from Swagger specs
- **QuickJS Engine** - Modern JavaScript scripting (replaces SpiderMonkey)

### Compiler Support
- **Delphi**: 7 through 12.2 (RAD Studio 12 Athens)
- **Free Pascal**: 3.2.2+ with fixes
- **Platforms**: Windows (32/64-bit), Linux (x64/ARM64), macOS, FreeBSD

---

## 1.1. Client-Server ORM/SOA Framework

The *mORMot 2 framework* implements a Client-Server RESTful architecture, following MVC, N-Tier, ORM, and SOA best-practice patterns.

Multiple clients can access the same remote or local server using diverse communication protocols:

```
┌───────────────────────────────────────────────────────────────────┐
│                     Network Architecture                          │
├───────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌─────────┐  ┌─────────┐                      ┌─────────┐        │
│  │Client 1 │  │Client 2 │    Internet/VPN      │Client n │        │
│  │ Delphi  │  │  AJAX   │        ║             │ Delphi  │        │
│  └────┬────┘  └────┬────┘        ║             └────┬────┘        │
│       │            │             ║                  │             │
│       └────────────┴─────────────╨──────────────────┘             │
│                          │                                        │
│                   JSON + REST                                     │
│                   over HTTP/HTTPS                                 │
│                          │                                        │
│                   ┌──────┴──────┐                                 │
│                   │   Server    │                                 │
│                   │ (mORMot 2)  │                                 │
│                   └─────────────┘                                 │
└───────────────────────────────────────────────────────────────────┘
```

Or the application can be stand-alone:

```
┌─────────────────────────────────────────┐
│       Stand-Alone Application           │
│  ┌──────────┐    ┌──────────┐           │
│  │  Client  │───►│  Server  │           │
│  │  Code    │    │  Code    │           │
│  └──────────┘    └──────────┘           │
│        direct in-process access         │
└─────────────────────────────────────────┘
```

Switching between embedded and client-server architecture is just a matter of how *mORMot* classes are initialized. The same executable can run as a stand-alone application, a server, or a client, depending on runtime parameters!

---

## 1.2. Highlights

Key distinguishing features of mORMot 2:

- **Client-Server orientation** with optimized request caching and intelligent updates over a RESTful architecture - but can also be used in stand-alone applications
- **No RAD components** - True ORM and SOA approach
- **Multi-Tier architecture** with integrated business rules as fast ORM-based classes and *Domain-Driven* design
- **Service-Oriented-Architecture** model using custom RESTful JSON services - send any `TObject`, dynamic array, or record as JSON via interface-based contracts shared on both client and server
- **Truly RESTful authentication** with dual security model (session + per-query)
- **Very fast JSON** producer and parser with caching at SQL level
- **Fast HTTP/HTTPS server** using `http.sys` kernel-mode server (Windows) or async event-driven server (all platforms) - plus named pipes, WebSockets, or in-process alternatives
- **SQLite3 kernel** with ability to connect to any external database (PostgreSQL, MySQL, Oracle, MS SQL, Firebird, etc.) via direct client libraries, ODBC, or OLE DB
- **RESTful ORM access to NoSQL** like MongoDB with the same code base
- **Multiple databases at once** via SQLite3 Virtual Tables mechanism
- **Full Text Search** engine with Google-like ranking algorithm
- **Native cryptography** including AES, SHA, ECC, RSA, JWT, X.509 with optional OpenSSL acceleration
- **Direct User Interface generation** with grids and Ribbon layouts
- **Integrated Reporting** system serving PDF reports
- **Optimized for performance** (assembly when needed, buffered I/O, multi-threaded architecture)
- **Cross-platform clients** from Delphi, Free Pascal, mobile, and AJAX
- **Full source code** provided under open source license

---

## 1.3. Benefits

*mORMot* provides a comprehensive set of features to manage your crosscutting concerns through a reusable set of components and core functionality.

Benefits include:

- **KISS convention over configuration**: All needed features at hand, with one clear way of doing things
- **Pascal-oriented**: Implementation leverages Object Pascal's type system rather than mimicking Java/C# patterns
- **Integrated**: All crosscutting scenarios are coupled with consistent APIs, extensive code reuse, and JSON/RESTful orientation from the ground up
- **Tested**: Most of the framework is test-driven, with comprehensive regression tests including system-wide integration tests
- **Don't reinvent the wheel**: Focus on your business logic, not infrastructure
- **Open Source and maintained**: Active development community - mORMot won't leave you soon!

---

## 1.4. Legacy Code and Existing Projects

Even if *mORMot* works best in projects designed from scratch, it fits very well for evolving existing *Delphi* projects or creating server-side components for AJAX applications.

One key benefit is facilitating the transition from traditional Client-Server architecture to N-Tier layered patterns.

Due to its modular design, you can integrate framework components into existing applications:

- **Add logging** to track issues and enable customer-side performance profiling
- **Use low-level classes** like record or dynamic array wrappers, or dynamic document storage via `TDocVariant`
- **Use direct DB layers** including high-performance database access, array binding for fast inserts, or NoSQL databases
- **Reports** can use the `mormot.ui.report` code-based system for server-side PDF generation
- **HTTP requests** can be exposed using method-based services, e.g., for rendering HTML with Mustache templates
- **Migrate to SOA** by moving logic into server services defined via interfaces, without SOAP/WCF overhead
- **RESTful interface** for JSON consumption via AJAX or mobile clients
- **New tables via ORM** in your existing SQL server or new storage like MongoDB
- **In-memory engine** or SQLite3-based consolidation for performance-critical scenarios
- **Support for older Delphi versions** (starting from Delphi 7) for projects that can't easily upgrade

---

## 1.5. FAQ

Before going further, here are answers to frequently asked questions.

### Should I use mORMot 1 since mORMot 2 is the maintained version?

**mORMot 2** is the way to go for any new project. mORMot 1 is in bug-fix-only mode. For existing mORMot 1 projects, we continue to fix bugs and supply SQLite3 updates, but no new features will appear. Consider migrating to mORMot 2 when you have time - the process is straightforward once you change to the new units.

### The documentation is too long to read quickly.

You don't need to read everything - most is detailed API reference. But do read the first part covering main concepts and patterns (15-30 minutes). Also see the slides and examples available at https://github.com/synopse/mORMot2

### Where should I start?

1. Read the Architecture Principles (Chapter 2)
2. Download and install the sources
3. Compile and run the test programs in `/test`
4. Learn about ORM, SOA, and MVC concepts
5. Try the sample projects in `/ex` folder

### I'm not a fan of ORM - I prefer writing SQL.

ORM makes development easier, but you can use interface-based services with "manual" SQL via the `mormot.db.*` classes for high performance and direct JSON export.

### mORMot requires inheriting from TOrm. Can I use any object?

We discuss this in detail in the ORM chapter. Adding attributes to existing classes pollutes your code. The framework provides CQRS services to persist any PODO (Plain Old Delphi Object) without requiring TOrm inheritance.

### Why don't you use generics or class attributes?

Our framework uses Object Pascal's type system effectively - specifying a class or interface type as parameter is safe and efficient. Generics tend to bloat executables, reduce performance, and hide implementation details. Attributes pollute code and introduce coupling. These features also reduce compatibility with older Delphi and FPC.

### What are RawUtf8 and other special types?

The framework uses UTF-8 internally. `RawUtf8` is optimized for UTF-8 strings across all Delphi versions. Search the keyword index for `RawUtf8` or see the Core Units chapter.

### My client receives non-standard JSON with unquoted fields.

Internally, the framework uses MongoDB extended JSON syntax (unquoted fields) for better performance. Add a proper `User-Agent` HTTP header to receive standard `"field":value` JSON.

### Why is this framework named mORMot?

- Because marmots hibernate, like our precious objects
- Because marmots are highly social and communicate with whistles, like our connected applications
- Because "Manage Object Relational Mapping Over Territory" works as an acronym
- Because we like mountains and those large ground rodents!

---

## 1.6. Getting Started

### Installation

Clone from GitHub:
```bash
git clone https://github.com/synopse/mORMot2.git
```

### Minimal Project Setup

```pascal
program MyFirstMormot;

{$APPTYPE CONSOLE}

uses
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.orm.sqlite3,
  mormot.rest.sqlite3,
  mormot.rest.http.server;

type
  TOrmSample = class(TOrm)
  private
    fName: RawUtf8;
    fValue: Integer;
  published
    property Name: RawUtf8 read fName write fName;
    property Value: Integer read fValue write fValue;
  end;

var
  Model: TOrmModel;
  Server: TRestServerDB;
  HttpServer: TRestHttpServer;
begin
  // Create ORM model with our class
  Model := TOrmModel.Create([TOrmSample]);

  // Create REST server with SQLite3 storage
  Server := TRestServerDB.Create(Model, 'sample.db3');
  Server.Server.CreateMissingTables;

  // Wrap in HTTP server
  HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);
  try
    WriteLn('Server running on http://localhost:8080');
    WriteLn('Press Enter to quit...');
    ReadLn;
  finally
    HttpServer.Free;
    Server.Free;
    Model.Free;
  end;
end.
```

### Required Units by Feature

**Core functionality:**
```pascal
uses
  mormot.core.base,      // Foundation types
  mormot.core.os,        // OS abstraction
  mormot.core.text,      // Text processing
  mormot.core.json;      // JSON handling
```

**ORM/Database:**
```pascal
uses
  mormot.orm.core,       // TOrm, TOrmModel
  mormot.orm.sqlite3,    // SQLite3 ORM
  mormot.db.sql.sqlite3; // SQLite3 engine (if direct SQL needed)
```

**REST Server/Client:**
```pascal
uses
  mormot.rest.core,        // TRest base
  mormot.rest.server,      // TRestServer
  mormot.rest.client,      // TRestClient
  mormot.rest.http.server, // TRestHttpServer
  mormot.rest.http.client; // TRestHttpClient
```

**Services (SOA):**
```pascal
uses
  mormot.soa.core,    // Service interfaces
  mormot.soa.server,  // Server-side services
  mormot.soa.client;  // Client-side service consumption
```

---

*Next Chapter: Architecture Principles*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Foreword](mORMot2-SAD-Foreword.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 2: Architecture Principles](mORMot2-SAD-Chapter-02.md) |
