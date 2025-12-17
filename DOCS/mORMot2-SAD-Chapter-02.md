# 2. Architecture Principles

*Adopt a mORMot*

This framework implements established "best-practice" patterns:

- **Model-View-Controller** (MVC)
- **Multi-tier architecture**
- **Test-Driven Design**
- **Stateless CRUD/REST**
- **Object-Relational Mapping** (ORM)
- **Object-Document Mapping** (ODM)
- **Service-Oriented Architecture** (SOA)

These patterns enable implementing projects up to complex *Domain-Driven Design* architectures.

---

## 2.1. General Design

The mORMot 2 architecture follows a layered design:

```
┌────────────────────────────────────────────────────────────────────┐
│                      mORMot 2 Architecture                         │
├────────────────────────────────────────────────────────────────────┤
│                                                                    │
│  ┌───────────────┐                       ┌───────────────┐         │
│  │  Web Clients  │                       │ REST Clients  │         │
│  │ (AJAX/Mobile) │                       │(Delphi/FPC/…) │         │
│  └───────┬───────┘                       └───────┬───────┘         │
│          │                                       │                 │
│          └───────────────┬───────────────────────┘                 │
│                          │ RESTful JSON                            │
│                          ▼                                         │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                      TRestServer                             │  │
│  │  ┌────────────┐  ┌───────────┐  ┌───────────────────────┐    │  │
│  │  │ Auth       │  │ MVC/MVVM  │  │ Services              │    │  │
│  │  │ (Sessions) │  │ WebServer │  │ (Interface-based SOA) │    │  │
│  │  └────────────┘  └───────────┘  └───────────────────────┘    │  │
│  │                         │                                    │  │
│  │                ┌────────┴────────┐                           │  │
│  │                │    IRestOrm     │                           │  │
│  │                │    ORM Layer    │                           │  │
│  │                └────────┬────────┘                           │  │
│  └─────────────────────────┼────────────────────────────────────┘  │
│                            │                                       │
│  ┌─────────────────────────┴────────────────────────────────────┐  │
│  │                    Storage Backends                          │  │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌────────────────┐   │  │
│  │  │ SQLite3 │  │External │  │ MongoDB │  │ In-Memory/File │   │  │
│  │  │ (native)│  │  SQL    │  │ (NoSQL) │  │  (JSON/Binary) │   │  │
│  │  └─────────┘  └─────────┘  └─────────┘  └────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                    │
│  Cross-Cutting Features:                                           │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Compression │ Security │ Crypto │ Logging │ Testing │ JSON   │  │
│  └──────────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────────┘
```

Key concepts of mORMot 2:

- **Cross-Platform**: Multiple clients and devices supported
- **Integration-friendly**: Can integrate with existing codebases
- **Client-Server RESTful**: JSON over HTTP/HTTPS/WebSockets
- **Layered (multi-tier)**: Clear separation of concerns
- **Service-Oriented**: Business logic via SOA interfaces
- **Shared Model**: Business rules and data model shared by clients and server
- **ORM/ODM**: Data mapped to objects for SQL and NoSQL
- **Flexible Storage**: SQLite3, external SQL, MongoDB, in-memory, or remote mORMot servers
- **Integrated Security**: Authentication and authorization at all layers
- **MVC/MVVM**: Build web applications from ORM/SOA methods
- **Pattern-based**: REST, JSON, MVC, SOLID principles
- **Testable**: Integrated testing and debugging API
- **Optimized**: Built for scaling and stability

---

## 2.2. Architecture Design Process

Architecture should be driven by actual application needs, not by theoretical patterns. There is no "one architecture fits all" solution. Architecture is about *how* you build your software.

```
┌─────────────────────────────────────────────────────────────────┐
│            Iterative Architecture Process                       │
│                                                                 │
│  Customer ──► BackLog ──► Design ──► Dev ──► Software           │
│     │            │          │         │         │               │
│     │            │          │         │         │               │
│  Use Cases  Requirements  Architecture Tasks   Definition       │
│                              │                   of Done        │
│                              │                                  │
│                         Risk Assessment                         │
│                         Technology & Models                     │
└─────────────────────────────────────────────────────────────────┘
```

### Avoiding Weak Design

Common pitfalls to avoid:

- Letting each developer decide implementation without review
- Letting teams work in isolation without system-wide collaboration
- Architecture at such a high level it doesn't affect coding
- Architecture so detailed that code becomes over-engineered
- Blindly following technology trends without evaluation

### Recommended Practices

- **Collaboration**: No one is alone, no team is better, no manager is always right
- **Sharing**: Between individuals, teams, and managers
- **Customer focus**: Stay content and customer focused
- **Long-term thinking**: Today's implementation prepares tomorrow
- **Pragmatism**: Make tomorrow's work easier
- **Courage**: "They did not know it was impossible, so they did it"

Frameworks like mORMot provide integrated, working sets of classes so you can focus on your product while enjoying collaboration with the Open Source community.

---

## 2.3. Model-View-Controller (MVC)

The *Model-View-Controller* (MVC) pattern isolates domain logic from user interface, permitting independent development, testing, and maintenance.

```
┌───────────────────────────────────────────────────────────┐
│                    MVC Pattern                            │
│                                                           │
│  ┌──────────┐    Uses     ┌──────────┐                    │
│  │Controller│ ──────────► │  Model   │                    │
│  └──────────┘             └──────────┘                    │
│       │                         │                         │
│       │ Command                 │ Notify Updates          │
│       ▼                         ▼                         │
│  ┌──────────┐    Refresh  ┌──────────┐                    │
│  │   View   │ ◄────────── │  Model   │                    │
│  └──────────┘             └──────────┘                    │
└───────────────────────────────────────────────────────────┘
```

**Model**: Manages behavior and data of the application domain. Responds to requests for information about its state and instructions to change state. In mORMot, implemented via `TOrmModel` class which centralizes all `TOrm`-inherited classes.

**View**: Renders the model into a form suitable for interaction:
- **Desktop clients**: Auto-generated UI using RTTI
- **Web clients**: Mustache templates with Delphi controllers
- **AJAX clients**: RESTful JSON services

**Controller**: Receives user input and initiates responses by making calls on model objects. In mORMot, already implemented within RESTful commands. Custom actions implemented via `TOrm` classes or RESTful Services.

---

## 2.4. Multi-Tier Architecture

Multi-tier architecture separates presentation, application processing, and data management into logically separate processes.

### Two-Tier (Traditional RAD)

```
┌─────────────────────────────────────────────────────────┐
│  ┌─────────────────────┐    ┌─────────────────────┐     │
│  │  Application Tier   │    │    Data Tier        │     │
│  │  (UI + Logic mixed) │───►│    (Database)       │     │
│  └─────────────────────┘    └─────────────────────┘     │
└─────────────────────────────────────────────────────────┘
```

### Three-Tier (mORMot ORM/SOA)

```
┌─────────────────────────────────────────────────────────────────┐
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ Presentation    │  │   Logic Tier    │  │   Data Tier     │  │
│  │     Tier        │─►│  (ORM + SOA)    │─►│  (Database)     │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Four-Tier (Domain-Driven Design)

```
┌───────────────────────────────────────────────────────────────────────┐
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────┐   │
│  │ Presentation │  │ Application  │  │Business Logic│  │  Data    │   │
│  │    Tier      │─►│    Tier      │─►│    Tier      │─►│  Tier    │   │
│  │(Delphi/AJAX) │  │(JSON Server) │  │  (Domain)    │  │(Storage) │   │
│  └──────────────┘  └──────────────┘  └──────────────┘  └──────────┘   │
└───────────────────────────────────────────────────────────────────────┘
```

In mORMot 2:

- **Data Tier**: SQLite3, external databases (PostgreSQL, MySQL, Oracle, MS SQL, etc.), MongoDB, or in-memory storage
- **Logic Tier**: ORM and SOA implementation - Delphi classes mapped to database via ORM, business logic as interfaces
- **Presentation Tier**: Delphi clients, AJAX applications, or MVC web apps

---

## 2.5. Service-Oriented Architecture (SOA)

SOA is a design approach where functionality is packaged as inter-operable services that can be used across multiple systems and business domains.

```
┌──────────────────────────────────────────────────────────────────┐
│                    SOA Architecture                              │
│                                                                  │
│  Consumers                Service Bus               Publishers   │
│  ─────────                ───────────               ──────────   │
│  ┌─────────┐             ┌───────────┐            ┌───────────┐  │
│  │Client A │◄───────────►│           │◄──────────►│Publisher 1│  │
│  └─────────┘             │           │            └───────────┘  │
│  ┌─────────┐             │  Service  │            ┌───────────┐  │
│  │Client B │◄───────────►│    Bus    │◄──────────►│Publisher 2│  │
│  └─────────┘             │           │            └───────────┘  │
│  ┌─────────┐             │           │            ┌───────────┐  │
│  │Client C │◄───────────►│           │◄──────────►│Publisher 3│  │
│  └─────────┘             └───────────┘            └───────────┘  │
└──────────────────────────────────────────────────────────────────┘
```

### Key SOA Characteristics

A software **service** is a logical representation of a repeatable activity that produces a precise result. Services are:

- **Loosely coupled**: No embedded calls to other services
- **Stateless**: Each invocation is independent
- **Single responsibility**: Each service implements one action
- **Protocol-based**: Defined protocols describe how services communicate

### SOA Benefits: Decoupling

| Dependency | Desired Decoupling | Technique |
|------------|-------------------|-----------|
| Platform | Hardware/Framework/OS should not constrain choices | Standard protocols (REST/JSON) |
| Location | Consumers unaffected by hosting changes | Routing and proxies |
| Availability | Maintenance transparent to clients | Server-side support |
| Versions | New services without client upgrades | Contract marshalling |

### ORM + SOA Coexistence

ORM and SOA complement each other:

- **ORM**: Efficient data access with native objects (CQRS pattern)
- **SOA**: High-level business logic with custom parameters, reducing bandwidth

In mORMot 2, interface-based SOA allows the same code to run on both client and server with better server performance and full interoperability.

---

## 2.6. Object-Relational Mapping (ORM)

ORM provides methods to persist high-level objects into a relational database.

```
┌───────────────────────────────────────────────────────────────┐
│                      ORM Process                              │
│                                                               │
│  ┌──────────────┐    ┌─────────────┐    ┌──────────────────┐  │
│  │    Object    │───►│     ORM     │───►│     RDBMS        │  │
│  │   Instance   │    │   (CRUD)    │    │   (Database)     │  │
│  └──────────────┘    └─────────────┘    └──────────────────┘  │
│                            │                                  │
│                      SQL Mapping                              │
└───────────────────────────────────────────────────────────────┘
```

### ORM Mapping Sources

```
┌────────────────────────────────────────────────────────────────┐
│  ┌─────────────────┐         ┌─────────────────┐               │
│  │   Class Type    │         │   Data Model    │               │
│  │   (via RTTI)    │────────►│   (Database)    │               │
│  └─────────────────┘         └─────────────────┘               │
│           │                           │                        │
│           └───────────┬───────────────┘                        │
│                       │                                        │
│                  ┌────┴────┐                                   │
│                  │   ORM   │                                   │
│                  └─────────┘                                   │
└────────────────────────────────────────────────────────────────┘
```

### Comparison of Approaches

| Scheme | Pros | Cons |
|--------|------|------|
| **RAD DB Components** | SQL is powerful; RAD approach | Business logic limited; SQL binds to engine; Poor multi-tier |
| **Manual SQL Mapping** | Elaborated business logic | SQL must be hand-coded; Duplication; Engine-specific |
| **Database ORM** | SQL generated by ORM; Engine-agnostic | More abstraction needed; May retrieve excess data |
| **Client-Server ORM** | All ORM benefits; Services for precise data; Full multi-tier | More abstraction needed |

mORMot implements a **Client-Server ORM** that can scale from stand-alone mode to complex Domain-Driven Design.

---

## 2.7. NoSQL and Object-Document Mapping (ODM)

### SQL vs NoSQL

**SQL (Relational)**:
- Schema-based
- Relational model with JOINs
- ACID transactions
- Time-proven and efficient

**NoSQL**:
- "Not Only SQL"
- Designed for web scale and BigData
- Easy replication and simple APIs
- No standard (diverse implementations)

### NoSQL Families

1. **Graph-oriented**: Store data by relations (e.g., Neo4j)
2. **Aggregate-oriented**:
   - Document-based (MongoDB, CouchDB)
   - Key/Value (Redis, Riak)
   - Column family (Cassandra, HBase)

### Document Model Example

SQL stores data per table (requires JOINs):
```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│     Users       │  │    Contacts     │  │     Access      │
├─────────────────┤  ├─────────────────┤  ├─────────────────┤
│ ID | UserName   │  │ UserID | Phone  │  │ UserID | Level  │
└─────────────────┘  └─────────────────┘  └─────────────────┘
```

NoSQL stores as documents (embedded):
```json
{
  "ID": 1234,
  "UserName": "John Smith",
  "Contact": {
    "Phone": "123-456-789",
    "Email": "xyz@abc.com"
  },
  "Access": {
    "Level": 5,
    "Group": "dev"
  }
}
```

### SQL vs NoSQL Trade-offs

| SQL | NoSQL |
|-----|-------|
| Ubiquitous SQL language | Maps OOP and complex types natively |
| Easy vertical scaling | Horizontal scaling (sharding) |
| Data normalization | Schema-less evolution |
| Data consistency (single source) | Version management |
| Complex ACID transactions | Graph/Document native storage |
| Aggregation functions | Map/Reduce support |

With mORMot, you can switch from SQL to MongoDB with one line of code change, even at runtime.

---

## 2.8. Domain-Driven Design (DDD)

### Definition

From domaindrivendesign.org:

*"The premise of domain-driven design is two-fold:*
- *For most software projects, the primary focus should be on the domain and domain logic;*
- *Complex domain designs should be based on a model."*

### DDD in mORMot

mORMot enables DDD through:

- **Entities**: `TOrm` descendants representing persistent domain objects
- **Value Objects**: Records and managed types for immutable data
- **Aggregates**: Object graphs with clear boundaries
- **Repositories**: `IRestOrm` interface for data access
- **Services**: Interface-based SOA for domain operations
- **Domain Events**: Async notifications via WebSockets

### Implementation Layers

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        DDD with mORMot 2                                │
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │ Domain Layer (Pure Pascal)                                        │  │
│  │ · TOrm entities with business logic                               │  │
│  │ · Value Objects as records                                        │  │
│  │ · Aggregates with clear boundaries                                │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                  │                                      │
│  ┌───────────────────────────────┴───────────────────────────────────┐  │
│  │ Application Layer (Services)                                      │  │
│  │ · IInvokable interfaces (mormot.soa.*)                            │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                  │                                      │
│  ┌───────────────────────────────┴───────────────────────────────────┐  │
│  │ Infrastructure Layer                                              │  │
│  │ · IRestOrm repositories (mormot.orm.*)                            │  │
│  │ · TSqlDBConnection (mormot.db.*)                                  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                  │                                      │
│  ┌───────────────────────────────┴───────────────────────────────────┐  │
│  │ Presentation Layer                                                │  │
│  │ · TRestHttpServer (mormot.rest.*)                                 │  │
│  │ · MVC views (mormot.core.mvc)                                     │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 2.9. SOLID Principles in mORMot 2

mORMot 2 embraces SOLID principles:

### Single Responsibility
- Each unit focuses on one concern
- `mormot.core.json` handles JSON, `mormot.core.rtti` handles RTTI

### Open/Closed
- Classes open for extension via inheritance
- Core behavior closed to modification

### Liskov Substitution
- `TOrm` descendants can substitute the base class
- Storage backends interchangeable

### Interface Segregation
- `IRestOrm` vs `IRestOrmClient` vs `IRestOrmServer`
- Clients only depend on what they need

### Dependency Inversion
- Code against interfaces (`IRestOrm`), not implementations
- DI support via `TInjectableObjectRest`

### Composition Over Inheritance (mORMot 2 Key Change)

**mORMot 1** used inheritance:
```pascal
TSQLRestServer = class(TSQLRest)
  // ORM methods directly in class
  function Add(...): TID;
```

**mORMot 2** uses composition:
```pascal
TRest = class
  property Orm: IRestOrm;     // ORM via interface
  property Services: TServiceContainer;  // SOA via container
end;

// Access ORM through .Orm property
Server.Orm.Add(Customer);
```

This change improves:
- **Testability**: Mock individual components
- **Flexibility**: Swap implementations
- **Clarity**: Clear boundaries between concerns

---

*Next Chapter: Meet mORMot 2 - New Units and Structure*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 1: mORMot 2 Overview](mORMot2-SAD-Chapter-01.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 3: Meet mORMot 2](mORMot2-SAD-Chapter-03.md) |
