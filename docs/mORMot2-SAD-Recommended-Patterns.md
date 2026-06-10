# Recommended Patterns & Default Toolbox

*A Possible Pattern for Using mORMot 2*

mORMot 2 is a large framework with many ways to do the same thing. That flexibility is a strength, but newcomers consistently ask the same question: **"What is the basic set of types, classes and configuration I should reach for by default?"** The reference chapters describe *what exists*; this guide describes *what to pick* when you have no specific reason to do otherwise.

It is organised in two parts, which are deliberately different in tone:

- **[Part A — The Default Toolbox](#part-a--the-default-toolbox)** is **framework-universal**. These are recommendations that hold for essentially any mORMot 2 project. Treat them as firm defaults: prefer the mORMot type unless you have a concrete reason not to.
- **[Part B — An Opinionated Application Blueprint](#part-b--an-opinionated-application-blueprint)** is **one proven way to structure a service**, not a framework rule. It is a coherent, batteries-included starting point. Adopt it wholesale for a new service, or cherry-pick what is applicable to your project.

> This is a *hub* document. Each topic links to the chapter that covers it in depth. If you read nothing else, read the two cheat-sheet tables in [A.1](#a1-the-one-screen-cheat-sheet) and [A.2](#a2-recommended-type-at-each-layer).

---

## Part A — The Default Toolbox

The single most useful thing to internalise: **mORMot ships its own data types, and on any code path the framework will serialize, you should use them instead of the RTL counterparts.** They reuse storage, leverage the cached RTTI (`TRttiCustom`/`PRttiCustom`), use pointer-based UTF-8 paths, and serialize to JSON/binary out of the box. The RTL types are general-purpose, allocate more, and have no shared serialization story. See [Chapter 4: Core Units](mORMot2-SAD-Chapter-04.md) for the units these live in.

### A.1. The one-screen cheat sheet

When you reach for a classic RTL construct, this is the default mORMot replacement:

| You want to…                                   | Use (default)                                   | Instead of (RTL)                          | Unit |
|------------------------------------------------|-------------------------------------------------|-------------------------------------------|------|
| Hold text on a server code path                | `RawUtf8`                                        | `string` (UTF-16)                         | `mormot.core.base` |
| Hold binary data                               | `RawByteString` / `TSQLRawBlob`                  | `TBytes`                                   | `mormot.core.base` |
| List-like ops over `array of T` (Add/Sort/Find)| `TDynArray` (wraps your array)                   | `TList` / `TList<T>`                       | `mormot.core.data` |
| O(1) lookup over an existing array             | `TDynArrayHashed` / `TDynArrayHasher`            | `TDictionary<K,V>`                         | `mormot.core.data` |
| Typed generic list/dict with serialization     | `IList<T>` / `IDict<K,V>` (`Collections.New…`)   | `Generics.Collections.TList<T>`           | `mormot.core.collections` |
| Schema-less JSON object/array                  | `TDocVariant` / `TDocVariantData`                | `Variant` / `System.JSON.TJSONObject`     | `mormot.core.variants` |
| Schema-less doc with a clean, typed API        | `IDocList` / `IDocDict`                          | raw `Variant` late binding                | `mormot.core.variants` |
| Build text/JSON/CSV/SQL efficiently            | `TTextWriter` / `TJsonWriter`                    | `TStringBuilder` / `Format` / concat      | `mormot.core.text` / `…json` |
| String list (server side)                      | `TRawUtf8List`                                   | `TStringList`                             | `mormot.core.data` |
| Thread-safe key→value dictionary               | `TSynDictionary`                                 | `TThreadList` + `TDictionary`             | `mormot.core.json` |
| Dedupe many identical strings                  | `TRawUtf8Interning`                              | manual hash table                         | `mormot.core.data` |
| Iterate lines of a file zero-copy              | `TMemoryMapText`                                 | `TStringList.LoadFromFile`                | `mormot.core.text` |
| Talk to the database                           | `IRestOrm` (interface) + `TOrm` descendants      | concrete `TRestOrmServer`                  | `mormot.orm.core` |

> **Why not just use the RTL?** Full reasoning is in [Chapter 4](mORMot2-SAD-Chapter-04.md). The short version: the mORMot types serialize automatically across the SOA/REST boundary, carry an interface-managed lifetime (no `try..finally Free`), keep UTF-8 end-to-end, and are backed by `TDynArray`/`TDynArrayHasher` for cache-friendly speed.

### A.2. Recommended type at each layer

The framework marshals values as they cross layers. Use the mORMot type on every signature it will touch; RTL types are fine **only inside an implementation body** where nothing crosses a boundary.

| Layer | Recommended type | Why |
|---|---|---|
| HTTP boundary | `RawUtf8` | Zero-copy on the socket buffer; UTF-8 end-to-end, no conversion |
| API input — schema-less | `IDocDict` / `variant` | Framework recognises it and streams JSON directly |
| API input — typed | `packed record` DTO + `Rtti.RegisterFromText` | Compile-time field checking; cheapest carrier (value type) |
| Service interface signature | simple types, records-with-RTTI, `TOrm` descendants, `array of …`, `variant`, `IDocList`/`IDocDict`, `IList<T>`/`IDict<K,V>` | **Only these serialize automatically** across the SOA boundary |
| Business logic — typed collections | `IList<T>` / `IDict<K,V>` | Interface lifetime, built-in JSON/binary, cache-friendly |
| Business logic — schema-less | `IDocList` / `IDocDict` | Same `TDocVariantData` core, strongly-typed surface |
| Hot-path raw storage | `TDynArray` over `array of T` | No class wrapper; external count → amortised O(1) `Add` |
| Hot-path hashed lookup | `TDynArrayHashed` | In-place hashing over the same storage; no parallel container |
| DB access | `IRestOrm` + `TOrm` descendants | Swappable backend, mockable, RTTI-driven SQL |
| JSON output | `TTextWriter` / `TJsonWriter` | O(1) allocations, RTTI-driven; never use concat or `Format` |
| String list (server) | `TRawUtf8List` | UTF-8 native, thread-safe (`fThreadSafe`), O(1) `IndexOf` (`fNoDuplicate`) |
| String list (UI binding) | `TStringList` | Keep where VCL/LCL `TStrings` forces it; convert at the boundary |

**Practical rule for service interfaces:** parameters and results of an `IInvokable` service should be one of *simple types, records-with-RTTI, `TOrm` descendants, `array of …`, `variant`/`TDocVariantData`, `IDocList`/`IDocDict`, or `IList<T>`/`IDict<K,V>`.* Reach for RTL generics only inside the implementation, never on the signature. See [Chapter 16: Services via Interfaces](mORMot2-SAD-Chapter-16.md).

### A.3. Replacing `TStringList`

`TStringList` is the most-overused RTL class in ported code. The default replacement is **`TRawUtf8List`** — thread-safe, UTF-8 native, O(1) `IndexOf` with `fNoDuplicate`:

```pascal
list := TRawUtf8List.CreateEx([fCaseSensitive, fNoDuplicate, fThreadSafe]);
try
  list.Add('foo');
  if list.IndexOf('foo') >= 0 then ... // O(1) with fNoDuplicate
  list.Values['key'] := 'value';       // name=value pair
finally
  list.Free;
end;
```

Pick by *what you actually used `TStringList` for*:

| Used `TStringList` for…             | Better mORMot type |
|-------------------------------------|--------------------|
| Plain UTF-8 string list             | `TRawUtf8List` |
| Typed string list w/ serialization  | `IList<RawUtf8>` |
| Name=value config                   | `TRawUtf8List` (Name/Value) or `TDocVariantData` for nested |
| Thread-safe key→value dictionary    | `TSynDictionary` |
| Typed key→value w/ generics         | `IDict<K,V>` |
| Schema-less JSON-like dict          | `IDocDict` / `TDocVariantData` |
| Parsing CSV/lines into tokens       | `CsvToRawUtf8DynArray`, `TSynTempBuffer` |
| Building delimited text             | `TTextWriter.AddCsvUtf8` / `AddString` |

**Keep `TStringList` only** where VCL/LCL UI controls (`TListBox.Items`, `TMemo.Lines`) require a `TStrings`, or in working code that isn't on a hot path. Convert to `RawUtf8` at the boundary.

### A.4. Service instance mode — default to `sicShared`

When you register an interface-based service, you choose an instance lifetime. The default:

```
Is the service stateless / read-mostly (validators, calculators, cache-fronted readers)?
        │
        ├── Yes ──► sicShared   (one instance, all threads — your code MUST be thread-safe)
        │
        └── No ──► Does it hold EXPENSIVE thread-affine state
                   (dedicated DB connection, prepared-statement cache,
                    crypto context, non-thread-safe library)?
                        │
                        ├── Yes ──► sicPerThread
                        │
                        └── No ──► Is the state genuinely conversational
                                   (a multi-call workflow)?
                                        │
                                        ├── Yes ──► sicPerSession / sicClientDriven
                                        │
                                        └── No ──► sicShared + a keyed cache for per-user data
```

**Thread-safety rules that hold in every mode:** a `TOrm` instance is never shared between threads (create one per call); a `TRestBatch` is never shared (build it as a local inside the method); `IRestOrm` reads are thread-safe; writes serialize at the storage layer. For shared mutable state in a `sicShared` service, guard it with `TSynLocker`, `TRWLock`, or `TSynDictionary`. See [Chapter 16](mORMot2-SAD-Chapter-16.md).

### A.5. Bind to `IRestOrm`, and the `_Safe()` rule

Two habits that prevent a large class of bugs:

- **Store `IRestOrm` (the interface), not `TRestOrmServer` (the class).** This is what makes the storage backend swappable, mockable in tests, and DI-friendly. Cast up to `IRestOrmServer` *only* at the composition root, when you need server-only methods like `CreateMissingTables`.
- **Always go through `_Safe(v)` before reinterpret-casting a `variant` to `TDocVariantData`.** `_Safe` walks `varByRef` indirection and returns a sentinel empty record if the value isn't a `TDocVariant`. A direct `TVarData` cast on a `null` or RTL variant corrupts memory.

```pascal
with _Safe(v)^ do        // safe even if v is null / not a TDocVariant
  email := U['email'];
```

### A.6. Minimum recommended server configuration

A sane default server wires a handful of deliberate decisions: a model, a storage server bound as `IRestOrm`, your services, authentication, an HTTP server, and logging. **Every one of these is a named decision — none should be left implicit.**

#### Two independent axes: logical layering vs physical topology

Before choosing a server layout, separate two decisions that are easy to conflate:

- **Logical** — how your services are *layered*: repository, CQRS read/write split, event-driven handlers, DDD use-cases. This is a code-organisation concern (it is what Part B is about), and it is **independent of where anything runs**.
- **Physical (topology)** — how many `TRestServer` / executable instances exist, and which of them are reachable from the network.

These axes are orthogonal because mORMot gives you **location transparency**: an `IInvokable` service is consumed identically whether its implementation lives in-process or on another machine. The call site never changes:

```pascal
var
  svc: IUserQuery;          // the service interface; same type, local or remote
  profile: TUserProfile;
begin
  // Resolve once. Local: from the server's own container.
  // Remote: from a client pointed at the other instance.
  if Rest.Services.Resolve(IUserQuery, svc) then
    // From here the call site is identical, wherever svc actually runs:
    profile := svc.GetProfile(userID);
```

Switching a service from local to remote is a matter of *instance initialization* — register it on a different server, point a client at a URI — **not a code change**. The layering you design in Part B therefore survives any topology you later deploy it on.

**Make the switch a localized decision with the resolver pattern.** Both `TRestServer.Services` and `TRestClientURI.Services` are **`TInterfaceResolver`** descendants (`mormot.core.interfaces`), so the local-vs-remote choice collapses to *which resolver instance the composition root hands out*. Consumer code depends on one global `TInterfaceResolver` (or, better, is itself a `TInjectableObject` whose published interface properties are auto-resolved — the same mechanism [B.5](#b5-soa-composition-root-and-di) recommends inside services) and never learns where the implementation runs:

```pascal
// AppUserClient.pas — the only unit consumer code depends on
var
  UserResolver: TInterfaceResolver;   // set once at startup by Local OR Remote unit
```

The two wiring units differ only in what they assign to that variable — an in-process server's container locally, an HTTP client's container remotely:

<details>
<summary><b>AppUserClientLocal.pas</b> — builds the whole layered stack in-process (the A.6.1 monolith)</summary>

```pascal
unit AppUserClientLocal;

implementation

var
  Rest: TRestServerDB;

initialization
  Rest := TRestServerDB.Create(TOrmModel.Create([TOrmUser]), ':memory:'); // or 'data.db'
  Rest.Model.Owner := Rest;
  Rest.Server.CreateMissingTables;
  // domain/persistence layer owns the ORM; app layer gets it injected (B.5 DI):
  Rest.ServiceDefine(
    TUserService.CreateInjected([], [], [TUserPersistence.Create(Rest.Orm)]),
    [IUserQuery, IUserCommand]);
  // hand out the server's own container — Resolve() returns direct references:
  UserResolver := Rest.Services;
finalization
  FreeAndNil(Rest);
end.
```

</details>

<details>
<summary><b>AppUserClientRemote.pas</b> — an HTTP client to the standalone server (the A.6.2 distributed layout)</summary>

```pascal
unit AppUserClientRemote;

implementation

var
  Http: TRestHttpClientSocket;

initialization
  // host/port/root come from your settings file — deployment config, not code
  Http := TRestHttpClientSocket.Create(
    Settings.Server, Settings.Port, TOrmModel.Create([], Settings.Root));
  Http.Model.Owner := Http;
  // declare the SAME interfaces, this time as client-side factories:
  Http.ServiceDefine([IUserQuery, IUserCommand], sicShared);
  // hand out the client's container — Resolve() returns HTTP proxies:
  UserResolver := Http.Services;
finalization
  FreeAndNil(Http);
end.
```

</details>

```pascal
// Project.dpr — the ONE place topology is chosen:
uses
  AppUserClient,
  AppUserClientLocal;      // <-- swap to AppUserClientRemote for distributed

// consumer code, identical and unaware of the line above:
if UserResolver.Resolve(IUserQuery, svc) then
  svc.GetProfile(userID, profile);
```

`Local` is the A.6.1 monolith (one exe, full stack in-process); `Remote` is the A.6.2 distributed layout (HTTP client to a standalone server). The business code is identical; only the linked unit differs. And inside a service implementation you usually don't even touch `UserResolver` directly — a `TInjectableObject` descendant publishes `property Users: IUserQuery` and the resolver chain fills it ([B.5](#b5-soa-composition-root-and-di)).

Both units self-wire in their `initialization`, so linking one is genuinely all you do; the only extra need of the remote unit is the host/port/root in your settings file — deployment configuration, not code. (Wiring in `initialization` does mean the settings must be loadable at unit-initialization time; if you want explicit startup control, move those few lines into your daemon's startup instead.)

<details>
<summary><b>IMPORTANT</b> — select the topology with a define (`ifdef`)</summary>

Centralize the choice in an include file: e.g. `project.inc` may contain `{$DEFINE LOCALMORMOT}`:

```pascal
// AppUserClient.pas — selects the implementation at compile time
{$I project.inc}   // defines LOCALMORMOT for the local build, nothing for remote

uses
  {$ifdef LOCALMORMOT}  
  AppUserClientLocal,  
  {$else}
  AppUserClientRemote, 
  {$endif}
  ...;
```

Now the single switch lives in `project.inc` — place `{$define LOCALMORMOT}` in `project.inc` and the `AppUserClientLocal` is used.

</details>

**A single-executable monolith can still be a fully layered internal stack of services.** You do not split processes to get clean layering — you get that logically, inside one exe. You split processes only when scale, isolation, or a public network boundary actually demands it. So pick the physical topology by deployment context, not by how layered you want the code to be:

- **A.6.1** — a single server, the right default for a **trusted / local** deployment (one exe, still logically layered).
- **A.6.2** — a private system-of-record plus a public edge, for an **Internet-facing or distributed** deployment.

**A cross-cutting rule that applies to both.** Topology reduces *exposure*; it never substitutes for *authorization*. Scope every read to the caller: a logged-in end user must retrieve only their own rows, never the whole table. This is a service-design concern (filter each query to the session's identity at the service boundary) — no choice of server count fixes it. See [B.6](#b6-cross-cutting-standards).

#### A.6.1. Trusted / local deployment

For a service that is only reachable on a **trusted network** (local box, corporate LAN, in-cluster), a **single server** is correct and simplest. The ORM REST routes it exposes (`/root/TUser`, …) are not an unguarded second door: they sit behind the *same* session authentication as your services, and behind `TOrmAccessRights`, which lets you make a table read-only, forbid CRUD per-table, or expose none at all.

```pascal
// 1. Logging: turn it on early (see Chapter 25)
TSynLog.Family.Level := LOG_VERBOSE;
TSynLog.Family.PerThreadLog := ptIdentifiedInOnFile;

// 2. Model: list every TOrm class the server persists
Model := TOrmModel.Create([TUser, TInvoice]);

// 3. Storage: embedded SQLite is the default system of record
RestServer := TRestServerDB.Create(Model, 'data.db');
RestServer.Server.CreateMissingTables;       // server-only call, at composition root

// 4. Services: register interface-based services (default sicShared)
RestServer.ServiceDefine(TUserService, [IUserQuery, IUserCommand], sicShared);

// 5. Authentication: enable it explicitly (JWT / mORMot auth)
RestServer.AuthenticationRegister(TRestServerAuthenticationDefault);

// 6. Access rights: tighten or disable the ORM REST routes you do not want
//    exposed (per-table CRUD bits, per TAuthGroup); services stay reachable.
//    See Chapter 21, 21.9.1.
Rights := SUPERVISOR_ACCESS_RIGHTS;
Rights.Edit(Model, TInvoice, {C=}false, {R=}true, {U=}false, {D=}false); // read-only
Group.OrmAccessRights := Rights;     // applied to the TAuthGroup row

// 7. HTTP: expose over HTTP/HTTPS
HttpServer := TRestHttpServer.Create('8080', [RestServer], '+', HTTP_DEFAULT_MODE);
```

Adjust per project: swap `TRestServerDB` for an external SQL/NoSQL backend behind the same `IRestOrm` abstraction (see [A.5](#a5-bind-to-irestorm-and-the-_safe-rule) and [B.1](#b1-storage-behind-an-interface)); choose your authentication scheme in [Chapter 21](mORMot2-SAD-Chapter-21.md); tune logging in [Chapter 25](mORMot2-SAD-Chapter-25.md).

#### A.6.2. Internet-facing or distributed deployment

When the edge is **public on the Internet**, or when you are **orchestrating several service instances** across a server network, split the topology to minimise the public surface; the gain is reduced exposure and a clean place to terminate TLS/JWT.

- A **private system-of-record** (`TRestServerDB`, SQLite or external SQL) that owns the business tables and the auth records, and is **never handed to an HTTP server** — so it has no network surface and is reachable only in-process. It still publishes services (repository / CQRS, authentication, the main data model); "private" means *not network-exposed*, not *data-only*.
- A **public edge** (`TRestServerFullMemory`, void model) that terminates TLS, validates JWT, and exposes only the **context-scoped** services each application actually needs. The edge consumes the private services either in-process (same instance) or over the network (distributed).

The edge coding is an *addition* to A.6.1, not a different world:

```pascal
// === PRIVATE: the system of record. Owns the data. No network surface. ===
// Same wiring as A.6.1 steps 1-6, but the resulting server is NOT passed to
// any TRestHttpServer: it stays reachable only in-process.
DbModel  := TOrmModel.Create([TUser, TInvoice, TAuthUser, TAuthGroup]);
DbServer := TRestServerDB.Create(DbModel, 'data.db');
DbServer.Server.CreateMissingTables;
// (register the private services on DbServer here: repository/CQRS, auth, ...)

// === PUBLIC: the edge. Owns no data. Only context-scoped services + JWT. ===
// Void model => no TOrm classes => no ORM REST routes can exist at all.
EdgeModel  := TOrmModel.Create([]);
EdgeServer := TRestServerFullMemory.Create(EdgeModel);

// Inject the private DB ORM explicitly at the composition root: the service
// runs on EdgeServer, whose inherited Server.Orm is the void edge ORM, so bind
// the private ORM here rather than relying on Server.Orm (the explicit form of
// B.5's DI). Publish only the services this application context needs.
EdgeServer.ServiceDefine(
  TUserService.Create(DbServer.Orm),          // inject the private DB ORM
  [IUserQuery, IUserCommand]);                // registered as sicShared

// Only the edge is handed to HTTP — the private server stays off the network.
HttpServer := TRestHttpServer.Create('443', [EdgeServer], '+', HTTP_DEFAULT_MODE);
```

Publishing too much is the classic Internet-facing mistake; the cross-cutting rule from A.6 — scope every read to the caller — is what actually prevents data leaks here. See [B.6](#b6-cross-cutting-standards).

#### A.6.3. Mapping to standard architecture vocabulary

The above wording has been picked on purpose — here is how it maps to the established literature, along the same two axes.

**Physical / topology axis** (A.6.2 — the network boundary):

| Term used here | Established name | Reference |
|---|---|---|
| Public edge | **API Gateway**; **Backend-for-Frontend (BFF)** when one edge serves one client app | Richardson, *Microservices Patterns* [\[8\]](#ref-8); Newman, *Building Microservices* [\[6\]](#ref-6) |
| Private system-of-record | **System of Record (SoR)**; the core/domain side of a **Hexagonal (Ports & Adapters)** boundary | Cockburn, *Hexagonal Architecture* [\[1\]](#ref-1) |
| Private/public split | an inbound **adapter** (the edge) in front of the core — Ports & Adapters applied at the deployment boundary | Hexagonal [\[1\]](#ref-1) / Onion architecture [\[7\]](#ref-7) |

**Logical / layering axis** (Part B — in-process, independent of topology):

| Term used here | Established name | Reference |
|---|---|---|
| Application layer / context-scoped services | **use cases / application services** | Evans, *DDD* [\[2\]](#ref-2); Martin, *Clean Architecture* [\[5\]](#ref-5) |
| Repository interface ([B.1](#b1-storage-behind-an-interface)) | **Repository pattern** | Evans, *DDD* [\[2\]](#ref-2); Fowler, *PoEAA* [\[3\]](#ref-3) |
| Query/Command split ([B.4](#b5-cqrs-read-write-split)) | **CQRS** | Young [\[9\]](#ref-9); Fowler [\[4\]](#ref-4) |
| Domain object carried on the `TOrm` ([B.3](#b3-two-types-per-entity-not-three)) | **entity / aggregate** | Evans, *DDD* [\[2\]](#ref-2) |

Two caveats, because it is tempting to file all of this under one label such as "Clean Architecture":

- **Clean Architecture [\[5\]](#ref-5) / Onion [\[7\]](#ref-7) / DDD [\[2\]](#ref-2) describe the *logical* axis** — in-process layering — not how many processes you run. They apply just as much to the A.6.1 monolith as to A.6.2.
- **API Gateway / BFF [\[8\]](#ref-8) / hexagonal-edge [\[1\]](#ref-1) describe the *physical* axis** — the network boundary. They are what A.6.2 adds on top of the layering.

---

## Part B — A Possible Application Blueprint — Deviate Where Required

> **This section is not about a framework rule. It is about a possible blueprint** Use it as a possible starting point. It is a coherent default for a new service in a non-trivial codebase. You can adopt it and deviate where your domain differs. It consolidates the SOA/DDD guidance from [Chapter 15](mORMot2-SAD-Chapter-15.md), [Chapter 16](mORMot2-SAD-Chapter-16.md), and [Chapter 24](mORMot2-SAD-Chapter-24.md).

### B.1. Storage behind an interface

Every service exposes an `IXxxRepository` (or a CQRS pair, see [B.4](#b5-cqrs-read-write-split)) in the domain layer. Multi-tenancy: per-tenant SQLite when data is tenant-scoped; a single shared database otherwise.

### B.2. ORM base class and casing

- Use **`TOrm`** everywhere; migrate legacy `TSQLRecord` holdouts opportunistically.
- Use the modern casing **`RawUtf8`** (not `RawUTF8`) — aliases, but lowercase is the current convention.
- Behavioural cost of both: zero. It is search-and-replace.

### B.3. Two types per entity (not three)

- A **`TOrm` class that is also the domain object** — `Equals`, validation, and computed properties live on it directly. No separate shadow class.

**Carrier choice — stay with `packed record` when possible:** cheapest carrier (value type, no heap, no constructor), serializes via cached RTTI after one `Rtti.RegisterFromText` call. `string` is forbidden on the wire — use `RawUtf8` for text and `RawByteString`/`TSQLRawBlob` for binary. Reach for `TSynPersistent`/`TSynAutoCreateFields` when the wire object needs methods or owned nested children. For lists across a SOA boundary, return `array of TXxxListItemDTO` (lighter) or `IList<TXxxListItemDTO>` (interface lifetime) — never raw `TList<T>` or `TJSONObject`.

### B.4. CQRS read/write split

**Two interfaces per entity by default:** `IXxxCommand` (writes) and `IXxxQuery` (reads), each descending from `IInvokable`.

**Why:** per-direction authorization (read-only clients can't reach write methods); the read side can be served by a replica or cache without touching writes; the interfaces become self-documenting. **Acceptable to skip** for genuinely trivial wrappers (one or two endpoints, no read/write asymmetry) — but skip it as an exception, not as the rule.

### B.5. SOA composition root and DI

- **`IRestOrm` is the binding type** in business code; cast to `IRestOrmServer` only at the composition root for server-only methods.
- **Inject via `TInjectableObjectRest` / `TInjectableObject`:** publish interface properties and let `AutoResolve` fill them through the `TRestServer.Services` resolver chain, or use the inherited `Server.Orm`. Concrete-class fields cannot be auto-injected — interfaces only.
- **Instance mode:** default `sicShared`, promote only when justified — see [A.4](#a4-service-instance-mode--default-to-sicshared).

### B.6. Cross-cutting standards

- **Folder layout.** Keep the persistence boundary visible: lowercase `dom/`, `infra/`, `app/`, `tests/`, one subfolder per entity. Don't merge domain and infrastructure into one folder.
- **Result handling.** A shared base enum (e.g. `TServiceResult`: success / invalid-request / not-found / denied / db-error) plus optional per-service extensions — stops the proliferation of near-identical result enums.
- **Auth on the consumer side.** Extract a single guard helper so every service does one `Guard.Require(Token, …)` call instead of a repeated multi-step token ritual.
- **Scope every read to the caller.** The most common and most damaging public-service mistake is publishing too much: letting a logged-in user retrieve *all* rows when they should see only their own. Filter every query to the session's identity at the service boundary. No server-topology choice (see [A.6](#a6-minimum-recommended-server-configuration)) substitutes for this.
- **Tests.** A `tests/` folder per service; mapper procedures are the cheapest, highest-value test target — exercise them first.

### B.7. Default starting point for a new service

The following list is a possible starting point. It is not a pattern to always follow:

1. `TOrm` per entity (in `infra/`), doubling as the domain object.
2. A family of (possibily `packed record`) DTOs (in `app/<Entity>/`), registered with `Rtti.RegisterFromText` in the unit's `initialization`.
3. Possibly pairing `IXxxCommand` + `IXxxQuery` interfaces descending from `IInvokable`.
4. Service implementations as `TInjectableObjectRest` subclasses, `IRestOrm` injected via published property.
5. `sicShared` instance mode unless profiling says otherwise.
6. Standard daemon: load settings, build the `TOrmModel`, create the persistence server, wire repositories, register services, start `TRestHttpServer`.
7. A single auth guard consuming tokens from the shared auth service.
8. `RawUtf8` everywhere data crosses a boundary; `string` only where VCL/LCL forces it.
9. A `tests/` folder, starting with mapper round-trip tests.

---

## When to deviate

Part A is a default you should rarely override on serialized code paths — if you find yourself using `TStringList` or `TDictionary<K,V>` on a service signature, that is almost always a mistake. Part B is a blueprint. You may collapse the DTO family to one DTO, skip CQRS, or front a legacy schema directly. That is fine.

---

## References

The works cited in [A.6.3](#a63-mapping-to-standard-architecture-vocabulary) and throughout the text:

1. <a id="ref-1"></a>Cockburn, Alistair. *Hexagonal Architecture (Ports & Adapters)*. 2005. <https://alistair.cockburn.us/hexagonal-architecture/>
2. <a id="ref-2"></a>Evans, Eric. *Domain-Driven Design: Tackling Complexity in the Heart of Software*. Addison-Wesley, 2003. <https://fabiofumarola.github.io/nosql/readingMaterial/Evans03.pdf>
3. <a id="ref-3"></a>Fowler, Martin. *Patterns of Enterprise Application Architecture* (PoEAA). Addison-Wesley, 2002. <https://sar.ac.id/stmik_ebook/prog_file_file/EFCofwzsj0.pdf>
4. <a id="ref-4"></a>Fowler, Martin. *CQRS*. 2011. <https://martinfowler.com/bliki/CQRS.html>
5. <a id="ref-5"></a>Martin, Robert C. *Clean Architecture: A Craftsman's Guide to Software Structure and Design*. Prentice Hall, 2017. <https://dl.acm.org/doi/10.5555/3175742>
6. <a id="ref-6"></a>Newman, Sam. *Building Microservices*, 2nd edition. O'Reilly, 2021.
7. <a id="ref-7"></a>Palermo, Jeffrey. *The Onion Architecture*. 2008. <https://jeffreypalermo.com/2008/07/the-onion-architecture-part-1/>
8. <a id="ref-8"></a>Richardson, Chris. *Microservices Patterns*. Manning, 2018. Pattern catalogue at <https://microservices.io/patterns/>
9. <a id="ref-9"></a>Young, Greg. *CQRS Documents*. 2010. <https://cqrs.files.wordpress.com/2010/11/cqrs_documents.pdf>

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 4: Core Units](mORMot2-SAD-Chapter-04.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 5: Object-Relational Mapping](mORMot2-SAD-Chapter-05.md) |
