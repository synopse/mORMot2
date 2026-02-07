# Rechnung

Invoice management application built with the mORMot2 framework.

## Technology Stack

- **Language**: Free Pascal (FPC) / Delphi 7
- **IDE**: Lazarus (LCL) or Delphi 7
- **Framework**: mORMot2 (Synopse)
- **Database**: SQLite (embedded or via HTTP daemon)
- **Platforms**: Windows, macOS, Linux

## Project Structure

```
10-InvoiceExample/
  src/                     - GUI application source
    *.pas                  - Pascal units (prefix: rg = Rechnung)
    *.dfm                  - Form definitions (Lazarus + Delphi compatible)
    Rechnung.lpi / .lpr    - Lazarus GUI project
    RechnungDelphi.dpr     - Delphi 7 GUI project
  daemon/                  - Server daemon
    RechnungDaemon.dpr     - TSynDaemon entry point (Delphi 7 + FPC)
    RechnungDaemon.lpi     - Lazarus daemon project
  Components/              - Reusable components (prefix: md = MartinDoyle)
    mdlayout.pas           - Platform-independent layout engine
    mdforms.pas            - Base form classes
    mdlayout_usage.md      - Layout framework guide
  docs/                    - Project documentation
```

## Architecture

The application supports two operating modes, configured via `rechnung.config` (JSON):

- **Local mode** (`"Mode": "local"`): GUI embeds `TRgServer` + SQLite directly
- **Service mode** (`"Mode": "service"`): GUI connects via HTTP to `RechnungDaemon`

```
Local mode:
  UI (Forms) → SOA Interfaces → TRgServer → ORM → SQLite

Service mode:
  UI (Forms) → SOA Interfaces → HTTP/JSON → RechnungDaemon → TRgServer → ORM → SQLite
```

Five stateless SOA interfaces (`sicShared`, thread-safe):

| Interface | Purpose |
|-----------|---------|
| `IRgCustomerService` | CRUD + list customers |
| `IRgInvoiceService` | CRUD + list invoices |
| `IRgPaymentService` | Add payment, get open amount |
| `IRgStatisticsService` | Dashboard stats, customer summary |
| `IRgReportService` | Open items, payments, revenue, monthly reports |

Key source files:

| File | Purpose |
|------|---------|
| `rgServiceInterfaces.pas` | 5 SOA interface definitions + `RegisterInterfaces` |
| `rgServiceImplementation.pas` | 5 `TInjectableObjectRest` server implementations |
| `rgServer.pas` | `TRgServer` (`TRestServerDB` + `ServiceDefine`) |
| `rgClient.pas` | `TRgServiceClient` (resolves interfaces in local/service mode) |
| `rgConfig.pas` | `TRgConfig` (`TSynJsonFileSettings` for JSON config) |
| `rgData.pas` | ORM model definitions |
| `rgDtoTypes.pas` | Data Transfer Objects for UI layer |
| `rgConst.pas` | Application constants, resourcestrings, paths |

## Design Decisions

### Denormalized JSON Collections

Contacts, addresses, and invoice line items are not stored in separate
relational tables.  Instead they live as **embedded JSON** inside a single ORM
column using standard `TCollection` / `TCollectionItem` hierarchies that
mORMot2 serializes automatically.  A `TOrmCustomer` carries its entire
`TPersonCollection` (with nested phones, emails, and addresses) in one field;
a `TOrmCustomerOrder` stores all line items in a `TItemCollection` field.
This keeps the schema to just three tables and avoids JOINs for nested data.

### Font-Based Responsive Layout

The layout engine (`Components/mdlayout.pas`) positions controls relative to
each other using **multiples of the font height** — not absolute pixels or DPI
percentages.  A single `BaseHeight` (taken from a reference label at runtime)
drives all margins, spacing, and proportional widths.  Because `BaseHeight`
changes with the platform's default font and DPI settings, dialogs adapt
automatically on Windows, Linux, and macOS without design-time anchors.

### Cross-Platform Custom Grid

Lazarus' `TListView` crashes under the macOS Cocoa widgetset.  The project
replaces it with `TMDListGrid` (`Components/mdGrids.pas`), a `TDrawGrid`-based
component that provides a ListView-compatible API (`Columns`, `Items`,
`SubItems`, `OnSelectItem`).  It uses `ThemeServices` for native header drawing
on each platform and follows the mORMot2 `TOrmTableToGrid` pattern for safe
destruction (event handlers set to `nil` before freeing).

### SOA Service Layer

No form accesses the ORM directly.  Every database operation goes through one
of 5 **SOA interfaces** defined in `rgServiceInterfaces.pas` and implemented
in `rgServiceImplementation.pas`.  Server implementations inherit from
`TInjectableObjectRest` and access the ORM via `Self.Server.Orm`.  Forms use
the global `RgServices` client which resolves interfaces either locally
(embedded server) or remotely (HTTP client) based on the JSON config.

### Data Transfer Objects

ORM entities store contacts as deeply nested JSON collections.  Rather than
exposing these hierarchies to forms, the service layer converts them to **flat
DTO records** (`rgDtoTypes.pas`) — e.g. `TDtoCustomer` with plain `string`
fields for Phone, City, etc.  DTOs also carry computed values like
`OpenAmount` and invoice `Status` that don't exist in the ORM entity.  This
decouples the UI from the storage structure and confines all `RawUtf8` ↔
`string` encoding to the service layer.

## Sample Database

A ready-to-use SQLite database (`Project10.db`) is available in the
[Data/](https://github.com/martin-doyle/mORMot2-Examples/tree/main/10-InvoiceExample/Data)
folder of the companion repository. It was migrated from the classic **Borland
DBDEMOS** database (the sample `.mdb` that shipped with Delphi) and contains
customers, orders, items, parts, vendors, and employees for a fictional
dive-equipment business. The migration collapsed flat relational tables into
mORMot2 ORM entities with embedded JSON for nested data.

## Build

### Free Pascal / Lazarus

```bash
# GUI client
lazbuild src/Rechnung.lpi

# Server daemon
lazbuild daemon/RechnungDaemon.lpi
```

### Delphi 7

- GUI: Open `src/RechnungDelphi.dpr` in the Delphi IDE and compile.
- Daemon: Open `daemon/RechnungDaemon.dpr` in the Delphi IDE and compile.

## Running

### Local Mode (default)

Run the GUI directly. It embeds the server and SQLite database.

### Service Mode

```bash
# 1. Start the daemon (port 11111)
./RechnungDaemon --console

# 2. Configure the GUI client
#    Set "Mode": "service" in rechnung.config

# 3. Run the GUI client
./Rechnung
```

The daemon supports Windows service installation (`/install`, `/start`, `/stop`)
and Linux daemonization (`--fork`).

## Configuration

Machine-specific paths (lazbuild, mORMot2 source) are stored in `.claude.local` (not committed).
Copy `.claude.local.example` to `.claude.local` and adjust for your system.

## Documentation

| Document | Purpose |
|----------|---------|
| `docs/SOA-IMPLEMENTATION-PLAN.md` | SOA migration plan and status |
| `docs/UI-DESIGN.md` | UI specification |
| `Components/mdlayout_usage.md` | mdLayout framework guide |
| `CLAUDE.md` | Coding rules for Claude Code |
