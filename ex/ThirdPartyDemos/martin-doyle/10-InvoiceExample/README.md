# Rechnung

Invoice management application built with the mORMot2 framework.

## Technology Stack

- **Language**: Free Pascal (FPC) / Delphi 7
- **IDE**: Lazarus (LCL) or Delphi 7
- **Framework**: mORMot2 (Synopse)
- **Database**: Local SQLite (no network)
- **Platforms**: Windows, macOS, Linux

## Project Structure

```
10-InvoiceExample/
  src/                     - Application source
    *.pas                  - Pascal units (prefix: rg = Rechnung)
    *.dfm                  - Form definitions (Lazarus + Delphi compatible)
    Rechnung.lpi / .lpr    - Lazarus project
    RechnungDelphi.dpr     - Delphi 7 project
  Components/              - Reusable components (prefix: md = MartinDoyle)
    mdlayout.pas           - Platform-independent layout engine
    mdforms.pas            - Base form classes
    mdlayout_usage.md      - Layout framework guide
```

## Data Architecture

```
  UI (Forms)
  Services (ICustomerService, IInvoiceService, ...)
  DTOs (TDtoCustomer, TDtoOrder, ...)
  ORM (TOrmCustomer, TOrmOrder, ...)
  REST Client (TRgRestClient)
  SQLite Database (local file)
```

Key source files:

| File | Purpose |
|------|---------|
| `rgClient.pas` | REST client, service interfaces and implementations |
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

### Service Layer Isolation

No form accesses the ORM directly.  Every database operation goes through a
dedicated **interface / implementation** pair in `rgClient.pas`
(e.g. `ICustomerEditService` → `TCustomerEditService`).  Each service receives
`IRestOrm` via constructor injection, keeping UI, business logic, and
persistence cleanly separated.

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
lazbuild Rechnung.lpi
```

### Delphi 7

Open `RechnungDelphi.dpr` in the Delphi IDE and compile.

## Configuration

Machine-specific paths (lazbuild, mORMot2 source) are stored in `.claude.local` (not committed).
Copy `.claude.local.example` to `.claude.local` and adjust for your system.

## Documentation

| Document | Purpose |
|----------|---------|
| `Components/mdlayout_usage.md` | mdLayout framework guide |
| `CLAUDE.md` | Coding rules for Claude Code |

Note: Documentation is in the `docs/` directory (at `martin-doyle/docs/` level).
