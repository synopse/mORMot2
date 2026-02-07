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
| `docs/UI-DESIGN.md` | Complete UI specification |
| `docs/IMPLEMENTATION-PLAN.md` | Phased implementation roadmap |
| `Components/mdlayout_usage.md` | mdLayout framework guide |
| `CLAUDE.md` | Coding rules for Claude Code |

Note: Documentation is in the `docs/` directory (at `martin-doyle/docs/` level).
