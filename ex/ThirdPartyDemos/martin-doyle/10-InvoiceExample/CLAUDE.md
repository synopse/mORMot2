# Rechnung - Rules for Claude Code

## Language

All code, comments, variables, and identifiers: **English only**.

## Compatibility

- Delphi 7 compatible: no generics, no anonymous methods, no inline variables
- Cross-platform: Windows, macOS, Linux (LCL + VCL)

## Code Conventions

- Unit prefixes: `rg` (Rechnung), `md` (MartinDoyle)
- Forms: `T<Name>Form` (e.g., `TCustomerEditForm`)
- Services: `I<Name>Service` / `T<Name>Service`
- Constants/resourcestrings in `rgConst.pas`
  - `const`: PascalCase
  - `resourcestring`: prefix `m` (e.g., `mErrorNoDatabase`)

## File Header

Only update when making functional changes. Copy from existing file (e.g., `rgMain.pas`).

Update: copyright year `2017-2026`, date `DD.MM.YYYY`, author `Martin Doyle`, email `martin-doyle@online.de`.

```pascal
{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
 Project : Rechnung
 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info
  Module : <unit_name>.pas
  Last modified
    Date : <DD.MM.YYYY>
    Author : Martin Doyle
    Email : martin-doyle@online.de
    Permission is hereby granted ... (MIT License)
    ----------------------------------------------------------------------------
}
```

## UI Localization

Use system locale for display/parse. Never hardcode formats.

| Data Type | Display | Parse |
|-----------|---------|-------|
| Date | `DateToStr()` | `TryStrToDate()` |
| Currency | `Format('%.2n', [Amount])` | `TryStrToCurr()` |
| Float | `FloatToStr()` | `TryStrToFloat()` |

- UI layer: system locale (`FormatSettings`)
- Database/ORM/DTO: fixed formats (mORMot2 internal)
- Error messages: show expected format via `FormatSettings.ShortDateFormat`

## Form Architecture

### Child Windows (main content)

- Inherit from `TMDChildForm`
- Use `Align` properties (`alClient`, `alBottom`, `alTop`)
- Override `GetFormMenu` for form-specific menus
- Do NOT use `mdLayout`

### Modal Dialogs

- Inherit from `TMDDBModeForm` for data entry
- Use `BorderStyle = bsDialog`
- Use `mdLayout` for responsive layout
- Layout code in `FormShow` or `FormCreate`
- Call `AdjustForPlatform` early, `AutoSizeForm` at end
- Always free `TLayoutHelper` in try/finally

See `Components/mdlayout_usage.md` for details.

## UI Design

See `docs/UI-DESIGN.md` for specification. Key rules:
- Navigation via menu only (no sidebar buttons)
- Customer list left, invoices right
- Payments integrated in invoice list
- Global statistics in top panel
- Modal dialogs for editing
- Reports as modal dialogs with filter + print

## Architecture: Client-Server SOA

Two operating modes configured via `rechnung.config` (JSON):

- **Local mode** (`"Mode": "local"`): GUI embeds `TRgServer` with SQLite directly
- **Service mode** (`"Mode": "service"`): GUI connects via HTTP to `RechnungDaemon`

```
[GUI Client] --HTTP/JSON--> [RechnungDaemon + SQLite + Services]
OR (local mode): [GUI + embedded TRgServer + SQLite + Services]
```

### Projects

| Project | Path | Purpose |
|---------|------|---------|
| `Rechnung.lpi` | `src/` | GUI client (LCL) |
| `RechnungDaemon.lpi` | `src/` | Console/service daemon (no GUI) |

### SOA Interfaces (5 services)

Defined in `rgServiceInterfaces.pas`, implemented in `rgServiceImplementation.pas`:

| Interface | Purpose |
|-----------|---------|
| `IRgCustomerService` | CRUD + list customers |
| `IRgInvoiceService` | CRUD + list invoices |
| `IRgPaymentService` | Add payment, get open amount |
| `IRgStatisticsService` | Dashboard stats, customer summary |
| `IRgReportService` | Open items, payments, revenue, monthly reports |

All services are `sicShared` (stateless, thread-safe). Server implementations
inherit from `TInjectableObjectRest` and access ORM via `Self.Server.Orm`.

### Service Client

Global `RgServices: TRgServiceClient` in `rgClient.pas` provides access to all 5 interfaces.
Initialized in `rgClient.pas` `initialization` section based on config mode.

```pascal
// Usage in forms:
RgServices.CustomerService.ListCustomers(Customers);
RgServices.InvoiceService.GetInvoice(ID, Detail);
RgServices.StatisticsService.GetDashboardStats(Stats);
```

### Key Units

| Unit | Purpose |
|------|---------|
| `rgDtoTypes.pas` | All DTO records, enums, dynamic arrays |
| `rgServiceInterfaces.pas` | 5 SOA interfaces + `RegisterInterfaces` |
| `rgServiceImplementation.pas` | 5 `TInjectableObjectRest` implementations |
| `rgServer.pas` | `TRgServer` (`TRestServerDB` + `ServiceDefine`) |
| `rgClient.pas` | `TRgServiceClient` + legacy service classes |
| `rgConst.pas` | Constants, config, version init, logging setup |
| `rgData.pas` | ORM model (`TOrmCustomer`, `TOrmCustomerOrder`) |

### Configuration

Operating mode and connection settings are constants in `rgConst.pas`:

```pascal
RunMode: TRunMode = rmLocal;  // rmLocal or rmService
HttpHost = 'localhost';
HttpPort = '11111';
```

## String Types in DTOs

- **DTO / SOA / ORM layer**: use `RawUtf8` (mORMot2 native; no conversion overhead)
- **UI layer**: use `string` (VCL/LCL component properties expect `string`)
- **Boundary conversion**: `Utf8ToString()` / `StringToUtf8()` at the UI edge only

`string` varies by platform (AnsiString on Delphi 7, UnicodeString on modern Delphi/FPC).
`RawUtf8` is consistent everywhere and matches mORMot2's internal JSON/UTF-8 processing.

## Delphi 7 RTTI for Records

Delphi 7 only generates `TypeInfo()` for records containing at least one managed-type field
(`RawUtf8`, `string`, dynamic array, interface, Variant). Records with only value-type fields
(integer, currency, double, TDateTime) have **no RTTI** and will fail with
*"has no type information"* when used in SOA interface parameters.

**Fix** (both steps required):

1. Add a managed-type field to the record (e.g. `Timestamp: RawUtf8`)
2. Register the field layout in the `initialization` section:

```pascal
initialization
  {$ifndef HASEXTRECORDRTTI}
  Rtti.RegisterFromText(TypeInfo(TMyRecord),
    'Field1,Field2: integer; Field3: RawUtf8');
  {$endif HASEXTRECORDRTTI}
```

Step 1 makes `TypeInfo()` compile. Step 2 tells mORMot the field names/types for JSON
serialization on Delphi 7 (which lacks extended RTTI). See `ex/mvc-blog/MVCViewModel.pas`
for a real example.

## DB Query Optimization

- Minimize round-trips: combine multiple queries into one
- Use `CASE WHEN ... THEN ... ELSE ... END` for conditional aggregations
- Use `COALESCE()` for NULL handling
- Prefer one complex query over multiple simple queries

## Development Workflow

1. **Plan**: Develop 2-3 alternatives, recommend best, wait for user decision
2. **Implement**: Make changes, update file headers
3. **Compile**: Both projects must build cleanly:
   - GUI: `${LAZBUILD_PATH} src/Rechnung.lpi`
   - Daemon: `${LAZBUILD_PATH} src/RechnungDaemon.lpi`
4. **Test**: User runs and confirms
5. **Document**: Update `docs/IMPLEMENTATION-PLAN.md` or `docs/SOA-IMPLEMENTATION-PLAN.md` if needed
6. **Commit**: Only after user confirms. Format: `Phase X: <Title>`

**Bug fixes follow the same workflow** - research first, develop alternatives, let user decide.

**Prohibited**: Writing code without user decision. Quick fixes without analysis. Assumptions without documentation check.

## Daemon

`src/RechnungDaemon.dpr` — `TSynDaemon`-based server process.

```bash
# Linux
./RechnungDaemon --console     # foreground with logging
./RechnungDaemon --run          # run as background process
./RechnungDaemon --fork         # daemonize

# Windows
RechnungDaemon.exe /console    # foreground
RechnungDaemon.exe /install    # install as Windows service
RechnungDaemon.exe /start      # start service
RechnungDaemon.exe /stop       # stop service
RechnungDaemon.exe /uninstall  # remove service
```

Port: 11111 (configurable via daemon settings JSON).

## Documentation

- `docs/UI-DESIGN.md` - UI specification
- `docs/IMPLEMENTATION-PLAN.md` - Implementation roadmap
- `docs/SOA-IMPLEMENTATION-PLAN.md` - SOA migration plan
- `Components/mdlayout_usage.md` - Layout guide
- Always use `docs/` directory (not `doc/`) for documentation files
