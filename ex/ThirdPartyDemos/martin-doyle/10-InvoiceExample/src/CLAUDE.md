# Rechnung

Lazarus/Free Pascal application using mORMot2 framework.

## Language

All code, comments, variables, and identifiers must be in English.

## Technology Stack

- **IDE**: Lazarus (LCL Version 4.5.0.0)
- **Language**: Free Pascal (FPC)
- **Framework**: mORMot2 (Synopse)
- **Platforms**: Windows, macOS, Linux

## Project Structure

- `*.pas` - Pascal units
- `*.dfm` - Form definitions (Lazarus compatible)
- `*.lps` - Lazarus project session
- `Components/` - Reusable components
- `docs/` - Documentation

## Documentation

**Important**: Always use `docs/` directory (not `doc/`) for documentation files.

| Document | Purpose |
|----------|---------|
| `docs/UI-DESIGN.md` | Complete UI specification |
| `docs/IMPLEMENTATION-PLAN.md` | Phased implementation roadmap |
| `Components/mdlayout_usage.md` | mdLayout framework guide |

## UI Design

See `docs/UI-DESIGN.md` for the complete UI specification.

Key principles:
- Navigation via menu only (no sidebar buttons)
- Customer list left, invoices right
- Payments integrated in invoice list (no separate payment module)
- Global statistics in top panel
- Modal dialogs for editing (with mdLayout)
- Reports as modal dialogs with filter + print

### UI Localization

**Always use system locale settings** for displaying and parsing user input:

| Data Type | Display | Parse |
|-----------|---------|-------|
| Date | `DateToStr()` | `TryStrToDate()` |
| Currency | `Format('%.2n', [Amount])` | `TryStrToCurr()` |
| Float | `FloatToStr()` / `Format()` | `TryStrToFloat()` |

**Rules**:
- **UI layer**: Use system locale (`FormatSettings`, `DefaultFormatSettings`)
- **Database/ORM/DTO**: Use fixed formats (mORMot2 handles this internally)
- **Never hardcode** formats like `'dd.mm.yyyy'` or `','` as decimal separator in UI code
- Error messages should show expected format via `FormatSettings.ShortDateFormat`

**Example** (correct):
```pascal
// Display date in system locale
EditDate.Text := DateToStr(FDate);

// Parse date with system locale
if not TryStrToDate(EditDate.Text, TempDate) then
  ShowMessage(Format('Invalid date format (%s)', [FormatSettings.ShortDateFormat]));
```

**Example** (wrong - hardcoded German format):
```pascal
// DON'T do this!
EditDate.Text := FormatDateTime('dd.mm.yyyy', FDate);
```

## Form Architecture

The application uses two form types:

### Child Windows (Main Content)

Child windows are displayed within the main form. They inherit from `TMDChildForm`.

- Use `Align` properties (`alClient`, `alBottom`, `alTop`) for layout
- Override `GetFormMenu` to provide form-specific menus
- Do NOT use `mdLayout` for child windows

Example structure (see `rgCustomerList.pas`):
```pascal
TCustomerListForm = class(TMDChildForm)
  CustomerListView: TListView;  // Align = alClient
  ButtonPanel: TPanel;          // Align = alBottom
  ...
  function GetFormMenu: TMainMenu; override;
end;
```

### Modal Dialogs

Modal dialogs use `mdLayout` for platform-independent, responsive UI design.

- Inherit from `TMDDBModeForm` for data entry forms
- Use `BorderStyle = bsDialog`
- Call `ShowModal` to display

Key rules:
- Layout code belongs in `FormShow` or `FormCreate`
- Always call `AdjustForPlatform` early
- Call `AutoSizeForm` at the end
- Always free `TLayoutHelper` in try/finally block

See `Components/mdlayout_usage.md` for details.

## Code Conventions

- Units use prefixes: `rg` (Rechnung), `md` (MartinDoyle)
- Forms end with `Form` (e.g., `TCustomerEditForm`)
- Services implement interfaces (e.g., `ICustomerService`)

## File Header

Every Pascal unit must include the standard file header.

**Important**: Only update the header when the file is actually modified for functional changes. Do not update headers proactively or in bulk.

When creating or modifying files:

1. **Copy the header** from an existing file (e.g., `rgMain.pas`) for new files
2. **Update copyright year** to current year: `© martindoyle 2017-2026`
3. **Update "Last modified" section**:
   - Date: Current date (format: `DD.MM.YYYY`)
   - Author: `Martin Doyle`
   - Email: `martin-doyle@online.de`

### Header Template

```pascal
{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : <unit_name>.pas

  Last modified
    Date : <DD.MM.YYYY>
    Author : Martin Doyle
    Email : martin-doyle@online.de

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to
    deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
    sell copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
    ----------------------------------------------------------------------------
}
```

## Development Workflow

Follow this workflow for all implementation tasks:

### 1. Show Changes (Plan)

- Develop 2-3 alternative solutions (e.g., DFM changes vs. source code changes)
- Recommend the best solution with reasoning
- Ask the user for their decision
- List affected files and components
- Describe the chosen approach

### IMPORTANT: Bug Fixes and Technical Decisions

**Bug fixes also follow the complete workflow:**

1. **Research first** - Consult framework documentation (mORMot2, LCL)
2. **Understand the problem** - Do not act based on assumptions
3. **Develop alternatives** - At least 2-3 solution approaches
4. **Let user decide** - ALWAYS wait for explicit confirmation
5. **Only then implement**

**Prohibited:**
- Writing code without prior user decision
- "Quick fixes" without analysis
- Assumptions about framework behavior without documentation check

### 2. Implement

- Make the code changes
- Update file headers (copyright year, modified date)
- Follow code conventions

### 3. Compile

Build the project using lazbuild:

```bash
${LAZBUILD_PATH} ${PROJECT_PATH}/Rechnung.lpi
```

**Paths** (configure in `.claude.local`):
- `LAZBUILD_PATH`: Path to lazbuild executable (e.g., `~/fpcupdeluxe/lazarus/lazbuild`)
- `PROJECT_PATH`: Path to project directory

### 4. Test

- Ask user to run and test the application
- Wait for user confirmation (OK) before proceeding
- Fix any issues reported by user

### 5. Update Documentation

- Update comments in code if logic changed
- Update `docs/IMPLEMENTATION-PLAN.md` with progress
- Update `CLAUDE.md` if conventions or architecture changed

### 6. Commit

- Only commit after user confirms successful test
- Commit message format: `Phase X: <Title>`
- Example: `Phase 2: QuickInfo Panel`

## Data Architecture

The application uses **local SQLite database only** (no HTTP/network).

### Layers

```
┌─────────────────────────────────────┐
│  UI (Forms)                         │
├─────────────────────────────────────┤
│  Services (ICustomerService, etc.)  │
├─────────────────────────────────────┤
│  DTOs (TDtoCustomer, TDtoOrder)     │
├─────────────────────────────────────┤
│  ORM (TOrmCustomer, TOrmOrder)      │
├─────────────────────────────────────┤
│  REST Client (TRgRestClient)        │
├─────────────────────────────────────┤
│  SQLite Database (local file)       │
└─────────────────────────────────────┘
```

### Key Files

| File | Purpose |
|------|---------|
| `rgClient.pas` | REST client, service interfaces and implementations |
| `rgData.pas` | ORM model definitions (`TOrmCustomer`, etc.) |
| `rgDtoTypes.pas` | Data Transfer Objects for UI layer |

### Service Pattern

Services abstract database access from UI:

```pascal
// Interface definition
ICustomerService = interface(IInvokable)
  procedure LoadCustomers;
  function GetCustomer: TDtoCustomer;
end;

// Implementation
TCustomerService = class(TInterfacedObject, ICustomerService)
private
  FRestOrm: IRestOrm;
public
  constructor Create;
  // ... interface methods
end;

// Usage in forms
var
  Service: ICustomerService;
begin
  Service := TCustomerService.Create;
  Service.LoadCustomers;
end;
```

### Global Client

`RgRestClient` is initialized at unit startup (`initialization`/`finalization` in `rgClient.pas`).

### Database Query Optimization

**Principle**: Minimize database round-trips. Combine multiple queries into single queries when fetching data from the same table(s).

**Bad** - Multiple queries for related data:
```pascal
// 5 separate queries to get statistics
FCustomerCount := FRestOrm.TableRowCount(TOrmCustomer);
CountStr := FRestOrm.OneFieldValue(TOrmCustomerOrder, 'COUNT(*)', ...);
AmountStr := FRestOrm.OneFieldValue(TOrmCustomerOrder, 'SUM(...)', ...);
CountStr := FRestOrm.OneFieldValue(TOrmCustomerOrder, 'COUNT(*)', ...);
CountStr := FRestOrm.OneFieldValue(TOrmCustomerOrder, 'COUNT(*)', ...);
```

**Good** - Single query with multiple aggregations:
```pascal
// 1 query with all statistics
SQL := 'SELECT ' +
  'COUNT(*) as TotalCount, ' +
  'SUM(CASE WHEN open THEN 1 ELSE 0 END) as OpenCount, ' +
  'COALESCE(SUM(CASE WHEN open THEN Amount ELSE 0 END), 0) as OpenAmount, ' +
  'SUM(CASE WHEN due_today THEN 1 ELSE 0 END) as DueCount ' +
  'FROM CustomerOrder';
```

**Guidelines**:
- Use `CASE WHEN ... THEN ... ELSE ... END` for conditional aggregations
- Use `COALESCE()` for NULL handling
- Prefer one complex query over multiple simple queries
- Consider using mORMot's `ExecuteJson` or raw SQL for complex aggregations

## Constants and Configuration

All application constants are centralized in `rgConst.pas`:

| Section | Content | Naming |
|---------|---------|--------|
| `const` | App version, file names, paths | PascalCase |
| `resourcestring` | User-facing messages | Prefix `m` (e.g., `mErrorNoDatabase`) |
| `var` | Runtime paths (initialized at startup) | PascalCase |

The unit initialization section configures:
- Application and data paths
- mORMot2 logging (`TSynLog`)
- Database file validation

Example:
```pascal
const
  ApplicationTitle = 'Rechnung';
  AppVersionMajor = 2;

resourcestring
  mErrorNoDatabase = 'Database not found.';

var
  DataPath: string;  // Set during initialization
```

## Local Configuration

Machine-specific paths are stored in `.claude.local` (not committed).
Copy `.claude.local.example` to `.claude.local` and adjust paths for your system.

## mORMot2

**Important**: Always prefer mORMot2 functions over Free Pascal or Lazarus RTL/library functions. Search mORMot2 first for any required functionality before falling back to FPC/LCL alternatives.

**Source Location**: `${MORMOT2_PATH}` (see `.claude.local`)
- `src/` - Source units organized by module (core, db, net, orm, rest, etc.)
- `test/` - Test units with usage examples
- `doc/` - SAD documentation (mORMot2-SAD-Chapter-*.md)

**CLAUDE.md in Source Directories**: Each `src/` subdirectory contains a `CLAUDE.md` file with detailed guidance for that module. These files provide:
- Architecture principles and dependency order
- Key unit descriptions and usage patterns
- Common patterns and best practices
- Cross-compiler considerations (FPC/Delphi)

Always consult these files when working with mORMot2 functionality:
- `src/core/CLAUDE.md` - Base types, JSON, RTTI, logging
- `src/orm/CLAUDE.md` - Object-Relational Mapping
- `src/rest/CLAUDE.md` - REST client/server
- `src/db/CLAUDE.md` - Database connectivity

### Module Structure

| Folder | Purpose |
|--------|---------|
| `src/core/` | Base types, OS abstraction, JSON, text utilities |
| `src/db/` | Database connectivity (SQLite, PostgreSQL, etc.) |
| `src/net/` | Network protocols (HTTP, WebSocket, etc.) |
| `src/orm/` | Object-Relational Mapping |
| `src/rest/` | REST client/server |
| `src/crypt/` | Cryptography |
| `src/lib/` | External library wrappers |

### Common Units

```pascal
uses
  mormot.core.base,    // fundamental types
  mormot.core.os,      // OS abstraction
  mormot.core.text,    // string/text utilities
  mormot.core.unicode, // UTF-8 handling
  mormot.core.datetime,// date/time functions
  mormot.core.json;    // JSON parsing
```

Includes: `{$I mormot.defines.inc}`
