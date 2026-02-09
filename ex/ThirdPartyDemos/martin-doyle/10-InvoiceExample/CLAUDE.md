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

## Service Pattern

Services in `rgClient.pas` abstract DB access from UI. Interface + implementation pattern:

```pascal
ICustomerService = interface(IInvokable)
  procedure LoadCustomers;
  function GetCustomer: TDtoCustomer;
end;
```

Global client: `RgRestClient` initialized via `initialization`/`finalization` in `rgClient.pas`.

## DB Query Optimization

- Minimize round-trips: combine multiple queries into one
- Use `CASE WHEN ... THEN ... ELSE ... END` for conditional aggregations
- Use `COALESCE()` for NULL handling
- Prefer one complex query over multiple simple queries

## Development Workflow

1. **Plan**: Develop 2-3 alternatives, recommend best, wait for user decision
2. **Implement**: Make changes, update file headers
3. **Compile**: `${LAZBUILD_PATH} ${PROJECT_PATH}/Rechnung.lpi`
4. **Test**: User runs and confirms
5. **Document**: Update `docs/IMPLEMENTATION-PLAN.md` if needed
6. **Commit**: Only after user confirms. Format: `Phase X: <Title>`

**Bug fixes follow the same workflow** - research first, develop alternatives, let user decide.

**Prohibited**: Writing code without user decision. Quick fixes without analysis. Assumptions without documentation check.

## Documentation

- `docs/UI-DESIGN.md` - UI specification
- `docs/IMPLEMENTATION-PLAN.md` - Implementation roadmap
- `Components/mdlayout_usage.md` - Layout guide
- Always use `docs/` directory (not `doc/`) for documentation files
