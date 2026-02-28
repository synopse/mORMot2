# Layout Integration Plan: mdLayout into mdForms (Option A)

## References

- Read `CLAUDE.md` (project root)
- Read `Components/mdlayout.pas` (TLayoutHelper, TLayoutBuilder, helper functions)
- Read `Components/mdforms.pas` (TMDChildForm, TMDDBModeForm, TMDDBNavStatForm)

## Goal

Eliminate layout boilerplate by integrating `TLayoutHelper` into `TMDChildForm`.
Forms call `InitLayout(BaseHeight)` once, then use `Layout.Place(...)` directly.
No behavior change. Pure mechanical refactoring.

## Current State (problem)

Every form repeats ~10 lines:
```pascal
var Layout: TLayoutHelper; Margins: TLayoutMargins; BaseHeight: Integer;
begin
  BaseHeight := SomeLabel.Height;
  Margins := LayoutMargins(BaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;
    // ... layout calls ...
  finally
    Layout.Free;
  end;
end;
```

8 forms affected: `rgCustomerEdit`, `rgInvoiceEdit`, `rgInvoiceItemEdit`, `rgPaymentEntry`, `rgAbout`, `rgReportBase`, `rgCustomerList`, `rgInvoiceList`.

## Target State

```pascal
procedure TMyForm.SetupLayout;
begin
  InitLayout(LabelFoo.Height, EditFoo.Height, 7.0, 18.75);

  // Prepare label-edit pairs (AutoSize off, uniform sizes, centered, FocusControl)
  PrepareLabelEdit(LabelFoo, EditFoo);
  PrepareLabelEdit(LabelBar, EditBar);

  // Position first label
  LabelFoo.SetBounds(Layout.Margins.Left, Layout.Margins.Top, LabelWidth, EditHeight);
  Layout.Place(LabelFoo, EditFoo, ldRight, 1.0);

  // Position remaining pairs
  Layout.Place(LabelFoo, LabelBar, ldBelow, 0.5);
  Layout.Place(LabelBar, EditBar, ldRight, 1.0);

  Layout.AutoSizeForm;
end;
```

## Class Hierarchy

```
TMDChildForm              <-- FLayout lives here
  +-- TMDDBModeForm       <-- inherits Layout
  |     +-- TMDDBNavStatForm
```

## Phases

### Phase 1: Integrate layout into TMDChildForm

| # | Task | Status |
|---|------|--------|
| 1.1 | Add `mdLayout` to `mdforms.pas` uses clause (interface section) | done |
| 1.2 | Add to `TMDChildForm`: `FLayout`, `FLabelHeight`, `FEditHeight`, `FLabelWidth`, `FEditWidth` fields; `Layout`, `LabelHeight`, `EditHeight`, `LabelWidth`, `EditWidth` properties; `InitLayout(ALabelHeight, AEditHeight: Integer; ALabelWidthMult, AEditWidthMult: Single)` | done |
| 1.3 | Implement `InitLayout`: store dimensions, create `FLayout` with `LayoutMargins(ALabelHeight)`, call `AdjustForPlatform` | done |
| 1.4 | Add destructor `Destroy` override to `TMDChildForm`: free `FLayout` | done |
| 1.5 | Add `PrepareLabel(ALabel)`, `PrepareEdit(AEdit)`, `PrepareLabelEdit(ALabel, AEdit)` helper methods to `TMDChildForm`. `PrepareLabel`: AutoSize off, uniform width/height, tlCenter. `PrepareEdit`: uniform width. `PrepareLabelEdit`: calls both + sets FocusControl. Preparation only, no placement. | done |
| 1.6 | Compile both projects | done |

### Phase 2: Migrate modal dialog forms (TMDDBModeForm descendants)

| # | Task | Status |
|---|------|--------|
| 2.1 | Migrate `rgCustomerEdit.pas`: keep `mdLayout` in uses (needed for `ldRight`/`ldBelow`), remove local Layout/Margins/BaseHeight vars, replace `Layout := TLayoutHelper.Create(...)` + try/finally/free with `InitLayout(...)`, use `Layout.` property, replace `Margins.*` with `Layout.Margins.*`. **Needs label height fix (2.1b).** | done |
| 2.1b | Fix `rgCustomerEdit.pas`: set label heights to edit height before placement to fix vertical misalignment between labels and edits | done |
| 2.2 | Migrate `rgInvoiceItemEdit.pas`: same pattern | pending |
| 2.3 | Migrate `rgPaymentEntry.pas`: same pattern | pending |
| 2.4 | Migrate `rgInvoiceEdit.pas`: same pattern (more complex layout, keep grid/button manual positioning) | pending |
| 2.5 | Migrate `rgAbout.pas`: same pattern | pending |
| 2.6 | Migrate `rgReportBase.pas`: same pattern | pending |
| 2.7 | Compile both projects | pending |

### Phase 3: Migrate child forms (TMDChildForm descendants)

| # | Task | Status |
|---|------|--------|
| 3.1 | Migrate `rgCustomerList.pas`: replace manual margin/spacing arithmetic in `SetupLayout` with `InitLayout` + `Layout.Place`/`Layout.PlaceRow` | pending |
| 3.2 | Migrate `rgInvoiceList.pas`: same pattern for toolbar + legend layout | pending |
| 3.3 | Compile both projects | pending |

### Phase 4: Cleanup

| # | Task | Status |
|---|------|--------|
| 4.1 | Remove `LayoutAboutDialog` procedure from `mdlayout.pas` (no longer used) | pending |
| 4.2 | Update `CLAUDE.md` form architecture section: remove "Always free TLayoutHelper in try/finally", add `InitLayout` pattern | pending |
| 4.3 | Final compile both projects | pending |

## File Changes

### Modified
| File | Change |
|------|--------|
| `Components/mdforms.pas` | +`mdLayout` uses, +`FLayout`/`FLabelHeight`/`FEditHeight`/`FLabelWidth`/`FEditWidth` fields, +properties, +`InitLayout(4 params)`, +`PrepareLabel`/`PrepareEdit`/`PrepareLabelEdit`, +`Destroy` |
| `Components/mdlayout.pas` | Remove `LayoutAboutDialog` (dead code after migration) |
| `src/rgCustomerEdit.pas` | Keep `mdLayout` uses, simplify `SetupLayout` |
| `src/rgInvoiceEdit.pas` | Keep `mdLayout` uses, simplify `SetupLayout` |
| `src/rgInvoiceItemEdit.pas` | Keep `mdLayout` uses, simplify `SetupLayout` |
| `src/rgPaymentEntry.pas` | Keep `mdLayout` uses, simplify `SetupLayout` |
| `src/rgAbout.pas` | Keep `mdLayout` uses, simplify `FormShow` layout |
| `src/rgReportBase.pas` | Keep `mdLayout` uses, simplify `SetupBaseLayout` |
| `src/rgCustomerList.pas` | Add `InitLayout`, replace manual arithmetic with `Layout.Place` |
| `src/rgInvoiceList.pas` | Add `InitLayout`, replace manual arithmetic with `Layout.Place` |
| `CLAUDE.md` | Update form architecture section |

### Unchanged
`mdlayout.pas` core classes (TLayoutHelper, TLayoutBuilder, record types) remain intact.

## Migration Pattern per Form

```
1. Keep `mdLayout` in uses clause (forms reference TLayoutDirection values directly)
2. Remove local var declarations: Layout, Margins, BaseHeight, LabelHeight, EditHeight, LabelWidth, EditWidth
3. Remove TLayoutHelper.Create + try/finally/Free wrapper
4. Add `InitLayout(SomeLabel.Height, SomeEdit.Height, LabelMult, EditMult)` as first line
5. Replace separate label/edit preparation with `PrepareLabelEdit(ALabel, AEdit)` calls
6. Replace local vars with properties: `LabelHeight`, `EditHeight`, `LabelWidth`, `EditWidth`
7. Replace `Margins.*` with `Layout.Margins.*`
8. `Layout.Place(...)` calls stay the same (property has same name as old local var)
9. Verify compile
```

## Verify

```bash
${LAZBUILD_PATH} src/Rechnung.lpi
${LAZBUILD_PATH} src/RechnungDaemon.lpi
```
