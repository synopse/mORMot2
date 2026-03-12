# SOA Implementation Plan 1: DTO String Migration + Delphi 7 RTTI Fix

## References

- Read `CLAUDE.md` section "String Types in DTOs" and "Delphi 7 RTTI for Records"
- Read `docs/SOA-IMPLEMENTATION-PLAN.md` for existing architecture
- Pattern reference: `ex/mvc-blog/MVCViewModel.pas` (RegisterFromText example)

## Goal

1. Fix Delphi 7 compilation error: `TDtoDashboardStats besitzt keine Typinformationen`
2. Migrate all DTO record `string` fields to `RawUtf8` (mORMot2 convention)
3. Add `Utf8ToString()` / `StringToUtf8()` conversions at UI boundaries
4. Register record RTTI for Delphi 7 compatibility

## Affected Records (in `rgDtoTypes.pas`)

| Record | String Fields to Convert | RTTI Issue |
|--------|--------------------------|------------|
| `TDtoInvoiceItem` | `PartNo`, `Description` | no (has string) |
| `TDtoCustomer` | `CustomerNo`, `Company`, `Phone`, `Fax`, `Address`, `Zip`, `City`, `Country` | no |
| `TDtoContact` | `FirstName`, `MiddleName`, `LastName`, `Phone`, `Fax`, `Address`, `Zip`, `City`, `Country` | no |
| `TDtoOrder` | `OrderNo`, `CustomerNo`, `Company`, `ShipAddress`, `ShipZip`, `ShipCity`, `ShipCountry` | no |
| `TDtoOpenItem` | `Company`, `OrderNo` | no |
| `TDtoPaymentReceipt` | `Company`, `OrderNo` | no |
| `TDtoCustomerRevenue` | `Company` | no |
| `TDtoMonthlyOverview` | `MonthName` | no |
| `TDtoInvoiceDetail` | `OrderNo`, `CustomerName` | no |
| `TDtoInvoiceSave` | `OrderNo` | no |
| **`TDtoDashboardStats`** | **none (add `Timestamp`)** | **YES** |
| `TDtoCustomerSummary` | `CustomerName` | no |

---

## Phases

### Phase F1: Fix `TDtoDashboardStats` RTTI

| # | Task | Status |
|---|------|--------|
| F1.1 | Add `Timestamp: RawUtf8` field to `TDtoDashboardStats` in `rgDtoTypes.pas` | done |
| F1.2 | Add `mormot.core.rtti` to `rgDtoTypes.pas` uses clause | done |
| F1.3 | Add `{$ifndef HASEXTRECORDRTTI}` / `Rtti.RegisterFromText` block in `rgDtoTypes.pas` initialization for `TDtoDashboardStats` | done |
| F1.4 | Update `rgServiceImplementation.pas` `GetDashboardStats` to populate `AStats.Timestamp` (uses `NowUtcToString`) | done |
| F1.5 | Compile both projects | done |

### Phase F2: Convert DTO `string` fields to `RawUtf8`

All `string` fields in all DTO records become `RawUtf8`. This is a single bulk change in `rgDtoTypes.pas`.

| # | Task | Status |
|---|------|--------|
| F2.1 | Change all `string` fields to `RawUtf8` in all 12 DTO records in `rgDtoTypes.pas` | done |
| F2.2 | Removed `Classes` from uses clause (`mormot.core.base` already added in F1.2) | done |
| F2.3 | Compile — no errors on FPC (both `RawUtf8` and `string` are `AnsiString`); UI conversions still needed for Delphi compat | done |

### Phase F3: Add UI boundary conversions

Every place where a DTO `RawUtf8` field is read into a UI component needs `Utf8ToString()`,
and every place where a UI value is written into a DTO field needs `StringToUtf8()`.

#### F3.1: `rgCustomerEdit.pas` (~16 lines)

| # | Task | Status |
|---|------|--------|
| F3.1.1 | Load path (lines 285-292): wrap 8 field reads with `Utf8ToString()` — `EditCompany.Text := Utf8ToString(Customer.Company)` etc. | done |
| F3.1.2 | Save path (lines 348-355): wrap 8 field writes with `StringToUtf8()` — `Customer.Company := StringToUtf8(Trim(EditCompany.Text))` etc. Added `mormot.core.unicode` to uses. | done |

#### F3.2: `rgCustomerList.pas` (~3 lines)

| # | Task | Status |
|---|------|--------|
| F3.2.1 | Filter logic (lines 229-231): removed redundant `StringToUtf8()` wrappers since DTO fields are already `RawUtf8` | done |
| F3.2.2 | Grid display (lines 242-243): wrapped `Customers[i].Company` / `.City` with `Utf8ToString()` | done |

#### F3.3: `rgInvoiceEdit.pas` (~4 lines)

| # | Task | Status |
|---|------|--------|
| F3.3.1 | Load path: wrapped `Detail.CustomerName`, `Detail.OrderNo`, `Summary.CustomerName`, items `Description` with `Utf8ToString()` | done |
| F3.3.2 | Save path: wrapped `Invoice.OrderNo` write with `StringToUtf8()` | done |

#### F3.4: `rgInvoiceItemEdit.pas` (~2 lines)

| # | Task | Status |
|---|------|--------|
| F3.4.1 | Load: wrapped `AItem.Description` read with `Utf8ToString()` | done |
| F3.4.2 | Save: wrapped `FItem.Description` write with `StringToUtf8()` | done |

#### F3.5: `rgInvoiceList.pas` (~2 lines)

| # | Task | Status |
|---|------|--------|
| F3.5.1 | Grid display: wrapped `Invoices[i].OrderNo` with `Utf8ToString()` | done |
| F3.5.2 | Payment call: wrapped `Invoice.OrderNo` parameter with `Utf8ToString()` | done |

#### F3.6: Report forms (~5 lines)

| # | Task | Status |
|---|------|--------|
| F3.6.1 | `rgReportOpenItems.pas`: wrapped `Items[i].Company`, `.OrderNo` with `Utf8ToString()` | done |
| F3.6.2 | `rgReportPayments.pas`: wrapped `Items[i].Company`, `.OrderNo` with `Utf8ToString()` | done |
| F3.6.3 | `rgReportRevenue.pas`: wrapped `Items[i].Company` with `Utf8ToString()` | done |
| F3.6.4 | `rgReportMonthly.pas`: wrapped `Items[i].MonthName` / `Totals.MonthName` with `Utf8ToString()`, added `mormot.core.base` to uses | done |

#### F3.7: `rgMain.pas` (~1 line)

| # | Task | Status |
|---|------|--------|
| F3.7.1 | Wrapped `Summary.CustomerName` with `Utf8ToString()`, added `mormot.core.base` to uses | done |

#### F3.8: Compile

| # | Task | Status |
|---|------|--------|
| F3.8.1 | Compile both projects, fix any remaining type mismatches | done |

### Phase F4: Remove redundant conversions in `rgServiceImplementation.pas`

The service implementation currently converts ORM `RawUtf8` fields to DTO `string` via
`Utf8ToString()`, and converts back via `StringToUtf8()`. With DTOs now using `RawUtf8`,
these conversions become unnecessary — direct assignment suffices.

| # | Task | Status |
|---|------|--------|
| F4.1 | `ListCustomers` (lines 138-151): remove `Utf8ToString()` wrappers — direct assign `ACustomers[Count].Company := Customer.Company` etc. | done |
| F4.2 | `GetCustomer` (lines 178-191): remove `Utf8ToString()` wrappers | done |
| F4.3 | `CreateCustomer` (lines 222-233): remove `StringToUtf8()` wrappers — direct assign `Customer.Company := ACustomer.Company` etc. | done |
| F4.4 | `UpdateCustomer`: remove `StringToUtf8()` wrappers | done |
| F4.5 | `ListInvoicesForCustomer` (lines 373+): remove `Utf8ToString()` wrappers on OrderNo field | done |
| F4.6 | `GetInvoice` (lines 425-456): remove `Utf8ToString()` wrappers on OrderNo, CustomerName, item PartNo/Description | done |
| F4.7 | `CreateInvoice` / `UpdateInvoice` (lines 503-574): remove `StringToUtf8()` wrappers | planned |
| F4.8 | Report methods: remove `Utf8ToString()` wrappers on Company, OrderNo, MonthName fields | planned |
| F4.9 | Compile both projects | planned |

### Phase F5: RegisterFromText for all DTO records (Delphi 7)

Add `Rtti.RegisterFromText` calls for all DTO records in `rgDtoTypes.pas` initialization,
guarded by `{$ifndef HASEXTRECORDRTTI}`.

| # | Task | Status |
|---|------|--------|
| F5.1 | Register all 12 record types with their full field definitions | done |
| F5.2 | Compile both projects | done |

Expected initialization section:

```pascal
initialization
  {$ifndef HASEXTRECORDRTTI}
  Rtti.RegisterFromText(TypeInfo(TDtoInvoiceItem),
    'Position: integer; PartNo: RawUtf8; Description: RawUtf8; ' +
    'Quantity: double; ListPrice: currency; Discount: integer; Amount: currency');
  Rtti.RegisterFromText(TypeInfo(TDtoCustomer),
    'CustomerID: longint; CustomerNo: RawUtf8; Company: RawUtf8; ' +
    'Phone: RawUtf8; Fax: RawUtf8; Address: RawUtf8; ' +
    'Zip: RawUtf8; City: RawUtf8; Country: RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDtoContact),
    'FirstName: RawUtf8; MiddleName: RawUtf8; LastName: RawUtf8; ' +
    'Phone: RawUtf8; Fax: RawUtf8; Address: RawUtf8; ' +
    'Zip: RawUtf8; City: RawUtf8; Country: RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDtoOrder),
    'OrderID: longint; OrderNo: RawUtf8; SaleDate: TDateTime; ShipDate: TDateTime; ' +
    'ItemsTotal: currency; AmountPaid: currency; OpenAmount: currency; ' +
    'Status: TInvoiceStatus; CustomerNo: RawUtf8; Company: RawUtf8; ' +
    'ShipAddress: RawUtf8; ShipZip: RawUtf8; ShipCity: RawUtf8; ShipCountry: RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDtoOpenItem),
    'OrderID: longint; Company: RawUtf8; OrderNo: RawUtf8; ' +
    'SaleDate: TDateTime; ItemsTotal: currency; OpenAmount: currency; DaysOverdue: integer');
  Rtti.RegisterFromText(TypeInfo(TDtoPaymentReceipt),
    'OrderID: longint; SaleDate: TDateTime; Company: RawUtf8; ' +
    'OrderNo: RawUtf8; AmountPaid: currency');
  Rtti.RegisterFromText(TypeInfo(TDtoCustomerRevenue),
    'CustomerID: longint; Company: RawUtf8; InvoiceCount: integer; ' +
    'TotalRevenue: currency; TotalPaid: currency; TotalOpen: currency');
  Rtti.RegisterFromText(TypeInfo(TDtoMonthlyOverview),
    'Month: integer; MonthName: RawUtf8; InvoiceCount: integer; ' +
    'Revenue: currency; PaymentsReceived: currency; OpenAmount: currency');
  Rtti.RegisterFromText(TypeInfo(TDtoInvoiceDetail),
    'OrderID: longint; OrderNo: RawUtf8; SaleDate: TDateTime; ShipDate: TDateTime; ' +
    'CustomerID: longint; CustomerName: RawUtf8; ItemsTotal: currency; ' +
    'AmountPaid: currency; OpenAmount: currency; Status: TInvoiceStatus; ' +
    'Items: [Position: integer; PartNo: RawUtf8; Description: RawUtf8; ' +
    'Quantity: double; ListPrice: currency; Discount: integer; Amount: currency]');
  Rtti.RegisterFromText(TypeInfo(TDtoInvoiceSave),
    'OrderNo: RawUtf8; SaleDate: TDateTime; ShipDate: TDateTime; ' +
    'Items: [Position: integer; PartNo: RawUtf8; Description: RawUtf8; ' +
    'Quantity: double; ListPrice: currency; Discount: integer; Amount: currency]');
  Rtti.RegisterFromText(TypeInfo(TDtoDashboardStats),
    'CustomerCount: integer; OpenItemsCount: integer; OpenItemsAmount: currency; ' +
    'DueTodayCount: integer; OverdueCount: integer; Timestamp: RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDtoCustomerSummary),
    'CustomerID: longint; CustomerName: RawUtf8; InvoiceCount: integer; ' +
    'TotalRevenue: currency; OpenCount: integer; OpenAmount: currency; PaidCount: integer');
  {$endif HASEXTRECORDRTTI}
```

---

## File Change Summary

| File | Changes |
|------|---------|
| `src/rgDtoTypes.pas` | All `string` → `RawUtf8`, add `Timestamp` to `TDtoDashboardStats`, add uses `mormot.core.base` + `mormot.core.rtti`, add `RegisterFromText` initialization |
| `src/rgServiceImplementation.pas` | Remove ~40 redundant `Utf8ToString()`/`StringToUtf8()` wrappers |
| `src/rgCustomerEdit.pas` | Add `Utf8ToString()`/`StringToUtf8()` at 16 UI boundary points |
| `src/rgCustomerList.pas` | Adjust filter logic (~3 lines), add `Utf8ToString()` for grid display (~2 lines) |
| `src/rgInvoiceEdit.pas` | Add conversions at ~4 UI boundary points |
| `src/rgInvoiceItemEdit.pas` | Add conversions at ~2 UI boundary points |
| `src/rgInvoiceList.pas` | Add conversions at ~2 UI boundary points |
| `src/rgReportOpenItems.pas` | Add `Utf8ToString()` at ~2 grid display points |
| `src/rgReportPayments.pas` | Add `Utf8ToString()` at ~2 grid display points |
| `src/rgReportRevenue.pas` | Add `Utf8ToString()` at ~1 grid display point |
| `src/rgReportMonthly.pas` | Add `Utf8ToString()` at ~2 grid display points |
| `src/rgMain.pas` | Add `Utf8ToString()` at ~1 point |

**Total**: ~12 files, ~33 UI conversion points, ~40 removed service-layer conversions

## Verify

```bash
${LAZBUILD_PATH} src/Rechnung.lpi
${LAZBUILD_PATH} src/RechnungDaemon.lpi
```
