# SOA Implementation Plan: Interface-Based Services with Daemon Backend

## References

- Read `CLAUDE.md` (root: `martin-doyle/CLAUDE.md`)
- Read `10-InvoiceExample/CLAUDE.md`
- Read `docs/IMPLEMENTATION-PLAN.md` for workflow/status conventions
- Reference patterns: `04-InterfacedBasedServices/src/server.pas`, `04-InterfacedBasedServices/src/data.pas`, `05-HttpDaemonORM/`
- mORMot2 docs: `src/soa/CLAUDE.md`, `src/rest/CLAUDE.md`, `src/app/CLAUDE.md`

## Goal

Transform 10-InvoiceExample from standalone GUI into client-server SOA:
- Daemon backend: `TSynDaemon` + `TRestHttpServer` + `ServiceDefine` (sicShared)
- GUI client: `TRestHttpClient` + `ServiceDefine` + `Services[].Get()`
- JSON config: switch between local (embedded) and service (HTTP) mode
- Stateless request/response interfaces (mORMot2 recommended pattern)

## Architecture

```
[GUI Client] --HTTP/JSON--> [Daemon + SQLite + Services]
OR (local mode): [GUI + embedded Server + SQLite + Services]
```

Config: `rechnung.config.json` with `Mode`=`local`|`service`, `Host`, `Port`

---

## Interface Consolidation: 12 old -> 5 SOA

| New Interface | Replaces | Source Logic |
|---|---|---|
| `IRgCustomerService` | `ICustomerService` + `ICustomerEditService` | `rgClient.pas:TCustomerService`, `TCustomerEditService` |
| `IRgInvoiceService` | `IInvoiceService` + `IInvoiceEditService` + `ICustomerOrderService` | `rgClient.pas:TInvoiceService`, `TInvoiceEditService`, `TCustomerOrderService` |
| `IRgPaymentService` | `IPaymentService` | `rgClient.pas:TPaymentService` |
| `IRgStatisticsService` | `IStatisticsService` + `ICustomerSummaryService` | `rgClient.pas:TStatisticsService`, `TCustomerSummaryService` |
| `IRgReportService` | 4 report interfaces | `rgClient.pas:TOpenItemsReportService`, `TPaymentReceiptsReportService`, `TCustomerRevenueReportService`, `TMonthlyOverviewReportService` |

## SOA Interface Signatures

```pascal
IRgCustomerService = interface(IInvokable)
  function ListCustomers(out ACustomers: TDtoCustomerDynArray): TCustomerEditResult;
  function GetCustomer(ACustomerID: longint; out ACustomer: TDtoCustomer): TCustomerEditResult;
  function CreateCustomer(const ACustomer: TDtoCustomer; out ANewID: longint): TCustomerEditResult;
  function UpdateCustomer(ACustomerID: longint; const ACustomer: TDtoCustomer): TCustomerEditResult;
  function DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
  function GenerateCustomerNo(out ACustomerNo: RawUtf8): TCustomerEditResult;
end;

IRgInvoiceService = interface(IInvokable)
  function ListInvoicesForCustomer(ACustomerID: longint; out AInvoices: TDtoOrderDynArray): TInvoiceEditResult;
  function GetInvoice(AInvoiceID: longint; out AInvoice: TDtoInvoiceDetail): TInvoiceEditResult;
  function CreateInvoice(ACustomerID: longint; const AInvoice: TDtoInvoiceSave; out ANewID: longint): TInvoiceEditResult;
  function UpdateInvoice(AInvoiceID: longint; const AInvoice: TDtoInvoiceSave): TInvoiceEditResult;
  function DeleteInvoice(AInvoiceID: longint): TInvoiceEditResult;
  function GenerateOrderNo(out AOrderNo: RawUtf8): TInvoiceEditResult;
end;

IRgPaymentService = interface(IInvokable)
  function AddPayment(AInvoiceID: longint; AAmount: currency; ADate: TDateTime): TPaymentResult;
  function GetInvoiceOpenAmount(AInvoiceID: longint; out AOpenAmount: currency): TPaymentResult;
end;

IRgStatisticsService = interface(IInvokable)
  function GetDashboardStats(out AStats: TDtoDashboardStats): integer;
  function GetCustomerSummary(ACustomerID: longint; out ASummary: TDtoCustomerSummary): integer;
end;

IRgReportService = interface(IInvokable)
  function GetOpenItemsReport(AFromDate, AToDate: TDateTime; AMinAmount: currency; out AItems: TDtoOpenItemDynArray): integer;
  function GetPaymentReceiptsReport(AFromDate, AToDate: TDateTime; out AItems: TDtoPaymentReceiptDynArray): integer;
  function GetCustomerRevenueReport(AYear: integer; out AItems: TDtoCustomerRevenueDynArray): integer;
  function GetMonthlyOverviewReport(AYear: integer; out AItems: TDtoMonthlyOverviewDynArray; out ATotals: TDtoMonthlyOverview): integer;
end;
```

## New DTO Types (add to `rgDtoTypes.pas`)

```pascal
TDtoCustomerDynArray = array of TDtoCustomer;
TDtoOrderDynArray = array of TDtoOrder;
TDtoOpenItemDynArray = array of TDtoOpenItem;
TDtoPaymentReceiptDynArray = array of TDtoPaymentReceipt;
TDtoCustomerRevenueDynArray = array of TDtoCustomerRevenue;
TDtoMonthlyOverviewDynArray = array of TDtoMonthlyOverview;

TDtoInvoiceDetail = packed record
  OrderID: longint; OrderNo: string; SaleDate, ShipDate: TDateTime;
  CustomerID: longint; CustomerName: string;
  ItemsTotal, AmountPaid, OpenAmount: currency;
  Status: TInvoiceStatus; Items: TDtoInvoiceItemArray;
end;

TDtoInvoiceSave = packed record
  OrderNo: string; SaleDate, ShipDate: TDateTime;
  Items: TDtoInvoiceItemArray;
end;

TDtoDashboardStats = packed record
  CustomerCount, OpenItemsCount: integer;
  OpenItemsAmount: currency;
  DueTodayCount, OverdueCount: integer;
end;

TDtoCustomerSummary = packed record
  CustomerID: longint; CustomerName: string;
  InvoiceCount: integer; TotalRevenue: currency;
  OpenCount: integer; OpenAmount: currency; PaidCount: integer;
end;
```

---

## Phases

### Phase A: Foundation (no behavior change)

| # | Task | Status |
|---|------|--------|
| A.1 | Add new DTO types + dynamic arrays to `src/rgDtoTypes.pas` | done |
| A.2 | Create `src/rgServiceInterfaces.pas` (5 interfaces + RegisterInterfaces) | done |
| A.3 | Create `src/rgConfig.pas` (TRgConfig + JSON load/save) | done |
| A.4 | Add `HttpPort`, `ConfigFileName` constants to `src/rgConst.pas` | done |
| A.5 | Compile, verify no regressions | done |

### Phase B: Server Implementation

| # | Task | Status |
|---|------|--------|
| B.1 | Create `src/rgServiceImplementation.pas` - all 5 `TInjectableObjectRest` classes | planned |
| B.2 | Create `src/rgServer.pas` - `TRgServer` with `ServiceDefine` calls | planned |
| B.3 | Compile server units | planned |

Server implementations: stateless methods, ORM via `Self.Server.Orm`, thread-safe for `sicShared`.
Port logic from existing `rgClient.pas` classes (replace iterator with batch query + array build).

### Phase C: Daemon

| # | Task | Status |
|---|------|--------|
| C.1 | Create `daemon/RechnungDaemon.lpr` + `daemon/RechnungDaemon.lpi` | planned |
| C.2 | Implement `TRgDaemon` (TSynDaemon: Start creates TRgServer + TRestHttpServer, Stop frees) | planned |
| C.3 | Compile + test `--console` mode | planned |

### Phase D: Client Adaptation

| # | Task | Status |
|---|------|--------|
| D.1 | Rewrite `src/rgClient.pas` -> `TRgServiceClient` (local/service mode, ~300 lines) | planned |
| D.2 | Adapt `src/rgMain.pas` (use `RgServices.*`) | planned |
| D.3 | Adapt `src/rgCustomerList.pas` (batch array) | planned |
| D.4 | Adapt `src/rgCustomerEdit.pas` (DTO CRUD) | planned |
| D.5 | Adapt `src/rgInvoiceList.pas` (batch array) | planned |
| D.6 | Adapt `src/rgInvoiceEdit.pas` (local item array + batch save) | planned |
| D.7 | Adapt `src/rgPaymentEntry.pas` | planned |
| D.8 | Adapt `src/rgReportOpenItems.pas`, `rgReportPayments.pas`, `rgReportRevenue.pas`, `rgReportMonthly.pas` | planned |
| D.9 | Update `src/Rechnung.lpr` + `src/Rechnung.lpi` | planned |

Client pattern change:
```pascal
// OLD: FService := TCustomerService.Create; FService.LoadCustomers; while FService.NextCustomer do ...
// NEW: RgServices.CustomerService.ListCustomers(Customers); for i := 0 to High(Customers) do ...
// OLD: FEditService.SetCompany(X); FEditService.Save;
// NEW: Customer.Company := X; RgServices.CustomerService.UpdateCustomer(ID, Customer);
```

### Phase E: Testing + Documentation

| # | Task | Status |
|---|------|--------|
| E.1 | Test local mode (`"Mode": "local"`) | planned |
| E.2 | Test service mode (daemon + GUI) | planned |
| E.3 | Compile daemon on Linux | planned |
| E.4 | Update `10-InvoiceExample/CLAUDE.md` | planned |
| E.5 | Update `docs/IMPLEMENTATION-PLAN.md` | planned |

---

## File Map

### New Files
| File | Lines | Purpose |
|------|-------|---------|
| `src/rgServiceInterfaces.pas` | ~120 | 5 SOA interfaces + RegisterInterfaces |
| `src/rgServiceImplementation.pas` | ~800 | 5 TInjectableObjectRest implementations |
| `src/rgServer.pas` | ~50 | TRgServer + ServiceDefine |
| `src/rgConfig.pas` | ~80 | JSON config |
| `daemon/RechnungDaemon.lpr` | ~100 | TSynDaemon entry |
| `daemon/RechnungDaemon.lpi` | - | Lazarus project |

### Modified Files
| File | Change |
|------|--------|
| `src/rgDtoTypes.pas` | +8 DTO types, +6 dynamic arrays |
| `src/rgConst.pas` | +2 constants |
| `src/rgClient.pas` | Rewrite: 2161->~300 lines |
| `src/rgMain.pas` | Use RgServices accessor |
| `src/rgCustomerList.pas` | Batch array pattern |
| `src/rgCustomerEdit.pas` | DTO CRUD pattern |
| `src/rgInvoiceList.pas` | Batch array pattern |
| `src/rgInvoiceEdit.pas` | Local items + batch save |
| `src/rgPaymentEntry.pas` | RgServices.PaymentService |
| `src/rgReport*.pas` (4 files) | RgServices.ReportService |
| `src/Rechnung.lpr` | +units, config init |
| `src/Rechnung.lpi` | +units |

### Unchanged
`rgData.pas`, `rgType.pas`, `rgAbout.pas`, `rgReportBase.pas`, `rgButtonFrame.pas`, `mdForms.pas`, `mdGrids.pas`, `mdLayout.pas`, `mdDates.pas`, `mdConst.pas`

## Technical Notes

- `sicShared` + stateless methods = thread-safe, no locks needed
- `packed record` with `string` fields: register via mORMot2 RTTI for JSON serialization
- `currency`: native mORMot2 support (Int64 * 10000)
- `TDateTime`: ISO 8601 serialization
- Delphi 7 compatible: no generics, no anonymous methods
- Port: 11111 (per root CLAUDE.md)

## Verify

```bash
${LAZBUILD_PATH} src/Rechnung.lpi
${LAZBUILD_PATH} daemon/RechnungDaemon.lpi
# Local: set "Mode":"local" in rechnung.config.json, run GUI
# Service: ./RechnungDaemon --console, set "Mode":"service", run GUI
```
