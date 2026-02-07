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

All service classes inherit from `TInjectableObjectRest` with stateless methods.
ORM access via `Self.Server.Orm`. Thread-safe for `sicShared`.
Port logic from existing `rgClient.pas` classes (replace iterator with batch query + array build).

#### B.1: Unit skeleton + PaymentService (simplest service first)

| # | Task | Status |
|---|------|--------|
| B.1.1 | Create `src/rgServiceImplementation.pas` with unit header, uses clause, all 5 class declarations (empty), and initialization section | done |
| B.1.2 | Implement `TRgPaymentService.AddPayment` — load order, validate amount, update AmountPaid (source: `rgClient.pas:1131-1189`) | done |
| B.1.3 | Implement `TRgPaymentService.GetInvoiceOpenAmount` — load order, return ItemsTotal - AmountPaid | done |
| B.1.4 | Compile | done |

#### B.2: CustomerService

| # | Task | Status |
|---|------|--------|
| B.2.1 | Implement `TRgCustomerService.ListCustomers` — batch query via `CreateAndFillPrepare`, build `TDtoCustomerDynArray` (source: `rgClient.pas:561-705`) | done |
| B.2.2 | Implement `TRgCustomerService.GetCustomer` — single row fetch, map ORM to DTO including nested contact/address fields | done |
| B.2.3 | Implement `TRgCustomerService.CreateCustomer` — build `TOrmCustomer` + nested contact/address from DTO, `Orm.Add` (source: `rgClient.pas:1498-1600`) | done |
| B.2.4 | Implement `TRgCustomerService.UpdateCustomer` — load existing, update from DTO, `Orm.Update` (source: `rgClient.pas:1600-1700`) | done |
| B.2.5 | Implement `TRgCustomerService.DeleteCustomer` — check for orders, then `Orm.Delete` (source: `rgClient.pas:1700-1756`) | done |
| B.2.6 | Implement `TRgCustomerService.GenerateCustomerNo` — MAX() query + increment (source: `rgClient.pas:1540-1560`) | done |
| B.2.7 | Compile | done |

#### B.3: InvoiceService

| # | Task | Status |
|---|------|--------|
| B.3.1 | Implement `TRgInvoiceService.ListInvoicesForCustomer` — batch query, calculate OpenAmount/Status per row (source: `rgClient.pas:1003-1129`) | done |
| B.3.2 | Implement `TRgInvoiceService.GetInvoice` — load order + items + customer name, flatten to `TDtoInvoiceDetail` (source: `rgClient.pas:1191-1300`) | done |
| B.3.3 | Implement `TRgInvoiceService.CreateInvoice` — build `TOrmCustomerOrder` + `TItemCollection` from DTO items array, calculate totals (source: `rgClient.pas:1300-1400`) | done |
| B.3.4 | Implement `TRgInvoiceService.UpdateInvoice` — load existing, update order + items, recalculate total (source: `rgClient.pas:1400-1494`) | done |
| B.3.5 | Implement `TRgInvoiceService.DeleteInvoice` — simple `Orm.Delete` | done |
| B.3.6 | Implement `TRgInvoiceService.GenerateOrderNo` — MAX() query on OrderNo (source: `rgClient.pas:1320-1340`) | done |
| B.3.7 | Compile | done |

#### B.4: StatisticsService

| # | Task | Status |
|---|------|--------|
| B.4.1 | Implement `TRgStatisticsService.GetDashboardStats` — combined SQL with CASE/COALESCE aggregations (source: `rgClient.pas:783-889`) | done |
| B.4.2 | Implement `TRgStatisticsService.GetCustomerSummary` — customer row + aggregated invoice stats SQL (source: `rgClient.pas:891-1001`) | done |
| B.4.3 | Compile | done |

#### B.5: ReportService

| # | Task | Status |
|---|------|--------|
| B.5.1 | Implement `TRgReportService.GetOpenItemsReport` — multi-table JOIN, date range filter, days overdue calc (source: `rgClient.pas:1790-1879`) | done |
| B.5.2 | Implement `TRgReportService.GetPaymentReceiptsReport` — JOIN with AmountPaid > 0 filter (source: `rgClient.pas:1881-1959`) | done |
| B.5.3 | Implement `TRgReportService.GetCustomerRevenueReport` — GROUP BY customer, year filter (source: `rgClient.pas:1961-2042`) | done |
| B.5.4 | Implement `TRgReportService.GetMonthlyOverviewReport` — loop-based aggregation with TTimeLogBits month extraction (source: `rgClient.pas:2044-2144`) | done |
| B.5.5 | Compile | done |

#### B.6: Server unit

| # | Task | Status |
|---|------|--------|
| B.6.1 | Create `src/rgServer.pas` — `TRgServer` extending `TRestServerDB`, constructor with 5 `ServiceDefine` calls | planned |
| B.6.2 | Full compile of all Phase B units together | planned |

### Phase C: Daemon

| # | Task | Status |
|---|------|--------|
| C.1 | Create `daemon/RechnungDaemon.lpr` + `daemon/RechnungDaemon.lpi` | planned |
| C.2 | Implement `TRgDaemon` (TSynDaemon: Start creates TRgServer + TRestHttpServer, Stop frees) | planned |
| C.3 | Compile + test `--console` mode | planned |

### Phase D: Client Adaptation

Replace old iterator-based service classes with stateless SOA interface calls.
Forms manage local DTOs/arrays; services are stateless request/response.

Pattern change:
```pascal
// OLD: FService := TCustomerService.Create; FService.LoadCustomers; while FService.NextCustomer do ...
// NEW: RgServices.CustomerService.ListCustomers(Customers); for i := 0 to High(Customers) do ...
// OLD: FEditService.SetCompany(X); FEditService.Save;
// NEW: Customer.Company := X; RgServices.CustomerService.UpdateCustomer(ID, Customer);
```

#### D.1: Rewrite rgClient.pas → TRgServiceClient

| # | Task | Status |
|---|------|--------|
| D.1.1 | Create new `rgClient.pas` with `TRgServiceClient` class declaration: 5 service interface properties (`CustomerService`, `InvoiceService`, `PaymentService`, `StatisticsService`, `ReportService`), global `var RgServices` | planned |
| D.1.2 | Implement local mode constructor — create `TRgServer` (embedded), resolve 5 service interfaces via `Services[].Get()` | planned |
| D.1.3 | Implement service mode constructor — create `TRestHttpClientUri`, resolve 5 service interfaces via `ServiceDefine` + `Services[].Get()` | planned |
| D.1.4 | Implement destructor + initialization/finalization (load config, create `RgServices`, free on finalization) | planned |
| D.1.5 | Compile | planned |

#### D.2: Simple form adaptations (payment + lists)

| # | Task | Status |
|---|------|--------|
| D.2.1 | Adapt `rgPaymentEntry.pas` — replace `FPaymentService` with `RgServices.PaymentService` (nearly compatible API, ~10 lines) | planned |
| D.2.2 | Adapt `rgCustomerList.pas` — replace iterator loop (`LoadCustomers`/`NextCustomer`/`GetCustomer`) with `ListCustomers(out Array)` + index loop | planned |
| D.2.3 | Adapt `rgInvoiceList.pas` — replace iterator loop with `ListInvoicesForCustomer(CustomerID, out Array)` + index loop | planned |
| D.2.4 | Compile | planned |

#### D.3: Report forms

| # | Task | Status |
|---|------|--------|
| D.3.1 | Adapt `rgReportOpenItems.pas` — replace `FReportService.LoadOpenItems` iterator with `RgServices.ReportService.GetOpenItemsReport(..., out FItems)` | planned |
| D.3.2 | Adapt `rgReportPayments.pas` — replace iterator with `GetPaymentReceiptsReport(..., out FItems)` | planned |
| D.3.3 | Adapt `rgReportRevenue.pas` — replace iterator with `GetCustomerRevenueReport(..., out FItems)` | planned |
| D.3.4 | Adapt `rgReportMonthly.pas` — replace iterator with `GetMonthlyOverviewReport(..., out FItems, out FTotals)` (note: extra `out ATotals` param) | planned |
| D.3.5 | Compile | planned |

#### D.4: Main form (dashboard stats consolidation)

| # | Task | Status |
|---|------|--------|
| D.4.1 | Adapt `rgMain.pas` — replace `FStatisticsService` (5 separate getters) with single `RgServices.StatisticsService.GetDashboardStats(out AStats)` call, read fields from DTO | planned |
| D.4.2 | Adapt `rgMain.pas` — replace `FCustomerSummaryService` with `RgServices.StatisticsService.GetCustomerSummary(CustomerID, out ASummary)` | planned |
| D.4.3 | Compile | planned |

#### D.5: Customer edit form (moderate — DTO construction)

| # | Task | Status |
|---|------|--------|
| D.5.1 | Adapt `rgCustomerEdit.pas` load path — replace 8 property getters (`GetCustomerNo`, `GetCompany`, etc.) with single `RgServices.CustomerService.GetCustomer(ID, out ACustomer)`, populate form from DTO fields | planned |
| D.5.2 | Adapt `rgCustomerEdit.pas` save path — build `TDtoCustomer` from form fields, call `CreateCustomer(DTO, out NewID)` or `UpdateCustomer(ID, DTO)` | planned |
| D.5.3 | Adapt `rgCustomerEdit.pas` delete + generate — replace `DeleteCustomer` and `GenerateCustomerNo` calls (direct mapping) | planned |
| D.5.4 | Compile | planned |

#### D.6: Invoice edit form (most complex — local item array management)

| # | Task | Status |
|---|------|--------|
| D.6.1 | Adapt `rgInvoiceEdit.pas` load path — add local `FItems: TDtoInvoiceItemArray` field, load via `RgServices.InvoiceService.GetInvoice(ID, out ADetail)`, populate form + `FItems` from DTO | planned |
| D.6.2 | Adapt `rgInvoiceEdit.pas` item operations — rewrite Add/Update/Delete item buttons to modify local `FItems` array directly (insert/update/remove array elements) | planned |
| D.6.3 | Adapt `rgInvoiceEdit.pas` save path — build `TDtoInvoiceSave` from form fields + `FItems` array, call `CreateInvoice` or `UpdateInvoice` with full DTO | planned |
| D.6.4 | Compile | planned |

#### D.7: Project files + final compile

| # | Task | Status |
|---|------|--------|
| D.7.1 | Update `Rechnung.lpr` uses clause — add `rgServer`, `rgServiceImplementation` (needed for local mode registration) | planned |
| D.7.2 | Update `Rechnung.lpi` — add new units to project | planned |
| D.7.3 | Full compile of all Phase D changes together | planned |

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
| `src/rgServiceImplementation.pas` | ~800 | 5 TInjectableObjectRest implementations (built incrementally in B.1-B.5) |
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
