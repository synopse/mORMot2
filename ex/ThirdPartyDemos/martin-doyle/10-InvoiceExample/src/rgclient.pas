{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgClient.pas

  Last modified
    Date : 01.02.2026
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
————————————————————————————————————————————————————————————————————————————
}
unit rgClient;

interface

{$I mormot.defines.inc}

uses
  Classes, SysUtils,
  mormot.core.base, mormot.core.text, mormot.core.unicode, mormot.core.variants,
  mormot.core.interfaces,
  mormot.orm.core, mormot.rest.core, mormot.soa.core,
  mormot.rest.sqlite3, mormot.db.raw.sqlite3.static, mormot.rest.http.client,
  rgData, rgDtoTypes, rgServiceInterfaces, rgServer;

type

  { TRgClient }

  TRgRestClient = class(TRestClientDB)
  private
    FModel: TOrmModel;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
  end;

  { TRgServiceClient }

  TRgServiceClient = class(TObject)
  private
    fModel: TOrmModel;
    fServer: TRgServer;
    fHttpClient: TRestHttpClient;
    fCustomerService: IRgCustomerService;
    fInvoiceService: IRgInvoiceService;
    fPaymentService: IRgPaymentService;
    fStatisticsService: IRgStatisticsService;
    fReportService: IRgReportService;
  public
    constructor CreateLocal;
    constructor CreateService(const AHost, APort: RawUtf8);
    destructor Destroy; override;
    property CustomerService: IRgCustomerService read fCustomerService;
    property InvoiceService: IRgInvoiceService read fInvoiceService;
    property PaymentService: IRgPaymentService read fPaymentService;
    property StatisticsService: IRgStatisticsService read fStatisticsService;
    property ReportService: IRgReportService read fReportService;
  end;

var
  RgServices: TRgServiceClient;

type

  { ICustomerService }

  ICustomerService = interface(IInvokable)
    ['{B9D4DF24-3D17-42FE-BFB1-F81092C0C2E1}']
    procedure LoadCustomers;
    function LoadCustomerByID(ACustomerID: longint): boolean;
    function NextCustomer: boolean;
    function GetCustomerCount(ARefresh: boolean = False): integer;
    function GetCustomer: TDtoCustomer;
    function GetCustomerNo: string;
    function GetCompany: string;
    function GetContactCount: integer;
    function GetContact(AIndex: integer): TDtoContact;
    function GetContactName: string;
  end;

  { ICustomerOrderService }

  ICustomerOrderService = interface(IInvokable)
    ['{BFBC3DBD-9431-E5F7-FD33-2072F271A841}']
    procedure LoadOrders;
    procedure LoadOrdersForCustomer(const ACustomerID: longint);
    procedure LoadOrderDetailsForOrder(const AOrderID: longint);
    function NextOrder: boolean;
    function GetOrderCount(ARefresh: boolean = False): integer;
    function GetOrder: TDtoOrder;
    function GetOrderNo: string;
  end;

  { IStatisticsService }

  IStatisticsService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetCustomerCount: integer;
    function GetOpenItemsCount: integer;
    function GetOpenItemsAmount: currency;
    function GetDueTodayCount: integer;
    function GetOverdueCount: integer;
    procedure Refresh;
  end;

  { ICustomerSummaryService }

  ICustomerSummaryService = interface(IInvokable)
    ['{C2D3E4F5-A6B7-8901-CDEF-234567890ABC}']
    procedure LoadForCustomer(ACustomerID: longint);
    function GetCustomerName: string;
    function GetInvoiceCount: integer;
    function GetTotalRevenue: currency;
    function GetOpenCount: integer;
    function GetOpenAmount: currency;
    function GetPaidCount: integer;
    function IsLoaded: boolean;
    procedure Clear;
  end;

  { IInvoiceService }

  IInvoiceService = interface(IInvokable)
    ['{D3E4F5A6-B7C8-9012-DEF1-34567890ABCD}']
    procedure LoadInvoicesForCustomer(ACustomerID: longint);
    function NextInvoice: boolean;
    function GetInvoiceCount: integer;
    function GetInvoice: TDtoOrder;
    procedure Clear;
    function IsLoaded: boolean;
    function GetCurrentCustomerID: longint;
  end;

  { IPaymentService }

  IPaymentService = interface(IInvokable)
    ['{E4F5A6B7-C8D9-0123-EF12-4567890ABCDE}']
    function AddPayment(AInvoiceID: longint; AAmount: currency;
      ADate: TDateTime): TPaymentResult;
    function GetInvoiceOpenAmount(AInvoiceID: longint): currency;
  end;

  { ICustomerEditService }

  ICustomerEditService = interface(IInvokable)
    ['{A6B7C8D9-E0F1-2345-A678-90ABCDEF1234}']
    function LoadCustomer(ACustomerID: longint): Boolean;
    function CreateNewCustomer: Boolean;
    function GetCustomerNo: string;
    procedure SetCustomerNo(const AValue: string);
    function GetCompany: string;
    procedure SetCompany(const AValue: string);
    function GetPhone: string;
    procedure SetPhone(const AValue: string);
    function GetFax: string;
    procedure SetFax(const AValue: string);
    function GetAddress: string;
    procedure SetAddress(const AValue: string);
    function GetZip: string;
    procedure SetZip(const AValue: string);
    function GetCity: string;
    procedure SetCity(const AValue: string);
    function GetCountry: string;
    procedure SetCountry(const AValue: string);
    function Save: TCustomerEditResult;
    function DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
    function IsNewCustomer: Boolean;
    function GenerateCustomerNo: string;
  end;

  { IInvoiceEditService }

  IInvoiceEditService = interface(IInvokable)
    ['{F5A6B7C8-D9E0-1234-F123-567890ABCDEF}']
    function LoadInvoice(AInvoiceID: longint): Boolean;
    function CreateNewInvoice(ACustomerID: longint): Boolean;
    function GetOrderNo: string;
    procedure SetOrderNo(const AValue: string);
    function GetSaleDate: TDateTime;
    procedure SetSaleDate(AValue: TDateTime);
    function GetShipDate: TDateTime;
    procedure SetShipDate(AValue: TDateTime);
    function GetCustomerID: longint;
    function GetCustomerName: string;
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoInvoiceItem;
    procedure AddItem(const AItem: TDtoInvoiceItem);
    procedure UpdateItem(AIndex: integer; const AItem: TDtoInvoiceItem);
    procedure DeleteItem(AIndex: integer);
    procedure ClearItems;
    function GetItemsTotal: currency;
    function Save: TInvoiceEditResult;
    function IsNewInvoice: Boolean;
    function GenerateOrderNo: string;
  end;

  { IOpenItemsReportService }

  IOpenItemsReportService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure LoadOpenItems(AFromDate, AToDate: TDateTime; AMinAmount: currency);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoOpenItem;
    procedure Clear;
  end;

  { IPaymentReceiptsReportService }

  IPaymentReceiptsReportService = interface(IInvokable)
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    procedure LoadPaymentReceipts(AFromDate, AToDate: TDateTime);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoPaymentReceipt;
    procedure Clear;
  end;

  { ICustomerRevenueReportService }

  ICustomerRevenueReportService = interface(IInvokable)
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    procedure LoadCustomerRevenue(AYear: integer);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoCustomerRevenue;
    procedure Clear;
  end;

  { IMonthlyOverviewReportService }

  IMonthlyOverviewReportService = interface(IInvokable)
    ['{D4E5F6A7-B8C9-0123-DEFA-234567890123}']
    procedure LoadMonthlyOverview(AYear: integer);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoMonthlyOverview;
    function GetTotals: TDtoMonthlyOverview;
    procedure Clear;
  end;


  { TCustomerService }

  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FCurrentCustomer: TDtoCustomer;
    FCurrentContact: TDtoContact;
    FCurrentIndex: integer;
    FCustomers: TOrmCustomer;
    FRestOrm: IRestOrm;
    procedure LoadCustomer(AIndex: integer);
    procedure LoadContact(AIndex: integer);
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    destructor Destroy; override;
    procedure LoadCustomers;
    function LoadCustomerByID(ACustomerID: longint): boolean;
    function NextCustomer: boolean;
    function GetCustomerCount(ARefresh: boolean = False): integer;
    function GetCustomer: TDtoCustomer;
    function GetCustomerNo: string;
    function GetCompany: string;
    function GetContactCount: integer;
    function GetContact(AIndex: integer): TDtoContact;
    function GetContactName: string;
  end;

  { TCustomerOrderService }

  TCustomerOrderService = class(TInterfacedObject, ICustomerOrderService)
  private
    FCurrentOrder: TDtoOrder;
    FCurrentIndex: integer;
    FOrders: TOrmCustomerOrder;
    FRestOrm: IRestOrm;
    procedure LoadOrder(AIndex: integer);
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    destructor Destroy; override;
    procedure LoadOrders;
    procedure LoadOrdersForCustomer(const ACustomerID: longint);
    procedure LoadOrderDetailsForOrder(const AOrderID: longint);
    function NextOrder: boolean;
    function GetOrderCount(ARefresh: boolean = False): integer;
    function GetOrder: TDtoOrder;
    function GetOrderNo: string;
  end;

  { TStatisticsService }

  TStatisticsService = class(TInterfacedObject, IStatisticsService)
  private
    FRestOrm: IRestOrm;
    FCustomerCount: integer;
    FOpenItemsCount: integer;
    FOpenItemsAmount: currency;
    FDueTodayCount: integer;
    FOverdueCount: integer;
    FLoaded: boolean;
    procedure LoadStatistics;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    function GetCustomerCount: integer;
    function GetOpenItemsCount: integer;
    function GetOpenItemsAmount: currency;
    function GetDueTodayCount: integer;
    function GetOverdueCount: integer;
    procedure Refresh;
  end;

  { TCustomerSummaryService }

  TCustomerSummaryService = class(TInterfacedObject, ICustomerSummaryService)
  private
    FRestOrm: IRestOrm;
    FCustomerID: longint;
    FCustomerName: string;
    FInvoiceCount: integer;
    FTotalRevenue: currency;
    FOpenCount: integer;
    FOpenAmount: currency;
    FPaidCount: integer;
    FLoaded: boolean;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    procedure LoadForCustomer(ACustomerID: longint);
    function GetCustomerName: string;
    function GetInvoiceCount: integer;
    function GetTotalRevenue: currency;
    function GetOpenCount: integer;
    function GetOpenAmount: currency;
    function GetPaidCount: integer;
    function IsLoaded: boolean;
    procedure Clear;
  end;

  { TInvoiceService }

  TInvoiceService = class(TInterfacedObject, IInvoiceService)
  private
    FRestOrm: IRestOrm;
    FOrders: TOrmCustomerOrder;
    FCurrentOrder: TDtoOrder;
    FCurrentIndex: integer;
    FCustomerID: longint;
    FLoaded: boolean;
    procedure LoadOrder(AIndex: integer);
    function DetermineStatus(AOpenAmount: currency; AShipDate: TDateTime): TInvoiceStatus;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    destructor Destroy; override;
    procedure LoadInvoicesForCustomer(ACustomerID: longint);
    function NextInvoice: boolean;
    function GetInvoiceCount: integer;
    function GetInvoice: TDtoOrder;
    procedure Clear;
    function IsLoaded: boolean;
    function GetCurrentCustomerID: longint;
  end;

  { TPaymentService }

  TPaymentService = class(TInterfacedObject, IPaymentService)
  private
    FRestOrm: IRestOrm;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    function AddPayment(AInvoiceID: longint; AAmount: currency;
      ADate: TDateTime): TPaymentResult;
    function GetInvoiceOpenAmount(AInvoiceID: longint): currency;
  end;

  { TInvoiceEditService }

  TInvoiceEditService = class(TInterfacedObject, IInvoiceEditService)
  private
    FRestOrm: IRestOrm;
    FOrder: TOrmCustomerOrder;
    FCustomerID: longint;
    FCustomerName: string;
    FItems: TDtoInvoiceItemArray;
    FIsNew: Boolean;
    procedure LoadItemsFromOrder;
    procedure SaveItemsToOrder;
    function CalculateItemAmount(const AItem: TDtoInvoiceItem): currency;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    destructor Destroy; override;
    function LoadInvoice(AInvoiceID: longint): Boolean;
    function CreateNewInvoice(ACustomerID: longint): Boolean;
    function GetOrderNo: string;
    procedure SetOrderNo(const AValue: string);
    function GetSaleDate: TDateTime;
    procedure SetSaleDate(AValue: TDateTime);
    function GetShipDate: TDateTime;
    procedure SetShipDate(AValue: TDateTime);
    function GetCustomerID: longint;
    function GetCustomerName: string;
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoInvoiceItem;
    procedure AddItem(const AItem: TDtoInvoiceItem);
    procedure UpdateItem(AIndex: integer; const AItem: TDtoInvoiceItem);
    procedure DeleteItem(AIndex: integer);
    procedure ClearItems;
    function GetItemsTotal: currency;
    function Save: TInvoiceEditResult;
    function IsNewInvoice: Boolean;
    function GenerateOrderNo: string;
  end;

  { TCustomerEditService }

  TCustomerEditService = class(TInterfacedObject, ICustomerEditService)
  private
    FRestOrm: IRestOrm;
    FCustomer: TOrmCustomer;
    FIsNew: Boolean;
    function EnsureContact: TPersonItem;
    function EnsureAddress: TAddressItem;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    destructor Destroy; override;
    function LoadCustomer(ACustomerID: longint): Boolean;
    function CreateNewCustomer: Boolean;
    function GetCustomerNo: string;
    procedure SetCustomerNo(const AValue: string);
    function GetCompany: string;
    procedure SetCompany(const AValue: string);
    function GetPhone: string;
    procedure SetPhone(const AValue: string);
    function GetFax: string;
    procedure SetFax(const AValue: string);
    function GetAddress: string;
    procedure SetAddress(const AValue: string);
    function GetZip: string;
    procedure SetZip(const AValue: string);
    function GetCity: string;
    procedure SetCity(const AValue: string);
    function GetCountry: string;
    procedure SetCountry(const AValue: string);
    function Save: TCustomerEditResult;
    function DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
    function IsNewCustomer: Boolean;
    function GenerateCustomerNo: string;
  end;

  { TOpenItemsReportService }

  TOpenItemsReportService = class(TInterfacedObject, IOpenItemsReportService)
  private
    FRestOrm: IRestOrm;
    FItems: TDtoOpenItemArray;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    procedure LoadOpenItems(AFromDate, AToDate: TDateTime; AMinAmount: currency);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoOpenItem;
    procedure Clear;
  end;

  { TPaymentReceiptsReportService }

  TPaymentReceiptsReportService = class(TInterfacedObject, IPaymentReceiptsReportService)
  private
    FRestOrm: IRestOrm;
    FItems: TDtoPaymentReceiptArray;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    procedure LoadPaymentReceipts(AFromDate, AToDate: TDateTime);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoPaymentReceipt;
    procedure Clear;
  end;

  { TCustomerRevenueReportService }

  TCustomerRevenueReportService = class(TInterfacedObject, ICustomerRevenueReportService)
  private
    FRestOrm: IRestOrm;
    FItems: TDtoCustomerRevenueArray;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    procedure LoadCustomerRevenue(AYear: integer);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoCustomerRevenue;
    procedure Clear;
  end;

  { TMonthlyOverviewReportService }

  TMonthlyOverviewReportService = class(TInterfacedObject, IMonthlyOverviewReportService)
  private
    FRestOrm: IRestOrm;
    FItems: TDtoMonthlyOverviewArray;
    FTotals: TDtoMonthlyOverview;
  public
    constructor Create; overload;
    constructor Create(ARestOrm: IRestOrm); overload;
    procedure LoadMonthlyOverview(AYear: integer);
    function GetItemCount: integer;
    function GetItem(AIndex: integer): TDtoMonthlyOverview;
    function GetTotals: TDtoMonthlyOverview;
    procedure Clear;
  end;


implementation

uses
  mormot.core.datetime,
  rgConst,
  rgConfig;

var
  RgRestClient: TRgRestClient;


{
******************************* TRgClient *******************************
}

constructor TRgRestClient.Create;
begin
  FModel := CreateModel;
  inherited Create(FModel, nil, DataFile,
    TRestServerDB, False, '');
  Server.CreateMissingTables;
end;

destructor TRgRestClient.Destroy;
begin
  inherited Destroy;
  FModel.Free;
end;

{ TRgServiceClient }

constructor TRgServiceClient.CreateLocal;
begin
  inherited Create;
  fModel := CreateModel;
  fServer := TRgServer.Create(fModel, DataFile);
  fServer.Services.Resolve(TypeInfo(IRgCustomerService), fCustomerService);
  fServer.Services.Resolve(TypeInfo(IRgInvoiceService), fInvoiceService);
  fServer.Services.Resolve(TypeInfo(IRgPaymentService), fPaymentService);
  fServer.Services.Resolve(TypeInfo(IRgStatisticsService), fStatisticsService);
  fServer.Services.Resolve(TypeInfo(IRgReportService), fReportService);
end;

constructor TRgServiceClient.CreateService(const AHost, APort: RawUtf8);
begin
  inherited Create;
  fModel := CreateModel;
  fHttpClient := TRestHttpClient.Create(AHost, APort, fModel);
  fHttpClient.ServiceDefine([IRgCustomerService, IRgInvoiceService,
    IRgPaymentService, IRgStatisticsService, IRgReportService], sicShared);
  fHttpClient.Services.Resolve(TypeInfo(IRgCustomerService), fCustomerService);
  fHttpClient.Services.Resolve(TypeInfo(IRgInvoiceService), fInvoiceService);
  fHttpClient.Services.Resolve(TypeInfo(IRgPaymentService), fPaymentService);
  fHttpClient.Services.Resolve(TypeInfo(IRgStatisticsService), fStatisticsService);
  fHttpClient.Services.Resolve(TypeInfo(IRgReportService), fReportService);
end;

destructor TRgServiceClient.Destroy;
begin
  fCustomerService := nil;
  fInvoiceService := nil;
  fPaymentService := nil;
  fStatisticsService := nil;
  fReportService := nil;
  FreeAndNil(fHttpClient);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited Destroy;
end;

{
******************************* TCustomerService *******************************
}
constructor TCustomerService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FCurrentIndex := -1;
end;

constructor TCustomerService.Create;
begin
  Create(RgRestClient.Orm);
end;

destructor TCustomerService.Destroy;
begin
  if FCustomers <> nil then
    FreeAndNil(FCustomers);
  inherited Destroy;
end;

procedure TCustomerService.LoadCustomer(AIndex: integer);
var
  Contact: TPersonItem;
  Address: TAddressItem;
  Index: integer;
begin
  if AIndex = -1 then
    Index := FCustomers.FillCurrentRow
  else
    Index := AIndex;
  if Index <> FCurrentIndex then
  begin
    Finalize(FCurrentCustomer);
    FillChar(FCurrentCustomer, SizeOf(FCurrentCustomer), 0);
    FCustomers.FillRow(AIndex);
    FCurrentCustomer.CustomerID := FCustomers.ID;
    FCurrentCustomer.CustomerNo := Utf8ToString(FCustomers.CustomerNo);
    FCurrentCustomer.Company := Utf8ToString(FCustomers.Company);
    if FCustomers.Contacts.Count > 0 then
    begin
      Contact := FCustomers.Contacts[0];
      FCurrentCustomer.Phone := Utf8ToString(Contact.Phones[0]);
      FCurrentCustomer.Fax := Utf8ToString(Contact.Phones[1]);
      if Contact.Addresses.Count > 0 then
      begin
        Address := Contact.Addresses[0];
        FCurrentCustomer.Address := Utf8ToString(Address.Street1);
        FCurrentCustomer.Zip := Utf8ToString(Address.Code);
        FCurrentCustomer.City := Utf8ToString(Address.City);
        FCurrentCustomer.Country := Utf8ToString(Address.Country);
      end;
    end;
    FCurrentIndex := Index;
  end;
end;

procedure TCustomerService.LoadContact(AIndex: integer);
var
  Contact: TPersonItem;
  Address: TAddressItem;
  Index: integer;
begin
  Finalize(FCurrentCustomer);
  FillChar(FCurrentCustomer, SizeOf(FCurrentCustomer), 0);
  if FCustomers.Contacts.Count > 1 then
  begin
    if (AIndex <= 0) or (AIndex >= FCustomers.Contacts.Count) then
      Index := 1
    else
      Index := AIndex;
    Contact := FCustomers.Contacts[Index];
    FCurrentContact.FirstName := Utf8ToString(Contact.FirstName);
    FCurrentContact.MiddleName := Utf8ToString(Contact.MiddleName);
    FCurrentContact.LastName := Utf8ToString(Contact.LastName);

    FCurrentContact.Phone := Utf8ToString(Contact.Phones[0]);
    FCurrentContact.Fax := Utf8ToString(Contact.Phones[1]);
    if Contact.Addresses.Count > 0 then
    begin
      Address := Contact.Addresses[0];
      FCurrentContact.Address := Utf8ToString(Address.Street1);
      FCurrentContact.Zip := Utf8ToString(Address.Code);
      FCurrentContact.City := Utf8ToString(Address.City);
      FCurrentContact.Country := Utf8ToString(Address.Country);
    end
    else
    begin
      Contact := FCustomers.Contacts[0];
      if Contact.Addresses.Count > 0 then
      begin
        Address := Contact.Addresses[0];
        FCurrentContact.Address := Utf8ToString(Address.Street1);
        FCurrentContact.Zip := Utf8ToString(Address.Code);
        FCurrentContact.City := Utf8ToString(Address.City);
        FCurrentContact.Country := Utf8ToString(Address.Country);
      end;
    end;
  end;
end;

procedure TCustomerService.LoadCustomers;
begin
  if FCustomers <> nil then
    FreeAndNil(FCustomers);
  FCustomers := TOrmCustomer.CreateAndFillPrepare(FRestOrm, '',
    'ID, CustomerNo, Company, Contacts');
  FCustomers.FillTable.SortFields(2);
end;

function TCustomerService.LoadCustomerByID(ACustomerID: longint): boolean;
begin
  if FCustomers <> nil then
    FreeAndNil(FCustomers);
  FCustomers := TOrmCustomer.CreateAndFillPrepare(FRestOrm,
    'ID=?', [ACustomerID], 'ID, CustomerNo, Company, Contacts');
  Result := FCustomers.FillOne;
  if Result then
  begin
    LoadCustomer(-1);
  end;
end;

function TCustomerService.NextCustomer: boolean;
begin
  Result := FCustomers.FillOne;
end;

function TCustomerService.GetCustomerCount(ARefresh: boolean = False): integer;
begin
  if ARefresh or (FCustomers = nil) then LoadCustomers;
  Result := FCustomers.FillTable.RowCount;
end;

function TCustomerService.GetCustomer: TDtoCustomer;
begin
  LoadCustomer(-1);
  Result := FCurrentCustomer;
end;

function TCustomerService.GetCustomerNo: string;
begin
  LoadCustomer(-1);
  Result := Utf8ToString(FCustomers.CustomerNo);
end;

function TCustomerService.GetCompany: string;
begin
  Result := Utf8ToString(FCustomers.Company);
end;

function TCustomerService.GetContactCount: integer;
begin
  Result := FCustomers.Contacts.Count - 1;
end;

function TCustomerService.GetContact(AIndex: integer): TDtoContact;
begin
  LoadContact(AIndex);
  Result := FCurrentContact;
end;

function TCustomerService.GetContactName: string;
begin
  Result := FCurrentContact.LastName + ', ' + FCurrentContact.FirstName;
end;

{ TCustomerOrderService }

procedure TCustomerOrderService.LoadOrder(AIndex: integer);
var
  Index: integer;
begin
  if AIndex = -1 then
    Index := FOrders.FillCurrentRow
  else
    Index := AIndex;
  Finalize(FCurrentOrder);
  FillChar(FCurrentOrder, SizeOf(FCurrentOrder), 0);
  FOrders.FillRow(AIndex);
  FCurrentOrder.OrderID := FOrders.ID;
  FCurrentOrder.OrderNo := Utf8ToString(FOrders.OrderNo);
  FCurrentOrder.SaleDate := TimeLogToDateTime(FOrders.SaleDate);
  FCurrentOrder.ItemsTotal := FOrders.ItemsTotal;
  FCurrentIndex := Index;
end;

constructor TCustomerOrderService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FCurrentIndex := -1;
end;

constructor TCustomerOrderService.Create;
begin
  Create(RgRestClient.Orm);
end;

destructor TCustomerOrderService.Destroy;
begin
  if FOrders <> nil then
    FreeAndNil(FOrders);
  inherited Destroy;
end;

procedure TCustomerOrderService.LoadOrders;
begin
  if FOrders <> nil then
    FreeAndNil(FOrders);
  FOrders := TOrmCustomerOrder.CreateAndFillPrepare(FRestOrm, '',
    'ID, OrderNo, Customer, ItemsTotal');
  FOrders.FillTable.SortFields(2);
end;

procedure TCustomerOrderService.LoadOrdersForCustomer(const ACustomerID: longint);
begin
  if FOrders <> nil then
    FreeAndNil(FOrders);
  FOrders := TOrmCustomerOrder.CreateAndFillPrepare(FRestOrm,
    'Customer=:(' + IntToStr(ACustomerID) + '):',
    'ID, OrderNo, SaleDate, Customer, Items, ItemsTotal');
  FOrders.FillTable.SortFields(2);
end;

procedure TCustomerOrderService.LoadOrderDetailsForOrder(const AOrderID: longint);
begin

end;

function TCustomerOrderService.NextOrder: boolean;
begin
  Result := FOrders.FillOne;
end;

function TCustomerOrderService.GetOrderCount(ARefresh: boolean): integer;
begin
  if ARefresh or (FOrders = nil) then LoadOrders;
  Result := FOrders.FillTable.RowCount;
end;

function TCustomerOrderService.GetOrder: TDtoOrder;
begin
  LoadOrder(-1);
  Result := FCurrentOrder;
end;

function TCustomerOrderService.GetOrderNo: string;
begin
  LoadOrder(-1);
  Result := Utf8ToString(FOrders.OrderNo);
end;

{ TStatisticsService }

constructor TStatisticsService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FLoaded := False;
end;

constructor TStatisticsService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TStatisticsService.LoadStatistics;
var
  Today: Int64;
  SQL: RawUtf8;
  Table: TOrmTable;
begin
  // Customer count (separate table)
  FCustomerCount := FRestOrm.TableRowCount(TOrmCustomer);

  // Initialize defaults
  FOpenItemsCount := 0;
  FOpenItemsAmount := 0;
  FDueTodayCount := 0;
  FOverdueCount := 0;

  // Combined query for all order statistics (4 queries -> 1)
  Today := TimeLogFromDateTime(Date);
  SQL := FormatUtf8(
    'SELECT ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal THEN 1 ELSE 0 END), ' +
    'COALESCE(SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal ' +
      'THEN ItemsTotal - COALESCE(AmountPaid, 0) ELSE 0 END), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal AND ShipDate = % THEN 1 ELSE 0 END), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal AND ShipDate < % AND ShipDate > 0 THEN 1 ELSE 0 END) ' +
    'FROM CustomerOrder', [Today, Today]);

  Table := FRestOrm.ExecuteList([TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    if Table.RowCount > 0 then
    begin
      FOpenItemsCount := Table.GetAsInteger(1, 0);
      FOpenItemsAmount := Table.GetAsCurrency(1, 1);
      FDueTodayCount := Table.GetAsInteger(1, 2);
      FOverdueCount := Table.GetAsInteger(1, 3);
    end;
  finally
    Table.Free;
  end;

  FLoaded := True;
end;

function TStatisticsService.GetCustomerCount: integer;
begin
  if not FLoaded then
    LoadStatistics;
  Result := FCustomerCount;
end;

function TStatisticsService.GetOpenItemsCount: integer;
begin
  if not FLoaded then
    LoadStatistics;
  Result := FOpenItemsCount;
end;

function TStatisticsService.GetOpenItemsAmount: currency;
begin
  if not FLoaded then
    LoadStatistics;
  Result := FOpenItemsAmount;
end;

function TStatisticsService.GetDueTodayCount: integer;
begin
  if not FLoaded then
    LoadStatistics;
  Result := FDueTodayCount;
end;

function TStatisticsService.GetOverdueCount: integer;
begin
  if not FLoaded then
    LoadStatistics;
  Result := FOverdueCount;
end;

procedure TStatisticsService.Refresh;
begin
  FLoaded := False;
  LoadStatistics;
end;

{ TCustomerSummaryService }

constructor TCustomerSummaryService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FLoaded := False;
  FCustomerID := 0;
end;

constructor TCustomerSummaryService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TCustomerSummaryService.LoadForCustomer(ACustomerID: longint);
var
  Customer: TOrmCustomer;
  SQL: RawUtf8;
  Table: TOrmTable;
begin
  Clear;

  if ACustomerID <= 0 then
    Exit;

  FCustomerID := ACustomerID;

  // Load customer name (separate table)
  Customer := TOrmCustomer.Create(FRestOrm, ACustomerID);
  try
    FCustomerName := Utf8ToString(Customer.Company);
  finally
    Customer.Free;
  end;

  // Combined query for all invoice statistics (5 queries -> 1)
  SQL := FormatUtf8(
    'SELECT ' +
    'COUNT(*), ' +
    'COALESCE(SUM(ItemsTotal), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal THEN 1 ELSE 0 END), ' +
    'COALESCE(SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal ' +
      'THEN ItemsTotal - COALESCE(AmountPaid, 0) ELSE 0 END), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) >= ItemsTotal THEN 1 ELSE 0 END) ' +
    'FROM CustomerOrder WHERE Customer = %', [ACustomerID]);

  Table := FRestOrm.ExecuteList([TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    if Table.RowCount > 0 then
    begin
      FInvoiceCount := Table.GetAsInteger(1, 0);
      FTotalRevenue := Table.GetAsCurrency(1, 1);
      FOpenCount := Table.GetAsInteger(1, 2);
      FOpenAmount := Table.GetAsCurrency(1, 3);
      FPaidCount := Table.GetAsInteger(1, 4);
    end;
  finally
    Table.Free;
  end;

  FLoaded := True;
end;

function TCustomerSummaryService.GetCustomerName: string;
begin
  Result := FCustomerName;
end;

function TCustomerSummaryService.GetInvoiceCount: integer;
begin
  Result := FInvoiceCount;
end;

function TCustomerSummaryService.GetTotalRevenue: currency;
begin
  Result := FTotalRevenue;
end;

function TCustomerSummaryService.GetOpenCount: integer;
begin
  Result := FOpenCount;
end;

function TCustomerSummaryService.GetOpenAmount: currency;
begin
  Result := FOpenAmount;
end;

function TCustomerSummaryService.GetPaidCount: integer;
begin
  Result := FPaidCount;
end;

function TCustomerSummaryService.IsLoaded: boolean;
begin
  Result := FLoaded;
end;

procedure TCustomerSummaryService.Clear;
begin
  FCustomerID := 0;
  FCustomerName := '';
  FInvoiceCount := 0;
  FTotalRevenue := 0;
  FOpenCount := 0;
  FOpenAmount := 0;
  FPaidCount := 0;
  FLoaded := False;
end;

{ TInvoiceService }

constructor TInvoiceService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FCurrentIndex := -1;
  FCustomerID := 0;
  FLoaded := False;
end;

constructor TInvoiceService.Create;
begin
  Create(RgRestClient.Orm);
end;

destructor TInvoiceService.Destroy;
begin
  if FOrders <> nil then
    FreeAndNil(FOrders);
  inherited Destroy;
end;

function TInvoiceService.DetermineStatus(AOpenAmount: currency; AShipDate: TDateTime): TInvoiceStatus;
begin
  if AOpenAmount <= 0 then
    Result := isPaid
  else if (AShipDate > 0) and (AShipDate < Date) then
    Result := isOverdue
  else
    Result := isOpen;
end;

procedure TInvoiceService.LoadOrder(AIndex: integer);
var
  Index: integer;
  ShipDateTime: TDateTime;
begin
  if AIndex = -1 then
    Index := FOrders.FillCurrentRow
  else
    Index := AIndex;

  Finalize(FCurrentOrder);
  FillChar(FCurrentOrder, SizeOf(FCurrentOrder), 0);

  FOrders.FillRow(AIndex);
  FCurrentOrder.OrderID := FOrders.ID;
  FCurrentOrder.OrderNo := Utf8ToString(FOrders.OrderNo);
  FCurrentOrder.SaleDate := TimeLogToDateTime(FOrders.SaleDate);

  if FOrders.ShipDate > 0 then
    ShipDateTime := TimeLogToDateTime(FOrders.ShipDate)
  else
    ShipDateTime := 0;
  FCurrentOrder.ShipDate := ShipDateTime;

  FCurrentOrder.ItemsTotal := FOrders.ItemsTotal;
  FCurrentOrder.AmountPaid := FOrders.AmountPaid;
  FCurrentOrder.OpenAmount := FOrders.ItemsTotal - FOrders.AmountPaid;
  FCurrentOrder.Status := DetermineStatus(FCurrentOrder.OpenAmount, ShipDateTime);

  FCurrentIndex := Index;
end;

procedure TInvoiceService.LoadInvoicesForCustomer(ACustomerID: longint);
begin
  Clear;

  if ACustomerID <= 0 then
    Exit;

  FCustomerID := ACustomerID;

  if FOrders <> nil then
    FreeAndNil(FOrders);

  FOrders := TOrmCustomerOrder.CreateAndFillPrepare(FRestOrm,
    'Customer=?', [ACustomerID],
    'ID, OrderNo, SaleDate, ShipDate, ItemsTotal, AmountPaid');

  // Sort by SaleDate descending (newest first)
  FOrders.FillTable.SortFields(2, False);

  FLoaded := True;
end;

function TInvoiceService.NextInvoice: boolean;
begin
  Result := False;
  if FOrders <> nil then
    Result := FOrders.FillOne;
end;

function TInvoiceService.GetInvoiceCount: integer;
begin
  Result := 0;
  if FOrders <> nil then
    Result := FOrders.FillTable.RowCount;
end;

function TInvoiceService.GetInvoice: TDtoOrder;
begin
  LoadOrder(-1);
  Result := FCurrentOrder;
end;

procedure TInvoiceService.Clear;
begin
  if FOrders <> nil then
    FreeAndNil(FOrders);
  FCustomerID := 0;
  FCurrentIndex := -1;
  FLoaded := False;
  Finalize(FCurrentOrder);
  FillChar(FCurrentOrder, SizeOf(FCurrentOrder), 0);
end;

function TInvoiceService.IsLoaded: boolean;
begin
  Result := FLoaded;
end;

function TInvoiceService.GetCurrentCustomerID: longint;
begin
  Result := FCustomerID;
end;

{ TPaymentService }

constructor TPaymentService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
end;

constructor TPaymentService.Create;
begin
  Create(RgRestClient.Orm);
end;

function TPaymentService.AddPayment(AInvoiceID: longint; AAmount: currency;
  ADate: TDateTime): TPaymentResult;
var
  Order: TOrmCustomerOrder;
  NewAmountPaid: currency;
begin
  Result := prDatabaseError;

  if AAmount <= 0 then
  begin
    Result := prInvalidAmount;
    Exit;
  end;

  Order := TOrmCustomerOrder.Create(FRestOrm, AInvoiceID);
  try
    if Order.ID = 0 then
    begin
      Result := prInvoiceNotFound;
      Exit;
    end;

    NewAmountPaid := Order.AmountPaid + AAmount;
    Order.AmountPaid := NewAmountPaid;

    if FRestOrm.Update(Order, 'AmountPaid') then
      Result := prSuccess;
  finally
    Order.Free;
  end;
end;

function TPaymentService.GetInvoiceOpenAmount(AInvoiceID: longint): currency;
var
  Order: TOrmCustomerOrder;
begin
  Result := 0;

  Order := TOrmCustomerOrder.Create(FRestOrm, AInvoiceID);
  try
    if Order.ID > 0 then
      Result := Order.ItemsTotal - Order.AmountPaid;
  finally
    Order.Free;
  end;
end;

{ TInvoiceEditService }

constructor TInvoiceEditService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FOrder := nil;
  FCustomerID := 0;
  FCustomerName := '';
  SetLength(FItems, 0);
  FIsNew := False;
end;

constructor TInvoiceEditService.Create;
begin
  Create(RgRestClient.Orm);
end;

destructor TInvoiceEditService.Destroy;
begin
  if FOrder <> nil then
    FreeAndNil(FOrder);
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TInvoiceEditService.CalculateItemAmount(const AItem: TDtoInvoiceItem): currency;
begin
  Result := AItem.ListPrice * AItem.Quantity;
  if AItem.Discount > 0 then
    Result := Result * (100 - AItem.Discount) / 100;
end;

procedure TInvoiceEditService.LoadItemsFromOrder;
var
  i: integer;
  Item: TItem;
  DtoItem: TDtoInvoiceItem;
begin
  SetLength(FItems, 0);
  if (FOrder = nil) or (FOrder.Items = nil) then
    Exit;

  SetLength(FItems, FOrder.Items.Count);
  for i := 0 to FOrder.Items.Count - 1 do
  begin
    Item := FOrder.Items[i];
    Finalize(DtoItem);
    FillChar(DtoItem, SizeOf(DtoItem), 0);
    DtoItem.Position := Item.Position;
    DtoItem.PartNo := Utf8ToString(Item.PartNo);
    DtoItem.Description := Utf8ToString(Item.Description);
    DtoItem.Quantity := Item.Quantity;
    DtoItem.ListPrice := Item.ListPrice;
    DtoItem.Discount := Item.Discount;
    DtoItem.Amount := CalculateItemAmount(DtoItem);
    FItems[i] := DtoItem;
  end;
end;

procedure TInvoiceEditService.SaveItemsToOrder;
var
  i: integer;
  Item: TItem;
begin
  if FOrder = nil then
    Exit;

  FOrder.Items.Clear;
  for i := 0 to Length(FItems) - 1 do
  begin
    Item := TItem(FOrder.Items.Add);
    Item.Position := i + 1;
    Item.PartNo := StringToUtf8(FItems[i].PartNo);
    Item.Description := StringToUtf8(FItems[i].Description);
    Item.Quantity := FItems[i].Quantity;
    Item.ListPrice := FItems[i].ListPrice;
    Item.Discount := FItems[i].Discount;
  end;
end;

function TInvoiceEditService.LoadInvoice(AInvoiceID: longint): Boolean;
begin
  Result := False;

  if FOrder <> nil then
    FreeAndNil(FOrder);

  // Use CreateJoined to load Order with Customer in one query
  FOrder := TOrmCustomerOrder.CreateJoined(FRestOrm, AInvoiceID);
  if FOrder.ID = 0 then
  begin
    FreeAndNil(FOrder);
    Exit;
  end;

  FIsNew := False;
  FCustomerID := FOrder.Customer.ID;
  FCustomerName := Utf8ToString(FOrder.Customer.Company);

  LoadItemsFromOrder;
  Result := True;
end;

function TInvoiceEditService.CreateNewInvoice(ACustomerID: longint): Boolean;
var
  Customer: TOrmCustomer;
begin
  Result := False;

  if ACustomerID <= 0 then
    Exit;

  Customer := TOrmCustomer.Create(FRestOrm, ACustomerID);
  try
    if Customer.ID = 0 then
      Exit;

    FCustomerID := ACustomerID;
    FCustomerName := Utf8ToString(Customer.Company);

    if FOrder <> nil then
      FreeAndNil(FOrder);

    FOrder := TOrmCustomerOrder.Create;
    FOrder.Customer := Customer.AsTOrm;  // FK assignment while Customer in scope
    FOrder.OrderNo := StringToUtf8(GenerateOrderNo);
    FOrder.SaleDate := TimeLogFromDateTime(Date);
    FOrder.ShipDate := TimeLogFromDateTime(Date + 14);
    FOrder.ItemsTotal := 0;
    FOrder.AmountPaid := 0;

    SetLength(FItems, 0);
    FIsNew := True;
    Result := True;
  finally
    Customer.Free;
  end;
end;

function TInvoiceEditService.GetOrderNo: string;
begin
  Result := '';
  if FOrder <> nil then
    Result := Utf8ToString(FOrder.OrderNo);
end;

procedure TInvoiceEditService.SetOrderNo(const AValue: string);
begin
  if FOrder <> nil then
    FOrder.OrderNo := StringToUtf8(AValue);
end;

function TInvoiceEditService.GetSaleDate: TDateTime;
begin
  Result := 0;
  if (FOrder <> nil) and (FOrder.SaleDate > 0) then
    Result := TimeLogToDateTime(FOrder.SaleDate);
end;

procedure TInvoiceEditService.SetSaleDate(AValue: TDateTime);
begin
  if FOrder <> nil then
    FOrder.SaleDate := TimeLogFromDateTime(AValue);
end;

function TInvoiceEditService.GetShipDate: TDateTime;
begin
  Result := 0;
  if (FOrder <> nil) and (FOrder.ShipDate > 0) then
    Result := TimeLogToDateTime(FOrder.ShipDate);
end;

procedure TInvoiceEditService.SetShipDate(AValue: TDateTime);
begin
  if FOrder <> nil then
    FOrder.ShipDate := TimeLogFromDateTime(AValue);
end;

function TInvoiceEditService.GetCustomerID: longint;
begin
  Result := FCustomerID;
end;

function TInvoiceEditService.GetCustomerName: string;
begin
  Result := FCustomerName;
end;

function TInvoiceEditService.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TInvoiceEditService.GetItem(AIndex: integer): TDtoInvoiceItem;
begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
    Result := FItems[AIndex];
end;

procedure TInvoiceEditService.AddItem(const AItem: TDtoInvoiceItem);
var
  Len: integer;
  NewItem: TDtoInvoiceItem;
begin
  Len := Length(FItems);
  SetLength(FItems, Len + 1);
  NewItem := AItem;
  NewItem.Position := Len + 1;
  NewItem.Amount := CalculateItemAmount(NewItem);
  FItems[Len] := NewItem;
end;

procedure TInvoiceEditService.UpdateItem(AIndex: integer; const AItem: TDtoInvoiceItem);
var
  UpdatedItem: TDtoInvoiceItem;
begin
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
  begin
    UpdatedItem := AItem;
    UpdatedItem.Position := AIndex + 1;
    UpdatedItem.Amount := CalculateItemAmount(UpdatedItem);
    FItems[AIndex] := UpdatedItem;
  end;
end;

procedure TInvoiceEditService.DeleteItem(AIndex: integer);
var
  i: integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
  begin
    for i := AIndex to Length(FItems) - 2 do
    begin
      FItems[i] := FItems[i + 1];
      FItems[i].Position := i + 1;
    end;
    SetLength(FItems, Length(FItems) - 1);
  end;
end;

procedure TInvoiceEditService.ClearItems;
begin
  SetLength(FItems, 0);
end;

function TInvoiceEditService.GetItemsTotal: currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FItems) - 1 do
    Result := Result + FItems[i].Amount;
end;

function TInvoiceEditService.Save: TInvoiceEditResult;
begin
  Result := ierDatabaseError;

  if FOrder = nil then
    Exit;

  if Trim(Utf8ToString(FOrder.OrderNo)) = '' then
  begin
    Result := ierMissingField;
    Exit;
  end;

  SaveItemsToOrder;
  FOrder.ItemsTotal := GetItemsTotal;

  if FIsNew then
  begin
    if FRestOrm.Add(FOrder, True) > 0 then
    begin
      FIsNew := False;
      Result := ierSuccess;
    end;
  end
  else
  begin
    if FRestOrm.Update(FOrder) then
      Result := ierSuccess;
  end;
end;

function TInvoiceEditService.IsNewInvoice: Boolean;
begin
  Result := FIsNew;
end;

function TInvoiceEditService.GenerateOrderNo: string;
var
  MaxNo: RawUtf8;
  NumPart: integer;
begin
  MaxNo := FRestOrm.OneFieldValue(TOrmCustomerOrder,
    'MAX(CAST(OrderNo AS INTEGER))', '', []);
  NumPart := Utf8ToInteger(MaxNo, 0);
  Inc(NumPart);
  Result := Format('%.6d', [NumPart]);
end;

{ TCustomerEditService }

constructor TCustomerEditService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  FCustomer := nil;
  FIsNew := False;
end;

constructor TCustomerEditService.Create;
begin
  Create(RgRestClient.Orm);
end;

destructor TCustomerEditService.Destroy;
begin
  if FCustomer <> nil then
    FreeAndNil(FCustomer);
  inherited Destroy;
end;

function TCustomerEditService.EnsureContact: TPersonItem;
begin
  if FCustomer.Contacts.Count = 0 then
  begin
    Result := TPersonItem(FCustomer.Contacts.Add);
    Result.Phones.Add('');
    Result.Phones.Add('');
    Result.Addresses.Add;
  end
  else
    Result := FCustomer.Contacts[0];
end;

function TCustomerEditService.EnsureAddress: TAddressItem;
var
  Contact: TPersonItem;
begin
  Contact := EnsureContact;
  if Contact.Addresses.Count = 0 then
    Result := TAddressItem(Contact.Addresses.Add)
  else
    Result := Contact.Addresses[0];
end;

function TCustomerEditService.LoadCustomer(ACustomerID: longint): Boolean;
begin
  Result := False;

  if FCustomer <> nil then
    FreeAndNil(FCustomer);

  FCustomer := TOrmCustomer.Create(FRestOrm, ACustomerID);
  if FCustomer.ID = 0 then
  begin
    FreeAndNil(FCustomer);
    Exit;
  end;

  FIsNew := False;
  Result := True;
end;

function TCustomerEditService.CreateNewCustomer: Boolean;
begin
  if FCustomer <> nil then
    FreeAndNil(FCustomer);

  FCustomer := TOrmCustomer.Create;
  FCustomer.CustomerNo := StringToUtf8(GenerateCustomerNo);
  EnsureContact;

  FIsNew := True;
  Result := True;
end;

function TCustomerEditService.GetCustomerNo: string;
begin
  Result := '';
  if FCustomer <> nil then
    Result := Utf8ToString(FCustomer.CustomerNo);
end;

procedure TCustomerEditService.SetCustomerNo(const AValue: string);
begin
  if FCustomer <> nil then
    FCustomer.CustomerNo := StringToUtf8(AValue);
end;

function TCustomerEditService.GetCompany: string;
begin
  Result := '';
  if FCustomer <> nil then
    Result := Utf8ToString(FCustomer.Company);
end;

procedure TCustomerEditService.SetCompany(const AValue: string);
begin
  if FCustomer <> nil then
    FCustomer.Company := StringToUtf8(AValue);
end;

function TCustomerEditService.GetPhone: string;
var
  Contact: TPersonItem;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) then
  begin
    Contact := FCustomer.Contacts[0];
    if Contact.Phones.Count > 0 then
      Result := Utf8ToString(Contact.Phones[0]);
  end;
end;

procedure TCustomerEditService.SetPhone(const AValue: string);
var
  Contact: TPersonItem;
begin
  if FCustomer <> nil then
  begin
    Contact := EnsureContact;
    while Contact.Phones.Count < 1 do
      Contact.Phones.Add('');
    Contact.Phones[0] := StringToUtf8(AValue);
  end;
end;

function TCustomerEditService.GetFax: string;
var
  Contact: TPersonItem;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) then
  begin
    Contact := FCustomer.Contacts[0];
    if Contact.Phones.Count > 1 then
      Result := Utf8ToString(Contact.Phones[1]);
  end;
end;

procedure TCustomerEditService.SetFax(const AValue: string);
var
  Contact: TPersonItem;
begin
  if FCustomer <> nil then
  begin
    Contact := EnsureContact;
    while Contact.Phones.Count < 2 do
      Contact.Phones.Add('');
    Contact.Phones[1] := StringToUtf8(AValue);
  end;
end;

function TCustomerEditService.GetAddress: string;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) and
     (FCustomer.Contacts[0].Addresses.Count > 0) then
    Result := Utf8ToString(FCustomer.Contacts[0].Addresses[0].Street1);
end;

procedure TCustomerEditService.SetAddress(const AValue: string);
begin
  if FCustomer <> nil then
    EnsureAddress.Street1 := StringToUtf8(AValue);
end;

function TCustomerEditService.GetZip: string;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) and
     (FCustomer.Contacts[0].Addresses.Count > 0) then
    Result := Utf8ToString(FCustomer.Contacts[0].Addresses[0].Code);
end;

procedure TCustomerEditService.SetZip(const AValue: string);
begin
  if FCustomer <> nil then
    EnsureAddress.Code := StringToUtf8(AValue);
end;

function TCustomerEditService.GetCity: string;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) and
     (FCustomer.Contacts[0].Addresses.Count > 0) then
    Result := Utf8ToString(FCustomer.Contacts[0].Addresses[0].City);
end;

procedure TCustomerEditService.SetCity(const AValue: string);
begin
  if FCustomer <> nil then
    EnsureAddress.City := StringToUtf8(AValue);
end;

function TCustomerEditService.GetCountry: string;
begin
  Result := '';
  if (FCustomer <> nil) and (FCustomer.Contacts.Count > 0) and
     (FCustomer.Contacts[0].Addresses.Count > 0) then
    Result := Utf8ToString(FCustomer.Contacts[0].Addresses[0].Country);
end;

procedure TCustomerEditService.SetCountry(const AValue: string);
begin
  if FCustomer <> nil then
    EnsureAddress.Country := StringToUtf8(AValue);
end;

function TCustomerEditService.Save: TCustomerEditResult;
begin
  Result := cerDatabaseError;

  if FCustomer = nil then
    Exit;

  if Trim(Utf8ToString(FCustomer.CustomerNo)) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;

  if Trim(Utf8ToString(FCustomer.Company)) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;

  if FIsNew then
  begin
    if FRestOrm.Add(FCustomer, True) > 0 then
    begin
      FIsNew := False;
      Result := cerSuccess;
    end;
  end
  else
  begin
    if FRestOrm.Update(FCustomer) then
      Result := cerSuccess;
  end;
end;

function TCustomerEditService.IsNewCustomer: Boolean;
begin
  Result := FIsNew;
end;

function TCustomerEditService.GenerateCustomerNo: string;
var
  MaxNo: RawUtf8;
  NumPart: integer;
begin
  MaxNo := FRestOrm.OneFieldValue(TOrmCustomer,
    'MAX(CAST(CustomerNo AS INTEGER))', '', []);
  NumPart := Utf8ToInteger(MaxNo, 0);
  Inc(NumPart);
  Result := Format('%.6d', [NumPart]);
end;

function TCustomerEditService.DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
var
  CountValue: RawUtf8;
  OrderCount: integer;
begin
  Result := cerDatabaseError;

  if ACustomerID <= 0 then
  begin
    Result := cerNotFound;
    Exit;
  end;

  if not FRestOrm.MemberExists(TOrmCustomer, ACustomerID) then
  begin
    Result := cerNotFound;
    Exit;
  end;

  CountValue := FRestOrm.OneFieldValue(TOrmCustomerOrder,
    'COUNT(*)', 'Customer=?', [ACustomerID]);
  OrderCount := Utf8ToInteger(CountValue, 0);
  if OrderCount > 0 then
  begin
    Result := cerHasReferences;
    Exit;
  end;

  if FRestOrm.Delete(TOrmCustomer, ACustomerID) then
    Result := cerSuccess;
end;

{ TOpenItemsReportService }

constructor TOpenItemsReportService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  SetLength(FItems, 0);
end;

constructor TOpenItemsReportService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TOpenItemsReportService.LoadOpenItems(AFromDate, AToDate: TDateTime;
  AMinAmount: currency);
var
  SQL: RawUtf8;
  Table: TOrmTable;
  i: integer;
  Item: TDtoOpenItem;
  SaleDateValue: Int64;
  FromDate, ToDate: Int64;
  MinAmountInt64: Int64;
begin
  Clear;

  FromDate := TimeLogFromDateTime(AFromDate);
  ToDate := TimeLogFromDateTime(AToDate);
  // Currency is stored as Int64 * 10000 in mORMot
  MinAmountInt64 := Trunc(AMinAmount * 10000);

  // Query open items with customer join, filter by SaleDate range
  SQL := FormatUtf8(
    'SELECT o.ID, c.Company, o.OrderNo, o.SaleDate, o.ItemsTotal, ' +
    '(o.ItemsTotal - COALESCE(o.AmountPaid, 0)) as OpenAmount ' +
    'FROM CustomerOrder o ' +
    'INNER JOIN Customer c ON o.Customer = c.ID ' +
    'WHERE COALESCE(o.AmountPaid, 0) < o.ItemsTotal ' +
    'AND o.SaleDate >= % ' +
    'AND o.SaleDate <= % ' +
    'AND (o.ItemsTotal - COALESCE(o.AmountPaid, 0)) >= % ' +
    'ORDER BY o.SaleDate ASC',
    [FromDate, ToDate, MinAmountInt64]);

  Table := FRestOrm.ExecuteList([TOrmCustomerOrder, TOrmCustomer], SQL);
  if Table <> nil then
  try
    SetLength(FItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.OrderID := Table.GetAsInteger(i, 0);
      Item.Company := Utf8ToString(Table.GetU(i, 1));
      Item.OrderNo := Utf8ToString(Table.GetU(i, 2));
      SaleDateValue := Table.GetAsInt64(i, 3);
      if SaleDateValue > 0 then
        Item.SaleDate := TimeLogToDateTime(SaleDateValue)
      else
        Item.SaleDate := 0;
      Item.ItemsTotal := Table.GetAsCurrency(i, 4);
      Item.OpenAmount := Table.GetAsCurrency(i, 5);
      // Calculate days overdue from SaleDate
      if SaleDateValue > 0 then
        Item.DaysOverdue := Trunc(Date - Item.SaleDate)
      else
        Item.DaysOverdue := 0;
      FItems[i - 1] := Item;
    end;
  finally
    Table.Free;
  end;
end;

function TOpenItemsReportService.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TOpenItemsReportService.GetItem(AIndex: integer): TDtoOpenItem;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
    Result := FItems[AIndex];
end;

procedure TOpenItemsReportService.Clear;
begin
  SetLength(FItems, 0);
end;

{ TPaymentReceiptsReportService }

constructor TPaymentReceiptsReportService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  SetLength(FItems, 0);
end;

constructor TPaymentReceiptsReportService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TPaymentReceiptsReportService.LoadPaymentReceipts(AFromDate, AToDate: TDateTime);
var
  SQL: RawUtf8;
  Table: TOrmTable;
  i: integer;
  Item: TDtoPaymentReceipt;
  SaleDateValue: Int64;
  FromDate, ToDate: Int64;
begin
  Clear;

  FromDate := TimeLogFromDateTime(AFromDate);
  ToDate := TimeLogFromDateTime(AToDate);

  // Query invoices with payments (AmountPaid > 0), filter by SaleDate range
  // Sorted by SaleDate descending (newest first)
  SQL := FormatUtf8(
    'SELECT o.ID, o.SaleDate, c.Company, o.OrderNo, o.AmountPaid ' +
    'FROM CustomerOrder o ' +
    'INNER JOIN Customer c ON o.Customer = c.ID ' +
    'WHERE COALESCE(o.AmountPaid, 0) > 0 ' +
    'AND o.SaleDate >= % ' +
    'AND o.SaleDate <= % ' +
    'ORDER BY o.SaleDate DESC',
    [FromDate, ToDate]);

  Table := FRestOrm.ExecuteList([TOrmCustomerOrder, TOrmCustomer], SQL);
  if Table <> nil then
  try
    SetLength(FItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.OrderID := Table.GetAsInteger(i, 0);
      SaleDateValue := Table.GetAsInt64(i, 1);
      if SaleDateValue > 0 then
        Item.SaleDate := TimeLogToDateTime(SaleDateValue)
      else
        Item.SaleDate := 0;
      Item.Company := Utf8ToString(Table.GetU(i, 2));
      Item.OrderNo := Utf8ToString(Table.GetU(i, 3));
      Item.AmountPaid := Table.GetAsCurrency(i, 4);
      FItems[i - 1] := Item;
    end;
  finally
    Table.Free;
  end;
end;

function TPaymentReceiptsReportService.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TPaymentReceiptsReportService.GetItem(AIndex: integer): TDtoPaymentReceipt;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
    Result := FItems[AIndex];
end;

procedure TPaymentReceiptsReportService.Clear;
begin
  SetLength(FItems, 0);
end;

{ TCustomerRevenueReportService }

constructor TCustomerRevenueReportService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  SetLength(FItems, 0);
end;

constructor TCustomerRevenueReportService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TCustomerRevenueReportService.LoadCustomerRevenue(AYear: integer);
var
  SQL: RawUtf8;
  Table: TOrmTable;
  i: integer;
  Item: TDtoCustomerRevenue;
  YearStart, YearEnd: Int64;
  StartDate, EndDate: TDateTime;
begin
  Clear;

  // Calculate year date range
  StartDate := EncodeDate(AYear, 1, 1);
  EndDate := EncodeDate(AYear, 12, 31);
  YearStart := TimeLogFromDateTime(StartDate);
  YearEnd := TimeLogFromDateTime(EndDate);

  // Aggregate by customer for the given year
  SQL := FormatUtf8(
    'SELECT c.ID, c.Company, ' +
    'COUNT(o.ID) as InvoiceCount, ' +
    'COALESCE(SUM(o.ItemsTotal), 0) as TotalRevenue, ' +
    'COALESCE(SUM(o.AmountPaid), 0) as TotalPaid, ' +
    'COALESCE(SUM(o.ItemsTotal - COALESCE(o.AmountPaid, 0)), 0) as TotalOpen ' +
    'FROM Customer c ' +
    'LEFT JOIN CustomerOrder o ON o.Customer = c.ID ' +
    'AND o.SaleDate >= % AND o.SaleDate <= % ' +
    'GROUP BY c.ID, c.Company ' +
    'HAVING COUNT(o.ID) > 0 ' +
    'ORDER BY TotalRevenue DESC',
    [YearStart, YearEnd]);

  Table := FRestOrm.ExecuteList([TOrmCustomer, TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    SetLength(FItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.CustomerID := Table.GetAsInteger(i, 0);
      Item.Company := Utf8ToString(Table.GetU(i, 1));
      Item.InvoiceCount := Table.GetAsInteger(i, 2);
      Item.TotalRevenue := Table.GetAsCurrency(i, 3);
      Item.TotalPaid := Table.GetAsCurrency(i, 4);
      Item.TotalOpen := Table.GetAsCurrency(i, 5);
      FItems[i - 1] := Item;
    end;
  finally
    Table.Free;
  end;
end;

function TCustomerRevenueReportService.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TCustomerRevenueReportService.GetItem(AIndex: integer): TDtoCustomerRevenue;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
    Result := FItems[AIndex];
end;

procedure TCustomerRevenueReportService.Clear;
begin
  SetLength(FItems, 0);
end;

{ TMonthlyOverviewReportService }

constructor TMonthlyOverviewReportService.Create(ARestOrm: IRestOrm);
begin
  inherited Create;
  FRestOrm := ARestOrm;
  SetLength(FItems, 0);
  FillChar(FTotals, SizeOf(FTotals), 0);
end;

constructor TMonthlyOverviewReportService.Create;
begin
  Create(RgRestClient.Orm);
end;

procedure TMonthlyOverviewReportService.LoadMonthlyOverview(AYear: integer);
const
  MonthNames: array[1..12] of string = (
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'
  );
var
  Orders: TOrmCustomerOrder;
  i, MonthNum: integer;
  YearStart, YearEnd: TTimeLog;
  StartDate, EndDate: TDateTime;
  SaleDateBits: TTimeLogBits;
begin
  Clear;

  // Calculate year date range using mORMot2 functions
  StartDate := EncodeDate(AYear, 1, 1);
  EndDate := EncodeDate(AYear, 12, 31) + 0.99999;  // Include full last day
  YearStart := TimeLogFromDateTime(StartDate);
  YearEnd := TimeLogFromDateTime(EndDate);

  // Initialize 12 months
  SetLength(FItems, 12);
  for i := 1 to 12 do
  begin
    FillChar(FItems[i - 1], SizeOf(TDtoMonthlyOverview), 0);
    FItems[i - 1].Month := i;
    FItems[i - 1].MonthName := MonthNames[i];
  end;

  // Load all orders for the year and aggregate in Pascal
  Orders := TOrmCustomerOrder.CreateAndFillPrepare(FRestOrm,
    'SaleDate >= ? AND SaleDate <= ?', [YearStart, YearEnd]);
  try
    while Orders.FillOne do
    begin
      // Use mORMot2 TTimeLogBits to extract month
      SaleDateBits.Value := Orders.SaleDate;
      MonthNum := SaleDateBits.Month;
      if (MonthNum >= 1) and (MonthNum <= 12) then
      begin
        Inc(FItems[MonthNum - 1].InvoiceCount);
        FItems[MonthNum - 1].Revenue := FItems[MonthNum - 1].Revenue + Orders.ItemsTotal;
        FItems[MonthNum - 1].PaymentsReceived := FItems[MonthNum - 1].PaymentsReceived + Orders.AmountPaid;
        FItems[MonthNum - 1].OpenAmount := FItems[MonthNum - 1].OpenAmount +
          (Orders.ItemsTotal - Orders.AmountPaid);
      end;
    end;
  finally
    Orders.Free;
  end;

  // Calculate totals
  FillChar(FTotals, SizeOf(FTotals), 0);
  FTotals.MonthName := 'Total';
  for i := 0 to 11 do
  begin
    FTotals.InvoiceCount := FTotals.InvoiceCount + FItems[i].InvoiceCount;
    FTotals.Revenue := FTotals.Revenue + FItems[i].Revenue;
    FTotals.PaymentsReceived := FTotals.PaymentsReceived + FItems[i].PaymentsReceived;
    FTotals.OpenAmount := FTotals.OpenAmount + FItems[i].OpenAmount;
  end;
end;

function TMonthlyOverviewReportService.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TMonthlyOverviewReportService.GetItem(AIndex: integer): TDtoMonthlyOverview;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex >= 0) and (AIndex < Length(FItems)) then
    Result := FItems[AIndex];
end;

function TMonthlyOverviewReportService.GetTotals: TDtoMonthlyOverview;
begin
  Result := FTotals;
end;

procedure TMonthlyOverviewReportService.Clear;
begin
  SetLength(FItems, 0);
  FillChar(FTotals, SizeOf(FTotals), 0);
end;

initialization
  RgRestClient := TRgRestClient.Create;
  case rgConfig.RgConfig.Mode of
    rmLocal:
      RgServices := TRgServiceClient.CreateLocal;
    rmService:
      RgServices := TRgServiceClient.CreateService(
        rgConfig.RgConfig.Host, rgConfig.RgConfig.Port);
  end;

finalization
  FreeAndNil(RgServices);
  RgRestClient.Free;

end.
