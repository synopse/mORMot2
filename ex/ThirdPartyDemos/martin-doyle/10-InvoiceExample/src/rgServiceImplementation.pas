{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgServiceImplementation.pas

  Last modified
    Date : 07.02.2026
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
unit rgServiceImplementation;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  Classes,
  Contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.text,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.soa.core,
  mormot.soa.server,
  rgData,
  rgDtoTypes,
  rgServiceInterfaces;

type

  { TRgCustomerService }

  TRgCustomerService = class(TInjectableObjectRest, IRgCustomerService)
  public
    function ListCustomers(out ACustomers: TDtoCustomerDynArray): TCustomerEditResult;
    function GetCustomer(ACustomerID: longint; out ACustomer: TDtoCustomer): TCustomerEditResult;
    function CreateCustomer(const ACustomer: TDtoCustomer; out ANewID: longint): TCustomerEditResult;
    function UpdateCustomer(ACustomerID: longint; const ACustomer: TDtoCustomer): TCustomerEditResult;
    function DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
    function GenerateCustomerNo(out ACustomerNo: RawUtf8): TCustomerEditResult;
  end;

  { TRgInvoiceService }

  TRgInvoiceService = class(TInjectableObjectRest, IRgInvoiceService)
  public
    function ListInvoicesForCustomer(ACustomerID: longint; out AInvoices: TDtoOrderDynArray): TInvoiceEditResult;
    function GetInvoice(AInvoiceID: longint; out AInvoice: TDtoInvoiceDetail): TInvoiceEditResult;
    function CreateInvoice(ACustomerID: longint; const AInvoice: TDtoInvoiceSave; out ANewID: longint): TInvoiceEditResult;
    function UpdateInvoice(AInvoiceID: longint; const AInvoice: TDtoInvoiceSave): TInvoiceEditResult;
    function DeleteInvoice(AInvoiceID: longint): TInvoiceEditResult;
    function GenerateOrderNo(out AOrderNo: RawUtf8): TInvoiceEditResult;
  end;

  { TRgPaymentService }

  TRgPaymentService = class(TInjectableObjectRest, IRgPaymentService)
  public
    function AddPayment(AInvoiceID: longint; AAmount: currency; ADate: TDateTime): TPaymentResult;
    function GetInvoiceOpenAmount(AInvoiceID: longint; out AOpenAmount: currency): TPaymentResult;
  end;

  { TRgStatisticsService }

  TRgStatisticsService = class(TInjectableObjectRest, IRgStatisticsService)
  public
    function GetDashboardStats(out AStats: TDtoDashboardStats): integer;
    function GetCustomerSummary(ACustomerID: longint; out ASummary: TDtoCustomerSummary): integer;
  end;

  { TRgReportService }

  TRgReportService = class(TInjectableObjectRest, IRgReportService)
  public
    function GetOpenItemsReport(AFromDate, AToDate: TDateTime; AMinAmount: currency;
      out AItems: TDtoOpenItemDynArray): integer;
    function GetPaymentReceiptsReport(AFromDate, AToDate: TDateTime;
      out AItems: TDtoPaymentReceiptDynArray): integer;
    function GetCustomerRevenueReport(AYear: integer;
      out AItems: TDtoCustomerRevenueDynArray): integer;
    function GetMonthlyOverviewReport(AYear: integer;
      out AItems: TDtoMonthlyOverviewDynArray; out ATotals: TDtoMonthlyOverview): integer;
  end;

implementation

{
******************************* TRgCustomerService *****************************
}
function TRgCustomerService.ListCustomers(out ACustomers: TDtoCustomerDynArray): TCustomerEditResult;
var
  Customer: TOrmCustomer;
  Contact: TPersonItem;
  Address: TAddressItem;
  Count: integer;
begin
  SetLength(ACustomers, 0);
  Customer := TOrmCustomer.CreateAndFillPrepare(Self.Server.Orm, '',
    'ID, CustomerNo, Company, Contacts');
  try
    Customer.FillTable.SortFields(2); // sort by Company
    Count := 0;
    SetLength(ACustomers, Customer.FillTable.RowCount);
    while Customer.FillOne do
    begin
      ACustomers[Count].CustomerID := Customer.ID;
      ACustomers[Count].CustomerNo := Utf8ToString(Customer.CustomerNo);
      ACustomers[Count].Company := Utf8ToString(Customer.Company);
      if Customer.Contacts.Count > 0 then
      begin
        Contact := Customer.Contacts[0];
        ACustomers[Count].Phone := Utf8ToString(Contact.Phones[0]);
        ACustomers[Count].Fax := Utf8ToString(Contact.Phones[1]);
        if Contact.Addresses.Count > 0 then
        begin
          Address := Contact.Addresses[0];
          ACustomers[Count].Address := Utf8ToString(Address.Street1);
          ACustomers[Count].Zip := Utf8ToString(Address.Code);
          ACustomers[Count].City := Utf8ToString(Address.City);
          ACustomers[Count].Country := Utf8ToString(Address.Country);
        end;
      end;
      Inc(Count);
    end;
    SetLength(ACustomers, Count);
    Result := cerSuccess;
  finally
    Customer.Free;
  end;
end;

function TRgCustomerService.GetCustomer(ACustomerID: longint; out ACustomer: TDtoCustomer): TCustomerEditResult;
var
  Customer: TOrmCustomer;
  Contact: TPersonItem;
  Address: TAddressItem;
begin
  Customer := TOrmCustomer.Create(Self.Server.Orm, ACustomerID);
  try
    if Customer.ID = 0 then
    begin
      Result := cerNotFound;
      Exit;
    end;

    ACustomer.CustomerID := Customer.ID;
    ACustomer.CustomerNo := Utf8ToString(Customer.CustomerNo);
    ACustomer.Company := Utf8ToString(Customer.Company);
    if Customer.Contacts.Count > 0 then
    begin
      Contact := Customer.Contacts[0];
      ACustomer.Phone := Utf8ToString(Contact.Phones[0]);
      ACustomer.Fax := Utf8ToString(Contact.Phones[1]);
      if Contact.Addresses.Count > 0 then
      begin
        Address := Contact.Addresses[0];
        ACustomer.Address := Utf8ToString(Address.Street1);
        ACustomer.Zip := Utf8ToString(Address.Code);
        ACustomer.City := Utf8ToString(Address.City);
        ACustomer.Country := Utf8ToString(Address.Country);
      end;
    end;
    Result := cerSuccess;
  finally
    Customer.Free;
  end;
end;

function TRgCustomerService.CreateCustomer(const ACustomer: TDtoCustomer; out ANewID: longint): TCustomerEditResult;
var
  Customer: TOrmCustomer;
  Contact: TPersonItem;
  Address: TAddressItem;
begin
  ANewID := 0;
  Result := cerDatabaseError;

  if Trim(ACustomer.CustomerNo) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;
  if Trim(ACustomer.Company) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;

  Customer := TOrmCustomer.Create;
  try
    Customer.CustomerNo := StringToUtf8(ACustomer.CustomerNo);
    Customer.Company := StringToUtf8(ACustomer.Company);

    // Build nested contact with phones and address
    Contact := TPersonItem(Customer.Contacts.Add);
    Contact.Phones.Add(StringToUtf8(ACustomer.Phone));
    Contact.Phones.Add(StringToUtf8(ACustomer.Fax));
    Address := TAddressItem(Contact.Addresses.Add);
    Address.Street1 := StringToUtf8(ACustomer.Address);
    Address.Code := StringToUtf8(ACustomer.Zip);
    Address.City := StringToUtf8(ACustomer.City);
    Address.Country := StringToUtf8(ACustomer.Country);

    ANewID := Self.Server.Orm.Add(Customer, True);
    if ANewID > 0 then
      Result := cerSuccess;
  finally
    Customer.Free;
  end;
end;

function TRgCustomerService.UpdateCustomer(ACustomerID: longint; const ACustomer: TDtoCustomer): TCustomerEditResult;
var
  Customer: TOrmCustomer;
  Contact: TPersonItem;
  Address: TAddressItem;
begin
  Result := cerDatabaseError;

  if Trim(ACustomer.CustomerNo) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;
  if Trim(ACustomer.Company) = '' then
  begin
    Result := cerMissingField;
    Exit;
  end;

  Customer := TOrmCustomer.Create(Self.Server.Orm, ACustomerID);
  try
    if Customer.ID = 0 then
    begin
      Result := cerNotFound;
      Exit;
    end;

    Customer.CustomerNo := StringToUtf8(ACustomer.CustomerNo);
    Customer.Company := StringToUtf8(ACustomer.Company);

    // Ensure contact structure exists
    if Customer.Contacts.Count = 0 then
    begin
      Contact := TPersonItem(Customer.Contacts.Add);
      Contact.Phones.Add('');
      Contact.Phones.Add('');
      Contact.Addresses.Add;
    end
    else
      Contact := Customer.Contacts[0];

    while Contact.Phones.Count < 2 do
      Contact.Phones.Add('');
    Contact.Phones[0] := StringToUtf8(ACustomer.Phone);
    Contact.Phones[1] := StringToUtf8(ACustomer.Fax);

    if Contact.Addresses.Count = 0 then
      Address := TAddressItem(Contact.Addresses.Add)
    else
      Address := Contact.Addresses[0];
    Address.Street1 := StringToUtf8(ACustomer.Address);
    Address.Code := StringToUtf8(ACustomer.Zip);
    Address.City := StringToUtf8(ACustomer.City);
    Address.Country := StringToUtf8(ACustomer.Country);

    if Self.Server.Orm.Update(Customer) then
      Result := cerSuccess;
  finally
    Customer.Free;
  end;
end;

function TRgCustomerService.DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
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

  if not Self.Server.Orm.MemberExists(TOrmCustomer, ACustomerID) then
  begin
    Result := cerNotFound;
    Exit;
  end;

  CountValue := Self.Server.Orm.OneFieldValue(TOrmCustomerOrder,
    'COUNT(*)', 'Customer=?', [ACustomerID]);
  OrderCount := Utf8ToInteger(CountValue, 0);
  if OrderCount > 0 then
  begin
    Result := cerHasReferences;
    Exit;
  end;

  if Self.Server.Orm.Delete(TOrmCustomer, ACustomerID) then
    Result := cerSuccess;
end;

function TRgCustomerService.GenerateCustomerNo(out ACustomerNo: RawUtf8): TCustomerEditResult;
var
  MaxNo: RawUtf8;
  NumPart: integer;
begin
  MaxNo := Self.Server.Orm.OneFieldValue(TOrmCustomer,
    'MAX(CAST(CustomerNo AS INTEGER))', '', []);
  NumPart := Utf8ToInteger(MaxNo, 0);
  Inc(NumPart);
  ACustomerNo := StringToUtf8(Format('%.6d', [NumPart]));
  Result := cerSuccess;
end;

{
******************************* TRgInvoiceService ******************************
}
function TRgInvoiceService.ListInvoicesForCustomer(ACustomerID: longint;
  out AInvoices: TDtoOrderDynArray): TInvoiceEditResult;
var
  Order: TOrmCustomerOrder;
  ShipDateTime: TDateTime;
  OpenAmount: currency;
  Count: integer;
begin
  SetLength(AInvoices, 0);

  Order := TOrmCustomerOrder.CreateAndFillPrepare(Self.Server.Orm,
    'Customer=?', [ACustomerID],
    'ID, OrderNo, SaleDate, ShipDate, ItemsTotal, AmountPaid');
  try
    Order.FillTable.SortFields(2, False); // sort by SaleDate descending
    Count := 0;
    SetLength(AInvoices, Order.FillTable.RowCount);
    while Order.FillOne do
    begin
      AInvoices[Count].OrderID := Order.ID;
      AInvoices[Count].OrderNo := Utf8ToString(Order.OrderNo);
      AInvoices[Count].SaleDate := TimeLogToDateTime(Order.SaleDate);

      if Order.ShipDate > 0 then
        ShipDateTime := TimeLogToDateTime(Order.ShipDate)
      else
        ShipDateTime := 0;
      AInvoices[Count].ShipDate := ShipDateTime;

      AInvoices[Count].ItemsTotal := Order.ItemsTotal;
      AInvoices[Count].AmountPaid := Order.AmountPaid;
      OpenAmount := Order.ItemsTotal - Order.AmountPaid;
      AInvoices[Count].OpenAmount := OpenAmount;

      // Determine status
      if OpenAmount <= 0 then
        AInvoices[Count].Status := isPaid
      else if (ShipDateTime > 0) and (ShipDateTime < Date) then
        AInvoices[Count].Status := isOverdue
      else
        AInvoices[Count].Status := isOpen;

      Inc(Count);
    end;
    SetLength(AInvoices, Count);
    Result := ierSuccess;
  finally
    Order.Free;
  end;
end;

function TRgInvoiceService.GetInvoice(AInvoiceID: longint;
  out AInvoice: TDtoInvoiceDetail): TInvoiceEditResult;
var
  Order: TOrmCustomerOrder;
  ShipDateTime: TDateTime;
  OpenAmount: currency;
  Item: TItem;
  ItemAmount: currency;
  i: integer;
begin
  Result := ierDatabaseError;

  Order := TOrmCustomerOrder.CreateJoined(Self.Server.Orm, AInvoiceID);
  try
    if Order.ID = 0 then
    begin
      Result := ierNotFound;
      Exit;
    end;

    AInvoice.OrderID := Order.ID;
    AInvoice.OrderNo := Utf8ToString(Order.OrderNo);
    AInvoice.SaleDate := TimeLogToDateTime(Order.SaleDate);

    if Order.ShipDate > 0 then
      ShipDateTime := TimeLogToDateTime(Order.ShipDate)
    else
      ShipDateTime := 0;
    AInvoice.ShipDate := ShipDateTime;

    AInvoice.CustomerID := Order.Customer.ID;
    AInvoice.CustomerName := Utf8ToString(Order.Customer.Company);

    AInvoice.ItemsTotal := Order.ItemsTotal;
    AInvoice.AmountPaid := Order.AmountPaid;
    OpenAmount := Order.ItemsTotal - Order.AmountPaid;
    AInvoice.OpenAmount := OpenAmount;

    if OpenAmount <= 0 then
      AInvoice.Status := isPaid
    else if (ShipDateTime > 0) and (ShipDateTime < Date) then
      AInvoice.Status := isOverdue
    else
      AInvoice.Status := isOpen;

    // Map items from TItemCollection to TDtoInvoiceItemArray
    SetLength(AInvoice.Items, Order.Items.Count);
    for i := 0 to Order.Items.Count - 1 do
    begin
      Item := Order.Items[i];
      AInvoice.Items[i].Position := Item.Position;
      AInvoice.Items[i].PartNo := Utf8ToString(Item.PartNo);
      AInvoice.Items[i].Description := Utf8ToString(Item.Description);
      AInvoice.Items[i].Quantity := Item.Quantity;
      AInvoice.Items[i].ListPrice := Item.ListPrice;
      AInvoice.Items[i].Discount := Item.Discount;
      ItemAmount := Item.ListPrice * Item.Quantity;
      if Item.Discount > 0 then
        ItemAmount := ItemAmount * (100 - Item.Discount) / 100;
      AInvoice.Items[i].Amount := ItemAmount;
    end;

    Result := ierSuccess;
  finally
    Order.Free;
  end;
end;

function TRgInvoiceService.CreateInvoice(ACustomerID: longint;
  const AInvoice: TDtoInvoiceSave; out ANewID: longint): TInvoiceEditResult;
var
  Order: TOrmCustomerOrder;
  Customer: TOrmCustomer;
  Item: TItem;
  ItemTotal: currency;
  Total: currency;
  i: integer;
begin
  ANewID := 0;
  Result := ierDatabaseError;

  if Trim(AInvoice.OrderNo) = '' then
  begin
    Result := ierMissingField;
    Exit;
  end;

  // Validate customer exists
  Customer := TOrmCustomer.Create(Self.Server.Orm, ACustomerID);
  try
    if Customer.ID = 0 then
    begin
      Result := ierNotFound;
      Exit;
    end;

    Order := TOrmCustomerOrder.Create;
    try
      Order.Customer := Customer.AsTOrm;
      Order.OrderNo := StringToUtf8(AInvoice.OrderNo);
      Order.SaleDate := TimeLogFromDateTime(AInvoice.SaleDate);
      Order.ShipDate := TimeLogFromDateTime(AInvoice.ShipDate);
      Order.AmountPaid := 0;

      // Build TItemCollection from DTO items and calculate total
      Total := 0;
      for i := 0 to Length(AInvoice.Items) - 1 do
      begin
        Item := TItem(Order.Items.Add);
        Item.Position := i + 1;
        Item.PartNo := StringToUtf8(AInvoice.Items[i].PartNo);
        Item.Description := StringToUtf8(AInvoice.Items[i].Description);
        Item.Quantity := AInvoice.Items[i].Quantity;
        Item.ListPrice := AInvoice.Items[i].ListPrice;
        Item.Discount := AInvoice.Items[i].Discount;
        ItemTotal := Item.ListPrice * Item.Quantity;
        if Item.Discount > 0 then
          ItemTotal := ItemTotal * (100 - Item.Discount) / 100;
        Total := Total + ItemTotal;
      end;
      Order.ItemsTotal := Total;

      ANewID := Self.Server.Orm.Add(Order, True);
      if ANewID > 0 then
        Result := ierSuccess;
    finally
      Order.Free;
    end;
  finally
    Customer.Free;
  end;
end;

function TRgInvoiceService.UpdateInvoice(AInvoiceID: longint;
  const AInvoice: TDtoInvoiceSave): TInvoiceEditResult;
var
  Order: TOrmCustomerOrder;
  Item: TItem;
  ItemTotal: currency;
  Total: currency;
  i: integer;
begin
  Result := ierDatabaseError;

  if Trim(AInvoice.OrderNo) = '' then
  begin
    Result := ierMissingField;
    Exit;
  end;

  Order := TOrmCustomerOrder.Create(Self.Server.Orm, AInvoiceID);
  try
    if Order.ID = 0 then
    begin
      Result := ierNotFound;
      Exit;
    end;

    Order.OrderNo := StringToUtf8(AInvoice.OrderNo);
    Order.SaleDate := TimeLogFromDateTime(AInvoice.SaleDate);
    Order.ShipDate := TimeLogFromDateTime(AInvoice.ShipDate);

    // Rebuild TItemCollection from DTO items and recalculate total
    Order.Items.Clear;
    Total := 0;
    for i := 0 to Length(AInvoice.Items) - 1 do
    begin
      Item := TItem(Order.Items.Add);
      Item.Position := i + 1;
      Item.PartNo := StringToUtf8(AInvoice.Items[i].PartNo);
      Item.Description := StringToUtf8(AInvoice.Items[i].Description);
      Item.Quantity := AInvoice.Items[i].Quantity;
      Item.ListPrice := AInvoice.Items[i].ListPrice;
      Item.Discount := AInvoice.Items[i].Discount;
      ItemTotal := Item.ListPrice * Item.Quantity;
      if Item.Discount > 0 then
        ItemTotal := ItemTotal * (100 - Item.Discount) / 100;
      Total := Total + ItemTotal;
    end;
    Order.ItemsTotal := Total;

    if Self.Server.Orm.Update(Order) then
      Result := ierSuccess;
  finally
    Order.Free;
  end;
end;

function TRgInvoiceService.DeleteInvoice(AInvoiceID: longint): TInvoiceEditResult;
begin
  Result := ierDatabaseError;

  if AInvoiceID <= 0 then
  begin
    Result := ierNotFound;
    Exit;
  end;

  if not Self.Server.Orm.MemberExists(TOrmCustomerOrder, AInvoiceID) then
  begin
    Result := ierNotFound;
    Exit;
  end;

  if Self.Server.Orm.Delete(TOrmCustomerOrder, AInvoiceID) then
    Result := ierSuccess;
end;

function TRgInvoiceService.GenerateOrderNo(out AOrderNo: RawUtf8): TInvoiceEditResult;
var
  MaxNo: RawUtf8;
  NumPart: integer;
begin
  MaxNo := Self.Server.Orm.OneFieldValue(TOrmCustomerOrder,
    'MAX(CAST(OrderNo AS INTEGER))', '', []);
  NumPart := Utf8ToInteger(MaxNo, 0);
  Inc(NumPart);
  AOrderNo := StringToUtf8(Format('%.6d', [NumPart]));
  Result := ierSuccess;
end;

{
******************************* TRgPaymentService ******************************
}
function TRgPaymentService.AddPayment(AInvoiceID: longint; AAmount: currency;
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

  Order := TOrmCustomerOrder.Create(Self.Server.Orm, AInvoiceID);
  try
    if Order.ID = 0 then
    begin
      Result := prInvoiceNotFound;
      Exit;
    end;

    NewAmountPaid := Order.AmountPaid + AAmount;
    Order.AmountPaid := NewAmountPaid;

    if Self.Server.Orm.Update(Order, 'AmountPaid') then
      Result := prSuccess;
  finally
    Order.Free;
  end;
end;

function TRgPaymentService.GetInvoiceOpenAmount(AInvoiceID: longint;
  out AOpenAmount: currency): TPaymentResult;
var
  Order: TOrmCustomerOrder;
begin
  AOpenAmount := 0;
  Result := prDatabaseError;

  Order := TOrmCustomerOrder.Create(Self.Server.Orm, AInvoiceID);
  try
    if Order.ID = 0 then
    begin
      Result := prInvoiceNotFound;
      Exit;
    end;

    AOpenAmount := Order.ItemsTotal - Order.AmountPaid;
    Result := prSuccess;
  finally
    Order.Free;
  end;
end;

{
***************************** TRgStatisticsService *****************************
}
function TRgStatisticsService.GetDashboardStats(out AStats: TDtoDashboardStats): integer;
var
  Today: Int64;
  SQL: RawUtf8;
  Table: TOrmTable;
begin
  FillCharFast(AStats, SizeOf(AStats), 0);

  // Customer count from separate table
  AStats.CustomerCount := Self.Server.Orm.TableRowCount(TOrmCustomer);

  // Combined query for all order statistics
  Today := TimeLogFromDateTime(Date);
  SQL := FormatUtf8(
    'SELECT ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal THEN 1 ELSE 0 END), ' +
    'COALESCE(SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal ' +
      'THEN ItemsTotal - COALESCE(AmountPaid, 0) ELSE 0 END), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal AND ShipDate = % THEN 1 ELSE 0 END), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal AND ShipDate < % AND ShipDate > 0 THEN 1 ELSE 0 END) ' +
    'FROM CustomerOrder', [Today, Today]);

  Table := Self.Server.Orm.ExecuteList([TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    if Table.RowCount > 0 then
    begin
      AStats.OpenItemsCount := Table.GetAsInteger(1, 0);
      AStats.OpenItemsAmount := Table.GetAsCurrency(1, 1);
      AStats.DueTodayCount := Table.GetAsInteger(1, 2);
      AStats.OverdueCount := Table.GetAsInteger(1, 3);
    end;
  finally
    Table.Free;
  end;

  Result := 0;
end;

function TRgStatisticsService.GetCustomerSummary(ACustomerID: longint;
  out ASummary: TDtoCustomerSummary): integer;
var
  Customer: TOrmCustomer;
  SQL: RawUtf8;
  Table: TOrmTable;
begin
  FillCharFast(ASummary, SizeOf(ASummary), 0);

  if ACustomerID <= 0 then
  begin
    Result := -1;
    Exit;
  end;

  // Load customer name
  Customer := TOrmCustomer.Create(Self.Server.Orm, ACustomerID);
  try
    if Customer.ID = 0 then
    begin
      Result := -1;
      Exit;
    end;
    ASummary.CustomerID := Customer.ID;
    ASummary.CustomerName := Utf8ToString(Customer.Company);
  finally
    Customer.Free;
  end;

  // Combined query for all invoice statistics
  SQL := FormatUtf8(
    'SELECT ' +
    'COUNT(*), ' +
    'COALESCE(SUM(ItemsTotal), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal THEN 1 ELSE 0 END), ' +
    'COALESCE(SUM(CASE WHEN COALESCE(AmountPaid, 0) < ItemsTotal ' +
      'THEN ItemsTotal - COALESCE(AmountPaid, 0) ELSE 0 END), 0), ' +
    'SUM(CASE WHEN COALESCE(AmountPaid, 0) >= ItemsTotal THEN 1 ELSE 0 END) ' +
    'FROM CustomerOrder WHERE Customer = %', [ACustomerID]);

  Table := Self.Server.Orm.ExecuteList([TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    if Table.RowCount > 0 then
    begin
      ASummary.InvoiceCount := Table.GetAsInteger(1, 0);
      ASummary.TotalRevenue := Table.GetAsCurrency(1, 1);
      ASummary.OpenCount := Table.GetAsInteger(1, 2);
      ASummary.OpenAmount := Table.GetAsCurrency(1, 3);
      ASummary.PaidCount := Table.GetAsInteger(1, 4);
    end;
  finally
    Table.Free;
  end;

  Result := 0;
end;

{
******************************* TRgReportService *******************************
}
function TRgReportService.GetOpenItemsReport(AFromDate, AToDate: TDateTime;
  AMinAmount: currency; out AItems: TDtoOpenItemDynArray): integer;
var
  SQL: RawUtf8;
  Table: TOrmTable;
  FromDate, ToDate: Int64;
  MinAmountInt64: Int64;
  i: integer;
  SaleDateValue: Int64;
begin
  SetLength(AItems, 0);

  FromDate := TimeLogFromDateTime(AFromDate);
  ToDate := TimeLogFromDateTime(AToDate);
  // Currency is stored as Int64 * 10000 in mORMot
  MinAmountInt64 := Trunc(AMinAmount * 10000);

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

  Table := Self.Server.Orm.ExecuteList([TOrmCustomerOrder, TOrmCustomer], SQL);
  if Table <> nil then
  try
    SetLength(AItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      AItems[i - 1].OrderID := Table.GetAsInteger(i, 0);
      AItems[i - 1].Company := Utf8ToString(Table.GetU(i, 1));
      AItems[i - 1].OrderNo := Utf8ToString(Table.GetU(i, 2));
      SaleDateValue := Table.GetAsInt64(i, 3);
      if SaleDateValue > 0 then
        AItems[i - 1].SaleDate := TimeLogToDateTime(SaleDateValue)
      else
        AItems[i - 1].SaleDate := 0;
      AItems[i - 1].ItemsTotal := Table.GetAsCurrency(i, 4);
      AItems[i - 1].OpenAmount := Table.GetAsCurrency(i, 5);
      if SaleDateValue > 0 then
        AItems[i - 1].DaysOverdue := Trunc(Date - AItems[i - 1].SaleDate)
      else
        AItems[i - 1].DaysOverdue := 0;
    end;
  finally
    Table.Free;
  end;

  Result := Length(AItems);
end;

function TRgReportService.GetPaymentReceiptsReport(AFromDate, AToDate: TDateTime;
  out AItems: TDtoPaymentReceiptDynArray): integer;
var
  SQL: RawUtf8;
  Table: TOrmTable;
  FromDate, ToDate: Int64;
  i: integer;
  SaleDateValue: Int64;
begin
  SetLength(AItems, 0);

  FromDate := TimeLogFromDateTime(AFromDate);
  ToDate := TimeLogFromDateTime(AToDate);

  SQL := FormatUtf8(
    'SELECT o.ID, o.SaleDate, c.Company, o.OrderNo, o.AmountPaid ' +
    'FROM CustomerOrder o ' +
    'INNER JOIN Customer c ON o.Customer = c.ID ' +
    'WHERE COALESCE(o.AmountPaid, 0) > 0 ' +
    'AND o.SaleDate >= % ' +
    'AND o.SaleDate <= % ' +
    'ORDER BY o.SaleDate DESC',
    [FromDate, ToDate]);

  Table := Self.Server.Orm.ExecuteList([TOrmCustomerOrder, TOrmCustomer], SQL);
  if Table <> nil then
  try
    SetLength(AItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      AItems[i - 1].OrderID := Table.GetAsInteger(i, 0);
      SaleDateValue := Table.GetAsInt64(i, 1);
      if SaleDateValue > 0 then
        AItems[i - 1].SaleDate := TimeLogToDateTime(SaleDateValue)
      else
        AItems[i - 1].SaleDate := 0;
      AItems[i - 1].Company := Utf8ToString(Table.GetU(i, 2));
      AItems[i - 1].OrderNo := Utf8ToString(Table.GetU(i, 3));
      AItems[i - 1].AmountPaid := Table.GetAsCurrency(i, 4);
    end;
  finally
    Table.Free;
  end;

  Result := Length(AItems);
end;

function TRgReportService.GetCustomerRevenueReport(AYear: integer;
  out AItems: TDtoCustomerRevenueDynArray): integer;
var
  SQL: RawUtf8;
  Table: TOrmTable;
  YearStart, YearEnd: Int64;
  StartDate, EndDate: TDateTime;
  i: integer;
begin
  SetLength(AItems, 0);

  StartDate := EncodeDate(AYear, 1, 1);
  EndDate := EncodeDate(AYear, 12, 31);
  YearStart := TimeLogFromDateTime(StartDate);
  YearEnd := TimeLogFromDateTime(EndDate);

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

  Table := Self.Server.Orm.ExecuteList([TOrmCustomer, TOrmCustomerOrder], SQL);
  if Table <> nil then
  try
    SetLength(AItems, Table.RowCount);
    for i := 1 to Table.RowCount do
    begin
      AItems[i - 1].CustomerID := Table.GetAsInteger(i, 0);
      AItems[i - 1].Company := Utf8ToString(Table.GetU(i, 1));
      AItems[i - 1].InvoiceCount := Table.GetAsInteger(i, 2);
      AItems[i - 1].TotalRevenue := Table.GetAsCurrency(i, 3);
      AItems[i - 1].TotalPaid := Table.GetAsCurrency(i, 4);
      AItems[i - 1].TotalOpen := Table.GetAsCurrency(i, 5);
    end;
  finally
    Table.Free;
  end;

  Result := Length(AItems);
end;

function TRgReportService.GetMonthlyOverviewReport(AYear: integer;
  out AItems: TDtoMonthlyOverviewDynArray; out ATotals: TDtoMonthlyOverview): integer;
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
  StartDate := EncodeDate(AYear, 1, 1);
  EndDate := EncodeDate(AYear, 12, 31) + 0.99999;
  YearStart := TimeLogFromDateTime(StartDate);
  YearEnd := TimeLogFromDateTime(EndDate);

  // Initialize 12 months
  SetLength(AItems, 12);
  for i := 1 to 12 do
  begin
    FillCharFast(AItems[i - 1], SizeOf(TDtoMonthlyOverview), 0);
    AItems[i - 1].Month := i;
    AItems[i - 1].MonthName := MonthNames[i];
  end;

  // Load all orders for the year and aggregate in Pascal
  Orders := TOrmCustomerOrder.CreateAndFillPrepare(Self.Server.Orm,
    'SaleDate >= ? AND SaleDate <= ?', [YearStart, YearEnd]);
  try
    while Orders.FillOne do
    begin
      SaleDateBits.Value := Orders.SaleDate;
      MonthNum := SaleDateBits.Month;
      if (MonthNum >= 1) and (MonthNum <= 12) then
      begin
        Inc(AItems[MonthNum - 1].InvoiceCount);
        AItems[MonthNum - 1].Revenue := AItems[MonthNum - 1].Revenue + Orders.ItemsTotal;
        AItems[MonthNum - 1].PaymentsReceived := AItems[MonthNum - 1].PaymentsReceived + Orders.AmountPaid;
        AItems[MonthNum - 1].OpenAmount := AItems[MonthNum - 1].OpenAmount +
          (Orders.ItemsTotal - Orders.AmountPaid);
      end;
    end;
  finally
    Orders.Free;
  end;

  // Calculate totals
  FillCharFast(ATotals, SizeOf(ATotals), 0);
  ATotals.MonthName := 'Total';
  for i := 0 to 11 do
  begin
    ATotals.InvoiceCount := ATotals.InvoiceCount + AItems[i].InvoiceCount;
    ATotals.Revenue := ATotals.Revenue + AItems[i].Revenue;
    ATotals.PaymentsReceived := ATotals.PaymentsReceived + AItems[i].PaymentsReceived;
    ATotals.OpenAmount := ATotals.OpenAmount + AItems[i].OpenAmount;
  end;

  Result := Length(AItems);
end;

end.
