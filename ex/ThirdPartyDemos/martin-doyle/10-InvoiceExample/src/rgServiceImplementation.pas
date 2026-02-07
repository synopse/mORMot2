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
begin
  SetLength(AInvoices, 0);
  Result := ierDatabaseError; // TODO: implement in B.3.1
end;

function TRgInvoiceService.GetInvoice(AInvoiceID: longint;
  out AInvoice: TDtoInvoiceDetail): TInvoiceEditResult;
begin
  Result := ierDatabaseError; // TODO: implement in B.3.2
end;

function TRgInvoiceService.CreateInvoice(ACustomerID: longint;
  const AInvoice: TDtoInvoiceSave; out ANewID: longint): TInvoiceEditResult;
begin
  ANewID := 0;
  Result := ierDatabaseError; // TODO: implement in B.3.3
end;

function TRgInvoiceService.UpdateInvoice(AInvoiceID: longint;
  const AInvoice: TDtoInvoiceSave): TInvoiceEditResult;
begin
  Result := ierDatabaseError; // TODO: implement in B.3.4
end;

function TRgInvoiceService.DeleteInvoice(AInvoiceID: longint): TInvoiceEditResult;
begin
  Result := ierDatabaseError; // TODO: implement in B.3.5
end;

function TRgInvoiceService.GenerateOrderNo(out AOrderNo: RawUtf8): TInvoiceEditResult;
begin
  AOrderNo := '';
  Result := ierDatabaseError; // TODO: implement in B.3.6
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
begin
  Result := -1; // TODO: implement in B.4.1
end;

function TRgStatisticsService.GetCustomerSummary(ACustomerID: longint;
  out ASummary: TDtoCustomerSummary): integer;
begin
  Result := -1; // TODO: implement in B.4.2
end;

{
******************************* TRgReportService *******************************
}
function TRgReportService.GetOpenItemsReport(AFromDate, AToDate: TDateTime;
  AMinAmount: currency; out AItems: TDtoOpenItemDynArray): integer;
begin
  SetLength(AItems, 0);
  Result := -1; // TODO: implement in B.5.1
end;

function TRgReportService.GetPaymentReceiptsReport(AFromDate, AToDate: TDateTime;
  out AItems: TDtoPaymentReceiptDynArray): integer;
begin
  SetLength(AItems, 0);
  Result := -1; // TODO: implement in B.5.2
end;

function TRgReportService.GetCustomerRevenueReport(AYear: integer;
  out AItems: TDtoCustomerRevenueDynArray): integer;
begin
  SetLength(AItems, 0);
  Result := -1; // TODO: implement in B.5.3
end;

function TRgReportService.GetMonthlyOverviewReport(AYear: integer;
  out AItems: TDtoMonthlyOverviewDynArray; out ATotals: TDtoMonthlyOverview): integer;
begin
  SetLength(AItems, 0);
  Result := -1; // TODO: implement in B.5.4
end;

end.
