{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgDtoTypes.pas

  Last modified
    Date : 09.02.2026
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
unit rgDtoTypes;

interface

{$I mormot.defines.inc}

uses
  mormot.core.base,
  mormot.core.rtti;

type
  TInvoiceStatus = (isPaid, isOpen, isOverdue);

  TCustomerEditResult = (cerSuccess, cerNotFound, cerMissingField, cerHasReferences, cerDatabaseError);
  TInvoiceEditResult = (ierSuccess, ierNotFound, ierMissingField, ierDatabaseError);
  TPaymentResult = (prSuccess, prInvoiceNotFound, prInvalidAmount, prDatabaseError);

  PDtoInvoiceItem = ^TDtoInvoiceItem;

  TDtoInvoiceItem = packed record
    Position: integer;
    PartNo: RawUtf8;
    Description: RawUtf8;
    Quantity: double;
    ListPrice: currency;
    Discount: integer;
    Amount: currency;
  end;

  TDtoInvoiceItemArray = array of TDtoInvoiceItem;

  PDtoCustomer = ^TDtoCustomer;

  TDtoCustomer = packed record
    CustomerID: longint;
    CustomerNo: RawUtf8;
    Company: RawUtf8;
    Phone: RawUtf8;
    Fax: RawUtf8;
    Address: RawUtf8;
    Zip: RawUtf8;
    City: RawUtf8;
    Country: RawUtf8;
  end;

  PDtoContact = ^TDtoContact;

  TDtoContact = packed record
    FirstName: RawUtf8;
    MiddleName: RawUtf8;
    LastName: RawUtf8;
    Phone: RawUtf8;
    Fax: RawUtf8;
    Address: RawUtf8;
    Zip: RawUtf8;
    City: RawUtf8;
    Country: RawUtf8;
  end;

  PDtoOrder = ^TDtoOrder;

  TDtoOrder = packed record
    OrderID: longint;
    OrderNo: RawUtf8;
    SaleDate: TDateTime;
    ShipDate: TDateTime;
    ItemsTotal: currency;
    AmountPaid: currency;
    OpenAmount: currency;
    Status: TInvoiceStatus;
    CustomerNo: RawUtf8;
    Company: RawUtf8;
    ShipAddress: RawUtf8;
    ShipZip: RawUtf8;
    ShipCity: RawUtf8;
    ShipCountry: RawUtf8;
  end;

  // Report DTOs

  PDtoOpenItem = ^TDtoOpenItem;

  TDtoOpenItem = packed record
    OrderID: longint;
    Company: RawUtf8;
    OrderNo: RawUtf8;
    SaleDate: TDateTime;
    ItemsTotal: currency;
    OpenAmount: currency;
    DaysOverdue: integer;
  end;

  TDtoOpenItemArray = array of TDtoOpenItem;

  PDtoPaymentReceipt = ^TDtoPaymentReceipt;

  TDtoPaymentReceipt = packed record
    OrderID: longint;
    SaleDate: TDateTime;
    Company: RawUtf8;
    OrderNo: RawUtf8;
    AmountPaid: currency;
  end;

  TDtoPaymentReceiptArray = array of TDtoPaymentReceipt;

  PDtoCustomerRevenue = ^TDtoCustomerRevenue;

  TDtoCustomerRevenue = packed record
    CustomerID: longint;
    Company: RawUtf8;
    InvoiceCount: integer;
    TotalRevenue: currency;
    TotalPaid: currency;
    TotalOpen: currency;
  end;

  TDtoCustomerRevenueArray = array of TDtoCustomerRevenue;

  PDtoMonthlyOverview = ^TDtoMonthlyOverview;

  TDtoMonthlyOverview = packed record
    Month: integer;
    MonthName: RawUtf8;
    InvoiceCount: integer;
    Revenue: currency;
    PaymentsReceived: currency;
    OpenAmount: currency;
  end;

  TDtoMonthlyOverviewArray = array of TDtoMonthlyOverview;

  // SOA dynamic array types (mORMot convention)
  TDtoCustomerDynArray = array of TDtoCustomer;
  TDtoOrderDynArray = array of TDtoOrder;
  TDtoOpenItemDynArray = array of TDtoOpenItem;
  TDtoPaymentReceiptDynArray = array of TDtoPaymentReceipt;
  TDtoCustomerRevenueDynArray = array of TDtoCustomerRevenue;
  TDtoMonthlyOverviewDynArray = array of TDtoMonthlyOverview;

  // SOA DTO types

  PDtoInvoiceDetail = ^TDtoInvoiceDetail;

  TDtoInvoiceDetail = packed record
    OrderID: longint;
    OrderNo: RawUtf8;
    SaleDate: TDateTime;
    ShipDate: TDateTime;
    CustomerID: longint;
    CustomerName: RawUtf8;
    ItemsTotal: currency;
    AmountPaid: currency;
    OpenAmount: currency;
    Status: TInvoiceStatus;
    Items: TDtoInvoiceItemArray;
  end;

  PDtoInvoiceSave = ^TDtoInvoiceSave;

  TDtoInvoiceSave = packed record
    OrderNo: RawUtf8;
    SaleDate: TDateTime;
    ShipDate: TDateTime;
    Items: TDtoInvoiceItemArray;
  end;

  PDtoDashboardStats = ^TDtoDashboardStats;

  TDtoDashboardStats = packed record
    CustomerCount: integer;
    OpenItemsCount: integer;
    OpenItemsAmount: currency;
    DueTodayCount: integer;
    OverdueCount: integer;
    Timestamp: RawUtf8;
  end;

  PDtoCustomerSummary = ^TDtoCustomerSummary;

  TDtoCustomerSummary = packed record
    CustomerID: longint;
    CustomerName: RawUtf8;
    InvoiceCount: integer;
    TotalRevenue: currency;
    OpenCount: integer;
    OpenAmount: currency;
    PaidCount: integer;
  end;

implementation

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
    'Status: byte; CustomerNo: RawUtf8; Company: RawUtf8; ' +
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
    'AmountPaid: currency; OpenAmount: currency; Status: byte; ' +
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

end.
