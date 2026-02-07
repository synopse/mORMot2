{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgDtoTypes.pas

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
unit rgDtoTypes;

interface

{$I mormot.defines.inc}

uses
  Classes;

type
  TInvoiceStatus = (isPaid, isOpen, isOverdue);

  TCustomerEditResult = (cerSuccess, cerNotFound, cerMissingField, cerHasReferences, cerDatabaseError);
  TInvoiceEditResult = (ierSuccess, ierNotFound, ierMissingField, ierDatabaseError);
  TPaymentResult = (prSuccess, prInvoiceNotFound, prInvalidAmount, prDatabaseError);

  PDtoInvoiceItem = ^TDtoInvoiceItem;

  TDtoInvoiceItem = packed record
    Position: integer;
    PartNo: string;
    Description: string;
    Quantity: double;
    ListPrice: currency;
    Discount: integer;
    Amount: currency;
  end;

  TDtoInvoiceItemArray = array of TDtoInvoiceItem;

  PDtoCustomer = ^TDtoCustomer;

  TDtoCustomer = packed record
    CustomerID: longint;
    CustomerNo: string;
    Company: string;
    Phone: string;
    Fax: string;
    Address: string;
    Zip: string;
    City: string;
    Country: string;
  end;

  PDtoContact = ^TDtoContact;

  TDtoContact = packed record
    FirstName: string;
    MiddleName: string;
    LastName: string;
    Phone: string;
    Fax: string;
    Address: string;
    Zip: string;
    City: string;
    Country: string;
  end;

  PDtoOrder = ^TDtoOrder;

  TDtoOrder = packed record
    OrderID: longint;
    OrderNo: string;
    SaleDate: TDateTime;
    ShipDate: TDateTime;
    ItemsTotal: currency;
    AmountPaid: currency;
    OpenAmount: currency;
    Status: TInvoiceStatus;
    CustomerNo: string;
    Company: string;
    ShipAddress: string;
    ShipZip: string;
    ShipCity: string;
    ShipCountry: string;
  end;

  // Report DTOs

  PDtoOpenItem = ^TDtoOpenItem;

  TDtoOpenItem = packed record
    OrderID: longint;
    Company: string;
    OrderNo: string;
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
    Company: string;
    OrderNo: string;
    AmountPaid: currency;
  end;

  TDtoPaymentReceiptArray = array of TDtoPaymentReceipt;

  PDtoCustomerRevenue = ^TDtoCustomerRevenue;

  TDtoCustomerRevenue = packed record
    CustomerID: longint;
    Company: string;
    InvoiceCount: integer;
    TotalRevenue: currency;
    TotalPaid: currency;
    TotalOpen: currency;
  end;

  TDtoCustomerRevenueArray = array of TDtoCustomerRevenue;

  PDtoMonthlyOverview = ^TDtoMonthlyOverview;

  TDtoMonthlyOverview = packed record
    Month: integer;
    MonthName: string;
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
    OrderNo: string;
    SaleDate: TDateTime;
    ShipDate: TDateTime;
    CustomerID: longint;
    CustomerName: string;
    ItemsTotal: currency;
    AmountPaid: currency;
    OpenAmount: currency;
    Status: TInvoiceStatus;
    Items: TDtoInvoiceItemArray;
  end;

  PDtoInvoiceSave = ^TDtoInvoiceSave;

  TDtoInvoiceSave = packed record
    OrderNo: string;
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
  end;

  PDtoCustomerSummary = ^TDtoCustomerSummary;

  TDtoCustomerSummary = packed record
    CustomerID: longint;
    CustomerName: string;
    InvoiceCount: integer;
    TotalRevenue: currency;
    OpenCount: integer;
    OpenAmount: currency;
    PaidCount: integer;
  end;

implementation

end.
