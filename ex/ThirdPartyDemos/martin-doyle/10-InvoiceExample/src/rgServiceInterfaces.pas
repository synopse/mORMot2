{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgServiceInterfaces.pas

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
unit rgServiceInterfaces;

interface

{$I mormot.defines.inc}

uses
  mormot.core.base,
  mormot.core.interfaces,
  rgDtoTypes;

type

  { IRgCustomerService }

  IRgCustomerService = interface(IInvokable)
    ['{1A2B3C4D-5E6F-7A8B-9C0D-E1F2A3B4C5D6}']
    function ListCustomers(out ACustomers: TDtoCustomerDynArray): TCustomerEditResult;
    function GetCustomer(ACustomerID: longint; out ACustomer: TDtoCustomer): TCustomerEditResult;
    function CreateCustomer(const ACustomer: TDtoCustomer; out ANewID: longint): TCustomerEditResult;
    function UpdateCustomer(ACustomerID: longint; const ACustomer: TDtoCustomer): TCustomerEditResult;
    function DeleteCustomer(ACustomerID: longint): TCustomerEditResult;
    function GenerateCustomerNo(out ACustomerNo: RawUtf8): TCustomerEditResult;
  end;

  { IRgInvoiceService }

  IRgInvoiceService = interface(IInvokable)
    ['{2B3C4D5E-6F7A-8B9C-0D1E-F2A3B4C5D6E7}']
    function ListInvoicesForCustomer(ACustomerID: longint; out AInvoices: TDtoOrderDynArray): TInvoiceEditResult;
    function GetInvoice(AInvoiceID: longint; out AInvoice: TDtoInvoiceDetail): TInvoiceEditResult;
    function CreateInvoice(ACustomerID: longint; const AInvoice: TDtoInvoiceSave; out ANewID: longint): TInvoiceEditResult;
    function UpdateInvoice(AInvoiceID: longint; const AInvoice: TDtoInvoiceSave): TInvoiceEditResult;
    function DeleteInvoice(AInvoiceID: longint): TInvoiceEditResult;
    function GenerateOrderNo(out AOrderNo: RawUtf8): TInvoiceEditResult;
  end;

  { IRgPaymentService }

  IRgPaymentService = interface(IInvokable)
    ['{3C4D5E6F-7A8B-9C0D-1E2F-A3B4C5D6E7F8}']
    function AddPayment(AInvoiceID: longint; AAmount: currency; ADate: TDateTime): TPaymentResult;
    function GetInvoiceOpenAmount(AInvoiceID: longint; out AOpenAmount: currency): TPaymentResult;
  end;

  { IRgStatisticsService }

  IRgStatisticsService = interface(IInvokable)
    ['{4D5E6F7A-8B9C-0D1E-2F3A-B4C5D6E7F8A9}']
    function GetDashboardStats(out AStats: TDtoDashboardStats): integer;
    function GetCustomerSummary(ACustomerID: longint; out ASummary: TDtoCustomerSummary): integer;
  end;

  { IRgReportService }

  IRgReportService = interface(IInvokable)
    ['{5E6F7A8B-9C0D-1E2F-3A4B-C5D6E7F8A9B0}']
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

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IRgCustomerService),
    TypeInfo(IRgInvoiceService),
    TypeInfo(IRgPaymentService),
    TypeInfo(IRgStatisticsService),
    TypeInfo(IRgReportService)
  ]);

end.
