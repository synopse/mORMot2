{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgClient.pas

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
unit rgClient;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.orm.core, mormot.rest.core, mormot.soa.core,
  mormot.rest.http.client,
  rgData, rgServiceInterfaces, rgServer;

type

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

implementation

uses
  rgConst;

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

initialization
  case RunMode of
    rmLocal:
      RgServices := TRgServiceClient.CreateLocal;
    rmService:
      RgServices := TRgServiceClient.CreateService(
        HttpHost, HttpPort);
  end;

finalization
  FreeAndNil(RgServices);

end.
