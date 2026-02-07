{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgServer.pas

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
unit rgServer;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.rest.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static,
  rgData,
  rgServiceInterfaces,
  rgServiceImplementation;

type

  { TRgServer }

  TRgServer = class(TRestServerDB)
  public
    constructor Create(aModel: TOrmModel; const aDBFileName: TFileName);
        reintroduce;
  end;

implementation

{ TRgServer }

constructor TRgServer.Create(aModel: TOrmModel; const aDBFileName: TFileName);
begin
  inherited Create(aModel, aDBFileName);
  CreateMissingTables;
  ServiceDefine(TRgCustomerService, [IRgCustomerService], sicShared);
  ServiceDefine(TRgInvoiceService, [IRgInvoiceService], sicShared);
  ServiceDefine(TRgPaymentService, [IRgPaymentService], sicShared);
  ServiceDefine(TRgStatisticsService, [IRgStatisticsService], sicShared);
  ServiceDefine(TRgReportService, [IRgReportService], sicShared);
end;

end.
