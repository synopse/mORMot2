unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Implementation of the middleware demonstration API
  // Port of DMVC TApp1MainController
  TMiddlewareApiService = class(TInjectableObjectRest, IMiddlewareApi)
  public
    function Index: RawUtf8;
    function ShowHeaders: RawUtf8;
    function Echo(const msg: RawUtf8): RawUtf8;
  end;

implementation

uses
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  mormot.rest.server;

{ TMiddlewareApiService }

function TMiddlewareApiService.Index: RawUtf8;
begin
  // Port of DMVC controller returning repeated characters
  // Middleware will add custom headers to this response
  Result := StringOfChar('*', 1024);

  TSynLog.Add.Log(sllTrace, 'Index: Returning % bytes of data', [Length(Result)]);
end;

function TMiddlewareApiService.ShowHeaders: RawUtf8;
begin
  // Show that middleware can inspect request headers
  // Note: In mORMot2 interface-based services, headers are accessible via middleware OnBeforeUri
  Result := 'Middleware can inspect request headers (check server logs for details)';

  TSynLog.Add.Log(sllTrace, 'ShowHeaders: This demonstrates middleware header inspection capability');
end;

function TMiddlewareApiService.Echo(const msg: RawUtf8): RawUtf8;
begin
  Result := FormatUtf8('Echo: %', [msg]);
  TSynLog.Add.Log(sllTrace, 'Echo: msg=%', [msg]);
end;

end.
