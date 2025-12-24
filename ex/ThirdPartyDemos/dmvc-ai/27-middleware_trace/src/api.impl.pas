unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.text,
  mormot.core.variants,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  TTraceApiService = class(TInjectableObjectRest, ITraceApi)
  public
    function Index: RawJson;
    function GetReversedString(const aValue: RawUtf8): RawUtf8;
    procedure DoError;
    function GetCustomers: RawJson;
    function GetCustomer(aId: Integer): RawJson;
    function CreateCustomer(const aCustomerData: RawJson): Integer;
    procedure UpdateCustomer(aId: Integer; const aCustomerData: RawJson);
    procedure DeleteCustomer(aId: Integer);
  end;

implementation

{ TTraceApiService }

function TTraceApiService.Index: RawJson;
begin
  Result := JsonEncode(['message', 'Hello from trace middleware sample!']);
end;

function TTraceApiService.GetReversedString(const aValue: RawUtf8): RawUtf8;
var
  i: PtrInt;
  len: PtrInt;
begin
  len := length(aValue);
  SetLength(Result, len);
  for i := 1 to len do
    Result[i] := aValue[len - i + 1];
end;

procedure TTraceApiService.DoError;
begin
  raise ERestException.CreateUtf8('This is an intentional error for tracing', []);
end;

function TTraceApiService.GetCustomers: RawJson;
begin
  Result := JsonEncode(['customers', _ObjFast(['id', 1, 'name', 'Customer 1'])]);
end;

function TTraceApiService.GetCustomer(aId: Integer): RawJson;
begin
  Result := JsonEncode(['customer', _ObjFast(['id', aId, 'name', FormatUtf8('Customer %', [aId])])]);
end;

function TTraceApiService.CreateCustomer(const aCustomerData: RawJson): Integer;
begin
  Result := 123; // Mock ID
end;

procedure TTraceApiService.UpdateCustomer(aId: Integer; const aCustomerData: RawJson);
begin
  // Mock implementation
end;

procedure TTraceApiService.DeleteCustomer(aId: Integer);
begin
  // Mock implementation
end;

end.
