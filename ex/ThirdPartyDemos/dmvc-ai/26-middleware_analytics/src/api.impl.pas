unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.text,
  mormot.rest.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Implementation of IAnalyticsApi
  // Port of DMVC TMainController
  TAnalyticsApiService = class(TInjectableObjectRest, IAnalyticsApi)
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

{ TAnalyticsApiService }

function TAnalyticsApiService.Index: RawJson;
begin
  // Port of DMVC: Render(StrDict(['hello'],['world']))
  Result := JsonEncode(['hello', 'world']);
end;

function TAnalyticsApiService.GetReversedString(const aValue: RawUtf8): RawUtf8;
var
  i: PtrInt;
  len: PtrInt;
begin
  // Port of DMVC: Render(System.StrUtils.ReverseString(Value.Trim))
  len := length(aValue);
  SetLength(Result, len);
  for i := 1 to len do
    Result[i] := aValue[len - i + 1];
end;

procedure TAnalyticsApiService.DoError;
begin
  // Port of DMVC: raise EMVCException.Create('This is an error')
  ERestException.RaiseUtf8('This is an error', []);
end;

function TAnalyticsApiService.GetCustomers: RawJson;
begin
  // Port of DMVC: todo: render a list of customers
  Result := JsonEncode(['message', 'GetCustomers - TODO: render a list of customers']);
end;

function TAnalyticsApiService.GetCustomer(aId: Integer): RawJson;
begin
  // Port of DMVC: todo: render the customer by id
  Result := JsonEncode(['message', FormatUtf8('GetCustomer(%) - TODO: render the customer by id', [aId])]);
end;

function TAnalyticsApiService.CreateCustomer(const aCustomerData: RawJson): Integer;
begin
  // Port of DMVC: todo: create a new customer
  Result := 0;  // Would return new customer ID
end;

procedure TAnalyticsApiService.UpdateCustomer(aId: Integer; const aCustomerData: RawJson);
begin
  // Port of DMVC: todo: update customer by id
end;

procedure TAnalyticsApiService.DeleteCustomer(aId: Integer);
begin
  // Port of DMVC: todo: delete customer by id
end;

end.
