unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  /// Implementation of exception handling API
  // Port of TMyController from DMVC
  TExceptionHandlingApiService = class(TInjectableObjectRest, IExceptionHandlingApi)
  public
    function Index: RawUtf8;
    function RaiseCustomError: RawUtf8;
    function RaiseStandardError: RawUtf8;
    function GetCustomer(aID: Integer): RawUtf8;
  end;

implementation

{ TExceptionHandlingApiService }

function TExceptionHandlingApiService.Index: RawUtf8;
begin
  Result := 'Exception Handling Sample - Use /RaiseCustomError or /RaiseStandardError to test';
end;

function TExceptionHandlingApiService.RaiseCustomError: RawUtf8;
begin
  // Port of TMyController.Index - raises custom exception
  raise EMyException.Create(
    'My Custom Error',
    Fatal,
    25,
    'some real problem',
    'Ensure Patient resource is valid',
    'Patient/Identifier/value'
  );
end;

function TExceptionHandlingApiService.RaiseStandardError: RawUtf8;
begin
  // Port of TMyController.Error - raises standard exception
  raise Exception.Create('Standard Error');
end;

function TExceptionHandlingApiService.GetCustomer(aID: Integer): RawUtf8;
begin
  // Port of TMyController.GetCustomer - returns empty result (HTTP 204 equivalent)
  Result := '';
end;

end.
