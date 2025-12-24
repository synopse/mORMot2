unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Implementation of exception handling API with controller-level handling
  // Port of TMyController from DMVC with OnException override
  // Note: In mORMot2, service-level exception handling is done via
  // ServiceRunningContext.{Error,Warning,Success} within methods
  TExceptionHandlingApiService = class(TInjectableObjectRest, IExceptionHandlingApi)
  public
    function Index: RawUtf8;
    function RaiseServiceError: RawUtf8;
    function RaiseStandardError: RawUtf8;
    function GetCustomer(aID: Integer): RawUtf8;
  end;

implementation

{ TExceptionHandlingApiService }

function TExceptionHandlingApiService.Index: RawUtf8;
begin
  Result := 'Exception Handling Sample - Use /RaiseServiceError or /RaiseStandardError to test';
end;

function TExceptionHandlingApiService.RaiseServiceError: RawUtf8;
begin
  // Port of TMyController with OnException
  // In mORMot2, service-level exception handling is done at factory level
  // This method just raises the exception
  raise EServiceException.CreateUtf8('My Custom Error (HTTP %)', [HTTP_SERVERERROR]);
end;

function TExceptionHandlingApiService.RaiseStandardError: RawUtf8;
begin
  // Port of TMyController.Error
  // This method just raises the exception
  raise Exception.Create('Standard Error');
end;

function TExceptionHandlingApiService.GetCustomer(aID: Integer): RawUtf8;
begin
  // Port of TMyController.GetCustomer - returns empty result
  Result := '';
end;

end.
