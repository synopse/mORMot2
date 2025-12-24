unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.core.os,
  api.interfaces;

type
  /// Implementation of IServiceApi
  TServiceApi = class(TInjectableObject, IServiceApi)
  private
    fStartTime: Int64;
    fPort: Integer;
  public
    constructor Create(aPort: Integer); reintroduce;
    function GetStatus: TServiceStatus;
    function Echo(const aMessage: RawUtf8): RawUtf8;
  end;

implementation

{ TServiceApi }

constructor TServiceApi.Create(aPort: Integer);
begin
  inherited Create;
  fPort := aPort;
  fStartTime := GetTickCount64;
end;

function TServiceApi.GetStatus: TServiceStatus;
begin
  Result.Running := True;
  Result.Uptime := (GetTickCount64 - fStartTime) div 1000;
  Result.Port := fPort;
  Result.Message := StringToUtf8(Format('Service running for %d seconds', [Result.Uptime]));
end;

function TServiceApi.Echo(const aMessage: RawUtf8): RawUtf8;
begin
  Result := FormatUtf8('Echo: %', [aMessage]);
end;

end.
