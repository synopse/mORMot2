unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.interfaces,
  api.interfaces;

type
  /// Base calculator implementation
  TCalculatorApi = class(TInjectableObject, ICalculatorApi)
  protected
    fServerName: RawUtf8;
  public
    constructor Create(const aServerName: RawUtf8); reintroduce;
    function Divide(a, b: Integer): Double; virtual;
    function Add(a, b: Integer): Integer; virtual;
    function GetInfo: RawUtf8; virtual;
  end;

  TCalculatorApiClass = class of TCalculatorApi;

  /// Calculator for Server 1
  TCalculatorApi1 = class(TCalculatorApi)
  public
    constructor Create; reintroduce;
    function GetInfo: RawUtf8; override;
  end;

  /// Calculator for Server 2
  TCalculatorApi2 = class(TCalculatorApi)
  public
    constructor Create; reintroduce;
    function Add(a, b: Integer): Integer; override;
  end;

  /// Calculator for Server 3
  TCalculatorApi3 = class(TCalculatorApi)
  public
    constructor Create; reintroduce;
    function Divide(a, b: Integer): Double; override;
  end;

implementation

{ TCalculatorApi }

constructor TCalculatorApi.Create(const aServerName: RawUtf8);
begin
  inherited Create;
  fServerName := aServerName;
end;

function TCalculatorApi.Divide(a, b: Integer): Double;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  Result := a / b;
end;

function TCalculatorApi.Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function TCalculatorApi.GetInfo: RawUtf8;
begin
  Result := FormatUtf8('mORMot2 Server: % (DMVC port)', [fServerName]);
end;

{ TCalculatorApi1 }

constructor TCalculatorApi1.Create;
begin
  inherited Create('Server01');
end;

function TCalculatorApi1.GetInfo: RawUtf8;
begin
  Result := FormatUtf8('% - Enhanced Info Mode', [inherited GetInfo]);
end;

{ TCalculatorApi2 }

constructor TCalculatorApi2.Create;
begin
  inherited Create('Server02');
end;

function TCalculatorApi2.Add(a, b: Integer): Integer;
begin
  Result := inherited Add(a, b) * 2; // Double the result
end;

{ TCalculatorApi3 }

constructor TCalculatorApi3.Create;
begin
  inherited Create('Server03');
end;

function TCalculatorApi3.Divide(a, b: Integer): Double;
begin
  Result := inherited Divide(a, b) * 10; // Multiply result by 10
end;

end.
