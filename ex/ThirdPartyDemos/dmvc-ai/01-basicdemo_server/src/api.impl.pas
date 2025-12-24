unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.orm.core,
  api.interfaces,
  entities;

type
  /// implement the Basic Demo API
  // Ports DMVC App1MainController to mORMot2
  TBasicDemoApi = class(TInterfacedObject, IBasicDemoApi)
  protected
    fRest: IRestOrm;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // IBasicDemoApi methods
    function HelloWorld: THelloWorldResponse;
    function HelloWorldPost(const data: RawUtf8): TPostRequestDTO;
    function Divide(par1, par2: integer): TDivisionResult;
  end;


implementation


{ TBasicDemoApi }

constructor TBasicDemoApi.Create(const aRest: IRestOrm);
begin
  fRest := aRest;
end;

function TBasicDemoApi.HelloWorld: THelloWorldResponse;
begin
  // Port of DMVC HelloWorld method
  // Original: Render('Hello World! It''s ' + TimeToStr(Time) + ' in the DMVCFramework Land!');
  result.Message := StringToUtf8('Hello World! It''s ' + TimeToStr(Time) + ' in the mORMot2 Land!');
  result.Time := StringToUtf8(TimeToStr(Time));

  TSynLog.Add.Log(sllInfo, 'HelloWorld called');
end;

function TBasicDemoApi.HelloWorldPost(const data: RawUtf8): TPostRequestDTO;
var
  doc: TDocVariantData;
begin
  // Port of DMVC HelloWorldPost method
  // Original: reads JSON from request body, adds 'modified' field, returns JSON
  TSynLog.Add.Log(sllInfo, 'HelloWorldPost called with data: %', [data]);

  doc.InitJson(data, JSON_FAST);

  result.Data := data;
  result.Modified := 'from server';

  TSynLog.Add.Log(sllInfo, 'HelloWorldPost returning modified response');
end;

function TBasicDemoApi.Divide(par1, par2: integer): TDivisionResult;
begin
  // Port of DMVC RaiseException method (renamed to Divide)
  // Original: Render(StrDict().Add('result', FloatToStr(par1 / par2, lFS)));
  TSynLog.Add.Log(sllInfo, 'Divide called: Parameter1=%, Parameter2=%', [par1, par2]);

  if par2 = 0 then
    raise EDivByZero.Create('Division by zero');

  result.Result := par1 / par2;

  TSynLog.Add.Log(sllInfo, 'Divide result: %', [result.Result]);
end;


end.
