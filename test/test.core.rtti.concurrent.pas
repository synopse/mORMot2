/// concurrent RTTI registration regression tests
// - licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.rtti.concurrent;

interface

{$I ..\src\mormot.defines.inc}

uses
  SysUtils,
  Classes,
  SyncObjs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.test;

type
  TTestRttiConcurrent = class(TSynTestCase)
  published
    procedure RegistrationPublication;
  end;

implementation

type
  TRegistrationItem = packed record
    Value: RawUtf8;
  end;
  TRegistrationItems = array of TRegistrationItem;

  TRegistrationObject = class(TPersistent)
  private
    fValue: integer;
  published
    property Value: integer read fValue write fValue;
  end;

  TRegistrationRtti = class(TRttiJson)
  protected
    function SetParserType(aParser: TRttiParserType;
      aParserComplex: TRttiParserComplexType): TRttiCustom; override;
  public
    procedure FromRtti(aInfo: PRttiInfo); override;
  end;

  TRegistrationThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Registry: TRttiCustomList;
    Info: PRttiInfo;
    Custom: TRttiCustom;
    Parser: TRttiParserType;
    ItemRtti: TRttiCustom;
    Error: string;
    Started, Done: TEvent;
    constructor Create(aRegistry: TRttiCustomList; aInfo: PRttiInfo);
    destructor Destroy; override;
  end;

var
  RegistrationEntered, RegistrationRelease: TEvent;
  RegistrationTarget: PRttiInfo;

procedure PauseRegistration(Info: PRttiInfo);
begin
  if Info <> RegistrationTarget then
    exit;
  RegistrationEntered.SetEvent;
  if RegistrationRelease.WaitFor(5000) <> wrSignaled then
    raise Exception.Create('RTTI registration barrier timeout');
end;

procedure TRegistrationRtti.FromRtti(aInfo: PRttiInfo);
begin
  if aInfo = TypeInfo(TRegistrationItems) then
    PauseRegistration(aInfo);
  inherited FromRtti(aInfo);
end;

function TRegistrationRtti.SetParserType(aParser: TRttiParserType;
  aParserComplex: TRttiParserComplexType): TRttiCustom;
begin
  if Info = TypeInfo(TRegistrationObject) then
    PauseRegistration(Info);
  result := inherited SetParserType(aParser, aParserComplex);
end;

constructor TRegistrationThread.Create(aRegistry: TRttiCustomList; aInfo: PRttiInfo);
begin
  inherited Create(true);
  Registry := aRegistry;
  Info := aInfo;
  Started := TEvent.Create(nil, true, false, '');
  Done := TEvent.Create(nil, true, false, '');
end;

destructor TRegistrationThread.Destroy;
begin
  inherited Destroy;
  Done.Free;
  Started.Free;
end;

procedure TRegistrationThread.Execute;
begin
  Started.SetEvent;
  try
    Custom := Registry.RegisterType(Info);
    Parser := Custom.Parser;
    ItemRtti := Custom.ArrayRtti;
  except
    on E: Exception do
      Error := E.ClassName + ': ' + E.Message;
  end;
  Done.SetEvent;
end;

procedure TTestRttiConcurrent.RegistrationPublication;
var
  registry: TRttiCustomList;
  a, b: TRegistrationThread;
  early: boolean;
  kind: integer;
begin
  for kind := 0 to 1 do
  begin
    if kind = 0 then
      RegistrationTarget := TypeInfo(TRegistrationItems)
    else
      RegistrationTarget := TypeInfo(TRegistrationObject);
    registry := TRttiCustomList.Create;
    registry.GlobalClass := TRegistrationRtti; // reuses primitive RTTI
    RegistrationEntered := TEvent.Create(nil, true, false, '');
    RegistrationRelease := TEvent.Create(nil, true, false, '');
    a := TRegistrationThread.Create(registry, RegistrationTarget);
    b := TRegistrationThread.Create(registry, RegistrationTarget);
    try
      a.Start;
      Check(RegistrationEntered.WaitFor(5000) = wrSignaled);
      b.Start;
      Check(b.Started.WaitFor(5000) = wrSignaled);
      early := b.Done.WaitFor(100) = wrSignaled;
      RegistrationRelease.SetEvent;
      a.WaitFor;
      b.WaitFor;
      Check(a.Error = '', 'first registration');
      Check(b.Error = '', 'second registration');
      Check(not early, 'RegisterType returned before initialization');
      Check(a.Custom = b.Custom);
      Check(b.Parser <> ptNone, 'parser not initialized');
      if kind = 0 then
        Check(b.ItemRtti <> nil, 'array item RTTI not initialized');
    finally
      RegistrationRelease.SetEvent;
      a.Free;
      b.Free;
      {$ifndef NOPATCHVMT}
      if kind = 1 then
        PatchPointer(PPtrUInt(PAnsiChar(TRegistrationObject) + vmtAutoTable), 0);
      {$endif NOPATCHVMT}
      registry.Free;
      RegistrationRelease.Free;
      RegistrationEntered.Free;
    end;
  end;
end;

initialization
  Rtti.RegisterFromText(TypeInfo(TRegistrationItem), 'Value RawUtf8');

end.
