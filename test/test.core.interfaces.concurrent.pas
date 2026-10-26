/// concurrent interface VMT regression tests
// - licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.interfaces.concurrent;

interface

{$I ..\src\mormot.defines.inc}

uses
  SysUtils,
  Classes,
  SyncObjs,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.test;

type
  TTestInterfaceVmtConcurrent = class(TSynTestCase)
  published
    procedure ConcurrentVmtCreation;
  end;

implementation

type
  IVmtProbe = interface(IInvokable)
    ['{023CDF52-F2AF-457B-A79E-D75B8A740196}']
    function Add(A, B: integer): integer;
  end;

  TVmtProbeFactory = class(TInterfaceFactory) // protected accessor
  public
    function Table: PPointerArray;
  end;

  TVmtProbeThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Factory: TVmtProbeFactory;
    Gate: TEvent;
    Vmt: PPointerArray;
    Complete: boolean;
    Error: string;
    constructor Create(aFactory: TVmtProbeFactory; aGate: TEvent);
  end;

function TVmtProbeFactory.Table: PPointerArray;
begin
  result := GetMethodsVirtualTable;
end;

constructor TVmtProbeThread.Create(aFactory: TVmtProbeFactory; aGate: TEvent);
begin
  inherited Create(true);
  Factory := aFactory;
  Gate := aGate;
end;

procedure TVmtProbeThread.Execute;
var
  i: integer;
begin
  try
    if Gate.WaitFor(5000) <> wrSignaled then
      raise Exception.Create('VMT barrier timeout');
    Vmt := Factory.Table;
    Complete := Vmt <> nil;
    if Complete then
      for i := 0 to 3 do
        if Vmt^[i] = nil then
          Complete := false;
  except
    on E: Exception do
      Error := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TTestInterfaceVmtConcurrent.ConcurrentVmtCreation;
const
  THREADS = 32;
var
  workers: array[0..THREADS - 1] of TVmtProbeThread;
  gate: TEvent;
  factory: TVmtProbeFactory;
  i: integer;
begin
  factory := TVmtProbeFactory(TInterfaceFactory.Get(TypeInfo(IVmtProbe)));
  gate := TEvent.Create(nil, true, false, '');
  FillChar(workers, SizeOf(workers), 0);
  try
    for i := 0 to high(workers) do
    begin
      workers[i] := TVmtProbeThread.Create(factory, gate);
      workers[i].Start;
    end;
    gate.SetEvent;
    for i := 0 to high(workers) do
    begin
      workers[i].WaitFor;
      Check(workers[i].Error = '');
      Check(workers[i].Complete, 'incomplete VMT returned');
      Check(workers[i].Vmt = workers[0].Vmt);
    end;
  finally
    gate.SetEvent;
    for i := 0 to high(workers) do
      workers[i].Free;
    gate.Free;
  end;
end;

end.
