/// concurrent service-list regression tests
// - licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.soa.concurrent;

interface

{$I ..\src\mormot.defines.inc}

uses
  SysUtils,
  Classes,
  SyncObjs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.core.test,
  mormot.soa.core;

type
  TTestServiceListConcurrent = class(TSynTestCase)
  published
    procedure CacheInvalidation;
    procedure ConcurrentFirstUse;
  end;

implementation

type
  IListProbe = interface(IInvokable)
    ['{ED63F35F-2468-46B9-8E40-426DF893463A}']
    function Value: integer;
  end;

  TListProbeFactory = class(TServiceFactory)
  public
    constructor Create(const Name: RawUtf8); reintroduce;
  end;

  TListProbeContainer = class(TServiceContainer)
  public
    procedure Add(const Name: RawUtf8);
  end;

  TListProbeThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Container: TServiceContainer;
    Gate: TEvent;
    Value: variant;
    Error: string;
    constructor Create(aContainer: TServiceContainer; aGate: TEvent);
  end;

constructor TListProbeFactory.Create(const Name: RawUtf8);
begin
  // AsSoa needs metadata only, without a resolver.
  fInterface := TInterfaceFactory.Get(TypeInfo(IListProbe));
  fInterfaceUri := Name;
  fContractExpected := '"contract"';
  fInstanceCreation := sicShared;
end;

procedure TListProbeContainer.Add(const Name: RawUtf8);
begin
  AddServiceInternal(TListProbeFactory.Create(Name));
end;

constructor TListProbeThread.Create(aContainer: TServiceContainer; aGate: TEvent);
begin
  inherited Create(true);
  Container := aContainer;
  Gate := aGate;
end;

procedure TListProbeThread.Execute;
begin
  try
    if Gate.WaitFor(5000) <> wrSignaled then
      raise Exception.Create('Service-list barrier timeout');
    Value := Container.AsSoa;
  except
    on E: Exception do
      Error := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TTestServiceListConcurrent.CacheInvalidation;
var
  container: TListProbeContainer;
  first, second: variant;
begin
  container := TListProbeContainer.Create(nil);
  try
    container.Add('One');
    first := container.AsSoa;
    container.Add('Two');
    second := container.AsSoa;
    CheckEqual(_Safe(first)^.Count, 3);
    CheckEqual(_Safe(second)^.Count, 6, 'new service absent from cached list');
  finally
    container.Free;
  end;
end;

procedure TTestServiceListConcurrent.ConcurrentFirstUse;
const
  THREADS = 8;
  SERVICES = 256;
var
  container: TListProbeContainer;
  workers: array[0..THREADS - 1] of TListProbeThread;
  gate: TEvent;
  i: integer;
begin
  container := TListProbeContainer.Create(nil);
  gate := TEvent.Create(nil, true, false, '');
  FillChar(workers, SizeOf(workers), 0);
  try
    for i := 1 to SERVICES do
      container.Add('Service' + UInt32ToUtf8(i));
    for i := 0 to high(workers) do
    begin
      workers[i] := TListProbeThread.Create(container, gate);
      workers[i].Start;
    end;
    gate.SetEvent;
    for i := 0 to high(workers) do
    begin
      workers[i].WaitFor;
      Check(workers[i].Error = '');
      CheckEqual(_Safe(workers[i].Value)^.Count, SERVICES * 3);
    end;
  finally
    gate.SetEvent;
    for i := 0 to high(workers) do
      workers[i].Free;
    gate.Free;
    container.Free;
  end;
end;

end.
