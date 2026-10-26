/// WebSocket thread completion regression tests
// - licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.net.ws.concurrent;

interface

{$I ..\src\mormot.defines.inc}

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.test,
  mormot.net.ws.core,
  mormot.net.ws.client;

type
  TTestWebSocketThreadCompletion = class(TSynTestCase)
  published
    procedure ConcurrentCompletion;
  end;

implementation

type
  TShutdownProcess = class(TWebSocketProcessClient)
  public
    procedure SetClosed;
  end;

  TShutdownClientThread = class(TWebSocketProcessClientThread)
  public
    procedure Prepare(Process: TWebSocketProcessClient);
    procedure ExecuteOnce;
    procedure Detach;
    function State: TWebSocketProcessClientThreadState;
  end;

  TShutdownWorker = class(TThread)
  protected
    procedure Execute; override;
  public
    ClientThread: TShutdownClientThread;
    Done: integer;
    Error: string;
    constructor Create(Client: TShutdownClientThread);
  end;

procedure TShutdownProcess.SetClosed;
begin
  fState := wpsClose;
end;

procedure TShutdownClientThread.Prepare(Process: TWebSocketProcessClient);
begin
  fProcess := Process;
  fThreadState := sRun;
  Terminate;
end;

procedure TShutdownClientThread.ExecuteOnce;
begin
  DoExecute;
end;

procedure TShutdownClientThread.Detach;
begin
  fProcess := nil;
end;

function TShutdownClientThread.State: TWebSocketProcessClientThreadState;
begin
  result := fThreadState;
end;

constructor TShutdownWorker.Create(Client: TShutdownClientThread);
begin
  inherited Create(true);
  ClientThread := Client;
end;

procedure TShutdownWorker.Execute;
begin
  try
    ClientThread.ExecuteOnce;
  except
    on E: Exception do
      Error := E.ClassName + ': ' + E.Message;
  end;
  InterlockedIncrement(Done);
end;

procedure TTestWebSocketThreadCompletion.ConcurrentCompletion;
var
  client: TShutdownClientThread;
  process: TShutdownProcess;
  worker: TShutdownWorker;
  i: integer;
  deadline: Int64;
begin
  // Only the terminated execution path is used; no network owner is constructed.
  client := TShutdownClientThread(TShutdownClientThread.NewInstance);
  process := TShutdownProcess(TShutdownProcess.NewInstance);
  process.SetClosed;
  try
    for i := 1 to 128 do
    begin
      client.Prepare(process);
      worker := TShutdownWorker.Create(client);
      try
        worker.Start;
        deadline := GetTickCount64 + 5000;
        while (client.State = sRun) and
              (worker.Done = 0) and
              (GetTickCount64 < deadline) do
          SleepHiRes(0);
        Check(GetTickCount64 < deadline, 'shutdown timeout');
        if client.State <> sRun then
          client.Detach;
        worker.WaitFor;
        Check(worker.Error = '', 'process accessed after completion');
        Check(client.State = sClosed, 'closed state lost during detach');
      finally
        worker.Free;
      end;
    end;
    client.Prepare(nil);
    client.ExecuteOnce;
    Check(client.State = sFinished);
  finally
    client.FreeInstance;
    process.FreeInstance;
  end;
end;

end.
