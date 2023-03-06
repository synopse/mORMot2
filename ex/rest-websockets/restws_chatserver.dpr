/// simple SOA server using callbacks for a chat room
program restws_chatserver;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // use FastMM4 on older versions of Delphi
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.net.ws.core,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  restws_chatinterface in 'restws_chatinterface.pas';

type
  TChatService = class(TInterfacedObject, IChatService)
  protected
    fConnected: array of IChatCallback;
  public
    // IChatService methods
    procedure Join(const {%H-}pseudo: string; const callback: IChatCallback);
    procedure BlaBla(const pseudo, msg: string);
    // IServiceWithCallbackReleased method
    procedure CallbackReleased(
      const callback: IInvokable; const interfaceName: RawUTF8);
  end;

procedure TChatService.Join(const pseudo: string; const callback: IChatCallback);
begin
  InterfaceArrayAdd(fConnected,callback);
end;

procedure TChatService.BlaBla(const pseudo, msg: string);
var
  i: PtrInt;
begin
  for i := high(fConnected) downto 0 do // downwards for InterfaceArrayDelete()
    try
      fConnected[i].NotifyBlaBla(pseudo,msg);
    except
      InterfaceArrayDelete(fConnected,i); // unsubscribe the callback on failure
    end;
end;

procedure TChatService.CallbackReleased(
  const callback: IInvokable; const interfaceName: RawUTF8);
begin
  if interfaceName = 'IChatCallback' then
    InterfaceArrayDelete(fConnected,callback);
end;


procedure Run;
var
  HttpServer: TRestHttpServer;
  Server: TRestServerFullMemory;
begin
  Server := TRestServerFullMemory.CreateWithOwnModel([]);
  try
    Server.CreateMissingTables;
    Server.ServiceDefine(TChatService, [IChatService], sicShared).
      SetOptions([], [optExecLockedPerInterface]). // thread-safe fConnected[]
      ByPassAuthentication := true;
    HttpServer := TRestHttpServer.Create(
      '8888', [Server], '+', WEBSOCKETS_DEFAULT_MODE);
    try
      HttpServer.WebSocketsEnable(Server, CHAT_TRANSMISSION_KEY)^.
        SetFullLog; // full verbose logs for this demo
      ConsoleWrite('WebSockets Chat Server running on localhost:8888'#13#10, ccLightGreen);
      ConsoleWrite('Please compile and run restws_chatclient'#13#10, ccWhite);
      ConsoleWrite('Press [Enter] to quit'#13#10);
      ConsoleWaitForEnterKey;
    finally
      HttpServer.Free;
    end;
  finally
    Server.Free;
  end;
end;


begin
  with TSynlog.Family do
  begin // enable logging to file and to console
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  WebSocketLog := TSynLog; // verbose log of all WebSockets activity
  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.
