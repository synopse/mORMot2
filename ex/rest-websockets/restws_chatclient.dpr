/// simple SOA client using callbacks for a chat room
program restws_chatclient;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // use FastMM4 on older versions of Delphi
  SysUtils,
  Classes,
  mormot.core.text,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.client,
  mormot.rest.http.client,
  restws_chatinterface in 'restws_chatinterface.pas';

type
  TChatCallback = class(TInterfacedCallback, IChatCallback)
  protected
    procedure NotifyBlaBla(const pseudo, msg: string);
  end;

{$I-} // for write/writeln below

procedure TChatCallback.NotifyBlaBla(const pseudo, msg: string);
begin
  TextColor(ccLightBlue);
  writeln(#13'@',pseudo,' ',msg);
  TextColor(ccLightGray);
  write('>');
end;


procedure Run;
var
  Client: TRestHttpClientWebsockets;
  pseudo, msg: string;
  Service: IChatService;
  callback: IChatCallback;
begin
  writeln('Connecting to the local Websockets server...');
  Client := TRestHttpClientWebsockets.Create('127.0.0.1', '8888', TSQLModel.Create([]));
  try
    Client.Model.Owner := Client;
    Client.WebSocketsUpgrade(CHAT_TRANSMISSION_KEY);
    if not Client.ServerTimeStampSynchronize then
      raise EServiceException.Create(
        'Error connecting to the server: please run Project31ChatServer.exe');
    Client.ServiceDefine([IChatService], sicShared);
    if not Client.Services.Resolve(IChatService, Service) then
      raise EServiceException.Create('Service IChatService unavailable');
    try
      TextColor(ccWhite);
      writeln('Please enter your name, then press [Enter] to join the chat');
      writeln('Enter a void line to quit');
      write('@');
      TextColor(ccLightGray);
      readln(pseudo);
      if pseudo = '' then
        exit;
      callback := TChatCallback.Create(Client, IChatCallback);
      Service.Join(pseudo, callback);
      TextColor(ccWhite);
      writeln('Please type a message, then press [Enter]');
      writeln('Enter a void line to quit');
      repeat
        TextColor(ccLightGray);
        write('>');
        readln(msg);
        if msg='' then
          break;
        Service.BlaBla(pseudo, msg);
      until false;
    finally
      callback := nil; // will unsubscribe from the remote publisher
      Service := nil;  // release the service local instance BEFORE Client.Free
    end;
  finally
    Client.Free;
  end;
end;


begin
  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.
