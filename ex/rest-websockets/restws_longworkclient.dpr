/// simple SOA client using a callback for long process ending notification
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program restws_longworkclient;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$ifndef FPC}
  {$R mormot.win.default.manifest.res}
  {$endif}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.net.ws.core,
  mormot.rest.client,
  mormot.rest.http.client,
  restws.longworkshared in 'restws.longworkshared.pas';


{$I-} // avoid io check in writeln()

type
  /// we define a class to implement the callback on client side
  TLongWorkCallback = class(TInterfacedCallback, ILongWorkCallback)
  protected
    procedure WorkFinished(const workName: string; timeTaken: integer);
    procedure WorkFailed(const workName, error: string);
  end;


{ TLongWorkCallback }

procedure TLongWorkCallback.WorkFailed(const workName, error: string);
begin
  TextColor(ccLightRed);
  writeln(#13'Received callback WorkFailed(', workName, ') with message "', error, '"');
  TextColor(ccLightGray);
  write('>');
end;

procedure TLongWorkCallback.WorkFinished(const workName: string; timeTaken: integer);
begin
  TextColor(ccLightBlue);
  writeln(#13'Received callback WorkFinished(', workName, ') in ', timeTaken, 'ms');
  TextColor(ccLightGray);
  write('>');
end;


// this is the main method of this client console application

type
  TClient = class
  private
    Client: TRestHttpClientWebsockets;
  public
    ClientIsTerminated: Boolean;
    ReconnectNeeded: Boolean;
    procedure OnConnect(Sender: TRestClientUri);
    procedure OnDisconnect(Sender: TObject);
    procedure Run;
  end;


{ TClient }

procedure TClient.Run;
var
  workName: string;
  Service: ILongWorkService;
  callback: ILongWorkCallback;
begin
  writeln('Connecting to the local Websockets server...');
  // create a TRestClientUri instance able to upgrade to WebSockets
  try
    While not ClientIsTerminated do
    try
      if not Assigned(Client) or ReconnectNeeded then
      begin
        FreeAndNil(Client);
        callback := nil;
        Service := nil;

        Writeln('Create client connection');
        ReconnectNeeded:=False;
        Client := TRestHttpClientWebsockets.Create('127.0.0.1', '8888', TOrmModel.Create([]),false,'','',0,0,2000);
        Client.OnWebSocketsClosed := OnDisconnect;
        Client.OnConnected := OnConnect;
        Client.RetryOnceOnTimeout := False;
        Client.Model.Owner := Client;
        
        // upgrade the HTTP link to WebSockets using our binary encrypted protocol
        Client.WebSocketsUpgrade(LONGWORK_TRANSMISSION_KEY);

        // call a simple REST method to ensure the server is actually running
        if not Client.ServerTimeStampSynchronize then
          raise EServiceException.Create('Error connecting to the server: please run Project31LongWorkServer.exe');

        // register and resolve our ILongWorkService in our TRestClientUri instance
        Client.ServiceDefine([ILongWorkService], sicShared);
      end;

      if not Assigned(Service) then
        if not Client.Services.Resolve(ILongWorkService, Service) then
          raise EServiceException.Create('Service ILongWorkService unavailable');

      // main console loop allowing to run remote long work tasks on server
      TextColor(ccWhite);
      writeln('Please type a work name, then press [Enter]');
      writeln('Enter a void line to quit,');
      writeln(' or enter "reset" to create a new client instance, or "break" to close the socket');

      // we will share the very same callback on client side
      callback := TLongWorkCallback.Create(Client, ILongWorkCallback);
      try
        // actual main loop
        repeat
          TextColor(ccLightGray);
          write('>');
          readln(workName);
          if workName = '' then
            break;
          if workName = 'break' then
          begin
            Client.Socket.Close; // simulate a raw socket disconnection
            break;
          end
          else if workName = 'reset' then
            raise Exception.Create('Reset');
          if ReconnectNeeded then
            Raise Exception.Create('reconnect needed');
          Service.StartWork(workName, callback);
          TextColor(ccBrown);
          writeln('Service.TotalWorkCount=', Service.TotalWorkCount);
        until false;
      finally
        // mandatory: release the service local interfaces BEFORE Client.Free!
        callback := nil;
        Service := nil;
        // remember: this is mandatory!
      end;

    except
      on E: Exception do
      begin
        Writeln(E.Message);
        callback := nil;
        Service := nil;
        FreeAndNil(Client);
      end;
    end;
  finally
    // close the connection to the server
    FreeAndNil(Client);
  end;
end;

procedure TClient.OnConnect(Sender: TRestClientUri);
begin
  Writeln('Connected !!!', Sender.GetCurrentSessionUserID);
  ReconnectNeeded := False;
end;

procedure TClient.OnDisconnect(Sender: TObject);
begin
  Writeln('Disconnected !!!');
  ReconnectNeeded := True;
end;


var
  App: TClient;
begin
  with TSynLog.Family do
    begin // enable logging to file and to console
      Level := LOG_VERBOSE;
      EchoToConsole := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOneFile;
    end;
  WebSocketLog := TSynLog; // verbose log of all WebSockets activity

  App := TClient.Create;
  try
    App.Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.


