/// simple SOA client using a callback for long process ending notification
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program restws_longworkclient;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.client,
  mormot.rest.http.client,
  restws.longworkshared in 'restws.longworkshared.pas';

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

procedure Run;
var
  Client: TRestHttpClientWebsockets;
  workName: string;
  Service: ILongWorkService;
  callback: ILongWorkCallback;
begin
  writeln('Connecting to the local Websockets server...');
  // create a TRestClientUri instance able to upgrade to WebSockets
  Client := TRestHttpClientWebsockets.Create('127.0.0.1', '8888', TOrmModel.Create([]));
  try
    Client.Model.Owner := Client;
    // upgrade the HTTP link to WebSockets using our binary encrypted protocol
    Client.WebSocketsUpgrade(PROJECT31_TRANSMISSION_KEY);

    // call a simple REST method to ensure the server is actually running
    if not Client.ServerTimeStampSynchronize then
      raise EServiceException.Create('Error connecting to the server: please run Project31LongWorkServer.exe');

    // register and resolve our ILongWorkService in our TRestClientUri instance
    Client.ServiceDefine([ILongWorkService], sicShared);
    if not Client.Services.Resolve(ILongWorkService, Service) then
      raise EServiceException.Create('Service ILongWorkService unavailable');

    // main console loop allowing to run remote long work tasks on server
    TextColor(ccWhite);
    writeln('Please type a work name, then press [Enter]');
    writeln('Enter a void line to quit');

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

  finally
    // close the connection to the server
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

