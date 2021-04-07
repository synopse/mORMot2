/// Asynchronous RTSP Relay/Proxy over HTTP
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.rtsphttp;

{
  *****************************************************************************

   RTSP Stream Tunnelling over HTTP as defined by Apple at https://goo.gl/CX6VA3
   - Low-level HTTP and RTSP Connections
   - RTSP over HTTP Tunnelling 

  *****************************************************************************

  Encapsulate a RTSP TCP/IP duplex video stream into two HTTP links,
  one POST for upgoing commands, and one GET for downloaded video.

  Thanks to TAsyncServer, it can handle thousands on concurrent streams,
  with minimal resources, in a cross-platform way.

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.threads,
  mormot.core.log,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async;


{ ******************** Low-level HTTP and RTSP Connections }

type
  /// holds a HTTP POST connection for RTSP proxy
  // - as used by the TRtspOverHttpServer class
  TPostConnection = class(TAsyncConnection)
  protected
    fRtspTag: TPollSocketTag;
    // redirect the POST base-64 encoded command to the RTSP socket
    function OnRead(
      Sender: TAsyncConnections): TPollAsyncSocketOnRead; override;
    // will release the associated TRtspConnection instance
    procedure BeforeDestroy(Sender: TAsyncConnections); override;
  end;

  /// holds a RTSP connection for HTTP GET proxy
  // - as used by the TRtspOverHttpServer class
  TRtspConnection = class(TAsyncConnection)
  protected
    fGetBlocking: TCrtSocket;
    // redirect the RTSP socket input to the GET content
    function OnRead(
      Sender: TAsyncConnections): TPollAsyncSocketOnRead; override;
    // will release the associated blocking GET socket
    procedure BeforeDestroy(Sender: TAsyncConnections); override;
  end;



{ ******************** RTSP over HTTP Tunnelling }

type
  /// exceptions raised by this unit
  ERtspOverHttp = class(ESynException);

  /// implements RTSP over HTTP asynchronous proxy
  // - the HTTP transport is built from two separate HTTP GET and POST requests
  // initiated by the client; the server then binds the connections to form a
  // virtual full-duplex connection - see https://goo.gl/CX6VA3 for reference
  // material about this horrible, but widely accepted, Apple hack
  TRtspOverHttpServer = class(TAsyncServer)
  protected
    fRtspServer, fRtspPort: RawUtf8;
    fPendingGet: TRawUtf8List;
    function GetHttpPort: RawUtf8;
    // creates TPostConnection and TRtspConnection instances for a given stream
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: RawUtf8;
      out aConnection: TAsyncConnection): boolean; override;
  public
    /// initialize the proxy HTTP server forwarding specified RTSP server:port
    constructor Create(const aRtspServer, aRtspPort, aHttpPort: RawUtf8;
      aLog: TSynLogClass; const aOnStart, aOnStop: TOnNotifyThread;
      aOptions: TAsyncConnectionsOptions = []); reintroduce;
    /// shutdown and finalize the server
    destructor Destroy; override;
    /// convert a rtsp://.... URI into a http://... proxy URI
    // - will reuse the rtsp public server name, but change protocol to http://
    // and set the port to RtspPort
    function RtspToHttp(const RtspUri: RawUtf8): RawUtf8;
    /// convert a http://... proxy URI into a rtsp://.... URI
    function HttpToRtsp(const HttpUri: RawUtf8): RawUtf8;
    /// the associated RTSP server address
    property RtspServer: RawUtf8
      read fRtspServer;
    /// the associated RTSP server port
    property RtspPort: RawUtf8
      read fRtspPort;
    /// the bound HTTP port
    property HttpPort: RawUtf8
      read GetHttpPort;
  end;


const
  RTSP_MIME = 'application/x-rtsp-tunnelled';



implementation



{ ******************** Low-level HTTP and RTSP Connections }

{ TRtspConnection }

function TRtspConnection.OnRead(
  Sender: TAsyncConnections): TPollAsyncSocketOnRead;
begin
  if acoVerboseLog in Sender.Options then
    Sender.LogVerbose(self, 'Frame forwarded', fSlot.readbuf);
  if fGetBlocking.TrySndLow(pointer(fSlot.readbuf), length(fSlot.readbuf)) then
  begin
    Sender.Log.Add.Log(sllDebug, 'OnRead % RTSP forwarded % bytes to GET',
      [Handle, length(fSlot.readbuf)], self);
    result := sorContinue;
  end
  else
  begin
    Sender.Log.Add.Log(sllDebug,
      'OnRead % RTSP failed send to GET -> close % connection',
      [Handle, RemoteIP], self);
    result := sorClose;
  end;
  fSlot.readbuf := '';
end;

procedure TRtspConnection.BeforeDestroy(Sender: TAsyncConnections);
begin
  fGetBlocking.Free;
  inherited BeforeDestroy(Sender);
end;


{ TPostConnection }

function TPostConnection.OnRead(
  Sender: TAsyncConnections): TPollAsyncSocketOnRead;
var
  decoded: RawByteString;
  rtsp: TAsyncConnection;
begin
  result := sorContinue;
  decoded := Base64ToBinSafe(TrimControlChars(fSlot.readbuf));
  if decoded = '' then
    exit; // maybe some pending command chars
  fSlot.readbuf := '';
  rtsp := Sender.ConnectionFindLocked(fRtspTag);
  if rtsp <> nil then
  try
    Sender.Write(rtsp, decoded); // async sending to RTSP server
    Sender.Log.Add.Log(sllDebug, 'OnRead % POST forwarded RTSP command [%]',
      [Handle, decoded], self);
  finally
    Sender.Unlock;
  end
  else
  begin
    Sender.Log.Add.Log(sllDebug, 'OnRead % POST found no rtsp=%',
      [Handle, fRtspTag], self);
    result := sorClose;
  end;
end;

procedure TPostConnection.BeforeDestroy(Sender: TAsyncConnections);
begin
  Sender.ConnectionRemove(fRtspTag); // disable associated RTSP and GET sockets
  inherited BeforeDestroy(Sender);
end;



{ ******************** RTSP over HTTP Tunnelling }


{ TRtspOverHttpServer }

constructor TRtspOverHttpServer.Create(
  const aRtspServer, aRtspPort, aHttpPort: RawUtf8; aLog: TSynLogClass;
  const aOnStart, aOnStop: TOnNotifyThread; aOptions: TAsyncConnectionsOptions);
begin
  fLog := aLog;
  fRtspServer := aRtspServer;
  fRtspPort := aRtspPort;
  fPendingGet := TRawUtf8List.Create([fObjectsOwned, fCaseSensitive]);
  inherited Create(
    aHttpPort, aOnStart, aOnStop, TPostConnection, 'rtsp/http', aLog, aOptions);
end;

destructor TRtspOverHttpServer.Destroy;
var
  {%H-}log: ISynLog;
begin
  log := fLog.Enter(self, 'Destroy');
  inherited Destroy;
  fPendingGet.Free;
end;

type
  TProxySocket = class(THttpServerSocket)
  protected
    fExpires: cardinal;
  published
    property Method;
    property URL;
    property RemoteIP;
  end;

function TRtspOverHttpServer.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: RawUtf8; out aConnection: TAsyncConnection): boolean;
var
  log: ISynLog;
  sock, get, old: TProxySocket;
  cookie: RawUtf8;
  res: TNetResult;
  rtsp: TNetSocket;
  i, found: PtrInt;
  postconn: TPostConnection;
  rtspconn: TRtspConnection;
  now: cardinal;

  procedure PendingDelete(i: integer; const reason: RawUtf8);
  begin
    if log <> nil then
      log.Log(sllDebug, 'ConnectionCreate rejected %', [reason], self);
    fPendingGet.Delete(i);
  end;

begin
  aConnection := nil;
  get := nil;
  result := false;
  log := fLog.Enter('ConnectionCreate(%)', [PtrUInt(aSocket)], self);
  try
    sock := TProxySocket.Create(nil);
    try
      sock.AcceptRequest(aSocket, nil);
      sock.RemoteIP := aRemoteIp;
      sock.CreateSockIn; // faster header process (released below once not needed)
      if (sock.GetRequest({withBody=}false, {headertix=}0) = grHeaderReceived) and
         (sock.URL <> '') then
      begin
        if log <> nil then
          log.Log(sllTrace, 'ConnectionCreate received % % %',
            [sock.Method, sock.URL, sock.HeaderGetText], self);
        cookie := sock.HeaderGetValue('X-SESSIONCOOKIE');
        if cookie = '' then
          exit;
        fPendingGet.Safe.Lock;
        try
          found := -1;
          now := mormot.core.os.GetTickCount64 shr 10;
          for i := fPendingGet.Count - 1 downto 0 do
          begin
            old := fPendingGet.ObjectPtr[i];
            if now > old.fExpires then
            begin
              if log <> nil then
                log.Log(sllTrace, 'ConnectionCreate deletes deprecated %',
                  [old], self);
              fPendingGet.Delete(i);
            end
            else if fPendingGet[i] = cookie then
              found := i;
          end;
          if IdemPropNameU(sock.Method, 'GET') then
          begin
            if found >= 0 then
              PendingDelete(found, 'duplicated')
            else
            begin
              sock.Write(FormatUtf8(
                'HTTP/1.0 200 OK'#13#10 +
                'Server: % %'#13#10 +
                'Connection: close'#13#10 +
                'Date: Thu, 19 Aug 1982 18:30:00 GMT'#13#10 +
                'Cache-Control: no-store'#13#10 +
                'Pragma: no-cache'#13#10 +
                'Content-Type: ' + RTSP_MIME + #13#10#13#10,
                [Executable.ProgramName, Executable.Version.DetailedOrVoid]));
              sock.fExpires := now + 60 * 15; // deprecated after 15 minutes
              sock.CloseSockIn; // we won't use it any more
              fPendingGet.AddObject(cookie, sock);
              sock := nil; // will be in fPendingGet until POST arrives
              result := true;
            end;
          end
          else if IdemPropNameU(sock.Method, 'POST') then
          begin
            if found < 0 then
            begin
              if log <> nil then
                log.Log(sllDebug, 'ConnectionCreate rejected on unknown %',
                  [sock], self)
            end
            else if not IdemPropNameU(sock.ContentType, RTSP_MIME) then
              PendingDelete(found, sock.ContentType)
            else
            begin
              get := fPendingGet.Objects[found];
              fPendingGet.Objects[found] := nil; // will be owned by rtspinstance
              fPendingGet.Delete(found);
              sock.Sock := TNetSocket(-1); // disable Close on sock.Free -> handled in pool
            end;
          end;
        finally
          fPendingGet.Safe.UnLock;
        end;
      end
      else if log <> nil then
        log.Log(sllDebug, 'ConnectionCreate: ignored invalid %', [sock], self);
    finally
      sock.Free;
    end;
    if get = nil then
      exit;
    if not get.SockConnected then
    begin
      if log <> nil then
        log.Log(sllDebug, 'ConnectionCreate: GET disconnected %', [get], self);
      exit;
    end;
    res := NewSocket(
      fRtspServer, fRtspPort, nlTCP, {bind=}false, 1000, 1000, 1000, 0, rtsp);
    if res <> nrOK then
      raise ERtspOverHttp.CreateUtf8('No RTSP server on %:% (%)',
        [fRtspServer, fRtspPort, ToText(res)^]);
    postconn := TPostConnection.Create(aRemoteIp);
    rtspconn := TRtspConnection.Create(aRemoteIp);
    if not inherited ConnectionAdd(aSocket, postconn) or
       not inherited ConnectionAdd(rtsp, rtspconn) then
      raise ERtspOverHttp.CreateUtf8('inherited %.ConnectionAdd(%) % failed',
        [self, aSocket, cookie]);
    aConnection := postconn;
    postconn.fRtspTag := rtspconn.Handle;
    rtspconn.fGetBlocking := get;
    if not fClients.Start(rtspconn) then
    begin
    if log <> nil then
      log.Log(sllWarning,
        'ConnectionCreate fClients.Start failed for %', [rtspconn], self);
      exit;
    end;
    get := nil;
    result := true;
    if log <> nil then
      log.Log(sllTrace,
        'ConnectionCreate added get=% post=%/% and rtsp=%/% for %',
        [PtrUInt(rtspconn.fGetBlocking.Sock), PtrUInt(aSocket), aConnection.Handle,
         PtrUInt(rtsp), rtspconn.Handle, cookie], self);
  except
    if log <> nil then
      log.Log(sllDebug, 'ConnectionCreate(%) failed', [PtrUInt(aSocket)], self);
    get.Free;
  end;
end;

function TRtspOverHttpServer.GetHttpPort: RawUtf8;
begin
  if self <> nil then
    result := fServer.Port
  else
    result := '';
end;

function TRtspOverHttpServer.RtspToHttp(const RtspUri: RawUtf8): RawUtf8;
var
  uri: TUri;
begin
  if (self <> nil) and
     IdemPChar(pointer(RtspUri), 'RTSP://') and
     uri.From(copy(RtspUri, 8, maxInt), fRtspPort) and
     IdemPropNameU(uri.Port, fRtspPort) then
    FormatUtf8('http://%:%/%', [uri.Server, fServer.Port, uri.Address], result)
  else
    result := RtspUri;
end;

function TRtspOverHttpServer.HttpToRtsp(const HttpUri: RawUtf8): RawUtf8;
var
  uri: TUri;
begin
  if (self <> nil) and
     uri.From(HttpUri, fServer.Port) and
     IdemPropNameU(uri.Port, fServer.Port) then
    FormatUtf8('rtsp://%:%/%', [uri.Server, fRtspPort, uri.Address], result)
  else
    result := HttpUri;
end;


end.

