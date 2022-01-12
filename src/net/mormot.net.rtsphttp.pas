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
    // redirect the POST Base64 encoded command to the RTSP socket
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    // will release the associated TRtspConnection instance
    procedure BeforeDestroy; override;
  end;

  /// holds a RTSP connection for HTTP GET proxy
  // - as used by the TRtspOverHttpServer class
  TRtspConnection = class(TAsyncConnection)
  protected
    fGetBlocking: TCrtSocket;
    // redirect the RTSP socket input to the GET content
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    // will release the associated blocking GET socket
    procedure BeforeDestroy; override;
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
    // - default aThreadPoolCount=1 is the best and fastest for our RTSP/HTTP
    // tunneling process, which is almost non blocking
    // - warning: should call WaitStarted() to let Execute bind and run
    constructor Create(const aRtspServer, aRtspPort, aHttpPort: RawUtf8;
      aLog: TSynLogClass; const aOnStart, aOnStop: TOnNotifyThread;
      aOptions: TAsyncConnectionsOptions = []; aThreadPoolCount: integer = 1); reintroduce;
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

function TRtspConnection.OnRead: TPollAsyncSocketOnReadWrite;
begin
  if acoVerboseLog in fOwner.Options then
    fOwner.LogVerbose(self, 'Frame forwarded', [], fRd);
  if fGetBlocking.TrySndLow(fRd.Buffer, fRd.Len) then
  begin
    fOwner.Log.Add.Log(sllDebug, 'OnRead % RTSP forwarded % bytes to GET',
      [Handle, fRd.Len], self);
    result := soContinue;
  end
  else
  begin
    fOwner.Log.Add.Log(sllDebug,
      'OnRead % RTSP failed send to GET -> close % connection',
      [Handle, RemoteIP], self);
    result := soClose;
  end;
  fRd.Reset;
end;

procedure TRtspConnection.BeforeDestroy;
begin
  fGetBlocking.Free;
  // inherited BeforeDestroy; // void parent method
end;


{ TPostConnection }

function TPostConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  b64: RawUtf8;
  decoded: RawByteString;
  rtsp: TAsyncConnection;
begin
  result := soContinue;
  fRd.AsText(b64);
  decoded := Base64ToBinSafe(TrimControlChars(b64));
  if decoded = '' then
    exit; // maybe some pending command chars
  fRd.Reset;
  rtsp := fOwner.ConnectionFindAndLock(fRtspTag, cReadOnly);
  if rtsp <> nil then
  try
    fOwner.WriteString(rtsp, decoded); // async sending to RTSP server
    fOwner.Log.Add.Log(sllDebug, 'OnRead % POST forwarded RTSP command [%]',
      [Handle, decoded], self);
  finally
    fOwner.Unlock(cReadOnly);
  end
  else
  begin
    fOwner.Log.Add.Log(sllDebug, 'OnRead % POST found no rtsp=%',
      [Handle, fRtspTag], self);
    result := soClose;
  end;
end;

procedure TPostConnection.BeforeDestroy;
begin
  fOwner.ConnectionRemove(fRtspTag); // disable associated RTSP and GET sockets
  // inherited BeforeDestroy; // void parent method
end;



{ ******************** RTSP over HTTP Tunnelling }

{ TRtspOverHttpServer }

constructor TRtspOverHttpServer.Create(
  const aRtspServer, aRtspPort, aHttpPort: RawUtf8; aLog: TSynLogClass;
  const aOnStart, aOnStop: TOnNotifyThread; aOptions: TAsyncConnectionsOptions;
  aThreadPoolCount: integer);
begin
  fLog := aLog;
  fRtspServer := aRtspServer;
  fRtspPort := aRtspPort;
  fPendingGet := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive]);
  inherited Create(aHttpPort, aOnStart, aOnStop, TPostConnection, 'rtsp/http',
    aLog, aOptions, aThreadPoolCount);
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
  parse: THttpServerSocketGetRequestResult;
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
  log := fLog.Enter('ConnectionCreate(%)', [pointer(aSocket)], self);
  try
    res := aSocket.MakeBlocking; // otherwise sock.GetRequest() fails
    if (res <> nrOK) and
       (log <> nil) then
      log.Log(sllTrace, 'ConnectionCreate MakeBlocking=%', [ToText(res)^], self);
    sock := TProxySocket.Create(nil);
    try
      sock.AcceptRequest(aSocket, nil);
      sock.RemoteIP := aRemoteIp;
      sock.CreateSockIn; // faster header process (released below once not needed)
      parse := sock.GetRequest({withBody=}false, {headertix=}0);
      if (parse = grHeaderReceived) and
         (sock.URL <> '') then
      begin
        if log <> nil then
          log.Log(sllTrace, 'ConnectionCreate received % % %',
            [sock.Method, sock.URL, sock.Http.Headers], self);
        cookie := sock.HeaderGetValue('X-SESSIONCOOKIE');
        if cookie = '' then
          exit;
        fPendingGet.Safe.WriteLock;
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
              sock.SndLow(FormatUtf8(
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
            else if not IdemPropNameU(sock.Http.ContentType, RTSP_MIME) then
              PendingDelete(found, sock.Http.ContentType)
            else
            begin
              get := fPendingGet.Objects[found];
              fPendingGet.Objects[found] := nil; // will be owned by rtspinstance
              fPendingGet.Delete(found);
              sock.Sock := TNetSocket(-1); // disable Close on sock.Free -> handled in pool
            end;
          end;
        finally
          fPendingGet.Safe.WriteUnLock;
        end;
      end
      else if log <> nil then
        log.Log(sllDebug, 'ConnectionCreate rejected % % % % % %',
          [ToText(parse)^, sock.Http.Command, sock, sock.Method, sock.URL,
           sock.Http.Headers], self);
    finally
      sock.Free;
      res := aSocket.MakeAsync; // as expected by TPollAsyncSockets
      if (res <> nrOK) and
         (log <> nil) then
        log.Log(sllTrace, 'ConnectionCreate MakeAsync=%', [ToText(res)^], self);
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
      fRtspServer, fRtspPort, nlTcp, {bind=}false, 1000, 1000, 1000, 0, rtsp);
    if res <> nrOK then
      raise ERtspOverHttp.CreateUtf8('No RTSP server on %:% (%)',
        [fRtspServer, fRtspPort, ToText(res)^]);
    // create the main POST connection and its associated RTSP connection
    postconn := TPostConnection.Create(self, aRemoteIp);
    rtspconn := TRtspConnection.Create(self, aRemoteIp);
    if not inherited ConnectionNew(aSocket, postconn) or
       not inherited ConnectionNew(rtsp, rtspconn) then
      raise ERtspOverHttp.CreateUtf8('inherited %.ConnectionNew(%) % failed',
        [self, aSocket, cookie]);
    aConnection := postconn;
    postconn.fRtspTag := rtspconn.Handle;
    rtspconn.fGetBlocking := get;
    res := rtspconn.Socket.MakeAsync; // as expected by fClients.Start
    if (res <> nrOK) and
       (log <> nil) then
      log.Log(sllTrace, 'ConnectionCreate rtspconn.MakeAsync=%', [ToText(res)^], self);
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
        [pointer(rtspconn.fGetBlocking.Sock), pointer(aSocket), aConnection.Handle,
         pointer(rtsp), rtspconn.Handle, cookie], self);
  except
    if log <> nil then
      log.Log(sllDebug, 'ConnectionCreate(%) failed', [pointer(aSocket)], self);
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

