/// regression tests for Several Network Protocols
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.net.proto;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.test,
  mormot.core.perf,
  mormot.core.threads,
  mormot.core.search,
  mormot.core.zip,
  mormot.crypt.core,
  mormot.crypt.secure,
  {$ifdef OSPOSIX}
  mormot.net.tftp.server,
  mormot.lib.curl, // client code for TFTP server validation
  {$endif OSPOSIX}
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async,
  mormot.net.ws.core,
  mormot.net.openapi,
  mormot.net.ldap,
  mormot.net.dns,
  mormot.net.rtsphttp,
  mormot.net.tunnel,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.rest.http.client;

const
  SYNOPSE_IP = '82.67.73.95'; // the mormot's home in the French mountains :)

type
  /// this test case will validate several low-level protocols
  TNetworkProtocols = class(TSynTestCase)
  protected
    // for DNSAndLDAP
    synopsednsip: RawUtf8;
    hasinternet: boolean;
    // for _THttpPeerCache
    peercacheopt: THttpRequestExtendedOptions;
    peercachedirect: THttpPeerCache;
    // for _TUriTree
    reqone, reqtwo: RawUtf8;
    request: integer;
    reqthree: boolean;
    reqfour: Int64;
    // for _TTunnelLocal
    tunnelappsec: RawUtf8;
    tunneloptions: TTunnelOptions;
    tunnelsequence: integer;
    procedure TunnelExecute(Sender: TObject);
    function TunnelBackgroundOpen(l: TTunnelLocal; s: TTunnelSession;
     const r: ITunnelTransmit; const sc, vc: ICryptCert): TLoggedWorkThread;
    procedure CheckBlocks(const log: ISynLog; const sent, recv: RawByteString;
      num: integer);
    procedure TunnelTest(var rnd: TLecuyer;
      const clientcert, servercert: ICryptCert; packets: integer = 100);
    procedure TunnelSocket(const log: ISynLog; var rnd: TLecuyer;
      clientinstance, serverinstance: TTunnelLocal; packets: integer);
    procedure TunnelRelay(relay: TTunnelRelay; const agent: array of ITunnelAgent;
      const console: array of ITunnelConsole; var rnd: TLecuyer; packets: integer);
    procedure RunLdapClient(Sender: TObject);
    procedure RunPeerCacheDirect(Sender: TObject);
    function OnPeerCacheDirect(var aUri: TUri; var aHeader: RawUtf8;
      var aOptions: THttpRequestExtendedOptions): integer;
    function OnPeerCacheRequest(Ctxt: THttpServerRequestAbstract): cardinal;
    // several methods used by _TUriTree
    function DoRequest_(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest0(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest1(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest2(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest3(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest4(Ctxt: THttpServerRequestAbstract): cardinal;
    // this is the main method called by RtspOverHttp[BufferedWrite]
    procedure DoRtspOverHttp(options: TAsyncConnectionsOptions);
  published
    /// Engine.IO and Socket.IO regression tests
    procedure _SocketIO;
    /// validate DNS and LDAP clients (and NTP/SNTP)
    procedure DNSAndLDAP;
    /// validate THttpPeerCache process
    procedure _THttpPeerCache;
    /// some HTTP shared/low-level process
    procedure HTTP;
    /// validate THttpProxyCache process
    procedure _THttpProxyCache;
    /// validate TUriTree high-level structure
    procedure _TUriTree;
    /// RTSP over HTTP, as implemented in mormot.net.rtsphttp unit
    procedure RTSPOverHTTP;
    /// RTSP over HTTP, with always temporary buffering
    procedure RTSPOverHTTPBufferedWrite;
    /// validate mormot.net.tunnel
    procedure Tunnel;
    /// validate IP processing functions
    procedure IPAddresses;
    /// validate mormot.net.openapi unit
    procedure OpenAPI;
    {$ifdef OSPOSIX}
    /// validate mormot.net.tftp.server using libcurl (so only POSIX by now)
    procedure TFTPServer;
    {$endif OSPOSIX}
  end;


implementation

procedure TNetworkProtocols._SocketIO;
var
  m: TSocketIOMessage;
begin
  // validate low-level Socket.IO message decoder
  Check(not m.Init(''));
  Check(not m.Init('z'));
  Check(m.Init('0'));
  Check(m.PacketType = sioConnect);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs(''));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('0/test,{}'));
  Check(m.PacketType = sioConnect);
  Check(m.NameSpaceIs('/test'));
  Check(m.DataIs('{}'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('1'));
  Check(m.PacketType = sioDisconnect);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs(''));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('1/admin,'));
  Check(m.PacketType = sioDisconnect);
  Check(m.NameSpaceIs('/admin'));
  Check(not m.NameSpaceIs('/admi'));
  Check(not m.NameSpaceIs('/admiN'));
  Check(m.DataIs(''));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('0/admin,{"sid":"oSO0OpakMV_3jnilAAAA"}'));
  Check(m.PacketType = sioConnect);
  Check(m.NameSpaceIs('/admin'));
  Check(m.DataIs('{"sid":"oSO0OpakMV_3jnilAAAA"}'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('4{"message":"Not authorized"}'));
  Check(m.PacketType = sioConnectError);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs('{"message":"Not authorized"}'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('2["foo"]'));
  Check(m.PacketType = sioEvent);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs('["foo"]'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('2/admin,["bar"]'));
  Check(m.PacketType = sioEvent);
  Check(m.NameSpaceIs('/admin'));
  Check(m.DataIs('["bar"]'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('212["foo"]'));
  Check(m.PacketType = sioEvent);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs('["foo"]'));
  CheckEqual(m.ID, 12);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('3/admin,13["bar"]'));
  Check(m.PacketType = sioAck);
  Check(m.NameSpaceIs('/admin'));
  Check(m.DataIs('["bar"]'));
  CheckEqual(m.ID, 13);
  CheckEqual(m.BinaryAttachment, 0);
  Check(m.Init('51-["baz",{"_placeholder":true,"num":0}]'));
  Check(m.PacketType = sioBinaryEvent);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs('["baz",{"_placeholder":true,"num":0}]'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 1);
  Check(m.Init('52-/admin,["baz",{"_placeholder":true,"num":0},{"_placeholder":true,"num":1}]'));
  Check(m.PacketType = sioBinaryEvent);
  Check(m.NameSpaceIs('/admin'));
  Check(m.DataIs('["baz",{"_placeholder":true,"num":0},{"_placeholder":true,"num":1}]'));
  CheckEqual(m.ID, 0);
  CheckEqual(m.BinaryAttachment, 2);
  Check(m.Init('61-15["bar",{"_placeholder":true,"num":0}]'));
  Check(m.PacketType = sioBinaryAck);
  Check(m.NameSpaceIs('/'));
  Check(m.DataIs('["bar",{"_placeholder":true,"num":0}]'));
  CheckEqual(m.ID, 15);
  CheckEqual(m.BinaryAttachment, 1);
  Check(m.Init('61-/admin,1[{"_placeholder":true,"num":0}]'));
  Check(m.PacketType = sioBinaryAck);
  Check(m.NameSpaceIs('/admin'));
  Check(m.DataIs('[{"_placeholder":true,"num":0}]'));
  CheckEqual(m.ID, 1);
  CheckEqual(m.BinaryAttachment, 1);
end;

type
   TMyEnum = (eNone, e1, e2);
const
  MYENUM2TXT: array[TMyEnum] of RawUtf8 = ('', 'one', 'and 2');

const
  // some reference OpenAPI/Swagger definitions
  // - downloaded as openapi-ref.zip since some of those endpoints are unstable
  OpenApiName: array[0 .. 6] of RawUtf8 = (
    'FinTrack',
    'Nakama',  // https://github.com/heroiclabs/nakama
    'Pets2',   // https://petstore.swagger.io/v2/swagger.json
    'Pets3',   // https://petstore3.swagger.io/api/v3/openapi.json
    'Qdrant',  // https://qdrant.github.io/qdrant/redoc/v1.8.x/openapi.json
    'VAS',     // https://platform-api-staging.vas.com/api/v1/swagger.json
    'JTL');    // https://developer.jtl-software.com/_spec/products/erpapi/@1.1-onprem/openapi.json?download

procedure TNetworkProtocols.OpenAPI;
var
  i: PtrInt;
  fn: TFileName;
  key, prev, dto, client: RawUtf8;
  refzip: RawByteString;
  api: TRawUtf8DynArray;
  oa: TOpenApiParser;
  timer: TPrecisionTimer;
begin
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and 2'), 2);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'one'), 1);
  CheckEqual(FindCustomEnum(MYENUM2TXT, ''), 0);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and'), 0);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and 3'), 0);
  for i := 0 to high(RESERVED_KEYWORDS) do
  begin
    key := RESERVED_KEYWORDS[i];
    CheckUtf8(StrComp(pointer(prev), pointer(key)) < 0, key);
    prev := key;
    Check(IsReservedKeyWord(key));
    inc(key[1], 32);    // lowercase first char
    Check(IsReservedKeyWord(key));
    LowerCaseSelf(key); // lowercase all chars
    Check(IsReservedKeyWord(key));
    key := key + 's';
    Check(not IsReservedKeyWord(key));
    Check(not IsReservedKeyWord(UInt32ToUtf8(i)));
  end;
  SetLength(api, length(OpenApiName));
  for i := 0 to high(OpenApiName) do
    if OpenApiName[i] <> '' then
    begin
      fn := FormatString('%OpenApi%.json', [WorkDir, OpenApiName[i]]);
      api[i] := StringFromFile(fn);
      if api[i] <> '' then
        continue; // already downloaded
      if refzip <> '' then
        continue; // download .zip once
      refzip := DownloadFile('https://synopse.info/files/openapi-ref.zip');
      if refzip = '' then
        refzip := 'none'
      else if UnZipMemAll(refzip, WorkDir) then // one url to rule them all
        api[i] := StringFromFile(fn); // try once
    end;
  for i := 0 to high(api) do
    if api[i] <> '' then
    begin
      timer.Start;
      oa := TOpenApiParser.Create(OpenApiName[i]);
      try
        //oa.Options := oa.Options + [opoGenerateStringType];
        //oa.Options := oa.Options + OPENAPI_CONCISE;
        //oa.Options := oa.Options + [opoNoEnum];
        //oa.Options := oa.Options + [opoGenerateOldDelphiCompatible];
        //oa.Options := oa.Options + [opoClientOnlySummary];
        //oa.Options := oa.Options + [opoDtoNoDescription, opoClientNoDescription];
        oa.ParseJson(api[i]);
        // ensure there was something properly parsed
        Check(oa.Version <> oavUnknown, 'version');
        Check(oa.Title <> '', 'title');
        Check(oa.Operations <> nil, 'operations');
        // generate the dto/client units
        dto := oa.GenerateDtoUnit;
        client := oa.GenerateClientUnit;
        Check((opoGenerateSingleApiUnit in oa.Options) or (dto <> ''), 'dto');
        Check(client <> '', 'client');
        {$ifdef OSLINUX}
        //oa.ExportToDirectory('/home/ab/dev/lib2/test/');
        {$endif OSLINUX}
        //ConsoleWrite(dto);
        //ConsoleWrite(client);
      finally
        oa.Free;
      end;
      NotifyTestSpeed('%', [OpenApiName[i]], 0, length(dto) + length(client), @timer);
    end;
end;

procedure RtspRegressionTests(proxy: TRtspOverHttpServer; test: TSynTestCase;
  clientcount, steps: integer);
var
  streamer: TCrtSocket;
  req: array of record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUtf8;
  end;

  procedure Shutdown;
  var
    r, rmax: PtrInt;
    log: ISynLog;
    timer, one: TPrecisionTimer;
  begin
    proxy.LogClass.EnterLocal(log, proxy, 'Shutdown');
    // first half deletes POST first, second half deletes GET first
    timer.Start;
    rmax := clientcount - 1;
    for r := 0 to rmax shr 1 do
      req[r].post.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 1 %', [timer.Stop], proxy);
    timer.Start;
    req[0].stream.Free; // validates remove POST when RTSP already down
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 2 %', [timer.Stop], proxy);
    timer.Start;
    for r := (rmax shr 1) + 1 to rmax do
      req[r].get.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 3 %', [timer.Stop], proxy);
    timer.Start;
    for r := 0 to rmax shr 1 do
      req[r].get.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 4 %', [timer.Stop], proxy);
    timer.Start;
    for r := (rmax shr 1) + 1 to rmax do
      req[r].post.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 5 %', [timer.Stop], proxy);
    timer.Start;
    sleep(10);
    //proxy.Shutdown; // don't make any difference
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 6 %', [timer.Stop], proxy);
    for r := 1 to rmax do
    begin
      one.Start;
      //req[r].stream.OnLog := TSynLog.DoLog;
      req[r].stream.Free;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests SHUTDOWN 6-% %', [r, one.Stop], proxy);
    end;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests % SHUTDOWN 7 %', [timer.Stop], proxy);
    timer.Start;
    streamer.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests ENDED %', [timer.Stop], proxy);
  end;

var
  rmax, r, i: PtrInt;
  res: TNetResult;
  raw: integer;
  text: RawUtf8;
  log: ISynLog;
begin
  // here we follow the steps and content stated by https://goo.gl/CX6VA3
  proxy.LogClass.EnterLocal(log, proxy, 'Tests');
  if (proxy = nil) or
     (proxy.RtspServer <> '127.0.0.1') then
    test.Check(false, 'expect a running proxy on 127.0.0.1')
  else
  try
    if SystemInfo.dwNumberOfProcessors < 8 then
      Sleep(50); // seems mandatory from LUTI regression tests
    rmax := clientcount - 1;
    streamer := TCrtSocket.Bind(proxy.RtspPort);
    try
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % GET', [clientcount], proxy);
      SetLength(req, clientcount);
      for r := 0 to rmax do
        with req[r] do
        begin
          session := RandomIdentifier(20 + r and 15);
          get := THttpSocket.Open('localhost', proxy.Server.Port, nlTcp, 1000);
          get.SndLow('GET /sw.mov HTTP/1.0'#13#10 +
                     'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
                     'x-sessioncookie: ' + session + #13#10 +
                     'Accept: ' + RTSP_MIME + #13#10 +
                     'Pragma: no-cache'#13#10 +
                     'Cache-Control: no-cache'#13#10#13#10);
          get.CreateSockIn; // much faster process
          get.SockRecvLn(text);
          test.CheckEqual(text, 'HTTP/1.0 200 OK');
          get.GetHeader(false);
          get.CloseSockIn; // buffer not needed from now on
          test.CheckUtf8(hfConnectionClose in get.Http.HeaderFlags,
            'flags=%', [ToText(get.Http.HeaderFlags)]);
          test.Check(get.SockConnected, 'conn');
          test.CheckEqual(get.Http.ContentType, RTSP_MIME);
        end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], proxy);
      if SystemInfo.dwNumberOfProcessors < 8 then
        Sleep(50); // seems mandatory from LUTI regression tests
      for r := 0 to rmax do
        with req[r] do
        begin
          test.Check(get.SockConnected);
          post := TCrtSocket.Open('localhost', proxy.Server.Port);
          post.SndLow('POST /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Content-Type: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10 +
            'Content-Length: 32767'#13#10 +
            'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
          if log <> nil then
            log.Log(sllTrace, 'req[%].get=% connected=%',
              [r, get.Sock, get.SockConnected], proxy);
          test.Check(get.SockConnected);
          stream := streamer.AcceptIncoming(nil, {async=}false);
          if stream = nil then
          begin
            test.Check(false);
            exit;
          end;
          stream.Sock.SetLinger(0); // otherwise shutdown takes 40ms with epoll
          test.Check(get.SockConnected);
          test.Check(post.SockConnected);
        end;
      for i := 0 to steps do
      begin
        if log <> nil then
          log.Log(sllCustom1, 'RegressionTests % RUN #%', [clientcount, i], proxy);
        // send a RTSP command once in a while to the POST request
        if i and 7 = 0 then
        begin
          for r := 0 to rmax do
            req[r].post.SndLow(
              'REVTQ1JJQkUgcnRzcDovL3R1Y2tydS5hcHBsZS5jb20vc3cubW92IFJUU1AvMS4w'#13#10 +
              'DQpDU2VxOiAxDQpBY2NlcHQ6IGFwcGxpY2F0aW9uL3NkcA0KQmFuZHdpZHRoOiAx'#13#10 +
              'NTAwMDAwDQpBY2NlcHQtTGFuZ3VhZ2U6IGVuLVVTDQpVc2VyLUFnZW50OiBRVFMg'#13#10 +
              'KHF0dmVyPTQuMTtjcHU9UFBDO29zPU1hYyA4LjYpDQoNCg=='); 
          for r := 0 to rmax do
          begin
            text := req[r].stream.SockReceiveString(@res, @raw);
            test.CheckUtf8(text =
              'DESCRIBE rtsp://tuckru.apple.com/sw.mov RTSP/1.0'#13#10 +
              'CSeq: 1'#13#10 +
              'Accept: application/sdp'#13#10 +
              'Bandwidth: 1500000'#13#10 +
              'Accept-Language: en-US'#13#10 +
              'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10#13#10,
              'describe res=% raw=%', [_NR[res], raw]);
          end;
        end;
        // stream output should be redirected to the GET request
        for r := 0 to rmax do
          req[r].stream.SndLow(req[r].session); // session text as video stream
        if log <> nil then
          log.Log(sllCustom1, 'RegressionTests % RUN #% SndLow',
            [clientcount, i], proxy);
        for r := 0 to rmax do
          with req[r] do
          begin
            text := get.SockReceiveString(@res, @raw);
            //if log <> nil then
            //  log.Log(sllCustom1, 'RegressionTests % #%/% received %',
            //    [clientcount, r, rmax, text], proxy);
            test.CheckUtf8(text = session, 'session res=% raw=%', [_NR[res], raw]);
          end;
      end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], proxy);
    finally
      Shutdown;
      //sleep(1000); // ensure all client sockets are detected as closed
    end;
  except
    on E: Exception do
      test.Check(false, E.ClassName);
  end;
end;

procedure TNetworkProtocols.DoRtspOverHttp(options: TAsyncConnectionsOptions);
var
  N: integer;
  proxy: TRtspOverHttpServer;
begin
  {$ifdef OSDARWIN}
  N := 10;
  {$else}
  N := 100;
  {$endif OSDARWIN}
  proxy := TRtspOverHttpServer.Create(
    '127.0.0.1', '3999', '3998', TSynLog, nil, nil, options, {threads=}1);
    // threads=1 is the safest & fastest - but you may set 16 for testing
  try
    proxy.WaitStarted(10);
    RtspRegressionTests(proxy, self, N, 10);
  finally
    proxy.Free;
  end;
end;

const
  //ASYNC_OPTION = ASYNC_OPTION_DEBUG;
  ASYNC_OPTION = ASYNC_OPTION_VERBOSE;

procedure TNetworkProtocols.RTSPOverHTTP;
begin
  DoRtspOverHttp(ASYNC_OPTION);
end;

procedure TNetworkProtocols.RTSPOverHTTPBufferedWrite;
begin
  DoRtspOverHttp(ASYNC_OPTION + [acoWritePollOnly]);
end;

function TNetworkProtocols.DoRequest_(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  reqone := Ctxt['one'];
  Ctxt.RouteUtf8('two', reqtwo);
  reqthree := Ctxt.RouteEquals('three', '3');
  if not Ctxt.RouteInt64('four', reqfour) then
    reqfour := -1;
  result := HTTP_SUCCESS;
end;

function TNetworkProtocols.DoRequest0(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := DoRequest_(Ctxt);
  request := 0;
end;

function TNetworkProtocols.DoRequest1(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := DoRequest_(Ctxt);
  request := 1;
end;

function TNetworkProtocols.DoRequest2(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := DoRequest_(Ctxt);
  request := 2;
end;

function TNetworkProtocols.DoRequest3(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := DoRequest_(Ctxt);
  request := 3;
end;

function TNetworkProtocols.DoRequest4(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := DoRequest_(Ctxt);
  request := 4;
end;

const
  NODES: array[0..10] of RawUtf8 = (
    'water', 'slow', 'slower', 'waste', 'watch', 'water',
    'toaster', 'team', 'tester', 't', 'toast');

procedure TNetworkProtocols._TUriTree;
var
  tree: TUriTree;
  router: TUriRouter;
  ctxt: THttpServerRequest;
  i: PtrInt;
  n: TRadixTreeNode;
  timer: TPrecisionTimer;
  rnd: array[0..999] of RawUtf8;

  procedure Call(const uri, exp1, exp2: RawUtf8; exp3: boolean = false;
    exp4: Int64 = -1; expstatus: integer = HTTP_SUCCESS;
    const met: RawUtf8 = 'GET');
  begin
    request := -1;
    reqone := '';
    reqtwo := '';
    reqthree := false;
    reqfour := -1;
    ctxt.Method := met;
    ctxt.Url := uri;
    CheckEqual(router.Process(ctxt), expstatus);
    CheckEqual(reqone, exp1);
    CheckEqual(reqtwo, exp2);
    Check(reqthree = exp3);
    CheckEqual(reqfour, exp4);
  end;

  procedure Compute(const uri, expected: RawUtf8; const met: RawUtf8 = 'POST';
    expstatus: integer = 0);
  begin
    ctxt.Method := met;
    ctxt.Url := uri;
    CheckEqual(router.Process(ctxt), expstatus);
    if expected <> '' then
      CheckEqual(ctxt.Url, expected);
  end;

begin
  tree := TUriTree.Create(TUriTreeNode);
  try
    tree.insert('romane');
    tree.insert('romanus');
    tree.insert('romulus');
    tree.insert('rubens');
    tree.insert('ruber');
    tree.insert('rubicon');
    tree.insert('rubicundus');
    CheckHash(tree.ToText, $0946B9A0);
    CheckEqual(tree.Root.Lookup('rubens', nil).FullText, 'rubens');
    Check(tree.Root.Lookup('Rubens', nil) = nil);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create(TUriTreeNode, [rtoCaseInsensitiveUri]);
  try
    tree.insert('romanus');
    tree.insert('romane');
    tree.insert('rubicundus');
    tree.insert('rubicon');
    tree.insert('ruber');
    tree.insert('romulus');
    tree.insert('rubens');
    CheckHash(tree.ToText, $305E57F1);
    CheckEqual(tree.Root.Lookup('rubens', nil).FullText, 'rubens');
    CheckEqual(tree.Root.Lookup('Rubens', nil).FullText, 'rubens');
  finally
    tree.Free;
  end;
  tree := TUriTree.Create(TUriTreeNode);
  try
    tree.insert('/plaintext');
    tree.insert('/');
    tree.insert('/plain');
    //writeln(tree.ToText);
    CheckHash(tree.ToText, $B3522B86);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create(TUriTreeNode);
  try
    for i := 0 to high(NODES) do
      CheckEqual(tree.Insert(NODES[i]).FullText, NODES[i]);
    //writeln(tree.ToText);
    CheckHash(tree.ToText, $CC40347C);
    for i := 0 to high(NODES) do
    begin
      n := tree.Find(NODES[i]);
      CheckUtf8(n <> nil, NODES[i]);
      CheckEqual(n.FullText, NODES[i]);
    end;
    for i := 0 to high(NODES) do
      CheckEqual(tree.Insert(NODES[i]).FullText, NODES[i]);
    tree.AfterInsert; // sort by depth
    //writeln(tree.ToText);
    CheckHash(tree.ToText, $200CAEEB);
    for i := 0 to high(NODES) do
      CheckEqual(tree.Find(NODES[i]).FullText, NODES[i]);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create(TUriTreeNode);
  try
    for i := 0 to high(rnd) do
      rnd[i] := RandomIdentifier(Random32(24) * 2 + 1);
    for i := 0 to high(rnd) do
      CheckEqual(tree.Insert(rnd[i]).FullText, rnd[i]);
    timer.Start;
    for i := 0 to high(rnd) do
      CheckEqual(tree.Find(rnd[i]).FullText, rnd[i]);
    NotifyTestSpeed('big tree lookups', length(rnd), 0, @timer);
  finally
    tree.Free;
  end;
  ctxt := THttpServerRequest.Create(nil, 0, nil, 0, [], nil);
  router := TUriRouter.Create(TUriTreeNode);
  try
    Call('/plaintext', '', '', false, -1, 0);
    Call('/', '', '', false, -1, 0);
    router.Get('/plaintext', DoRequest_);
    router.Get('/plaintext', DoRequest_);
    CheckEqual(request, -1);
    Call('/plaintext', '', '');
    Call('/', '', '', false, -1, 0);
    //writeln(router.Tree[urmGet].ToText);
    router.Get('/', DoRequest0);
    Call('/plaintext', '', '');
    CheckEqual(request, -1);
    Call('/', '', '', false);
    CheckEqual(request, 0);
    router.Get('/do/<one>/pic/<two>', DoRequest0);
    router.Get('/do/<one>', DoRequest1);
    router.Get('/do/<one>/pic', DoRequest2);
    router.Get('/do/<one>/pic/<two>/', DoRequest3);
    router.Get('/da/<one>/<two>/<three>/<four>/', DoRequest4);
    //writeln(router.Tree[urmGet].ToText);
    Call('/do/a', 'a', '');
    CheckEqual(request, 1);
    Call('/do/123', '123', '');
    CheckEqual(request, 1);
    Call('/do/toto/pic', 'toto', '');
    CheckEqual(request, 2);
    Call('/do/toto/pic/titi/', 'toto', 'titi');
    CheckEqual(request, 3);
    Call('/do/toto/pic/titi', 'toto', 'titi');
    CheckEqual(request, 0);
    Call('/do/toto/pic/titi/', 'toto', 'titi');
    CheckEqual(request, 3);
    Call('/da/1/2/3/4', '', '', false, -1, 0);
    CheckEqual(request, -1);
    Call('/da/1/2/3/4/', '1', '2', true, 4);
    CheckEqual(request, 4);
    Call('/da/a1/b2/3/47456/', 'a1', 'b2', true, 47456);
    CheckEqual(request, 4);
    Compute('/static', '/static');
    Compute('/static2', '/static2');
    Compute('/', '/');
    router.Post('/static', '/some/static');
    Compute('/static', '/some/static');
    Compute('/static2', '/static2');
    Compute('/', '/');
    router.Post('/static2', '/some2/static');
    router.Post('/', '/index');
    Compute('/static', '/some/static');
    Compute('/static2', '/some2/static');
    Compute('/', '/index');
    Compute('/stat', '/stat');
    router.Post('/user/<id>', '/root/user.new?id=<id>');
    Compute('/user/1234', '/root/user.new?id=1234');
    Compute('/user/1234/', '/user/1234/');
    router.Post('/user/<id>/picture', '/root/user.newpic?id=<id>&pic=');
    router.Post('/user/<id>/picture/<pic>', '/root/user.newpic?pic=<pic>&id=<id>');
    Compute('/user/1234/picture', '/root/user.newpic?id=1234&pic=');
    Compute('/user/1234/picture/5', '/root/user.newpic?pic=5&id=1234');
    Compute('/user/1234/picture/', '/user/1234/picture/');
    Compute('/user/1234', '/root/user.new?id=1234');
    Compute('/user/1234/', '/user/1234/');
    Compute('/static', '/some/static');
    Compute('/static2', '/some2/static');
    Compute('/', '/index');
    Compute('/stat', '/stat');
    timer.Start;
    for i := 1 to 1000 do
      CheckEqual(router.Tree[urmPost].Find('/static').FullText, '/static');
    NotifyTestSpeed('URI lookups', 1000, 0, @timer);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/static', '/some/static');
    NotifyTestSpeed('URI static rewrites', 1000, 0, @timer);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/user/1234', '/root/user.new?id=1234');
    NotifyTestSpeed('URI parametrized rewrites', 1000, 0, @timer);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/plaintext', '', 'GET', 200);
    NotifyTestSpeed('URI static execute', 1000, 0, @timer);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/do/toto/pic', '', 'GET', 200);
    NotifyTestSpeed('URI parametrized execute', 1000, 0, @timer);
    router.Put('/index.php', '404');
    router.Put('/index.php', '404');
    router.Put('/admin.php', '404');
    Compute('/index.php', '/index.php', 'PUT', 404);
    Compute('/admin.php', '/admin.php', 'PUT', 404);
    router.Delete('/*', '/static/*');
    router.Delete('/root1/<path:url>', '/roota/<url>');
    router.Delete('/root2/*', '/rootb/*');
    router.Delete('/root3/<url>', '/rootc/<url>');
    router.Delete('/root4/<int:id>', '/rootd/<id>');
    Compute('/root1/one', '/roota/one', 'DELETE');
    Compute('/root1/one/', '/roota/one/', 'DELETE');
    Compute('/root1/one/two', '/roota/one/two', 'DELETE');
    Compute('/root2/one', '/rootb/one', 'DELETE');
    Compute('/root2/one/', '/rootb/one/', 'DELETE');
    Compute('/root2/one/two', '/rootb/one/two', 'DELETE');
    Compute('/root3/one', '/rootc/one', 'DELETE');
    Compute('/root3/one/', '/static/root3/one/', 'DELETE');
    Compute('/root3/one/two', '/static/root3/one/two', 'DELETE');
    Compute('/root4/one', '/static/root4/one', 'DELETE');
    Compute('/root4/1', '/rootd/1', 'DELETE');
    Compute('/root4/123', '/rootd/123', 'DELETE');
    Compute('/roota/one', '/static/roota/one', 'DELETE');
    Compute('/one', '/static/one', 'DELETE');
    Compute('/one/two', '/static/one/two', 'DELETE');
    //writeln(router.Tree[urmGet].ToText);
    //writeln(router.Tree[urmPost].ToText);
    //writeln(router.Tree[urmPut].ToText);
    //writeln(router.Tree[urmDelete].ToText);
    CheckHash(router.Tree[urmGet].ToText, $18A0BF58);
    CheckHash(router.Tree[urmPost].ToText, $E173FBB0);
    CheckHash(router.Tree[urmPut].ToText, $80F7A0EF);
    CheckHash(router.Tree[urmDelete].ToText, $39501147);
    router.Clear([urmPost]);
    Call('/plaintext', '', '');
    Compute('/static', '/static');
    router.Clear;
    Call('/plaintext', '', '', false, -1, 0);
    Compute('/static', '/static');
  finally
    router.Free;
    ctxt.Free;
  end;
end;

procedure CheckSynopseReverse(test: TNetworkProtocols; const ip: RawUtf8);
begin
  if ip = 'blog.synopse.info' then // occurs on some weird DNS servers
    test.Check(true)
  else // French marmots have fiber connection in their mountains
    test.CheckUtf8(PosEx('fbx.proxad.net', ip) <> 0, ip);
end;

procedure TNetworkProtocols.RunLdapClient(Sender: TObject);
var
  rev: RawUtf8;
  endtix: cardinal;
  one: TLdapClient;
  dv: variant;
begin
  // async validate actual LDAP client on public ldap.forumsys.com server
  try
    one := TLdapClient.Create;
    try
      if TSynLogTestLog.HasLevel([sllTrace, sllDebug, sllError]) then
        one.Log := TSynLogTestLog;
      one.Settings.TargetUri := 'ldap://ldap.forumsys.com';
      one.Settings.UserName := 'uid=einstein,dc=example,dc=com';
      one.Settings.Password := 'password';
      one.Settings.AllowUnsafePasswordBind := true;
      inc(fAssertions);
      if not one.Connect then // third party server issue -> skip
      begin
        NotifyProgress(['Warning: Connect to ', one.Settings.TargetUri,
          ' failed as ', one.ResultString], ccLightRed);
        exit;
      end;
      CheckUtf8(one.Bind, 'bind=%', [one.ResultString]);
      CheckEqual(one.BoundUser, one.Settings.UserName);
      CheckEqual(one.ExtWhoAmI, 'dn:uid=einstein,dc=example,dc=com');
      Check(one.Connected, 'before close');
      one.Sock.Close; // simulate socket disconnection
      Check(not one.Connected, 'closed'); // validate Reconnect
      dv := one.SearchAllRaw('uid=einstein,dc=example,dc=com', '', [], []);
      CheckHash(_Safe(dv)^.ToHumanJson, $39B7C7F1, one.Settings.UserName);
    finally
      one.Free;
    end;
  except
    on E: Exception do
      Check(false, E.Message);
  end;
  // retry reverse lookup DNS after some time
  if synopsednsip <> '' then
  begin
    endtix := GetTickSec + 5; // never wait forever
    repeat
      inc(fAssertions);
      rev := DnsReverseLookup(synopsednsip);
      if rev <> '' then
        break; // success
      Sleep(100); // wait a little and retry up to 2 seconds
    until GetTickSec > endtix;
    CheckSynopseReverse(self, rev);
  end;
end;

procedure TNetworkProtocols.DNSAndLDAP;
var
  ip, rev, u, v, json, sid: RawUtf8;
  o: TAsnObject;
  c: cardinal;
  withntp: boolean;
  guid: TGuid;
  i, j, k, n: PtrInt;
  dns, clients, a: TRawUtf8DynArray;
  le: TLdapError;
  rl, rl2: TLdapResultList;
  r: TLdapResult;
  at: TLdapAttributeType;
  ats, ats2: TLdapAttributeTypes;
  sat: TSamAccountType;
  gt: TGroupType;
  gts: TGroupTypes;
  ua: TUserAccountControl;
  uas: TUserAccountControls;
  sf: TSystemFlag;
  sfs: TSystemFlags;
  l: TLdapClientSettings;
  one: TLdapClient;
  res: TLdapResult;
  utc1, utc2: TDateTime;
  ntp, usr, pwd, ku, main, txt: RawUtf8;
  dn: TNameValueDNs;
  endtix: cardinal;
begin
  // validate NTP/SNTP client using NTP_DEFAULT_SERVER = time.google.com
  if not Executable.Command.Get('ntp', ntp) then
    ntp := NTP_DEFAULT_SERVER;
  withntp := not Executable.Command.Option('nontp');
  if Executable.Command.Has('dns') and
     Executable.Command.Has(['t', 'test']) then
    hasinternet := false // once is enough (e.g. from LUTI)
  else
    hasinternet := DnsLookups('yahoo.com', '', 500) <> nil; // avoid abusive wait
  if hasinternet then
  begin
    utc1 := GetSntpTime(ntp);
    if utc1 <> 0 then
    begin
      utc2 := NowUtc;
      AddConsole('% : % = %', [ntp, DateTimeMSToString(utc1), DateTimeMSToString(utc2)]);
      // only make a single GetSntpTime call - most servers refuse to scale
      if withntp then
        CheckSame(utc1, utc2, 1, 'NTP system A'); // allow 1 day diff
    end;
  end
  else
    AddConsole('no Internet connection');
  // validate some IP releated process
  Check(not NetIsIP4(nil));
  Check(not NetIsIP4('1'));
  Check(not NetIsIP4('1.2'));
  Check(not NetIsIP4('1.2.3'));
  Check(not NetIsIP4('1.2.3.'));
  Check(not NetIsIP4('1.2.3.4.'));
  Check(not NetIsIP4('1.2.3.4.5'));
  Check(NetIsIP4('1.2.3.4'));
  Check(NetIsIP4('12.3.4.5'));
  Check(NetIsIP4('12.34.5.6'));
  Check(NetIsIP4('12.34.56.7'));
  Check(NetIsIP4('12.34.56.78'));
  Check(NetIsIP4('112.134.156.178'));
  Check(not NetIsIP4('312.34.56.78'));
  Check(not NetIsIP4('12.334.56.78'));
  Check(not NetIsIP4('12.34.256.78'));
  Check(not NetIsIP4('12.34.56.278'));
  c := 0;
  Check(NetIsIP4('1.2.3.4', @c));
  CheckEqual(c, $04030201);
  // validate DNS client with some known values
  CheckEqual(ord(drrOPT), 41);
  CheckEqual(ord(drrHTTPS), 65);
  CheckEqual(ord(drrSPF), 99);
  CheckEqual(ord(drrEUI64), 109);
  CheckEqual(ord(drrTKEY), 249);
  CheckEqual(ord(drrAMTRELAY), 260);
  CheckEqual(DnsLookup(''), '');
  CheckEqual(DnsLookup('localhost'), '127.0.0.1');
  CheckEqual(DnsLookup('LocalHost'), '127.0.0.1');
  CheckEqual(DnsLookup('::1'), '127.0.0.1');
  CheckEqual(DnsLookup('1.2.3.4'), '1.2.3.4');
  CheckEqual(NetAddrResolve('1.2.3.4'), '1.2.3.4');
  if hasinternet then
  begin
    endtix := GetTickSec + 5; // never wait forever
    repeat
      inc(fAssertions);
      ip := DnsLookup('synopse.info');
      if ip <> '' then
        break;
      Sleep(100); // some DNS servers may fail at first: wait a little
    until GetTickSec > endtix;
    CheckEqual(NetAddrResolve('synopse.info'), ip, 'NetAddrResolve');
    rev := SYNOPSE_IP; // the marmots' home IP
    CheckEqual(ip, rev, 'dns1');
    repeat
      inc(fAssertions);
      ip := DnsLookup('blog.synopse.info');
      if ip <> '' then
        break;
      Sleep(100); // some DNS servers may fail at first: wait a little
    until GetTickSec > endtix;
    CheckEqual(ip, rev, 'dns2');
    inc(fAssertions);
    rev := DnsReverseLookup(ip);
    if rev = '' then
      synopsednsip := ip // we will retry in the background thread
    else
      CheckSynopseReverse(self, rev);
    // async validate actual LDAP client on public ldap.forumsys.com server
    if not fOwner.MultiThread then
      Run(RunLdapClient, self, 'ldap', true, false); // fails in the background
  end;
  // validate LDAP distinguished name conversion (no client)
  CheckEqual(DNToCN('CN=User1,OU=Users,OU=London,DC=xyz,DC=local'),
    'xyz.local/London/Users/User1');
  CheckEqual(DNToCN(
    'cn=JDoe,ou=Widgets,ou=Manufacturing,dc=USRegion,dc=OrgName,dc=com'),
    'USRegion.OrgName.com/Manufacturing/Widgets/JDoe');
  CheckEqual(DNToCN(
    'OU=d..zaf(fds )da\,z \"\"((''\\/ df\3D\3Dez,OU=test_wapt,OU=computers,' +
    'OU=tranquilit,DC=ad,DC=tranquil,DC=it'),
    'ad.tranquil.it/tranquilit/computers/test_wapt/d\.\.zaf(fds )da,z ""((''\\\/ df==ez');
  CheckEqual(DNToCN('dc=ad,dc=company,dc=it'), 'ad.company.it');
  CheckEqual(DNToCN('cn=foo, ou=bar'), '/bar/foo');
  CheckEqual(NormalizeDN('cn=foo, ou = bar'), 'CN=foo,OU=bar');
  Check(ParseDn('dc=ad, dc=company, dc = it', dn));
  CheckEqual(length(dn), 3);
  CheckEqual(dn[0].Name, 'dc');
  CheckEqual(dn[0].Value, 'ad');
  CheckEqual(dn[1].Name, 'dc');
  CheckEqual(dn[1].Value, 'company');
  CheckEqual(dn[2].Name, 'dc');
  CheckEqual(dn[2].Value, 'it');
  Check(ParseDn('uid=33\,test\=dans le nom,ou=Users,ou=montaigu,dc=sermo,dc=fr', dn));
  CheckEqual(length(dn), 5);
  CheckEqual(dn[0].Name, 'uid');
  CheckEqual(dn[0].Value, '33\,test\=dans le nom');
  CheckEqual(dn[1].Name, 'ou');
  CheckEqual(dn[1].Value, 'Users');
  CheckEqual(dn[2].Name, 'ou');
  CheckEqual(dn[2].Value, 'montaigu');
  CheckEqual(dn[3].Name, 'dc');
  CheckEqual(dn[3].Value, 'sermo');
  CheckEqual(dn[4].Name, 'dc');
  CheckEqual(dn[4].Value, 'fr');
  Check(not ParseDn('dc=ad, dc=company, dc', dn, {noraise=}true));
  // validate LDAP error recognition
  Check(RawLdapError(-1) = leUnknown);
  Check(RawLdapError(LDAP_RES_TOO_LATE) = leTooLate);
  Check(RawLdapError(10000) = leUnknown);
  Check(RawLdapError(LDAP_RES_AUTHORIZATION_DENIED) = leAuthorizationDenied);
  Check(RawLdapError(LDAP_RES_ESYNC_REFRESH_REQUIRED) = leEsyncRefreshRequired);
  Check(RawLdapError(LDAP_RES_NO_OPERATION) = leNoOperation);
  for le := low(le) to high(le) do
    Check(LDAP_ERROR_TEXT[le] <> '');
  for le := low(LDAP_RES_CODE) to high(LDAP_RES_CODE) do
    CheckUtf8(RawLdapError(LDAP_RES_CODE[le]) = le, LDAP_ERROR_TEXT[le]);
  CheckEqual(LDAP_ERROR_TEXT[leUnknown], 'unknown');
  CheckEqual(LDAP_ERROR_TEXT[leCompareTrue], 'compareTrue');
  CheckEqual(LDAP_ERROR_TEXT[leEsyncRefreshRequired], 'e-syncRefreshRequired');
  // validate LDAP escape/unescape
  for c := 0 to 200 do
  begin
    u := RandomIdentifier(c); // alphanums are never escaped
    CheckEqual(LdapEscape(u), u);
    CheckEqual(LdapUnescape(u), u);
    if u <> '' then
      CheckEqual(LdapEscapeName(u), u);
    CheckEqual(LdapEscapeCN(u), u);
    u := RandomAnsi7(c);
    CheckEqual(LdapUnescape(LdapEscape(u)), u);
  end;
  CheckEqual(LdapUnescape('abc\>'), 'abc>');
  CheckEqual(LdapUnescape('abc\>e'), 'abc>e');
  CheckEqual(LdapUnescape('abc\'), 'abc');
  Check(LdapSafe(''));
  Check(LdapSafe('abc'));
  Check(LdapSafe('ab cd'));
  Check(LdapSafe('@abc'));
  Check(not LdapSafe('\abc'));
  Check(not LdapSafe('abc*'));
  Check(not LdapSafe('a(bc'));
  Check(not LdapSafe('abc)'));
  Check(not LdapSafe('*'));
  Check(not LdapSafe('()'));
  // validate LDIF format
  Check(IsLdifSafe(nil, 0));
  Check(IsLdifSafe('toto', 0));
  Check(IsLdifSafe(nil, -1));
  Check(IsLdifSafe('toto', -1));
  Check(IsLdifSafe('toto', 1));
  Check(IsLdifSafe('toto', 2));
  Check(IsLdifSafe('toto', 3));
  Check(IsLdifSafe('toto', 4));
  Check(not IsLdifSafe('toto', 5), 'ending #0');
  Check(not IsLdifSafe(':oto', 4));
  Check(IsLdifSafe('t:to', 4));
  Check(IsLdifSafe('tot:', 4));
  Check(not IsLdifSafe(' oto', 4));
  Check(IsLdifSafe('t to', 4));
  Check(not IsLdifSafe('tot ', 4));
  Check(not IsLdifSafe('<oto', 4));
  Check(IsLdifSafe('t<to', 4));
  Check(IsLdifSafe('tot<', 4));
  Check(not IsLdifSafe(#0'oto', 4));
  Check(not IsLdifSafe('t'#0'to', 4));
  Check(not IsLdifSafe('tot', 4));
  Check(IsLdifSafe(#1'oto', 4));
  Check(IsLdifSafe('t'#1'to', 4));
  Check(IsLdifSafe('tot'#1'', 4));
  Check(not IsLdifSafe(#10'oto', 4));
  Check(not IsLdifSafe('t'#10'to', 4));
  Check(not IsLdifSafe('tot'#10'', 4));
  Check(not IsLdifSafe(#13'oto', 4));
  Check(not IsLdifSafe('t'#13'to', 4));
  Check(not IsLdifSafe('tot'#13'', 4));
  k := 100;
  u := RandomIdentifier(k);
  for i := 0 to k + 1 do
    Check(IsLdifSafe(pointer(u), i) = (i <= k));
  Append(u, ' '); // trailing space is unsafe
  for i := 0 to k + 2 do
    Check(IsLdifSafe(pointer(u), i) = (i <= k));
  // validate LDAP filter text parsing
  // against https://ldap.com/ldapv3-wire-protocol-reference-search reference
  CheckEqual(RawLdapTranslateFilter('', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('', {noraise=}false), '');
  o := RawLdapTranslateFilter('(attr=toto)');
  CheckHash(o, $E2C7F47C);
  o := RawLdapTranslateFilter('&(attr=toto)');
  CheckHash(o, $7B08F48C);
  o := RawLdapTranslateFilter('(&(attr1=a)(attr2=b)(attr3=c)(attr4=d))');
  CheckHash(o, $1AAB9884);
  o := RawLdapTranslateFilter('&(attr1=a)(attr2=b)(attr3=c)(attr4=d)');
  CheckHash(o, $1AAB9884);
  o := RawLdapTranslateFilter('(& ( attr1=a) (attr2=b) (attr3=c) (attr4=d))');
  CheckHash(o, $1AAB9884);
  o := RawLdapTranslateFilter('( & (attr1=a)(attr2=b)(attr3=c)(attr4=d) )');
  CheckHash(o, $1AAB9884);
  o := RawLdapTranslateFilter('(&(attr1=a)(&(attr2=b)(&(attr3=c)(attr4=d))))');
  CheckHash(o, $B1BB5EE1);
  o := RawLdapTranslateFilter('(&(givenName=John)(sn=Doe))');
  CheckHash(o, $372C9EF2);
  o := RawLdapTranslateFilter('(&)');
  CheckHash(o, $00A000A0, 'absolute true');
  o := RawLdapTranslateFilter('(|(givenName=John)(givenName=Jonathan))');
  CheckHash(o, $A9670687);
  o := RawLdapTranslateFilter('|(givenName=John)(givenName=Jonathan)');
  CheckHash(o, $A9670687);
  o := RawLdapTranslateFilter('(!(givenName=John))');
  CheckHash(o, $231C39EF);
  o := RawLdapTranslateFilter('(|)');
  CheckHash(o, $00A100A1, 'absolute false');
  o := RawLdapTranslateFilter('*');
  CheckHash(o, $01AD0187, 'present1');
  o := RawLdapTranslateFilter('(*)');
  CheckHash(o, $01AD0187, 'present2');
  o := RawLdapTranslateFilter('(uid:=jdoe)');
  CheckHash(o, $C93ADF87);
  o := RawLdapTranslateFilter('(:caseIgnoreMatch:=foo)');
  CheckHash(o, $4F000E3E);
  o := RawLdapTranslateFilter('(uid:dn:caseIgnoreMatch:=jdoe)');
  CheckHash(o, $921D9031);
  o := RawLdapTranslateFilter('(cn=*)');
  CheckHash(o, $6B6D0287, 'present3');
  o := RawLdapTranslateFilter('(cn=abc*)');
  CheckHash(o, $E8897DEA);
  o := RawLdapTranslateFilter('(cn=*lmn*)');
  CheckHash(o, $F5897DF6);
  o := RawLdapTranslateFilter('(cn=*xyz)');
  CheckHash(o, $019B7E03);
  o := RawLdapTranslateFilter('(cn=abc*def*lmn*uvw*xyz)');
  CheckHash(o, $9BA95FBA);
  o := RawLdapTranslateFilter('(createTimestamp>=20170102030405.678Z)');
  CheckHash(o, $D4CECB30);
  o := RawLdapTranslateFilter('(accountBalance<=1234)');
  CheckHash(o, $075CEC71);
  o := RawLdapTranslateFilter('accountBalance<=1234');
  CheckHash(o, $075CEC71);
  o := RawLdapTranslateFilter('(givenName~=John)');
  CheckHash(o, $7C293651);
  o := RawLdapTranslateFilter('(&(|(cn=Jon)(sn=Brion)(!(cn=Alex))))');
  CheckHash(o, $ADF30CEA);
  o := RawLdapTranslateFilter('&(objectCategory=person)(objectClass=user)' +
    '(useraccountcontrol:1.2.840.113556.1.4.803:=16)');
  CheckHash(o, $82EE2554);
  //writeln(AsnDump(o));
  CheckEqual(RawLdapTranslateFilter('(givenName=John', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('(!(givenName=John)', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('!', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('&', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('|', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('! ', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('& ', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('| ', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('!( )', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('&()', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('| ( )', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('(toto)', {noraise=}true), '');
  CheckEqual(RawLdapTranslateFilter('x', {noraise=}true), '');
  // validate LDAP attributes definitions
  n := 0;
  ats2 := [];
  for at := low(at) to high(at) do
  begin
    CheckEqual(ToText(at), AttrTypeName[at]);
    CheckUtf8(AttributeNameType(AttrTypeName[at]) = at, AttrTypeName[at]);
    u := AttrTypeName[at];
    CheckEqual(u, AttrTypeName[at]);
    Check(pointer(u) = pointer(AttrTypeName[at]));
    UpperCaseSelf(u);
    Check(IdemPropNameU(u, AttrTypeName[at]));
    Check((u = '') or (pointer(u) <> pointer(AttrTypeName[at])));
    AttributeNameNormalize(u);
    CheckEqual(u, AttrTypeName[at]);
    Check(pointer(u) = pointer(AttrTypeName[at]));
    ats := [at];
    a := ToText(ats);
    if at = low(at) then
      Check(a = nil)
    else
    begin
      CheckEqual(length(a), 1);
      CheckEqual(a[0], ToText(at));
      include(ats2, at);
      inc(n);
      a := ToText(ats2);
      CheckEqual(length(a), n);
      for i := 0 to n - 1 do
        CheckEqual(a[i], ToText(TLdapAttributeType(i + 1)));
    end;
  end;
  for i := low(AttrTypeNameAlt) to high(AttrTypeNameAlt) do
    CheckUtf8(AttributeNameType(AttrTypeNameAlt[i]) = AttrTypeAltType[i],
      AttrTypeNameAlt[i]);
  ats := [];
  Check(ToText(ats) = nil);
  a := ToText([atOrganizationUnitName, atObjectClass, atCommonName]);
  CheckEqual(RawUtf8ArrayToCsv(a), 'objectClass,cn,ou');
  // validate LDAP attributes values and high-level recognition
  for sat := low(sat) to high(sat) do
  begin
    c := SamAccountTypeValue(sat);
    Check((c = 0) = (sat = satUnknown));
    Check(SamAccountTypeFromText(UInt32ToUtf8(c)) = sat);
  end;
  for gt := low(gt) to high(gt) do
  begin
    gts := [gt];
    Check(GroupTypesFromInteger(GroupTypesValue(gts)) = gts);
  end;
  gts := [];
  Check(GroupTypesValue(gts) = 0);
  for gt := low(gt) to high(gt) do
  begin
    include(gts, gt);
    Check(GroupTypesFromInteger(GroupTypesValue(gts)) = gts);
  end;
  for ua := low(ua) to high(ua) do
  begin
    uas := [ua];
    Check(UserAccountControlsFromInteger(UserAccountControlsValue(uas)) = uas);
  end;
  uas := [];
  Check(UserAccountControlsValue(uas) = 0);
  for ua := low(ua) to high(ua) do
  begin
    include(uas, ua);
    Check(UserAccountControlsFromInteger(UserAccountControlsValue(uas)) = uas);
  end;
  for sf := low(sf) to high(sf) do
  begin
    sfs := [sf];
    Check(SystemFlagsFromInteger(SystemFlagsValue(sfs)) = sfs);
  end;
  sfs := [];
  Check(SystemFlagsValue(sfs) = 0);
  for sf := low(sf) to high(sf) do
  begin
    include(sfs, sf);
    Check(SystemFlagsFromInteger(SystemFlagsValue(sfs)) = sfs);
  end;
  // validate LDAP resultset and LDIF content
  rl2 := nil;
  rl := TLdapResultList.Create;
  try
    u := rl.Dump({noTime=}true);
    CheckEqual(u, 'results: 0' + CRLF);
    rl2 := CopyObject(rl);
    Check(rl2 <> nil);
    Check(rl2.ClassType = TLdapResultList);
    CheckEqual(rl2.Dump({noTime=}true), u);
    CheckEqual(rl.ExportToLdifContent,
      'version: 1'#$0A'# total number of entries: 0'#$0A);
    CheckEqual(rl.Count, 0);
    CheckEqual(rl.GetJson, '{}');
    r := rl.Add;
    r.ObjectName := 'cn=foo, ou=bar';
    CheckEqual(r.ObjectName, 'CN=foo,OU=bar', 'normalized');
    CheckEqual(r.Attributes.Count, 0);
    v := 'John E Doxx';
    PWord(PAnsiChar(UniqueRawUtf8(v)) + 9)^ := $a9c3; // UTF-8 'e'acute (Delphi)
    r.Attributes[atObjectClass] := 'person';
    CheckEqual(r.Attributes.Count, 1);
    r.Attributes.AddPairs(['cn', 'John Doe',
                           'CN', v,           // CN will be identified as cn
                           'Sn', 'Doe']);     // Sn will be normalized as sn
    CheckEqual(r.Attributes.Count, 3);
    json := rl.GetJson([]);
// '{"bar":{"foo":{"objectName":"CN=foo,OU=bar","objectClass":"person","cn":["John Doe",v],"sn":"Doe"}}}'
    CheckHash(json, $8AAB69D2);
    CheckHash(rl.GetJson([roRawValues]), $8AAB69D2);
    CheckHash(rl.GetJson([roNoDCAtRoot]), $8AAB69D2);
    CheckHash(rl.GetJson([roNoObjectName]), $6A4853FA);
    CheckHash(rl.GetJson([roCanonicalNameAtRoot]), $20AF5125);
    CheckHash(rl.GetJson([roObjectNameAtRoot]), $92FE1BFD);
    CheckHash(rl.GetJson([roCommonNameAtRoot]), $047EED2F);
    CheckHash(rl.GetJson([roObjectNameWithoutDCAtRoot, roNoObjectName]), $F41233F2);
    CheckHash(rl.GetJson([roWithCanonicalName]), $C4BA2ED3);
    CheckHash(rl.GetJson([roNoObjectName, roWithCanonicalName]), $0BCFC3BC);
    c := {$ifdef OSWINDOWS}$8D553E5D{$else}$D74DDA27{$endif};
    CheckHash(rl.Dump({noTime=}true), c, 'hashDump');
    CheckHash(rl.ExportToLdifContent, $4A97B4B2, 'hashLdif');
    CopyObject(rl, rl2);
    CheckHash(rl2.Dump({noTime=}true), c, 'hashDump2');
    CheckHash(rl2.ExportToLdifContent, $4A97B4B2, 'hashLdif2');
    CheckHash(rl2.ExportToLdifContent({human=}true), $E6C54523, 'hashLdif3');
    r.Attributes.Delete(atCommonName);
    CheckEqual(r.Attributes.Count, 2);
    v := rl.GetJson([roNoObjectName]);
    CheckEqual(v, '{"bar":{"foo":{"objectClass":"person","sn":"Doe"}}}');
    r.ObjectName := 'cn=foo, ou=bar, dc=toto, dc=it';
    CheckHash(rl.GetJson([]), $DF03674D);
    CheckHash(rl.GetJson([roRawValues]), $DF03674D);
    CheckHash(rl.GetJson([roNoDCAtRoot]), $DB4EF1DC);
    CheckEqual(rl.GetJson([roNoObjectName, roNoDCAtRoot]), v);
    CheckHash(rl.ExportToLdifContent, $31A4283C, 'hashLdif4');
    o := RandomWinAnsi(200 + Random32(300));   // force not UTF-8
    Check(not IsValidUtf8(o), 'utf8');
    r.Attributes.Add('jpegphoto', o);
    o := RandomUri(50 + Random32(200)); // force pure US-ASCII
    r.Attributes.Add('longname', o); //.KnownType := atAlias;
    u := rl.ExportToLdifContent({human=}false, {maxline=}80);
    CheckEqual(rl.ExportToLdifContent({human=}true, {maxline=}80), u);
    v := rl.ExportToLdifContent({human=}false, {maxline=}0);
    CheckNotEqual(v, u);
    u := StringReplaceAll(u, #10' ', ''); // mimics maxline=0
    CheckEqual(v, u);
    CheckEqual(rl.ExportToLdifContent({human=}true,  {maxline=}0), u);
//sleep(1010); consolewrite(rl.ExportToLdifContent(true));
  finally
    rl.Free;
    rl2.Free;
  end;
  // validate LDAP settings
  l := TLdapClientSettings.Create;
  try
    CheckEqual(l.TargetUri, '');
    CheckEqual(l.KerberosDN, '');
    l.TargetHost := 'ad.synopse.info';
    CheckEqual(l.TargetUri, 'ldap://ad.synopse.info');
    l.Tls := true;
    CheckEqual(l.TargetUri, 'ldaps://ad.synopse.info:389');
    l.TargetPort := LDAP_TLS_PORT;
    CheckEqual(l.TargetUri, 'ldaps://ad.synopse.info');
    l.TargetPort := '1234';
    u := l.TargetUri;
    CheckEqual(u, 'ldaps://ad.synopse.info:1234');
    l.TargetUri := 'http://ad.synopse.com';
    CheckEqual(l.TargetUri, '');
    l.TargetUri := 'ldap2://ad.synopse.com';
    CheckEqual(l.TargetUri, '');
    l.TargetUri := 'ldap://ad.synopse.com';
    CheckEqual(l.TargetUri, 'ldap://ad.synopse.com');
    l.TargetUri := 'ad.synopse.info';
    CheckEqual(l.TargetUri, 'ldap://ad.synopse.info');
    CheckEqual(l.KerberosDN, '');
  finally
    l.Free;
  end;
  l := TLdapClientSettings.Create;
  try
    CheckEqual(l.TargetUri, '');
    CheckEqual(l.KerberosDN, '');
    l.TargetHost := 'dc.synopse.com';
    CheckEqual(l.TargetUri, 'ldap://dc.synopse.com');
    CheckEqual(l.KerberosDN, '');
    l.KerberosDN := 'ad.synopse.com';
    v := l.TargetUri;
    CheckEqual(v, 'ldap://dc.synopse.com/ad.synopse.com');
    l.TargetUri := u;
    CheckEqual(l.TargetUri, u);
    CheckEqual(l.TargetUri, 'ldaps://ad.synopse.info:1234');
    CheckEqual(l.KerberosDN, '');
    l.TargetUri := v;
    CheckEqual(l.TargetUri, v);
    CheckEqual(l.KerberosDN, 'ad.synopse.com');
  finally
    l.Free;
  end;
  // optional LDAP client tests
  if Executable.Command.Get(['dns'], dns) then
    for i := 0 to high(dns) do
    begin
      // syntax is -dns server1 [-dns server2]
      clients := DnsLdapServices(dns[i]);
      if clients = nil then
      begin
        CheckUtf8(false, 'no Ldap for --dns %', [dns[i]]);
        continue;
      end;
      main := CldapGetBestLdapController(clients, dns[i], '');
      utc1 := GetSntpTime(dns[i]);
      if utc1 <> 0 then
      begin
        utc2 := NowUtc;
        AddConsole('% : % = %',
          [dns[i], DateTimeMSToString(utc1), DateTimeMSToString(utc2)]);
        if withntp then
          CheckSame(utc1, utc2, 1, 'NTP system B'); // allow 1 day diff
      end;
      for j := 0 to high(clients) do
      begin
        txt := '';
        if clients[j] = main then
          txt := ' (main)';
        one := TLdapClient.Create;
        try
          one.Settings.TargetUri := clients[j];
          one.Settings.KerberosDN := dns[i];
          try
            if Executable.Command.Get('ldapusr', usr) and
               Executable.Command.Get('ldappwd', pwd) then
            begin
              one.Settings.UserName := usr;
              one.Settings.Password := pwd;
              if Executable.Command.Option('ldaps') then
              begin
                // plain over TLS
                one.Settings.TargetPort := LDAP_TLS_PORT; // force TLS
                if one.Bind then
                  AddConsole('connected to % with TLS + plain Bind',
                    [one.Settings.TargetUri])
                else
                begin
                  CheckUtf8(false, 'Bind % res=% [%]%',
                    [one.Settings.TargetUri, one.ResultCode, one.ResultString, txt]);
                  continue;
                end;
              end
              else
                // Windows/SSPI and POSIX/GSSAPI with no prior loggued user
                if one.BindSaslKerberos('', @ku) then
                  AddConsole('connected to % with specific user % = %',
                    [one.Settings.TargetUri, usr, ku])
                else
                begin
                  CheckUtf8(false, '% on ldap:% [%]%',
                    [usr, clients[j], one.ResultString, txt]);
                  continue;
                end;
            end
            else
              // Windows/SSPI and POSIX/GSSAPI with a prior loggued user (kinit)
              if one.BindSaslKerberos('', @ku) then
                AddConsole('connected to % with current Kerberos user %',
                  [one.Settings.TargetUri, ku])
              else
              begin
                CheckUtf8(false, 'currentuser on ldap:% [%]%',
                  [clients[j], one.ResultString, txt]);
                continue;
              end;
            Check(one.NetbiosDN <> '', 'NetbiosDN');
            Check(one.ConfigDN <> '', 'ConfigDN');
            Check(one.Search(one.WellKnownObject(lkoUsers), {typesonly=}false,
                  '(cn=users)', ['*']), 'Search');
            Check(one.SearchResult.Count <> 0, 'SeachResult');
            AddConsole('%% = % search=%', [one.Settings.TargetHost, txt,
              one.NetbiosDN, one.SearchResult.Count]);
            for k := 0 to one.SearchResult.Count - 1 do
            begin
              res := one.SearchResult.Items[k];
              Check(res.ObjectName <> '', 'objectName');
              Check(res[atDistinguishedName] <> '', 'distinguishedName');
              sid := '';
              if res.CopyObjectSid(sid) then
                Check(sid <> '');
              FillZero(guid);
              Check(res.CopyObjectGuid(guid), 'objectGUID');
              Check(not IsNullGuid(guid));
              CheckEqual(res.CanonicalName, DNToCN(res.ObjectName));
              Check(IdemPropNameU(res.Attributes[atCommonName], 'users'), 'cn');
              Check(res.Attributes.GetByName('name') <> '', 'name');
              Check(res.Attributes.SystemFlags <> [], 'sf');
            end;
            //writeln(one.SearchResult.Dump);
          except
            on E: Exception do
              Check(false, E.Message);
          end;
        finally
          one.Free;
        end;
      end;
    end;
end;

type
   TTunnelExecute = class
   public
     local: TTunnelLocal;
     session: TTunnelSession;
     remote: ITunnelTransmit;
     signcert, verifcert: ICryptCert;
   end;

function TNetworkProtocols.TunnelBackgroundOpen(l: TTunnelLocal; s: TTunnelSession;
  const r: ITunnelTransmit; const sc, vc: ICryptCert): TLoggedWorkThread;
var
  context: TTunnelExecute;
  name: RawUtf8;
begin
  context := TTunnelExecute.Create;
  context.local := l;
  context.session := s;
  context.remote := r;
  context.signcert := sc;
  context.verifcert := vc;
  inc(tunnelsequence);
  Make(['open', tunnelsequence], name);
  result := TLoggedWorkThread.Create(TSynLog, name, context,
    TunnelExecute, {suspended=}false, {ManualWaitForAndFree=}true);
end;

procedure TNetworkProtocols.TunnelExecute(Sender: TObject);
var
  exec: TTunnelExecute;
  port: TNetPort;
begin
  exec := Sender as TTunnelExecute;
  // one of the two handshakes should be done in another thread
  if not CheckFailed(exec <> nil) then
  try
    check(exec.local <> nil);
    check(exec.session <> 0);
    with exec do
      port := local.Open(session, remote, tunneloptions, 1000, tunnelappsec,
        cLocalHost, ['remoteHost', Executable.Host], signcert, verifcert);
    check(port <> 0);
    checkEqual(port, exec.local.Port);
    check(exec.local.Port <> 0);
    check(exec.local.RemotePort <> 0);
  finally
    exec.Free; // always free transient call parameters
  end;
end;

procedure TNetworkProtocols.CheckBlocks(const log: ISynLog; const sent, recv: RawByteString; num: integer);
begin
  CheckUtf8(sent = recv, 'block% %=%', [num, length(sent), length(recv)]);
  if (sent <> recv) and
     Assigned(log) then
  begin
    log.Log(sllDebug, 'block%: sent=%', [num, sent], self);
    log.Log(sllDebug, 'block%: recv=%', [num, recv], self);
  end;
end;

procedure TNetworkProtocols.TunnelSocket(const log: ISynLog; var rnd: TLecuyer;
  clientinstance, serverinstance: TTunnelLocal; packets: integer);
var
  i: integer;
  nr: TNetResult;
  clientsock, serversock: TNetSocket;
  local, remote: TNetPort;
  closed: PBoolean;
  sent, sent2: RawUtf8;
  received, received2: RawByteString;
  nfo: variant;
begin
  local := clientinstance.Port;
  remote := serverinstance.Port;
  Check(local <> 0, 'no local');
  Check(remote <> 0, 'no remote');
  Check(remote = clientinstance.RemotePort);
  Check(local = serverinstance.RemotePort);
  Check(clientinstance.LocalPort <> '', 'no client localport');
  Check(serverinstance.LocalPort <> '', 'no server localport');
  Check(serverinstance.LocalPort <> clientinstance.LocalPort, 'ports');
  if Assigned(log) then
    log.Log(sllTrace, 'TunnelTest: sockets start', self);
  nr := NewSocket('127.0.0.1', clientinstance.LocalPort, nlTcp, {bind=}false,
    1000, 1000, 1000, 0, clientsock);
  CheckUtf8(nr = nrOk, 'clientsock=%', [_NR[nr]]);
  nr := NewSocket('127.0.0.1', serverinstance.LocalPort, nlTcp, {bind=}false,
    1000, 1000, 1000, 0, serversock);
  CheckUtf8(nr = nrOk, 'serversock=%', [_NR[nr]]);
  if not CheckFailed(Assigned(clientinstance.Thread), 'no client thread') and
     not CheckFailed(Assigned(serverinstance.Thread), 'no server thread') then
  try
    // validate raw TCP tunnelling
    if Assigned(log) then
      log.Log(sllTrace, 'TunnelTest: sockets started', self);
    CheckEqual(clientinstance.BytesIn, 0);
    CheckEqual(clientinstance.BytesOut, 0);
    CheckEqual(serverinstance.BytesIn, 0);
    CheckEqual(serverinstance.BytesOut, 0);
    for i := 1 to packets do
    begin
      rnd.FillAscii(rnd.Next(200) + 1, sent);
      PByteArray(sent)[length(sent) shr 1] := 0;
      rnd.FillAscii(rnd.Next(200) + 1, sent2);
      PByteArray(sent2)[length(sent2) shr 1] := ord('"');
      nr := clientsock.SendAll(pointer(sent), length(sent));
      CheckUtf8(nr = nrOk, 'SendAll1=%', [_NR[nr]]);
      nr := serversock.RecvWait(1000, received);
      CheckUtf8(nr = nrOk, 'RecvWait1=%', [_NR[nr]]);
      CheckBlocks(log, sent, received, 1);
      if CheckFailed(clientinstance.Thread.Processing, 'no client process') or
         CheckFailed(serverinstance.Thread.Processing, 'no server process') then
        break; // don't try any further
      nr := clientsock.SendAll(pointer(sent2), length(sent2));
      CheckUtf8(nr = nrOk, 'SendAll2=%', [_NR[nr]]);
      nr := serversock.SendAll(pointer(sent), length(sent));
      CheckUtf8(nr = nrOk, 'SendAll3=%', [_NR[nr]]);
      nr := clientsock.RecvWait(1000, received);
      CheckUtf8(nr = nrOk, 'RecvWait2=%', [_NR[nr]]);
      nr := serversock.RecvWait(1000, received2);
      CheckUtf8(nr = nrOk, 'RecvWait3=%', [_NR[nr]]);
      CheckBlocks(log, sent, received, 2);
      CheckBlocks(log, sent2, received2, 3);
      CheckEqual(clientinstance.BytesIn, serverinstance.BytesOut);
      CheckEqual(clientinstance.BytesOut, serverinstance.BytesIn);
      Check(clientinstance.BytesIn <> 0);
      Check(clientinstance.BytesOut <> 0);
      Check(serverinstance.BytesIn <> 0);
      Check(serverinstance.BytesOut <> 0);
    end;
    Check(clientinstance.BytesIn < clientinstance.BytesOut, 'smaller');
    Check(serverinstance.BytesIn > serverinstance.BytesOut, 'bigger');
    CheckEqual(serverinstance.FramesIn, clientinstance.FramesOut, 'frames1');
    CheckEqual(serverinstance.FramesOut, clientinstance.FramesIn, 'frames2');
    nfo := serverinstance.TunnelInfo;
    Check(_Safe(nfo)^.Count > 4);
    if Assigned(log) then
      log.Log(sllTrace, 'TunnelTest: server=%', [nfo], self);
    nfo := clientinstance.TunnelInfo;
    Check(_Safe(nfo)^.Count > 4);
    if Assigned(log) then
      log.Log(sllTrace, 'TunnelTest: client=%', [nfo], self);
  finally
    if Assigned(log) then
      log.Log(sllTrace, 'TunnelTest: sockets stop', self);
    clientsock.ShutdownAndClose(true);
    serversock.ShutdownAndClose(true);
  end;
  clientinstance.ClosePort;
  closed := @serverinstance.Closed; // trick to access this propery by value
  SleepHiRes(1000, closed^);
end;

procedure TNetworkProtocols.TunnelTest(var rnd: TLecuyer;
  const clientcert, servercert: ICryptCert; packets: integer);
var
  log: ISynLog;
  sess: TTunnelSession;
  clientinstance, serverinstance: TTunnelLocal;
  clienttunnel, servertunnel: ITunnelLocal;
  local, remote: TNetPort;
  worker: TLoggedWorkThread;
begin
  // setup the two instances with the specified options and certificates
  TSynLogTestLog.EnterLocal(log, 'TunnelTest [%]', [ToText(tunneloptions)], self);
  clientinstance := TTunnelLocalClient.Create(TSynLog);
  serverinstance := TTunnelLocalServer.Create(TSynLog);
  //clientinstance.VerboseLog := true;
  //serverinstance.VerboseLog := true;
  clienttunnel := clientinstance;
  servertunnel := serverinstance;
  // perform handshaking
  repeat
    sess := rnd.Next31;
  until sess <> 0;
  rnd.FillAscii(10, tunnelappsec);
  worker := TunnelBackgroundOpen(
    serverinstance, sess, clienttunnel, servercert, clientcert);
  try
    local := clientinstance.Open(
      sess, servertunnel, tunneloptions, 1000, tunnelappsec, clocalhost,
      ['remoteHost', Executable.Host], clientcert, servercert);
    worker.WaitFinished(5000);
  finally
    worker.Free;
  end;
  remote := clienttunnel.RemotePort;
  CheckEqual(local, servertunnel.RemotePort);
  CheckEqual(remote, serverinstance.Port);
  Check(clienttunnel.Encrypted = (toEncrypted * tunneloptions <> []), 'cEncrypted');
  Check(servertunnel.Encrypted = (toEncrypted * tunneloptions <> []), 'sEncrypted');
  // create two local sockets and let them play with the tunnel
  TunnelSocket(log, rnd, clientinstance, serverinstance, packets);
  // avoid circular references memory leak (not needed over SOA websockets)
  clientinstance.RawTransmit := nil;
end;

const
  AGENT_COUNT = 7;
  CONSOLE_COUNT = 4;

procedure TNetworkProtocols.TunnelRelay(relay: TTunnelRelay;
  const agent: array of ITunnelAgent; const console: array of ITunnelConsole;
  var rnd: TLecuyer; packets: integer);
var
  log: ISynLog;
  a: ITunnelAgent;
  session: array of TTunnelSession;
  worker: array of TLoggedWorkThread; // as multi-threaded as possible
  agentcallback, consolecallback: array of ITunnelTransmit;
  agentlocal, consolelocal: array of TTunnelLocal;
  sess: TTunnelSession;
  i, j, c: PtrInt;
  local: TNetPort;
begin
  TSynLogTestLog.EnterLocal(log, self, 'TTunnelRelay');
  if CheckFailed(length(console) <> 0) then
    exit; // avoid division per zero in "i mod length(console)" below
  // emulate connection of AGENT_COUNT tunnels using several consoles
  // (see ITunnelOpen main comment about the typical TTunnelRelay steps)
  SetLength(agentcallback, AGENT_COUNT);
  SetLength(consolecallback, AGENT_COUNT);
  SetLength(agentlocal, AGENT_COUNT);
  SetLength(consolelocal, AGENT_COUNT);
  // 1) TTunnelLocal.Create() to have an ITunnelTransmit callback
  if Assigned(log) then
    log.Log(sllInfo, 'Tunnel: create % TTunnelLocal callbacks', [AGENT_COUNT], self);
  for i := 0 to AGENT_COUNT - 1 do
  begin
    agentlocal[i]   := TTunnelLocalClient.Create(TSynLog);;
    consolelocal[i] := TTunnelLocalServer.Create(TSynLog);
    agentcallback[i]   := agentlocal[i];
    consolecallback[i] := consolelocal[i];
    check(Assigned(agentcallback[i]));
    check(Assigned(consolecallback[i]));
  end;
  // 2) ITunnelConsole/ITunnelAgent.TunnelPrepare/TunnelAccept
  if Assigned(log) then
    log.Log(sllInfo, 'Tunnel: ITunnelOpen.TunnelPrepare', self);
  SetLength(session, AGENT_COUNT);
  for i := 0 to AGENT_COUNT - 1 do
  begin
    c := i mod length(console); // round-robin of agents over consoles
    Check(c <= high(console));
    if i >= length(agent) then
      a := agent[0]
    else
      a := agent[i];
    if (i and 3) = 0 then // initiate from one endpoint or the other
    begin
      sess := console[c].TunnelPrepare(consolecallback[i]);
      check(a.TunnelAccept(sess, agentcallback[i]));
    end
    else
    begin
      sess := a.TunnelPrepare(agentcallback[i]);
      check(console[c].TunnelAccept(sess, consolecallback[i]));
    end;
    check(sess <> 0, 'session=0');
    for j := 0 to i - 1 do
      check(session[j] <> sess, 'unique session');
    session[i] := sess;
  end;
  if not CheckEqual(relay.ConsoleCount, length(console), 'ConsoleCount') then
    exit; // all console[] should be connected to the relay
  // 3) TTunnelLocal.Open() on the console and agent sides
  if Assigned(log) then
    log.Log(sllInfo, 'Tunnel: reciprocal Open() handshake', self);
  try
    SetLength(worker, AGENT_COUNT);
    for i := 0 to AGENT_COUNT - 1 do
    begin
      if i >= length(agent) then
        a := agent[0]
      else
        a := agent[i];
      worker[i] := TunnelBackgroundOpen(agentlocal[i], session[i], a, nil, nil);
      c := i mod length(console); // round-robin of agents over consoles
      local := consolelocal[i].Open(
        session[i], console[c], tunneloptions, 1000, tunnelappsec, cLocalhost,
        ['agentNumber', i]);
      check(local <> 0, 'local');
      checkEqual(local, consolelocal[i].Port);
    end;
    if Assigned(log) then
      log.Log(sllInfo, 'Tunnel: wait for background threads', self);
    for i := 0 to AGENT_COUNT - 1 do
    begin
      worker[i].WaitFinished(1000);
      CheckEqual(agentlocal[i].RemotePort, consolelocal[i].Port);
      CheckEqual(agentlocal[i].Port, consolelocal[i].RemotePort);
    end;
    // 5a) ITunnelOpen.TunnelCommit or TunnelRollback against Open() result
    if Assigned(log) then
      log.Log(sllInfo, 'Tunnel: all TunnelCommit()', self);
    for i := 0 to AGENT_COUNT - 1 do
    begin
      if i >= length(agent) then
        a := agent[0]
      else
        a := agent[i];
      Check(a.TunnelCommit(session[i]));
      c := i mod length(console); // round-robin of agents over consoles
      Check(console[c].TunnelCommit(session[i]));
    end;
    // create two local sockets and let them play with each tunnel
    if Assigned(log) then
      log.Log(sllInfo, 'Tunnel: actual sockets relay on loopback', self);
    for i := 0 to AGENT_COUNT - 1 do
      TunnelSocket(log, rnd, agentlocal[i], consolelocal[i], packets);
  finally
    for i := 0 to AGENT_COUNT - 1 do
      worker[i].Free;
  end;
  // release internal references
  if Assigned(log) then
    log.Log(sllInfo, 'Tunnel: finalize agent/console references', self);
  // retrieve SOA agents + consoles endpoints (emulated on stack)
  agentcallback := nil;
  consolecallback := nil;
end;

const
  WS_KEY = ''; // no encryption is needed at WebSockets level - Tunnel ecdhe

procedure TNetworkProtocols.Tunnel;
var
  clientcert, servercert: ICryptCert;
  bak: TSynLogLevels;
  relay: TTunnelRelay;
  rnd: TLecuyer;
  i: PtrInt;
  agent: array of ITunnelAgent;     // single instance (sicShared mode)
  console: array of ITunnelConsole; // one per console (sicPerSession)
  restserver: TRestServerFullMemory;
  httpserver: TRestHttpServer;
  agentclient, consoleclient: array of TRestHttpClientWebsockets;
begin
  bak := TSynLog.Family.Level;
  TSynLog.Family.Level := LOG_VERBOSE; // for convenient LUTI debugging
  // 1. validate TTunnelLocal and all its handshaking options
  RandomLecuyer(rnd);
  // plain tunnelling
  TunnelTest(rnd, nil, nil);
  // symmetric secret encrypted tunnelling
  tunneloptions := [toEncrypt];
  TunnelTest(rnd, nil, nil);
  // ECDHE encrypted tunnelling
  tunneloptions := [toEcdhe];
  TunnelTest(rnd, nil, nil);
  // tunnelling with mutual authentication
  tunneloptions := [];
  clientcert := Cert('syn-es256').Generate([cuDigitalSignature]);
  servercert := Cert('syn-es256').Generate([cuDigitalSignature]);
  TunnelTest(rnd, clientcert, servercert);
  // symmetric secret encrypted tunnelling with mutual authentication
  tunneloptions := [toEncrypt];
  TunnelTest(rnd, clientcert, servercert);
  // ECDHE encrypted tunnelling with mutual authentication
  tunneloptions := [toEcdhe];
  TunnelTest(rnd, clientcert, servercert);
  // options (e.g. encryption/ecdhe) are now considered validated
  tunneloptions := [];
  // 2. validate TTunnelRelay and its associated TTunnelAgent/TTunnelConsole
  SetLength(console, CONSOLE_COUNT); // several agents per console
  relay := TTunnelRelay.Create(TSynLog, {timeoutsecs=}120);
  try
    // 2.1. no SOA transmission first
    TSynLog.Add.Log(sllInfo, 'Tunnel: retrieve SOA endpoints', self);
    SetLength(agent, 1);
    Check(relay.Resolve(ITunnelAgent, agent[0]), 'sicShared agent');
    Check(Assigned(agent[0]));
    CheckEqual(relay.ConsoleCount, 0);
    for i := 0 to high(console) do
      Check(relay.Resolve(ITunnelConsole, console[i]), 'sicPerSession');
    TunnelRelay(relay, agent, console, rnd, {packets=}10);
    agent := nil;
    console := nil;
    // 2.2. setup a SOA WebSockets server as actual relay over WebSockets
    TSynLog.Add.Log(sllInfo, 'Tunnel: start WebSockets server', self);
    restserver := TRestServerFullMemory.CreateWithOwnModel(
      [], {withauth=}true, 'tun');
    try
      restserver.Options := restserver.Options +
        [rsoSessionInConnectionOpaque, // also validate some security options :)
         rsoPerConnectionNonce];
      // validate SetUser('User', 'password') below using mcfMd5Crypt
      AuthUserDefaultPassword := '$1$3azHgidD$SrJPt7B.9rekpmwJwtON31';
      restserver.Server.CreateMissingTables;
      AuthUserDefaultPassword := DEFAULT_HASH_SYNOPSE;
      restserver.ServiceDefine(relay.Agent, [ITunnelAgent]);
      restserver.ServiceContainer.InjectResolver([relay]);
      restserver.ServiceDefine(TTunnelconsole, [ITunnelConsole], sicPerSession);
      restserver.LogClass := TSynLog;
      WebSocketLog := TSynLog;
      httpserver := TRestHttpServer.Create('8888', [restserver], '+',
        WEBSOCKETS_DEFAULT_MODE, {threads=}2); // TunnelRelay() is sequential
      try
        httpserver.WebSocketsEnable('', WS_KEY, {WSjson=}false, [])^.
          SendDelay := 0; // disable frame gathering on the loopback
        // setup as many SOA clients over WebSockets as needed
        TSynLog.Add.Log(sllInfo, 'Tunnel: connect agentclient[]', self);
        SetLength(agentclient, AGENT_COUNT);
        SetLength(agent, AGENT_COUNT);
        for i := 0 to high(agentclient) do
        begin
          agentclient[i] := TRestHttpClientWebsockets.CreateWithOwnModel(
            'localhost', '8888', 'tun');
          agentclient[i].WebSocketsUpgrade(WS_KEY, false, []);
          check(agentclient[i].SetUser('User', 'password'), 'setuser1');
          agentclient[i].ServiceDefine(ITunnelAgent, sicShared);
          agentclient[i].Resolve(ITunnelAgent, agent[i]);
        end;
        TSynLog.Add.Log(sllInfo, 'Tunnel: connect consoleclient[]', self);
        SetLength(consoleclient, CONSOLE_COUNT);
        SetLength(console, CONSOLE_COUNT);
        for i := 0 to high(consoleclient) do
        begin
          consoleclient[i] := TRestHttpClientWebsockets.CreateWithOwnModel(
            'localhost', '8888', 'tun');
          consoleclient[i].WebSocketsUpgrade(WS_KEY, false, []);
          check(consoleclient[i].SetUser('User', 'password'), 'setuser2');
          consoleclient[i].ServiceDefine(ITunnelConsole, sicPerSession);
          consoleclient[i].Resolve(ITunnelConsole, console[i]);
        end;
        TSynLog.Add.Log(sllInfo, 'Tunnel: call TunnelRelay', self);
        TunnelRelay(relay, agent, console, rnd, {packets=}5);
      finally
        agent := nil; // keep refcount clean
        console := nil;
        ObjArrayClear(consoleclient);
        ObjArrayClear(agentclient);
        httpserver.Free;
      end;
    finally
      restserver.Free;
    end;
  finally
    TSynLog.Add.Log(sllInfo, 'Tunnel: eventual TTunnelRelay.Free', self);
    relay.Free;
  end;
  // 3. cleanup
  TSynLog.Family.Level := bak;
end;

procedure TNetworkProtocols.IPAddresses;
var
  i, n, n2: PtrInt;
  s: ShortString;
  txt, uri: RawUtf8;
  ip: THash128Rec;
  sub: TIp4SubNets;
  bin, bin2: RawByteString;
  timer: TPrecisionTimer;
begin
  FillZero(ip.b);
  Check(IsZero(ip.b));
  IP4Short(@ip, s);
  Check(s = '0.0.0.0');
  IP4Text(@ip, txt);
  CheckEqual(txt, '');
  IP6Short(@ip, s);
  Check(s = '::', '::');
  IP6Text(@ip, txt);
  CheckEqual(txt, '');
  ip.b[15] := 1;
  IP6Short(@ip, s);
  Check(s = '::1', '::1');
  IP6Text(@ip, txt);
  CheckEqual(txt, '127.0.0.1', 'IPv6 loopback');
  ip.b[0] := 1;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100::1');
  ip.b[15] := 0;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100::');
  ip.b[6] := $70;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100:0:0:7000::');
  for i := 0 to 7 do
    ip.b[i] := i;
  IP6Text(@ip, txt);
  CheckEqual(txt, '1:203:405:607::');
  for i := 8 to 15 do
    ip.b[i] := i;
  IP6Text(@ip, txt);
  CheckEqual(txt, '1:203:405:607:809:a0b:c0d:e0f');
  for i := 0 to 15 do
    ip.b[i] := i or $70;
  IP6Text(@ip, txt);
  CheckEqual(txt, '7071:7273:7475:7677:7879:7a7b:7c7d:7e7f');
  Check(mormot.core.text.HexToBin('200100B80A0B12F00000000000000001', PByte(@ip), 16));
  IP6Text(@ip, txt);
  CheckEqual(txt, '2001:b8:a0b:12f0::1');
  CheckEqual(IP4Netmask(1), $00000080);
  CheckEqual(IP4Netmask(8), $000000ff);
  CheckEqual(IP4Netmask(24), $00ffffff);
  CheckEqual(IP4Netmask(31), $feffffff);
  CheckEqual(IP4Netmask(32), $ffffffff);
  CheckEqual(IP4Netmask(0),  0, 'invalid prefix=0');
  CheckEqual(IP4Netmask(33), 0, 'invalid prefix=33');
  CheckEqual(IP4Prefix('128.0.0.0'), 1);
  CheckEqual(IP4Prefix('192.0.0.0'), 2);
  CheckEqual(IP4Prefix('254.0.0.0'), 7);
  CheckEqual(IP4Prefix('255.0.0.0'), 8);
  CheckEqual(IP4Prefix('255.128.0.0'), 9);
  CheckEqual(IP4Prefix('255.192.0.0'), 10);
  CheckEqual(IP4Prefix('255.254.0.0'), 15);
  CheckEqual(IP4Prefix('255.255.0.0'), 16);
  CheckEqual(IP4Prefix('255.255.128.0'), 17);
  CheckEqual(IP4Prefix('255.255.192.0'), 18);
  CheckEqual(IP4Prefix('255.255.254.0'), 23);
  CheckEqual(IP4Prefix('255.255.255.0'), 24);
  CheckEqual(IP4Prefix('255.255.255.128'), 25);
  CheckEqual(IP4Prefix('255.255.255.252'), 30);
  CheckEqual(IP4Prefix('255.255.255.254'), 31);
  CheckEqual(IP4Prefix('255.255.255.255'), 32);
  CheckEqual(IP4Prefix(''), 0, 'invalid netmask 1');
  CheckEqual(IP4Prefix('0.0.0.0'), 0, 'invalid netmask 2');
  CheckEqual(IP4Prefix('0.0.0.1'), 0, 'invalid netmask 3');
  CheckEqual(IP4Prefix('255.254.1.0'), 0, 'invalid netmask 4');
  CheckEqual(IP4Prefix('255.255.255.256'), 0, 'invalid netmask 5');
  CheckEqual(IP4Subnet('192.168.1.135', '255.255.255.0'), '192.168.1.0/24');
  Check(IP4Match('192.168.1.1',   '192.168.1.0/24'), 'match1');
  Check(IP4Match('192.168.1.135', '192.168.1.0/24'), 'match2');
  Check(IP4Match('192.168.1.250', '192.168.1.0/24'), 'match3');
  Check(not IP4Match('192.168.2.135', '192.168.1.0/24'), 'match4');
  Check(not IP4Match('191.168.1.250', '192.168.1.0/24'), 'match5');
  Check(not IP4Match('192.168.1',     '192.168.1.0/24'), 'match6');
  Check(not IP4Match('192.168.1.135', '192.168.1/24'),   'match7');
  Check(not IP4Match('192.168.1.135', '192.168.1.0/65'), 'match8');
  Check(not IP4Match('193.168.1.1',   '192.168.1.0/24'), 'match9');
  Check(IP4Match('192.168.1.250', '192.168.1.250'),     'match10');
  Check(not IP4Match('192.168.1.251', '192.168.1.250'), 'match11');
  sub := TIp4SubNets.Create;
  try
    CheckEqual(sub.AfterAdd, 0);
    bin := sub.SaveToBinary;
    if CheckEqual(length(bin), 8) then
      CheckEqual(PInt64(bin)^, IP4SUBNET_MAGIC);
    Check(not sub.Match('190.16.1.1'));
    Check(not sub.Match('190.16.1.135'));
    Check(not sub.Match('190.16.1.250'));
    Check(not sub.Match('190.16.2.135'));
    Check(sub.Add('190.16.1.0/24'));
    Check(sub.Match('190.16.1.1'));
    Check(sub.Match('190.16.1.135'));
    Check(sub.Match('190.16.1.250'));
    Check(not sub.Match('193.168.1.1'));
    Check(not sub.Match('190.16.2.135'));
    Check(not sub.Match('191.168.1.250'));
    CheckEqual(sub.AfterAdd, 1);
    bin := sub.SaveToBinary;
    sub.Clear;
    CheckEqual(sub.AfterAdd, 0);
    Check(not sub.Match('190.16.1.1'));
    Check(not sub.Match('190.16.1.135'));
    Check(not sub.Match('190.16.1.250'));
    CheckEqual(sub.LoadFromBinary(bin), 1, 'load1');
    CheckEqual(sub.AfterAdd, 1);
    Check(sub.Match('190.16.1.1'));
    Check(sub.Match('190.16.1.135'));
    Check(sub.Match('190.16.1.250'));
    Check(not sub.Match('193.168.1.1'));
    sub.Clear;
    Check(sub.Add('190.16.40.0/21'));
    Check(sub.Match('190.16.43.1'));
    Check(sub.Match('190.16.44.1'));
    Check(sub.Match('190.16.45.1'));
    Check(not sub.Match('190.16.55.1'));
    Check(sub.Add('190.16.1.0/24'));
    Check(sub.Match('190.16.1.1'));
    Check(sub.Match('190.16.1.135'));
    Check(sub.Match('190.16.1.250'));
    Check(sub.Match('190.16.43.1'));
    Check(sub.Match('190.16.44.1'));
    Check(sub.Match('190.16.45.1'));
    Check(not sub.Match('190.16.55.1'));
    CheckEqual(sub.AfterAdd, 2);
    bin := sub.SaveToBinary;
    sub.Clear;
    Check(not sub.Match('190.16.43.1'));
    CheckEqual(sub.LoadFromBinary(bin), 2, 'load');
    CheckEqual(sub.AfterAdd, 2);
    Check(sub.Match('190.16.1.1'));
    Check(sub.Match('190.16.1.135'));
    Check(sub.Match('190.16.1.250'));
    Check(sub.Match('190.16.43.1'));
    Check(sub.Match('190.16.44.1'));
    Check(sub.Match('190.16.45.1'));
    Check(sub.SaveToBinary = bin, 'save');
    sub.Clear;
    CheckEqual(sub.AddFromText('# test'#10'190.16.40.0/21 # comment'#10 +
      '190.16.1.0/24'), 2);
    Check(sub.SaveToBinary = bin, 'loadtext');
    CheckEqual(sub.AfterAdd, 2);
    sub.Clear;
    Check(sub.Add('1.2.3.4'));
    Check(sub.Match('1.2.3.4'));
    Check(not sub.Match('1.2.3.5'));
    bin := sub.SaveToBinary;
    sub.Clear;
    CheckEqual(sub.AddFromText('1.2.3.4 ; comment'#10), 1);
    CheckEqual(sub.AfterAdd, 1);
    Check(sub.SaveToBinary = bin, 'loadtext');
    Check(sub.Match('1.2.3.4'));
    Check(not sub.Match('1.2.3.5'));
    //deleteFile('firehol.netset');
    txt := DownloadFile('https://raw.githubusercontent.com/firehol/blocklist-ipsets/' +
      'refs/heads/master/firehol_level1.netset', 'firehol.netset');
    if txt <> '' then
    begin
      Make(['file://', WorkDir, 'firehol.netset'], uri);
      CheckUtf8(DownloadFile(uri) = txt, uri);
      sub.Clear;
      timer.Start;
      n := sub.AddFromText(txt);
      NotifyTestSpeed('parse TIp4SubNets', n, length(txt), @timer);
      // typically 4491 subnets, 612,537,665 unique IPs, 19 unique subnets
      Check(n > 4000);
      CheckEqual(sub.AfterAdd, n);
      CheckUtf8(length(sub.SubNet) in [17 .. 20], 'sub=%', [length(sub.SubNet)]);
      Check(not sub.Match('1.2.3.4'));
      Check(not sub.Match('1.2.3.5'));
      Check(not sub.Match('192.168.1.1'), '192'); // only IsPublicIP() was added
      Check(not sub.Match('10.18.1.1'), '10');
      Check(not sub.Match(SYNOPSE_IP), 'synopse.info');
      // 223.254.0.0/16 as https://check.spamhaus.org/results/?query=SBL212803
      Check(sub.Match('223.254.0.1') ,'a0');
      Check(sub.Match('223.254.1.1'), 'b0');
      Check(sub.Match('223.254.200.129'), 'c0');
      CheckEqual(sub.AddFromText(txt), 0, 'twice');
      // e.g. 18 masks, 4471 subnets, 612.950.208 unique IPs: around 16M/s
      timer.Start;
      for i := 1 to 20000 do
        Check(not sub.Match($01010101), '1.1.1.1');
      NotifyTestSpeed('blacklist TIp4SubNets', 20000, 0, @timer);
      bin := sub.SaveToBinary;
      Check(length(bin) < length(txt), 'bin<txt'); // 18020 < 71138
      FileFromString(bin, WorkDir + 'firehol-bin.netset');
      // TSynAlgo.Compress: bin=18020 AlgoSynLZ=16598 AlgoDeflate=11923
      Check(IP4SubNetMatch(bin, '223.254.0.1') ,'a1');
      Check(IP4SubNetMatch(bin, '223.254.1.1'), 'b1');
      Check(IP4SubNetMatch(bin, '223.254.200.129'), 'c1');
      // IP4SubNetMatch() is actually not faster than sub.Match()
      timer.Start;
      for i := 1 to 20000 do
        Check(not IP4SubNetMatch(pointer(bin), $01010101), 'IP4SubNetMatch');
      NotifyTestSpeed('blacklist direct', 20000, 0, @timer);
      txt := DownloadFile('https://www.spamhaus.org/drop/drop.txt',
        'spamhaus.netset');
      if txt <> '' then
      begin
        Check(sub.AddFromText(txt) < 1000, 'spamhaus within firehol');
        sub.Clear;
        Check(not sub.Match('10.18.1.1'), '10');
        n2 := sub.AddFromText(txt);
        Check(n2 > 1000, 'spamhaus=1525');
        Check(not sub.Match(SYNOPSE_IP), 'cauterets.site');
        Check(sub.Match('223.254.0.1') ,'a2'); // 223.254.0.0/16
        Check(sub.Match('223.254.1.1'), 'b2');
        Check(sub.Match('223.254.200.129'), 'c2');
        bin2 := sub.SaveToBinary;
        CheckEqual(sub.AddFromText(txt), 0, 'twice');
        CheckEqual(sub.SaveToBinary, bin2);
        sub.Clear;
        CheckEqual(sub.LoadFrom(txt), n2, 'loadtxt');
        Check(sub.SaveToBinary = bin2, 'savebin');
      end;
      CheckEqual(sub.LoadFrom(bin), n, 'loadfrom');
      Check(sub.SaveToBinary = bin, 'savebin');
      Check(sub.Match('223.254.0.1') ,'a3'); // 223.254.0.0/16
      Check(sub.Match('223.254.1.1'), 'b3');
      Check(sub.Match('223.254.200.129'), 'c3');
    end;
  finally
    sub.Free;
  end;
end;

const
  HTTP_TIMEOUT = 30000;

type
  THttpPeerCacheHook = class(THttpPeerCache); // to test protected methods
  THttpPeerCryptHook = class(THttpPeerCrypt);

function TNetworkProtocols.OnPeerCacheDirect(var aUri: TUri;
  var aHeader: RawUtf8; var aOptions: THttpRequestExtendedOptions): integer;
begin
  // ext parameters only for the first resource
  CheckUtf8((aUri.Address = '0') = aOptions.TLS.IgnoreCertificateErrors, aUri.Address);
  if aOptions.TLS.IgnoreCertificateErrors then
    CheckEqual(aOptions.TLS.PrivatePassword, 'password')
  else
    CheckEqual(aOptions.TLS.PrivatePassword, '');
  // it is time to setup our custom parameters, needed e.g. with https
  aOptions.TLS.IgnoreCertificateErrors := true;
  // continue
  result := HTTP_SUCCESS;
end;

function FakeGif(const Url: RawUtf8): RawByteString;
begin // not a true GIF, but enough for GetMimeContentType()
  result := Join(['GIF89a', Url]);
end;

function TNetworkProtocols.OnPeerCacheRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  // a local web server is safer and less error-prone than an Internet resource
  result := HTTP_SUCCESS;
  Ctxt.OutContent := FakeGif(Ctxt.Url);
  Ctxt.OutContentType := 'image/gif';
  Check(Assigned(peercachedirect));
  CheckEqual(peercachedirect.HttpServer.CurrentProcess, 1, 'hpcState');
  Check(gasProcessing in peercachedirect.State, 'hpcStateRequest');
end;

procedure TNetworkProtocols.RunPeerCacheDirect(Sender: TObject);
var
  hpc: THttpPeerCacheHook absolute Sender;
  msg2: THttpPeerCacheMessage;
  dUri, dBearer, dTok, dAddr, ctyp, params, url, gif, hash: RawUtf8;
  cache: TFileName;
  i: PtrInt;
  status, len: integer;
  res: THttpPeerCryptMessageDecode;
  hcs: THttpClientSocket;
  decoded: TUri;
  tls: TNetTlsContext;
  popt: PHttpRequestExtendedOptions;
  localserver: THttpServer;

  procedure WaitNotProcessing(const Context: string);
  var
    endtix: Int64; // ms resolution
  begin
    if not (gasProcessing in hpc.State) then
    begin
      Check(true, Context); // most common case
      exit;
    end;
    endtix := GetTickCount64 + 500; // never wait forever
    repeat
      SleepHiRes(10); // let the HTTP thread finalize its course
    until (not (gasProcessing in hpc.State)) or
          (GetTickCount64 > endtix);
    CheckUtf8(not (gasProcessing in hpc.State),
      '%=%', [Context, ToText(hpc.State)]);
  end;

begin
  hcs := nil;
  localserver := THttpServer.Create('8889', nil, nil, 'local', 2);
  try
    peercachedirect := hpc;
    Check(hpc.State = [], 'hpcState1');
    localserver.OnRequest := OnPeerCacheRequest; // return the URL
    hpc.OnDirectOptions := OnPeerCacheDirect;
    try
      // validate all resources
      popt := @peercacheopt;
      for i := 0 to 2 do
      begin
        // test according to local cache status
        url := '/' + SmallUInt32Utf8[i];
        gif := FakeGif(url);
        CheckEqual(GetMimeContentType(gif), 'image/gif');
        hash := Sha256(gif);
        cache := MakeString([hpc.TempFilesPath, '02', hash, '.cache']);
        DeleteFile(cache);
        // compute the direct proxy URI and bearer
        Check(hpc.Settings.HttpDirectUri('secret', 'http://127.0.0.1:8889' + url,
          hash, dUri, dBearer, false, false, popt));
        popt := nil; // ext parameters only for the first
        Check(PosEx(':8008', dUri) <> 0, ':8008');
        Check(dBearer <> '', 'dBearer');
        Check(IdemPChar(pointer(dBearer), HEADER_BEARER_UPPER));
        Check(decoded.From(dUri), 'decoded');
        // decode dBearer
        dTok := '';
        Check(FindNameValue(PAnsiChar(pointer(dBearer)),
                HEADER_BEARER_UPPER, dTok), 'dTok');
        params := '';
        FillCharFast(msg2, SizeOf(msg2), 0);
        res := hpc.BearerDecode(dTok, pcfBearerDirect, msg2, @params);
        if i = 0 then
          Check(params <> '', 'params')
        else
          CheckEqual(params, '');
        Check(res = mdOk, 'directDecode');
        Check(msg2.Kind = pcfBearerDirect, 'directKind');
        // first GET request to download from reference website
        if hcs = nil then
        begin
          InitNetTlsContext(tls);
          tls.IgnoreCertificateErrors := true;
          hcs := THttpClientSocket.OpenUri(dUri, dAddr, '', 10000, @tls);
          hcs.OnLog := TSynLog.DoLog;
          CheckEqual(dAddr, decoded.Address);
        end;
        status := hcs.Get(decoded.Address, HTTP_TIMEOUT, dBearer);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(Sha256(hcs.Content), hash);
        CheckEqual(HashFileSha256(cache), hash);
        ctyp := hcs.ContentType;
        CheckEqual(ctyp, 'image/gif');
        len := hcs.ContentLength;
        CheckUtf8(PosEx('Repr-Digest: sha-256=:', hcs.Headers) <> 0, hcs.Headers);
        WaitNotProcessing('hpcState2');
        // GET twice to retrieve from cache
        status := hcs.Get(decoded.Address, HTTP_TIMEOUT, dBearer);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(hcs.ContentLength, len);
        CheckEqual(hcs.ContentType, ctyp);
        CheckEqual(Sha256(hcs.Content), hash);
        WaitNotProcessing('hpcState3');
        // HEAD should work with cache
        status := hcs.Head(decoded.Address, HTTP_TIMEOUT, dBearer);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(hcs.ContentLength, len);
        CheckEqual(hcs.ContentType, ctyp);
        CheckUtf8(PosEx('Repr-Digest: sha-256=:', hcs.Headers) <> 0, hcs.Headers);
        WaitNotProcessing('hpcState4');
        // prepare local requests on cache in pcfBearer mode (like a peer)
        hpc.MessageInit(pcfBearer, 0, msg2);
        CheckEqual(msg2.IP4, hpc.IP4);
        Check(msg2.Hash.Algo = low(THashAlgo), 'hfMD5');
        hpc.MessageEncodeBearer(msg2, dTok);
        Check(dTok <> dBearer);
        // GET on cache in pcfBearer mode
        status := hcs.Get('dummy', HTTP_TIMEOUT, dTok);
        CheckEqual(status, HTTP_NOTFOUND, 'no hash');
        msg2.Hash.Algo := hfSHA256;
        Check(Sha256StringToDigest(hash, msg2.Hash.Bin.Lo), 'sha');
        hpc.MessageEncodeBearer(msg2, dTok);
        status := hcs.Get('dummy', HTTP_TIMEOUT, dTok);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(hcs.ContentLength, len);
        CheckEqual(hcs.ContentType, ctyp, 'ctyp from cached content');
        CheckEqual(Sha256(hcs.Content), hash);
        WaitNotProcessing('hpcState5');
        // HEAD on cache in pcfBearer mode
        status := hcs.Head('dummies', HTTP_TIMEOUT, dtok);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(hcs.ContentLength, len);
        CheckEqual(hcs.ContentType, '');
        WaitNotProcessing('hpcState6');
        // HEAD should work without cache and call directly the http server
        Check(DeleteFile(cache));
        status := hcs.Head(decoded.Address, HTTP_TIMEOUT, dBearer);
        CheckEqual(status, HTTP_SUCCESS);
        CheckEqual(hcs.ContentLength, len);
        CheckEqual(hcs.ContentType, ctyp);
        WaitNotProcessing('hpcState7');
      end;
    finally
      hcs.Free;
    end;
  finally
    WaitNotProcessing('hpcStateFinal');
    peercachedirect := nil;
    hpc.Settings.Free;
    hpc.Free;
    localserver.Free;
  end;
end;

procedure TNetworkProtocols._THttpPeerCache;
var
  hpc: THttpPeerCacheHook;
  hpc2: THttpPeerCryptHook; // another instance to validate remote decoding
  hps: THttpPeerCacheSettings;
  msg, msg2: THttpPeerCacheMessage;
  m, m2: RawUtf8;
  res: THttpPeerCryptMessageDecode;
  i, n, alter: integer;
  tmp: THttpPeerCacheMessageEncoded;
  dUri, dBearer, dTok, params: RawUtf8;
  decoded: TUri;
  timer: TPrecisionTimer;
begin
  CheckEqual(SizeOf(msg), 192);
  CheckEqual(PEER_CACHE_MESSAGELEN, SizeOf(msg) + 4 + SizeOf(TAesBlock) * 3);
  CheckEqual(Base64uriToBinLength(PEER_CACHE_BEARERLEN), PEER_CACHE_MESSAGELEN);
  // validate THttpRequestExtendedOptions serialization
  peercacheopt.Init;
  Check(not peercacheopt.TLS.IgnoreCertificateErrors, 'tice1');
  Check(VarIsEmptyOrNull(peercacheopt.ToDocVariant), 'tdv1');
  CheckEqual(peercacheopt.ToUrlEncode('/root'), '/root');
  peercacheopt.TLS.IgnoreCertificateErrors := true;
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true}');
  CheckEqual(peercacheopt.ToUrlEncode('/root'), '/root?ti=1');
  peercacheopt.Auth.Scheme := wraNegotiate;
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true,"as":3}');
  CheckEqual(peercacheopt.ToUrlEncode('/root'), '/root?ti=1&as=3');
  peercacheopt.Init;
  Check(not peercacheopt.TLS.IgnoreCertificateErrors, '2');
  Check(VarIsEmptyOrNull(peercacheopt.ToDocVariant), 'tdv2');
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), 'null');
  Check(peercacheopt.InitFromUrl('ti=1&as=3'));
  Check(peercacheopt.TLS.IgnoreCertificateErrors, 'tice3');
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true,"as":3}');
  Check(peercacheopt.InitFromUrl('ti=1'));
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true}');
  Check(peercacheopt.TLS.IgnoreCertificateErrors);
  peercacheopt.TLS.PrivatePassword := 'password';
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true}');
  Check(peercacheopt.InitFromUrl('pp=RGUUac6iP4Y&ti=1'));
  Check(peercacheopt.TLS.IgnoreCertificateErrors);
  CheckEqual(peercacheopt.TLS.PrivatePassword, '', 'missing pf');
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant), '{"ti":true}');
  peercacheopt.TLS.PrivateKeyFile := '/p/f';
  peercacheopt.TLS.PrivatePassword := 'password'; // also for OnPeerCacheDirect() below
  CheckEqual(VariantSaveJson(peercacheopt.ToDocVariant),
    '{"ti":true,"pf":"/p/f","pp":"Y7rINao7mcc"}');
  params := peercacheopt.ToUrlEncode('/root');
  CheckEqual(params, '/root?ti=1&pf=%2Fp%2Ff&pp=Y7rINao7mcc');
  peercacheopt.Init;
  CheckEqual(peercacheopt.TLS.PrivatePassword, '');
  Check(peercacheopt.InitFromUrl(params));
  CheckEqual(peercacheopt.TLS.PrivatePassword, 'password');
  peercacheopt.TLS.IgnoreCertificateErrors := true;
  params := '';
  // for further tests, use the dedicated "mORMot GET" (mget) sample
  hps := THttpPeerCacheSettings.Create;
  try
    hps.CacheTempPath := Executable.ProgramFilePath + 'peercachetemp';
    hps.CachePermPath := Executable.ProgramFilePath + 'peercacheperm';
    hps.CacheTempMinBytes := 100;
    hps.BroadCastDirectMinBytes := 10000; // broadcast for HTTP_LINK[1] only
    hps.Port := 8008; // don't use default 8099
    hps.Options := [pcoHttpDirect, pcoCacheTempNoCheckSize,
      pcoVerboseLog, pcoHttpReprDigest {}, pcoSelfSignedHttps{}];
    try
      hpc := THttpPeerCacheHook.Create(hps, 'secret'{,THttpAsyncServer});
      try
        hpc2 := THttpPeerCryptHook.Create('secret', nil, nil);
        try
          hpc2.fSettings := hps;
          hpc2.AfterSettings;
          FillCharFast(msg, SizeOf(msg2), 0);
          CheckEqual(msg.IP4, 0);
          hpc.MessageInit(pcfBearer, 0, msg);
          CheckEqual(msg.IP4, hpc.IP4);
          msg.Hash.Algo := hfSHA256;
          RandomBytes(@msg.Hash.Bin, HASH_SIZE[msg.Hash.Algo]);
          // validate UDP messages encoding/decoding
          timer.Start;
          n := 1000;
          for i := 1 to n do
          begin
            msg.Size := i;
            msg.Hash.Bin.i0 := i;
            hpc.MessageEncode(msg, tmp);
            res := hpc2.MessageDecode(@tmp, SizeOf(tmp), msg2);
            Check(res = mdOk, 'hpc2');
            CheckEqual(msg2.Size, i);
            Check(CompareMem(@msg, @msg2, SizeOf(msg)), 'hpc2mem');
          end;
          NotifyTestSpeed('messages', n * 2, n * 2 * SizeOf(msg), @timer);
          m := RawUtf8(ToText(msg));
          m2 := RawUtf8(ToText(msg2));
          CheckEqual(m, m2);
          // validate UDP messages alteration (quick CRC identification)
          timer.Start;
          n := 10000;
          for i := 1 to n do
          begin
            alter := Random32(SizeOf(tmp));
            inc(PByteArray(@tmp)[alter]); // should be detected at crc level
            res := hpc2.MessageDecode(@tmp, SizeOf(tmp), msg2);
            if CheckFailed(res = mdCrc, 'alt') then
              TestFailed('alt=%', [ToText(res)^]);
            dec(PByteArray(@tmp)[alter]); // restore
          end;
          NotifyTestSpeed('altered', n, n * SizeOf(msg), @timer);
          res := hpc.MessageDecode(@tmp, SizeOf(tmp), msg2);
          Check(res = mdOk, 'hpc');
          Check(CompareMem(@msg, @msg2, SizeOf(msg)));
          // validate the UDP client/server stack is running
          if hpc.Ping <> nil then // multiple VMs may ping - twice may fail
            Check(not fOwner.MultiThread, 'ping<>nil') // LUTI = not multithread
          else
            Check(true, 'ping=nil');
          // validate THttpPeerCrypt.HttpDirectUri request encoding/decoding
          Check(THttpPeerCrypt.HttpDirectUri('secret',
            'https://synopse.info/forum', ToText(msg.Hash), dUri, dBearer), 'direct');
          CheckEqual(dUri, '/https/synopse.info/forum');
          Check(THttpPeerCrypt.HttpDirectUriReconstruct(pointer(dUri), decoded), 'reconst');
          CheckEqual(decoded.URI, 'https://synopse.info/forum');
          Check(dBearer <> '', 'dBearer');
          FillCharFast(msg2, SizeOf(msg2), 0);
          Check(msg2.Hash.Algo <> hfSHA256);
          Check(not CompareMem(@msg.Hash.Bin, @msg2.Hash.Bin, HASH_SIZE[hfSHA256]));
          Check(not HashDigestEqual(msg.Hash, msg2.Hash), 'hde0');
          res := hpc2.BearerDecode(dBearer, pcfBearerDirect, msg2);
          Check(res = mdBParam, 'directB64');
          dTok := '';
          Check(FindNameValue(PAnsiChar(pointer(dBearer)), HEADER_BEARER_UPPER, dTok));
          FillCharFast(msg2, SizeOf(msg2), 0);
          res := hpc2.BearerDecode(dTok, pcfBearer, msg2);
          Check(res = mdBearer, 'directKo');
          FillCharFast(msg2, SizeOf(msg2), 0);
          res := hpc2.BearerDecode(dTok, pcfBearerDirect, msg2);
          Check(res = mdOk, 'directOk');
          Check(not CompareMem(@msg, @msg2, SizeOf(msg)), 'cm');
          Check(CompareMem(@msg.Hash.Bin, @msg2.Hash.Bin, HASH_SIZE[hfSHA256]));
          Check(HashDigestEqual(msg.Hash, msg2.Hash), 'hde1');
          Check(msg2.Kind = pcfBearerDirect);
          CheckEqual(msg2.Opaque, 7142701337754149600, 'Opaque');
          Check(msg2.Hash.Algo = hfSHA256);
          Check(CompareMem(@msg.Hash.Bin, @msg2.Hash.Bin, HASH_SIZE[hfSHA256]));
          Check(HashDigestEqual(msg.Hash, msg2.Hash), 'hde2');
          FillCharFast(msg2, SizeOf(msg2), 0);
          inc(dTok[10]);
          res := hpc2.BearerDecode(dTok, pcfBearer, msg2);
          Check(res in [mdCrc, mdB64], 'altered');
          Check(THttpPeerCrypt.HttpDirectUri('secret',
            'https://synopse.info:123/forum', ToText(msg.Hash), dUri, dBearer,
            {permanent=}true, @peercacheopt));
          CheckEqual(dUri, '/https/synopse.info_123/forum');
          Check(THttpPeerCrypt.HttpDirectUriReconstruct(pointer(dUri), decoded), 'reconst');
          CheckEqual(decoded.URI, 'https://synopse.info:123/forum');
          dTok := '';
          Check(FindNameValue(PAnsiChar(pointer(dBearer)), HEADER_BEARER_UPPER, dTok));
          FillCharFast(msg2, SizeOf(msg2), 0);
          Check(msg2.Kind = pcfPing);
          CheckEqual(params, '');
          res := hpc2.BearerDecode(dTok, pcfBearerDirectPermanent, msg2, @params);
          CheckEqual(params, 'ti=1&pf=%2Fp%2Ff&pp=NCpB3InJzms');
          Check(res = mdOk, 'directOkParams');
          Check(msg2.Kind = pcfBearerDirectPermanent);
        finally
          hpc2.Free;
        end;
        // validate pcoHttpDirect proxy mode with some constant web resources
        // in a background thread due to remote http://ictuswin.com access
        // (will also validate rfProgressiveStatic process of our web server)
        if hasinternet then // checked by above DNSAndLDAP method
          Run(RunPeerCacheDirect, hpc, 'peercachedirect', true, false, false);
        hpc := nil; // will be owned and freed by RunPeerCacheDirect from now on
        hps := nil;
      finally
        hpc.Free;
      end;
    except
      // exception here is likely to be port 8008 already used -> continue
    end;
  finally
    hps.Free;
  end;
end;

const
  HDR1: PUtf8Char = 'one: value'#13#10'cookie: name=value';
  HDR2: PUtf8Char = 'one: value'#13#10'cookie: name = value ';
  HDR3: PUtf8Char = 'cookie: name=value'#13#10 +
    'Cookie: name 1=value1; name 2 = value 2; name3=value3'#13#10 +
    'cookone: value'#13#10;
  HDR4: PUtf8Char = 'cookie: name=value'#10'toto: titi'#10#10 +
    'Cookie: name 1=value1; name 2 = value 2; name3=value3'#13#10 +
    'cookone: value'#13#10#13#10;

procedure TNetworkProtocols.HTTP;
var
  met: TUriMethod;
  s: RawUtf8;
  hc: THttpCookies;
  U: TUri;
  h, v: PUtf8Char;
  l: PtrInt;
  dig: THashDigest;

  procedure Check4;
  begin
    CheckEqual(hc.Name(0), 'name');
    CheckEqual(hc.Value(0), 'value');
    CheckEqual(hc.Name(1), 'name 1');
    CheckEqual(hc.Value(1), 'value1');
    CheckEqual(hc.Name(2), 'name 2');
    CheckEqual(hc.Value(2), 'value 2');
    CheckEqual(hc.Name(3), 'name3');
    CheckEqual(hc.Value(3), 'value3');
    {$ifdef HASINLINE}
    CheckEqual(hc.Cookie['name'], 'value');
    CheckEqual(hc.Cookie['name 1'], 'value1');
    CheckEqual(hc.Cookie['name 2'], 'value 2');
    CheckEqual(hc.Cookie['name3'], 'value3');
    {$else}
    CheckEqual(hc.GetCookie('name'), 'value');
    CheckEqual(hc.GetCookie('name 1'), 'value1');
    CheckEqual(hc.GetCookie('name 2'), 'value 2');
    CheckEqual(hc.GetCookie('name3'), 'value3');
    {$endif HASINLINE}
  end;

begin
  // validate method names and HTTP status codes or schemes
  Check(ToMethod('') = mNone);
  Check(ToMethod('toto') = mNone);
  Check(ToMethod('get') = mGET);
  Check(ToMethod('Patch') = mPATCH);
  Check(ToMethod('OPTIONS') = mOPTIONS);
  Check(not IsGet('get'));
  Check(IsGet('GET'));
  Check(not IsPost('Post'));
  Check(IsPost('POST'));
  for met := low(met) to high(met) do
  begin
    s := RawUtf8(ToText(met));
    Check(ToMethod(s) = met);
    LowerCaseSelf(s);
    Check(ToMethod(s) = met);
  end;
  Check(IsOptions('OPTIONS'));
  Check(not IsOptions('opTIONS'));
  Check(IsUrlFavIcon('/favicon.ico'));
  Check(not IsUrlFavIcon('/favicon.ice'));
  Check(not IsHttp('http:'));
  Check(IsHttp('https:'));
  Check(IsHttp('http://toto'));
  Check(IsHttp('https://titi'));
  Check(not IsHttp('c:\'));
  Check(not IsHttp('c:\toto'));
  Check(not IsHttp('file://toto'));
  CheckEqual(StatusCodeToText(100)^, 'Continue');
  CheckEqual(StatusCodeToText(200)^, 'OK');
  CheckEqual(StatusCodeToText(206)^, 'Partial Content');
  CheckEqual(StatusCodeToText(300)^, 'Multiple Choices');
  CheckEqual(StatusCodeToText(503)^, 'Service Unavailable');
  CheckEqual(StatusCodeToText(513)^, 'Invalid Request');
  CheckEqual(StatusCodeToText(514)^, 'Invalid Request');
  CheckEqual(StatusCodeToText(499)^, 'Invalid Request');
  CheckEqual(StatusCodeToText(666)^, 'Client Side Connection Error');
  // validate TUri data structure
  U.Clear;
  Check(U.UriScheme = usUndefined);
  CheckEqual(U.Uri, '');
  Check(U.From('toto.com'));
  CheckEqual(U.Uri, 'http://toto.com/');
  Check(U.UriScheme = usHttp);
  Check(not U.Https);
  Check(U.From('toto.com:123'));
  CheckEqual(U.Uri, 'http://toto.com:123/');
  Check(U.UriScheme = usHttp);
  Check(not U.Https);
  Check(U.From('https://toto.com:123/tata/titi'));
  CheckEqual(U.Uri, 'https://toto.com:123/tata/titi');
  Check(U.UriScheme = usHttps);
  Check(U.Https);
  CheckEqual(U.Address, 'tata/titi');
  Check(U.From('https://toto.com:123/tata/tutu:tete'));
  Check(U.UriScheme = usHttps);
  CheckEqual(U.Address, 'tata/tutu:tete');
  CheckEqual(U.Uri, 'https://toto.com:123/tata/tutu:tete');
  Check(U.From('http://user:password@server:port/address'));
  Check(not U.Https);
  CheckEqual(U.Uri, 'http://server:port/address');
  CheckEqual(U.User, 'user');
  CheckEqual(U.Password, 'password');
  CheckEqual(U.Address, 'address');
  Check(U.From('https://user@server:port/address'));
  Check(U.Https);
  CheckEqual(U.Uri, 'https://server:port/address');
  CheckEqual(U.User, 'user');
  CheckEqual(U.Password, '');
  Check(U.From('toto.com/tata/tutu:tete'));
  Check(U.UriScheme = usHttp);
  CheckEqual(U.Uri, 'http://toto.com/tata/tutu:tete');
  CheckEqual(U.User, '');
  CheckEqual(U.Password, '');
  CheckEqual(U.URI, 'http://toto.com/tata/tutu:tete');
  Check(U.From('file://server/path/to%20image.jpg'));
  CheckEqual(U.Scheme, 'file');
  Check(U.UriScheme = usFile);
  CheckEqual(U.Server, 'server');
  CheckEqual(U.Address, 'path/to%20image.jpg');
  CheckEqual(U.Uri, 'file://server/path/to%20image.jpg');
  Check(U.From('file://server/c:\path\to'));
  CheckEqual(U.Scheme, 'file');
  CheckEqual(U.Server, 'server');
  CheckEqual(U.Address, 'c:\path\to');
  Check(U.From('file:////server/folder/data.xml'));
  CheckEqual(U.Scheme, 'file');
  CheckEqual(U.Server, 'server');
  CheckEqual(U.Address, 'folder/data.xml');
  Check(not U.From('file:///C:/DirA/DirB/With%20Spaces.txt'));
  CheckEqual(U.Scheme, 'file');
  CheckEqual(U.Server, '');
  CheckEqual(U.Address, 'C:/DirA/DirB/With%20Spaces.txt');
  Check(not U.From('FILE:///path/to%20image.jpg'), 'false if valid');
  CheckEqual(U.Scheme, 'FILE');
  Check(U.UriScheme = usFile);
  CheckEqual(U.Server, '');
  CheckEqual(U.User, '');
  CheckEqual(U.Password, '');
  CheckEqual(U.Address, 'path/to%20image.jpg');
  CheckEqual(U.Uri, 'file:///path/to%20image.jpg');
  Check(U.From('ftp://user:password@host:port/path'), 'ftp');
  CheckEqual(U.Scheme, 'ftp');
  Check(U.UriScheme = usFtp);
  CheckEqual(U.Server, 'host');
  CheckEqual(U.Port, 'port');
  CheckEqual(U.User, 'user');
  CheckEqual(U.Password, 'password');
  CheckEqual(U.Address, 'path');
  CheckEqual(U.Uri, 'ftp://host:port/path');
  Check(U.From('ftpS://user:password@host/path'), 'ftps');
  CheckEqual(U.Scheme, 'ftpS');
  Check(U.UriScheme = usFtps);
  CheckEqual(U.Server, 'host');
  CheckEqual(U.Port, '989');
  CheckEqual(U.User, 'user');
  CheckEqual(U.Password, 'password');
  CheckEqual(U.Address, 'path');
  CheckEqual(U.Uri, 'ftps://host/path');
  s := '?subject=This%20is%20the%20subject' +
       '&cc=someone_else@example.com&body=This%20is%20the%20body';
  Check(U.From('mailto://someone@example.com' + s));
  CheckEqual(U.Scheme, 'mailto');
  Check(U.UriScheme = usCustom);
  CheckEqual(U.Server, 'example.com');
  CheckEqual(U.Port, '');
  CheckEqual(U.User, 'someone');
  CheckEqual(U.Password, '');
  CheckEqual(U.Address, s);
  CheckEqual(U.Uri, 'mailto://example.com/' + s);
  U.Clear; // TUri may be used to create an URI from some parameters
  U.Server := '127.0.0.1';
  U.Port := '991';
  U.Address := 'endpoint';
  CheckEqual(U.Uri, 'http://127.0.0.1:991/endpoint');
  U.Clear;
  U.Https := true;
  U.Server := '127.0.0.1';
  U.Address := 'endpoint';
  CheckEqual(U.Port, '');
  CheckEqual(U.Uri, 'https://127.0.0.1/endpoint');
  CheckEqual(U.Port, '');
  U.Port := '443';
  CheckEqual(U.Uri, 'https://127.0.0.1/endpoint');
  U.Port := '444';
  CheckEqual(U.Uri, 'https://127.0.0.1:444/endpoint');
  // validate THttpCookies and CookieFromHeaders()
  hc.ParseServer('');
  CheckEqual(length(hc.Cookies), 0);
  hc.ParseServer(HDR1);
  CheckEqual(hc.Name(0), 'name');
  CheckEqual(hc.Value(0), 'value');
  CheckEqual(length(hc.Cookies), 1);
  CheckEqual(hc.GetCookie('name'), 'value');
  CheckEqual(hc.Cookies[0].NameLen, 4);
  CheckEqual(hc.Cookies[0].ValueLen, 5);
  Check(hc.GetCookie('name2') <> 'value');
  hc.Clear;
  CheckEqual(length(hc.Cookies), 0);
  hc.ParseServer(HDR2);
  CheckEqual(hc.Name(0), 'name');
  CheckEqual(hc.Value(0), 'value');
  CheckEqual(length(hc.Cookies), 1);
  CheckEqual(hc.GetCookie('name'), 'value');
  CheckEqual(hc.Cookies[0].NameLen, 4);
  CheckEqual(hc.Cookies[0].ValueLen, 5);
  Check(hc.GetCookie('name2') <> 'value');
  hc.ParseServer(HDR3);
  CheckEqual(length(hc.Cookies), 4);
  Check4;
  hc.ParseServer(HDR4);
  CheckEqual(length(hc.Cookies), 4, 'malformatted CRLF');
  Check4;
  v := nil;
  CheckEqual(CookieFromHeaders(HDR1, 'name', v), 5);
  Check((v <> nil) and (v^ = 'v'));
  CheckEqual(CookieFromHeaders(HDR1, 'name'), 'value');
  CheckEqual(CookieFromHeaders(HDR1, 'name2', v), 0);
  CheckEqual(CookieFromHeaders(HDR1, 'name3'), '');
  v := nil;
  CheckEqual(CookieFromHeaders(HDR2, 'name', v), 5);
  Check((v <> nil) and (v^ = 'v'));
  CheckEqual(CookieFromHeaders(HDR2, 'name'), 'value');
  CheckEqual(CookieFromHeaders(HDR2, 'name3'), '');
  CheckEqual(CookieFromHeaders(HDR3, 'name'), 'value');
  CheckEqual(CookieFromHeaders(HDR3, 'name 1'), 'value1');
  CheckEqual(CookieFromHeaders(HDR3, 'name 2'), 'value 2');
  CheckEqual(CookieFromHeaders(HDR3, 'name3'), 'value3');
  CheckEqual(CookieFromHeaders(HDR4, 'name'), 'value');
  CheckEqual(CookieFromHeaders(HDR4, 'name 1'), 'value1');
  CheckEqual(CookieFromHeaders(HDR4, 'name 2'), 'value 2');
  CheckEqual(CookieFromHeaders(HDR4, 'name3'), 'value3');
  // validate HttpRequestLength() and HttpRequestHash()
  h := HttpRequestLength(
    'Content-Length: 100'#13#10'content-range: bytes 100-199/3083'#13#10, l);
  check(h <> nil);
  checkEqual(l, 4);
  Check(IdemPropName('3083', h, 4));
  h := HttpRequestLength('Content-Length: 100'#13#10, l);
  check(h <> nil);
  checkEqual(l, 3);
  Check(IdemPropName('100', h, 3));
  h := HttpRequestLength('Content-Range: 100-199/2000'#13#10, l);
  check(h <> nil);
  checkEqual(l, 4);
  Check(IdemPropName('2000', h, 4));
  h := HttpRequestLength('Content-Range: 100-199'#13#10, l);
  check(h = nil);
  check(U.From('https://ictuswin.com/toto/titi'));
  h := HttpRequestLength('Content-Lengths: 100'#13#10, l);
  check(h = nil);
  FillCharFast(dig, SizeOf(dig), 0);
  CheckEqual(ord(dig.Algo), 0);
  l := HttpRequestHash(hfSHA256, U, 'etag: "1234"'#13#10, dig);
  CheckEqual(l, SizeOf(THash256));
  Check(dig.Algo = hfSHA256);
  CheckEqual(Sha256DigestToString(dig.Bin.Lo),
    'cc991f15d823e419ef45f8b94e6759c4f992056c1c1a64cc79338c49f9720273');
  FillCharFast(dig, SizeOf(dig), 0);
  l := HttpRequestHash(hfSHA256, U,
    'Content-Length: 100'#13#10'Last-Modified: 2025', dig);
  CheckEqual(l, SizeOf(THash256));
  Check(dig.Algo = hfSHA256);
  CheckEqual(Sha256DigestToString(dig.Bin.Lo),
    '9b23e3b9894578f2709eca35aa9afad277ab5aa4afe9344192f59535719ac734');
  CheckEqual(HttpRequestHashBase32(
    U, 'Content-Length: 100'#13#10'Last-Modified: 2025'),
    'tmr6homjiv4pe4e6zi22vgx22j32wwve');
  CheckEqual(HttpRequestHashBase32(
    U, 'Content-Length: 101'#13#10'Last-Modified: 2025'),
    '5umuom5hoh7sohesrs3fqse4rweeum7d');
  CheckEqual(HttpRequestHashBase32(U, nil), 'bq4n2dkrduzo2v3arzy2lafegac3wmbw');
end;

procedure TNetworkProtocols._THttpProxyCache;

  procedure TryOne(const force, ignore: RawUtf8;
    const v: array of RawUtf8; const k: array of THttpProxyCacheKind);
  var
    m: THttpProxyMem;
    n: TUriMatchName;
    i: PtrInt;
    r: THttpProxyCacheKind;
  begin
    m := THttpProxyMem.Create;
    try
      m.ForceCsv := force;
      m.IgnoreCsv := ignore;
      for i := 0 to high(v) do
      begin
        n.Path.Text := pointer(v[i]);
        n.Path.Len := length(v[i]);
        n.ParsePath;
        r := [];
        if i <= high(k) then
          r := k[i];
        Check(m.FromUri(n) = r);
      end;
    finally
      m.Free;
    end;
  end;

begin
  TryOne('', '', ['', 'p', 'p/2', '/2'], []);
  TryOne('*', '', ['', 'p', 'p/2', '/2'], [[], [pckForce], [pckForce], [pckForce]]);
  TryOne('*2', '', ['', 'p', 'p/2', '/2'], [[], [], [pckForce], [pckForce]]);
  TryOne('p*', '', ['', 'p', 'p/2', '/2'], [[], [pckForce]]);
  TryOne('*', '', ['', 'pas', 'pas/12', '/12'], [[], [pckForce], [pckForce], [pckForce]]);
  TryOne('*2', '', ['', 'pas', 'pas/12', '/12'], [[], [], [pckForce], [pckForce]]);
  TryOne('p*', '', ['', 'pas', 'pas/12', '/12'], [[], [pckForce]]);
  TryOne('', '*', ['', 'p', 'p/2', '/2'], [[], [pckIgnore], [pckIgnore], [pckIgnore]]);
  TryOne('', '*2', ['', 'p', 'p/2', '/2'], [[], [], [pckIgnore], [pckIgnore]]);
  TryOne('', 'p*', ['', 'p', 'p/2', '/2'], [[], [pckIgnore]]);
  TryOne('', '*', ['', 'pas', 'pas/12', '/12'], [[], [pckIgnore], [pckIgnore], [pckIgnore]]);
  TryOne('', '*2', ['', 'pas', 'pas/12', '/12'], [[], [], [pckIgnore], [pckIgnore]]);
  TryOne('', 'p*', ['', 'pas', 'pas/12', '/12'], [[], [pckIgnore]]);
  TryOne('*', '*', ['', 'p', 'p/2', '/2'], [[],
    [pckForce, pckIgnore], [pckForce, pckIgnore], [pckForce, pckIgnore]]);
  TryOne('*2', '*2', ['', 'p', 'p/2', '/2'],
    [[], [], [pckForce, pckIgnore], [pckForce, pckIgnore]]);
  TryOne('p*', 'p*', ['', 'p', 'p/2', '/2'],
    [[], [pckForce, pckIgnore]]);
  TryOne('*', '*', ['', 'pas', 'pas/12', '/12'],
    [[], [pckForce, pckIgnore], [pckForce, pckIgnore], [pckForce, pckIgnore]]);
  TryOne('*2', '*2', ['', 'pas', 'pas/12', '/12'],
    [[], [], [pckForce, pckIgnore], [pckForce, pckIgnore]]);
  TryOne('p*', 'p*', ['', 'pas', 'pas/12', '/12'],
    [[], [pckForce, pckIgnore]]);
end;

{$ifdef OSPOSIX}
procedure TNetworkProtocols.TFTPServer;
var
  srv: TTftpServerThread;
  res: TCurlResult;
  tmp: TFileName;
  uri: RawUtf8;
  timer: TPrecisionTimer;
  orig, rd: RawByteString;
begin
  {$ifdef OSDARWINARM}
  if true then // mac M1 libcurl seems not tftp compatible
  {$else}
  if not CurlIsAvailable then
  {$endif OSDARWINARM}
  begin
    AddConsole('libcurl is not available on this system -> skip test');
    exit;
  end;
  // create a temporary file to server
  orig := RandomAnsi7(256 shl 10 + Random32(100)); // 256.1KB of random data
  tmp := TemporaryFileName; // e.g. '/tmp/mormot2tests_28F3D8C5.tmp'
  if not CheckFailed(FileFromString(orig, tmp), 'tmp file') then
  try
    // start the TFTP server
    srv := TTftpServerThread.Create(ExtractFilePath(tmp),
      [ttoRrq , {ttoLowLevelLog,} ttoCaseInsensitiveFileName, ttoAllowSubFolders],
      TSynLogTestLog, '127.0.0.1', '6969', '');
    try
      // request the temporary file using the libcurl client
      timer.Start;
      StringToUtf8(ExtractFileName(tmp), uri); // 'mormot2tests_28F3D8C5.tmp'
      res := CurlPerform('tftp://127.0.0.1:6969/' + uri, rd);
      CheckUtf8(res = crOK, 'tftp exact case %', [ToText(res)^]);
      if res <> crOk then
        exit;
      CheckEqual(length(rd), length(orig), 'tftp1a');
      CheckEqual(rd, orig, 'tftp1b');
      // validate case-insensitive URI as e.g. 'MORMOT2TESTS_28F3D8C5.TMP'
      UpperCaseSelf(uri);
      rd := ''; // paranoid
      res := CurlPerform('tftp://127.0.0.1:6969/' + uri, rd, 1000, nil,
        {tftpblocksize=}1468);
      Check(res = crOK, 'tftp uppercase and custom blocksize');
      if res = crOk then
        CheckEqual(rd, orig, 'tftp2');
      NotifyTestSpeed('TFTP request', 2, length(rd) * 2, @timer);
    finally
      srv.Free;
    end;
  finally
    // remove the temporary file to serve
    Check(DeleteFile(tmp), 'delete tmp');
  end;
end;
{$endif OSPOSIX}


end.

