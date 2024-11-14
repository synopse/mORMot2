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
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.test,
  mormot.core.perf,
  mormot.core.threads,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async,
  mormot.net.openapi,
  mormot.net.ldap,
  mormot.net.dns,
  mormot.net.rtsphttp,
  mormot.net.tunnel;

type
  /// this test case will validate several low-level protocols
  TNetworkProtocols = class(TSynTestCase)
  protected
    // for _TUriTree
    one, two: RawUtf8;
    three: boolean;
    request: integer;
    four: Int64;
    // for _TTunnelLocal
    session: Int64;
    appsec: RawUtf8;
    options: TTunnelOptions;
    tunnelexecutedone: boolean;
    tunnelexecuteremote, tunnelexecutelocal: TNetPort;
    procedure TunnelExecute(Sender: TObject);
    procedure TunnelExecuted(Sender: TObject);
    procedure TunnelTest(const clientcert, servercert: ICryptCert);
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
    /// validate mormot.net.openapi unit
    procedure OpenAPI;
    /// validate TUriTree high-level structure
    procedure _TUriTree;
    /// validate DNS and LDAP clients (and NTP/SNTP)
    procedure DNSAndLDAP;
    /// RTSP over HTTP, as implemented in mormot.net.rtsphttp unit
    procedure RTSPOverHTTP;
    /// RTSP over HTTP, with always temporary buffering
    procedure RTSPOverHTTPBufferedWrite;
    /// validate mormot.net.tunnel
    procedure _TTunnelLocal;
    /// validate IP processing functions
    procedure IPAddresses;
    /// validate THttpPeerCache process
    procedure _THttpPeerCache;
  end;


implementation

type
   TMyEnum = (eNone, e1, e2);
const
  MYENUM2TXT: array[TMyEnum] of RawUtf8 = ('', 'one', 'and 2');

const
  // some reference from https://github.com/OAI/OpenAPI-Specification
  OpenApiRef: array[0..1] of RawUtf8 = (
    'v2.0/json/petstore-simple.json',
    'v3.0/petstore.json');

procedure TNetworkProtocols.OpenAPI;
var
  i: PtrInt;
  fn: TFileName;
  u, ud, uc, url: RawUtf8;
  pets: TRawUtf8DynArray;
  oa: TOpenApiParser;
begin
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and 2'), 2);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'one'), 1);
  CheckEqual(FindCustomEnum(MYENUM2TXT, ''), 0);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and'), 0);
  CheckEqual(FindCustomEnum(MYENUM2TXT, 'and 3'), 0);
  for i := 1 to high(RESERVED_KEYWORDS) do
    CheckUtf8(StrComp(pointer(RESERVED_KEYWORDS[i - 1]),
      pointer(RESERVED_KEYWORDS[i])) < 0, RESERVED_KEYWORDS[i]);
  for i := 0 to high(RESERVED_KEYWORDS) do
  begin
    u := RESERVED_KEYWORDS[i];
    Check(IsReservedKeyWord(u));
    inc(u[1], 32); // lowercase
    Check(IsReservedKeyWord(u));
    LowerCaseSelf(u);
    Check(IsReservedKeyWord(u));
    u := u + 's';
    Check(not IsReservedKeyWord(u));
    Check(not IsReservedKeyWord(UInt32ToUtf8(i)));
  end;
  SetLength(pets, length(OpenApiRef));
  for i := 0 to high(OpenApiRef) do
  begin
    fn := FormatString('%petstore%.json', [WorkDir, i + 1]);
    pets[i] := StringFromFile(fn);
    if pets[i] = '' then
    begin
      url := OpenApiRef[i];
      if not IdemPChar(pointer(url), 'HTTP') then
        url := 'https://raw.githubusercontent.com/OAI/' +
                 'OpenAPI-Specification/main/examples/' + url;
       JsonBufferReformat(pointer(
        HttpGet(url, nil, false, nil, 0, {forcesock:}false, {igncerterr:}true)),
        pets[i]);
      if pets[i] <> '' then
        FileFromString(pets[i], fn);
    end;
  end;
  for i := 0 to high(pets) do
    if pets[i] <> '' then
    begin
      oa := TOpenApiParser.Create(FormatUtf8('Pets%', [i + 1]));
      try
        oa.ParseJson(pets[i]);
        ud := oa.GenerateDtoUnit;
        Check(ud <> '', 'DTO');
        uc := oa.GenerateClientUnit;
        Check(uc <> '', 'CLIENT');
        //ConsoleWrite(ud);
        //ConsoleWrite(uc);
      finally
        oa.Free;
      end;
    end;
end;

procedure RtspRegressionTests(proxy: TRtspOverHttpServer; test: TSynTestCase;
  clientcount, steps: integer);
type
  TReq = record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUtf8;
  end;
var
  streamer: TCrtSocket;
  req: array of TReq;

  procedure Shutdown;
  var
    r, rmax: PtrInt;
    log: ISynLog;
    timer, one: TPrecisionTimer;
  begin
    log := proxy.Log.Enter(proxy, 'Shutdown');
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
  text: RawUtf8;
  log: ISynLog;
begin
  // here we follow the steps and content stated by https://goo.gl/CX6VA3
  log := proxy.Log.Enter(proxy, 'Tests');
  if (proxy = nil) or
     (proxy.RtspServer <> '127.0.0.1') then
    test.Check(false, 'expect a running proxy on 127.0.0.1')
  else
  try
    rmax := clientcount - 1;
    streamer := TCrtSocket.Bind(proxy.RtspPort);
    try
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % GET', [clientcount], proxy);
      SetLength(req, clientcount);
      for r := 0 to rmax do
        with req[r] do
        begin
          session := TSynTestCase.RandomIdentifier(20 + r and 15);
          get := THttpSocket.Open('localhost', proxy.Server.Port, nlTcp, 1000);
          get.SndLow('GET /sw.mov HTTP/1.0'#13#10 +
                     'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
                     'x-sessioncookie: ' + session + #13#10 +
                     'Accept: ' + RTSP_MIME + #13#10 +
                     'Pragma: no-cache'#13#10 +
                     'Cache-Control: no-cache'#13#10#13#10);
          get.SockRecvLn(text);
          test.Check(text = 'HTTP/1.0 200 OK');
          get.GetHeader(false);
          test.Check(hfConnectionClose in get.Http.HeaderFlags);
          test.Check(get.SockConnected);
          test.Check(get.Http.ContentType = RTSP_MIME);
        end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], proxy);
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
            test.check(req[r].stream.SockReceiveString =
              'DESCRIBE rtsp://tuckru.apple.com/sw.mov RTSP/1.0'#13#10 +
              'CSeq: 1'#13#10 +
              'Accept: application/sdp'#13#10 +
              'Bandwidth: 1500000'#13#10 +
              'Accept-Language: en-US'#13#10 +
              'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10#13#10);
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
            text := get.SockReceiveString;
            //if log <> nil then
            //  log.Log(sllCustom1, 'RegressionTests % #%/% received %',
            //    [clientcount, r, rmax, text], proxy);
            test.CheckEqual(text, session);
          end;
      end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], proxy);
    finally
      Shutdown;
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
  one := Ctxt['one'];
  Ctxt.RouteUtf8('two', two);
  three := Ctxt.RouteEquals('three', '3');
  if not Ctxt.RouteInt64('four', four) then
    four := -1;
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
    one := '';
    two := '';
    three := false;
    four := -1;
    ctxt.Method := met;
    ctxt.Url := uri;
    CheckEqual(router.Process(ctxt), expstatus);
    CheckEqual(one, exp1);
    CheckEqual(two, exp2);
    Check(three = exp3);
    CheckEqual(four, exp4);
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
  ctxt := THttpServerRequest.Create(nil, 0, nil, [], nil);
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

procedure TNetworkProtocols.DNSAndLDAP;
var
  ip, u, v, sid: RawUtf8;
  o: TAsnObject;
  c: cardinal;
  withntp: boolean;
  guid: TGuid;
  i, j, k: PtrInt;
  dns, clients, a: TRawUtf8DynArray;
  le: TLdapError;
  rl, rl2: TLdapResultList;
  r: TLdapResult;
  at: TLdapAttributeType;
  ats: TLdapAttributeTypes;
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
  hasinternet: boolean;
begin
  // validate NTP/SNTP client using NTP_DEFAULT_SERVER = time.google.com
  if not Executable.Command.Get('ntp', ntp) then
    ntp := NTP_DEFAULT_SERVER;
  withntp := not Executable.Command.Option('nontp');
  hasinternet := DnsLookups('yahoo.com') <> nil; // avoid waiting for nothing
  if hasinternet then
  begin
    utc1 := GetSntpTime(ntp);
    //writeln(DateTimeMSToString(utc), ' = ', DateTimeMSToString(NowUtc));
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
  CheckEqual(DnsLookup(''), '');
  CheckEqual(DnsLookup('localhost'), '127.0.0.1');
  CheckEqual(DnsLookup('LocalHost'), '127.0.0.1');
  CheckEqual(DnsLookup('::1'), '127.0.0.1');
  CheckEqual(DnsLookup('1.2.3.4'), '1.2.3.4');
  if hasinternet then
  begin
    ip := DnsLookup('synopse.info');
    CheckEqual(ip, '62.210.254.173', 'dns1');
    ip := DnsLookup('blog.synopse.info');
    CheckEqual(ip, '62.210.254.173', 'dns2');
    CheckEqual(DnsReverseLookup(ip), '62-210-254-173.rev.poneytelecom.eu', 'rev');
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
  Check(RawLdapError(LDAP_RES_TOO_LATE) = leUnknown);
  Check(RawLdapError(10000) = leUnknown);
  Check(RawLdapError(LDAP_RES_AUTHORIZATION_DENIED) = leAuthorizationDenied);
  for le := low(le) to high(le) do
  begin
    Check(LDAP_ERROR_TEXT[le] <> '');
    if le <> leUnknown then
      CheckUtf8(RawLdapError(LDAP_RES_CODE[le]) = le, LDAP_ERROR_TEXT[le]);
  end;
  CheckEqual(LDAP_ERROR_TEXT[leUnknown], 'Unknown');
  CheckEqual(LDAP_ERROR_TEXT[leCompareTrue], 'Compare true');
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
  for at := low(at) to high(at) do
  begin
    CheckEqual(ToText(at), AttrTypeName[at]);
    CheckUtf8(AttributeNameType(AttrTypeName[at]) = at, ToText(at));
    ats := [at];
    a := ToText(ats);
    if at = low(at) then
      Check(a = nil)
    else
    begin
      CheckEqual(length(a), 1);
      CheckEqual(a[0], ToText(at));
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
    CheckEqual(u, 'results: 0'#13#10);
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
                           'cn', v,
                           'sn', 'Doe']);
    CheckEqual(r.Attributes.Count, 3);
    CheckHash(rl.GetJson([]), $8AAB69D2);
    CheckHash(rl.GetJson([roRawValues]), $8AAB69D2);
    CheckHash(rl.GetJson([roNoDCAtRoot]), $8AAB69D2);
    CheckHash(rl.GetJson([roNoObjectName]), $6A4853FA);
    CheckHash(rl.GetJson([roCanonicalNameAtRoot]), $20AF5125);
    CheckHash(rl.GetJson([roObjectNameAtRoot]), $92FE1BFD);
    CheckHash(rl.GetJson([roCommonNameAtRoot]), $047EED2F);
    CheckHash(rl.GetJson([roObjectNameWithoutDCAtRoot, roNoObjectName]), $F41233F2);
    CheckHash(rl.GetJson([roWithCanonicalName]), $C4BA2ED3);
    CheckHash(rl.GetJson([roNoObjectName, roWithCanonicalName]), $0BCFC3BC);
    CheckHash(rl.Dump({noTime=}true), $DF59A0A9, 'hashDump');
    CheckHash(rl.ExportToLdifContent, $4A97B4B2, 'hashLdif');
    CopyObject(rl, rl2);
    CheckHash(rl2.Dump({noTime=}true), $DF59A0A9, 'hashDump2');
    CheckHash(rl2.ExportToLdifContent, $4A97B4B2, 'hashLdif2');
    r.Attributes.Delete(atCommonName);
    CheckEqual(r.Attributes.Count, 2);
    v := rl.GetJson([roNoObjectName]);
    CheckEqual(v, '{"bar":{"foo":{"objectClass":"person","sn":"Doe"}}}');
    r.ObjectName := 'cn=foo, ou=bar, dc=toto, dc=it';
    //writeln(rl.GetJson([roNoDCAtRoot, roNoObjectName]));
    CheckHash(rl.GetJson([]), $DF03674D);
    CheckHash(rl.GetJson([roRawValues]), $DF03674D);
    CheckHash(rl.GetJson([roNoDCAtRoot]), $DB4EF1DC);
    CheckEqual(rl.GetJson([roNoObjectName, roNoDCAtRoot]), v);
    CheckHash(rl.ExportToLdifContent, $31A4283C, 'hashLdif');
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
            Check(one.Search(one.WellKnownObjects[lkoUsers], {typesonly=}false,
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
              Check(res.CopyObjectGUID(guid), 'objectGUID');
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

procedure TNetworkProtocols.TunnelExecute(Sender: TObject);
begin
  // one of the two handshakes should be done in another thread
  tunnelexecutelocal := (Sender as TTunnelLocal).Open(
    session, options, 1000, appsec, cLocalhost, tunnelexecuteremote);
  Check(tunnelexecutelocal <> 0);
  Check(tunnelexecuteremote <> 0);
end;

procedure TNetworkProtocols.TunnelExecuted(Sender: TObject);
begin
  tunnelexecutedone := true;
end;

procedure TNetworkProtocols.TunnelTest(const clientcert, servercert: ICryptCert);
var
  clientinstance, serverinstance: TTunnelLocal;
  clientcb, servercb: ITunnelTransmit;
  clienttunnel, servertunnel: ITunnelLocal;
  i: integer;
  sent, received, sent2, received2: RawByteString;
  clientsock, serversock: TNetSocket;
  local, remote: TNetPort;
begin
  // setup the two instances with the specified options and certificates
  clientinstance := TTunnelLocalClient.Create;
  clientinstance.SignCert := clientcert;
  clientinstance.VerifyCert := servercert;
  clienttunnel := clientinstance;
  clientcb := clientinstance;
  serverinstance := TTunnelLocalServer.Create;
  serverinstance.SignCert := servercert;
  serverinstance.VerifyCert := clientcert;
  servertunnel := serverinstance;
  servercb := serverinstance;
  clienttunnel.SetTransmit(servercb); // set before Open()
  servertunnel.SetTransmit(clientcb);
  // validate handshaking
  session := Random64;
  appsec := RandomAnsi7(10);
  TLoggedWorkThread.Create(
    TSynLog, 'servertunnel', serverinstance, TunnelExecute, TunnelExecuted);
  local := clienttunnel.Open(session, options, 1000, appsec, clocalhost, remote);
  Check(local <> 0);
  Check(remote <> 0);
  SleepHiRes(1000, tunnelexecutedone);
  CheckEqual(local, tunnelexecuteremote);
  CheckEqual(remote, tunnelexecutelocal);
  Check(tunnelexecutedone, 'TunnelExecuted');
  tunnelexecutedone := false; // for the next run
  Check(clienttunnel.LocalPort <> '');
  Check(servertunnel.LocalPort <> '');
  Check(servertunnel.LocalPort <> clienttunnel.LocalPort, 'ports');
  Check(clienttunnel.Encrypted = (toEncrypted * options <> []), 'cEncrypted');
  Check(servertunnel.Encrypted = (toEncrypted * options <> []), 'cEncrypted');
  Check(NewSocket('127.0.0.1', clienttunnel.LocalPort, nlTcp, {bind=}false,
    1000, 1000, 1000, 0, clientsock) = nrOk);
  Check(NewSocket('127.0.0.1', servertunnel.LocalPort, nlTcp, {bind=}false,
    1000, 1000, 1000, 0, serversock) = nrOk);
  try
    // validate raw TCP tunnelling
    CheckEqual(clientinstance.Thread.Received, 0);
    CheckEqual(clientinstance.Thread.Sent, 0);
    CheckEqual(serverinstance.Thread.Received, 0);
    CheckEqual(serverinstance.Thread.Sent, 0);
    for i := 1 to 100 do
    begin
      sent := RandomString(Random32(200) + 1);
      sent2 := RandomString(Random32(200) + 1);
      Check(clientsock.SendAll(pointer(sent), length(sent)) = nrOk);
      Check(serversock.RecvWait(1000, received) = nrOk);
      CheckEqual(sent, received);
      Check(clientsock.SendAll(pointer(sent2), length(sent2)) = nrOk);
      Check(serversock.SendAll(pointer(sent), length(sent)) = nrOk);
      Check(clientsock.RecvWait(1000, received) = nrOk);
      Check(serversock.RecvWait(1000, received2) = nrOk);
      CheckEqual(sent, received);
      CheckEqual(sent2, received2);
      CheckEqual(clientinstance.Thread.Received, serverinstance.Thread.Sent);
      CheckEqual(clientinstance.Thread.Sent, serverinstance.Thread.Received);
      Check(clientinstance.Thread.Received <> 0);
      Check(clientinstance.Thread.Sent <> 0);
      Check(serverinstance.Thread.Received <> 0);
      Check(serverinstance.Thread.Sent <> 0);
    end;
    Check(clientinstance.Thread.Received < clientinstance.Thread.Sent, 'smaller');
    Check(serverinstance.Thread.Received > serverinstance.Thread.Sent, 'bigger');
  finally
    clientsock.ShutdownAndClose(true);
    serversock.ShutdownAndClose(true);
  end;
  servertunnel.SetTransmit(nil); // avoid memory leak due to circular references
end;

procedure TNetworkProtocols._TTunnelLocal;
var
  c, s: ICryptCert;
begin
  c := Cert('syn-es256').Generate([cuDigitalSignature]);
  s := Cert('syn-es256').Generate([cuDigitalSignature]);
  // plain tunnelling
  TunnelTest(nil, nil);
  // symmetric secret encrypted tunnelling
  options := [toEncrypt];
  TunnelTest(nil, nil);
  // ECDHE encrypted tunnelling
  options := [toEcdhe];
  TunnelTest(nil, nil);
  // tunnelling with mutual authentication
  options := [];
  TunnelTest(c, s);
  // symmetric secret encrypted tunnelling with mutual authentication
  options := [toEncrypt];
  TunnelTest(c, s);
  // ECDHE encrypted tunnelling with mutual authentication
  options := [toEcdhe];
  TunnelTest(c, s);
end;

procedure TNetworkProtocols.IPAddresses;
var
  i: PtrInt;
  s: shortstring;
  txt: RawUtf8;
  ip: THash128Rec;
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
  Check(IP4Match('192.168.1.1', '192.168.1.0/24'), 'match1');
  Check(IP4Match('192.168.1.135', '192.168.1.0/24'), 'match2');
  Check(IP4Match('192.168.1.250', '192.168.1.0/24'), 'match3');
  Check(not IP4Match('192.168.2.135', '192.168.1.0/24'), 'match4');
  Check(not IP4Match('191.168.1.250', '192.168.1.0/24'), 'match5');
  Check(not IP4Match('192.168.1', '192.168.1.0/24'), 'match6');
  Check(not IP4Match('192.168.1.135', '192.168.1/24'), 'match7');
  Check(not IP4Match('192.168.1.135', '192.168.1.0/65'), 'match8');
  Check(not IP4Match('193.168.1.1', '192.168.1.0/24'), 'match9');
end;

type
  THttpPeerCacheHook = class(THttpPeerCache); // to test protected methods
  THttpPeerCryptHook = class(THttpPeerCrypt);

procedure TNetworkProtocols._THttpPeerCache;
var
  hpc: THttpPeerCacheHook;
  hpc2: THttpPeerCryptHook; // another instance to validate remote decoding
  hps: THttpPeerCacheSettings;
  msg, msg2: THttpPeerCacheMessage;
  i, n, alter: integer;
  tmp: RawByteString;
  timer: TPrecisionTimer;
begin
  // for further tests, use the dedicated "mORMot GET" (mget) sample
  hps := THttpPeerCacheSettings.Create;
  try
    hps.CacheTempPath := Executable.ProgramFilePath + 'peercachetemp';
    hps.CachePermPath := Executable.ProgramFilePath + 'peercacheperm';
    hps.Port := 8008; // don't use default 8099
    hps.Options := [pcoVerboseLog {,pcoSelfSignedHttps}];
    try
      hpc := THttpPeerCacheHook.Create(hps, 'secret');
      try
        hpc2 := THttpPeerCryptHook.Create('secret');
        try
          hpc2.fSettings := hps;
          hpc2.AfterSettings;
          hpc.MessageInit(pcfBearer, 0, msg);
          msg.Hash.Algo := hfSHA256;
          timer.Start;
          n := 1000;
          for i := 1 to n do
          begin
            msg.Size := i;
            msg.Hash.Hash.i0 := i;
            tmp := hpc.MessageEncode(msg);
            Check(tmp <> '');
            Check(hpc2.MessageDecode(pointer(tmp), length(tmp), msg2), 'hpc2');
            CheckEqual(msg2.Size, i);
            Check(CompareMem(@msg, @msg2, SizeOf(msg)));
          end;
          NotifyTestSpeed('messages', n * 2, n * 2 * SizeOf(msg), @timer);
          Check(ToText(msg) = ToText(msg2));
          timer.Start;
          n := 10000;
          for i := 1 to n do
          begin
            alter := Random32(length(tmp));
            inc(PByteArray(tmp)[alter]); // should be detected at crc level
            Check(not hpc2.MessageDecode(pointer(tmp), length(tmp), msg2), 'alt');
            dec(PByteArray(tmp)[alter]); // restore
          end;
          NotifyTestSpeed('altered', n, n * SizeOf(msg), @timer);
          Check(hpc.MessageDecode(pointer(tmp), length(tmp), msg2), 'hpc');
          Check(CompareMem(@msg, @msg2, SizeOf(msg)));
          for i := 1 to 10 do
            Check(hpc.Ping = nil);
        finally
          hpc2.Free;
        end;
      finally
        hpc.Free;
      end;
    except
      // exception here is likely to be port 8099 already used -> continue
    end;
  finally
    hps.Free;
  end;
end;

end.

