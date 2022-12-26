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
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.test,
  mormot.core.perf,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.async,
  mormot.net.rtsphttp;

type
  /// this test case will validate several low-level protocols
  TNetworkProtocols = class(TSynTestCase)
  protected
    one, two: RawUtf8;
    three: boolean;
    request: integer;
    four: Int64;
    procedure DoRtspOverHttp(options: TAsyncConnectionsOptions);
    function DoRequest_(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest0(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest1(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest2(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest3(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoRequest4(Ctxt: THttpServerRequestAbstract): cardinal;
  published
    /// validate TUriTree high-level structure
    procedure _TUriTree;
    /// RTSP over HTTP, as implemented in SynProtoRTSPHTTP unit
    procedure RtspOverHttp;
    /// RTSP over HTTP, with always temporary buffering
    procedure RtspOverHttpBufferedWrite;
  end;


implementation

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
          post := TCrtSocket.Open('localhost', proxy.Server.Port);
          post.SndLow('POST /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Content-Type: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10 +
            'Content-Length: 32767'#13#10 +
            'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
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
            test.check(text = session);
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

procedure TNetworkProtocols.RtspOverHttp;
begin
  DoRtspOverHttp(ASYNC_OPTION);
end;

procedure TNetworkProtocols.RtspOverHttpBufferedWrite;
begin
  DoRtspOverHttp(ASYNC_OPTION + [acoWritePollOnly]);
end;

function TNetworkProtocols.DoRequest_(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  one := Ctxt['one'];
  two := Ctxt['two'];
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
  ctxt: THttpServerRequestAbstract;
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
  tree := TUriTree.Create;
  try
    tree.insert('romane');
    tree.insert('romanus');
    tree.insert('romulus');
    tree.insert('rubens');
    tree.insert('ruber');
    tree.insert('rubicon');
    tree.insert('rubicundus');
    CheckHash(tree.ToText, $0946B9A0);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create;
  try
    tree.insert('romanus');
    tree.insert('romane');
    tree.insert('rubicundus');
    tree.insert('rubicon');
    tree.insert('ruber');
    tree.insert('romulus');
    tree.insert('rubens');
    CheckHash(tree.ToText, $305E57F1);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create;
  try
    tree.insert('/plaintext');
    tree.insert('/');
    tree.insert('/plain');
    //writeln(tree.ToText);
    CheckHash(tree.ToText, $B3522B86);
  finally
    tree.Free;
  end;
  tree := TUriTree.Create;
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
  tree := TUriTree.Create;
  try
    for i := 0 to high(rnd) do
      rnd[i] := RandomIdentifier(Random32(24) * 2 + 1);
    for i := 0 to high(rnd) do
      CheckEqual(tree.Insert(rnd[i]).FullText, rnd[i]);
    timer.Start;
    for i := 0 to high(rnd) do
      CheckEqual(tree.Find(rnd[i]).FullText, rnd[i]);
    NotifyTestSpeed('big tree lookups', length(rnd));
  finally
    tree.Free;
  end;
  ctxt := THttpServerRequestAbstract.Create;
  router := TUriRouter.Create;
  try
    Call('/plaintext', '', '', false, -1, 0);
    Call('/', '', '', false, -1, 0);
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
    NotifyTestSpeed('URI lookups', 1000);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/static', '/some/static');
    NotifyTestSpeed('URI static rewrites', 1000);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/user/1234', '/root/user.new?id=1234');
    NotifyTestSpeed('URI parametrized rewrites', 1000);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/plaintext', '', 'GET', 200);
    NotifyTestSpeed('URI static execute', 1000);
    timer.Start;
    for i := 1 to 1000 do
      Compute('/do/toto/pic', '', 'GET', 200);
    NotifyTestSpeed('URI parametrized execute', 1000);
    //writeln(router.Tree[urmGet].ToText);
    //writeln(router.Tree[urmPost].ToText);
    CheckHash(router.Tree[urmGet].ToText, $18A0BF58);
    CheckHash(router.Tree[urmPost].ToText, $E173FBB0);
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


end.

