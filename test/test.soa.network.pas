/// regression tests for RESTful SOA over Http or WebSockets
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.soa.network;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  contnrs,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.mustache,
  mormot.core.test,
  mormot.core.interfaces,
  mormot.core.jwt,
  mormot.net.client,
  mormot.net.server,
  mormot.net.relay,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.net.ws.server,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.storage,
  mormot.orm.sqlite3,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.sqlite3,
  mormot.rest.http.client,
  mormot.rest.http.server,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  test.core.data,
  test.core.base,
  test.orm.core;


type
  /// SOA callback definition as expected by TTestBidirectionalRemoteConnection
  IBidirCallback = interface(IInvokable)
    ['{5C5818CC-FFBA-445C-82C1-39F45B84520C}']
    procedure AsyncEvent(a: integer);
    function Value: Integer;
  end;

  /// SOA service definition as expected by TTestBidirectionalRemoteConnection
  IBidirService = interface(IInvokable)
    ['{0984A2DA-FD1F-49D6-ACFE-4D45CF08CA1B}']
    function TestRest(a, b: integer; out c: RawUtf8): variant;
    function TestRestCustom(a: integer): TServiceCustomAnswer;
    function TestCallback(d: Integer; const callback: IBidirCallback): boolean;
    procedure LaunchAsyncCallback(a: integer);
    procedure RemoveCallback;
  end;

  TBidirServer = class(TInterfacedObject, IBidirService)
  protected
    fCallback: IBidirCallback;
    // IBidirService implementation methods
    function TestRest(a, b: integer; out c: RawUtf8): variant;
    function TestRestCustom(a: integer): TServiceCustomAnswer;
    function TestCallback(d: Integer; const callback: IBidirCallback): boolean;
    procedure LaunchAsyncCallback(a: integer);
    procedure RemoveCallback;
  public
    function LaunchSynchCallback: integer;
  end;

  /// a test case for all bidirectional remote access, e.g. WebSockets
  TTestBidirectionalRemoteConnection = class(TSynTestCase)
  protected
    fHttpServer: TRestHttpServer;
    fServer: TRestServerFullMemory;
    fBidirServer: TBidirServer;
    fPublicRelayClientsPort, fPublicRelayPort: RawUtf8;
    fPublicRelay: TPublicRelay;
    fPrivateRelay: TPrivateRelay;
    procedure CleanUp; override;
    function NewClient(const port: RawUtf8): TRestHttpClientWebsockets;
    procedure WebsocketsLowLevel(protocol: TWebSocketProtocol;
      opcode: TWebSocketFrameOpCode; const name: RawUtf8);
    procedure TestRest(Rest: TRest);
    procedure TestCallback(Rest: TRest);
    procedure SoaCallbackViaWebsockets(Ajax, Relay: boolean);
  public
    property HttpServer: TRestHttpServer
      read fHttpServer;
  published
    /// low-level test of our 'synopsejson' and 'synopsebinary' protocols
    procedure WebsocketsProtocols;
    /// launch the WebSockets-ready HTTP server
    procedure RunHttpServer;
    /// test the callback mechanism via interface-based services on server side
    procedure SoaCallbackOnServerSide;
    /// test callbacks via interface-based services over JSON WebSockets
    procedure SoaCallbackViaJsonWebsockets;
    /// test callbacks via interface-based services over binary WebSockets
    procedure SoaCallbackViaBinaryWebsockets;
    /// initialize SynProtoRelay tunnelling
    procedure RelayStart;
    /// test SynProtoRelay tunnelling over JSON WebSockets
    procedure RelaySoaCallbackViaJsonWebsockets;
    /// verify ability to reconect from Private Relay to Public Relay
    procedure RelayConnectionRecreate;
    /// test SynProtoRelay tunnelling over binary WebSockets
    procedure RelaySoaCallbackViaBinaryWebsockets;
    /// finalize SynProtoRelay tunnelling
    procedure RelayShutdown;
    /// test Master/Slave replication using TRecordVersion field over WebSockets
    procedure _TRecordVersion;
  end;


implementation

uses
  test.orm.sqlite3;

type
  TBidirCallbackInterfacedObject = class(TInterfacedObject, IBidirCallback)
  protected
    fValue: Integer;
  public
    function Value: Integer;
    procedure AsyncEvent(a: integer);
  end;

  TBidirCallback = class(TInterfacedCallback, IBidirCallback)
  protected
    fValue: Integer;
  public
    function Value: Integer;
    procedure AsyncEvent(a: integer);
  end;


{ TBidirServer }

function TBidirServer.TestRest(a, b: integer; out c: RawUtf8): variant;
begin
  c := Int32ToUtf8(a + b);
  result := _ObjFast(['a', a, 'b', b, 'c', c]);
end;

function TBidirServer.TestRestCustom(a: integer): TServiceCustomAnswer;
begin
  result.Header := BINARY_CONTENT_TYPE_HEADER;
  result.Content := Int32ToUtf8(a) + #0#1;
  result.Status := HTTP_SUCCESS;
end;

function TBidirServer.TestCallback(d: Integer;
  const callback: IBidirCallback): boolean;
begin
  fCallback := callback;
  result := d <> 0;
end;

procedure TBidirServer.LaunchAsyncCallback(a: integer);
begin
  if Assigned(fCallback) then
    fCallback.AsyncEvent(a);
end;

function TBidirServer.LaunchSynchCallback: integer;
begin
  if Assigned(fCallback) then
    result := fCallback.Value
  else
    result := 0;
end;

procedure TBidirServer.RemoveCallback;
begin
  fCallback := nil;
end;


{ TBidirCallbackInterfacedObject }

procedure TBidirCallbackInterfacedObject.AsyncEvent(a: integer);
begin
  inc(fValue, a);
end;

function TBidirCallbackInterfacedObject.Value: integer;
begin
  result := fValue;
end;


{ TBidirCallback }

procedure TBidirCallback.AsyncEvent(a: integer);
begin
  inc(fValue, a);
end;

function TBidirCallback.Value: integer;
begin
  result := fValue;
end;


{ TTestBidirectionalRemoteConnection }

const
  WEBSOCKETS_KEY = 'key';

procedure TTestBidirectionalRemoteConnection.WebsocketsProtocols;
var
  Settings: TWebSocketProcessSettings;
begin
  Settings.SetDefaults;
  WebsocketsLowLevel(
    TWebSocketProtocolJson.Create(''), focText, '');
  WebsocketsLowLevel(
    TWebSocketProtocolBinary.Create('', false, '', @Settings, false),
    focBinary, '');
  WebsocketsLowLevel(
    TWebSocketProtocolBinary.Create('', false, 'pass', @Settings, false),
    focBinary, ' encrypted');
  WebsocketsLowLevel(
    TWebSocketProtocolBinary.Create('', false, '', @Settings, true),
    focBinary, ' compressed');
  WebsocketsLowLevel(
    TWebSocketProtocolBinary.Create('', false, 'pass', @Settings, true),
      focBinary, ' encrypted compressed');
end;


type // to access protected low-level frame methods
  TWebSocketProtocolRestHook = class(TWebSocketProtocolRest);

procedure TTestBidirectionalRemoteConnection.WebsocketsLowLevel(
  protocol: TWebSocketProtocol; opcode: TWebSocketFrameOpCode;
  const name: RawUtf8);

  procedure TestOne(const content, contentType: RawByteString);
  var
    C1, C2: THttpServerRequest;
    P2: TWebSocketProtocol;
    frame: TWebSocketFrame;
    head: RawUtf8;
    noAnswer1, noAnswer2: boolean;
    i: integer;
  begin
    C1 := THttpServerRequest.Create(nil, 0, nil, []);
    C2 := THttpServerRequest.Create(nil, 0, nil, []);
    P2 := protocol.Clone('');
    try
      for i := 1 to 100 do
      begin
        C1.Prepare('url', 'POST', 'headers', content, contentType, '');
        noAnswer1 := opcode = focBinary;
        noAnswer2 := not noAnswer1;
        TWebSocketProtocolRestHook(protocol).InputToFrame(C1, noAnswer1, frame, head);
        check(frame.opcode = opcode);
        TWebSocketProtocolRestHook(P2).FrameToInput(frame, noAnswer2, C2);
        check(noAnswer1 = noAnswer2);
        check(C2.URL = 'url');
        check(C2.Method = 'POST');
        check(C2.InHeaders = 'headers');
        check(C2.InContentType = contentType);
        check(C2.InContent = content);
        C1.OutContent := content;
        C1.OutContentType := contentType;
        C1.OutCustomHeaders := 'outheaders';
        frame.opcode := focContinuation;
        head := 'answer';
        TWebSocketProtocolRestHook(protocol).OutputToFrame(C1, 200, head, frame);
        check(frame.opcode = opcode);
        check(TWebSocketProtocolRestHook(P2).FrameToOutput(frame, C2) = 200);
        check(C2.OutContent = content);
        check(C2.OutContentType = contentType);
        check(C2.OutCustomHeaders = 'outheaders');
      end;
    finally
      P2.Free;
      C2.Free;
      C1.Free;
    end;
  end;

var
  timer: TPrecisionTimer;
begin
  timer.Start;
  try
    TestOne('content', TEXT_CONTENT_TYPE);
    TestOne('{"content":1234}', JSON_CONTENT_TYPE);
    TestOne('"content"', JSON_CONTENT_TYPE);
    TestOne('["json",2]', JSON_CONTENT_TYPE);
    TestOne('binary'#0'data', BINARY_CONTENT_TYPE);
    // NotifyTestSpeed('%%', [protocol.Name, name], 100 * 5, 0, @timer);
  finally
    protocol.Free;
  end;
end;

procedure TTestBidirectionalRemoteConnection.RunHttpServer;
var
  port: integer;
begin
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IBidirService), TypeInfo(IBidirCallback)]);
  // sicClientDriven services expect authentication for sessions
  fServer := TRestServerFullMemory.CreateWithOwnModel([], true);
  fServer.Server.CreateMissingTables;
  fBidirServer := TBidirServer.Create;
  check(fServer.ServiceDefine(fBidirServer, [IBidirService]) <> nil);
  fHttpServer := TRestHttpServer.Create(HTTP_DEFAULTPORT, [], '+', useBidirSocket);
  check(fHttpServer.AddServer(fServer));
  fHttpServer.WebSocketsEnable(fServer, WEBSOCKETS_KEY, true).Settings.SetFullLog;
  //(fHttpServer.HttpServer as TWebSocketServer).HeartbeatDelay := 5000;
  port := Utf8ToInteger(HTTP_DEFAULTPORT);
  fPublicRelayClientsPort := ToUtf8(port + 1);
  fPublicRelayPort := ToUtf8(port + 2);
end;

procedure TTestBidirectionalRemoteConnection.TestRest(Rest: TRest);
var
  I: IBidirService;
  a, b: integer;
  c: RawUtf8;
  v: variant;
  res: TServiceCustomAnswer;
begin
  Rest.Services.Resolve(IBidirService, I);
  if CheckFailed(Assigned(I), 'Rest IBidirService') then
    exit;
  for a := -10 to 10 do
    for b := -10 to 10 do
    begin
      v := I.TestRest(a, b, c);
      check(GetInteger(pointer(c)) = a + b);
      if CheckFailed(DocVariantType.IsOfType(v)) then
        continue;
      check(v.a = a);
      check(v.b = b);
      check(v.c = c);
    end;
  for a := -10 to 10 do
  begin
    res := I.TestRestCustom(a);
    check(res.Status = HTTP_SUCCESS);
    check(GetInteger(pointer(res.Content)) = a);
    check(res.Content[Length(res.Content)] = #1);
  end;
end;

procedure TTestBidirectionalRemoteConnection.TestCallback(Rest: TRest);
var
  I: IBidirService;
  d: integer;
  subscribed: IBidirCallback;

  procedure WaitUntilNotified;
  var
    timeout: Int64;
  begin
    timeout := GetTickCount64 + 5000;
    while (subscribed.value <> 6) and
          (GetTickCount64 < timeout) do
      sleep(1);
    check(subscribed.value = 6);
  end;

begin
  Rest.Services.Resolve(IBidirService, I);
  if CheckFailed(Assigned(I), 'Callback IBidirService') then
    exit;
  subscribed := TBidirCallbackInterfacedObject.Create;
  for d := -5 to 6 do
  begin
    check(I.TestCallback(d, subscribed) = (d <> 0));
    I.LaunchAsyncCallback(d);
  end;
  WaitUntilNotified;
  check(fBidirServer.LaunchSynchCallback = 6);
  Rest.Services.CallBackUnRegister(subscribed); // manual callback release notify
  subscribed := TBidirCallback.Create(Rest, IBidirCallback); // auto notification
  for d := -5 to 6 do
  begin
    check(I.TestCallback(d, subscribed) = (d <> 0));
    I.LaunchAsyncCallback(d);
  end;
  WaitUntilNotified;
  subscribed := TBidirCallback.Create(Rest, IBidirCallback);
  for d := -5 to 6 do
  begin
    check(I.TestCallback(d, subscribed) = (d <> 0));
    I.LaunchAsyncCallback(d);
    I.RemoveCallback;
  end;
  WaitUntilNotified;
  check(fBidirServer.LaunchSynchCallback = 0);
end; // here TBidirCallback.Free will notify Rest.Services.CallBackUnRegister()

procedure TTestBidirectionalRemoteConnection.SoaCallbackOnServerSide;
begin
  TestRest(fServer);
  TestCallback(fServer);
  TestRest(fServer);
end;

function TTestBidirectionalRemoteConnection.NewClient(const port: RawUtf8):
  TRestHttpClientWebsockets;
begin
  result := TRestHttpClientWebsockets.Create('127.0.0.1', port, TOrmModel.Create
    (fServer.Model));
  result.Model.Owner := result;
  result.WebSockets.Settings.SetFullLog;
end;

procedure TTestBidirectionalRemoteConnection.SoaCallbackViaWebsockets(
  Ajax, Relay: boolean);

  procedure ServiceDefine(c: TRestHttpClientWebsockets; const msg: string);
  begin
    check(c.SetUser('User', 'synopse'), 'setuser' + msg);
    check(c.ServiceDefine(IBidirService, sicShared) <> nil, 'IBidirService' + msg);
  end;

var
  c1, c2: TRestHttpClientWebsockets;
  port: RawUtf8;
  stats: RawUtf8;
begin
  if Relay then
    port := fPublicRelayClientsPort
  else
    port := HTTP_DEFAULTPORT;
  c1 := NewClient(port);
  try
    // check plain HTTP REST calls
    check(c1.ServerTimestampSynchronize);
    ServiceDefine(c1, '1');
    TestRest(c1);
    // check WebSockets communication
    CheckEqual(c1.WebSocketsUpgrade(WEBSOCKETS_KEY, Ajax, true), '',
      'WebSocketsUpgrade1');
    TestCallback(c1);
    c2 := NewClient(port);
    try
      CheckEqual(c2.WebSocketsUpgrade(WEBSOCKETS_KEY, Ajax, true), '',
        'WebSocketsUpgrade2');
      ServiceDefine(c2, '2');
      TestCallback(c2);
      if Relay then
      begin
        stats := OpenHttpGet('127.0.0.1', fPublicRelayPort, '/stats', '');
        check(PosEx('"version"', stats) > 0, 'stats');
      end;
      TestRest(c1);
      TestRest(c2);
    finally
      c2.Free;
    end;
  finally
    c1.Free;
  end;
end;

procedure TTestBidirectionalRemoteConnection.SoaCallbackViaJsonWebsockets;
begin
  SoaCallbackViaWebsockets({ajax=}true, {relay=}false);
end;

procedure TTestBidirectionalRemoteConnection.SoaCallbackViaBinaryWebsockets;
begin
  SoaCallbackViaWebsockets({ajax=}false, {relay=}false);
end;

procedure TTestBidirectionalRemoteConnection.RelayStart;
const
  RELAYKEY = 'aes256secret';
var
  stats: RawUtf8;
begin
  fPublicRelay := TPublicRelay.Create(nil, fPublicRelayClientsPort,
    fPublicRelayPort, RELAYKEY, TJwtHS256.Create('jwtsecret', 100, [], []));
  fPrivateRelay := TPrivateRelay.Create(nil, '127.0.0.1', fPublicRelayPort,
    RELAYKEY, fPublicRelay.ServerJWT.Compute([]), '127.0.0.1', HTTP_DEFAULTPORT,
    'X-Real-IP');
  check(not fPrivateRelay.Connected);
  check(fPrivateRelay.TryConnect);
  checkEqual(OpenHttpGet('127.0.0.1', fPublicRelayPort, '/invalid', ''), '', 'wrong URI');
  stats := OpenHttpGet('127.0.0.1', fPublicRelayPort, '/stats', '');
  check(PosEx('version', stats) > 0, 'stats');
end;

procedure TTestBidirectionalRemoteConnection.RelaySoaCallbackViaJsonWebsockets;
begin
  SoaCallbackViaWebsockets({ajax=}true, {relay=}true);
  SleepHiRes(10);
end;

procedure TTestBidirectionalRemoteConnection.RelayConnectionRecreate;
begin
  check(fPrivateRelay.TryConnect);
end;

procedure TTestBidirectionalRemoteConnection.RelaySoaCallbackViaBinaryWebsockets;
begin
  SoaCallbackViaWebsockets({ajax=}false, {relay=}true);
end;

procedure TTestBidirectionalRemoteConnection.RelayShutdown;
var
  stats: RawUtf8;
begin
  stats := OpenHttpGet('127.0.0.1', fPublicRelayPort, '/stats', '');
  check(PosEx('"version"', stats) > 0, 'stats');
  fPrivateRelay.Free;
  SleepHiRes(100);
  stats := OpenHttpGet('127.0.0.1', fPublicRelayPort, '/stats', '');
  check(PosEx('"version"', stats) > 0, 'stats');
  fPublicRelay.Free;
  SleepHiRes(10);
end;

procedure TTestBidirectionalRemoteConnection._TRecordVersion;
begin
  TestMasterSlaveRecordVersion(Self, 'ws.db3');
end;

procedure TTestBidirectionalRemoteConnection.CleanUp;
begin
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
end;



end.

