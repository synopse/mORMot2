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
  mormot.core.crypto,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.test,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.rtsphttp;

type
  /// this test case will validate several low-level protocols
  TNetworkProtocols = class(TSynTestCase)
  published
    /// RTSP over HTTP, as implemented in SynProtoRTSPHTTP unit
    procedure RTSPOverHTTP;
  end;

  
implementation

procedure RegressionTests(proxy: TRTSPOverHTTPServer; test: TSynTestCase;
  clientcount, steps: integer);
type
  TReq = record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUTF8;
  end;
var
  streamer: TCrtSocket;
  req: array of TReq;
  rmax, r, i: PtrInt;
  text: RawUTF8;
  log: ISynLog;
begin
  // here we follow the steps and content stated by https://goo.gl/CX6VA3
  log := proxy.Log.Enter(proxy, 'Tests');
  if (proxy = nil) or (proxy.RtspServer <> '127.0.0.1') then
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
          get := THttpSocket.Open('localhost', proxy.Server.Port);
          get.Write('GET /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Accept: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10#13#10);
          get.SockRecvLn(text);
          test.Check(text = 'HTTP/1.0 200 OK');
          get.GetHeader(false);
          test.Check(hfConnectionClose in get.HeaderFlags);
          test.Check(get.SockConnected);
          test.Check(get.ContentType = RTSP_MIME);
        end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], proxy);
      for r := 0 to rmax do
        with req[r] do
        begin
          post := TCrtSocket.Open('localhost', proxy.Server.Port);
          post.Write('POST /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Content-Type: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10 +
            'Content-Length: 32767'#13#10 +
            'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
          stream := streamer.AcceptIncoming;
          if stream = nil then
          begin
            test.Check(false);
            exit;
          end;
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
            req[r].post.Write(
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
          req[r].stream.Write(req[r].session);
        for r := 0 to rmax do
          with req[r] do
            test.check(get.SockReceiveString = session);
      end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], proxy);
    finally
      // first half deletes POST first, second half deletes GET first
      for r := 0 to rmax shr 1 do
        req[r].post.Free;
      req[0].stream.Free; // validates remove POST when RTSP already down
      for r := (rmax shr 1) + 1 to rmax do
        req[r].get.Free;
      for r := 0 to rmax shr 1 do
        req[r].get.Free;
      for r := (rmax shr 1) + 1 to rmax do
        req[r].post.Free;
      Sleep(10); // warning: waits typically 10-15 ms on Windows
      for r := 1 to rmax do
        req[r].stream.Free;
      streamer.Free;
    end;
  except
    on E: Exception do
      test.Check(false, E.ClassName);
  end;
end;

procedure TNetworkProtocols.RTSPOverHTTP;
const
  {$ifdef DARWIN}
  N = 10;
  {$else}
  N = 100;
  {$endif DARWIN}
var
  proxy: TRTSPOverHTTPServer;
begin
  proxy := TRTSPOverHTTPServer.Create(
    '127.0.0.1', '3999', '3998', TSynLog, nil, nil);
  try
    RegressionTests(proxy, self, N, 10);
  finally
    proxy.Free;
  end;
end;

end.

