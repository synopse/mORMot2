/// WebSocket Echo Server - mORMot2 port of DMVCFramework sample
/// Demonstrates periodic server-initiated heartbeat messages
program WebSocketServerEcho;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.net.server,
  mormot.net.ws.core,
  mormot.net.ws.server;

type
  /// Per-client session data
  /// This object is created in OnClientConnect and automatically freed on disconnect
  TClientSessionData = class
  private
    fProcess: TWebSocketProcess;
    fMessageCount: Integer;
    fLastPeriodicTicks: Int64;
    fRemoteIP: RawUtf8;
  public
    constructor Create(aProcess: TWebSocketProcess);
    /// Get the heartbeat interval based on client IP and message count
    function GetHeartbeatIntervalMs: Integer;
    property Process: TWebSocketProcess read fProcess;
    property MessageCount: Integer read fMessageCount write fMessageCount;
    property LastPeriodicTicks: Int64 read fLastPeriodicTicks write fLastPeriodicTicks;
    property RemoteIP: RawUtf8 read fRemoteIP;
  end;

  /// Custom WebSocket protocol for echo with periodic messages
  TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
  private
    fConnectionsLock: TRTLCriticalSection;
    fConnections: array of TClientSessionData;
    procedure AddConnection(session: TClientSessionData);
    procedure RemoveConnection(session: TClientSessionData);
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame; const info: RawUtf8); override;
  public
    constructor Create(const aName, aUri: RawUtf8); reintroduce;
    destructor Destroy; override;
    /// Send periodic heartbeat messages to all connected clients
    procedure SendPeriodicMessages;
  end;

{ TClientSessionData }

constructor TClientSessionData.Create(aProcess: TWebSocketProcess);
begin
  inherited Create;
  fProcess := aProcess;
  fMessageCount := 0;
  fLastPeriodicTicks := GetTickCount64;
  fRemoteIP := aProcess.Protocol.RemoteIP;
end;

function TClientSessionData.GetHeartbeatIntervalMs: Integer;
begin
  // Dynamic interval based on client IP
  if (fRemoteIP = '127.0.0.1') or (fRemoteIP = '::1') then
    Result := 2000  // localhost: 2 seconds
  else if (PosEx('192.168.', fRemoteIP) > 0) or (PosEx('10.', fRemoteIP) = 1) then
    Result := 3000  // LAN: 3 seconds
  else
    Result := 10000; // remote: 10 seconds

  // Slow down after 5 messages (demonstrates dynamic adjustment)
  if fMessageCount >= 5 then
    Result := Result * 2;
end;

{ TWebSocketProtocolEcho }

constructor TWebSocketProtocolEcho.Create(const aName, aUri: RawUtf8);
begin
  inherited Create(aName, aUri, nil);
  InitializeCriticalSection(fConnectionsLock);
  SetLength(fConnections, 0);
end;

destructor TWebSocketProtocolEcho.Destroy;
begin
  DeleteCriticalSection(fConnectionsLock);
  inherited;
end;

procedure TWebSocketProtocolEcho.AddConnection(session: TClientSessionData);
begin
  EnterCriticalSection(fConnectionsLock);
  try
    SetLength(fConnections, Length(fConnections) + 1);
    fConnections[High(fConnections)] := session;
  finally
    LeaveCriticalSection(fConnectionsLock);
  end;
end;

procedure TWebSocketProtocolEcho.RemoveConnection(session: TClientSessionData);
var
  i, j: Integer;
begin
  EnterCriticalSection(fConnectionsLock);
  try
    for i := 0 to High(fConnections) do
      if fConnections[i] = session then
      begin
        for j := i to High(fConnections) - 1 do
          fConnections[j] := fConnections[j + 1];
        SetLength(fConnections, Length(fConnections) - 1);
        Break;
      end;
  finally
    LeaveCriticalSection(fConnectionsLock);
  end;
end;

procedure TWebSocketProtocolEcho.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame; const info: RawUtf8);
var
  msg: RawUtf8;
  sessionData: TClientSessionData;
  echoFrame: TWebSocketFrame;
begin
  TextColor(ccLightMagenta);
  write(GetEnumName(TypeInfo(TWebSocketFrameOpCode), ord(Frame.opcode))^, ' - ');
  TextColor(ccWhite);

  case Frame.opcode of
    focContinuation:
      begin
        write('Connected');
        // Initialize session data for this connection
        if Sender.Protocol.ConnectionOpaque^.ValueExternal = 0 then
        begin
          sessionData := TClientSessionData.Create(Sender);
          Sender.Protocol.ConnectionOpaque^.ValueExternal := PtrUInt(sessionData);
          // Add to our tracked connections for periodic messages
          AddConnection(sessionData);
        end;
      end;

    focConnectionClose:
      begin
        write('Disconnected');
        // Log session statistics before cleanup
        if Sender.Protocol.ConnectionOpaque^.ValueExternal <> 0 then
        begin
          sessionData := TClientSessionData(Sender.Protocol.ConnectionOpaque^.ValueExternal);
          TextColor(ccYellow);
          writeln;
          writeln('  -> Total heartbeat messages sent to this client: ', sessionData.MessageCount);
          // Remove from tracked connections
          RemoveConnection(sessionData);
          // Free session data
          sessionData.Free;
          Sender.Protocol.ConnectionOpaque^.ValueExternal := 0;
        end;
      end;

    focText,
    focBinary:
      begin
        write('Echoing ', length(Frame.payload), ' bytes');
        // Echo back the message
        msg := Frame.payload;
        FormatUtf8('Echo: %', [msg], msg);
        echoFrame.opcode := focText;
        echoFrame.payload := msg;
        Sender.SendFrame(echoFrame);
      end;

    focPing:
      begin
        write('Ping received, sending Pong');
        // Pong is automatically sent by the framework
      end;
  end;

  TextColor(ccCyan);
  writeln(' from ', Sender.Protocol.RemoteIP, '/', Sender.Protocol.ConnectionID);
end;

procedure TWebSocketProtocolEcho.SendPeriodicMessages;
var
  i: Integer;
  session: TClientSessionData;
  frame: TWebSocketFrame;
  msg: RawUtf8;
  nowTicks: Int64;
  nowTime: TDateTime;
  elapsedMs: Int64;
  intervalMs: Integer;
  connections: array of TClientSessionData;
begin
  nowTicks := GetTickCount64;
  nowTime := Now;

  // Copy connections under lock to avoid holding lock during SendFrame
  EnterCriticalSection(fConnectionsLock);
  try
    SetLength(connections, Length(fConnections));
    for i := 0 to High(fConnections) do
      connections[i] := fConnections[i];
  finally
    LeaveCriticalSection(fConnectionsLock);
  end;

  // Send periodic messages to clients that need them
  for i := 0 to High(connections) do
  begin
    session := connections[i];
    if session = nil then
      Continue;

    intervalMs := session.GetHeartbeatIntervalMs;
    // Calculate elapsed time using tick difference
    elapsedMs := nowTicks - session.fLastPeriodicTicks;

    if elapsedMs >= intervalMs then
    begin
      Inc(session.fMessageCount);
      session.fLastPeriodicTicks := nowTicks;

      // Build heartbeat message
      FormatUtf8('[SERVER HEARTBEAT #%] Time: % | Interval: % ms | IP: %',
        [session.MessageCount, TimeToStr(nowTime), intervalMs, session.RemoteIP], msg);

      // Send to this client
      frame.opcode := focText;
      frame.payload := msg;

      try
        session.Process.SendFrame(frame);

        // Log to console
        TextColor(ccGreen);
        write('[', TimeToStr(nowTime), '] ');
        TextColor(ccWhite);
        writeln('Sent heartbeat #', session.MessageCount, ' to ', session.RemoteIP,
          ' (interval: ', intervalMs, 'ms)');
      except
        // Client may have disconnected - ignore errors
      end;
    end;
  end;
end;

var
  Server: TWebSocketServer;
  Protocol: TWebSocketProtocolEcho;
  LastCheckTicks: Int64;

procedure Run;
begin
  ReportMemoryLeaksOnShutdown := True;

  try
    TextColor(ccLightCyan);
    writeln('=== WebSocket Echo Server with Periodic Messages (mORMot2) ===');
    writeln('');
    TextColor(ccWhite);
    writeln('Starting echo server on port 9091...');

    Server := TWebSocketServer.Create('9091', nil, nil, 'echo');
    try
      Protocol := TWebSocketProtocolEcho.Create('echo', '');
      Server.WebSocketProtocols.Add(Protocol);

      TextColor(ccLightGreen);
      writeln('Echo server running!');
      writeln('');
      TextColor(ccWhite);
      writeln('Connect with:');
      writeln('  ws://localhost:9091/');
      writeln('');
      writeln('Features:');
      writeln('  - Echoes back all text messages');
      writeln('  - Responds to Ping with Pong');
      writeln('  - Sends periodic heartbeat messages (interval varies by client)');
      writeln('  - Interval: localhost=2s, LAN=3s, remote=10s');
      writeln('  - Interval doubles after 5 messages (demonstrates dynamic adjustment)');
      writeln('  - Per-client session tracking');
      writeln('');
      TextColor(ccLightGray);
      writeln('Press ENTER to stop...');
      writeln('');

      // Main loop with periodic message sending
      LastCheckTicks := GetTickCount64;
      while True do
      begin
        // Check for Enter key (VK_RETURN = 13)
        if ConsoleKeyPressed(13) then
          Break;

        // Send periodic messages every 100ms
        if GetTickCount64 - LastCheckTicks >= 100 then
        begin
          Protocol.SendPeriodicMessages;
          LastCheckTicks := GetTickCount64;
        end;

        // Small sleep to avoid busy-waiting
        SleepHiRes(10);
      end;

      TextColor(ccYellow);
      writeln('Stopping server...');

    finally
      Server.Free;
    end;

    TextColor(ccWhite);
    writeln('Done.');

  except
    on E: Exception do
    begin
      TextColor(ccLightRed);
      writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      TextColor(ccWhite);
      readln;
      ExitCode := 1;
    end;
  end;
end;

begin
  Run;
end.
