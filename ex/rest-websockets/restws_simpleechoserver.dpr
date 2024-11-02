/// simple Echo server using WebSockets
program restws_simpleechoserver;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // use FastMM4 on older versions of Delphi
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.net.server,
  mormot.net.ws.core,
  mormot.net.ws.server;

type
  TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
  protected
    procedure EchoFrame(Sender: TWebSocketProcess; const Frame: TWebSocketFrame);
  end;

{$I-} // for write/writeln below

procedure TWebSocketProtocolEcho.EchoFrame(Sender: TWebSocketProcess; const Frame: TWebSocketFrame);
begin
  TextColor(ccLightMagenta);
  write(GetEnumName(TypeInfo(TWebSocketFrameOpCode), ord(Frame.opcode))^, ' - ');
  TextColor(ccWhite);
  case Frame.opcode of
    focContinuation:
      write('Connected');
    focConnectionClose:
      write('Disconnected');
    focText,
    focBinary: 
      begin
        write('Echoing ', length(Frame.payload), ' bytes');
        SendFrame(Sender, Frame); // echo back the same frame
        // a real protocol would use e.g. SendFrameJson() here
      end;
  end;
  TextColor(ccCyan);
  writeln(' from ', Sender.Protocol.RemoteIP, '/', Sender.Protocol.ConnectionID);
end;

procedure Run;                   
var Server: TWebSocketServer;
    protocol: TWebSocketProtocolEcho;
begin
  Server := TWebSocketServer.Create('8888', nil, nil, 'test');
  try
    protocol := TWebSocketProtocolEcho.Create('meow', '');
    protocol.OnIncomingFrame := protocol.EchoFrame;
    Server.WebSocketProtocols.Add(protocol);
    TextColor(ccLightGreen);
    writeln('WebSockets Chat Server running on localhost:8888'#13#10);
    TextColor(ccWhite);
    writeln('Please load restws_simpleechoserver.html in your browser'#13#10);
    TextColor(ccLightGray);
    writeln('Press [Enter] to quit'#13#10);
    TextColor(ccCyan);
    readln;
  finally
    Server.Free;
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
