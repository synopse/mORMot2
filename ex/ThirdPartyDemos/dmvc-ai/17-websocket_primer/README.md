# mORMot2 WebSocket Echo Server

Port of DMVCFramework's `websocket_primer` sample to mORMot2, demonstrating WebSocket functionality with periodic server-initiated messages.

## Original Sample

**Source**: `/mnt/w/DMVCframework/samples/websocket_primer/`

## Features

This example demonstrates:

- ✅ **WebSocket Server** - Echo server on port 9091
- ✅ **Bidirectional Communication** - Client-to-server and server-to-client messages
- ✅ **Periodic Heartbeat Messages** - Server sends automatic heartbeat messages to connected clients
- ✅ **Per-Client Session Data** - Maintains message count and timing per client
- ✅ **Dynamic Intervals** - Heartbeat interval adjusts based on client IP and message count
- ✅ **HTML Test Client** - Modern web interface for testing
- ✅ **Connection Lifecycle** - Proper connect/disconnect handling

## Architecture

### Server Implementation

The server uses mORMot2's WebSocket infrastructure:

**Key Classes:**
- `TWebSocketServer` - Main HTTP/WebSocket server
- `TWebSocketProtocolEcho` - Custom protocol extending `TWebSocketProtocolChat`
- `TClientSessionData` - Per-client session management

**Key Features:**
```pascal
// Custom protocol with frame processing
TWebSocketProtocolEcho.ProcessIncomingFrame()
  - Handles text/binary messages
  - Echoes messages back to client
  - Manages session data

// Periodic message sending
TWebSocketProtocolEcho.SendPeriodicMessages()
  - Sends heartbeat messages at dynamic intervals
  - Interval varies by client IP (localhost: 2s, LAN: 3s, remote: 10s)
  - Slows down after 5 messages (demonstrates dynamic adjustment)
```

### mORMot2 Mapping

| DMVCFramework Concept | mORMot2 Equivalent |
|----------------------|-------------------|
| `TMVCWebSocketServer` | `TWebSocketServer` |
| `TMVCWebSocketClient` (client object) | `TWebSocketProcess` |
| `OnClientConnect` callback | Connection state in `ProcessIncomingFrame` |
| `OnMessage` callback | `ProcessIncomingFrame` override |
| `OnPeriodicMessage` callback | Custom `SendPeriodicMessages` method |
| `AClient.Data` (session storage) | `Sender.Protocol.ConnectionOpaque` |
| `AClient.SendText()` | `Sender.SendFrame()` |

### Protocol Negotiation

The server uses the `'echo'` WebSocket subprotocol:

```pascal
// Server side
Protocol := TWebSocketProtocolEcho.Create('echo', '');
Server.WebSocketProtocols.Add(Protocol);

// Client side (JavaScript)
ws = new WebSocket('ws://localhost:9091/', 'echo');
```

## Project Structure

```
17-websocket_primer/
├── WebSocketServerEcho.dpr       # Main server program
├── WebSocketServerEcho.dproj     # Delphi project file (D12)
├── www/
│   └── index.html                # HTML test client
└── README.md                     # This file
```

## Running the Sample

### 1. Build and Run Server

```bash
# Compile
cd /mnt/w/mORMot2/ex/dmvc/17-websocket_primer
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe WebSocketServerEcho.dproj

# Run
./Win32/Debug/WebSocketServerEcho.exe
```

**Expected Output:**
```
=== WebSocket Echo Server with Periodic Messages (mORMot2) ===

Starting echo server on port 9091...
Echo server running!

Connect with:
  ws://localhost:9091/

Features:
  - Echoes back all text messages
  - Responds to Ping with Pong
  - Sends periodic heartbeat messages (interval varies by client)
  - Interval increases after 5 messages (demonstrates dynamic adjustment)
  - Per-client session data (message counter)

Press ENTER to stop...
```

### 2. Open HTML Client

Open `www/index.html` in a web browser:

```bash
# Windows
start www/index.html

# Or serve via HTTP (optional)
python -m http.server 8000
# Then open: http://localhost:8000/www/index.html
```

### 3. Test WebSocket Communication

**HTML Client:**
1. Click "Connect" button
2. Wait for connection confirmation
3. Type messages in the input box and press Enter or click "Send"
4. Observe echoed messages and periodic heartbeat messages

**Server Console:**
```
focText - Echoing 12 bytes from 127.0.0.1/1
[14:23:45] Sent periodic message to 127.0.0.1: [SERVER HEARTBEAT #1] Time: 14:23:45 | Interval: 2000 ms
focText - Echoing 5 bytes from 127.0.0.1/1
[14:23:47] Sent periodic message to 127.0.0.1: [SERVER HEARTBEAT #2] Time: 14:23:47 | Interval: 2000 ms
...
```

## Key Implementation Details

### Periodic Message System

Unlike the DMVCFramework version which uses per-client timers, mORMot2 requires a polling approach:

```pascal
// Main server loop
while true do
begin
  // Non-blocking keyboard check (100ms timeout)
  if ConsoleKeyPressed(100) then
    break;

  // Send periodic messages every 100ms check
  if MilliSecondsBetween(NowUtc, lastCheck) >= 100 then
  begin
    Protocol.SendPeriodicMessages;
    lastCheck := NowUtc;
  end;
end;
```

The `SendPeriodicMessages` method iterates through all connected clients and sends messages based on elapsed time since last message.

### Session Data Management

Session data is stored in `TWebSocketProcess.Protocol.ConnectionOpaque`:

```pascal
// Create session on connection
sessionData := TClientSessionData.Create;
Sender.Protocol.ConnectionOpaque := sessionData;

// Access during communication
sessionData := TClientSessionData(Sender.Protocol.ConnectionOpaque);
Inc(sessionData.FMessageCount);

// Free on disconnect
sessionData.Free;
Sender.Protocol.ConnectionOpaque := nil;
```

### Color Console Output

The server uses mORMot2's `TextColor()` for colorized console output:

```pascal
TextColor(ccLightMagenta);  // Frame type
write('focText - ');
TextColor(ccWhite);         // Message content
write('Echoing ', length(Frame.payload), ' bytes');
TextColor(ccCyan);          // Connection info
writeln(' from ', Sender.Protocol.RemoteIP);
```

## Testing Tips

### Test Scenarios

1. **Basic Echo Test**
   - Send: "Hello, mORMot2!"
   - Expect: "Echo: Hello, mORMot2!"

2. **Periodic Messages**
   - Connect and wait
   - Observe automatic heartbeat messages every 2 seconds (localhost)
   - After 5 messages, observe interval increasing

3. **Multiple Clients**
   - Open multiple browser tabs with the HTML client
   - Each receives independent periodic messages
   - Different intervals based on IP address

4. **Large Messages**
   - Send large text messages
   - Verify proper frame handling

### WebSocket Protocol Testing

```javascript
// JavaScript console testing
const ws = new WebSocket('ws://localhost:9091/', 'echo');
ws.onmessage = (e) => console.log('Received:', e.data);
ws.onopen = () => ws.send('Test message');
```

## Differences from DMVCFramework

### Architecture Differences

| Aspect | DMVCFramework | mORMot2 |
|--------|--------------|---------|
| **Event Model** | Callback-based (OnMessage, OnPeriodicMessage) | Override-based (ProcessIncomingFrame) |
| **Periodic Messages** | Built-in timer system per client | Custom polling in main loop |
| **Session Storage** | `AClient.Data` property | `Protocol.ConnectionOpaque` pointer |
| **Connection Events** | Dedicated OnClientConnect/OnDisconnect | Derived from frame opcodes (focContinuation/focConnectionClose) |
| **Sending Messages** | `AClient.SendText(msg)` | `Sender.SendFrame(TWebSocketFrame.Init(focText, msg))` |

### Implementation Notes

1. **No Built-in Periodic Timer**
   - mORMot2 doesn't have automatic periodic message support
   - Solution: Polling loop in main thread with manual interval tracking

2. **Connection Lifecycle**
   - DMVCFramework: Explicit OnClientConnect/OnDisconnect callbacks
   - mORMot2: Detect via frame opcodes (focContinuation = connect, focConnectionClose = disconnect)

3. **Thread Safety**
   - Access to `fConnections` list requires `fSafe.Lock/UnLock`
   - Session data access is single-threaded per connection

4. **Color Output**
   - DMVCFramework: Custom logging
   - mORMot2: Built-in `TextColor()` support

## Performance Characteristics

- **Latency**: Low-latency bidirectional messaging
- **Throughput**: Handles multiple concurrent clients efficiently
- **Memory**: ~200 bytes per connection for session data
- **CPU**: Minimal overhead, periodic checks run at 10 Hz (100ms intervals)

## Related mORMot2 Examples

- `/mnt/w/mORMot2/ex/rest-websockets/restws_simpleechoserver.dpr` - Basic echo server
- `/mnt/w/mORMot2/ex/rest-websockets/restws_chatserver.dpr` - Chat server example
- `/mnt/w/mORMot2/ex/rest-websockets/restws_longworkserver.dpr` - Long-running operations

## Documentation References

- **mORMot2 WebSocket**: `/mnt/w/mORMot2/src/net/mormot.net.ws.server.pas`
- **mORMot2 Net Guide**: `/mnt/w/mORMot2/src/net/CLAUDE.md`
- **DMVCFramework WebSocket**: `/mnt/w/DMVCframework/sources/MVCFramework.WebSocket.*`

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).

---

**Port Date**: 2024-12-20
**Original Sample**: DMVCFramework websocket_primer
**mORMot2 Version**: 2.x (latest)
