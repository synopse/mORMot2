# Server-Sent Events (SSE) Sample

Port of DMVCFramework's `serversentevents` sample to mORMot2.

## Overview

This sample demonstrates **Server-Sent Events (SSE)** implementation in mORMot2 - a technology for pushing real-time updates from server to browser over HTTP.

### What is SSE?

Server-Sent Events is a standard allowing servers to push data to web clients over HTTP. Unlike WebSockets (which are bidirectional), SSE is unidirectional (server→client only) and uses standard HTTP, making it simpler for many use cases.

**Key features**:
- ✅ Automatic reconnection with `Last-Event-ID` header
- ✅ Named events (e.g., `stockupdate`)
- ✅ Event IDs for tracking
- ✅ Works over standard HTTP (no special protocols)
- ✅ Built-in browser support via `EventSource` API

## Sample Application

This sample implements a **real-time stock ticker** that pushes price updates to the browser:

- **Backend**: mORMot2 HTTP server with custom SSE endpoint
- **Frontend**: HTML5 + JavaScript `EventSource` client
- **Data**: Random stock prices for IBM, AAPL, GOOG, MSFT
- **Update frequency**: Random delay between 500ms and 2.5 seconds

## File Structure

```
16-serversentevents/
├── src/
│   ├── server.pas       - HTTP server with SSE support
│   └── storage.pas      - Stock data generator
├── www/
│   └── index.html       - Web interface
├── SSESample.dpr        - Main program
├── SSESample.dproj      - Delphi project file
└── README.md            - This file
```

## Implementation Details

### SSE Event Format

mORMot2 sends events in the standard SSE format:

```
event: stockupdate
id: 123
data: {"stock":"AAPL","value":567.89}

```

(Note the blank line at the end - required by SSE spec)

### Server-Side (mORMot2)

The sample demonstrates:

1. **Custom HTTP server** (`TSSEHttpServer`) extending `TRestHttpServer`
2. **Request override** to handle custom endpoints:
   - `/root/stocks` - SSE stream endpoint
   - `/root/static/*` - Static file serving for HTML client
3. **SSE headers**:
   ```pascal
   'Content-Type: text/event-stream'
   'Cache-Control: no-cache'
   'Connection: keep-alive'
   ```
4. **Event ID support** for automatic reconnection

### Client-Side (JavaScript)

Browser uses the standard `EventSource` API:

```javascript
var source = new EventSource("/root/stocks");

source.addEventListener("stockupdate", function(e) {
    var data = JSON.parse(e.data);
    updateQuote(data.stock, data.value);
});
```

## Differences from DMVC Version

| Aspect | DMVCFramework | mORMot2 |
|--------|--------------|---------|
| **SSE Support** | Built-in `TMVCSSEController` | Custom implementation via `Request()` override |
| **URL Pattern** | `/stocks` | `/root/stocks` |
| **Static Files** | `TMVCStaticFilesMiddleware` | Custom file serving in `Request()` |
| **Event Generation** | `GetServerSentEvents()` method | `HandleSSE()` function |
| **Architecture** | Controller-based | HTTP server override |

### Key Challenges

**SSE is not built-in to mORMot2**, so this sample demonstrates:

1. ✅ Manual SSE event formatting (`event:`, `id:`, `data:` fields)
2. ✅ Correct HTTP headers for event streaming
3. ✅ `Last-Event-ID` header handling for reconnection
4. ✅ Custom `TRestHttpServer` descendant for routing

**Alternative**: For bidirectional real-time communication, consider mORMot2's **WebSockets** support (fully built-in).

## Building and Running

### Prerequisites

- Delphi 11+ or FPC 3.2+
- mORMot2 source code (included in this repository)

### Compile

```bash
# Windows
dcc32 SSESample.dpr

# Or use Delphi IDE to open SSESample.dproj
```

### Run

```bash
SSESample.exe
```

The server will start on **http://localhost:8080**

### Test

1. Open browser to: **http://localhost:8080/root/static/index.html**
2. You should see 4 stock tickers updating in real-time with color-coded changes:
   - **Green** = price increased
   - **Red** = price decreased
3. Click **"Toggle Updates"** to pause/resume the event stream

### Manual SSE Testing

You can test the SSE endpoint directly:

```bash
curl -N http://localhost:8080/root/stocks
```

Output:
```
event: stockupdate
id: 1
data: {"stock":"GOOG","value":612.34}

event: stockupdate
id: 2
data: {"stock":"IBM","value":545.67}

...
```

## Learning Points

### 1. SSE Event Format

Each event requires:
```pascal
'event: ' + eventName + #10 +
'id: ' + eventID + #10 +
'data: ' + jsonData + #10 +
#10  // Blank line terminates event
```

### 2. Required Headers

```pascal
'Content-Type: text/event-stream'
'Cache-Control: no-cache'
'Connection: keep-alive'
```

### 3. Browser Reconnection

Browsers automatically reconnect after 3 seconds if connection drops. The `Last-Event-ID` header tells the server which event was last received.

### 4. Named Events

Use `event: eventname` to allow client-side filtering:

```javascript
// Only listen to "stockupdate" events
source.addEventListener("stockupdate", handler);
```

## Extending This Sample

### Add More Event Types

```pascal
// Server sends different event types
sseEvent := 'event: alert'#10'data: Server restarting'#10#10;
sseEvent := 'event: news'#10'data: {"headline":"..."}'#10#10;

// Client listens to each
source.addEventListener("alert", alertHandler);
source.addEventListener("news", newsHandler);
```

### Implement Server-Side State

For real applications, maintain connection state:

```pascal
type
  TSSEConnection = record
    LastEventID: Integer;
    ConnectedSince: TDateTime;
  end;

var
  Connections: TSynDictionary; // Thread-safe
```

### Add Authentication

Combine with mORMot2 authentication:

```pascal
if Ctxt.Session = SESSION_USER_UNAUTHORIZED then
begin
  result := HTTP_UNAUTHORIZED;
  Exit;
end;
```

## Performance Notes

- **Scaling**: Each SSE connection holds an HTTP connection open. For thousands of concurrent clients, consider WebSockets or connection pooling.
- **Async Mode**: This sample uses `useHttpAsync` for better handling of long-lived connections.
- **Memory**: Each connection maintains minimal state (just event ID).

## Related Samples

- **15-middleware_staticfiles** - Static file serving pattern
- **WebSockets samples** - Bidirectional real-time communication (mORMot2 built-in)

## References

- **SSE Specification**: https://html.spec.whatwg.org/multipage/server-sent-events.html
- **MDN EventSource**: https://developer.mozilla.org/en-US/docs/Web/API/EventSource
- **mORMot2 HTTP Server**: See `/mnt/w/mORMot2/src/net/mormot.net.http.pas`

---

**Original DMVC Sample**: `/mnt/w/DMVCframework/samples/serversentevents/`

**Ported**: 2025-12-20
