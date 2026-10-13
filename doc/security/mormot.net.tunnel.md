# `mormot.net.tunnel` — End-to-End Tunnel Security and Deployment Guidance

**Scope:** mORMot2 local TCP forwarding through `ITunnelTransmit`, including `TTunnelLocalServer`, `TTunnelLocalClient`, the relay interfaces, the `tlmVersion2` handshake, and authenticated encrypted TCP payloads. The principal example is administrator-to-agent remote assistance carrying VNC/RFB through a relay.

**Implementation:** [`src/net/mormot.net.tunnel.pas`](../../src/net/mormot.net.tunnel.pas).  
**Cryptographic dependencies:** [`mormot.crypt.ecc256r1.pas`](../../src/crypt/mormot.crypt.ecc256r1.pas), [`mormot.crypt.core.pas`](../../src/crypt/mormot.crypt.core.pas), and `ICryptCert` in [`mormot.crypt.secure.pas`](../../src/crypt/mormot.crypt.secure.pas).  
**Related tests:** [`test/test.net.proto.pas`](../../test/test.net.proto.pas) (`TNetworkProtocols.Tunnel`, `TunnelTest`, `TunnelRelay`, `TunnelDeferred`) and [`test/test.core.ecc.pas`](../../test/test.core.ecc.pas).

**Review basis:** Inspection of the current trunk implementation, cryptographic construction, protocol invariants, threat boundaries, and existing tests. This is a source-level engineering security review: it is **not** a formal cryptographic proof, external certification, fuzzing report, or assertion that the complete application has been penetration-tested.

> **Executive assessment.** `tlmVersion2` provides a compact end-to-end TCP tunnel with optional P-256 ECDHE, role-separated AES-128-GCM traffic keys, 128-bit GCM authentication tags, per-frame evolving nonces, and optional endpoint certificate signatures. A sufficiently unpredictable, confidential `AppSecret` provides an alternative shared-secret authenticator for the fixed handshake, without certificates. The relay routes opaque traffic and does not need the established ECDHE traffic keys.
>
> **Essential condition.** Encryption and authentication are separate properties. Against an **active, malicious relay**, use `[toEcdhe]` **and** authenticate the endpoint with a separately trusted `VerifyCert` or a **strong, independently provisioned `AppSecret`**. ECDHE with an empty/known secret and no certificate can be intercepted by an active man-in-the-middle. The application remains responsible for authorization, endpoint selection, user consent, and credential provisioning.

## 1. Security goals and trust boundaries

A representative topology is:

```text
Administrator console                   Relay / transport                    End-user agent
VNC viewer --local TCP-- TTunnelLocalServer ===== TunnelSend frames ===== TTunnelLocalClient --local TCP-- VNC server
                         <-------------- P-256 ECDHE + AES-128-GCM -------------->
                            Endpoint authentication occurs at each endpoint
```

The endpoint cryptographic server/client roles can be reversed, and the relay transport need not be WebSockets. See section 3 for the distinction between administrator console, end-user agent, SOA service endpoints, and local TCP processes. `TTunnelLocal` protects the bytes carried between the two local forwarding endpoints; it does not provide access control for the local application protocol itself.

| Property | Intended protection | Trust boundary |
| --- | --- | --- |
| TCP payload confidentiality | AES-128-GCM when `toEcdhe` or `toEncrypt` is enabled | Requires an authenticated handshake against an active attacker |
| TCP payload integrity | 128-bit GCM tag checked before writing plaintext to the local socket | A relay may still drop, delay, reorder, or terminate frames |
| Endpoint authentication | Trusted peer certificate signature, and/or proof of a strong shared `AppSecret` | Trust anchors/secrets must be provisioned by the application |
| Forward secrecy | Fresh P-256 ECDHE key material for each tunnel | Does not apply to static `SpecificKey` reuse or PSK-only `toEncrypt` |
| Session association | Handshake session value and GCM additional authenticated data | A session number is a public routing identifier, not a credential |
| Diagnostic metadata | Optional AES-CTR obfuscation; certificate-signature coverage when enabled | `InfoNameValue` must not contain secrets or authorization assertions |
| Availability | Frame-size and pending-queue bounds limit local resource use | The relay can still prevent service or terminate a connection |

The expected production security model assumes uncompromised endpoints, trustworthy randomness, suitably protected long-term credentials, correctly implemented cryptographic primitives, and an appropriate application authorization flow. Neither the tunnel nor its cryptography protects a device already controlled by an attacker.

## 2. Components and API contract

| Component | Function | Security-relevant behavior |
| --- | --- | --- |
| `ITunnelTransmit.TunnelSend` | Carries binary frames between peers | Routing trailer contains a clear 32-bit `TTunnelSession` |
| `TTunnelRelay`, `TTunnelAgent`, `TTunnelConsole` | Maintain the shared agent SOA service and per-console session routing/callbacks | Do not require the end-to-end ECDHE private keys |
| `TTunnelLocalServer` / `TTunnelLocalClient` | Reciprocal end-to-end handshake | Implement role ordering and certificate-signing options |
| `TTunnelLocal.Open` | Bind/connect a local TCP socket and open the tunnel | Accepts `AppSecret`, `SignCert`, and `VerifyCert` |
| `TTunnelLocal.OpenSocket` | Transfer ownership of an already connected/accepted socket | The caller must validate its local socket peer |
| `TTunnelLocal.WaitForHandshake` | Wait for a queued peer handshake | Useful for demand-driven connections; does not consume the frame |
| `TTunnelLocalThread` | Forward TCP data | Owns directional GCM objects, IV state, and socket I/O |
| `Ecc256r1MakeKey` / `Ecc256r1SharedSecret` | Establish a P-256 shared secret | Native implementation validates peer points and private scalar ranges |

The defined tunnel options are `toEcdhe`, `toEncrypt`, `toClientSigned`, `toServerSigned`, and `toAcceptNonLocal`.

- `[toEcdhe]` enables P-256 ephemeral agreement and AES-GCM data frames. It is the preferred encrypted data-plane mode.
- `[toEncrypt]` enables AES-GCM traffic protection derived from `AppSecret` without requiring ECDHE. It does **not** give forward secrecy by itself; a nonempty `AppSecret` is required if `toEcdhe` is absent.
- `toClientSigned` and `toServerSigned` are resolved internally from the supplied `SignCert` / `VerifyCert`, not established merely because callers set these flags in `TransmitOptions`.
- `toAcceptNonLocal` permits non-loopback clients at the local listening socket. It should normally remain disabled for a local remote-desktop tunnel.

Both endpoints must agree on the session number, effective options, and `AppSecret`, and must use consistent certificates if signatures are enabled. The two handshake methods exchange frames reciprocally; one side may need to start in a separate thread. `OpenSocket` transfers the socket to the tunnel on a valid call.

### 2.1 Options in practice

`TTunnelOptions` controls the data cipher, signing flags and local listening policy. It does **not** itself authenticate a relay connection or approve access to a VNC server.

| Requested option | Actual effect | Practical guidance |
| --- | --- | --- |
| `toEcdhe` | Fresh P-256 ECDH key agreement (unless a `SpecificKey` is supplied), followed by AES-128-GCM forwarding | Preferred mode for remote assistance; combine with a strong `AppSecret` and/or independently trusted peer certificates |
| `toEncrypt` without `toEcdhe` | AES-128-GCM with keys based on `AppSecret`, without ECDHE forward secrecy | Use only where a strong, confidential PSK is available; the code rejects an empty `AppSecret` in this mode |
| Neither encryption option | Forwarded TCP bytes remain plaintext | Suitable for debugging and explicitly trusted local transport, not confidential VNC traffic over a relay |
| `toClientSigned`, `toServerSigned` | Describe which endpoint's handshake is digitally signed | Do not set these by hand to request signatures; `OpenInternal()` recomputes them from `SignCert` and `VerifyCert` |
| `toAcceptNonLocal` | Allows non-loopback connections to the listener bound by `Open()` | Usually leave unset; the VNC viewer should connect on the local machine |

Both `[toEcdhe]` and `[toEncrypt]` enable data encryption (`toEncrypted` is a helper set containing those options); `toEcdhe` is preferable when forward secrecy matters. If both are supplied, the ECDHE branch is used for the shared secret. Signing can also be requested independently of encryption, but a **signed plaintext tunnel is not confidential**.

The actual certificate-signing direction is determined by the endpoint class:

| End-to-end role | `SignCert` supplied | `VerifyCert` supplied |
| --- | --- | --- |
| `TTunnelLocalServer` | Sets `toServerSigned` | Sets `toClientSigned` |
| `TTunnelLocalClient` | Sets `toClientSigned` | Sets `toServerSigned` |

For mutual certificate authentication, supply *both* certificates on *both* sides: each endpoint signs with its own `SignCert` and verifies with the expected peer's `VerifyCert`. With a shared-secret-only design, both may be `nil`, provided the same independently provisioned strong `AppSecret` is used on each side. Sign-only and encrypt-only are supported, but give different guarantees.

### 2.2 `Open()`, `OpenSocket()` and their lifetimes

- `Open(Sess, Transmit, TransmitOptions, TimeOutMS, AppSecret, Address, InfoNameValue, SignCert, VerifyCert)` creates a local listening socket on an ephemeral port if `Address` has no port, or connects to the given TCP `Address:port` otherwise. On handled handshake/setup failure it returns zero; invalid preconditions may raise `ETunnel`. Invoke only once per `TTunnelLocal` instance.
- `OpenSocket(Sess, Transmit, TransmitOptions, TimeOutMS, AppSecret, Socket, LocalPort, InfoNameValue, SignCert, VerifyCert)` adopts an already connected/accepted socket. The application remains responsible for deciding which local peer and backend service may use that socket. A valid call transfers socket ownership to the tunnel.
- `WaitForHandshake(Sess, TimeOutMS)` waits for an actual incoming handshake frame without removing it from the queue. It is useful when an agent should **not** connect to its VNC backend until an administrator's VNC viewer has connected. Only one waiter should use an instance; at least one peer must actively begin its `Open`/`OpenSocket` handshake.
- `ClosePort()` terminates forwarding, shuts down sockets and notifies the other side; releasing an SOA callback/interface also leads to session cleanup. Each `TTunnelLocal` manages **one TCP byte stream**. Use another session and `TTunnelLocal` instance for each additional viewer connection.

`TimeOutMS` applies to the end-to-end handshake wait and also influences the local accept timeout for the background forwarding thread. Since both sides exchange handshakes reciprocally, normally start one side in a worker thread before opening the other. In demand-driven mode, the handshake event replaces periodic polling.

## 3. Console, agent, relay, and VNC session architecture

The terms **console** and **agent** describe *application roles*, while **client** and **server** in `TTunnelLocalClient` / `TTunnelLocalServer` describe *opposite cryptographic-handshake roles*. They should not be confused with TCP client/server roles, the WebSockets client/server, or the physical host running the VNC server.

### 3.1 What "console" and "agent" mean

For remote VNC assistance:

- **Console**: the administrator-side application. It manages the administrator's connection to the relay and normally exposes a loopback TCP port for a conventional **VNC viewer**. It typically creates one `TTunnelLocalServer` per forwarded VNC TCP stream.
- **Agent**: the application running on the end user's device, behind a NAT/firewall in many deployments. It maintains its own relay connection and normally connects a `TTunnelLocalClient` to the **local VNC server** on that machine (e.g. an approved `127.0.0.1:5900` endpoint). It need not accept an incoming Internet connection.
- **Relay**: the central process that associates console-side and agent-side callbacks by `TTunnelSession` and forwards binary `ITunnelTransmit.TunnelSend()` frames. It need not terminate or decrypt the established end-to-end AES-GCM stream.
- **Viewer** and **VNC server**: ordinary local TCP applications that exchange RFB bytes. Neither needs to understand mORMot, WebSockets, the end-to-end handshake or the relay protocol.

```text
ADMINISTRATOR DEVICE                RELAY SERVER                   END-USER DEVICE

VNC viewer                          TTunnelRelay                   VNC server
   | TCP localhost                      |                             ^ TCP localhost
   v                                    |                             |
TTunnelLocalServer -- ITunnelConsole === ITunnelAgent -- TTunnelLocalClient
      [session S]                        |                 [session S]
       <----------- ECDHE + AES-128-GCM payload ------------>
           (relay routes frames by S; no plaintext keys)
```

The relay implements **one shared `TTunnelAgent` service endpoint** for all agent-associated sessions, plus **one `TTunnelConsole` service endpoint per resolved console SOA session**. Thus, an `ITunnelAgent` reference on the relay is a *multiplexing service*, not an object representing one specific end-user PC. Similarly, one console service endpoint may own several tunnel sessions (several viewers, tabs, or devices). The application must maintain its own mapping from authenticated agent/device identity to routing requests.

The code conventionally pairs `TTunnelLocalServer` with the console and `TTunnelLocalClient` with the agent, but the classes do **not** mandate which machine opens a TCP listener. What matters is that the two ends use opposite handshake roles for correct certificate flags and client/server nonce ordering.

### 3.2 Which interfaces are involved?

| Interface / class | Where it is implemented or used | Main responsibility |
| --- | --- | --- |
| `ITunnelTransmit` | Implemented by each `TTunnelLocal` callback and by relay SOA endpoints | `TunnelSend(Frame)` transports binary frames; `TunnelInfo()` describes local state or a multiplexed console's sessions |
| `ITunnelLocal` | Implemented by `TTunnelLocalClient` / `TTunnelLocalServer` | Extends `ITunnelTransmit` with `LocalPort()`, `RemotePort()`, `Encrypted()` and callback-release handling |
| `ITunnelOpen` | Parent SOA interface of `ITunnelAgent` and `ITunnelConsole` | `TunnelPrepare()`, `TunnelAccept()`, `TunnelCommit()` and `TunnelRollback()` manage the two-stage session lifecycle |
| `ITunnelConsole` | Implemented on relay by `TTunnelConsole` | Console-side relay transport, session routing, per-console `TunnelInfo()`, plus `TunnelSetInfo()` metadata |
| `ITunnelAgent` | Implemented on relay by `TTunnelAgent` | Agent-side relay transport, session routing and shared entry point for multiple agent-associated sessions |
| `TTunnelRelay` | Main relay/server application | Owns the shared agent endpoint and console registry; `AgentsInfo()` and `ConsolesInfo()` expose server-side session snapshots |
| `TTunnelList` / `ITunnelOpenState` | Relay internals | Thread-safe mapping between `TTunnelSession`, callbacks, pending state and session-information snapshots |

`ITunnelTransmit.TunnelSend()` receives a `RawByteString` binary frame whose **last four bytes** contain the session ID. The relay looks up the corresponding registered callback and forwards the frame without requiring a payload decryption key. With WebSockets/SOA, it can carry this binary parameter without application-side base64 encoding.

The `TTunnelSession` is a **32-bit routing identifier**, typically generated when a session is prepared. It must match on both sides but is **not** a password, an authenticated user identity, or a substitute for `AppSecret`. **One session corresponds to one TCP byte stream**, not to an entire user desktop or a persistent console-agent relationship.

### 3.3 Preparing, accepting, committing and rolling back a session

Both `ITunnelAgent` and `ITunnelConsole` implement `ITunnelOpen`, so **either side** may initiate a new tunnel. The caller passes its local `TTunnelLocal` callback to `TunnelPrepare()`; the other side registers its own callback using the returned session ID and `TunnelAccept()`. For example, with an agent-initiated session:

```pascal
// At the agent application:
AgentLocal := TTunnelLocalClient.Create(TSynLog);
AgentCallback := AgentLocal; // ITunnelTransmit, retain for session lifetime
Sess := AgentService.TunnelPrepare(AgentCallback);

// At the console application, after receiving the session ID by its
// authorized application-specific coordination channel:
ConsoleLocal := TTunnelLocalServer.Create(TSynLog);
ConsoleCallback := ConsoleLocal; // ITunnelTransmit
Accepted := ConsoleService.TunnelAccept(Sess, ConsoleCallback);
```

This is *illustrative wiring*, not a complete application: handle zero/false results, object ownership, credential provisioning and exceptions. The application may equally call `ConsoleService.TunnelPrepare()` followed by `AgentService.TunnelAccept()`.

After registration:

1. Open the two local forwarders with the **same** `Sess`, matching effective `TTunnelOptions`, matching `AppSecret`, and reciprocal certificates if enabled. The console typically binds a loopback viewer port; the agent typically connects to its loopback VNC backend. Start one `Open()` in a separate worker thread because both sides send and wait for a handshake.
2. **On success**, call `TunnelCommit(Sess)` on **both** relay-side service interfaces. These methods mark each side's previously pending session as committed; they do not perform the cryptographic handshake themselves.
3. **On failure**, call `TunnelRollback(Sess)` for each still-pending side, and release or close the associated local forwarder. A failed/expired session should not be reused for an unrelated viewer connection.
4. On normal closure, `ClosePort()` sends the four-byte routing-only close frame and the relay removes the corresponding registered callbacks. Releasing a console SOA interface also cleans up its registered sessions.

**Important for VNC:** pending sessions are already routable *before* `TunnelCommit()`, because the handshake must cross the relay while the session is pending. A VNC server often sends its `RFB 003.008` greeting immediately after the local backend connects. The implementation allows this server-first data to reach the viewer even before both sides have committed the session. Delaying all frames until commit would break or deadlock this flow.

### 3.4 Demand-driven VNC forwarding (`OpenSocket` and `WaitForHandshake`)

Demand-driven startup avoids connecting to a VNC backend when no viewer is present. The supported sequence is:

1. Create the `TTunnelLocalServer` and `TTunnelLocalClient` callbacks, then call `TunnelPrepare()` on either relay endpoint and `TunnelAccept()` on the other. The application should already know which administrator/device pair has been authorized.
2. On the console, listen for a **real VNC viewer** on a loopback TCP port using application-owned socket code. On the agent, call `AgentLocal.WaitForHandshake(Sess, TimeoutMS)` in a worker; it does not consume the first incoming handshake.
3. Once a viewer connects, the console accepts its socket and calls `ConsoleLocal.OpenSocket(...)`. This transfers the connected socket to the tunnel and sends the first end-to-end handshake frame.
4. The agent's waiter wakes on this frame, connects to the approved local VNC backend, then calls `AgentLocal.Open(Sess, AgentService, [toEcdhe], ..., '127.0.0.1:5900', ...)` or `OpenSocket()` if the application connected the backend itself. The peer's initial handshake remains queued for `Open()` to consume.
5. After successful reciprocal handshakes, commit both sides. An early VNC/RFB banner may already have traversed the pending session. If either side fails, roll back that session and close the sockets.

The exact socket ownership and `OpenSocket()` signature, including its `var Socket` parameter, are shown in the unit declaration and the [`TunnelDeferred`](../../test/test.net.proto.pas) regression example. Unlike artificial sleeps, frame counters or polling loops, `WaitForHandshake()` provides explicit synchronization with the first actual peer handshake.

### 3.5 Session information: one tunnel vs. one console vs. the relay

There are **three distinct scopes** of session information. They should not be conflated:

| API | Return value | Intended use |
| --- | --- | --- |
| `TTunnelLocal.TunnelInfo()` (through `ITunnelTransmit`) | A `TDocVariant` **object** describing one established forwarding stream, or `null` when no local port exists | Per-stream diagnostics and counters |
| `ITunnelConsole.TunnelInfo()` | A `TDocVariant` **array** containing the current session information for *that console endpoint*, or `null` if none | Console application inspects its own sessions |
| `ITunnelAgent.TunnelInfo()` | `null` in the current shared relay service implementation | This method does **not** enumerate all agents; use `TTunnelRelay.AgentsInfo()` on the server |
| `TTunnelRelay.AgentsInfo()` | `TVariantDynArray` of per-session `TunnelInfo()` records from the **agent-side** registered callbacks | Relay-side overview of agent-associated streams |
| `TTunnelRelay.ConsolesInfo()` | `TVariantDynArray`, one object per console service endpoint, with configured console metadata, `count` and `list` | Relay-side overview of all consoles and their streams |

A local `TTunnelLocal.TunnelInfo()` object contains the remote `InfoNameValue` fields received in the handshake and the following standard fields after startup:

| Field | Meaning |
| --- | --- |
| `session` | The 32-bit `TTunnelSession` routing ID |
| `remotePort`, `localPort` | Port metadata exchanged/established during startup |
| `started`, `options`, `encrypted` | Start timestamp, resolved options and whether a data cipher is installed |
| `elapsed` | Approximate number of elapsed seconds |
| `bytesIn`, `bytesOut` | Counts of forwarded plaintext bytes in each direction |
| `framesIn`, `framesOut` | Frame counters for the local forwarder |
| Additional fields | Non-sensitive `InfoNameValue` metadata from the peer, or application fields added by overriding `OnTunnelInfo()` |

For example, from a console's relay proxy:

```pascal
var
  Info: variant;
begin
  Info := ConsoleService.TunnelInfo;
  // Info is a TDocVariant array of this console's registered sessions,
  // or null when it has no sessions. Iterate the array with TDocVariant APIs.
end;
```

`ITunnelConsole.TunnelSetInfo()` separately lets the console attach **console-level metadata** (e.g. a display name, operator label or application version) stored by the relay. This is **not** the same as the diagnostic `InfoNameValue` exchanged by individual local tunnels, and it is not returned as part of `ITunnelConsole.TunnelInfo()` in the current implementation. It is included in the server's `TTunnelRelay.ConsolesInfo()` view:

```pascal
var
  ConsoleInfo: variant; // application-created TDocVariant object
begin
  // Populate ConsoleInfo with NON-SENSITIVE labels before this call.
  ConsoleService.TunnelSetInfo(ConsoleInfo);
end;
```

On the relay server, the two summaries are obtained directly from `TTunnelRelay`:

```pascal
var
  AgentStreams: TVariantDynArray;
  Consoles: TVariantDynArray;
begin
  AgentStreams := Relay.AgentsInfo;
  Consoles := Relay.ConsolesInfo;
end;
```

Each item in `ConsolesInfo()` has the shape below (illustrative values, **not** an exact serialized response):

```json
{
  "name": "Support console A",
  "count": 2,
  "list": [
    { "session": 123456789, "encrypted": true, "remotePort": 5900,
      "localPort": 51912, "bytesIn": 10240, "bytesOut": 20480 },
    { "session": 234567890, "encrypted": true, "remotePort": 5900,
      "localPort": 51913, "bytesIn": 4096, "bytesOut": 8192 }
  ]
}
```

Here `name` is merely an example of a console-supplied `TunnelSetInfo()` field; the implementation adds `count` and `list`. The `list` values come from the console-side local forwarding callbacks. The method uses **per-second caching** for aggregate session information. `ITunnelConsole.TunnelInfo()` is limited to the caller's own console sessions; the broader `TTunnelRelay.ConsolesInfo()` view is a **server-side API**, not automatically exposed to every remote console. An application wishing to publish an operator dashboard must provide its own authenticated, authorized management interface. If no sessions exist, `list` may be `null` instead of an empty array.

**"Active sessions" qualification:** the underlying `TTunnelList` registers **pending** sessions when they are prepared/accepted, before `TunnelCommit()`. Therefore `count` means the number of currently **registered** sessions (pending **or** committed), not a precise count of TCP streams that are already handshaken and forwarding data. Similarly, a pending callback may yield `null` `TunnelInfo()` until its local socket opens. Applications needing a strict "connected and processing" status should maintain that state explicitly and should not infer it solely from a relay `count` value.

These information methods are designed for dashboards and diagnostics, **not** as an authorization database. `InfoNameValue` is non-sensitive and non-authoritative; console-supplied metadata is also application-provided and should not be assumed trusted merely because the relay stores it. The actual peer trust comes from certificates or `AppSecret`, not from a `session`, `name`, or `host` field in a variant object.

### 3.6 Hosting the relay and SOA service lifetimes

`TTunnelRelay` is a `TInterfaceResolver`. It owns one shared `TTunnelAgent` endpoint (`Agent` property) and creates `TTunnelConsole` endpoints when `ITunnelConsole` is resolved. A conventional mORMot SOA/WebSockets registration is:

```pascal
Relay := TTunnelRelay.Create(TSynLog, 120); // pending-session timeout, seconds
RestServer.ServiceDefine(Relay.Agent, [ITunnelAgent]);
RestServer.ServiceContainer.InjectResolver([Relay]);
RestServer.ServiceDefine(TTunnelConsole, [ITunnelConsole], sicPerSession);
```

The `ITunnelAgent` endpoint is shared (`sicShared`); each console service is created with `sicPerSession` and can multiplex multiple `TTunnelSession` values. The relay tracks each console's callback state and `TunnelSetInfo()` metadata separately. Closing an endpoint's SOA session cleans up its associated forwarding sessions. At the transport boundary, account authentication, SOA permissions, quotas and device selection remain requirements of the hosting application: **SOA login is not a replacement for end-to-end peer authentication** when relay compromise is in scope.

The regression suite demonstrates direct interface callbacks, WebSockets/SOA registration, multiple concurrent agent-associated sessions, several console endpoints, `AgentsInfo()`, `ConsolesInfo()`, two-way session preparation, and the server-first VNC/RFB banner case. See [`TNetworkProtocols.TunnelRelay`](../../test/test.net.proto.pas) and [`TunnelDeferred`](../../test/test.net.proto.pas).


## 4. Authentication modes and recommendations

**Recommended data-plane option:** `TransmitOptions := [toEcdhe]`, with a deliberate authentication choice. Other modes are useful for compatibility, testing, or already trusted local wiring.

| Effective configuration | Confidentiality | Endpoint authentication | Appropriate use |
| --- | --- | --- | --- |
| No encryption, no certificates | None for forwarded bytes | None unless a private `AppSecret` authenticates the fixed handshake | Trusted local tests only |
| `[toEncrypt]` + strong `AppSecret` | AES-GCM | Shared-secret possession | PSK-only environments that do not require forward secrecy |
| `[toEcdhe]`, empty `AppSecret`, no certificates | AES-GCM against passive observers | **None against an active MITM** | Test/diagnostic use; not a hostile-relay guarantee |
| `[toEcdhe]` + strong private `AppSecret`, no certificates | AES-GCM, with ephemeral agreement | Shared-secret possession | Certificate-free, independently provisioned PSK pairing |
| `[toEcdhe]` + correctly pinned peer certificates | AES-GCM, with ephemeral agreement | Expected signing-key holder at each verifying endpoint | Endpoint-specific authenticated tunnel |
| `[toEcdhe]` + pinned certificates + strong `AppSecret` | AES-GCM, with ephemeral agreement | Both independently managed authenticators | Defense in depth when both credentials can be provisioned securely |

A certificate is **not mandatory** for authentication if `AppSecret` has sufficient entropy, remains confidential, and is known only to the intended trust domain. Conversely, a nonempty `AppSecret` is **not mandatory** when correctly pinned peer certificates authenticate the handshake and ECDHE protects the stream. The best choice depends on available key management.

`ITunnelLocal.Encrypted = true` indicates an active traffic cipher; **it does not assert that the remote party was authenticated or authorized**.

### 4.1 Certificate-based authentication

`SignCert` is the local signing identity, normally holding the private key. `VerifyCert` identifies the **expected remote signer**. Their corresponding keys/certificates should match on the opposite endpoint, and the intended usage includes `[cuDigitalSignature]`.

For **mutual** certificate authentication, both endpoints need a local `SignCert` and a separately trusted `VerifyCert` for the other endpoint. With only one verifying side, only that side has signature-based assurance about its peer. Signature flags are incorporated in the common handshake configuration and checked for consistency.

The critical operation is not simply obtaining *a* certificate: the application must already trust the *correct* certificate/public key for the requested console, agent, user, or device. It should provision or pin that identity independently of any relay it intends to distrust. A certificate or public key supplied unverified by a compromised relay is not a trustworthy endpoint identity. Certificate validity policy, key rotation, revocation, and identity-to-device binding are application responsibilities; `FrameVerify()` checks the signature using the supplied `ICryptCert`, not an independent deployment PKI policy.

Certificate authentication covers the **fixed handshake and the JSON ciphertext**. In the wire layout, `FrameSign()` operates after appending the optional JSON payload, and `FrameVerify()` receives a signed payload length of `SizeOf(TTunnelLocalHandshake) + JsonLength`. The signature is verified **before** the JSON is decrypted and parsed. The trailing JSON length and session value are parsed/checked as framing fields; they are not independently signed trailer bytes.

### 4.2 Strong `AppSecret`: certificate-free authentication

`TunnelHandshakeCrc()` computes a 128-bit SHAKE128-based check over a domain separator, `AppSecret` (or a documented public fallback for an empty secret), and the fixed handshake fields preceding the checksum. The receiver reconstructs and checks that value. These fields include the ephemeral public key, random handshake material, session, options, and relevant common context.

With a private high-entropy `AppSecret`, the resulting handshake provides a practical **proof of possession of the PSK**, binding the fixed handshake and its ECDHE public key to that shared secret. It is mutual when each side knows the expected secret and checks its peer's checksum. It is not a certificate-like proof of a unique device unless that secret was assigned exclusively to the intended device/pair.

`AppSecret` best practices:

1. Generate it using a cryptographically secure random-number generator with **at least 128 bits of entropy**; 256 bits is also reasonable. A 16-byte random value can be represented by 32 hexadecimal characters if a printable `RawUtf8` value is convenient.
2. **Provision it securely and independently of an untrusted relay.** Never assume a public session ID, machine identifier, timestamp, or relay-provided string is an authentication secret.
3. Prefer a different secret per endpoint pair or defined trust group. A single secret distributed to every installed client lets any holder impersonate another holder and makes compromise of one installation a group-wide compromise.
4. A strong secret **may remain constant across sessions** if appropriately stored and protected. Fresh per-session secrets reduce cross-session and revocation exposure, but transience is not necessary for cryptographic entropy.
5. Rotate or revoke a secret if an endpoint or distribution channel is compromised. Do not log it or bake a universal recoverable secret into publicly distributed binaries.
6. Do **not** use a short PIN, ordinary password, or human-memorable pairing code as the sole credential. The visible handshake admits **offline candidate testing**; this construction is not a PAKE. If short codes are required, use a vetted PAKE or independently authenticated pairing mechanism.

The checksum is a custom **secret-prefixed SHAKE128 construction**, not KMAC, HMAC, or a formally analyzed password-authenticated key exchange. With a strong PSK the source provides a useful authentication mechanism, but the broader handshake should not be presented as a formally proven AKE protocol.

An **empty** `AppSecret` invokes public fallback constants for the checksum and metadata obfuscation. Those constants are implementation defaults, **not confidential credentials**. `[toEcdhe]` plus empty `AppSecret` remains safe against an *active* relay only if trusted certificate verification supplies peer authentication.

## 5. `tlmVersion2` handshake and framing

The two peers each send one handshake frame and receive the other. The fixed `TTunnelLocalHandshake` consists of an ECDHE frame (`rnd` and compressed P-256 public key) and a packed `TTunnelLocalInfo` (magic, options, session, local port, checksum).

The current wire layout is:

```text
[TTunnelLocalHandshake][JSON or AES-CTR-obfuscated JSON][optional signature][JSON length: u16][session: u32]
<--------------------- bytes signed by SignCert --------------------->
```

The certificate-signed bytes are **only** the fixed handshake plus JSON (or its ciphertext); the signature is appended after those bytes. The JSON length and session trailer remain outside the signature. With no `SignCert`, the signature field is absent.

The main sequence is:

1. Resolve the effective options from the call and available certificates. Construct the `tlmVersion2` header, a 128-bit random nonce, and a fresh compressed P-256 public key if `toEcdhe` is requested.
2. Compute `TunnelHandshakeCrc()` over the fixed handshake using `AppSecret` (or the documented fallback when empty). This checksum is not an X.509 signature.
3. Encode `InfoNameValue` as JSON. When traffic encryption is enabled, apply the metadata-specific AES-CTR obfuscation using an HMAC-SHA256 derivation with an `'Info'` domain label and common handshake information.
4. Append the JSON or its ciphertext to the fixed handshake. If `SignCert` is present, digitally sign **the entire fixed handshake plus JSON portion**. Append the 16-bit JSON length and 32-bit session trailer, then transmit.
5. On receipt, reject frames that are too short, belong to the wrong session, or advertise a JSON length extending beyond the available payload. Locate the signature after the JSON using the trailer length.
6. If `VerifyCert` is supplied, verify the signature over the fixed handshake **and JSON ciphertext**. Then check the expected common header, verify the peer's `AppSecret`-based checksum, and reject identical directional checksum values.
7. Extract, optionally deobfuscate, and parse the JSON only **after** those fixed-handshake checks. No caller should treat this diagnostic metadata as an authorization source.
8. For `toEcdhe`, validate the peer P-256 key and compute the shared secret. Feed it into the pre-initialized HMAC-SHA256 key schedule, along with role-ordered handshake material. Derive directional AES-GCM keys and an initial IV; start the forwarding thread.

The protocol compares a common fixed-header prefix that excludes the endpoint-specific local port and checksum. Both sides must agree on the version, options, session, and other common header context; the opposite endpoint's port and checksum are intentionally different.

`tlmVersion2` is the emitted wire version. A legacy enum entry does not imply automatic fallback to an older cipher or handshake. Wire interoperability requires peers implementing the **same full version-2 framing rules**, including signature placement; a version label alone does not ensure compatibility between distinct wire layouts.

## 6. P-256 ECDHE, public-key validation, and forward secrecy

ECDHE uses the NIST P-256 / secp256r1 group, giving approximately **128-bit classical security**. `Ecc256r1MakeKey()` supplies the key pair by default when no `SpecificKey` was provided to `TTunnelLocal.Create()`. The local private scalar is not sent to the relay.

The native Pascal ECC implementation validates incoming public keys before relying on them for ECDH:

- compressed-point prefix must be `$02` or `$03`;
- X must be a canonical field element (`x < p`);
- the candidate Y must actually solve the curve equation, rejecting nonsquare right-hand sides;
- both affine coordinates must be in the field and satisfy `y^2 = x^3 - 3x + b (mod p)`;
- the local private scalar must be in `[1, n-1]` before scalar multiplication;
- invalid inputs return failure with the derived secret cleared.

P-256 has cofactor 1; a valid non-infinity point on the curve is in the intended prime-order group. These checks defend the native routine against malformed and off-curve peer input. The OpenSSL-backed ECC implementation has its own key-decoding and validation path and should also be covered by backend-specific tests.

The traffic-key derivation uses `THmacSha256`, incorporating the `AppSecret` context, client/server-ordered ephemeral information, and the ECDH secret when enabled. Labeled derivations with the local and remote checksum values produce separate 128-bit **send** and **receive** AES-GCM keys. The initial IV is also derived from this context. Each peer's send key is the other's receive key, and matching directional keys within one peer are rejected. Private and temporary key buffers are cleared along the handshake/teardown paths.

**Forward secrecy** requires a *fresh* ECDHE private scalar for each session and eventual erasure of that scalar. Under these conditions, later disclosure of a long-lived `AppSecret` or certificate signing key alone is not sufficient to recover previously recorded GCM traffic keys. It does **not** protect traffic if the endpoint was compromised during a session. An intentionally supplied, repeatedly reused `SpecificKey` weakens this guarantee; avoid it for remote assistance.

This is an application-specific, HMAC-based key schedule, not TLS, Noise, or a claimed implementation of RFC 5869 HKDF extract-and-expand. The protocol's conditional security argument relies on its specific transcript checks, correct ECDH implementation, key separation, correct randomness, and authenticators trusted outside the relay.

## 7. AES-128-GCM protection of forwarded TCP frames

For encrypted traffic, the wire frame is:

```text
[ciphertext of N TCP bytes][16-byte GCM authentication tag][4-byte session ID]
```

`TTunnelLocalThread.Create()` uses separate `TAesFast[mGcm]` objects for receiving and sending. Their keys are independently derived. The local `TTunnelSession` is passed as **GCM additional authenticated data (AAD)**, binding ciphertext frames to the session even though the routing trailer is transmitted in cleartext.

Each direction maintains its own evolving GCM IV. `GCM_IncIV()` advances the nonce for each send or receive operation, and the record does not carry a redundant explicit nonce. This keeps the record overhead at **20 bytes** (16-byte tag plus 4-byte routing trailer), with the tradeoff that the transport must preserve **frame order and frame boundaries** for each direction.

On receipt, `DecryptGcm()` calculates the expected authentication tag. On failure, the temporary plaintext result is wiped and cleared; `OnReceived()` detects the empty result, terminates the tunnel, and does **not** forward unverified plaintext to the local TCP socket. Altered, replayed, lost, or reordered frames may therefore disconnect the session rather than be transparently recovered.

AES-GCM's fundamental requirement is **no repetition of a key/nonce pair**. Fresh ECDHE sessions, directional keys, and evolving IVs are designed to preserve this invariant during normal sequential forwarding. Do not clone live tunnel state, reuse ephemeral secrets as static `SpecificKey` values, or introduce out-of-order processing/retransmission of encrypted records without revisiting nonce handling. There is no explicit sequence-number-based replay window or rekey protocol.

In the current implementation `TransmitSock.RecvWait()` receives up to 64 KiB per TCP read. The receiver enforces a **72 KiB** maximum incoming tunnel frame size, leaving room for the GCM tag, session trailer, and normal handshake structures.

## 8. Diagnostic JSON: obfuscation, not a secret channel

`InfoNameValue` carries convenience metadata such as host names and diagnostic information that may appear in `ITunnelTransmit.TunnelInfo()`. **It must not include secrets or security-critical assertions.** In particular, do not rely on it to authorize a user, select a privileged VNC target, convey a credential, or confirm an endpoint identity.

If `toEcdhe` or `toEncrypt` is enabled, JSON is obfuscated using AES-CTR with an HMAC-SHA256-derived key/IV. The derivation has a dedicated `'Info'` label and includes common handshake context. Without traffic encryption the JSON is sent in clear. JSON obfuscation is deliberately **not presented as authenticated encryption or sensitive-data confidentiality**.

Both peers use matching obfuscation material, so the CTR keystream is not direction-separated for JSON; a passive observer with two ciphertexts can obtain their plaintext XOR. With an empty `AppSecret`, public fallback derivation values prevent meaningful confidentiality claims for this metadata. This is acceptable under the explicit **non-sensitive diagnostics-only** policy, but would not be suitable for authentication tokens or private information.

When `SignCert` / `VerifyCert` are configured, the certificate signature **does cover the JSON ciphertext**. Without `VerifyCert`, `FrameVerify()` does not check a certificate signature; the `AppSecret`-based checksum protects the **fixed handshake**, not the JSON. The receiver still checks fixed-handshake authentication before parsing the metadata, but the JSON itself must be regarded as *untrusted diagnostics* in certificate-free mode. A relay can tamper with it or cause a parsing failure; this does not imply it can forge AES-GCM-protected application bytes after a correctly authenticated key exchange.

## 9. Malicious-relay and credential-compromise scenarios

Assume an active relay can inspect, replace, reorder, replay, drop, or forge transport frames, but cannot steal endpoint private keys or obtain a strong PSK distributed through a different trusted channel.

| Scenario | Expected outcome / qualification |
| --- | --- |
| Passive recording of `[toEcdhe]` GCM traffic | Ciphertext does not reveal the forwarded TCP plaintext under normal cryptographic assumptions |
| Active relay substitutes ECDHE keys with no `VerifyCert` and empty/known `AppSecret` | **MITM is possible**: confidentiality against passive recording is not peer authentication |
| Active relay substitutes ECDHE keys where the endpoint has a correctly pinned `VerifyCert` | Missing signing key prevents a valid substituted handshake |
| Active relay substitutes ECDHE keys when both endpoints use a strong PSK unknown to the relay | Forging the fixed-handshake checksum should be computationally infeasible; authentication is of PSK possession |
| Short/guessable `AppSecret` | Offline dictionary attack on recorded handshake is possible; not a PAKE |
| Relay provides a replacement certificate or secret | Authentication fails as an identity guarantee if the application uncritically trusts the relay-provided credential |
| Relay modifies established GCM ciphertext or tag | Authentication failure; no unauthenticated payload should be forwarded |
| Relay drops, reorders, or replays GCM records | Session desynchronization / termination may follow; no availability guarantee |
| Relay sends an unauthenticated four-byte close frame | Can terminate the tunnel; closure is a control signal, not a proof of peer intent |
| Relay changes diagnostic JSON without a verifying certificate | JSON can be modified; it is explicitly non-authoritative and non-sensitive |
| An endpoint is compromised | Endpoint plaintext, stored credentials, or live keys may be exposed; E2E protection cannot fix this |

The strongest justified statement is **conditional**: end-to-end VNC byte confidentiality and integrity can survive relay compromise **when** the ECDHE handshake is authenticated independently of that relay, the local endpoints remain trustworthy, and the cryptographic algorithms and randomness behave correctly. Availability, local-device authorization, and metadata privacy are separate properties.

## 10. Bounds, lifetime, and local network exposure

`TTunnelLocal.TunnelSend()` applies defensive admission checks before queuing or decrypting frames:

```pascal
MAX_TUNNEL_FRAME_BYTES = 72 shl 10;
MAX_TUNNEL_PENDING_FRAMES = 128;
```

The first rejects frames over 72 KiB. The second bounds the number of frames queued during handshaking, accommodating early forwarding before the opposite side has finished starting. These are modest local DoS/malformed-input controls, **not** a promise to remain available under arbitrary relay flooding; transport buffers and other application/session quotas remain outside this unit.

A `TTunnelLocal` ordinarily accepts loopback TCP connections only. Set `toAcceptNonLocal` only if the application intentionally exposes the listener beyond the local machine, with corresponding firewall/access policy. `OpenSocket()` accepts an already-connected socket and therefore leaves **peer-origin validation** to its caller. The remote agent should connect only to the intended local backend (for example the approved VNC port), never to an arbitrary relay-selected host/port.

Session IDs are public routing metadata, not secrets. Handshake timeouts, deferred startup, pending-session expiry, relay callback lifetime, and connection cleanup reduce accidental resource retention, but do not make denial of service impossible. The cleartext close frame allows prompt shutdown and can also be forged by a relay. This is a conscious availability boundary rather than a confidentiality/integrity bypass of established GCM traffic.

## 11. Deployment examples and operational checklist

### 11.1 Certificate-authenticated ECDHE

On each peer, select `[toEcdhe]`, provide the local signing identity, and pin the expected remote verification identity **before** the relay interaction. Do this reciprocally for mutual certificate authentication:

```pascal
Options := [toEcdhe];
// LocalSignCert has the local private signing key.
// ExpectedRemoteCert is provisioned through a trusted channel.
Local.Open(Sess, RelayEndpoint, Options, TimeoutMS,
  '', LocalAddress, ['side', 'agent'], LocalSignCert, ExpectedRemoteCert);
```

A blank `AppSecret` is a supported configuration in **this case**, because independently trusted certificate signatures authenticate the handshake. The diagnostic JSON is still only obfuscation-grade, not a secret channel. Ensure the certificate's key usage supports digital signatures and the peer certificate matches the requested endpoint's expected identity.

### 11.2 Certificate-free ECDHE with a strong PSK

When endpoint-specific certificate provisioning is impractical, create a private, high-entropy `AppSecret`, share it securely with the intended endpoint or narrow trust group, and provide the same value on both sides:

```pascal
Options := [toEcdhe];
// SharedSecret: >= 128 bits of CSPRNG entropy, confidential and independent
// of any relay that is not trusted for endpoint authentication.
Local.Open(Sess, RelayEndpoint, Options, TimeoutMS,
  SharedSecret, LocalAddress, ['side', 'agent'], nil, nil);
```

This authenticates **possession of the PSK**, not a uniquely named machine unless the PSK distribution defines that identity. Avoid a global embedded application secret. If the PSK is long-lived, secure its storage and arrange rotation on compromise.

### 11.3 Remote assistance / VNC operational policy

1. Have the administrator select a specific device and the agent confirm any required end-user consent. Record authorization and revocation separately from the tunnel handshake.
2. Use a fresh `TTunnelSession` for routing, with `[toEcdhe]` and **either** a trusted peer certificate or a strong secret. Do not treat the session ID as a pairing credential.
3. Keep the VNC listener/backend loopback-only unless there is an explicit reason otherwise. With demand-driven operation, use `WaitForHandshake()` and `OpenSocket()` with suitable local socket-origin checks.
4. Keep credentials, private keys, screen contents, and other sensitive values out of `InfoNameValue` and diagnostic logs. Transport encryption does not make those logs or metadata automatically secret.
5. Terminate the connection on authorization revocation, local socket closure, or cryptographic verification failure. Use local VNC authentication/permissions as appropriate for the application.
6. Verify that the selected `ICryptCert` and PSK trust anchors were provisioned independently of any relay from which active-interception resistance is required.

These code examples illustrate the security-relevant call parameters rather than a complete compilable relay/client application.

## 12. Tests and further validation

The current [`TNetworkProtocols.Tunnel`](../../test/test.net.proto.pas) test area exercises plain forwarding, `toEncrypt`, `toEcdhe`, signature-only handshakes, symmetric encryption with signatures, ECDHE with mutual signatures, relay routing, deferred startup, bidirectional binary traffic, and actual WebSockets/SOA callbacks. ECC tests exercise P-256 key generation, signatures, ECDH agreement, and public-key compression/uncompression. These are important functional and interoperability checks, not substitutes for adversarial security tests.

A focused negative-security suite should also exercise:

| Test | Expected safety property |
| --- | --- |
| Flip a byte in certificate-signed handshake JSON | Certificate verification fails before diagnostic JSON is accepted |
| Change fixed handshake fields or ECDHE public key | Certificate and/or strong PSK authentication fails |
| Use wrong or unknown `VerifyCert` | Signed handshake rejected |
| Use wrong `AppSecret` | Fixed-handshake checksum rejected; no forwarding started |
| Corrupt AES-GCM ciphertext or tag | Fail closed; **no altered plaintext delivered** |
| Replay or reorder an encrypted record | No unauthenticated data forwarded; tunnel may disconnect |
| Supply malformed P-256 prefixes, `x >= p`, non-residue X, off-curve affine coordinates, or invalid scalar | Native ECC operation safely rejects and clears outputs as applicable |
| Send oversized or truncated frames / invalid JSON lengths | No out-of-bounds access or uncontrolled plaintext allocation |
| Exceed 128 queued handshake frames | Predictable rejection without unbounded local queue growth |
| Repeated open/close, deferred callbacks, and socket shutdown races | No deadlock, use-after-free, leaked descriptor, or unintended key reuse |

Test runs should record compiler (Delphi/FPC), operating system, architecture, cryptographic backend (native/OpenSSL), and transport (direct callbacks/WebSockets). The present review is based on source code and test inspection; it does **not** assert these negative cases were run successfully.

## 13. Security properties and remaining design boundaries

The major security-sensitive implementation paths are coherent at the source level: certificate signatures span the fixed handshake **and JSON ciphertext**, fixed-handshake shared-secret verification is available without certificates, P-256 native input validation rejects malformed points, directional GCM keys and evolving IVs protect each application frame, authentication failure prevents forwarding of altered plaintext, and incoming frames/handshake queues have explicit bounds.

The remaining limitations are intentional or operational:

- The handshake/key schedule is application-specific, not a standardized, formally proven AKE or PAKE. Independent protocol review and cross-platform regression testing remain valuable.
- `[toEcdhe]` without a privately provisioned strong `AppSecret` or trusted peer certificate does not resist an active man-in-the-middle.
- A static strong PSK is permissible, but compromised or group-shared PSKs permit impersonation of other holders. The application controls identity mapping, expiry, and rotation.
- Certificate signatures are useful only with the **expected peer's** independently trusted verification credential; the tunnel does not automatically enforce PKI, authorization, or consent.
- JSON metadata is deliberately non-sensitive, non-authoritative, and at most obfuscated, even though a configured certificate signature authenticates its ciphertext.
- GCM uses implicit per-direction record order, with no reorder window or rekey exchange; the relay can disrupt traffic and send unauthenticated closure notifications.
- The relay/transport can still deny service, and an endpoint or its local VNC process may independently be compromised.

**Conclusion:** For an administrator-to-agent tunnel using **fresh P-256 ECDHE**, **AES-128-GCM**, **independently trusted peer certificates and/or a strong confidential `AppSecret`**, and **diagnostics-only JSON**, the implementation provides a practical end-to-end security design that does not require the relay to see the forwarded VNC payload. The decisive deployment requirement is authenticating the intended peer: certificates are an appropriate endpoint-identity mechanism, while a high-entropy private PSK is a viable certificate-free alternative when its distribution and trust scope are well controlled.

## References

**Repository sources**

- [`mormot.net.tunnel.pas`](../../src/net/mormot.net.tunnel.pas) — handshake, relay routing, forwarding, AES-GCM record handling, frame bounds.
- [`mormot.crypt.ecc256r1.pas`](../../src/crypt/mormot.crypt.ecc256r1.pas) — P-256 key generation, compressed-point decoding, public-key validation, ECDH.
- [`mormot.crypt.core.pas`](../../src/crypt/mormot.crypt.core.pas) — AES-GCM implementation and IV progression.
- [`mormot.crypt.secure.pas`](../../src/crypt/mormot.crypt.secure.pas) — `ICryptCert` signing and verification abstraction.
- [`test.net.proto.pas`](../../test/test.net.proto.pas) and [`test.core.ecc.pas`](../../test/test.core.ecc.pas) — functional and ECC test coverage.

**Further reading**

- [NIST SP 800-38D](https://csrc.nist.gov/pubs/sp/800/38/d/final) — Galois/Counter Mode and nonce requirements.
- [NIST SP 800-56A Rev. 3](https://csrc.nist.gov/pubs/sp/800/56/a/r3/final) — elliptic-curve key agreement.
- [NIST SP 800-108 Rev. 1](https://csrc.nist.gov/pubs/sp/800/108/r1/upd1/final) — PRF-based key derivation.
- [RFC 5869](https://www.rfc-editor.org/rfc/rfc5869) — HKDF (for comparison; this protocol has its own key schedule).
- [RFC 8236](https://www.rfc-editor.org/rfc/rfc8236) — PAKE properties and resistance to offline password guessing.

*Engineering source review and deployment guidance, not cryptographic certification or a formal security proof.*
