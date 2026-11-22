# Windows SChannel TLS and Server mTLS — Design, Security Rationale, and Implementation Review

**Scope:** Native Windows SChannel TLS client and HTTPS/TLS server in mORMot 2, with particular attention to mutual TLS (mTLS), certificate trust, callbacks, connection state, and the OpenSSL compatibility contract.  
**Source snapshot:** mORMot 2 **2.4.17601**, [`c3e08fe860a464af36a839c7b5c69a76cae0cc71`](https://github.com/synopse/mORMot2/tree/c3e08fe860a464af36a839c7b5c69a76cae0cc71) (13 October 2026, `master` at review time).  
**Primary implementation:** [`mormot.net.sock.windows.inc`](../../src/net/mormot.net.sock.windows.inc) ([reviewed revision](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.windows.inc)).  
**API and integration:** [`mormot.net.sock.pas`](../../src/net/mormot.net.sock.pas), [`mormot.net.server.pas`](../../src/net/mormot.net.server.pas), and [`mormot.lib.sspi.pas`](../../src/lib/mormot.lib.sspi.pas).  
**OpenSSL comparison:** [`mormot.lib.openssl11.pas`](../../src/lib/mormot.lib.openssl11.pas).  
**Tests:** [`test.net.proto.pas`](../../test/test.net.proto.pas), `TNetworkProtocols._SChannel` (Windows with `USE_OPENSSL`).  
**Review method:** Static inspection of the pinned implementation, its API and test source, plus relevant Microsoft API contracts. **No Windows build or integration test was executed in this review.**

> **Engineering conclusion.** The native SChannel backend deliberately provides a **dependency-light Windows TLS transport** with normal operating-system server-certificate validation on the client and optional **server-side client-certificate authentication**. For mTLS, the implementation requests a certificate through SSPI, obtains the remote certificate, builds and evaluates a client-authentication chain with CryptoAPI, and rejects a missing or invalid certificate **unless an application-installed per-certificate callback explicitly overrides the validation result**. Explicit trust anchors are isolated in a private chain engine; TLS and certificate resources have well-defined ownership across accepted connections.
>
> The intended default is straightforward: **use ordinary one-way TLS unless client identity authentication is required; for mTLS, enable `ClientCertificateAuthentication`, configure the appropriate trusted CA set, and leave validation callbacks unset unless they enforce a carefully designed additional policy.** The callback override, offline-revocation tolerance, Windows-specific trust-anchor behavior, and currently absent SChannel client-certificate configuration are **real boundaries**, not properties that documentation should hide. This is an implementation-oriented security review, **not a claim of formal certification or an audit of Microsoft's TLS stack**.

## 1. Goals, boundaries, and threat model

### 1.1 Responsibilities and design intent

| Component | Responsibility | Security-relevant code/API |
| --- | --- | --- |
| `TNetTlsContext`, `INetTls` | Shared TLS configuration and stable caller/backend contract | `AfterConnection`, `AfterBind`, `AfterAccept`, `ResetNetTlsContext` |
| `TSChannelNetTls` | One connection's SSPI context, handshake, encrypted records, post-handshake processing, output fields | `CredInit`, `HandshakeLoop`, `Receive`, `Send`, `GetRawTls` |
| `TSChannelServerContext` | Shared inbound credential, server certificate, private client trust, mTLS chain verification | `LoadCertificate`, `LoadClientTrust`, `ValidateClientCertificate` |
| Windows SSPI / SChannel | TLS negotiation, cryptographic traffic protection, remote certificate retrieval | `AcquireCredentialsHandleW`, `InitializeSecurityContextW`, `AcceptSecurityContext`, `QueryContextAttributesW` |
| Windows CryptoAPI / certificate wrappers | Certificate decoding, chain construction, chain policy, trust stores, context ownership | `CertGetCertificateChain`, `CertVerifyCertificateChainPolicy`, `TWinCertStore`, `WinCryptTextToBinary` |
| OpenSSL backend | Cross-backend API reference, not the native implementation dependency | `TOpenSslNetTls.SetupCtx`, `AfterConnectionPeerVerify`, `AfterAccept` |

The SChannel path uses Windows SSPI, CryptoAPI and (for supported private keys) CAPI/CNG through mORMot's core/SSPI wrappers; it does **not** load OpenSSL or depend on `mormot.crypt.*` to carry out native TLS validation. The OpenSSL test client and certificate generators are **test dependencies**, not SChannel production dependencies.

**Security objective:** an HTTPS server configured for mTLS should admit an application request only after a completed TLS handshake and an acceptable client certificate chain, subject to any deliberately installed application verification policy. A trusted certificate establishes a chain to an accepted anchor and client-authentication suitability; it **does not by itself grant application authorization** or bind a certificate to an account, tenant, role, or request.

**Assumptions:** trusted application process and callback code; a working Windows certificate/SSPI implementation; accessible server private keys; correctly provisioned CA certificates; and appropriate operating-system security updates. Compromised trust anchors, stolen endpoint private keys, malicious local code, Windows trust-store policy changes, and certificate issuance mistakes are outside the guarantees of this adapter. The documented code review does not establish timing, denial-of-service, or memory-safety properties of Windows components.

### 1.2 Default-first choices

1. **One-way TLS:** `ClientCertificateAuthentication = false`. Server certificate is presented; no client certificate is required. The historical lightweight `AfterAccept` path is retained.
2. **mTLS without a custom callback:** `ClientCertificateAuthentication = true`, an intentional CA policy, and `OnEachPeerVerify = nil`. No client certificate and Windows-reported chain-policy failures reject the connection.
3. **mTLS with callbacks:** callbacks can implement authorization/extra checks, but `OnEachPeerVerify` is a **verification override** rather than a guaranteed second, stricter verification pass. Callbacks that always return `true` can neutralize Windows validation errors.
4. **Outbound SChannel client:** Windows-managed server-certificate checking when `IgnoreCertificateErrors = false`. Custom client certificate input and custom client CA stores are **not implemented** for the native SChannel client; choose a supported backend for those needs.

## 2. TLS negotiation and credentials

`CredInit` selects between two Windows credential layouts:

| Platform/path | Credential setup | Protocol intent |
| --- | --- | --- |
| Windows with `wsTls13`, global `SChannelEnableTls13 = true`, and `DisableTls13 = false` | `SCH_CREDENTIALS`; with `AllowDeprecatedTls = false`, disable `SP_PROT_TLS_UNSAFE` via `TLS_PARAMETERS` | Permit TLS 1.3 when the OS supports it; exclude pre-TLS-1.2 protocols by default |
| Other supported modern Windows path, or `DisableTls13 = true` | Legacy `SCHANNEL_CRED` | Explicit TLS 1.2; opt-in to TLS 1.0/1.1 only with `AllowDeprecatedTls` |
| Pre-Windows-7 compatibility path | Legacy `SCHANNEL_CRED` | TLS 1.0 only; **server mTLS is rejected before credential acquisition** |

`SCH_USE_STRONG_CRYPTO` is requested when supported and deprecated TLS is not allowed. Actual cipher suites, OS policy, and TLS 1.3 availability remain **Windows configuration and platform dependent**. SChannel does not implement the OpenSSL-specific `CipherList` and `CipherSuites` inputs. On the reviewed revision `SChannelEnableTls13` defaults to `true`.

The handshake deals explicitly with `SEC_I_CONTINUE_NEEDED`, incomplete records, output tokens, `SECBUFFER_EXTRA`, and `CompleteAuthToken` statuses. Data received with the last handshake message is retained for normal decryption rather than being discarded. Application records are protected using SChannel's `EncryptMessage` and `DecryptMessage`; partial encrypted writes are buffered and must finish before new plaintext is consumed.

**Boundary:** a successful TLS handshake is necessary but **not sufficient** for an mTLS server connection: explicit certificate validation follows the initial server handshake in `AfterAccept`.

## 3. Outbound SChannel client: what is and is not authenticated

`TSChannelNetTls.AfterConnection` creates an outbound SChannel credential with `SCH_CRED_NO_DEFAULT_CREDS` and passes the `ServerAddress` to `InitializeSecurityContextW` for normal target-name handling. With `IgnoreCertificateErrors = false`, it asks SChannel for chain-revocation checking while tolerating an offline revocation source. After the handshake, it exposes `CipherName` and, if decoded, `PeerIssuer` and `PeerSubject`; `PeerInfo` is generated on request. `PeerCert` on this **client output path points to the connection's `TWinCertInfo` data**, not a server-side borrowed `PCCERT_CONTEXT` callback argument.

`IgnoreCertificateErrors = true` instead sets `SCH_CRED_MANUAL_CRED_VALIDATION`, but the native client path performs **no replacement manual chain/hostname verification**. It therefore disables standard server-certificate authentication for that connection. Use it only under an independently controlled trust model (for example, focused tests); transport encryption **alone** does not authenticate the remote endpoint.

`HostNamesCsv`, `OnPeerValidate`, `OnAcceptServerName`, custom CA inputs, and `CertificateFile`/`CertificateBin`/`CertificateRaw` as **outbound client credentials** are not implemented on SChannel. In the `SEC_I_INCOMPLETE_CREDENTIALS` branch, the handshake deliberately continues with `ISC_REQ_USE_SUPPLIED_CREDS` rather than selecting an arbitrary client identity. This is **not** a client-side mTLS implementation. OpenSSL can support client certificates and custom verification through its separate implementation.

## 4. Inbound server identity and mTLS handshake

### 4.1 Server certificate source and private key

`TSChannelServerContext.LoadCertificate` uses the following source order:

1. `CertificateRaw` as a `PCCERT_CONTEXT`, duplicated so the bound server owns its own reference; its CAPI/CNG private key must be accessible.
2. Otherwise `CertificateBin`, then `CertificateFile`, as a **PFX/PKCS#12** bundle containing the server certificate and private key. The import password comes from `PrivatePassword`, overridden by `OnPrivatePassword` when assigned.
3. Otherwise the first certificate with an accessible private key in the Windows personal (`MY`) store.

`PrivateKeyFile` and `PrivateKeyRaw` are OpenSSL-specific, not supported in the SChannel server. PKCS#12 import and private-key discovery can use Windows CNG on capable systems. A PFX is distinct from a PEM certificate file: PEM is supported for **CA trust input**, not as the native server's separate certificate/key file pair.

### 4.2 Client certificate requirement

With `ClientCertificateAuthentication = true`, `AfterAccept` includes `ASC_REQ_MUTUAL_AUTH` in the initial `AcceptSecurityContext` request. The successful handshake is followed by `QueryContextAttributesW(SECPKG_ATTR_REMOTE_CERT_CONTEXT)`. `SEC_E_NO_CREDENTIALS` or an absent remote certificate is treated as an error. The implementation must do this check because SChannel server applications are responsible for validating and interpreting client certificates; TLS negotiation alone is not an authorization decision.

`SCH_CRED_NO_SYSTEM_MAPPER` prevents automatic Windows user-account mapping. A valid client certificate therefore **does not log the application user in** or establish an application role. Certificate-to-principal mapping remains the application's responsibility.

For ordinary one-way TLS, `AfterAccept` does not run the mTLS chain-validation path or its per-peer callbacks and preserves its existing fast path.

## 5. Client trust selection and Windows chain validation

### 5.1 Trust-source precedence and construction

`TSChannelServerContext.LoadClientTrust` runs at `AfterBind`. **Only one** trust-source branch is loaded, in this order:

| Priority | `TNetTlsContext` input | Native behavior |
| --- | --- | --- |
| 1 | Non-empty `CACertificatesFile` | Read one DER X.509 certificate or extract one or more `-----BEGIN CERTIFICATE-----` PEM blocks; `WinCryptTextToBinary` decodes PEM |
| 2 | Non-empty `CASystemStores` | Enumerate selected Windows stores via `TWinCertStore`, copy their certificates into a memory store |
| 3 | Non-`nil` `CACertificatesRaw` | Treat entries as borrowed `PCCERT_CONTEXT` pointers, add independently retained certificates; reject an effectively empty set |
| Default | No explicit source | Use the default Windows chain engine and normal Windows trust |

**Important distinctions:** `CASystemStores = []` is **not** an empty exclusive trust store: it selects the normal Windows trust behavior if no other CA input is supplied. A non-empty store selector builds a **snapshot** of selected certificates, not a live lookup into those stores. File selection takes precedence even if other CA inputs are set. An explicitly specified but empty/malformed CA source is a configuration error; it is not silently replaced by system trust.

The code constructs an `HCERTSTORE` in memory and a private `HCERTCHAINENGINE` with `hExclusiveRoot`; this constrains **trust anchors** for the server's subsequent manual chain validation. From Windows 8/Server 2012, `CERT_CHAIN_EXCLUSIVE_ENABLE_CA_FLAG` also permits a **non-self-signed intermediate CA** in that store to act as an anchor. On Windows 7/Server 2008 R2 that flag is unavailable; only self-signed roots are anchors under the documented Windows rule. Treat every configured anchor as highly trusted: when an intermediate is deliberately selected as an anchor, normal signature/revocation validation does **not** continue above that anchor.

Do not equate this with OpenSSL's precise trust-store semantics: the backends follow the same **configuration precedence**, but OpenSSL adds CAs to its own `X509_STORE` and calls different validation machinery. This is a compatibility goal, **not a proof of bitwise-identical trust decisions**.

### 5.2 Implemented certificate checks

For the remote client leaf, the server:

1. Requests the TLS **client authentication EKU** (`szOID_PKIX_KP_CLIENT_AUTH`) in `CERT_CHAIN_PARA`.
2. Builds the chain using `CertGetCertificateChain`, a private engine if one was configured, and `CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT`.
3. Applies `CERT_CHAIN_POLICY_SSL` with `SSL_EXTRA_CERT_CHAIN_POLICY_PARA.dwAuthType = AUTHTYPE_CLIENT` using `CertVerifyCertificateChainPolicy`.
4. Records `policy.dwError` as a Windows error description in `LastError` if nonzero, then evaluates callbacks or rejects the error by default.

`CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS` makes revocation status **soft-fail when the relevant revocation source is unknown or unavailable**. This is deliberately similar to the existing SChannel client policy, but must not be represented as strict, always-online revocation enforcement. A certificate known to be revoked is different from one whose revocation status cannot be determined. If hard-fail revocation is mandatory, the current policy is not sufficient without further design or code changes.

The code does not supply an application client hostname to the SSL chain policy; that is appropriate for generic client identity, but it means the application must independently decide **which authenticated client is authorized**. The subject DN or display text alone is not a complete identity/authorization policy.

### 5.3 Default outcome and explicit override

| Condition | No `OnEachPeerVerify` | Callback installed |
| --- | --- | --- |
| No client certificate | Reject before callbacks | Reject before callbacks |
| Valid chain and usage | Accept chain | Callback may accept or veto each supplied element |
| Expired, untrusted, or wrong-usage certificate reported as a policy error | Reject | **May be accepted if all callbacks return `true`** |
| Any per-element callback returns `false` or raises | Not applicable | Reject; the after-validation callback is not invoked |
| `IgnoreCertificateErrors = true` on the server | **Does not** disable mTLS chain checks | Does not itself disable checks; callbacks may override errors |

`OnEachPeerVerify` is powerful by design. For **additive** application restrictions, a callback should preserve Windows' result (conceptually `Result := wasok and ApplicationAllowsThisCertificate`) rather than return `true` unconditionally. If the application deliberately overrides trust, the override and its consequences should be explicit and audited. `OnAfterPeerValidate` is a `procedure`: it can inspect the accepted client leaf and **raise an exception to reject** the connection, but returning normally does not reverse an earlier chain failure that was already rejected.

## 6. Callback semantics and output fields

| Callback/output | Server-side SChannel behavior |
| --- | --- |
| `OnEachPeerVerify` | Invoked **after** native chain construction/policy evaluation, in chain/element traversal order (typically leaf to root for the selected simple chain); `wasok` combines an element's `TrustStatus.dwErrorStatus` with the location of the SSL policy error |
| `OnAfterPeerValidate` | Invoked once after successful **initial** mTLS validation (including permitted explicit overrides), with the remote leaf; not rerun by the reviewed post-handshake validation call |
| `TLS` argument | Borrowed `PCtxtHandle` (`@fCtxt`) for the current SChannel security context |
| `Peer` argument and `Context.PeerCert` | Borrowed `PCCERT_CONTEXT` for the current element or client leaf; valid during the callback only |
| `PeerIssuer`, `PeerSubject` | Populated from `WinCertCtxtDecode` where decoding succeeds; the element callback sees the current certificate, the after callback sees the client leaf |
| `PeerInfo` | `WinCertInfoToText` output when requested or needed to describe a policy error, if decoding/text formatting is available |
| `CipherName` | Established cipher description for the initial connection, available to the after callback |
| `LastError` | Windows policy-error text or an appropriate backend error when set; **not a structured, exhaustive verification report** |

Per-element callback state is a **temporary copy** of `TNetTlsContext`; its output mutations are not published to the shared listener or guaranteed to persist into subsequent callbacks. The connection-local mTLS context preserves the **leaf** metadata outside the per-element traversal. Exceptions raised inside `OnEachPeerVerify` are caught and treated as rejection; exceptions in `OnAfterPeerValidate` propagate and abort acceptance.

The Windows callback executes **after** its chain was constructed. In OpenSSL, `AfterConnectionPeerVerify` runs **during** path verification; therefore iteration order, which errors are surfaced, and the exact meaning of `wasok` can differ. Do not write portable callbacks assuming identical native pointer types, intermediate verification stages, or error text. SChannel's `OnPeerValidate` and `OnAcceptServerName` remain unimplemented.

## 7. Post-handshake TLS processing and `ClientVerifyOnce`

`TSChannelNetTls.Receive` handles `SEC_I_RENEGOTIATE`, including TLS 1.3 post-handshake/key-update traffic. It retains extra encrypted bytes, runs `HandshakeLoop(NeedRecv = false)` under an SSPI connection lock, then applies these mTLS rules:

- **Initial handshake:** the server always requests and validates the client certificate when `ClientCertificateAuthentication` is set.
- **`ClientVerifyOnce = false` (default):** `ASC_REQ_MUTUAL_AUTH` remains set. On later `SEC_I_RENEGOTIATE` processing the implementation re-queries and revalidates the currently exposed peer certificate. The repeated validation calls use `CallAfter = false`, so `OnAfterPeerValidate` is **not** repeated.
- **`ClientVerifyOnce = true`:** after initial validation the code clears `ASC_REQ_MUTUAL_AUTH`, retains the initial authenticated-identity assumption, and does **not** perform that later mTLS revalidation. This follows the intended "once" policy but does **not** reproduce OpenSSL's `SSL_VERIFY_CLIENT_ONCE` handshake timing exactly.

TLS 1.3 key updates are not a general TLS 1.2 renegotiation mechanism. The code uses SChannel's status to enter its common post-handshake processing path; a test invokes an OpenSSL TLS 1.3 key update to exercise it on supported Windows versions.

`fSspiSafe` coordinates receive-side post-handshake operations with `Send`. If encrypted output is already pending when the receive path encounters `SEC_I_RENEGOTIATE`, the implementation returns a fatal receive result rather than attempt complex concurrent record reordering. This is an **explicit compatibility/availability limitation**, not an assertion that all overlapping send/key-update schedules succeed. Subsequent exceptions from receive-side processing are turned into `nrFatalError` for caller-driven connection shutdown.

## 8. Ownership, lifetimes, and concurrency

`AfterBind` constructs one reference-counted `TSChannelServerContext`, keeping the inbound `CredHandle`, server certificate, imported PFX/personal-store handle if applicable, client CA memory store, and optional private chain engine. Accepted connections retain an interface reference to the shared object, so `FreeCredentialsHandle` is deferred until their SSPI contexts have been deleted.

`CertificateRaw` is duplicated. `CACertificatesRaw` items are added to the memory store, which retains independent certificate references. Once `AfterBind` has **completed successfully**, callers may release their original raw certificate contexts and remove the temporary CA file; the server's loaded snapshot remains usable. Do not mutate the listener's CA policy expecting an already-bound server to refresh it.

Each `ValidateClientCertificate` invocation releases the remote `PCCERT_CONTEXT` returned by `QueryContextAttributesW` and the `PCERT_CHAIN_CONTEXT` from `CertGetCertificateChain`. Callback pointers are borrowed from those live objects; application code must use an appropriate Windows duplication API if it needs a persistent reference. `PeerCert` is cleared before leaving the validation routine.

A listener's `BoundContext` may be shared by asynchronous accept loops. The socket server copies the **full mTLS policy** to each connection (while clearing output fields), and SChannel stores a private `fAcceptContext` for subsequent post-handshake validation. For one-way TLS the listener still propagates the lightweight `AcceptCert` reference. The `INetTls.AfterAccept` parameters and output-pointer behavior have not been changed to accommodate mTLS.

**Concurrency boundary:** immutable trust and shared inbound credentials are reused across connections; their lifetime is reference-counted. The per-connection SSPI handles, handshake buffers, peer-output strings, and verification callback state are not intentionally shared. This source-level design does not substitute for stress testing under high connection churn, failed handshakes, or concurrent `Receive`/`Send` activity.

## 9. Existing tests and what they actually establish

### 9.1 Source-inspected regression scenarios

The test is **not** in a standalone `test.net.schannel.pas` unit. It is `TNetworkProtocols._SChannel` in [`test/test.net.proto.pas`](../../test/test.net.proto.pas), compiled with `SCHANNEL_VERIFY` when **`OSWINDOWS` and `USE_OPENSSL`** are defined. The test still skips on Windows versions older than Windows 7 and at runtime if OpenSSL is unavailable. It generates temporary **ECDSA P-256** identities through the OpenSSL certificate implementation, forces `NewSChannelNetTls` for the first server pass, and uses a **direct `NewOpenSslNetTls` client** so a client certificate can be supplied.

| Test area | Present in the reviewed test source | Important qualification |
| --- | --- | --- |
| Accept a valid private-CA client | Yes: file CA; raw `PCCERT_CONTEXT` CA | Raw input is released after `WaitStarted` to exercise reference ownership |
| Reject absent, unknown-CA, expired, wrong-EKU clients | Yes | Validated by expected HTTP connection success/failure, not a separate native error-code assertion for each case |
| No callback vs callback policy | Yes: valid certificate acceptance; per-element veto; deliberate expired-certificate override | Verifies the **power of the override**; does not demonstrate that callback use is always safe |
| Peer field and callback pointer contracts | Yes: non-`nil` pointers, matching `PeerCert`, nonempty decoded fields, cipher in after callback | Does not inspect all certificate extensions or pointer retention after callback |
| Windows default trust | Yes: private test CA is expected to be untrusted | Assumes the private test CA was not independently installed as trusted |
| TLS 1.2 | Yes: `DisableTls13` case and 16 MiB-plus POST | Negotiated cipher/protocol not explicitly asserted by the test's own parser |
| TLS 1.3 and post-handshake key update | Yes, on Windows 11+ branch: large POST and OpenSSL `SSL_key_update` | Platform/OS capabilities are conditional; not a universal TLS 1.3 pass claim |
| One-way SChannel HTTPS compatibility | Yes | Confirms a client without a certificate can still use a server not requiring mTLS |
| OpenSSL server parity | Yes: valid/missing client, callback veto/override, one-way TLS, large POST and key update | Exercises API-level expectations, not bit-identical chain decisions |
| `CASystemStores` | **OpenSSL-server test only** with `[scsRoot]` | The original document's claim of a SChannel system-store rejection test is **not supported by the reviewed test source** |

The server test is based on asynchronous HTTP; it creates/deletes temporary CA files, temporary client private-key files, and PFX content without installing certificates in persistent Windows certificate stores. Some cases request that the test client **ignore the server certificate**, which is reasonable for this isolated private-CA integration test but **not recommended as a production client policy**.

**Evidence level:** the above scenarios are **present in source**. Their presence does not prove they passed on every supported Windows/Delphi/FPC configuration. This review did not run the suite and does not claim a current green CI result.

### 9.2 Recommended release-level verification

The following are **additional tests to implement or execute**, not existing confirmed coverage:

1. **SChannel `CASystemStores`:** nonempty selection, private-CA rejection, selected-store acceptance, empty-store handling, and the exact current-user/machine store scopes.
2. **CA parsing and anchoring:** DER input, multi-PEM input, malformed/truncated PEM, empty raw array, duplicate entries, self-signed root versus intermediate-only anchors, and Windows 7 versus Windows 8+ trust-anchor behavior.
3. **Client certificate edge cases:** revoked certificate, offline/unknown revocation, missing EKU, EKU mismatch, invalid basic constraints, partial chain, and expired intermediate/root, with callback absent, rejecting, and explicitly overriding.
4. **Post-handshake policy:** both values of `ClientVerifyOnce`; TLS 1.2 renegotiation where available; TLS 1.3 key update and `SEC_I_RENEGOTIATE`; verify initial-only `OnAfterPeerValidate` behavior.
5. **Concurrency and I/O:** concurrent connects, `Receive`/`Send` overlap, pending ciphertext during key update, fragmented handshake tokens, `SECBUFFER_EXTRA`, connection teardown while listener ownership changes, and clean shutdown.
6. **Credential variants:** server `CertificateRaw`, PFX file/binary, protected PFX password callback, missing private key, CAPI and CNG key providers, and personal-store fallback.
7. **Outbound SChannel client:** normal hostname/chain enforcement, expired/untrusted server rejection, explicit `IgnoreCertificateErrors` semantics, TLS version fallback, and confirmation that client-cert inputs are unsupported rather than silently expected to work.

For a release claim, record the exact commit, Windows edition/build, certificate-store state, Delphi/FPC versions, OpenSSL availability/version, test invocation, observed cipher/protocol, and outcomes. Exercise both Windows 10/TLS 1.2 and a supported Windows 11/Server 2022 TLS 1.3 host if those deployments are claimed.

## 10. Security decisions, caveats, and action priorities

| Priority | Decision or limitation | Engineering implication |
| --- | --- | --- |
| **High — integration policy** | `OnEachPeerVerify` may override Windows validation failure | Review custom callbacks as **security-critical trust policy**; additive callbacks should preserve `wasok`; do not install an unconditional `true` callback |
| **High — deployment policy** | Unknown/offline revocation status is ignored | Appropriate for a selected availability-oriented policy, **not** for deployments requiring mandatory fresh revocation evidence |
| **High — identity/authorization** | Valid client CA + client-auth EKU is not an application identity grant | Bind verified client identity to a principal and authorization policy, preferably using stable parsed certificate properties, not display text alone |
| **Medium — platform** | Exclusive trust anchors differ on Windows 7 versus Windows 8+ | Test intermediate-CA anchors on every supported Windows generation; prefer a purpose-managed root CA when practical |
| **Medium — parity** | SChannel callback occurs after chain construction; OpenSSL callback occurs during it | Treat callback portability as **API compatibility with documented differences**, not identical validation traces |
| **Medium — post-handshake** | `ClientVerifyOnce` changes revalidation; pending output may prevent post-handshake processing | Test requirements against actual client traffic and TLS 1.3 key-update frequency |
| **Medium — test evidence** | No native SChannel `CASystemStores` integration assertion; other edge cases untested | Extend the focused Windows regression matrix before stronger verification claims |
| **Feature boundary** | No SChannel outbound client-certificate/custom-CA implementation | Choose OpenSSL for those features or implement and separately audit native support |

The first three rows are **usage and threat-model requirements**, not necessarily defects in the code. A stricter fail-closed revocation model, disallowing verification overrides, client-side mTLS, or broader SNI support would require **product/API and implementation decisions**, not a documentation-only change.

### 10.1 Minimal server configuration guidance

For a private-CA mTLS service, initialize a server `TNetTlsContext`, supply a valid SChannel server identity (PFX or native certificate with private key), set `ClientCertificateAuthentication := true`, and set **one intentional trust source**, e.g. `CACertificatesFile := 'client-ca.pem'`. Do **not** set `OnEachPeerVerify` merely to return `true`. Add `OnAfterPeerValidate` if the server must associate the accepted client certificate with application identity or reject a client under an additional authorization rule. Keep trusted CA keys and the server PFX private key protected, and define a certificate renewal/revocation procedure.

Prefer OS-managed outbound SChannel validation on normal HTTPS clients (`IgnoreCertificateErrors = false`). Do not use the test's `IgnoreCertificateErrors = true` pattern as a deployment example. If the connecting application must send its own certificate or constrain the trust anchors to a custom file, use an implementation that actually supports those inputs rather than assuming SChannel will read the OpenSSL fields.

### 10.2 What this review does not claim

- No independent cryptographic primitive, Windows SChannel binary, or CryptoAPI validation audit was performed.
- No native Windows runtime/compiler matrix was executed as part of this document update.
- No formal equivalence is established between SChannel and OpenSSL verification, revocation, or post-handshake behavior.
- No guarantees are made about certificate-to-application-principal mapping unless an application implements and tests that policy.
- No assertion is made that a certificate with unknown revocation status is automatically rejected; the reviewed policy deliberately permits that class of uncertainty.

## 11. Conclusion and maintenance criteria

The reviewed SChannel server mTLS implementation is **architecturally consistent with mORMot's minimal-dependency, native-OS, default-first approach**: it shares immutable credentials, uses Windows chain-policy validation, isolates explicit anchors, retains native handles safely, and preserves the existing `INetTls` contract. The most important security property is **conditional**: client-certificate validation rejects Windows-reported failures **unless an application callback overrides them**. Security-sensitive deployments must intentionally choose CA anchors, callback policy, revocation availability requirements, and client authorization rules.

Revisit this document when any of the following changes: `CredInit` or OS TLS version selection; SChannel outbound certificate handling; `LoadClientTrust` or exclusive anchor semantics; `ValidateClientCertificate` or callbacks; `SEC_I_RENEGOTIATE` / `ClientVerifyOnce` logic; shared-context lifetime; OpenSSL callback compatibility; or the focused Windows test suite. Pin the reviewed commit and update **source-inspected vs executed-test** evidence independently.

## References

**Pinned source and tests**

- [Native SChannel implementation, `mormot.net.sock.windows.inc`](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.windows.inc#L2100-L2655); [client and server handshake](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.windows.inc#L2857-L3100); [receive/key-update and I/O](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.windows.inc#L3183-L3424).
- [TLS interface and `TNetTlsContext` declarations](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.pas#L943-L1284); [context reset](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.sock.pas#L4884-L4953).
- [HTTP server mTLS context propagation](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/net/mormot.net.server.pas#L5282-L5297).
- [OpenSSL verification and callbacks](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/lib/mormot.lib.openssl11.pas#L11401-L11444); [verification setup](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/lib/mormot.lib.openssl11.pas#L11695-L11761); [server after-accept callback](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/src/lib/mormot.lib.openssl11.pas#L11953-L12017).
- [Windows/OpenSSL focused network tests](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/test/test.net.proto.pas#L7433-L7932).
- [Design-review precedent at `c3e08fe86`: `TAesPrng.md`](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/doc/security/TAesPrng.md) and [`mormot.crypt.rsa.md`](https://github.com/synopse/mORMot2/blob/c3e08fe860a464af36a839c7b5c69a76cae0cc71/doc/security/mormot.crypt.rsa.md).

**Windows and TLS API contracts**

- [Performing Authentication Using Schannel](https://learn.microsoft.com/en-us/windows/win32/secauthn/performing-authentication-using-schannel); [Manually Validating Schannel Credentials](https://learn.microsoft.com/en-us/windows/win32/secauthn/manually-validating-schannel-credentials).
- [`SCHANNEL_CRED`](https://learn.microsoft.com/en-us/windows/win32/api/schannel/ns-schannel-schannel_cred) and [`SCH_CREDENTIALS`](https://learn.microsoft.com/en-us/windows/win32/api/schannel/ns-schannel-sch_credentials).
- [`AcceptSecurityContext`](https://learn.microsoft.com/en-us/windows/win32/api/sspi/nf-sspi-acceptsecuritycontext); [Obtaining a Client Certificate](https://learn.microsoft.com/en-us/windows/win32/secauthn/obtaining-a-client-certificate).
- [`CERT_CHAIN_ENGINE_CONFIG`](https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/ns-wincrypt-cert_chain_engine_config), [`CertGetCertificateChain`](https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatechain), and [`CertVerifyCertificateChainPolicy`](https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certverifycertificatechainpolicy).
- [RFC 8446, TLS 1.3](https://www.rfc-editor.org/rfc/rfc8446) (protocol background, not a claim of SChannel version-independent behavior).
