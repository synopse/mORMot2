# `TAesPrng` — CSPRNG Design, Security Rationale, and Implementation Review

**Scope:** mORMot2's fast AES-counter cryptographic pseudorandom generator, entropy conditioning, direct operating-system interfaces, and trusted random-source combination.  
**Source snapshot:** mORMot2 **2.4.17599**, [`efdf73f3e20f`](https://github.com/synopse/mORMot2/tree/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527) (13 October 2026).  
**Implementation:** [`src/crypt/mormot.crypt.core.pas`](../../src/crypt/mormot.crypt.core.pas) ([reviewed source](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/crypt/mormot.crypt.core.pas)).  
**Entropy sources:** [`mormot.core.os.pas`](../../src/core/mormot.core.os.pas), [`mormot.core.os.windows.inc`](../../src/core/mormot.core.os.windows.inc), and [`mormot.core.os.posix.inc`](../../src/core/mormot.core.os.posix.inc).  
**Tests:** [`test/test.core.crypt.pas`](../../test/test.core.crypt.pas).  
**Companion RSA review:** [`mormot.crypt.rsa.md`](mormot.crypt.rsa.md).

> **Engineering conclusion.** `TAesPrng` is designed to be a **general-purpose CSPRNG usable without any application tuning** on the native platforms supported by mORMot2. Its default architecture combines OS-seeded SHAKE256 conditioning and an evolving process-wide entropy chain with a secret AES-CTR key, automatic rekeying, synchronized counter reservation, and fast scalar or AES-NI output generation. These are conventional, well-understood cryptographic building blocks arranged for **cross-platform availability, high performance, and surprise-free defaults**.
>
> The library also deliberately exposes **distinct interfaces for distinct needs**: `FillRandom`/`TAesPrng.Fill` for routine cryptographic random bytes, `TSystemPrng`/direct system entropy modes for OS-source requests, and `FillRandomTrusted` for source-combined initial secrets such as RSA prime seeds. This document explains the algorithms, their concrete protections, and the few operational assumptions important to applications. It does not require users to select unusual algorithms or configuration values for normal use.

## 1. Scope and security objectives

`TAesPrng` serves as a fast local source of pseudorandom bytes for mORMot2 cryptographic operations, including RSA key generation, Miller-Rabin witnesses, PSS salts, padding, and ephemeral keys. It is built around four boundaries:

1. **Entropy acquisition** from operating-system facilities and optionally other process/library inputs.
2. **Entropy conditioning** using SHAKE256 and a process-global chaining state.
3. **AES-CTR expansion** under a secret per-instance AES key, with a monotonically allocated counter space.
4. **Rekeying** from newly conditioned entropy after a configurable usage threshold or on an explicit `Seed` request.

Its optimization goals are low overhead for short random values, scalable throughput for large requests, and minimal contention between threads. Its security goal is computational unpredictability **when its AES state is seeded with sufficient unpredictable entropy and that state remains secret**.

### 1.1 Implementation and trust boundaries

| Component | Responsibility | Relevant API/state |
| --- | --- | --- |
| `TAesPrngAbstract` | Common random-byte interface, helpers, and entropy collection | `FillRandom`, `GetEntropy`, `FillRandomNotZero`, `Random32`, `XorRandom` |
| `TAesPrng` | Stateful AES-counter generator with reseeding | `Seed`, `LockAndPrepareFill`, `FillRandom`, `fAes`, `fBytesSinceSeed` |
| `TSystemPrng` | Direct wrapper around operating-system random acquisition | `FillRandom` -> `FillSystemRandom` |
| `TSha3` | SHAKE256 absorb/squeeze operation | `Init(SHAKE_256)`, `Update`, `Final` |
| `_EntropyChain` | Process-global state propagated across conditioned entropy requests | `_EntropyChainSafe`, `_EntropyChain` |
| `TAesContext` | AES key schedule, IV/counter, and accelerated block processing | `DoRndBlock`, `DoRnd`, `AesNiEncryptCtrNist` |
| `mormot.core.os` | OS entropy access and explicit fallback behavior | `FillSystemRandom`, `XorOSEntropy` |

The implementation presumes a trusted native process, secure compiler/runtime, secrecy of AES keys and entropy-chain state, and an OS random source that succeeds whenever cryptographic unpredictability is required. Physical observation, malicious code in-process, memory disclosure, process cloning/snapshot restoration, and attacker control of system entropy are separate threats requiring additional measures.

### 1.2 Why the default design is a defensible CSPRNG

The relevant argument has several independent parts:

1. **Unpredictable seed material:** under normal operation, the operating system supplies cryptographic randomness; SHAKE256 also absorbs available framework state and optional provider material.
2. **Strong deterministic expansion:** AES-128 and AES-256 are standard block ciphers. With an unpredictable key and unique counter blocks, the output stream is computationally pseudorandom under the AES security assumption.
3. **No accidental counter reuse:** the AES state and range reservations are serialized, including bulk requests processed outside the lock.
4. **Periodic key replacement:** the seed threshold limits the amount of output generated under one key and allows new external entropy to affect future output.
5. **Separate stronger-source contracts where useful:** RSA initial prime seeds use `FillRandomTrusted`, while direct system callers can use an API that reports OS failure.

The design uses primitives and operational patterns discussed by NIST FIPS 197, FIPS 202, SP 800-38A, and the SP 800-90 series. It is a **purpose-built CSPRNG**, rather than a byte-for-byte implementation of the SP 800-90A CTR_DRBG state machine. This distinction should be documented once, without confusing it with a claim that the normal generator is weak.

## 2. Relevant cryptographic standards and guidance

| Publication | What it specifies | Applicability to this design |
| --- | --- | --- |
| [FIPS 197](https://csrc.nist.gov/pubs/fips/197/final) | AES encryption with 128-, 192-, and 256-bit keys and 128-bit blocks | The AES primitive used for output expansion; not a CSPRNG specification. |
| [FIPS 202](https://csrc.nist.gov/pubs/fips/202/final) | SHA-3 hashes and SHAKE128/SHAKE256 XOFs | The entropy-conditioning primitive and output expansion. |
| [NIST SP 800-38A](https://csrc.nist.gov/pubs/sp/800/38/a/final) | Block-cipher operating modes, including CTR | Reference for unique-counter AES keystream construction; not proof of DRBG approval. |
| [NIST SP 800-90A Rev. 1](https://csrc.nist.gov/pubs/sp/800/90/a/r1/final) | Hash-, HMAC- and counter-based deterministic generators | Useful reference for reseeding, lifetime and state-compromise reasoning; mORMot implements its own AES-counter design. |
| [NIST SP 800-90B](https://csrc.nist.gov/pubs/sp/800/90/b/final) | Entropy-source assessment, health testing, and min-entropy concepts | Guidance for avoiding unjustified entropy estimates from easily observed process values. |
| [NIST SP 800-90C](https://csrc.nist.gov/pubs/sp/800/90/c/final) | System-level arrangements of entropy sources and deterministic generators | Background for separating entropy-acquisition policy from fast local generation. |
| [FIPS 186-5](https://csrc.nist.gov/pubs/fips/186-5/final) | RSA digital signatures, key-generation constraints and primality tests | Context for `FillRandomTrusted` initial seeding and strong randomized Miller–Rabin witnesses. |
| [RFC 4086](https://www.rfc-editor.org/rfc/rfc4086) | Randomness requirements and risks of low-entropy inputs | Context for assigning conservative, rather than speculative, entropy credits to process data. |

**Interpretation:** The standards are references for **cryptographic primitives, counter uniqueness, entropy-source engineering, and generator lifecycle**. The general-purpose mORMot implementation applies those principles in its own fast, portable construction. The normal application question is whether it provides a suitably unpredictable stream under a functioning cryptographic OS environment, rather than requiring callers to manage cryptographic internals.

## 3. Per-instance AES-counter state

### 3.1 Default configuration and key derivation

`TAesPrng.Create` initializes:

```pascal
fSeedAfterBytes := 256 shl 20;
fAesKeySize := 128 shl ord(HasHWAes);
fSeedNonce := 'mORMot AES-PRNG Seed';
```

Therefore:

- The default reseed threshold is **256 MiB**, a practical key-lifetime bound chosen for normal applications.
- The default is **AES-256 when hardware AES support is detected** and **AES-128 otherwise**. Both key sizes are widely used strong AES configurations; the conditional default is a deliberate speed/portability choice.
- The default nonce is a fixed application-specific personalization label. An application can choose a different `Nonce` and entropy mode for a privately created instance.
- Configuration properties are meant to be set before first use; they are **not** documented as atomic live-reconfiguration controls.

`Seed` requests **64 bytes** of conditioned entropy (`THash512Rec`) and initializes the AES engine from the first `fAesKeySize` bits. Another 64-bit portion (`e.q[7]`) is used to set the initial low half of the IV; the high half starts at zero. The 64-byte seed should **not** be interpreted as a claim of 512 bits of min-entropy. Security strength is bounded by the AES key size, actual seed unpredictability, and the overall construction.

### 3.2 Deterministic expansion

For a fixed AES key `K` and successive, unique 128-bit counter blocks `V_i`:

```
R_i = AES_Encrypt(K, V_i)
V_(i+1) = increment_big_endian(V_i)
```

A full 16-byte block is produced per AES encryption. A short request may consume a full final block and return only its requested prefix; the remaining bytes are not later reused. This intentionally favors a simple no-reuse rule over maintaining buffered remnants across public calls.

The software path uses `DoRndBlock`; the accelerated path may use the existing AES-NI/SSE implementation with multiple interleaved counters. Both are intended to implement the same counter progression, and cross-path equivalence belongs in release testing.

**Core invariant:** No two generated blocks in one AES-key epoch may use the same counter input. This is a necessary condition for AES-counter pseudorandom output; it is not, by itself, a full prediction-/backtracking-resistance guarantee.

### 3.3 Shared counter reservation and bulk generation

`FillRandom` has two processing modes:

- For requests of at most 16 full blocks (including small tails), it holds the instance lock while generating.
- For larger requests, it copies the AES context locally and **reserves** the required counter range under the lock; encryption proceeds using the private local context after unlocking.

The reservation size is `ceil(requested_bytes / 16)` blocks, and the shared IV is advanced before another request can reserve that region. The implementation uses a big-endian increment of the counter's upper 64-bit representation when reserving ranges; ordinary block generation advances the complete big-endian IV. These operations must agree on block ordering.

This design is defensible because one lock covers *allocation of disjoint counter ranges*, not all AES encryptions. Threads can finish in a different order from their reservation order; output ordering is not a security requirement, but **counter uniqueness is**. Local snapshots also allow an in-flight large request to finish using its original key when another thread reseeds the shared instance.

This is not a promise that a globally shared generator returns a reproducible sequence under scheduling changes.

### 3.4 Limits and accounting

`fBytesSinceSeed` and `fTotalBytes` are advanced by the bytes **consumed from the AES counter stream**, rounded to 16-byte blocks where applicable. They are not guaranteed to equal the sum of caller-visible output lengths.

`SeedAfterBytes` is checked **at the start of a request**; a request that crosses the threshold completes using its allocated AES-key epoch. The threshold is therefore a rekeying *trigger*, not a strict hard maximum of generated bytes per key.

Setting `SeedAfterBytes = 0` disables periodic automatic reseeding, but the first random request still initializes the instance. Such configuration removes the usual recovery and key-lifetime benefits and should require an explicit application rationale. Very large calls, counter exhaustion, 32-bit accounting overflow, and hostile concurrent configuration changes are outside the ordinary default usage profile and merit dedicated tests before changing these limits.

## 4. Entropy acquisition and conditioning

### 4.1 The OS source and three useful API contracts

`FillSystemRandom(Buffer, Len, AllowBlocking)` integrates native entropy providers. On Windows, it tries `ProcessPrng` where available and Windows system-generator fallbacks. On POSIX, it uses the available `getrandom` or device-based facilities and can optionally use blocking sources. The helper **returns `true` when the system request succeeds** and `false` when it falls back to the noncryptographic L'Ecuyer buffer-filling path. On some POSIX systems, requests larger than the native read limit have their OS-provided prefix expanded by a diffusion function; thus successful acquisition should not be described as one independent OS request per returned byte.

mORMot provides distinct behavior at the caller level:

| API | Source policy and failure behavior | Intended use |
| --- | --- | --- |
| `TAesPrng.Main.FillRandom` / `TAesPrng.Fill` | Fast, internally seeded AES-counter CSPRNG. First use automatically seeds the instance; later output is local and periodically reseeded. | **Default general-purpose cryptographic random bytes**. |
| `TSystemPrng.FillRandom` | Requests `FillSystemRandom` and **raises on OS-source failure** instead of treating the fallback buffer as a successful system result. | Direct OS-source random data. |
| `GetEntropy(..., gesSystemOnly / gesSystemOnlyMayBlock)` | Calls `FillSystemRandom` and **raises if that call reports failure**; does not mix the SHAKE stream. | Explicit direct system randomness with optional blocking. |
| `GetEntropy(..., gesSystemFast / gesSystemFull)` | Mixes system-oriented output with SHAKE256 output derived from process, OS and optional provider state, and an evolving entropy chain. | Normal conditioned seeding; `gesSystemFast` is the default. |
| `FillRandomTrusted(Len)` | Combines OS randomness and checked hardware RDRAND where available; if OS acquisition fails but checked RDRAND is available, fills first from this instance's CSPRNG and then XORs RDRAND; returns `''` if neither permitted source combination is available. | Initial secret material, particularly RSA prime generation. |

This split is intentional. **Routine random generation need not contact the OS for every call**, while security-critical initial key generation can demand the trusted-source combination without exposing extra configuration to the caller.

`FillSystemRandom` itself is a low-level interface; its Boolean status matters. On the default `gesSystemFast` path, the result is combined with a SHAKE-derived stream, not presented as a direct-system-only result. The normal security assumption is that a contemporary supported OS can supply strong seed material at startup and reseeds. If *all* unpredictable sources and prior generator state are unavailable or attacker-controlled, no deterministic construction can create secrecy from predictable input.

### 4.2 `TAesPrngGetEntropySource` selection

| Source | Actual behavior | Why it exists |
| --- | --- | --- |
| `gesSystemFast` | System-oriented bytes XORed with SHAKE256-conditioned framework/chain output | **Default**: a good balance of entropy diversity and reseed speed. |
| `gesSystemFull` | As above, with additional `XorOSEntropy` collection | Extra input diversity when its cost is acceptable. |
| `gesSystemOnly` | Direct system request; failure reported | Explicit operating-system trust boundary. |
| `gesSystemOnlyMayBlock` | Direct system request permitting blocking; failure reported | Callers prioritizing OS acquisition over responsiveness. |
| `gesUserOnly` | SHAKE256 expansion from framework state and the global entropy chain; may indirectly use OS entropy, but skips the initial output-length system fill | Specialized mode rather than the normal default. |

`TAesPrng.Seed` requests **64 bytes** from the selected mode, a small and practical system-source request. Its `fSeedEntropySource` defaults to `gesSystemFast`. For normal use, **do not change this property**.

### 4.3 SHAKE256 conditioning

For non-direct modes, `GetEntropy` initializes `TSha3` with `SHAKE_256` and absorbs, in order:

1. The executable hash (`Executable.Hash`).
2. The process-level `BaseEntropy` state.
3. The operating-system-related `SystemEntropy` state.
4. Optional output from a registered `OpenSslRandBytes` provider.
5. Additional `XorOSEntropy` material in modes other than `gesSystemFast`.
6. `XorEntropy` material gathered by core OS/base routines.
7. The caller-supplied `AppNonce` personalization string.
8. The previous 256-bit process-global entropy chain.

The implementation then squeezes a 256-bit next chain value and subsequently squeezes the requested SHAKE output. With `gesSystemFast` or `gesSystemFull`, that output is XORed into the system-oriented destination buffer; with `gesUserOnly`, it becomes the destination itself.

SHAKE256 is a sensible conditioning and expansion primitive: it can absorb variable-length, heterogenous input and produce an arbitrary requested number of pseudorandom-looking output bits. **It cannot create new entropy from predictable material**, however. Timestamps, process addresses, identifiers, and static executable information should not be assigned a claimed number of bits of security without source measurements and a threat model consistent with SP 800-90B.

#### Entropy XOR rationale and limits

For the hybrid modes, conceptually:

```
output = OS_stream XOR SHAKE256(mixed_inputs || chain || nonce)
```

XOR is an effective way to combine candidate random streams: an unpredictable independent component is sufficient for the combined output to remain unpredictable. The code deliberately obtains diverse inputs rather than trusting timestamps or process identifiers as cryptographic entropy on their own. SHAKE256 is the **conditioning/mixing primitive**, not an assertion that predictable input can create entropy. Even if an optional input is weak, incorporating it does not require lowering the quality of a sound independent stream.

### 4.4 Shared entropy chain

`_EntropyChain` is a process-global 256-bit state protected by `_EntropyChainSafe`. It is initialized using an OS random request when first required, added to the SHAKE absorb state, and replaced by a newly squeezed 256-bit value. Thus later entropy requests incorporate the evolving state, and `GetEntropy` calls cannot race over the chain-update step.

This ratchet is useful for mixing successive requests, reducing dependence on the same momentary collection of process information and complicating simple state replay. It does **not** automatically establish NIST prediction resistance or retroactive secrecy of an AES key whose state has been compromised. Such properties depend on when new unpredictable entropy enters, what an attacker has observed, and whether previous states have actually been erased.

`AppNonce` provides *personalization of the derivation input*, not an assertion that independently instantiated objects possess independent entropy sources. The same nonce may be reused, and the shared chain intentionally couples entropy requests within a process.

## 5. Rekeying and compromise analysis

### 5.1 Implemented `Seed` lifecycle

The sequence in `TAesPrng.Seed` is:

1. Ask `GetEntropy` for a fresh 64-byte conditioned buffer using the configured source and nonce.
2. Lock the AES instance.
3. Call `fAes.Done`, clearing the old AES context.
4. Initialize a new AES key using the configured AES key size.
5. Initialize the IV/counter from the remaining seed material and reset `fBytesSinceSeed`.
6. Unlock and wipe the temporary seed buffer.

The new seed is acquired **before** locking the instance, so slow OS gathering does not occupy the generator lock. The key replacement and state reset are serialized under that lock. Large calls that already reserved an old counter range operate from private local AES copies and can finish without interference from a concurrent `Seed`.

**Security rationale:** periodic rekeying limits long-term reuse of a single AES key and can help recover from a transient disclosure if later reseeding incorporates fresh unpredictable entropy. Exact resistance to entropy-failure, forced resets, VM rollback, and active fault injection requires additional assurance.

### 5.2 Backtracking resistance: exact boundary

An essential limitation compared with SP 800-90A DRBG mechanisms is that `TAesPrng` does **not** update the AES key after every generate request. It generates many requests with the same key until `Seed` is called.

Therefore a compromise of the **current key and counter state** may enable reconstruction of outputs from that same key epoch, to the extent the relevant counter inputs can be recovered or inferred. Erasing the previous key at a successful reseed can protect previous *key epochs* against compromise of the newly keyed state, assuming old copies have been eliminated and entropy/key derivation is sound. It is not correct to promise general *backtracking resistance for every generated request*.

Likewise, **prediction resistance** should not be claimed without fresh unpredictable entropy and the corresponding generate/reseed controls. A process-global SHAKE chain and periodic rekeying provide security-motivated mechanisms, but they do not equal the formal SP 800-90A prediction-resistance definition.

### 5.3 Operational limits, without unusual application tuning

- **Normal operation:** default `gesSystemFast` seeding draws on the supported OS and mORMot framework state. Applications do not need to collect additional entropy or select a particular AES key size.
- **Explicit OS failure behavior:** `TSystemPrng` and the direct `gesSystemOnly` modes raise on failure. `FillRandomTrusted` has its documented alternative checked-RDRAND/CSPRNG source combination and returns empty if no acceptable combination exists; RSA rejects that empty result.
- **Process cloning/snapshots:** a copied process state could contain the same AES key and counter. Reseed in forked or restored instances before using them independently in protocols requiring unique randomness. This is a runtime operational consideration, not a reason to change ordinary generator defaults.
- **Live settings and custom implementations:** configure newly constructed generators before concurrent use; a custom `TAesPrngAbstract` instance must preserve CSPRNG and thread-safety expectations.
- **Memory exposure:** wiping AES and seed temporaries reduces remnants but, as with other native cryptography, does not protect against arbitrary memory-reading malware or every core dump.

## 6. Helper methods and their practical guarantees

### 6.1 Use the least specialized API that meets the task

For ordinary application randomness, the recommended pattern is simply:

```pascal
TAesPrng.Fill(@buffer, SizeOf(buffer));
// or TAesPrng.Main.FillRandom(@buffer, SizeOf(buffer))
```

`FillRandomHex`, `FillRandomBytes`, `FillGuid`, `Random64`, `RandomDouble`, `RandomExt`, `RandomPassword`, and `RandomSalt` are convenience wrappers. `XorRandom` combines CSPRNG bytes with an existing buffer, a useful operation for mixing independent material or hardening an already initialized random candidate. The abstract generator interface is shared by the native, system and optional provider implementations.

For fresh external-source combinations, `FillRandomTrusted(Len)` has a different, explicit contract and is intentionally used by RSA prime generation. It attempts the system source; when `cfRAND` is available, it XORs RDRAND into the buffer. If the system request fails on a supported RDRAND machine, the buffer is initialized by `FillRandom` first. A checked hardware source is therefore supplementary when the OS works and forms a combined fallback with the CSPRNG when it does not. When that acceptable path is unavailable, it returns `''`.

`RdRand32` mixes complete 32-bit words. Any final 1–3 bytes of `FillRandomTrusted` remain populated by the initial system/CSPRNG fill; RSA's prime-candidate buffers are multiples of four. No application-level PRNG tuning is required to obtain this behavior.

### 6.2 Nonzero octets for RSA encryption padding

`FillRandomNotZero(Buffer, Len)` starts with a bulk random fill and replaces zero octets using further random bytes until no zero remains, with a bounded failure safeguard and final scan. Under independently uniform input octets and successful termination, this is uniform rejection sampling over **1..255**. That is appropriate for the `PS` field of RFC 8017's `EME-PKCS1-v1_5` encoding.

The helper is used by [`TRsa.Pkcs1Pad`](../../src/crypt/mormot.crypt.rsa.pas) to implement the nonzero, unpredictable type-2 padding required by RSA encryption. Randomized Miller-Rabin witnesses use the separate full-width `FillRandom`/rejection path, not this nonzero helper.

### 6.3 Bounded `Random32(max)` is not exact rejection sampling

The current implementation computes:

```pascal
result := (QWord(Random32) * max) shr 32;
```

For `max > 0` it maps uniformly distributed 32-bit inputs to `0..max-1` with bucket populations differing by at most one. It avoids a general division and is excellent for many practical randomized tasks, but **it is not mathematically exactly uniform** whenever `max` does not divide `2^32`. Setting `max=0` returns zero.

For a strict unbiased sampler, including standards that explicitly require uniform integers over a range, use rejection sampling from enough random bits rather than treating this multiply-high mapping as exact. This caveat is independent of the raw `FillRandom` output quality.

### 6.4 Statistical testing is not cryptographic validation

The presence of nonzero bytes, poor compressibility, a balanced split of low/high random samples, and unequal consecutive outputs are useful regression smoke tests, not evidence of an entropy-source min-entropy value or of formal DRBG security. This matters when interpreting the project's existing `Prng` tests.

## 7. Thread-safety and lifetime boundaries

### 7.1 What is synchronized

- The instance AES key schedule and shared counter state are updated or reserved under `fSafe`.
- Bulk requests use independent local AES contexts after counter reservation.
- Process-global chain transitions are serialized by `_EntropyChainSafe`.
- Initialization of `TAesPrng.Main` uses a global lock and registers the singleton for orderly shutdown.

These choices explain the performance goal and the intended uniqueness of allocated counter blocks without introducing a separate worker thread or request queue.

### 7.2 What callers must not assume

- Per-instance configuration mutations (`Nonce`, `SeedAfterBytes`, `AesKeySize`, `SeedEntropySource`) are **not** automatically safe while other threads use the same instance.
- The `TotalBytes` counter is accounting state, not a cryptographic security proof and not necessarily exact external output length.
- Custom `TAesPrngAbstract` subclasses must implement their own thread-safe `FillRandom` where needed; the abstract class itself is not a lock around every subclass operation.
- Multiple threads and independent processes do not yield reproducible request ordering; reproducibility is not a goal of a production CSPRNG.

The boundary between seed collection and lock acquisition also means that the configured 256 MiB threshold should be regarded as an approximately observed trigger rather than a strict atomic block cap under simultaneous callers.

## 8. Known test coverage and review evidence

### 8.1 Existing repository tests (source-inspected, not executed here)

The `_PRNG` / `Prng` routines of [`test/test.core.crypt.pas`](../../test/test.core.crypt.pas) cover:

- Creation of separate generator instances and checks that random blocks differ.
- Fixed-size blocks and lengths from 1 through 2,000 bytes.
- Hexadecimal encoding, password-character constraints, and `Random32` range checks.
- `FillRandomNotZero` output with no zero octets.
- Distribution smoke checks for `Random32`, `RandomDouble`, and `RandomExt`.
- `AFSplit`/`AFUnsplit` round-trips and cryptographic helper benchmarks.
- Conditional OpenSSL-backed PRNG comparison when built with that provider.

The SHAKE256 primitive also has known-answer reference tests in the core cryptographic test suite. These are valuable implementation regressions but do **not** validate the entropy collector as an SP 800-90B source or the generator as an SP 800-90A mechanism.

### 8.2 Source paths and invariants reviewed

The assessment covered:

1. `TAesPrngAbstract.GetEntropy`, the ordering of SHAKE256 absorption, serialized chain transition, and independent system-output XOR.
2. `TAesPrng.Create`, `Seed`, `LockAndPrepareFill`, all three `FillRandom` overloads, and scalar/accelerated `DoRnd`/`DoRndBlock` processing.
3. The explicit contracts of `FillRandomTrusted`, `FillRandomNotZero`, `XorRandom`, `TSystemPrng.FillRandom`, and typed helper methods.
4. Native `FillSystemRandom` integrations on Windows and POSIX, including their Boolean result and intentional fallback behavior, and whether the caller requires that result.
5. Existing project tests for randomized output, numerical bounds, nonzero padding, SHAKE256 vectors and generator benchmarks.
6. RSA usage of `FillRandomTrusted` for prime initial seeding and `FillRandom` for witness sampling, salts and encryption padding.

The review checked algorithm-level invariants and API behavior in source. It does not represent executable assembly validation or exhaustive concurrent/fault-injection testing.

### 8.3 Independent checks performed during this review

| Check | Method | Result and interpretation |
| --- | --- | --- |
| SHAKE256 empty-input reference | Compare independent Python `hashlib.shake_256(b'').digest(32)` with the public FIPS 202 reference output | Matched. This independently checks the known vector, **not** the Pascal SHAKE implementation. |
| Counter reservation arithmetic | Independent 12,000-request model with randomized lengths and ceiling-to-block allocation, totaling 6,125,348 blocks | Sequential reservation ranges had no overlap in the model. This verifies the arithmetic concept, **not** the full concurrent Pascal implementation or AES assembly. |
| `Random32(max)` mapping | Analyze `floor(x * max / 2^32)` and bucket cardinalities for 32-bit uniform `x` | Exact uniformity only when `max` divides `2^32`; otherwise one-count bucket differences. |
| Entropy-source contracts | Trace `FillSystemRandom` Boolean status through direct modes, `TSystemPrng`, `FillRandomTrusted` and default conditioned seeding | Direct-system callers raise on failure; trusted seeding accepts its documented OS or checked-RDRAND/CSPRNG combination; the normal mixed mode is intentionally distinct. |
| Reseed/backtracking boundary | Inspect whether the AES key changes after each request or only on `Seed` | It changes on `Seed`, so SP 800-90A-style per-request backtracking claims would be unsupported. |

**No Free Pascal/Delphi build or test execution was performed during this source-based review**, and no successful CI run was independently verified. Neither the independent mathematical models nor the existing test sources constitute a formal cryptographic security proof.

### 8.4 Tests to add for stronger release evidence

| Test area | Proposed assertion |
| --- | --- |
| Known-answer test of full AES-counter stream | Match output to a reference AES implementation for AES-128 and AES-256, including partial-block calls. |
| Small/large boundary | Verify 1, 15, 16, 17, 255, 256, 257 and larger requests across both generic and AES-NI paths. |
| Concurrent counter allocations | Inject deterministic seed/state and verify no duplicate `(key, counter)` pairs under mixed requests. |
| Reseed concurrency | Concurrent generation and `Seed`, with independent proof that old-context reservations remain disjoint. |
| Failure injection | Make OS entropy acquisition report failure; verify documented security policy and production handling. |
| Threshold boundaries | Check output just below/at/above `SeedAfterBytes`, including oversized single calls. |
| Nonzero-byte sampling | Inject zeros and nonzeros; check all outputs are nonzero or a controlled failure occurs. |
| Process clone / fork | Establish explicit fork handling and test rekeying before both parent and child emit new cryptographic output. |
| Entropy-source qualification | SP 800-90B min-entropy estimates, startup and continuous health tests if seeking an approved construction. |
| Platform matrix | FPC and Delphi on x86, x64, AArch64, Windows and POSIX; compare scalar and accelerated code. |

## 9. Implementation decisions and review conclusions

The assessment is organized by what the code actually guarantees for ordinary use, rather than measuring every design choice against a different certified DRBG interface.

| Design choice | Evidence and benefit | Boundary to preserve |
| --- | --- | --- |
| AES-CTR output with unique counter blocks | FIPS 197 AES primitive; counter-mode construction in SP 800-38A; serialized counter allocation | Never reuse `(key, counter)` across small, bulk, or concurrent calls. |
| SHAKE256-conditioned seed and 256-bit entropy chain | FIPS 202 XOF; diverse framework/OS inputs; serialized chain transition | Conditioning does not manufacture entropy from an entirely predictable environment. |
| AES-128 without hardware AES / AES-256 with hardware AES | Both standard AES key sizes; hardware-adaptive speed | Fixed 256-bit-key policy is a specialized configuration decision, not required for normal use. |
| Default reseeding at 256 MiB | Limits practical key lifetime and mixes subsequent entropy; automatic first-use seed | Compromise of the live key can expose earlier stream blocks from the same key epoch; no per-call backtracking promise. |
| Small-request locking, bulk counter reservation | Low overhead and parallel throughput; counters allocated while locked | Maintain disjoint ranges in all code paths and ensure reseed coordination. |
| `FillRandomTrusted` for RSA initial seeds | Explicit external-source acceptance, checked RDRAND mixing, CSPRNG fallback with checked hardware, empty-result failure | Do not reinterpret its contract as requiring *only* OS randomness. |
| Direct `TSystemPrng` and system-only modes | Native system-source integration, explicit failure reporting | For applications that specifically require direct OS randomness, use this dedicated interface. |
| Nonzero-byte rejection sampling | Generates PKCS#1 type-2 padding bytes from `[1..255]` without substitution bias | Keep rejection-based sampling for strict uniform-value requirements. |
| Convenience ranges and helpers | Efficient `Random32(max)` multiply-high mapping, string/hex/UUID utilities | Exact uniformity for arbitrary ranges requires a separate rejection sampler; the mapping has only one-count bucket imbalance. |

The reviewed design is **appropriate for an everyday CSPRNG under the standard assumption of a functioning supported OS cryptographic entropy environment**. No application-specific nonce, AES key size, reseed threshold, or entropy mode is needed to obtain the intended defaults. Further guarantees about host compromise, VM cloning, local side channels, or certified cryptographic-module profiles are different deployment questions.

## 10. Choosing the right API: default-first guidance

| Need | Recommended interface | Configuration |
| --- | --- | --- |
| Nonce/IV in a normal mORMot application | `Random128()` (unpredictable output) | **None**. First-use seeding and periodic reseeding are automatic. |
| Key/random bytes in a normal mORMot application | `TAesPrng.Fill` or `TAesPrng.Main.FillRandom` | **None** |
| Fresh seed for native RSA key generation | `TRsa.GenerateNew` (internally uses `FillRandomTrusted`) | **None** beyond choosing a key size if required. |
| Exactly nonzero cryptographic octets | `FillRandomNotZero` on a `TAesPrngAbstract` instance | None. |
| Direct OS random output and explicit OS-error reporting | `TSystemPrng.Main.FillRandom` or `GetEntropy(..., gesSystemOnly...)` | Only select if this API contract is specifically needed. |
| Extra OS/process collection on reseeds | `gesSystemFull` for a privately owned `TAesPrng` instance | Optional and generally unnecessary. |
| Deterministic test vectors / alternative provider | Inject an appropriate custom `TAesPrngAbstract` or local test-specific instance | Test/deployment-specific only. |

These methods coexist because **performance and origin of randomness are different application needs**. The default generator is intentionally optimized for frequent calls, while trusted or direct-system operations are explicitly available without forcing their overhead onto every call.

### 10.1 Practical operating assumptions

A general-purpose native cryptographic library assumes the OS can provide a strong initial random seed, the process keeps its secrets, and cryptographic state is not cloned and then reused independently. Normal Windows/POSIX applications satisfy the first assumption in ordinary operation. After a fork, snapshot restore, or secret-state disclosure, the application should establish an appropriately fresh state before generating security-sensitive outputs; this is a standard deployment rule for stateful CSPRNGs.

No routine caller must compensate for this design with homemade random seeding, custom hash mixing or ad-hoc changes to `AesKeySize`, `Nonce`, or `SeedAfterBytes`.

## 11. Conclusion and maintenance criteria

`TAesPrng` follows a coherent and efficient CSPRNG design: established AES and SHAKE256 primitives, source-diverse seed material, a serialized secret entropy chain, automatic reseeding, and disjoint counter allocation for multithreaded requests. Its architecture gives a technically defensible basis for cryptographic pseudorandomness **with the normal assumption of an unpredictable OS-backed seed and secret AES state**, while delivering the library's priorities: **portable native Pascal, excellent throughput, and defaults that work without knobs**.

A few distinctions strengthen rather than weaken that conclusion: `FillSystemRandom` communicates OS acquisition status; direct-system APIs raise when acquisition fails; the default hybrid entropy mode mixes additional conditioned state; and `FillRandomTrusted` defines the accepted multi-source combination used for RSA prime seeds. `TAesPrng` is a mORMot-specific AES-counter CSPRNG rather than the exact SP 800-90A CTR_DRBG state machine. That standards classification is separate from the ordinary engineering assessment of the implementation.

Future changes should preserve five invariants:

1. **Counter uniqueness:** maintain nonoverlapping AES input blocks for one key, including partial output, bulk calls, reseeding and simultaneous threads.
2. **Seed unpredictability and source semantics:** preserve the explicit failure behaviors of direct-system and trusted-source APIs, and the conditioning behavior of the default mode.
3. **Reasoned lifetimes:** keep first-use seeding, periodic key replacement, and best-effort erasure of retired contexts and temporary seeds.
4. **Portable equivalence:** compare Pascal, x86/x64/AArch64 and accelerated AES paths against common reference vectors and concurrency tests.
5. **Useful API contracts:** leave ordinary cryptographic random-byte generation fast and easy; reserve more specialized entropy-source selection for the few callers who actually need it.

**Operational result:** for general-purpose mORMot applications, **`TAesPrng` defaults are the recommended starting point**. The standard crypto primitives, deliberate state lifecycle, and source-aware RSA integration make extra random-source configuration unnecessary for normal workloads.

---

## References

1. **mORMot2**, [`mormot.crypt.core.pas` at `efdf73f3e2`](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/crypt/mormot.crypt.core.pas), [`mormot.core.os` POSIX code](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/core/mormot.core.os.posix.inc), and [Windows code](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/core/mormot.core.os.windows.inc).
2. **mORMot2**, [cryptography tests](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/test/test.core.crypt.pas) and [related RSA audit](mormot.crypt.rsa.md).
3. **NIST**, [FIPS 197 - Advanced Encryption Standard](https://csrc.nist.gov/pubs/fips/197/final).
4. **NIST**, [FIPS 202 - SHA-3 Standard](https://csrc.nist.gov/pubs/fips/202/final).
5. **NIST**, [SP 800-38A - Recommendation for Block Cipher Modes of Operation](https://csrc.nist.gov/pubs/sp/800/38/a/final).
6. **NIST**, [SP 800-90A Rev. 1 - Deterministic Random Bit Generators](https://csrc.nist.gov/pubs/sp/800/90/a/r1/final).
7. **NIST**, [SP 800-90B - Entropy Sources for Random Bit Generation](https://csrc.nist.gov/pubs/sp/800/90/b/final).
8. **NIST**, [SP 800-90C - Random Bit Generator Constructions](https://csrc.nist.gov/pubs/sp/800/90/c/final), final September 2025.
9. **NIST**, [FIPS 186-5 - Digital Signature Standard](https://csrc.nist.gov/pubs/fips/186-5/final), including the approved-DRBG prerequisites for RSA prime testing.
10. **IETF**, [RFC 4086 - Randomness Requirements for Security](https://www.rfc-editor.org/rfc/rfc4086).
11. **IETF**, [RFC 8017 - PKCS #1: RSA Cryptography Specifications Version 2.2](https://www.rfc-editor.org/rfc/rfc8017.html), for randomness requirements of RSA encryption padding and PSS.
