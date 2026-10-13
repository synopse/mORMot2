# `mormot.crypt.rsa` — RSA Design, Security Rationale, and Implementation Review

**Scope:** mORMot2 native RSA engine for Delphi and Free Pascal; cryptographic algorithms, implementation invariants, and production usage.  
**Source snapshot:** mORMot2 **2.4.17599**, [`efdf73f3e20f`](https://github.com/synopse/mORMot2/tree/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527) (13 October 2026).  
**Implementation:** [`src/crypt/mormot.crypt.rsa.pas`](../../src/crypt/mormot.crypt.rsa.pas) ([reviewed source](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/crypt/mormot.crypt.rsa.pas)).  
**Supporting code and tests:** [`mormot.crypt.core.pas`](../../src/crypt/mormot.crypt.core.pas), [`test.core.crypt.pas`](../../test/test.core.crypt.pas).  
**Companion CSPRNG design:** [`TAesPrng.md`](TAesPrng.md).

> **Engineering conclusion.** The built-in RSA engine uses well-established number theory, probabilistic primality testing, standards-based PKCS#1 encodings, and a CSPRNG-backed generation path. Its defaults are intended to be useful **without application-specific tuning**: RSA-2048 generation, exponent 65537, appropriate primality-test rounds, fresh cryptographic randomness, fixed-width signatures, and high-level signature self-verification. The optimized arithmetic and cross-platform Pascal/assembly implementations support mORMot's priorities: **safe and unsurprising defaults, portability, and speed**.
>
> This document records how those design choices work, the applicable portions of FIPS 186-5 and RFC 8017, source inspection and independent algorithm checks, and the separate responsibilities of callers using legacy encryption formats or special-purpose deployment policies. It is an engineering security review, not a replacement for production testing.

## 1. Scope, goals, and trust boundaries

`mormot.crypt.rsa` supplies a native Pascal RSA engine for Delphi and Free Pascal. Its responsibilities include:

| Component | Responsibility | Security-relevant interface |
| --- | --- | --- |
| `TBigInt` | Non-negative multiprecision integers, arithmetic, primality tests, candidate generation | `IsPrime`, `FillPrime`, `PrimeAllowedFor65537`, `Release` |
| `TRsaContext` | BigInt ownership and free list, cached moduli, Barrett reduction, exponentiation | `Allocate`, `Reduce`, `ModPower`, `WipeReleased` |
| `TRsa` | RSA key pairs, CRT operations, RSASSA-PKCS1-v1_5, RSAES-PKCS1-v1_5, hybrid envelope | `GenerateNew`, `CheckPrivateKey`, `Sign`, `Verify`, `Seal`, `Open` |
| `TRsaPss` | RSASSA-PSS encoding, signing, verification | `Sign`, `Verify` |
| `TRsaPrimeWorker` | Independent concurrent generation of one RSA prime | `Execute`, `Destroy` |
| Crypto catalog adapters | Framework integration and algorithm selection | `RS256`/`RS384`/`RS512`, `PS256`/`PS384`/`PS512` and internal aliases |

The audit assumes a trusted host process, sufficiently unpredictable operating-system entropy, correct compiler/runtime and platform integer semantics, and protected private-key storage. Inputs such as ciphertexts, public keys, signatures, and externally supplied DER/PEM/JWK material may be untrusted. Physical compromise, malicious native code running in the process, and active control of the entropy source are outside the promised protections.

`RS256-int` and `PS256-int` expose this engine even if the default cryptographic catalog is configured to use OpenSSL. Testing the intended backend is essential.

### 1.1 What the default contract provides

- **Mathematical correctness:** BigInt arithmetic, CRT reconstruction, probable-prime generation, modular inverses, and consistent RSA key parameters.
- **Standards-oriented encodings:** RSASSA-PKCS1-v1_5 and RSASSA-PSS implementations following RFC 8017; deterministic v1.5 encodings are compared exactly.
- **Responsible cryptographic randomness:** `FillRandomTrusted()` supplies the initial prime-candidate seed from suitable external/CSPRNG sources; `TAesPrng` supplies randomized Miller–Rabin witnesses, PSS salts, and nonzero encryption-padding bytes.
- **Fault-detection at the correct layer:** High-level `Sign` verifies the signature before returning it; `ChineseRemainderTheorem` remains a focused mathematical primitive.
- **Low configuration burden:** Applications can normally use `TRsa.GenerateNew`, `Sign`, and `Verify` directly. Explicit tuning is relevant only when key size, algorithm profile, interoperability or attack model requires a different policy.

The engine does not attempt to make **every** low-level primitive a protocol-level security boundary. Protocol authentication, key-purpose policies, protection from decryption oracles, and physical side-channel assurance belong to the appropriate calling layer. This separation keeps the core API small and the hot arithmetic paths fast.

## 2. Standards and the intended security profile

**Primary references** are [FIPS 186-5](https://doi.org/10.6028/NIST.FIPS.186-5), especially §5 and Appendices A.1, B.3, and C; [RFC 8017](https://www.rfc-editor.org/rfc/rfc8017.html); [NIST SP 800-90A Rev. 1](https://doi.org/10.6028/NIST.SP.800-90Ar1); [NIST SP 800-57 Part 1 Rev. 5](https://doi.org/10.6028/NIST.SP.800-57pt1r5); and [NIST SP 800-56B Rev. 2](https://doi.org/10.6028/NIST.SP.800-56Br2). The [Handbook of Applied Cryptography, chapter 4](https://cacr.uwaterloo.ca/hac/about/chap4.pdf) provides mathematical background for primality tests and candidate searches.

**Key-purpose distinction:** FIPS 186-5 is a *digital-signature* standard. Its §5.1 separates signature keys from key-establishment keys. `TRsa` deliberately supports both signing and legacy/interoperable RSA encryption operations as different **API capabilities**, without implying that the same key pair should be reused for both purposes. NIST SP 800-56B addresses key establishment separately.

**Key sizes:** FIPS 186-5 §5.1 specifies an **even** signature-modulus bit length of **at least 2048** and equal-length `p` and `q`. `TRsa.Generate` deliberately supports `512` and `1024` for legacy/testing compatibility, besides `2048`, `3072`, `4096`, `7680`, and `8192`. Such smaller sizes are **outside** the FIPS signing-key profile and are unsuitable for new production keys. Default generation uses RSA-2048; applications can select RSA-3072 or larger for a different security/performance trade-off. Strength also depends on the chosen signature hash (FIPS 186-5 §5.1; SP 800-57).

### 2.1 Requirements traceability

| FIPS / standards reference | Source design and audit interpretation |
| --- | --- |
| FIPS 186-5 §5.1, Appendix A.1.1: `n = p*q`, equal prime lengths | Two half-modulus-length prime candidates; the final modulus and CRT parameters are assembled from them. |
| Appendix A.1.1(1): odd `2^16 < e < 2^256` | Fixed `e = 65537`, chosen before generating primes: a permitted fixed exponent with efficient public operations. |
| Appendix A.1.1(2)(a): `gcd(e,p-1) = gcd(e,q-1) = 1` | Dedicated `PrimeAllowedFor65537` rejects prime candidates congruent to 1 modulo `e`; a subsequent full GCD check provides an additional invariant. |
| Appendix A.1.1(2)(b,c): primes in the upper `sqrt(2)` half-range | `FIPS_MIN = $b504f334` constrains the most-significant 32 bits of candidate primes. The constant is the ceiling of `sqrt(2) * 2^31`; the prefix test realizes a conservative discrete bound. |
| Appendix A.1.1(2)(d): `abs(p-q) > 2^(nlen/2-100)` | Rejects candidates whose difference has insufficient `BitCount`; requires distinct primes and normalizes ordering `p > q`. |
| Appendix A.1.1(3): `d = e^-1 mod lcm(p-1,q-1)` and `d > 2^(nlen/2)` | Computes the least positive modular inverse and checks the private exponent's bit length, retrying otherwise. |
| Appendix A.1.1(A.2), A.1.3: generation using probable primes | Uses random initial candidates and probabilistic primality tests; an engineering search strategy rather than a verbatim copy of the prescribed A.1.3 generation procedure. |
| Appendix B.3: optional trial division | `MatchKnownPrime` quickly excludes small factors, reducing expensive modular exponentiations. |
| Appendix B.3, Table B.1: Miller–Rabin rounds | `FipsMinIterations` enforces 5 randomized rounds for 1024-bit factors, 4 for 1536-bit factors, and at least 4 for larger factors. Coverage at non-tabulated sizes is a policy approximation, not independent proof of conformance to Appendix C.1. |
| Appendix B.3.1: `wlen`-bit random witnesses in `[2,w-2]` | `FillMillerRabinWitness` draws full-bit-length values, masks unused bits, and uses unbiased rejection sampling. Requires the supplied generator to provide cryptographically strong uniform output. |
| FIPS 186-5 §5.4; RFC 8017 §§8–9 | Signature schemes are implemented as PKCS#1 v1.5 and PSS, with matching encoding and decoding rules. |
| FIPS 186-5 §5.1: CRT form of private key allowed | `TRsa` maintains `dP`, `dQ`, and `qInv` and uses CRT to avoid a full-size private modular exponentiation. |

The table documents the mathematical and algorithmic correspondence of the implementation. FIPS 186-5 addresses digital signatures, whereas this general-purpose library also provides encryption and historical compatibility functions; deployments requiring a particular regulatory profile impose those additional rules separately.

## 3. BigInt representation and arithmetic architecture

### 3.1 Data representation, ownership, and wiping

`TBigInt` stores unsigned integers in little-endian **16-bit limbs on CPU32** and **32-bit limbs on CPU64**, using `HALF_BITS`, `HALF_BYTES`, and `RSA_RADIX` abstractions. Each `PBigInt` belongs to a `TRsaContext`; values must not be moved between contexts as bare pointers. `TRsaPrimeWorker` instead transmits a fixed-width serialized prime.

The context caches freed objects and associated arithmetic buffers for speed. `Copy`/`Clone`, `RefCnt`, and `SetPermanent` encode the sharing/lifetime contract; several arithmetic methods consume/release their operands, a detail that must be respected in every caller. `Trim` normalizes the limb count before size-sensitive comparisons. `ResetPermanentAndRelease`, `WipeReleased`, context destruction, and `TRsaPrivateKey.Done` contribute best-effort secret erasure. Reuse improves throughput and reduces heap churn, while wiping reduces residual exposure; neither prevents swap, crash dumps, or copies made outside the unit.

**Review invariant:** active, free-list, and permanent objects must remain balanced on successful, composite, rejected, and timeout paths. The regression suite checks reference counts and `ActiveCount` around primality testing.

### 3.2 Modular reduction

`TRsaContext.SetModulo` may cache a normalized modulus and a Barrett reciprocal

```
mu = floor(B^(2k) / modulus),  B = RSA_RADIX, k = limb count.
```

`TRsaContext.Reduce` selects Barrett reduction only when the operand-size precondition holds; otherwise it delegates to the general division path. `BarrettReduce` deliberately computes only the significant part of a multiplication and corrects the approximate quotient by a bounded number of subtractions. Its implementation comments describe why the truncation error is limited. This is a **performance transformation of exact modular arithmetic**, not an approximate result accepted for cryptographic use.

The independent-division path is valuable both as a correctness fallback and as an oracle for test comparisons. The in-repository `RsaBarrett` and `RsaMillerRabin` tests exercise reduction/exponentiation equivalence across several operand sizes and cached/noncached moduli. These native tests were *reviewed*, not executed in the verification described below.

### 3.3 Exponentiation and processor-specific optimizations

`TRsaContext.ModPower` supports:

1. A **65537 fast path**: sixteen squarings and one multiplication (`65537 = 2^16 + 1`). This is especially valuable for public verification and the public-operation half of RSA self-checking.
2. **Binary exponentiation** for short exponents: avoids setting up a large sliding-window table unnecessarily.
3. A **four-bit sliding window** for larger exponents: amortizes exponent-bit handling by precomputing odd powers.
4. Dedicated `Square`, multiplication/reduction primitives, and x86/x64/AArch64 assembly where enabled, with portable Pascal alternatives.

These choices improve performance without changing the modular exponentiation identity. The bounded Barrett path is reused during Miller–Rabin to avoid reconstructing expensive reciprocals for every witness.

**Security boundary:** the private-exponent code is not documented or established as constant-time. Sliding-window selection, variable-length arithmetic, and division may have secret-dependent execution characteristics. Claims of protection from local timing/cache attacks require a separate side-channel review; ordinary algorithmic tests do not establish this property.

## 4. Prime candidate generation

`TBigInt.FillPrime` searches for **probable primes** from a cryptographically seeded starting value. It obtains an initial full-width seed using the supplied `TAesPrngAbstract.FillRandomTrusted(bytes)` method, applies `AFDiffusion` to the starting BigInt, and adds CSPRNG output with `XorStrongRandom`. The method makes the integer odd and enforces the FIPS-inspired high-prefix interval. It then tests consecutive odd candidates, advancing by two until a probable prime is accepted or the configured timeout expires.

**Why these choices work together:**

1. `FillRandomTrusted` attempts a system random source and additionally combines checked hardware RDRAND on supported Intel/AMD CPUs. If the system source reports failure but checked RDRAND is available, an instance CSPRNG fill precedes the RDRAND XOR; if neither permitted source combination is available, the method returns empty and RSA prime generation raises instead of proceeding with an untrusted seed.
2. `AFDiffusion` spreads input material across the candidate representation; `XorStrongRandom` contributes additional CSPRNG bytes and preserves the odd/high-bit candidate constraints.
3. Candidate incrementing avoids reinitializing a full-width seed on every trial, an established optimization for probabilistic prime searches (HAC §4.5). The randomized Miller–Rabin test is still applied to the candidate that passes the cheap filters.
4. Timeouts and repeat/rejection conditions prevent a generator from silently accepting unsuitable factors.

This is a practical, cross-platform probable-prime search **guided by** the mathematical requirements of FIPS 186-5 Appendix A.1 and the primality-testing guidance of Appendix B. It is not necessary to require developers to configure iteration counts or an entropy source for ordinary key generation; the defaults are chosen within the implementation.

### 4.1 Trial division and early composite rejection

`MatchKnownPrime` provides configurable sieving (`bspFast`: primes below ~256; `bspMost`: below ~2000, the default; `bspAll`: below ~18000). Removing easy composites before modular exponentiation is an established performance choice. FIPS 186-5 Appendix B.3 explicitly permits an optional trial-division pretest; it does not demand trial division of every candidate.

`IsPrime` also applies a strong **base-2** probable-prime pretest before the randomized Miller–Rabin rounds. This is an additional cheap composite filter, **not** a substitute for the required randomized bases. The number of additional iterations remains controlled by `Iterations` and the enforced generation minimum.

### 4.2 Miller–Rabin witness selection

For odd candidate `w`, the implementation computes `W = w - 1`; the witness must satisfy

```
2 <= witness <= w - 2, equivalently 1 < witness < W.
```

`FillMillerRabinWitness` requests `BitCount(W)` random bits, clears excess high bytes, masks unused high bits in the highest limb, calls `Trim`, and rejects values outside that interval. For odd candidates greater than two, `BitCount(W) = BitCount(w)`, so the sampled bit width is the `wlen` required by FIPS 186-5 B.3.1. Parity is **not** forced: both even and odd witnesses are valid. Because rejection is used instead of reduction modulo an interval, the accepted values are uniform if the underlying bits are uniform.

The code uses the full witness width, correctly clears/masks unused high bits, and permits up to 128 attempts as a defensive stop. The rejection probability at supported prime sizes makes this limit extremely unlikely to be reached with a working generator. It is an inexpensive safeguard against an unexpectedly broken randomness provider.

### 4.3 Number of rounds and error interpretation

`FipsMinIterations` floors caller-supplied generation iterations to the configured minimum. For the common FIPS-size choices, the mapping corresponds to the stronger entries of **FIPS 186-5 Table B.1**:

| RSA modulus | Prime factor length | Random Miller–Rabin rounds selected | Table B.1 error-probability entry |
| ---: | ---: | ---: | --- |
| 2048 | 1024 | 5 | `2^-112` |
| 3072 | 1536 | 4 | `2^-128` |
| 4096 | 2048 | 4 | `2^-144` |

These tabulated bounds apply under the assumptions in FIPS Appendix B/C on how **random candidates** are generated. They must not be transplanted uncritically as a general guarantee for arbitrary adversarially chosen integers. The code also allows extra rounds and executes the separate base-2 strong test. Factor lengths beyond the tabulated cases reuse a four-round lower bound; FIPS directs implementations to Appendix C.1 for sizes not appearing in the table.

The optional Lucas test discussed in FIPS Appendix B.3/C is not implemented as part of this path. FIPS describes Miller–Rabin-only testing as an alternative, so absence of a Lucas test is not, on its own, a defect.

### 4.4 Default random source, optional injection, and trusted seeding

The instance `Drbg` property accepts any `TAesPrngAbstract`; in ordinary use, `TRsa.GetDrbg` initializes `fDrbg` to `TAesPrng.Main`. `TRsa.Generate` performs that initialization before launching the prime worker, which uses `Owner.fDrbg` directly. This stateful initialization is intentional and important to the worker contract.

The **initial prime seed** has a stronger external-source policy than routine witness sampling: `FillRandomTrusted(bytes)` combines a successful system request with checked RDRAND where available, or uses the library CSPRNG plus checked RDRAND when the OS source fails. If neither acceptable condition is satisfied, key generation stops. This source selection is already part of the default RSA generation algorithm; callers need not supply extra entropy themselves.

**Subsequent operations** use the normal `TAesPrngAbstract.FillRandom` CSPRNG interface for probabilistic witnesses and candidate mixing. This is appropriate because Miller–Rabin requires fresh uniformly sampled witnesses, not repeated operating-system calls for every witness. Custom `Drbg` injection is primarily for testing or explicit application policy. See [`TAesPrng.md`](TAesPrng.md) for the distinct guarantees of `FillRandom`, `FillSystemRandom`, `TSystemPrng`, and `FillRandomTrusted`.

FIPS 186-5 Appendix B.3.1 specifies witness generation using an approved DRBG for applications claiming that formal profile. The mORMot engine follows the relevant **witness selection algorithm** while remaining a general-purpose cross-platform RSA implementation.

## 5. Prime/exponent constraints and the 65537 optimization

The public exponent is fixed to `65537`, a Fermat prime satisfying FIPS A.1.1's odd-exponent range and permitting especially efficient signature verification. Candidate primes congruent to 1 modulo 65537 cannot be used because `gcd(e, p-1)` would then exceed one.

`TBigInt.PrimeAllowedFor65537` tests this condition without creating a temporary remainder BigInt. Its algebraic basis is

```
2^16 == -1 (mod 65537)
2^32 ==  1 (mod 65537)
```

For 16-bit limbs, the method sums alternating limbs; for 32-bit limbs, it sums low half-words and subtracts high half-words. The candidate is rejected when `(sum - 1) mod 65537 = 0`. The signed accumulator is essential because the alternating sum can be negative. Its magnitude is bounded well inside `PtrInt` for all supported RSA sizes.

**Arithmetic justification and review:** The modulo identity holds exactly for 16-bit or 32-bit little-endian limbs. Because alternating sums can be negative, the method uses a signed `PtrInt` accumulator. The reviewed implementation was cross-checked with independent integer arithmetic and a signed negative-sum regression case; both the equivalence and signed-width bounds held for supported RSA sizes (see §11).

The test is performed **after** a candidate has passed `FillPrime`, not on every trial odd integer. Consequently, the overall key-generation speedup is expected to be modest; the simpler, allocation-free implementation has value, especially on CPU32 where `65537` occupies two limbs. The later complete GCD check in `TRsa.Generate` remains useful as a separate consistency guard.

## 6. Parallel key generation and RSA private parameters

`TRsa.Generate` generates `p` and `q` concurrently when at least two processors are reported, or sequentially otherwise. The worker owns a separate `TRsaContext`; transferring `NewPrime` as fixed-width bytes prevents sharing context-owned BigInt pointers. The main thread waits for the worker before consuming its result, and the worker destructor wipes the serialized prime. A timeout bounds work on slow devices, while `GenerateNew` handles its documented limited retry behavior.

The main generator enforces:

- A fixed public exponent selected before the primes, with exponent-related candidate rejection.
- Distinct prime factors, sorted so `p > q` for the CRT layout.
- A minimum distance `|p-q|` using `BitCount` of the absolute difference.
- Relatively prime public exponent and both factors' decremented values (including a full GCD check).
- `d` as the modular inverse of `e` modulo `lcm(p-1, q-1)`, with an adequate lower bit-length threshold.
- CRT parameters `dP = d mod (p-1)`, `dQ = d mod (q-1)`, `qInv = q^-1 mod p`, and `n = p*q`.

The prime-pair checks mirror the mathematical constraints of FIPS 186-5 Appendix A.1.1. Choosing the **least positive** modular inverse minimizes ambiguity and matches the usual RSA representation. CRT representation is explicitly permitted by FIPS 186-5 §5.1; RFC 8017 also describes the multi-prime/CRT structures.

The code accepts a final modulus whose `BitCount` is either the requested bit length or one bit less. The high-prefix bound should normally imply the requested full length when both prime ranges hold, but this permissive return-path condition should not be mistaken for a strict check of the exact FIPS-requested `nlen`. Auditors targeting formal conformance should evaluate this explicitly rather than infer compliance from an input parameter.

### 6.1 Why a separate worker context matters

The free lists, cached normalization data, active-object counts, and reference-counting contract of `TRsaContext` are not designed for concurrent mutation by unrelated tasks. One context per prime worker and a serialized handoff avoid synchronization around hundreds of BigInt operations. The shared DRBG must be thread-safe, and custom injected generators must meet the same concurrency/lifetime expectations.

### 6.2 Why timeout and cleanup are part of the design

Large RSA keys can be expensive to generate. A bounded timeout avoids unbounded service stalls; the worker's `Destroy` wipes `NewPrime`, including paths where the main thread did not consume it. Generation retries also release a previously allocated `_p` before starting a new pair. Released BigInts are scrubbed through the existing context machinery. Those protections reduce cleanup mistakes without adding modular arithmetic to the hot path.

## 7. Private-key consistency and CRT computation

`TRsa.CheckPrivateKey` verifies the **coherence of stored RSA parameters**, including `p*q = n`, the public exponent, CRT coefficients, `dP/dQ`, GCD and modular-inverse relationships. It is intentionally **not** a complete rerun of prime generation and does not call costly randomized primality tests on every stored factor. This keeps an often-used consistency API economical. An application requiring provenance or independent primality assurance for imported factors must establish that separately.

`TRsa.ChineseRemainderTheorem` computes the standard private RSA operation modulo `p` and `q`, then recombines using `qInv`. Keeping it a focused **computation primitive**, rather than adding public verification or policy checks inside it, preserves its responsibility and performance. Its callers are responsible for operation-specific encoding, decoding, locking, and any additional policy.

A corrupted CRT signature can in some fault models reveal a prime factor. Instead of imposing fault verification on every CRT operation, the current high-level **signing** APIs verify their own completed signatures (§8). RSA decryption has a different threat model; RSA blinding and padding-oracle defenses are separate considerations, not already proven by the CRT routine.

## 8. RSA signature schemes and self-verification

### 8.1 RSASSA-PKCS1-v1_5 (`TRsa`)

`RsaSignHashToDer` constructs the ASN.1 DER `DigestInfo` with the selected hash algorithm identifier. `Pkcs1Pad` uses block type 1 for deterministic signature encoding, and `Pkcs1Sign` produces a **fixed-width `fModulusLen` signature**. Verification rejects signature representatives outside the modulus and compares the recovered `DigestInfo` with a freshly generated expected encoding, rather than accepting partially parsed or trailing ASN.1 contents. This implements a deliberately strict encoding profile. See RFC 8017 §§8.2 and 9.2.

The high-level `TRsa.Sign` now calls `Verify` **unconditionally** after signing, raising an exception on mismatch. This catches accidental or certain fault-induced incorrect signatures *before they are returned*. The public exponent `65537` makes this check relatively cheap. A benchmark cited during review was approximately **500 signatures/s versus 24,000 verifications/s on x64**; those are conversation-supplied measurements, not independently rerun results. The expected added work is therefore only a few percent in that configuration.

The low-level public `Pkcs1Sign` method does **not** automatically perform this post-check. The documented guarantee belongs to the high-level `Sign` operation, not every raw private RSA primitive.

### 8.2 RSASSA-PSS (`TRsaPss`)

The PSS variant uses MGF1 and the **same selected hash** for message hashing and mask generation, in line with RFC 8017 §8.1's recommendation. It generates fresh salt from `GetDrbg`, choosing **salt length equal to the digest length** as a defined interoperability profile. The implementation handles the `modBits - 1` encoded-message convention, checks the trailing `$bc` octet and the unused high bits, recovers the salt, and recomputes the expected salted hash.

The final signature uses `SaveFixed(fModulusLen)` to meet RFC 8017's **exact `k`-octet output** requirement even when the leading bytes of the signature integer are zero. As with PKCS#1 v1.5, high-level `TRsaPss.Sign` performs **unconditional self-verification after releasing `fSafe`**, avoiding recursive acquisition of a non-reentrant lock.

The signer/verifier deliberately uses a fixed salt-length profile. Interoperability testing should state this choice explicitly because RFC 8017 allows other configured PSS salt lengths.

### 8.3 What signing self-verification does not establish

The self-check is effective detection of incorrect signatures before publication; it is not a separate constant-time or physical fault-resistance guarantee. It also does not protect `Pkcs1Decrypt` from chosen-ciphertext attacks. Signer input hashes must be produced with an appropriately strong hash under the application's algorithm policy; supporting a hash algorithm in an enum does not automatically approve every algorithm for every FIPS usage.

## 9. RSA encryption, random padding, and EVP-style envelopes

`TRsa.Pkcs1Encrypt` / `Pkcs1Decrypt` implement **RSAES-PKCS1-v1_5** (RFC 8017 §7.2), with EME type-2 padding for encryption. Unlike signatures, those padding bytes must be unpredictable and nonzero. `Pkcs1Pad` obtains them from `GetDrbg.FillRandomNotZero`, which uses cryptographic random bytes and resamples zero octets instead of introducing substitution bias.

`TRsa.Seal` / `Open` wrap an ephemeral AES key and IV in the project's established **OpenSSL EVP_SealInit/EVP_OpenInit-compatible envelope**: transport the ephemeral AES key using RSA and encrypt the message with the caller-selected symmetric cipher (default `aes-128-ctr`). This is an intentional **compatibility and deployment design**, not an accidental omission of OAEP/GCM. The OpenSSL EVP API specifies the envelope operations; the particular serialized header is a mORMot/OpenSSL-wrapper contract, not a universal EVP wire format.

**Security boundaries of that protocol:**

- RSAES-PKCS1-v1_5 is sensitive to padding-oracle designs. `Pkcs1UnPad` uses early returns, and the application must not expose fine-grained validity, timing, or error distinctions as a decryption oracle. Neither the newer CSPRNG padding nor signature self-verification eliminates that class of risk.
- AES-CTR provides **confidentiality without authentication**. Tampering is not cryptographically detected by CTR alone. An external MAC/authenticated protocol layer is needed if integrity is required; a new AEAD/OAEP envelope would be a separate versioned protocol and should not silently replace the existing compatible representation.
- RSA key transport and RSA signatures have different standards and permitted key-usage policies. A FIPS 186-5 signature key pair must not be used for key establishment under the FIPS signature profile.

These are **documented use constraints**, not requests to break the existing public API.

## 10. Serialization, key validation, concurrency, and secrets

- **Key formats:** PKCS#1 and PKCS#8 DER/PEM private keys, public DER/PEM, and public JWK import are supported as documented by the relevant classes. `SaveFixed` is used for fixed-width RSA cryptographic outputs; DER integer encodings have separate ASN.1 length/sign rules.
- **Imported-key checks:** `HasPublicKey`/`HasPrivateKey` check presence; `CheckPrivateKey` checks mathematical consistency. Policy validation of key size, intended algorithm, trusted public-key origin, and provenance of imported factors remains an application responsibility.
- **Concurrency:** high-level `Sign` and `Verify` use `fSafe` for shared mutable BigInt/context state. The PSS self-check occurs outside the private-operation lock. `TRsa` does not claim that every load, generation, and property mutation is safe concurrently with signing or verification.
- **Custom random generators:** `Drbg` is an injectable `TAesPrngAbstract`, not a lifetime-managed certificate of security. In production it must be cryptographically appropriate and thread-safe; applications must not mutate/free it concurrently with active operations.
- **Secret disposal:** `FillZero`, `WipeReleased`, worker destruction and private-key `Done` are best-effort anti-forensic measures. They do not defend against arbitrary process memory inspection or every exceptional stack/heap copy.

## 11. Verification record — what was checked and how

The review combines inspection of the current source, standards-oriented reasoning, earlier code-review discussions, independent integer models, and an inventory of the repository's tests. The purpose is to document **reproducible technical evidence** for the optimized code paths—not to confuse static review with a compiled test run.

### 11.1 Source paths and invariants inspected

| Area | Reviewed functions | Invariant / conclusion |
| --- | --- | --- |
| Memory and BigInt arithmetic | `TRsaContext.Allocate/Reduce/ModPower`, `TBigInt.Square`, `Trim`, `Divide` | Context ownership, normalized values, bounded Barrett fast path with division fallback, optimized exponentiation. |
| Probable-prime search | `FillPrime`, `IsPrime`, `FillMillerRabinWitness`, `XorStrongRandom` | Strong random starting point; fast sieve/base-2 pretest; full-width unbiased random witnesses; configured minimum rounds. |
| Trusted seed integration | `FillRandomTrusted` in `mormot.crypt.core.pas`, `FillPrime` call site | The initial seed has explicit OS/hardware/CSPRNG source acceptance and failure behavior, distinct from fast routine witness generation. |
| Fixed-exponent optimization | `PrimeAllowedFor65537`, `ModPower` | Alternating-limb modular identity and signed accumulator; fast 65537 public exponent handling. |
| Parallel RSA generation | `TRsaPrimeWorker`, `TRsa.Generate` | Separate worker context, serialized prime handoff, completion synchronization and wipe-on-destruction. |
| PKCS#1 v1.5 and PSS | `Pkcs1Pad`, `Pkcs1UnPad`, `Pkcs1Sign`, `TRsa/ TRsaPss.Sign/Verify` | Nonzero randomized encryption padding, exact DER `DigestInfo`, fixed-width signatures, and high-level self-verification. |
| CRT and compatibility | `ChineseRemainderTheorem`, `Seal`, `Open` | CRT limited to arithmetic, protocol checks in callers; established EVP-style envelope contract preserved. |

Previous review discussions also compared the underlying Barrett correction logic, the base-2 probable-prime pretest, the 65537 exponent path, and the four-bit sliding-window strategy with their reference mathematical operations. These were static reasoning and source checks, not formal proofs of each assembly backend.

### 11.2 Independent arithmetic modeling in this update

A separate, deterministic Python integer model (not a Pascal implementation or FPC execution) was used during the related review with a fixed random seed. It checked:

1. **11,200** signed alternating-limb-sum cases, across 16- and 32-bit limb representations and the seven supported RSA key-size configurations, against direct integer remainder modulo 65537. It also checked that the accumulator stays within the relevant signed pointer width.
2. Witness byte packing, high-limb masking and bit-length bounds for both limb widths over nonaligned small sizes and boundaries including 512, 1024, 1536, 2048 and 4096 bits (over **10,000 synthetic samples**).
3. A negative-sum `mod 65537` regression case, verifying the signed arithmetic rejects a candidate with remainder 1.

All three model checks passed. Their value is to catch algebraic, width, and masking mistakes; they **do not** exercise the Pascal compiler, `Drbg.FillRandom` implementation, hardware instructions, or statistical/cryptographic randomness.

Previously in these conversations, an independent model had also been reported for Miller–Rabin witness masking across 16/32-bit limbs and for the modulo-65537 identity. The cited mathematical models are supporting evidence for the identities and bit-width rules. No claim is made that the models execute the native Pascal or assembly routines.

### 11.3 Existing repository tests examined, but not executed here

`test/test.core.crypt.pas` contains:

- `RsaBarrett`: optimized modular-reduction comparisons using division-based reference paths.
- `RsaMillerRabin`: composite/prime examples, Barrett-versus-division modular powers, reference counts, context cache invariants, and preservation of candidate values.
- `RsaSlow`: RSA-2048 generation, public/private consistency, primality checks for `p`/`q`, DER round-trip, signing/verification, and `Seal`/`Open` round-trip.
- CSPRNG tests exercising `FillRandomNotZero` and nonzero output.

**Execution limit:** no native Free Pascal/Delphi compiler or compiled test results were available in this environment for this revision; no passing CI run was established for the reviewed HEAD. Source review and arithmetic modeling must not be reported as a completed build or test pass.

### 11.4 Recommended reproducible verification for a release

| Test family | Proposed evidence / pass criterion |
| --- | --- |
| Compiler and platform matrix | FPC + Delphi, CPU32/CPU64, x86/x64/AArch64; generic Pascal and available assembly paths agree. |
| BigInt reduction | Randomized `Reduce`/`ModPower` vs reference division and a trusted external big-integer library, including Barrett operand-size boundaries. |
| 65537 shortcut | Property-test `PrimeAllowedFor65537 = ((p mod 65537) <> 1)` across signed-sum and limb extremes; retain the regression counterexample. |
| Witness generation | Deterministic injected DRBG for 0, 1, `w-2`, `w-1`, extreme bit lengths, repeated `Trim`, and forced retries. |
| Prime and key generation | RSA-2048, 3072, 4096; single-core and parallel paths; timeout, equality, distance, exponent and CRT invariants; verify actual modulus length. |
| Signatures | RFC 8017 test vectors and OpenSSL interoperability for RS256/384/512 and PS256/384/512, including exact fixed-width signatures and negative cases. |
| Self-check failure | Corrupt an intermediate CRT result in a test fixture and verify high-level `Sign` never emits an invalid signature. |
| Encryption compatibility | Historical mORMot/OpenSSL-compatible envelope fixtures, nonzero EME padding, malformed ciphertext behavior, and application-level oracle/timing analysis. |
| Memory and concurrency | Parallel workers, reference/leak instrumentation, injected generator failure, repeated signing on one object, and secret wipe checks. |
| Side-channel tests | Dedicated timing/cache/power analysis if deploying against attackers able to observe private operations locally. |

## 12. Production use and trust boundaries

The built-in defaults are intended for normal mORMot server and application workloads without cryptographic tuning. Choose the higher-level API appropriate to the desired operation and let the implementation select its seeded CSPRNG and optimizations.

| Usage | Default behavior and rationale | When to choose differently |
| --- | --- | --- |
| RSA key generation | `TRsa.GenerateNew` selects RSA-2048, `e=65537`, randomized primality testing, prime-pair constraints and trusted initial seed. | Choose RSA-3072/4096 if application security lifetime or policy calls for a larger modulus; do not create new 512/1024-bit production keys. |
| RS256 / RS384 / RS512 signatures | Standard PKCS#1 v1.5 encoding, fixed-size output, strict verification, automatic high-level signature self-check. | Select the corresponding hash/key profile required by a protocol; preserve the separate key-purpose policy. |
| PS256 / PS384 / PS512 | Fresh CSPRNG salt, MGF1, precise PSS encoding, self-verification. | Choose PSS for new signature protocols when both parties support the same salt/hash profile. |
| Direct `Pkcs1Sign` / private CRT | Low-level arithmetic/encoding primitives stay reusable and fast. | Use high-level `Sign` to receive the automatic signature self-verification guarantee. |
| RSAES-PKCS1-v1_5 and `Seal` / `Open` | Existing interoperable encryption format remains available; encryption padding uses nonzero CSPRNG bytes. | Use an authenticated higher-level protocol when ciphertext integrity is needed; avoid exposing decryption errors/timing as an oracle. |
| Custom `Drbg` or imported key material | Defaults handle the common case; import checks establish mathematical consistency. | Configure custom generators or validation policies only for a specialized trust or interoperability requirement. |

### 12.1 Deliberate architectural boundaries

- `ChineseRemainderTheorem` is a mathematical primitive, not an expensive all-purpose validator. High-level signing already checks its externally returned result with efficient public RSA verification.
- `CheckPrivateKey` checks key consistency, not whether every imported factor has undergone costly new probable-prime testing.
- General-purpose BigInt arithmetic and its optimized Pascal/assembly implementations prioritize throughput; this is not a claim of constant-time behavior against local power/cache/timing observation.
- The existing AES-CTR-based EVP-style envelope is an interoperability feature; application-level integrity and confidentiality policies remain separate.
- Key import and cryptographic key purpose remain caller responsibilities. Strict FIPS signature profiles additionally mandate their own key sizes, entropy policies, and separation from key establishment.

These conditions are **boundaries of the applicable API or threat model**, not reasons for ordinary developers to avoid the default RSA engine or add arbitrary parameters.

## 13. Conclusion and maintenance criteria

The source review supports the design of `mormot.crypt.rsa` as a **practical, secure-by-default general-purpose RSA implementation** for the supported native targets, assuming a functioning cryptographic entropy environment and appropriate application protocol. The reasons are concrete: well-understood BigInt/CRT arithmetic, reproducible modular identities behind fast paths, randomized full-width Miller–Rabin witnesses, appropriate prime-pair restrictions, source-aware trusted seeding, strict RFC 8017 encodings, and low-cost **unconditional high-level signature self-verification**.

Its NIST correspondence is best described as **RSA generation and validation guided by FIPS 186-5**. That is a useful technical reference for algorithms and best practices, not a requirement for a general-purpose runtime to implement a certified FIPS profile.

For subsequent performance or portability changes, maintain five review habits:

1. State the mathematical identity and its preconditions (limb width, normalization, modulus and operand ownership).
2. Keep an independent reference implementation or algebraic model for each optimization; explicitly test overflows and boundary values.
3. Test FPC/Delphi and CPU32/CPU64, plus assembly versus Pascal implementations, whenever a fast path changes.
4. Preserve entropy and encoding guarantees: strong random starting points, unbiased rejection sampling where required, fixed-size RSA signatures and strict decoding.
5. Preserve the public API's layering: arithmetic routines compute, high-level signing verifies, and callers define protocol-level authentication and key-use policies.

The important default is **no special tuning required** for ordinary generation, signing or verification. A different key size or protocol format is an intentional application choice, not a prerequisite for using the library responsibly.

---

## References

1. **NIST**, [FIPS 186-5 — Digital Signature Standard](https://doi.org/10.6028/NIST.FIPS.186-5), §5, Appendix A.1 (RSA prime/key generation), Appendix B.3 (primality tests), Appendix C (test counts and rationale). See also the [NIST publication page and errata notice](https://csrc.nist.gov/pubs/fips/186-5/final).
2. **NIST**, [SP 800-90A Rev. 1 — Recommendation for Random Number Generation Using Deterministic Random Bit Generators](https://doi.org/10.6028/NIST.SP.800-90Ar1).
3. **NIST**, [SP 800-57 Part 1 Rev. 5 — Key Management](https://doi.org/10.6028/NIST.SP.800-57pt1r5), for modulus/hash security strengths and key-management policy.
4. **NIST**, [SP 800-56B Rev. 2 — Pair-Wise Key Establishment Using Integer Factorization Cryptography](https://doi.org/10.6028/NIST.SP.800-56Br2), distinct from the RSA *signature* profile.
5. **IETF**, [RFC 8017 — PKCS #1: RSA Cryptography Specifications Version 2.2](https://www.rfc-editor.org/rfc/rfc8017.html), §§4–9 and Appendix A.
6. **OpenSSL**, [EVP_SealInit / EVP_SealFinal reference](https://docs.openssl.org/3.0/man3/EVP_SealInit/), for operation-level interoperability context.
7. **Menezes, van Oorschot, Vanstone**, [Handbook of Applied Cryptography, chapter 4](https://cacr.uwaterloo.ca/hac/about/chap4.pdf), for probabilistic prime generation and testing background.
8. **mORMot2**, [reviewed immutable RSA source at `efdf73f3e2`](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/src/crypt/mormot.crypt.rsa.pas) and [RSA tests](https://github.com/synopse/mORMot2/blob/efdf73f3e20fe620ee0ce03f8bd2a0bdcaf49527/test/test.core.crypt.pas).
