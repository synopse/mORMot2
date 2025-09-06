/// low-level access to the OpenSSL 1.1 / 3.x Library
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.openssl11;

{
  *****************************************************************************

   Cross-Platform and Cross-Compiler OpenSSL 1.1 / 3.x API
   - Dynamic or Static OpenSSL Library Loading
   - OpenSSL Library Constants
   - OpenSSL Library Types and Structures
   - OpenSSL Library Functions
   - OpenSSL Helpers
   - TLS / HTTPS Encryption Layer using OpenSSL for mormot.net.sock / TCrtSocket

    In respect to OpenSSL 1.0.x, the new 1.1 API hides most structures
   behind getter/setter functions, and does not require complex initialization.
    OpenSSL 1.1 features TLS 1.3, but is now deprecated.
    OpenSSL 3.x is supported as the current major version.
    OpenSSL 1.1 / 3.x API adaptation is done at runtime by dynamic loading.

  *****************************************************************************

  Warning:
   On Windows, the USE_OPENSSL conditional is defined, but the OpenSSL
     lib*.dll will be loaded at runtime, only if needed, and silently fail if
     they are not available or in an unexpected version.
   Therefore, the SChannel layer will be used for TLS support by default, until
     OpenSslInitialize or OpenSslIsAvailable are called and succeeded.
   We did not enable OpenSSL by default on Windows, because from experience,
     it is very likely that your executable may find some obsolete dll in your
     Windows path, if it can't find any suitable dll in its own folder.

   On POSIX, this unit will always try to load OpenSSL at startup, as if
     FORCE_OPENSSL conditional was defined.
   On Darwin/MacOS, the .dylib supplied by the system are unstable and should
     not be used. Try instead e.g. https://synopse.info/files/OpenSSLMacX64.tgz
     (for x64) or https://synopse.info/files/OpenSSLMacA64.tgz (for arm).

   This unit will only enable direct low-level OpenSSL APIs and TLS: you need
     to call explicitly RegisterOpenSsl to enable mormot.crypt.openssl.pas
     algorithms in mORMot high-level classes and wrappers.

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
     to any restriction about the use of cryptographic software in your country.
}


{.$define OPENSSLFULLAPI}
// define this conditional to publish the whole (huge) OpenSSL API - unsupported
// - as stored in mormot.lib.openssl11.full.inc separated file
// - by default, only the API features needed by mORMot are published
// - full API increases compilation time, is unsupported, but kept as reference
// - the full API libraries will be directly/statically linked, not dynamically:
// if you have "cannot find -lcrypto" errors at linking, run e.g. the following:
//     cd /usr/lib/x86_64-linux-gnu
//     sudo ln -s libcrypto.so.1.1 libcrypto.so
//     sudo ln -s libssl.so.1.1 libssl.so

{.$define OPENSSLUSERTLMM}
// define this so that OpenSSL will use pascal RTL GetMem/FreeMem/ReallocMem
// - note that OpenSSL has no "finalize" API, and is likely to leak memory - so
// you may try to define it if you don't check memory leaks (at you own risk)

{.$define NOOPENSSL1}
// define this to disable OpenSSL 1.1 API - safer on any recent system

{.$define NOOPENSSL3}
// define this to disable OpenSSL 3.x API - not a good idea


{$ifdef FPCMM_REPORTMEMORYLEAKS}
  {$undef OPENSSLUSERTLMM} // incompatible for sure
{$endif FPCMM_REPORTMEMORYLEAKS}


interface

{$I ..\mormot.defines.inc}

{$ifdef USE_OPENSSL}

// compile as a void unit if USE_OPENSSL is not defined

uses
  sysutils,
  mormot.core.base,
  {$ifdef OSPOSIX}
  unixtype,
  {$endif OSPOSIX}
  mormot.core.os,
  mormot.net.sock; // for INetTls


{ ******************** Dynamic or Static OpenSSL Library Loading }

type
  /// exception class raised by this unit
  EOpenSsl = class(ExceptionWithProps)
  protected
    fLastError: integer;
    class function GetOpenSsl: string;
    // wrap ERR_get_error/ERR_error_string_n or SSL_get_error/SSL_error
    class procedure CheckFailed(caller: TObject; const method: ShortString;
      errormsg: PRawUtf8 = nil; ssl: pointer = nil; sslretcode: integer = 0;
      const context: RawUtf8 = '');
    class procedure TryNotAvailable(caller: TClass; const method: ShortString);
  public
    /// if res <> OPENSSLSUCCESS, raise the exception with some detailed message
    // - warning: optional ssl parameter is expected to be a PSSL, not a PSSL_CTX
    class procedure Check(caller: TObject; const method: ShortString;
      res: integer; errormsg: PRawUtf8 = nil; ssl: pointer = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// if res <> OPENSSLSUCCESS, raise the exception with some detailed message
    // - warning: optional ssl parameter is expected to be a PSSL, not a PSSL_CTX
    class procedure Check(res: integer; const method: ShortString = '';
      ssl: pointer = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// raise the exception if OpenSslIsAvailable if false
    class procedure CheckAvailable(caller: TClass; const method: ShortString);
      {$ifdef HASINLINE} inline; {$endif}
  published
    /// the last error code from OpenSSL, after Check() failure
    property LastError: integer
      read fLastError;
    /// returns the OpenSslVersionHexa value
    property OpenSsl: string
      read GetOpenSsl;
  end;


const
  { some binaries may be retrieved from
    - on Windows, try http://wiki.overbyte.eu/wiki/index.php/ICS_Download
      or https://slproweb.com/products/Win32OpenSSL.html (which is WinXP ready)
    - on Mac, you could try our https://synopse.info/files/OpenSSLMacX64.tgz
      or the now deprecated https://github.com/grijjy/DelphiOpenSsl
    - in practice, we found out that OpenSSL 3.0 seems slower than OpenSSL 1.1
      not in its raw process, but due to some API overhead (small blocks)
  }
  {$ifdef OSWINDOWS}
    {$ifdef CPU32}
    LIB_CRYPTO1 = 'libcrypto-1_1.dll';
    LIB_SSL1    = 'libssl-1_1.dll';
    LIB_CRYPTO3 = 'libcrypto-3.dll';
    LIB_SSL3    = 'libssl-3.dll';
    _PU = '';
    {$else}
    LIB_CRYPTO1 = 'libcrypto-1_1-x64.dll';
    LIB_SSL1    = 'libssl-1_1-x64.dll';
    LIB_CRYPTO3 = 'libcrypto-3-x64.dll';
    LIB_SSL3    = 'libssl-3-x64.dll';
    _PU = '';
    {$endif CPU32}
  {$else}
    {$ifdef OSANDROID}
      {$define NOOPENSSL3} // unsupported yet
      {$ifdef CPU32}
      LIB_CRYPTO1 = 'libcrypto-android32.a';
      LIB_SSL1    = 'libssl-android32.a';
      _PU = '';
      {$define OPENSSLSTATIC}
      {$else}
      LIB_CRYPTO1 = 'libcrypto-android64.a';
      LIB_SSL1    = 'libssl-android64.a';
      _PU = '';
      {$define OPENSSLSTATIC}
      {$endif CPU32}
    {$else}
      {$ifdef OSDARWIN}
        {$ifdef CPUINTEL}
          // from https://github.com/grijjy/DelphiOpenSsl
          {$ifdef CPUX86}
          LIB_CRYPTO1 = 'libssl-merged-osx32.dylib';
          LIB_SSL1    = 'libssl-merged-osx32.dylib';
          _PU = '_';
          {$endif CPUX86}
          {$ifdef CPUX64}
          LIB_CRYPTO1 = 'libssl-merged-osx64.dylib';
          LIB_SSL1    = 'libssl-merged-osx64.dylib';
          _PU = '_';
          {$endif CPUX64}
          {$ifdef CPUX64_static}
          LIB_CRYPTO1 = 'libcrypto-osx64.a';
          LIB_SSL1    = 'libssl-osx64.a';
          _PU = '';
          {$define OPENSSLSTATIC}
          {$endif CPUX64_static}
        {$else}
          // regular OpenSSL 1.1 dylib - to be supplied
          LIB_CRYPTO1 = 'libcrypto.1.1.dylib'; // typically ARM64
          LIB_SSL1    = 'libssl.1.1.dylib';
          _PU = '';
        {$endif CPUINTEL}
        // regular OpenSSL 3 from https://synopse.info/files/OpenSSLMacX64.tgz
        // the system dylib fails as "xxx is loading libcrypto in an unsafe way"
        // because Apple deprecates its OpenSSL API since 10.7 days (2011) in
        // favor of its own "Cryptographic Services", so we won't try to load
        // plain libcrypto/libssl.dylib but search for modern custom .dylib
        LIB_CRYPTO3 = 'libcrypto.3.dylib';
        LIB_SSL3    = 'libssl.3.dylib';
      {$else}
        {$ifdef OSLINUX}
        // specific versions on Linux
        LIB_CRYPTO1 = 'libcrypto.so.1.1';
        LIB_SSL1    = 'libssl.so.1.1';
        LIB_CRYPTO3 = 'libcrypto.so.3';
        LIB_SSL3    = 'libssl.so.3';
        {$else} // not tested on OpenBSD/FreeBSD yet
        LIB_CRYPTO1 = 'libcrypto.so'; // should redirect to 1.1 or 3
        LIB_SSL1    = 'libssl.so';
        LIB_CRYPTO3 = 'libcrypto.so.3';
        LIB_SSL3    = 'libssl.so.3';
        {$endif OSLINUX}
        _PU = '';
      {$endif OSDARWIN}
    {$endif OSANDROID}
  {$endif OSWINDOWS}

var
  /// optional libcrypto location for OpenSslIsAvailable/OpenSslInitialize
  // - you could also set OpenSslDefaultPath or OPENSSL_LIBPATH environment variable
  OpenSslDefaultCrypto: TFileName;
  /// optional libssl location for OpenSslIsAvailable/OpenSslInitialize
  // - you could also set OpenSslDefaultPath or OPENSSL_LIBPATH environment variable
  OpenSslDefaultSsl: TFileName;

  /// OpenSSL library path, if OPENSSL_LIBPATH environment variable is not set
  // - will search for the default library names in this location
  // - you can also set specific OpenSslDefaultCrypto and OpenSslDefaultSsl values
  OpenSslDefaultPath: TFileName;

  /// numeric OpenSSL library version loaded e.g. after OpenSslIsAvailable call
  // - equals e.g. $1010106f or $300000b0
  // - "if OpenSslVersion >= OPENSSL3_VERNUM then" to detect OpenSSL 3.x
  OpenSslVersion: cardinal;
  /// hexadecimal OpenSSL library version loaded e.g. after OpenSslIsAvailable call
  // - equals e.g. '1010106F' or '300000B0'
  OpenSslVersionHexa: string;
  /// high-level OpenSSL text description, e.g. 'OpenSSL 3.0.11 19 Sep 2023'
  OpenSslVersionText: RawUtf8;

  /// internal PSSL data reference slot - for SSL_get_ex_data/SSL_set_ex_data
  OpenSslExIndexSsl: integer;

{$ifdef OPENSSLSTATIC}

  // only OpenSSL 1.1 is supported yet as static linking (need more testing)
  {$undef NOOPENSSL1}
  {$define NOOPENSSL3}

const
  LIB_CRYPTO = LIB_CRYPTO1;  // for external LIB_CRYPTO function definitions
  LIB_SSL    = LIB_SSL1;     // for external LIB_SSL    function definitions

  OpenSslStatic = true;

{$else}

const
  OpenSslStatic = false;

var
  /// internal flag used by OpenSslIsAvailable function for dynamic loading
  openssl_initialized: TLibraryState;

  /// the error message triggerred by OpenSslIsAvailable when loading OpenSSL
  // - is replicated as a global variable to allow early initialization before
  // mormot.core.log exception interception is enabled
  openssl_initialize_errormsg: string;

{$endif OPENSSLSTATIC}


const
  /// the minimal 32-bit OpenSslVersion value for Open SSL 1.1.0
  OPENSSL1_VERNUM = $10100000;
  /// the minimal 32-bit OpenSslVersion value for Open SSL 1.1.1
  OPENSSL11_VERNUM = $10101000;
  /// the minimal 32-bit OpenSslVersion value for Open SSL 3.0.0
  OPENSSL3_VERNUM = $30000000;
  /// the minimal 32-bit OpenSslVersion value for Open SSL 3.1.0
  OPENSSL31_VERNUM = $30100000;

  {$ifdef NOOPENSSL3}
  LIB_TXT = '1.1';
  LIB_MIN = OPENSSL1_VERNUM;
  {$else}
  {$ifdef NOOPENSSL1}
  LIB_TXT = '3.x';
  LIB_MIN = OPENSSL3_VERNUM;
  {$else}
  LIB_TXT = '1.1/3.x';
  LIB_MIN = OPENSSL1_VERNUM;
  {$endif NOOPENSSL1}
  {$endif NOOPENSSL3}

/// return TRUE if OpenSSL 1.1 / 3.x library can be used
// - will load and initialize it, calling OpenSslInitialize if necessary with
// the global/default search paths, catching any exception during the process
// - always return true if OPENSSLFULLAPI or OPENSSLSTATIC conditionals are set
// - on success, returns true and register OpenSSL for TLS support - but
// you need to call explicitly RegisterOpenSsl to enable mormot.crypt.openssl
// algorithms in mORMot high-level wrappers
// - you should never call any OpenSSL function if false is returned
// - this method is thread safe, using function LibraryAvailable/GlobalLock
function OpenSslIsAvailable: boolean;

/// return TRUE if OpenSSL 1.1 / 3.x library has been initialized
// - don't try to load it if was not already done
// - could be run before OpenSslInitialize() is called
function OpenSslIsLoaded: boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// initialize the OpenSSL 1.1 / 3.x API, accessible via the global functions
// - will raise EOpenSsl exception on any loading issue
// - you can force the library path names to load as parameters, but by default
// OpenSSL 3.x / 1.1 libraries will be searched from OpenSslDefaultCrypto and
// OpenSslDefaultSsl global variables or OPENSSL_LIBPATH environment variable,
// then within the executable folder, and then in the system path
// - do nothing if the library has already been loaded or if
// OPENSSLFULLAPI or OPENSSLSTATIC conditionals have been defined
// - on success, returns true and register OpenSSL for TLS support - but
// you need to call explicitly RegisterOpenSsl to enable mormot.crypt.openssl
// algorithms in mORMot high-level wrappers
// - this method is not thread safe and should be executed e.g. at startup or
// within GlobalLock/GlobalUnlock - or via OpenSslIsAvailable wrapper
function OpenSslInitialize(
   const libcryptoname: TFileName = '';
   const libsslname: TFileName = '';
   const libprefix: RawUtf8 = _PU): boolean;


{ ******************** OpenSSL Library Constants }

const
  OPENSSLSUCCESS = 1; // API returns usually 0 or <0 on error

  OPENSSL_VERSION_ = 0;
  OPENSSL_CFLAGS   = 1;
  OPENSSL_BUILT_ON = 2;
  OPENSSL_PLATFORM = 3;
  OPENSSL_DIR      = 4;

  CRYPTO_EX_INDEX_SSL = 0;
  CRYPTO_EX_INDEX_SSL_CTX = 1;
  CRYPTO_EX_INDEX_SSL_SESSION = 2;
  CRYPTO_EX_INDEX_X509 = 3;
  CRYPTO_EX_INDEX_X509_STORE = 4;
  CRYPTO_EX_INDEX_X509_STORE_CTX = 5;
  CRYPTO_EX_INDEX_DH = 6;
  CRYPTO_EX_INDEX_DSA = 7;
  CRYPTO_EX_INDEX_EC_KEY = 8;
  CRYPTO_EX_INDEX_RSA = 9;
  CRYPTO_EX_INDEX_ENGINE = 10;
  CRYPTO_EX_INDEX_UI = 11;
  CRYPTO_EX_INDEX_BIO = 12;
  CRYPTO_EX_INDEX_APP = 13;
  CRYPTO_EX_INDEX_UI_METHOD = 14;
  CRYPTO_EX_INDEX_DRBG = 15;
  CRYPTO_EX_INDEX__COUNT = 16;

  EVP_CIPH_NO_PADDING = $100;
  EVP_CTRL_GCM_GET_TAG = $10;
  EVP_CTRL_GCM_SET_TAG = $11;

  EVP_MAX_MD_SIZE = 64; // 512-bit
  EVP_MAX_KEY_LENGTH = 64;
  EVP_MAX_IV_LENGTH = 16;
  EVP_MAX_BLOCK_LENGTH = 32;

  EVP_MD_FLAG_ONESHOT = $0001;
  EVP_MD_FLAG_XOF = $0002;
  EVP_MD_FLAG_DIGALGID_MASK = $0018;
  EVP_MD_FLAG_DIGALGID_NULL = $0000;
  EVP_MD_FLAG_DIGALGID_ABSENT = $0008;
  EVP_MD_FLAG_DIGALGID_CUSTOM = $0018;
  EVP_MD_FLAG_FIPS = $0400;

  SN_pbe_WithSHA1And3_Key_TripleDES_CBC = 'PBE-SHA1-3DES';
  NID_pbe_WithSHA1And3_Key_TripleDES_CBC = 146; // for old PKCS#12 nid_key
  SN_X9_62_prime192v1 = 'prime192v1';
  NID_X9_62_prime192v1 = 409;
  SN_X9_62_prime192v2 = 'prime192v2';
  NID_X9_62_prime192v2 = 410;
  SN_X9_62_prime192v3 = 'prime192v3';
  NID_X9_62_prime192v3 = 411;
  SN_X9_62_prime239v1 = 'prime239v1';
  NID_X9_62_prime239v1 = 412;
  SN_X9_62_prime239v2 = 'prime239v2';
  NID_X9_62_prime239v2 = 413;
  SN_X9_62_prime239v3 = 'prime239v3';
  NID_X9_62_prime239v3 = 414;
  SN_X9_62_prime256v1 = 'prime256v1';
  NID_X9_62_prime256v1 = 415; // = secp256r1
  NID_aes_256_cbc = 427;
  NID_sha256 = 672;
  NID_sha384 = 673;
  NID_sha512 = 674;
  NID_sha224 = 675;
  SN_secp112r1 = 'secp112r1';
  NID_secp112r1 = 704;
  SN_secp112r2 = 'secp112r2';
  NID_secp112r2 = 705;
  SN_secp128r1 = 'secp128r1';
  NID_secp128r1 = 706;
  SN_secp128r2 = 'secp128r2';
  NID_secp128r2 = 707;
  SN_secp160k1 = 'secp160k1';
  NID_secp160k1 = 708;
  SN_secp160r1 = 'secp160r1';
  NID_secp160r1 = 709;
  SN_secp160r2 = 'secp160r2';
  NID_secp160r2 = 710;
  SN_secp192k1 = 'secp192k1';
  NID_secp192k1 = 711;
  SN_secp224k1 = 'secp224k1';
  NID_secp224k1 = 712;
  SN_secp224r1 = 'secp224r1';
  NID_secp224r1 = 713;
  SN_secp256k1 = 'secp256k1';
  NID_secp256k1 = 714;
  SN_secp384r1 = 'secp384r1';
  NID_secp384r1 = 715;
  SN_secp521r1 = 'secp521r1';
  NID_secp521r1 = 716;
  SN_sect113r1 = 'sect113r1';
  NID_sect113r1 = 717;
  SN_sect113r2 = 'sect113r2';
  NID_sect113r2 = 718;
  SN_sect131r1 = 'sect131r1';
  NID_sect131r1 = 719;
  SN_sect131r2 = 'sect131r2';
  NID_sect131r2 = 720;
  SN_sect163k1 = 'sect163k1';
  NID_sect163k1 = 721;
  SN_sect163r1 = 'sect163r1';
  NID_sect163r1 = 722;
  SN_sect163r2 = 'sect163r2';
  NID_sect163r2 = 723;
  SN_sect193r1 = 'sect193r1';
  NID_sect193r1 = 724;
  SN_sect193r2 = 'sect193r2';
  NID_sect193r2 = 725;
  SN_sect233k1 = 'sect233k1';
  NID_sect233k1 = 726;
  SN_sect233r1 = 'sect233r1';
  NID_sect233r1 = 727;
  SN_sect239k1 = 'sect239k1';
  NID_sect239k1 = 728;
  SN_sect283k1 = 'sect283k1';
  NID_sect283k1 = 729;
  SN_sect283r1 = 'sect283r1';
  NID_sect283r1 = 730;
  SN_sect409k1 = 'sect409k1';
  NID_sect409k1 = 731;
  SN_sect409r1 = 'sect409r1';
  NID_sect409r1 = 732;
  SN_sect571k1 = 'sect571k1';
  NID_sect571k1 = 733;
  SN_sect571r1 = 'sect571r1';
  NID_sect571r1 = 734;
  SN_ED25519 = 'ED25519';
  NID_ED25519 = 1087;
  SN_poly1305 = 'Poly1305';
  LN_poly1305 = 'poly1305';
  NID_poly1305 = 1061;
  LN_rsaEncryption = 'rsaEncryption';
  NID_rsaEncryption = 6;
  LN_rsassaPss = 'rsassaPss';
  NID_rsassaPss = 912;
  LN_dsa = 'dsaEncryption';
  NID_dsa = 116;
  NID_sha256WithRSAEncryption = 668;
  NID_sha384WithRSAEncryption = 669;
  NID_sha512WithRSAEncryption = 670;
  NID_ecdsa_with_SHA256 = 794;
  NID_ecdsa_with_SHA384 = 795;
  NID_ecdsa_with_SHA512 = 796;
  LN_dhKeyAgreement = 'dhKeyAgreement';
  NID_dhKeyAgreement = 28;
  SN_X9_62_id_ecPublicKey = 'id-ecPublicKey';
  NID_X9_62_id_ecPublicKey = 408;

  SN_crl_number = 'crlNumber';
  LN_crl_number = 'X509v3 CRL Number';
  NID_crl_number = 88;
  SN_crl_reason = 'CRLReason';
  LN_crl_reason = 'X509v3 CRL Reason Code';
  NID_crl_reason = 141;
  SN_invalidity_date = 'invalidityDate';
  LN_invalidity_date = 'Invalidity Date';
  NID_invalidity_date = 142;
  SN_delta_crl = 'deltaCRL';
  LN_delta_crl = 'X509v3 Delta CRL Indicator';
  NID_delta_crl = 140;
  SN_id_scrypt = 'id-scrypt';
  LN_id_scrypt = 'scrypt';
  NID_id_scrypt = 973;

  EVP_PKEY_RSA = NID_rsaEncryption;
  EVP_PKEY_DSA = NID_dsa;
  EVP_PKEY_RSA_PSS = NID_rsassaPss;
  EVP_PKEY_DH = NID_dhKeyAgreement;
  EVP_PKEY_EC = NID_X9_62_id_ecPublicKey;
  EVP_PKEY_ED25519 = NID_ED25519;
  EVP_PKEY_POLY1305 = NID_poly1305;
  EVP_PKEY_SCRYPT = NID_id_scrypt;
  EVP_PKEY_OP_PARAMGEN = 1 shl 1;
  EVP_PKEY_OP_KEYGEN = 1 shl 2;
  EVP_PKEY_ALG_CTRL = $1000;
  EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID = EVP_PKEY_ALG_CTRL + 1;
  EVP_PKEY_CTRL_DSA_PARAMGEN_BITS = EVP_PKEY_ALG_CTRL + 1;
  EVP_PKEY_CTRL_RSA_KEYGEN_BITS = EVP_PKEY_ALG_CTRL + 3;
  EVP_PKEY_CTRL_RSA_PADDING = EVP_PKEY_ALG_CTRL + 1;
  EVP_PKEY_CTRL_RSA_PSS_SALTLEN = EVP_PKEY_ALG_CTRL + 2;
  EVP_PKEY_CTRL_RSA_KEYGEN_PUBEXP = EVP_PKEY_ALG_CTRL + 4;
  EVP_PKEY_CTRL_RSA_MGF1_MD = EVP_PKEY_ALG_CTRL + 5;
  EVP_PKEY_CTRL_GET_RSA_PADDING = EVP_PKEY_ALG_CTRL + 6;
  EVP_PKEY_CTRL_GET_RSA_PSS_SALTLEN = EVP_PKEY_ALG_CTRL + 7;
  EVP_PKEY_CTRL_GET_RSA_MGF1_MD = EVP_PKEY_ALG_CTRL + 8;
  EVP_PKEY_CTRL_RSA_OAEP_MD = EVP_PKEY_ALG_CTRL + 9;
  EVP_PKEY_CTRL_RSA_OAEP_LABEL = EVP_PKEY_ALG_CTRL + 10;
  EVP_PKEY_CTRL_GET_RSA_OAEP_MD = EVP_PKEY_ALG_CTRL + 11;
  EVP_PKEY_CTRL_GET_RSA_OAEP_LABEL = EVP_PKEY_ALG_CTRL + 12;
  EVP_PKEY_CTRL_RSA_KEYGEN_PRIMES = EVP_PKEY_ALG_CTRL + 13;

  EVP_F_EVP_PKEY_DERIVE = 153;
  EVP_F_EVP_PKEY_DERIVE_INIT = 154;
  EVP_F_EVP_PKEY_DERIVE_SET_PEER = 155;

  BIO_FLAGS_READ = $01;
  BIO_FLAGS_WRITE = $02;
  BIO_FLAGS_IO_SPECIAL = $04;
  BIO_FLAGS_RWS = BIO_FLAGS_READ or BIO_FLAGS_WRITE or BIO_FLAGS_IO_SPECIAL;
  BIO_FLAGS_SHOULD_RETRY = $08;

  BIO_C_SET_CONNECT = 100;
  BIO_C_DO_STATE_MACHINE = 101;
  BIO_C_SET_NBIO = 102;
  BIO_C_SET_FD = 104;
  BIO_C_GET_FD = 105;
  BIO_C_SET_FILE_PTR = 106;
  BIO_C_GET_FILE_PTR = 107;
  BIO_C_SET_FILENAME = 108;
  BIO_C_SET_SSL = 109;
  BIO_C_GET_SSL = 110;
  BIO_C_SET_MD = 111;
  BIO_C_GET_MD = 112;
  BIO_C_GET_CIPHER_STATUS = 113;
  BIO_C_SET_BUF_MEM = 114;
  BIO_C_GET_BUF_MEM_PTR = 115;
  BIO_C_GET_BUFF_NUM_LINES = 116;
  BIO_C_SET_BUFF_SIZE = 117;
  BIO_C_SET_ACCEPT = 118;
  BIO_C_SSL_MODE = 119;
  BIO_C_GET_MD_CTX = 120;
  BIO_C_SET_BUFF_READ_DATA = 122;
  BIO_C_GET_CONNECT = 123;
  BIO_C_GET_ACCEPT = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT = 127;
  BIO_C_FILE_SEEK = 128;
  BIO_C_GET_CIPHER_CTX = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN = 130;
  BIO_C_SET_BIND_MODE = 131;
  BIO_C_GET_BIND_MODE = 132;
  BIO_C_FILE_TELL = 133;
  BIO_C_GET_SOCKS = 134;
  BIO_C_SET_SOCKS = 135;
  BIO_C_SET_WRITE_BUF_SIZE = 136;
  BIO_C_GET_WRITE_BUF_SIZE = 137;
  BIO_C_MAKE_BIO_PAIR = 138;
  BIO_C_DESTROY_BIO_PAIR = 139;
  BIO_C_GET_WRITE_GUARANTEE = 140;
  BIO_C_GET_READ_REQUEST = 141;
  BIO_C_SHUTDOWN_WR = 142;
  BIO_C_NREAD0 = 143;
  BIO_C_NREAD = 144;
  BIO_C_NWRITE0 = 145;
  BIO_C_NWRITE = 146;
  BIO_C_RESET_READ_REQUEST = 147;
  BIO_C_SET_MD_CTX = 148;
  BIO_C_SET_PREFIX = 149;
  BIO_C_GET_PREFIX = 150;
  BIO_C_SET_SUFFIX = 151;
  BIO_C_GET_SUFFIX = 152;
  BIO_C_SET_EX_ARG = 153;
  BIO_C_GET_EX_ARG = 154;
  BIO_C_SET_CONNECT_MODE = 155;

  BIO_CTRL_RESET = 1;
  BIO_CTRL_EOF = 2;
  BIO_CTRL_INFO = 3;
  BIO_CTRL_SET = 4;
  BIO_CTRL_GET = 5;
  BIO_CTRL_PUSH = 6;
  BIO_CTRL_POP = 7;
  BIO_CTRL_GET_CLOSE = 8;
  BIO_CTRL_SET_CLOSE = 9;
  _BIO_CTRL_PENDING = 10;
  BIO_CTRL_FLUSH = 11;
  BIO_CTRL_DUP = 12;
  _BIO_CTRL_WPENDING = 13;
  BIO_CTRL_SET_CALLBACK = 14;
  BIO_CTRL_GET_CALLBACK = 15;
  BIO_CTRL_PEEK = 29;
  BIO_CTRL_SET_FILENAME = 30;
  BIO_CTRL_DGRAM_CONNECT = 31;
  BIO_CTRL_DGRAM_SET_CONNECTED = 32;
  BIO_CTRL_DGRAM_SET_RECV_TIMEOUT = 33;
  BIO_CTRL_DGRAM_GET_RECV_TIMEOUT = 34;
  BIO_CTRL_DGRAM_SET_SEND_TIMEOUT = 35;
  BIO_CTRL_DGRAM_GET_SEND_TIMEOUT = 36;
  BIO_CTRL_DGRAM_GET_RECV_TIMER_EXP = 37;
  BIO_CTRL_DGRAM_GET_SEND_TIMER_EXP = 38;
  BIO_CTRL_DGRAM_MTU_DISCOVER = 39;
  BIO_CTRL_DGRAM_QUERY_MTU = 40;
  BIO_CTRL_DGRAM_GET_FALLBACK_MTU = 47;
  BIO_CTRL_DGRAM_GET_MTU = 41;
  BIO_CTRL_DGRAM_SET_MTU = 42;
  BIO_CTRL_DGRAM_MTU_EXCEEDED = 43;
  BIO_CTRL_DGRAM_GET_PEER = 46;
  BIO_CTRL_DGRAM_SET_PEER = 44;
  BIO_CTRL_DGRAM_SET_NEXT_TIMEOUT = 45;
  BIO_CTRL_DGRAM_SET_DONT_FRAG = 48;
  BIO_CTRL_DGRAM_GET_MTU_OVERHEAD = 49;
  BIO_CTRL_DGRAM_SCTP_SET_IN_HANDSHAKE = 50;
  BIO_CTRL_DGRAM_SET_PEEK_MODE = 71;

  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;
  SSL_ERROR_WANT_ASYNC = 9;
  SSL_ERROR_WANT_ASYNC_JOB = 10;
  SSL_ERROR_WANT_CLIENT_HELLO_CB = 11;

  SSL_OP_LEGACY_SERVER_CONNECT = $00000004;
  SSL_OP_TLSEXT_PADDING = $00000010;
  SSL_OP_SAFARI_ECDHE_ECDSA_BUG = $00000040;
  SSL_OP_ALLOW_NO_DHE_KEX = $00000400;
  SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS = $00000800;
  SSL_OP_NO_QUERY_MTU = $00001000;
  SSL_OP_COOKIE_EXCHANGE = $00002000;
  SSL_OP_NO_TICKET = $00004000;
  SSL_OP_CISCO_ANYCONNECT = $00008000;
  SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = $00010000;
  SSL_OP_NO_COMPRESSION = $00020000;
  SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION = $00040000;
  SSL_OP_NO_ENCRYPT_THEN_MAC = $00080000;
  SSL_OP_ENABLE_MIDDLEBOX_COMPAT = $00100000;
  SSL_OP_PRIORITIZE_CHACHA = $00200000;
  SSL_OP_CIPHER_SERVER_PREFERENCE = $00400000;
  SSL_OP_TLS_ROLLBACK_BUG = $00800000;
  SSL_OP_NO_ANTI_REPLAY = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_NO_TLSv1_2 = $08000000;
  SSL_OP_NO_TLSv1_1 = $10000000;
  SSL_OP_NO_TLSv1_3 = $20000000;
  SSL_OP_NO_DTLSv1 = $04000000;
  SSL_OP_NO_DTLSv1_2 = $08000000;
  SSL_OP_NO_SSL_MASK = SSL_OP_NO_SSLv3 or
                       SSL_OP_NO_TLSv1 or
                       SSL_OP_NO_TLSv1_1 or
                       SSL_OP_NO_TLSv1_2 or
                       SSL_OP_NO_TLSv1_3;
  SSL_OP_NO_DTLS_MASK = SSL_OP_NO_DTLSv1 or
                        SSL_OP_NO_DTLSv1_2;
  SSL_OP_NO_RENEGOTIATION = $40000000;
  SSL_OP_CRYPTOPRO_TLSEXT_BUG = $80000000;
  SSL_OP_ALL = SSL_OP_CRYPTOPRO_TLSEXT_BUG or
               SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS or
               SSL_OP_LEGACY_SERVER_CONNECT or
               SSL_OP_TLSEXT_PADDING or
               SSL_OP_SAFARI_ECDHE_ECDSA_BUG;

  SSL_CTRL_SET_TMP_DH = 3;
  SSL_CTRL_SET_TMP_ECDH = 4;
  SSL_CTRL_SET_TMP_DH_CB = 6;

  SSL_CTRL_GET_CLIENT_CERT_REQUEST = 9;
  SSL_CTRL_GET_NUM_RENEGOTIATIONS = 10;
  SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS = 11;
  SSL_CTRL_GET_TOTAL_RENEGOTIATIONS = 12;
  SSL_CTRL_GET_FLAGS = 13;
  SSL_CTRL_EXTRA_CHAIN_CERT = 14;
  SSL_CTRL_SET_MSG_CALLBACK = 15;
  SSL_CTRL_SET_MSG_CALLBACK_ARG = 16;
  SSL_CTRL_SET_MTU = 17;
  SSL_CTRL_SESS_NUMBER = 20;
  SSL_CTRL_SESS_CONNECT = 21;
  SSL_CTRL_SESS_CONNECT_GOOD = 22;
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE = 23;
  SSL_CTRL_SESS_ACCEPT = 24;
  SSL_CTRL_SESS_ACCEPT_GOOD = 25;
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE = 26;
  SSL_CTRL_SESS_HIT = 27;
  SSL_CTRL_SESS_CB_HIT = 28;
  SSL_CTRL_SESS_MISSES = 29;
  SSL_CTRL_SESS_TIMEOUTS = 30;
  SSL_CTRL_SESS_CACHE_FULL = 31;
  SSL_CTRL_MODE = 33;
  SSL_CTRL_GET_READ_AHEAD = 40;
  SSL_CTRL_SET_READ_AHEAD = 41;
  SSL_CTRL_SET_SESS_CACHE_SIZE = 42;
  SSL_CTRL_GET_SESS_CACHE_SIZE = 43;
  SSL_CTRL_SET_SESS_CACHE_MODE = 44;
  SSL_CTRL_GET_SESS_CACHE_MODE = 45;
  SSL_CTRL_GET_MAX_CERT_LIST = 50;
  SSL_CTRL_SET_MAX_CERT_LIST = 51;
  SSL_CTRL_SET_MAX_SEND_FRAGMENT = 52;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB = 53;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG = 54;
  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
  SSL_CTRL_SET_TLSEXT_DEBUG_CB = 56;
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG = 57;
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS = 58;
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS = 59;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB = 63;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG = 64;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE = 65;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS = 66;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS = 67;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS = 68;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS = 69;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP = 70;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP = 71;
  SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB = 72;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB = 75;
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB = 76;
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB = 77;
  SSL_CTRL_SET_SRP_ARG = 78;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME = 79;
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH = 80;
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD = 81;
  DTLS_CTRL_GET_TIMEOUT = 73;
  DTLS_CTRL_HANDLE_TIMEOUT = 74;
  SSL_CTRL_SET_MIN_PROTO_VERSION = 123;
  SSL_CTRL_SET_MAX_PROTO_VERSION = 124;

  TLSEXT_NAMETYPE_host_name = 0;
  TLSEXT_STATUSTYPE_ocsp = 1;

  TLS1_VERSION = $0301;
  TLS1_1_VERSION = $0302;
  TLS1_2_VERSION = $0303;
  TLS1_3_VERSION = $0304;
  TLS_MAX_VERSION = TLS1_3_VERSION;
  TLS_ANY_VERSION = $10000;

  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_ALERT_WARNING = 1;
  SSL_TLSEXT_ERR_ALERT_FATAL = 2;
  SSL_TLSEXT_ERR_NOACK = 3;

  X509_FILETYPE_PEM = 1;
  X509_FILETYPE_ASN1 = 2;
  X509_FILETYPE_DEFAULT = 3;
  SSL_FILETYPE_ASN1 = X509_FILETYPE_ASN1;
  SSL_FILETYPE_PEM = X509_FILETYPE_PEM;

  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;
  SSL_VERIFY_CLIENT_ONCE = $04;
  SSL_VERIFY_POST_HANDSHAKE = $08;

  X509_V_OK = 0;
  X509_V_ERR_UNSPECIFIED = 1;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2;
  X509_V_ERR_UNABLE_TO_GET_CRL = 3;
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4;
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5;
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6;
  X509_V_ERR_CERT_SIGNATURE_FAILURE = 7;
  X509_V_ERR_CRL_SIGNATURE_FAILURE = 8;
  X509_V_ERR_CERT_NOT_YET_VALID = 9;
  X509_V_ERR_CERT_HAS_EXPIRED = 10;
  X509_V_ERR_CRL_NOT_YET_VALID = 11;
  X509_V_ERR_CRL_HAS_EXPIRED = 12;
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13;
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14;
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15;
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16;
  X509_V_ERR_OUT_OF_MEM = 17;
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20;
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21;
  X509_V_ERR_CERT_CHAIN_TOO_LONG = 22;
  X509_V_ERR_CERT_REVOKED = 23;
  X509_V_ERR_INVALID_CA = 24;

  NID_netscape_comment = 78;
  NID_subject_key_identifier = 82;
  NID_key_usage = 83;
  NID_subject_alt_name = 85;
  NID_issuer_alt_name = 86;
  NID_basic_constraints = 87;
  NID_authority_key_identifier = 90;
  NID_ext_key_usage = 126;
  NID_info_access = 177;

  X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT = $1;
  X509_CHECK_FLAG_NO_WILDCARDS = $2;
  X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS = $4;
  X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS = $8;
  X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS = $10;
  X509_CHECK_FLAG_NEVER_CHECK_SUBJECT = $20;
  _X509_CHECK_FLAG_DOT_SUBDOMAINS = $8000;

  X509_V_ERR_PATH_LENGTH_EXCEEDED = 25;
  X509_V_ERR_INVALID_PURPOSE = 26;
  X509_V_ERR_CERT_UNTRUSTED = 27;
  X509_V_ERR_CERT_REJECTED = 28;
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH = 29;
  X509_V_ERR_AKID_SKID_MISMATCH = 30;
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31;
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN = 32;
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER = 33;
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION = 34;
  X509_V_ERR_KEYUSAGE_NO_CRL_SIGN = 35;
  X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION = 36;
  X509_V_ERR_INVALID_NON_CA = 37;
  X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED = 38;
  X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE = 39;
  X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED = 40;
  X509_V_ERR_INVALID_EXTENSION = 41;
  X509_V_ERR_INVALID_POLICY_EXTENSION = 42;
  X509_V_ERR_NO_EXPLICIT_POLICY = 43;
  X509_V_ERR_DIFFERENT_CRL_SCOPE = 44;
  X509_V_ERR_UNSUPPORTED_EXTENSION_FEATURE = 45;
  X509_V_ERR_UNNESTED_RESOURCE = 46;
  X509_V_ERR_PERMITTED_VIOLATION = 47;
  X509_V_ERR_EXCLUDED_VIOLATION = 48;
  X509_V_ERR_SUBTREE_MINMAX = 49;
  X509_V_ERR_APPLICATION_VERIFICATION = 50;
  X509_V_ERR_UNSUPPORTED_CONSTRAINT_TYPE = 51;
  X509_V_ERR_UNSUPPORTED_CONSTRAINT_SYNTAX = 52;
  X509_V_ERR_UNSUPPORTED_NAME_SYNTAX = 53;
  X509_V_ERR_CRL_PATH_VALIDATION_ERROR = 54;
  X509_V_ERR_PATH_LOOP = 55;
  X509_V_ERR_SUITE_B_INVALID_VERSION = 56;
  X509_V_ERR_SUITE_B_INVALID_ALGORITHM = 57;
  X509_V_ERR_SUITE_B_INVALID_CURVE = 58;
  X509_V_ERR_SUITE_B_INVALID_SIGNATURE_ALGORITHM = 59;
  X509_V_ERR_SUITE_B_LOS_NOT_ALLOWED = 60;
  X509_V_ERR_SUITE_B_CANNOT_SIGN_P_384_WITH_P_256 = 61;
  X509_V_ERR_HOSTNAME_MISMATCH = 62;
  X509_V_ERR_EMAIL_MISMATCH = 63;
  X509_V_ERR_IP_ADDRESS_MISMATCH = 64;
  X509_V_ERR_DANE_NO_MATCH = 65;
  X509_V_ERR_EE_KEY_TOO_SMALL = 66;
  X509_V_ERR_CA_KEY_TOO_SMALL = 67;
  X509_V_ERR_CA_MD_TOO_WEAK = 68;
  X509_V_ERR_INVALID_CALL = 69;
  X509_V_ERR_STORE_LOOKUP = 70;
  X509_V_ERR_NO_VALID_SCTS = 71;
  X509_V_ERR_PROXY_SUBJECT_NAME_VIOLATION = 72;
  X509_V_ERR_OCSP_VERIFY_NEEDED = 73;
  X509_V_ERR_OCSP_VERIFY_FAILED = 74;
  X509_V_ERR_OCSP_CERT_UNKNOWN = 75;

  X509_V_FLAG_CB_ISSUER_CHECK = $0;
  X509_V_FLAG_USE_CHECK_TIME = $2;
  X509_V_FLAG_CRL_CHECK = $4;
  X509_V_FLAG_CRL_CHECK_ALL = $8;
  X509_V_FLAG_IGNORE_CRITICAL = $10;
  X509_V_FLAG_X509_STRICT = $20;
  X509_V_FLAG_ALLOW_PROXY_CERTS = $40;
  X509_V_FLAG_POLICY_CHECK = $80;
  X509_V_FLAG_EXPLICIT_POLICY = $100;
  X509_V_FLAG_INHIBIT_ANY = $200;
  X509_V_FLAG_INHIBIT_MAP = $400;
  X509_V_FLAG_NOTIFY_POLICY = $800;
  X509_V_FLAG_EXTENDED_CRL_SUPPORT = $1000;
  X509_V_FLAG_USE_DELTAS = $2000;
  X509_V_FLAG_CHECK_SS_SIGNATURE = $4000;
  X509_V_FLAG_TRUSTED_FIRST = $8000;
  X509_V_FLAG_SUITEB_128_LOS_ONLY = $10000;
  X509_V_FLAG_SUITEB_192_LOS = $20000;
  X509_V_FLAG_SUITEB_128_LOS = $30000;
  X509_V_FLAG_PARTIAL_CHAIN = $80000;
  X509_V_FLAG_NO_ALT_CHAINS = $100000;
  X509_V_FLAG_NO_CHECK_TIME = $200000;

  // X509_get_key_usage() returned flags
  X509v3_KU_ENCIPHER_ONLY = $0001;
  X509v3_KU_CRL_SIGN = $0002;
  X509v3_KU_KEY_CERT_SIGN = $0004;
  X509v3_KU_KEY_AGREEMENT = $0008;
  X509v3_KU_DATA_ENCIPHERMENT = $0010;
  X509v3_KU_KEY_ENCIPHERMENT = $0020;
  X509v3_KU_NON_REPUDIATION = $0040;
  X509v3_KU_DIGITAL_SIGNATURE = $0080;
  X509v3_KU_DECIPHER_ONLY = $8000;
  X509v3_KU_UNDEF = $ffff;

  // X509_get_extended_key_usage() returned flags
  XKU_SSL_SERVER = $1;
  XKU_SSL_CLIENT = $2;
  XKU_SMIME = $4;
  XKU_CODE_SIGN = $8;
  XKU_SGC = $10;
  XKU_OCSP_SIGN = $20;
  XKU_TIMESTAMP = $40;
  XKU_DVCS = $80;
  XKU_ANYEKU = $100;

  X509_L_FILE_LOAD = 1;
  X509_L_ADD_DIR = 2;

  // X509V3_EXT_print() flags
  X509V3_EXT_UNKNOWN_MASK = $f shl 16;
  X509V3_EXT_DEFAULT = 0;
  X509V3_EXT_ERROR_UNKNOWN = 1 shl 16;
  X509V3_EXT_PARSE_UNKNOWN = 2 shl 16;
  X509V3_EXT_DUMP_UNKNOWN = 3 shl 16;

  // X509_STORE_CTX_set_purpose() flags
  X509_PURPOSE_DYNAMIC = $1;
  X509_PURPOSE_DYNAMIC_NAME = $2;
  X509_PURPOSE_SSL_CLIENT = 1;
  X509_PURPOSE_SSL_SERVER = 2;
  X509_PURPOSE_NS_SSL_SERVER = 3;
  X509_PURPOSE_SMIME_SIGN = 4;
  X509_PURPOSE_SMIME_ENCRYPT = 5;
  X509_PURPOSE_CRL_SIGN = 6;
  X509_PURPOSE_ANY = 7;
  X509_PURPOSE_OCSP_HELPER = 8;
  X509_PURPOSE_TIMESTAMP_SIGN = 9;
  X509_PURPOSE_MIN = 1;
  X509_PURPOSE_MAX = 9;

  CRLDP_ALL_REASONS = $807f;
  CRL_REASON_NONE = -1;
  CRL_REASON_UNSPECIFIED = 0;
  CRL_REASON_KEY_COMPROMISE = 1;
  CRL_REASON_CA_COMPROMISE = 2;
  CRL_REASON_AFFILIATION_CHANGED = 3;
  CRL_REASON_SUPERSEDED = 4;
  CRL_REASON_CESSATION_OF_OPERATION = 5;
  CRL_REASON_CERTIFICATE_HOLD = 6;
  CRL_REASON_REMOVE_FROM_CRL = 8;
  CRL_REASON_PRIVILEGE_WITHDRAWN = 9;
  CRL_REASON_AA_COMPROMISE = 10;

  // X509_STORE_CTX_set_trust() flags
  X509_TRUST_DEFAULT = 0;
  X509_TRUST_COMPAT = 1;
  X509_TRUST_SSL_CLIENT = 2;
  X509_TRUST_SSL_SERVER = 3;
  X509_TRUST_EMAIL = 4;
  X509_TRUST_OBJECT_SIGN = 5;
  X509_TRUST_OCSP_SIGN = 6;
  X509_TRUST_OCSP_REQUEST = 7;
  X509_TRUST_TSA = 8;
  X509_TRUST_MIN = 1;
  X509_TRUST_MAX = 8;
  X509_TRUST_DYNAMIC = 1 shl 0;
  X509_TRUST_DYNAMIC_NAME = 1 shl 1;
  X509_TRUST_NO_SS_COMPAT = 1 shl 2;
  X509_TRUST_DO_SS_COMPAT = 1 shl 3;
  X509_TRUST_OK_ANY_EKU = 1 shl 4;
  X509_TRUST_TRUSTED = 1;
  X509_TRUST_REJECTED = 2;
  X509_TRUST_UNTRUSTED = 3;

  PKCS5_SALT_LEN = 8;
  PKCS5_DEFAULT_ITER = 2048;

  PKCS12_KEY_ID = 1;
  PKCS12_IV_ID = 2;
  PKCS12_MAC_ID = 3;
  PKCS12_DEFAULT_ITER = PKCS5_DEFAULT_ITER;
  PKCS12_MAC_KEY_LENGTH = 20;
  PKCS12_SALT_LEN = 8;

  ASN1_STRFLGS_ESC_2253 = 1;
  ASN1_STRFLGS_ESC_CTRL = 2;
  ASN1_STRFLGS_ESC_MSB = 4;
  ASN1_STRFLGS_ESC_QUOTE = 8;

  CHARTYPE_PRINTABLESTRING = $10;
  CHARTYPE_FIRST_ESC_2253 = $20;
  CHARTYPE_LAST_ESC_2253 = $40;

  ASN1_STRFLGS_UTF8_CONVERT = $10;
  ASN1_STRFLGS_IGNORE_TYPE = $20;
  ASN1_STRFLGS_SHOW_TYPE = $40;
  ASN1_STRFLGS_DUMP_ALL = $80;
  ASN1_STRFLGS_DUMP_UNKNOWN = $100;
  ASN1_STRFLGS_DUMP_DER = $200;
  ASN1_STRFLGS_ESC_2254 = $400;
  ASN1_STRFLGS_RFC2253 = ASN1_STRFLGS_ESC_2253 or
                         ASN1_STRFLGS_ESC_CTRL or
                         ASN1_STRFLGS_ESC_MSB or
                         ASN1_STRFLGS_UTF8_CONVERT or
                         ASN1_STRFLGS_DUMP_UNKNOWN or
                         ASN1_STRFLGS_DUMP_DER;

  XN_FLAG_SEP_MASK = $f shl 16;
  XN_FLAG_COMPAT = 0;
  XN_FLAG_SEP_COMMA_PLUS = 1 shl 16;
  XN_FLAG_SEP_CPLUS_SPC = 2 shl 16;
  XN_FLAG_SEP_SPLUS_SPC = 3 shl 16;
  XN_FLAG_SEP_MULTILINE = 4 shl 16;
  XN_FLAG_DN_REV = 1 shl 20;
  XN_FLAG_FN_MASK = $3 shl 21;
  XN_FLAG_FN_SN = 0;
  XN_FLAG_FN_LN = 1 shl 21;
  XN_FLAG_FN_OID = 2 shl 21;
  XN_FLAG_FN_NONE = 3 shl 21;
  XN_FLAG_SPC_EQ = 1 shl 23;
  XN_FLAG_DUMP_UNKNOWN_FIELDS = 1 shl 24;
  XN_FLAG_FN_ALIGN = 1 shl 25;
  XN_FLAG_RFC2253 = ASN1_STRFLGS_RFC2253 or
                    XN_FLAG_SEP_COMMA_PLUS or
                    XN_FLAG_DN_REV or
                    XN_FLAG_FN_SN or
                    XN_FLAG_DUMP_UNKNOWN_FIELDS;
  XN_FLAG_ONELINE = ASN1_STRFLGS_RFC2253 or
                    ASN1_STRFLGS_ESC_QUOTE or
                    XN_FLAG_SEP_CPLUS_SPC or
                    XN_FLAG_SPC_EQ or
                    XN_FLAG_FN_SN;
  XN_FLAG_MULTILINE = ASN1_STRFLGS_ESC_CTRL or
                      ASN1_STRFLGS_ESC_MSB or
                      XN_FLAG_SEP_MULTILINE or
                      XN_FLAG_SPC_EQ or
                      XN_FLAG_FN_LN or
                      XN_FLAG_FN_ALIGN;

  MBSTRING_FLAG = $1000;
  MBSTRING_UTF8 = MBSTRING_FLAG;
  MBSTRING_ASC = MBSTRING_FLAG or 1;
  MBSTRING_BMP = MBSTRING_FLAG or 2;
  MBSTRING_UNIV = MBSTRING_FLAG or 4;
  MBSTRING: array[{utf8=}boolean] of integer = (
    MBSTRING_ASC,
    MBSTRING_UTF8);

  RSA_PKCS1_PADDING = 1;
  RSA_SSLV23_PADDING = 2;
  RSA_NO_PADDING = 3;
  RSA_PKCS1_OAEP_PADDING = 4;
  RSA_X931_PADDING = 5;
  RSA_PKCS1_PSS_PADDING = 6;
  RSA_PKCS1_PADDING_SIZE = 11;
  RSA_FLAG_FIPS_METHOD = $0400;
  RSA_FLAG_NON_FIPS_ALLOW = $0400;
  RSA_FLAG_CHECKED = $0800;

  EVP_PKEY_OP_UNDEFINED = 0;
  EVP_PKEY_OP_SIGN = 1 shl 3;
  EVP_PKEY_OP_VERIFY = 1 shl 4;
  EVP_PKEY_OP_VERIFYRECOVER = 1 shl 5;
  EVP_PKEY_OP_SIGNCTX = 1 shl 6;
  EVP_PKEY_OP_VERIFYCTX = 1 shl 7;
  EVP_PKEY_OP_ENCRYPT = 1 shl 8;
  EVP_PKEY_OP_DECRYPT = 1 shl 9;
  EVP_PKEY_OP_DERIVE = 1 shl 10;
  EVP_PKEY_OP_TYPE_SIG = EVP_PKEY_OP_SIGN or
                         EVP_PKEY_OP_VERIFY or
                         EVP_PKEY_OP_VERIFYRECOVER or
                         EVP_PKEY_OP_SIGNCTX or
                         EVP_PKEY_OP_VERIFYCTX;
  EVP_PKEY_OP_TYPE_CRYPT = EVP_PKEY_OP_ENCRYPT or
                           EVP_PKEY_OP_DECRYPT;
  EVP_PKEY_OP_TYPE_NOGEN = EVP_PKEY_OP_TYPE_SIG or
                           EVP_PKEY_OP_TYPE_CRYPT or
                           EVP_PKEY_OP_DERIVE;
  EVP_PKEY_OP_TYPE_GEN = EVP_PKEY_OP_PARAMGEN or
                         EVP_PKEY_OP_KEYGEN;

type
  OSSL_HANDSHAKE_STATE = (
    TLS_ST_BEFORE = 0,
    TLS_ST_OK = 1,
    DTLS_ST_CR_HELLO_VERIFY_REQUEST = 2,
    TLS_ST_CR_SRVR_HELLO = 3,
    TLS_ST_CR_CERT = 4,
    TLS_ST_CR_CERT_STATUS = 5,
    TLS_ST_CR_KEY_EXCH = 6,
    TLS_ST_CR_CERT_REQ = 7,
    TLS_ST_CR_SRVR_DONE = 8,
    TLS_ST_CR_SESSION_TICKET = 9,
    TLS_ST_CR_CHANGE = 10,
    TLS_ST_CR_FINISHED = 11,
    TLS_ST_CW_CLNT_HELLO = 12,
    TLS_ST_CW_CERT = 13,
    TLS_ST_CW_KEY_EXCH = 14,
    TLS_ST_CW_CERT_VRFY = 15,
    TLS_ST_CW_CHANGE = 16,
    TLS_ST_CW_NEXT_PROTO = 17,
    TLS_ST_CW_FINISHED = 18,
    TLS_ST_SW_HELLO_REQ = 19,
    TLS_ST_SR_CLNT_HELLO = 20,
    DTLS_ST_SW_HELLO_VERIFY_REQUEST = 21,
    TLS_ST_SW_SRVR_HELLO = 22,
    TLS_ST_SW_CERT = 23,
    TLS_ST_SW_KEY_EXCH = 24,
    TLS_ST_SW_CERT_REQ = 25,
    TLS_ST_SW_SRVR_DONE = 26,
    TLS_ST_SR_CERT = 27,
    TLS_ST_SR_KEY_EXCH = 28,
    TLS_ST_SR_CERT_VRFY = 29,
    TLS_ST_SR_NEXT_PROTO = 30,
    TLS_ST_SR_CHANGE = 31,
    TLS_ST_SR_FINISHED = 32,
    TLS_ST_SW_SESSION_TICKET = 33,
    TLS_ST_SW_CERT_STATUS = 34,
    TLS_ST_SW_CHANGE = 35,
    TLS_ST_SW_FINISHED = 36,
    TLS_ST_SW_ENCRYPTED_EXTENSIONS = 37,
    TLS_ST_CR_ENCRYPTED_EXTENSIONS = 38,
    TLS_ST_CR_CERT_VRFY = 39,
    TLS_ST_SW_CERT_VRFY = 40,
    TLS_ST_CR_HELLO_REQ = 41,
    TLS_ST_SW_KEY_UPDATE = 42,
    TLS_ST_CW_KEY_UPDATE = 43,
    TLS_ST_SR_KEY_UPDATE = 44,
    TLS_ST_CR_KEY_UPDATE = 45,
    TLS_ST_EARLY_DATA = 46,
    TLS_ST_PENDING_EARLY_DATA_END = 47,
    TLS_ST_CW_END_OF_EARLY_DATA = 48,
    TLS_ST_SR_END_OF_EARLY_DATA = 49);
  POSSL_HANDSHAKE_STATE = ^OSSL_HANDSHAKE_STATE;


{ ******************** OpenSSL Library Types and Structures }

type
  /// X509v3 Key and Extended Key Usage Flags
  // - kuCA match NID_basic_constraints containing 'CA:TRUE'
  // - kuEncipherOnly .. kuDecipherOnly match NID_key_usage values
  // - kuTlsServer .. kuAnyeku match NID_ext_key_usage values
  // - see https://omvs.de/2019/11/13/key-usage-extensions-at-x-509-certificates
  // - is an exact match of TCryptCertUsage enumerate in mormot.crypt.secure.pas
  // and TWinCertUsage in mormot.lib.sspi
  TX509Usage = (
    kuCA,
    kuEncipherOnly,
    kuCrlSign,
    kuKeyCertSign,
    kuKeyAgreement,
    kuDataEncipherment,
    kuKeyEncipherment,
    kuNonRepudiation,
    kuDigitalSignature,
    kuDecipherOnly,
    kuTlsServer,
    kuTlsClient,
    kuSMime,
    kuCodeSign,
    kuOcspSign,
    kuTimestamp);

  /// X509v3 Key and Extended Key Usage Flags
  // - is a convenient way to get or set a Certificate extensions
  // - an exact match of TCryptCertUsages enumerate in mormot.crypt.secure.pas
  // and TWinCertUsages in mormot.lib.sspi
  TX509Usages = set of TX509Usage;

{$MINENUMSIZE 4}

type
  PSSL = ^SSL;
  PPSSL = ^PSSL;
  PSSL_CIPHER = ^SSL_CIPHER;
  PPSSL_CIPHER = ^PSSL_CIPHER;

  POPENSSL_STACK = ^OPENSSL_STACK;
  PPOPENSSL_STACK = ^POPENSSL_STACK;

  PX509 = ^X509;
  PPX509 = ^PX509;
  PX509DynArray = array of PX509;
  Pstack_st_X509 = POPENSSL_STACK;
  PPstack_st_X509 = ^Pstack_st_X509;

  /// convenient wrapper to a PSSL instance
  SSL = object
  public
    function CurrentCipher: PSSL_CIPHER;
    function PeerChain: Pstack_st_X509;
    function PeerCertificate: PX509;
    function PeerCertificates(acquire: boolean = false): PX509DynArray;
    function PeerCertificatesAsPEM: RawUtf8;
    function PeerCertificatesAsText: RawUtf8;
    function IsVerified(msg: PRawUtf8 = nil): boolean;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// convenient wrapper to a PSSL_CIPHER instance
  SSL_CIPHER = object
  public
    function Description: RawUtf8;
  end;

  /// convenient wrapper to a PBIO instance
  BIO = object
  public
    function get_flags: integer;
      {$ifdef HASINLINE} inline; {$endif}
    function should_retry: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function should_read: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function should_write: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function should_io_special: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function retry_type: integer;
      {$ifdef HASINLINE} inline; {$endif}
    function get_ssl(s: PSSL): integer;
    function pending: integer;
    function eof: boolean;
    procedure ToString(var data: RawByteString;
      cp: integer = CP_RAWBYTESTRING); overload;
    function ToUtf8: RawUtf8; overload;
      {$ifdef HASINLINE} inline; {$endif}
    function ToUtf8AndFree: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    procedure Reset;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  PBIO = ^BIO;
  PPBIO = ^PBIO;

  PSSL_METHOD = type pointer;
  PPSSL_METHOD = ^PSSL_METHOD;

  PSSL_SESSION = type pointer;
  PPSSL_SESSION = ^PSSL_SESSION;

  /// convenient wrapper to a PSSL_CTX instance
  SSL_CTX = object
  public
    /// cut-down version of TOpenSslNetTls.SetupCtx, used e.g. with SNI
    // - warning: CertFile and KeyFile are UTF-8 encoded, not regular TFileName
    procedure SetCertificateFiles(const CertFile, KeyFile: RawUtf8;
      const KeyPassword: SpiUtf8);
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  PSSL_CTX = ^SSL_CTX;
  PPSSL_CTX = ^PSSL_CTX;

  PEVP_MD = type pointer;
  PPEVP_MD = ^PEVP_MD;

  /// buffer able to hold up to the maximum PEVP_MD digest size (i.e. 512-bit)
  EVP_MD_DIG = array[0..EVP_MAX_MD_SIZE - 1] of byte;
  PEVP_MD_DIG = ^EVP_MD_DIG;

  PEVP_CIPHER = type pointer;
  PPEVP_CIPHER = ^PEVP_CIPHER;

  /// convenient wrapper to a PEVP_PKEY instance
  EVP_PKEY = object
  public
    function PrivateToDer(const PassWord: SpiUtf8): RawByteString;
    function PublicToDer: RawByteString;
    function PrivateToPem(const PassWord: SpiUtf8): RawUtf8;
    function PublicToPem: RawUtf8;
    procedure ToPem(out PrivateKey, PublicKey: RawUtf8;
      const PrivateKeyPassWord: SpiUtf8 = '');
    function Sign(Algo: PEVP_MD; Msg: pointer; Len: integer): RawByteString;
    function Verify(Algo: PEVP_MD;
      Sig, Msg: pointer; SigLen, MsgLen: integer): boolean;
    function ToAltNames(const Subjects: TRawUtf8DynArray): RawUtf8;
    function CreateSelfSignedCsr(Algo: PEVP_MD;
      const Subjects: TRawUtf8DynArray): RawByteString;
    function Size: integer;
    function AlgoName: RawUtf8;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
    // methods below are for RSA algorithm only
    function RsaSeal(Cipher: PEVP_CIPHER; const Msg: RawByteString): RawByteString;
    function RsaOpen(Cipher: PEVP_CIPHER; const Msg: RawByteString;
      CodePage: integer = CP_RAWBYTESTRING): RawByteString;
    function RsaEncrypt(const Content: RawByteString; MD: PEVP_MD): RawByteString;
    function RsaDecrypt(const Content: RawByteString; MD: PEVP_MD;
      CodePage: integer = CP_RAWBYTESTRING): RawByteString;
    procedure RsaGetPubKey(out e, n: RawByteString);
    // methods below are for ECC algorithm only
    procedure EccGetPubKeyCompressed(out k: RawByteString);
    procedure EccGetPubKeyUncompressed(out x, y: RawByteString);
  end;

  PEVP_PKEY = ^EVP_PKEY;
  PPEVP_PKEY = ^PEVP_PKEY;

  PEVP_CIPHER_CTX = type pointer;
  PPEVP_CIPHER_CTX = ^PEVP_CIPHER_CTX;

  PEVP_MD_CTX = type pointer;
  PPEVP_MD_CTX = ^PEVP_MD_CTX;

  PEVP_PKEY_CTX = type pointer;
  PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;

  PHMAC_CTX = type pointer;
  PPHMAC_CTX = ^PHMAC_CTX;

  PRSA = type pointer;
  PPRSA = ^PRSA;

  PENGINE = type pointer;
  PPENGINE = ^PENGINE;

  POSSL_LIB_CTX = type pointer;
  POSSL_PROVIDER = type pointer;

  PBIO_METHOD = type pointer;
  PPBIO_METHOD = ^PBIO_METHOD;

  OPENSSL_sk_compfunc = function(p1: pointer; p2: pointer): integer; cdecl;
  OPENSSL_sk_freefunc = procedure(p1: pointer); cdecl;

  /// convenient wrapper to a POPENSSL_STACK instance
  OPENSSL_STACK = object
  private
    function GetItem(index: integer): pointer;
      {$ifdef HASINLINE} inline; {$endif}
  public
    function Count: integer;
      {$ifdef HASINLINE} inline; {$endif}
    function Add(one: pointer): integer;
      {$ifdef HASINLINE} inline; {$endif}
    function Find(one: pointer): integer;
    function Extract(index: integer): pointer;
    /// low-level method needing an explicit result typecast e.g. to PX509DynArray
    function ToDynArray: TPointerDynArray;
    /// note: instances should be released explicitly before or call e.g. FreeX509
    procedure Free;
    /// make PX509/_CRL/_EXTENSION.Free to all items, then free the stack
    procedure FreeX509;
    procedure FreeX509_CRL;
    procedure FreeX509_EXTENSION;
    /// weak access to stored Items[0..Count] - note: Delphi 7 fails as "default"
    property Items[index: integer]: pointer
      read GetItem; default;
  end;

  PBIGNUM = ^BIGNUM;

  /// convenient wrapper to a PBIGNUM instance
  BIGNUM = object
  public
    function ToDecimal: RawUtf8;
    function ToHex: RawUtf8;
    function Size: integer;
    procedure ToBin(bin: PByte); overload;
    procedure ToBin(out bin: RawByteString); overload;
    procedure Free;
  end;

  PPBIGNUM = ^PBIGNUM;

  PBN_CTX = type pointer;
  PPBN_CTX = ^PBN_CTX;

  PEC_KEY = type pointer;
  PPEC_KEY = ^PEC_KEY;

  PEC_GROUP = type pointer;
  PPEC_GROUP = ^PEC_GROUP;

  PEC_POINT = type pointer;
  PPEC_POINT = ^PEC_POINT;

  (** Enum for the point conversion form as defined in X9.62 (ECDSA)
   *  for the encoding of a elliptic curve point (x,y) *)
  point_conversion_form_t = (
    (** the point is encoded as z||x, where the octet z specifies
     *  which solution of the quadratic equation y is  *)
    POINT_CONVERSION_COMPRESSED = 2,
    (** the point is encoded as z||x||y, where z is the octet 0x04  *)
    POINT_CONVERSION_UNCOMPRESSED = 4,
    (** the point is encoded as z||x||y, where the octet z specifies
     *  which solution of the quadratic equation y is  *)
    POINT_CONVERSION_HYBRID = 6);
  Ppoint_conversion_form_t = ^point_conversion_form_t;

  Prsa_st = pointer;
  PPrsa_st = ^Prsa_st;

  buf_mem_st = record
    length: PtrUInt;
    data: PUtf8Char;
    max: PtrUInt;
    flags: cardinal;
  end;

  BUF_MEM = buf_mem_st;
  PBUF_MEM = ^BUF_MEM;

  Pstack_st_void = pointer;
  crypto_ex_data_st = record
    sk: Pstack_st_void;
  end;
  CRYPTO_EX_DATA = crypto_ex_data_st;
  PCRYPTO_EX_DATA = ^CRYPTO_EX_DATA;

  /// convenient wrapper to a PASN1_STRING instance
  asn1_string_st = object
  public
    function Data: pointer;
      {$ifdef HASINLINE} inline; {$endif}
    function Len: integer;
      {$ifdef HASINLINE} inline; {$endif}
    function GetType: integer;
    procedure ToUtf8(out result: RawUtf8;
      flags: cardinal = ASN1_STRFLGS_RFC2253 and not ASN1_STRFLGS_ESC_MSB);
    procedure ToHex(out result: RawUtf8); overload;
    function ToHex: RawUtf8; overload;
    function Equals(another: pointer): boolean;
  end;
  ASN1_STRING = asn1_string_st;
  PASN1_STRING = ^ASN1_STRING;
  PPASN1_STRING = ^PASN1_STRING;

  /// convenient wrapper to a PASN1_INTEGER instance
  ASN1_INTEGER = object(asn1_string_st)
  public
    function ToBigInt: PBIGNUM;
    function ToDecimal: RawUtf8;
    procedure Free;
  end;
  PASN1_INTEGER = ^ASN1_INTEGER;
  PPASN1_INTEGER = ^PASN1_INTEGER;

  /// convenient wrapper to a PASN1_OBJECT instance
  ASN1_OBJECT = object
  public
    function NID: integer;
      {$ifdef HASINLINE} inline; {$endif}
    function Name: PUtf8Char;
  end;
  PASN1_OBJECT = ^ASN1_OBJECT;
  PPASN1_OBJECT = ^PASN1_OBJECT;

  ASN1_BOOLEAN = integer;
  ASN1_NULL = integer;
  PASN1_NULL = ^ASN1_NULL;
  ASN1_ENUMERATED = asn1_string_st;
  PASN1_ENUMERATED = ^ASN1_ENUMERATED;
  ASN1_BIT_STRING = asn1_string_st;
  PASN1_BIT_STRING = ^ASN1_BIT_STRING;
  ASN1_OCTET_STRING = asn1_string_st;
  PASN1_OCTET_STRING = ^ASN1_OCTET_STRING;
  ASN1_PRINTABLESTRING = asn1_string_st;
  PASN1_PRINTABLESTRING = ^ASN1_PRINTABLESTRING;
  ASN1_T61STRING = asn1_string_st;
  PASN1_T61STRING = ^ASN1_T61STRING;
  ASN1_IA5STRING = asn1_string_st;
  PASN1_IA5STRING = ^ASN1_IA5STRING;
  ASN1_GENERALSTRING = asn1_string_st;
  PASN1_GENERALSTRING = ^ASN1_GENERALSTRING;
  ASN1_UNIVERSALSTRING = asn1_string_st;
  PASN1_UNIVERSALSTRING = ^ASN1_UNIVERSALSTRING;
  ASN1_UTCTIME = asn1_string_st;
  PASN1_UTCTIME = ^ASN1_UTCTIME;
  ASN1_BMPSTRING = asn1_string_st;
  PASN1_BMPSTRING = ^ASN1_BMPSTRING;
  PPASN1_BMPSTRING = ^PASN1_BMPSTRING;
  ASN1_GENERALIZEDTIME = asn1_string_st;
  PASN1_GENERALIZEDTIME = ^ASN1_GENERALIZEDTIME;
  ASN1_VISIBLESTRING = asn1_string_st;
  PASN1_VISIBLESTRING = ^ASN1_VISIBLESTRING;
  ASN1_UTF8STRING = asn1_string_st;
  PASN1_UTF8STRING = ^ASN1_UTF8STRING;
  PASN1_VALUE = type pointer;
  PPASN1_VALUE = ^PASN1_VALUE;

  /// convenient wrapper to a PASN1_TIME instance
  ASN1_TIME = object
  public
    /// may return 0 if ASN1_TIME_to_tm() is not supported on oldest OpenSSL
    function ToDateTime: TDateTime;
  end;
  PASN1_TIME = ^ASN1_TIME;

  _anonymous_type_1 = record
    case integer of
      0:  (ptr: PUtf8Char);
      1:  (boolean: ASN1_BOOLEAN);
      2:  (asn1_string: PASN1_STRING);
      3:  (_object: PASN1_OBJECT);
      4:  (_integer: PASN1_INTEGER);
      5:  (enumerated: PASN1_ENUMERATED);
      6:  (bit_string: PASN1_BIT_STRING);
      7:  (octet_string: PASN1_OCTET_STRING);
      8:  (printablestring: PASN1_PRINTABLESTRING);
      9:  (t61string: PASN1_T61STRING);
      10: (ia5string: PASN1_IA5STRING);
      11: (generalstring: PASN1_GENERALSTRING);
      12: (bmpstring: PASN1_BMPSTRING);
      13: (universalstring: PASN1_UNIVERSALSTRING);
      14: (utctime: PASN1_UTCTIME);
      15: (generalizedtime: PASN1_GENERALIZEDTIME);
      16: (visiblestring: PASN1_VISIBLESTRING);
      17: (utf8string: PASN1_UTF8STRING);
      18: (_set: PASN1_STRING);
      19: (sequence: PASN1_STRING);
      20: (asn1_value: PASN1_VALUE);
  end;
  P_anonymous_type_1 = ^_anonymous_type_1;

  asn1_type_st = record
    asn1type: integer;
    value: _anonymous_type_1;
  end;

  ASN1_TYPE = asn1_type_st;
  PASN1_TYPE = ^ASN1_TYPE;
  PPASN1_TYPE = ^PASN1_TYPE;

  PX509_ALGORS = type pointer;
  PPX509_ALGORS = ^PX509_ALGORS;

  PX509_REQ_INFO = type pointer;
  PPX509_REQ_INFO = ^PX509_REQ_INFO;

  PX509_NAME = ^X509_NAME;

  /// convenient wrapper to a PX509_REQ instance
  X509_REQ = object
  public
    function GetName: PX509_NAME;
    function GetPublicKey: PEVP_PKEY;
    function ToBinary: RawByteString;
    function ToPem: RawUtf8;
    procedure AddExtension(nid: integer; const value: RawUtf8);
    /// set key_usage/ext_key_usage extensions
    // - any previous usage set will be first deleted
    function SetUsageAndAltNames(usages: TX509Usages;
      const altnames: RawUtf8 = ''): boolean;
    /// check if the public key of this certificate matches a given private key
    // - returns the size of the signature in bytes for success and zero for failure
    function Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
    /// check if the public key of this certificate matches a given public key
    function Verify(pkey: PEVP_PKEY): boolean;
    function VerifySelf: boolean;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PX509_REQ = ^X509_REQ;
  PPX509_REQ = ^PX509_REQ;

  PX509_CERT_AUX = type pointer;
  PPX509_CERT_AUX = ^PX509_CERT_AUX;

  PX509_LOOKUP = pointer;
  PPX509_LOOKUP = ^PX509_LOOKUP;
  PX509_LOOKUP_METHOD = pointer;
  PPX509_LOOKUP_METHOD = ^PX509_LOOKUP_METHOD;

  PX509_VERIFY_PARAM = pointer;
  PPX509_VERIFY_PARAM = ^PX509_VERIFY_PARAM;

  Pstack_st_CONF_VALUE = POPENSSL_STACK;
  Plhash_st_CONF_VALUE = POPENSSL_STACK;

  /// convenient wrapper to a PX509V3_CONF_METHOD instance
  X509V3_CONF_METHOD = record
    get_string: function(db: pointer; section: PUtf8Char; value: PUtf8Char): PUtf8Char; cdecl;
    get_section: function(db: pointer; section: PUtf8Char): Pstack_st_CONF_VALUE; cdecl;
    free_string: procedure(db: pointer; _string: PUtf8Char); cdecl;
    free_section: procedure(db: pointer; section: Pstack_st_CONF_VALUE); cdecl;
  end;
  PX509V3_CONF_METHOD = ^X509V3_CONF_METHOD;

  PX509_NAME_ENTRY = ^X509_NAME_ENTRY;

  /// convenient wrapper to a PX509_NAME instance
  X509_NAME = object
  public
    function Count: integer;
    function Item(ndx: integer): PX509_NAME_ENTRY;
    function GetEntry(NID: integer): RawUtf8; overload; // not MBSTRING ready
    function GetEntry(const Name: RawUtf8): RawUtf8; overload;
    procedure ToUtf8(out result: RawUtf8;
      flags: cardinal = XN_FLAG_RFC2253 and not ASN1_STRFLGS_ESC_MSB);
    procedure AddEntry(const Name, Value: RawUtf8);
    procedure AddEntries(const Country, State, Locality, Organization, OrgUnit,
      CommonName, EmailAddress, SurName, GivenName, SerialNumber: RawUtf8);
    procedure SetEntry(const Name, Value: RawUtf8);
    procedure DeleteEntry(NID: integer); overload;
    procedure DeleteEntry(const Name: RawUtf8); overload;
    function Compare(another: PX509_NAME): integer;
    // as used for X509_STORE.SetLocations() CAFolder 'Hash.N' names
    function Hash: cardinal;
    function ToBinary(out dest: TSynTempBuffer): integer; overload;
    function ToBinary: RawByteString; overload;
    function ToDigest(md: PEVP_MD = nil): RawUtf8;
    function ToText: RawUtf8;
  end;

  /// convenient wrapper to a PX509_NAME_ENTRY instance
  X509_NAME_ENTRY = object
  public
    function Data: PASN1_STRING;
    function NID: integer;
    function Name: PUtf8Char;
    function Value: RawUtf8;
    procedure Free;
  end;

  PPX509_NAME_ENTRY = ^PX509_NAME_ENTRY;
  PPX509_NAME = ^PX509_NAME;

  PX509_CRL = ^X509_CRL;
  PPX509_CRL = ^PX509_CRL;
  Pstack_st_X509_CRL = POPENSSL_STACK;
  PPstack_st_X509_CRL = ^Pstack_st_X509_CRL;
  PX509_CRLDynArray = array of PX509_CRL;
  PX509_REVOKED = ^X509_REVOKED;
  PPX509_REVOKED = ^PX509_REVOKED;
  PX509_CRL_METHOD = pointer;
  PPX509_CRL_METHOD = ^PX509_CRL_METHOD;

  Pstack_st_X509_REVOKED = POPENSSL_STACK;
  PPstack_st_X509_REVOKED = ^Pstack_st_X509_REVOKED;
  Pstack_st_X509_EXTENSION = POPENSSL_STACK;
  PPstack_st_X509_EXTENSION = ^Pstack_st_X509_EXTENSION;

  BASIC_CONSTRAINTS_st = record
    ca: integer;
    pathlen: PASN1_INTEGER;
  end;

  BASIC_CONSTRAINTS = BASIC_CONSTRAINTS_st;
  PBASIC_CONSTRAINTS = ^BASIC_CONSTRAINTS;
  PPBASIC_CONSTRAINTS = ^PBASIC_CONSTRAINTS;

  /// convenient wrapper to a PX509_EXTENSION instance
  X509_EXTENSION = object
  public
    function BasicConstraintIsCA: boolean;
    procedure ToUtf8(out result: RawUtf8; flags: cardinal = X509V3_EXT_DEFAULT);
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PX509_EXTENSION = ^X509_EXTENSION;
  PPX509_EXTENSION = ^PX509_EXTENSION;
  PX509_EXTENSIONS = POPENSSL_STACK;
  PPX509_EXTENSIONS = ^PX509_EXTENSIONS;

  /// high-level data structure to decode and store PX509_EXTENSION fields
  TX509_Extension = object
  public
    /// the low-level X509 extension instance
    // - use ext.ToUtf8() to return its human-readable text value
    ext: PX509_EXTENSION;
    // if this extension was marked as critical
    critical: boolean;
    /// the NID of this extension
    // - use OBJ_nid2ln() or OBJ_nid2sn() to get its long or short name
    nid: integer;
    /// the value of this extension
    // - you can use value.ToUtf8() or value.ToHex()
    value: PASN1_STRING;
    /// set ext and compute critical/nid/value associated fields
    procedure SetExtension(x: PX509_EXTENSION);
  end;
  TX509_Extensions = array of TX509_Extension;

  /// convenient wrapper to a PX509_REVOKED instance
  X509_REVOKED = object
  public
    function SerialNumber: RawUtf8;
    function RevocationDate: TDateTime;
    function Reason: integer;
    function SetReason(value: integer): boolean;
    function ToBinary: RawByteString;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// convenient wrapper to a PX509_CRL instance
  X509_CRL = object
  public
    function Version: integer;
    function LastUpdate: TDateTime;
    function NextUpdate: TDateTime;
    function IssuerName: RawUtf8;
    function Revoked: Pstack_st_X509_REVOKED;
      {$ifdef HASINLINE} inline; {$endif}
    // returns the reason, CRL_REASON_NONE=-1 if none found - can check >= 0
    function IsRevoked(const serialnumber: RawUtf8): integer; overload;
    function IsRevoked(serial: PASN1_INTEGER): integer; overload;
    function AddFrom(another: PX509_CRL): integer;
    function AddFromPem(const Pem: RawUtf8): integer;
    function AddRevokedSerial(serial: PASN1_INTEGER; ca: PX509; reason: integer = 0;
      lastUpdateDays: integer = 0; nextUpdateDays: integer = 30): boolean;
    function AddRevokedCertificate(x, ca: PX509; reason: integer;
      lastUpdateDays: integer = 0; nextUpdateDays: integer = 30): boolean;
    function Extensions: Pstack_st_X509_EXTENSION;
    function GetExtensions: TX509_Extensions;
    function Extension(nid: integer): PX509_EXTENSION;
    function ToBinary: RawByteString;
    function ToPem: RawUtf8;
    function ToText: RawUtf8;
    // return the size of the signature in bytes for success and zero for failure
    function Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
    /// increment the X509 reference count to avoid premature release
    function Acquire: integer;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  PX509_PUBKEY = type pointer;
  PPX509_PUBKEY = ^PX509_PUBKEY;

  PX509_STORE = ^X509_STORE;
  PPX509_STORE = ^PX509_STORE;

  PX509_STORE_CTX = ^X509_STORE_CTX;
  PPX509_STORE_CTX = ^PX509_STORE_CTX;

  X509_STORE_CTX_verify_cb = function(p1: integer; p2: PX509_STORE_CTX): integer; cdecl;

  /// convenient wrapper to a PX509_STORE instance
  X509_STORE = object
  public
    function CertificateCount: integer;
    function CrlCount: integer;
    function MainCrlAcquired: PX509_CRL; // eventual result.Free
    function CertificatesLocked: PX509DynArray; // eventual Unlock
    function CrlsLocked: PX509_CRLDynArray;     // eventual Unlock
    procedure Lock;
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
    function StackX509(addref: boolean = true): Pstack_st_X509;
    function StackX509_CRL(addref: boolean = true): Pstack_st_X509_CRL;
    // caller should make result.Free once done (to decrease refcount)
    function BySerial(const serial: RawUtf8): PX509;
    function BySkid(const id: RawUtf8): PX509;
    function HasSerial(serial: PASN1_INTEGER): boolean;
    // returns the revocation reason
    function IsRevoked(const serial: RawUtf8): integer; overload;
    function IsRevoked(serial: PASN1_INTEGER): integer; overload;
    function SetDefaultPaths: boolean;
    // those methods will increase the certificate/CRL refcount
    function AddCertificate(x: PX509): boolean;
    function AddCertificates(const x: PX509DynArray): boolean;
    function AddCrl(c: PX509_CRL): boolean;
    // try binary DER serialization of X509 Certificate or CRL
    function AddFromBinary(const Der: RawByteString): RawUtf8;
    /// returns the number of added certificates or CRLs
    function AddCertificateFromPem(const Pem: RawUtf8;
      Serials: PRawUtf8DynArray = nil): integer;
    function AddCrlFromPem(const Pem: RawUtf8): integer;
    // - CAFile format is concatenated PEM certificates and CRLs
    // - CAFolder expects the certificates to be stored as hash.N (or hash.rN for
    // CRLs), with hash.N being X509_NAME.Hash SHA1 first four bytes - see
    // https://www.openssl.org/docs/man1.1.1/man3/X509_LOOKUP_hash_dir.html
    function SetLocations(const CAFile: RawUtf8;
      const CAFolder: RawUtf8 = ''): boolean;
    // returns 0 on success, or an error code (optionally in errstr^/errcert^)
    // - allow partial chain verification if MaxDepth<>0, and optional policy
    // verification if X509_V_FLAG_POLICY_CHECK is included in Flags - to
    // fulfill e.g. https://nvd.nist.gov/vuln/detail/CVE-2023-0466
    // - if errcert^ is set, caller should call errcert^.Free
    function Verify(x509: PX509; chain: Pstack_st_X509 = nil;
      errstr: PPUtf8Char = nil; errcert: PPX509 = nil;
      callback: X509_STORE_CTX_verify_cb = nil; MaxDepth: integer = 0;
      Flags: cardinal = X509_V_FLAG_PARTIAL_CHAIN): integer;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// convenient wrapper to a PX509_STORE_CTX instance
  X509_STORE_CTX = object
  public
    function CurrentCert: PX509;
    function CurrentError(errstr: PPUtf8Char = nil): integer;
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  PX509_OBJECT = type pointer;
  PPX509_OBJECT = ^PX509_OBJECT;
  Pstack_st_X509_OBJECT = POPENSSL_STACK;
  PPstack_st_X509_OBJECT = ^Pstack_st_X509_OBJECT;

  X509_algor_st = record
    algorithm: PASN1_OBJECT;
    parameter: PASN1_TYPE;
  end;

  X509_ALGOR = X509_algor_st;
  PX509_ALGOR = ^X509_ALGOR;
  PPX509_ALGOR = ^PX509_ALGOR;

  PX509_VAL = type pointer;

  PX509V3_CTX = ^v3_ext_ctx;

  /// used by X509.ToPkcs12Ex() method and OpenSslDefaultPkcs12 global variable
  TX509Pkcs12Format = (
   p12Default,
   p12Legacy,
   p12New,
   p12PrefixDisabled);

  /// convenient wrapper to a PX509 instance
  X509 = object
  public
    function GetSerial: PASN1_INTEGER;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectName: PX509_NAME;
      {$ifdef HASINLINE} inline; {$endif}
    function GetIssuerName: PX509_NAME;
      {$ifdef HASINLINE} inline; {$endif}
    function GetPublicKey: PEVP_PKEY;
      {$ifdef HASINLINE} inline; {$endif}
    /// the Certificate Genuine Serial Number
    // - e.g. '04:f9:25:39:39:f8:ce:79:1a:a4:0e:b3:fa:72:e3:bc:9e:d6'
    function SerialNumber: RawUtf8;
    /// the High-Level Certificate Main Subject
    // - e.g. 'CN=synopse.info'
    // - see SubjectAlternativeNames for a full set of items
    function SubjectName: RawUtf8;
    /// extract a given part of the Certificate Main Subject
    // - e.g. 'synopse.info' from SubjectName = 'CN=synopse.info'
    // - id could also be a hash name to get the GetSubjectName.ToDigest() value
    function GetSubject(const id: RawUtf8 = 'CN'): RawUtf8;
    /// an array of (DNS) Subject names covered by this Certificate
    // - will search and remove the 'DNS:' trailer by default (dns=true)
    // - e.g. ['synopse.info', 'www.synopse.info']
    function SubjectAlternativeNames(dns: boolean = true): TRawUtf8DynArray;
    /// an array of (DNS) Subject names covered by the Issuer of this Certificate
    function IssuerAlternativeNames(dns: boolean = true): TRawUtf8DynArray;
    /// the High-Level Certificate Issuer
    // - e.g. '/C=US/O=Let''s Encrypt/CN=R3'
    function IssuerName: RawUtf8;
    /// extract a given part of the Certificate Main Subject
    // - e.g. 'R3' from IssuerName = '/C=US/O=Let''s Encrypt/CN=R3'
    // - id could also be a hash name to get the GetIssuerName.ToDigest() value
    function GetIssuer(const id: RawUtf8 = 'CN'): RawUtf8;
    /// the minimum Validity timestamp of this Certificate
    function NotBefore: TDateTime;
    /// the maximum Validity timestamp of this Certificate
    function NotAfter: TDateTime;
    /// check a date/time coherency with NotBefore/NotAfter
    // - a grace period of CERT_DEPRECATION_THRESHOLD (half a day) is applied
    function IsValidDate(TimeUtc: TDateTime = 0): boolean;
    /// returns the hexadecimal SHA-1 digest of the whole certificate
    // - you can set e.g. md = EVP_sha256 to retrieve the SHA-256 digest
    function FingerPrint(md: PEVP_MD = nil): RawUtf8;
    /// verbose certificate information, returned as X509_print() huge text blob
    function PeerInfo: RawUtf8;
    /// access a Certificate extension by NID
    function Extension(nid: integer): PX509_EXTENSION;
    /// return a Certificate extension value as text by NID
    // - just a wrapper around
    // ! Extension(nid).ToUtf8(result);
    function ExtensionText(nid: integer): RawUtf8;
    /// low-level Certificate extension attributes
    function GetExtensions: TX509_Extensions;
    /// if the Certificate X509v3 Basic Constraints contains 'CA:TRUE'
    // - match kuCA flag in GetUsage/HasUsage
    function IsCA: boolean;
    /// if the Certificate issuer is itself
    function IsSelfSigned: boolean;
    /// retrieve the signature algorithm as human-readable text
    // - returns e.g. '128 ecdsa-with-SHA256' or '256 ecdsa-with-SHA512'
    // '128 RSA-SHA256' or '128 ED25519'
    // - the first number being the actual security bits of the algorithm
    // as retrieved by X509_get_signature_info()
    function GetSignatureAlgo: RawUtf8;
    /// retrieve the digest name used for the signature algorithm
    // - returns e.g. 'SHA256'
    function GetSignatureHash: RawUtf8;
    /// the X509v3 Key and Extended Key Usage Flags of this Certificate
    function GetUsage: TX509Usages;
    /// check a X509v3 Key and Extended Key Usage Flag of this Certificate
    // - fastest and easiest way of checking Certificate abilities from code
    // - OpenSSL caches the flags, so any SetUsage() won't be taken into account
    function HasUsage(u: TX509Usage): boolean;
    /// the X509v3 text Key Usage of this Certificate
    // - e.g. 'Digital Signature, Key Encipherment'
    function KeyUsage: RawUtf8;
    /// the X509v3 text Extended Key Usage of this Certificate
    // - e.g. 'TLS Web Server Authentication, TLS Web Client Authentication'
    function ExtendedKeyUsage: RawUtf8;
    /// the X509v3 Subject Key Identifier (SKID) of this Certificate
    // - e.g. '3B:C5:FB:28:27:2C:45:FE:50:03:B9:88:E4:84:1A:81:8E:F8:B5:CC'
    function SubjectKeyIdentifier: RawUtf8;
    /// the X509v3 Authority Key Identifier (AKID) of this Certificate
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    // - if there are several AKID, only returns the first
    function AuthorityKeyIdentifier: RawUtf8;
    /// set the Not Before / Not After Vailidy of this Certificate
    // - ValidDays and ExpireDays are relative to the current time - ValidDays
    // is usually -1 to avoid most clock synch issues
    function SetValidity(ValidDays, ExpireDays: integer): boolean;
    /// low-level set an extension to a X509 Certificate
    // - any previous extension with this NID will be first deleted
    // - typical nid are NID_subject_alt_name (with 'DNS:xxx'), NID_info_access
    // (as 'caIssuers;xxurlxx'), or NID_netscape_comment
    function SetExtension(nid: cardinal; const value: RawUtf8;
      issuer: PX509 = nil; subject: PX509 = nil): boolean;
    /// copy all existing extensions to a X509 Certificate
    procedure CopyExtensions(exts: PX509_EXTENSIONS);
    /// copy all existing PX509_REQ CSR extensions to a X509 Certificate
    procedure CopyCsrExtensions(req: PX509_REQ);
    /// set basic extensions
    // - any previous value with this NID will be first deleted
    function SetBasic(ca: boolean; const altnames: RawUtf8 = '';
      const subjectkey: RawUtf8 = 'hash'): boolean;
    /// set key_usage/ext_key_usage extensions
    // - any previous usage set will be first deleted
    function SetUsage(usages: TX509Usages): boolean;
    /// check if the public key of this certificate matches a given private key
    function MatchPrivateKey(pkey: PEVP_PKEY): boolean;
    /// serialize the certificate as DER raw binary
    function ToBinary: RawByteString;
    /// serialize the certificate as PEM text
    function ToPem: RawUtf8;
    /// serialize the certificate and associated private key as PKCS12 raw binary
    // - warning: default algorithm changed to AES-256-CBC with OpenSSL 3
    // https://github.com/openssl/openssl/commit/762970bd686c4aa
    // - see also ToPkcs12Ex() - as used by TCryptCertOpenSsl.Save()
    function ToPkcs12(pkey: PEVP_PKEY; const password: SpiUtf8;
      CA: Pstack_st_X509 = nil; nid_key: integer = 0; nid_cert: integer = 0;
      iter: integer = 0; mac_iter: integer = 0; md_type: PEVP_MD = nil;
      const FriendlyName: RawUtf8 = ''): RawByteString; overload;
    /// serialize the certificate and associated private key as PKCS12 raw binary
    // - can specify the actual output format
    function ToPkcs12(pkey: PEVP_PKEY; const password: SpiUtf8;
      format: TX509Pkcs12Format): RawByteString; overload;
    /// serialize the certificate and associated private key as PKCS12 raw binary
    // - this method will recognize '3des=' (PKCS12_3DES_PREFIX) and 'aes='
    // (PKCS12_AES_PREFIX) prefixes to the password text (which will be trimmed),
    // to force either legacy SHA1-3DES or new AES-256-CBC algorithm
    // - will use OpenSslDefaultPkcs12 global variable format by default
    // - as used by TCryptCertOpenSsl.Save() with cccCertWithPrivateKey and ccfBinary
    function ToPkcs12Ex(pkey: PEVP_PKEY; const password: SpiUtf8): RawByteString;
    /// increment the X509 reference count to avoid premature release
    function Acquire: integer;
    /// sign this Certificate with the supplied private key and algorithm
    // - you can use EVP_sha256 e.g. for md parameter
    // - return the size of the signature in bytes for success and zero for failure
    function Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
    /// release this X509 Certificate instance
    // - in practice, decrement the X509 reference count and free it once 0
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  PPKCS12 = ^PKCS12;
  PPPKCS12 = ^PPKCS12;
  PPKCS12_SAFEBAG = pointer;
  PPPKCS12_SAFEBAG = ^PPKCS12_SAFEBAG;
  Pstack_st_PKCS12_SAFEBAG = POPENSSL_STACK;
  PPstack_st_PKCS12_SAFEBAG = ^Pstack_st_PKCS12_SAFEBAG;

  /// convenient wrapper to a PPKCS12 instance
  PKCS12 = object
  public
    /// parse and extract the private key, certificate and CAs in this PKCS12 store
    // - use pointers to result structures, nil if one is not needed
    // - caller should call needed privatekey^.Free, cert^.Free and ca^.FreeX509
    function Extract(const password: SpiUtf8;
      privatekey: PPEVP_PKEY; cert: PPX509; ca: PPstack_st_X509): boolean;
    /// serialize the PKCS12 Structure as DER raw binary
    function ToBinary: RawByteString;
    /// release this PKCS12 Structure instance
    procedure Free;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  // OpenSSL (even 3.0!) uses an hardcoded struct for this, which is against its
  // own policy since 1.1.x to never expose internal structures :(
  v3_ext_ctx = record
    flags: integer;
    issuer_cert: PX509;
    subject_cert: PX509;
    subject_req: PX509_REQ;
    crl: PX509_CRL;
    db_meth: PX509V3_CONF_METHOD;
    db: pointer;
    issuer_pkey: PEVP_PKEY; // introduced in OpenSSL 3.0
    padding: array[0..3] of pointer; // paranoid - to avoid stack corruption
  end;

  Ptm = ^tm;
  tm = record
    tm_sec: integer;
    tm_min: integer;
    tm_hour: integer;
    tm_mday: integer;
    tm_mon: integer;
    tm_year: integer;
    tm_wday: integer;
    tm_yday: integer;
    tm_isdst: integer;
    tm_gmtoff: integer;
    tm_zone: PAnsiChar;
  end;

  {$ifdef OSWINDOWS}

  // minimal C-like definitions to mimic unixtype FPC unit on Windows
  clong = PtrInt;
  time_t = PtrInt; // may suffer Year2038 issue on CPU32 - not used in practice
  ptime_t = ^time_t;

  timeval = record
    tv_sec: integer;
    tv_usec: integer;
  end;
  PTimeVal = ^timeval;

  {$endif OSWINDOWS}

  SSL_verify_cb = function(preverify_ok: integer; x509_ctx: PX509_STORE_CTX): integer; cdecl;
  SSL_SNI_servername_cb = function(s: PSSL; ad: PInteger; arg: pointer): integer; cdecl;
  SSL_CTX_callback_ctrl_ = procedure; cdecl;
  Ppem_password_cb = function(buf: PUtf8Char; size, rwflag: integer; userdata: pointer): integer; cdecl;
  ECDH_compute_key_KDF = function(_in: pointer; inlen: PtrUInt; _out: pointer; outlen: PPtrUInt): pointer; cdecl;

  dyn_MEM_malloc_fn = function(p1: PtrUInt; p2: PUtf8Char; p3: integer): pointer; cdecl;
  dyn_MEM_realloc_fn = function(p1: pointer; p2: PtrUInt; p3: PUtf8Char; p4: integer): pointer; cdecl;
  dyn_MEM_free_fn = procedure(p1: pointer; p2: PUtf8Char; p3: integer); cdecl;

  PCRYPTO_EX_new = procedure(parent: pointer; ptr: pointer; ad: PCRYPTO_EX_DATA; idx: integer; argl: integer; argp: pointer); cdecl;
  PCRYPTO_EX_free = procedure(parent: pointer; ptr: pointer; ad: PCRYPTO_EX_DATA; idx: integer; argl: integer; argp: pointer); cdecl;
  PCRYPTO_EX_dup = function(_to: PCRYPTO_EX_DATA; from: PCRYPTO_EX_DATA; from_d: pointer; idx: integer; argl: integer; argp: pointer): integer; cdecl;

const
  /// password prefix recognized by X509.ToPkcs12Ex() to force legacy PBE-SHA1-3DES
  PKCS12_3DES_PREFIX = '3des=';
  /// password prefix recognized by X509.ToPkcs12Ex() to force new AES-256-CBC
  PKCS12_AES_PREFIX = 'aes=';

var
  /// globally set a specific algorithm for X509.ToPkcs12Ex() binary persistence
  // - see also TCryptCertOpenSsl.Save() with cccCertWithPrivateKey and ccfBinary
  // - could also disable password prefix recognition with p12PrefixDisabled
  OpenSslDefaultPkcs12: TX509Pkcs12Format;


{ ******************** OpenSSL Library Functions }


{ --------- libssl entries }

function SSL_CTX_new(meth: PSSL_METHOD): PSSL_CTX; cdecl;
procedure SSL_CTX_free(p1: PSSL_CTX); cdecl;
function SSL_CTX_set_timeout(ctx: PSSL_CTX; t: integer): integer; cdecl;
function SSL_CTX_get_timeout(ctx: PSSL_CTX): integer; cdecl;
procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: integer; callback: SSL_verify_cb); cdecl;
function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer; cdecl;
function SSL_CTX_use_RSAPrivateKey(ctx: PSSL_CTX; rsa: PRSA): integer; cdecl;
function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char;
   typ: integer): integer; cdecl;
function SSL_CTX_use_certificate(ctx: PSSL_CTX; x: PX509): integer; cdecl;
function SSL_CTX_check_private_key(ctx: PSSL_CTX): integer; cdecl;
function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; _file: PUtf8Char;
   typ: integer): integer; cdecl;
function SSL_CTX_get_cert_store(p1: PSSL_CTX): PX509_STORE; cdecl;
function SSL_CTX_load_verify_locations(ctx: PSSL_CTX;
   CAfile: PUtf8Char; CApath: PUtf8Char): integer; cdecl;
function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; _file: PUtf8Char): integer; cdecl;
function SSL_CTX_set_alpn_protos(ctx: PSSL_CTX;
   protos: PByte; protos_len: cardinal): integer; cdecl;
function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
// op/result are cardinal for OpenSSL 1.1, but QWord since OpenSSL 3.0 :(
function SSL_CTX_set_options(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
function SSL_CTX_clear_options(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
function SSL_CTX_callback_ctrl(p1: PSSL_CTX; p2: integer; p3: SSL_CTX_callback_ctrl_): integer; cdecl;
function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
function SSL_set_SSL_CTX(ssl: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl;
function SSL_shutdown(s: PSSL): integer; cdecl;
function SSL_get_error(s: PSSL; ret_code: integer): integer; cdecl;
function SSL_ctrl(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
procedure SSL_set_bio(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
function SSL_set_ex_data(ssl: PSSL; idx: integer; data: pointer): integer; cdecl;
function SSL_get_ex_data(ssl: PSSL; idx: integer): pointer; cdecl;
function SSL_get_peer_certificate(s: PSSL): PX509; cdecl;
function SSL_get_peer_cert_chain(s: PSSL): Pstack_st_X509; cdecl;
procedure SSL_free(ssl: PSSL); cdecl;
function SSL_connect(ssl: PSSL): integer; cdecl;
function SSL_accept(ssl: PSSL): integer; cdecl;
procedure SSL_set_connect_state(s: PSSL); cdecl;
procedure SSL_set_accept_state(s: PSSL); cdecl;
function SSL_read(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
function SSL_write(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
function SSL_get_state(ssl: PSSL): OSSL_HANDSHAKE_STATE; cdecl;
function SSL_pending(s: PSSL): integer; cdecl;
function SSL_set_cipher_list(s: PSSL; str: PUtf8Char): integer; cdecl;
procedure SSL_get0_alpn_selected(ssl: PSSL; data: PPByte; len: PCardinal); cdecl;
function SSL_get_servername(s: PSSL; typ: integer): PUtf8Char; cdecl;
function SSL_clear(s: PSSL): integer; cdecl;
function TLS_client_method(): PSSL_METHOD; cdecl;
function TLS_server_method(): PSSL_METHOD; cdecl;
function SSL_CTX_set_default_verify_paths(ctx: PSSL_CTX): integer; cdecl;
procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Ppem_password_cb); cdecl;
procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: pointer); cdecl;
function SSL_CTX_use_PrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char;
   typ: integer): integer; cdecl;
function SSL_CTX_set_cipher_list(p1: PSSL_CTX; str: PUtf8Char): integer; cdecl;
function SSL_set_fd(s: PSSL; fd: integer): integer; cdecl;
function SSL_get_current_cipher(s: PSSL): PSSL_CIPHER; cdecl;
function SSL_CIPHER_description(p1: PSSL_CIPHER;
   buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
function SSL_get_verify_result(ssl: PSSL): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
procedure SSL_set_hostflags(s: PSSL; flags: cardinal); cdecl;
function SSL_set1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;
function SSL_add1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;
function SSL_CTX_set_session_id_context(ctx: PSSL_CTX; sid_ctx: PByte; sid_ctx_len: cardinal): integer; cdecl;

{ --------- libcrypto entries }

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn;
  f: dyn_MEM_free_fn): integer; cdecl;
procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
function CRYPTO_get_ex_new_index(class_index: integer;
  argl: integer; argp: pointer; new_func: PCRYPTO_EX_new;
  dup_func: PCRYPTO_EX_dup; free_func: PCRYPTO_EX_free): integer; cdecl;
procedure ERR_remove_state(pid: cardinal); cdecl;
procedure ERR_error_string_n(e: cardinal; buf: PUtf8Char; len: PtrUInt); cdecl;
function ERR_get_error(): cardinal; cdecl;
procedure ERR_remove_thread_state(p1: pointer); cdecl;
function ERR_load_BIO_strings(): integer; cdecl;
function EVP_MD_CTX_create(): PEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX); cdecl;
function EVP_PKEY_size(pkey: PEVP_PKEY): integer; cdecl;
function EVP_PKEY_type(typ: integer): integer; cdecl;
function EVP_PKEY_id(pkey: PEVP_PKEY): integer; cdecl;
function EVP_PKEY_base_id(pkey: PEVP_PKEY): integer; cdecl;
function EVP_PKEY_bits(pkey: PEVP_PKEY): integer; cdecl;
procedure EVP_PKEY_free(pkey: PEVP_PKEY); cdecl;
function EVP_PKEY_decrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
function EVP_PKEY_decrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
function EVP_PKEY_encrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
function EVP_PKEY_encrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD;
  e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
function EVP_DigestUpdate(ctx: PEVP_MD_CTX; d: pointer; cnt: PtrUInt): integer; cdecl;
function EVP_DigestSignFinal(ctx: PEVP_MD_CTX;
  sigret: PByte; var siglen: PtrUInt): integer; cdecl;
function EVP_DigestVerifyInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD;
  e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
function EVP_DigestVerifyFinal(ctx: PEVP_MD_CTX; sig: PByte; siglen: PtrUInt): integer; cdecl;
function EVP_DigestSign(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
function EVP_DigestVerify(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
function EVP_SealInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PPByte; ekl: PInteger; iv: PByte; pubk: PPEVP_PKEY; npubk: integer): integer; cdecl;
function EVP_SealFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
function EVP_OpenInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PByte; ekl: integer; iv: PByte; priv: PEVP_PKEY): integer; cdecl;
function EVP_OpenFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger; _in: PByte; inl: integer): integer; cdecl;
function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer;
  d: PByte; n: PtrUInt; md: PEVP_MD_DIG; var md_len: cardinal): PByte; cdecl;
function BIO_new(typ: PBIO_METHOD): PBIO;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function BIO_free(a: PBIO): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function BIO_test_flags(b: PBIO; flags: integer): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function BIO_ctrl(bp: PBIO; cmd: integer; larg: clong; parg: pointer): clong;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function BIO_new_mem_buf(buf: pointer; len: integer): PBIO; cdecl;
function BIO_s_mem(): PBIO_METHOD;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function BIO_read(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
function BIO_write(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
function BIO_new_socket(sock: integer; close_flag: integer): PBIO; cdecl;
function X509_get_issuer_name(a: PX509): PX509_NAME; cdecl;
function X509_get_subject_name(a: PX509): PX509_NAME; cdecl;
function X509_get_pubkey(x: PX509): PEVP_PKEY; cdecl;
function X509_get_signature_nid(x: PX509): integer; cdecl;
function X509_get_signature_info(x: PX509; mdnid, pknid, secbits, flags: PInteger): integer; cdecl;
function X509_up_ref(x: PX509): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
procedure X509_STORE_free(v: PX509_STORE); cdecl;
procedure X509_STORE_CTX_free(ctx: PX509_STORE_CTX); cdecl;
procedure X509_free(a: PX509); cdecl;
function X509_new(): PX509; cdecl;
function X509_set_version(x: PX509; version: integer): integer; cdecl;
function X509_set_serialNumber(x: PX509; serial: PASN1_INTEGER): integer; cdecl;
function X509_set_issuer_name(x: PX509; name: PX509_NAME): integer; cdecl;
function X509_set_subject_name(x: PX509; name: PX509_NAME): integer; cdecl;
function X509_set_pubkey(x: PX509; pkey: PEVP_PKEY): integer; cdecl;
function X509_sign(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
function X509_REQ_new(): PX509_REQ; cdecl;
function X509_REQ_set_version(x: PX509_REQ; version: integer): integer; cdecl;
procedure X509_REQ_free(a: PX509_REQ); cdecl;
function X509_REQ_sign(x: PX509_REQ; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
function X509_REQ_verify(a: PX509_REQ; r: PEVP_PKEY): integer; cdecl;
function d2i_X509_REQ_bio(bp: PBIO; req: PPX509_REQ): PX509_REQ; cdecl;
function i2d_X509_REQ_bio(bp: PBIO; req: PX509_REQ): integer; cdecl;
function PEM_write_bio_X509_REQ(bp: PBIO; x: PX509_REQ): integer; cdecl;
function X509_REQ_get_subject_name(req: PX509_REQ): PX509_NAME; cdecl;
function X509_REQ_get_pubkey(req: PX509_REQ): PEVP_PKEY; cdecl;
function X509_REQ_set_pubkey(x: PX509_REQ; pkey: PEVP_PKEY): integer; cdecl;
function X509_getm_notBefore(x: PX509): PASN1_TIME; cdecl;
function X509_getm_notAfter(x: PX509): PASN1_TIME; cdecl;
function X509V3_EXT_conf_nid(conf: Plhash_st_CONF_VALUE; ctx: PX509V3_CTX; ext_nid: integer; value: PUtf8Char): PX509_EXTENSION; cdecl;
function X509_add_ext(x: PX509; ex: PX509_EXTENSION; loc: integer): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_delete_ext(x: PX509; loc: integer): PX509_EXTENSION; cdecl;
procedure X509V3_set_ctx(ctx: PX509V3_CTX; issuer, subject: PX509; req: PX509_REQ; crl: PX509_CRL; flags: integer);
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_gmtime_adj(s: PASN1_TIME; adj: integer): PASN1_TIME; cdecl;
procedure X509_EXTENSION_free(a: PX509_EXTENSION); cdecl;
procedure BASIC_CONSTRAINTS_free(a: PBASIC_CONSTRAINTS);
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function d2i_BASIC_CONSTRAINTS(a: PPBASIC_CONSTRAINTS; _in: PPByte; len: integer): PBASIC_CONSTRAINTS;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_NAME_add_entry_by_txt(name: PX509_NAME; field: PUtf8Char; typ: integer; bytes: PAnsiChar; len: integer; loc: integer; _set: integer): integer; cdecl;
function X509_NAME_print_ex(_out: PBIO; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
function X509_NAME_entry_count(name: PX509_NAME): integer; cdecl;
function X509_NAME_get_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
function X509_NAME_get_text_by_NID(name: PX509_NAME; nid: integer; buf: PUtf8Char; len: integer): integer; cdecl;
function X509_NAME_get_index_by_NID(name: PX509_NAME; nid: integer; lastpos: integer): integer; cdecl;
function X509_NAME_delete_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
function X509_NAME_ENTRY_get_data(ne: PX509_NAME_ENTRY): PASN1_STRING; cdecl;
function X509_NAME_ENTRY_get_object(ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl;
procedure X509_NAME_ENTRY_free(a: PX509_NAME_ENTRY); cdecl;
function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
function X509_NAME_hash(x: PX509_NAME): cardinal; cdecl;
function X509_NAME_cmp(a: PX509_NAME; b: PX509_NAME): integer; cdecl;
function d2i_X509_NAME(a: PPX509_NAME; _in: PPByte; len: integer): PX509_NAME; cdecl;
function i2d_X509_NAME(a: PX509_NAME; _out: PPByte): integer; cdecl;
function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509; cdecl;
function X509_digest(data: PX509; typ: PEVP_MD; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
function X509_get_serialNumber(x: PX509): PASN1_INTEGER;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_check_private_key(x509: PX509; pkey: PEVP_PKEY): integer; cdecl;
function X509_get_ext_count(x: PX509): integer; cdecl;
function X509_get_ext(x: PX509; loc: integer): PX509_EXTENSION;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_get_ext_by_NID(x: PX509; nid: integer; lastpos: integer): integer; cdecl;
function X509_EXTENSION_get_object(ex: PX509_EXTENSION): PASN1_OBJECT;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_EXTENSION_get_data(ne: PX509_EXTENSION): PASN1_OCTET_STRING;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_EXTENSION_get_critical(ex: PX509_EXTENSION): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_get_version(x: PX509): integer; cdecl;
function X509_get0_notBefore(x: PX509): PASN1_TIME; cdecl;
function X509_get0_notAfter(x: PX509): PASN1_TIME; cdecl;
function X509_get_extension_flags(x: PX509): cardinal; cdecl;
function X509_get_key_usage(x: PX509): cardinal; cdecl;
function X509_get_extended_key_usage(x: PX509): cardinal; cdecl;
function X509V3_EXT_print(_out: PBIO; ext: PX509_EXTENSION; flag: cardinal; indent: integer): integer; cdecl;
function i2d_X509_bio(bp: PBIO; x509: PX509): integer; cdecl;
function d2i_X509_bio(bp: PBIO; x509: PPX509): PX509; cdecl;
function PEM_read_bio_X509_AUX(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
function X509_CRL_new(): PX509_CRL; cdecl;
procedure X509_CRL_free(a: PX509_CRL); cdecl;
function X509_CRL_verify(a: PX509_CRL; r: PEVP_PKEY): integer; cdecl;
function X509_CRL_sign(x: PX509_CRL; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
function X509_CRL_dup(crl: PX509_CRL): PX509_CRL; cdecl;
function X509_CRL_up_ref(crl: PX509_CRL): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_CRL_set_version(x: PX509_CRL; version: integer): integer; cdecl;
function X509_CRL_set_issuer_name(x: PX509_CRL; name: PX509_NAME): integer; cdecl;
function X509_CRL_set_lastUpdate(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
function X509_CRL_set_nextUpdate(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
function X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME; cdecl;
function X509_CRL_get_version(crl: PX509_CRL): integer; cdecl;
function X509_CRL_get_lastUpdate(crl: PX509_CRL): PASN1_TIME; cdecl;
function X509_CRL_get_nextUpdate(crl: PX509_CRL): PASN1_TIME; cdecl;
function X509_CRL_print(bp: PBIO; x: PX509_CRL): integer; cdecl;
function d2i_X509_CRL_bio(bp: PBIO; crl: PPX509_CRL): PX509_CRL; cdecl;
function i2d_X509_CRL_bio(bp: PBIO; crl: PX509_CRL): integer; cdecl;
function PEM_write_bio_X509_CRL(bp: PBIO; x: PX509_CRL): integer; cdecl;
function PEM_read_bio_X509_CRL(bp: PBIO; x: PPX509_CRL; cb: Ppem_password_cb; u: pointer): PX509_CRL; cdecl;
function X509_CRL_add1_ext_i2d(x: PX509_CRL; nid: integer; value: pointer; crit: integer; flags: cardinal): integer; cdecl;
function X509_CRL_get_ext_d2i(x: PX509_CRL; nid: integer; crit: PInteger; idx: PInteger): pointer; cdecl;
function X509_CRL_add0_revoked(crl: PX509_CRL; rev: PX509_REVOKED): integer; cdecl;
function X509_CRL_delete_ext(x: PX509_CRL; loc: integer): PX509_EXTENSION; cdecl;
function X509_CRL_get_REVOKED(crl: PX509_CRL): Pstack_st_X509_REVOKED; cdecl;
function X509_CRL_get0_extensions(crl: PX509_CRL): Pstack_st_X509_EXTENSION; cdecl;
function X509_CRL_sort(crl: PX509_CRL): integer; cdecl;
function X509_REVOKED_new(): PX509_REVOKED; cdecl;
procedure X509_REVOKED_free(a: PX509_REVOKED); cdecl;
function X509_REVOKED_set_serialNumber(x: PX509_REVOKED; serial: PASN1_INTEGER): integer; cdecl;
function X509_REVOKED_set_revocationDate(r: PX509_REVOKED; tm: PASN1_TIME): integer; cdecl;
function X509_REVOKED_get0_serialNumber(x: PX509_REVOKED): PASN1_INTEGER; cdecl;
function X509_REVOKED_get0_revocationDate(x: PX509_REVOKED): PASN1_TIME; cdecl;
function X509_REVOKED_get_ext_d2i(x: PX509_REVOKED; nid: integer; crit: PInteger; idx: PInteger): pointer; cdecl;
function X509_REVOKED_add1_ext_i2d(x: PX509_REVOKED; nid: integer; value: pointer; crit: integer; flags: cardinal): integer; cdecl;
function d2i_X509_REVOKED(a: PPX509_REVOKED; _in: PPByte; len: integer): PX509_REVOKED; cdecl;
function i2d_X509_REVOKED(a: PX509_REVOKED; _out: PPByte): integer; cdecl;
function X509_STORE_new(): PX509_STORE; cdecl;
function X509_STORE_load_locations(ctx: PX509_STORE; _file: PUtf8Char; dir: PUtf8Char): integer; cdecl;
function X509_STORE_set_default_paths(ctx: PX509_STORE): integer; cdecl;
function X509_STORE_add_cert(ctx: PX509_STORE; x: PX509): integer; cdecl;
function X509_STORE_add_crl(ctx: PX509_STORE; x: PX509_CRL): integer; cdecl;
function X509_STORE_add_lookup(v: PX509_STORE; m: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl;
function X509_STORE_set_flags(ctx: PX509_STORE; flags: cardinal): integer; cdecl;
function X509_STORE_set1_param(ctx: PX509_STORE; pm: PX509_VERIFY_PARAM): integer; cdecl;
function X509_STORE_get0_param(ctx: PX509_STORE): PX509_VERIFY_PARAM; cdecl;
procedure X509_STORE_set_verify_cb(ctx: PX509_STORE; verify_cb: X509_STORE_CTX_verify_cb); cdecl;
function X509_STORE_lock(ctx: PX509_STORE): integer; cdecl;
function X509_STORE_unlock(ctx: PX509_STORE): integer; cdecl;
function X509_STORE_up_ref(v: PX509_STORE): integer; cdecl;
function X509_STORE_get0_objects(v: PX509_STORE): Pstack_st_X509_OBJECT; cdecl;
function X509_OBJECT_get0_X509(a: PX509_OBJECT): PX509;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_OBJECT_get0_X509_CRL(a: PX509_OBJECT): PX509_CRL;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function X509_LOOKUP_hash_dir(): PX509_LOOKUP_METHOD; cdecl;
function X509_LOOKUP_file(): PX509_LOOKUP_METHOD; cdecl;
function X509_LOOKUP_ctrl(ctx: PX509_LOOKUP; cmd: integer; argc: PUtf8Char;
  argl: integer; ret: PPUtf8Char): integer; cdecl;
function X509_load_cert_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
function X509_load_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
function X509_load_cert_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
function X509_STORE_CTX_new(): PX509_STORE_CTX; cdecl;
function X509_STORE_CTX_init(ctx: PX509_STORE_CTX; store: PX509_STORE;
  x509: PX509; chain: Pstack_st_X509): integer; cdecl;
procedure X509_STORE_CTX_set_verify_cb(ctx: PX509_STORE_CTX; verify: X509_STORE_CTX_verify_cb); cdecl;
procedure X509_STORE_CTX_set_cert(c: PX509_STORE_CTX; x: PX509); cdecl;
function X509_verify_cert(ctx: PX509_STORE_CTX): integer; cdecl;
function X509_STORE_CTX_get_error(ctx: PX509_STORE_CTX): integer; cdecl;
function X509_verify_cert_error_string(n: integer): PUtf8Char; cdecl;
function X509_verify(a: PX509; r: PEVP_PKEY): integer; cdecl;
procedure X509_STORE_CTX_set_time(ctx: PX509_STORE_CTX; flags: cardinal; t: time_t); cdecl;
function X509_STORE_CTX_set_purpose(ctx: PX509_STORE_CTX; purpose: integer): integer; cdecl;
function X509_STORE_CTX_set_trust(ctx: PX509_STORE_CTX; trust: integer): integer; cdecl;
procedure X509_STORE_CTX_set0_untrusted(ctx: PX509_STORE_CTX; sk: Pstack_st_X509); cdecl;
procedure X509_STORE_CTX_set0_param(ctx: PX509_STORE_CTX; param: PX509_VERIFY_PARAM); cdecl;
function X509_VERIFY_PARAM_new(): PX509_VERIFY_PARAM; cdecl;
procedure X509_VERIFY_PARAM_free(param: PX509_VERIFY_PARAM); cdecl;
function X509_VERIFY_PARAM_set_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
function X509_VERIFY_PARAM_clear_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
function X509_VERIFY_PARAM_get_flags(param: PX509_VERIFY_PARAM): cardinal; cdecl;
function X509_VERIFY_PARAM_set_purpose(param: PX509_VERIFY_PARAM; purpose: integer): integer; cdecl;
function X509_VERIFY_PARAM_set_trust(param: PX509_VERIFY_PARAM; trust: integer): integer; cdecl;
procedure X509_VERIFY_PARAM_set_depth(param: PX509_VERIFY_PARAM; depth: integer); cdecl;
procedure X509_VERIFY_PARAM_set_auth_level(param: PX509_VERIFY_PARAM; auth_level: integer); cdecl;
function X509_LOOKUP_load_file(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer; cdecl;
function X509_LOOKUP_add_dir(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer; cdecl;
function PKCS12_new(): PPKCS12; cdecl;
procedure PKCS12_free(a: PPKCS12); cdecl;
function PKCS12_create(pass: PUtf8Char; name: PUtf8Char; pkey: PEVP_PKEY; cert: PX509;
  ca: Pstack_st_X509; nid_key, nid_cert, iter, mac_iter, keytype: integer): PPKCS12; cdecl;
function PKCS12_set_mac(p12: PPKCS12; pass: PUtf8Char; passlen: integer;
  salt: PByte; saltlen: integer; iter: integer; md_type: PEVP_MD): integer; cdecl;
function PKCS12_add_cert(pbags: PPstack_st_PKCS12_SAFEBAG; cert: PX509): PPKCS12_SAFEBAG; cdecl;
function PKCS12_add_key(pbags: PPstack_st_PKCS12_SAFEBAG; key: PEVP_PKEY; key_usage: integer;
  iter: integer; key_nid: integer; pass: PUtf8Char): PPKCS12_SAFEBAG; cdecl;
function i2d_PKCS12_bio(bp: PBIO; p12: PPKCS12): integer; cdecl;
function d2i_PKCS12_bio(bp: PBIO; p12: PPPKCS12): PPKCS12; cdecl;
function PKCS12_newpass(p12: PPKCS12; oldpass: PUtf8Char; newpass: PUtf8Char): integer; cdecl;
function PKCS12_parse(p12: PPKCS12; pass: PUtf8Char; pkey: PPEVP_PKEY; cert: PPX509; ca: PPstack_st_X509): integer; cdecl;
function ASN1_TIME_new(): PASN1_TIME; cdecl;
procedure ASN1_TIME_free(a: PASN1_TIME); cdecl;
function ASN1_TIME_set(s: PASN1_TIME; t: time_t): PASN1_TIME; cdecl;
function ASN1_TIME_set_string_X509(s: PASN1_TIME; str: PUtf8Char): integer; cdecl;
function ASN1_TIME_to_tm(s: PASN1_TIME; tm: Ptm): integer; cdecl;
function ASN1_TIME_normalize(s: PASN1_TIME): integer; cdecl;
function OPENSSL_sk_new(cmp: OPENSSL_sk_compfunc): POPENSSL_STACK; cdecl;
procedure OPENSSL_sk_free(p1: POPENSSL_STACK); cdecl;
procedure OPENSSL_sk_pop_free(st: POPENSSL_STACK; func: OPENSSL_sk_freefunc); cdecl;
function OPENSSL_sk_delete(st: POPENSSL_STACK; loc: integer): pointer; cdecl;
function OPENSSL_sk_find(st: POPENSSL_STACK; data: pointer): integer; cdecl;
function OPENSSL_sk_push(st: POPENSSL_STACK; data: pointer): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer; cdecl;
function OPENSSL_sk_num(p1: POPENSSL_STACK): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function OPENSSL_sk_value(p1: POPENSSL_STACK; p2: integer): pointer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
function OBJ_nid2ln(n: integer): PUtf8Char; cdecl;
function OBJ_nid2sn(n: integer): PUtf8Char; cdecl;
function OBJ_txt2nid(s: PUtf8Char): integer; cdecl;
function OBJ_obj2nid(o: PASN1_OBJECT): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function ASN1_STRING_data(x: PASN1_STRING): PByte;
   {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function ASN1_STRING_length(x: PASN1_STRING): integer;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function ASN1_STRING_type(x: PASN1_STRING): integer; cdecl;
function ASN1_STRING_print_ex(_out: PBIO; str: PASN1_STRING; flags: cardinal): integer; cdecl;
function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb;
  u: pointer): PX509; cdecl;
function PEM_write_bio_X509(bp: PBIO; x: PX509): integer; cdecl;
function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
function RSA_public_encrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
function RSA_private_encrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
function RSA_public_decrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
function RSA_private_decrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
function RSA_pkey_ctx_ctrl(ctx: PEVP_PKEY_CTX; optype: integer;
  cmd: integer; p1: integer; p2: pointer): integer; cdecl;
function i2d_PrivateKey_bio(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
function d2i_PrivateKey_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
function i2d_PUBKEY_bio(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
function d2i_PUBKEY_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
function RAND_bytes(buf: PByte; num: integer): integer; cdecl;
procedure RAND_seed(buf: pointer; num: integer); cdecl;
function EVP_get_cipherbyname(name: PUtf8Char): PEVP_CIPHER; cdecl;
function EVP_get_digestbyname(name: PUtf8Char): PEVP_MD; cdecl;
function EVP_CIPHER_CTX_new(): PEVP_CIPHER_CTX; cdecl;
function EVP_CIPHER_CTX_reset(c: PEVP_CIPHER_CTX): integer; cdecl;
procedure EVP_CIPHER_CTX_free(c: PEVP_CIPHER_CTX); cdecl;
function EVP_CIPHER_CTX_copy(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer; cdecl;
function EVP_CIPHER_CTX_set_key_length(x: PEVP_CIPHER_CTX; keylen: integer): integer; cdecl;
function EVP_CIPHER_CTX_ctrl(ctx: PEVP_CIPHER_CTX; typ: integer;
  arg: integer; ptr: pointer): integer; cdecl;
function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  impl: PENGINE; key: PByte; iv: PByte; enc: integer): integer; cdecl;
function EVP_CipherInit_ex2(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  key: PByte; iv: PByte; enc: integer; params: pointer): integer; cdecl;
function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer; cdecl;
function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
function EVP_CIPHER_CTX_iv(ctx: PEVP_CIPHER_CTX): PByte; cdecl;
function EVP_MD_CTX_new(): PEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl;
function EVP_MD_CTX_md(ctx: PEVP_MD_CTX): PEVP_MD; cdecl;
function EVP_MD_flags(md: PEVP_MD): cardinal; cdecl;
function EVP_MD_size(md: PEVP_MD): integer; cdecl;
function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer; cdecl;
function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; s: PCardinal): integer; cdecl;
function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; len: PtrUInt): integer; cdecl;
function HMAC_CTX_new(): PHMAC_CTX; cdecl;
procedure HMAC_CTX_free(ctx: PHMAC_CTX); cdecl;
function HMAC_Init_ex(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD;
  impl: PENGINE): integer; cdecl;
function HMAC_Update(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
function HMAC_Final(ctx: PHMAC_CTX; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
function EVP_sha1: PEVP_MD;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function EVP_sha256: PEVP_MD;
  {$ifdef OPENSSLSTATIC} cdecl; {$else} {$ifdef FPC} inline; {$endif} {$endif}
function EC_GROUP_new_by_curve_name(nid: integer): PEC_GROUP; cdecl;
function EC_KEY_new(): PEC_KEY; cdecl;
function EC_KEY_set_group(key: PEC_KEY; group: PEC_GROUP): integer; cdecl;
function BN_bin2bn(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM; cdecl;
function BN_bn2dec(a: PBIGNUM): PUtf8Char; cdecl;
function BN_dec2bn(a: PPBIGNUM; str: PUtf8Char): integer; cdecl;
function BN_to_ASN1_INTEGER(bn: PBIGNUM; ai: PASN1_INTEGER): PASN1_INTEGER; cdecl;
function ASN1_bn_print(bp: PBIO; number: PUtf8Char;
  num: PBIGNUM; buf: PByte; off: integer): integer; cdecl; // returns hexa :(
function ASN1_INTEGER_to_BN(ai: PASN1_INTEGER; bn: PBIGNUM): PBIGNUM; cdecl;
function ASN1_INTEGER_new(): PASN1_INTEGER; cdecl;
procedure ASN1_INTEGER_free(a: PASN1_INTEGER); cdecl;
function ASN1_ENUMERATED_set(a: PASN1_ENUMERATED; v: integer): integer; cdecl;
function ASN1_ENUMERATED_get(a: PASN1_ENUMERATED): integer; cdecl;
function ASN1_ENUMERATED_new(): PASN1_ENUMERATED; cdecl;
procedure ASN1_ENUMERATED_free(a: PASN1_ENUMERATED); cdecl;
function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
function EC_POINT_hex2point(p1: PEC_GROUP; p2: PUtf8Char; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
function EC_KEY_set_public_key_affine_coordinates(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): integer; cdecl;
function ECDSA_verify(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: integer; eckey: PEC_KEY): integer; cdecl;
procedure EC_POINT_free(point: PEC_POINT); cdecl;
procedure BN_free(a: PBIGNUM); cdecl;
function BN_num_bits(a: PBIGNUM): integer; cdecl;
procedure EC_KEY_free(key: PEC_KEY); cdecl;
procedure EC_GROUP_free(group: PEC_GROUP); cdecl;
function EC_KEY_generate_key(key: PEC_KEY): integer; cdecl;
function EC_KEY_get0_private_key(key: PEC_KEY): PBIGNUM; cdecl;
function EC_KEY_set_private_key(key: PEC_KEY; prv: PBIGNUM): integer; cdecl;
function EC_KEY_get0_public_key(key: PEC_KEY): PEC_POINT; cdecl;
function EC_KEY_key2buf(key: PEC_KEY; form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
function EVP_PKEY_get0_RSA(pkey: PEVP_PKEY): Prsa_st; cdecl;
procedure RSA_get0_key(r: PRSA; n: PPBIGNUM; e: PPBIGNUM; d: PPBIGNUM); cdecl;
function X509_REQ_add_extensions(req: PX509_REQ; exts: Pstack_st_X509_EXTENSION): integer; cdecl;
function X509_REQ_get_extensions(req: PX509_REQ): Pstack_st_X509_EXTENSION; cdecl;
function EC_POINT_point2buf(group: PEC_GROUP; point: PEC_POINT;
  form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
function BN_bn2bin(a: PBIGNUM; _to: pointer): integer; cdecl;
function ECDSA_size(eckey: PEC_KEY): integer; cdecl;
function ECDSA_sign(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: PCardinal; eckey: PEC_KEY): integer; cdecl;
function EC_POINT_new(group: PEC_GROUP): PEC_POINT; cdecl;
function EC_POINT_oct2point(group: PEC_GROUP; p: PEC_POINT;
  buf: PByte; len: PtrUInt; ctx: PBN_CTX): integer; cdecl;
function ECDH_compute_key(_out: pointer; outlen: PtrUInt; pub_key: PEC_POINT;
  ecdh: PEC_KEY; KDF: ECDH_compute_key_KDF): integer; cdecl;
function EVP_PKEY_CTX_new_id(id: integer; e: PENGINE): PEVP_PKEY_CTX; cdecl;
function EVP_PKEY_paramgen_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
function EVP_PKEY_paramgen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
function EVP_PKEY_keygen_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
function EVP_PKEY_keygen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
function EVP_PKEY_CTX_ctrl(ctx: PEVP_PKEY_CTX; keytype: integer; optype: integer;
  cmd: integer; p1: integer; p2: pointer): integer; cdecl;
function EVP_PKEY_CTX_set1_pbe_pass(ctx: PEVP_PKEY_CTX;
  pass: PAnsiChar; passlen: integer): integer; cdecl;
function EVP_PKEY_CTX_set1_scrypt_salt(ctx: PEVP_PKEY_CTX;
  salt: PByte; saltlen: integer): integer; cdecl;
function EVP_PKEY_CTX_set_scrypt_N(ctx: PEVP_PKEY_CTX; n: QWord): integer; cdecl;
function EVP_PKEY_CTX_set_scrypt_r(ctx: PEVP_PKEY_CTX; r: QWord): integer; cdecl;
function EVP_PKEY_CTX_set_scrypt_p(ctx: PEVP_PKEY_CTX; p: QWord): integer; cdecl;
function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX); cdecl;
function EVP_PKEY_derive_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
function EVP_PKEY_derive_set_peer(ctx: PEVP_PKEY_CTX; peer: PEVP_PKEY): integer; cdecl;
function EVP_PKEY_derive(ctx: PEVP_PKEY_CTX; key: PByte; keylen: PPtrUInt): integer; cdecl;
function EVP_PKEY_get0_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl;
function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
function PEM_write_bio_PKCS8PrivateKey(p1: PBIO; p2: PEVP_PKEY; p3: PEVP_CIPHER;
  p4: PUtf8Char; p5: integer; p6: Ppem_password_cb; p7: pointer): integer; cdecl;
function i2d_PKCS8PrivateKey_bio(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PUtf8Char; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
function d2i_PKCS8PrivateKey_bio(bp: PBIO; x: PPEVP_PKEY;
  cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
function EVP_aes_256_cbc(): PEVP_CIPHER; cdecl;
function EVP_bf_cbc(): PEVP_CIPHER; cdecl;
function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
function OpenSSL_version_num(): cardinal; cdecl;
function OpenSSL_version(typ: integer): PUtf8Char; cdecl;
function OSSL_PROVIDER_load(libctx: POSSL_LIB_CTX; name: PAnsiChar): POSSL_PROVIDER; cdecl;
function OSSL_PROVIDER_set_default_search_path(libctx: POSSL_LIB_CTX; path: PAnsiChar): integer; cdecl;
function OSSL_PROVIDER_available(libctx: POSSL_LIB_CTX; name: PAnsiChar): integer; cdecl;
function X509_print(bp: PBIO; x: PX509): integer; cdecl;


{ ******************** OpenSSL Full API Declaration }

{$ifdef OPENSSLFULLAPI}
  // dynamic linking is verbose, so we don't support it on the Full API
  {$define OPENSSLSTATIC}
  // this huge separated .inc file is included only if needed
  {$I .\mormot.lib.openssl11.full.inc}
{$endif OPENSSLFULLAPI}


{ ******************** OpenSSL Helpers }

procedure OpenSSL_Free(ptr: pointer);

function OpenSSL_error_short(error: integer): ShortString;
procedure OpenSSL_error(error: integer; var result: RawUtf8); overload;
function OpenSSL_error(error: integer): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

function SSL_is_fatal_error(get_error: integer): boolean;
procedure SSL_get_error_text(get_error: integer; var result: RawUtf8);
procedure SSL_get_error_short(get_error: integer; var dest: shortstring);
function SSL_get_ex_new_index(l: integer; p: pointer; newf: PCRYPTO_EX_new;
  dupf: PCRYPTO_EX_dup; freef: PCRYPTO_EX_free): integer;

function SSL_CTX_set_session_cache_mode(ctx: PSSL_CTX; mode: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_add_extra_chain_cert(ctx: PSSL_CTX; cert: PX509): cardinal;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_tmp_dh(ctx: PSSL_CTX; dh: pointer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_tmp_ecdh(ctx: PSSL_CTX; ecdh: pointer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_ecdh_auto(ctx: PSSL_CTX; onoff: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_set_tlsext_host_name(const s: PSSL; const name: RawUtf8): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: SSL_SNI_servername_cb): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: pointer): integer;
  {$ifdef HASINLINE} inline; {$endif}
function SSL_set_mode(s: PSSL; version: integer): integer;
function SSL_get_mode(s: PSSL): integer;

function EVP_MD_CTX_size(ctx: PEVP_MD_CTX): integer;
function BN_num_bytes(bn: PBIGNUM): integer;
function BigNumFromDecimal(const Text: RawUtf8): PBIGNUM;
function BigNumHexFromDecimal(const Text: RawUtf8): RawUtf8;
function EVP_PKEY_CTX_set_rsa_padding(ctx: PEVP_PKEY_CTX; padding: integer): integer;
function EVP_PKEY_CTX_set_rsa_mgf1_md(ctx: PEVP_PKEY_CTX; md: PEVP_MD): integer;
function EVP_PKEY_CTX_set_rsa_oaep_md(ctx: PEVP_PKEY_CTX; md: PEVP_MD): integer;
function TmToDateTime(const t: tm): TDateTime;
function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
procedure DTLSv1_handle_timeout(s: PSSL);

/// compute the binary hash of one binary buffer
// - will use EVP_sha1 if no md instance is specified
// - return the number of bytes stored into dig
function Digest(md: PEVP_MD; buf: pointer; buflen: integer;
  out dig: THash512): integer; overload;

/// compute the hexadecimal hash of one binary buffer
// - will use EVP_sha1 if no md instance is specified
function Digest(md: PEVP_MD; buf: pointer; buflen: integer): RawUtf8; overload;

/// compute the hexadecimal hash of one binary buffer
// - will use EVP_sha1 if no md instance is specified
function Digest(md: PEVP_MD; const buf: RawByteString): RawUtf8; overload;

/// load a private key from a PEM or DER buffer, optionally with a password
// - try first with PEM text format, then will fallback to DER binary (as raw,
// PKCS#8 or PKCS#12 format)
// - can also extract the certificate stored with the key in PKCS12/.PFX format
// - caller should make result.Free once done with the result
function LoadPrivateKey(PrivateKey: pointer; PrivateKeyLen: integer;
  const Password: SpiUtf8; Pkcs12Cert: PPX509 = nil): PEVP_PKEY; overload;

/// load a private key from a PEM or DER content, optionally with a password
// - just a wrapper to the overloaded LoadPrivateKey() function
// - caller should make result.Free once done with the result
function LoadPrivateKey(const Saved: RawByteString;
  const Password: SpiUtf8 = ''; Pkcs12Cert: PPX509 = nil): PEVP_PKEY; overload;

/// load a public key from a PEM or DER buffer, optionally with a password
// - try first with PEM text format, then will fallback to DER binary
// - caller should make result.Free once done with the result
function LoadPublicKey(PublicKey: pointer; PublicKeyLen: integer;
  const Password: SpiUtf8 = ''): PEVP_PKEY; overload;

/// load a public key from a PEM or DER content, optionally with a password
// - just a wrapper to the overloaded LoadPublicKey() function
// - caller should make result.Free once done with the result
function LoadPublicKey(const Saved: RawByteString;
  const Password: SpiUtf8 = ''): PEVP_PKEY; overload;

/// convert e.g. SSL.GetPeerCertificates result as a PEM text
function PX509DynArrayToPem(const X509: PX509DynArray): RawUtf8;

/// convert e.g. SSL.GetPeerCertificates result as list of PeerInfo text
function PX509DynArrayToText(const X509: PX509DynArray): RawUtf8;

/// finalize a dynamic array of X509 instances
procedure PX509DynArrayFree(var X509: PX509DynArray);

/// create a new X509 Certificate Instance
// - with a random serial number
// - use PX509.SetValidity/SetBasic/SetUsage methods to set additional fields
// - caller should make result.Free once done with the result
function NewCertificate: PX509;

/// unserialize as a new X509 Certificate Instance
// - from DER binary as serialized by X509.ToBinary, or PEM text format
function LoadCertificate(const DerOrPem: RawByteString): PX509;

/// unserialize as a new X509 CSR Instance
// - from DER binary as serialized by X509_REQ.ToBinary, or PEM text format
// - use LoadCsr(PemToDer()) to load a PEM certificate
function LoadCsr(const Der: RawByteString): PX509_REQ;

/// unserialize one or several new X509 Certificate Instance(s) from PEM
// - from PEM concatenated text content
// - once done with the X509 instances, free them e.g. using PX509DynArrayFree()
function LoadCertificates(const Pem: RawUtf8; Max: integer = 0): PX509DynArray;

/// retrieve the OS certificates store as PX509DynArray
// - wrap LoadCertificates() over mormot.core.os.GetSystemStoreAsPem()
// - an internal cache of PX509 instances is maintained
function LoadCertificatesFromSystemStore(
  CertStores: TSystemCertificateStores = [scsCA, scsRoot];
  FlushCache: boolean = false; OnlySystemStore: boolean = false): PX509DynArray;

/// retrieve all certificates of a given system store as PX509DynArray
// - wrap LoadCertificates() over mormot.core.os.GetOneSystemStoreAsPem()
// - an internal cache of PX509 instances is maintained
function LoadCertificatesFromOneSystemStore(CertStore: TSystemCertificateStore;
  FlushCache: boolean = false): PX509DynArray;

/// create a new X509 Certificates Store Instance
function NewCertificateStore: PX509_STORE;

/// create a new X509 Certificates Store Context
function NewCertificateStoreCtx(store: PX509_STORE; x509: PX509;
  chain: Pstack_st_X509; callback: X509_STORE_CTX_verify_cb): PX509_STORE_CTX;

/// create a new X509 Certificate CRL Instance
// - then use X509_CRL.AddRevokedCertificate/AddRevokedSerial to tune the content
// - caller should make result.Free once done with the result
function NewCertificateCrl: PX509_CRL;

/// unserialize a new X509 Certificate CRL Instance
// - from DER binary as serialized by X509_CRL.ToBinary
function LoadCertificateCrl(const Der: RawByteString): PX509_CRL;

/// unserialize a new X509 Certificate CRL Instance
// - from PEM as serialized by X509_CRL.ToPem
function LoadCertificateCrlFromPem(const Pem: RawUtf8): PX509_CRL;

/// create a new X509 Certificate Request Instance
// - caller should make result.Free once done with the result
function NewCertificateRequest: PX509_REQ;

/// unserialize a new X509 Certificate Request Instance
// - from DER binary as serialized by X509_REQ.ToBinary
// - use LoadCertificateRequest(PemToDer()) to load a PEM certificate CRL
function LoadCertificateRequest(const Der: RawByteString): PX509_REQ;

/// create a new OpenSSL pointer Stack storage instance
function NewOpenSslStack: POPENSSL_STACK;

/// create a new OpenSSL PKCS12/.PFX structure instance with all given parameters
// - nid_key/nid_cert could be retrieved from OBJ_txt2nid()
function NewPkcs12(const Password: SpiUtf8; PrivKey: PEVP_PKEY; Cert: PX509;
  CA: Pstack_st_X509 = nil; nid_key: integer = 0; nid_cert: integer = 0;
  iter: integer = 0; mac_iter: integer = 0;
  const FriendlyName: RawUtf8 = ''): PPKCS12;

/// unserialize a new OpenSSL PKCS12/.PFX structure instance
function LoadPkcs12(const Der: RawByteString): PPKCS12;

/// unserialize a PKCS12/.PFX binary into its certificate and private key
// - by default no additional CA certificates are returned - caller should
// call CA^.FreeX509 if one is allocated
function ParsePkcs12(const Saved: RawByteString; const Password: SpiUtf8;
  out Cert: PX509; out PrivateKey: PEVP_PKEY; CA: PPstack_st_X509 = nil): boolean;

/// low-level SCrypt hash computation as available since OpenSSL 3.x
// - see http://www.tarsnap.com/scrypt.html and RFC 7914
// - OpenSSL is slower than mormot.crypt.other.pas i386/x86_64 tuned SSE2 code:
// $ on Win32:     RawSCrypt in 101ms, OpenSslScrypt in 157ms
// $ on Win64:     RawSCrypt in 92ms,  OpenSslScrypt in 124ms
// $ on Linux x64: RawSCrypt in 74ms,  OpenSslScrypt in 103ms
// - assigned with OpenSSL 3.x to mormot.crypt.core.pas SCrypt() redirection on
// non-Intel (e.g. ARM) platforms - where RawSCrypt() is less optimized
function OpenSslSCrypt(const Password: RawUtf8; const Salt: RawByteString;
  N, R, P, DestLen: PtrUInt): RawByteString;

type
  /// a convenient PX509 array wrapper to leverage mormot.core.os.pas PEM cache
  {$ifdef USERECORDWITHMETHODS}
  TX509Cache = record
  {$else}
  TX509Cache = object
  {$endif USERECORDWITHMETHODS}
  private
    fSafe: TLightLock; // to ensure Cache() is thread-safe
    fPem: RawUtf8;
    fX509: PX509DynArray;
  public
    /// fill res[] from pem content, using an internal cache of last instances
    // - a faster alternative to res := LoadCertificates(pem)
    procedure Cache(const pem: RawUtf8; out res: PX509DynArray);
    /// should eventually be called to release the stored PX509 instances
    procedure Done;
      {$ifdef HASINLINE} inline; {$endif}
  end;


{ ************** TLS / HTTPS Encryption Layer using OpenSSL for TCrtSocket }

type
  /// exception class raised by NewOpenSslNetTls implementation class
  EOpenSslNetTls = class(EOpenSsl);


/// OpenSSL TLS layer communication factory - as expected by mormot.net.sock.pas
// - on non-Windows systems, this unit initialization will register OpenSSL for TLS
// - on Windows systems, SChannel will be kept as default so you would need to
// set the FORCE_OPENSSL conditional, or call OpenSslInitialize() explicitly
function NewOpenSslNetTls: INetTls;

var
  /// force to true so that NewOpenSslNetTls won't disable SIG_PIPE on POSIX
  // - due to an OpenSSL limitation, which does not set socket MSG_NOSIGNAL
  // - just ignored on Windows
  NewOpenSslNetTlsNoSigPipeIntercept: boolean;

/// retrieve the peer certificates chain from a given HTTPS server URI
// - caller should call procedure PX509DynArrayFree(result) once done
function GetPeerCertFromUrl(const url: RawUtf8): PX509DynArray;

/// retrieve the peer certificates PEM chain from a given HTTPS server URI
function GetPeerCertPemFromUrl(const url: RawUtf8): RawUtf8;

/// retrieve the peer certificates information from a given HTTPS server URI
function GetPeerCertInfoFromUrl(const url: RawUtf8): RawUtf8;


implementation

uses
  mormot.crypt.core;


{ ******************** Dynamic or Static OpenSSL Library Loading }

{ EOpenSsl }

class procedure EOpenSsl.Check(caller: TObject; const method: ShortString;
  res: integer; errormsg: PRawUtf8; ssl: pointer);
begin
  if res <> OPENSSLSUCCESS then
    CheckFailed(caller, method, errormsg, ssl, res);
end;

class procedure EOpenSsl.Check(res: integer; const method: ShortString;
  ssl: pointer);
begin
  if res <> OPENSSLSUCCESS then
    CheckFailed(nil, method, nil, ssl, res);
end;

class procedure EOpenSsl.CheckFailed(caller: TObject; const method: ShortString;
  errormsg: PRawUtf8; ssl: pointer; sslretcode: integer; const context: RawUtf8);
var
  res: integer;
  msg: RawUtf8;
  exc: EOpenSsl;
begin
  if ssl = nil then
  begin
    // generic OpenSSL error (e.g. within cryptography context)
    res := ERR_get_error;    // unqueue earliest error code, or 0 if no more
    OpenSSL_error(res, msg); // get corresponding text from library
  end
  else
  begin
    // specific error within the context of ssl_*() methods during TLS process
    res := SSL_get_error(ssl, sslretcode);
    SSL_get_error_text(res, msg); // recognize SSL_ERROR_* constant and more
    PSSL(ssl).IsVerified(@msg); // append cert verif error text to msg if needed
  end;
  if errormsg <> nil then
  begin
    if errormsg^ <> '' then // caller may have set additional information
      msg := Join([msg, errormsg^]);
    errormsg^ := msg;
  end;
  if context <> '' then
    msg := Join([msg, ' ', context]);
  if caller = nil then
    exc := CreateFmt('OpenSSL %s error %d [%s]', [OpenSslVersionHexa, res, msg])
  else
    exc := CreateFmt('%s.%s: OpenSSL %s error %d [%s]',
      [ClassNameShort(caller)^, method, OpenSslVersionHexa, res, msg]);
  exc.fLastError := res;
  raise exc;
end;

{$ifdef OPENSSLSTATIC} // OpenSSL is always available when statically linked

class procedure EOpenSsl.TryNotAvailable(caller: TClass; const method: ShortString);
begin
end;

class procedure EOpenSsl.CheckAvailable(caller: TClass; const method: ShortString);
begin
end;

{$else}

class procedure EOpenSsl.TryNotAvailable(caller: TClass; const method: ShortString);
var
  name: ShortString;
begin
  if OpenSslIsAvailable then
    exit;
  if caller = nil then
    name := method
  else
    name := ClassNameShort(caller)^ + '.' + method;
  raise CreateFmt('%s: OpenSSL ' + LIB_TXT + ' not available [%s]',
    [name, openssl_initialize_errormsg])
end;

class procedure EOpenSsl.CheckAvailable(caller: TClass; const method: ShortString);
begin
  if openssl_initialized <> lsAvailable then
    TryNotAvailable(caller, method);
end;

{$endif OPENSSLSTATIC}

class function EOpenSsl.GetOpenSsl: string;
begin
  result := OpenSslVersionHexa;
end;


{ ******************** Dynamically linked OpenSSL Library Functions }

{$ifndef OPENSSLSTATIC}

{ --------- libssl entries }

type
  TLibSsl = class(TSynLibrary)
  public
    SSL_CTX_new: function(meth: PSSL_METHOD): PSSL_CTX; cdecl;
    SSL_CTX_free: procedure(p1: PSSL_CTX); cdecl;
    SSL_CTX_set_timeout: function(ctx: PSSL_CTX; t: integer): integer; cdecl;
    SSL_CTX_get_timeout: function(ctx: PSSL_CTX): integer; cdecl;
    SSL_CTX_set_verify: procedure(ctx: PSSL_CTX; mode: integer; callback: SSL_verify_cb); cdecl;
    SSL_CTX_use_PrivateKey: function(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer; cdecl;
    SSL_CTX_use_RSAPrivateKey: function(ctx: PSSL_CTX; rsa: PRSA): integer; cdecl;
    SSL_CTX_use_RSAPrivateKey_file: function(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
    SSL_CTX_use_certificate: function(ctx: PSSL_CTX; x: PX509): integer; cdecl;
    SSL_CTX_check_private_key: function(ctx: PSSL_CTX): integer; cdecl;
    SSL_CTX_use_certificate_file: function(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
    SSL_CTX_get_cert_store: function(p1: PSSL_CTX): PX509_STORE; cdecl;
    SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; CAfile: PUtf8Char; CApath: PUtf8Char): integer; cdecl;
    SSL_CTX_use_certificate_chain_file: function(ctx: PSSL_CTX; _file: PUtf8Char): integer; cdecl;
    SSL_CTX_set_alpn_protos: function(ctx: PSSL_CTX; protos: PByte; protos_len: cardinal): integer; cdecl;
    SSL_CTX_ctrl: function(ctx: PSSL_CTX; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
    SSL_CTX_set_options: pointer; // variable signature between 1.1 vs 3.0 :(
    SSL_CTX_clear_options: pointer;
    SSL_CTX_callback_ctrl: function(p1: PSSL_CTX; p2: integer; p3: SSL_CTX_callback_ctrl_): integer; cdecl;
    SSL_new: function(ctx: PSSL_CTX): PSSL; cdecl;
    SSL_set_SSL_CTX: function(ssl: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl;
    SSL_shutdown: function(s: PSSL): integer; cdecl;
    SSL_get_error: function(s: PSSL; ret_code: integer): integer; cdecl;
    SSL_ctrl: function(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
    SSL_set_bio: procedure(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
    SSL_set_ex_data: function(ssl: PSSL; idx: integer; data: pointer): integer; cdecl;
    SSL_get_ex_data: function(ssl: PSSL; idx: integer): pointer; cdecl;
    SSL_get_peer_certificate: function(s: PSSL): PX509; cdecl;
    SSL_get_peer_cert_chain: function(s: PSSL): Pstack_st_X509; cdecl;
    SSL_free: procedure(ssl: PSSL); cdecl;
    SSL_connect: function(ssl: PSSL): integer; cdecl;
    SSL_accept: function(ssl: PSSL): integer; cdecl;
    SSL_set_connect_state: procedure(s: PSSL); cdecl;
    SSL_set_accept_state: procedure(s: PSSL); cdecl;
    SSL_read: function(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_write: function(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_get_state: function(ssl: PSSL): OSSL_HANDSHAKE_STATE; cdecl;
    SSL_pending: function(s: PSSL): integer; cdecl;
    SSL_set_cipher_list: function(s: PSSL; str: PUtf8Char): integer; cdecl;
    SSL_get0_alpn_selected: procedure(ssl: PSSL; data: PPByte; len: PCardinal); cdecl;
    SSL_get_servername: function(s: PSSL; typ: integer): PUtf8Char; cdecl;
    SSL_clear: function(s: PSSL): integer; cdecl;
    TLS_client_method: function(): PSSL_METHOD; cdecl;
    TLS_server_method: function(): PSSL_METHOD; cdecl;
    SSL_CTX_set_default_verify_paths: function(ctx: PSSL_CTX): integer; cdecl;
    SSL_CTX_set_default_passwd_cb: procedure(ctx: PSSL_CTX; cb: Ppem_password_cb); cdecl;
    SSL_CTX_set_default_passwd_cb_userdata: procedure(ctx: PSSL_CTX; u: pointer); cdecl;
    SSL_CTX_use_PrivateKey_file: function(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
    SSL_CTX_set_cipher_list: function(p1: PSSL_CTX; str: PUtf8Char): integer; cdecl;
    SSL_set_fd: function(s: PSSL; fd: integer): integer; cdecl;
    SSL_get_current_cipher: function(s: PSSL): PSSL_CIPHER; cdecl;
    SSL_CIPHER_description: function(p1: PSSL_CIPHER; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
    SSL_get_verify_result: function(ssl: PSSL): integer; cdecl;
    SSL_set_hostflags: procedure(s: PSSL; flags: cardinal); cdecl;
    SSL_set1_host: function(s: PSSL; hostname: PUtf8Char): integer; cdecl;
    // expected to be the last entry in OpenSslInitialize() below
    SSL_add1_host: function(s: PSSL; hostname: PUtf8Char): integer; cdecl;
    SSL_CTX_set_session_id_context: function(ctx: PSSL_CTX; sid_ctx: PByte; sid_ctx_len: cardinal): integer; cdecl;
  end;

const
  LIBSSL_ENTRIES: array[0..57] of PAnsiChar = (
    'SSL_CTX_new',
    'SSL_CTX_free',
    'SSL_CTX_set_timeout',
    'SSL_CTX_get_timeout',
    'SSL_CTX_set_verify',
    'SSL_CTX_use_PrivateKey',
    'SSL_CTX_use_RSAPrivateKey',
    'SSL_CTX_use_RSAPrivateKey_file',
    'SSL_CTX_use_certificate',
    'SSL_CTX_check_private_key',
    'SSL_CTX_use_certificate_file',
    'SSL_CTX_get_cert_store',
    'SSL_CTX_load_verify_locations',
    'SSL_CTX_use_certificate_chain_file',
    'SSL_CTX_set_alpn_protos',
    'SSL_CTX_ctrl',
    'SSL_CTX_set_options',
    'SSL_CTX_clear_options',
    'SSL_CTX_callback_ctrl',
    'SSL_new',
    'SSL_set_SSL_CTX',
    'SSL_shutdown',
    'SSL_get_error',
    'SSL_ctrl',
    'SSL_set_bio',
    'SSL_set_ex_data',
    'SSL_get_ex_data',
    'SSL_get1_peer_certificate SSL_get_peer_certificate', // OpenSSL 3.0 / 1.1
    'SSL_get_peer_cert_chain',
    'SSL_free',
    'SSL_connect',
    'SSL_accept',
    'SSL_set_connect_state',
    'SSL_set_accept_state',
    'SSL_read',
    'SSL_write',
    'SSL_get_state',
    'SSL_pending',
    'SSL_set_cipher_list',
    'SSL_get0_alpn_selected',
    'SSL_get_servername',
    'SSL_clear',
    'TLS_client_method',
    'TLS_server_method',
    'SSL_CTX_set_default_verify_paths',
    'SSL_CTX_set_default_passwd_cb',
    'SSL_CTX_set_default_passwd_cb_userdata',
    'SSL_CTX_use_PrivateKey_file',
    'SSL_CTX_set_cipher_list',
    'SSL_set_fd',
    'SSL_get_current_cipher',
    'SSL_CIPHER_description',
    'SSL_get_verify_result',
    'SSL_set_hostflags',
    'SSL_set1_host',
    'SSL_add1_host',
    'SSL_CTX_set_session_id_context',
    nil);

var
  libssl: TLibSsl;

function SSL_CTX_new(meth: PSSL_METHOD): PSSL_CTX;
begin
  result := libssl.SSL_CTX_new(meth);
end;

procedure SSL_CTX_free(p1: PSSL_CTX);
begin
  libssl.SSL_CTX_free(p1);
end;

function SSL_CTX_set_timeout(ctx: PSSL_CTX; t: integer): integer;
begin
  result := libssl.SSL_CTX_set_timeout(ctx, t);
end;

function SSL_CTX_get_timeout(ctx: PSSL_CTX): integer;
begin
  result := libssl.SSL_CTX_get_timeout(ctx);
end;

procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: integer; callback: SSL_verify_cb);
begin
  libssl.SSL_CTX_set_verify(ctx, mode, callback);
end;

function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer;
begin
  result := libssl.SSL_CTX_use_PrivateKey(ctx, pkey);
end;

function SSL_CTX_use_RSAPrivateKey(ctx: PSSL_CTX; rsa: PRSA): integer;
begin
  result := libssl.SSL_CTX_use_RSAPrivateKey(ctx, rsa);
end;

function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer;
begin
  result := libssl.SSL_CTX_use_RSAPrivateKey_file(ctx, _file, typ);
end;

function SSL_CTX_use_certificate(ctx: PSSL_CTX; x: PX509): integer;
begin
  result := libssl.SSL_CTX_use_certificate(ctx, x);
end;

function SSL_CTX_check_private_key(ctx: PSSL_CTX): integer;
begin
  result := libssl.SSL_CTX_check_private_key(ctx);
end;

function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer;
begin
  result := libssl.SSL_CTX_use_certificate_file(ctx, _file, typ);
end;

function SSL_CTX_get_cert_store(p1: PSSL_CTX): PX509_STORE;
begin
  result := libssl.SSL_CTX_get_cert_store(p1);
end;

function SSL_CTX_load_verify_locations(ctx: PSSL_CTX; CAfile: PUtf8Char; CApath: PUtf8Char): integer;
begin
  result := libssl.SSL_CTX_load_verify_locations(ctx, CAfile, CApath);
end;

function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; _file: PUtf8Char): integer;
begin
  result := libssl.SSL_CTX_use_certificate_chain_file(ctx, _file);
end;

function SSL_CTX_set_alpn_protos(ctx: PSSL_CTX; protos: PByte; protos_len: cardinal): integer;
begin
  result := libssl.SSL_CTX_set_alpn_protos(ctx, protos, protos_len);
end;

function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: integer; larg: clong; parg: pointer): clong;
begin
  result := libssl.SSL_CTX_ctrl(ctx, cmd, larg, parg);
end;

type
  // OpenSSL 3.0 changed the SSL_CTX_set_options() parameter types to 64-bit
  TSSL_CTX_set_options32 = function(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
  TSSL_CTX_set_options64 = function(ctx: PSSL_CTX; op: qword): qword; cdecl;
  TSSL_CTX_clear_options32 = function(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
  TSSL_CTX_clear_options64 = function(ctx: PSSL_CTX; op: qword): qword; cdecl;

function SSL_CTX_set_options(ctx: PSSL_CTX; op: cardinal): cardinal;
begin
  // we publish a 32-bit 1.1 version anyway - enough for SSL_OP_LEGACY_SERVER_CONNECT
  if OpenSslVersion < OPENSSL3_VERNUM then
    result := TSSL_CTX_set_options32(libssl.SSL_CTX_set_options)(ctx, op)
  else
    result := TSSL_CTX_set_options64(libssl.SSL_CTX_set_options)(ctx, op);
end;

function SSL_CTX_clear_options(ctx: PSSL_CTX; op: cardinal): cardinal;
begin
  // we publish a 32-bit 1.1 version anyway
  if OpenSslVersion < OPENSSL3_VERNUM then
    result := TSSL_CTX_clear_options32(libssl.SSL_CTX_clear_options)(ctx, op)
  else
    result := TSSL_CTX_clear_options64(libssl.SSL_CTX_clear_options)(ctx, op);
end;

function SSL_CTX_callback_ctrl(p1: PSSL_CTX; p2: integer; p3: SSL_CTX_callback_ctrl_): integer;
begin
  result := libssl.SSL_CTX_callback_ctrl(p1, p2, p3);
end;

function SSL_new(ctx: PSSL_CTX): PSSL;
begin
  result := libssl.SSL_new(ctx);
end;

function SSL_set_SSL_CTX(ssl: PSSL; ctx: PSSL_CTX): PSSL_CTX;
begin
  result := libssl.SSL_set_SSL_CTX(ssl, ctx);
end;

function SSL_shutdown(s: PSSL): integer;
begin
  result := libssl.SSL_shutdown(s);
end;

function SSL_get_error(s: PSSL; ret_code: integer): integer;
begin
  result := libssl.SSL_get_error(s, ret_code);
end;

function SSL_ctrl(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong;
begin
  result := libssl.SSL_ctrl(ssl, cmd, larg, parg);
end;

procedure SSL_set_bio(s: PSSL; rbio: PBIO; wbio: PBIO);
begin
  libssl.SSL_set_bio(s, rbio, wbio);
end;

function SSL_set_ex_data(ssl: PSSL; idx: integer; data: pointer): integer;
begin
  result := libssl.SSL_set_ex_data(ssl, idx, data);
end;

function SSL_get_ex_data(ssl: PSSL; idx: integer): pointer;
begin
  result := libssl.SSL_get_ex_data(ssl, idx);
end;

function SSL_get_peer_certificate(s: PSSL): PX509;
begin
  result := libssl.SSL_get_peer_certificate(s);
end;

function SSL_get_peer_cert_chain(s: PSSL): Pstack_st_X509;
begin
  result := libssl.SSL_get_peer_cert_chain(s);
end;

procedure SSL_free(ssl: PSSL);
begin
  libssl.SSL_free(ssl);
end;

function SSL_connect(ssl: PSSL): integer;
begin
  result := libssl.SSL_connect(ssl);
end;

function SSL_accept(ssl: PSSL): integer;
begin
  result := libssl.SSL_accept(ssl);
end;

procedure SSL_set_connect_state(s: PSSL);
begin
  libssl.SSL_set_connect_state(s);
end;

procedure SSL_set_accept_state(s: PSSL);
begin
  libssl.SSL_set_accept_state(s);
end;

function SSL_read(ssl: PSSL; buf: pointer; num: integer): integer;
begin
  result := libssl.SSL_read(ssl, buf, num);
end;

function SSL_write(ssl: PSSL; buf: pointer; num: integer): integer;
begin
  result := libssl.SSL_write(ssl, buf, num);
end;

function SSL_get_state(ssl: PSSL): OSSL_HANDSHAKE_STATE;
begin
  result := libssl.SSL_get_state(ssl);
end;

function SSL_pending(s: PSSL): integer;
begin
  result := libssl.SSL_pending(s);
end;

function SSL_set_cipher_list(s: PSSL; str: PUtf8Char): integer;
begin
  result := libssl.SSL_set_cipher_list(s, str);
end;

procedure SSL_get0_alpn_selected(ssl: PSSL; data: PPByte; len: PCardinal);
begin
  libssl.SSL_get0_alpn_selected(ssl, data, len);
end;

function SSL_get_servername(s: PSSL; typ: integer): PUtf8Char;
begin
  result := libssl.SSL_get_servername(s, typ);
end;

function SSL_clear(s: PSSL): integer;
begin
  result := libssl.SSL_clear(s);
end;

function TLS_client_method(): PSSL_METHOD;
begin
  result := libssl.TLS_client_method;
end;

function TLS_server_method(): PSSL_METHOD;
begin
  result := libssl.TLS_server_method;
end;

function SSL_CTX_set_default_verify_paths(ctx: PSSL_CTX): integer;
begin
  result := libssl.SSL_CTX_set_default_verify_paths(ctx);
end;

procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Ppem_password_cb);
begin
  libssl.SSL_CTX_set_default_passwd_cb(ctx, cb);
end;

procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: pointer);
begin
  libssl.SSL_CTX_set_default_passwd_cb_userdata(ctx, u);
end;

function SSL_CTX_use_PrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer;
begin
  result := libssl.SSL_CTX_use_PrivateKey_file(ctx, _file, typ);
end;

function SSL_CTX_set_cipher_list(p1: PSSL_CTX; str: PUtf8Char): integer;
begin
  result := libssl.SSL_CTX_set_cipher_list(p1, str);
end;

function SSL_set_fd(s: PSSL; fd: integer): integer;
begin
  result := libssl.SSL_set_fd(s, fd);
end;

function SSL_get_current_cipher(s: PSSL): PSSL_CIPHER;
begin
  result := libssl.SSL_get_current_cipher(s);
end;

function SSL_CIPHER_description(p1: PSSL_CIPHER; buf: PUtf8Char; size: integer): PUtf8Char;
begin
  result := libssl.SSL_CIPHER_description(p1, buf, size);
end;

function SSL_get_verify_result(ssl: PSSL): integer;
begin
  result := libssl.SSL_get_verify_result(ssl);
end;

procedure SSL_set_hostflags(s: PSSL; flags: cardinal);
begin
  libssl.SSL_set_hostflags(s, flags);
end;

function SSL_set1_host(s: PSSL; hostname: PUtf8Char): integer;
begin
  result := libssl.SSL_set1_host(s, hostname);
end;

function SSL_add1_host(s: PSSL; hostname: PUtf8Char): integer;
begin
  result := libssl.SSL_add1_host(s, hostname);
end;

function SSL_CTX_set_session_id_context(ctx: PSSL_CTX; sid_ctx: PByte; sid_ctx_len: cardinal): integer;
begin
  result := libssl.SSL_CTX_set_session_id_context(ctx, sid_ctx, sid_ctx_len);
end;

{ --------- libcrypto entries }

type
  TLibCrypto = class(TSynLibrary)
  public
    CRYPTO_malloc: function(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
    CRYPTO_set_mem_functions: function (m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn; f: dyn_MEM_free_fn): integer; cdecl;
    CRYPTO_free: procedure(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
    CRYPTO_get_ex_new_index: function(class_index: integer; argl: integer; argp: pointer; new_func: PCRYPTO_EX_new; dup_func: PCRYPTO_EX_dup; free_func: PCRYPTO_EX_free): integer; cdecl;
    ERR_remove_state: procedure(pid: cardinal); cdecl;
    ERR_error_string_n: procedure(e: cardinal; buf: PUtf8Char; len: PtrUInt); cdecl;
    ERR_get_error: function(): cardinal; cdecl;
    ERR_remove_thread_state: procedure(p1: pointer); cdecl;
    ERR_load_BIO_strings: function(): integer; cdecl;
    EVP_MD_CTX_create: function(): PEVP_MD_CTX; cdecl;
    EVP_MD_CTX_destroy: procedure(ctx: PEVP_MD_CTX); cdecl;
    EVP_PKEY_size: function(pkey: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_type: function(typ: integer): integer; cdecl;
    EVP_PKEY_id: function(pkey: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_base_id: function(pkey: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_bits: function(pkey: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_free: procedure(pkey: PEVP_PKEY); cdecl;
    EVP_PKEY_decrypt_init: function(ctx: PEVP_PKEY_CTX): integer; cdecl;
    EVP_PKEY_decrypt: function(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt; input: PByte; inputLen: PtrUInt): integer; cdecl;
    EVP_PKEY_encrypt_init: function(ctx: PEVP_PKEY_CTX): integer; cdecl;
    EVP_PKEY_encrypt: function(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt; input: PByte; inputLen: PtrUInt): integer; cdecl;
    EVP_DigestSignInit: function(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
    EVP_DigestUpdate: function(ctx: PEVP_MD_CTX; d: pointer; cnt: PtrUInt): integer; cdecl;
    EVP_DigestSignFinal: function(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt): integer; cdecl;
    EVP_DigestVerifyInit: function(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
    EVP_DigestVerifyFinal: function(ctx: PEVP_MD_CTX; sig: PByte; siglen: PtrUInt): integer; cdecl;
    EVP_DigestSign: function(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
    EVP_DigestVerify: function(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
    EVP_SealInit: function(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PPByte; ekl: PInteger; iv: PByte; pubk: PPEVP_PKEY; npubk: integer): integer; cdecl;
    EVP_SealFinal: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
    EVP_OpenInit: function(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PByte; ekl: integer; iv: PByte; priv: PEVP_PKEY): integer; cdecl;
    EVP_OpenFinal: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
    EVP_EncryptUpdate: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger; _in: PByte; inl: integer): integer; cdecl;
    EVP_DecryptUpdate: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger; _in: PByte; inl: integer): integer; cdecl;
    HMAC: function(evp_md: PEVP_MD; key: pointer; key_len: integer; d: PByte; n: PtrUInt; md: PEVP_MD_DIG; var md_len: cardinal): PByte; cdecl;
    BIO_new: function(typ: PBIO_METHOD): PBIO; cdecl;
    BIO_free: function(a: PBIO): integer; cdecl;
    BIO_test_flags: function(b: PBIO; flags: integer): integer; cdecl;
    BIO_ctrl: function(bp: PBIO; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
    BIO_new_mem_buf: function(buf: pointer; len: integer): PBIO; cdecl;
    BIO_s_mem: function(): PBIO_METHOD; cdecl;
    BIO_read: function(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
    BIO_write: function(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
    BIO_new_socket: function(sock: integer; close_flag: integer): PBIO; cdecl;
    X509_get_issuer_name: function(a: PX509): PX509_NAME; cdecl;
    X509_get_subject_name: function(a: PX509): PX509_NAME; cdecl;
    X509_get_pubkey: function(x: PX509): PEVP_PKEY; cdecl;
    X509_get_signature_nid: function(x: PX509): integer; cdecl;
    X509_get_signature_info: function(x: PX509; mdnid, pknid, secbits, flags: PInteger): integer; cdecl;
    X509_up_ref: function(x: PX509): integer; cdecl;
    X509_STORE_free: procedure(v: PX509_STORE); cdecl;
    X509_STORE_CTX_free: procedure(ctx: PX509_STORE_CTX); cdecl;
    X509_free: procedure(a: PX509); cdecl;
    X509_new: function(): PX509; cdecl;
    X509_set_version: function(x: PX509; version: integer): integer; cdecl;
    X509_set_serialNumber: function(x: PX509; serial: PASN1_INTEGER): integer; cdecl;
    X509_set_issuer_name: function(x: PX509; name: PX509_NAME): integer; cdecl;
    X509_set_subject_name: function(x: PX509; name: PX509_NAME): integer; cdecl;
    X509_set_pubkey: function(x: PX509; pkey: PEVP_PKEY): integer; cdecl;
    X509_sign: function(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
    X509_REQ_new: function(): PX509_REQ; cdecl;
    X509_REQ_set_version: function(x: PX509_REQ; version: integer): integer; cdecl;
    X509_REQ_free: procedure(a: PX509_REQ); cdecl;
    X509_REQ_sign: function(x: PX509_REQ; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
    X509_REQ_verify: function(a: PX509_REQ; r: PEVP_PKEY): integer; cdecl;
    d2i_X509_REQ_bio: function(bp: PBIO; req: PPX509_REQ): PX509_REQ; cdecl;
    i2d_X509_REQ_bio: function(bp: PBIO; req: PX509_REQ): integer; cdecl;
    PEM_write_bio_X509_REQ: function(bp: PBIO; x: PX509_REQ): integer; cdecl;
    X509_REQ_get_subject_name: function(req: PX509_REQ): PX509_NAME; cdecl;
    X509_REQ_get_pubkey: function(req: PX509_REQ): PEVP_PKEY; cdecl;
    X509_REQ_set_pubkey: function(x: PX509_REQ; pkey: PEVP_PKEY): integer; cdecl;
    X509_getm_notBefore: function(x: PX509): PASN1_TIME; cdecl;
    X509_getm_notAfter: function(x: PX509): PASN1_TIME; cdecl;
    X509V3_EXT_conf_nid: function(conf: Plhash_st_CONF_VALUE; ctx: PX509V3_CTX; ext_nid: integer; value: PUtf8Char): PX509_EXTENSION; cdecl;
    X509_add_ext: function(x: PX509; ex: PX509_EXTENSION; loc: integer): integer; cdecl;
    X509_delete_ext: function(x: PX509; loc: integer): PX509_EXTENSION; cdecl;
    X509V3_set_ctx: procedure(ctx: PX509V3_CTX; issuer: PX509; subject: PX509; req: PX509_REQ; crl: PX509_CRL; flags: integer); cdecl;
    X509_gmtime_adj: function(s: PASN1_TIME; adj: integer): PASN1_TIME; cdecl;
    X509_EXTENSION_free: procedure(a: PX509_EXTENSION); cdecl;
    BASIC_CONSTRAINTS_free: procedure(a: PBASIC_CONSTRAINTS); cdecl;
    d2i_BASIC_CONSTRAINTS: function(a: PPBASIC_CONSTRAINTS; _in: PPByte; len: integer): PBASIC_CONSTRAINTS; cdecl;
    X509_NAME_add_entry_by_txt: function(name: PX509_NAME; field: PUtf8Char; typ: integer; bytes: PAnsiChar; len: integer; loc: integer; _set: integer): integer; cdecl;
    X509_NAME_print_ex: function(_out: PBIO; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
    X509_NAME_print_ex_fp: function(fp: PPointer; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
    X509_NAME_entry_count: function(name: PX509_NAME): integer; cdecl;
    X509_NAME_get_entry: function(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
    X509_NAME_get_text_by_NID: function (name: PX509_NAME; nid: integer; buf: PUtf8Char; len: integer): integer; cdecl;
    X509_NAME_get_index_by_NID: function(name: PX509_NAME; nid: integer; lastpos: integer): integer; cdecl;
    X509_NAME_delete_entry: function(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
    X509_NAME_ENTRY_get_data: function(ne: PX509_NAME_ENTRY): PASN1_STRING; cdecl;
    X509_NAME_ENTRY_get_object: function(ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl;
    X509_NAME_ENTRY_free: procedure(a: PX509_NAME_ENTRY); cdecl;
    X509_NAME_oneline: function(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
    X509_NAME_hash: function(x: PX509_NAME): cardinal; cdecl;
    X509_NAME_cmp: function(a: PX509_NAME; b: PX509_NAME): integer; cdecl;
    d2i_X509_NAME: function(a: PPX509_NAME; _in: PPByte; len: integer): PX509_NAME; cdecl;
    i2d_X509_NAME: function(a: PX509_NAME; _out: PPByte): integer; cdecl;
    X509_STORE_CTX_get_current_cert: function(ctx: PX509_STORE_CTX): PX509; cdecl;
    X509_digest: function(data: PX509; typ: PEVP_MD; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
    X509_get_serialNumber: function(x: PX509): PASN1_INTEGER; cdecl;
    X509_check_private_key: function(x509: PX509; pkey: PEVP_PKEY): integer; cdecl;
    X509_get_ext_count: function(x: PX509): integer; cdecl;
    X509_get_ext: function(x: PX509; loc: integer): PX509_EXTENSION; cdecl;
    X509_get_ext_by_NID: function(x: PX509; nid: integer; lastpos: integer): integer; cdecl;
    X509_EXTENSION_get_object: function(ex: PX509_EXTENSION): PASN1_OBJECT; cdecl;
    X509_EXTENSION_get_data: function(ne: PX509_EXTENSION): PASN1_OCTET_STRING; cdecl;
    X509_EXTENSION_get_critical: function(ex: PX509_EXTENSION): integer; cdecl;
    X509_get_version: function(x: PX509): integer; cdecl;
    X509_get0_notBefore: function(x: PX509): PASN1_TIME; cdecl;
    X509_get0_notAfter: function(x: PX509): PASN1_TIME; cdecl;
    X509_get_extension_flags: function(x: PX509): cardinal; cdecl;
    X509_get_key_usage: function(x: PX509): cardinal; cdecl;
    X509_get_extended_key_usage: function(x: PX509): cardinal; cdecl;
    X509V3_EXT_print: function(_out: PBIO; ext: PX509_EXTENSION; flag: cardinal; indent: integer): integer; cdecl;
    i2d_X509_bio: function(bp: PBIO; x509: PX509): integer; cdecl;
    d2i_X509_bio: function(bp: PBIO; x509: PPX509): PX509; cdecl;
    PEM_read_bio_X509_AUX: function(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
    X509_CRL_new: function(): PX509_CRL; cdecl;
    X509_CRL_free: procedure(a: PX509_CRL); cdecl;
    X509_CRL_verify: function(a: PX509_CRL; r: PEVP_PKEY): integer; cdecl;
    X509_CRL_sign: function(x: PX509_CRL; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
    X509_CRL_dup: function(crl: PX509_CRL): PX509_CRL; cdecl;
    X509_CRL_up_ref: function(crl: PX509_CRL): integer; cdecl;
    X509_CRL_set_version: function(x: PX509_CRL; version: integer): integer; cdecl;
    X509_CRL_set_issuer_name: function(x: PX509_CRL; name: PX509_NAME): integer; cdecl;
    X509_CRL_set_lastUpdate: function(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
    X509_CRL_set_nextUpdate: function(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
    X509_CRL_get_issuer: function(crl: PX509_CRL): PX509_NAME; cdecl;
    X509_CRL_get_version: function(crl: PX509_CRL): integer; cdecl;
    X509_CRL_get_lastUpdate: function(crl: PX509_CRL): PASN1_TIME; cdecl;
    X509_CRL_get_nextUpdate: function(crl: PX509_CRL): PASN1_TIME; cdecl;
    X509_CRL_print: function(bp: PBIO; x: PX509_CRL): integer; cdecl;
    d2i_X509_CRL_bio: function(bp: PBIO; crl: PPX509_CRL): PX509_CRL; cdecl;
    i2d_X509_CRL_bio: function(bp: PBIO; crl: PX509_CRL): integer; cdecl;
    PEM_write_bio_X509_CRL: function(bp: PBIO; x: PX509_CRL): integer; cdecl;
    PEM_read_bio_X509_CRL: function(bp: PBIO; x: PPX509_CRL; cb: Ppem_password_cb; u: pointer): PX509_CRL; cdecl;
    X509_CRL_add1_ext_i2d: function(x: PX509_CRL; nid: integer; value: pointer; crit: integer; flags: cardinal): integer; cdecl;
    X509_CRL_get_ext_d2i: function(x: PX509_CRL; nid: integer; crit: PInteger; idx: PInteger): pointer; cdecl;
    X509_CRL_add0_revoked: function(crl: PX509_CRL; rev: PX509_REVOKED): integer; cdecl;
    X509_CRL_delete_ext: function(x: PX509_CRL; loc: integer): PX509_EXTENSION; cdecl;
    X509_CRL_get_REVOKED: function(crl: PX509_CRL): Pstack_st_X509_REVOKED; cdecl;
    X509_CRL_get0_extensions: function(crl: PX509_CRL): Pstack_st_X509_EXTENSION; cdecl;
    X509_CRL_sort: function(crl: PX509_CRL): integer; cdecl;
    X509_REVOKED_new: function(): PX509_REVOKED; cdecl;
    X509_REVOKED_free: procedure(a: PX509_REVOKED); cdecl;
    X509_REVOKED_set_serialNumber: function(x: PX509_REVOKED; serial: PASN1_INTEGER): integer; cdecl;
    X509_REVOKED_set_revocationDate: function(r: PX509_REVOKED; tm: PASN1_TIME): integer; cdecl;
    X509_REVOKED_get0_serialNumber: function(x: PX509_REVOKED): PASN1_INTEGER; cdecl;
    X509_REVOKED_get0_revocationDate: function(x: PX509_REVOKED): PASN1_TIME; cdecl;
    X509_REVOKED_get_ext_d2i: function(x: PX509_REVOKED; nid: integer; crit: PInteger; idx: PInteger): pointer; cdecl;
    X509_REVOKED_add1_ext_i2d: function(x: PX509_REVOKED; nid: integer; value: pointer; crit: integer; flags: cardinal): integer; cdecl;
    d2i_X509_REVOKED: function(a: PPX509_REVOKED; _in: PPByte; len: integer): PX509_REVOKED; cdecl;
    i2d_X509_REVOKED: function(a: PX509_REVOKED; _out: PPByte): integer; cdecl;
    X509_STORE_new: function(): PX509_STORE; cdecl;
    X509_STORE_load_locations: function(ctx: PX509_STORE; _file: PUtf8Char; dir: PUtf8Char): integer; cdecl;
    X509_STORE_set_default_paths: function(ctx: PX509_STORE): integer; cdecl;
    X509_STORE_add_cert: function(ctx: PX509_STORE; x: PX509): integer; cdecl;
    X509_STORE_add_crl: function(ctx: PX509_STORE; x: PX509_CRL): integer; cdecl;
    X509_STORE_add_lookup: function(v: PX509_STORE; m: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl;
    X509_STORE_set_flags: function(ctx: PX509_STORE; flags: cardinal): integer; cdecl;
    X509_STORE_set1_param: function(ctx: PX509_STORE; pm: PX509_VERIFY_PARAM): integer; cdecl;
    X509_STORE_get0_param: function(ctx: PX509_STORE): PX509_VERIFY_PARAM; cdecl;
    X509_STORE_set_verify_cb: procedure(ctx: PX509_STORE; verify_cb: X509_STORE_CTX_verify_cb); cdecl;
    X509_STORE_lock: function(ctx: PX509_STORE): integer; cdecl;
    X509_STORE_unlock: function(ctx: PX509_STORE): integer; cdecl;
    X509_STORE_up_ref: function(v: PX509_STORE): integer; cdecl;
    X509_STORE_get0_objects: function(v: PX509_STORE): Pstack_st_X509_OBJECT; cdecl;
    X509_OBJECT_get0_X509: function(a: PX509_OBJECT): PX509; cdecl;
    X509_OBJECT_get0_X509_CRL: function(a: PX509_OBJECT): PX509_CRL; cdecl;
    X509_LOOKUP_hash_dir: function(): PX509_LOOKUP_METHOD; cdecl;
    X509_LOOKUP_file: function(): PX509_LOOKUP_METHOD; cdecl;
    X509_LOOKUP_ctrl: function(ctx: PX509_LOOKUP; cmd: integer; argc: PUtf8Char; argl: integer; ret: PPUtf8Char): integer; cdecl;
    X509_load_cert_file: function(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
    X509_load_crl_file: function(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
    X509_load_cert_crl_file: function(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
    X509_STORE_CTX_new: function(): PX509_STORE_CTX; cdecl;
    X509_STORE_CTX_init: function(ctx: PX509_STORE_CTX; store: PX509_STORE; x509: PX509; chain: Pstack_st_X509): integer; cdecl;
    X509_STORE_CTX_set_verify_cb: procedure(ctx: PX509_STORE_CTX; verify: X509_STORE_CTX_verify_cb); cdecl;
    X509_STORE_CTX_set_cert: procedure(c: PX509_STORE_CTX; x: PX509); cdecl;
    X509_verify_cert: function(ctx: PX509_STORE_CTX): integer; cdecl;
    X509_STORE_CTX_get_error: function(ctx: PX509_STORE_CTX): integer; cdecl;
    X509_verify_cert_error_string: function(n: integer): PUtf8Char; cdecl;
    X509_verify: function(a: PX509; r: PEVP_PKEY): integer; cdecl;
    X509_STORE_CTX_set_time: procedure(ctx: PX509_STORE_CTX; flags: cardinal; t: time_t); cdecl;
    X509_STORE_CTX_set_purpose: function(ctx: PX509_STORE_CTX; purpose: integer): integer; cdecl;
    X509_STORE_CTX_set_trust: function(ctx: PX509_STORE_CTX; trust: integer): integer; cdecl;
    X509_STORE_CTX_set0_untrusted: procedure(ctx: PX509_STORE_CTX; sk: Pstack_st_X509); cdecl;
    X509_STORE_CTX_set0_param: procedure(ctx: PX509_STORE_CTX; param: PX509_VERIFY_PARAM); cdecl;
    X509_VERIFY_PARAM_new: function(): PX509_VERIFY_PARAM; cdecl;
    X509_VERIFY_PARAM_free: procedure(param: PX509_VERIFY_PARAM); cdecl;
    X509_VERIFY_PARAM_set_flags: function(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
    X509_VERIFY_PARAM_clear_flags: function(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
    X509_VERIFY_PARAM_get_flags: function(param: PX509_VERIFY_PARAM): cardinal; cdecl;
    X509_VERIFY_PARAM_set_purpose: function(param: PX509_VERIFY_PARAM; purpose: integer): integer; cdecl;
    X509_VERIFY_PARAM_set_trust: function(param: PX509_VERIFY_PARAM; trust: integer): integer; cdecl;
    X509_VERIFY_PARAM_set_depth: procedure(param: PX509_VERIFY_PARAM; depth: integer); cdecl;
    X509_VERIFY_PARAM_set_auth_level: procedure(param: PX509_VERIFY_PARAM; auth_level: integer); cdecl;
    PKCS12_new: function(): PPKCS12; cdecl;
    PKCS12_free: procedure(a: PPKCS12); cdecl;
    PKCS12_create: function(pass: PUtf8Char; name: PUtf8Char; pkey: PEVP_PKEY; cert: PX509; ca: Pstack_st_X509; nid_key: integer; nid_cert: integer; iter: integer; mac_iter: integer; keytype: integer): PPKCS12; cdecl;
    PKCS12_set_mac: function(p12: PPKCS12; pass: PUtf8Char; passlen: integer; salt: PByte; saltlen: integer; iter: integer; md_type: PEVP_MD): integer; cdecl;
    PKCS12_add_cert: function(pbags: PPstack_st_PKCS12_SAFEBAG; cert: PX509): PPKCS12_SAFEBAG; cdecl;
    PKCS12_add_key: function(pbags: PPstack_st_PKCS12_SAFEBAG; key: PEVP_PKEY; key_usage: integer; iter: integer; key_nid: integer; pass: PUtf8Char): PPKCS12_SAFEBAG; cdecl;
    i2d_PKCS12_bio: function(bp: PBIO; p12: PPKCS12): integer; cdecl;
    d2i_PKCS12_bio: function(bp: PBIO; p12: PPPKCS12): PPKCS12; cdecl;
    PKCS12_newpass: function(p12: PPKCS12; oldpass: PUtf8Char; newpass: PUtf8Char): integer; cdecl;
    PKCS12_parse: function(p12: PPKCS12; pass: PUtf8Char; pkey: PPEVP_PKEY; cert: PPX509; ca: PPstack_st_X509): integer; cdecl;
    ASN1_TIME_new: function(): PASN1_TIME; cdecl;
    ASN1_TIME_free: procedure(a: PASN1_TIME); cdecl;
    ASN1_TIME_set: function(s: PASN1_TIME; t: time_t): PASN1_TIME; cdecl;
    ASN1_TIME_set_string_X509: function(s: PASN1_TIME; str: PUtf8Char): integer; cdecl;
    ASN1_TIME_to_tm: function(s: PASN1_TIME; tm: Ptm): integer; cdecl;
    ASN1_TIME_normalize: function(s: PASN1_TIME): integer; cdecl;
    OPENSSL_sk_new: function(cmp: OPENSSL_sk_compfunc): POPENSSL_STACK; cdecl;
    OPENSSL_sk_free: procedure(p1: POPENSSL_STACK); cdecl;
    OPENSSL_sk_pop_free: procedure(st: POPENSSL_STACK; func: OPENSSL_sk_freefunc); cdecl;
    OPENSSL_sk_delete: function(st: POPENSSL_STACK; loc: integer): pointer; cdecl;
    OPENSSL_sk_find: function(st: POPENSSL_STACK; data: pointer): integer; cdecl;
    OPENSSL_sk_push: function(st: POPENSSL_STACK; data: pointer): integer; cdecl;
    OPENSSL_sk_pop: function(st: POPENSSL_STACK): pointer; cdecl;
    OPENSSL_sk_num: function(p1: POPENSSL_STACK): integer; cdecl;
    OPENSSL_sk_value: function(p1: POPENSSL_STACK; p2: integer): pointer; cdecl;
    ASN1_BIT_STRING_get_bit: function(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
    OBJ_nid2ln: function(n: integer): PUtf8Char; cdecl;
    OBJ_nid2sn: function(n: integer): PUtf8Char; cdecl;
    OBJ_txt2nid: function(s: PUtf8Char): integer; cdecl;
    OBJ_obj2nid: function(o: PASN1_OBJECT): integer; cdecl;
    ASN1_STRING_data: function(x: PASN1_STRING): PByte; cdecl;
    ASN1_STRING_length: function(x: PASN1_STRING): integer; cdecl;
    ASN1_STRING_type: function(x: PASN1_STRING): integer; cdecl;
    ASN1_STRING_print_ex: function(_out: PBIO; str: PASN1_STRING; flags: cardinal): integer; cdecl;
    PEM_read_bio_X509: function(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
    PEM_write_bio_X509: function(bp: PBIO; x: PX509): integer; cdecl;
    PEM_read_bio_PrivateKey: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_PUBKEY: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_RSAPublicKey: function(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
    PEM_read_bio_RSAPrivateKey: function(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
    RSA_public_encrypt: function(flen: integer; from: PByte; _to: PByte; rsa: PRSA; padding: integer): integer; cdecl;
    RSA_private_encrypt: function(flen: integer; from: PByte; _to: PByte; rsa: PRSA; padding: integer): integer; cdecl;
    RSA_public_decrypt: function(flen: integer; from: PByte; _to: PByte; rsa: PRSA; padding: integer): integer; cdecl;
    RSA_private_decrypt: function(flen: integer; from: PByte; _to: PByte; rsa: PRSA; padding: integer): integer; cdecl;
    RSA_pkey_ctx_ctrl: function(ctx: PEVP_PKEY_CTX; optype: integer; cmd: integer; p1: integer; p2: pointer): integer; cdecl;
    i2d_PrivateKey_bio: function(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
    d2i_PrivateKey_bio: function(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
    i2d_PUBKEY_bio: function(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
    d2i_PUBKEY_bio: function(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
    RAND_bytes: function(buf: PByte; num: integer): integer; cdecl;
    RAND_seed: procedure(buf: pointer; num: integer); cdecl;
    EVP_get_cipherbyname: function(name: PUtf8Char): PEVP_CIPHER; cdecl;
    EVP_get_digestbyname: function(name: PUtf8Char): PEVP_MD; cdecl;
    EVP_CIPHER_CTX_new: function(): PEVP_CIPHER_CTX; cdecl;
    EVP_CIPHER_CTX_reset: function(c: PEVP_CIPHER_CTX): integer; cdecl;
    EVP_CIPHER_CTX_free: procedure(c: PEVP_CIPHER_CTX); cdecl;
    EVP_CIPHER_CTX_copy: function(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer; cdecl;
    EVP_CIPHER_CTX_set_key_length: function(x: PEVP_CIPHER_CTX; keylen: integer): integer; cdecl;
    EVP_CIPHER_CTX_ctrl: function(ctx: PEVP_CIPHER_CTX; typ: integer; arg: integer; ptr: pointer): integer; cdecl;
    EVP_CipherInit_ex: function(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte; enc: integer): integer; cdecl;
    EVP_CipherInit_ex2: function(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
      key: PByte; iv: PByte; enc: integer; params: pointer): integer; cdecl;
    EVP_CipherUpdate: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger; _in: PByte; inl: integer): integer; cdecl;
    EVP_CipherFinal_ex: function(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer; cdecl;
    EVP_CIPHER_CTX_set_padding: function(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
    EVP_CIPHER_CTX_iv: function(ctx: PEVP_CIPHER_CTX): PByte; cdecl;
    EVP_MD_CTX_new: function: PEVP_MD_CTX; cdecl;
    EVP_MD_CTX_free: procedure(ctx: PEVP_MD_CTX); cdecl;
    EVP_MD_CTX_md: function(ctx: PEVP_MD_CTX): PEVP_MD; cdecl;
    EVP_MD_flags: function(md: PEVP_MD): cardinal; cdecl;
    EVP_MD_size: function(md: PEVP_MD): integer; cdecl;
    EVP_DigestInit_ex: function(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer; cdecl;
    EVP_DigestFinal_ex: function(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; s: PCardinal): integer; cdecl;
    EVP_DigestFinalXOF: function(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; len: PtrUInt): integer; cdecl;
    HMAC_CTX_new: function: PHMAC_CTX; cdecl;
    HMAC_CTX_free: procedure(ctx: PHMAC_CTX); cdecl;
    HMAC_Init_ex: function(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD; impl: PENGINE): integer; cdecl;
    HMAC_Update: function(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
    HMAC_Final: function(ctx: PHMAC_CTX; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
    EVP_sha1: function: PEVP_MD; cdecl;
    EVP_sha256: function: PEVP_MD; cdecl;
    EC_GROUP_new_by_curve_name: function(nid: integer): PEC_GROUP; cdecl;
    EC_KEY_new: function(): PEC_KEY; cdecl;
    EC_KEY_set_group: function(key: PEC_KEY; group: PEC_GROUP): integer; cdecl;
    BN_bin2bn: function(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM; cdecl;
    BN_bn2dec: function(a: PBIGNUM): PUtf8Char; cdecl;
    BN_dec2bn: function(a: PPBIGNUM; str: PUtf8Char): integer; cdecl;
    BN_to_ASN1_INTEGER: function(bn: PBIGNUM; ai: PASN1_INTEGER): PASN1_INTEGER; cdecl;
    ASN1_bn_print: function(bp: PBIO; number: PUtf8Char; num: PBIGNUM; buf: PByte; off: integer): integer; cdecl;
    ASN1_INTEGER_to_BN: function(ai: PASN1_INTEGER; bn: PBIGNUM): PBIGNUM; cdecl;
    ASN1_INTEGER_new: function(): PASN1_INTEGER; cdecl;
    ASN1_INTEGER_free: procedure(a: PASN1_INTEGER); cdecl;
    ASN1_ENUMERATED_set: function(a: PASN1_ENUMERATED; v: integer): integer; cdecl;
    ASN1_ENUMERATED_get: function(a: PASN1_ENUMERATED): integer; cdecl;
    ASN1_ENUMERATED_new: function(): PASN1_ENUMERATED; cdecl;
    ASN1_ENUMERATED_free: procedure(a: PASN1_ENUMERATED); cdecl;
    EC_POINT_bn2point: function(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
    EC_POINT_hex2point: function(p1: PEC_GROUP; p2: PUtf8Char; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
    EC_KEY_set_public_key: function(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
    EC_KEY_set_public_key_affine_coordinates: function(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): integer; cdecl;
    ECDSA_verify: function(typ: integer; dgst: PByte; dgstlen: integer; sig: PByte; siglen: integer; eckey: PEC_KEY): integer; cdecl;
    EC_POINT_free: procedure(point: PEC_POINT); cdecl;
    BN_free: procedure(a: PBIGNUM); cdecl;
    BN_num_bits: function(a: PBIGNUM): integer; cdecl;
    EC_KEY_free: procedure(key: PEC_KEY); cdecl;
    EC_GROUP_free: procedure(group: PEC_GROUP); cdecl;
    EC_KEY_generate_key: function(key: PEC_KEY): integer; cdecl;
    EC_KEY_get0_private_key: function(key: PEC_KEY): PBIGNUM; cdecl;
    EC_KEY_set_private_key: function(key: PEC_KEY; prv: PBIGNUM): integer; cdecl;
    EC_KEY_get0_public_key: function(key: PEC_KEY): PEC_POINT; cdecl;
    EC_KEY_key2buf: function(key: PEC_KEY; form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
    EVP_PKEY_get0_RSA: function(pkey: PEVP_PKEY): Prsa_st; cdecl;
    RSA_get0_key: procedure(r: PRSA; n: PPBIGNUM; e: PPBIGNUM; d: PPBIGNUM); cdecl;
    X509_REQ_add_extensions: function(req: PX509_REQ; exts: Pstack_st_X509_EXTENSION): integer; cdecl;
    X509_REQ_get_extensions: function(req: PX509_REQ): Pstack_st_X509_EXTENSION; cdecl;
    EC_POINT_point2buf: function(group: PEC_GROUP; point: PEC_POINT; form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
    BN_bn2bin: function(a: PBIGNUM; _to: pointer): integer; cdecl;
    ECDSA_size: function(eckey: PEC_KEY): integer; cdecl;
    ECDSA_sign: function(typ: integer; dgst: PByte; dgstlen: integer; sig: PByte; siglen: PCardinal; eckey: PEC_KEY): integer; cdecl;
    EC_POINT_new: function(group: PEC_GROUP): PEC_POINT; cdecl;
    EC_POINT_oct2point: function(group: PEC_GROUP; p: PEC_POINT; buf: PByte; len: PtrUInt; ctx: PBN_CTX): integer; cdecl;
    ECDH_compute_key: function(_out: pointer; outlen: PtrUInt; pub_key: PEC_POINT; ecdh: PEC_KEY; KDF: ECDH_compute_key_KDF): integer; cdecl;
    EVP_PKEY_CTX_new_id: function(id: integer; e: PENGINE): PEVP_PKEY_CTX; cdecl;
    EVP_PKEY_paramgen_init: function(ctx: PEVP_PKEY_CTX): integer; cdecl;
    EVP_PKEY_paramgen: function(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
    EVP_PKEY_keygen_init: function(ctx: PEVP_PKEY_CTX): integer; cdecl;
    EVP_PKEY_keygen: function(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
    EVP_PKEY_CTX_ctrl: function(ctx: PEVP_PKEY_CTX; keytype: integer; optype: integer; cmd: integer; p1: integer; p2: pointer): integer; cdecl;
    EVP_PKEY_CTX_set1_pbe_pass: function(ctx: PEVP_PKEY_CTX; pass: PAnsiChar; passlen: integer): integer; cdecl;
    EVP_PKEY_CTX_set1_scrypt_salt: function(ctx: PEVP_PKEY_CTX; salt: PByte; saltlen: integer): integer; cdecl;
    EVP_PKEY_CTX_set_scrypt_N: function(ctx: PEVP_PKEY_CTX; n: QWord): integer; cdecl;
    EVP_PKEY_CTX_set_scrypt_r: function(ctx: PEVP_PKEY_CTX; r: QWord): integer; cdecl;
    EVP_PKEY_CTX_set_scrypt_p: function(ctx: PEVP_PKEY_CTX; p: QWord): integer; cdecl;
    EVP_PKEY_CTX_new: function(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
    EVP_PKEY_CTX_free: procedure(ctx: PEVP_PKEY_CTX); cdecl;
    EVP_PKEY_derive_init: function(ctx: PEVP_PKEY_CTX): integer; cdecl;
    EVP_PKEY_derive_set_peer: function(ctx: PEVP_PKEY_CTX; peer: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_derive: function(ctx: PEVP_PKEY_CTX; key: PByte; keylen: PPtrUInt): integer; cdecl;
    EVP_PKEY_get0_EC_KEY: function(pkey: PEVP_PKEY): PEC_KEY; cdecl;
    PEM_write_bio_PrivateKey: function(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER; kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
    PEM_write_bio_PKCS8PrivateKey: function(p1: PBIO; p2: PEVP_PKEY; p3: PEVP_CIPHER; p4: PUtf8Char; p5: integer; p6: Ppem_password_cb; p7: pointer): integer; cdecl;
    i2d_PKCS8PrivateKey_bio: function(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER; kstr: PUtf8Char; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
    d2i_PKCS8PrivateKey_bio: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    EVP_aes_256_cbc: function(): PEVP_CIPHER; cdecl;
    EVP_bf_cbc: function(): PEVP_CIPHER; cdecl;
    PEM_write_bio_PUBKEY: function(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
    OpenSSL_version_num: function(): cardinal; cdecl;
    OpenSSL_version: function(typ: integer): PUtf8Char; cdecl;
    OSSL_PROVIDER_load: function(libctx: POSSL_LIB_CTX; name: PAnsiChar): POSSL_PROVIDER; cdecl;
    OSSL_PROVIDER_set_default_search_path: function(libctx: POSSL_LIB_CTX; path: PAnsiChar): integer; cdecl;
    OSSL_PROVIDER_available: function(libctx: POSSL_LIB_CTX; name: PAnsiChar): integer; cdecl;
    // expected to be the last entry in OpenSslInitialize() below
    X509_print: function(bp: PBIO; x: PX509): integer; cdecl;
  end;

const
  LIBCRYPTO_ENTRIES: array[0..348] of PAnsiChar = (
    'CRYPTO_malloc',
    'CRYPTO_set_mem_functions',
    'CRYPTO_free',
    'CRYPTO_get_ex_new_index',
    'ERR_remove_state',
    'ERR_error_string_n',
    'ERR_get_error',
    'ERR_remove_thread_state',
    'ERR_load_BIO_strings',
    'EVP_MD_CTX_new',
    'EVP_MD_CTX_free',
    'EVP_PKEY_get_size EVP_PKEY_size', // OpenSSL 3.0 / 1.1 alternate names
    'EVP_PKEY_type',
    'EVP_PKEY_get_id EVP_PKEY_id',
    'EVP_PKEY_get_base_id EVP_PKEY_base_id',
    'EVP_PKEY_get_bits EVP_PKEY_bits',
    'EVP_PKEY_free',
    'EVP_PKEY_decrypt_init',
    'EVP_PKEY_decrypt',
    'EVP_PKEY_encrypt_init',
    'EVP_PKEY_encrypt',
    'EVP_DigestSignInit',
    'EVP_DigestUpdate',
    'EVP_DigestSignFinal',
    'EVP_DigestVerifyInit',
    'EVP_DigestVerifyFinal',
    '?EVP_DigestSign',   // not defined in oldest versions
    '?EVP_DigestVerify',
    'EVP_SealInit',
    'EVP_SealFinal',
    'EVP_OpenInit',
    'EVP_OpenFinal',
    'EVP_EncryptUpdate',
    'EVP_DecryptUpdate',
    'HMAC',
    'BIO_new',
    'BIO_free',
    'BIO_test_flags',
    'BIO_ctrl',
    'BIO_new_mem_buf',
    'BIO_s_mem',
    'BIO_read',
    'BIO_write',
    'BIO_new_socket',
    'X509_get_issuer_name',
    'X509_get_subject_name',
    'X509_get_pubkey',
    'X509_get_signature_nid',
    '?X509_get_signature_info',
    'X509_up_ref',
    'X509_STORE_free',
    'X509_STORE_CTX_free',
    'X509_free',
    'X509_new',
    'X509_set_version',
    'X509_set_serialNumber',
    'X509_set_issuer_name',
    'X509_set_subject_name',
    'X509_set_pubkey',
    'X509_sign',
    'X509_REQ_new',
    'X509_REQ_set_version',
    'X509_REQ_free',
    'X509_REQ_sign',
    'X509_REQ_verify',
    'd2i_X509_REQ_bio',
    'i2d_X509_REQ_bio',
    'PEM_write_bio_X509_REQ',
    'X509_REQ_get_subject_name',
    'X509_REQ_get_pubkey',
    'X509_REQ_set_pubkey',
    'X509_getm_notBefore',
    'X509_getm_notAfter',
    'X509V3_EXT_conf_nid',
    'X509_add_ext',
    'X509_delete_ext',
    'X509V3_set_ctx',
    'X509_gmtime_adj',
    'X509_EXTENSION_free',
    'BASIC_CONSTRAINTS_free',
    'd2i_BASIC_CONSTRAINTS',
    'X509_NAME_add_entry_by_txt',
    'X509_NAME_print_ex',
    'X509_NAME_print_ex_fp',
    'X509_NAME_entry_count',
    'X509_NAME_get_entry',
    'X509_NAME_get_text_by_NID',
    'X509_NAME_get_index_by_NID',
    'X509_NAME_delete_entry',
    'X509_NAME_ENTRY_get_data',
    'X509_NAME_ENTRY_get_object',
    'X509_NAME_ENTRY_free',
    'X509_NAME_oneline',
    '?X509_NAME_hash', // not defined on OpenSSL 3.0 -> ? = ignored by now
    'X509_NAME_cmp',
    'd2i_X509_NAME',
    'i2d_X509_NAME',
    'X509_STORE_CTX_get_current_cert',
    'X509_digest',
    'X509_get_serialNumber',
    'X509_check_private_key',
    'X509_get_ext_count',
    'X509_get_ext',
    'X509_get_ext_by_NID',
    'X509_EXTENSION_get_object',
    'X509_EXTENSION_get_data',
    'X509_EXTENSION_get_critical',
    'X509_get_version',
    'X509_get0_notBefore',
    'X509_get0_notAfter',
    'X509_get_extension_flags',
    'X509_get_key_usage',
    'X509_get_extended_key_usage',
    'X509V3_EXT_print',
    'i2d_X509_bio',
    'd2i_X509_bio',
    'PEM_read_bio_X509_AUX',
    'X509_CRL_new',
    'X509_CRL_free',
    'X509_CRL_verify',
    'X509_CRL_sign',
    'X509_CRL_dup',
    'X509_CRL_up_ref',
    'X509_CRL_set_version',
    'X509_CRL_set_issuer_name',
    'X509_CRL_set1_lastUpdate',
    'X509_CRL_set1_nextUpdate',
    'X509_CRL_get_issuer',
    'X509_CRL_get_version',
    'X509_CRL_get_lastUpdate',
    'X509_CRL_get_nextUpdate',
    'X509_CRL_print',
    'd2i_X509_CRL_bio',
    'i2d_X509_CRL_bio',
    'PEM_write_bio_X509_CRL',
    'PEM_read_bio_X509_CRL',
    'X509_CRL_add1_ext_i2d',
    'X509_CRL_get_ext_d2i',
    'X509_CRL_add0_revoked',
    'X509_CRL_delete_ext',
    'X509_CRL_get_REVOKED',
    'X509_CRL_get0_extensions',
    'X509_CRL_sort',
    'X509_REVOKED_new',
    'X509_REVOKED_free',
    'X509_REVOKED_set_serialNumber',
    'X509_REVOKED_set_revocationDate',
    'X509_REVOKED_get0_serialNumber',
    'X509_REVOKED_get0_revocationDate',
    'X509_REVOKED_get_ext_d2i',
    'X509_REVOKED_add1_ext_i2d',
    'd2i_X509_REVOKED',
    'i2d_X509_REVOKED',
    'X509_STORE_new',
    'X509_STORE_load_locations',
    'X509_STORE_set_default_paths',
    'X509_STORE_add_cert',
    'X509_STORE_add_crl',
    'X509_STORE_add_lookup',
    'X509_STORE_set_flags',
    'X509_STORE_set1_param',
    'X509_STORE_get0_param',
    'X509_STORE_set_verify_cb',
    'X509_STORE_lock',
    'X509_STORE_unlock',
    'X509_STORE_up_ref',
    'X509_STORE_get0_objects',
    'X509_OBJECT_get0_X509',
    'X509_OBJECT_get0_X509_CRL',
    'X509_LOOKUP_hash_dir',
    'X509_LOOKUP_file',
    'X509_LOOKUP_ctrl',
    'X509_load_cert_file',
    'X509_load_crl_file',
    'X509_load_cert_crl_file',
    'X509_STORE_CTX_new',
    'X509_STORE_CTX_init',
    'X509_STORE_CTX_set_verify_cb',
    'X509_STORE_CTX_set_cert',
    'X509_verify_cert',
    'X509_STORE_CTX_get_error',
    'X509_verify_cert_error_string',
    'X509_verify',
    'X509_STORE_CTX_set_time',
    'X509_STORE_CTX_set_purpose',
    'X509_STORE_CTX_set_trust',
    'X509_STORE_CTX_set0_untrusted',
    'X509_STORE_CTX_set0_param',
    'X509_VERIFY_PARAM_new',
    'X509_VERIFY_PARAM_free',
    'X509_VERIFY_PARAM_set_flags',
    'X509_VERIFY_PARAM_clear_flags',
    'X509_VERIFY_PARAM_get_flags',
    'X509_VERIFY_PARAM_set_purpose',
    'X509_VERIFY_PARAM_set_trust',
    'X509_VERIFY_PARAM_set_depth',
    'X509_VERIFY_PARAM_set_auth_level',
    'PKCS12_new',
    'PKCS12_free',
    'PKCS12_create',
    'PKCS12_set_mac',
    'PKCS12_add_cert',
    'PKCS12_add_key',
    'i2d_PKCS12_bio',
    'd2i_PKCS12_bio',
    'PKCS12_newpass',
    'PKCS12_parse',
    'ASN1_TIME_new',
    'ASN1_TIME_free',
    'ASN1_TIME_set',
    '?ASN1_TIME_set_string_X509',
    '?ASN1_TIME_to_tm',
    '?ASN1_TIME_normalize',
    'OPENSSL_sk_new',
    'OPENSSL_sk_free',
    'OPENSSL_sk_pop_free',
    'OPENSSL_sk_delete',
    'OPENSSL_sk_find',
    'OPENSSL_sk_push',
    'OPENSSL_sk_pop',
    'OPENSSL_sk_num',
    'OPENSSL_sk_value',
    'ASN1_BIT_STRING_get_bit',
    'OBJ_nid2ln',
    'OBJ_nid2sn',
    'OBJ_txt2nid',
    'OBJ_obj2nid',
    'ASN1_STRING_data ASN1_STRING_get0_data', // alternate names
    'ASN1_STRING_length',
    'ASN1_STRING_type',
    'ASN1_STRING_print_ex',
    'PEM_read_bio_X509',
    'PEM_write_bio_X509',
    'PEM_read_bio_PrivateKey',
    'PEM_read_bio_PUBKEY',
    'PEM_read_bio_RSAPublicKey',
    'PEM_read_bio_RSAPrivateKey',
    'RSA_public_encrypt',
    'RSA_private_encrypt',
    'RSA_public_decrypt',
    'RSA_private_decrypt',
    '?RSA_pkey_ctx_ctrl',
    'i2d_PrivateKey_bio',
    'd2i_PrivateKey_bio',
    'i2d_PUBKEY_bio',
    'd2i_PUBKEY_bio',
    'RAND_bytes',
    'RAND_seed',
    'EVP_get_cipherbyname',
    'EVP_get_digestbyname',
    'EVP_CIPHER_CTX_new',
    'EVP_CIPHER_CTX_reset',
    'EVP_CIPHER_CTX_free',
    'EVP_CIPHER_CTX_copy',
    'EVP_CIPHER_CTX_set_key_length',
    'EVP_CIPHER_CTX_ctrl',
    'EVP_CipherInit_ex',
    '?EVP_CipherInit_ex2',    // OpenSSL 3.0 only
    'EVP_CipherUpdate',
    'EVP_CipherFinal_ex',
    'EVP_CIPHER_CTX_set_padding',
    'EVP_CIPHER_CTX_iv',
    'EVP_MD_CTX_new',
    'EVP_MD_CTX_free',
    'EVP_MD_CTX_md',
    'EVP_MD_get_flags EVP_MD_flags', // OpenSSL 3.0 / 1.1 alternate names
    'EVP_MD_get_size EVP_MD_size',
    'EVP_DigestInit_ex',
    'EVP_DigestFinal_ex',
    '?EVP_DigestFinalXOF', // not defined in oldest versions
    'HMAC_CTX_new',
    'HMAC_CTX_free',
    'HMAC_Init_ex',
    'HMAC_Update',
    'HMAC_Final',
    'EVP_sha1',
    'EVP_sha256',
    'EC_GROUP_new_by_curve_name',
    'EC_KEY_new',
    'EC_KEY_set_group',
    'BN_bin2bn',
    'BN_bn2dec',
    'BN_dec2bn',
    'BN_to_ASN1_INTEGER',
    'ASN1_bn_print',
    'ASN1_INTEGER_to_BN',
    'ASN1_INTEGER_new',
    'ASN1_INTEGER_free',
    'ASN1_ENUMERATED_set',
    'ASN1_ENUMERATED_get',
    'ASN1_ENUMERATED_new',
    'ASN1_ENUMERATED_free',
    'EC_POINT_bn2point',
    'EC_POINT_hex2point',
    'EC_KEY_set_public_key',
    'EC_KEY_set_public_key_affine_coordinates',
    'ECDSA_verify',
    'EC_POINT_free',
    'BN_free',
    'BN_num_bits',
    'EC_KEY_free',
    'EC_GROUP_free',
    'EC_KEY_generate_key',
    'EC_KEY_get0_private_key',
    'EC_KEY_set_private_key',
    'EC_KEY_get0_public_key',
    'EC_KEY_key2buf',
    'EVP_PKEY_get0_RSA',
    'RSA_get0_key',
    'X509_REQ_add_extensions',
    'X509_REQ_get_extensions',
    'EC_POINT_point2buf',
    'BN_bn2bin',
    'ECDSA_size',
    'ECDSA_sign',
    'EC_POINT_new',
    'EC_POINT_oct2point',
    'ECDH_compute_key',
    'EVP_PKEY_CTX_new_id',
    'EVP_PKEY_paramgen_init',
    'EVP_PKEY_paramgen',
    'EVP_PKEY_keygen_init',
    'EVP_PKEY_keygen',
    'EVP_PKEY_CTX_ctrl',
    '?EVP_PKEY_CTX_set1_pbe_pass', // macros, not real functions on OpenSSL 1.1
    '?EVP_PKEY_CTX_set1_scrypt_salt',
    '?EVP_PKEY_CTX_set_scrypt_N',
    '?EVP_PKEY_CTX_set_scrypt_r',
    '?EVP_PKEY_CTX_set_scrypt_p',
    'EVP_PKEY_CTX_new',
    'EVP_PKEY_CTX_free',
    'EVP_PKEY_derive_init',
    'EVP_PKEY_derive_set_peer',
    'EVP_PKEY_derive',
    'EVP_PKEY_get0_EC_KEY',
    'PEM_write_bio_PrivateKey',
    'PEM_write_bio_PKCS8PrivateKey',
    'i2d_PKCS8PrivateKey_bio',
    'd2i_PKCS8PrivateKey_bio',
    'EVP_aes_256_cbc',
    'EVP_bf_cbc',
    'PEM_write_bio_PUBKEY',
    'OpenSSL_version_num',
    'OpenSSL_version',
    '?OSSL_PROVIDER_load',                    // OpenSSL 3 only
    '?OSSL_PROVIDER_set_default_search_path', // OpenSSL 3 only
    '?OSSL_PROVIDER_available',               // OpenSSL 3 only
    'X509_print',
    nil);

var
  libcrypto: TLibCrypto;

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer;
begin
  result := libcrypto.CRYPTO_malloc(num, _file, line);
end;

function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn;
  f: dyn_MEM_free_fn): integer;
begin
  result := libcrypto.CRYPTO_set_mem_functions(m, r, f);
end;

procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer);
begin
  libcrypto.CRYPTO_free(ptr, _file, line);
end;

function CRYPTO_get_ex_new_index(class_index: integer; argl: integer; argp: pointer;
  new_func: PCRYPTO_EX_new; dup_func: PCRYPTO_EX_dup; free_func: PCRYPTO_EX_free): integer; cdecl;
begin
  result := libcrypto.CRYPTO_get_ex_new_index(
    class_index, argl, argp, new_func, dup_func, free_func);
end;

procedure ERR_remove_state(pid: cardinal);
begin
  libcrypto.ERR_remove_state(pid);
end;

procedure ERR_error_string_n(e: cardinal; buf: PUtf8Char; len: PtrUInt);
begin
  libcrypto.ERR_error_string_n(e, buf, len);
end;

function ERR_get_error(): cardinal;
begin
  result := libcrypto.ERR_get_error;
end;

procedure ERR_remove_thread_state(p1: pointer);
begin
  libcrypto.ERR_remove_thread_state(p1);
end;

function ERR_load_BIO_strings(): integer;
begin
  result := libcrypto.ERR_load_BIO_strings;
end;

function EVP_MD_CTX_create(): PEVP_MD_CTX;
begin
  result := libcrypto.EVP_MD_CTX_create;
end;

procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX);
begin
  libcrypto.EVP_MD_CTX_destroy(ctx);
end;

function EVP_PKEY_size(pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_size(pkey);
end;

function EVP_PKEY_type(typ: integer): integer;
begin
  result := libcrypto.EVP_PKEY_type(typ);
end;

function EVP_PKEY_id(pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_id(pkey);
end;

function EVP_PKEY_base_id(pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_base_id(pkey);
end;

function EVP_PKEY_bits(pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_bits(pkey);
end;

procedure EVP_PKEY_free(pkey: PEVP_PKEY);
begin
  libcrypto.EVP_PKEY_free(pkey);
end;

function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX;
   typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_DigestSignInit(ctx, pctx, typ, e, pkey);
end;

function EVP_DigestUpdate(ctx: PEVP_MD_CTX; d: pointer; cnt: PtrUInt): integer;
begin
  result := libcrypto.EVP_DigestUpdate(ctx, d, cnt);
end;

function EVP_DigestSignFinal(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt): integer;
begin
  result := libcrypto.EVP_DigestSignFinal(ctx, sigret, siglen);
end;

function EVP_DigestVerifyInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX;
   typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_DigestVerifyInit(ctx, pctx, typ, e, pkey);
end;

function EVP_DigestVerifyFinal(ctx: PEVP_MD_CTX; sig: PByte; siglen: PtrUInt): integer;
begin
  result := libcrypto.EVP_DigestVerifyFinal(ctx, sig, siglen);
end;

function EVP_DigestSign(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt;
   tbs: PByte; tbslen: PtrUInt): integer;
begin
  if Assigned(libcrypto.EVP_DigestSign) then
    // new 1.1/3.x API - as required e.g. by ED25519
    result := libcrypto.EVP_DigestSign(ctx, sigret, siglen, tbs, tbslen)
  else
  begin
    // fallback for oldest OpenSSL versions
    if sigret = nil then
      result := OPENSSLSUCCESS
    else // = EVP_DigestSignUpdate macro
      result := libcrypto.EVP_DigestUpdate(ctx, tbs, tbslen);
    if result = OPENSSLSUCCESS then
      result := libcrypto.EVP_DigestSignFinal(ctx, sigret, siglen);
  end;
end;

function EVP_DigestVerify(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt;
   tbs: PByte; tbslen: PtrUInt): integer;
begin
  if Assigned(libcrypto.EVP_DigestVerify) then
    // new 1.1/3.x API - as required e.g. by ED25519
    result := libcrypto.EVP_DigestVerify(ctx, sigret, siglen, tbs, tbslen)
  else
  begin
    // fallback for oldest OpenSSL versions
    if sigret = nil then
      result := OPENSSLSUCCESS
    else // = EVP_DigestVerifyUpdate macro
      result := libcrypto.EVP_DigestUpdate(ctx, tbs, tbslen);
    if result = OPENSSLSUCCESS then
      result := libcrypto.EVP_DigestVerifyFinal(ctx, sigret, siglen);
  end;
end;

function EVP_SealInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER;
   ek: PPByte; ekl: PInteger; iv: PByte; pubk: PPEVP_PKEY; npubk: integer): integer;
begin
  result := libcrypto.EVP_SealInit(ctx, typ, ek, ekl, iv, pubk, npubk);
end;

function EVP_SealFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer;
begin
  result := libcrypto.EVP_SealFinal(ctx, _out, outl);
end;

function EVP_OpenInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER;
   ek: PByte; ekl: integer; iv: PByte; priv: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_OpenInit(ctx, typ, ek, ekl, iv, priv);
end;

function EVP_OpenFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer;
begin
  result := libcrypto.EVP_OpenFinal(ctx, _out, outl);
end;

function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer;
begin
  result := libcrypto.EVP_EncryptUpdate(ctx, _out, outl, _in, inl);
end;

function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer;
begin
  result := libcrypto.EVP_DecryptUpdate(ctx, _out, outl, _in, inl);
end;

function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer; d: PByte; n: PtrUInt;
   md: PEVP_MD_DIG; var md_len: cardinal): PByte;
begin
  result := libcrypto.HMAC(evp_md, key, key_len, d, n, md, md_len);
end;

function BIO_new(typ: PBIO_METHOD): PBIO;
begin
  result := libcrypto.BIO_new(typ);
end;

function BIO_free(a: PBIO): integer;
begin
  result := libcrypto.BIO_free(a);
end;

function BIO_test_flags(b: PBIO; flags: integer): integer;
begin
  result := libcrypto.BIO_test_flags(b, flags);
end;

function BIO_ctrl(bp: PBIO; cmd: integer; larg: clong; parg: pointer): clong;
begin
  result := libcrypto.BIO_ctrl(bp, cmd, larg, parg);
end;

function BIO_new_mem_buf(buf: pointer; len: integer): PBIO;
begin
  result := libcrypto.BIO_new_mem_buf(buf, len);
end;

function BIO_s_mem(): PBIO_METHOD;
begin
  result := libcrypto.BIO_s_mem;
end;

function BIO_read(b: PBIO; data: pointer; dlen: integer): integer;
begin
  result := libcrypto.BIO_read(b, data, dlen);
end;

function BIO_write(b: PBIO; data: pointer; dlen: integer): integer;
begin
  result := libcrypto.BIO_write(b, data, dlen);
end;

function BIO_new_socket(sock: integer; close_flag: integer): PBIO;
begin
  result := libcrypto.BIO_new_socket(sock, close_flag);
end;

function X509_get_issuer_name(a: PX509): PX509_NAME;
begin
  result := libcrypto.X509_get_issuer_name(a);
end;

function X509_get_subject_name(a: PX509): PX509_NAME;
begin
  result := libcrypto.X509_get_subject_name(a);
end;

function X509_get_pubkey(x: PX509): PEVP_PKEY;
begin
  result := libcrypto.X509_get_pubkey(x);
end;

function X509_get_signature_nid(x: PX509): integer;
begin
  result := libcrypto.X509_get_signature_nid(x);
end;

function X509_get_signature_info(x: PX509; mdnid, pknid, secbits, flags: PInteger): integer;
begin
  if Assigned(libcrypto.X509_get_signature_info) then
    result := libcrypto.X509_get_signature_info(x, mdnid, pknid, secbits, flags)
  else
    result := 0; // unsupported
end;

function X509_up_ref(x: PX509): integer;
begin
  result := libcrypto.X509_up_ref(x);
end;

procedure X509_STORE_free(v: PX509_STORE);
begin
  libcrypto.X509_STORE_free(v);
end;

procedure X509_STORE_CTX_free(ctx: PX509_STORE_CTX);
begin
  libcrypto.X509_STORE_CTX_free(ctx);
end;

procedure X509_free(a: PX509);
begin
  libcrypto.X509_free(a);
end;

function X509_new(): PX509;
begin
  result := libcrypto.X509_new();
end;

function X509_set_version(x: PX509; version: integer): integer;
begin
  result := libcrypto.X509_set_version(x, version);
end;

function X509_set_serialNumber(x: PX509; serial: PASN1_INTEGER): integer;
begin
  result := libcrypto.X509_set_serialNumber(x, serial);
end;

function X509_set_issuer_name(x: PX509; name: PX509_NAME): integer;
begin
  result := libcrypto.X509_set_issuer_name(x, name);
end;

function X509_set_subject_name(x: PX509; name: PX509_NAME): integer;
begin
  result := libcrypto.X509_set_subject_name(x, name);
end;

function X509_set_pubkey(x: PX509; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_set_pubkey(x, pkey);
end;

function X509_sign(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  result := libcrypto.X509_sign(x, pkey, md);
end;

function X509_REQ_new(): PX509_REQ;
begin
  result := libcrypto.X509_REQ_new();
end;

function X509_REQ_set_version(x: PX509_REQ; version: integer): integer;
begin
  result := libcrypto.X509_REQ_set_version(x, version);
end;

procedure X509_REQ_free(a: PX509_REQ);
begin
  libcrypto.X509_REQ_free(a);
end;

function X509_REQ_sign(x: PX509_REQ; pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  result := libcrypto.X509_REQ_sign(x, pkey, md);
end;

function X509_REQ_verify(a: PX509_REQ; r: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_REQ_verify(a, r);
end;

function d2i_X509_REQ_bio(bp: PBIO; req: PPX509_REQ): PX509_REQ;
begin
  result := libcrypto.d2i_X509_REQ_bio(bp, req);
end;

function i2d_X509_REQ_bio(bp: PBIO; req: PX509_REQ): integer;
begin
  result := libcrypto.i2d_X509_REQ_bio(bp, req);
end;

function PEM_write_bio_X509_REQ(bp: PBIO; x: PX509_REQ): integer;
begin
  result := libcrypto.PEM_write_bio_X509_REQ(bp, x);
end;

function X509_REQ_get_subject_name(req: PX509_REQ): PX509_NAME;
begin
  result := libcrypto.X509_REQ_get_subject_name(req);
end;

function X509_REQ_get_pubkey(req: PX509_REQ): PEVP_PKEY;
begin
  result := libcrypto.X509_REQ_get_pubkey(req);
end;

function X509_REQ_set_pubkey(x: PX509_REQ; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_REQ_set_pubkey(x, pkey);
end;

function X509_getm_notBefore(x: PX509): PASN1_TIME;
begin
  result := libcrypto.X509_getm_notBefore(x);
end;

function X509_getm_notAfter(x: PX509): PASN1_TIME;
begin
  result := libcrypto.X509_getm_notAfter(x);
end;

function X509V3_EXT_conf_nid(conf: Plhash_st_CONF_VALUE; ctx: PX509V3_CTX;
   ext_nid: integer; value: PUtf8Char): PX509_EXTENSION;
begin
  result := libcrypto.X509V3_EXT_conf_nid(conf, ctx, ext_nid, value);
end;

function X509_add_ext(x: PX509; ex: PX509_EXTENSION; loc: integer): integer;
begin
  result := libcrypto.X509_add_ext(x, ex, loc);
end;

function X509_delete_ext(x: PX509; loc: integer): PX509_EXTENSION;
begin
  result := libcrypto.X509_delete_ext(x, loc);
end;

procedure X509V3_set_ctx(ctx: PX509V3_CTX; issuer, subject: PX509;
   req: PX509_REQ; crl: PX509_CRL; flags: integer);
begin
  libcrypto.X509V3_set_ctx(ctx, issuer, subject, req, crl, flags);
end;

function X509_gmtime_adj(s: PASN1_TIME; adj: integer): PASN1_TIME;
begin
  result := libcrypto.X509_gmtime_adj(s, adj);
end;

procedure X509_EXTENSION_free(a: PX509_EXTENSION);
begin
  libcrypto.X509_EXTENSION_free(a);
end;

procedure BASIC_CONSTRAINTS_free(a: PBASIC_CONSTRAINTS);
begin
  libcrypto.BASIC_CONSTRAINTS_free(a);
end;

function d2i_BASIC_CONSTRAINTS(a: PPBASIC_CONSTRAINTS;
  _in: PPByte; len: integer): PBASIC_CONSTRAINTS;
begin
  result := libcrypto.d2i_BASIC_CONSTRAINTS(a, _in, len);
end;

function X509_NAME_add_entry_by_txt(name: PX509_NAME; field: PUtf8Char;
   typ: integer; bytes: PAnsiChar; len: integer; loc: integer; _set: integer): integer;
begin
  result := libcrypto.X509_NAME_add_entry_by_txt(name, field, typ, bytes, len, loc, _set);
end;

function X509_NAME_print_ex(_out: PBIO; nm: PX509_NAME; indent: integer;
   flags: cardinal): integer;
begin
  result := libcrypto.X509_NAME_print_ex(_out, nm, indent, flags);
end;

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME;
   indent: integer; flags: cardinal): integer;
begin
  result := libcrypto.X509_NAME_print_ex_fp(fp, nm, indent, flags);
end;

function X509_NAME_entry_count(name: PX509_NAME): integer;
begin
  result := libcrypto.X509_NAME_entry_count(name);
end;

function X509_NAME_get_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY;
begin
  result := libcrypto.X509_NAME_get_entry(name, loc);
end;

function X509_NAME_get_text_by_NID(name: PX509_NAME; nid: integer;
   buf: PUtf8Char; len: integer): integer;
begin
  result := libcrypto.X509_NAME_get_text_by_NID(name, nid, buf, len);
end;

function X509_NAME_get_index_by_NID(name: PX509_NAME; nid, lastpos: integer): integer;
begin
  result := libcrypto.X509_NAME_get_index_by_NID(name, nid, lastpos);
end;

function X509_NAME_delete_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY;
begin
  result := libcrypto.X509_NAME_delete_entry(name, loc);
end;

function X509_NAME_ENTRY_get_data(ne: PX509_NAME_ENTRY): PASN1_STRING;
begin
  result := libcrypto.X509_NAME_ENTRY_get_data(ne);
end;

function X509_NAME_ENTRY_get_object(ne: PX509_NAME_ENTRY): PASN1_OBJECT;
begin
  result := libcrypto.X509_NAME_ENTRY_get_object(ne);
end;

procedure X509_NAME_ENTRY_free(a: PX509_NAME_ENTRY);
begin
  libcrypto.X509_NAME_ENTRY_free(a);
end;

function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char;
begin
  result := libcrypto.X509_NAME_oneline(a, buf, size);
end;

function X509_NAME_hash(x: PX509_NAME): cardinal;
begin
  if Assigned(libcrypto.X509_NAME_hash) then
    result := libcrypto.X509_NAME_hash(x)
  else
    result := 0; // unsupported
end;

function X509_NAME_cmp(a: PX509_NAME; b: PX509_NAME): integer;
begin
  result := libcrypto.X509_NAME_cmp(a, b);
end;

function d2i_X509_NAME(a: PPX509_NAME; _in: PPByte; len: integer): PX509_NAME;
begin
  result := libcrypto.d2i_X509_NAME(a, _in, len);
end;

function i2d_X509_NAME(a: PX509_NAME; _out: PPByte): integer;
begin
  result := libcrypto.i2d_X509_NAME(a, _out);
end;

function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509;
begin
  result := libcrypto.X509_STORE_CTX_get_current_cert(ctx);
end;

function X509_digest(data: PX509; typ: PEVP_MD; md: PEVP_MD_DIG; len: PCardinal): integer;
begin
  result := libcrypto.X509_digest(data, typ, md, len);
end;

function X509_get_serialNumber(x: PX509): PASN1_INTEGER;
begin
  result := libcrypto.X509_get_serialNumber(x);
end;

function X509_check_private_key(x509: PX509; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_check_private_key(x509, pkey);
end;

function X509_get_ext_count(x: PX509): integer;
begin
  result := libcrypto.X509_get_ext_count(x);
end;

function X509_get_ext(x: PX509; loc: integer): PX509_EXTENSION;
begin
  result := libcrypto.X509_get_ext(x, loc);
end;

function X509_get_ext_by_NID(x: PX509; nid: integer; lastpos: integer): integer;
begin
  result := libcrypto.X509_get_ext_by_NID(x, nid, lastpos);
end;

function X509_EXTENSION_get_object(ex: PX509_EXTENSION): PASN1_OBJECT;
begin
  result := libcrypto.X509_EXTENSION_get_object(ex);
end;

function X509_EXTENSION_get_data(ne: PX509_EXTENSION): PASN1_OCTET_STRING;
begin
  result := libcrypto.X509_EXTENSION_get_data(ne);
end;

function X509_EXTENSION_get_critical(ex: PX509_EXTENSION): integer;
begin
  result := libcrypto.X509_EXTENSION_get_critical(ex);
end;

function X509_get_version(x: PX509): integer;
begin
  result := libcrypto.X509_get_version(x);
end;

function X509_get0_notBefore(x: PX509): PASN1_TIME;
begin
  result := libcrypto.X509_get0_notBefore(x);
end;

function X509_get0_notAfter(x: PX509): PASN1_TIME;
begin
  result := libcrypto.X509_get0_notAfter(x);
end;

function X509_get_extension_flags(x: PX509): cardinal;
begin
  result := libcrypto.X509_get_extension_flags(x);
end;

function X509_get_key_usage(x: PX509): cardinal;
begin
  result := libcrypto.X509_get_key_usage(x);
end;

function X509_get_extended_key_usage(x: PX509): cardinal;
begin
  result := libcrypto.X509_get_extended_key_usage(x);
end;

function X509V3_EXT_print(_out: PBIO; ext: PX509_EXTENSION;
  flag: cardinal; indent: integer): integer;
begin
  result := libcrypto.X509V3_EXT_print(_out, ext, flag, indent);
end;

function i2d_X509_bio(bp: PBIO; x509: PX509): integer;
begin
  result := libcrypto.i2d_X509_bio(bp, x509);
end;

function d2i_X509_bio(bp: PBIO; x509: PPX509): PX509;
begin
  result := libcrypto.d2i_X509_bio(bp, x509);
end;

function PEM_read_bio_X509_AUX(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509;
begin
  result := libcrypto.PEM_read_bio_X509_AUX(bp, x, cb, u);
end;

function X509_CRL_new(): PX509_CRL;
begin
  result := libcrypto.X509_CRL_new();
end;

procedure X509_CRL_free(a: PX509_CRL);
begin
  libcrypto.X509_CRL_free(a);
end;

function X509_CRL_verify(a: PX509_CRL; r: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_CRL_verify(a, r);
end;

function X509_CRL_sign(x: PX509_CRL; pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  result := libcrypto.X509_CRL_sign(x, pkey, md);
end;

function X509_CRL_dup(crl: PX509_CRL): PX509_CRL;
begin
  result := libcrypto.X509_CRL_dup(crl);
end;

function X509_CRL_up_ref(crl: PX509_CRL): integer;
begin
  result := libcrypto.X509_CRL_up_ref(crl);
end;

function X509_CRL_set_version(x: PX509_CRL; version: integer): integer;
begin
  result := libcrypto.X509_CRL_set_version(x, version);
end;

function X509_CRL_set_issuer_name(x: PX509_CRL; name: PX509_NAME): integer;
begin
  result := libcrypto.X509_CRL_set_issuer_name(x, name);
end;

function X509_CRL_set_lastUpdate(x: PX509_CRL; tm: PASN1_TIME): integer;
begin
  result := libcrypto.X509_CRL_set_lastUpdate(x, tm);
end;

function X509_CRL_set_nextUpdate(x: PX509_CRL; tm: PASN1_TIME): integer;
begin
  result := libcrypto.X509_CRL_set_nextUpdate(x, tm);
end;

function X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME;
begin
  result := libcrypto.X509_CRL_get_issuer(crl);
end;

function X509_CRL_get_version(crl: PX509_CRL): integer;
begin
  result := libcrypto.X509_CRL_get_version(crl);
end;

function X509_CRL_get_lastUpdate(crl: PX509_CRL): PASN1_TIME;
begin
  result := libcrypto.X509_CRL_get_lastUpdate(crl);
end;

function X509_CRL_get_nextUpdate(crl: PX509_CRL): PASN1_TIME;
begin
  result := libcrypto.X509_CRL_get_nextUpdate(crl);
end;

function X509_CRL_print(bp: PBIO; x: PX509_CRL): integer;
begin
  result := libcrypto.X509_CRL_print(bp, x);
end;

function d2i_X509_CRL_bio(bp: PBIO; crl: PPX509_CRL): PX509_CRL;
begin
  result := libcrypto.d2i_X509_CRL_bio(bp, crl);
end;

function i2d_X509_CRL_bio(bp: PBIO; crl: PX509_CRL): integer;
begin
  result := libcrypto.i2d_X509_CRL_bio(bp, crl);
end;

function PEM_write_bio_X509_CRL(bp: PBIO; x: PX509_CRL): integer;
begin
  result := libcrypto.PEM_write_bio_X509_CRL(bp, x);
end;

function PEM_read_bio_X509_CRL(bp: PBIO; x: PPX509_CRL; cb: Ppem_password_cb; u: pointer): PX509_CRL;
begin
  result := libcrypto.PEM_read_bio_X509_CRL(bp, x, cb, u);
end;

function X509_CRL_add1_ext_i2d(x: PX509_CRL; nid: integer; value: pointer;
  crit: integer; flags: cardinal): integer;
begin
  result := libcrypto.X509_CRL_add1_ext_i2d(x, nid, value, crit, flags);
end;

function X509_CRL_get_ext_d2i(x: PX509_CRL; nid: integer; crit: PInteger;
  idx: PInteger): pointer;
begin
  result := libcrypto.X509_CRL_get_ext_d2i(x, nid, crit, idx);
end;

function X509_CRL_add0_revoked(crl: PX509_CRL; rev: PX509_REVOKED): integer;
begin
  result := libcrypto.X509_CRL_add0_revoked(crl, rev);
end;

function X509_CRL_delete_ext(x: PX509_CRL; loc: integer): PX509_EXTENSION;
begin
  result := libcrypto.X509_CRL_delete_ext(x, loc);
end;

function X509_CRL_get_REVOKED(crl: PX509_CRL): Pstack_st_X509_REVOKED;
begin
  result := libcrypto.X509_CRL_get_REVOKED(crl);
end;

function X509_CRL_get0_extensions(crl: PX509_CRL): Pstack_st_X509_EXTENSION;
begin
  result := libcrypto.X509_CRL_get0_extensions(crl);
end;

function X509_CRL_sort(crl: PX509_CRL): integer;
begin
  result := libcrypto.X509_CRL_sort(crl);
end;

function X509_REVOKED_new(): PX509_REVOKED;
begin
  result := libcrypto.X509_REVOKED_new();
end;

procedure X509_REVOKED_free(a: PX509_REVOKED);
begin
  libcrypto.X509_REVOKED_free(a);
end;

function X509_REVOKED_set_serialNumber(x: PX509_REVOKED; serial: PASN1_INTEGER): integer;
begin
  result := libcrypto.X509_REVOKED_set_serialNumber(x, serial);
end;

function X509_REVOKED_set_revocationDate(r: PX509_REVOKED; tm: PASN1_TIME): integer;
begin
  result := libcrypto.X509_REVOKED_set_revocationDate(r, tm);
end;

function X509_REVOKED_get0_serialNumber(x: PX509_REVOKED): PASN1_INTEGER;
begin
  result := libcrypto.X509_REVOKED_get0_serialNumber(x);
end;

function X509_REVOKED_get0_revocationDate(x: PX509_REVOKED): PASN1_TIME;
begin
  result := libcrypto.X509_REVOKED_get0_revocationDate(x);
end;

function X509_REVOKED_get_ext_d2i(x: PX509_REVOKED; nid: integer; crit: PInteger;
  idx: PInteger): pointer;
begin
  result := libcrypto.X509_REVOKED_get_ext_d2i(x, nid, crit, idx);
end;

function X509_REVOKED_add1_ext_i2d(x: PX509_REVOKED; nid: integer; value: pointer;
  crit: integer; flags: cardinal): integer;
begin
  result := libcrypto.X509_REVOKED_add1_ext_i2d(x, nid, value, crit, flags);
end;

function d2i_X509_REVOKED(a: PPX509_REVOKED; _in: PPByte; len: integer): PX509_REVOKED;
begin
  result := libcrypto.d2i_X509_REVOKED(a, _in, len);
end;

function i2d_X509_REVOKED(a: PX509_REVOKED; _out: PPByte): integer;
begin
  result := libcrypto.i2d_X509_REVOKED(a, _out);
end;

function X509_STORE_new(): PX509_STORE;
begin
  result := libcrypto.X509_STORE_new();
end;

function X509_STORE_load_locations(ctx: PX509_STORE; _file: PUtf8Char; dir: PUtf8Char): integer;
begin
  result := libcrypto.X509_STORE_load_locations(ctx, _file, dir);
end;

function X509_STORE_set_default_paths(ctx: PX509_STORE): integer;
begin
  result := libcrypto.X509_STORE_set_default_paths(ctx);
end;

function X509_STORE_add_cert(ctx: PX509_STORE; x: PX509): integer;
begin
  result := libcrypto.X509_STORE_add_cert(ctx, x);
end;

function X509_STORE_add_crl(ctx: PX509_STORE; x: PX509_CRL): integer;
begin
  result := libcrypto.X509_STORE_add_crl(ctx, x);
end;

function X509_STORE_add_lookup(v: PX509_STORE; m: PX509_LOOKUP_METHOD): PX509_LOOKUP;
begin
  result := libcrypto.X509_STORE_add_lookup(v, m);
end;

function X509_STORE_set_flags(ctx: PX509_STORE; flags: cardinal): integer;
begin
  result := libcrypto.X509_STORE_set_flags(ctx, flags);
end;

function X509_STORE_set1_param(ctx: PX509_STORE; pm: PX509_VERIFY_PARAM): integer;
begin
  result := libcrypto.X509_STORE_set1_param(ctx, pm);
end;

function X509_STORE_get0_param(ctx: PX509_STORE): PX509_VERIFY_PARAM;
begin
  result := libcrypto.X509_STORE_get0_param(ctx);
end;

procedure X509_STORE_set_verify_cb(ctx: PX509_STORE; verify_cb: X509_STORE_CTX_verify_cb);
begin
  libcrypto.X509_STORE_set_verify_cb(ctx, verify_cb);
end;

function X509_STORE_lock(ctx: PX509_STORE): integer;
begin
  result := libcrypto.X509_STORE_lock(ctx);
end;

function X509_STORE_unlock(ctx: PX509_STORE): integer;
begin
  result := libcrypto.X509_STORE_unlock(ctx);
end;

function X509_STORE_up_ref(v: PX509_STORE): integer;
begin
  result := libcrypto.X509_STORE_up_ref(v);
end;

function X509_STORE_get0_objects(v: PX509_STORE): Pstack_st_X509_OBJECT;
begin
  result := libcrypto.X509_STORE_get0_objects(v);
end;

function X509_OBJECT_get0_X509(a: PX509_OBJECT): PX509;
begin
  result := libcrypto.X509_OBJECT_get0_X509(a);
end;

function X509_OBJECT_get0_X509_CRL(a: PX509_OBJECT): PX509_CRL;
begin
  result := libcrypto.X509_OBJECT_get0_X509_CRL(a);
end;

function X509_LOOKUP_hash_dir(): PX509_LOOKUP_METHOD;
begin
  result := libcrypto.X509_LOOKUP_hash_dir();
end;

function X509_LOOKUP_file(): PX509_LOOKUP_METHOD;
begin
  result := libcrypto.X509_LOOKUP_file();
end;

function X509_LOOKUP_ctrl(ctx: PX509_LOOKUP; cmd: integer; argc: PUtf8Char;
  argl: integer; ret: PPUtf8Char): integer;
begin
  result := libcrypto.X509_LOOKUP_ctrl(ctx, cmd, argc, argl, ret);
end;

function X509_load_cert_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer;
begin
  result := libcrypto.X509_load_cert_file(ctx, _file, typ);
end;

function X509_load_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer;
begin
  result := libcrypto.X509_load_crl_file(ctx, _file, typ);
end;

function X509_load_cert_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer;
begin
  result := libcrypto.X509_load_cert_crl_file(ctx, _file, typ);
end;

function X509_STORE_CTX_new(): PX509_STORE_CTX;
begin
  result := libcrypto.X509_STORE_CTX_new();
end;

function X509_STORE_CTX_init(ctx: PX509_STORE_CTX; store: PX509_STORE;
  x509: PX509; chain: Pstack_st_X509): integer;
begin
  result := libcrypto.X509_STORE_CTX_init(ctx, store, X509, chain);
end;

procedure X509_STORE_CTX_set_verify_cb(ctx: PX509_STORE_CTX;
  verify: X509_STORE_CTX_verify_cb);
begin
  libcrypto.X509_STORE_CTX_set_verify_cb(ctx, verify);
end;

procedure X509_STORE_CTX_set_cert(c: PX509_STORE_CTX; x: PX509);
begin
  libcrypto.X509_STORE_CTX_set_cert(c, x);
end;

function X509_verify_cert(ctx: PX509_STORE_CTX): integer;
begin
  result := libcrypto.X509_verify_cert(ctx);
end;

function X509_STORE_CTX_get_error(ctx: PX509_STORE_CTX): integer;
begin
  result := libcrypto.X509_STORE_CTX_get_error(ctx);
end;

function X509_verify_cert_error_string(n: integer): PUtf8Char;
begin
  result := libcrypto.X509_verify_cert_error_string(n);
end;

function X509_verify(a: PX509; r: PEVP_PKEY): integer;
begin
  result := libcrypto.X509_verify(a, r);
end;

procedure X509_STORE_CTX_set_time(ctx: PX509_STORE_CTX; flags: cardinal; t: time_t);
begin
  libcrypto.X509_STORE_CTX_set_time(ctx, flags, t);
end;

function X509_STORE_CTX_set_purpose(ctx: PX509_STORE_CTX; purpose: integer): integer;
begin
  result := libcrypto.X509_STORE_CTX_set_purpose(ctx, purpose);
end;

function X509_STORE_CTX_set_trust(ctx: PX509_STORE_CTX; trust: integer): integer;
begin
  result := libcrypto.X509_STORE_CTX_set_trust(ctx, trust);
end;

procedure X509_STORE_CTX_set0_untrusted(ctx: PX509_STORE_CTX; sk: Pstack_st_X509);
begin
  libcrypto.X509_STORE_CTX_set0_untrusted(ctx, sk);
end;

procedure X509_STORE_CTX_set0_param(ctx: PX509_STORE_CTX; param: PX509_VERIFY_PARAM);
begin
  libcrypto.X509_STORE_CTX_set0_param(ctx, param);
end;

function X509_VERIFY_PARAM_new(): PX509_VERIFY_PARAM;
begin
  result := libcrypto.X509_VERIFY_PARAM_new();
end;

procedure X509_VERIFY_PARAM_free(param: PX509_VERIFY_PARAM);
begin
  libcrypto.X509_VERIFY_PARAM_free(param);
end;

function X509_VERIFY_PARAM_set_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer;
begin
  result := libcrypto.X509_VERIFY_PARAM_set_flags(param, flags);
end;

function X509_VERIFY_PARAM_clear_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer;
begin
  result := libcrypto.X509_VERIFY_PARAM_clear_flags(param, flags);
end;

function X509_VERIFY_PARAM_get_flags(param: PX509_VERIFY_PARAM): cardinal;
begin
  result := libcrypto.X509_VERIFY_PARAM_get_flags(param);
end;

function X509_VERIFY_PARAM_set_purpose(param: PX509_VERIFY_PARAM; purpose: integer): integer;
begin
  result := libcrypto.X509_VERIFY_PARAM_set_purpose(param, purpose);
end;

function X509_VERIFY_PARAM_set_trust(param: PX509_VERIFY_PARAM; trust: integer): integer;
begin
  result := libcrypto.X509_VERIFY_PARAM_set_trust(param, trust);
end;

procedure X509_VERIFY_PARAM_set_depth(param: PX509_VERIFY_PARAM; depth: integer);
begin
  libcrypto.X509_VERIFY_PARAM_set_depth(param, depth);
end;

procedure X509_VERIFY_PARAM_set_auth_level(param: PX509_VERIFY_PARAM; auth_level: integer);
begin
  libcrypto.X509_VERIFY_PARAM_set_auth_level(param, auth_level);
end;

function X509_LOOKUP_load_file(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer;
begin
  result := libcrypto.X509_LOOKUP_ctrl(ctx, X509_L_FILE_LOAD, name, typ, nil);
end;

function X509_LOOKUP_add_dir(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer;
begin
  result := libcrypto.X509_LOOKUP_ctrl(ctx, X509_L_ADD_DIR, name, typ, nil);
end;

function PKCS12_new(): PPKCS12;
begin
  result := libcrypto.PKCS12_new();
end;

procedure PKCS12_free(a: PPKCS12);
begin
  libcrypto.PKCS12_free(a);
end;

function PKCS12_create(pass: PUtf8Char; name: PUtf8Char; pkey: PEVP_PKEY; cert: PX509;
  ca: Pstack_st_X509; nid_key, nid_cert, iter, mac_iter, keytype: integer): PPKCS12;
begin
  result := libcrypto.PKCS12_create(
    pass, name, pkey, cert, ca, nid_key, nid_cert, iter, mac_iter, keytype);
end;

function PKCS12_set_mac(p12: PPKCS12; pass: PUtf8Char; passlen: integer;
  salt: PByte; saltlen: integer; iter: integer; md_type: PEVP_MD): integer;
begin
  result := libcrypto.PKCS12_set_mac(p12, pass, passlen, salt, saltlen, iter, md_type);
end;

function PKCS12_add_cert(pbags: PPstack_st_PKCS12_SAFEBAG; cert: PX509): PPKCS12_SAFEBAG;
begin
  result := libcrypto.PKCS12_add_cert(pbags, cert);
end;

function PKCS12_add_key(pbags: PPstack_st_PKCS12_SAFEBAG; key: PEVP_PKEY;
  key_usage: integer; iter: integer; key_nid: integer; pass: PUtf8Char): PPKCS12_SAFEBAG;
begin
  result := libcrypto.PKCS12_add_key(pbags, key, key_usage, iter, key_nid, pass);
end;

function i2d_PKCS12_bio(bp: PBIO; p12: PPKCS12): integer;
begin
  result := libcrypto.i2d_PKCS12_bio(bp, p12);
end;

function d2i_PKCS12_bio(bp: PBIO; p12: PPPKCS12): PPKCS12;
begin
  result := libcrypto.d2i_PKCS12_bio(bp, p12);
end;

function PKCS12_newpass(p12: PPKCS12; oldpass, newpass: PUtf8Char): integer;
begin
  result := libcrypto.PKCS12_newpass(p12, oldpass, newpass);
end;

function PKCS12_parse(p12: PPKCS12; pass: PUtf8Char;
  pkey: PPEVP_PKEY; cert: PPX509; ca: PPstack_st_X509): integer;
begin
  result := libcrypto.PKCS12_parse(p12, pass, pkey, cert, ca);
end;

function ASN1_TIME_new(): PASN1_TIME;
begin
  result := libcrypto.ASN1_TIME_new();
end;

procedure ASN1_TIME_free(a: PASN1_TIME);
begin
  libcrypto.ASN1_TIME_free(a);
end;

function ASN1_TIME_set(s: PASN1_TIME; t: time_t): PASN1_TIME;
begin
  result := libcrypto.ASN1_TIME_set(s, t);
end;

function ASN1_TIME_set_string_X509(s: PASN1_TIME; str: PUtf8Char): integer;
begin
  if Assigned(libcrypto.ASN1_TIME_set_string_X509) then
    result := libcrypto.ASN1_TIME_set_string_X509(s, str)
  else
    result := 0; // unsupported
end;

function ASN1_TIME_to_tm(s: PASN1_TIME; tm: Ptm): integer;
begin
  if Assigned(libcrypto.ASN1_TIME_to_tm) then
    result := libcrypto.ASN1_TIME_to_tm(s, tm)
  else
    result := 0;
end;

function ASN1_TIME_normalize(s: PASN1_TIME): integer;
begin
  if Assigned(libcrypto.ASN1_TIME_normalize) then
    result := libcrypto.ASN1_TIME_normalize(s)
  else
    result := 0;
end;

function OPENSSL_sk_new(cmp: OPENSSL_sk_compfunc): POPENSSL_STACK;
begin
  result := libcrypto.OPENSSL_sk_new(cmp);
end;

procedure OPENSSL_sk_free(p1: POPENSSL_STACK);
begin
  libcrypto.OPENSSL_sk_free(p1);
end;

procedure OPENSSL_sk_pop_free(st: POPENSSL_STACK; func: OPENSSL_sk_freefunc);
begin
  libcrypto.OPENSSL_sk_pop_free(st, func);
end;

function OPENSSL_sk_delete(st: POPENSSL_STACK; loc: integer): pointer;
begin
  result := libcrypto.OPENSSL_sk_delete(st, loc);
end;

function OPENSSL_sk_find(st: POPENSSL_STACK; data: pointer): integer;
begin
  result := libcrypto.OPENSSL_sk_find(st, data);
end;

function OPENSSL_sk_push(st: POPENSSL_STACK; data: pointer): integer;
begin
  result := libcrypto.OPENSSL_sk_push(st, data);
end;

function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer;
begin
  result := libcrypto.OPENSSL_sk_pop(st);
end;

function OPENSSL_sk_num(p1: POPENSSL_STACK): integer;
begin
  result := libcrypto.OPENSSL_sk_num(p1);
end;

function OPENSSL_sk_value(p1: POPENSSL_STACK; p2: integer): pointer;
begin
  result := libcrypto.OPENSSL_sk_value(p1, p2);
end;

function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer;
begin
  result := libcrypto.ASN1_BIT_STRING_get_bit(a, n);
end;

function OBJ_nid2ln(n: integer): PUtf8Char;
begin
  result := libcrypto.OBJ_nid2ln(n);
end;

function OBJ_nid2sn(n: integer): PUtf8Char;
begin
  result := libcrypto.OBJ_nid2sn(n);
end;

function OBJ_txt2nid(s: PUtf8Char): integer;
begin
  result := libcrypto.OBJ_txt2nid(s);
end;

function OBJ_obj2nid(o: PASN1_OBJECT): integer;
begin
  result := libcrypto.OBJ_obj2nid(o);
end;

function ASN1_STRING_data(x: PASN1_STRING): PByte;
begin
  result := libcrypto.ASN1_STRING_data(x);
end;

function ASN1_STRING_length(x: PASN1_STRING): integer;
begin
  result := libcrypto.ASN1_STRING_length(x);
end;

function ASN1_STRING_type(x: PASN1_STRING): integer;
begin
  result := libcrypto.ASN1_STRING_type(x);
end;

function ASN1_STRING_print_ex(_out: PBIO; str: PASN1_STRING; flags: cardinal): integer;
begin
  result := libcrypto.ASN1_STRING_print_ex(_out, str, flags);
end;

function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb;
  u: pointer): PX509;
begin
  result := libcrypto.PEM_read_bio_X509(bp, x, cb, u);
end;

function PEM_write_bio_X509(bp: PBIO; x: PX509): integer;
begin
  result := libcrypto.PEM_write_bio_X509(bp, x);
end;

function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY;
begin
  result := libcrypto.PEM_read_bio_PrivateKey(bp, x, cb, u);
end;

function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY;
begin
  result := libcrypto.PEM_read_bio_PUBKEY(bp, x, cb, u);
end;

function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA;
begin
  result := libcrypto.PEM_read_bio_RSAPublicKey(bp, x, cb, u);
end;

function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA;
begin
  result := libcrypto.PEM_read_bio_RSAPrivateKey(bp, x, cb, u);
end;

function RSA_public_encrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer;
begin
  result := libcrypto.RSA_public_encrypt(flen, from, _to, rsa, padding);
end;

function RSA_private_encrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
begin
  result := libcrypto.RSA_private_encrypt(flen, from, _to, rsa, padding);
end;

function RSA_public_decrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
begin
  result := libcrypto.RSA_public_decrypt(flen, from, _to, rsa, padding);
end;

function RSA_private_decrypt(flen: integer; from: PByte; _to: PByte;
  rsa: PRSA; padding: integer): integer; cdecl;
begin
  result := libcrypto.RSA_private_decrypt(flen, from, _to, rsa, padding);
end;

function RSA_pkey_ctx_ctrl(ctx: PEVP_PKEY_CTX; optype: integer;
  cmd: integer; p1: integer; p2: pointer): integer; cdecl;
begin
  if Assigned(libcrypto.RSA_pkey_ctx_ctrl) then
    result := libcrypto.RSA_pkey_ctx_ctrl(ctx, optype, cmd, p1, p2)
  else
    result := 0;
end;

function i2d_PrivateKey_bio(bp: PBIO; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.i2d_PrivateKey_bio(bp, pkey);
end;

function d2i_PrivateKey_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY;
begin
  result := libcrypto.d2i_PrivateKey_bio(bp, a);
end;

function i2d_PUBKEY_bio(bp: PBIO; pkey: PEVP_PKEY): integer;
begin
  result := libcrypto.i2d_PUBKEY_bio(bp, pkey);
end;

function d2i_PUBKEY_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY;
begin
  result := libcrypto.d2i_PUBKEY_bio(bp, a);
end;

function RAND_bytes(buf: PByte; num: integer): integer;
begin
  result := libcrypto.RAND_bytes(buf, num);
end;

procedure RAND_seed(buf: pointer; num: integer);
begin
  libcrypto.RAND_seed(buf, num);
end;

function EVP_get_cipherbyname(name: PUtf8Char): PEVP_CIPHER;
begin
  result := libcrypto.EVP_get_cipherbyname(name);
end;

function EVP_get_digestbyname(name: PUtf8Char): PEVP_MD;
begin
  result := libcrypto.EVP_get_digestbyname(name);
end;

function EVP_CIPHER_CTX_new(): PEVP_CIPHER_CTX;
begin
  result := libcrypto.EVP_CIPHER_CTX_new();
end;

function EVP_CIPHER_CTX_reset(c: PEVP_CIPHER_CTX): integer;
begin
  result := libcrypto.EVP_CIPHER_CTX_reset(c);
end;

procedure EVP_CIPHER_CTX_free(c: PEVP_CIPHER_CTX);
begin
  libcrypto.EVP_CIPHER_CTX_free(c);
end;

function EVP_CIPHER_CTX_copy(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer;
begin
  result := libcrypto.EVP_CIPHER_CTX_copy(_out, _in);
end;

function EVP_CIPHER_CTX_set_key_length(x: PEVP_CIPHER_CTX; keylen: integer): integer;
begin
  result := libcrypto.EVP_CIPHER_CTX_set_key_length(x, keylen);
end;

function EVP_CIPHER_CTX_ctrl(ctx: PEVP_CIPHER_CTX; typ: integer; arg: integer;
  ptr: pointer): integer;
begin
  result := libcrypto.EVP_CIPHER_CTX_ctrl(ctx, typ, arg, ptr);
end;

function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  impl: PENGINE; key: PByte; iv: PByte; enc: integer): integer;
begin
  result := libcrypto.EVP_CipherInit_ex(ctx, cipher, impl, key, iv, enc);
end;

function EVP_CipherInit_ex2(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  key: PByte; iv: PByte; enc: integer; params: pointer): integer;
begin
  // note: the latest API (i.e. EVP_CipherInit_ex on 1.1, EVP_CipherInit_ex2
  // on 3.0) should be called to be able to reuse the context
  if Assigned(libcrypto.EVP_CipherInit_ex2) then // OpenSSL 3.0 API
    result := libcrypto.EVP_CipherInit_ex2(ctx, cipher, key, iv, enc, params)
  else                                // fallback to OpenSSL 1.1 call
    result := libcrypto.EVP_CipherInit_ex(ctx, cipher, nil, key, iv, enc);
end;

function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer;
begin
  result := libcrypto.EVP_CipherUpdate(ctx, _out, outl, _in, inl);
end;

function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer;
begin
  result := libcrypto.EVP_CipherFinal_ex(ctx, outm, outl);
end;

function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: integer): integer;
begin
  result := libcrypto.EVP_CIPHER_CTX_set_padding(c, pad);
end;

function EVP_CIPHER_CTX_iv(ctx: PEVP_CIPHER_CTX): PByte;
begin
  result := libcrypto.EVP_CIPHER_CTX_iv(ctx);
end;

function EVP_MD_CTX_new: PEVP_MD_CTX;
begin
  result := libcrypto.EVP_MD_CTX_new;
end;

procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX);
begin
  libcrypto.EVP_MD_CTX_free(ctx);
end;

function EVP_MD_CTX_md(ctx: PEVP_MD_CTX): PEVP_MD;
begin
  result := libcrypto.EVP_MD_CTX_md(ctx);
end;

function EVP_MD_flags(md: PEVP_MD): cardinal;
begin
  result := libcrypto.EVP_MD_flags(md);
end;

function EVP_MD_size(md: PEVP_MD): integer;
begin
  result := libcrypto.EVP_MD_size(md);
end;

function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer;
begin
  result := libcrypto.EVP_DigestInit_ex(ctx, typ, impl);
end;

function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; s: PCardinal): integer;
begin
  result := libcrypto.EVP_DigestFinal_ex(ctx, md, s);
end;

function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; len: PtrUInt): integer;
begin
  if Assigned(libcrypto.EVP_DigestFinalXOF) then
    result := libcrypto.EVP_DigestFinalXOF(ctx, md, len)
  else
    result := 0; // unsupported
end;

function HMAC_CTX_new: PHMAC_CTX;
begin
  result := libcrypto.HMAC_CTX_new;
end;

procedure HMAC_CTX_free(ctx: PHMAC_CTX);
begin
  libcrypto.HMAC_CTX_free(ctx);
end;

function HMAC_Init_ex(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD;
  impl: PENGINE): integer;
begin
  result := libcrypto.HMAC_Init_ex(ctx, key, len, md, impl);
end;

function HMAC_Update(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer;
begin
  result := libcrypto.HMAC_Update(ctx, data, len);
end;

function HMAC_Final(ctx: PHMAC_CTX; md: PEVP_MD_DIG; len: PCardinal): integer;
begin
  result := libcrypto.HMAC_Final(ctx, md, len);
end;

function EVP_sha1: PEVP_MD;
begin
  result := libcrypto.EVP_sha1;
end;

function EVP_sha256: PEVP_MD;
begin
  result := libcrypto.EVP_sha256;
end;

function EC_GROUP_new_by_curve_name(nid: integer): PEC_GROUP;
begin
  result := libcrypto.EC_GROUP_new_by_curve_name(nid);
end;

function EC_KEY_new(): PEC_KEY;
begin
  result := libcrypto.EC_KEY_new;
end;

function EC_KEY_set_group(key: PEC_KEY; group: PEC_GROUP): integer;
begin
  result := libcrypto.EC_KEY_set_group(key, group);
end;

function BN_bin2bn(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM;
begin
  result := libcrypto.BN_bin2bn(s, len, ret);
end;

function BN_bn2dec(a: PBIGNUM): PUtf8Char;
begin
  result := libcrypto.BN_bn2dec(a);
end;

function BN_dec2bn(a: PPBIGNUM; str: PUtf8Char): integer;
begin
  result := libcrypto.BN_dec2bn(a, str);
end;

function BN_to_ASN1_INTEGER(bn: PBIGNUM; ai: PASN1_INTEGER): PASN1_INTEGER;
begin
  result := libcrypto.BN_to_ASN1_INTEGER(bn, ai);
end;

function ASN1_bn_print(bp: PBIO; number: PUtf8Char; num: PBIGNUM;
  buf: PByte; off: integer): integer;
begin
  result := libcrypto.ASN1_bn_print(bp, number, num, buf, off);
end;

function ASN1_INTEGER_to_BN(ai: PASN1_INTEGER; bn: PBIGNUM): PBIGNUM;
begin
  result := libcrypto.ASN1_INTEGER_to_BN(ai, bn);
end;

function ASN1_INTEGER_new(): PASN1_INTEGER;
begin
  result := libcrypto.ASN1_INTEGER_new();
end;

procedure ASN1_INTEGER_free(a: PASN1_INTEGER);
begin
  libcrypto.ASN1_INTEGER_free(a);
end;

function ASN1_ENUMERATED_set(a: PASN1_ENUMERATED; v: integer): integer;
begin
  result := libcrypto.ASN1_ENUMERATED_set(a, v);
end;

function ASN1_ENUMERATED_get(a: PASN1_ENUMERATED): integer;
begin
  result := libcrypto.ASN1_ENUMERATED_get(a);
end;

function ASN1_ENUMERATED_new(): PASN1_ENUMERATED;
begin
  result := libcrypto.ASN1_ENUMERATED_new();
end;

procedure ASN1_ENUMERATED_free(a: PASN1_ENUMERATED);
begin
  libcrypto.ASN1_ENUMERATED_free(a);
end;

function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT;
  p4: PBN_CTX): PEC_POINT;
begin
  result := libcrypto.EC_POINT_bn2point(p1, p2, p3, p4);
end;

function EC_POINT_hex2point(p1: PEC_GROUP; p2: PUtf8Char; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT;
begin
  result := libcrypto.EC_POINT_hex2point(p1, p2, p3, p4);
end;

function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer;
begin
  result := libcrypto.EC_KEY_set_public_key(key, pub);
end;

function EC_KEY_set_public_key_affine_coordinates(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): integer;
begin
  result := libcrypto.EC_KEY_set_public_key_affine_coordinates(key, x, y);
end;

function ECDSA_verify(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: integer; eckey: PEC_KEY): integer;
begin
  result := libcrypto.ECDSA_verify(typ, dgst, dgstlen, sig, siglen, eckey);
end;

procedure EC_POINT_free(point: PEC_POINT);
begin
  libcrypto.EC_POINT_free(point);
end;

procedure BN_free(a: PBIGNUM);
begin
  libcrypto.BN_free(a);
end;

function BN_num_bits(a: PBIGNUM): integer;
begin
  result := libcrypto.BN_num_bits(a);
end;

procedure EC_KEY_free(key: PEC_KEY);
begin
  libcrypto.EC_KEY_free(key);
end;

procedure EC_GROUP_free(group: PEC_GROUP);
begin
  libcrypto.EC_GROUP_free(group);
end;

function EC_KEY_generate_key(key: PEC_KEY): integer;
begin
  result := libcrypto.EC_KEY_generate_key(key);
end;

function EC_KEY_get0_private_key(key: PEC_KEY): PBIGNUM;
begin
  result := libcrypto.EC_KEY_get0_private_key(key);
end;

function EC_KEY_set_private_key(key: PEC_KEY; prv: PBIGNUM): integer;
begin
  result := libcrypto.EC_KEY_set_private_key(key, prv);
end;

function EC_KEY_get0_public_key(key: PEC_KEY): PEC_POINT;
begin
  result := libcrypto.EC_KEY_get0_public_key(key);
end;

function EC_KEY_key2buf(key: PEC_KEY; form: point_conversion_form_t;
  pbuf: PPByte; ctx: PBN_CTX): PtrUInt;
begin
  result := libcrypto.EC_KEY_key2buf(key, form, pbuf, ctx);
end;

function EVP_PKEY_get0_RSA(pkey: PEVP_PKEY): Prsa_st;
begin
  result := libcrypto.EVP_PKEY_get0_RSA(pkey);
end;

procedure RSA_get0_key(r: PRSA; n: PPBIGNUM; e: PPBIGNUM; d: PPBIGNUM);
begin
  libcrypto.RSA_get0_key(r, n, e, d);
end;

function X509_REQ_add_extensions(req: PX509_REQ; exts: Pstack_st_X509_EXTENSION): integer;
begin
  result := libcrypto.X509_REQ_add_extensions(req, exts);
end;

function X509_REQ_get_extensions(req: PX509_REQ): Pstack_st_X509_EXTENSION;
begin
  result := libcrypto.X509_REQ_get_extensions(req);
end;

function EC_POINT_point2buf(group: PEC_GROUP; point: PEC_POINT;
  form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt;
begin
  result := libcrypto.EC_POINT_point2buf(group, point, form, pbuf, ctx);
end;

function BN_bn2bin(a: PBIGNUM; _to: pointer): integer;
begin
  result := libcrypto.BN_bn2bin(a, _to);
end;

function ECDSA_size(eckey: PEC_KEY): integer;
begin
  result := libcrypto.ECDSA_size(eckey);
end;

function ECDSA_sign(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: PCardinal; eckey: PEC_KEY): integer;
begin
  result := libcrypto.ECDSA_sign(typ, dgst, dgstlen, sig, siglen, eckey);
end;

function EC_POINT_new(group: PEC_GROUP): PEC_POINT;
begin
  result := libcrypto.EC_POINT_new(group);
end;

function EC_POINT_oct2point(group: PEC_GROUP; p: PEC_POINT;
  buf: PByte; len: PtrUInt; ctx: PBN_CTX): integer;
begin
  result := libcrypto.EC_POINT_oct2point(group, p, buf, len, ctx);
end;

function ECDH_compute_key(_out: pointer; outlen: PtrUInt; pub_key: PEC_POINT;
  ecdh: PEC_KEY; KDF: ECDH_compute_key_KDF): integer;
begin
  result := libcrypto.ECDH_compute_key(_out, outlen, pub_key, ecdh, kdf);
end;

function EVP_PKEY_CTX_new_id(id: integer; e: PENGINE): PEVP_PKEY_CTX;
begin
  result := libcrypto.EVP_PKEY_CTX_new_id(id, e);
end;

function EVP_PKEY_paramgen_init(ctx: PEVP_PKEY_CTX): integer;
begin
  result := libcrypto.EVP_PKEY_paramgen_init(ctx);
end;

function EVP_PKEY_paramgen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_paramgen(ctx, ppkey);
end;

function EVP_PKEY_keygen_init(ctx: PEVP_PKEY_CTX): integer;
begin
  result := libcrypto.EVP_PKEY_keygen_init(ctx);
end;

function EVP_PKEY_keygen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_keygen(ctx, ppkey);
end;

function EVP_PKEY_CTX_ctrl(ctx: PEVP_PKEY_CTX; keytype: integer; optype: integer;
  cmd: integer; p1: integer; p2: pointer): integer;
begin
  result := libcrypto.EVP_PKEY_CTX_ctrl(ctx, keytype, optype, cmd, p1, p2);
end;

function EVP_PKEY_CTX_set1_pbe_pass(ctx: PEVP_PKEY_CTX;
  pass: PAnsiChar; passlen: integer): integer;
begin
  if Assigned(libcrypto.EVP_PKEY_CTX_set1_pbe_pass) then
    result := libcrypto.EVP_PKEY_CTX_set1_pbe_pass(ctx, pass, passlen)
  else
    result := 0; // defined as macro in deprecated openssl 1.1
end;

function EVP_PKEY_CTX_set1_scrypt_salt(ctx: PEVP_PKEY_CTX;
  salt: PByte; saltlen: integer): integer;
begin
  if Assigned(libcrypto.EVP_PKEY_CTX_set1_scrypt_salt) then
    result := libcrypto.EVP_PKEY_CTX_set1_scrypt_salt(ctx, salt, saltlen)
  else
    result := 0; // defined as macro in deprecated openssl 1.1
end;


function EVP_PKEY_CTX_set_scrypt_N(ctx: PEVP_PKEY_CTX; n: QWord): integer; cdecl;
begin
  if Assigned(libcrypto.EVP_PKEY_CTX_set_scrypt_N) then
    result := libcrypto.EVP_PKEY_CTX_set_scrypt_N(ctx, n)
  else
    result := 0; // defined as macro in deprecated openssl 1.1
end;

function EVP_PKEY_CTX_set_scrypt_r(ctx: PEVP_PKEY_CTX; r: QWord): integer; cdecl;
begin
  if Assigned(libcrypto.EVP_PKEY_CTX_set_scrypt_r) then
    result := libcrypto.EVP_PKEY_CTX_set_scrypt_r(ctx, r)
  else
    result := 0; // defined as macro in deprecated openssl 1.1
end;

function EVP_PKEY_CTX_set_scrypt_p(ctx: PEVP_PKEY_CTX; p: QWord): integer; cdecl;
begin
  if Assigned(libcrypto.EVP_PKEY_CTX_set_scrypt_p) then
    result := libcrypto.EVP_PKEY_CTX_set_scrypt_p(ctx, p)
  else
    result := 0; // defined as macro in deprecated openssl 1.1
end;

function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX;
begin
  result := libcrypto.EVP_PKEY_CTX_new(pkey, e);
end;

procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX);
begin
  libcrypto.EVP_PKEY_CTX_free(ctx);
end;

function EVP_PKEY_derive_init(ctx: PEVP_PKEY_CTX): integer;
begin
  result := libcrypto.EVP_PKEY_derive_init(ctx);
end;

function EVP_PKEY_derive_set_peer(ctx: PEVP_PKEY_CTX; peer: PEVP_PKEY): integer;
begin
  result := libcrypto.EVP_PKEY_derive_set_peer(ctx, peer);
end;

function EVP_PKEY_derive(ctx: PEVP_PKEY_CTX; key: PByte; keylen: PPtrUInt): integer;
begin
  result := libcrypto.EVP_PKEY_derive(ctx, key, keylen);
end;

function EVP_PKEY_get0_EC_KEY(pkey: PEVP_PKEY): PEC_KEY;
begin
  result := libcrypto.EVP_PKEY_get0_EC_KEY(pkey);
end;

function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer;
begin
  result := libcrypto.PEM_write_bio_PrivateKey(bp, x, enc, kstr, klen, cb, u);
end;

function PEM_write_bio_PKCS8PrivateKey(p1: PBIO; p2: PEVP_PKEY; p3: PEVP_CIPHER;
  p4: PUtf8Char; p5: integer; p6: Ppem_password_cb; p7: pointer): integer;
begin
  result := libcrypto.PEM_write_bio_PKCS8PrivateKey(p1, p2, p3, p4, p5, p6, p7);
end;

function i2d_PKCS8PrivateKey_bio(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PUtf8Char; klen: integer; cb: Ppem_password_cb; u: pointer): integer;
begin
  result := libcrypto.i2d_PKCS8PrivateKey_bio(bp, x, enc, kstr, klen, cb, u);
end;

function d2i_PKCS8PrivateKey_bio(bp: PBIO; x: PPEVP_PKEY;
  cb: Ppem_password_cb; u: pointer): PEVP_PKEY;
begin
  result := libcrypto.d2i_PKCS8PrivateKey_bio(bp, x, cb, u);
end;

function EVP_aes_256_cbc(): PEVP_CIPHER;
begin
  result := libcrypto.EVP_aes_256_cbc();
end;

function EVP_bf_cbc(): PEVP_CIPHER;
begin
  result := libcrypto.EVP_bf_cbc();
end;

function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer;
begin
  result := libcrypto.PEM_write_bio_PUBKEY(bp, x);
end;

function OpenSSL_version_num(): cardinal;
begin
  result := libcrypto.OpenSSL_version_num();
end;

function OpenSSL_version(typ: integer): PUtf8Char;
begin
  result := libcrypto.OpenSSL_version(typ);
end;

function OSSL_PROVIDER_load(libctx: POSSL_LIB_CTX; name: PAnsiChar): POSSL_PROVIDER;
begin
  if Assigned(libcrypto.OSSL_PROVIDER_load) then
    result := libcrypto.OSSL_PROVIDER_load(libctx, name)
  else
    result := nil; // unsupported
end;

function OSSL_PROVIDER_set_default_search_path(libctx: POSSL_LIB_CTX; path: PAnsiChar): integer;
begin
  if Assigned(libcrypto.OSSL_PROVIDER_set_default_search_path) then
    result := libcrypto.OSSL_PROVIDER_set_default_search_path(libctx, path)
  else
    result := 0; // unsupported in openssl 1.1 - 0 indicates an OpenSSL error
end;

function OSSL_PROVIDER_available(libctx: POSSL_LIB_CTX; name: PAnsiChar): integer;
begin
  if Assigned(libcrypto.OSSL_PROVIDER_available) then
    result := libcrypto.OSSL_PROVIDER_available(libctx, name)
  else
    result := 0; // unsupported in openssl 1.1 - 0 indicates an OpenSSL error
end;

function X509_print(bp: PBIO; x: PX509): integer;
begin
  result := libcrypto.X509_print(bp, x);
end;

function EVP_PKEY_decrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
begin
  result := libcrypto.EVP_PKEY_decrypt_init(ctx);
end;

function EVP_PKEY_decrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
begin
  result := libcrypto.EVP_PKEY_decrypt(ctx, output, outlen, input, inputLen);
end;

function EVP_PKEY_encrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
begin
  result := libcrypto.EVP_PKEY_encrypt_init(ctx);
end;

function EVP_PKEY_encrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
begin
  result := libcrypto.EVP_PKEY_encrypt(ctx, output, outlen, input, inputLen);
end;


{$ifdef OPENSSLUSERTLMM}

// redirect OpenSSL to use our current pascal heap :)

function rtl_malloc(siz: PtrUInt; fname: PUtf8Char; fline: integer): pointer; cdecl;
begin
  if siz <= 0 then
    result := nil
  else
    GetMem(result, siz);
end;

function rtl_realloc(str: pointer; siz: PtrUInt;
  fname: PUtf8Char; fline: integer): pointer; cdecl;
begin
  if str = nil then
    if siz <= 0 then
      result := nil
    else
      GetMem(result, siz)
  else
    result := ReallocMem(str, siz);
end;

procedure rtl_free(str: pointer; fname: PUtf8Char; fline: integer); cdecl;
begin
  FreeMem(str);
end;

{$endif OPENSSLUSERTLMM}

procedure _OpenSslInitialize;
begin
  OpenSslInitialize; // try loading OpenSSL with default parameters
end;

function OpenSslIsAvailable: boolean;
begin
  result := LibraryAvailable(openssl_initialized, _OpenSslInitialize);
end;

function OpenSslIsLoaded: boolean;
begin
  result := openssl_initialized = lsAvailable;
end;

function OpenSslInitialize(const libcryptoname, libsslname: TFileName;
  const libprefix: RawUtf8): boolean;
var
  error: string;
  libenv, libsys1, libsys3, libexe1, libexe3, libpath, libexact, libname: TFileName;
begin
  // not thread-safe: use manual GlobalLock/GlobalUnLock or OpenSslIsAvailable
  result := openssl_initialized = lsAvailable;
  if not result then // set it once, but can retry with specific alternate libnames
  try
    // paranoid thread-safe double check
    if openssl_initialized = lsAvailable then
      exit;
    // read and validate OPENSSL_LIBPATH environment variable
    libenv := OpenSslDefaultPath; // priority to the global variable
    if libenv = '' then
      libenv := GetEnvironmentVariable('OPENSSL_LIBPATH');
    if libenv <> '' then
      if DirectoryExists(libenv) then
        libenv := IncludeTrailingPathDelimiter(libenv)
      else
        libenv := ''; // search anywhere within system path
    // initialize library loaders
    libcrypto := TLibCrypto.Create;
    libssl    := TLibSsl.Create;
    try
      // try to guess the potential libcrypto library names
      if libcryptoname = '' then
      begin
        {$ifndef NOOPENSSL1}
        libexe1 := Executable.ProgramFilePath + LIB_CRYPTO1;
        if not FileExists(libexe1) then
          libexe1 := '';
        libsys1 := libenv + LIB_CRYPTO1;
        {$endif NOOPENSSL1}
        {$ifndef NOOPENSSL3}
        libexe3 := Executable.ProgramFilePath + LIB_CRYPTO3;
        if not FileExists(libexe3) then
          libexe3 := '';
        libsys3 := libenv + LIB_CRYPTO3;
        {$endif NOOPENSSL3}
      end;
      // attempt to load libcrypto
      if not libcrypto.TryLoadResolve([
        // first try the exact supplied crypto library name
        libcryptoname,
        // try with the global variable
        OpenSslDefaultCrypto,
        // try from executable folder
        libexe3,
        libexe1,
        // try the library from OPENSSL_LIBPATH or somewhere in the system
        libsys3,
        libsys1
        {$ifdef OSPOSIX}
        {$ifndef OSDARWIN}
        // generic library name on most UNIX (but MacOS)
        , 'libcrypto.so'
        {$endif OSDARWIN}
        {$endif OSPOSIX}
        ], libprefix, @LIBCRYPTO_ENTRIES, @@libcrypto.CRYPTO_malloc, nil, @error) then
        exit; // silent failure on missing library or entry
      // validate the loaded libcrypto
      if not Assigned(libcrypto.X509_print) then // last known entry (paranoid)
        raise EOpenSsl.Create('OpenSslInitialize: incorrect libcrypto API');
      OpenSslVersion := libcrypto.OpenSSL_version_num;
      OpenSslVersionHexa := IntToHex(OpenSslVersion, 8);
      OpenSslVersionText := RawUtf8(libcrypto.OpenSSL_version(OPENSSL_VERSION_));
      if OpenSslVersion and $ffffff00 < LIB_MIN then // paranoid check
      begin
        error := Format('Incorrect %s version in %s - expects ' + LIB_TXT,
          [OpenSslVersionText, libcrypto.LibraryPath]);
        exit;
      end;
      // guess libssl names of the same version or name pattern from libcrypto
      libpath := ExtractFilePath(libcrypto.LibraryPath);
      if OpenSslVersion < OPENSSL3_VERNUM then
        libexact := libpath + LIB_SSL1
      else
        libexact := libpath + LIB_SSL3;
      libname := libpath + StringReplace(ExtractFileName(libcrypto.LibraryPath),
        'libcrypto', 'libssl', [rfReplaceAll {$ifdef OSWINDOWS}, rfIgnoreCase{$endif}]);
      // attempt to load libssl
      if not libssl.TryLoadResolve([
        // first try the exact supplied ssl library name
        libsslname,
        // try with the global variable
        OpenSslDefaultSsl,
        // try same version and/or name in the libcrypto folder
        libexact,
        libname
        {$ifdef OSPOSIX}
        {$ifndef OSDARWIN}
        // generic library name on most UNIX
        , 'libssl.so'
        {$endif OSDARWIN}
        {$endif OSPOSIX}
        ], libprefix, @LIBSSL_ENTRIES, @@libssl.SSL_CTX_new, nil, @error) then
        exit; // silent failure on missing library or entry
      if not Assigned(libssl.SSL_add1_host) then // last known entry (paranoid)
        raise EOpenSsl.Create('OpenSslInitialize: incorrect libssl API');
      // nothing is to be initialized with OpenSSL 1.1.*
      {$ifdef OPENSSLUSERTLMM}
      if libcrypto.CRYPTO_set_mem_functions(@rtl_malloc, @rtl_realloc, @rtl_free) = 0 then
        raise EOpenSsl.Create('CRYPTO_set_mem_functions() failure');
      {$endif OPENSSLUSERTLMM}
      OpenSslExIndexSsl := SSL_get_ex_new_index(0, nil, nil, nil, nil);
      result := true; // if we reached here, everything is OK
    except
      on E: Exception do // EOpenSsl above are logic error and should be logged
        error := E.Message;
    end;
  finally
    if result then
    begin
      @NewNetTls := @NewOpenSslNetTls; // favor OpenSSL for TLS from now on
      openssl_initialized := lsAvailable; // flag should be set the last
    end
    else
    begin
      FreeAndNil(libcrypto);
      FreeAndNil(libssl);
      openssl_initialized := lsNotAvailable
    end;
    openssl_initialize_errormsg := error;
  end;
end;

{$else}

{ ******************** Statically linked OpenSSL Library Functions }

{ --------- libssl entries }

{$ifdef FPC}
  {$ifdef CPUX64}
    {$ifdef OSDARWIN}
      {$linklib 'libcrypto-osx64.a'}
      {$linklib 'libssl-osx64.a'}
    {$endif OSDARWIN}
  {$endif CPUX64}
{$endif FPC}

function SSL_CTX_new(meth: PSSL_METHOD): PSSL_CTX; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_new';

procedure SSL_CTX_free(p1: PSSL_CTX); cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_free';

function SSL_CTX_set_timeout(ctx: PSSL_CTX; t: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_timeout';

function SSL_CTX_get_timeout(ctx: PSSL_CTX): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_get_timeout';

procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: integer; callback: SSL_verify_cb); cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_verify';

function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_PrivateKey';

function SSL_CTX_use_RSAPrivateKey(ctx: PSSL_CTX; rsa: PRSA): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_RSAPrivateKey';

function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_RSAPrivateKey_file';

function SSL_CTX_use_certificate(ctx: PSSL_CTX; x: PX509): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_certificate';

function SSL_CTX_check_private_key(ctx: PSSL_CTX): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_check_private_key';

function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_certificate_file';

function SSL_CTX_get_cert_store(p1: PSSL_CTX): PX509_STORE; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_get_cert_store';

function SSL_CTX_load_verify_locations(ctx: PSSL_CTX;
  CAfile: PUtf8Char; CApath: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_load_verify_locations';

function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; _file: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_certificate_chain_file';

function SSL_CTX_set_alpn_protos(ctx: PSSL_CTX;
  protos: PByte; protos_len: cardinal): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_alpn_protos';

function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_ctrl';

function SSL_CTX_set_options(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_options';

function SSL_CTX_clear_options(ctx: PSSL_CTX; op: cardinal): cardinal; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_clear_options';

function SSL_CTX_callback_ctrl(p1: PSSL_CTX; p2: integer; p3: SSL_CTX_callback_ctrl_): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_callback_ctrl';

function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
  external LIB_SSL name _PU + 'SSL_new';

function SSL_set_SSL_CTX(ssl: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl;
  external LIB_SSL name _PU + 'SSL_set_SSL_CTX';

function SSL_shutdown(s: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_shutdown';

function SSL_get_error(s: PSSL; ret_code: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_get_error';

function SSL_ctrl(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
  external LIB_SSL name _PU + 'SSL_ctrl';

procedure SSL_set_bio(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
  external LIB_SSL name _PU + 'SSL_set_bio';

function SSL_set_ex_data(ssl: PSSL; idx: integer; data: pointer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_set_ex_data';

function SSL_get_ex_data(ssl: PSSL; idx: integer): pointer; cdecl;
  external LIB_SSL name _PU + 'SSL_get_ex_data';

function SSL_get_peer_certificate(s: PSSL): PX509; cdecl;
  external LIB_SSL name _PU + 'SSL_get_peer_certificate';

function SSL_get_peer_cert_chain(s: PSSL): Pstack_st_X509; cdecl;
  external LIB_SSL name _PU + 'SSL_get_peer_cert_chain';

procedure SSL_free(ssl: PSSL); cdecl;
  external LIB_SSL name _PU + 'SSL_free';

function SSL_connect(ssl: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_connect';

function SSL_accept(ssl: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_accept';

procedure SSL_set_connect_state(s: PSSL); cdecl;
  external LIB_SSL name _PU + 'SSL_set_connect_state';

procedure SSL_set_accept_state(s: PSSL); cdecl;
  external LIB_SSL name _PU + 'SSL_set_accept_state';

function SSL_read(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_read';

function SSL_write(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_write';

function SSL_get_state(ssl: PSSL): OSSL_HANDSHAKE_STATE; cdecl;
  external LIB_SSL name _PU + 'SSL_get_state';

function SSL_pending(s: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_pending';

function SSL_set_cipher_list(s: PSSL; str: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_set_cipher_list';

procedure SSL_get0_alpn_selected(ssl: PSSL; data: PPByte; len: PCardinal); cdecl;
  external LIB_SSL name _PU + 'SSL_get0_alpn_selected';

function SSL_get_servername(s: PSSL; typ: integer): PUtf8Char; cdecl;
  external LIB_SSL name _PU + 'SSL_get_servername';

function SSL_clear(s: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_clear';

function TLS_client_method(): PSSL_METHOD; cdecl;
  external LIB_SSL name _PU + 'TLS_client_method';

function TLS_server_method(): PSSL_METHOD; cdecl;
  external LIB_SSL name _PU + 'TLS_server_method';

function SSL_CTX_set_default_verify_paths(ctx: PSSL_CTX): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_default_verify_paths';

procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Ppem_password_cb); cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_default_passwd_cb';

procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: pointer); cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_default_passwd_cb_userdata';

function SSL_CTX_use_PrivateKey_file(ctx: PSSL_CTX; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_use_PrivateKey_file';

function SSL_CTX_set_cipher_list(p1: PSSL_CTX; str: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_CTX_set_cipher_list';

function SSL_set_fd(s: PSSL; fd: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_set_fd';

function SSL_get_current_cipher(s: PSSL): PSSL_CIPHER; cdecl;
  external LIB_SSL name _PU + 'SSL_get_current_cipher';

function SSL_CIPHER_description(p1: PSSL_CIPHER; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
  external LIB_SSL name _PU + 'SSL_CIPHER_description';

function SSL_get_verify_result(ssl: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_get_verify_result';

procedure SSL_set_hostflags(s: PSSL; flags: cardinal); cdecl;
  external LIB_SSL name _PU + 'SSL_set_hostflags';

function SSL_set1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_set1_host';

function SSL_add1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_add1_host';


{ --------- libcrypto entries }

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_malloc';

function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn;
  f: dyn_MEM_free_fn): integer; cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_set_mem_functions';

procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_free';

function CRYPTO_get_ex_new_index(class_index: integer; argl: integer;
  argp: pointer; new_func: PCRYPTO_EX_new; dup_func: PCRYPTO_EX_dup;
  free_func: PCRYPTO_EX_free): integer; cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_get_ex_new_index';

procedure ERR_remove_state(pid: cardinal); cdecl;
  external LIB_CRYPTO name _PU + 'ERR_remove_state';

procedure ERR_error_string_n(e: cardinal; buf: PUtf8Char; len: PtrUInt); cdecl;
  external LIB_CRYPTO name _PU + 'ERR_error_string_n';

function ERR_get_error(): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_get_error';

procedure ERR_remove_thread_state(p1: pointer); cdecl;
  external LIB_CRYPTO name _PU + 'ERR_remove_thread_state';

function ERR_load_BIO_strings(): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_load_BIO_strings';

function EVP_MD_CTX_create(): PEVP_MD_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_new';

procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_free';

function EVP_PKEY_size(pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_size';

function EVP_PKEY_type(typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_type';

function EVP_PKEY_id(pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_id';

function EVP_PKEY_base_id(pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_base_id';

function EVP_PKEY_bits(pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_bits';

procedure EVP_PKEY_free(pkey: PEVP_PKEY); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_free';

function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD;
  e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignInit';

function EVP_DigestUpdate(ctx: PEVP_MD_CTX; d: pointer; cnt: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestUpdate';

function EVP_DigestSignFinal(ctx: PEVP_MD_CTX;
  sigret: PByte; var siglen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignFinal';

function EVP_DigestVerifyInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX;
  typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestVerifyInit';

function EVP_DigestVerifyFinal(ctx: PEVP_MD_CTX; sig: PByte; siglen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestVerifyFinal';

function EVP_DigestSign(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt;
  tbs: PByte; tbslen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSign';

function EVP_DigestVerify(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt;
  tbs: PByte; tbslen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestVerify';

function EVP_SealInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PPByte; ekl: PInteger;
  iv: PByte; pubk: PPEVP_PKEY; npubk: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_SealInit';

function EVP_SealFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_SealFinal';

function EVP_OpenInit(ctx: PEVP_CIPHER_CTX; typ: PEVP_CIPHER; ek: PByte; ekl: integer;
  iv: PByte; priv: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_OpenInit';

function EVP_OpenFinal(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_OpenFinal';

function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_EncryptUpdate';

function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DecryptUpdate';

function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer;
  d: PByte; n: PtrUInt; md: PEVP_MD_DIG; var md_len: cardinal): PByte; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC';

function BIO_new(typ: PBIO_METHOD): PBIO; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_new';

function BIO_free(a: PBIO): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_free';

function BIO_test_flags(b: PBIO; flags: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_test_flags';

function BIO_ctrl(bp: PBIO; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_ctrl';

function BIO_new_mem_buf(buf: pointer; len: integer): PBIO; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_new_mem_buf';

function BIO_s_mem(): PBIO_METHOD; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_s_mem';

function BIO_read(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_read';

function BIO_write(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_write';

function BIO_new_socket(sock: integer; close_flag: integer): PBIO; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_new_socket';

function X509_get_issuer_name(a: PX509): PX509_NAME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_issuer_name';

function X509_get_subject_name(a: PX509): PX509_NAME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_subject_name';

function X509_get_pubkey(x: PX509): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_pubkey';

function X509_get_signature_nid(x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_signature_nid';

function X509_get_signature_info(x: PX509;
    mdnid, pknid, secbits, flags: PInteger): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_signature_info';

function X509_up_ref(x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_up_ref';

procedure X509_STORE_free(v: PX509_STORE); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_free';

procedure X509_STORE_CTX_free(ctx: PX509_STORE_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_free';

procedure X509_free(a: PX509); cdecl;
  external LIB_CRYPTO name _PU + 'X509_free';

function X509_new(): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'X509_new';

function X509_set_version(x: PX509; version: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_set_version';

function X509_set_serialNumber(x: PX509; serial: PASN1_INTEGER): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_set_serialNumber';

function X509_set_issuer_name(x: PX509; name: PX509_NAME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_set_issuer_name';

function X509_set_subject_name(x: PX509; name: PX509_NAME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_set_subject_name';

function X509_set_pubkey(x: PX509; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_set_pubkey';

function X509_sign(x: PX509; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_sign';

function X509_REQ_new(): PX509_REQ; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_new';

function X509_REQ_set_version(x: PX509_REQ; version: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_set_version';

procedure X509_REQ_free(a: PX509_REQ); cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_free';

function X509_REQ_sign(x: PX509_REQ; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_sign';

function X509_REQ_verify(a: PX509_REQ; r: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_verify';

function d2i_X509_REQ_bio(bp: PBIO; req: PPX509_REQ): PX509_REQ; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_X509_REQ_bio';

function i2d_X509_REQ_bio(bp: PBIO; req: PX509_REQ): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_X509_REQ_bio';

function PEM_write_bio_X509_REQ(bp: PBIO; x: PX509_REQ): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_X509_REQ';

function X509_REQ_get_subject_name(req: PX509_REQ): PX509_NAME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_get_subject_name';

function X509_REQ_get_pubkey(req: PX509_REQ): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_get_pubkey';

function X509_REQ_set_pubkey(x: PX509_REQ; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_set_pubkey';

function X509_getm_notBefore(x: PX509): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_getm_notBefore';

function X509_getm_notAfter(x: PX509): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_getm_notAfter';

function X509V3_EXT_conf_nid(conf: Plhash_st_CONF_VALUE; ctx: PX509V3_CTX;
  ext_nid: integer; value: PUtf8Char): PX509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509V3_EXT_conf_nid';

function X509_add_ext(x: PX509; ex: PX509_EXTENSION; loc: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_add_ext';

function X509_delete_ext(x: PX509; loc: integer): PX509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509_delete_ext';

procedure X509V3_set_ctx(ctx: PX509V3_CTX; issuer: PX509; subject: PX509;
  req: PX509_REQ; crl: PX509_CRL; flags: integer); cdecl;
  external LIB_CRYPTO name _PU + 'X509V3_set_ctx';

function X509_gmtime_adj(s: PASN1_TIME; adj: integer): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_gmtime_adj';

procedure X509_EXTENSION_free(a: PX509_EXTENSION); cdecl;
  external LIB_CRYPTO name _PU + 'X509_EXTENSION_free';

procedure BASIC_CONSTRAINTS_free(a: PBASIC_CONSTRAINTS); cdecl;
  external LIB_CRYPTO name _PU + 'BASIC_CONSTRAINTS_free';

function d2i_BASIC_CONSTRAINTS(a: PPBASIC_CONSTRAINTS; _in: PPByte;
  len: integer): PBASIC_CONSTRAINTS; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_BASIC_CONSTRAINTS';

function X509_NAME_add_entry_by_txt(name: PX509_NAME; field: PUtf8Char;
  typ: integer; bytes: PAnsiChar; len: integer; loc: integer; _set: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_add_entry_by_txt';

function X509_NAME_print_ex(_out: PBIO; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_print_ex';

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_print_ex_fp';

function X509_NAME_entry_count(name: PX509_NAME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_entry_count';

function X509_NAME_get_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_get_entry';

function X509_NAME_get_text_by_NID(name: PX509_NAME; nid: integer;
  buf: PUtf8Char; len: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_get_text_by_NID';

function X509_NAME_get_index_by_NID(name: PX509_NAME; nid: integer;
  lastpos: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_get_index_by_NID';

function X509_NAME_delete_entry(name: PX509_NAME; loc: integer): PX509_NAME_ENTRY; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_delete_entry';

function X509_NAME_ENTRY_get_data(ne: PX509_NAME_ENTRY): PASN1_STRING; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_ENTRY_get_data';

function X509_NAME_ENTRY_get_object(ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_ENTRY_get_object';

procedure X509_NAME_ENTRY_free(a: PX509_NAME_ENTRY); cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_ENTRY_free';

function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_oneline';

function X509_NAME_hash(x: PX509_NAME): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_hash';

function X509_NAME_cmp(a: PX509_NAME; b: PX509_NAME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_cmp';

function d2i_X509_NAME(a: PPX509_NAME; _in: PPByte; len: integer): PX509_NAME; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_X509_NAME';

function i2d_X509_NAME(a: PX509_NAME; _out: PPByte): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_X509_NAME';

function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_get_current_cert';

function X509_digest(data: PX509; typ: PEVP_MD; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_digest';

function X509_get_serialNumber(x: PX509): PASN1_INTEGER; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_serialNumber';

function X509_check_private_key(x509: PX509; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_check_private_key';

function X509_get_ext_count(x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_ext_count';

function X509_get_ext(x: PX509; loc: integer): PX509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_ext';

function X509_get_ext_by_NID(x: PX509; nid: integer; lastpos: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_ext_by_NID';

function X509_EXTENSION_get_object(ex: PX509_EXTENSION): PASN1_OBJECT; cdecl;
  external LIB_CRYPTO name _PU + 'X509_EXTENSION_get_object';

function X509_EXTENSION_get_data(ne: PX509_EXTENSION): PASN1_OCTET_STRING; cdecl;
  external LIB_CRYPTO name _PU + 'X509_EXTENSION_get_data';

function X509_EXTENSION_get_critical(ex: PX509_EXTENSION): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_EXTENSION_get_critical';

function X509_get_version(x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_version';

function X509_get0_notBefore(x: PX509): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get0_notBefore';

function X509_get0_notAfter(x: PX509): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get0_notAfter';

function X509_get_extension_flags(x: PX509): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_extension_flags';

function X509_get_key_usage(x: PX509): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_key_usage';

function X509_get_extended_key_usage(x: PX509): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'X509_get_extended_key_usage';

function X509V3_EXT_print(_out: PBIO; ext: PX509_EXTENSION; flag: cardinal;
  indent: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509V3_EXT_print';

function i2d_X509_bio(bp: PBIO; x509: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_X509_bio';

function d2i_X509_bio(bp: PBIO; x509: PPX509): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_X509_bio';

function PEM_read_bio_X509_AUX(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_X509_AUX';

function X509_CRL_new(): PX509_CRL; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_new';

procedure X509_CRL_free(a: PX509_CRL); cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_free';

function X509_CRL_verify(a: PX509_CRL; r: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_verify';

function X509_CRL_sign(x: PX509_CRL; pkey: PEVP_PKEY; md: PEVP_MD): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_sign';

function X509_CRL_dup(crl: PX509_CRL): PX509_CRL; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_dup';

function X509_CRL_up_ref(crl: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_up_ref';

function X509_CRL_set_version(x: PX509_CRL; version: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_set_version';

function X509_CRL_set_issuer_name(x: PX509_CRL; name: PX509_NAME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_set_issuer_name';

function X509_CRL_set_lastUpdate(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_set1_lastUpdate';

function X509_CRL_set_nextUpdate(x: PX509_CRL; tm: PASN1_TIME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_set1_nextUpdate';

function X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_issuer';

function X509_CRL_get_version(crl: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_version';

function X509_CRL_get_lastUpdate(crl: PX509_CRL): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_lastUpdate';

function X509_CRL_get_nextUpdate(crl: PX509_CRL): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_nextUpdate';

function X509_CRL_print(bp: PBIO; x: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_print';

function d2i_X509_CRL_bio(bp: PBIO; crl: PPX509_CRL): PX509_CRL; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_X509_CRL_bio';

function i2d_X509_CRL_bio(bp: PBIO; crl: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_X509_CRL_bio';

function PEM_write_bio_X509_CRL(bp: PBIO; x: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_X509_CRL';

function PEM_read_bio_X509_CRL(bp: PBIO; x: PPX509_CRL;
  cb: Ppem_password_cb; u: pointer): PX509_CRL; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_X509_CRL';

function X509_CRL_add1_ext_i2d(x: PX509_CRL; nid: integer; value: pointer;
  crit: integer; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_add1_ext_i2d';

function X509_CRL_get_ext_d2i(x: PX509_CRL; nid: integer; crit: PInteger;
  idx: PInteger): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_ext_d2i';

function X509_CRL_add0_revoked(crl: PX509_CRL; rev: PX509_REVOKED): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_add0_revoked';

function X509_CRL_delete_ext(x: PX509_CRL; loc: integer): PX509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_delete_ext';

function X509_CRL_get_REVOKED(crl: PX509_CRL): Pstack_st_X509_REVOKED; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get_REVOKED';

function X509_CRL_get0_extensions(crl: PX509_CRL): Pstack_st_X509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_get0_extensions';

function X509_CRL_sort(crl: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_CRL_sort';

function X509_REVOKED_new(): PX509_REVOKED; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_new';

procedure X509_REVOKED_free(a: PX509_REVOKED); cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_free';

function X509_REVOKED_set_serialNumber(x: PX509_REVOKED; serial: PASN1_INTEGER): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_set_serialNumber';

function X509_REVOKED_set_revocationDate(r: PX509_REVOKED; tm: PASN1_TIME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_set_revocationDate';

function X509_REVOKED_get0_serialNumber(x: PX509_REVOKED): PASN1_INTEGER; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_get0_serialNumber';

function X509_REVOKED_get0_revocationDate(x: PX509_REVOKED): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_get0_revocationDate';

function X509_REVOKED_get_ext_d2i(x: PX509_REVOKED; nid: integer;
  crit: PInteger; idx: PInteger): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_get_ext_d2i';

function X509_REVOKED_add1_ext_i2d(x: PX509_REVOKED; nid: integer;
  value: pointer; crit: integer; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REVOKED_add1_ext_i2d';

function d2i_X509_REVOKED(a: PPX509_REVOKED; _in: PPByte; len: integer): PX509_REVOKED; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_X509_REVOKED';

function i2d_X509_REVOKED(a: PX509_REVOKED; _out: PPByte): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_X509_REVOKED';

function X509_STORE_new(): PX509_STORE; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_new';

function X509_STORE_load_locations(ctx: PX509_STORE; _file: PUtf8Char; dir: PUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_load_locations';

function X509_STORE_set_default_paths(ctx: PX509_STORE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_set_default_paths';

function X509_STORE_add_cert(ctx: PX509_STORE; x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_add_cert';

function X509_STORE_add_crl(ctx: PX509_STORE; x: PX509_CRL): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_add_crl';

function X509_STORE_add_lookup(v: PX509_STORE; m: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_add_lookup';

function X509_STORE_set_flags(ctx: PX509_STORE; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_set_flags';

function X509_STORE_set1_param(ctx: PX509_STORE; pm: PX509_VERIFY_PARAM): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_set1_param';

function X509_STORE_get0_param(ctx: PX509_STORE): PX509_VERIFY_PARAM; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_get0_param';

procedure X509_STORE_set_verify_cb(ctx: PX509_STORE; verify_cb: X509_STORE_CTX_verify_cb); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_set_verify_cb';

function X509_STORE_lock(ctx: PX509_STORE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_lock';

function X509_STORE_unlock(ctx: PX509_STORE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_unlock';

function X509_STORE_up_ref(v: PX509_STORE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_up_ref';

function X509_STORE_get0_objects(v: PX509_STORE): Pstack_st_X509_OBJECT; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_get0_objects';

function X509_OBJECT_get0_X509(a: PX509_OBJECT): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'X509_OBJECT_get0_X509';

function X509_OBJECT_get0_X509_CRL(a: PX509_OBJECT): PX509_CRL; cdecl;
  external LIB_CRYPTO name _PU + 'X509_OBJECT_get0_X509_CRL';

function X509_LOOKUP_hash_dir(): PX509_LOOKUP_METHOD; cdecl;
  external LIB_CRYPTO name _PU + 'X509_LOOKUP_hash_dir';

function X509_LOOKUP_file(): PX509_LOOKUP_METHOD; cdecl;
  external LIB_CRYPTO name _PU + 'X509_LOOKUP_file';

function X509_LOOKUP_ctrl(ctx: PX509_LOOKUP; cmd: integer; argc: PUtf8Char;
  argl: integer; ret: PPUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_LOOKUP_ctrl';

function X509_load_cert_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_load_cert_file';

function X509_load_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_load_crl_file';

function X509_load_cert_crl_file(ctx: PX509_LOOKUP; _file: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_load_cert_crl_file';

function X509_STORE_CTX_new(): PX509_STORE_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_new';

function X509_STORE_CTX_init(ctx: PX509_STORE_CTX; store: PX509_STORE;
  x509: PX509; chain: Pstack_st_X509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_init';

procedure X509_STORE_CTX_set_verify_cb(ctx: PX509_STORE_CTX;
  verify: X509_STORE_CTX_verify_cb); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set_verify_cb';

procedure X509_STORE_CTX_set_cert(c: PX509_STORE_CTX; x: PX509); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set_cert';

function X509_verify_cert(ctx: PX509_STORE_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_verify_cert';

function X509_STORE_CTX_get_error(ctx: PX509_STORE_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_get_error';

function X509_verify_cert_error_string(n: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'X509_verify_cert_error_string';

function X509_verify(a: PX509; r: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_verify';

procedure X509_STORE_CTX_set_time(ctx: PX509_STORE_CTX; flags: cardinal; t: time_t); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set_time';

function X509_STORE_CTX_set_purpose(ctx: PX509_STORE_CTX; purpose: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set_purpose';

function X509_STORE_CTX_set_trust(ctx: PX509_STORE_CTX; trust: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set_trust';

procedure X509_STORE_CTX_set0_untrusted(ctx: PX509_STORE_CTX; sk: Pstack_st_X509); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set0_untrusted';

procedure X509_STORE_CTX_set0_param(ctx: PX509_STORE_CTX; param: PX509_VERIFY_PARAM); cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_set0_param';

function X509_VERIFY_PARAM_new(): PX509_VERIFY_PARAM; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_new';

procedure X509_VERIFY_PARAM_free(param: PX509_VERIFY_PARAM); cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_free';

function X509_VERIFY_PARAM_set_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_set_flags';

function X509_VERIFY_PARAM_clear_flags(param: PX509_VERIFY_PARAM; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_clear_flags';

function X509_VERIFY_PARAM_get_flags(param: PX509_VERIFY_PARAM): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_get_flags';

function X509_VERIFY_PARAM_set_purpose(param: PX509_VERIFY_PARAM; purpose: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_set_purpose';

function X509_VERIFY_PARAM_set_trust(param: PX509_VERIFY_PARAM; trust: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_set_trust';

procedure X509_VERIFY_PARAM_set_depth(param: PX509_VERIFY_PARAM; depth: integer); cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_set_depth';

procedure X509_VERIFY_PARAM_set_auth_level(param: PX509_VERIFY_PARAM; auth_level: integer); cdecl;
  external LIB_CRYPTO name _PU + 'X509_VERIFY_PARAM_set_auth_level';

function X509_LOOKUP_load_file(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_LOOKUP_load_file';

function X509_LOOKUP_add_dir(ctx: PX509_LOOKUP; name: PUtf8Char; typ: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_LOOKUP_add_dir';

function PKCS12_new(): PPKCS12; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_new';

procedure PKCS12_free(a: PPKCS12); cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_free';

function PKCS12_create(pass: PUtf8Char; name: PUtf8Char; pkey: PEVP_PKEY;
  cert: PX509; ca: Pstack_st_X509; nid_key: integer; nid_cert: integer;
  iter: integer; mac_iter: integer; keytype: integer): PPKCS12; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_create';

function PKCS12_set_mac(p12: PPKCS12; pass: PUtf8Char; passlen: integer;
  salt: PByte; saltlen: integer; iter: integer; md_type: PEVP_MD): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_set_mac';

function PKCS12_add_cert(pbags: PPstack_st_PKCS12_SAFEBAG; cert: PX509): PPKCS12_SAFEBAG; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_add_cert';

function PKCS12_add_key(pbags: PPstack_st_PKCS12_SAFEBAG; key: PEVP_PKEY;
  key_usage: integer; iter: integer; key_nid: integer; pass: PUtf8Char): PPKCS12_SAFEBAG; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_add_key';

function i2d_PKCS12_bio(bp: PBIO; p12: PPKCS12): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_PKCS12_bio';

function d2i_PKCS12_bio(bp: PBIO; p12: PPPKCS12): PPKCS12; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_PKCS12_bio';

function PKCS12_newpass(p12: PPKCS12; oldpass: PUtf8Char; newpass: PUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_newpass';

function PKCS12_parse(p12: PPKCS12; pass: PUtf8Char; pkey: PPEVP_PKEY;
  cert: PPX509; ca: PPstack_st_X509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PKCS12_parse';

function ASN1_TIME_new(): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_new';

procedure ASN1_TIME_free(a: PASN1_TIME); cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_free';

function ASN1_TIME_set(s: PASN1_TIME; t: time_t): PASN1_TIME; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_set';

function ASN1_TIME_set_string_X509(s: PASN1_TIME; str: PUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_set_string_X509';

function ASN1_TIME_to_tm(s: PASN1_TIME; tm: Ptm): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_to_tm';

function ASN1_TIME_normalize(s: PASN1_TIME): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_TIME_normalize';

function OPENSSL_sk_new(cmp: OPENSSL_sk_compfunc): POPENSSL_STACK; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_new';

procedure OPENSSL_sk_pop_free(st: POPENSSL_STACK; func: OPENSSL_sk_freefunc); cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_pop_free';

procedure OPENSSL_sk_free(p1: POPENSSL_STACK); cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_free';

function OPENSSL_sk_delete(st: POPENSSL_STACK; loc: integer): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_delete';

function OPENSSL_sk_find(st: POPENSSL_STACK; data: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_find';

function OPENSSL_sk_push(st: POPENSSL_STACK; data: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_push';

function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_pop';

function OPENSSL_sk_num(p1: POPENSSL_STACK): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_num';

function OPENSSL_sk_value(p1: POPENSSL_STACK; p2: integer): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_value';

function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_BIT_STRING_get_bit';

function OBJ_nid2ln(n: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_nid2ln';

function OBJ_nid2sn(n: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_nid2sn';

function OBJ_obj2nid(o: PASN1_OBJECT): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_obj2nid';

function OBJ_txt2nid(s: PUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_txt2nid';

function ASN1_STRING_data(x: PASN1_STRING): PByte; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_STRING_data';

function ASN1_STRING_length(x: PASN1_STRING): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_STRING_length';

function ASN1_STRING_type(x: PASN1_STRING): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_STRING_type';

function ASN1_STRING_print_ex(_out: PBIO; str: PASN1_STRING; flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_STRING_print_ex';

function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_X509';

function PEM_write_bio_X509(bp: PBIO; x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_X509';

function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_PrivateKey';

function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_PUBKEY';

function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_RSAPublicKey';

function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_RSAPrivateKey';

function RSA_public_encrypt(flen: integer; from: PByte; _to: PByte; rsa: PRSA;
  padding: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RSA_public_encrypt';

function RSA_private_encrypt(flen: integer; from: PByte; _to: PByte; rsa: PRSA;
  padding: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RSA_private_encrypt';

function RSA_public_decrypt(flen: integer; from: PByte; _to: PByte; rsa: PRSA;
  padding: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RSA_public_decrypt';

function RSA_private_decrypt(flen: integer; from: PByte; _to: PByte; rsa: PRSA;
  padding: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RSA_private_decrypt';

function RSA_pkey_ctx_ctrl(ctx: PEVP_PKEY_CTX; optype: integer; cmd: integer;
  p1: integer; p2: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RSA_pkey_ctx_ctrl';

function i2d_PrivateKey_bio(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_PrivateKey_bio';

function d2i_PrivateKey_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_PrivateKey_bio';

function i2d_PUBKEY_bio(bp: PBIO; pkey: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_PUBKEY_bio';

function d2i_PUBKEY_bio(bp: PBIO; a: PPEVP_PKEY): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_PUBKEY_bio';

function RAND_bytes(buf: PByte; num: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RAND_bytes';

procedure RAND_seed(buf: pointer; num: integer); cdecl;
  external LIB_CRYPTO name _PU + 'RAND_seed';

function EVP_get_cipherbyname(name: PUtf8Char): PEVP_CIPHER; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_get_cipherbyname';

function EVP_get_digestbyname(name: PUtf8Char): PEVP_MD; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_get_digestbyname';

function EVP_CIPHER_CTX_new(): PEVP_CIPHER_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_new';

function EVP_CIPHER_CTX_reset(c: PEVP_CIPHER_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_reset';

procedure EVP_CIPHER_CTX_free(c: PEVP_CIPHER_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_free';

function EVP_CIPHER_CTX_copy(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_copy';

function EVP_CIPHER_CTX_set_key_length(x: PEVP_CIPHER_CTX; keylen: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_set_key_length';

function EVP_CIPHER_CTX_ctrl(ctx: PEVP_CIPHER_CTX; typ: integer; arg: integer;
  ptr: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_ctrl';

function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; impl: PENGINE;
  key: PByte; iv: PByte; enc: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherInit_ex';

function EVP_CipherInit_ex2(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  key: PByte; iv: PByte; enc: integer; params: pointer): integer;
begin // redirect from OpenSSL 3 API into 1.1 call
  result := EVP_CipherInit_ex(ctx, cipher, nil, key, iv, enc);
end;

function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherUpdate';

function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherFinal_ex';

function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_set_padding';

function EVP_CIPHER_CTX_iv(ctx: PEVP_CIPHER_CTX): PByte; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_iv';

function EVP_MD_CTX_new(): PEVP_MD_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_new';

procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_free';

function EVP_MD_CTX_md(ctx: PEVP_MD_CTX): PEVP_MD; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_md';

function EVP_MD_flags(md: PEVP_MD): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_flags';

function EVP_MD_size(md: PEVP_MD): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_size';

function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestInit_ex';

function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; s: PCardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestFinal_ex';

function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PEVP_MD_DIG; len: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestFinalXOF';

function HMAC_CTX_new(): PHMAC_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_CTX_new';

procedure HMAC_CTX_free(ctx: PHMAC_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_CTX_free';

function HMAC_Init_ex(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD;
  impl: PENGINE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Init_ex';

function HMAC_Update(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Update';

function HMAC_Final(ctx: PHMAC_CTX; md: PEVP_MD_DIG; len: PCardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Final';

function EVP_sha1: PEVP_MD; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_sha1';

function EVP_sha256: PEVP_MD; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_sha256';

function EC_GROUP_new_by_curve_name(nid: integer): PEC_GROUP; cdecl;
  external LIB_CRYPTO name _PU + 'EC_GROUP_new_by_curve_name';

function EC_KEY_new(): PEC_KEY; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_new';

function EC_KEY_set_group(key: PEC_KEY; group: PEC_GROUP): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_set_group';

function BN_bin2bn(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM; cdecl;
  external LIB_CRYPTO name _PU + 'BN_bin2bn';

function BN_bn2dec(a: PBIGNUM): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'BN_bn2dec';

function BN_dec2bn(a: PPBIGNUM; str: PUtf8Char): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BN_dec2bn';

function BN_to_ASN1_INTEGER(bn: PBIGNUM; ai: PASN1_INTEGER): PASN1_INTEGER; cdecl;
  external LIB_CRYPTO name _PU + 'BN_to_ASN1_INTEGER';

function ASN1_bn_print(bp: PBIO; number: PUtf8Char; num: PBIGNUM; buf: PByte;
  off: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_bn_print';

function ASN1_INTEGER_to_BN(ai: PASN1_INTEGER; bn: PBIGNUM): PBIGNUM; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_INTEGER_to_BN';

function ASN1_INTEGER_new(): PASN1_INTEGER; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_INTEGER_new';

procedure ASN1_INTEGER_free(a: PASN1_INTEGER); cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_INTEGER_free';

function ASN1_ENUMERATED_set(a: PASN1_ENUMERATED; v: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_ENUMERATED_set';

function ASN1_ENUMERATED_get(a: PASN1_ENUMERATED): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_ENUMERATED_get';

function ASN1_ENUMERATED_new(): PASN1_ENUMERATED; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_ENUMERATED_new';

procedure ASN1_ENUMERATED_free(a: PASN1_ENUMERATED); cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_ENUMERATED_free';

function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT;
  p4: PBN_CTX): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_bn2point';

function EC_POINT_hex2point(p1: PEC_GROUP; p2: PUtf8Char; p3: PEC_POINT;
  p4: PBN_CTX): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_hex2point';

function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_set_public_key';

function EC_KEY_set_public_key_affine_coordinates(key: PEC_KEY; x, y: PBIGNUM): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_set_public_key_affine_coordinates';

function ECDSA_verify(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: integer; eckey: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDSA_verify';

procedure EC_POINT_free(point: PEC_POINT); cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_free';

procedure BN_free(a: PBIGNUM); cdecl;
  external LIB_CRYPTO name _PU + 'BN_free';

function BN_num_bits(a: PBIGNUM): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BN_num_bits';

procedure EC_KEY_free(key: PEC_KEY); cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_free';

procedure EC_GROUP_free(group: PEC_GROUP); cdecl;
  external LIB_CRYPTO name _PU + 'EC_GROUP_free';

function EC_KEY_generate_key(key: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_generate_key';

function EC_KEY_get0_private_key(key: PEC_KEY): PBIGNUM; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_get0_private_key';

function EC_KEY_set_private_key(key: PEC_KEY; prv: PBIGNUM): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_set_private_key';

function EC_KEY_get0_public_key(key: PEC_KEY): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_get0_public_key';

function EC_KEY_key2buf(key: PEC_KEY; form: point_conversion_form_t;
  pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_key2buf';

function EVP_PKEY_get0_RSA(pkey: PEVP_PKEY): Prsa_st; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_get0_RSA';

procedure RSA_get0_key(r: PRSA; n: PPBIGNUM; e: PPBIGNUM; d: PPBIGNUM); cdecl;
  external LIB_CRYPTO name _PU + 'RSA_get0_key';

function X509_REQ_add_extensions(req: PX509_REQ; exts: Pstack_st_X509_EXTENSION): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_add_extensions';

function X509_REQ_get_extensions(req: PX509_REQ): Pstack_st_X509_EXTENSION; cdecl;
  external LIB_CRYPTO name _PU + 'X509_REQ_get_extensions';

function EC_POINT_point2buf(group: PEC_GROUP; point: PEC_POINT;
  form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_point2buf';

function BN_bn2bin(a: PBIGNUM; _to: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BN_bn2bin';

function ECDSA_size(eckey: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDSA_size';

function ECDSA_sign(typ: integer; dgst: PByte; dgstlen: integer;
  sig: PByte; siglen: PCardinal; eckey: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDSA_sign';

function EC_POINT_new(group: PEC_GROUP): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_new';

function EC_POINT_oct2point(group: PEC_GROUP; p: PEC_POINT;
  buf: PByte; len: PtrUInt; ctx: PBN_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_oct2point';

function ECDH_compute_key(_out: pointer; outlen: PtrUInt;
  pub_key: PEC_POINT; ecdh: PEC_KEY; KDF: ECDH_compute_key_KDF): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDH_compute_key';

function EVP_PKEY_CTX_new_id(id: integer; e: PENGINE): PEVP_PKEY_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_new_id';

function EVP_PKEY_paramgen_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_paramgen_init';

function EVP_PKEY_paramgen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_paramgen';

function EVP_PKEY_keygen_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_keygen_init';

function EVP_PKEY_keygen(ctx: PEVP_PKEY_CTX; ppkey: PPEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_keygen';

function EVP_PKEY_CTX_ctrl(ctx: PEVP_PKEY_CTX; keytype: integer; optype: integer;
  cmd: integer; p1: integer; p2: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_ctrl';

function EVP_PKEY_CTX_set1_pbe_pass(ctx: PEVP_PKEY_CTX;
  pass: PAnsiChar; passlen: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_set1_pbe_pass';

function EVP_PKEY_CTX_set1_scrypt_salt(ctx: PEVP_PKEY_CTX;
  salt: PByte; saltlen: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_set1_scrypt_salt';

function EVP_PKEY_CTX_set_scrypt_N(ctx: PEVP_PKEY_CTX; n: QWord): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_set_scrypt_N';

function EVP_PKEY_CTX_set_scrypt_r(ctx: PEVP_PKEY_CTX; r: QWord): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_set_scrypt_r';

function EVP_PKEY_CTX_set_scrypt_p(ctx: PEVP_PKEY_CTX; p: QWord): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_set_scrypt_p';

function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_new';

procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_free';

function EVP_PKEY_derive_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_derive_init';

function EVP_PKEY_derive_set_peer(ctx: PEVP_PKEY_CTX; peer: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_derive_set_peer';

function EVP_PKEY_derive(ctx: PEVP_PKEY_CTX; key: PByte; keylen: PPtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_derive';

function EVP_PKEY_get0_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_get0_EC_KEY';

function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_PrivateKey';

function PEM_write_bio_PKCS8PrivateKey(p1: PBIO; p2: PEVP_PKEY; p3: PEVP_CIPHER;
  p4: PUtf8Char; p5: integer; p6: Ppem_password_cb; p7: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_PKCS8PrivateKey';

function i2d_PKCS8PrivateKey_bio(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PUtf8Char; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'i2d_PKCS8PrivateKey_bio';

function d2i_PKCS8PrivateKey_bio(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'd2i_PKCS8PrivateKey_bio';

function EVP_aes_256_cbc(): PEVP_CIPHER; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_aes_256_cbc';

function EVP_bf_cbc(): PEVP_CIPHER; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_bf_cbc';

function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_PUBKEY';

function OpenSSL_version_num(): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'OpenSSL_version_num';

function OpenSSL_version(typ: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'OpenSSL_version';

function OSSL_PROVIDER_load(libctx: POSSL_LIB_CTX; name: PAnsiChar): POSSL_PROVIDER; cdecl;
  external LIB_CRYPTO name _PU + 'OSSL_PROVIDER_load';

function OSSL_PROVIDER_set_default_search_path(libctx: POSSL_LIB_CTX; path: PAnsiChar): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OSSL_PROVIDER_set_default_search_path';

function OSSL_PROVIDER_available(libctx: POSSL_LIB_CTX; name: PAnsiChar): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OSSL_PROVIDER_available';

function X509_print(bp: PBIO; x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_print';

function EVP_PKEY_decrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_decrypt_init';

function EVP_PKEY_decrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_decrypt';

function EVP_PKEY_encrypt_init(ctx: PEVP_PKEY_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_encrypt_init';

function EVP_PKEY_encrypt(ctx: PEVP_PKEY_CTX; output: PByte; var outlen: PtrUInt;
  input: PByte; inputLen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_encrypt';


function OpenSslInitialize(const libcryptoname, libsslname: TFileName;
  const libprefix: RawUtf8): boolean;
begin
  OpenSslVersion := OpenSSL_version_num;
  OpenSslVersionHexa := IntToHex(OpenSslVersion, 8);
  OpenSslVersionText := RawUtf8(OpenSSL_version(OPENSSL_VERSION_));
  OpenSslExIndexSsl := SSL_get_ex_new_index(0, nil, nil, nil, nil);
  result := true;
end;

function OpenSslIsAvailable: boolean;
begin
  result := true;
end;

function OpenSslIsLoaded: boolean;
begin
  result := true;
end;

{$endif OPENSSLSTATIC}


{ ******************** OpenSSL Library Types and Structures }

{ OPENSSL_STACK }

function OPENSSL_STACK.Count: integer;
begin
  if @self = nil then
    result := 0 // OPENSSL_sk_num(nil) would return -1
  else
    result := OPENSSL_sk_num(@self);
end;

function OPENSSL_STACK.Add(one: pointer): integer;
begin
  result := OPENSSL_sk_push(@self, one);
end;

function OPENSSL_STACK.Find(one: pointer): integer;
begin
  result := OPENSSL_sk_find(@self, one);
end;

function OPENSSL_STACK.Extract(index: integer): pointer;
begin
  result := OPENSSL_sk_delete(@self, index);
end;

function OPENSSL_STACK.GetItem(index: integer): pointer;
begin
  result := OPENSSL_sk_value(@self, index);
end;

function OPENSSL_STACK.ToDynArray: TPointerDynArray;
var
  i: PtrInt;
begin
  SetLength(result, Count);
  for i := 0 to length(result) - 1 do
    result[i] := OPENSSL_sk_value(@self, i);
end;

procedure OPENSSL_STACK.Free;
begin
  if @self <> nil then
    OPENSSL_sk_free(@self);
end;

procedure OPENSSL_STACK.FreeX509;
begin
  if @self <> nil then
    OPENSSL_sk_pop_free(@self, @X509_free);
end;

procedure OPENSSL_STACK.FreeX509_CRL;
begin
  if @self <> nil then
    OPENSSL_sk_pop_free(@self, @X509_CRL_free);
end;

procedure OPENSSL_STACK.FreeX509_EXTENSION;
begin
  if @self <> nil then
    OPENSSL_sk_pop_free(@self, @X509_EXTENSION_free);
end;


{ SSL_CIPHER }

function SSL_CIPHER.Description: RawUtf8;
var
  cipher: TByteToAnsiChar;
  s, d: PtrInt;
begin
  if (@self = nil) or
     (SSL_CIPHER_description(@self, @cipher, SizeOf(cipher)) = nil) then
    result := ''
  else
  begin
    s := 0;
    d := 0;
    while cipher[s] <> #0 do
    begin
      while PWord(@cipher[s])^ = $2020 do // remove any double space
        inc(s);
      if cipher[s] >= ' ' then
      begin
        if s <> d then
          cipher[d] := cipher[s];
        inc(d);
      end;
      inc(s);
    end;
    FastSetString(result, @cipher, d);
  end;
end;


{ SSL }

function SSL.PeerCertificate: PX509;
begin
  if @self = nil then
    result := nil
  else
    result := SSL_get_peer_certificate(@self);
end;

function SSL.CurrentCipher: PSSL_CIPHER;
begin
  if @self = nil then
    result := nil
  else
    result := SSL_get_current_cipher(@self);
end;

function SSL.PeerChain: Pstack_st_X509;
begin
  if @self = nil then
    result := nil
  else
    result := SSL_get_peer_cert_chain(@self);
end;

function SSL.PeerCertificates(acquire: boolean): PX509DynArray;
var
  i: PtrInt;
begin
  result := PX509DynArray(PeerChain.ToDynArray);
  if acquire then
    for i := 0 to length(result) - 1 do
      result[i].Acquire;
end;

function SSL.PeerCertificatesAsPEM: RawUtf8;
begin
  result := PX509DynArrayToPem(PeerCertificates);
end;

function SSL.PeerCertificatesAsText: RawUtf8;
begin
  result := PX509DynArrayToText(PeerCertificates);
end;

function SSL.IsVerified(msg: PRawUtf8): boolean;
var
  res: integer;
begin
  res := X509_V_OK;
  if @self <> nil then
    res := SSL_get_verify_result(@self);
  result := (res = X509_V_OK); // not yet verified, or peer verification failed
  if (msg <> nil) and
     not result then // append '(text #error)' to msg^
    msg^ := RawUtf8(format('%s (%s #%d)',
              [msg^, X509_verify_cert_error_string(res), res]));
end;

procedure SSL.Free;
begin
  if @self <> nil then
    SSL_free(@self);
end;


{ SSL_CTX }

procedure SSL_CTX.SetCertificateFiles(const CertFile, KeyFile: RawUtf8;
  const KeyPassword: SpiUtf8);
begin
  EOpenSslNetTls.Check(
    SSL_CTX_use_certificate_chain_file(@self, pointer(CertFile)),
    'SetCertificateFiles chain_file'); // note: @self <> PSSL so keep ssl=nil
  if KeyPassword <> '' then
    SSL_CTX_set_default_passwd_cb_userdata(@self, pointer(KeyPassword));
  EOpenSslNetTls.Check(
    SSL_CTX_use_PrivateKey_file(@self, pointer(KeyFile), SSL_FILETYPE_PEM),
    'SetCertificateFiles key_file');
  EOpenSslNetTls.Check(SSL_CTX_check_private_key(@self),
    'SetCertificateFiles check');
end;

procedure SSL_CTX.Free;
begin
  if @self <> nil then
    SSL_CTX_free(@self);
end;


{ BIO }

function BIO.get_flags: integer;
begin
  result := BIO_test_flags(@self, not 0);
end;

function BIO.should_retry: boolean;
begin
  result := BIO_test_flags(@self, BIO_FLAGS_SHOULD_RETRY) <> 0;
end;

function BIO.should_read: boolean;
begin
  result := BIO_test_flags(@self, BIO_FLAGS_READ) <> 0;
end;

function BIO.should_write: boolean;
begin
  result := BIO_test_flags(@self, BIO_FLAGS_WRITE) <> 0;
end;

function BIO.should_io_special: boolean;
begin
  result := BIO_test_flags(@self, BIO_FLAGS_IO_SPECIAL) <> 0;
end;

function BIO.retry_type: integer;
begin
  result := BIO_test_flags(@self, BIO_FLAGS_RWS);
end;

function BIO.get_ssl(s: PSSL): integer;
begin
  result := BIO_ctrl(@self, BIO_C_GET_SSL, 0, s);
end;

function BIO.pending: integer;
begin
  result := BIO_ctrl(@self, _BIO_CTRL_PENDING, 0, nil);
end;

function BIO.eof: boolean;
begin
  result := BIO_ctrl(@self, BIO_CTRL_EOF, 0, nil) = 1;
end;

procedure BIO.Reset;
begin
  if @self <> nil then
    BIO_ctrl(@self, BIO_CTRL_RESET, 0, nil);
end;

procedure BIO.ToString(var data: RawByteString; cp: integer);
var
  mem: PBUF_MEM;
begin
  if (@self = nil) or
     (BIO_ctrl(@self, BIO_C_GET_BUF_MEM_PTR, 0, @mem) <> OPENSSLSUCCESS) then
    data := ''
  else
    FastSetStringCP(data, mem.data, mem.length, cp);
end;

function BIO.ToUtf8: RawUtf8;
begin
  ToString(RawByteString(result), CP_UTF8);
end;

function BIO.ToUtf8AndFree: RawUtf8;
begin
  ToString(RawByteString(result), CP_UTF8);
  Free;
end;

procedure BIO.Free;
begin
  if @self <> nil then
    BIO_free(@self);
end;

type
  TBioSave = function(bio: PBIO; instance: pointer): integer; cdecl;
  TBioLoad = function(bio: PBIO; instance: pointer): pointer; cdecl;

function BioSave(instance: pointer; sav: TBioSave;
  codepage: integer = CP_RAWBYTESTRING): RawByteString;
var
  bio: PBIO;
begin
  result := '';
  if instance = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  try
    EOpenSsl.Check(sav(bio, instance));
    bio.ToString(result, codepage);
  finally
    bio.Free;
  end;
end;

function BioLoad(const mem: RawByteString; load: TBioLoad): pointer;
var
  bio: PBIO;
begin
  EOpenSsl.CheckAvailable(nil, 'BioLoad');
  if mem = '' then
    result := nil
  else
  begin
    bio := BIO_new_mem_buf(pointer(mem), length(mem));
    result := load(bio, nil);
    bio.Free;
  end;
end;

{ X509_REQ }

function X509_REQ.GetName: PX509_NAME;
begin
  if @self = nil then
    result := nil
  else
    result := X509_REQ_get_subject_name(@self);
end;

function X509_REQ.ToBinary: RawByteString;
begin
  result := BioSave(@self, @i2d_X509_REQ_bio);
end;

function X509_REQ.ToPem: RawUtf8;
begin
  result := BioSave(@self, @PEM_write_bio_X509_REQ, CP_UTF8);
end;

procedure X509_REQ.AddExtension(nid: integer; const value: RawUtf8);
var
  ex: PX509_EXTENSION;
  exts: Pstack_st_X509_EXTENSION;
begin
  if @self = nil then
    exit;
  ex := X509V3_EXT_conf_nid(nil, nil, nid, pointer(value));
  if ex = nil then
    exit;
  exts := NewOpenSslStack;
  exts.Add(ex);
  X509_REQ_add_extensions(@self, exts);
  exts.Free;
  ex.Free;
end;

function X509_REQ.Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  if (@self = nil) or
     (pkey = nil) then
    result := 0
  else
    result := X509_REQ_sign(@self, pkey, md);
end;

function X509_REQ.GetPublicKey: PEVP_PKEY;
begin
  if @self = nil then
    result := nil
  else
    result := X509_REQ_get_pubkey(@self);
end;

function X509_REQ.Verify(pkey: PEVP_PKEY): boolean;
begin
  if (@self = nil) or
     (pkey = nil) then
    result := false
  else
    result := X509_REQ_verify(@self, pkey) = OPENSSLSUCCESS;
end;

function X509_REQ.VerifySelf: boolean;
var
  pkey: PEVP_PKEY;
begin
  result := false;
  if @self = nil then
    exit;
  pkey := X509_REQ_get_pubkey(@self);
  if pkey = nil then
    exit;
  result := Verify(pkey);
  pkey.Free;
end;

const
  _CA: array[boolean] of RawUtf8 = (
    'critical,CA:FALSE',
    'critical,CA:TRUE');

  KU_: array[kuEncipherOnly .. kuDecipherOnly] of RawUtf8 = (
    ',encipherOnly',
    ',cRLSign',
    ',keyCertSign',
    ',keyAgreement',
    ',dataEncipherment',
    ',keyEncipherment',
    ',nonRepudiation',
    ',digitalSignature',
    ',decipherOnly');

  XU_: array[kuTlsServer .. kuTimestamp] of RawUtf8 = (
    'serverAuth,',
    'clientAuth,',
    'emailProtection,',
    'codeSigning,',
    'OCSPSigning,',
    'timeStamping,');

function KuText(usages: TX509Usages): RawUtf8;
var
  u: TX509Usage;
begin
  result := '';
  for u := low(KU_) to high(KU_) do
    if u in usages then
      result := Join([result, KU_[u]]);
end;

function XuText(usages: TX509Usages): RawUtf8;
var
  u: TX509Usage;
begin
  result := '';
  for u := low(XU_) to high(XU_) do
    if u in usages then
      result := Join([result, XU_[u]]);
end;

function X509_REQ.SetUsageAndAltNames(
  usages: TX509Usages; const altnames: RawUtf8): boolean;
var
  exts: Pstack_st_X509_EXTENSION;
  v: RawUtf8;

  function Add(ext_nid: integer; value: PUtf8Char): boolean;
  var
    ex: PX509_EXTENSION;
  begin
    ex := X509V3_EXT_conf_nid(nil, nil, ext_nid, value);
    result := (ex <> nil);
    if result then
      exts.Add(ex);
  end;

begin
  result := false;
  if @self = nil then
    exit;
  exts := NewOpenSslStack;
  try
    if altnames <> '' then
      if not Add(NID_subject_alt_name, pointer(altnames)) then
        exit;
    if kuCA in usages then
      if not Add(NID_basic_constraints, pointer(_CA[true])) then
        exit;
    v := KuText(usages);
    if v <> '' then
    begin
      v := Join(['critical', v]); // heading comma included
      if not Add(NID_key_usage, pointer(v)) then
        exit;
    end;
    v := XuText(usages);
    if v <> '' then
    begin
      SetLength(v, length(v) - 1); // trailing comma
      if not Add(NID_ext_key_usage, pointer(v)) then
        exit;
    end;
    result := (X509_REQ_add_extensions(@self, exts) = OPENSSLSUCCESS);
  finally
    exts.FreeX509_EXTENSION;
  end;
end;

procedure X509_REQ.Free;
begin
  if @self <> nil then
    X509_REQ_free(@self);
end;


{ X509_NAME }

function X509_NAME.Count: integer;
begin
  if @self = nil then
    result := 0
  else
    result := X509_NAME_entry_count(@self);
end;

function X509_NAME.Item(ndx: integer): PX509_NAME_ENTRY;
begin
  if @self = nil then
    result := nil
  else
    result := X509_NAME_get_entry(@self, ndx);
end;

function X509_NAME.GetEntry(NID: integer): RawUtf8;
var
  tmp: TSynTempBuffer;
  L: integer;
begin
  result := '';
  if (@self = nil) or
     (NID <= 0) then
    exit;
  L := SizeOf(tmp);
  L := X509_NAME_get_text_by_NID(@self, NID, @tmp, L); // not MBSTRING ready
  if L > 0 then
    FastSetString(result, @tmp, L);
end;

function X509_NAME.GetEntry(const Name: RawUtf8): RawUtf8;
begin
  result := GetEntry(OBJ_txt2nid(pointer(name)));
end;

procedure X509_NAME.ToUtf8(out result: RawUtf8; flags: cardinal);
var
  bio: PBIO;
begin
  if @self = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  X509_NAME_print_ex(bio, @self, 0, flags);
  result := bio.ToUtf8AndFree;
end;

procedure X509_NAME.AddEntry(const Name, Value: RawUtf8);
begin
  if (@self <> nil) and
     (Name <> '') and
     (Value <> '') then
    X509_NAME_add_entry_by_txt(@self, pointer(Name),
      MBSTRING[not IsAnsiCompatible(Value)], pointer(Value), -1, -1, 0);
end;

procedure X509_NAME.AddEntries(const Country, State, Locality, Organization,
  OrgUnit, CommonName, EmailAddress, SurName, GivenName, SerialNumber: RawUtf8);
begin
  // warning: don't check for duplicates
  AddEntry('C',  Country);
  AddEntry('ST', State);
  AddEntry('L',  Locality);
  AddEntry('O',  Organization);
  AddEntry('OU', OrgUnit);
  AddEntry('CN', CommonName);
  AddEntry('emailAddress', EmailAddress);
  AddEntry('SN', Surname);
  AddEntry('GN', GivenName);
  AddEntry('serialNumber', SerialNumber);
end;

procedure X509_NAME.SetEntry(const Name, Value: RawUtf8);
var
  nid: integer;
begin
  nid := OBJ_txt2nid(pointer(Name));
  if (@self = nil) or
     (Name = '') or
     (nid <= 0) or
     (GetEntry(nid) = Value) then
    exit;
  DeleteEntry(nid);
  AddEntry(Name, Value);
end;

procedure X509_NAME.DeleteEntry(NID: integer);
var
  last, loc: integer;
begin
  if (@self = nil) or
     (NID <= 0) then
    exit;
  last := -1;
  repeat
    loc := X509_NAME_get_index_by_NID(@self, NID, last);
    if loc < 0 then
      break;
    X509_NAME_delete_entry(@self, loc).Free;
    last := loc; // find and delete all occurrences
  until false;
end;

procedure X509_NAME.DeleteEntry(const Name: RawUtf8);
begin
  DeleteEntry(OBJ_txt2nid(pointer(name)));
end;

function X509_NAME.Compare(another: PX509_NAME): integer;
begin
  if @self = another then // not done in OpenSSL C code
    result := 0
  else
    result := X509_NAME_cmp(@self, another); // will compare the DER binary
end;

function X509_NAME.Hash: cardinal;
begin
  if @self = nil then
    result := 0
  else
    result := X509_NAME_hash(@self);
end;

function X509_NAME.ToBinary(out dest: TSynTempBuffer): integer;
var
  data: PByte;
begin
  result := 0;
  if @self = nil then
    exit;
  data := @dest;
  result := i2d_X509_NAME(@self, nil); // compute canonical encoding in cache
  if (result <= 0) or
     (result > SizeOf(dest)) or
     (i2d_X509_NAME(@self, @data) <> result) then
    result := 0 // avoid buffer overflow (4KB is big enough for any real value)
end;

function X509_NAME.ToBinary: RawByteString;
var
  tmp: TSynTempBuffer;
begin
  FastSetRawByteString(result, @tmp, ToBinary(tmp));
end;

function X509_NAME.ToDigest(md: PEVP_MD): RawUtf8;
var
  tmp: TSynTempBuffer;
begin
  result := Digest(md, @tmp, ToBinary(tmp));
end;

function X509_NAME.ToText: RawUtf8;
begin
  ToUtf8(result);
end;


{ X509_NAME_ENTRY }

function X509_NAME_ENTRY.Data: PASN1_STRING;
begin
  result := X509_NAME_ENTRY_get_data(@self);
end;

function X509_NAME_ENTRY.NID: integer;
begin
  result := X509_NAME_ENTRY_get_object(@self).NID;
end;

function X509_NAME_ENTRY.Name: PUtf8Char;
begin
  result := X509_NAME_ENTRY_get_object(@self).Name;
end;

function X509_NAME_ENTRY.Value: RawUtf8;
begin
  Data^.ToUtf8(result);
end;

procedure X509_NAME_ENTRY.Free;
begin
  if @self <> nil then
    X509_NAME_ENTRY_free(@self);
end;


{ X509_REVOKED }

function X509_REVOKED.SerialNumber: RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    X509_REVOKED_get0_serialNumber(@self).ToHex(result);
end;

function X509_REVOKED.RevocationDate: TDateTime;
begin
  if @self = nil then
    result := 0
  else
    result := X509_REVOKED_get0_revocationDate(@self).ToDateTime; // may be 0
end;

function X509_REVOKED.Reason: integer;
var
  crit: integer;
  enum: pointer;
begin
  result := CRL_REASON_UNSPECIFIED; // = 0
  if @self = nil then
    exit;
  crit := 0;
  enum := X509_REVOKED_get_ext_d2i(@self, NID_crl_reason, @crit, nil);
  if enum = nil then
    if crit <> -1 then
      exit
    else
      result := CRL_REASON_NONE // = -1
  else
  begin
    result := ASN1_ENUMERATED_get(enum);
    ASN1_ENUMERATED_free(enum);
  end;
end;

function X509_REVOKED.SetReason(value: integer): boolean;
var
  enum: PASN1_ENUMERATED;
begin
  result := false;
  if @self = nil then
    exit;
  enum := ASN1_ENUMERATED_new();
  result := (ASN1_ENUMERATED_set(enum, value) = OPENSSLSUCCESS) and
    (X509_REVOKED_add1_ext_i2d(@self, NID_crl_reason, enum, 0, 0) = OPENSSLSUCCESS);
  ASN1_ENUMERATED_free(enum);
end;

function X509_REVOKED.ToBinary: RawByteString;
begin
  result := BioSave(@self, @i2d_X509_REVOKED);
end;

procedure X509_REVOKED.Free;
begin
  if @self <> nil then
    X509_REVOKED_free(@self);
end;


{ X509_CRL }

function X509_CRL.Version: integer;
begin
  if @self = nil then
    result := 0
  else
    result := X509_CRL_get_version(@self);
end;

function X509_CRL.LastUpdate: TDateTime;
begin
  if @self = nil then
    result := 0
  else
    result := X509_CRL_get_lastUpdate(@self).ToDateTime; // may return 0
end;

function X509_CRL.NextUpdate: TDateTime;
begin
  if @self = nil then
    result := 0
  else
    result := X509_CRL_get_nextUpdate(@self).ToDateTime;
end;

function X509_CRL.IssuerName: RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    X509_CRL_get_issuer(@self).ToUtf8(result);
end;

function X509_CRL.Revoked: Pstack_st_X509_REVOKED;
begin
  if @self = nil then
    result := nil
  else
    result := X509_CRL_get_REVOKED(@self);
end;

function X509_CRL.IsRevoked(const serialnumber: RawUtf8): integer;
var
  rev: Pstack_st_X509_REVOKED;
  r: PX509_REVOKED;
  i: integer;
begin
  rev := Revoked;
  for i := 0 to rev^.Count - 1 do
  begin
    r := rev.Items[i];
    if r.SerialNumber = serialnumber then
    begin
      result := r.Reason;
      if (result >= 0) and
         (result <> CRL_REASON_REMOVE_FROM_CRL) then
        exit;
    end;
  end;
  result := CRL_REASON_NONE; // = -1 if not revoked
end;

function X509_CRL.IsRevoked(serial: PASN1_INTEGER): integer;
var
  rev: Pstack_st_X509_REVOKED;
  r: PX509_REVOKED;
  i: integer;
begin
  if serial.Len <> 0 then
  begin
    rev := Revoked;
    for i := 0 to rev^.Count - 1 do
    begin
      r := rev.Items[i];
      if X509_REVOKED_get0_serialNumber(r).Equals(serial) then
      begin
        result := r.Reason;
        if (result >= 0) and
           (result <> CRL_REASON_REMOVE_FROM_CRL) then
          exit;
      end;
    end;
  end;
  result := CRL_REASON_NONE; // = -1 if not revoked
end;

function X509_CRL.AddFrom(another: PX509_CRL): integer;
var
  rev: Pstack_st_X509_REVOKED;
  i: integer;
begin
  result := 0;
  if (@self = nil) or
     (another = nil) then
    exit;
  rev := Revoked;
  if rev^.Count = 0 then
    exit;
  for i := 0 to rev^.Count - 1 do
    if X509_CRL_add0_revoked(@self, rev.Items[i]) = OPENSSLSUCCESS then
      inc(result);
  if result <> 0 then
    X509_CRL_sort(@self);
end;

function X509_CRL.AddFromPem(const Pem: RawUtf8): integer;
var
  crl: PX509_CRL;
begin
  crl := LoadCertificateCrlFromPem(Pem);
  result := AddFrom(crl);
  crl.Free;
end;

function X509_CRL.AddRevokedSerial(serial: PASN1_INTEGER; ca: PX509;
  reason, lastUpdateDays, nextUpdateDays: integer): boolean;
var
  rev: PX509_REVOKED;
  tm: PASN1_TIME;
begin
  result := false;
  if (@self = nil) or
     (serial = nil) or
     (IsRevoked(serial) >= 0) or
     ((ca <> nil) and
      (not ca.IsCA)) then
    exit;
  rev := X509_REVOKED_new();
  X509_REVOKED_set_serialNumber(rev, serial);
  tm := ASN1_TIME_new(); // now
  X509_CRL_set_lastUpdate(@self, tm);
  if lastUpdateDays >= 0 then
    X509_gmtime_adj(tm, SecsPerDay * lastUpdateDays);
  X509_REVOKED_set_revocationDate(rev, tm);
  if lastUpdateDays >= 0 then
    X509_gmtime_adj(tm, -SecsPerDay * lastUpdateDays);
  X509_gmtime_adj(tm, SecsPerDay * nextUpdateDays);
  X509_CRL_set_nextUpdate(@self, tm);
  ASN1_TIME_free(tm);
  if reason = CRL_REASON_UNSPECIFIED then
    reason := CRL_REASON_SUPERSEDED;
  rev.SetReason(reason);
  if X509_CRL_add0_revoked(@self, rev) = OPENSSLSUCCESS then
  begin
    X509_CRL_sort(@self);
    if ca <> nil then
      X509_CRL_set_issuer_name(@self, ca.GetSubjectName);
    result := true;
  end
  else
    rev^.Free;
end;

function X509_CRL.AddRevokedCertificate(x, ca: PX509;
  reason, lastUpdateDays, nextUpdateDays: integer): boolean;
begin
  result := (x <> nil) and
            AddRevokedSerial(X509_get_serialNumber(x), ca,
              reason, lastUpdateDays, nextUpdateDays);
end;

function X509_CRL.Extensions: Pstack_st_X509_EXTENSION;
begin
  if @self = nil then
    result := nil
  else
    result := X509_CRL_get0_extensions(@self);
end;

function X509_CRL.GetExtensions: TX509_Extensions;
var
  i: PtrInt;
  ext: Pstack_st_X509_EXTENSION;
begin
  ext := Extensions;
  SetLength(result, ext.Count);
  for i := 0 to length(result) - 1 do
    result[i].SetExtension(ext.Items[i]);
end;

function X509_CRL.Extension(nid: integer): PX509_EXTENSION;
var
  i: integer;
  ext: Pstack_st_X509_EXTENSION;
begin
  ext := Extensions;
  for i := 0 to ext.Count - 1 do
  begin
    result := ext.Items[i];
    if OBJ_obj2nid(X509_EXTENSION_get_object(result)) = nid then
      exit;
  end;
  result := nil;
end;

function X509_CRL.ToBinary: RawByteString;
begin
  result := BioSave(@self, @i2d_X509_CRL_bio);
end;

function X509_CRL.ToPem: RawUtf8;
begin
  try
    result := BioSave(@self, @PEM_write_bio_X509_CRL, CP_UTF8);
  except
    result := ''; // EOpenSSL if the CRL is not signed -> ignore
  end;
end;

function X509_CRL.ToText: RawUtf8;
begin
  result := BioSave(@self, @X509_CRL_print, CP_UTF8);
end;

function X509_CRL.Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  if @self = nil then
    result := 0
  else
    result := X509_CRL_sign(@self, pkey, md);
end;

function X509_CRL.Acquire: integer;
begin
  if @self = nil then
    result := -1
  else
    result := X509_CRL_up_ref(@self);
end;

procedure X509_CRL.Free;
begin
  if @self <> nil then
    X509_CRL_free(@self);
end;


{ X509_STORE_CTX }

function X509_STORE_CTX.CurrentCert: PX509;
begin
  if @self = nil then
    result := nil
  else
    result := X509_STORE_CTX_get_current_cert(@self);
end;

function X509_STORE_CTX.CurrentError(errstr: PPUtf8Char): integer;
begin
  if @self = nil then
    result := -1
  else
    result := X509_STORE_CTX_get_error(@self);
  if errstr <> nil then
    errstr^ := X509_verify_cert_error_string(result);
end;

procedure X509_STORE_CTX.Free;
begin
  if @self <> nil then
    X509_STORE_CTX_free(@self);
end;



{ X509_STORE }

procedure X509_STORE.Lock;
begin
  if @self <> nil then
    X509_STORE_lock(@self);
end;

procedure X509_STORE.UnLock;
begin
  if @self <> nil then
    X509_STORE_unlock(@self);
end;

function CountObjects(store: PX509_STORE; crl: boolean): integer;
var
  i: integer; // no PtrInt here for integer C API parameters
  p: pointer; // either PX509 or PX509_CRL
  obj: Pstack_st_X509_OBJECT;
begin
  result := 0;
  if store = nil then
    exit;
  X509_STORE_lock(store);
  obj := X509_STORE_get0_objects(store);
  for i := 0 to obj^.Count - 1 do
  begin
    p := obj^.Items[i];
    if crl then
      p := X509_OBJECT_get0_X509_CRL(p)
    else
      p := X509_OBJECT_get0_X509(p);
    inc(result, ord(p <> nil));
  end;
  X509_STORE_unlock(store);
end;

function GetObjectsLocked(store: PX509_STORE; crl: boolean): TPointerDynArray;
var
  i, n: PtrInt;
  p: pointer;    // either PX509 or PX509_CRL
  obj: Pstack_st_X509_OBJECT;
begin
  result := nil;
  if store = nil then
    exit;
  X509_STORE_lock(store);
  obj := X509_STORE_get0_objects(store);
  SetLength(result, obj^.Count);
  n := 0;
  for i := 0 to length(result) - 1 do
  begin
    p := obj^.Items[i];
    if crl then
      p := X509_OBJECT_get0_X509_CRL(p)
    else
      p := X509_OBJECT_get0_X509(p);
    if p = nil then
      continue; // this object is not of the expected type
    result[n] := p;  // store by reference with no Acquire
    inc(n);
  end;
  if n = 0 then
    result := nil
  else
    DynArrayFakeLength(result, n);
  // caller should eventually run store.UnLock
end;

// our own version of X509_STORE_get1_all_certs() - not exported on oldest API
function StackObjects(store: PX509_STORE; crl, addref: boolean): POPENSSL_STACK;
var
  i: integer; // no PtrInt here for integer C API parameters
  p: pointer; // either PX509 or PX509_CRL
  obj: Pstack_st_X509_OBJECT;
begin
  result := nil;
  if store = nil then
    exit;
  X509_STORE_lock(store);
  obj := X509_STORE_get0_objects(store);
  for i := 0 to obj^.Count - 1 do
  begin
    p := obj^.Items[i];
    if crl then
      p := X509_OBJECT_get0_X509_CRL(p)
    else
      p := X509_OBJECT_get0_X509(p);
    if p = nil then
      continue;
    if addref then
      if crl then // inlined Acquire
        X509_CRL_up_ref(p)
      else
        X509_up_ref(p);
    if result = nil then
      result := NewOpenSslStack;
    result.Add(p);
  end;
  X509_STORE_unlock(store);
end;

function X509_STORE.CertificateCount: integer;
begin
  result := CountObjects(@self, {crl=}false);
end;

function X509_STORE.CrlCount: integer;
begin
  result := CountObjects(@self, {crl=}true);
end;

function X509_STORE.CertificatesLocked: PX509DynArray;
begin
  result := PX509DynArray(GetObjectsLocked(@self, {crl=}false));
  // caller should eventually run UnLock
end;

function X509_STORE.CrlsLocked: PX509_CRLDynArray;
begin
  result := PX509_CRLDynArray(GetObjectsLocked(@self, {crl=}true));
  // caller should eventually run UnLock
end;

function X509_STORE.StackX509(addref: boolean): Pstack_st_X509;
begin
  result := StackObjects(@self, {crl=}false, addref);
end;

function X509_STORE.StackX509_CRL(addref: boolean): Pstack_st_X509_CRL;
begin
  result := StackObjects(@self, {crl=}true, addref);
end;

function X509_STORE.MainCrlAcquired: PX509_CRL;
var
  i: integer; // no PtrInt here for integer C API parameters
  obj: Pstack_st_X509_OBJECT;
begin
  result := nil;
  if @self = nil then
    exit;
  X509_STORE_lock(@self);
  obj := X509_STORE_get0_objects(@self);
  for i := 0 to obj^.Count - 1 do
  begin
    result := X509_OBJECT_get0_X509_CRL(obj^.Items[i]);
    if result = nil then
      continue; // not a CRL
    result.Acquire;
    break;
  end;
  X509_STORE_unlock(@self);
  if result = nil then
  begin
    result := NewCertificateCrl; // set a first CRL
    if not AddCrl(result) then
    begin
      result.Free;
      result := nil;
    end
    else
      result.Acquire;
  end;
end;

function X509_STORE.BySerial(const serial: RawUtf8): PX509;
var
  i: PtrInt;
  c: PX509DynArray;
begin
  result := nil;
  if serial = '' then
    exit;
  c := CertificatesLocked;
  for i := 0 to length(c) - 1 do
    if c[i].SerialNumber = serial then
    begin
      result := c[i];
      result.Acquire;
      break;
    end;
  Unlock;
end;

function X509_STORE.BySkid(const id: RawUtf8): PX509;
var
  i: PtrInt;
  c: PX509DynArray;
begin
  result := nil;
  if id = '' then
    exit;
  c := CertificatesLocked;
  for i := 0 to length(c) - 1 do
    if c[i].SubjectKeyIdentifier = id then
    begin
      result := c[i];
      result.Acquire;
      break;
    end;
  UnLock;
end;

function X509_STORE.HasSerial(serial: PASN1_INTEGER): boolean;
var
  i: PtrInt;
  c: PX509DynArray;
begin
  result := false;
  if (@self = nil) or
     (serial = nil) then
    exit;
  c := CertificatesLocked;
  for i := 0 to length(c) - 1 do
    if c[i].GetSerial = serial then
    begin
      result := true;
      break;
    end;
  UnLock;
end;

function X509_STORE.IsRevoked(const serial: RawUtf8): integer;
var
  i: PtrInt;
  rev: integer;
  c: PX509_CRLDynArray;
begin
  result := CRL_REASON_NONE; // -1 if not revoked
  if (@self = nil) or
     (serial = '') then
    exit;
  c := CrlsLocked;
  for i := 0 to length(c) - 1 do
  begin
    rev := c[i].IsRevoked(serial);
    if rev < 0 then
      continue;
    result := rev;
    break;
  end;
  UnLock;
end;

function X509_STORE.IsRevoked(serial: PASN1_INTEGER): integer;
var
  i: PtrInt;
  rev: integer;
  c: PX509_CRLDynArray;
begin
  result := CRL_REASON_NONE; // -1 if not revoked
  if (@self = nil) or
     (serial = nil) then
    exit;
  c := CrlsLocked;
  for i := 0 to length(c) - 1 do
  begin
    rev := c[i].IsRevoked(serial);
    if rev < 0 then
      continue;
    result := rev;
    break;
  end;
  UnLock;
end;

function X509_STORE.SetDefaultPaths: boolean;
begin
  result := (@self <> nil) and
            (X509_STORE_set_default_paths(@self) = OPENSSLSUCCESS);
end;

function X509_STORE.AddCertificate(x: PX509): boolean;
begin
  result := (@self <> nil) and
            (x <> nil) and
            (X509_STORE_add_cert(@self, x) = OPENSSLSUCCESS);
end;

function X509_STORE.AddCertificates(const x: PX509DynArray): boolean;
var
  i: PtrInt;
begin
  result := false;
  if @self = nil then
    exit;
  for i := 0 to length(x) - 1 do
    if x[i] <> nil then
      if X509_STORE_add_cert(@self, x[i]) <> OPENSSLSUCCESS then
        exit;
  result := true;
end;

function X509_STORE.AddCrl(c: PX509_CRL): boolean;
begin
  result := (@self <> nil) and
            (c <> nil) and
            (X509_STORE_add_crl(@self, c) = OPENSSLSUCCESS);
end;

function X509_STORE.AddFromBinary(const Der: RawByteString): RawUtf8;
var
  x: PX509;
  c: PX509_CRL;
begin
  result := '';
  if (@self = nil) or
     (Der = '') then
    exit;
  x := LoadCertificate(Der);
  if x <> nil then
  begin
    if AddCertificate(x) then
      result := x.SerialNumber;
    x.Free;
  end
  else
  begin
    c := LoadCertificateCrl(Der);
    if c <> nil then
    begin
      AddCrl(c);
      c.Free;
    end;
  end;
end;

procedure AddSerial(var Serials: TRawUtf8DynArray; x: PX509);
var
  n: PtrInt;
begin
  n := length(Serials);
  SetLength(Serials, n + 1);
  Serials[n] := x^.SerialNumber;
end;

function X509_STORE.AddCertificateFromPem(const Pem: RawUtf8;
  Serials: PRawUtf8DynArray): integer;
var
  x: PX509DynArray;
  i: PtrInt;
begin
  result := 0;
  if @self = nil then
    exit;
  x := LoadCertificates(Pem);
  for i := 0 to length(x) - 1 do
    if AddCertificate(x[i]) then
    begin
      inc(result);
      if Serials <> nil then
        AddSerial(Serials^, x[i]);
    end
    else
      x[i].Free;
end;

function X509_STORE.AddCrlFromPem(const Pem: RawUtf8): integer;
var
  c: PX509_CRL;
begin
  result := 0;
  if @self = nil then
    exit;
  c := LoadCertificateCrlFromPem(Pem);
  if c <> nil then
    if AddCrl(c) then
      inc(result)
    else
      c.Free;
end;

function X509_STORE.SetLocations(const CAFile, CAFolder: RawUtf8): boolean;
begin
  result := (@self <> nil) and
            (X509_STORE_load_locations(@self,
               pointer(CAFile), pointer(CAFolder)) = OPENSSLSUCCESS);
end;

function X509_STORE.Verify(x509: PX509; chain: Pstack_st_X509;
  errstr: PPUtf8Char; errcert: PPX509; callback: X509_STORE_CTX_verify_cb;
  MaxDepth: integer; Flags: cardinal): integer;
var
  c: PX509_STORE_CTX;
  param: PX509_VERIFY_PARAM;
begin
  result := -1;
  if @self = nil then
    exit;
  c := X509_STORE_CTX_new();
  if c <> nil then
  try
    if X509_STORE_CTX_init(c, @self, x509, chain) = OPENSSLSUCCESS then
    begin
      if MaxDepth > 0 then
      begin
        param := X509_VERIFY_PARAM_new();
        X509_VERIFY_PARAM_set_flags(param, Flags);
        X509_VERIFY_PARAM_set_depth(param, MaxDepth);
        X509_STORE_CTX_set0_param(c, param);
      end;
      if Assigned(callback) then
        X509_STORE_CTX_set_verify_cb(c, callback);
      if X509_verify_cert(c) = OPENSSLSUCCESS then
        result := 0; // success!
    end;
    if result <> 0 then
      result := c.CurrentError(errstr);
    if errcert <> nil then
    begin
      errcert^ := c.CurrentCert;
      X509_up_ref(errcert^);
    end;
  finally
    c.Free;
  end;
end;

procedure X509_STORE.Free;
begin
  if @self <> nil then
    X509_STORE_free(@self);
end;


{ BIGNUM }

function BIGNUM.ToDecimal: RawUtf8;
var
  tmp: PUtf8Char;
begin
  result := '';
  if @self = nil then
    exit;
  tmp := BN_bn2dec(@self);
  FastSetString(result, tmp, StrLen(tmp));
  OpenSSL_Free(tmp);
end;

function BIGNUM.ToHex: RawUtf8;
var
  bin: RawByteString;
begin
  result := '';
  if @self = nil then
    exit;
  ToBin(bin);
  result := MacToHex(pointer(bin), length(bin));
end;

function BIGNUM.Size: integer;
begin
  if @self = nil then
    result := 0
  else
    result := BN_num_bytes(@self);
end;

procedure BIGNUM.ToBin(bin: PByte);
begin
  if @self <> nil then
    BN_bn2bin(@self, bin);
end;

procedure BIGNUM.ToBin(out bin: RawByteString);
begin
  pointer(bin) := FastNewString(Size);
  ToBin(pointer(bin));
end;

procedure BIGNUM.Free;
begin
  if @self <> nil then
    BN_free(@self);
end;


{ ASN1_INTEGER }

function ASN1_INTEGER.ToBigInt: PBIGNUM;
begin
  if @self = nil then
    result := nil
  else
    result := ASN1_INTEGER_to_BN(@self, nil);
end;

function ASN1_INTEGER.ToDecimal: RawUtf8;
begin
  result := ToBigInt.ToDecimal;
end;

procedure ASN1_INTEGER.Free;
begin
  if @self <> nil then
    ASN1_INTEGER_free(@self);
end;


{ ASN1_OBJECT }

function ASN1_OBJECT.NID: integer;
begin
  result := OBJ_obj2nid(@self);
end;

function ASN1_OBJECT.Name: PUtf8Char;
begin
  result := OBJ_nid2ln(OBJ_obj2nid(@self));
end;


{ asn1_string_st }

function asn1_string_st.Data: pointer;
begin
  if @self = nil then
    result := nil
  else
    result := ASN1_STRING_data(@self);
end;

function asn1_string_st.Len: integer;
begin
  if @self = nil then
    result := 0
  else
    result := ASN1_STRING_length(@self);
end;

function asn1_string_st.GetType: integer;
begin
  if @self = nil then
    result := 0
  else
    result := ASN1_STRING_type(@self);
end;

procedure asn1_string_st.ToUtf8(out result: RawUtf8; flags: cardinal);
var
  bio: PBIO;
begin
  if @self = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  ASN1_STRING_print_ex(bio, @self, flags);
  result := bio.ToUtf8AndFree;
end;

procedure asn1_string_st.ToHex(out result: RawUtf8);
begin
  if @self <> nil then
    ToHumanHex(result, Data, Len);
end;

function asn1_string_st.ToHex: RawUtf8;
begin
  ToHex(result);
end;

function asn1_string_st.Equals(another: pointer): boolean;
var
  n1, n2: PtrInt;
begin
  n1 := Len;
  n2 := PASN1_STRING(another).Len;
  result := (n1 = n2) and
    mormot.core.base.CompareMem(Data, PASN1_STRING(another).Data, n1);
end;


{ X509_EXTENSION }

function X509_EXTENSION.BasicConstraintIsCA: boolean;
var
  d: PASN1_OCTET_STRING;
  data: PByte;
  c: PBASIC_CONSTRAINTS;
begin
  result := false;
  if @self = nil then
    exit;
  d := X509_EXTENSION_get_data(@self);
  if d = nil then
    exit;
  data := d.data;
  if data = nil then
    exit;
  c := d2i_BASIC_CONSTRAINTS(nil, @data, d.Len);
  if c = nil then
    exit;
  result := c^.ca <> 0;
  BASIC_CONSTRAINTS_free(c);
end;

procedure X509_EXTENSION.ToUtf8(out result: RawUtf8; flags: cardinal);
var
  bio: PBIO;
begin
  if @self = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  X509V3_EXT_print(bio, @self, flags, 0);
  result := bio.ToUtf8AndFree;
  TrimSelf(result); // OpenSSL add ending spaces to some extensions  :(
end;

procedure X509_EXTENSION.Free;
begin
  if @self <> nil then
    X509_EXTENSION_free(@self);
end;


{ ASN1_TIME }

function ASN1_TIME.ToDateTime: TDateTime;
var
  t: tm;
begin
  if (@self <> nil) and
     (ASN1_TIME_to_tm(@self, @t) = OPENSSLSUCCESS) then
    result := TmToDateTime(t)
  else
    result := 0; // e.g. on deprecated OpenSSL without ASN1_TIME_to_tm()
end;


{ TX509_Extension }

procedure TX509_Extension.SetExtension(x: PX509_EXTENSION);
begin
  ext := x;
  nid := OBJ_obj2nid(X509_EXTENSION_get_object(x));
  value := X509_EXTENSION_get_data(x);
  critical := X509_EXTENSION_get_critical(x) <> 0;
end;


{ X509 }

function X509.GetSerial: PASN1_INTEGER;
begin
  if @self = nil then
    result := nil
  else
    result := X509_get_serialNumber(@self);
end;

function X509.GetSubjectName: PX509_NAME;
begin
  if @self = nil then
    result := nil
  else
    result := X509_get_subject_name(@self);
end;

function X509.GetIssuerName: PX509_NAME;
begin
  if @self = nil then
    result := nil
  else
    result := X509_get_issuer_name(@self);
end;

function X509.GetPublicKey: PEVP_PKEY;
begin
  if @self = nil then
    result := nil
  else
    result := X509_get_pubkey(@self);
end;

function X509.SerialNumber: RawUtf8;
begin
  GetSerial.ToHex(result);
end;

function X509.SubjectName: RawUtf8;
begin
  GetSubjectName.ToUtf8(result);
end;

function X509.IssuerName: RawUtf8;
begin
  GetIssuerName.ToUtf8(result);
end;

procedure GetNext(var P: PUtf8Char; Sep1, Sep2: AnsiChar; var result: RawUtf8);
var
  S, E: PUtf8Char;
begin // see GetNextItemTrimed() from mormot.core.text
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P); // trim left
  S := P;
  while not (S^ in [#0, Sep1, Sep2]) do
    inc(S);
  E := S;
  while (E > P) and
        (E[-1] in [#1..' ']) do
    dec(E); // trim right
  FastSetString(result, P, E - P);
  if S^ <> #0 then
    P := S + 1
  else
    P := nil;
end;

// note: PX509_Name.GetEntry() is not MBSTRING ready so we favor manual parsing
function GetPair(p: PUtf8Char; const rdn: RawUtf8): RawUtf8;
var
  nam: RawUtf8;
begin
  while p <> nil do
  begin
    GetNext(p, '=', #0, nam);
    if p = nil then
      break;
    GetNext(p, ',', '/', result);
    if nam = rdn then
      exit;
  end;
  result := ''; // id not found
end;

function X509.GetSubject(const id: RawUtf8): RawUtf8;
var
  md: PEVP_MD;
begin
  if id = 'DER' then
    result := GetSubjectName.ToBinary
  else
    result := GetPair(pointer(SubjectName), id);
  if result <> '' then
    exit;
  md := EVP_get_digestbyname(pointer(id));
  if md <> nil then
    result := GetSubjectName.ToDigest(md);
end;

function X509.GetIssuer(const id: RawUtf8): RawUtf8;
var
  md: PEVP_MD;
begin
  if id = 'DER' then
    result := GetIssuerName.ToBinary
  else
    result := GetPair(pointer(IssuerName), id);
  if result <> '' then
    exit;
  md := EVP_get_digestbyname(pointer(id));
  if md <> nil then
    result := GetIssuerName.ToDigest(md);
end;

function X509.PeerInfo: RawUtf8;
begin
  result := BioSave(@self, @X509_print, CP_UTF8);
end;

function X509.GetExtensions: TX509_Extensions;
var
  i, n: integer;
begin
  result := nil;
  if @self = nil then
    exit;
  n := X509_get_ext_count(@self);
  if n <= 0 then
    exit;
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i].SetExtension(X509_get_ext(@self, i));
end;

function X509.Extension(nid: integer): PX509_EXTENSION;
var
  loc: integer;
begin
  result := nil;
  if @self = nil then
    exit;
  loc := X509_get_ext_by_NID(@self, nid, -1);
  if loc >= 0 then
    result := X509_get_ext(@self, loc);
end;

function X509.ExtensionText(nid: integer): RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    Extension(nid).ToUtf8(result);
end;

function X509.IsCA: boolean;
begin
  result := Extension(NID_basic_constraints).BasicConstraintIsCA;
end;

function X509.IsSelfSigned: boolean;
begin
  // X509 usually does not compare serial numbers nor SKID/AKID but the names
  // in practice, OpenSSL self-signed certificates have a SKID but no AKID
  result := (@self <> nil) and
      (X509_get_issuer_name(@self).Compare(X509_get_subject_name(@self)) = 0);
end;

function X509.GetSignatureAlgo: RawUtf8;
var
  nid, md, bits: integer;
begin
  if @self = nil then
    result := ''
  else
  begin
    nid := X509_get_signature_nid(@self);
    result := OBJ_nid2sn(nid);
    if X509_get_signature_info(@self, @md, nil, @bits, nil) = OPENSSLSUCCESS then
    begin
      result := RawUtf8(format('%d %s', [bits, result]));
      if nid = NID_rsassaPss then // only PS256/PS384/PS512 don't supply the MD
        result := Join([result, '-', RawUtf8(OBJ_nid2sn(md))]);
    end;
  end;
end;

function X509.GetSignatureHash: RawUtf8;
var
  md: integer;
begin
  result := '';
  md := 0;
  if (@self <> nil) and
     (X509_get_signature_info(@self, @md, nil, nil, nil) = OPENSSLSUCCESS) and
     (md <> 0) then
    result := OBJ_nid2sn(md);
end;

const
  KU: array[kuEncipherOnly .. kuDecipherOnly] of integer = (
    X509v3_KU_ENCIPHER_ONLY,
    X509v3_KU_CRL_SIGN,
    X509v3_KU_KEY_CERT_SIGN,
    X509v3_KU_KEY_AGREEMENT,
    X509v3_KU_DATA_ENCIPHERMENT,
    X509v3_KU_KEY_ENCIPHERMENT,
    X509v3_KU_NON_REPUDIATION,
    X509v3_KU_DIGITAL_SIGNATURE,
    X509v3_KU_DECIPHER_ONLY);

  XU: array[kuTlsServer .. kuTimestamp] of integer = (
    XKU_SSL_SERVER,
    XKU_SSL_CLIENT,
    XKU_SMIME,
    XKU_CODE_SIGN,
    XKU_OCSP_SIGN,
    XKU_TIMESTAMP);

function X509.GetUsage: TX509Usages;
var
  f: integer;
  u: TX509Usage;
begin
  result := [];
  if @self = nil then
    exit;
  if IsCA then
    include(result, kuCA);
  f := X509_get_key_usage(@self); // returns -1 if not present
  if f > 0 then
    for u := low(KU) to high(KU) do
      if (f and KU[u]) <> 0 then
        include(result, u);
  f := X509_get_extended_key_usage(@self);
  if f > 0 then
    for u := low(XU) to high(XU) do
      if (f and XU[u]) <> 0 then
        include(result, u);
end;

function X509.HasUsage(u: TX509Usage): boolean;
var
  f: integer;
begin
  if @self = nil then
    result := false
  else
  if u = kuCA then
    result := IsCA
  else if (u >= low(KU)) and
          (u <= high(KU)) then
  begin
    f := X509_get_key_usage(@self); // -1 if not present
    result := (f > 0) and ((f and KU[u]) <> 0);
  end
  else if (u >= low(XU)) and
          (u <= high(XU)) then
  begin
    f := X509_get_extended_key_usage(@self);
    result := (f > 0) and ((f and XU[u]) <> 0);
  end
  else
    result := false;
end;

function X509.KeyUsage: RawUtf8;
begin
  result := ExtensionText(NID_key_usage);
end;

function X509.ExtendedKeyUsage: RawUtf8;
begin
  result := ExtensionText(NID_ext_key_usage);
end;

function X509.SubjectKeyIdentifier: RawUtf8;
begin
  result := ExtensionText(NID_subject_key_identifier);
end;

function X509.AuthorityKeyIdentifier: RawUtf8;
var
  i: PtrInt;
begin
  result := ExtensionText(NID_authority_key_identifier);
  if NetStartWith(pointer(result), 'KEYID:') then
    delete(result, 1, 6);
  i := PosExChar(#10, result);
  if i > 0 then // some certificates have e.g.
    // 'KEYID:F2:97:...:99'#$0A'DirName:/CN=SERMO/C=FR/ST=LA'#$0A'serial:...'
    FakeLength(result, i - 1);
end;

function AlternativeNames(p: PUtf8Char; dns: boolean): TRawUtf8DynArray;
var
  s: PUtf8Char;
  n: PtrInt;
begin
  result := nil;
  if p = nil then
    exit;
  n := 0;
  repeat
    while p^ in [#1..' ', ','] do
      inc(p);
    if p^ = #0 then
      break;
    if dns then
      if NetStartWith(p, 'DNS:') then
      begin
        inc(p, 4);
        s := p;
      end
      else
        s := nil
    else
      s := p; // return raw list of values
    while not (p^ in [#0 .. ' ', ',']) do
      inc(p);
    if s <> nil then
    begin
      SetLength(result, n + 1);
      FastSetString(result[n], s, p - s);
      inc(n);
    end;
  until P^ = #0;
end;

function X509.SubjectAlternativeNames(dns: boolean): TRawUtf8DynArray;
begin
  if @self = nil then
    result := nil
  else
    result := AlternativeNames(pointer(ExtensionText(NID_subject_alt_name)), dns);
end;

function X509.IssuerAlternativeNames(dns: boolean): TRawUtf8DynArray;
begin
  if @self = nil then
    result := nil
  else
    result := AlternativeNames(pointer(ExtensionText(NID_issuer_alt_name)), dns);
end;

function X509.NotBefore: TDateTime;
begin
  if @self = nil then
    result := 0
  else
    result := X509_getm_notBefore(@self).ToDateTime; // 0 if no ASN1_TIME_to_tm()
end;

function X509.NotAfter: TDateTime;
begin
  if @self = nil then
    result := 0
  else
    result := X509_getm_notAfter(@self).ToDateTime;
end;

function X509.IsValidDate(TimeUtc: TDateTime): boolean;
var
  na, nb: TDateTime;
begin
  na := NotAfter; // 0 if ASN1_TIME_to_tm() not supported by old OpenSSL
  nb := NotBefore;
  if TimeUtc = 0 then
    TimeUtc := NowUtc;
  result := ((na = 0) or
             (TimeUtc < na + CERT_DEPRECATION_THRESHOLD)) and
            ((nb = 0) or
             (TimeUtc + CERT_DEPRECATION_THRESHOLD > nb));
end;

function X509.FingerPrint(md: PEVP_MD): RawUtf8;
var
  dig: EVP_MD_DIG;
  len: integer;
begin
  if md = nil then
    md := EVP_sha1; // SHA-1 fingerprint by default
  len := 0;
  if (@self = nil) or
     (X509_digest(@self, md, @dig, @len) <> OPENSSLSUCCESS) or
     (len <= 0) or
     (len > SizeOf(dig)) then
    result := ''
  else
    result := MacToHex(@dig, len);
end;

function X509.SetValidity(ValidDays, ExpireDays: integer): boolean;
begin
  if ValidDays < 0 then
    ValidDays := 0; // X509_gmtime_adj() does not support negative values
  result := (@self <> nil) and
    (ExpireDays > ValidDays) and
    (X509_gmtime_adj(X509_getm_notBefore(@self), SecsPerDay * ValidDays) <> nil) and
    (X509_gmtime_adj(X509_getm_notAfter(@self),  SecsPerDay * ExpireDays) <> nil);
end;

function X509.SetExtension(nid: cardinal; const value: RawUtf8; issuer: PX509;
  subject: PX509): boolean;
var
  x, old: PX509_EXTENSION;
  prev, p: integer;
  c: v3_ext_ctx; // some extensions need the associated issuer/subject certs
begin
  result := false;
  if (@self = nil) or
     (nid = 0) or
     (value = '') then
    exit;
  if issuer = nil then
    issuer := @self;
  if subject = nil then
    subject := @self;
  FillCharFast(c, SizeOf(c), 0); // paranoid on non opaque structures
  X509V3_set_ctx(@c, issuer, subject, nil, nil, 0);
  x := X509V3_EXT_conf_nid(nil, @c, nid, pointer(value));
  if x = nil then
    exit;
  prev := -1;
  repeat
    p := X509_get_ext_by_NID(@self, nid, -1); // remove previous extensions
    if p < 0 then
      break;
    old := X509_delete_ext(@self, p);
    old.Free;
    prev := p; // put at the same location
  until false;
  result := X509_add_ext(@self, x, prev) = OPENSSLSUCCESS;
  x.Free;
end;

// see copy_extensions() from openssl/apps/lib/apps.c
procedure X509.CopyExtensions(exts: PX509_EXTENSIONS);
var
  i: integer;
begin
  if (@self <> nil) and
     (exts <> nil) then
    for i := 0 to exts^.Count - 1 do
      // won't try to remove any existing extension
      X509_add_ext(@self, exts.Items[i], -1);
end;

procedure X509.CopyCsrExtensions(req: PX509_REQ);
var
  exts: PX509_EXTENSIONS;
begin
  if (@self = nil) or
     (req = nil) then
    exit;
  exts := X509_REQ_get_extensions(req);
  if exts <> nil then
  try
    CopyExtensions(exts);
  finally
    exts.FreeX509_EXTENSION;
  end;
end;

function X509.SetBasic(ca: boolean; const altnames: RawUtf8;
  const subjectkey: RawUtf8): boolean;
begin
  result := SetExtension(NID_basic_constraints, _CA[ca]);
  if result and
     (subjectkey <> '') then // 'hash' by default on most certificates
    result  := SetExtension(NID_subject_key_identifier, subjectkey);
  if result and
     (altnames <> '') then
     result  := SetExtension(NID_subject_alt_name, altnames);
end;


function X509.SetUsage(usages: TX509Usages): boolean;
var
  v: RawUtf8;
begin
  result := false;
  if kuCA in usages then
    if not SetExtension(NID_basic_constraints, _CA[true]) then
      exit;
  v := KuText(usages);
  if v <> '' then
    if not SetExtension(NID_key_usage, Join(['critical', v])) then
      exit;
  v := XuText(usages);
  if v <> '' then
  begin
    SetLength(v, length(v) - 1); // trailing comma
    if not SetExtension(NID_ext_key_usage, v) then
      exit;
  end;
  result := true;
end;

function X509.MatchPrivateKey(pkey: PEVP_PKEY): boolean;
begin
  result := (@self <> nil) and
            (pkey <> nil) and
            (X509_check_private_key(@self, pkey) = OPENSSLSUCCESS);
end;

function X509.ToBinary: RawByteString;
begin
  result := BioSave(@self, @i2d_X509_bio);
end;

function X509.ToPem: RawUtf8;
begin
  result := BioSave(@self, @PEM_write_bio_X509, CP_UTF8);
end;

function X509.ToPkcs12(pkey: PEVP_PKEY; const password: SpiUtf8;
  CA: Pstack_st_X509; nid_key: integer; nid_cert: integer; iter: integer;
  mac_iter: integer; md_type: PEVP_MD; const FriendlyName: RawUtf8): RawByteString;
var
  p12: PPKCS12;
begin
  result := '';
  if (@self = nil) or
     (pkey = nil) then
    exit;
  p12 := NewPkcs12(password, pkey, @self,
           CA, nid_key, nid_cert, iter, mac_iter, FriendlyName);
  if p12 = nil then
    exit;
  if md_type <> nil then // non-default hasher: recompute the password digest
    // PKCS12_create() did PKCS12_set_mac(p12, pass, -1, NULL, 0, mac_iter, NULL))
    PKCS12_set_mac(p12, pointer(password), -1, nil, 0, mac_iter, md_type);
  result := p12.ToBinary;
  p12.Free;
end;

function X509.ToPkcs12(pkey: PEVP_PKEY; const password: SpiUtf8;
  format: TX509Pkcs12Format): RawByteString;
var
  nid, mac_iter: integer;
  md_type: PEVP_MD;
begin
  // warning: default algorithm changed to AES-256-CBC with OpenSSL 3
  // see https://github.com/openssl/openssl/commit/762970bd686c4aa
  nid := 0;
  mac_iter := 0;
  md_type := nil;
  case format of
    p12Legacy:
      // force legacy compatibility with Windows Server 2012 or MacOS/iOS
      // - warning: OpenSSL 1.x uses legacy 40bit-RC2 by default, which is sadly
      // incompatible with OpenSSL 3.x, so we force those safer (but still
      // downward compatible with Windows XP) parameters on all OpenSSL versions
      begin
        nid := NID_pbe_WithSHA1And3_Key_TripleDES_CBC; // old SHA1-3DES algo
        mac_iter := 1;
        md_type := EVP_sha1;
      end;
    p12New:
      // force OpenSSL 3.x new algorithm on OpenSSL 1.x (keep default otherwise)
      if OpenSslVersion < OPENSSL3_VERNUM then
      begin
        nid := NID_aes_256_cbc; // new AES-256-CBC safer algo
        mac_iter := PKCS12_DEFAULT_ITER;
        md_type := EVP_sha256;
      end;
  end;
  // perform the actual PCKS#12 binary export
  result := ToPkcs12(pkey, password, nil, nid, nid, 0, mac_iter, md_type);
end;

function X509.ToPkcs12Ex(pkey: PEVP_PKEY; const password: SpiUtf8): RawByteString;
var
  pwd: RawUtf8;
  fmt: TX509Pkcs12Format;
begin
  // retrieve the default global format
  fmt := OpenSslDefaultPkcs12;
  // allow to force a specific algorithm via a password prefix
  if password <> '' then
  begin
    pwd := password;
    if fmt <> p12PrefixDisabled then
      case PCardinal(pwd)^ of
        ord('3') + ord('d') shl 8 + ord('e') shl 16 + ord('s') shl 24:
          if pwd[5] = '=' then // start with PKCS12_3DES_PREFIX = '3des='
          begin
            delete(pwd, 1, 5); // trim
            fmt := p12Legacy;
          end;
        ord('a') + ord('e') shl 8 + ord('s') shl 16 + ord('=') shl 24:
          begin // start with PKCS12_AES_PREFIX = 'aes='
            delete(pwd, 1, 4);
            fmt := p12New;
          end;
      end;
  end;
  // perform the actual PCKS#12 binary export
  result := ToPkcs12(pkey, pwd, fmt);
  if pointer(pwd) <> pointer(password) then
    FillCharFast(pointer(pwd)^, length(pwd), 0); // anti-forensic
end;

function X509.Acquire: integer;
begin
  if @self = nil then
    result := -1
  else
    result := X509_up_ref(@self);
end;

function X509.Sign(pkey: PEVP_PKEY; md: PEVP_MD): integer;
begin
  if @self = nil then
    result := 0
  else
    result := X509_sign(@self, pkey, md);
end;

procedure X509.Free;
begin
  if @self <> nil then
    X509_free(@self);
end;


{ PKCS12 }

var
  VOID_STRING: AnsiChar; // initialized at #0 at startup

function PassNotNil(const PassWord: SpiUtf8): pointer;
begin
  result := pointer(Password);
  if result = nil then
    result := @VOID_STRING; // nil would trigger the OpenSSL callback
end;

function PKCS12.Extract(const password: SpiUtf8; privatekey: PPEVP_PKEY;
  cert: PPX509; ca: PPstack_st_X509): boolean;
begin
  result := (@self <> nil) and
    (PKCS12_parse(@self, PassNotNil(password), privatekey, cert, ca) = OPENSSLSUCCESS);
end;

function PKCS12.ToBinary: RawByteString;
begin
  result := BioSave(@self, @i2d_PKCS12_bio);
end;

procedure PKCS12.Free;
begin
  if @self <> nil then
    PKCS12_free(@self);
end;


{ EVP_PKEY }

function EVP_PKEY.PrivateToDer(const PassWord: SpiUtf8): RawByteString;
var
  bio: PBIO;
  res: integer;
begin
  result := '';
  if @self = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  if PassWord = '' then
    res := i2d_PrivateKey_bio(bio, @self)
  else
    res := i2d_PKCS8PrivateKey_bio(bio, @self, EVP_aes_256_cbc,
      pointer(PassWord), Length(PassWord), nil, nil);
  if res = OPENSSLSUCCESS then
    bio.ToString(result);
  bio.Free;
end;

function EVP_PKEY.PublicToDer: RawByteString;
begin
  result := BioSave(@self, @i2d_PUBKEY_bio);
end;

function EVP_PKEY.PrivateToPem(const PassWord: SpiUtf8): RawUtf8;
var
  bio: PBIO;
  res: integer;
begin
  result := '';
  if @self = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  if PassWord = '' then
    res := PEM_write_bio_PrivateKey(bio, @self, nil, nil, 0, nil, nil)
  else
    res := PEM_write_bio_PKCS8PrivateKey(bio, @self, EVP_aes_256_cbc,
      pointer(PassWord), Length(PassWord), nil, nil);
  if res = OPENSSLSUCCESS then
    result := bio.ToUtf8;
  bio.Free;
end;

function EVP_PKEY.PublicToPem: RawUtf8;
begin
  result := BioSave(@self, @PEM_write_bio_PUBKEY, CP_UTF8);
end;

procedure EVP_PKEY.ToPem(out PrivateKey, PublicKey: RawUtf8;
  const PrivateKeyPassWord: SpiUtf8);
begin
  if @self = nil then
    exit;
  PrivateKey := PrivateToPem(PrivateKeyPassWord);
  PublicKey := PublicToPem;
end;

function EVP_PKEY.Sign(Algo: PEVP_MD; Msg: pointer; Len: integer): RawByteString;
var
  ctx: PEVP_MD_CTX;
  s: PtrUInt;
begin
  // expects @self to be a private key
  // we don't check "if @self = nil" because may be called without EVP_PKEY
  result := ''; // '' on error
  ctx := EVP_MD_CTX_new;
  try
    // note: ED25519 requires single-pass EVP_DigestSign()
    s := 0;
    if (EVP_DigestSignInit(ctx, nil, Algo, nil, @self) = OPENSSLSUCCESS) and
       (EVP_DigestSign(ctx, nil, s, Msg, Len) = OPENSSLSUCCESS) then
    begin
      SetLength(result, s); // here size is maximum s bytes
      if EVP_DigestSign(ctx, pointer(result), s, Msg, Len) = OPENSSLSUCCESS then
      begin
        if s <> PtrUInt(length(result)) then
          SetLength(result, s); // result leading zeros may trim the size
      end
      else
        result := '';
    end;
  finally
    EVP_MD_CTX_free(ctx);
  end;
end;

function EVP_PKEY.Verify(Algo: PEVP_MD;
  Sig, Msg: pointer; SigLen, MsgLen: integer): boolean;
var
  ctx: PEVP_MD_CTX;
begin
  // expects @self to be a public (or private) key
  // we don't check "if @self = nil" because may be called without EVP_PKEY
  // we don't check "Algo = nil" because algo may have its built-in hashing
  ctx := EVP_MD_CTX_new;
  try
    // note: ED25519 requires single-pass EVP_DigestVerify()
    result :=
      (EVP_DigestVerifyInit(ctx, nil, Algo, nil, @self) = OPENSSLSUCCESS) and
      (EVP_DigestVerify(ctx, Sig, SigLen, Msg, MsgLen) = OPENSSLSUCCESS);
  finally
    EVP_MD_CTX_free(ctx);
  end;
end;

function EVP_PKEY.ToAltNames(const Subjects: TRawUtf8DynArray): RawUtf8;
var
  i: PtrInt;
  s: RawUtf8;
begin
  // self instance is not used
  result := '';
  for i := 0 to length(Subjects) - 1 do // in-place modified
  begin
    s := Subjects[i];
    if PosExChar(':', s) = 0 then
      s := Join(['DNS:', s]); // e.g. DNS: email: IP: URI:
    if result <> '' then
      result := Join([result, ',', s])
    else
      result := s;
  end;
end;

function EVP_PKEY.CreateSelfSignedCsr(Algo: PEVP_MD;
  const Subjects: TRawUtf8DynArray): RawByteString;
var
  req: PX509_REQ;
  names: PX509_NAME;
begin
  // same logic as in TCryptCertOpenSsl.Generate
  result := '';
  if (@self = nil) or
     (Subjects = nil) then
    exit;
  req := NewCertificateRequest;
  try
    names := X509_REQ_get_subject_name(req);
    names^.AddEntry('CN', Subjects[0]); // first subject is the X509 Common Name
    req.AddExtension(NID_subject_alt_name, ToAltNames(Subjects));
    if X509_REQ_set_pubkey(req, @self) = OPENSSLSUCCESS then
      if req.Sign(@self, Algo) <> 0 then // returns signature size in bytes
        result := req^.ToBinary;
  finally
    req.Free;
  end;
end;

function EVP_PKEY.Size: integer;
begin
  if @self <> nil then
    result := EVP_PKEY_size(@self)
  else
    result := 0;
end;

function EVP_PKEY.AlgoName: RawUtf8;
begin
  if @self <> nil then
    result := OBJ_nid2sn(EVP_PKEY_id(@self))
  else
    result := '';
end;

procedure EVP_PKEY.Free;
begin
  if @self <> nil then
    EVP_PKEY_free(@self);
end;

type
  // extra header for IV and plain text / key size storage
  // - should match the very same record definition in TRsa.Seal/Open
  // from mormot.crypt.rsa.pas, which is fully compatible with this unit
  TRsaSealHeader = packed record
    iv: THash128;
    plainlen: integer;
    encryptedkeylen: word; // typically 256 bytes for RSA-2048
    // followed by the encrypted key then the encrypted message
  end;
  PRsaSealHeader = ^TRsaSealHeader;

function EVP_PKEY.RsaSeal(Cipher: PEVP_CIPHER;
  const Msg: RawByteString): RawByteString;
var
  ctx: PEVP_CIPHER_CTX;
  pubk: PEVP_PKEY;
  ek: RawByteString;
  ekl, lu, lf, msgpos: integer;
  p: PAnsiChar;
  head: TRsaSealHeader;
begin
  // expects @self to be a public key
  // must be RSA because it is the only OpenSSL algorithm featuring key transport
  result := '';
  // validate input parameters
  head.plainlen := length(Msg);
  if (@self = nil) or
     (head.plainlen = 0) or
     (head.plainlen > 128 shl 20) or // fair limitation for in-memory encryption
     (Cipher = nil) then
    exit;
  // generate the ephemeral secret key and IV within the corresponding header
  // and encrypt this ephemeral secret using the current RSA public key
  ctx := EVP_CIPHER_CTX_new;
  if ctx = nil then
    exit;
  pubk := @self;
  SetLength(ek, EVP_PKEY_size(@self));
  if EVP_SealInit(ctx, Cipher, @ek, @ekl, @head.iv, @pubk, 1) = OPENSSLSUCCESS then
  begin
    head.encryptedkeylen := ekl;
    msgpos := SizeOf(head) + ekl;
    pointer(result) := FastNewString(msgpos + head.plainlen + 16);
    // encrypt the message
    if EVP_EncryptUpdate(ctx, @PByteArray(result)[msgpos], @lu,
         pointer(Msg), head.plainlen) = OPENSSLSUCCESS then
    begin
      // concatenate the header, encrypted key and message
      PRsaSealHeader(result)^ := head;
      MoveFast(pointer(ek)^, PByteArray(result)[SizeOf(head)], ekl);
      p := @PByteArray(result)[msgpos + lu];
      if EVP_SealFinal(ctx, pointer(p), @lf) = OPENSSLSUCCESS then
        FakeLength(result, p + lf - pointer(result))
      else
        result := '';
    end
    else
      result := '';
  end;
  EVP_CIPHER_CTX_free(ctx);
end;

{ for reference, some python reference code with OpenSSL 3.x:
  crt = x509.load_pem_x509_certificate(pem, default_backend())
  rsa = crt.public_key()
  apadding = padding.PKCS1v15()
  aes_key = os.urandom(16)
  aes_iv = os.urandom(16)
  cipher = Cipher(algorithms.AES(aes_key), modes.CTR(aes_iv), backend=default_backend())
  encryptor = cipher.encryptor()
  encrypted_data = encryptor.update(content) + encryptor.finalize()
  encrypted_aes_key = rsa.encrypt(aes_key, apadding)
  header = TRsaSealHeader(iv=aes_iv, plainlen=len(content), encryptedkeylen=len(encrypted_aes_key))
  final_message = header.pack() + encrypted_aes_key + encrypted_data
}

function EVP_PKEY.RsaOpen(Cipher: PEVP_CIPHER;
  const Msg: RawByteString; CodePage: integer): RawByteString;
var
  ctx: PEVP_CIPHER_CTX;
  msgpos, lu, lf, lm: integer;
  head: PRsaSealHeader absolute Msg;
  input: PByteArray absolute Msg;
begin
  // expects @self to be a private key
  result := '';
  // decode and validate the header
  lm := length(Msg);
  if (@self = nil) or
     (Cipher = nil) or
     (lm < SizeOf(head^)) or
     (head^.plainlen <= 0) or
     (head^.plainlen > 128 shl 20) then
    exit;
  msgpos := SizeOf(head^) + head^.encryptedkeylen;
  if lm < msgpos + head^.plainlen then
    exit; // avoid buffer overflow on malformatted/forged input
  // decrypt the ephemeral key, then the message
  ctx := EVP_CIPHER_CTX_new;
  if ctx = nil then
    exit;
  if EVP_OpenInit(ctx, Cipher, @input[SizeOf(head^)],
       head.encryptedkeylen, @head.iv, @self) = OPENSSLSUCCESS then
  begin
    FastSetStringCP(result, nil, head^.plainlen, CodePage);
    if (EVP_DecryptUpdate(ctx,
         pointer(result), @lu, @input[msgpos], lm - msgpos) <> OPENSSLSUCCESS) or
       (EVP_OpenFinal(ctx,
         @PByteArray(result)[{%H-}lu], @lf) <> OPENSSLSUCCESS) or
       (lu + {%H-}lf <> head^.plainlen) then
      result:= '';
  end;
  EVP_CIPHER_CTX_free(ctx);
end;

procedure RsaSetPadding(ctx: PEVP_PKEY_CTX; md: PEVP_MD);
begin
  EOpenSsl.Check(
    EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_PKCS1_OAEP_PADDING),
    'EVP_PKEY_CTX_set_rsa_padding');
  if OpenSslVersion < OPENSSL3_VERNUM then
  begin
    EOpenSsl.Check(
      EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, md), 'EVP_PKEY_CTX_set_rsa_mgf1_md');
    EOpenSsl.Check(
      EVP_PKEY_CTX_set_rsa_oaep_md(ctx, md), 'EVP_PKEY_CTX_set_rsa_oaep_md');
  end;
end;

function EVP_PKEY.RsaEncrypt(const Content: RawByteString; MD: PEVP_MD): RawByteString;
var
  ctx: PEVP_PKEY_CTX;
  len: PtrUInt;
begin
  // to be used for a very small content since this may be very slow
  result := '';
  if @self = nil then
    exit;
  ctx := EVP_PKEY_CTX_new(@self, nil);
  if Assigned(ctx) then
  try
    EOpenSsl.Check(
      EVP_PKEY_encrypt_init(ctx), 'EVP_PKEY_encrypt_init');
    RsaSetPadding(ctx, MD);
    EOpenSsl.Check( // first call to retrieve the maximum output length
      EVP_PKEY_encrypt(ctx, nil, len, pointer(Content), Length(Content)),
      'EVP_PKEY_encrypt1');
    pointer(result) := FastNewString(len); // allocate
    EOpenSsl.Check( // second call to make the actual encryption
      EVP_PKEY_encrypt(ctx, pointer(result), len, pointer(Content), Length(Content)),
      'EVP_PKEY_encrypt2');
    FakeSetLength(result, len); // previous len was the max, now is final size
  finally
    EVP_PKEY_CTX_free(ctx);
  end;
end;

function EVP_PKEY.RsaDecrypt(const Content: RawByteString; MD: PEVP_MD;
  CodePage: integer): RawByteString;
var
  ctx: PEVP_PKEY_CTX;
  len: PtrUInt;
begin
  result := '';
  if @self = nil then
    exit;
  ctx := EVP_PKEY_CTX_new(@self, nil);
  if Assigned(ctx) then
  try
    EOpenSsl.Check(
      EVP_PKEY_decrypt_init(ctx), 'EVP_PKEY_decrypt_init');
    RsaSetPadding(ctx, MD);
    EOpenSsl.Check(
      EVP_PKEY_decrypt(ctx, nil, len, pointer(Content), Length(Content)),
      'EVP_PKEY_decrypt1');
    FastSetStringCP(result, nil, len, CodePage);
    EOpenSsl.Check(
      EVP_PKEY_decrypt(ctx, pointer(result), len, pointer(Content), Length(Content)),
      'EVP_PKEY_decrypt2');
    FakeSetLength(result, len);
  finally
    EVP_PKEY_CTX_free(ctx);
  end;
end;

procedure EVP_PKEY.RsaGetPubKey(out e, n: RawByteString);
var
  n_num, e_num: PBIGNUM;
begin
  if @self = nil then
    exit;
  RSA_get0_key(EVP_PKEY_get0_RSA(@self), @n_num, @e_num, nil);
  e_num.ToBin(e);
  n_num.ToBin(n);
end;

procedure EVP_PKEY.EccGetPubKeyCompressed(out k: RawByteString);
var
  pub: PByte;
  publen: integer;
begin
  if @self = nil then
    exit;
  pub := nil;
  publen := EC_KEY_key2buf(EVP_PKEY_get0_EC_KEY(@self),
    POINT_CONVERSION_COMPRESSED, @pub, nil);
  FastSetRawByteString(k, pub, publen);
  OPENSSL_free(pub);
end;

procedure EVP_PKEY.EccGetPubKeyUncompressed(out x, y: RawByteString);
var
  pub: PAnsiChar;
  publen: integer;
begin
  if @self = nil then
    exit;
  pub := nil;
  publen := EC_KEY_key2buf(EVP_PKEY_get0_EC_KEY(@self),
    POINT_CONVERSION_UNCOMPRESSED, @pub, nil);
  if publen = 0 then
    exit;
  publen := (publen - 1) shr 1; // skip first byte, it's key compression marker
  FastSetRawByteString(x, pub + 1, publen);
  FastSetRawByteString(y, pub + publen + 1, publen);
  OPENSSL_free(pub);
end;


{ ******************** OpenSSL Helpers }

{ TX509Cache }

procedure TX509Cache.Cache(const pem: RawUtf8; out res: PX509DynArray);
begin
  if pem = '' then
    exit;
  if fPem = '' then
    fSafe.Init; // may be needed if was allocated on stack
  fSafe.Lock;
  try
    if fPem <> pem then
    begin
      fPem := pem;
      PX509DynArrayFree(fX509);
      fX509 := LoadCertificates(pem);
    end;
    res := fX509;
  finally
    fSafe.UnLock;
  end;
end;

procedure TX509Cache.Done;
begin
  if fX509 <> nil then
    PX509DynArrayFree(fX509);
end;


procedure OpenSSL_Free(ptr: pointer);
begin
  CRYPTO_free(ptr, 'mormot', 0);
end;

function OpenSSL_error(error: integer): RawUtf8;
begin
  OpenSSL_error(error, result);
end;

procedure OpenSSL_error(error: integer; var result: RawUtf8);
var
  tmp: TBuffer1K;
begin
  result := '';
  if error = 0 then // no error in the queue
    exit;
  ERR_error_string_n(error, @tmp, SizeOf(tmp));
  FastSetString(result, @tmp, mormot.core.base.StrLen(@tmp));
end;

function OpenSSL_error_short(error: integer): ShortString;
begin
  result[0] := #0;
  if error = 0 then // no error in the queue
    exit;
  ERR_error_string_n(error, @result[1], 254);
  result[0] := AnsiChar(mormot.core.base.StrLen(@result[1]));
end;

// see https://www.openssl.org/docs/man1.1.1/man3/SSL_get_error.html
function SSL_is_fatal_error(get_error: integer): boolean;
begin
  case get_error of
    SSL_ERROR_NONE,
    SSL_ERROR_WANT_READ,
    SSL_ERROR_WANT_WRITE,
    SSL_ERROR_WANT_CONNECT,
    SSL_ERROR_WANT_ACCEPT:
      result := false;
  else
    result := true;
  end;
end;

const
  // documented errors constants names after SSL_*() functions failure
  SSL_ERROR_TEXT: array[SSL_ERROR_NONE .. SSL_ERROR_WANT_CLIENT_HELLO_CB] of TShort23 = (
    'NONE',
    'SSL',
    'WANT_READ',
    'WANT_WRITE',
    'WANT_X509_LOOKUP',
    'SYSCALL',
    'ZERO_RETURN',
    'WANT_CONNECT',
    'WANT_ACCEPT',
    'WANT_ASYNC',
    'WANT_ASYNC_JOB',
    'WANT_CLIENT_HELLO_CB');

procedure SSL_get_error_short(get_error: integer; var dest: shortstring);
begin
  dest := 'SSL_ERROR_';
  if get_error in [low(SSL_ERROR_TEXT) .. high(SSL_ERROR_TEXT)] then
  begin
    AppendShort(SSL_ERROR_TEXT[get_error], dest);
    case get_error of
      SSL_ERROR_SSL:
        // non-recoverable protocol error
        begin
          get_error := ERR_get_error;
          if get_error <> 0 then
          begin
            AppendShortTwoChars(ord(' ') + ord('(') shl 8, @dest);
            AppendShort(OpenSSL_error_short(get_error), dest);
            AppendShortChar(')', @dest)
          end;
        end;
      SSL_ERROR_SYSCALL:
        begin
          // non-recoverable I/O error
          get_error := RawSocketErrNo; // try to get additional info from OS
          if get_error <> NO_ERROR then
            OsErrorAppend(get_error, dest, ' ');
        end;
    end; // non-fatal SSL_ERROR_WANT_* codes are unexpected here
  end
  else
    AppendShortCardinal(get_error, dest); // paranoid / undocumented
end;

procedure SSL_get_error_text(get_error: integer; var result: RawUtf8);
var
  tmp: ShortString;
begin
  SSL_get_error_short(get_error, tmp);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function SSL_get_ex_new_index(l: integer; p: pointer; newf: PCRYPTO_EX_new;
  dupf: PCRYPTO_EX_dup; freef: PCRYPTO_EX_free): integer;
begin
  result := CRYPTO_get_ex_new_index(CRYPTO_EX_INDEX_SSL, l, p, newf, dupf, freef);
end;

function Digest(md: PEVP_MD; buf: pointer; buflen: integer;
  out dig: THash512): integer;
var
  ctx: PEVP_MD_CTX;
begin
  result := 0;
  if (buf = nil) or
     (buflen < 0) then
    exit;
  ctx := EVP_MD_CTX_new;
  if ctx = nil then
    exit;
  if md = nil then
    md := EVP_sha1; // SHA-1 fingerprint by default
  if (EVP_DigestInit_ex(ctx, md, nil) = OPENSSLSUCCESS) and
     (EVP_DigestUpdate(ctx, buf, buflen) = OPENSSLSUCCESS) and
     (EVP_DigestFinal_ex(ctx, @dig, nil) = OPENSSLSUCCESS) then
    result := EVP_MD_size(md);
  EVP_MD_CTX_free(ctx);
end;

function Digest(md: PEVP_MD; buf: pointer; buflen: integer): RawUtf8;
var
  dig: THash512;
begin
  result := MacToHex(@dig, Digest(md, buf, buflen, dig));
end;

function Digest(md: PEVP_MD; const buf: RawByteString): RawUtf8;
begin
  result := Digest(md, pointer(buf), length(buf));
end;

function SSL_CTX_set_session_cache_mode(ctx: PSSL_CTX; mode: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_SESS_CACHE_MODE, mode, nil);
end;

function SSL_CTX_add_extra_chain_cert(ctx: PSSL_CTX; cert: PX509): cardinal;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_EXTRA_CHAIN_CERT, 0, cert);
end;

function SSL_CTX_set_tmp_dh(ctx: PSSL_CTX; dh: pointer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TMP_DH, 0, dh);
end;

function SSL_CTX_set_tmp_ecdh(ctx: PSSL_CTX; ecdh: pointer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TMP_ECDH, 0, ecdh);
end;

function SSL_CTX_set_ecdh_auto(ctx: PSSL_CTX; onoff: integer): integer;
begin
  result := 1;
end;

function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, version, nil);
end;

function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MAX_PROTO_VERSION, version, nil);
end;

function SSL_set_tlsext_host_name(const s: PSSL; const name: RawUtf8): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_SET_TLSEXT_HOSTNAME,
    TLSEXT_NAMETYPE_host_name, pointer(name));
end;

function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: SSL_SNI_servername_cb): integer;
begin
  result := SSL_CTX_callback_ctrl(
    ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, SSL_CTX_callback_ctrl_(cb));
end;

function SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: pointer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, arg);
end;

function SSL_set_mode(s: PSSL; version: integer): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_MODE, version, nil);
end;

function SSL_get_mode(s: PSSL): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_MODE, 0, nil);
end;

function EVP_MD_CTX_size(ctx: PEVP_MD_CTX): integer;
begin
  result := EVP_MD_size(EVP_MD_CTX_md(ctx))
end;

function BN_num_bytes(bn: PBIGNUM): integer;
begin
  result := (BN_num_bits(bn) + 7) shr 3;
end;

function BigNumFromDecimal(const Text: RawUtf8): PBIGNUM;
begin
  result := nil;
  if BN_dec2bn(@result, pointer(Text)) = 0 then
    result := nil;
end;

function BigNumHexFromDecimal(const Text: RawUtf8): RawUtf8;
var
  bn: PBIGNUM;
begin
  bn := BigNumFromDecimal(Text);
  result := bn^.ToHex;
  bn^.Free;
end;

function EVP_PKEY_CTX_set_rsa_padding(ctx: PEVP_PKEY_CTX; padding: integer): integer;
begin
  result := {$ifndef OPENSSLSTATIC}libcrypto.{$endif}RSA_pkey_ctx_ctrl(
    ctx, -1, EVP_PKEY_CTRL_RSA_PADDING, padding, nil);
end;

function EVP_PKEY_CTX_set_rsa_mgf1_md(ctx: PEVP_PKEY_CTX; md: PEVP_MD): integer;
begin
  result := RSA_pkey_ctx_ctrl(ctx,
    EVP_PKEY_OP_TYPE_SIG or EVP_PKEY_OP_TYPE_CRYPT,
    EVP_PKEY_CTRL_RSA_MGF1_MD, 0, md);
end;

function EVP_PKEY_CTX_set_rsa_oaep_md(ctx: PEVP_PKEY_CTX; md: PEVP_MD): integer;
begin
  result := EVP_PKEY_CTX_ctrl(ctx,
    EVP_PKEY_RSA, EVP_PKEY_OP_TYPE_CRYPT, EVP_PKEY_CTRL_RSA_OAEP_MD, 0, md);
end;

function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
begin
  result := SSL_ctrl(s, DTLS_CTRL_GET_TIMEOUT, 0, timeval);
end;

procedure DTLSv1_handle_timeout(s: PSSL);
begin
  SSL_ctrl(s, DTLS_CTRL_HANDLE_TIMEOUT, 0, nil);
end;

function TmToDateTime(const t: tm): TDateTime;
begin
  result := EncodeDate(t.tm_year + 1900, t.tm_mon + 1, t.tm_mday) +
            EncodeTime(t.tm_hour, t.tm_min, t.tm_sec, 0);
end;

function NewOpenSslStack: POPENSSL_STACK;
begin
  result := OPENSSL_sk_new(nil);
end;

function LoadCsr(const Der: RawByteString): PX509_REQ;
begin
  result := BioLoad(Der, @d2i_X509_REQ_bio);
end;

function LoadPrivateKey(PrivateKey: pointer; PrivateKeyLen: integer;
  const Password: SpiUtf8; Pkcs12Cert: PPX509): PEVP_PKEY;
var
  pw: pointer;
  priv: PBIO;
  pkcs12: PPKCS12;
begin
  if (PrivateKey = nil) or
     (PrivateKeyLen = 0) then
    result := nil
  else
  begin
    pw := PassNotNil(Password);
    priv := BIO_new_mem_buf(PrivateKey, PrivateKeyLen);
    if NetIsPem(PrivateKey) then
      result := PEM_read_bio_PrivateKey(priv, nil, nil, pw)
    else
      result := nil;
    if result = nil then
    begin
      if Password = '' then
      begin
        priv.Reset;
        result := d2i_PrivateKey_bio(priv, nil); // try raw binary format
      end;
      if result = nil then
      begin
        priv.Reset;
        result := d2i_PKCS8PrivateKey_bio(priv, nil, nil, pw); // try PKCS#8
      end;
      if result = nil then
      begin
        priv.Reset;
        pkcs12 := d2i_PKCS12_bio(priv, nil); // try PKCS#12
        pkcs12.Extract(Password, @result, Pkcs12Cert, nil); // ignore CA
        pkcs12.Free;
      end;
    end;
    priv.Free;
  end;
end;

function LoadPrivateKey(const Saved: RawByteString;
  const Password: SpiUtf8; Pkcs12Cert: PPX509): PEVP_PKEY;
begin
  if Saved <> '' then
    result := LoadPrivateKey(pointer(Saved), length(Saved), Password, Pkcs12Cert)
  else
    result := nil;
end;

function LoadPublicKey(PublicKey: pointer; PublicKeyLen: integer;
  const Password: SpiUtf8): PEVP_PKEY;
var
  pub: PBIO;
begin
  if (PublicKey = nil) or
     (PublicKeyLen = 0) then
    result := nil
  else
  begin
    pub := BIO_new_mem_buf(PublicKey, PublicKeyLen);
    if NetIsPem(PublicKey) then
      result := PEM_read_bio_PUBKEY(pub, nil, nil, PassNotNil(Password))
    else
      result := nil;
    if (result = nil) and
       (Password = '') then
    begin
      pub.reset;
      result := d2i_PUBKEY_bio(pub, nil); // try raw binary format
    end;
    pub.Free;
  end;
end;

function LoadPublicKey(const Saved: RawByteString;
  const Password: SpiUtf8): PEVP_PKEY;
begin
  if Saved <> '' then
    result := LoadPublicKey(pointer(Saved), length(Saved), Password)
  else
    result := nil;
end;

const
  X509v3 = 2;    // X509_VERSION_3 has value 2 and X509_VERSION_1 has value 0 ;)
  X509reqv1 = 0; // version number should be 0 as stated by RFC 2986
  X509crlv2 = 1; // version number should be 1 as stated by RFC 5280

function NewCertificate: PX509;
var
  rnd: THash160;
  bn: PBIGNUM;
  ai: PASN1_INTEGER;
  x: PX509;
begin
  EOpenSsl.CheckAvailable(nil, 'NewCertificate');
  result := nil;
  x := X509_new();
  EOpenSsl.Check(X509_set_version(x, X509v3));
  // compute a 160-bit random serial from OpenSSL PRNG
  RAND_bytes(@rnd, SizeOf(rnd));
  rnd[0] := rnd[0] and $7f; // ensure > 0
  bn := BN_bin2bn(@rnd, 20, nil);
  ai := BN_to_ASN1_INTEGER(bn, nil);
  if X509_set_serialNumber(x, ai) = OPENSSLSUCCESS then
    result := x;
  ai.Free;
  bn.Free;
  if result = nil then
    x.Free;
end;

function LoadCertificate(const DerOrPem: RawByteString): PX509;
var
  x: PX509DynArray;
begin
  result := nil;
  if NetIsPem(pointer(DerOrPem)) then
  begin
    x := LoadCertificates(DerOrPem, {max=}1); // read first PEM
    if x <> nil then
      result := x[0];
  end
  else
    result := BioLoad(DerOrPem, @d2i_X509_bio); // DER binary format
end;

function LoadCertificates(const Pem: RawUtf8; Max: integer): PX509DynArray;
var
  bio: PBIO;
  x: PX509;
  n: integer;
begin
  result := nil;
  if Pem = '' then
    exit;
  bio := BIO_new_mem_buf(pointer(Pem), length(Pem));
  n := 0;
  repeat
    x := PEM_read_bio_X509_AUX(bio, nil, nil, nil);
    if x = nil then
      break;
    PtrArrayAdd(result, x, n);
  until (Max <> 0) and (n >= Max);
  if n <> 0 then
    DynArrayFakeLength(result, n);
  bio.Free;
end;

var
  _lasts: TX509Cache;
  _last: array[TSystemCertificateStore] of TX509Cache;

function LoadCertificatesFromSystemStore(CertStores: TSystemCertificateStores;
  FlushCache, OnlySystemStore: boolean): PX509DynArray;
begin
  _lasts.Cache(GetSystemStoreAsPem(CertStores, FlushCache, OnlySystemStore), result);
end;

function LoadCertificatesFromOneSystemStore(CertStore: TSystemCertificateStore;
  FlushCache: boolean): PX509DynArray;
begin
  _last[CertStore].Cache(GetOneSystemStoreAsPem(CertStore, FlushCache), result);
end;

function NewCertificateStore: PX509_STORE;
begin
  EOpenSsl.CheckAvailable(nil, 'NewCertificateStore');
  result := X509_STORE_new();
end;

function NewCertificateStoreCtx(store: PX509_STORE; x509: PX509;
  chain: Pstack_st_X509; callback: X509_STORE_CTX_verify_cb): PX509_STORE_CTX;
begin
  result := X509_STORE_CTX_new();
  if result = nil then
    exit;
  if X509_STORE_CTX_init(result, store, x509, chain) <> OPENSSLSUCCESS then
  begin
    result.Free;
    result := nil;
    exit;
  end;
  if Assigned(callback) then
    X509_STORE_CTX_set_verify_cb(result, callback);
end;

function NewCertificateCrl: PX509_CRL;
begin
  EOpenSsl.CheckAvailable(nil, 'NewCertificateCrl');
  result := X509_CRL_new();
  EOpenSsl.Check(X509_CRL_set_version(result, X509crlv2));
end;

function LoadCertificateCrl(const Der: RawByteString): PX509_CRL;
begin
  result := BioLoad(Der, @d2i_X509_CRL_bio);
end;

function LoadCertificateCrlFromPem(const Pem: RawUtf8): PX509_CRL;
var
  bio: PBIO;
begin
  result := nil;
  if Pem = '' then
    exit;
  bio := BIO_new_mem_buf(pointer(Pem), length(Pem));
  result := PEM_read_bio_X509_CRL(bio, nil, nil, nil);
  bio.Free;
end;

function NewCertificateRequest: PX509_REQ;
begin
  EOpenSsl.CheckAvailable(nil, 'NewCertificateRequest');
  result := X509_REQ_new;
  EOpenSsl.Check(X509_REQ_set_version(result, X509reqv1), 'X509_REQ_set_version');
end;

function LoadCertificateRequest(const Der: RawByteString): PX509_REQ;
begin
  result := BioLoad(Der, @d2i_X509_REQ_bio);
end;

function NewPkcs12(const Password: SpiUtf8; PrivKey: PEVP_PKEY;
  Cert: PX509; CA: Pstack_st_X509; nid_key, nid_cert, iter, mac_iter: integer;
  const FriendlyName: RawUtf8): PPKCS12;
begin
  EOpenSsl.CheckAvailable(nil, 'NewPkcs12');
  if not Cert.MatchPrivateKey(PrivKey) then
    raise EOpenSsl.Create('NewPkcs12: PrivKey does not match Cert');
  result := PKCS12_create(pointer(Password), pointer(FriendlyName),
    PrivKey, Cert, CA, nid_key, nid_cert, iter, mac_iter, 0);
end;

function LoadPkcs12(const Der: RawByteString): PPKCS12;
begin
  result := BioLoad(Der, @d2i_PKCS12_bio);
end;

function ParsePkcs12(const Saved: RawByteString; const Password: SpiUtf8;
  out Cert: PX509; out PrivateKey: PEVP_PKEY; CA: PPstack_st_X509): boolean;
var
  pkcs12: PPKCS12;
begin
  Cert := nil;
  PrivateKey := nil;
  pkcs12 := LoadPkcs12(Saved);
  result := pkcs12.Extract(Password, @PrivateKey, @Cert, CA);
  pkcs12.Free;
end;

function OpenSslSCrypt(const Password: RawUtf8; const Salt: RawByteString;
  N, R, P, DestLen: PtrUInt): RawByteString;
var
  ctx: PEVP_PKEY_CTX;
  len: PtrUInt;
begin
  result := '';
  // validate parameters
  if (DestLen < 16) or
     (N <= 1) or
     (N >= PtrUInt(1 shl 31)) or
     (not IsPowerOfTwo(N)) or  // must be a power of 2 greater than 1
     (R = 0) or                // R = blocksize
     (P = 0) or                // P = parallel
     (QWord(R) * QWord(N) * 128 >= 1 shl 30) or // consume up to 1GB of RAM
     (R * P >= 1 shl 30) then                   // must satisfy r * p < 2^30
    exit;
  ctx := EVP_PKEY_CTX_new_id(EVP_PKEY_SCRYPT, nil);
  if ctx <> nil then
  try
    // setup parameters
    if (EVP_PKEY_derive_init(ctx) <= 0) or
       (EVP_PKEY_CTX_set1_pbe_pass(ctx, pointer(Password), Length(Password)) <= 0) or
       (EVP_PKEY_CTX_set1_scrypt_salt(ctx, pointer(Salt), length(Salt)) <= 0) or
       (EVP_PKEY_CTX_set_scrypt_N(ctx, N) <= 0) or
       (EVP_PKEY_CTX_set_scrypt_r(ctx, R) <= 0) or
       (EVP_PKEY_CTX_set_scrypt_p(ctx, P) <= 0) then
      exit;
   // derive key
   len := DestLen;
   if (EVP_PKEY_derive(ctx, FastNewRawByteString(result, len), @len) <= 0) or
      (len <> DestLen) then
     result := '';
  finally
    EVP_PKEY_CTX_free(ctx);
  end;
end;

function PX509DynArrayToPem(const X509: PX509DynArray): RawUtf8;
var
  bio: PBIO;
  i: PtrInt;
begin
  result := '';
  if X509 = nil then
    exit;
  bio := BIO_new(BIO_s_mem);
  for i := 0 to length(X509) - 1 do
    if X509[i] <> nil then
      PEM_write_bio_X509(bio, X509[i]);
  result := bio.ToUtf8;
  bio.Free;
end;

function PX509DynArrayToText(const X509: PX509DynArray): RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  for i := 0 to length(X509) - 1 do
    result := Join([result, X509[i].PeerInfo, '---------'#13#10]);
end;

procedure PX509DynArrayFree(var X509: PX509DynArray);
var
  i: PtrInt;
begin
  for i := 0 to length(X509) - 1 do
    X509[i].Free;
  X509 := nil;
end;


{ ************** TLS / HTTPS Encryption Layer using OpenSSL for TCrtSocket }

{ TOpenSslNetTls }

type
  /// OpenSSL TLS layer communication
  TOpenSslNetTls = class(TInterfacedObject, INetTls)
  private
    fSocket: TNetSocket;
    fContext: PNetTlsContext; // the client-side TCrtSocket context
    fLastError: PRawUtf8;
    fCtx: PSSL_CTX;
    fSsl: PSSL;
    fPeer: PX509;
    fCipherName, fServerAddress: RawUtf8;
    fDoSslShutdown: boolean;
    procedure Check(const method: ShortString; res: integer);
      {$ifdef HASINLINE} inline; {$endif}
    function CheckSsl(res: integer): TNetResult;
    procedure SetupCtx(var Context: TNetTlsContext; Bind: boolean);
  public
    destructor Destroy; override;
    // INetTls methods
    procedure AfterConnection(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    procedure AfterBind(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    procedure AfterAccept(Socket: TNetSocket; const BoundContext: TNetTlsContext;
      LastError, CipherName: PRawUtf8);
    function GetCipherName: RawUtf8;
    function GetRawTls: pointer;
    function GetRawCert(SignHashName: PRawUtf8): RawByteString;
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    function ReceivePending: integer;
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;

threadvar // do not publish for compilation within Delphi packages
  _PeerVerify: TOpenSslNetTls; // OpenSSL is a dumb library for sure

function AfterConnectionPeerVerify(
  wasok: integer; store: PX509_STORE_CTX): integer; cdecl;
var
  peer: PX509;
  c: TOpenSslNetTls;
begin
  peer := X509_STORE_CTX_get_current_cert(store);
  c := _PeerVerify;
  c.fContext.PeerIssuer := peer.IssuerName;
  c.fContext.PeerSubject := peer.SubjectName;
  c.fContext.PeerCert := peer;
  try
    result := ord(c.fContext.OnEachPeerVerify(
      c.fSocket, c.fContext, wasok <> 0, c.fSsl, peer));
  except
    result := 0; // abort the connection on exception within callback
  end;
end;

function AfterConnectionAskPassword(buf: PUtf8Char; size, rwflag: integer;
  userdata: pointer): integer; cdecl;
var
  c: TOpenSslNetTls;
  pwd: RawUtf8;
begin
  c := _PeerVerify;
  try
    pwd := c.fContext.OnPrivatePassword(c.fSocket, c.fContext, c.fSsl);
    result := length(pwd);
    if result <> 0 then
      if size > result  then
        MoveByOne(pointer(pwd), buf, result + 1) // +1 to include #0 terminator
      else
        result := 0; // buf[0..size-1] is too small for this password -> abort
  except
    result := 0; // abort on exception within callback
  end;
end;

function GetNextCsv(var P: PUtf8Char; var value: RawUtf8): boolean;
var
  S: PUtf8Char;
begin
  if P = nil then
    result := false
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> ',') do
      inc(S);
    FastSetString(value, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
    result := true;
  end;
end;

procedure TOpenSslNetTls.Check(const method: ShortString; res: integer);
begin
  if res <> OPENSSLSUCCESS then
    EOpenSslNetTls.CheckFailed(self, method, fLastError, fSsl, res, fServerAddress);
end;

const
  // list taken on 2021-02-19 from https://ssl-config.mozilla.org/
  SAFE_CIPHERLIST: array[ {hwaes=} boolean ] of PUtf8Char = (
    // without AES acceleration: prefer CHACHA20-POLY1305
    'ECDHE-ECDSA-CHACHA20-POLY1305:' +
    'ECDHE-RSA-CHACHA20-POLY1305:' +
    'ECDHE-ECDSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-ECDSA-AES256-GCM-SHA384:' +
    'ECDHE-RSA-AES256-GCM-SHA384:' +
    'DHE-RSA-AES128-GCM-SHA256:' +
    'DHE-RSA-AES256-GCM-SHA384',
    // with AES acceleration
    'ECDHE-ECDSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-ECDSA-AES256-GCM-SHA384:' +
    'ECDHE-RSA-AES256-GCM-SHA384:' +
    'ECDHE-ECDSA-CHACHA20-POLY1305:' +
    'ECDHE-RSA-CHACHA20-POLY1305:' +
    'DHE-RSA-AES128-GCM-SHA256:' +
    'DHE-RSA-AES256-GCM-SHA384');

// see https://www.ibm.com/support/knowledgecenter/SSB23S_1.1.0.2020/gtps7/s5sple2.html

procedure TOpenSslNetTls.AfterConnection(Socket: TNetSocket;
  var Context: TNetTlsContext; const ServerAddress: RawUtf8);
var
  P: PUtf8Char;
  h: RawUtf8;
  peer: PPointer;
  //x: PX509DynArray;
  //ext: TX509_Extensions; exts: TRawUtf8DynArray; len: PtrInt;
begin
  fSocket := Socket;
  fContext := @Context;
  // reset output information
  ResetNetTlsContext(Context);
  fLastError := @Context.LastError;
  fServerAddress := ServerAddress;
  peer := @_PeerVerify;
  // prepare TLS connection properties
  fCtx := SSL_CTX_new(TLS_client_method);
  try
    peer^ := self;
    SetupCtx(Context, {bind=}false);
    fSsl := SSL_new(fCtx);
    SSL_set_tlsext_host_name(fSsl, ServerAddress); // SNI field
    if not Context.IgnoreCertificateErrors then
    begin
      P := pointer(Context.HostNamesCsv);
      if GetNextCsv(P, h) then
      begin
        SSL_set_hostflags(fSsl, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
        Check('AfterConnection set1_host', SSL_set1_host(fSsl, pointer(h)));
        while GetNextCsv(P, h) do
          Check('AfterConnection add1_host', SSL_add1_host(fSsl, pointer(h)));
      end;
    end;
    Check('AfterConnection set_fd', SSL_set_fd(fSsl, Socket.Socket));
    // client TLS negotiation with server
    Check('AfterConnection connect', SSL_connect(fSsl));
    fDoSslShutdown := true; // need explicit SSL_shutdown() at closing
    Context.CipherName := GetCipherName;
    // writeln(Context.CipherName);
    // peer validation
    if Assigned(Context.OnPeerValidate) then
      // via a global custom callback
      Context.OnPeerValidate(Socket, fContext, fSsl)
    else
    begin
      // get OpenSSL peer certificate information
      fPeer := fSsl.PeerCertificate;
      // writeln(fSsl.PeerCertificatesAsPEM);
      // writeln(fSsl.PeerCertificatesAsText);
      //x := LoadCertificates(fSsl.PeerCertificatesAsPEM);
      //writeln(PX509DynArrayToPem(x));
      //PX509DynArrayFree(x);
      if (fPeer = nil) and
         not Context.IgnoreCertificateErrors then
        Check('AfterConnection get_peer_certificate', 0);
      try
        if fPeer <> nil then
        begin
          // writeln(fPeer.SetExtension(NID_netscape_comment, 'toto est le plus bo'));
          // writeln(fPeer.SetUsage([kuCodeSign, kuDigitalSignature, kuTlsServer, kuTlsClient]));
          Context.PeerIssuer := fPeer.IssuerName;
          Context.PeerSubject := fPeer.SubjectName;
          Context.PeerCert := fPeer;
          if Context.WithPeerInfo or
             (not Context.IgnoreCertificateErrors and
              not fSsl.IsVerified(@Context.LastError)) then
            // include full peer info on certificate verification failure
            Context.PeerInfo := fPeer.PeerInfo;
          {
          writeln(#10'------------'#10#10'PeerInfo=',Context.PeerInfo);
          writeln('SerialNumber=',fPeer.SerialNumber);
          writeln(fPeer.GetSerial.ToDecimal);
          writeln(fPeer.GetSignatureAlgo);
          writeln(fPeer.GetSignatureHash);
          writeln(fPeer.GetIssuerName.ToDigest);
          exts := fPeer.SubjectAlternativeNames;
          for len := 0 to high(exts) do
            writeln('dns=',exts[len]);
          ext := fPeer.GetExtensions;
          writeln(length(ext));
          for len := 0 to high(ext) do
            writeln(OBJ_nid2sn(ext[len].nid),'=',OBJ_nid2ln(ext[len].nid),'=',ext[len].nid);
          writeln('NotBefore= ',DateTimeToStr(fPeer.NotBefore));
          writeln('NotAfter= ',DateTimeToStr(fPeer.NotAfter));
          writeln('SubjectKeyIdentifier=',fPeer.SubjectKeyIdentifier);
          writeln('AuthorityKeyIdentifier=',fPeer.AuthorityKeyIdentifier);
          writeln('Usage=',word(fPeer.GetUsage));
          writeln('kuDigitalSignature=',fPeer.HasUsage(kuDigitalSignature));
          writeln('kuCodeSign=',fPeer.HasUsage(kuCodeSign));
          writeln('kuTlsClient=',fPeer.HasUsage(kuTlsClient));
          writeln('KeyUsage=',fPeer.KeyUsage);
          writeln('ExtendedKeyUsage=',fPeer.ExtendedKeyUsage);
          writeln('FingerPrint=',fPeer.FingerPrint);
          writeln('IssuerName=',fPeer.IssuerName);
          writeln('SubjectName=',fPeer.SubjectName);
          writeln(fPeer.ExtensionText(NID_basic_constraints));
          writeln(length(fPeer.ToBinary));
          writeln(fPeer.SubjectName);
          writeln(fPeer.GetSubject('O'));
          fPeer.GetSubjectName.SetEntry('O', 'Synopse');
          writeln(fPeer.SubjectName);
          writeln(fPeer.GetSubject('O'));
          fPeer.GetSubjectName.SetEntry('O', 'Synopse2');
          writeln(fPeer.SubjectName);
          fPeer.GetSubjectName.SetEntry('O', 'Synopse2');
          writeln(fPeer.SubjectName);
          }
        end;
        if Context.IgnoreCertificateErrors then
          Context.LastError := 'not verified';
        if Assigned(Context.OnAfterPeerValidate) then
          // allow e.g. to verify CN or DNSName fields
          Context.OnAfterPeerValidate(Socket, fContext, fSsl, fPeer);
      finally
        fPeer.Free;
        fPeer := nil;
      end;
    end;
  finally
    peer^ := nil; // but keep fLastError since fContext remains
  end;
end;

procedure TOpenSslNetTls.SetupCtx(var Context: TNetTlsContext; Bind: boolean);
var
  v, mode, i: integer;
  cert: RawByteString;
  x: PX509;
  xa: PX509DynArray;
  pk: PEVP_PKEY;
  c: PX509;
  ca: Pstack_st_X509;
begin
  // setup the peer verification patterns
  if Context.IgnoreCertificateErrors then
    SSL_CTX_set_verify(fCtx, SSL_VERIFY_NONE, nil)
  else
  begin
    if Assigned(Context.OnEachPeerVerify) then
    begin
      mode := SSL_VERIFY_PEER;
      if Context.ClientCertificateAuthentication then
        mode := mode or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
      if Context.ClientVerifyOnce then
        mode := mode or SSL_VERIFY_CLIENT_ONCE;
      SSL_CTX_set_verify(fCtx, mode, AfterConnectionPeerVerify);
    end
    else
      SSL_CTX_set_verify(fCtx, SSL_VERIFY_PEER, nil);
    if FileExists(TFileName(Context.CACertificatesFile)) then
      SSL_CTX_load_verify_locations(
        fCtx, pointer(Context.CACertificatesFile), nil)
    else if Context.CASystemStores <> [] then
      SSL_CTX_get_cert_store(fCtx)^.AddCertificates(
        LoadCertificatesFromSystemStore(Context.CASystemStores)) // cached
    else if Context.CACertificatesRaw <> nil then
      SSL_CTX_get_cert_store(fCtx)^.AddCertificates(
        PX509DynArray(Context.CACertificatesRaw))
    else
      SSL_CTX_set_default_verify_paths(fCtx);
    if not Bind then
    begin
      if Context.ClientAllowUnsafeRenegotation then
        SSL_CTX_set_options(fCtx, SSL_OP_LEGACY_SERVER_CONNECT);
    end;
  end;
  // load any certificate (and private key)
  pk := nil;
  cert := Context.CertificateBin;
  if (cert = '') and
     (Context.CertificateFile <> '') then
    cert := StringFromFile(TFileName(Context.CertificateFile));
  if cert <> '' then
  begin
    ca := nil;
    xa := LoadCertificates(cert); // PEM
    if xa <> nil then
    try
      Check('SetupCtx Certificate0',
        SSL_CTX_use_certificate(fCtx, xa[0]));
      for i := 1 to high(xa) do
      begin
        Check('SetupCtx Chain',
          SSL_CTX_add_extra_chain_cert(fCtx, xa[i]));
        xa[i] := nil; // fCtx owns it now - no inc(refcnt)
      end;
    finally
      PX509DynArrayFree(xa);
    end
     else if (Context.PrivateKeyRaw = nil) and
            (Context.PrivateKeyFile = '') and
            ParsePkcs12(cert, Context.PrivatePassword, x, pk, @ca) then
      try // was .pfx/pkcs#12 format as with SChannel
        Check('SetupCtx Certificate',
          SSL_CTX_use_certificate(fCtx, x));
        if ca <> nil then
          for i := 0 to ca^.Count - 1 do
          begin
            c := ca^.Items[i];
            X509_up_ref(c); // no inc(refcnt) in fCtx
            SSL_CTX_add_extra_chain_cert(fCtx, c);
          end;
        Check('SetupCtx PrivateKey',
          SSL_CTX_use_PrivateKey(fCtx, pk));
        Check('SetupCtx pfx',
          SSL_CTX_check_private_key(fCtx));
      finally
        x^.Free;
        pk^.Free;
        ca^.FreeX509;
      end
    else
      EOpenSslNetTls.CheckFailed(self, 'SetupCtx: unsupported Certificate',
        nil, nil, 0, fServerAddress);
  end
  else if Context.CertificateRaw <> nil then
    Check('SetupCtx CertificateRaw',
      SSL_CTX_use_certificate(fCtx, Context.CertificateRaw))
  else if Bind then
    raise EOpenSslNetTls.Create('AfterBind: Certificate required');
  if FileExists(TFileName(Context.PrivateKeyFile)) then
  begin
    if Assigned(Context.OnPrivatePassword) then
      SSL_CTX_set_default_passwd_cb(fCtx, AfterConnectionAskPassword)
    else if Context.PrivatePassword <> '' then
      SSL_CTX_set_default_passwd_cb_userdata(
        fCtx, pointer(Context.PrivatePassword));
    SSL_CTX_use_PrivateKey_file(
      fCtx, pointer(Context.PrivateKeyFile), SSL_FILETYPE_PEM);
    Check('SetupCtx check_private_key file',
      SSL_CTX_check_private_key(fCtx));
  end
  else if Context.PrivateKeyRaw <> nil then
  begin
    SSL_CTX_use_PrivateKey(fCtx, Context.PrivateKeyRaw);
    Check('SetupCtx check_private_key raw',
      SSL_CTX_check_private_key(fCtx));
  end
  else if Bind and (pk = nil) then
    raise EOpenSslNetTls.Create('AfterBind: PrivateKey required');
  if Context.CipherList = '' then
    Context.CipherList := SAFE_CIPHERLIST[HasHWAes];
  Check('SetupCtx set_cipher_list',
    SSL_CTX_set_cipher_list(fCtx, pointer(Context.CipherList)));
  v := TLS1_2_VERSION; // no SSL3 TLS1.0 TLS1.1
  if Context.AllowDeprecatedTls then
    v := TLS1_VERSION; // allow TLS1.0 TLS1.1 but no SSL
  SSL_CTX_set_min_proto_version(fCtx, v);
end;

function AfterAcceptSNI(s: PSSL; ad: PInteger; arg: pointer): integer; cdecl;
var
  servername: PUtf8Char;
  nettlscontext: PNetTlsContext absolute arg;
  sslctx: PSSL_CTX;
begin
  result := SSL_TLSEXT_ERR_OK; // requested servername has been accepted
  if not Assigned(nettlscontext) or
     not Assigned(nettlscontext^.OnAcceptServerName) then
    exit; // use default context/certificate
  servername := SSL_get_servername(s, TLSEXT_NAMETYPE_host_name);
  if servername = nil then
    exit;
  sslctx := nettlscontext^.OnAcceptServerName(nettlscontext, s, servername);
  if sslctx <> nil then
    // switching server context
    if SSL_set_SSL_CTX(s, sslctx) = nil then // note: only change certificates
      result := SSL_TLSEXT_ERR_NOACK; // requested servername has been rejected
end;

procedure TOpenSslNetTls.AfterBind(Socket: TNetSocket;
  var Context: TNetTlsContext; const ServerAddress: RawUtf8);
const
  SSL_MAX_SSL_SESSION_ID_LENGTH = 32;
var
  peer: PPointer;
  cert: RawByteString;
  session_ctx: RawByteString;
begin
  // we don't keep any fSocket/fContext bound socket on server side
  Context.LastError := '';
  fLastError := @Context.LastError;
  fServerAddress := ServerAddress;
  peer := @_PeerVerify;
  // prepare global TLS connection properties, as reused by AfterAccept()
  fCtx := SSL_CTX_new(TLS_server_method);
  try
    peer^ := self;

    session_ctx := '';

    // If a certificate is available, derive the session id context from it.
    cert := Context.CertificateBin;
    if (cert = '') and
      (Context.CertificateFile <> '') then
      cert := StringFromFile(TFileName(Context.CertificateFile));
    if cert <> '' then
      session_ctx := Sha256(Context.CertificateBin);

    // If no certificate is available, generate 32 random bytes for the session id context
    if session_ctx = '' then
    begin
      SetLength(session_ctx, SSL_MAX_SSL_SESSION_ID_LENGTH);
      RandomBytes(@session_ctx[1], SSL_MAX_SSL_SESSION_ID_LENGTH);
    end;

    if Length(session_ctx) > SSL_MAX_SSL_SESSION_ID_LENGTH then
      SetLength(session_ctx, SSL_MAX_SSL_SESSION_ID_LENGTH);

    { Fix for TOpenSslNetTls.AfterAccept accept:
      OpenSSL 1010104F error 1 [SSL_ERROR_SSL (error:140D9115:SSL
      routines:ssl_get_prev_session:session id context uninitialized)]

      This error happens because the OpenSSL session id context is not initialized:
      error:140D9115:SSL routines:ssl_get_prev_session:session id context uninitialized

      It occurs when the server tries to resume a previous TLS session but
      the session management context has not been properly configured.

      Therefore we call SSL_CTX_set_session_id_context to set a unique
      session id context identifier. }

    if SSL_CTX_set_session_id_context(fCtx,
        Pointer(session_ctx), Length(session_ctx)) <> 1 then
      raise EOpenSslNetTls.Create('SSL_CTX_set_session_id_context failed');

    SetupCtx(Context, {bind=}true);
    // allow SNI per-server certificate via OnAcceptServerName callback
    if EnableOnNetTlsAcceptServerName then
    begin
      SSL_CTX_set_tlsext_servername_callback(fCtx, AfterAcceptSNI);
      SSL_CTX_set_tlsext_servername_arg(fCtx, @Context);
    end;
    // this global context fCtx will be reused by AfterAccept()
    Context.AcceptCert := fCtx;
  finally
    fLastError := nil; // as expected on server side
    peer^ := nil;
  end;
end;

procedure TOpenSslNetTls.AfterAccept(Socket: TNetSocket;
  const BoundContext: TNetTlsContext; LastError, CipherName: PRawUtf8);
var
  peer: PPointer;
begin
  fSocket := Socket;
  fContext := @BoundContext; // main context may be shared e.g. for TAsyncServer
  // reset output information
  fLastError := LastError;
  peer := @_PeerVerify;
  try
    peer^ := self;
    // prepare TLS connection properties from AfterBind() global context
    if BoundContext.AcceptCert = nil then
      raise EOpenSslNetTls.Create('AfterAccept: missing AfterBind');
    fSsl := SSL_new(BoundContext.AcceptCert);
    Check('AfterAccept set_fd', SSL_set_fd(fSsl, Socket.Socket));
    // server TLS negotiation with server
    Check('AfterAccept accept', SSL_accept(fSsl));
    fDoSslShutdown := true; // need explicit SSL_shutdown() at closing
    if CipherName <> nil then
      CipherName^ := GetCipherName;
  finally
    fLastError := nil; // main fContext is shared, but not as error state
    peer^ := nil;
  end;
end;

function TOpenSslNetTls.GetCipherName: RawUtf8;
begin
  if fCipherName = '' then
    fCipherName := fSsl.CurrentCipher.Description;
  result := fCipherName;
end;

function TOpenSslNetTls.GetRawTls: pointer;
// e.g. to retrieve fSsl.PeerCertificate fields to set in HTTP headers:
//   at least FingerPrint + SubjectName
// SerialNumber,SubjectKeyIdentifier,AuthorityKeyIdentifier,SubjectName
// see https://nginx.org/en/docs/http/ngx_http_ssl_module.html
//   $ssl_client_verify  $ssl_client_i_dn  $ssl_client_escaped_cert
//   $ssl_client_fingerprint $ssl_client_s_dn
begin
  result := fSsl;
end;

function TOpenSslNetTls.GetRawCert(SignHashName: PRawUtf8): RawByteString;
begin
  result := '';
  if (fSsl = nil) or
     (fSsl.PeerCertificate = nil) then
    exit;
  result := fSsl.PeerCertificate^.ToBinary;
  if SignHashName <> nil then
    SignHashName^ := fSsl.PeerCertificate^.GetSignatureHash;
end;

destructor TOpenSslNetTls.Destroy;
begin
  if fSsl <> nil then // client or AfterAccept server connection
  begin
    if fDoSslShutdown then
      SSL_shutdown(fSsl);
    fSsl.Free;
    if fContext <> nil then
      fContext^.PeerCert := nil;
  end;
  if fCtx <> nil then
    fCtx.Free; // client or AfterBind server context
  inherited Destroy;
end;

// see https://www.openssl.org/docs/man1.1.1/man3/SSL_get_error.html
function TOpenSslNetTls.CheckSsl(res: integer): TNetResult;
var
  err: integer;
begin
  err := SSL_get_error(fSsl, res); // caller ensured res > 0
  case err of
    SSL_ERROR_WANT_READ,
    SSL_ERROR_WANT_WRITE:
      // note that want_read may appear during recv, and want_write during send
      result := nrRetry;
    SSL_ERROR_ZERO_RETURN:
      // peer issued an SSL_shutdown -> keep fDoSslShutdown=true
      result := nrClosed;
    SSL_ERROR_SYSCALL:
      begin
        result := NetLastError; // try to get some additional context from OS
        if result in [nrOK, nrRetry] then
          result := nrFatalError;
        fDoSslShutdown := false; // connection is likely to be broken
      end;
    else
      begin
        result := nrFatalError;
        fDoSslShutdown := false;
      end;
  end;
  if (result <> nrRetry) and
     (fLastError <> nil) then
    SSL_get_error_text(err, fLastError^); // retrieve as human-readable text
end;

function TOpenSslNetTls.Receive(Buffer: pointer; var Length: integer): TNetResult;
begin
  Length := SSL_read(fSsl, Buffer, Length);
  if Length <= 0 then
    // read operation was not successful
    result := CheckSsl(Length)
  else
    // return value is number of bytes actually read from the TLS connection
    result := nrOK;
end;

function TOpenSslNetTls.ReceivePending: integer;
begin
  result := SSL_pending(fSsl);
end;

function TOpenSslNetTls.Send(Buffer: pointer; var Length: integer): TNetResult;
begin
  Length := SSL_write(fSsl, Buffer, Length);
  if Length <= 0 then
    // write operation was not successful
    result := CheckSsl(Length)
  else
    // return value is number of bytes actually written to the TLS connection
    result := nrOK;
end;

function NewOpenSslNetTls: INetTls;
begin
  if OpenSslIsAvailable then
  begin
    {$ifdef OSPOSIX}
    if not NewOpenSslNetTlsNoSigPipeIntercept then
      SigPipeIntercept;
    {$endif OSPOSIX}
    result := TOpenSslNetTls.Create;
  end
  else
    result := nil;
end;


function GetPeerCertFromUrl(const url: RawUtf8): PX509DynArray;
var
  u: TUri;
  ns: TNetSocket;
  c: PSSL_CTX;
  s: PSSL;
begin
  Finalize(result);
  if OpenSslIsAvailable and
     u.From(url) and
     (NewSocket(u.Server, u.Port, nlTcp, false, 1000, 1000, 1000, 2, ns) = nrOk) then
  try
    // cut-down version of TOpenSslNetTls.AfterConnection
    c := SSL_CTX_new(TLS_client_method);
    SSL_CTX_set_verify(c, SSL_VERIFY_NONE, nil);
    s := SSL_new(c);
    SSL_set_tlsext_host_name(s, u.Server);
    try
      if (SSL_set_fd(s, ns.Socket) = OPENSSLSUCCESS) and
         (SSL_connect(s) = OPENSSLSUCCESS) then
        result := s.PeerCertificates({acquire=}true);
    finally
      s.Free;
      c.Free;
    end;
  finally
    ns.ShutdownAndClose(false);
  end;
end;

function GetPeerCertPemFromUrl(const url: RawUtf8): RawUtf8;
var
  chain: PX509DynArray;
begin
  chain := GetPeerCertFromUrl(url);
  result := PX509DynArrayToPem(chain);
  PX509DynArrayFree(chain);
end;

function GetPeerCertInfoFromUrl(const url: RawUtf8): RawUtf8;
var
  chain: PX509DynArray;
begin
  chain := GetPeerCertFromUrl(url);
  result := PX509DynArrayToText(chain);
  PX509DynArrayFree(chain);
end;

procedure FinalizeUnit;
var
  s: TSystemCertificateStore;
begin
  _lasts.Done;
  for s := low(s) to high(s) do
    _last[s].Done;
end;


initialization
  // register the OpenSSL TLS layer factory for TCrtSocket (if no SChannel set)
  {$ifndef FORCE_OPENSSL}
  if not Assigned(NewNetTls) then
  {$endif FORCE_OPENSSL}
    @NewNetTls := @NewOpenSslNetTls;

finalization
  FinalizeUnit;
  {$ifndef OPENSSLSTATIC}
  FreeAndNil(libssl);
  FreeAndNil(libcrypto);
  {$endif OPENSSLSTATIC}

{$else}

// some default void globals to avoid most $ifdef USE_OPENSSL ... $endif
var
  OpenSslDefaultCrypto, OpenSslDefaultSsl, OpenSslDefaultPath: string;
  OpenSslVersion: cardinal;
  OpenSslVersionHexa: string;
  OpenSslVersionText: Utf8String;

// some global functions doing nothing or returning false
function OpenSslInitialize(const libcryptoname: string = '';
  const libsslname: string = ''; const libprefix: Utf8String = ''): boolean;
function OpenSslIsAvailable: boolean;
function OpenSslIsLoaded: boolean;


implementation

function OpenSslInitialize(const libcryptoname, libsslname: string;
  const libprefix: Utf8String): boolean;
begin
  result := false;
end;

function OpenSslIsAvailable: boolean;
begin
  result := false;
end;

function OpenSslIsLoaded: boolean;
begin
  result := false;
end;

{$endif USE_OPENSSL}


end.
