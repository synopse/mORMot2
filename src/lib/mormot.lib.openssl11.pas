/// low-level access to the Open SSL 1.1.1 Library
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.openssl11;

{
  *****************************************************************************

   Cross-Platform and Cross-Compiler OpenSSL 1.1.1 API
   - Dynamic or Static OpenSSL Library Loading
   - OpenSSL Library Constants
   - OpenSSL Library Types and Structures
   - OpenSSL Library Functions
   - OpenSSL Helpers
   - TLS / HTTPS Encryption Layer using OpenSSL for mormot.net.sock / TCrtSocket

    In respect to OpenSSL 1.0.x, the new 1.1.1 API hides most structures
   behind getter/setter functions, and doesn't require complex initialization.
    OpenSSL 1.1.1 features TLS 1.3, and is a LTS revision (until 2023-09-11).

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

{.$define OPENSSLFULLAPI}
// define this conditional to publish the whole (huge) OpenSSL API
// - by default, only the API features needed by mORMot are published
// - full API increases compilation time, but is kept as reference
// - the full API libraries will be directly/statically linked, not dynamically:
// if you have "cannot find -lcrypto" errors at linking, run the following:
//     cd /usr/lib/x86_64-linux-gnu
//     sudo ln -s libcrypto.so.1.1 libcrypto.so
//     sudo ln -s libssl.so.1.1 libssl.so

{.$define OPENSSLUSERTLMM}
// define this so that OpenSSL will use pascal RTL getmem/freemem/reallocmem
// - note that OpenSSL has no "finalize" API, and is likely to leak memory - so
// you may define it on production only, if you don't check memory leaks

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
  EOpenSsl = class(Exception)
  protected
    fLastError: integer;
    fLastErrorMsg: RawUtf8;
    class procedure CheckFailed(caller: TObject; const method: string;
      res: integer; errormsg: PRawUtf8);
  public
    /// wrapper around ERR_get_error/ERR_error_string_n if res <> 1
    class procedure Check(caller: TObject; const method: string;
      res: integer; errormsg: PRawUtf8 = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// wrapper around ERR_get_error/ERR_error_string_n if res <> 1
    class procedure Check(res: integer); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// raise the exception if OpenSslIsAvailable if false
    class procedure CheckAvailable(caller: TClass; const method: string);
    /// the last error code from OpenSSL, after Check() failure
    property LastError: integer
      read fLastError;
    /// the last error message text from OpenSSL, after Check() failure
    property LastErrorMsg: RawUtf8
      read fLastErrorMsg;
  end;


const
  // some binaries may be retrieved from https://github.com/grijjy/DelphiOpenSsl
  {$ifdef OSWINDOWS}
    {$ifdef CPU32}
    LIB_CRYPTO = 'libcrypto-1_1.dll';
    LIB_SSL = 'libssl-1_1.dll';
    _PU = '';
    {$else}
    LIB_CRYPTO = 'libcrypto-1_1-x64.dll';
    LIB_SSL = 'libssl-1_1-x64.dll';
    _PU = '';
    {$endif CPU32}
  {$else}
    {$ifdef OSANDROID}
      {$ifdef CPUARM}
      LIB_CRYPTO = 'libcrypto-android32.a';
      LIB_SSL = 'libssl-android32.a';
      _PU = '';
      {$define OPENSSLSTATIC}
      {$else}
      LIB_CRYPTO = 'libcrypto-android64.a';
      LIB_SSL = 'libssl-android64.a';
      _PU = '';
      {$define OPENSSLSTATIC}
      {$endif CPUARM}
    {$else}
      {$ifdef OSDARWIN}
        {$ifdef CPUX86}
        LIB_CRYPTO = 'libssl-merged-osx32.dylib';
        LIB_SSL = 'libssl-merged-osx32.dylib';
        _PU = '_';
        {$endif CPUX86}
        {$ifdef CPUX64}
        LIB_CRYPTO = 'libssl-merged-osx64.dylib';
        LIB_SSL = 'libssl-merged-osx64.dylib';
        _PU = '_';
        {$endif CPUX64}
        {$ifdef CPUX64_static}
        LIB_CRYPTO = 'libcrypto-osx64.a';
        LIB_SSL = 'libssl-osx64.a';
        _PU = '';
        {$define OPENSSLSTATIC}
        {$endif CPUX64}
      {$else}
        {$ifdef OSLINUX}
        LIB_CRYPTO = 'libcrypto.so.1.1'; // specific version on Linux
        LIB_SSL = 'libssl.so.1.1';
        {$else}
        LIB_CRYPTO = 'libcrypto.so'; // should redirect to 1.1.1
        LIB_SSL = 'libssl.so';
        {$endif OSLINUX}
        _PU = '';
      {$endif OSDARWIN}
    {$endif OSANDROID}
  {$endif OSWINDOWS}

var
  /// optional libcrypto location for OpenSslIsAvailable/OpenSslInitialize
  OpenSslDefaultCrypto: TFileName;
  /// optional libssl location for OpenSslIsAvailable/OpenSslInitialize
  OpenSslDefaultSsl: TFileName;


  /// numeric OpenSSL library version loaded e.g. after OpenSslIsAvailable call
  // - equals e.g. $1010106f
  OpenSslVersion: cardinal;

{$ifndef OPENSSLSTATIC}
  /// internal flag used by OpenSslIsAvailable function for dynamic loading
  openssl_initialized: (
    osslUnTested,
    osslAvailable,
    osslNotAvailable);
{$endif OPENSSLSTATIC}


/// return TRUE if OpenSSL 1.1.1 library can be used
// - will load and initialize it, calling OpenSslInitialize if necessary,
// catching any exception during the process
// - return always true if OPENSSLFULLAPI or OPENSSLSTATIC conditionals have
// been defined, since they link the library at compile or startup time
// - you should never call any OpenSSL function if false is returned
function OpenSslIsAvailable: boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// initialize the OpenSSL 1.1.1 API, accessible via the global functions
// - will raise EOpenSsl exception on any loading issue
// - you can force the library names to load
// - do nothing if the library has already been loaded or if
// OPENSSLFULLAPI or OPENSSLSTATIC conditionals have been defined
function OpenSslInitialize(
   const libcryptoname: TFileName = LIB_CRYPTO;
   const libsslname: TFileName = LIB_SSL): boolean;


{ ******************** OpenSSL Library Constants }

const
  OPENSSLSUCCESS = 1;

  EVP_CIPH_NO_PADDING = $100;
  EVP_CTRL_GCM_GET_TAG = $10;
  EVP_CTRL_GCM_SET_TAG = $11;

  EVP_MD_FLAG_ONESHOT = $0001;
  EVP_MD_FLAG_XOF = $0002;
  EVP_MD_FLAG_DIGALGID_MASK = $0018;
  EVP_MD_FLAG_DIGALGID_NULL = $0000;
  EVP_MD_FLAG_DIGALGID_ABSENT = $0008;
  EVP_MD_FLAG_DIGALGID_CUSTOM = $0018;
  EVP_MD_FLAG_FIPS = $0400;

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
  LN_dhKeyAgreement = 'dhKeyAgreement';
  NID_dhKeyAgreement = 28;
  SN_X9_62_id_ecPublicKey = 'id-ecPublicKey';
  NID_X9_62_id_ecPublicKey = 408;

  EVP_PKEY_RSA = NID_rsaEncryption;
  EVP_PKEY_DSA = NID_dsa;
  EVP_PKEY_RSA_PSS = NID_rsassaPss;
  EVP_PKEY_DH = NID_dhKeyAgreement;
  EVP_PKEY_EC = NID_X9_62_id_ecPublicKey;
  EVP_PKEY_ED25519 = NID_ED25519;
  EVP_PKEY_POLY1305 = NID_poly1305;
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

  X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT = $1;
  X509_CHECK_FLAG_NO_WILDCARDS = $2;
  X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS = $4;
  X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS = $8;
  X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS = $10;
  X509_CHECK_FLAG_NEVER_CHECK_SUBJECT = $20;
  _X509_CHECK_FLAG_DOT_SUBDOMAINS = $8000;

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

{$MINENUMSIZE 4}

type
  PBIO = pointer;
  PPBIO = ^PBIO;

  PSSL = pointer;
  PPSSL = ^PSSL;

  PSSL_METHOD = type pointer;
  PPSSL_METHOD = ^PSSL_METHOD;

  PSSL_CIPHER = type pointer;
  PPSSL_CIPHER = ^PSSL_CIPHER;

  PSSL_SESSION = type pointer;
  PPSSL_SESSION = ^PSSL_SESSION;

  PSSL_CTX = type pointer;
  PPSSL_CTX = ^PSSL_CTX;

  PEVP_PKEY = type pointer;
  PPEVP_PKEY = ^PEVP_PKEY;

  PEVP_CIPHER = type pointer;
  PPEVP_CIPHER = ^PEVP_CIPHER;

  PEVP_CIPHER_CTX = type pointer;
  PPEVP_CIPHER_CTX = ^PEVP_CIPHER_CTX;

  PEVP_MD = type pointer;
  PPEVP_MD = ^PEVP_MD;

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

  PBIO_METHOD = type pointer;
  PPBIO_METHOD = ^PBIO_METHOD;

  POPENSSL_STACK = type pointer;
  PPOPENSSL_STACK = ^POPENSSL_STACK;

  PBIGNUM = type pointer;
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

  buf_mem_st = record
    length: PtrUInt;
    data: PUtf8Char;
    max: PtrUInt;
    flags: cardinal;
  end;

  BUF_MEM = buf_mem_st;
  PBUF_MEM = ^BUF_MEM;

  asn1_string_st = record
    length: integer;
    asn1type: integer;
    data: PByte;
    flags: integer;
  end;

  ASN1_INTEGER = asn1_string_st;
  PASN1_INTEGER = ^ASN1_INTEGER;
  PPASN1_INTEGER = ^PASN1_INTEGER;
  PASN1_OBJECT = type pointer;
  PPASN1_OBJECT = ^PASN1_OBJECT;
  ASN1_STRING = asn1_string_st;
  PASN1_STRING = ^ASN1_STRING;
  PPASN1_STRING = ^PASN1_STRING;
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
  ASN1_TIME = asn1_string_st;
  PASN1_TIME = ^ASN1_TIME;

  _anonymous_type_1 = record
    case integer of
      0: (ptr: PUtf8Char);
      1: (boolean: ASN1_BOOLEAN);
      2: (asn1_string: PASN1_STRING);
      3: (_object: PASN1_OBJECT);
      4: (_integer: PASN1_INTEGER);
      5: (enumerated: PASN1_ENUMERATED);
      6: (bit_string: PASN1_BIT_STRING);
      7: (octet_string: PASN1_OCTET_STRING);
      8: (printablestring: PASN1_PRINTABLESTRING);
      9: (t61string: PASN1_T61STRING);
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

  PX509_REQ = type pointer;
  PPX509_REQ = ^PX509_REQ;

  PX509_CERT_AUX = type pointer;
  PPX509_CERT_AUX = ^PX509_CERT_AUX;

  PX509_NAME = type pointer;
  PPX509_NAME = ^PX509_NAME;

  PX509_PUBKEY = type pointer;
  PPX509_PUBKEY = ^PX509_PUBKEY;

  PX509_STORE = type pointer;
  PPX509_STORE = ^PX509_STORE;

  PX509_STORE_CTX = type pointer;
  PPX509_STORE_CTX = ^PX509_STORE_CTX;

  PX509_OBJECT = type pointer;
  PPX509_OBJECT = ^PX509_OBJECT;


  X509_algor_st = record
    algorithm: PASN1_OBJECT;
    parameter: PASN1_TYPE;
  end;

  X509_ALGOR = X509_algor_st;
  PX509_ALGOR = ^X509_ALGOR;
  PPX509_ALGOR = ^PX509_ALGOR;

  X509_val_st = record
    notBefore: PASN1_TIME;
    notAfter: PASN1_TIME;
  end;

  X509_VAL = X509_val_st;
  PX509_VAL = ^X509_VAL;

  TX509_CINF = record
    version: PASN1_INTEGER;
    serialNumber: PASN1_INTEGER;
    signature: PX509_ALGOR;
    issuer: PX509_NAME;
    validity: PX509_VAL;
    subject: PX509_NAME;
    key: PX509_PUBKEY;
  end;
  PX509_CINF = ^TX509_CINF;
  PPX509_CINF = ^PX509_CINF;

  TX509 = record
    cert_info: PX509_CINF;
    sig_alg: PX509_ALGOR;
    signature: PASN1_BIT_STRING;
    valid: integer;
    references: integer;
    name: PUtf8Char;
  end;
  PX509  = ^TX509;
  PPX509 = ^PX509;

  {$ifdef OSWINDOWS}

  // minimal C-like definitions to mimic unixtype FPC unit on Windows
  clong = PtrInt;
  time_t = PtrInt;
  ptime_t = ^time_t;

  timeval = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;
  PTimeVal = ^timeval;

  {$endif OSWINDOWS}

  SSL_verify_cb = function(preverify_ok: integer; x509_ctx: PX509_STORE_CTX): integer; cdecl;
  Ppem_password_cb = function(buf: PUtf8Char; size, rwflag: integer; userdata: pointer): integer; cdecl;
  ECDH_compute_key_KDF = function(_in: pointer; inlen: PtrUInt; _out: pointer; outlen: PPtrUInt): pointer; cdecl;

  dyn_MEM_malloc_fn = function(p1: PtrUInt; p2: PUtf8Char; p3: integer): pointer; cdecl;
  dyn_MEM_realloc_fn = function(p1: pointer; p2: PtrUInt; p3: PUtf8Char; p4: integer): pointer; cdecl;
  dyn_MEM_free_fn = procedure(p1: pointer; p2: PUtf8Char; p3: integer); cdecl;


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
function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
function SSL_shutdown(s: PSSL): integer; cdecl;
function SSL_get_error(s: PSSL; ret_code: integer): integer; cdecl;
function SSL_ctrl(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
procedure SSL_set_bio(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
function SSL_get_peer_certificate(s: PSSL): PX509; cdecl;
procedure SSL_free(ssl: PSSL); cdecl;
function SSL_connect(ssl: PSSL): integer; cdecl;
procedure SSL_set_connect_state(s: PSSL); cdecl;
procedure SSL_set_accept_state(s: PSSL); cdecl;
function SSL_read(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
function SSL_write(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
function SSL_get_state(ssl: PSSL): OSSL_HANDSHAKE_STATE; cdecl;
function SSL_pending(s: PSSL): integer; cdecl;
function SSL_set_cipher_list(s: PSSL; str: PUtf8Char): integer; cdecl;
procedure SSL_get0_alpn_selected(ssl: PSSL; data: PPByte; len: PCardinal); cdecl;
function SSL_clear(s: PSSL): integer; cdecl;
function TLS_client_method(): PSSL_METHOD; cdecl;
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
function SSL_get_verify_result(ssl: PSSL): integer; cdecl;
procedure SSL_set_hostflags(s: PSSL; flags: cardinal); cdecl;
function SSL_set1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;
function SSL_add1_host(s: PSSL; hostname: PUtf8Char): integer; cdecl;

{ --------- libcrypto entries }

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn;
  f: dyn_MEM_free_fn): integer; cdecl;
procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
procedure ERR_remove_state(pid: cardinal); cdecl;
procedure ERR_error_string_n(e: cardinal; buf: PUtf8Char; len: PtrUInt); cdecl;
function ERR_get_error(): cardinal; cdecl;
procedure ERR_remove_thread_state(p1: pointer); cdecl;
function ERR_load_BIO_strings(): integer; cdecl;
function EVP_MD_CTX_create(): PEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX); cdecl;
function EVP_PKEY_size(pkey: PEVP_PKEY): integer; cdecl;
procedure EVP_PKEY_free(pkey: PEVP_PKEY); cdecl;
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
function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer;
  d: PByte; n: PtrUInt; md: PByte; var md_len: cardinal): PByte; cdecl;
function BIO_new(typ: PBIO_METHOD): PBIO; cdecl;
function BIO_free(a: PBIO): integer; cdecl;
function BIO_test_flags(b: PBIO; flags: integer): integer; cdecl;
function BIO_ctrl(bp: PBIO; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
function BIO_new_mem_buf(buf: pointer; len: integer): PBIO; cdecl;
function BIO_s_mem(): PBIO_METHOD; cdecl;
function BIO_read(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
function BIO_write(b: PBIO; data: pointer; dlen: integer): integer; cdecl;
function BIO_new_socket(sock: integer; close_flag: integer): PBIO; cdecl;
function X509_get_issuer_name(a: PX509): PX509_NAME; cdecl;
function X509_get_subject_name(a: PX509): PX509_NAME; cdecl;
function X509_get_pubkey(x: PX509): PEVP_PKEY; cdecl;
procedure X509_free(a: PX509); cdecl;
function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509; cdecl;
function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer; cdecl;
function OPENSSL_sk_num(p1: POPENSSL_STACK): integer; cdecl;
function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
function OBJ_nid2sn(n: integer): PUtf8Char; cdecl;
function OBJ_obj2nid(o: PASN1_OBJECT): integer; cdecl;
function ASN1_STRING_data(x: PASN1_STRING): PByte; cdecl;
function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb;
  u: pointer): PX509; cdecl;
function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
function RAND_bytes(buf: PByte; num: integer): integer; cdecl;
function EVP_get_cipherbyname(name: PUtf8Char): PEVP_CIPHER; cdecl;
function EVP_get_digestbyname(name: PUtf8Char): PEVP_MD; cdecl;
function EVP_CIPHER_CTX_new(): PEVP_CIPHER_CTX; cdecl;
function EVP_CIPHER_CTX_reset(c: PEVP_CIPHER_CTX): integer; cdecl;
procedure EVP_CIPHER_CTX_free(c: PEVP_CIPHER_CTX); cdecl;
function EVP_CIPHER_CTX_copy(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer; cdecl;
function EVP_CIPHER_CTX_ctrl(ctx: PEVP_CIPHER_CTX; typ: integer;
  arg: integer; ptr: pointer): integer; cdecl;
function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
  impl: PENGINE; key: PByte; iv: PByte; enc: integer): integer; cdecl;
function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: integer): integer; cdecl;
function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer; cdecl;
function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
function EVP_MD_CTX_new(): PEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl;
function EVP_MD_CTX_md(ctx: PEVP_MD_CTX): PEVP_MD; cdecl;
function EVP_MD_flags(md: PEVP_MD): cardinal; cdecl;
function EVP_MD_size(md: PEVP_MD): integer; cdecl;
function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer; cdecl;
function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PByte; s: PCardinal): integer; cdecl;
function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PByte; len: PtrUInt): integer; cdecl;
function HMAC_CTX_new(): PHMAC_CTX; cdecl;
procedure HMAC_CTX_free(ctx: PHMAC_CTX); cdecl;
function HMAC_Init_ex(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD;
  impl: PENGINE): integer; cdecl;
function HMAC_Update(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
function HMAC_Final(ctx: PHMAC_CTX; md: PByte; len: PCardinal): integer; cdecl;
function EVP_sha256: PEVP_MD; cdecl;
function EC_GROUP_new_by_curve_name(nid: integer): PEC_GROUP; cdecl;
function EC_KEY_new(): PEC_KEY; cdecl;
function EC_KEY_set_group(key: PEC_KEY; group: PEC_GROUP): integer; cdecl;
function BN_bin2bn(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM; cdecl;
function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
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
function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX); cdecl;
function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
function OpenSSL_version_num(): cardinal; cdecl;
function X509_print(bp: PBIO; x: PX509): integer; cdecl;


{ ******************** OpenSSL Full API Declaration }

{$ifdef OPENSSLFULLAPI}
  // dynamic linking is verbose, so we don't support it on the Full API
  {$define OPENSSLSTATIC}
  // this huge separated .inc file is included only if needed
  {$I .\mormot.lib.openssl11.full.inc}
{$endif OPENSSLFULLAPI}


{ ******************** OpenSSL Helpers }

function BIO_get_flags(b: PBIO): integer;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_should_retry(b: PBIO): boolean;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_should_read(b: PBIO): boolean;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_should_write(b: PBIO): boolean;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_should_io_special(b: PBIO): boolean;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_retry_type(b: PBIO): integer;
  {$ifdef HASINLINE} inline; {$endif}
function BIO_get_ssl(b: PBIO; s: PSSL): integer;
function BIO_pending(b: PBIO): integer;
procedure BIO_ToString(b: PBIO; var data: RawByteString); overload;
function BIO_ToString(b: PBIO; andfree: boolean = false): RawUtf8; overload;

procedure OpenSSL_Free(ptr: pointer);
function SSL_error(error: integer): RawUtf8; overload;
procedure SSL_error(error: integer; var result: RawUtf8); overload;
function SSL_error_short(error: integer): shortstring;
function SSL_is_fatal_error(error: integer): boolean;
procedure WritelnSSL_error; // very useful when debugging

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
function SSL_set_mode(s: PSSL; version: integer): integer;
function SSL_get_mode(s: PSSL): integer;

function EVP_MD_CTX_size(ctx: PEVP_MD_CTX): integer;
function BN_num_bytes(bn: PBIGNUM): integer;
function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
procedure DTLSv1_handle_timeout(s: PSSL);

procedure X509_NAME_ToUtf8(a: PX509_NAME; out result: RawUtf8);
function X509_SubjectName(cert: PX509): RawUtf8;
function X509_IssuerName(cert: PX509): RawUtf8;
function X509_PeerInfo(cert: PX509): RawUtf8;


{ ************** TLS / HTTPS Encryption Layer using OpenSSL for TCrtSocket }

type
  /// exception class raised by OpenSslNewNetTls implementation class
  EOpenSslClient = class(EOpenSsl);


/// OpenSSL TLS layer communication factory - as expected by mormot.net.sock.pas
// - on non-Windows systems, this unit initialization will register OpenSSL for TLS
// - on Windows systems, SChannel will be kept as default so you would need to
// set the FORCE_OPENSSL conditional, or register OpenSSL for TLS mannually:
// ! @NewNetTls := @OpenSslNewNetTls;
function OpenSslNewNetTls: INetTls;



implementation

{ ******************** Minimal Dynamically linked OpenSSL Library }

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
    SSL_new: function(ctx: PSSL_CTX): PSSL; cdecl;
    SSL_shutdown: function(s: PSSL): integer; cdecl;
    SSL_get_error: function(s: PSSL; ret_code: integer): integer; cdecl;
    SSL_ctrl: function(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
    SSL_set_bio: procedure(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
    SSL_get_peer_certificate: function(s: PSSL): PX509; cdecl;
    SSL_free: procedure(ssl: PSSL); cdecl;
    SSL_connect: function(ssl: PSSL): integer; cdecl;
    SSL_set_connect_state: procedure(s: PSSL); cdecl;
    SSL_set_accept_state: procedure(s: PSSL); cdecl;
    SSL_read: function(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_write: function(ssl: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_get_state: function(ssl: PSSL): OSSL_HANDSHAKE_STATE; cdecl;
    SSL_pending: function(s: PSSL): integer; cdecl;
    SSL_set_cipher_list: function(s: PSSL; str: PUtf8Char): integer; cdecl;
    SSL_get0_alpn_selected: procedure(ssl: PSSL; data: PPByte; len: PCardinal); cdecl;
    SSL_clear: function(s: PSSL): integer; cdecl;
    TLS_client_method: function(): PSSL_METHOD; cdecl;
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
    SSL_add1_host: function(s: PSSL; hostname: PUtf8Char): integer; cdecl;
  end;

const
  LIBSSL_ENTRIES: array[0..45] of RawUtf8 = (
    'SSL_CTX_new', 'SSL_CTX_free', 'SSL_CTX_set_timeout', 'SSL_CTX_get_timeout',
    'SSL_CTX_set_verify', 'SSL_CTX_use_PrivateKey', 'SSL_CTX_use_RSAPrivateKey',
    'SSL_CTX_use_RSAPrivateKey_file', 'SSL_CTX_use_certificate',
    'SSL_CTX_check_private_key', 'SSL_CTX_use_certificate_file',
    'SSL_CTX_get_cert_store', 'SSL_CTX_load_verify_locations',
    'SSL_CTX_use_certificate_chain_file', 'SSL_CTX_set_alpn_protos',
    'SSL_CTX_ctrl', 'SSL_new', 'SSL_shutdown', 'SSL_get_error',
    'SSL_ctrl', 'SSL_set_bio', 'SSL_get_peer_certificate', 'SSL_free',
    'SSL_connect', 'SSL_set_connect_state', 'SSL_set_accept_state',
    'SSL_read', 'SSL_write', 'SSL_get_state', 'SSL_pending', 'SSL_set_cipher_list',
    'SSL_get0_alpn_selected', 'SSL_clear', 'TLS_client_method',
    'SSL_CTX_set_default_verify_paths',
    'SSL_CTX_set_default_passwd_cb', 'SSL_CTX_set_default_passwd_cb_userdata',
    'SSL_CTX_use_PrivateKey_file', 'SSL_CTX_set_cipher_list', 'SSL_set_fd',
    'SSL_get_current_cipher', 'SSL_CIPHER_description', 'SSL_get_verify_result',
    'SSL_set_hostflags', 'SSL_set1_host', 'SSL_add1_host');

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

function SSL_new(ctx: PSSL_CTX): PSSL;
begin
  result := libssl.SSL_new(ctx);
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

function SSL_get_peer_certificate(s: PSSL): PX509;
begin
  result := libssl.SSL_get_peer_certificate(s);
end;

procedure SSL_free(ssl: PSSL);
begin
  libssl.SSL_free(ssl);
end;

function SSL_connect(ssl: PSSL): integer;
begin
  result := libssl.SSL_connect(ssl);
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

function SSL_clear(s: PSSL): integer;
begin
  result := libssl.SSL_clear(s);
end;

function TLS_client_method(): PSSL_METHOD;
begin
  result := libssl.TLS_client_method;
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


{ --------- libcrypto entries }

type
  TLibCrypto = class(TSynLibrary)
  public
    CRYPTO_malloc: function(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
    CRYPTO_set_mem_functions: function (m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn; f: dyn_MEM_free_fn): integer; cdecl;
    CRYPTO_free: procedure(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
    ERR_remove_state: procedure(pid: cardinal); cdecl;
    ERR_error_string_n: procedure(e: cardinal; buf: PUtf8Char; len: PtrUInt); cdecl;
    ERR_get_error: function(): cardinal; cdecl;
    ERR_remove_thread_state: procedure(p1: pointer); cdecl;
    ERR_load_BIO_strings: function(): integer; cdecl;
    EVP_MD_CTX_create: function(): PEVP_MD_CTX; cdecl;
    EVP_MD_CTX_destroy: procedure(ctx: PEVP_MD_CTX); cdecl;
    EVP_PKEY_size: function(pkey: PEVP_PKEY): integer; cdecl;
    EVP_PKEY_free: procedure(pkey: PEVP_PKEY); cdecl;
    EVP_DigestSignInit: function(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
    EVP_DigestUpdate: function(ctx: PEVP_MD_CTX; d: pointer; cnt: PtrUInt): integer; cdecl;
    EVP_DigestSignFinal: function(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt): integer; cdecl;
    EVP_DigestVerifyInit: function(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; typ: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): integer; cdecl;
    EVP_DigestVerifyFinal: function(ctx: PEVP_MD_CTX; sig: PByte; siglen: PtrUInt): integer; cdecl;
    EVP_DigestSign: function(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
    EVP_DigestVerify: function(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
    HMAC: function(evp_md: PEVP_MD; key: pointer; key_len: integer; d: PByte; n: PtrUInt; md: PByte; var md_len: cardinal): PByte; cdecl;
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
    X509_free: procedure(a: PX509); cdecl;
    X509_NAME_print_ex_fp: function(fp: PPointer; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
    X509_STORE_CTX_get_current_cert: function(ctx: PX509_STORE_CTX): PX509; cdecl;
    X509_NAME_oneline: function(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
    OPENSSL_sk_pop: function(st: POPENSSL_STACK): pointer; cdecl;
    OPENSSL_sk_num: function(p1: POPENSSL_STACK): integer; cdecl;
    ASN1_BIT_STRING_get_bit: function(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
    OBJ_nid2sn: function(n: integer): PUtf8Char; cdecl;
    OBJ_obj2nid: function(o: PASN1_OBJECT): integer; cdecl;
    ASN1_STRING_data: function(x: PASN1_STRING): PByte; cdecl;
    PEM_read_bio_X509: function(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
    PEM_read_bio_PrivateKey: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_PUBKEY: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_RSAPublicKey: function(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
    PEM_read_bio_RSAPrivateKey: function(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
    RAND_bytes: function(buf: PByte; num: integer): integer; cdecl;
    EVP_get_cipherbyname: function(name: PUtf8Char): PEVP_CIPHER; cdecl;
    EVP_get_digestbyname: function(name: PUtf8Char): PEVP_MD; cdecl;
    EVP_CIPHER_CTX_new: function(): PEVP_CIPHER_CTX; cdecl;
    EVP_CIPHER_CTX_reset: function(c: PEVP_CIPHER_CTX): integer; cdecl;
    EVP_CIPHER_CTX_free: procedure(c: PEVP_CIPHER_CTX); cdecl;
    EVP_CIPHER_CTX_copy: function(_out: PEVP_CIPHER_CTX; _in: PEVP_CIPHER_CTX): integer; cdecl;
    EVP_CIPHER_CTX_ctrl: function(ctx: PEVP_CIPHER_CTX; typ: integer; arg: integer; ptr: pointer): integer; cdecl;
    EVP_CipherInit_ex: function(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte; enc: integer): integer; cdecl;
    EVP_CipherUpdate: function(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger; _in: PByte; inl: integer): integer; cdecl;
    EVP_CipherFinal_ex: function(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): integer; cdecl;
    EVP_CIPHER_CTX_set_padding: function(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
    EVP_MD_CTX_new: function: PEVP_MD_CTX; cdecl;
    EVP_MD_CTX_free: procedure(ctx: PEVP_MD_CTX); cdecl;
    EVP_MD_CTX_md: function(ctx: PEVP_MD_CTX): PEVP_MD; cdecl;
    EVP_MD_flags: function(md: PEVP_MD): cardinal; cdecl;
    EVP_MD_size: function(md: PEVP_MD): integer; cdecl;
    EVP_DigestInit_ex: function(ctx: PEVP_MD_CTX; typ: PEVP_MD; impl: PENGINE): integer; cdecl;
    EVP_DigestFinal_ex: function(ctx: PEVP_MD_CTX; md: PByte; s: PCardinal): integer; cdecl;
    EVP_DigestFinalXOF: function(ctx: PEVP_MD_CTX; md: PByte; len: PtrUInt): integer; cdecl;
    HMAC_CTX_new: function: PHMAC_CTX; cdecl;
    HMAC_CTX_free: procedure(ctx: PHMAC_CTX); cdecl;
    HMAC_Init_ex: function(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD; impl: PENGINE): integer; cdecl;
    HMAC_Update: function(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
    HMAC_Final: function(ctx: PHMAC_CTX; md: PByte; len: PCardinal): integer; cdecl;
    EVP_sha256: function: PEVP_MD; cdecl;
    EC_GROUP_new_by_curve_name: function(nid: integer): PEC_GROUP; cdecl;
    EC_KEY_new: function(): PEC_KEY; cdecl;
    EC_KEY_set_group: function(key: PEC_KEY; group: PEC_GROUP): integer; cdecl;
    BN_bin2bn: function(s: pointer; len: integer; ret: PBIGNUM): PBIGNUM; cdecl;
    EC_POINT_bn2point: function(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
    EC_KEY_set_public_key: function(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
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
    EVP_PKEY_CTX_new: function(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
    EVP_PKEY_CTX_free: procedure(ctx: PEVP_PKEY_CTX); cdecl;
    PEM_write_bio_PrivateKey: function(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER; kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
    PEM_write_bio_PUBKEY: function(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
    OpenSSL_version_num: function(): cardinal; cdecl;
    X509_print: function(bp: PBIO; x: PX509): integer; cdecl;
  end;

const
  LIBCRYPTO_ENTRIES: array[0..107] of RawUtf8 = (
    'CRYPTO_malloc', 'CRYPTO_set_mem_functions', 'CRYPTO_free',
    'ERR_remove_state', 'ERR_error_string_n',
    'ERR_get_error', 'ERR_remove_thread_state', 'ERR_load_BIO_strings',
    'EVP_MD_CTX_new', 'EVP_MD_CTX_free',
    'EVP_PKEY_size', 'EVP_PKEY_free', 'EVP_DigestSignInit', 'EVP_DigestUpdate',
    'EVP_DigestSignFinal', 'EVP_DigestVerifyInit', 'EVP_DigestVerifyFinal',
    'EVP_DigestSign', 'EVP_DigestVerify',
    'HMAC', 'BIO_new', 'BIO_free', 'BIO_test_flags', 'BIO_ctrl', 'BIO_new_mem_buf',
    'BIO_s_mem', 'BIO_read', 'BIO_write', 'BIO_new_socket', 'X509_get_issuer_name',
    'X509_get_subject_name', 'X509_get_pubkey', 'X509_free', 'X509_NAME_print_ex_fp',
    'X509_STORE_CTX_get_current_cert', 'X509_NAME_oneline',
    'OPENSSL_sk_pop', 'OPENSSL_sk_num',
    'ASN1_BIT_STRING_get_bit', 'OBJ_nid2sn', 'OBJ_obj2nid',
    'ASN1_STRING_data', 'PEM_read_bio_X509', 'PEM_read_bio_PrivateKey',
    'PEM_read_bio_PUBKEY', 'PEM_read_bio_RSAPublicKey',
    'PEM_read_bio_RSAPrivateKey', 'RAND_bytes', 'EVP_get_cipherbyname',
    'EVP_get_digestbyname', 'EVP_CIPHER_CTX_new', 'EVP_CIPHER_CTX_reset',
    'EVP_CIPHER_CTX_free', 'EVP_CIPHER_CTX_copy', 'EVP_CIPHER_CTX_ctrl',
    'EVP_CipherInit_ex', 'EVP_CipherUpdate', 'EVP_CipherFinal_ex',
    'EVP_CIPHER_CTX_set_padding', 'EVP_MD_CTX_new', 'EVP_MD_CTX_free',
    'EVP_MD_CTX_md', 'EVP_MD_flags', 'EVP_MD_size', 'EVP_DigestInit_ex',
    'EVP_DigestFinal_ex', 'EVP_DigestFinalXOF', 'HMAC_CTX_new', 'HMAC_CTX_free',
    'HMAC_Init_ex', 'HMAC_Update', 'HMAC_Final', 'EVP_sha256',
    'EC_GROUP_new_by_curve_name', 'EC_KEY_new', 'EC_KEY_set_group',
    'BN_bin2bn', 'EC_POINT_bn2point', 'EC_KEY_set_public_key', 'ECDSA_verify',
    'EC_POINT_free', 'BN_free', 'BN_num_bits', 'EC_KEY_free', 'EC_GROUP_free',
    'EC_KEY_generate_key', 'EC_KEY_get0_private_key', 'EC_KEY_set_private_key',
    'EC_KEY_get0_public_key', 'EC_POINT_point2buf', 'BN_bn2bin', 'ECDSA_size',
    'ECDSA_sign', 'EC_POINT_new', 'EC_POINT_oct2point', 'ECDH_compute_key',
    'EVP_PKEY_CTX_new_id', 'EVP_PKEY_paramgen_init', 'EVP_PKEY_paramgen',
    'EVP_PKEY_keygen_init', 'EVP_PKEY_keygen', 'EVP_PKEY_CTX_ctrl',
    'EVP_PKEY_CTX_new', 'EVP_PKEY_CTX_free', 'PEM_write_bio_PrivateKey',
    'PEM_write_bio_PUBKEY', 'OpenSSL_version_num', 'X509_print');

var
  libcrypto: TLibCrypto;

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer;
begin
  result := libcrypto.CRYPTO_malloc(num, _file, line);
end;

function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn; f: dyn_MEM_free_fn): integer; cdecl;
begin
  result := libcrypto.CRYPTO_set_mem_functions(m, r, f);
end;

procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer);
begin
  libcrypto.CRYPTO_free(ptr, _file, line);
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
  result := libcrypto.EVP_DigestSign(ctx, sigret, siglen, tbs, tbslen);
end;

function EVP_DigestVerify(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt;
   tbs: PByte; tbslen: PtrUInt): integer;
begin
  result := libcrypto.EVP_DigestVerify(ctx, sigret, siglen, tbs, tbslen);
end;

function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer; d: PByte; n: PtrUInt;
   md: PByte; var md_len: cardinal): PByte;
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

procedure X509_free(a: PX509);
begin
  libcrypto.X509_free(a);
end;

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME;
   indent: integer; flags: cardinal): integer;
begin
  result := libcrypto.X509_NAME_print_ex_fp(fp, nm, indent, flags);
end;

function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509;
begin
  result := libcrypto.X509_STORE_CTX_get_current_cert(ctx);
end;

function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char;
begin
  result := libcrypto.X509_NAME_oneline(a, buf, size);
end;

function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer;
begin
  result := libcrypto.OPENSSL_sk_pop(st);
end;

function OPENSSL_sk_num(p1: POPENSSL_STACK): integer;
begin
  result := libcrypto.OPENSSL_sk_num(p1);
end;

function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer;
begin
  result := libcrypto.ASN1_BIT_STRING_get_bit(a, n);
end;

function OBJ_nid2sn(n: integer): PUtf8Char;
begin
  result := libcrypto.OBJ_nid2sn(n);
end;

function OBJ_obj2nid(o: PASN1_OBJECT): integer;
begin
  result := libcrypto.OBJ_obj2nid(o);
end;

function ASN1_STRING_data(x: PASN1_STRING): PByte;
begin
  result := libcrypto.ASN1_STRING_data(x);
end;

function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb;
  u: pointer): PX509;
begin
  result := libcrypto.PEM_read_bio_X509(bp, x, cb, u);
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

function RAND_bytes(buf: PByte; num: integer): integer;
begin
  result := libcrypto.RAND_bytes(buf, num);
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

function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PByte; s: PCardinal): integer;
begin
  result := libcrypto.EVP_DigestFinal_ex(ctx, md, s);
end;

function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PByte; len: PtrUInt): integer;
begin
  result := libcrypto.EVP_DigestFinalXOF(ctx, md, len);
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

function HMAC_Final(ctx: PHMAC_CTX; md: PByte; len: PCardinal): integer;
begin
  result := libcrypto.HMAC_Final(ctx, md, len);
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

function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT;
begin
  result := libcrypto.EC_POINT_bn2point(p1, p2, p3, p4);
end;

function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer;
begin
  result := libcrypto.EC_KEY_set_public_key(key, pub);
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

function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX;
begin
  result := libcrypto.EVP_PKEY_CTX_new(pkey, e);
end;

procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX);
begin
  libcrypto.EVP_PKEY_CTX_free(ctx);
end;

function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER;
  kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer;
begin
  result := libcrypto.PEM_write_bio_PrivateKey(bp, x, enc, kstr, klen, cb, u);
end;

function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer;
begin
  result := libcrypto.PEM_write_bio_PUBKEY(bp, x);
end;

function OpenSSL_version_num(): cardinal;
begin
  result := libcrypto.OpenSSL_version_num();
end;

function X509_print(bp: PBIO; x: PX509): integer;
begin
  result := libcrypto.X509_print(bp, x);
end;


{$ifdef OPENSSLUSERTLMM}

// redirect OpenSSL to use our current pascal heap :)

function rtl_malloc(siz: PtrUInt; fname: PUtf8Char; fline: integer): pointer; cdecl;
begin
  if siz <= 0 then
    result := nil
  else
    Getmem(result, siz);
end;

function rtl_realloc(str: pointer; siz: PtrUInt;
  fname: PUtf8Char; fline: integer): pointer; cdecl;
begin
  if str = nil then
    if siz <= 0 then
      result := nil
    else
      Getmem(result, siz)
  else
    result := ReAllocMem(str, siz);
end;

procedure rtl_free(str: pointer; fname: PUtf8Char; fline: integer); cdecl;
begin
  Freemem(str);
end;

{$endif OPENSSLUSERTLMM}

function OpenSslIsAvailable: boolean;
begin
  case openssl_initialized of
    osslUnTested:
      result := OpenSslInitialize;
    osslAvailable:
      result := true;
  else
    result := false;
  end;
end;

function OpenSslInitialize(const libcryptoname, libsslname: TFileName): boolean;
var
  P: PPointerArray;
  api: PtrInt;
begin
  result := true;
  GlobalLock;
  try
    if openssl_initialized = osslAvailable then
      // set it once, but allow to retry given libnames
      exit;
    libcrypto := TLibCrypto.Create;
    libssl := TLibSsl.Create;
    try
      // attempt to load libcrypto
      libcrypto.TryLoadLibrary([
        OpenSslDefaultCrypto,
      {$ifdef OSWINDOWS}
        // first try the libcrypto*.dll in the local executable folder
        Executable.ProgramFilePath + libcryptoname,
      {$endif OSWINDOWS}
        libcryptoname
      {$ifdef OSPOSIX}
        , 'libcrypto.so' // generic library name on most systems
      {$endif OSPOSIX}
        ], EOpenSsl);
      P := @@libcrypto.CRYPTO_malloc;
      for api := low(LIBCRYPTO_ENTRIES) to high(LIBCRYPTO_ENTRIES) do
        libcrypto.Resolve(pointer(_PU + LIBCRYPTO_ENTRIES[api]),
          @P[api], {onfail=}EOpenSsl);
      // attempt to load libssl
      libssl.TryLoadLibrary([
        OpenSslDefaultSsl,
      {$ifdef OSWINDOWS}
        // first try the libssl*.dll in the local executable folder
        Executable.ProgramFilePath + libsslname,
      {$endif OSWINDOWS}
        libsslname
      {$ifdef OSPOSIX}
        , 'libssl.so'  // generic library name on most UNIX
      {$endif OSPOSIX}
        ], EOpenSsl);
      P := @@libssl.SSL_CTX_new;
      for api := low(LIBSSL_ENTRIES) to high(LIBSSL_ENTRIES) do
        libssl.Resolve(pointer(_PU + LIBSSL_ENTRIES[api]),
          @P[api], {onfail=}EOpenSsl);
      // nothing is to be initialized with OpenSSL 1.1.*
      {$ifdef OPENSSLUSERTLMM}
      if libcrypto.CRYPTO_set_mem_functions(@rtl_malloc, @rtl_realloc, @rtl_free) = 0 then
        raise EOpenSsl.Create('CRYPTO_set_mem_functions() failure');
      {$endif OPENSSLUSERTLMM}
      OpenSslVersion := libcrypto.OpenSSL_version_num;
      if OpenSslVersion and $ffffff00 < $10101000 then // paranoid check 1.1.1
        raise EOpenSsl.CreateFmt('Incorrect OpenSSL version %x in %s - expects' +
          ' >= 101010xx (1.1.1.x)', [libcrypto.LibraryPath, OpenSslVersion]);
    except
      FreeAndNil(libssl);
      FreeAndNil(libcrypto);
    end;
  finally
    if libssl = nil then // flag should be set the last
      openssl_initialized := osslNotAvailable
    else
      openssl_initialized := osslAvailable;
    GlobalUnLock;
    result := libssl <> nil;
  end;
end;

{$endif OPENSSLSTATIC}


{ ******************** Full API via Statically linked OpenSSL Library }

{$ifdef OPENSSLSTATIC}

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

function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
  external LIB_SSL name _PU + 'SSL_new';

function SSL_shutdown(s: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_shutdown';

function SSL_get_error(s: PSSL; ret_code: integer): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_get_error';

function SSL_ctrl(ssl: PSSL; cmd: integer; larg: clong; parg: pointer): clong; cdecl;
  external LIB_SSL name _PU + 'SSL_ctrl';

procedure SSL_set_bio(s: PSSL; rbio: PBIO; wbio: PBIO); cdecl;
  external LIB_SSL name _PU + 'SSL_set_bio';

function SSL_get_peer_certificate(s: PSSL): PX509; cdecl;
  external LIB_SSL name _PU + 'SSL_get_peer_certificate';

procedure SSL_free(ssl: PSSL); cdecl;
  external LIB_SSL name _PU + 'SSL_free';

function SSL_connect(ssl: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_connect';

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

function SSL_clear(s: PSSL): integer; cdecl;
  external LIB_SSL name _PU + 'SSL_clear';

function TLS_client_method(): PSSL_METHOD; cdecl;
  external LIB_SSL name _PU + 'TLS_client_method';

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

function CRYPTO_set_mem_functions(m: dyn_MEM_malloc_fn; r: dyn_MEM_realloc_fn; f: dyn_MEM_free_fn): integer; cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_set_mem_functions';

procedure CRYPTO_free(ptr: pointer; _file: PUtf8Char; line: integer); cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_free';

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

function EVP_DigestSign(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSign';

function EVP_DigestVerify(ctx: PEVP_MD_CTX; sigret: PByte; siglen: PtrUInt; tbs: PByte; tbslen: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestVerify';

function HMAC(evp_md: PEVP_MD; key: pointer; key_len: integer;
  d: PByte; n: PtrUInt; md: PByte; var md_len: cardinal): PByte; cdecl;
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

procedure X509_free(a: PX509); cdecl;
  external LIB_CRYPTO name _PU + 'X509_free';

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_print_ex_fp';

function X509_STORE_CTX_get_current_cert(ctx: PX509_STORE_CTX): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'X509_STORE_CTX_get_current_cert';

function X509_NAME_oneline(a: PX509_NAME; buf: PUtf8Char; size: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_oneline';

function OPENSSL_sk_pop(st: POPENSSL_STACK): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_pop';

function OPENSSL_sk_num(p1: POPENSSL_STACK): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OPENSSL_sk_num';

function ASN1_BIT_STRING_get_bit(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_BIT_STRING_get_bit';

function OBJ_nid2sn(n: integer): PUtf8Char; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_nid2sn';

function OBJ_obj2nid(o: PASN1_OBJECT): integer; cdecl;
  external LIB_CRYPTO name _PU + 'OBJ_obj2nid';

function ASN1_STRING_data(x: PASN1_STRING): PByte; cdecl;
  external LIB_CRYPTO name _PU + 'ASN1_STRING_data';

function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_X509';

function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb;
  u: pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_PrivateKey';

function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_PUBKEY';

function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_RSAPublicKey';

function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb; u: pointer): PRSA; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_RSAPrivateKey';

function RAND_bytes(buf: PByte; num: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'RAND_bytes';

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

function EVP_CIPHER_CTX_ctrl(ctx: PEVP_CIPHER_CTX; typ: integer; arg: integer;
  ptr: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_ctrl';

function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; impl: PENGINE;
  key: PByte; iv: PByte; enc: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherInit_ex';

function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; _out: PByte; outl: PInteger;
  _in: PByte; inl: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherUpdate';

function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; outl: PInteger): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CipherFinal_ex';

function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: integer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_set_padding';

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

function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PByte; s: PCardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestFinal_ex';

function EVP_DigestFinalXOF(ctx: PEVP_MD_CTX; md: PByte; len: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestFinalXOF';

function HMAC_CTX_new(): PHMAC_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_CTX_new';

procedure HMAC_CTX_free(ctx: PHMAC_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_CTX_free';

function HMAC_Init_ex(ctx: PHMAC_CTX; key: pointer; len: integer; md: PEVP_MD; impl: PENGINE): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Init_ex';

function HMAC_Update(ctx: PHMAC_CTX; data: PByte; len: PtrUInt): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Update';

function HMAC_Final(ctx: PHMAC_CTX; md: PByte; len: PCardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'HMAC_Final';

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

function EC_POINT_bn2point(p1: PEC_GROUP; p2: PBIGNUM; p3: PEC_POINT; p4: PBN_CTX): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_bn2point';

function EC_KEY_set_public_key(key: PEC_KEY; pub: PEC_POINT): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_KEY_set_public_key';

function ECDSA_verify(typ: integer; dgst: PByte; dgstlen: integer; sig: PByte; siglen: integer; eckey: PEC_KEY): integer; cdecl;
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

function EC_POINT_point2buf(group: PEC_GROUP; point: PEC_POINT; form: point_conversion_form_t; pbuf: PPByte; ctx: PBN_CTX): PtrUInt; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_point2buf';

function BN_bn2bin(a: PBIGNUM; _to: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'BN_bn2bin';

function ECDSA_size(eckey: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDSA_size';

function ECDSA_sign(typ: integer; dgst: PByte; dgstlen: integer; sig: PByte; siglen: PCardinal; eckey: PEC_KEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'ECDSA_sign';

function EC_POINT_new(group: PEC_GROUP): PEC_POINT; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_new';

function EC_POINT_oct2point(group: PEC_GROUP; p: PEC_POINT; buf: PByte; len: PtrUInt; ctx: PBN_CTX): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EC_POINT_oct2point';

function ECDH_compute_key(_out: pointer; outlen: PtrUInt; pub_key: PEC_POINT; ecdh: PEC_KEY; KDF: ECDH_compute_key_KDF): integer; cdecl;
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

function EVP_PKEY_CTX_ctrl(ctx: PEVP_PKEY_CTX; keytype: integer; optype: integer; cmd: integer; p1: integer; p2: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_ctrl';

function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_new';

procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_free';

function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; enc: PEVP_CIPHER; kstr: PByte; klen: integer; cb: Ppem_password_cb; u: pointer): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_PrivateKey';

function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): integer; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_write_bio_PUBKEY';

function OpenSSL_version_num(): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'OpenSSL_version_num';

function X509_print(bp: PBIO; x: PX509): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_print';


function OpenSslInitialize(const libcryptoname, libsslname: TFileName): boolean;
begin
  OpenSslVersion := OpenSSL_version_num;
  result := true;
end;

function OpenSslIsAvailable: boolean;
begin
  result := true;
end;

{$endif OPENSSLSTATIC}


{ ******************** OpenSSL Helpers }

function BIO_get_flags(b: PBIO): integer;
begin
  result := BIO_test_flags(b, not 0);
end;

function BIO_should_retry(b: PBIO): boolean;
begin
  result := BIO_test_flags(b, BIO_FLAGS_SHOULD_RETRY) <> 0;
end;

function BIO_should_read(b: PBIO): boolean;
begin
  result := BIO_test_flags(b, BIO_FLAGS_READ) <> 0;
end;

function BIO_should_write(b: PBIO): boolean;
begin
  result := BIO_test_flags(b, BIO_FLAGS_WRITE) <> 0;
end;

function BIO_should_io_special(b: PBIO): boolean;
begin
  result := BIO_test_flags(b, BIO_FLAGS_IO_SPECIAL) <> 0;
end;

function BIO_retry_type(b: PBIO): integer;
begin
  result := BIO_test_flags(b, BIO_FLAGS_RWS);
end;

function BIO_get_ssl(b: PBIO; s: PSSL): integer;
begin
  result := BIO_ctrl(b, BIO_C_GET_SSL, 0, s);
end;

function BIO_pending(b: PBIO): integer;
begin
  result := BIO_ctrl(b, _BIO_CTRL_PENDING, 0, nil);
end;

procedure BIO_ToString(b: PBIO; var data: RawByteString);
var
  mem: PBUF_MEM;
begin
  if (b = nil) or
     (BIO_ctrl(b, BIO_C_GET_BUF_MEM_PTR, 0, @mem) <> OPENSSLSUCCESS) then
    data := ''
  else
    SetString(data, PAnsiChar(pointer(mem.data)), mem.length);
end;

function BIO_ToString(b: PBIO; andfree: boolean): RawUtf8;
begin
  BIO_ToString(b, RawByteString(result));
  if andfree then
    BIO_free(b);
end;

procedure OpenSSL_Free(ptr: pointer);
begin
  CRYPTO_free(ptr, 'mormot', 0);
end;

function SSL_error(error: integer): RawUtf8;
begin
  SSL_error(error, result);
end;

procedure SSL_error(error: integer; var result: RawUtf8);
var
  tmp: array[0..1023] of AnsiChar;
begin
  ERR_error_string_n(error, @tmp, SizeOf(tmp));
  FastSetString(result, @tmp, mormot.core.base.StrLen(@tmp));
end;

function SSL_error_short(error: integer): shortstring;
begin
  ERR_error_string_n(error, @result[1], 254);
  result[0] := AnsiChar(mormot.core.base.StrLen(@result[1]));
end;

function SSL_is_fatal_error(error: integer): boolean;
begin
  case error of
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

{$I-}
procedure WritelnSSL_error;
var
  err: integer;
  tmp: array[0..1023] of AnsiChar;
begin
  err := ERR_get_error;
  if err = 0 then
    exit;
  ERR_error_string_n(err, @tmp, SizeOf(tmp));
  writeln({$ifdef FPC}stderr,{$endif} tmp);
  ioresult;
end;
{$I+}

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

function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
begin
  result := SSL_ctrl(s, DTLS_CTRL_GET_TIMEOUT, 0, timeval);
end;

procedure DTLSv1_handle_timeout(s: PSSL);
begin
  SSL_ctrl(s, DTLS_CTRL_HANDLE_TIMEOUT, 0, nil);
end;

procedure X509_NAME_ToUtf8(a: PX509_NAME; out result: RawUtf8);
var
  temp: array[0..511] of AnsiChar;
begin
  temp[0] := #0;
  X509_NAME_oneline(a, @temp, SizeOf(temp) - 1);
  if temp[0] <> #0 then
    FastSetString(result, @temp, StrLen(@temp));
end;

function X509_SubjectName(cert: PX509): RawUtf8;
begin
  X509_NAME_ToUtf8(X509_get_subject_name(cert), result);
end;

function X509_IssuerName(cert: PX509): RawUtf8;
begin
  X509_NAME_ToUtf8(X509_get_issuer_name(cert), result);
end;

function X509_PeerInfo(cert: PX509): RawUtf8;
var
  bio: PBIO;
begin
  bio := BIO_new(BIO_s_mem());
  X509_print(bio, cert);
  result := BIO_ToString(bio, {andfree=}true);
end;


{ EOpenSsl }

class procedure EOpenSsl.Check(caller: TObject; const method: string;
  res: integer; errormsg: PRawUtf8);
begin
  if res <> OPENSSLSUCCESS then
    CheckFailed(caller, method, res, errormsg);
end;

class procedure EOpenSsl.Check(res: integer);
begin
  if res <> OPENSSLSUCCESS then
    CheckFailed(nil, '', res, nil);
end;

class procedure EOpenSsl.CheckFailed(caller: TObject; const method: string;
  res: integer; errormsg: PRawUtf8);
var
  msg: RawUtf8;
  exc: EOpenSsl;
begin
  res := ERR_get_error;
  SSL_error(res, msg);
  if errormsg <> nil then
    errormsg^ := msg;
  if caller = nil then
    exc := CreateFmt('OpenSSL error %d [%s]', [res, msg])
  else
    exc := CreateFmt('%s.%s: OpenSSL error %d [%s]',
      [ClassNameShort(caller)^, method, res, msg]);
  exc.fLastError := res;
  exc.fLastErrorMsg := msg;
  raise exc;
end;

class procedure EOpenSsl.CheckAvailable(caller: TClass; const method: string);
begin
  if not OpenSslIsAvailable then
    if caller = nil then
      raise CreateFmt('%s: OpenSSL 1.1.1 is not available',
        [method])
    else
      raise CreateFmt('%s.%s: OpenSSL 1.1.1 is not available',
        [ClassNameShort(caller)^, method]);
end;



{ ************** TLS / HTTPS Encryption Layer using OpenSSL for TCrtSocket }

{ TOpenSslClient }

type
  /// OpenSSL TLS layer communication
  TOpenSslClient = class(TInterfacedObject, INetTls)
  private
    fSocket: TNetSocket;
    fContext: PNetTlsContext;
    fCtx: PSSL_CTX;
    fSsl: PSSL;
    fPeer: PX509;
    fDoSslShutdown: boolean;
  public
    destructor Destroy; override;
    // INetTls methods
    procedure AfterConnection(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;

threadvar
  _PeerVerify: TOpenSslClient; // OpenSSL is a dumb library for sure

function AfterConnectionPeerVerify(wasok: integer;
  store: PX509_STORE_CTX): integer; cdecl;
var
  peer: PX509;
  c: TOpenSslClient;
begin
  peer := X509_STORE_CTX_get_current_cert(store);
  c := _PeerVerify;
  c.fContext.PeerIssuer := X509_IssuerName(peer);
  c.fContext.PeerSubject := X509_SubjectName(peer);
  result := ord(c.fContext.OnEachPeerVerify(
    c.fSocket, c.fContext, wasok <> 0, c.fSsl, peer));
end;

function AfterConnectionAskPassword(buf: PUtf8Char; size, rwflag: integer;
  userdata: pointer): integer; cdecl;
var
  c: TOpenSslClient;
  pwd: RawUtf8;
begin
  c := _PeerVerify;
  pwd := c.fContext.OnPrivatePassword(c.fSocket, c.fContext, c.fSsl);
  result := length(pwd);
  if result <> 0 then
    if size > result  then
      MoveSmall(pointer(pwd), buf, result + 1) // +1 to include trailing #0
    else
      result := 0; // buf[0..size-1] is too small for this password -> abort
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

const
  // list taken on 2021-02-19 from https://ssl-config.mozilla.org/
  // - prefer CHACHA20-POLY1305 if no AES acceleration is available
  SAFE_CIPHERLIST: array[ {aes=} boolean ] of PUtf8Char = (
    'ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:' +
    'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:' +
    'DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384',
    'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:' +
    'ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:' +
    'DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384');

// see https://www.ibm.com/support/knowledgecenter/SSB23S_1.1.0.2020/gtps7/s5sple2.html

procedure TOpenSslClient.AfterConnection(Socket: TNetSocket;
  var Context: TNetTlsContext; const ServerAddress: RawUtf8);
var
  v, res: integer;
  len: PtrInt;
  ciph: PSSL_CIPHER;
  P: PUtf8Char;
  h: RawUtf8;
  cipher: array[byte] of AnsiChar;
begin
  fSocket := Socket;
  fContext := @Context;
  _PeerVerify := self; // safe and simple context for the callbacks
  // reset output information
  Context.CipherName := '';
  Context.PeerIssuer := '';
  Context.PeerSubject := '';
  Context.PeerInfo := '';
  Context.LastError := '';
  // prepare TLS connection properties
  fCtx := SSL_CTX_new(TLS_client_method);
  if Context.IgnoreCertificateErrors then
    SSL_CTX_set_verify(fCtx, SSL_VERIFY_NONE, nil)
  else
  begin
    if Assigned(Context.OnEachPeerVerify) then
      SSL_CTX_set_verify(fCtx, SSL_VERIFY_PEER, AfterConnectionPeerVerify)
    else
      SSL_CTX_set_verify(fCtx, SSL_VERIFY_PEER, nil);
    if FileExists(TFileName(Context.CACertificatesFile)) then
      SSL_CTX_load_verify_locations(
        fCtx, pointer(Context.CACertificatesFile), nil)
    else
      SSL_CTX_set_default_verify_paths(fCtx);
  end;
  if FileExists(TFileName(Context.CertificateFile)) then
     SSL_CTX_use_certificate_file(
       fCtx, pointer(Context.CertificateFile), SSL_FILETYPE_PEM);
  if FileExists(TFileName(Context.PrivateKeyFile)) then
  begin
    if Assigned(Context.OnPrivatePassword) then
      SSL_CTX_set_default_passwd_cb(fCtx, AfterConnectionAskPassword)
    else if Context.PrivatePassword <> '' then
      SSL_CTX_set_default_passwd_cb_userdata(fCtx, pointer(Context.PrivatePassword));
    SSL_CTX_use_PrivateKey_file(
      fCtx, pointer(Context.PrivateKeyFile), SSL_FILETYPE_PEM);
  end;
  if Context.CipherList = '' then
    Context.CipherList := SAFE_CIPHERLIST[
      {$ifdef CPUINTEL} cfAESNI in CpuFeatures {$else} false {$endif} ];
  EOpenSslClient.Check(self, 'AfterConnection setcipherlist',
    SSL_CTX_set_cipher_list(fCtx, pointer(Context.CipherList)),
    @Context.LastError);
  v := TLS1_2_VERSION; // no SSL3 TLS1.0 TLS1.1
  if Context.AllowDeprecatedTls then
    v := TLS1_VERSION; // allow TLS1.0 TLS1.1
  SSL_CTX_set_min_proto_version(fCtx, v);
  fSsl := SSL_new(fCtx);
  SSL_set_tlsext_host_name(fSsl, ServerAddress); // SNI field
  if not Context.IgnoreCertificateErrors then
  begin
    P := pointer(Context.HostNamesCsv);
    if GetNextCsv(P, h) then
    begin
      SSL_set_hostflags(fSsl, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
      EOpenSslClient.Check(self, 'AfterConnection set1host',
        SSL_set1_host(fSsl, pointer(h)), @Context.LastError);
      while GetNextCsv(P, h) do
        EOpenSslClient.Check(self, 'AfterConnection add1host',
          SSL_add1_host(fSsl, pointer(h)), @Context.LastError);
    end;
  end;
  EOpenSslClient.Check(self, 'AfterConnection setfd',
    SSL_set_fd(fSsl, Socket.Socket), @Context.LastError);
  // client TLS negotiation with server
  EOpenSslClient.Check(self, 'AfterConnection connect',
    SSL_connect(fSsl), @Context.LastError);
  fDoSslShutdown := true; // need explicit SSL_shutdown() at closing
  ciph := SSL_get_current_cipher(fSsl);
  if (ciph <> nil) and
     (SSL_CIPHER_description(ciph, @cipher, SizeOf(cipher)) <> nil) then
  begin
    len := StrLen(@cipher);
    while (len > 0) and
          (cipher[len - 1] <= ' ') do
      dec(len); // trim right #10
    FastSetString(Context.CipherName, @cipher, len);
  end;
  // peer validation
  if Assigned(Context.OnPeerValidate) then
    // via a global custom callback
    Context.OnPeerValidate(Socket, fContext, fSsl)
  else
  begin
    // OpenSSL powered certificate validation
    fPeer := SSL_get_peer_certificate(fSsl);
    if (fPeer = nil) and
       not Context.IgnoreCertificateErrors then
      EOpenSslClient.Check(self, 'AfterConnection getpeercertificate',
        0, @Context.LastError);
    try
      res := SSL_get_verify_result(fSsl);
      if fPeer <> nil then
      begin
        Context.PeerIssuer := X509_IssuerName(fPeer);
        Context.PeerSubject := X509_SubjectName(fPeer);
        if (Context.WithPeerInfo or
           (not Context.IgnoreCertificateErrors and
            (res <> X509_V_OK))) then // include full peer info on failure
          Context.PeerInfo := X509_PeerInfo(fPeer);
      end;
      if res <> X509_V_OK then
      begin
        str(res, Context.LastError);
        if not Context.IgnoreCertificateErrors then
          EOpenSslClient.Check(self, 'AfterConnection getverifyresult',
            0, @Context.LastError);
      end;
      if Assigned(Context.OnAfterPeerValidate) then
        // allow e.g. to verify CN or DNSName fields
        Context.OnAfterPeerValidate(Socket, fContext, fSsl, fPeer);
    finally
      X509_free(fPeer);
      fPeer := nil;
    end;
  end;
end;

destructor TOpenSslClient.Destroy;
begin
  if fCtx <> nil then
  begin
    if fDoSslShutdown then
      SSL_shutdown(fSsl);
    SSL_free(fSsl);
    SSL_CTX_free(fCtx);
  end;
  inherited Destroy;
end;

function TOpenSslClient.Receive(Buffer: pointer; var Length: integer): TNetResult;
var
  read, err: integer;
begin
  read := SSL_read(fSsl, Buffer, Length);
  if read < 0 then
  begin
    err := SSL_get_error(fSsl, read);
    case err of
      SSL_ERROR_WANT_READ:
        result := nrRetry;
      SSL_ERROR_ZERO_RETURN:
        // peer issued an SSL_shutdown -> keep fDoSslShutdown=true
        result := nrFatalError;
      else
        begin
          result := nrFatalError;
          fDoSslShutdown := false;
        end;
    end;
    if result <> nrRetry then
      SSL_error(err, fContext^.LastError);
  end
  else
  begin
    Length := read;
    result := nrOK;
  end;
end;

function TOpenSslClient.Send(Buffer: pointer; var Length: integer): TNetResult;
var
  sent, err: integer;
begin
  sent := SSL_write(fSsl, Buffer, Length);
  if sent < 0 then
  begin
    err := SSL_get_error(fSsl, sent);
    case err of
      SSL_ERROR_WANT_WRITE:
        result := nrRetry;
      SSL_ERROR_ZERO_RETURN:
        // peer issued an SSL_shutdown -> keep fDoSslShutdown=true
        result := nrFatalError;
      else
        begin
          result := nrFatalError;
          fDoSslShutdown := false;
        end;
    end;
    if result <> nrRetry then
      SSL_error(err, fContext^.LastError);
  end
  else
  begin
    Length := sent;
    result := nrOK;
  end;
end;

function OpenSslNewNetTls: INetTls;
begin
  if OpenSslIsAvailable then
    result := TOpenSslClient.Create
  else
    result := nil;
end;



initialization
  // register the OpenSSL TLS layer factory for TCrtSocket (if no SChannel set)
  {$ifndef FORCE_OPENSSL}
  if not Assigned(NewNetTls) then
  {$endif FORCE_OPENSSL}
    @NewNetTls := @OpenSslNewNetTls;

finalization
  {$ifndef OPENSSLSTATIC}
  FreeAndNil(libssl);
  FreeAndNil(libcrypto);
  {$endif OPENSSLSTATIC}

{$else}

implementation

{$endif USE_OPENSSL}


end.
