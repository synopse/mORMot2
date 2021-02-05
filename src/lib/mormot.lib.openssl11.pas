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

    In respect to OpenSSL 1.0.x, the new API hide most structures behind
   getter/setter functions, and don't require complex initialization.
    OpenSSL 1.1.1 features TLS 1.3, and is a LTS revision (until 2023-09-11).

  *****************************************************************************

}

{.$define OPENSSLFULLAPI}
// define this conditional to publish the whole (huge) OpenSSL API
// - by default, only the API features needed by mORMot are published
// - the full API libraries will be directly/statically linked, not dynamically
// - full API increases the compilation time of mormot2tests from 1.9 to 2.4s


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  {$ifdef OSPOSIX}
  unixtype,
  {$endif OSPOSIX}
  mormot.core.os;


{ ******************** Dynamic or Static OpenSSL Library Loading }

type
  /// exception class raised by this unit
  EOpenSsl = class(Exception)
  protected
    class procedure CheckFailed(caller: TObject; const method: string; res: integer);
  public
    /// wrapper around ERR_get_error/ERR_error_string_n if res <> 1
    class procedure Check(caller: TObject; const method: string; res: integer);
      {$ifdef HASINLINE} inline; {$endif}
    /// raise the exception if OpenSslIsAvailable if false
    class procedure CheckAvailable(caller: TClass; const method: string);
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
        LIB_CRYPTO = 'libcrypto-osx64.a';
        LIB_SSL = 'libssl-osx64.a';
        _PU = '';
        {$define OPENSSLSTATIC}
        {$endif CPUX64}
      {$else}
        {$ifdef OSLINUX}
        LIB_CRYPTO = 'libcrypto.so.1.1'; // specific verion on Linux
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
  /// numeric OpenSSL library version loaded e.g. after OpenSslIsAvailable call
  // - equals e.g. $1010106f
  OpenSslVersion: cardinal;

/// return TRUE if OpenSSL 1.1.1 library can be used
// - will load and initialize it, calling OpenSslInitialize if necessary,
// catching any exception during the process
// - return always true if OPENSSLFULLAPI or OPENSSLSTATIC conditionals have
// been defined, since they link the library at compile or startup time
// - you should never call any OpenSSL function if false is returned
function OpenSslIsAvailable: boolean;

/// initialize the OpenSSL 1.1.1 API, accessible via the global functions
// - will raise EOpenSsl exception on any loading issue
// - you can force the library names to load
// - do nothing if the library has already been loaded or if
// OPENSSLFULLAPI or OPENSSLSTATIC conditionals have been defined
procedure OpenSslInitialize(
   const libcryptoname: TFileName = LIB_CRYPTO;
   const libsslname: TFileName = LIB_SSL);


{ ******************** OpenSSL Library Constants }

const
  OPENSSLSUCCESS = 1;

  EVP_CIPH_NO_PADDING = $100;
  EVP_CTRL_GCM_GET_TAG = $10;
  EVP_CTRL_GCM_SET_TAG = $11;

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

  PRSA = type pointer;
  PPRSA = ^PRSA;

  PENGINE = type pointer;
  PPENGINE = ^PENGINE;

  PBIO_METHOD = type pointer;
  PPBIO_METHOD = ^PBIO_METHOD;

  POPENSSL_STACK = type pointer;
  PPOPENSSL_STACK = ^POPENSSL_STACK;

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
function OpenSSL_version_num(): cardinal; cdecl;
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

{ --------- libcrypto entries }

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
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
procedure X509_free(a: PX509); cdecl;
function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
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

function SSL_error(error: integer): RawUtf8;
function SSL_error_short(error: integer): shortstring;
function SSL_is_fatal_error(error: integer): boolean;

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

function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
procedure DTLSv1_handle_timeout(s: PSSL);


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
    OpenSSL_version_num: function(): cardinal; cdecl;
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
  end;

const
  LIBSSL_ENTRIES: array[0..33] of PAnsiChar = (
    'SSL_CTX_new', 'SSL_CTX_free', 'SSL_CTX_set_timeout', 'SSL_CTX_get_timeout',
    'SSL_CTX_set_verify', 'SSL_CTX_use_PrivateKey', 'SSL_CTX_use_RSAPrivateKey',
    'SSL_CTX_use_RSAPrivateKey_file', 'SSL_CTX_use_certificate',
    'SSL_CTX_check_private_key', 'SSL_CTX_use_certificate_file',
    'SSL_CTX_get_cert_store', 'SSL_CTX_load_verify_locations',
    'SSL_CTX_use_certificate_chain_file', 'SSL_CTX_set_alpn_protos',
    'SSL_CTX_ctrl', 'SSL_new', 'SSL_shutdown', 'SSL_get_error', 'OpenSSL_version_num',
    'SSL_ctrl', 'SSL_set_bio', 'SSL_get_peer_certificate', 'SSL_free',
    'SSL_connect', 'SSL_set_connect_state', 'SSL_set_accept_state',
    'SSL_read', 'SSL_write', 'SSL_get_state', 'SSL_pending', 'SSL_set_cipher_list',
    'SSL_get0_alpn_selected', 'SSL_clear');

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

function OpenSSL_version_num(): cardinal;
begin
  result := libssl.OpenSSL_version_num();
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


{ --------- libcrypto entries }

type
  TLibCrypto = class(TSynLibrary)
  public
    CRYPTO_malloc: function(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
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
    X509_free: procedure(a: PX509); cdecl;
    X509_NAME_print_ex_fp: function(fp: PPointer; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
    OPENSSL_sk_pop: function(st: POPENSSL_STACK): pointer; cdecl;
    OPENSSL_sk_num: function(p1: POPENSSL_STACK): integer; cdecl;
    ASN1_BIT_STRING_get_bit: function(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
    OBJ_nid2sn: function(n: integer): PUtf8Char; cdecl;
    OBJ_obj2nid: function(o: PASN1_OBJECT): integer; cdecl;
    ASN1_STRING_data: function(x: PASN1_STRING): PByte; cdecl;
    PEM_read_bio_X509: function(bp: PBIO; x: PPX509; cb: Ppem_password_cb; u: pointer): PX509; cdecl;
    PEM_read_bio_PrivateKey: function(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: pointer): PEVP_PKEY; cdecl;
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
  end;

const
  LIBCRYPTO_ENTRIES: array[0..50] of PAnsiChar = (
    'CRYPTO_malloc', 'CRYPTO_free', 'ERR_remove_state', 'ERR_error_string_n',
    'ERR_get_error', 'ERR_remove_thread_state', 'ERR_load_BIO_strings',
    'EVP_MD_CTX_new', 'EVP_MD_CTX_free',
    'EVP_PKEY_size', 'EVP_PKEY_free', 'EVP_DigestSignInit', 'EVP_DigestUpdate',
    'EVP_DigestSignFinal', 'EVP_DigestVerifyInit', 'EVP_DigestVerifyFinal',
    'HMAC', 'BIO_new', 'BIO_free', 'BIO_test_flags', 'BIO_ctrl', 'BIO_new_mem_buf',
    'BIO_s_mem', 'BIO_read', 'BIO_write', 'BIO_new_socket', 'X509_get_issuer_name',
    'X509_get_subject_name', 'X509_free', 'X509_NAME_print_ex_fp', 'OPENSSL_sk_pop',
    'OPENSSL_sk_num', 'ASN1_BIT_STRING_get_bit', 'OBJ_nid2sn', 'OBJ_obj2nid',
    'ASN1_STRING_data', 'PEM_read_bio_X509', 'PEM_read_bio_PrivateKey',
    'PEM_read_bio_RSAPrivateKey', 'RAND_bytes', 'EVP_get_cipherbyname',
    'EVP_get_digestbyname', 'EVP_CIPHER_CTX_new', 'EVP_CIPHER_CTX_reset',
    'EVP_CIPHER_CTX_free', 'EVP_CIPHER_CTX_copy', 'EVP_CIPHER_CTX_ctrl',
    'EVP_CipherInit_ex', 'EVP_CipherUpdate', 'EVP_CipherFinal_ex',
    'EVP_CIPHER_CTX_set_padding');

var
  libcrypto: TLibCrypto;

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer;
begin
  result := libcrypto.CRYPTO_malloc(num, _file, line);
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

procedure X509_free(a: PX509);
begin
  libcrypto.X509_free(a);
end;

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME;
   indent: integer; flags: cardinal): integer;
begin
  result := libcrypto.X509_NAME_print_ex_fp(fp, nm, indent, flags);
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


var
  openssl_initialized: boolean;

function OpenSslIsAvailable: boolean;
begin
  if not openssl_initialized then
    OpenSslInitialize;
  result := libssl <> nil;
end;

procedure OpenSslInitialize(const libcryptoname, libsslname: TFileName);
var
  P: PPointerArray;
  api: PtrInt;
begin
  GlobalLock;
  try
    if openssl_initialized and
       (libssl <> nil) then // set it once, but allow to retry given libnames
      exit;
    libcrypto := TLibCrypto.Create;
    libssl := TLibSsl.Create;
    try
      // attempt to load libcrypto
      libcrypto.TryLoadLibrary([
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
        libcrypto.Resolve(LIBCRYPTO_ENTRIES[api], @P[api], {onfail=}EOpenSsl);
      // attempt to load libssl
      libssl.TryLoadLibrary([
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
        libssl.Resolve(LIBSSL_ENTRIES[api], @P[api], {onfail=}EOpenSsl);
      // nothing is to be initialized with OpenSSL 1.1.*
      OpenSslVersion := libssl.OpenSSL_version_num;
      if OpenSslVersion and $ffffff00 <> $10101000 then // paranoid check 1.1.1
        raise EOpenSsl.CreateFmt(
          'Incorrect OpenSSL version %x - expected 101010xx', [OpenSslVersion]);
    except
      FreeAndNil(libssl);
      FreeAndNil(libcrypto);
    end;
  finally
    openssl_initialized := true; // should be done the last
    GlobalUnLock;
  end;
end;

{$endif OPENSSLSTATIC}


{ ******************** Full API via Statically linked OpenSSL Library }

{$ifdef OPENSSLSTATIC}

{ --------- libssl entries }

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

function OpenSSL_version_num(): cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'OpenSSL_version_num';

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


{ --------- libcrypto entries }

function CRYPTO_malloc(num: PtrUInt; _file: PUtf8Char; line: integer): pointer; cdecl;
  external LIB_CRYPTO name _PU + 'CRYPTO_malloc';

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

procedure X509_free(a: PX509); cdecl;
  external LIB_CRYPTO name _PU + 'X509_free';

function X509_NAME_print_ex_fp(fp: PPointer; nm: PX509_NAME; indent: integer;
  flags: cardinal): integer; cdecl;
  external LIB_CRYPTO name _PU + 'X509_NAME_print_ex_fp';

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

function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: Ppem_password_cb;
  u: pointer): PRSA; cdecl;
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


procedure OpenSslInitialize(const libcryptoname, libsslname: TFileName);
begin
  OpenSslVersion := OpenSSL_version_num;
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

function SSL_error(error: integer): RawUtf8;
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
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, version, Nil);
end;

function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MAX_PROTO_VERSION, version, Nil);
end;

function SSL_set_tlsext_host_name(const s: PSSL; const name: RawUtf8): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_SET_TLSEXT_HOSTNAME,
    TLSEXT_NAMETYPE_host_name, pointer(name));
end;

function SSL_set_mode(s: PSSL; version: integer): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_MODE, version, Nil);
end;

function SSL_get_mode(s: PSSL): integer;
begin
  result := SSL_ctrl(s, SSL_CTRL_MODE, 0, Nil);
end;

function DTLSv1_get_timeout(s: PSSL; timeval: PTimeVal): time_t;
begin
  result := SSL_ctrl(s, DTLS_CTRL_GET_TIMEOUT, 0, timeval);
end;

procedure DTLSv1_handle_timeout(s: PSSL);
begin
  SSL_ctrl(s, DTLS_CTRL_HANDLE_TIMEOUT, 0, Nil);
end;


{ EOpenSsl }

class procedure EOpenSsl.Check(caller: TObject; const method: string; res: integer);
begin
  if res <> OPENSSLSUCCESS then
    CheckFailed(caller, method, res);
end;

class procedure EOpenSsl.CheckFailed(caller: TObject; const method: string; res: integer);
begin
  res := ERR_get_error;
  raise CreateFmt('%s.%s: OpenSSL error %d [%s]',
    [ClassNameShort(caller)^, method, res, SSL_error_short(res)]);
end;

class procedure EOpenSsl.CheckAvailable(caller: TClass; const method: string);
begin
  if not OpenSslIsAvailable then
    raise CreateFmt('%s.%s: OpenSSL 1.1.1 is not available',
      [ClassNameShort(caller)^, method]);
end;



initialization

finalization
  {$ifndef OPENSSLSTATIC}
  FreeAndNil(libssl);
  FreeAndNil(libcrypto);
  {$endif OPENSSLSTATIC}

end.
