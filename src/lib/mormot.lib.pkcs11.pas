/// low-level access to a PKCS#11 API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.pkcs11;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler PKCS#11 API
   - Low-Level PKCS#11 / Cryptoki API Definitions
   - PKCS#11 High-Level Wrappers

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.json;


{ ***************** Low-Level PKCS#11 / Cryptoki API Definitions }

{
  Notes:
  - we manually translated latest PKCS#11 v3.0 specs
  - we wrote struct fields as CK_ULONG, but defined the proper enums/sets with
    associated ToULONG/ENUMTYPE wrapper functions, to leverage types and RTTI
  - POSIX OpenSC does not follow the OASIS definitions as implemented on Windows
    e.g. Visual C++ makes sizeof(unsigned long int) = 4 so maps cardinal/integer
     but x86_64 gcc makes sizeof(unsigned long int) = 8 so maps PtrInt/PtrUInt
    e.g. all struct are packed on Windows, but aligned on POSIX
}

type
{$ifdef OSWINDOWS}
  /// an unsigned value, at least 32 bits long
  CK_ULONG = cardinal;
  /// a signed value, the same size as a CK_ULONG
  CK_LONG = integer;

  {$A1} // on Windows (and not POSIX!), all struct are packed

{$else}
  /// an unsigned value, at least 32 bits long
  CK_ULONG = PtrUInt;

  /// a signed value, the same size as a CK_ULONG
  CK_LONG = PtrInt;
{$endif OSWINDOWS}

  CK_ULONG_PTR = ^CK_ULONG;


{ ---------- 3.1 General information }

type
  /// used to define a numerical version
  CK_VERSION = record
    /// major version number
    major: byte;
    /// minor version number
    minor: byte;
  end;
  CK_VERSION_PTR = ^CK_VERSION;

  /// provides general information about this Cryptoki
  CK_INFO = record
    /// Cryptoki interface version number
    // - i.e. the API as defined in the OASIS specifications
    cryptokiVersion: CK_VERSION;
    /// UTF-8 ID of the Cryptoki library manufacturer
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    manufacturerID: array[0..31] of AnsiChar;
    /// bit flags reserved for future versions
    // - MUST be zero for this version
    flags: CK_ULONG;
    /// UTF-8 description of the library
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    libraryDescription: array[0..31] of AnsiChar;
    /// Version number of the library software itself
    libraryVersion: CK_VERSION;
  end;


{ ---------- 3.2 Slot and Token types }

type
  /// Cryptoki-assigned value that identifies a slot
  CK_SLOT_ID = type CK_ULONG;
  CK_SLOT_ID_PTR = ^CK_SLOT_ID;

  /// Slot Information Flags
  // - CKF_TOKEN_PRESENT is set e.g. if a device is in the reader
  // - CKF_REMOVABLE_DEVICE is set if the reader supports removable devices
  // - CKF_HW_SLOT if the slot is a hardware slot, as opposed to a software slot
  // implementing a "soft token"
  CKSL_FLAG = (
    CKF_TOKEN_PRESENT,
    CKF_REMOVABLE_DEVICE,
    CKF_HW_SLOT);
  /// set of Slot Information Flags, stored as CK_ULONG field
  CKSL_FLAGS = set of CKSL_FLAG;

  /// the CK_ULONG integer value mapping CKSL_FLAGS high-level set
  CKSL_FLAGS_ULONG = type CK_ULONG;

  /// provides information about a slot
  CK_SLOT_INFO = record
    /// UTF-8 description of the slot
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    slotDescription: array[0..63] of AnsiChar;
    /// UTF-8 ID of the slot manufacturer
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    manufacturerID: array[0..31] of AnsiChar;
    /// Slot Information Flags
    // - to be mapped to a CKSL_FLAGS set
    flags: CKSL_FLAGS_ULONG;
    /// version number of the slot's hardware
    hardwareVersion: CK_VERSION;
    /// version number of the slot's firmware
    firmwareVersion: CK_VERSION;
  end;

  /// Token Information Flags
  CKT_FLAG = (
     CKF_RNG,
     CKF_WRITE_PROTECTED,
     CKF_LOGIN_REQUIRED,
     CKF_USER_PIN_INITIALIZED,
     CKF_10,
     CKF_RESTORE_KEY_NOT_NEEDED,
     CKF_CLOCK_ON_TOKEN,
     CKF_80,
     CKF_PROTECTED_AUTHENTICATION_PATH,
     CKF_DUAL_CRYPTO_OPERATIONS,
     CKF_TOKEN_INITIALIZED,
     CKF_SECONDARY_AUTHENTICATION,
     CKF_1000,
     CKF_2000,
     CKF_4000,
     CKF_8000,
     CKF_USER_PIN_COUNT_LOW,
     CKF_USER_PIN_FINAL_TRY,
     CKF_USER_PIN_LOCKED,
     CKF_USER_PIN_TO_BE_CHANGED,
     CKF_SO_PIN_COUNT_LOW,
     CKF_SO_PIN_FINAL_TRY,
     CKF_SO_PIN_LOCKED,
     CKF_SO_PIN_TO_BE_CHANGED,
     CKF_ERROR_STATE);
  /// set of Token Information Flags, stored as CK_ULONG field
  CKT_FLAGS = set of CKT_FLAG;

  /// the CK_ULONG integer value mapping CKT_FLAGS high-level set
  CKT_FLAGS_ULONG = type CK_ULONG;

  /// provides information about a token
  CK_TOKEN_INFO = record
    /// UTF-8 application-defined label, assigned during token initialization
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    _label: array[0..31] of AnsiChar;
    /// UTF-8 ID of the token manufacturer
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    manufacturerID: array[0..31] of AnsiChar;
    /// UTF-8 model of the token
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    model: array[0..15] of AnsiChar;
    /// ASCII serial number of the slot
    // - MUST be padded with the blank character (' ')
    // - Should not be null-terminated.
    serialNumber: array[0..15] of AnsiChar;
    /// Token Information Flags
    // - to be mapped to a CKT_FLAGS set
    flags: CKT_FLAGS_ULONG;
    /// maximum number of sessions that can be opened with the token at
    // one time by a single application
    // - may be CK_UNAVAILABLE_INFORMATION (ffff) or CK_EFFECTIVELY_INFINITE (0)
    ulMaxSessionCount: CK_ULONG;
    /// number of sessions that this application currently has open with the token
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulSessionCount: CK_ULONG;
    /// maximum number of R/W sessions that can be opened with the token at
    // one time by a single application
    // - may be CK_UNAVAILABLE_INFORMATION (ffff) or CK_EFFECTIVELY_INFINITE (0)
    ulMaxRwSessionCount: CK_ULONG;
    /// number of R/W sessions that this application currently has open with the token
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulRwSessionCount: CK_ULONG;
    /// maximum length in bytes of the PIN
    ulMaxPinLen: CK_ULONG;
    /// minimum length in bytes of the PIN
    ulMinPinLen: CK_ULONG;
    /// the total amount of memory on the token in bytes in which public
    // objects may be stored
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulTotalPublicMemory: CK_ULONG;
    /// the amount of free (unused) memory on the token in bytes for public objects
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulFreePublicMemory: CK_ULONG;
    /// the total amount of memory on the token in bytes in which private
    // objects may be stored
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulTotalPrivateMemory: CK_ULONG;
    /// the amount of free (unused) memory on the token in bytes for
    // private objects
    // - may be CK_UNAVAILABLE_INFORMATION (ffff)
    ulFreePrivateMemory: CK_ULONG;
    /// version number of hardware
    hardwareVersion: CK_VERSION;
    /// version number of firmware
    firmwareVersion: CK_VERSION;
    /// current time as a character-string of length 16
    // - represented in the format YYYYMMDDhhmmssxx (4 characters for the year;
    // 2 characters each for the month, the day, the hour, the minute, and the
    // second; and 2 additional reserved '0' characters).
    // - The value of this field only makes sense for tokens equipped with a
    // clock, as indicated in the token information flags
    utcTime: array[0..15] of AnsiChar;
  end;

const
  /// specific value as used e.g. by CK_TOKEN_INFO or CK_ATTRIBUTE.ulValueLen
  CK_UNAVAILABLE_INFORMATION = CK_ULONG(not CK_ULONG(0));
  /// specific value as used e.g. by CK_TOKEN_INFO
  CK_EFFECTIVELY_INFINITE = CK_ULONG(0);


{ ---------- 3.3 Session types }

const
  /// specific value as used for invalid CK_SESSION_HANDLE or CK_OBJECT_HANDLE value
  CK_INVALID_HANDLE = 0;

type
  /// Cryptoki-assigned value that identifies a session
  // - Valid object handles in Cryptoki always have nonzero values; for
  // convenience, CK_INVALID_HANDLE (=0) identifies a non-valid session handle
  CK_SESSION_HANDLE = type CK_ULONG;
  CK_SESSION_HANDLE_PTR = ^CK_SESSION_HANDLE;

  /// holds the types of Cryptoki users described in [PKCS11-UG]
  // - and, in addition, a context-specific type
  // - mapped as a CK_ULONG
  CK_USER_TYPE = (
    CKU_SO,
    CKU_USER,
    CKU_CONTEXT_SPECIFIC);

  /// holds the session state, as described in [PKCS11-UG]
  // - mapped as a CK_ULONG
  CK_STATE = (
    CKS_RO_PUBLIC_SESSION,
    CKS_RO_USER_FUNCTIONS,
    CKS_RW_PUBLIC_SESSION,
    CKS_RW_USER_FUNCTIONS,
    CKS_RW_SO_FUNCTIONS);

  /// the CK_ULONG integer value mapping CK_STATE high-level set
  CK_STATE_ULONG = type CK_ULONG;

  /// define the type of session
  // - CKF_RW_SESSION is set if the session is read/write
  // - deprecated CKF_SERIAL_SESSION flag should always be set
  CKSE_FLAG = (
    CKF_1,
    CKF_RW_SESSION,
    CKF_SERIAL_SESSION);
  /// define the type of session, stored as CK_ULONG field
  CKSE_FLAGS = set of CKSE_FLAG;

  /// the CK_ULONG integer value mapping CKSE_FLAGS high-level set
  CKSE_FLAGS_ULONG = type CK_ULONG;

  CK_SESSION_INFO = record
    /// ID of the slot that interfaces with the token
    slotID: CK_SLOT_ID;
    /// the state of the session - mapped as CK_STATE enum
    state: CK_STATE_ULONG;
    /// bit flags that define the type of session - mapped as CKSE_FLAGS
    flags: CKSE_FLAGS_ULONG;
    /// an error code defined by the cryptographic device itself
    // - Used for errors not covered by Cryptoki
    ulDeviceError: CK_ULONG;
  end;

function ToText(f: CKT_FLAGS): shortstring; overload;


{ ---------- 3.4 Object types }

type
  /// token-specific identifier for an object
  // - When an object is created or found on a token by an application, Cryptoki
  // assigns it an object handle for that application sessions to use to access it
  // - A particular object on a token does not necessarily have a handle which
  // is fixed for the lifetime of the object; however, if a particular session
  // can use a particular handle to access a particular object, then that
  // session will continue to be able to use that handle to access that object
  // as long as the session continues to exist, the object continues to exist,
  // and the object continues to be accessible to the session.
  // - Valid object handles in Cryptoki always have nonzero values; for
  // convenience, CK_INVALID_HANDLE (=0) identifies a non-valid object handle
  CK_OBJECT_HANDLE = type CK_ULONG;
  CK_OBJECT_HANDLE_PTR = ^CK_OBJECT_HANDLE;

  /// identifies the classes (or types) of objects that Cryptoki recognizes
  // - Object classes are defined with the objects that use them.
  // - The type is specified on an object through the CKA_CLASS attribute of
  // the object.
  // - stored as a CK_ULONG field - and CKO_VENDOR_DEFINED as $80000000 - use
  // ToULONG/OBJECT_CLASS() function wrappers for any conversion
  // - CKO_DATA hold information defined by an application, with CKA_APPLICATION
  // CKA_OBJECT_ID and CKA_VALUE attributes
  // - CKO_CERTIFICATE hold public-key or attribute certificates, with
  // CKA_CERTIFICATE_TYPE, CKA_CERTIFICATE_CATEGORY, CKA_CHECK_VALUE (SHA-1 sum),
  // CKA_START_DATE, CKA_END_DATE and CKA_PUBLIC_KEY_INFO - and for CKC_X_509:
  // CKA_SUBJECT CKA_ID CKA_ISSUER CKA_SERIAL_NUMBER CKA_VALUE CKA_URL; for
  // CKC_WTLS: CKA_SUBJECT CKA_ISSUER CKA_VALUE CKA_URL; for CKC_X_509_ATTR_CERT:
  // CKA_OWNER CKA_AC_ISSUER CKA_SERIAL_NUMBER CKA_ATTR_TYPES CKA_VALUE
  // - CKO_PUBLIC_KEY (4.8), CKO_PRIVATE_KEY (4.9) and CKO_SECRET_KEY (4.10)
  // hold encryption or authentication keys, with CKA_KEY_TYPE CKA_ID CKA_START_DATE
  // CKA_END_DATE CKA_DERIVE CKA_KEY_GEN_MECHANISM CKA_ALLOWED_MECHANISMS
  // - CKO_HW_FEATURE represent CKA_HW_FEATURE_TYPE of the device, e.g.
  // CKH_CLOCK CKH_MONOTONIC_COUNTER CKH_USER_INTERFACE
  // - CKO_DOMAIN_PARAMETERS store certain algorithm's (DSA,DH) extended parameters
  // - CKO_MECHANISM provide extended information about supported mechanisms
  // - CKO_PROFILE describe which PKCS #11 CKA_PROFILE_ID the token implements
  CK_OBJECT_CLASS = (
    CKO_DATA,
    CKO_CERTIFICATE,
    CKO_PUBLIC_KEY,
    CKO_PRIVATE_KEY,
    CKO_SECRET_KEY,
    CKO_HW_FEATURE,
    CKO_DOMAIN_PARAMETERS,
    CKO_MECHANISM,
    CKO_OTP_KEY,
    CKO_PROFILE,
    CKO_VENDOR_DEFINED);

const
  /// the CKO_VENDOR_DEFINED value stored as CK_ULONG
  CKO_VENDOR_DEFINED_ULONG = $80000000;

function ToULONG(oc: CK_OBJECT_CLASS): CK_ULONG; overload;
  {$ifdef HASINLINE} inline; {$endif}
function OBJECT_CLASS(uu: CK_ULONG): CK_OBJECT_CLASS;
  {$ifdef HASINLINE} inline; {$endif}
function ToText(oc: CK_OBJECT_CLASS): PShortString; overload;

type
  /// identifies the hardware feature type of an object with CK_OBJECT_CLASS equal
  // to CKO_HW_FEATURE
  // - stored as a CK_ULONG field - and CKH_VENDOR_DEFINED as $80000000 - use
  // ToULONG/HW_FEATURE_TYPE() function wrappers for any conversion
  CK_HW_FEATURE_TYPE = (
    CKH_0,
    CKH_MONOTONIC_COUNTER,
    CKH_CLOCK,
    CKH_USER_INTERFACE,
    CKH_VENDOR_DEFINED);

const
  /// the CKH_VENDOR_DEFINED value stored as CK_ULONG
  CKH_VENDOR_DEFINED_ULONG = $80000000;

function ToULONG(hw: CK_HW_FEATURE_TYPE): CK_ULONG; overload;
  {$ifdef HASINLINE} inline; {$endif}
function HW_FEATURE_TYPE(uu: CK_ULONG): CK_HW_FEATURE_TYPE;
  {$ifdef HASINLINE} inline; {$endif}
function ToText(hw: CK_HW_FEATURE_TYPE): PShortString; overload;

type
  /// identifies a key type
  // - stored as a CK_ULONG field - and CKK_VENDOR_DEFINED as $80000000 - use
  // ToULONG/KEY_TYPE() function wrappers for any conversion
  CK_KEY_TYPE = (
    CKK_RSA,
    CKK_DSA,
    CKK_DH,
    CKK_EC,
    CKK_X9_42_DH,
    CKK_KEA,
    CKK_6, CKK_7, CKK_8, CKK_9, CKK_A, CKK_B, CKK_C, CKK_D, CKK_E, CKK_F,
    CKK_GENERIC_SECRET,
    CKK_RC2,
    CKK_RC4,
    CKK_DES,
    CKK_DES2,
    CKK_DES3,
    CKK_CAST,
    CKK_CAST3,
    CKK_CAST128,
    CKK_RC5,
    CKK_IDEA,
    CKK_SKIPJACK,
    CKK_BATON,
    CKK_JUNIPER,
    CKK_CDMF,
    CKK_AES,
    CKK_BLOWFISH,
    CKK_TWOFISH,
    CKK_SECURID,
    CKK_HOTP,
    CKK_ACTI,
    CKK_CAMELLIA,
    CKK_ARIA,
    CKK_MD5_HMAC,
    CKK_SHA_1_HMAC,
    CKK_RIPEMD128_HMAC,
    CKK_RIPEMD160_HMAC,
    CKK_SHA256_HMAC,
    CKK_SHA384_HMAC,
    CKK_SHA512_HMAC,
    CKK_SHA224_HMAC,
    CKK_SEED,
    CKK_GOSTR3410,
    CKK_GOSTR3411,
    CKK_GOST28147,
    CKK_CHACHA20,
    CKK_POLY1305,
    CKK_AES_XTS,
    CKK_SHA3_224_HMAC,
    CKK_SHA3_256_HMAC,
    CKK_SHA3_384_HMAC,
    CKK_SHA3_512_HMAC,
    CKK_BLAKE2B_160_HMAC,
    CKK_BLAKE2B_256_HMAC,
    CKK_BLAKE2B_384_HMAC,
    CKK_BLAKE2B_512_HMAC,
    CKK_SALSA20,
    CKK_X2RATCHET,
    CKK_EC_EDWARDS,
    CKK_EC_MONTGOMERY,
    CKK_HKDF,
    CKK_SHA512_224_HMAC,
    CKK_SHA512_256_HMAC,
    CKK_SHA512_T_HMAC,
    CKK_VENDOR_DEFINED);

const
  /// the CKH_VENDOR_DEFINED value stored as CK_ULONG
  CKK_VENDOR_DEFINED_ULONG = $80000000;

function ToULONG(kt: CK_KEY_TYPE): CK_ULONG; overload;
  {$ifdef HASINLINE} inline; {$endif}
function KEY_TYPE(uu: CK_ULONG): CK_KEY_TYPE;
  {$ifdef HASINLINE} inline; {$endif}
function ToText(kt: CK_KEY_TYPE): PShortString; overload;

type
  ///  identifies a certificate type
// - stored as a CK_ULONG field - and CKC_VENDOR_DEFINED as $80000000 - use
// ToULONG/CERTIFICATE_TYPE() function wrappers for any conversion
  CK_CERTIFICATE_TYPE = (
    CKC_X_509,
    CKC_X_509_ATTR_CERT,
    CKC_WTLS,
    CKC_VENDOR_DEFINED);

const
  /// the CKC_VENDOR_DEFINED value stored as CK_ULONG
  CKC_VENDOR_DEFINED_ULONG = $80000000;

function ToULONG(ct: CK_CERTIFICATE_TYPE): CK_ULONG; overload;
  {$ifdef HASINLINE} inline; {$endif}
function CERTIFICATE_TYPE(uu: CK_ULONG): CK_CERTIFICATE_TYPE;
  {$ifdef HASINLINE} inline; {$endif}
function ToText(ct: CK_CERTIFICATE_TYPE): PShortString; overload;

type
  /// identifies a certificate category
  // - stored as a CK_ULONG field
  CK_CERTIFICATE_CATEGORY = (
    CK_CERTIFICATE_CATEGORY_UNSPECIFIED,
    CK_CERTIFICATE_CATEGORY_TOKEN_USER,
    CK_CERTIFICATE_CATEGORY_AUTHORITY,
    CK_CERTIFICATE_CATEGORY_OTHER_ENTITY);

type
  /// identifies an attribute type
  // - stored as a CK_ULONG field but NOT FOLLOWING ord(CK_ATTRIBUTE_TYPE) - use
  // ToULONG/ATTRIBUTE_TYPE() function wrappers for any conversion
  CK_ATTRIBUTE_TYPE = (
    CKA_CLASS,
    CKA_TOKEN,
    CKA_PRIVATE,
    CKA_LABEL,
    CKA_UNIQUE_ID,
    CKA_APPLICATION,
    CKA_VALUE,
    CKA_OBJECT_ID,
    CKA_CERTIFICATE_TYPE,
    CKA_ISSUER,
    CKA_SERIAL_NUMBER,
    CKA_AC_ISSUER,
    CKA_OWNER,
    CKA_ATTR_TYPES,
    CKA_TRUSTED,
    CKA_CERTIFICATE_CATEGORY,
    CKA_JAVA_MIDP_SECURITY_DOMAIN,
    CKA_URL,
    CKA_HASH_OF_SUBJECT_PUBLIC_KEY,
    CKA_HASH_OF_ISSUER_PUBLIC_KEY,
    CKA_NAME_HASH_ALGORITHM,
    CKA_CHECK_VALUE,
    CKA_KEY_TYPE,
    CKA_SUBJECT,
    CKA_ID,
    CKA_SENSITIVE,
    CKA_ENCRYPT,
    CKA_DECRYPT,
    CKA_WRAP,
    CKA_UNWRAP,
    CKA_SIGN,
    CKA_SIGN_RECOVER,
    CKA_VERIFY,
    CKA_VERIFY_RECOVER,
    CKA_DERIVE,
    CKA_START_DATE,
    CKA_END_DATE,
    CKA_MODULUS,
    CKA_MODULUS_BITS,
    CKA_PUBLIC_EXPONENT,
    CKA_PRIVATE_EXPONENT,
    CKA_PRIME_1,
    CKA_PRIME_2,
    CKA_EXPONENT_1,
    CKA_EXPONENT_2,
    CKA_COEFFICIENT,
    CKA_PUBLIC_KEY_INFO,
    CKA_PRIME,
    CKA_SUBPRIME,
    CKA_BASE,
    CKA_PRIME_BITS,
    CKA_SUBPRIME_BITS,
    CKA_VALUE_BITS,
    CKA_VALUE_LEN,
    CKA_EXTRACTABLE,
    CKA_LOCAL,
    CKA_NEVER_EXTRACTABLE,
    CKA_ALWAYS_SENSITIVE,
    CKA_KEY_GEN_MECHANISM,
    CKA_MODIFIABLE,
    CKA_COPYABLE,
    CKA_DESTROYABLE,
    CKA_EC_PARAMS,
    CKA_EC_POINT,
    CKA_ALWAYS_AUTHENTICATE,
    CKA_WRAP_WITH_TRUSTED,
    CKA_WRAP_TEMPLATE,
    CKA_UNWRAP_TEMPLATE,
    CKA_DERIVE_TEMPLATE,
    CKA_OTP_FORMAT,
    CKA_OTP_LENGTH,
    CKA_OTP_TIME_INTERVAL,
    CKA_OTP_USER_FRIENDLY_MODE,
    CKA_OTP_CHALLENGE_REQUIREMENT,
    CKA_OTP_TIME_REQUIREMENT,
    CKA_OTP_COUNTER_REQUIREMENT,
    CKA_OTP_PIN_REQUIREMENT,
    CKA_OTP_COUNTER,
    CKA_OTP_TIME,
    CKA_OTP_USER_IDENTIFIER,
    CKA_OTP_SERVICE_IDENTIFIER,
    CKA_OTP_SERVICE_LOGO,
    CKA_OTP_SERVICE_LOGO_TYPE,
    CKA_GOSTR3410_PARAMS,
    CKA_GOSTR3411_PARAMS,
    CKA_GOST28147_PARAMS,
    CKA_HW_FEATURE_TYPE,
    CKA_RESET_ON_INIT,
    CKA_HAS_RESET,
    CKA_PIXEL_X,
    CKA_PIXEL_Y,
    CKA_RESOLUTION,
    CKA_CHAR_ROWS,
    CKA_CHAR_COLUMNS,
    CKA_COLOR,
    CKA_BITS_PER_PIXEL,
    CKA_CHAR_SETS,
    CKA_ENCODING_METHODS,
    CKA_MIME_TYPES,
    CKA_MECHANISM_TYPE,
    CKA_REQUIRED_CMS_ATTRIBUTES,
    CKA_DEFAULT_CMS_ATTRIBUTES,
    CKA_SUPPORTED_CMS_ATTRIBUTES,
    CKA_ALLOWED_MECHANISMS,
    CKA_PROFILE_ID,
    CKA_X2RATCHET_BAG,
    CKA_X2RATCHET_BAGSIZE,
    CKA_X2RATCHET_BOBS1STMSG,
    CKA_X2RATCHET_CKR,
    CKA_X2RATCHET_CKS,
    CKA_X2RATCHET_DHP,
    CKA_X2RATCHET_DHR,
    CKA_X2RATCHET_DHS,
    CKA_X2RATCHET_HKR,
    CKA_X2RATCHET_HKS,
    CKA_X2RATCHET_ISALICE,
    CKA_X2RATCHET_NHKR,
    CKA_X2RATCHET_NHKS,
    CKA_X2RATCHET_NR,
    CKA_X2RATCHET_NS,
    CKA_X2RATCHET_PNS,
    CKA_X2RATCHET_RK,
    CKA_VENDOR_DEFINED);

  /// the CK_ULONG integer value mapping CK_ATTRIBUTE_TYPE high-level enum
  CK_ATTRIBUTE_TYPE_ULONG = type CK_ULONG;

const
  CKF_ARRAY_ATTRIBUTE = $40000000;

function ToULONG(at: CK_ATTRIBUTE_TYPE): CK_ATTRIBUTE_TYPE_ULONG; overload;
  {$ifdef FPC} inline; {$endif}
function ATTRIBUTE_TYPE(uu: CK_ATTRIBUTE_TYPE_ULONG): CK_ATTRIBUTE_TYPE;
function ToText(at: CK_ATTRIBUTE_TYPE): PShortString; overload;

type
  /// structure that includes the type, value, and length of an attribute
  CK_ATTRIBUTE = record
    /// the attribute type, mapped as CK_ATTRIBUTE_TYPE
    _type: CK_ATTRIBUTE_TYPE_ULONG;
    /// pointer to the value of the attribute
    pValue: pointer;
    /// length in bytes of the value
    // - may be CK_UNAVAILABLE_INFORMATION if this field does not exist
    ulValueLen: CK_ULONG;
  end;
  CK_ATTRIBUTE_PTR = ^CK_ATTRIBUTE;

  // see below the CK_ATTRIBUTES helper to manage a set of CK_ATTRIBUTE


  /// structure that defines a date
  // - this 8 bytes text match DateToIso8601() layout
  CK_DATE = record
    /// the year ("1900" - "9999")
    year: array[0..3] of AnsiChar;
    /// the month ("01" - "12")
    month: array[0..1] of AnsiChar;
    /// the day ("01" - "31")
    day: array[0..1] of AnsiChar;
  end;


{ ---------- 3.5 Data types for mechanisms }

type
  /// identifies a mechanism type
  // - stored as a CK_ULONG field but NOT FOLLOWING ord(CK_MECHANISM_TYPE) - use
  // ToULONG/MECHANISM_TYPE() function wrappers for any conversion
  CK_MECHANISM_TYPE = (
    CKM_RSA_PKCS_KEY_PAIR_GEN,
    CKM_RSA_PKCS,
    CKM_RSA_9796,
    CKM_RSA_X_509,
    CKM_MD2_RSA_PKCS,
    CKM_MD5_RSA_PKCS,
    CKM_SHA1_RSA_PKCS,
    CKM_RIPEMD128_RSA_PKCS,
    CKM_RIPEMD160_RSA_PKCS,
    CKM_RSA_PKCS_OAEP,
    CKM_RSA_X9_31_KEY_PAIR_GEN,
    CKM_RSA_X9_31,
    CKM_SHA1_RSA_X9_31,
    CKM_RSA_PKCS_PSS,
    CKM_SHA1_RSA_PKCS_PSS,
    CKM_DSA_KEY_PAIR_GEN,
    CKM_DSA,
    CKM_DSA_SHA1,
    CKM_DSA_SHA224,
    CKM_DSA_SHA256,
    CKM_DSA_SHA384,
    CKM_DSA_SHA512,
    CKM_DSA_SHA3_224,
    CKM_DSA_SHA3_256,
    CKM_DSA_SHA3_384,
    CKM_DSA_SHA3_512,
    CKM_DH_PKCS_KEY_PAIR_GEN,
    CKM_DH_PKCS_DERIVE,
    CKM_X9_42_DH_KEY_PAIR_GEN,
    CKM_X9_42_DH_DERIVE,
    CKM_X9_42_DH_HYBRID_DERIVE,
    CKM_X9_42_MQV_DERIVE,
    CKM_SHA256_RSA_PKCS,
    CKM_SHA384_RSA_PKCS,
    CKM_SHA512_RSA_PKCS,
    CKM_SHA256_RSA_PKCS_PSS,
    CKM_SHA384_RSA_PKCS_PSS,
    CKM_SHA512_RSA_PKCS_PSS,
    CKM_SHA224_RSA_PKCS,
    CKM_SHA224_RSA_PKCS_PSS,
    CKM_SHA512_224,
    CKM_SHA512_224_HMAC,
    CKM_SHA512_224_HMAC_GENERAL,
    CKM_SHA512_224_KEY_DERIVATION,
    CKM_SHA512_256,
    CKM_SHA512_256_HMAC,
    CKM_SHA512_256_HMAC_GENERAL,
    CKM_SHA512_256_KEY_DERIVATION,
    CKM_SHA512_T,
    CKM_SHA512_T_HMAC,
    CKM_SHA512_T_HMAC_GENERAL,
    CKM_SHA512_T_KEY_DERIVATION,
    CKM_SHA3_256_RSA_PKCS,
    CKM_SHA3_384_RSA_PKCS,
    CKM_SHA3_512_RSA_PKCS,
    CKM_SHA3_256_RSA_PKCS_PSS,
    CKM_SHA3_384_RSA_PKCS_PSS,
    CKM_SHA3_512_RSA_PKCS_PSS,
    CKM_SHA3_224_RSA_PKCS,
    CKM_SHA3_224_RSA_PKCS_PSS,
    CKM_RC2_KEY_GEN,
    CKM_RC2_ECB,
    CKM_RC2_CBC,
    CKM_RC2_MAC,
    CKM_RC2_MAC_GENERAL,
    CKM_RC2_CBC_PAD,
    CKM_RC4_KEY_GEN,
    CKM_RC4,
    CKM_DES_KEY_GEN,
    CKM_DES_ECB,
    CKM_DES_CBC,
    CKM_DES_MAC,
    CKM_DES_MAC_GENERAL,
    CKM_DES_CBC_PAD,
    CKM_DES2_KEY_GEN,
    CKM_DES3_KEY_GEN,
    CKM_DES3_ECB,
    CKM_DES3_CBC,
    CKM_DES3_MAC,
    CKM_DES3_MAC_GENERAL,
    CKM_DES3_CBC_PAD,
    CKM_DES3_CMAC_GENERAL,
    CKM_DES3_CMAC,
    CKM_CDMF_KEY_GEN,
    CKM_CDMF_ECB,
    CKM_CDMF_CBC,
    CKM_CDMF_MAC,
    CKM_CDMF_MAC_GENERAL,
    CKM_CDMF_CBC_PAD,
    CKM_DES_OFB64,
    CKM_DES_OFB8,
    CKM_DES_CFB64,
    CKM_DES_CFB8,
    CKM_MD2,
    CKM_MD2_HMAC,
    CKM_MD2_HMAC_GENERAL,
    CKM_MD5,
    CKM_MD5_HMAC,
    CKM_MD5_HMAC_GENERAL,
    CKM_SHA_1,
    CKM_SHA_1_HMAC,
    CKM_SHA_1_HMAC_GENERAL,
    CKM_RIPEMD128,
    CKM_RIPEMD128_HMAC,
    CKM_RIPEMD128_HMAC_GENERAL,
    CKM_RIPEMD160,
    CKM_RIPEMD160_HMAC,
    CKM_RIPEMD160_HMAC_GENERAL,
    CKM_SHA256,
    CKM_SHA256_HMAC,
    CKM_SHA256_HMAC_GENERAL,
    CKM_SHA224,
    CKM_SHA224_HMAC,
    CKM_SHA224_HMAC_GENERAL,
    CKM_SHA384,
    CKM_SHA384_HMAC,
    CKM_SHA384_HMAC_GENERAL,
    CKM_SHA512,
    CKM_SHA512_HMAC,
    CKM_SHA512_HMAC_GENERAL,
    CKM_SECURID_KEY_GEN,
    CKM_SECURID,
    CKM_HOTP_KEY_GEN,
    CKM_HOTP,
    CKM_ACTI,
    CKM_ACTI_KEY_GEN,
    CKM_SHA3_256,
    CKM_SHA3_256_HMAC,
    CKM_SHA3_256_HMAC_GENERAL,
    CKM_SHA3_256_KEY_GEN,
    CKM_SHA3_224,
    CKM_SHA3_224_HMAC,
    CKM_SHA3_224_HMAC_GENERAL,
    CKM_SHA3_224_KEY_GEN,
    CKM_SHA3_384,
    CKM_SHA3_384_HMAC,
    CKM_SHA3_384_HMAC_GENERAL,
    CKM_SHA3_384_KEY_GEN,
    CKM_SHA3_512,
    CKM_SHA3_512_HMAC,
    CKM_SHA3_512_HMAC_GENERAL,
    CKM_SHA3_512_KEY_GEN,
    CKM_CAST_KEY_GEN,
    CKM_CAST_ECB,
    CKM_CAST_CBC,
    CKM_CAST_MAC,
    CKM_CAST_MAC_GENERAL,
    CKM_CAST_CBC_PAD,
    CKM_CAST3_KEY_GEN,
    CKM_CAST3_ECB,
    CKM_CAST3_CBC,
    CKM_CAST3_MAC,
    CKM_CAST3_MAC_GENERAL,
    CKM_CAST3_CBC_PAD,
    CKM_CAST128_KEY_GEN,
    CKM_CAST128_ECB,
    CKM_CAST128_CBC,
    CKM_CAST128_MAC,
    CKM_CAST128_MAC_GENERAL,
    CKM_CAST128_CBC_PAD,
    CKM_RC5_KEY_GEN,
    CKM_RC5_ECB,
    CKM_RC5_CBC,
    CKM_RC5_MAC,
    CKM_RC5_MAC_GENERAL,
    CKM_RC5_CBC_PAD,
    CKM_IDEA_KEY_GEN,
    CKM_IDEA_ECB,
    CKM_IDEA_CBC,
    CKM_IDEA_MAC,
    CKM_IDEA_MAC_GENERAL,
    CKM_IDEA_CBC_PAD,
    CKM_GENERIC_SECRET_KEY_GEN,
    CKM_CONCATENATE_BASE_AND_KEY,
    CKM_CONCATENATE_BASE_AND_DATA,
    CKM_CONCATENATE_DATA_AND_BASE,
    CKM_XOR_BASE_AND_DATA,
    CKM_EXTRACT_KEY_FROM_KEY,
    CKM_SSL3_PRE_MASTER_KEY_GEN,
    CKM_SSL3_MASTER_KEY_DERIVE,
    CKM_SSL3_KEY_AND_MAC_DERIVE,
    CKM_SSL3_MASTER_KEY_DERIVE_DH,
    CKM_TLS_PRE_MASTER_KEY_GEN,
    CKM_TLS_MASTER_KEY_DERIVE,
    CKM_TLS_KEY_AND_MAC_DERIVE,
    CKM_TLS_MASTER_KEY_DERIVE_DH,
    CKM_TLS_PRF,
    CKM_SSL3_MD5_MAC,
    CKM_SSL3_SHA1_MAC,
    CKM_MD5_KEY_DERIVATION,
    CKM_MD2_KEY_DERIVATION,
    CKM_SHA1_KEY_DERIVATION,
    CKM_SHA256_KEY_DERIVATION,
    CKM_SHA384_KEY_DERIVATION,
    CKM_SHA512_KEY_DERIVATION,
    CKM_SHA224_KEY_DERIVATION,
    CKM_SHA3_256_KEY_DERIVATION,
    CKM_SHA3_224_KEY_DERIVATION,
    CKM_SHA3_384_KEY_DERIVATION,
    CKM_SHA3_512_KEY_DERIVATION,
    CKM_SHAKE_128_KEY_DERIVATION,
    CKM_SHAKE_256_KEY_DERIVATION,
    CKM_PBE_MD2_DES_CBC,
    CKM_PBE_MD5_DES_CBC,
    CKM_PBE_MD5_CAST_CBC,
    CKM_PBE_MD5_CAST3_CBC,
    CKM_PBE_MD5_CAST128_CBC,
    CKM_PBE_SHA1_CAST128_CBC,
    CKM_PBE_SHA1_RC4_128,
    CKM_PBE_SHA1_RC4_40,
    CKM_PBE_SHA1_DES3_EDE_CBC,
    CKM_PBE_SHA1_DES2_EDE_CBC,
    CKM_PBE_SHA1_RC2_128_CBC,
    CKM_PBE_SHA1_RC2_40_CBC,
    CKM_SP800_108_COUNTER_KDF,
    CKM_SP800_108_FEEDBACK_KDF,
    CKM_SP800_108_DOUBLE_PIPELINE_KDF,
    CKM_PKCS5_PBKD2,
    CKM_PBA_SHA1_WITH_SHA1_HMAC,
    CKM_WTLS_PRE_MASTER_KEY_GEN,
    CKM_WTLS_MASTER_KEY_DERIVE,
    CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC,
    CKM_WTLS_PRF,
    CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE,
    CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE,
    CKM_TLS12_MAC,
    CKM_TLS12_KDF,
    CKM_TLS12_MASTER_KEY_DERIVE,
    CKM_TLS12_KEY_AND_MAC_DERIVE,
    CKM_TLS12_MASTER_KEY_DERIVE_DH,
    CKM_TLS12_KEY_SAFE_DERIVE,
    CKM_TLS_MAC,
    CKM_TLS_KDF,
    CKM_KEY_WRAP_LYNKS,
    CKM_KEY_WRAP_SET_OAEP,
    CKM_CMS_SIG,
    CKM_KIP_DERIVE,
    CKM_KIP_WRAP,
    CKM_KIP_MAC,
    CKM_CAMELLIA_KEY_GEN,
    CKM_CAMELLIA_ECB,
    CKM_CAMELLIA_CBC,
    CKM_CAMELLIA_MAC ,
    CKM_CAMELLIA_MAC_GENERAL,
    CKM_CAMELLIA_CBC_PAD,
    CKM_CAMELLIA_ECB_ENCRYPT_DATA,
    CKM_CAMELLIA_CBC_ENCRYPT_DATA,
    CKM_CAMELLIA_CTR,
    CKM_ARIA_KEY_GEN,
    CKM_ARIA_ECB,
    CKM_ARIA_CBC,
    CKM_ARIA_MAC,
    CKM_ARIA_MAC_GENERAL,
    CKM_ARIA_CBC_PAD,
    CKM_ARIA_ECB_ENCRYPT_DATA,
    CKM_ARIA_CBC_ENCRYPT_DATA,
    CKM_SEED_KEY_GEN,
    CKM_SEED_ECB,
    CKM_SEED_CBC,
    CKM_SEED_MAC,
    CKM_SEED_MAC_GENERAL,
    CKM_SEED_CBC_PAD,
    CKM_SEED_ECB_ENCRYPT_DATA,
    CKM_SEED_CBC_ENCRYPT_DATA,
    CKM_SKIPJACK_KEY_GEN,
    CKM_SKIPJACK_ECB64,
    CKM_SKIPJACK_CBC64,
    CKM_SKIPJACK_OFB64,
    CKM_SKIPJACK_CFB64,
    CKM_SKIPJACK_CFB32,
    CKM_SKIPJACK_CFB16,
    CKM_SKIPJACK_CFB8,
    CKM_SKIPJACK_WRAP,
    CKM_SKIPJACK_PRIVATE_WRAP,
    CKM_SKIPJACK_RELAYX,
    CKM_KEA_KEY_PAIR_GEN,
    CKM_KEA_KEY_DERIVE,
    CKM_KEA_DERIVE,
    CKM_FORTEZZA_TIMESTAMP,
    CKM_BATON_KEY_GEN,
    CKM_BATON_ECB128,
    CKM_BATON_ECB96,
    CKM_BATON_CBC128,
    CKM_BATON_COUNTER,
    CKM_BATON_SHUFFLE,
    CKM_BATON_WRAP,
    CKM_EC_KEY_PAIR_GEN,
    CKM_ECDSA,
    CKM_ECDSA_SHA1,
    CKM_ECDSA_SHA224,
    CKM_ECDSA_SHA256,
    CKM_ECDSA_SHA384,
    CKM_ECDSA_SHA512,
    CKM_ECDSA_SHA3_224,
    CKM_ECDSA_SHA3_256,
    CKM_ECDSA_SHA3_384,
    CKM_ECDSA_SHA3_512,
    CKM_ECDH1_DERIVE,
    CKM_ECDH1_COFACTOR_DERIVE,
    CKM_ECMQV_DERIVE,
    CKM_ECDH_AES_KEY_WRAP,
    CKM_RSA_AES_KEY_WRAP,
    CKM_EC_EDWARDS_KEY_PAIR_GEN,
    CKM_EC_MONTGOMERY_KEY_PAIR_GEN,
    CKM_EDDSA,
    CKM_JUNIPER_KEY_GEN,
    CKM_JUNIPER_ECB128,
    CKM_JUNIPER_CBC128,
    CKM_JUNIPER_COUNTER,
    CKM_JUNIPER_SHUFFLE,
    CKM_JUNIPER_WRAP,
    CKM_FASTHASH,
    CKM_AES_XTS,
    CKM_AES_XTS_KEY_GEN,
    CKM_AES_KEY_GEN,
    CKM_AES_ECB,
    CKM_AES_CBC,
    CKM_AES_MAC,
    CKM_AES_MAC_GENERAL,
    CKM_AES_CBC_PAD,
    CKM_AES_CTR,
    CKM_AES_GCM,
    CKM_AES_CCM,
    CKM_AES_CTS,
    CKM_AES_CMAC,
    CKM_AES_CMAC_GENERAL,
    CKM_AES_XCBC_MAC,
    CKM_AES_XCBC_MAC_96,
    CKM_AES_GMAC,
    CKM_BLOWFISH_KEY_GEN,
    CKM_BLOWFISH_CBC,
    CKM_TWOFISH_KEY_GEN,
    CKM_TWOFISH_CBC,
    CKM_BLOWFISH_CBC_PAD,
    CKM_TWOFISH_CBC_PAD,
    CKM_DES_ECB_ENCRYPT_DATA,
    CKM_DES_CBC_ENCRYPT_DATA,
    CKM_DES3_ECB_ENCRYPT_DATA,
    CKM_DES3_CBC_ENCRYPT_DATA,
    CKM_AES_ECB_ENCRYPT_DATA,
    CKM_AES_CBC_ENCRYPT_DATA,
    CKM_GOSTR3410_KEY_PAIR_GEN,
    CKM_GOSTR3410,
    CKM_GOSTR3410_WITH_GOSTR3411,
    CKM_GOSTR3410_KEY_WRAP,
    CKM_GOSTR3410_DERIVE,
    CKM_GOSTR3411,
    CKM_GOSTR3411_HMAC,
    CKM_GOST28147_KEY_GEN,
    CKM_GOST28147_ECB,
    CKM_GOST28147,
    CKM_GOST28147_MAC,
    CKM_GOST28147_KEY_WRAP,
    CKM_CHACHA20_KEY_GEN,
    CKM_CHACHA20,
    CKM_POLY1305_KEY_GEN,
    CKM_POLY1305,
    CKM_EC_KEY_PAIR_GEN_W_EXTRA_BITS,
    CKM_DSA_PARAMETER_GEN,
    CKM_DH_PKCS_PARAMETER_GEN,
    CKM_X9_42_DH_PARAMETER_GEN,
    CKM_DSA_PROBABILISTIC_PARAMETER_GEN,
    CKM_DSA_SHAWE_TAYLOR_PARAMETER_GEN,
    CKM_DSA_FIPS_G_GEN,
    CKM_AES_OFB,
    CKM_AES_CFB64,
    CKM_AES_CFB8,
    CKM_AES_CFB128,
    CKM_AES_CFB1,
    CKM_AES_KEY_WRAP,
    CKM_AES_KEY_WRAP_PAD,
    CKM_AES_KEY_WRAP_KWP,
    CKM_RSA_PKCS_TPM_1_1,
    CKM_RSA_PKCS_OAEP_TPM_1_1,
    CKM_SHA_1_KEY_GEN,
    CKM_SHA224_KEY_GEN,
    CKM_SHA256_KEY_GEN,
    CKM_SHA384_KEY_GEN,
    CKM_SHA512_KEY_GEN,
    CKM_SHA512_224_KEY_GEN,
    CKM_SHA512_256_KEY_GEN,
    CKM_SHA512_T_KEY_GEN,
    CKM_NULL,
    CKM_BLAKE2B_160,
    CKM_BLAKE2B_160_HMAC,
    CKM_BLAKE2B_160_HMAC_GENERAL,
    CKM_BLAKE2B_160_KEY_DERIVE,
    CKM_BLAKE2B_160_KEY_GEN,
    CKM_BLAKE2B_256,
    CKM_BLAKE2B_256_HMAC,
    CKM_BLAKE2B_256_HMAC_GENERAL,
    CKM_BLAKE2B_256_KEY_DERIVE,
    CKM_BLAKE2B_256_KEY_GEN,
    CKM_BLAKE2B_384,
    CKM_BLAKE2B_384_HMAC,
    CKM_BLAKE2B_384_HMAC_GENERAL,
    CKM_BLAKE2B_384_KEY_DERIVE,
    CKM_BLAKE2B_384_KEY_GEN,
    CKM_BLAKE2B_512,
    CKM_BLAKE2B_512_HMAC,
    CKM_BLAKE2B_512_HMAC_GENERAL,
    CKM_BLAKE2B_512_KEY_DERIVE,
    CKM_BLAKE2B_512_KEY_GEN,
    CKM_SALSA20,
    CKM_CHACHA20_POLY1305,
    CKM_SALSA20_POLY1305,
    CKM_X3DH_INITIALIZE,
    CKM_X3DH_RESPOND,
    CKM_X2RATCHET_INITIALIZE,
    CKM_X2RATCHET_RESPOND,
    CKM_X2RATCHET_ENCRYPT,
    CKM_X2RATCHET_DECRYPT,
    CKM_XEDDSA,
    CKM_HKDF_DERIVE,
    CKM_HKDF_DATA,
    CKM_HKDF_KEY_GEN,
    CKM_SALSA20_KEY_GEN,
    CKM_VENDOR_DEFINED);
  CK_MECHANISM_TYPES = array of CK_MECHANISM_TYPE;

  CK_MECHANISM_TYPE_ULONG = type CK_ULONG;
  CK_MECHANISM_TYPE_ULONG_PTR = ^CK_MECHANISM_TYPE_ULONG;

const
  CKM_VENDOR_DEFINED_ULONG = $80000000;

function ToULONG(mt: CK_MECHANISM_TYPE): CK_MECHANISM_TYPE_ULONG; overload;
  {$ifdef FPC} inline; {$endif}
function MECHANISM_TYPE(uu: CK_MECHANISM_TYPE_ULONG): CK_MECHANISM_TYPE;
function ToText(mt: CK_MECHANISM_TYPE): PShortString; overload;

/// the default CK_MECHANISM_TYPE used for most known key types generation
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultGenerateMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

/// the default CK_MECHANISM_TYPE used for most known key types domain generation
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultParamGenerateMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

/// the default CK_MECHANISM_TYPE used for most known key types encrypt/decrypt
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultEncryptMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

/// the default CK_MECHANISM_TYPE used for most known key types sign/verify
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultSignMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

/// the default CK_MECHANISM_TYPE used for most known key types wrap/unwrap
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultWrapMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

/// the default CK_MECHANISM_TYPE used for most known key types derivation
// - returns false if not known enough, or true and set uu with the mechanism type
function DefaultDeriveMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;

type
  /// specifies a particular mechanism and any parameters it requires
  CK_MECHANISM = record
    /// the type of mechanism, mapped as CK_MECHANISM_TYPE
    mechanism: CK_ULONG;
    /// pointer to the parameter if required by the mechanism
    pParameter: pointer;
    /// length in bytes of the parametes
    ulParameterLen: CK_ULONG;
  end;

  /// Mechanism Information Flags
  CKM_FLAG = (
   CKF_HW,
   CKF_MESSAGE_ENCRYPT,
   CKF_MESSAGE_DECRYPT,
   CKF_MESSAGE_SIGN,
   CKF_MESSAGE_VERIFY,
   CKF_MULTI_MESSAGE,
   CKF_FIND_OBJECTS,
   CKF_M80,
   CKF_ENCRYPT,
   CKF_DECRYPT,
   CKF_DIGEST,
   CKF_SIGN,
   CKF_SIGN_RECOVER,
   CKF_VERIFY,
   CKF_VERIFY_RECOVER,
   CKF_GENERATE,
   CKF_GENERATE_KEY_PAIR,
   CKF_WRAP,
   CKF_UNWRAP,
   CKF_DERIVE,
   CKF_EC_F_P,
   CKF_EC_F_2M,
   CKF_EC_ECPARAMETERS,
   CKF_EC_OID,
   CKF_EC_UNCOMPRESS,
   CKF_EC_COMPRESS,
   CKF_EC_CURVENAME,
   CKF_M08, CKF_M10, CKF_M20, CKF_M40,
   CKF_EXTENSION);
  /// set of Mechanism Information Flags
  CKM_FLAGS = set of CKM_FLAG;

  /// the CK_ULONG integer value mapping CKM_FLAGS high-level set
  CKM_FLAGS_ULONG = type CK_ULONG;

  /// provides information about a particular mechanism
  // - for some mechanisms, the ulMinKeySize and ulMaxKeySize fields have
  // meaningless values
  CK_MECHANISM_INFO = record
    /// the minimum size of the key for the mechanism (whether this is
    // measured in bits or in bytes is mechanism-dependent)
    ulMinKeySize: CK_ULONG;
    /// the maximum size of the key for the mechanism (whether this is
    // measured in bits or in bytes is mechanism-dependent)
    ulMaxKeySize: CK_ULONG;
    /// Mechanism Information Flags
    // - to be mapped to a CKM_FLAGS set
    flags: CKM_FLAGS_ULONG;
  end;


type
  /// helper to manage a set of CK_ATTRIBUTE at runtime
  {$ifdef USERECORDWITHMETHODS}
  CK_ATTRIBUTES = record
  {$else}
  CK_ATTRIBUTES = object
  {$endif USERECORDWITHMETHODS}
  public
    /// storage of CK_ATTRIBUTE items
    // - length(Attrs) is the capacity of the array: use Count for actual number
    Attrs: array of CK_ATTRIBUTE;
    /// how many items are currently stored in Attrs[]
    Count: PtrInt;

    /// append a CKA_CLASS attribute
    procedure New(aClass: CK_OBJECT_CLASS); overload;
    /// append a CKA_CLASS storage attribute as token object (CKA_TOKEN = true)
    // - if aLabel is not '', will also set a CKA_LABEL attribute
    // - if aID is not '', will also set CKA_ID attribute
    procedure New(aClass: CK_OBJECT_CLASS; const aLabel: RawUtf8;
      const aID: RawUtf8 = ''; aStore: boolean = true); overload;
    /// append a raw CK_ATTRIBUTE
    procedure Add(aType: CK_ATTRIBUTE_TYPE; aValue: pointer; aLen: CK_ULONG); overload;
    /// append a CK_ULONG CK_ATTRIBUTE
    procedure Add(aType: CK_ATTRIBUTE_TYPE; aValue: CK_ULONG); overload;
    /// append a CK_BBOOL CK_ATTRIBUTE
    procedure Add(aType: CK_ATTRIBUTE_TYPE; aValue: boolean); overload;
    /// append a CK_DATE text/binary CK_ATTRIBUTE
    procedure AddDate(aType: CK_ATTRIBUTE_TYPE; aValue: TDateTime);
    /// append a text/binary CK_ATTRIBUTE
    procedure Add(aType: CK_ATTRIBUTE_TYPE; const aValue: RawByteString); overload;
    /// append a CKA_CERTIFICATE_TYPE attribute
    procedure Add(aCert: CK_CERTIFICATE_TYPE); overload;
    /// append a CKA_CERTIFICATE_CATEGORY attribute
    procedure Add(aCert: CK_CERTIFICATE_CATEGORY); overload;
    /// append a CKA_KEY_TYPE attribute
    procedure Add(aKey: CK_KEY_TYPE); overload;
    /// append a CKA_MECHANISM_TYPE attribute
    procedure Add(aMech: CK_MECHANISM_TYPE); overload;
    /// append another attribute template
    procedure Add(aType: CK_ATTRIBUTE_TYPE; const aAttrib: CK_ATTRIBUTES); overload;

    /// append a void CK_ATTRIBUTE - used e.g. for GetAttributeValue()
    procedure Add(aType: CK_ATTRIBUTE_TYPE); overload;
    /// append several void CK_ATTRIBUTE - used e.g. for GetAttributeValue()
    procedure Add(const aType: array of CK_ATTRIBUTE_TYPE); overload;

    /// reset the CK_ATTRIBUTE list
    procedure Clear;
    /// reset the CK_ATTRIBUTE.pValue/ulValueLen fields, keeping Attr[]._type
    // - to be used e.g. before first GetAttributeValue() call
    procedure ClearValues;
    /// allocate CK_ATTRIBUTE.pValue from ulValueLen size
    // - to be used e.g. after ClearValue then first GetAttributeValue() call
    procedure AllocateValues;

    /// search for a given attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE): CK_ATTRIBUTE_PTR; overload;
    /// search for a given attribute len
    // - returns -1 if not found
    function FindLen(aType: CK_ATTRIBUTE_TYPE): integer;
    /// search for a given CK_ULONG attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE;
      out aValue: CK_ULONG): boolean; overload;
    /// search for a given CK_BBOOL attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE;
      out aValue: boolean): boolean; overload;
    /// search for a given CK_DATE attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE;
      out aValue: TDateTime): boolean; overload;
    /// search for a given binary attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE;
      out aValue: RawByteString): boolean; overload;
    /// search for a given binary attribute value
    function Find(aType: CK_ATTRIBUTE_TYPE;
      out aValue: RawUtf8; aEncodeHex: boolean = false): boolean; overload;

    /// search and compare for a given CK_ULONG attribute value
    function Equals(aType: CK_ATTRIBUTE_TYPE; aValue: CK_ULONG): boolean; overload;
    /// search and compare for a given CK_ULONG attribute value
    function Equals(aType: CK_ATTRIBUTE_TYPE; aValue: boolean): boolean; overload;
    /// search and compare for a given attribute value
    function Equals(aType: CK_ATTRIBUTE_TYPE;
      const aValue: RawByteString): boolean; overload;
  private
    // temp storage to maintain stable pointers for CK_ATTRIBUTE.pValue
    fStoreBin: TRawByteStringDynArray;
    fStoreBinPos, fStoreUlongPos: PtrInt;
    fStoreULong: array[0..31] of CK_ULONG;
    function InternalStore(const aValue: RawByteString): pointer; overload;
    function InternalStore(aValue: CK_ULONG): CK_ULONG_PTR; overload;
  end;
  PCK_ATTRIBUTES = ^CK_ATTRIBUTES;


{ ---------- 3.6 Function types }

type
  /// the CK_RV raw integer value as returned by Cryptoki functions
  CK_RVULONG = type CK_ULONG;

  /// identifies the return value of a Cryptoki function
  // - returned as a CK_RVULONG value but NOT FOLLOWING ord(CK_RV) - use
  // ToULONG/RV() function wrappers for any conversion
  CK_RV = (
    CKR_OK,
    CKR_CANCEL,
    CKR_HOST_MEMORY,
    CKR_SLOT_ID_INVALID,
    CKR_GENERAL_ERROR,
    CKR_FUNCTION_FAILED,
    CKR_ARGUMENTS_BAD,
    CKR_NO_EVENT,
    CKR_NEED_TO_CREATE_THREADS,
    CKR_CANT_LOCK,
    CKR_ATTRIBUTE_READ_ONLY,
    CKR_ATTRIBUTE_SENSITIVE,
    CKR_ATTRIBUTE_TYPE_INVALID,
    CKR_ATTRIBUTE_VALUE_INVALID,
    CKR_ACTION_PROHIBITED,
    CKR_DATA_INVALID,
    CKR_DATA_LEN_RANGE,
    CKR_DEVICE_ERROR,
    CKR_DEVICE_MEMORY,
    CKR_DEVICE_REMOVED,
    CKR_ENCRYPTED_DATA_INVALID,
    CKR_ENCRYPTED_DATA_LEN_RANGE,
    CKR_AEAD_DECRYPT_FAILED,
    CKR_FUNCTION_CANCELED,
    CKR_FUNCTION_NOT_PARALLEL,
    CKR_FUNCTION_NOT_SUPPORTED,
    CKR_KEY_HANDLE_INVALID,
    CKR_KEY_SIZE_RANGE,
    CKR_KEY_TYPE_INCONSISTENT,
    CKR_KEY_NOT_NEEDED,
    CKR_KEY_CHANGED,
    CKR_KEY_NEEDED,
    CKR_KEY_INDIGESTIBLE,
    CKR_KEY_FUNCTION_NOT_PERMITTED,
    CKR_KEY_NOT_WRAPPABLE,
    CKR_KEY_UNEXTRACTABLE,
    CKR_MECHANISM_INVALID,
    CKR_MECHANISM_PARAM_INVALID,
    CKR_OBJECT_HANDLE_INVALID,
    CKR_OPERATION_ACTIVE,
    CKR_OPERATION_NOT_INITIALIZED,
    CKR_PIN_INCORRECT,
    CKR_PIN_INVALID,
    CKR_PIN_LEN_RANGE,
    CKR_PIN_EXPIRED,
    CKR_PIN_LOCKED,
    CKR_SESSION_CLOSED,
    CKR_SESSION_COUNT,
    CKR_SESSION_HANDLE_INVALID,
    CKR_SESSION_PARALLEL_NOT_SUPPORTED,
    CKR_SESSION_READ_ONLY,
    CKR_SESSION_EXISTS,
    CKR_SESSION_READ_ONLY_EXISTS,
    CKR_SESSION_READ_WRITE_SO_EXISTS,
    CKR_SIGNATURE_INVALID,
    CKR_SIGNATURE_LEN_RANGE,
    CKR_TEMPLATE_INCOMPLETE,
    CKR_TEMPLATE_INCONSISTENT,
    CKR_TOKEN_NOT_PRESENT,
    CKR_TOKEN_NOT_RECOGNIZED,
    CKR_TOKEN_WRITE_PROTECTED,
    CKR_UNWRAPPING_KEY_HANDLE_INVALID,
    CKR_UNWRAPPING_KEY_SIZE_RANGE,
    CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT,
    CKR_USER_ALREADY_LOGGED_IN,
    CKR_USER_NOT_LOGGED_IN,
    CKR_USER_PIN_NOT_INITIALIZED,
    CKR_USER_TYPE_INVALID,
    CKR_USER_ANOTHER_ALREADY_LOGGED_IN,
    CKR_USER_TOO_MANY_TYPES,
    CKR_WRAPPED_KEY_INVALID,
    CKR_WRAPPED_KEY_LEN_RANGE,
    CKR_WRAPPING_KEY_HANDLE_INVALID,
    CKR_WRAPPING_KEY_SIZE_RANGE,
    CKR_WRAPPING_KEY_TYPE_INCONSISTENT,
    CKR_RANDOM_SEED_NOT_SUPPORTED,
    CKR_RANDOM_NO_RNG,
    CKR_DOMAIN_PARAMS_INVALID,
    CKR_CURVE_NOT_SUPPORTED,
    CKR_BUFFER_TOO_SMALL,
    CKR_SAVED_STATE_INVALID,
    CKR_INFORMATION_SENSITIVE,
    CKR_STATE_UNSAVEABLE,
    CKR_CRYPTOKI_NOT_INITIALIZED,
    CKR_CRYPTOKI_ALREADY_INITIALIZED,
    CKR_MUTEX_BAD,
    CKR_MUTEX_NOT_LOCKED,
    CKR_NEW_PIN_MODE,
    CKR_NEXT_OTP,
    CKR_EXCEEDED_MAX_ITERATIONS,
    CKR_FIPS_SELF_TEST_FAILED,
    CKR_LIBRARY_LOAD_FAILED,
    CKR_PIN_TOO_WEAK,
    CKR_PUBLIC_KEY_INVALID,
    CKR_FUNCTION_REJECTED,
    CKR_TOKEN_RESOURCE_EXCEEDED,
    CKR_OPERATION_CANCEL_FAILED,
    CKR_VENDOR_DEFINED);

const
  CKR_SUCCESS         = 0;          // = ToULONG(CKR_OK)
  CKR_ABORT           = 1;          // = ToULONG(CKR_CANCEL)
  CKR_NOEVENT         = 8;          // = ToULONG(CKR_NO_EVENT)
  CKR_SENSITIVE       = $0011;      // = ToULONG(CKR_ATTRIBUTE_SENSITIVE)
  CKR_INVALID         = $0012;      // = ToULONG(CKR_ATTRIBUTE_TYPE_INVALID)
  CKR_SIGNINVALID     = $00C0;      // = ToULONG(CKR_SIGNATURE_INVALID)
  CKR_BUFFER_TOOSMALL = $0150;      // = ToULONG(CKR_BUFFER_TOO_SMALL)
  CKR_VENDORDEFINED   = $80000000;  // = ToULONG(CKR_VENDOR_DEFINED)

function ToULONG(rv: CK_RV): CK_RVULONG; overload;
  {$ifdef FPC} inline; {$endif}
function RV(uu: CK_RVULONG): CK_RV;
function ToText(rv: CK_RV): PShortString; overload;

type
  /// types of notifications that Cryptoki provides to an application
  CK_NOTIFICATION = type CK_ULONG;

  /// integer flags type for C_WaitForSlotEvent()
  CKWAIT_FLAGS = type CK_ULONG;

const
  /// CK_NOTIFICATION value indicating that Cryptoki is surrendering the
  // execution of a function executing in a session so that the application may
  // perform other operations
  // - After performing any desired operations, the application should indicate
  // to Cryptoki whether to continue or cancel the function
  CKN_SURRENDER = 0;
  /// CK_NOTIFICATION value indicating that OTP changed
  CKN_OTP_CHANGED = 1;

  /// additionnal CKWAIT_FLAGS for C_WaitForSlotEvent()
  CKF_DONT_BLOCK = 1;

type
  /// signature of a function used by Cryptoki to perform notification callbacks
  // - hSession is the handle of the session performing the callback
  // - event is the type of notification callback
  // - pApplication is an Opaque application-defined value, as was passed to
  // C_OpenSession to open the session performing the callback
  // - should return CKR_SUCCESS (=CKR_OK) or CKR_ABORT (=CKR_CANCEL)
  CK_NOTIFY = function(hSession: CK_SESSION_HANDLE; event: CK_NOTIFICATION;
    pApplication: pointer): CK_RVULONG; cdecl;

  CK_FUNCTION_LIST_PTR = ^CK_FUNCTION_LIST;

  /// function prototype to retrieve the Cryptoki standard API functions
  TfC_GetFunctionList = function(
    out ppFunctionList: CK_FUNCTION_LIST_PTR): CK_RVULONG; cdecl;

  /// the list of all Cryptoki API functions, in its regular 2.x revision
  // - we don't support the extended 3.0 functions yet
  CK_FUNCTION_LIST = record
    /// the version of this function list API
    Version: CK_VERSION;
    /// initializes the Cryptoki library
    // - if pInitArgs is not NULL_PTR, it gets cast to CK_INITIALIZE_ARGS_PTR
    // and dereferenced
    Initialize: function(pInitArgs: pointer): CK_RVULONG; cdecl;
    /// indicates that an application is done with the Cryptoki library
    // - pReserved should be nil
    Finalize: function(pReserved: pointer = nil): CK_RVULONG; cdecl;
    /// returns general information about Cryptoki
    GetInfo: function(out pInfo: CK_INFO): CK_RVULONG; cdecl;
    /// returns the function list
    GetFunctionList: TfC_GetFunctionList;
    /// obtains a list of slots in the system
    GetSlotList: function(tokenPresent: boolean;
      pSlotList: CK_SLOT_ID_PTR; var Count: CK_ULONG): CK_RVULONG; cdecl;
    /// obtains information about a particular slot in the system
    GetSlotInfo: function(
      slotID: CK_SLOT_ID; out pInfo: CK_SLOT_INFO): CK_RVULONG; cdecl;
    /// obtains information about a particular token in the system
    GetTokenInfo: function(
      slotID: CK_SLOT_ID; out pInfo: CK_TOKEN_INFO): CK_RVULONG; cdecl;
    /// obtains a list of mechanism types supported by a token
    GetMechanismList: function(slotID: CK_SLOT_ID;
      pMechanismList: CK_MECHANISM_TYPE_ULONG_PTR; var Count: CK_ULONG): CK_RVULONG; cdecl;
    /// obtains information about a particular mechanism possibly supported by a token
    GetMechanismInfo: function(slotID: CK_SLOT_ID; _type: CK_MECHANISM_TYPE_ULONG;
      out pInfo: CK_MECHANISM_INFO): CK_RVULONG; cdecl;
    /// initializes a token
    // - ulPinLen is the length in bytes of the PIN
    // - pLabel is 32-byte token label (blank padded)
    InitToken: function(slotID: CK_SLOT_ID;
      pPin: PUtf8Char; ulPinLen: CK_ULONG; pLabel: PUtf8Char): CK_RVULONG; cdecl;
    /// InitPIN initializes the normal user's PIN
    InitPIN: function(hSession: CK_SESSION_HANDLE;
      pPin: PUtf8Char; ulPinLen: CK_ULONG): CK_RVULONG; cdecl;
    /// modifies the PIN of the user who is logged in
    SetPIN: function(hSession: CK_SESSION_HANDLE;
      pOldPin: PUtf8Char; ulOldLen: CK_ULONG;
      pNewPin: PUtf8Char; ulNewLen: CK_ULONG): CK_RVULONG; cdecl;
    /// opens a session between an application and a token
    OpenSession: function(slotID: CK_SLOT_ID; flags: CKSE_FLAGS_ULONG;
      pApplication: pointer; Notify: CK_NOTIFY;
      out phSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// closes a session between an application and a token
    CloseSession: function(hSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// closes all sessions with a token
    CloseAllSessions: function(slotID: CK_SLOT_ID): CK_RVULONG; cdecl;
    /// obtains information about the session
    GetSessionInfo: function(hSession: CK_SESSION_HANDLE;
      out pInfo: CK_SESSION_INFO): CK_RVULONG; cdecl;
    /// obtains the state of the cryptographic operation in a session
    GetOperationState: function(hSession: CK_SESSION_HANDLE;
      pOperationState: PByte; var OperationStateLen: CK_ULONG): CK_RVULONG; cdecl;
    /// restores the state of the cryptographic operation in a session
    SetOperationState: function(hSession: CK_SESSION_HANDLE;
      pOperationState: PByte;  ulOperationStateLen: CK_ULONG;
      hEncryptionKey: CK_OBJECT_HANDLE;
      hAuthenticationKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// logs a user into a token
    Login: function(hSession: CK_SESSION_HANDLE; userType: CK_USER_TYPE;
      pPin: PUtf8Char; ulPinLen: CK_ULONG): CK_RVULONG; cdecl;
    /// logs a user out from a token
    Logout: function(hSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// creates a new object
    CreateObject: function(hSession: CK_SESSION_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG;
      out phObject: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// copies an object; creating a new object for the copy
    CopyObject: function(hSession: CK_SESSION_HANDLE;
      hObject: CK_OBJECT_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG;
      out phNewObject: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// destroys an object
    DestroyObject: function(hSession: CK_SESSION_HANDLE;
      hObject: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// gets the size of an object in bytes
    GetObjectSize: function(hSession: CK_SESSION_HANDLE;
      hObject: CK_OBJECT_HANDLE; out pulSize: CK_ULONG): CK_RVULONG; cdecl;
    /// obtains the value of one or more object attributes
    GetAttributeValue: function(hSession: CK_SESSION_HANDLE;
      hObject: CK_OBJECT_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG): CK_RVULONG; cdecl;
    /// modifies the value of one or more object attributes
    SetAttributeValue: function(hSession: CK_SESSION_HANDLE;
      hObject: CK_OBJECT_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a search for token and session objects that match a template
    FindObjectsInit: function(hSession: CK_SESSION_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a search for token and session objects that match a template
    // - obtaining additional object handles
    FindObjects: function(hSession: CK_SESSION_HANDLE;
      phObject: CK_OBJECT_HANDLE_PTR; ulMaxObjectCount: CK_ULONG;
      out pulObjectCount: CK_ULONG): CK_RVULONG; cdecl;
    /// finishes a search for token and session objects
    FindObjectsFinal: function(hSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// initializes an encryption operation
    EncryptInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM; hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// encrypts single-part data
    Encrypt: function(hSession: CK_SESSION_HANDLE;
      pData: PByte; ulDataLen: CK_ULONG;
      pEncryptedData: PByte; var EncryptedDataLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part encryption operation
    EncryptUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG;
      pEncryptedPart: PByte; var EncryptedPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// finishes a multiple-part encryption operation
    EncryptFinal: function(hSession: CK_SESSION_HANDLE;
      pLastEncryptedPart: PByte; var LastEncryptedPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a decryption operation
    DecryptInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM; hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// decrypts encrypted data in a single call
    Decrypt: function(hSession: CK_SESSION_HANDLE;
      pEncryptedData: PByte; ulEncryptedDataLen: CK_ULONG;
      pData: PByte; var DataLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part decryption operation
    DecryptUpdate: function(hSession: CK_SESSION_HANDLE;
      pEncryptedPart: PByte; ulEncryptedPartLen: CK_ULONG;
      pPart: PByte; var PartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// finishes a multiple-part decryption operation
    DecryptFinal: function(hSession: CK_SESSION_HANDLE;
      pLastPart: PByte; var LastPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a message-digesting operation
    DigestInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM): CK_RVULONG; cdecl;
    /// digests data in a single call
    Digest: function(hSession: CK_SESSION_HANDLE;
      pData: PByte; ulDataLen: CK_ULONG;
      pDigest: PByte; var pulDigestLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part message-digesting operation
    DigestUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multi-part message-digesting operation
    // - by digesting the value of a secret key as part of the data already digested
    DigestKey: function(hSession: CK_SESSION_HANDLE;
      hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// finishes a multiple-part message-digesting operation
    DigestFinal: function(hSession: CK_SESSION_HANDLE;
      pDigest: PByte; var pulDigestLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a signature (private key encryption operation)
    // -  where the signature is (will be) an appendix to the data;
    // - and plaintext cannot be recovered from the signature
    SignInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;  hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// signs (encrypts with private key) data in a single call
    // - where the signature is (will be) an appendix to the data;
    // - and plaintext cannot be recovered from the signature
    Sign: function(hSession: CK_SESSION_HANDLE;
      pData: PByte; ulDataLen: CK_ULONG;
      pSignature: PByte; var SignatureLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part signature operation
    // -  where the signature is (will be) an appendix to the data,
    // and plaintext cannot be recovered from the signature
    SignUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// finishes a multiple-part signature operation, returning the signature
    SignFinal: function(hSession: CK_SESSION_HANDLE;
      pSignature: PByte; var SignatureLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a signature operation
    // - where the data can be recovered from the signature
    SignRecoverInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM; hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// signs data in a single operation
    // - where the data can be recovered from the signature
    SignRecover: function(hSession: CK_SESSION_HANDLE;
      pData: PByte; ulDataLen: CK_ULONG;
      pSignature: PByte; out SignatureLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a verification operation
    // - where the signature is an appendix to the data; and plaintext cannot
    // be recovered from the signature (e.g. DSA)
    VerifyInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM; hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// verifies a signature in a single-part operation
    // - where the signature is an appendix to the data; and plaintext
    // cannot be recovered from the signature
    Verify: function(hSession: CK_SESSION_HANDLE;
      pData: PByte; ulDataLen: CK_ULONG;
      pSignature: PByte; ulSignatureLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part verification operation
    // - where the signature is an appendix to the data,
    // and plaintext cannot be recovered from the signature
    VerifyUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// finishes a multiple-part verification operation; checking the signature
    VerifyFinal: function(hSession: CK_SESSION_HANDLE;
      pSignature: PByte; ulSignatureLen: CK_ULONG): CK_RVULONG; cdecl;
    /// initializes a signature verification operation
    // - where the data is recovered from the signature
    VerifyRecoverInit: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      hKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// verifies a signature in a single-part operation
    // -  where the data is recovered from the signature
    VerifyRecover: function(hSession: CK_SESSION_HANDLE;
      pSignature: PByte; ulSignatureLen: CK_ULONG;
      pData: PByte; var DataLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part digesting and encryption operation
    DigestEncryptUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG;
      pEncryptedPart: PByte; var EncryptedPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part decryption and digesting operation
    DecryptDigestUpdate: function(hSession: CK_SESSION_HANDLE;
      pEncryptedPart: PByte; ulEncryptedPartLen: CK_ULONG;
      pPart: PByte; var PartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part signing and encryption operation
    SignEncryptUpdate: function(hSession: CK_SESSION_HANDLE;
      pPart: PByte; ulPartLen: CK_ULONG;
      pEncryptedPart: PByte; var EncryptedPartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// continues a multiple-part decryption and verify operation
    DecryptVerifyUpdate: function(hSession: CK_SESSION_HANDLE;
      pEncryptedPart: PByte; ulEncryptedPartLen: CK_ULONG;
      pPart: PByte; var PartLen: CK_ULONG): CK_RVULONG; cdecl;
    /// generates a secret key; creating a new key object
    GenerateKey: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      pTemplate: CK_ATTRIBUTE_PTR; ulCount: CK_ULONG;
      out phKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// generates a public-key/private-key pair, creating new key objects
    GenerateKeyPair: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      pPublicKeyTemplate: CK_ATTRIBUTE_PTR;  ulPublicKeyAttributeCount: CK_ULONG;
      pPrivateKeyTemplate: CK_ATTRIBUTE_PTR; ulPrivateKeyAttributeCount: CK_ULONG;
      out phPublicKeyn, phPrivateKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// wraps (i.e. encrypts) a key
    WrapKey: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      hWrappingKey: CK_OBJECT_HANDLE; hKey: CK_OBJECT_HANDLE;
      pWrappedKey: PByte; var WrappedKeyLen: CK_ULONG): CK_RVULONG; cdecl;
    /// unwraps (decrypts) a wrapped key; creating a new key object
    UnwrapKey: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      hUnwrappingKey: CK_OBJECT_HANDLE;
      pWrappedKey: PByte; ulWrappedKeyLen: CK_ULONG;
      pTemplate: CK_ATTRIBUTE_PTR; ulAttributeCount: CK_ULONG;
      out phKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// derives a key from a base key; creating a new key object
    DeriveKey: function(hSession: CK_SESSION_HANDLE;
      var pMechanism: CK_MECHANISM;
      hBaseKey: CK_OBJECT_HANDLE;
      pTemplate: CK_ATTRIBUTE_PTR; ulAttributeCount: CK_ULONG;
      out phKey: CK_OBJECT_HANDLE): CK_RVULONG; cdecl;
    /// mixes additional seed material into the token's random number generator
    SeedRandom: function(hSession: CK_SESSION_HANDLE;
      pSeed: PByte; ulSeedLen: CK_ULONG): CK_RVULONG; cdecl;
    /// generates random data
    GenerateRandom: function(hSession: CK_SESSION_HANDLE;
      RandomData: PByte; ulRandomLen: CK_ULONG): CK_RVULONG; cdecl;
    /// legacy function; it obtains an updated status of a function running
    // in parallel with an application.
    GetFunctionStatus: function(hSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// is a legacy function; it cancels a function running in parallel
    CancelFunction: function(hSession: CK_SESSION_HANDLE): CK_RVULONG; cdecl;
    /// waits for a slot event (token insertion, removal; etc.) to occur
    // - added for Cryptoki Version 2.01
    // - flags could be 0 (blocking) or CKF_DONT_BLOCK (non blocking)
    WaitForSlotEvent: function(flags: CKWAIT_FLAGS;
      out pSlot: CK_SLOT_ID; pReserved: pointer = nil): CK_RVULONG; cdecl;
  end;

{ ------------ 3.7 Locking-related types }

type
  /// abstract pointer which identifies a mutex object
  CK_MUTEX = pointer;

  /// application-supplied function which creates a new mutex object
  // - it should return either CKR_OK, CKR_GENERAL_ERROR or CKR_HOST_MEMORY
  CK_CREATEMUTEX = function(out ppMutex: CK_MUTEX): CK_RV;

  /// application-supplied function which destroys an existing mutex object
  // - it should return either CKR_OK, CKR_GENERAL_ERROR, CKR_HOST_MEMORY
  // or CKR_MUTEX_BAD
  CK_DESTROYMUTEX = function(ppMutex: CK_MUTEX): CK_RV;

  /// application-supplied function which locks an existing mutex object
  // - it should return either CKR_OK, CKR_GENERAL_ERROR, CKR_HOST_MEMORY
  // or CKR_MUTEX_BAD
  CK_LOCKMUTEX = function(ppMutex: CK_MUTEX): CK_RV;

  /// application-supplied function which unlocks an existing mutex object
  // - it should return either CKR_OK, CKR_GENERAL_ERROR, CKR_HOST_MEMORY,
  // CKR_MUTEX_BAD or CKR_MUTEX_NOT_LOCKED
  CK_UNLOCKMUTEX = function(ppMutex: CK_MUTEX): CK_RV;

  /// CK_C_INITIALIZE_ARGS options
  CKI_FLAG = (
    CKF_LIBRARY_CANT_CREATE_OS_THREADS,
    CKF_OS_LOCKING_OK
  );
  /// CK_C_INITIALIZE_ARGS set of options
  // - stored as CKI_FLAGS_ULONG integer value
  CKI_FLAGS = set of CKI_FLAG;

  /// how CKI_FLAGS are stored in CK_C_INITIALIZE_ARGS
  CKI_FLAGS_ULONG = type CK_ULONG;

  //// provides the optional arguments to C_Initialize
  CK_C_INITIALIZE_ARGS = record
    /// callback to create a new mutex object
    CreateMutex: CK_CREATEMUTEX;
    /// callback to destroy a mutex
    DestroyMutex: CK_DESTROYMUTEX;
    /// callback to lock a mutex
    LockMutex: CK_LOCKMUTEX;
    /// callback to unlock a mutex
    UnlockMutex: CK_UNLOCKMUTEX;
    /// bit flags for specifying options - map CKI_FLAGS
    flags: CKI_FLAGS_ULONG;
    /// reserved for future use - should be nil for this version of Cryptoki
    pReserved: pointer;
  end;


// TO BE DONE (if really needed):
//  - CK_FUNCTION_LIST_3_0


{ ***************** PKCS#11 High-Level Wrappers }

{$A+} // back to normal alignment
{$Z1} // back to normal enum size


type
  /// exception class raised during TPkcs11 process
  EPkcs11 = class(ESynException);

  /// map CK_SLOT_ID but with a fixed 32-bit size
  // - 32-bit is enough, and raw CK_SLOT_ID may be 64-bit
  TPkcs11SlotID = cardinal;

  /// map several CK_SLOT_ID but with a fixed 32-bit size
  TPkcs11SlotIDDynArray = array of TPkcs11SlotID;

  /// high-level information about a PKCS#11 mechanism
  TPkcs11Mechanism = packed record
    /// the type of Mechanism
    Kind: CK_MECHANISM_TYPE;
    /// minimum size of the Mechanism key (in bits or bytes, or even meaningless)
    MinKey: cardinal;
    /// maximum size of the Mechanism key (in bits or bytes, or even meaningless)
    MaxKey: cardinal;
    /// define the Mechanism behavior
    Flags: CKM_FLAGS;
  end;
  /// high-level information about one or several PKCS#11 mechanism(s)
  TPkcs11Mechanisms = array of TPkcs11Mechanism;

  /// high-level information about a PKCS#11 Slot
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11Slot = packed record
    /// the internal ID of this slot
    Slot: TPkcs11SlotID;
    /// the description of the Slot
    Description: RawUtf8;
    /// the manufacturer of the Slot
    Manufacturer: RawUtf8;
    /// Slot Information Flags
    Flags: CKSL_FLAGS;
    /// the Mechanism supported by this Slot
    Mechanism: TPkcs11Mechanisms;
    /// version number of the slot's hardware
    Hardware: CK_VERSION;
    /// version number of the slot's firmware
    Firmware: CK_VERSION;
  end;
  /// pointer to high-level information about a PKCS#11 Slot
  PPkcs11Slot = ^TPkcs11Slot;

  /// high-level information about several PKCS#11 Slots
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11SlotDynArray = array of TPkcs11Slot;

  /// the flags of PKCS#11 one Storage Object
  // - posToken .. posTrusted map CKA_TOKEN ... CKA_TRUSTED boolean attributes
  // - posX509 .. posWtls map CKA_CERTIFICATE_TYPE attribute value
  TPkcs11ObjectStorage = (
    posToken,
    posPrivate,
    posSensitive,
    posModifiable,
    posCopiable,
    posDestroyable,
    posExtractable,
    posEncrypt,
    posDecrypt,
    posVerify,
    posVerifyRecover,
    posSign,
    posSignRecover,
    posWrap,
    posUnWrap,
    posDerive,
    posTrusted,
    posX509,
    posX509Attr,
    posWtls);

  /// set of flags for PKCS#11 Storage Object
  // - see AddToAttributes() wrapper function to convert them to attributes
  TPkcs11ObjectStorages = set of TPkcs11ObjectStorage;

  /// high-level information about one PKCS#11 Object
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11Object = packed record
    /// the class of the object (from CKA_CLASS)
    ObjClass: CK_OBJECT_CLASS;
    /// the identifier of this Storage Object (from CKA_ID), as hexadecimal
    StorageID: RawUtf8;
    /// the description of this Storage Object (from CKA_LABEL value)
    StorageLabel: RawUtf8;
    /// the flags of a Storage Object (from various CKA_* values)
    StorageFlags: TPkcs11ObjectStorages;
    /// the stored Key Type (from CKA_KEY_TYPE value)
    KeyType: CK_KEY_TYPE;
    /// how this stored Key has been generated (from CKA_KEY_GEN_MECHANISM)
    KeyGen: CK_MECHANISM_TYPE;
    /// the size of the object key in bits
    // - e.g. from CKA_MODULUS_BITS (for CKK_RSA) or CKA_EC_POINT (for CKK_EC)
    // - may be 0 for not-so-used/unsupported algorithms
    // - note that CKA_VALUE_LEN is likely to be not present on most HW
    KeyBits: cardinal;
    /// start date of this Storage Object (from CKA_START_DATE)
    Start: TDateTime;
    /// end date of this Storage Object (from CKA_END_DATE)
    Stop: TDateTime;
    /// the application name of this Storage Object (from CKA_APPLICATION)
    Application: RawUtf8;
    /// the DER subject of this Storage Object (from CKA_SUBJECT or CKA_OWNER)
    Subject: RawByteString;
    /// the DER serial number of this Storage Object (from CKA_SERIAL_NUMBER)
    Serial: RawByteString;
    /// the DER issuer of this Storage Object (from CKA_ISSUER)
    Issuer: RawByteString;
    /// the DER unique ID of this Certificate (from CKA_UNIQUE_ID)
    UniqueID: RawByteString;
    /// the low-level CK_OBJECT_HANDLE, which lifetime would match the session
    // - not defined as CK_OBJECT_HANDLE because this type is not cross-platform
    SessionHandle: cardinal;
  end;
  /// high-level information about several PKCS#11 Objects
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11ObjectDynArray = array of TPkcs11Object;

  /// high-level information about a PKCS#11 Token
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11Token = packed record
    /// the internal slot ID of this token
    Slot: TPkcs11SlotID;
    /// application-defined label
    Name: RawUtf8;
    /// token manufacturer
    Manufacturer: RawUtf8;
    /// token model
    Model: RawUtf8;
    /// token Serial Number
    Serial: RawUtf8;
    /// current UTC time - if CKF_CLOCK_ON_TOKEN flag is set
    Time: RawUtf8;
    /// information flags
    Flags: CKT_FLAGS;
    /// current number of sessions opened with this token
    Sessions: integer;
    /// how many sessions can be opened with this token
    // - may be -1 if unknown, 0 if without any limit
    MaxSessions: integer;
    /// minimum length of the PIN
    MinPin: integer;
    /// maximum length of the PIN
    MaxPin: integer;
  end;
  PPkcs11Token = ^TPkcs11Token;

  /// high-level information about several PKCS#11 Tokens
  // - can be (un) serialized as binary or JSON if needed
  TPkcs11TokenDynArray = array of TPkcs11Token;

const
  /// value return when TPkcs11 process return no actual slot value
  PKCS11_NOSLOT = TPkcs11SlotID(-1);

/// convert a raw CK_TOKEN_INFO into a high-level TPkcs11Token
// - as used by TPkcs11.GetSlots
procedure FillToken(ID: CK_SLOT_ID; const Raw: CK_TOKEN_INFO;
  out Token: TPkcs11Token);

/// convert a raw CK_SLOT_INFO into a high-level TPkcs11Slot
// - as used by TPkcs11.GetSlots
procedure FillSlot(ID: CK_SLOT_ID; const Raw: CK_SLOT_INFO;
  out Slot: TPkcs11Slot);

/// append some CK_ATTRIBUTES boolean fields from a TPkcs11ObjectStorages set
procedure AddToAttributes(var Attr: CK_ATTRIBUTES; Flags: TPkcs11ObjectStorages);

/// the default TPkcs11ObjectStorages used for most known key types generation
// - returns [] if not known enough, or the appropriate flags
function DefaultKeyStorageFlags(kt: CK_KEY_TYPE): TPkcs11ObjectStorages;

/// compute the ECC bits (e.g. 256) from the CKA_EC_POINT attribute length
function EccBitsFromPointLen(bytes: integer; out bits: cardinal): boolean;

type
  TPkcs11 = class;

  /// a callback called during session long-process
  // - return false to continue, or true to abort the process
  TOnPkcs11Notify = function(Sender: TPkcs11; Slot: TPkcs11SlotID): boolean;

  /// can load and use a PKCS#11 library
  // - need to explicitely call Safe.Lock/UnLock if you need in a multi-thread usage
  TPkcs11 = class(TSynLocked)
  protected
    fC: CK_FUNCTION_LIST_PTR;
    fHandle: TLibHandle;
    fLibraryName: TFileName;
    fApi, fVersion, fManufacturer, fDescription: RawUtf8;
    fSlots: TPkcs11SlotDynArray;
    fSlotIDs: TPkcs11SlotIDDynArray;
    fTokens: TPkcs11TokenDynArray;
    fApiNum, fVersionNum: CK_VERSION;
    fSession: CK_SESSION_HANDLE;
    fSessionSlot: TPkcs11SlotID;
    fSessionFlags: set of (sfRW, sfLogIn);
    fOnNotify: TOnPkcs11Notify;
    procedure EnsureLoaded(const ctxt: ShortString);
    procedure EnsureSession(const ctxt: ShortString);
    procedure Check(res: CK_RVULONG; const ctxt: ShortString;
      unlock: boolean = false);
    procedure CheckAttr(res: CK_RVULONG);
    // some actions within the current opened session
    function SessionCreateObject(const a: CK_ATTRIBUTES): RawUtf8;
    function SessionGetAttribute(obj: CK_OBJECT_HANDLE;
      attr: CK_ATTRIBUTE_TYPE): RawUtf8;
  public
    /// try to load a PKCS#11 library, raising EPkcs11 on failure
    // - is just a wrapper around inherited Create and Load() + raise EPkcs11
    // - note that Load() could wait several seconds  - use plain Create for
    // a lazy/quick initialization, then make "if not Loaded then Load(...)"
    constructor Create(const aLibraryName: TFileName); reintroduce; overload;
    /// finalize this instance
    destructor Destroy; override;
    /// try to load a PKCS#11 library, returning true on success
    // - this method takes several seconds, because it will connect to the
    // actual peripheral, and communicate with it to retrieve its information
    function Load(const aLibraryName: TFileName): boolean;
    /// unload a PKCS#11 previously loaded library
    procedure UnLoad;
    /// returns true if a previous Create(aFileName) or Load() was successfull
    function Loaded: boolean;
      {$ifdef HASINLINE} inline; {$endif}

    /// get information about this instance in Slots[] and Tokens[] properties
    procedure RetrieveConfig(IncludeVoidSlots: boolean = false);
    /// update information in Slots[] and Tokens[] about a single slot
    // - as called e.g. by RetrieveConfig() and also function WaitForSlotEvent()
    procedure UpdateConfig(SlotID: TPkcs11SlotID);
    /// wait for a Slot state to change, e.g. a credential to be inserted
    // - if NotBlocking is default false, will wait for a change and always
    // return true the changed Slot ID, or raise an EPkcs11 on error - note
    // that the OnNotify event is sadly not triggerred by this Cryptoki API
    // - if NotBlocking is true, return PKCS11_NOSLOT if no state changed, or
    // the changed Slot ID
    // - if a Slot ID is returned, will call UpdateConfig() to update Slots[]
    // and Tokens[] so you can e.g. call SlotByID(Slot) to get information
    // - this method process is protected via Safe.Lock/Unlock
    function WaitForSlotEvent(NotBlocking: boolean = false): TPkcs11SlotID;
    /// search for a given TPkcs11Slot.Slot within current Slots[]
    // - returns nil if no slot was found, or add a new entry if AddNew is set
    // - not thread-safe: use Safe.Lock/UnLock when you are outside a session
    function SlotByID(SlotID: TPkcs11SlotID; AddNew: boolean = false): PPkcs11Slot;
    /// search for a given TPkcs11Token.Slot within current Token[]
    // - returns nil if no token was found, or add a new entry if AddNew is set
    // - not thread-safe: use Safe.Lock/UnLock when you are outside a session
    function TokenByID(SlotID: TPkcs11SlotID; AddNew: boolean = false): PPkcs11Token;
    /// search for a given TPkcs11Token.Name within current Tokens[]
    // - returns nil if the token was not found
    // - not thread-safe: use Safe.Lock/UnLock when you are outside a session
    function TokenByName(const TokenName: RawUtf8;
      CaseInsensitive: boolean = false): PPkcs11Token;

    /// enter public session by Slot ID, R/O by default
    // - only a single session can be opened at once in a TPkcs11 instance
    // - raise EPkcs11 on error, or set Safe.Lock on success: caller should
    // always use a "Open(...); try ... finally Close end" pattern
    procedure Open(slot: TPkcs11SlotID; rw: boolean = false); overload;
    /// enter user session by Slot ID, R/O and for a non-Supervisor user by default
    // - only a single session can be opened at once in a TPkcs11 instance
    // - raise EPkcs11 on error, or set Safe.Lock on success: caller should
    // always use a "Open(...); try ... finally Close end" pattern
    procedure Open(slot: TPkcs11SlotID; const pin: RawUtf8;
      rw: boolean = false; so: boolean = false); overload;
    /// enter public session by Token name, R/O by default
    // - only a single session can be opened at once in a TPkcs11 instance
    // - return nil if the name was not found, would raise EPkcs11 on error, or
    // make Safe.Lock and return the token on success - caller should use e.g.
    // ! if Open('somename') <> nil then try ... finally Close end;
    function Open(const name: RawUtf8; rw: boolean = false;
      namecaseins: boolean = false): PPkcs11Token; overload;
    /// enter user session by Token name, R/O by default
    // - only a single session can be opened at once in a TPkcs11 instance
    // - return nil if the name was not found, would raise EPkcs11 on error, or
    // make Safe.Lock and return the token on success - caller should use e.g.
    // ! if Open('somename') <> nil then try ... finally Close end;
    function Open(const name: RawUtf8; const pin: RawUtf8; rw: boolean = false;
      so: boolean = false; namecaseins: boolean = false): PPkcs11Token; overload;
    /// retrieve information about all objects from the current Session
    // - should have called Open() then call Close() once done
    // - can optionally retrieve the whole object Value as RawByteString arrays
    function GetObjects(Filter: PCK_ATTRIBUTES = nil;
      Values: PRawByteStringDynArray = nil): TPkcs11ObjectDynArray;
    /// retrieve one object by class type and label/ID from current Session
    // - should have called Open() then call Close() once done
    // - ObjectClass can be CKO_CERTIFICATE for a X509 certificate, CKO_PUBLIC_KEY
    // or CKO_PRIVATE_KEY for asymmetric keys, and CKO_SECRET_KEY for some
    // symmetric secret, to be used e.g. for AES encryption
    // - can optionally retrieve the whole object Value as RawByteString, if it
    // is allowed by the object itself (e.g. posExtractable key)
    // - return false if there is no such object in this Session
    // - return true and fill Info with the found Object information on success
    function GetObject(ObjectClass: CK_OBJECT_CLASS; out Info: TPkcs11Object;
      const StorageLabel: RawUtf8 = ''; const StorageID: RawUtf8 = '';
      Value: PRawByteString = nil): boolean; overload;
    /// retrieve one object handle by class type and label/ID from current Session
    // - should have called Open() then call Close() once done
    // - ObjectClass can be CKO_CERTIFICATE for a X509 certificate, CKO_PUBLIC_KEY
    // or CKO_PRIVATE_KEY for asymmetric keys, and CKO_SECRET_KEY for some
    // symmetric secret, to be used e.g. for AES encryption
    // - return CK_INVALID_HANDLE (=0) if there is no such object in this Session
    // - return the matching CK_OBJECT_HANDLE, which lifetime is the Session
    function GetObject(ObjectClass: CK_OBJECT_CLASS; const StorageLabel: RawUtf8 = '';
      const StorageID: RawUtf8 = ''): CK_OBJECT_HANDLE; overload;
    /// retrieve some random bytes using the device opened in the current Session
    function GetRandom(Len: PtrInt): RawByteString;
    /// digitally sign a memory buffer using a supplied Private Key
    // - you must supply a mechanism - method won't setup any default parameter
    // - return the signature as a binary blob
    function Sign(Data: pointer; Len: PtrInt; PrivKey: CK_OBJECT_HANDLE;
      var Mechanism: CK_MECHANISM): RawByteString;
    /// digitally verify a memory buffer signature using a supplied Public Key
    // - you must supply a mechanism - method won't setup any default parameter
    // - raise an EPkcs11 exception on error
    // - some HW (e.g. OpenSC Nitrokey) does not allow to verify using the
    // device: you need to extract the key and verify the signature in software
    function Verify(Data, Sig: pointer; DataLen, SigLen: PtrInt;
      PubKey: CK_OBJECT_HANDLE; var Mechanism: CK_MECHANISM): boolean;
    /// store a CKO_DATA object using the current R/W Session
    // - return the CKA_UNIQUE_ID generated by the token, or raise EPkcs11
    function AddSessionData(const Application, DataLabel: RawUtf8;
      const Data: RawByteString; const DerID: RawByteString = ''): RawUtf8;

    /// release a session previously created with Open() overloads
    // - will release the lock with Safe.UnLock
    // - do nothing if no session did actually began with a former Open()
    procedure Close;

    /// initialize a Token on slot #SlotID
    // - if the token has not been initialized (i.e. new from the factory), then
    // the SOPin parameter becomes the initial value of the SO PIN
    // - if the token is being reinitialized, the SOPin parameter is checked
    // against the existing SO PIN to authorize the initialization operation
    // - When a token is initialized, all objects that can be destroyed are
    // destroyed, and access by the normal user is disabled until the SO sets
    // the normal user PIN
    procedure InitToken(SlotID: TPkcs11SlotID; const SOPin, TokenLabel: RawUtf8);
    /// initialize an User PIN for the Token on slot #SlotID
    procedure InitUserPin(SlotID: TPkcs11SlotID; const SOPin, UserPin: RawUtf8);
    /// change the User PIN for the Token on slot #SlotID
    procedure ChangeUserPin(SlotID: TPkcs11SlotID; const OldPin, NewPin: RawUtf8);

    /// low-level numerical version of the loaded library
    property VersionNum: CK_VERSION
      read fVersionNum;
    /// low-level numerical version of the loaded library API
    property ApiNum: CK_VERSION
      read fApiNum;
    /// event called during any long-term process, to notify the user
    // - the callback could return TRUE to abort the process
    property OnNotify: TOnPkcs11Notify
      read fOnNotify write fOnNotify;
    /// access to the internal Cryptoki API raw functions, once loaded
    property C: CK_FUNCTION_LIST_PTR
      read fC;
  published
    /// the library name after a successful Load() call
    property LibraryName: TFileName
      read fLibraryName;
    /// the ready-to-be displayed version of the Cryptoki API
    property Api: RawUtf8
      read fApi;
    /// the manufacturer of this Cryptoky library
    property Manufacturer: RawUtf8
      read fManufacturer;
    /// the description of this Cryptoky library
    property Description: RawUtf8
      read fDescription;
    /// the ready-to-be displayed version of the loaded library
    property Version: RawUtf8
      read fVersion;
    /// the information of the known slots, as retrieved by RetrieveConfig
    property Slots: TPkcs11SlotDynArray
      read fSlots;
    /// the TPkcs11SlotID of the known slots, as retrieved by RetrieveConfig
    property SlotIDs: TPkcs11SlotIDDynArray
      read fSlotIDs;
    /// the information of the known tokens, as retrieved by RetrieveConfig
    property Tokens: TPkcs11TokenDynArray
      read fTokens;
  end;


implementation

{ ***************** Low-Level PKCS#11 / Cryptoki API Definitions }

function ToText(f: CKT_FLAGS): shortstring;
begin
  GetSetNameShort(TypeInfo(CKT_FLAGS), f, result);
end;

function ToText(oc: CK_OBJECT_CLASS): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_OBJECT_CLASS), ord(oc));
end;

function ToULONG(oc: CK_OBJECT_CLASS): CK_ULONG;
begin
  if oc = CKO_VENDOR_DEFINED then
    result := CKO_VENDOR_DEFINED_ULONG
  else
    result := ord(oc);
end;

function OBJECT_CLASS(uu: CK_ULONG): CK_OBJECT_CLASS;
begin
  if uu >= CK_ULONG(CKO_VENDOR_DEFINED) then
    result := CKO_VENDOR_DEFINED
  else
    result := CK_OBJECT_CLASS(uu);
end;

function ToText(hw: CK_HW_FEATURE_TYPE): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_HW_FEATURE_TYPE), ord(hw));
end;

function ToULONG(hw: CK_HW_FEATURE_TYPE): CK_ULONG;
begin
  if hw = CKH_VENDOR_DEFINED then
    result := CKH_VENDOR_DEFINED_ULONG
  else
    result := ord(hw);
end;

function HW_FEATURE_TYPE(uu: CK_ULONG): CK_HW_FEATURE_TYPE;
begin
  if uu >= CK_ULONG(CKH_VENDOR_DEFINED) then
    result := CKH_VENDOR_DEFINED
  else
    result := CK_HW_FEATURE_TYPE(uu);
end;

function ToText(kt: CK_KEY_TYPE): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_KEY_TYPE), ord(kt));
end;

function ToULONG(kt: CK_KEY_TYPE): CK_ULONG;
begin
  if kt = CKK_VENDOR_DEFINED then
    result := CKK_VENDOR_DEFINED_ULONG
  else
    result := ord(kt);
end;

function KEY_TYPE(uu: CK_ULONG): CK_KEY_TYPE;
begin
  if uu >= CK_ULONG(CKK_VENDOR_DEFINED) then
    result := CKK_VENDOR_DEFINED
  else
    result := CK_KEY_TYPE(uu);
end;

function ToText(ct: CK_CERTIFICATE_TYPE): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_CERTIFICATE_TYPE), ord(ct));
end;

function ToULONG(ct: CK_CERTIFICATE_TYPE): CK_ULONG;
begin
  if ct = CKC_VENDOR_DEFINED then
    result := CKC_VENDOR_DEFINED_ULONG
  else
    result := ord(ct);
end;

function CERTIFICATE_TYPE(uu: CK_ULONG): CK_CERTIFICATE_TYPE;
begin
  if uu >= CK_ULONG(CKC_VENDOR_DEFINED) then
    result := CKC_VENDOR_DEFINED
  else
    result := CK_CERTIFICATE_TYPE(uu);
end;

function ToText(at: CK_ATTRIBUTE_TYPE): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_ATTRIBUTE_TYPE), ord(at));
end;

const
  CKA_ULONG: array[CK_ATTRIBUTE_TYPE] of cardinal = (
   $00000000,                         // CKA_CLASS
   $00000001,                         // CKA_TOKEN
   $00000002,                         // CKA_PRIVATE
   $00000003,                         // CKA_LABEL
   $00000004,                         // CKA_UNIQUE_ID
   $00000010,                         // CKA_APPLICATION
   $00000011,                         // CKA_VALUE
   $00000012,                         // CKA_OBJECT_ID
   $00000080,                         // CKA_CERTIFICATE_TYPE
   $00000081,                         // CKA_ISSUER
   $00000082,                         // CKA_SERIAL_NUMBER
   $00000083,                         // CKA_AC_ISSUER
   $00000084,                         // CKA_OWNER
   $00000085,                         // CKA_ATTR_TYPES
   $00000086,                         // CKA_TRUSTED
   $00000087,                         // CKA_CERTIFICATE_CATEGORY
   $00000088,                         // CKA_JAVA_MIDP_SECURITY_DOMAIN
   $00000089,                         // CKA_URL
   $0000008A,                         // CKA_HASH_OF_SUBJECT_PUBLIC_KEY
   $0000008B,                         // CKA_HASH_OF_ISSUER_PUBLIC_KEY
   $0000008C,                         // CKA_NAME_HASH_ALGORITHM
   $00000090,                         // CKA_CHECK_VALUE
   $00000100,                         // CKA_KEY_TYPE
   $00000101,                         // CKA_SUBJECT
   $00000102,                         // CKA_ID
   $00000103,                         // CKA_SENSITIVE
   $00000104,                         // CKA_ENCRYPT
   $00000105,                         // CKA_DECRYPT
   $00000106,                         // CKA_WRAP
   $00000107,                         // CKA_UNWRAP
   $00000108,                         // CKA_SIGN
   $00000109,                         // CKA_SIGN_RECOVER
   $0000010A,                         // CKA_VERIFY
   $0000010B,                         // CKA_VERIFY_RECOVER
   $0000010C,                         // CKA_DERIVE
   $00000110,                         // CKA_START_DATE
   $00000111,                         // CKA_END_DATE
   $00000120,                         // CKA_MODULUS
   $00000121,                         // CKA_MODULUS_BITS
   $00000122,                         // CKA_PUBLIC_EXPONENT
   $00000123,                         // CKA_PRIVATE_EXPONENT
   $00000124,                         // CKA_PRIME_1
   $00000125,                         // CKA_PRIME_2
   $00000126,                         // CKA_EXPONENT_1
   $00000127,                         // CKA_EXPONENT_2
   $00000128,                         // CKA_COEFFICIENT
   $00000129,                         // CKA_PUBLIC_KEY_INFO
   $00000130,                         // CKA_PRIME
   $00000131,                         // CKA_SUBPRIME
   $00000132,                         // CKA_BASE
   $00000133,                         // CKA_PRIME_BITS
   $00000134,                         // CKA_SUBPRIME_BITS
   $00000160,                         // CKA_VALUE_BITS
   $00000161,                         // CKA_VALUE_LEN
   $00000162,                         // CKA_EXTRACTABLE
   $00000163,                         // CKA_LOCAL
   $00000164,                         // CKA_NEVER_EXTRACTABLE
   $00000165,                         // CKA_ALWAYS_SENSITIVE
   $00000166,                         // CKA_KEY_GEN_MECHANISM
   $00000170,                         // CKA_MODIFIABLE
   $00000171,                         // CKA_COPYABLE
   $00000172,                         // CKA_DESTROYABLE
   $00000180,                         // CKA_EC_PARAMS
   $00000181,                         // CKA_EC_POINT
   $00000202,                         // CKA_ALWAYS_AUTHENTICATE
   $00000210,                         // CKA_WRAP_WITH_TRUSTED
   CKF_ARRAY_ATTRIBUTE or $00000211,  // CKA_WRAP_TEMPLATE
   CKF_ARRAY_ATTRIBUTE or $00000212,  // CKA_UNWRAP_TEMPLATE
   CKF_ARRAY_ATTRIBUTE or $00000213,  // CKA_DERIVE_TEMPLATE
   $00000220,                         // CKA_OTP_FORMAT
   $00000221,                         // CKA_OTP_LENGTH
   $00000222,                         // CKA_OTP_TIME_INTERVAL
   $00000223,                         // CKA_OTP_USER_FRIENDLY_MODE
   $00000224,                         // CKA_OTP_CHALLENGE_REQUIREMENT
   $00000225,                         // CKA_OTP_TIME_REQUIREMENT
   $00000226,                         // CKA_OTP_COUNTER_REQUIREMENT
   $00000227,                         // CKA_OTP_PIN_REQUIREMENT
   $0000022E,                         // CKA_OTP_COUNTER
   $0000022F,                         // CKA_OTP_TIME
   $0000022A,                         // CKA_OTP_USER_IDENTIFIER
   $0000022B,                         // CKA_OTP_SERVICE_IDENTIFIER
   $0000022C,                         // CKA_OTP_SERVICE_LOGO
   $0000022D,                         // CKA_OTP_SERVICE_LOGO_TYPE
   $00000250,                         // CKA_GOSTR3410_PARAMS
   $00000251,                         // CKA_GOSTR3411_PARAMS
   $00000252,                         // CKA_GOST28147_PARAMS
   $00000300,                         // CKA_HW_FEATURE_TYPE
   $00000301,                         // CKA_RESET_ON_INIT
   $00000302,                         // CKA_HAS_RESET
   $00000400,                         // CKA_PIXEL_X
   $00000401,                         // CKA_PIXEL_Y
   $00000402,                         // CKA_RESOLUTION
   $00000403,                         // CKA_CHAR_ROWS
   $00000404,                         // CKA_CHAR_COLUMNS
   $00000405,                         // CKA_COLOR
   $00000406,                         // CKA_BITS_PER_PIXEL
   $00000480,                         // CKA_CHAR_SETS
   $00000481,                         // CKA_ENCODING_METHODS
   $00000482,                         // CKA_MIME_TYPES
   $00000500,                         // CKA_MECHANISM_TYPE
   $00000501,                         // CKA_REQUIRED_CMS_ATTRIBUTES
   $00000502,                         // CKA_DEFAULT_CMS_ATTRIBUTES
   $00000503,                         // CKA_SUPPORTED_CMS_ATTRIBUTES
   CKF_ARRAY_ATTRIBUTE or $00000600,  // CKA_ALLOWED_MECHANISMS
   $00000601,                         // CKA_PROFILE_ID
   $00000602,                         // CKA_X2RATCHET_BAG
   $00000603,                         // CKA_X2RATCHET_BAGSIZE
   $00000604,                         // CKA_X2RATCHET_BOBS1STMSG
   $00000605,                         // CKA_X2RATCHET_CKR
   $00000606,                         // CKA_X2RATCHET_CKS
   $00000607,                         // CKA_X2RATCHET_DHP
   $00000608,                         // CKA_X2RATCHET_DHR
   $00000609,                         // CKA_X2RATCHET_DHS
   $0000060A,                         // CKA_X2RATCHET_HKR
   $0000060B,                         // CKA_X2RATCHET_HKS
   $0000060C,                         // CKA_X2RATCHET_ISALICE
   $0000060D,                         // CKA_X2RATCHET_NHKR
   $0000060E,                         // CKA_X2RATCHET_NHKS
   $0000060F,                         // CKA_X2RATCHET_NR
   $00000610,                         // CKA_X2RATCHET_NS
   $00000611,                         // CKA_X2RATCHET_PNS
   $00000612,                         // CKA_X2RATCHET_RK
   $80000000);                        // CKA_VENDOR_DEFINED

function ToULONG(at: CK_ATTRIBUTE_TYPE): CK_ATTRIBUTE_TYPE_ULONG;
begin
  result := CKA_ULONG[at];
end;

function ATTRIBUTE_TYPE(uu: CK_ATTRIBUTE_TYPE_ULONG): CK_ATTRIBUTE_TYPE;
var
  i: PtrInt;
begin
  i := IntegerScanIndex(@CKA_ULONG, length(CKA_ULONG), uu);
  if i >= 0 then
    result := CK_ATTRIBUTE_TYPE(i)
  else
    result := CKA_VENDOR_DEFINED;
end;

function ToText(mt: CK_MECHANISM_TYPE): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_MECHANISM_TYPE), ord(mt));
end;

const
  // warning: MECHANISM_TYPE() expects this array to be sorted
  CKM_WORD: array[CK_MECHANISM_TYPE] of word = (
    $0000, // CKM_RSA_PKCS_KEY_PAIR_GEN
    $0001, // CKM_RSA_PKCS
    $0002, // CKM_RSA_9796
    $0003, // CKM_RSA_X_509
    $0004, // CKM_MD2_RSA_PKCS
    $0005, // CKM_MD5_RSA_PKCS
    $0006, // CKM_SHA1_RSA_PKCS
    $0007, // CKM_RIPEMD128_RSA_PKCS
    $0008, // CKM_RIPEMD160_RSA_PKCS
    $0009, // CKM_RSA_PKCS_OAEP
    $000A, // CKM_RSA_X9_31_KEY_PAIR_GEN
    $000B, // CKM_RSA_X9_31
    $000C, // CKM_SHA1_RSA_X9_31
    $000D, // CKM_RSA_PKCS_PSS
    $000E, // CKM_SHA1_RSA_PKCS_PSS
    $0010, // CKM_DSA_KEY_PAIR_GEN
    $0011, // CKM_DSA
    $0012, // CKM_DSA_SHA1
    $0013, // CKM_DSA_SHA224
    $0014, // CKM_DSA_SHA256
    $0015, // CKM_DSA_SHA384
    $0016, // CKM_DSA_SHA512
    $0018, // CKM_DSA_SHA3_224
    $0019, // CKM_DSA_SHA3_256
    $001A, // CKM_DSA_SHA3_384
    $001B, // CKM_DSA_SHA3_512
    $0020, // CKM_DH_PKCS_KEY_PAIR_GEN
    $0021, // CKM_DH_PKCS_DERIVE
    $0030, // CKM_X9_42_DH_KEY_PAIR_GEN
    $0031, // CKM_X9_42_DH_DERIVE
    $0032, // CKM_X9_42_DH_HYBRID_DERIVE
    $0033, // CKM_X9_42_MQV_DERIVE
    $0040, // CKM_SHA256_RSA_PKCS
    $0041, // CKM_SHA384_RSA_PKCS
    $0042, // CKM_SHA512_RSA_PKCS
    $0043, // CKM_SHA256_RSA_PKCS_PSS
    $0044, // CKM_SHA384_RSA_PKCS_PSS
    $0045, // CKM_SHA512_RSA_PKCS_PSS
    $0046, // CKM_SHA224_RSA_PKCS
    $0047, // CKM_SHA224_RSA_PKCS_PSS
    $0048, // CKM_SHA512_224
    $0049, // CKM_SHA512_224_HMAC
    $004A, // CKM_SHA512_224_HMAC_GENERAL
    $004B, // CKM_SHA512_224_KEY_DERIVATION
    $004C, // CKM_SHA512_256
    $004D, // CKM_SHA512_256_HMAC
    $004E, // CKM_SHA512_256_HMAC_GENERAL
    $004F, // CKM_SHA512_256_KEY_DERIVATION
    $0050, // CKM_SHA512_T
    $0051, // CKM_SHA512_T_HMAC
    $0052, // CKM_SHA512_T_HMAC_GENERAL
    $0053, // CKM_SHA512_T_KEY_DERIVATION
    $0060, // CKM_SHA3_256_RSA_PKCS
    $0061, // CKM_SHA3_384_RSA_PKCS
    $0062, // CKM_SHA3_512_RSA_PKCS
    $0063, // CKM_SHA3_256_RSA_PKCS_PSS
    $0064, // CKM_SHA3_384_RSA_PKCS_PSS
    $0065, // CKM_SHA3_512_RSA_PKCS_PSS
    $0066, // CKM_SHA3_224_RSA_PKCS
    $0067, // CKM_SHA3_224_RSA_PKCS_PSS
    $0100, // CKM_RC2_KEY_GEN
    $0101, // CKM_RC2_ECB
    $0102, // CKM_RC2_CBC
    $0103, // CKM_RC2_MAC
    $0104, // CKM_RC2_MAC_GENERAL
    $0105, // CKM_RC2_CBC_PAD
    $0110, // CKM_RC4_KEY_GEN
    $0111, // CKM_RC4
    $0120, // CKM_DES_KEY_GEN
    $0121, // CKM_DES_ECB
    $0122, // CKM_DES_CBC
    $0123, // CKM_DES_MAC
    $0124, // CKM_DES_MAC_GENERAL
    $0125, // CKM_DES_CBC_PAD
    $0130, // CKM_DES2_KEY_GEN
    $0131, // CKM_DES3_KEY_GEN
    $0132, // CKM_DES3_ECB
    $0133, // CKM_DES3_CBC
    $0134, // CKM_DES3_MAC
    $0135, // CKM_DES3_MAC_GENERAL
    $0136, // CKM_DES3_CBC_PAD
    $0137, // CKM_DES3_CMAC_GENERAL
    $0138, // CKM_DES3_CMAC
    $0140, // CKM_CDMF_KEY_GEN
    $0141, // CKM_CDMF_ECB
    $0142, // CKM_CDMF_CBC
    $0143, // CKM_CDMF_MAC
    $0144, // CKM_CDMF_MAC_GENERAL
    $0145, // CKM_CDMF_CBC_PAD
    $0150, // CKM_DES_OFB64
    $0151, // CKM_DES_OFB8
    $0152, // CKM_DES_CFB64
    $0153, // CKM_DES_CFB8
    $0200, // CKM_MD2
    $0201, // CKM_MD2_HMAC
    $0202, // CKM_MD2_HMAC_GENERAL
    $0210, // CKM_MD5
    $0211, // CKM_MD5_HMAC
    $0212, // CKM_MD5_HMAC_GENERAL
    $0220, // CKM_SHA_1
    $0221, // CKM_SHA_1_HMAC
    $0222, // CKM_SHA_1_HMAC_GENERAL
    $0230, // CKM_RIPEMD128
    $0231, // CKM_RIPEMD128_HMAC
    $0232, // CKM_RIPEMD128_HMAC_GENERAL
    $0240, // CKM_RIPEMD160
    $0241, // CKM_RIPEMD160_HMAC
    $0242, // CKM_RIPEMD160_HMAC_GENERAL
    $0250, // CKM_SHA256
    $0251, // CKM_SHA256_HMAC
    $0252, // CKM_SHA256_HMAC_GENERAL
    $0255, // CKM_SHA224
    $0256, // CKM_SHA224_HMAC
    $0257, // CKM_SHA224_HMAC_GENERAL
    $0260, // CKM_SHA384
    $0261, // CKM_SHA384_HMAC
    $0262, // CKM_SHA384_HMAC_GENERAL
    $0270, // CKM_SHA512
    $0271, // CKM_SHA512_HMAC
    $0272, // CKM_SHA512_HMAC_GENERAL
    $0280, // CKM_SECURID_KEY_GEN
    $0282, // CKM_SECURID
    $0290, // CKM_HOTP_KEY_GEN
    $0291, // CKM_HOTP
    $02A0, // CKM_ACTI
    $02A1, // CKM_ACTI_KEY_GEN
    $02B0, // CKM_SHA3_256
    $02B1, // CKM_SHA3_256_HMAC
    $02B2, // CKM_SHA3_256_HMAC_GENERAL
    $02B3, // CKM_SHA3_256_KEY_GEN
    $02B5, // CKM_SHA3_224
    $02B6, // CKM_SHA3_224_HMAC
    $02B7, // CKM_SHA3_224_HMAC_GENERAL
    $02B8, // CKM_SHA3_224_KEY_GEN
    $02C0, // CKM_SHA3_384
    $02C1, // CKM_SHA3_384_HMAC
    $02C2, // CKM_SHA3_384_HMAC_GENERAL
    $02C3, // CKM_SHA3_384_KEY_GEN
    $02D0, // CKM_SHA3_512
    $02D1, // CKM_SHA3_512_HMAC
    $02D2, // CKM_SHA3_512_HMAC_GENERAL
    $02D3, // CKM_SHA3_512_KEY_GEN
    $0300, // CKM_CAST_KEY_GEN
    $0301, // CKM_CAST_ECB
    $0302, // CKM_CAST_CBC
    $0303, // CKM_CAST_MAC
    $0304, // CKM_CAST_MAC_GENERAL
    $0305, // CKM_CAST_CBC_PAD
    $0310, // CKM_CAST3_KEY_GEN
    $0311, // CKM_CAST3_ECB
    $0312, // CKM_CAST3_CBC
    $0313, // CKM_CAST3_MAC
    $0314, // CKM_CAST3_MAC_GENERAL
    $0315, // CKM_CAST3_CBC_PAD
    $0320, // CKM_CAST128_KEY_GEN
    $0321, // CKM_CAST128_ECB
    $0322, // CKM_CAST128_CBC
    $0323, // CKM_CAST128_MAC
    $0324, // CKM_CAST128_MAC_GENERAL
    $0325, // CKM_CAST128_CBC_PAD
    $0330, // CKM_RC5_KEY_GEN
    $0331, // CKM_RC5_ECB
    $0332, // CKM_RC5_CBC
    $0333, // CKM_RC5_MAC
    $0334, // CKM_RC5_MAC_GENERAL
    $0335, // CKM_RC5_CBC_PAD
    $0340, // CKM_IDEA_KEY_GEN
    $0341, // CKM_IDEA_ECB
    $0342, // CKM_IDEA_CBC
    $0343, // CKM_IDEA_MAC
    $0344, // CKM_IDEA_MAC_GENERAL
    $0345, // CKM_IDEA_CBC_PAD
    $0350, // CKM_GENERIC_SECRET_KEY_GEN
    $0360, // CKM_CONCATENATE_BASE_AND_KEY
    $0362, // CKM_CONCATENATE_BASE_AND_DATA
    $0363, // CKM_CONCATENATE_DATA_AND_BASE
    $0364, // CKM_XOR_BASE_AND_DATA
    $0365, // CKM_EXTRACT_KEY_FROM_KEY
    $0370, // CKM_SSL3_PRE_MASTER_KEY_GEN
    $0371, // CKM_SSL3_MASTER_KEY_DERIVE
    $0372, // CKM_SSL3_KEY_AND_MAC_DERIVE
    $0373, // CKM_SSL3_MASTER_KEY_DERIVE_DH
    $0374, // CKM_TLS_PRE_MASTER_KEY_GEN
    $0375, // CKM_TLS_MASTER_KEY_DERIVE
    $0376, // CKM_TLS_KEY_AND_MAC_DERIVE
    $0377, // CKM_TLS_MASTER_KEY_DERIVE_DH
    $0378, // CKM_TLS_PRF
    $0380, // CKM_SSL3_MD5_MAC
    $0381, // CKM_SSL3_SHA1_MAC
    $0390, // CKM_MD5_KEY_DERIVATION
    $0391, // CKM_MD2_KEY_DERIVATION
    $0392, // CKM_SHA1_KEY_DERIVATION
    $0393, // CKM_SHA256_KEY_DERIVATION
    $0394, // CKM_SHA384_KEY_DERIVATION
    $0395, // CKM_SHA512_KEY_DERIVATION
    $0396, // CKM_SHA224_KEY_DERIVATION
    $0397, // CKM_SHA3_256_KEY_DERIVATION
    $0398, // CKM_SHA3_224_KEY_DERIVATION
    $0399, // CKM_SHA3_384_KEY_DERIVATION
    $039A, // CKM_SHA3_512_KEY_DERIVATION
    $039B, // CKM_SHAKE_128_KEY_DERIVATION
    $039C, // CKM_SHAKE_256_KEY_DERIVATION
    $03A0, // CKM_PBE_MD2_DES_CBC
    $03A1, // CKM_PBE_MD5_DES_CBC
    $03A2, // CKM_PBE_MD5_CAST_CBC
    $03A3, // CKM_PBE_MD5_CAST3_CBC
    $03A4, // CKM_PBE_MD5_CAST128_CBC
    $03A5, // CKM_PBE_SHA1_CAST128_CBC
    $03A6, // CKM_PBE_SHA1_RC4_128
    $03A7, // CKM_PBE_SHA1_RC4_40
    $03A8, // CKM_PBE_SHA1_DES3_EDE_CBC
    $03A9, // CKM_PBE_SHA1_DES2_EDE_CBC
    $03AA, // CKM_PBE_SHA1_RC2_128_CBC
    $03AB, // CKM_PBE_SHA1_RC2_40_CBC
    $03AC, // CKM_SP800_108_COUNTER_KDF
    $03AD, // CKM_SP800_108_FEEDBACK_KDF
    $03AE, // CKM_SP800_108_DOUBLE_PIPELINE_KDF
    $03B0, // CKM_PKCS5_PBKD2
    $03C0, // CKM_PBA_SHA1_WITH_SHA1_HMAC
    $03D0, // CKM_WTLS_PRE_MASTER_KEY_GEN
    $03D1, // CKM_WTLS_MASTER_KEY_DERIVE
    $03D2, // CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC
    $03D3, // CKM_WTLS_PRF
    $03D4, // CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE
    $03D5, // CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE
    $03D8, // CKM_TLS12_MAC
    $03D9, // CKM_TLS12_KDF
    $03E0, // CKM_TLS12_MASTER_KEY_DERIVE
    $03E1, // CKM_TLS12_KEY_AND_MAC_DERIVE
    $03E2, // CKM_TLS12_MASTER_KEY_DERIVE_DH
    $03E3, // CKM_TLS12_KEY_SAFE_DERIVE
    $03E4, // CKM_TLS_MAC
    $03E5, // CKM_TLS_KDF
    $0400, // CKM_KEY_WRAP_LYNKS
    $0401, // CKM_KEY_WRAP_SET_OAEP
    $0500, // CKM_CMS_SIG
    $0510, // CKM_KIP_DERIVE
    $0511, // CKM_KIP_WRAP
    $0512, // CKM_KIP_MAC
    $0550, // CKM_CAMELLIA_KEY_GEN
    $0551, // CKM_CAMELLIA_ECB
    $0552, // CKM_CAMELLIA_CBC
    $0553, // CKM_CAMELLIA_MAC
    $0554, // CKM_CAMELLIA_MAC_GENERAL
    $0555, // CKM_CAMELLIA_CBC_PAD
    $0556, // CKM_CAMELLIA_ECB_ENCRYPT_DATA
    $0557, // CKM_CAMELLIA_CBC_ENCRYPT_DATA
    $0558, // CKM_CAMELLIA_CTR
    $0560, // CKM_ARIA_KEY_GEN
    $0561, // CKM_ARIA_ECB
    $0562, // CKM_ARIA_CBC
    $0563, // CKM_ARIA_MAC
    $0564, // CKM_ARIA_MAC_GENERAL
    $0565, // CKM_ARIA_CBC_PAD
    $0566, // CKM_ARIA_ECB_ENCRYPT_DATA
    $0567, // CKM_ARIA_CBC_ENCRYPT_DATA
    $0650, // CKM_SEED_KEY_GEN
    $0651, // CKM_SEED_ECB
    $0652, // CKM_SEED_CBC
    $0653, // CKM_SEED_MAC
    $0654, // CKM_SEED_MAC_GENERAL
    $0655, // CKM_SEED_CBC_PAD
    $0656, // CKM_SEED_ECB_ENCRYPT_DATA
    $0657, // CKM_SEED_CBC_ENCRYPT_DATA
    $1000, // CKM_SKIPJACK_KEY_GEN
    $1001, // CKM_SKIPJACK_ECB64
    $1002, // CKM_SKIPJACK_CBC64
    $1003, // CKM_SKIPJACK_OFB64
    $1004, // CKM_SKIPJACK_CFB64
    $1005, // CKM_SKIPJACK_CFB32
    $1006, // CKM_SKIPJACK_CFB16
    $1007, // CKM_SKIPJACK_CFB8
    $1008, // CKM_SKIPJACK_WRAP
    $1009, // CKM_SKIPJACK_PRIVATE_WRAP
    $100A, // CKM_SKIPJACK_RELAYX
    $1010, // CKM_KEA_KEY_PAIR_GEN
    $1011, // CKM_KEA_KEY_DERIVE
    $1012, // CKM_KEA_DERIVE
    $1020, // CKM_FORTEZZA_TIMESTAMP
    $1030, // CKM_BATON_KEY_GEN
    $1031, // CKM_BATON_ECB128
    $1032, // CKM_BATON_ECB96
    $1033, // CKM_BATON_CBC128
    $1034, // CKM_BATON_COUNTER
    $1035, // CKM_BATON_SHUFFLE
    $1036, // CKM_BATON_WRAP
    $1040, // CKM_EC_KEY_PAIR_GEN
    $1041, // CKM_ECDSA
    $1042, // CKM_ECDSA_SHA1
    $1043, // CKM_ECDSA_SHA224
    $1044, // CKM_ECDSA_SHA256
    $1045, // CKM_ECDSA_SHA384
    $1046, // CKM_ECDSA_SHA512
    $1047, // CKM_ECDSA_SHA3_224
    $1048, // CKM_ECDSA_SHA3_256
    $1049, // CKM_ECDSA_SHA3_384
    $104A, // CKM_ECDSA_SHA3_512
    $1050, // CKM_ECDH1_DERIVE
    $1051, // CKM_ECDH1_COFACTOR_DERIVE
    $1052, // CKM_ECMQV_DERIVE
    $1053, // CKM_ECDH_AES_KEY_WRAP
    $1054, // CKM_RSA_AES_KEY_WRAP
    $1055, // CKM_EC_EDWARDS_KEY_PAIR_GEN
    $1056, // CKM_EC_MONTGOMERY_KEY_PAIR_GEN
    $1057, // CKM_EDDSA
    $1060, // CKM_JUNIPER_KEY_GEN
    $1061, // CKM_JUNIPER_ECB128
    $1062, // CKM_JUNIPER_CBC128
    $1063, // CKM_JUNIPER_COUNTER
    $1064, // CKM_JUNIPER_SHUFFLE
    $1065, // CKM_JUNIPER_WRAP
    $1070, // CKM_FASTHASH
    $1071, // CKM_AES_XTS
    $1072, // CKM_AES_XTS_KEY_GEN
    $1080, // CKM_AES_KEY_GEN
    $1081, // CKM_AES_ECB
    $1082, // CKM_AES_CBC
    $1083, // CKM_AES_MAC
    $1084, // CKM_AES_MAC_GENERAL
    $1085, // CKM_AES_CBC_PAD
    $1086, // CKM_AES_CTR
    $1087, // CKM_AES_GCM
    $1088, // CKM_AES_CCM
    $1089, // CKM_AES_CTS
    $108A, // CKM_AES_CMAC
    $108B, // CKM_AES_CMAC_GENERAL
    $108C, // CKM_AES_XCBC_MAC
    $108D, // CKM_AES_XCBC_MAC_96
    $108E, // CKM_AES_GMAC
    $1090, // CKM_BLOWFISH_KEY_GEN
    $1091, // CKM_BLOWFISH_CBC
    $1092, // CKM_TWOFISH_KEY_GEN
    $1093, // CKM_TWOFISH_CBC
    $1094, // CKM_BLOWFISH_CBC_PAD
    $1095, // CKM_TWOFISH_CBC_PAD
    $1100, // CKM_DES_ECB_ENCRYPT_DATA
    $1101, // CKM_DES_CBC_ENCRYPT_DATA
    $1102, // CKM_DES3_ECB_ENCRYPT_DATA
    $1103, // CKM_DES3_CBC_ENCRYPT_DATA
    $1104, // CKM_AES_ECB_ENCRYPT_DATA
    $1105, // CKM_AES_CBC_ENCRYPT_DATA
    $1200, // CKM_GOSTR3410_KEY_PAIR_GEN
    $1201, // CKM_GOSTR3410
    $1202, // CKM_GOSTR3410_WITH_GOSTR3411
    $1203, // CKM_GOSTR3410_KEY_WRAP
    $1204, // CKM_GOSTR3410_DERIVE
    $1210, // CKM_GOSTR3411
    $1211, // CKM_GOSTR3411_HMAC
    $1220, // CKM_GOST28147_KEY_GEN
    $1221, // CKM_GOST28147_ECB
    $1222, // CKM_GOST28147
    $1223, // CKM_GOST28147_MAC
    $1224, // CKM_GOST28147_KEY_WRAP
    $1225, // CKM_CHACHA20_KEY_GEN
    $1226, // CKM_CHACHA20
    $1227, // CKM_POLY1305_KEY_GEN
    $1228, // CKM_POLY1305
    $140B, // CKM_EC_KEY_PAIR_GEN_W_EXTRA_BITS
    $2000, // CKM_DSA_PARAMETER_GEN
    $2001, // CKM_DH_PKCS_PARAMETER_GEN
    $2002, // CKM_X9_42_DH_PARAMETER_GEN
    $2003, // CKM_DSA_PROBABILISTIC_PARAMETER_GEN
    $2004, // CKM_DSA_SHAWE_TAYLOR_PARAMETER_GEN
    $2005, // CKM_DSA_FIPS_G_GEN
    $2104, // CKM_AES_OFB
    $2105, // CKM_AES_CFB64
    $2106, // CKM_AES_CFB8
    $2107, // CKM_AES_CFB128
    $2108, // CKM_AES_CFB1
    $2109, // CKM_AES_KEY_WRAP
    $210A, // CKM_AES_KEY_WRAP_PAD
    $210B, // CKM_AES_KEY_WRAP_KWP
    $4001, // CKM_RSA_PKCS_TPM_1_1
    $4002, // CKM_RSA_PKCS_OAEP_TPM_1_1
    $4003, // CKM_SHA_1_KEY_GEN
    $4004, // CKM_SHA224_KEY_GEN
    $4005, // CKM_SHA256_KEY_GEN
    $4006, // CKM_SHA384_KEY_GEN
    $4007, // CKM_SHA512_KEY_GEN
    $4008, // CKM_SHA512_224_KEY_GEN
    $4009, // CKM_SHA512_256_KEY_GEN
    $400A, // CKM_SHA512_T_KEY_GEN
    $400B, // CKM_NULL
    $400C, // CKM_BLAKE2B_160
    $400D, // CKM_BLAKE2B_160_HMAC
    $400E, // CKM_BLAKE2B_160_HMAC_GENERAL
    $400F, // CKM_BLAKE2B_160_KEY_DERIVE
    $4010, // CKM_BLAKE2B_160_KEY_GEN
    $4011, // CKM_BLAKE2B_256
    $4012, // CKM_BLAKE2B_256_HMAC
    $4013, // CKM_BLAKE2B_256_HMAC_GENERAL
    $4014, // CKM_BLAKE2B_256_KEY_DERIVE
    $4015, // CKM_BLAKE2B_256_KEY_GEN
    $4016, // CKM_BLAKE2B_384
    $4017, // CKM_BLAKE2B_384_HMAC
    $4018, // CKM_BLAKE2B_384_HMAC_GENERAL
    $4019, // CKM_BLAKE2B_384_KEY_DERIVE
    $401A, // CKM_BLAKE2B_384_KEY_GEN
    $401B, // CKM_BLAKE2B_512
    $401C, // CKM_BLAKE2B_512_HMAC
    $401D, // CKM_BLAKE2B_512_HMAC_GENERAL
    $401E, // CKM_BLAKE2B_512_KEY_DERIVE
    $401F, // CKM_BLAKE2B_512_KEY_GEN
    $4020, // CKM_SALSA20
    $4021, // CKM_CHACHA20_POLY1305
    $4022, // CKM_SALSA20_POLY1305
    $4023, // CKM_X3DH_INITIALIZE
    $4024, // CKM_X3DH_RESPOND
    $4025, // CKM_X2RATCHET_INITIALIZE
    $4026, // CKM_X2RATCHET_RESPOND
    $4027, // CKM_X2RATCHET_ENCRYPT
    $4028, // CKM_X2RATCHET_DECRYPT
    $4029, // CKM_XEDDSA
    $402A, // CKM_HKDF_DERIVE
    $402B, // CKM_HKDF_DATA
    $402C, // CKM_HKDF_KEY_GEN
    $402D, // CKM_SALSA20_KEY_GEN
    $FFFF);// CKM_VENDOR_DEFINED = $80000000 > 16-bit word

function ToULONG(mt: CK_MECHANISM_TYPE): CK_MECHANISM_TYPE_ULONG;
begin
  if mt = CKM_VENDOR_DEFINED then
    result := CKM_VENDOR_DEFINED_ULONG // = $80000000
  else
    result := CKM_WORD[mt];
end;

function MECHANISM_TYPE(uu: CK_MECHANISM_TYPE_ULONG): CK_MECHANISM_TYPE;
var
  i: PtrInt;
begin
  result := CKM_VENDOR_DEFINED;
  if uu > $ffff then
    exit;
  i := FastFindWordSorted(@CKM_WORD, length(CKM_WORD), uu); // O(log(n)) search
  if i >= 0 then
    result := CK_MECHANISM_TYPE(i);
end;

function ToText(rv: CK_RV): PShortString;
begin
  result := GetEnumName(TypeInfo(CK_RV), ord(rv));
end;

function DefaultGenerateMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_AES:
      mt := CKM_AES_KEY_GEN;
    CKK_DH:
      mt := CKM_DH_PKCS_KEY_PAIR_GEN;
    CKK_DSA:
      mt := CKM_DSA_KEY_PAIR_GEN;
    CKK_EC:
      mt := CKM_EC_KEY_PAIR_GEN;
    CKK_RSA:
      mt := CKM_RSA_PKCS_KEY_PAIR_GEN;
    CKK_X9_42_DH:
      mt := CKM_X9_42_DH_KEY_PAIR_GEN;
    CKK_EC_EDWARDS:
      mt := CKM_EC_EDWARDS_KEY_PAIR_GEN;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

function DefaultParamGenerateMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_DH:
      mt := CKM_DH_PKCS_PARAMETER_GEN;
    CKK_DSA:
      mt := CKM_DSA_PARAMETER_GEN;
    CKK_X9_42_DH:
      mt := CKM_X9_42_DH_PARAMETER_GEN;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

function DefaultEncryptMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_AES:
      mt := CKM_AES_CBC_PAD;
    CKK_RSA:
      mt := CKM_RSA_PKCS_OAEP;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

function DefaultSignMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_AES:
      mt := CKM_AES_MAC;
    CKK_RSA:
      mt := CKM_SHA512_RSA_PKCS;
    CKK_DSA:
      mt := CKM_DSA_SHA512;
    CKK_EC:
      mt := CKM_ECDSA_SHA512;
    CKK_EC_EDWARDS:
      mt := CKM_EDDSA;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

function DefaultWrapMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_AES:
      mt := CKM_AES_KEY_WRAP;
    CKK_RSA:
      mt := CKM_RSA_PKCS_OAEP;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

function DefaultDeriveMechanism(kt: CK_KEY_TYPE;
  out uu: CK_MECHANISM_TYPE_ULONG): boolean;
var
  mt: CK_MECHANISM_TYPE;
begin
  result := false;
  case kt of
    CKK_DH:
      mt := CKM_DH_PKCS_DERIVE;
    CKK_EC:
      mt := CKM_ECDH1_DERIVE;
    CKK_X9_42_DH:
      mt := CKM_X9_42_DH_DERIVE;
  else
    exit;
  end;
  uu := CKM_WORD[mt];
  result := true;
end;

const
  // warning: RV() expects this array to be sorted
  CKR_WORD: array[CK_RV] of word = (
    $0000, // CKR_OK
    $0001, // CKR_CANCEL
    $0002, // CKR_HOST_MEMORY
    $0003, // CKR_SLOT_ID_INVALID
    $0005, // CKR_GENERAL_ERROR
    $0006, // CKR_FUNCTION_FAILED
    $0007, // CKR_ARGUMENTS_BAD
    $0008, // CKR_NO_EVENT
    $0009, // CKR_NEED_TO_CREATE_THREADS
    $000A, // CKR_CANT_LOCK
    $0010, // CKR_ATTRIBUTE_READ_ONLY
    $0011, // CKR_ATTRIBUTE_SENSITIVE
    $0012, // CKR_ATTRIBUTE_TYPE_INVALID
    $0013, // CKR_ATTRIBUTE_VALUE_INVALID
    $001B, // CKR_ACTION_PROHIBITED
    $0020, // CKR_DATA_INVALID
    $0021, // CKR_DATA_LEN_RANGE
    $0030, // CKR_DEVICE_ERROR
    $0031, // CKR_DEVICE_MEMORY
    $0032, // CKR_DEVICE_REMOVED
    $0040, // CKR_ENCRYPTED_DATA_INVALID
    $0041, // CKR_ENCRYPTED_DATA_LEN_RANGE
    $0042, // CKR_AEAD_DECRYPT_FAILED
    $0050, // CKR_FUNCTION_CANCELED
    $0051, // CKR_FUNCTION_NOT_PARALLEL
    $0054, // CKR_FUNCTION_NOT_SUPPORTED
    $0060, // CKR_KEY_HANDLE_INVALID
    $0062, // CKR_KEY_SIZE_RANGE
    $0063, // CKR_KEY_TYPE_INCONSISTENT
    $0064, // CKR_KEY_NOT_NEEDED
    $0065, // CKR_KEY_CHANGED
    $0066, // CKR_KEY_NEEDED
    $0067, // CKR_KEY_INDIGESTIBLE
    $0068, // CKR_KEY_FUNCTION_NOT_PERMITTED
    $0069, // CKR_KEY_NOT_WRAPPABLE
    $006A, // CKR_KEY_UNEXTRACTABLE
    $0070, // CKR_MECHANISM_INVALID
    $0071, // CKR_MECHANISM_PARAM_INVALID
    $0082, // CKR_OBJECT_HANDLE_INVALID
    $0090, // CKR_OPERATION_ACTIVE
    $0091, // CKR_OPERATION_NOT_INITIALIZED
    $00A0, // CKR_PIN_INCORRECT
    $00A1, // CKR_PIN_INVALID
    $00A2, // CKR_PIN_LEN_RANGE
    $00A3, // CKR_PIN_EXPIRED
    $00A4, // CKR_PIN_LOCKED
    $00B0, // CKR_SESSION_CLOSED
    $00B1, // CKR_SESSION_COUNT
    $00B3, // CKR_SESSION_HANDLE_INVALID
    $00B4, // CKR_SESSION_PARALLEL_NOT_SUPPORTED
    $00B5, // CKR_SESSION_READ_ONLY
    $00B6, // CKR_SESSION_EXISTS
    $00B7, // CKR_SESSION_READ_ONLY_EXISTS
    $00B8, // CKR_SESSION_READ_WRITE_SO_EXISTS
    $00C0, // CKR_SIGNATURE_INVALID
    $00C1, // CKR_SIGNATURE_LEN_RANGE
    $00D0, // CKR_TEMPLATE_INCOMPLETE
    $00D1, // CKR_TEMPLATE_INCONSISTENT
    $00E0, // CKR_TOKEN_NOT_PRESENT
    $00E1, // CKR_TOKEN_NOT_RECOGNIZED
    $00E2, // CKR_TOKEN_WRITE_PROTECTED
    $00F0, // CKR_UNWRAPPING_KEY_HANDLE_INVALID
    $00F1, // CKR_UNWRAPPING_KEY_SIZE_RANGE
    $00F2, // CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT
    $0100, // CKR_USER_ALREADY_LOGGED_IN
    $0101, // CKR_USER_NOT_LOGGED_IN
    $0102, // CKR_USER_PIN_NOT_INITIALIZED
    $0103, // CKR_USER_TYPE_INVALID
    $0104, // CKR_USER_ANOTHER_ALREADY_LOGGED_IN
    $0105, // CKR_USER_TOO_MANY_TYPES
    $0110, // CKR_WRAPPED_KEY_INVALID
    $0112, // CKR_WRAPPED_KEY_LEN_RANGE
    $0113, // CKR_WRAPPING_KEY_HANDLE_INVALID
    $0114, // CKR_WRAPPING_KEY_SIZE_RANGE
    $0115, // CKR_WRAPPING_KEY_TYPE_INCONSISTENT
    $0120, // CKR_RANDOM_SEED_NOT_SUPPORTED
    $0121, // CKR_RANDOM_NO_RNG
    $0130, // CKR_DOMAIN_PARAMS_INVALID
    $0140, // CKR_CURVE_NOT_SUPPORTED
    $0150, // CKR_BUFFER_TOO_SMALL
    $0160, // CKR_SAVED_STATE_INVALID
    $0170, // CKR_INFORMATION_SENSITIVE
    $0180, // CKR_STATE_UNSAVEABLE
    $0190, // CKR_CRYPTOKI_NOT_INITIALIZED
    $0191, // CKR_CRYPTOKI_ALREADY_INITIALIZED
    $01A0, // CKR_MUTEX_BAD
    $01A1, // CKR_MUTEX_NOT_LOCKED
    $01B0, // CKR_NEW_PIN_MODE
    $01B1, // CKR_NEXT_OTP
    $01B5, // CKR_EXCEEDED_MAX_ITERATIONS
    $01B6, // CKR_FIPS_SELF_TEST_FAILED
    $01B7, // CKR_LIBRARY_LOAD_FAILED
    $01B8, // CKR_PIN_TOO_WEAK
    $01B9, // CKR_PUBLIC_KEY_INVALID
    $0200, // CKR_FUNCTION_REJECTED
    $0201, // CKR_TOKEN_RESOURCE_EXCEEDED
    $0202, // CKR_OPERATION_CANCEL_FAILED
    $ffff);// CKR_VENDOR_DEFINED


function ToULONG(rv: CK_RV): CK_RVULONG;
begin
  if rv = CKR_VENDOR_DEFINED then
    result := CKR_VENDORDEFINED // = $80000000
  else
    result := CKR_WORD[rv];
end;

function RV(uu: CK_RVULONG): CK_RV;
var
  i: PtrInt;
begin
  result := CKR_VENDOR_DEFINED;
  if uu > $ffff then
    exit;
  i := FastFindWordSorted(@CKR_WORD, length(CKR_WORD), uu); // O(log(n)) search
  if i >= 0 then
    result := CK_RV(i);
end;



{ CK_ATTRIBUTES }

function CK_ATTRIBUTES.InternalStore(const aValue: RawByteString): pointer;
begin
  if Attrs = nil then
    Clear; // initialize Count when needed (e.g. CK_ATTRIBUTES on stack)
  if fStoreBinPos = length(fStoreBin) then
    SetLength(fStoreBin, NextGrow(fStoreBinPos));
  fStoreBin[fStoreBinPos] := aValue; // fast ref-count copy for safety
  result := pointer(fStoreBin[fStoreBinPos]); // we know it won't disappear
  inc(fStoreBinPos);
end;

function CK_ATTRIBUTES.InternalStore(aValue: CK_ULONG): CK_ULONG_PTR;
begin
  if Attrs = nil then
    Clear; // initialize Count when needed (e.g. CK_ATTRIBUTES on stack)
  if fStoreUlongPos = high(fStoreUlong) then
    raise EPkcs11.Create('CK_ATTRIBUTES: too many CK_ULONG attributes');
  result := @fStoreUlong[fStoreUlongPos];
  result^ := aValue;
  inc(fStoreUlongPos);
end;

procedure CK_ATTRIBUTES.Clear;
begin
  Count := 0;
  fStoreBinPos := 0;
  fStoreULongPos := 0;
  // no need to reset the arrays
end;

procedure CK_ATTRIBUTES.ClearValues;
var
  i: PtrInt;
begin
  fStoreBinPos := 0;
  fStoreULongPos := 0;
  if Attrs <> nil then
    for i := 0 to Count - 1 do
      with Attrs[i] do
      begin
        pValue := nil;
        ulValueLen := 0;
      end;
end;

procedure CK_ATTRIBUTES.AllocateValues;
var
  i: PtrInt;
begin
  if Attrs <> nil then
    for i := 0 to Count - 1 do
      with Attrs[i] do
        if ulValueLen = CK_UNAVAILABLE_INFORMATION then
          ulValueLen := 0
        else if ulValueLen <= SizeOf(CK_ULONG) then
          pValue := InternalStore(0) // from static fStoreULong[]
        else
          pValue := InternalStore(RawUtf8OfChar(' ', ulValueLen)); // alloc
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE; aValue: pointer;
  aLen: CK_ULONG);
begin
  if Attrs = nil then
    Clear; // initialize Count when needed (e.g. CK_ATTRIBUTES on stack)
  if Count = length(Attrs) then
    SetLength(Attrs, NextGrow(Count)); // grow capacity by chunks
  with Attrs[Count] do
  begin
    _type := ToULONG(aType);
    pValue := aValue;
    ulValueLen := aLen;
  end;
  inc(Count);
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE);
begin
  Add(aType, nil, 0);
end;

procedure CK_ATTRIBUTES.Add(const aType: array of CK_ATTRIBUTE_TYPE);
var
  a: PtrInt;
begin
  for a := 0 to high(aType) do
    Add(aType[a], nil, 0);
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE; aValue: CK_ULONG);
begin
  Add(aType, InternalStore(aValue), SizeOf(aValue));
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE; aValue: boolean);
begin
  Add(aType, InternalStore(ord(aValue)), SizeOf(aValue));
end;

procedure CK_ATTRIBUTES.AddDate(aType: CK_ATTRIBUTE_TYPE; aValue: TDateTime);
begin
  if aValue = 0 then
    Add(aType, nil, 0) // see Cryptoki CK_DATE spec for "empty" date
  else
    Add(aType, DateToIso8601(aValue, {expanded=}false)); // 'YYYYMMDD' format
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE; const aValue: RawByteString);
begin
  if aValue = '' then
    Add(aType, nil, 0)
  else
    Add(aType, InternalStore(aValue), length(aValue));
end;

procedure CK_ATTRIBUTES.New(aClass: CK_OBJECT_CLASS);
begin
  Clear;
  Add(CKA_CLASS, ToULONG(aClass));
end;

procedure CK_ATTRIBUTES.New(aClass: CK_OBJECT_CLASS; const aLabel, aID: RawUtf8;
  aStore: boolean);
begin
  New(aClass);
  Add(CKA_TOKEN, aStore); // object stored in token
  if aLabel <> '' then
    Add(CKA_LABEL, aLabel);
  if aID <> '' then
    Add(CKA_ID, aID);
end;

procedure CK_ATTRIBUTES.Add(aCert: CK_CERTIFICATE_TYPE);
begin
  Add(CKA_CERTIFICATE_TYPE, ToULONG(aCert));
end;

procedure CK_ATTRIBUTES.Add(aCert: CK_CERTIFICATE_CATEGORY);
begin
  Add(CKA_CERTIFICATE_CATEGORY, ord(aCert));
end;

procedure CK_ATTRIBUTES.Add(aKey: CK_KEY_TYPE);
begin
  Add(CKA_KEY_TYPE, ToULONG(aKey));
end;

procedure CK_ATTRIBUTES.Add(aMech: CK_MECHANISM_TYPE);
begin
  Add(CKA_CERTIFICATE_TYPE, ToULONG(aMech));
end;

procedure CK_ATTRIBUTES.Add(aType: CK_ATTRIBUTE_TYPE;
  const aAttrib: CK_ATTRIBUTES);
begin
  Add(aType, pointer(aAttrib.Attrs), aAttrib.Count * SizeOf(aAttrib.Attrs[0]));
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE): CK_ATTRIBUTE_PTR;
var
  u: CK_ATTRIBUTE_TYPE_ULONG;
  i: PtrInt;
begin
  u := ToULong(aType);
  result := pointer(Attrs);
  if result <> nil then
    for i := 1 to Count do
      if result^._type = u then
        if result^.ulValueLen = CK_UNAVAILABLE_INFORMATION then
          break // not existing or too sensitive
        else
          exit  // found
      else
        inc(result);
  result := nil;
end;

function CK_ATTRIBUTES.FindLen(aType: CK_ATTRIBUTE_TYPE): integer;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  if found = nil then
    result := - 1
  else
    result := found^.ulValueLen;
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE;
  out aValue: CK_ULONG): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := (found <> nil) and
            (found^.ulValueLen = SizeOf(aValue));
  if result then
    aValue := CK_ULONG_PTR(found^.pValue)^;
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE;
  out aValue: boolean): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := (found <> nil) and
            (found^.ulValueLen = SizeOf(aValue));
  if result then
    aValue := PBoolean(found^.pValue)^;
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE;
  out aValue: TDateTime): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := found <> nil;
  if result then
    if found^.ulValueLen <> SizeOf(CK_DATE) then
      aValue := 0     // e.g. "empty" CK_DATE
    else
      Iso8601ToDateTimePUtf8CharVar(found^.pValue, SizeOf(CK_DATE), aValue);
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE;
  out aValue: RawByteString): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := found <> nil;
  if result then
    FastSetRawByteString(aValue, found^.pValue, found^.ulValueLen);
end;

function CK_ATTRIBUTES.Find(aType: CK_ATTRIBUTE_TYPE;
  out aValue: RawUtf8; aEncodeHex: boolean): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := found <> nil;
  if result then
    if aEncodeHex then
      BinToHexLower(found^.pValue, found^.ulValueLen, aValue)
    else
      FastSetString(aValue, found^.pValue, found^.ulValueLen);
end;

function CK_ATTRIBUTES.Equals(aType: CK_ATTRIBUTE_TYPE; aValue: CK_ULONG): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := (found <> nil) and
            (found^.ulValueLen = SizeOf(aValue)) and
            (CK_ULONG_PTR(found^.pValue)^ = aValue);
end;

function CK_ATTRIBUTES.Equals(aType: CK_ATTRIBUTE_TYPE; aValue: boolean): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := (found <> nil) and
            (found^.ulValueLen = SizeOf(aValue)) and
            (PBoolean(found^.pValue)^ = aValue);
end;

function CK_ATTRIBUTES.Equals(aType: CK_ATTRIBUTE_TYPE;
  const aValue: RawByteString): boolean;
var
  found: CK_ATTRIBUTE_PTR;
begin
  found := Find(aType);
  result := (found <> nil) and
            (found^.ulValueLen = CK_ULONG(length(aValue))) and
    mormot.core.base.CompareMem(found^.pValue, pointer(aValue), found^.ulValueLen);
end;


{ ***************** PKCS#11 High-Level Wrappers }

procedure UnPad(p: PUtf8Char; max: integer; var text: RawUtf8);
begin
  FastSetString(text, p, max);
  TrimSelf(text);
end;

function Pad(const text: RawUtf8; max: integer): RawUtf8;
var
  len: integer;
begin
  FastSetString(result, nil, max);
  len := length(text);
  if len > max then
    len := max
  else
    FillCharFast(PByteArray(result)[len], max - len, ord(' '));
  MoveFast(pointer(text)^, pointer(result)^, len);
end;

procedure FillToken(ID: CK_SLOT_ID; const Raw: CK_TOKEN_INFO;
  out Token: TPkcs11Token);
begin
  Token.Slot:= ID;
  UnPad(Raw._label, SizeOf(Raw._label), Token.Name);
  UnPad(Raw.manufacturerID, SizeOf(Raw.manufacturerID), Token.Manufacturer);
  UnPad(Raw.model, SizeOf(Raw.model), Token.Model);
  UnPad(Raw.serialNumber, SizeOf(Raw.serialNumber), Token.Serial);
  UnPad(Raw.utcTime, SizeOf(Raw.utcTime), Token.Time);
  Token.Flags := CKT_FLAGS(cardinal(Raw.flags));
  Token.Sessions := Raw.ulSessionCount;
  Token.MaxSessions := Raw.ulMaxSessionCount;
  Token.MinPin := Raw.ulMinPinLen;
  Token.MaxPin := Raw.ulMaxPinLen;
end;

procedure FillSlot(ID: CK_SLOT_ID; const Raw: CK_SLOT_INFO;
  out Slot: TPkcs11Slot);
begin
  Slot.Slot := ID;
  UnPad(Raw.slotDescription, SizeOf(Raw.slotDescription), Slot.Description);
  UnPad(Raw.manufacturerID, SizeOf(Raw.manufacturerID), Slot.Manufacturer);
  Slot.Flags := CKSL_FLAGS(byte(Raw.flags));
  Slot.Hardware := Raw.hardwareVersion;
  Slot.Firmware := Raw.firmwareVersion;
  Slot.Mechanism := nil;
end;

const
  POS2CKA: array[low(TPkcs11ObjectStorage) .. pred(posX509)] of CK_ATTRIBUTE_TYPE = (
    CKA_TOKEN,           // posToken
    CKA_PRIVATE,         // posPrivate
    CKA_SENSITIVE,       // posSensitive
    CKA_MODIFIABLE,      // posModifiable
    CKA_COPYABLE,        // posCopiable
    CKA_DESTROYABLE,     // posDestroyable
    CKA_EXTRACTABLE,     // posExtractable
    CKA_ENCRYPT,         // posEncrypt
    CKA_DECRYPT,         // posDecrypt
    CKA_VERIFY,          // posVerify
    CKA_VERIFY_RECOVER,  // posVerifyRecover
    CKA_SIGN,            // posSign
    CKA_SIGN_RECOVER,    // posSignRecover
    CKA_WRAP,            // posWrap
    CKA_UNWRAP,          // posUnWrap
    CKA_DERIVE,          // posDerive
    CKA_TRUSTED);        // posTrusted

procedure AddToAttributes(var Attr: CK_ATTRIBUTES; Flags: TPkcs11ObjectStorages);
var
  s: TPkcs11ObjectStorage;
begin
  for s := low(POS2CKA) to high(POS2CKA) do
    if s in Flags then
      if Attr.Find(POS2CKA[s]) = nil then
        Attr.Add(POS2CKA[s], true);
  if posX509 in Flags then
    Attr.Add(CKA_CERTIFICATE_TYPE, ToULONG(CKC_X_509))
  else if posX509Attr in Flags then
    Attr.Add(CKA_CERTIFICATE_TYPE, ToULONG(CKC_X_509_ATTR_CERT))
  else if posWtls in Flags then
    Attr.Add(CKA_CERTIFICATE_TYPE, ToULONG(CKC_WTLS));
end;

function DefaultKeyStorageFlags(kt: CK_KEY_TYPE): TPkcs11ObjectStorages;
begin
  case kt of
    CKK_AES,
    CKK_RSA:
      result := [posEncrypt, posDecrypt, posSign, posVerify, posWrap, posUnWrap];
    CKK_DH:
      result := [posDerive];
    CKK_DSA,
    CKK_EC_EDWARDS:
      result := [posSign, posVerify];
    CKK_EC:
      result := [posDerive, posSign, posVerify];
  else
    result := [];
  end;
end;

function EccBitsFromPointLen(bytes: integer; out bits: cardinal): boolean;
begin
  // ECC uncompressed key is ASN1/DER encoded as 04 41 04 ..x.. ..y..
  if bytes <= 3 then
  begin
    result := false;
    exit;
  end;
  if bytes <= 127 + 2 then
    bits := (bytes - 3) * 4
  else if bytes <= 255 + 3 then
    bits := (bytes - 4) * 4
  else
    bits := (bytes - 5) * 4;
  result := true;
end;


{ TPkcs11 }

constructor TPkcs11.Create(const aLibraryName: TFileName);
begin
  inherited Create;
  if not Load(aLibraryName) then
    raise EPkcs11.CreateUtf8('%: error loading %', [self, aLibraryName]);
end;

destructor TPkcs11.Destroy;
begin
  UnLoad;
  inherited Destroy;
end;

function TPkcs11.Loaded: boolean;
begin
  result := (self <> nil) and
            (fHandle <> 0);
end;

procedure TPkcs11.EnsureLoaded(const ctxt: ShortString);
begin
  if not Loaded then
    raise EPkcs11.CreateUtf8('%.%: no library loaded', [self, ctxt]);
end;

procedure TPkcs11.EnsureSession(const ctxt: ShortString);
begin
  if (self = nil) or
     (fSession = 0) then
    raise EPkcs11.CreateUtf8('%.% requires a session', [self, ctxt]);
end;

procedure TPkcs11.Check(res: CK_RVULONG; const ctxt: ShortString;
  unlock: boolean);
begin
  if res = CKR_SUCCESS then
    exit;
  if unlock then
    Safe.UnLock;
  raise EPkcs11.CreateUtf8(
    '%.%: failed as % (%)', [self, ctxt, ToText(RV(res))^, res]);
end;

procedure TPkcs11.CheckAttr(res: CK_RVULONG);
begin
  if (res <> CKR_SUCCESS) and
     (res <> CKR_SENSITIVE) and // more tolerant
     (res <> CKR_INVALID) then
    Check(res, 'GetAttributeValue');
end;

function DoNotify(hSession: CK_SESSION_HANDLE; event: CK_NOTIFICATION;
  pApplication: TPkcs11): CK_RVULONG; cdecl;
begin
  result := CKR_SUCCESS;
  if (event = CKN_SURRENDER) and
     (pApplication <> nil) and
     (pApplication.fSession <> 0) and
     Assigned(pApplication.OnNotify) and
     pApplication.OnNotify(pApplication, pApplication.fSessionSlot) then
    result := CKR_ABORT; // callback returned TRUE to abort
end;

procedure TPkcs11.Open(slot: TPkcs11SlotID; rw: boolean);
const
  FLAGS: array[boolean] of CKSE_FLAGS = (
    [CKF_SERIAL_SESSION],
    [CKF_RW_SESSION, CKF_SERIAL_SESSION]);
var
  notif: pointer;
begin
  if fSession <> 0 then
    raise EPkcs11.CreateUtf8('%: pending session', [self]);
  Safe.Lock;
  try
    fSessionSlot := slot;
    fSessionFlags := [];
    notif := nil;
    if Assigned(fOnNotify) then
      notif := @DoNotify;
    Check(fC.OpenSession(slot, byte(FLAGS[rw]), pointer(self), notif, fSession),
      'Open', {unlock=}true);
    if rw then
      include(fSessionFlags, sfRW);
  except
    begin
      Safe.UnLock;
      raise;
    end;
  end;
end;

procedure TPkcs11.Open(slot: TPkcs11SlotID; const pin: RawUtf8;
  rw: boolean; so: boolean);
const
  FLAGS: array[boolean] of CK_USER_TYPE = (CKU_USER, CKU_SO);
begin
  Open(slot, rw);
  try
    Check(fC.Login(fSession, FLAGS[so], pointer(pin), length(pin)), 'Login');
    include(fSessionFlags, sfLogIn);
  except
    begin
      Close;
      raise;
    end;
  end;
end;

function TPkcs11.Open(const name: RawUtf8; rw, namecaseins: boolean): PPkcs11Token;
begin
  result := TokenByName(name, namecaseins);
  if result <> nil then
    Open(result^.Slot, rw);
end;

function TPkcs11.Open(const name: RawUtf8; const pin: RawUtf8;
  rw, so, namecaseins: boolean): PPkcs11Token;
begin
  result := TokenByName(name, namecaseins);
  if result <> nil then
    Open(result^.Slot, pin, rw, so);
end;

procedure TPkcs11.Close;
begin
  if fSession = 0 then
    exit; // it won't hurt if called more than once
  if sfLogIn in fSessionFlags then
    fC.Logout(fSession);     // no error check
  fC.CloseSession(fSession); // no error check
  fSession := 0;
  fSessionFlags := [];
  Safe.UnLock; // always eventually release lock
end;

function TPkcs11.Load(const aLibraryName: TFileName): boolean;
var
  getlist: TfC_GetFunctionList;
  info: CK_INFO;
begin
  result := false;
  UnLoad;
  fHandle := LibraryOpen(aLibraryName);
  if fHandle = 0 then
    exit;
  FillCharFast(info, SizeOf(info), 0);
  getlist := LibraryResolve(fHandle, 'C_GetFunctionList');
  if Assigned(getlist) and
     (getlist(fC) = CKR_SUCCESS) and
     (fC^.Initialize(nil) = CKR_SUCCESS) and // Initialize() may take 10 secs
     (fC^.GetInfo(info) = CKR_SUCCESS) and
     (info.cryptokiVersion.major >= 2) then
  begin
    fLibraryName := aLibraryName;
    fApiNum := info.cryptokiVersion;
    fVersionNum := info.libraryVersion;
    FormatUtf8('%.%', [fApiNum.major, fApiNum.minor], fApi);
    FormatUtf8('%.%', [fVersionNum.major, fVersionNum.minor], fVersion);
    UnPad(info.manufacturerID, SizeOf(info.manufacturerID), fManufacturer);
    UnPad(info.libraryDescription, SizeOf(info.libraryDescription), fDescription);
    result := true;
    exit;
  end;
  Unload;
end;

procedure TPkcs11.UnLoad;
begin
  if fHandle = 0 then
    exit;
  if Assigned(fC) then
    fC^.Finalize(nil);
  fC := nil;
  LibraryClose(fHandle);
  fHandle := 0;
  fLibraryName := '';
  fApi := '';
  fVersion := '';
  word(fApiNum) := 0;
  word(fVersionNum) := 0;
  fManufacturer := '';
  fDescription := '';
  fSlots := nil;
  fSlotIDs := nil;
  fTokens := nil;
end;

procedure TPkcs11.RetrieveConfig(IncludeVoidSlots: boolean);
var
  n: CK_ULONG;
  s: array of CK_SLOT_ID; // may be 64-bit on POSIX
  res: integer;
  i: PtrInt;
begin
  EnsureLoaded('RetrieveConfig');
  fSlots := nil;
  fSlotIDs := nil;
  fTokens := nil;
  if not Assigned(fC^.GetSlotList) then
    exit;
  fSafe.Lock;
  try
    Check(fC^.GetSlotList(not IncludeVoidSlots, nil, n), 'GetSlotList');
    if n = 0 then
      exit;
    repeat // need to loop because token number may have changed in-between!
      SetLength(s, n);
      res := fC^.GetSlotList(not IncludeVoidSlots, pointer(s), n);
    until res <> CKR_BUFFER_TOOSMALL;
    Check(res, 'GetSlotList');
    for i := 0 to CK_LONG(n) - 1 do
      UpdateConfig(s[i]);
  finally
    fSafe.UnLock;
  end;
end;

procedure TPkcs11.UpdateConfig(SlotID: TPkcs11SlotID);
var
  i: PtrInt;
  sltnfo: CK_SLOT_INFO;
  toknfo: CK_TOKEN_INFO;
  mecnfo: CK_MECHANISM_INFO;
  s: PPkcs11Slot;
  mn, res: CK_ULONG;
  m: array of CK_ULONG;
begin
  EnsureLoaded('UpdateConfig');
  fSafe.Lock;
  try
    FillCharFast(sltnfo, SizeOf(sltnfo), 0);
    Check(fC^.GetSlotInfo(SlotID, sltnfo), 'GetSlotInfo');
    AddInteger(TIntegerDynArray(fSlotIDs), SlotID, {nodup=}true);
    s := SlotByID(SlotID, {addnew=}true);
    FillSlot(SlotID, sltnfo, s^);
    if not (CKF_TOKEN_PRESENT in s^.Flags) then
    begin
      for i := 0 to length(fTokens) - 1 do
        if fTokens[i].Slot = SlotID then // there is no such token any more
        begin
          DynArray(TypeInfo(TPkcs11TokenDynArray), fTokens).Delete(i);
          break;
        end;
      exit;
    end;
    mn := 64;
    repeat
      if length(m) < CK_LONG(mn) then
        SetLength(m, mn);
      res := fC^.GetMechanismList(SlotID, pointer(m), mn);
    until res <> CKR_BUFFER_TOOSMALL; // loop if 64 was not enough
    Check(res, 'GetMechanismList');
    SetLength(s^.Mechanism, mn);
    for i := 0 to CK_LONG(mn) - 1 do
    begin
      Check(fC^.GetMechanismInfo(SlotID, m[i], mecnfo), 'GetMechanismInfo');
      with s^.Mechanism[i] do
      begin
        Kind := MECHANISM_TYPE(m[i]);
        MinKey := mecnfo.ulMinKeySize;
        MaxKey := mecnfo.ulMaxKeySize;
        Flags := CKM_FLAGS(cardinal(mecnfo.flags));
      end;
    end;
    FillCharFast(toknfo, SizeOf(toknfo), 0);
    Check(fC^.GetTokenInfo(SlotID, toknfo), 'GetTokenInfo');
    FillToken(SlotID, toknfo, TokenByID(SlotID, {addnew=}true)^);
  finally
    fSafe.UnLock;
  end;
end;

function TPkcs11.WaitForSlotEvent(NotBlocking: boolean): TPkcs11SlotID;
var
  flags: CK_ULONG;
  slotid: CK_SLOT_ID;
  res: CK_RVULONG;
begin
  EnsureLoaded('WaitForSlotEvent');
  flags := 0;
  if NotBlocking then
    flags := CKF_DONT_BLOCK;
  result := PKCS11_NOSLOT;
  fSafe.Lock;
  try
    res := fC.WaitForSlotEvent(flags, slotid, nil);
    if res = CKR_NOEVENT then
      exit;
    Check(res, 'WaitForSlotEvent');
    UpdateConfig(slotid);
    result := slotid;
  finally
    fSafe.UnLock;
  end;
end;

function TPkcs11.SlotByID(SlotID: TPkcs11SlotID; AddNew: boolean): PPkcs11Slot;
var
  i, n: PtrInt;
begin
  if (self <> nil) and
     (integer(SlotID) >= 0) then
  begin
    result := pointer(fSlots);
    n := length(fSlots);
    for i := 0 to n - 1 do
      if result^.Slot = SlotID then
        exit
      else
        inc(result);
    if AddNew then
    begin
      SetLength(fSlots, n + 1);
      result := @fSlots[n];
      result^.Slot := SlotID;
      exit;
    end;
  end;
  result := nil;
end;

function TPkcs11.TokenByID(SlotID: TPkcs11SlotID; AddNew: boolean): PPkcs11Token;
var
  i, n: PtrInt;
begin
  if (self <> nil) and
     (integer(SlotID) >= 0) then
  begin
    result := pointer(fTokens);
    n := length(fTokens);
    for i := 0 to n - 1 do
      if result^.Slot = SlotID then
        exit
      else
        inc(result);
    if AddNew then
    begin
      SetLength(fTokens, n + 1);
      result := @fTokens[n];
      result^.Slot := SlotID;
      exit;
    end;
  end;
  result := nil;
end;

function TPkcs11.TokenByName(const TokenName: RawUtf8;
  CaseInsensitive: boolean): PPkcs11Token;
var
  i: PtrInt;
begin
  if (self <> nil) and
     (TokenName <> '') then
  begin
    result := pointer(fTokens);
    for i := 0 to length(fTokens) - 1 do
      if (CaseInsensitive and
          IdemPropNameU(TokenName, result^.Name)) or
         ((not CaseInsensitive) and
          (TokenName = result^.Name)) then
          exit
        else
          inc(result);
  end;
  result := nil;
end;

function TPkcs11.GetObjects(Filter: PCK_ATTRIBUTES;
  Values: PRawByteStringDynArray): TPkcs11ObjectDynArray;
var
  n, count, u: CK_ULONG;
  i: PtrInt;
  s: TPkcs11ObjectStorage;
  b: boolean;
  obj: array[byte] of CK_OBJECT_HANDLE;
  arr: CK_ATTRIBUTES;
begin
  EnsureSession('GetObjects');
  result := nil;
  count := 0;
  if Filter = nil then
    u := fC.FindObjectsInit(fSession, nil, 0) // find all
  else // search from some attributes
    u := fC.FindObjectsInit(fSession, pointer(Filter^.Attrs), Filter^.Count);
  Check(u, 'FindObjectsInit');
  arr.Clear;
  arr.Add([CKA_CLASS, CKA_LABEL, CKA_APPLICATION, CKA_UNIQUE_ID,
           CKA_START_DATE, CKA_END_DATE, CKA_ID, CKA_SERIAL_NUMBER,
           CKA_ISSUER, CKA_SUBJECT, CKA_OWNER, CKA_URL, CKA_CERTIFICATE_TYPE,
           CKA_KEY_TYPE, CKA_KEY_GEN_MECHANISM, CKA_MODULUS_BITS,
           CKA_EC_POINT, CKA_VALUE_LEN]);
  for s := low(POS2CKA) to high(POS2CKA) do
    arr.Add(POS2CKA[s]);
  if Values <> nil then
    arr.Add(CKA_VALUE);
  repeat
    n := 0;
    Check(fC.FindObjects(fSession, @obj, length(obj), n), 'FindObjects');
    if n = 0 then
      break;
    SetLength(result, n + count);
    if Values <> nil then
      SetLength(Values^, n + count);
    for i := 0 to CK_LONG(n) - 1 do
    begin
      arr.ClearValues;    // keep _type
      CheckAttr(fC.GetAttributeValue(
        fSession, obj[i], pointer(arr.Attrs), arr.Count)); // get length
      arr.AllocateValues; // fill pValue
      CheckAttr(fC.GetAttributeValue(
        fSession, obj[i], pointer(arr.Attrs), arr.Count)); // get data
      if arr.Find(CKA_CLASS, u) then
        with result[count] do
        begin
          ObjClass := OBJECT_CLASS(u);
          for s := low(POS2CKA) to high(POS2CKA) do
            if arr.Find(POS2CKA[s], b) and b then
              include(StorageFlags, s);
          arr.Find(CKA_LABEL, StorageLabel);
          arr.Find(CKA_APPLICATION, Application);
          arr.Find(CKA_ID, StorageID, {hex=}true);
          arr.Find(CKA_UNIQUE_ID, UniqueID);
          arr.Find(CKA_SUBJECT, Subject);
          if arr.Find(CKA_CERTIFICATE_TYPE, u) then
          begin
            case CERTIFICATE_TYPE(u) of
              CKC_X_509:
                include(StorageFlags, posX509);
              CKC_X_509_ATTR_CERT:
                include(StorageFlags, posX509Attr);
              CKC_WTLS:
                include(StorageFlags, posWtls);
            end;
            arr.Find(CKA_SERIAL_NUMBER, Serial);
            arr.Find(CKA_ISSUER, Issuer);
            if Subject = '' then
              arr.Find(CKA_OWNER, Subject);
            if Application = '' then
              arr.Find(CKA_URL, Application);
          end;
          arr.Find(CKA_START_DATE, Start);
          arr.Find(CKA_END_DATE, Stop);
          if arr.Find(CKA_KEY_TYPE, u) then
          begin
            KeyType := KEY_TYPE(u);
            if arr.Find(CKA_KEY_GEN_MECHANISM, u) then
              KeyGen := MECHANISM_TYPE(u);
            if arr.Find(CKA_MODULUS_BITS, u) then // for RSA
              KeyBits := u
            else if not EccBitsFromPointLen(arr.FindLen(CKA_EC_POINT), KeyBits) then
              if arr.Find(CKA_VALUE_LEN, u) then // CKK_EC
                KeyBits := u shl 3; // CKK_AES
          end;
          SessionHandle := obj[i];
          if Values <> nil then
            arr.Find(CKA_VALUE, Values^[count]);
          inc(count);
        end;
    end;
  until false;
  Check(fc.FindObjectsFinal(fSession), 'FindObjectsFinal');
  if count <> CK_ULONG(length(result)) then
    SetLength(result, count);
 if (Values <> nil) and
    (count <> CK_ULONG(length(Values^))) then
   SetLength(Values^, count);
end;

function TPkcs11.GetObject(ObjectClass: CK_OBJECT_CLASS; out Info: TPkcs11Object;
  const StorageLabel, StorageID: RawUtf8; Value: PRawByteString): boolean;
var
  attr: CK_ATTRIBUTES;
  res: TPkcs11ObjectDynArray;
  val: TRawByteStringDynArray;
  valp: PRawByteStringDynArray;
begin
  EnsureSession('GetObject');
  result := false;
  FillCharFast(Info, SizeOf(Info), 0);
  attr.New(ObjectClass, StorageLabel, StorageID);
  if Value = nil then
    valp := nil
  else
    valp := @val;
  res := GetObjects(@attr, valp);
  if res = nil then
    exit;
  if Value <> nil then
    if length(val) <> 1 then
      exit
    else
      Value^ := val[0];
  result := true;
end;

function TPkcs11.GetObject(ObjectClass: CK_OBJECT_CLASS;
  const StorageLabel, StorageID: RawUtf8): CK_OBJECT_HANDLE;
var
  attr: CK_ATTRIBUTES;
  res: TPkcs11ObjectDynArray;
begin
  EnsureSession('GetObject');
  attr.New(ObjectClass, StorageLabel, StorageID);
  res := GetObjects(@attr);
  if length(res) = 1 then
    result := res[0].SessionHandle
  else
    result := CK_INVALID_HANDLE; // return 0 on error
end;

function TPkcs11.GetRandom(Len: PtrInt): RawByteString;
begin
  EnsureSession('GetRandom');
  if Len > 0 then
    FastSetRawByteString(result, nil, Len);
  if (Len <= 0) or
     (fC.GenerateRandom(fSession, pointer(result), Len) <> CKR_SUCCESS) then
    result := '';
end;

function TPkcs11.Sign(Data: pointer; Len: PtrInt; PrivKey: CK_OBJECT_HANDLE;
  var Mechanism: CK_MECHANISM): RawByteString;
var
  reslen: CK_ULONG;
begin
  result := '';
  if (Data = nil) or
     (Len <= 0) or
     (PrivKey = CK_INVALID_HANDLE) then
    exit;
  EnsureSession('Sign');
  Check(fC.SignInit(fSession, Mechanism, PrivKey), 'SignInit');
  Check(fC.Sign(fSession, nil, 0, nil, reslen), 'Sign');
  SetLength(result, reslen);
  Check(fC.Sign(fSession, Data, Len, pointer(result), reslen), 'Sign');
  if len <> length(result) then
    SetLength(result, reslen);
end;

function TPkcs11.Verify(Data, Sig: pointer; DataLen, SigLen: PtrInt;
  PubKey: CK_OBJECT_HANDLE; var Mechanism: CK_MECHANISM): boolean;
var
  res: CK_RVULONG;
begin
  EnsureSession('Verify');
  result := false;
  if (Data = nil) or
     (Sig = nil) or
     (DataLen <= 0) or
     (SigLen <= 0) or
     (PubKey = CK_INVALID_HANDLE) then
    exit;
  Check(fC.VerifyInit(fSession, Mechanism, PubKey), 'VerifyInit');
  res := fC.Verify(fSession, Data, DataLen, Sig, SigLen);
  case res of
    CKR_SUCCESS:
      result := true;
    CKR_SIGNINVALID:
      exit;
  else
    Check(res, 'Verify'); // fatal error raise EPkcs11 exception
  end;
end;

procedure TPkcs11.InitToken(SlotID: TPkcs11SlotID;
  const SOPin, TokenLabel: RawUtf8);
begin
  EnsureLoaded('InitToken');
  fSafe.Lock;
  try
    Check(fC.InitToken(SlotID, pointer(SOPin), length(SOPin),
      pointer(Pad(TokenLabel, 32))), 'InitToken');
  finally
    fSafe.UnLock;
  end;
end;

procedure TPkcs11.InitUserPin(SlotID: TPkcs11SlotID; const SOPin, UserPin: RawUtf8);
begin
  EnsureLoaded('InitUserPin');
  Open(SlotID, SOPin, {rw=}true, {so=}true); // need a R/W SO session
  try
    Check(fC.InitPIN(fSession, pointer(UserPin), length(UserPin)), 'InitPIN');
  finally
    Close;
  end;
end;

procedure TPkcs11.ChangeUserPin(SlotID: TPkcs11SlotID; const OldPin, NewPin: RawUtf8);
begin
  EnsureLoaded('ChangePin');
  Open(SlotID, OldPin, {rw=}true, {so=}false); // need a R/W User session
  try
    Check(fC.SetPIN(fSession, pointer(OldPin), length(OldPin),
      pointer(NewPin), length(NewPin)), 'SetPIN');
  finally
    Close;
  end;
end;


function TPkcs11.SessionGetAttribute(
  obj: CK_OBJECT_HANDLE; attr: CK_ATTRIBUTE_TYPE): RawUtf8;
var
  a: CK_ATTRIBUTE;
begin
  result := '';
  a._type := ToULONG(attr);
  a.pValue := nil;
  a.ulValueLen := 0;
  if (fC.GetAttributeValue(fSession, obj, @a, 1) <> CKR_SUCCESS) or
     (a.ulValueLen = CK_UNAVAILABLE_INFORMATION) then
    exit; // impossible to retrieve the length
  FastSetString(result, nil, a.ulValueLen);
  a.pValue := pointer(result); // copy the attribute value to result
  if fC.GetAttributeValue(fSession, obj, @a, 1) <> CKR_SUCCESS then
    result := '';
end;

function TPkcs11.SessionCreateObject(const a: CK_ATTRIBUTES): RawUtf8;
var
  obj: CK_OBJECT_HANDLE;
begin
  Check(fC.CreateObject(
    fSession, pointer(a.Attrs), a.Count, obj), 'CreateObject');
  result := SessionGetAttribute(obj, CKA_UNIQUE_ID); // just generated
end;

function TPkcs11.AddSessionData(const Application, DataLabel: RawUtf8;
  const Data: RawByteString; const DerID: RawByteString): RawUtf8;
var
  a: CK_ATTRIBUTES;
begin
  if not (sfRW in fSessionFlags) then
    raise EPkcs11.CreateUtf8('%.AddSessionData requires a R/W session', [self]);
  a.New(CKO_DATA, DataLabel);
  if Application <> '' then
    a.Add(CKA_APPLICATION, Application);
  if DerID <> '' then
    a.Add(CKA_OBJECT_ID, DerID);
  a.Add(CKA_VALUE, Data);
  result := SessionCreateObject(a);
end;


initialization
  // paranoid cross-platform validation
  assert(cardinal(1 shl ord(CKF_ERROR_STATE)) = $01000000);
  assert(cardinal(CKK_SHA512_T_HMAC) = $00000045);
  assert(cardinal(1 shl ord(CKF_EXTENSION)) = $80000000);
  assert(SizeOf(CK_DATE) = 8);
  // allow proper JSON serialization of TPkcs11Slot/TPkcs11Token/TPkcs11
  Rtti.RegisterTypes([
    TypeInfo(CKT_FLAGS),
    TypeInfo(CKSL_FLAGS),
    TypeInfo(CK_MECHANISM_TYPE),
    TypeInfo(CKM_FLAGS),
    TypeInfo(CK_OBJECT_CLASS),
    TypeInfo(CK_KEY_TYPE),
    TypeInfo(TPkcs11ObjectStorages)
    ]);
  Rtti.RegisterFromText([
    TypeInfo(TPkcs11Mechanisms),
      'type:CK_MECHANISM_TYPE min,max:cardinal flags:CKM_FLAGS',
    TypeInfo(TPkcs11Slot),
      'slot:cardinal description,manufacturer:RawUtf8 flags:CKSL_FLAGS' +
      ' mechanism:TPkcs11Mechanisms hwmaj,hwmin,fwmaj,fwmin:byte',
    TypeInfo(TPkcs11ObjectDynArray),
      'class:CK_OBJECT_CLASS id,label:RawUtf8 flags:TPkcs11ObjectStorages' +
      ' keytype:CK_KEY_TYPE keygen:CK_MECHANISM_TYPE keybits:cardinal ' +
      ' start,end:TDateTime app:RawUtf8 sub,sn,iss,uid:RawByteString hdl:cardinal',
    TypeInfo(TPkcs11Token),
      'slot:cardinal name,manufacturer,model,serial,time:RawUtf8 flags:CKT_FLAGS' +
      ' sessions,maxsessions,minpin,maxpin: integer'
    ]);


end.

