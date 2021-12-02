/// Framework Core Low-Level Wrappers to the Operating-System API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os;

{
  *****************************************************************************

  Cross-platform functions shared by all framework units
  - Some Cross-System Type and Constant Definitions
  - Gather Operating System Information
  - Operating System Specific Types (e.g. TWinRegistry)
  - Unicode, Time, File, Console, Library process
  - Per Class Properties O(1) Lookup via vmtAutoTable Slot (e.g. for RTTI cache)
  - TSynLocker/TSynLocked and Low-Level Threading Features
  - Unix Daemon and Windows Service Support

   Aim of this unit is to centralize most used OS-specific API calls, like a
  SysUtils unit on steroids, to avoid $ifdef/$endif in "uses" clauses.
   In practice, no "Windows", nor "Linux/Posix" reference should be needed in
  regular units, once mormot.core.os is included. :)
   This unit only refers to mormot.core.base so can be used almost stand-alone.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef OSWINDOWS}
  Windows, // needed here e.g. for redefinition of standard types
  Messages,
  {$endif OSWINDOWS}
  classes,
  contnrs,
  syncobjs,
  types,
  sysutils,
  mormot.core.base;


{ ****************** Some Cross-System Type and Constant Definitions }

const
  /// void HTTP Status Code (not a standard value, for internal use only)
  HTTP_NONE = 0;
  /// HTTP Status Code for "Continue"
  HTTP_CONTINUE = 100;
  /// HTTP Status Code for "Switching Protocols"
  HTTP_SWITCHINGPROTOCOLS = 101;
  /// HTTP Status Code for "Success"
  HTTP_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  HTTP_CREATED = 201;
  /// HTTP Status Code for "Accepted"
  HTTP_ACCEPTED = 202;
  /// HTTP Status Code for "Non-Authoritative Information"
  HTTP_NONAUTHORIZEDINFO = 203;
  /// HTTP Status Code for "No Content"
  HTTP_NOCONTENT = 204;
  /// HTTP Status Code for "Reset Content"
  HTTP_RESETCONTENT = 205;
  /// HTTP Status Code for "Partial Content"
  HTTP_PARTIALCONTENT = 206;
  /// HTTP Status Code for "Multiple Choices"
  HTTP_MULTIPLECHOICES = 300;
  /// HTTP Status Code for "Moved Permanently"
  HTTP_MOVEDPERMANENTLY = 301;
  /// HTTP Status Code for "Found"
  HTTP_FOUND = 302;
  /// HTTP Status Code for "See Other"
  HTTP_SEEOTHER = 303;
  /// HTTP Status Code for "Not Modified"
  HTTP_NOTMODIFIED = 304;
  /// HTTP Status Code for "Use Proxy"
  HTTP_USEPROXY = 305;
  /// HTTP Status Code for "Temporary Redirect"
  HTTP_TEMPORARYREDIRECT = 307;
  /// HTTP Status Code for "Bad Request"
  HTTP_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  HTTP_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  HTTP_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  HTTP_NOTFOUND = 404;
  // HTTP Status Code for "Method Not Allowed"
  HTTP_NOTALLOWED = 405;
  // HTTP Status Code for "Not Acceptable"
  HTTP_NOTACCEPTABLE = 406;
  // HTTP Status Code for "Proxy Authentication Required"
  HTTP_PROXYAUTHREQUIRED = 407;
  /// HTTP Status Code for "Request Time-out"
  HTTP_TIMEOUT = 408;
  /// HTTP Status Code for "Conflict"
  HTTP_CONFLICT = 409;
  /// HTTP Status Code for "Payload Too Large"
  HTTP_PAYLOADTOOLARGE = 413;
  /// HTTP Status Code for "Range Not Satisfiable"
  HTTP_RANGENOTSATISFIABLE = 416;
  /// HTTP Status Code for "Internal Server Error"
  HTTP_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  HTTP_NOTIMPLEMENTED = 501;
  /// HTTP Status Code for "Bad Gateway"
  HTTP_BADGATEWAY = 502;
  /// HTTP Status Code for "Service Unavailable"
  HTTP_UNAVAILABLE = 503;
  /// HTTP Status Code for "Gateway Timeout"
  HTTP_GATEWAYTIMEOUT = 504;
  /// HTTP Status Code for "HTTP Version Not Supported"
  HTTP_HTTPVERSIONNONSUPPORTED = 505;

/// retrieve the HTTP reason text from its integer code
// - e.g. StatusCodeToReason(200)='OK'
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - see also StatusCodeToErrorMsg() from mormot.core.text if you need
// the HTTP error as both integer and text, returned as ShortString
function StatusCodeToReason(Code: cardinal): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the HTTP reason text from its integer code
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8); overload;

/// returns true for successful HTTP status codes, i.e. in 200..399 range
// - will map mainly SUCCESS (200), CREATED (201), NOCONTENT (204),
// PARTIALCONTENT (206), NOTMODIFIED (304) or TEMPORARYREDIRECT (307) codes
// - any HTTP status not part of this range will be identified as erronous
// request in the internal server statistics
function StatusCodeIsSuccess(Code: integer): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check the supplied HTTP header to not contain more than one EOL
// - to avoid unexpected HTTP body injection, e.g. from unsafe business code
function IsInvalidHttpHeader(head: PUtf8Char; headlen: PtrInt): boolean;


const
  /// HTTP header name for the content type, as defined in the corresponding RFC
  HEADER_CONTENT_TYPE = 'Content-Type: ';

  /// HTTP header name for the content type, in upper case
  // - as defined in the corresponding RFC
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  HEADER_CONTENT_TYPE_UPPER = 'CONTENT-TYPE: ';

  /// HTTP header name for the client IP, in upper case
  // - as defined in our HTTP server classes
  // - could be used e.g. with IdemPChar() to retrieve the remote IP address
  HEADER_REMOTEIP_UPPER = 'REMOTEIP: ';

  /// HTTP header name for the authorization token, in upper case
  // - could be used e.g. with IdemPChar() to retrieve a JWT value
  // - will detect header computed e.g. by motmot.net.http's
  // AuthorizationBearer()
  HEADER_BEARER_UPPER = 'AUTHORIZATION: BEARER ';

  /// MIME content type used for JSON communication (as used by the Microsoft
  // WCF framework and the YUI framework)
  JSON_CONTENT_TYPE = 'application/json; charset=UTF-8';

  /// HTTP header for MIME content type used for plain JSON
  // - i.e. 'Content-Type: application/json; charset=UTF-8'
  JSON_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + JSON_CONTENT_TYPE;

  /// MIME content type used for plain JSON, in upper case
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  JSON_CONTENT_TYPE_UPPER = 'APPLICATION/JSON';

  /// HTTP header for MIME content type used for plain JSON, in upper case
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  JSON_CONTENT_TYPE_HEADER_UPPER =
    HEADER_CONTENT_TYPE_UPPER + JSON_CONTENT_TYPE_UPPER;

  /// MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE = 'text/plain; charset=UTF-8';

  /// HTTP header for MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + TEXT_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE = 'text/html; charset=UTF-8';

  /// HTTP header for MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + HTML_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded XML
  XML_CONTENT_TYPE = 'text/xml; charset=UTF-8';

  /// HTTP header for MIME content type used for UTF-8 encoded XML
  XML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + XML_CONTENT_TYPE;

  /// MIME content type used for raw binary data
  BINARY_CONTENT_TYPE = 'application/octet-stream';

  /// MIME content type used for raw binary data, in upper case
  BINARY_CONTENT_TYPE_UPPER = 'APPLICATION/OCTET-STREAM';

  /// HTTP header for MIME content type used for raw binary data
  BINARY_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + BINARY_CONTENT_TYPE;

  /// MIME content type used for a JPEG picture
  JPEG_CONTENT_TYPE = 'image/jpeg';

  /// internal HTTP content-type for efficient static file sending
  // - detected e.g. by http.sys' THttpApiServer.Request or via the NGINX
  // X-Accel-Redirect header's THttpServer.Process (see
  // THttpServer.NginxSendFileFrom) for direct sending with no local bufferring
  // - the OutCustomHeader should contain the proper 'Content-type: ....'
  // corresponding to the file (e.g. by calling GetMimeContentType() function)
  STATICFILE_CONTENT_TYPE = '!STATICFILE';

  /// internal HTTP content-type Header for efficient static file sending
  STATICFILE_CONTENT_TYPE_HEADER =
    HEADER_CONTENT_TYPE + STATICFILE_CONTENT_TYPE;

  /// uppercase version of HTTP header for static file content serving
  STATICFILE_CONTENT_TYPE_HEADER_UPPPER =
    HEADER_CONTENT_TYPE_UPPER + STATICFILE_CONTENT_TYPE;

  /// used to notify e.g. the THttpServerRequest not to wait for any response
  // from the client
  // - is not to be used in normal HTTP process, but may be used e.g. by
  // TWebSocketProtocolRest.ProcessFrame() to avoid to wait for an incoming
  // response from the other endpoint
  NORESPONSE_CONTENT_TYPE = '!NORESPONSE';

  /// JSON compatible representation of a boolean value, i.e. 'false' and 'true'
  // - can be used e.g. in logs, or anything accepting a ShortString
  BOOL_STR: array[boolean] of string[7] = (
    'false', 'true');

  /// the JavaScript-like values of non-number IEEE constants
  // - as recognized by FloatToShortNan, and used by TTextWriter.Add()
  // when serializing such single/double/extended floating-point values
  JSON_NAN: array[TFloatNan] of string[11] = (
    '0', '"NaN"', '"Infinity"', '"-Infinity"');

var
  /// MIME content type used for JSON communication
  // - i.e. 'application/json; charset=UTF-8'
  // - this global will be initialized with JSON_CONTENT_TYPE constant, to
  // avoid a memory allocation each time it is assigned to a variable
  JSON_CONTENT_TYPE_VAR: RawUtf8;

  /// HTTP header for MIME content type used for plain JSON
  // - this global will be initialized with JSON_CONTENT_TYPE_HEADER constant,
  // to avoid a memory allocation each time it is assigned to a variable
  JSON_CONTENT_TYPE_HEADER_VAR: RawUtf8;

  /// can be used to avoid a memory allocation for res := 'null'
  // - this global will be initialized with 'null' constant, to
  // avoid a memory allocation each time it is assigned to a variable
  NULL_STR_VAR: RawUtf8;

  /// JSON compatible representation of a boolean value, i.e. 'false' and 'true'
  // - can be used when a RawUtf8 string is expected
  // - this global will be initialized with 'false' and 'true' constants, to
  // avoid a memory allocation each time it is assigned to a variable
  BOOL_UTF8: array[boolean] of RawUtf8;


{ ****************** Gather Operating System Information }

type
  /// Exception types raised by this mormot.core.os unit
  EOSException = class(Exception);

  /// the recognized operating systems
  // - it will also recognize most Linux distributions
  TOperatingSystem = (
    osUnknown,
    osWindows,
    osLinux,
    osOSX,
    osBSD,
    osPOSIX,
    osArch,
    osAurox,
    osDebian,
    osFedora,
    osGentoo,
    osKnoppix,
    osMint,
    osMandrake,
    osMandriva,
    osNovell,
    osUbuntu,
    osSlackware,
    osSolaris,
    osSuse,
    osSynology,
    osTrustix,
    osClear,
    osUnited,
    osRedHat,
    osLFS,
    osOracle,
    osMageia,
    osCentOS,
    osCloud,
    osXen,
    osAmazon,
    osCoreOS,
    osAlpine,
    osAndroid);

  /// the recognized Windows versions
  // - defined even outside OSWINDOWS to access e.g. from monitoring tools
  TWindowsVersion = (
    wUnknown,
    w2000,
    wXP,
    wXP_64,
    wServer2003,
    wServer2003_R2,
    wVista,
    wVista_64,
    wServer2008,
    wServer2008_64,
    wSeven,
    wSeven_64,
    wServer2008_R2,
    wServer2008_R2_64,
    wEight,
    wEight_64,
    wServer2012,
    wServer2012_64,
    wEightOne,
    wEightOne_64,
    wServer2012R2,
    wServer2012R2_64,
    wTen,
    wTen_64,
    wServer2016,
    wServer2016_64,
    wEleven,
    wEleven_64,
    wServer2019_64,
    wServer2022_64);

  /// the running Operating System, encoded as a 32-bit integer
  TOperatingSystemVersion = packed record
    case os: TOperatingSystem of
    osUnknown: (
      b: array[0..2] of byte);
    osWindows: (
      win: TWindowsVersion;
      winbuild: word);
    osLinux: (
      utsrelease: array[0..2] of byte);
  end;

const
  /// the recognized Windows versions, as plain text
  // - defined even outside OSWINDOWS to allow process e.g. from monitoring tools
  WINDOWS_NAME: array[TWindowsVersion] of RawUtf8 = (
    '',
    '2000',
    'XP',
    'XP 64bit',
    'Server 2003',
    'Server 2003 R2',
    'Vista',
    'Vista 64bit',
    'Server 2008',
    'Server 2008 64bit',
    '7',
    '7 64bit',
    'Server 2008 R2',
    'Server 2008 R2 64bit',
    '8',
    '8 64bit',
    'Server 2012',
    'Server 2012 64bit',
    '8.1',
    '8.1 64bit',
    'Server 2012 R2',
    'Server 2012 R2 64bit',
    '10',
    '10 64bit',
    'Server 2016',
    'Server 2016 64bit',
    '11',
    '11 64bit',
    'Server 2019 64bit',
    'Server 2022 64bit');

  /// the recognized Windows versions which are 32-bit
  WINDOWS_32 = [
     w2000,
     wXP,
     wServer2003,
     wServer2003_R2,
     wVista,
     wServer2008,
     wSeven,
     wServer2008_R2,
     wEight,
     wServer2012,
     wEightOne,
     wServer2012R2,
     wTen,
     wEleven,
     wServer2016];

  /// translate one operating system (and distribution) into a its common name
  OS_NAME: array[TOperatingSystem] of RawUtf8 = (
    'Unknown',
    'Windows',
    'Linux',
    'OSX',
    'BSD',
    'POSIX',
    'Arch',
    'Aurox',
    'Debian',
    'Fedora',
    'Gentoo',
    'Knoppix',
    'Mint',
    'Mandrake',
    'Mandriva',
    'Novell',
    'Ubuntu',
    'Slackware',
    'Solaris',
    'Suse',
    'Synology',
    'Trustix',
    'Clear',
    'United',
    'RedHat',
    'LFS',
    'Oracle',
    'Mageia',
    'CentOS',
    'Cloud',
    'Xen',
    'Amazon',
    'CoreOS',
    'Alpine',
    'Android');

  /// translate one operating system (and distribution) into a single character
  // - may be used internally e.g. for a HTTP User-Agent header, as with
  // TFileVersion.UserAgent
  OS_INITIAL: array[TOperatingSystem] of AnsiChar = (
    '?', // Unknown
    'W', // Windows
    'L', // Linux
    'X', // OSX
    'B', // BSD
    'P', // POSIX
    'A', // Arch
    'a', // Aurox
    'D', // Debian
    'F', // Fedora
    'G', // Gentoo
    'K', // Knoppix
    'M', // Mint
    'm', // Mandrake
    'n', // Mandriva
    'N', // Novell
    'U', // Ubuntu
    'S', // Slackware
    's', // Solaris
    'u', // Suse
    'Y', // Synology
    'T', // Trustix
    'C', // Clear
    't', // United
    'R', // RedHat
    'l', // LFS
    'O', // Oracle
    'G', // Mageia
    'c', // CentOS
    'd', // Cloud
    'x', // Xen
    'Z', // Amazon
    'r', // CoreOS
    'p', // Alpine
    'J'  // Android (J=JVM)
    );

  /// the operating systems items which actually have a Linux kernel
  OS_LINUX = [
    osLinux,
    osArch .. osAndroid];

  /// the compiler family used
  COMP_TEXT = {$ifdef FPC}'Fpc'{$else}'Delphi'{$endif};

  /// the target Operating System used for compilation, as short text
  OS_TEXT =
    {$ifdef OSWINDOWS}
      'Win';
    {$else} {$ifdef OSDARWIN}
      'OSX';
    {$else}{$ifdef OSBSD}
      'BSD';
    {$else} {$ifdef OSANDROID}
      'Android';
    {$else} {$ifdef OSLINUX}
      'Linux';
    {$else}
      'Posix';
    {$endif OSLINUX}
    {$endif OSANDROID}
    {$endif OSBSD}
    {$endif OSDARWIN}
    {$endif OSWINDOWS}

  /// the CPU architecture used for compilation
  CPU_ARCH_TEXT =
    {$ifdef CPUX86}
      'x86'
    {$else} {$ifdef CPUX64}
      'x64'
    {$else} {$ifdef CPUARM}
      'arm' +
    {$else} {$ifdef CPUAARCH64}
      'aarch' +
    {$ifdef CPUPOWERPC}
      'ppc' +
    {$else} {$ifdef CPUSPARC}
      'sparc' +
    {$endif CPUSPARC}
    {$endif CPUPOWERPC}
    {$endif CPUARM}
    {$endif CPUAARCH64}
    {$ifdef CPU32}
      '32'
    {$else}
      '64'
    {$endif CPU32}
    {$endif CPUX64}
    {$endif CPUX86};

var
  /// the target Operating System used for compilation, as TOperatingSystem
  // - a specific Linux distribution may be detected instead of plain osLinux
  OS_KIND: TOperatingSystem =
    {$ifdef OSWINDOWS}
      osWindows
    {$else} {$ifdef OSDARWIN}
      osOSX
    {$else} {$ifdef OSBSD}
      osBSD
    {$else} {$ifdef OSANDROID}
      osAndroid
    {$else} {$ifdef OSLINUX}
      osLinux
    {$else}
      osPOSIX
    {$endif OSLINUX}
    {$endif OSANDROID}
    {$endif OSBSD}
    {$endif OSDARWIN}
    {$endif OSWINDOWS};

  /// the current Operating System version, as retrieved for the current process
  // - contains e.g. 'Windows Seven 64 SP1 (6.1.7601)' or
  // 'Ubuntu 16.04.5 LTS - Linux 3.13.0 110 generic#157 Ubuntu SMP Mon Feb 20 11:55:25 UTC 2017'
  OSVersionText: RawUtf8;
  /// some addition system information as text, e.g. 'Wine 1.1.5'
  // - also always appended to OSVersionText high-level description
  // - use if PosEx('Wine', OSVersionInfoEx) > 0 then to check for Wine presence
  OSVersionInfoEx: RawUtf8;
  /// the current Operating System version, as retrieved for the current process
  // and computed by ToTextOS(OSVersionInt32)
  // - contains e.g. 'Windows Vista' or 'Ubuntu 5.4.0'
  OSVersionShort: RawUtf8;

  /// some textual information about the current CPU
  // - contains e.g. '4 x Intel(R) Core(TM) i5-7300U CPU @ 2.60GHz (x64)'
  CpuInfoText: RawUtf8;
  /// some textual information about the current computer hardware, from BIOS
  // - contains e.g. 'LENOVO 20HES23B0U ThinkPad T470'
  BiosInfoText: RawUtf8;

  {$ifdef OSLINUXANDROID}
  /// contains the Flags: or Features: value of Linux /proc/cpuinfo
  CpuInfoFeatures: RawUtf8;
  {$endif OSLINUXANDROID}

  /// the running Operating System
  OSVersion32: TOperatingSystemVersion;
  /// the running Operating System, encoded as a 32-bit integer
  OSVersionInt32: integer absolute OSVersion32;

/// convert an Operating System type into its text representation
// - returns e.g. 'Windows Vista' or 'Ubuntu'
function ToText(const osv: TOperatingSystemVersion): RawUtf8; overload;

/// convert an Operating System type into its one-word text representation
// - returns e.g. 'Vista' or 'Ubuntu'
function ToTextShort(const osv: TOperatingSystemVersion): RawUtf8;

/// convert a 32-bit Operating System type into its full text representation
// including the kernel revision (not the distribution version) on POSIX systems
// - returns e.g. 'Windows Vista', 'Windows 11 64-bit 22000' or 'Ubuntu 5.4.0'
function ToTextOS(osint32: integer): RawUtf8;

type
  /// the recognized ARM/AARCH64 CPU types
  // - https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  // - is defined on all platforms for cross-system use
  TArmCpuType = (
    actUnknown,
    actARM810,
    actARM920,
    actARM922,
    actARM926,
    actARM940,
    actARM946,
    actARM966,
    actARM1020,
    actARM1022,
    actARM1026,
    actARM11MPCore,
    actARM1136,
    actARM1156,
    actARM1176,
    actCortexA5,
    actCortexA7,
    actCortexA8,
    actCortexA9,
    actCortexA12,
    actCortexA15,
    actCortexA17,
    actCortexR4,
    actCortexR5,
    actCortexR7,
    actCortexR8,
    actCortexM0,
    actCortexM1,
    actCortexM3,
    actCortexM4,
    actCortexM7,
    actCortexM0P,
    actCortexA32,
    actCortexA53,
    actCortexA35,
    actCortexA55,
    actCortexA65,
    actCortexA57,
    actCortexA72,
    actCortexA73,
    actCortexA75,
    actCortexA76,
    actNeoverseN1,
    actCortexA77,
    actCortexA76AE,
    actCortexR52,
    actCortexM23,
    actCortexM33,
    actCortexA78,
    actCortexA78AE,
    actNeoverseE1,
    actCortexA78C);
  /// a set of recognized ARM/AARCH64 CPU types
  TArmCpuTypes = set of TArmCpuType;

  /// the recognized ARM/AARCH64 CPU hardware implementers
  // - https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  TArmCpuImplementer = (
    aciUnknown,
    aciARM,
    aciBroadcom,
    aciCavium,
    aciDEC,
    aciFUJITSU,
    aciHiSilicon,
    aciInfineon,
    aciMotorola,
    aciNVIDIA,
    aciAPM,
    aciQualcomm,
    aciSamsung,
    aciMarvell,
    aciApple,
    aciFaraday,
    aciIntel,
    aciAmpere);
  /// a set of recognized ARM/AARCH64 CPU hardware implementers
  TArmCpuImplementers = set of TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU from its 12-bit hardware ID
function ArmCpuType(id: word): TArmCpuType;

/// recognize a given ARM/AARCH64 CPU type name from its 12-bit hardware ID
function ArmCpuTypeName(act: TArmCpuType; id: word): RawUtf8;

/// recognize a given ARM/AARCH64 CPU implementer from its 8-bit hardware ID
function ArmCpuImplementer(id: byte): TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU implementer name from its 8-bit hardware ID
function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word): RawUtf8;


const
  /// contains the Delphi/FPC Compiler Version as text
  // - e.g. 'Delphi 10.3 Rio', 'Delphi 2010' or 'Free Pascal 3.3.1'
  COMPILER_VERSION: RawUtf8 =
  {$ifdef FPC}
    'Free Pascal'
    {$ifdef VER2_6_4} + ' 2.6.4'{$endif}
    {$ifdef VER3_0}   + ' 3.0'  {$ifdef VER3_0_4} + '.4' {$else}
    {$ifdef VER3_0_2} + '.2'    {$endif} {$endif} {$endif}
    {$ifdef VER3_1}   + ' 3.1'  {$ifdef VER3_1_1} + '.1' {$endif} {$endif}
    {$ifdef VER3_2}   + ' 3.2'  {$endif}
    {$ifdef VER3_3}   + ' 3.3'  {$ifdef VER3_3_1} + '.1' {$endif} {$endif}
    {$ifdef VER3_4}   + ' 3.4'  {$endif}
  {$else}
    'Delphi'
    {$ifdef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
      {$if     defined(VER140)} + ' 6'
      {$elseif defined(VER150)} + ' 7'
      {$elseif defined(VER160)} + ' 8'
      {$elseif defined(VER170)} + ' 2005'
      {$elseif defined(VER185)} + ' 2007'
      {$elseif defined(VER180)} + ' 2006'
      {$elseif defined(VER200)} + ' 2009'
      {$elseif defined(VER210)} + ' 2010'
      {$elseif defined(VER220)} + ' XE'
      {$elseif defined(VER230)} + ' XE2'
      {$elseif defined(VER240)} + ' XE3'
      {$elseif defined(VER250)} + ' XE4'
      {$elseif defined(VER260)} + ' XE5'
      {$elseif defined(VER265)} + ' AppMethod 1'
      {$elseif defined(VER270)} + ' XE6'
      {$elseif defined(VER280)} + ' XE7'
      {$elseif defined(VER290)} + ' XE8'
      {$elseif defined(VER300)} + ' 10 Seattle'
      {$elseif defined(VER310)} + ' 10.1 Berlin'
      {$elseif defined(VER320)} + ' 10.2 Tokyo'
      {$elseif defined(VER330)} + ' 10.3 Rio'
      {$elseif defined(VER340)} + ' 10.4 Sydney'
      {$elseif defined(VER350)} + ' 11 Alexandria'
      {$elseif defined(VER360)} + ' 11.1 Next'
      {$ifend}
    {$endif CONDITIONALEXPRESSIONS}
  {$endif FPC}
  {$ifdef CPU64} + ' 64 bit' {$else} + ' 32 bit' {$endif};

{$ifndef PUREMORMOT2}
/// deprecated function: use COMPILER_VERSION constant instead
function GetDelphiCompilerVersion: RawUtf8; deprecated;
{$endif PUREMORMOT2}

{$ifdef OSWINDOWS}

{$ifdef UNICODE}

const
  /// a global constant to be appended for Windows Ansi or wide API names
  _AW = 'W';

{$else}

const
  /// a global constant to be appended for Windows Ansi or wide API names
  _AW = 'A';

type
  /// low-level API structure, not defined in old Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

{$endif UNICODE}

var
  /// is set to TRUE if the current process is a 32-bit image running under WOW64
  // - WOW64 is the x86 emulator that allows 32-bit Windows-based applications
  // to run seamlessly on 64-bit Windows
  // - equals always FALSE if the current executable is a 64-bit image
  IsWow64: boolean;
  /// the current System information, as retrieved for the current process
  // - under a WOW64 process, it will use the GetNativeSystemInfo() new API
  // to retrieve the real top-most system information
  // - note that the lpMinimumApplicationAddress field is replaced by a
  // more optimistic/realistic value ($100000 instead of default $10000)
  // - under BSD/Linux, only contain dwPageSize and dwNumberOfProcessors fields
  SystemInfo: TSystemInfo;
  /// low-level Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;
  /// the current Windows edition, as retrieved for the current process
  OSVersion: TWindowsVersion;

{$else OSWINDOWS}

var
  /// emulate only some used fields of Windows' TSystemInfo
  SystemInfo: record
    // retrieved from libc's getpagesize() - is expected to not be 0
    dwPageSize: cardinal;
    // retrieved from HW_NCPU (BSD) or /proc/cpuinfo (Linux)
    dwNumberOfProcessors: cardinal;
    // meaningful system information, as returned by fpuname()
    uts: record
      sysname, release, version: RawUtf8;
    end;
    /// Linux Distribution release name, retrieved from /etc/*-release
    release: RawUtf8;
  end;
  
{$endif OSWINDOWS}

{$M+} // to have existing RTTI for published properties

type
  /// used to retrieve version information from any EXE
  // - under Linux, all version numbers are set to 0 by default, unless
  // you define the FPCUSEVERSIONINFO conditional and information is
  // extracted from executable resources
  // - you should not have to use this class directly, but via the
  // Executable global variable
  TFileVersion = class
  protected
    fDetailed: string;
    fFileName: TFileName;
    fBuildDateTime: TDateTime;
    fVersionInfo, fUserAgent: RawUtf8;
    /// change the version (not to be used in most cases)
    procedure SetVersion(aMajor, aMinor, aRelease, aBuild: integer);
  public
    /// executable major version number
    Major: integer;
    /// executable minor version number
    Minor: integer;
    /// executable release version number
    Release: integer;
    /// executable release build number
    Build: integer;
    /// build year of this exe file
    BuildYear: word;
    /// version info of the exe file as '3.1'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    Main: string;
    /// associated CompanyName string version resource
    CompanyName: RawUtf8;
    /// associated FileDescription string version resource
    FileDescription: RawUtf8;
    /// associated FileVersion string version resource
    FileVersion: RawUtf8;
    /// associated InternalName string version resource
    InternalName: RawUtf8;
    /// associated LegalCopyright string version resource
    LegalCopyright: RawUtf8;
    /// associated OriginalFileName string version resource
    OriginalFilename: RawUtf8;
    /// associated ProductName string version resource
    ProductName: RawUtf8;
    /// associated ProductVersion string version resource
    ProductVersion: RawUtf8;
    /// associated Comments string version resource
    Comments: RawUtf8;
    /// associated Language Translation string version resource
    LanguageInfo: RawUtf8;
    /// retrieve application version from exe file name
    // - DefaultVersion32 is used if no information Version was included into
    // the executable resources (on compilation time)
    // - you should not have to use this constructor, but rather access the
    // Executable global variable
    constructor Create(const aFileName: TFileName; aMajor: integer = 0;
      aMinor: integer = 0; aRelease: integer = 0; aBuild: integer = 0);
    /// retrieve the version as a 32-bit integer with Major.Minor.Release
    // - following Major shl 16+Minor shl 8+Release bit pattern
    function Version32: integer;
    /// build date and time of this exe file, as plain text
    function BuildDateTimeString: string;
    /// version info of the exe file as '3.1.0.123' or ''
    // - this method returns '' if Detailed is '0.0.0.0'
    function DetailedOrVoid: string;
    /// returns the version information of this exe file as text
    // - includes FileName (without path), Detailed and BuildDateTime properties
    // - e.g. 'myprogram.exe 3.1.0.123 (2016-06-14 19:07:55)'
    function VersionInfo: RawUtf8;
    /// returns a ready-to-use User-Agent header with exe name, version and OS
    // - e.g. 'myprogram/3.1.0.123W32' for myprogram running on Win32
    // - here OS_INITIAL[] character is used to identify the OS, with '32'
    // appended on Win32 only (e.g. 'myprogram/3.1.0.2W', is for Win64)
    function UserAgent: RawUtf8;
    /// returns the version information of a specified exe file as text
    // - includes FileName (without path), Detailed and BuildDateTime properties
    // - e.g. 'myprogram.exe 3.1.0.123 2016-06-14 19:07:55'
    class function GetVersionInfo(const aFileName: TFileName): RawUtf8;
  published
    /// version info of the exe file as '3.1.0.123'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - under Linux, always return '0.0.0.0' if no custom version number
    // has been defined
    // - consider using DetailedOrVoid method if '0.0.0.0' is not expected
    property Detailed: string
      read fDetailed write fDetailed;
    /// build date and time of this exe file
    property BuildDateTime: TDateTime
      read fBuildDateTime write fBuildDateTime;
  end;

{$M-}

/// quickly parse the TFileVersion.UserAgent content
// - identify e.g. 'myprogram/3.1.0.2W' or 'myprogram/3.1.0.2W32' text
function UserAgentParse(const UserAgent: RawUtf8;
  out ProgramName, ProgramVersion: RawUtf8;
  out OS: TOperatingSystem): boolean;

type
  /// stores some global information about the current executable and computer
  TExecutable = record
    /// the main executable name, without any path nor extension
    // - e.g. 'Test' for 'c:\pathto\Test.exe'
    ProgramName: RawUtf8;
    /// the main executable details, as used e.g. by TSynLog
    // - e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.123 (2011-03-29 11:09:06)'
    ProgramFullSpec: RawUtf8;
    /// the main executable file name (including full path)
    // - same as paramstr(0)
    ProgramFileName: TFileName;
    /// the main executable full path (excluding .exe file name)
    // - same as ExtractFilePath(paramstr(0))
    ProgramFilePath: TFileName;
    /// the full path of the running executable or library
    // - for an executable, same as paramstr(0)
    // - for a library, will contain the whole .dll file name
    InstanceFileName: TFileName;
    /// the current executable version
    Version: TFileVersion;
    /// the current computer host name
    Host: RawUtf8;
    /// the current computer user name
    User: RawUtf8;
    /// some hash representation of this information
    // - the very same executable on the very same computer run by the very
    // same user will always have the same Hash value
    // - is computed from the crc32c of this TExecutable fields: c0 from
    // Version32, CpuFeatures and Host, c1 from User, c2 from ProgramFullSpec
    // and c3 from InstanceFileName
    // - may be used as an entropy seed, or to identify a process execution
    Hash: THash128Rec;
  end;

var
  /// global information about the current executable and computer
  // - this structure is initialized in this unit's initialization block below
  // - you can call SetExecutableVersion() with a custom version, if needed
  Executable: TExecutable;

  {$ifndef PUREMORMOT2}
  /// deprecated global: use Executable variable instead
  ExeVersion: TExecutable absolute Executable;
  {$endif PUREMORMOT2}

/// initialize Executable global variable, supplying a custom version number
// - by default, the version numbers will be retrieved at startup from the
// executable itself (if it was included at build time)
// - but you can use this function to set any custom version numbers
procedure SetExecutableVersion(aMajor,aMinor,aRelease,aBuild: integer); overload;

/// initialize Executable global variable, supplying the version as text
// - e.g. SetExecutableVersion('7.1.2.512');
procedure SetExecutableVersion(const aVersionText: RawUtf8); overload;

/// return a function/method location according to the supplied code address
// - returns the address as hexadecimal by default, e.g. '004cb765'
// - if mormot.core.log.pas is defined in the project, will redirect to
// TDebugFile.FindLocationShort() method using .map/.dbg/.mab information, and
// return filename, symbol name and line number (if any) as plain text, e.g.
// '4cb765 ../src/core/mormot.core.base.pas statuscodeissuccess (11183)' on FPC
var
  GetExecutableLocation: function(aAddress: pointer): ShortString;


type
  /// identify an operating system folder
  TSystemPath = (
    spCommonData,
    spUserData,
    spCommonDocuments,
    spUserDocuments,
    spTempFolder,
    spLog);

/// returns an operating system folder
// - will return the full path of a given kind of private or shared folder,
// depending on the underlying operating system
// - will use SHGetFolderPath and the corresponding CSIDL constant under Windows
// - under POSIX, will return $TMP/$TMPDIR folder for spTempFolder, ~/.cache/appname
// for spUserData, /var/log for spLog, or the $HOME folder
// - returned folder name contains the trailing path delimiter (\ or /)
function GetSystemPath(kind: TSystemPath): TFileName;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{$ifdef OSWINDOWS}

type
  TThreadID = DWORD;
  TMessage = Messages.TMessage;
  HWND = Windows.HWND;
  LARGE_INTEGER = Windows.LARGE_INTEGER;
  BOOL = Windows.BOOL;

  /// the known Windows Registry Root key used by TWinRegistry.ReadOpen
  TWinRegistryRoot = (
    wrClasses,
    wrCurrentUser,
    wrLocalMachine,
    wrUsers);

  /// direct access to the Windows Registry
  // - could be used as alternative to TRegistry, which doesn't behave the same on
  // all Delphi versions, and is enhanced on FPC (e.g. which supports REG_MULTI_SZ)
  // - is also Unicode ready for text, using UTF-8 conversion on all compilers
  TWinRegistry = object
  public
    /// the opened HKEY handle
    key: HKEY;
    /// start low-level read access to a Windows Registry node
    // - on success (returned true), Close method should be eventually called
    function ReadOpen(root: TWinRegistryRoot; const keyname: RawUtf8;
      closefirst: boolean = false): boolean;
    /// finalize low-level read access to the Windows Registry after ReadOpen()
    procedure Close;
    /// low-level read a UTF-8 string from the Windows Registry after ReadOpen()
    // - in respect to Delphi's TRegistry, will properly handle REG_MULTI_SZ
    // (return the first value of the multi-list)
    // - we don't use string here since it would induce a dependency to
    // mormot.core.unicode
    function ReadString(const entry: SynUnicode; andtrim: boolean = true): RawUtf8;
    /// low-level read a Windows Registry content after ReadOpen()
    // - works with any kind of key, but was designed for REG_BINARY
    function ReadData(const entry: SynUnicode): RawByteString;
    /// low-level read a Windows Registry 32-bit REG_DWORD value after ReadOpen()
    function ReadDword(const entry: SynUnicode): cardinal;
    /// low-level read a Windows Registry 64-bit REG_QWORD value after ReadOpen()
    function ReadQword(const entry: SynUnicode): QWord;
    /// low-level read a Windows Registry content as binary buffer after ReadOpen()
    // - just a wrapper around RegQueryValueExW() API call
    function ReadBuffer(const entry: SynUnicode; Data: pointer; DataLen: DWORD): boolean;
    /// low-level enumeration of all sub-entries names of a Windows Registry key
    function ReadEnumEntries: TRawUtf8DynArray;
  end;

  /// TSynWindowsPrivileges enumeration synchronized with WinAPI
  // - see https://docs.microsoft.com/en-us/windows/desktop/secauthz/privilege-constants
  TWinSystemPrivilege = (
    wspCreateToken,
    wspAssignPrimaryToken,
    wspLockMemory,
    wspIncreaseQuota,
    wspUnsolicitedInput,
    wspMachineAccount,
    wspTCP,
    wspSecurity,
    wspTakeOwnership,
    wspLoadDriver,
    wspSystemProfile,
    wspSystemTime,
    wspProfSingleProcess,
    wspIncBasePriority,
    wspCreatePageFile,
    wspCreatePermanent,
    wspBackup,
    wspRestore,
    wspShutdown,
    wspDebug,
    wspAudit,
    wspSystemEnvironment,
    wspChangeNotify,
    wspRemoteShutdown,
    wspUndock,
    wspSyncAgent,
    wspEnableDelegation,
    wspManageVolume,
    wspImpersonate,
    wspCreateGlobal,
    wspTrustedCredmanAccess,
    wspRelabel,
    wspIncWorkingSet,
    wspTimeZone,
    wspCreateSymbolicLink);

  /// TSynWindowsPrivileges set synchronized with WinAPI
  TWinSystemPrivileges = set of TWinSystemPrivilege;

  /// TSynWindowsPrivileges enumeration synchronized with WinAPI
  // - define the execution context, i.e. if the token is used for the current
  // process or the current thread
  TPrivilegeTokenType = (
    pttProcess,
    pttThread);

  /// manage available privileges on Windows platform
  // - not all available privileges are active for all process
  // - for usage of more advanced WinAPI, explicit enabling of privilege is
  // sometimes needed
  TSynWindowsPrivileges = object
  private
    fAvailable: TWinSystemPrivileges;
    fEnabled: TWinSystemPrivileges;
    fDefEnabled: TWinSystemPrivileges;
    function SetPrivilege(
      aPrivilege: TWinSystemPrivilege; aEnablePrivilege: boolean): boolean;
    procedure LoadPrivileges;
  public
    /// handle to privileges token
    Token: THandle;
    /// initialize the object dedicated to management of available privileges
    // - aTokenPrivilege can be used for current process or current thread
    procedure Init(aTokenPrivilege: TPrivilegeTokenType = pttProcess);
    /// finalize the object and relese Token handle
    // - aRestoreInitiallyEnabled parameter can be used to restore initially
    // state of enabled privileges
    procedure Done(aRestoreInitiallyEnabled: boolean = true);
    /// enable privilege
    // - if aPrivilege is already enabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Enable(aPrivilege: TWinSystemPrivilege): boolean;
    /// disable privilege
    // - if aPrivilege is already disabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Disable(aPrivilege: TWinSystemPrivilege): boolean;
    /// set of available privileges for current process/thread
    property Available: TWinSystemPrivileges
      read fAvailable;
    /// set of enabled privileges for current process/thread
    property Enabled: TWinSystemPrivileges
      read fEnabled;
  end;

  /// which information was returned by GetProcessInfo() overloaded functions
  TWinProcessAvailableInfos = set of (
    wpaiPID,
    wpaiBasic,
    wpaiPEB,
    wpaiCommandLine,
    wpaiImagePath);

  /// information returned by GetProcessInfo() overloaded functions
  TWinProcessInfo = record
    AvailableInfo: TWinProcessAvailableInfos;
    PID: cardinal;
    ParentPID: cardinal;
    SessionID: cardinal;
    PEBBaseAddress: Pointer;
    AffinityMask: cardinal;
    BasePriority: integer;
    ExitStatus: integer;
    BeingDebugged: byte;
    ImagePath: SynUnicode;
    CommandLine: SynUnicode;
  end;

  PWinProcessInfo = ^TWinProcessInfo;
  TWinProcessInfoDynArray = array of TWinProcessInfo;


/// retrieve low-level process information, from the Windows API
procedure GetProcessInfo(aPid: cardinal;
  out aInfo: TWinProcessInfo); overload;

/// retrieve low-level process(es) information, from the Windows API
procedure GetProcessInfo(const aPidList: TCardinalDynArray;
  out aInfo: TWinProcessInfoDynArray); overload;


type
  HCRYPTPROV = pointer;
  HCRYPTKEY = pointer;
  HCRYPTHASH = pointer;

  PCCERT_CONTEXT = pointer;
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;
  PCCRL_CONTEXT = pointer;
  PPCCRL_CONTEXT = ^PCCRL_CONTEXT;
  PCRYPT_ATTRIBUTE = pointer;
  PCERT_INFO = pointer;
  HCERTSTORE = pointer;

  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PByte;
  end;
  CRYPT_OBJID_BLOB = CRYPTOAPI_BLOB;

  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: PAnsiChar;
    Parameters: CRYPT_OBJID_BLOB;
  end;

  CRYPT_SIGN_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    pSigningCert: PCCERT_CONTEXT;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
    cMsgCert: DWORD;
    rgpMsgCert: PPCCERT_CONTEXT;
    cMsgCrl: DWORD;
    rgpMsgCrl: PPCCRL_CONTEXT;
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashEncryptionAuxInfo: Pointer;
  end;

  PFN_CRYPT_GET_SIGNER_CERTIFICATE = function(pvGetArg: Pointer;
    dwCertEncodingType: DWORD; pSignerId: PCERT_INFO;
    hMsgCertStore: HCERTSTORE): PCCERT_CONTEXT; stdcall;
  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: Pointer;
  end;

  /// direct access to the Windows CryptoApi
  TWinCryptoApi = object
  private
    /// if the presence of this API has been tested
    Tested: boolean;
    /// if this API has been loaded
    Handle: THandle;
    /// used when inlining Available method
    procedure Resolve;
  public
    /// acquire a handle to a particular key container within a
    // particular cryptographic service provider (CSP)
    AcquireContextA: function(var phProv: HCRYPTPROV; pszContainer: PAnsiChar;
      pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
    /// releases the handle of a cryptographic service provider (CSP) and a
    // key container
    ReleaseContext: function(hProv: HCRYPTPROV; dwFlags: PtrUInt): BOOL; stdcall;
    /// transfers a cryptographic key from a key BLOB into a cryptographic
    // service provider (CSP)
    ImportKey: function(hProv: HCRYPTPROV; pbData: pointer; dwDataLen: DWORD;
      hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
    /// customizes various aspects of a session key's operations
    SetKeyParam: function(hKey: HCRYPTKEY; dwParam: DWORD; pbData: pointer;
      dwFlags: DWORD): BOOL; stdcall;
    /// releases the handle referenced by the hKey parameter
    DestroyKey: function(hKey: HCRYPTKEY): BOOL; stdcall;
    /// encrypt the data designated by the key held by the CSP module
    // referenced by the hKey parameter
    Encrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
    /// decrypts data previously encrypted by using the CryptEncrypt function
    Decrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD): BOOL; stdcall;
    /// fills a buffer with cryptographically random bytes
    // - since Windows Vista with Service Pack 1 (SP1), an AES counter-mode
    // based PRNG specified in NIST Special Publication 800-90 is used
    GenRandom: function(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall;
    /// sign a message (not resolved yet - in crypt32.dll)
    SignMessage: function(var pSignPara: CRYPT_SIGN_MESSAGE_PARA;
      fDetachedSignature: BOOL; cToBeSigned: DWORD; rgpbToBeSigned: pointer;
      var rgcbToBeSigned: DWORD; pbSignedBlob: pointer; var pcbSignedBlob: DWORD): BOOL; stdcall;
    /// verify a signed message (not resolved yet - in crypt32.dll)
    VerifyMessageSignature: function(var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
      dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD;
      pbDecoded: PByte; pcbDecoded: LPDWORD; ppSignerCert: PPCCERT_CONTEXT): BOOL; stdcall;
    /// try to load the CryptoApi on this system
    function Available: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

const
  NO_ERROR = Windows.NO_ERROR;
  ERROR_ACCESS_DENIED = Windows.ERROR_ACCESS_DENIED;
  ERROR_INVALID_PARAMETER = Windows.ERROR_INVALID_PARAMETER;
  INVALID_HANDLE_VALUE = Windows.INVALID_HANDLE_VALUE;
  ENGLISH_LANGID = $0409;

  PROV_RSA_AES = 24;
  CRYPT_NEWKEYSET = 8;
  PLAINTEXTKEYBLOB = 8;
  CUR_BLOB_VERSION = 2;
  KP_IV = 1;
  KP_MODE = 4;
  CALG_AES_128 = $660E;
  CALG_AES_192 = $660F;
  CALG_AES_256 = $6610;
  CRYPT_MODE_CBC = 1;
  CRYPT_MODE_ECB = 2;
  CRYPT_MODE_OFB = 3;
  CRYPT_MODE_CFB = 4;
  CRYPT_MODE_CTS = 5;
  HCRYPTPROV_NOTTESTED = HCRYPTPROV(-1);
  NTE_BAD_KEYSET = HRESULT($80090016);
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT = DWORD($F0000000);

var
  CryptoApi: TWinCryptoApi;

/// protect some data for the current user, using Windows DPAPI
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - will use CryptProtectData DPAPI function call under Windows
// - see https://msdn.microsoft.com/en-us/library/ms995355
// - this function is Windows-only, could be slow, and you don't know which
// algorithm is really used on your system, so using our mormot.crypt.core.pas
// CryptDataForCurrentUser() is probably a safer (and cross-platform) alternative
// - also note that DPAPI has been closely reverse engineered - see e.g.
// https://www.passcape.com/index.php?section=docsys&cmd=details&id=28
function CryptDataForCurrentUserDPAPI(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;

/// this global procedure should be called from each thread needing to use OLE
// - it is called e.g. by TOleDBConnection.Create when an OleDb connection
// is instantiated for a new thread
// - every call of CoInit shall be followed by a call to CoUninit
// - implementation will maintain some global counting, to call the CoInitialize
// API only once per thread
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoInit;

/// this global procedure should be called at thread termination
// - it is called e.g. by TOleDBConnection.Destroy, when thread associated
// to an OleDb connection is terminated
// - every call of CoInit shall be followed by a call to CoUninit
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoUninit;

/// retrieves the current executable module handle, i.e.  its memory load address
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetModuleHandle(lpModuleName: PChar): HMODULE;

/// post a message to the Windows message queue
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function PostMessage(hWnd: HWND; Msg:UINT; wParam: WPARAM; lParam: LPARAM): BOOL;

/// retrieves the current stack trace
// - only available since Windows XP
// - FramesToSkip + FramesToCapture should be <= 62
function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: cardinal;
  BackTrace, BackTraceHash: pointer): byte; stdcall;

/// compatibility function, wrapping Win32 API available since XP
function IsDebuggerPresent: BOOL; stdcall;

/// retrieves the current thread ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentThreadId: DWORD; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentProcessId: DWORD; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function WaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function GetEnvironmentStringsW: PWideChar; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function FreeEnvironmentStringsW(EnvBlock: PWideChar): BOOL; stdcall;

/// expand any embedded environment variables, i.e %windir%
function ExpandEnvVars(const aStr: string): string;

/// try to enter a Critical Section (Lock)
// - returns 1 if the lock was acquired, or 0 if the mutex is already locked
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer; stdcall;

/// enter a Critical Section (Lock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure EnterCriticalSection(var cs: TRTLCriticalSection); stdcall;

/// leave a Critical Section (UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure LeaveCriticalSection(var cs: TRTLCriticalSection); stdcall;

/// initialize IOCP instance
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey: pointer; NumberOfConcurrentThreads: DWORD): THandle; stdcall;

/// retrieve IOCP instance status
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: PtrUInt;
  var lpOverlapped: pointer; dwMilliseconds: DWORD): BOOL; stdcall;

/// trigger a IOCP instance
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function PostQueuedCompletionStatus(CompletionPort: THandle;
  NumberOfBytesTransferred: DWORD; dwCompletionKey: pointer;
  lpOverlapped: POverlapped): BOOL; stdcall;

/// finalize a Windows resource (e.g. IOCP instance)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function CloseHandle(hObject: THandle): BOOL; stdcall;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function FileCreate(const aFileName: TFileName): THandle;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
procedure FileClose(F: THandle); stdcall;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function DeleteFile(const aFileName: TFileName): boolean;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function RenameFile(const OldName, NewName: TFileName): boolean;

{$else}

/// returns how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit, false for the current process
// - returns -1 if the getrlimit() API call failed
function GetFileOpenLimit(hard: boolean = false): integer;

/// changes how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit (requires root priviledges),
// false for the current process
// - returns the new value set (may not match the expected max value on error)
// - returns -1 if the getrlimit().setrlimit() API calls failed
// - for instance, to set the limit of the current process to its highest value:
// ! SetFileOpenLimit(GetFileOpenLimit(true));
function SetFileOpenLimit(max: integer; hard: boolean = false): integer;

type
  /// Low-level access to the ICU library installed on this system
  // - "International Components for Unicode" (ICU) is an open-source set of
  // libraries for Unicode support, internationalization and globalization
  // - used by Unicode_CompareString, Unicode_AnsiToWide, Unicode_WideToAnsi,
  // Unicode_InPlaceUpper and Unicode_InPlaceLower function from this unit
  TIcuLibrary = packed object
  protected
    icu, icudata, icui18n: pointer;
    Loaded: boolean;
    procedure DoLoad(const LibName: TFileName = ''; const Version: string = '');
    procedure Done;
  public
    /// Initialize an ICU text converter for a given encoding
    ucnv_open: function (converterName: PAnsiChar; var err: SizeInt): pointer; cdecl;
    /// finalize the ICU text converter for a given encoding
    ucnv_close: procedure (converter: pointer); cdecl;
    /// customize the ICU text converter substitute char
    ucnv_setSubstChars: procedure (converter: pointer;
      subChars: PAnsiChar; len: byte; var err: SizeInt); cdecl;
    /// enable the ICU text converter fallback
    ucnv_setFallback: procedure (cnv: pointer; usesFallback: LongBool); cdecl;
    /// ICU text conversion from UTF-16 to a given encoding
    ucnv_fromUChars: function (cnv: pointer; dest: PAnsiChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU text conversion from a given encoding to UTF-16
    ucnv_toUChars: function (cnv: pointer; dest: PWideChar; destCapacity: cardinal;
      src: PAnsiChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to uppercase
    u_strToUpper: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to lowercase
    u_strToLower: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text comparison
    u_strCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; codePointOrder: LongBool): cardinal; cdecl;
    /// ICU UTF-16 text comparison with options, e.g. for case-insensitivity
    u_strCaseCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; options: cardinal;
      var err: SizeInt): cardinal; cdecl;
    /// get the ICU data folder
    u_getDataDirectory: function: PAnsiChar; cdecl;
    /// set the ICU data folder
    u_setDataDirectory: procedure(directory: PAnsiChar); cdecl;
    /// initialize the ICU library
    u_init: procedure(var status: SizeInt); cdecl;
    /// try to initialize a specific version of the ICU library
    // - first finalize any existing loaded instance
    // - returns true if was successfully loaded and setup
    function ForceLoad(const LibName: TFileName; const Version: string): boolean;
    /// returns TRUE if a ICU library is available on this system
    // - will thread-safely load and initialize it if necessary
    function IsAvailable: boolean; inline;
    /// Initialize an ICU text converter for a given codepage
    // - returns nil if ICU is not available on this system
    // - wrapper around ucnv_open/ucnv_setSubstChars/ucnv_setFallback calls
    // - caller should make ucnv_close() once done with the returned instance
    function ucnv(codepage: cardinal): pointer;
  end;

var
  /// low-level late-binding access to any installed ICU library
  // - typical use is to check icu.IsAvailable then the proper icu.*() functions
  // - this unit will make icu.Done in its finalization section
  icu: TIcuLibrary;


{$ifdef OSLINUX} { the systemd API is Linux-specific }

const
  /// The first passed file descriptor is fd 3
  SD_LISTEN_FDS_START = 3;

  /// low-level libcurl library file name, depending on the running OS
  LIBSYSTEMD_PATH = 'libsystemd.so.0';

  ENV_INVOCATION_ID: PAnsiChar = 'INVOCATION_ID';

type
  /// low-level systemd parameter to sd.journal_sendv() function
  TIoVec = record
    iov_base: pointer;
    iov_len: PtrUInt;
  end;

  /// implements late-binding of the systemd library
  // - about systemd: see https://www.freedesktop.org/wiki/Software/systemd
  // and http://0pointer.de/blog/projects/socket-activation.html - to get headers
  // on debian: `sudo apt install libsystemd-dev && cd /usr/include/systemd`
  TSystemD = packed object
  private
    systemd: pointer;
    tested: boolean;
    procedure DoLoad;
  public
    /// returns how many file descriptors have been passed to process
    // - if result=1 then socket for accepting connection is LISTEN_FDS_START
    listen_fds: function(unset_environment: integer): integer; cdecl;
    /// returns 1 if the file descriptor is an AF_UNIX socket of the specified type and path
    is_socket_unix: function(fd, typr, listening: integer;
      var path: TFileName; pathLength: PtrUInt): integer; cdecl;
    /// systemd: submit simple, plain text log entries to the system journal
    // - priority value can be obtained using integer(LOG_TO_SYSLOG[logLevel])
    journal_print: function(priority: integer; args: array of const): integer; cdecl;
    /// systemd: submit array of iov structures instead of the format string to the system journal.
    //  - each structure should reference one field of the entry to submit.
    //  - the second argument specifies the number of structures in the array.
    journal_sendv: function(var iov: TIoVec; n: integer): integer; cdecl;
    /// sends notification to systemd
    // - see https://www.freedesktop.org/software/systemd/man/notify.html
    // status notification sample: sd.notify(0, 'READY=1');
    // watchdog notification: sd.notify(0, 'WATCHDOG=1');
    notify: function(unset_environment: integer; state: PUtf8Char): integer; cdecl;
    /// check whether the service manager expects watchdog keep-alive
    // notifications from a service
    // - if result > 0 then usec contains the notification interval (app should
    // notify every usec/2)
    watchdog_enabled: function(unset_environment: integer; usec: Puint64): integer; cdecl;
    /// returns true in case the current process was started by systemd
    // - For systemd v232+
    function ProcessIsStartedBySystemd: boolean;
    /// returns TRUE if a systemd library is available
    // - will thread-safely load and initialize it if necessary
    function IsAvailable: boolean; inline;
    /// release the systemd library
    procedure Done;
  end;

var
  /// low-level late-binding of the systemd library
  // - typical use is to check sd.IsAvailable then the proper sd.*() functions
  // - this unit will make sd.Done in its finalization section
  sd: TSystemD;

{$endif OSLINUX}

{$endif OSWINDOWS}


{ ****************** Unicode, Time, File, Console, Library process }

{$ifdef OSWINDOWS}

type
  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  // - warning: do not use this type directly, but rather TSynSystemTime as
  // defined in mormot.core.datetime which is really cross-platform, and has
  // consistent field order (FPC POSIX/Windows fields do not match!)
  TSystemTime = Windows.TSystemTime;

{$ifdef ISDELPHI}

  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  TRTLCriticalSection = Windows.TRTLCriticalSection;

  /// defined as in FPC RTL, to avoid dependency to Windows.pas unit
  TLibHandle = THandle;

{$endif ISDELPHI}

/// returns the current UTC time as TSystemTime
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - you should call directly FPC's version otherwise
// - warning: do not call this function directly, but use TSynSystemTime as
// defined in mormot.core.datetime which is really cross-platform
procedure GetLocalTime(out result: TSystemTime); stdcall;

/// a wrapper around FileTimeToLocalFileTime/FileTimeToSystemTime Windows APIs
// - only used by mormot.lib.static for proper SQlite3 linking on Windows
procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);

/// convert an Unix seconds time to a Win32 64-bit FILETIME value
procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);

/// convert a Win32 64-bit FILETIME value into an Unix seconds time
function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
  {$ifdef FPC} inline; {$endif}

/// convert a Win32 64-bit FILETIME value into an Unix milliseconds time
function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
  {$ifdef FPC} inline; {$endif}

{$else}

{$ifdef OSLINUX}
  {$define OSPTHREADS} // direct pthread calls were tested on Linux only
{$endif OSLINUX}

{$endif OSWINDOWS}

/// raw cross-platform library loading function
// - alternative to LoadLibrary() Windows API and FPC RTL
// - consider inheriting TSynLibrary if you want to map a set of API functions
function LibraryOpen(const LibraryName: TFileName): TLibHandle;

/// raw cross-platform library unloading function
// - alternative to FreeLibrary() Windows API and FPC RTL
procedure LibraryClose(Lib: TLibHandle);

/// raw cross-platform library resolution function, as defined in FPC RTL
// - alternative to GetProcAddr() Windows API and FPC RTL
function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
  {$ifdef OSWINDOWS} stdcall; {$endif}


const
  /// redefined here to avoid dependency to the Windows or SyncObjs units
  INFINITE = cardinal(-1);

/// initialize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// finalize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

{$ifdef OSPOSIX}

/// returns the unique ID of the current running thread
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var GetCurrentThreadId: function: TThreadID;

/// enter a Critical Section (Lock)
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var EnterCriticalSection: procedure(var cs: TRTLCriticalSection);

/// leave a Critical Section (UnLock)
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var LeaveCriticalSection: procedure(var cs: TRTLCriticalSection);

/// try to acquire and lock a Critical Section (Lock)
// - returns 1 if the lock was acquired, or 0 if the mutex is already locked
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var TryEnterCriticalSection: function(var cs: TRTLCriticalSection): integer;

{$endif OSPOSIX}

/// returns TRUE if the supplied mutex has been initialized
// - will check if the supplied mutex is void (i.e. all filled with 0 bytes)
function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// on need initialization of a mutex, then enter the lock
// - if the supplied mutex has been initialized, do nothing
// - if the supplied mutex is void (i.e. all filled with 0), initialize it
procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
  {$ifdef HASINLINEWINAPI}inline;{$endif}

/// on need finalization of a mutex
// - if the supplied mutex has been initialized, delete it
// - if the supplied mutex is void (i.e. all filled with 0), do nothing
procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);

/// returns the current UTC time as TSystemTime
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// - under Windows, directly call the homonymous Win32 API
// - warning: do not call this function directly, but use TSynSystemTime as
// defined in mormot.core.datetime which is really cross-platform
procedure GetSystemTime(out result: TSystemTime);
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API file flush to disk
procedure FlushFileBuffers(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code
function GetLastError: integer;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code
procedure SetLastError(error: integer);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns a given error code as plain text
// - calls FormatMessageW on Windows, or StrError() on POSIX
function GetErrorText(error: integer): RawUtf8;

/// retrieve the text corresponding to an error message for a given Windows module
// - use RTL SysErrorMessage() as fallback
function SysErrorMessagePerModule(Code: cardinal; ModuleName: PChar): string;

{$ifdef OSWINDOWS}
/// override the RTL function to force the ENGLISH_LANGID flag
function SysErrorMessage(Code: cardinal; ModuleName: PChar = nil): string;
{$endif OSWINDOWS}

/// raise an Exception from the last system error
procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);

/// compatibility function, wrapping Win32 API function
// - returns the current main Window handle on Windows, or 0 on POSIX/Linux
function GetDesktopWindow: PtrInt;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns the curent system code page for AnsiString types
// - as used to initialize CurrentAnsiConvert in mormot.core.unicode unit
// - initialized at startup: contains GetACP() Win32 API value on Delphi,
// or DefaultSystemCodePage on FPC - i.e. GetSystemCodePage() on POSIX (likely
// to be UTF-8) or the value used by the LCL for its "string" types
function Unicode_CodePage: integer;
  {$ifdef FPC} inline; {$endif}

/// compatibility function, wrapping CompareStringW() Win32 API text comparison
// - returns 1 if PW1>PW2, 2 if PW1=PW2, 3 if PW1<PW2 - so substract 2 to have
// -1,0,1 as regular StrCompW/StrICompW comparison function result
// - will compute StrLen(PW1/PW2) if L1 or L2 < 0
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - in practice, is seldom called, unless our proprietary WIN32CASE collation
// is used in mormot.db.raw.sqlite3
// - consider Utf8ILCompReference() from mormot.core.unicode.pas for an
// operating-system-independent Unicode 10.0 comparison function
function Unicode_CompareString(
  PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: boolean): integer;

/// compatibility function, wrapping MultiByteToWideChar() Win32 API call
// - returns the number of WideChar written into W^ destination buffer
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - raw function called by TSynAnsiConvert.AnsiBufferToUnicode from
// mormot.core.unicode unit
function Unicode_AnsiToWide(
  A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;

/// compatibility function, wrapping WideCharToMultiByte() Win32 API call
// - returns the number of AnsiChar written into A^ destination buffer
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - raw function called by TSynAnsiConvert.UnicodeBufferToAnsi from
// mormot.core.unicode unit
function Unicode_WideToAnsi(
  W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;

/// conversion of some UTF-16 buffer into a temporary Ansi ShortString
// - used when mormot.core.unicode is an overkill, e.g. TCrtSocket.SockSend()
procedure Unicode_WideToShort(
  W: PWideChar; LW, CodePage: PtrInt; var res: ShortString);

/// compatibility function, wrapping Win32 API CharUpperBuffW()
// - on POSIX, use the ICU library, or fallback to 'a'..'z' conversion only
// - raw function called by UpperCaseUnicode() from mormot.core.unicode unit
function Unicode_InPlaceUpper(W: PWideChar; WLen: integer): integer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API CharLowerBuffW()
// - on POSIX, use the ICU library, or fallback to 'A'..'Z' conversion only
// - raw function called by LowerCaseUnicode() from mormot.core.unicode unit
function Unicode_InPlaceLower(W: PWideChar; WLen: integer): integer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// returns a system-wide current monotonic timestamp as milliseconds
// - will use the corresponding native API function under Vista+, or will be
// redirected to a custom wrapper function for older Windows versions (XP)
// to avoid the 32-bit overflow/wrapping issue of GetTickCount
// - warning: FPC's SysUtils.GetTickCount64 or TThread.GetTickCount64 don't
// handle properly 49 days wrapping under XP -> always use this safe version
// - warning: FPC's SysUtils.GetTickCount64 may call fpgettimeofday() e.g.
// on Darwin, which is not monotonic -> always use this more coherent version
// - on POSIX, will call (via vDSO) the very fast CLOCK_MONOTONIC_COARSE if
// available, or the low-level mach_absolute_time() monotonic Darwin API
// - do not expect exact millisecond resolution - it may rather be within the
// 10-16 ms range, especially under Windows
{$ifdef OSWINDOWS}
var
  GetTickCount64: function: Int64; stdcall;
{$else}
function GetTickCount64: Int64;
{$endif OSWINDOWS}

/// returns the current UTC time
// - will convert from clock_gettime(CLOCK_REALTIME_COARSE) if available
function NowUtc: TDateTime;

/// returns the current UTC date/time as a second-based c-encoded time
// - i.e. current number of seconds elapsed since Unix epoch 1/1/1970
// - faster than NowUtc or GetTickCount64, on Windows or Unix platforms
// (will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime under Windows)
// - returns a 64-bit unsigned value, so is "Year2038bug" free
function UnixTimeUtc: TUnixTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - i.e. current number of milliseconds elapsed since Unix epoch 1/1/1970
// - faster and more accurate than NowUtc or GetTickCount64, on Windows or Unix
// - will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime/GetSystemTimePreciseAsFileTime under Windows - the
// later being more accurate, but slightly slower than the former, so you may
// consider using UnixMSTimeUtcFast on Windows if its 10-16ms accuracy is enough
function UnixMSTimeUtc: TUnixMSTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - under Linux/POSIX, is the very same than UnixMSTimeUtc (inlined call)
// - under Windows 8+, will call GetSystemTimeAsFileTime instead of
// GetSystemTimePreciseAsFileTime, which has higher precision, but is slower
// - prefer it under Windows, if a dozen of ms resolution is enough for your task
function UnixMSTimeUtcFast: TUnixMSTime;
  {$ifdef OSPOSIX} inline; {$endif}

/// the number of minutes bias in respect to UTC/GMT date/time
// - as retrieved via -GetLocalTimeOffset() at startup
var
  TimeZoneLocalBias: integer;

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// calling context when intercepting exceptions
  // - used e.g. for TSynLogExceptionToStr or RawExceptionIntercept() handlers
  TSynLogExceptionContext = object
    /// the raised exception class
    EClass: ExceptClass;
    /// the Delphi Exception instance
    // - may be nil for external/OS exceptions
    EInstance: Exception;
    /// the OS-level exception code
    // - could be $0EEDFAE0 of $0EEDFADE for Delphi-generated exceptions
    ECode: DWord;
    /// the address where the exception occured
    EAddr: PtrUInt;
    /// the optional stack trace
    EStack: PPtrUInt;
    /// = FPC's RaiseProc() FrameCount if EStack is Frame: PCodePointer
    EStackCount: integer;
    /// timestamp of this exception, as number of seconds since UNIX Epoch
    // - UnixTimeUtc is faster than NowUtc or GetSystemTime
    // - use UnixTimeToDateTime() to convert it into a regular TDateTime
    ETimestamp: TUnixTime;
    /// the logging level corresponding to this exception
    // - may be either sllException or sllExceptionOS
    ELevel: TSynLogInfo;
    /// retrieve some extended information about a given Exception
    // - on Windows, recognize most DotNet CLR Exception Names
    function AdditionalInfo(out ExceptionNames: TPUtf8CharDynArray): cardinal;
  end;

  /// the global function signature expected by RawExceptionIntercept()
  // - assigned e.g. to SynLogException() in mormot.core.log.pas
  TOnRawLogException = procedure(const Ctxt: TSynLogExceptionContext);

/// setup Exception interception for the whole process
// - call RawExceptionIntercept(nil) to disable custom exception handling
procedure RawExceptionIntercept(const Handler: TOnRawLogException);

{$endif NOEXCEPTIONINTERCEPT}

/// returns a high-resolution system-wide monotonic timestamp as microseconds
// - under Linux/POSIX, has true microseconds resolution, calling e.g.
// CLOCK_MONOTONIC on Linux/BSD
// - under Windows, calls QueryPerformanceCounter / QueryPerformanceFrequency
procedure QueryPerformanceMicroSeconds(out Value: Int64);

/// cross-platform check if the supplied THandle is not invalid
function ValidHandle(Handle: THandle): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check for unsafe '..' '/xxx' 'c:xxx' or '\\' patterns in a filename
function SafeFileName(const FileName: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' or '\\' patterns in a filename
function SafeFileNameU(const FileName: RawUtf8): boolean;

/// get a file date and time, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// get the date and time of one file into a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileAgeToWindowsTime(const FileName: TFileName): integer;

/// copy the date of one file to another
function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;

/// copy the date of one file from a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;

/// convert a Windows File 32-bit TimeStamp into a regular TDateTime
// - returns 0 if the conversion failed
// - used e.g. by FileSetDateFromWindowsTime() on POSIX
function WindowsFileTimeToDateTime(WinTime: integer): TDateTime;

/// convert a Windows File 64-bit TimeStamp into a regular TDateTime
// - i.e. a FILETIME value as returned by GetFileTime() Win32 API
// - returns 0 if the conversion failed
// - some binary formats (e.g. ISO 9660) has such FILETIME fields
function WindowsFileTime64ToDateTime(WinTime: QWord): TDateTime;

/// low-level conversion of a TDateTime into a Windows File 32-bit TimeStamp
// - returns 0 if the conversion failed
function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;

/// reduce the visibility of a given file by setting its read/write attributes
// - on POSIX, change attributes for the the owner, and reset group/world flags
// - if Secret=false, will have normal file attributes, with read/write access
// - if Secret=true, will have read-only attributes (and hidden on Windows -
// under POSIX, there is no "hidden" file attribute, but you should define a
// FileName starting by '.')
procedure FileSetAttributes(const FileName: TFileName; Secret: boolean);

/// get a file size, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
// - on POSIX, will use efficient fpStat() single call but not FileOpen/FileClose
function FileSize(const FileName: TFileName): Int64; overload;

/// get a file size, from its handle
// - returns 0 if file doesn't exist
// - on POSIX, will use efficient FpFStat() single call and not file seek
function FileSize(F: THandle): Int64; overload;

/// FileSeek() overloaded function, working with huge files
// - Delphi FileSeek() is buggy -> use this function to safely access files
// bigger than 2 GB (thanks to sanyin for the report)
function FileSeek64(Handle: THandle; const Offset: Int64;
  Origin: cardinal): Int64;

/// get low-level file information, in a cross-platform way
// - returns true on success
// - here file write/creation time are given as TUnixMSTime values, for better
// cross-platform process - note that FileCreateDateTime may not be supported
// by most Linux file systems, so the oldest timestamp available is returned
// as failover on such systems (probably the latest file metadata writing)
function FileInfoByHandle(aFileHandle: THandle; out FileId, FileSize,
  LastWriteAccess, FileCreateDateTime: Int64): boolean;

/// copy one file to another, similar to the Windows API
function CopyFile(const Source, Target: TFileName;
  FailIfExists: boolean): boolean;

/// conversion of Windows OEM CP-437 charset into a UTF-16 encoded string
function OemToUnicode(const oem: RawByteString): SynUnicode;

/// conversion of Windows OEM CP-437 charset into a file name
// - as used e.g. by mormot.core.zip for non UTF-8 file names
function OemToFileName(const oem: RawByteString): TFileName;

/// prompt the user for an error message to notify an unexpected issue
// - in practice, text encoding is expected to be plain 7-bit ASCII
// - on Windows, will use Writeln() on a (newly allocated if needed) console
// - on POSIX, will use Writeln(StdErr)
procedure DisplayFatalError(const title, msg: RawUtf8);

/// prompt the user for an error message to notify an unexpected issue
// - redirect to DisplayFatalError without any title
// - expects the regular Format() layout with %s %d - not the FormatUtf8() %
procedure DisplayError(const fmt: string; const args: array of const);

const
  /// operating-system dependent Line Feed characters
  {$ifdef OSWINDOWS}
  CRLF = #13#10;
  {$else}
  CRLF = #10;
  {$endif OSWINDOWS}

  /// operating-system dependent wildchar to match all files in a folder
  {$ifdef OSWINDOWS}
  FILES_ALL = '*.*';
  {$else}
  FILES_ALL = '*';
  {$endif OSWINDOWS}

/// get a file date and time, from a FindFirst/FindNext search
// - the returned timestamp is in local time, not UTC
// - this method would use the F.Timestamp field available since Delphi XE2
function SearchRecToDateTime(const F: TSearchRec): TDateTime;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a FindFirst/FindNext found instance is actually a file
function SearchRecValidFile(const F: TSearchRec): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a FindFirst/FindNext found instance is actually a folder
function SearchRecValidFolder(const F: TSearchRec): boolean;
  {$ifdef HASINLINE}inline;{$endif}

type
  // FPC TFileStream miss a Create(aHandle) constructor like Delphi
  TFileStreamFromHandle = class(THandleStream)
  protected
    fDontReleaseHandle: boolean;
  public
    /// explictely close the handle if needed
    destructor Destroy; override;
    /// Destroy calls FileClose(Handle) unless this property is true
    property DontReleaseHandle: boolean
      read fDontReleaseHandle write fDontReleaseHandle;
  end;

/// overloaded function optimized for one pass file reading
// - will use e.g. the FILE_FLAG_SEQUENTIAL_SCAN flag under Windows, as stated
// by http://blogs.msdn.com/b/oldnewthing/archive/2012/01/20/10258690.aspx
// - note: under XP, we observed ERROR_NO_SYSTEM_RESOURCES problems when calling
// FileRead() for chunks bigger than 32MB on files opened with this flag,
// so it would use regular FileOpen() on this deprecated OS
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by StringFromFile() or HashFile() functions
function FileOpenSequentialRead(const FileName: string): integer;

/// returns a TFileStream optimized for one pass file reading
// - will use FileOpenSequentialRead(), i.e. FILE_FLAG_SEQUENTIAL_SCAN on Windows
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by TRestOrmServerFullMemory and TAlgoCompress
function FileStreamSequentialRead(const FileName: string): THandleStream;

/// read a File content into a string
// - content can be binary or text
// - returns '' if file was not found or any read error occured
// - wil use GetFileSize() API by default, unless HasNoSize is defined,
// and read will be done using a buffer (required e.g. for char files under Linux)
// - uses RawByteString for byte storage, whatever the codepage is
function StringFromFile(const FileName: TFileName;
  HasNoSize: boolean = false): RawByteString;

/// create a File from a string content
// - uses RawByteString for byte storage, whatever the codepage is
function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean = false; FileDate: TDateTime = 0): boolean;

/// compute an unique temporary file name
// - following 'exename_123.tmp' pattern, in the system temporary folder
function TemporaryFileName: TFileName;

/// compute the file name, including its path if supplied, but without its extension
// - e.g. GetFileNameWithoutExt('/var/toto.ext') = '/var/toto'
// - may optionally return the extracted extension, as '.ext'
function GetFileNameWithoutExt(const FileName: TFileName;
  Extension: PFileName = nil): TFileName;

/// compare two "array of TFileName" elements, as file names
// - i.e. with no case sensitivity on Windows, and grouped by file extension
// - the expected string type is the generic RTL string, i.e. TFileName
// - calls internally GetFileNameWithoutExt() and AnsiCompareFileName()
function SortDynArrayFileName(const A, B): integer;

{$ifdef ISDELPHI20062007}
/// compatibility function defined to avoid hints on buggy Delphi 2006/2007
function AnsiCompareFileName(const S1, S2 : TFileName): integer;
{$endif ISDELPHI20062007}

/// creates a directory if not already existing
// - returns the full expanded directory name, including trailing path delimiter
// - returns '' on error, unless RaiseExceptionOnCreationFailure is true
function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: boolean = false): TFileName;

/// delete the content of a specified directory
// - only one level of file is deleted within the folder: no recursive deletion
// is processed by this function (for safety)
// - if DeleteOnlyFilesNotDirectory is TRUE, it won't remove the folder itself,
// but just the files found in it
function DirectoryDelete(const Directory: TFileName;
  const Mask: TFileName = FILES_ALL; DeleteOnlyFilesNotDirectory: boolean = false;
  DeletedCount: PInteger = nil): boolean;

/// delete the files older than a given age in a specified directory
// - for instance, to delete all files older than one day:
// ! DirectoryDeleteOlderFiles(FolderName, 1);
// - only one level of file is deleted within the folder: no recursive deletion
// is processed by this function, unless Recursive is TRUE
// - if Recursive=true, caller should set TotalSize^=0 to have an accurate value
function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName = FILES_ALL;
  Recursive: boolean = false; TotalSize: PInt64 = nil): boolean;

/// check if the directory is writable for the current user
// - try to write a small file with a random name
function IsDirectoryWritable(const Directory: TFileName): boolean;

type
  /// text file layout, as recognized by TMemoryMap.TextFileKind
  TTextFileKind = (
    isUnicode,
    isUtf8,
    isAnsi);

  /// cross-platform memory mapping of a file content
  TMemoryMap = object
  protected
    fBuf: PAnsiChar;
    fBufSize: PtrUInt;
    fFile: THandle;
    {$ifdef OSWINDOWS}
    fMap: THandle;
    {$endif OSWINDOWS}
    fFileSize: Int64;
    fFileLocal, fLoadedNotMapped: boolean;
    function DoMap(aCustomOffset: Int64): boolean;
    procedure DoUnMap;
  public
    /// map the corresponding file handle
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    function Map(aFile: THandle; aCustomSize: PtrUInt = 0;
      aCustomOffset: Int64 = 0; aFileOwned: boolean = false): boolean; overload;
    /// map the file specified by its name
    // - file will be closed when UnMap will be called
    function Map(const aFileName: TFileName): boolean; overload;
    /// set a fixed buffer for the content
    // - emulated a memory-mapping from an existing buffer
    procedure Map(aBuffer: pointer; aBufferSize: PtrUInt); overload;
    /// recognize the BOM of a text file
    // - BOM is common only with Microsoft products
    // - returns isAnsi if no BOM is available - but may be UTF-8 e.g. on POSIX
    function TextFileKind: TTextFileKind;
    /// unmap the file
    procedure UnMap;
    /// retrieve the memory buffer mapped to the file content
    property Buffer: PAnsiChar
      read fBuf;
    /// retrieve the buffer size
    property Size: PtrUInt
      read fBufSize;
    /// retrieve the mapped file size
    property FileSize: Int64
      read fFileSize;
    /// access to the low-level associated File handle (if any)
    property FileHandle: THandle
      read fFile;
  end;

  /// a TStream created from a file content, using fast memory mapping
  TSynMemoryStreamMapped = class(TSynMemoryStream)
  protected
    fMap: TMemoryMap;
    fFileStream: TFileStream;
    fFileName: TFileName;
  public
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(const aFileName: TFileName;
      aCustomSize: PtrUInt = 0; aCustomOffset: Int64 = 0); overload;
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(aFile: THandle;
      aCustomSize: PtrUInt = 0; aCustomOffset: Int64 = 0); overload;
    /// release any internal mapped file instance
    destructor Destroy; override;
    /// the file name, if created from such Create(aFileName) constructor
    property FileName: TFileName
      read fFileName;
  end;

  /// low-level access to a resource bound to the executable
  // - so that Windows is not required in your unit uses clause
  TExecutableResource = object
  private
    HResInfo: THandle;
    HGlobal: THandle;
  public
    /// the resource memory pointer, after successful Open()
    Buffer: pointer;
    /// the resource memory size in bytes, after successful Open()
    Size: PtrInt;
    /// locate and lock a resource
    // - use the current executable if Instance is left to its 0 default value
    // - returns TRUE if the resource has been found, and Buffer/Size are set
    function Open(const ResourceName: string; ResType: PChar;
      Instance: THandle = 0): boolean;
    /// unlock and finalize a resource
    procedure Close;
  end;


type
  /// store CPU and RAM usage for a given process
  // - as used by TSystemUse class
  TSystemUseData = packed record
    /// when the data has been sampled
    Timestamp: TDateTime;
    /// percent of current Kernel-space CPU usage for this process
    Kernel: single;
    /// percent of current User-space CPU usage for this process
    User: single;
    /// how many KB of working memory are used by this process
    WorkKB: cardinal;
    /// how many KB of virtual memory are used by this process
    VirtualKB: cardinal;
  end;

  /// store CPU and RAM usage history for a given process
  // - as returned by TSystemUse.History
  TSystemUseDataDynArray = array of TSystemUseData;

  /// low-level structure used to compute process memory and CPU usage
  TProcessInfo = object
  private
    {$ifdef OSWINDOWS}
    fSysPrevIdle, fSysPrevKernel, fSysPrevUser,
    fDiffIdle, fDiffKernel, fDiffUser, fDiffTotal: Int64;
    {$endif OSWINDOWS}
  public
    /// initialize the system/process resource tracking
    function Init: boolean;
    /// to be called before PerSystem() or PerProcess() iteration
    function Start: boolean;
    /// percent of current Idle/Kernel/User CPU usage for all processes
    function PerSystem(out Idle, Kernel, User: single): boolean;
    /// retrieve CPU and RAM usage for a given process
    function PerProcess(PID: cardinal; Now: PDateTime;
      out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
  end;

  /// hold low-level information about current memory usage
  // - as filled by GetMemoryInfo()
  TMemoryInfo = record
    memtotal, memfree, filetotal, filefree,
    vmtotal, vmfree, allocreserved, allocused: QWord;
    percent: integer;
  end;

  /// stores information about a disk partition
  TDiskPartition = packed record
    /// the name of this partition
    // - is the Volume name under Windows, or the Device name under POSIX
    name: RawUtf8;
    /// where this partition has been mounted
    // - e.g. 'C:' or '/home'
    // - you can use GetDiskInfo(mounted) to retrieve current space information
    mounted: TFileName;
    /// total size (in bytes) of this partition
    size: QWord;
  end;

  /// stores information about several disk partitions
  TDiskPartitions = array of TDiskPartition;


{$ifdef CPUARM}
var
  /// internal wrapper address for ReserveExecutableMemory()
  // - set to @TInterfacedObjectFake.ArmFakeStub by mormot.core.interfaces.pas
  ArmFakeStubAddr: pointer;
{$endif CPUARM}


/// cross-platform reserve some executable memory
// - using PAGE_EXECUTE_READWRITE flags on Windows, and PROT_READ or PROT_WRITE
// or PROT_EXEC on POSIX
// - this function maintain an internal list of 64KB memory pages for efficiency
// - memory blocks can not be released (don't try to use fremeem on them) and
// will be returned to the system at process finalization
function ReserveExecutableMemory(size: cardinal): pointer;

/// to be called after ReserveExecutableMemory() when you want to actually write
// the memory blocks
// - affect the mapping flags of the first memory page (4KB) of the Reserved
// buffer, so its size should be < 4KB
// - do nothing on Windows and Linux, but may be needed on OpenBSD
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);

/// return the PIDs of all running processes
// - under Windows, is a wrapper around EnumProcesses() PsAPI call
// - on Linux, will enumerate /proc/* pseudo-files
function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;

/// return the process name of a given PID
// - under Windows, is a wrapper around QueryFullProcessImageNameW/GetModuleFileNameEx
// PsAPI call
// - on Linux, will query /proc/[pid]/exe or /proc/[pid]/cmdline pseudo-file
function EnumProcessName(PID: cardinal): RawUtf8;

/// return the system-wide time usage information
// - under Windows, is a wrapper around GetSystemTimes() kernel API call
// - return false on POSIX system - call RetrieveLoadAvg() instead
function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;

/// return the system-wide time usage information
// - on LINUX, retrieve /proc/loadavg or on OSX/BSD call libc getloadavg()
// - return '' on Windows - call RetrieveSystemTimes() instead
function RetrieveLoadAvg: RawUtf8;

/// return the time and memory usage information about a given process
// - under Windows, is a wrapper around GetProcessTimes/GetProcessMemoryInfo
function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;

/// retrieve low-level information about current memory usage
// - as used by TSynMonitorMemory
// - under BSD, only memtotal/memfree/percent are properly returned
// - allocreserved and allocused are set only if withalloc is TRUE
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;

/// retrieve low-level information about a given disk partition
// - as used by TSynMonitorDisk and GetDiskPartitionsText()
// - only under Windows the Quotas are applied separately to aAvailableBytes
// in respect to global aFreeBytes
function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord
  {$ifdef OSWINDOWS}; aVolumeName: PSynUnicode = nil{$endif}): boolean;

/// retrieve low-level information about all mounted disk partitions of the system
// - returned partitions array is sorted by "mounted" ascending order
function GetDiskPartitions: TDiskPartitions;

/// call several Operating System APIs to gather 512-bit of entropy information
procedure XorOSEntropy(var e: THash512Rec);

/// low-level function returning some random binary from then available
// Operating System pseudorandom source
// - will call /dev/urandom or /dev/random under POSIX, and CryptGenRandom API
// on Windows then return TRUE, or fallback to mormot.core.base gsl_rng_taus2
// generator and return FALSE if the system API failed
// - on POSIX, only up to 32 bytes (256 bits) bits are retrieved from /dev/urandom
// or /dev/random as stated by "man urandom" Usage - then RandomBytes() padded
// - so you may consider that the output Buffer is always filled with random
// - you should not have to call this procedure, but faster and safer TAesPrng
// from mormot.crypt.core - also consider the TSystemPrng class
function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;

type
  /// available console colors
  TConsoleColor = (
    ccBlack,
    ccBlue,
    ccGreen,
    ccCyan,
    ccRed,
    ccMagenta,
    ccBrown,
    ccLightGray,
    ccDarkGray,
    ccLightBlue,
    ccLightGreen,
    ccLightCyan,
    ccLightRed,
    ccLightMagenta,
    ccYellow,
    ccWhite);

{$ifdef OSPOSIX}
var
  stdoutIsTTY: boolean;
{$endif OSPOSIX}

/// similar to Windows AllocConsole API call, to be truly cross-platform
// - do nothing on Linux/POSIX
procedure AllocConsole;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// change the console text writing color
// - you should call this procedure to initialize StdOut global variable, if
// you manually initialized the Windows console, e.g. via the following code:
// ! AllocConsole;
// ! TextColor(ccLightGray); // initialize internal console context
procedure TextColor(Color: TConsoleColor);

/// write some text to the console using a given color
procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor = ccLightGray;
  NoLineFeed: boolean = false; NoColor: boolean = false); overload;

/// change the console text background color
procedure TextBackground(Color: TConsoleColor);

/// will wait for the ENTER key to be pressed, processing Synchronize() pending
// notifications, and the internal Windows Message loop (on this OS)
// - to be used e.g. for proper work of console applications with interface-based
// service implemented as optExecInMainThread
procedure ConsoleWaitForEnterKey;

/// read all available content from stdin
// - could be used to retrieve some file piped to the command line
// - the content is not converted, so will follow the encoding used for storage
function ConsoleReadBody: RawByteString;

{$ifdef OSWINDOWS}

/// low-level access to the keyboard state of a given key
function ConsoleKeyPressed(ExpectedKey: Word): boolean;

// local RTL wrapper function to avoid linking mormot.core.unicode.pas
// when using Windows API
procedure Win32PWideCharToUtf8(P: PWideChar; Len: integer; out res: RawUtf8);

{$else}

// internal function to avoid linking mormot.core.buffers.pas
function PosixParseHex32(p: PAnsiChar): integer;

{$endif OSWINDOWS}

/// direct conversion of a UTF-8 encoded string into a console OEM-encoded string
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function Utf8ToConsole(const S: RawUtf8): RawByteString;

var
  /// low-level handle used for console writing
  // - may be overriden when console is redirected
  // - is initialized when TextColor() is called
  StdOut: THandle;


type
  /// encapsulate cross-platform loading of library files
  // - this generic class can be used for any external library (.dll/.so)
  TSynLibrary = class
  protected
    fHandle: TLibHandle;
    fLibraryPath: TFileName;
    {$ifdef OSPOSIX}
    fLibraryPathTested: boolean;
    {$endif OSPOSIX}
  public
    /// cross-platform resolution of a function entry in this library
    // - if RaiseExceptionOnFailure is set, missing entry will call FreeLib then raise it
    function Resolve(ProcName: PAnsiChar; Entry: PPointer;
      RaiseExceptionOnFailure: ExceptionClass = nil): boolean;
    /// cross-platform call to FreeLibrary() + set fHandle := 0
    // - as called by Destroy, but you can use it directly to reload the library
    procedure FreeLib;
    /// same as SafeLoadLibrary() but setting fLibraryPath and cwd on Windows
    function TryLoadLibrary(const aLibrary: array of TFileName;
      aRaiseExceptionOnFailure: ExceptionClass): boolean; virtual;
    /// release associated memory and linked library
    destructor Destroy; override;
    /// the associated library handle
    property Handle: TLibHandle
      read fHandle write fHandle;
    /// the loaded library path
    // - on POSIX, contains the full path (via dladdr) once Resolve() is called
    property LibraryPath: TFileName
      read fLibraryPath;
  end;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

/// self-modifying code - change some memory buffer in the code segment
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overridden code buffer, for further hook disabling
procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer = nil;
  LeaveUnprotected: boolean = false);

/// self-modifying code - change one PtrUInt in the code segment
procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt;
  LeaveUnprotected: boolean = false);

{$ifdef CPUINTEL}
/// low-level i386/x86_64 asm routine patch and redirection
procedure RedirectCode(Func, RedirectFunc: Pointer);
{$endif CPUINTEL}


{ **************** TSynLocker/TSynLocked and Low-Level Threading Features }

type
  /// a lightweight exclusive non-rentrant lock, stored in a PtrUInt value
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - warning: methods are non rentrant, i.e. calling Lock twice in a raw would
  // deadlock: use TRWLock or TSynLocker/TRTLCriticalSection for reentrant methods
  // - light locks are expected to be kept a very small amount of time: use
  // TSynLocker or TRTLCriticalSection if the lock may block too long
  // - only consume 4 bytes on CPU32, 8 bytes on CPU64
  {$ifdef USERECORDWITHMETHODS}
  TLightLock = record
  {$else}
  TLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt;
    // low-level function called by the Lock method when inlined
    procedure LockSpin;
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter an exclusive non-rentrant lock
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter an exclusive non-rentrant lock
    // - if returned true, caller should eventually call LightUnLock()
    // - several lightlocks, each protecting a few variables (e.g. a list), may be
    // more efficient than a more global TRTLCriticalSection/TRWLock
    function TryLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave an exclusive non-rentrant lock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;

type
  /// how TRWLock.Lock and TRWLock.UnLock high-level wrapper methods are called
  TRWLockContext = (
    cReadOnly,
    cReadWrite,
    cWrite);

  /// a lightweight multiple Reads / exclusive Write lock
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - locks are expected to be kept a very small amount of time: use TSynLocker
  // or TRTLCriticalSection if the lock may block too long
  // - warning: all methods are reentrant, but WriteLock/ReadWriteLock would
  // deadlock if called after a ReadOnlyLock
  {$ifdef USERECORDWITHMETHODS}
  TRWLock = record
  {$else}
  TRWLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt; // bit 0 = WriteLock, 1 = ReadWriteLock, >1 = ReadOnlyLock
    LastReadWriteLockThread, LastWriteLockThread: TThreadID; // to be reentrant
    LastReadWriteLockCount,  LastWriteLockCount: cardinal;
    {$ifndef ASMINTEL}
    procedure ReadOnlyLockSpin;
    {$endif ASMINTEL}
  public
    /// initialize the R/W lock
    // - not needed if TRWLock is part of a class - i.e. if was filled with 0
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called at shutdown to ensure that the R/W lock is in neutral state
    procedure AssertDone;
    /// wait for the lock to be available for reading, but not upgradable to write
    // - several readers could acquire the lock simultaneously
    // - ReadOnlyLock is reentrant since there is an internal counter
    // - warning: calling ReadWriteLock/WriteLock after ReadOnlyLock would deadlock
    // - typical usage is the following:
    // ! rwlock.ReadOnlyLock; // won't block concurrent ReadOnlyLock
    // ! try
    // !   result := Exists(value);
    // ! finally
    // !   rwlock.ReadOnlyUnLock;
    // ! end;
    procedure ReadOnlyLock;
      {$ifndef ASMINTEL} inline; {$endif}
    /// release a previous ReadOnlyLock call
    procedure ReadOnlyUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// wait for the lock to be accessible for reading - later upgradable to write
    // - will mark the lock with the current thread so that a nested WriteLock
    // would be possible, but won't block concurrent ReadOnlyLock
    // - several readers could acquire ReadOnlyLock simultaneously, but only a
    // single thread could acquire a ReadWriteLock
    // - reentrant method, and nested WriteLock is allowed
    // - typical usage is the following:
    // ! rwlock.ReadWriteLock;      // won't block concurrent ReadOnlyLock
    // ! try                        // but block other ReadWriteLock/WriteLock
    // !   result := Exists(value);
    // !   if not result then
    // !   begin
    // !     rwlock.WriteLock; // block any ReadOnlyLock/ReadWriteLock/WriteLock
    // !     try
    // !       Add(value);
    // !     finally
    // !       rwlock.WriteUnLock;
    // !     end;
    // !   end;
    // ! finally
    // !   rwlock.ReadWriteUnLock;
    // ! end;
    procedure ReadWriteLock;
    /// release a previous ReadWriteLock call
    procedure ReadWriteUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// wait for the lock to be accessible for writing
    // - the write lock is exclusive
    // - calling WriteLock within a ReadWriteLock is allowed and won't block
    // - but calling WriteLock within a ReadOnlyLock would deaadlock
    // - this method is rentrant from a single thread
    // - typical usage is the following:
    // ! rwlock.WriteLock; // block any ReadOnlyLock/ReadWriteLock/WriteLock
    // ! try
    // !   Add(value);
    // ! finally
    // !   rwlock.WriteUnLock;
    // ! end;
    procedure WriteLock;
    /// release a previous WriteLock call
    procedure WriteUnlock;
      {$ifdef FPC_OR_DELPHIXE4} inline; {$endif} // circumvent weird Delphi bug
    /// a high-level wrapper over ReadOnlyLock/ReadWriteLock/WriteLock methods
    procedure Lock(context: TRWLockContext (*{$ifndef PUREMORMOT2} = cWrite {$endif}*));
      {$ifdef HASINLINE} inline; {$endif}
    /// a high-level wrapper over ReadOnlyUnLock/ReadWriteUnLock/WriteUnLock methods
    procedure UnLock(context: TRWLockContext (*{$ifndef PUREMORMOT2} = cWrite {$endif}*));
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PRWLock = ^TRWLock;

const
  RW_FORCE: array[{write}boolean] of TRWLockContext = (
    cReadOnly,
    cWrite);
  RW_UPGRADE: array[{write}boolean] of TRWLockContext = (
    cReadOnly,
    cReadWrite);

type
  /// how TSynLocker handles its thread processing
  // - by default, uSharedLock will use the main TRTLCriticalSection
  // - you may set uRWLock and call overloaded RWLock/RWUnLock() to use our
  // lighter TRWLock - but be aware that cReadOnly followed by cReadWrite/cWrite
  // would deadlock - regular Lock/UnLock will use cWrite exclusive lock
  // - uNoLock will disable the whole locking mechanism
  TSynLockerUse = (
    uSharedLock,
    uRWLock,
    uNoLock);

  /// allow to add cross-platform locking methods to any class instance
  // - typical use is to define a Safe: TSynLocker property, call Safe.Init
  // and Safe.Done in constructor/destructor methods, and use Safe.Lock/UnLock
  // methods in a try ... finally section
  // - in respect to the TCriticalSection class, fix a potential CPU cache line
  // conflict which may degrade the multi-threading performance, as reported by
  // @http://www.delphitools.info/2011/11/30/fixing-tcriticalsection
  // - internal padding is used to safely store up to 7 values protected
  // from concurrent access with a mutex, so that SizeOf(TSynLocker)>128
  // - for object-level locking, see TSynPersistentLock which owns one such
  // instance, or call low-level fSafe := NewSynLocker in your constructor,
  // then fSafe^.DoneAndFreemem in your destructor
  // - RWUse property could replace the TRTLCriticalSection by a lighter TRWLock
  // - see also TRWLock and TSynPersistentRWLock if the multiple read / exclusive
  // write lock is better (only if the locked process does not take too much time)
  TSynLocker = object
  protected
    fSection: TRTLCriticalSection;
    fRW: TRWLock;
    fLockCount: integer;
    fInitialized: boolean;
    fRWUse: TSynLockerUse;
    function GetVariant(Index: integer): Variant;
    procedure SetVariant(Index: integer; const Value: Variant);
    function GetInt64(Index: integer): Int64;
    procedure SetInt64(Index: integer; const Value: Int64);
    function GetBool(Index: integer): boolean;
    procedure SetBool(Index: integer; const Value: boolean);
    function GetUnlockedInt64(Index: integer): Int64;
    procedure SetUnlockedInt64(Index: integer; const Value: Int64);
    function GetPointer(Index: integer): Pointer;
    procedure SetPointer(Index: integer; const Value: Pointer);
    function GetUtf8(Index: integer): RawUtf8;
    procedure SetUtf8(Index: integer; const Value: RawUtf8);
    function GetIsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// number of values stored in the internal Padding[] array
    // - equals 0 if no value is actually stored, or a 1..7 number otherwise
    // - you should not have to use this field, but for optimized low-level
    // direct access to Padding[] values, within a Lock/UnLock safe block
    PaddingUsedCount: integer;
    /// internal padding data, also used to store up to 7 variant values
    // - this memory buffer will ensure no CPU cache line mixup occurs
    // - you should not use this field directly, but rather the Locked[],
    // LockedInt64[], LockedUtf8[] or LockedPointer[] methods
    // - if you want to access those array values, ensure you protect them
    // using a Safe.Lock; try ... Padding[n] ... finally Safe.Unlock structure,
    // and maintain the PaddingUsedCount field accurately
    Padding: array[0..6] of TVarData;
    /// initialize the mutex
    // - calling this method is mandatory (e.g. in the class constructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Init;
    /// finalize the mutex
    // - calling this method is mandatory (e.g. in the class destructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Done;
    /// finalize the mutex, and call FreeMem() on the pointer of this instance
    // - should have been initiazed with a NewSynLocker call
    procedure DoneAndFreeMem;
    /// low-level acquisition of the lock, depending on RWUse property
    // - warning: if RWUse=uRWLock, this method will use the internal TRWLock
    procedure RWLock(context: TRWLockContext);
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// low-level release of the lock, depending on RWUse property
    procedure RWUnLock(context: TRWLockContext);
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// lock the instance for exclusive access
    // - redirects to RWLock(cWrite)
    // - with default RWUse=uSharedLock, this method is re-entrant from the same
    // thread i.e. you can nest Lock/UnLock calls in the same thread
    // - warning: with RWUse=uRWLock, this method would deadlock on nested calls
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! Safe.Lock;
    // ! try
    // !   ...
    // ! finally
    // !   Safe.Unlock;
    // ! end;
    procedure Lock;
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// will try to acquire the mutex
    // - do nothing and return false if RWUse is not the default uSharedLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLock then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLock: boolean;
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// will try to acquire the mutex for a given time
    // - just wait and return false if RWUse is not the default uSharedLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLockMS(100) then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLockMS(retryms: integer; terminated: PBoolean = nil): boolean;
    /// release the instance for exclusive access
    // - redirects to RWUnLock(cWrite)
    // - each Lock/TryLock should have its exact UnLock opposite, so a
    // try..finally block is mandatory for safe code
    procedure UnLock; overload;
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// will enter the mutex until the IUnknown reference is released
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with Safe.ProtectMethod do
    // !  begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// returns true if the mutex is currently locked by another thread
    // - only accurate with default RWUse=uSharedLock
    property IsLocked: boolean
      read GetIsLocked;
    /// returns true if the Init method has been called for this mutex
    // - is only relevant if the whole object has been previously filled with 0,
    // i.e. as part of a class or as global variable, but won't be accurate
    // when allocated on stack
    property IsInitialized: boolean
      read fInitialized;
    /// safe locked access to a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // LockedBool, LockedInt64, LockedPointer and LockedUtf8 array properties
    // - returns null if the Index is out of range
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property Locked[Index: integer]: Variant
      read GetVariant write SetVariant;
    /// safe locked access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedInt64[Index: integer]: Int64
      read GetInt64 write SetInt64;
    /// safe locked access to a boolean value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedInt64, LockedPointer and LockedUtf8 array properties
    // - value will be stored internally as a varboolean variant
    // - returns nil if the Index is out of range, or does not store a boolean
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedBool[Index: integer]: boolean
      read GetBool write SetBool;
    /// safe locked access to a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedBool, LockedInt64 and LockedUtf8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns nil if the Index is out of range, or does not store a pointer
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedPointer[Index: integer]: Pointer
      read GetPointer write SetPointer;
    /// safe locked access to an UTF-8 string value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedPointer array properties
    // - UTF-8 string will be stored internally as a varString variant
    // - returns '' if the Index is out of range, or does not store a string
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedUtf8[Index: integer]: RawUtf8
      read GetUtf8 write SetUtf8;
    /// safe locked in-place increment to an Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns the newly stored value
    // - if the internal value is not defined yet, would use 0 as default value
    function LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
    /// safe locked in-place exchange of a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - returns the previous stored value, or null if the Index is out of range
    function LockedExchange(Index: integer; const Value: variant): variant;
    /// safe locked in-place exchange of a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns the previous stored value, nil if the Index is out of range,
    // or does not store a pointer
    function LockedPointerExchange(Index: integer; Value: pointer): pointer;
    /// unsafe access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - you should rather call LockedInt64[] property, or use this property
    // with a Lock; try ... finally UnLock block
    property UnlockedInt64[Index: integer]: Int64
      read GetUnlockedInt64 write SetUnlockedInt64;
    /// how RWLock/RWUnLock would be processed
    property RWUse: TSynLockerUse
      read fRWUse write fRWUse;
  end;

  /// a pointer to a TSynLocker mutex instance
  // - see also NewSynLocker and TSynLocker.DoneAndFreemem functions
  PSynLocker = ^TSynLocker;

  /// raw class used by TAutoLocker.ProtectMethod and TSynLocker.ProtectMethod
  // - defined here for use by TAutoLocker in mormot.core.data.pas
  TAutoLock = class(TInterfacedObject)
  protected
    fLock: PSynLocker;
  public
    constructor Create(aLock: PSynLocker);
    destructor Destroy; override;
  end;


/// initialize a TSynLocker instance from heap
// - call DoneandFreeMem to release the associated memory and OS mutex
// - is used e.g. in TSynPersistentLock to reduce class instance size
function NewSynLocker: PSynLocker;

type
  {$M+}

  /// a persistent-agnostic alternative to TSynPersistentLock
  // - can be used as base class when custom JSON persistence is not needed
  // - consider a TRWLock field as a lighter multi read / exclusive write option
  TSynLocked = class
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the instance, and its associated lock
    // - is defined as virtual, just like TObjectWithCustomCreate/TSynPersistent
    constructor Create; virtual;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker
      read fSafe;
  end;

  {$M-}

  /// meta-class definition of the TSynLocked hierarchy
  TSynLockedClass = class of TSynLocked;

  /// a thread-safe Pierre L'Ecuyer software random generator
  // - just wrap TLecuyer with a LighLock()
  // - should not be used, unless may be slightly faster than a threadvar
  TLecuyerThreadSafe = object
    Safe: TLightLock;
    Generator: TLecuyer;
    /// compute the next 32-bit generated value
    function Next: cardinal; overload;
    /// compute a 64-bit floating point value
    function NextDouble: double;
    /// XOR some memory buffer with random bytes
    procedure Fill(dest: pointer; count: integer);
    /// fill some string[31] with 7-bit ASCII random text
    procedure FillShort31(var dest: TShort31);
  end;

  TThreadIDDynArray = array of TThreadID;

var
  /// a global thread-safe Pierre L'Ecuyer software random generator
  SharedRandom: TLecuyerThreadSafe;

{$ifdef OSPOSIX}
  /// could be set to TRUE to force SleepHiRes(0) to call the sched_yield API
  // - in practice, it has been reported as buggy under POSIX systems
  // - even Linus Torvald himself raged against its usage - see e.g.
  // https://www.realworldtech.com/forum/?threadid=189711&curpostid=189752
  // - you may tempt the devil and try it by yourself
  SleepHiRes0Yield: boolean = false;
{$endif OSPOSIX}

/// similar to Windows sleep() API call, to be truly cross-platform
// - using millisecond resolution
// - SleepHiRes(0) calls ThreadSwitch on Windows, but POSIX version will
// wait 10 microsecond unless SleepHiRes0Yield is forced to true (bad idea)
// - in respect to RTL's Sleep() function, it will return on ESysEINTR if was
// interrupted by any OS signal
// - warning: wait typically the next system timer interrupt on Windows, which
// is every 16ms by default; as a consequence, never rely on the ms supplied
// value to guess the elapsed time, but call GetTickCount64
procedure SleepHiRes(ms: cardinal); overload;

/// similar to Windows sleep() API call, but truly cross-platform and checking
// the Terminated flag during its wait for quick abort response
// - returns true if terminated^ was set to true
function SleepHiRes(ms: cardinal; var terminated: boolean;
  terminatedvalue: boolean = true): boolean; overload;

/// call SleepHiRes() taking count of the activity, in 0/1/5/50/120-250 ms steps
// - range is agressively designed burning some CPU in favor of responsiveness
// - should reset start := 0 when some activity occured
// - would optionally return if terminated^ is set, or event is signaled
// - returns the current GetTickCount64 value
function SleepStep(var start: Int64; terminated: PBoolean = nil;
  event: TEvent = nil): Int64;

/// compute optimal sleep time as 0/1/5/50 then 120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
function SleepDelay(elapsed: PtrInt): PtrInt;

/// compute optimal sleep time as SleepStep, in 0/1/5/50/120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
function SleepStepTime(var start, tix: Int64; endtix: PInt64 = nil): PtrInt;

/// similar to Windows SwitchToThread API call, to be truly cross-platform
// - call fpnanosleep(10) on POSIX systems
procedure SwitchToThread;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// try LockedExc() in a loop, calling SwitchToThread after some spinning
procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);

/// low-level naming of a thread
// - under Linux/FPC, calls pthread_setname_np API which truncates to 16 chars
procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);

/// name the current thread so that it would be easily identified in the IDE debugger
// - could then be retrieved by CurrentThreadName/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const); overload;

/// name the current thread so that it would be easily identified in the IDE debugger
// - could also be retrieved by CurrentThreadName/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Name: RawUtf8); overload;

var
  /// name a thread so that it would be easily identified in the IDE debugger
  // - default implementation does nothing, unless mormot.core.log is included
  // - you can force this function to do nothing by setting the NOSETTHREADNAME
  // conditional, if you have issues with this feature when debugging your app
  // - most meaningless patterns (like 'TSql') are trimmed to reduce the
  // resulting length - which is convenient e.g. with POSIX truncation to 16 chars
  // - you can retrieve the name later on using CurrentThreadName
  // - this method will register TSynLog.LogThreadName(), so threads calling it
  // should also call TSynLogFamily.OnThreadEnded/TSynLog.NotifyThreadEnded
  SetThreadName: procedure(ThreadID: TThreadID; const Format: RawUtf8;
    const Args: array of const);

threadvar
  /// low-level access to the thread name, as set by SetThreadName()
  // - since threadvar can't contain managed strings, it is limited to 31 chars,
  // which is enough since POSIX truncates to 16 chars and SetThreadName does
  // trim meaningless patterns
  CurrentThreadName: TShort31;

/// retrieve the thread name, as set by SetThreadName()
// - if possible, direct CurrentThreadName threadvar access is slightly faster
// - will return the CurrentThreadName value, truncated to 31 chars
function GetCurrentThreadName: RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// returns the thread id and the thread name as a ShortString
// - returns e.g. 'Thread 0001abcd [shortthreadname]'
// - for convenient use when logging or raising an exception
function GetCurrentThreadInfo: ShortString;

/// enter a process-wide giant lock for thread-safe shared process
// - shall be protected as such:
// ! GlobalLock;
// ! try
// !   .... do something thread-safe but as short as possible
// ! finally
// !  GlobalUnLock;
// ! end;
// - you should better not use such a giant-lock, but an instance-dedicated
// critical section/TSynLocker or TRWLock - these functions are just here to be
// convenient, for non time-critical process (e.g. singleton initialization)
procedure GlobalLock;

/// release the giant lock for thread-safe shared process
procedure GlobalUnLock;

/// framework will register here some instances to be released eventually
// - better in this root unit than in each finalization section
// - its use is protected by the GlobalLock
function RegisterGlobalShutdownRelease(Instance: TObject;
  SearchExisting: boolean = false): pointer;


{ ****************** Unix Daemon and Windows Service Support }

{$ifdef OSWINDOWS}

{ *** some minimal Windows API definitions, replacing WinSvc.pas missing for FPC }

const
  SERVICE_QUERY_CONFIG         = $0001;
  SERVICE_CHANGE_CONFIG        = $0002;
  SERVICE_QUERY_STATUS         = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START                = $0010;
  SERVICE_STOP                 = $0020;
  SERVICE_PAUSE_CONTINUE       = $0040;
  SERVICE_INTERROGATE          = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;
  SERVICE_ALL_ACCESS           = STANDARD_RIGHTS_REQUIRED or
                                 SERVICE_QUERY_CONFIG or
                                 SERVICE_CHANGE_CONFIG or
                                 SERVICE_QUERY_STATUS or
                                 SERVICE_ENUMERATE_DEPENDENTS or
                                 SERVICE_START or
                                 SERVICE_STOP or
                                 SERVICE_PAUSE_CONTINUE or
                                 SERVICE_INTERROGATE or
                                 SERVICE_USER_DEFINED_CONTROL;

  SC_MANAGER_CONNECT            = $0001;
  SC_MANAGER_CREATE_SERVICE     = $0002;
  SC_MANAGER_ENUMERATE_SERVICE  = $0004;
  SC_MANAGER_LOCK               = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS  = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;
  SC_MANAGER_ALL_ACCESS         = STANDARD_RIGHTS_REQUIRED or
                                  SC_MANAGER_CONNECT or
                                  SC_MANAGER_CREATE_SERVICE or
                                  SC_MANAGER_ENUMERATE_SERVICE or
                                  SC_MANAGER_LOCK or
                                  SC_MANAGER_QUERY_LOCK_STATUS or
                                  SC_MANAGER_MODIFY_BOOT_CONFIG;

  SERVICE_CONFIG_DESCRIPTION    = $0001;

  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_INTERACTIVE_PROCESS   = $00000100;

  SERVICE_BOOT_START            = $00000000;
  SERVICE_SYSTEM_START          = $00000001;
  SERVICE_AUTO_START            = $00000002;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_DISABLED              = $00000004;
  SERVICE_ERROR_IGNORE          = $00000000;
  SERVICE_ERROR_NORMAL          = $00000001;
  SERVICE_ERROR_SEVERE          = $00000002;
  SERVICE_ERROR_CRITICAL        = $00000003;

  SERVICE_CONTROL_STOP          = $00000001;
  SERVICE_CONTROL_PAUSE         = $00000002;
  SERVICE_CONTROL_CONTINUE      = $00000003;
  SERVICE_CONTROL_INTERROGATE   = $00000004;
  SERVICE_CONTROL_SHUTDOWN      = $00000005;
  SERVICE_STOPPED               = $00000001;
  SERVICE_START_PENDING         = $00000002;
  SERVICE_STOP_PENDING          = $00000003;
  SERVICE_RUNNING               = $00000004;
  SERVICE_CONTINUE_PENDING      = $00000005;
  SERVICE_PAUSE_PENDING         = $00000006;
  SERVICE_PAUSED                = $00000007;

type
  PServiceStatus = ^TServiceStatus;
  TServiceStatus = object
  public
    dwServiceType: cardinal;
    dwCurrentState: cardinal;
    dwControlsAccepted: cardinal;
    dwWin32ExitCode: cardinal;
    dwServiceSpecificExitCode: cardinal;
    dwCheckPoint: cardinal;
    dwWaitHint: cardinal;
  end;

  PServiceStatusProcess = ^TServiceStatusProcess;
  TServiceStatusProcess = object(TServiceStatus)
  public
    dwProcessId: cardinal;
    dwServiceFlags: cardinal;
  end;

  SC_HANDLE = THandle;
  SERVICE_STATUS_HANDLE = cardinal;
  TServiceTableEntry = record
    lpServiceName: PChar;
    lpServiceProc: procedure(ArgCount: cardinal; Args: PPChar); stdcall;
  end;
  PServiceTableEntry = ^TServiceTableEntry;

  {$Z4}
  SC_STATUS_TYPE = (SC_STATUS_PROCESS_INFO);
  {$Z1}

function OpenSCManager(lpMachineName, lpDatabaseName: PChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32
  name 'OpenSCManager' + _AW;
function ChangeServiceConfig2(hService: SC_HANDLE; dwsInfoLevel: cardinal;
  lpInfo: Pointer): BOOL; stdcall; external advapi32 name 'ChangeServiceConfig2W';
function StartService(hService: SC_HANDLE; dwNumServiceArgs: cardinal;
  lpServiceArgVectors: Pointer): BOOL; stdcall; external advapi32
  name 'StartService' + _AW;
function CreateService(hSCManager: SC_HANDLE; lpServiceName, lpDisplayName: PChar;
  dwDesiredAccess, dwServiceType, dwStartType, dwErrorControl: cardinal;
  lpBinaryPathName, lpLoadOrderGroup: PChar; lpdwTagId: LPDWORD; lpDependencies,
  lpServiceStartName, lpPassword: PChar): SC_HANDLE; stdcall; external advapi32
  name 'CreateService' + _AW;
function OpenService(hSCManager: SC_HANDLE; lpServiceName: PChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32
  name 'OpenService' + _AW;
function DeleteService(hService: SC_HANDLE): BOOL; stdcall; external advapi32;
function CloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall; external advapi32;
function QueryServiceStatus(hService: SC_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function QueryServiceStatusEx(hService: SC_HANDLE;
  InfoLevel: SC_STATUS_TYPE; lpBuffer: Pointer; cbBufSize: cardinal;
  var pcbBytesNeeded: cardinal): BOOL; stdcall; external advapi32;
function ControlService(hService: SC_HANDLE; dwControl: cardinal;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function RegisterServiceCtrlHandler(lpServiceName: PChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall; external advapi32
  name 'RegisterServiceCtrlHandler' + _AW;
function StartServiceCtrlDispatcher(
  lpServiceStartTable: PServiceTableEntry): BOOL; stdcall; external advapi32
  name 'StartServiceCtrlDispatcher' + _AW;


{ *** high level classes to define and manage Windows Services }

var
  /// can be assigned from TSynLog.DoLog class method for
  // TServiceController/TService logging
  // - default is nil, i.e. disabling logging, since it may interfere with the
  // logging process of the Windows Service itself
  WindowsServiceLog: TSynLogProc;

type
  /// all possible states of the service
  TServiceState = (
    ssNotInstalled,
    ssStopped,
    ssStarting,
    ssStopping,
    ssRunning,
    ssResuming,
    ssPausing,
    ssPaused,
    ssErrorRetrievingState);

  /// TServiceControler class is intended to create a new Windows Service instance
  // or to maintain (that is start, stop, pause, resume...) an existing Service
  // - to provide the service itself, use the TService class
  TServiceController = class
  protected
    fSCHandle: THandle;
    fHandle: THandle;
    fStatus: TServiceStatus;
    fName: RawUtf8;
  protected
    function GetStatus: TServiceStatus;
    function GetState: TServiceState;
  public
    /// create a new Windows Service and control it and/or its configuration
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DisplayName - display name of a service.
    // - Path - a path to binary (executable) of the service created.
    // - OrderGroup - an order group name (unnecessary)
    // - Dependencies - string containing a list with names of services, which must
    // start before (every name should be separated with #0, entire
    // list should be separated with #0#0. Or, an empty string can be
    // passed if there is no dependancy).
    // - Username - login name. For service type SERVICE_WIN32_OWN_PROCESS, the
    // account name in the form of "DomainName\Username"; If the account
    // belongs to the built-in domain, ".\Username" can be specified;
    // Services of type SERVICE_WIN32_SHARE_PROCESS are not allowed to
    // specify an account other than LocalSystem. If '' is specified, the
    // service will be logged on as the 'LocalSystem' account, in which
    // case, the Password parameter must be empty too.
    // - Password - a password for login name. If the service type is
    // SERVICE_KERNEL_DRIVER or SERVICE_FILE_SYSTEM_DRIVER,
    // this parameter is ignored.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS (default value), SERVICE_CHANGE_CONFIG,
    // SERVICE_ENUMERATE_DEPENDENTS, SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE,
    // SERVICE_QUERY_CONFIG, SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP,
    // SERVICE_USER_DEFINED_CONTROL
    // - ServiceType - a set of following flags:
    // SERVICE_WIN32_OWN_PROCESS (default value, which specifies a Win32 service
    // that runs in its own process), SERVICE_WIN32_SHARE_PROCESS,
    // SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER,
    // SERVICE_INTERACTIVE_PROCESS (default value, which enables a Win32 service
    // process to interact with the desktop)
    // - StartType - one of following values:
    // SERVICE_BOOT_START, SERVICE_SYSTEM_START,
    // SERVICE_AUTO_START (which specifies a device driver or service started by
    // the service control manager automatically during system startup),
    // SERVICE_DEMAND_START (default value, which specifies a service started by
    // a service control manager when a process calls the StartService function,
    // that is the TServiceController.Start method), SERVICE_DISABLED
    // - ErrorControl - one of following:
    // SERVICE_ERROR_IGNORE, SERVICE_ERROR_NORMAL (default value, by which
    // the startup program logs the error and displays a message but continues
    // the startup operation), SERVICE_ERROR_SEVERE,
    // SERVICE_ERROR_CRITICAL
    constructor CreateNewService(
      const TargetComputer, DatabaseName, Name, DisplayName, Path: string;
      const OrderGroup: string = ''; const Dependencies: string = '';
      const Username: string = ''; const Password: string = '';
      DesiredAccess: cardinal = SERVICE_ALL_ACCESS;
      ServiceType: cardinal = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      StartType: cardinal = SERVICE_DEMAND_START;
      ErrorControl: cardinal = SERVICE_ERROR_NORMAL);
    /// wrapper around CreateNewService() to install the current executable as service
    class function Install(const Name, DisplayName, Description: string;
      AutoStart: boolean; ExeName: TFileName = '';
      Dependencies: string = ''): TServiceState;
    /// open an existing service, in order to control it or its configuration
    // from your application
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS, SERVICE_CHANGE_CONFIG, SERVICE_ENUMERATE_DEPENDENTS,
    // SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE, SERVICE_QUERY_CONFIG,
    // SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP, SERVICE_USER_DEFINED_CONTROL
    constructor CreateOpenService(
      const TargetComputer, DataBaseName, Name: string;
      DesiredAccess: cardinal = SERVICE_ALL_ACCESS);
    /// release memory and handles
    destructor Destroy; override;
    /// Handle of SC manager
    property SCHandle: THandle
      read fSCHandle;
    /// Handle of service opened or created
    // - its value is 0 if something failed in any Create*() method
    property Handle: THandle
      read fHandle;
    /// Retrieve the Current status of the service
    property Status: TServiceStatus
      read GetStatus;
    /// Retrieve the Current state of the service
    property State: TServiceState
      read GetState;
    /// Requests the service to stop
    function Stop: boolean;
    /// Requests the service to pause
    function Pause: boolean;
    /// Requests the paused service to resume
    function Resume: boolean;
    /// Requests the service to update immediately its current status information
    // to the service control manager
    function Refresh: boolean;
    /// Request the service to shutdown
    // - this function always return false
    function Shutdown: boolean;
    /// Removes service from the system, i.e. close the Service
    function Delete: boolean;
    /// starts the execution of a service with some specified arguments
    // - this version expect PChar pointers, either AnsiString (for FPC and old
    //  Delphi compiler), either UnicodeString (till Delphi 2009)
    function Start(const Args: array of PChar): boolean;
    /// try to define the description text of this service
    procedure SetDescription(const Description: string);
    /// this class method will check the command line parameters, and will let
    //  control the service according to it
    // - MyServiceSetup.exe /install will install the service
    // - MyServiceSetup.exe /start   will start the service
    // - MyServiceSetup.exe /stop    will stop the service
    // - MyServiceSetup.exe /uninstall will uninstall the service
    // - so that you can write in the main block of your .dpr:
    // !CheckParameters('MyService.exe',HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
    // - if ExeFileName='', it will install the current executable
    // - optional Description and Dependencies text may be specified
    class procedure CheckParameters(const ExeFileName: TFileName;
      const ServiceName, DisplayName, Description: string;
      const Dependencies: string = '');
  end;

  {$M+}
  TService = class;
  {$M-}

  /// callback procedure for Windows Service Controller
  TServiceControlHandler = procedure(CtrlCode: cardinal); stdcall;

  /// event triggered for Control handler
  TServiceControlEvent = procedure(Sender: TService; Code: cardinal) of object;

  /// event triggered to implement the Service functionality
  TServiceEvent = procedure(Sender: TService) of object;

  /// let an executable implement a Windows Service
  TService = class
  protected
    fSName: string;
    fDName: string;
    fStartType: cardinal;
    fServiceType: cardinal;
    fData: cardinal;
    fControlHandler: TServiceControlHandler;
    fOnControl: TServiceControlEvent;
    fOnInterrogate: TServiceEvent;
    fOnPause: TServiceEvent;
    fOnShutdown: TServiceEvent;
    fOnStart: TServiceEvent;
    fOnExecute: TServiceEvent;
    fOnResume: TServiceEvent;
    fOnStop: TServiceEvent;
    fStatusRec: TServiceStatus;
    fArgsList: array of string;
    fStatusHandle: THandle;
    function GetArgCount: Integer;
    function GetArgs(Idx: Integer): string;
    function GetInstalled: boolean;
    procedure SetStatus(const Value: TServiceStatus);
    procedure CtrlHandle(Code: cardinal);
    function GetControlHandler: TServiceControlHandler;
    procedure SetControlHandler(const Value: TServiceControlHandler);
  public
    /// this method is the main service entrance, from the OS point of view
    // - it will call OnControl/OnStop/OnPause/OnResume/OnShutdown events
    // - and report the service status to the system (via ReportStatus method)
    procedure DoCtrlHandle(Code: cardinal); virtual;
    /// Creates the service
    // - the service is added to the internal registered services
    // - main application must call the global ServicesRun procedure to actually
    // start the services
    // - caller must free the TService instance when it's no longer used
    constructor Create(const aServiceName, aDisplayName: string); reintroduce; virtual;
    /// Reports new status to the system
    function ReportStatus(dwState, dwExitCode, dwWait: cardinal): BOOL;
    /// Installs the service in the database
    // - return true on success
    // - create a local TServiceController with the current executable file,
    // with the supplied command line parameters
    function Install(const Params: string = ''): boolean;
    /// Removes the service from database
    //  - uses a local TServiceController with the current Service Name
    procedure Remove;
    /// Starts the service
    //  - uses a local TServiceController with the current Service Name
    procedure Start;
    /// Stops the service
    // - uses a local TServiceController with the current Service Name
    procedure Stop;
    /// this is the main method, in which the Service should implement its run
    procedure Execute; virtual;

    /// Number of arguments passed to the service by the service controler
    property ArgCount: Integer
      read GetArgCount;
    /// List of arguments passed to the service by the service controler
    property Args[Idx: Integer]: string
      read GetArgs;
    /// Any data You wish to associate with the service object
    property Data: cardinal
      read FData write FData;
    /// Whether service is installed in DataBase
    // - uses a local TServiceController to check if the current Service Name exists
    property Installed: boolean
      read GetInstalled;
    /// Current service status
    // - To report new status to the system, assign another
    // value to this record, or use ReportStatus method (preferred)
    property Status: TServiceStatus
      read fStatusRec write SetStatus;
    /// Callback handler for Windows Service Controller
    // - if handler is not set, then auto generated handler calls DoCtrlHandle
    // (note that this auto-generated stubb is... not working yet - so you should
    // either set your own procedure to this property, or use TServiceSingle)
    // - a typical control handler may be defined as such:
    // ! var MyGlobalService: TService;
    // !
    // ! procedure MyServiceControlHandler(Opcode: LongWord); stdcall;
    // ! begin
    // !   if MyGlobalService<>nil then
    // !     MyGlobalService.DoCtrlHandle(Opcode);
    // ! end;
    // !
    // ! ...
    // ! MyGlobalService := TService.Create(...
    // ! MyGlobalService.ControlHandler := MyServiceControlHandler;
    property ControlHandler: TServiceControlHandler
      read GetControlHandler write SetControlHandler;
    /// Start event is executed before the main service thread (i.e. in the Execute method)
    property OnStart: TServiceEvent
      read fOnStart write fOnStart;
    /// custom Execute event
    // - launched in the main service thread (i.e. in the Execute method)
    property OnExecute: TServiceEvent
      read fOnExecute write fOnExecute;
    /// custom event triggered when a Control Code is received from Windows
    property OnControl: TServiceControlEvent
      read fOnControl write fOnControl;
    /// custom event triggered when the service is stopped
    property OnStop: TServiceEvent
      read fOnStop write fOnStop;
    /// custom event triggered when the service is paused
    property OnPause: TServiceEvent
      read fOnPause write fOnPause;
    /// custom event triggered when the service is resumed
    property OnResume: TServiceEvent
      read fOnResume write fOnResume;
    /// custom event triggered when the service receive an Interrogate
    property OnInterrogate: TServiceEvent
      read fOnInterrogate write fOnInterrogate;
    /// custom event triggered when the service is shut down
    property OnShutdown: TServiceEvent
      read fOnShutdown write fOnShutdown;
  published
    /// Name of the service. Must be unique
    property ServiceName: string
      read fSName;
    /// Display name of the service
    property DisplayName: string
      read fDName write fDName;
    /// Type of service
    property ServiceType: cardinal
      read fServiceType write fServiceType;
    /// Type of start of service
    property StartType: cardinal
      read fStartType write fStartType;
  end;

  /// inherit from this class if your application has a single Windows Service
  // - note that the TService jumper does not work well - so use this instead
  TServiceSingle = class(TService)
  public
    /// will set a global function as service controller
    constructor Create(const aServiceName, aDisplayName: string); override;
    /// will release the global service controller
    destructor Destroy; override;
  end;


var
  /// the main TService instance running in the current executable
  ServiceSingle: TServiceSingle = nil;

/// launch the registered Service execution
// - ServiceSingle provided by this aplication is sent to the operating system
// - returns TRUE on success
// - returns FALSE on error (to get extended information, call GetLastError)
function ServiceSingleRun: boolean;

/// convert the Control Code retrieved from Windows into a service state
// enumeration item
function CurrentStateToServiceState(CurrentState: cardinal): TServiceState;

/// return the ready to be displayed text of a TServiceState value
function ServiceStateText(State: TServiceState): string;

function ToText(st: TServiceState): PShortString; overload;

/// return service PID
function GetServicePid(const aServiceName: string): cardinal;

/// kill Windows process
function KillProcess(pid: cardinal; waitseconds: integer = 30): boolean;

{$else}

/// low-level function able to properly run or fork the current process
// then execute the start/stop methods of a TSynDaemon / TDDDDaemon instance
// - fork will create a local /run/[ProgramName]-[ProgramPathHash].pid file name
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; const onlog: TSynLogProc = nil;
  const servicename: string = '');

/// kill a process previously created by RunUntilSigTerminated(dofork=true)
// - will lookup a local /run/[ProgramName]-[ProgramPathHash].pid file name to
// retrieve the actual PID to be killed, then send a SIGTERM, and wait
// waitseconds for the .pid file to disapear
// - returns true on success, false on error (e.g. no valid .pid file or
// the file didn't disappear, which may mean that the daemon is broken)
function RunUntilSigTerminatedForKill(waitseconds: integer = 30): boolean;

/// local .pid file name as created by RunUntilSigTerminated(dofork=true)
function RunUntilSigTerminatedPidFile: TFileName;

var
  /// once SynDaemonIntercept has been called, this global variable
  // contains the SIGQUIT / SIGTERM / SIGINT received signal
  SynDaemonTerminated: integer;

/// enable low-level interception of executable stop signals
// - any SIGQUIT / SIGTERM / SIGINT signal will set appropriately the global
// SynDaemonTerminated variable, with an optional logged entry to log
// - as called e.g. by RunUntilSigTerminated()
// - you can call this method several times with no issue
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure SynDaemonIntercept(const onlog: TSynLogProc = nil);

{$endif OSWINDOWS}

type
  /// command line patterns recognized by ParseCommandArgs()
  TParseCommand = (
    pcHasRedirection,
    pcHasSubCommand,
    pcHasParenthesis,
    pcHasJobControl,
    pcHasWildcard,
    pcHasShellVariable,
    pcUnbalancedSingleQuote,
    pcUnbalancedDoubleQuote,
    pcTooManyArguments,
    pcInvalidCommand,
    pcHasEndingBackSlash);
  TParseCommands = set of TParseCommand;
  PParseCommands = ^TParseCommands;

  /// used to store references of arguments recognized by ParseCommandArgs()
  TParseCommandsArgs = array[0..31] of PAnsiChar;
  PParseCommandsArgs = ^TParseCommandsArgs;

const
  /// identifies some bash-specific processing
  PARSECOMMAND_BASH =
    [pcHasRedirection .. pcHasShellVariable];

  /// identifies obvious invalid content
  PARSECOMMAND_ERROR =
    [pcUnbalancedSingleQuote .. pcHasEndingBackSlash];

/// low-level parsing of a RunCommand() execution command
// - parse and fills argv^[0..argc^-1] with corresponding arguments, after
// un-escaping and un-quoting if applicable, using temp^ to store the content
// - if argv=nil, do only the parsing, not the argument extraction - could be
// used for fast validation of the command line syntax
// - you can force arguments OS flavor using the posix parameter - note that
// Windows parsing is not consistent by itself (e.g. double quoting or
// escaping depends on the actual executable called) so returned flags
// should be considered as indicative only with posix=false
function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs = nil;
  argc: PInteger = nil; temp: PRawUtf8 = nil;
  posix: boolean = {$ifdef OSWINDOWS} false {$else} true {$endif}): TParseCommands;

/// like SysUtils.ExecuteProcess, but allowing not to wait for the process to finish
// - optional env value follows 'n1=v1'#0'n2=v2'#0'n3=v3'#0#0 Windows layout
function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2: TFileName = ''; const arg3: TFileName = '';
  const arg4: TFileName = ''; const arg5: TFileName = '';
  const env: TFileName = ''; envaddexisting: boolean = false): integer;

/// like fpSystem, but cross-platform
// - under POSIX, calls bash only if needed, after ParseCommandArgs() analysis
// - under Windows (especially Windows 10), creating a process can be dead slow
// https://randomascii.wordpress.com/2019/04/21/on2-in-createprocess
function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName = ''; envaddexisting: boolean = false;
  parsed: PParseCommands = nil): integer;




implementation

// those include files hold all OS-specific functions
// note: the *.inc files start with their own "uses" clause, so both $include
// should remain here, just after the "implementation" clause

{$ifdef OSPOSIX}
  {$include mormot.core.os.posix.inc}
{$endif OSPOSIX}

{$ifdef OSWINDOWS}
  {$include mormot.core.os.windows.inc}
{$endif OSWINDOWS}


{ ****************** Some Cross-System Type and Constant Definitions }

var
  // live cache array to avoid memory allocations
  ReasonCache: array[1..5, 0..13] of RawUtf8;

procedure StatusCode2Reason(Code: cardinal; var Reason: RawUtf8);
begin
  case Code of
    HTTP_CONTINUE:
      Reason := 'Continue';
    HTTP_SWITCHINGPROTOCOLS:
      Reason := 'Switching Protocols';
    HTTP_SUCCESS:
      Reason := 'OK';
    HTTP_CREATED:
      Reason := 'Created';
    HTTP_ACCEPTED:
      Reason := 'Accepted';
    HTTP_NONAUTHORIZEDINFO:
      Reason := 'Non-Authoritative Information';
    HTTP_NOCONTENT:
      Reason := 'No Content';
    HTTP_RESETCONTENT:
      Reason := 'Reset Content';
    HTTP_PARTIALCONTENT:
      Reason := 'Partial Content';
    207:
      Reason := 'Multi-Status';
    HTTP_MULTIPLECHOICES:
      Reason := 'Multiple Choices';
    HTTP_MOVEDPERMANENTLY:
      Reason := 'Moved Permanently';
    HTTP_FOUND:
      Reason := 'Found';
    HTTP_SEEOTHER:
      Reason := 'See Other';
    HTTP_NOTMODIFIED:
      Reason := 'Not Modified';
    HTTP_USEPROXY:
      Reason := 'Use Proxy';
    HTTP_TEMPORARYREDIRECT:
      Reason := 'Temporary Redirect';
    308:
      Reason := 'Permanent Redirect';
    HTTP_BADREQUEST:
      Reason := 'Bad Request';
    HTTP_UNAUTHORIZED:
      Reason := 'Unauthorized';
    HTTP_FORBIDDEN:
      Reason := 'Forbidden';
    HTTP_NOTFOUND:
      Reason := 'Not Found';
    HTTP_NOTALLOWED:
      Reason := 'Method Not Allowed';
    HTTP_NOTACCEPTABLE:
      Reason := 'Not Acceptable';
    HTTP_PROXYAUTHREQUIRED:
      Reason := 'Proxy Authentication Required';
    HTTP_TIMEOUT:
      Reason := 'Request Timeout';
    HTTP_CONFLICT:
      Reason := 'Conflict';
    410:
      Reason := 'Gone';
    411:
      Reason := 'Length Required';
    412:
      Reason := 'Precondition Failed';
    HTTP_PAYLOADTOOLARGE:
      Reason := 'Payload Too Large';
    414:
      Reason := 'URI Too Long';
    415:
      Reason := 'Unsupported Media Type';
    HTTP_RANGENOTSATISFIABLE:
      Reason := 'Requested Range Not Satisfiable';
    426:
      Reason := 'Upgrade Required';
    HTTP_SERVERERROR:
      Reason := 'Internal Server Error';
    HTTP_NOTIMPLEMENTED:
      Reason := 'Not Implemented';
    HTTP_BADGATEWAY:
      Reason := 'Bad Gateway';
    HTTP_UNAVAILABLE:
      Reason := 'Service Unavailable';
    HTTP_GATEWAYTIMEOUT:
      Reason := 'Gateway Timeout';
    HTTP_HTTPVERSIONNONSUPPORTED:
      Reason := 'HTTP Version Not Supported';
    511:
      Reason := 'Network Authentication Required';
  else
    Reason := 'Invalid Request';
  end;
end;

function StatusCodeToReason(Code: cardinal): RawUtf8;
begin
  StatusCodeToReason(Code, result);
end;

procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8);
var
  Hi, Lo: cardinal;
begin
  if Code = 200 then
  begin
    Hi := 2; // optimistic approach :)
    Lo := 0;
    Reason := ReasonCache[2, 0];
  end
  else
  begin
    Hi := Code div 100;
    Lo := Code - Hi * 100;
    if not ((Hi in [1..5]) and
            (Lo in [0..13])) then
    begin
      Hi := 5;
      Lo := 13; // returns cached 'Invalid Request'
    end;
    Reason := ReasonCache[Hi, Lo];
  end;
  if Reason <> '' then
    exit;
  StatusCode2Reason(Code, Reason);
  ReasonCache[Hi, Lo] := Reason;
end;

function StatusCodeIsSuccess(Code: integer): boolean;
begin
  result := (Code >= HTTP_SUCCESS) and
            (Code < HTTP_BADREQUEST); // 200..399
end;

function IsInvalidHttpHeader(head: PUtf8Char; headlen: PtrInt): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to headlen - 3 do
    if (PInteger(head + i)^ = $0a0d0a0d) or
       (PWord(head + i)^ = $0d0d) or
       (PWord(head + i)^ = $0a0a) then
      exit;
  result := false;
end;


{ ****************** Gather Operating System Information }

function ToText(const osv: TOperatingSystemVersion): RawUtf8;
begin
  if osv.os = osWindows then
    result := 'Windows ' + WINDOWS_NAME[osv.win]
  else
    result := OS_NAME[osv.os];
end;

function ToTextShort(const osv: TOperatingSystemVersion): RawUtf8;
begin
  if osv.os = osWindows then
    result := WINDOWS_NAME[osv.win]
  else
    result := OS_NAME[osv.os];
end;

function ToTextOS(osint32: integer): RawUtf8;
var
  osv: TOperatingSystemVersion absolute osint32;
begin
  if osint32 = 0 then
  begin
    result := '';
    exit;
  end;
  result := ToText(osv);
  if (osv.os = osWindows) and
     (osv.winbuild <> 0) then
    // include the Windows build number, e.g. 'Windows 11 64-bit 22000'
    result := RawUtf8(Format('%s %d', [result, osv.winbuild]));
  if (osv.os >= osLinux) and
     (osv.utsrelease[2] <> 0) then
    // include the kernel number to the distribution name, e.g. 'Ubuntu 5.4.0'
    result := RawUtf8(Format('%s %d.%d.%d', [result, osv.utsrelease[2],
      osv.utsrelease[1], osv.utsrelease[0]]));
end;

const
  // https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  ARMCPU_ID: array[TArmCpuType] of word = (
    0,      // actUnknown
    $0810,  // actARM810
    $0920,  // actARM920
    $0922,  // actARM922
    $0926,  // actARM926
    $0940,  // actARM940
    $0946,  // actARM946
    $0966,  // actARM966
    $0a20,  // actARM1020
    $0a22,  // actARM1022
    $0a26,  // actARM1026
    $0b02,  // actARM11MPCore
    $0b36,  // actARM1136
    $0b56,  // actARM1156
    $0b76,  // actARM1176
    $0c05,  // actCortexA5
    $0c07,  // actCortexA7
    $0c08,  // actCortexA8
    $0c09,  // actCortexA9
    $0c0d,  // actCortexA12
    $0c0f,  // actCortexA15
    $0c0e,  // actCortexA17
    $0c14,  // actCortexR4
    $0c15,  // actCortexR5
    $0c17,  // actCortexR7
    $0c18,  // actCortexR8
    $0c20,  // actCortexM0
    $0c21,  // actCortexM1
    $0c23,  // actCortexM3
    $0c24,  // actCortexM4
    $0c27,  // actCortexM7
    $0c60,  // actCortexM0P
    $0d01,  // actCortexA32
    $0d03,  // actCortexA53
    $0d04,  // actCortexA35
    $0d05,  // actCortexA55
    $0d06,  // actCortexA65
    $0d07,  // actCortexA57
    $0d08,  // actCortexA72
    $0d09,  // actCortexA73
    $0d0a,  // actCortexA75
    $0d0b,  // actCortexA76
    $0d0c,  // actNeoverseN1
    $0d0d,  // actCortexA77
    $0d0e,  // actCortexA76AE
    $0d13,  // actCortexR52
    $0d20,  // actCortexM23
    $0d21,  // actCortexM33
    $0d41,  // actCortexA78
    $0d42,  // actCortexA78AE
    $0d4a,  // actNeoverseE1
    $0d4b); // actCortexA78C

  ARMCPU_IMPL: array[TArmCpuImplementer] of byte = (
    0,    // aciUnknown
    $41,  // aciARM
    $42,  // aciBroadcom
    $43,  // aciCavium
    $44,  // aciDEC
    $46,  // aciFUJITSU
    $48,  // aciHiSilicon
    $49,  // aciInfineon
    $4d,  // aciMotorola
    $4e,  // aciNVIDIA
    $50,  // aciAPM
    $51,  // aciQualcomm
    $53,  // aciSamsung
    $56,  // aciMarvell
    $61,  // aciApple
    $66,  // aciFaraday
    $69,  // aciIntel
    $c0); // aciAmpere

  ARMCPU_ID_TXT: array[TArmCpuType] of RawUtf8 = (
     '',
     'ARM810', 'ARM920', 'ARM922', 'ARM926', 'ARM940', 'ARM946', 'ARM966',
     'ARM1020', 'ARM1022', 'ARM1026', 'ARM11 MPCore', 'ARM1136', 'ARM1156',
     'ARM1176', 'Cortex-A5', 'Cortex-A7', 'Cortex-A8', 'Cortex-A9',
     'Cortex-A17',{Originally A12} 'Cortex-A15', 'Cortex-A17', 'Cortex-R4',
     'Cortex-R5', 'Cortex-R7', 'Cortex-R8', 'Cortex-M0', 'Cortex-M1',
     'Cortex-M3', 'Cortex-M4', 'Cortex-M7', 'Cortex-M0+', 'Cortex-A32',
     'Cortex-A53', 'Cortex-A35', 'Cortex-A55', 'Cortex-A65', 'Cortex-A57',
     'Cortex-A72', 'Cortex-A73', 'Cortex-A75', 'Cortex-A76', 'Neoverse-N1',
     'Cortex-A77', 'Cortex-A76AE', 'Cortex-R52', 'Cortex-M23', 'Cortex-M33',
     'Cortex-A78', 'Cortex-A78AE', 'Neoverse-E1', 'Cortex-A78C');

  ARMCPU_IMPL_TXT: array[TArmCpuImplementer] of RawUtf8 = (
      '',
      'ARM', 'Broadcom', 'Cavium', 'DEC', 'FUJITSU', 'HiSilicon', 'Infineon',
      'Motorola/Freescale', 'NVIDIA', 'APM', 'Qualcomm', 'Samsung', 'Marvell',
      'Apple', 'Faraday', 'Intel', 'Ampere');

function ArmCpuType(id: word): TArmCpuType;
begin
  for result := low(TArmCpuType) to high(TArmCpuType) do
    if ARMCPU_ID[result] = id then
      exit;
  result := actUnknown;
end;

function ArmCpuTypeName(act: TArmCpuType; id: word): RawUtf8;
begin
  if act = actUnknown then
    result := 'ARM 0x' + RawUtf8(IntToHex(id, 3))
  else
    result := ARMCPU_ID_TXT[act];
end;

function ArmCpuImplementer(id: byte): TArmCpuImplementer;
begin
  for result := low(TArmCpuImplementer) to high(TArmCpuImplementer) do
    if ARMCPU_IMPL[result] = id then
      exit;
  result := aciUnknown;
end;

function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word): RawUtf8;
begin
  if aci = aciUnknown then
    result := 'HW 0x' + RawUtf8(IntToHex(id, 2))
  else
    result := ARMCPU_IMPL_TXT[aci];
end;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt; LeaveUnprotected: boolean);
begin
  PatchCode(Code, @Value, SizeOf(Code^), nil, LeaveUnprotected);
end;

{$ifdef CPUINTEL}
procedure RedirectCode(Func, RedirectFunc: Pointer);
var
  rel: PtrInt;
  NewJump: packed record
    Code: byte;        // $e9 = jmp {relative}
    Distance: integer; // relative jump is 32-bit even on CPU64
  end;
begin
  if (Func = nil) or
     (RedirectFunc = nil) or
     (Func = RedirectFunc) then
    exit; // nothing to redirect to
  NewJump.Code := $e9; // on both i386 and x86_64
  rel := PtrInt(PtrUInt(RedirectFunc) - PtrUInt(Func) - SizeOf(NewJump));
  NewJump.Distance := rel;
  {$ifdef CPU64}
  if NewJump.Distance <> rel then
    exit; // RedirectFunc is too far away from the original code :(
  {$endif CPU64}
  PatchCode(Func, @NewJump, SizeOf(NewJump));
  assert(PByte(Func)^ = $e9);
end;
{$endif CPUINTEL}



{ ****************** Unicode, Time, File, Console, Library process }

procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
begin
  if not IsInitializedCriticalSection(cs) then
    InitializeCriticalSection(cs);
  mormot.core.os.EnterCriticalSection(cs);
end;

procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);
begin
  if IsInitializedCriticalSection(cs) then
    DeleteCriticalSection(cs);
end;

function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;
begin
  result := '';
  if Code = 0 then
    exit;
  {$ifdef OSWINDOWS}
  result := SysErrorMessage(Code, ModuleName);
  if result <> '' then
    exit;
  {$endif OSWINDOWS}
  result := SysErrorMessage(Code);
  if result = '' then
    {$ifdef OSWINDOWS}
    if Code = ERROR_WINHTTP_CANNOT_CONNECT then
      result := 'cannot connect'
    else if Code = ERROR_WINHTTP_TIMEOUT then
      result := 'timeout'
    else if Code = ERROR_WINHTTP_INVALID_SERVER_RESPONSE then
      result := 'invalid server response'
    else
    {$endif OSWINDOWS}
      result := IntToHex(Code, 8);
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var
  LastError: integer;
  Error: Exception;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := ModuleException.CreateFmt('%s error %x (%s)',
      [ModuleName, LastError, SysErrorMessagePerModule(LastError, ModuleName)])
  else
    Error := ModuleException.CreateFmt('Undefined %s error', [ModuleName]);
  raise Error;
end;

function Unicode_CodePage: integer;
begin
{$ifdef FPC}
  // = GetSystemCodePage on POSIX, Lazarus may override to UTF-8 on Windows
  result := DefaultSystemCodePage;
{$else}
  // Delphi always uses the main Windows System Code Page
  result := GetACP;
{$endif FPC}
end;

function Unicode_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt;
  IgnoreCase: boolean): integer;
const
  _CASEFLAG: array[boolean] of DWORD = (0, NORM_IGNORECASE);
begin
  result := CompareStringW(LOCALE_USER_DEFAULT, _CASEFLAG[IgnoreCase], PW1, L1, PW2, L2);
end;

procedure Unicode_WideToShort(W: PWideChar; LW, CodePage: PtrInt;
  var res: ShortString);
var
  i: PtrInt;
begin
  if LW <= 0 then
    res[0] := #0
  else if (LW <= 255) and
          IsAnsiCompatibleW(W, LW) then
  begin
    // fast handling of pure English content
    res[0] := AnsiChar(LW);
    i := 1;
    repeat
      res[i] := AnsiChar(W^);
      if i = LW then
        break;
      inc(W);
      inc(i);
    until false;
  end
  else
    // use ICU or cwstring/RTL for accurate conversion
    res[0] := AnsiChar(
      Unicode_WideToAnsi(W, PAnsiChar(@res[1]), LW, 255, CodePage));
end;

function NowUtc: TDateTime;
begin
  result := UnixMSTimeUtcFast / MSecsPerDay + UnixDelta;
end;

function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;
var
  YY, MM, DD, H, m, s, ms: word;
begin
  DecodeDate(DateTime, YY, MM, DD);
  DecodeTime(DateTime, h, m, s, ms);
  if (YY < 1980) or
     (YY > 2099) then
    result := 0
  else
    result := (s shr 1) or (m shl 5) or (h shl 11) or
      cardinal((DD shl 16) or (MM shl 21) or (cardinal(YY - 1980) shl 25));
end;

function WindowsFileTimeToDateTime(WinTime: integer): TDateTime;
var
  date, time: TDateTime;
begin
  with PLongRec(@WinTime)^ do
  if TryEncodeDate(Hi shr 9 + 1980, Hi shr 5 and 15, Hi and 31, date) and
     TryEncodeTime(Lo shr 11, Lo shr 5 and 63, Lo and 31 shl 1, 0, time) then
    result := date + time
  else
    result := 0;
end;

const
  DateFileTimeDelta =  94353120000000000; // from year 1601 to 1899

function WindowsFileTime64ToDateTime(WinTime: QWord): TDateTime;
begin
  result := (Int64(WinTime) - DateFileTimeDelta) / 10000;
end;

function ValidHandle(Handle: THandle): boolean;
begin
  result := PtrInt(Handle) > 0;
end;

function SafeFileName(const FileName: TFileName): boolean;
begin
  result := (FileName <> '') and
            (FileName[1] <> '/') and
            (PosExString('..', FileName) = 0) and
            (PosExString(':', FileName) = 0) and
            (PosExString('\\', FileName) = 0);
end;

function SafeFileNameU(const FileName: RawUtf8): boolean;
begin
  result := (FileName <> '') and
            (FileName[1] <> '/') and
            (PosEx('..', FileName) = 0) and
            (PosEx(':', FileName) = 0) and
            (PosEx('\\', FileName) = 0);
end;

procedure DisplayError(const fmt: string; const args: array of const);
var
  msg: string;
begin
  msg := Format(fmt, args);
  DisplayFatalError('', RawUtf8(msg));
end;

function SearchRecToDateTime(const F: TSearchRec): TDateTime;
begin
  {$ifdef ISDELPHIXE}
  result := F.Timestamp; // use new API
  {$else}
  result := FileDateToDateTime(F.Time);
  {$endif ISDELPHIXE}
end;

function SearchRecValidFile(const F: TSearchRec): boolean;
begin
  result := (F.Name <> '') and
            (F.Attr and faInvalidFile = 0);
end;

function SearchRecValidFolder(const F: TSearchRec): boolean;
begin
  result := (F.Attr and faDirectoryMask = faDirectory) and
            (F.Name <> '') and
            (F.Name <> '.') and
            (F.Name <> '..');
end;

destructor TFileStreamFromHandle.Destroy;
begin
  if not fDontReleaseHandle then
    FileClose(Handle); // otherwise file is stil opened
end;

function FileStreamSequentialRead(const FileName: string): THandleStream;
begin
  result := TFileStreamFromHandle.Create(FileOpenSequentialRead(FileName));
end;

function StringFromFile(const FileName: TFileName; HasNoSize: boolean): RawByteString;
var
  F: THandle;
  Read, Size, Chunk: integer;
  P: PUtf8Char;
  tmp: array[0..$7fff] of AnsiChar; // 32KB stack buffer
begin
  result := '';
  if FileName = '' then
    exit;
  F := FileOpenSequentialRead(FileName);
  if ValidHandle(F) then
  begin
    if HasNoSize then
    begin
      Size := 0;
      repeat
        Read := FileRead(F, tmp, SizeOf(tmp));
        if Read <= 0 then
          break;
        SetLength(result, Size + Read); // in-place resize
        MoveFast(tmp, PByteArray(result)^[Size], Read);
        inc(Size, Read);
      until false;
    end
    else
    begin
      Size := FileSize(F);
      if Size > 0 then
      begin
        SetLength(result, Size);
        P := pointer(result);
        repeat
          Chunk := Size;
          Read := FileRead(F, P^, Chunk);
          if Read <= 0 then
          begin
            result := '';
            break;
          end;
          inc(P, Read);
          dec(Size, Read);
        until Size = 0;
      end;
    end;
    FileClose(F);
  end;
end;

function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean; FileDate: TDateTime): boolean;
var
  F: THandle;
  P: PByte;
  L, written: integer;
begin
  result := false;
  if FileName = '' then
    exit;
  F := FileCreate(FileName);
  if PtrInt(F) < 0 then
    exit;
  L := length(Content);
  P := pointer(Content);
  while L > 0 do
  begin
    written := FileWrite(F, P^, L);
    if written < 0 then
    begin
      FileClose(F);
      exit;
    end;
    dec(L, written);
    inc(P, written);
  end;
  if FlushOnDisk then
    FlushFileBuffers(F);
  {$ifdef OSWINDOWS}
  if FileDate <> 0 then
    FileSetDate(F, DateTimeToFileDate(FileDate));
  FileClose(F);
  {$else}
  FileClose(F); // POSIX expects the file to be closed to set the date
  if FileDate <> 0 then
    FileSetDate(FileName, DateTimeToFileDate(FileDate));
  {$endif OSWINDOWS}
  result := true;
end;

var
  _TmpCounter: integer;

function TemporaryFileName: TFileName;
var
  folder: TFileName;
  retry: integer;
begin
  // fast cross-platform implementation
  folder := GetSystemPath(spTempFolder);
  if _TmpCounter = 0 then
    _TmpCounter := Random32;
  retry := 10;
  repeat
    // thread-safe unique file name generation
    result := Format('%s%s_%x.tmp',
      [folder, Executable.ProgramName, InterlockedIncrement(_TmpCounter)]);
    if not FileExists(result) then
      exit;
    dec(retry); // no endless loop
  until retry = 0;
  raise EOSException.Create('TemporaryFileName failed');
end;

function GetFileNameWithoutExt(const FileName: TFileName; Extension: PFileName): TFileName;
var
  i, max: PtrInt;
begin
  i := length(FileName);
  max := i - 16;
  while (i > 0) and
        not (cardinal(FileName[i]) in [ord('\'), ord('/'), ord('.')]) and
        (i >= max) do
    dec(i);
  if (i = 0) or
     (FileName[i] <> '.') then
  begin
    result := FileName;
    if Extension <> nil then
      Extension^ := '';
  end
  else
  begin
    result := copy(FileName, 1, i - 1);
    if Extension <> nil then
      Extension^ := copy(FileName, i, 20);
  end;
end;

function SortDynArrayFileName(const A, B): integer;
var
  Aname, Aext, Bname, Bext: TFileName;
begin
  // code below is not very fast, but correct ;)
  Aname := GetFileNameWithoutExt(string(A), @Aext);
  Bname := GetFileNameWithoutExt(string(B), @Bext);
  result := AnsiCompareFileName(Aext, Bext);
  if result = 0 then
    // if both extensions matches, compare by filename
    result := AnsiCompareFileName(Aname, Bname);
end;

{$ifdef ISDELPHI20062007} // circumvent Delphi 2007 RTL inlining issue
function AnsiCompareFileName(const S1, S2 : TFileName): integer;
begin
  result := SysUtils.AnsiCompareFileName(S1,S2);
end;
{$endif ISDELPHI20062007}

function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: boolean): TFileName;
begin
  result := IncludeTrailingPathDelimiter(ExpandFileName(Directory));
  if not DirectoryExists(result) then
    if not ForceDirectories(result) then
      if not RaiseExceptionOnCreationFailure then
        result := ''
      else
        raise Exception.CreateFmt('Impossible to create folder %s', [result]);
end;

function DirectoryDelete(const Directory: TFileName; const Mask: TFileName;
  DeleteOnlyFilesNotDirectory: boolean; DeletedCount: PInteger): boolean;
var
  F: TSearchRec;
  Dir: TFileName;
  n: integer;
begin
  n := 0;
  result := true;
  if DirectoryExists(Directory) then
  begin
    Dir := IncludeTrailingPathDelimiter(Directory);
    if FindFirst(Dir + Mask, faAnyFile - faDirectory, F) = 0 then
    begin
      repeat
        if SearchRecValidFile(F) then
          if DeleteFile(Dir + F.Name) then
            inc(n)
          else
            result := false;
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    if not DeleteOnlyFilesNotDirectory and
       not RemoveDir(Dir) then
      result := false;
  end;
  if DeletedCount <> nil then
    DeletedCount^ := n;
end;

function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName; Recursive: boolean;
  TotalSize: PInt64): boolean;
var
  F: TSearchRec;
  Dir: TFileName;
  old: TDateTime;
begin
  if not Recursive and
     (TotalSize <> nil) then
    TotalSize^ := 0;
  result := true;
  if (Directory = '') or
     not DirectoryExists(Directory) then
    exit;
  Dir := IncludeTrailingPathDelimiter(Directory);
  if FindFirst(Dir + Mask, faAnyFile, F) = 0 then
  begin
    old := Now - TimePeriod;
    repeat
      if SearchRecValidFolder(F) then
      begin
        if Recursive then
          DirectoryDeleteOlderFiles(
            Dir + F.Name, TimePeriod, Mask, true, TotalSize);
      end
      else if SearchRecValidFile(F) and
              (SearchRecToDateTime(F) < old) then
        if not DeleteFile(Dir + F.Name) then
          result := false
        else if TotalSize <> nil then
          inc(TotalSize^, F.Size);
    until FindNext(F) <> 0;
    FindClose(F);
  end;
end;

function IsDirectoryWritable(const Directory: TFileName): boolean;
var
  dir, fn: TFileName;
  f: THandle;
  retry: integer;
begin
  dir := ExcludeTrailingPathDelimiter(Directory);
  result := false;
  if FileIsReadOnly(dir) then
    exit;
  retry := 20;
  repeat
    fn := Format('%s' + PathDelim + '%x.test', [dir, Random32]);
    if not FileExists(fn) then
      break;
    dec(retry); // never loop forever
    if retry = 0 then
      exit;
  until false;
  f := FileCreate(fn);
  if PtrInt(f) < 0 then
    exit;
  FileClose(f);
  result := DeleteFile(fn);
end;

{$ifndef NOEXCEPTIONINTERCEPT}

{$ifdef WITH_RAISEPROC} // for FPC on Win32 + Linux (Win64=WITH_VECTOREXCEPT)
var
  OldRaiseProc: TExceptProc;

procedure SynRaiseProc(Obj: TObject; Addr: CodePointer;
  FrameCount: integer; Frame: PCodePointer);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWORD;
  backuphandler: TOnRawLogException;
begin
  if Assigned(_RawLogException) then
    if (Obj <> nil) and
       Obj.InheritsFrom(Exception) then
    begin
      backuplasterror := GetLastError;
      backuphandler := _RawLogException;
      try
        _RawLogException := nil; // disable nested exception
        ctxt.EClass := PPointer(Obj)^;
        ctxt.EInstance := Exception(Obj);
        ctxt.EAddr := PtrUInt(Addr);
        if Obj.InheritsFrom(EExternal) then
          ctxt.ELevel := sllExceptionOS
        else
          ctxt.ELevel := sllException;
        ctxt.ETimestamp := UnixTimeUtc;
        ctxt.EStack := pointer(Frame);
        ctxt.EStackCount := FrameCount;
        backuphandler(ctxt);
      except
        { ignore any nested exception }
      end;
      _RawLogException := backuphandler;
      SetLastError(backuplasterror); // may have changed above
    end;
  if Assigned(OldRaiseProc) then
    OldRaiseProc(Obj, Addr, FrameCount, Frame);
end;

{$endif WITH_RAISEPROC}

procedure RawExceptionIntercept(const Handler: TOnRawLogException);
begin
  _RawLogException := Handler;
  if not Assigned(Handler) then
    exit;
  {$ifdef WITH_RAISEPROC} // FPC RTL redirection function
  if @RaiseProc <> @SynRaiseProc then
  begin
    OldRaiseProc := RaiseProc;
    RaiseProc := @SynRaiseProc; // register once
  end;
  {$endif WITH_RAISEPROC}
  {$ifdef WITH_VECTOREXCEPT} // SEH32/SEH64 official API
  // RemoveVectoredContinueHandler() is available under 64 bit editions only
  if Assigned(AddVectoredExceptionHandler) then
  begin
    AddVectoredExceptionHandler(0, @SynLogVectoredHandler);
    AddVectoredExceptionHandler := nil; // register once
  end;
  {$endif WITH_VECTOREXCEPT}
  {$ifdef WITH_RTLUNWINDPROC} // Delphi x86 RTL redirection function
  if @RTLUnwindProc <> @SynRtlUnwind then
  begin
    oldUnWindProc := RTLUnwindProc;
    RTLUnwindProc := @SynRtlUnwind;
  end;
  {$endif WITH_RTLUNWINDPROC}
end;

{$endif NOEXCEPTIONINTERCEPT}


{ TMemoryMap }

function TMemoryMap.Map(aFile: THandle; aCustomSize: PtrUInt;
  aCustomOffset: Int64; aFileOwned: boolean): boolean;
var
  Available: Int64;
begin
  fBuf := nil;
  fBufSize := 0;
  {$ifdef OSWINDOWS}
  fMap := 0;
  {$endif OSWINDOWS}
  fFileLocal := aFileOwned;
  fFile := aFile;
  fFileSize := mormot.core.os.FileSize(fFile);
  if fFileSize = 0 then
  begin
    result := true; // handle 0 byte file without error (but no memory map)
    exit;
  end;
  result := false;
  if (fFileSize <= 0)
     {$ifdef CPU32} or (fFileSize > maxInt){$endif} then
    // maxInt = $7FFFFFFF = 1.999 GB (2GB would induce PtrInt errors on CPU32)
    exit;
  if aCustomSize = 0 then
    fBufSize := fFileSize
  else
  begin
    Available := fFileSize - aCustomOffset;
    if Available < 0 then
      exit;
    if aCustomSize > Available then
      fBufSize := Available;
    fBufSize := aCustomSize;
  end;
  fLoadedNotMapped := fBufSize < 1 shl 20;
  if fLoadedNotMapped then
  begin
    // mapping is not worth it for size < 1MB which can be just read at once
    GetMem(fBuf, fBufSize);
    FileSeek64(fFile, aCustomOffset, soFromBeginning);
    result := PtrUInt(FileRead(fFile, fBuf^, fBufSize)) = fBufSize;
    if not result then
    begin
      Freemem(fBuf);
      fBuf := nil;
      fLoadedNotMapped := false;
    end;
  end
  else
    // call actual Windows/POSIX memory mapping API
    result := DoMap(aCustomOffset);
end;

procedure TMemoryMap.Map(aBuffer: pointer; aBufferSize: PtrUInt);
begin
  fBuf := aBuffer;
  fFileSize := aBufferSize;
  fBufSize := aBufferSize;
  {$ifdef OSWINDOWS}
  fMap := 0;
  {$endif OSWINDOWS}
  fFile := 0;
  fFileLocal := false;
end;

function TMemoryMap.Map(const aFileName: TFileName): boolean;
var
  F: THandle;
begin
  result := false;
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless here
  F := FileOpen(aFileName, fmOpenRead or fmShareDenyNone);
  if not ValidHandle(F) then
    exit;
  if Map(F) then
    result := true
  else
    FileClose(F);
  fFileLocal := result;
end;

procedure TMemoryMap.UnMap;
begin
  if fLoadedNotMapped then
    // mapping was not worth it
    Freemem(fBuf)
  else
    // call actual Windows/POSIX map API
    DoUnMap;
  fBuf := nil;
  fBufSize := 0;
  if fFile <> 0 then
  begin
    if fFileLocal then
      FileClose(fFile);
    fFile := 0;
  end;
end;

function TMemoryMap.TextFileKind: TTextFileKind;
begin
  result := isAnsi;
  if (fBuf <> nil) and
     (fBufSize >= 3) then
    if PWord(fBuf)^ = $FEFF then
      result := isUnicode
    else if (PWord(fBuf)^ = $BBEF) and
            (PByteArray(fBuf)[2] = $BF) then
      result := isUtf8;
end;


{ TSynMemoryStreamMapped }

constructor TSynMemoryStreamMapped.Create(const aFileName: TFileName;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  fFileName := aFileName;
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless here
  fFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  Create(fFileStream.Handle, aCustomSize, aCustomOffset);
end;

constructor TSynMemoryStreamMapped.Create(aFile: THandle;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  if not fMap.Map(aFile, aCustomSize, aCustomOffset) then
    raise EOSException.CreateFmt('%s.Create(%s) mapping error',
      [ClassNameShort(self)^, fFileName]);
  inherited Create(fMap.fBuf, fMap.fBufSize);
end;

destructor TSynMemoryStreamMapped.Destroy;
begin
  fMap.UnMap;
  fFileStream.Free;
  inherited;
end;


{ TExecutableResource }

function TExecutableResource.Open(const ResourceName: string; ResType: PChar;
  Instance: THandle): boolean;
begin
  result := false;
  if Instance = 0 then
    Instance := HInstance;
  HResInfo := FindResource(Instance, PChar(ResourceName), ResType);
  if HResInfo = 0 then
    exit;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then // direct decompression from memory mapped .exe content
    exit;
  Buffer := LockResource(HGlobal);
  Size := SizeofResource(Instance, HResInfo);
  result := true;
end;

procedure TExecutableResource.Close;
begin
  if HGlobal <> 0 then
  begin
    UnlockResource(HGlobal); // only needed outside of Windows
    FreeResource(HGlobal);
    HGlobal := 0;
  end;
end;


{ ReserveExecutableMemory() / TFakeStubBuffer }

type
  // internal memory buffer created with PAGE_EXECUTE_READWRITE flags
  TFakeStubBuffer = class
  protected
    fStub: PByteArray;
    fStubUsed: cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  CurrentFakeStubBuffer: TFakeStubBuffer;
  CurrentFakeStubBuffers: array of TFakeStubBuffer;
  {$ifdef UNIX}
  MemoryProtection: boolean = false; // set to true if PROT_EXEC seems to fail
  {$endif UNIX}

constructor TFakeStubBuffer.Create;
begin
  {$ifdef OSWINDOWS}
  fStub := VirtualAlloc(nil, STUB_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if fStub = nil then
  {$else OSWINDOWS}
  if not MemoryProtection then
    fStub := StubCallAllocMem(STUB_SIZE, PROT_READ or PROT_WRITE or PROT_EXEC);
  if (fStub = MAP_FAILED) or
     MemoryProtection then
  begin
    // i.e. on OpenBSD, we can have w^x protection
    fStub := StubCallAllocMem(STUB_SIZE, PROT_READ OR PROT_WRITE);
    if fStub <> MAP_FAILED then
      MemoryProtection := True;
  end;
  if fStub = MAP_FAILED then
  {$endif OSWINDOWS}
    raise EOSException.Create('ReserveExecutableMemory(): OS memory allocation failed');
end;

destructor TFakeStubBuffer.Destroy;
begin
  {$ifdef OSWINDOWS}
  VirtualFree(fStub, 0, MEM_RELEASE);
  {$else}
  fpmunmap(fStub, STUB_SIZE);
  {$endif OSWINDOWS}
  inherited;
end;

function ReserveExecutableMemory(size: cardinal): pointer;
begin
  if size > STUB_SIZE then
    raise EOSException.CreateFmt('ReserveExecutableMemory(size=%d>%d)',
      [size, STUB_SIZE]);
  GlobalLock;
  try
    if (CurrentFakeStubBuffer <> nil) and
       (CurrentFakeStubBuffer.fStubUsed + size > STUB_SIZE) then
      CurrentFakeStubBuffer := nil;
    if CurrentFakeStubBuffer = nil then
    begin
      CurrentFakeStubBuffer := TFakeStubBuffer.Create;
      ObjArrayAdd(CurrentFakeStubBuffers, CurrentFakeStubBuffer);
    end;
    with CurrentFakeStubBuffer do
    begin
      result := @fStub[fStubUsed];
      inc(fStubUsed, size);
    end;
  finally
    GlobalUnLock;
  end;
end;

{$ifdef UNIX}
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);
var
  PageAlignedFakeStub: pointer;
  flags: cardinal;
begin
  if not MemoryProtection then
    // nothing to be done on this platform
    exit;
  // toggle execution permission of memory to be able to write into memory
  PageAlignedFakeStub := Pointer(
    (PtrUInt(Reserved) div SystemInfo.dwPageSize) * SystemInfo.dwPageSize);
  if Exec then
    flags := PROT_READ OR PROT_EXEC
  else
    flags := PROT_READ or PROT_WRITE;
  if SynMProtect(PageAlignedFakeStub, SystemInfo.dwPageSize shl 1, flags) < 0 then
     raise EOSException.Create('ReserveExecutableMemoryPageAccess(: SynMProtect write failure');
end;
{$else}
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);
begin
  // nothing to be done
end;
{$endif UNIX}

{$ifndef PUREMORMOT2}

function GetDelphiCompilerVersion: RawUtf8;
begin
  result := COMPILER_VERSION;
end;

{$endif PUREMORMOT2}

function ConsoleReadBody: RawByteString;
var
  len, n: integer;
  P: PByte;
begin
  result := '';
  len := ConsoleStdInputLen;
  SetLength(result, len);
  P := pointer(result);
  while len > 0 do
  begin
    n := FileRead(StdInputHandle, P^, len);
    if n <= 0 then
    begin
      result := ''; // read error
      break;
    end;
    dec(len, n);
    inc(P, n);
  end;
end;

{$I-}
procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor;
  NoLineFeed, NoColor: boolean);
begin
  {$ifdef OSWINDOWS}
  if not HasConsole then
    exit;
  {$endif OSWINDOWS}
  if not NoColor then
    TextColor(Color); 
  write(Utf8ToConsole(Text));
  if not NoLineFeed then
    writeln;
  if not NoColor then
    TextColor(ccLightGray);
  ioresult;
end;
{$I+}


{ TSynLibrary }

function TSynLibrary.Resolve(ProcName: PAnsiChar; Entry: PPointer;
  RaiseExceptionOnFailure: ExceptionClass): boolean;
{$ifdef OSPOSIX}
var
  dlinfo: dl_info;
{$endif OSPOSIX}
begin
  if (Entry = nil) or
     (fHandle = 0) or
     (ProcName = nil) then
    result := false // avoid GPF
  else
  begin
    Entry^ := LibraryResolve(fHandle, ProcName);
    result := Entry^ <> nil;
    {$ifdef OSPOSIX}
    if result and
       not fLibraryPathTested then
    begin
      fLibraryPathTested := true;
      FillCharFast(dlinfo, SizeOf(dlinfo), 0);
      dladdr(Entry^, @dlinfo);
      if dlinfo.dli_fname <> nil then
        fLibraryPath := dlinfo.dli_fname;
    end;
    {$endif OSPOSIX}
  end;
  if (RaiseExceptionOnFailure <> nil) and
     not result then
  begin
    FreeLib;
    raise RaiseExceptionOnFailure.CreateFmt('%s.Resolve(''%s''): not found in %s',
      [ClassNameShort(self)^, ProcName, LibraryPath]);
  end;
end;

procedure TSynLibrary.FreeLib;
begin
  if fHandle = 0 then
    exit; // nothing to free
  LibraryClose(fHandle);
  fHandle := 0;
end;

function TSynLibrary.TryLoadLibrary(const aLibrary: array of TFileName;
  aRaiseExceptionOnFailure: ExceptionClass): boolean;
var
  i: integer;
  lib, libs: TFileName;
  {$ifdef OSWINDOWS}
  nwd, cwd: TFileName;
  {$endif OSWINDOWS}
begin
  for i := 0 to high(aLibrary) do
  begin
    lib := aLibrary[i];
    if lib = '' then
      continue;
    {$ifdef OSWINDOWS}
    nwd := ExtractFilePath(lib);
    if nwd <> '' then
    begin
      cwd := GetCurrentDir;
      SetCurrentDir(nwd); // change the current folder at loading on Windows
    end;
    fHandle := SafeLoadLibrary(lib);
    if nwd <> '' then
      SetCurrentDir(cwd{%H-});
    {$else}
    fHandle := LibraryOpen(lib); // use regular .so loading behavior
    {$endif OSWINDOWS}
    if fHandle <> 0 then
    begin
      {$ifdef OSWINDOWS} // on POSIX, will call dladdr() in Resolve()
      fLibraryPath := GetModuleName(fHandle);
      if length(fLibraryPath) < length(lib) then
      {$endif OSWINDOWS}
        fLibraryPath := lib;
      result := true;
      exit;
    end;
    if {%H-}libs = '' then
      libs := lib
    else
      libs := libs + ', ' + lib;
  end;
  result := false;
  if aRaiseExceptionOnFailure <> nil then
    raise aRaiseExceptionOnFailure.CreateFmt('%s.TryLoadLibray failed' +
      ' - searched in %s', [ClassNameShort(self)^, libs]);
end;

destructor TSynLibrary.Destroy;
begin
  FreeLib;
  inherited Destroy;
end;


{ TFileVersion }

function TFileVersion.Version32: integer;
begin
  result := Major shl 16 + Minor shl 8 + Release;
end;

procedure TFileVersion.SetVersion(aMajor, aMinor, aRelease, aBuild: integer);
begin
  Major := aMajor;
  Minor := aMinor;
  Release := aRelease;
  Build := aBuild;
  Main := Format('%d.%d', [Major, Minor]);
  fDetailed := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
  fVersionInfo :=  '';
  fUserAgent := '';
end;

function TFileVersion.BuildDateTimeString: string;
begin
  result := DateTimeToIsoString(fBuildDateTime);
end;

function TFileVersion.DetailedOrVoid: string;
begin
  if (self = nil) or
     (Major or Minor or Release or Build = 0) then
    result := ''
  else
    result := fDetailed;
end;

function TFileVersion.VersionInfo: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fVersionInfo = '' then
      fVersionInfo := RawUtf8(Format('%s %s (%s)', [ExtractFileName(fFileName),
        DetailedOrVoid, BuildDateTimeString]));
    result := fVersionInfo;
  end;
end;

function TFileVersion.UserAgent: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fUserAgent = '' then
    begin
      fUserAgent := RawUtf8(Format('%s/%s%s', [GetFileNameWithoutExt(
        ExtractFileName(fFileName)), DetailedOrVoid, OS_INITIAL[OS_KIND]]));
      {$ifdef OSWINDOWS}
      if OSVersion in WINDOWS_32 then
        fUserAgent := fUserAgent + '32';
      {$endif OSWINDOWS}
    end;
    result := fUserAgent;
  end;
end;

class function TFileVersion.GetVersionInfo(const aFileName: TFileName): RawUtf8;
begin
  with Create(aFileName, 0, 0, 0, 0) do
  try
    result := VersionInfo;
  finally
    Free;
  end;
end;

function UserAgentParse(const UserAgent: RawUtf8;
  out ProgramName, ProgramVersion: RawUtf8;
  out OS: TOperatingSystem): boolean;
var
  i, v, vlen, o: PtrInt;
begin
  result := false;
  ProgramName := Split(UserAgent, '/');
  v := length(ProgramName);
  if (v = 0) or
     (UserAgent[v + 1] <> '/') then
    exit;
  inc(v, 2);
  vlen := 0;
  o := -1;
  for i := v to length(UserAgent) do
    if not (UserAgent[i] in ['0'..'9', '.']) then
    begin
      vlen := i - v; // vlen may be 0 if DetailedOrVoid was ''
      if UserAgent[i + 1] in [#0, '3'] then // end with OS_INITIAL or '32' suffix
        o := ByteScanIndex(pointer(@OS_INITIAL),
          ord(high(TOperatingSystem)) + 1, ord(UserAgent[i]));
      break;
    end;
  if o < 0 then
    exit; // should end with OS_INITIAL[OS_KIND]]
  os := TOperatingSystem(o);
  ProgramVersion := copy(UserAgent, v, vlen);
  result := true;
end;

procedure SetExecutableVersion(const aVersionText: RawUtf8);
var
  P: PAnsiChar;
  i: integer;
  ver: array[0..3] of integer;
begin
  P := pointer(aVersionText);
  for i := 0 to 3 do
    ver[i] := GetNextCardinal(P);
  SetExecutableVersion(ver[0], ver[1], ver[2], ver[3]);
end;

procedure SetExecutableVersion(aMajor, aMinor, aRelease, aBuild: integer);
begin
  with Executable do
  begin
    if Version = nil then
    begin
      {$ifdef OSWINDOWS}
      ProgramFileName := paramstr(0);
      {$else}
      ProgramFileName := GetModuleName(HInstance);
      if ProgramFileName = '' then
        ProgramFileName := ExpandFileName(paramstr(0));
      {$endif OSWINDOWS}
      ProgramFilePath := ExtractFilePath(ProgramFileName);
      if IsLibrary then
        InstanceFileName := GetModuleName(HInstance)
      else
        InstanceFileName := ProgramFileName;
      ProgramName := RawUtf8(GetFileNameWithoutExt(ExtractFileName(ProgramFileName)));
      GetUserHost(User, Host);
      if Host = '' then
        Host := 'unknown';
      if User = '' then
        User := 'unknown';
      Version := TFileVersion.Create(
        InstanceFileName, aMajor, aMinor, aRelease, aBuild);
    end
    else
      Version.SetVersion(aMajor, aMinor, aRelease, aBuild);
    ProgramFullSpec := RawUtf8(Format('%s %s (%s)', [ProgramFileName,
      Version.Detailed, Version.BuildDateTimeString]));
    Hash.c0 := Version.Version32;
    {$ifdef OSLINUXANDROID}
    Hash.c0 := crc32c(Hash.c0, pointer(CpuInfoFeatures), length(CpuInfoFeatures));
    {$else}
    {$ifdef CPUINTELARM}
    Hash.c0 := crc32c(Hash.c0, @CpuFeatures, SizeOf(CpuFeatures));
    {$else}
    Hash.c0 := crc32c(Hash.c0, pointer(CpuInfoText), length(CpuInfoText));
    {$endif OSLINUXANDROID}
    {$endif CPUINTELARM}
    Hash.c0 := crc32c(Hash.c0, pointer(Host), length(Host));
    Hash.c1 := crc32c(Hash.c0, pointer(User), length(User));
    Hash.c2 := crc32c(Hash.c1, pointer(ProgramFullSpec), length(ProgramFullSpec));
    Hash.c3 := crc32c(Hash.c2, pointer(InstanceFileName), length(InstanceFileName));
  end;
end;

const
  // hexstr() is not available on Delphi -> use our own simple version
  HexCharsLower: array[0..15] of AnsiChar = '0123456789abcdef';

function _GetExecutableLocation(aAddress: pointer): ShortString;
var
  i: PtrInt;
  b: PByte;
begin // just return the address as hexadecimal
  result[0] := AnsiChar(SizeOf(aAddress) * 2);
  b := @aAddress;
  for i := SizeOf(aAddress) - 1 downto 0 do
  begin
    result[i * 2 + 1] := HexCharsLower[b^ shr 4];
    result[i * 2 + 2] := HexCharsLower[b^ and $F];
    inc(b);
  end;
end; // mormot.core.log.pas will properly decode debug info - and handle .mab


{ **************** TSynLocker Threading Features }

var
  GlobalCriticalSection: TRTLCriticalSection;

procedure GlobalLock;
begin
  mormot.core.os.EnterCriticalSection(GlobalCriticalSection);
end;

procedure GlobalUnLock;
begin
  mormot.core.os.LeaveCriticalSection(GlobalCriticalSection);
end;

var
  InternalGarbageCollection: record
    Instances:  TObjectDynArray;
    Count: integer;
    Shutdown: boolean; // paranoid check to avoid messing with Instances[]
  end;

function RegisterGlobalShutdownRelease(Instance: TObject;
  SearchExisting: boolean): pointer;
begin
  if not InternalGarbageCollection.Shutdown then
  begin
    GlobalLock;
    try
      with InternalGarbageCollection do
        if not SearchExisting or
           (ObjArrayFind(Instances, Count, Instance) < 0) then
          ObjArrayAddCount(Instances, Instance, Count);
    finally
      GlobalUnLock;
    end;
  end;
  result := Instance;
end;

function SleepDelay(elapsed: PtrInt): PtrInt;
begin
  if elapsed < 50 then
    result := 0 // 10us on POSIX, SwitchToThread on Windows
  else if elapsed < 200 then
    result := 1
  else if elapsed < 500 then
    result := 5
  else if elapsed < 2000 then
    result := 50
  else
    result := 120 + Random32(130); // random 120-250 ms
end;

function SleepStepTime(var start, tix: Int64; endtix: PInt64): PtrInt;
begin
  tix := GetTickCount64;
  if start = 0 then
    start := tix;
  result := SleepDelay(tix - start);
  if endtix <> nil then
    endtix^ := tix + result;
end;

function SleepStep(var start: Int64; terminated: PBoolean; event: TEvent): Int64;
var
  ms: integer;
  endtix: Int64;
begin
  ms := SleepStepTime(start, result, @endtix);
  if (ms < 10) or
     (terminated = nil) then
    if (ms = 0) or
       (event = nil) then
      SleepHiRes(ms) // < 16 ms is a pious wish on Windows anyway
    else
    begin
      if event.WaitFor(ms) = wrSignaled then
        start := 0; // make more reactive once signaled
    end
  else
    repeat
      if event = nil then
        SleepHiRes(10) // on Windows, HW clock resolution is around 16 ms
      else if event.WaitFor(10) = wrSignaled then
      begin
        start := 0; // more reactive
        ms := 0; // and quit
      end;
      result := GetTickCount64;
    until (ms = 0) or
          terminated^ or
          (result >= endtix);
end;

function SleepHiRes(ms: cardinal; var terminated: boolean;
  terminatedvalue: boolean): boolean;
var
  start, endtix: Int64;
begin
  if terminated <> terminatedvalue then
    if ms < 20 then
      SleepHiRes(ms) // below HW clock resolution
    else
    begin
      start := GetTickCount64;
      endtix := start + ms;
      repeat
      until (terminated = terminatedvalue) or
            (SleepStep(start, @terminated) > endtix);
    end;
  result := terminated = terminatedvalue;
end;

{$ifdef CPUINTEL}
procedure DoPause; {$ifdef FPC} assembler; nostackframe; {$endif}
asm
      pause
end;
{$endif CPUINTEL}

const
  {$ifdef CPUINTEL}
  SPIN_COUNT = 1000;
  {$else}
  SPIN_COUNT = 100; // since DoPause does nothing, switch to thread sooner
  {$endif CPUINTEL}

function DoSpin(spin: PtrUInt): PtrUInt;
  {$ifdef HASINLINE} inline; {$endif}
begin
  {$ifdef CPUINTEL}
  DoPause;
  {$endif CPUINTEL}
  dec(spin);
  if spin = 0 then
  begin
    SwitchToThread;
    spin := SPIN_COUNT;
  end;
  result := spin;
end;

procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  while not LockedExc(Target, NewValue, Comperand) do
    spin := DoSpin(spin);
end;


procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUtf8;
  const Args: array of const);
begin
  // do nothing - properly implemented in mormot.core.log
end;

procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const);
begin
  SetThreadName(GetCurrentThreadId, Format, Args);
end;

procedure SetCurrentThreadName(const Name: RawUtf8);
begin
  SetThreadName(GetCurrentThreadId, '%', [Name]);
end;

function GetCurrentThreadName: RawUtf8;
begin
  ShortStringToAnsi7String(CurrentThreadName, result);
end;

function GetCurrentThreadInfo: ShortString;
begin
  result := ShortString(format('Thread %x [%s]',
    [PtrUInt(GetCurrentThreadId), CurrentThreadName]));
end;


{ TLightLock }

procedure TLightLock.Init;
begin
  Flags := 0;
end;

procedure TLightLock.LockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until LockedExc(Flags, 1, 0);
end;

procedure TLightLock.Lock;
begin
  // we tried a dedicated asm but it was slower: inlining is preferred
  if not LockedExc(Flags, 1, 0) then
    LockSpin;
end;

function TLightLock.TryLock: boolean;
begin
  result := LockedExc(Flags, 1, 0);
end;

procedure TLightLock.UnLock;
begin
  Flags := 0;
end;



{ TRWLock }

procedure TRWLock.Init;
begin
  // bit 0 = WriteLock, 1 = ReadWriteLock, 2.. = ReadOnlyLock counter
  Flags := 0;
  // no need to set the other fields because they will be reset if Flags=0
end;

procedure TRWLock.AssertDone;
begin
  if Flags <> 0 then
    raise EOSException.CreateFmt('TRWLock Flags=%x', [Flags]);
end;

// dedicated asm for this most simple (and used) method
{$ifdef ASMX64}

procedure TRWLock.ReadOnlyLock;
asm     // since we may call SwitchToThread we need to have a stackframe
        {$ifndef WIN64ABI}
        mov     rcx, rdi      // rcx = self
        {$endif WIN64ABI}
@retry: mov     r8d, SPIN_COUNT
@spin:  mov     rax, qword ptr [rcx + TRWLock.Flags]
        and     rax, not 1
        lea     rdx, [rax + 4]
   lock cmpxchg qword ptr [rcx + TRWLock.Flags], rdx
        jz      @done
        pause
        dec     r8d
        jnz     @spin
        push    rcx
        call    SwitchToThread
        pop     rcx
        jmp     @retry
@done:  // restore the stack frame
end;

{$else}

{$ifdef ASMX86}

procedure TRWLock.ReadOnlyLock;
  {$ifdef FPCWINDOWS} nostackframe; assembler; {$endif}
asm     // since we may call SwitchToThread we need to have a stackframe
        push    ebx
        mov     ebx, eax
@retry: mov     ecx, SPIN_COUNT
@spin:  mov     eax, dword ptr [ebx + TRWLock.Flags]
        and     eax, not 1
        lea     edx, [eax + 4]
   lock cmpxchg dword ptr [ebx + TRWLock.Flags], edx
        jz      @done
        pause
        dec     ecx
        jnz     @spin
        call    SwitchToThread
        jmp     @retry
@done:  pop     ebx
end;    // restore the stack frame on systems which expects it

{$else}

procedure TRWLock.ReadOnlyLock;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
  if not LockedExc(Flags, f + 4, f) then
    ReadOnlyLockSpin;
end;

procedure TRWLock.ReadOnlyLockSpin;
var
  spin, f: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
    f := Flags and not 1; // retry ReadOnlyLock
  until LockedExc(Flags, f + 4, f);
end;

{$endif ASMX86}
{$endif ASMX64}

procedure TRWLock.ReadOnlyUnLock;
begin
  LockedDec(Flags, 4);
end;

procedure TRWLock.ReadWriteLock;
var
  spin, f: PtrUInt;
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  if (Flags and 2 = 2) and
     (LastReadWriteLockThread = tid) then
  begin
    inc(LastReadWriteLockCount); // allow ReadWriteLock to be reentrant
    exit;
  end;
  // if not writing, atomically acquire the upgradable RD flag bit
  spin := SPIN_COUNT;
  repeat
    f := Flags and not 3; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
    if LockedExc(Flags, f + 2, f) then
      break;
    spin := DoSpin(spin);
  until false;
  LastReadWriteLockThread := tid;
  LastReadWriteLockCount := 0;
end;

procedure TRWLock.ReadWriteUnLock;
begin
  if LastReadWriteLockCount <> 0 then
  begin
    dec(LastReadWriteLockCount);
    exit;
  end;
  LastReadWriteLockThread := TThreadID(0);
  LockedDec(Flags, 2);
end;

procedure TRWLock.WriteLock;
var
  spin, f: PtrUInt;
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  if (Flags and 1 = 1) and
     (LastWriteLockThread = tid) then
  begin
    inc(LastWriteLockCount); // allow WriteLock to be reentrant
    exit;
  end;
  spin := SPIN_COUNT;
  // acquire the WR flag bit
  repeat
    f := Flags and not 1; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
    if LockedExc(Flags, f + 1, f) then
      if (Flags and 2 = 2) and
         (LastReadWriteLockThread <> tid) then
        // there is a pending ReadWriteLock but not on this thread
        LockedDec(Flags, 1) // try again
      else
        // we exclusively acquired the WR lock
        break;
    spin := DoSpin(spin);
  until false;
  LastWriteLockThread := tid;
  LastWriteLockCount := 0;
  // wait for all readers to have finished their job
  while Flags > 3 do
    spin := DoSpin(spin);
end;

procedure TRWLock.WriteUnlock;
begin
  if LastWriteLockCount <> 0 then
  begin
    dec(LastWriteLockCount);
    exit;
  end;
  LastWriteLockThread := TThreadID(0);
  LockedDec(Flags, 1);
end;

procedure TRWLock.Lock(context: TRWLockContext);
begin
  if context = cReadOnly then
    ReadOnlyLock
  else if context = cReadWrite then
    ReadWriteLock
  else
    WriteLock;
end;

procedure TRWLock.UnLock(context: TRWLockContext);
begin
  if context = cReadOnly then
    ReadOnlyUnLock
  else if context = cReadWrite then
    ReadWriteUnLock
  else
    WriteUnLock;
end;


{ TAutoLock }

constructor TAutoLock.Create(aLock: PSynLocker);
begin
  fLock := aLock;
  fLock^.Lock;
end;

destructor TAutoLock.Destroy;
begin
  fLock^.UnLock;
end;


{ TSynLocker }

function NewSynLocker: PSynLocker;
begin
  result := AllocMem(SizeOf(TSynLocker));
  InitializeCriticalSection(result^.fSection);
  result^.fInitialized := true;
end;

procedure TSynLocker.Init;
begin
  InitializeCriticalSection(fSection);
  fLockCount := 0;
  fInitialized := true;
  fRW.Init;
  PaddingUsedCount := 0;
end;

procedure TSynLocker.Done;
var
  i: PtrInt;
begin
  for i := 0 to PaddingUsedCount - 1 do
    if not (integer(Padding[i].VType) in VTYPE_SIMPLE) then
      VarClear(variant(Padding[i]));
  DeleteCriticalSection(fSection);
  fInitialized := false;
end;

procedure TSynLocker.DoneAndFreeMem;
begin
  Done;
  FreeMem(@self);
end;

function TSynLocker.GetIsLocked: boolean;
begin
  result := fLockCount <> 0;
end;

procedure TSynLocker.RWLock(context: TRWLockContext);
begin
  case fRWUse of
    uSharedLock:
      begin
        mormot.core.os.EnterCriticalSection(fSection);
        inc(fLockCount);
      end;
    uRWLock:
      fRW.Lock(context);
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.RWUnLock(context: TRWLockContext);
begin
  case fRWUse of
    uSharedLock:
      begin
        dec(fLockCount);
        mormot.core.os.LeaveCriticalSection(fSection);
      end;
    uRWLock:
      fRW.UnLock(context);
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.Lock;
begin
  case fRWUse of
    uSharedLock:
      begin
        mormot.core.os.EnterCriticalSection(fSection);
        inc(fLockCount);
      end;
    uRWLock:
      fRW.WriteLock;
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.UnLock;
begin
  case fRWUse of
    uSharedLock:
      begin
        dec(fLockCount);
        mormot.core.os.LeaveCriticalSection(fSection);
      end;
    uRWLock:
      fRW.WriteUnLock;
  end; // uNoLock will just do nothing
end;

function TSynLocker.TryLock: boolean;
begin
  result := (fRWUse = uSharedLock) and
            (mormot.core.os.TryEnterCriticalSection(fSection) <> 0);
  if result then
    inc(fLockCount);
end;

function TSynLocker.TryLockMS(retryms: integer; terminated: PBoolean): boolean;
var
  ms: integer;
  endtix: Int64;
begin
  result := TryLock;
  if result or
     (fRWUse <> uSharedLock) or
     (retryms <= 0) then
    exit;
  ms := 0;
  endtix := GetTickCount64 + retryms;
  repeat
    SleepHiRes(ms);
    result := TryLock;
    if result or
       ((terminated <> nil) and
        terminated^) then
      exit;
    ms := ms xor 1; // 0,1,0,1... seems to be good for scaling
  until GetTickCount64 > endtix;
end;

function TSynLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@self);
end;

function TSynLocker.GetVariant(Index: integer): Variant;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    result := variant(Padding[Index]);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    VarClear(result);
end;

procedure TSynLocker.SetVariant(Index: integer; const Value: Variant);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.GetInt64(Index: integer): Int64;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    if not VariantToInt64(variant(Padding[Index]), result) then
      result := 0;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := 0;
end;

procedure TSynLocker.SetInt64(Index: integer; const Value: Int64);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetBool(Index: integer): boolean;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    if not VariantToBoolean(variant(Padding[Index]), result) then
      result := false;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := false;
end;

procedure TSynLocker.SetBool(Index: integer; const Value: boolean);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetUnlockedInt64(Index: integer): Int64;
begin
  if (cardinal(Index) >= cardinal(PaddingUsedCount)) or
     not VariantToInt64(variant(Padding[Index]), result) then
    result := 0;
end;

procedure TSynLocker.SetUnlockedInt64(Index: integer; const Value: Int64);
begin
  if cardinal(Index) <= high(Padding) then
  begin
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  end;
end;

function TSynLocker.GetPointer(Index: integer): Pointer;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    with Padding[Index] do
      if VType = varUnknown then
        result := VUnknown
      else
        result := nil;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := nil;
end;

procedure TSynLocker.SetPointer(Index: integer; const Value: Pointer);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    with Padding[Index] do
    begin
      VarClearAndSetType(PVariant(@VType)^, varUnknown);
      VUnknown := Value;
    end;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.GetUtf8(Index: integer): RawUtf8;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    VariantStringToUtf8(variant(Padding[Index]), result);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := '';
end;

procedure TSynLocker.SetUtf8(Index: integer; const Value: RawUtf8);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    RawUtf8ToVariant(Value, variant(Padding[Index]));
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    result := 0;
    if Index < PaddingUsedCount then
      VariantToInt64(variant(Padding[Index]), result)
    else
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Int64(result + Increment);
  finally
    RWUnLock(cWrite);
  end
  else
    result := 0;
end;

function TSynLocker.LockedExchange(Index: integer; const Value: variant): variant;
begin
  VarClear(result);
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    with Padding[Index] do
    begin
      if Index < PaddingUsedCount then
        result := PVariant(@VType)^
      else
        PaddingUsedCount := Index + 1;
      PVariant(@VType)^ := Value;
    end;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.LockedPointerExchange(Index: integer; Value: pointer): pointer;
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    with Padding[Index] do
    begin
      if Index < PaddingUsedCount then
        if VType = varUnknown then
          result := VUnknown
        else
        begin
          VarClear(PVariant(@VType)^);
          result := nil;
        end
      else
      begin
        PaddingUsedCount := Index + 1;
        result := nil;
      end;
      VType := varUnknown;
      VUnknown := Value;
    end;
  finally
    RWUnLock(cWrite);
  end
  else
    result := nil;
end;



{ TSynLocked }

constructor TSynLocked.Create;
begin
  fSafe := NewSynLocker;
end;

destructor TSynLocked.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;

{ TLecuyerThreadSafe }

function TLecuyerThreadSafe.Next: cardinal;
begin
  Safe.Lock;
  result := Generator.Next;
  Safe.UnLock;
end;

function TLecuyerThreadSafe.NextDouble: double;
begin
  Safe.Lock;
  result := Generator.NextDouble;
  Safe.UnLock;
end;

procedure TLecuyerThreadSafe.Fill(dest: pointer; count: integer);
begin
  Safe.Lock;
  Generator.Fill(dest, count);
  Safe.UnLock;
end;

procedure TLecuyerThreadSafe.FillShort31(var dest: TShort31);
begin
  Safe.Lock;
  Generator.FillShort31(dest);
  Safe.UnLock;
end;



{ ****************** Unix Daemon and Windows Service Support }

function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs;
  argc: PInteger; temp: PRawUtf8; posix: boolean): TParseCommands;
var
  n: PtrInt;
  state: set of (sWhite, sInArg, sInSQ, sInDQ, sSpecial, sBslash);
  c: AnsiChar;
  D, P: PAnsiChar;
begin
  result := [pcInvalidCommand];
  if argv <> nil then
    argv[0] := nil;
  if argc <> nil then
    argc^ := 0;
  if cmd = '' then
    exit;
  if argv = nil then
    D := nil
  else
  begin
    if temp = nil then
      exit;
    SetLength(temp^, length(cmd));
    D := pointer(temp^);
  end;
  state := [];
  n := 0;
  P := pointer(cmd);
  repeat
    c := P^;
    if D <> nil then
      D^ := c;
    inc(P);
    case c of
      #0:
        begin
          if sInSQ in state then
            include(result, pcUnbalancedSingleQuote);
          if sInDQ in state then
            include(result, pcUnbalancedDoubleQuote);
          exclude(result, pcInvalidCommand);
          if argv <> nil then
            argv[n] := nil;
          if argc <> nil then
            argc^ := n;
          exit;
        end;
      #1 .. ' ':
        begin
         if state = [sInArg] then
         begin
           state := [];
           if D <> nil then
           begin
             D^ := #0;
             inc(D);
           end;
           continue;
         end;
         if state * [sInSQ, sInDQ] = [] then
           continue;
        end;
      '\':
        if posix and
           (state * [sInSQ, sBslash] = []) then
          if sInDQ in state then
          begin
            case P^ of
              '"', '\', '$', '`':
                begin
                  include(state, sBslash);
                  continue;
                end;
            end;
          end
          else if P^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '^':
        if not posix and
           (state * [sInSQ, sInDQ, sBslash] = []) then
          if PWord(P)^ = $0a0d then
          begin
            inc(P, 2);
            continue;
          end
          else if P^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '''':
        if posix and
           not(sInDQ in state) then
          if sInSQ in state then
          begin
            exclude(state, sInSQ);
            continue;
          end
          else if state = [] then
          begin
            if argv <> nil then
            begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInSQ, sInArg];
            continue;
          end
          else if state = [sInArg] then
          begin
            state := [sInSQ, sInArg];
            continue;
          end;
      '"':
        if not(sInSQ in state) then
          if sInDQ in state then
          begin
            exclude(state, sInDQ);
            continue;
          end
          else if state = [] then
          begin
            if argv <> nil then
            begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInDQ, sInArg];
            continue;
          end
          else if state = [sInArg] then
          begin
            state := [sInDQ, sInArg];
            continue;
          end;
      '|', '<', '>':
        if state * [sInSQ, sInDQ] = [] then
          include(result, pcHasRedirection);
      '&', ';':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
        begin
          include(state, sSpecial);
          include(result, pcHasJobControl);
        end;
      '`':
        if posix and
           (state * [sInSQ, sBslash] = []) then
           include(result, pcHasSubCommand);
      '(', ')':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasParenthesis);
      '$':
        if posix and
           (state * [sInSQ, sBslash] = []) then
          if p^ = '(' then
            include(result, pcHasSubCommand)
          else
            include(result, pcHasShellVariable);
      '*', '?':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasWildcard);
    end;
    exclude(state, sBslash);
    if state = [] then
    begin
      if argv <> nil then
      begin
        argv[n] := D;
        inc(n);
        if n = high(argv^) then
          exit;
      end;
      state := [sInArg];
    end;
    if D <> nil then
      inc(D);
  until false;
end;


procedure FinalizeUnit;
var
  i: PtrInt;
begin
  with InternalGarbageCollection do
  begin
    Shutdown := true;
    for i := Count - 1 downto 0 do
      FreeAndNilSafe(Instances[i]); // before GlobalCriticalSection deletion
  end;
  ObjArrayClear(CurrentFakeStubBuffers);
  Executable.Version.Free;
  DeleteCriticalSection(GlobalCriticalSection);
  FinalizeSpecificUnit; // in mormot.core.os.posix/windows.inc files
end;

initialization
  {$ifdef ISFPC27}
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$endif ISFPC27}
  InitializeCriticalSection(GlobalCriticalSection);
  InitializeUnit; // in mormot.core.os.posix/windows.inc files
  OSVersionShort := ToTextOS(OSVersionInt32);
  SetExecutableVersion(0, 0, 0, 0);
  JSON_CONTENT_TYPE_VAR := JSON_CONTENT_TYPE;
  JSON_CONTENT_TYPE_HEADER_VAR := JSON_CONTENT_TYPE_HEADER;
  NULL_STR_VAR := 'null';
  BOOL_UTF8[false] := 'false';
  BOOL_UTF8[true] := 'true';
  // minimal stubs which will be properly implemented in mormot.core.log.pas
  GetExecutableLocation := _GetExecutableLocation;
  SetThreadName := _SetThreadName;

finalization
  FinalizeUnit;

end.

