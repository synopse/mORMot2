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
    See mormot.core.os.mac and mormot.core.os.security units for completion.
  In practice, no "Windows", nor "Linux/Posix" reference should be needed in
    regular units, once mormot.core.os is included. :)
  This unit only refers to mormot.core.base so can be used almost stand-alone.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef OSWINDOWS}
  Windows, // needed here e.g. for redefinition/redirection of standard types
  Messages,
  {$endif OSWINDOWS}
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base;


{ ****************** Some Cross-System Type and Constant Definitions }

type
  /// allow to customize the possible line feeds
  TLineFeed = (
    lfSystem,
    lfCR,
    lfCRLF);

const
  {$ifdef OSWINDOWS}
  /// operating-system dependent Line Feed characters (#13#10 on Windows)
  CRLF = #13#10;
  /// operating-system dependent wildchar to match all files in a folder
  FILES_ALL = '*.*';
  /// operating-system dependent "inverted" delimiter for NormalizeFileName()
  InvertedPathDelim = '/';
  /// operating-system dependent boolean if paths are case-insensitive
  PathCaseInsensitive = true;
  {$else}
  /// operating-system dependent Line Feed characters (#10 on POSIX)
  CRLF = #10;
  /// operating-system dependent wildchar to match all files in a folder
  FILES_ALL = '*';
  /// operating-system dependent "inverted" delimiter for NormalizeFileName()
  InvertedPathDelim = '\';
  /// operating-system dependent boolean if paths are case-insensitive
  PathCaseInsensitive = false;
  {$endif OSWINDOWS}
  /// system-independent CR+LF two chars, as used on Windows or HTTP headers
  EOL = #13#10;
  /// system-independent CR+LF two chars, as 16-bit constant
  EOLW = $0a0d;
  /// convert a TLineFeed value into its UTF-8 text representation
  LINE_FEED: array[TLineFeed] of TShort3 = (CRLF, #10, #13#10);

  /// human-friendly alias to open a file for exclusive writing ($20)
  fmShareRead      = fmShareDenyWrite;
  /// human-friendly alias to open a file for exclusive reading ($30)
  fmShareWrite     = {$ifdef DELPHIPOSIX} fmShareDenyNone {$else} fmShareDenyRead {$endif};
  /// human-friendly alias to open a file with no read/write exclusion ($40)
  fmShareReadWrite = fmShareDenyNone;

  /// execute FileOpen/TFileStreamEx.Create for reading without exclusion
  fmOpenReadShared = fmOpenRead or fmShareReadWrite;

  /// execute FileOpen/TFileStreamEx.Create for writing without exclusion
  fmOpenWriteShared = fmOpenReadWrite or fmShareReadWrite;

  /// a convenient constant to create a file without exclusion
  // - warning: on Delphi 7..2009, fmCreate is defined as $ffff so can't be
  // associated with file sharing attributes: we will force fmShareReadWrite
  fmCreateShared = fmCreate or fmShareReadWrite;

  /// a convenient array constant to open a file for writing without exclusion
  fmCreateOrRewrite: array[{rewrite=}boolean] of cardinal = (
    fmCreateShared,
    fmOpenWriteShared);

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
  /// HTTP Status Code for "Permanent Redirect"
  HTTP_PERMANENTREDIRECT = 308;
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
  /// HTTP Status Code for "Gone"
  HTTP_GONE = 410;
  /// HTTP Status Code for "Length Required"
  HTTP_LENGTHREQUIRED = 411;
  /// HTTP Status Code for "Payload Too Large"
  HTTP_PAYLOADTOOLARGE = 413;
  /// HTTP Status Code for "Range Not Satisfiable"
  HTTP_RANGENOTSATISFIABLE = 416;
  /// HTTP Status Code for "I'm a teapot"
  HTTP_TEAPOT = 418;
  /// HTTP Status Code for "Unprocessable Content"
  HTTP_UNPROCESSABLE_CONTENT = 422;
  /// HTTP Status Code for "Upgrade Required"
  HTTP_UPGRADE_REQUIRED = 426;
  /// HTTP Status Code for "Too Many Requests"
  HTTP_TOO_MANY_REQUESTS = 429;
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

  /// a fake response code, generated for client side panic failure/exception
  // - for it is the number of a man, and that number is 666
  HTTP_CLIENTERROR = 666;
  /// a fake response code, usedfor internal THttpAsyncServer asynchronous process
  HTTP_ASYNCRESPONSE = 777;

  /// the successful HTTP response codes after a GET request
  HTTP_GET_OK = [HTTP_SUCCESS, HTTP_NOCONTENT, HTTP_PARTIALCONTENT];

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
  // - no 'charset=utf-8' encoding is necessary, as specified by RFC 7159
  JSON_CONTENT_TYPE = 'application/json';

  /// HTTP header for MIME content type used for plain JSON
  // - i.e. 'Content-Type: application/json'
  JSON_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + JSON_CONTENT_TYPE;

  /// MIME content type used for plain UTF-8 text - see RFC 7231 section-3.1.1
  TEXT_CONTENT_TYPE = 'text/plain;charset=utf-8';

  /// HTTP header for MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + TEXT_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded HTML - see RFC 7231 section-3.1.1
  HTML_CONTENT_TYPE = 'text/html;charset=utf-8';

  /// HTTP header for MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + HTML_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded XML
  XML_CONTENT_TYPE = 'text/xml';

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

  BOTBUSTER_RESPONSE = 'Botbusters message: "I ain''t ''fraid of no bot"';

  /// HTTP body from RFC 2324 e.g. for banned IP or hsoRejectBotUserAgent
  HTTP_BANIP_RESPONSE: string[191] =
    'HTTP/1.0 418 I''m a teapot'#13#10 +
    'Content-Length: 111'#13#10 +
    'Content-Type: text/plain'#13#10#13#10 +
    'Server refuses to brew coffee because it is currently a teapot.'#13#10 +
    BOTBUSTER_RESPONSE;

  /// JSON compatible representation of a boolean value, i.e. 'false' and 'true'
  // - can be used e.g. in logs, or anything accepting a ShortString
  BOOL_STR: array[boolean] of string[7] = (
    'false', 'true');

  /// the JavaScript-like values of non-number IEEE constants
  // - as recognized by ShortToFloatNan, and used by TTextWriter.Add()
  // when serializing such single/double/extended floating-point values
  // - GetExtended() should also detect those values
  JSON_NAN: array[TFloatNan] of string[11] = (
    '0', '"NaN"', '"Infinity"', '"-Infinity"');

var
  /// MIME content type used for JSON communication
  // - i.e. 'application/json' as stated by datatracker.ietf.org/doc/html/rfc7159
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

const // some time conversion constants with Milli/Micro/NanoSec resolution
  SecsPerHour  = SecsPerMin * MinsPerHour; // missing in oldest Delphi
  SecsPerDay   = SecsPerMin * MinsPerDay;
  SecsPerWeek  = 7 * SecsPerDay;
  SecsPerMonth = 2629746; // rough approximation of SecsPerDay * 365.2425 / 12
  SecsPerYear  = 12 * SecsPerMonth;

  MilliSecsPerSec      = 1000;
  MilliSecsPerMin      = MilliSecsPerSec  * SecsPerMin;
  MilliSecsPerHour     = MilliSecsPerMin  * MinsPerHour;
  MilliSecsPerDay      = MilliSecsPerHour * HoursPerDay;
  MicroSecsPerMilliSec = 1000;
  MicroSecsPerSec      = MicroSecsPerMilliSec * MilliSecsPerSec;
  NanoSecsPerMicroSec  = 1000;
  NanoSecsPerMilliSec  = NanoSecsPerMicroSec  * MicroSecsPerMilliSec;
  NanoSecsPerSec       = NanoSecsPerMilliSec  * MilliSecsPerSec;

/// check if Host is in 127.0.0.0/8 range (e.g. cLocalhost or c6Localhost)
// - warning: Host should be not nil
// - would detect both IPv4 '127.x.x.x' pattern and plain IPv6 '::1' constant
function IsLocalHost(Host: PUtf8Char): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// returns length(Address) if there is no ?parameter nor #anchor in the URI
function UriTruncLen(const Address: RawUtf8): PtrInt;

/// returns length(Address) if there is no ?#anchor in the URI
function UriTruncAnchorLen(const Address: RawUtf8): PtrInt;
  {$ifdef HASINLINE} inline; {$endif}


{ ****************** Gather Operating System Information }

type
  /// Exception types raised by this mormot.core.os unit
  EOSException = class(ExceptionWithProps);

  /// the known operating systems
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
    osAndroid,
    osApt,
    osRpm);
  TOperatingSystems = set of TOperatingSystem;

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
    wServer2022_64,
    wServer2025_64);

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

  /// notable Linux distributions, organized by their package management system
  TLinuxDistribution = (
    ldNotLinux,
    ldUndefined,
    ldApt,
    ldRpm,
    ldPacman,
    ldPortage,
    ldAndroid);

const
  /// the recognized MacOS versions, as plain text
  // - indexed from OSVersion32.utsrelease[2] kernel revision
  // - see https://en.wikipedia.org/wiki/MacOS_version_history#Releases
  MACOS_NAME: array[8 .. 26] of TShort23 = (
    '10.4 Tiger',
    '10.5 Leopard',
    '10.6 Snow Leopard',
    '10.7 Lion',
    '10.8 Mountain Lion',
    '10.9 Mavericks',
    '10.10 Yosemite',
    '10.11 El Capitan',
    '10.12 Sierra',
    '10.13 High Sierra',
    '10.14 Mojave',
    '10.15 Catalina',
    '11 Big Sur',
    '12 Monterey',
    '13 Ventura',
    '14 Sonoma',
    '15 Sequoia',
    '26 Tahoe',
    '27 Next');

  /// the recognized Windows versions, as plain text
  // - defined even outside OSWINDOWS to allow process e.g. from monitoring tools
  WINDOWS_NAME: array[TWindowsVersion] of TShort23 = (
    '',                     // wUnknown
    '2000',                 // w2000
    'XP',                   // wXP
    'XP 64bit',             // wXP_64
    'Server 2003',          // wServer2003
    'Server 2003 R2',       // wServer2003_R2
    'Vista',                // wVista
    'Vista 64bit',          // wVista_64
    'Server 2008',          // wServer2008
    'Server 2008 64bit',    // wServer2008_64
    '7',                    // wSeven
    '7 64bit',              // wSeven_64
    'Server 2008 R2',       // wServer2008_R2
    'Server 2008 R2 64bit', // wServer2008_R2_64
    '8',                    // wEight
    '8 64bit',              // wEight_64
    'Server 2012',          // wServer2012
    'Server 2012 64bit',    // wServer2012_64
    '8.1',                  // wEightOne
    '8.1 64bit',            // wEightOne_64
    'Server 2012 R2',       // wServer2012R2
    'Server 2012 R2 64bit', // wServer2012R2_64
    '10',                   // wTen
    '10 64bit',             // wTen_64
    'Server 2016',          // wServer2016
    'Server 2016 64bit',    // wServer2016_64
    '11',                   // wEleven
    '11 64bit',             // wEleven_64
    'Server 2019 64bit',    // wServer2019_64
    'Server 2022 64bit',    // wServer2022_64
    'Server 2025 64bit');   // wServer2025_64

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
     wServer2016,
     wEleven];

  /// translate one operating system (and distribution) into a its common name
  OS_NAME: array[TOperatingSystem] of TShort15 = (
    'Unknown',      // osUnknown
    'Windows',      // osWindows
    'Linux',        // osLinux
    'OSX',          // osOSX
    'BSD',          // osBSD
    'POSIX',        // osPOSIX
    'Arch',         // osArch
    'Aurox',        // osAurox
    'Debian',       // osDebian
    'Fedora',       // osFedora
    'Gentoo',       // osGentoo
    'Knoppix',      // osKnoppix
    'Mint',         // osMint
    'Mandrake',     // osMandrake
    'Mandriva',     // osMandriva
    'Novell',       // osNovell
    'Ubuntu',       // osUbuntu
    'Slackware',    // osSlackware
    'Solaris',      // osSolaris
    'Suse',         // osSuse
    'Synology',     // osSynology
    'Trustix',      // osTrustix
    'Clear',        // osClear
    'United',       // osUnited
    'RedHat',       // osRedHat
    'LFS',          // osLFS
    'Oracle',       // osOracle
    'Mageia',       // osMageia
    'CentOS',       // osCentOS
    'Cloud',        // osCloud
    'Xen',          // osXen
    'Amazon',       // osAmazon
    'CoreOS',       // osCoreOS
    'Alpine',       // osAlpine
    'Android',      // osAndroid
    'Debian-based', // osApt - any Debian-based flavor
    'Rpm-based');   // osRpm - any RedHat-based flavor

  /// translate one operating system (and distribution) into a single character
  // - may be used internally e.g. for a HTTP User-Agent header, as with
  // TFileVersion.UserAgent and UserAgentParse()
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
    'g', // Mageia
    'c', // CentOS
    'd', // Cloud
    'x', // Xen
    'Z', // Amazon
    'r', // CoreOS
    'p', // Alpine
    'J', // Android (J=JVM)
    '1', // Apt-based
    '2'  // Rpm-based
    );

  /// the operating systems items which actually have a Linux kernel
  OS_LINUX = [osLinux, osArch .. osSlackware, osSuse, osTrustix .. osRpm];

  /// used to recognize the package management system used by a Linux OS
  LINUX_DIST: array[TLinuxDistribution] of TOperatingSystems = (
    [osUnknown, osWindows, osOSX, osBSD, osPOSIX, osSolaris, osSynology],  // ldNotLinux
    [osLinux, osSlackware, osClear, osLFS, osXen, osAlpine],               // ldUndefined
    [osDebian, osKnoppix, osMint, osUbuntu, osApt],                          // ldApt
    [osAurox, osFedora, osMandrake, osMandriva, osNovell, osSuse, osTrustix, // ldRpm
     osUnited, osRedHat, osOracle, osMageia, osCentOS, osCloud, osAmazon, osRpm],
    [osArch],                                                              // ldPacman
    [osGentoo, osCoreOs],                                                  // ldPortage
    [osAndroid]);                                                          // ldAndroid

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

  /// the current Linux Distribution, depending on its package management system
  OS_DISTRI: TLinuxDistribution;

  /// the current Operating System version, as retrieved for the current process
  // - contains e.g. 'Windows Seven 64 SP1 (6.1.7601)' or 'Windows XP SP3 (5.1.2600)' or
  // 'Windows 10 64bit 22H2 (10.0.19045.4046)' or 'macOS 13 Ventura (Darwin 22.3.0)' or
  // 'Ubuntu 16.04.5 LTS - Linux 3.13.0 110 generic#157 Ubuntu SMP Mon Feb 20 11:55:25 UTC 2017'
  OSVersionText: RawUtf8;
  /// some addition system information as text, e.g. 'Wine 1.1.5' or 'Prism'
  // - also always appended to OSVersionText high-level description
  // - use if PosEx('Wine', OSVersionInfoEx) > 0 then to check for Wine presence
  OSVersionInfoEx: RawUtf8;
  /// the current Operating System version, as retrieved for the current process
  // and computed by ToTextOSU(OSVersionInt32)
  // - contains e.g. 'Windows Vista' or 'Ubuntu Linux 5.4.0' or
  // 'macOS 13 Ventura 22.3.0'
  OSVersionShort: RawUtf8;

  {$ifdef OSWINDOWS}
  /// on Windows, the Update Build Revision as shown with the "ver/winver" command
  // - to track the current update state of the system
  WindowsUbr: integer;
  /// on Windows, the ready-to-be-displayed text version of the system
  // - e.g. 'Windows 10 Entreprise N'
  WindowsProductName: RawUtf8;
  /// on Windows, the ready-to-be-displayed text version of the system
  // - e.g. '22H2'
  WindowsDisplayVersion: RawUtf8;
  {$endif OSWINDOWS}

  /// some textual information about the current CPU and its known cache
  // - contains e.g. '4 x Intel(R) Core(TM) i5-7300U CPU @ 2.60GHz [3MB]'
  CpuInfoText: RawUtf8;
  /// the available cache information as returned by the OS
  // - e.g. 'L1=2*32KB  L2=256KB  L3=3MB' on Windows or '3072 KB' on Linux
  CpuCacheText: RawUtf8;

  /// the on-chip cache size, in bytes, as returned by the OS
  // - retrieved from /proc/cpuinfo "cache size" entry (L3 cache) on Linux or
  // CpuCache[3/4].Size (from GetLogicalProcessorInformation) on Windows
  CpuCacheSize: cardinal;
  /// how many hardware CPU sockets are defined on this system
  // - i.e. the number of physical CPU slots, not the number of logical CPU
  // cores as returned by SystemInfo.dwNumberOfProcessors
  // - as used e.g. by SetThreadAffinity()
  CpuSockets: integer;

  /// Level 1 to 4 CPU caches as returned by GetLogicalProcessorInformation
  // - yes, Intel introduced a Level 4 cache (eDRAM) with some Haswell/Iris CPUs
  // - this information is not retrieved on all Linux / POSIX systems yet
  // - only Unified or Data caches are include (not Instruction or Trace)
  // - note: some CPU - like the Apple M1 - have 128 bytes of LineSize
  CpuCache: array[1..4] of record
    Count, Size, LineSize: cardinal;
  end;

  {$ifdef OSLINUXANDROID}
  /// contains the content of Linux /proc/cpuinfo as retrieved at startup
  CpuInfoLinux: RawUtf8;
  /// contains the Flags: or Features: value of Linux /proc/cpuinfo
  CpuInfoFeatures: RawUtf8;
  {$endif OSLINUXANDROID}

  /// the running Operating System
  OSVersion32: TOperatingSystemVersion;
  /// the running Operating System, encoded as a 32-bit integer
  OSVersionInt32: integer absolute OSVersion32;

/// some textual information about the current computer hardware, from BIOS
// - contains e.g. 'LENOVO 20HES23B0U ThinkPad T470'
// - on Windows, will check the registry; on BSD, will read HW_MACHINE and
// HW_MODEL; on Linux/Android will use a function to avoid some syscalls
{$ifdef OSLINUXANDROID} function {$endif} BiosInfoText: RawUtf8;

/// convert an Operating System type into its human-friendly text representation
// - returns e.g. 'Windows Vista' or 'Windows 10 22H2' or 'Ubuntu' or
// 'macOS 13 Ventura' or 'Windows Server 2022 64bit 21H2'
function ToText(const osv: TOperatingSystemVersion): TShort47; overload;

/// convert an Operating System type into its human-friendly text representation
function ToTextU(const osv: TOperatingSystemVersion): RawUtf8; overload;

/// low-level function used internally by ToText(osv) to detect Windows versions
// - append e.g. ' 25H2' with sep = ' ' - see also WindowsDisplayVersion
procedure AppendOsBuild(const osv: TOperatingSystemVersion; dest: PAnsiChar;
  sep: AnsiChar = #0);

/// returns '' or the detected Windows Version, e.g. ' 25H2' with sep=' '
function WinOsBuild(const osv: TOperatingSystemVersion; sep: AnsiChar): TShort7;
  {$ifdef HASINLINE} inline; {$endif}

/// convert an Operating System type into its one-word text representation
// - returns e.g. 'Vista' or 'Ubuntu' or 'OSX'
function OsvToShort(const osv: TOperatingSystemVersion): PShortString;

/// convert a 32-bit Operating System type into its full text representation
// - including the kernel revision (not the distribution version) on POSIX systems
// - returns e.g. 'Windows Vista', 'Windows 11 64-bit 21H2 22000' or
// 'Ubuntu Linux 5.4.0'
function ToTextOS(osint32: integer): TShort47;

/// convert a 32-bit Operating System type into its full RawUtf8 representation
// - returns e.g. 'Windows Server 2022 64bit 21H2 20349'
function ToTextOSU(osint32: integer): RawUtf8;

/// check if the current OS (i.e. OS_KIND value) match a description
// - will handle osPosix and osLinux as generic detection of those systems
// - osUnknown will always return true
function MatchOS(os: TOperatingSystem): boolean;

/// recognize the Linux distribution for a given Operating System
function LinuxDistribution(os: TOperatingSystem): TLinuxDistribution;

/// return the best known ERROR_* system error message constant texts
// - without the 'ERROR_' prefix, but in a cross-platform way
// - as used by WinApiErrorString() and some low-level Windows API wrappers
function WinErrorConstant(Code: cardinal): PShortString;

/// return the error code number, and its regular constant on Windows (if known)
// - e.g. WinErrorShort(5) = '5 ERROR_ACCESS_DENIED' or
// WinErrorShort($c00000fd) = 'c00000fd EXCEPTION_STACK_OVERFLOW'
function WinErrorShort(Code: cardinal; NoInt: boolean = false): TShort47; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// return the error code number, and its regular constant on Windows (if known)
procedure WinErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean = false); overload;

/// return the error code number, and its regular constant on Linux (if known)
procedure LinuxErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean = false);

/// return the error code number, and its regular constant on Bsd (if known)
procedure BsdErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean = false);

/// return the error code number, and its regular constant on the current OS
// - redirect to WinErrorShort/LinuxErrorShort/BsdErrorShort() functions
// - e.g. OsErrorShort(5) = '5 ERROR_ACCESS_DENIED' on Windows or '5 EIO' on POSIX
procedure OsErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean = false); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// return the error code number, and its regular constant on the current OS
// - redirect to WinErrorShort/LinuxErrorShort/BsdErrorShort() functions
// - e.g. OsErrorShort(5) = '5 ERROR_ACCESS_DENIED' on Windows or '5 EIO' on POSIX
function OsErrorShort(Code: cardinal = 0; NoInt: boolean = false): TShort47; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append the error code number, and its regular constant on the current OS
procedure OsErrorAppend(Code: cardinal; var Dest: ShortString;
  Sep: AnsiChar = #0; NoInt: boolean = false);

/// append the error as ' ERROR_*' constant and return TRUE if known
// - append nothing and return FALSE if Code is not known
function AppendWinErrorText(Code: cardinal; var Dest: ShortString;
  Sep: AnsiChar): boolean;

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
    actNeoverseV1,
    actCortexA78,
    actCortexA78AE,
    actCortexX1,
    actCortex510,
    actCortex710,
    actCortexX2,
    actNeoverseN2,
    actNeoverseE1,
    actCortexA78C,
    actCortexX1C,
    actCortexA715,
    actCortexX3,
    actNeoverseV2,
    actCortexA520,
    actCortexA720,
    actCortexX4,
    actNeoverseV3AE,
    actNeoverseV3,
    actCortextX925,
    actCortextA725,
    actCortextA520AE,
    actCortextA720AE,
    actC1Nano,
    actC1Pro,
    actC1Ultra,
    actNeoverseN3,
    actCortextA320,
    actC1Premium);

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
    aciMicrosoft,
    aciPhytium,
    aciAmpere);
  /// a set of recognized ARM/AARCH64 CPU hardware implementers
  TArmCpuImplementers = set of TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU from its 12-bit hardware ID
function ArmCpuType(id: word): TArmCpuType;

/// recognize a given ARM/AARCH64 CPU type name from its 12-bit hardware ID
function ArmCpuTypeName(act: TArmCpuType; id: word;
  const before: ShortString = ''): ShortString;

/// recognize a given ARM/AARCH64 CPU implementer from its 8-bit hardware ID
function ArmCpuImplementer(id: byte): TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU implementer name from its 8-bit hardware ID
function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word;
  const after: ShortString = ''): ShortString;


const
  /// contains the Delphi/FPC Compiler Version as text
  // - e.g. 'Delphi 10.3 Rio', 'Delphi 2010' or 'Free Pascal 3.3.1'
  COMPILER_VERSION: RawUtf8 =
  {$ifdef FPC}
    'Free Pascal ' + {$I %FPCVERSION%} // FPC makes it simple
  {$else}
    'Delphi'
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
    {$elseif defined(VER350)} + ' 11'
      {$if declared(RTLVersion113)} + '.3' {$else}
      {$if declared(RTLVersion112)} + '.2' {$else}
      {$if declared(RTLVersion111)} + '.1' {$ifend} {$ifend} {$ifend}
                              + ' Alexandria'
    {$elseif defined(VER360)} + ' 12'
      {$if declared(RTLVersion123)} + '.3' {$else}
      {$if declared(RTLVersion122)} + '.2' {$else}
      {$if declared(RTLVersion121)} + '.1' {$ifend} {$ifend} {$ifend}
                              + ' Athens'
    {$elseif defined(VER370)} + ' 13 Florence'
    {$elseif defined(VER380)} + ' 14 Next'
    {$ifend}
  {$endif FPC}
  {$ifdef CPU64} + ' 64 bit' {$else} + ' 32 bit' {$endif};

{$ifndef PUREMORMOT2}
const
  HTTP_RESP_STATICFILE = STATICFILE_CONTENT_TYPE;

/// deprecated function: use COMPILER_VERSION constant instead
function GetDelphiCompilerVersion: RawUtf8; deprecated;
{$endif PUREMORMOT2}

{$ifdef OSWINDOWS}

{$ifdef UNICODE}

const
  /// a global constant to be appended for Windows Ansi or Wide API names
  // - match the Wide API on Delphi, since String=UnicodeString
  // - you should not use this suffix, but the 'W' API everywhere, with proper
  // conversion into RawUtf8 or TFileName/string
  _AW = 'W';

{$else}

const
  /// a global constant to be appended for Windows Ansi or Wide API names
  // - match the Ansi API oldest Delphi, where String=AnsiString
  // - but won't always match the Ansi API on FPC, because Lazarus forces
  // CP_UTF8, so you should NOT use this suffix, but the '*W' API everywhere,
  // with proper conversion into RawUtf8 or TFileName/string
  _AW = 'A';

type
  /// low-level API structure, not defined in old Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWord;
    dwMajorVersion: DWord;
    dwMinorVersion: DWord;
    dwBuildNumber: DWord;
    dwPlatformId: DWord;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;

{$endif UNICODE}

{$endif OSWINDOWS}

type
  /// used to retrieve version information from any EXE
  // - under Linux, all version numbers are set to 0 by default, unless
  // you define the FPCUSEVERSIONINFO conditional and information is
  // extracted from executable resources
  // - for the main executable, do not create once instance of this class, but
  // call GetExecutableVersion / SetExecutableVersion and access the Executable
  // global variable
  TFileVersion = class(TSynPersistent)
  protected
    fDetailed: string;
    fFileName: TFileName;
    fBuildDateTime: TDateTime;
    fVersionInfo, fUserAgent: RawUtf8;
    // change the version - returns true if supplied values are actually new
    function SetVersion(aMajor, aMinor, aRelease, aBuild: integer): boolean;
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
    /// initialize the version information, with optional custom values
    // - will set the version numbers, and get BuildDateTime/BuildYear
    // - call RetrieveInformationFromFileName to parse its internal resources
    // - for the main executable, do not use this constructor, but call
    // GetExecutableVersion / SetExecutableVersion and access the Executable
    // global variable
    constructor Create(const aFileName: TFileName; aMajor: integer = 0;
      aMinor: integer = 0; aRelease: integer = 0; aBuild: integer = 0;
      aBuildDate: TDateTime = 0); reintroduce;
    /// open and extract file information from the executable FileName
    // - note that resource extraction is not available on POSIX, unless the
    // FPCUSEVERSIONINFO conditional has been specified in the project options
    // - for the main executable, don't call from Executable.Version, but just
    // run GetExecutableVersion global procedure instead
    function RetrieveInformationFromFileName: boolean;
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
    // - use UserAgentParse() to decode this text into meaningful information
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

/// quickly parse the TFileVersion.UserAgent content
// - identify e.g. 'myprogram/3.1.0.2W' or 'myprogram/3.1.0.2W32' text
function UserAgentParse(const UserAgent: RawUtf8;
  out ProgramName, ProgramVersion: RawUtf8; out OS: TOperatingSystem): boolean;

/// detect any & character, and extract it as part of the result array
// - e.g. UnAmp('alter&nate') returns ['n', 'alternate']
function UnAmp(const name: RawUtf8): TRawUtf8DynArray;

type
  /// the command line switches supported by TExecutableCommandLine
  // - clkArg is for "exename arg1 arg2 arg3" main indexed arguments
  // - clkOption is for "exename -o --opt1" boolean flags
  // - clkParam is for "exename -n value --name value --name2=value2" pairs
  TExecutableCommandLineKind = (
    clkUndefined,
    clkArg,
    clkOption,
    clkParam);

  /// implements command-line arguments parsing
  // - in practice, is accessible via the Executable.Command global instance
  // - call Arg() Options() and Get/Param() to define and retrieve the flags
  // from their names and supply some description text, then call
  // DetectUnknown and/or FullDescription to interact with the user
  // - by default, will use -/-- switches on POSIX, and / on Windows
  TExecutableCommandLine = class
  protected
    fNames: array[clkArg .. clkParam] of TRawUtf8DynArray;
    fRawParams, fValues: TRawUtf8DynArray; // for clkParam
    fDesc, fDescDetail: array[clkArg .. clkParam] of RawUtf8;
    fRetrieved: array[clkArg .. clkParam] of TBooleanDynArray;
    fDescArg: TRawUtf8DynArray;
    fCaseSensitiveNames: boolean;
    fSwitch: array[{long=}boolean] of RawUtf8;
    fLineFeed, fExeDescription, fUnknown: RawUtf8;
    procedure Describe(const v: array of RawUtf8;
      k: TExecutableCommandLineKind; d, def: RawUtf8; argindex: integer);
    function Find(const v: array of RawUtf8;
      k: TExecutableCommandLineKind = clkUndefined; const d: RawUtf8 = '';
      const def: RawUtf8 = ''; f: PtrInt = 0): PtrInt;
  public
    /// mark and describe an "arg" value by 0-based index in Args[]
    // - if true, you can access the value from Args[index]
    function Arg(index: integer; const description: RawUtf8 = '';
      optional: boolean = true): boolean; overload;
    /// mark and describe an "arg" value by 0-based index in Args[]
    // - if existing, returns the value from Args[index] - otherwise returns ''
    function ArgU(index: integer; const description: RawUtf8 = '';
      optional: boolean = true): RawUtf8;
    /// mark and describe a string/TFileName "arg" value by 0-based index in Args[]
    // - if existing, returns Args[index] as string - otherwise returns ''
    function ArgString(index: integer; const description: RawUtf8 = '';
      optional: boolean = true): string;
    /// mark and describe an existing TFileName "arg" value by 0-based index in Args[]
    // - if set, will fail in DetectUnknown if the file (or the folder) does not
    // exist, or returns Args[index] file/folder name as string
    function ArgFile(index: integer; const description: RawUtf8 = '';
      optional: boolean = true; isFolder: boolean = false): TFileName;
    /// will fail in DetectUnknown if the file or folder name does not exist
    // - also calls and return ExpandFileName() on the supplied file or folder name
    function CheckFileName(const name: TFileName; isFolder: boolean = false): TFileName;
    /// mark and describe an "arg" value in Args[]
    // - e.g. returns true if the name appears in Args[]
    function Arg(const name: RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// mark and describe or or several "arg" value(s) in Args[]
    // - e.g. returns true if any of the name(s) appears in Args[]
    function Arg(const name: array of RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for a -xxxx switch in Options[]
    // - returns true if '-name' or '--name' or '/name' do appear
    // - if name contains a & character, will also register the following char,
    // e.g. Option('&concise') is the same as Option(['c', 'concise'])
    function Option(const name: RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for one or severl -xxxx switches in Options[]
    // - returns true if any '-name' or '--name' or '/name' do appear
    function Option(const name: array of RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its RawUtf8 "parametervalue"
    // - returns true if '-name' or '--name' or '/name' do appear with a value
    // - if name contains a & character, will also register the following char,
    // e.g. Get('&concise') is the same as Get(['c', 'concise'])
    function Get(const name: RawUtf8; out value: RawUtf8;
      const description: RawUtf8 = ''; const default: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its RawUtf8 "parametervalue"
    // - returns true if any '-name' or '--name' or '/name' do appear with a value
    function Get(const name: array of RawUtf8; out value: RawUtf8;
      const description: RawUtf8 = ''; const default: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return all RawUtf8 "parametervalue" occurrences
    function Get(const name: array of RawUtf8; out value: TRawUtf8DynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its plain string "parametervalue"
    // - if name contains a & character, will also register the following char
    function Get(const name: RawUtf8; out value: string;
      const description: RawUtf8 = ''; const default: string = ''): boolean; overload;
    /// search for "-parametername" and return all string "parametervalue" occurrences
    function Get(const name: array of RawUtf8; out value: TStringDynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its plain string "parametervalue"
    function Get(const name: array of RawUtf8; out value: string;
      const description: RawUtf8 = ''; const default: string = ''): boolean; overload;
    /// search for "-parametername" and return all string "parametervalue" occurrences
    // - if name contains a & character, will also register the following char
    function Get(const name: RawUtf8; out value: TStringDynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    // - if name contains a & character, will also register the following char
    function Get(const name: RawUtf8; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: array of RawUtf8; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    // - if name contains a & character, will also register the following char
    function Get(const name: RawUtf8; min, max: integer; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: array of RawUtf8; min, max: integer;
      out value: integer; const description: RawUtf8 = '';
      default: integer = -1): boolean; overload;
    /// search for "-parametername" parameter in Names[]
    // - if name contains a & character, will also search the following char
    function Has(const name: RawUtf8): boolean; overload;
    /// search for "-parametername" parameter in Names[]
    function Has(const name: array of RawUtf8): boolean; overload;
    /// search for "-parametername" and return '' or its RawUtf8 "parametervalue"
    // - if name contains a & character, will also register the following char
    function Param(const name: RawUtf8; const description: RawUtf8 = '';
      const default: RawUtf8 = ''): RawUtf8; overload;
    /// search for "-parametername" and return '' or its string "parametervalue"
    // - if name contains a & character, will also register the following char
    function ParamS(const name: RawUtf8; const description: RawUtf8 = '';
      const default: string = ''): string; overload;
    /// search for "-parametername" and return '' or its RawUtf8 "parametervalue"
    function Param(const name: array of RawUtf8; const description: RawUtf8 = '';
      const default: RawUtf8 = ''): RawUtf8; overload;
    /// search for "-parametername" and return '' or its string "parametervalue"
    function ParamS(const name: array of RawUtf8; const description: RawUtf8 = '';
      const default: string = ''): string; overload;
    /// search for "-parametername" and return its integer "parametervalue" or default
    // - if name contains a & character, will also register the following char
    function Param(const name: RawUtf8; default: integer;
      const description: RawUtf8 = ''): integer; overload;
    /// search for "-parametername" and return its integer "parametervalue" or default
    function Param(const name: array of RawUtf8; default: integer;
      const description: RawUtf8 = ''): integer; overload;
    /// generate the text from all Arg() Options() and Get/Param() descriptions
    // and the supplied high-level description of the program
    // - the parameter <name> would be extracted from any #word in the
    // description text,
    // - for instance:
    // ! with Executable.Command do // or use a local variable
    // ! begin
    // !   ExeDescription := 'An executable to test mORMot Execute.Command';
    // !   verbose := Option('&verbose', 'generate verbose output');
    // !   Get(['t', 'threads'], threads, '#number of threads to run', 5);
    // !   ConsoleWrite(FullDescription);
    // ! end;
    // will fill "verbose" and "threads" local variables, and output on Linux:
    // $ An executable to test mORMot Execute.Command
    // $
    // $ Usage: mormot2tests [options] [params]
    // $
    // $ Options:
    // $   -v, --verbose       generate verbose output
    // $
    // $ Params:
    // $   -t, --threads <number> (default 5)
    // $                       number of threads to run
    function FullDescription(const customexedescription: RawUtf8 = '';
      const exename: RawUtf8 = ''; const onlyusage: RawUtf8 = ''): RawUtf8;
    /// check if the supplied parameters were all registered from previous
    // Arg() Options() and Get/Param() calls
    // - return '' if no unexpected flag has been supplied
    // - return an error message like 'Unexpected --name option' otherwise
    function DetectUnknown: RawUtf8;
    /// call DetectUnknown and output any error message to the console
    // - return false if the parameters are valid
    // - otherwise, return true and caller should exit the process
    function ConsoleWriteUnknown(const exedescription: RawUtf8 = ''): boolean;
    /// define 'h help' and call ConsoleWriteUnknown()
    // - caller should exit the process if this method returned true
    function ConsoleHelpFailed(const exedescription: RawUtf8 = ''): boolean;
    /// fill the stored arguments and options from executable parameters
    // - called e.g. at unit inialization to set Executable.CommandLine variable
    // - you can execute it again e.g. to customize the switches characters -
    // but to be done at startup, before any Option() or Param() methods
    function Parse(const DescriptionLineFeed: RawUtf8 = CRLF;
      const ShortSwitch: RawUtf8 = {$ifdef OSWINDOWS} '/' {$else} '-' {$endif};
      const LongSwitch: RawUtf8 = {$ifdef OSWINDOWS} '/' {$else} '--' {$endif}): boolean;
    /// remove all recognized arguments and switches
    procedure Clear;
    /// internal method returning a switch text from its identifier
    function SwitchAsText(const v: RawUtf8): RawUtf8;
    /// the ParamStr(1..ParamCount) arguments as RawUtf8, excluding Options[]
    // switches and Params[]/Values[] parameters
    property Args: TRawUtf8DynArray
      read fNames[clkArg];
    /// the "-optionname" boolean switches as stored in ParamStr()
    property Options: TRawUtf8DynArray
      read fNames[clkOption];
    /// the names of "-parametername parametervalue" as stored in ParamStr()
    // - mapping the Values[] associated array
    property Names: TRawUtf8DynArray
      read fNames[clkParam];
    /// the values of "-parametername parametervalue" as stored in ParamStr()
    // - mapping the Names[] associated array
    property Values: TRawUtf8DynArray
      read fValues;
    /// if search within Args[] Options[] or Names[] should be case-sensitive
    property CaseSensitiveNames: boolean
      read fCaseSensitiveNames write fCaseSensitiveNames;
    /// set a text which describes the executable
    // - as used by default by FullDescription() and ConsoleWriteUnknown()
    property ExeDescription: RawUtf8
      read fExeDescription write fExeDescription;
    /// DescriptionLineFeed value from TExecutableCommandLine.Parse()
    property LineFeed: RawUtf8
      read fLineFeed write fLineFeed;
    /// map ParamStr(1 .. ParamCount) values, encoded as RawUtf8
    // - may be used e.g. for regression tests instead of ParamStr():
    // ! c.RawParams := CsvToRawUtf8DynArray('-o file.txt --y -v -t 1', ' ');
    property RawParams: TRawUtf8DynArray
      read fRawParams write fRawParams;
  end;

  /// stores some global information about the current executable and computer
  // - as set at unit initialization into the Executable global variable
  TExecutable = record
    /// the main executable name, without any path nor extension
    // - e.g. 'Test' for 'c:\pathto\Test.exe'
    ProgramName: RawUtf8;
    /// the main executable details, as used e.g. by TSynLog
    // - e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.123 (2011-03-29 11:09:06)'
    // - you should have called GetExecutableVersion or SetExecutableVersion
    // to populate this field
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
    // - you should have called GetExecutableVersion or SetExecutableVersion
    // to populate this field
    Version: TFileVersion;
    /// the current computer host name
    Host: RawUtf8;
    /// the current computer user name
    User: RawUtf8;
    /// some hash representation of this information
    // - the very same executable on the very same computer run by the very
    // same user on the same OS should always have the same Hash value
    // - is computed from the crc32c of this TExecutable fields: c0 from
    // Version32, CpuFeatures and Host, c1 from User, c2 from ProgramFullSpec
    // and c3 from InstanceFileName
    // - may be used as an entropy seed, or to identify a process execution
    Hash: THash128Rec;
    /// the Command Line arguments, parsed during unit initialization
    Command: TExecutableCommandLine;
  end;

var
  /// system and process 256-bit entropy dual states
  // - could be used as 512-bit salt: followed by other system global variables
  SystemEntropy: record
    /// 128-bit of entropy quickly gathered during unit/process initialization
    // - not supposed to change during process execution
    Startup: THash128Rec;
    /// 128-bit shuffled each time strong randomness is retrieved from the OS
    // - set at startup e.g. from getauxval(AT_RANDOM) or CoCreateGuid()
    // - then e.g. by each FillSystemRandom/GetRawSmbios/LinuxGetRandom call
    // - together with the intangible Startup value, ensure forward secrecy
    LiveFeed: THash128Rec;
  end;

  /// the number of physical memory bytes available to the process
  // - equals TMemoryInfo.memtotal as retrieved from GetMemoryInfo() at startup
  SystemMemorySize: PtrUInt;

{$ifdef OSWINDOWS}

  /// the current System information, as retrieved for the current process
  // - under a WOW64 process, it will use the GetNativeSystemInfo() new API
  // to retrieve the real top-most system information
  // - note that the lpMinimumApplicationAddress field is replaced by a
  // more optimistic/realistic value ($100000 instead of default $10000)
  // - under BSD/Linux, only contain dwPageSize and dwNumberOfProcessors fields
  SystemInfo: TSystemInfo;
  /// the current Windows edition, as retrieved for the current process
  OSVersion: TWindowsVersion;
  /// is set to TRUE if the current process is a 32-bit image running under WOW64
  // - WOW64 is the x86 emulator that allows 32-bit Windows-based applications
  // to run seamlessly on 64-bit Windows
  // - equals always FALSE if the current executable is a 64-bit image
  IsWow64: boolean;
  /// is set to TRUE if the current process running through a software emulation
  // - e.g. a Win32/Win64 Intel application running via Prism on Windows for Arm
  IsWow64Emulation: boolean;
  /// low-level Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;

{$else OSWINDOWS}

  /// emulate only the most used fields of Windows' TSystemInfo
  SystemInfo: record
    /// retrieved from libc's getpagesize() - is expected to not be 0
    dwPageSize: cardinal;
    /// the number of available logical CPUs
    // - retrieved from HW_NCPU (BSD) or /proc/cpuinfo (Linux)
    // - see CpuSockets for the number of physical CPU sockets
    dwNumberOfProcessors: cardinal;
    /// meaningful system information, as returned by fpuname()
    uts: record
      sysname, release, version, nodename: RawUtf8;
    end;
    /// Linux Distribution release name, retrieved from /etc/*-release
    release: RawUtf8;
  end;

{$endif OSWINDOWS}

  /// global information about the current executable and computer
  // - this structure is initialized in this unit's initialization block below
  // but you need to call GetExecutableVersion to initialize its Version fields
  // from the executable version resource (if any)
  // - you can call SetExecutableVersion() with a custom version, if needed
  Executable: TExecutable;

  {$ifndef PUREMORMOT2}
  /// deprecated global: use Executable variable instead
  ExeVersion: TExecutable absolute Executable;
  {$endif PUREMORMOT2}

/// initialize Executable global variable, from the program version resources
// - is not retrieved at startup, unless this function is especially called
// - on POSIX, requires FPCUSEVERSIONINFO conditional to be set for the project
// - use SetExecutableVersion() if you want to force a custom version
// - is in fact just a wrapper around SetExecutableVersion(0, 0, 0, 0)
procedure GetExecutableVersion;

/// initialize Executable global variable with custom version numbers
// - GetExecutableVersion will retrieve version information from the
// executable itself (if it was included at build time and FPCUSEVERSIONINFO
// conditional was specified for the project)
// - but you can use this function to set any custom version number
procedure SetExecutableVersion(aMajor, aMinor, aRelease, aBuild: integer); overload;

/// initialize Executable global variable, supplying the version as text
// - e.g. SetExecutableVersion('7.1.2.512');
procedure SetExecutableVersion(const aVersionText: RawUtf8); overload;

/// return a function/method location according to the supplied code address
// - returns the address as hexadecimal by default, e.g. '4cb765'
// - if mormot.core.log.pas is defined in the project, will redirect to
// TDebugFile.FindLocationShort() method using .map/.dbg/.mab information, and
// return filename, symbol name and line number (if any) as plain text, e.g.
// '4cb765 ../src/core/mormot.core.base.pas statuscodeissuccess (11183)' on FPC
var
  GetExecutableLocation: function(aAddress: pointer): ShortString;

/// try to retrieve the file name of the executable/library holding a function
// - calls dladdr() on POSIX, or GetModuleFileName() on Windows
function GetExecutableName(aAddress: pointer): TFileName;

/// check if a function address is known within the main executable module
// - calls dladdr() on POSIX, or GetModuleHandleEx() on Windows
function IsMainExecutable(aAddress: pointer): boolean;

var
  /// retrieve the MAC addresses of all hardware network adapters
  // - mormot.net.sock.pas will inject here its own cross-platform version
  // - this unit will include a simple parser of /sys/class/net/* for Linux only
  // - as used e.g. by GetComputerUuid() fallback if SMBIOS is not available
  GetSystemMacAddress: function: TRawUtf8DynArray;

type
  /// identify an operating system folder for GetSystemPath()
  // - on Windows, spCommonData maps e.g. 'C:\ProgramData',
  // spUserData points to 'C:\Users\<user>\AppData\Local',
  // spCommonDocuments to 'C:\Users\Public\Documents',
  // spUserDocuments to 'C:\Users\<user>\Documents',
  // spTemp will call GetTempPath() or read the $TEMP environment variable,
  // pointing typically to 'C:\Users\<user>\AppData\Local\Temp\',
  // and spLog either to '<exepath>\log' or
  // 'C:\Users\<user>\AppData\Local\<exename>-log' (the first writable)
  // - on POSIX, spTemp will use $TMPDIR/$TMP environment variables,
  // spCommonData, spCommonDocuments and spUserDocuments point to $HOME,
  // spUserData maps $XDG_CACHE_HOME or '$HOME/.cache' or '$TMP/<user>', and
  // spLog maps '/var/log/<exename>' or '<exepath>/log' or '$TMP/<exename>-log'
  // - on all systems, returned spTemp, spLog and spUserData folders are always
  // writable by the current user
  TSystemPath = (
    spCommonData,
    spUserData,
    spCommonDocuments,
    spUserDocuments,
    spTemp,
    spLog);

{$ifndef PUREMORMOT2}
const
  spTempFolder = spTemp;
{$endif PUREMORMOT2}

/// returns an operating system folder
// - will return the full path of a given kind of private or shared folder,
// depending on the underlying operating system
// - will use SHGetFolderPath and the corresponding CSIDL constant under Windows
// - under POSIX, will return the proper environment variable
// - spLog is a writable sub-folder specific to mORMot, always created if needed
// - returned folder name contains the trailing path delimiter (\ or /)
function GetSystemPath(kind: TSystemPath): TFileName;

/// force an operating system folder
// - if the default location is not good enough for your project
// - will just check that the directory exists, not that it is writable
function SetSystemPath(kind: TSystemPath; const path: TFileName): boolean;

type
  /// identify the (Windows) system certificate stores for GetSystemStoreAsPem()
  // - ignored on POSIX systems, in which the main cacert.pem file is used
  // - scsCA contains known Certification Authority certificates, i.e. from
  // entities entrusted to issue certificates that assert that the recipient
  // individual, computer, or organization requesting the certificate fulfills
  // the conditions of an established policy
  // - scsMY holds certificates with associated private keys (Windows only)
  // - scsRoot contains known Root certificates, i.e. self-signed CA certificates
  // which are the root of the whole certificates trust tree
  // - scsSpc contains Software Publisher Certificates (Windows only)
  TSystemCertificateStore = (
    scsCA,
    scsMY,
    scsRoot,
    scsSpc);
  TSystemCertificateStores = set of TSystemCertificateStore;

var
  /// the local PEM file name to be searched by GetSystemStoreAsPem() to
  // override the OS certificates store
  // - a relative file name (i.e. with no included path, e.g. 'cacert.pem') will
  // be searched in the Executable.ProgramFilePath folder
  // - an absolute file name (e.g. 'C:\path\to\file.pem' or '/posix/path') could
  // also be specified
  // - set by default to '' to disable this override (for security purposes)
  GetSystemStoreAsPemLocalFile: TFileName;

/// retrieve the OS certificates store as PEM text
// - first search for [Executable.ProgramFilePath+]GetSystemStoreAsPemLocalFile,
// then for a file pointed by a 'SSL_CA_CERT_FILE' environment variable - unless
// OnlySystemStore is forced to true
// - if no such file exists, or if OnlySystemStore is true, will concatenate the
// supplied CertStores values via individual GetOneSystemStoreAsPem() calls
// - return CA + ROOT certificates by default, ready to validate a certificate
// - Darwin specific API is not supported yet, and is handled as a BSD system
// - an internal cache is refreshed every 4 minutes unless FlushCache is set
function GetSystemStoreAsPem(
  CertStores: TSystemCertificateStores = [scsCA, scsRoot];
  FlushCache: boolean = false; OnlySystemStore: boolean = false): RawUtf8;

/// retrieve all certificates of a given system store as PEM text
// - on Windows, will use the System Crypt API
// - on POSIX, scsRoot loads the main CA file of the known system file, and
// scsCA the additional certificate files which may not be part of the main file
// - GetSystemStoreAsPemLocalFile file and 'SSL_CA_CERT_FILE' environment
// variables are ignored: call GetSystemStoreAsPem() instead for the global store
// - an internal cache is refreshed every 4 minutes unless FlushCache is set
function GetOneSystemStoreAsPem(CertStore: TSystemCertificateStore;
  FlushCache: boolean = false; now: cardinal = 0): RawUtf8;

var
  /// low-level function used by StuffExeCertificate() in mormot.misc.pecoff.pas
  // - properly implemented by mormot.crypt.openssl.pas, but mormot.misc.pecoff
  // has its own stand-alone version using a pre-generated fixed certificate
  // - warning: the Marker should have no 0 byte within
  CreateDummyCertificate: function(const Stuff, CertName: RawUtf8;
    Marker: cardinal): RawByteString;

var
  /// allow half a day margin when checking a Certificate date validity
  // - this global setting is used as default for all our units
  CERT_DEPRECATION_THRESHOLD: TDateTime = 0.5;

type
  /// the raw SMBIOS information as filled by GetRawSmbios
  // - first 4 bytes are $010003ff on POSIX if read from /var/tmp/.synopse.smb
  TRawSmbiosInfo = record
    /// some flag only set by GetSystemFirmwareTable() Windows API
    Reserved: byte;
    /// typically 2-3
    SmbMajorVersion: byte;
    /// typically 0-1
    SmbMinorVersion: byte;
    /// typically 0 for SMBIOS 2.1, 1 for SMBIOS 3.0
    DmiRevision: byte;
    /// the length of encoded binary in data
    Length: DWord;
    /// low-level binary of the SMBIOS Structure Table
    Data: RawByteString;
  end;

var
  /// global variable filled by GetRawSmbios from SMBIOS binary information
  RawSmbios: TRawSmbiosInfo;

/// retrieve the SMBIOS raw information as a single RawSmbios gloabl binary blob
// - will try the Windows API if available, or search and parse the main system
// memory with UEFI redirection if needed - via /systab system file on Linux, or
// kenv() on FreeBSD (only fully tested to work on Windows XP+ and Linux)
// - follow DSP0134 3.6.0 System Management BIOS (SMBIOS) Reference Specification
// with both SMBIOS 2.1 (32-bit) or SMBIOS 3.0 (64-bit) entry points
// - the current user should have enough rights to read the main system memory,
// which means it should be root on most POSIX Operating Systems - so we persist
// this raw binary in /var/tmp/.synopse.smb to retrieve it from non-root user
function GetRawSmbios: boolean;

type
  /// the basic SMBIOS fields supported by GetSmbios/DecodeSmbios functions
  // - only include the first occurrence for board/cpu/battery types
  // - see TSmbiosInfo in mormot.core.perf.pas for more complete decoding
  TSmbiosBasicInfo = (
    sbiUndefined,
    sbiBiosVendor,
    sbiBiosVersion,
    sbiBiosFirmware,
    sbiBiosRelease,
    sbiBiosDate,
    sbiManufacturer,
    sbiProductName,
    sbiVersion,
    sbiSerial,
    sbiUuid,
    sbiSku,
    sbiFamily,
    sbiBoardManufacturer,
    sbiBoardProductName,
    sbiBoardVersion,
    sbiBoardSerial,
    sbiBoardAssetTag,
    sbiBoardLocation,
    sbiCpuManufacturer,
    sbiCpuVersion,
    sbiCpuSerial,
    sbiCpuAssetTag,
    sbiCpuPartNumber,
    sbiBatteryLocation,
    sbiBatteryManufacturer,
    sbiBatteryName,
    sbiBatteryVersion,
    sbiBatteryChemistry,
    sbiOem
  );

  /// the text fields stored by GetSmbios/DecodeSmbios functions
  TSmbiosBasicInfos = array[TSmbiosBasicInfo] of RawUtf8;

/// decode basic SMBIOS information as text from a TRawSmbiosInfo binary blob
// - see DecodeSmbiosInfo() in mormot.core.perf.pas for a more complete decoder
// - returns the total size of DMI/SMBIOS information in raw.data (may be lower)
// - will also adjust raw.Length and truncate raw.Data to the actual useful size
function DecodeSmbios(var raw: TRawSmbiosInfo; out info: TSmbiosBasicInfos): PtrInt;

// some global definitions for proper caching and inlining of GetSmbios()
procedure ComputeGetSmbios;
procedure DecodeSmbiosUuid(src: PGuid; out dest: RawUtf8; const raw: TRawSmbiosInfo);
var
  _Smbios: TSmbiosBasicInfos;
  _SmbiosRetrieved: boolean;

  /// customize how DecodeSmbiosUuid() handle endianess of its first bytes
  // - sduDirect will directly use GUIDToString() layout (seems expected on
  // Windows to match "wmic csproduct get uuid" value)
  // - sduInvert will force first values inversion (mandatory on MacOS)
  // - sduVersion will invert for SMBios version < 2.6 (set outside Windows)
  _SmbiosDecodeUuid: (sduDirect, sduInvert, sduVersion)
    {$ifdef OSDARWIN}  = sduInvert  {$else}
      {$ifdef OSPOSIX} = sduVersion {$endif} {$endif};

/// retrieve SMBIOS information as text
// - only the main values are decoded - see GetSmbiosInfo in mormot.core.perf
// for a more complete DMI/SMBIOS decoder
// - on POSIX, requires root to access full SMBIOS information - will fallback
// reading /sys/class/dmi/id/* on Linux or kenv() on FreeBSD for most entries
// if we found no previous root-retrieved cache in local /var/tmp/.synopse.smb
// - see _SmbiosDecodeUuid global flag for UUID decoding
function GetSmbios(info: TSmbiosBasicInfo): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

type
  /// define how GetComputerUuid() computes its 128-bit UUID identifier
  // - set of potential sources, which may be excluded from computation
  TGetComputerUuid = set of (
    gcuSmbios,
    gcuSmbiosData,
    gcuSmbiosText,
    gcuCpuFeatures,
    gcuCpuInfoText,
    gcuBiosInfoText,
    gcuMacAddress);

/// retrieve a genuine 128-bit UUID identifier for this computer
// - first try GetSmbios(sbiUuid), i.e. the SMBIOS System UUID
// - otherwise, will compute a genuine hash from known hardware information
// (CPU, Bios, MAC) and store it in a local file for the next access (if
// disable is void), e.g. into '/var/tmp/.synopse.uid' on POSIX
// - on Mac, include the mormot.core.os.mac unit to properly read this UUID
// - note: some BIOS have no UUID, so we fallback to our hardware hash on those
// - you can specify some HW sources to be ignored during the calculation
procedure GetComputerUuid(out uuid: TGuid; disable: TGetComputerUuid = []); overload;

/// retrieve a genuine 128-bit UUID identifier for this computer
// - returns GetSmbios(sbiUuid) if available
// - if no GetSmbios(sbiUuid) is available, will compute a genuine hash from HW
// and store it on a local file for the next access (if disable is void)
// - you can specify some HW sources to be ignored during the calculation
function GetComputerUuid(disable: TGetComputerUuid = []): RawUtf8; overload;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{$ifdef OSWINDOWS}

type
  // publish basic WinAPI types to avoid including "Windows" in our uses clause
  TMessage      = Messages.TMessage;
  HWND          = Windows.HWND;
  BOOL          = Windows.BOOL;
  LARGE_INTEGER = Windows.LARGE_INTEGER;
  TFileTime     = Windows.FILETIME;
  PFileTime     = ^TFileTime;

  /// Windows handle for a Thread - for cross-platform/cross-compiler clarity
  // - note that on POSIX TThreadID is a pointer and not a 32-bit file handle
  TThreadID = DWord;
  /// a TThreadID-sized unsigned integer, to ease TThreadID alignment
  TThreadIDInt = cardinal;

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
  {$ifdef USERECORDWITHMETHODS}
  TWinRegistry = record
  {$else}
  TWinRegistry = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the opened HKEY handle
    key: HKEY;
    /// start low-level read access to a Windows Registry node
    // - on success (returned true), Close method should be eventually called
    function ReadOpen(root: TWinRegistryRoot; const keyname: RawUtf8;
      closefirst: boolean = false): boolean;
    /// finalize low-level read access to the Windows Registry after ReadOpen()
    procedure Close;
    /// read a UTF-8 string from the Windows Registry after ReadOpen()
    // - in respect to Delphi's TRegistry, will properly handle REG_MULTI_SZ
    // (return the first value of the multi-list) - use ReadData to retrieve
    // all REG_MULTI_SZ values as one blob
    // - we don't use string here since it would induce a dependency to
    // mormot.core.unicode and UTF-8 is needed on Delphi 7/2007
    function ReadString(const entry: SynUnicode; andtrim: boolean = true): RawUtf8;
    /// read a Windows Registry content after ReadOpen()
    // - works with any kind of key, but was designed for REG_BINARY
    function ReadData(const entry: SynUnicode): RawByteString;
    /// read a Windows Registry 32-bit REG_DWORD value after ReadOpen()
    function ReadDword(const entry: SynUnicode): cardinal;
    /// read a Windows Registry 64-bit REG_QWORD value after ReadOpen()
    function ReadQword(const entry: SynUnicode): QWord;
    /// read a Windows Registry content as binary buffer after ReadOpen()
    // - just a wrapper around RegQueryValueExW() API call
    function ReadBuffer(const entry: SynUnicode; data: pointer; datalen: DWord): boolean;
    /// read a Windows Registry content as length-specified buffer after ReadOpen()
    // - returns the number of bytes written to Data
    function ReadMax(const entry: SynUnicode; data: pointer; maxdatalen: DWord): DWord;
    /// retrieve a Windows Registry content size as binary bytes after ReadOpen()
    // - returns -1 if the entry is not found
    function ReadSize(const entry: SynUnicode): integer;
    /// enumeration of all sub-entries names of a Windows Registry key
    function ReadEnumEntries: TRawUtf8DynArray;
  end;

/// rough detection of 'c:\windows' and 'c:\program files' folders
function IsSystemFolder(const Folder: TFileName): boolean;

// check if a folder may be affected by UAC folder virtualization
// - on Win32 Vista+, detects 'c:\windows' and 'c:\program files' UAC folders
// - returns always false on Win64
function IsUacVirtualFolder(const Folder: TFileName): boolean;
  {$ifdef CPU64} inline; {$endif}

/// check if UAC folder/registry virtualization is enabled for this process
// - returns always false on Win64 - by design
// - calls GetTokenInformation(TokenVirtualizationEnabled) on Win32
// - if you include {$R src\mormot.win.default.manifest.res} in your project,
// UAC virtualization is disabled and this function returns false
function IsUacVirtualizationEnabled: boolean;
  {$ifdef CPU64} inline; {$endif}

/// quickly retrieve a Text value from Registry
// - could be used if TWinRegistry is not needed, e.g. for a single value
function ReadRegString(Key: THandle; const Path, Value: string): string;

/// convenient late-binding of any external library function
// - thread-safe wrapper around LoadLibray + GetProcAddress once over a pointer
function DelayedProc(var api; var lib: THandle;
  libname: PChar; procname: PAnsiChar): boolean;

const
  /// Windows file APIs have hardcoded MAX_PATH = 260 :(
  // - but more than 260 chars are possible with the \\?\..... prefix
  // or by disabling the limitation in registry since Windows 10, version 1607
  // https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
  // - extended-length path allows up to 32,767 widechars in theory, but 2047
  // widechars seems big enough in practice e.g. with NTFS - POSIX uses 4096
  W32_MAX = 2047;

type
  /// 4KB stack buffer for no heap allocation during UTF-16 path encoding or
  // switch to extended-length on > MAX_PATH up to 2047 widechars
  TW32Temp = array[0 .. W32_MAX] of WideChar;

/// efficiently return a PWideChar from a TFileName on all compilers
// - without any memory allocation, and with proper Unicode support
// - is also able to handle FileName with length > MAX_PATH, up to 2048 chars
// - all the low-level file functions of this unit (e.g. FileCreate or FileOpen)
// will use this function to support file names longer than MAX_PATH
function W32(const FileName: TFileName; var Temp: TW32Temp; DoCopy: boolean = false): PWideChar;

const
  NO_ERROR  = Windows.NO_ERROR; // = ERROR_SUCCESS

  ERROR_ACCESS_DENIED       = Windows.ERROR_ACCESS_DENIED;
  ERROR_INVALID_HANDLE      = Windows.ERROR_INVALID_HANDLE;
  ERROR_INSUFFICIENT_BUFFER = Windows.ERROR_INSUFFICIENT_BUFFER;
  ERROR_INVALID_PARAMETER   = Windows.ERROR_INVALID_PARAMETER;
  ERROR_HANDLE_EOF          = Windows.ERROR_HANDLE_EOF;
  ERROR_ALREADY_EXISTS      = Windows.ERROR_ALREADY_EXISTS;
  ERROR_MORE_DATA           = Windows.ERROR_MORE_DATA;
  ERROR_CONNECTION_INVALID  = Windows.ERROR_CONNECTION_INVALID;
  ERROR_OLD_WIN_VERSION     = Windows.ERROR_OLD_WIN_VERSION;
  ERROR_IO_PENDING          = Windows.ERROR_IO_PENDING;
  ERROR_OPERATION_ABORTED   = Windows.ERROR_OPERATION_ABORTED;

  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770
  ERROR_WINHTTP_TIMEOUT                 = 12002;
  ERROR_WINHTTP_OPERATION_CANCELLED     = 12017;
  ERROR_WINHTTP_CANNOT_CONNECT          = 12029;
  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = 12044;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;
  ERROR_MUI_FILE_NOT_FOUND              = 15100;

  INVALID_HANDLE_VALUE = Windows.INVALID_HANDLE_VALUE; // = HANDLE(-1)
  ENGLISH_LANGID       = $0409;

const
  WINDOWS_CERTSTORE: array[TSystemCertificateStore] of PWideChar = (
    'CA', 'MY', 'ROOT', 'SPC');

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

/// retrieves the current executable library handle, i.e. its memory load address
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetModuleHandle(lpModuleName: PChar): HMODULE;

/// post a message to the Windows message queue
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;

/// retrieves the current stack trace
// - only available since Windows XP
// - FramesToSkip + FramesToCapture should be <= 62
function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: cardinal;
  BackTrace, BackTraceHash: pointer): byte; stdcall;

/// retrieves the current thread ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentThreadId: DWord; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentProcessId: DWord; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentProcess: THandle; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function WaitForSingleObject(hHandle: THandle; dwMilliseconds: DWord): DWord; stdcall;

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

/// initialize Windows IOCP instance
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpCreate(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey: pointer; NumberOfConcurrentThreads: DWord): THandle; stdcall;

/// retrieve Windows IOCP instance status
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpGetQueuedStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWord; var lpCompletionKey: pointer;
  var lpOverlapped: pointer; dwMilliseconds: DWord): BOOL; stdcall;

/// trigger a Windows IOCP instance
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpPostQueuedStatus(CompletionPort: THandle;
  NumberOfBytesTransferred: DWord; dwCompletionKey: pointer;
  lpOverlapped: POverlapped): BOOL; stdcall;

/// finalize a Windows resource (e.g. IOCP instance)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function CloseHandle(hObject: THandle): BOOL; stdcall;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
// - also supports aFileName longer than MAX_PATH
// - on Windows, aRights parameter is just ignored, and on POSIX aRights = 0
// will set the default octal 644 file access attributes (-rw-r-r--)
// - warning: this function replaces ALL SysUtils.FileCreate() overloads,
// putting aMode as the SECOND parameter, just like with FileOpen()
function FileCreate(const aFileName: TFileName; aMode: integer = 0;
  aRights: integer = 0): THandle;

/// redefined here to call CreateFileW() on non-Unicode RTL and support
// aFileName longer than MAX_PATH
function FileOpen(const aFileName: TFileName; aMode: integer): THandle;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
procedure FileClose(F: THandle); stdcall;

/// redefined here to avoid warning to include "Windows" in uses clause
// and support FileName longer than MAX_PATH
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function RenameFile(const OldName, NewName: TFileName): boolean;

/// redirection to Windows SetFileTime() of a file name from Int64(TFileTime)
// - if any Int64 is 0, the proper value will be guess from the non-0 values
function FileSetTime(const FileName: TFileName;
  const Created, Accessed, Written: Int64): boolean;

/// defined here to call the GetFullPathNameW() Windows API
// - this function may convert to extended-length path if needed
// - instead of the "manual" file name expansion of the FPC RTL
// - also for consistency with Delphi on Windows
function ExpandFileName(const FileName: TFileName): TFileName;

{$else}

/// redefined from FPC RTL sysutils for consistency
// - warning: this function replaces ALL SysUtils.FileCreate() overloads,
// putting aMode as the SECOND parameter, just like with FileOpen()
// - on POSIX, aRights = 0 will set default octal 644 attributes (-rw-r-r--)
function FileCreate(const aFileName: TFileName; aMode: integer = 0;
  aRights: integer = 0): THandle;

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

/// read /proc/pid/status to ensure pid is of a real process, not a thread
function IsValidPid(pid: cardinal): boolean;

type
  /// Low-level access to the ICU library installed on this system
  // - "International Components for Unicode" (ICU) is an open-source set of
  // libraries for Unicode support, internationalization and globalization
  // - ICU seems more complete and standard than FPC RTL iconv/cwstrings
  // - used by Unicode_CompareString, Unicode_AnsiToWide, Unicode_WideToAnsi,
  // Unicode_InPlaceUpper and Unicode_InPlaceLower function from this unit
  // - can maintain a thread-safe cache of up to 32 code page converters,
  // via SharedUcnv() and SharedUcnvUnLock()
  // - ICU is loaded only when needed outside of mORMot needs
  TIcuLibrary = record
  private
    icu, icudata, icui18n: pointer;
    fLoaded: boolean;
    procedure DoLoad(const LibName: TFileName = ''; Version: string = '');
    procedure Done;
  public
    /// Initialize an ICU text converter for a given encoding
    ucnv_open: function (converterName: PAnsiChar; var err: SizeInt): pointer; cdecl;
    /// finalize the ICU text converter for a given encoding
    ucnv_close: procedure (converter: pointer); cdecl;
    /// reset the ICU text converter for a given encoding
    ucnv_reset: procedure (converter: pointer); cdecl;
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
    /// return a shared ICU text converter instance
    // - the first call will initialize a shared instance for the whole process
    // - if nil is returned, regular ucnv() should be called with a local instance
    // - if <> nil is returned, SharedUcnvUnLock should eventually be called
    function SharedUcnv(codepage: cardinal; out ndx: PtrInt): pointer;
    /// release the SharedUcnv() instance
    procedure SharedUcnvUnLock(ndx: PtrInt);
  private
    // implement a thread-safe cache of up to 32 shared ICU text converters
    fSharedMainLock: PtrUInt; // = TLightLock
    fSharedCP:   array[0 .. 31] of word;    // CPU cache-friendly lookup
    fSharedLock: array[0 .. 31] of PtrUInt; // = TLightLock
    fSharedCnv:  array[0 .. 31] of pointer; // = ICU converter instance
    fSharedCount, fSharedLast: integer;
  end;

var
  /// low-level late-binding access to any installed ICU library
  // - typical use is to check icu.IsAvailable then the proper icu.*() functions
  // - this unit will make icu.Done in its finalization section
  icu: TIcuLibrary;

  /// contains the current POSIX kernel revision, as one 24-bit integer
  // - allow quick comparison mainly for kernel feature checking
  // - e.g. on Linux, may equal $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;


{$ifdef OSLINUX} { some Linux-specific APIs (e.g. systemd or eventfd) }

const
  /// The first passed file descriptor is fd 3
  SD_LISTEN_FDS_START = 3;

  /// low-level libcurl library file name, depending on the running OS
  LIBSYSTEMD_PATH = 'libsystemd.so.0';

type
  /// low-level systemd parameter to sd.journal_sendv() function
  TIoVec = record
    iov_base: PAnsiChar;
    iov_len: PtrUInt;
  end;

  /// implements late-binding of the systemd library
  // - about systemd: see https://www.freedesktop.org/wiki/Software/systemd
  // and http://0pointer.de/blog/projects/socket-activation.html - to get headers
  // on debian: `sudo apt install libsystemd-dev && cd /usr/include/systemd`
  TSystemD = record
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
    // - each structure should reference one field of the entry to submit
    // - the second argument specifies the number of structures in the array
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

/// a wrapper to the Linux eventfd() syscall
// - returns 0 if the kernel does not support eventfd2 (before 2.6.27) or if the
// platform is not validated yet (only Linux i386/x86_64/aarch64 by now)
// - returns a file descriptor handle on success, to be eventually closed
function LinuxEventFD(nonblocking, semaphore: boolean): integer;

/// wrapper to read from a eventfd() file
// - return 1 and decrement the counter by 1 in semaphore mode
// - return the current counter value and set it to 0 in non-semaphor mode
// - may be blocking or not blocking, depending on how LinuxEventFD() was called
// - return -1 on error
function LinuxEventFDRead(fd: integer): Int64;

/// wrapper to write to a eventfd() file
procedure LinuxEventFDWrite(fd: integer; count: QWord);

/// wrapper to wait for a eventfd() file read
// - return true if was notified for reading, or false on timeout
function LinuxEventFDWait(fd: integer; ms: integer): boolean; inline;

/// a wrapper to the Linux getrandom() syscall
// - returns false if the kernel is unsupported (before 3.17) or if the
// platform is not validated yet (only Linux i386, x86_64 and aarch64 by now)
// - used e.g. by function FillSystemRandom() if available, since it makes a
// single syscall, and /dev/urandom may be not available from some chroot
function LinuxGetRandom(buf: pointer; len: PtrInt): boolean;

/// a wrapper to the Linux prctl(PR_SET_NAME) syscall
// - returns false if the kernel is unsupported (before 2.6.9) or on failure
function LinuxSetProcessName(const NewName: RawUtf8): boolean;

{$endif OSLINUX}

var
  /// allow runtime-binding of complex OS API calls
  // - used e.g. by mormot.core.os.mac.pas to inject its own methods
  PosixInject: record
    GetSmbios: function(info: TSmbiosBasicInfo): RawUtf8;
    GetSmbiosData: function: RawByteString;
  end;

{$endif OSWINDOWS}


{ ****************** Unicode, Time, File, Console, Library process }

{$ifdef OSWINDOWS}

type
  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  // - warning: do not use this type directly, but rather TSynSystemTime as
  // defined in mormot.core.datetime which is really cross-platform, and has
  // consistent field order (FPC POSIX/Windows fields do not match!)
  TSystemTime = Windows.TSystemTime;
  PSystemTime = Windows.PSystemTime;

  /// system-specific type returned by FileAge(): local 32-bit bitmask on Windows
  TFileAge = integer;

{$ifdef ISDELPHI}

  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  TRTLCriticalSection = Windows.TRTLCriticalSection;

  /// defined as in FPC RTL, to avoid dependency to Windows.pas unit
  // - note that on POSIX, a THandle is a 32-bit integer, but library or
  // resource handles are likely to map pointers, i.e. up to a 64-bit integer
  TLibHandle = THandle;

{$endif ISDELPHI}

  /// handle for Slim Reader/Writer (SRW) locks in exclusive mode
  TOSLightMutex = pointer;

/// a wrapper calling SystemTimeToTzSpecificLocalTime Windows API
// - note: FileTimeToLocalFileTime is not to be involved here
// - only used by mormot.lib.static for proper SQLite3 linking on Windows
procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);

/// convert an Unix seconds time to a Win32 64-bit FILETIME value
procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);

/// convert an Unix milliseconds time to a Win32 64-bit FILETIME value
procedure UnixMSTimeToFileTime(I64: TUnixMSTime; out FT: TFileTime);

/// convert a TDateTime to a Win32 64-bit FILETIME value
procedure DateTimeToFileTime(dt: TDateTime; out FT: TFileTime);

/// convert a Win32 64-bit FILETIME value into an Unix seconds time
function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
  {$ifdef FPC} inline; {$endif}

/// convert a Win32 64-bit FILETIME value into a TDateTime
function FileTimeToDateTime(const FT: TFileTime): TDateTime;

/// convert a Win32 64-bit FILETIME value into an Unix milliseconds time
function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
  {$ifdef FPC} inline; {$endif}

/// detect if a file name starts with the long path '\\?\' prefix
// - https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
function IsExtendedPathName(const Name: TFileName): boolean;

var
  // Slim Reader/Writer (SRW) API exclusive mode - fallback to TLightLock on XP
  InitializeSRWLock,
  AcquireSRWLockExclusive,
  ReleaseSRWLockExclusive: procedure(var P: TOSLightMutex); stdcall;
  TryAcquireSRWLockExclusive: function (var P: TOSLightMutex): BOOL; stdcall;

{$else}

const
  /// a cross-platform incorrect THandle value, as defined in Windows unit
  INVALID_HANDLE_VALUE = THandle(-1);

  /// allow to assign proper signed symbol table name for a libc.so.6 method
  {$ifdef OSLINUXX64}
  LIBC_SUFFIX = '@GLIBC_2.2.5';
  {$else}
  {$ifdef OSLINUXX86}
  LIBC_SUFFIX = '@GLIBC_2.0';
  {$else}
  LIBC_SUFFIX = ''; // no suffix seems needed outside of Intel/AMD systems
  {$endif OSLINUXX86}
  {$endif OSLINUXX64}

{$undef HAS_OSPTHREADS}
{$ifdef OSLINUX}
  {$define OSPTHREADSLIB}    // direct pthread calls were tested on Linux only
{$endif OSLINUX}
{$ifdef OSDARWIN}
  {$define OSPTHREADSSTATIC} // direct pthread calls from the 'c' library
{$endif OSDARWIN}
{$ifdef OSBSD}
  {$define OSPTHREADSSTATIC} // direct pthread calls from the c library
{$endif OSBSD}

// some pthread_mutex_*() API defined here for proper inlining
{$ifdef OSPTHREADSLIB}
{$define HAS_OSPTHREADS}
var
  {%H-}pthread: pointer; // access to pthread.so e.g. for mormot.lib.static
  pthread_mutex_lock:    function(mutex: pointer): integer; cdecl;
  pthread_mutex_trylock: function(mutex: pointer): integer; cdecl;
  pthread_mutex_unlock:  function(mutex: pointer): integer; cdecl;
{$endif OSPTHREADSLIB}
{$ifdef OSPTHREADSSTATIC}
{$define HAS_OSPTHREADS}
function pthread_mutex_lock(mutex: pointer): integer; cdecl;
function pthread_mutex_trylock(mutex: pointer): integer; cdecl;
function pthread_mutex_unlock(mutex: pointer): integer; cdecl;
{$endif OSPTHREADSSTATIC}

type
  /// system-specific type returned by FileAge(): UTC 64-bit Epoch on POSIX
  TFileAge = TUnixTime;

  {$ifdef FPC}
  /// system-specific structure holding a non-recursive mutex
  TOSLightMutex = TRTLCriticalSection;
  {$endif FPC}

{$endif OSWINDOWS}

/// raw cross-platform library loading function
// - alternative to LoadLibrary() and SafeLoadLibrary() Windows API and RTL
// - on Windows, set the SEM_NOOPENFILEERRORBOX and SEM_FAILCRITICALERRORS flags
// to avoid unexpected message boxes (which should not happen e.g. on a service)
// - on Win32, reset the FPU flags after load as required with some libraries
// - consider inheriting TSynLibrary if you want to map a set of API functions
function LibraryOpen(const LibraryName: TFileName): TLibHandle;

/// raw cross-platform library unloading function
// - alternative to FreeLibrary() Windows API and FPC RTL
procedure LibraryClose(Lib: TLibHandle);

/// raw cross-platform library resolution function, as defined in FPC RTL
// - alternative to GetProcAddr() Windows API and FPC RTL
function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// raw cross-platform library resolution error, e.g. after LibraryOpen
function LibraryError: string;


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

type
  /// a TThreadID-sized unsigned integer, to ease TThreadID alignment
  TThreadIDInt = PtrUInt;

{$ifndef OSLINUX} // try to stabilize MacOS/BSD pthreads API calls
  {$define NODIRECTTHREADMANAGER}
{$endif OSLINUX}

{$ifdef NODIRECTTHREADMANAGER} // try to stabilize MacOS pthreads API calls
function GetCurrentThreadId: TThreadID; inline;
function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer; inline;
procedure EnterCriticalSection(var cs: TRTLCriticalSection); inline;
procedure LeaveCriticalSection(var cs: TRTLCriticalSection); inline;
{$else}

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

{$endif NODIRECTTHREADMANAGER}

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

/// returns the current UTC time as TSystemTime from the OS
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// or fpgettimeofday() on Darwin/MacOS
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.FromNowUtc cross-platform method instead
procedure GetSystemTime(out result: TSystemTime);
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// returns the current Local time as TSystemTime from the OS
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// or fpgettimeofday() on Darwin/MacOS, with FPC RTL TZSeconds adjustment (so
// will be fixed for the whole process lifetime and won't change at daylight)
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.FromNowLocal cross-platform method instead
procedure GetLocalTime(out result: TSystemTime);
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API file truncate at current position
// or FpFtruncate() on POSIX
procedure SetEndOfFile(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API file flush to disk or FpFsync()
procedure FlushFileBuffers(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code or fpgeterrno
function GetLastError: integer;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// check if the last error reporting by the system is a file access violation
// - call GetLastError is no ErrorCode is supplied
function IsSharedViolation(ErrorCode: integer = 0): boolean;

/// compatibility function, wrapping Win32 API last error code
procedure SetLastError(error: integer);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns a given error code as plain text
// - redirects to WinApiErrorShort(error, nil) on Windows, or StrError() on POSIX
// - e.g. GetErrorText(10) = 'ECHILD (No child processes)' on Linux
function GetErrorText(error: integer = 0): RawUtf8;

/// returns a given error code as plain text ShortString
function GetErrorShort(error: integer = 0): ShortString;

/// returns a given error code as plain text ShortString
procedure GetErrorShortVar(error: integer; var dest: ShortString);

{$ifdef OSWINDOWS}

/// return the error message - maybe of a given library - as generic string
// - may be used e.g. in conjunction with Exception.CreateFmt()
// - if optional HMODULE does not support this Code, will try as System error
// - first try WinErrorConstant() for system error constants, then call
// FormatMessage() ensuring the ENGLISH_LANGID flag is used first
// - replace SysErrorMessagePerModule() and SysErrorMessage() from mORMot 1
function WinApiErrorString(Code: cardinal; Lib: HMODULE = 0): string;

/// return the error message - maybe of a given library - as UTF-8 ShortString
// - may be used e.g. in conjunction with Exception.CreateUtf8()
function WinApiErrorShort(Code: cardinal; Lib: HMODULE = 0): shortstring;

/// return the error message - maybe of a given library - as UTF-8 string
function WinApiErrorUtf8(Code: cardinal; Lib: HMODULE = 0): RawUtf8;

/// raise an EOSException from the last system error using WinApiErrorString()
// - if Code is kept to its default 0, GetLastError is called
procedure RaiseLastError(const Context: ShortString;
  RaisedException: ExceptClass = nil; Code: integer = 0);

/// return a RaiseLastError-like error message using WinApiErrorString()
// - if Code is kept to its default 0, GetLastError is called
function WinLastError(const Context: ShortString; Code: integer = 0): string;

/// call RaiseLastError(Code) if Code <> NO_ERROR = ERROR_SUCCESS
procedure WinCheck(const Context: ShortString; Code: integer;
  RaisedException: ExceptClass = nil);
  {$ifdef HASINLINE} inline; {$endif}

/// raise an Exception from the last library error using WinApiErrorString()
procedure RaiseLastModuleError(const Context: ShortString; Lib: HMODULE;
  ModuleException: ExceptClass);

{$else}
/// set the current system time as UTC timestamp
// - we define two functions with diverse signature to circumvent the FPC RTL
// TSystemTime field issue - Windows version is in mormot.core.os.security
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.ChangeOperatingSystemTime cross-platform method instead
function SetSystemTime(utctime: TUnixTime): boolean;

{$endif OSWINDOWS}

/// compatibility function, wrapping Win32 API function
// - returns the current main Window handle on Windows, or 0 on POSIX/Linux
function GetDesktopWindow: PtrUInt;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns the curent system code page for AnsiString types
// - as used to initialize CurrentAnsiConvert in mormot.core.unicode unit
// - calls GetACP() Win32 API value on Delphi, or DefaultSystemCodePage on FPC -
// i.e. GetSystemCodePage() on POSIX (likely to be CP_UTF8) or the value used
// by the LCL for its "string" types (also typically be CP_UTF8 even on Windows)
function Unicode_CodePage: integer;
  {$ifdef FPC} inline; {$endif}

/// compatibility function, wrapping CompareStringW() Win32 API text comparison
// - returns 1 if PW1>PW2, 2 if PW1=PW2, 3 if PW1<PW2 - so substract 2 to have
// -1,0,1 as regular StrCompW/StrICompW comparison function result
// - will compute StrLen(PW1/PW2) if L1 or L2 < 0
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - in practice, is hardly called, unless our proprietary WIN32CASE collation
// is used in mormot.db.raw.sqlite3, or via Utf8CompareOS() or Utf8CompareIOS()
// functions from mormot.core.unicode
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
// - used when mormot.core.unicode is an overkill, e.g. SDDL generation
// - calls IsAnsiCompatibleW() first to quickly handle pure ASCII-7 content
procedure Unicode_WideToShort(W: PWideChar; LW, CodePage: PtrInt;
  var res: ShortString);

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

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
// - returns dest.buf as result, and dest.len as length in WideChar (not bytes)
// - caller should always call Dest.Done to release any (unlikely) allocated memory
function Unicode_FromUtf8(Text: PUtf8Char; TextLen: PtrInt;
  var Dest: TSynTempBuffer): PWideChar;

/// return a code page number into ICU-compatible charset name
// - Unicode_CodePageName(932) returns e.g. 'SHIFT_JIS'
// - Unicode_CodePageName(1251) returns 'MS1251' since 'CP####' is used
// for IBM code pages by ICU - which do not match Windows code pages
procedure Unicode_CodePageName(CodePage: cardinal; var Name: ShortString);

/// returns a system-wide current monotonic timestamp as milliseconds
// - will use the corresponding native API function under Vista+, or will be
// redirected to a custom wrapper function for older Windows versions (XP)
// to avoid the 32-bit overflow/wrapping issue of GetTickCount
// - warning: FPC's SysUtils.GetTickCount64 or TThread.GetTickCount64 don't
// handle properly 49.7 days wrapping under XP -> always use this safe version
// - warning: FPC's SysUtils.GetTickCount64 may call fpgettimeofday() e.g.
// on Darwin, which is not monotonic -> always use this more coherent version
// - on POSIX, will call (via vDSO) the very fast CLOCK_MONOTONIC_COARSE if
// available, or the low-level mach_absolute_time() monotonic Darwin API
// - do not expect exact millisecond resolution - steps may rather be e.g.
// within the 15-16 ms range on Windows, and 4-5 ms range on Linux
{$ifdef OSWINDOWS}
var
  GetTickCount64: function: Int64; stdcall;
{$else}
function GetTickCount64: Int64;
{$endif OSWINDOWS}

/// returns a system-wide current monotonic timestamp as 32-bit seconds
// - simply wrap GetTickCount64 div 1000 on Windows/Mac, or call clock_gettime()
// and return directly its timespec.tv_sec part on Linux/BSD
// - overflow after 136 years when compared as 32-bit, or 68 years as 31-bit
function GetTickSec: cardinal;
  {$ifdef OSWINDOWS} {$ifdef HASINLINE} inline; {$endif} {$endif}

/// returns how many seconds the system was up, accouting for time when
// the computer is asleep, i.e. the time elapsed on the wall clock
// - on Windows, computes GetTickCount64 div 1000
// - on Linux/BSD, will use CLOCK_BOOTTIME/CLOCK_UPTIME clock
// - on MacOS, will use mach_continuous_time() API
// - overflow after 136 years when compared as 32-bit, or 68 years as 31-bit
function GetUptimeSec: cardinal;

/// returns the current UTC time
// - wrap UnixMSTimeUtcFast, so use e.g. clock_gettime(CLOCK_REALTIME_COARSE)
// under Linux, or GetSystemTimeAsFileTime under Windows
function NowUtc: TDateTime;

/// returns the current UTC date/time as a second-based c-encoded time
// - i.e. current number of seconds elapsed since Unix epoch 1/1/1970
// - use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime under Windows
// - returns a 64-bit unsigned value, so is "Year2038bug" free - internal
// 32-bit storage may use either proper unsigned cardinal (so overflow in
// 2106) or most likely the TUnixTimeMinimal epoch (valid up to 2152)
function UnixTimeUtc: TUnixTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - i.e. current number of milliseconds elapsed since Unix epoch 1/1/1970
// - will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimePreciseAsFileTime under Windows 8 and later
// - on Windows, is slightly more accurate, but slower than UnixMSTimeUtcFast
function UnixMSTimeUtc: TUnixMSTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - under Linux/POSIX, is the very same than UnixMSTimeUtc (inlined call)
// - under Windows 8+, will call GetSystemTimeAsFileTime instead of
// GetSystemTimePreciseAsFileTime, which has higher precision, but is slower
// - prefer it under Windows, if a dozen of ms resolution is enough for your task
function UnixMSTimeUtcFast: TUnixMSTime;
  {$ifdef OSPOSIX} inline; {$endif}

const
  /// number of days offset between the Unix Epoch (1970) and TDateTime origin
  UnixDelta = 25569;
  /// number of Windows TFileTime ticks (100ns) from year 1601 to 1970
  UnixFileTimeDelta = 116444736000000000;

/// the number of minutes bias in respect to UTC/GMT date/time
// - as retrieved via -GetLocalTimeOffset() at startup, so may not be accurate
// after a time shift during the process execution - but any long-running
// process (like a service) should use UTC timestamps only
var
  TimeZoneLocalBias: integer;

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// calling context when intercepting exceptions
  // - used e.g. for TSynLogExceptionToStr or RawExceptionIntercept() handlers
  {$ifdef USERECORDWITHMETHODS}
  TSynLogExceptionContext = record
  {$else}
  TSynLogExceptionContext = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the raised exception class
    EClass: ExceptClass;
    /// the Delphi Exception instance
    // - may be nil for external/OS exceptions
    EInstance: Exception;
    /// the OS-level exception code
    // - could be $0EEDFAE0 of $0EEDFADE for Delphi-generated exceptions
    ECode: DWord;
    /// = FPC's RaiseProc() FrameCount if EStack is Frame: PCodePointer
    EStackCount: integer;
    /// the address where the exception occurred
    EAddr: PtrUInt;
    /// the optional stack trace
    EStack: PPtrUIntArray;
    /// timestamp of this exception, as number of seconds since UNIX Epoch
    // - UnixTimeUtc is faster than NowUtc or GetSystemTime
    // - use UnixTimeToDateTime() to convert it into a regular TDateTime
    ETimestamp: TUnixTime;
    /// the logging level corresponding to this exception
    // - may be either sllException or sllExceptionOS
    ELevel: TSynLogLevel;
    {$ifdef OSWINDOWS}
    /// retrieve some DotNet CLR extended information about a given Exception
    function AdditionalInfo(var dest: shortstring): boolean;
    {$endif OSWINDOWS}
  end;

  /// the global function signature expected by RawExceptionIntercept()
  // - assigned e.g. to SynLogException() in mormot.core.log.pas
  TOnRawLogException = procedure(const Ctxt: TSynLogExceptionContext);

/// setup Exception interception for the whole process
// - the first to call this procedure will be elected until the process ending
// - returns true on success, false if there is already an handler
function RawExceptionIntercept(const Handler: TOnRawLogException): boolean;

{$endif NOEXCEPTIONINTERCEPT}

/// returns a high-resolution system-wide monotonic timestamp as microseconds
// - under Linux/POSIX, has true microseconds resolution, calling e.g.
// CLOCK_MONOTONIC on Linux/BSD
// - under Windows, calls QueryPerformanceCounter / QueryPerformanceFrequency
procedure QueryPerformanceMicroSeconds(out Value: Int64);

/// cross-platform check if the supplied THandle is not invalid
function ValidHandle(Handle: THandle): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// faster cross-platform alternative to sysutils homonymous function
// - on Windows, will support FileName longer than MAX_PATH
// - FPC on Windows is not consistent with Delphi and the expected low-level API
// - warning: on both Windows and POSIX, this function would change the current
// default folder for the whole process, not just the current thread
function SetCurrentDir(const NewDir: TFileName): boolean;

/// faster cross-platform alternative to sysutils homonymous function
// - on Windows, will support FileName longer than MAX_PATH
// - CheckAsDir = true is used by DirectoryExists()
function FileExists(const FileName: TFileName; FollowLink: boolean = true;
  CheckAsDir: boolean = false): boolean;

/// faster cross-platform alternative to sysutils homonymous function
// - on Windows, will support FileName longer than MAX_PATH
function DirectoryExists(const FileName: TFileName;
  FollowLink: boolean = true): boolean;

/// check if this filename is in the 'c:\...' or '/full/path' pattern
function IsExpandedPath(const FileName: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a path
function SafePathName(const Path: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a RawUtf8 path
function SafePathNameU(const Path: RawUtf8): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a filename
function SafeFileName(const FileName: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a RawUtf8 filename
function SafeFileNameU(const FileName: RawUtf8): boolean;

/// ensure all \ / path delimiters are normalized into the current OS expectation
// - i.e. normalize file name to use '\' on Windows, or '/' on POSIX
// - see MakePath() from mormot.core.text.pas to concatenate path items
function NormalizeFileName(const FileName: TFileName): TFileName;

/// ensure all \ / path delimiters are normalized into the current OS expectation
// - this function works in-place on an UTF-8 string instance
procedure NormalizeFileNameU(var FileName: RawUtf8);

/// add some " before and after if FileName has some space within
// - could be used when generating command line parameters
function QuoteFileName(const FileName: TFileName): TFileName;

/// faster cross-platform alternative to sysutils homonymous function
// - on Windows, just redirect to WindowsFileTimeToDateTime() since FileDate
// is already expected to be in local time from FileAge()
// - on POSIX, FileDate is a 64-bit UTC value as returned from OS stat API, and
// will be converted into a local TDateTime
// - note: FPC FileAge(TDateTime) is wrong and truncates 1-2 seconds on Windows
function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
  {$ifdef HASINLINE}{$ifdef OSWINDOWS}inline;{$endif}{$endif}

/// get a file date and time, from its name
// - returns 0 if file doesn't exist
// - returns the local file age, encoded as TDateTime
// - under Windows, will use GetFileAttributesEx fast API
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// get a file date and time, from its name, as seconds since Unix Epoch
// - returns 0 if file (or folder if AllowDir is true) doesn't exist
// - returns the system API file age (not converted local), encoded as TUnixTime
// - under Windows, will use GetFileAttributesEx and FileTimeToUnixTime
// - under POSIX, will call directly the stat syscall
// - faster than FileAgeToDateTime() since don't convert to local time
function FileAgeToUnixTimeUtc(const FileName: TFileName;
  AllowDir: boolean = false): TUnixTime;

/// get the date and time of one file into a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileAgeToWindowsTime(F: THandle): integer;

/// copy the date of one file to another
// - FileSetDate(THandle, Age) is not available on POSIX: filename is needed
function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean; overload;

/// copy the date of one file to another
// - FileSetDate(THandle, Age) is not available on POSIX: filename is needed
function FileSetDateFrom(const Dest, Source: TFileName): boolean; overload;

/// copy the date of one file from a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
// - FileSetDate(THandle, Age) is not available on POSIX: filename is needed
function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;

/// set the file date/time from a supplied UTC TUnixTime value
// - avoid any temporary conversion to local time
// - Time may come from FileAgeToUnixTimeUtc()
function FileSetDateFromUnixUtc(const Dest: TFileName; Time: TUnixTime): boolean;

/// convert a Windows API File 32-bit TimeStamp into a regular TDateTime
// - returns 0 if the conversion failed
// - used e.g. by FileSetDateFromWindowsTime() on POSIX
function WindowsFileTimeToDateTime(WinTime: integer): TDateTime;

/// convert a Windows API File 64-bit TimeStamp into a regular TUnixMSTime
// - i.e. a FILETIME value as returned by GetFileTime() Win32 API
// - some binary formats (e.g. ISO 9660 or LDAP) have such FILETIME fields
function WindowsFileTime64ToUnixMSTime(WinTime: QWord): TUnixMSTime;

/// low-level conversion of a TDateTime into a Windows File 32-bit TimeStamp
// - returns 0 if the conversion failed
function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;

/// redefined here to avoid warning to include "Windows" in uses clause
// and support FileName longer than MAX_PATH
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function DeleteFile(const aFileName: TFileName): boolean;

/// redefined here to avoid warning to include "Windows" in uses clause
// and support FileName longer than MAX_PATH
function RemoveDir(const aDirName: TFileName): boolean;

/// check if a file (or folder) exists and can be written
// - on POSIX, call fpaccess() and check for the W_OK attribute
// - on Windows, check faReadOnly and supports aFileName longer than MAX_PATH
function FileIsWritable(const FileName: TFileName): boolean;

/// reduce the visibility of a given file, and set its read/write attributes
// - on POSIX, change attributes for the the owner, and reset group/world flags
// so that it is accessible by the current user only; under POSIX, there is
// no "hidden" file attribute, but you should define a FileName starting by '.'
// - on Windows, will set the "hidden" file attribue
procedure FileSetHidden(const FileName: TFileName; ReadOnly: boolean);

/// set the "sticky bit" on a file or directory
// - on POSIX, a "sticky" folder will ensure that its nested files will be
// deleted by their owner; and a "sticky" file will ensure e.g. that no
// /var/tmp file is deleted by systemd during its clean up phases
// - on Windows, will set the Hidden and System file attributes
procedure FileSetSticky(const FileName: TFileName);

/// get a file size, from its name
// - returns 0 if file doesn't exist, or is a directory
// - under Windows, will use GetFileAttributesEx fast API
// - on POSIX, will use efficient fpStat() single call but not FileOpen/FileClose
function FileSize(const FileName: TFileName): Int64; overload;

/// get a file size, from its handle
// - returns 0 if file doesn't exist
// - under Windows, will use the GetFileSizeEx fast API
// - on POSIX, will use efficient FpFStat() single call and no file seek
function FileSize(F: THandle): Int64; overload;

/// FileSeek() overloaded function, working with huge files
// - Delphi FileSeek() is buggy -> use this function to safely access files
// bigger than 2 GB (thanks to sanyin for the report)
function FileSeek64(Handle: THandle; const Offset: Int64;
  Origin: cardinal = soFromBeginning): Int64;

/// get a file size and its UTC Unix timestamp in milliseconds resolution
// - return false if FileName was not found
// - return true and set FileSize and FileTimestampUtc if found - note that
// no local time conversion is done, so timestamp won't match FileAge()
// - if FileName is a folder/directory, then returned FileSize equals -1
// - use a single Operating System call, so is faster than FileSize + FileAge
function FileInfoByName(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime; FileAttr: PInteger = nil): boolean; overload;

/// get low-level file information with timings, in a cross-platform way
// - returns true on success
// - you can specify nil for any returned value if you don't need
// - here file write/creation time are given as TUnixMSTime values, for better
// cross-platform process - note that FileCreateDateTime may not be supported
// by most Linux file systems, so the oldest timestamp available is returned
// as failover on such systems (probably the latest file metadata writing)
function FileInfoByHandle(aFileHandle: THandle; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;

/// get low-level file information with timings, in a cross-platform way
// - is a wrapper around FileInfoByHandle() function - rarely called
// - please prefer FileInfoByName() overload if FileCreateDateTime is not needed
// - returns true on success
function FileInfoByName(const FileName: TFileName; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean; overload;

/// check if a given file is likely to be an executable
// - will check the DOS/WinPE executable header in its first bytes on Windows
// - will call fpStat() on POSIX to check the File and Executable bits
function FileIsExecutable(const FileName: TFileName): boolean;

/// check if a given file is a symbolic link
function FileIsSymLink(const FileName: TFileName): boolean;

/// copy one file to another, using the Windows API if possible
// - on POSIX, will call StreamCopyUntilEnd() between two TFileStreamEx
// - will delete the Target file on any copying issue (e.g. process abort)
function CopyFile(const Source, Target: TFileName;
  FailIfExists: boolean): boolean;

/// create a symbolic link named SymLink pointing to the Target file
// - won't work with directories, a non existing target file, or on Windows XP
// - need specific (admin) priviledges on Windows, or developper mode enabled
function FileSymLink(const SymLink, Target: TFileName): boolean;

/// prompt the user for an error message to notify an unexpected issue
// - in practice, text encoding is better to be plain 7-bit ASCII
// - on Windows, will allocate a console if needed and write to STD_ERROR_HANDLE
// - on POSIX, will send the output to StdErrorHandle (=2)
procedure DisplayFatalError(const title, msg: RawUtf8);

/// prompt the user for an error message to notify an unexpected issue
// - redirect to DisplayFatalError() without any title
// - expects the regular Format() layout with %s %d - not the FormatUtf8() %
procedure DisplayError(const fmt: string; const args: array of const);

/// get a file date and time, from a FindFirst/FindNext search
// - the returned timestamp is in local time, not UTC
// - this method would use the F.Timestamp field available since Delphi XE2
function SearchRecToDateTime(const F: TSearchRec): TDateTime;

/// get a file UTC date and time, from a FindFirst/FindNext search
// - SearchRecToDateTime(), SearchRecToWindowsTime() and F.TimeStamp, which have
// local time and require a conversion, may appear less useful on server side
// - is implemented as a wrapper around SearchRecToUnixTimeUtc()
function SearchRecToDateTimeUtc(const F: TSearchRec): TDateTime;

/// get a file UTC date and time, from a FindFirst/FindNext search, as Unix time
// - SearchRecToDateTime(), SearchRecToWindowsTime() and F.TimeStamp, which have
// local time and require a conversion, may appear less useful on server side
function SearchRecToUnixTimeUtc(const F: TSearchRec): TUnixTime;
  {$ifdef OSPOSIX}inline;{$endif}

/// get a file date and time, from a FindFirst/FindNext search, as Windows time
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function SearchRecToWindowsTime(const F: TSearchRec): integer;

/// check if a FindFirst/FindNext found instance is actually a file
// - on Windows, hidden files are ignored by default unless IncludeHidden is true
function SearchRecValidFile(const F: TSearchRec; IncludeHidden: boolean = false): boolean;

/// check if a FindFirst/FindNext found instance is actually a folder
function SearchRecValidFolder(const F: TSearchRec; IncludeHidden: boolean = false): boolean;

/// just a wrapper around FindFirst() with proper faHidden support
function FindFirstDirectory(const Path: TFileName; IncludeHidden: boolean;
    out F: TSearchRec): integer;

type
  /// a TFileStream replacement which supports FileName longer than MAX_PATH,
  // and a proper Create(aHandle) constructor in FPC
  TFileStreamEx = class(THandleStream)
  protected
    fFileName : TFileName;
    fDontReleaseHandle: boolean;
    function GetSize: Int64; override;
  public
    /// open or create the file from its name, depending on the supplied Mode
    // - Mode is typically fmCreate / fmOpenReadShared
    constructor Create(const aFileName: TFileName; Mode: cardinal);
    /// can use this class from a low-level file OS handle
    constructor CreateFromHandle(aHandle: THandle; const aFileName: TFileName;
      aDontReleaseHandle: boolean = false);
    /// open for reading via FileOpenSequentialRead()
    constructor CreateRead(const aFileName: TFileName);
    /// open for writing or create a non-existing file from its name
    // - use fmCreate if aFileName does not exists, or fmOpenWrite otherwise
    constructor CreateWrite(const aFileName: TFileName);
    /// explictely close the handle if needed
    destructor Destroy; override;
    /// Destroy calls FileClose(Handle) unless this property is true
    property DontReleaseHandle: boolean
      read fDontReleaseHandle write fDontReleaseHandle;
    /// the file name assigned to this class constructor
    property FileName : TFileName
      read fFilename;
  end;

  /// file stream which ignores I/O write errors
  // - in case disk space is exhausted, TFileStreamNoWriteError.WriteBuffer
  // won't throw any exception, so application will continue to work
  // - used e.g. by TSynLog to let the application continue with no exception,
  // even in case of a disk/partition full of logs
  TFileStreamNoWriteError = class(TFileStreamEx)
  public
    /// open for writing, potentially with alternate unlocked file names
    // - use fmCreate if aFileName does not exists, or fmOpenWrite otherwise
    // - on error, will try up to aAliases alternate '<filename>-locked<#>.<ext>'
    constructor CreateAndRenameIfLocked(
      var aFileName: TFileName; aAliases: integer = 3);
    /// this overriden function returns Count, as if it was always successful
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

/// a wrapper around FileRead() to ensure a whole memory buffer is retrieved
// - expects Size to be up to 2GB (seems like a big enough memory buffer)
// - on Windows, will read by 16MB chunks max to avoid ERROR_NO_SYSTEM_RESOURCES
// - will call FileRead() and retry up to Size bytes are filled in the buffer
// - return true if all memory buffer has been read, or false on error
function FileReadAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;

/// a wrapper around FileWrite() to ensure a whole memory buffer is retrieved
// - will call FileWrite() and retry up to Size bytes are written from the buffer
// - return true if all memory buffer has been written, or false on error
function FileWriteAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;

/// overloaded function optimized for one pass reading of a (huge) file
// - will use e.g. the FILE_FLAG_SEQUENTIAL_SCAN flag under Windows, as stated
// by http://blogs.msdn.com/b/oldnewthing/archive/2012/01/20/10258690.aspx
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by StringFromFile() or HashFile() functions
// - note: you could better use FileReadAll() to retrieve a whole data buffer
function FileOpenSequentialRead(const FileName: TFileName): integer;

/// returns a TFileStreamFromHandle optimized for one pass file reading
// - wrap TFileStreamEx.CreateRead() to use FileOpenSequentialRead(),
// i.e. FILE_FLAG_SEQUENTIAL_SCAN on Windows
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by TRestOrmServerFullMemory and TAlgoCompress
// - returns nil if FileName does not exist, without any exception
function FileStreamSequentialRead(const FileName: TFileName): THandleStream;

/// try to open the file from its name, as fmOpenReadShared
// - on Windows, calls CreateFileW(aFileName,GENERIC_READ) then CloseHandle
// - on POSIX, calls fpOpen(pointer(aFileName),O_RDONLY) with no fpFlock() call
function FileIsReadable(const aFileName: TFileName): boolean;

/// copy all Source content into Dest from current position
// - on Delphi, Dest.CopyFrom(Source, 0) uses GetSize and ReadBuffer which is
// not compatible e.g. with TAesPkcs7Reader padding - and has a small buffer
// - returns the number of bytes copied from Source to Dest
function StreamCopyUntilEnd(Source, Dest: TStream): Int64;

/// a wrapper around TStream.Read() to ensure a whole memory buffer is retrieved
// - same as TStream.ReadBuffer but returning false with no exception on failure
// - on Windows, will read by 16MB chunks max to avoid ERROR_NO_SYSTEM_RESOURCES
function StreamReadAll(S: TStream; Buffer: pointer; Size: PtrInt): boolean;

/// read a File content into a string, using FileSize() to guess its length
// - content can be binary or text
// - returns '' if file was not found or any read error occurred
// - uses RawByteString for byte storage, forcing CP_UTF8 for FPC consistency
function StringFromFile(const FileName: TFileName): RawByteString;

/// read a File content from a list of potential files
// - returns '' if no file was found, or the first matching FileName[] content
function StringFromFirstFile(const FileName: array of TFileName): RawByteString;

/// read all Files content from a list of file names
// - returns '' if no FileName[] file was found, or the read content
function StringFromFiles(const FileName: array of TFileName): TRawByteStringDynArray;

/// read all Files content from a list of folders names
// - returns the content of every file contained in the supplied Folders[]
// - with optionally the FileNames[] corresponding to each result[] content
function StringFromFolders(const Folders: array of TFileName;
  const Mask: TFileName = FILES_ALL;
  FileNames: PFileNameDynArray = nil): TRawByteStringDynArray;

/// create a File from a string content
// - uses RawByteString for byte storage, whatever the codepage is
// - can optionaly force flush all write buffers to disk
function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean = false): boolean;

/// create a File from a memory buffer content
function FileFromBuffer(Buf: pointer; Len: PtrInt; const FileName: TFileName;
  FlushOnDisk: boolean = false): boolean;

/// fill a memory buffer from a file content
function BufferFromFile(const FileName: TFileName; Buf: pointer; Len: PtrInt): boolean;

/// create or append a string content to a File
// - can optionally rotate the file to a FileName+'.bak'  over a specific size
function AppendToFile(const Content: RawUtf8; const FileName: TFileName;
  BackupOverMaxSize: Int64 = 0): boolean;

/// compute an unique temporary file name
// - return by default GetSystemPath(spTemp) + 'exename_xxxxxxxx.tmp'
// - return the first non-existing file name: caller would ensure it is writable
function TemporaryFileName(FolderName: TFileName = '';
  ExeName: RawUtf8 = ''): TFileName;

/// extract a path from a file name like ExtractFilePath function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractPath(const FileName: TFileName): TFileName;

/// extract a path from a RawUtf8 file name like ExtractFilePath function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractPathU(const FileName: RawUtf8): RawUtf8;

/// extract a name from a file name like ExtractFileName function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractName(const FileName: TFileName): TFileName;

/// extract a name from a file name like ExtractFileName function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractNameU(const FileName: RawUtf8): RawUtf8;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractExt(const FileName: TFileName; WithoutDot: boolean = false): TFileName;

// defined here for proper ExtractExtP() inlining
function GetLastDelimU(const FileName: RawUtf8; OtherDelim: AnsiChar = #0): PtrInt;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractExtU(const FileName: RawUtf8; WithoutDot: boolean = false): RawUtf8;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
// - don't allocate anything, but return a pointer to the extension within FileName
function ExtractExtP(const FileName: RawUtf8; WithoutDot: boolean = false): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// compute the file name, including its path if supplied, but without its extension
// - e.g. GetFileNameWithoutExt('/var/toto.ext') = '/var/toto'
// - may optionally return the extracted extension, as '.ext'
// - will be cross-platform, i.e. detect both '\' and '/' on all platforms
function GetFileNameWithoutExt(const FileName: TFileName;
  Extension: PFileName = nil): TFileName;

/// extract the file name without any path nor extension, as UTF-8
// - e.g. GetFileNameWithoutExt('/var/toto.ext') = 'toto'
// - used e.g. to compute Executable.ProgramName
function GetFileNameWithoutExtOrPath(const FileName: TFileName): RawUtf8;

/// compare two "array of TFileName" elements, grouped by file extension
// - i.e. with no case sensitivity on Windows
// - the expected string type is the RTL string, i.e. TFileName
// - like calling GetFileNameWithoutExt() and AnsiCompareFileName()
function SortDynArrayFileName(const A, B): integer;

{$ifdef ISDELPHI20062007}
/// compatibility function defined to avoid hints on buggy Delphi 2006/2007
function AnsiCompareFileName(const S1, S2 : TFileName): integer;
{$endif ISDELPHI20062007}

/// creates a directory if not already existing
// - returns the full expanded directory name, including trailing path delimiter
// - returns '' on error, unless RaiseExceptionOnCreationFailure is set
// - you can set NoExpand=true if you know that Directory has already a full path
function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil;
  NoExpand: boolean = false): TFileName; overload;

/// just a wrapper around EnsureDirectoryExists({noexpand=}true) for Delphi 7
function EnsureDirectoryExistsNoExpand(const Directory: TFileName): TFileName;
  {$ifdef HASINLINE} inline; {$endif}

/// just a wrapper around EnsureDirectoryExists(NormalizeFileName(Directory))
function NormalizeDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil): TFileName; overload;

/// compute the size of all directory's files, optionally with nested folders
// - basic implementation using FindFirst/FindNext so won't be the fastest
// available, nor fully accurate when files are actually (hard) links
// - use TDirectoryBrowser to circumvent MAX_PATH issue on Windows
function DirectorySize(const Path: TFileName; Recursive: boolean = false;
  const FileMask: TFileName = FILES_ALL): Int64;

/// delete the content of a specified directory
// - only one level of file is deleted within the folder: no recursive deletion
// is processed by this function (for safety) - see instead DirectoryDeleteAll()
// - if DeleteOnlyFilesNotDirectory is TRUE, it won't remove the folder itself,
// but just the files found in it
// - use TDirectoryBrowser to circumvent MAX_PATH issue on Windows
// - return false if any DeleteFile() or RemoveDir() did fail during the process
// - warning: DeletedCount^ should be a 32-bit "integer" variable, not a PtrInt
function DirectoryDelete(const Directory: TFileName;
  const Mask: TFileName = FILES_ALL; DeleteOnlyFilesNotDirectory: boolean = false;
  DeletedCount: PInteger = nil): boolean;

/// delete the content of a specified directory and all its nested sub-folders
// - just a wrapper to recursive DirectoryDeleteOlderFiles() with TimePeriod=0
// - use TDirectoryBrowser to circumvent MAX_PATH issue on Windows
function DirectoryDeleteAll(const Directory: TFileName): boolean;

/// delete the files older than a given age in a specified directory
// - for instance, to delete all files older than one day:
// ! DirectoryDeleteOlderFiles(FolderName, 1);
// - TimePeriod = 0 will delete all files found, whatever timestamp they are
// - Recursive = true will delete all nested folders
// - use TDirectoryBrowser to circumvent MAX_PATH issue on Windows
// - return false if any DeleteFile() or RemoveDir() did fail during the process
function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName = FILES_ALL;
  Recursive: boolean = false; TotalSize: PInt64 = nil;
  DeleteFolders: boolean = false): boolean;

type
  /// recursively search a folder and its nested sub-folders via methods
  // - as used e.g. by DirectorySize() or DirectoryDeleteOlderFiles()
  // - you should inherit from this class, supply some private parameters,
  // then override OnFile and OnFolder to match your expected process
  // - this class will detect and circumvent any MAX_PATH limitation on Windows
  TDirectoryBrowser = class
  protected
    fCurrentDir: TFileName;
    fFileMask: TFileNameDynArray;
    fRecursive, fExtendedPath, fAborted: boolean;
    fTotalSize: Int64;
    fLevelCounter: PInteger; // a ProcessDir recursive counter
    function Make(const fn: TFileName): TFileName; // handle MAX_PATH on Windows
    procedure ProcessDir; virtual;   // main recursive method
    procedure OnProcessDir; virtual; // executed before each folder
    // virtual method executed for each file - true = continue, false = abort
    function OnFile(const FileInfo: TSearchRec; const FullFileName: TFileName): boolean; virtual;
    // virtual method executed for each folder, after OnFile() have been called
    function OnFolder(const FullFolderName: TFileName): boolean; virtual;
  public
    /// prepare the process for a given folder
    // - typical FileMasks value is [FILES_ALL] but also e.g. ['*.pas','*.pp']
    // - FileMasks = [] will trigger only OnFolder but not OnFile
    constructor Create(const Directory: TFileName;
      const FileMasks: array of TFileName; Recursive: boolean = true);
    /// this method is the main processing function
    // - returns false if has been aborted by a OnFile() or OnFolder() method
    function Run: boolean;
  end;

  /// defines how IsDirectoryWritable() verifies a folder
  // - on Win32 Vista+, idwExcludeWinUac will check IsUacVirtualFolder()
  // - on Windows, idwExcludeWinSys will check IsSystemFolder()
  // - on Windows, idwTryWinExeFile will try to generate a 'xxxxx.exe' file
  // - idwAttributesOnly won't create any file but just check folder faReadOnly
  // which may be enough with a POSIX's fpaccess() call
  // - idwWriteSomeContent will also try to write some bytes into the file
  TIsDirectoryWritable = set of (
    idwExcludeWinUac,
    idwExcludeWinSys,
    idwTryWinExeFile,
    idwAttributesOnly,
    idwWriteSomeContent);

/// check if the directory is writable for the current user
// - try to write and delete a void file with a random name in this folder
// - use idwAttributesOnly by default on POSIX, since fpaccess() is accurate
// unless the current user was changed via setuid / DropPriviledges()
function IsDirectoryWritable(const Directory: TFileName;
  Flags: TIsDirectoryWritable = [ {$ifdef OSPOSIX} idwAttributesOnly {$endif} ]): boolean;

type
  /// cross-platform memory mapping of a file content
  {$ifdef USERECORDWITHMETHODS}
  TMemoryMap = record
  {$else}
  TMemoryMap = object
  {$endif USERECORDWITHMETHODS}
  private
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
    /// map the corresponding file handle into memory
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view is created (by default, will map whole file, but it could
    // be handy to only map a portion of it, e.g. its start for TSynPELoader,
    // or to keep the pointers in a workable range on CPU32 systems)
    function Map(aFile: THandle; aCustomSize: PtrUInt = 0;
      aCustomOffset: Int64 = 0; aFileOwned: boolean = false;
      aFileSize: Int64 = -1; aForceMap: boolean = false): boolean; overload;
    /// map the file specified by its name into memory for reading
    // - file will be closed when UnMap is called
    function Map(const aFileName: TFileName; aForceMap: boolean = false;
      aCustomSize: Int64 = 0; aCustomOffset: Int64 = 0): boolean; overload;
    /// set a fixed buffer for the content
    // - emulates memory-mapping over an existing buffer
    procedure Map(aBuffer: pointer; aBufferSize: PtrUInt); overload;
    /// unmap the file - to be called eventually after any successfull Map()
    procedure UnMap;
    /// retrieve the memory buffer mapped to the file content
    property Buffer: PAnsiChar
      read fBuf;
    /// retrieve the memory mapped buffer size (may be < FileSize)
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
    fFileStream: THandleStream;
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
  {$ifdef USERECORDWITHMETHODS}
  TExecutableResource = record
  {$else}
  TExecutableResource = object
  {$endif USERECORDWITHMETHODS}
  private
    // note: we can't use THandle which is 32-bit on 64-bit POSIX
    HResInfo: TLibHandle;
    HGlobal: TLibHandle;
  public
    /// the resource memory pointer, after successful Open()
    Buffer: pointer;
    /// the resource memory size in bytes, after successful Open()
    Size: PtrInt;
    /// locate and lock a resource
    // - use the current executable if Instance is left to its 0 default value
    // - returns TRUE if the resource has been found, and Buffer/Size are set
    function Open(const ResourceName: string; ResType: PChar;
      Instance: TLibHandle = 0): boolean;
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
  {$ifdef USERECORDWITHMETHODS}
  TProcessInfo = record
  {$else}
  TProcessInfo = object
  {$endif USERECORDWITHMETHODS}
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

  /// hold low-level GetMemoryInfo() result about current memory usage
  // - most fields are in bytes, except percent which is the % of used memory
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

/// return a memory block aligned to 16 bytes, e.g. for proper SMID processs
// - Size >= 128KB will call the OS mmap/VirtualAlloc to returned aligned memory
// - do not use FreeMem() on the returned pointer, but FreeMemAligned()
function GetMemAligned(Size: PtrUInt; FillWith: pointer = nil): pointer;

/// properly release GetMemAligned() allocated memory
procedure FreeMemAligned(p: pointer; Size: PtrUInt);

const
  // 16*4KB (4KB = memory granularity) for ReserveExecutableMemory()
  STUB_SIZE = 65536;

/// cross-platform reserve some executable memory
// - using PAGE_EXECUTE_READWRITE flags on Windows, and PROT_READ or PROT_WRITE
// or PROT_EXEC on POSIX
// - this function maintain an internal list of 64KB memory pages for efficiency
// - memory blocks can not be released (don't try to use fremeem on them) and
// will be returned to the system at process finalization
// - caller needs to eventually call ReserveExecutableMemoryPageAccess()
// - raise EOSException if the memory allocation failed at OS level
function ReserveExecutableMemory(size: cardinal
  {$ifdef CPUARM} ; ArmFakeStubAddr: pointer {$endif}): pointer;

/// to be called after ReserveExecutableMemory() when you want to actually write
// the memory blocks
// - affect the mapping flags of the first memory page (4KB) of the Reserved
// buffer, so its size should be < 4KB
// - do nothing on Windows and Linux, but may be needed on OpenBSD / OSX
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);

/// check if the supplied pointer is actually pointing to some memory page
// - will call slow but safe VirtualQuery API on Windows, or try a fpaccess()
// syscall on POSIX systems (validated on Linux only)
function SeemsRealPointer(p: pointer): boolean;

/// check if the supplied pointer is likely to be a valid TObject
// - will call SeemsRealPointer(p) then check its main VMT entries
function SeemsRealObject(p: pointer): boolean;

/// fill a buffer with a copy of some low-level system memory
// - used e.g. by GetRawSmbios on XP or Linux/POSIX
// - will allow to read up to 4MB of memory
// - use low-level ntdll.dll API on Windows, or reading /dev/mem on POSIX - so
// expect sudo/root rights on most systems
function ReadSystemMemory(address, size: PtrUInt): RawByteString;

/// return the PIDs of all running processes
// - under Windows, is a wrapper around EnumProcesses() PsAPI call
// - on Linux, will enumerate /proc/* pseudo-files
function EnumAllProcesses: TCardinalDynArray;

/// return the process name of a given process  ID
// - under Windows, is a wrapper around
// QueryFullProcessImageNameW/GetModuleFileNameEx PsAPI call
// - on Linux, will query /proc/[pid]/exe or /proc/[pid]/cmdline pseudo-file
function EnumProcessName(PID: cardinal): RawUtf8;

/// return the process ID of the parent of a given PID
// - by default (PID = 0), will search for the parent of the current process
// - returns 0 if the PID was not found
function GetParentProcess(PID: cardinal = 0): cardinal;

/// check if this process is currently running into the debugger
// - redirect to the homonymous WinAPI function on Windows, or check if the
// /proc/self/status "TracerPid:" value is non zero on Linux, or search if
// "lazarus" is part of the parent process name for BSD
{$ifdef OSWINDOWS}
function IsDebuggerPresent: BOOL; stdcall;
{$else}
function IsDebuggerPresent: boolean;
{$endif ODWINDOWS}

/// return the time and memory usage information about a given process
// - under Windows, is a wrapper around GetProcessTimes/GetProcessMemoryInfo
// - on POSIX, is not implemented yet, and return false
function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;

/// return the system-wide time usage information
// - under Windows, is a wrapper around GetSystemTimes() kernel API call
// - will use /proc/stat on Linux, or kern.cp_time sysctl on BSD
// - returned KernelTime includes IdleTime, as with GetSystemTimes() WinAPI
function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;

/// return the system-wide time usage information as 'U:#.## K:#.##' percents
// - calling RetrieveSystemTimes() on all platforms
function RetrieveSystemTimesText: TShort23;

/// return the system-wide time usage information
// - on LINUX, retrieve /proc/loadavg or on OSX/BSD call libc getloadavg()
// - on Windows, calls RetrieveSystemTimesText to return 'U:user K:kernel' text
function RetrieveLoadAvg: TShort23;
  {$ifdef OSWINDOWS} {$ifdef HASINLINE} inline; {$endif} {$endif}

/// a shorter version of GetSystemInfoText, used e.g. by TSynLogFamily.LevelSysInfo
// - 'ncores avg1 avg5 avg15 [updays] used/totalram [used/totalswap] osint32' on POSIX,
// or 'ncores user kern [updays] used/totalram [used/totalswap] osint32' on Windows
procedure RetrieveSysInfoText(out text: ShortString);

/// retrieve low-level information about current memory usage
// - as used e.g. by TSynMonitorMemory or GetMemoryInfoText
// - under BSD, only memtotal/memfree/percent are properly returned
// - allocreserved and allocused are set only if withalloc is TRUE
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;

/// retrieve some human-readable text from GetMemoryInfo
// - numbers are rounded up to a single GB number with no decimals
// - returns the size of used memory / total memory e.g. as '6GB/16GB (35%)'
function GetMemoryInfoText: TShort31;

/// retrieve some human-readable text about the current system in several lines
// - includes UTC timestamp, memory and disk availability, and exe/OS/CPU info
function GetSystemInfoText: RawUtf8;

/// retrieve low-level information about a given disk partition
// - as used e.g. by TSynMonitorDisk and GetDiskPartitionsText()
// - aDriveFolderOrFile is a directory on disk (no need to specify a raw drive
// name like 'c:\' on Windows)
// - warning: aDriveFolderOrFile may be modified at input
// - only under Windows the Quotas are applied separately to aAvailableBytes
// in respect to global aFreeBytes
function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord
  {$ifdef OSWINDOWS}; aVolumeName: PSynUnicode = nil{$endif}): boolean;

/// retrieve how many bytes are currently available on a given folder
// - returns 0 if the function fails
function GetDiskAvailable(aDriveFolderOrFile: TFileName): QWord;

/// retrieve low-level information about all mounted disk partitions of the system
function GetDiskPartitions: TDiskPartitions;

/// call several Operating System APIs to gather 512-bit of entropy information
procedure XorOSEntropy(var e: THash512Rec);

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

var
  /// low-level handle used for console writing
  // - may be overriden when console is redirected
  // - on Windows, is initialized when AllocConsole, HasConsole or TextColor()
  // are actually called
  StdOut: THandle;

  /// global flag to modify the code behavior at runtime when run from TSynTests
  // - e.g. TSynDaemon.AfterCreate won't overwrite TSynTests.RunAsConsole logs
  RunFromSynTests: boolean;

/// similar to Windows AllocConsole API call, to be truly cross-platform
// - do nothing on Linux/POSIX, but set StdOut propertly from StdOutputHandle
// - on Windows, will call the corresponding API, and set StdOut global variable
procedure AllocConsole;

/// always true on POSIX, may be false for a plain Windows GUI application
{$ifdef OSWINDOWS}
function HasConsole: boolean;
{$else}
const HasConsole = true; // assume POSIX has always a console somewhere

/// POSIX only: true if StdOut has the TTY flag and env has a known TERM
// - equals false if the console does not support colors, e.g. piped to a file
// or from the Lazarus debugger
function StdOutIsTTY: boolean; inline;
{$endif OSWINDOWS}

/// change the console text writing color
procedure TextColor(Color: TConsoleColor);

/// change the console text background color
procedure TextBackground(Color: TConsoleColor);

/// write some text to the console using a given color
// - this method is protected by its own CriticalSection for output consistency
procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor = ccLightGray;
  NoLineFeed: boolean = false; NoColor: boolean = false); overload;

/// write some text to the console using the current color
// - similar to writeln() but redirect to ConsoleWrite(NoColor=true)
procedure ConsoleWriteRaw(const Text: RawUtf8; NoLineFeed: boolean = false); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append a line feed to the console
// - similar to writeln but redirect to ConsoleWrite() with proper thread safety
procedure ConsoleWriteLn;
  {$ifdef HASINLINE} inline; {$endif}

/// will wait for the ENTER key to be pressed, with all needed waiting process
// - on the main thread, will call Synchronize() for proper work e.g. with
// interface-based service implemented as optExecInMainThread
// - on Windows, from a non-main Thread, respond to PostThreadMessage(WM_QUIT)
// - on Windows, also properly respond to Ctrl-C or closing console events
// - on POSIX, will call SynDaemonIntercept first, so that Ctrl-C or SIG_QUIT
// will also be intercepted and let this procedure return
procedure ConsoleWaitForEnterKey;

/// read all available content from stdin
// - could be used to retrieve some file piped to the command line
// - the content is not converted, so will follow the encoding used for storage
function ConsoleReadBody: RawByteString;

{$ifdef OSWINDOWS}

/// low-level access to the keyboard state of a given key
// - will consume all pending console events until this key is pressed (or not)
function ConsoleKeyPressed(ExpectedKey: Word): boolean;

type
  TWinWaitFor = (wwfFailed, wwfQuit, wwfTimeout, wwfSignaled);

/// can wait for some time and/or event, not blocking the Windows main thread
// - supposed to be called in a loop, so expects ms value in [50..200] range
// - can optionally call WaitForSingleObject(h) instead of plain Sleep()
// - can intercept messages and return wwfQuit on WM_QUIT on a sub-thread
// - running on the main thread, would properly call CheckSynchronize(ms) then
// Application.ProcessMessages, as expected by a regular GUI application
// - outside the main thread, behave like a single Sleep/WaitForSingleObject
function WinWaitFor(ms: cardinal; h: THandle = 0;
  checkSubThreadQuit: boolean = false): TWinWaitFor;

var
  /// used by Win32PWideCharToUtf8() when IsAnsiCompatibleW(P, Len) = false
  // - overriden by mormot.core.unicode for performance and Delphi 7/2007 fix
  DoWin32PWideCharToUtf8: procedure(P: PWideChar; Len: PtrInt; var res: RawUtf8);

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; Len: PtrInt;
  out res: RawUtf8); overload;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; out res: RawUtf8); overload;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToFileName(P: PWideChar; out fn: TFileName);

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
// - just a wrapper around Unicode_FromUtf8() over a temporary buffer
// - caller should always call d.Done to release any (unlikely) allocated memory
function Utf8ToWin32PWideChar(const u: RawUtf8; var d: TSynTempBuffer): PWideChar;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
// - returns true and set A on conversion success from UTF-8 to code page CP
// - as used internally by Utf8ToConsole()
function Win32Utf8ToAnsi(P: pointer; L, CP: integer; var A: RawByteString): boolean;

{$ifndef NOEXCEPTIONINTERCEPT}
/// get DotNet exception class names from HRESULT - published for testing purpose
function Win32DotNetExceptions(code: cardinal; var dest: ShortString): boolean;
{$endif NOEXCEPTIONINTERCEPT}

/// ask the Operating System to convert a file URL to a local file path
// - only Windows has a such a PathCreateFromUrlW() API
// - POSIX define this in mormot.net.http.pas, where TUri is available
// - used e.g. by TNetClientProtocolFile to implement the 'file://' protocol
function GetFileNameFromUrl(const Uri: RawUtf8): TFileName;

{$else}

/// internal function just wrapping fppoll(POLLIN or POLLPRI)
function WaitReadPending(fd, timeout: integer): boolean;

type
  /// optional callback used by PosixFileNames()
  // - same signature as mormot.core.search MatchAnyP()
  TOnPosixFileName = function(opaque: pointer; name: PUtf8Char; namelen: PtrInt): boolean;

/// POSIX-only function calling directly getdents/getdents64 syscall
// - could be used when FindFirst/FindNext are an overkill, e.g. to quickly
// cache all file names of a folder in memory, optionally with its sub-folders
// - used e.g. by TPosixFileCaseInsensitive from mormot.core.unicode
// - warning: the file system has to support d_type (e.g. btrfs, ext2-ext4) so
// that Recursive is handled and only DT_REG files are retrieved; non-compliant
// file systems (or Linux Kernel older than 2.6.4) won't support the Recursive
// search, and may return some false positives, like symlinks or nested folders
// - an optional callback can be supplied, used e.g. by the FileNames() function
// in mormot.core.search to efficiently implement name mask search with a TMatch
// - IncludeFolders would include nested folders as '/foldername'
function PosixFileNames(const Folder: TFileName; Recursive: boolean;
  OnFile: TOnPosixFileName = nil; OnFileOpaque: pointer = nil;
  ExcludesDir: boolean = false; IncludeHiddenFiles: boolean = false;
  IncludeFolders: boolean = false): TRawUtf8DynArray;

/// POSIX-only function to change the public name of the current process
// - supplied Name should be pure ASCII-7 identifier - or fails returning false
// - on Linux, will use prctl(PR_SET_NAME) syscall - truncating Name to 15 bytes
// - on other POSIX (e.g. Darwin), will try to override the argv[0] value
// directly in the process memory - which is somehow supported by some tools
// - Windows does not allow to change the process name at all
function PosixSetProcessName(const Name: RawUtf8): boolean;

{$ifdef OSLINUXANDROID}
/// read a File content into a string, without using FileSize()
// - result will be filled using a buffer as required e.g. for POSIX char files
// like /proc/... or /sys/...
function StringFromFileNoSize(const FileName: TFileName): RawByteString;

/// read a small File content into a string, without using FileSize()
// - in respect to StringFromFileNoSize(), this will make a single read()
procedure LoadProcFileTrimed(fn: PAnsiChar; var result: RawUtf8); overload;
{$endif OSLINUXANDROID}

/// low-level function returning some random binary from the Operating System
// - Windows version calling the CryptGenRandom API is in mormot.core.os.security
// - on POSIX, only up to 256 bytes (2048-bits) are retrieved from /dev/urandom
// or /dev/random as stated by "man urandom" Usage - then padded with our shared
// gsl_rng_taus2 "L'Ecuyer" random generator
// - so you may consider that the output Buffer is always filled with random
// - you should not have to call this low-level procedure, but faster and safer
// TAesPrng from mormot.crypt.core - also consider the TSystemPrng class
function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;

{$endif OSWINDOWS}

/// will append the value as one-decimal number text and B/KB/MB/GB/TB suffix
// - append EB, PB, TB, GB, MB, KB or B symbol with or without a preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure AppendKb(Size: Int64; var Dest: ShortString; WithSpace: boolean = false);

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with preceding space
function KB(Size: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol without preceding space
function KBNoSpace(Size: Int64): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

type
  /// function prototype for AppendShortUuid()
  TAppendShortUuid = procedure(const u: TGuid; var s: ShortString);
  /// function prototype for ShortToUuid()
  TShortToUuid = function(const text: ShortString; out uuid: TGuid): boolean;

var
  /// decode a '3F2504E0-4F89-11D3-9A0C-0305E82C3301' text into a TGuid
  // - this unit defaults to the RTL, but mormot.core.text.pas will override it
  ShortToUuid: TShortToUuid;

  /// append a TGuid into lower-cased '3f2504e0-4f89-11d3-9a0c-0305e82c3301' text
  // - this unit defaults to the RTL, but mormot.core.text.pas will override it
  AppendShortUuid: TAppendShortUuid;

  /// late binding to binary encoding to Base64 or Base64-URI
  // - as used by mormot.net.sock.pas for its NetBinToBase64() function
  // - this unit raises an EOSException - properly injected by mormot.core.buffers.pas
  RawToBase64: function(Bin: pointer; Bytes: PtrInt; Base64Uri: boolean): RawUtf8;

  /// return the RTTI text of a given enumerate as mormot.core.rtti GetEnumName()
  // - this unit defaults to minimal code, but overriden by mormot.core.rtti.pas
  GetEnumNameRtti: function(Info: pointer; Value: integer): PShortString;

/// direct conversion of a UTF-8 encoded string into a console OEM-encoded string
// - under Windows, will use GetConsoleOutputCP() codepage, following CP_OEM
// - under Linux, will expect the console to be defined with UTF-8 encoding
// - we don't propose any ConsoleToUtf8() function because Windows depends on
// the running program itself: most generate CP_OEM (e.g. 850) as expected,
// but some could use the system code page or even UTF-16 binary with BOM (!) -
// so you may consider using AnsiToUtf8() from mormot.core.unicode.pas with the
// proper code page depending on each application
function Utf8ToConsole(const S: RawUtf8): RawByteString;


type
  /// encapsulate cross-platform loading of library files
  // - this generic class can be used for any external library (.dll/.so)
  TSynLibrary = class
  protected
    fHandle: TLibHandle;
    fLibraryPath: TFileName;
    fTryFromExecutableFolder: boolean;
    {$ifdef OSPOSIX}
    fLibraryPathTested: boolean;
    {$endif OSPOSIX}
  public
    /// cross-platform resolution of a function entry in this library
    // - if RaiseExceptionOnFailure is set, missing entry will call FreeLib then raise it
    // - ProcName can be a space-separated list of procedure names, to try
    // alternate API names (e.g. for OpenSSL 1.1.1/3.x compatibility)
    // - if ProcName starts with '?' then RaiseExceptionOnFailure = nil is set
    function Resolve(const Prefix: RawUtf8; ProcName: PAnsiChar; Entry: PPointer;
      RaiseExceptionOnFailure: ExceptionClass = nil; SilentError: PString = nil): boolean;
    /// cross-platform resolution of all function entries in this library
    // - will search and fill Entry^ for all ProcName^ until ProcName^=nil
    // - return true on success
    // - if any entry is missing, raise RaiseExceptionOnFailure or return false
    // and continue if SilentError = nil, or exit if SilentError <> nil
    function ResolveAll(ProcName: PPAnsiChar; Entry: PPointer;
      const Prefix: RawUtf8 = ''; RaiseExceptionOnFailure: ExceptionClass = nil;
      SilentError: PString = nil): boolean;
    /// cross-platform call to FreeLibrary() + set fHandle := 0
    // - as called by Destroy, but you can use it directly to reload the library
    procedure FreeLib;
    /// same as SafeLoadLibrary() but setting fLibraryPath and cwd on Windows
    function TryLoadLibrary(const aLibrary: array of TFileName;
      aRaiseExceptionOnFailure: ExceptionClass = nil;
      aSilentError: PString = nil): boolean; virtual;
    /// wrap TryLoadLibrary() and Resolve() with optional exception call
    function TryLoadResolve(const aLibrary: array of TFileName;
      const Prefix: RawUtf8; ProcName: PPAnsiChar; Entry: PPointer;
      RaiseExceptionOnFailure: ExceptionClass = nil; SilentError: PString = nil): boolean;
    /// release associated memory and linked library
    destructor Destroy; override;
    /// return TRUE if the library and all procedures were found
    function Exists: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// the associated library handle
    property Handle: TLibHandle
      read fHandle write fHandle;
    /// the loaded library path
    // - on POSIX, contains the full path (via dladdr) once Resolve() is called
    property LibraryPath: TFileName
      read fLibraryPath;
    /// if set, and no path is specified, will try from Executable.ProgramFilePath
    // - see also the even more unsafe LibraryGlobalPath variable
    property TryFromExecutableFolder: boolean
      read fTryFromExecutableFolder write fTryFromExecutableFolder;
  end;

  /// used to track e.g. a library or API availability at runtime
  TLibraryState = (
    lsUnTested,
    lsAvailable,
    lsNotAvailable);

var
  /// TSynLibrary.TryLoadLibrary() check for an existing library in this folder
  // - similar to LD_LIBRARY_PATH environment on POSIX, but for this process
  // - ensure any value defined here ends with the proper \ or / delimiter
  // - some (set of) applications tend to supply their preferred libraries in
  // their own shared folder, which could be specified here at startup
  // - warning: use with caution, because this could lead to unexpected behavior
  // or - even worse - some unattended exploits: the safest is to always
  // specify the expected full path of a library, if possible
  LibraryGlobalPath: TFileName;

/// call once Init if State is in its default lsUntested (0) value
function LibraryAvailable(var State: TLibraryState; Init: TProcedure): boolean;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

/// self-modifying code - change some memory buffer in the code segment
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overridden code buffer, for further hook disabling
// - some systems do forbid such live patching: consider setting NOPATCHVMT
// and NOPATCHRTL conditionals for such projects
procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer = nil;
  LeaveUnprotected: boolean = {$ifdef OSLINUX} true {$else} false {$endif});

/// self-modifying code - change one PtrUInt in the code segment
procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt;
  LeaveUnprotected: boolean = {$ifdef OSLINUX} true {$else} false {$endif});

{$ifdef CPUINTEL}
/// low-level i386/x86_64 asm routine patch and redirection
procedure RedirectCode(Func, RedirectFunc: pointer);
{$endif CPUINTEL}

/// close any LeaveUnprotected=true R/W/X memory-mapped paged back as R/X
// - could be used to harden back all memory regions at once, when every RTTI,
// ORM or SOA features are eventually initialized
// - only implemented and tested on Linux by now
procedure PatchCodeProtectBack;


{ **************** TSynLocker/TSynLocked and Low-Level Threading Features }

type
  /// a lightweight exclusive non-reentrant lock, stored in a PtrUInt value
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - warning: methods are non reentrant, i.e. calling Lock twice in a raw would
  // deadlock: see reentrant TMultiLightLock or TRWLock or TSynLocker/TOSLock
  // - several lightlocks, each protecting a few variables (e.g. a list), may
  // be more efficient than a more global TOSLock/TRWLock
  // - our light locks are expected to be kept a very small amount of time (a
  // few CPU cycles): use TOSLightLock if the lock may block too long
  // - TryLock/UnLock can be used to thread-safely acquire a shared resource
  // - only consume 4 bytes on CPU32, 8 bytes on CPU64
  {$ifdef USERECORDWITHMETHODS}
  TLightLock = record
  {$else}
  TLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt;
    procedure LockSpin; // called by the Lock method when inlined
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called to finalize the instance as a TOSLock
    // - does nothing - just for compatibility with TOSLock
    procedure Done;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter an exclusive non-reentrant lock
    procedure Lock;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// try to enter an exclusive non-reentrant lock
    // - if returned true, caller should eventually call UnLock()
    // - could also be used to thread-safely acquire a shared resource
    function TryLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the non-reentrant lock has been acquired
    function IsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave an exclusive non-reentrant lock
    procedure UnLock;
      {$ifdef HASSAFEINLINE} inline; {$endif}
  end;
  PLightLock = ^TLightLock;

  /// a lightweight exclusive reentrant lock
  // - methods are reentrant, i.e. calling Lock twice in a raw would not deadlock
  // - our light locks are expected to be kept a very small amount of time (a
  // few CPU cycles): use TSynLocker or TOSLock if the lock may block too long
  // - TryLock/UnLock can be used also to thread-safely acquire a shared
  // resource in a re-entrant way, as e.g. for sockets in TPollAsyncConnection
  {$ifdef USERECORDWITHMETHODS}
  TMultiLightLock = record
  {$else}
  TMultiLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt;      // is also the reentrant > 0 counter
    ThreadID: TThreadID; // pointer on POSIX, DWord on Windows
    procedure LockSpin;  // called by the Lock method when inlined
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TMultiLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called to finalize the instance as a TOSLock
    // - will make any further TryLock fail - also for compatibility with TOSLock
    procedure Done;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter an exclusive reentrant lock
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter an exclusive reentrant lock
    // - if returned true, caller should eventually call UnLock()
    // - could also be used to thread-safely acquire a shared resource
    function TryLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// acquire this lock for the current thread, ignore any previous state
    // - could be done to safely acquire and finalize a resource, as used e.g.
    // in TPollAsyncSockets.CloseConnection
    // - this method is reentrant: you can call Lock/UnLock on this thread
    procedure ForceLock;
    /// check if the reentrant lock has been acquired
    function IsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave an exclusive reentrant lock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PMultiLightLock = ^TMultiLightLock;

  /// a lightweight multiple Reads / exclusive Write non-upgradable lock
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - warning: ReadLocks are reentrant and allow concurrent acccess, but calling
  // WriteLock within a ReadLock, or within another WriteLock, would deadlock
  // - consider TRWLock if you need an upgradable lock - but for mostly reads,
  // TRWLightLock.ReadLock/ReadUnLock/WriteLock pattern is faster than upgrading
  // - our light locks are expected to be kept a very small amount of time (a
  // few CPU cycles): use TSynLocker or TOSLock if the lock may block too long
  // - several lightlocks, each protecting a few variables (e.g. a list), may
  // be more efficient than a more global TOSLock/TRWLock
  // - only consume 4 bytes on CPU32, 8 bytes on CPU64
  {$ifdef USERECORDWITHMETHODS}
  TRWLightLock = record
  {$else}
  TRWLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt; // bit 0 = WriteLock, bits 1..31/63 = ReadLock
    // low-level functions called by the Lock methods when inlined
    procedure ReadLockSpin;
    procedure WriteLockSpin;
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TRWLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter a non-upgradable multiple reads lock
    // - read locks maintain a thread-safe counter, so are reentrant and non blocking
    // - warning: nested WriteLock call after a ReadLock would deadlock
    procedure ReadLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter a non-upgradable multiple reads lock
    // - if returned true, caller should eventually call ReadUnLock
    // - read locks maintain a thread-safe counter, so are reentrant and non blocking
    // - warning: nested WriteLock call after a ReadLock would deadlock
    function TryReadLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave a non-upgradable multiple reads lock
    procedure ReadUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter a non-reentrant non-upgradable exclusive write lock
    // - warning: nested WriteLock call after a ReadLock or another WriteLock
    // would deadlock
    procedure WriteLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter a non-reentrant non-upgradable exclusive write lock
    // - if returned true, caller should eventually call WriteUnLock
    // - warning: nested TryWriteLock call after a ReadLock or another WriteLock
    // would deadlock
    function TryWriteLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave a non-reentrant non-upgradable exclusive write lock
    procedure WriteUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the non-reentrant lock has been acquired as read or write
    function IsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PRWLightLock = ^TRWLightLock;

  /// how TRWLock.Lock and TRWLock.UnLock high-level wrapper methods are called
  TRWLockContext = (
    cReadOnly,
    cReadWrite,
    cWrite);

  /// a lightweight multiple Reads / exclusive Write reentrant and upgradable lock
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - our light locks are expected to be kept a very small amount of time (some
  // CPU cycles): use TSynLocker or TOSLock if the lock may block too long
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
    {$ifndef ASMX64}
    procedure ReadOnlyLockSpin;
    {$endif ASMX64}
  public
    /// initialize the R/W lock
    // - not needed if TRWLock is part of a class - i.e. if was filled with 0
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called at shutdown to ensure that the R/W lock is in neutral state
    procedure AssertDone;
    /// wait for the lock to be available for reading, but not upgradable to write
    // - several readers could acquire the lock simultaneously
    // - ReadOnlyLock is reentrant since there is a thread-safe internal counter
    // - warning: calling ReadWriteLock/WriteLock after ReadOnlyLock would deadlock
    // - typical usage is the following:
    // ! rwlock.ReadOnlyLock; // won't block concurrent ReadOnlyLock
    // ! try
    // !   result := Exists(value);
    // ! finally
    // !   rwlock.ReadOnlyUnLock;
    // ! end;
    procedure ReadOnlyLock;
      {$ifdef HASINLINE} {$ifndef ASMX64} inline; {$endif} {$endif}
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
    // !       Add(value); // safely modify the shared content
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
    // - this method is reentrant from a single thread
    // - typical usage is the following:
    // ! rwlock.WriteLock; // block any ReadOnlyLock/ReadWriteLock/WriteLock
    // ! try
    // !   Add(value); // safely modify the shared content
    // ! finally
    // !   rwlock.WriteUnLock;
    // ! end;
    procedure WriteLock;
    /// release a previous WriteLock call
    procedure WriteUnlock;
      {$ifdef FPC_OR_DELPHIXE4} inline; {$endif} // circumvent weird Delphi bug
    /// a high-level wrapper over ReadOnlyLock/ReadWriteLock/WriteLock methods
    procedure Lock(context: TRWLockContext {$ifndef PUREMORMOT2} = cWrite {$endif});
      {$ifdef HASINLINE} inline; {$endif}
    /// a high-level wrapper over ReadOnlyUnLock/ReadWriteUnLock/WriteUnLock methods
    procedure UnLock(context: TRWLockContext {$ifndef PUREMORMOT2} = cWrite {$endif});
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the reentrant lock has been acquired as read or write
    function IsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PRWLock = ^TRWLock;

  /// the standard reentrant lock supplied by the Operating System
  // - maps TRTLCriticalSection, i.e. calls Win32 API or pthreads library
  // - don't forget to call Init and Done to properly initialize the structure
  // - if you do require a non-reentrant/recursive lock, consider TOSLightLock
  // - same signature as TLightLock/TOSLightLock, usable as compile time alternatives
  {$ifdef USERECORDWITHMETHODS}
  TOSLock = record
  {$else}
  TOSLock = object
  {$endif USERECORDWITHMETHODS}
  private
    CS: TRTLCriticalSection;
  public
    /// to be called to setup the instance
    // - mandatory in all cases, even if TOSLock is part of a class
    procedure Init;
    /// to be called to finalize the instance
    procedure Done;
    /// enter an OS lock
    // - notice: this method IS reentrant/recursive
    procedure Lock;
      {$ifdef FPC} inline; {$endif} { Delphi can't inline EnterCriticalSection }
    /// try to enter an OS lock
    // - if returned true, caller should eventually call UnLock()
    function TryLock: boolean;
      {$ifdef FPC} inline; {$endif} { Delphi can't inline TryEnterCriticalSection }
    /// leave an OS lock
    procedure UnLock;
      {$ifdef FPC} inline; {$endif} { Delphi can't inline LeaveCriticalSection }
  end;
  POSLock = ^TOSLock;

  /// the fastest non-reentrant lock supplied by the Operating System
  // - calls Slim Reader/Writer (SRW) Win32 API in exclusive mode or directly
  // the pthread_mutex_*() library calls in non-recursive/fast mode on Linux
  // - on XP, where SRW are not available, fallback to a TLightLock
  // - on non-Linux POSIX, fallback to regular cthreads/TRTLCriticalSection
  // - don't forget to call Init and Done to properly initialize the structure
  // - to protect a very small code section of a few CPU cycles with no Init/Done
  // needed, and a lower footprint, you may consider our TLightLock
  // - same signature as TOSLock/TLightLock, usable as compile time alternatives
  // - warning: non-reentrant, i.e. nested Lock calls would block, as TLightLock
  // - no TryLock is defined on Windows, because TryAcquireSRWLockExclusive()
  // raised some unexpected EExternalException C000026 NT_STATUS_RESOURCE_NOT_OWNED
  // ("Attempt to release mutex not owned by caller") during testing
  {$ifdef USERECORDWITHMETHODS}
  TOSLightLock = record
  {$else}
  TOSLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    fMutex: TOSLightMutex;
  public
    /// to be called to setup the instance
    // - mandatory in all cases, even if TOSLock is part of a class
    procedure Init;
    /// to be called to finalize the instance
    procedure Done;
    /// enter an OS lock
    // - warning: this method is NOT reentrant/recursive, so any nested call
    // would deadlock
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef OSPOSIX}
    /// access to raw pthread_mutex_trylock() method
    // - TryAcquireSRWLockExclusive() seems not stable on all Windows revisions
    function TryLock: boolean;
     {$ifdef HASINLINE} inline; {$endif}
    {$endif OSPOSIX}
    /// leave an OS lock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  POSLightLock = ^TOSLightLock;

  /// points to one data entry in TLockedList
  PLockedListOne = ^TLockedListOne;
  /// abstract parent of one data entry in TLockedList, storing two PLockedListOne
  // - TLockedList should store unmanaged records starting with those fields
  // - sequence field contains an incremental random-seeded 30-bit integer > 65535,
  // to avoid ABA problems when instances are recycled
  TLockedListOne = record
    next, prev: pointer;
    sequence: PtrUInt;
  end;
  /// optional callback event to finalize one TLockedListOne instance
  TOnLockedListOne = procedure(one: PLockedListOne) of object;

  /// thread-safe dual-linked list of TLockedListOne descendants with recycling
  {$ifdef USERECORDWITHMETHODS}
  TLockedList = record
  {$else}
  TLockedList = object
  {$endif USERECORDWITHMETHODS}
  public
    /// thread-safe access to the list
    Safe: TLightLock;
    /// how many TLockedListOne instances are currently stored in this list
    // - excluding the instances in the recycle bin
    Count: integer;
    /// the size of one stored instance, including its TLockedListOne header
    Size: integer;
  private
    fHead, fBin: pointer;
    fSequence: PtrUInt;
    fOnFree: TOnLockedListOne;
  public
    /// initialize the storage for an inherited TLockedListOne size
    procedure Init(onesize: PtrUInt; const onefree: TOnLockedListOne = nil);
    /// release all stored memory
    procedure Done;
    /// allocate a new PLockedListOne data instance in threadsafe O(1) process
    // - returned buffer is filled with Size zero bytes
    function New: pointer;
    /// release one PLockedListOne used data instance in threadsafe O(1) process
    function Free(one: pointer): boolean;
    /// release all TLockedListOne instances currently stored in this list
    // - without moving any of those instances into the internal recycle bin
    procedure Clear;
    /// release all to-be-recycled items available in the internal bin
    // - returns how many items have been released from the internal collector
    function EmptyBin: integer;
    /// raw access to the stored items as PLockedListOne dual-linked list
    property Head: pointer
      read fHead;
  end;
  PLockedList = ^TLockedList;

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
  // - for object-level locking, see TSynLocked which owns one such
  // instance, or call low-level fSafe := NewSynLocker in your constructor,
  // then fSafe^.DoneAndFreemem in your destructor
  // - RWUse property could replace the TRTLCriticalSection by a lighter TRWLock
  // - see also TRWLock and TObjectRWLock if the multiple read / exclusive
  // write lock is better (only if the locked process does not take too much time)
  {$ifdef USERECORDWITHMETHODS}
  TSynLocker = record
  {$else}
  TSynLocker = object
  {$endif USERECORDWITHMETHODS}
  private
    fSection: TOSLock;
    fRW: TRWLock;
    fPaddingUsedCount: byte;
    fInitialized: boolean;
    fRWUse: TSynLockerUse;
    fLockCount: integer;
    function GetVariant(Index: integer): Variant;
    procedure SetVariant(Index: integer; const Value: Variant);
    function GetInt64(Index: integer): Int64;
    procedure SetInt64(Index: integer; const Value: Int64);
    function GetBool(Index: integer): boolean;
    procedure SetBool(Index: integer; const Value: boolean);
    function GetUnlockedInt64(Index: integer): Int64;
    procedure SetUnlockedInt64(Index: integer; const Value: Int64);
    function GetPointer(Index: integer): pointer;
    procedure SetPointer(Index: integer; const Value: pointer);
    function GetUtf8(Index: integer): RawUtf8;
    procedure SetUtf8(Index: integer; const Value: RawUtf8);
    function GetIsLocked: boolean;
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - warning: if RWUse=uRWLock, this method will use the internal TRWLock
    // - defined in protected section for better inlining and to fix a Delphi
    // compiler bug about warning a missing Windows unit in the uses classes
    procedure RWLock(context: TRWLockContext);
      {$ifdef HASSAFEINLINE} inline; {$endif}
    procedure RWUnLock(context: TRWLockContext);
      {$ifdef HASSAFEINLINE} inline; {$endif}
  public
    /// internal padding data, also used to store up to 7 variant values
    // - this memory buffer will ensure no CPU cache line mixup occurs
    // - you should not use this field directly, but rather the Locked[],
    // LockedInt64[], LockedUtf8[] or LockedPointer[] methods
    // - if you want to access those array values, ensure you protect them
    // using a Safe.Lock; try ... Padding[n] ... finally Safe.Unlock structure,
    // and maintain the PaddingUsedCount property accurately
    Padding: array[0..6] of TSynVarData;
    /// initialize the mutex
    // - calling this method is mandatory (e.g. in the class constructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Init;
    /// initialize the mutex if this TSynLocker instance is filled with zeros
    procedure InitFromClass;
       {$ifdef FPC} inline; {$endif} { Delphi makes warning about Windows unit }
    /// finalize the mutex
    // - calling this method is mandatory (e.g. in the class destructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Done;
    /// finalize the mutex, and call FreeMem() on the pointer of this instance
    // - should have been initiazed with a NewSynLocker call
    procedure DoneAndFreeMem;
    /// low-level acquisition of the lock, as RWLock(cReadOnly)
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - warning: with RWUse=uRWLock, a nested Lock call would deadlock, but not
    // nested ReadLock calls
    procedure ReadLock;
    /// low-level release of the lock, as RWUnLock(cReadOnly)
    procedure ReadUnLock;
    /// low-level acquisition of the lock, as RWLock(cReadWrite)
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - with RWUse=uRWLock, a nested Lock call would not deadlock
    procedure ReadWriteLock;
    /// low-level release of the lock, as RWUnLock(cReadWrite)
    procedure ReadWriteUnLock;
    /// lock the instance for exclusive access, as RWLock(cWrite)
    // - is re-entrant from the same thread i.e. you can nest Lock/UnLock calls
    // - warning: with RWUse=uRWLock, would deadlock after a nested ReadLock,
    // but not after ReadWriteLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! Safe.Lock;
    // ! try
    // !   ...
    // ! finally
    // !   Safe.Unlock;
    // ! end;
    procedure Lock;
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
    /// will try to acquire the mutex for a given time
    // - just wait and return false if RWUse is not the default uSharedLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLockMS(100) then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLockMS(retryms: integer; terminated: PBoolean = nil;
      tix64: Int64 = 0): boolean;
    /// release the instance for exclusive access, as RWUnLock(cWrite)
    // - each Lock/TryLock should have its exact UnLock opposite, so a
    // try..finally block is mandatory for safe code
    procedure UnLock; overload;
    /// will enter the mutex until the IUnknown reference is released
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var
    // !  LockFPC: IUnknown;
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
    /// number of values stored in the internal Padding[] array
    // - equals 0 if no value is actually stored, or a 1..7 number otherwise
    // - you should not have to use this field, but for optimized low-level
    // direct access to Padding[] values, within a Lock/UnLock safe block
    property PaddingUsedCount: byte
      read fPaddingUsedCount write fPaddingUsedCount;
    /// returns true if the mutex is currently locked by another thread
    // - with RWUse=uRWLock, any lock (even ReadOnlyLock) would return true
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
    // - pointers will be stored internally as a varAny variant
    // - returns nil if the Index is out of range, or does not store a pointer
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedPointer[Index: integer]: pointer
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
    // - returns the previously stored value
    // - if the internal value is not defined yet, would return 0 as default
    function LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
    /// safe locked in-place exchange of a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - returns the previous stored value, or null if the Index is out of range
    function LockedExchange(Index: integer; const Value: variant): variant;
    /// safe locked in-place exchange of a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - pointers will be stored internally as a varAny variant
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

  /// raw class used by TSynLocker.ProtectMethod and TAutoLocker.ProtectMethod
  TAutoLock = class(TInterfacedObject)
  protected
    fLock: PSynLocker;
  public
    constructor Create(aLock: PSynLocker);
    destructor Destroy; override;
  end;

  /// our lightweight cross-platform TEvent-like component
  // - on Windows, calls directly the CreateEvent/ResetEvent/SetEvent API
  // - on Linux, will use eventfd() in blocking and non-semaphore mode
  // - on other POSIX, will use PRTLEvent which is lighter than TEvent BasicEvent
  TSynEvent = class(TSynPersistent)
  protected
    fHandle: pointer; // Windows THandle or FPC PRTLEvent
    {$ifdef OSLINUX}
    fFD: integer;     // for eventfd()
    {$endif OSLINUX}
    fNotified: boolean;
  public
    /// initialize an instance of cross-platform event
    constructor Create; override;
    /// finalize this instance of cross-platform event
    destructor Destroy; override;
    /// ignore any pending events, so that WaitFor will be set on next SetEvent
    procedure ResetEvent;
      {$ifdef OSPOSIX} inline; {$endif}
    /// trigger any pending event, releasing the WaitFor/WaitForEver methods
    procedure SetEvent;
      {$ifdef OSPOSIX} inline; {$endif}
    /// wait until SetEvent is called from another thread, with a maximum time
    // - returns true if was signaled by SetEvent, or false on timeout
    // - WARNING: you should wait from a single thread at once
    function WaitFor(TimeoutMS: integer): boolean;
      {$ifdef OSPOSIX} inline; {$endif}
    /// wait until SetEvent is called from another thread, with no maximum time
    // - returns true if was signaled by SetEvent, or false if aborted/destroyed
    function WaitForEver: boolean;
      {$ifdef OSPOSIX} inline; {$endif}
    /// wait until SetEvent is called, calling CheckSynchronize() on main thread
    // - returns true if was signaled by SetEvent, or false on timeout
    function WaitForSafe(TimeoutMS: integer): boolean;
    /// calls SleepHiRes() in steps while checking terminated flag and this event
    function SleepStep(var start: Int64; terminated: PBoolean): Int64;
    /// could be used to tune your algorithm if the eventfd() API is used
    function IsEventFD: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// a thread-safe class with a virtual constructor and properties persistence
  // - publishes a TSynLocker instance, and its OS lock and padding fields
  // - consider a TLightLock field as lighter options, or a R/W lock with
  // TObjectRWLock and TObjectRWLightLock classes, or even a TObjectOSLightLock
  // or a TObjectOSLock if you don't need additional TSynLocker fields/features
  // - TSynLockedWithRttiMethods would add paranoid JSON persistence lock
  TSynLocked = class(TSynPersistent)
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the instance, and its associated lock
    constructor Create; override;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    property Safe: PSynLocker
      read fSafe;
    /// could be used as a short-cut to Safe^.Lock
    procedure Lock;
      {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Safe^.UnLock
    procedure Unlock;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// a thread-safe class with a virtual constructor and properties persistence
  // - publishes one Operating System standard lock without the TSynLocker overhead
  // - on high contention, for proper padding over the 64-bytes CPU cache line,
  // you should add at least 36 bytes on CPU32 (i.e. 9 integer/pointer fields)
  TObjectOSLock = class(TSynPersistent)
  protected
    fSafe: TOSLock; // TRTLCriticalSection with no padding (24 bytes on CPU32)
  public
    /// initialize the instance, and its associated OS lock
    constructor Create; override;
    /// finalize the instance, and its associated OS lock
    destructor Destroy; override;
    /// access to the associated non-reentrant Operating System lock instance
    property Safe: TOSLock
      read fSafe;
    /// could be used as a short-cut to Safe^.Lock
    procedure Lock;
      {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Safe^.UnLock
    procedure Unlock;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// a thread-safe class with a virtual constructor and properties persistence
  // - publishes the fastest available non-reentrant Operating System lock
  TObjectOSLightLock = class(TSynPersistent)
  protected
    fSafe: TOSLightLock; // = TOSLightMutex = SRW lock or direct pthread mutex
  public
    /// initialize the instance, and its associated OS lock
    constructor Create; override;
    /// finalize the instance, and its associated OS lock
    destructor Destroy; override;
    /// access to the associated non-reentrant Operating System lock instance
    property Safe: TOSLightLock
      read fSafe;
    /// could be used as a short-cut to Safe^.Lock
    procedure Lock;
      {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Safe^.UnLock
    procedure Unlock;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// a thread-safe class with a virtual constructor and properties persistence
  // - publishes a non-upgradable multiple Read / exclusive Write TRWLightLock
  TObjectRWLightLock = class(TSynPersistent)
  protected
    fSafe: TRWLightLock;
  public
    /// access to the associated non-upgradable TRWLightLock instance
    // - call Safe methods to protect multi-thread access on this storage
    property Safe: TRWLightLock
      read fSafe;
  end;

  /// a thread-safe class with a virtual constructor and properties persistence
  // - publishes an upgradable multiple Read / exclusive Write TRWLock
  TObjectRWLock = class(TSynPersistent)
  protected
    fSafe: TRWLock;
  public
    /// access to the associated upgradable and reentrant TRWLock instance
    // - call Safe methods to protect multi-thread access on this storage
    property Safe: TRWLock
      read fSafe;
  end;

/// initialize a TSynLocker instance from heap
// - call DoneandFreeMem to release the associated memory and OS mutex
// - as used e.g. by TSynLocked/TSynLockedWithRttiMethods to reduce class instance size
function NewSynLocker: PSynLocker;

type
  /// a thread-safe Pierre L'Ecuyer gsl_rng_taus2 software random generator
  // - just wrap a TLecuyer generator with a TLighLock in a 20-24 bytes structure
  // - as used by SharedRandom to implement Random32/RandomBytes/... functions
  // - see RandomLecuyer() from mormot.crypt.core.pas to setup a local instance
  {$ifdef USERECORDWITHMETHODS}
  TLecuyerThreadSafe = record
  {$else}
  TLecuyerThreadSafe = object
  {$endif USERECORDWITHMETHODS}
  public
    /// protect the Generator process
    Safe: TLightLock;
    /// all methods just redirect to this instance, with TLightLock wrapping
    Generator: TLecuyer;
    /// compute the next 32-bit generated value
    function Next: cardinal;
    /// compute a 64-bit floating point value
    function NextDouble: double;
    /// compute a 64-bit integer value
    function NextQWord: QWord;
    /// XOR some memory buffer with random bytes
    procedure Fill(dest: pointer; count: integer);
    /// fill some string[31] with 7-bit ASCII random text
    procedure FillShort31(var dest: TShort31);
    /// seed this gsl_rng_taus2 generator
    procedure Seed(entropy: pointer; entropylen: PtrInt);
  end;

  TThreadIDDynArray = array of TThreadID;

var
  /// a global thread-safe Pierre L'Ecuyer gsl_rng_taus2 software random generator
  // - called e.g. by Random32/Random31/Random64/RandomDouble/RandomBytes functions
  // - you can always seed and use your own TLecuyer (threadvar) instance, if needed
  SharedRandom: TLecuyerThreadSafe;

/// fast compute of some 32-bit random value, using the gsl_rng_taus2 generator
// - this function will use well documented and proven Pierre L'Ecuyer software
// generator - which happens to be faster (and safer) than RDRAND opcode (which
// is used for seeding anyway)
// - note that TAesPrng.Main.Random32() cryptographic-level seems pointless for
// only 32-bit of output - and it is twice slower (even with AES-NI)
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random32: cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute of a 32-bit random value <> 0, using the gsl_rng_taus2 generator
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random32Not0: cardinal;

/// fast compute of some 31-bit random value, using the gsl_rng_taus2 generator
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random31: integer;
  {$ifdef HASINLINE}inline;{$endif}

/// compute of a 31-bit random value <> 0, using the gsl_rng_taus2 generator
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random31Not0: integer;

/// fast compute of a 64-bit random value, using the gsl_rng_taus2 generator
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random64: QWord;
  {$ifdef HASINLINE}inline;{$endif}

/// fast compute of bounded 32-bit random value, using the gsl_rng_taus2 generator
// - returns 0 <= Random32(max) < max, calling the overloaded Random32 function
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function Random32(max: cardinal): cardinal; overload;

/// fast compute of a 64-bit random floating point, using the gsl_rng_taus2 generator
// - returns a random value in range [0..1)
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
function RandomDouble: double;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a memory buffer with random bytes from the gsl_rng_taus2 generator
// - this method is good enough e.g. for padding or generating test data
// - consider cryptographic-level mormot.core.crypt TAesPrng.Main.FillRandom()
// method or Random128() function to initialize a safe secret key, nonce or IV
// - will actually XOR the Dest buffer with TLecuyer numbers
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
procedure RandomBytes(Dest: pointer; Count: integer);

/// fill a RawByteString with random bytes from the gsl_rng_taus2 generator
// - content is really binary, i.e. would contain the whole #0..#255 byte range
// - see also e.g. RandomAnsi7() or RandomIdentifier() in mormot.core.text.pas
function RandomByteString(Count: integer; var Dest;
  CodePage: cardinal = CP_RAWBYTESTRING): pointer;

/// fill some string[31] with 7-bit ASCII random text
// - thread-safe function calling SharedRandom - whereas the RTL Random() is not
procedure RandomShort31(var dest: TShort31);

{$ifndef PUREMORMOT2}
/// fill some 32-bit memory buffer with values from the gsl_rng_taus2 generator
// - the destination buffer is expected to be allocated as 32-bit items
procedure FillRandom(Dest: PCardinal; CardinalCount: integer);
{$endif PUREMORMOT2}

/// compute a random UUid value from the RandomBytes() generator and RFC 4122
procedure RandomGuid(out result: TGuid); overload;

/// compute a random UUid value from the RandomBytes() generator and RFC 4122
function RandomGuid: TGuid; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// mark a 128-bit random binary into a UUid value according to RFC 4122
procedure MakeRandomGuid(u: PHash128);
  {$ifdef HASINLINE}inline;{$endif}

/// check if the supplied UUid value was randomly-generated according to RFC 4122
function IsRandomGuid(u: PHash128): boolean;

/// re-seed the global gsl_rng_taus2 Random32/RandomBytes generator
// - use XorEntropy() and optional entropy/entropylen as derivation source
procedure Random32Seed(entropy: pointer = nil; entropylen: PtrInt = 0);

{$ifdef OSPOSIX}
var
  /// could be set to TRUE to force SleepHiRes(0) to call the POSIX sched_yield
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
// - warning: wait typically for the next system timer interrupt (on Windows,
// 16ms by default); as a consequence, never rely on the relative ms delay
// to guess the elapsed time, but compare with on absolute GetTickCount64; for
// the very same reason, it won't retry on any ESysEINTR (not as RTL's Sleep)
procedure SleepHiRes(ms: cardinal); overload;

/// similar to Windows sleep() API call, but truly cross-platform and checking
// the Terminated flag during its wait for quick abort response
// - returns true if terminated^ was set to true (terminatedvalue)
function SleepHiRes(ms: cardinal; var terminated: boolean;
  terminatedvalue: boolean = true): boolean; overload;

/// call SleepHiRes() taking count of the activity, in 0/1/5/50/120-250 ms steps
// - range is agressively designed burning some CPU in favor of responsiveness
// - should reset start := 0 when some activity occurred, or start := -1 on
// Windows to avoid any SleepHiRes(0) = SwitchToThread call
// - would optionally return if terminated^ is set, or event is signaled
// - returns the current GetTickCount64 value
function SleepStep(var start: Int64; terminated: PBoolean = nil): Int64;

/// compute optimal sleep time as 0/1/5/50 then 120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
function SleepDelay(elapsed: PtrInt): PtrInt;

/// compute optimal sleep time as SleepStep, in 0/1/5/50/120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
// - start=0 would fill its value with tix; start<0 would fill its value with
// tix-50 so that SleepDelay() would never call SleepHiRes(0)
function SleepStepTime(var start, tix: Int64; endtix: PInt64 = nil): PtrInt;

/// similar to Windows SwitchToThread API call, to be truly cross-platform
// - call the homonymous API on Windows
// - call direclty the sched_yield Linux syscall or the FPC RTL on BSD
// - you should not call this function in your own code, especially since
// sched_yield is reported to be unfair and misleading by Linux kernel devs
procedure SwitchToThread;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// try LockedExc() in a loop, calling SwitchToThread after some spinning
procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);

/// wrapper to implement a thread-safe T*ObjArray dynamic array storage
// - warning: aCount^ should be a 32-bit "integer" variable, not a PtrInt
function ObjArrayAdd(var aObjArray; aItem: TObject;
  var aSafe: TLightLock; aCount: PInteger = nil): PtrInt; overload;

/// wrapper to implement a thread-safe pointer dynamic array storage
// - warning: aCount^ should be a 32-bit "integer" variable, not a PtrInt
function PtrArrayDelete(var aPtrArray; aItem: pointer; var aSafe: TLightLock;
  aCount: PInteger = nil): PtrInt; overload;

/// try to kill/cancel a thread
// - on Windows, calls the TerminateThread() API
// - under Linux/FPC, calls pthread_cancel() API which is asynchronous
function RawKillThread(Thread: TThread): boolean;

type
  /// store a bitmask of logical CPU cores, as used by SetThreadMaskAffinity
  // - has 32/64-bit pointer-size on Windows, or 1024 bits on POSIX
  TCpuSet = {$ifdef OSWINDOWS} PtrUInt {$else} array[0..127] of byte {$endif};
var
  /// low-level bitmasks of logical CPU cores hosted on each hardware CPU socket
  // - filled at process startup as CpuSocketsMask[0 .. CpuSockets - 1] range
  CpuSocketsMask: array of TCpuSet;

/// fill a bitmask of CPU cores with zeros
procedure ResetCpuSet(out CpuSet: TCpuSet);
  {$ifdef HASINLINE} inline; {$endif}

/// set a particular bit in a mask of CPU cores
function SetCpuSet(var CpuSet: TCpuSet; CpuIndex: cardinal): boolean;

/// retrieve the current CPU cores masks available of the system
// - the current process may have been tuned to use only a sub-set of the cores
// e.g. via "taskset -c" on Linux
// - return the number of accessible CPU cores - i.e. GetBitsCount(CpuSet) or
// 0 if the function failed
function CurrentCpuSet(out CpuSet: TCpuSet): integer;

/// try to assign a given thread to a specific set of logical CPU core(s)
// - on Windows, calls the SetThreadAffinityMask() API
// - under Linux/FPC, calls pthread_setaffinity_np() API
function SetThreadMaskAffinity(Thread: TThread; const Mask: TCpuSet): boolean;

/// try to assign a given thread to a specific logical CPU core
// - CpuIndex should be in 0 .. SystemInfo.dwNumberOfProcessors - 1 range
function SetThreadCpuAffinity(Thread: TThread; CpuIndex: cardinal): boolean;

/// try to assign a given thread to a specific hardware CPU socket
// - SocketIndex should be in 0 .. CpuSockets - 1 range, and will use the
// CpuSocketsMask[] information retrieved during process startup
function SetThreadSocketAffinity(Thread: TThread; SocketIndex: cardinal): boolean;

/// low-level naming of a thread
// - on Windows, will raise a standard "fake" exception to notify the thread name
// - under Linux/FPC, calls pthread_setname_np() API which truncates to 16 chars
procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);

/// name the current thread so that it would be easily identified in the IDE debugger
// - could then be retrieved by CurrentThreadNameShort/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const); overload;

/// name the current thread so that it would be easily identified in the IDE debugger
// - could also be retrieved by CurrentThreadNameShort/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Name: RawUtf8); overload;

var
  /// name a thread so that it would be easily identified in the IDE debugger
  // - default implementation does nothing, unless mormot.core.log is included
  // - you can force this function to do nothing by setting the NOSETTHREADNAME
  // conditional, if you have issues with this feature when debugging your app
  // - most meaningless patterns (like 'TSql') are trimmed to reduce the
  // resulting length - which is convenient e.g. with POSIX truncation to 16 chars
  // - you can retrieve the name later on using CurrentThreadNameShort
  // - this method will register TSynLog.LogThreadName(), so threads calling it
  // should also call TSynLogFamily.OnThreadEnded/TSynLog.NotifyThreadEnded
  SetThreadName: procedure(ThreadID: TThreadID; const Format: RawUtf8;
    const Args: array of const);
  /// retrieve the thread name, as set by SetThreadName()
  // - if enough, direct CurrentThreadNameShort function is slightly faster
  // - will return the CurrentThreadNameShort^ threadvar 31 chars value or
  // the full thread name as set via mormot.core.log's SetThreadName()
  GetCurrentThreadName: function: RawUtf8;

/// low-level access to the thread name, as set by SetThreadName()
// - since threadvar can't contain managed strings, it is defined as TShort31,
// so is limited to 31 chars, which is enough since POSIX truncates to 16 chars
// and SetThreadName does trim meaningless patterns
function CurrentThreadNameShort: PShortString;

/// returns the thread id and the thread name as a ShortString
// - returns e.g. 'Thread 0001abcd [shortthreadname]'
// - for convenient use when logging or raising an exception
function GetCurrentThreadInfo: TShort63;

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
// convenient, for non time-critical process (e.g. singleton initialization
// of external libraries, or before RegisterGlobalShutdownRelease() which will
// use it anyway)
procedure GlobalLock;

/// release the giant lock for thread-safe shared process
procedure GlobalUnLock;

/// framework will register here some instances to be released eventually
// - better in this root unit than in each finalization section
// - its use is protected by the GlobalLock
function RegisterGlobalShutdownRelease(Instance: TObject;
  SearchExisting: boolean = false): pointer;


{ ****************** Unix Daemon and Windows Service Support }

type
  /// all possible states of a Windows service
  // - on POSIX, will identify only if the daemon is ssRunning or ssStopped
  TServiceState = (
    ssNotInstalled,
    ssStopped,
    ssStarting,
    ssStopping,
    ssRunning,
    ssResuming,
    ssPausing,
    ssPaused,
    ssFailed,
    ssErrorRetrievingState);
  PServiceState = ^TServiceState;
  TServiceStateDynArray = array of TServiceState;

/// return the ready to be displayed text of a TServiceState value
function ToText(st: TServiceState): PShortString; overload;

const
  /// could be used with ConsoleWrite() to notify a Windows service state
  SERVICESTATE_COLOR: array[TServiceState] of TConsoleColor = (
    ccBlue,       // NotInstalled
    ccLightRed,   // Stopped
    ccGreen,      // Starting
    ccRed,        // Stopping
    ccLightGreen, // Running
    ccGreen,      // Resuming
    ccBrown,      // Pausing
    ccWhite,      // Paused
    ccMagenta,    // Failed
    ccYellow);    // ErrorRetrievingState


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

  ERROR_FAILED_SERVICE_CONTROLLER_CONNECT = 1063;

type
  PServiceStatus = ^TServiceStatus;
  TServiceStatus = record
    dwServiceType: cardinal;
    dwCurrentState: cardinal;
    dwControlsAccepted: cardinal;
    dwWin32ExitCode: cardinal;
    dwServiceSpecificExitCode: cardinal;
    dwCheckPoint: cardinal;
    dwWaitHint: cardinal;
  end;

  PServiceStatusProcess = ^TServiceStatusProcess;
  TServiceStatusProcess = record
    Service: TServiceStatus;
    dwProcessId: cardinal;
    dwServiceFlags: cardinal;
  end;

  SC_HANDLE = THandle;
  SERVICE_STATUS_HANDLE = THandle;
  TServiceTableEntry = record
    lpServiceName: PWideChar;
    lpServiceProc: procedure(ArgCount: cardinal; Args: PPWideChar); stdcall;
  end;
  PServiceTableEntry = ^TServiceTableEntry;
  TServiceDescription = record
    lpDestription: PWideChar;
  end;

  {$Z4}
  SC_STATUS_TYPE = (SC_STATUS_PROCESS_INFO);
  {$Z1}

function OpenSCManagerW(lpMachineName, lpDatabaseName: PWideChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32;
function ChangeServiceConfig2W(hService: SC_HANDLE; dwsInfoLevel: cardinal;
  lpInfo: pointer): BOOL; stdcall; external advapi32;
function StartServiceW(hService: SC_HANDLE; dwNumServiceArgs: cardinal;
  lpServiceArgVectors: PPWideChar): BOOL; stdcall; external advapi32;
function CreateServiceW(hSCManager: SC_HANDLE;
  lpServiceName, lpDisplayName: PWideChar;
  dwDesiredAccess, dwServiceType, dwStartType, dwErrorControl: cardinal;
  lpBinaryPathName, lpLoadOrderGroup: PWideChar; lpdwTagId: LPDWORD; lpDependencies,
  lpServiceStartName, lpPassword: PWideChar): SC_HANDLE; stdcall; external advapi32;
function OpenServiceW(hSCManager: SC_HANDLE; lpServiceName: PWideChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32;
function DeleteService(hService: SC_HANDLE): BOOL; stdcall; external advapi32;
function CloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall; external advapi32;
function QueryServiceStatus(hService: SC_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function QueryServiceStatusEx(hService: SC_HANDLE;
  InfoLevel: SC_STATUS_TYPE; lpBuffer: pointer; cbBufSize: cardinal;
  var pcbBytesNeeded: cardinal): BOOL; stdcall; external advapi32;
function ControlService(hService: SC_HANDLE; dwControl: cardinal;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function RegisterServiceCtrlHandlerW(lpServiceName: PWideChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall; external advapi32;
function StartServiceCtrlDispatcherW(
  lpServiceStartTable: PServiceTableEntry): BOOL; stdcall; external advapi32;

function OpenServiceManager(const TargetComputer, DatabaseName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
function OpenServiceInstance(hSCManager: SC_HANDLE; const ServiceName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;

function GetNamedSecurityInfoW(pObjectName: PWideChar; ObjectType,
  SecurityInfo: cardinal; ppsidOwner, ppsidGroup, ppDacl, ppSacl: pointer;
  var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWord; stdcall; external advapi32;
function SetNamedSecurityInfoW(pObjectName: PWideChar; ObjectType,
  SecurityInfo: cardinal; psidOwner, psidGroup: pointer;
  pDacl, pSacl: pointer): DWord; stdcall; external advapi32;


{ *** high level classes to define and manage Windows Services }

var
  /// can be assigned from TSynLog.DoLog class method for
  // TServiceController/TService logging
  // - default is nil, i.e. disabling logging, since it may interfere with the
  // logging process of the Windows Service itself
  WindowsServiceLog: TSynLogProc;

type
  /// TServiceController class is intended to create a new Windows Service instance
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
    // start before this service (every name should be separated with ';' or an
    // empty string can be passed if there is no dependency).
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
      const TargetComputer, DatabaseName, Name, DisplayName: RawUtf8;
      const Path: TFileName;
      const OrderGroup: RawUtf8 = ''; const Dependencies: RawUtf8 = '';
      const Username: RawUtf8 = ''; const Password: RawUtf8 = '';
      DesiredAccess: cardinal = SERVICE_ALL_ACCESS;
      ServiceType: cardinal = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      StartType: cardinal = SERVICE_DEMAND_START;
      ErrorControl: cardinal = SERVICE_ERROR_NORMAL);
    /// wrapper around CreateNewService() to install the current executable as service
    class function Install(const Name, DisplayName, Description: RawUtf8;
      AutoStart: boolean; ExeName: TFileName = '';
      const Dependencies: RawUtf8 = ''; const UserName: RawUtf8 = '';
      const Password: RawUtf8 = ''): TServiceState;
    /// wrapper around CreateOpenService(SERVICE_QUERY_STATUS) and GetState
    class function CurrentState(const Name: RawUtf8): TServiceState;
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
      const TargetComputer, DataBaseName, Name: RawUtf8;
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
    // - this version expect PWideChar pointers, i.e. UTF-16 strings
    function Start(const Args: array of PWideChar): boolean;
    /// try to define the description text of this service
    function SetDescription(const Description: RawUtf8): boolean;
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
      const ServiceName, DisplayName, Description: RawUtf8;
      const Dependencies: RawUtf8 = '');
  end;

  TService = class;

  /// callback procedure for Windows Service Controller
  TServiceControlHandler = procedure(CtrlCode: cardinal); stdcall;

  /// event triggered for Control handler
  TServiceControlEvent = procedure(Sender: TService; Code: cardinal) of object;

  /// event triggered to implement the Service functionality
  TServiceEvent = procedure(Sender: TService) of object;

  /// abstract class to let an executable implement a Windows Service
  // - do not use this class directly, but TServiceSingle
  TService = class(TSynPersistent)
  protected
    fServiceName: RawUtf8;
    fDisplayName: RawUtf8;
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
    fArgsList: TRawUtf8DynArray;
    fStatusHandle: THandle;
    function GetArgCount: integer;
    function GetArgs(Idx: integer): RawUtf8;
    function GetInstalled: boolean;
    procedure SetStatus(const Value: TServiceStatus);
    procedure CtrlHandle(Code: cardinal);
    function GetControlHandler: TServiceControlHandler;
    procedure SetControlHandler(const Value: TServiceControlHandler);
    procedure ServiceProc(ArgCount: integer; Args: PPWideChar);
  public
    /// internal method redirecting to WindowsServiceLog global variable
    class procedure DoLog(Level: TSynLogLevel; Fmt: PUtf8Char;
      const Args: array of const; Instance: TObject = nil);
    /// Creates the service
    // - the service is added to the internal registered services
    // - main application must instantiate the TServiceSingle class, then call
    // the global ServiceSingleRun procedure to actually start the services
    // - caller must free the TService instance when it's no longer used
    constructor Create(const aServiceName, aDisplayName: RawUtf8); reintroduce; virtual;
    /// this method is the main service entrance, from the OS point of view
    // - it will call OnControl/OnStop/OnPause/OnResume/OnShutdown events
    // - and report the service status to the system (via ReportStatus method)
    procedure DoCtrlHandle(Code: cardinal); virtual;
    /// Reports new status to the system
    function ReportStatus(dwState, dwExitCode, dwWait: cardinal): BOOL;
    /// Installs the service in the database
    // - return true on success
    // - create a local TServiceController with the current executable file,
    // with the supplied command line parameters
    // - you can optionally append some parameters, which will be appended
    // to the
    function Install(const Params: TFileName = ''): boolean;
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

    /// Number of arguments passed to the service by the service controller
    property ArgCount: integer
      read GetArgCount;
    /// List of arguments passed to the service by the service controller
    // - Idx is in range 0..ArgCount - 1
    property Args[Idx: integer]: RawUtf8
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
    /// custom event triggered when the service receive an Interrogate command
    // - could call ReportStatus() e.g. to notify a problem
    property OnInterrogate: TServiceEvent
      read fOnInterrogate write fOnInterrogate;
    /// custom event triggered when the service is shut down
    property OnShutdown: TServiceEvent
      read fOnShutdown write fOnShutdown;
  published
    /// Name of the service. Must be unique
    property ServiceName: RawUtf8
      read fServiceName;
    /// Display name of the service
    property DisplayName: RawUtf8
      read fDisplayName write fDisplayName;
    /// Type of service
    property ServiceType: cardinal
      read fServiceType write fServiceType;
    /// Type of start of service
    property StartType: cardinal
      read fStartType write fStartType;
  end;

  /// inherit from this class if your application has a single Windows Service
  // - note that only this single-service implementation is available by now
  // - the regular way of executing services is to instantiate a TServiceSingle
  // instance (which will fill the ServiceSingle variable) and its methods,
  // then eventually call ServiceSingleRun
  TServiceSingle = class(TService)
  public
    /// will set a global function as service controller
    constructor Create(const aServiceName, aDisplayName: RawUtf8); override;
    /// will release the global service controller
    destructor Destroy; override;
  end;

var
  /// the main TServiceSingle instance running in the current executable
  // - the regular way of executing services is to instantiate a TServiceSingle
  // instance (which will fill this ServiceSingle variable) and its methods,
  // then eventually call ServiceSingleRun
  ServiceSingle: TServiceSingle = nil;

/// launch the registered Service execution
// - ServiceSingle provided by this application (most probably from
// TServiceSingle.Create) is sent to the operating system
// - returns TRUE on success
// - returns FALSE on error (to get extended information, call GetLastError)
function ServiceSingleRun: boolean;

/// convert the Control Code retrieved from Windows into a service state
// enumeration item
function CurrentStateToServiceState(CurrentState: cardinal): TServiceState;

/// return the ProcessID of a given service, by name
function GetServicePid(const aServiceName: RawUtf8;
  aServiceState: PServiceState = nil): cardinal;

/// try to gently stop a given Windows console app from its ProcessID
// - will send a Ctrl-C event (acquiring the process console)
// - won't wait after this event if waitseconds = 0
function CancelProcess(pid: cardinal; waitseconds: integer): boolean;

/// try to gently quit a Windows process from its ProcessID
// - will send a WM_QUIT message to all its threads
function QuitProcess(pid: cardinal; waitseconds: integer): boolean;

/// forcibly terminate a Windows process from its ProcessID
// - call TerminateProcess() and wait for its ending
function KillProcess(pid: cardinal; waitseconds: integer = 30): boolean;

/// install a Windows event handler for Ctrl+C pressed on the Console
function HandleCtrlC(const OnClose: TThreadMethod): boolean;

/// define a Windows Job with the flags to close associated processes together
// - warning: parent process should include the CREATE_BREAKAWAY_FROM_JOB flag
// - will create a new Job with JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE and
// JOB_OBJECT_LIMIT_BREAKAWAY_OK
// - allowSilentBreakaway=true will set JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK
// to avoid any unexpected behavior in sensitive child process (which may not
// include CREATE_BREAKAWAY_FROM_JOB themselves, e.g. ServiceUI.exe)
// - you should later call CloseHandle() on the returned handle, if not 0 
function CreateJobToClose(parentpid: cardinal; const ctxt: ShortString;
  allowSilentBreakaway: boolean = false): THandle;

/// associate a process to a Windows Job created by CreateJobToClose()
// - is called usually just after CreateJobToClose()
function AssignJobToProcess(job, process: THandle; const ctxt: ShortString): boolean;

{$else}

/// low-level function able to properly run or fork the current process
// then execute the start/stop methods of a TSynDaemon / TDDDDaemon instance
// - dofork will create e.g. a /run/.[ProgramName][ProgramFilePathHash].pid file
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; const onlog: TSynLogProc = nil;
  const servicename: string = '');

/// kill a process previously created by RunUntilSigTerminated(dofork=true)
// - will lookup a local /run/.[ProgramName][ProgramFilePathHash].pid file
// to retrieve the actual PID to be killed, then send a SIGTERM, and wait
// waitseconds for the .pid file to disapear
// - returns true on success, false on error (e.g. no valid .pid file or
// the file didn't disappear, which may mean that the daemon is broken)
function RunUntilSigTerminatedForKill(waitseconds: integer = 30): boolean;

var
  /// optional folder where the .pid is created by RunUntilSigTerminatedPidFile()
  // - should include a trailing '/' character
  // - will be used insted of /run if the current executable folder is read/only
  RunUntilSigTerminatedPidFilePath: TFileName;
  /// optional genuine number to identify this executable instance
  // - is filled with Executable.ProgramFilePath hash by default
  RunUntilSigTerminatedPidFileGenuine: cardinal;
  /// the computed RunUntilSigTerminatedPidFile result value, used as cache
  RunUntilSigTerminatedPidFileName: TFileName;

/// local .pid file name as created by RunUntilSigTerminated(dofork=true)
// - typically return /run/.[ProgramName][ProgramFilePathHash].pid file name
function RunUntilSigTerminatedPidFile: TFileName;

/// check the local .pid file to return either ssRunning or ssStopped
function RunUntilSigTerminatedState: TServiceState;

var
  /// once SynDaemonIntercept has been called, this global variable
  // contains the SIGQUIT / SIGTERM / SIGINT received signal
  SynDaemonTerminated: integer;

/// enable low-level interception of executable stop signals
// - any SIGQUIT / SIGTERM / SIGINT signal will set appropriately the global
// SynDaemonTerminated variable, with an optional logged entry to log
// - called e.g. by RunUntilSigTerminated() or ConsoleWaitForEnterKey()
// - you can call this method several times with no issue
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure SynDaemonIntercept(const onlog: TSynLogProc = nil);

/// disable SIGPIPE signal for the current process
// - is called e.g. by NewOpenSslNetTls since the OpenSsl TLS layer does not
// (yet) use MSG_NOSIGNAL when accessing the socket
procedure SigPipeIntercept;

{$endif OSWINDOWS}

/// change the current UID/GID to another user, by name
// - only implemented on POSIX by now
function DropPriviledges(const UserName: RawUtf8 = 'nobody'): boolean;

/// changes the root directory of the calling process
// - only implemented on POSIX by now
function ChangeRoot(const FolderName: RawUtf8): boolean;

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
  // - up to 255 arguments are usually stored on the stack as function result
  // - POSIX has almost no limit (since command line is intensively used in scripts),
  // but some Windows versions seemed to limit to 8KB total as a whole
  TParseCommandsArgs = array[byte] of PAnsiChar;
  PParseCommandsArgs = ^TParseCommandsArgs;

const
  /// identifies some bash-specific processing
  PARSECOMMAND_BASH =
    [pcHasRedirection .. pcHasShellVariable];

  /// identifies obvious invalid content
  PARSECOMMAND_ERROR =
    [pcUnbalancedSingleQuote .. pcHasEndingBackSlash];

  /// let ParseCommandArgs/ExtractExecutableName/ExtractCommandArgs follow the
  // current running OS command-line expectations by default
  PARSCOMMAND_POSIX = {$ifdef OSWINDOWS} false {$else} true {$endif};

/// low-level parsing of a RunCommand() execution command
// - parse and fill argv^[0 .. argc^ - 1] with corresponding arguments, after
// un-escaping and un-quoting if applicable, using temp^ to store the content,
// and argv^[argc^] = nil, as expected by low-level OS exec() syscall parameters
// - if argv=nil, do only the parsing, not the argument extraction - could be
// used for fast validation of the command line syntax
// - you can force arguments OS flavor using the posix parameter - note that
// Windows parsing is not consistent by itself (e.g. double quoting or
// escaping depends on the actual executable called) so returned flags
// should be considered as indicative only with posix=false
// - you can check for errors with result * PARSECOMMAND_ERROR <> []
// - warning: argc^ should be a 32-bit "integer" variable, not a PtrInt
function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs = nil;
  argc: PInteger = nil; temp: PRawUtf8 = nil;
  posix: boolean = PARSCOMMAND_POSIX): TParseCommands;

/// high-level extraction of the executable of a RunCommand() execution command
// - returns the first parameter returned by ParseCommandArgs()
// - returns '' if cmd is incorrectly formatted
function ExtractExecutableName(const cmd: RawUtf8;
  posix: boolean = PARSCOMMAND_POSIX): RawUtf8;

/// high-level extraction of all parts of a RunCommand() execution command
// - output param[0] is the executable name, and other param[] are the
// actual command line arguments, just like the ParamStr() RTL function
// - param is left nil on error, with result * PARSECOMMAND_ERROR <> []
function ExtractCommandArgs(const cmd: RawUtf8; out param: TRawUtf8DynArray;
  posix: boolean = PARSCOMMAND_POSIX): TParseCommands;

type
  /// callback used by RunRedirect() to notify of console output at runtime
  // - newly console output text is given as raw bytes sent by the application,
  // with no conversion: on POSIX, it is likely to be UTF-8 but on Windows it
  // depends on the actual program so most generate CP_OEM but others could use
  // the system code page or even UTF-16 binary with BOM (!) - so you may consider
  // calling AnsiToUtf8() from mormot.core.unicode.pas with the proper code page
  // - should return true to stop/abort the execution, or false to continue
  // - is called once when the process is started, with text='', ignoring its return
  // - on idle state (each 200ms), is called with text='' to allow execution abort
  // - the raw process ID (dword on Windows, cint on POSIX) is also supplied
  // - see RedirectToConsole to write to the console e.g. for debugging purpose
  TOnRedirect = function(const text: RawByteString; pid: cardinal): boolean of object;

  /// define how RunCommand() and RunRedirect() run their sub-process
  // - roEnvAddExisting is used when the env pairs should be added to the
  // existing system environment variable
  // - roWinJobCloseChildren will use the CREATE_BREAKAWAY_FROM_JOB flag and
  // run CreateJobToClose(allowSilentBreakaway=true) and AssignJobToProcess()
  // on the new process so that any of its future children would be
  // synchronized and closed with their father, in a relaxed way
  // - on Windows, will create its own console and its own execution group, unless
  // roWinNoProcessDetach is defined - e.g. as RUN_CMD for RunCommand/RunRedirect
  // - roWinNewConsole won't inherit the parent console, but have its own console
  // - roWinKeepProcessOnTimeout won't make Ctrl+C / WM_QUIT or TerminateProcess
  TRunOptions = set of (
    roEnvAddExisting,
    roWinJobCloseChildren,
    roWinNoProcessDetach,
    roWinNewConsole,
    roWinKeepProcessOnTimeout);

const
  /// the default options for RunCommand() and RunRedirect() transient execution
  // - detaching the process from the console and Job group by default does only
  // make sense for RunProcess() which will use TRunOptions = []
  RUN_CMD = [roWinNoProcessDetach];

/// like SysUtils.ExecuteProcess, but allowing not to wait for the process to finish
// - optional env value follows 'n1=v1'#0'n2=v2'#0'n3=v3'#0#0 Windows layout
// - by default, TRunOptions = [] so would detach from the current process
// console and Job group as we would expect from launch a new stand-alone process
function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2: TFileName = ''; const arg3: TFileName = '';
  const arg4: TFileName = ''; const arg5: TFileName = '';
  const env: TFileName = ''; options: TRunOptions = []): integer;

/// like fpSystem function, but cross-compiler and cross-platform
// - under POSIX, calls bash only if needed, after ParseCommandArgs() analysis
// - on Windows, consider RunCommandWin() specific version with more parameters
// - optional env should be encoded as 'n1=v1'#0'n2=v2'#0#0 pairs
// - TRunOptions = RUN_CMD as expected from executing a transient command
// - parsed^ is implemented on POSIX only, and processhandle^ on Windows only
// - under Windows (especially Windows 10/11), creating a process can be dead
// slow https://randomascii.wordpress.com/2019/04/21/on2-in-createprocess
function RunCommand(const cmd: TFileName; waitfor: boolean = true;
  const env: TFileName = ''; options: TRunOptions = RUN_CMD;
  {$ifdef OSPOSIX} parsed: PParseCommands = nil
  {$else} processhandle: PHandle = nil {$endif OSPOSIX}): integer;

/// execute a command, returning its output console as text
// - calls CreateProcessW on Windows (via our RunCommandWin function), and
// FPC RTL popen/pclose on POSIX to be truly cross-platform
// - return '' on cmd execution error, or the whole output console text content
// with no conversion (unless setresult is false - see below)
// - on POSIX, result is likely to be UTF-8 but on Windows it depends on each
// program so most generate CP_OEM, but others could use the system code page or
// even UTF-16 binary with BOM - so you may consider calling AnsiToUtf8() from
// mormot.core.unicode.pas with the proper code page generated by this command
// - will optionally call onoutput() to notify the new output state; aborting if
// onoutput() callback returns true - see RedirectToConsole global callback
// - will abort once waitfordelayms expires - if not its default INFINITE
// - force setresult=false if you only need onouput() and will discard the result
// - optional env is Windows only, (FPC popen does not support it), and should
// be encoded as name=value#0 pairs
// - you can specify a wrkdir if the path specified by cmd is not good enough
// - TRunOptions = RUN_CMD as expected from executing a transient command
// - warning: exitcode^ should be a 32-bit "integer" variable, not a PtrInt
function RunRedirect(const cmd: TFileName; exitcode: PInteger = nil;
  const onoutput: TOnRedirect = nil; waitfordelayms: cardinal = INFINITE;
  setresult: boolean = true; const env: TFileName = '';
  const wrkdir: TFileName = ''; options: TRunOptions = RUN_CMD): RawByteString;

var
  /// a RunRedirect() callback for console output e.g. for debugging purpose
  // - you should call at least once AllocConsole to setup its content
  RedirectToConsole: TOnRedirect;

  /// global variable to define how many seconds RunRedirect/RunCommand should
  // wait for gracefull/soft process termination
  // - set 0 to disable gracefull exit, and force hard SIGKILL/TerminateProcess
  RunAbortTimeoutSecs: integer = 5;

{$ifdef OSWINDOWS}
type
  /// for use as RunCommandWin() parameter with no "uses Windows" clause
  TWinProcessInfo = Windows.TProcessInformation;

  /// how RunRedirect/RunCommand/RunCommandWin could try to gracefully terminate
  // - ramCtrlC calls CancelProcess(), i.e. send CTRL_C_EVENT
  // - ramQuit calls QuitProcess(), i.e. send WM_QUIT on all the process threads
  // - note that hard TerminateProcess is always called after RunAbortTimeoutSecs
  // timeout, or if this set of methods is void - unless the
  // roWinKeepProcessOnTimeout option has been specified
  TRunAbortMethod = (ramCtrlC, ramQuit);

/// Windows-specific RunCommand/RunRedirect function
// - returning raw TWinProcessInfo and accepting some additional parameters
function RunCommandWin(const cmd: TFileName; waitfor: boolean;
  var processinfo: TWinProcessInfo; const env: TFileName = '';
  options: TRunOptions = RUN_CMD; waitfordelayms: cardinal = INFINITE;
  redirected: PRawByteString = nil; const onoutput: TOnRedirect = nil;
  const wrkdir: TFileName = ''; jobtoclose: PHandle = nil): integer;

{$else}
type
  /// how RunRedirect/RunCommand could try to gracefully terminate
  // - ramSigTerm send a fpkill(pid, SIGTERM) to the process
  // - note that hard SIGKILL is always sent after RunAbortTimeoutSecs timeout,
  // or if ramSigTerm was not supplied
  TRunAbortMethod = (ramSigTerm);
{$endif OSWINDOWS}
type
  /// define how RunRedirect/RunCommand should try to gracefully terminate
  TRunAbortMethods = set of TRunAbortMethod;
const
  /// by default, RunRedirect/RunCommand tries all soft killing methods
  RUNABORT_DEFAULT = [low(TRunAbortMethod) .. high(TRunAbortMethod)];
var
  /// global variable to define RunRedirect/RunCommand soft termination methods
  // - used before hard SIGKILL/TerminateProcess
  RunAbortMethods: TRunAbortMethods = RUNABORT_DEFAULT;


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

function _fmt(const Fmt: string; const Args: array of const): RawUtf8; overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure _fmt(const Fmt: string; const Args: array of const;
  var result: RawUtf8); overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure _AddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8);
var
  n: PtrInt;
begin
  n := length(Values);
  SetLength(Values, n + 1);
  Values[n] := Value;
end;

function _GetNextSpaced(var P: PAnsiChar): RawUtf8; // separated by space/feed
var
  S: PAnsiChar;
begin
  result := '';
  S := P;
  if S = nil then
    exit;
  while S^ <= ' ' do
    if S^ = #0 then
      exit
    else
      inc(S);
  P := S;
  repeat
    inc(S);
  until S^ <= ' ';
  FastSetString(result, P, S - P);
  P := S;
end;

function _GetNextCardinal(var P: PAnsiChar): PtrUInt;
var
  c: cardinal;
  S: PAnsiChar;
begin
  result := 0;
  S := P;
  if S = nil then
    exit;
  while not (S^ in ['0'..'9']) do
    if S^ = #0 then
      exit
    else
      inc(S);
  repeat
    c := ord(S^) - 48;
    if c > 9 then
      break;
    result := result * 10 + c;
    inc(S);
  until false;
  P := S;
end;

{$ifdef ISDELPHI} // missing convenient RTL function in Delphi
function TryStringToGUID(const s: string; var uuid: TGuid): boolean;
begin
  try
    uuid := StringToGUID(s);
    result := true;
  except
    result := false;
  end;
end;
{$endif ISDELPHI}

function _ShortToUuid(const text: ShortString; out uuid: TGuid): boolean;
begin
  result := (text[0] = #36) and
            TryStringToGUID('{' + string(text) + '}', uuid); // RTL
end;

procedure _AppendShortUuid(const u: TGuid; var s: ShortString);
begin
  AppendShortAnsi7String(AnsiString(LowerCase(copy(GUIDToString(u), 2, 36))), s);
end;

function TextToUuid(const text: RawUtf8; out uuid: TGuid): boolean;
var
  tmp: string[36];
begin
  result := false;
  if length(text) <> 36 then
    exit;
  tmp[0] := #36;
  MoveFast(pointer(text)^, tmp[1], 36);
  result := ShortToUuid(tmp, uuid); // may call mormot.core.text
end;

procedure UuidToText(const u: TGuid; var result: RawUtf8); // internal call
var
  tmp: ShortString;
begin
  tmp[0] := #0;
  AppendShortUuid(u, tmp); // may call mormot.core.text
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function KB(Size: Int64): TShort16;
begin
  result[0] := #0;
  AppendKb(Size, result, {withspace=}true);
end;

function KBNoSpace(Size: Int64): TShort16;
begin
  result[0] := #0;
  AppendKb(Size, result, {withspace=}false);
end;

function IsLocalHost(Host: PUtf8Char): boolean;
begin
  result := (PCardinal(Host)^ = HOST_127) or // also check for c6Localhost:
            (PCardinal(Host)^ = ord(':') + ord(':') shl 8 + ord('1') shl 16);
end;

function UriTruncLen(const Address: RawUtf8): PtrInt;
var
  l: PtrInt;
begin
  result := PtrUInt(Address);
  if result = 0 then
    exit;
  l := PStrLen(PAnsiChar(result) - _STRLEN)^;
  result := ByteScanIndex(pointer(Address), l, ord('?')); // exclude ?arguments
  if result < 0 then
    result := ByteScanIndex(pointer(Address), l, ord('#')); // exclude #anchor
  if result < 0 then
    result := l;
end;

function UriTruncAnchorLen(const Address: RawUtf8): PtrInt;
var
  l: PtrInt;
begin
  result := PtrUInt(Address);
  if result = 0 then
    exit;
  l := PStrLen(PAnsiChar(result) - _STRLEN)^;
  result := ByteScanIndex(pointer(Address), l, ord('#')); // exclude #anchor
  if result < 0 then
    result := l;
end;

function {%H-}_RawToBase64(Bin: pointer; Bytes: PtrInt; Base64Uri: boolean): RawUtf8;
begin
  raise EOSException.Create('No RawToBase64(): needs mormot.core.buffers.pas');
end;


{ ****************** Gather Operating System Information }

const // cf https://preview.changewindows.org/platforms/pc
  DESKTOP_INT: array[0 .. 19] of cardinal = (
     10240,  10586,  14393,  15063,  16299,  17134,  17763,  18362,  18363,
     19041,  19042,  19043,  19044,  19045,  22000,  22621,  22631,  26100,
     26200,  27881); // detect 26H2 Canary builds since 19/6/2025
  DESKTOP_TXT: array[0 .. high(DESKTOP_INT), 0 .. 3] of AnsiChar = (
    '1507', '1511', '1607', '1703', '1709', '1803', '1809', '1903', '1909',
    '2004', '20H2', '21H1', '21H2', '22H2', '21H2', '22H2', '23H2', '24H2',
    '25H2', '26H2'); // stored as 32-bit cardinal = array[0..3] of AnsiChar
  SERVER_INT: array[0 .. 10] of cardinal = (
    14393,  16299,  17134,  17763,  18362,  18363,  19041,  19042,  20348,
    25398,  26100);
  SERVER_TXT: array[0 .. high(SERVER_INT), 0 .. 3] of AnsiChar = (
    '1607', '1709', '1803', '1809', '1903', '1909', '2004', '20H2', '21H2',
    '23H2', '24H2');

function FindOsBuild(c: cardinal; hi: PtrInt; b, t: PCardinalArray): cardinal;
begin
  repeat
    if c >= b^[hi] then
    begin
      result := t^[hi];
      exit;
    end;
    dec(hi);
  until hi < 0;
  result := 0;
end;

procedure AppendOsBuild(const osv: TOperatingSystemVersion; dest: PAnsiChar;
  sep: AnsiChar);
var
  txt4: cardinal; // = array[0..3] of AnsiChar
begin
  if osv.os <> osWindows then
    exit;
  txt4 := osv.winbuild;
  case  osv.win of
    wTen, wTen_64, wEleven, wEleven_64: // desktop versions
      txt4 := FindOsBuild(txt4, high(DESKTOP_INT), @DESKTOP_INT, @DESKTOP_TXT);
    wServer2016, wServer2016_64, wServer2019_64, wServer2022_64, wServer2025_64:
      txt4 := FindOsBuild(txt4, high(SERVER_INT), @SERVER_INT, @SERVER_TXT);
  else
    exit;
  end;
  if txt4 = 0 then
    exit;
  if sep <> #0 then
    AppendShortChar(sep, dest); // e.g. ' 21H2' for sep=' '
  PCardinal(@dest[ord(dest[0]) + 1])^ := txt4;
  inc(dest[0], 4);
end;

function WinOsBuild(const osv: TOperatingSystemVersion; sep: AnsiChar): TShort7;
begin
  result[0] := #0;
  AppendOsBuild(osv, @result, sep);
end;

procedure AppendOsv(const osv: TOperatingSystemVersion; var dest: Shortstring);
begin
  case osv.os of
    osWindows:
      begin
        AppendShort('Windows ', dest);
        AppendShort(WINDOWS_NAME[osv.win], dest);
        AppendOsBuild(osv, @dest, ' ');
        exit;
      end;
    osOSX: // guess end-user MacOS Name from Darwin version number
      if osv.utsrelease[2] in [low(MACOS_NAME) .. high(MACOS_NAME)] then
      begin
        AppendShort('macOS ', dest);
        AppendShort(MACOS_NAME[osv.utsrelease[2]], dest);
        exit;
      end;
  end;
  AppendShort(OS_NAME[osv.os], dest);
end;

function ToText(const osv: TOperatingSystemVersion): TShort47;
begin
  result[0] := #0;
  AppendOsv(osv, result);
end;

function ToTextU(const osv: TOperatingSystemVersion): RawUtf8;
var
  tmp: TShort47;
begin
  tmp[0] := #0;
  AppendOsv(osv, tmp);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function OsvToShort(const osv: TOperatingSystemVersion): PShortString;
begin
  result := nil;
  case osv.os of
    osWindows:
      result := @WINDOWS_NAME[osv.win];
    osOSX:
      if osv.utsrelease[2] in [low(MACOS_NAME) .. high(MACOS_NAME)] then
        result := @MACOS_NAME[osv.utsrelease[2]];
  end;
  if (result = nil) or
     (result^ = '') then
    result := @OS_NAME[osv.os];
end;

function ToTextOS(osint32: integer): TShort47;
var
  osv: TOperatingSystemVersion absolute osint32;
begin
  result[0] := #0;
  if osint32 = 0 then
    exit;
  AppendOsv(osv, result);
  if (osv.os = osWindows) and
     (osv.winbuild <> 0) then
  begin
    // include the Windows build number, e.g. 'Windows 11 64bit 21H2 22000'
    AppendShortChar(' ', @result);
    AppendShortCardinal(osv.winbuild, result);
  end;
  if (osv.os >= osLinux) and
     (osv.utsrelease[2] <> 0) then
  begin
    // include kernel number to the distribution name
    if osv.os in (OS_LINUX - [osAndroid]) then
      AppendShort(' Linux ', result) // e.g. 'Ubuntu Linux 5.4.0'
    else
      AppendShortChar(' ', @result);
    AppendShortCardinal(osv.utsrelease[2], result);
    AppendShortChar('.', @result);
    AppendShortCardinal(osv.utsrelease[1], result);
    AppendShortChar('.', @result);
    AppendShortCardinal(osv.utsrelease[0], result);
  end;
end;

function ToTextOSU(osint32: integer): RawUtf8;
begin
  ShortStringToAnsi7String(ToTextOS(osint32), result);
end;

function MatchOS(os: TOperatingSystem): boolean;
var
  current: TOperatingSystem;
begin
  current := OS_KIND;
  if (os = osUnknown) or
     (current = osUnknown) or
     (os = current) then
    result := true // exact match
  else
    case os of // search by family
      osPosix:
        result := current <> osWindows;
      osLinux:
        result := current in OS_LINUX;
    else
      result := false;
    end;
end;

function LinuxDistribution(os: TOperatingSystem): TLinuxDistribution;
begin
  for result := succ(low(result)) to high(result) do
    if os in LINUX_DIST[result] then
      exit;
  result := ldNotLinux;
end;

// PUtf8Char for system error text reduces the executable size vs RawUtf8
// on Delphi (aligned to 4 bytes), but not on FPC (aligned to 16 bytes), and
// enumerates let compiler generate the smallest code
// - all errors are cross-platform, e.g. when used in centralized servers

const
  NULL_STR: string[1] = '';

function _GetEnumNameRtti(Info: pointer; Value: integer): PShortString;
begin
  // naive version - will properly be implemented in mormot.core.rtti.pas anyway
  result := @NULL_STR;
  // arm32 is overcomplex and would rather use typinfo and mormot.core.rtti
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  if Value < 0 then
    exit;
  // quickly jump over Kind + NameLen + Name +  OrdType + Min + Max + EnumBaseType
  result := @PAnsiChar(Info)[PByteArray(Info)[1] + (1 + 1 + 1 + 4 + 4 +
    SizeOf(pointer) {$ifdef FPC_PROVIDE_ATTR_TABLE} + SizeOf(pointer) {$endif})];
  if Value > 0 then
    repeat
      inc(PByte(result), ord(result^[0]) + 1); // next ShortString
      dec(Value);
    until Value = 0;
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

type
  // https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes
  TWinError0 = ( // 0..39
    SUCCESS, INVALID_FUNCTION, FILE_NOT_FOUND, PATH_NOT_FOUND,
    TOO_MANY_OPEN_FILES, ACCESS_DENIED, INVALID_HANDLE, ARENA_TRASHED,
    NOT_ENOUGH_MEMORY, INVALID_BLOCK, BAD_ENVIRONMENT, BAD_FORMAT,
    INVALID_ACCESS, INVALID_DATA, OUTOFMEMORY, INVALID_DRIVE,
    CURRENT_DIRECTORY, NOT_SAME_DEVICE, NO_MORE_FILES, WRITE_PROTECT,
    BAD_UNIT, NOT_READY, BAD_COMMAND, CRC, BAD_LENGTH, SEEK,
    NOT_DOS_DISK, SECTOR_NOT_FOUND, OUT_OF_PAPER, WRITE_FAULT,
    READ_FAULT, GEN_FAILURE, SHARING_VIOLATION, LOCK_VIOLATION,
    WRONG_DISK, FAIL_I35, SHARING_BUFFER_EXCEEDED, FAIL_I37, HANDLE_EOF,
    HANDLE_DISK_FULL);
  TWinError50 = ( // 50..55
    NOT_SUPPORTED, REM_NOT_LIST, DUP_NAME, BAD_NETPATH,
    NETWORK_BUSY, DEV_NOT_EXIST);
  TWinError80 = ( // 80..89
    FILE_EXISTS, FAIL_I81, CANNOT_MAKE, FAIL_I83, OUT_OF_STRUCTURES,
    ALREADY_ASSIGNED, INVALID_PASSWORD, INVALID_PARAMETER,
    NET_WRITE_FAULT, NO_PROC_SLOTS);
  TWinError108 = ( // 108..129
    DRIVE_LOCKED, BROKEN_PIPE, OPEN_FAILED, BUFFER_OVERFLOW,
    DISK_FULL, NO_MORE_SEARCH_HANDLES, INVALID_TARGET_HANDLE, FAIL_I115,
    FAIL_I116, INVALID_CATEGORY, INVALID_VERIFY_SWITCH, BAD_DRIVER_LEVEL,
    CALL_NOT_IMPLEMENTED, SEM_TIMEOUT, INSUFFICIENT_BUFFER,
    INVALID_NAME, INVALID_LEVEL, NO_VOLUME_LABEL, MOD_NOT_FOUND,
    PROC_NOT_FOUND, WAIT_NO_CHILDREN, CHILD_NOT_COMPLETE);
  TWinError995 = ( // 995..1013
    OPERATION_ABORTED, IO_INCOMPLETE, IO_PENDING, NOACCESS, SWAPERROR,
    FAIL_I1000, STACK_OVERFLOW, INVALID_MESSAGE, CAN_NOT_COMPLETE,
    INVALID_FLAGS, UNRECOGNIZED_VOLUME, FILE_INVALID, FULLSCREEN_MODE,
    NO_TOKEN, BADDB, BADKEY, CANTOPEN, CANTREAD, CANTWRITE);
  TWinError1051 = ( // 1051..1079
    DEPENDENT_SERVICES_RUNNING, INVALID_SERVICE_CONTROL,
    SERVICE_REQUEST_TIMEOUT, SERVICE_NO_THREAD, SERVICE_DATABASE_LOCKED,
    SERVICE_ALREADY_RUNNING, INVALID_SERVICE_ACCOUNT, SERVICE_IS_DISABLED,
    CIRCULAR_DEPENDENCY, SERVICE_DOES_NOT_EXIST, SERVICE_CANNOT_ACCEPT_CTRL,
    SERVICE_NOT_ACTIVE, FAILED_SERVICE_CONTROLLER_CONNECT,
    EXCEPTION_IN_SERVICE, DATABASE_DOES_NOT_EXIST, SERVICE_SPECIFIC_ERROR,
    PROCESS_ABORTED, SERVICE_DEPENDENCY_FAIL, SERVICE_LOGON_FAILED,
    SERVICE_START_HANG, INVALID_SERVICE_LOCK, SERVICE_MARKED_FOR_DELETE,
    SERVICE_EXISTS, ALREADY_RUNNING_LKG, SERVICE_DEPENDENCY_DELETED,
    BOOT_ALREADY_ACCEPTED, SERVICE_NEVER_STARTED, DUPLICATE_SERVICE_NAME,
    DIFFERENT_SERVICE_ACCOUNT);
  TWinError1200 = ( // 1200..1246
    BAD_DEVICE, CONNECTION_UNAVAIL, DEVICE_ALREADY_REMEMBERED,
    NO_NET_OR_BAD_PATH, BAD_PROVIDER, CANNOT_OPEN_PROFILE, BAD_PROFILE,
    NOT_CONTAINER, EXTENDED_ERROR, INVALID_GROUPNAME, INVALID_COMPUTERNAME,
    INVALID_EVENTNAME, INVALID_DOMAINNAME, INVALID_SERVICENAME,
    INVALID_NETNAME, INVALID_SHARENAME, INVALID_PASSWORDNAME,
    INVALID_MESSAGENAME, INVALID_MESSAGEDEST, SESSION_CREDENTIAL_CONFLICT,
    REMOTE_SESSION_LIMIT_EXCEEDED, DUP_DOMAINNAME, NO_NETWORK, CANCELLED,
    USER_MAPPED_FILE, CONNECTION_REFUSED, GRACEFUL_DISCONNECT,
    ADDRESS_ALREADY_ASSOCIATED, ADDRESS_NOT_ASSOCIATED, CONNECTION_INVALID,
    CONNECTION_ACTIVE, NETWORK_UNREACHABLE, HOST_UNREACHABLE,
    PROTOCOL_UNREACHABLE, PORT_UNREACHABLE, REQUEST_ABORTED,
    CONNECTION_ABORTED, RETRY, CONNECTION_COUNT_LIMIT,
    LOGIN_TIME_RESTRICTION, LOGIN_WKSTA_RESTRICTION, INCORRECT_ADDRESS,
    ALREADY_REGISTERED, SERVICE_NOT_FOUND, NOT_AUTHENTICATED,
    NOT_LOGGED_ON, _CONTINUE);
  TWinError1315 = ( // 1315..1342
    INVALID_ACCOUNT_NAME, USER_EXISTS, NO_SUCH_USER, GROUP_EXISTS,
    NO_SUCH_GROUP, MEMBER_IN_GROUP, MEMBER_NOT_IN_GROUP, LAST_ADMIN,
    WRONG_PASSWORD, ILL_FORMED_PASSWORD, PASSWORD_RESTRICTION, LOGON_FAILURE,
    ACCOUNT_RESTRICTION, INVALID_LOGON_HOURS, INVALID_WORKSTATION,
    PASSWORD_EXPIRED, ACCOUNT_DISABLED, NONE_MAPPED, TOO_MANY_LUIDS_REQUESTED,
    LUIDS_EXHAUSTED, INVALID_SUB_AUTHORITY, INVALID_ACL, INVALID_SID,
    INVALID_SECURITY_DESCR, _1339, BAD_INHERITANCE_ACL, SERVER_DISABLED,
    SERVER_NOT_DISABLED);
  // O(log(n)) binary search in WINERR_SORTED[] constants
  TWinErrorSorted = (
    // some EXCEPTION_* in range $80000000 .. $800000ff
    DATATYPE_MISALIGNMENT, BREAKPOINT, SINGLE_STEP,
    // some SEC_E_* errors as returned by SSPI
    E_UNSUPPORTED_FUNCTION, E_INVALID_TOKEN, E_MESSAGE_ALTERED,
    E_CONTEXT_EXPIRED, E_INCOMPLETE_MESSAGE, E_BUFFER_TOO_SMALL,
    E_ILLEGAL_MESSAGE, E_CERT_UNKNOWN, E_CERT_EXPIRED, E_ENCRYPT_FAILURE,
    E_DECRYPT_FAILURE, E_ALGORITHM_MISMATCH,
    // some security-related HRESULT errors (negative 32-bit values first)
    CRYPT_E_BAD_ENCODE, CRYPT_E_SELF_SIGNED, CRYPT_E_BAD_MSG, CRYPT_E_REVOKED,
    CRYPT_E_NO_REVOCATION_CHECK, CRYPT_E_REVOCATION_OFFLINE, TRUST_E_BAD_DIGEST,
    TRUST_E_NOSIGNATURE, CERT_E_EXPIRED, CERT_E_CHAINING, CERT_E_REVOKED,
    // some EXCEPTION_* in range $c0000000 .. $c00000ff
    ACCESS_VIOLATION, IN_PAGE_ERROR, INVALID_HANDLE_,
    NONCONTINUABLE_EXCEPTION, ILLEGAL_INSTRUCTION,
    INVALID_DISPOSITION, ARRAY_BOUNDS_EXCEEDED, FLT_DENORMAL_OPERAND,
    FLT_DIVIDE_BY_ZERO, FLT_INEXACT_RESULT, FLT_INVALID_OPERATION,
    FLT_OVERFLOW, FLT_STACK_CHECK, FLT_UNDERFLOW, INT_DIVIDE_BY_ZERO,
    INT_OVERFLOW, PRIV_INSTRUCTION, STACK_OVERFLOW_,
    // sparse system errors
    ALREADY_EXISTS, MORE_DATA, ACCOUNT_EXPIRED, OLD_WIN_VERSION, NO_SYSTEM_RESOURCES,
    RPC_S_SERVER_UNAVAILABLE, PASSWORD_MUST_CHANGE, ACCOUNT_LOCKED_OUT,
    // main Windows Socket API (WSA*) errors
    EFAULT, EINVAL, EMFILE, EWOULDBLOCK, ENOTSOCK, ENETDOWN,
    ENETUNREACH, ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS,
    ETIMEDOUT, ECONNREFUSED, TRY_AGAIN,
    // most common WinHttp API (ERROR_WINHTTP_*) errors in range 12000...12152
    TIMEOUT, OPERATION_CANCELLED, CANNOT_CONNECT,
    CLIENT_AUTH_CERT_NEEDED, INVALID_SERVER_RESPONSE,
    // some SEC_I_* status as returned by SSPI
    I_CONTINUE_NEEDED, I_COMPLETE_NEEDED, I_COMPLETE_AND_CONTINUE,
    I_CONTEXT_EXPIRED, I_INCOMPLETE_CREDENTIALS, I_RENEGOTIATE);

const
  WINERR_SORTED: array[TWinErrorSorted] of cardinal = (
    // some EXCEPTION_* in range $80000000 .. $800000ff
    $80000002, $80000003, $80000004,
    // some SEC_E_* errors as returned by SSPI
    $80090302, $80090308, $8009030F, $80090317, $80090318, $80090321,
    $80090326, $80090327, $80090328, $80090329, $80090330, $80090331,
    // some security-related HRESULT errors (negative 32-bit values first)
    $80092002, $80092007, $8009200d, $80092010, $80092012, $80092013, $80096010,
    $800b0100, $800b0101, $800b010a, $800b010c,
    // some EXCEPTION_* in range $c0000000 .. $c00000ff
    $c0000005, $c0000006, $c0000008, $c000001d, $c0000025, $c0000026,
    $c000008c, $c000008d, $c000008e, $c000008f, $c0000090, $c0000091,
    $c0000092, $c0000093, $c0000094, $c0000095, $c0000096, $c00000fd,
    // sparse system errors
    183, 234, 701, 1150, 1450, 1722, 1907, 1909,
    // main Windows Socket API (WSA*) errors
    10014, 10022, 10024, 10035, 10038, 10050, 10051, 10052, 10053, 10054, 10055,
    10060, 10061, 11002,
    // most common WinHttp API (ERROR_WINHTTP_*) errors in range 12000...12152
    12002, 12017, 12029, 12044, 12152,
    // some SEC_I_* status as returned by SSPI
    $00090312, $00090313, $00090314, $00090317, $00090320, $00090321);

function WinErrorConstant(Code: cardinal): PShortString;
begin
  case Code of // split into TWinError* types (faster and cross-platform)
    0 .. ord(high(TWinError0)):
      result := GetEnumNameRtti(TypeInfo(TWinError0), Code);
    50 .. 50 + ord(high(TWinError50)):
      result := GetEnumNameRtti(TypeInfo(TWinError50), Code - 50);
    80 .. 80 + ord(high(TWinError80)):
      result := GetEnumNameRtti(TypeInfo(TWinError80), Code - 80);
    108 .. 108 + ord(high(TWinError108)):
      result := GetEnumNameRtti(TypeInfo(TWinError108), Code - 108);
    995 .. 995 + ord(high(TWinError995)):
      result := GetEnumNameRtti(TypeInfo(TWinError995), Code - 995);
    1051 .. 1051 + ord(high(TWinError1051)):
      result := GetEnumNameRtti(TypeInfo(TWinError1051), Code - 1051);
    1200 .. 1200 + ord(high(TWinError1200)):
      result := GetEnumNameRtti(TypeInfo(TWinError1200), Code - 1200);
    1315 .. 1315 + ord(high(TWinError1315)):
      result := GetEnumNameRtti(TypeInfo(TWinError1315), Code - 1315);
  else
    result := GetEnumNameRtti(TypeInfo(TWinErrorSorted),
      FastFindIntegerSorted(@WINERR_SORTED, ord(high(WINERR_SORTED)), Code));
  end;
end;

function WinErrorShort(Code: cardinal; NoInt: boolean): TShort47;
begin
  WinErrorShort(Code, @result, NoInt);
end;

procedure WinErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean);
begin
  Dest^[0] := #0;
  if NoInt then
    AppendWinErrorText(Code, Dest^, #0)
  else
  begin
    if integer(Code) < 0 then
      AppendShortIntHex(Code, Dest^) // e.g. '80092002 CRYPT_E_BAD_ENCODE'
    else
      AppendShortCardinal(Code, Dest^); // e.g. '5 ERROR_ACCESS_DENIED'
    AppendWinErrorText(Code, Dest^, ' ');
  end;
end;

const
  _PREFIX: array[0..5] of string[15] = (
    'WSA', 'ERROR_WINHTTP_', '', 'EXCEPTION_', 'SEC_', 'ERROR_');

function AppendWinErrorText(Code: cardinal; var Dest: ShortString;
  Sep: AnsiChar): boolean;
var
  txt: PShortString;
begin
  result := false;
  txt := WinErrorConstant(Code);
  if txt^[0] = #0 then
    exit; // unknown
  if Sep <> #0 then
    AppendShortChar(Sep, @Dest);
  case Code of
    10000 .. 11999:
      Code := 0;  // main Windows Socket API errors
    12000 .. 12152:
      Code := 1;  // most common WinHttp API errors
    1722, $80092002 .. $800b010c:
      Code := 2;  // no prefix for security-related HRESULT errors
    $80000000 .. $800000ff, $c0000000 .. $c00000ff:
      Code := 3;  // EXCEPTION_* constants
    $00090312 .. $00090321,
    $80090302 .. $80090331:
      Code := 4; // SEC_* SSPI constants
  else
    Code := 5;   // regular Windows ERROR_* constant
  end;
  AppendShort(_PREFIX[Code], Dest);
  AppendShort(txt^, Dest);
  if Dest[ord(Dest[0])] = '_' then
    dec(Dest[0]); // 'EXCEPTION_STACK_OVERFLOW_' -> 'EXCEPTION_STACK_OVERFLOW'
  result := true;
end;

type
  // the main errors returned on Linux, in their ordinal order
  TLinuxError = (
    lSUCCESS, lPERM, lNOENT, lSRCH, lINTR, lIO, lNXIO, l2BIG, lNOEXEC, lBADF, lCHILD, lAGAIN, lNOMEM, lACCES,
    lFAULT, lNOTBLK, lBUSY, lEXIST, lXDEV, lNODEV, lNOTDIR, lISDIR, lINVAL, lNFILE, lMFILE, lNOTTY,
    lTXTBSY, lFBIG, lNOSPC, lSPIPE, lROFS, lMLINK, lPIPE, lDOM, lRANGE, lDEADLK, lNAMETOOLONG, lNOLCK,
    lNOSYS, lNOTEMPTY, lLOOP, lWOULDBLOCK, lNOMSG, lIDRM, lCHRNG, lL2NSYNC, lL3HLT, lL3RST, lLNRNG,
    lUNATCH, lNOCSI, lL2HLT, lBADE, lBADR, lXFULL, lNOANO, lBADRQC, lBADSLT, lDEADLOCK, lBFONT, lNOSTR,
    lNODATA, lTIME, lNOSR, lNONET, lNOPKG, lREMOTE, lNOLINK, lADV, lSRMNT, lCOMM, lPROTO, lMULTIHOP,
    lDOTDOT, lBADMSG, lOVERFLOW, lNOTUNIQ, lBADFD, lREMCHG, lLIBACC, lLIBBAD, lLIBSCN, lLIBMAX, lLIBEXEC,
    lILSEQ, lRESTART, lSTRPIPE, lUSERS, lNOTSOCK, lDESTADDRREQ, lMSGSIZE, lPROTOTYPE, lNOPROTOOPT,
    lPROTONOSUPPORT, lSOCKTNOSUPPORT, lOPNOTSUPP, lPFNOSUPPORT, lAFNOSUPPORT, lADDRINUSE, lADDRNOTAVAIL,
    lNETDOWN, lNETUNREACH, lNETRESET, lCONNABORTED, lCONNRESET, lNOBUFS, lISCONN, lNOTCONN, lSHUTDOWN,
    lTOOMANYREFS, lTIMEDOUT, lCONNREFUSED, lHOSTDOWN, lHOSTUNREACH, lALREADY, lINPROGRESS, lSTALE,
    lUCLEAN, lNOTNAM, lNAVAIL, lISNAM, lREMOTEIO, lDQUOT, lNOMEDIUM, lMEDIUMTYPE);

  // the main errors returned on BSD (and Darwin), in their ordinal order
  // - only the main common set of errors is defined, not higher divergent values
  TBsdError = (
    bSUCCESS, bPERM, bNOENT, bSRCH, bINTR, bIO, bNXIO, b2BIG, bNOEXEC, bBADF, bCHILD, bDEADLK, bNOMEM, bACCES,
    bFAULT, bNOTBLK, bBUSY, bEXIST, bXDEV, bNODEV, bNOTDIR, bISDIR, bINVAL, bNFILE, bMFILE, bNOTTY,
    bTXTBSY, bFBIG, bNOSPC, bSPIPE, bROFS, bMLINK, bPIPE, bDOM, bRANGE, bAGAIN, bINPROGRESS, bALREADY,
    bNOTSOCK, bDESTADDRREQ, bMSGSIZE, bPROTOTYPE, bNOPROTOOPT, bPROTONOSUPPORT, bSOCKTNOSUPPORT,
    bOPNOTSUPP, bPFNOSUPPORT, bAFNOSUPPORT, bADDRINUSE, bADDRNOTAVAIL, bNETDOWN, bNETUNREACH, bNETRESET,
    bCONNABORTED, bCONNRESET, bNOBUFS, bISCONN, bNOTCONN, bSHUTDOWN, bTOOMANYREFS, bTIMEDOUT,
    bCONNREFUSED, bLOOP, bNAMETOOLONG, bHOSTDOWN, bHOSTUNREACH, bNOTEMPTY, bPROCLIM, bUSERS, bDQUOT,
    bSTALE, bREMOTE, bBADRPC, bRPCMISMATCH, bPROGUNAVAIL, bPROGMISMATCH, bPROCUNAVAIL, bNOLCK, bNOSYS,
    bFTYPE, bAUTH, bNEEDAUTH);

procedure _PosixError(Code, Max: cardinal; Dest: PShortString;
  Info: pointer; NoInt: boolean);
var
  ps: PShortString;
  d: PAnsiChar;
begin
  Dest^[0] := #0;
  if not NoInt then
    AppendShortCardinal(Code, Dest^); // e.g. '1 EPERM'
  if Code > Max then
    exit;
  if not NoInt then
    AppendShortChar(' ', pointer(Dest));
  ps := GetEnumNameRtti(Info, Code);
  d := @Dest^[ord(Dest^[0]) + 1];
  inc(Dest^[0], ord(ps^[0]));
  d[0] := 'E'; // ignore 'l' or 'b' prefix and write 'E' instead
  MoveFast(ps^[2], d[1], ord(ps^[0]) - 1);
end;

procedure LinuxErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean);
begin
  _PosixError(Code, ord(high(TLinuxError)), Dest, TypeInfo(TLinuxError), NoInt);
end;

procedure BsdErrorShort(Code: cardinal; Dest: PShortString; NoInt: boolean);
begin
  _PosixError(Code, ord(high(TBsdError)), Dest, TypeInfo(TBsdError), NoInt);
end;

function OsErrorShort(Code: cardinal; NoInt: boolean): TShort47;
begin
  if Code = 0 then
    Code := GetLastError;
  OsErrorShort(Code, @result, NoInt); // redirect to Win/Linux/BsdErrorShort()
end;

procedure OsErrorAppend(Code: cardinal; var Dest: ShortString;
  Sep: AnsiChar; NoInt: boolean);
var
  os: TShort47;
begin
  OsErrorShort(Code, @os, NoInt); // redirect to Win/Linux/BsdErrorShort()
  if Sep <> #0 then
    AppendShortChar(Sep, @Dest);
  AppendShort(os, Dest);
end;

function GetErrorShort(error: integer): ShortString;
begin
  if error = 0 then
    error := GetLastError;
  GetErrorShortVar(error, result);
end;

function GetErrorText(error: integer): RawUtf8;
var
  txt: shortstring;
begin
  if error = 0 then
    error := GetLastError;
  GetErrorShortVar(error, txt);
  FastSetString(result, @txt[1], ord(txt[0]));
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
    $0d40,  // actNeoverseV1
    $0d41,  // actCortexA78
    $0d42,  // actCortexA78AE
    $0d44,  // actCortexX1
    $0d46,  // actCortex510
    $0d47,  // actCortex710
    $0d48,  // actCortexX2
    $0d49,  // actNeoverseN2
    $0d4a,  // actNeoverseE1
    $0d4b,  // actCortexA78C
    $0d4c,  // actCortexX1C
    $0d4d,  // actCortexA715
    $0d4e,  // actCortexX3
    $0d4f,  // actNeoverseV2
    $0d80,  // actCortexA520
    $0d81,  // actCortexA720
    $0d82,  // actCortexX4
    $0d83,  // actNeoverseV3AE
    $0d84,  // actNeoverseV3
    $0d85,  // actCortextX925
    $0d87,  // actCortextA725
    $0d88,  // actCortextA520AE
    $0d89,  // actCortextA720AE
    $0d8a,  // actC1Nano
    $0d8b,  // actC1Pro
    $0d8c,  // actC1Ultra
    $0d8e,  // actNeoverseN3
    $0d8f,  // actCortextA320
    $0d90); // actC1Premium

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
    $6d,  // aciMicrosoft
    $70,  // aciPhytium
    $c0); // aciAmpere

  ARMCPU_ID_TXT: array[TArmCpuType] of string[15] = (
     '',
     'ARM810', 'ARM920', 'ARM922', 'ARM926', 'ARM940', 'ARM946', 'ARM966',
     'ARM1020', 'ARM1022', 'ARM1026', 'ARM11 MPCore', 'ARM1136', 'ARM1156',
     'ARM1176',     'Cortex-A5',   'Cortex-A7',   'Cortex-A8',   'Cortex-A9',
     'Cortex-A17',{originally A12} 'Cortex-A15',  'Cortex-A17',  'Cortex-R4',
     'Cortex-R5',   'Cortex-R7',   'Cortex-R8',   'Cortex-M0',   'Cortex-M1',
     'Cortex-M3',   'Cortex-M4',   'Cortex-M7',   'Cortex-M0+',  'Cortex-A32',
     'Cortex-A53',  'Cortex-A35',  'Cortex-A55',  'Cortex-A65',  'Cortex-A57',
     'Cortex-A72',  'Cortex-A73',  'Cortex-A75',  'Cortex-A76',  'Neoverse-N1',
     'Cortex-A77',  'Cortex-A76AE','Cortex-R52',  'Cortex-M23',  'Cortex-M33',
     'Neoverse-V1', 'Cortex-A78',  'Cortex-A78AE','Cortex-X1',   'Cortex-510',
     'Cortex-710',  'Cortex-X2',   'Neoverse-N2', 'Neoverse-E1', 'Cortex-A78C',
     'Cortex-X1C',  'Cortex-A715', 'Cortex-X3',   'Neoverse-V2', 'Cortex-A520',
     'Cortex-A720', 'Cortex-X4',   'Neoverse-V3AE','Neoverse-V3','Cortex-X925',
     'Cortex-A725', 'Cortex-A520AE', 'Cortex-A720AE', 'C1-Nano', 'C1-Pro',
     'C1-Ultra',    'Neoverse-N3',   'Cortex-A320', 'C1-Premium');

  ARMCPU_IMPL_TXT: array[TArmCpuImplementer] of string[18] = (
      '',
      'ARM', 'Broadcom', 'Cavium', 'DEC', 'FUJITSU', 'HiSilicon', 'Infineon',
      'Motorola/Freescale', 'NVIDIA', 'APM', 'Qualcomm', 'Samsung', 'Marvell',
      'Apple', 'Faraday', 'Intel', 'Microsoft', 'Phytium', 'Ampere');

function ArmCpuType(id: word): TArmCpuType;
begin
  for result := low(TArmCpuType) to high(TArmCpuType) do
    if ARMCPU_ID[result] = id then
      exit;
  result := actUnknown;
end;

function ArmCpuTypeName(act: TArmCpuType; id: word; const before: ShortString): ShortString;
begin
  result := before;
  if act = actUnknown then
  begin
    AppendShort('ARM 0x', result);;
    AppendShortIntHex(id, result);
  end
  else
    AppendShort(ARMCPU_ID_TXT[act], result);
end;

function ArmCpuImplementer(id: byte): TArmCpuImplementer;
begin
  for result := low(TArmCpuImplementer) to high(TArmCpuImplementer) do
    if ARMCPU_IMPL[result] = id then
      exit;
  result := aciUnknown;
end;

function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word;
  const after: ShortString): ShortString;
begin
  if aci = aciUnknown then
  begin
    result := 'HW 0x';
    AppendShortIntHex(id, result);
  end
  else
    result := ARMCPU_IMPL_TXT[aci];
  AppendShort(after, result);
end;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt; LeaveUnprotected: boolean);
begin
  PatchCode(Code, @Value, SizeOf(Code^), nil, LeaveUnprotected);
end;

{$ifdef CPUINTEL}
procedure RedirectCode(Func, RedirectFunc: pointer);
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

function Unicode_CodePage: integer;
begin
  {$ifdef FPC}
  // = GetSystemCodePage on POSIX, Lazarus may override to be CP_UTF8 on Windows
  result := DefaultSystemCodePage;
  {$else}
  // Delphi always uses the main Windows System Code Page
  result := GetACP;
  {$endif FPC}
end;

function Unicode_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt;
  IgnoreCase: boolean): integer;
const
  _CASEFLAG: array[boolean] of DWord = (0, NORM_IGNORECASE);
begin // mormot.core.os.posix.inc CompareStringW() may be using ICU or the RTL
  result := CompareStringW(LOCALE_USER_DEFAULT, _CASEFLAG[IgnoreCase], PW1, L1, PW2, L2);
end;

procedure Unicode_WideToShort(W: PWideChar; LW, CodePage: PtrInt;
  var res: ShortString);
var
  i: PtrInt;
begin
  if LW <= 0 then
    res[0] := #0
  else if IsAnsiCompatibleW(W, LW) then
  begin
    // fast handling of pure ASCII-7 content (very common case)
    if LW > 255 then
      LW := 255;
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
    // use WinAPI, ICU or cwstring/RTL for accurate conversion
    res[0] := AnsiChar(
      Unicode_WideToAnsi(W, PAnsiChar(@res[1]), LW, 255, CodePage));
end;

function Unicode_FromUtf8(Text: PUtf8Char; TextLen: PtrInt;
  var Dest: TSynTempBuffer): PWideChar;
var
  i: PtrInt;
begin
  if (Text = nil) or
     (TextLen <= 0) then
    result := Dest.Init(0)
  else if IsAnsiCompatible(pointer(Text), TextLen) then // optimistic way
  begin
    result := Dest.Init(TextLen);
    for i := 0 to TextLen - 1 do
      PWordArray(result)[i] := PByteArray(Text)[i];
    result[Dest.len] := #0; // Text[TextLen] may not be #0
  end
  else // use the RTL to perform the UTF-8 to UTF-16 conversion
  begin                               
    result := Dest.Init(TextLen * 2); // maximum absolute UTF-16 size in bytes
    Dest.len := Utf8ToUnicode(result, TextLen + 8, pointer(Text), TextLen);
    if Dest.len <= 0 then                  // + 8 = + SYNTEMPTRAIL/2
      Dest.len := 0
    else
    begin
      dec(Dest.len); // Utf8ToUnicode() returned length includes #0 terminator
      result[Dest.len] := #0; // missing on FPC
    end;
  end;
end;

procedure Unicode_CodePageName(CodePage: cardinal; var Name: ShortString);
begin // cut-down and fixed version of FPC rtl/objpas/sysutils/syscodepages.inc
  case codepage of
    932:
      Name  := 'SHIFT_JIS';
    936:
      Name := 'GBK';
    949:
      Name := 'KS-C5601'; // Unified Hangul Code
    950:
      Name := 'BIG5';
    951: // not standard: will fallback to 950 in mormot.core.os.windows.inc
      Name := 'BIG5-HKSCS';
    CP_UTF16: // = 1200
      Name := 'UTF16LE';
    1201:
      Name := 'UTF16BE';
    1361:
      Name := 'JOHAB';
    12000:
      Name := 'UTF32LE';
    20932:
      Name := 'EUC-JP'; // Japanese (JIS 0208-1990 and 0121-1990)
    28591 .. 28606:
      begin
        Name := 'ISO-8859-';
        AppendShortCardinal(codepage - 28590, Name);
      end;
    50220, 50222:
      Name := 'ISO-2022-JP';
    50221:
      Name := 'CDISO2022JP';
    50225:
      Name := 'ISO-2022-KR';
    50227:
      Name := 'ISO-2022-CN';
    51936:
      Name := 'EUC-CN';  // EUC Simplified Chinese
    51949:
      Name := 'EUC-KR';  // EUC Korean
    CP_HZ: // = 52936
      Name := 'HZ';      // HZ-GB2312 Simplified Chinese
    54936:
      Name := 'GB18030'; // GB18030 Simplified Chinese
    CP_UTF8: // = 65001
      Name := 'UTF8';
  else
    begin  // 'MS####' is enough for most code pages
      Name := 'MS';
      AppendShortCardinal(codepage, Name);
    end; // ICU expects 'CP####' for IBM codepages which are not Windows'
  end;
  Name[ord(Name[0]) + 1] := #0; // ensure is ASCIIZ - e.g. for ucnv_open()
end;

function NowUtc: TDateTime;
begin
  result := UnixMSTimeUtcFast / Int64(MilliSecsPerDay) + Int64(UnixDelta);
end;

function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;
var
  yy, mm, dd, h, m, s, ms: word;
begin
  result := 0;
  if DateTime = 0 then
    exit;
  DecodeDate(DateTime, yy, mm, dd);
  DecodeTime(DateTime, h, m, s, ms);
  if (yy >= 1980) and
     (yy <= 2099) then // hard limit is 2108, but WinAPI up to 2099/12/31
    result := (s shr 1) or (m shl 5) or (h shl 11) or
      cardinal((dd shl 16) or (mm shl 21) or (cardinal(yy - 1980) shl 25));
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
  FileTimePerMs = 10000; // a tick is 100ns

function WindowsFileTime64ToUnixMSTime(WinTime: QWord): TUnixMSTime;
begin
  result := (Int64(WinTime) - UnixFileTimeDelta) div FileTimePerMs;
end;

function FileInfoByName(const FileName: TFileName; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;
var
  h: THandle;
begin
  result := false;
  h := FileOpenSequentialRead(FileName); // = plain fpOpen() on POSIX
  if not ValidHandle(h) then
    exit;
  result := FileInfoByHandle(h, FileId, FileSize, LastWriteAccess, FileCreateDateTime);
  FileClose(h);
end;


{ TDirectoryBrowser }

constructor TDirectoryBrowser.Create(const Directory: TFileName;
  const FileMasks: array of TFileName; Recursive: boolean);
var
  i: PtrInt;
begin
  fCurrentDir := ExcludeTrailingPathDelimiter(Directory);
  fRecursive := Recursive;
  SetLength(fFileMask, length(FileMasks));
  for i := 0 to high(FileMasks) do
    fFileMask[i] := FileMasks[i];
end;

function TDirectoryBrowser.OnFile(const FileInfo: TSearchRec;
  const FullFileName: TFileName): boolean;
begin
  inc(fTotalSize, FileInfo.Size); // default behavior for DirectorySize()
  result := true; // continue
end;

function TDirectoryBrowser.OnFolder(const FullFolderName: TFileName): boolean;
begin
  result := true; // continue
end;

function TDirectoryBrowser.Run: boolean;
begin
  result := (fCurrentDir <> '') and
            FileExists(fCurrentDir, {link=}true, {asdir=}true);
  if not result then
    exit;
  {$ifdef OSWINDOWS} // check if already in '\\?\...' format ?
  fExtendedPath := IsExtendedPathName(fCurrentDir);
  {$endif OSWINDOWS}
  ProcessDir;
  result := not fAborted;
end;

procedure TDirectoryBrowser.OnProcessDir;
begin
end;

procedure TDirectoryBrowser.ProcessDir;
var
  f: TSearchRec;
  prev: TFileName;
  i, level: integer;
begin
  level := 0;
  fLevelCounter := @level;
  OnProcessDir;
  fCurrentDir := fCurrentDir + PathDelim;
  if fRecursive then
    if FindFirst(Make(FILES_ALL), faDirectory, f) = 0 then
    try
      repeat
        if SearchRecValidFolder(f) then
        begin
          prev := fCurrentDir;
          fCurrentDir := Make(f.Name); // nested folder
          ProcessDir;                  // recursive call
          if fAborted then
            exit;
          fCurrentDir := prev; // back to the parent folder
        end;
      until FindNext(f) <> 0;
    finally // OnFile/OnFolder may trigger some exception
      FindClose(f);
    end;
  for i := 0 to length(fFileMask) - 1 do
    if FindFirst(Make(fFileMask[i]), faAnyfile - faDirectory, f) = 0 then
    try
      repeat
        if SearchRecValidFile(f) then
          if not OnFile(f, Make(f.Name)) then
          begin
            fAborted := true;
            exit;
          end;
      until FindNext(f) <> 0;
    finally // OnFile/OnFolder may trigger some exception
      FindClose(f);
    end;
  fAborted := not OnFolder(fCurrentDir); // always called last
end;

function DirectorySize(const Path: TFileName; Recursive: boolean;
  const FileMask: TFileName): Int64;
var
  browse: TDirectoryBrowser;
begin
  browse := TDirectoryBrowser.Create(Path, [FileMask], Recursive);
  try
    browse.Run;
    result := browse.fTotalSize; // as computed by the default OnFile() method
  finally
    browse.Free;
  end;
end;

function DirectoryExists(const FileName: TFileName; FollowLink: boolean): boolean;
var
  len: PtrInt;
begin
  len := length(FileName);
  if len = 0 then
    result := false
  else if (len = 1) and
          (FileName[1] = '.') then
    result := true
  else if FileName[len] <> PathDelim then
    result := FileExists(FileName, FollowLink, {checkasdir=}true)
  else
    result := FileExists(copy(FileName, 1, len - 1), FollowLink, {asdir=}true);
end;

function SafePathName(const Path: TFileName): boolean;
var
  i, o: PtrInt;
begin
  if Path <> '' then
  begin
    result := false;
    if (Path[1] = '/') or
       (PosExString(':', Path) <> 0) or
       (PosExString('\\', Path) <> 0) then
      exit;
    o := 1;
    repeat
      i := PosExString('..', Path, o);
      if i = 0 then
        break;
      o := i + 2; // '..test' or 'test..' are valid folder names
      if cardinal(Path[o]) in [0, ord('\'), ord('/')] then
        if (i = 1) or
           (cardinal(Path[i - 1]) in [ord('\'), ord('/')]) then
          exit;
    until false;
  end;
  result := true;
end;

function SafePathNameU(const Path: RawUtf8): boolean;
var
  i, o: PtrInt;
begin
  if Path <> '' then
  begin
    result := false;
    if (Path[1] = '/') or
       (PosExChar(':', Path) <> 0) or
       (PosEx('\\', Path) <> 0) then
      exit;
    o := 1;
    repeat
      i := PosEx('..', Path, o);
      if i = 0 then
        break;
      o := i + 2;
      if Path[o] in [#0, '\', '/'] then
        if (i = 1) or
           (Path[i - 1] in ['\', '/']) then
          exit;
    until false;
  end;
  result := true;
end;

function SafeFileName(const FileName: TFileName): boolean;
begin
  result := SafePathName(ExtractPath(FileName));
end;

function SafeFileNameU(const FileName: RawUtf8): boolean;
begin
  result := SafePathNameU(ExtractPathU(FileName));
end;

function NormalizeFileName(const FileName: TFileName): TFileName;
var
  i, j: PtrInt;
begin
  result := FileName;
  j := Pos(InvertedPathDelim, result);
  if j <> 0 then
    for i := j to length(result) do
      if result[i] = InvertedPathDelim then
        result[i] := PathDelim;
end;

procedure NormalizeFileNameU(var FileName: RawUtf8);
var
  i: PtrInt;
  p: PAnsiChar;
begin
  i := PosExChar(InvertedPathDelim, FileName);
  if i = 0 then
    exit;
  p := UniqueRawUtf8(FileName);
  inc(p, i - 1);
  repeat
    if p^ = InvertedPathDelim then
      p^ := PathDelim;
    inc(p);
  until p^ = #0;
end;

function QuoteFileName(const FileName: TFileName): TFileName;
begin
  if (FileName <> '') and
     (PosExString(' ', FileName) <> 0) and
     (FileName[1] <> '"') then
    result := '"' + FileName + '"'
  else
    result := FileName;
end;

procedure DisplayFatalError(const title, msg: RawUtf8);
var
  u: RawUtf8;
begin
  if title <> '' then
  begin
    SetLength(u, length(Title) + 1);
    FillCharFast(pointer(u)^, length(u), ord('-'));
    u := Join([CRLF, title, CRLF, u, CRLF + CRLF, msg, CRLF]);
  end
  else
    Join([msg, CRLF], u);
  ConsoleErrorWrite(u);
end;

procedure DisplayError(const fmt: string; const args: array of const);
var
  msg: string;
begin
  msg := Format(fmt, args);
  DisplayFatalError('', RawUtf8(msg));
end;

function SearchRecToDateTimeUtc(const F: TSearchRec): TDateTime;
begin
  result := SearchRecToUnixTimeUtc(F) / Int64(SecsPerDay) + Int64(UnixDelta);
end;

const
  // faHidden is supported by the FPC RTL on POSIX, by checking an initial '.'
  faInvalid = faDirectory + {$ifdef OSWINDOWS} faVolumeID{%H-} + {$endif} faSysFile{%H-};

function SearchRecValidFile(const F: TSearchRec; IncludeHidden: boolean): boolean;
begin
  result := (F.Name <> '') and
            (F.Attr and faInvalid = 0) and
            (IncludeHidden or
             (F.Attr and faHidden{%H-} = 0));
end;

function SearchRecValidFolder(const F: TSearchRec; IncludeHidden: boolean): boolean;
begin
  result := (F.Attr and faDirectory <> 0) and
            (IncludeHidden or
             (F.Attr and faHidden{%H-} = 0)) and
            (F.Name <> '') and
            (F.Name <> '.') and
            (F.Name <> '..');
end;

function FindFirstDirectory(const Path: TFileName; IncludeHidden: boolean;
  out F: TSearchRec): integer;
begin
  result := faDirectory;
  if IncludeHidden then
    result := result or faHidden{%H-};
  result := FindFirst(Path, result, F);
end;


{ TFileStreamEx }

constructor TFileStreamEx.Create(const aFileName: TFileName; Mode: cardinal);
var
  h: THandle;
begin
  if Mode and fmCreate = fmCreate then
    h := FileCreate(aFileName, Mode and $00ff) // fmCreate=$ffff on oldest Delphi
  else
    h := FileOpen(aFileName, Mode);
  CreateFromHandle(h, aFileName); // raise EOSException on invalid h
end;

constructor TFileStreamEx.CreateFromHandle(aHandle: THandle;
  const aFileName: TFileName; aDontReleaseHandle: boolean);
begin
  if not ValidHandle(aHandle) then
    raise EOSException.CreateFmt('%s.Create(%s) failed as %s',
      [ClassNameShort(self)^, aFileName, GetErrorShort])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$else} at ReturnAddress {$endif};
  inherited Create(aHandle); // TFileStreamFromHandle constructor which own it
  fFileName := aFileName;
  fDontReleaseHandle := aDontReleaseHandle;
end;

constructor TFileStreamEx.CreateRead(const aFileName: TFileName);
begin // raise EOSException on invalid handle/aFileName
  CreateFromHandle(FileOpenSequentialRead(aFileName), aFileName);
end;

constructor TFileStreamEx.CreateWrite(const aFileName: TFileName);
var
  h: THandle;
begin
  h := FileOpen(aFileName, fmOpenReadWrite or fmShareRead);
  if not ValidHandle(h) then // we may need to create the file
    h := FileCreate(aFileName, fmShareRead);
  CreateFromHandle(h, aFileName); // raise EOSException on invalid h
end;

destructor TFileStreamEx.Destroy;
begin
  if not fDontReleaseHandle then
    FileClose(Handle); // otherwise file remains opened (FPC RTL inconsistency)
end;

function TFileStreamEx.GetSize: Int64;
begin
  result := FileSize(Handle); // faster than 3 FileSeek() calls - and threadsafe
end;


{ TFileStreamNoWriteError }

constructor TFileStreamNoWriteError.CreateAndRenameIfLocked(
  var aFileName: TFileName; aAliases: integer);
var
  h: THandle;
  fn, ext: TFileName;
  err, retry: integer;

  function CanOpenWrite: boolean;
  begin
    h := FileOpen(aFileName, fmOpenReadWrite or fmShareRead);
    result := ValidHandle(h);
    if not result then
      err := GetLastError;
  end;

begin
  // logic similar to TSynLog.CreateLogWriter
  h := 0;
  err := 0;
  if not CanOpenWrite then
    if not FileExists(aFileName) then
      // immediately raise EOSException if this new file could not be created
      h := FileCreate(aFileName, fmShareRead)
    else
    begin
      fn := aFileName;
      ext := ExtractFileExt(aFileName);
      for retry := 1 to aAliases do
      begin
        if IsSharedViolation(err) then
        begin
          // file was locked: wait a little for a background process and retry
          SleepHiRes(50);
          if CanOpenWrite then
            break;
        end;
        // file can't be opened: try '<filename>-locked<#>.<ext>' alternatives
        aFileName := ChangeFileExt(fn, '-locked' + IntToStr(retry) + ext);
        if CanOpenWrite then
          break;
      end;
    end;
  CreateFromHandle(h, aFileName); // raise EOSException on invalid h
end;

function TFileStreamNoWriteError.Write(const Buffer; Count: Longint): Longint;
begin
  FileWriteAll(Handle, @Buffer, Count); // and ignore any I/O error
  result := Count; // optimistic view: emulate all data was properly written
end;

function FileStreamSequentialRead(const FileName: TFileName): THandleStream;
var
  h: THandle;
begin
  result := nil;
  h := FileOpenSequentialRead(FileName);
  if ValidHandle(h) then // would raise EOSException on invalid h
    result := TFileStreamEx.CreateFromHandle(h, FileName);
end;

function StreamCopyUntilEnd(Source, Dest: TStream): Int64;
var
  tmp: array[word] of word; // 128KB stack buffer
  read: integer;
begin
  result := 0;
  if (Source <> nil) and
     (Dest <> nil) then
    repeat
      read := Source.Read(tmp, SizeOf(tmp));
      if read <= 0 then
        break;
      Dest.WriteBuffer(tmp, read);
      inc(result, read);
    until false;
end;

function FileReadAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;
var
  chunk, read: PtrInt;
begin
  result := false;
  while Size > 0 do
  begin
    chunk := Size;
    {$ifdef OSWINDOWS}
    if chunk > 16 shl 20 then
      chunk := 16 shl 20; // to avoid ERROR_NO_SYSTEM_RESOURCES errors
    {$endif OSWINDOWS}
    read := FileRead(F, Buffer^, chunk);
    if read <= 0 then
      exit; // error reading Size bytes
    inc(PByte(Buffer), read);
    dec(Size, read);
  end;
  result := true;
end;

function StreamReadAll(S: TStream; Buffer: pointer; Size: PtrInt): boolean;
var
  chunk, read: PtrInt;
begin
  result := false;
  while Size > 0 do
  begin
    chunk := Size;
    {$ifdef OSWINDOWS}
    if chunk > 16 shl 20 then
      chunk := 16 shl 20; // to avoid ERROR_NO_SYSTEM_RESOURCES errors
    {$endif OSWINDOWS}
    read := S.Read(Buffer^, chunk);
    if read <= 0 then
      exit; // error reading Size bytes
    inc(PByte(Buffer), read);
    dec(Size, read);
  end;
  result := true;
end;

function FileWriteAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;
var
  written: PtrInt;
begin
  result := false;
  if Size > 0 then
    repeat
      written := FileWrite(F, Buffer^, Size);
      if written <= 0 then
        exit; // fatal error
      inc(PByte(Buffer), written); // e.g. may have been interrrupted
      dec(Size, written);
    until Size = 0;
  result := true;
end;

function StringFromFile(const FileName: TFileName): RawByteString;
var
  h: THandle;
  size: Int64;
begin
  result := '';
  if FileName = '' then
    exit;
  h := FileOpenSequentialRead(FileName); // = plain fpOpen() on POSIX
  if not ValidHandle(h) then
    exit;
  size := FileSize(h);
  if (size < MaxInt) and // 2GB seems big enough for a RawByteString
     (size > 0) then
  begin
    pointer(result) := FastNewString(size, CP_UTF8); // UTF-8 for FPC RTL bug
    if not FileReadAll(h, pointer(result), size) then
      result := ''; // error reading
  end;
  FileClose(h);
end;

function StringFromFirstFile(const FileName: array of TFileName): RawByteString;
var
  f: PtrInt;
begin
  for f := 0 to high(FileName) do
  begin
    result := StringFromFile(FileName[f]);
    if result <> '' then
      exit;
  end;
  result := '';
end;

function StringFromFiles(const FileName: array of TFileName): TRawByteStringDynArray;
var
  f: PtrInt;
begin
  result := nil;
  SetLength(result, length(FileName));
  for f := 0 to high(FileName) do
    result[f] := StringFromFile(FileName[f]);
end;

function StringFromFolders(const Folders: array of TFileName;
  const Mask: TFileName; FileNames: PFileNameDynArray): TRawByteStringDynArray;
var
  dir, fn: TFileName;
  sr: TSearchRec;
  f, n: PtrInt;
  one: RawUtf8;
begin
  result := nil;
  if FileNames <> nil then
    FileNames^ := nil;
  n := 0;
  for f := 0 to high(Folders) do
    if DirectoryExists(Folders[f]) then
    begin
      dir := IncludeTrailingPathDelimiter(Folders[f]);
      if FindFirst(dir + Mask, faAnyFile - faDirectory, sr) = 0 then
      begin
        repeat
          if SearchRecValidFile(sr) then
          begin
            fn := dir + sr.Name;
            one := StringFromFile(fn);
            if one <> '' then
            begin
              if length(result) = n then
              begin
                SetLength(result, NextGrow(n));
                if FileNames <> nil then
                  SetLength(FileNames^, length(result));
              end;
              result[n] := one;
              if FileNames <> nil then
                FileNames^[n] := fn;
              inc(n);
            end;
          end;
        until FindNext(sr) <> 0;
        FindClose(sr);
      end;
    end;
  if n = 0 then
    exit;
  DynArrayFakeLength(result, n);
  if FileNames <> nil then
    DynArrayFakeLength(FileNames^, n);
end;

function FileFromString(const Content: RawByteString;
  const FileName: TFileName; FlushOnDisk: boolean): boolean;
begin
  result := FileFromBuffer(pointer(Content), length(Content), FileName, FlushOnDisk);
end;

function FileFromBuffer(Buf: pointer; Len: PtrInt; const FileName: TFileName;
  FlushOnDisk: boolean): boolean;
var
  h: THandle;
begin
  result := false;
  h := FileCreate(FileName);
  if not ValidHandle(h) then
    exit;
  result := FileWriteAll(h, Buf, Len);
  if result and
     FlushOnDisk then
    FlushFileBuffers(h);
  FileClose(h);
end;

function BufferFromFile(const FileName: TFileName; Buf: pointer; Len: PtrInt): boolean;
var
  h: THandle;
begin
  result := false;
  h := FileOpen(FileName, fmOpenReadShared);
  if not ValidHandle(h) then
    exit;
  result := FileReadAll(h, Buf, Len);
  FileClose(h);
end;

function AppendToFile(const Content: RawUtf8; const FileName: TFileName;
  BackupOverMaxSize: Int64): boolean;
var
  h: THandle;
  bak: TFileName;
begin
  result := Content = '';
  if result then
    exit;
  if (BackupOverMaxSize > 0) and
     (FileSize(FileName) > BackupOverMaxSize) then
  begin
    bak := FileName + '.bak';
    DeleteFile(bak);
    RenameFile(FileName, bak);
    h := 0;
  end
  else
    h := FileOpen(FileName, fmOpenWriteShared);
  if ValidHandle(h) then
    FileSeek64(h, 0, soFromEnd) // append
  else
  begin
    h := FileCreate(FileName, fmShareReadWrite);
    if not ValidHandle(h) then
      exit;
  end;
  result := FileWriteAll(h, pointer(Content), Length(Content));
  FileClose(h);
end;

var
  _TmpCounter: integer; // global thread-safe counter for this process

function TemporaryFileName(FolderName: TFileName; ExeName: RawUtf8): TFileName;
var
  retry: integer;
begin
  if FolderName = '' then
    FolderName := GetSystemPath(spTemp)
  else
    FolderName := IncludeTrailingPathDelimiter(FolderName);
  if ExeName = '' then
    ExeName := Executable.ProgramName;
  if _TmpCounter = 0 then
    _TmpCounter := Random31Not0; // to avoid collision and easily forged names
  for retry := 1 to 10 do // no endless loop
  begin
    // thread-safe unique file name generation
    result := Format('%s%s_%x.tmp',
      [FolderName, ExeName, InterlockedIncrement(_TmpCounter)]);
    if not FileExists(result) then
      exit;
  end;
  raise EOSException.Create('TemporaryFileName failed');
end;

function GetLastDelim(const FileName: TFileName; OtherDelim: cardinal): PtrInt;
var
  {$ifdef UNICODE}
  p: PWordArray;
  {$else}
  p: PByteArray;
  {$endif UNICODE}
  c: cardinal;
begin
  result := length(FileName);
  if result = 0 then
    exit;
  p := pointer(FileName);
  repeat
    c := p[result - 1];
    if (c = OtherDelim) or  (c = ord('\')) or (c = ord('/')) or (c = ord(':')) then
      exit;
    dec(result);
  until result = 0;
end;

function GetLastDelimU(const FileName: RawUtf8; OtherDelim: AnsiChar): PtrInt;
var
  c: AnsiChar;
begin
  result := length(FileName);
  if result = 0 then
    exit;
  repeat
    c := AnsiChar(PByteArray(FileName)[result - 1]);
    if (c = OtherDelim) or  (c = '\') or (c = '/') or (c = ':') then
      exit;
    dec(result);
  until result = 0;
end;

function ExtractPath(const FileName: TFileName): TFileName;
begin
  SetString(result, PChar(pointer(FileName)), GetLastDelim(FileName, 0));
end;

function ExtractName(const FileName: TFileName): TFileName;
begin
  result := copy(FileName, GetLastDelim(FileName, 0) + 1, maxInt);
end;

function ExtractNameU(const FileName: RawUtf8): RawUtf8;
begin
  result := copy(FileName, GetLastDelimU(FileName) + 1, maxInt);
end;

function ExtractPathU(const FileName: RawUtf8): RawUtf8;
begin
  FastSetString(result, pointer(FileName), GetLastDelimU(FileName));
end;

function ExtractExt(const FileName: TFileName; WithoutDot: boolean): TFileName;
var
  i: PtrInt;
begin
  result := '';
  i := GetLastDelim(FileName, ord('.'));
  if (i <= 1) or
     (FileName[i] <> '.') then
    exit;
  if WithoutDot then
    inc(i);
  result := copy(FileName, i, 100);
end;

function ExtractExtU(const FileName: RawUtf8; WithoutDot: boolean): RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  i := GetLastDelimU(FileName, '.');
  if (i <= 1) or
     (FileName[i] <> '.') then
    exit;
  if WithoutDot then
    inc(i);
  result := copy(FileName, i, 100);
end;

function ExtractExtP(const FileName: RawUtf8; WithoutDot: boolean): PUtf8Char;
var
  i: PtrInt;
begin
  result := nil;
  i := GetLastDelimU(FileName, '.') - 1;
  if i <= 0 then
    exit;
  result := PUtf8Char(pointer(FileName)) + i;
  if result^ <> '.' then
    result := nil
  else if WithoutDot then
    inc(result);
end;

function GetFileNameWithoutExt(const FileName: TFileName; Extension: PFileName): TFileName;
var
  i, max: PtrInt;
begin
  i := length(FileName);
  max := i - 16; // a file .extension is unlikely to be more than 16 chars
  while (i > 0) and
        not (cardinal(FileName[i]) in [ord('\'), ord('/'), ord('.'), ord(':')]) and
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
      Extension^ := copy(FileName, i, 100);
  end;
end;

function GetFileNameWithoutExtOrPath(const FileName: TFileName): RawUtf8;
begin
  result := RawUtf8(GetFileNameWithoutExt(ExtractFileName(FileName)));
end;

function PosExtString(Str: PChar): PChar; // work on AnsiString + UnicodeString
var
  i: PtrInt;
begin
  result := nil;
  if Str <> nil then // excludes '.' at first position e.g. for '.htdigest'
    for i := PStrLen(PAnsiChar(Str) - _STRLEN)^ - 1 downto 1 do
      case Str[i] of
        {$ifdef OSWINDOWS} '\', ':' {$else} '/' {$endif}:
          exit; // reached end of filename
        '.':
          begin
            result := @Str[i + 1]; // compare extension just after '.'
            exit;
          end;
      end;
end;

function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass; NoExpand: boolean): TFileName;
begin
  if Directory = '' then
    if RaiseExceptionOnCreationFailure <> nil then
      raise RaiseExceptionOnCreationFailure.Create('EnsureDirectoryExists('''')')
    else
      result := ''
  else
  begin
    if NoExpand then // caller won't store the result, so no full path needed
      result := Directory
    else
      result := ExpandFileName(Directory);
    result := IncludeTrailingPathDelimiter(result);
    if not DirectoryExists(result) then
      if not ForceDirectories(result) then
        if RaiseExceptionOnCreationFailure <> nil then
          raise RaiseExceptionOnCreationFailure.CreateFmt(
            'EnsureDirectoryExists(%s) failed as %s', [result, GetErrorShort])
        else
          result := '';
  end;
end;

function EnsureDirectoryExistsNoExpand(const Directory: TFileName): TFileName;
begin // circumvent a weird Delphi 7 compiler issue
  result := EnsureDirectoryExists(Directory, ExceptionClass(nil), {noexpand=}true);
end;

function NormalizeDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass): TFileName;
begin
  result := EnsureDirectoryExists(NormalizeFileName(Directory),
    RaiseExceptionOnCreationFailure);
end;

type // state machine for DirectoryDeleteOlderFiles() / DirectoryDeleteAll()
  TDirectoryDelete = class(TDirectoryBrowser)
  protected
    fDeleteBefore: TDateTime;
    fDeleteFolders, fDeleteError: boolean;
    fDeletedCount: integer;
    function OnFile(const FileInfo: TSearchRec;
      const FullFileName: TFileName): boolean; override;
    function OnFolder(const FullFolderName: TFileName): boolean; override;
  end;

function TDirectoryDelete.OnFile(const FileInfo: TSearchRec;
  const FullFileName: TFileName): boolean;
begin
  if (fDeleteBefore = 0) or
     (SearchRecToDateTimeUtc(FileInfo) < fDeleteBefore) then
    if DeleteFile(FullFileName) then
    begin
      inc(fTotalSize, FileInfo.Size);
      inc(fDeletedCount);
    end
    else
      fDeleteError := true;
  result := true; // continue
end;

function TDirectoryDelete.OnFolder(const FullFolderName: TFileName): boolean;
begin
  if fDeleteFolders then
    if not RemoveDir(FullFolderName) then
      fDeleteError := true;
  result := true; // continue
end;

function DirectoryDelete(const Directory: TFileName; const Mask: TFileName;
  DeleteOnlyFilesNotDirectory: boolean; DeletedCount: PInteger): boolean;
var
  browse: TDirectoryDelete;
begin
  browse := TDirectoryDelete.Create(Directory, [Mask], {recursive=}false);
  try
    browse.fDeleteFolders := not DeleteOnlyFilesNotDirectory;
    browse.Run;
    if DeletedCount <> nil then
      DeletedCount^ := browse.fDeletedCount;
    result := not browse.fDeleteError;
  finally
    browse.Free;
  end;
end;

function DirectoryDeleteAll(const Directory: TFileName): boolean;
begin
  result := DirectoryDeleteOlderFiles(Directory, 0, FILES_ALL, true, nil, true);
end;

function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName; Recursive: boolean;
  TotalSize: PInt64; DeleteFolders: boolean): boolean;
var
  browse: TDirectoryDelete;
begin
  browse := TDirectoryDelete.Create(Directory, [Mask], Recursive);
  try
    if TimePeriod > 0 then
      browse.fDeleteBefore := NowUtc - TimePeriod;
    browse.fDeleteFolders := DeleteFolders;
    browse.Run;
    if TotalSize <> nil then
      TotalSize^ := browse.fTotalSize;
    result := not browse.fDeleteError;
  finally
    browse.Free;
  end;
end;

function IsDirectoryWritable(const Directory: TFileName;
  Flags: TIsDirectoryWritable): boolean;
var
  dir, fmt, fn: TFileName;
  h: THandle;
  retry: integer;
begin
  // check the Directory itself
  result := false;
  if Directory = '' then
    exit;                       
  dir := ExcludeTrailingPathDelimiter(Directory);
  if not FileIsWritable(dir) then
    exit; // the folder does not exist or is read-only for the current user
  if idwAttributesOnly in Flags then
  begin
    result := true; // e.g. POSIX folder fpaccess() seems enough
    exit;
  end;
  {$ifdef OSWINDOWS}
  // ensure is not a system/virtual folder
  if ((idwExcludeWinUac in Flags) and
      IsUacVirtualFolder(dir)) or
     ((idwExcludeWinSys in Flags) and
      IsSystemFolder(dir)) then
    exit;
  // compute a non existing temporary file name in this Directory
  if idwTryWinExeFile in Flags then
    fmt := '%s\%x.exe'  // may trigger the anti-virus heuristic
  else
    fmt := '%s\%x.test'; // neutral file name
    // we tried .crt which triggered UAC heuristic but also some anti-viruses :(
  {$else}
  // compute a non existing temporary file name in this Directory
  fmt := '%s/.%x.test'; // make the file "invisible"
  {$endif OSWINDOWS}
  retry := 10;
  repeat
    fn := Format(fmt, [dir, Random32]);
    if not FileExists(fn) then
      break;
    dec(retry); // never loop forever
    if retry = 0 then
      exit;
  until false;
  // ensure we can create this temporary file
  h := FileCreate(fn);
  if not ValidHandle(h) then
    exit; // a file can't be created
  result := (not (idwWriteSomeContent in flags)) or // some pointers and hash
            (FileWrite(h, Executable, SizeOf(Executable)) = SizeOf(Executable));
  FileClose(h);
  if not DeleteFile(fn) then // success if the file can be created and deleted
    result := false;
end;

procedure AppendKb(Size: Int64; var Dest: ShortString; WithSpace: boolean);
const
  _U: array[1..5] of AnsiChar = 'KMGTE';
var
  u: PtrInt;
  b: Int64;
begin
  if Size < 0 then
    exit;
  u := 0;
  b := 1 shl 10;
  repeat
    if Size < b - (b shr 3) then
      break;
    b := b shl 10;
    inc(u);
  until u = high(_u);
  Size := (Size * 10000) shr (u * 10);
  SimpleRoundTo2DigitsCurr64(Size);
  AppendShortCurr64(Size, Dest, 1);
  if WithSpace then
    AppendShortChar(' ', @Dest);
  if u <> 0 then
    AppendShortChar(_U[u], @Dest);
  AppendShortChar('B', @Dest);
end;

{$ifndef NOEXCEPTIONINTERCEPT}

{$ifdef WITH_RAISEPROC} // for FPC on Win32 + Linux (Win64=WITH_VECTOREXCEPT)
var
  OldRaiseProc: TExceptProc;

procedure SynRaiseProc(Obj: TObject; Addr: CodePointer;
  FrameCount: integer; Frame: PCodePointer);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWord;
begin
  if (Obj <> nil) and
     Obj.InheritsFrom(Exception) and
     Assigned(_RawLogException) then
  begin
    backuplasterror := GetLastError;
    try
      ctxt.EClass := PPointer(Obj)^;
      ctxt.EInstance := Exception(Obj);
      ctxt.EAddr := PtrUInt(Addr);
      if Obj.InheritsFrom(EExternal) then // e.g. EDivByZero or EMathError
        ctxt.ELevel := sllExceptionOS
      else
        ctxt.ELevel := sllException; // regular "raise" exception
      ctxt.ETimestamp := UnixTimeUtc;
      ctxt.EStack := pointer(Frame);
      ctxt.EStackCount := FrameCount;
      _RawLogException(ctxt); // e.g. SynLogException() from mormot.core.log
      // note that SynLogException() will use PerThreadInfo.ExceptionIgnore
      // to avoid recursive exception loggin: _RawLogException should not be set
      // to nil or exceptions on concurrent threads would not be logged
    except
      { ignore any nested exception }
    end;
    SetLastError(backuplasterror); // may have changed above
  end;
  if Assigned(OldRaiseProc) then
    OldRaiseProc(Obj, Addr, FrameCount, Frame);
end;

{$endif WITH_RAISEPROC}

var
  RawExceptionIntercepted: boolean; // single global Exception interception

function RawExceptionIntercept(const Handler: TOnRawLogException): boolean;
begin
  result := false;
  GlobalLock;
  try
    _RawLogException := Handler; // e.g. SynLogException() from mormot.core.log
    if RawExceptionIntercepted or
       not Assigned(Handler) then
      exit;
    RawExceptionIntercepted := true; // intercept once
    {$ifdef WITH_RAISEPROC}
    // FPC RTL redirection function
    if not Assigned(OldRaiseProc) then
    begin
      OldRaiseProc := RaiseProc;
      RaiseProc := @SynRaiseProc;
      result := true;
    end;
    {$endif WITH_RAISEPROC}
    {$ifdef WITH_VECTOREXCEPT} // SEH32/SEH64 official API
    if not AddVectoredExceptionHandlerCalled then
    begin
      AddVectoredExceptionHandler(0, @SynLogVectoredHandler);
      AddVectoredExceptionHandlerCalled := true;
      result := true;
    end;
    {$endif WITH_VECTOREXCEPT}
    {$ifdef WITH_RTLUNWINDPROC}
    // Delphi x86 RTL redirection function
    if not Assigned(OldUnWindProc) then
    begin
      OldUnWindProc := RTLUnwindProc;
      RTLUnwindProc := @SynRtlUnwind;
      result := true;
    end;
    {$endif WITH_RTLUNWINDPROC}
  finally
    GlobalUnLock;
  end;
end;

{$endif NOEXCEPTIONINTERCEPT}


{ TMemoryMap }

function TMemoryMap.Map(aFile: THandle; aCustomSize: PtrUInt; aCustomOffset: Int64;
  aFileOwned: boolean; aFileSize: Int64; aForceMap: boolean): boolean;
var
  available: Int64;
begin
  fBuf := nil;
  fBufSize := 0;
  {$ifdef OSWINDOWS}
  fMap := 0;
  {$endif OSWINDOWS}
  fFileLocal := aFileOwned;
  fFile := aFile;
  if aFileSize < 0 then
    aFileSize := mormot.core.os.FileSize(fFile);
  fFileSize := aFileSize;
  if aFileSize = 0 then
  begin
    result := true; // no error on inexistant / void file (but no memory map)
    exit;
  end;
  result := false;
  if aCustomSize = 0 then
    fBufSize := fFileSize
  else
  begin
    available := fFileSize - aCustomOffset;
    if available < 0 then
      exit;
    if aCustomSize < available then
      available := aCustomSize; // truncate to what is needed
    fBufSize := available;
  end;
  {$ifdef CPU32}
  if fBufSize > Int64(MaxInt) then
    // maxInt = $7FFFFFFF = 1.999 GB (2GB would induce PtrInt errors on CPU32)
    exit; // use aCustomSize instead
  {$endif CPU32}
  fLoadedNotMapped := (fBufSize < 1 shl 20) and not aForceMap;
  if fLoadedNotMapped then
  begin
    // mapping is not worth it for size < 1MB which can be just read at once
    GetMem(fBuf, fBufSize);
    FileSeek64(fFile, aCustomOffset);
    if FileReadAll(fFile, fBuf, fBufSize) then
      result := true
    else
    begin
      FreeMem(fBuf);
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

function TMemoryMap.Map(const aFileName: TFileName; aForceMap: boolean;
  aCustomSize, aCustomOffset: Int64): boolean;
var
  h: THandle;
begin
  result := false;
  {$ifdef OSWINDOWS}
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless on Windows
  h := FileOpen(aFileName, fmOpenReadShared);
  {$else}
  h := FileOpenSequentialRead(aFileName); // but no lock is a good idea on POSIX
  {$endif OSWINDOWS}
  if not ValidHandle(h) then
    exit;
  result := Map(h, aCustomSize, aCustomOffset, {owned=}false, -1, aForceMap);
  if not result then
    FileClose(h);
  fFileLocal := result;
end;

procedure TMemoryMap.UnMap;
begin
  if fLoadedNotMapped then
    // mapping was not worth it
    FreeMem(fBuf)
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



{ TSynMemoryStreamMapped }

constructor TSynMemoryStreamMapped.Create(const aFileName: TFileName;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  fFileName := aFileName;
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless here
  fFileStream := TFileStreamEx.Create(aFileName, fmOpenReadShared);
  Create(fFileStream.Handle, aCustomSize, aCustomOffset);
end;

constructor TSynMemoryStreamMapped.Create(aFile: THandle;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  if not fMap.Map(aFile, aCustomSize, aCustomOffset) then
    EOSException.RaiseFmt(self, 'Create(%s) mapping error', [fFileName]);
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
  Instance: TLibHandle): boolean;
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
  if Size > 0 then
    result := true
  else
    Close; // paranoid check
end;

procedure TExecutableResource.Close;
begin
  if HGlobal = 0 then
    exit;
  UnlockResource(HGlobal); // only needed outside of Windows
  FreeResource(HGlobal);
  HGlobal := 0;
end;


{ ReserveExecutableMemory() / TFakeStubBuffer }

type
  // internal memory buffer created with PAGE_EXECUTE_READWRITE flags
  TFakeStubBuffer = class
  public
    Stub: PByteArray;
    StubUsed: cardinal;
    constructor Create;
    destructor Destroy; override;
    function Reserve(size: cardinal): pointer;
  end;

var
  CurrentFakeStubBuffer: TFakeStubBuffer;
  CurrentFakeStubBuffers: array of TFakeStubBuffer;
  CurrentFakeStubBufferLock: TLightLock;

constructor TFakeStubBuffer.Create;
begin
  Stub := StubMemoryAlloc;
  if Stub = nil then
    raise EOSException.Create('ReserveExecutableMemory(): OS mmap failed');
  PtrArrayAdd(CurrentFakeStubBuffers, self);
end;

destructor TFakeStubBuffer.Destroy;
begin
  StubMemoryFree(Stub);
  inherited;
end;

function TFakeStubBuffer.Reserve(size: cardinal): pointer;
begin
  result := @Stub[StubUsed];
  while size and 15 <> 0 do
    inc(size); // ensure the returned buffers are 16 bytes aligned
  inc(StubUsed, size);
end;

function ReserveExecutableMemory(size: cardinal
  {$ifdef CPUARM} ; ArmFakeStubAddr: pointer {$endif}): pointer;
begin
  if size > STUB_SIZE then
    raise EOSException.CreateFmt('ReserveExecutableMemory(size=%d>%d)',
      [size, STUB_SIZE]);
  CurrentFakeStubBufferLock.Lock;
  try
    {$ifdef CPUARM}
    StubCallFakeStubAddr := ArmFakeStubAddr; // for StubCallAllocMem()
    {$endif CPUARM}
    if (CurrentFakeStubBuffer = nil) or
       (CurrentFakeStubBuffer.StubUsed + size > STUB_SIZE) then
      CurrentFakeStubBuffer := TFakeStubBuffer.Create;
    result := CurrentFakeStubBuffer.Reserve(size);
  finally
    CurrentFakeStubBufferLock.UnLock;
  end;
end;

const
  OS_ALIGNED = 128 shl 10; // call the OS for any block >= 128KB

function GetMemAligned(Size: PtrUInt; FillWith: pointer): pointer;
var
  pad: PtrUInt;
begin
  if Size >= OS_ALIGNED then
  begin
    Size := (Size + 65535) and not 65535;   // default wrap to 64KB boundaries
    result := _GetLargeMem(Size)            // mmap/VirtualAlloc is 4KB aligned
  end
  else // smaller blocks will just use the heap, with a padding hidden prefix
  begin
    GetMem(result, Size + 16); // 15 bytes for alignment + 1 byte for padding
    pad := 16 - (PtrUInt(result) and 15);   // adjust by 1..16 bytes
    inc(PAnsiChar(result), pad);            // Delphi Win32 only needs padding
    PAnsiChar(result)[-1] := AnsiChar(pad); // store the padding in p[-1]
  end;
  if FillWith <> nil then
    MoveFast(FillWith^, result^, Size);
end;

procedure FreeMemAligned(p: pointer; Size: PtrUInt);
begin
  if p = nil then
    exit;
  if Size >= OS_ALIGNED then
  begin
    Size := (Size + 65535) and not 65535;   // as in GetMemAligned()
    _FreeLargeMem(p, Size);                 // munmap or VirtualFree
    exit;
  end;
  dec(PAnsiChar(p), ord(PAnsiChar(p)[-1])); // adjust p[-1]=1..16 padding bytes
  FreeMem(p);
end;

function SeemsRealObject(p: pointer): boolean;
var
  i: PtrInt;
begin
  result := false;
  if SeemsRealPointer(p) then
  try
    p := PPointer(p)^; // p = vmt
    if (not SeemsRealPointer(p)) or
       (PPtrInt(PAnsiChar(p) + vmtInstanceSize)^ < sizeof(pointer)) or
       (PPtrInt(PAnsiChar(p) + vmtDestroy)^ = 0) then
      exit;
    p := PPointer(PAnsiChar(p) + vmtClassName)^; // p = ClassName: PShortString
    if (not SeemsRealPointer(p)) or
       (PByte(p)^ = 0) then // length(ClassName)
      exit;
    for i := 1 to PByte(p)^ do
      if PAnsiChar(p)[i] <= ' ' then
        exit; // should be a valid ASCII or UTF-8 pascal identifier
    result := true;
  except
    result := false; // paranoid
    {$ifdef OSWINDOWS}
    LastMemInfo.State := 0; // reset VirtualQuery() cache
    {$endif OSWINDOWS}
  end;
end;

{$ifndef PUREMORMOT2}
function GetDelphiCompilerVersion: RawUtf8;
begin
  result := COMPILER_VERSION;
end;
{$endif PUREMORMOT2}

function RetrieveSystemTimesText: TShort23;
var
  I, K, U, S: Int64;
begin // return 'U:usr K:krn' percents on windows
  result[0] := #0;
  if not RetrieveSystemTimes(I, K, U) then
    exit;
  dec(K, I); // raw KernelTime includes IdleTime with GetSystemTimes() WinAPI
  S := I + K + U;
  if S = 0 then
    exit;
  U := (U * 1000000) div S;
  K := (K * 1000000) div S;
  PCardinal(@result)^ := 2 + ord('U') shl 8 + ord(':') shl 16;
  AppendShortCurr64(U, result, {fixeddecimals=}2);
  AppendShort(' K:', result);
  AppendShortCurr64(K, result, {fixeddecimals=}2);
end;

procedure AppendFreeTotalKB(free, total: QWord; var dest: ShortString);
begin
  AppendKb(free, dest);
  AppendShortChar('/', @dest);
  AppendKb(total, dest);
  AppendShortChar(' ', @dest);
end;

function GetMemoryInfoText: TShort31;
var
  info: TMemoryInfo;
begin
  result[0] := #0;
  if not GetMemoryInfo(info, false) then
    exit;
  AppendFreeTotalKB(info.memtotal - info.memfree, info.memtotal, result);
  AppendShortChar('(', @result);
  AppendShortCardinal(info.percent, result);
  AppendShortTwoChars(ord('%') + ord(')') shl 8, @result);
end;

function GetDiskAvailable(aDriveFolderOrFile: TFileName): QWord;
var
  free, total: QWord; // dummy values
begin
  if not GetDiskInfo(aDriveFolderOrFile, result, free, total) then
    result := 0;
end;

function GetSystemInfoText: RawUtf8;
var
  avail, free, total: QWord;
begin
  GetDiskInfo(Executable.ProgramFilePath, avail, free, total);
  _fmt('Current UTC date: %s (%d)'+ CRLF +'Memory used: %s'+ CRLF +
       'Current disk free: %s/%s'+ CRLF +'Load: %s'+ CRLF +
       'Exe: %s'+ CRLF +'OS: %s'+ CRLF +'Cpu: %s'+ CRLF +'Bios: %s'+ CRLF,
    [FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', NowUtc), UnixTimeUtc,
     GetMemoryInfoText, KB(avail), KB(total), RetrieveLoadAvg,
     Executable.Version.VersionInfo, OSVersionText, CpuInfoText, BiosInfoText],
     result);
end;

procedure RetrieveSysInfoText(out text: ShortString);
var
  si: TSysInfo;  // Linuxism, but properly emulated in thit unit on Mac/BSD
begin
  text[0] := #0;
  AppendShortCardinal(SystemInfo.dwNumberOfProcessors, text);
  if not RetrieveSysInfo(si) then // single syscall on Linux/Android
    exit;
  AppendShortChar(' ', @text); // si.loads[0/1] = user kern on Windows
  AppendShortCurr64((Int64(si.loads[0]) * CURR_RES + 5000) shr 16, text, 2);
  AppendShortChar(' ', @text);
  AppendShortCurr64((Int64(si.loads[1]) * CURR_RES + 5000) shr 16, text, 2);
  AppendShortChar(' ', @text);
  {$ifdef OSPOSIX} // si.loads[0/1/2] = avg1 avg5 avg15 on POSIX
  AppendShortCurr64((Int64(si.loads[2]) * CURR_RES + 5000) shr 16, text, 2);
  AppendShortChar(' ', @text);
  inc(si.freeram, si.bufferram);
  {$endif OSPOSIX}
  if si.uptime > SecsPerDay then // optional [ndays]
  begin
    AppendShortCardinal(cardinal(si.uptime) div SecsPerDay, text);
    AppendShortChar(' ', @text);
  end;
  AppendFreeTotalKB(QWord(si.totalram - si.freeram) * si.mem_unit,
                    QWord(si.totalram) * si.mem_unit, text);
  if si.freeswap < si.totalswap shr 2 then // include swap if free below 25%
    AppendFreeTotalKB(QWord(si.totalswap - si.freeswap) * si.mem_unit,
                      QWord(si.totalswap) * si.mem_unit, text);
  AppendShortIntHex(OSVersionInt32, text); // identify and OS version
end;


procedure ConsoleWriteRaw(const Text: RawUtf8; NoLineFeed: boolean);
begin
  ConsoleWrite(Text, ccLightGray, NoLineFeed, {nocolor=}true);
end;

procedure ConsoleWriteLn;
begin
  ConsoleWrite(CRLF, ccLightGray, {nolinefeed=}true, {nocolor=}true);
end;

function ConsoleReadBody: RawByteString;
var
  len, n: integer;
  p: PByte;
begin
  len := ConsoleStdInputLen;
  p := FastNewRawByteString(result, len);
  while len > 0 do
  begin
    n := FileRead(StdInputHandle, p^, len);
    if n <= 0 then
    begin
      result := ''; // read error
      break;
    end;
    dec(len, n);
    inc(p, n);
  end;
end;

function _ToConsole(self: TObject; const text: RawByteString; pid: cardinal): boolean;
begin
  result := false; // continue
  if (text = '') or
     not HasConsole then
    exit;
  ConsoleCriticalSection.Lock;
  try
    FileWriteAll(StdOut, pointer(text), length(text)); // no code page involved
  finally
    ConsoleCriticalSection.UnLock;
  end;
end;

procedure AllocConsole;
begin
  TMethod(RedirectToConsole).Code := @_ToConsole;
  {$ifdef OSWINDOWS}
  WinAllocConsole;
  {$endif OSWINDOWS}
end;

var
  GlobalCriticalSection: TOSLock;

{ TSynLibrary }

function TSynLibrary.Resolve(const Prefix: RawUtf8; ProcName: PAnsiChar;
  Entry: PPointer; RaiseExceptionOnFailure: ExceptionClass; SilentError: PString): boolean;
var
  p: PAnsiChar;
  name, search: RawUtf8;
  ignoremissing: boolean;
  error: string;
  {$ifdef OSPOSIX}
  dlinfo: dl_info;
  {$endif OSPOSIX}
begin
  result := false;
  if (Entry = nil) or
     (fHandle = 0) or
     (ProcName = nil) then
    exit; // avoid GPF
  p := ProcName; // transient copy to keep ProcName for error message below
  ignoremissing := false;
  repeat
    name := _GetNextSpaced(p); // try all alternate 'name1 name2 ... name#'
    if name = '' then
      break;
    if name[1] = '?' then
    begin
      ignoremissing := true;
      delete(name, 1, 1);
    end;
    Join([Prefix, name], search);
    Entry^ := LibraryResolve(fHandle, pointer(search));
    if (Entry^ = nil) and
       (Prefix <> '') then // try without the prefix
      Entry^ := LibraryResolve(fHandle, pointer(name));
    result := Entry^ <> nil;
  until result;
  {$ifdef OSPOSIX} // on POSIX we can retrieve the fully response LibraryPath
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
  result := result or ignoremissing;
  if result or
     ((RaiseExceptionOnFailure = nil) and
      (SilentError = nil)) then
    exit;
  FreeLib; // abort loading
  error := Format('%s.Resolve(''%s%s''): not found in %s',
    [ClassNameShort(self)^, Prefix, ProcName, LibraryPath]);
  if RaiseExceptionOnFailure <> nil then
    raise RaiseExceptionOnFailure.Create(error)
  else if SilentError <> nil then
    SilentError^:= error;
end;

function TSynLibrary.ResolveAll(ProcName: PPAnsiChar; Entry: PPointer;
  const Prefix: RawUtf8; RaiseExceptionOnFailure: ExceptionClass;
  SilentError: PString): boolean;
begin
  result := true;
  while ProcName^ <> nil do
  begin
    if not Resolve(Prefix, ProcName^, Entry, RaiseExceptionOnFailure, SilentError) then
    begin
      result := false;
      if SilentError <> nil then
        exit; // Resolve() made FreeLib anyway
    end;
    inc(ProcName);
    inc(Entry);
  end;
end;

function TSynLibrary.TryLoadResolve(const aLibrary: array of TFileName;
  const Prefix: RawUtf8; ProcName: PPAnsiChar; Entry: PPointer;
  RaiseExceptionOnFailure: ExceptionClass; SilentError: PString): boolean;
begin
  result := TryLoadLibrary(aLibrary, RaiseExceptionOnFailure) and
      ResolveAll(ProcName, Entry, Prefix, RaiseExceptionOnFailure, SilentError);
end;

destructor TSynLibrary.Destroy;
begin
  FreeLib;
  inherited Destroy;
end;

procedure TSynLibrary.FreeLib;
begin
  if fHandle = 0 then
    exit; // nothing to free
  LibraryClose(fHandle);
  fHandle := 0;
end;

function TSynLibrary.TryLoadLibrary(const aLibrary: array of TFileName;
  aRaiseExceptionOnFailure: ExceptionClass; aSilentError: PString): boolean;
var
  i: PtrInt;
  libs, nwd: TFileName;
  err: string;

  function LoadOne(lib: TFileName; current: PtrInt): boolean;
  var
    j: PtrInt;
  begin
    // check library name
    result := false;
    if lib = '' then
      exit;
    for j := 0 to current - 1 do
      if aLibrary[j] = lib then
        exit; // don't try twice the same library name
    // try to open this library
    nwd := ExtractFilePath(lib);
    if nwd = '' then // has no specific path -> try exe folder or global path
      if fTryFromExecutableFolder and
         FileExists(Executable.ProgramFilePath + lib) then
      begin
        nwd := Executable.ProgramFilePath;
        lib := nwd + lib;
      end
      else if (LibraryGlobalPath <> '') and
              FileExists(LibraryGlobalPath + lib) then
      begin
        nwd := LibraryGlobalPath;
        lib := nwd + lib;
      end;
    if {%H-}libs = '' then
      libs := lib
    else
      libs := libs + ', ' + lib; // include path
    // change the current folder at loading on Windows
    {$ifdef OSWINDOWS}
    try
      if nwd <> '' then
      begin
        GlobalLock; // SetDllDirectoryW() is for the whole process not thread
        if not LibrarySetDirectory(nwd) then // as documented on microsoft.com
        begin
          GlobalUnLock;
          nwd := '';
        end;
      end;
      fHandle := LibraryOpen(lib); // preserve x87 flags and prevent msg box
    finally
      if nwd <> '' then
      begin
        SetDllDirectoryW(nil); // revert to default
        GlobalUnLock;
      end;
    end;
    {$else}
    fHandle := LibraryOpen(lib); // use regular .so loading behavior
    {$endif OSWINDOWS}
    if fHandle <> 0 then // found this library
    begin
      {$ifdef OSWINDOWS} // on POSIX, will call dladdr() in Resolve()
      fLibraryPath := GetModuleName(fHandle);
      if length(fLibraryPath) < length(lib) then
      {$endif OSWINDOWS}
        fLibraryPath := lib;
      result := true;
      exit;
    end;
    // handle any error
    err := LibraryError;
    if err <> '' then
      libs := libs + ' [' + err + ']';
  end;

begin
  result := true;
  for i := 0 to high(aLibrary) do
    if LoadOne(aLibrary[i], i) then
      exit;
  libs := Format('%s.TryLoadLibray failed - searched in %s',
    [ClassNameShort(self)^, libs]);
  if aRaiseExceptionOnFailure <> nil then
    raise aRaiseExceptionOnFailure.Create(libs)
  else if aSilentError <> nil then
    aSilentError^ := libs;
  result := false;
end;

function TSynLibrary.Exists: boolean;
begin
  result := (self <> nil) and
            (fHandle <> 0);
end;


function LibraryAvailable(var State: TLibraryState; Init: TProcedure): boolean;
begin
  if State = lsUnTested then
  begin
    GlobalLock; // thread-safe check and initialization
    try
      if State = lsUntested then
        Init; // should eventually set State as lsAvailable or lsNotAvailable
    finally
      GlobalUnLock;
    end;
  end;
  result := State = lsAvailable;
end;


{ TFileVersion }

constructor TFileVersion.Create(const aFileName: TFileName;
  aMajor, aMinor, aRelease, aBuild: integer; aBuildDate: TDateTime);
var
  m, d: word;
begin
  fFileName := aFileName;
  SetVersion(aMajor, aMinor, aRelease, aBuild);
  if aBuildDate = 0 then // get build date from file age
    aBuildDate := FileAgeToDateTime(aFileName);
  fBuildDateTime := aBuildDate;
  if aBuildDate <> 0 then
    DecodeDate(aBuildDate, BuildYear, m, d);
end;

function TFileVersion.Version32: integer;
begin
  if self = nil then
    result := 0
  else
    result := Major shl 16 + Minor shl 8 + Release;
end;

function TFileVersion.SetVersion(aMajor, aMinor, aRelease, aBuild: integer): boolean;
begin
  result := (Major <> aMajor) or
            (Minor <> aMinor) or
            (Release <> aRelease) or
            (Build <> aBuild);
  if not result then
    exit;
  Major := aMajor;
  Minor := aMinor;
  Release := aRelease;
  Build := aBuild;
  Main := Format('%d.%d', [Major, Minor]);
  if Build <> 0 then
    fDetailed := Format('%s.%d.%d', [Main, Release, Build])
  else if Release <> 0 then
    fDetailed := Format('%s.%d', [Main, Release])
  else
    fDetailed := Main;
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
      _fmt('%s %s (%s)', [ExtractFileName(fFileName),
        DetailedOrVoid, BuildDateTimeString], fVersionInfo);
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
      _fmt('%s/%s%s', [GetFileNameWithoutExtOrPath(fFileName), DetailedOrVoid,
        OS_INITIAL[OS_KIND]], fUserAgent);
      {$ifdef OSWINDOWS}
      if OSVersion in WINDOWS_32 then
        AppendShortToUtf8('32', fUserAgent);
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
  out ProgramName, ProgramVersion: RawUtf8; out OS: TOperatingSystem): boolean;
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
    if not (UserAgent[i] in ['0' .. '9', '.']) then
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
  p: PAnsiChar;
  i: PtrInt;
  ver: array[0 .. 3] of integer;
begin
  p := pointer(aVersionText);
  for i := 0 to 3 do
    ver[i] := _GetNextCardinal(p);
  SetExecutableVersion(ver[0], ver[1], ver[2], ver[3]);
end;

procedure AfterExecutableInfoChanged; // set Executable.ProgramFullSpec+Hash
begin
  with Executable do
  begin
    _fmt('%s %s (%s)', [ProgramFileName,
      Version.DetailedOrVoid, Version.BuildDateTimeString], ProgramFullSpec);
    Hash.c0 := Version.Version32;
    {$ifdef OSLINUXANDROID}
    Hash.c0 := crc32c(Hash.c0, pointer(CpuInfoLinux), length(CpuInfoLinux));
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

procedure GetExecutableVersion;
begin
  if Executable.Version.RetrieveInformationFromFileName then
    AfterExecutableInfoChanged;
end;

procedure TrimDualSpaces(var s: RawUtf8);
var
  i: PtrInt;
begin
  i := 1;
  repeat
    i := PosEx('  ', s, i);
    if i = 0 then
      break;
    delete(s, i, 1); // dual spaces -> single space
  until false;
  TrimSelf(s);
end;

procedure InitializeProcessInfo; // called once at startup
var
  dt: TDateTime;
  rnd: PBlock128;
begin
  TrimDualSpaces(OSVersionText); // clean InitializeSpecificUnit info
  TrimDualSpaces(OSVersionInfoEx);
  {$ifndef OSLINUXANDROID} TrimDualSpaces(BiosInfoText); {$endif}
  TrimDualSpaces(CpuInfoText);
  OSVersionShort := ToTextOSU(OSVersionInt32);
  with Executable do            // retrieve Executable + Host/User info
  begin
    {$ifdef OSWINDOWS}
    ProgramFileName := ParamStr(0); // RTL seems just fine here
    dt := FileAgeToDateTime(ProgramFileName);
    {$else}
    dt := 0;
    dladdr(@InitializeProcessInfo, @PosixProgramInfo);
    GetDlInfoName(PosixProgramInfo, ProgramFileName);
    if ProgramFileName <> '' then
    begin
      dt := FileAgeToDateTime(ProgramFileName);
      if dt = 0 then
        ProgramFileName := '';
    end;
    if ProgramFileName = '' then
      ProgramFileName := ExpandFileName(ParamStr(0));
    crcblock(@SystemEntropy.Startup, @PosixProgramInfo); // won't hurt
    {$endif OSWINDOWS}
    ProgramFilePath := ExtractFilePath(ProgramFileName);
    if IsLibrary then
      InstanceFileName := GetModuleName(HInstance)
    else
      InstanceFileName := ProgramFileName;
    ProgramName := GetFileNameWithoutExtOrPath(ProgramFileName);
    GetUserHost(User, Host);
    if Host = '' then
      Host := 'unknown';
    if User = '' then
      User := 'unknown';
    Version := TFileVersion.Create(ProgramFileName, 0, 0, 0, 0, dt);
    Command := TExecutableCommandLine.Create;
    Command.ExeDescription := ProgramName;
    Command.Parse;
  end;
  AfterExecutableInfoChanged; // set Executable.ProgramFullSpec+Hash
  // finalize SystemEntropy.Startup and setup SharedRandom instance
  rnd := @SystemEntropy.Startup;
  crcblocks(rnd, @BaseEntropy, SizeOf(BaseEntropy) shr 4); // cpuid+rdrand+rdtsc
  PBlock128(@SharedRandom.Generator)^ := rnd^;
  SharedRandom.Generator.SeedGenerator; // we have enough entropy yet
  crcblock(rnd, @Executable.Hash);
  crcblocks(rnd, @CpuCache, SizeOf(CpuCache) div SizeOf(THash128));
end;

procedure SetExecutableVersion(aMajor, aMinor, aRelease, aBuild: integer);
begin
  if Executable.Version.SetVersion(aMajor, aMinor, aRelease, aBuild) then
    AfterExecutableInfoChanged; // re-compute if changed
end;


{ TExecutableCommandLine }

function TExecutableCommandLine.SwitchAsText(const v: RawUtf8): RawUtf8;
begin
  Join([fSwitch[length(v) > 1], v], result);
end;

procedure TExecutableCommandLine.Describe(const v: array of RawUtf8;
  k: TExecutableCommandLineKind; d, def: RawUtf8; argindex: integer);
var
  i, j: PtrInt;
  desc, param, pnames, sp: RawUtf8;
begin
  if (self = nil) or
     (d = '') then
    exit;
  if k <> clkArg then
  begin
    if high(v) < 0 then
      exit;
    desc := SwitchAsText(v[0]);
    if length(v[0]) <> 1 then
      desc := Join(['    ', desc]); // right align --#
    for i := 1 to high(v) do
      desc := Join([desc, ', ', SwitchAsText(v[i])]);
  end;
  if k <> clkOption then
  begin
    i := PosExChar('#', d); // #valuename in description -> <valuename>
    if i > 0 then
    begin
      j := 1;
      while d[i + j] > ' ' do
        inc(j);
      delete(d, i, 1); // remove #
      if d[i] <> '#' then
        param := copy(d, i, j - 1) // extract '<valuename>'
      else
      begin
        param := copy(d, i + 1, j - 2); // ##type
        delete(d, i, j);                // not included in description
      end;
    end
    else if k = clkArg then
      if high(v) = 0 then
        param := v[0]
      else if argindex > 0 then
        _fmt('arg%d', [argindex], param)
      else
        param := 'arg'
    else
    begin
      i := PosEx(' - values: ', d); // see SetObjectFromExecutableCommandLine()
      if i > 0 then
      begin
        inc(i, 11);
        j := 1;
        if copy(d, i, 7) = 'set of ' then
          inc(j, 7);
        while d[i + j] > ' ' do
          inc(j);
        param := copy(d, i, j);
        dec(i, 11);
        delete(d, i, j + 11);
        if j > 50 then
        begin
          j := 50;
          for i := 50 downto 1 do
            if param[i] in [',', '|'] then
            begin
              j := i;
              break;
            end;
          insert(Join([fLineFeed, '         ']), param, j + 1);
        end;
      end
      else
        param := 'value';
    end;
    desc := Join([desc, ' <', param, '>']);
    if (k = clkArg) and
       (argindex > 0) then
    begin
      if argindex > length(fDescArg) then
        SetLength(fDescArg, argindex);
      fDescArg[argindex - 1] := param;
    end;
  end;
  fDesc[k] := Join([fDesc[k], ' ', desc]);
  j := 1;
  if fSwitch[true] <> '--' then
    repeat
      i := PosEx('--', d, j); // e.g. '--switch' -> '/switch' on Windows
      if i = 0 then
        break;
      delete(d, i, 2);
      insert(fSwitch[true], d, i);
      j := i;
    until false;
  if def <> '' then
    def := Join([' (default ', def, ')']);
  _fmt('  %0:-20s', [Join([desc + def])], pnames);
  if (length(pnames) > 22) or
     (length(d) > 80) then
  begin
    // write description on next line(s)
    sp := fLineFeed + '                      ';
    while length(d) > 57 do
    begin
      j := 57;
      for i := 57 downto 1 do
        if d[i] = ' ' then
        begin
          j := i;
          break;
        end;
      if j = 57 then
        for i := 57 downto 1 do
          if d[i] in [',', ';', '|'] then
          begin
            j := i;
            break;
          end;
      pnames := Join([pnames, sp, copy(d, 1, j)]);
      delete(d, 1, j);
    end;
    pnames := Join([pnames, sp, d]);
  end
  else
    pnames := pnames + d; // we can put everything on the same line
  fDescDetail[k] := Join([fDescDetail[k], pnames, fLineFeed]);
end;

function TExecutableCommandLine.Find(const v: array of RawUtf8;
  k: TExecutableCommandLineKind; const d, def: RawUtf8; f: PtrInt): PtrInt;
var
  i: PtrInt;
begin
  if self <> nil then
  begin
    if k <> clkUndefined then
      Describe(v, k, d, def, -1);
    if (high(v) >= 0) and
       (fNames[k] <> nil) then
      for i := 0 to high(v) do
      begin
        result := FindNonVoid[fCaseSensitiveNames](
          @fNames[k][f], pointer(v[i]), length(v[i]), length(fNames[k]) - f);
        if result >= 0 then
        begin
          inc(result, f);
          fRetrieved[k][result] := true;
          exit;
        end;
      end;
  end;
  result := -1
end;

function TExecutableCommandLine.Arg(index: integer; const description: RawUtf8;
  optional: boolean): boolean;
var
  n: PtrUInt;
begin
  result := self <> nil;
  if not result then
    exit;
  n := length(fNames[clkArg]);
  result := PtrUInt(index) < n;
  if result then
    fRetrieved[clkArg][index] := true
  else
  begin
    SetLength(fRetrieved[clkArg], n + 1); // to notify missing <arg>
    if optional then
      fRetrieved[clkArg][index] := true;
  end;
  Describe([], clkArg, description, '', index + 1);
end;

function TExecutableCommandLine.ArgU(index: integer; const description: RawUtf8;
  optional: boolean): RawUtf8;
begin
  result := '';
  if Arg(index, description, optional) then
    result := Args[index];
end;

function TExecutableCommandLine.ArgString(index: integer;
  const description: RawUtf8; optional: boolean): string;
begin
  result := '';
  if Arg(index, description, optional) then
    result := string(Args[index]);
end;

function TExecutableCommandLine.ArgFile(index: integer;
  const description: RawUtf8; optional, isFolder: boolean): TFileName;
begin
  result := ArgString(index, description, optional);
  if result <> '' then
    result := CheckFileName(result, isFolder);
end;

const
  FD: array[boolean] of string[7] = ('File', 'Folder');

function TExecutableCommandLine.CheckFileName(const name: TFileName;
  isFolder: boolean): TFileName;
begin
  result := ExpandFileName(name);
  if isFolder then
  begin
    if DirectoryExists(result) then
      exit;
  end
  else if FileExists(result) then
    exit;
  fUnknown := _fmt('%s%s %s does not exist%s',
    [fUnknown, FD[isFolder], result, fLineFeed]);
end;

function TExecutableCommandLine.Arg(const name, description: RawUtf8): boolean;
begin
  result := Arg([name], description);
end;

function TExecutableCommandLine.Arg(const name: array of RawUtf8;
  const description: RawUtf8): boolean;
begin
  result := Find(name, clkArg, description) >= 0;
end;

function UnAmp(const name: RawUtf8): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  i := PosExChar('&', name);
  SetLength(result, ord(i <> 0) + 1);
  result[0] := name;
  if i = 0 then
    exit;
  delete(result[0], i, 1);
  result[1] := result[0]; // &# char first
  result[0] := copy(name, i + 1, 1);
end;

function TExecutableCommandLine.Option(const name, description: RawUtf8): boolean;
begin
  result := Find(UnAmp(name), clkOption, description) >= 0
end;

function TExecutableCommandLine.Option(const name: array of RawUtf8;
  const description: RawUtf8): boolean;
begin
  result := Find(name, clkOption, description) >= 0;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; out value: RawUtf8;
  const description, default: RawUtf8): boolean;
begin
  result := Get(UnAmp(name), value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: TRawUtf8DynArray; const description: RawUtf8): boolean;
var
  first, i: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  Describe(name, clkParam, description, '', -1);
  first := 0;
  repeat
    i := Find(name, clkParam, '', '', first);
    if i < 0 then
      break; // no more occurence
    if fValues[i] <> '' then
    begin
      _AddRawUtf8(value, fValues[i]);
      result := true;
    end;
    first := i + 1;
  until first >= length(fValues);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: RawUtf8; const description, default: RawUtf8): boolean;
var
  i: PtrInt;
begin
  if self = nil then
    i := -1
  else
    i := Find(name, clkParam, description, default);
  if i >= 0 then
  begin
    value := Values[i];
    result := true;
  end
  else
  begin
    value := default;
    result := false;
  end;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; out value: string;
  const description: RawUtf8; const default: string): boolean;
begin
  result := Get(UnAmp(name), value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: string; const description: RawUtf8; const default: string): boolean;
var
  tmp: RawUtf8;
begin
  result := Get(name, tmp, description);
  if result then
    value := string(tmp)
  else
    value := default; // no conversion needed
end;

function TExecutableCommandLine.Get(const name: RawUtf8;
  out value: TStringDynarray; const description: RawUtf8): boolean;
begin
  result := Get(UnAmp(name), value, description);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: TStringDynarray; const description: RawUtf8): boolean;
var
  tmp: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := Get(name, tmp, description);
  SetLength(value, length(tmp));
  for i := 0 to length(tmp) - 1 do
    value[i] := string(tmp[i]);
end;

function TExecutableCommandLine.Get(const name: RawUtf8;
  out value: integer; const description: RawUtf8; default: integer): boolean;
begin
  result := Get(UnAmp(name), value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: integer; const description: RawUtf8; default: integer): boolean;
var
  i: PtrInt;
  def: RawUtf8;
begin
  i := -1;
  if self <> nil then
  begin
    if default <> maxInt then
      ShortStringToAnsi7String(ToShort(default), def); // no Delphi str(RawUtf8)
    i := Find(name, clkParam, description, def);
  end;
  result := (i >= 0) and
            ToInteger(Values[i], value);
  if not result and
     (default <> maxInt) then
    value := default;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; min, max: integer;
  out value: integer; const description: RawUtf8; default: integer): boolean;
begin
  result := Get(UnAmp(name), min, max, value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  min, max: integer; out value: integer; const description: RawUtf8;
  default: integer): boolean;
begin
  result := Get(name, value, description, default) and
            (value >= min) and
            (value <= max);
end;

function TExecutableCommandLine.Has(const name: RawUtf8): boolean;
begin
  result := Find(UnAmp(name), clkParam) >= 0;
end;

function TExecutableCommandLine.Has(const name: array of RawUtf8): boolean;
begin
  result := Find(name, clkParam) >= 0;
end;

function TExecutableCommandLine.Param(
  const name, description, default: RawUtf8): RawUtf8;
begin
  Get(UnAmp(name), result, description, default);
end;

function TExecutableCommandLine.Param(const name: array of RawUtf8;
  const description, default: RawUtf8): RawUtf8;
begin
  Get(name, result, description, default);
end;

function TExecutableCommandLine.ParamS(const name: array of RawUtf8;
  const description: RawUtf8; const default: string): string;
begin
  Get(name, result, description, default);
end;

function TExecutableCommandLine.Param(const name: RawUtf8;
  default: integer; const description: RawUtf8): integer;
begin
  Get(UnAmp(name), result, description, default);
end;

function TExecutableCommandLine.ParamS(const name: RawUtf8;
  const description: RawUtf8; const default: string): string;
begin
  Get(UnAmp(name), result, description, default);
end;

function TExecutableCommandLine.Param(const name: array of RawUtf8;
  default: integer;const description: RawUtf8): integer;
begin
  Get(name, result, description, default);
end;

const
  CLK_TXT: array[clkOption .. clkParam] of RawUtf8 = (
    ' [options]', ' [params]');
  CLK_DESCR: array[clkOption .. clkParam] of RawUtf8 = (
    'Options', 'Params');
  CASE_DESCR: array[boolean] of RawUtf8 = (
    ':', ' (case-sensitive):');

function TExecutableCommandLine.FullDescription(
  const customexedescription, exename, onlyusage: RawUtf8): RawUtf8;
var
  clk: TExecutableCommandLineKind;
begin
  if customexedescription <> '' then
    fExeDescription := customexedescription;
  result := Join([fExeDescription, fLineFeed, fLineFeed, 'Usage: ']);
  if exename = '' then
    result := result + Executable.ProgramName
  else
    result := result + exename;
  result := result + fDesc[clkArg];
  for clk := low(CLK_TXT) to high(CLK_TXT) do
    if fDesc[clk] <> '' then
      result := result + CLK_TXT[clk];
  result := result + fLineFeed;
  if onlyusage <> '' then
    result := result + onlyusage
  else
    for clk := low(fDescDetail) to high(fDescDetail) do
      if fDescDetail[clk] <> '' then
      begin
        if clk in [low(CLK_DESCR) .. high(CLK_DESCR)] then
          result := Join([result,
                    fLineFeed, CLK_DESCR[clk], CASE_DESCR[CaseSensitiveNames]]);
        result := Join([result, fLineFeed, fDescDetail[clk]]);
      end;
end;

function TExecutableCommandLine.DetectUnknown: RawUtf8;
var
  clk: TExecutableCommandLineKind;
  i: PtrInt;
begin
  result := fUnknown;
  for clk := low(fRetrieved) to high(fRetrieved) do
    for i := 0 to length(fRetrieved[clk]) - 1 do
      if not fRetrieved[clk][i] then
        if clk = clkArg then
          if i >= length(fDescArg) then
            result := Join([result, 'Unexpected "', fRawParams[i], '" argument', fLineFeed])
          else
            result := Join([result, 'Missing <', fDescArg[i], '> argument', fLineFeed])
        else
        begin
          result := Join([result, 'Unexpected ', SwitchAsText(fNames[clk][i]), ' ']);
          case clk of
            clkOption:
              result := Join([result, 'option']);
            clkParam:
              result := Join([result, fValues[i], ' parameter']);
          end;
          result := result + fLineFeed;
        end;
end;

function TExecutableCommandLine.ConsoleWriteUnknown(
  const exedescription: RawUtf8): boolean;
var
  err: RawUtf8;
begin
  err := DetectUnknown;
  result := err <> '';
  if not result then
    exit;
  ConsoleWrite(FullDescription(exedescription));
  ConsoleWrite(err, ccLightRed);
end;

function TExecutableCommandLine.ConsoleHelpFailed(
  const exedescription: RawUtf8): boolean;
begin
  if exedescription <> '' then
    fExeDescription := exedescription;
  result := Option(['?', 'h', 'help'], 'display this help');
  if result then
    ConsoleWrite(FullDescription)
  else
    result := ConsoleWriteUnknown(exedescription);
end;

procedure TExecutableCommandLine.Clear;
begin
  CleanupInstance; // finalize all TRawUtf8DynArray fields
end;

function TExecutableCommandLine.Parse(
  const DescriptionLineFeed, ShortSwitch, LongSwitch: RawUtf8): boolean;
var
  i, j, n: PtrInt;
  swlen: TByteDynArray;
  s: RawUtf8;
begin
  result := false;
  fLineFeed := DescriptionLineFeed;
  if (ShortSwitch = '') or
     (LongSwitch  = '') then
    exit;
  fSwitch[false] := ShortSwitch;
  fSwitch[true]  := LongSwitch;
  if fRawParams = nil then
  begin
    n := ParamCount;
    if n <= 0 then
      exit; // may equal -1 e.g. from a .so on MacOS
    SetLength(fRawParams, n);
    for i := 0 to n - 1 do
      fRawParams[i] := RawUtf8(ParamStr(i + 1));
  end;
  Finalize(fNames);
  Finalize(fValues);
  Finalize(fRetrieved);
  n := length(fRawParams);
  if n = 0 then
  begin
    result := true;
    exit;
  end;
  SetLength(swlen, n);
  for i := 0 to n - 1 do
  begin
    s := fRawParams[i];
    if s <> '' then
      if CompareMemSmall(pointer(s), pointer(LongSwitch), length(LongSwitch)) then
        swlen[i] := length(LongSwitch)  // start with LongSwitch
      else if CompareMemSmall(pointer(s), pointer(ShortSwitch), length(ShortSwitch)) then
        swlen[i] := length(ShortSwitch) // start with ShortSwitch
      {$ifdef OSWINDOWS}
      else
        while s[swlen[i] + 1] = '-' do
          inc(swlen[i]); // allow -v --verbose on Windows for cross-platform run
      {$endif OSWINDOWS}
  end;
  i := 0;
  repeat
    s := fRawParams[i];
    if s <> '' then
      if swlen[i] <> 0 then
      begin
        delete(s, 1, swlen[i]);
        if s <> '' then
        begin
          j := PosExChar('=', s);
          if j <> 1 then
            if j <> 0 then
            begin
              _AddRawUtf8(fNames[clkParam], copy(s, 1, j - 1));
              _AddRawUtf8(fValues, copy(s, j + 1, MaxInt));
            end
            else if (i + 1 = n) or
                    (swlen[i + 1] <> 0) then
              _AddRawUtf8(fNames[clkOption], s)
            else
            begin
              _AddRawUtf8(fNames[clkParam], s);
              inc(i);
              _AddRawUtf8(fValues, fRawParams[i]);
            end;
          end;
      end
      else
        _AddRawUtf8(fNames[clkArg], s);
    inc(i);
  until i = n;
  SetLength(fRetrieved[clkArg],    length(fNames[clkArg]));
  SetLength(fRetrieved[clkOption], length(fNames[clkOption]));
  SetLength(fRetrieved[clkParam],  length(fNames[clkParam]));
  result := true;
end;

var
  _SystemPath: array[TSystemPath] of TFileName; // GetSystemPath() cache

function GetSystemPath(kind: TSystemPath): TFileName;
begin
  result := _SystemPath[kind];
  if result <> '' then
    exit;
  _ComputeSystemPath(kind, result); // in os.posix.inc or os.windows.inc
  _SystemPath[kind] := result;
end;

function SetSystemPath(kind: TSystemPath; const path: TFileName): boolean;
var
  full: TFileName;
begin
  full := ExpandFileName(ExcludeTrailingPathDelimiter(path));
  result := DirectoryExists(full);
  if result then
    _SystemPath[kind] := IncludeTrailingPathDelimiter(full);
end;

function _GetExecutableLocation(aAddress: pointer): ShortString;
begin // return the address as hexadecimal - hexstr() is not available on Delphi
  result[0] := #0;
  AppendShortIntHex(PtrUInt(aAddress), result);
end; // mormot.core.log.pas will properly decode debug info - and handle .mab

var
  _SystemStoreAsPemSafe: TLightLock;
  _OneSystemStoreAsPem: array[TSystemCertificateStore] of record
    Tix: cardinal;
    Pem: RawUtf8;
  end;
  _SystemStoreAsPem: record
    Tix: cardinal;
    Scope: TSystemCertificateStores;
    Pem: RawUtf8;
  end;

function GetOneSystemStoreAsPem(CertStore: TSystemCertificateStore;
  FlushCache: boolean; now: cardinal): RawUtf8;
begin
  if now = 0 then
    now := GetTickCount64 shr 18 + 1; // div 262.144 seconds = every 4.4 min
  _SystemStoreAsPemSafe.Lock;
  try
    // first search if not already in cache
    with _OneSystemStoreAsPem[CertStore] do
    begin
      if not FlushCache then
        if Tix = now then
        begin
          result := Pem; // quick retrieved from cache
          exit;
        end;
      // fallback search depending on the POSIX / Windows specific OS
      result := _GetSystemStoreAsPem(CertStore); // implemented in each .inc
      Tix := now;
      Pem := result;
    end;
  finally
    _SystemStoreAsPemSafe.UnLock;
  end;
end;

function GetSystemStoreAsPem(CertStores: TSystemCertificateStores;
  FlushCache, OnlySystemStore: boolean): RawUtf8;
var
  now: cardinal;
  s: TSystemCertificateStore;
  v: RawUtf8;
begin
  result := '';
  now := GetTickCount64 shr 18 + 1;
  _SystemStoreAsPemSafe.Lock;
  try
    // first search if not already in cache
    if not FlushCache then
      with _SystemStoreAsPem do
        if (Tix = now) and
           (Scope = CertStores) and
           (Pem <> '') then
        begin
          result := Pem; // quick retrieved from cache
          exit;
        end;
    // load from a file, bounded within the application or from env variable
    if not OnlySystemStore then
    begin
      if GetSystemStoreAsPemLocalFile <> '' then
        {$ifdef OSPOSIX}
        if GetSystemStoreAsPemLocalFile[1] = '/' then // full /posix/path
        {$else}
        if GetSystemStoreAsPemLocalFile[2] = ':' then // 'C:\path\to\file.pem'
        {$endif OSPOSIX}
          result := StringFromFile(GetSystemStoreAsPemLocalFile)
        else
          result := StringFromFile(
            Executable.ProgramFilePath + GetSystemStoreAsPemLocalFile);
      if result = '' then
        result := StringFromFile(GetEnvironmentVariable('SSL_CA_CERT_FILE'));
    end;
  finally
    _SystemStoreAsPemSafe.UnLock; // GetOneSystemStoreAsPem() blocks
  end;
  // fallback to search depending on the POSIX / Windows specific OS stores
  if result = '' then
    for s := low(s) to high(s) do
      if s in CertStores then
      begin
        v := GetOneSystemStoreAsPem(s, FlushCache, now); // may use its cache
        if v <> '' then
          result := Join([result, v, #13#10]);
      end;
  if result = '' then
    exit;
  _SystemStoreAsPemSafe.Lock;
  try
    with _SystemStoreAsPem do
    begin
      Tix := now;
      Scope := CertStores;
      Pem := result;
    end;
  finally
    _SystemStoreAsPemSafe.UnLock;
  end;
end;

// SMBIOS can be available outside of Intel/AMD - e.g. on aarch64-win64

// from DSP0134 3.6.0 System Management BIOS (SMBIOS) Reference Specification
const
  SMB_ANCHOR  = $5f4d535f;  // _SM_
  SMB_INT4    = $494d445f;  // _DMI
  SMB_INT5    = $5f;        // _
  SMB_ANCHOR4 = $334d535f;  // _SM3
  SMB_ANCHOR5 = $5f;        // _

type
  TSmbEntryPoint32 = packed record
    Anchor: cardinal;  // = SMB_ANCHOR
    Checksum: byte;
    Length: byte;
    MajVers: byte;
    MinVers: byte;
    MaxSize: word;
    Revision: byte;
    PadTo16: array[1 .. 5] of byte;
    IntAnch4: cardinal; // = SMB_INT4
    IntAnch5: byte;     // = SMB_INT5
    IntChecksum: byte;
    StructLength: word;
    StructAddr: cardinal;
    NumStruct: word;
    BcdRevision: byte;
  end;
  PSmbEntryPoint32 = ^TSmbEntryPoint32;

  TSmbEntryPoint64 = packed record
    Anch4: cardinal; // = SMB_ANCHOR4
    Anch5: byte;     // = SMB_ANCHOR5
    Checksum: byte;
    Length: byte;
    MajVers: byte;
    MinVers: byte;
    DocRev: byte;
    Revision: byte;
    Reserved: byte;
    StructMaxLength: cardinal;
    StructAddr: QWord;
  end;
  PSmbEntryPoint64 = ^TSmbEntryPoint64;

function SmbiosChecksum(p: pointer; l: PtrInt): PtrUInt;
var
  cs: byte;
  i: PtrInt;
begin
  cs := 0;
  for i := 0 to l - 1 do
    inc(cs, PByteArray(p)[i]);
  result := ord(cs = 0); // returns 0 if failed
end;

function GetRawSmbios32(p: PSmbEntryPoint32; var info: TRawSmbiosInfo): PtrUInt;
begin
  result := SmbiosChecksum(p, p^.Length);
  if result = 0 then
    exit;
  result := p^.StructAddr;
  info.SmbMajorVersion := p^.MajVers;
  info.SmbMinorVersion := p^.MinVers;
  info.DmiRevision := p^.Revision; // 0 = SMBIOS 2.1
  info.Length := p^.StructLength;
end;

function GetRawSmbios64(p: PSmbEntryPoint64; var info: TRawSmbiosInfo): PtrUInt;
begin
  result := SmbiosChecksum(p, p^.Length);
  if result = 0 then
    exit;
  result := p^.StructAddr;
  info.SmbMajorVersion := p^.MajVers;
  info.SmbMinorVersion := p^.MinVers;
  info.DmiRevision := p^.Revision; // 1 = SMBIOS 3.0
  info.Length := p^.StructMaxLength;
end;

// caller should then try to decode SMB from pointer(result) + info.Len
function SearchSmbios(const mem: RawByteString; var info: TRawSmbiosInfo): PtrUInt;
var
  p, pend: PSmbEntryPoint32;
begin
  result := 0;
  if mem = '' then
    exit;
  p := pointer(mem);
  pend := @PByteArray(mem)[length(mem) - SizeOf(p^)];
  repeat
    if (p^.Anchor = SMB_ANCHOR) and
       (p^.IntAnch4 = SMB_INT4) and
       (p^.IntAnch5 = SMB_INT5) then
    begin
      result := GetRawSmbios32(p, info);
      if result <> 0 then
        exit;
    end
    else if (p^.Anchor = SMB_ANCHOR4) and
            (PSmbEntryPoint64(p)^.Anch5 = SMB_ANCHOR5) then
    begin
      result := GetRawSmbios64(pointer(p), info);
      if result <> 0 then
        exit; // here info.Length = max length
    end;
    inc(PHash128(p)); // search on 16-byte (paragraph) boundaries
  until PtrUInt(p) >= PtrUInt(pend);
end;

procedure ComputeGetSmbios;
begin
  GlobalLock; // thread-safe retrieval
  try
    if not _SmbiosRetrieved then
    begin
      _SmbiosRetrieved := true;
      Finalize(RawSmbios.Data);
      FillCharFast(RawSmbios, SizeOf(RawSmbios), 0);
      if _GetRawSmbios(RawSmbios) then // OS specific call
         if DecodeSmbios(RawSmbios, _Smbios) <> 0 then
         begin
           // we were able to retrieve and decode SMBIOS information
           {$ifdef OSPOSIX}
           _AfterDecodeSmbios(RawSmbios); // persist in SMB_CACHE for non-root
           {$endif OSPOSIX}
           DefaultHasher128(@SystemEntropy.LiveFeed, pointer(RawSmbios.Data),
             MinPtrInt(512, length(RawSmbios.Data))); // won't hurt
           exit;
         end;
      // if not root on POSIX, SMBIOS is not available
      // -> try to get what the OS exposes (Linux, MacOS or FreeBSD)
      DirectSmbiosInfo(_Smbios);
    end;
  finally
    GlobalUnLock;
  end;
end;

function GetRawSmbios: boolean;
begin
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // fill both RawSmbios and _Smbios[]
  result := RawSmbios.Data <> '';
end;

function GetSmbios(info: TSmbiosBasicInfo): RawUtf8;
begin
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // fill both RawSmbios and _Smbios[]
  result := _Smbios[info];
end;

procedure GetComputerUuid(out uuid: TGuid; disable: TGetComputerUuid);
var
  n, i: PtrInt;
  u: THash128Rec absolute uuid;
  s: RawByteString;
  fn: TFileName;
  mac: TRawUtf8DynArray;

  procedure crctext(const s: RawUtf8);
  begin
    if s = '' then
      exit;
    u.c[n] := crc32c(u.c[n], pointer(s), length(s));
    n := (n + 1) and 3; // update only 32-bit of UUID per crctext() call
  end;

begin
  // first try to retrieve the Machine BIOS UUID
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // maybe from local SMB_CACHE file for non-root
  if not (gcuSmbios in disable) and
     (_Smbios[sbiUuid] <> '') and
     TextToUuid(_Smbios[sbiUuid], uuid) then
    exit;
  // did we already compute (and persist) this UUID?
  if disable = [] then // we persist a fully-qualified UUID only
  begin
    fn := UUID_CACHE;
    s := StringFromFile(fn);
    if length(s) = SizeOf(uuid) then
    begin
      uuid := PGuid(s)^; // seems to be a valid UUID binary blob
      exit;
    end;
  end;
  // no known UUID: compute and store a 128-bit hash from HW specs
  // which should remain identical even between full OS reinstalls
  // note: /etc/machine-id is no viable alternative since it is from SW random
  s := CPU_ARCH_TEXT;
  crc128c(pointer(s), length(s), u.b); // rough starting point
  {$ifdef CPUINTELARM}
  if not (gcuCpuFeatures in disable) then
    crc128c(@CpuFeatures, SizeOf(CpuFeatures), u.b); // override
  {$endif CPUINTELARM}
  if (RawSmbios.Data <> '') and // some bios have no uuid but some HW info
     not (gcuSmbiosData in disable) then
    crc32c128(@u.b, pointer(RawSmbios.Data), length(RawSmbios.Data));
  n := 0;
  if not (gcuSmbiosText in disable) then
    for i := 0 to length(_Smbios) - 1 do // some of _Smbios[] may be set
      crctext(PRawUtf8Array(@_Smbios)[i]);
  if not (gcuCpuInfoText in disable) then
    crctext(CpuCacheText);
  if not (gcuBiosInfoText in disable) then
    crctext(BiosInfoText);
  if not (gcuCpuInfoText in disable) then
    crctext(CpuInfoText);
  if Assigned(GetSystemMacAddress) and
     not (gcuMacAddress in disable) then
    // from mormot.net.sock or mormot.core.os.posix.inc for Linux only
    mac := GetSystemMacAddress;
  if mac <> nil then
    // MAC should make it unique at least over the local network
    for i := 0 to high(mac) do
      crctext(mac[i])
  else
  begin
    // unpersisted fallback if mormot.net.sock is not included (very unlikely)
    crctext(Executable.Host);
    exit; // unpersisted
  end;
  if fn <> '' then // disable = []
    // we have enough unique HW information to store it locally for next startup
    // note: RawSmbios.Data may not be genuine e.g. between VMs
    if FileFromBuffer(@u, SizeOf(u), fn) then
      FileSetSticky(fn); // use S_ISVTX so that file is not removed from /var/tmp
end;

var
  _GetComputerUuid: RawUtf8;

function GetComputerUuid(disable: TGetComputerUuid): RawUtf8;
var
  u: TGuid;
begin
  if disable = [] then
  begin
    result := _GetComputerUuid; // try from cache
    if result <> '' then
      exit;
  end;
  GetComputerUuid(u, disable);
  UuidToText(u, result);
  if disable <> [] then
    exit; // cache fully-qualified UUID only
  GlobalLock;
  _GetComputerUuid := result;
  GlobalUnLock;
end;

procedure DecodeSmbiosUuid(src: PGuid; out dest: RawUtf8; const raw: TRawSmbiosInfo);
var
  uid: TGuid;
begin
  uid := src^;
  // reject full $00 = unsupported or full $ff = not set
  if IsZero(@uid, SizeOf(uid)) or
     ((PCardinalArray(@uid)[0] = $ffffffff) and
      (PCardinalArray(@uid)[1] = $ffffffff) and
      (PCardinalArray(@uid)[2] = $ffffffff) and
      (PCardinalArray(@uid)[3] = $ffffffff)) then
    exit;
  // GUIDToString() already displays the first 4 bytes as little-endian
  // - we don't need to swap those bytes as dmi_system_uuid() in dmidecode.c
  // on Windows, to match "wmic csproduct get uuid" official value
  // - on MacOs, sduInvert is set to match IOPlatformUUID value from ioreg :(
  if (_SmbiosDecodeUuid = sduInvert) or
  // - dmi_save_uuid() from the Linux kernel do check for SMBIOS 2.6 version
  // https://elixir.bootlin.com/linux/latest/source/drivers/firmware/dmi_scan.c
     ((_SmbiosDecodeUuid = sduVersion) and
      (raw.SmbMajorVersion shl 8 + raw.SmbMinorVersion < $0206)) then
  begin
    uid.D1 := bswap32(uid.D1);
    uid.D2 := bswap16(uid.D2);
    uid.D3 := bswap16(uid.D3);
  end;
  UuidToText(uid, dest);
end;

function DecodeSmbios(var raw: TRawSmbiosInfo; out info: TSmbiosBasicInfos): PtrInt;
var
  lines: array[byte] of TSmbiosBasicInfo; // single pass efficient decoding
  len, trimright: PtrInt;
  cur: ^TSmbiosBasicInfo;
  s, sEnd: PByteArray;
begin
  result := 0;
  Finalize(info);
  s := pointer(raw.Data);
  if s = nil then
    exit;
  sEnd := @s[length(raw.Data)];
  FillCharFast(lines, SizeOf(lines), ord(sbiUndefined));
  repeat
    if (s[0] = 127) or // type (127=EOT)
       (s[1] < 4) or   // length
       (PtrUInt(@s[s[1]]) > PtrUInt(sEnd)) then
    begin
      s := @s[2]; // truncate to the exact end of DMI/SMBIOS input
      break;
    end;
    case s[0] of
      0: // Bios Information (type 0)
        begin
          lines[s[4]] := sbiBiosVendor;
          lines[s[5]] := sbiBiosVersion;
          lines[s[8]] := sbiBiosDate;
          if s[1] >= $17 then // 2.4+
          begin
            _fmt('%d.%d', [s[$14], s[$15]], info[sbiBiosRelease]);
            _fmt('%d.%d', [s[$16], s[$17]], info[sbiBiosFirmware]);
          end;
        end;
      1: // System Information (type 1)
        begin
          lines[s[4]] := sbiManufacturer;
          lines[s[5]] := sbiProductName;
          lines[s[6]] := sbiVersion;
          lines[s[7]] := sbiSerial;
          if s[1] >= $18 then // 2.1+
          begin
            DecodeSmbiosUuid(@s[8], info[sbiUuid], raw);
            if s[1] >= $1a then // 2.4+
            begin
              lines[s[$19]] := sbiSku;
              lines[s[$1a]] := sbiFamily;
            end;
          end;
        end;
      2: // Baseboard (or Module) Information (type 2) - keep only the first
        begin
          lines[s[4]] := sbiBoardManufacturer;
          lines[s[5]] := sbiBoardProductName;
          lines[s[6]] := sbiBoardVersion;
          lines[s[7]] := sbiBoardSerial;
          lines[s[8]] := sbiBoardAssetTag;
          lines[s[10]] := sbiBoardLocation;
        end;
      4: // Processor Information (type 4) - keep only the first
        begin
          lines[s[7]] := sbiCpuManufacturer;
          lines[s[$10]] := sbiCpuVersion;
          if s[1] >= $22 then // 2.3+
          begin
            lines[s[$20]] := sbiCpuSerial;
            lines[s[$21]] := sbiCpuAssetTag;
            lines[s[$22]] := sbiCpuPartNumber;
          end;
        end;
      11: // OEM Strings (Type 11) - keep only the first
        if s[4] <> 0 then
          lines[1] := sbiOem; // e.g. 'vboxVer_6.1.36'
      22: // Portable Battery (type 22) - keep only the first
        if s[1] >= $0f then // 2.1+
        begin
          lines[s[4]] := sbiBatteryLocation;
          lines[s[5]] := sbiBatteryManufacturer;
          lines[s[8]] := sbiBatteryName;
          lines[s[$0e]] := sbiBatteryVersion;
          if s[1] >= $14 then // 2.2+
            lines[s[$14]] := sbiBatteryChemistry;
        end;
    end;
    s := @s[s[1]]; // go to string table
    cur := @lines[1];
    if s[0] = 0 then
      inc(PByte(s)) // no string table
    else
      repeat
        len := StrLen(s);
        if cur^ <> sbiUndefined then
        begin
          if info[cur^] = '' then // only set the first occurrence if multiple
          begin
            trimright := len;
            while (trimright <> 0) and
                  (s[trimright - 1] <= ord(' ')) do
              dec(trimright);
            FastSetString(info[cur^], s, trimright);
          end;
          cur^ := sbiUndefined; // reset slot in lines[]
        end;
        s := @s[len + 1]; // next string
        inc(cur);
      until s[0] = 0; // end of string table
    inc(PByte(s)); // go to next structure
  until false;
  // compute the exact DMI/SMBIOS size, and adjust the raw.Data length
  result := PtrUInt(s) - PtrUInt(raw.Data);
  raw.Length := result;
  if length(raw.Data) <> result then
    FakeSetLength(raw.Data, result);
end;


{ **************** TSynLocker Threading Features }

const
  // default value for all spining, up to 993 "pause" opcode calls
  // - on Intel, taking around 5us on old CPU, but modern Intel have bigger pause
  // latency (up to 100 cycles) so takes up to 50us
  // - AMD Zen 3 and later has a latency of only 1-2 cycles so we identify them
  // via CPUID and adjust a SpinFactor global variable at startup to reach 5us
  // - 5..50us range seems consistent with our eventual nanosleep(10us) syscall
  SPIN_COUNT = pred(6 shl 5); // = 191

// as reference, take a look at Linus insight (TL&WR: better use futex)
// from https://www.realworldtech.com/forum/?threadid=189711&curpostid=189755

// our light locks do not use the resource of an associated futex, so are easier
// if there is almost no contention - and really seldom call fpnanosleep(10us)

{$ifdef CPUINTEL}
var
  SpinFactor: PtrUInt = 1; // default value on Intel - set to 10 on AMD Zen3+

// on Intel/AMD, the pause CPU instruction would relax the core
// - but it is expected to be inlined within the spinning loop itself
// - sadly, Delphi does not support inlined asm on Win64 so we use a function
{$ifdef WIN64DELPHI}
procedure DoPause(n: PtrUInt);
asm
@s:   pause          // = "rep nop" opcode
      dec     rcx
      jnz     @s     // within its own 1..16x loop (better than nothing)
end;
{$endif WIN64DELPHI}
{$endif CPUINTEL}

{$ifdef FPC_CPUARM}
const
  SpinFactor = 2; // ARM yield has smaller latency than Intel's pause

// "yield" is available since ARMv6K architecture, including ARMv7-A and ARMv8-A
// - but our FPC arm32 asm seems not knowledgable of this
procedure DoPause; assembler; nostackframe;
asm
     {$ifdef CPUARMYIELD}
     yield // a few cycles, but helps modern CPU adjust their power requirements
     {$endif CPUARMYIELD}
end;
{$endif FPC_CPUARM}

function DoSpin(spin: PtrUInt): PtrUInt;
begin
  {$ifdef CPUINTELARM}
  // adaptive spinning to reduce cache coherence traffic
  result := (SPIN_COUNT - spin) shr 5; // 0..5 range, each 32 times
  if result <> 0 then // no pause up to 32 times (low latency acquisition)
  {$ifdef OSLINUX_SCHEDYIELDONCE}      // yield once during the process
  {$ifndef OSLINUX_SCHEDYIELD}         // if not already = SwitchToThread
  if spin = SPIN_COUNT shr 2 then
    Do_SysCall(syscall_nr_sched_yield) // properly defined in syscall.pp
  else
  {$endif OSLINUX_SCHEDYIELD}
  {$endif OSLINUX_SCHEDYIELDONCE}
  begin // exponential backoff: 1,2,4,8,16 x DoPause
    result := SpinFactor shl pred(result);
    // "pause" called 992 times until SwithToThread = up to 50us on modern CPU
    {$ifdef WIN64DELPHI}
    DoPause(result);
    {$else}
    repeat
      {$ifdef CPUINTEL}
      asm
        pause // "rep nop" opcode should be inlined within the spinning loop
      end;
      {$else}
      DoPause; // "yield" arm/aarch64 opcode
      {$endif CPUINTEL}
      dec(result);
    until result = 0;
    {$endif WIN64DELPHI}
  end;
  {$endif CPUINTELARM}
  dec(spin);
  if spin = 0 then // eventually call the OS for long wait
  begin
    SwitchToThread; // homonymous Win API call or proper POSIX call
    spin := SPIN_COUNT; // try again
  end;
  result := spin;
end;


{ TLightLock }

procedure TLightLock.Init;
begin
  Flags := 0;
end;

procedure TLightLock.Done;
begin // just for compatibility with TOSLock
end;

procedure TLightLock.Lock;
begin
  // we tried a dedicated asm but it was slower: inlining is preferred
  if not LockedExc(Flags, {to=}1, {from=}0) then
    LockSpin;
end;

procedure TLightLock.UnLock;
begin
  {$ifdef CPUINTEL}
  Flags := 0; // non reentrant locks need no additional thread safety
  {$else}
  LockedExc(Flags, {to=}0, {from=}1); // ARM can be weak-ordered
  // https://preshing.com/20121019/this-is-why-they-call-it-a-weakly-ordered-cpu
  {$endif CPUINTEL}
end;

function TLightLock.TryLock: boolean;
begin
  result := (Flags = 0) and // first check without any (slow) atomic opcode
            LockedExc(Flags, {to=}1, {from=}0);
end;

function TLightLock.IsLocked: boolean;
begin
  result := Flags <> 0;
end;

procedure TLightLock.LockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryLock;
end;


{ TMultiLightLock }

procedure TMultiLightLock.Init;
begin
  Flags := 0;
  ThreadID := TThreadID(0);
end;

procedure TMultiLightLock.Done;
begin
  Flags := PtrUInt(-1);
  ThreadID := TThreadID(0); // invalid combination to let TryLock fail
end;

procedure TMultiLightLock.Lock;
begin
  if not TryLock then
    LockSpin;
end;

procedure TMultiLightLock.UnLock;
begin
  if Flags = 1 then
    ThreadID := TThreadID(0); // paranoid
  LockedDec(Flags, 1);
end;

function TMultiLightLock.TryLock: boolean;
var
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  if Flags = 0 then    // is not locked
    if LockedExc(Flags, {to=}1, {from=}0) then // try atomic acquisition
    begin
      ThreadID := tid;
      result := true;  // acquired the lock
    end
    else
      result := false  // impossible to acquire this lock
  else if ThreadID <> tid then
    result := false // locked by another thread
  else
  begin
    inc(Flags);     // make it reentrant
    result := true; // locked by this thread
  end;
end;

procedure TMultiLightLock.ForceLock;
begin
  Flags := MaxInt; // forced acquisition, whatever the current state is
  ThreadID := GetCurrentThreadId;
end;

function TMultiLightLock.IsLocked: boolean;
begin
  result := Flags <> 0;
end;

procedure TMultiLightLock.LockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryLock;
end;


{ TRWLightLock }

procedure TRWLightLock.Init;
begin
  Flags := 0; // bit 0=WriteLock, >0=ReadLock counter
end;

procedure TRWLightLock.ReadLock;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock counter
  if not LockedExc(Flags, {to=}f + 2, {from=}f) then
    ReadLockSpin;
end;

function TRWLightLock.TryReadLock: boolean;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock counter
  result := LockedExc(Flags, {to=}f + 2, {from=}f);
end;

procedure TRWLightLock.ReadUnLock;
begin
  LockedDec(Flags, 2);
end;

procedure TRWLightLock.ReadLockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryReadLock;
end;

function TRWLightLock.TryWriteLock: boolean;
var
  f: PtrUInt;
begin
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock
  result := (Flags = f) and
            LockedExc(Flags, {to=}f + 1, {from=}f);
end;

procedure TRWLightLock.WriteLock;
begin
  if not TryWriteLock then
    WriteLockSpin;
end;

procedure TRWLightLock.WriteUnLock;
begin
  LockedDec(Flags, 1);
end;

procedure TRWLightLock.WriteLockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryWriteLock;
end;

function TRWLightLock.IsLocked: boolean;
begin
  result := (Flags <> 0);
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
    raise EOSException.CreateFmt('TRWLock Flags=%x', [Flags])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;

// dedicated asm for this most simple (and used) method
{$ifdef ASMX64}

procedure TRWLock.ReadOnlyLock;
// stack frame is required (at least on Windows) since it may call SwitchToThread
var
  backup: pointer; // better than push/pop since we have a stack frame
asm
        {$ifdef SYSVABI}
        mov     rcx, rdi      // rcx = self
        {$endif SYSVABI}
@retry: mov     r8d, SPIN_COUNT
@spin:  mov     rax, qword ptr [rcx + TRWLock.Flags]
        and     rax, not 1
        lea     rdx, [rax + 4]
   lock cmpxchg qword ptr [rcx + TRWLock.Flags], rdx
        jz      @done
        pause
        dec     r8d
        jnz     @spin
        mov     qword ptr [backup], rcx
        call    SwitchToThread
        mov     rcx, qword ptr [backup] // restore for the wait loop
        jmp     @retry
@done:  // restore the stack frame
end;

{$else}

procedure TRWLock.ReadOnlyLock;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
  if not LockedExc(Flags, {to=}f + 4, {from=}f) then
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
  until (Flags = f) and
        LockedExc(Flags, {to=}f + 4, {from=}f);
end;

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
    if (Flags = f) and
       LockedExc(Flags, {to=}f + 2, {from=}f) then
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
    if (Flags = f) and
       LockedExc(Flags, {to=}f + 1, {from=}f) then
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
    dec(LastWriteLockCount); // reentrant call
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

function TRWLock.IsLocked: boolean;
begin
  result := (Flags <> 0);
end;


{ TOSLock }

procedure TOSLock.Init;
begin
  mormot.core.os.InitializeCriticalSection(CS);
end;

procedure TOSLock.Done;
begin
  DeleteCriticalSectionIfNeeded(CS);
end;

procedure TOSLock.Lock;
begin
  mormot.core.os.EnterCriticalSection(CS);
end;

function TOSLock.TryLock: boolean;
begin
  result := mormot.core.os.TryEnterCriticalSection(CS) <> 0;
end;

procedure TOSLock.UnLock;
begin
  mormot.core.os.LeaveCriticalSection(CS);
end;


{ TLockedList }

procedure TLockedList.Init(onesize: PtrUInt; const onefree: TOnLockedListOne);
begin
  FillCharFast(self, SizeOf(self), 0);
  Size := onesize;
  fOnFree := onefree;
  fSequence := (Random32 shr 2) + 65536; // 65535 < sequence < MaxInt
end;

function LockedListFreeAll(o: PLockedListOne; const OnFree: TOnLockedListOne): integer;
var
  next: PLockedListOne;
begin
  result := 0;
  while o <> nil do
  begin
    inc(result);
    next := o.next;
    if Assigned(OnFree) then
      OnFree(o);
    FreeMem(o);
    o := next;
  end;
end;

procedure TLockedList.Done;
begin
  Clear;
  EmptyBin;
end;

procedure TLockedList.Clear;
begin
  Safe.Lock;
  try
    LockedListFreeAll(fHead, fOnFree);
    fHead := nil;
    Count := 0;
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.EmptyBin: integer;
begin
  Safe.Lock;
  try
    result := LockedListFreeAll(fBin, nil);
    fBin := nil;
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.New: pointer;
begin
  Safe.Lock;
  try
    // try to recycle from single-linked list bin, or allocate
    result := fBin;
    if result <> nil then
      fBin := PLockedListOne(result).next
    else
      result := AllocMem(Size);
    PLockedListOne(result).sequence := fSequence;
    inc(fSequence); // protected by Safe.Lock
    // insert at beginning of the main double-linked list
    PLockedListOne(result).next := fHead;
    if fHead <> nil then
      PLockedListOne(fHead).prev := result;
    fHead := result;
    inc(Count);
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.Free(one: pointer): boolean;
var
  o: PLockedListOne absolute one;
begin
  result := false;
  if (o = nil) or
     (o^.sequence = 0) then
    exit;
  Safe.Lock;
  try
    // remove from main double-linked list
    if o = fHead then
      fHead := o.next;
    if o.next <> nil then
      PLockedListOne(o.next).prev := o.prev;
    if o.prev <> nil then
      PLockedListOne(o.prev).next := o.next;
    // release internals and add to the recycle bin
    if Assigned(fOnFree) then
      fOnFree(o);
    FillCharFast(o^, Size, 0); // garbage collect as void
    o.next := fBin;
    fBin := o;
    dec(Count);
  finally
    Safe.UnLock;
  end;
  result := true;
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

procedure TSynLocker.InitFromClass;
begin
  fSection.Init;
  fInitialized := true;
end;

function NewSynLocker: PSynLocker;
begin
  result := AllocMem(SizeOf(TSynLocker));
  result^.InitFromClass;
end;

procedure TSynLocker.Init;
begin
  fLockCount := 0;
  fPaddingUsedCount := 0;
  fRW.Init;
  fSection.Init;
  fInitialized := true;
end;

procedure TSynLocker.Done;
var
  i: PtrInt;
  v: PSynVarData;
begin
  v := @Padding[0];
  for i := 1 to fPaddingUsedCount do
  begin
    if (v^.VType and VTYPE_STATIC) <> 0 then
      VarClearProc(v^.Data); // won't include varAny = SetPointer
    inc(v);
  end;
  fSection.Done;
  fInitialized := false;
end;

procedure TSynLocker.DoneAndFreeMem;
begin
  Done;
  FreeMem(@self);
end;

function TSynLocker.GetIsLocked: boolean;
begin
  case fRWUse of
    uSharedLock:
      result := fLockCount <> 0; // only updated by uSharedLock
    uRWLock:
      result := fRW.Flags = 0;   // no lock at all
  else
    result := false;             // uNoLock will never lock
  end;
end;

procedure TSynLocker.RWLock(context: TRWLockContext);
begin
  case fRWUse of
    uSharedLock:
      begin
        fSection.Lock;
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
        fSection.UnLock;
      end;
    uRWLock:
      fRW.UnLock(context);
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.ReadLock;
begin
  RWLock(cReadOnly); // will be properly inlined
end;

procedure TSynLocker.ReadUnLock;
begin
  RWUnLock(cReadOnly);
end;

procedure TSynLocker.ReadWriteLock;
begin
  RWLock(cReadWrite);
end;

procedure TSynLocker.ReadWriteUnLock;
begin
  RWUnLock(cReadWrite);
end;

procedure TSynLocker.Lock;
begin
  RWLock(cWrite);
end;

procedure TSynLocker.UnLock;
begin
  RWUnLock(cWrite);
end;

function TSynLocker.TryLock: boolean;
begin
  result := (fRWUse = uSharedLock) and
            fSection.TryLock;
  if result then
    inc(fLockCount);
end;

function TSynLocker.TryLockMS(retryms: integer; terminated: PBoolean;
  tix64: Int64): boolean;
var
  ms: integer;
begin
  result := TryLock;
  if result or
     (fRWUse <> uSharedLock) or
     (retryms <= 0) then
    exit;
  ms := 0;
  if tix64 = 0 then
    tix64 := GetTickCount64;
  inc(tix64, retryms);
  repeat
    SleepHiRes(ms);
    result := TryLock;
    if result or
       ((terminated <> nil) and
        terminated^) then
      exit;
    ms := ms xor 1; // 0,1,0,1... seems to be good for scaling
  until GetTickCount64 > tix64;
end;

function TSynLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@self);
end;

function TSynLocker.GetVariant(Index: integer): Variant;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    result := variant(Padding[Index]); // safe copy
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
    if Index >= fPaddingUsedCount then
      fPaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.GetInt64(Index: integer): Int64;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
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
  result := false;
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
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
  end;
end;

procedure TSynLocker.SetBool(Index: integer; const Value: boolean);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetUnlockedInt64(Index: integer): Int64;
begin
  if (cardinal(Index) >= cardinal(fPaddingUsedCount)) or
     not VariantToInt64(variant(Padding[Index]), result) then
    result := 0;
end;

procedure TSynLocker.SetUnlockedInt64(Index: integer; const Value: Int64);
begin
  if cardinal(Index) >= high(Padding) then
    exit;
  if Index >= fPaddingUsedCount then
    fPaddingUsedCount := Index + 1;
  variant(Padding[Index]) := Value;
end;

function TSynLocker.GetPointer(Index: integer): pointer;
begin
  result := nil;
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    with Padding[Index].Data do
      if cardinal(VType) = varAny then
        result := VAny;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end;
end;

procedure TSynLocker.SetPointer(Index: integer; const Value: pointer);
begin
  if cardinal(Index) <= high(Padding) then
    try
      RWLock(cWrite);
      if Index >= fPaddingUsedCount then
        fPaddingUsedCount := Index + 1;
      with Padding[Index] do
      begin
        VarClearAndSetType(variant(Data), varAny);
        VAny := Value;
      end;
    finally
      RWUnLock(cWrite);
    end;
end;

function TSynLocker.GetUtf8(Index: integer): RawUtf8;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
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
      if Index >= fPaddingUsedCount then
        fPaddingUsedCount := Index + 1;
      RawUtf8ToVariant(Value, variant(Padding[Index]));
    finally
      RWUnLock(cWrite);
    end;
end;

function TSynLocker.LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
begin
  result := 0;
  if cardinal(Index) <= high(Padding) then
    try
      RWLock(cWrite);
      with Padding[Index] do
      begin
        if Index < fPaddingUsedCount then
          VariantToInt64(variant(Data), result)
        else
          fPaddingUsedCount := Index + 1;
        variant(Data) := Int64(result + Increment);
      end;
    finally
      RWUnLock(cWrite);
    end;
end;

function TSynLocker.LockedExchange(Index: integer; const Value: variant): variant;
begin
  VarClear(result);
  if cardinal(Index) <= high(Padding) then
    try
      RWLock(cWrite);
      with Padding[Index] do
      begin
        if Index < fPaddingUsedCount then
          result := variant(Data)
        else
          fPaddingUsedCount := Index + 1;
        variant(Data) := Value;
      end;
    finally
      RWUnLock(cWrite);
    end;
end;

function TSynLocker.LockedPointerExchange(Index: integer; Value: pointer): pointer;
begin
  result := nil;
  if cardinal(Index) <= high(Padding) then
    try
      RWLock(cWrite);
      with Padding[Index] do
      begin
        if Index < fPaddingUsedCount then
          if cardinal(VType) = varAny then
            result := VAny
          else
            VarClearProc(Data)
        else
          fPaddingUsedCount := Index + 1;
        VType := varAny;
        VAny := Value;
      end;
    finally
      RWUnLock(cWrite);
    end;
end;


{ TSynLocked }

constructor TSynLocked.Create;
begin
  fSafe := NewSynLocker;
end;

destructor TSynLocked.Destroy;
begin
  fSafe^.DoneAndFreeMem;
end;

procedure TSynLocked.Lock;
begin
  if self <> nil then
    fSafe^.Lock;
end;

procedure TSynLocked.Unlock;
begin
  if self <> nil then
    fSafe^.UnLock;
end;


{ TObjectOSLock }

constructor TObjectOSLock.Create;
begin
  fSafe.Init;
end;

destructor TObjectOSLock.Destroy;
begin
  fSafe.Done;
end;

procedure TObjectOSLock.Lock;
begin
  if self <> nil then
    fSafe.Lock;
end;

procedure TObjectOSLock.Unlock;
begin
  if self <> nil then
    fSafe.UnLock;
end;


{ TObjectOSLightLock }

constructor TObjectOSLightLock.Create;
begin
  fSafe.Init;
end;

destructor TObjectOSLightLock.Destroy;
begin
  fSafe.Done;
end;

procedure TObjectOSLightLock.Lock;
begin
  if self <> nil then
    fSafe.Lock;
end;

procedure TObjectOSLightLock.Unlock;
begin
  if self <> nil then
    fSafe.UnLock;
end;


{ TSynEvent }

function TSynEvent.SleepStep(var start: Int64; terminated: PBoolean): Int64;
var
  ms: integer;
  endtix: Int64;
begin
  ms := SleepStepTime(start, result, @endtix);
  if (ms < 10) or
     (terminated = nil) then
    if ms = 0 then
      SleepHiRes(0) // < 16 ms is a pious wish on Windows anyway
    else
      WaitFor(ms)
  else
    repeat
      WaitFor(10);
      if terminated^ then
        exit;
      result := GetTickCount64;
    until result >= endtix;
end;

function TSynEvent.WaitForSafe(TimeoutMS: integer): boolean;
var
  endtix: Int64;
begin
  if GetCurrentThreadID = MainThreadID then
  begin
    endtix := GetTickCount64 + TimeoutMS;
    repeat
      CheckSynchronize(1); // make UI responsive enough
    until WaitFor(10) or
          (GetTickCount64 > endtix);
    result := fNotified;
  end
  else
    result := WaitFor(TimeoutMS);
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

function TLecuyerThreadSafe.NextQWord: QWord;
begin
  Safe.Lock;
  result := Generator.NextQWord;
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
  Fill(@dest, 32);
  AdjustShortStringFromRandom(@dest, 32);
end;

procedure TLecuyerThreadSafe.Seed(entropy: pointer; entropylen: PtrInt);
begin
  Safe.Lock;
  Generator.Seed(entropy, entropylen);
  Safe.UnLock;
end;

function Random32: cardinal;
begin
  result := SharedRandom.Next;
end;

function Random32Not0: cardinal;
begin
  repeat
    result := SharedRandom.Next;
  until result <> 0;
end;

function Random31: integer;
begin
  result := SharedRandom.Next shr 1;
end;

function Random31Not0: integer;
begin
  repeat
    result := SharedRandom.Next shr 1;
  until result <> 0;
end;

function Random32(max: cardinal): cardinal;
begin
  result := (QWord(SharedRandom.Next) * max) shr 32;
end;

function Random64: QWord;
begin
  result := SharedRandom.NextQWord;
end;

function RandomDouble: double;
begin
  result := SharedRandom.NextDouble;
end;

procedure RandomBytes(Dest: pointer; Count: integer);
begin
  if Count > 0 then
    SharedRandom.Fill(Dest, Count);
end;

function RandomByteString(Count: integer; var Dest; CodePage: cardinal): pointer;
begin
  FastSetStringCP(Dest, nil, Count, CodePage);
  SharedRandom.Fill(pointer(Dest), Count);
  result := pointer(Dest);
end;

procedure RandomShort31(var dest: TShort31);
begin
  SharedRandom.FillShort31(dest);
end;

function RandomGuid: TGuid;
begin
  RandomGuid(result);
end;

procedure MakeRandomGuid(u: PHash128);
begin // see https://datatracker.ietf.org/doc/html/rfc4122#section-4.4
  PCardinal(@u[6])^ := (PCardinal(@u[6])^ and $ff3f0fff) or $00804000;
  // u[7] := PtrUInt(u[7] and $0f) or $40; // version bits 12-15 = 4 (random)
  // u[8] := PtrUInt(u[8] and $3f) or $80; // reserved bits 6-7 = 1
end;

function IsRandomGuid(u: PHash128): boolean;
begin
  result := (u[7] and $f0 = $40) and (u[8] and $c0 = $80);
end;

procedure RandomGuid(out result: TGuid);
begin
  SharedRandom.Fill(@result, SizeOf(TGuid));
  MakeRandomGuid(@result);
end;

{$ifndef PUREMORMOT2}
procedure FillRandom(Dest: PCardinal; CardinalCount: integer);
begin
  if CardinalCount > 0 then
    SharedRandom.Fill(pointer(Dest), CardinalCount shl 2);
end;
{$endif PUREMORMOT2}

procedure Random32Seed(entropy: pointer; entropylen: PtrInt);
begin
  SharedRandom.Seed(entropy, entropylen);
end;


procedure GlobalLock;
begin
  GlobalCriticalSection.Lock;
end;

procedure GlobalUnLock;
begin
  GlobalCriticalSection.UnLock;
end;

var
  InternalGarbageCollection: record // RegisterGlobalShutdownRelease() list
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
           not PtrUIntScanExists(pointer(Instances), Count, PtrUInt(Instance)) then
          PtrArrayAdd(Instances, Instance, Count);
    finally
      GlobalUnLock;
    end;
  end;
  result := Instance;
end;

function SleepDelay(elapsed: PtrInt): PtrInt;
begin
  if elapsed < 50 then
    result := 0 // redirect to SwitchToThread
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
  if (start = 0) or
     (tix < 50) then
    start := tix
  else if start < 0 then
    start := tix - 50; // ensure tix - start = elapsed is not < 50
  result := SleepDelay(tix - start);
  if endtix <> nil then
    endtix^ := tix + result;
end;

function SleepStep(var start: Int64; terminated: PBoolean): Int64;
var
  ms: integer;
  endtix: Int64;
begin
  ms := SleepStepTime(start, result, @endtix);
  if (ms < 10) or
     (terminated = nil) then
    SleepHiRes(ms) // < 16 ms is a pious wish on Windows anyway
  else
    repeat
      SleepHiRes(10); // on Windows, HW clock resolution is around 16 ms
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

procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  while (Target <> Comperand) or
        not LockedExc(Target, {to=}NewValue, {from=}Comperand) do
    spin := DoSpin(spin);
end;

function ObjArrayAdd(var aObjArray; aItem: TObject;
  var aSafe: TLightLock; aCount: PInteger): PtrInt;
begin
  aSafe.Lock;
  if aCount <> nil then
    result := PtrArrayAdd(aObjArray, aItem, aCount^)
  else
    result := PtrArrayAdd(aObjArray, aItem);
  aSafe.UnLock;
end;

function PtrArrayDelete(var aPtrArray; aItem: pointer; var aSafe: TLightLock;
  aCount: PInteger): PtrInt;
begin
  if pointer(aPtrArray) = nil then
  begin
    result := -1; // no need to lock anything
    exit;
  end;
  aSafe.Lock;
  result := PtrArrayDelete(aPtrArray, aItem, aCount);
  aSafe.UnLock;
end;

function SetCpuSet(var CpuSet: TCpuSet; CpuIndex: cardinal): boolean;
begin
  result := false;
  if (CpuIndex >= SizeOf(CpuSet) shl 3) or
     (CpuIndex >= SystemInfo.dwNumberOfProcessors) then
    exit;
  SetBitPtr(@CpuSet, CpuIndex);
  result := true;
end;

function CurrentCpuSet(out CpuSet: TCpuSet): integer;
begin
  ResetCpuSet(CpuSet);
  if GetMaskAffinity(CpuSet) then
    result := GetBitsCount(CpuSet, SizeOf(CpuSet) shl 3)
  else
    result := 0;
end;

function SetThreadCpuAffinity(Thread: TThread; CpuIndex: cardinal): boolean;
var
  mask: TCpuSet;
begin
  ResetCpuSet(mask);
  result := SetCpuSet(mask, CpuIndex) and
            SetThreadMaskAffinity(Thread, mask);
end;

function SetThreadSocketAffinity(Thread: TThread; SocketIndex: cardinal): boolean;
begin
  result := (SocketIndex < cardinal(length(CpuSocketsMask))) and
            SetThreadMaskAffinity(Thread, CpuSocketsMask[SocketIndex]);
end;

procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUtf8;
  const Args: array of const);
begin
  // do nothing - properly implemented in mormot.core.log using FormatUtf8()
end;

procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const);
begin
  SetThreadName(GetCurrentThreadId, Format, Args);
end;

procedure SetCurrentThreadName(const Name: RawUtf8);
begin
  SetThreadName(GetCurrentThreadId, '%', [Name]);
end;

threadvar // do not publish for compilation within Delphi packages
  _CurrentThreadName: TShort31; // 31 chars is enough for our debug purpose

function CurrentThreadNameShort: PShortString;
begin
  result := @_CurrentThreadName;
  if result^[0] > #31 then
    result := @NULCHAR; // paranoid range check
end;

function _GetCurrentThreadName: RawUtf8;
begin
  ShortStringToAnsi7String(_CurrentThreadName, result);
end;

function GetCurrentThreadInfo: TShort63;
begin
  result := 'Thread ';
  AppendShortIntHex(PtrUInt(GetCurrentThreadId), result);
  AppendShortTwoChars(ord(' ') + ord('[') shl 8, @result);
  AppendShort(_CurrentThreadName, result);
  AppendShortChar(']', @result);
end;


{ ****************** Unix Daemon and Windows Service Support }

const
  // hardcoded to avoid linking mormot.core.rtti for GetEnumName()
  _SERVICESTATE: array[TServiceState] of string[12] = (
    'NotInstalled',
    'Stopped',
    'Starting',
    'Stopping',
    'Running',
    'Resuming',
    'Pausing',
    'Paused',
    'Failed',
    'Error');

function ToText(st: TServiceState): PShortString;
begin
  result := @_SERVICESTATE[st];
end;

function ExtractExecutableName(const cmd: RawUtf8; posix: boolean): RawUtf8;
var
  temp: RawUtf8;
  argv: TParseCommandsArgs;
  argc: integer;
begin
  if (pcInvalidCommand in ParseCommandArgs(cmd, @argv, @argc, @temp, posix)) or
     ({%H-}argc = 0) then
    result := ''
  else
    FastSetString(result, argv[0], StrLen(argv[0]));
end;

function ExtractCommandArgs(const cmd: RawUtf8; out param: TRawUtf8DynArray;
  posix: boolean): TParseCommands;
var
  temp: RawUtf8;
  argv: TParseCommandsArgs;
  argc: integer;
  i: PtrInt;
begin
  result := ParseCommandArgs(cmd, @argv, @argc, @temp, posix);
  if result * PARSECOMMAND_ERROR <> [] then
    exit; // failed
  SetLength(param, argc);
  for i := 0 to argc - 1 do
    FastSetString(param[i], argv[i], StrLen(argv[i]));
end;

function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs;
  argc: PInteger; temp: PRawUtf8; posix: boolean): TParseCommands;
var
  n: PtrInt;
  state: set of (sWhite, sInArg, sInSQ, sInDQ, sSpecial, sBslash);
  c: AnsiChar;
  d, p: PAnsiChar;
begin
  result := [pcInvalidCommand];
  if argv <> nil then
    argv[0] := nil;
  if argc <> nil then
    argc^ := 0;
  if cmd = '' then
    exit;
  if argv = nil then
    d := nil
  else
  begin
    if temp = nil then
      exit;
    SetLength(temp^, length(cmd));
    d := pointer(temp^);
  end;
  state := [];
  n := 0;
  p := pointer(cmd);
  repeat // parse the command line text, using a state machine in the loop
    c := p^;
    if d <> nil then
      d^ := c;
    inc(p);
    case c of
      #0:
        begin
          if sInSQ in state then
            include(result, pcUnbalancedSingleQuote);
          if sInDQ in state then
            include(result, pcUnbalancedDoubleQuote);
          exclude(result, pcInvalidCommand);
          if argv <> nil then
            if n <= high(argv^) then
              argv^[n] := nil // always end with a last argv^[] = nil
            else
              include(result, pcTooManyArguments);
          if argc <> nil then
            argc^ := n;
          exit;
        end;
      #1 .. ' ':
        begin
         if state = [sInArg] then
         begin
           state := [];
           if d <> nil then
           begin
             d^ := #0;
             inc(d);
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
            case p^ of
              '"', '\', '$', '`':
                begin
                  include(state, sBslash);
                  continue;
                end;
            end;
          end
          else if p^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if d <> nil then
              d^ := p^;
            inc(p);
          end;
      '^':
        if not posix and
           (state * [sInSQ, sInDQ, sBslash] = []) then
          if PWord(p)^ = EOLW then
          begin
            inc(p, 2);
            continue;
          end
          else if p^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if d <> nil then
              d^ := p^;
            inc(p);
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
            if (argv <> nil) and
               (n <= high(argv^)) then
              argv^[n] := d;
            inc(n);
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
            if (argv <> nil) and
               (n <= high(argv^)) then
              argv^[n] := d;
            inc(n);
            state := [sInDQ, sInArg];
            continue;
          end
          else if state = [sInArg] then
          begin
            state := [sInDQ, sInArg];
            continue;
          end;
      '|',
      '<',
      '>':
        if state * [sInSQ, sInDQ] = [] then
          include(result, pcHasRedirection);
      '&',
      ';':
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
      '(',
      ')':
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
      '*',
      '?':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasWildcard);
    end;
    exclude(state, sBslash);
    if state = [] then
    begin
      if (argv <> nil) and
         (n <= high(argv^)) then
        argv^[n] := d;
      inc(n);
      state := [sInArg];
    end;
    if d <> nil then
      inc(d);
  until false;
end;


procedure InitializeUnit;
begin
  {$ifdef ISFPC27}
  // we force UTF-8 everywhere on FPC for consistency with Lazarus
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$endif ISFPC27}
  GlobalCriticalSection.Init;
  ConsoleCriticalSection.Init;
  InitializeSpecificUnit; // in mormot.core.os.posix/windows.inc files
  InitializeProcessInfo;  // cross-platform info - e.g. User/Host + Executable
  // setup some constants
  JSON_CONTENT_TYPE_VAR := JSON_CONTENT_TYPE;
  JSON_CONTENT_TYPE_HEADER_VAR := JSON_CONTENT_TYPE_HEADER;
  NULL_STR_VAR := 'null';
  BOOL_UTF8[false] := 'false';
  BOOL_UTF8[true]  := 'true';
  {$ifdef CPUINTEL}
  if (CpuManufacturer = icmAmd) and
     (CpuFamily = $19) and
     (CpuModel >= $30) then // Zen 3 or later
    SpinFactor := 10;       // "pause" opcode is only 1-2 cycles
  {$endif CPUINTEL}
  // minimal stubs which will be properly implemented in other mormot.core units
  GetExecutableLocation := _GetExecutableLocation; // mormot.core.log
  SetThreadName         := _SetThreadName;
  GetCurrentThreadName  := _GetCurrentThreadName;
  ShortToUuid           := _ShortToUuid;           // mormot.core.text
  AppendShortUuid       := _AppendShortUuid;
  GetEnumNameRtti       := _GetEnumNameRtti;       // mormot.core.rtti
  RawToBase64           := _RawToBase64;           // mormot.core.buffers
end;

procedure FinalizeUnit;
var
  i: PtrInt;
begin
  with InternalGarbageCollection do
  begin
    Shutdown := true; // avoid nested initialization at shutdown
    for i := Count - 1 downto 0 do
      FreeAndNilSafe(Instances[i]); // before GlobalCriticalSection deletion
  end;
  ObjArrayClear(CurrentFakeStubBuffers);
  Executable.Version.Free;
  Executable.Command.Free;
  FinalizeSpecificUnit; // in mormot.core.os.posix/windows.inc files
  ConsoleCriticalSection.Done;
  GlobalCriticalSection.Done;
  {$ifndef NOEXCEPTIONINTERCEPT}
  _RawLogException := nil;
  RawExceptionIntercepted := true;
  {$endif NOEXCEPTIONINTERCEPT}
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

