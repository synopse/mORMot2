/// Framework Core Low-Level Wrappers to the Operating-System API
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os;

{
  *****************************************************************************

  Cross-platform functions shared by all framework units
  - Gather Operating System Information
  - Operating System Specific Types (e.g. TWinRegistry)
  - Unicode, Time, File, Console process
  - Per Class Properties O(1) Lookup via vmtAutoTable Slot (e.g. for RTTI cache)
  - TSynLocker Threading Features

   Aim of this unit is to centralize most used OS-specific API calls, like a
  SysUtils unit on steroids, to avoid $ifdef/$endif in "uses" clauses.
   In practice, no "Windows", nor "Linux/Unix" reference should be needed in
  regular units, once mormot.core.os is included.

  This unit only refers to mormot.core.base so could be used almost stand-alone.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef MSWINDOWS}
  windows, // needed here e.g. for redefinition of standard types
  {$endif MSWINDOWS}
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base;


{ ****************** Gather Operating System Information }

type
  /// Exception types raised by this mormot.core.os unit
  EOSException = class(Exception);

  /// the recognized operating systems
  // - it will also recognize most Linux distributions
  TOperatingSystem = (osUnknown, osWindows, osLinux, osOSX, osBSD, osPOSIX,
    osArch, osAurox, osDebian, osFedora, osGentoo, osKnoppix, osMint, osMandrake,
    osMandriva, osNovell, osUbuntu, osSlackware, osSolaris, osSuse, osSynology,
    osTrustix, osClear, osUnited, osRedHat, osLFS, osOracle, osMageia, osCentOS,
    osCloud, osXen, osAmazon, osCoreOS, osAlpine, osAndroid);

  /// the recognized Windows versions
  // - defined even outside MSWINDOWS to access e.g. from monitoring tools
  TWindowsVersion = (
    wUnknown, w2000, wXP, wXP_64, wServer2003, wServer2003_R2,
    wVista, wVista_64, wServer2008, wServer2008_64,
    wSeven, wSeven_64, wServer2008_R2, wServer2008_R2_64,
    wEight, wEight_64, wServer2012, wServer2012_64,
    wEightOne, wEightOne_64, wServer2012R2, wServer2012R2_64,
    wTen, wTen_64, wServer2016, wServer2016_64, wServer2019_64);

  /// the running Operating System, encoded as a 32-bit integer
  TOperatingSystemVersion = packed record
    case os: TOperatingSystem of
    osUnknown: (b: array[0..2] of byte);
    osWindows: (win: TWindowsVersion);
    osLinux:   (utsrelease: array[0..2] of byte);
  end;

const
  /// the recognized Windows versions, as plain text
  // - defined even outside MSWINDOWS to allow process e.g. from monitoring tools
  WINDOWS_NAME: array[TWindowsVersion] of RawUTF8 = (
    '', '2000', 'XP', 'XP 64bit', 'Server 2003', 'Server 2003 R2',
    'Vista', 'Vista 64bit', 'Server 2008', 'Server 2008 64bit',
    '7', '7 64bit', 'Server 2008 R2', 'Server 2008 R2 64bit',
    '8', '8 64bit', 'Server 2012', 'Server 2012 64bit',
    '8.1', '8.1 64bit', 'Server 2012 R2', 'Server 2012 R2 64bit',
    '10', '10 64bit', 'Server 2016', 'Server 2016 64bit', 'Server 2019 64bit');

  /// the recognized Windows versions which are 32-bit
  WINDOWS_32 = [w2000, wXP, wServer2003, wServer2003_R2, wVista, wServer2008,
    wSeven, wServer2008_R2, wEight, wServer2012, wEightOne, wServer2012R2,
    wTen, wServer2016];

  /// translate one operating system (and distribution) into a its common name
  OS_NAME: array[TOperatingSystem] of RawUTF8 =
   ('Unknown', 'Windows', 'Linux', 'OSX', 'BSD', 'POSIX',
    'Arch', 'Aurox', 'Debian', 'Fedora', 'Gentoo', 'Knoppix', 'Mint', 'Mandrake',
    'Mandriva', 'Novell', 'Ubuntu', 'Slackware', 'Solaris', 'Suse', 'Synology',
    'Trustix', 'Clear', 'United', 'RedHat', 'LFS', 'Oracle', 'Mageia', 'CentOS',
    'Cloud', 'Xen', 'Amazon', 'CoreOS', 'Alpine', 'Android');

  /// translate one operating system (and distribution) into a single character
  // - may be used internally e.g. for a HTTP User-Agent header, as with
  // TFileVersion.UserAgent
  OS_INITIAL: array[TOperatingSystem] of AnsiChar =
    ('?', 'W', 'L', 'X', 'B', 'P', 'A', 'a', 'D', 'F', 'G', 'K', 'M', 'm',
     'n', 'N', 'U', 'S', 's', 'u', 'Y', 'T', 'C', 't', 'R', 'l', 'O', 'G',
     'c', 'd', 'x', 'Z', 'r', 'p', 'J'); // for Android: J=JVM

  /// the operating systems items which actually have a Linux kernel
  OS_LINUX = [osLinux, osArch .. osAndroid];

  /// the compiler family used
  COMP_TEXT = {$ifdef FPC}'Fpc'{$else}'Delphi'{$endif};

  /// the target Operating System used for compilation, as text
  OS_TEXT = {$ifdef MSWINDOWS}'Win'{$else}{$ifdef DARWIN}'OSX'{$else}
  {$ifdef BSD}'BSD'{$else}{$ifdef ANDROID}'Android'{$else}{$ifdef LINUX}'Linux'{$else}'Posix'
  {$endif}{$endif}{$endif}{$endif}{$endif};

  /// the CPU architecture used for compilation
  CPU_ARCH_TEXT = {$ifdef CPUX86}'x86'{$else}{$ifdef CPUX64}'x64'{$else}
    {$ifdef CPUARM3264}'arm'+{$else}
    {$ifdef CPUPOWERPC}'ppc'+{$else}
    {$ifdef CPUSPARC}'sparc'+{$endif}{$endif}{$endif}
    {$ifdef CPU32}'32'{$else}'64'{$endif}{$endif}{$endif};

var
  /// the target Operating System used for compilation, as TOperatingSystem
  // - a specific Linux distribution may be detected instead of plain osLinux
  OS_KIND: TOperatingSystem = {$ifdef MSWINDOWS}osWindows{$else}{$ifdef DARWIN}osOSX{$else}
  {$ifdef BSD}osBSD{$else}{$ifdef Android}osAndroid{$else}{$ifdef LINUX}osLinux{$else}osPOSIX
  {$endif}{$endif}{$endif}{$endif}{$endif};
  /// the current Operating System version, as retrieved for the current process
  // - contains e.g. 'Windows Seven 64 SP1 (6.1.7601)' or
  // 'Ubuntu 16.04.5 LTS - Linux 3.13.0 110 generic#157 Ubuntu SMP Mon Feb 20 11:55:25 UTC 2017'
  OSVersionText: RawUTF8;
  /// some addition system information as text, e.g. 'Wine 1.1.5'
  // - also always appended to OSVersionText high-level description
  OSVersionInfoEx: RawUTF8;
  /// some textual information about the current CPU
  CpuInfoText: RawUTF8;
  /// some textual information about the current computer hardware, from BIOS
  BiosInfoText: RawUTF8;
  /// the running Operating System
  OSVersion32: TOperatingSystemVersion;
  /// the running Operating System, encoded as a 32-bit integer
  OSVersionInt32: integer absolute OSVersion32;

const
  /// contains the Delphi/FPC Compiler Version as text
  // - e.g. 'Delphi 10.3 Rio', 'Delphi 2010' or 'Free Pascal 3.3.1'
  COMPILER_VERSION: RawUTF8 =
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
      {$elseif defined(VER340)} + ' 10.4 Next'
      {$ifend}
    {$endif CONDITIONALEXPRESSIONS}
  {$endif FPC}
  {$ifdef CPU64} + ' 64 bit' {$else} + ' 32 bit' {$endif};

{$ifndef PUREMORMOT2}

/// deprecated function: use COMPILER_VERSION constant instead
function GetDelphiCompilerVersion: RawUTF8; deprecated;

{$endif PUREMORMOT2}

{$ifdef MSWINDOWS}

{$ifndef UNICODE}
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

{$else MSWINDOWS}

var
  /// emulate only some used fields of Windows' TSystemInfo
  SystemInfo: record
    // retrieved from libc's getpagesize() - is expected to not be 0
    dwPageSize: cardinal;
    // retrieved from HW_NCPU (BSD) or /proc/cpuinfo (Linux)
    dwNumberOfProcessors: cardinal;
    // meaningful system information, as returned by fpuname()
    uts: record
      sysname, release, version: RawUTF8;
    end;
    /// Linux Distribution release name, retrieved from /etc/*-release
    release: RawUTF8;
  end;
  
{$endif MSWINDOWS}

{$M+} // to have existing RTTI for published properties

type
  /// used to retrieve version information from any EXE
  // - under Linux, all version numbers are set to 0 by default, unless
  // you define the FPCUSEVERSIONINFO conditional and information is
  // extracted from executable resources
  // - you should not have to use this class directly, but via the
  // ExeVersion global variable
  TFileVersion = class
  protected
    fDetailed: string;
    fFileName: TFileName;
    fBuildDateTime: TDateTime;
    fVersionInfo, fUserAgent: RawUTF8;
    /// change the version (not to be used in most cases)
    procedure SetVersion(aMajor, aMinor, aRelease, aBuild: integer);
  public
    /// executable major version number
    Major: Integer;
    /// executable minor version number
    Minor: Integer;
    /// executable release version number
    Release: Integer;
    /// executable release build number
    Build: Integer;
    /// build year of this exe file
    BuildYear: word;
    /// version info of the exe file as '3.1'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    Main: string;
    /// associated CompanyName string version resource
    CompanyName: RawUTF8;
    /// associated FileDescription string version resource
    FileDescription: RawUTF8;
    /// associated FileVersion string version resource
    FileVersion: RawUTF8;
    /// associated InternalName string version resource
    InternalName: RawUTF8;
    /// associated LegalCopyright string version resource
    LegalCopyright: RawUTF8;
    /// associated OriginalFileName string version resource
    OriginalFilename: RawUTF8;
    /// associated ProductName string version resource
    ProductName: RawUTF8;
    /// associated ProductVersion string version resource
    ProductVersion: RawUTF8;
    /// associated Comments string version resource
    Comments: RawUTF8;
    /// associated Language Translation string version resource
    LanguageInfo: RawUTF8;
    /// retrieve application version from exe file name
    // - DefaultVersion32 is used if no information Version was included into
    // the executable resources (on compilation time)
    // - you should not have to use this constructor, but rather access the
    // ExeVersion global variable
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
    function VersionInfo: RawUTF8;
    /// returns a ready-to-use User-Agent header with exe name, version and OS
    // - e.g. 'myprogram/3.1.0.123W32' for myprogram running on Win32
    // - here OS_INITIAL[] character is used to identify the OS, with '32'
    // appended on Win32 only (e.g. 'myprogram/3.1.0.2W', is for Win64)
    function UserAgent: RawUTF8;
    /// returns the version information of a specified exe file as text
    // - includes FileName (without path), Detailed and BuildDateTime properties
    // - e.g. 'myprogram.exe 3.1.0.123 2016-06-14 19:07:55'
    class function GetVersionInfo(const aFileName: TFileName): RawUTF8;
  published
    /// version info of the exe file as '3.1.0.123'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - under Linux, always return '0.0.0.0' if no custom version number
    // has been defined
    // - consider using DetailedOrVoid method if '0.0.0.0' is not expected
    property Detailed: string read fDetailed write fDetailed;
    /// build date and time of this exe file
    property BuildDateTime: TDateTime read fBuildDateTime write fBuildDateTime;
  end;

{$M-}

type
  /// stores some global information about the current executable and computer
  TExeVersion = record
    /// the main executable name, without any path nor extension
    // - e.g. 'Test' for 'c:\pathto\Test.exe'
    ProgramName: RawUTF8;
    /// the main executable details, as used e.g. by TSynLog
    // - e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.123 (2011-03-29 11:09:06)'
    ProgramFullSpec: RawUTF8;
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
    Host: RawUTF8;
    /// the current computer user name
    User: RawUTF8;
    /// some hash representation of this information
    // - the very same executable on the very same computer run by the very
    // same user will always have the same Hash value
    // - is computed from the crc32c of this TExeVersion fields: c0 from
    // Version32, CpuFeatures and Host, c1 from User, c2 from ProgramFullSpec
    // and c3 from InstanceFileName
    // - may be used as an entropy seed, or to identify a process execution
    Hash: THash128Rec;
  end;

var
  /// global information about the current executable and computer
  // - this structure is initialized in this unit's initialization block below
  // - you can call SetExecutableVersion() with a custom version, if needed
  ExeVersion: TExeVersion;

/// initialize ExeVersion global variable, supplying a custom version number
// - by default, the version numbers will be retrieved at startup from the
// executable itself (if it was included at build time)
// - but you can use this function to set any custom version numbers
procedure SetExecutableVersion(aMajor,aMinor,aRelease,aBuild: integer); overload;

/// initialize ExeVersion global variable, supplying the version as text
// - e.g. SetExecutableVersion('7.1.2.512');
procedure SetExecutableVersion(const aVersionText: RawUTF8); overload;

type
  /// identify an operating system folder
  TSystemPath = (
    spCommonData, spUserData, spCommonDocuments, spUserDocuments, spTempFolder, spLog);

/// returns an operating system folder
// - will return the full path of a given kind of private or shared folder,
// depending on the underlying operating system
// - will use SHGetFolderPath and the corresponding CSIDL constant under Windows
// - under POSIX, will return $TMP/$TMPDIR folder for spTempFolder, ~/.cache/appname
// for spUserData, /var/log for spLog, or the $HOME folder
// - returned folder name contains the trailing path delimiter (\ or /)
function GetSystemPath(kind: TSystemPath): TFileName;



{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{$ifdef MSWINDOWS}

type
  TThreadID = DWORD;

  /// the known Windows Registry Root key used by TWinRegistry.Open
  TWinRegistryRoot = (
    wrClasses, wrCurrentUser, wrLocalMachine, wrUsers);

  /// direct access to the Windows Registry
  // - could be used as alternative to TRegistry, which doesn't behave the same on
  // all Delphi versions, and is enhanced on FPC (e.g. which supports REG_MULTI_SZ)
  // - is also Unicode ready for text, using UTF-8 conversion on all compilers
  TWinRegistry = object
  public
    /// the opened HKEY handle
    key: HKEY;
    /// start low-level read access to a Windows Registry node
    // - on success (returned true), ReadClose() should be called
    function ReadOpen(root: TWinRegistryRoot; const keyname: RawUTF8;
      closefirst: boolean = false): boolean;
    /// finalize low-level read access to the Windows Registry after ReadOpen()
    procedure Close;
    /// low-level read a string from the Windows Registry after ReadOpen()
    // - in respect to Delphi's TRegistry, will properly handle REG_MULTI_SZ
    // (return the first value of the multi-list)
    function ReadString(const entry: SynUnicode; andtrim: boolean = true): RawUTF8;
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
    function ReadEnumEntries: TRawUTF8DynArray;
  end;


type
  HCRYPTPROV = pointer;
  HCRYPTKEY = pointer;
  HCRYPTHASH = pointer;

  /// direct access to the Windows CryptoAPI
  TWinCryptoAPI = object
  private
    /// if the presence of this API has been tested
    Tested: boolean;
    /// if this API has been loaded
    Handle: THandle;
    /// used when inlining Available method
    procedure Acquire;
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
    Encrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
    /// decrypts data previously encrypted by using the CryptEncrypt function
    Decrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD): BOOL; stdcall;
    /// fills a buffer with cryptographically random bytes
    // - since Windows Vista with Service Pack 1 (SP1), an AES counter-mode
    // based PRNG specified in NIST Special Publication 800-90 is used
    GenRandom: function(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall;
    /// try to load the CryptoAPI on this system
    function Available: boolean; {$ifdef HASINLINE} inline; {$endif}
  end;

const
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
  CryptoAPI: TWinCryptoAPI;

/// protect some data for the current user, using Windows DPAPI
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - will use CryptProtectData DPAPI function call under Windows
// - see https://msdn.microsoft.com/en-us/library/ms995355
// - this function is Windows-only, could be slow, and you don't know which
// algorithm is really used on your system, so using our mormot.core.crypto.pas
// CryptDataForCurrentUser() is probably a better (and cross-platform) alternative
// - also note that DPAPI has been closely reverse engineered - see e.g.
// https://www.passcape.com/index.php?section=docsys&cmd=details&id=28
function CryptDataForCurrentUserDPAPI(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;

/// retrieves the current executable module handle, i.e.  its memory load address
// - redefined in mormot.core.os to avoid dependency to Windows
function GetModuleHandle(lpModuleName: PChar): HMODULE;

/// retrieves the current stack trace
// - only available since Windows XP
// - FramesToSkip + FramesToCapture should be <= 62
function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: cardinal;
  BackTrace, BackTraceHash: pointer): byte; stdcall;

/// compatibility function, wrapping Win32 API available since XP
function IsDebuggerPresent: BOOL; stdcall;

/// retrieves the current thread ID
// - redefined in mormot.core.os to avoid dependency to Windows
function GetCurrentThreadId: DWORD; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to Windows
function GetCurrentProcessId: DWORD; stdcall;

/// redefined in mormot.core.os to avoid dependency to Windows
function GetEnvironmentStringsW: PWideChar; stdcall;

/// redefined in mormot.core.os to avoid dependency to Windows
function FreeEnvironmentStringsW(EnvBlock: PWideChar): BOOL; stdcall;

/// try to enter a Critical Section (Lock)
// - redefined in mormot.core.os to avoid dependency to Windows
// - under Delphi/Windows, directly call the homonymous Win32 API
function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer; stdcall;

/// enter a Critical Section (Lock)
// - redefined in mormot.core.os to avoid dependency to Windows
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure EnterCriticalSection(var cs: TRTLCriticalSection); stdcall;

/// leave a Critical Section (UnLock)
// - redefined in mormot.core.os to avoid dependency to Windows
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure LeaveCriticalSection(var cs: TRTLCriticalSection); stdcall;

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

{$endif MSWINDOWS}


{ ****************** Unicode, Time, File, Console process }

{$ifdef MSWINDOWS}
type
  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  TSystemTime = Windows.TSystemTime;

  {$ifdef ISDELPHI}
  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  TRTLCriticalSection = Windows.TRTLCriticalSection;
  {$endif ISDELPHI}

/// returns the current UTC time as TSystemTime
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to Windows
// - you should call directly FPC's version otherwise
procedure GetLocalTime(out result: TSystemTime); stdcall;

{$endif MSWINDOWS}

/// initialize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to Windows
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// finalize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to Windows
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns TRUE if the supplied mutex has been initialized
// - will check if the supplied mutex is void (i.e. all filled with 0 bytes)
function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// on need initialization of a mutex, then enter the lock
// - if the supplied mutex has been initialized, do nothing
// - if the supplied mutex is void (i.e. all filled with 0), initialize it
procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
  {$ifdef HASINLINE}inline;{$endif}

/// on need finalization of a mutex
// - if the supplied mutex has been initialized, delete it
// - if the supplied mutex is void (i.e. all filled with 0), do nothing
procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);

/// enter a giant lock for thread-safe shared process
// - shall be protected as such:
// ! GlobalLock;
// ! try
// !   .... do something thread-safe but as short as possible
// ! finally
// !  GlobalUnLock;
// ! end;
// - you should better not use such a giant-lock, but an instance-dedicated
// critical section or TSynLocker - these functions are just here to be
// convenient, for non time-critical process (e.g. singleton initialization)
procedure GlobalLock;

/// release the giant lock for thread-safe shared process
// - you should better not use such a giant-lock, but an instance-dedicated
// critical section or TSynLocker - these functions are just here to be
// convenient, for non time-critical process (e.g. singleton initialization)
procedure GlobalUnLock;

/// returns the current UTC time as TSystemTime
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// - under Windows, directly call the homonymous Win32 API
procedure GetSystemTime(out result: TSystemTime);
  {$ifdef MSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(F: THandle);
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API file flush to disk
procedure FlushFileBuffers(F: THandle);
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code
function GetLastError: longint;
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code
procedure SetLastError(error: longint);
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns a given error code as plain text
// - calls FormatMessageW on Windows, or StrError() on POSIX
function GetErrorText(error: longint): RawUTF8;

/// compatibility function, wrapping GetACP() Win32 API function
// - returns the curent system code page (default WinAnsi)
function Unicode_CodePage: integer;

/// compatibility function, wrapping CompareStringW() Win32 API text comparison
// - returns 1 if PW1>PW2, 2 if PW1=PW2, 3 if PW1<PW2 - so substract 2 to have
// -1,0,1 as regular StrCompW/StrICompW comparison function result
// - will compute StrLen(PW1/PW2) if L1 or L2 < 0
// - somewhat slow by using two temporary UnicodeString on POSIX - but seldom
// called, unless our proprietary WIN32CASE collation is used in SynSQLite3
function Unicode_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: Boolean): integer;

/// compatibility function, wrapping MultiByteToWideChar() Win32 API call
// - returns the number of WideChar written into W^ destination buffer
function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;

/// compatibility function, wrapping WideCharToMultiByte() Win32 API call
// - returns the number of AnsiChar written into A^ destination buffer
function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;

/// returns a system-wide current monotonic timestamp as milliseconds
// - will use the corresponding native API function under Vista+, or will be
// redirected to a custom wrapper function for older Windows versions (XP)
// to avoid the 32-bit overflow/wrapping issue of GetTickCount
// - warning: FPC's SysUtils.GetTickCount64 or TThread.GetTickCount64 don't
// handle properly 49 days wrapping under XP -> always use this safe version
// - on POSIX, will call (via vDSO) the very fast CLOCK_MONOTONIC_COARSE if
// available, or the low-level mach_absolute_time() monotonic Darwin API
// - warning: FPC's SysUtils.GetTickCount64 may call fpgettimeofday() e.g.
// on Darwin, which is not monotonic -> always use this safe version
// - do not expect exact millisecond resolution - it may rather be within the
// 10-16 ms range, especially under Windows
{$ifdef MSWINDOWS}
var GetTickCount64: function: Int64; stdcall;
{$else}
function GetTickCount64: Int64;
{$endif MSWINDOWS}

/// returns the current UTC time
// - will convert from clock_gettime(CLOCK_REALTIME_COARSE) if available
function NowUTC: TDateTime;

/// returns the current UTC date/time as a second-based c-encoded time
// - i.e. current number of seconds elapsed since Unix epoch 1/1/1970
// - faster than NowUTC or GetTickCount64, on Windows or Unix platforms
// (will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime under Windows)
// - returns a 64-bit unsigned value, so is "Year2038bug" free
function UnixTimeUTC: Int64;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - i.e. current number of milliseconds elapsed since Unix epoch 1/1/1970
// - faster and more accurate than NowUTC or GetTickCount64, on Windows or Unix
// - will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime/GetSystemTimePreciseAsFileTime under Windows - the
// later being more accurate, but slightly slower than the former, so you may
// consider using UnixMSTimeUTCFast on Windows if its 10-16ms accuracy is enough
function UnixMSTimeUTC: Int64;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - under Linux/POSIX, is the very same than UnixMSTimeUTC (inlined call)
// - under Windows 8+, will call GetSystemTimeAsFileTime instead of
// GetSystemTimePreciseAsFileTime, which has higher precision, but is slower
// - prefer it under Windows, if a dozen of ms resolution is enough for your task
function UnixMSTimeUTCFast: Int64;
  {$ifdef LINUX} inline; {$endif}

{$ifndef MSWINDOWS}

var
  /// could be set to TRUE to force SleepHiRes(0) to call the sched_yield API
  // - in practice, it has been reported as buggy under POSIX systems
  // - even Linus Torvald himself raged against its usage - see e.g.
  // https://www.realworldtech.com/forum/?threadid=189711&curpostid=189752
  // - you may tempt the devil and try it by yourself
  SleepHiRes0Yield: boolean = false;

{$endif MSWINDOWS}

/// similar to Windows sleep() API call, to be truly cross-platform
// - using millisecond resolution
// - SleepHiRes(0) calls ThreadSwitch on Windows, but POSIX version will
// wait 10 microsecond unless SleepHiRes0Yield is forced to true (bad idea)
// - in respect to RTL's Sleep() function, it will return on ESysEINTR
procedure SleepHiRes(ms: cardinal);

/// low-level naming of a thread
// - under Linux/FPC, calls pthread_setname_np API which truncates to 16 chars
procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUTF8);

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
    /// timestamp of this exception, as number of seconds since UNIX Epoch (TUnixTime)
    // - UnixTimeUTC is faster than NowUTC or GetSystemTime
    // - use UnixTimeToDateTime() to convert it into a regular TDateTime
    ETimestamp: Int64;
    /// the logging level corresponding to this exception
    // - may be either sllException or sllExceptionOS
    ELevel: TSynLogInfo;
    /// retrieve some extended information about a given Exception
    // - on Windows, recognize most DotNet CLR Exception Names
    function AdditionalInfo(out ExceptionNames: TPUTF8CharDynArray): cardinal;
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

/// get a file date and time, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// low-level conversion of a TDateTime into a Windows File 32-bit TimeStamp
function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;

/// get the date and time of one file into a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileAgeToWindowsTime(const FileName: TFileName): integer;

/// copy the date of one file to another
function FileSetDateFrom(const Dest: TFileName; SourceHandle: integer): boolean;

/// copy the date of one file from a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;

/// modify the attributes of a given file
// - if Secret=false, will have normal file attributes, with read/write access
// - if Secret=true, will have hidden and read-only attributes
// - under POSIX, there is no "hidden" file attribute, but you should define a
// FileName starting by '.'
procedure FileSetAttributes(const FileName: TFileName; Secret: boolean);

/// get a file size, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
function FileSize(const FileName: TFileName): Int64; overload;

/// get a file size, from its handle
// - returns 0 if file doesn't exist
function FileSize(F: THandle): Int64; overload;

/// FileSeek() overloaded function, working with huge files
// - Delphi FileSeek() is buggy -> use this function to safe access files > 2 GB
// (thanks to sanyin for the report)
function FileSeek64(Handle: THandle; const Offset: Int64; Origin: cardinal): Int64;

/// get low-level file information, in a cross-platform way
// - returns true on success
// - here file write/creation time are given as TUnixMSTime values, for better
// cross-platform process - note that FileCreateDateTime may not be supported
// by most Linux file systems, so the oldest timestamp available is returned
// as failover on such systems (probably the latest file metadata writing)
function FileInfoByHandle(aFileHandle: THandle; out FileId, FileSize,
  LastWriteAccess, FileCreateDateTime: Int64): Boolean;

/// conversion of Windows OEM charset into a UTF-16 encoded string
function OemToUnicode(const OEM: RawByteString): SynUnicode;

/// conversion of Windows OEM charset into a file name
// - as used e.g. by mormot.core.zip for non UTF-8 file names
function OemToFileName(const OEM: RawByteString): TFileName;

const
  /// operating-system dependent wildchar to match all files in a folder
  {$ifdef MSWINDOWS}
  FILES_ALL = '*.*';
  {$else}
  FILES_ALL = '*';
  {$endif MSWINDOWS}

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

/// overloaded function optimized for one pass file reading
// - will use e.g. the FILE_FLAG_SEQUENTIAL_SCAN flag under Windows, as stated
// by http://blogs.msdn.com/b/oldnewthing/archive/2012/01/20/10258690.aspx
// - note: under XP, we observed ERROR_NO_SYSTEM_RESOURCES problems when calling
// FileRead() for chunks bigger than 32MB on files opened with this flag,
// so it would use regular FileOpen() on this deprecated OS
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by StringFromFile() and TSynMemoryStreamMapped.Create()
function FileOpenSequentialRead(const FileName: string): Integer;
  {$ifdef FPC}inline;{$endif}

/// returns a TFileStream optimized for one pass file reading
// - will use FileOpenSequentialRead(), i.e. FILE_FLAG_SEQUENTIAL_SCAN on Windows
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
function FileStreamSequentialRead(const FileName: string): THandleStream;

/// read a File content into a String
// - content can be binary or text
// - returns '' if file was not found or any read error occured
// - wil use GetFileSize() API by default, unless HasNoSize is defined,
// and read will be done using a buffer (required e.g. for char files under Linux)
// - uses RawByteString for byte storage, whatever the codepage is
function StringFromFile(const FileName: TFileName; HasNoSize: boolean = false): RawByteString;

/// create a File from a string content
// - uses RawByteString for byte storage, whatever the codepage is
function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean = false; FileDate: TDateTime = 0): boolean;

/// compute an unique temporary file name
// - following 'exename_123.tmp' pattern, in the system temporary folder
function TemporaryFileName: TFileName;

/// check if the directory is writable for the current user
// - try to write a small file with a random name
function IsDirectoryWritable(const Directory: TFileName): boolean;

type
  /// text file layout, as recognized by TMemoryMap.TextFileKind
  TTextFileKind = (isUnicode, isUTF8, isAnsi);

  /// cross-platform memory mapping of a file content
  TMemoryMap = object
  protected
    fBuf: PAnsiChar;
    fBufSize: PtrUInt;
    fFile: THandle;
    {$ifdef MSWINDOWS}
    fMap: THandle;
    {$endif MSWINDOWS}
    fFileSize: Int64;
    fFileLocal: boolean;
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
    /// recognize the BOM of a text file - returns isAnsi if no BOM is available
    function TextFileKind: TTextFileKind;
    /// unmap the file
    procedure UnMap;
    /// retrieve the memory buffer mapped to the file content
    property Buffer: PAnsiChar read fBuf;
    /// retrieve the buffer size
    property Size: PtrUInt read fBufSize;
    /// retrieve the mapped file size
    property FileSize: Int64 read fFileSize;
    /// access to the low-level associated File handle (if any)
    property FileHandle: THandle read fFile;
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
    property FileName: TFileName read fFileName;
  end;

  /// low-level access to a resource bound to the executable
  // - so that Windows is not required by Delphi in your unit uses clause
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
    {$ifdef MSWINDOWS}
    fSysPrevIdle, fSysPrevKernel, fSysPrevUser,
    fDiffIdle, fDiffKernel, fDiffUser, fDiffTotal: Int64;
    {$endif MSWINDOWS}
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
    name: RawUTF8;
    /// where this partition has been mounted
    // - e.g. 'C:' or '/home'
    // - you can use GetDiskInfo(mounted) to retrieve current space information
    mounted: TFileName;
    /// total size (in bytes) of this partition
    size: QWord;
  end;

  /// stores information about several disk partitions
  TDiskPartitions = array of TDiskPartition;


/// return the PIDs of all running processes
// - under Windows, is a wrapper around EnumProcesses() PsAPI call
// - on Linux, will enumerate /proc/* pseudo-files
function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;

/// return the process name of a given PID
// - under Windows, is a wrapper around QueryFullProcessImageNameW/GetModuleFileNameEx
// PsAPI call
// - on Linux, will query /proc/[pid]/exe or /proc/[pid]/cmdline pseudo-file
function EnumProcessName(PID: Cardinal): RawUTF8;

/// return the system-wide time usage information
// - under Windows, is a wrapper around GetSystemTimes() kernel API call
function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;

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
  {$ifdef MSWINDOWS}; aVolumeName: PSynUnicode = nil{$endif}): boolean;

/// retrieve low-level information about all mounted disk partitions of the system
// - returned partitions array is sorted by "mounted" ascending order
function GetDiskPartitions: TDiskPartitions;

type
  /// available console colors
  TConsoleColor = (
    ccBlack, ccBlue, ccGreen, ccCyan, ccRed, ccMagenta, ccBrown, ccLightGray,
    ccDarkGray, ccLightBlue, ccLightGreen, ccLightCyan, ccLightRed, ccLightMagenta,
    ccYellow, ccWhite);

{$ifdef LINUX}
var
  stdoutIsTTY: boolean;
{$endif LINUX}

/// similar to Windows AllocConsole API call, to be truly cross-platform
// - do nothing on Linux/POSIX
procedure AllocConsole;
  {$ifdef MSWINDOWS} stdcall; {$else} inline; {$endif}

/// change the console text writing color
// - you should call this procedure to initialize StdOut global variable, if
// you manually initialized the Windows console, e.g. via the following code:
// ! AllocConsole;
// ! TextColor(ccLightGray); // initialize internal console context
procedure TextColor(Color: TConsoleColor);

/// write some text to the console using a given color
procedure ConsoleWrite(const Text: RawUTF8; Color: TConsoleColor = ccLightGray;
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

{$ifdef MSWINDOWS}

/// low-level access to the keyboard state of a given key
function ConsoleKeyPressed(ExpectedKey: Word): Boolean;

{$endif MSWINDOWS}

/// direct conversion of a UTF-8 encoded string into a console OEM-encoded String
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function Utf8ToConsole(const S: RawUTF8): RawByteString;

var
  /// low-level handle used for console writing
  // - may be overriden when console is redirected
  // - is initialized when TextColor() is called
  StdOut: THandle;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

/// self-modifying code - change some memory buffer in the code segment
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overridden code buffer, for further hook disabling
procedure PatchCode(Old,New: pointer; Size: PtrInt; Backup: pointer = nil;
  LeaveUnprotected: boolean = false);

/// self-modifying code - change one PtrUInt in the code segment
procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt;
  LeaveUnprotected: boolean = false);


/// search for a given class stored in an object vmtAutoTable Slot
// - up to 15 properties could be registered per class
// - quickly returns the PropertiesClass instance for this class on success
// - returns nil if no Properties was registered for this class; caller should
// call ClassPropertiesAdd() to initialize
function ClassPropertiesGet(ObjectClass, PropertiesClass: TClass): pointer;
  {$ifdef HASINLINE} inline; {$endif}

/// try to register a given Properties instance for a given class
// - returns associated PropertiesInstance otherwise, which may not be the supplied
// PropertiesInstance, if it has been registered by another thread in between -
// it will free the supplied PropertiesInstance in this case, and return the existing
function ClassPropertiesAdd(ObjectClass: TClass; PropertiesInstance: TObject;
  FreeExistingPropertiesInstance: boolean = true): TObject;



{ **************** TSynLocker Threading Features }

type
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
  TSynLocker = object
  protected
    fSection: TRTLCriticalSection;
    fSectionPadding: PtrInt; // paranoid to avoid FUTEX_WAKE_PRIVATE=EAGAIN
    fLocked, fInitialized: boolean;
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
    function GetUTF8(Index: integer): RawUTF8;
    procedure SetUTF8(Index: integer; const Value: RawUTF8);
  public
    /// internal padding data, also used to store up to 7 variant values
    // - this memory buffer will ensure no CPU cache line mixup occurs
    // - you should not use this field directly, but rather the Locked[],
    // LockedInt64[], LockedUTF8[] or LockedPointer[] methods
    // - if you want to access those array values, ensure you protect them
    // using a Safe.Lock; try ... Padding[n] ... finally Safe.Unlock structure,
    // and maintain the PaddingUsedCount field accurately
    Padding: array[0..6] of TVarData;
    /// number of values stored in the internal Padding[] array
    // - equals 0 if no value is actually stored, or a 1..7 number otherwise
    // - you should not have to use this field, but for optimized low-level
    // direct access to Padding[] values, within a Lock/UnLock safe block
    PaddingUsedCount: integer;
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
    /// lock the instance for exclusive access
    // - this method is re-entrant from the same thread (you can nest Lock/UnLock
    // calls in the same thread), but would block any other Lock attempt in
    // another thread
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! Safe.Lock;
    // ! try
    // !   ...
    // ! finally
    // !   Safe.Unlock;
    // ! end;
    procedure Lock; {$ifdef FPC} inline; {$endif}
    /// will try to acquire the mutex
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLock then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLock: boolean; {$ifdef FPC} inline; {$endif}
    /// will try to acquire the mutex for a given time
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLockMS(100) then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLockMS(retryms: integer): boolean;
    /// release the instance for exclusive access
    // - each Lock/TryLock should have its exact UnLock opposite, so a
    // try..finally block is mandatory for safe code
    procedure UnLock; {$ifdef FPC} inline; {$endif}
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
    // !  with Safe.ProtectMethod do begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// returns true if the mutex is currently locked by another thread
    property IsLocked: boolean read fLocked;
    /// returns true if the Init method has been called for this mutex
    // - is only relevant if the whole object has been previously filled with 0,
    // i.e. as part of a class or as global variable, but won't be accurate
    // when allocated on stack
    property IsInitialized: boolean read fInitialized;
    /// safe locked access to a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // LockedBool, LockedInt64, LockedPointer and LockedUTF8 array properties
    // - returns null if the Index is out of range
    property Locked[Index: integer]: Variant read GetVariant write SetVariant;
    /// safe locked access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    property LockedInt64[Index: integer]: Int64 read GetInt64 write SetInt64;
    /// safe locked access to a boolean value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedInt64, LockedPointer and LockedUTF8 array properties
    // - value will be stored internally as a varBoolean variant
    // - returns nil if the Index is out of range, or does not store a boolean
    property LockedBool[Index: integer]: boolean read GetBool write SetBool;
    /// safe locked access to a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedBool, LockedInt64 and LockedUTF8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns nil if the Index is out of range, or does not store a pointer
    property LockedPointer[Index: integer]: Pointer read GetPointer write SetPointer;
    /// safe locked access to an UTF-8 string value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedPointer array properties
    // - UTF-8 string will be stored internally as a varString variant
    // - returns '' if the Index is out of range, or does not store a string
    property LockedUTF8[Index: integer]: RawUTF8 read GetUTF8 write SetUTF8;
    /// safe locked in-place increment to an Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns the newly stored value
    // - if the internal value is not defined yet, would use 0 as default value
    function LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
    /// safe locked in-place exchange of a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - returns the previous stored value, or null if the Index is out of range
    function LockedExchange(Index: integer; const Value: variant): variant;
    /// safe locked in-place exchange of a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns the previous stored value, nil if the Index is out of range,
    // or does not store a pointer
    function LockedPointerExchange(Index: integer; Value: pointer): pointer;
    /// unsafe access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - you should rather call LockedInt64[] property, or use this property
    // with a Lock; try ... finally UnLock block
    property UnlockedInt64[Index: integer]: Int64 read GetUnlockedInt64 write SetUnlockedInt64;
  end;

  /// a pointer to a TSynLocker mutex instance
  // - see also NewSynLocker and TSynLocker.DoneAndFreemem functions
  PSynLocker = ^TSynLocker;

/// initialize a TSynLocker instance from heap
// - call DoneandFreeMem to release the associated memory and OS mutex
// - is used e.g. in TSynPersistentLock to reduce class instance size
function NewSynLocker: PSynLocker;

type
  {$M+}

  /// a lighter alternative to TSynPersistentLock
  // - can be used as base class when custom JSON persistence is not needed
  TSynLocked = class
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the instance, and its associated lock
    // - is defined as virtual, just like TSynPersistent
    constructor Create; virtual;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker read fSafe;
  end;

  {$M-}

  /// meta-class definition of the TSynLocked hierarchy
  TSynLockedClass = class of TSynLocked;

implementation

// those include files hold all OS-specific functions
// note: the *.inc files start with their own "uses" clause, so both $include
// should remain here, just after the "implementation" clause

{$ifdef LINUX}
  {$include mormot.core.os.posix.inc}
{$endif LINUX}

{$ifdef MSWINDOWS}
  {$include mormot.core.os.windows.inc}
{$endif MSWINDOWS}


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

const
  MAX_AUTOSLOT = 7;

type
  TAutoSlots = array[0..MAX_AUTOSLOT] of TObject; // always end with last=nil
  PAutoSlots = ^TAutoSlots;

var
  AutoSlotsLock: TRTLCriticalSection;
  AutoSlots: array of PAutoSlots; // not "of TAutoSlots" to have static pointers


procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt; LeaveUnprotected: boolean);
begin
  PatchCode(Code, @Value, SizeOf(Code^), nil, LeaveUnprotected);
end;


function ClassPropertiesGet(ObjectClass, PropertiesClass: TClass): pointer;
var
  slots: PObject;
begin
  slots := PPointer(PAnsiChar(ObjectClass) + vmtAutoTable)^;
  if slots <> nil then
  begin
    ObjectClass := PropertiesClass; // better TClass constant inlining on FPC
    repeat
      result := slots^;
      if (result = nil) or (PClass(result)^ = ObjectClass) then
        exit; // reached end of list, or found the expected class type
      inc(slots);
    until false;
  end;
  result := nil;
end;

function ClassPropertiesAdd(ObjectClass: TClass; PropertiesInstance: TObject;
  FreeExistingPropertiesInstance: boolean): TObject;
var
  vmt: PPointer;
  slots: PAutoSlots;
  i: PtrInt;
begin
  EnterCriticalSection(AutoSlotsLock);
  try
    result := ClassPropertiesGet(ObjectClass, PropertiesInstance.ClassType);
    if result <> nil then
    begin
      // some background thread registered its own
      if FreeExistingPropertiesInstance then
        PropertiesInstance.Free;
      exit;
    end;
    vmt := Pointer(PAnsiChar(ObjectClass) + vmtAutoTable);
    slots := vmt^;
    if slots = nil then
    begin
      slots := AllocMem(SizeOf(slots^));
      PtrArrayAdd(AutoSlots, slots);
      PatchCodePtrUInt(pointer(vmt), PtrUInt(slots), {leaveunprotected=}true);
      if vmt^ <> slots then
        raise EOSException.CreateFmt('ClassPropertiesAdd: mprotect failed for %s',
          [ObjectClass.ClassName]);
    end;
    for i := 0 to high(slots^) - 1 do
      if slots^[i] = nil then
      begin
        // use the first void slot
        slots^[i] := PropertiesInstance;
        result := PropertiesInstance;
        exit;
      end;
  finally
    LeaveCriticalSection(AutoSlotsLock);
  end;
  raise EOSException.CreateFmt('ClassPropertiesAdd: no slot available on %s',
    [ObjectClass.ClassName]);
end;


{ ****************** Unicode, Time, File, Console process }

procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
begin
  if not IsInitializedCriticalSection(cs) then
    InitializeCriticalSection(cs);
  EnterCriticalSection(cs);
end;

procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);
begin
  if IsInitializedCriticalSection(cs) then
    DeleteCriticalSection(cs);
end;

var
  GlobalCriticalSection: TRTLCriticalSection;

procedure GlobalLock;
begin
  EnterCriticalSection(GlobalCriticalSection);
end;

procedure GlobalUnLock;
begin
  LeaveCriticalSection(GlobalCriticalSection);
end;

function Unicode_CodePage: integer;
begin
  result := GetACP;
end;

function Unicode_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: Boolean): integer;
const
  _CASEFLAG: array[boolean] of DWORD = (0, NORM_IGNORECASE);
begin
  result := CompareStringW(LOCALE_USER_DEFAULT, _CASEFLAG[IgnoreCase], PW1, L1, PW2, L2);
end;

function NowUTC: TDateTime;
begin
  result := UnixMSTimeUTC / MSecsPerDay + UnixDelta;
end;

function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;
var
  YY, MM, DD, H, m, s, ms: word;
begin
  DecodeDate(DateTime, YY, MM, DD);
  DecodeTime(DateTime, h, m, s, ms);
  If (YY < 1980) or (YY > 2099) then
    result := 0
  else
    result := (s shr 1) or (m shl 5) or (h shl 11) or
              integer((DD shl 16) or (MM shl 21) or (word(YY - 1980) shl 25));
end;

function SearchRecToDateTime(const F: TSearchRec): TDateTime;
begin
  {$ifdef ISDELPHIXE}
  result := F.Timestamp;
  {$else}
  result := FileDateToDateTime(F.Time);
  {$endif}
end;

function SearchRecValidFile(const F: TSearchRec): boolean;
begin
  result := (F.Name <> '') and (F.Attr and faInvalidFile = 0);
end;

function SearchRecValidFolder(const F: TSearchRec): boolean;
begin
  result := (F.Attr and faDirectoryMask = faDirectory) and
            (F.Name <> '') and (F.Name <> '.') and (F.Name <> '..');
end;

{$ifdef FPC}
type
  // FPC TFileStream miss a Create(aHandle) constructor like Delphi
  TFileStreamFromHandle = class(THandleStream)
  public
    destructor Destroy; override;
  end;

destructor TFileStreamFromHandle.Destroy;
begin
  FileClose(Handle); // otherwise file is still opened
end;

{$else}

type
  TFileStreamFromHandle = TFileStream;

{$endif FPC}

function FileStreamSequentialRead(const FileName: string): THandleStream;
begin
  result := TFileStreamFromHandle.Create(FileOpenSequentialRead(FileName));
end;

function StringFromFile(const FileName: TFileName; HasNoSize: boolean): RawByteString;
var
  F: THandle;
  Read, Size, Chunk: integer;
  P: PUTF8Char;
  tmp: array[0..$7fff] of AnsiChar; // 32KB stack buffer
begin
  result := '';
  if FileName = '' then
    exit;
  F := FileOpenSequentialRead(FileName);
  if PtrInt(F) >= 0 then
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
  {$ifdef MSWINDOWS}
  if FileDate <> 0 then
    FileSetDate(F, DateTimeToFileDate(FileDate));
  FileClose(F);
  {$else}
  FileClose(F); // POSIX expects the file to be closed to set the date
  if FileDate <> 0 then
    FileSetDate(FileName, DateTimeToFileDate(FileDate));
  {$endif MSWINDOWS}
  result := true;
end;

var
  _TmpCounter: integer;

function TemporaryFileName: TFileName;
var
  folder: TFileName;
  retry: integer;
begin // fast cross-platform implementation
  folder := GetSystemPath(spTempFolder);
  if _TmpCounter = 0 then
    _TmpCounter := Random32;
  retry := 10;
  repeat // thread-safe unique file name generation
    result := Format('%s%s_%x.tmp', [folder, ExeVersion.ProgramName,
      InterlockedIncrement(_TmpCounter)]);
    if not FileExists(result) then
      exit;
    dec(retry); // no endless loop
  until retry = 0;
  raise EOSException.Create('TemporaryFileName failed');
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
  FrameCount: Longint; Frame: PCodePointer);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWORD;
  backuphandler: TOnRawLogException;
begin
  if Assigned(_RawLogException) then
    if (Obj <> nil) and (Obj.InheritsFrom(Exception)) then
    begin
      backuplasterror := GetLastError;
      backuphandler := _RawLogException;
      try
        _RawLogException := nil; // disable exception
        ctxt.EClass := PPointer(Obj)^;
        ctxt.EInstance := Exception(Obj);
        ctxt.EAddr := PtrUInt(Addr);
        if Obj.InheritsFrom(EExternal) then
          ctxt.ELevel := sllExceptionOS
        else
          ctxt.ELevel := sllException;
        ctxt.ETimestamp := UnixTimeUTC;
        ctxt.EStack := pointer(Frame);
        ctxt.EStackCount := FrameCount;
        _RawLogException(ctxt);
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
  {$ifdef WITH_VECTOREXCEPT} // Win64 official API
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

function TMemoryMap.Map(aFile: THandle; aCustomSize: PtrUInt; aCustomOffset: Int64;
  aFileOwned: boolean): boolean;
var
  Available: Int64;
begin
  fBuf := nil;
  fBufSize := 0;
  {$ifdef MSWINDOWS}
  fMap := 0;
  {$endif MSWINDOWS}
  fFileLocal := aFileOwned;
  fFile := aFile;
  fFileSize := FileSeek64(fFile, 0, soFromEnd);
  if fFileSize = 0 then
  begin
    result := true; // handle 0 byte file without error (but no memory map)
    exit;
  end;
  result := false;
  if (fFileSize <= 0) {$ifdef CPU32} or (fFileSize > maxInt){$endif} then
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
  result := DoMap(aCustomOffset); // call actual Windows/POSIX map API
end;

procedure TMemoryMap.Map(aBuffer: pointer; aBufferSize: PtrUInt);
begin
  fBuf := aBuffer;
  fFileSize := aBufferSize;
  fBufSize := aBufferSize;
  {$ifdef MSWINDOWS}
  fMap := 0;
  {$endif MSWINDOWS}
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
  if PtrInt(F) < 0 then
    exit;
  if Map(F) then
    result := true
  else
    FileClose(F);
  fFileLocal := result;
end;

procedure TMemoryMap.UnMap;
begin
  DoUnMap; // call actual Windows/POSIX unmap API
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
  if (fBuf <> nil) and (fBufSize >= 3) then
    if PWord(fBuf)^ = $FEFF then
      result := isUnicode
    else if (PWord(fBuf)^ = $BBEF) and (PByteArray(fBuf)[2] = $BF) then
      result := isUTF8;
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



function GetDelphiCompilerVersion: RawUTF8;
begin
  result := COMPILER_VERSION;
end;

{$I-}

function ConsoleReadBody: RawByteString;
var
  len, n: integer;
  P: PByte;
  {$ifndef FPC}
  StdInputHandle: THandle;
  {$endif FPC}
begin
  result := '';
  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  {$endif FPC}
  if not PeekNamedPipe(StdInputHandle, nil, 0, nil, @len, nil) then
  {$else}
  if fpioctl(StdInputHandle, FIONREAD, @len) < 0 then
  {$endif MSWINDOWS}
    len := 0;
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

procedure ConsoleWrite(const Text: RawUTF8; Color: TConsoleColor;
  NoLineFeed, NoColor: boolean);
begin
  if not NoColor then
    TextColor(Color);
  write(Utf8ToConsole(Text));
  if not NoLineFeed then
    writeln;
  ioresult;
end;

{$I+}

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
  if (self = nil) or (Major or Minor or Release or Build = 0) then
    result := ''
  else
    result := fDetailed;
end;

function TFileVersion.VersionInfo: RawUTF8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fVersionInfo = '' then
      fVersionInfo := RawUTF8(Format('%s %s (%s)', [ExtractFileName(fFileName),
        DetailedOrVoid, BuildDateTimeString]));
    result := fVersionInfo;
  end;
end;

function TFileVersion.UserAgent: RawUTF8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fUserAgent = '' then
    begin
      fUserAgent := RawUTF8(Format('%s/%s%s', [GetFileNameWithoutExt(
        ExtractFileName(fFileName)), DetailedOrVoid, OS_INITIAL[OS_KIND]]));
      {$ifdef MSWINDOWS}
      if OSVersion in WINDOWS_32 then
        fUserAgent := fUserAgent + '32';
      {$endif MSWINDOWS}
    end;
    result := fUserAgent;
  end;
end;

class function TFileVersion.GetVersionInfo(const aFileName: TFileName): RawUTF8;
begin
  with Create(aFileName, 0, 0, 0, 0) do
  try
    result := VersionInfo;
  finally
    Free;
  end;
end;

procedure SetExecutableVersion(const aVersionText: RawUTF8);
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
  with ExeVersion do
  begin
    if Version = nil then
    begin
      {$ifdef MSWINDOWS}
      ProgramFileName := paramstr(0);
      {$else}
      ProgramFileName := GetModuleName(HInstance);
      if ProgramFileName = '' then
        ProgramFileName := ExpandFileName(paramstr(0));
      {$endif MSWINDOWS}
      ProgramFilePath := ExtractFilePath(ProgramFileName);
      if IsLibrary then
        InstanceFileName := GetModuleName(HInstance)
      else
        InstanceFileName := ProgramFileName;
      ProgramName := RawUTF8(GetFileNameWithoutExt(ExtractFileName(ProgramFileName)));
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
    ProgramFullSpec := RawUTF8(Format('%s %s (%s)', [ProgramFileName,
      Version.Detailed, Version.BuildDateTimeString]));
    Hash.c0 := Version.Version32;
    {$ifdef CPUINTEL}
    Hash.c0 := crc32c(Hash.c0, @CpuFeatures, SizeOf(CpuFeatures));
    {$endif CPUINTEL}
    Hash.c0 := crc32c(Hash.c0, pointer(Host), length(Host));
    Hash.c1 := crc32c(Hash.c0, pointer(User), length(User));
    Hash.c2 := crc32c(Hash.c1, pointer(ProgramFullSpec), length(ProgramFullSpec));
    Hash.c3 := crc32c(Hash.c2, pointer(InstanceFileName), length(InstanceFileName));
  end;
end;


{ **************** TSynLocker Threading Features }

function NewSynLocker: PSynLocker;
begin
  GetMem(result, SizeOf(TSynLocker));
  result^.Init;
end;


{ TAutoLock }

type
  /// used by TAutoLocker.ProtectMethod and TSynLocker.ProtectMethod
  TAutoLock = class(TInterfacedObject)
  protected
    fLock: PSynLocker;
  public
    constructor Create(aLock: PSynLocker);
    destructor Destroy; override;
  end;

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

procedure TSynLocker.Init;
begin
  fSectionPadding := 0;
  PaddingUsedCount := 0;
  InitializeCriticalSection(fSection);
  fLocked := false;
  fInitialized := true;
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

procedure TSynLocker.Lock;
begin
  EnterCriticalSection(fSection);
  fLocked := true;
end;

procedure TSynLocker.UnLock;
begin
  fLocked := false;
  LeaveCriticalSection(fSection);
end;

function TSynLocker.TryLock: boolean;
begin
  result := not fLocked and (TryEnterCriticalSection(fSection) <> 0);
end;

function TSynLocker.TryLockMS(retryms: integer): boolean;
begin
  repeat
    result := TryLock;
    if result or (retryms <= 0) then
      break;
    SleepHiRes(1);
    dec(retryms);
  until false;
end;

function TSynLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@self);
end;

function TSynLocker.GetVariant(Index: integer): Variant;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    result := variant(Padding[Index]);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    VarClear(result);
end;

procedure TSynLocker.SetVariant(Index: integer; const Value: Variant);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.GetInt64(Index: integer): Int64;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if not VariantToInt64(variant(Padding[Index]), result) then
      result := 0;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
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
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if not VariantToBoolean(variant(Padding[Index]), result) then
      result := false;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := false;
end;

procedure TSynLocker.SetBool(Index: integer; const Value: boolean);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetUnLockedInt64(Index: integer): Int64;
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
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    with Padding[Index] do
      if VType = varUnknown then
        result := VUnknown
      else
        result := nil;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := nil;
end;

procedure TSynLocker.SetPointer(Index: integer; const Value: Pointer);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    with Padding[Index] do
    begin
      if not (integer(VType) in VTYPE_SIMPLE) then
        VarClear(PVariant(@VType)^);
      VType := varUnknown;
      VUnknown := Value;
    end;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.GetUTF8(Index: integer): RawUTF8;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    VariantStringToUTF8(variant(Padding[Index]), result);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := '';
end;

procedure TSynLocker.SetUTF8(Index: integer; const Value: RawUTF8);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    RawUTF8ToVariant(Value, variant(Padding[Index]));
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    result := 0;
    if Index < PaddingUsedCount then
      VariantToInt64(variant(Padding[Index]), result)
    else
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Int64(result + Increment);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := 0;
end;

function TSynLocker.LockedExchange(Index: integer; const Value: Variant): Variant;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    with Padding[Index] do
    begin
      if Index < PaddingUsedCount then
        result := PVariant(@VType)^
      else
      begin
        PaddingUsedCount := Index + 1;
        VarClear(result);
      end;
      PVariant(@VType)^ := Value;
    end;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    VarClear(result);
end;

function TSynLocker.LockedPointerExchange(Index: integer; Value: pointer): pointer;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
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
    fLocked := false;
    LeaveCriticalSection(fSection);
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


procedure FinalizeUnit;
var
  i: PtrInt;
begin
  for i := 0 to high(AutoSlots) do
    FreeMem(AutoSlots[i]);
  ExeVersion.Version.Free;
  DeleteCriticalSection(AutoSlotsLock);
  DeleteCriticalSection(GlobalCriticalSection);
  {$ifdef MSWINDOWS}
  if CryptoAPI.Handle <> 0 then
    FreeLibrary(CryptoAPI.Handle);
  {$else}
  if pthread <> nil then
    dlclose(pthread);
  {$endif MSWINDOWS}
end;

initialization
  {$ifdef ISFPC27}
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$endif ISFPC27}
  InitializeCriticalSection(GlobalCriticalSection);
  InitializeCriticalSection(AutoSlotsLock);
  InitializeUnit; // in mormot.core.os.posix/windows.inc files
  SetExecutableVersion(0,0,0,0);

finalization
  FinalizeUnit;
end.

