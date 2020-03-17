/// Framework Core Low-Level Wrappers to the Operating-System API
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os;

{
  *****************************************************************************

  Cross-platform functions shared by all framework units
  - Gather Operating System Information
  - Operating System Specific Types (e.g. TWinRegistry)
  - Unicode, Time, File process

  Aim of this unit is to centralize most used OS-specific API calls, like a
  SysUtils unit on steroids, to avoid using $ifdef/$endif in "uses" clauses.
  In practice, no "Windows", nor "Linux/Unix" reference should be needed in
  regular units, once mormot.core.os is included.

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
  /// the recognized operating systems
  // - it will also recognize most Linux distributions
  TOperatingSystem = (osUnknown, osWindows, osLinux, osOSX, osBSD, osPOSIX,
    osArch, osAurox, osDebian, osFedora, osGentoo, osKnoppix, osMint, osMandrake,
    osMandriva, osNovell, osUbuntu, osSlackware, osSolaris, osSuse, osSynology,
    osTrustix, osClear, osUnited, osRedHat, osLFS, osOracle, osMageia, osCentOS,
    osCloud, osXen, osAmazon, osCoreOS, osAlpine, osAndroid);

  /// the recognized Windows versions
  // - defined even outside MSWINDOWS to allow process e.g. from monitoring tools
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
  OSVersionInt32: integer absolute OSVersion32;

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
  /// the current Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;
  /// the current Operating System version, as retrieved for the current process
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

/// return the Delphi/FPC Compiler Version
// - returns 'Delphi 10.3 Rio', 'Delphi 2010' or 'Free Pascal 3.3.1' e.g.
function GetCompilerVersion: RawUTF8;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{$ifdef MSWINDOWS}

type
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
    function ReadOpen(root: HKEY; const keyname: RawUTF8; closefirst: boolean=false): boolean;
    /// finalize low-level read access to the Windows Registry after ReadOpen()
    procedure Close;
    /// low-level read a string from the Windows Registry after ReadOpen()
    // - in respect to Delphi's TRegistry, will properly handle REG_MULTI_SZ
    // (return the first value of the multi-list)
    function ReadString(const entry: SynUnicode; andtrim: boolean=true): RawUTF8;
    /// low-level read a Windows Registry content after ReadOpen()
    // - works with any kind of key, but was designed for REG_BINARY
    function ReadData(const entry: SynUnicode): RawByteString;
    /// low-level read a Windows Registry 32-bit REG_DWORD value after ReadOpen()
    function ReadDword(const entry: SynUnicode): cardinal;
    /// low-level read a Windows Registry 64-bit REG_QWORD value after ReadOpen()
    function ReadQword(const entry: SynUnicode): QWord;
    /// low-level enumeration of all sub-entries names of a Windows Registry key
    function ReadEnumEntries: TRawUTF8DynArray;
  end;

{$endif MSWINDOWS}


{ ****************** Unicode, Time, File process }

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
function DeleteFile(const aFileName: TFileName): boolean;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function RenameFile(const OldName, NewName: TFileName): boolean;

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

/// returns the current UTC time as TSystemTime
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// - under Windows, directly call the homonymous Win32 API
procedure GetSystemTime(out result: TSystemTime);
  {$ifdef MSWINDOWS} stdcall; {$endif}


/// compatibility function, wrapping GetACP() Win32 API function
// - returns the curent system code page (default WinAnsi)
function os_CodePage: integer;

/// compatibility function, wrapping CompareStringW() Win32 API text comparison
// - returns 1 if PW1>PW2, 2 if PW1=PW2, 3 if PW1<PW2 - so substract 2 to have
// -1,0,1 as regular StrCompW/StrICompW comparison function result
// - will compute StrLen(PW1/PW2) if L1 or L2 < 0
// - somewhat slow by using two temporary UnicodeString on POSIX - but seldom
// called, unless our proprietary WIN32CASE collation is used in SynSQLite3
function os_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: Boolean): integer;

/// compatibility function, wrapping MultiByteToWideChar() Win32 API call
// - returns the number of WideChar written into W^ destination buffer
function os_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;

/// compatibility function, wrapping WideCharToMultiByte() Win32 API call
// - returns the number of AnsiChar written into A^ destination buffer
function os_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;

/// returns a system-wide current monotonic timestamp as milliseconds
// - will use the corresponding native API function under Vista+, or will be
// redirected to a custom wrapper function for older Windows versions (XP)
// to avoid the 32-bit overflow/wrapping issue of GetTickCount
// - warning: FPC's SysUtils.GetTickCount64 or TThread.GetTickCount64 don't
// handle properly 49 days wrapping under XP -> always use this safe version
// - on POSIX, will call (via vDSO) the very fast CLOCK_MONOTONIC_COARSE if available
// - do not expect extact millisecond resolution - it may rather be within the
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

/// returns a high-resolution system-wide monotonic timestamp as microseconds
// - under Linux/POSIX, has true microseconds resolution, calling e.g.
// CLOCK_MONOTONIC on Linux/BSD
// - under Windows, calls QueryPerformanceCounter / QueryPerformanceFrequency
procedure QueryPerformanceMicroSeconds(out Value: Int64);

/// get a file date and time, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// get a file size, from its name
// - returns 0 if file doesn't exist
// - under Windows, will use GetFileAttributesEx fast API
function FileSize(const FileName: TFileName): Int64; overload;

/// get a file size, from its handle
// - returns 0 if file doesn't exist
function FileSize(F: THandle): Int64; overload;

/// get low-level file information, in a cross-platform way
// - returns true on success
// - here file write/creation time are given as TUnixMSTime values, for better
// cross-platform process - note that FileCreateDateTime may not be supported
// by most Linux file systems, so the oldest timestamp available is returned
// as failover on such systems (probably the latest file metadata writing)
function FileInfoByHandle(aFileHandle: THandle; out FileId, FileSize,
  LastWriteAccess, FileCreateDateTime: Int64): Boolean;

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
// - under POSIX, calls plain FileOpen(FileName,fmOpenRead or fmShareDenyNone)
// - is used e.g. by StringFromFile() and TSynMemoryStreamMapped.Create()
function FileOpenSequentialRead(const FileName: string): Integer;
  {$ifdef HASINLINE}inline;{$endif}

/// returns a TFileStream optimized for one pass file reading
// - will use FileOpenSequentialRead(), i.e. FILE_FLAG_SEQUENTIAL_SCAN on Windows
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


{ ****************** Gather Operating System Information }

function GetCompilerVersion: RawUTF8;
begin
  result :=
{$ifdef FPC}
  'Free Pascal'
  {$ifdef VER2_6_4} + ' 2.6.4'{$endif}
  {$ifdef VER3_0_0} + ' 3.0.0'{$endif}
  {$ifdef VER3_0_1} + ' 3.0.1'{$endif}
  {$ifdef VER3_0_2} + ' 3.0.2'{$endif}
  {$ifdef VER3_1_1} + ' 3.1.1'{$endif}
  {$ifdef VER3_2}   + ' 3.2'  {$endif}
  {$ifdef VER3_3_1} + ' 3.3.1'{$endif}
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
{$ifdef CPU64} +' 64 bit' {$else} +' 32 bit' {$endif}
end;



{ ****************** Unicode, Time, File process }

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

function os_CodePage: integer;
begin
  result := GetACP;
end;

function os_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: Boolean): integer;
const
  _CASEFLAG: array[boolean] of DWORD = (0, NORM_IGNORECASE);
begin
  result := CompareStringW(LOCALE_USER_DEFAULT, _CASEFLAG[IgnoreCase], PW1, L1, PW2, L2);
end;

function NowUTC: TDateTime;
begin
  result := UnixMSTimeUTC / MSecsPerDay + UnixDelta;
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

function FileStreamSequentialRead(const FileName: string): THandleStream;
begin
  result := THandleStream.Create(FileOpenSequentialRead(FileName));
end;

function StringFromFile(const FileName: TFileName; HasNoSize: boolean): RawByteString;
var
  F: THandle;
  Read, Size, Chunk: integer;
  P: PUTF8Char;
  tmp: array[0..$7fff] of AnsiChar;
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
          {$ifdef MSWINDOWS} // FILE_FLAG_SEQUENTIAL_SCAN has limits on XP
          if Chunk > 32 shl 20 then
            Chunk := 32 shl 20; // avoid e.g. ERROR_NO_SYSTEM_RESOURCES
          {$endif}
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

initialization
  InitializeUnit; // in the mormot.core.os.*.inc files
  {$ifdef ISFPC27}
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$endif ISFPC27}
end.

