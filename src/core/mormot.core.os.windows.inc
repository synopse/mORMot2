{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Windows API calls for FPC/Delphi, as used by mormot.core.os.pas
}


{ ****************** Unicode, Time, File process }

procedure DoWin32PWideCharToUtf8(P: PWideChar; Len: PtrInt; var res: RawUtf8);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Len * 3);
  Len := UnicodeToUtf8(tmp.Buf, Len * 3 + 16, P, Len); // use RTL if complex
  if Len > 0 then
    dec(Len); // UnicodeToUtf8() result includes the null terminator
  FastSetString(res, tmp.buf, Len);
  tmp.Done;
end; // warning: Delphi 7/2007 UnicodeToUtf8() RTL doesn't handle surrogates

function IsExtendedPathName(Name: PWideChar): boolean;
begin
  result := (Name <> nil) and
            (Name[0] = '\') and
            (Name[1] = '\') and
            (Name[2] = '?') and
            (Name[3] = '\');
end;

procedure ExtendedPathName(Name: PWideChar; Len: PtrInt; var Temp: TW32Temp);
var
  fp: PWideChar;
begin
  Len := Len * 2 + 2; // in bytes, +2 to include ending #0
  if (Len <= MAX_PATH * 2) or
     ((Len <= SizeOf(Temp) - 8) and
      IsExtendedPathName(Name)) then
  begin
    MoveFast(Name^, Temp[0], Len); // we can use the supplied UTF-16 file name
    exit;
  end;
  // need to switch to extended-length path
  Temp[0] := #0;
  if Len > SizeOf(Temp) - 8 then
    exit; // avoid buffer overflow
  // append the full path name to the \\?\ prefix
  Temp[0] := '\';
  Temp[1] := '\';
  Temp[2] := '?';
  Temp[3] := '\';
  if ((ord(Name[0]) in [ord('A')..ord('Z'), ord('a')..ord('z')]) and
      (Name[1] = ':')) or
     (GetFullPathNameW(Name, high(Temp) - 4, @Temp[4], fp) = 0) then
    MoveFast(Name^, Temp[4], Len);
end;

{$ifdef UNICODE}

function W32(const FileName: TFileName; var Temp: TW32Temp; DoCopy: boolean): PWideChar;
var
  len: PtrInt;
begin
  result := pointer(FileName);
  len := length(FileName);
  if (len = 0) or
     ((len < MAX_PATH) and
      not DoCopy) then
    exit; // most common case could direclty use FileName[] UTF-16 content
  ExtendedPathName(pointer(FileName), len, Temp);
  result := @Temp;
end;

procedure Win32PWideCharToFileName(P: PWideChar; out fn: TFileName);
begin
  SetString(fn, P, StrLenW(P)); // TFileName is UnicodeString
end;

{$else}

procedure W32Convert(const FileName: TFileName; var Temp: TW32Temp);
var
  u: SynUnicode;
begin
  u := SynUnicode(FileName); // let the RTL do the conversion
  ExtendedPathName(pointer(u), length(u), Temp);
end;

function W32(const FileName: TFileName; var Temp: TW32Temp; DoCopy: boolean): PWideChar;
var
  i, len: PtrInt;
begin
  result := nil;
  if FileName = '' then
    exit;
  len := length(FileName);
  if (len < MAX_PATH) and
     IsAnsiCompatible(pointer(FileName), len) then
    // most common cases do not need any Unicode conversion
    for i := 0 to len do // includes #0 terminator
      PWordArray(@Temp)[i] := PByteArray(FileName)[i]
  else
    // use a temporary SynUnicode variable for complex UTF-16 conversion
    // or if MAX_PATH is reached and \\?\ prefix is needed for extended-length
    W32Convert(FileName, Temp);
  result := @Temp;
end;

procedure Win32PWideCharToFileNameConv(P: PWideChar; L: PtrInt; var fn: TFileName);
var
  u: SynUnicode;
begin
  SetString(u, P, l);
  fn := TFileName(u); // let the RTL do the conversion
end;

procedure Win32PWideCharToFileName(P: PWideChar; out fn: TFileName);
var
  l, i: PtrInt;
begin
  l := StrLenW(P);
  if IsAnsiCompatibleW(P, l) then // most common case
  begin
    SetString(fn, nil, l);
    for i := 0 to l - 1 do
      PByteArray(fn)[i] := PWordArray(P)[i]; // fast direct conversion
  end
  {$ifdef FPC}
  else if DefaultRTLFileSystemCodePage = CP_UTF8 then // happens with Lazarus
    DoWin32PWideCharToUtf8(P, l, RawUtf8(fn)) // use RTL UnicodeToUtf8()
  {$endif FPC}
  else
    Win32PWideCharToFileNameConv(P, l, fn);
end;

{$endif UNICODE}

function _fmt(const Fmt: string; const Args: array of const): RawUtf8; overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure _fmt(const Fmt: string; const Args: array of const;
  var result: RawUtf8); overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

// local RTL wrapper functions to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; Len: PtrInt; out res: RawUtf8);
var
  i: PtrInt;
begin
  if Len > 0 then
    if IsAnsiCompatibleW(P, Len) then
    begin
      FastSetString(res, Len);
      for i := 0 to Len - 1 do
        PByteArray(res)[i] := PWordArray(P)[i]; // fast direct conversion
    end
    else
      DoWin32PWideCharToUtf8(P, Len, res); // use RTL UnicodeToUtf8()
end;

procedure Win32PWideCharToUtf8(P: PWideChar; out res: RawUtf8);
begin
  if P <> nil then
    Win32PWideCharToUtf8(P, StrLenW(P), res);
end;

function Utf8ToWin32PWideChar(const u: RawUtf8; var d: TSynTempBuffer): PWideChar;
begin
  result := Unicode_FromUtf8(pointer(u), length(u), d); // call the RTL
end;

const
  DefaultCharVar: AnsiChar = '?';

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
var
  flags: cardinal;
begin
  flags := 0;
  if CodePage < 50220 then // wild guess - FPC RTL would always use 0 anyway
    if CodePage = 951 then // 951 is a "fake" code page
      CodePage := 950
    else if (CodePage <> 932) and (CodePage <> 949) then
      flags := MB_PRECOMPOSED; // force no diaresis in output
  result := MultiByteToWideChar(CodePage, flags, A, LA, W, LW);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
var
  defchar: PAnsiChar;
begin
  defchar := nil;
  if CodePage < 50220 then // wild guess - FPC RTL would always use 0 anyway
    if CodePage = 951 then // 951 is a "fake" code page
      CodePage := 950
    else if (CodePage <> 932) and (CodePage <> 949) then
      defchar := @DefaultCharVar;
  result := WideCharToMultiByte(CodePage, 0, W, LW, A, LA, defchar, nil);
end;

function Win32Utf8ToAnsi(P: pointer; L, CP: integer; var A: RawByteString): boolean;
var
  tmpW, tmpA: TSynTempBuffer;
begin
  result := false;
  tmpW.Init(L * 2);
  L := Utf8ToUnicode(tmpW.Buf, L + 8, P, L); // use the RTL
  if L > 0 then // valid UTF-8
  begin
    dec(L); // Utf8ToUnicode() included trailing #0
    tmpA.Init(L * 3);
    L := Unicode_WideToAnsi(tmpW.buf, tmpA.buf, L, tmpA.len + 16, CP);
    if L > 0 then
    begin
      FastSetStringCP(A, tmpA.buf, L, CP); // keep A untouched on failure
      result := true; // success
    end;
    tmpA.Done;
  end;
  tmpW.Done;
end;

function LibraryOpen(const LibraryName: TFileName): TLibHandle;
var
  tmp: TW32Temp;
  err: DWord;
  {$ifdef CPUX86}
  x87cw: word;
  {$endif CPUX86}
begin
  // note: GetErrorMode() is not available on XP
  err := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  {$ifdef CPUX86}
  asm
      fnstcw x87cw // save x87 flags
  end;
  {$endif CPUX86}
  result := Windows.LoadLibraryW(W32(LibraryName, tmp));
  {$ifdef CPUX86}
  asm
      fnclex       // clear pending x87 exceptions
      fldcw x87cw  // restore flags (Visual C++ librairies usually change them)
  end;
  {$endif CPUX86}
  SetErrorMode(err);
end;

procedure LibraryClose(Lib: TLibHandle);
begin
  if pointer(Lib) <> nil then
    Windows.FreeLibrary(Lib);
end;

// Delphi Unicode has an ambiguous GetProcAddress() overload with PWideChar
function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
  external kernel32 name 'GetProcAddress'; // this is an Ansi-only API

function LibraryError: string;
begin
  result := IntToStr(GetLastError); // enough for basic troubleshouting
end;

procedure FileTimeToInt64(const FT: TFileTime; out I64: Int64);
  {$ifdef HASINLINE} inline; {$endif} 
begin
  PInt64Rec(@I64)^.Lo := FT.dwLowDateTime; // Delphi 2007 bug with PInt64()
  PInt64Rec(@I64)^.Hi := FT.dwHighDateTime;
end;

const
  MilliSecsPerFileTime = 10000;
  SecsPerFileTime      = 10000000;

procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);
begin
  I64 := (I64 * SecsPerFileTime) + UnixFileTimeDelta;
  FT.dwLowDateTime  := PInt64Rec(@I64)^.Lo; // Delphi 2007 bug with PInt64()
  FT.dwHighDateTime := PInt64Rec(@I64)^.Hi;
end;

procedure UnixMSTimeToFileTime(I64: TUnixMSTime; out FT: TFileTime);
begin
  I64 := (I64 * MilliSecsPerFileTime) + UnixFileTimeDelta;
  FT.dwLowDateTime  := PInt64Rec(@I64)^.Lo; // Delphi 2007 bug with PInt64()
  FT.dwHighDateTime := PInt64Rec(@I64)^.Hi;
end;

// To account for daylight saving time when converting a file time to a local
// time, use the following function in place of using FileTimeToLocalFileTime:
// https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-filetimetolocalfiletime

function FileTimeToLocalSystemTime(const ft: FILETIME; out lt: TSystemTime): boolean;
var
  st: TSystemTime;
begin
  FillCharFast(lt, SizeOf(lt), 0);
  result := FileTimeToSystemTime(ft, st);
  if result then
    if not SystemTimeToTzSpecificLocalTime(nil, st, lt) then
      lt := st;
end;

procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);
var
  lft: TFileTime;
begin
  UnixTimeToFileTime(I64, lft);
  FileTimeToLocalSystemTime(lft, Local); // no FileTimeToLocalFileTime
end;

function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  if PInt64(@FT)^ = 0 then
  begin
    result := 0;
    exit;
  end;
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div SecsPerFileTime;
  {$else} // use PInt64 to avoid URW699 with Delphi 6 / Kylix
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div SecsPerFileTime;
  {$endif CPU64}
end;

function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  if PInt64(@FT)^ = 0 then
  begin
    result := 0;
    exit;
  end;
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div MilliSecsPerFileTime;
  {$else} 
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div MilliSecsPerFileTime;
  {$endif CPU64}
end;

function FileTimeToDateTime(const FT: TFileTime): TDateTime;
begin
  if PInt64(@FT)^ = 0 then
    result := 0
  else // inlined UnixTimeToDateTime()
    result := FileTimeToUnixMSTime(FT) / MilliSecsPerDay + UnixDateDelta;
end;

procedure DateTimeToFileTime(dt: TDateTime; out FT: TFileTime);
begin
  if dt = 0 then
    PInt64(@FT)^ := 0
  else // inlined DateTimeToUnixTime()
    UnixTimeToFileTime(Round((dt - UnixDateDelta) * SecsPerDay), FT);
end;

function UnixTimeUtc: TUnixTime;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // fast (HW resolution is < TUnixTime second)
  result := FileTimeToUnixTime(ft);
end;

var
  // redirect to a slower but more accurate API available since Windows 8
  // - points to GetSystemTimeAsFileTime() before Windows 8
  GetSystemTimePreciseAsFileTime: procedure(var ft: TFILETIME); stdcall;

function UnixMSTimeUtc: TUnixMSTime;
var
  ft: TFileTime;
begin
  GetSystemTimePreciseAsFileTime(ft); // slower, but try to achieve ms resolution
  result := FileTimeToUnixMSTime(ft);
end;

function UnixMSTimeUtcFast: TUnixMSTime;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // faster, but with HW interupt resolution
  result := FileTimeToUnixMSTime(ft);
end;

procedure GetSystemTime;              external kernel32;
procedure GetLocalTime;               external kernel32;
procedure InitializeCriticalSection;  external kernel32;
procedure EnterCriticalSection;       external kernel32;
procedure LeaveCriticalSection;       external kernel32;
procedure DeleteCriticalSection;      external kernel32;
function  TryEnterCriticalSection;    external kernel32;
function  CloseHandle;                external kernel32;
procedure FileClose;                  external kernel32 name 'CloseHandle';
function  GetCurrentThreadId;         external kernel32;
procedure SwitchToThread;             external kernel32;
function  GetCurrentProcessId;        external kernel32;
function  GetCurrentProcess;          external kernel32;
function  WaitForSingleObject;        external kernel32;
function  GetEnvironmentStringsW;     external kernel32;
function  FreeEnvironmentStringsW;    external kernel32;
function  RtlCaptureStackBackTrace;   external kernel32;
function  IsDebuggerPresent;          external kernel32;
procedure SetEndOfFile;               external kernel32;
procedure FlushFileBuffers;           external kernel32;
function  GetLastError;               external kernel32;
procedure SetLastError;               external kernel32;
function  IocpCreate;                 external kernel32 name 'CreateIoCompletionPort';
function  IocpGetQueuedStatus;        external kernel32 name 'GetQueuedCompletionStatus';
function  IocpPostQueuedStatus;       external kernel32 name 'PostQueuedCompletionStatus';
function  GetDesktopWindow;           external user32;
function  Unicode_InPlaceUpper;       external user32 name 'CharUpperBuffW';
function  Unicode_InPlaceLower;       external user32 name 'CharLowerBuffW';

var
  ConsoleCriticalSection: TOSLock; // too early to use TOSLightLock
  ConsoleOutputCP: cardinal;       // to call the Windows API once
  {$ifdef ISDELPHI}
  StdInputHandle: THandle;         // Delphi doesn't define this global variable
  {$endif ISDELPHI}

function HasConsole: boolean;
begin
  if StdOut = 0 then
    StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  result := (StdOut <> 0) and
            (StdOut <> INVALID_HANDLE_VALUE);
end;

procedure AllocConsole;
begin
  ConsoleOutputCP := 0; // reset to force calling GetConsoleOutputCP() again
  Windows.AllocConsole;
  if (StdOut = 0) or
     (StdOut = INVALID_HANDLE_VALUE) then
    // force setup StdOut global variable
    StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
end;

procedure ConsoleErrorWrite(const text: RawUtf8);
var
  err: THandle;
  txt: RawByteString;
begin
  // better than a MessageBox() especially for services
  if not HasConsole then
    AllocConsole; // will create one black window console if none
  err := GetStdHandle(STD_ERROR_HANDLE);
  if (err = INVALID_HANDLE_VALUE) or
     (err = 0) then
    exit;
  ConsoleOutputCP := 0; // reset to force calling GetConsoleOutputCP() again
  txt := Utf8ToConsole(text); // convert to GetConsoleOutputCP() code page
  FileWriteAll(err, pointer(txt), length(txt));
end;

function IsSharedViolation(ErrorCode: integer): boolean;
begin
  if ErrorCode = 0 then
    ErrorCode := GetLastError;
  result := ErrorCode in [ERROR_SHARING_VIOLATION, ERROR_LOCK_VIOLATION];
end;

function GetModuleHandle(lpModuleName: PChar): HMODULE;
begin
  result := Windows.GetModuleHandle(lpModuleName); // call either A or W API
end;

function SetSystemTime(const utctime: TSystemTime): boolean;
var
  privileges: TSynWindowsPrivileges;
begin
  try
    privileges.Init;
    try
      privileges.Enable(wspSystemTime); // ensure has SE_SYSTEMTIME_NAME
      result := Windows.SetSystemTime(PSystemTime(@utctime)^);
    finally
      privileges.Done;
    end;
    if result then
      PostMessage(HWND_BROADCAST, WM_TIMECHANGE, 0, 0); // notify the apps
  except
    result := false;
  end;
end;

// PUtf8Char for system error text reduces the executable size vs RawUtf8
// on Delphi (aligned to 4 bytes), but not on FPC (aligned to 16 bytes)
// -> use an enumeration and minimal RTTI support

const
  NULL_STR: string[1] = '';

function WinGetEnumName(Info: PAnsiChar; Value: integer): PShortString;
begin
  // minimal version with no Kind, EnumBaseType nor Value min/max check
  result := @NULL_STR;
  // no Windows arm32 support yet - see fpc_shortstr_enum_intern() in sstrings.inc
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  if Value < 0 then
    exit;
  // quickly jump over Kind + NameLen + Name + Min + Max + EnumBaseType
  Info := @Info[ord(Info[1]) + (2 + 9 + SizeOf(pointer))];
  if Value > 0 then
    repeat
      Info := @Info[ord(Info^) + 1]; // next shortstring
      dec(Value);
    until Value = 0;
  result := pointer(Info);
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
  // searched in WINERR_ONE[] constants
  TWinErrorOne = (
    ALREADY_EXISTS, MORE_DATA, NO_SYSTEM_RESOURCES,
    WSAEFAULT, WSAEINVAL, WSAEMFILE, WSAEWOULDBLOCK, WSAENOTSOCK, WSAECONNABORTED,
    WSAECONNRESET, WSAENOBUFS, WSAETIMEDOUT, WSAECONNREFUSED, WSATRY_AGAIN,
    WINHTTP_TIMEOUT, WINHTTP_OPERATION_CANCELLED, WINHTTP_CANNOT_CONNECT,
    WINHTTP_CLIENT_AUTH_CERT_NEEDED, WINHTTP_INVALID_SERVER_RESPONSE,
    CRYPT_E_BAD_ENCODE, CRYPT_E_SELF_SIGNED, CRYPT_E_BAD_MSG, CRYPT_E_REVOKED,
    CRYPT_E_NO_REVOCATION_CHECK, CRYPT_E_REVOCATION_OFFLINE, TRUST_E_BAD_DIGEST,
    TRUST_E_NOSIGNATURE, CERT_E_EXPIRED, CERT_E_CHAINING, CERT_E_REVOKED);
const
  WINERR_ONE: array[TWinErrorOne] of cardinal = (
    // sparse system errors (183, 234, 1450)
    ERROR_ALREADY_EXISTS, ERROR_MORE_DATA, ERROR_NO_SYSTEM_RESOURCES,
    // main Windows Socket API (WSA) errors
    10014, 10022, 10024, 10035, 10038, 10053, 10054, 10055, 10060, 10061, 11003,
    // most common WinHttp API errors (in range 12000...12152)
    ERROR_WINHTTP_TIMEOUT, ERROR_WINHTTP_OPERATION_CANCELLED,
    ERROR_WINHTTP_CANNOT_CONNECT, ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED,
    ERROR_WINHTTP_INVALID_SERVER_RESPONSE,
    // some security-related HRESULT errors
    $80092002, $80092007, $8009200d, $80092010, $80092012, $80092013, $80096010,
    $800b0100, $800b0101, $800b010a, $800b010c);

function WinErrorConstant(Code: cardinal): PShortString;
begin
  case Code of // split into TWinError* types (faster and easier to maintain)
    0 .. ord(high(TWinError0)):
      result := WinGetEnumName(TypeInfo(TWinError0), Code);
    50 .. 50 + ord(high(TWinError50)):
      result := WinGetEnumName(TypeInfo(TWinError50), Code - 50);
    80 .. 80 + ord(high(TWinError80)):
      result := WinGetEnumName(TypeInfo(TWinError80), Code - 80);
    108 .. 108 + ord(high(TWinError108)):
      result := WinGetEnumName(TypeInfo(TWinError108), Code - 108);
    995 .. 995 + ord(high(TWinError995)):
      result := WinGetEnumName(TypeInfo(TWinError995), Code - 995);
    1051 .. 1051 + ord(high(TWinError1051)):
      result := WinGetEnumName(TypeInfo(TWinError1051), Code - 1051);
    1200 .. 1200 + ord(high(TWinError1200)):
      result := WinGetEnumName(TypeInfo(TWinError1200), Code - 1200);
  else
    result := WinGetEnumName(TypeInfo(TWinErrorOne),
      IntegerScanIndex(@WINERR_ONE, length(WINERR_ONE), Code));
  end;
end;

function WinErrorText(Code: cardinal; ModuleName: PChar): RawUtf8;
var
  bak: integer;
  flags, len: PtrUInt;
  src: pointer;
  cod: PShortString;
  tmp: array[0..511] of WideChar;
  tmps: shortstring absolute tmp;
begin
  bak := GetLastError;
  src := nil;
  flags := FORMAT_MESSAGE_FROM_SYSTEM;
  if ModuleName = nil then
  begin
    // system error codes
    cod := WinErrorConstant(Code);
    if cod^[0] <> #0 then
    begin
      // we can return directly the standard system error code constant
      tmps := 'ERROR_';
      AppendShort(cod^, tmps);
      FastSetString(result, @tmps[1], ord(tmps[0]));
      SetLastError(bak);
      exit;
    end;
  end
  else
  begin
    // module specific error codes
    src := pointer(GetModuleHandle(ModuleName));
    if src <> nil then
      flags := FORMAT_MESSAGE_FROM_HMODULE;
  end;
  // first try if there is an English message version of this error code
  len := FormatMessageW(flags, src, Code, ENGLISH_LANGID, @tmp, SizeOf(tmp), nil);
  if len <= 0 then
    // typically ERROR_RESOURCE_LANG_NOT_FOUND or ERROR_MUI_FILE_NOT_FOUND
    len := FormatMessageW(flags, src, Code, 0, @tmp, SizeOf(tmp), nil);
  if (len <= 0) and
     (src <> nil) then
  begin
    // fallback to the system error message if this module as no such code
    SetLastError(bak);
    result := WinErrorText(Code, nil);
    exit;
  end;
  while (len > 0) and
        (ord(tmp[len - 1]) in [0..32, ord('.')]) do
    dec(len); // trim right
  Win32PWideCharToUtf8(@tmp, len, result);
  SetLastError(bak);
end;

function GetErrorText(error: integer): RawUtf8;
begin
  result := WinErrorText(error, nil);
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var
  code: integer;
begin
  code := GetLastError;
  raise ModuleException.CreateFmt('%s error [%s] (0x%x)',
    [ModuleName, string(WinErrorText(code, ModuleName)), code]);
end;

function WinLastError(const Context: shortstring; Code: integer): string;
begin
  if Code = 0 then
    Code := GetLastError;
  result := Format('%s error [%s] (0x%x)',
    [Context, string(WinErrorText(Code, nil)), code]);
end;

procedure RaiseLastError(const Context: shortstring;
  RaisedException: ExceptClass; Code: integer);
begin
  if RaisedException = nil then
    RaisedException := EOSException;
  raise RaisedException.Create(WinLastError(Context, Code))
end;

procedure WinCheck(const Context: shortstring; Code: integer;
  RaisedException: ExceptClass);
begin
  if Code <> NO_ERROR then
    RaiseLastError(Context, RaisedException, Code);
end;

function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  result := Windows.PostMessage(hWnd, Msg, wParam, lParam); // call either A or W API
end;

function ExpandEnvVars(const aStr: string): string;
// adapted from http://delphidabbler.com/articles?article=6
var
  size: integer;
begin
  // Get required buffer size
  size := ExpandEnvironmentStrings(pointer(aStr), nil, 0);
  if size > 0 then
  begin
    // Read expanded string into result string (calling the A or W API)
    SetString(result, nil, size - 1);
    ExpandEnvironmentStrings(pointer(aStr), pointer(result), size);
  end
  else
    result := aStr; // return the original file name
end;

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  result := not IsZero(@cs, SizeOf(cs));
end;

var
  // value is documented as stable after boot, so we get it at startup
  _QueryPerformanceFrequency: QWord;
  // from HyperV or if HPET disabled e.g. -> direct division
  _QueryPerformanceFrequencyPer10: boolean;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  v: Int64; // for proper alignment on some old Delphi revisions + Win32
begin
  QueryPerformanceCounter(v);
  if _QueryPerformanceFrequencyPer10 then
    Value := QWord(v) div 10 // faster div by a constant (especially on FPC_64)
  else
    Value := QWord((QWord(v) * MicroSecsPerSec) div _QueryPerformanceFrequency);
end;

var
  shlwapiDll: THandle; // lazy loading (only by TNetClientProtocolFile)
  PathCreateFromUrlW: function(pszUrl, pszPath: PWideChar;
    var pcchPath: cardinal; dwFlags: cardinal): HRESULT; stdcall;

function GetFileNameFromUrl(const Uri: RawUtf8): TFileName;
var
  len: DWORD;
  u: TSynTempBuffer;
  fn: array[0..MAX_PATH] of WideChar;
begin
  result := '';
  Utf8ToWin32PWideChar(Uri, u);
  len := MAX_PATH;
  if DelayedProc(PathCreateFromUrlW, shlwapiDll, 'shlwapi.dll',
       'PathCreateFromUrlW') and
     (PathCreateFromUrlW(u.buf, @fn, len, 0) = S_OK) then
    Win32PWideCharToFileName(fn, result);
  u.Done;
end;

function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
begin
  result := WindowsFileTimeToDateTime(FileDate);
end;

// some definitions missing on oldest Delphi
const
  FILE_ATTRIBUTE_REPARSE_POINT = $0000400; // = faSymLink

function FindFirstFileExW(lpfilename: PWideChar; fInfoLevelId: FINDEX_INFO_LEVELS;
   lpFindFileData: pointer; fSearchOp: FINDEX_SEARCH_OPS;
   lpSearchFilter: pointer = nil; dwAdditionalFlags: cardinal = 0): THandle;
  stdcall; external kernel32;

// an alternative to GetFileAttributesExW() with fallback to FindFirstFileEx API
function GetFileAttributesRaw(fn: PWideChar;
  out Attr: WIN32_FILE_ATTRIBUTE_DATA): boolean;
var
  h: THandle;
  fd: TWin32FindDataW;
begin
  // this API is much faster than CreateFile/GetFileTime/GetFileSize/CloseHandle
  result := GetFileAttributesExW(fn, GetFileExInfoStandard, @Attr);
  if result or
     (GetLastError in [ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND,
        ERROR_INVALID_NAME, ERROR_INVALID_DRIVE, ERROR_NOT_READY,
        ERROR_INVALID_PARAMETER, ERROR_BAD_PATHNAME, ERROR_BAD_NETPATH,
        ERROR_BAD_NET_NAME]) then
    exit;
  // access denied, or locked file: fallback to slower but regular API
  h := FindFirstFileExW(fn, FindExInfoStandard, @fd, FindExSearchNameMatch);
  if not ValidHandle(h) then
    exit;
  windows.FindClose(h);
  Attr.dwFileAttributes := fd.dwFileAttributes;
  Attr.ftCreationTime   := fd.ftCreationTime;
  Attr.ftLastAccessTime := fd.ftLastAccessTime;
  Attr.ftLastWriteTime  := fd.ftLastWriteTime;
  Attr.nFileSizeHigh    := fd.nFileSizeHigh;
  Attr.nFileSizeLow     := fd.nFileSizeLow;
  result := true;
end;

function GetFileAttributesInternal(const FileName: TFileName;
  out Attr: WIN32_FILE_ATTRIBUTE_DATA; FollowLink: boolean = true): boolean;
var
  fn: PWideChar;
  h: THandle;
  f: cardinal;
  lp: TByHandleFileInformation;
  tmp: TW32Temp;
begin
  result := false;
  if FileName = '' then
    exit;
  fn := W32(FileName, tmp);
  result := GetFileAttributesRaw(fn, Attr);
  if result and
     FollowLink and
     (Attr.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) then
  begin
    // we need to follow a symbolic link
    f := 0;
    if Attr.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
      f := FILE_FLAG_BACKUP_SEMANTICS; // to access folder handle
    FillCharFast(Attr, SizeOf(Attr), 0); // enough for FileExists()
    // raw file access seems better than FileGetSymLinkTarget() in our case
    // and it will be consistent on both FPC and Delphi (including pre-Unicode)
    // - if we require file information, it is likely we would like to access it
    // - note that FPC and Delphi RTL seems overcomplicated and non-consistent
    // about symbolic links: mORMot will share this function everywhere
    h := CreateFileW(fn, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, f, 0);
    if ValidHandle(h) then
    begin
      if GetFileInformationByHandle(h, lp) then
      begin
        Attr.dwFileAttributes := lp.dwFileAttributes;
        Attr.ftCreationTime   := lp.ftCreationTime;
        Attr.ftLastAccessTime := lp.ftLastAccessTime;
        Attr.ftLastWriteTime  := lp.ftLastWriteTime;
        Attr.nFileSizeHigh    := lp.nFileSizeHigh;
        Attr.nFileSizeLow     := lp.nFileSizeLow;
      end
      else
        result := false;
      CloseHandle(h);
    end
    else
      result := IsSharedViolation;
  end;
end;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
  systim, loctim: TSystemTime;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, fa) and
     FileTimeToSystemTime({%H-}fa.ftLastWriteTime, systim) and
     SystemTimeToTzSpecificLocalTime(nil, systim, loctim) then
    result := SystemTimeToDateTime(loctim)
  else
    result := 0;
end;

function FileAgeToUnixTimeUtc(const FileName: TFileName; AllowDir: boolean): TUnixTime;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, fa) and
     (AllowDir or ({%H-}fa.dwFileAttributes and faDirectory = 0)) then
    result := FileTimeToUnixTime(fa.ftLastWriteTime) // no local time conversion
  else
    result := 0;
end;

function FileTimeToDosTime(const ft: FILETIME): integer;
var
  lst: TSystemTime;
  lft: TFileTime;
begin
  result := 0;
  if FileTimeToLocalSystemTime(ft, lst) and // no FileTimeToLocalFileTime
     SystemTimeToFileTime(lst, lft) then
    FileTimeToDosDateTime(lft, LongRec(result).Hi, LongRec(result).Lo);
end;

function FileAgeToWindowsTime(F: THandle): integer;
var
  wt: FILETIME;
begin
  result := 0;
  if ValidHandle(F) and
     GetFileTime(F, nil, nil, @wt) then
    result := FileTimeToDosTime(wt);
end;

function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;
begin
  result := FileSetDate(Dest, WinTime) = 0; // we already are on Windows
end;

function FileSetDateFromUnixUtc(const Dest: TFileName; Time: TUnixTime): boolean;
var
  h: THandle;
  ft: TFileTime;
begin
  result := false;
  if (Dest = '') or
     (Time = 0) then
    exit;
  h := FileOpen(Dest, fmOpenWrite);
  if not ValidHandle(h) then
    exit;
  UnixTimeToFileTime(Time, ft);
  result := SetFileTime(h, nil, nil, @ft);
  FileClose(h);
end;

function SearchRecToWindowsTime(const F: TSearchRec): integer;
begin // don't use F.Time since latest Delphi (and FPC) RTL deprecated it
 result := FileTimeToDosTime(F.FindData.ftLastWriteTime);
end;

function SearchRecToDateTime(const F: TSearchRec): TDateTime;
var
  lst: TSystemTime;
begin // don't use F.Time since latest Delphi (and FPC) RTL deprecated it
  if FileTimeToLocalSystemTime(F.FindData.ftLastWriteTime, lst) then
    result := SystemTimeToDateTime(lst) // no FileTimeToLocalFileTime
  else
    result := 0;
end;

function SearchRecToUnixTimeUtc(const F: TSearchRec): TUnixTime;
begin // return the search record timestamp with no local time conversion
  result := FileTimeToUnixTime(F.FindData.ftLastWriteTime);
end;

function FileInfoByHandle(aFileHandle: THandle; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;
var
  mtime, atime, ctime: Int64;
  lp: TByHandleFileInformation;
begin
  result := GetFileInformationByHandle(aFileHandle, lp);
  if not result then
    exit;
  if FileId <> nil then
  begin
    PInt64Rec(FileId)^.lo := lp.nFileIndexLow;
    PInt64Rec(FileId)^.hi := lp.nFileIndexHigh;
  end;
  if FileSize <> nil then
  begin
    PInt64Rec(FileSize)^.lo := lp.nFileSizeLow;
    PInt64Rec(FileSize)^.hi := lp.nFileSizeHigh;
  end;
  if (LastWriteAccess = nil) and
     (FileCreateDateTime = nil) then
    exit;
  mtime := FileTimeToUnixMSTime(lp.ftLastWriteTime);
  if LastWriteAccess <> nil then
    LastWriteAccess^ := mtime;
  if FileCreateDateTime = nil then
    exit;
  atime := FileTimeToUnixMSTime(lp.ftLastAccessTime);
  ctime := FileTimeToUnixMSTime(lp.ftCreationTime);
  if mtime <> 0 then
    if (ctime = 0) or
       (ctime > mtime) then
      ctime := mtime;
  if atime <> 0 then
    if (ctime = 0) or
       (ctime > atime) then
      ctime := atime;
  FileCreateDateTime^ := ctime;
end;

function FileIsExecutable(const FileName: TFileName): boolean;
var
  header: word;
begin
  result := BufferFromFile(FileName, @header, SizeOf(header)) and
            (header = $5A4D); // DOS Magic Number
end;

function GetModuleHandleExA(dwFlags: cardinal; lpModuleName: pointer;
 var phModule: HMODULE): BOOL; stdcall; external kernel32;

const
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = $00000002;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = $00000004;

function GetExecutableName(aAddress: pointer): TFileName;
var
  tmp: array[byte] of WideChar;
  hm: HMODULE;
begin
  result := '';
  FillcharFast(tmp, SizeOf(tmp), 0);
  hm := 0;
  if not GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT or
           GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, aAddress, hm) then
    exit;
  GetModuleFileNameW(hm, tmp, SizeOf(tmp));
  Win32PWideCharToFileName(tmp, result);
end;

function FileIsWritable(const FileName: TFileName): boolean;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, fa) and
            (fa.dwFileAttributes and faReadOnly = 0);
end;

function FileExists(const FileName: TFileName; FollowLink, CheckAsDir: boolean): boolean;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, fa, FollowLink) and
            ((fa.dwFileAttributes and faDirectory <> 0) = CheckAsDir);
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
  else if FileName[len] <> '\' then
    result := FileExists(FileName, FollowLink, {checkasdir=}true)
  else
    result := FileExists(copy(FileName, 1, len - 1), FollowLink, true);
end;

function FileSize(const FileName: TFileName): Int64;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, fa) and
     (fa.dwFileAttributes and faDirectory = 0) then
    result := Qword(fa.nFileSizeHigh) shl 32 + fa.nFileSizeLow
  else
    result := 0;
end;

function FileInfoByName(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime): boolean;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, fa);
  if not result then
    exit;
  if fa.dwFileAttributes and faDirectory = 0 then
  begin
    PInt64Rec(@FileSize)^.Lo := fa.nFileSizeLow;
    PInt64Rec(@FileSize)^.Hi := fa.nFileSizeHigh;
  end
  else
    FileSize := -1; // FileName is a folder
  FileTimestampUtc := FileTimeToUnixMSTime(fa.ftLastWriteTime) // no local time
end;

function FileIsSymLink(const FileName: TFileName): boolean;
var
  fa: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, fa) and
            (fa.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0);
end;

function GetFileSizeEx(hFile: THandle; var FileSize: Int64): BOOL;
  stdcall; external kernel32;

function FileSize(F: THandle): Int64;
begin
  if (PtrInt(F) <= 0) or
     not GetFileSizeEx(F, result) then
    result := 0;
end;

function FileSeek64(Handle: THandle; const Offset: Int64;
  Origin: cardinal): Int64;
var
  r: TQWordRec;
begin
  r.V := Offset;
  r.L := SetFilePointer(Handle, r.L, @r.H, Origin);
  if (r.Li = -1) and
     (GetLastError <> 0) then
    result := -1
  else
    result := r.V;
end;

function DeleteFile(const aFileName: TFileName): boolean;
var
  tmp: TW32Temp;
begin
  if aFileName = '' then
    result := false
  else
    result := DeleteFileW(W32(aFileName, tmp));
end;

function FileShare(aMode: integer): DWord;
begin
  case (aMode and $f0) of
    fmShareRead:       // fmShareDenyWrite
      result := FILE_SHARE_READ;
    fmShareWrite:      // fmShareDenyRead
      result := FILE_SHARE_WRITE;
    fmShareReadWrite:  // fmShareDenyNone
      result := FILE_SHARE_READ or FILE_SHARE_WRITE;
  else
    result := 0;
  end;
end;

function FileCreate(const aFileName: TFileName; aMode, aRights: integer): THandle;
var
  tmp: TW32Temp;
begin
  // aRights parameter is just ignored on Windows
  if aFileName = '' then
    result := 0
  else
    result := CreateFileW(W32(aFileName, tmp), GENERIC_READ or GENERIC_WRITE,
      FileShare(aMode), nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

const
  FILE_WRITE_ATTRIBUTES = $0100; // not defined on oldest Delphi
  FILE_ACCESS: array[fmOpenRead .. fmOpenReadWrite {0..2}] of DWord = (
    GENERIC_READ,                                            // fmOpenRead
    GENERIC_WRITE,                                           // fmOpenWrite
    GENERIC_READ or GENERIC_WRITE or FILE_WRITE_ATTRIBUTES); // fmOpenReadWrite

// W32() will support length > MAX_PATH even if aFileName is UnicodeString
function FileOpen(const aFileName: TFileName; aMode: integer): THandle;
var
  tmp: TW32Temp;
begin
  if aFileName = '' then
    result := 0
  else
   result := CreateFileW(W32(aFileName, tmp), FILE_ACCESS[aMode and 3],
      FileShare(aMode), nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
end;

function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;
var
  ft: TFileTime;
  d: THandle;
begin
  if (Dest = '') or
     not ValidHandle(SourceHandle) then
    result := false
  else
  begin
    d := FileOpen(Dest, fmOpenWrite);
    if ValidHandle(d) then
    begin
      result := GetFileTime(SourceHandle, nil, nil, @ft) and
                SetFileTime(d, nil, nil, @ft);
      FileClose(d);
    end
    else
      result := false;
  end;
end;

function FileSetDateFrom(const Dest, Source: TFileName): boolean;
var
  s: THandle;
begin
  result := false;
  if (Dest = '') or
     (Source = '') then
    exit;
  s := FileOpen(Source, fmOpenReadShared);
  if not ValidHandle(s) then
    exit;
  result := FileSetDateFrom(Dest, s);
  FileClose(s);
end;

procedure FileSetAttr(const FileName: TFileName; Attr: integer);
var
  tmp: TW32Temp;
begin
  if FileName <> '' then
    SetFileAttributesW(W32(FileName, tmp), Attr);
end;

procedure FileSetHidden(const FileName: TFileName; ReadOnly: boolean);
const
  FLAGS: array[boolean] of integer = (
    FILE_ATTRIBUTE_HIDDEN,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_READONLY);
begin
  FileSetAttr(FileName, FLAGS[ReadOnly]);
end;

procedure FileSetSticky(const FileName: TFileName);
begin
  FileSetAttr(FileName, FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
end;

function RenameFile(const OldName, NewName: TFileName): boolean;
var
  o, n: TW32Temp;
begin
  if (OldName = '') or
     (NewName = '') then
    result := false
  else
    result := MoveFileW(W32(OldName, o), W32(NewName, n));
end;

function FileSetTime(const FileName: TFileName;
  const Created, Accessed, Written: Int64): boolean;
var
  tmp: TW32Temp;
  h: THandle;
  pct, pat, pwt: pointer;
begin
  result := false;
  h := CreateFileW(W32(FileName, tmp), FILE_WRITE_ATTRIBUTES,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
  if ValidHandle(h) then
    try
      // some input code may not set all properties: use what we got
      if Created <> 0 then
        pct := @Created
      else if Written <> 0 then
        pct := @Written
      else if Accessed <> 0 then
        pct := @Accessed
      else
        exit;
      if Accessed <> 0 then
        pat := @Accessed
      else if Written <> 0 then
        pat := @Written
      else
        pat := @Created;
      if Written <> 0 then
        pwt := @Written
      else if Created <> 0 then
        pwt := @Created
      else
        pwt := @Accessed;
      result := SetFileTime(h, pct, pat, pwt);
    finally
      CloseHandle(h);
    end;
end;

function ExpandFileName(const FileName: TFileName): TFileName;
var
  w, fp: PWideChar;
  t1, t2: TW32Temp;
begin
  result := FileName;
  if (FileName = '') or
     ((FileName[2] = ':') and 
      (length(FileName) < MAX_PATH) and
      (ord(FileName[1]) in [ord('A')..ord('Z'), ord('a')..ord('z')]) and
      (Pos('..', FileName) = 0)) then
    exit; // it seems to be already expanded 
  w := W32(FileName, t1);
  if w = nil then
    exit;
  if not IsExtendedPathName(w) then // W32() may have converted to extended-length
    if GetFullPathNameW(w, high(t2), @t2, fp) = 0 then
      exit
    else
      w := @t2; // full expanded file name from Windows API
  Win32PWideCharToFileName(w, result);
end;

function CopyFile(const Source, Target: TFileName; FailIfExists: boolean): boolean;
var
  s, t: TW32Temp;
begin
  if (Source = '') or
     (Target = '') then
    result := false
  else
    result := Windows.CopyFileW(W32(Source, s), W32(Target, t), FailIfExists);
end;

const
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2; // devmode
var
  CreateSymbolicLinkW: function(lpSymlinkFileName, lpTargetFileName: PWideChar;
    dwFlags: DWORD): BOOL; stdcall;
  CreateSymbolicLinkFlags: DWORD = SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;

function FileSymLink(const SymLink, Target: TFileName): boolean;
var
  s, t: TW32Temp;
  sw, tw: PWideChar;
begin
  result := (SymLink <> '') and
            Assigned(CreateSymbolicLinkW) and // Vista+
            FileExists(Target);
  if not result then
    exit;
  sw := W32(SymLink, s);
  tw := W32(Target, t);
  result := CreateSymbolicLinkW(sw, tw, CreateSymbolicLinkFlags);
  if result or
     (CreateSymbolicLinkFlags = 0) or
     (GetLastError <> ERROR_INVALID_PARAMETER) then
    exit;
  CreateSymbolicLinkFlags := 0; // unsupported before Windows version 1703
  result := CreateSymbolicLinkW(sw, tw, 0);
end;

function ValidHandle(Handle: THandle): boolean;
begin
  result := PtrInt(Handle) > 0;
end;

function FileOpenSequentialRead(const FileName: TFileName): integer;
var
  tmp: TW32Temp;
begin
  result := CreateFileW(W32(FileName, tmp), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_SEQUENTIAL_SCAN, 0);
end;

function FileIsReadable(const aFileName: TFileName): boolean;
var
  tmp: TW32Temp;
  h: THandle;
begin
  h := CreateFileW(W32(aFileName, tmp), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  result := ValidHandle(h);
  if result then
    CloseHandle(h);
end;

threadvar // mandatory: GetTickCount seems per-thread on XP :(
  LastTickXP: TQWordRec;

function GetTickCount64ForXP: Int64; stdcall;
var
  t32: cardinal;
  p: PQWordRec;
begin
  // warning: GetSystemTimeAsFileTime() is fast, but not monotonic!
  t32 := Windows.GetTickCount; // we only have the 32-bit counter on XP
  p := @LastTickXP;
  inc(p^.H, ord(t32 < p^.L)); // wrap-up overflow after 49.7 days
  p^.L := t32;
  result := p^.V;
end; // warning: FPC's GetTickCount64 doesn't handle 49.7 days wrap on XP :(

procedure InitializeSRWLockForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).Init; // TLightLock is good enough on XP
end;

procedure AcquireSRWLockExclusiveForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).Lock;
end;

procedure ReleaseSRWLockExclusiveForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).UnLock;
end;

function GetUptimeSec: cardinal;
begin
  result := GetTickCount64 div MilliSecsPerSec; // good enough
end;

procedure SleepHiRes(ms: cardinal);
begin
  if ms <> 0 then
    Windows.Sleep(ms) // follow the HW timer: typically up to 16ms on Windows
  else
    SwitchToThread;
end;


{ TOSLightLock }

procedure TOSLightLock.Init;
begin
  fMutex := nil;
  InitializeSRWLock(fMutex); // fallback to TLightLock on XP
end;

procedure TOSLightLock.Done;
begin // nothing needed
end;

procedure TOSLightLock.Lock;
begin
  AcquireSRWLockExclusive(fMutex);
end;

procedure TOSLightLock.UnLock;
begin
  ReleaseSRWLockExclusive(fMutex);
end;


{ TSynEvent }

constructor TSynEvent.Create;
begin
  fHandle := pointer(CreateEvent(nil, false, false, nil));
end;

destructor TSynEvent.Destroy;
begin
  CloseHandle(THandle(fHandle));
  inherited Destroy;
end;

procedure TSynEvent.ResetEvent;
begin
  Windows.ResetEvent(THandle(fHandle));
end;

procedure TSynEvent.SetEvent;
begin
  Windows.SetEvent(THandle(fHandle));
end;

procedure TSynEvent.WaitFor(TimeoutMS: integer);
begin
  WaitForSingleObject(THandle(fHandle), TimeoutMS);
end;

procedure TSynEvent.WaitForEver;
begin
  WaitForSingleObject(THandle(fHandle), INFINITE);
end;


{$ifdef FPC}
  {$define NOSETTHREADNAME} // only tested and supported on Delphi
{$endif FPC}

const
  // see http://msdn.microsoft.com/en-us/library/xcb2z8hs
  cSetThreadNameException = $406D1388;

{$ifdef NOSETTHREADNAME}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
begin
end;

{$else}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
var
  s: AnsiString;
  {$ifndef ISDELPHIXE2}
  info: record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;    // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
  {$endif ISDELPHIXE2}
begin
  if not IsDebuggerPresent then
    exit;
  s := AnsiString(Name);
  {$ifdef ISDELPHIXE2}
  TThread.NameThreadForDebugging(s, ThreadID); // use
  {$else}
  info.FType := $1000;
  info.FName := pointer(s);
  info.FThreadID := ThreadID;
  info.FFlags := 0;
  try
    RaiseException(
      cSetThreadNameException, 0, SizeOf(info) div SizeOf(LongWord), @info);
  except
    {ignore}
  end;
  {$endif ISDELPHIXE2}
end;

{$endif NOSETTHREADNAME}

function RawKillThread(Thread: TThread): boolean;
begin
  result := (Thread <> nil) and
            Windows.TerminateThread(Thread.Handle, 777);
end;

procedure ResetCpuSet(out CpuSet: TCpuSet);
begin
  CpuSet := 0;
end;

function SetThreadMaskAffinity(Thread: TThread; const Mask: TCpuSet): boolean;
begin
  result := (Thread <> nil) and
            (Windows.SetThreadAffinityMask(Thread.Handle, Mask) <> 0);
end;

function GetProcessAffinityMask(hProcess: THandle;
  var lpProcessAffinityMask, lpSystemAffinityMask: TCpuSet): BOOL;
    stdcall; external kernel32; // redefined for Delphi 7 compatibility

function GetMaskAffinity(out CpuSet: TCpuSet): boolean;
var
  process, thread: TCpuSet;
begin
  result := GetProcessAffinityMask(GetCurrentProcess, process, thread);
  if result then
    CpuSet := process;
end;

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

{$ifndef NOEXCEPTIONINTERCEPT}

type
  // https://docs.microsoft.com/en-us/archive/blogs/yizhang/interpreting-hresults-returned-from-netclr-0x8013xxxx
  // see also https://referencesource.microsoft.com/#mscorlib/system/__hresults.cs
  TDotNetException = (
    _Access,                            // $8013151A
    _AmbiguousMatch,                    // $8000211D
    _appdomainUnloaded,                 // $80131015
    _Application,                       // $80131600
    _Argument,                          // $80070057
    _ArgumentNull,                      // $80004003
    _ArgumentOutOfRange,                // $80131502
    _Arithmetic,                        // $80070216
    _ArrayTypeMismatch,                 // $80131503
    _BadImageFormat,                    // $8007000B
    _CannotUnloadappdomain,             // $80131015
    _ContextMarshal,                    // $80090020
    _Cryptographic,                     // $80004001
    _CryptographicUnexpectedOperation,  // $80131431
    _CustomAttributeFormat,             // $80131537
    _DirectoryNotFound,                 // $80070003
    _DirectoryNotFound2,                // $80030003
    _DivideByZero,                      // $80020012
    _DllNotFound,                       // $80131524
    _DuplicateWaitObject,               // $80131529
    _EndOfStream,                       // $00801338
    _EntryPointNotFound,                // $80131522
    _,                                  // $80131500 - name is plain Exception
    _ExecutionEngine,                   // $80131506
    _External,                          // $80004005
    _FieldAccess,                       // $80131507
    _FileLoad,                          // $80131621
    _FileLoad2,                         // $80131018
    _FileNotFound,                      // $80070002
    _Format,                            // $80131537
    _IndexOutOfRange,                   // $80131508
    _InvalidCast,                       // $80004002
    _InvalidComObject,                  // $80131527
    _InvalidFilterCriteria,             // $80131601
    _InvalidOleVariantType,             // $80131531
    _InvalidOperation,                  // $80131509
    _InvalidProgram,                    // $8013153A
    _IO,                                // $80131620
    _IsolatedStorage,                   // $80131450
    _MarshalDirective,                  // $80131535
    _MethodAccess,                      // $80131510
    _MissingField,                      // $80131511
    _MissingManifestResource,           // $80131532
    _MissingMember,                     // $80131512
    _MissingMethod,                     // $80131513
    _MulticastNotSupported,             // $80131514
    _NotFiniteNumber,                   // $80131528
    _NotImplemented,                    // $80004001
    _NotSupported,                      // $80131515
    _NullReference,                     // $80004003
    _OutOfMemory,                       // $8007000E
    _Overflow,                          // $80131516
    _PlatformNotSupported,              // $80131539
    _Policy,                            // $80131416
    _Rank,                              // $80131517
    _ReflectionTypeLoad,                // $80131602
    _Remoting,                          // $8013150B
    _RemotingTimeout,                   // $8013150B
    _SafeArrayTypeMismatch,             // $80131533
    _SafeArrayRankMismatch,             // $80131538
    _Security,                          // $8013150A
    _SEH,                               // $80004005
    _Serialization,                     // $8013150C
    _Server,                            // $8013150E
    _StackOverflow,                     // $800703E9
    _SUDSGenerator,                     // $80131500
    _SUDSParser,                        // $80131500
    _SynchronizationLock,               // $80131518
    _System,                            // $80131501
    _Target,                            // $80131603
    _TargetInvocation,                  // $80131604
    _TargetParameterCount,              // $80138002
    _ThreadAbort,                       // $80131530
    _ThreadInterrupted,                 // $80131519
    _ThreadState,                       // $80131520
    _ThreadStop,                        // $80131521
    _TypeInitialization,                // $80131534
    _TypeLoad,                          // $80131522
    _TypeUnloaded,                      // $80131013
    _UnauthorizedAccess,                // $80070005
    _InClassConstructor,                // $80131543
    _KeyNotFound,                       // $80131577
    _InsufficientStack,                 // $80131578
    _InsufficientMemory,                // $8013153D
    _Verification,                      // $8013150D
    _HostProtection,                    // $80131640
    _MinGrantFailed,                    // $80131417
    _Crypto,                            // $80131430
    _CryptoUnexOper,                    // $80131431
    _Overflow2,                         // $8002000a
    _InvalidName,                       // $80131047
    _TypeMismatch);                     // $80028ca0
const
  DOTNET_EXCEPTIONHRESULT: array[TDotNetException] of cardinal = (
    $8013151A,
    $8000211D,
    $80131015,
    $80131600,
    $80070057,
    $80004003,
    $80131502,
    $80070216,
    $80131503,
    $8007000B,
    $80131015,
    $80090020,
    $80004001,
    $80131431,
    $80131537,
    $80070003,
    $80030003,
    $80020012,
    $80131524,
    $80131529,
    $00801338,
    $80131522,
    $80131500,
    $80131506,
    $80004005,
    $80131507,
    $80131621,
    $80131018,
    $80070002,
    $80131537,
    $80131508,
    $80004002,
    $80131527,
    $80131601,
    $80131531,
    $80131509,
    $8013153A,
    $80131620,
    $80131450,
    $80131535,
    $80131510,
    $80131511,
    $80131532,
    $80131512,
    $80131513,
    $80131514,
    $80131528,
    $80004001,
    $80131515,
    $80004003,
    $8007000E,
    $80131516,
    $80131539,
    $80131416,
    $80131517,
    $80131602,
    $8013150B,
    $8013150B,
    $80131533,
    $80131538,
    $8013150A,
    $80004005,
    $8013150C,
    $8013150E,
    $800703E9,
    $80131500,
    $80131500,
    $80131518,
    $80131501,
    $80131603,
    $80131604,
    $80138002,
    $80131530,
    $80131519,
    $80131520,
    $80131521,
    $80131534,
    $80131522,
    $80131013,
    $80070005,
    $80131543,
    $80131577,
    $80131578,
    $8013153D,
    $8013150D,
    $80131640,
    $80131417,
    $80131430,
    $80131431,
    $8002000a,
    $80131047,
    $80028ca0);

procedure Win32DotNetExceptions(code: cardinal; var names: TPShortStringDynArray);
var
  e: TDotNetException;
begin
  for e := low(e) to high(e) do
    // manual loop: the same error code can appear several times
    if DOTNET_EXCEPTIONHRESULT[e] = code then
      PtrArrayAdd(names, WinGetEnumName(TypeInfo(TDotNetException), ord(e)));
end;

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPShortStringDynArray): cardinal;
begin
  if ClassInheritsFromName(EClass, 'EOleSysError') then
  begin
    result := EOleSysError(EInstance).ErrorCode;
    if result > $80000000 then
      Win32DotNetExceptions(result, ExceptionNames);
  end
  else
    result := 0;
end;

var
  _RawLogException: TOnRawLogException;

{$ifdef FPC}
  {$ifdef WIN64}
    {$define WITH_VECTOREXCEPT} // use AddVectoredExceptionHandler Win64 API
  {$else}
    {$ifdef FPC_USE_WIN32_SEH}
      {$define WITH_VECTOREXCEPT} // new since FPC 3.2
    {$else}
      // Win32, Linux: intercept via the RaiseProc global variable
      {$define WITH_RAISEPROC} // RaiseProc is set in main mormot.core.os.pas
    {$endif FPC_USE_WIN32_SEH}
  {$endif WIN64}
{$else}
  {$ifdef CPU64}
    {$define WITH_VECTOREXCEPT}
  {$else}
    {$define WITH_RTLUNWINDPROC} //  use x86_64 asm -> Win32 only
  {$endif CPU64}
{$endif FPC}

{$ifndef WITH_RAISEPROC}

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: integer;
    case {IsOsException:} boolean of
      true:
        (ExceptionInformation: array[0..14] of PtrUInt);
      false:
        (ExceptAddr: PtrUInt;
         ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  {$ifdef FPC}
  cRtlException = $E0465043; // $E0 F P C
  {$else}
  cRtlException = $0EEDFADE; // Delphi exception
  {$endif FPC}

procedure LogExcept(stack: PPtrUInt; const Exc: TExceptionRecord);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWord;
  backuphandler: TOnRawLogException;
begin
  if Exc.ExceptionCode = cSetThreadNameException then
    exit;
  backuplasterror := GetLastError;
  backuphandler := _RawLogException;
  if Assigned(backuphandler) then // paranoid check (tested before calling)
    try
      _RawLogException := nil; // disable nested exception
      ctxt.ECode := Exc.ExceptionCode;
      if (Exc.ExceptionCode = cRtlException) and
         (Exc.ExceptObject <> nil) then
      begin
        if Exc.ExceptObject.InheritsFrom(Exception) then
          ctxt.EClass := PPointer(Exc.ExceptObject)^
        else
          ctxt.EClass := EExternalException;
        ctxt.EInstance := Exc.ExceptObject;
        ctxt.ELevel := sllException;
        ctxt.EAddr := Exc.ExceptAddr;
      end
      else
      begin
        if Assigned(ExceptClsProc) then
          ctxt.EClass := GetExceptionClass(ExceptClsProc)(Exc)
        else
          ctxt.EClass := EExternal;
        ctxt.EInstance := nil;
        ctxt.ELevel := sllExceptionOS;
        ctxt.EAddr := Exc.ExceptionAddress;
      end;
      ctxt.EStack := pointer(stack);
      ctxt.EStackCount := 0;
      ctxt.ETimestamp := UnixTimeUtc; // fast API call
      backuphandler(ctxt);
    except
      { ignore any nested exception }
    end;
  _RawLogException := backuphandler;
  SetLastError(backuplasterror); // code above could have changed this
end;

{$ifdef WITH_VECTOREXCEPT}

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = packed record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: pointer;
  end;

var
  AddVectoredExceptionHandler: function(FirstHandler: cardinal;
    VectoredHandler: pointer): PtrInt; stdcall;

function SynLogVectoredHandler(ExceptionInfo: PExceptionInfo): PtrInt; stdcall;
const
  EXCEPTION_CONTINUE_SEARCH = 0;
begin
  if Assigned(_RawLogException) then
    LogExcept(nil, ExceptionInfo^.ExceptionRecord^);
  result := EXCEPTION_CONTINUE_SEARCH;
end;

{$endif WITH_VECTOREXCEPT}

{$ifdef WITH_RTLUNWINDPROC}

var
  OldUnWindProc: pointer;

procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
  ExceptionRecord: PExceptionRecord; ReturnValue: pointer); stdcall;
asm
        cmp     dword ptr _RawLogException, 0
        jz      @old
        pushad
        mov     eax, TargetFrame
        mov     edx, ExceptionRecord
        call    LogExcept
        popad
@old:   pop     ebp // hidden push ebp at asm level
        jmp     OldUnWindProc
end;

{$endif WITH_RTLUNWINDPROC}

{$endif WITH_RAISEPROC}

{$endif NOEXCEPTIONINTERCEPT}

{ TMemoryMap }

function TMemoryMap.DoMap(aCustomOffset: Int64): boolean;
begin
  with PInt64Rec(@fFileSize)^ do
    fMap := CreateFileMapping(fFile, nil, PAGE_READONLY, Hi, Lo, nil);
  if fMap = 0 then
    RaiseLastError('TMemoryMap.Map: CreateFileMapping');
  with PInt64Rec(@aCustomOffset)^ do
    fBuf := MapViewOfFile(fMap, FILE_MAP_READ, Hi, Lo, fBufSize);
  if fBuf = nil then
  begin
    // Windows failed to find a contiguous VA space -> fall back on direct read
    CloseHandle(fMap);
    fMap := 0;
  end;
  result := fMap <> 0;
end;

procedure TMemoryMap.DoUnMap;
begin
  if fMap <> 0 then
  begin
    UnmapViewOfFile(fBuf);
    CloseHandle(fMap);
    fMap := 0;
  end;
end;

type
  TProcessMemoryCounters = record
    cb: DWord;
    PageFaultCount: DWord;
    PeakWorkingSetSize: PtrUInt;
    WorkingSetSize: PtrUInt;
    QuotaPeakPagedPoolUsage: PtrUInt;
    QuotaPagedPoolUsage: PtrUInt;
    QuotaPeakNonPagedPoolUsage: PtrUInt;
    QuotaNonPagedPoolUsage: PtrUInt;
    PagefileUsage: PtrUInt;
    PeakPagefileUsage: PtrUInt;
  end;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

var
  // PROCESS_QUERY_INFORMATION (XP) / PROCESS_QUERY_LIMITED_INFORMATION (Vista+)
  OpenProcessAccess: DWord;

  // late-binding of Windows API entries not available on XP
  GetSystemTimes: function(
    var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessTimes: function(hProcess: THandle;
    var lpCreationTime, lpExitTime, lpKernelTime,
        lpUserTime: TFileTime): BOOL; stdcall;
  // Vista+/WS2008+ (use GetModuleFileNameEx on XP)
  QueryFullProcessImageNameW: function(hProcess: THandle; dwFlags: DWord;
    lpExeName: PWideChar; lpdwSize: PDWord): BOOL; stdcall;
  // PSAPI API late-binding via DelayedProc()
  GetProcessMemoryInfo: function(Process: THandle;
    var ppsmemCounters: TProcessMemoryCounters; cb: DWord): BOOL; stdcall;
  EnumProcesses: function(lpidProcess: PDWord; cb: DWord;
    var cbNeeded: DWord): BOOL; stdcall;
  GetModuleFileNameExW: function(hProcess: THandle; hModule: HMODULE;
    lpBaseName: PWideChar; nSize: DWord): DWord; stdcall;

function DelayedProc(var api; var lib: THandle;
  libname: PChar; procname: PAnsiChar): boolean;
var
  proc: pointer;
begin
  if pointer(api) = nil then
  begin
    proc := nil;
    GlobalLock; // avoid race condition
    if lib = 0 then
      lib := Windows.LoadLibrary(libname);
    if lib >= 32 then
      proc := Windows.GetProcAddress(lib, procname)
    else
      lib := 1; // try to load the library once
    if proc = nil then
      proc := pointer(1); // mark non available
    pointer(api) := proc; // set it last
    GlobalUnLock;
  end;
  result := pointer(api) <> pointer(1);
end;

function GetNextItem(var P: PAnsiChar): RawUtf8;
var
  beg: PAnsiChar;
begin
  result := '';
  while P^ <= ' ' do
    if P^ = #0 then
      exit
    else
      inc(P);
  beg := P;
  repeat
    inc(P);
  until P^ <= ' ';
  FastSetString(result, beg, P - beg);
end;

const
  PAGE_GUARD = $0100;
  PAGE_VALID = $00e6; // PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or
      // PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY

var
  LastMemInfo: TMemoryBasicInformation; // simple cache

function SeemsRealPointer(p: pointer): boolean;
var
  meminfo: TMemoryBasicInformation;
begin
  result := false;
  if PtrUInt(p) <= 65535 then
    exit; // first 64KB is not a valid pointer by definition
  if (LastMemInfo.State <> 0) and
     (PtrUInt(p) - PtrUInt(LastMemInfo.BaseAddress) < PtrUInt(LastMemInfo.RegionSize)) then
    result := true // quick check against last valid memory region
  else
  begin
    // VirtualQuery API is slow but better than raising an exception
    // see https://stackoverflow.com/a/37547837/458259
    FillCharFast(meminfo, SizeOf(meminfo), 0);
    result := (VirtualQuery(p, meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
              (meminfo.State = MEM_COMMIT) and
              (PtrUInt(p) - PtrUInt(meminfo.BaseAddress) < PtrUInt(meminfo.RegionSize)) and
              (meminfo.Protect and PAGE_VALID <> 0) and
              (meminfo.Protect and PAGE_GUARD = 0);
    if result then
      LastMemInfo := meminfo;
  end;
end;

function SetTimeZoneInformation(const info: TTimeZoneInformation): BOOL;
  stdcall; external kernel32; // make it consistent on Delphi and FPC

procedure SetSystemTimeZone(const info: TDynamicTimeZoneInformation);
var
  SetDynamicTimeZoneInformation: function(
    const lpTimeZoneInformation: TDynamicTimeZoneInformation): BOOL; stdcall;
  privileges: TSynWindowsPrivileges;
  ok: BOOL;
  err: integer;
begin
  privileges.Init;
  try
    privileges.Enable(wspTimeZone); // ensure has SE_TIME_ZONE_NAME
    SetDynamicTimeZoneInformation := GetProcAddress(
      GetModuleHandle(kernel32), 'SetDynamicTimeZoneInformation');
    if Assigned(SetDynamicTimeZoneInformation) then
      ok := SetDynamicTimeZoneInformation(info)    // Vista+
    else
      ok := SetTimeZoneInformation(info.TimeZone); // XP
    err := GetLastError;
  finally
    privileges.Done;
  end;
  if not ok then
    RaiseLastError('SetSystemTimeZone', EOSException, err);
  PostMessage(HWND_BROADCAST, WM_TIMECHANGE, 0, 0); // notify the apps
end;

var
  PsapiDll: THandle;

function EnumAllProcesses: TCardinalDynArray;
var
  n, count: cardinal;
begin
  result := nil;
  if not DelayedProc(EnumProcesses, PsapiDll, 'Psapi.dll', 'EnumProcesses') then
    exit;
  count := 0;
  n := 2048; // retrieve 2KB of IDs, i.e. 512 processes, by default
  repeat
    SetLength(result, n);
    if EnumProcesses(pointer(result), n * 4, count) then
      count := count shr 2 // from bytes to count
    else
      count := 0; // error
    if count < n then
      break;
    // count=n if buffer was too small
    inc(n, 1024);
  until n > 8192;
  if count = 0 then
    result := nil // on error
  else
    DynArrayFakeLength(result, count); // no realloc
end;

function EnumProcessName(PID: cardinal): RawUtf8;
var
  h: THandle;
  len: DWord;
  name: array[0..4095] of WideChar;
begin
  result := '';
  if PID = 0 then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h <> 0 then
    try
      if Assigned(QueryFullProcessImageNameW) then
      begin
        len := high(name);
        if QueryFullProcessImageNameW(h, 0, name, @len) then
          Win32PWideCharToUtf8(name, len, result);
      end
      else if DelayedProc(GetModuleFileNameExW, PsapiDll, 'Psapi.dll',
                'GetModuleFileNameExW') and
              (GetModuleFileNameExW(h, 0, name, high(name)) <> 0) then
        Win32PWideCharToUtf8(name, result);
    finally
      CloseHandle(h);
    end;
end;

// some definitions missing on Delphi and/or FPC
type
  TProcessEntry32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;          // this process
    th32DefaultHeapID: PtrUInt;
    th32ModuleID:DWORD;            // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD;    // this process's parent process
    pcPriClassBase: integer;          // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array [0..MAX_PATH - 1] of WideChar;   // Path
  end;

  TThreadEntry32 = record
    dwSize: DWord;
    cntUsage: DWord;
    th32ThreadID: DWord;       // this thread
    th32OwnerProcessID: DWord; // Process this thread is associated with
    tpBasePri: integer;
    tpDeltaPri: integer;
    dwFlags: DWord;
  end;

const
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD = $00000004;

function AttachConsole(pid: cardinal): BOOL;
  stdcall; external kernel32;
function GetConsoleWindow: HWND;
  stdcall; external kernel32;
function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWord): THandle;
  stdcall; external kernel32;
function Process32FirstW(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
  stdcall; external kernel32;
function Process32NextW(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
  stdcall; external kernel32;
function Thread32First(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL;
  stdcall; external kernel32;
function Thread32Next(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL;
  stdcall; external kernel32;

function PostThreadMessage(idThread: DWord; Msg: UINT; wParam: WPARAM;
    lParam: LPARAM): BOOL;
  stdcall; external user32 name 'PostThreadMessageW';

function RawProcessInfo(pid: cardinal; var e: TProcessEntry32): boolean;
var
  snap: THandle;
begin
  result := false;
  if integer(pid) <= 0 then
    pid := GetCurrentProcessId;
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snap <= 0 then
    exit;
  FillCharFast(e, SizeOf(e), 0);
  e.dwSize := SizeOf(e);
  result := true;
  if Process32FirstW(snap, e) then // loop over all processes of the system
    repeat
      if e.th32ProcessID = pid then
        exit;
    until not Process32NextW(snap, e);
  CloseHandle(snap);
  result := false;
end;

function GetParentProcess(PID: cardinal): cardinal;
var
  e: TProcessEntry32;
begin
  if RawProcessInfo(PID, e) then
    result := e.th32ParentProcessID
  else
    result := 0;
end;

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
begin
  result := Assigned(GetSystemTimes) and
            GetSystemTimes(ftidl, ftkrn, ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl, IdleTime);
  FileTimeToInt64(ftkrn, KernelTime);
  FileTimeToInt64(ftusr, UserTime);
end;

function RetrieveLoadAvg: RawUtf8;
begin
  result := ''; // call RetrieveSystemTimes() instead
end;

function DelayedGetProcessMemoryInfo: boolean;
begin
  result:= DelayedProc(GetProcessMemoryInfo, PsapiDll, 'Psapi.dll',
    'GetProcessMemoryInfo');
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  mem: TProcessMemoryCounters;
begin
  result := false;
  if (not Assigned(GetProcessTimes)) or
     (not DelayedGetProcessMemoryInfo) then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h = 0 then
    exit;
  try
    if GetProcessTimes(h, ftp, fte, ftkrn, ftusr) then
    begin
      FileTimeToInt64(ftkrn, KernelTime);
      FileTimeToInt64(ftusr, UserTime);
      FillCharFast(mem, SizeOf(mem), 0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h, mem, SizeOf(mem)) then
      begin
        WorkKB := mem.WorkingSetSize shr 10;
        VirtualKB := mem.PagefileUsage shr 10;
      end;
      result := true;
    end;
  finally
    CloseHandle(h);
  end;
end;

function CoCreateGuid(out guid: THash128Rec): HRESULT;
  stdcall; external 'ole32.dll';

{$ifndef UNICODE} // 64-bit aware Windows API missing on FPC and oldest Delphi

type
  TMemoryStatusEx = record
    dwLength: DWord;
    dwMemoryLoad: DWord;
    ullTotalPhys: QWord;
    ullAvailPhys: QWord;
    ullTotalPageFile: QWord;
    ullAvailPageFile: QWord;
    ullTotalVirtual: QWord;
    ullAvailVirtual: QWord;
    ullAvailExtendedVirtual: QWord;
  end;

// information about the system's current usage of both physical and virtual memory
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
  stdcall; external kernel32;

{$endif UNICODE}

procedure XorOSEntropy(var e: THash512Rec);
var
  ft: packed record
    krn, usr, p, e: TFileTime;
  end;
  mem: TProcessMemoryCounters;
  memst: TMemoryStatusEx;
  h: THash128Rec absolute memst;
begin
  QueryPerformanceCounter(h.Lo); // e.h3 xored with raw timestamps
  e.i[6] := e.i[6] xor h.Lo;
  if Assigned(GetProcessTimes) then
    GetProcessTimes(GetCurrentProcess, ft.p, ft.e, ft.krn, ft.usr);
  DefaultHasher128(@e.h0, @ft, SizeOf(ft));
  FillCharFast(mem, SizeOf(mem), 0);
  mem.cb := SizeOf(mem);
  if Assigned(GetProcessMemoryInfo) then // may have been delayed
    if GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem)) then
      DefaultHasher128(@e.h1, @mem, SizeOf(mem));
  FillCharFast(memst, SizeOf(memst), 0);
  memst.dwLength := SizeOf(memst);
  if GlobalMemoryStatusEx(memst) then
    DefaultHasher128(@e.h2, @memst, SizeOf(memst));
  if Assigned(GetSystemTimes) then
    GetSystemTimes(ft.usr, ft.p, ft.e);
  DefaultHasher128(@e.h3, @ft, SizeOf(ft));
  CoCreateGuid(h); // very fast on Windows - used to obfuscate system info
  e.i[0] := e.i[0] xor h.Lo;
  e.i[1] := e.i[1] xor h.Hi;
  CoCreateGuid(h);
  e.i[2] := e.i[2] xor h.Lo;
  e.i[3] := e.i[3] xor h.Hi;
  CoCreateGuid(h);
  e.i[4] := e.i[4] xor h.Lo;
  e.i[5] := e.i[5] xor h.Hi;
  CoCreateGuid(h);
  e.i[6] := e.i[6] xor h.Lo;
  e.i[7] := e.i[7] xor h.Hi;
  QueryPerformanceCounter(h.Lo); // is likely to have changed in-between
  e.i[7] := e.i[7] xor h.Lo;     // e.h3 xored with raw timestamps
end;

function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;
var
  prov: HCRYPTPROV;
begin
  result := false;
  if Len <= 0 then
    exit;
  // warning: on some Windows versions, this could take up to 30 ms!
  if CryptoApi.Available then
    if CryptoApi.AcquireContextA(prov, nil, nil,
      PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    begin
      result := CryptoApi.GenRandom(prov, Len, Buffer);
      CryptoApi.ReleaseContext(prov, 0);
    end;
  if not result then
    // OS API call failed -> fallback to our Lecuyer's gsl_rng_taus2 generator
    RandomBytes(pointer(Buffer), Len);
end;

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self, SizeOf(self), 0);
  // no monitoring API available under oldest Windows
  result := Assigned(GetSystemTimes) and
            Assigned(GetProcessTimes) and
            DelayedGetProcessMemoryInfo;
end;

function TProcessInfo.Start: boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
  sidl, skrn, susr: Int64;
begin
  result := Assigned(GetSystemTimes) and
            GetSystemTimes(ftidl, ftkrn, ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl, sidl);
  FileTimeToInt64(ftkrn, skrn);
  FileTimeToInt64(ftusr, susr);
  fDiffIdle   := sidl - fSysPrevIdle;
  fDiffKernel := skrn - fSysPrevKernel;
  fDiffUser   := susr - fSysPrevUser;
  fDiffTotal  := fDiffKernel + fDiffUser; // kernel time also includes idle time
  dec(fDiffKernel, fDiffIdle);
  fSysPrevIdle   := sidl;
  fSysPrevKernel := skrn;
  fSysPrevUser   := susr;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  pkrn, pusr: Int64;
  mem: TProcessMemoryCounters;
begin
  result := false;
  FillCharFast(Data, SizeOf(Data), 0);
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h <> 0 then
  try
    if GetProcessTimes(h, ftp, fte, ftkrn, ftusr) then
    begin
      if Now <> nil then
        Data.Timestamp := Now^;
      FileTimeToInt64(ftkrn, pkrn);
      FileTimeToInt64(ftusr, pusr);
      if (PrevKernel <> 0) and
         (fDiffTotal > 0) then
      begin
        Data.Kernel := ((pkrn - PrevKernel) * 100) / fDiffTotal;
        Data.User   := ((pusr - PrevUser)   * 100) / fDiffTotal;
      end;
      PrevKernel := pkrn;
      PrevUser := pusr;
      FillCharFast(mem, SizeOf(mem), 0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h, mem, SizeOf(mem)) then
      begin
        Data.WorkKB    := mem.WorkingSetSize shr 10;
        Data.VirtualKB := mem.PagefileUsage  shr 10;
      end;
      result := true;
    end;
  finally
    CloseHandle(h);
  end;
end;

function TProcessInfo.PerSystem(out Idle, Kernel, User: single): boolean;
begin
  if fDiffTotal <= 0 then
  begin
    Idle   := 0;
    Kernel := 0;
    User   := 0;
    result := false;
  end
  else
  begin
    Kernel := {%H-}SimpleRoundTo2Digits((fDiffKernel * 100) / fDiffTotal);
    User   := {%H-}SimpleRoundTo2Digits((fDiffUser * 100)   / fDiffTotal);
    Idle   := 100 - Kernel - User; // ensure sum is always 100%
    result := true;
  end;
end;

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
{$ifdef WITH_FASTMM4STATS}
var
  Heap: TMemoryManagerState;
  sb: PtrInt;
{$endif WITH_FASTMM4STATS}
var
  global: TMemoryStatusEx;
  mem: TProcessMemoryCounters;
begin
  FillCharFast(global, SizeOf(global), 0);
  global.dwLength := SizeOf(global);
  result := GlobalMemoryStatusEx(global);
  info.percent   := global.dwMemoryLoad;
  info.memtotal  := global.ullTotalPhys;
  info.memfree   := global.ullAvailPhys;
  info.filetotal := global.ullTotalPageFile;
  info.filefree  := global.ullAvailPageFile;
  info.vmtotal   := global.ullTotalVirtual;
  info.vmfree    := global.ullAvailVirtual;
  info.allocreserved := 0;
  info.allocused     := 0;
  if not withalloc then
    exit;
  {$ifdef WITH_FASTMM4STATS} // override OS information by actual FastMM4
  GetMemoryManagerState(Heap); // direct raw FastMM4 access
  info.allocused := Heap.TotalAllocatedMediumBlockSize +
                    Heap.TotalAllocatedLargeBlockSize;
  info.allocreserved := Heap.ReservedMediumBlockAddressSpace +
                        Heap.ReservedLargeBlockAddressSpace;
  for sb := 0 to high(Heap.SmallBlockTypeStates) do
    with Heap.SmallBlockTypeStates[sb] do
    begin
      inc(info.allocused, UseableBlockSize * AllocatedBlockCount);
      inc(info.allocreserved, ReservedAddressSpace);
    end;
  {$else}
  if not DelayedGetProcessMemoryInfo then
    exit;
  FillcharFast(mem, SizeOf(mem), 0);
  mem.cb := SizeOf(mem);
  GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem));
  info.allocreserved := mem.PeakWorkingSetSize;
  info.allocused     := mem.WorkingSetSize;
  {$endif WITH_FASTMM4STATS}
end;

function GetDiskFreeSpaceExW(lpDirectoryName: PWideChar;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes,
  lpTotalNumberOfFreeBytes: QWord): LongBool;
   stdcall; external kernel32;

{
// DeviceIoControl(IOCTL_DISK_GET_PARTITION_INFO) requires root -> not used
function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWord;
  lpInBuffer: pointer; nInBufferSize: DWord; lpOutBuffer: pointer;
  nOutBufferSize: DWord; var lpBytesReturned: DWord;
  lpOverlapped: POverlapped): BOOL; stdcall; external kernel32;
}

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord;
  aVolumeName: PSynUnicode): boolean;
var
  tmp: array[0..MAX_PATH - 1] of WideChar;
  dummy, flags: DWord;
  dn: SynUnicode;
begin
  if aDriveFolderOrFile = '' then
    aDriveFolderOrFile := SysUtils.UpperCase(
      ExtractFileDrive(Executable.ProgramFilePath));
  dn := SynUnicode(aDriveFolderOrFile); // use RTL for UTF-16 conversion
  if (dn <> '') and
     (dn[2] = ':') and
     (dn[3] = #0) then
    dn := dn + '\';
  if (aVolumeName <> nil) and
     (aVolumeName^ = '') then
  begin
    tmp[0] := #0;
    GetVolumeInformationW(pointer(dn), tmp, MAX_PATH, nil, dummy, flags, nil, 0);
    aVolumeName^ := tmp;
  end;
  result := GetDiskFreeSpaceExW(pointer(dn),
    aAvailableBytes, aTotalBytes, aFreeBytes);
end;

function GetDiskPartitions: TDiskPartitions;
var
  drives, drive, m, n: integer;
  fn: TFileName;
  volume: SynUnicode;
  av, fr, tot: QWord;
  p: ^TDiskPartition;
begin
  result := nil;
  n := 0;
  fn := '#:';
  drives := GetLogicalDrives;
  m := 1 shl 2; // bit 2 = drive C
  for drive := 3 to 26 do
  begin
    // retrieve partitions mounted as C..Z drives
    if drives and m <> 0 then
    begin
      fn[1] := char(64 + drive);
      if GetDiskInfo(fn, av, fr, tot, @volume) then
      begin
        SetLength(result, n + 1);
        p := @result[n];
        Win32PWideCharToUtf8(pointer(volume), length(volume), p^.name);
        p^.mounted := fn;
        p^.size := tot;
        volume := '';
        inc(n);
      end;
    end;
    m := m shl 1;
  end;
end;

var
  TextAttr: integer = ord(ccDarkGray);

procedure TextColor(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if not HasConsole then
    exit;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $F0) or ord(Color);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TextBackground(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if not HasConsole then
    exit;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $0F) or (ord(Color) shl 4);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor;
  NoLineFeed, NoColor: boolean);
var
  txt: RawByteString;
  l: PtrInt;
begin
  if not HasConsole then
    exit;
  txt := Utf8ToConsole(Text); // convert to final GetConsoleOutputCP() code page
  l := length(txt);
  if not NoLineFeed then
  begin
    SetLength(txt, l + 2); // faster to reallocate than WriteFile() twice
    PWord(@PByteArray(txt)[l])^ := CRLFW;
    inc(l, 2);
  end;
  ConsoleCriticalSection.Lock;
  try
    if not NoColor then
      TextColor(Color);
    FileWriteAll(StdOut, pointer(txt), l);
    if not NoColor then
      TextColor(ccLightGray);
    // FlushFileBuffers(StdOut); // don't: would block until read on the pipe
  finally
    ConsoleCriticalSection.UnLock;
  end;
end;

function ConsoleKeyPressed(ExpectedKey: Word): boolean;
var
  events, read: DWord;
  rec: TInputRecord;
  h: THandle;
begin
  result := false;
  h := GetStdHandle(STD_INPUT_HANDLE);
  events := 0;
  GetNumberOfConsoleInputEvents(h, events);
  if events <> 0 then
  begin
    PeekConsoleInput(h, rec, 1, read);
    if read <> 0 then
      if rec.EventType = KEY_EVENT then
        if rec.Event.KeyEvent.bKeyDown and
           ((ExpectedKey = 0) or
            (rec.Event.KeyEvent.wVirtualKeyCode = ExpectedKey)) then
          result := true
        else
          FlushConsoleInputBuffer(h)
      else
        FlushConsoleInputBuffer(h);
  end;
end;

type
  TConsoleHandleCtrlC = class
  private
    class procedure HandleCtrlC;
  end;
var
  ConsoleHandleCtrlCPressed: boolean;

class procedure TConsoleHandleCtrlC.HandleCtrlC;
begin
  ConsoleHandleCtrlCPressed := true;
end;

procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  ConsoleHandleCtrlCPressed := false;
  HandleCtrlC(TConsoleHandleCtrlC.HandleCtrlC);
  try
    if GetCurrentThreadID = MainThreadID then
      // process the messages from the main thread while waiting
      while not ConsoleKeyPressed(VK_RETURN) and
            not ConsoleHandleCtrlCPressed do
      begin
        if IsMultiThread then
          CheckSynchronize{$ifndef DELPHI6OROLDER}(100){$endif}
        else
          Windows.Sleep(100);
        while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
          if msg.Message = WM_QUIT then
            exit // stop waiting when the process is gracefully closing
          else
          begin
            TranslateMessage(msg);
            DispatchMessage(msg);
          end;
      end
    else
      // just intercept any WM_QUIT message on this sub-thread
      while not ConsoleKeyPressed(VK_RETURN) and
            not ConsoleHandleCtrlCPressed do
      begin
        Windows.Sleep(100);
        if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
          if msg.Message = WM_QUIT then
            exit; // nothing to dispatch with PostThreadMessage()
      end;
  finally
    HandleCtrlC(nil);
  end;
end;

function ConsoleStdInputLen: integer;
begin
  if StdInputHandle = 0 then
    StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  if not PeekNamedPipe(StdInputHandle, nil, 0, nil, @result, nil) then
    result := 0;
end;

function Utf8ToConsole(const S: RawUtf8): RawByteString;
begin
  if not IsAnsiCompatible(S) then // need some encoding conversion
  begin
    if ConsoleOutputCP = 0 then // retrieve once CP_OEM final code page
      ConsoleOutputCP := GetConsoleOutputCP; // makes a syscall
    if ConsoleOutputCP <> CP_UTF8 then
      if Win32Utf8ToAnsi(pointer(S), length(S), ConsoleOutputCP, result) then
        exit;
  end;
  result := S; // no conversion needed, or conversion failed
end;

function TFileVersion.RetrieveInformationFromFileName: boolean;
var
  siz, siz2: DWord;
  pt: pointer;
  trans: PWordArray;
  lngnfo: RawUtf8;
  nfo: ^TVSFixedFileInfo;
  ft: TFILETIME;
  st: TSYSTEMTIME;
  tmp: TW32Temp;

  procedure ReadResourceByName(const From: RawUtf8; out Res: RawUtf8);
  var
    str: pointer;
    sz: DWord;
    u: SynUnicode;
  begin
    u := Utf8Decode(lngnfo + From);
    if VerQueryValueW(pt, pointer(u), str, sz) and
       (sz > 0) then
      Win32PWideCharToUtf8(str, Res);
  end;

begin
  result := false;
  if fFileName = '' then
    exit;
  // GetFileVersionInfo() modifies the filename parameter data while parsing
  // -> copy the FileName into local tmp buffer to create a writable copy
  siz := GetFileVersionInfoSizeW(W32(fFileName, tmp, {copy=}true), siz2);
  if siz > 0 then
  begin
    GetMem(pt, siz);
    try
      if GetFileVersionInfoW(W32(fFileName, tmp, {copy=}true), 0, siz, pt) then
      begin
        if VerQueryValueW(pt, '\', pointer(nfo), siz2) then
          with nfo^ do
          begin
            SetVersion({major=}   dwFileVersionMS shr 16,
                       {minor=}   dwFileVersionMS and 65535,
                       {release=} dwFileVersionLS shr 16,
                       {build=}   dwFileVersionLS and 65535);
            if (dwFileDateLS <> 0) and
               (dwFileDateMS <> 0) then
            begin
              ft.dwLowDateTime := dwFileDateLS; // built date from version nfo
              ft.dwHighDateTime := dwFileDateMS;
              FileTimeToSystemTime(ft, st);
              fBuildDateTime := EncodeDate(
                st.wYear, st.wMonth, st.wDay);
            end;
          end;
        if VerQueryValueW(pt, '\VarFileInfo\Translation', pointer(trans), siz2) and
           (siz2 >= 4) then
        begin
          _fmt('\StringFileInfo\%4.4x%4.4x\', [trans^[0], trans^[1]], lngnfo);
          ReadResourceByName('CompanyName', CompanyName);
          ReadResourceByName('FileDescription', FileDescription);
          ReadResourceByName('FileVersion', FileVersion);
          ReadResourceByName('InternalName', InternalName);
          ReadResourceByName('LegalCopyright', LegalCopyright);
          ReadResourceByName('OriginalFilename',OriginalFilename);
          ReadResourceByName('ProductName', ProductName);
          ReadResourceByName('ProductVersion', ProductVersion);
          ReadResourceByName('Comments', Comments);
        end;
        result := true;
      end;
    finally
      Freemem(pt);
    end;
  end;
end;

procedure GetUserHost(out User, Host: RawUtf8);
var
  tmp: array[byte] of WideChar;
  tmpsize: cardinal;
begin
  tmpsize := SizeOf(tmp);
  GetComputerNameW(tmp{%H-}, tmpsize);
  Win32PWideCharToUtf8(@tmp, Host);
  tmpsize := SizeOf(tmp);
  GetUserNameW(tmp, tmpsize);
  Win32PWideCharToUtf8(@tmp, User);
end;

var
  SHFolderDll: THandle;
  // avoid unneeded reference to ShlObj.pas
  // - late binding is mandatory to be used on WinPE which does NOT have this dll
  // - late binding also ensure that we load libraries only when needed
  SHGetFolderPathW: function(hwnd: hwnd; csidl: integer; hToken: THandle;
    dwFlags: DWord; pszPath: PChar): HRESULT; stdcall;

const
  CSIDL_PERSONAL         = $0005;
  CSIDL_LOCAL_APPDATA    = $001C; // local non roaming user folder
  CSIDL_COMMON_APPDATA   = $0023;
  CSIDL_COMMON_DOCUMENTS = $002E;

  CSIDL: array[TSystemPath] of integer = (
    CSIDL_COMMON_APPDATA,   // spCommonData
                            // C:\ProgramData
    CSIDL_LOCAL_APPDATA,    // spUserData
                            // C:\Users\<user>\AppData\Local
    CSIDL_COMMON_DOCUMENTS, // spCommonDocuments
                            // C:\Users\Public\Documents
    CSIDL_PERSONAL,         // spUserDocuments
                            // C:\Users\<user>\Documents
    0,                      // spTemp
    0);                     // spLog
  // note: for SYSTEM user, got C:\Windows\System32\config\systemprofile\AppData
  // or C:\Windows\SysWOW64\config\systemprofile\AppData (on Win32 over Win64)

procedure _ComputeSystemPath(kind: TSystemPath; var result: TFileName);
const
  _ENV: array[TSystemPath] of TFileName = (
    'ALLUSERSAPPDATA', // spCommonData
    'LOCALAPPDATA',    // spUserData
    '',                // spCommonDocuments
    '',                // spUserDocuments
    'TEMP',            // spTemp
    '');               // spLog
var
  tmp: array[0..MAX_PATH] of WideChar;
begin
  result := '';
  case kind of
    spLog:
      begin
        // try <exepath>\log - without [idwExcludeWinSys] (writable is enough)
        result := Executable.ProgramFilePath;
        if not IsDirectoryWritable(result) then
          // fallback to 'C:\Users\<user>\AppData\Local\<exename>-log'
          result := format('%s%s-',
                      [GetSystemPath(spUserData), Executable.ProgramName]);
        result := EnsureDirectoryExists(result + 'log');
        if IsDirectoryWritable(result) then
          exit; // found a folder able to receive new logs
        // 'C:\Users\<user>\AppData\Local\Temp\<exename>-log'
        result := EnsureDirectoryExists(format('%s%s-log',
                    [GetSystemPath(spTemp), Executable.ProgramName]));
      end;
    spTemp:
      begin
        // typically 'C:\Users\<user>\AppData\Local\Temp'
        if GetTempPathW(MAX_PATH, @tmp) <> 0 then // first try Windows API
          Win32PWideCharToFileName(tmp, result);
        if result = '' then
          result := GetEnvironmentVariable(_ENV[spTemp]); // fallback
      end;
  else
    if (CSIDL[kind] <> 0) and
       DelayedProc(SHGetFolderPathW, SHFolderDll, 'SHFolder.dll',
         'SHGetFolderPathW') and
       (SHGetFolderPathW(0, CSIDL[kind], 0, 0, @tmp) = S_OK) then
      // retrieved from official CSIDL
      Win32PWideCharToFileName(tmp, result)
    else
    begin
      // fallback to environment variables (very unlikely)
      result := GetEnvironmentVariable(_ENV[kind]);
      if result = '' then
      begin
        result := GetEnvironmentVariable('APPDATA');
        if result = '' then
          result := Executable.ProgramFilePath;
      end;
    end;
  end;
  if result <> '' then
    result := IncludeTrailingPathDelimiter(result); // no EnsureDirectoryExists
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  restore, ignore: DWord;
  i: PtrInt;
begin
  if VirtualProtect(Old, Size, PAGE_EXECUTE_READWRITE, restore) then
  begin
    if Backup <> nil then
      for i := 0 to Size - 1 do  // do not use Move() here
        PByteArray(Backup)^[i] := PByteArray(Old)^[i];
    for i := 0 to Size - 1 do    // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    if not LeaveUnprotected then
      VirtualProtect(Old, Size, restore, ignore);
    FlushInstructionCache(GetCurrentProcess, Old, Size);
    if not CompareMemFixed(Old, New, Size) then
      raise Exception.Create('PatchCode?');
  end;
end;


function StubMemoryAlloc: pointer;
begin
  result := VirtualAlloc(nil, STUB_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
end;

procedure StubMemoryFree(stub: pointer);
begin
  VirtualFree(stub, 0, MEM_RELEASE);
end;

procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);
begin
  // nothing to be done on Windows: PAGE_EXECUTE_READWRITE always work
end;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{ TWinRegistry }

const
  _HKEY: array[TWinRegistryRoot] of HKEY = (
    HKEY_CLASSES_ROOT,
    HKEY_CURRENT_USER,
    HKEY_LOCAL_MACHINE,
    HKEY_USERS);

function TWinRegistry.ReadOpen(root: TWinRegistryRoot; const keyname: RawUtf8;
  closefirst: boolean): boolean;
var
  tmp: TSynTempBuffer;
begin
  if closefirst then
    Close;
  key := 0;
  result := RegOpenKeyExW(
    _HKEY[root], Utf8ToWin32PWideChar(keyname, tmp), 0, KEY_READ, key) = NO_ERROR;
  tmp.Done;
end;

procedure TWinRegistry.Close;
begin
  if key <> 0 then
    RegCloseKey(key);
end;

function TWinRegistry.ReadString(const entry: SynUnicode; andtrim: boolean): RawUtf8;
var
  rtype, rsize, res: DWord;
  tmp: TSynTempBuffer;
begin
  result := '';
  rsize := {%H-}tmp.Init; // most of the time, a single call is enough
  res := RegQueryValueExW(key, pointer(entry), nil, @rtype, tmp.buf, @rsize);
  if res <> NO_ERROR then
    if res = ERROR_MORE_DATA then // more than 4KB of data (unlikely)
      res := RegQueryValueExW(key, pointer(entry), nil, nil, tmp.Init(rsize), @rsize)
    else
      exit;
  if res = NO_ERROR then
  begin
    case rtype of
      REG_SZ,
      REG_EXPAND_SZ,
      REG_MULTI_SZ: // StrLen() will return the first value of REG_MULTI_SZ
        Win32PWideCharToUtf8(tmp.buf, result);
    end;
    if andtrim then
      TrimSelf(result);
  end;
  tmp.Done;
end;

function TWinRegistry.ReadData(const entry: SynUnicode): RawByteString;
var
  rsize: DWord;
begin
  result := '';
  if RegQueryValueExW(key, pointer(entry), nil, nil, nil, @rsize) <> NO_ERROR then
    exit;
  SetLength(result, rsize);
  if RegQueryValueExW(key, pointer(entry), nil, nil, pointer(result), @rsize) <> NO_ERROR then
    result := '';
end;

function TWinRegistry.ReadDword(const entry: SynUnicode): cardinal;
begin
  if not ReadBuffer(entry, @result, SizeOf(result)) then
    result := 0;
end;

function TWinRegistry.ReadQword(const entry: SynUnicode): QWord;
begin
  if not ReadBuffer(entry, @result, SizeOf(result)) then
    result := 0;
end;

function TWinRegistry.ReadBuffer(const entry: SynUnicode;
  data: pointer; datalen: DWord): boolean;
begin
  result := RegQueryValueExW(key, pointer(entry), nil, nil, data, @datalen) = NO_ERROR;
end;

function TWinRegistry.ReadMax(const entry: SynUnicode;
  Data: pointer; MaxDataLen: DWORD): DWORD;
var
  tmp: RawByteString;
begin
  tmp := ReadData(entry); // we need to read the whole entry or the API fails
  result := length(tmp);
  if result = 0 then
    exit;
  if result > MaxDataLen then
    result := MaxDataLen;
  MoveFast(pointer(tmp)^, Data^, result);
end;

function TWinRegistry.ReadSize(const entry: SynUnicode): integer;
begin
  if RegQueryValueExW(key, pointer(entry), nil, nil, nil, @result) <> NO_ERROR then
    result := -1;
end;

function TWinRegistry.ReadEnumEntries: TRawUtf8DynArray;
var
  count, maxlen, i, len: DWord;
  tmp: TSynTempBuffer;
begin
  result := nil;
  count := 0;
  if (RegQueryInfoKeyW(key, nil, nil, nil, @count, @maxlen,
       nil, nil, nil, nil, nil, nil) <> NO_ERROR) or
     (count = 0) then
    exit;
  SetLength(result, count);
  inc(maxlen);
  tmp.Init(maxlen * 2);
  for i := 0 to count - 1 do
  begin
    len := maxlen;
    if RegEnumKeyExW(key, i, tmp.buf, len, nil, nil, nil, nil) = NO_ERROR then
      Win32PWideCharToUtf8(tmp.buf, len, result[i]);
  end;
  tmp.Done;
end;


const
  _WSP: array[TWinSystemPrivilege] of string[32] = (
    // note: string[32] to ensure there is a #0 terminator for all items
    'SeCreateTokenPrivilege',          // wspCreateToken
    'SeAssignPrimaryTokenPrivilege',   // wspAssignPrimaryToken
    'SeLockMemoryPrivilege',           // wspLockMemory
    'SeIncreaseQuotaPrivilege',        // wspIncreaseQuota
    'SeUnsolicitedInputPrivilege',     // wspUnsolicitedInput
    'SeMachineAccountPrivilege',       // wspMachineAccount
    'SeTcbPrivilege',                  // wspTCP
    'SeSecurityPrivilege',             // wspSecurity
    'SeTakeOwnershipPrivilege',        // wspTakeOwnership
    'SeLoadDriverPrivilege',           // wspLoadDriver
    'SeSystemProfilePrivilege',        // wspSystemProfile
    'SeSystemtimePrivilege',           // wspSystemTime
    'SeProfileSingleProcessPrivilege', // wspProfSingleProcess
    'SeIncreaseBasePriorityPrivilege', // wspIncBasePriority
    'SeCreatePagefilePrivilege',       // wspCreatePageFile
    'SeCreatePermanentPrivilege',      // wspCreatePermanent
    'SeBackupPrivilege',               // wspBackup
    'SeRestorePrivilege',              // wspRestore
    'SeShutdownPrivilege',             // wspShutdown
    'SeDebugPrivilege',                // wspDebug
    'SeAuditPrivilege',                // wspAudit
    'SeSystemEnvironmentPrivilege',    // wspSystemEnvironment
    'SeChangeNotifyPrivilege',         // wspChangeNotify
    'SeRemoteShutdownPrivilege',       // wspRemoteShutdown
    'SeUndockPrivilege',               // wspUndock
    'SeSyncAgentPrivilege',            // wspSyncAgent
    'SeEnableDelegationPrivilege',     // wspEnableDelegation
    'SeManageVolumePrivilege',         // wspManageVolume
    'SeImpersonatePrivilege',          // wspImpersonate
    'SeCreateGlobalPrivilege',         // wspCreateGlobal
    'SeTrustedCredManAccessPrivilege', // wspTrustedCredmanAccess
    'SeRelabelPrivilege',              // wspRelabel
    'SeIncreaseWorkingSetPrivilege',   // wspIncWorkingSet
    'SeTimeZonePrivilege',             // wspTimeZone
    'SeCreateSymbolicLinkPrivilege');  // wspCreateSymbolicLink

  _TokenVirtualizationEnabled = TTokenInformationClass(24); // for oldest Delphi

type
  TOKEN_PRIVILEGES = packed record
    PrivilegeCount : DWord;
    Privileges : array[0..0] of LUID_AND_ATTRIBUTES;
  end;
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;

  TOKEN_GROUPS = record
    GroupCount: DWord;
    Groups: array [0..0] of SID_AND_ATTRIBUTES;
  end;
  PTOKEN_GROUPS = ^TOKEN_GROUPS;

function OpenProcessToken(ProcessHandle: THandle; DesiredAccess: DWord;
  var TokenHandle: THandle): BOOL;
    stdcall; external advapi32;

function LookupPrivilegeValueA(lpSystemName, lpName: PAnsiChar;
  var lpLuid: TLargeInteger): BOOL;
    stdcall; external advapi32;

function LookupPrivilegeNameA(lpSystemName: PAnsiChar; var lpLuid: TLargeInteger;
  lpName: PAnsiChar; var cbName: DWord): BOOL;
    stdcall; external advapi32;

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TOKEN_PRIVILEGES; BufferLength: DWord;
  PreviousState: PTokenPrivileges; ReturnLength: PDWord): BOOL;
    stdcall; external advapi32;


function IsSystemFolder(const Folder: TFileName): boolean;
begin
  if CompareText(copy(Folder, 2,  9), ':\windows') = 0 then
    result := ord(Folder[11]) in [0, ord('\')]
  else
    result := (CompareText(copy(Folder, 2, 15), ':\program files') = 0) and
              ((ord(Folder[17]) in [0, ord('\')]) or
               (CompareText(copy(Folder, 17, 6), ' (x86)') = 0));
end;

{$ifdef CPU32}
var
  IsUacEnabled: (iueUntested, iueDisabled, iueEnabled);

function IsUacVirtualizationEnabled: boolean;
var
  token: THandle;
  enabled, len: DWORD;
begin
  if IsUacEnabled = iueUntested then
    if OSVersion < wVista then
      IsUacEnabled := iueDisabled // no UAC on Windows XP
    else
    begin
      IsUacEnabled := iueEnabled; // enabled by default
      if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
      begin
        enabled := 1;
        len := SizeOf(enabled);
        if GetTokenInformation(token, _TokenVirtualizationEnabled,
             @enabled, SizeOf(enabled), len) and
           (enabled = 0) then
          // returns enabled=0 if mormot.win.default.manifest.res is included
          IsUacEnabled := iueDisabled;
        CloseHandle(token);
      end;
    end;
  result := IsUacEnabled = iueEnabled;
end;

function IsUacVirtualFolder(const Folder: TFileName): boolean;
begin
  // note: IsUacVirtualizationEnabled returns false if our manifest is included
  result := (OSVersion >= wVista) and // no UAC on Windows XP
            IsUacVirtualizationEnabled and
            IsSystemFolder(Folder);
end;

{$else}

function IsUacVirtualizationEnabled: boolean;
begin
  result := false; // never enabled for a Win64 process
end;

function IsUacVirtualFolder(const Folder: TFileName): boolean;
begin
  result := false; // never enabled for a Win64 process
end;

{$endif CPU32}

function RawTokenOpen(wtt: TWinTokenType; access: cardinal): THandle;
begin
  if wtt = wttProcess then
  begin
    if not OpenProcessToken(GetCurrentProcess, access, result) then
      RaiseLastError('OpenToken: OpenProcessToken');
  end
  else if not OpenThreadToken(GetCurrentThread, access, false, result) then
    if GetLastError = ERROR_NO_TOKEN then
    begin
      // try to impersonate the thread
      if not ImpersonateSelf(SecurityImpersonation) or
         not OpenThreadToken(GetCurrentThread, access, false, result) then
        RaiseLastError('OpenToken: ImpersonateSelf');
    end
    else
      RaiseLastError('OpenToken: OpenThreadToken');
end;

function RawTokenGetInfo(tok: THandle; tic: TTokenInformationClass;
  var buf: TSynTempBuffer): cardinal;
begin
  buf.Init; // stack-allocated buffer (enough in most cases)
  result := 0; // error
  if (tok = INVALID_HANDLE_VALUE) or
     (tok = 0) or
     GetTokenInformation(tok, tic, buf.buf, buf.len, result) then
    exit; // we directly store the output buffer on buf stack
  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
  begin
    result := 0;
    exit;
  end;
  buf.Done;
  buf.Init(result); // we need a bigger buffer (unlikely)
  if not GetTokenInformation(tok, tic, buf.buf, buf.len, result) then
    result := 0;
end;


{ TSynWindowsPrivileges }

function ToText(p: TWinSystemPrivilege): PShortString;
begin
  result := @_WSP[p];
end;

procedure TSynWindowsPrivileges.Init(aTokenPrivilege: TWinTokenType;
  aLoadPrivileges: boolean);
begin
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  fToken := RawTokenOpen(aTokenPrivilege, TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES);
  if aLoadPrivileges then
    LoadPrivileges;
end;

procedure TSynWindowsPrivileges.Done(aRestoreInitiallyEnabled: boolean);
var
  p: TWinSystemPrivilege;
  new: TWinSystemPrivileges;
begin
  if aRestoreInitiallyEnabled then
  begin
    new := fEnabled - fDefEnabled;
    if new <> [] then
      for p := low(p) to high(p) do
        if p in new then
        begin
          Disable(p);
          exclude(new, p);
          if new = [] then
            break; // all done
        end;
  end;
  CloseHandle(fToken);
  fToken := 0;
end;

function TSynWindowsPrivileges.Enable(aPrivilege: TWinSystemPrivilege): boolean;
begin
  result := aPrivilege in fEnabled;
  if result or
     not (aPrivilege in fAvailable) or
     not SetPrivilege(aPrivilege, true) then
    exit;
  Include(fEnabled, aPrivilege);
  result := true;
end;

function TSynWindowsPrivileges.Enable(aPrivilege: TWinSystemPrivileges): boolean;
var
  p: TWinSystemPrivilege;
begin
  result := true;
  for p := low(p) to high(p) do
    if p in aPrivilege then
      if not Enable(p) then
        result := false; // notify an error at some point
end;

function TSynWindowsPrivileges.Disable(
  aPrivilege: TWinSystemPrivilege): boolean;
begin
  result := not (aPrivilege in fEnabled);
  if result or
     not (aPrivilege in fAvailable) or
     not SetPrivilege(aPrivilege, false) then
    exit;
  Exclude(fEnabled, aPrivilege);
  result := true;
end;

procedure TSynWindowsPrivileges.LoadPrivileges;
var
  buf: TSynTempBuffer;
  name: string[127];
  tp: PTOKEN_PRIVILEGES;
  i: PtrInt;
  len: cardinal;
  p: TWinSystemPrivilege;
  priv: PLUIDANDATTRIBUTES;
begin
  if Token = 0 then
    raise EOSException.Create('LoadPriviledges: no token');
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  try
    if RawTokenGetInfo(Token, TokenPrivileges, buf) = 0 then
      RaiseLastError('LoadPriviledges: GetTokenInformation');
    tp := buf.buf;
    priv := @tp.Privileges;
    for i := 1 to tp.PrivilegeCount do
    begin
      len := high(name);
      if not LookupPrivilegeNameA(nil, priv.Luid, @name[1], len) or
         (len = 0) then
         RaiseLastError('LoadPriviledges: LookupPrivilegeNameA');
      name[0] := AnsiChar(len);
      for p := low(p) to high(p) do
        if not (p in fAvailable) and
           PropNameEquals(PShortString(@name), PShortString(@_WSP[p])) then
        begin
          include(fAvailable, p);
          if priv.Attributes and SE_PRIVILEGE_ENABLED <> 0 then
            include(fDefEnabled, p);
          break;
        end;
      inc(priv);
    end;
    fEnabled := fDefEnabled;
  finally
    buf.Done;
  end;
end;

function TSynWindowsPrivileges.SetPrivilege(
  wsp: TWinSystemPrivilege; on: boolean): boolean;
var
  tp: TOKEN_PRIVILEGES;
  id: TLargeInteger;
  tpprev: TOKEN_PRIVILEGES;
  cbprev: DWord;
begin
  result := false;
  if not LookupPrivilegeValueA(nil, @_WSP[wsp][1], id) then
    exit;
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid := PInt64(@id)^;
  tp.Privileges[0].Attributes := 0;
  cbprev := SizeOf(TOKEN_PRIVILEGES);
  AdjustTokenPrivileges(
    Token, false, tp, SizeOf(TOKEN_PRIVILEGES), @tpprev, @cbprev);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  tpprev.PrivilegeCount := 1;
  tpprev.Privileges[0].Luid := PInt64(@id)^;
  with tpprev.Privileges[0] do
    if on then
      Attributes := Attributes or SE_PRIVILEGE_ENABLED
    else
      Attributes := Attributes xor (SE_PRIVILEGE_ENABLED and Attributes);
  AdjustTokenPrivileges(
    Token, false, tpprev, cbprev, nil, nil);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  result := true;
end;

const
  ntdll = 'NTDLL.DLL';

type
  _PPS_POST_PROCESS_INIT_ROUTINE = ULONG;

  PMS_PEB_LDR_DATA = ^MS_PEB_LDR_DATA;
  MS_PEB_LDR_DATA = packed record
    Reserved1: array[0..7] of byte;
    Reserved2: array[0..2] of pointer;
    InMemoryOrderModuleList: LIST_ENTRY;
  end;

  PMS_RTL_USER_PROCESS_PARAMETERS = ^MS_RTL_USER_PROCESS_PARAMETERS;
  MS_RTL_USER_PROCESS_PARAMETERS = packed record
    Reserved1: array[0..15] of byte;
    Reserved2: array[0..9] of pointer;
    ImagePathName: UNICODE_STRING;
    CommandLine: UNICODE_STRING ;
  end;

  PMS_PEB = ^MS_PEB;
  MS_PEB = packed record
    Reserved1: array[0..1] of byte;
    BeingDebugged: BYTE;
    Reserved2: array[0..0] of byte;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif CPUX64}
    Reserved3: array[0..1] of pointer;
    Ldr: PMS_PEB_LDR_DATA;
    ProcessParameters: PMS_RTL_USER_PROCESS_PARAMETERS;
    Reserved4: array[0..103] of byte;
    Reserved5: array[0..51] of pointer;
    PostProcessInitRoutine: _PPS_POST_PROCESS_INIT_ROUTINE;
    Reserved6: array[0..127] of byte;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif CPUX64}
    Reserved7: array[0..0] of pointer;
    SessionId: ULONG;
    {$ifdef CPUX64}
    _align3: array[0..3] of byte;
    {$endif CPUX64}
  end;

  PMS_PROCESS_BASIC_INFORMATION = ^MS_PROCESS_BASIC_INFORMATION;
  MS_PROCESS_BASIC_INFORMATION = packed record
    ExitStatus: integer;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif CPUX64}
    PebBaseAddress: PMS_PEB;
    AffinityMask: PtrUInt;
    BasePriority: integer;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif CPUX64}
    UniqueProcessId: PtrUInt;
    InheritedFromUniqueProcessId: PtrUInt;
  end;

  {$Z4}
  PROCESSINFOCLASS = (
    ProcessBasicInformation = 0,
    ProcessDebugPort = 7,
    ProcessWow64Information = 26,
    ProcessImageFileName = 27,
    ProcessBreakOnTermination = 29,
    ProcessSubsystemInformation = 75);
  {$Z1}

  NTSTATUS = integer;
  PVOID = pointer;
  PPVOID = ^PVOID;

  OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: pointer;       // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

var
  // low-level (undocumented) ntdll.dll functions - accessed via late-binding
  NtQueryInformationProcess: function(ProcessHandle: THandle;
    ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: pointer;
    ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  RtlInitUnicodeString: function(var DestinationString: UNICODE_STRING;
    const SourceString: PWideChar): NTSTATUS; stdcall;
  NtOpenSection: function (SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  NtMapViewOfSection: function (SectionHandle, ProcessHandle: THandle;
    BaseAddress: PPVOID; ZeroBits: ULONG; CommitSize: ULONG;
    var SectionOffset: TLargeInteger; ViewSize: PULONG; InheritDisposition: DWord;
    AllocationType: ULONG; Protect: ULONG): NTSTATUS; stdcall;
  NtUnmapViewOfSection: function (ProcessHandle: THandle;
    BaseAddress: PVOID): NTSTATUS; stdcall;

function ReadSystemMemory(address, size: PtrUInt): RawByteString;
var
  memfile: UNICODE_STRING;
  att: OBJECT_ATTRIBUTES;
  mem: THandle;
  add: TLargeInteger;
  virt: pointer;
begin
  result := '';
  if (size <= 4 shl 20) and // map up to 4MB
     Assigned(RtlInitUnicodeString) and
     Assigned(NtOpenSection) and
     Assigned(NtMapViewOfSection) and
     Assigned(NtUnmapViewOfSection) then
  begin
    RtlInitUnicodeString(memfile, '\device\physicalmemory');
    FillCharFast(att, SizeOf(att), 0);
    att.Length := SizeOf(att);
    att.ObjectName := @memfile;
    att.Attributes := $40; // OBJ_CASE_INSENSITIVE
    if NtOpenSection(@mem, SECTION_MAP_READ, @att) <> 0 then
      exit;
    add := address;
    virt := nil;
    if NtMapViewOfSection(mem, INVALID_HANDLE_VALUE, @virt, 0, size, add, @size,
         1, 0, PAGE_READONLY) = 0 then
    begin
      FastSetRawByteString(result, virt, size);
      NtUnmapViewOfSection(INVALID_HANDLE_VALUE, virt);
    end;
    CloseHandle(mem);
  end;
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: pointer;
  lpBuffer: pointer; nSize: PtrUInt; var lpNumberOfBytesRead: PtrUInt): BOOL;
    stdcall; external kernel32;

function InternalGetProcessInfo(aPID: DWord; out aInfo: TWinProcessInfo): boolean;
var
  bytesread: PtrUInt;
  sizeneeded: DWord;
  pbi: MS_PROCESS_BASIC_INFORMATION;
  peb: MS_PEB;
  peb_upp: MS_RTL_USER_PROCESS_PARAMETERS;
  prochandle: THandle;
begin
  result := false;
  Finalize(aInfo);
  FillCharFast(aInfo, SizeOf(aInfo), 0);
  if (APID = 0) and
     Assigned(NtQueryInformationProcess) then
    exit;
  prochandle := OpenProcess(
    PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, aPid);
  if prochandle = INVALID_HANDLE_VALUE then
    exit;
  Include(aInfo.AvailableInfo, wpaiPID);
  aInfo.PID := aPid;
  try
    // read PBI (Process Basic Information)
    sizeneeded := 0;
    FillCharFast(pbi, SizeOf(pbi), 0);
    if NtQueryInformationProcess(prochandle, ProcessBasicInformation,
         @pbi, Sizeof(pbi), @sizeneeded) < 0 then
      exit;
    with aInfo do
    begin
      Include(AvailableInfo, wpaiBasic);
      PID := pbi.UniqueProcessId;
      ParentPID := pbi.InheritedFromUniqueProcessId;
      BasePriority := pbi.BasePriority;
      ExitStatus := pbi.ExitStatus;
      PEBBaseAddress := pbi.PebBaseAddress;
      AffinityMask := pbi.AffinityMask;
    end;
    // read PEB (Process Environment Block)
    if not Assigned(pbi.PebBaseAddress) then
      exit;
    bytesread := 0;
    FillCharFast(peb, SizeOf(peb), 0);
    if not ReadProcessMemory(prochandle, pbi.PebBaseAddress,
             @peb, SizeOf(peb), bytesread) then
      exit;
    Include(aInfo.AvailableInfo, wpaiPEB);
    aInfo.SessionID := peb.SessionId;
    aInfo.BeingDebugged := peb.BeingDebugged;
    FillCharFast(peb_upp, SizeOf(MS_RTL_USER_PROCESS_PARAMETERS), 0);
    bytesread := 0;
    if not ReadProcessMemory(prochandle, peb.ProcessParameters,
         @peb_upp, SizeOf(MS_RTL_USER_PROCESS_PARAMETERS), bytesread) then
      exit;
    // command line info
    if peb_upp.CommandLine.Length > 0 then
    begin
      SetLength(aInfo.CommandLine, peb_upp.CommandLine.Length shr 1);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.CommandLine.Buffer,
           pointer(aInfo.CommandLine), peb_upp.CommandLine.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiCommandLine);
    end;
    // image info
    if peb_upp.ImagePathName.Length > 0 then
    begin
      SetLength(aInfo.ImagePath, peb_upp.ImagePathName.Length shr 1);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.ImagePathName.Buffer,
           pointer(aInfo.ImagePath), peb_upp.ImagePathName.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiImagePath);
    end;
    result := true;
  finally
    CloseHandle(prochandle);
  end;
end;

procedure GetProcessInfo(aPid: cardinal; out aInfo: TWinProcessInfo);
var
  privileges: TSynWindowsPrivileges;
begin
  privileges.Init(wttThread);
  try
    privileges.Enable(wspDebug);
    InternalGetProcessInfo(aPid, aInfo);
  finally
    privileges.Done;
  end;
end;

procedure GetProcessInfo(const aPidList: TCardinalDynArray;
  out aInfo: TWinProcessInfoDynArray);
var
  privileges: TSynWindowsPrivileges;
  i: PtrInt;
begin
  SetLength(aInfo, Length(aPidList));
  privileges.Init(wttThread);
  try
    privileges.Enable(wspDebug);
    for i := 0 to High(aPidList) do
      InternalGetProcessInfo(aPidList[i], aInfo[i]);
  finally
    privileges.Done;
  end;
end;

function ReadRegString(Key: THandle; const Path, Value: string): string;
var
  siz, typ: DWord;
  tmp: array[byte] of char;
  k: HKey;
begin
  result := '';
  if RegOpenKeyEx(Key, pointer(Path), 0, KEY_QUERY_VALUE, k) <> ERROR_SUCCESS then
    exit;
  siz := 250;
  typ := REG_SZ;
  if RegQueryValueEx(k, pointer(Value), nil, @typ, @tmp, @siz) = ERROR_SUCCESS then
    result := tmp;
  RegCloseKey(k);
end;


{ TWinCryptoApi }

function TWinCryptoApi.Available: boolean;
begin
  if not Tested then
    Resolve;
  result := Assigned(AcquireContextA);
end;

procedure TWinCryptoApi.Resolve;
const
  NAMES: array[0..8] of PAnsiChar = (
    'CryptAcquireContextA',
    'CryptReleaseContext',
    'CryptImportKey',
    'CryptSetKeyParam',
    'CryptDestroyKey',
    'CryptEncrypt',
    'CryptDecrypt',
    'CryptGenRandom',
    'ConvertSecurityDescriptorToStringSecurityDescriptorA');
var
  p: PPointer;
  i: PtrInt;
begin
  Tested := true;
  Handle := GetModuleHandle('advapi32.dll');
  if Handle <> 0 then
  begin
    p := @@AcquireContextA;
    for i := 0 to high(NAMES) do
    begin
      p^ := LibraryResolve(Handle, NAMES[i]);
      if p^ = nil then
      begin
        PPointer(@@AcquireContextA)^ := nil;
        break;
      end;
      inc(p);
    end;
  end;
end;

const
  SDDL_REVISION_1 = 1;
  ALL_INFO = OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or
             DACL_SECURITY_INFORMATION  or SACL_SECURITY_INFORMATION;

function TWinCryptoApi.SecurityDescriptorToText(sd: pointer; out text: RawUtf8): boolean;
var
  txt: PAnsiChar;
begin
  result := false;
  txt := nil;
  if (sd = nil) or
     not Available or
     not ConvertSecurityDescriptorToStringSecurityDescriptorA(
       sd, SDDL_REVISION_1, ALL_INFO, txt, nil) then
    exit;
  FastSetString(text, txt, StrLen(txt));
  LocalFree(HLOCAL(txt));
  result := true;
end;

type
  {$ifdef FPC}
  {$packrecords C} // mandatory under Win64
  {$endif FPC}
  DATA_BLOB = record
    cbData: DWord;
    pbData: PAnsiChar;
  end;
  PDATA_BLOB = ^DATA_BLOB;
  {$ifdef FPC}
  {$packrecords DEFAULT}
  {$endif FPC}

const
  crypt32 = 'Crypt32.dll';
  CRYPTPROTECT_UI_FORBIDDEN = 1;
  CRYPT_STRING_BASE64HEADER = 0; // = PEM textual format

function CryptProtectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: pointer; dwFlags: DWord;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external crypt32;

function CryptUnprotectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: pointer; dwFlags: DWord;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external crypt32;

function CryptDataForCurrentUserDPAPI(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;
var
  src, dst, ent: DATA_BLOB;
  e: PDATA_BLOB;
  ok: boolean;
begin
  src.pbData := pointer(Data);
  src.cbData := length(Data);
  if AppSecret <> '' then
  begin
    ent.pbData := pointer(AppSecret);
    ent.cbData := length(AppSecret);
    e := @ent;
  end
  else
    e := nil;
  if Encrypt then
    ok := CryptProtectData(
      src, nil, e, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, dst)
  else
    ok := CryptUnprotectData(
      src, nil, e, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, dst);
  if ok then
  begin
    FastSetRawByteString(result, dst.pbData, dst.cbData);
    LocalFree(HLOCAL(dst.pbData));
  end
  else
    result := '';
end;

function CertOpenSystemStoreW(hProv: HCRYPTPROV;
  szSubsystemProtocol: PWideChar): HCERTSTORE ;
    stdcall; external crypt32;

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT;
  stdcall; external crypt32;

function CryptBinaryToStringA(pBinary: PByte; cbBinary, dwFlags: DWord;
  pszString: PAnsiChar; var pchString: DWord): BOOL;
    stdcall; external crypt32;

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWord): BOOL;
    stdcall; external crypt32;

function _GetSystemStoreAsPem(CertStore: TSystemCertificateStore): RawUtf8;
var
  store: HCERTSTORE;
  ctx: PCCERT_CONTEXT;
  certlen: DWord;
  tmp: TSynTempBuffer;
begin
  // call the Windows API to retrieve the System certificates
  result := '';
  store := CertOpenSystemStoreW(nil, WINDOWS_CERTSTORE[CertStore]);
  try
    ctx := CertEnumCertificatesInStore(store, nil);
    while ctx <> nil do
    begin
      certlen := 0;
      if not CryptBinaryToStringA(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, nil, certlen) then
        break;
      tmp.Init(certlen); // a PEM is very likely to be < 8KB so will be on stack
      if CryptBinaryToStringA(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, tmp.buf, certlen) then
         AppendBufferToUtf8(tmp.buf, certlen, result);
      tmp.Done;
      ctx := CertEnumCertificatesInStore(store, ctx); // next certificate
    end;
  finally
    CertCloseStore(store, 0);
  end;
end;

function SearchSmbios(const mem: RawByteString; var info: TRawSmbiosInfo): PtrUInt;
  forward; // implemented later in mormot.core.os.pas

const
  // raw SMBIOS firmware table provider signature = 'RSMB' in big-endian
  _RSMB_ = $52534D42;
  // potential location of the SMBIOS buffer pointers within a 64KB fixed frame
  SMB_START  = $000f0000;
  SMB_STOP   = $00100000;

function _GetRawSmbios(var info: TRawSmbiosInfo): boolean;
var
  siz: DWord;
  tmp: RawByteString;
  addr: PtrUInt;
  get: function(sig, id: DWord; buf: pointer; siz: DWord): PtrUInt; stdcall;
begin
  // first try to use Vista+ API which supports EFI (even on aarch64-win64!)
  get := GetProcAddress(GetModuleHandle(kernel32), 'GetSystemFirmwareTable');
  if Assigned(get) then
  begin
    siz := get(_RSMB_, 0, nil, 0); // first call to retrieve the full size
    if siz > SizeOf(info) then
    begin
      FastNewRawByteString(tmp, siz);
      get(_RSMB_, 0, pointer(tmp), siz);
      PInt64(@info)^ := PInt64(tmp)^; // header fields = 64-bit
      FastSetRawByteString(info.data, @PInt64Array(tmp)[1], siz - SizeOf(Int64));
      result := true;
      exit;
    end;
  end;
  // on XP, read directly from physical memory via ntdll.dll low-level API
  result := false;
  {$ifdef CPUINTEL} // don't even try on aarch64-win64
  tmp := ReadSystemMemory(SMB_START, SMB_STOP - SMB_START);
  if tmp = '' then
    exit;
  addr := SearchSmbios(tmp, info);
  if addr = 0 then
    exit;
  info.data := ReadSystemMemory(addr, info.Length);
  result := info.data <> '';
  {$endif CPUINTEL}
end;

procedure DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
  // not needed - GetRawSmbios() is likely to work with no administrator rights
end;

threadvar // do not publish for compilation within Delphi packages
  CoInitCounter: integer;

// avoid including ActiveX unit
function CoInitialize(_para1: pointer): HRESULT;
  stdcall; external 'ole32.dll';
procedure CoUninitialize;
  stdcall; external 'ole32.dll';

procedure CoInit;
begin
  inc(CoInitCounter); // is a threadvar: no InterlockedIncrement() needed
  if CoInitCounter = 1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  if CoInitCounter <= 0 then
    raise EOleSysError.Create('You should call TOleDBConnection.Free from the same ' +
      'thread which called its Create: i.e. call MyProps.EndCurrentThread from an ' +
      'THttpServerGeneric.OnHttpThreadTerminate event - see ticket 213544b2f5');
  dec(CoInitCounter);
  if CoInitCounter = 0 then
    CoUninitialize;
end;


{ ****************** Unix Daemon and Windows Service Support }

function OpenServiceManager(const TargetComputer, DatabaseName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
var
  t1, t2: TSynTempBuffer;
begin
  result := OpenSCManagerW(
    Utf8ToWin32PWideChar(TargetComputer, t1),
    Utf8ToWin32PWideChar(DatabaseName, t2), dwDesiredAccess);
  t1.Done;
  t2.Done;
end;

function OpenServiceInstance(hSCManager: SC_HANDLE; const ServiceName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
var
  t: TSynTempBuffer;
begin
  result := OpenServiceW(
    hSCManager, Utf8ToWin32PWideChar(ServiceName, t), dwDesiredAccess);
  t.Done;
end;


{ TServiceController }

type
  EService = class(Exception);

constructor TServiceController.CreateNewService(const TargetComputer,
  DatabaseName, Name, DisplayName: RawUtf8; const Path: TFileName;
  const OrderGroup: RawUtf8; const Dependencies: RawUtf8;
  const Username: RawUtf8; const Password: RawUtf8; DesiredAccess: cardinal;
  ServiceType: cardinal; StartType: cardinal; ErrorControl: cardinal);
var
  exeName: TFileName;
  exeNameW: SynUnicode;
  depW: PWideChar;
  i: PtrInt;
  t0, t1, t2, t3, t4, t5: TSynTempBuffer;
begin
  inherited Create;
  if Path = '' then
  begin
    TService.DoLog(sllError,
      'CreateNewService(''%'',''%'') with no Path', [Name, DisplayName], self);
    exit;
  end;
  if TargetComputer = '' then
    if GetDriveType(pointer(ExtractFileDrive(Path))) = DRIVE_REMOTE then
    begin
      exeName := ExpandUNCFileName(Path);
      if (copy(exeName, 1, 12) <> '\\localhost\') or
         (exeName[14] <> '$') then
        raise EService.CreateFmt(
          '%s.CreateNewService(''%s'',''%s'') on remote drive: Path=%s is %s',
          [ClassNameShort(self)^, Name, DisplayName, Path, exeName]);
      system.delete(exeName, 1, 12); // \\localhost\c$\... -> c:\...
      exeName[2] := ':';
    end
    else
      exeName := Path;
  exeNameW := SynUnicode(exeName); // use RTL for TFileName to UTF-16
  fName := Name;
  fSCHandle := OpenServiceManager(
    TargetComputer, DatabaseName, SC_MANAGER_ALL_ACCESS);
  if fSCHandle = 0 then
    RaiseLastError('TServiceController.CreateService: OpenServiceManager', EService);
  depW := Utf8ToWin32PWideChar(Dependencies, t0);
  if depW <> nil then
  begin
    for i := 0 to t0.len - 1 do
      if depW[i] = ';' then
        depW[i] := #0; // as expected by CreateServiceW() API
    depW[t0.len + 1] := #0; // should end with #0#0
  end;
  fHandle := CreateServiceW(fSCHandle,
    Utf8ToWin32PWideChar(Name, t1), Utf8ToWin32PWideChar(DisplayName, t2),
    DesiredAccess, ServiceType, StartType,
    ErrorControl, pointer(exeNameW), Utf8ToWin32PWideChar(OrderGroup, t3),
    nil, depW, Utf8ToWin32PWideChar(Username, t4),
    Utf8ToWin32PWideChar(Password, t5));
  t0.Done;
  t1.Done;
  t2.Done;
  t3.Done;
  t4.Done;
  t5.Done;
  if fHandle = 0 then
    RaiseLastError('TServiceController.CreateService:', EService);
  TService.DoLog(sllInfo, 
    'CreateService(''%'',''%'',''%'')', [Name, DisplayName, exeName], self);
end;

constructor TServiceController.CreateOpenService(
  const TargetComputer, DataBaseName, Name: RawUtf8; DesiredAccess: cardinal);
begin
  inherited Create;
  fName := RawUtf8(Name);
  fSCHandle := OpenServiceManager(TargetComputer, DataBaseName, GENERIC_READ);
  if fSCHandle = 0 then
  begin
    TService.DoLog(sllLastError, 'OpenSCManager(''%'',''%'') for [%]',
      [TargetComputer, DataBaseName, fName], self);
    exit;
  end;
  fHandle := OpenServiceInstance(fSCHandle, Name, DesiredAccess);
  if fHandle = 0 then
    TService.DoLog(sllLastError, 'OpenService(%)', [Name], self);
end;

function TServiceController.Delete: boolean;
begin
  result := false;
  if fHandle <> 0 then
    if DeleteService(fHandle) then
    begin
      result := CloseServiceHandle(fHandle);
      fHandle := 0;
    end
    else
      TService.DoLog(sllLastError, 'DeleteService(%)', [fName], self);
end;

destructor TServiceController.Destroy;
begin
  if fHandle <> 0 then
  begin
    CloseServiceHandle(fHandle);
    fHandle := 0;
  end;
  if fSCHandle <> 0 then
  begin
    CloseServiceHandle(fSCHandle);
    fSCHandle := 0;
  end;
  inherited;
end;

function TServiceController.GetState: TServiceState;
begin
  if (self = nil) or
     (fSCHandle = 0) or
     (fHandle = 0) then
    result := ssNotInstalled
  else
    result := CurrentStateToServiceState(GetStatus.dwCurrentState);
  TService.DoLog(sllTrace, 'GetState(%)=%', [fName, ToText(result)^], self);
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  FillCharFast(fStatus, SizeOf(fStatus), 0);
  QueryServiceStatus(fHandle, fStatus);
  result := fStatus;
end;

function TServiceController.Pause: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_PAUSE, fStatus);
end;

function TServiceController.Refresh: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_INTERROGATE, fStatus);
end;

function TServiceController.Resume: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_CONTINUE, fStatus);
end;

function TServiceController.Shutdown: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_SHUTDOWN, fStatus);
end;

function TServiceController.Start(const Args: array of PWideChar): boolean;
begin
  TService.DoLog(sllDebug, 'Start(%) Args=% Handle=%',
    [fName, length(Args), fHandle], self);
  if fHandle = 0 then
  begin
    TService.DoLog(sllError, 'Start(%): no Service', [fName], self);
    result := false;
    exit;
  end;
  if length(Args) = 0 then
    result := StartServiceW(fHandle, 0, nil)
  else
    result := StartServiceW(fHandle, length(Args), @Args[0]);
  if not result then
    TService.DoLog(sllLastError, 'Start(%) failed', [fName], self);
end;

function TServiceController.Stop: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_STOP, fStatus);
end;

function TServiceController.SetDescription(const Description: RawUtf8): boolean;
var
  sd: TServiceDescription;
  t: TSynTempBuffer;
begin
  if Description = '' then
  begin
    result := false;
    exit;
  end;
  sd.lpDestription := Utf8ToWin32PWideChar(Description, t);
  result := ChangeServiceConfig2W(fHandle, SERVICE_CONFIG_DESCRIPTION, @sd);
  t.Done;
end;

class procedure TServiceController.CheckParameters(
  const ExeFileName: TFileName; const ServiceName, DisplayName,
  Description: RawUtf8; const Dependencies: RawUtf8);
var
  param: string;
  i: integer;

  procedure ShowError(Msg: RawUtf8);
  begin
    Msg := _fmt('%s: "%s" failed for %s', [ServiceName, Msg, param]);
    TService.DoLog(sllLastError, '%', [Msg], nil);
    ConsoleWrite(Msg, ccLightRed);
  end;

begin
  for i := 1 to ParamCount do
  begin
    param := SysUtils.LowerCase(ParamStr(i));
    TService.DoLog(sllInfo,
      'Controling % with command [%]', [ServiceName, param], nil);
    if param = '/install' then
      TServiceController.Install(
        ServiceName, DisplayName, Description, true, ExeFileName, Dependencies)
    else
      with TServiceController.CreateOpenService('', '', ServiceName) do
      try
        if State = ssErrorRetrievingState then
          ShowError('State')
        else if param = '/uninstall' then
        begin
          if not Stop then
            ShowError('Stop');
          if not Delete then
            ShowError('Delete');
        end
        else if param = '/stop' then
        begin
          if not Stop then
            ShowError('Stop');
        end
        else if param = '/start' then
        begin
          if not Start([]) then
            ShowError('Start');
        end;
      finally
        Free;
      end;
  end;
end;

class function TServiceController.Install(
  const Name, DisplayName, Description: RawUtf8; AutoStart: boolean;
  ExeName: TFileName; const Dependencies, Username, Password: RawUtf8): TServiceState;
var
  ctrl: TServiceController;
  start: cardinal;
begin
  if AutoStart then
    start := SERVICE_AUTO_START
  else
    start := SERVICE_DEMAND_START;
  if ExeName = '' then
    ExeName := Executable.ProgramFileName;
  ctrl := TServiceController.CreateNewService(
    '', '', Name, DisplayName, ExeName, '', Dependencies, UserName, Password,
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, start);
  try
    result := ctrl.State;
    if result <> ssNotInstalled then
      ctrl.SetDescription(Description);
    TService.DoLog(sllDebug,
      'Install(%)=%', [Name, ToText(result)^], ctrl);
  finally
    ctrl.Free;
  end;
end;

class function TServiceController.CurrentState(const Name: RawUtf8): TServiceState;
begin
  try
    with CreateOpenService('', '', Name, SERVICE_QUERY_STATUS) do
      try
        result := GetState;
      finally
        Free;
      end;
  except
    result := ssErrorRetrievingState;
  end;
end;


{ TService }

class procedure TService.DoLog(Level: TSynLogLevel; const Fmt: RawUtf8;
  const Args: array of const; Instance: TObject);
begin
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(Level, Fmt, Args, Instance);
end;

constructor TService.Create(const aServiceName, aDisplayName: RawUTf8);
begin
  fServiceName := aServiceName;
  if aDisplayName = '' then
    fDisplayName := aServiceName
  else
    fDisplayName := aDisplayName;
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  DoLog(sllDebug, 'Create: % (%) running as [%]',
    [ServiceName, aDisplayName, Executable.ProgramFullSpec], self);
end;

procedure TService.CtrlHandle(Code: cardinal);
begin
  DoCtrlHandle(Code);
end;

const
  _CMD: array[0.. 5] of string[11] = (
    'UNKNOWN', 'STOP', 'PAUSE', 'CONTINUE', 'INTERROGATE', 'SHUTDOWN');

procedure TService.DoCtrlHandle(Code: cardinal);
var
  c: PShortString;
begin
  if Code <= high(_CMD) then
    c := @_CMD[Code]
  else
    c := @_CMD[0];
  DoLog(sllTrace, '% DoCtrlHandle(SERVICE_CONTROL_%=%)',
    [ServiceName, c^, Code], self);
  try
    case Code of
      SERVICE_CONTROL_STOP:
        begin
          ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnStop) then
              fOnStop(Self);
            ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
          except
            ReportStatus(SERVICE_STOPPED, ERROR_CAN_NOT_COMPLETE, 0);
          end;
        end;
      SERVICE_CONTROL_PAUSE:
        begin
          ReportStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnPause) then
              fOnPause(Self);
            ReportStatus(SERVICE_PAUSED, NO_ERROR, 0)
          except
            ReportStatus(SERVICE_PAUSED, ERROR_CAN_NOT_COMPLETE, 0)
          end;
        end;
      SERVICE_CONTROL_CONTINUE:
        begin
          ReportStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnResume) then
              fOnResume(Self);
            ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
          except
            ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
          end;
        end;
      SERVICE_CONTROL_SHUTDOWN:
        begin
          if Assigned(fOnShutdown) then
            fOnShutdown(Self);
          Code := 0;
        end;
      SERVICE_CONTROL_INTERROGATE:
        begin
          SetServiceStatus(fStatusHandle, fStatusRec);
          if Assigned(fOnInterrogate) then
            fOnInterrogate(Self);
        end;
    end;
    if Assigned(fOnControl) then
      fOnControl(Self, Code);
  except
  end;
  DoLog(sllTrace, '% DoCtrlHandle(SERVICE_CONTROL_%=%) finished',
    [ServiceName, c^, Code], self);
end;

procedure TService.Execute;
begin
  try
    if Assigned(fOnStart) then
      fOnStart(Self);
    ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
    if Assigned(fOnExecute) then
      fOnExecute(Self);
  except
    ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
  end;
  DoLog(sllTrace, '% Execute finished', [ServiceName], self);
end;

function TService.GetArgCount: integer;
begin
  result := length(fArgsList);
end;

function TService.GetArgs(Idx: integer): RawUtf8;
begin
  if cardinal(Idx) > cardinal(high(fArgsList)) then
    result := ''
  else
    // avoid GPF
    result := fArgsList[Idx];
end;

function TService.GetControlHandler: TServiceControlHandler;
begin
  result := fControlHandler;
  if not Assigned(result) then
    DoLog(sllError, '% GetControlHandler=nil: use TServiceSingle or ' +
      'assign a custom ControlHandler', [ServiceName], self);
end;

function TService.GetInstalled: boolean;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_QUERY_STATUS) do
  try
    result := Handle <> 0;
  finally
    Free;
  end;
end;

function TService.Install(const Params: TFileName): boolean;
var
  schService: SC_HANDLE;
  schSCManager: SC_HANDLE;
  ServicePath: TFileName;
  p: SynUnicode;
  t1, t2: TSynTempBuffer;
begin
  result := false;
  if installed then
    exit;
  ServicePath := Executable.ProgramFileName;
  if Params <> '' then
    ServicePath := ServicePath + ' ' + Params;
  p := SynUnicode(ServicePath); // use RTL for TFileName to UTF-16 conversion
  schSCManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if schSCManager <= 0 then
    exit;
  schService := CreateServiceW(schSCManager,
    Utf8ToWin32PWideChar(fServiceName, t1),
    Utf8ToWin32PWideChar(fDisplayName, t2),
    SERVICE_ALL_ACCESS, fServiceType, fStartType, SERVICE_ERROR_NORMAL,
    pointer(p), nil, nil, nil, nil, nil);
  t1.Done;
  t2.Done;
  if schService > 0 then
  begin
    result := true;
    CloseServiceHandle(schService);
  end;
  CloseServiceHandle(schSCManager);
end;

procedure TService.Remove;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    if Handle = 0 then
      exit;
    Stop;
    Delete;
  finally
    Free;
  end;
end;

function TService.ReportStatus(dwState, dwExitCode, dwWait: cardinal): BOOL;
var
  status: PShortString;
begin
  status := ToText(CurrentStateToServiceState(dwState));
  DoLog(sllTrace, '% ReportStatus(%=%,%)=%', [ServiceName,
    WinErrorConstant(dwExitCode)^, dwExitCode, dwWait, status^], self);
  if dwState = SERVICE_START_PENDING then
    fStatusRec.dwControlsAccepted := 0
  else
    fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwCurrentState := dwState;
  fStatusRec.dwWin32ExitCode := dwExitCode;
  fStatusRec.dwWaitHint := dwWait;
  if (dwState = SERVICE_RUNNING) or
     (dwState = SERVICE_STOPPED) then
    fStatusRec.dwCheckPoint := 0
  else
    inc(fStatusRec.dwCheckPoint);
  result := SetServiceStatus(fStatusHandle, fStatusRec);
  if not result then
    DoLog(sllLastError, '% ReportStatus(%,%)=% SetServiceStatus() failed',
      [ServiceName, dwExitCode, dwWait, status^], self);
end;

procedure TService.SetControlHandler(const Value: TServiceControlHandler);
begin
  fControlHandler := Value;
end;

procedure TService.SetStatus(const Value: TServiceStatus);
begin
  fStatusRec := Value;
  if fStatusHandle <> 0 then
    SetServiceStatus(fStatusHandle, fStatusRec);
end;

procedure TService.Start;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    Start([]);
  finally
    Free;
  end;
end;

procedure TService.Stop;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    Stop;
  finally
    Free;
  end;
end;

procedure TService.ServiceProc(ArgCount: integer; Args: PPWideChar);
var
  i: PtrInt;
  t: TSynTempBuffer;
begin
  SetCurrentThreadName('ServiceProc');
  DoLog(sllTrace, 'ServiceProc: ArgCount=% ServiceSingle=%',
    [ArgCount, self], self);
  if self = nil then
    exit;
  dec(ArgCount); // first argument is the service name to be ignored
  if (Args = nil) or
     (ArgCount <= 0) then
    fArgsList := nil // no argument
  else
  begin
    SetLength(fArgsList, ArgCount);
    for i := 0 to ArgCount - 1 do
    begin
      inc(Args); // first was service name
      Win32PWideCharToUtf8(Args^, fArgsList[i]); // to string
    end;
  end;
  fStatusHandle := RegisterServiceCtrlHandlerW(
    Utf8ToWin32PWideChar(fServiceName, t), @ControlHandler);
  t.Done;
  if fStatusHandle = 0 then
  begin
    ReportStatus(SERVICE_STOPPED, GetLastError, 0);
    exit;
  end;
  ReportStatus(SERVICE_START_PENDING, 0, 0);
  Execute;
end;


function CurrentStateToServiceState(CurrentState: cardinal): TServiceState;
begin
  case CurrentState of
    SERVICE_STOPPED:
      result := ssStopped;
    SERVICE_START_PENDING:
      result := ssStarting;
    SERVICE_STOP_PENDING:
      result := ssStopping;
    SERVICE_RUNNING:
      result := ssRunning;
    SERVICE_CONTINUE_PENDING:
      result := ssResuming;
    SERVICE_PAUSE_PENDING:
      result := ssPausing;
    SERVICE_PAUSED:
      result := ssPaused;
  else
    // e.g. SERVICE_CONTROL_SHUTDOWN
    result := ssNotInstalled;
  end;
end;

function GetServicePid(const aServiceName: RawUtf8;
  aServiceState: PServiceState): cardinal;
var
  ss: TServiceState;
  st: TServiceStatus;
  ssp: TServiceStatusProcess;
  scm: THandle;
  svc: THandle;
  size: cardinal;
begin
  result := 0;
  ss := ssErrorRetrievingState;
  scm := OpenSCManagerW(nil, nil, SC_MANAGER_CONNECT);
  if scm <> 0 then
  try
    svc := OpenServiceInstance(scm, aServiceName, SERVICE_QUERY_STATUS);
    if svc <> 0 then
    try
      if QueryServiceStatusEx(svc, SC_STATUS_PROCESS_INFO,
          @ssp, SizeOf(TServiceStatusProcess), size) then
      begin
        result := ssp.dwProcessId;
        if aServiceState <> nil then
        begin
          FillCharFast(st, SizeOf(st), 0);
          QueryServiceStatus(svc, st);
          ss := CurrentStateToServiceState(st.dwCurrentState);
        end;
      end
      else
        TService.DoLog(sllLastError, 'GetServicePid(%)', [aServiceName], nil);
    finally
      CloseServiceHandle(svc);
    end
    else
      ss := ssNotInstalled;
  finally
    CloseServiceHandle(scm);
  end;
  if aServiceState <> nil then
    aServiceState^ := ss;
end;

{  function that a service process specifies as the entry point function
   of a particular service. The function can have any application-defined name
  - Args points to an array of pointers that point to null-terminated
    argument strings. The first argument in the array is the name of the service,
    and subsequent arguments are any strings passed to the service by the process
    that called the StartService function to start the service. Args can
    be nil if there are no arguments. }

procedure ServiceProc(ArgCount: cardinal; Args: PPWideChar); stdcall;
begin
  ServiceSingle.ServiceProc(ArgCount, Args);
end;

function ServiceSingleRun: boolean;
var
  S: array[0..1] of TServiceTableEntry;
  t: TSynTempBuffer;
begin
  if ServiceSingle = nil then
  begin
    result := false;
    exit;
  end;
  S[0].lpServiceName := Utf8ToWin32PWideChar(ServiceSingle.ServiceName, t);
  S[0].lpServiceProc := ServiceProc;
  S[1].lpServiceName := nil;
  S[1].lpServiceProc := nil;
  { TODO : disable EExternal exception logging in ServicesSingleRun? }
  result := StartServiceCtrlDispatcherW(@S);
  t.Done;
end;


{ TServiceSingle }

procedure SingleServiceControlHandler(Opcode: LongWord); stdcall;
begin
  if ServiceSingle <> nil then
    ServiceSingle.DoCtrlHandle(Opcode);
end;

constructor TServiceSingle.Create(const aServiceName, aDisplayName: RawUtf8);
begin
  if ServiceSingle <> nil then
    raise EOSException.Create('Only one TServiceSingle is allowed at a time');
  inherited Create(aServiceName, aDisplayName);
  ServiceSingle := self;
  SetControlHandler(SingleServiceControlHandler);
end;

destructor TServiceSingle.Destroy;
begin
  try
    inherited;
  finally
    if ServiceSingle = self then
      ServiceSingle := nil;
  end;
end;


function WaitProcess(pid: cardinal; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  result := false;
  ph := OpenProcess(SYNCHRONIZE, false, pid);
  if ph = 0 then
    exit;
  result := WaitForSingleObject(ph, waitseconds * 1000) = WAIT_OBJECT_0;
  CloseHandle(ph);
end;

function CancelProcess(pid: cardinal; waitseconds: integer): boolean;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  if GetConsoleWindow <> 0 then              // can attach to a single console
    FreeConsole;
  if not AttachConsole(pid) then             // attach to the pid console
    exit;
  SetConsoleCtrlHandler(nil, true);          // nil=ignore the event ourself
  GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0); // send Ctrl+C event
  FreeConsole;                               // detach
  SetConsoleCtrlHandler(nil, false);         // remove our nil=ignore handler
  result := WaitProcess(pid, waitseconds);
end;

function QuitProcess(pid: cardinal; waitseconds: integer): boolean;
var
  snap: THandle;
  e: TThreadEntry32;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if snap <= 0 then
    exit;
  FillCharFast(e, SizeOf(e), 0);
  e.dwSize := SizeOf(e);
  if Thread32First(snap, e) then // loop over all threads of the system
    repeat
      if e.th32OwnerProcessID = pid then
        if PostThreadMessage(e.th32ThreadID, WM_QUIT, 0, 0) then
          result := true; // at least one thread found
    until not Thread32Next(snap, e);
  CloseHandle(snap);
  if result and
     (waitseconds <> 0) then
    result := WaitProcess(pid, waitseconds);
end;

function KillProcess(pid: cardinal; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  ph := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, false, pid);
  if ph = 0 then
    exit;
  result := TerminateProcess(ph, 0) and
            (WaitForSingleObject(ph, waitseconds * 1000) <> WAIT_TIMEOUT);
  CloseHandle(ph);
end;

var
  OnHandleCtrlC: TThreadMethod;

function ConsoleCtrlHandler(typ : dword) : BOOL; stdcall;
begin
  result := false;
  if Assigned(OnHandleCtrlC) then
    case typ of
      CTRL_C_EVENT,
      CTRL_CLOSE_EVENT,
      CTRL_LOGOFF_EVENT,
      CTRL_SHUTDOWN_EVENT:
        begin
          OnHandleCtrlC();
          result := true;
        end;
    end;
end;

function HandleCtrlC(const OnClose: TThreadMethod): boolean;
begin
  result := SetConsoleCtrlHandler(@ConsoleCtrlHandler, Assigned(OnClose));
  if result then
    OnHandleCtrlC := OnClose;
end;

function DropPriviledges(const UserName: RawUtf8): boolean;
begin
  result := false;
end;

function ChangeRoot(const FolderName: RawUtf8): boolean;
begin
  result := false;
end;

type
  TJobObjectInfoClass = (
    BasicLimitInformation              = 2,
    JobObjectBasicProcessIdList        = 3,
    BasicUIRestrictions                = 4,
    SecurityLimitInformation           = 5,
    EndOfJobTimeInformation            = 6,
    AssociateCompletionPortInformation = 7,
    ExtendedLimitInformation           = 9,
    GroupInformation                   = 11);

  TJobObjectBasicLimitInformation = record
    PerProcessUserTimeLimit: LARGE_INTEGER;
    PerJobUserTimeLimit:     LARGE_INTEGER;
    LimitFlags:            DWord;
    MinimumWorkingSetSize: PtrUInt;
    MaximumWorkingSetSize: PtrUInt;
    ActiveProcessLimit:    DWord;
    Affinity:              PtrUInt;
    PriorityClass:         DWord;
    SchedulingClass:       DWord;
  end;

  TIOCounter = record
    ReadOperationCount:  QWord;
    WriteOperationCount: QWord;
    OtherOperationCount: QWord;
    ReadTransferCount:   QWord;
    WriteTransferCount:  QWord;
    OtherTransferCount:  QWord;
  end;

  TJobObjectExtendedLimitInformation = record
    BasicLimitInformation: TJobObjectBasicLimitInformation;
    IoInfo:                TIOCounter;
    ProcessMemoryLimit:    PtrUInt;
    JobMemoryLimit:        PtrUInt;
    PeakProcessMemoryUsed: PtrUInt;
    PeakJobMemoryUsed:     PtrUInt;
  end;

const
  // to create a child process in a new job object
  // https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects
  CREATE_BREAKAWAY_FROM_JOB =  $1000000;

  JOB_OBJECT_LIMIT_PROCESS_MEMORY             = $00000100;
  JOB_OBJECT_LIMIT_JOB_MEMORY                 = $00000200;
  JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION = $00000400;
  JOB_OBJECT_LIMIT_BREAKAWAY_OK               = $00000800;
  JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK        = $00001000;
  JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE          = $00002000;

function CreateJobObjectA(lpJobAttributes: PSecurityAttributes;
   lpName: PWideChar): THandle;
  stdcall; external kernel32;
function SetInformationJobObject(hJob: THandle;
   JobObjectInformationClass: TJobObjectInfoClass; lpJobObjectInformation: pointer;
   cbJobObjectInformationLength: DWord): BOOL;
  stdcall; external kernel32;
function AssignProcessToJobObject(hJob, hProcess: THandle): BOOL;
  stdcall; external kernel32;

// redefined here so that we can share code with FPC and Delphi
function CreateProcessW(lpApplicationName: PWideChar; lpCommandLine: PWideChar;
   lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
   bInheritHandles: BOOL; dwCreationFlags: cardinal; lpEnvironment: pointer;
   lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfo;
   out lpProcessInformation: TProcessInformation): BOOL;
  stdcall; external kernel32;

function GetExitCodeProcess(hProcess: THandle; out lpExitCode: cardinal): BOOL;
  stdcall; external kernel32;

function CreateJobToClose(parentpid: cardinal): THandle;
var
  security: TSecurityAttributes;
  limits: TJobObjectExtendedLimitInformation;
  jobname: RawUtf8;
begin
  security.nLength := SizeOf(security);
  security.bInheritHandle := false; // should be false
  security.lpSecurityDescriptor := nil;
  _fmt('AutoCloseChild%d', [parentpid], jobname);
  result := CreateJobObjectA(@security, pointer(jobname));
  if result = 0 then
    exit;
  FillCharFast(limits, SizeOf(limits), 0);
  limits.BasicLimitInformation.LimitFlags :=
    JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE or
    JOB_OBJECT_LIMIT_BREAKAWAY_OK;
  if SetInformationJobObject(result, ExtendedLimitInformation,
       @limits, SizeOf(limits)) then
    exit;
  CloseHandle(result); // error initializing the job (too old or too new OS?)
  result := 0;
end;

function AssignJobToProcess(job, process: THandle; const ctxt: ShortString): boolean;
begin
  result := (job <> 0) and
            AssignProcessToJobObject(job, process);
  if result then
    TService.DoLog(sllTrace, 'RunCommand: % AssignProcessToJobObject success',
      [ctxt], nil)
  else
    TService.DoLog(sllDebug, 'RunCommand: % AssignProcessToJobObject failed % %',
      [ctxt, GetLastError, WinErrorConstant(GetLastError)^], nil);
end;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2, arg3, arg4, arg5, env: TFileName; options: TRunOptions): integer;
begin
  result := RunCommand(Format('"%s" %s %s %s %s %s',
    [path, arg1, arg2, arg3, arg4, arg5]), waitfor, env, options);
end;

var
  EnvironmentCache: SynUnicode;
  EnvironmentCacheLock: TLightLock; // just set once

procedure GetEnvironmentCache;
var
  e, p: PWideChar;
begin
  EnvironmentCacheLock.Lock;
  if EnvironmentCache = '' then
  begin
    e := GetEnvironmentStringsW;
    p := e;
    while p^ <> #0 do
      inc(p, StrLenW(p) + 1); // go to name=value#0 pairs end
    FastSynUnicode(EnvironmentCache, e, (PtrUInt(p) - PtrUInt(e)) shr 1);
    FreeEnvironmentStringsW(e);
  end;
  EnvironmentCacheLock.UnLock;
end;

function RunCommand(const cmd: TFileName; waitfor: boolean; const env: TFileName;
  options: TRunOptions; waitfordelayms: cardinal; processhandle: PHandle;
  redirected: PRawByteString; const onoutput: TOnRedirect;
  const wrkdir: TFileName): integer;
var
  processinfo: TProcessInformation;
begin
  result := RunCommandWin(cmd, waitfor, processinfo, env, options,
    waitfordelayms, redirected, onoutput, wrkdir);
  if processhandle <> nil then
    processhandle^ := processinfo.hProcess;
end;

function RunCommandWin(const cmd: TFileName; waitfor: boolean;
  var processinfo: TProcessInformation; const env: TFileName;
  options: TRunOptions; waitfordelayms: cardinal;
  redirected: PRawByteString; const onoutput: TOnRedirect;
  const wrkdir: TFileName): integer;
var
  startupinfo: TStartupInfo; // _STARTUPINFOW or _STARTUPINFOA is equal here
  security: TSecurityAttributes;
  exe, path: TFileName;
  rd, wr, job: THandle;
  // CreateProcess can alter the strings -> use local SynUnicode temp variables
  wcmd, wenv, wpath: SynUnicode;
  endtix: Int64;
  flags, exitcode, err, res: cardinal;
  ram: TRunAbortMethods;
  created, terminated: boolean;
  i, l: PtrInt;

  procedure RedirectOutput(flush: boolean);
  var
    new: RawByteString;
    pending, n: cardinal;
    tmp: TSynTempBuffer;
  begin
    repeat
      pending := 0;
      if not PeekNamedPipe(rd, nil, 0, nil, @pending, nil) or
         (pending = 0) then
      begin
        if (not flush) and
           Assigned(onoutput) and
           onoutput('', processinfo.dwProcessId) then
            exitcode := WAIT_OBJECT_0; // onoutput() returned true to abort
        break;
      end;
      if pending > SizeOf(tmp) then
        pending := SizeOf(tmp);
      n := 0;
      Win32Check(ReadFile(rd, tmp, pending, n, nil));
      if n <= 0 then
        break;
      if redirected <> nil then
      begin
        SetLength(redirected^, l + PtrInt(n));
        MoveFast(tmp, PByteArray(redirected^)[l], n); // append without convert
        inc(l, n);
      end;
      if Assigned(onoutput) then
      begin
        SetString(new, PAnsiChar(@tmp), n);
        if onoutput(new, processinfo.dwProcessId) then // notify new content
          // onoutput() callback returned true to stop the execution
          if not flush then
          begin
            exitcode := WAIT_OBJECT_0;
            break;
          end;
      end;
    until false;
  end;

begin
  // https://support.microsoft.com/en-us/help/175986/info-understanding-createprocess-and-command-line-arguments
  result := -1;
  FillCharFast(processinfo, SizeOf(processinfo), 0);
  // extract path and exe from cmd input
  if cmd = '' then
    exit;
  if cmd[1] = '"' then
  begin
    exe := copy(cmd, 2, maxInt);
    i := Pos('"', exe);
    if i = 0 then
      exit;
    SetLength(exe, i - 1); // unquote "exe" string
  end
  else
  begin
    i := Pos(' ', cmd);
    if i = 0 then
      exe := cmd // no parameter
    else
      exe := copy(cmd, 1, i - 1); // split exe and parameter(s)
  end;
  path := wrkdir;
  if (path = '') and
     (exe <> '') then
    path := ExtractFilePath(ExpandFileName(exe));
  if (path = '') and
     FileExists(Executable.ProgramFilePath + exe) then
    path := Executable.ProgramFilePath; // prefers the current folder
  // prepare the CreateProcess arguments
  wcmd := SynUnicode(cmd);
  UniqueString(wcmd);
  wpath := SynUnicode(path);
  if env <> '' then
  begin
    wenv := SynUnicode(env);
    if roEnvAddExisting in options then
    begin
      if EnvironmentCache = '' then
        GetEnvironmentCache;
      wenv := EnvironmentCache + wenv;
    end
    else
      UniqueString(wenv);
  end;
  security.nLength := SizeOf(security);
  security.bInheritHandle := true;
  security.lpSecurityDescriptor := nil;
  // launch the process
  FillCharFast(startupinfo, SizeOf(startupinfo), 0);
  startupinfo.cb := SizeOf(startupinfo);
  ram := RunAbortMethods;
  l := 0;
  rd := 0;
  job := 0;
  if Assigned(onoutput) or
     (redirected <> nil) then
    if CreatePipe(rd, wr, @security, 0) then
    begin
      SetHandleInformation(rd, HANDLE_FLAG_INHERIT, 0);
      startupinfo.wShowWindow := SW_HIDE;
      startupinfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      startupinfo.hStdOutput := wr;
      startupinfo.hStdError := wr;
      startupinfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      if redirected <> nil then
        redirected^ := '';
      exclude(ram, ramCtrlC); // not compatible with redirection
    end
    else
      rd := 0;
  // https://docs.microsoft.com/en-en/windows/desktop/ProcThread/process-creation-flags
  flags := CREATE_UNICODE_ENVIRONMENT or CREATE_DEFAULT_ERROR_MODE;
  if not (roWinNoProcessDetach in options) then
    flags := flags or (DETACHED_PROCESS or CREATE_NEW_PROCESS_GROUP);
  if roWinJobCloseChildren in options then
    // create the child process in a new job object
    flags := flags or CREATE_BREAKAWAY_FROM_JOB;
  // actually create the new process
  created := CreateProcessW(nil, pointer(wcmd), @security, @security, true,
    flags, pointer({%H-}wenv), pointer(wpath), startupinfo, processinfo);
  if (not created) and
     (flags and CREATE_BREAKAWAY_FROM_JOB <> 0) then
  begin
    err := GetLastError;
    TService.DoLog(sllTrace,
      'RunCommand: unsupported CREATE_BREAKAWAY_FROM_JOB = % %',
      [err, WinErrorConstant(err)^], nil);
    flags := flags and (not CREATE_BREAKAWAY_FROM_JOB);
    wcmd := SynUnicode(cmd); // CreateProcesW() modified wcmd content: recreate
    UniqueString(wcmd);
    created := CreateProcessW(nil, pointer(wcmd), @security, @security, true,
      flags, pointer({%H-}wenv), pointer(wpath), startupinfo, processinfo);
  end;
  if not created then
  begin
    err := GetLastError;
    TService.DoLog(sllTrace,  'RunCommand: CreateProcess = % %',
      [err, WinErrorConstant(err)^], nil);
    result := -err; // returns CreateProcessW() error as negative
    exit;
  end;
  // setup the newly created process
  if flags and CREATE_BREAKAWAY_FROM_JOB <> 0 then
  begin
    job := CreateJobToClose(processinfo.dwProcessID);
    if (job <> 0) and
       not AssignJobToProcess(job, processinfo.hProcess, 'CloseChildren') then
    begin
      CloseHandle(job);
      job := 0;
    end;
  end;
  if Assigned(onoutput) then
    onoutput('', processinfo.dwProcessId);
  // main wait (and redirect) until the process is finished (or not)
  if rd <> 0 then
  begin
    // wait and redirect - see https://stackoverflow.com/a/25725197/458259
    CloseHandle(wr);
    if waitfordelayms = INFINITE then
      endtix := 0
    else
      endtix := GetTickCount64 + waitfordelayms;
    repeat
      exitcode := WaitForSingleObject(processinfo.hProcess, 50);
      // note: WaitForMultipleObjects() with rd burns 100% of one core :(
      Win32Check(exitcode <> WAIT_FAILED);
      RedirectOutput({flush=}false);
    until (exitcode = WAIT_OBJECT_0) or
          ((endtix <> 0) and
           (GetTickCount64 > endtix));
    if GetExitCodeProcess(processinfo.hProcess, exitcode) and
       (exitcode <> STILL_ACTIVE) then
      result := exitcode // process ended from natural death -> return code
    else
    begin
      result := -GetLastError; // not able to retrieve exit code
      // e.g. -STILL_ACTIVE if aborted by onoutput()=true above
      terminated := false;
      if RunAbortTimeoutSecs > 0 then
      begin
        if ramCtrlC in ram then  // try Ctrl+C (disabled above)
        begin
          terminated := CancelProcess(processinfo.dwProcessId, RunAbortTimeoutSecs);
          TService.DoLog(sllTrace, 'RunCommand: CancelProcess(%)=%',
            [processinfo.dwProcessId, ord(terminated)], nil);
        end;
        if (not terminated) and
           (ramQuit in ram) then
        begin // try WM_QUIT
          terminated := QuitProcess(processinfo.dwProcessId, 0);
          TService.DoLog(sllTrace, 'RunCommand: QuitProcess(%)=%',
            [processinfo.dwProcessId, ord(terminated)], nil);
          if terminated then
          begin
            endtix := GetTickCount64 + RunAbortTimeoutSecs * 1000; // wait ended
            repeat
              res := WaitForSingleObject(processinfo.hProcess, 10);
              RedirectOutput({flush=}true); // mandatory to unlock pipe
            until (res <> WAIT_TIMEOUT) or
                  (GetTickCount64 > endtix);
            terminated := res = WAIT_OBJECT_0;
          end;
        end;
      end;
      RedirectOutput({flush=}true); // ensure there is no pending data
      if terminated and
         // gracefully ended -> try to retrieve the exit code
         GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := exitcode
      else
      begin
        TerminateProcess(processinfo.hProcess, result); // forced kill
        TService.DoLog(sllTrace, 'RunCommand: TerminateProcess(%)=%',
          [processinfo.dwProcessId, result], nil);
      end;
    end;
  end
  else if waitfor then
    if WaitForSingleObject(processinfo.hProcess, waitfordelayms) = WAIT_FAILED then
      if waitfordelayms <> INFINITE then
        result := -1 // still runing after waitfordelayms
      else
        result := -GetLastError // failed to wait
    else if GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := exitcode      // waited for process to end -> return code
      else
        result := -GetLastError // was not able to retrieve exit code
  else
    // waitfor is false: asynchronous process launch
    result := 0;
  // release the handles created for this process
  CloseHandle(processinfo.hProcess);
  CloseHandle(processinfo.hThread);
  if rd <> 0 then // CloseHandle(wr) has already be done
    CloseHandle(rd);
  if job <> 0 then
    CloseHandle(job);
end;

function RunRedirect(const cmd: TFileName; exitcode: PInteger;
  const onoutput: TOnRedirect; waitfordelayms: cardinal; setresult: boolean;
  const env, wrkdir: TFileName; options: TRunOptions): RawByteString;
var
  res: integer;
  redir: PRawByteString;
begin
  result := '';
  if setresult then
    redir := @result
  else
    redir := nil;
  res := RunCommand(cmd, true, env, options, waitfordelayms, nil,
    redir, onoutput, wrkdir);
  if exitcode <> nil then
    exitcode^ := res;
end;


{ ****************** Gather Operating System Information }

const
  // lpMinimumApplicationAddress retrieved from Windows is very low ($10000)
  // - i.e. maximum number of ID per table would be 65536 in TOrm.GetID
  // - so we'll force an higher and almost "safe" value as 1,048,576
  // (real value from runnning Windows is greater than $400000)
  MIN_PTR_VALUE = $100000;

  // see http://msdn.microsoft.com/en-us/library/ms724833(v=vs.85).aspx
  VER_NT_WORKSTATION = 1;
  VER_NT_DOMAIN_CONTROLLER = 2;
  VER_NT_SERVER = 3;
  SM_SERVERR2 = 89;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

type
  TSystemLogicalProcessorRelation = (
    RelationProcessorCore,
    RelationNumaNode,
    RelationCache,
    RelationProcessorPackage,
    RelationGroup);
  TSystemLogicalProcessorCache = (
    CacheUnified,
    CacheInstruction,
    CacheData,
    CacheTrace);

  {$ifdef CPU64}
  {$A8}
  {$else}
  {$A4}
  {$endif CPU64}
  TSystemLogicalProcessorInformation = record
    ProcessorMask: PtrUInt;
    case Relationship: TSystemLogicalProcessorRelation of
      RelationProcessorCore: (
        ProcessorCoreFlags: BYTE);
      RelationNumaNode: (
        NumaNodeNumber: DWord);
      RelationCache: (
        Cache: record
          Level: BYTE;
          Associativity: BYTE;
          LineSize: WORD;
          Size: DWord;
          CacheType: TSystemLogicalProcessorCache;
        end);
      RelationGroup: (
        Reserved: array [0..1] of QWord); // to define the actual struct size
  end;
  {$A+}


{$ifndef UNICODE}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL;
  stdcall; external kernel32 name 'GetVersionExA';
{$endif UNICODE}

function GetLocalTimeOffset: integer; // not defined in oldest Delphi
var
  tzi: TTimeZoneInformation;
begin
   case GetTimeZoneInformation(tzi) of
     TIME_ZONE_ID_UNKNOWN:
       result := tzi.Bias;
     TIME_ZONE_ID_STANDARD:
       result := tzi.Bias + tzi.StandardBias;
     TIME_ZONE_ID_DAYLIGHT:
       result := tzi.Bias + tzi.DaylightBias;
   else
     result := 0;
   end;
end;

function UUID_CACHE: TFileName;
begin // where to cache our computed UUID as a local file
  result := GetSystemPath(spCommonData) + 'synopse.uuid';
end;

const
  IMAGE_FILE_MACHINE_UNKNOWN = 0; // if the process is not under WOW64
  IMAGE_FILE_MACHINE_I386    = $014c;
  IMAGE_FILE_MACHINE_AMD64   = $8664;
  IMAGE_FILE_MACHINE_ARM64   = $aa64;

procedure InitializeSpecificUnit;
var
  h: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  IsWow64Process2: function(Handle: THandle; var Process, Machine: word): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var SystemInfo: TSystemInfo); stdcall;
  GetLogicalProcessorInformation: function(
   var Info: TSystemLogicalProcessorInformation; Len: PDWord): BOOL; stdcall;
  wine_get_version: function: PAnsiChar; stdcall;
  mem: TMemoryStatusEx;
  res: BOOL;
  process, machine: word;
  P: pointer;
  Vers: TWindowsVersion;
  cpu, manuf, prod, prodver: RawUtf8;
  reg: TWinRegistry;
  proc: array of TSystemLogicalProcessorInformation;
  i: integer;
  siz: DWord;
begin
  {$ifdef ASMX86}
  {$ifndef HASNOSSE2}
  if not (cfSSE2 in CpuFeatures) then
  begin
    // avoid illegal opcode in MoveFast() and SynLZ functions
    {$ifdef ISDELPHI} // FPC_X86 already redirect to FastCode RTL Move()
    RedirectCode(@MoveFast, @System.Move);
    {$endif ISDELPHI}
    RedirectCode(@SynLZcompress1, @SynLZcompress1Pas);
    RedirectCode(@SynLZdecompress1, @SynLZdecompress1Pas);
    ConsoleWrite('WARNING: too old CPU - recompile with HASNOSSE2', ccLightRed);
    // note: FillCharFast is handled by mormot.core.base via ERMSB
    // and Byte/Word/IntegerScanIndex() are likely to GPF at runtime
  end;
  {$endif HASNOSSE2}
  {$endif ASMX86}
  // late-binding of newest Windows APIs
  h := GetModuleHandle(kernel32);
  GetTickCount64 := GetProcAddress(h, 'GetTickCount64'); // Vista+
  if not Assigned(GetTickCount64) then
    GetTickCount64 := @GetTickCount64ForXP;
  GetSystemTimePreciseAsFileTime :=
    GetProcAddress(h, 'GetSystemTimePreciseAsFileTime'); // Win8+
  if not Assigned(GetSystemTimePreciseAsFileTime) then
    GetSystemTimePreciseAsFileTime := @GetSystemTimeAsFileTime;
  CreateSymbolicLinkW := GetProcAddress(h, 'CreateSymbolicLinkW'); // Vista+
  {$ifdef WITH_VECTOREXCEPT}
  AddVectoredExceptionHandler :=
    GetProcAddress(h, 'AddVectoredExceptionHandler');
  {$endif WITH_VECTOREXCEPT}
  QueryPerformanceFrequency(PInt64(@_QueryPerformanceFrequency)^);
  if _QueryPerformanceFrequency = 0 then
    raise Exception.Create('QueryPerformanceFrequency=0'); // paranoid
  _QueryPerformanceFrequencyPer10 := _QueryPerformanceFrequency = 10000000;
  IsWow64Process := GetProcAddress(h, 'IsWow64Process');
  res := false;
  IsWow64 := Assigned(IsWow64Process) and
             IsWow64Process(GetCurrentProcess, res) and
             res;
  if IsWow64 then
    // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(h, 'GetNativeSystemInfo')
  else
    @GetNativeSystemInfo := nil;
  // see https://learn.microsoft.com/en-us/windows/arm/apps-on-arm-x86-emulation
  IsWow64Process2 := GetProcAddress(h, 'IsWow64Process2'); // Windows 10+
  process := 0; // = IMAGE_FILE_MACHINE_UNKNOWN if not under WOW64
  machine := 0;
  IsWow64Emulation := Assigned(IsWow64Process2) and
    IsWow64Process2(GetCurrentProcess, process, machine) and
    {$ifdef CPUINTEL}
    (machine <> IMAGE_FILE_MACHINE_I386) and
    (machine <> IMAGE_FILE_MACHINE_AMD64); // detecting PRISM is enough by now
    {$else}
    ((machine = IMAGE_FILE_MACHINE_I386) or
     (machine = IMAGE_FILE_MACHINE_AMD64));
    {$endif CPUINTEL}
  GetSystemTimes := GetProcAddress(h, 'GetSystemTimes');
  GetProcessTimes := GetProcAddress(h, 'GetProcessTimes');
  QueryFullProcessImageNameW := GetProcAddress(h, 'QueryFullProcessImageNameW');
  GetLogicalProcessorInformation := GetProcAddress(h, 'GetLogicalProcessorInformation');
  InitializeSRWLock := GetProcAddress(h, 'InitializeSRWLock');
  AcquireSRWLockExclusive := GetProcAddress(h, 'AcquireSRWLockExclusive');
  ReleaseSRWLockExclusive := GetProcAddress(h, 'ReleaseSRWLockExclusive');
  if not Assigned(InitializeSRWLock) or
     not Assigned(AcquireSRWLockExclusive) or
     not Assigned(ReleaseSRWLockExclusive) then
  begin // SRW was introduced with Vista: on XP, fallback to our TLightLock
    InitializeSRWLock := @InitializeSRWLockForXP;
    AcquireSRWLockExclusive := @AcquireSRWLockExclusiveForXP;
    ReleaseSRWLockExclusive := @ReleaseSRWLockExclusiveForXP;
  end;
  // retrieve system information
  TimeZoneLocalBias := -GetLocalTimeOffset;
  FillcharFast(SystemInfo, SizeOf(SystemInfo), 0);
  if Assigned(GetNativeSystemInfo) then
    GetNativeSystemInfo(SystemInfo)
  else
    Windows.GetSystemInfo(SystemInfo);
  GetMem(P, 10); // ensure that using MIN_PTR_VALUE won't break anything
  if (PtrUInt(P) > MIN_PTR_VALUE) and
     (PtrUInt(SystemInfo.lpMinimumApplicationAddress) <= MIN_PTR_VALUE) then
    PtrUInt(SystemInfo.lpMinimumApplicationAddress) := MIN_PTR_VALUE;
  Freemem(P);
  FillCharFast(mem, SizeOf(mem), 0);
  mem.dwLength := SizeOf(mem);
  if GlobalMemoryStatusEx(mem) then
    SystemMemorySize := mem.ullTotalPhys;
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Vers := wUnknown;
  with OSVersionInfo do
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833
    case dwMajorVersion of
      5:
        case dwMinorVersion of
          0:
            Vers := w2000; // should not happen, but maybe with API translation
          1:
            Vers := wXP;
          2:
            if (wProductType = VER_NT_WORKSTATION) and
               (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
              Vers := wXP_64
            else if GetSystemMetrics(SM_SERVERR2) = 0 then
              Vers := wServer2003
            else
              Vers := wServer2003_R2;
        end;
      6:
        case dwMinorVersion of
          0:
            Vers := wVista;
          1:
            Vers := wSeven;
          2:
            Vers := wEight;
          3:
            Vers := wEightOne;
          4:
            Vers := wTen;
        end;
      10:
        Vers := wTen;
    end;
  if Vers >= wVista then
  begin
    // see https://en.wikipedia.org/wiki/List_of_Microsoft_Windows_versions
    if OSVersionInfo.wProductType <> VER_NT_WORKSTATION then
    begin
      // Server edition
      inc(Vers, 2); // e.g. wEight -> wServer2012
      if Vers = wServer2016 then
        // we identify only LTSC server versions
        // see e.g. https://betawiki.net/wiki/Windows_Server_2025
        if OSVersionInfo.dwBuildNumber >= 177609 then    // up to 17763 RTM
          if OSVersionInfo.dwBuildNumber >= 19504 then   // up to 20348 Preview
            if OSVersionInfo.dwBuildNumber >= 25871 then // up to 26100 Preview
              Vers := wServer2025_64
            else
              Vers := wServer2022_64
          else
            Vers := wServer2019_64;
    end
    else if (Vers = wTen) and
            (OSVersionInfo.dwBuildNumber >= 22000) then
      // Windows 11 has always 22000.###
      Vers := wEleven;
    if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) and
       (Vers < wServer2019_64) then
      inc(Vers);   // e.g. wEight -> wEight64
  end;
  OSVersion := Vers;
  OpenProcessAccess := PROCESS_QUERY_LIMITED_INFORMATION;
  if Vers < wVista then
    OpenProcessAccess := PROCESS_QUERY_INFORMATION or PROCESS_VM_READ;
  OSVersion32.os := osWindows;
  OSVersion32.win := Vers;
  OSVersion32.winbuild := OSVersionInfo.dwBuildNumber;
  h := GetModuleHandle(ntdll);
  if h > 0 then
  begin
    wine_get_version := GetProcAddress(h, 'wine_get_version');
    if Assigned(wine_get_version) then
    begin
      OSVersionInfoEx := wine_get_version;
      OSVersionInfoEx := TrimU('Wine ' + TrimU(OSVersionInfoEx));
    end;
    NtQueryInformationProcess := GetProcAddress(h, 'NtQueryInformationProcess');
    RtlInitUnicodeString      := GetProcAddress(h, 'RtlInitUnicodeString');
    NtOpenSection             := GetProcAddress(h, 'NtOpenSection');
    NtMapViewOfSection        := GetProcAddress(h, 'NtMapViewOfSection');
    NtUnmapViewOfSection      := GetProcAddress(h, 'NtUnmapViewOfSection');
  end;
  // retrieve Software/Hardware information from Registry
  if reg.ReadOpen(wrLocalMachine, 'Software\Microsoft\Windows NT\CurrentVersion') then
  begin
    WindowsUbr := reg.ReadDword('UBR');
    WindowsProductName := reg.ReadString('ProductName');
    WindowsDisplayVersion := reg.ReadString('DisplayVersion');
  end;
  with OSVersionInfo do
  begin
    _fmt('Windows %s (%d.%d.%d)', [WINDOWS_NAME[Vers],
      dwMajorVersion, dwMinorVersion, dwBuildNumber], OSVersionText);
    if wServicePackMajor <> 0 then
      insert(_fmt('SP%d ', [wServicePackMajor]), OSVersionText, PosExChar('(', OSVersionText));
  end;
  if WindowsUbr <> 0 then
    insert(_fmt('.%d', [WindowsUbr]), OSVersionText, length(OSVersionText));
  if WindowsDisplayVersion <> '' then
    insert(WindowsDisplayVersion + ' ', OSVersionText, PosExChar('(', OSVersionText));
  if OSVersionInfoEx <> '' then
    OSVersionText := OSVersionText + ' - ' + OSVersionInfoEx;
  if reg.ReadOpen(wrLocalMachine,
       'Hardware\Description\System\CentralProcessor\0', {closefirst=}true) then
  begin
    cpu := reg.ReadString('ProcessorNameString');
    if cpu = '' then
      cpu := reg.ReadString('Identifier');
  end;
  if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System\BIOS', true) then
  begin
    manuf := reg.ReadString('SystemManufacturer');
    if manuf <> '' then
      manuf := manuf + ' ';
    prod := reg.ReadString('SystemProductName');
    prodver := reg.ReadString('SystemVersion');
    if prodver = '' then
      prodver := reg.ReadString('BIOSVersion');
  end;
  if ({%H-}prod = '') or
     ({%H-}prodver = '') then
  begin
    if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System', true) then
    begin
      if prod = '' then
        prod := reg.ReadString('SystemBiosVersion');
      if prodver = '' then
        prodver := reg.ReadString('VideoBiosVersion');
    end;
  end;
  reg.Close;
  BiosInfoText := manuf{%H-} + prod;
  if prodver <> '' then
    BiosInfoText := BiosInfoText + ' ' + prodver;
  if {%H-}cpu = '' then
    cpu := RawUtf8(GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
  if Assigned(GetLogicalProcessorInformation) then
  begin
    SetLength(proc, 1024);
    siz := SizeOf(proc[0]) * length(proc);
    if GetLogicalProcessorInformation(proc[0], @siz) then
    begin
      for i := 0 to (siz div SizeOf(proc[0])) - 1 do
        with proc[i] do
          case Relationship of
            RelationProcessorPackage: // physical processor socket
              AddPtrUInt(TPtrUIntDynArray(CpuSocketsMask), CpuSockets, ProcessorMask);
            RelationCache:            // raw cache information
              if Cache.CacheType in [CacheUnified, CacheData] then
                if (Cache.Level >= low(CpuCache)) and
                   (Cache.Level <= high(CpuCache)) then
                  with CpuCache[Cache.Level] do
                    if (Count = 0) or
                       (Cache.CacheType <> CacheUnified) then
                    begin
                      inc(Count);
                      Size := Cache.Size;
                      LineSize := Cache.LineSize;
                    end;
          end;
      for i := high(CpuCache) downto low(CpuCache) do
      begin
        CpuCacheSize := CpuCache[i].Size;
        if CpuCacheSize <> 0 then // append the biggest level Cache size
        begin
          cpu := _fmt('%s [%s]', [cpu, _oskb(CpuCacheSize)]);
          break;
        end;
      end;
      for i := low(CpuCache) to high(CpuCache) do
        with CpuCache[i] do
          if Count <> 0 then
            if Count = 1 then
              CpuCacheText :=
                _fmt('%s L%d=%s ', [CpuCacheText, i, _oskb(Size)])
            else
              CpuCacheText :=
                _fmt('%s L%d=%d*%s ', [CpuCacheText, i, Count, _oskb(Size)]);
      TrimSelf(CpuCacheText);
    end;
  end;
  if CpuSockets = 0 then
    CpuSockets := 1; // e.g. on XP prior to SP3
  _fmt('%d x %s (' + CPU_ARCH_TEXT + ')',
    [SystemInfo.dwNumberOfProcessors, cpu], CpuInfoText);
  // writeln(CpuInfoText); writeln(CpuCacheText);
end;

procedure FinalizeSpecificUnit;
begin
  if CryptoApi.Handle <> 0 then
    Windows.FreeLibrary(CryptoApi.Handle);
  if CoInitCounter <> 0 then
    ConsoleWrite('Missing CoUninit (e.g. TOleDBConnection.Destroy call)');
end;


