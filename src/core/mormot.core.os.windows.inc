{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Windows API calls for FPC/Delphi, as used by mormot.core.os.pas
}


{ ****************** Unicode, Time, File process }

// W32() get a PWideChar buffer from a TFileName using a temp static buffer
{$ifdef UNICODE}

type
  TW32Temp = byte; // not used

function W32(const FileName: TFileName; var Temp: TW32Temp): PWideChar; inline;
begin
  Temp := 0; // make compiler happy
  result := pointer(FileName); // no conversion needed
end;

{$else}

const
  /// FPC system hardcoded MAX_PATH = 260 :(
  // - but more than 260 chars are possible with the \\?\..... prefix
  // or by disabling the limitation in registry since Windows 10, version 1607
  // https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
  // - 2047 chars seems big enough in practice - POSIX uses 4096
  W32_MAX = 2047;

type
  /// 2KB stack buffer for no heap allocation during UTF-16 encoding
  TW32Temp = array[0..W32_MAX] of WideChar;

procedure W32Convert(const FileName: TFileName; var Temp: TW32Temp);
var
  U: SynUnicode;
  len: PtrInt;
begin
  // identical to FPC RTL, which converts to UnicodeString before Wide API call 
  U := SynUnicode(FileName); // let the RTL + OS do the conversion
  // switch to extended-length path if needed, allowing up to 32,767 widechars
  len := length(U);
  if len > MAX_PATH then
    U := '\\?\' + U;
  // move to TW32Temp stack buffer
  len := length(U) * 2 + 2;  // +2 to include trailing #0
  if len > SizeOf(Temp) then
    Temp[0] := #0 // avoid buffer overflow (will be rejected by Windows anyway)
  else
    MoveFast(pointer(U)^, Temp, len);
end;

function W32(const FileName: TFileName; var Temp: TW32Temp): PWideChar;
var
  i, len: PtrInt;
begin
  len := length(FileName);
  if len = 0 then
    result := nil
  else
  begin
    if (len < MAX_PATH) and
       IsAnsiCompatible(pointer(FileName), len) then
      // most common case doesn't need any Unicode conversion
      for i := 0 to len do // include trailing #0
        PWordArray(@Temp)[i] := PByteArray(FileName)[i]
    else
      // use a temporary SynUnicode variable for complex UTF-16 conversion
      // or if MAX_PATH is reached and \\?\ prefix is needed for extended length
      W32Convert(FileName, Temp);
    result := @Temp;
  end;
end;

{$endif UNICODE}

function _fmt(const Fmt: string; const Args: array of const): RawUtf8;
begin
  result := RawUtf8(format(Fmt, Args));
end;

// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; Len: integer; out res: RawUtf8);
var
  tmp: TSynTempBuffer;
begin
  if Len <= 0 then
    exit;
  tmp.Init(Len * 3);
  Len := UnicodeToUtf8(tmp.Buf, Len * 3, P, Len); // RTL function is good enough
  if Len > 0 then
    dec(Len); // UnicodeToUtf8() result includes the null terminator
  FastSetString(res, tmp.buf, Len);
  tmp.Done;
end;

const
  DefaultCharVar: AnsiChar = '?';

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
begin
  result := MultiByteToWideChar(CodePage, MB_PRECOMPOSED, A, LA, W, LW);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
begin
  result := WideCharToMultiByte(CodePage, 0, W, LW, A, LA, @DefaultCharVar, nil);
end;

function LibraryOpen(const LibraryName: TFileName): TLibHandle;
var
  tmp: TW32Temp;
  err: DWORD;
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
  external kernel32 name 'GetProcAddress';

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
  UnixFileTimeDelta = 116444736000000000; // from year 1601 to 1970

procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);
begin
  I64 := I64 * 10000000;
  inc(I64, UnixFileTimeDelta);
  FT.dwLowDateTime  := PInt64Rec(@I64)^.Lo; // Delphi 2007 bug with PInt64()
  FT.dwHighDateTime := PInt64Rec(@I64)^.Hi;
end;

procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);
var
  ft, lt: TFileTime;
begin
  UnixTimeToFileTime(I64, ft);
  FileTimeToLocalFileTime(ft, lt);
  FileTimeToSystemTime(lt, Local);
end;

function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div 10000000;
  {$else} // use PInt64 to avoid URW699 with Delphi 6 / Kylix
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div 10000000;
  {$endif CPU64}
end;

function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div 10000;
  {$else} 
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div 10000;
  {$endif CPU64}
end;

function UnixTimeUtc: TUnixTime;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // fast (HW resolution is < TUnixTime second)
  result := FileTimeToUnixTime(ft);
end;

var
  // redirect to a separated slower but more accurate API since Windows 8
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
function  WaitForSingleObject;        external kernel32;
function  GetEnvironmentStringsW;     external kernel32;
function  FreeEnvironmentStringsW;    external kernel32;
function  RtlCaptureStackBackTrace;   external kernel32;
function  IsDebuggerPresent;          external kernel32;
procedure SetEndOfFile;               external kernel32;
procedure FlushFileBuffers;           external kernel32;
function  GetLastError;               external kernel32;
procedure SetLastError;               external kernel32;
function  CreateIoCompletionPort;     external kernel32;
function  GetQueuedCompletionStatus;  external kernel32;
function  PostQueuedCompletionStatus; external kernel32;
function  GetDesktopWindow;           external user32;
function  Unicode_InPlaceUpper;       external user32 name 'CharUpperBuffW';
function  Unicode_InPlaceLower;       external user32 name 'CharLowerBuffW';

function HasConsole: boolean;
begin
  if StdOut = 0 then
  begin
    StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
    if StdOut = INVALID_HANDLE_VALUE then
      StdOut := 0;
  end;
  result := StdOut <> 0;
end;

procedure AllocConsole;
begin
  Windows.AllocConsole;
  if StdOut = 0 then
    HasConsole; // setup StdOut
end;

{$I-}
procedure DisplayFatalError(const title, msg: RawUtf8);
begin
  // better than a MessageBox() especially for services
  AllocConsole; // will create one black window console if none
  if title <> '' then
  begin
    TextColor(ccWhite);
    writeln(#13#10, title);
    writeln(StringOfChar('-', length(title) + 1), #13#10);
    TextColor(ccLightRed);
    writeln(msg);
    TextColor(ccLightGray);
  end
  else
    writeln(msg);
  ioresult;
end;
{$I+}

function GetModuleHandle(lpModuleName: PChar): HMODULE;
begin
  result := Windows.GetModuleHandle(lpModuleName); // call either A or W API
end;

const
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770
  ERROR_WINHTTP_TIMEOUT = 12002;
  ERROR_WINHTTP_CANNOT_CONNECT = 12029;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;

function SysErrorMessage(Code: cardinal; ModuleName: PChar): string;
var
  flags, len: PtrUInt;
  src: pointer;
  tmp: array[0..1023] of Char;
begin
  if ModuleName = nil then
  begin
    flags := FORMAT_MESSAGE_FROM_SYSTEM;
    src := nil;
  end
  else
  begin
    flags := FORMAT_MESSAGE_FROM_HMODULE;
    src := pointer(GetModuleHandle(ModuleName));
  end;
  len := FormatMessage(flags, src, Code, ENGLISH_LANGID, @tmp, 256, nil);
  if (len = 0) and
     (GetLastError = ERROR_RESOURCE_LANG_NOT_FOUND) then
    len := FormatMessage(flags, src, Code, 0, @tmp, SizeOf(tmp), nil);
  if len > 0 then
  begin
    while (len > 0) and
          (ord(tmp[len - 1]) in [0..32, ord('.')]) do
      dec(len);
    SetString(result, PChar(@tmp), len);
  end;
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
    // Read expanded string into result string
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
    Value := QWord((QWord(v) * 1000000) div _QueryPerformanceFrequency);
end;

const
  faInvalidFile = faDirectory + faVolumeID{%H-} + faSysFile{%H-} + faHidden{%H-};
  faDirectoryMask = faDirectory + faHidden{%H-};

function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
begin
  result := WindowsFileTimeToDateTime(FileDate);
end;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  ST, LT: TSystemTime;
  tmp: TW32Temp;
begin
  // much faster than CreateFile, GetFileTime, CloseHandle or FindFirst/Find...
  if GetFileAttributesExW(W32(FileName, tmp), GetFileExInfoStandard, @FA) and
     FileTimeToSystemTime({%H-}FA.ftLastWriteTime, ST) and
     SystemTimeToTzSpecificLocalTime(nil, ST, LT) then
    result := SystemTimeToDateTime(LT)
  else
    result := 0;
end;

function FileAgeToUnixTimeUtc(const FileName: TFileName; AllowDir: boolean): TUnixTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  tmp: TW32Temp;
begin
  if GetFileAttributesExW(W32(FileName, tmp), GetFileExInfoStandard, @FA) and
     (AllowDir or
      (({%H-}FA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0)) then
    result := FileTimeToUnixTime(FA.ftLastWriteTime) // no local time conversion
  else
    result := 0;
end;

function FileAgeToWindowsTime(const FileName: TFileName): integer;
begin
  result := FileAge(FileName); // only called by mormot.core.zip
end;

function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;
begin
  result := FileSetDate(Dest, WinTime) = 0;
end;

function FileInfoByHandle(aFileHandle: THandle; out FileId, FileSize: Int64;
 out LastWriteAccess, FileCreateDateTime: TUnixMSTime): boolean;
var
  lastreadaccess: TUnixMSTime;
  lp: TByHandleFileInformation;
begin
  result := GetFileInformationByHandle(aFileHandle, lp);
  if not result then
    exit;
  LastWriteAccess := FileTimeToUnixMSTime(lp.ftLastWriteTime);
  FileCreateDateTime := FileTimeToUnixMSTime(lp.ftCreationTime);
  lastreadaccess := FileTimeToUnixMSTime(lp.ftLastAccessTime);
  PInt64Rec(@FileSize).lo := lp.nFileSizeLow;
  PInt64Rec(@FileSize).hi := lp.nFileSizeHigh;
  PInt64Rec(@FileId).lo := lp.nFileIndexLow;
  PInt64Rec(@FileId).hi := lp.nFileIndexHigh;
  if LastWriteAccess <> 0 then
    if (FileCreateDateTime = 0) or
       (FileCreateDateTime > LastWriteAccess) then
      FileCreateDateTime := LastWriteAccess;
  if lastreadaccess <> 0 then
    if (FileCreateDateTime = 0) or
       (FileCreateDateTime > lastreadaccess) then
      FileCreateDateTime := lastreadaccess;
end;

function OemToFileName(const OEM: RawByteString): TFileName;
begin
  // decode OEM/DOS file name into native encoding
  SetString(result, nil, length(OEM));
  OemToChar(pointer(OEM), pointer(result)); // OemToCharW/OemToCharA = TFileName
end;

function OemToUnicode(const OEM: RawByteString): SynUnicode;
begin
  SetString(result, nil, length(OEM));
  OemToCharW(pointer(OEM), pointer(result));
end;

function FileSize(const FileName: TFileName): Int64;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  tmp: TW32Temp;
begin
  // much faster than CreateFile, GetFileSize, CloseHandle
  if GetFileAttributesExW(W32(FileName, tmp), GetFileExInfoStandard, @FA) then
  begin
    PInt64Rec(@result)^.Lo := FA.nFileSizeLow;
    PInt64Rec(@result)^.Hi := FA.nFileSizeHigh;
  end
  else
    result := 0;
end;

function FileInfo(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime): boolean;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  tmp: TW32Temp;
begin
  // much faster than CreateFile, GetFileSize, GetFileTime, CloseHandle
  result := GetFileAttributesExW(W32(FileName, tmp), GetFileExInfoStandard, @FA);
  if not result then
    exit;
  PInt64Rec(@FileSize)^.Lo := FA.nFileSizeLow;
  PInt64Rec(@FileSize)^.Hi := FA.nFileSizeHigh;
  FileTimestampUtc := FileTimeToUnixMSTime(FA.ftLastWriteTime) // no local time
end;

function FileSize(F: THandle): Int64;
var
  res: Int64Rec absolute result;
begin
  result := 0;
  if PtrInt(F) > 0 then
    res.Lo := GetFileSize(F, @res.Hi);
end;

function FileSeek64(Handle: THandle; const Offset: Int64; Origin: DWORD): Int64;
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

function FileCreate(const aFileName: TFileName): THandle;
var
  tmp: TW32Temp;
begin
  result := CreateFileW(W32(aFileName, tmp), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

{$ifndef UNICODE} // W32() is not worth it if aFileName is UnicodeString
function FileOpen(const aFileName: TFileName; aMode: integer): THandle;
const
  FILE_WRITE_ATTRIBUTES = $0100; // not defined on oldest Delphi
  ACCESS: array[0..2] of DWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE or FILE_WRITE_ATTRIBUTES);
  SHARE: array[0..4] of DWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  tmp: TW32Temp;
  s: PtrInt;
begin
  s := (aMode and $f0) shr 4;
  if s <= high(SHARE) then
   result := CreateFileW(W32(aFileName, tmp), ACCESS[aMode and 3], SHARE[s],
     nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
  else
    result := THandle(-1);
end;
{$endif UNICODE}

function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;
var
  FileTime: TFileTime;
  D: THandle;
begin
  if Dest = '' then
    result := false
  else
  begin
    D := FileOpen(Dest, fmOpenWrite);
    if ValidHandle(D) then
    begin
      result := GetFileTime(SourceHandle, nil, nil, @FileTime) and
                SetFileTime(D, nil, nil, @FileTime);
      FileClose(D);
    end
    else
      result := false;
  end;
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

function FileOpenSequentialRead(const FileName: TFileName): integer;
var
  tmp: TW32Temp;
  flags: integer;
begin
  if OSVersion >= wVista then // XP has troubles with the "sequential" flag
    flags := FILE_FLAG_SEQUENTIAL_SCAN
  else
    flags := FILE_ATTRIBUTE_NORMAL;
  result := CreateFileW(W32(FileName, tmp), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, // same as fmShareDenyNone
    OPEN_EXISTING, flags, 0)
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
  inc(p^.H, ord(t32 < p^.L)); // wrap-up overflow after 49 days
  p^.L := t32;
  result := p^.V;
end; // warning: FPC's GetTickCount64 doesn't handle 49 days wrap on XP :(

procedure SleepHiRes(ms: cardinal);
begin
  if ms <> 0 then
    Windows.Sleep(ms) // follow the HW timer: typically up to 16ms on Windows
  else
     SwitchToThread;
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

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

{$ifndef NOEXCEPTIONINTERCEPT}

const
  // https://docs.microsoft.com/en-us/archive/blogs/yizhang/interpreting-hresults-returned-from-netclr-0x8013xxxx
  // see also https://referencesource.microsoft.com/#mscorlib/system/__hresults.cs
  DOTNET_EXCEPTIONNAME: array[0..91] of PUtf8Char = (
    'Access',                            // $8013151A
    'AmbiguousMatch',                    // $8000211D
    'appdomainUnloaded',                 // $80131015
    'Application',                       // $80131600
    'Argument',                          // $80070057
    'ArgumentNull',                      // $80004003
    'ArgumentOutOfRange',                // $80131502
    'Arithmetic',                        // $80070216
    'ArrayTypeMismatch',                 // $80131503
    'BadImageFormat',                    // $8007000B
    'CannotUnloadappdomain',             // $80131015
    'ContextMarshal',                    // $80090020
    'Cryptographic',                     // $80004001
    'CryptographicUnexpectedOperation',  // $80131431
    'CustomAttributeFormat',             // $80131537
    'DirectoryNotFound',                 // $80070003
    'DirectoryNotFound',                 // $80030003
    'DivideByZero',                      // $80020012
    'DllNotFound',                       // $80131524
    'DuplicateWaitObject',               // $80131529
    'EndOfStream',                       // $00801338
    'EntryPointNotFound',                // $80131522
    '',                                  // $80131500 - name is plain Exception
    'ExecutionEngine',                   // $80131506
    'External',                          // $80004005
    'FieldAccess',                       // $80131507
    'FileLoad',                          // $80131621
    'FileLoad',                          // $80131018
    'FileNotFound',                      // $80070002
    'Format',                            // $80131537
    'IndexOutOfRange',                   // $80131508
    'InvalidCast',                       // $80004002
    'InvalidComObject',                  // $80131527
    'InvalidFilterCriteria',             // $80131601
    'InvalidOleVariantType',             // $80131531
    'InvalidOperation',                  // $80131509
    'InvalidProgram',                    // $8013153A
    'IO',                                // $80131620
    'IsolatedStorage',                   // $80131450
    'MarshalDirective',                  // $80131535
    'MethodAccess',                      // $80131510
    'MissingField',                      // $80131511
    'MissingManifestResource',           // $80131532
    'MissingMember',                     // $80131512
    'MissingMethod',                     // $80131513
    'MulticastNotSupported',             // $80131514
    'NotFiniteNumber',                   // $80131528
    'NotImplemented',                    // $80004001
    'NotSupported',                      // $80131515
    'NullReference',                     // $80004003
    'OutOfMemory',                       // $8007000E
    'Overflow',                          // $80131516
    'PlatformNotSupported',              // $80131539
    'Policy',                            // $80131416
    'Rank',                              // $80131517
    'ReflectionTypeLoad',                // $80131602
    'Remoting',                          // $8013150B
    'RemotingTimeout',                   // $8013150B
    'SafeArrayTypeMismatch',             // $80131533
    'SafeArrayRankMismatch',             // $80131538
    'Security',                          // $8013150A
    'SEH',                               // $80004005
    'Serialization',                     // $8013150C
    'Server',                            // $8013150E
    'StackOverflow',                     // $800703E9
    'SUDSGenerator',                     // $80131500
    'SUDSParser',                        // $80131500
    'SynchronizationLock',               // $80131518
    'System',                            // $80131501
    'Target',                            // $80131603
    'TargetInvocation',                  // $80131604
    'TargetParameterCount',              // $80138002
    'ThreadAbort',                       // $80131530
    'ThreadInterrupted',                 // $80131519
    'ThreadState',                       // $80131520
    'ThreadStop',                        // $80131521
    'TypeInitialization',                // $80131534
    'TypeLoad',                          // $80131522
    'TypeUnloaded',                      // $80131013
    'UnauthorizedAccess',                // $80070005
    'InClassConstructor',                // $80131543
    'KeyNotFound',                       // $80131577
    'InsufficientStack',                 // $80131578
    'InsufficientMemory',                // $8013153D
    'Verification',                      // $8013150D
    'HostProtection',                    // $80131640
    'MinGrantFailed',                    // $80131417
    'Crypto',                            // $80131430
    'CryptoUnexOper',                    // $80131431
    'Overflow',                          // $8002000a
    'InvalidName',                       // $80131047
    'TypeMismatch');                     // $80028ca0

  DOTNET_EXCEPTIONHRESULT: array[0..91] of cardinal = (
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

function ExceptionInheritsFrom(E: TClass; const Name: ShortString): boolean;
begin
  result := true;
  while (E <> nil) and
        (E <> Exception) do
    if PropNameEquals(PPointer(PtrInt(E) + vmtClassName)^, @Name) then
      exit
    else
      E := GetClassParent(E);
  result := false;
end;

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUtf8CharDynArray): cardinal;
var
  i: PtrInt;
begin
  if ExceptionInheritsFrom(EClass, 'EOleSysError') then
  begin
    result := EOleSysError(EInstance).ErrorCode;
    if result > $80000000 then
      for i := 0 to high(DOTNET_EXCEPTIONHRESULT) do
        // manual loop: the same error code can appear several times
        if DOTNET_EXCEPTIONHRESULT[i] = result then
          PtrArrayAdd(ExceptionNames, DOTNET_EXCEPTIONNAME[i]);
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
  backuplasterror: DWORD;
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
      ctxt.EStack := stack;
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
  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
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
    raise EOSException.Create('TMemoryMap.Map: CreateFileMapping()=0');
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

const
  STUB_SIZE = 65536; // 16*4 KB (4 KB = memory granularity)

type
  TProcessMemoryCounters = record
    cb: DWORD;
    PageFaultCount: DWORD;
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
  OpenProcessAccess: DWORD;

  // late-binding of Windows version specific API entries
  GetSystemTimes: function(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessTimes: function(hProcess: THandle;
    var lpCreationTime, lpExitTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessMemoryInfo: function(Process: THandle;
    var ppsmemCounters: TProcessMemoryCounters; cb: DWORD): BOOL; stdcall;
  // EnumProcessModules: function (hProcess: THandle; var lphModule: HMODULE; cb: DWORD;
  //  var lpcbNeeded: DWORD): BOOL; stdcall;
  EnumProcesses: function(lpidProcess: PDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL; stdcall;
  GetModuleFileNameExW: function(hProcess: THandle; hModule: HMODULE;
    lpBaseName: PWideChar; nSize: DWORD): DWORD; stdcall;
  // Vista+/WS2008+ (use GetModuleFileNameEx on XP)
  QueryFullProcessImageNameW: function(hProcess: THandle; dwFlags: DWORD;
    lpExeName: PWideChar; lpdwSize: PDWORD): BOOL; stdcall;

function GetNextCardinal(var P: PAnsiChar): cardinal;
var
  c: cardinal;
begin
  result := 0;
  repeat
    c := ord(P^) - 48;
    if c > 9 then
      break
    else
      result := result * 10 + c;
    inc(P);
  until false;
  while P^ in ['.', '-', ' '] do
    inc(P);
end;

function GetNextItem(var P: PAnsiChar): RawUtf8;
var
  S: PAnsiChar;
begin
  result := '';
  while P^ <= ' ' do
    if P^ = #0 then
      exit
    else
      inc(P);
  S := P;
  repeat
    inc(P);
  until P^ <= ' ';
  FastSetString(result, S, P - S);
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
    exit;
  if (LastMemInfo.State <> 0) and
     (PtrUInt(p) - PtrUInt(LastMemInfo.BaseAddress) <=
       PtrUInt(LastMemInfo.RegionSize)) then
    result := true // reuse last memory region information if we can
  else
  begin
    // VirtualQuery API is slow but better than raising an exception
    // see https://stackoverflow.com/a/37547837/458259
    FillChar(meminfo, SizeOf(meminfo), 0);
    result := (VirtualQuery(p, meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
              (meminfo.RegionSize >= SizeOf(pointer)) and
              (meminfo.State = MEM_COMMIT) and
              (meminfo.Protect and PAGE_VALID <> 0) and
              (meminfo.Protect and PAGE_GUARD = 0);
    if result then
      LastMemInfo := meminfo;
  end;
end;

function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;
var n: cardinal;
begin
  result := nil;
  if not Assigned(EnumProcesses) then
    exit;
  n := 2048;
  repeat
    SetLength(result, n);
    if EnumProcesses(pointer(result), n * 4, Count) then
      // Count=n if buffer is too small
      Count := Count shr 2
    else
      // error
      Count := 0;
    if Count < n then
    begin
      if Count = 0 then
        result := nil;
      exit;
    end;
    inc(n, 1024); // (very unlikely) too small buffer
  until n > 8192;
end;

function GetErrorText(error: longint): RawUtf8;
var
  Len: integer;
  Buffer: array[0..511] of WideChar;
begin
  Len := FormatMessageW(
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil,
    error, 0, Buffer{%H-}, SizeOf(Buffer), nil);
  while (Len > 0) and
        (ord(Buffer[Len - 1]) in [0..32, ord('.')]) do
    dec(Len);
  Win32PWideCharToUtf8(Buffer, Len, result);
end;

function EnumProcessName(PID: cardinal): RawUtf8;
var
  h: THandle;
  len: DWORD;
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
      else if Assigned(GetModuleFileNameExW) and
              (GetModuleFileNameExW(h, 0, name, high(name)) <> 0) then
        Win32PWideCharToUtf8(name, StrLenW(name), result);
    finally
      CloseHandle(h);
    end;
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

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  mem: TProcessMemoryCounters;
begin
  result := false;
  if (not Assigned(GetProcessTimes)) or
     (not Assigned(GetProcessMemoryInfo)) then
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

function CoCreateGuid(out guid: THash128Rec): HRESULT; stdcall; external 'ole32.dll';

procedure XorOSEntropy(var e: THash512Rec);
var
  h: THash128Rec;
  ft: packed record
    krn, usr, p, e: TFileTime;
  end;
  mem: TProcessMemoryCounters;
begin
  QueryPerformanceCounter(h.Lo); // e.h3 xored with raw timestamps
  e.i[6] := e.i[6] xor h.Lo;
  if Assigned(GetProcessTimes) then
    GetProcessTimes(GetCurrentProcess, ft.p, ft.e, ft.krn, ft.usr);
  DefaultHasher128(@e.h0, @ft, SizeOf(ft));
  mem.cb := SizeOf(mem);
  if Assigned(GetProcessMemoryInfo) then
    GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem));
  DefaultHasher128(@e.h1, @mem, SizeOf(mem));
  if Assigned(GetSystemTimes) then
    GetSystemTimes(ft.usr, ft.p, ft.e);
  DefaultHasher128(@e.h2, @ft, SizeOf(ft));
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
            Assigned(GetProcessMemoryInfo);
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
  fDiffIdle := sidl - fSysPrevIdle;
  fDiffKernel := skrn - fSysPrevKernel;
  fDiffUser := susr - fSysPrevUser;
  fDiffTotal := fDiffKernel + fDiffUser; // kernel time also includes idle time
  dec(fDiffKernel, fDiffIdle);
  fSysPrevIdle := sidl;
  fSysPrevKernel := skrn;
  fSysPrevUser := susr;
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
        Data.User := ((pusr - PrevUser) * 100) / fDiffTotal;
      end;
      PrevKernel := pkrn;
      PrevUser := pusr;
      FillCharFast(mem, SizeOf(mem), 0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h, mem, SizeOf(mem)) then
      begin
        Data.WorkKB := mem.WorkingSetSize shr 10;
        Data.VirtualKB := mem.PagefileUsage shr 10;
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
    Idle := 0;
    Kernel := 0;
    User := 0;
    result := false;
  end
  else
  begin
    Kernel := {%H-}SimpleRoundTo2Digits((fDiffKernel * 100) / fDiffTotal);
    User := {%H-}SimpleRoundTo2Digits((fDiffUser * 100) / fDiffTotal);
    Idle := 100 - Kernel - User; // ensure sum is always 100%
    result := true;
  end;
end;

{$ifndef UNICODE} // missing API for FPC and oldest Delphi

type
  DWORDLONG = QWord;

  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;

// information about the system's current usage of both physical and virtual memory
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
  stdcall; external kernel32;

{$endif UNICODE}

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
  info.percent := global.dwMemoryLoad;
  info.memtotal := global.ullTotalPhys;
  info.memfree := global.ullAvailPhys;
  info.filetotal := global.ullTotalPageFile;
  info.filefree := global.ullAvailPageFile;
  info.vmtotal := global.ullTotalVirtual;
  info.vmfree := global.ullAvailVirtual;
  info.allocreserved := 0;
  info.allocused := 0;
  {$ifdef WITH_FASTMM4STATS} // override OS information by actual FastMM4
  if withalloc then
  begin
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
  end;
  {$else}
  if withalloc and
     Assigned(GetProcessMemoryInfo) then
  begin
    FillcharFast(mem, SizeOf(mem), 0);
    mem.cb := SizeOf(mem);
    GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem));
    info.allocreserved := mem.PeakWorkingSetSize;
    info.allocused := mem.WorkingSetSize;
  end;
  {$endif WITH_FASTMM4STATS}
end;

function GetDiskFreeSpaceExW(lpDirectoryName: PWideChar;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes,
  lpTotalNumberOfFreeBytes: QWord): LongBool; stdcall; external kernel32;

{
// DeviceIoControl(IOCTL_DISK_GET_PARTITION_INFO) requires root -> not used
function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWORD;
  lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer;
  nOutBufferSize: DWORD; var lpBytesReturned: DWORD;
  lpOverlapped: POverlapped): BOOL; stdcall; external kernel32;
}

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord;
  aVolumeName: PSynUnicode): boolean;
var
  tmp: array[0..MAX_PATH - 1] of WideChar;
  dummy, flags: DWORD;
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
begin
  result := nil;
  n := 0;
  fn := '#:';
  drives := GetLogicalDrives;
  m := 1 shl 2;
  for drive := 3 to 26 do
  begin
    // retrieve partitions mounted as C..Z drives
    if drives and m <> 0 then
    begin
      fn[1] := char(64 + drive);
      if GetDiskInfo(fn, av, fr, tot, @volume) then
      begin
        SetLength(result, n + 1);
        Win32PWideCharToUtf8(pointer(volume), length(volume), result[n].name);
        volume := '';
        result[n].mounted := fn;
        result[n].size := tot;
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

function ConsoleKeyPressed(ExpectedKey: Word): boolean;
var
  lpNumberOfEvents: DWORD;
  lpBuffer: TInputRecord;
  lpNumberOfEventsRead: DWORD;
  nStdHandle: THandle;
begin
  result := false;
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(nStdHandle, lpNumberOfEvents);
  if lpNumberOfEvents <> 0 then
  begin
    PeekConsoleInput(nStdHandle, lpBuffer, 1, lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
      if lpBuffer.EventType = KEY_EVENT then
        if lpBuffer.Event.KeyEvent.bKeyDown and
           ((ExpectedKey = 0) or
            (lpBuffer.Event.KeyEvent.wVirtualKeyCode = ExpectedKey)) then
          result := true
        else
          FlushConsoleInputBuffer(nStdHandle)
      else
        FlushConsoleInputBuffer(nStdHandle);
  end;
end;

{$I-}
procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  if GetCurrentThreadID = MainThreadID then
    while not ConsoleKeyPressed(VK_RETURN) do
    begin
      if IsMultiThread then
        CheckSynchronize{$ifndef DELPHI6OROLDER}(100){$endif}
      else
        Sleep(100);
      while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
        if msg.Message = WM_QUIT then
          exit
        else
        begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;
    end
  else
    ReadLn;
  ioresult;
end;
{$I+}

{$ifndef FPC}
var
  // Delphi doesn't define this global variable need by ConsoleReadBody
  StdInputHandle: THandle;
{$endif FPC}

function ConsoleStdInputLen: integer;
begin
  {$ifndef FPC}
  if StdInputHandle = 0 then
    StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  {$endif FPC}
  if not PeekNamedPipe(StdInputHandle, nil, 0, nil, @result, nil) then
    result := 0;
end;

procedure Utf8ToConsoleDoConv(const Utf8: RawUtf8; var Console: RawByteString);
var
  utf16: SynUnicode;
  tmp: TSynTempBuffer;
begin
  utf16 := Utf8Decode(Utf8);
  tmp.Init(length(utf16) * 3);
  CharToOemBuffW(pointer(utf16), tmp.buf, length(utf16) + 1); // +1 = ending #0
  FastSetStringCP(Console, tmp.buf, StrLen(tmp.buf), CP_OEMCP);
  tmp.Done;
end;

function Utf8ToConsole(const S: RawUtf8): RawByteString;
begin
  if IsAnsiCompatible(S) then
    result := S // no conversion needed
  else
    Utf8ToConsoleDoConv(S, result);
end;

procedure ConsoleToUtf8DoConv(const Console: RawByteString; var Utf8: RawUtf8);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(length(Console) * 4);
  OemToCharBuffW(pointer(Console), tmp.buf, length(Console) + 1); // +1 = #0
  Win32PWideCharToUtf8(tmp.buf, StrLenW(tmp.Buf), Utf8);
  tmp.Done;
end;

function ConsoleToUtf8(const S: RawByteString): RawUtf8;
begin
  if IsAnsiCompatible(S) then
    result := S // no conversion needed
  else
    ConsoleToUtf8DoConv(S, result);
end;


constructor TFileVersion.Create(const aFileName: TFileName;
  aMajor, aMinor, aRelease, aBuild: integer);
var
  M, D: word;
  Size, Size2: DWord;
  Pt: Pointer;
  Trans: PWordArray;
  LanguageInfo: RawUtf8;
  Info: ^TVSFixedFileInfo;
  FileTime: TFILETIME;
  SystemTime: TSYSTEMTIME;
  tmp: SynUnicode;

  function ReadResourceByName(const From: RawUtf8): RawUtf8;
  var
    StrValPt: pointer;
    sz: DWord;
    u: SynUnicode;
  begin
    u := Utf8Decode('\StringFileInfo\' + LanguageInfo + '\' + From);
    VerQueryValueW(Pt, pointer(u), StrValPt, sz);
    if sz > 0 then
      Win32PWideCharToUtf8(StrValPt, StrLenW(StrValPt), result)
    else
      result := '';
  end;

begin
  fFileName := aFileName;
  if aFileName <> '' then
  begin
    // GetFileVersionInfo modifies the filename parameter data while parsing
    // Copy the string const into a local variable to create a writeable copy
    tmp := SynUnicode(aFileName); // use RTL for UTF-16 conversion
    {$ifdef UNICODE}
    UniqueString(tmp);
    {$endif UNICODE}
    Size := GetFileVersionInfoSizeW(pointer(tmp), Size2);
    if Size > 0 then
    begin
      GetMem(Pt, Size);
      try
        tmp := SynUnicode(aFileName);
        {$ifdef UNICODE}
        UniqueString(tmp);
        {$endif UNICODE}
        GetFileVersionInfoW(pointer(tmp), 0, Size, Pt);
        VerQueryValueW(Pt, '\', pointer(Info), Size2);
        with Info^ do
        begin
          if Version32 = 0 then
          begin
            aMajor := dwFileVersionMS shr 16;
            aMinor := word(dwFileVersionMS);
            aRelease := dwFileVersionLS shr 16;
          end;
          aBuild := word(dwFileVersionLS);
          if (dwFileDateLS <> 0) and
             (dwFileDateMS <> 0) then
          begin
            FileTime.dwLowDateTime := dwFileDateLS; // built date from version info
            FileTime.dwHighDateTime := dwFileDateMS;
            FileTimeToSystemTime(FileTime, SystemTime);
            fBuildDateTime := EncodeDate(
              SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay);
          end;
        end;
        VerQueryValue(Pt, '\VarFileInfo\Translation', pointer(Trans), Size2);
        if Size2 >= 4 then
        begin
          LanguageInfo := _fmt('%4.4x%4.4x', [Trans^[0], Trans^[1]]);
          CompanyName := ReadResourceByName('CompanyName');
          FileDescription := ReadResourceByName('FileDescription');
          FileVersion := ReadResourceByName('FileVersion');
          InternalName := ReadResourceByName('InternalName');
          LegalCopyright := ReadResourceByName('LegalCopyright');
          OriginalFilename := ReadResourceByName('OriginalFilename');
          ProductName := ReadResourceByName('ProductName');
          ProductVersion := ReadResourceByName('ProductVersion');
          Comments := ReadResourceByName('Comments');
        end
      finally
        Freemem(Pt);
      end;
    end;
  end;
  SetVersion(aMajor, aMinor, aRelease, aBuild);
  if fBuildDateTime = 0 then  // get build date from file age
    fBuildDateTime := FileAgeToDateTime(aFileName);
  if fBuildDateTime <> 0 then
    DecodeDate(fBuildDateTime, BuildYear, M, D);
end;

procedure GetUserHost(out User, Host: RawUtf8);
var
  tmp: array[byte] of WideChar;
  tmpsize: cardinal;
begin
  tmpsize := SizeOf(tmp);
  GetComputerNameW(tmp{%H-}, tmpsize);
  Win32PWideCharToUtf8(@tmp, StrLenW(tmp), Host);
  tmpsize := SizeOf(tmp);
  GetUserNameW(tmp, tmpsize);
  Win32PWideCharToUtf8(@tmp, StrLenW(tmp), User);
end;

var
  // avoid unneeded reference to ShlObj.pas
  // - late binding is mandatory to be used on WinPE which does NOT have this dll
  SHGetFolderPath: function(hwnd: hwnd; csidl: integer; hToken: THandle;
    dwFlags: DWord; pszPath: PChar): HRESULT; stdcall;
  _SystemPath: array[TSystemPath] of TFileName; // cache

function ComputeSystemPath(kind: TSystemPath): TFileName;
const
  CSIDL_PERSONAL = $0005;
  CSIDL_LOCAL_APPDATA = $001C; // local non roaming user folder
  CSIDL_COMMON_APPDATA = $0023;
  CSIDL_COMMON_DOCUMENTS = $002E;
  csidl: array[TSystemPath] of integer = (
    // spCommonData,       spUserData,          spCommonDocuments
    CSIDL_COMMON_APPDATA, CSIDL_LOCAL_APPDATA, CSIDL_COMMON_DOCUMENTS,
    // spUserDocuments, spTempFolder, spLog
    CSIDL_PERSONAL,     0,      CSIDL_LOCAL_APPDATA);
  ENV: array[TSystemPath] of TFileName = (
    'ALLUSERSAPPDATA', 'LOCALAPPDATA', '', '', 'TEMP', 'LOCALAPPDATA');
var
  tmp: array[0..MAX_PATH] of char;
begin
  if (kind = spLog) and
     IsDirectoryWritable(Executable.ProgramFilePath) then
    result := EnsureDirectoryExists(Executable.ProgramFilePath + 'log')
  else if (csidl[kind] <> 0) and
          Assigned(SHGetFolderPath) and
          (SHGetFolderPath(0, csidl[kind], 0, 0, @tmp) = S_OK) then
    result := IncludeTrailingPathDelimiter(tmp)
  else
  begin
    result := GetEnvironmentVariable(ENV[kind]);
    if result = '' then
    begin
      result := GetEnvironmentVariable('APPDATA');
      if result = '' then
      begin
        result := GetEnvironmentVariable('TEMP');
        if result = '' then
          result := Executable.ProgramFilePath;
      end;
    end;
    result := IncludeTrailingPathDelimiter(result);
  end;
  _SystemPath[kind] := result;
end;

function GetSystemPath(kind: TSystemPath): TFileName;
begin
  result := _SystemPath[kind];
  if result = '' then
    result := ComputeSystemPath(kind);
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  RestoreProtection, Ignore: DWORD;
  i: PtrInt;
begin
  if VirtualProtect(Old, Size, PAGE_EXECUTE_READWRITE, RestoreProtection) then
  begin
    if Backup <> nil then
      for i := 0 to Size - 1 do  // do not use Move() here
        PByteArray(Backup)^[i] := PByteArray(Old)^[i];
    for i := 0 to Size - 1 do    // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    if not LeaveUnprotected then
      VirtualProtect(Old, Size, RestoreProtection, Ignore);
    FlushInstructionCache(GetCurrentProcess, Old, Size);
    if not CompareMemFixed(Old, New, Size) then
      raise Exception.Create('PatchCode?');
  end;
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
  L: integer;
begin
  if closefirst then
    Close;
  L := length(keyname);
  tmp.Init(L * 2);
  Utf8ToUnicode(tmp.buf, L + 8, pointer(keyname), L); // uses RTL
  key := 0;
  result := RegOpenKeyExW(_HKEY[root], tmp.buf, 0, KEY_READ, key) = 0;
  tmp.Done;
end;

procedure TWinRegistry.Close;
begin
  if key <> 0 then
    RegCloseKey(key);
end;

function TWinRegistry.ReadString(const entry: SynUnicode; andtrim: boolean): RawUtf8;
var
  rtype, rsize: DWORD;
  tmp: TSynTempBuffer;
begin
  result := '';
  if RegQueryValueExW(key, pointer(entry), nil, @rtype, nil, @rsize) <> 0 then
    exit;
  tmp.Init(rsize);
  if RegQueryValueExW(key, pointer(entry), nil, nil, tmp.buf, @rsize) = 0 then
  begin
    case rtype of
      REG_SZ,
      REG_EXPAND_SZ,
      REG_MULTI_SZ:
        Win32PWideCharToUtf8(tmp.buf, StrLenW(tmp.buf), result);
    end;
    if andtrim then
      TrimSelf(result);
  end;
  tmp.Done;
end;

function TWinRegistry.ReadData(const entry: SynUnicode): RawByteString;
var
  rtype, rsize: DWORD;
begin
  result := '';
  if RegQueryValueExW(key, pointer(entry), nil, @rtype, nil, @rsize) <> 0 then
    exit;
  SetLength(result, rsize);
  if RegQueryValueExW(key, pointer(entry), nil, nil, pointer(result), @rsize) <> 0 then
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

function TWinRegistry.ReadBuffer(const entry: SynUnicode; Data: pointer; DataLen: DWORD): boolean;
begin
  result := RegQueryValueExW(key, pointer(entry), nil, nil, Data, @DataLen) = 0;
end;

function TWinRegistry.ReadEnumEntries: TRawUtf8DynArray;
var
  count, maxlen, i, len: DWORD;
  tmp: TSynTempBuffer;
begin
  result := nil;
  count := 0;
  if (RegQueryInfoKeyW(key, nil, nil, nil, @count, @maxlen,
       nil, nil, nil, nil, nil, nil) <> 0) or
     (count = 0) then
    exit;
  SetLength(result, count);
  inc(maxlen);
  tmp.Init(maxlen * 2);
  for i := 0 to count - 1 do
  begin
    len := maxlen;
    if RegEnumKeyExW(key, i, tmp.buf, len, nil, nil, nil, nil) = 0 then
      Win32PWideCharToUtf8(tmp.buf, len, result[i]);
  end;
  tmp.Done;
end;


const
  MAX_SE_NAME_LENGTH = 31;
  WINSYSTEMPRIVILEGENAME: array[TWinSystemPrivilege] of string = (
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

type
  TOKEN_PRIVILEGES = packed record
    PrivilegeCount : DWORD;
    Privileges : array[0..0] of LUID_AND_ATTRIBUTES;
  end;
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;

function OpenProcessToken(ProcessHandle: THandle; DesiredAccess: DWORD;
  var TokenHandle: THandle): BOOL;
    stdcall; external advapi32 name 'OpenProcessToken';

function LookupPrivilegeValue(lpSystemName, lpName: PChar;
  var lpLuid: TLargeInteger): BOOL;
    stdcall; external advapi32 name 'LookupPrivilegeValue' + _AW;

function LookupPrivilegeName(lpSystemName: LPCSTR; var lpLuid: TLargeInteger;
  lpName: PChar; var cbName: DWORD): BOOL;
    stdcall; external advapi32 name 'LookupPrivilegeName' + _AW;

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TOKEN_PRIVILEGES; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL;
    stdcall; external advapi32 name 'AdjustTokenPrivileges';


{ TSynWindowsPrivileges }

procedure TSynWindowsPrivileges.Init(aTokenPrivilege: TPrivilegeTokenType);
var
  access: cardinal;
begin
  Token := 0;
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  access := TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES;
  if aTokenPrivilege = pttProcess then
  begin
    if not OpenProcessToken(GetCurrentProcess, access, Token) then
      raise EOSException.Create(
        'TSynWindowsPrivileges cannot open process token');
  end
  else if not OpenThreadToken(GetCurrentThread, access, false, Token) then
    if GetLastError = ERROR_NO_TOKEN then
    begin
      if not ImpersonateSelf(SecurityImpersonation) or
         not OpenThreadToken(GetCurrentThread, access, false, Token) then
        raise EOSException.Create(
          'TSynWindowsPrivileges cannot open thread token');
    end
    else
      raise EOSException.Create(
        'TSynWindowsPrivileges cannot open thread token');
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
    for p := low(p) to high(p) do
      if p in new then
        Disable(p);
  end;
  CloseHandle(Token);
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
  tp: PTOKEN_PRIVILEGES;
  len: cardinal;
  i: PtrInt;
  name: string;
  p: TWinSystemPrivilege;
begin
  if Token = 0 then
    raise EOSException.Create(
      'TSynWindowsPrivileges: invalid privileges token');
  buf.Init;
  try
    len := 0;
    if not GetTokenInformation(
             Token, TokenPrivileges, buf.buf, buf.len, len) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise EOSException.CreateFmt(
          'TSynWindowsPrivileges cannot get token information (error=%d)',
          [GetLastError])
      else
      begin
        buf.Done;
        buf.Init(len); // we need a bigger buffer
        if not GetTokenInformation(
                 Token, TokenPrivileges, buf.buf, buf.len, len) then
          raise EOSException.CreateFmt(
            'TSynWindowsPrivileges cannot retrieve token information (error=%d)',
            [GetLastError])
      end;
    tp := buf.buf;
    SetLength(name, MAX_SE_NAME_LENGTH);
    for i := 0 to tp.PrivilegeCount - 1 do
    begin
      len := length(name);
      if not LookupPrivilegeName(
               nil, tp.Privileges[i].Luid, pointer(name), len) then
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          raise EOSException.CreateFmt(
            'TSynWindowsPrivileges LookupPrivilegeName=%d for Luid %d',
            [GetLastError, PInt64(@tp.Privileges[i].Luid)^])
        else
        begin
          SetLength(name, len);
          if not LookupPrivilegeName(
                   nil, tp.Privileges[i].Luid, pointer(name), len) then
            raise EOSException.CreateFmt(
              'TSynWindowsPrivileges LookupPrivilegeName=%d for Luid=%d',
              [GetLastError, PInt64(@tp.Privileges[i].Luid)^])
        end;
      for p := low(p) to high(p) do
        if CompareText(name, WINSYSTEMPRIVILEGENAME[p]) = 0 then
        begin
          Include(fAvailable, p);
          if tp.Privileges[i].Attributes and SE_PRIVILEGE_ENABLED <> 0 then
            Include(fDefEnabled, p);
        end;
    end;
    fEnabled := fDefEnabled;
  finally
    buf.Done;
  end;
end;

function TSynWindowsPrivileges.SetPrivilege(aPrivilege: TWinSystemPrivilege;
  aEnablePrivilege: boolean): boolean;
var
  tp: TOKEN_PRIVILEGES;
  id: TLargeInteger;
  tpprev: TOKEN_PRIVILEGES;
  cbprev: DWORD;
begin
  result := false;
  cbprev := SizeOf(TOKEN_PRIVILEGES);
  if not LookupPrivilegeValue(
           nil, Pointer(WINSYSTEMPRIVILEGENAME[aPrivilege]), id) then
    exit;
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid := PInt64(@id)^;
  tp.Privileges[0].Attributes := 0;
  AdjustTokenPrivileges(
    Token, false, tp, SizeOf(TOKEN_PRIVILEGES), @tpprev, @cbprev);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  tpprev.PrivilegeCount := 1;
  tpprev.Privileges[0].Luid := PInt64(@id)^;
  with tpprev.Privileges[0] do
    if aEnablePrivilege then
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

  PUNICODE_STRING = ^UNICODE_STRING;
  UNICODE_STRING = packed record
    Length: word;
    MaximumLength: word;
    {$ifdef CPUX64}
    _align: array[0..3] of byte;
    {$endif CPUX64}
    Buffer: PWideChar;
  end;

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
    SecurityDescriptor: Pointer;       // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
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
    var SectionOffset: TLargeInteger; ViewSize: PULONG; InheritDisposition: DWORD;
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
    if NtMapViewOfSection(mem, THandle(-1), @virt, 0, size, add, @size,
         1, 0, PAGE_READONLY) = 0 then
    begin
      FastSetRawByteString(result, virt, size);
      NtUnmapViewOfSection(THandle(-1), virt);
    end;
    CloseHandle(mem);
  end;
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: PtrUInt; var lpNumberOfBytesRead: PtrUInt): BOOL;
    stdcall; external kernel32 name 'ReadProcessMemory';

function InternalGetProcessInfo(aPID: DWORD; out aInfo: TWinProcessInfo): boolean;
var
  bytesread: PtrUInt;
  sizeneeded: DWORD;
  pbi: PMS_PROCESS_BASIC_INFORMATION;
  peb: MS_PEB;
  peb_upp: MS_RTL_USER_PROCESS_PARAMETERS;
  prochandle: THandle;
  buf: TSynTempBuffer;
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
  buf.InitZero(0); // 0 to initialize the whole buf.tmp[] stack buffer to zero
  try
    sizeneeded := 0;
    if NtQueryInformationProcess(prochandle,
         ProcessBasicInformation, buf.buf, buf.len, @sizeneeded) < 0 then
      exit;
    if buf.len < integer(sizeneeded) then
    begin
      buf.Done;
      buf.InitZero(sizeneeded);
      if NtQueryInformationProcess(prochandle,
           ProcessBasicInformation, buf.buf, buf.len, @sizeneeded) < 0 then
        exit;
    end;
    Include(aInfo.AvailableInfo, wpaiBasic);
    pbi := buf.buf;
    with aInfo do
    begin
      PID := pbi^.UniqueProcessId;
      ParentPID := pbi^.InheritedFromUniqueProcessId;
      BasePriority := pbi^.BasePriority;
      ExitStatus := pbi^.ExitStatus;
      PEBBaseAddress := pbi^.PebBaseAddress;
      AffinityMask := pbi^.AffinityMask;
    end;
    // read PEB (Process Environment Block)
    if not Assigned(pbi.PebBaseAddress) then
      exit;
    bytesread := 0;
    FillCharFast(peb, SizeOf(MS_PEB), 0);
    if not ReadProcessMemory(prochandle, pbi.PebBaseAddress,
             @peb, SizeOf(MS_PEB), bytesread) then
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
    buf.Done;
  end;
end;

procedure GetProcessInfo(aPid: cardinal;
  out aInfo: TWinProcessInfo);
var
  privileges: TSynWindowsPrivileges;
begin
  privileges.Init(pttThread);
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
  privileges.Init(pttThread);
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
  siz, typ: DWORD;
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
  NAMES: array[0..7] of PAnsiChar = (
    'CryptAcquireContextA',
    'CryptReleaseContext',
    'CryptImportKey',
    'CryptSetKeyParam',
    'CryptDestroyKey',
    'CryptEncrypt',
    'CryptDecrypt',
    'CryptGenRandom');
var
  P: PPointer;
  i: PtrInt;
begin
  Tested := true;
  Handle := GetModuleHandle('advapi32.dll');
  if Handle <> 0 then
  begin
    P := @@AcquireContextA;
    for i := 0 to high(NAMES) do
    begin
      P^ := GetProcAddress(Handle, NAMES[i]);
      if P^ = nil then
      begin
        PPointer(@@AcquireContextA)^ := nil;
        break;
      end;
      inc(P);
    end;
  end;
  // note: CryptSignMessage and CryptVerifyMessageSignature are in crypt32.dll
end;

type
  {$ifdef FPC}
  {$packrecords C} // mandatory under Win64
  {$endif FPC}
  DATA_BLOB = record
    cbData: DWORD;
    pbData: PAnsiChar;
  end;
  PDATA_BLOB = ^DATA_BLOB;
  {$ifdef FPC}
  {$packrecords DEFAULT}
  {$endif FPC}

const
  CRYPTDLL = 'Crypt32.dll';
  CRYPTPROTECT_UI_FORBIDDEN = 1;
  CRYPT_STRING_BASE64HEADER = 0; // = PEM textual format

function CryptProtectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external CRYPTDLL name 'CryptProtectData';

function CryptUnprotectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external CRYPTDLL name 'CryptUnprotectData';

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
    stdcall; external CRYPTDLL name 'CertOpenSystemStoreW';

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT;
  stdcall; external CRYPTDLL name 'CertEnumCertificatesInStore';

function CryptBinaryToString(pBinary: PByte; cbBinary, dwFlags: DWORD;
  pszString: PAnsiChar; var pchString: DWORD): BOOL;
    stdcall; external CRYPTDLL name 'CryptBinaryToStringA';

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL;
    stdcall; external CRYPTDLL name 'CertCloseStore';

function _GetSystemStoreAsPem(CertStore: TSystemCertificateStore): RawUtf8;
var
  store: HCERTSTORE;
  ctx: PCCERT_CONTEXT;
  resultlen, certlen: DWORD;
  tmp: TSynTempBuffer;
begin
  // call the Windows API to retrieve the System certificates
  resultlen := 0;
  store := CertOpenSystemStoreW(nil, WINDOWS_CERTSTORE[CertStore]);
  try
    ctx := CertEnumCertificatesInStore(store, nil);
    while ctx <> nil do
    begin
      certlen := 0;
      if not CryptBinaryToString(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, nil, certlen) then
        break;
      tmp.Init(certlen); // a PEM is very likely to be < 8KB so will be on stack
      if CryptBinaryToString(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, tmp.buf, certlen) then
      begin
        SetLength(result, resultlen + certlen);
        MoveFast(tmp.buf^, PByteArray(result)[resultlen], certlen);
        inc(resultlen, certlen);
      end;
      tmp.Done;
      ctx := CertEnumCertificatesInStore(store, ctx); // next certificate
    end;
  finally
    CertCloseStore(store, 0);
  end;
end;

function GetRawSmbiosFromMem(var info: TRawSmbiosInfo): boolean; forward;

const
  _RSMB_ = $52534D42;

function _GetRawSmbios(out info: TRawSmbiosInfo): boolean;
var
  h: HMODULE;
  siz: DWORD;
  tmp: RawByteString;
  get: function(sig, id: DWORD; buf: pointer; siz: DWORD): PtrUInt; stdcall;
begin
  // first try to use Vista+ API which supports EFI
  h := GetModuleHandle(kernel32);
  get := GetProcAddress(h, 'GetSystemFirmwareTable');
  if Assigned(get) then
  begin
    siz := get(_RSMB_, 0, nil, 0); // first call to retrieve the full size
    if siz > SizeOf(info) then
    begin
      FastSetRawByteString(tmp, nil, siz);
      get(_RSMB_, 0, pointer(tmp), siz);
      PInt64(@info)^ := PInt64(tmp)^; // header fields = 64-bit
      FastSetRawByteString(info.data, @PInt64Array(tmp)[1], siz - SizeOf(Int64));
      result := true;
      exit;
    end;
  end;
  // on XP, try to read directly from physical memory via ntdl.dll low-level API
  result := GetRawSmbiosFromMem(info);
end;

function GetSmbEfiMem: RawByteString;
begin
  result := '';
end;

procedure DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
  // not needed - GetRawSmbios() is likely to work with no administrator rights
end;

threadvar
  OleDBCoinitialized: integer;

// avoid including ActiveX unit
function CoInitialize(_para1: pointer): HRESULT; stdcall; external 'ole32.dll';
procedure CoUninitialize; stdcall; external 'ole32.dll';

procedure CoInit;
begin
  inc(OleDBCoInitialized); // is a threadvar: no InterlockedIncrrement() needed
  if OleDBCoInitialized = 1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  if OleDBCoinitialized <= 0 then
    raise EOleSysError.Create('You should call TOleDBConnection.Free from the same ' +
      'thread which called its Create: i.e. call MyProps.EndCurrentThread from an ' +
      'THttpServerGeneric.OnHttpThreadTerminate event - see ticket 213544b2f5');
  dec(OleDBCoinitialized);
  if OleDBCoinitialized = 0 then
    CoUninitialize;
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
    'Error');

function ToText(st: TServiceState): PShortString; overload;
begin
  result := @_SERVICESTATE[st];
end;


{ TServiceController }

constructor TServiceController.CreateNewService(
  const TargetComputer, DatabaseName, Name, DisplayName, Path,
        OrderGroup, Dependencies, Username, Password: string;
  DesiredAccess, ServiceType, StartType, ErrorControl: cardinal);
var
  exeName: TFileName;
  backupError: cardinal;
begin
  inherited Create;
  if Path = '' then
  begin
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllError,
        'CreateNewService("%","%") with Path=""', [Name, DisplayName], self);
    exit;
  end;
  if TargetComputer = '' then
    if GetDriveType(pointer(ExtractFileDrive(Path))) = DRIVE_REMOTE then
    begin
      exeName := ExpandUNCFileName(Path);
      if (copy(exeName, 1, 12) <> '\\localhost\') or
         (exeName[14] <> '$') then
      begin
        if Assigned(WindowsServiceLog) then
          WindowsServiceLog(sllError,
            'CreateNewService("%","%") on remote drive: Path="%" is %',
            [Name, DisplayName, Path, exeName], self);
        exit;
      end;
      system.delete(exeName, 1, 12); // \\localhost\c$\... -> c:\...
      exeName[2] := ':';
    end
    else
      exeName := Path;
  fName := RawUtf8(Name);
  fSCHandle := OpenSCManager(
    pointer(TargetComputer), pointer(DatabaseName), SC_MANAGER_ALL_ACCESS);
  if fSCHandle = 0 then
  begin
    backupError := GetLastError;
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, 'OpenSCManager(''%'',''%'') for [%]',
        [TargetComputer, DatabaseName, fName], self);
    SetLastError(backupError);
    exit;
  end;
  fHandle := CreateService(fSCHandle,
    pointer(Name), pointer(DisplayName), DesiredAccess, ServiceType, StartType,
    ErrorControl, pointer({%H-}exeName), pointer(OrderGroup), nil,
    pointer(Dependencies), pointer(Username), pointer(Password));
  if fHandle = 0 then
  begin
    backupError := GetLastError;
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError,
        'CreateService("%","%","%")', [Name, DisplayName, Path], self);
    SetLastError(backupError);
  end;
end;

constructor TServiceController.CreateOpenService(
  const TargetComputer, DataBaseName, Name: string; DesiredAccess: cardinal);
var
  backupError: cardinal;
begin
  inherited Create;
  fName := RawUtf8(Name);
  fSCHandle := OpenSCManager(
    pointer(TargetComputer), pointer(DataBaseName), GENERIC_READ);
  if fSCHandle = 0 then
  begin
    backupError := GetLastError;
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, 'OpenSCManager(''%'',''%'') for [%]',
        [TargetComputer, DataBaseName, fName], self);
    SetLastError(backupError);
    exit;
  end;
  fHandle := OpenService(fSCHandle, pointer(Name), DesiredAccess);
  if fHandle = 0 then
  begin
    backupError := GetLastError;
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, 'OpenService("%")', [Name], self);
    SetLastError(backupError);
  end;
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
    else if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, 'DeleteService("%")', [fName], self);
end;

destructor TServiceController.Destroy;
begin
  if fHandle <> 0 then
    CloseServiceHandle(fHandle);
  if fSCHandle <> 0 then
    CloseServiceHandle(fSCHandle);
  inherited;
end;

function TServiceController.GetState: TServiceState;
begin
  if (self = nil) or
     (fSCHandle = 0) or
     (fHandle = 0) then
    result := ssNotInstalled
  else
    result := CurrentStateToServiceState(Status.dwCurrentState);
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(sllTrace, 'GetState(%)=%', [fName, _SERVICESTATE[result]], self);
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  FillCharFast(fStatus, SizeOf(fStatus), 0);
  QueryServiceStatus(fHandle, fStatus);
  result := fStatus;
end;

function TServiceController.Pause: boolean;
begin
  result := ControlService(fHandle, SERVICE_CONTROL_PAUSE, fStatus);
end;

function TServiceController.Refresh: boolean;
begin
  result := ControlService(fHandle, SERVICE_CONTROL_INTERROGATE, fStatus);
end;

function TServiceController.Resume: boolean;
begin
  result := ControlService(fHandle, SERVICE_CONTROL_CONTINUE, fStatus);
end;

function TServiceController.Shutdown: boolean;
begin
  result := ControlService(fHandle, SERVICE_CONTROL_SHUTDOWN, fStatus);
end;

function TServiceController.Start(const Args: array of PChar): boolean;
begin
  if length(Args) = 0 then
    result := StartService(fHandle, 0, nil)
  else
    result := StartService(fHandle, length(Args), @Args[0]);
end;

function TServiceController.Stop: boolean;
begin
  result := ControlService(fHandle, SERVICE_CONTROL_STOP, fStatus);
end;

procedure TServiceController.SetDescription(const Description: string);
var
  desc: SynUnicode;
begin
  if Description = '' then
    exit;
  desc := SynUnicode(Description);
  ChangeServiceConfig2(fHandle, SERVICE_CONFIG_DESCRIPTION, @desc);
end;

class procedure TServiceController.CheckParameters(const ExeFileName: TFileName;
  const ServiceName, DisplayName, Description, Dependencies: string);
var
  param: string;
  i: integer;

  procedure ShowError(Msg: string);
  begin
    Msg := ServiceName + ': "' + Msg + '" failed for ' + param;
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, '%', [Msg]);
    ConsoleWrite(RawUtf8(Msg), ccLightRed);
  end;

begin
  for i := 1 to ParamCount do
  begin
    param := SysUtils.LowerCase(paramstr(i));
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllInfo,
        'Controling % with command [%]', [ServiceName, param]);
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
  const Name, DisplayName, Description: string;
  AutoStart: boolean; ExeName: TFileName; Dependencies: string): TServiceState;
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
    '', '', Name, DisplayName, ExeName, '', Dependencies, '', '',
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, start);
  try
    result := ctrl.State;
    if result <> ssNotInstalled then
      ctrl.SetDescription(Description);
  finally
    ctrl.Free;
  end;
end;


{ TService }

constructor TService.Create(const aServiceName, aDisplayName: string);
begin
  fSName := aServiceName;
  fDName := aDisplayName;
  if aDisplayName = '' then
    fDName := aServiceName;
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(sllInfo, 'Create: % (%) running as [%]',
      [ServiceName, aDisplayName, Executable.ProgramFullSpec], self);
end;

procedure TService.CtrlHandle(Code: cardinal);
begin
  DoCtrlHandle(Code);
end;

procedure TService.DoCtrlHandle(Code: cardinal);
begin
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(sllInfo, '%: command % received from OS',
      [ServiceName, Code], self);
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
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(sllInfo, '%: command % finished', [ServiceName, Code], self);
end;

procedure TService.Execute;
begin
  try
    if Assigned(fOnStart) then
      fOnStart(@Self);
    ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
    if Assigned(fOnExecute) then
      fOnExecute(@Self);
  except
    ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
  end;
end;

function TService.GetArgCount: Integer;
begin
  result := length(fArgsList);
end;

function TService.GetArgs(Idx: Integer): string;
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
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllError, '%.GetControlHandler with ControlHandler=nil: ' +
        'use TServiceSingle or assign a custom ControlHandler', [self], self);
end;

function TService.GetInstalled: boolean;
begin
  with TServiceController.CreateOpenService(
    '', '', fSName, SERVICE_QUERY_STATUS) do
  try
    result := Handle <> 0;
  finally
    Free;
  end;
end;

function TService.Install(const Params: string): boolean;
var
  schService: SC_HANDLE;
  schSCManager: SC_HANDLE;
  ServicePath: TFileName;
begin
  result := false;
  if installed then
    exit;
  ServicePath := Executable.ProgramFileName;
  if Params <> '' then
    ServicePath := ServicePath + ' ' + Params;
  schSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if schSCManager > 0 then
  begin
    schService := CreateService(schSCManager, pointer(fSName), pointer(fDName),
      SERVICE_ALL_ACCESS, fServiceType, fStartType, SERVICE_ERROR_NORMAL,
      pointer(ServicePath), nil, nil, nil, nil, nil);
    if schService > 0 then
    begin
      result := true;
      CloseServiceHandle(schService);
    end;
  end;
end;

procedure TService.Remove;
begin
  with TServiceController.CreateOpenService(
    '', '', fSName, SERVICE_ALL_ACCESS) do
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
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(sllInfo, '% ReportStatus(%,%,%)', 
      [ServiceName, status^, dwExitCode, dwWait], self);
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
    if Assigned(WindowsServiceLog) then
      WindowsServiceLog(sllLastError, '% ReportStatus(%,%,%)',
        [ServiceName, status^, dwExitCode, dwWait], self);
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
    '', '', fSName, SERVICE_ALL_ACCESS) do
  try
    Start([]);
  finally
    Free;
  end;
end;

procedure TService.Stop;
begin
  with TServiceController.CreateOpenService(
    '', '', fSName, SERVICE_ALL_ACCESS) do
  try
    Stop;
  finally
    Free;
  end;
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

function GetServicePid(const aServiceName: string): cardinal;
var
  ssp: TServiceStatusProcess;
  scm: THandle;
  svc: THandle;
  size: cardinal;
begin
  result := 0;
  scm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if scm <> 0 then
  try
    svc := OpenService(scm, pointer(aServiceName), SERVICE_QUERY_STATUS);
    if svc <> 0 then
    try
      if QueryServiceStatusEx(svc, SC_STATUS_PROCESS_INFO,
          @ssp, SizeOf(TServiceStatusProcess), size) then
        result := ssp.dwProcessId
      else if Assigned(WindowsServiceLog) then
        WindowsServiceLog(sllLastError, 'GetServicePid(%)', [aServiceName]);
    finally
      CloseServiceHandle(svc);
    end;
  finally
    CloseServiceHandle(scm);
  end;
end;

function KillProcess(pid: cardinal; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  ph := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, false, pid);
  result := ph <> 0;
  if result then
  begin
    try
      result := TerminateProcess(ph, 0) and
                (WaitForSingleObject(ph, waitseconds * 1000) <> WAIT_TIMEOUT);
    finally
      CloseHandle(ph);
    end;
  end;
end;

{  function that a service process specifies as the entry point function
   of a particular service. The function can have any application-defined name
  - Args points to an array of pointers that point to null-terminated
    argument strings. The first argument in the array is the name of the service,
    and subsequent arguments are any strings passed to the service by the process
    that called the StartService function to start the service.  }
    
procedure ServiceProc(ArgCount: integer; Args: PPChar); stdcall;
var
  i: PtrInt;
begin
  dec(ArgCount); // first argument is the service name to be ignored
  if (ArgCount < 0) or
     not Assigned(ServiceSingle) then
    exit;
  SetLength(ServiceSingle.fArgsList, ArgCount);
  for i := 0 to ArgCount - 1 do
  begin
    inc(Args);
    ServiceSingle.fArgsList[i] := Args^; // convert to string
  end;
  ServiceSingle.fStatusHandle := RegisterServiceCtrlHandler(
    pointer(ServiceSingle.fSName), @ServiceSingle.ControlHandler);
  if ServiceSingle.fStatusHandle = 0 then
  begin
    ServiceSingle.ReportStatus(SERVICE_STOPPED, GetLastError, 0);
    exit;
  end;
  ServiceSingle.ReportStatus(SERVICE_START_PENDING, 0, 0);
  ServiceSingle.Execute;
end;

function ServiceSingleRun: boolean;
var
  S: array[0..1] of TServiceTableEntry;
begin
  if ServiceSingle = nil then
  begin
    result := false;
    exit;
  end;
  S[0].lpServiceName := pointer(ServiceSingle.ServiceName);
  S[0].lpServiceProc := ServiceProc;
  S[1].lpServiceName := nil;
  S[1].lpServiceProc := nil;
  { TODO : disable EExternal exception logging in ServicesSingleRun? }
  result := StartServiceCtrlDispatcher(@S);
end;


{ TServiceSingle }

procedure SingleServiceControlHandler(Opcode: LongWord); stdcall;
begin
  if ServiceSingle <> nil then
    ServiceSingle.DoCtrlHandle(Opcode);
end;

constructor TServiceSingle.Create(const aServiceName, aDisplayName: string);
begin
  if ServiceSingle <> nil then
    raise EOSException.Create('Only one TServiceSingle is allowed at a time');
  inherited Create(aServiceName, aDisplayName);
  ServiceSingle := self;
  ControlHandler := SingleServiceControlHandler;
end;

destructor TServiceSingle.Destroy;
begin
  try
    inherited;
  finally
    ServiceSingle := nil;
  end;
end;


function DropPriviledges(const UserName: RawUtf8): boolean;
begin
  result := false;
end;

function ChangeRoot(const FolderName: RawUtf8): boolean;
begin
  result := false;
end;


// redefined here so that we can share code with FPC and Delphi
function CreateProcessW(lpApplicationName: PWideChar; lpCommandLine: PWideChar;
   lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
   bInheritHandles: BOOL; dwCreationFlags: cardinal; lpEnvironment: Pointer;
   lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfo;
   out lpProcessInformation: TProcessInformation): BOOL;
  stdcall; external kernel32;

function GetExitCodeProcess(hProcess: THandle; out lpExitCode: cardinal): BOOL;
  stdcall; external kernel32;

function GetFileSizeEx(hFile: THandle; var FileSize: Int64): BOOL;
  stdcall; external kernel32;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2, arg3, arg4, arg5, env: TFileName; envaddexisting: boolean): integer;
begin
  result := RunCommand(Format('"%s" %s %s %s %s %s',
    [path, arg1, arg2, arg3, arg4, arg5]), waitfor, env, envaddexisting);
end;

var
  EnvironmentCache: SynUnicode;
  EnvironmentCacheLock: TLightLock;

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
    SetString(EnvironmentCache, e, (PtrUInt(p) - PtrUInt(e)) shr 1);
    FreeEnvironmentStringsW(e);
  end;
  EnvironmentCacheLock.UnLock;
end;

function RunCommand(const cmd: TFileName; waitfor: boolean; const env: TFileName;
  envaddexisting: boolean; waitfordelayms: cardinal; processid: PHandle;
  redirected: PRawUtf8; const onoutput: TOnRedirect): integer;
var
  startupinfo: TStartupInfo; // _STARTUPINFOW or _STARTUPINFOA is equal here
  processinfo: TProcessInformation;
  security: TSecurityAttributes;
  exe, path: TFileName;
  rd, wr: THandle;
  // CreateProcess can alter the strings -> use local SynUnicode temp variables
  wcmd, wenv, wpath: SynUnicode;
  output: RawByteString;
  endtix, pending: Int64;
  exitcode, n: cardinal;
  i, l, ulen: PtrInt;
  tmp: TSynTempBuffer;
begin
  // https://support.microsoft.com/en-us/help/175986/info-understanding-createprocess-and-command-line-arguments
  result := -1;
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
  if exe <> '' then
    path := ExtractFilePath(exe);
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
    if envaddexisting then
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
  if redirected <> nil then
    if CreatePipe(rd, wr, @security, 0) then
    begin
      SetHandleInformation(rd, HANDLE_FLAG_INHERIT, 0);
      startupinfo.wShowWindow := SW_HIDE;
      startupinfo.hStdOutput := wr;
      startupinfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    end
    else
      redirected := nil;
  FillCharFast(processinfo, SizeOf(processinfo), 0);
  // https://docs.microsoft.com/en-en/windows/desktop/ProcThread/process-creation-flags
  if CreateProcessW(nil, pointer(wcmd), @security, @security, true,
    CREATE_UNICODE_ENVIRONMENT or CREATE_DEFAULT_ERROR_MODE or
      DETACHED_PROCESS or CREATE_NEW_PROCESS_GROUP,
    pointer({%H-}wenv), pointer(wpath), startupinfo, processinfo) then
  begin
    if processid <> nil then
      processid^ := processinfo.hProcess;
    if redirected <> nil then
    begin
      // see https://stackoverflow.com/a/25725197/458259
      l := 0;
      ulen := 0;
      if waitfordelayms = INFINITE then
        endtix := 0
      else
        endtix := GetTickCount64 + waitfordelayms;
      repeat
        exitcode := WaitForSingleObject(processinfo.hProcess, 200);
        Win32Check(exitcode <> WAIT_FAILED);
        repeat
          Win32Check(GetFileSizeEx(rd, pending));
          if pending = 0 then
          begin
            if Assigned(onoutput) and
               onoutput('') then
              exitcode := WAIT_OBJECT_0; // returned true to abort the execution
            break;
          end;
          n := 0;
          Win32Check(ReadFile(rd, tmp, SizeOf(tmp), n, nil));
          if n <= 0 then
            break;
          SetLength(output, PtrInt(n) + l);
          MoveFast(tmp, PByteArray(output)[l], n);
          inc(l, n);
          if Assigned(onoutput) then
          begin
            redirected^ := ConsoleToUtf8(output);
            if onoutput(copy(redirected^, ulen + 1, maxInt)) then
            begin
              exitcode := WAIT_OBJECT_0;
              break; // returned true to stop the execution, or false to continue
            end;
            ulen := length(redirected^); // incremental valid UTF-8 output
          end;
        until false;
      until (exitcode = WAIT_OBJECT_0) or
            ((endtix <> 0) and
             (GetTickCount64 > endtix));
      if redirected^ = '' then
        redirected^ := ConsoleToUtf8(output);
      if GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := exitcode       // waited for process to end -> return code
      else
        result := -GetLastError; // was not able to retrieve exit code
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
      result := 0; // asynchronous process launch
    CloseHandle(processinfo.hProcess);
    CloseHandle(processinfo.hThread);
  end
  else
    result := -GetLastError;
  if redirected <> nil then
  begin
    CloseHandle(rd);
    CloseHandle(wr);
  end;
end;

function RunRedirect(const cmd: TFileName; const onoutput: TOnRedirect): RawUtf8;
begin
  result := '';
  RunCommand(cmd, true, '', false, INFINITE, nil, @result, onoutput);
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
        NumaNodeNumber: DWORD);
      RelationCache: (
        Cache: record
          Level: BYTE;
          Associativity: BYTE;
          LineSize: WORD;
          Size: DWORD;
          CacheType: TSystemLogicalProcessorCache;
        end);
      RelationGroup: (Reserved: array [0..1] of QWord);
  end;
  {$A+}


{$ifndef UNICODE}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL;
  stdcall; external kernel32 name 'GetVersionExA';
{$endif UNICODE}

function GetLocalTimeOffset: Integer; // not defined in oldest Delphi
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

procedure InitializeSpecificUnit;
var
  h: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var SystemInfo: TSystemInfo); stdcall;
  GetLogicalProcessorInformation: function(
   var Info: TSystemLogicalProcessorInformation; Len: PDWORD): BOOL; stdcall;
  wine_get_version: function: PAnsiChar; stdcall;
  Res: BOOL;
  P: pointer;
  Vers: TWindowsVersion;
  cpu, manuf, prod, prodver: RawUtf8;
  reg: TWinRegistry;
  proc: array of TSystemLogicalProcessorInformation;
  i: integer;
  siz: DWORD;
begin
  {$ifndef FPC} // FPC_X86 already redirect to FastCode RTL Move()
  {$ifdef ASMX86}
  {$ifndef HASNOSSE2}
  if not (cfSSE2 in CpuFeatures) then
  begin
    // avoid illegal opcode in MoveFast() and SynLZ functions
    RedirectCode(@MoveFast, @System.Move);
    RedirectCode(@SynLZcompress1, @SynLZcompress1Pas);
    RedirectCode(@SynLZdecompress1, @SynLZdecompress1Pas);
    ConsoleWrite('WARNING: too old CPU - recompile with HASNOSSE2', ccLightRed);
    // note: FillCharFast is handled by mormot.core.base via ERMSB
    // and Byte/Word/IntegerScanIndex() are likely to GPF at runtime
  end;
  {$endif HASNOSSE2}
  {$endif ASMX86}
  {$endif FPC}
  // late-binding of newest Windows APIs
  h := GetModuleHandle(kernel32);
  GetTickCount64 := GetProcAddress(h, 'GetTickCount64');
  if not Assigned(GetTickCount64) then // WinXP+
    GetTickCount64 := @GetTickCount64ForXP;
  GetSystemTimePreciseAsFileTime :=
    GetProcAddress(h, 'GetSystemTimePreciseAsFileTime');
  if not Assigned(GetSystemTimePreciseAsFileTime) then // Win8+
    GetSystemTimePreciseAsFileTime := @GetSystemTimeAsFileTime;
  {$ifdef WITH_VECTOREXCEPT}
  AddVectoredExceptionHandler := GetProcAddress(h, 'AddVectoredExceptionHandler');
  {$endif WITH_VECTOREXCEPT}
  QueryPerformanceFrequency(PInt64(@_QueryPerformanceFrequency)^);
  if _QueryPerformanceFrequency = 0 then
    raise Exception.Create('QueryPerformanceFrequency=0'); // paranoid
  _QueryPerformanceFrequencyPer10 := _QueryPerformanceFrequency = 10000000;
  IsWow64Process := GetProcAddress(h, 'IsWow64Process');
  Res := false;
  IsWow64 := Assigned(IsWow64Process) and
             IsWow64Process(GetCurrentProcess, Res) and
             Res;
  if IsWow64 then
    // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(h, 'GetNativeSystemInfo')
  else
    @GetNativeSystemInfo := nil;
  @GetSystemTimes := GetProcAddress(h, 'GetSystemTimes');
  @GetProcessTimes := GetProcAddress(h, 'GetProcessTimes');
  @QueryFullProcessImageNameW := GetProcAddress(h, 'QueryFullProcessImageNameW');
  @GetLogicalProcessorInformation := GetProcAddress(h, 'GetLogicalProcessorInformation');
  h := Windows.LoadLibrary('Psapi.dll');
  if h >= 32 then
  begin
    @EnumProcesses := GetProcAddress(h, 'EnumProcesses');
    @GetModuleFileNameExW := GetProcAddress(h, 'GetModuleFileNameExW');
    //@EnumProcessModules := GetProcAddress(h, 'EnumProcessModules');
    @GetProcessMemoryInfo := GetProcAddress(h, 'GetProcessMemoryInfo');
  end;
  h := Windows.LoadLibrary('SHFolder.dll');
  if h >= 32 then
    // e.g. WinPE doesn't have it - https://stackoverflow.com/q/8683038/458259
    @SHGetFolderPath := GetProcAddress(h, 'SHGetFolderPath' + _AW);
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
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Vers := wUnknown;
  with OSVersionInfo do
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833
    case dwMajorVersion of
      5:
        case dwMinorVersion of
          0:
            Vers := w2000;
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
    if OSVersionInfo.wProductType <> VER_NT_WORKSTATION then
    begin
      // Server edition - see https://shorturl.at/aryAY for build numbers
      inc(Vers, 2); // e.g. wEight -> wServer2012
      if Vers = wServer2016 then
        // we identify only LTSC server versions
        if OSVersionInfo.dwBuildNumber >= 17763 then
          if OSVersionInfo.dwBuildNumber >= 20285 then // released as 20348
            Vers := wServer2022_64
          else
            Vers := wServer2019_64;
    end
    else if (Vers = wTen) and
            (OSVersionInfo.dwBuildNumber >= 22000) then
      // Windows 11 has always 22000.### build https://shorturl.at/fntPS
      Vers := wEleven;
    if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) and
       (Vers < wServer2019_64) then
      inc(Vers);   // e.g. wEight -> wEight64
  end;
  OSVersion := Vers;
  if OSVersion >= wVista then
    OpenProcessAccess := PROCESS_QUERY_LIMITED_INFORMATION
  else
    OpenProcessAccess := PROCESS_QUERY_INFORMATION or PROCESS_VM_READ;
  with OSVersionInfo do
    if wServicePackMajor = 0 then
      OSVersionText := _fmt('Windows %s (%d.%d.%d)',
        [WINDOWS_NAME[Vers], dwMajorVersion, dwMinorVersion, dwBuildNumber])
    else
      OSVersionText := _fmt('Windows %s SP%d (%d.%d.%d)', [WINDOWS_NAME[Vers],
         wServicePackMajor, dwMajorVersion, dwMinorVersion, dwBuildNumber]);
  OSVersion32.os := osWindows;
  OSVersion32.win := Vers;
  OSVersion32.winbuild := OSVersionInfo.dwBuildNumber;
  h := Windows.LoadLibrary(ntdll);
  if h > 0 then
  begin
    wine_get_version := GetProcAddress(h, 'wine_get_version');
    if Assigned(wine_get_version) then
    begin
      OSVersionInfoEx := wine_get_version;
      OSVersionInfoEx := TrimU('Wine ' + TrimU(OSVersionInfoEx));
    end;
    NtQueryInformationProcess := GetProcAddress(h, 'NtQueryInformationProcess');
    RtlInitUnicodeString := GetProcAddress(h, 'RtlInitUnicodeString');
    NtOpenSection := GetProcAddress(h, 'NtOpenSection');
    NtMapViewOfSection := GetProcAddress(h, 'NtMapViewOfSection');
    NtUnmapViewOfSection := GetProcAddress(h, 'NtUnmapViewOfSection');
  end;
  if OSVersionInfoEx <> '' then
    OSVersionText := OSVersionText + ' - ' + OSVersionInfoEx;
  // retrieve Hardware information from Registry
  if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System\CentralProcessor\0') then
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
    SetLength(proc, 512);
    siz := SizeOf(proc[0]) * 512;
    if GetLogicalProcessorInformation(Proc[0], @siz) then
      for i := 0 to (siz div SizeOf(proc[0])) - 1 do
        with Proc[i] do
          case Relationship of
            RelationCache:
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
        cpu := _fmt('%s %s cache', [cpu, _oskb(CpuCacheSize)]);
        break;
      end;
    end;
    for i := low(CpuCache) to high(CpuCache) do
      with CpuCache[i] do
        if Count <> 0 then       
          if Count = 1 then
            CpuCacheText := _fmt('%s L%d=%s ', [CpuCacheText, i, _oskb(Size)])
          else
            CpuCacheText := _fmt('%s L%d=%d*%s ', [CpuCacheText, i, Count, _oskb(Size)]);
    TrimSelf(CpuCacheText);
  end;
  CpuInfoText := _fmt('%d x %s (' + CPU_ARCH_TEXT + ')',
    [SystemInfo.dwNumberOfProcessors, cpu]);
  // writeln(CpuInfoText); writeln(CpuCacheText);
end;

procedure FinalizeSpecificUnit;
begin
  if CryptoApi.Handle <> 0 then
    Windows.FreeLibrary(CryptoApi.Handle);
  if OleDBCoinitialized <> 0 then
    ConsoleWrite('Missing CoUninitialize TOleDBConnection.Destroy call');
end;



