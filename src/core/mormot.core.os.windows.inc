{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Windows API calls for FPC/Delphi, as used by mormot.core.os.pas
}

uses
  messages;


{ ****************** Unicode, Time, File process }

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

procedure FileTimeToInt64(const FT: TFileTime; out I64: Int64);
begin
  {$ifdef CPU64}
  PInt64Rec(@I64)^.Lo := FT.dwLowDateTime;
  PInt64Rec(@I64)^.Hi := FT.dwHighDateTime;
  {$else}
  I64 := PInt64(@FT)^;
  {$endif}
end;

const
  UnixDelta = 25569;
  UnixFileTimeDelta = 116444736000000000; // from year 1601 to 1970
  DateFileTimeDelta =  94353120000000000; // from year 1601 to 1899

function FileTimeToUnixTime(const FT: TFileTime): Int64;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif}
begin
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div 10000000;
  {$else} // use PInt64 to avoid URW699 with Delphi 6 / Kylix
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div 10000000;
  {$endif}
end;

function FileTimeToUnixMSTime(const FT: TFileTime): Int64;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif}
begin
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div 10000;
  {$else} 
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div 10000;
  {$endif}
end;

function UnixTimeUTC: Int64;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // fast but with HW low resolution
  result := FileTimeToUnixTime(ft);
end;

var
  GetSystemTimePreciseAsFileTime: procedure(var ft: TFILETIME); stdcall;

function UnixMSTimeUTC: Int64;
var
  ft: TFileTime;
begin
  GetSystemTimePreciseAsFileTime(ft); // slower, but try to achieve ms resolution
  result := FileTimeToUnixMSTime(ft);
end;

function UnixMSTimeUTCFast: Int64;
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
procedure AllocConsole;               external kernel32;
procedure FileClose;                  external kernel32 name 'CloseHandle';
function  GetCurrentThreadId;         external kernel32;
function  GetCurrentProcessId;        external kernel32;
function  GetEnvironmentStringsW;     external kernel32;
function  FreeEnvironmentStringsW;    external kernel32;
function  RtlCaptureStackBackTrace;   external kernel32;
function  IsDebuggerPresent;          external kernel32;
procedure SetEndOfFile;               external kernel32;
procedure FlushFileBuffers;           external kernel32;
function  GetLastError;               external kernel32;
procedure SetLastError;               external kernel32;

function GetModuleHandle(lpModuleName: PChar): HMODULE;
begin
  result := Windows.GetModuleHandle(lpModuleName); // call either A or W API
end;

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  result := not IsZero(@cs, SizeOf(cs));
end;

var
  _QueryPerformanceFrequency: QWord; // is documented as stable after boot

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  cnt, freq: QWord;
begin
  QueryPerformanceCounter(PInt64(@cnt)^);
  freq := _QueryPerformanceFrequency;
  if freq = 10000000 then
    // from HyperV or if HPET disabled e.g. -> faster division by a constant
    Value := cnt div 10
  else
    Value := (cnt * 1000000) div freq;
end;

const
  faInvalidFile   = faDirectory + faVolumeID{%H-} + faSysFile{%H-} + faHidden{%H-};
  faDirectoryMask = faDirectory + faHidden{%H-};

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  ST, LT: TSystemTime;
begin
  // 5 times faster than CreateFile, GetFileSizeEx, CloseHandle
  if GetFileAttributesEx(pointer(FileName), GetFileExInfoStandard, @FA) and
     FileTimeToSystemTime({%H-}FA.ftLastWriteTime, ST) and
     SystemTimeToTzSpecificLocalTime(nil, ST, LT) then
    result := SystemTimeToDateTime(LT)
  else
    result := 0;
end;

function FileAgeToWindowsTime(const FileName: TFileName): integer;
begin
  result := FileAge(FileName);
end;

function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;
begin
  result := FileSetDate(Dest, WinTime) = 0;
end;

function FileInfoByHandle(aFileHandle: THandle;
  out FileId, FileSize, LastWriteAccess, FileCreateDateTime: Int64): Boolean;
var
  lastreadaccess: Int64;
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
    if (FileCreateDateTime = 0) or (FileCreateDateTime > LastWriteAccess) then
      FileCreateDateTime := LastWriteAccess;
  if lastreadaccess <> 0 then
    if (FileCreateDateTime = 0) or (FileCreateDateTime > lastreadaccess) then
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
begin
  // 5 times faster than CreateFile, GetFileSizeEx, CloseHandle
  if GetFileAttributesEx(pointer(FileName), GetFileExInfoStandard, @FA) then
  begin
    PInt64Rec(@result)^.Lo := FA.nFileSizeLow;
    PInt64Rec(@result)^.Hi := FA.nFileSizeHigh;
  end
  else
    result := 0;
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
  r.L := SetFilePointer(Handle, R.L, @R.H, Origin);
  if (r.Li = -1) and (GetLastError <> 0) then
    result := -1
  else
    result := r.V;
end;

function DeleteFile(const aFileName: TFileName): boolean;
begin
  result := SysUtils.DeleteFile(aFileName);
end;

function FileCreate(const aFileName: TFileName): THandle;
begin
  result := SysUtils.FileCreate(aFileName);
end;

function FileSetDateFrom(const Dest: TFileName; SourceHandle: integer): boolean;
var
  FileTime: TFileTime;
  D: THandle;
begin
  D := FileOpen(Dest, fmOpenWrite);
  if D<>THandle(-1) then
  begin
    result := GetFileTime(SourceHandle, nil, nil, @FileTime) and
              SetFileTime(D, nil, nil, @FileTime);
    FileClose(D);
  end
  else
    result := false;
end;

procedure FileSetAttributes(const FileName: TFileName; Secret: boolean);
begin
  if Secret then
    SetFileAttributes(pointer(FileName),
      FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_READONLY)
  else
    SetFileAttributes(pointer(FileName), FILE_ATTRIBUTE_NORMAL);
end;

function RenameFile(const OldName, NewName: TFileName): boolean;
begin
  result := SysUtils.RenameFile(OldName,NewName);
end;

function FileOpenSequentialRead(const FileName: string): Integer;
begin
  if OSVersion >= wVista then // don't use the flag on XP
    result := CreateFile(pointer(FileName), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, // same as fmShareDenyNone
      OPEN_EXISTING,FILE_FLAG_SEQUENTIAL_SCAN, 0)
  else
    result := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
end;

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

threadvar // mandatory: GetTickCount seems per-thread on XP :(
  LastTickXP: TQWordRec;

function GetTickCount64ForXP: Int64; stdcall;
var
  t32: cardinal;
  p: PQWordRec;
begin // warning: GetSystemTimeAsFileTime() is fast, but not monotonic!
  t32 := Windows.GetTickCount; // we only have the 32-bit counter
  p := @LastTickXP;
  if t32 < p^.L then
    inc(p^.H);  // wrap-up overflow after 49 days
  p^.L := t32;
  result := p^.V;
end; // warning: FPC's GetTickCount64 doesn't handle 49 days wrap on XP :(

{$ifdef FPC} // oddly not defined in fpc\rtl\win
function SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
{$endif FPC}

procedure SleepHiRes(ms: cardinal);
begin
  if (ms <> 0) or not SwitchToThread then
    Windows.Sleep(ms);
end;

{$ifdef FPC}
  {$define NOSETTHREADNAME} // only tested and supported on Delphi
{$endif FPC}

{$ifdef NOSETTHREADNAME}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUTF8);
begin
end;

{$else}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUTF8);
var s: AnsiString;
    {$ifndef ISDELPHIXE2}
    {$ifdef MSWINDOWS}
    info: record
      FType: LongWord;     // must be 0x1000
      FName: PAnsiChar;    // pointer to name (in user address space)
      FThreadID: LongWord; // thread ID (-1 indicates caller thread)
      FFlags: LongWord;    // reserved for future use, must be zero
    end;
    {$endif}
    {$endif}
begin
  if not IsDebuggerPresent then
    exit;
  s := AnsiString(Name);
  {$ifdef ISDELPHIXE2}
  TThread.NameThreadForDebugging(s,ThreadID); // use
  {$else}
  info.FType := $1000;
  info.FName := pointer(s);
  info.FThreadID := ThreadID;
  info.FFlags := 0;
  try
    RaiseException($406D1388,0,SizeOf(info) div SizeOf(LongWord),@info);
  except {ignore} end;
  {$endif ISDELPHIXE2}
end;

{$endif NOSETTHREADNAME}

{$ifndef NOEXCEPTIONINTERCEPT}

const
  // see http://msdn.microsoft.com/en-us/library/xcb2z8hs
  cSetThreadNameException = $406D1388;

  // https://docs.microsoft.com/en-us/archive/blogs/yizhang/interpreting-hresults-returned-from-netclr-0x8013xxxx
  DOTNET_EXCEPTIONNAME: array[0..83] of PUTF8Char = (
    'Access', 'AmbiguousMatch', 'appdomainUnloaded', 'Application', 'Argument',
    'ArgumentNull', 'ArgumentOutOfRange', 'Arithmetic', 'ArrayTypeMismatch',
    'BadImageFormat', 'CannotUnloadappdomain', 'ContextMarshal', 'Cryptographic',
    'CryptographicUnexpectedOperation', 'CustomAttributeFormat',
    'DirectoryNotFound', 'DirectoryNotFound', 'DivideByZero', 'DllNotFound',
    'DuplicateWaitObject', 'EndOfStream', 'EntryPointNotFound', '',
    'ExecutionEngine', 'External', 'FieldAccess', 'FileLoad', 'FileLoad',
    'FileNotFound', 'Format', 'IndexOutOfRange', 'InvalidCast',
    'InvalidComObject', 'InvalidFilterCriteria', 'InvalidOleVariantType',
    'InvalidOperation', 'InvalidProgram', 'IO', 'IsolatedStorage',
    'MarshalDirective', 'MethodAccess', 'MissingField',
    'MissingManifestResource', 'MissingMember', 'MissingMethod',
    'MulticastNotSupported', 'NotFiniteNumber', 'NotImplemented',
    'NotSupported', 'NullReference', 'OutOfMemory', 'Overflow',
    'PlatformNotSupported', 'Policy', 'Rank', 'ReflectionTypeLoad', 'Remoting',
    'RemotingTimeout', 'SafeArrayTypeMismatch', 'SafeArrayRankMismatch',
    'Security', 'SEH', 'Serialization', 'Server', 'StackOverflow',
    'SUDSGenerator', 'SUDSParser', 'SynchronizationLock', 'System', 'Target',
    'TargetInvocation', 'TargetParameterCount', 'ThreadAbort',
    'ThreadInterrupted', 'ThreadState', 'ThreadStop', 'TypeInitialization',
    'TypeLoad', 'TypeUnloaded', 'UnauthorizedAccess', 'InClassConstructor',
    'KeyNotFound', 'InsufficientStack', 'InsufficientMemory');

  DOTNET_EXCEPTIONHRESULT: array[0..83] of cardinal = (
    $8013151A, $8000211D, $80131015, $80131600, $80070057, $80004003, $80131502,
    $80070216, $80131503, $8007000B, $80131015, $80090020, $80004001, $80131431,
    $80131537, $80070003, $80030003, $80020012, $80131524, $80131529, $00801338,
    $80131522, $80131500, $80131506, $80004005, $80131507, $80131621, $80131018,
    $80070002, $80131537, $80131508, $80004002, $80131527, $80131601, $80131531,
    $80131509, $8013153A, $80131620, $80131450, $80131535, $80131510, $80131511,
    $80131532, $80131512, $80131513, $80131514, $80131528, $80004001, $80131515,
    $80004003, $8007000E, $80131516, $80131539, $80131416, $80131517, $80131602,
    $8013150B, $8013150B, $80131533, $80131538, $8013150A, $80004005, $8013150C,
    $8013150E, $800703E9, $80131500, $80131500, $80131518, $80131501, $80131603,
    $80131604, $80138002, $80131530, $80131519, $80131520, $80131521, $80131534,
    $80131522, $80131013, $80070005, $80131543, $80131577, $80131578, $8013153D);

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

function ExceptionInheritsFrom(E: TClass; const Name: ShortString): boolean;
begin
  result := true;
  while (E <> nil) and (E <> Exception) do
    if PropNameEquals(PPointer(PtrInt(E) + vmtClassName)^, @Name) then
      exit
    else
      E := GetClassParent(E);
  result := false;
end;

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUTF8CharDynArray): cardinal;
var
  i: PtrInt;
begin
  if ExceptionInheritsFrom(EClass, 'EOleSysError') then
  begin
    result := EOleSysError(EInstance).ErrorCode;
    for i := 0 to high(DOTNET_EXCEPTIONHRESULT) do
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
    // Win32, Linux: intercept via the RaiseProc global variable
    {$define WITH_RAISEPROC} // RaiseProc is set in main mormot.core.os.pas
  {$endif}
{$else}
  {$ifdef CPU64}
    {$define WITH_VECTOREXCEPT}
  {$else}
    {$define WITH_RTLUNWINDPROC} //  use x86_64 asm -> Win32 only
  {$endif}
{$endif FPC}

{$ifndef WITH_RAISEPROC}

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: Longint;
    case {IsOsException:} Boolean of
      true:
        (ExceptionInformation: array[0..14] of PtrUInt);
      false:
        (ExceptAddr: PtrUInt;
         ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  {$ifdef FPC}
  cDelphiException = $E0465043;
  {$else}
  cDelphiException = $0EEDFADE;
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
  try
    _RawLogException := nil; // disable nested exception
    ctxt.ECode := Exc.ExceptionCode;
    if (Exc.ExceptionCode = cDelphiException) and (Exc.ExceptObject <> nil) then
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
    ctxt.ETimestamp := UnixTimeUTC; // fast API call
    _RawLogException(ctxt);
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
  oldUnWindProc: pointer;

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
        jmp     oldUnWindProc
end;

{$endif WITH_RTLUNWINDPROC}

{$endif WITH_RAISEPROC}

{$endif NOEXCEPTIONINTERCEPT}

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
  EnumProcessModules: function (hProcess: THandle; var lphModule: HMODULE; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
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

function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;
var n: cardinal;
begin
  result := nil;
  n := 2048;
  repeat
    SetLength(result, n);
    if EnumProcesses(pointer(result), n * 4, Count) then
      Count := Count shr 2
    else // Count=n if buffer is too small
      Count := 0; // error
    if Count < n then
    begin
      if Count = 0 then
        result := nil;
      exit;
    end;
    inc(n, 1024); // (very unlikely) too small buffer
  until n > 8192;
end;

procedure RawUnicodeToUtf8(P: PWideChar; Len: integer; out res: RawUTF8);
var // defined here to avoid linking mormot.core.text.pas
  tmp: TSynTempBuffer;
begin
  if Len <= 0 then
    exit;
  tmp.Init(Len * 3);
  Len := UnicodeToUtf8(tmp.Buf, Len * 3, P, Len); // use RTL function
  if Len > 0 then
    dec(Len); // UnicodeToUtf8() result includes the null terminator
  FastSetString(res, tmp.buf, Len);
  tmp.Done;
end;

function GetErrorText(error: longint): RawUTF8;
var
  Len: Integer;
  Buffer: array[0..511] of WideChar;
begin
  Len := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, error, 0, Buffer{%H-}, SizeOf(Buffer), nil);
  while (Len > 0) and (ord(Buffer[Len - 1]) in [0..32, ord('.')]) do
    dec(Len);
  RawUnicodeToUtf8(Buffer, Len, result);
end;

function EnumProcessName(PID: Cardinal): RawUTF8;
var h: THandle;
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
          RawUnicodeToUtf8(name, len, result);
      end else
        if GetModuleFileNameExW(h,0,name,high(name)) <> 0 then
          RawUnicodeToUtf8(name, StrLenW(name), result);
    finally
      CloseHandle(h);
    end;
end;

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
begin
  result := Assigned(GetSystemTimes) and GetSystemTimes(ftidl, ftkrn, ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl, IdleTime);
  FileTimeToInt64(ftkrn, KernelTime);
  FileTimeToInt64(ftusr, UserTime);
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  mem: TProcessMemoryCounters;
begin
  result := false;
  if not Assigned(GetProcessTimes) or not Assigned(GetProcessMemoryInfo) then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h = 0 then
    exit;
  try
    if GetProcessTimes(h, ftp, fte, ftkrn, ftusr) then
    begin
      FileTimeToInt64(ftkrn, KernelTime);
      FileTimeToInt64(ftusr, UserTime);
      FillCharFast(mem,SizeOf(mem),0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h,mem,SizeOf(mem)) then
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

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self, SizeOf(self), 0);
  result := Assigned(GetSystemTimes) and Assigned(GetProcessTimes) and
    Assigned(GetProcessMemoryInfo); // no monitoring API under oldest Windows
end;

function TProcessInfo.Start: boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
  sidl, skrn, susr: Int64;
begin
  result := Assigned(GetSystemTimes) and GetSystemTimes(ftidl, ftkrn, ftusr);
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
      if (PrevKernel <> 0) and (fDiffTotal > 0) then
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
    Kernel := SimpleRoundTo2Digits((fDiffKernel * 100) / fDiffTotal);
    User := SimpleRoundTo2Digits((fDiffUser * 100) / fDiffTotal);
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
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall; external kernel32;
{$endif UNICODE}

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
{$ifdef WITH_FASTMM4STATS}
var
  Heap: TMemoryManagerState;
  sb: integer;
{$endif}
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
  if withalloc and Assigned(GetProcessMemoryInfo) then
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
      ExtractFileDrive(ExeVersion.ProgramFilePath));
  dn := SynUnicode(aDriveFolderOrFile);
  if (dn <> '') and (dn[2] = ':') and (dn[3] = #0) then
    dn := dn + '\';
  if (aVolumeName <> nil) and (aVolumeName^ = '') then
  begin
    tmp[0] := #0;
    GetVolumeInformationW(pointer(dn), tmp, MAX_PATH, nil, dummy, flags, nil, 0);
    aVolumeName^ := tmp;
  end;
  result := GetDiskFreeSpaceExW(pointer(dn), aAvailableBytes, aTotalBytes, aFreeBytes);
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
  begin // retrieve partitions mounted as C..Z drives
    if drives and m <> 0 then
    begin
      fn[1] := char(64 + drive);
      if GetDiskInfo(fn, av, fr, tot, @volume) then
      begin
        SetLength(result, n + 1);
        RawUnicodeToUtf8(pointer(volume), length(volume), result[n].name);
        volume := '';
        result[n].mounted := fn;
        result[n].size := tot;
        inc(n);
      end;
    end;
    m := m shl 1;
  end;
end;

{$I-}

var
  TextAttr: integer = ord(ccDarkGray);

procedure InitConsole;
begin
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if StdOut = INVALID_HANDLE_VALUE then
    StdOut := 0;
end;

procedure TextColor(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if StdOut = 0 then
    InitConsole;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $F0) or ord(Color);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TextBackground(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if StdOut = 0 then
    InitConsole;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $0F) or (ord(Color) shl 4);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

function ConsoleKeyPressed(ExpectedKey: Word): Boolean;
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

procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  while not ConsoleKeyPressed(VK_RETURN) do
  begin
    if GetCurrentThreadID = MainThreadID then
      CheckSynchronize{$ifdef WITHUXTHEME}(1000){$endif}
    else
      WaitMessage;
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
      if msg.Message = WM_QUIT then
        exit
      else
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
  end;
end;

function Utf8ToConsole(const S: RawUTF8): RawByteString;
var
  tmp: SynUnicode;
begin
  tmp := UTF8Decode(S);
  SetLength(result, length(tmp) * 4);
  CharToOemW(pointer(tmp), pointer(result));
  SetLength(result, StrLen(pointer(result)));
end;

{$I+}

constructor TFileVersion.Create(const aFileName: TFileName;
  aMajor, aMinor, aRelease, aBuild: integer);
var
  M, D: word;
  Size, Size2: DWord;
  Pt: Pointer;
  Trans: PWordArray;
  LanguageInfo: RawUTF8;
  Info: ^TVSFixedFileInfo;
  FileTime: TFILETIME;
  SystemTime: TSYSTEMTIME;
  tmp: TFileName;

  function ReadResourceByName(const From: RawUTF8): RawUTF8;
  var
    StrValPt: pointer;
    sz: DWord;
    u: SynUnicode;
  begin
    u := UTF8Decode('\StringFileInfo\' + LanguageInfo + '\' + From);
    VerQueryValueW(Pt, pointer(u), StrValPt, sz);
    if sz > 0 then
      RawUnicodeToUtf8(StrValPt, StrLenW(StrValPt), result)
    else
      result := '';
  end;

begin
  fFileName := aFileName;
  if aFileName <> '' then
  begin
    // GetFileVersionInfo modifies the filename parameter data while parsing
    // Copy the string const into a local variable to create a writeable copy
    SetString(tmp, PChar(aFileName), length(aFileName));
    Size := GetFileVersionInfoSize(pointer(tmp), Size2);
    if Size > 0 then
    begin
      GetMem(Pt, Size);
      try
        GetFileVersionInfo(pointer(aFileName), 0, Size, Pt);
        VerQueryValue(Pt, '\', pointer(Info), Size2);
        with Info^ do
        begin
          if Version32 = 0 then
          begin
            aMajor := dwFileVersionMS shr 16;
            aMinor := word(dwFileVersionMS);
            aRelease := dwFileVersionLS shr 16;
          end;
          aBuild := word(dwFileVersionLS);
          if (dwFileDateLS <> 0) and (dwFileDateMS <> 0) then
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
          LanguageInfo := RawUTF8(format('%4.4x%4.4x', [Trans^[0], Trans^[1]]));
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

procedure GetUserHost(out User, Host: RawUTF8);
var
  tmp: array[byte] of WideChar;
  tmpsize: cardinal;
begin
  tmpsize := SizeOf(tmp);
  GetComputerNameW(tmp{%H-}, tmpsize);
  RawUnicodeToUtf8(@tmp, StrLenW(tmp), Host);
  tmpsize := SizeOf(tmp);
  GetUserNameW(tmp, tmpsize);
  RawUnicodeToUtf8(@tmp, StrLenW(tmp), User);
end;

// avoid unneeded reference to ShlObj.pas
function SHGetFolderPath(hwnd: hwnd; csidl: Integer; hToken: THandle; dwFlags: DWord;
  pszPath: PChar): HRESULT; stdcall; external 'SHFolder.dll' name
  {$ifdef UNICODE} 'SHGetFolderPathW'; {$else} 'SHGetFolderPathA'; {$endif}

var
  _SystemPath: array[TSystemPath] of TFileName;

function GetSystemPath(kind: TSystemPath): TFileName;
const
  CSIDL_PERSONAL = $0005;
  CSIDL_LOCAL_APPDATA = $001C; // local non roaming user folder
  CSIDL_COMMON_APPDATA = $0023;
  CSIDL_COMMON_DOCUMENTS = $002E;
  csidl: array[TSystemPath] of integer =(
    // spCommonData,       spUserData,          spCommonDocuments
    CSIDL_COMMON_APPDATA, CSIDL_LOCAL_APPDATA, CSIDL_COMMON_DOCUMENTS,
    // spUserDocuments, spTempFolder, spLog
    CSIDL_PERSONAL,     0,      CSIDL_LOCAL_APPDATA);
  ENV: array[TSystemPath] of TFileName = (
    'ALLUSERSAPPDATA', 'LOCALAPPDATA', '', '', 'TEMP', 'LOCALAPPDATA');
var
  tmp: array[0..MAX_PATH] of char;
begin
  result := _SystemPath[kind];
  if result <> '' then
    exit;
  if (kind = spLog) and IsDirectoryWritable(ExeVersion.ProgramFilePath) then
    result := EnsureDirectoryExists(ExeVersion.ProgramFilePath + 'log')
  else if (csidl[kind] <> 0) and
          (SHGetFolderPath(0, csidl[kind], 0, 0, @tmp) = S_OK) then
    result := IncludeTrailingPathDelimiter(tmp)
  else
  begin
    result := GetEnvironmentVariable(ENV[kind]);
    if result = '' then
      result := GetEnvironmentVariable('APPDATA');
    result := IncludeTrailingPathDelimiter(result);
  end;
  _SystemPath[kind] := result;
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
    HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS);

function TWinRegistry.ReadOpen(root: TWinRegistryRoot; const keyname: RawUTF8;
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

function TWinRegistry.ReadString(const entry: SynUnicode; andtrim: boolean): RawUTF8;
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
      REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ:
        RawUnicodeToUtf8(tmp.buf, StrLenW(tmp.buf), result);
    end;
    if andtrim then
      result := Trim(result);
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

function TWinRegistry.ReadEnumEntries: TRawUTF8DynArray;
var
  count, maxlen, i, len: DWORD;
  tmp: TSynTempBuffer;
begin
  result := nil;
  if (RegQueryInfoKeyW(key, nil, nil, nil, @count, @maxlen,
       nil, nil, nil, nil, nil, nil) <> 0) or
     ({%H-}count = 0) then
    exit;
  SetLength(result, count);
  inc(maxlen);
  tmp.Init(maxlen * 3);
  for i := 0 to count - 1 do
  begin
    len := maxlen;
    if RegEnumKeyExW(key, i, tmp.buf, len, nil, nil, nil, nil) = 0 then
      RawUnicodeToUtf8(tmp.buf, len, result[i]);
  end;
  tmp.Done;
end;


{ TWinCryptoAPI }

function TWinCryptoAPI.Available: boolean;
begin
  if not Tested then
    Resolve;
  result := Assigned(AcquireContextA);
end;

procedure TWinCryptoAPI.Resolve;
const
  NAMES: array[0..7] of PChar = (
    'CryptAcquireContextA', 'CryptReleaseContext', 'CryptImportKey',
    'CryptSetKeyParam', 'CryptDestroyKey', 'CryptEncrypt', 'CryptDecrypt',
    'CryptGenRandom');
var
  P: PPointer;
  i: integer;
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
end;

type
  {$ifdef FPC}
  {$PACKRECORDS C} // mandatory under Win64
  {$endif}
  DATA_BLOB = record
    cbData: DWORD;
    pbData: PAnsiChar;
  end;

  PDATA_BLOB = ^DATA_BLOB;
  {$ifdef FPC}
  {$PACKRECORDS DEFAULT}
  {$endif}

const
  CRYPTDLL = 'Crypt32.dll';
  CRYPTPROTECT_UI_FORBIDDEN = $1;

function CryptProtectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): BOOL; stdcall; external CRYPTDLL name 'CryptProtectData';

function CryptUnprotectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): Bool; stdcall; external CRYPTDLL name 'CryptUnprotectData';

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
    SetString(result, dst.pbData, dst.cbData);
    LocalFree(HLOCAL(dst.pbData));
  end
  else
    result := '';
end;


{ ****************** Gather Operating System Information }

const
  // lpMinimumApplicationAddress retrieved from Windows is very low ($10000)
  // - i.e. maximum number of ID per table would be 65536 in TSQLRecord.GetID
  // - so we'll force an higher and almost "safe" value as 1,048,576
  // (real value from runnning Windows is greater than $400000)
  MIN_PTR_VALUE = $100000;

  // see http://msdn.microsoft.com/en-us/library/ms724833(v=vs.85).aspx
  VER_NT_WORKSTATION = 1;
  VER_NT_DOMAIN_CONTROLLER = 2;
  VER_NT_SERVER = 3;
  SM_SERVERR2 = 89;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

{$ifndef UNICODE}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';
{$endif UNICODE}

procedure TrimSpaces(var s: RawUTF8);
var
  i: integer;
begin
  repeat
    i := PosEx('  ', s);
    if i = 0 then
      break;
    delete(s, i, 1);
  until false;
  s := Trim(s);
end;

procedure InitializeUnit;
var
  h: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var SystemInfo: TSystemInfo); stdcall;
  wine_get_version: function: PAnsiChar; stdcall;
  Res: BOOL;
  P: pointer;
  Vers: TWindowsVersion;
  cpu, manuf, prod, prodver: RawUTF8;
  reg: TWinRegistry;
begin
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
  IsWow64Process := GetProcAddress(h, 'IsWow64Process');
  Res := false;
  IsWow64 := Assigned(IsWow64Process) and
             IsWow64Process(GetCurrentProcess, Res) and Res;
  if IsWow64 then
    // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(h, 'GetNativeSystemInfo')
  else
    @GetNativeSystemInfo := nil;
  @GetSystemTimes := GetProcAddress(h, 'GetSystemTimes');
  @GetProcessTimes := GetProcAddress(h, 'GetProcessTimes');
  @QueryFullProcessImageNameW := GetProcAddress(h, 'QueryFullProcessImageNameW');
  h := LoadLibrary('Psapi.dll');
  if h >= 32 then
  begin
    @EnumProcesses := GetProcAddress(h, 'EnumProcesses');
    @GetModuleFileNameExW := GetProcAddress(h, 'GetModuleFileNameExW');
    @EnumProcessModules := GetProcAddress(h, 'EnumProcessModules');
    @GetProcessMemoryInfo := GetProcAddress(h, 'GetProcessMemoryInfo');
  end;
  // retrieve system information
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
    begin // Server edition
      inc(Vers, 2); // e.g. wEight -> wServer2012
      if (Vers = wServer2016) and (OSVersionInfo.dwBuildNumber >= 17763) then
        // https://stackoverflow.com/q/53393150
        Vers := wServer2019_64;
    end;
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
      OSVersionText := RawUTF8(Format('Windows %s (%d.%d.%d)',
        [WINDOWS_NAME[Vers], dwMajorVersion, dwMinorVersion, dwBuildNumber]))
    else
      OSVersionText := RawUTF8(Format(
        'Windows %s SP%d (%d.%d.%d)', [WINDOWS_NAME[Vers],
         wServicePackMajor, dwMajorVersion, dwMinorVersion, dwBuildNumber]));
  OSVersionInt32 := (integer(Vers) shl 8) + ord(osWindows);
  h := LoadLibrary('ntdll.dll');
  if h > 0 then
  begin
    wine_get_version := GetProcAddress(h, 'wine_get_version');
    if Assigned(wine_get_version) then
    begin
      OSVersionInfoEx := wine_get_version;
      OSVersionInfoEx := trim('Wine ' + trim(OSVersionInfoEx));
    end;
    FreeLibrary(h);
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
  if ({%H-}prod = '') or ({%H-}prodver = '') then
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
    cpu := RawUTF8(GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
  cpu := Trim(cpu);
  CpuInfoText := RawUTF8(Format('%d x %s (' + CPU_ARCH_TEXT + ')',
    [SystemInfo.dwNumberOfProcessors, cpu]));
  TrimSpaces(OSVersionText);
  TrimSpaces(OSVersionInfoEx);
  TrimSpaces(BiosInfoText);
  TrimSpaces(CpuInfoText);
end;

