{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   POSIX API calls for FPC, as used by mormot.core.os.pas
}

uses
  baseunix,
  unix,
  unixcp,
  unixtype,
  {$ifdef BSD}
  sysctl,
  {$else}
  linux,
  syscall,
  {$endif BSD}
  {$ifdef FPCUSEVERSIONINFO} // to be enabled in mormot.defines.inc
    fileinfo,        // FPC 3.0 and up
    {$ifdef DARWIN}
      machoreader,   // MACH-O executables
    {$else}
      elfreader,     // ELF executables
    {$endif DARWIN}
  {$endif FPCUSEVERSIONINFO}
  termio,
  dl;


{ ****************** Unicode, Time, File process }
var
  /// contains the curent system code page (default WinAnsi)
  GetACP: integer = CODEPAGE_US;

  /// contains the current Linux kernel revision, as one 24-bit integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;

const
  // for CompareStringW()
  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1 shl ord(coIgnoreCase); // [widestringmanager.coIgnoreCase]

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD;
  lpString1: PWideChar; cchCount1: longint; lpString2: PWideChar; cchCount2: longint): longint;
var
  U1, U2: UnicodeString; // faster than WideString on Windows
begin // not inlined to avoid try..finally UnicodeString protection
  if cchCount1 < 0 then
    cchCount1 := StrLenW(lpString1);
  SetString(U1, lpString1, cchCount1);
  if cchCount2 < 0 then
    cchCount2 := StrLenW(lpString2);
  SetString(U2, lpString2, cchCount2);
  result := widestringmanager.CompareUnicodeStringProc(U1, U2, TCompareOptions(dwCmpFlags)) + 2;
end;

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
var
  tmp: UnicodeString; // not called often on UNIX which is likely to be UTF-8
begin
  widestringmanager.Ansi2UnicodeMoveProc(A, {$ifdef ISFPC27}CodePage, {$endif} tmp, LA);
  result := length(tmp);
  MoveFast(Pointer(tmp)^, W^, result * 2);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
var
  tmp: RawByteString;
begin
  widestringmanager.Unicode2AnsiMoveProc(W, tmp, {$ifdef ISFPC27}CodePage, {$endif} LA);
  result := length(tmp);
  MoveFast(Pointer(tmp)^, A^, result);
end;

const // Date Translation - see http://en.wikipedia.org/wiki/Julian_day
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  MinsPerDay  = HoursPerDay * MinsPerHour;
  SecsPerDay  = MinsPerDay * SecsPerMin;
  SecsPerHour = MinsPerHour * SecsPerMin;
  C1970       = 2440588;
  D0          = 1461;
  D1          = 146097;
  D2          = 1721119;
  UnixDelta   = 25569;
  
  THOUSAND = Int64(1000);
  MILLION  = Int64(THOUSAND * THOUSAND);

procedure JulianToGregorian(JulianDN: PtrUInt; out result: TSystemTime);
  {$ifdef HASINLINE} inline;{$endif}
var
  YYear, XYear, Temp, TempMonth: PtrUInt;
begin
  Temp := ((JulianDN - D2) * 4) - 1;
  JulianDN := Temp div D1;
  XYear := (Temp - (JulianDN * D1)) or 3;
  YYear := XYear div D0;
  Temp := (((XYear - (YYear * D0) + 4) shr 2) * 5) - 3;
  TempMonth := Temp div 153;
  result.Day := ((Temp - (TempMonth * 153)) + 5) div 5;
  if TempMonth >= 10 then
  begin
    inc(YYear);
    dec(TempMonth, 12 - 3);
  end
  else
    inc(TempMonth, 3);
  result.Month := TempMonth;
  result.Year := YYear + (JulianDN * 100);
  // initialize fake dayOfWeek - as used by FromGlobalTime RCU128
  result.DayOfWeek := 0;
end;

procedure EpochToSystemTime(epoch: PtrUInt; out result: TSystemTime);
var
  t: PtrUInt;
begin
  t := epoch div SecsPerDay;
  JulianToGregorian(t + C1970, result);
  dec(epoch, t * SecsPerDay);
  t := epoch div SecsPerHour;
  result.Hour := t;
  dec(epoch, t * SecsPerHour);
  t := epoch div SecsPerMin;
  result.Minute := t;
  result.Second := epoch - t * SecsPerMin;
end;

{$ifdef DARWIN} // OSX has no clock_gettime() API

type
  TTimebaseInfoData = record
    Numer: cardinal;
    Denom: cardinal;
  end;

function mach_absolute_time: UInt64;
  cdecl external 'libc.dylib' name 'mach_absolute_time';

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Integer;
  cdecl external 'libc.dylib' name 'mach_timebase_info';

var
  mach_timeinfo: TTimebaseInfoData;
  mach_timecoeff: double;
  mach_timenanosecond: boolean; // very likely to be TRUE on Intel CPUs

procedure QueryPerformanceCounter(out Value: Int64);
begin // returns time in nano second resolution
  Value := mach_absolute_time;
  if mach_timeinfo.Denom = 1 then
    if mach_timeinfo.Numer = 1 then
      // seems to be the case on Intel CPUs
      exit
    else
      Value := Value * mach_timeinfo.Numer
  else
    // use floating point to avoid potential overflow
    Value := round(Value * mach_timecoeff);
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
begin
  if mach_timenanosecond then
    Value := mach_absolute_time div THOUSAND
  else
  begin
    QueryPerformanceCounter(Value);
    Value := Value div THOUSAND; // ns to us
  end;
end;

function GetTickCount64: Int64;
begin
  if mach_timenanosecond then
    result := mach_absolute_time
  else
    QueryPerformanceCounter(result);
  result := result div MILLION; // ns to ms
end;

function UnixTimeUTC: Int64;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  result := tz.tv_sec;
end;

function UnixMSTimeUTC: Int64;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  result := (tz.tv_sec * THOUSAND) + tz.tv_usec div THOUSAND; // in milliseconds
end;

procedure GetSystemTime(out result: TSystemTime);
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  EpochToSystemTime(tz.tv_sec, result);
  result.MilliSecond := tz.tv_usec div THOUSAND;
end;

{$else}

{$ifdef BSD}

const // see https://github.com/freebsd/freebsd/blob/master/sys/sys/time.h
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 4;
  CLOCK_REALTIME_COARSE = 10; // named CLOCK_REALTIME_FAST in FreeBSD 8.1+
  CLOCK_MONOTONIC_COARSE = 12;

function clock_gettime(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_gettime';

function clock_getres(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_getres';

{$else}

const
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 1;
  CLOCK_REALTIME_COARSE = 5; // see http://lwn.net/Articles/347811
  CLOCK_MONOTONIC_COARSE = 6;

// libc's clock_gettime function uses vDSO (avoid syscall) while FPC by default
// is compiled without FPC_USE_LIBC defined and do a syscall each time
//   GetTickCount64 fpc    2 494 563 op/sec
//   GetTickCount64 libc 119 919 893 op/sec
function clock_gettime(clk_id : clockid_t; tp: ptimespec) : cint;
  cdecl; external name 'clock_gettime';

{$endif BSD}

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_FAST: integer = CLOCK_REALTIME;

  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_FAST: integer = CLOCK_MONOTONIC;

function UnixMSTimeUTC: Int64;
var
  r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST,@r); // likely = CLOCK_REALTIME_COARSE
  result := (Int64(r.tv_sec) * THOUSAND) + (r.tv_nsec div MILLION); // in ms
end;

function UnixTimeUTC: Int64;
var
  r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST, @r);
  result := r.tv_sec;
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @r);
  value := PtrUInt(r.tv_nsec) div THOUSAND + r.tv_sec * MILLION; // as microseconds
end;

procedure GetSystemTime(out result: TSystemTime);
var
  r: timespec;
begin
  // faster than fpgettimeofday() which makes a syscall and don't use vDSO
  clock_gettime(CLOCK_REALTIME_FAST, @r);
  EpochToSystemTime(r.tv_sec, result);
  result.MilliSecond := r.tv_nsec div MILLION;
end;

function GetTickCount64: Int64;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_FAST, @tp); // likely = CLOCK_MONOTONIC_COARSE
  Result := (Int64(tp.tv_sec) * THOUSAND) + (tp.tv_nsec div MILLION); // in ms
end;

{$endif DARWIN}

function UnixMSTimeUTCFast: Int64;
begin
  result := UnixMSTimeUTC;
end;

const
  faInvalidFile = faDirectory;
  faDirectoryMask = faDirectory;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  Age: integer;
begin
  Age := FileAge(FileName);
  if Age <> -1 then
    result := FileDateToDateTime(Age)
  else
    result := 0;
end;

function FileSize(const FileName: TFileName): Int64;
var
  f: THandle;
begin
  f := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if PtrInt(f) > 0 then
  begin
    result := FileSize(f);
    FileClose(f);
  end
  else
    result := 0;
end;

function FileSize(F: THandle): Int64;
var
  FileInfo: TStat;
begin
  if fpFstat(F, FileInfo) <> 0 then
    result := 0
  else
    result := FileInfo.st_size;
end;

function FileSeek64(Handle: THandle; const Offset: Int64; Origin: DWORD): Int64;
begin
  result := FPLSeek(Handle, Offset, Origin);
end;

function FileInfoByHandle(aFileHandle: THandle;
  out FileId, FileSize, LastWriteAccess, FileCreateDateTime: Int64): Boolean;
var
  lastreadaccess: Int64;
  lp: stat;
  r: integer;
begin
  r := FpFStat(aFileHandle, lp);
  result := r >= 0;
  if not result then
    exit;
  FileId := lp.st_ino;
  FileSize := lp.st_size;
  lastreadaccess := lp.st_atime * MSecsPerSec;
  LastWriteAccess := lp.st_mtime * MSecsPerSec;
  {$ifdef OPENBSD}
  if (lp.st_birthtime <> 0) and (lp.st_birthtime < lp.st_ctime) then
    lp.st_ctime := lp.st_birthtime;
  {$endif OPENBSD}
  FileCreateDateTime := lp.st_ctime * MSecsPerSec;
  if LastWriteAccess <> 0 then
    if (FileCreateDateTime = 0) or (FileCreateDateTime > LastWriteAccess) then
      FileCreateDateTime := LastWriteAccess;
  if lastreadaccess <> 0 then
    if (FileCreateDateTime = 0) or (FileCreateDateTime > lastreadaccess) then
      FileCreateDateTime := lastreadaccess;
end;

function FileOpenSequentialRead(const FileName: string): Integer;
begin // SysUtils.FileOpen = fpOpen + fpFlock
  result := fpOpen(pointer(FileName), O_RDONLY); // no fpFlock() call
end;

procedure SetEndOfFile(hFile: cInt);
begin
  FpFtruncate(hFile, FPLseek(hFile, 0, SEEK_CUR));
end;

procedure FlushFileBuffers(hFile: cInt);
begin
  FpFsync(hFile);
end;

procedure SleepHiRes(ms: cardinal);
var
  timeout: TTimespec;
begin
  if ms = 0 then // handle SleepHiRes(0) special case
    if SleepHiRes0Yield then
    begin // reported as buggy by Alan on POSIX
      ThreadSwitch; // call e.g. pthread's sched_yield API
      exit;
    end
    else
    begin
      timeout.tv_sec := 0;
      timeout.tv_nsec := 10000; // 10us is around timer resolution on modern HW
    end
  else
  begin
    timeout.tv_sec := ms div 1000;
    timeout.tv_nsec := 1000000 * (ms mod 1000);
  end;
  fpnanosleep(@timeout, nil)
  // no retry loop on ESysEINTR (as with regular RTL's Sleep)
end;

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  {$ifdef LINUXNOTBSD}
  result := cs.__m_kind <> 0;
  {$else}
  result := not IsZero(@cs, SizeOf(cs));
  {$endif LINUXNOTBSD}
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  DoneCriticalSection(cs);
end;

{$ifdef BSD}

function fpsysctlhwint(hwid: cint): Int64;
var mib: array[0..1] of cint;
    len: cint;
begin
  result := 0;
  mib[0] := CTL_HW;
  mib[1] := hwid;
  len := SizeOf(result);
  fpsysctl(pointer(@mib), 2, @result, @len, nil, 0);
end;

function fpsysctlhwstr(hwid: cint; var temp: shortstring): pointer;
var mib: array[0..1] of cint;
    len: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := hwid;
  FillCharFast(temp, SizeOf(temp), 0); // shortstring as 0-terminated buffer
  len := SizeOf(temp);
  fpsysctl(pointer(@mib), 2, @temp, @len, nil, 0);
  if temp[0] <> #0 then
    result := @temp
  else
    result := nil;
end;

{$endif BSD}

// define some raw functions, to avoid linking mormot.core.text

function IdemPChar(p, up: PUTF8Char): boolean;
var
  c, u: AnsiChar;
begin
  result := false;
  if (p = nil) or (up = nil) then
    exit;
  repeat
    u := up^;
    if u = #0 then
      break;
    inc(up);
    c := p^;
    inc(p);
    if (c >= 'a') and (c <= 'z') then
      dec(c, 32);
    if c <> u then
      exit;
  until false;
  result := true;
end;

procedure FindNameValue(const s, up: RawUTF8; var res: RawUTF8);
var
  p: PUTF8Char;
  L: PtrInt;
begin
  p := pointer(s);
  while p <> nil do
  begin
    if IdemPChar(p, pointer(up)) then
    begin
      inc(p, length(up));
      while (p^ <= ' ') and (p^ <> #0) do
        inc(p);
      L := 0;
      while p[L] > #13 do
        inc(L);
      while p[L - 1] = ' ' do
        dec(L);
      FastSetString(res, p, L);
      exit;
    end;
    p := GotoNextLine(p);
  end;
  res := '';
end;

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


// we bypass crt.pp since this unit cancels the SIGINT signal

procedure AllocConsole;
begin // nothing to do on Linux
end;

{$I-}

var
  TextAttr: integer = ord(ccDarkGray);

procedure TextColor(Color: TConsoleColor);
const
  AnsiTbl: string[8] = '04261537';
begin
{$ifdef LINUX}
  if not stdoutIsTTY then
    exit;
{$endif LINUX}
  if ord(Color) = TextAttr then
    exit;
  TextAttr := ord(Color);
  if ord(Color) >= 8 then
    write(#27'[1;3', AnsiTbl[(ord(Color) and 7) + 1], 'm')
  else
    write(#27'[0;3', AnsiTbl[(ord(Color) and 7) + 1], 'm');
  ioresult;
end;

procedure TextBackground(Color: TConsoleColor);
begin // not implemented yet - but not needed either
end;

function UnixKeyPending: boolean;
var
  fdsin: tfdSet;
begin
  fpFD_ZERO(fdsin);
  fpFD_SET(StdInputHandle, fdsin);
  result := fpSelect(StdInputHandle + 1, @fdsin, nil, nil, 0) > 0;
end;

procedure ConsoleWaitForEnterKey;
var
  c: AnsiChar;
begin
  if IsMultiThread and (GetCurrentThreadID = MainThreadID) then
    repeat
      CheckSynchronize(100);
      if UnixKeyPending then
        repeat
          c := #0;
          if FpRead(StdInputHandle, c, 1) <> 1 then
            break;
          if c in [#10, #13] then
            exit;
        until false;
    until false
  else
    ReadLn;
  ioresult;
end;

function Utf8ToConsole(const S: RawUTF8): RawByteString;
begin
  result := S; // expect a UTF-8 console under Linux/BSD
end;

{$I+}

constructor TFileVersion.Create(const aFileName: TFileName;
  aMajor, aMinor, aRelease, aBuild: integer);
var
  M, D: word;
{$ifdef FPCUSEVERSIONINFO}
  VI: TVersionInfo;
  TI, I: Integer;
{$endif FPCUSEVERSIONINFO}
begin
  fFileName := aFileName;
  {$ifdef FPCUSEVERSIONINFO} // FPC 3.0+ if enabled in Synopse.inc / project options
  if aFileName <> '' then
  begin
    VI := TVersionInfo.Create;
    try
      if (aFileName <> '') and (aFileName <> ParamStr(0)) then
        VI.Load(aFileName)
      else
        VI.Load(HInstance); // load info for currently running program
      aMajor := VI.FixedInfo.FileVersion[0];
      aMinor := VI.FixedInfo.FileVersion[1];
      aRelease := VI.FixedInfo.FileVersion[2];
      aBuild := VI.FixedInfo.FileVersion[3];
      // detect translation
      if VI.VarFileInfo.Count > 0 then
        with VI.VarFileInfo.Items[0] do
          LanguageInfo := Format('%.4x%.4x', [language, codepage]);
      if LanguageInfo = '' then
      begin
        // take first language
        TI := 0;
        if VI.StringFileInfo.Count > 0 then
          LanguageInfo := VI.StringFileInfo.Items[0].Name
      end
      else
      begin
        // look for index of language
        TI := VI.StringFileInfo.Count - 1;
        while (TI >= 0) and
              (CompareText(VI.StringFileInfo.Items[TI].Name, LanguageInfo) <> 0) do
          dec(TI);
        if TI < 0 then
        begin
          TI := 0; // revert to first translation
          LanguageInfo := VI.StringFileInfo.Items[TI].Name;
        end;
      end;
      with VI.StringFileInfo.Items[TI] do
      begin
        CompanyName := Values['CompanyName'];
        FileDescription := Values['FileDescription'];
        FileVersion := Values['FileVersion'];
        InternalName := Values['InternalName'];
        LegalCopyright := Values['LegalCopyright'];
        OriginalFilename := Values['OriginalFilename'];
        ProductName := Values['ProductName'];
        ProductVersion := Values['ProductVersion'];
        Comments := Values['Comments'];
      end;
    finally
      VI.Free;
    end;
  end;
  {$endif FPCUSEVERSIONINFO}
  SetVersion(aMajor, aMinor, aRelease, aBuild);
  if fBuildDateTime = 0 then // get build date from file age
    fBuildDateTime := FileAgeToDateTime(aFileName);
  if fBuildDateTime <> 0 then
    DecodeDate(fBuildDateTime, BuildYear, M, D);
end;

procedure GetUserHost(out User, Host: RawUTF8);
begin
  Host := RawUTF8(GetHostName);
  if Host = '' then
    Host := RawUTF8(GetEnvironmentVariable('HOSTNAME'));
  User := RawUTF8(GetEnvironmentVariable('LOGNAME')); // POSIX
  if User = '' then
    User := RawUTF8(GetEnvironmentVariable('USER'));
end;

var
  _HomePath, _TempPath, _UserPath, _LogPath: TFileName;

function GetSystemPath(kind: TSystemPath): TFileName;
begin
  case kind of
    spLog:
      begin
        if _LogPath = '' then
          if IsDirectoryWritable('/var/log') then
            _LogPath := '/var/log/'
          else // may not be writable by not root on POSIX
          if IsDirectoryWritable(ExeVersion.ProgramFilePath) then
            _LogPath := ExeVersion.ProgramFilePath
          else
            _LogPath := GetSystemPath(spUserData);
        result := _LogPath;
      end;
    spUserData:
      begin
        if _UserPath = '' then
        begin //  ~/.cache/appname
          _UserPath := GetEnvironmentVariable('XDG_CACHE_HOME');
          if (_UserPath = '') or not IsDirectoryWritable(_UserPath) then
            _UserPath := EnsureDirectoryExists(GetSystemPath(spUserDocuments) + '.cache');
          _UserPath := EnsureDirectoryExists(_UserPath + TFileName(ExeVersion.ProgramName));
        end;
        result := _UserPath;
      end;
    spTempFolder:
      begin
        if _TempPath = '' then
        begin
          _TempPath := GetEnvironmentVariable('TMPDIR'); // POSIX
          if _TempPath = '' then
            _TempPath := GetEnvironmentVariable('TMP');
          if _TempPath = '' then
            if DirectoryExists('/tmp') then
              _TempPath := '/tmp'
            else
              _TempPath := '/var/tmp';
          _TempPath := IncludeTrailingPathDelimiter(_TempPath);
        end;
        result := _TempPath;
      end
  else
    begin
      if _HomePath = '' then // POSIX requires a value for $HOME
        _HomePath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
      result := _HomePath;
    end;
  end;
end;

{$ifdef BSD}
// https://kaashif.co.uk/2015/06/18/how-to-get-a-list-of-processes-on-openbsd-in-c/

function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;
begin
  result := nil;
end;

function EnumProcessName(PID: Cardinal): RawUTF8;
begin
  result := '';
end;

{$else}

const
  // function DirectoryFileNames(const Folder: TFileName): TFileNameDynArray; ?
  // function DirectoryFolderNames(const Folder: TFileName): TFileNameDynArray; ?
  DT_UNKNOWN  = 0; // need to call fpstat() if returned this
  DT_FIFO     = 1;
  DT_CHR      = 2;
  DT_DIR      = 4;
  DT_BLK      = 6;
  DT_REG      = 8;
  DT_LNK      = 10;
  DT_SOCK     = 12;
  DT_WHT      = 14;

function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;
var
  d: pDir;
  e: pDirent;
  n: integer;
  pid: cardinal;
  fn, status, tgid: RawUTF8;
begin
  result := nil;
  d := FpOpendir('/proc'); // alternative to FindFirst()
  if d = nil then
    exit;
  n := 0;
  SetLength(result, 64);
  repeat
    e := FpReaddir(d^);
    if e = nil then
      break;
    if (e.d_type <> DT_UNKNOWN) and (e.d_type <> DT_DIR) and
       (e.d_name[0] in ['1'..'9']) then
      continue;
    pid := GetCardinal(@e.d_name[0]);
    if pid = 0 then
      continue;
    fn := e.d_name;
    status := StringFromFile('/proc/' + fn + '/status', {nosize=}true);
    FindNameValue(status, 'TGID:', tgid);
    if GetCardinal(pointer(tgid)) = pid then
      // ensure is a real process, not a thread
      // https://www.memsql.com/blog/the-curious-case-of-thread-groups-identifiers
      AddInteger(TIntegerDynArray(result), n, pid);
  until false;
  FpClosedir(d^);
  SetLength(result, n);
end;

var
  tryprocexe: boolean = true;

function EnumProcessName(PID: Cardinal): RawUTF8;
var
  proc: TFileName;
  cmdline: RawUTF8;
begin
  proc := '/proc/' + IntToStr(PID);
  if tryprocexe then
  begin // need to be root to follow /proc/[pid]/exe
    result := fpReadLink(proc + '/exe');
    if result <> '' then
      exit;
  end;
  cmdline := StringFromFile(proc + '/cmdline', {nosize=}true);
  // set of strings separated by null bytes -> exe is the first argument
  FastSetString(result, pointer(cmdline), StrLen(pointer(cmdline)));
  if result <> '' then
    tryprocexe := false; // no need to try again next time
end;

{$endif BSD}

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
begin
  result := false;
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
begin
  result := false;
end;

{$ifdef BSD}
function mprotect(Addr: Pointer; Len: size_t; Prot: Integer): Integer;
  {$ifdef Darwin} cdecl external 'libc.dylib' name 'mprotect';
  {$else} cdecl external 'libc.so' name 'mprotect'; {$endif}
{$endif BSD}

function SynMProtect(addr: pointer; size: size_t; prot: integer): longint;
begin
  result := -1;
  {$ifdef UNIX}
    {$ifdef BSD}
    result := mprotect(addr, size, prot);
    {$else}
    if Do_SysCall(syscall_nr_mprotect, PtrUInt(addr), size, prot) >= 0 then
      result := 0;
    {$endif BSD}
  {$endif UNIX}
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  PageSize: PtrUInt;
  AlignedAddr: pointer;
  i: PtrInt;
  ProtectedResult: boolean;
  ProtectedMemory: boolean;
begin
  if Backup <> nil then
    for i := 0 to Size - 1 do // do not use Move() here
      PByteArray(Backup)^[i] := PByteArray(Old)^[i];
  PageSize := SystemInfo.dwPageSize;
  AlignedAddr := Pointer((PtrUInt(Old) div SystemInfo.dwPageSize) * SystemInfo.dwPageSize);
  while PtrUInt(Old) + PtrUInt(Size) >= PtrUInt(AlignedAddr) + PageSize do
    Inc(PageSize, SystemInfo.dwPageSize);
  ProtectedResult := SynMProtect(AlignedAddr, PageSize, PROT_READ or PROT_WRITE or PROT_EXEC) = 0;
  ProtectedMemory := not ProtectedResult;
  if ProtectedMemory then
    ProtectedResult := SynMProtect(AlignedAddr, PageSize, PROT_READ or PROT_WRITE) = 0;
  if ProtectedResult then
  try
    for i := 0 to Size - 1 do // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    if not LeaveUnprotected and ProtectedMemory then
      SynMProtect(AlignedAddr, PageSize, PROT_READ or PROT_EXEC);
  except
  end;
end;


{ ****************** Gather Operating System Information }

function getpagesize: Integer; cdecl; external 'c';

{$ifndef BSD}

procedure SetLinuxDistrib(const release: RawUTF8);
var
  distrib: TOperatingSystem;
  rel, dist: RawUTF8;
begin
  rel := UpperCase(release);
  for distrib := osArch to high(distrib) do
  begin
    dist := UpperCase(OS_NAME[distrib]);
    if PosEx(dist, rel) > 0 then
    begin
      OS_KIND := distrib;
      break;
    end;
  end;
end;

{$endif BSD}

procedure InitializeUnit;
var
  tp: timespec;
  P: PAnsiChar;
var
  modname, beg: PUTF8Char;
  uts: UtsName;
  {$ifdef BSD}
  temp: shortstring;
  {$else}
  cpuinfo: PUTF8Char;
  proccpuinfo, prod, prodver, release, dist: RawUTF8;
  SR: TSearchRec;
  {$endif BSD}
  c: cardinal;
begin
  EnumAllProcesses(c);
  // retrieve Kernel and Hardware information
  stdoutIsTTY := IsATTY(StdOutputHandle) = 1;
  modname := nil;
  SystemInfo.dwPageSize := getpagesize;
  fpuname(uts);
  {$ifdef BSD}
  SystemInfo.dwNumberOfProcessors := fpsysctlhwint(HW_NCPU);
  beg := fpsysctlhwstr(HW_MACHINE, temp);
  FastSetString(BiosInfoText, beg, StrLen(beg));
  modname := fpsysctlhwstr(HW_MODEL, temp);
  with uts do
    OSVersionText := sysname + '-' + release + ' ' + version;
  {$else}
  prod := Trim(StringFromFile('/sys/class/dmi/id/product_name', true));
  if prod <> '' then
  begin
    prodver := Trim(StringFromFile('/sys/class/dmi/id/product_version', true));
    if prodver <> '' then
      prod := prod + ' ' + prodver;
  end;
  BiosInfoText := prod;
  SystemInfo.dwNumberOfProcessors := 0;
  proccpuinfo := StringFromFile('/proc/cpuinfo', true);
  cpuinfo := pointer(proccpuinfo);
  while cpuinfo <> nil do
  begin
    beg := cpuinfo;
    cpuinfo := GotoNextLine(cpuinfo);
    if IdemPChar(beg, 'PROCESSOR') then
      if beg^ = 'P' then
        modname := beg
      else // Processor : ARMv7
        inc(SystemInfo.dwNumberOfProcessors)
    else // processor : 0
    if IdemPChar(beg, 'MODEL NAME') then
      modname := beg;
  end;
  if modname <> nil then
    modname := strscan(modname, ':');
  if modname <> nil then
    repeat
      inc(modname);
    until (modname^ = #0) or (modname^ > ' ');
  FindNameValue(StringFromFile('/etc/os-release'), 'PRETTY_NAME=', release);
  if (release <> '') and (release[1] = '"') then
    release := copy(release, 2, length(release) - 2);
  release := trim(release);
  if release = '' then
  begin
    FindNameValue(StringFromFile('/etc/lsb-release'), 'DISTRIB_DESCRIPTION=', release);
    if (release <> '') and (release[1] = '"') then
      release := copy(release, 2, length(release) - 2);
  end;
  if (release = '') and (FindFirst('/etc/*-release', faAnyFile, SR) = 0) then
  begin
    release := SR.Name; // 'redhat-release' 'SuSE-release'
    if IdemPChar(pointer(release), 'LSB-') and (FindNext(SR) = 0) then
      release := SR.Name;
    release := split(release, '-');
    dist := split(trim(StringFromFile('/etc/' + SR.Name)), #10);
    if (dist <> '') and (PosExChar('=', dist) = 0) and (PosExChar(' ', dist) > 0) then
      SetLinuxDistrib(dist) // e.g. 'Red Hat Enterprise Linux Server release 6.7 (Santiago)'
    else
      dist := '';
    FindClose(SR);
  end;
  if (release <> '') and (OS_KIND = osLinux) then
  begin
    SetLinuxDistrib(release);
    if (OS_KIND = osLinux) and ({%H-}dist <> '') then
    begin
      SetLinuxDistrib(dist);
      release := dist;
    end;
    if (OS_KIND = osLinux) and ((PosEx('RH', release) > 0) or (PosEx('Red Hat', release) > 0)) then
      OS_KIND := osRedHat;
  end;
  SystemInfo.release := release;
  {$endif BSD}
  SystemInfo.uts.release := uts.Release;
  SystemInfo.uts.sysname := uts.Sysname;
  SystemInfo.uts.version := uts.Version;
  P := @uts.release[0];
  KernelRevision := GetNextCardinal(P) shl 16 + GetNextCardinal(P) shl 8 +
    GetNextCardinal(P);
  OSVersionInt32 := integer(KernelRevision shl 8) + ord(OS_KIND);
  with SystemInfo.uts do
    OSVersionText := sysname + ' ' + release;
  if SystemInfo.release <> '' then
    OSVersionText := SystemInfo.release + ' - ' + OSVersionText;
  {$ifdef Android}
  OSVersionText := 'Android (' + OSVersionText + ')';
  {$endif}
  if (SystemInfo.dwNumberOfProcessors > 0) and (modname <> nil) then
  begin
    beg := modname;
    while not (ord(modname^) in [0, 10, 13]) do
    begin
      if modname^ < ' ' then
        modname^ := ' ';
      inc(modname);
    end;
    modname^ := #0;
    CpuInfoText := Format('%d x %s (' + CPU_ARCH_TEXT + ')',
      [SystemInfo.dwNumberOfProcessors, beg]);
  end;
  if CpuInfoText = '' then
    CpuInfoText := CPU_ARCH_TEXT;
  // intialize supported APIs
  {$ifdef ISFPC27}
  GetACP := GetSystemCodePage;
  {$endif ISFPC27}
  {$ifdef DARWIN}
  mach_timebase_info(mach_timeinfo);
  mach_timecoeff := mach_timeinfo.Numer / mach_timeinfo.Denom;
  mach_timenanosecond := (mach_timeinfo.Numer = 1) and (mach_timeinfo.Denom = 1);
  {$else}
  {$ifdef LINUX}
  // try Linux kernel 2.6.32+ or FreeBSD 8.1+ fastest clocks
  if clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0 then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0 then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  if (clock_gettime(CLOCK_REALTIME_FAST, @tp) <> 0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST, @tp) <> 0) then
    raise Exception.CreateFmt('clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release), fpgeterrno]);
  {$endif LINUX}
  {$endif DARWIN}
end;


