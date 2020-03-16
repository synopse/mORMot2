{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   POSIX API calls for FPC, as used by mormot.core.os.pas
}

uses
  mormot.core.text,
  mormot.core.data,
  baseunix,
  unix,
  unixcp,
  unixtype,
  {$ifdef BSD}
  sysctl,
  {$else}
  linux,
  {$endif BSD}
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

function os_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
var
  tmp: UnicodeString;
begin
  widestringmanager.Ansi2UnicodeMoveProc(A, {$ifdef ISFPC27}CodePage, {$endif} tmp, LA);
  result := length(tmp);
  MoveFast(Pointer(tmp)^, W^, result * 2);
end;

function os_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
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
  fpgettimeofday(@tz,nil);
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
  result := fpOpen(pointer(FileName), O_RDONLY);
end;

function SetFilePointer(hFile: cInt; lDistanceToMove: TOff;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff;
var
  offs: Int64;
begin
  Int64Rec(offs).Lo := lDistanceToMove;
  if lpDistanceToMoveHigh = nil then
    Int64Rec(offs).Hi := 0
  else
    Int64Rec(offs).Hi := PDWord(lpDistanceToMoveHigh)^;
  offs := FpLseek(hFile, offs, dwMoveMethod);
  result := Int64Rec(offs).Lo;
  if lpDistanceToMoveHigh <> nil then
    PDWord(lpDistanceToMoveHigh)^ := Int64Rec(offs).Hi;
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
  FillChar(temp, SizeOf(temp), 0); // use shortstring as temp 0-terminated buffer
  len := SizeOf(temp);
  fpsysctl(pointer(@mib), 2, @temp, @len, nil, 0);
  if temp[0] <> #0 then
    result := @temp
  else
    result := nil;
end;

{$else}

procedure SetLinuxDistrib(const release: RawUTF8);
var
  distrib: TOperatingSystem;
  dist: RawUTF8;
begin
  for distrib := osArch to high(distrib) do
  begin
    dist := UpperCase(OS_NAME[distrib]);
    if PosI(pointer(dist), release) > 0 then
    begin
      OS_KIND := distrib;
      break;
    end;
  end;
end;

{$endif BSD}


{ ****************** Gather Operating System Information }

function getpagesize: Integer; cdecl; external 'c';

procedure InitializeUnit;
var
  tp: timespec;
  P: PAnsiChar;

  function GetNext: cardinal;
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
    if P^ in ['.', '-', ' '] then
      inc(P);
  end;

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
begin
  // initialize mormot.core.text to avoid GPF in this function
  InitializeTextConstants;
  // retrieve Kernel and Hardware information
  modname := nil;
  SystemInfo.dwPageSize := getpagesize;
  fpuname(uts);
  {$ifdef BSD}
  SystemInfo.dwNumberOfProcessors := fpsysctlhwint(HW_NCPU);
  Utf8ToRawUTF8(fpsysctlhwstr(HW_MACHINE, temp), BiosInfoText);
  modname := fpsysctlhwstr(HW_MODEL, temp);
  with uts do
    FormatUTF8('%-% %', [sysname, release, version], OSVersionText);
  {$else}
  prod := Trim(StringFromFile('/sys/class/dmi/id/product_name', true));
  if prod <> '' then
  begin
    prodver := Trim(StringFromFile('/sys/class/dmi/id/product_version', true));
    if prodver <> '' then
      FormatUTF8('% %', [prod, prodver], BiosInfoText)
    else
      BiosInfoText := prod;
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
  modname := PosChar(modname, ':');
  if modname <> nil then
    modname := GotoNextNotSpace(modname + 1);
  release := trim(FindIniNameValue(pointer(StringFromFile('/etc/os-release')), 'PRETTY_NAME='));
  if (release <> '') and (release[1] = '"') then
    release := copy(release, 2, length(release) - 2);
  release := trim(release);
  if release = '' then
  begin
    release := trim(FindIniNameValue(pointer(StringFromFile('/etc/lsb-release')), 'DISTRIB_DESCRIPTION='));
    if (release <> '') and (release[1] = '"') then
      release := copy(release, 2, length(release) - 2);
  end;
  if (release = '') and (FindFirst('/etc/*-release', faAnyFile, SR) = 0) then
  begin
    release := StringToUTF8(SR.Name); // 'redhat-release' 'SuSE-release'
    if IdemPChar(pointer(release), 'LSB-') and (FindNext(SR) = 0) then
      release := StringToUTF8(SR.Name);
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
    if (OS_KIND = osLinux) and (dist <> '') then
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
  KernelRevision := GetNext shl 16 + GetNext shl 8 + GetNext;
  OSVersionInt32 := integer(KernelRevision shl 8) + ord(OS_KIND);
  with SystemInfo.uts do
    FormatUTF8('% %', [sysname, release], OSVersionText);
  if SystemInfo.release <> '' then
    OSVersionText := FormatUTF8('% - %', [SystemInfo.release, OSVersionText]);
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
    FormatUTF8('% x % (' + CPU_ARCH_TEXT + ')', [SystemInfo.dwNumberOfProcessors, beg], CpuInfoText);
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


