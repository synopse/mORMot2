{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   POSIX API calls for FPC, as used by mormot.core.os.pas
}

uses
  baseunix,
  unix,
  unixcp,
  unixtype,
  {$ifdef OSBSDDARWIN}
  sysctl,
  {$else}
  linux,
  syscall,
  {$endif OSBSDDARWIN}
  {$ifdef FPCUSEVERSIONINFO} // to be enabled in mormot.defines.inc
    fileinfo,        // FPC 3.0 and up
    {$ifdef OSDARWIN}
      machoreader,   // MACH-O executables
    {$else}
      elfreader,     // ELF executables
    {$endif OSDARWIN}
  {$endif FPCUSEVERSIONINFO}
  errors,
  termio,
  dl,
  initc; // we statically link the libc for some raw calls

{$ifdef UNICODE}
  'mORMot assumes no UNICODE on POSIX, i.e. as TFileName = PChar = PUtf8Char'
{$endif UNICODE}


// define some raw text functions, to avoid linking mormot.core.text

function IdemPChar(p, up: PUtf8Char): boolean; inline;
var
  c, u: AnsiChar;
begin
  // we know that p<>nil and up<>nil within this unit
  result := false;
  repeat
    u := up^;
    if u = #0 then
      break;
    inc(up);
    c := p^;
    inc(p);
    if c <> u then
      if (c >= 'a') and
         (c <= 'z') then
      begin
        dec(c, 32);
        if c <> u then
          exit;
      end
      else
        exit;
  until false;
  result := true;
end;

function IdemPChars(const s: RawUtf8; const up: array of PUtf8Char): boolean;
var
  i: PtrInt;
begin
  if s <> '' then
  begin
    result := true;
    for i := 0 to high(up) do
      if IdemPChar(pointer(s), up[i]) then
        exit;
  end;
  result := false;
end;

procedure FindNameValue(const s, up: RawUtf8; var res: RawUtf8);
var
  p: PUtf8Char;
  L: PtrInt;
begin
  p := pointer(s);
  while p <> nil do
  begin
    if IdemPChar(p, pointer(up)) then
    begin
      inc(p, length(up));
      while (p^ <= ' ') and
            (p^ <> #0) do
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

function GetNextCardinal(var P: PAnsiChar): PtrUInt;
var
  c: cardinal;
begin
  result := 0;
  while not (P^ in ['0'..'9']) do
    if P^ = #0 then
      exit
    else
      inc(P);
  repeat
    c := ord(P^) - 48;
    if c > 9 then
      break;
    result := result * 10 + c;
    inc(P);
  until false;
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


{ ****************** Unicode, Time, File process }
var
  /// contains the current Linux kernel revision, as one 24-bit integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;

function LibraryOpen(const LibraryName: TFileName): TLibHandle;
begin
  result := TLibHandle(dlopen(pointer(LibraryName), RTLD_LAZY));
end;

procedure LibraryClose(Lib: TLibHandle);
begin
  dlclose(Lib);
end;

function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
begin
  result := dlsym(Lib, ProcName);
end;


{ TIcuLibrary }

procedure TIcuLibrary.Done;
begin
  if icui18n <> nil then
    dlclose(icui18n);
  if icu <> nil then
    dlclose(icu);
  if icudata <> nil then
    dlclose(icudata);
  icu := nil;
  icudata := nil;
  icui18n := nil;
  @ucnv_open := nil;
end;

function TIcuLibrary.IsAvailable: boolean;
begin
  if not Loaded then
    DoLoad;
  result := Assigned(ucnv_open);
end;

function IsWideStringManagerProperlyInstalled: boolean;
const
  u: WideChar = #$2020; // should convert to dagger glyph #134 in CP 1252
var
  d: RawByteString;
begin
  try
    widestringmanager.Unicode2AnsiMoveProc(@u, d, 1252, 1);
    result := (length(d) = 1) and
              (d[1] = #134);
    // the default RTL handler would just return '?'
  except
    result := false;
  end;
end;

procedure TIcuLibrary.DoLoad(const LibName: TFileName; const Version: string);
const
  NAMES: array[0..12] of string = (
    'ucnv_open',
    'ucnv_close',
    'ucnv_setSubstChars',
    'ucnv_setFallback',
    'ucnv_fromUChars',
    'ucnv_toUChars',
    'u_strToUpper',
    'u_strToLower',
    'u_strCompare',
    'u_strCaseCompare',
    'u_getDataDirectory',
    'u_setDataDirectory',
    'u_init');
  {$ifdef OSANDROID}
  // from https://developer.android.com/guide/topics/resources/internationalization
  ICU_VER: array[1..13] of string = (
    '_3_8', '_4_2', '_44', '_46', '_48', '_50', '_51', '_53', '_55',
    '_56', '_58', '_60', '_63');
  SYSDATA: PAnsiChar = '/system/usr/icu';
  {$else}
  SYSDATA: PAnsiChar = '';
  {$endif OSANDROID}
var
  i: integer;
  err: SizeInt;
  P: PPointer;
  v, vers: string;
  data: PAnsiChar;
begin
  GlobalLock;
  try
    if Loaded then
      exit;
    Loaded := true;
    if LibName <> '' then
    begin
      icu := dlopen(pointer(LibName), RTLD_LAZY);
      if icu = nil then
        exit;
    end
    else
    begin
      {$ifdef OSDARWIN}
      // Mach OS has its own ICU set of libraries
      icu := dlopen('libicuuc.dylib', RTLD_LAZY);
      if icu <> nil then
        icui18n := dlopen('libicui18n.dylib', RTLD_LAZY);
      {$else}
      // libicudata should be loaded first because other two depend on it
      icudata := dlopen('libicudata.so', RTLD_LAZY);
      if icudata <> nil then
      begin
        icu := dlopen('libicuuc.so', RTLD_LAZY);
        if icu <> nil then
          icui18n := dlopen('libicui18n.so', RTLD_LAZY);
      end;
      {$endif OSDARWIN}
      if icui18n = nil then
      begin
        if not IsWideStringManagerProperlyInstalled then
          DisplayFatalError('ICU ' + CPU_ARCH_TEXT + ' is not available',
            'Either install it or put cwstring in your uses clause as fallback');
        Done;
        exit;
      end
    end;
    // ICU append a version prefix to all its functions e.g. ucnv_open_66
    if (Version <> '') and
       (dlsym(icu, pointer('ucnv_open_' + Version)) <> nil) then
      vers := '_' + Version
    else
    begin
      {$ifdef OSANDROID}
      for i := high(ICU_VER) downto 1 do
      begin
        if dlsym(icu, pointer(NAMES[0] + ICU_VER[i])) <> nil then
        begin
          vers := ICU_VER[i];
          break;
        end;
      end;
      if vers <> '' then
      {$endif OSANDROID}
      if dlsym(icu, 'ucnv_open') = nil then
        for i := 80 downto 44 do
        begin
          str(i, v);
          if dlsym(icu, pointer('ucnv_open_' + v)) <> nil then
          begin
            vers := '_' + v;
            break;
          end;
        end;
    end;
    P := @@ucnv_open;
    for i := 0 to high(NAMES) do
    begin
      P[i] := dlsym(icu, pointer(NAMES[i] + vers));
      if P[i] = nil then
      begin
        @ucnv_open := nil;
        exit;
      end;
    end;
    data := u_getDataDirectory;
    if (data = nil) or
       (data^ = #0) then
      if SYSDATA <> '' then
        u_setDataDirectory(SYSDATA);
    err := 0;
    u_init(err);
  finally
    GlobalUnLock;
  end;
end;

function TIcuLibrary.ForceLoad(const LibName: TFileName; const Version: string): boolean;
begin
  Done;
  Loaded := false;
  DoLoad(LibName, Version);
  result := Assigned(ucnv_open);
end;

function TIcuLibrary.ucnv(codepage: cardinal): pointer;
var
  s: ShortString;
  err: SizeInt;
  {$ifdef CPUINTEL}
  mask: cardinal;
  {$endif CPUINTEL}
begin
  if not IsAvailable then
    exit(nil);
  str(codepage, s);
  MoveFast(s[1], s[3], ord(s[0]));
  PWord(@s[1])^ := ord('c') + ord('p') shl 8;
  inc(s[0], 3);
  s[ord(s[0])] := #0;
  {$ifdef CPUINTEL}
  mask := GetMXCSR;
  SetMXCSR(mask or $0080 {MM_MaskInvalidOp} or $1000 {MM_MaskPrecision});
  {$endif CPUINTEL}
  err := 0;
  result := ucnv_open(@s[1], err);
  if result <> nil then
  begin
    err := 0;
    ucnv_setSubstChars(result, '?', 1, err);
    ucnv_setFallback(result, true);
  end;
  {$ifdef CPUINTEL}
  SetMXCSR(mask);
  {$endif CPUINTEL}
end;


const
  // for CompareStringW()
  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1 shl ord(coIgnoreCase); // [widestringmanager.coIgnoreCase]

function CompareStringRTL(A, B: PWideChar; AL, BL, flags: integer): integer;
var
  U1, U2: UnicodeString; // allocate two temporary strings
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  SetString(U1, A, AL);
  SetString(U2, B, BL);
  result := widestringmanager.CompareUnicodeStringProc(
    U1, U2, TCompareOptions(flags));
end;

function CompareStringW(locale, flags: DWORD;
  A: PWideChar; AL: integer; B: PWideChar; BL: integer): PtrInt;
const
  CODE_POINT_ORDER = $8000;
var
  err: SizeInt;
begin
  if AL < 0 then
    AL := StrLenW(A);
  if BL < 0 then
    BL := StrLenW(B);
  err := 0;
  if icu.IsAvailable then
    if flags and NORM_IGNORECASE <> 0 then
      result := icu.u_strCaseCompare(A, AL, B, BL, CODE_POINT_ORDER, err)
    else
      result := icu.u_strCompare(A, AL, B, BL, true)
  else
    result := CompareStringRTL(A, B, AL, BL, flags);
  inc(result, 2); // caller would make -2 to get regular -1/0/1 comparison values
end;

function AnsiToWideRTL(CodePage: cardinal; A: PAnsiChar; W: PWideChar;
  AL, WL: PtrInt): PtrInt;
var
  tmp: UnicodeString;
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  widestringmanager.Ansi2UnicodeMoveProc(A, CodePage, tmp, AL);
  result := length(tmp);
  if result > WL then
    result := WL;
  MoveFast(pointer(tmp)^, W^, result * 2);
end;

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar;
  LA, LW, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if CodePage = CP_UTF8 then
    exit(Utf8ToUnicode(W, A, LA));
  cnv := icu.ucnv(CodePage);
  if cnv = nil then
    exit(AnsiToWideRTL(CodePage, A, W, LA, LW)); // fallback to cwstring/iconv
  err := 0;
  result := icu.ucnv_toUChars(cnv, W, LW, A, LA, err);
  if result < 0 then
    result := 0;
  icu.ucnv_close(cnv);
end;

function WideToAnsiRTL(CodePage: cardinal; W: PWideChar; A: PAnsiChar;
  WL, AL: PtrInt): PtrInt;
var
  tmp: RawByteString;
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  widestringmanager.Unicode2AnsiMoveProc(W, tmp, CodePage, WL);
  result := length(tmp);
  if result > AL then
    result := AL;
  MoveFast(pointer(tmp)^, A^, result);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar;
  LW, LA, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if CodePage = CP_UTF8 then
    exit(UnicodeToUtf8(A, W, LW));
  cnv := icu.ucnv(CodePage);
  if cnv = nil then
    exit(WideToAnsiRTL(CodePage, W, A, LW, LA)); // fallback to cwstring/iconv
  err := 0;
  result := icu.ucnv_fromUChars(cnv, A, LA, W, LW, err);
  if result < 0 then
    result := 0;
  icu.ucnv_close(cnv);
end;

function Unicode_InPlaceUpper(W: PWideChar; WLen: integer): integer;
var
  err, i: SizeInt;
begin
  if icu.IsAvailable then
  begin
    // call the accurate ICU library
    err := 0;
    result := icu.u_strToUpper(W, WLen, W, WLen, nil, err);
  end
  else
  begin
    // simple fallback code only handling 'a'..'z' -> 'A'..'Z' basic conversion
    for i := 0 to WLen - 1 do
      if ord(W[i]) in [ord('a')..ord('z')] then
        dec(W[i], 32);
    result := WLen;
  end;
end;

function Unicode_InPlaceLower(W: PWideChar; WLen: integer): integer;
var
  err, i: SizeInt;
begin
  if icu.IsAvailable then
  begin
    // call the accurate ICU library
    err := 0;
    result := icu.u_strToLower(W, WLen, W, WLen, nil, err);
  end
  else
  begin
    // simple fallback code only handling 'A'..'Z' -> 'a'..'z' basic conversion
    for i := 0 to WLen - 1 do
      if ord(W[i]) in [ord('A')..ord('Z')] then
        inc(W[i], 32);
    result := WLen;
  end;
end;

function GetDesktopWindow: PtrInt;
begin
  result := 0; // fixed result on a window-abstracted system
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
  {$ifdef HASINLINE}inline;{$endif}
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
  // initialize fake dayOfWeek - as used by FromGlobalTime Rcu128
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

{$ifdef OSDARWIN} // OSX has no clock_gettime() API

type
  TTimebaseInfoData = record
    Numer: cardinal;
    Denom: cardinal;
  end;

function mach_absolute_time: UInt64;
  cdecl external clib name 'mach_absolute_time';

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): integer;
  cdecl external clib name 'mach_timebase_info';

var
  mach_timeinfo: TTimebaseInfoData;
  mach_timecoeff: double;
  mach_timenanosecond: boolean; // very likely to be TRUE on Intel CPUs

procedure QueryPerformanceNanosec(out Value: Int64);
begin
  // Win32-API like function which returns time in nano second resolution
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
    QueryPerformanceNanosec(Value);
    Value := Value div THOUSAND; // ns to us
  end;
end;

function GetTickCount64: Int64;
begin
  if mach_timenanosecond then
    result := mach_absolute_time
  else
    QueryPerformanceNanosec(result);
  result := result div MILLION; // ns to ms
end;

function UnixTimeUtc: TUnixTime;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  result := tz.tv_sec;
end;

function UnixMSTimeUtc: TUnixMSTime;
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


{$ifdef OSBSD}

const
  // see https://github.com/freebsd/freebsd/blob/master/sys/sys/time.h
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 4;
  CLOCK_REALTIME_COARSE = 10; // named CLOCK_REALTIME_FAST in FreeBSD 8.1+
  CLOCK_MONOTONIC_COARSE = 12;

function clock_gettime(clk_id: cardinal; tp: ptimespec): integer;
  cdecl external clib name 'clock_gettime';

function clock_getres(clk_id: cardinal; tp: ptimespec): integer;
  cdecl external clib name 'clock_getres';

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
function clock_gettime(clk_id: clockid_t; tp: ptimespec): cint;
  cdecl; external clib name 'clock_gettime';

function gettimeofday(tp: ptimeval; tzp: ptimezone): cint;
  cdecl; external clib name 'gettimeofday';

{$endif OSBSD}

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_FAST: integer = CLOCK_REALTIME;

  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_FAST: integer = CLOCK_MONOTONIC;

  // contains CLOCK_MONOTONIC_RAW since kernel 2.6.28
  // - so that QueryPerformanceMicroSeconds() is not subject to NTP adjustments
  CLOCK_MONOTONIC_HIRES: integer = CLOCK_MONOTONIC;


function UnixMSTimeUtc: TUnixMSTime;
var
  r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST, @r); // likely = CLOCK_REALTIME_COARSE
  // convert from nanoseconds into milliseconds
  result := PtrUInt(r.tv_nsec) div MILLION + (r.tv_sec * THOUSAND);
end;

function UnixTimeUtc: TUnixTime;
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
  clock_gettime(CLOCK_MONOTONIC_HIRES, @r);
  // convert from nanoseconds into microseconds
  Value := PtrUInt(r.tv_nsec) div THOUSAND + r.tv_sec * MILLION;
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
  // convert from nanoseconds into milliseconds
  result := PtrUInt(tp.tv_nsec) div MILLION + (tp.tv_sec * THOUSAND);
end;

{$endif OSDARWIN}

function UnixMSTimeUtcFast: TUnixMSTime;
begin
  result := UnixMSTimeUtc;
end;

{$ifdef OSPTHREADS}

var
  {%H-}pthread: pointer;
  pthread_setname_np: function(thread: pointer; name: PAnsiChar): integer; cdecl;

{$endif OSPTHREADS}

procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);
var
  // truncated to 16 non space chars (including #0)
  {%H-}trunc: array[0..15] of AnsiChar;
  i, L, c4: integer;
begin
  if Name = '' then
    exit;
  L := 0; // trim unrelevant spaces and prefixes when filling the 16 chars
  i := 1;
  if Name[1] = 'T' then
  begin
    c4 := PCardinal(Name)^ and $dfdfdfdf;
    if (c4 = ord('T') + ord('S') shl 8 + ord('Q') shl 16 + ord('L') shl 24) or
       (c4 = ord('T') + ord('O') shl 8 + ord('R') shl 16 + ord('M') shl 24) then
      i := 5
    else
      i := 2;
  end;
  while i <= length(Name) do
  begin
    if Name[i] > ' ' then
    begin
      trunc[L] := Name[i];
      inc(L);
      if L = high(trunc) then
        break;
    end;
    inc(i);
  end;
  if L = 0 then
    exit;
  trunc[L] := #0;
  {$ifdef OSPTHREADS} // see https://stackoverflow.com/a/7989973
  if Assigned(pthread_setname_np) then
  try
    pthread_setname_np(pointer(ThreadID), @trunc[0]);
  except
    ; // ignore any exception (pthread confusion with its static version?)
  end;
  {$endif OSPTHREADS}
end;

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
begin
  if ThreadID <> MainThreadID then // don't change the main process name
    SetUnixThreadName(ThreadID, Name); // call pthread_setname_np()
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUtf8CharDynArray): cardinal;
begin
  result := 0; // Windows/CLR specific by now
end;

var
  _RawLogException: TOnRawLogException;

// FPC: intercept via the RaiseProc global variable
{$define WITH_RAISEPROC}
// RaiseProc redirection is implemented in main mormot.core.os.pas

{$endif NOEXCEPTIONINTERCEPT}

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

function FileAgeToUnixTimeUtc(const FileName: TFileName; AllowDir: boolean): TUnixTime;
var
  st: stat;
begin
  if (fpStat(pointer(FileName), st) = 0) and
     (AllowDir or
      (not FpS_ISDIR(st.st_mode))) then
    result := st.st_mtime // no local conversion
  else
    result := 0
end;

function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;
begin
  result := FileSetDate(Dest, FileGetDate(SourceHandle)) = 0;
end;

function FileSetDateFromWindowsTime(const Dest: TFileName;
  WinTime: integer): boolean;
var
  datetime: TDateTime;
begin
  datetime := WindowsFileTimeToDateTime(WinTime);
  result := (datetime <> 0) and
            (FileSetDate(Dest, DateTimeToFileDate(datetime)) = 0);
end;

function FileAgeToWindowsTime(const FileName: TFileName): integer;
begin
  result := DateTimeToWindowsFileTime(FileAgeToDateTime(FileName));
end;

procedure FileSetAttributes(const FileName: TFileName; Secret: boolean);
begin
  if Secret then
    fpchmod(pointer(FileName), S_IRUSR)
  else
    fpchmod(pointer(FileName), S_IRUSR or S_IWUSR);
end;

function FileSize(const FileName: TFileName): Int64;
var
  st: TStat;
begin
  if fpStat(pointer(FileName), st) <> 0 then
    result := 0
  else
    result := st.st_size;
end;

function FileSize(F: THandle): Int64;
var
  st: TStat;
begin
  if fpFstat(F, st) <> 0 then
    result := 0
  else
    result := st.st_size;
end;

function FileSeek64(Handle: THandle; const Offset: Int64; Origin: cardinal): Int64;
begin
  result := FPLSeek(Handle, Offset, Origin);
end;

function FileInfoByHandle(aFileHandle: THandle;
  out FileId, FileSize, LastWriteAccess, FileCreateDateTime: Int64): boolean;
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
  {$ifdef OSOPENBSD}
  if (lp.st_birthtime <> 0) and
     (lp.st_birthtime < lp.st_ctime) then
    lp.st_ctime := lp.st_birthtime;
  {$endif OSOPENBSD}
  FileCreateDateTime := lp.st_ctime * MSecsPerSec;
  if LastWriteAccess <> 0 then
    if (FileCreateDateTime = 0) or
       (FileCreateDateTime > LastWriteAccess) then
      FileCreateDateTime := LastWriteAccess;
  if lastreadaccess <> 0 then
    if (FileCreateDateTime = 0) or
       (FileCreateDateTime > lastreadaccess) then
      FileCreateDateTime := lastreadaccess;
end;

function CopyFile(const Source, Target: TFileName;
  FailIfExists: boolean): boolean;
var
  src, dst: TFileStream;
begin
  result := false;
  if FileExists(Target) then
    if FailIfExists then
      exit
    else
      DeleteFile(Target);
  try
    src := TFileStream.Create(Source, fmOpenRead);
    try
      dst := TFileStream.Create(Target, fmCreate);
      try
        dst.CopyFrom(src, src.Size);
      finally
        dst.Free;
      end;
      FileSetDateFrom(Target, src.Handle);
    finally
      src.Free;
    end;
    result := true;
  except
    result := false;
  end;
end;

function OemToUnicode(const oem: RawByteString): SynUnicode;
const
  OEMTOWIDE: array[128..255] of word = (
    // from Windows API oemtocharw()
    199, 252, 233, 226, 228, 224, 229, 231, 234, 235, 232, 239, 238, 236, 196,
    197, 201, 230, 198, 244, 246, 242, 251, 249, 255, 214, 220, 248, 163, 216,
    215, 402, 225, 237, 243, 250, 241, 209, 170, 186, 191, 174, 172, 189, 188,
    161, 171, 187, 9617, 9618, 9619, 9474, 9508, 193, 194, 192, 169, 9571, 9553,
    9559, 9565, 162, 165, 9488, 9492, 9524, 9516, 9500, 9472, 9532, 227, 195,
    9562, 9556, 9577, 9574, 9568, 9552, 9580, 164, 240, 208, 202, 203, 200, 305,
    205, 206, 207, 9496, 9484, 9608, 9604, 166, 204, 9600, 211, 223, 212, 210,
    245, 213, 181, 254, 222, 218, 219, 217, 253, 221, 175, 180, 173, 177, 8215,
    190, 182, 167, 247, 184, 176, 168, 183, 185, 179, 178, 9632, 160);
var
  len, i, c: PtrInt;
begin
  len := length(oem);
  SetLength(result, len);
  for i := 0 to len - 1 do
  begin
    c := PByteArray(oem)[i];
    if c > 127 then
      c := OEMTOWIDE[c];
    PWordArray(result)[i] := c;
  end;
end;

function OemToFileName(const oem: RawByteString): TFileName;
begin
  // let's assume TFileName is UTF-8 under Linux/POSIX
  result := Utf8Encode(OemToUnicode(oem));
end;

{$I-}
procedure DisplayFatalError(const title, msg: RawUtf8);
var
  err: ^Text;
begin
  err := @StdErr;
  if title <> '' then
    writeln(err^, Executable.ProgramName, ': ', title);
  writeln(err^, Executable.ProgramName, ': ', msg);
  ioresult;
end;
{$I+}

function FileOpenSequentialRead(const FileName: TFileName): integer;
begin
  // SysUtils.FileOpen = fpOpen + fpFlock
  result := fpOpen(pointer(FileName), O_RDONLY); // no fpFlock() call
end;

procedure SetEndOfFile(F: THandle);
begin
  FpFtruncate(F, FPLseek(F, 0, SEEK_CUR));
end;

procedure FlushFileBuffers(F: THandle);
begin
  FpFsync(F);
end;

function GetLastError: integer;
begin
  result := fpgeterrno;
end;

procedure SetLastError(error: integer);
begin
  fpseterrno(error);
end;

function GetErrorText(error: integer): RawUtf8;
begin
  result := StrError(error);
end;

function TMemoryMap.DoMap(aCustomOffset: Int64): boolean;
begin
  if aCustomOffset <> 0 then
    if (aCustomOffset and (SystemInfo.dwPageSize - 1)) <> 0 then
      raise EOSException.CreateFmt(
        'DoMap(aCustomOffset=%d) incompatible with dwPageSize=%d',
        [aCustomOffset, SystemInfo.dwPageSize])
    else
      aCustomOffset := aCustomOffset div SystemInfo.dwPageSize;
  fBuf := fpmmap(nil, fBufSize, PROT_READ, MAP_SHARED, fFile, aCustomOffset);
  if fBuf = MAP_FAILED then
  begin
    fBuf := nil;
    result := false;
  end
  else
    result := true;
end;

procedure TMemoryMap.DoUnMap;
begin
  if (fBuf <> nil) and
     (fBufSize > 0) and
     (fFile <> 0) then
    fpmunmap(fBuf, fBufSize);
end;

procedure SleepHiRes(ms: cardinal);
var
  timeout: TTimespec;
  d: cardinal;
begin
  if ms = 0 then
    // handle SleepHiRes(0) special case
    if SleepHiRes0Yield then
    begin
      // warning: reported as buggy by Alan on POSIX, and despitable by Linus
      // - from our testing, it gives worse performance than fpnanosleep()
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
    d := ms div 1000;
    timeout.tv_sec := d;
    timeout.tv_nsec := 1000000 * (ms - d * 1000);
  end;
  fpnanosleep(@timeout, nil)
  // no retry loop on ESysEINTR (as with regular RTL's Sleep)
end;

procedure SwitchToThread;
var
  timeout: Ttimespec;
begin
  // nanosleep() seems better than FPC RTL ThreadSwitch = sched_yield
  timeout.tv_sec := 0;
  timeout.tv_nsec := 10; // empirically identified on a recent Linux Kernel
  // note: nanosleep() adds a few dozen of microsecs for context switching
  fpnanosleep(@timeout, nil);
end;

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  {$ifdef OSLINUX}
  result := cs.__m_kind <> 0;
  {$else}
  result := not IsZero(@cs, SizeOf(cs));
  {$endif OSLINUX}
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  DoneCriticalSection(cs);
end;

function GetFileOpenLimit(hard: boolean): integer;
var
  limit: TRLIMIT;
begin
  if fpgetrlimit(RLIMIT_NOFILE, @limit) = 0 then
    if hard then
      result := limit.rlim_max
    else
      result := limit.rlim_cur
  else
    result := -1;
end;

function SetFileOpenLimit(max: integer; hard: boolean): integer;
var
  limit: TRLIMIT;
begin
  result := -1;
  if fpgetrlimit(RLIMIT_NOFILE, @limit) <> 0 then
    exit;
  if (hard and
      (integer(limit.rlim_max) = max)) or
     (not hard and
      (integer(limit.rlim_cur) = max)) then
    exit(max); // already to the expected value
  if hard then
    limit.rlim_max := max
  else
    limit.rlim_cur := max;
  if fpsetrlimit(RLIMIT_NOFILE, @limit) = 0 then
    result := GetFileOpenLimit(hard);
end;


{$ifdef OSLINUX} { the systemd API is Linux-specific }

{ TSystemD }

procedure TSystemD.DoLoad;
var
  p: PPointer;
  i, j: PtrInt;
const
  NAMES: array[0..5] of PAnsiChar = (
    'sd_listen_fds',
    'sd_is_socket_unix',
    'sd_journal_print',
    'sd_journal_sendv',
    'sd_notify',
    'sd_watchdog_enabled');
begin
  GlobalLock;
  if not tested then
  begin
    systemd := dlopen(LIBSYSTEMD_PATH, RTLD_LAZY);
    if systemd <> nil then
    begin
      p := @@listen_fds;
      for i := 0 to high(NAMES) do
      begin
        p^ := dlsym(systemd, NAMES[i]);
        if p^ = nil then
        begin
          p := @@listen_fds;
          for j := 0 to i do
          begin
            p^ := nil;
            inc(p);
          end;
          break;
        end;
        inc(p);
      end;
    end;
    tested := true;
  end;
  GlobalUnLock;
end;

function TSystemD.IsAvailable: boolean;
begin
  if not tested then
    DoLoad;
  result := Assigned(listen_fds);
end;

function TSystemD.ProcessIsStartedBySystemd: boolean;
begin
  result := IsAvailable and
    // note: for example on Ubuntu 20.04 INVOCATION_ID is always defined
    // from the other side PPID 1 can be in case we run under docker of started
    // by init.d so let's verify both
    (fpgetppid() = 1) and
    (fpGetenv(ENV_INVOCATION_ID) <> nil);
end;

procedure TSystemD.Done;
begin
  if systemd <> nil then
  begin
    dlclose(systemd);
    systemd := nil;
  end;
end;

{$endif OSLINUX}


// we bypass crt.pp since this unit cancels the SIGINT signal

procedure AllocConsole;
begin
  // nothing to do on Linux
end;

{$I-}

var
  TextAttr: integer = ord(ccDarkGray);

procedure TextColor(Color: TConsoleColor);
const
  TERM_CTRL: string[8] = '04261537';
begin
{$ifdef OSPOSIX}
  if not stdoutIsTTY then
    exit;
{$endif OSPOSIX}
  if ord(Color) = TextAttr then
    exit;
  TextAttr := ord(Color);
  if ord(Color) >= 8 then
    write(#27'[1;3', TERM_CTRL[(ord(Color) and 7) + 1], 'm')
  else
    write(#27'[0;3', TERM_CTRL[(ord(Color) and 7) + 1], 'm');
  ioresult;
end;

procedure TextBackground(Color: TConsoleColor);
begin
  // not implemented yet - but not needed either
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
  if IsMultiThread and
     (GetCurrentThreadID = MainThreadID) then
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

function ConsoleStdInputLen: integer;
begin
  if fpioctl(StdInputHandle, FIONREAD, @result) < 0 then
    result := 0;
end;

function Utf8ToConsole(const S: RawUtf8): RawByteString;
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
  TI, I: integer;
{$endif FPCUSEVERSIONINFO}
begin
  fFileName := aFileName;
  {$ifdef FPCUSEVERSIONINFO} // FPC 3.0+ if enabled in Synopse.inc / project options
  if aFileName <> '' then
  begin
    VI := TVersionInfo.Create;
    try
      if (aFileName <> '') and
         (aFileName <> ParamStr(0)) then
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

procedure GetUserHost(out User, Host: RawUtf8);
begin
  Host := RawUtf8(GetHostName);
  if Host = '' then
    Host := RawUtf8(GetEnvironmentVariable('HOSTNAME'));
  User := RawUtf8(GetEnvironmentVariable('LOGNAME')); // POSIX
  if User = '' then
    User := RawUtf8(GetEnvironmentVariable('USER'));
end;

var
  _Path: array[TSystemPath] of TFileName;

procedure SetSystemPath(kind: TSystemPath);
var
  s: TFileName;
begin
  case kind of
    spLog:
      if IsDirectoryWritable('/var/log') then
         // may not be writable by not root on POSIX
        s := '/var/log/'
      else if IsDirectoryWritable(Executable.ProgramFilePath) then
        s := Executable.ProgramFilePath
      else
        s := GetSystemPath(spUserData);
    spUserData:
      begin
        //  ~/.cache/appname
        s := GetEnvironmentVariable('XDG_CACHE_HOME');
        if (s = '') or
           not IsDirectoryWritable(s) then
          s := EnsureDirectoryExists(GetSystemPath(spUserDocuments) + '.cache');
        s := EnsureDirectoryExists(s + TFileName(Executable.ProgramName));
      end;
    spTempFolder:
      begin
        s := GetEnvironmentVariable('TMPDIR'); // POSIX
        if s = '' then
          s := GetEnvironmentVariable('TMP');
        if s = '' then
          if DirectoryExists('/tmp') then
            s := '/tmp'
          else
            s := '/var/tmp';
        s := IncludeTrailingPathDelimiter(s);
      end;
  else
    // POSIX requires a value for $HOME
    s := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  end;
  _Path[kind] := s;
end;

function GetSystemPath(kind: TSystemPath): TFileName;
begin
  result := _Path[kind];
  if result <> '' then
    exit;
  SetSystemPath(kind);
  result := _Path[kind];
end;

function SeemsRealPointer(p: pointer): boolean;
begin
  // let the GPF happen silently in the kernel (validated on Linux only)
  result := (PtrUInt(p) > 65535) and
            (fpaccess(p, F_OK) <> 0) and
            (fpgeterrno <> ESysEFAULT);
end;

{$ifdef OSBSDDARWIN}

function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;
begin
  result := nil; // not implemented yet on BSD/Darwin
  // https://kaashif.co.uk/2015/06/18/how-to-get-a-list-of-processes-on-openbsd-in-c/
end;

function EnumProcessName(PID: cardinal): RawUtf8;
begin
  result := '';
end;

function fpsysctlhwint(hwid: cint): Int64;
var
  mib: array[0..1] of cint;
  len: cint;
begin
  result := 0;
  mib[0] := CTL_HW;
  mib[1] := hwid;
  len := SizeOf(result);
  fpsysctl(pointer(@mib), 2, @result, @len, nil, 0);
end;

function fpsysctlhwstr(hwid: cint; var temp: ShortString): pointer;
var
  mib: array[0..1] of cint;
  len: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := hwid;
  FillCharFast(temp, SizeOf(temp), 0); // ShortString as 0-terminated buffer
  len := SizeOf(temp);
  fpsysctl(pointer(@mib), 2, @temp, @len, nil, 0);
  if temp[0] <> #0 then
    result := @temp
  else
    result := nil;
end;

type
  TLoadAvg = array[0..2] of double;

function getloadavg(var loadavg: TLoadAvg; nelem: integer): integer;
  cdecl external clib name 'getloadavg';

function RetrieveLoadAvg: RawUtf8;
var
  avg: TLoadAvg;
begin
  if getloadavg(avg, 3) = 3 then
    result := Format('%g %g %g', [avg[0], avg[1], avg[2]])
  else
    result := '';
end;

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
begin
  FillCharFast(info, SizeOf(info), 0);
  info.memtotal := fpsysctlhwint(
    {$ifdef OSDARWIN} HW_MEMSIZE {$else} HW_PHYSMEM {$endif});
  info.memfree := info.memtotal - fpsysctlhwint(HW_USERMEM);
  result := info.memtotal <> 0;// avoid div per 0 exception
  if result then
    info.percent := ((info.memtotal - info.memfree) * 100) div info.memtotal;
end;

procedure XorOSEntropy(var e: THash512Rec);
var
  avg: TLoadAvg;
  mem: TMemoryInfo;
  us: Int64;
begin
  QueryPerformanceMicroSeconds(us);
  e.i[0] := e.i[0] xor us;
  getloadavg(avg, 3);
  DefaultHasher128(@e.h1, @avg, SizeOf(avg));
  GetMemoryInfo(mem, {withalloc=}false);
  DefaultHasher128(@e.h2, @mem, SizeOf(mem));
  {$ifdef OSDARWIN} // FPC CreateGUID calls /dev/urandom which is not advised
  e.i[6] := e.i[6] xor mach_absolute_time; // monotonic clock in nanoseconds
  {$else}
  CreateGuid(TGUID(e.h3)); // use e.g. FreeBSD syscall or /dev/urandom
  {$endif OSDARWIN}
  QueryPerformanceMicroSeconds(us); // should have changed in-between
  e.i[7] := e.i[7] xor us;
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

function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;
var
  d: pDir;
  e: pDirent;
  n: integer;
  pid: cardinal;
  fn, status, tgid: RawUtf8;
begin
  result := nil;
  d := FpOpendir('/proc'); // (much) faster alternative to FindFirst()
  if d = nil then
    exit;
  n := 0;
  SetLength(result, 128);
  repeat
    e := FpReaddir(d^); // FPC RTL use getdents64 syscall on Linux and BSD :)
    if e = nil then
      break;
    if ((e.d_type = DT_UNKNOWN) or
        (e.d_type = DT_DIR)) and
       (e.d_name[0] in ['1'..'9']) then
    begin
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
    end;
  until false;
  FpClosedir(d^);
  SetLength(result, n);
end;

var
  tryprocexe: boolean = true;

function EnumProcessName(PID: cardinal): RawUtf8;
var
  proc: TFileName;
  cmdline: RawUtf8;
begin
  proc := '/proc/' + IntToStr(PID);
  if tryprocexe then
  begin
    // need to be root to follow /proc/[pid]/exe
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

function RetrieveLoadAvg: RawUtf8;
begin
  // the libc parses this file anyway :)
  result := StringFromFile('/proc/loadavg', {nosize=}true);
end;

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
var
  si: TSysInfo; // Linuxism
  P: PUtf8Char;
  mu: PtrUInt;
begin
  FillCharFast(info, SizeOf(info), 0);
  result := SysInfo(@si) = 0;
  mu := si.mem_unit;
  if si.totalram <> 0 then // avoid div per 0 exception
    info.percent := ((si.totalram - si.freeram) * 100) div si.totalram;
  info.memtotal := si.totalram * mu;
  info.memfree := si.freeram * mu;
  info.filetotal := si.totalswap * mu;
  info.filefree := si.freeswap * mu;
  // GetHeapStatus is only about current thread -> use /proc/[pid]/statm
  if withalloc then
  begin
    // virtual memory information is not available under Linux
    P := pointer(StringFromFile('/proc/self/statm', {hasnosize=}true));
    info.allocreserved := GetNextCardinal(P) * SystemInfo.dwPageSize; // VmSize
    info.allocused := GetNextCardinal(P) * SystemInfo.dwPageSize;     // VmRSS
  end;
end;

procedure DoHash128File(var h: THash128; const filename: TFileName);
var
  s: RawByteString;
begin
  s := StringFromFile(filename, {nosize=}true);
  DefaultHasher128(@h, pointer(s), length(s)); // maybe AesNiHash128
end;

procedure XorOSEntropy(var e: THash512Rec);
var
  si: TSysInfo;  // Linuxism
  rt: TTimeSpec; // with nanoseconds resolution
begin
  clock_gettime(CLOCK_MONOTONIC_HIRES, @rt);
  DefaultHasher128(@e.h0, @rt, SizeOf(rt));
  SysInfo(@si); // uptime + loadavg + meminfo + numprocess
  DefaultHasher128(@e.h0, @si, SizeOf(si));
  // detailed CPU execution context and timing from Linux kernel
  DoHash128File(e.h0, '/proc/self/statm');
  DoHash128File(e.h0, '/proc/self/stat');
  DoHash128File(e.h1, '/proc/stat');
  // read-only random UUID text like '6fd5a44b-35f4-4ad4-a9b9-6b9be13e1fe9'
  DoHash128File(e.h2, '/proc/sys/kernel/random/uuid');
  DoHash128File(e.h3, '/proc/sys/kernel/random/boot_id');
  clock_gettime(CLOCK_MONOTONIC_HIRES, @rt); // should have changed in-between
  DefaultHasher128(@e.h3, @rt, SizeOf(rt));
end;

{$endif OSBSDDARWIN}

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
begin
  result := false; // should call e.g. RetrieveLoadAvg() instead
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
begin
  result := false;
end;

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self, SizeOf(self), 0);
  result := false;
end;

function TProcessInfo.Start: boolean;
begin
  result := false;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
begin
  result := false;
end;

function TProcessInfo.PerSystem(out Idle, Kernel, User: single): boolean;
var
  P: PUtf8Char;
  U, K, I, S: cardinal;
begin
  // see http://www.linuxhowtos.org/System/procstat.htm
  result := false;
  P := pointer(StringFromFile('/proc/stat', {nosize=}true));
  if P = nil then
    exit;
  // e.g. 'cpu  3418147 18140 265232 6783435 12184 0 34219 0 0 0'
  U := GetNextCardinal(P){=user} + GetNextCardinal(P){=nice};
  K := GetNextCardinal(P){=system};
  I := GetNextCardinal(P){=idle};
  S := U + K + I;
  result := S <> 0;
  if not result then
    exit;
  Kernel := {%H-}SimpleRoundTo2Digits((K * 100) / S);
  User   := {%H-}SimpleRoundTo2Digits((U * 100) / S);
  Idle   := 100 - Kernel - User; // ensure sum is always 100%
end; { TODO : use a diff approach for TProcessInfo.PerSystem on Linux? }


function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;
var
  rd, dev: integer;
begin
  result := false;
  dev := FileOpenSequentialRead('/dev/urandom');  // non blocking on Linux + BSD
  if (dev <= 0) and
     AllowBlocking then
    dev := FileOpenSequentialRead('/dev/random'); // may block until got entropy
  if dev > 0 then
    try
      rd := 32; // read up to 256 bits - see "man urandom" Usage paragraph
      if Len <= 32 then
        rd := Len;
      result := (FileRead(dev, Buffer[0], rd) = rd);
      if result and
         (Len > 32) then
        RandomBytes(@Buffer[32], Len - 32); // simple gsl_rng_taus2 padding
    finally
      FileClose(dev);
    end;
  if not result then
    // OS API call failed -> fallback to our Lecuyer's gsl_rng_taus2 generator
    RandomBytes(pointer(Buffer), Len);
end;

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord): boolean;
var
  fs: tstatfs;
begin
  if aDriveFolderOrFile = '' then
    aDriveFolderOrFile := '.';
  result := fpStatFS(aDriveFolderOrFile, @fs) = 0;
  aAvailableBytes := QWord(fs.bavail) * QWord(fs.bsize);
  aFreeBytes := aAvailableBytes; // no user Quota involved here
  aTotalBytes := QWord(fs.blocks) * QWord(fs.bsize);
end;

const
  {$ifdef OSBSDDARWIN}
  POSIXMOUNTPOINTS = '/etc/mtab';
  {$else}
  POSIXMOUNTPOINTS = '/proc/self/mounts';
  {$endif OSBSDDARWIN}

function GetDiskPartitions: TDiskPartitions;
var
  mounts, fs, mnt, typ: RawUtf8;
  p: PUtf8Char;
  fn: TFileName;
  n: integer;
  av, fr, tot: QWord;
begin
  // see https://github.com/gagern/gnulib/blob/master/lib/mountlist.c
  mounts := StringFromFile(POSIXMOUNTPOINTS, {hasnosize=}true);
  n := 0;
  p := pointer(mounts);
  repeat
    fs :=  GetNextItem(p);
    mnt := GetNextItem(p);
    typ := GetNextItem(p);
    if (fs <> '') and
       (fs <> 'rootfs') and
       not IdemPChar(pointer(fs), '/DEV/LOOP') and
       (mnt <> '') and
       (mnt <> '/mnt') and
       (typ <> '') and
       not IdemPChars(mnt, ['/PROC/', '/SYS/', '/RUN/']) and
       not IdemPChars(typ, ['AUTOFS', 'PROC', 'SUBFS', 'DEBUGFS', 'DEVPTS',
        'FUSECTL', 'MQUEUE', 'RPC-PIPEFS', 'SYSFS', 'DEVFS', 'KERNFS',
        'IGNORE', 'NONE', 'TMPFS', 'SECURITYFS', 'RAMFS', 'ROOTFS', 'DEVTMPFS',
        'HUGETLBFS', 'ISO9660']) then
    begin
      fn := mnt;
      if GetDiskInfo(fn, av, fr, tot) and
         (tot > 1 shl 20) then
      begin
  //writeln('fs=',fs,' mnt=',mnt,' typ=',typ, ' av=',av,' fr=',fr,' tot=',tot);
        if n = length(result) then
          SetLength(result, NextGrow(n));
        result[n].name := fs;
        result[n].mounted := fn;
        result[n].size := tot;
        inc(n);
      end;
    end;
    p := GotoNextLine(p);
  until p = nil;
  SetLength(result, n);
end;

{$ifdef OSBSDDARWIN}
  {$define USEMPROTECT}
{$else}
  {$ifdef OSANDROID}
    {$define USEMPROTECT}
  {$endif OSANDROID}
{$endif OSBSDDARWIN}

{$ifdef USEMPROTECT}
function mprotect(Addr: Pointer; Len: size_t; Prot: integer): integer;
  cdecl; external clib name 'mprotect';
{$endif USEMPROTECT}

function SynMProtect(addr: pointer; size: size_t; prot: integer): integer;
begin
  result := -1;
  {$ifdef UNIX}
    {$ifdef USEMPROTECT}
    result := mprotect(addr, size, prot);
    {$else}
    if Do_SysCall(syscall_nr_mprotect, PtrUInt(addr), size, prot) >= 0 then
      result := 0;
    {$endif USEMPROTECT}
  {$endif UNIX}
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  PageSize: PtrUInt;
  AlignedAddr: pointer;
  i: PtrInt;
  ProtectedResult, ProtectedMemory: boolean;
begin
  if Backup <> nil then
    for i := 0 to Size - 1 do // do not use Move() here
      PByteArray(Backup)^[i] := PByteArray(Old)^[i];
  PageSize := SystemInfo.dwPageSize;
  AlignedAddr :=
    Pointer((PtrUInt(Old) div SystemInfo.dwPageSize) * SystemInfo.dwPageSize);
  while PtrUInt(Old) + PtrUInt(Size) >= PtrUInt(AlignedAddr) + PageSize do
    Inc(PageSize, SystemInfo.dwPageSize);
  ProtectedResult := SynMProtect(
    AlignedAddr, PageSize, PROT_READ or PROT_WRITE or PROT_EXEC) = 0;
  ProtectedMemory := not ProtectedResult;
  if ProtectedMemory then
    ProtectedResult := SynMProtect(
      AlignedAddr, PageSize, PROT_READ or PROT_WRITE) = 0;
  if ProtectedResult then
  try
    for i := 0 to Size - 1 do // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    if not LeaveUnprotected and
       ProtectedMemory then
      SynMProtect(AlignedAddr, PageSize, PROT_READ or PROT_EXEC);
  except
  end;
end;

const
  STUB_SIZE = 65536; // 16*4 KB (4 KB = memory granularity)
  // on most platforms, Compute_FAKEVMT is run once with all JITted stubs
  // on i386, it needs ArgsSizeInStack adjustement, but only 24 bytes per method

{$ifdef CPUARM}

var
  StubCallAllocMemLastStart: PtrUInt; // avoid unneeded fpmmap() calls

function StubCallAllocMem(const Size, flProtect: DWORD): pointer;
const
  STUB_RELJMP = {$ifdef CPUARM} $7fffff {$else} $7fffffff {$endif}; // rel jmp
  STUB_INTERV = STUB_RELJMP + 1; // try to reserve in closed stub interval
  STUB_ALIGN = QWord($ffffffffffff0000); // align to STUB_SIZE
var
  start, stop, stub, dist: PtrUInt;
begin
  stub := PtrUInt(ArmFakeStubAddr); // = @TInterfacedObjectFake.ArmFakeStub
  if StubCallAllocMemLastStart <> 0 then
    start := StubCallAllocMemLastStart
  else
  begin
    start := stub - STUB_INTERV;
    if start > stub then
      start := 0; // avoid range overflow
    start := start and STUB_ALIGN;
  end;
  stop := stub + STUB_INTERV;
  if stop < stub then
    stop := high(PtrUInt);
  stop := stop and STUB_ALIGN;
  while start < stop do
  begin
    // try whole -STUB_INTERV..+STUB_INTERV range
    inc(start, STUB_SIZE);
    result := fpmmap(pointer(start), STUB_SIZE,
      flProtect, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
    if result <> MAP_FAILED then
    begin
      // close enough for a 24/32-bit relative jump?
      dist := abs(stub - PtrUInt(result));
      if dist < STUB_RELJMP then
      begin
        StubCallAllocMemLastStart := start;
        exit;
      end
      else
        fpmunmap(result, STUB_SIZE);
    end;
  end;
  result := MAP_FAILED; // error
end;

{$else}

// other platforms (Intel+Arm64) use plain Kernel call and PtrInt jump
function StubCallAllocMem(const Size, flProtect: DWORD): pointer;
begin
  result := fpmmap(nil, STUB_SIZE,
    flProtect, MAP_PRIVATE OR MAP_ANONYMOUS, -1, 0);
end;

{$endif CPUARM}


{ ****************** Unix Daemon and Windows Service Support }

// Linux/POSIX signal interception

var
  SynDaemonIntercepted: boolean;
  SynDaemonInterceptLog: TSynLogProc;

procedure DoShutDown(Sig: integer; Info: PSigInfo; Context: PSigContext); cdecl;
var
  level: TSynLogInfo;
  si_code: integer;
  text: ShortString; // code below has no memory allocation
begin
  if Assigned(SynDaemonInterceptLog) then
  begin
    case Sig of
      SIGQUIT:
        text := 'QUIT';
      SIGTERM:
        text := 'TERM';
      SIGINT:
        text := 'INT';
      SIGABRT:
        text := 'ABRT';
    else
      str(Sig, text);
    end;
    if Sig = SIGTERM then
      // polite quit
      level := sllInfo
    else
       // abort after panic
      level := sllExceptionOS;
    if Info = nil then
      si_code := 0
    else
      si_code := Info^.si_code;
    SynDaemonInterceptLog(level,
      'SynDaemonIntercepted received SIG%=% si_code=%',
      [text, Sig, si_code], nil);
  end;
  SynDaemonTerminated := Sig;
end;

procedure SynDaemonIntercept(const onlog: TSynLogProc);
var
  saOld, saNew: SigactionRec;
begin
  // note: SIGFPE/SIGSEGV/SIGBUS/SIGILL are handled by the RTL
  if SynDaemonIntercepted then
    exit;
  SynDaemonIntercepted := true;
  SynDaemonInterceptLog := onlog;
  FillCharFast(saNew, SizeOf(saNew), 0);
  saNew.sa_handler := @DoShutDown;
  fpSigaction(SIGQUIT, @saNew, @saOld);
  fpSigaction(SIGTERM, @saNew, @saOld);
  fpSigaction(SIGINT, @saNew, @saOld);
  fpSigaction(SIGABRT, @saNew, @saOld);
end;

type
  TPasswd = record
    pw_name: PAnsiChar;    // user name
    pw_passwd: PAnsiChar;  // encrypted password
    pw_uid: TUid;	   // user uid
    pw_gid: TGid;	   // user gid
    // following fields are not consistent on BSD or Linux, but not needed
  end;
  PPasswd  = ^TPasswd;

// retrieve information of a given user by name
function getpwnam(name: PAnsiChar): PPasswd;
  cdecl; external clib;

// sets the supplementary group IDs for the calling process
function setgroups(n: size_t; groups: PGid): integer;
  cdecl; external clib;

function setuid(uid: TUid): integer;
  cdecl; external clib;
function setgid(gid: TGid): integer;
  cdecl; external clib;

// changes the root directory of the calling process
function chroot(rootpath: PAnsiChar): integer;
  cdecl; external clib;

function DropPriviledges(const UserName: RawUtf8): boolean;
var
  pwnam: PPasswd;
begin
  result := false;
  pwnam := getpwnam(pointer(UserName));
  if (pwnam = nil) or
     ((setgid(pwnam.pw_gid) <> 0) and
      (fpgeterrno <> ESysEPERM)) or
     ((setuid(pwnam.pw_uid) <> 0) and
      (fpgeterrno <> ESysEPERM)) then
    exit;
  result := true;
end;

function ChangeRoot(const FolderName: RawUtf8): boolean;
begin
  result := (FolderName <> '') and
            (FpChdir(pointer(FolderName)) = 0) and
            (chroot('.') = 0);
end;

function RunUntilSigTerminatedPidFile: TFileName;
begin
  result := RunUntilSigTerminatedPidFilePath;
  if result = '' then
    result := Executable.ProgramFilePath;
  result := Format('%s.%s.pid', [result, Executable.ProgramName]);
end;

function RunUntilSigTerminatedForKill(waitseconds: integer): boolean;
var
  pid: PtrInt;
  pidfilename: TFileName;
  tix: Int64;
begin
  result := false;
  pidfilename := RunUntilSigTerminatedPidFile;
  pid := GetInteger(pointer(StringFromFile(pidfilename)));
  if pid <= 0 then
    exit;
  if fpkill(pid, SIGTERM) <> 0 then // polite quit
    if fpgeterrno <> ESysESRCH then
      exit
    else
    // ESysESRCH = no such process -> try to delete the .pid file
    if DeleteFile(pidfilename) then
    begin
      result := true; // process crashed or hard reboot -> nothing to kill
      exit;
    end;
  if waitseconds <= 0 then
  begin
    result := true;
    exit;
  end;
  tix := GetTickCount64 + waitseconds * 1000;
  repeat
    // RunUntilSigTerminated() below should delete the .pid file
    sleep(10);
    if not FileExists(pidfilename) then
      result := true;
  until result or
        (GetTickCount64 > tix);
  if not result then
    fpkill(pid, SIGKILL); // finesse
end;

procedure CleanAfterFork;
begin
  fpUMask(0); // reset file mask
  chdir('/'); // avoid locking current directory
  Close(input);
  AssignFile(input, '/dev/null');
  ReWrite(input);
  Close(output);
  AssignFile(output, '/dev/null');
  ReWrite(output);
  Close(stderr);
end;

procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; const onlog: TSynLogProc;
  const servicename: string);
var
  pid, sid: TPID;
  pidfilename: TFileName;
  s: AnsiString;
const
  TXT: array[boolean] of string[4] = ('run', 'fork');
begin
  SynDaemonIntercept(onlog);
  if dofork then
  begin
    pidfilename := RunUntilSigTerminatedPidFile;
    pid := GetInteger(pointer(StringFromFile(pidfilename)));
    if pid > 0 then
      if (fpkill(pid, 0) = 0) or
         not DeleteFile(pidfilename) then
        raise EOSException.CreateFmt(
          '%s.CommandLine Fork failed: %s is already forked as pid=%d',
          [daemon.ClassName, Executable.ProgramName, PtrInt(pid)]);
    pid := fpFork;
    if pid < 0 then
      raise EOSException.CreateFmt(
        '%s.CommandLine Fork failed', [daemon.ClassName]);
    if pid > 0 then  // main program - just terminate
      exit;
    // clean forked instance
    sid := fpSetSID;
    if sid < 0 then // new session (process group) created?
      raise EOSException.CreateFmt(
        '%s.CommandLine SetSID failed', [daemon.ClassName]);
    CleanAfterFork;
    // create local .[Executable.ProgramName].pid file
    pid := fpgetpid;
    str(pid, s);
    FileFromString(s, pidfilename);
  end;
  try
    if Assigned(onlog) then
      onlog(sllNewRun, 'Start % /% %',
        [servicename, TXT[dofork], Executable.Version.DetailedOrVoid], nil);
    Start;
    while SynDaemonTerminated = 0 do
      if GetCurrentThreadID = MainThreadID then
        CheckSynchronize(100)
      else
        Sleep(100);
  finally
    if Assigned(onlog) then
      onlog(sllNewRun, 'Stop /% from Sig=%',
        [TXT[dofork], SynDaemonTerminated], nil);
    try
      Stop;
    finally
      if dofork and
         (pidfilename <> '') then
      begin
        DeleteFile(pidfilename);
        if Assigned(onlog) then
          onlog(sllTrace, 'RunUntilSigTerminated: deleted file %',
            [pidfilename], nil);
      end;
    end;
  end;
end;

function RunInternal(args: PPAnsiChar; waitfor: boolean; const env: TFileName;
  envaddexisting: boolean): integer;
var
  pid: TPID;
  e: array[0..511] of PAnsiChar; // max 512 environment variables
  envpp: PPAnsiChar;
  P: PAnsiChar;
  n: PtrInt;
begin
  {$ifdef FPC}
  {$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
  pid := FpvFork;
  {$else}
  pid := FpFork;
  {$ifend}
  {$else}
  'only FPC is supported yet';
  {$endif FPC}
  if pid < 0 then
  begin
    // fork failed
    result := -1;
    exit;
  end;
  if pid = 0 then
  begin
    // we are in child process -> switch to new executable
    if not waitfor then
      // don't share the same console
      CleanAfterFork;
    envpp := envp;
    if env <> '' then
    begin
      n := 0;
      result := -ESysE2BIG;
      if envaddexisting and
         (envpp <> nil) then
      begin
        while envpp^ <> nil do
        begin
          if PosChar(envpp^, #10) = nil then
          begin
            // filter to add only single-line variables
            if n = high(e) - 1 then
              exit;
            e[n] := envpp^;
            inc(n);
          end;
          inc(envpp);
        end;
      end;
      P := pointer(env); // env follows Windows layout 'n1=v1'#0'n2=v2'#0#0
      while P^ <> #0 do
      begin
        if n = high(e) - 1 then
          exit;
        e[n] := P; // makes POSIX compatible
        inc(n);
        inc(P, StrLen(P) + 1);
      end;
      e[n] := nil; // end with null
      envpp := @e;
    end;
    FpExecve(args^, args, envpp);
    FpExit(127);
  end;
  if waitfor then
  begin
    result := WaitProcess(pid);
    if result = 127 then
      // execv() failed in child process
      result := -result;
  end
  else
    // fork success (don't wait for the child process to fail)
    result := 0;
end;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2, arg3, arg4, arg5, env: TFileName;
  envaddexisting: boolean): integer;
var
  a: array[0..6] of PAnsiChar; // assume no UNICODE on POSIX, i.e. as TFileName
begin
  a[0] := pointer(path);
  a[1] := pointer(arg1);
  a[2] := pointer(arg2);
  a[3] := pointer(arg3);
  a[4] := pointer(arg4);
  a[5] := pointer(arg5);
  a[6] := nil; // end pointer list with null
  result := RunInternal(@a, waitfor, env, envaddexisting);
end;

function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName; envaddexisting: boolean;
  parsed: PParseCommands): integer;
var
  temp: RawUtf8;
  err: TParseCommands;
  a: TParseCommandsArgs;
begin
  err := ParseCommandArgs(cmd, @a, nil, @temp);
  if parsed <> nil then
    parsed^ := err;
  if err = [] then
    // no need to spawn the shell for simple commands
    result := RunInternal(a, waitfor, env, envaddexisting)
  else if err * PARSECOMMAND_ERROR <> [] then
    // no system call for clearly invalid command line
    result := -ESysEPERM
  else
  begin
    // execute complex commands via the shell
    a[0] := '/bin/sh';
    a[1] := '-c';
    a[2] := pointer(cmd);
    a[3] := nil;
    result := RunInternal(@a, waitfor, env, envaddexisting);
  end;
end;


{ ****************** Gather Operating System Information }

{$ifndef OSANDROID}
function getpagesize: integer; cdecl; external clib;
{$endif OSANDROID}

{$ifdef OSLINUX}

function get_nprocs: integer;  cdecl; external clib;

procedure SetLinuxDistrib(const release: RawUtf8);
var
  distrib: TOperatingSystem;
  rel, dist: RawUtf8;
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

function clock_gettime_c(clk_id: clockid_t; tp: ptimespec): cint;
begin
  // FPC only knows the regular clocks: convert to the *_FAST version
  case clk_id of
    // 1 ms resolution is good enough for milliseconds-based RTL functions
    CLOCK_REALTIME:
      clk_id := CLOCK_REALTIME_FAST;
    CLOCK_MONOTONIC:
      clk_id := CLOCK_MONOTONIC_FAST;
    // no CLOCK_MONOTONIC_RAW redirect because it doesn't match CLOCK_MONOTONIC
    // and cthreads.pp forces pthread_condattr_setclock(CLOCK_MONOTONIC_RAW)
  end;
  // it is much faster to not use the Linux syscall but the libc vDSO call
  result := clock_gettime(clk_id, tp);
end;

function gettimeofday_c(tp: ptimeval; tzp: ptimezone): cint;
begin
  // it is much faster to not use the Linux syscall but the libc vDSO call
  result := gettimeofday(tp, tzp);
end;

{$endif OSLINUX}

function Hex2Dec(c: AnsiChar): integer; inline;
begin
  result := ord(c);
  case c of
    '0'..'9':
      dec(result, ord('0'));
    'A'..'Z':
      dec(result, ord('A') - 10);
    'a'..'z':
      dec(result, ord('a') - 10);
  else
    result := -1;
  end;
end;

function PosixParseHex32(p: PAnsiChar): integer;
var
  v0, v1: integer;
begin
  result := 0;
  p := StrScan(p, 'x');
  if p = nil then
    exit;
  repeat
    inc(p); // points to trailing 'x' at start
    v0 := Hex2Dec(p^);
    if v0 < 0 then
      break; // not in '0'..'9','a'..'f'
    inc(p);
    v1 := Hex2Dec(p^);
    if v1 < 0 then
    begin
      result := (result shl 4) or v0; // only one char left
      break;
    end;
    result := (result shl 8) or (v0 shl 4) or v1;
  until false;
end;

procedure ParseHex32Add(p: PAnsiChar; var result: TIntegerDynArray);
var
  v: integer;
begin
  v := PosixParseHex32(p);
  if v <> 0 then
    AddInteger(result, v, {nodup=}true);
end;

{$ifdef CPUARM3264} // POSIX libc is faster than FPC RTL or our pascal code

function libc_strlen(s: PAnsiChar): SizeInt;
  cdecl external clib name 'strlen';

function libc_memmove(dst, src: pointer; n: SizeInt): pointer;
  cdecl external clib name 'memmove';

function libc_memset(dst: pointer; c: integer; n: SizeInt): pointer;
  cdecl external clib name 'memset';

function StrLenLibc(s: PAnsiChar): SizeInt;
begin
  if s = nil then
    result := PtrUInt(s)
  else
    result := libc_strlen(s);
end;

procedure MoveFastLibC(const source; var dest; count: SizeInt);
begin
  if (@dest <> @source) and
     (count > 0) then
    libc_memmove(@dest, @source, count);
end;

procedure FillCharLibC(var dest; count: PtrInt; value: byte);
begin
  if (@dest <> nil) and
     (count > 0) then
    libc_memset(@dest, value, count);
end;

{$ifdef OSLINUXANDROID}

procedure RetrieveCpuInfoArm;
begin
  if CpuFeatures = [] then
  begin
    // fallback to /proc/cpuinfo "Features:" text
    if PosEx(' aes', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcAes);
    if PosEx(' pmull', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcPmull);
    if PosEx(' sha1', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcSha1);
    if PosEx(' sha2', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcSha2);
    if PosEx(' crc32', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcCrc32);
  end;
end;

{$endif OSLINUXANDROID}

{$endif CPUARM3264}

function ParseLine(P: PUtf8Char): PUtf8Char;
begin
  if P <> nil then
    P := strscan(P, ':');
  result := P;
  if P = nil then
    exit;
  repeat
    inc(P);
  until (P^ = #0) or
        (P^ > ' ');
  result := P;
  while not (ord(P^) in [0, 10, 13]) do
  begin
    if P^ < ' ' then
      P^ := ' '; // change any tab into space
    inc(P);
  end;
  P^ := #0; // make asciiz
end;

{$ifdef CPUAARCH64}

{$L ..\..\static\aarch64-linux\armv8.o} // ARMv8 crc32c Linux code for POSIX

function crc32carm64(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal; external;
function crc32cby4arm64(crc, value: cardinal): cardinal; external;
procedure crc32blockarm64(crc128, data128: PBlock128); external;
procedure crc32blocksarm64(crc128, data128: PBlock128; count: integer); external;

{$endif CPUAARCH64}

function KB(Size: cardinal): shortstring;
begin
  if Size >= 1 shl 30 then
  begin
    str(Size shr 30, result);
    result := result + 'GB';
  end
  else if Size >= 1 shl 20 then
  begin
    str(Size shr 20, result);
    result := result + 'MB';
  end
  else
  begin
    str(Size shr 10, result);
    result := result + 'KB';
  end
end;

procedure InitializeSpecificUnit;
var
  P: PAnsiChar;
  modname, beg: PUtf8Char;
  uts: UtsName;
  {$ifdef CPUARM3264}
  act, aci: TIntegerDynArray;
  i: PtrInt;
  {$endif CPUARM3264}
  {$ifndef OSDARWIN}
  tp: timespec;
  {$endif OSDARWIN}
  {$ifdef OSBSDDARWIN}
  temp: ShortString;
  {$else}
  hw, cache, cpuinfo: PUtf8Char;
  proccpuinfo, release, prod: RawUtf8;
  {$ifdef OSLINUX}
  prodver, dist: RawUtf8;
  SR: TSearchRec;
  {$endif OSLINUX}
  {$endif OSBSDDARWIN}
  tm: TThreadManager;
begin
  // retrieve Kernel and Hardware information
  stdoutIsTTY := (IsATTY(StdOutputHandle) = 1) and
                 IdemPChars(RawUtf8(GetEnvironmentVariable('TERM')), [
                   'XTERM', 'SCREEN', 'TMUX', 'RXVT', 'LINUX', 'CYGWIN']);
  modname := nil;
  fpuname(uts);
  {$ifdef OSANDROID}
  SystemInfo.dwPageSize := 4096;
  {$else}
  SystemInfo.dwPageSize := getpagesize; // call libc API
  {$endif OSANDROID}
  {$ifdef OSBSDDARWIN}
  SystemInfo.dwNumberOfProcessors := fpsysctlhwint(HW_NCPU);
  beg := fpsysctlhwstr(HW_MACHINE, temp);
  FastSetString(BiosInfoText, beg, StrLen(beg));
  modname := fpsysctlhwstr(HW_MODEL, temp);
  with uts do
    OSVersionText := sysname + '-' + release + ' ' + version;
  {$ifdef OSDARWIN}
  CpuCache[1].LineSize := fpsysctlhwint(HW_CACHELINE);
  CpuCache[1].Size := fpsysctlhwint(HW_L1DCACHESIZE);
  CpuCache[2].LineSize := fpsysctlhwint(HW_CACHELINE);
  CpuCache[2].Size := fpsysctlhwint(HW_L2CACHESIZE);
  CpuCacheSize := CpuCache[2].Size;
  if CpuCacheSize = 0 then
    CpuCacheSize := CpuCache[1].Size;
  if CpuCacheSize <> 0 then
    CpuCacheText := 'L1=' + KB(CpuCache[1].Size) + ' L2=' + KB(CpuCache[2].Size);
  {$endif OSDARWIN}
  {$else}
  {$ifdef OSANDROID}
  release := GetSystemProperty('ro.build.version.release');
  prod := TrimU(RawUtf8(GetSystemProperty('ro.product.brand') + ' ' +
                        GetSystemProperty('ro.product.name') + ' ' +
                        GetSystemProperty('ro.product.device')));
  {$else}
  prod := TrimU(TrimU(StringFromFile('/sys/class/dmi/id/sys_vendor', true)) +
    ' ' + StringFromFile('/sys/class/dmi/id/product_name', true));
  if prod <> '' then
  begin // e.g. 'QEMU KVM Virtual Machine' or 'LENOVO 20HES23B0U'
    prodver := TrimU(StringFromFile('/sys/class/dmi/id/product_version', true));
    if prodver <> '' then
      prod := prod + ' ' + prodver;
  end
  else
    // return e.g. 'Raspberry Pi 3 Model B Rev 1.2'
    prod := TrimU(StringFromFile('/proc/device-tree/model', true));
  FindNameValue(StringFromFile('/etc/os-release'), 'PRETTY_NAME=', release);
  if (release <> '') and
     (release[1] = '"') then
    release := copy(release, 2, length(release) - 2);
  TrimSelf(release);
  if release = '' then
  begin
    FindNameValue(StringFromFile('/etc/lsb-release'), 'DISTRIB_DESCRIPTION=', release);
    if (release <> '') and
       (release[1] = '"') then
      release := copy(release, 2, length(release) - 2);
  end;
  if (release = '') and
     (FindFirst('/etc/*-release', faAnyFile, SR) = 0) then
  begin
    release := SR.Name; // 'redhat-release' 'SuSE-release'
    if IdemPChar(pointer(release), 'LSB-') and
       (FindNext(SR) = 0) then
      release := SR.Name;
    release := split(release, '-');
    dist := split(TrimU(StringFromFile('/etc/' + SR.Name)), #10);
    if (dist <> '') and
       (PosExChar('=', dist) = 0) and
       (PosExChar(' ', dist) > 0) then
      // e.g. 'Red Hat Enterprise Linux Server release 6.7 (Santiago)'
      SetLinuxDistrib(dist)
    else
      dist := '';
    FindClose(SR);
  end;
  if (release <> '') and
     (OS_KIND = osLinux) then
  begin
    SetLinuxDistrib(release);
    if (OS_KIND = osLinux) and
       ({%H-}dist <> '') then
    begin
      SetLinuxDistrib(dist);
      release := dist;
    end;
    if (OS_KIND = osLinux) and
       ((PosEx('RH', release) > 0) or
        (PosEx('Red Hat', release) > 0)) then
      OS_KIND := osRedHat;
  end;
  {$endif OSANDROID}
  BiosInfoText := prod;
  hw := nil;
  cache := nil;
  SystemInfo.dwNumberOfProcessors := 0;
  proccpuinfo := StringFromFile('/proc/cpuinfo', true);
  cpuinfo := pointer(proccpuinfo);
  while cpuinfo <> nil do
  begin
    beg := cpuinfo;
    cpuinfo := GotoNextLine(cpuinfo);
    if IdemPChar(beg, 'PROCESSOR') then
      if beg^ = 'P' then
        modname := ParseLine(beg) // Processor : ARMv7
      else
        // loop over all "processor : 0 .. 1 .. 2" lines to count the CPUs
        inc(SystemInfo.dwNumberOfProcessors)
    else if IdemPChar(beg, 'MODEL NAME') then
      modname := ParseLine(beg)
    else if IdemPChar(beg, 'FEATURES') or
            IdemPChar(beg, 'FLAGS') then
      CpuInfoFeatures := ParseLine(beg)
    else if IdemPChar(beg, 'HARDWARE') then
      hw := ParseLine(beg)
    else if IdemPChar(beg, 'CACHE SIZE') then
      cache := ParseLine(beg)
    {$ifdef CPUARM3264}
    else if IdemPChar(beg, 'CPU IMPLEMENTER') then
      ParseHex32Add(beg, aci)
    else if IdemPChar(beg, 'CPU PART') then
      ParseHex32Add(beg, act);
    {$endif CPUARM3264}
  end;
  {$ifdef CPUARM3264}
  if act <> nil then // CPU part/implementer are more detailed than model name
  begin
    proccpuinfo := '';
    for i := 0 to high(aci) do // there should be a single implementer
      proccpuinfo := proccpuinfo +
        ArmCpuImplementerName(ArmCpuImplementer(aci[i]), aci[i]) + ' ';
    proccpuinfo := proccpuinfo + ArmCpuTypeName(ArmCpuType(act[0]), act[0]);
    for i := 1 to high(act) do // but there may be several parts/models
      proccpuinfo := proccpuinfo + ' / ' + ArmCpuTypeName(ArmCpuType(act[i]), act[i]);
    modname := pointer(proccpuinfo);
  end;
  RetrieveCpuInfoArm;
  {$endif CPUARM3264}
  if hw <> nil then
  begin
    prod := hw;
    if BiosInfoText = '' then
      BiosInfoText := prod
    else
      BiosInfoText := BiosInfoText + ' (' + prod + ')';
  end;
  if cache <> nil then
  begin
    CpuCacheText := TrimU(cache);
    CpuCacheSize := GetNextCardinal(cache);
    while cache^ = ' ' do
      inc(cache);
    case upcase(cache^) of
      'K':
        CpuCacheSize := CpuCacheSize shl 10;
      'M':
        CpuCacheSize := CpuCacheSize shl 20;
      'G':
        CpuCacheSize := CpuCacheSize shl 30;
    end;
  end;
  SystemInfo.release := release;
  {$endif OSBSDDARWIN}
  if CpuCacheSize <> 0 then
    CpuInfoText := KB(CpuCacheSize) + ' cache';
  SystemInfo.uts.release := uts.Release;
  SystemInfo.uts.sysname := uts.Sysname;
  SystemInfo.uts.version := uts.Version;
  P := @uts.release[0];
  KernelRevision := GetNextCardinal(P) shl 16 +
                    GetNextCardinal(P) shl 8 +
                    GetNextCardinal(P);
  OSVersion32.os := OS_KIND;
  MoveSmall(@KernelRevision, @OSVersion32.utsrelease, 3); // 24-bit version
  with SystemInfo.uts do
    OSVersionText := sysname + ' ' + release;
  if SystemInfo.release <> '' then
    OSVersionText := SystemInfo.release + ' - ' + OSVersionText;
  {$ifdef OSANDROID}
  OSVersionText := 'Android (' + OSVersionText + ')';
  {$else}
  {$ifdef OSLINUX}
  if SystemInfo.dwNumberOfProcessors = 0 then // e.g. QEMU limited /proc/cpuinfo
    SystemInfo.dwNumberOfProcessors := get_nprocs;
  {$endif OSLINUX}
  {$endif OSANDROID}
  if SystemInfo.dwNumberOfProcessors = 0 then
    SystemInfo.dwNumberOfProcessors := 1;
  if modname = nil then
    CpuInfoText := Format('%d x generic ' + CPU_ARCH_TEXT + ' cpu %s',
      [SystemInfo.dwNumberOfProcessors, CpuInfoText])
  else
    CpuInfoText := Format('%d x %s %s (' + CPU_ARCH_TEXT + ')',
      [SystemInfo.dwNumberOfProcessors, modname, CpuInfoText]);
  if CpuInfoText = '' then
    CpuInfoText := CPU_ARCH_TEXT;
  // intialize supported APIs
  TimeZoneLocalBias := -GetLocalTimeOffset;
  GetThreadManager(tm); // for inlined RTL calls (avoid one level of redirection)
  @GetCurrentThreadId      := @tm.GetCurrentThreadId;
  @TryEnterCriticalSection := @tm.TryEnterCriticalSection;
  @EnterCriticalSection    := @tm.EnterCriticalSection;
  @LeaveCriticalSection    := @tm.LeaveCriticalSection;
  {$ifdef OSDARWIN}
  mach_timebase_info(mach_timeinfo);
  mach_timecoeff := mach_timeinfo.Numer / mach_timeinfo.Denom;
  mach_timenanosecond := (mach_timeinfo.Numer = 1) and
                         (mach_timeinfo.Denom = 1);
  {$else}
  {$ifdef OSPOSIX}
  // try Linux kernel 2.6.32+ or FreeBSD 8.1+ fastest clocks
  if clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0 then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0 then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  {$ifdef OSLINUX}
  if clock_gettime(CLOCK_MONOTONIC_RAW, @tp) = 0 then
    CLOCK_MONOTONIC_HIRES := CLOCK_MONOTONIC_RAW;
  {$endif OSLINUX}
  if (clock_gettime(CLOCK_REALTIME_FAST, @tp) <> 0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST, @tp) <> 0) or
     (clock_gettime(CLOCK_MONOTONIC_HIRES, @tp) <> 0) then
    raise EOSException.CreateFmt(
      'clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release), fpgeterrno]);
  // direct access to the pthread library if possible (Linux only)
  {$ifdef OSPTHREADS}
  // mutex_lock() is blocking when dlopen run from a .so: cthreads uses both
  // both static and dynamic linking, which is really confusing to our code
  // -> we don't open libpthread but we get its symbol
  pthread := dlopen('libpthread.so.0', RTLD_LAZY);
  if pthread <> nil then
    @pthread_setname_np := dlsym(pthread, 'pthread_setname_np');
  {$endif OSPTHREADS}
  // redirect some syscall FPC RTL functions to faster vDSO libc variant
  {$ifdef CPUARM3264}
  StrLen := @StrLenLibc; // libc version is faster than plain pascal or RTL code
  MoveFast := @MoveFastLibC;
  FillCharFast := @FillCharLibC;
  {$ifdef CPUAARCH64}
  if ahcCrc32 in CpuFeatures then
    try
      if (crc32cby4arm64(0, 1) = 3712330424) and
         (crc32carm64(0, @SystemInfo, SizeOf(SystemInfo)) =
          crc32cfast(0, @SystemInfo, SizeOf(SystemInfo))) then
      begin
        crc32c := @crc32carm64;
        DefaultHasher := @crc32carm64;
        InterningHasher := @crc32carm64;
        crc32cby4 := @crc32cby4arm64;
        crcblock := @crc32blockarm64;
        crcblocks := @crc32blocksarm64;
      end;
    except
      exclude(CpuFeatures, ahcCrc32); // crc32 was actually not supported
    end;
  {$endif CPUAARCH64}
  {$endif CPUARM3264}
  {$ifdef CPUX64}
  {$ifdef OSLINUX}
  {$ifndef NOPATCHRTL}
  {$ifndef FPC_USE_LIBC}
  // will avoid syscall e.g. for events timeout in cthreads.pp
  RedirectCode(@Linux.clock_gettime, @clock_gettime_c);
  RedirectCode(@fpgettimeofday, @gettimeofday_c);
  {$endif FPC_USE_LIBC}
  {$endif NOPATCHRTL}
  {$endif OSLINUX}
  {$endif CPUX64}
  {$endif OSPOSIX}
  {$endif OSDARWIN}
end;

procedure FinalizeSpecificUnit;
begin
  {$ifdef OSPTHREADS}
  if pthread <> nil then
    dlclose(pthread);
  {$endif OSPTHREADS}
  {$ifdef OSLINUX} // systemd API is Linux-specific
  sd.Done;
  {$endif OSLINUX}
  icu.Done;
  SynDaemonInterceptLog := nil;
end;


