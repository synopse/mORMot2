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
  errors,
  termio,
  dl;

{$ifdef UNICODE}
  'mORMot assumes no UNICODE on POSIX, i.e. as TFileName = PChar = PUTF8Char'
{$endif UNICODE}


{ ****************** Unicode, Time, File process }
var
  /// contains the curent system code page (default UTF-8 on POSIX systems)
  GetACP: integer = CP_UTF8;

  /// contains the current Linux kernel revision, as one 24-bit integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;


{ TICULibrary }

procedure TICULibrary.Done;
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

function TICULibrary.IsAvailable: boolean;
begin
  if not Loaded then
    DoLoad;
  result := Assigned(ucnv_open);
end;

procedure TICULibrary.DoLoad(const LibName: TFileName; const Version: string);
const
  NAMES: array[0..12] of string = (
    'ucnv_open', 'ucnv_close', 'ucnv_setSubstChars', 'ucnv_setFallback',
    'ucnv_fromUChars', 'ucnv_toUChars', 'u_strToUpper', 'u_strToLower',
    'u_strCompare', 'u_strCaseCompare', 'u_getDataDirectory',
    'u_setDataDirectory', 'u_init');
  {$ifdef ANDROID}
  // from https://developer.android.com/guide/topics/resources/internationalization
  ICU_VER: array[1..13] of string = (
    '_3_8', '_4_2', '_44', '_46', '_48', '_50', '_51', '_53', '_55',
    '_56', '_58', '_60', '_63');
  SYSDATA: PAnsiChar = '/system/usr/icu';
  {$else}
  SYSDATA: PAnsiChar = '';
  {$endif ANDROID}
var
  i, j: integer;
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
      {$ifdef DARWIN}
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
      {$endif DARWIN}
      if icui18n = nil then
      begin
        DisplayFatalError('ICU is not available on this system',
          'Put cwstring in your uses clause as fallback');
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
      {$ifdef ANDROID}
      for i := high(ICU_VER) downto 1 do
      begin
        if dlsym(icu, pointer(NAMES[0] + ICU_VER[i])) <> nil then
        begin
          vers := ICU_VER[i];
          break;
        end;
      end;
      if vers <> '' then
      {$endif ANDROID}
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

function TICULibrary.ForceLoad(const LibName: TFileName; const Version: string): boolean;
begin
  Done;
  Loaded := false;
  DoLoad(LibName, Version);
  result := Assigned(ucnv_open);
end;

function TICULibrary.ucnv(codepage: cardinal): pointer;
var
  s: shortstring;
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
  AL: PtrInt): PtrInt;
var
  tmp: UnicodeString;
begin
  widestringmanager.Ansi2UnicodeMoveProc(A, CodePage, tmp, AL);
  result := length(tmp);
  MoveFast(pointer(tmp)^, W^, result * 2);
end;

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar;
  LA, LW, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if codepage = CP_UTF8 then
    exit(Utf8ToUnicode(W, A, LA));
  cnv := icu.ucnv(codepage);
  if cnv = nil then
    exit(AnsiToWideRTL(codepage, A, W, LA));
  err := 0;
  result := icu.ucnv_toUChars(cnv, W, LW, A, LA, err);
  if result < 0 then
    result := 0;
  icu.ucnv_close(cnv);
end;

function WideToAnsiRTL(CodePage: cardinal; W: PWideChar; A: PAnsiChar;
  WL: PtrInt): PtrInt;
var
  tmp: RawByteString;
begin
  widestringmanager.Unicode2AnsiMoveProc(W, tmp, CodePage, WL);
  result := length(tmp);
  MoveFast(pointer(tmp)^, A^, result);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar;
  LW, LA, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if CodePage = CP_UTF8 then
    exit(UnicodeToUTF8(A, W, LW));
  cnv := icu.ucnv(CodePage);
  if cnv = nil then
    exit(WideToAnsiRTL(CodePage, W, A, LW));
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
    err := 0;
    result := icu.u_strToUpper(W, WLen, W, WLen, nil, err);
  end
  else
  begin
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
    err := 0;
    result := icu.u_strToLower(W, WLen, W, WLen, nil, err);
  end
  else
  begin
    for i := 0 to WLen - 1 do
      if ord(W[i]) in [ord('A')..ord('Z')] then
        inc(W[i], 32);
    result := WLen;
  end;
end;

function GetDesktopWindow: PtrInt;
begin
  result := 0; // fixed result on a window-less system
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

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): integer;
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

function clock_gettime(ID: cardinal; r: ptimespec): integer;
  cdecl external 'libc.so' name 'clock_gettime';

function clock_getres(ID: cardinal; r: ptimespec): integer;
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
  cdecl; external 'c' name 'clock_gettime';

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
  clock_gettime(CLOCK_REALTIME_FAST, @r); // likely = CLOCK_REALTIME_COARSE
  // convert from nanoseconds into milliseconds
  result := PtrUInt(r.tv_nsec) div MILLION + (r.tv_sec * THOUSAND);
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
  // convert from nanoseconds into microseconds
  value := PtrUInt(r.tv_nsec) div THOUSAND + r.tv_sec * MILLION;
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

{$endif DARWIN}

function UnixMSTimeUTCFast: Int64;
begin
  result := UnixMSTimeUTC;
end;

var
  {%H-}pthread: pointer;
  {$ifdef LINUXNOTBSD} // see https://stackoverflow.com/a/7989973
  pthread_setname_np: function(thread: pointer; name: PAnsiChar): integer; cdecl;
  {$endif LINUXNOTBSD}

procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);
var
  // truncated to 16 non space chars (including #0)
  trunc: array[0..15] of AnsiChar;
  i, L: integer;
begin
  if Name = '' then
    exit;
  L := 0; // trim unrelevant spaces and prefixes when filling the 16 chars
  i := 1;
  if Name[1] = 'T' then
    if PCardinal(Name)^ =
         ord('T') + ord('S') shl 8 + ord('Q') shl 16 + ord('L') shl 24 then
      i := 5
    else
      i := 2;
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
  {$ifdef LINUXNOTBSD}
  if Assigned(pthread_setname_np) then
    pthread_setname_np(pointer(ThreadID), @trunc[0]);
  {$endif LINUXNOTBSD}
end;

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUTF8);
begin
  if ThreadID <> MainThreadID then // don't change the main process name
    SetUnixThreadName(ThreadID, Name); // call pthread_setname_np()
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUTF8CharDynArray): cardinal;
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

function FileSetDateFrom(const Dest: TFileName; SourceHandle: integer): boolean;
begin
  result := FileSetDate(Dest, FileGetDate(SourceHandle)) = 0;
end;

function FileSetDateFromWindowsTime(const Dest: TFileName;
  WinTime: integer): boolean;
var
  date, time: TDateTime;
begin
  with PLongRec(@WinTime)^ do
  result :=
    TryEncodeDate(Hi shr 9 + 1980, Hi shr 5 and 15, Hi and 31, date) and
    TryEncodeTime(Lo shr 11, Lo shr 5 and 63, Lo and 31 shl 1, 0, time) and
    (FileSetDate(Dest, DateTimeToFileDate(date + time)) = 0);
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
  {$ifdef OPENBSD}
  if (lp.st_birthtime <> 0) and
     (lp.st_birthtime < lp.st_ctime) then
    lp.st_ctime := lp.st_birthtime;
  {$endif OPENBSD}
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
  SourceF, DestF: TFileStream;
begin
  result := false;
  if FileExists(Target) then
    if FailIfExists then
      exit
    else
      DeleteFile(Target);
  try
    SourceF := TFileStream.Create(Source, fmOpenRead);
    try
      DestF := TFileStream.Create(Target, fmCreate);
      try
        DestF.CopyFrom(SourceF, SourceF.Size);
      finally
        DestF.Free;
      end;
      FileSetDateFrom(Target, SourceF.Handle);
    finally
      SourceF.Free;
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
  d: PUTF8Char;
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
  // let's assume UTF-8 under Linux/POSIX
  result := UTF8Encode(OemToUnicode(oem));
end;

{$I-}
procedure DisplayFatalError(const title, msg: RawUTF8);
begin
  writeln(StdErr, title);
  writeln(StdErr, msg);
  ioresult;
end;
{$I+}

function FileOpenSequentialRead(const FileName: string): integer;
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

function GetErrorText(error: integer): RawUTF8;
begin
  result := StrError(error);
end;

function TMemoryMap.DoMap(aCustomOffset: Int64): boolean;
begin
  if aCustomOffset <> 0 then
    if (aCustomOffset and (SystemInfo.dwPageSize - 1)) <> 0 then
      raise EOSException.CreateFmt(
        'DoMap(aCustomOffset=%d) with dwPageSize=%d',
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
  begin
    result := max; // already to the expected value
    exit;
  end;
  if hard then
    limit.rlim_max := max
  else
    limit.rlim_cur := max;
  if fpsetrlimit(RLIMIT_NOFILE, @limit) = 0 then
    result := GetFileOpenLimit(hard);
end;


{$ifdef LINUXNOTBSD} { the systemd API is Linux-specific }

{ TSystemDAPI }

procedure TSystemDAPI.DoLoad;
var
  p: PPointer;
  i, j: integer;
const
  NAMES: array[0..5] of PAnsiChar = (
    'sd_listen_fds', 'sd_is_socket_unix', 'sd_journal_print',
    'sd_journal_sendv', 'sd_notify', 'sd_watchdog_enabled');
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

function TSystemDAPI.IsAvailable: boolean;
begin
  if not tested then
    DoLoad;
  result := Assigned(listen_fds);
end;

function TSystemDAPI.ProcessIsStartedBySystemd: boolean;
begin
  result := IsAvailable and
    // note: for example on Ubuntu 20.04 INVOCATION_ID is always defined
    // from the other side PPID 1 can be in case we run under docker of started
    // by init.d so let's verify both
    (fpgetppid() = 1) and
    (fpGetenv(ENV_INVOCATION_ID) <> nil);
end;

procedure TSystemDAPI.Done;
begin
  if systemd <> nil then
  begin
    dlclose(systemd);
    systemd := nil;
  end;
end;

{$endif LINUXNOTBSD}


// define some raw text functions, to avoid linking mormot.core.text

function IdemPChar(p, up: PUTF8Char): boolean; inline;
var
  c, u: AnsiChar;
begin
  result := false;
  if (p = nil) or
     (up = nil) then
    exit;
  repeat
    u := up^;
    if u = #0 then
      break;
    inc(up);
    c := p^;
    inc(p);
    if (c >= 'a') and
       (c <= 'z') then
      dec(c, 32);
    if c <> u then
      exit;
  until false;
  result := true;
end;

function IdemPChars(const s: RawUTF8; const up: array of PUTF8Char): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to high(up) do
    if IdemPChar(pointer(s), up[i]) then
      exit;
  result := false;
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

function GetNextItem(var P: PAnsiChar): RawUTF8;
var
  c: cardinal;
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

function PosChar(Str: PUTF8Char; Chr: AnsiChar): PUTF8Char; inline;
begin
  result := nil;
  repeat
    if Str^ = #0 then
      exit;
    if Str^ = Chr then
      break;
    inc(Str);
  until false;
  result := Str;
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
             // may not be writable by not root on POSIX
            _LogPath := '/var/log/'
          else if IsDirectoryWritable(ExeVersion.ProgramFilePath) then
            _LogPath := ExeVersion.ProgramFilePath
          else
            _LogPath := GetSystemPath(spUserData);
        result := _LogPath;
      end;
    spUserData:
      begin
        if _UserPath = '' then
        begin
          //  ~/.cache/appname
          _UserPath := GetEnvironmentVariable('XDG_CACHE_HOME');
          if (_UserPath = '') or
             not IsDirectoryWritable(_UserPath) then
            _UserPath := EnsureDirectoryExists(
              GetSystemPath(spUserDocuments) + '.cache');
          _UserPath := EnsureDirectoryExists(
            _UserPath + TFileName(ExeVersion.ProgramName));
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
      if _HomePath = '' then
        // POSIX requires a value for $HOME
        _HomePath := IncludeTrailingPathDelimiter(
          GetEnvironmentVariable('HOME'));
      result := _HomePath;
    end;
  end;
end;

{$ifdef BSD}
// https://kaashif.co.uk/2015/06/18/how-to-get-a-list-of-processes-on-openbsd-in-c/

function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;
begin
  result := nil;
end;

function EnumProcessName(PID: cardinal): RawUTF8;
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

function EnumAllProcesses(out Count: cardinal): TCardinalDynArray;
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
    if (e.d_type = DT_UNKNOWN) or
       (e.d_type = DT_DIR) or
       not (e.d_name[0] in ['1'..'9']) then
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

function EnumProcessName(PID: cardinal): RawUTF8;
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
  P: PUTF8Char;
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
  Kernel := SimpleRoundTo2Digits((K * 100) / S);
  User := SimpleRoundTo2Digits((U * 100) / S);
  Idle := 100 - Kernel - User; // ensure sum is always 100%
  result := S <> 0;
end; { TODO : use a diff approach for TProcessInfo.PerSystem on Linux }

{$ifdef BSD}
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
begin
  FillCharFast(info, SizeOf(info), 0);
  info.memtotal := fpsysctlhwint(
    {$ifdef DARWIN}HW_MEMSIZE{$else}HW_PHYSMEM{$endif});
  info.memfree := info.memtotal - fpsysctlhwint(HW_USERMEM);
  if info.memtotal <> 0 then
  begin
    // avoid div per 0 exception
    info.percent := ((info.memtotal - info.memfree) * 100) div info.memtotal;
    result := true;
  end
  else
    result := false;
end;
{$else}
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
var
  si: TSysInfo; // Linuxism
  P: PUTF8Char;
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
{$endif BSD}

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
  {$ifdef BSD}
  POSIXMOUNTPOINTS = '/etc/mtab';
  {$else}
  POSIXMOUNTPOINTS = '/proc/self/mounts';
  {$endif BSD}

function GetDiskPartitions: TDiskPartitions;
var
  mounts, fs, mnt, typ: RawUTF8;
  p: PUTF8Char;
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
        SetLength(result, n + 1);
        result[n].name := fs;
        result[n].mounted := fn;
        result[n].size := tot;
        inc(n);
      end;
    end;
    p := GotoNextLine(p);
  until p = nil;
end;

{$ifdef BSD}
function mprotect(Addr: Pointer; Len: size_t; Prot: integer): integer;
  {$ifdef Darwin} cdecl external 'libc.dylib' name 'mprotect';
  {$else} cdecl external 'libc.so' name 'mprotect'; {$endif}
{$endif BSD}

function SynMProtect(addr: pointer; size: size_t; prot: integer): integer;
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

{$ifdef CPUARM}

var
  StubCallAllocMemLastStart: PtrUInt; // avoid unneeded fpmmap() calls

function StubCallAllocMem(const Size, flProtect: DWORD): pointer;
const
  STUB_RELJMP = {$ifdef CPUARM} $7fffff {$else} $7fffffff {$endif}; // relative jmp
  STUB_INTERV = STUB_RELJMP+1; // try to reserve in closed stub interval
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

// other platforms (Intel+Arm64) use plain Kernel call
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
  SynDaemonInterceptLog: TOnDaemonLog;

procedure DoShutDown(Sig: integer; Info: PSigInfo; Context: PSigContext); cdecl;
var
  level: TSynLogInfo;
  si_code: integer;
  text: shortstring; // code below has no memory allocation
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

procedure SynDaemonIntercept(const onlog: TOnDaemonLog);
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

function RunUntilSigTerminatedPidFile: TFileName;
begin
  result := Format('%s.%s.pid',
     [ExeVersion.ProgramFilePath, ExeVersion.ProgramName]);
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
    // no such process -> try to delete the .pid file
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
  const start, stop: TThreadMethod; const onlog: TOnDaemonLog;
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
          [daemon.ClassName, ExeVersion.ProgramName, PtrInt(pid)]);
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
    // create local .[ExeVersion.ProgramName].pid file
    pid := fpgetpid;
    str(pid, s);
    FileFromString(s, pidfilename);
  end;
  try
    if Assigned(onlog) then
      onlog(sllNewRun, 'Start % /% %',
        [servicename, TXT[dofork], ExeVersion.Version.DetailedOrVoid], nil);
    start;
    while SynDaemonTerminated = 0 do
      fpPause;
  finally
    if Assigned(onlog) then
      onlog(sllNewRun, 'Stop /% from Sig=%',
        [TXT[dofork], SynDaemonTerminated], nil);
    try
      stop;
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
  {$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
  pid := FpvFork;
  {$else}
  pid := FpFork;
  {$ifend}
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
  temp: RawUTF8;
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
    result := {$ifdef FPCLINUXNOTBSD} -ESysELIBBAD {$else} -80 {$endif}
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

function getpagesize: integer; cdecl; external 'c';

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
  SystemInfo.dwPageSize := getpagesize; // call libc API
  fpuname(uts);
  {$ifdef BSD}
  SystemInfo.dwNumberOfProcessors := fpsysctlhwint(HW_NCPU);
  beg := fpsysctlhwstr(HW_MACHINE, temp);
  FastSetString(BiosInfoText, beg, StrLen(beg));
  modname := fpsysctlhwstr(HW_MODEL, temp);
  with uts do
    OSVersionText := sysname + '-' + release + ' ' + version;
  {$else}
  {$ifndef ANDROID}
  pthread := dlopen('libpthread.so.0', RTLD_LAZY);
  if pthread <> nil then
  begin
    {$ifdef LINUXNOTBSD}
    @pthread_setname_np := dlsym(pthread, 'pthread_setname_np');
    {$endif LINUXNOTBSD}
  end;
  {$endif ANDROID}
  prod := TrimU(StringFromFile('/sys/class/dmi/id/product_name', true));
  if prod <> '' then
  begin
    prodver := TrimU(StringFromFile('/sys/class/dmi/id/product_version', true));
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
    until (modname^ = #0) or
          (modname^ > ' ');
  FindNameValue(StringFromFile('/etc/os-release'), 'PRETTY_NAME=', release);
  if (release <> '') and
     (release[1] = '"') then
    release := copy(release, 2, length(release) - 2);
  release := TrimU(release);
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
  SystemInfo.release := release;
  {$endif BSD}
  SystemInfo.uts.release := uts.Release;
  SystemInfo.uts.sysname := uts.Sysname;
  SystemInfo.uts.version := uts.Version;
  P := @uts.release[0];
  KernelRevision := GetNextCardinal(P) shl 16 +
                    GetNextCardinal(P) shl 8 +
                    GetNextCardinal(P);
  OSVersionInt32 := integer(KernelRevision shl 8) +
                    ord(OS_KIND);
  with SystemInfo.uts do
    OSVersionText := sysname + ' ' + release;
  if SystemInfo.release <> '' then
    OSVersionText := SystemInfo.release + ' - ' + OSVersionText;
  {$ifdef ANDROID}
  OSVersionText := 'Android (' + OSVersionText + ')';
  {$endif ANDROID}
  if (SystemInfo.dwNumberOfProcessors > 0) and
     (modname <> nil) then
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
  mach_timenanosecond := (mach_timeinfo.Numer = 1) and
                         (mach_timeinfo.Denom = 1);
  {$else}
  {$ifdef LINUX}
  // try Linux kernel 2.6.32+ or FreeBSD 8.1+ fastest clocks
  if clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0 then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0 then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  if (clock_gettime(CLOCK_REALTIME_FAST, @tp) <> 0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST, @tp) <> 0) then
    raise EOSException.CreateFmt(
      'clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release), fpgeterrno]);
  {$endif LINUX}
  {$endif DARWIN}
end;

procedure FinalizeSpecificUnit;
begin
  if pthread <> nil then
    dlclose(pthread);
  icu.Done;
  {$ifdef LINUXNOTBSD} { the systemd API is Linux-specific }
  sd.Done;
  {$endif LINUXNOTBSD}
end;


