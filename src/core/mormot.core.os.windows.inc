{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Windows API calls for FPC/Delphi, as used by mormot.core.os.pas
}

uses
  mormot.core.text;

{ ****************** Unicode, Time, File process }

const
  DefaultCharVar: AnsiChar = '?';

function os_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
begin
  result := MultiByteToWideChar(CodePage, MB_PRECOMPOSED, A, LA, W, LW);
end;

function os_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
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
  nano100: Int64;
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
  nano100: Int64;
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
  GetSystemTimeAsFileTime(ft); // very fast, with 100 ns unit
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

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  result := not IsZero(@cs, SizeOf(cs));
end;

var
  os_QueryPerformanceFrequency: QWord; // is documented as stable after boot

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  cnt: QWord;
begin
  QueryPerformanceCounter(PInt64(@cnt)^);
  Value := (cnt * 1000000) div os_QueryPerformanceFrequency;
end;

{$ifndef DELPHI5OROLDER}
{$WARN SYMBOL_DEPRECATED OFF} // for faVolumeID
{$endif}
const
  faInvalidFile   = faDirectory + faVolumeID + faSysFile + faHidden;
  faDirectoryMask = faDirectory + faHidden;
{$ifndef DELPHI5OROLDER}
{$WARN SYMBOL_DEPRECATED ON}
{$endif}

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  ST, LT: TSystemTime;
begin // 5 times faster than CreateFile, GetFileSizeEx, CloseHandle
  if GetFileAttributesEx(pointer(FileName), GetFileExInfoStandard, @FA) and
     FileTimeToSystemTime(FA.ftLastWriteTime, ST) and
     SystemTimeToTzSpecificLocalTime(nil, ST, LT) then
    result := SystemTimeToDateTime(LT)
  else
    result := 0;
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

function FileSize(const FileName: TFileName): Int64;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin // 5 times faster than CreateFile, GetFileSizeEx, CloseHandle
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

function DeleteFile(const aFileName: TFileName): boolean;
begin
  result := SysUtils.DeleteFile(aFileName);
end;

function RenameFile(const OldName, NewName: TFileName): boolean;
begin
  result := SysUtils.RenameFile(OldName,NewName);
end;

function FileOpenSequentialRead(const FileName: string): Integer;
begin
  if OSVersion>=wVista then // don't use the flag on XP
    result := CreateFile(pointer(FileName), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, // same as fmShareDenyNone
      OPEN_EXISTING,FILE_FLAG_SEQUENTIAL_SCAN, 0)
  else
    result := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
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
end; // warning: FPC's GetTickCount64 doesn't handle 49 days wrap :(

{$ifdef FPC} // oddly not defined in fpc\rtl\win
function SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
{$endif FPC}

procedure SleepHiRes(ms: cardinal);
begin
  if (ms <> 0) or not SwitchToThread then
    Windows.Sleep(ms);
end;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{ TWinRegistry }

function TWinRegistry.ReadOpen(root: HKEY; const keyname: RawUTF8; closefirst: boolean): boolean;
var
  tmp: TSynTempBuffer;
begin
  if closefirst then
    Close;
  tmp.Init(length(keyname) * 2);
  UTF8ToWideChar(tmp.buf, pointer(keyname));
  key := 0;
  result := RegOpenKeyExW(root, tmp.buf, 0, KEY_READ, key) = 0;
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
var
  rsize: DWORD;
begin
  rsize := 4;
  if RegQueryValueExW(key, pointer(entry), nil, nil, @result, @rsize) <> 0 then
    result := 0;
end;

function TWinRegistry.ReadQword(const entry: SynUnicode): QWord;
var
  rsize: DWORD;
begin
  rsize := 8;
  if RegQueryValueExW(key, pointer(entry), nil, nil, @result, @rsize) <> 0 then
    result := 0;
end;

function TWinRegistry.ReadEnumEntries: TRawUTF8DynArray;
var
  count, maxlen, i, len: DWORD;
  tmp: TSynTempBuffer;
begin
  result := nil;
  if (RegQueryInfoKeyW(key, nil, nil, nil, @count, @maxlen, nil, nil, nil, nil, nil, nil) <> 0) or
     (count = 0) then
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
  // initialize mormot.core.text to avoid GPF in this function
  InitializeTextConstants;
  // late-binding of newest Windows APIs
  h := GetModuleHandle(kernel32);
  GetTickCount64 := GetProcAddress(h, 'GetTickCount64');
  if not Assigned(GetTickCount64) then // WinXP+
    GetTickCount64 := @GetTickCount64ForXP;
  GetSystemTimePreciseAsFileTime := GetProcAddress(h, 'GetSystemTimePreciseAsFileTime');
  if not Assigned(GetSystemTimePreciseAsFileTime) then // Win8+
    GetSystemTimePreciseAsFileTime := @GetSystemTimeAsFileTime;
  QueryPerformanceFrequency(PInt64(@os_QueryPerformanceFrequency)^);
  if os_QueryPerformanceFrequency = 0 then
    raise Exception.Create('QueryPerformanceFrequency=0'); // paranoid
  IsWow64Process := GetProcAddress(h, 'IsWow64Process');
  Res := false;
  IsWow64 := Assigned(IsWow64Process) and IsWow64Process(GetCurrentProcess, Res) and Res;
  FillcharFast(SystemInfo, SizeOf(SystemInfo), 0);
  if IsWow64 then // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(h, 'GetNativeSystemInfo')
  else
    @GetNativeSystemInfo := nil;
  // retrieve system information
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
  with OSVersionInfo do  // see https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833
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
        Vers := wServer2019_64; // https://stackoverflow.com/q/53393150
    end;
    if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) and (Vers < wServer2019_64) then
      inc(Vers);   // e.g. wEight -> wEight64
  end;
  OSVersion := Vers;
  with OSVersionInfo do
    if wServicePackMajor = 0 then
      FormatUTF8('Windows % (%.%.%)', [WINDOWS_NAME[Vers], dwMajorVersion, dwMinorVersion, dwBuildNumber], OSVersionText)
    else
      FormatUTF8('Windows % SP% (%.%.%)', [WINDOWS_NAME[Vers], wServicePackMajor, dwMajorVersion, dwMinorVersion, dwBuildNumber], OSVersionText);
  OSVersionInt32 := (integer(Vers) shl 8) + ord(osWindows);
  // retrieve Hardware information from Registry
  if reg.ReadOpen(HKEY_LOCAL_MACHINE, 'Hardware\Description\System\CentralProcessor\0') then
  begin
    cpu := reg.ReadString('ProcessorNameString');
    if cpu = '' then
      cpu := reg.ReadString('Identifier');
  end;
  if reg.ReadOpen(HKEY_LOCAL_MACHINE, 'Hardware\Description\System\BIOS', true) then
  begin
    manuf := reg.ReadString('SystemManufacturer');
    if manuf <> '' then
      manuf := manuf + ' ';
    prod := reg.ReadString('SystemProductName');
    prodver := reg.ReadString('SystemVersion');
    if prodver = '' then
      prodver := reg.ReadString('BIOSVersion');
  end;
  if (prod = '') or (prodver = '') then
  begin
    if reg.ReadOpen(HKEY_LOCAL_MACHINE, 'Hardware\Description\System', true) then
    begin
      if prod = '' then
        prod := reg.ReadString('SystemBiosVersion');
      if prodver = '' then
        prodver := reg.ReadString('VideoBiosVersion');
    end;
  end;
  reg.Close;
  if prodver <> '' then
    FormatUTF8('%% %', [manuf, prod, prodver], BiosInfoText)
  else
    FormatUTF8('%%', [manuf, prod], BiosInfoText);
  if cpu = '' then
    cpu := StringToUTF8(GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
  cpu := Trim(cpu);
  FormatUTF8('% x % (' + CPU_ARCH_TEXT + ')', [SystemInfo.dwNumberOfProcessors, cpu], CpuInfoText);
  h := LoadLibrary('ntdll.dll');
  if h > 0 then
  begin
    wine_get_version := GetProcAddress(h, 'wine_get_version');
    if Assigned(wine_get_version) then
      OSVersionInfoEx := trim('Wine ' + trim(wine_get_version));
    FreeLibrary(h);
  end;
  if OSVersionInfoEx <> '' then
    OSVersionText := FormatUTF8('% - %', [OSVersionText, OSVersionInfoEx]);
end;


