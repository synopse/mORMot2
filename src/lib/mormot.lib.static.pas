/// low-level definitions needed to link static binaries
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.static;


{
  *****************************************************************************

   Ease Link Static Files, especially for FPC/GCC
   - Some Linking Constants
   - Link Dependencies
   - GCC Math Functions
   - Minimal libc Replacement for Windows
   - Cross-Platform FPU Exceptions Masking

  *****************************************************************************

  Please download latest https://synopse.info/files/mormot2static.7z and
  extract its content to the /static sub-folder of the mORMot 2 repository.

}

interface

{$I ..\mormot.defines.inc}

uses
  SysUtils,
  math,
  mormot.core.base,
  mormot.core.os;

{ ****************** Some Linking Constants }

const
{$ifdef OSWINDOWS}
  {$ifdef CPU64}
    _PREFIX = '';
  {$else}
    _PREFIX = '_';
  {$endif CPU64}
{$else}
  {$ifdef OSDARWIN}
    _PREFIX = '_';
  {$else}
    _PREFIX = ''; // other POSIX systems don't haveany trailing underscore
  {$endif OSDARWIN}
{$endif OSWINDOWS}



{ ********************** Minimal libc Replacement for Windows }

{$ifdef OSWINDOWS}

// some public symbols for Delphi proper linking
// e.g. for mormot.db.raw.sqlite3.static.pas and mormot.lib.quickjs.pas
// (static linking is sadly local to each unit with Delphi)

const
  _CLIB = 'msvcrt.dll'; // redirect to the built-in Microsoft "libc"

type
  qsort_compare_func = function(P1, P2: Pointer): Integer; cdecl;

procedure libc_qsort(baseP: PByte; NElem, Width: PtrInt; comparF: qsort_compare_func); cdecl;
function libc_rename(oldname, newname: PUtf8Char): integer; cdecl;
function libc_printf(format: PAnsiChar): Integer; cdecl; varargs;
function libc_sprintf(buf: Pointer; format: PAnsiChar): Integer; cdecl; varargs;
function libc_fprintf(fHandle: pointer; format: PAnsiChar): Integer; cdecl; varargs;
function libc_vsnprintf(buf: pointer; nzize: PtrInt; format: PAnsiChar;
  param: pointer): integer; cdecl;
function libc_strcspn(str, reject: PUtf8Char): integer; cdecl;
function libc_strcat(dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
function libc_strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
function libc_strncmp(p1, p2: PByte; Size: integer): integer; cdecl;
function libc_memchr(s: Pointer; c: Integer; n: PtrInt): Pointer; cdecl;
function libc_memcmp(p1, p2: PByte; Size: integer): integer; cdecl;
function libc_strchr(s: Pointer; c: Integer): Pointer; cdecl;
function libc_strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
function libc_write(handle: Integer; buf: Pointer; len: LongWord): Integer; cdecl;
function libc_log(d: double): double; cdecl;

type
  timeval = record
    sec: integer; // defined as long in Windows SDK
    usec: integer;
  end;

  time_t = packed record
    tm_sec: integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: integer;            { Minutes.      [0-59]  }
    tm_hour: integer;           { Hours.        [0-23]  }
    tm_mday: integer;           { Day.          [1-31]  }
    tm_mon: integer;            { Month.        [0-11]  }
    tm_year: integer;           { Year          - 1900. }
    tm_wday: integer;           { Day of week.  [0-6]   }
    tm_yday: integer;           { Days in year. [0-365] }
    tm_isdst: integer;          { DST.          [-1/0/1]}
    __tm_gmtoff: integer;       { Seconds east of UTC  }
    __tm_zone: PChar;           { Timezone abbreviation}
  end;

function localtime64_s(var t: Int64; var atm: time_t): integer; cdecl;
function localtime32_s(var t: cardinal; var atm: time_t): integer; cdecl;
function putchar(c: integer): integer; cdecl;
function gettimeofday(var tv: timeval; zone: pointer): integer; cdecl;
function fputc(c: integer; f: pointer): integer; cdecl;
function fwrite(buf: pointer; size, count: PtrInt; f: pointer): integer; cdecl;
function strrchr(s: PUtf8Char; c: AnsiChar): PUtf8Char; cdecl;
function moddi3(num, den: int64): int64; cdecl;
function umoddi3(num, den: uint64): uint64; cdecl;
function divdi3(num, den: int64): int64; cdecl;
function udivdi3(num, den: uint64): uint64; cdecl;
function udivmoddi4(a, b: UInt64; var c: UInt64): UInt64; cdecl;
procedure __chkstk_ms;

{$ifdef CPUX64}

procedure __udivti3;
procedure __udivmodti4;
procedure __divti3;
procedure __umodti3;

{$endif CPUX64}

{$endif OSWINDOWS}

/// calls setlocale(LC_NUMERIC, 'C') to force to use the C default locale
// - is mandatory e.g. for mormot.lib.quickjs to properly parse float values
// - on Windows, redirects to msvcrt.dll's setlocale() API
procedure SetLibcNumericLocale;


{ ********************** Cross-Platform FPU Exceptions Masking }

type
  TFpuFlags = (
    ffLibrary,
    ffPascal);

{$ifdef CPUINTEL}

var
  /// direct efficient x87 / SSE2 FPU flags for rounding and exceptions
  _FPUFLAGS: array[TFpuFlags] of cardinal = (
    {$ifdef CPU64}
      $1FA0, $1920);
    {$else}
      $137F, $1372);
    {$endif CPU64}

{$else}

var
  /// on non Intel/AMD, use slower but cross-platform RTL Math unit
  // - defined as var for runtime customization
  _FPUFLAGS: array[TFpuFlags] of TFPUExceptionMask = (
    // ffLibrary
    [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision],
    // ffPascal
    [exDenormalized, exUnderflow, exPrecision]);

{$endif CPUINTEL}

/// mask/unmask all FPU exceptions, according to the running CPU
// - returns the previous execption flags, for ResetFpuFlags() call
// - x87 flags are $1372 for pascal, or $137F for library
// - sse flags are $1920 for pascal, or $1FA0 for library
// - on non Intel/AMD CPUs, will use TFPUExceptionMask from the RTL Math unit
// - do nothing and return -1 if the supplied flags are the one already set
function SetFpuFlags(flags: TFpuFlags): cardinal;

/// restore the FPU exceptions flags as overriden by SetFpuFlags()
// - do nothing if the saved flags are the one already set, i.e. -1
procedure ResetFpuFlags(saved: cardinal);



implementation

{ ****************** Link Dependencies }

{$ifdef FPC}

{$ifdef OSWINDOWS}
  {$ifdef CPU64}
    {$linklib ..\..\static\x86_64-win64\libkernel32.a}
  {$else}
    {$linklib ..\..\static\i386-win32\libkernel32.a}
  {$endif CPU64}
{$endif OSWINDOWS}

{$ifdef OSANDROID}
  {$ifdef CPUAARCH64}
    {$linklib ..\..\static\aarch64-android\libgcc.a}
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
    {$linklib ..\..\static\arm-android\libgcc.a}
  {$endif CPUARM}
  {$ifdef CPUX64}
    {$linklib ..\..\static\x86_64-android\libgcc.a}
  {$endif CPUX64}
{$endif OSANDROID}

{$ifdef OSLINUX}
  {$ifdef CPUAARCH64}
    {$linklib ..\..\static\aarch64-linux\libgcc.a}
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
    {$linklib ..\..\static\arm-linux\libgcc.a}
  {$endif CPUARM}
{$endif OSLINUX}

{$endif FPC}


{ ****************** Reimplemented some C Functions in Pascal }

// QuickJS and lizard source e.g. was patched to call those heap functions
// warning: we can't replace malloc/free/realloc and link libc/libpthread/libdl

function pas_malloc(size: cardinal): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_malloc'; {$endif}
begin
  GetMem(result, size);
end;

function pas_calloc(n, size: PtrInt): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_calloc'; {$endif}
begin
  result := AllocMem(size * n);
end;

procedure pas_free(P: pointer); cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_free'; {$endif}
begin
  FreeMem(P);
end;

function pas_realloc(P: pointer; Size: integer): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_realloc'; {$endif}
begin
  ReallocMem(P, Size);
  result := P;
end;

function pas_malloc_usable_size(P: pointer): integer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_malloc_usable_size'; {$endif}
begin
  {$ifdef FPC}
  result := MemSize(P); // only available on FPC
  {$else}
  result := 0; // will reallocate each time - good enough with Delphi's FastMM4
  {$endif FPC}
end;

// see e.g. #define assert(x) in QuickJS cutils.h

type
  ELibStatic = class(Exception);

procedure pas_assertfailed(cond, fn: PAnsiChar; line: integer); cdecl;
 {$ifdef FPC} public name _PREFIX + 'pas_assertfailed'; {$endif}
begin
  raise ELibStatic.CreateFmt('Panic in %s:%d: assert(%s)', [fn, line, cond]);
end;


{ ********************** Minimal libc Replacement for Windows }

{$ifdef OSWINDOWS}

// SQlite3 is compiled with SQLITE_OMIT_LOCALTIME and SQLITE_NO_THREAD
// to ease static linking

function libc_rename(oldname, newname: PUtf8Char): integer; cdecl;
  external _CLIB name 'rename';
function libc_printf(format: PAnsiChar): Integer; cdecl; varargs;
  external _CLIB name 'printf';
function libc_sprintf(buf: Pointer; format: PAnsiChar): Integer; cdecl; varargs;
  external _CLIB name 'sprintf';
function libc_fprintf(fHandle: pointer; format: PAnsiChar): Integer; cdecl; varargs;
  external _CLIB name 'fprintf';
function libc_vsnprintf(buf: pointer; nzize: PtrInt; format: PAnsiChar;
   param: pointer): integer; cdecl;
  external _CLIB name '_vsnprintf';
function libc_strcspn(str, reject: PUtf8Char): integer; cdecl;
  external _CLIB name 'strcspn';
function libc_strcat(dest, src: PAnsiChar): PAnsiChar; cdecl;
  external _CLIB name 'strcat';
function libc_strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
  external _CLIB name 'strcpy';
function libc_strncmp(p1, p2: PByte; Size: integer): integer; cdecl;
  external _CLIB name 'strncmp';
function libc_memchr(s: Pointer; c: Integer; n: PtrInt): Pointer; cdecl;
  external _CLIB name 'memchr';
function libc_memcmp(p1, p2: PByte; Size: integer): integer; cdecl;
  external _CLIB name 'memcmp';
function libc_strchr(s: Pointer; c: Integer): Pointer; cdecl;
  external _CLIB name 'strchr';
function libc_strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
  external _CLIB name 'strtod';
function libc_write(handle: Integer; buf: Pointer; len: LongWord): Integer; cdecl;
  external _CLIB name '_write';
procedure libc_qsort(baseP: PByte; NElem, Width: PtrInt; comparF: qsort_compare_func); cdecl;
  external _CLIB name 'qsort';
function libc_log(d: double): double; cdecl;
  external _CLIB name 'log';
function libc_beginthreadex(security: pointer; stksize: cardinal;
  start, arg: pointer; flags: cardinal; var threadid: cardinal): THandle; cdecl;
  external _CLIB name '_beginthreadex';
procedure libc_endthreadex(exitcode: cardinal); cdecl;
  external _CLIB name '_endthreadex';

function localtime64_s(var t: Int64; var atm: time_t): integer;
var
 S: TSystemTime;
begin
  UnixTimeToLocalTime(t, S);
  atm.tm_sec := S.wSecond;
  atm.tm_min := S.wMinute;
  atm.tm_hour := S.wHour;
  atm.tm_mday := S.wDay;
  atm.tm_mon := S.wMonth - 1;
  atm.tm_year := S.wYear - 1900;
  atm.tm_wday := S.wDay;
  result := 0; // on Windows, should return 0; on POSIX, nil or @atm :(
end;

function localtime32_s(var t: cardinal; var atm: time_t): integer;
var
  t64: Int64;
begin
  t64 := t;
  result := localtime64_s(t64, atm);
end;

var
 { as standard C library documentation states:
   Statically allocated buffer, shared by the functions gmtime() and localtime().
   Each call of these functions overwrites the content of this structure.
   -> this buffer is shared, but SQlite3 will protect it with a mutex :) }
   atm: time_t;

function localtime32(t: PCardinal): pointer; cdecl;
begin
  localtime32_s(t^, atm);
  result := @atm;
end;

function localtime64(t: PInt64): pointer; cdecl;
begin
  localtime64_s(t^, atm);
  result := @atm;
end;

function putchar(c: integer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'putchar'; {$endif}
begin
  {$I-}
  write(AnsiChar(c));
  ioresult;
  {$I+}
  result := c;
end;

function gettimeofday(var tv: timeval; zone: pointer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'gettimeofday'; {$else} export; {$endif}
var
  now, sec: QWord;
begin
  now := UnixMSTimeUtcFast;
  sec := now div 1000;
  tv.sec := sec;
  tv.usec := (now - sec * 1000) * 1000;
  result := 0;
end;

function fputc(c: integer; f: pointer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fputc'; {$endif}
begin
  if libc_write(PtrInt(f), @c, 1) = 1 then
    result := c
  else
    result := 26;
end;

function strrchr(s: PUtf8Char; c: AnsiChar): PUtf8Char; cdecl;
 {$ifdef FPC} public name _PREFIX + 'strrchr'; {$endif}
begin
  // simple full pascal version of the standard C library function
  result := nil;
  if s <> nil then
    while s^ <> #0 do
    begin
      if s^ = c then
        result := s;
      inc(s);
    end;
end;

function fwrite(buf: pointer; size, count: PtrInt; f: pointer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fwrite'; {$endif}
begin
  result := libc_write(PtrInt(f), buf, size * count) div size;
end;

{$ifdef CPUX86}

// asm stubs to circumvent libgcc.a (cross)linking issues on Win32

procedure __chkstk_ms; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__chkstk_ms'; {$endif}
asm
        push    ecx
        push    eax
        cmp     eax, 4096
        lea     ecx, dword ptr [esp+0CH]
        jc      @@002
@@001:  sub     ecx, 4096
        or      dword ptr [ecx], 00H
        sub     eax, 4096
        cmp     eax, 4096
        ja      @@001
@@002:  sub     ecx, eax
        or      dword ptr [ecx], 00H
        pop     eax
        pop     ecx
end;

{$endif CPUX86}

{$ifdef CPUX64}

procedure __chkstk_ms; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '___chkstk_ms'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        push    rcx
        push    rax
        cmp     rax, 4096
        lea     rcx, qword ptr [rsp+18H]
        jc      @@002
@@001:  sub     rcx, 4096
        or      qword ptr [rcx], 00H
        sub     rax, 4096
        cmp     rax, 4096
        ja      @@001
@@002:  sub     rcx, rax
        or      qword ptr [rcx], 00H
        pop     rax
        pop     rcx
end;

{$endif CPUX64}

{$ifdef FPC}

// we can safely redirect c code to our pascal heap on FPC/Windows

function malloc(size: cardinal): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'malloc'; {$endif}
begin
  GetMem(result, size);
end;

function calloc(n, size: PtrInt): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'calloc'; {$endif}
begin
  result := AllocMem(size * n);
end;

procedure free(P: pointer); cdecl;
 {$ifdef FPC} public name _PREFIX + 'free'; {$endif}
begin
  FreeMem(P);
end;

function realloc(P: pointer; Size: integer): pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'realloc'; {$endif}
begin
  result := P;
  ReallocMem(result, Size);
end;

function malloc_usable_size(P: pointer): integer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'malloc_usable_size'; {$endif}
begin
  {$ifdef FPC}
  result := MemSize(P); // only available on FPC
  {$else}
  result := 0; // will reallocate each time - good enough with Delphi's FastMM4
  {$endif FPC}
end;

function memcpy(dest, src: Pointer; count: PtrInt): Pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'memcpy'; {$endif}
begin
  MoveFast(src^, dest^, count);
  result := dest;
end;

function memset(dest: Pointer; val: Integer; count: PtrInt): Pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'memset'; {$endif}
begin
  FillCharFast(dest^, count, val);
  result := dest;
end;

function memmove(dest, src: Pointer; count: PtrInt): Pointer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'memmove'; {$endif}
begin
  MoveFast(src^, dest^, count);
  result := dest;
end;

function strlen(p: PAnsiChar): integer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'strlen'; {$endif}
begin
  result := mormot.core.base.StrLen(P);
end;

function strcmp(p1, p2: PAnsiChar): integer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'strcmp'; {$endif}
begin
  // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := mormot.core.base.StrComp(p1, p2);
end;

function rename(oldname, newname: PUtf8Char): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'rename'; {$else} export; {$endif}
begin
  result := libc_rename(oldname, newname);
end;

{$ifdef FPC}
var
  // redirect mingw import pointer to libc_beginthreadex
  beginthreadex: pointer public name
    {$ifdef CPU32} '__imp___beginthreadex' {$else} '__imp__beginthreadex' {$endif};
  // redirect mingw import pointer to libc_endthreadex
  endthreadex: pointer public name
    {$ifdef CPU32} '__imp___endthreadex' {$else} '__imp__endthreadex' {$endif};
  // redirect mingw import pointer to our internal functions
  {$ifdef CPU32}
  imp_localtime32: pointer public name '__imp___localtime32';
  {$else}
  imp_localtime64: pointer public name '__imp__localtime64';
  {$endif CPU32}
{$endif FPC}

procedure __exit;
 {$ifdef FPC} public name _PREFIX + 'exit'; {$else} export; {$endif}
begin
  raise ELibStatic.Create('Unexpected exit() call');
end;

procedure printf; assembler; 
 {$ifdef FPC} nostackframe; public name _PREFIX + 'printf'; {$else} export; {$endif}
asm
  jmp libc_printf
end;

procedure sprintf; assembler;
 {$ifdef FPC} nostackframe; public name _PREFIX + 'sprintf'; {$else} export; {$endif}
asm
  jmp libc_sprintf
end;

procedure fprintf; assembler;
 {$ifdef FPC} nostackframe; public name _PREFIX + 'fprintf'; {$else} export; {$endif}
asm
  jmp libc_fprintf
end;

procedure _vsnprintf; assembler;
 {$ifdef FPC} nostackframe; public name _PREFIX + '__ms_vsnprintf'; {$else} export; {$endif}
asm
  jmp libc_vsnprintf
end;

function strcspn(str, reject: PUtf8Char): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'strcspn'; {$else} export; {$endif}
begin
  result := libc_strcspn(str, reject);
end;

function strcat(dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
  {$ifdef FPC} public name _PREFIX + 'strcat'; {$else} export; {$endif}
begin
  result := libc_strcat(dest, src);
end;

function strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
  {$ifdef FPC} public name _PREFIX + 'strcpy'; {$else} export; {$endif}
begin
  result := libc_strcpy(dest, src);
end;

function strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
  {$ifdef FPC} public name _PREFIX + '__strtod'; {$else} export; {$endif}
begin
  result := libc_strtod(value, endPtr);
end;

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'strncmp'; {$endif}
begin
  result := libc_strncmp(p1, p2, Size);
end;

function atoi(const str: PUtf8Char): PtrInt; cdecl;
  {$ifdef FPC} public name _PREFIX + 'atoi'; {$else} export; {$endif}
begin
  result := GetInteger(str);
end;

function memchr(s: Pointer; c: Integer; n: PtrInt): Pointer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'memchr'; {$else} export; {$endif}
begin
  result := libc_memchr(s, c, n);
end;

function memcmp(p1, p2: PByte; Size: integer): integer; cdecl;
 {$ifdef FPC} public name _PREFIX + 'memcmp'; {$endif}
begin
 result := libc_memcmp(p1, p2, Size);
end;

function strchr(s: PAnsiChar; c: Integer): Pointer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'strchr'; {$else} export; {$endif}
begin
  result := libc_strchr(s, c);
end;

procedure qsort(baseP: PByte; NElem, Width: PtrInt; comparF: qsort_compare_func); cdecl;
  {$ifdef FPC} public name _PREFIX + 'qsort'; {$endif}
begin
  libc_qsort(baseP, NElem, Width, comparF);
end;

{$ifdef CPUX86} // not a compiler intrinsic on x86

function _InterlockedCompareExchange(
  var Dest: integer; New, Comp: integer): longint; stdcall;
  public alias: '_InterlockedCompareExchange@12';
begin
  result := InterlockedCompareExchange(Dest, New, Comp);
end;

{$endif CPUX86}

{$endif FPC}

{$endif OSWINDOWS}

// see clocale.pp unit for those values

function setlocale(category: integer; locale: PAnsiChar): PAnsiChar; cdecl;
{$ifdef OSWINDOWS}
  external _CLIB name 'setlocale'; // redirect to msvcrt.dll
{$else}
  {$ifdef NETBSD}
  // NetBSD has a new setlocale function defined in /usr/include/locale.h
  external 'c' name '__setlocale_mb_len_max_32';
  {$else}
  external 'c' name 'setlocale'; // regular libc POSIX call
  {$endif NETBSD}
{$endif OSWINDOWS}

const
  LC_CTYPE = 0;
  LC_NUMERIC = 1;
  LC_ALL = 6;

procedure SetLibcNumericLocale;
begin
  setlocale(LC_NUMERIC, 'C');
end;


{ ****************** GCC Math Functions }

function fabs(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fabs'; {$endif}
begin
  result := abs(x);
end;

function sqrt(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'sqrt'; {$endif}
begin
  result := system.sqrt(x);
end;

function cbrt(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'cbrt'; {$endif}
begin
  result := exp( (1 / 3) * ln(x));
end;

function trunk(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'trunc'; {$endif}
begin
  result := trunc(x);
end;

function kos(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'cos'; {$endif}
begin
  result := cos(x);
end;

function cin(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'sin'; {$endif}
begin
  result := sin(x);
end;

function akos(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'acos'; {$endif}
begin
  result := arccos(x);
end;

function acin(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'asin'; {$endif}
begin
  result := arcsin(x);
end;

function axp(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'exp'; {$endif}
begin
  result := exp(x);
end;

function axpm1(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'expm1'; {$endif}
begin
  result := exp(x) - 1;
end;

function l0g(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'log'; {$endif}
begin
  result := ln(x);
end;

function t0n(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'tan'; {$endif}
begin
  result := tan(x);
end;

function kosh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'cosh'; {$endif}
begin
  result := cosh(x);
end;

function cinh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'sinh'; {$endif}
begin
  result := sinh(x);
end;

function t0nh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'tanh'; {$endif}
begin
  result := tanh(x);
end;

function ac0sh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'acosh'; {$endif}
begin
  result := arccosh(x);
end;

function as1nh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'asinh'; {$endif}
begin
  result := arcsinh(x);
end;

function at0n(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'atan'; {$endif}
begin
  result := arctan(x);
end;

function at0n2(y, x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'atan2'; {$endif}
begin
  result := ArcTan2(y, x);
end;

function at0nh(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'atanh'; {$endif}
begin
  result := arctanh(x);
end;

function hyp0t(y, x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'hypot'; {$endif}
begin
  result := hypot(y, x);
end;

function p0w(b, e: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'pow'; {$endif}
begin
  result := power(b, e);
end;

function fl00r(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'floor'; {$endif}
begin
  result := floor(x);
end;

function r0und(x: double): Int64; cdecl;
  {$ifdef FPC} public name _PREFIX + 'round'; {$endif}
begin
  result := round(x);
end;

function ce1l(x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'ceil'; {$endif}
var
  i: Int64;
begin
  i := trunc(x) + ord(frac(x) > 0);
  result := i; // libc returns a double
end;


function lr1nt(x: double): Int64; cdecl;
  {$ifdef FPC} public name _PREFIX + 'lrint'; {$endif}
begin
  result := round(x);
end;

function l0g1p(const x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'log1p'; {$endif}
begin
  result := ln(1 + X);
end;

function l0g10(const x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'log10'; {$endif}
const
  InvLn10 : UInt64 = $3FDBCB7B1526E50E; // 1/Ln(10)
begin
  result := ln(X) * PDouble(@InvLn10)^;
end;

function l0g2(const x: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'log2'; {$endif}
const
  InvLn2 : UInt64 = $3FF71547652B82FE; // 1/Ln(2)
begin
  result := ln(X) * PDouble(@InvLn2)^;
end;

function fesetr0und(mode: integer): integer; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fesetround'; {$endif}
begin
  // not implemented yet
  result := mode;
end;

function fm0d(x, y: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fmod'; {$endif}
begin
  result:= x - y * int(x / y);
end;

function fm4x(x, y: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fmax'; {$endif}
begin
  result := Max(x, y);
end;

function fm1n(x, y: double): double; cdecl;
  {$ifdef FPC} public name _PREFIX + 'fmin'; {$endif}
begin
  result := min(x, y);
end;

// we redirect basic 64-bit computation to the FPC/Delphi RTL

// long __moddi3 (long a, long b)
function moddi3(num, den: int64): int64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__moddi3'; {$endif}
begin
  result := num mod den;
end;

// unsigned long __umoddi3 (unsigned long a, unsigned long b)
function umoddi3(num, den: uint64): uint64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__umoddi3'; {$endif}
begin
  result := num mod den;
end;

// long __divdi3 (long a, long b)
function divdi3(num, den: int64): int64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__divdi3'; {$endif}
begin
  result := num div den;
end;

// unsigned long __udivdi3 (unsigned long a, unsigned long b)
function udivdi3(num, den: uint64): uint64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__udivdi3'; {$endif}
begin
  result := num div den;
end;

// unsigned long __udivmoddi4 (unsigned long a, unsigned long b, unsigned long *c)
// return value is the quotient, and the remainder is placed in c
function udivmoddi4(a, b: UInt64; var c: UInt64): UInt64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__udivmoddi4'; {$endif}
{$ifdef CPU32}
var
  q, r: cardinal;
{$endif CPU32}
begin
  {$ifdef CPU32}
  if (a shr 32 = 0) and
     (b shr 32 = 0) then
  begin
    // faster direct computation for small numbers (typical use in QuickJS)
    q := cardinal(a) div cardinal(b);
    r := cardinal(a) - (q * cardinal(b));
    c := r;
    result := q;
    exit;
  end;
  {$endif CPU32}
  result := a div b;
  c := a - (result * b); // single division is possible for unsigned values
end;

// long __divmoddi4 (long a, long b, long *c)
function __divmoddi4(a, b: Int64; var c: Int64): Int64; cdecl;
  {$ifdef FPC} public name _PREFIX + '__divmoddi4'; {$endif}
begin
  result := a div b;
  c := a mod b; // as two divisions to properly handle the sign (seldom called)
end;

{$ifdef FPC}

{$ifdef OSANDROID}

function mmap64(addr: pointer; len: PtrInt; prot, flags, fd: integer;
  offset: Int64): pointer; cdecl;
  {$ifdef FPC} public name 'mmap64'; {$endif}
begin
  result := nil; // fails until really needed
  //  result := fpcmmap(addr, len, prot, flags, fd, offset shr 12);
  // mmap2 uses 4096 bytes as offset
end;

{$endif OSANDROID}

{$ifdef OSANDROID2}

{$ifdef CPUARM}
function bswapsi2(num: uint32): uint32; cdecl;
  public alias: '__bswapsi2';
asm
  rev r0, r0	// reverse bytes in parameter and put into result register
  bx  lr
end;

function bswapdi2(num: uint64):uint64; cdecl;
  public alias: '__bswapdi2';
asm
  rev r2, r0  // r2 = rev(r0)
  rev r0, r1  // r0 = rev(r1)
  mov r1, r2  // r1 = r2 = rev(r0)
  bx  lr
end;
{$endif CPUARM}

{$endif OSANDROID}

{$endif FPC}

{$ifdef CPUINTEL}

{$ifdef CPU64}

{$ifdef OSWINDOWS}

// asm stubs to circumvent libgcc.a (cross)linking issues on FPC/Win64

// unsigned long long __udivti3 (unsigned long long a, unsigned long long b)
procedure __udivti3; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__udivti3'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        push    rdi
        push    rsi
        push    rbx
        sub     rsp, 16
        mov     r10, qword ptr [rdx]
        mov     rdx, qword ptr [rdx+8H]
        mov     r9, qword ptr [rcx]
        mov     r8, qword ptr [rcx+8H]
        mov     rcx, r10
        test    rdx, rdx
        jnz     @@001
        cmp     r10, r8
        jbe     @@005
        mov     rdx, r8
        mov     rax, r9
        xor     r8d, r8d
        div     r10
        jmp     @@002
@@001:  cmp     rdx, r8
        jbe     @@003
        xor     r8d, r8d
        xor     eax, eax
@@002:  mov     qword ptr [rsp], rax
        mov     qword ptr [rsp+8H], r8
        movdqu  xmm0, oword ptr [rsp]
        add     rsp, 16
        pop     rbx
        pop     rsi
        pop     rdi
        ret
@@003:  bsr     rax, rdx
        xor     rax, 3FH
        mov     ebx, eax
        test    eax, eax
        jnz     @@007
        cmp     rdx, r8
        jc      @@004
        xor     r8d, r8d
        xor     eax, eax
        cmp     r10, r9
        ja      @@002
@@004:  xor     r8d, r8d
        mov     eax, 1
        jmp     @@002
@@005:  test    r10, r10
        jnz     @@006
        mov     eax, 1
        xor     edx, edx
        div     r10
        mov     rcx, rax
@@006:  mov     rax, r8
        xor     edx, edx
        div     rcx
        mov     r8, rax
        mov     rax, r9
        div     rcx
        jmp     @@002
@@007:  mov     ecx, eax
        mov     edi, 64
        mov     rsi, r10
        mov     r11, r8
        shl     rdx, cl
        movsxd  rcx, eax
        sub     rdi, rcx
        mov     ecx, edi
        shr     rsi, cl
        mov     ecx, eax
        shl     r10, cl
        mov     ecx, edi
        or      rsi, rdx
        shr     r11, cl
        mov     ecx, eax
        mov     rax, r9
        shl     r8, cl
        mov     ecx, edi
        mov     rdx, r11
        shr     rax, cl
        or      rax, r8
        div     rsi
        mov     r11, rdx
        mov     r8, rax
        mul     r10
        cmp     r11, rdx
        jc      @@009
        mov     ecx, ebx
        shl     r9, cl
        cmp     r9, rax
        jnc     @@008
        cmp     r11, rdx
        jz      @@009
@@008:  mov     rax, r8
        xor     r8d, r8d
        jmp     @@002
@@009:  lea     rax, qword ptr [r8 - 1]
        xor     r8d, r8d
        jmp     @@002
end;

// unsigned long long __udivmodti4 (unsigned long long a, unsigned long long b, unsigned long long *c)
procedure __udivmodti4; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__udivmodti4'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        push    r13
        push    r12
        push    rbp
        push    rdi
        push    rsi
        push    rbx
        sub     rsp, 24
        mov     r10, qword ptr [rcx]
        mov     r9, qword ptr [rcx+8H]
        mov     r11, qword ptr [rdx]
        mov     rbx, qword ptr [rdx+8H]
        mov     rax, r10
        mov     rdx, r9
        mov     rcx, r11
        test    rbx, rbx
        jnz     @@005
        cmp     r11, r9
        jbe     @@003
        xor     r9d, r9d
        div     r11
@@001:  mov     rsi, rax
        test    r8, r8
        jz      @@002
        mov     qword ptr [r8], rdx
        mov     qword ptr [r8+8H], 0
@@002:  mov     qword ptr [rsp], rsi
        mov     qword ptr [rsp+8H], r9
        movdqu  xmm0, oword ptr [rsp]
        nop
        add     rsp, 24
        pop     rbx
        pop     rsi
        pop     rdi
        pop     rbp
        pop     r12
        pop     r13
        ret
@@003:  test    r11, r11
        jnz     @@004
        mov     eax, 1
        xor     edx, edx
        div     r11
        mov     rcx, rax
@@004:  mov     rax, r9
        xor     edx, edx
        div     rcx
        mov     r9, rax
        mov     rax, r10
        div     rcx
        jmp     @@001
@@005:  cmp     rbx, r9
        jbe     @@006
        test    r8, r8
        je      @@011
        mov     qword ptr [r8+8H], r9
        xor     esi, esi
        xor     r9d, r9d
        mov     qword ptr [r8], r10
        jmp     @@002
@@006:  bsr     rdi, rbx
        xor     rdi, 3FH
        test    edi, edi
        jnz     @@008
        cmp     rbx, r9
        jc      @@012
        xor     esi, esi
        cmp     r11, r10
        jbe     @@012
@@007:  xor     r9d, r9d
        test    r8, r8
        je      @@002
        mov     qword ptr [r8], rax
        mov     qword ptr [r8+8H], rdx
        jmp     @@002
@@008:  movsxd  rax, edi
        mov     r12d, 64
        mov     ecx, edi
        mov     rsi, r10
        sub     r12, rax
        shl     rbx, cl
        mov     rax, r11
        mov     ebp, edi
        mov     ecx, r12d
        shr     rax, cl
        mov     ecx, edi
        shl     r11, cl
        mov     ecx, r12d
        or      rbx, rax
        shr     rdx, cl
        mov     ecx, edi
        shl     r9, cl
        mov     ecx, r12d
        shr     rsi, cl
        mov     ecx, edi
        or      r9, rsi
        shl     r10, cl
        mov     rax, r9
        div     rbx
        mov     rcx, rdx
        mov     rsi, rax
        mov     r9, rax
        mul     r11
        mov     r13, rax
        mov     rdi, rdx
        cmp     rcx, rdx
        jc      @@009
        jnz     @@010
        cmp     r10, rax
        jnc     @@010
@@009:  sub     rax, r11
        sbb     rdx, rbx
        lea     r9, qword ptr [rsi-1H]
        mov     rdi, rdx
        mov     r13, rax
@@010:  mov     rsi, r9
        xor     r9d, r9d
        test    r8, r8
        je      @@002
        mov     rdx, rcx
        mov     ecx, r12d
        sub     r10, r13
        sbb     rdx, rdi
        mov     rax, rdx
        shl     rax, cl
        mov     ecx, ebp
        shr     r10, cl
        shr     rdx, cl
        or      r10, rax
        mov     qword ptr [r8+8H], rdx
        mov     qword ptr [r8], r10
        jmp     @@002
@@011:  xor     r9d, r9d
        xor     esi, esi
        jmp     @@002
@@012:  mov     rdx, r9
        mov     rax, r10
        mov     esi, 1
        sub     rax, r11
        sbb     rdx, rbx
        jmp     @@007
end;

// long long __divti3 (long long a, long long b)
procedure __divti3; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__divti3'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        push    rdi
        push    rsi
        push    rbx
        sub     rsp, 16
        xor     r10d, r10d
        mov     rdi, qword ptr [rcx+8H]
        mov     rax, qword ptr [rdx]
        mov     rsi, qword ptr [rcx]
        mov     rdx, qword ptr [rdx+8H]
        mov     r8, rdi
        test    rdi, rdi
        jns     @@001
        neg     rsi
        mov     r10, -1
        adc     rdi, 0
        neg     rdi
        mov     r8, rdi
@@001:  mov     r9, rdx
        test    rdx, rdx
        jns     @@002
        neg     rax
        not     r10
        adc     rdx, 0
        neg     rdx
        mov     r9, rdx
@@002:  mov     rbx, rax
        mov     r11, rsi
        test    r9, r9
        jnz     @@003
        cmp     rax, r8
        jbe     @@008
        mov     rdx, r8
        mov     rax, rsi
        xor     r8d, r8d
        div     rbx
        jmp     @@004
@@003:  cmp     r9, r8
        jbe     @@006
        xor     r8d, r8d
        xor     eax, eax
@@004:  mov     qword ptr [rsp], rax
        mov     qword ptr [rsp+8H], r8
        test    r10, r10
        jz      @@005
        neg     qword ptr [rsp]
        adc     qword ptr [rsp+8H], 0
        neg     qword ptr [rsp+8H]
@@005:  movdqu  xmm0, oword ptr [rsp]
        add     rsp, 16
        pop     rbx
        pop     rsi
        pop     rdi
        ret
@@006:  bsr     rax, r9
        xor     rax, 3FH
        mov     esi, eax
        test    eax, eax
        jnz     @@010
        cmp     r9, r8
        jc      @@007
        xor     r8d, r8d
        xor     eax, eax
        cmp     rbx, r11
        ja      @@004
@@007:  xor     r8d, r8d
        mov     eax, 1
        jmp     @@004
@@008:  test    rax, rax
        jnz     @@009
        mov     eax, 1
        xor     edx, edx
        div     r9
        mov     rbx, rax
@@009:  mov     rax, r8
        xor     edx, edx
        div     rbx
        mov     r8, rax
        mov     rax, r11
        div     rbx
        jmp     @@004
@@010:  mov     ecx, eax
        mov     edx, 64
        mov     rdi, rbx
        shl     r9, cl
        movsxd  rcx, eax
        sub     rdx, rcx
        mov     ecx, edx
        shr     rdi, cl
        mov     ecx, eax
        or      r9, rdi
        shl     rbx, cl
        mov     rdi, r8
        mov     ecx, edx
        shr     rdi, cl
        mov     ecx, eax
        mov     rax, r11
        shl     r8, cl
        mov     ecx, edx
        mov     rdx, rdi
        shr     rax, cl
        or      rax, r8
        div     r9
        mov     rdi, rdx
        mov     r8, rax
        mul     rbx
        cmp     rdi, rdx
        jc      @@012
        mov     ecx, esi
        shl     r11, cl
        cmp     r11, rax
        jnc     @@011
        cmp     rdi, rdx
        jz      @@012
@@011:  mov     rax, r8
        xor     r8d, r8d
        jmp     @@004
@@012:  lea     rax, qword ptr [r8-1H]
        xor     r8d, r8d
        jmp     @@004
end;

// unsigned long long __umodti3 (unsigned long long a, unsigned long long b)
procedure __umodti3; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__umodti3'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        push    rdi
        push    rsi
        push    rbx
        sub     rsp, 16
        mov     r8, qword ptr [rcx+8H]
        mov     r10, qword ptr [rdx]
        mov     r11, qword ptr [rdx+8H]
        mov     r9, qword ptr [rcx]
        mov     rdx, r8
        mov     rcx, r10
        test    r11, r11
        jnz     @@003
        cmp     r10, r8
        jbe     @@005
        mov     rax, r9
        div     r10
        mov     r9, rdx
@@001:  xor     r8d, r8d
@@002:  mov     qword ptr [rsp], r9
        mov     qword ptr [rsp+8H], r8
        movdqu  xmm0, oword ptr [rsp]
        nop
        add     rsp, 16
        pop     rbx
        pop     rsi
        pop     rdi
        ret
@@003:  mov     rax, r9
        cmp     r11, r8
        ja      @@002
        bsr     rdi, r11
        xor     rdi, 3FH
        test    edi, edi
        jnz     @@007
        cmp     r11, r8
        jc      @@010
        cmp     r10, r9
        jbe     @@010
@@004:  mov     r9, rax
        mov     r8, rdx
        jmp     @@002
@@005:  test    r10, r10
        jnz     @@006
        mov     eax, 1
        xor     edx, edx
        div     r10
        mov     rcx, rax
@@006:  mov     rax, r8
        xor     edx, edx
        div     rcx
        mov     rax, r9
        div     rcx
        mov     r9, rdx
        jmp     @@001
@@007:  movsxd  rax, edi
        mov     esi, 64
        mov     ecx, edi
        mov     ebx, edi
        sub     rsi, rax
        shl     r11, cl
        mov     rax, r10
        mov     ecx, esi
        shr     rax, cl
        mov     ecx, edi
        shl     r10, cl
        mov     ecx, esi
        or      r11, rax
        mov     rax, r9
        shr     rdx, cl
        mov     ecx, edi
        shl     r8, cl
        mov     ecx, esi
        shr     rax, cl
        mov     ecx, edi
        or      rax, r8
        shl     r9, cl
        div     r11
        mov     r8, rdx
        mul     r10
        mov     rdi, rax
        mov     rcx, rdx
        cmp     r8, rdx
        jc      @@008
        jnz     @@009
        cmp     r9, rax
        jnc     @@009
@@008:  sub     rax, r10
        sbb     rdx, r11
        mov     rcx, rdx
        mov     rdi, rax
@@009:  mov     r10, r9
        sub     r10, rdi
        sbb     r8, rcx
        mov     ecx, esi
        mov     r9, r8
        shl     r9, cl
        mov     ecx, ebx
        shr     r10, cl
        shr     r8, cl
        or      r9, r10
        jmp     @@002
@@010:  sub     r9, r10
        sbb     r8, r11
        mov     rax, r9
        mov     rdx, r8
        jmp     @@004
end;

{$endif OSWINDOWS}

{$ifdef OSLINUX}

// unsigned long long __udivti3 (unsigned long long a, unsigned long long b)
procedure __udivti3; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__udivti3'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        mov     r8, rcx
        mov     r9, rdx
        mov     r10, rdx
        test    r8, r8
        mov     rcx, rdx
        jnz     @@003
        cmp     rdx, rsi
        ja      @@006
        test    rdx, rdx
        jnz     @@001
        mov     eax, 1
        xor     edx, edx
        div     r9
        mov     rcx, rax
@@001:  mov     rax, rsi
        xor     edx, edx
        div     rcx
        mov     rsi, rax
        mov     rax, rdi
        div     rcx
        mov     rdx, rsi
@@002:  ret
@@003:  cmp     r8, rsi
        ja      @@005
        bsr     rax, r8
        xor     rax, 3FH
        test    eax, eax
        mov     r11d, eax
        jz      @@007
        mov     ecx, eax
        mov     edx, 64
        shl     r8, cl
        movsxd  rcx, eax
        sub     rdx, rcx
        mov     ecx, edx
        shr     r9, cl
        mov     ecx, eax
        or      r8, r9
        shl     r10, cl
        mov     r9, rsi
        mov     ecx, edx
        shr     r9, cl
        mov     ecx, eax
        mov     rax, rdi
        shl     rsi, cl
        mov     ecx, edx
        mov     rdx, r9
        shr     rax, cl
        or      rsi, rax
        mov     rax, rsi
        div     r8
        mov     r9, rdx
        mov     rsi, rax
        mul     r10
        cmp     r9, rdx
        jc      @@004
        mov     ecx, r11d
        shl     rdi, cl
        cmp     rdi, rax
        jnc     @@009
        cmp     r9, rdx
        jnz     @@009
@@004:  lea     rax, qword ptr [rsi-1H]
        xor     edx, edx
        ret
@@005:  xor     edx, edx
        xor     eax, eax
        ret
@@006:  mov     rax, rdi
        mov     rdx, rsi
        div     r9
        xor     edx, edx
        ret
@@007:  cmp     r8, rsi
        jc      @@008
        xor     edx, edx
        xor     eax, eax
        cmp     r9, rdi
        ja      @@002
@@008:  xor     edx, edx
        mov     eax, 1
        ret
@@009:  mov     rax, rsi
        xor     edx, edx
end;

// unsigned long long __udivmodti4 (unsigned long long a, unsigned long long b, unsigned long long *c)
procedure __udivmodti4; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__udivmodti4'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        test    rcx, rcx
        push    r13
        mov     r9, rdx
        push    r12
        mov     r11, rdx
        push    rbp
        mov     rdx, rsi
        push    rbx
        jnz     @@002
        cmp     r9, rsi
        jbe     @@006
        mov     rax, rdi
        div     r9
        mov     rsi, rdx
        xor     edx, edx
@@001:  test    r8, r8
        jz      @@003
        mov     qword ptr [r8], rsi
        mov     qword ptr [r8+8H], 0
        pop     rbx
        pop     rbp
        pop     r12
        pop     r13
        ret
@@002:  cmp     rcx, rsi
        mov     r10, rcx
        jbe     @@004
        test    r8, r8
        je      @@013
        mov     qword ptr [r8], rdi
        mov     qword ptr [r8+8H], rsi
        xor     edx, edx
        xor     eax, eax
@@003:  pop     rbx
        pop     rbp
        pop     r12
        pop     r13
        ret
@@004:  bsr     r12, rcx
        xor     r12, 3FH
        test    r12d, r12d
        jnz     @@008
        cmp     rcx, rsi
        jc      @@012
        xor     eax, eax
        cmp     r9, rdi
        mov     rbx, rdi
        jbe     @@012
@@005:  test    r8, r8
        je      @@014
        mov     qword ptr [r8+8H], rdx
        xor     edx, edx
        mov     qword ptr [r8], rbx
        pop     rbx
        pop     rbp
        pop     r12
        pop     r13
        ret
@@006:  test    r9, r9
        jnz     @@007
        mov     eax, 1
        xor     edx, edx
        div     r9
        mov     r11, rax
@@007:  mov     rax, rsi
        xor     edx, edx
        div     r11
        mov     rcx, rax
        mov     rax, rdi
        div     r11
        mov     rsi, rdx
        mov     rdx, rcx
        jmp     @@001
@@008:  movsxd  rax, r12d
        mov     ebp, 64
        mov     ecx, r12d
        sub     rbp, rax
        shl     r10, cl
        mov     rax, r9
        mov     ecx, ebp
        mov     r11, r9
        mov     rdx, rdi
        shr     rax, cl
        mov     ecx, r12d
        mov     ebx, r12d
        or      r10, rax
        shl     r11, cl
        mov     rax, rsi
        mov     ecx, ebp
        shr     rax, cl
        mov     ecx, r12d
        shl     rsi, cl
        mov     ecx, ebp
        shr     rdx, cl
        mov     ecx, r12d
        or      rsi, rdx
        shl     rdi, cl
        mov     rcx, rax
        mov     rdx, rcx
        mov     rax, rsi
        div     r10
        mov     rcx, rdx
        mov     rsi, rax
        mov     r9, rax
        mul     r11
        cmp     rcx, rdx
        mov     r13, rax
        mov     r12, rdx
        jc      @@009
        jnz     @@010
        cmp     rdi, rax
        jnc     @@010
@@009:  lea     r9, qword ptr [rsi-1H]
        sub     rax, r11
        sbb     rdx, r10
        mov     r12, rdx
        mov     r13, rax
@@010:  test    r8, r8
        jz      @@011
        mov     rsi, rcx
        mov     rax, rdi
        mov     ecx, ebp
        sub     rax, r13
        sbb     rsi, r12
        mov     rdi, rsi
        shl     rdi, cl
        mov     ecx, ebx
        shr     rax, cl
        shr     rsi, cl
        or      rax, rdi
        mov     qword ptr [r8+8H], rsi
        mov     qword ptr [r8], rax
@@011:  mov     rax, r9
        xor     edx, edx
        pop     rbx
        pop     rbp
        pop     r12
        pop     r13
        ret
@@012:  mov     rax, rdi
        sub     rax, r9
        sbb     rsi, rcx
        mov     rbx, rax
        mov     rdx, rsi
        mov     eax, 1
        jmp     @@005
@@013:  xor     edx, edx
        xor     eax, eax
        jmp     @@003
@@014:  xor     edx, edx
        jmp     @@003
end;

// unsigned long long __umodti3 (unsigned long long a, unsigned long long b)
procedure __umodti3; assembler;
  {$ifdef FPC} nostackframe; public name _PREFIX + '__umodti3'; {$endif}
asm
        {$ifndef FPC}
        .noframe
        {$endif FPC}
        test    rcx, rcx
        push    r12
        mov     r9, rdx
        push    rbp
        mov     r10, rdx
        push    rbx
        mov     rax, rdi
        mov     rbx, rdx
        mov     rdx, rsi
        jnz     @@003
        cmp     r9, rsi
        jbe     @@006
        div     r9
@@001:  mov     rax, rdx
        xor     edx, edx
@@002:  pop     rbx
        pop     rbp
        pop     r12
        ret
@@003:  cmp     rcx, rsi
        mov     r8, rcx
        ja      @@008
        bsr     rbp, rcx
        xor     rbp, 3FH
        test    ebp, ebp
        je      @@009
        movsxd  rax, ebp
        mov     r11d, 64
        mov     ecx, ebp
        sub     r11, rax
        shl     r8, cl
        mov     r10, rbx
        mov     ecx, r11d
        mov     r12, rsi
        mov     rax, rdi
        shr     r10, cl
        mov     ecx, ebp
        mov     r9d, ebp
        shl     rbx, cl
        mov     ecx, r11d
        or      r10, r8
        shr     r12, cl
        mov     ecx, ebp
        shl     rsi, cl
        mov     ecx, r11d
        shr     rax, cl
        mov     rdx, rsi
        mov     ecx, ebp
        or      rdx, rax
        shl     rdi, cl
        mov     rax, rdx
        mov     rdx, r12
        mov     r8, rdi
        div     r10
        mov     rsi, rdx
        mul     rbx
        cmp     rsi, rdx
        mov     rdi, rax
        mov     rcx, rdx
        jc      @@004
        jnz     @@005
        cmp     r8, rax
        jnc     @@005
@@004:  sub     rax, rbx
        sbb     rdx, r10
        mov     rcx, rdx
        mov     rdi, rax
@@005:  sub     r8, rdi
        sbb     rsi, rcx
        mov     ecx, r11d
        mov     rax, rsi
        shl     rax, cl
        mov     ecx, r9d
        shr     r8, cl
        shr     rsi, cl
        or      rax, r8
        mov     rdx, rsi
        pop     rbx
        pop     rbp
        pop     r12
        ret
@@006:  test    r9, r9
        jnz     @@007
        mov     eax, 1
        xor     edx, edx
        div     r9
        mov     r10, rax
@@007:  mov     rax, rsi
        xor     edx, edx
        div     r10
        mov     rax, rdi
        div     r10
        jmp     @@001
@@008:  mov     rax, rdi
        mov     rdx, rsi
        pop     rbx
        pop     rbp
        pop     r12
        ret
@@009:  cmp     rcx, rsi
        jc      @@010
        cmp     r9, rdi
        ja      @@002
@@010:  mov     rdx, rsi
        mov     rax, rdi
        sub     rax, r9
        sbb     rdx, rcx
        jmp     @@002
end;

{$endif OSLINUX}

{$endif CPU64}

{$endif CPUINTEL}



{ ********************** Cross-Platform FPU Exceptions Masking }

const
  _FPUFLAGSIDEM = cardinal(-1); // fake value used for faster nested calls

procedure _SetFlags(flags: cardinal);
  {$ifdef HASINLINE} inline; {$endif}
begin
{$ifdef CPUINTEL}
  {$ifdef CPU64}
  SetMXCSR(flags);
  {$else}
  Set8087CW(flags);
  {$endif CPU64}
{$else}
  SetExceptionMask(TFPUExceptionMask(flags));
{$endif CPUINTEL}
end;

function SetFpuFlags(flags: TFpuFlags): cardinal;
var
  new: cardinal;
begin
{$ifdef CPUINTEL}
  {$ifdef CPU64}
  result := GetMXCSR;
  {$else}
  result := Get8087CW;
  {$endif CPU64}
{$else}
  result := cardinal(GetExceptionMask);
{$endif CPUINTEL}
  new := cardinal(_FPUFLAGS[flags]);
  if new <> result then
    _SetFlags(new)
  else
    result := _FPUFLAGSIDEM;
end;

procedure ResetFpuFlags(saved: cardinal);
begin
  if saved <> _FPUFLAGSIDEM then
    _SetFlags(saved);
end;


initialization
{$ifdef FPC}
{$ifdef OSWINDOWS}
  // manual fill of our raw mingw import table
  beginthreadex := @libc_beginthreadex;
  endthreadex := @libc_endthreadex;
  {$ifdef CPU32}
  imp_localtime32 := @localtime32;
  {$else}
  imp_localtime64 := @localtime64;
  {$endif CPU32}
{$endif OSWINDOWS}
{$endif FPC}

end.

