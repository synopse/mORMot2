/// Database Framework Low-Level Statically Linked SQLite3 Engine
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.sqlite3.static;

{
  *****************************************************************************

    Statically linked SQLite3 3.34.1 engine with optional AES encryption
    - TSqlite3LibraryStatic Implementation
    - Encryption-Related Functions

      Just include this unit in your uses clause, and the mormot.db.raw.sqlite3
    sqlite3 global variable will be filled with linked .o/.obj API entries.
      If the platform is not supported yet, fallback to a system .so is done.
      To patch and compile the official SQlite3 amalgamation file, follow the
    instruction from db\amalgamation\ReadMe.md

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}


{$ifdef NOSQLITE3STATIC} // conditional defined -> auto-load local .dll/.so

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.db.raw.sqlite3;

implementation

procedure DoInitialization;
begin
  FreeAndNil(sqlite3);
  try
    sqlite3 := TSqlite3LibraryDynamic.Create(SQLITE_LIBRARY_DEFAULT_NAME);
    sqlite3.ForceToUseSharedMemoryManager; // faster process
  except
    on E: Exception do
      {$ifdef OSPOSIX} // there is always an error console ouput on POSIX
      writeln(SQLITE_LIBRARY_DEFAULT_NAME + ' initialization failed with ',
        ClassNameShort(E)^, ': ', E.Message);
      {$endif OSPOSIX}
  end;
end;

initialization
  DoInitialization;

{$else NOSTATIC}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.crypto, // for our AES encryption
  mormot.core.secure,
  mormot.db.raw.sqlite3;


{ ************ TSqlite3LibraryStatic Implementation }

type
  /// access class to the static .obj/.o SQLite3 engine
  // - the intialization section of this unit calls:
  // ! sqlite3 := TSqlite3LibraryStatic.Create;
  // therefore, adding mormot.db.raw.sqlite3.static to your uses clause is enough
  // to use the statically linked SQLite3 engine with mormot.db.raw.sqlite3
  TSqlite3LibraryStatic = class(TSqlite3Library)
  public
    /// fill the internal API reference s with the static .obj engine
    constructor Create; override;
    /// unload the static library
    destructor Destroy; override;
  end;


{ ************ Encryption-Related Functions }

/// use this procedure to change the password for an existing SQLite3 database file
// - convenient and faster alternative to the sqlite3.rekey() API call
// - conversion is done in-place at file level, with no SQL nor BTree pages
// involved, therefore it can process very big files with best possible speed
// - the OldPassWord must be correct, otherwise the resulting file will be corrupted
// - any password can be '' to mark no encryption as input or output
// - the password may be a JSON-serialized TSynSignerParams object, or will use
// AES-OFB-128 (or AES-CTR-128 if ForceSQLite3AESCTR is set) after SHAKE_128 with
// rounds=1000 and a fixed salt on plain password text
// - please note that this encryption is compatible only with SQlite3 files made
// with SynSQLiteStatic.pas unit (not external/official/wxsqlite3 dll)
// - implementation is NOT compatible with the official SQLite Encryption Extension
// (SEE) file format, not the wxsqlite3 extension, but is (much) faster thanks
// to our SynCrypto AES-NI enabled unit
// - if the key is not correct, a ESqlite3Exception will be raised with
// 'database disk image is malformed' (SQLITE_CORRUPT) at database opening
// - see also IsSQLite3File/IsSQLite3FileEncrypted functions
// - warning: this encryption is NOT compatible with our previous (<1.18.4413)
// cyphered format, which was much less safe (simple XOR on fixed tables), and
// was not working on any database size, making unclean patches to the official
// sqlite3.c amalgamation file, so is deprecated and unsupported any longer -
// see OldSQLEncryptTablePassWordToPlain() to convert your existing databases
function ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUtf8): boolean;

/// this function may be used to create a plain database file from an existing
// one encrypted with our old/deprecated/unsupported format (<1.18.4413)
// - then call ChangeSQLEncryptTablePassWord() to convert to the new safer format
procedure OldSQLEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUtf8);

/// could be used to detect a database in old/deprecated/unsupported format (<1.18.4413)
// - to call OldSQLEncryptTablePassWordToPlain + ChangeSQLEncryptTablePassWord
// and switch to the new format
function IsOldSQLEncryptTable(const FileName: TFileName): boolean;

var
  /// global flag to use initial AES encryption scheme
  // - IV derivation was hardened in revision 1.18.4607 - set TRUE to this
  // global constant to use the former implementation (theoritically slightly
  // less resistant to brute force attacks) and convert existing databases
  ForceSQLite3LegacyAES: boolean;

  /// global flag to use AES-CTR instead of AES-OFB encryption
  // - on x86_64 our optimized asm is 2.5GB/s instead of 770MB/s
  ForceSQLite3AESCTR: boolean;



implementation

{$ifdef OSWINDOWS}

uses
  Windows; // statically linked .obj requires the Windows API

{$endif WINDOWS}


{ ************ TSqlite3LibraryStatic Implementation }

// ---------------- link raw .o .obj files

{$ifdef FPC}  // FPC expects .o linking, and only one version including FTS

  {$ifdef OSWINDOWS}
    {$ifdef CPU64}
      const _PREFIX = '';
      {$L ..\..\static\x86_64-win64\sqlite3.o}
      {$linklib ..\..\static\x86_64-win64\libkernel32.a}
      {$linklib ..\..\static\x86_64-win64\libgcc.a}
      {$linklib ..\..\static\x86_64-win64\libmsvcrt.a}
    {$else}
      const _PREFIX = '_';
      {$L ..\..\static\i386-win32\sqlite3.o}
      {$linklib ..\..\static\i386-win32\libkernel32.a}
      {$linklib ..\..\static\i386-win32\libgcc.a}
      {$linklib ..\..\static\i386-win32\libmsvcrt.a}
    {$endif CPU64}
  {$endif OSWINDOWS}

  {$ifdef OSDARWIN}
    const _PREFIX = '_';
    {$ifdef CPU64}
      {$linklib ..\..\static\x86_64-darwin\libsqlite3.a}
    {$else}
      {$linklib ..\..\static\i386-darwin\libsqlite3.a}
    {$endif}
  {$endif OSDARWIN}

  {$ifdef OSANDROID}
    const _PREFIX = '';
    {$ifdef CPUAARCH64}
      {$L ..\..\static\aarch64-android\libsqlite3.a}
      {$L ..\..\static\aarch64-android\libgcc.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L ..\..\static\arm-android\libsqlite3.a}
      {$L ..\..\static\arm-android\libgcc.a}
    {$endif CPUARM}
  {$endif OSANDROID}

  {$ifdef OSFREEBSD}
    {$ifdef CPUX86}
    const _PREFIX = '';
    {$L ..\..\static\i386-freebsd\sqlite3.o}
    {$ifdef FPC_CROSSCOMPILING}
      {$linklib ..\..\static\i386-freebsd\libgcc.a}
    {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
    const _PREFIX = '';
    {$L ..\..\static\x86_64-freebsd\sqlite3.o}
    {$ifdef FPC_CROSSCOMPILING}
      {$linklib ..\..\static\x86_64-freebsd\libgcc.a}
    {$endif}
    {$endif CPUX64}
  {$endif OSFREEBSD}

  {$ifdef OSOPENBSD}
    {$ifdef CPUX86}
      const _PREFIX = '';
      {$L ..\..\static\i386-openbsd\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib ..\..\static\i386-openbsd\libgcc.a}
      {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
      const _PREFIX = '';
      {$L ..\..\static\x86_64-openbsd\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib ..\..\static\x86_64-openbsd\libgcc.a}
      {$endif}
    {$endif CPUX64}
  {$endif OSOPENBSD}

  {$ifdef OSLINUX}
    const _PREFIX = '';
    {$ifdef CPUAARCH64}
      {$L ..\..\static\aarch64-linux\sqlite3.o}
      {$L ..\..\static\aarch64-linux\libgcc.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L ..\..\static\arm-linux\sqlite3.o}
      {$L ..\..\static\arm-linux\libgcc.a}
    {$endif CPUARM}
    {$ifdef CPUX86}
      {$L ..\..\static\i386-linux\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib ..\..\static\i386-linux\libgcc.a}
      {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {$L ..\..\static\x86_64-linux\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib ..\..\static\x86_64-linux\libgcc.a}
      {$endif}
    {$endif CPUX64}
  {$endif OSLINUX}

function log(x: double): double; cdecl; public name _PREFIX + 'log'; export;
begin
  result := ln(x);
end;

{$ifdef OSWINDOWS}
{$ifdef CPUX86} // not a compiler intrinsic on x86

function _InterlockedCompareExchange(
  var Dest: integer; New, Comp: integer): longint; stdcall;
  public alias: '_InterlockedCompareExchange@12';
begin
  result := InterlockedCompareExchange(Dest,New,Comp);
end;

{$endif CPUX86}
{$endif OSWINDOWS}

{$ifdef OSDARWIN}

function moddi3(num, den: int64): int64; cdecl; public alias: '___moddi3';
begin
  result := num mod den;
end;
function umoddi3(num, den: uint64): uint64; cdecl; public alias: '___umoddi3';
begin
  result := num mod den;
end;
function divdi3(num, den: int64): int64; cdecl; public alias: '___divdi3';
begin
  result := num div den;
end;
function udivdi3(num, den: uint64): uint64; cdecl; public alias: '___udivdi3';
begin
  result := num div den;
end;

{$endif OSDARWIN}

{$ifdef OSANDROID}
{$ifdef CPUARM}
function bswapsi2(num:uint32):uint32; cdecl; public alias: '__bswapsi2';
asm
  rev r0, r0	// reverse bytes in parameter and put into result register
  bx  lr
end;
function bswapdi2(num:uint64):uint64; cdecl; public alias: '__bswapdi2';
asm
  rev r2, r0  // r2 = rev(r0)
  rev r0, r1  // r0 = rev(r1)
  mov r1, r2  // r1 = r2 = rev(r0)
  bx  lr
end;
{$endif}
{$endif OSANDROID}

{$else FPC}

  // Delphi has a diverse linking strategy, since $linklib doesn't exist :(
  {$ifdef OSWINDOWS}
    {$ifdef CPU64}
      {$L ..\..\static\delphi\sqlite3.o}  // compiled with C++ Builder 10.3 Community Edition bcc64
    {$else}
      {$L ..\..\static\delphi\sqlite3.obj}  // compiled with free Borland C++ Compiler 5.5
    {$endif}
  {$endif OSWINDOWS}

// those functions will be called only under Delphi + Win32/Win64

function malloc(size: cardinal): Pointer; cdecl; { always cdecl }
begin
  GetMem(result, size);
end;

procedure free(P: Pointer); cdecl; { always cdecl }
begin
  FreeMem(P);
end;

function realloc(P: Pointer; Size: integer): Pointer; cdecl; { always cdecl }
begin
  result := P;
  ReallocMem(result, Size);
end;

function rename(oldname, newname: PUtf8Char): integer; cdecl; { always cdecl }
begin
  if RenameFile(Utf8DecodeToString(oldname, StrLen(oldname)),
                Utf8DecodeToString(newname, StrLen(newname))) then
    result := 0
  else
    result := -1;
end;

{$ifdef OSWINDOWS}
{$ifdef CPU32} // Delphi Win32 will link static Borland C++ sqlite3.obj

// we then implement all needed Borland C++ runtime functions in pure pascal:

function _ftol: Int64;
// Borland C++ float to integer (Int64) conversion
asm
          jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function _ftoul: Int64;
// Borland C++ float to integer (Int64) conversion
asm
          jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

var __turbofloat: word; { not used, but must be present for linking }

// Borland C++ and Delphi share the same low level Int64 _ll*() functions:

procedure _lldiv;
asm
          jmp System.@_lldiv
end;

procedure _lludiv;
asm
          jmp System.@_lludiv
end;

procedure _llmod;
asm
          jmp System.@_llmod
end;

procedure _llmul;
asm
          jmp System.@_llmul
end;

procedure _llumod;
asm
          jmp System.@_llumod
end;

procedure _llshl;
asm
          jmp System.@_llshl
end;

procedure _llshr;
asm
          // need this code for Borland/CodeGear default System.pas
          shrd    eax, edx, cl
          sar     edx, cl
          cmp     cl, 32
          jl      @done
          cmp     cl, 64
          jge     @sign
          mov     eax, edx
          sar     edx, 31
          ret
@sign:    sar     edx, 31
          mov     eax, edx
@done:
end;

procedure _llushr;
asm
          jmp System.@_llushr
end;

function log(const val: double): double; cdecl; { always cdecl }
asm
          fld qword ptr val
          fldln2
          fxch
          fyl2x
end;

{$endif CPU32}
{$endif OSWINDOWS}

function memset(P: Pointer; B: integer; count: integer): pointer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  FillCharFast(P^, count, B);
  result := P;
end;

function memmove(dest, source: pointer; count: integer): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memmove'{$else}'_memmove'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count); // move() is overlapping-friendly
  result := dest;
end;

function memcpy(dest, source: Pointer; count: integer): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memcpy'{$else}'_memcpy'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count);
  result := dest;
end;

function strlen(p: PAnsiChar): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strlen'{$else}'_strlen'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := mormot.core.base.StrLen(pointer(p));
end;

function strcmp(p1,p2: PAnsiChar): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strcmp'{$else}'_strcmp'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := mormot.core.base.StrComp(p1, p2);
end;

function strcspn(str,reject: PAnsiChar): integer; cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'strcspn'{$else}'_strcspn'{$endif};{$endif}
begin
  // called e.g. during LIKE process
  result := mormot.core.unicode.strcspn(str, reject); // use SSE4.2 if available
end;

function strrchr(s: PAnsiChar; c: AnsiChar): PAnsiChar; cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'strrchr'{$else}'_strrchr'{$endif};{$endif}
begin
  // simple full pascal version of the standard C library function
  result := nil;
  if s <> nil then
    while s^<>#0 do
    begin
      if s^ = c then
        result := s;
      inc(s);
    end;
end;

{$ifdef FPC}
function memcmp(p1, p2: pByte; Size: integer): integer; cdecl; { always cdecl }
  public name{$ifdef CPU64}'memcmp'{$else}'_memcmp'{$endif};
begin
  result := CompareByte(p1, p2, Size); // use FPC RTL
end;
{$else}
function memcmp(p1, p2: pByte; Size: integer): integer; cdecl; { always cdecl }
begin
  // full pascal version of the standard C library function
  if (p1 <> p2) and
     (Size <> 0) then
    if p1 <> nil then
      if p2 <> nil then
      begin
        repeat
          if p1^ = p2^ then
          begin
            inc(p1);
            inc(p2);
            dec(Size);
            if Size <> 0 then
              continue
            else
              break;
          end;
          result := p1^ - p2^;
          exit;
        until false;
        result := 0;
      end
      else
        result := 1
    else
      result := -1
  else
    result := 0;
end;
{$endif FPC}

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strncmp'{$else}'_strncmp'{$endif};{$endif}
var
  i: integer;
begin
  // a fast full pascal version of the standard C library function
  for i := 1 to Size do
  begin
    result := p1^ - p2^;
    if (result <> 0) or
       (p1^ = 0) then
      exit;
    inc(p1);
    inc(p2);
  end;
  result := 0;
end;

type
  // qsort() is used if SQLITE_ENABLE_FTS3 is defined
  // this function type is defined for calling termDataCmp() in sqlite3.c
  qsort_compare_func = function(P1, P2: pointer): integer; cdecl; { always cdecl }

procedure QuickSortPtr(base: PPointerArray; L, R: integer; comparF: qsort_compare_func);
var
  I, J, P: integer;
  PP, C: PAnsiChar;
begin
  repeat // from SQLite (FTS), With=sizeof(PAnsiChar) AFAIK
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      PP := @base[P];
      while comparF(@base[I], PP) < 0 do
        inc(I);
      while comparF(@base[J], PP) > 0 do
        dec(J);
      if I <= J then
      begin
        C := base[I];
        base[I] := base[J];
        base[J] := C; // fast memory exchange
        if P = I then
          P := J
        else if P = J then
          P := I;
        inc(I);
        dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortPtr(base, L, J, comparF);
    L := I;
  until I >= R;
end;

procedure QuickSort(baseP: PAnsiChar; Width: integer; L, R: integer;
  comparF: qsort_compare_func);

  procedure Exchg(P1, P2: PAnsiChar; Size: integer);
  var
    B: AnsiChar;
    i: integer;
  begin
    for i := 0 to Size - 1 do
    begin
      B := P1[i];
      P1[i] := P2[i];
      P2[i] := B;
    end;
  end;

var
  I, J, P: integer;
  PP, C: PAnsiChar;
begin
  repeat // generic sorting algorithm
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      PP := baseP + P * Width; // compute PP at every loop, since P may change
      C := baseP + I * Width;
      while comparF(C, PP) < 0 do
      begin
        inc(I);
        inc(C, Width); // avoid slower multiplication in loop
      end;
      C := baseP + J * Width;
      while comparF(C, PP) > 0 do
      begin
        dec(J);
        dec(C, Width); // avoid slower multiplication in loop
      end;
      if I <= J then
      begin
        Exchg(baseP + I * Width, baseP + J * Width, Width); // fast memory exchange
        if P = I then
          P := J
        else if P = J then
          P := I;
        inc(I);
        dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(baseP, Width, L, J, comparF);
    L := I;
  until I >= R;
end;

procedure qsort(baseP: pointer; NElem, Width: integer; comparF: pointer); cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'qsort'{$else}'_qsort'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  if (cardinal(NElem) > 1) and
     (Width > 0) then
    if Width = sizeof(pointer) then
      QuickSortPtr(baseP, 0, NElem-1, qsort_compare_func(comparF))
    else
      QuickSort(baseP, Width, 0, NElem-1, qsort_compare_func(comparF));
end;

var
  { as standard C library documentation states:
  Statically allocated buffer, shared by the functions gmtime() and localtime().
  Each call of these functions overwrites the content of this structure.
  -> since timing is not thread-dependent, it's OK to share this buffer :) }
  atm: packed record
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

function localtime64(const t: Int64): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name '__imp__localtime64';{$endif}
// a fast full pascal version of the standard C library function
var S: TSystemTime;
begin
  GetLocalTime(S);
  atm.tm_sec := S.wSecond;
  atm.tm_min := S.wMinute;
  atm.tm_hour := S.wHour;
  atm.tm_mday := S.wDay;
  atm.tm_mon := S.wMonth-1;
  atm.tm_year := S.wYear-1900;
  atm.tm_wday := S.wDay;
  result := @atm;
end;

function localtime(t: PCardinal): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'localtime32'{$else}'__localtime32'{$endif};{$endif}
begin
  result := localtime64(t^);
end;

{$ifdef OSWINDOWS}

const
  msvcrt = 'msvcrt.dll';
  kernel = 'kernel32.dll';

function _beginthreadex(security: pointer; stksize: dword;
  start,arg: pointer; flags: dword; var threadid: dword): THandle; cdecl; external msvcrt;
procedure _endthreadex(exitcode: dword); cdecl; external msvcrt;

{$ifdef CPU64}

// Delphi Win64 will link its own static sqlite3.o (diverse from FPC's)

function _log(x: double): double; export; // to link LLVM bcc64 compiler
begin
  result := ln(x);
end;

function log(x: double): double; export; // to link old non-LLVM bcc64 compiler
begin
  result := ln(x);
end;

procedure __chkstk;
begin
end;

procedure __faststorefence;
asm
          .noframe
          mfence;
end;

var
  _finf: double = 1.0 / 0.0; // compiles to some double infinity constant
  _fltused: Int64 = 0; // to link old non-LLVM bcc64 compiler

{$endif CPU64}

{$endif OSWINDOWS}

{$endif FPC}



{ ************ Encryption-Related Functions }

// some external functions as expected by codecext.c and our sqlite3mc.c wrapper

const
  _CODEC_PBKDF2_SALT = 'J6CuDftfPr22FnYn';

procedure CodecGenerateKey(var aes: TAes;
  userPassword: pointer; passwordLength: integer);
var
  s: TSynSigner;
  k: THash512Rec;
begin
  s.Pbkdf2(userPassword, passwordLength, k, _CODEC_PBKDF2_SALT);
  s.AssignTo(k, aes, {encrypt=}true);
end;

function CodecGetReadKey(codec: pointer): PAes;  cdecl; external;
function CodecGetWriteKey(codec: pointer): PAes; cdecl; external;

procedure CodecGenerateReadKey(codec: pointer;
  userPassword: PAnsiChar; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecGenerateReadKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetReadKey(codec)^, userPassword, passwordLength);
end;

procedure CodecGenerateWriteKey(codec: pointer;
  userPassword: PAnsiChar; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecGenerateWriteKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetWriteKey(codec)^, userPassword, passwordLength);
end;

procedure CodecAESProcess(page: cardinal; data: PAnsiChar; len: integer;
  aes: PAes; encrypt: boolean);
var
  plain: Int64;    // bytes 16..23 should always be unencrypted
  iv: THash128Rec; // is genuine and AES-protected (since not random)
begin
  if (len and AesBlockMod <> 0) or
     (len <= 0) or
     (integer(page) <= 0) then
    raise ESqlite3Exception.CreateUtf8(
      'CodecAESProcess(page=%,len=%)', [page, len]);
  iv.c0 := page xor 668265263; // prime-based initialization
  iv.c1 := page * 2654435761;
  iv.c2 := page * 2246822519;
  iv.c3 := page * 3266489917;
  if not ForceSQLite3LegacyAES then
    aes^.Encrypt(iv.b); // avoid potential brute force attack
  len := len shr AesBlockShift;
  if page = 1 then
    // ensure header bytes 16..23 are stored unencrypted
    if (PInt64(data)^ = SQLITE_FILE_HEADER128.lo) and
       (data[21] = #64) and
       (data[22] = #32) and
       (data[23] = #32) then
      if encrypt then
      begin
        plain := PInt64(data + 16)^;
        if ForceSQLite3AESCTR then
          aes^.DoBlocksCtr(@iv.b, data + 16, data + 16, len - 1)
        else
          aes^.DoBlocksOfb(@iv.b, data + 16, data + 16, len - 1);
        // 8..15 are encrypted bytes 16..23
        PInt64(data + 8)^ := PInt64(data + 16)^;
        PInt64(data + 16)^ := plain;
      end
      else
      begin
        PInt64(data + 16)^ := PInt64(data + 8)^;
        if ForceSQLite3AESCTR then
          aes^.DoBlocksCtr(@iv.b, data + 16, data + 16, len - 1)
        else
          aes^.DoBlocksOfb(@iv.b, data + 16, data + 16, len - 1);
        if (data[21] = #64) and
           (data[22] = #32) and
           (data[23] = #32) then
          PHash128(data)^ := SQLITE_FILE_HEADER128.b
        else
          FillZero(PHash128(data)^); // report incorrect password
      end
    else
      FillZero(PHash128(data)^)
  else
    // whole page encryption if not the first one
    if ForceSQLite3AESCTR then
      aes^.DoBlocksCtr(@iv.b, data, data, len)
    else
      aes^.DoBlocksOfb(@iv.b, data, data, len);
end;

function CodecEncrypt(codec: pointer; page: integer; data: PAnsiChar;
  len, useWriteKey: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecEncrypt';{$endif} export;
begin
  if useWriteKey = 1 then
     CodecAESProcess(page, data, len, CodecGetWriteKey(codec), true)
  else
     CodecAESProcess(page, data, len, CodecGetReadKey(codec), true);
  result := SQLITE_OK;
end;

function CodecDecrypt(codec: pointer; page: integer;
  data: PAnsiChar; len: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecDecrypt';{$endif} export;
begin
  CodecAESProcess(page, data, len, CodecGetReadKey(codec), false);
  result := SQLITE_OK;
end;

function CodecTerm(codec: pointer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecTerm';{$endif} export;
begin
  CodecGetReadKey(codec)^.Done;
  CodecGetWriteKey(codec)^.Done;
  result := SQLITE_OK;
end;

function ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUtf8): boolean;
var
  F: THandle;
  bufsize, page, pagesize, pagecount, n, p, read: cardinal;
  head: THash256Rec;
  buf: PAnsiChar;
  temp: RawByteString;
  size, posi: Int64;
  old, new: TAes;
begin
  result := false;
  if OldPassWord = NewPassword then
    exit;
  F := FileOpen(FileName, fmOpenReadWrite);
  if ValidHandle(F) then
  try
    if OldPassWord <> '' then
      CodecGenerateKey(old, pointer(OldPassWord), length(OldPassWord));
    if NewPassword <> '' then
      CodecGenerateKey(new, pointer(NewPassword), length(NewPassword));
    size := FileSize(F);
    read := FileRead(F, head, SizeOf(head));
    if read <> SizeOf(head) then
      exit;
    if size > 4 shl 20 then // use up to 4MB of R/W buffer
      bufsize := 4 shl 20
    else
      bufsize := size;
    pagesize := cardinal(head.b[16]) shl 8 + head.b[17];
    pagecount := size div pagesize;
    if (pagesize < 1024) or
       (pagesize and AesBlockMod <> 0) or
       (pagesize > bufsize) or
       (QWord(pagecount) * pagesize <> size) or
       (head.d0 <> SQLITE_FILE_HEADER128.Lo) or
       ((head.d1 = SQLITE_FILE_HEADER128.Hi) <> (OldPassWord = '')) then
      exit;
    FileSeek64(F, 0, soFromBeginning);
    SetLength(temp, bufsize);
    posi := 0;
    page := 1;
    while page <= pagecount do
    begin
      n := bufsize div pagesize;
      read := pagecount - page + 1;
      if read < n then
        n := read;
      buf := pointer(temp);
      read := FileRead(F, buf^, pagesize * n);
      if read <> pagesize * n then
        exit; // stop on any read error
      for p := 0 to n - 1 do
      begin
        if OldPassWord <> '' then
        begin
          CodecAESProcess(page + p, buf, pagesize, @old, false);
          if (p = 0) and
             (page = 1) and
             (PInteger(buf)^ = 0) then
            exit; // OldPassword is obviously incorrect
        end;
        if NewPassword <> '' then
          CodecAESProcess(page + p, buf, pagesize, @new, true);
        inc(buf, pagesize);
      end;
      FileSeek64(F, posi, soFromBeginning);
      FileWrite(F, pointer(temp)^, pagesize * n); // update in-place
      inc(posi, pagesize * n);
      inc(page, n);
    end;
    result := true;
  finally
    FileClose(F);
    if OldPassWord <> '' then
      old.Done;
    if NewPassword <> '' then
      new.Done;
  end;
end;

function IsOldSQLEncryptTable(const FileName: TFileName): boolean;
var
  F: THandle;
  Header: array[0..2047] of byte;
begin
  result := false;
  F := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if not ValidHandle(F) then
    exit;
  if (FileRead(F, Header, SizeOf(Header)) = SizeOf(Header)) and
     // see https://www.sqlite.org/fileformat.html (4 in big endian = 1024 bytes)
     (PWord(@Header[16])^ = 4) and
     IsEqual(PHash128(@Header)^, SQLITE_FILE_HEADER128.b) then
    if not (Header[1024] in [5, 10, 13]) then
      // B-tree leaf Type to be either 5 (interior) 10 (index) or 13 (table)
      result := true;
  FileClose(F);
end;

procedure OldSQLEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUtf8);
const
  SQLEncryptTableSize = $4000;

  procedure CreateSqlEncryptTableBytes(const PassWord: RawUtf8; Table: PByteArray);
  // very fast table (private key) computation from a given password
  // - use a simple prime-based random generator, strong enough for common use
  // - execution speed and code size was the goal here: can be easily broken
  // - SynCrypto proposes SHA-256 and AES-256 for more secure encryption
  var
    i, j, k, L: integer;
  begin
    L := length(PassWord) - 1;
    j := 0;
    k := integer(L * ord(PassWord[1])) + 134775813; // initial value, prime number derivated
    for i := 0 to SQLEncryptTableSize - 1 do
    begin
      Table^[i] := (ord(PassWord[j + 1])) xor byte(k);
      k := integer(k * 3 + i); // fast prime-based pseudo random generator
      if j = L then
        j := 0
      else
        inc(j);
    end;
  end;

  procedure XorOffset(P: PByte; Index, Count: cardinal; SQLEncryptTable: PByteArray);
  var
    Len: cardinal;
  begin
    // deprecated fast and simple Cypher using Index (= offset in file)
    if Count > 0 then
      repeat
        Index := Index and (SQLEncryptTableSize - 1);
        Len := SQLEncryptTableSize - Index;
        if Len > Count then
          Len := Count;
        XorMemory(pointer(P), @SQLEncryptTable^[Index], Len);
        inc(P, Len);
        inc(Index, Len);
        dec(Count, Len);
      until Count = 0;
  end;

var
  F: THandle;
  R: integer;
  Buf: array[word] of byte; // temp buffer for read/write (64KB is enough)
  Size, Posi: Int64;
  OldP: array[0..SQLEncryptTableSize - 1] of byte; // 2x16KB tables
begin
  F := FileOpen(FileName, fmOpenReadWrite);
  if not ValidHandle(F) then
    exit;
  Size := FileSize(F);
  if Size <= 1024 then
  begin
    FileClose(F); // file is to small to be modified
    exit;
  end;
  if OldPassWord <> '' then
    CreateSqlEncryptTableBytes(OldPassWord, @OldP);
  Posi := 1024; // don't change first page, which is uncrypted
  FileSeek(F, 1024, soFromBeginning);
  while Posi < Size do
  begin
    R := FileRead(F, Buf, sizeof(Buf)); // read buffer
    if R < 0 then
      break; // stop on any read error
    if OldPassWord <> '' then
      XorOffset(@Buf, Posi, R, @OldP); // uncrypt with old key
    FileSeek64(F, Posi, soFromBeginning);
    FileWrite(F, Buf, R); // update buffer
    inc(Posi, cardinal(R));
  end;
  FileClose(F);
end;

function sqlite3_initialize: integer; cdecl; external;
function sqlite3_shutdown: integer; cdecl; external;
function sqlite3_open(filename: PUtf8Char; var DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_open_v2(filename: PUtf8Char; var DB: TSqlite3DB; flags: integer; vfs: PUtf8Char): integer; cdecl; external;
function sqlite3_close(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_key(DB: TSqlite3DB; key: pointer; keyLen: integer): integer; cdecl; external;
function sqlite3_rekey(DB: TSqlite3DB; key: pointer; keyLen: integer): integer; cdecl; external;
function sqlite3_create_function(DB: TSqlite3DB; FunctionName: PUtf8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSqlFunctionFunc;
  xFinal: TSqlFunctionFinal): integer; cdecl; external;
function sqlite3_create_function_v2(DB: TSqlite3DB; FunctionName: PUtf8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSqlFunctionFunc;
  xFinal: TSqlFunctionFinal; xDestroy: TSqlDestroyPtr): integer; cdecl; external;
function sqlite3_create_window_function(DB: TSqlite3DB; FunctionName: PUtf8Char;
  nArg, eTextRep: integer; pApp: pointer; xStep: TSqlFunctionFunc;
  xFinal, xValue: TSqlFunctionFinal; xInverse: TSqlFunctionFunc; xDestroy: TSqlDestroyPtr): integer;   cdecl; external;
function sqlite3_create_collation(DB: TSqlite3DB; CollationName: PUtf8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSqlCollateFunc): integer; cdecl; external;
function sqlite3_libversion: PUtf8Char; cdecl; external;
function sqlite3_errmsg(DB: TSqlite3DB): PAnsiChar; cdecl; external;
function sqlite3_extended_errcode(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_last_insert_rowid(DB: TSqlite3DB): Int64; cdecl; external;
function sqlite3_busy_timeout(DB: TSqlite3DB; Milliseconds: integer): integer; cdecl; external;
function sqlite3_busy_handler(DB: TSqlite3DB;
  CallbackPtr: TSqlBusyHandler; user: Pointer): integer;  cdecl; external;
function sqlite3_prepare_v2(DB: TSqlite3DB; SQL: PUtf8Char; SQL_bytes: integer;
  var S: TSqlite3Statement; var SQLtail: PUtf8Char): integer; cdecl; external;
function sqlite3_finalize(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_next_stmt(DB: TSqlite3DB; S: TSqlite3Statement): TSqlite3Statement; cdecl; external;
function sqlite3_reset(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_stmt_readonly(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_step(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_column_count(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_column_type(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_decltype(S: TSqlite3Statement; Col: integer): PAnsiChar; cdecl; external;
function sqlite3_column_name(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_bytes(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_value(S: TSqlite3Statement; Col: integer): TSqlite3Value; cdecl; external;
function sqlite3_column_double(S: TSqlite3Statement; Col: integer): double; cdecl; external;
function sqlite3_column_int(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_int64(S: TSqlite3Statement; Col: integer): int64; cdecl; external;
function sqlite3_column_text(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_text16(S: TSqlite3Statement; Col: integer): PWideChar; cdecl; external;
function sqlite3_column_blob(S: TSqlite3Statement; Col: integer): PAnsiChar; cdecl; external;
function sqlite3_value_type(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_numeric_type(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_bytes(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_double(Value: TSqlite3Value): double; cdecl; external;
function sqlite3_value_int64(Value: TSqlite3Value): Int64; cdecl; external;
function sqlite3_value_text(Value: TSqlite3Value): PUtf8Char; cdecl; external;
function sqlite3_value_blob(Value: TSqlite3Value): pointer; cdecl; external;
procedure sqlite3_result_null(Context: TSqlite3FunctionContext); cdecl; external;
procedure sqlite3_result_int64(Context: TSqlite3FunctionContext; Value: Int64); cdecl; external;
procedure sqlite3_result_double(Context: TSqlite3FunctionContext; Value: double); cdecl; external;
procedure sqlite3_result_blob(Context: TSqlite3FunctionContext; Value: Pointer;
  Value_bytes: integer=0; DestroyPtr: TSqlDestroyPtr=SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_text(Context: TSqlite3FunctionContext; Value: PUtf8Char;
  Value_bytes: integer=-1; DestroyPtr: TSqlDestroyPtr=SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_value(Context: TSqlite3FunctionContext; Value: TSqlite3Value); cdecl; external;
procedure sqlite3_result_error(Context: TSqlite3FunctionContext; Msg: PUtf8Char; MsgLen: integer=-1); cdecl; external;
function sqlite3_user_data(Context: TSqlite3FunctionContext): pointer; cdecl; external;
function sqlite3_context_db_handle(Context: TSqlite3FunctionContext): TSqlite3DB; cdecl; external;
function sqlite3_aggregate_context(Context: TSqlite3FunctionContext;
   nBytes: integer): pointer; cdecl; external;
function sqlite3_bind_text(S: TSqlite3Statement; Param: integer;
  Text: PUtf8Char; Text_bytes: integer=-1; DestroyPtr: TSqlDestroyPtr=SQLITE_TRANSIENT): integer;
  cdecl; external;
function sqlite3_bind_blob(S: TSqlite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
  DestroyPtr: TSqlDestroyPtr=SQLITE_TRANSIENT): integer; cdecl; external;
function sqlite3_bind_zeroblob(S: TSqlite3Statement; Param: integer; Size: integer): integer; cdecl; external;
function sqlite3_bind_double(S: TSqlite3Statement; Param: integer; Value: double): integer; cdecl; external;
function sqlite3_bind_int(S: TSqlite3Statement; Param: integer; Value: integer): integer; cdecl; external;
function sqlite3_bind_int64(S: TSqlite3Statement; Param: integer; Value: Int64): integer; cdecl; external;
function sqlite3_bind_null(S: TSqlite3Statement; Param: integer): integer; cdecl; external;
function sqlite3_clear_bindings(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_bind_parameter_count(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_blob_open(DB: TSqlite3DB; DBName, TableName, ColumnName: PUtf8Char;
  RowID: Int64; Flags: integer; var Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_blob_reopen(DB: TSqlite3DB; RowID: Int64): integer; cdecl; external;
function sqlite3_blob_close(Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_blob_read(Blob: TSqlite3Blob; const Data; Count, Offset: integer): integer; cdecl; external;
function sqlite3_blob_write(Blob: TSqlite3Blob; const Data; Count, Offset: integer): integer; cdecl; external;
function sqlite3_blob_bytes(Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_create_module_v2(DB: TSqlite3DB; const zName: PAnsiChar;
  var p: TSqlite3Module; pClientData: Pointer; xDestroy: TSqlDestroyPtr): integer; cdecl; external;
function sqlite3_declare_vtab(DB: TSqlite3DB; const zSQL: PAnsiChar): integer; cdecl; external;
function sqlite3_set_authorizer(DB: TSqlite3DB; xAuth: TSqlAuthorizerCallback;
  pUserData: Pointer): integer;   cdecl; external;
function sqlite3_update_hook(DB: TSqlite3DB; xCallback: TSqlUpdateCallback;
  pArg: pointer): pointer; cdecl; external;
function sqlite3_commit_hook(DB: TSqlite3DB; xCallback: TSqlCommitCallback;
  pArg: Pointer): Pointer; cdecl; external;
function sqlite3_rollback_hook(DB: TSqlite3DB;  xCallback: TSqlCommitCallback;
  pArg: Pointer): Pointer; cdecl; external;
function sqlite3_changes(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_total_changes(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_malloc(n: integer): Pointer; cdecl; external;
function sqlite3_realloc(pOld: Pointer; n: integer): Pointer; cdecl; external;
procedure sqlite3_free(p: Pointer); cdecl; external;
function sqlite3_memory_used: Int64; cdecl; external;
function sqlite3_memory_highwater(resetFlag: integer): Int64; cdecl; external;
function sqlite3_limit(DB: TSqlite3DB; id,newValue: integer): integer; cdecl; external;
function sqlite3_backup_init(DestDB: TSqlite3DB; DestDatabaseName: PUtf8Char;
  SourceDB: TSqlite3DB; SourceDatabaseName: PUtf8Char): TSqlite3Backup; cdecl; external;
function sqlite3_backup_step(Backup: TSqlite3Backup; nPages: integer): integer; cdecl; external;
function sqlite3_backup_finish(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_backup_remaining(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_backup_pagecount(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_serialize(DB: TSqlite3DB; Schema: PUtf8Char; Size: PInt64;
  Flags: integer): pointer; cdecl; external;
function sqlite3_deserialize(DB: TSqlite3DB; Schema: PUtf8Char; Data: pointer;
  DBSize, BufSize: Int64; Flags: integer): pointer; cdecl; external;
function sqlite3_config(operation: integer): integer; cdecl varargs; external;
function sqlite3_db_config(DB: TSqlite3DB; operation: integer): integer; cdecl varargs; external;
function sqlite3_trace_v2(DB: TSqlite3DB; Mask: integer; Callback: TSqlTraceCallback;
  UserData: Pointer): Pointer; cdecl; external;


{ TSqlite3LibraryStatic }

const
  // error message if statically linked sqlite3.o(bj) does not match this
  // - Android version may be a little behind, so we are more releaxed here
  EXPECTED_SQLITE3_VERSION =
    {$ifdef OSANDROID} '3.34' {$else} '3.34.1' {$endif};

  // where to download the latest available static binaries, including SQLite3
  EXPECTED_STATIC_DOWNLOAD =
    'https://github.com/synopse/mORMot2/releases/tag/sqlite.' + EXPECTED_SQLITE3_VERSION;

constructor TSqlite3LibraryStatic.Create;
var
  error: RawUtf8;
begin
  initialize             := @sqlite3_initialize;
  shutdown               := @sqlite3_shutdown;
  open                   := @sqlite3_open;
  open_v2                := @sqlite3_open_v2;
  key                    := @sqlite3_key;
  rekey                  := @sqlite3_rekey;
  close                  := @sqlite3_close;
  libversion             := @sqlite3_libversion;
  errmsg                 := @sqlite3_errmsg;
  extended_errcode       := @sqlite3_extended_errcode;
  create_function        := @sqlite3_create_function;
  create_function_v2     := @sqlite3_create_function_v2;
  create_window_function := @sqlite3_create_window_function;
  create_collation       := @sqlite3_create_collation;
  last_insert_rowid      := @sqlite3_last_insert_rowid;
  busy_timeout           := @sqlite3_busy_timeout;
  busy_handler           := @sqlite3_busy_handler;
  prepare_v2             := @sqlite3_prepare_v2;
  finalize               := @sqlite3_finalize;
  next_stmt              := @sqlite3_next_stmt;
  reset                  := @sqlite3_reset;
  stmt_readonly          := @sqlite3_stmt_readonly;
  step                   := @sqlite3_step;
  column_count           := @sqlite3_column_count;
  column_type            := @sqlite3_column_type;
  column_decltype        := @sqlite3_column_decltype;
  column_name            := @sqlite3_column_name;
  column_bytes           := @sqlite3_column_bytes;
  column_value           := @sqlite3_column_value;
  column_double          := @sqlite3_column_double;
  column_int             := @sqlite3_column_int;
  column_int64           := @sqlite3_column_int64;
  column_text            := @sqlite3_column_text;
  column_text16          := @sqlite3_column_text16;
  column_blob            := @sqlite3_column_blob;
  value_type             := @sqlite3_value_type;
  value_numeric_type     := @sqlite3_value_numeric_type;
  value_bytes            := @sqlite3_value_bytes;
  value_double           := @sqlite3_value_double;
  value_int64            := @sqlite3_value_int64;
  value_text             := @sqlite3_value_text;
  value_blob             := @sqlite3_value_blob;
  result_null            := @sqlite3_result_null;
  result_int64           := @sqlite3_result_int64;
  result_double          := @sqlite3_result_double;
  result_blob            := @sqlite3_result_blob;
  result_text            := @sqlite3_result_text;
  result_value           := @sqlite3_result_value;
  result_error           := @sqlite3_result_error;
  user_data              := @sqlite3_user_data;
  context_db_handle      := @sqlite3_context_db_handle;
  aggregate_context      := @sqlite3_aggregate_context;
  bind_text              := @sqlite3_bind_text;
  bind_blob              := @sqlite3_bind_blob;
  bind_zeroblob          := @sqlite3_bind_zeroblob;
  bind_double            := @sqlite3_bind_double;
  bind_int               := @sqlite3_bind_int;
  bind_int64             := @sqlite3_bind_int64;
  bind_null              := @sqlite3_bind_null;
  clear_bindings         := @sqlite3_clear_bindings;
  bind_parameter_count   := @sqlite3_bind_parameter_count;
  blob_open              := @sqlite3_blob_open;
  blob_reopen            := @sqlite3_blob_reopen;
  blob_close             := @sqlite3_blob_close;
  blob_read              := @sqlite3_blob_read;
  blob_write             := @sqlite3_blob_write;
  blob_bytes             := @sqlite3_blob_bytes;
  create_module_v2       := @sqlite3_create_module_v2;
  declare_vtab           := @sqlite3_declare_vtab;
  set_authorizer         := @sqlite3_set_authorizer;
  update_hook            := @sqlite3_update_hook;
  commit_hook            := @sqlite3_commit_hook;
  rollback_hook          := @sqlite3_rollback_hook;
  changes                := @sqlite3_changes;
  total_changes          := @sqlite3_total_changes;
  malloc                 := @sqlite3_malloc;
  realloc                := @sqlite3_realloc;
  free_                  := @sqlite3_free;
  memory_used            := @sqlite3_memory_used;
  memory_highwater       := @sqlite3_memory_highwater;
  trace_v2               := @sqlite3_trace_v2;
  limit                  := @sqlite3_limit;
  backup_init            := @sqlite3_backup_init;
  backup_step            := @sqlite3_backup_step;
  backup_finish          := @sqlite3_backup_finish;
  backup_remaining       := @sqlite3_backup_remaining;
  backup_pagecount       := @sqlite3_backup_pagecount;
  serialize              := @sqlite3_serialize;
  deserialize            := @sqlite3_deserialize;
  config                 := @sqlite3_config;
  db_config              := @sqlite3_db_config;

  // our static SQLite3 is compiled with SQLITE_OMIT_AUTOINIT defined
  {$ifdef FPC}
  ForceToUseSharedMemoryManager; // before sqlite3_initialize otherwise SQLITE_MISUSE
  {$else}
  {$ifdef CPUX86}
  fUseInternalMM := true; // Delphi .obj are using FastMM4
  {$else}
  ForceToUseSharedMemoryManager; // Delphi .o
  {$endif CPUX86}
  {$endif FPC}
  sqlite3_initialize;
  inherited Create; // set fVersionNumber/fVersionText
  if (EXPECTED_SQLITE3_VERSION <> '') and
     not IdemPChar(pointer(fVersionText), EXPECTED_SQLITE3_VERSION) then
  begin
    FormatUtf8('Static SQLite3 library as included within % is outdated!' + CRLF +
      'Linked version is % whereas the current/expected is ' + EXPECTED_SQLITE3_VERSION +
      '.' + CRLF + CRLF + 'Please download latest SQLite3 ' + EXPECTED_SQLITE3_VERSION +
      ' revision from'+ CRLF + EXPECTED_STATIC_DOWNLOAD,
      [Executable.ProgramName, fVersionText], error);
    // SQLite3Log.Add.Log() would do nothing: we are in .exe initialization
    DisplayFatalError(' WARNING: deprecated SQLite3 engine', error);
  end;
end;

destructor TSqlite3LibraryStatic.Destroy;
begin
  if Assigned(shutdown) then
    shutdown;
  inherited;
end;


initialization
  FreeAndNil(sqlite3);
  sqlite3 := TSqlite3LibraryStatic.Create;

{$endif NOSQLITE3STATIC} // conditional defined -> auto-load local .dll/.so

end.

