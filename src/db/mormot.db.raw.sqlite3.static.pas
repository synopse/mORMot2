/// Database Framework Low-Level Statically Linked SQLite3 Engine
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.sqlite3.static;

{
  *****************************************************************************

    Statically linked SQLite3 3.39.1 engine with optional AES encryption
    - TSqlite3LibraryStatic Implementation
    - Encryption-Related Functions

      Just include this unit in your uses clause, and the mormot.db.raw.sqlite3
    sqlite3 global variable will be filled with linked .o/.obj API entries -
    ensure you downloaded latest https://synopse.info/files/mormot2static.7z
      If the platform is not supported yet, fallback loading a system library.
      To patch and compile the official SQlite3 amalgamation file, follow the
    instruction from the res/static/sqlite3 folder.

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
      DisplayFatalError(SQLITE_LIBRARY_DEFAULT_NAME + ' initialization failed',
        RawUtf8(E.ClassName +  ': ' + E.Message));
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
  mormot.crypt.core, // for our AES encryption
  mormot.crypt.secure,
  mormot.lib.static,
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
    /// calls ForceToUseSharedMemoryManager after SQlite3 is loaded
    procedure BeforeInitialization; override;
    /// validates EXPECTED_SQLITE3_VERSION after SQlite3 is initialized
    procedure AfterInitialization; override;
  end;


{ ************ Encryption-Related Functions }

/// use this procedure to change the password for an existing SQLite3 database file
// - convenient and faster alternative to the sqlite3.rekey() API call
// - conversion is done in-place at file level, with no SQL nor BTree pages
// involved, therefore it can process very big files with best possible speed
// - the OldPassWord must be correct, otherwise the resulting file will be corrupted
// - any password can be '' to mark no encryption as input or output
// - password will use AES-128 (see ForceSQLite3AesCtr) after PBKDF2 SHAKE_128
// with rounds=1000 or a JSON (extended) serialized TSynSignerParams object like
// ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
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
// see OldSqlEncryptTablePassWordToPlain() to convert your existing databases
function ChangeSqlEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUtf8): boolean;

/// this function may be used to create a plain database file from an existing
// one encrypted with our old/deprecated/unsupported format (<1.18.4413)
// - then call ChangeSqlEncryptTablePassWord() to convert to the new safer format
procedure OldSqlEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUtf8);

/// could be used to detect a database in old/deprecated/unsupported format (<1.18.4413)
// - to call OldSqlEncryptTablePassWordToPlain + ChangeSqlEncryptTablePassWord
// and switch to the new format
function IsOldSqlEncryptTable(const FileName: TFileName): boolean;

var
  /// global flag to use initial AES encryption scheme
  // - IV derivation was hardened in revision 1.18.4607 - set TRUE to this
  // global constant to use the former implementation (theoritically slightly
  // less resistant to brute force attacks) and convert existing databases
  ForceSQLite3LegacyAes: boolean;

  /// global flag to use AES-CTR instead of AES-OFB encryption
  // - on x86_64 our optimized asm is 2.5GB/s for CTR instead of 770MB/s for OFB
  // - enabled in PUREMORMOT2 mode, since performance is better on x86_64 (or OpenSSL)
  // - set ForceSQLite3AesCtr := true for compatibility reasons with mORMot 1
  ForceSQLite3AesCtr: boolean {$ifdef PUREMORMOT2} = true {$endif};



implementation

{$ifndef FPC}
{$ifdef OSWINDOWS}
uses
  Windows; // statically linked Delphi .obj requires the Windows API
{$endif OSWINDOWS}
{$endif FPC}


{ ************ TSqlite3LibraryStatic Implementation }

// ---------------- link raw .o .obj files

// see res/static/libsqlite3 for patched source and build instructions

{$ifdef FPC}  // FPC expects .o linking

  {$ifdef OSWINDOWS}
    {$ifdef CPU64}
      {$L ..\..\static\x86_64-win64\sqlite3.o}
    {$else}
      {$L ..\..\static\i386-win32\sqlite3.o}
    {$endif CPU64}
  {$endif OSWINDOWS}

  {$ifdef OSDARWIN}
    {$ifdef CPU64}
      {$linklib ..\..\static\x86_64-darwin\libsqlite3.a}
    {$else}
      {$linklib ..\..\static\i386-darwin\libsqlite3.a}
    {$endif}
  {$endif OSDARWIN}

  {$ifdef OSANDROID}
    {$ifdef CPUAARCH64}
      {$L ..\..\static\aarch64-android\sqlite3.o}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L ..\..\static\arm-android\sqlite3.o}
    {$endif CPUARM}
    {$ifdef CPUX86}
      {$L ..\..\static\i386-android\sqlite3.o}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {$L ..\..\static\x86_64-android\sqlite3.o}
      // x86_64-linux-android-ld.bfd: final link failed
      // (Nonrepresentable section on output)
    {$endif CPUX64}
  {$endif OSANDROID}

  {$ifdef OSFREEBSD}
    {$ifdef CPUX86}
    {$L ..\..\static\i386-freebsd\sqlite3.o}
    {$endif CPUX86}
    {$ifdef CPUX64}
    {$L ..\..\static\x86_64-freebsd\sqlite3.o}
    {$endif CPUX64}
  {$endif OSFREEBSD}

  {$ifdef OSOPENBSD}
    {$ifdef CPUX86}
      {$L ..\..\static\i386-openbsd\sqlite3.o}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {$L ..\..\static\x86_64-openbsd\sqlite3.o}
    {$endif CPUX64}
  {$endif OSOPENBSD}

  {$ifdef OSLINUX}
    {$ifdef CPUAARCH64}
      {$L ..\..\static\aarch64-linux\sqlite3.o}
      {$linklib ..\..\static\aarch64-linux\libgcc.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L ..\..\static\arm-linux\sqlite3.o}
      {$linklib ..\..\static\arm-linux\libgcc.a}
    {$endif CPUARM}
    {$ifdef CPUX86}
      {$L ..\..\static\i386-linux\sqlite3.o}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {$L ..\..\static\x86_64-linux\sqlite3.o}
    {$endif CPUX64}
  {$endif OSLINUX}

{$else FPC} // Delphi static linking has diverse expectations

  // Delphi has a diverse linking strategy, since $linklib doesn't exist :(
  {$ifdef OSWINDOWS}
    {$ifdef CPU64}
      // compiled with C++ Builder 10.3 Community Edition bcc64
      {$L ..\..\static\delphi\sqlite3.o}
    {$else}
      // compiled with the free Borland C++ Compiler 5.5
      {$L ..\..\static\delphi\sqlite3.obj}
    {$endif}
  {$endif OSWINDOWS}

// those functions will be called only under Delphi + Win32/Win64
// - FPC will use explicit public name exports from mormot.lib.static
// but Delphi requires the exports to be defined in this very same unit

function malloc(size: cardinal): Pointer; cdecl;
begin
  GetMem(result, size);
end;

procedure free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function realloc(P: Pointer; Size: integer): Pointer; cdecl;
begin
  ReallocMem(P, Size);
  result := P;
end;

function rename(oldname, newname: PUtf8Char): integer; cdecl; 
begin
  result := libc_rename(oldname, newname);
end;

{$ifdef OSWINDOWS}

{$ifdef CPU32} // Delphi Win32 will link static C++ Builder sqlite3.obj

// we then implement all needed C++ Builder runtime functions in pure pascal:

function _ftol: Int64;
// C++ Builder float to integer (Int64) conversion
asm
          jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function _ftoul: Int64;
// C++ Builder float to integer (Int64) conversion
asm
          jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

var
  __turbofloat: word; { not used, but must be present for linking }

// C++ Builder and Delphi share the same low level Int64 _ll*() functions:

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

function log(const val: double): double; cdecl;
asm
          fld qword ptr val
          fldln2
          fxch
          fyl2x
end;

{$endif CPU32}

{$endif OSWINDOWS}

function memset(P: Pointer; B: integer; count: integer): pointer; cdecl;
// a fast full pascal version of the standard C library function
begin
  FillCharFast(P^, count, B);
  result := P;
end;

function memmove(dest, source: pointer; count: integer): pointer; cdecl;
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count); // move() is overlapping-friendly
  result := dest;
end;

function memcpy(dest, source: Pointer; count: integer): pointer; cdecl;
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count);
  result := dest;
end;

function strlen(p: PUtf8Char): integer; cdecl;
// a fast full pascal version of the standard C library function
begin
  // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := mormot.core.base.StrLen(p);
end;

function strcmp(p1, p2: PUtf8Char): integer; cdecl;
// a fast full pascal version of the standard C library function
begin
  // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := mormot.core.base.StrComp(p1, p2);
end;

function strcspn(str, reject: PUtf8Char): integer; cdecl;
begin
  // called e.g. during LIKE process
  result := mormot.core.unicode.strcspn(str, reject);
end;

function strrchr(s: PUtf8Char; c: AnsiChar): PUtf8Char; cdecl;
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

function memcmp(p1, p2: pByte; Size: integer): integer; cdecl;
begin
  result := libc_memcmp(p1, p2, Size);
end;

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl;  
begin
  result := libc_strncmp(p1, p2, Size);
end;

procedure qsort(baseP: PByte; NElem, Width: PtrInt; comparF: qsort_compare_func); cdecl;
begin
  libc_qsort(baseP, NElem, Width, comparF);
end;

{$ifdef OSWINDOWS}

var
  { as standard C library documentation states:
  Statically allocated buffer, shared by the functions gmtime() and localtime().
  Each call of these functions overwrites the content of this structure.
  -> this buffer is shared, but SQlite3 will protect it with a mutex :) }
  atm: time_t;

function localtime(t: PCardinal): pointer; cdecl;
begin
  localtime32_s(t^, atm);
  result := @atm;
end;

function _beginthreadex(security: pointer; stksize: dword;
  start, arg: pointer; flags: dword; var threadid: dword): THandle; cdecl;
  external _CLIB;

procedure _endthreadex(exitcode: dword); cdecl;
  external _CLIB;

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

{
 Our SQlite3 static files includes a SQLite3MultipleCiphers VFS for encryption.
 See https://github.com/synopse/mORMot2/tree/master/res/static/libsqlite3
 The SQLite3 source is not patched to implement the VFS itself (it is not
 mandatory), but is patched to add some key-related high-level features - see
 https://utelle.github.io/SQLite3MultipleCiphers/docs/architecture/arch_patch
 VFS codecext.c and sqlite3mc.c redirect to those external functions below.

 Encryption is done in CodecAESProcess() using AES-CTR or AES-OFB. It uses mORMot
 optimized asm, which is faster than OpenSSL on x86_64 (our main server target).
 Each page is encrypted with an IV derived from its page number using AES. By
 default with mORMot a page size is 4KB. No overhead is used for IV or HMAC
 storage. The first bytes of the files are not encrypted, because it is mandatory
 for proper work with most SQLite3 tools. This is what all other libraries do,
 including the official (but not free) SSE extension from SQLite3 authors - see
 https://utelle.github.io/SQLite3MultipleCiphers/docs/ciphers/cipher_legacy_mode

 Key derivation from password is done in CodecGenerateKey() using PBKDF2 safe
 iterative key derivation over the SHA-3 (SHAKE_128) algorithm, reduced into
 128-bit. There is no benefit of using AES-256 in practice, because a 128-bit
 password using a 80-character alphabet (i.e. very strong computerized password)
 already require at least 21 chars, which is very unlikely in practice.
 It uses 1000 rounds by default, and you can customize the password derivation
 using overridden parameters in JSON format instead of the plain password,
 following TSynSignerParams fields.
}

const
  CODEC_PBKDF2_SALT = 'J6CuDftfPr22FnYn';

procedure CodecGenerateKey(var aes: TAes;
  userPassword: pointer; passwordLength: integer);
var
  s: TSynSigner;
  k: THash512Rec;
begin
  // userPassword may be TSynSignerParams JSON content
  s.Pbkdf2(userPassword, passwordLength, k, CODEC_PBKDF2_SALT);
  s.AssignTo(k, aes, {encrypt=}true);
end;

function CodecGetReadKey(codec: pointer):  PAes; cdecl; external;
function CodecGetWriteKey(codec: pointer): PAes; cdecl; external;

procedure CodecGenerateReadKey(codec: pointer;
  userPassword: PUtf8Char; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecGenerateReadKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetReadKey(codec)^, userPassword, passwordLength);
end;

procedure CodecGenerateWriteKey(codec: pointer;
  userPassword: PUtf8Char; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecGenerateWriteKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetWriteKey(codec)^, userPassword, passwordLength);
end;

procedure CodecAesProcess(page: cardinal; data: PUtf8Char; len: integer;
  aes: PAes; encrypt: boolean);
var
  plain: Int64;    // bytes 16..23 should always be unencrypted
  iv: THash128Rec; // is genuine and AES-protected (since not random)
begin
  if (len and AesBlockMod <> 0) or
     (len <= 0) or
     (integer(page) <= 0) then
    raise ESqlite3Exception.CreateUtf8(
      'Unexpected CodecAesProcess(page=%,len=%)', [page, len]);
  iv.c0 := page xor 668265263; // prime-based initialization
  iv.c1 := page * 2654435761;
  iv.c2 := page * 2246822519;
  iv.c3 := page * 3266489917;
  if not ForceSQLite3LegacyAes then
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
        if ForceSQLite3AesCtr then
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
        if ForceSQLite3AesCtr then
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
    if ForceSQLite3AesCtr then
      aes^.DoBlocksCtr(@iv.b, data, data, len) // faster on x86_64 SSE4.1
    else
      aes^.DoBlocksOfb(@iv.b, data, data, len);
end;

function CodecEncrypt(codec: pointer; page: integer; data: PUtf8Char;
  len, useWriteKey: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecEncrypt';{$endif} export;
begin
  if useWriteKey = 1 then
     CodecAesProcess(page, data, len, CodecGetWriteKey(codec), true)
  else
     CodecAesProcess(page, data, len, CodecGetReadKey(codec), true);
  result := SQLITE_OK;
end;

function CodecDecrypt(codec: pointer; page: integer;
  data: PUtf8Char; len: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecDecrypt';{$endif} export;
begin
  CodecAesProcess(page, data, len, CodecGetReadKey(codec), false);
  result := SQLITE_OK;
end;

function CodecTerm(codec: pointer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX + 'CodecTerm';{$endif} export;
begin
  CodecGetReadKey(codec)^.Done;
  CodecGetWriteKey(codec)^.Done;
  result := SQLITE_OK;
end;

function ChangeSqlEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUtf8): boolean;
var
  F: THandle;
  bufsize, page, pagesize, pagecount, n, p, read: cardinal;
  head: THash256Rec;
  buf: PUtf8Char;
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
          CodecAesProcess(page + p, buf, pagesize, @old, false);
          if (p = 0) and
             (page = 1) and
             (PInteger(buf)^ = 0) then
            exit; // OldPassword is obviously incorrect
        end;
        if NewPassword <> '' then
          CodecAesProcess(page + p, buf, pagesize, @new, true);
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

function IsOldSqlEncryptTable(const FileName: TFileName): boolean;
var
  F: THandle;
  hdr: array[0..2047] of byte;
begin
  result := false;
  F := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if not ValidHandle(F) then
    exit;
  if (FileRead(F, hdr, SizeOf(hdr)) = SizeOf(hdr)) and
     // see https://www.sqlite.org/fileformat.html (4 in bigendian = 1024 bytes)
     (PWord(@hdr[16])^ = 4) and
     IsEqual(PHash128(@hdr)^, SQLITE_FILE_HEADER128.b) then
    if not (hdr[1024] in [5, 10, 13]) then
      // B-tree leaf Type to be either 5 (interior) 10 (index) or 13 (table)
      result := true;
  FileClose(F);
end;

procedure OldSqlEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUtf8);
const
  OLDENCRYPTTABLESIZE = $4000;

  procedure CreateSqlEncryptTableBytes(const PassWord: RawUtf8; Table: PByteArray);
  // very fast table (private key) computation from a given password
  // - execution speed and code size was the goal here: can be easily broken
  // - the new encryption scheme is both safer and more performant
  var
    i, j, k, L: integer;
  begin
    L := length(PassWord) - 1;
    j := 0;
    k := integer(L * ord(PassWord[1])) + 134775813; // initial value, prime based
    for i := 0 to OLDENCRYPTTABLESIZE - 1 do
    begin
      Table^[i] := (ord(PassWord[j + 1])) xor byte(k);
      k := integer(k * 3 + i); // fast prime-based pseudo random generator
      if j = L then
        j := 0
      else
        inc(j);
    end;
  end;

  procedure XorOffset(P: PByte; Index, Count: cardinal; SqlEncryptTable: PByteArray);
  var
    len: cardinal;
  begin
    // deprecated fast and simple Cypher using Index (= offset in file)
    if Count > 0 then
      repeat
        Index := Index and (OLDENCRYPTTABLESIZE - 1);
        len := OLDENCRYPTTABLESIZE - Index;
        if len > Count then
          len := Count;
        XorMemory(pointer(P), @SqlEncryptTable^[Index], len);
        inc(P, len);
        inc(Index, len);
        dec(Count, len);
      until Count = 0;
  end;

var
  F: THandle;
  R: integer;
  buf: array[word] of byte; // temp buffer for read/write (64KB seems enough)
  size, posi: Int64;
  oldtable: array[0..OLDENCRYPTTABLESIZE - 1] of byte; // 2x16KB tables
begin
  F := FileOpen(FileName, fmOpenReadWrite);
  if not ValidHandle(F) then
    exit;
  size := FileSize(F);
  if size <= 1024 then
  begin
    FileClose(F); // file is to small to be modified
    exit;
  end;
  if OldPassWord <> '' then
    CreateSqlEncryptTableBytes(OldPassWord, @oldtable);
  posi := 1024; // don't change first page, which is uncrypted
  FileSeek64(F, 1024, soFromBeginning);
  while posi < size do
  begin
    R := FileRead(F, buf, SizeOf(buf)); // read buffer
    if R < 0 then
      break; // stop on any read error
    if OldPassWord <> '' then
      XorOffset(@buf, posi, R, @oldtable); // uncrypt with oldtable key
    FileSeek64(F, posi, soFromBeginning);
    FileWrite(F, buf, R); // update buffer
    inc(posi, cardinal(R));
  end;
  FileClose(F);
end;

function sqlite3_initialize: integer; cdecl; external;
function sqlite3_shutdown: integer; cdecl; external;
function sqlite3_open(filename: PUtf8Char; var DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_open_v2(filename: PUtf8Char; var DB: TSqlite3DB; flags: integer;
  vfs: PUtf8Char): integer; cdecl; external;
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
  xFinal, xValue: TSqlFunctionFinal; xInverse: TSqlFunctionFunc;
  xDestroy: TSqlDestroyPtr): integer; cdecl; external;
procedure sqlite3_set_auxdata(Context: TSqlite3FunctionContext; N: integer;
  Value: pointer; DestroyPtr: TSqlDestroyPtr); cdecl; external;
function sqlite3_get_auxdata(Context: TSqlite3FunctionContext; N: integer): pointer; cdecl; external;
function sqlite3_create_collation(DB: TSqlite3DB; CollationName: PUtf8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSqlCollateFunc): integer; cdecl; external;
function sqlite3_libversion: PUtf8Char; cdecl; external;
function sqlite3_libversion_number: integer; cdecl; external;
function sqlite3_sourceid: PUtf8Char; cdecl; external;
function sqlite3_threadsafe: integer; cdecl; external;
function sqlite3_errcode(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_extended_errcode(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_errmsg(DB: TSqlite3DB): PUtf8Char; cdecl; external;
function sqlite3_errstr(Code: integer): PUtf8Char; cdecl; external;
function sqlite3_system_errno(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_extended_result_codes(DB: TSqlite3DB; OnOff: integer): integer; cdecl; external;
function sqlite3_complete(SQL: PUtf8Char): integer; cdecl; external;
function sqlite3_keyword_count: integer; cdecl; external;
function sqlite3_keyword_name(Nth: integer; var Identifier: PUtf8Char;
  L: PInteger): integer; cdecl; external;
function sqlite3_keyword_check(Identifier: PUtf8Char; L: integer): integer; cdecl; external;
function sqlite3_txn_state(DB: TSqlite3DB; SchemaName: PUtf8Char): integer; cdecl; external;
function sqlite3_create_collation_v2(DB: TSqlite3DB; CollationName: PUtf8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSqlCollateFunc;
  DestroyPtr: TSqlDestroyPtr): integer; cdecl; external;
function sqlite3_collation_needed(DB: TSqlite3DB; CollateParam: pointer;
  Callback: TSqlCollationNeededCallback): integer; cdecl; external;
function sqlite3_last_insert_rowid(DB: TSqlite3DB): Int64; cdecl; external;
procedure sqlite3_set_last_insert_rowid(DB: TSqlite3DB; R: Int64); cdecl; external;
function sqlite3_busy_timeout(DB: TSqlite3DB; Milliseconds: integer): integer; cdecl; external;
function sqlite3_busy_handler(DB: TSqlite3DB;
  CallbackPtr: TSqlBusyHandler; user: Pointer): integer;  cdecl; external;
function sqlite3_prepare_v2(DB: TSqlite3DB; SQL: PUtf8Char; SQL_bytes: integer;
  var S: TSqlite3Statement; var SQLtail: PUtf8Char): integer; cdecl; external;
function sqlite3_prepare_v3(DB: TSqlite3DB; SQL: PUtf8Char; SQL_bytes: integer;
  prepFlags: Cardinal; var S: TSqlite3Statement; var SQLtail: PUtf8Char): integer; cdecl; external;
function sqlite3_finalize(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_exec(DB: TSqlite3DB; SQL: PUtf8Char; Callback: TSqlExecCallback;
  UserData: Pointer; var ErrorMsg: PUtf8Char): integer; cdecl; external;
function sqlite3_next_stmt(DB: TSqlite3DB; S: TSqlite3Statement): TSqlite3Statement; cdecl; external;
function sqlite3_reset(S: TSqlite3Statement): integer; cdecl; external;
procedure sqlite3_interrupt(DB: TSqlite3DB); cdecl; external;
procedure sqlite3_progress_handler(DB: TSqlite3DB; N: integer; Callback: TSqlProgressCallback;
  UserData: pointer); cdecl; external;
function sqlite3_stmt_busy(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_stmt_isexplain(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_stmt_readonly(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_stmt_scanstatus(S: TSqlite3Statement; idx: integer; iScanStatusOp: integer;
  pOut: pointer): integer; cdecl; external;
procedure sqlite3_stmt_scanstatus_reset(S: TSqlite3Statement); cdecl; external;
function sqlite3_stmt_status(S: TSqlite3Statement; Operation: integer;
  resetFlag: integer): integer; cdecl; external;
function sqlite3_db_handle(S: TSqlite3Statement): TSqlite3DB; cdecl; external;
function sqlite3_sql(S: TSqlite3Statement): PUtf8Char; cdecl; external;
function sqlite3_expanded_sql(S: TSqlite3Statement): PUtf8Char; cdecl; external;
function sqlite3_normalized_sql(S: TSqlite3Statement): PUtf8Char; cdecl; external;
function sqlite3_step(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_table_column_metadata(DB: TSqlite3DB; zDbName, zTableName, zColumnName: PUtf8Char;
  var pzDataType, pzCollSeq: PUtf8Char;
  out pNotNull, pPrimaryKey, pAutoinc: integer): integer; cdecl; external;
function sqlite3_column_count(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_column_type(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_decltype(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_name(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_database_name(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_table_name(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_origin_name(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_bytes(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_value(S: TSqlite3Statement; Col: integer): TSqlite3Value; cdecl; external;
function sqlite3_column_double(S: TSqlite3Statement; Col: integer): double; cdecl; external;
function sqlite3_column_int(S: TSqlite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_int64(S: TSqlite3Statement; Col: integer): int64; cdecl; external;
function sqlite3_column_text(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_column_text16(S: TSqlite3Statement; Col: integer): PWideChar; cdecl; external;
function sqlite3_column_blob(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl; external;
function sqlite3_value_type(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_subtype(Value: TSqlite3Value): cardinal; cdecl; external;
function sqlite3_value_numeric_type(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_nochange(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_frombind(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_bytes(Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_value_dup(Value: TSqlite3Value): TSqlite3Value; cdecl; external;
procedure sqlite3_value_free(Value: TSqlite3Value); cdecl; external;
function sqlite3_value_pointer(Value: TSqlite3Value; Typ: PUtf8Char): pointer; cdecl; external;
function sqlite3_value_double(Value: TSqlite3Value): double; cdecl; external;
function sqlite3_value_int64(Value: TSqlite3Value): Int64; cdecl; external;
function sqlite3_value_text(Value: TSqlite3Value): PUtf8Char; cdecl; external;
function sqlite3_value_blob(Value: TSqlite3Value): pointer; cdecl; external;
procedure sqlite3_result_pointer(Context: TSqlite3FunctionContext; Value: pointer;
  Typ: PUtf8Char; DestroyPtr: TSqlDestroyPtr); cdecl; external;
procedure sqlite3_result_null(Context: TSqlite3FunctionContext); cdecl; external;
procedure sqlite3_result_int64(Context: TSqlite3FunctionContext; Value: Int64); cdecl; external;
procedure sqlite3_result_double(Context: TSqlite3FunctionContext; Value: double); cdecl; external;
procedure sqlite3_result_blob(Context: TSqlite3FunctionContext; Value: Pointer;
  Value_bytes: integer = 0; DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_zeroblob(Context: TSqlite3FunctionContext; Value_bytes: integer); cdecl; external;
procedure sqlite3_result_text(Context: TSqlite3FunctionContext; Value: PUtf8Char;
  Value_bytes: integer = -1; DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_value(Context: TSqlite3FunctionContext; Value: TSqlite3Value); cdecl; external;
procedure sqlite3_result_subtype(Context: TSqlite3FunctionContext; Value: cardinal); cdecl; external;
procedure sqlite3_result_error(Context: TSqlite3FunctionContext; Msg: PUtf8Char;
  MsgLen: integer = -1); cdecl; external;
function sqlite3_user_data(Context: TSqlite3FunctionContext): pointer; cdecl; external;
function sqlite3_context_db_handle(Context: TSqlite3FunctionContext): TSqlite3DB; cdecl; external;
function sqlite3_aggregate_context(Context: TSqlite3FunctionContext; nBytes: integer): pointer; cdecl; external;
function sqlite3_bind_text(S: TSqlite3Statement; Param: integer;Text: PUtf8Char;
  Text_bytes: integer = -1; DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT): integer;cdecl; external;
function sqlite3_bind_blob(S: TSqlite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
  DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT): integer; cdecl; external;
function sqlite3_bind_zeroblob(S: TSqlite3Statement; Param: integer; Size: integer): integer; cdecl; external;
function sqlite3_bind_double(S: TSqlite3Statement; Param: integer; Value: double): integer; cdecl; external;
function sqlite3_bind_int(S: TSqlite3Statement; Param: integer; Value: integer): integer; cdecl; external;
function sqlite3_bind_int64(S: TSqlite3Statement; Param: integer; Value: Int64): integer; cdecl; external;
function sqlite3_bind_null(S: TSqlite3Statement; Param: integer): integer; cdecl; external;
function sqlite3_bind_pointer(S: TSqlite3Statement; Param: integer; Value: pointer; Typ: PUtf8Char;
  DestroyPtr: TSqlDestroyPtr): integer; cdecl; external;
function sqlite3_bind_value(S: TSqlite3Statement; Param: integer; Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_clear_bindings(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_bind_parameter_count(S: TSqlite3Statement): integer; cdecl; external;
function sqlite3_bind_parameter_index(S: TSqlite3Statement; ParamName: PUtf8Char): integer; cdecl; external;
function sqlite3_bind_parameter_name(S: TSqlite3Statement; Param: integer): PUtf8Char; cdecl; external;
function sqlite3_blob_open(DB: TSqlite3DB; DBName, TableName, ColumnName: PUtf8Char;
  RowID: Int64; Flags: integer; var Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_blob_reopen(DB: TSqlite3DB; RowID: Int64): integer; cdecl; external;
function sqlite3_blob_close(Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_blob_read(Blob: TSqlite3Blob; const Data; Count, Offset: integer): integer; cdecl; external;
function sqlite3_blob_write(Blob: TSqlite3Blob; const Data; Count, Offset: integer): integer; cdecl; external;
function sqlite3_blob_bytes(Blob: TSqlite3Blob): integer; cdecl; external;
function sqlite3_create_module_v2(DB: TSqlite3DB; const zName: PUtf8Char;
  var p: TSqlite3Module; pClientData: Pointer; xDestroy: TSqlDestroyPtr): integer; cdecl; external;
function sqlite3_drop_modules(DB: TSqlite3DB; azKeep: PUtf8Char): integer; cdecl; external;
function sqlite3_declare_vtab(DB: TSqlite3DB; const zSQL: PUtf8Char): integer; cdecl; external;
function sqlite3_vtab_collation(var IndexInfo: TSqlite3IndexInfo; Index: integer): PUtf8Char; cdecl; external;
function sqlite3_vtab_config(DB: TSqlite3DB; op: integer): integer; cdecl; external;
function sqlite3_vtab_nochange(Context: TSqlite3FunctionContext): integer; cdecl; external;
function sqlite3_vtab_on_conflict(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_overload_function(DB: TSqlite3DB; zFuncName: PUtf8Char; nArg: integer): integer; cdecl; external;
function sqlite3_auto_extension(xEntryPoint: TSqlEntryPointCallback): integer; cdecl; external;
function sqlite3_cancel_auto_extension(xEntryPoint: TSqlEntryPointCallback): integer; cdecl; external;
procedure sqlite3_reset_auto_extension; cdecl; external;
function sqlite3_load_extension(DB: TSqlite3DB; zFile, zProc: PUtf8Char;
  var pzErrMsg: PUtf8Char): integer; cdecl; external;
function sqlite3_get_autocommit(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_set_authorizer(DB: TSqlite3DB; xAuth: TSqlAuthorizerCallback;
  pUserData: Pointer): integer; cdecl; external;
function sqlite3_preupdate_hook(DB: TSqlite3DB; xCallback: TSqlPreUpdateCallback;
  pArg: pointer): pointer; cdecl; external;
function sqlite3_preupdate_old(DB: TSqlite3DB; N: integer; var Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_preupdate_new(DB: TSqlite3DB; N: integer; var Value: TSqlite3Value): integer; cdecl; external;
function sqlite3_preupdate_count(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_preupdate_depth(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_unlock_notify(pBlocked: TSqlite3DB; xNotify: TSqlUnlockNotify;
  pArg: Pointer): Pointer; cdecl; external;
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
function sqlite3_msize(p: Pointer): Int64; cdecl; external;
function sqlite3_release_memory(N: integer): integer; cdecl; external;
function sqlite3_db_release_memory(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_memory_used: Int64; cdecl; external;
function sqlite3_memory_highwater(resetFlag: integer): Int64; cdecl; external;
function sqlite3_status64(Operation: integer; pCurrent, pHighwater: PInt64;
  resetFlag: integer): integer; cdecl; external;
function sqlite3_db_status(DB: TSqlite3DB; Operation: integer; pCurrent, pHighwater: PInteger;
  resetFlag: integer): Integer; cdecl; external;
function sqlite3_db_cacheflush(DB: TSqlite3DB): integer; cdecl; external;
function sqlite3_db_filename(DB: TSqlite3DB; DBName: PUtf8Char): PUtf8Char; cdecl; external;
function sqlite3_db_readonly(DB: TSqlite3DB; DBName: PUtf8Char): integer; cdecl; external;
function sqlite3_limit(DB: TSqlite3DB; id,newValue: integer): integer; cdecl; external;
function sqlite3_backup_init(DestDB: TSqlite3DB; DestDatabaseName: PUtf8Char;
  SourceDB: TSqlite3DB; SourceDatabaseName: PUtf8Char): TSqlite3Backup; cdecl; external;
function sqlite3_backup_step(Backup: TSqlite3Backup; nPages: integer): integer; cdecl; external;
function sqlite3_backup_finish(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_backup_remaining(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_backup_pagecount(Backup: TSqlite3Backup): integer; cdecl; external;
function sqlite3_serialize(DB: TSqlite3DB; Schema: PUtf8Char; Size: PInt64;
  Flags: cardinal): pointer; cdecl; external;
function sqlite3_deserialize(DB: TSqlite3DB; Schema: PUtf8Char; Data: pointer;
  DBSize, BufSize: Int64; Flags: cardinal): pointer; cdecl; external;
function sqlite3_wal_hook(DB: TSqlite3DB; Callback: TSqlWalHookCallback; UserData: pointer): pointer; cdecl; external;
function sqlite3_wal_autocheckpoint(DB: TSqlite3DB; N: integer): integer; cdecl; external;
function sqlite3_wal_checkpoint_v2(DB: TSqlite3DB; zDb: PUtf8Char; eMode: integer;
  var pnLog, pnCkpt: integer): integer; cdecl; external;
function sqlite3_snapshot_get(DB: TSqlite3DB; zSchema: PUtf8Char;
  var Snapshot: PSqlite3Snapshot): integer; cdecl; external;
function sqlite3_snapshot_open(DB: TSqlite3DB; zSchema: PUtf8Char;
  Snapshot: PSqlite3Snapshot): integer; cdecl; external;
function sqlite3_snapshot_recover(DB: TSqlite3DB; zDB: PUtf8Char): integer; cdecl; external;
function sqlite3_snapshot_cmp(DB: TSqlite3DB; P1, P2: PSqlite3Snapshot): integer; cdecl; external;
function sqlite3_snapshot_free(DB: TSqlite3DB; Snapshot: PSqlite3Snapshot): integer; cdecl; external;
function sqlite3_soft_heap_limit64(N: Int64): Int64; cdecl; external;
function sqlite3_config(operation: integer): integer; cdecl varargs; external;
function sqlite3_db_config(DB: TSqlite3DB; operation: integer): integer; cdecl varargs; external;
function sqlite3_trace_v2(DB: TSqlite3DB; Mask: integer; Callback: TSqlTraceCallback;
  UserData: Pointer): Pointer; cdecl; external;
function sqlite3_error_offset(DB: TSqlite3DB): integer; cdecl; external;


{ TSqlite3LibraryStatic }

const
  // error message if statically linked sqlite3.o(bj) does not match this value
  EXPECTED_SQLITE3_VERSION = '3.39.1';

  // the github release tag associated with this EXPECTED_SQLITE3_VERSION
  // - you could download the static for this exact mORMot source revision e.g. as
  // https://github.com/synopse/mORMot2/releases/download/2.0.3717/mormot2static.7z
  EXPECTED_RELEASE_TAG = '2.0.3717';

  // where to download the latest available static binaries, including SQLite3
  EXPECTED_STATIC_DOWNLOAD = 'https://synopse.info/files/mormot2static.7z';


constructor TSqlite3LibraryStatic.Create;
begin
  initialize             := @sqlite3_initialize;
  shutdown               := @sqlite3_shutdown;
  open                   := @sqlite3_open;
  open_v2                := @sqlite3_open_v2;
  key                    := @sqlite3_key;
  rekey                  := @sqlite3_rekey;
  close                  := @sqlite3_close;
  libversion             := @sqlite3_libversion;
  libversion_number      := @sqlite3_libversion_number;
  sourceid               := @sqlite3_sourceid;
  threadsafe             := @sqlite3_threadsafe;
  errcode                := @sqlite3_errcode;
  extended_errcode       := @sqlite3_extended_errcode;
  errmsg                 := @sqlite3_errmsg;
  errstr                 := @sqlite3_errstr;
  system_errno           := @sqlite3_system_errno;
  extended_result_codes  := @sqlite3_extended_result_codes;
  complete               := @sqlite3_complete;
  keyword_count          := @sqlite3_keyword_count;
  keyword_name           := @sqlite3_keyword_name;
  keyword_check          := @sqlite3_keyword_check;
  txn_state              := @sqlite3_txn_state;
  create_function        := @sqlite3_create_function;
  create_function_v2     := @sqlite3_create_function_v2;
  create_window_function := @sqlite3_create_window_function;
  set_auxdata            := @sqlite3_set_auxdata;
  get_auxdata            := @sqlite3_get_auxdata;
  create_collation       := @sqlite3_create_collation;
  create_collation_v2    := @sqlite3_create_collation_v2;
  collation_needed       := @sqlite3_collation_needed;
  last_insert_rowid      := @sqlite3_last_insert_rowid;
  set_last_insert_rowid  := @sqlite3_set_last_insert_rowid;
  busy_timeout           := @sqlite3_busy_timeout;
  busy_handler           := @sqlite3_busy_handler;
  prepare_v2             := @sqlite3_prepare_v2;
  prepare_v3             := @sqlite3_prepare_v3;
  finalize               := @sqlite3_finalize;
  exec                   := @sqlite3_exec;
  next_stmt              := @sqlite3_next_stmt;
  reset                  := @sqlite3_reset;
  interrupt              := @sqlite3_interrupt;
  progress_handler       := @sqlite3_progress_handler;
  stmt_busy              := @sqlite3_stmt_busy;
  stmt_isexplain         := @sqlite3_stmt_isexplain;
  stmt_readonly          := @sqlite3_stmt_readonly;
  stmt_scanstatus        := @sqlite3_stmt_scanstatus;
  stmt_scanstatus_reset  := @sqlite3_stmt_scanstatus_reset;
  stmt_status            := @sqlite3_stmt_status;
  db_handle              := @sqlite3_db_handle;
  sql                    := @sqlite3_sql;
  expanded_sql           := @sqlite3_expanded_sql;
  normalized_sql         := @sqlite3_normalized_sql;
  step                   := @sqlite3_step;
  table_column_metadata  := @sqlite3_table_column_metadata;
  column_count           := @sqlite3_column_count;
  column_type            := @sqlite3_column_type;
  column_decltype        := @sqlite3_column_decltype;
  column_name            := @sqlite3_column_name;
  column_database_name   := @sqlite3_column_database_name;
  column_table_name      := @sqlite3_column_table_name;
  column_origin_name     := @sqlite3_column_origin_name;
  column_bytes           := @sqlite3_column_bytes;
  column_value           := @sqlite3_column_value;
  column_double          := @sqlite3_column_double;
  column_int             := @sqlite3_column_int;
  column_int64           := @sqlite3_column_int64;
  column_text            := @sqlite3_column_text;
  column_text16          := @sqlite3_column_text16;
  column_blob            := @sqlite3_column_blob;
  value_type             := @sqlite3_value_type;
  value_subtype          := @sqlite3_value_subtype;
  value_numeric_type     := @sqlite3_value_numeric_type;
  value_nochange         := @sqlite3_value_nochange;
  value_frombind         := @sqlite3_value_frombind;
  value_bytes            := @sqlite3_value_bytes;
  value_dup              := @sqlite3_value_dup;
  value_free             := @sqlite3_value_free;
  value_pointer          := @sqlite3_value_pointer;
  value_double           := @sqlite3_value_double;
  value_int64            := @sqlite3_value_int64;
  value_text             := @sqlite3_value_text;
  value_blob             := @sqlite3_value_blob;
  result_pointer         := @sqlite3_result_pointer;
  result_null            := @sqlite3_result_null;
  result_int64           := @sqlite3_result_int64;
  result_double          := @sqlite3_result_double;
  result_blob            := @sqlite3_result_blob;
  result_zeroblob        := @sqlite3_result_zeroblob;
  result_text            := @sqlite3_result_text;
  result_value           := @sqlite3_result_value;
  result_subtype         := @sqlite3_result_subtype;
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
  bind_pointer           := @sqlite3_bind_pointer;
  bind_value             := @sqlite3_bind_value;
  clear_bindings         := @sqlite3_clear_bindings;
  bind_parameter_count   := @sqlite3_bind_parameter_count;
  bind_parameter_index   := @sqlite3_bind_parameter_index;
  bind_parameter_name    := @sqlite3_bind_parameter_name;
  blob_open              := @sqlite3_blob_open;
  blob_reopen            := @sqlite3_blob_reopen;
  blob_close             := @sqlite3_blob_close;
  blob_read              := @sqlite3_blob_read;
  blob_write             := @sqlite3_blob_write;
  blob_bytes             := @sqlite3_blob_bytes;
  create_module_v2       := @sqlite3_create_module_v2;
  drop_modules           := @sqlite3_drop_modules;
  declare_vtab           := @sqlite3_declare_vtab;
  vtab_collation         := @sqlite3_vtab_collation;
  vtab_config            := @sqlite3_vtab_config;
  vtab_nochange          := @sqlite3_vtab_nochange;
  vtab_on_conflict       := @sqlite3_vtab_on_conflict;
  overload_function      := @sqlite3_overload_function;
  auto_extension         := @sqlite3_auto_extension;
  cancel_auto_extension  := @sqlite3_cancel_auto_extension;
  reset_auto_extension   := @sqlite3_reset_auto_extension;
  load_extension         := @sqlite3_load_extension;
  get_autocommit         := @sqlite3_get_autocommit;
  set_authorizer         := @sqlite3_set_authorizer;
  preupdate_hook         := @sqlite3_preupdate_hook;
  preupdate_old          := @sqlite3_preupdate_old;
  preupdate_new          := @sqlite3_preupdate_new;
  preupdate_count        := @sqlite3_preupdate_count;
  preupdate_depth        := @sqlite3_preupdate_depth;
  unlock_notify          := @sqlite3_unlock_notify;
  update_hook            := @sqlite3_update_hook;
  commit_hook            := @sqlite3_commit_hook;
  rollback_hook          := @sqlite3_rollback_hook;
  changes                := @sqlite3_changes;
  total_changes          := @sqlite3_total_changes;
  malloc                 := @sqlite3_malloc;
  realloc                := @sqlite3_realloc;
  free_                  := @sqlite3_free;
  msize                  := @sqlite3_msize;
  release_memory         := @sqlite3_release_memory;
  db_release_memory      := @sqlite3_db_release_memory;
  memory_used            := @sqlite3_memory_used;
  memory_highwater       := @sqlite3_memory_highwater;
  status64               := @sqlite3_status64;
  db_status              := @sqlite3_db_status;
  db_cacheflush          := @sqlite3_db_cacheflush;
  db_filename            := @sqlite3_db_filename;
  db_readonly            := @sqlite3_db_readonly;
  trace_v2               := @sqlite3_trace_v2;
  limit                  := @sqlite3_limit;
  backup_init            := @sqlite3_backup_init;
  backup_step            := @sqlite3_backup_step;
  backup_finish          := @sqlite3_backup_finish;
  backup_remaining       := @sqlite3_backup_remaining;
  backup_pagecount       := @sqlite3_backup_pagecount;
  serialize              := @sqlite3_serialize;
  deserialize            := @sqlite3_deserialize;
  wal_hook               := @sqlite3_wal_hook;
  wal_autocheckpoint     := @sqlite3_wal_autocheckpoint;
  wal_checkpoint_v2      := @sqlite3_wal_checkpoint_v2;
  snapshot_get           := @sqlite3_snapshot_get;
  snapshot_open          := @sqlite3_snapshot_open;
  snapshot_recover       := @sqlite3_snapshot_recover;
  snapshot_cmp           := @sqlite3_snapshot_cmp;
  snapshot_free          := @sqlite3_snapshot_free;
  soft_heap_limit64      := @sqlite3_soft_heap_limit64;
  config                 := @sqlite3_config;
  db_config              := @sqlite3_db_config;
  error_offset           := @sqlite3_error_offset;

  // ForceToUseSharedMemoryManager call before initialize otherwise SQLITE_MISUSE
  BeforeInitialization;
  // note: our static SQLite3 is compiled with SQLITE_OMIT_AUTOINIT defined
  sqlite3_initialize;
  // set fVersionNumber/fVersionText and call AfterInitialization
  inherited Create;
end;

destructor TSqlite3LibraryStatic.Destroy;
begin
  if Assigned(shutdown) then
    shutdown;
  inherited;
end;

procedure TSqlite3LibraryStatic.BeforeInitialization;
begin
  inherited BeforeInitialization; // set SQLITE_CONFIG_MULTITHREAD
  {$ifdef FPC}
  ForceToUseSharedMemoryManager;
  {$else}
  {$ifdef CPUX86}
  // Delphi .obj are using FastMM4 via malloc/free/realloc above functions
  fUseInternalMM := true;
  {$else}
  ForceToUseSharedMemoryManager; // Delphi .o
  {$endif CPUX86}
  {$endif FPC}
end;

procedure TSqlite3LibraryStatic.AfterInitialization;
var
  error: RawUtf8;
begin
  inherited AfterInitialization; // do nothing by default
  if (EXPECTED_SQLITE3_VERSION <> '') and
     not IdemPChar(pointer(fVersionText), EXPECTED_SQLITE3_VERSION) then
  begin
    FormatUtf8('Static SQLite3 library as included within % is outdated!' + CRLF +
      'Linked version is % whereas the current/expected is ' + EXPECTED_SQLITE3_VERSION +
      '.' + CRLF + CRLF + 'Please download latest SQLite3 ' + EXPECTED_SQLITE3_VERSION +
      ' revision from'+ CRLF + EXPECTED_STATIC_DOWNLOAD,
      [Executable.ProgramFileName, fVersionText], error);
    // SQLite3Log.Add.Log() would do nothing: we are in .exe initialization
    DisplayFatalError(' WARNING: deprecated SQLite3 engine', error);
  end;
end;


initialization
  FreeAndNil(sqlite3);
  sqlite3 := TSqlite3LibraryStatic.Create;

{$endif NOSQLITE3STATIC} // conditional defined -> auto-load local .dll/.so

end.
