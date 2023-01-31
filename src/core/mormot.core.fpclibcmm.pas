/// Use libc Memory Manager on FPC
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fpclibcmm;

{
  *****************************************************************************

   Include as first unit to use the libc memory manager on FPC + Linux
    - mormot.core.fpcx64mm shines on standard load over a few cores
    - mormot.core.fpclibcmm scales better on heavy load on > 16 cores

   WARNING/DISCLAMER:
    glibc free/realloc may scale better, but do abort/SIG_KILL on any GPF, so
    first ensure your project is very clean about its memory (using heaptrc)
    before running it on production - fpcx64mm or FPC RTL MM are less paranoid
    - getmem() is almost twice slower on single thread than fpcx64mm
    - so only use it if you know what you are doing, and really see a difference
    - consider it as a more direct (and correct?) alternative to FPC cmem unit

  *****************************************************************************
}

(*
  In practice, write in your main project (.dpr/.lpr) source:
  
  uses
    {$I mormot.uses.inc} // may include fpcx64mm or fpclibcmm
    sysutils,
    mormot.core.base,
    ...
    
  Then define either FPC_X64MM or FPC_LIBCMM conditional.
  If both are set, FPC_64MM will be used on x86_64, and FPC_LIBCMM otherwise.
  
  Note: we tried Intel TBB and jemalloc external libraries, but they consummed
  much more memory on heavy load (TBB seems not usable for server work).
*)

// this unit installs its own memory manager only for FPC + POSIX
// - Delphi/Windows targets would compile as a void unit

interface

// nothing published

implementation

{$ifdef FPC}
{$ifndef MSWINDOWS}

// low-level direct calls to the external libc library

function malloc(size: PtrUInt): pointer;
  cdecl; external 'c' name 'malloc';

function calloc(count, size: PtrUInt): pointer;
  cdecl; external 'c' name 'calloc';

procedure free(p: pointer);
  cdecl; external 'c' name 'free';

function realloc(p: pointer; size: PtrUInt): pointer;
  cdecl; external 'c' name 'realloc';


// TMemoryManager replacement

function _GetMem(size: PtrUInt): pointer;
begin
  if PtrInt(size) <= 0 then
    size := 1; // as FPC RTL: _Getmem(0) returns _Getmem(1)
  result := malloc(size);
end;

function _FreeMem(p: pointer): PtrUInt;
begin
  free(p);
  result := 0; // only used by heaptrc
end;

function _FreeMemSize(p: pointer; size: PtrUInt): PtrUInt;
begin
  // our unit won't check the "size" value (not mandatory)
  if size <> 0 then
    free(p);
  result := 0; // should return the chunk size - only used by heaptrc anyway
end;

function _AllocMem(size: PtrUInt): pointer;
begin
  result := calloc(size, 1); // no need to call FillChar() e.g. from mmap
end;

function _ReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  result := realloc(p, size); // is free(p) if size=0 or malloc(size) if p=nil
  p := result;
end;

// we assume that MemSize() could return 0, so we won't store any length prefix
// - FPC fpc_AnsiStr_SetLength() will always reallocate (rely on realloc)
// - TSqlite3Library will detect it and use its own wrapper with a prefix
// - note: the FPC RTL cmem unit triggers some errors during mormot2tests on
// x86_64 (at least) whereas this unit won't

{$ifdef LINUX}

// any recent Linux glibc has this API call to retrieve a size from a pointer
// - may be missing on other platforms, so this code is enabled only on LINUX
// https://www.gnu.org/software/gnulib/manual/html_node/malloc_005fusable_005fsize.html
// = MacOS 11.1, FreeBSD 6.0, NetBSD 9.0, OpenBSD 6.7, Minix 3.1.8, AIX 5.1,
// HP-UX 11.00, IRIX 6.5, Solaris 11.4, mingw, MSVC 14, BeOS, Android 4.1.
function msize(p: pointer): PtrUInt;
  cdecl; external 'c' name 'malloc_usable_size';

// enable paranoid memory checks - but for SINGLE/MAIN thread only
// - see http://man7.org/linux/man-pages/man3/mcheck.3.html
function mcheck(abort: pointer): integer;
  cdecl external 'c' name 'mcheck';

function _MemSize(p: pointer): PtrUInt;
begin
  result := msize(p);
end;

{$else}

function _MemSize(p: pointer): PtrUInt;
begin
  result := 0;
  // good enough in practice: FPC fpc_AnsiStr_SetLength() will reallocate
  // and TSqlite3Library will detect it and use a prefix
end;

{$endif LINUX}


function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;


const
  NewMM: TMemoryManager = (
    NeedLock:         false;
    GetMem:           @_Getmem;
    FreeMem:          @_FreeMem;
    FreememSize:      @_FreememSize;
    AllocMem:         @_AllocMem;
    ReallocMem:       @_ReAllocMem;
    MemSize:          @_MemSize;
    InitThread:       nil;
    DoneThread:       nil;
    RelocateHeap:     nil;
    GetHeapStatus:    @_GetHeapStatus;
    GetFPCHeapStatus: @_GetFPCHeapStatus);

var
  OldMM: TMemoryManager;
  
initialization
  //mcheck(nil); // paranoid extended memory checks - NOT THREAD SAFE !!!
  GetMemoryManager(OldMM);
  SetMemoryManager(NewMM);

finalization
  SetMemoryManager(OldMM);

{$endif MSWINDOWS}
{$endif FPC}

end.

