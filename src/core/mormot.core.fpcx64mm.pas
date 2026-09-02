/// Fast Memory Manager for FPC x86_64
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fpcx64mm;

{
  *****************************************************************************

    A Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
    - targetting Linux (and Windows) multi-threaded Services
    - only for FPC on the x86_64 target - use the RTL MM on Delphi or ARM
    - based on proven FastMM4 by Pierre le Riche - with tuning and enhancements
    - can report detailed statistics (with threads contention and memory leaks)
    - three main app modes: default FPCMM_SERVER, FPCMM_GUI, or FPCMM_BOOSTER

    Usage: include this unit as the very first in your FPC project uses clause

    Why another Memory Manager on FPC?
    - The built-in heap.inc is well written and cross-platform and cross-CPU,
      but its threadvar arena for small blocks tends to consume a lot of memory
      on multi-threaded servers, and has suboptimal allocation performance
    - C memory managers (glibc, Intel TBB, jemalloc) have a very high RAM
      consumption (especially Intel TBB) and do panic/SIG_KILL on any GPF - but
      they were reported to scale better on heavy load with cpu core count > 16
      even if GetMem() is almost twice faster on single thread with fpcx64mm
    - Pascal alternatives (FastMM4,ScaleMM2,BrainMM) are Windows+Delphi specific
    - ThreadID hash with round-robin of arenas and FreeMem binlist are unique
      algorithms among Memory Managers, and match modern CPUs and workloads
    - It was so fun diving into SSE2 x86_64 assembly and Pierre's insight
    - Resulting code is still easy to understand and maintain

    DISCLAIMER: seems stable on Linux and Win64 but feedback is welcome!

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
  If both are set, FPC_X64MM will be used on x86_64, and FPC_LIBCMM otherwise.
*)


{ ---- Ready-To-Use Scenarios for Memory Manager Tuning }

{
  TL;DR:
    1. default FPCMM_SERVER is perfect for a multi-threaded app/service/daemon;
    2. set FPCMM_GUI for GUI/console almost-mono-threaded apps (FastMM4 mode);
    3. try FPCMM_BOOST or FPCMM_BOOSTER on high-end hardware;
    4. try mormot.core.fpclibcmm as POSIX alternative.
}

// target a multi-threaded service on a modern CPU (default)
// - define FPCMM_DEBUG, FPCMM_ASSUMEMULTITHREAD, FPCMM_ERMS, FPCMM_TINYPERTHREAD
// and FPCMM_MEDIUMTOLARGE
// - currently mormot2tests run with no contention when FPCMM_SERVER is set :)
{$define FPCMM_SERVER}

// increase FPCMM_SERVER settings for more aggressive multi-threaded process
// - tiny blocks will up to 256 bytes (instead of 128 bytes);
// - will enable FPCMM_SMALLNOTWITHMEDIUM to reduce medium sleeps.
{.$define FPCMM_BOOST}

// target high-end CPU/process when FPCMM_SERVER/FPCMM_BOOST are not enough
// - will use 128 arenas for <= 256B blocks to scale on high number of cores;
// - enable FPCMM_MULTIPLESMALLNOTWITHMEDIUM to reduce small pools locks;
// - enable FPCMM_MEDIUMPERTHREAD for 4 user-medium arenas on Linux/Win64.
{.$define FPCMM_BOOSTER}

// target a GUI/console mono-threaded app
// - disable all FPCMM_SERVER/FPCMM_BOOST/FPCMM_BOOSTER optimizations
// - consume less RSS memory from the OS and behave like stock FastMM4
{.$define FPCMM_GUI}


{ ---- Fine Grained Memory Manager Tuning }

// includes more detailed information to WriteHeapStatus()
{.$define FPCMM_DEBUG}

// on thread contention, don't spin executing "pause" but directly call Sleep()
// - may help on specific workloads, together with the FPCMM_OSYIELD conditional
{.$define FPCMM_NOPAUSE}

{.$define FPCMM_OSYIELD}
// for yielding in contention, sched_yield is usually considered better as it's
// designed for that, while nanosleep suits actual sleeps; but for our use case
// after some "pause" spinning, we prefer to reduce syscalls and rely on a well
// defined delay of 1us by default, which aligns with typical scheduler quanta
// - define this conditional if you want to experiment with sched_yield syscall

// let FPCMM_DEBUG include SleepCycles information from rdtsc
// and FPCMM_PAUSE call rdtsc for its spinnning loop
// - since rdtsc is emulated so unrealiable on VM, and it may even trigger a
// GPF if CR4.TSD bit is set on hardened systems, it is disabled by default
{.$define FPCMM_SLEEPTSC}

// checks leaks and write them to the console at process shutdown
// - only basic information will be included: more debugging information (e.g.
// call stack) may be gathered using heaptrc or valgrid
{.$define FPCMM_REPORTMEMORYLEAKS}

// won't check the IsMultiThread global, but assume it is true
// - multi-threaded apps (e.g. a Server Daemon instance) will be faster with it
// - mono-threaded (console/LCL) apps are faster without this conditional
{.$define FPCMM_ASSUMEMULTITHREAD}

// won't use mremap but a regular GetMem/move/FreeMem pattern for large blocks
// - depending on the actual system (e.g. on a VM), mremap may be slower
// - will disable Linux mremap() or Windows following block VirtualQuery/Alloc
{.$define FPCMM_NOMREMAP}

// customize mmap() allocation strategy
{.$define FPCMM_MEDIUM32BIT}   // enable MAP_32BIT for OsAllocMedium() on Linux
{.$define FPCMM_LARGEBIGALIGN} // THP alignment of large chunks to PMD_SIZE=2MB
{.$define FPCMM_LARGEPOPULATE} // use MAP_POPULATE flag for large blocks

// force the tiny/small blocks to be in their own arena, not with medium blocks
// - would use a little more memory, but medium pool is less likely to sleep
// - not defined for FPCMM_SERVER because no performance difference was found
// - defined for FPCMM_BOOST
{.$define FPCMM_SMALLNOTWITHMEDIUM}

// force several tiny/small blocks arenas, not with medium blocks
// - would use a little more memory, but more medium pools could help
// - defined for FPCMM_BOOSTER
{.$define FPCMM_MULTIPLESMALLNOTWITHMEDIUM}

// use the current thread id to identify a prefered arena for Tiny block GetMem
// - defined for FPCMM_SERVER since thread affinity matters even with 8 arenas
// - warning: Linux and Win64 ONLY, due to very low-level asm trick
{.$define FPCMM_TINYPERTHREAD}

// allocate any medium block > 48KB using the large allocator on contention
// - defined for FPCMM_SERVER since mmap/VirtualAlloc is cheaper than waiting
// - would slightly increase OS memory but usually seldom happens
{.$define FPCMM_MEDIUMTOLARGE}

// use the current thread id to identify one of 4 user-medium arenas
// - warning: EXPERIMENTAL Linux and Win64 ONLY, due to aligned OS allocations
// and very low-level asm tricks
{.$define FPCMM_MEDIUMPERTHREAD}

// use "rep movsb/stosd" ERMS for blocks > 256 bytes instead of SSE2 "movaps"
// - ERMS is available since Ivy Bridge, and we use "movaps" for smallest blocks
// (to not slow down older CPUs), so it is safe to enable this on FPCMM_SERVER
{.$define FPCMM_ERMS}

// try "cmp" before "lock cmpxchg" for old processors with huge lock penalty
{.$define FPCMM_CMPBEFORELOCK}

// will export libc-like functions, and not replace the FPC MM
// - e.g. to use this unit as a stand-alone C memory allocator
{.$define FPCMM_STANDALONE}

// this whole unit will compile as void
// - may be defined e.g. when compiled as Design-Time Lazarus package
{.$define FPCMM_DISABLE}

// by default, munmap/VirtualFree are not called at unit finalization
{.$define FPCMM_FULLCLEANUP}

interface

{$undef FPCX64MM_AVAILABLE}  // global conditional to enable this unit
{$ifdef FPC}
  {$ifdef CPUX64}            // this unit is for FPC + x86_64 only
    {$ifndef FPCMM_DISABLE}  // disabled on some targets/projects
      {$define FPCX64MM_AVAILABLE}
    {$endif FPCMM_DISABLE}
  {$endif CPUX64}
{$endif FPC}

{$ifdef FPCX64MM_AVAILABLE}
// this unit is available only for FPC + X86_64 CPU
// other targets would compile as a void unit

// cut-down version of mormot.defines.inc to make this unit standalone
{$mode Delphi}
{$inline on}
{$asmmode Intel}
{$R-} // disable Range checking
{$S-} // disable Stack checking
{$W-} // disable stack frame generation
{$Q-} // disable overflow checking
{$B-} // expect short circuit boolean

{$ifdef OLDLINUXKERNEL}
  {$define FPCMM_NOMREMAP}
{$endif OLDLINUXKERNEL}

{$ifdef FPCMM_GUI}
  {$undef FPCMM_SERVER}
  {$undef FPCMM_BOOST}
  {$undef FPCMM_BOOSTER}
{$else}
  {$ifdef FPCMM_BOOSTER}
    {$define FPCMM_BOOST}
    {$define FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
    {$define FPCMM_MEDIUMPERTHREAD}
  {$endif FPCMM_BOOSTER}
  {$ifdef FPCMM_BOOST}
    {$define FPCMM_SERVER}
    {$define FPCMM_SMALLNOTWITHMEDIUM}
    {$define FPCMM_LARGEBIGALIGN} // bigger blocks implies less reallocation
  {$endif FPCMM_BOOST}
  {$ifdef FPCMM_SERVER}
    {$define FPCMM_DEBUG}
    {$define FPCMM_ASSUMEMULTITHREAD}
    {$define FPCMM_ERMS}
    {$define FPCMM_TINYPERTHREAD} // thread affinity matters with a few threads
    {$define FPCMM_MEDIUMTOLARGE} // mmap is better than sleeping
  {$endif FPCMM_SERVER}
  {$ifdef FPCMM_BOOSTER}
    {$undef FPCMM_DEBUG} // when performance matters more than stats
  {$endif FPCMM_BOOSTER}
{$endif FPCMM_GUI}

type
  /// Arena (middle/large) heap information as returned by CurrentHeapStatus
  TMMStatusArena = record
    /// how many bytes are currently reserved (mmap) to the Operating System
    CurrentBytes: PtrUInt;
    /// how many bytes have been reserved (mmap) to the Operating System
    CumulativeBytes: PtrUInt;
    {$ifdef FPCMM_DEBUG}
    /// maximum bytes count reserved (mmap) to the Operating System
    PeakBytes: PtrUInt;
    /// how many VirtualAlloc/mmap calls to the Operating System did occur
    CumulativeAlloc: PtrUInt;
    /// how many VirtualFree/munmap calls to the Operating System did occur
    CumulativeFree: PtrUInt;
    {$endif FPCMM_DEBUG}
    /// how many times this Arena did wait from been unlocked by another thread
    SleepCount: PtrUInt;
  end;

  /// heap information as returned by CurrentHeapStatus
  TMMStatus = record
    /// how many tiny/small memory blocks (<=2600 bytes) are currently allocated
    SmallBlocks: PtrUInt;
    /// how many bytes of tiny/small memory blocks are currently allocated
    // - this size is included in Medium.CurrentBytes value, even if
    // FPCMM_SMALLNOTWITHMEDIUM has been defined
    SmallBlocksSize: PtrUInt;
    /// information about blocks up to 256KB (tiny, small and medium)
    // - includes also the memory needed for tiny/small blocks
    // - is shared by both small & medium pools even if FPCMM_SMALLNOTWITHMEDIUM
    Medium: TMMStatusArena;
    /// information about large blocks > 256KB
    // - those blocks are directly handled by the Operating System
    Large: TMMStatusArena;
    {$ifdef FPCMM_DEBUG}
    {$ifdef FPCMM_SLEEPTSC}
    /// how much rdtsc cycles were spent within SwitchToThread/nanosleep API
    // - we rdtsc since it is an indicative but very fast way of timing on
    // direct hardware
    // - warning: on virtual machines, the rdtsc opcode is usually emulated so
    // these SleepCycles number are non indicative anymore
    SleepCycles: PtrUInt;
    {$endif FPCMM_SLEEPTSC}
    {$endif FPCMM_DEBUG}
    /// how many times the Operating System Sleep/nanosleep API was called
    // - should be as small as possible - 0 is perfect
    SleepCount: PtrUInt;
    /// how many times GetMem() did block and wait for a tiny/small block
    // - see also GetSmallBlockContention() for more detailed information
    // - by design, our FreeMem() can't block thanks to its lock-less free list
    SmallGetmemSleepCount: PtrUInt;
  end;
  PMMStatus = ^TMMStatus;

/// allocate a new memory buffer
// - as FPC default heap, _Getmem(0) returns _Getmem(1)
function _GetMem(size: PtrUInt): pointer;

/// allocate a new zeroed memory buffer
function _AllocMem(Size: PtrUInt): pointer;

/// release a memory buffer
// - returns the allocated size of the supplied pointer (as FPC default heap)
function _FreeMem(P: pointer): PtrUInt;

/// change the size of a memory buffer
// - won't move any data if in-place reallocation is possible
// - as FPC default heap, _ReallocMem(P=nil,Size) maps P := _getmem(Size) and
// _ReallocMem(P,0) maps _Freemem(P)
function _ReallocMem(var P: pointer; Size: PtrUInt): pointer;

/// retrieve the allocated size of a memory buffer
// - equal or greater to the size supplied to _GetMem(), due to MM granularity
function _MemSize(P: pointer): PtrUInt; inline;

/// retrieve high-level statistics about the current memory manager state
// - see also GetSmallBlockContention for detailed small blocks information
// - standard GetHeapStatus and GetFPCHeapStatus gives less accurate information
// (only CurrHeapSize and MaxHeapSize are set), since we don't track "free" heap
// bytes: I can't figure how "free" memory is relevant nowadays - on 21th century
// Operating Systems, memory is virtual, and reserved/mapped by the OS but
// physically hosted in the HW RAM chips only when written the first time -
// GetHeapStatus information made sense on MSDOS with fixed 640KB of RAM
// - note that FPC GetHeapStatus and GetFPCHeapStatus is only about the
// current thread (irrelevant for sure) whereas CurrentHeapStatus is global
function CurrentHeapStatus: TMMStatus;

{$ifdef FPCMM_STANDALONE}

/// should be called before using any memory function
procedure InitializeMemoryManager;

/// should be called to finalize this memory manager process and release all RAM
procedure FreeAllMemory;

{$undef FPCMM_DEBUG} // excluded FPC-specific debugging

/// IsMultiThread global variable is not correct outside of the FPC RTL
{$define FPCMM_ASSUMEMULTITHREAD}
/// not supported to reduce dependencies and console writing
{$undef FPCMM_REPORTMEMORYLEAKS}

{$else}

type
  /// one GetSmallBlockContention info about unexpected multi-thread waiting
  TSmallBlockContention = packed record
    /// how many times a small block GetMem() has been waiting for unlock
    GetmemSleepCount: PtrUInt;
    /// the small block size on which GetMem() has been blocked
    GetmemBlockSize: PtrUInt;
    /// not used in GetSmallBlockContention() context - reserved for future use
    Reserved: PtrUInt;
  end;

  /// small blocks detailed information as returned GetSmallBlockContention
  TSmallBlockContentionDynArray = array of TSmallBlockContention;

  /// one GetSmallBlockStatus information
  TSmallBlockStatus = packed record
    /// how many times a memory block of this size has been allocated
    Total: PtrUInt;
    /// how many memory blocks of this size are currently allocated
    Current: PtrUInt;
    /// the standard size of the small memory block
    BlockSize: PtrUInt;
  end;

  /// small blocks detailed information as returned GetSmallBlockStatus
  TSmallBlockStatusDynArray = array of TSmallBlockStatus;

  /// sort order of detailed information as returned GetSmallBlockStatus
  TSmallBlockOrderBy = (
    obTotal,
    obCurrent,
    obBlockSize);

/// retrieve the use counts of allocated small blocks
// - returns maxcount biggest results, sorted by "orderby" field occurrence
function GetSmallBlockStatus(maxcount: integer = 10;
  orderby: TSmallBlockOrderBy = obTotal; count: PPtrUInt = nil; bytes: PPtrUInt = nil;
  small: PCardinal = nil; tiny: PCardinal = nil): TSmallBlockStatusDynArray;

/// retrieve all small blocks which suffered from blocking during multi-thread
// - returns maxcount biggest results, sorted by SleepCount Occurrence
function GetSmallBlockContention(
  maxcount: integer = 10): TSmallBlockContentionDynArray;


/// convenient debugging function into the console
// - if smallblockcontentioncount > 0, includes GetSmallBlockContention() info
// up to the smallblockcontentioncount biggest occurrences
// - see also RetrieveMemoryManagerInfo from mormot.core.log for runtime call
procedure WriteHeapStatus(const context: ShortString = '';
  smallblockstatuscount: integer = 8; smallblockcontentioncount: integer = 8;
  compilationflags: boolean = false);

/// convenient debugging function of the heap details into a text buffer
// - if smallblockcontentioncount > 0, includes GetSmallBlockContention() info
// up to the smallblockcontentioncount biggest occurrences
// - see also RetrieveMemoryManagerInfo from mormot.core.log for more details
// - warning: this function is not thread-safe, and return a global static buffer
function GetHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags, onsameline: boolean): PAnsiChar;

const
  // some pre-defined internal constants
  LargeBlockGranularityAnd  = (1 shl 16) - 1; // 64KB minimum for large blocks
  MediumBlockPoolSizeMem    = 20 shl 16;      // medium blocks are 1.25MB
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  MediumBlockAlignment     = 1 shl 21; // resolve pool header from any block
  MediumBlockAlignmentMask = MediumBlockAlignment - 1;
  {$endif FPCMM_MEDIUMPERTHREAD}

  /// human readable information about how our MM was built
  // - similar to WriteHeapStatus(compilationflags=true) output
  FPCMM_FLAGS = ' '
    {$ifdef FPCMM_BOOSTER}           + 'BOOSTER '     {$else}
      {$ifdef FPCMM_BOOST}           + 'BOOST '       {$else}
        {$ifdef FPCMM_SERVER}        + 'SERVER '      {$endif}
      {$endif FPCMM_BOOST}
    {$endif FPCMM_BOOSTER}
    {$ifdef FPCMM_ASSUMEMULTITHREAD} + ' assumt'      {$endif}
    {$ifdef FPCMM_PAUSE}             + ' pause'       {$endif}
    {$ifdef FPCMM_SLEEPTSC}          + ' rdtsc'       {$endif}
    {$ifndef BSD}
      {$ifdef FPCMM_NOMREMAP}        + ' nomremap'    {$endif}
    {$endif BSD}
    {$ifdef FPCMM_SMALLNOTWITHMEDIUM}+ ' smallpool'
      {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM} + 's' {$endif} {$endif}
    {$ifdef FPCMM_TINYPERTHREAD}     + ' tinpt'       {$endif}
    {$ifdef FPCMM_MEDIUMPERTHREAD}   + ' medpt'       {$endif}
    {$ifdef FPCMM_MEDIUMTOLARGE}     + ' medlrg'      {$endif}
    {$ifdef FPCMM_ERMS}              + ' erms'        {$endif}
    {$ifdef FPCMM_DEBUG}             + ' debug'       {$endif}
    {$ifdef FPCMM_REPORTMEMORYLEAKS} + ' repmemleak'  {$endif};

{$endif FPCMM_STANDALONE}

{$endif FPCX64MM_AVAILABLE}



implementation

{
   High-level Allocation Strategy Description
  --------------------------------------------

  The allocator handles the following families of memory blocks:
  - TINY <= 128 B (<= 256 B for FPCMM_BOOST)
    Per-Thread or Round-robin distribution into 8-128 arenas, fed from one or
    several pool(s) (fair scaling from with no threadvar nor GC involved)
  - SMALL <= 2600 B
    One arena per block size, fed from one or several pool(s)
  - MEDIUM <= 256 KB
    Separated pool(s) of bitmap-marked chunks, fed from 1.25MB of OS chunks
  - LARGE  > 256 KB
    Directly fed from OS mmap/virtualalloc with mremap when growing

  The original FastMM4 was enhanced as such, especially in FPCMM_SERVER mode:
  - FPC compatibility, even on POSIX/Linux, also for FPC specific API behavior;
  - Memory leaks and thread contention tracked without performance impact;
  - Detailed per-block statistics with little performance penalty;
  - x86_64 code was refactored and tuned in respect to 2020's hardware;
  - Inlined SSE2 movaps loop or ERMS are more efficient that subfunction(s);
  - New thread-friendly arenas of tiny (8 or 128) or medium blocks (4);
  - Tiny/medium arenas are assigned by locked round-robin or by thread ID;
  - Tiny and small blocks can fed from their own pool(s), not the medium pool;
  - Lock-less free lists to reduce tiny/small/medium FreeMem thread contention;
  - Large blocks logic has been rewritten, especially realloc;
  - OsAllocLarge() can use MAP_POPULATE to reduce page faults;
  - On Linux, mremap is used for efficient realloc of large blocks;
  - Largest blocks can grow by 2MB=PMD_SIZE chunks for even faster mremap.

  About locking:
  - Tiny and Small blocks have their own per-size lock;
  - Tiny and Small blocks have per-pool lock when feeding;
  - Lock-less free lists reduce tiny/small GetMem/FreeMem thread contention;
  - Lock-less free lists reduce medium FreeMem thread contention;
  - Medium arenas and Large blocks have one giant lock over their own pool;
  - Medium arenas have an unlocked prefetched memory chunk to reduce contention;
  - Large blocks don't lock during mmap/virtualalloc system calls;
  - SwitchToThread/nanosleep OS call is done after initial spinning;
  - FPCMM_DEBUG helps identifying the lock contention(s) in WriteHeapStatus.

}

{$ifdef FPCX64MM_AVAILABLE}
// this unit is available only for FPC + X86_64 CPU

{$ifndef FPCMM_NOPAUSE}
  // on contention problem, execute "pause" opcode and spin retrying the lock
  // - defined by default to follow Intel recommendatations from
  // https://software.intel.com/content/www/us/en/develop/articles/benefitting-power-and-performance-sleep-loops.html
  // - spinning loop is either using constants or rdtsc (if FPCMM_SLEEPTSC is set)
  // - on SkylakeX (Intel 7th gen), "pause" opcode went from 10-20 to 140 cycles
  // so our constants below will favor those latest CPUs with a longer pause
  {$define FPCMM_PAUSE}
{$endif FPCMM_NOPAUSE}

{$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
  {$define FPCMM_SMALLNOTWITHMEDIUM}
{$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}


{ ********* Operating System Specific API Calls }

{$ifdef MSWINDOWS}

// Win64: any assembler function with sub-calls should have a stack frame
// -> nostackframe is defined only on Linux or for functions with no nested call
{$undef NOSFRAME}

const
  kernel32 = 'kernel32.dll';

  MEM_COMMIT    = $1000;
  MEM_RESERVE   = $2000;
  MEM_RELEASE   = $8000;
  MEM_FREE      = $10000;
  MEM_TOP_DOWN  = $100000;
  MEM_64K_PAGES = $20400000; // VirtualAlloc2() hint parameter

  PAGE_READWRITE = 4;
  PAGE_GUARD = $0100;
  PAGE_VALID = $00e6; // PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or
      // PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY

type
  // VirtualQuery() API result structure
  TMemInfo = record
    BaseAddress, AllocationBase: PtrUInt;
    AllocationProtect: cardinal;
    PartitionId: word;
    RegionSize: PtrUInt;
    State, Protect, MemType: cardinal;
  end;

function VirtualAlloc(lpAddress: pointer;
   dwSize: PtrUInt; flAllocationType, flProtect: cardinal): pointer;
  stdcall; external kernel32 name 'VirtualAlloc';

function VirtualFree(lpAddress: pointer; dwSize: PtrUInt;
   dwFreeType: cardinal): LongBool;
  stdcall; external kernel32 name 'VirtualFree';

function VirtualQuery(lpAddress, lpMemInfo: pointer; dwLength: PtrUInt): PtrUInt;
  stdcall; external kernel32 name 'VirtualQuery';

procedure SwitchToThread;
  stdcall; external kernel32 name 'SwitchToThread';

function GetCurrentProcess: THandle;
  stdcall; external kernel32 name 'GetCurrentProcess';

function GetModuleHandleW(lpModuleName: PWideChar): THandle;
  stdcall; external kernel32 name 'GetModuleHandleW';

function GetProcAddress(Lib: THandle; ProcName: PAnsiChar): pointer;
  stdcall; external kernel32 name 'GetProcAddress'; // Ansi-only API

var
  VirtualAlloc2: function(Process: THandle; BaseAddress: pointer; Size: PtrUInt;
    AllocationType, PageProtection: cardinal;
    ExtendedParameters: pointer; ParameterCount: cardinal): pointer; stdcall;

function OsAllocLarge(Size: PtrInt; AllocType: cardinal = MEM_COMMIT;
  BaseAddress: pointer = nil): pointer;
begin
  if ((Size and LargeBlockGranularityAnd) = 0) and // paranoid: always 64K aligned
     Assigned(VirtualAlloc2) then
  begin
    result := VirtualAlloc2(GetCurrentProcess, BaseAddress, Size,
      AllocType or MEM_64K_PAGES, PAGE_READWRITE, nil, 0); // reduce page-faults
    if result <> nil then
      exit;
    if Size = MediumBlockPoolSizeMem then // this should always be accepted
      VirtualAlloc2 := nil; // happens e.g. when running on Wine
  end;
  result := VirtualAlloc(BaseAddress, Size, AllocType, PAGE_READWRITE);
  // FastMM4 uses top-down allocation (MEM_TOP_DOWN) of large blocks to "reduce
  // fragmentation", but on a 64-bit system I am not sure of this statement, and
  // VirtualAlloc() was reported to have a huge slowdown due to this option
  // https://randomascii.wordpress.com/2011/08/05/making-virtualalloc-arbitrarily-slower
end;

{$ifdef FPCMM_MEDIUMPERTHREAD}
function OsAllocMedium(Size: PtrInt): pointer;
var
  raw: pointer;
begin
  raw := VirtualAlloc(nil, Size + MediumBlockAlignment, MEM_RESERVE, PAGE_READWRITE);
  if raw = nil then
    exit(nil);
  result := pointer((PtrUInt(raw) + MediumBlockAlignmentMask) and
                    not MediumBlockAlignmentMask);
  if VirtualAlloc(result, Size, MEM_COMMIT, PAGE_READWRITE) <> nil then
    exit;
  VirtualFree(raw, 0, MEM_RELEASE);
  result := nil;
end;
{$else}
function OsAllocMedium(Size: PtrInt): pointer; inline;
begin
  result := OsAllocLarge(Size);
end;
{$endif FPCMM_MEDIUMPERTHREAD}

procedure OsFreeMedium(ptr: pointer; Size: PtrInt); inline;
{$ifdef FPCMM_MEDIUMPERTHREAD}
var
  nfo: TMemInfo;
{$endif FPCMM_MEDIUMPERTHREAD}
begin
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  FillChar(nfo, SizeOf(nfo), 0);
  if VirtualQuery(ptr, @nfo, SizeOf(nfo)) = SizeOf(nfo) then
    VirtualFree(pointer(nfo.AllocationBase), 0, MEM_RELEASE);
  {$else}
  VirtualFree(ptr, 0, MEM_RELEASE);
  {$endif FPCMM_MEDIUMPERTHREAD}
end;

procedure OsFreeLarge(ptr: pointer; Size: PtrInt); forward;
// implemented below with knowledge of PLargeBlockHeader/LargeBlockIsSegmented

{$ifndef FPCMM_NOMREMAP}

function OsRemapLarge(addr: pointer; old_len: size_t; var new_len: size_t): pointer;
var
  nfo: TMemInfo;
  next: pointer;
  nextsize, tomove: size_t;
const
  LargeBlockIsSegmented = 8; // forward definition
begin
  // old_len and new_len have 64KB granularity, so match Windows page size
  nextsize := new_len - old_len;
  if PtrInt(nextsize) > 0 then
  begin
    // try to allocate the memory just after the existing one
    FillChar(nfo, SizeOf(nfo), 0);
    next := addr + old_len;
    if (VirtualQuery(next, @nfo, SizeOf(nfo)) = SizeOf(nfo)) and
       (nfo.State = MEM_FREE) and
       (nfo.BaseAddress <= PtrUInt(next)) and // enough space?
       (nfo.BaseAddress + nfo.RegionSize >= PtrUInt(next) + nextsize) and
       // set the address space in two reserve + commit steps for thread safety
       (OsAllocLarge(nextsize, MEM_RESERVE, next) <> nil) and
       (OsAllocLarge(nextsize, MEM_COMMIT, next) <> nil) then
      begin
        new_len := new_len or LargeBlockIsSegmented; // several VirtualFree()
        result := addr; // in-place realloc: no need to move memory :)
        exit;
      end;
  end;
  // we need to use the slower but safe Alloc/Move/Free pattern
  result := OsAllocLarge(new_len);
  tomove := new_len;
  if tomove > old_len then // handle size up or down
    tomove := old_len;
  Move(addr^, result^, tomove); // RTL non-volatile asm or our AVX MoveFast()
  OsFreeLarge(addr, old_len);
end;

{$endif FPCMM_NOMREMAP}

// aligning large chunks > 4MB to 2MB units seems always a good idea
{$define FPCMM_LARGEBIGALIGN}

// experimental VirtualQuery detection of object class - use at your own risk
{$define FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}

{$else}

uses
  {$ifndef DARWIN}
  syscall,
  {$endif DARWIN}
  BaseUnix;

// in practice, SYSV ABI seems to not require a stack frame, as Win64 does, for
// our use case of nested calls with no local stack storage and direct kernel
// syscalls - but since it is clearly undocumented, we set it on LINUX only
// -> appears to work with no problem from our tests: feedback is welcome!
// -> see FPCMM_NOSFRAME conditional to disable it on LINUX
{$ifdef LINUX}
  {$define NOSFRAME}
{$else}
  {$define OLDLINUXKERNEL}      // no Linuxism on BSD
  {$undef FPCMM_TINYPERTHREAD}  // no inlined pthread_self on BSD
  {$undef FPCMM_MEDIUMPERTHREAD}
{$endif LINUX}

// on Linux, mremap() on PMD_SIZE=2MB aligned data can make a huge speedup
// - Transparent Huge Pages (THP) exists since Kernel 2.6.38
// - HAVE_MOVE_PMD enabled on arm64 since 2020 - https://lwn.net/Articles/833208
// - FreeBSD has a similar behavior with its Superpages - feedback is needed
{$ifdef LINUX}
  {$define FPCMM_LARGEBIGALIGN} // align large chunks to 21-bit = 2MB = PMD_SIZE
{$endif LINUX}

// we directly call the OS Kernel, so this unit doesn't require any libc

const
  {$ifdef OLDLINUXKERNEL}
    {$undef FPCMM_MEDIUM32BIT}
    MAP_POPULATE = 0;
  {$else}
    /// put the mapping in first 2 GB of memory (31-bit addresses) - 2.4.20, 2.6
    MAP_32BIT = $40;
    /// populate (prefault) pagetables to avoid page faults later - 2.5.46
    MAP_POPULATE = $08000;
  {$endif OLDLINUXKERNEL}

  /// tiny/small/medium blocks mmap() flags
  // - MAP_POPULATE is not included because small and medium blocks are sparsely
  // accessed, and OsAllocMedium() is done within the global medium lock
  // - FPCMM_MEDIUM32BIT allocates only 31-bit pointers, but may be incompatible
  // e.g. with TOrmTable for data >256KB so would require the NOPOINTEROFFSET
  // conditional - therefore is not set by default
  MAP_MEDIUM = MAP_PRIVATE or MAP_ANONYMOUS
     {$ifdef FPCMM_MEDIUM32BIT} or MAP_32BIT {$endif};

  /// large blocks mmap() flags
  // - no MAP_32BIT since could use the whole 64-bit address space
  // - MAP_POPULATE is not included by default, even if mmap/mremap are called
  // outside the large blocks lock: in practice, it may lead to unnecessary
  // memory usage and increased initial mapping time - set FPCMM_LARGEPOPULATE
  MAP_LARGE = MAP_PRIVATE or MAP_ANONYMOUS
     {$ifdef FPCMM_LARGEPOPULATE} or MAP_POPULATE {$endif};

{$ifdef FPCMM_MEDIUM32BIT}
var
  AllocMediumflags: integer = MAP_MEDIUM;
{$else}
  AllocMediumflags = MAP_MEDIUM;
{$endif FPCMM_MEDIUM32BIT}

function OsAllocMediumRaw(Size: PtrInt): pointer;
  {$ifndef FPCMM_MEDIUM32BIT} inline; {$endif}
begin
  result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, AllocMediumflags, -1, 0);
  if result = MAP_FAILED then
    result := nil; // as VirtualAlloc()
  {$ifdef FPCMM_MEDIUM32BIT}
  if (result <> nil) or
     ((AllocMediumflags and MAP_32BIT) = 0) then
    exit;
  // try with no 2GB limit from now on
  AllocMediumflags := AllocMediumflags and not MAP_32BIT;
  result := OsAllocMediumRaw(Size);
  {$endif FPCMM_MEDIUM32BIT}
end;

{$ifdef FPCMM_MEDIUMPERTHREAD}
function OsAllocMedium(Size: PtrInt): pointer;
var
  raw: pointer;
  allocsize, prefix, suffix: PtrInt;
begin
  allocsize := Size + MediumBlockAlignment;
  raw := OsAllocMediumRaw(allocsize);
  if raw = nil then
  begin
    result := nil;
    exit;
  end;
  result := pointer((PtrUInt(raw) + MediumBlockAlignmentMask) and
    not MediumBlockAlignmentMask);
  prefix := PtrUInt(result) - PtrUInt(raw);
  if prefix <> 0 then
    fpmunmap(raw, prefix);
  suffix := allocsize - prefix - Size;
  if suffix <> 0 then
    fpmunmap(PByte(result) + Size, suffix);
end;
{$else}
function OsAllocMedium(Size: PtrInt): pointer; inline;
begin
  result := OsAllocMediumRaw(Size);
end;
{$endif FPCMM_MEDIUMPERTHREAD}

function OsAllocLarge(Size: PtrInt): pointer; inline;
begin
  result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_LARGE, -1, 0);
  if result = MAP_FAILED then
    result := nil; // as VirtualAlloc()
end;

procedure OsFreeMedium(ptr: pointer; Size: PtrInt); inline;
begin
  fpmunmap(ptr, Size);
end;

procedure OsFreeLarge(ptr: pointer; Size: PtrInt); inline;
begin
  fpmunmap(ptr, Size);
end;

{$ifdef LINUX}

{$ifndef FPCMM_NOMREMAP}

const
  syscall_nr_mremap = 25; // valid on x86_64 Linux and Android
  MREMAP_MAYMOVE = 1;

function OsRemapLarge(addr: pointer; old_len, new_len: size_t): pointer;
begin
  // let the Linux Kernel mremap() the memory using its TLB magic
  result := pointer(do_syscall(syscall_nr_mremap, TSysParam(addr),
    TSysParam(old_len), TSysParam(new_len), TSysParam(MREMAP_MAYMOVE)));
  if result <> MAP_FAILED then
    exit;
  // some OS (e.g. Alma Linux 9 with 5.x kernel) seems to fail sometimes :(
  // https://github.com/ClickHouse/ClickHouse/issues/52955#issuecomment-1664710083
  // -> it should not, because we use the MREMAP_MAYMOVE flag - but anyway...
  // -> fallback to safe, simple (and slower) Alloc/Move/Free pattern
  result := OsAllocLarge(new_len);
  if result = nil then
    exit; // out of memory
  if new_len > old_len then
    new_len := old_len; // resize down
  Move(addr^, result^, new_len); // RTL non-volatile asm or our AVX MoveFast()
  OsFreeLarge(addr, old_len);
end;

{$endif FPCMM_NOMREMAP}

{$ifdef FPCMM_TINYPERTHREAD}
function pthread_self: PtrUInt; external; // ensure is linked
{$endif FPCMM_TINYPERTHREAD}

// experimental detection of object class - use at your own risk
{$define FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
// (untested on BSD/DARWIN)

{$else} // BSD branch

{$define FPCMM_NOMREMAP} // mremap is a Linux-specific syscall
{$undef FPCMM_OSYIELD}   // no yield syscall defined

{$endif LINUX}

{$ifdef FPCMM_OSYIELD}
procedure SwitchToThread;
begin
  // trigger more syscalls than nanosleep, with no actual benefit
  Do_SysCall(syscall_nr_sched_yield); // properly defined in syscall.pp
end;
{$else}
procedure SwitchToThread;
var
  t: TTimeSpec;
begin
  // note: nanosleep() may flood the kernel with timer/scheduling events under
  // repeated calls - but here we spin-and-pause between calls
  t.tv_sec := 0;
  t.tv_nsec := 1000; // 1us seems fair enough in respect to OS timers resolution
  fpnanosleep(@t, nil);
end;
{$endif FPCMM_OSYIELD}

{$endif MSWINDOWS}

// fallback to safe and simple Alloc/Move/Free pattern
{$ifdef FPCMM_NOMREMAP}

function OsRemapLarge(addr: pointer; old_len, new_len: size_t): pointer;
begin
  result := OsAllocLarge(new_len);
  if new_len > old_len then
    new_len := old_len; // resize down
  Move(addr^, result^, new_len); // RTL non-volatile asm or our AVX MoveFast()
  OsFreeLarge(addr, old_len);
end;

{$undef FPCMM_LARGEBIGALIGN}  // keep 64KB granularity if no mremap()

{$endif FPCMM_NOMREMAP}


{ ********* Some Assembly Helpers }

// low-level conditional to disable nostackframe code on Linux
{$ifdef FPCMM_NOSFRAME}
  {$undef NOSFRAME}
{$endif FPCMM_NOSFRAME}

var
  HeapStatus: TMMStatus;

procedure ReleaseCoreSafe;
var
  _c, _s, _d, _8, _9, _10, _11: pointer;
asm
        mov     _c,  rcx  // always preserve volatile registers
        mov     _s,  rsi
        mov     _d,  rdi
        mov     _8,  r8
        mov     _9,  r9
        mov     _10, r10
        mov     _11, r11
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc // returns the TSC in EDX:EAX
        shl     rdx, 32
        or      rax, rdx
        push    rax
        call    SwitchToThread
        pop     rcx
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        lea     rdx, [rip + HeapStatus]
        sub     rax, rcx
   lock add     qword ptr [rdx + TMMStatus.SleepCycles], rax
        {$else}
        call    SwitchToThread
        lea     rdx, [rip + HeapStatus]
        {$endif FPCMM_SLEEPTSC}
   lock inc     qword ptr [rdx + TMMStatus.SleepCount]
        mov     rcx, _c
        mov     rsi, _s
        mov     rdi, _d
        mov     r8,  _8
        mov     r9,  _9
        mov     r10, _10
        mov     r11, _11
end;

procedure NotifyArenaAlloc(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
        {$ifdef FPCMM_DEBUG}
   lock add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
   lock add     qword ptr [Arena].TMMStatusArena.CumulativeBytes, Size
   lock inc     qword ptr [Arena].TMMStatusArena.CumulativeAlloc
        mov     rax, qword ptr [Arena].TMMStatusArena.CurrentBytes
        cmp     rax, qword ptr [Arena].TMMStatusArena.PeakBytes
        jbe     @s
        mov     qword ptr [Arena].TMMStatusArena.PeakBytes, rax
@s:     {$else}
        {$ifdef FPCMM_MEDIUMPERTHREAD} lock {$endif}
        add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
        {$ifdef FPCMM_MEDIUMPERTHREAD} lock {$endif}
        add     qword ptr [Arena].TMMStatusArena.CumulativeBytes, Size
       {$endif FPCMM_DEBUG}
end;

procedure NotifyMediumLargeFree(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
        neg     Size
        {$ifdef FPCMM_DEBUG}
   lock add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
   lock inc     qword ptr [Arena].TMMStatusArena.CumulativeFree
        {$else}
        {$ifdef FPCMM_MEDIUMPERTHREAD} lock {$endif}
        add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
        {$endif FPCMM_DEBUG}
end;


{ ********* Constants and Data Structures Definitions }

// during spinning, there is clearly thread contention: in this case, plain
// "cmp" before "lock cmpxchg" seems preferable, especially on older CPUs
{$define FPCMM_CMPBEFORELOCK_SPIN}

// prepare a Medium arena chunk in TMediumInfo.Prefetch outside of the lock
{$define FPCMM_MEDIUMPREFETCH}

const
  // define maximum size of tiny blocks, and the number of arenas
  {$ifdef FPCMM_BOOSTER}
  NumTinyBlockTypesPO2  = 4; // tiny are <= 256 bytes
  NumTinyBlockArenasPO2 = 7; // 128 arenas
  {$else}
    {$ifdef FPCMM_BOOST}
    NumTinyBlockTypesPO2  = 4; // tiny are <= 256 bytes
    NumTinyBlockArenasPO2 = 3; // 8 arenas
    {$else}
    // default FPCMM_SERVER settings
    NumTinyBlockTypesPO2  = 3; // multiple arenas for tiny blocks <= 128 bytes
    NumTinyBlockArenasPO2 = 3; // 8 round-robin arenas (including Small[])
    {$endif FPCMM_BOOST}
  {$endif FPCMM_BOOSTER}

  NumSmallBlockTypes       = 46;
  MaximumSmallBlockSize    = 2608;
  NumTinyBlockTypes        =
     1 shl NumTinyBlockTypesPO2; // 8 (128B) or 16 (256B)
  NumTinyBlockArenas       =
     (1 shl NumTinyBlockArenasPO2) - 1; // -1 = main Small[]
  NumSmallInfoBlock        =
    NumSmallBlockTypes + NumTinyBlockArenas * NumTinyBlockTypes;
  SmallBlockSizes: array[0..NumSmallBlockTypes - 1] of word = (
    16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256,
    272, 288, 304, 320, 352, 384, 416, 448, 480, 528, 576, 624, 672, 736, 800,
    880, 960, 1056, 1152, 1264, 1376, 1504, 1648, 1808, 1984, 2176, 2384,
    MaximumSmallBlockSize, MaximumSmallBlockSize, MaximumSmallBlockSize);

  SmallBlockGranularity         = 16;
  MaximumTinyBlockSize          = NumTinyBlockTypes * SmallBlockGranularity;
  NumSmallBlockGranularitySlots =
    (MaximumSmallBlockSize + SmallBlockGranularity - 1) div SmallBlockGranularity;
  TargetSmallBlocksPerPool      = 48;
  MinimumSmallBlocksPerPool     = 12;
  SmallBlockDownsizeCheckAdder  = 64;
  SmallBlockUpsizeAdder         = 32;
  SmallBlockTypePO2             = 6;  // SizeOf(TSmallBlockType)=64
  SmallBlockRetention           = 8;

  MediumBlockPoolSize         = MediumBlockPoolSizeMem - 16;
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  {$if MediumBlockPoolSizeMem > MediumBlockAlignment}
    {$error MediumBlockAlignment must cover a complete medium pool}
  {$ifend}
  NumMediumBlockArenasPO2     = 2;
  NumMediumBlockArenas        = 1 shl NumMediumBlockArenasPO2;
  {$endif FPCMM_MEDIUMPERTHREAD}
  {$ifdef FPCMM_MEDIUMTOLARGE}
  MediumBlockToLargeMinSize   = 48 shl 10; // promoted to Large if > 48KB
  {$endif FPCMM_MEDIUMTOLARGE}
  MediumBlockSizeOffset       = 48;
  MinimumMediumBlockSize      = 11 * 256 + MediumBlockSizeOffset;
  MediumBlockBinsPerGroup     = 32;
  MediumBlockBinGroupCount    = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  MediumBlockGranularity      = 256;
  MaximumMediumBlockSize      =
    MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  OptimalSmallBlockPoolSizeLowerLimit =
    29 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit =
    64 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  MaximumSmallBlockPoolSize   =
    OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  MediumInPlaceDownsizeLimit  = MinimumMediumBlockSize div 4;

  {$ifdef FPCMM_SLEEPTSC}
  // pause using rdtsc (30 cycles latency on hardware but emulated on VM)
  SpinMediumLockTSC          = 10000;
  SpinLargeLockTSC           = 10000;
  {$ifdef FPCMM_PAUSE}
  SpinSmallGetmemLockTSC     = 1000;
  {$endif FPCMM_PAUSE}
  {$else}
  // pause with constant spinning counts (empirical values)
  SpinMediumLockCount        = pred(6 shl 5); // with exponential pause backoff
  SpinLargeLockCount         = 1000;          // linear backoff is enough here
  {$ifdef FPCMM_PAUSE}
  SpinSmallGetmemLockCount   = 500;
  {$endif FPCMM_PAUSE}
  SpinMediumFreememLockCount = 500;
  {$endif FPCMM_SLEEPTSC}

  {$ifdef FPCMM_ERMS}
  // pre-ERMS expects at least 256 bytes, IvyBridge+ with ERMS is good from 64
  // (copy_user_enhanced_fast_string() in recent Linux kernel uses 64)
  // see https://stackoverflow.com/a/43837564/458259 for explanations and timing
  // -> "movaps" loop is used up to 256 bytes of data: good on all CPUs
  // -> "movnt" Move/MoveFast is used for large blocks: always faster than ERMS
  ErmsMinSize = 256;
  {$endif FPCMM_ERMS}

  // some binary-level constants for internal flags
  IsFreeBlockFlag                = 1;
  IsMediumBlockFlag              = 2;
  IsSmallBlockPoolInUseFlag      = 4;
  IsLargeBlockFlag               = 4;
  PreviousMediumBlockIsFreeFlag  = 8;
  LargeBlockIsSegmented          = 8; // see also OsRemapLarge() above
  DropSmallFlagsMask             = -8;
  ExtractSmallFlagsMask          = 7;
  DropMediumAndLargeFlagsMask    = -16;
  ExtractMediumAndLargeFlagsMask = 15;

type
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  // information for each small block size - 64 bytes long = CPU cache line
  TSmallBlockType = packed record
    Locked: boolean;
    AllowedGroupsForBlockPoolBitmap: byte;
    BlockSize: Word;
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    NextSequentialFeedBlockAddress: pointer;
    MaxSequentialFeedBlockAddress: pointer;
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
    GetmemCount: cardinal;
    FreememCount: cardinal;
    LastFreeLocked: boolean;
    EmptyPoolReuse: byte;
    Padding: array[1 .. 2] of byte;
    LastFreeCount: cardinal;
  end;
  PSmallBlockType = ^TSmallBlockType;

  TSmallBlockTypes = array[0..NumSmallBlockTypes - 1] of TSmallBlockType;
  TTinyBlockTypes  = array[0..NumTinyBlockTypes - 1]  of TSmallBlockType;
  TSmallBlockInfo = record
    Small: TSmallBlockTypes;
    Tiny: array[0..NumTinyBlockArenas - 1] of TTinyBlockTypes;
    GetmemLookup: array[0..NumSmallBlockGranularitySlots - 1] of byte;
    // safe access to IsMultiThread global variable - accessed via GOT sub-call
    IsMultiThreadPtr: PBoolean;
    {$ifndef FPCMM_TINYPERTHREAD}
    TinyCurrentArena: integer;
    {$endif FPCMM_TINYPERTHREAD}
    // assembly indexes by BlockSize div SmallBlockGranularity,
    // not by the irregular class ordinal
    GetmemSleepCount: array[0..NumSmallBlockGranularitySlots - 1] of cardinal;
    // some fiedls here because there was no room in TSmallBlockType
    {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM} // PMediumBlockInfo lookup
    SmallMediumBlockInfo: array[0..NumSmallInfoBlock - 1] of pointer;
    {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
    SmallLastFree: array[0 .. NumSmallInfoBlock - 1] of pointer;
  end;

  TSmallBlockPoolHeader = record
    BlockType: PSmallBlockType;
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    FirstFreeBlock: pointer;
    BlocksInUse: cardinal;
    SmallBlockPoolSignature: cardinal;
    FirstBlockPoolPointerAndFlags: PtrUInt;
  end;

  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = record
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    MediumPerThreadBase: pointer; // used by FPCMM_MEDIUMPERTHREAD
    FirstMediumBlockSizeAndFlags: PtrUInt;
  end;

  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  PMediumBlockInfo = ^TMediumBlockInfo;
  TMediumBlockInfo = packed record
    // first cache line contains the main context
    Locked: boolean;
    MainPad: array[1 .. 64 - (SizeOf(boolean) + SizeOf(TMediumBlockPoolHeader) +
      SizeOf(pointer) * 2 + SizeOf(cardinal) * 2)] of byte;
    PoolsCircularList: TMediumBlockPoolHeader;
    LastSequentiallyFed: pointer;
    IsMultiThreadPtr: PBoolean; // safe access to IsMultiThread global variable
    SequentialFeedBytesLeft: cardinal;
    BinGroupBitmap: cardinal;
    // second cache line holds the LastFree linked list
    LastFreeLocked: boolean;
    LastFreePad: array[1 .. 64 - (SizeOf(boolean) + SizeOf(pointer))] of byte;
    LastFree: pointer;
    {$ifdef FPCMM_MEDIUMPREFETCH}
    // optional cache line to contain the medium prefetch information
    PrefetchLocked: boolean;
    PrefetchPad: array[1 .. 64 - (SizeOf(boolean) + SizeOf(pointer))] of byte;
    Prefetch: pointer;
    {$endif FPCMM_MEDIUMPREFETCH}
    // then all medium bins follow
    BinBitmaps: array[0 .. MediumBlockBinGroupCount - 1] of cardinal;
    Bins: array[0 .. MediumBlockBinCount - 1] of TMediumFreeBlock;
  end;

  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    Reserved: PtrUInt;
    BlockSizeAndFlags: PtrUInt;
  end;

const
  BlockHeaderSize            = SizeOf(pointer);
  SmallBlockPoolHeaderSize   = SizeOf(TSmallBlockPoolHeader);
  SmallBlockTypeSize         = SizeOf(TSmallBlockType);
  MediumBlockPoolHeaderSize  = SizeOf(TMediumBlockPoolHeader);
  LargeBlockHeaderSize       = SizeOf(TLargeBlockHeader);
  {$ifdef FPCMM_LARGEBIGALIGN}
  LargeBlockGranularity2And  = (1 shl 21) - 1; // PMD_SIZE=2MB granularity
  LargeBlockGranularity2Size = 2 shl 21;  // for size >= 4MB
  // on Linux, mremap() on PMD_SIZE=2MB aligned data can make a huge speedup
  {$endif FPCMM_LARGEBIGALIGN}

// all T*BlockInfo variables are local to this unit, so are FPC_PIC compatible

{$CODEALIGN VARMIN=64} // align all those var to 64 bytes = CPU cache line size
var
  SmallBlockInfo: TSmallBlockInfo;
  MediumBlockInfo: TMediumBlockInfo;
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  MediumBlockInfoExtra:  array[1..NumMediumBlockArenas - 1] of TMediumBlockInfo;
  MediumBlockInfoLookup: array[0..NumMediumBlockArenas - 1] of PMediumBlockInfo;
  {$endif FPCMM_MEDIUMPERTHREAD}
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
  {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
  SmallMediumBlockInfo: array[0.. (NumTinyBlockTypes * 2) - 2] of TMediumBlockInfo;
  // -2 to ensure same small block size won't share the same medium block
  // note: including NumTinyBlockArenasPO2 to the calculation has no benefit
  {$else}
  SmallMediumBlockInfo: array[0..0] of TMediumBlockInfo;
  {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
  {$else}
  SmallMediumBlockInfo: TMediumBlockInfo absolute MediumBlockInfo;
  {$endif FPCMM_SMALLNOTWITHMEDIUM}
  LargeBlocksLocked: boolean;
  LargeBlocksCircularList: TLargeBlockHeader;


{ ********* Shared Routines }

// small/tiny blocks maintain a separated locked free list on lock contention

procedure GetSmallLastFreeBlock; nostackframe; assembler;
asm
        // input: rsi = TSmallBlockType
        mov     rax, rsi
        lea     r10, [rip + SmallBlockInfo]
        sub     rax, r10
        shr     eax, SmallBlockTypePO2 - 3 // 1 shl 3 = SizeOf(SmallLastFree[])
        lea     r10, [r10 + rax].TSmallBlockInfo.SmallLastFree
        // r10 = @SmallLastFree[] of this rsi = TSmallBlockType
        mov     eax, $100
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.LastFreeLocked, ah
        je      @ok
        xor     rax, rax // just try once to acquire the lock
        ret
@ok:    mov     rax, [r10]
        test    rax, rax
        jz      @done
        mov     r11, [rax] // very simple (and quick) linked list pattern
        mov     [r10], r11
        dec     dword ptr [rsi].TSmallBlockType.LastFreeCount
        test    rax, rax   // dec above did change the z flag
@done:  mov     byte ptr [rsi].TSmallBlockType.LastFreeLocked, false
        // nz = rax=to be freed or z = nothing found - modifies r10+r11
end;

{$ifdef FPCMM_MEDIUMPREFETCH}
procedure PrefetchMediumBlock(dummy: cardinal);
{$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// on input/output: r10=TMediumBlockInfo
asm
        mov     rcx, r10
        xor     edx, edx
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [rcx].TMediumBlockInfo.PrefetchLocked, dl
        jnz     @done
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
        mov     eax, $100
        lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.PrefetchLocked, ah
        jne     @done
        cmp     qword ptr [rcx].TMediumBlockInfo.Prefetch, rdx
        jnz     @none
        push    rsi
        push    rdi
        push    r10
        push    r11
        mov     dummy, MediumBlockPoolSizeMem
        call    OsAllocMedium // mmap() is usually very fast
        pop     r11
        pop     r10
        pop     rdi
        pop     rsi
        mov     qword ptr [r10].TMediumBlockInfo.Prefetch, rax
@none:  mov     byte ptr [r10].TMediumBlockInfo.PrefetchLocked, false
@done:
end;
{$endif FPCMM_MEDIUMPREFETCH}

procedure SpinLockMediumBlocks;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// on input/output: r10=TMediumBlockInfo; on output: ZF=1 if Locked
asm
        // spin and acquire the medium arena lock
        {$ifdef FPCMM_SLEEPTSC}
@s:     rdtsc   // tsc in edx:eax
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinMediumLockTSC] // r9 = endtsc
@sp:    pause
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @none // timeout
        {$else}
        // same algorithm than function SpinAndWait() in mormot.core.os.pas
@s:     mov     edx, SpinMediumLockCount // = pred(6 shl 5)
@sp:    mov     ecx, SpinMediumLockCount
        sub     ecx, edx
        dec     edx
        jz      @none   // timeout
        shr     ecx, 5  // 0..6 range, each 32 times
        jz      @try
        dec     ecx
        mov     eax, 1
        shl     eax, cl // exponential backoff: 1,2,4,8,16 x pause
@p:     pause           // "rep nop" called 992 times until yield to the OS
        dec     eax
        jnz     @p
        {$endif FPCMM_SLEEPTSC}
@try:   mov     rcx, r10
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [r10].TMediumBlockInfo.Locked, true
        je      @sp
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        jne     @sp
        {$ifdef NOSFRAME} // return with ZF=1 if locked
        ret
        {$else}
        jmp     @done
        {$endif NOSFRAME}
@none:  test    r10, r10 // return with ZF=0 if not locked
@done:
end;

procedure LockMediumBlocks;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// on input/output: r10=TMediumBlockInfo
asm
        {$ifdef FPCMM_MEDIUMPREFETCH}
        // since we are waiting for the lock, prefetch one medium memory chunk
        cmp     qword ptr [r10].TMediumBlockInfo.Prefetch, 0
        jnz     @spin // there is already a prefetched memory chunk available
        call    PrefetchMediumBlock
        {$endif FPCMM_MEDIUMPREFETCH}
        // spin and acquire the medium arena lock
@spin:  call    SpinLockMediumBlocks
        jz      @ok
        call    ReleaseCoreSafe // Windows SwitchToThread or POSIX nanosleep(1us)
        lea     rax, [rip + HeapStatus]
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     qword ptr [rax].TMMStatus.Medium.SleepCount
        jmp     @spin
@ok:
end;

procedure InsertMediumBlockIntoBin; nostackframe; assembler;
// rcx=P edx=blocksize r10=TMediumBlockInfo - even on POSIX
asm
        mov     rax, rcx
        // Get the bin number for this block size
        sub     edx, MinimumMediumBlockSize
        shr     edx, 8
        // Validate the bin number
        sub     edx, MediumBlockBinCount - 1
        sbb     ecx, ecx
        and     edx, ecx
        add     edx, MediumBlockBinCount - 1
        mov     r9, rdx
        // Get the bin address in rcx
        shl     edx, 4
        lea     rcx, [r10 + rdx + TMediumBlockInfo.Bins]
        // Bins are LIFO, se we insert this block as the first free block in the bin
        mov     rdx, TMediumFreeBlock[rcx].NextFreeBlock
        mov     TMediumFreeBlock[rax].PreviousFreeBlock, rcx
        mov     TMediumFreeBlock[rax].NextFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].PreviousFreeBlock, rax
        mov     TMediumFreeBlock[rcx].NextFreeBlock, rax
        // Was this bin empty?
        cmp     rdx, rcx
        jne     @Done
        // Get ecx=bin number, edx=group number
        mov     rcx, r9
        mov     rdx, r9
        shr     edx, 5
        // Flag this bin as not empty
        mov     eax, 1
        shl     eax, cl
        or      dword ptr [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        // Flag the group as not empty
        mov     eax, 1
        mov     ecx, edx
        shl     eax, cl
        or      [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure RemoveMediumFreeBlock; nostackframe; assembler;
asm
        // rcx=MediumFreeBlock r10=TMediumBlockInfo - even on POSIX
        // Get the current previous and next blocks
        mov     rdx, TMediumFreeBlock[rcx].PreviousFreeBlock
        mov     rcx, TMediumFreeBlock[rcx].NextFreeBlock
        // Remove this block from the linked list
        mov     TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].NextFreeBlock, rcx
        // Is this bin now empty? If the previous and next free block pointers are
        // equal, they must point to the bin
        cmp     rcx, rdx
        jne     @Done
        // Get ecx=bin number, edx=group number
        lea     r8, [r10 + TMediumBlockInfo.Bins]
        sub     rcx, r8
        mov     edx, ecx
        shr     ecx, 4
        shr     edx, 9
        // Flag this bin as empty
        mov     eax, -2
        rol     eax, cl
        and     dword ptr [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        jnz     @Done
        // Flag this group as empty
        mov     eax, -2
        mov     ecx, edx
        rol     eax, cl
        and     [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure BinMediumSequentialFeedRemainder(
  var Info: TMediumBlockInfo); nostackframe; assembler;
asm
        mov     r10, Info
        mov     eax, [Info + TMediumBlockInfo.SequentialFeedBytesLeft]
        test    eax, eax
        jz      @Done
        // Is the last fed sequentially block free?
        mov     rax, [Info + TMediumBlockInfo.LastSequentiallyFed]
        test    byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
        jnz     @LastBlockFedIsFree
        // Set the "previous block is free" flag in the last block fed
        or      qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
        // Get edx=remainder size, rax=remainder start
        mov     edx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        sub     rax, rdx
@BinTheRemainder:
        // Store the size of the block as well as the flags
        lea     rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rax - BlockHeaderSize], rcx
        // Store the trailing size marker
        mov     [rax + rdx - 16], rdx
        // Bin this medium block
        cmp     edx, MinimumMediumBlockSize
        jb      @Done
        mov     rcx, rax
        jmp     InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
@Done:  ret
@LastBlockFedIsFree:
        // Drop the flags
        mov     rdx, DropMediumAndLargeFlagsMask
        and     rdx, [rax - BlockHeaderSize]
        // Free the last block fed
        cmp     edx, MinimumMediumBlockSize
        jb      @DontRemoveLastFed
        // Last fed block is free - remove it from its size bin
        mov     rcx, rax
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        // Re-read rax and rdx
        mov     rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        mov     rdx, DropMediumAndLargeFlagsMask
        and     rdx, [rax - BlockHeaderSize]
@DontRemoveLastFed:
        // Get the number of bytes left in ecx
        mov     ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        // rax = remainder start, rdx = remainder size
        sub    rax, rcx
        add    edx, ecx
        jmp    @BinTheRemainder
end;

procedure LockLargeBlocks;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
@s:     mov     eax, $100
        lea     rcx, [rip + LargeBlocksLocked]
  lock  cmpxchg byte ptr [rcx], ah
        je      @ok
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinLargeLockTSC] // r9 = endtsc
@sp:    pause
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @rc // timeout
        {$else}
        mov     edx, SpinLargeLockCount
@sp:    pause
        dec     edx
        jz      @rc // timeout
        {$endif FPCMM_SLEEPTSC}
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [rcx], true
        je      @sp
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
  lock  cmpxchg byte ptr [rcx], ah
        je      @ok
        jmp     @sp
@rc:    call    ReleaseCoreSafe
        lea     rax, [rip + HeapStatus]
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     qword ptr [rax].TMMStatus.Large.SleepCount
        jmp     @s
@ok:    // reset the stack frame before ret
end;

{$ifdef FPCMM_MEDIUMPREFETCH}

// munmap() takes more time than mmap() so it makes sense to cache one chunk
function TrySaveMediumPrefetch(var Info: TMediumBlockInfo;
  MediumBlock: PMediumBlockPoolHeader): pointer; nostackframe; assembler;
asm
        {$ifndef MSWINDOWS}
        mov     rcx, Info
        mov     rdx, MediumBlock
        {$endif MSWINDOWS}
        xor     eax, eax
        cmp     qword ptr [rcx].TMediumBlockInfo.Prefetch, rax
        jnz     @ko // there is already a prefetched memory chunk available
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.PrefetchLocked, ah
        jne     @ko
        cmp     qword ptr [rcx].TMediumBlockInfo.Prefetch, 0
        jnz     @ko2
        // store this Medium block for the next TryAllocMediumPrefetch()
        mov     [rcx].TMediumBlockInfo.Prefetch, rdx
        xor     edx, edx // return nil if was saved
@ko2:   mov     byte ptr [rcx].TMediumBlockInfo.PrefetchLocked, false
@ko:    mov     rax, rdx
end;

function TryAllocMediumPrefetch(var Info: TMediumBlockInfo): pointer;
  nostackframe; assembler;
asm
        {$ifndef MSWINDOWS}
        mov     rcx, Info
        {$endif MSWINDOWS}
        xor     eax, eax
        cmp     qword ptr [rcx].TMediumBlockInfo.Prefetch, rax
        jz      @ok        // is there a prefetched memory chunk available?
        xor     edx, edx
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.PrefetchLocked, ah
        jne     @busy
        // just get the memory chunk - no need to call mmap/VirtualAlloc
        mov     rax, [rcx].TMediumBlockInfo.Prefetch
        mov     [rcx].TMediumBlockInfo.Prefetch, rdx
        mov     [rcx].TMediumBlockInfo.PrefetchLocked, dl
        ret
@busy:  xor     eax, eax
@ok:
end;

{$endif FPCMM_MEDIUMPREFETCH}

procedure FreeMedium(ptr: PMediumBlockPoolHeader; var info: TMediumBlockInfo);
begin
  {$ifdef FPCMM_MEDIUMPREFETCH}
  ptr := TrySaveMediumPrefetch(info, ptr);
  if ptr <> nil then
  {$endif FPCMM_MEDIUMPREFETCH}
    OsFreeMedium(ptr, MediumBlockPoolSizeMem);
  NotifyMediumLargeFree(HeapStatus.Medium, MediumBlockPoolSizeMem);
end;

function AllocNewSequentialFeedMediumPool(BlockSize: cardinal;
  var Info: TMediumBlockInfo): pointer;
var
  old: PMediumBlockPoolHeader;
  new: pointer;
begin
  BinMediumSequentialFeedRemainder(Info);
  {$ifdef FPCMM_MEDIUMPREFETCH}
  new := TryAllocMediumPrefetch(Info);
  if new = nil then
  {$endif FPCMM_MEDIUMPREFETCH}
    new := OsAllocMedium(MediumBlockPoolSizeMem);
  if new <> nil then
  begin
    {$ifdef FPCMM_MEDIUMPERTHREAD}
    // written once before the pool becomes reachable from any shared list
    PMediumBlockPoolHeader(new).MediumPerThreadBase := @Info;
    {$endif FPCMM_MEDIUMPERTHREAD}
    old := Info.PoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(new).PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
    Info.PoolsCircularList.NextMediumBlockPoolHeader := new;
    PMediumBlockPoolHeader(new).NextMediumBlockPoolHeader := old;
    old.PreviousMediumBlockPoolHeader := new;
    PPtrUInt(PByte(new) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    Info.SequentialFeedBytesLeft :=
      (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - BlockSize;
    result := pointer(PByte(new) + MediumBlockPoolSize - BlockSize);
    Info.LastSequentiallyFed := result;
    PPtrUInt(PByte(result) - BlockHeaderSize)^ := BlockSize or IsMediumBlockFlag;
    NotifyArenaAlloc(HeapStatus.Medium, MediumBlockPoolSizeMem);
  end
  else
  begin
    Info.SequentialFeedBytesLeft := 0; // system is unstable for sure
    result := nil;
  end;
end;

{$ifdef MSWINDOWS} // implemented here with knowledge of PLargeBlockHeader
procedure OsFreeLarge(ptr: pointer; Size: PtrInt);
var
  nfo: TMemInfo;
begin
  if (PLargeBlockHeader(ptr)^.BlockSizeAndFlags and LargeBlockIsSegmented) = 0 then
    // there was a regular single VirtualAlloc() call
    VirtualFree(ptr, 0, MEM_RELEASE)
  else
    // OsRemapLarge() requires several VirtualFree() calls
    repeat
      FillChar(nfo, SizeOf(nfo), 0);
      if (VirtualQuery(ptr, @nfo, SizeOf(nfo)) <> SizeOf(nfo)) or
         not VirtualFree(ptr, 0, MEM_RELEASE) then
        exit;
      inc(PByte(ptr), nfo.RegionSize);
      dec(Size, PtrInt(nfo.RegionSize));
    until Size <= 0;
end;
{$endif MSWINDOWS}

function ComputeLargeBlockSize(size: PtrUInt): PtrUInt; inline;
begin
  inc(size, LargeBlockHeaderSize + BlockHeaderSize);
  // aligned_size := ((size + align - 1) AND (NOT (align - 1)))
  {$ifdef FPCMM_LARGEBIGALIGN}
  // on Linux, mremap() on PMD_SIZE=2MB aligned data make a huge speedup
  if size >= LargeBlockGranularity2Size then // trigger if size>=4MB
    result := (size + LargeBlockGranularity2And) and not LargeBlockGranularity2And
  else
  {$endif FPCMM_LARGEBIGALIGN}
    // use default 64KB granularity for large blocks up to 4MB
    result := (size + LargeBlockGranularityAnd) and not LargeBlockGranularityAnd;
end;

function AllocateLargeBlockFrom(existing: pointer;
  oldblocksize, newblocksize: PtrUInt): pointer;
var
  new, old: PLargeBlockHeader;
begin
  if existing = nil then
    new := OsAllocLarge(newblocksize)
  else
    new := OsRemapLarge(existing, oldblocksize, newblocksize);
    // note: on Windows, newblocksize may now include LargeBlockIsSegmented flag
  if new <> nil then
  begin
    NotifyArenaAlloc(HeapStatus.Large, DropMediumAndLargeFlagsMask and newblocksize);
    if existing <> nil then
      NotifyMediumLargeFree(HeapStatus.Large, oldblocksize);
    new.BlockSizeAndFlags := newblocksize or IsLargeBlockFlag;
    LockLargeBlocks;
    old := LargeBlocksCircularList.NextLargeBlockHeader;
    new.PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := new;
    new.NextLargeBlockHeader := old;
    old.PreviousLargeBlockHeader := new;
    LargeBlocksLocked := false;
    inc(new);
  end;
  result := new;
end;

function AllocateLargeBlock(size: PtrUInt): pointer;
begin
  result := AllocateLargeBlockFrom(nil, 0, ComputeLargeBlockSize(size));
end;

procedure FreeLarge(ptr: PLargeBlockHeader; size: PtrUInt);
begin
  NotifyMediumLargeFree(HeapStatus.Large, size);
  OsFreeLarge(ptr, size);
end;

function FreeLargeBlock(p: pointer): PtrInt;
var
  header, prev, next: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  if header.BlockSizeAndFlags and IsFreeBlockFlag <> 0 then
  begin
    // try to release the same pointer twice
    result := 0;
    exit;
  end;
  LockLargeBlocks;
  prev := header.PreviousLargeBlockHeader;
  next := header.NextLargeBlockHeader;
  next.PreviousLargeBlockHeader := prev;
  prev.NextLargeBlockHeader := next;
  LargeBlocksLocked := false;
  result := DropMediumAndLargeFlagsMask and header.BlockSizeAndFlags;
  FreeLarge(header, result);
end; // returns the size for _FreeMem()

function ReallocateLargeBlock(p: pointer; size: PtrUInt): pointer;
var
  oldavail, minup, new, old: PtrUInt;
  prev, next, header: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  oldavail := (DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags) -
              (LargeBlockHeaderSize + BlockHeaderSize);
  new := size;
  if size > oldavail then
  begin
    // size-up with 1/8 or 1/4 overhead for any future growing realloc
    if oldavail > 128 shl 20 then
      minup := oldavail + oldavail shr 3
    else
      minup := oldavail + oldavail shr 2;
    if size < minup then
      new := minup;
  end
  else
  begin
    result := p;
    oldavail := oldavail shr 1;
    if size >= oldavail then
      // small size-up within current buffer -> no reallocate
      exit
    else
      // size-down and move just the trailing data
      oldavail := size;
  end;
  if new < MaximumMediumBlockSize then
  begin
    // size was reduced to a small/medium block: use GetMem/Move/FreeMem
    result := _GetMem(new);
    if result <> nil then
      Move(p^, result^, oldavail); // RTL non-volatile asm or our AVX MoveFast()
    _FreeMem(p);
  end
  else
  begin
    old := DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags;
    size := ComputeLargeBlockSize(new);
    if size = old then
      // no need to realloc anything (paranoid check: should be handled above)
      result := p
    else
    begin
      // remove previous large block from current chain list
      LockLargeBlocks;
      prev := header^.PreviousLargeBlockHeader;
      next := header^.NextLargeBlockHeader;
      next.PreviousLargeBlockHeader := prev;
      prev.NextLargeBlockHeader := next;
      LargeBlocksLocked := false;
      // on Linux, call Kernel mremap() and its TLB magic
      // on Windows, try to reserve the memory block just after the existing
      // otherwise, use Alloc/Move/Free pattern, with asm/AVX move
      result := AllocateLargeBlockFrom(header, old, size);
    end;
  end;
end;


{ ********* Main Memory Manager Functions }

function _GetMem(size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm     // size = rcx on Windows, = rdi on SystemV; use rsi = TSmallBlockType
        {$ifdef MSWINDOWS}
        push    rsi
        push    rdi
        {$endif MSWINDOWS}
        // Since most allocations are for small blocks, determine small block type
        lea     rsi, [rip + SmallBlockInfo]
@VoidSizeToSomething:
        lea     rdx, [size + BlockHeaderSize - 1]
        shr     rdx, 4 // div SmallBlockGranularity
        // Is it a tiny/small block?
        cmp     size, (MaximumSmallBlockSize - BlockHeaderSize)
        ja      @NotTinySmallBlock
        test    size, size
        jz      @VoidSize
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, qword ptr [rsi].TSmallBlockInfo.IsMultiThreadPtr
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Get the tiny/small TSmallBlockType[] offset in rcx
        movzx   ecx, byte ptr [rsi + rdx].TSmallBlockInfo.GetmemLookup
        mov     r8, rsi
        shl     ecx, SmallBlockTypePO2
        // ---------- Acquire block type lock ----------
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        cmp     byte ptr [rax], false
        je      @GotLockOnSmallBlock // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Can use one of the several arenas reserved for tiny blocks?
        cmp     ecx, SizeOf(TTinyBlockTypes)
        jae     @NotTinyBlockType
        // ---------- TINY (size<=128/256) block lock ----------
@LockTinyBlockTypeLoop:
        {$ifdef FPCMM_TINYPERTHREAD}
        lea     rsi, [r8 + rcx]
        mov     edx, $9E3779B1   // KNUTH_HASH32_MUL magic number
        {$ifdef LINUX}
        mov     rax, qword ptr [r8].TSmallBlockInfo.IsMultiThreadPtr
        cmp     byte ptr [rax], false
        je      @GotLockOnSmallBlockType // no pthread yet
        // mov rax,fs:[$00000010] = inlined pthread_self on Linux X86_64
        db $64, $48, $8B, $04, $25, $10, $00, $00, $00
        {$else}
        {$ifdef WINDOWS}
        // TEB.ClientId.UniqueThread (tested on Windows 7-11)
        db $65, $8B, $04, $25, $48, $00, $00, $00 // mov eax, gs:[$0048]
        {$else}
        unsupported
        {$endif WINDOWS}
        {$endif LINUX}
        imul    eax, edx // very fast on modern CPUs
        shr     eax, 32 - NumTinyBlockArenasPO2 // high bits hash truncate
        jz      @Aren0  // Arena 0 = TSmallBlockInfo.Small[]
        shl     eax, NumTinyBlockTypesPO2 + SmallBlockTypePO2 // TTinyBlockTypes
        lea	rsi, [rax + rsi + TSmallBlockInfo.Tiny - SizeOf(TTinyBlockTypes)]
@Aren0: mov     edx, NumTinyBlockArenas + 1 // 8/128 Small + Tiny[] arenas
        jmp     @TinySmall
        {$else}
        mov     edx, NumTinyBlockArenas + 1 // 8/128 Small + Tiny[] arenas
        {$endif FPCMM_TINYPERTHREAD}
        // Round-Robin attempt to lock next SmallBlockInfo.Tiny[]
@TinyBlockArenaLoop:
        mov     eax, SizeOf(TTinyBlockTypes)
        {$ifdef FPCMM_TINYPERTHREAD}
        // try next arenas following the per-thread one
        sub     rsi, r8
        sub     rsi, rcx
        jz      @Sml    // from Small[rcx] to Tiny[0][rcx]
        lea     rax, [rax * 2 + rsi - TSmallBlockInfo.Tiny] // Tiny[+1][rcx]
@Sml:   {$else}
        // fair distribution among calls to reduce thread contention
        {$ifdef FPCMM_BOOST}
        // "lock xadd" decreases loop iterations but is slower on normal load
        lock
        {$endif FPCMM_BOOST}
        xadd    dword ptr [r8 + TSmallBlockInfo.TinyCurrentArena], eax
        {$endif FPCMM_TINYPERTHREAD}
        lea     rsi, [r8 + rcx]
        and     eax, ((NumTinyBlockArenas + 1) * SizeOf(TTinyBlockTypes)) - 1
        jz      @TinySmall // Arena 0 = TSmallBlockInfo.Small[]
	lea	rsi, [rax + rsi + TSmallBlockInfo.Tiny - SizeOf(TTinyBlockTypes)]
@TinySmall:
        // Can we get a Tiny block from its LastFree list?
        cmp     dword ptr [rsi].TSmallBlockType.LastFreeCount, 0
        je      @NoLastFree
        cmp     byte ptr [rsi].TSmallBlockType.LastFreeLocked, false
        jne     @NoLastFree
        call    GetSmallLastFreeBlock
        {$ifdef NOSFRAME}
        jz      @NoLastFree
        ret
        {$else}
        jnz     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@NoLastFree:
        // Try to lock this Tiny block
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rsi].TSmallBlockType.Locked, false // no lock in loop
        jnz     @NextTinyBlockArena1
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
@NextTinyBlockArena1:
        dec     edx
        jnz     @TinyBlockArenaLoop
        // Fallback to SmallBlockInfo.Small[] next 2 small sizes - rarely needed
        lea     rsi, [r8 + rcx + TSmallBlockInfo.Small + SizeOf(TSmallBlockType)]
        mov     eax, $100
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        add     rsi, SizeOf(TSmallBlockType) // next two small sizes
        mov     eax, $100
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        // Thread Contention (_Freemem is more likely)
        movzx   rax, [rsi].TSmallBlockType.BlockSize
        shr     rax, 2 // div by SmallBlockGranularity then * SizeOf(cardinal)
   lock inc     dword ptr [r8 + rax - 4].TSmallBlockInfo.GetmemSleepCount
        call    ReleaseCoreSafe
        jmp     @LockTinyBlockTypeLoop
        // ---------- SMALL (size<2600) block lock ----------
@NotTinyBlockType:
        // Try to get a Small block from its SmallLastFree[] list or the next two
        lea     rsi, [r8 + rcx].TSmallBlockInfo.Small
        cmp     dword ptr [rsi].TSmallBlockType.LastFreeCount, 0
        je      @SLL0
        cmp     byte ptr [rsi].TSmallBlockType.LastFreeLocked, false
        je      @SmallLockLess0
@SLL0:  cmp     dword ptr [rsi + SmallBlockTypeSize].TSmallBlockType.LastFreeCount, 0
        je      @SLL1
        cmp     byte ptr [rsi + SmallBlockTypeSize].TSmallBlockType.LastFreeLocked, false
        je      @SmallLockLess1
@SLL1:  cmp     dword ptr [rsi + SmallBlockTypeSize * 2].TSmallBlockType.LastFreeCount, 0
        je      @LockBlockTypeLoopRetry
        cmp     byte ptr [rsi + SmallBlockTypeSize * 2].TSmallBlockType.LastFreeLocked, false
        je      @LockBlockTypeLoopRetry
        add     rsi, SizeOf(TSmallBlockType) * 2
        call    GetSmallLastFreeBlock
        jnz     {$ifdef NOSFRAME} @SLL {$else} @Quit {$endif}
        sub     rsi, SizeOf(TSmallBlockType) * 2
        jmp     @LockBlockTypeLoopRetry
@SmallLockLess0:
        call    GetSmallLastFreeBlock
        jz      @SLL0
@SLL:   {$ifdef NOSFRAME}
        ret
        {$else}
        jmp     @Quit// on Win64, a stack frame is required
        {$endif NOSFRAME}
@SmallLockLess1:
        add     rsi, SizeOf(TSmallBlockType)
        call    GetSmallLastFreeBlock
        jnz     {$ifdef NOSFRAME} @SLL {$else} @Quit {$endif}
        sub     rsi, SizeOf(TSmallBlockType)
        jmp     @SLL1
        // Try to lock this Small block or the next two
@LockBlockTypeLoopRetry:
        {$ifdef FPCMM_PAUSE}
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinSmallGetmemLockTSC] // r9 = endtsc
        {$else}
        mov    edx, SpinSmallGetmemLockCount
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
@LockBlockTypeLoop:
        // Grab the default block type
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rsi].TSmallBlockType.Locked, false
        jnz     @NextLockBlockType1
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        // Try up to two next sizes
        mov     eax, $100
@NextLockBlockType1:
        add     rsi, SizeOf(TSmallBlockType)
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rsi].TSmallBlockType.Locked, al
        jnz     @NextLockBlockType2
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        mov     eax, $100
@NextLockBlockType2:
        add     rsi, SizeOf(TSmallBlockType)
        pause
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rsi].TSmallBlockType.Locked, al
        jnz     @NextLockBlockType3
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
@NextLockBlockType3:
        sub     rsi, 2 * SizeOf(TSmallBlockType)
        {$ifdef FPCMM_PAUSE}
        pause
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        jb      @LockBlockTypeLoop // continue spinning until timeout
        {$else}
        dec     edx
        jnz     @LockBlockTypeLoop // continue until spin count reached
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
        // Block type and two sizes larger are all locked - give up and sleep
        lea     rcx, [rip + SmallBlockInfo]
        movzx   rax, [rsi].TSmallBlockType.BlockSize
        shr     rax, 2 // div by SmallBlockGranularity then * SizeOf(cardinal)
   lock inc     dword ptr [rcx + rax - 4].TSmallBlockInfo.GetmemSleepCount
        call    ReleaseCoreSafe
        jmp     @LockBlockTypeLoopRetry
        // ---------- TINY/SMALL block registration ----------
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
@GotLockOnSmallBlock:
        add     rsi, rcx
        {$endif FPCMM_ASSUMEMULTITHREAD}
@GotLockOnSmallBlockType:
        // set rdx=NextPartiallyFreePool rax=FirstFreeBlock rcx=DropSmallFlagsMask
        mov     rdx, [rsi].TSmallBlockType.NextPartiallyFreePool
        add     [rsi].TSmallBlockType.GetmemCount, 1
        mov     rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
        mov     rcx, DropSmallFlagsMask
        // Is there a pool with free blocks?
        cmp     rdx, rsi
        je      @TrySmallSequentialFeed
        add     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        // Set the new first free block and the block header
        and     rcx, [rax - BlockHeaderSize]
        mov     [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
        mov     [rax - BlockHeaderSize], rdx
        // Is the chunk now full?
        jz      @RemoveSmallPool
        // Unlock the block type and leave
        mov     byte ptr [rsi].TSmallBlockType.Locked, false
        {$ifdef NOSFRAME}
        ret
        {$else}
        jmp     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@VoidSize:
        inc     size // "we always need to allocate something" (see RTL heap.inc)
        jmp     @VoidSizeToSomething
@TrySmallSequentialFeed:
        // Feed a small block sequentially
        movzx   ecx, [rsi].TSmallBlockType.BlockSize
        mov     rdx, [rsi].TSmallBlockType.CurrentSequentialFeedPool
        add     rcx, rax
        // Can another block fit?
        cmp     rax, [rsi].TSmallBlockType.MaxSequentialFeedBlockAddress
        ja      @AllocateSmallBlockPool
        // Adjust number of used blocks and sequential feed pool
        mov     [rsi].TSmallBlockType.NextSequentialFeedBlockAddress, rcx
        add     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        // Unlock the block type, set the block header and leave
        mov     byte ptr [rsi].TSmallBlockType.Locked, false
        mov     [rax - BlockHeaderSize], rdx
        {$ifdef NOSFRAME}
        ret
        {$else}
        jmp     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@RemoveSmallPool:
        // Pool is full - remove it from the partially free list
        mov     rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rsi
        mov     [rsi].TSmallBlockType.NextPartiallyFreePool, rcx
        // Unlock the block type and leave
        mov     byte ptr [rsi].TSmallBlockType.Locked, false
        {$ifdef NOSFRAME}
        ret
        {$else}
        jmp     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@AllocateSmallBlockPool:
        // Access shared information about Medium blocks storage
        {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        mov     rax, rsi
        lea     rdx, [rip + SmallBlockInfo]
        sub     rax, rdx
        shr     eax, SmallBlockTypePO2 - 3 // 1 shl 3 = SizeOf(pointer)
        mov     rcx, [rdx + rax].TSmallBlockInfo.SmallMediumBlockInfo
        {$else}
        lea     rcx, [rip + SmallMediumBlockInfo]
        {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        mov     r10, rcx
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [rcx + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumLocked1 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumLocked1
        call    LockMediumBlocks
@MediumLocked1:
        // From now own rbx=TSmallBlockType, so we need to preserve it
        push    rbx
        mov     rbx, rsi
        // Are there any available blocks of a suitable size?
        movsx   esi, [rbx].TSmallBlockType.AllowedGroupsForBlockPoolBitmap
        and     esi, [r10 + TMediumBlockInfo.BinGroupBitmap]
        jz      @NoSuitableMediumBlocks
        // Compute rax = bin group number with free blocks, rcx = bin number
        bsf     eax, esi
        lea     r9, [rax * 4]
        mov     ecx, [r10 + TMediumBlockInfo.BinBitmaps + r9]
        bsf     ecx, ecx
        lea     rcx, [rcx + r9 * 8]
        // Set rdi = @bin, rsi = free block
        lea     rsi, [rcx * 8] // SizeOf(TMediumBlockInfo.Bins[]) = 16
        lea     rdi, [r10 + TMediumBlockInfo.Bins + rsi * 2]
        mov     rsi, TMediumFreeBlock[rdi].NextFreeBlock
        // Remove the first block from the linked list (LIFO)
        mov     rdx, TMediumFreeBlock[rsi].NextFreeBlock
        mov     TMediumFreeBlock[rdi].NextFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
        // Is this bin now empty?
        cmp     rdi, rdx
        jne     @MediumBinNotEmpty
        // rbx = block type, rax = bin group number,
        // r9 = bin group number * 4, rcx = bin number, rdi = @bin, rsi = free block
        // Flag this bin (and the group if needed) as empty
        mov     edx,  - 2
        rol     edx, cl
        and     [r10 + TMediumBlockInfo.BinBitmaps + r9], edx
        jnz     @MediumBinNotEmpty
        btr     [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@MediumBinNotEmpty:
        // rsi = free block, rbx = block type
        // Get the size of the available medium block in edi
        mov     rdi, DropMediumAndLargeFlagsMask
        and     rdi, [rsi - BlockHeaderSize]
        cmp     edi, MaximumSmallBlockPoolSize
        jb      @UseWholeBlock
        // Split the block: new block size is the optimal size
        mov     edx, edi
        movzx   edi, [rbx].TSmallBlockType.OptimalBlockPoolSize
        sub     edx, edi
        lea     rcx, [rsi + rdi]
        lea     rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], rax
        // Store the size of the second split as the second last pointer
        mov     [rcx + rdx - 16], rdx
        // Put the remainder in a bin (it will be big enough)
        call    InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
        jmp     @GotMediumBlock
@NoSuitableMediumBlocks:
        // Check the sequential feed medium block pool for space
        movzx   ecx, [rbx].TSmallBlockType.MinimumBlockPoolSize
        mov     edi, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        cmp     edi, ecx
        jb      @AllocateNewSequentialFeed
        // Get the address of the last block that was fed
        mov     rsi, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        // Enough sequential feed space: Will the remainder be usable?
        movzx   ecx, [rbx].TSmallBlockType.OptimalBlockPoolSize
        lea     rdx, [rcx + MinimumMediumBlockSize]
        cmp     edi, edx
        cmovae  edi, ecx
        sub     rsi, rdi
        // Update the sequential feed parameters
        sub     [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], edi
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rsi
        jmp     @GotMediumBlock
@AllocateNewSequentialFeed:
        // Use the optimal size for allocating this small block pool
        {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        mov     rax, rbx
        lea     rdx, [rip + SmallBlockInfo]
        sub     rax, rdx
        shr     eax, SmallBlockTypePO2 - 3 // 1 shl 3 = SizeOf(pointer)
        mov     rsi, [rdx + rax].TSmallBlockInfo.SmallMediumBlockInfo
        {$else}
        lea     rsi, [rip + SmallMediumBlockInfo]
        {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        {$ifdef MSWINDOWS}
        movzx   ecx, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
        mov     rdx, rsi
        push    rcx
        push    rdx
        {$else}
        movzx   edi, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
        push    rdi
        push    rsi
        {$endif MSWINDOWS}
        // on input: ecx/edi=BlockSize, rdx/rsi=Info
        call    AllocNewSequentialFeedMediumPool
        pop     r10
        pop     rdi  // restore edi=blocksize and r10=TMediumBlockInfo
        mov     rsi, rax
        test    rax, rax
        jnz     @GotMediumBlock // rsi=freeblock rbx=blocktype edi=blocksize
        mov     [r10 + TMediumBlockInfo.Locked], al
        mov     [rbx].TSmallBlockType.Locked, al
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@UseWholeBlock:
        // rsi = free block, rbx = block type, edi = block size
        // Mark this block as used in the block following it
        and     byte ptr [rsi + rdi - BlockHeaderSize],  NOT PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
        // rsi = free block, rbx = small block type, edi = block size
        // Set the size and flags for this block
        lea     rcx, [rdi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
        mov     [rsi - BlockHeaderSize], rcx
        // Unlock medium blocks and setup the block pool
        xor     eax, eax
        mov     [r10 + TMediumBlockInfo.Locked], al
        mov     TSmallBlockPoolHeader[rsi].BlockType, rbx
        mov     TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
        mov     TSmallBlockPoolHeader[rsi].BlocksInUse, 1
        mov     [rbx].TSmallBlockType.CurrentSequentialFeedPool, rsi
        // Return the pointer to the first block, compute next/last block addresses
        lea     rax, [rsi + SmallBlockPoolHeaderSize]
        movzx   ecx, [rbx].TSmallBlockType.BlockSize
        lea     rdx, [rax + rcx]
        mov     [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rdx
        add     rdi, rsi
        sub     rdi, rcx
        mov     [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rdi
        // Unlock the small block type, set header and leave
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        mov     [rax - BlockHeaderSize], rsi
        {$ifdef FPCMM_MEDIUMPREFETCH}
        // optional mmap() prefetch once we are outside the lock
        cmp     qword ptr [r10].TMediumBlockInfo.Prefetch, 0
        jnz     @SmallMediumPrefetchDone
        mov     rsi, rax // preserve result
        call    PrefetchMediumBlock
        mov     rax, rsi
@SmallMediumPrefetchDone:
        {$endif FPCMM_MEDIUMPREFETCH}
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ---------- MEDIUM block allocation ----------
@NotTinySmallBlock:
        // from now on, we may use the rbx register
        push    rbx
        // Do we need a Large block?
        {$ifndef FPCMM_MEDIUMPERTHREAD}
        lea     r10, [rip + MediumBlockInfo]
        {$endif FPCMM_MEDIUMPERTHREAD}
        cmp     size, MaximumMediumBlockSize - BlockHeaderSize
        ja      @IsALargeBlockRequest
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        lea     r10, [rip + MediumBlockInfo]
        xor     r9d, r9d
        mov     rax, qword ptr [rsi].TSmallBlockInfo.IsMultiThreadPtr
        cmp     byte ptr [rax], false
        je      @MediumArenaSelected
        mov     edx, $9E3779B1 // same per-thread hash as tiny arenas
        {$ifdef LINUX}
        // mov rax,fs:[$00000010] = inlined pthread_self on Linux X86_64
        db $64, $48, $8B, $04, $25, $10, $00, $00, $00
        {$else}
        {$ifdef WINDOWS}
        // TEB.ClientId.UniqueThread (tested on Windows 7-11)
        db $65, $8B, $04, $25, $48, $00, $00, $00 // mov eax, gs:[$0048]
        {$else}
        unsupported
        {$endif WINDOWS}
        {$endif LINUX}
        imul    eax, edx
        shr     eax, 32 - NumMediumBlockArenasPO2
        mov     r9d, eax
        lea     r10, [rip + MediumBlockInfoLookup]
        mov     r10, [r10 + rax * 8]
@MediumArenaSelected:
        {$endif FPCMM_MEDIUMPERTHREAD}
        // Get the bin size for this block size (rounded up to the next bin size)
        lea     rbx, [size + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
        mov     rcx, r10
        and     ebx,  - MediumBlockGranularity
        add     ebx, MediumBlockSizeOffset
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumLocked2 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumLocked2
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        // A hash collision should not make us wait while another arena is free.
        mov     r8d, NumMediumBlockArenas - 1
@TryNextMediumArena:
        inc     r9d
        and     r9d, NumMediumBlockArenas - 1
        lea     r10, [rip + MediumBlockInfoLookup]
        mov     r10, [r10 + r9 * 8]
        mov     rcx, r10
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumLocked2
        dec     r8d
        jnz     @TryNextMediumArena
        {$endif FPCMM_MEDIUMPERTHREAD}
        {$ifdef FPCMM_MEDIUMTOLARGE}
        // On contention, allocate medium blocks > 48KB as large blocks
        cmp     ebx, MediumBlockToLargeMinSize
        jb      @WaitForMedium // < 48KB won't be allocated as a large block
        // Spin a little then fallback to large block allocation path
        call    SpinLockMediumBlocks
        jz      @MediumLocked2
        // AllocateLargeBlock() would round it up by 64KB anyway
        mov     size, rbx // reset first parameter
        jmp     @IsALargeBlockRequest
@WaitForMedium:
        {$endif FPCMM_MEDIUMTOLARGE}
        call    LockMediumBlocks
@MediumLocked2:
        // Compute ecx = bin number in ecx and edx = group number
        lea     rdx, [rbx - MinimumMediumBlockSize]
        mov     ecx, edx
        shr     edx, 8 + 5
        shr     ecx, 8
        mov     eax, -1
        shl     eax, cl
        and     eax, [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4]
        jz      @GroupIsEmpty
        and     ecx,  - 32
        bsf     eax, eax
        or      ecx, eax
        jmp     @GotBinAndGroup
@GroupIsEmpty:
        // Try all groups greater than this group
        mov     eax,  - 2
        mov     ecx, edx
        shl     eax, cl
        and     eax, [r10 + TMediumBlockInfo.BinGroupBitmap]
        jz      @TrySequentialFeedMedium
        // There is a suitable group with enough space
        bsf     edx, eax
        mov     eax, [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4]
        bsf     ecx, eax
        mov     eax, edx
        shl     eax, 5
        or      ecx, eax
        jmp     @GotBinAndGroup
@TrySequentialFeedMedium:
        mov     ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        // Can block be fed sequentially?
        sub     ecx, ebx
        jc      @AllocateNewSequentialFeedForMedium
        // Get the block address, store remaining bytes, set the flags and unlock
        mov     rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        sub     rax, rbx
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rax
        mov     [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], ecx
        or      rbx, IsMediumBlockFlag
        mov     [rax - BlockHeaderSize], rbx
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@AllocateNewSequentialFeedForMedium:
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        {$ifdef MSWINDOWS}
        mov     ecx, ebx
        mov     rbx, r10 // preserve the selected arena across the Pascal call
        mov     rdx, rbx
        {$else}
        mov     edi, ebx
        mov     rbx, r10
        mov     rsi, rbx
        {$endif MSWINDOWS}
        {$else}
        {$ifdef MSWINDOWS}
        mov     ecx, ebx
        lea     rdx, [rip + MediumBlockInfo]
        {$else}
        mov     edi, ebx
        lea     rsi, [rip + MediumBlockInfo]
        {$endif MSWINDOWS}
        {$endif FPCMM_MEDIUMPERTHREAD}
        // on input: ecx/edi=BlockSize, rdx/rsi=Info
        call    AllocNewSequentialFeedMediumPool
        // restore r10=actual medium arena and unlock it
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        mov     r10, rbx
        {$else}
        lea     r10, [rip + MediumBlockInfo]
        {$endif FPCMM_MEDIUMPERTHREAD}
        mov     byte ptr [r10].TMediumBlockInfo.Locked, false
        {$ifdef FPCMM_MEDIUMPREFETCH}
        // optional mmap() prefetch once we are outside the lock
        test    rax, rax
        jz      @MediumPrefetchDone // allocation failed
        cmp     qword ptr [r10].TMediumBlockInfo.Prefetch, 0
        jnz     @MediumPrefetchDone
        mov     rsi, rax // preserve result
        call    PrefetchMediumBlock
        mov     rax, rsi
@MediumPrefetchDone:
        {$endif FPCMM_MEDIUMPREFETCH}
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@GotBinAndGroup:
        // ebx = block size, ecx = bin number, edx = group number
        // Compute rdi = @bin, rsi = free block
        lea     rax, [rcx + rcx]
        lea     rdi, [r10 + TMediumBlockInfo.Bins + rax * 8]
        mov     rsi, TMediumFreeBlock[rdi].NextFreeBlock
        // Remove the first block from the linked list (LIFO)
        mov     rax, TMediumFreeBlock[rsi].NextFreeBlock
        mov     TMediumFreeBlock[rdi].NextFreeBlock, rax
        mov     TMediumFreeBlock[rax].PreviousFreeBlock, rdi
        // Is this bin now empty?
        cmp     rdi, rax
        jne     @MediumBinNotEmptyForMedium
        // edx=bingroupnumber, ecx=binnumber, rdi=@bin, rsi=freeblock, ebx=blocksize
        // Flag this bin (and the group if needed) as empty
        mov     eax,  - 2
        rol     eax, cl
        and     [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        jnz     @MediumBinNotEmptyForMedium
        btr     [r10 + TMediumBlockInfo.BinGroupBitmap], edx
@MediumBinNotEmptyForMedium:
        // rsi = free block, ebx = block size
        // Get rdi = size of the available medium block, rdx = second split size
        mov     rdi, DropMediumAndLargeFlagsMask
        and     rdi, [rsi - BlockHeaderSize]
        mov     edx, edi
        sub     edx, ebx
        jz      @UseWholeBlockForMedium
        // Split the block in two
        lea     rcx, [rsi + rbx]
        lea     rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], rax
        // Store the size of the second split as the second last pointer
        mov     [rcx + rdx - 16], rdx
        // Put the remainder in a bin
        cmp     edx, MinimumMediumBlockSize
        jb      @GotMediumBlockForMedium
        call    InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
        jmp     @GotMediumBlockForMedium
@UseWholeBlockForMedium:
        // Mark this block as used in the block following it
        and     byte ptr [rsi + rdi - BlockHeaderSize], NOT PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
        // Set the size and flags for this block
        lea     rcx, [rbx + IsMediumBlockFlag]
        mov     [rsi - BlockHeaderSize], rcx
        // Unlock medium blocks and leave
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, rsi
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ---------- LARGE block allocation ----------
@IsALargeBlockRequest:
        xor     rax, rax
        test    size, size
        js      @Done
        // Note: size is still in the rcx/rdi first param register
        call    AllocateLargeBlock
@Done:  // restore registers and the stack frame before ret
        pop     rbx
@Quit:  {$ifdef MSWINDOWS}
        pop     rdi
        pop     rsi
        {$endif MSWINDOWS}
end;

function FreeMediumBlock(arg1, arg2: pointer): PtrUInt;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// rcx=P rdx=[P-BlockHeaderSize] r10=TMediumBlockInfo
// (arg1/arg2 are used only for proper call of pascal functions below on all ABI)
asm
        // Drop the flags, and set r11=P rbx=blocksize
        and     rdx, DropMediumAndLargeFlagsMask
        push    rbx
        push    rdx // save blocksize
        mov     rbx, rdx
        mov     r11, rcx
        // Lock the Medium blocks
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked
        // Locked: add r11=P in TMediumBlockInfo.LastFree and Quit
@Atom0: mov     eax, $100
        pause
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.LastFreeLocked, ah
        jne     @Atom0
        mov     rax, [rcx].TMediumBlockInfo.LastFree
        mov     [r11], rax // use freed buffer as next linked list slot
        mov     [rcx].TMediumBlockInfo.LastFree, r11 // in list
        mov     byte ptr [rcx + TMediumBlockInfo.LastFreeLocked], false
        jmp     @Quit
@MediumBlocksLocked:
        // We acquired the lock: get rcx = next block size and flags
        mov     rcx, [r11 + rbx - BlockHeaderSize]
        // Can we combine this block with the next free block?
        test    qword ptr [r11 + rbx - BlockHeaderSize], IsFreeBlockFlag
        jnz     @NextBlockIsFree
        // Set the "PreviousIsFree" flag in the next block
        or      rcx, PreviousMediumBlockIsFreeFlag
        mov     [r11 + rbx - BlockHeaderSize], rcx
@NextBlockChecked:
        // Re-read the flags and try to combine with previous free block
        test    byte ptr [r11 - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
        jnz     @PreviousBlockIsFree
@PreviousBlockChecked:
        // Check if entire medium block pool is free
        cmp     ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
        je      @EntireMediumPoolFree
@Bin:   // Store size of the block, flags and trailing size marker and insert into bin
        lea     rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [r11 - BlockHeaderSize], rax
        mov     [r11 + rbx - 16], rbx
        mov     rcx, r11
        mov     rdx, rbx
        call    InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
        // Check if some LastFree is pending
        cmp     qword ptr [r10].TMediumBlockInfo.LastFree, 0
        jnz     @LastFree
@Done:  // Unlock medium blocks and leave
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        jmp     @Quit
@LastFree:
        // Release the next LastFree list block while we own the lock
@Atom1: mov     rcx, r10
        mov     eax, $100
        pause
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.LastFreeLocked, ah
        jne     @Atom1
        mov     r11, [rcx].TMediumBlockInfo.LastFree
        test    r11, r11
        jz      @Done
        mov     rax, [r11]
        mov     [rcx].TMediumBlockInfo.LastFree, rax
        mov     byte ptr [r10 + TMediumBlockInfo.LastFreeLocked], false
@OneBin:// Compute rbx=blocksize of r11 pointer retrieved from LastFree list
        mov     rbx, qword ptr [r11 - BlockHeaderSize]
        and     rbx, DropMediumAndLargeFlagsMask
        jmp     @MediumBlocksLocked
@NextBlockIsFree:
        // Get rax = next block address, rbx = end of the block
        lea     rax, [r11 + rbx]
        and     rcx, DropMediumAndLargeFlagsMask
        add     rbx, rcx
        // Was the block binned?
        cmp     rcx, MinimumMediumBlockSize
        jb      @NextBlockChecked
        mov     rcx, rax
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        jmp     @NextBlockChecked
@PreviousBlockIsFree:
        // Get rcx =  size/point of the previous free block, rbx = new block end
        mov     rcx, [r11 - 16]
        sub     r11, rcx
        add     rbx, rcx
        // Remove the previous block from the linked list
        cmp     ecx, MinimumMediumBlockSize
        jb      @PreviousBlockChecked
        mov     rcx, r11
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        jmp     @PreviousBlockChecked
@EntireMediumPoolFree:
        // Ensure current sequential feed pool is free
        cmp     dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
        jne     @MakeEmptyMediumPoolSequentialFeed
        // Remove this medium block pool from the linked list stored in its header
        sub     r11, MediumBlockPoolHeaderSize
        mov     rax, TMediumBlockPoolHeader[r11].PreviousMediumBlockPoolHeader
        mov     rdx, TMediumBlockPoolHeader[r11].NextMediumBlockPoolHeader
        mov     TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
        mov     TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
        // Unlock medium blocks and free the block pool
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     arg1, r11
        mov     arg2, r10
        call    FreeMedium // munmap() may take time - or cache in Info.Prefetch
        jmp     @Quit
@MakeEmptyMediumPoolSequentialFeed:
        // Get rbx = end-marker block, and recycle the current sequential feed pool
        lea     rbx, [r11 + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
        mov     arg1, r10
        call    BinMediumSequentialFeedRemainder
        // Set this medium pool up as the new sequential feed pool, unlock and leave
        mov     qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
        mov     dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rbx
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
@Quit:  // restore registers and the stack frame
        pop     rax // medium block size
        pop     rbx
end;

{$ifdef FPCMM_REPORTMEMORYLEAKS}
const
  /// mark freed blocks with 00000000 BLODLESS marker to track incorrect usage
  REPORTMEMORYLEAK_FREEDHEXSPEAK = $B10D1E55;
{$endif FPCMM_REPORTMEMORYLEAKS}

function _FreeMem(P: pointer): PtrUInt;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm     // P = rcx on Windows, P = rdi on SystemV; use rsi = TSmallBlockType
        {$ifndef MSWINDOWS}
        mov     rcx, P
        {$endif MSWINDOWS}
        {$ifdef FPCMM_REPORTMEMORYLEAKS}
        mov     eax, REPORTMEMORYLEAK_FREEDHEXSPEAK // 00000000 BLODLESS marker
        {$endif FPCMM_REPORTMEMORYLEAKS}
        test    P, P
        jz      @Void
        {$ifdef FPCMM_REPORTMEMORYLEAKS}
        mov     [rcx], rax // overwrite TObject VMT or string/dynarray header
        {$endif FPCMM_REPORTMEMORYLEAKS}
        mov     rdx, [rcx - BlockHeaderSize]
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, qword ptr [rip + SmallBlockInfo].TSmallBlockInfo.IsMultiThreadPtr
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Is it a small block in use?
        test    dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
        jnz     @NotSmallBlockInUse
        // Get the small block type in rsi and try to grab it
        {$ifdef MSWINDOWS}
        push    rsi
        {$endif MSWINDOWS}
        mov     rsi, [rdx].TSmallBlockPoolHeader.BlockType
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        cmp     byte ptr [rax], false
        je      @FreeAndUnLock
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.Locked, ah
        jne     @TinySmallLocked
@FreeAndUnlock:
        // rsi=TSmallBlockType rcx=P rdx=TSmallBlockPoolHeader
        // Adjust number of blocks in use, set rax = old first free block
        add     [rsi].TSmallBlockType.FreememCount, 1
        mov     rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
        sub     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        jz      @PoolIsNowEmpty
@StoreFreeBlock:
        // Store this as the new first free block
        mov     [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
        // Store the previous first free block as the block header
        lea     r9, [rax + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], r9
        // Was the pool full?
        test    rax, rax
        jnz     @SmallPoolWasNotFull
        // Insert the pool back into the linked list if it was full
        mov     rcx, [rsi].TSmallBlockType.NextPartiallyFreePool
        mov     [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rsi
        mov     [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool, rcx
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rdx
        mov     [rsi].TSmallBlockType.NextPartiallyFreePool, rdx
@SmallPoolWasNotFull:
        // Try to release all pending bin from this block while we have the lock
        cmp     dword ptr [rsi].TSmallBlockType.LastFreeCount, 0
        jne     @ProcessPendingBin
        // Release the lock and return the block size as FPC RTL MM
@NoBin: mov     byte ptr [rsi].TSmallBlockType.Locked, false
        movzx   eax, word ptr [rsi].TSmallBlockType.BlockSize
        {$ifdef NOSFRAME}
        {$ifdef MSWINDOWS}
        pop     rsi
        {$endif MSWINDOWS}
        ret
@Void:  xor     eax, eax
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
@Void:  xor     eax, eax
        jmp     @Quit
        {$endif NOSFRAME}
@PoolIsNowEmpty:
        // FirstFreeBlock=nil means it is the sequential feed pool with a single block
        test    rax, rax
        {$ifdef FPCMM_SERVER}
        jz      @EmptySequentialFeedPool
        {$else}
        jz      @IsSequentialFeedPool
        {$endif FPCMM_SERVER}
        // Pool is now empty: Remove it from the linked list and free it
        mov     rax, [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool
        mov     rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
        mov     TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rax
        // Is this the sequential feed pool? If so, stop sequential feeding
        xor     eax, eax
        cmp     [rsi].TSmallBlockType.CurrentSequentialFeedPool, rdx
        jne     @NotSequentialFeedPool
        {$ifdef FPCMM_SERVER}
        // A drained multi-block sequential pool ends single-block churn.
        // Reset on this cold release path, not on every sequential GetMem.
        mov     byte ptr [rsi].TSmallBlockType.EmptyPoolReuse, 0
        {$endif FPCMM_SERVER}
@IsSequentialFeedPool:
        mov     [rsi].TSmallBlockType.MaxSequentialFeedBlockAddress, rax
@NotSequentialFeedPool:
        // Unlock blocktype and release this pool
        mov     byte ptr [rsi].TSmallBlockType.Locked, false
        mov     rcx, rdx
        mov     rdx, [rdx - BlockHeaderSize]
        {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        mov     rax, rsi
        lea     r10, [rip + SmallBlockInfo]
        sub     rax, r10
        shr     eax, SmallBlockTypePO2 - 3 // 1 shl 3 = SizeOf(pointer)
        mov     r10, [r10 + rax].TSmallBlockInfo.SmallMediumBlockInfo
        {$else}
        lea     r10, [rip + SmallMediumBlockInfo]
        {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
        movzx   eax, word ptr [rsi].TSmallBlockType.BlockSize
        push    rax
        call    FreeMediumBlock // no call nor BinLocked to avoid race condition
        pop     rax
        {$ifdef NOSFRAME}
        {$ifdef MSWINDOWS}
        pop     rsi
        {$endif MSWINDOWS}
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        {$ifdef FPCMM_SERVER}
@EmptySequentialFeedPool:
        // Larger small classes share one block type process-wide
        cmp     word ptr [rsi].TSmallBlockType.BlockSize, MaximumTinyBlockSize
        jbe     @IsSequentialFeedPool
        // Retain one pool only after repeated single-block churn
        cmp     byte ptr [rsi].TSmallBlockType.EmptyPoolReuse, SmallBlockRetention
        jae     @StoreFreeBlock
        inc     byte ptr [rsi].TSmallBlockType.EmptyPoolReuse
        // Counter is reset once the sequential pool serves more than one block
        jmp     @IsSequentialFeedPool
        {$endif FPCMM_SERVER}
@ProcessPendingBin:
        // Release the next SmallLastFree list block while we own the lock
        cmp     byte ptr [rsi].TSmallBlockType.LastFreeLocked, false
        jne     @NoBin
        call    GetSmallLastFreeBlock
        jz      @NoBin
        mov     rcx, rax
        mov     rdx, [rax - BlockHeaderSize]
        // rsi=TSmallBlockType rcx=P rdx=TSmallBlockPoolHeader
        jmp     @FreeAndUnlock // will loop until LastFreeCount=0
@NotSmallBlockInUse:
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        test    dl, IsFreeBlockFlag + IsLargeBlockFlag
        // P is still in rcx/rdi first param register
        jnz     @MediumOrLargeIsLarge
        mov     r10, rcx
        and     r10, not MediumBlockAlignmentMask
        mov     r10, [r10 + TMediumBlockPoolHeader.MediumPerThreadBase]
        {$ifdef NOSFRAME}
        jmp     FreeMediumBlock
@MediumOrLargeIsLarge:
        jmp     FreeLargeBlock
        {$else}
        call    FreeMediumBlock
        jmp     @Quit
@MediumOrLargeIsLarge:
        call    FreeLargeBlock
        jmp     @Quit
        {$endif NOSFRAME}
        {$else}
        lea     r10, [rip + MediumBlockInfo]
        test    dl, IsFreeBlockFlag + IsLargeBlockFlag
        // P is still in rcx/rdi first param register
        {$ifdef NOSFRAME}
        jz      FreeMediumBlock
        jmp     FreeLargeBlock // local function returns 0 or the block size
        {$else} // on Win64, a stack frame is required
        jz      @Medium
        call    FreeLargeBlock
        jmp     @Quit
@Medium:call    FreeMediumBlock
        jmp     @Quit
        {$endif NOSFRAME}
        {$endif FPCMM_MEDIUMPERTHREAD}
@TinySmallLocked:
        // This small block is locked: add rcx=P to the LastFree list block
        mov     rax, rsi
        lea     r10, [rip + SmallBlockInfo]
        sub     rax, r10
        shr     eax, SmallBlockTypePO2 - 3 // 1 shl 3 = SizeOf(pointer)
        lea     r10, [r10 + rax].TSmallBlockInfo.SmallLastFree
        // r10 = @SmallLastFree[] of this rsi
@Atom2: mov     eax, $100
  lock  cmpxchg byte ptr [rsi].TSmallBlockType.LastFreeLocked, ah
        je      @Atom3
        pause
        jmp     @Atom2
@Atom3: mov     rax, [r10]
        mov     [rcx], rax  // very simple linked list
        mov     [r10], rcx
        inc     dword ptr [rsi].TSmallBlockType.LastFreeCount
        mov     byte ptr [rsi].TSmallBlockType.LastFreeLocked, false
        movzx   eax, word ptr [rsi].TSmallBlockType.BlockSize
@Done:  // restore rsi and the stack frame before ret
        {$ifdef MSWINDOWS}
        pop     rsi
        {$endif MSWINDOWS}
@Quit:
end;

// warning: FPC signature is not the same than Delphi: requires "var P"
function _ReallocMem(var P: pointer; Size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        {$ifdef MSWINDOWS}
        push    rdi
        push    rsi
        {$else}
        mov     rdx, Size
        {$endif MSWINDOWS}
        push    rbx
        push    r14
        push    P // for assignement in @Done
        mov     r14, qword ptr [P]
        test    rdx, rdx
        jz      @VoidSize  // ReallocMem(P,0)=FreeMem(P)
        test    r14, r14
        jz      @GetMemMoveFreeMem // ReallocMem(nil,Size)=GetMem(Size)
        mov     rcx, [r14 - BlockHeaderSize]
        test    cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
        jnz     @NotASmallBlock
        // -------------- TINY/SMALL block -------------
        // Get rbx=blocktype, rcx=available size, rax=inplaceresize
        mov     rbx, [rcx].TSmallBlockPoolHeader.BlockType
        lea     rax, [rdx * 4 + SmallBlockDownsizeCheckAdder]
        movzx   ecx, [rbx].TSmallBlockType.BlockSize
        sub     ecx, BlockHeaderSize
        cmp     rcx, rdx
        jb      @SmallUpsize
        // Downsize or small growup with enough space: reallocate only if need
        cmp     eax, ecx
        jb      @GetMemMoveFreeMem // r14=P rdx=size
@NoResize:
        // branchless execution if current block is good enough for this size
        mov     rax, r14 // keep original pointer
        pop     rcx
        {$ifdef NOSFRAME}
        pop     r14
        pop     rbx
        ret
        {$else}
        jmp     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@VoidSize:
        push    rdx    // to set P=nil
        jmp     @DoFree // ReallocMem(P,0)=FreeMem(P)
@SmallUpsize:
        // State: r14=pointer, rdx=NewSize, rcx=CurrentBlockSize, rbx=CurrentBlockType
        // Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes
        lea     P, qword ptr [rcx * 2 + SmallBlockUpsizeAdder]
        movzx   ebx, [rbx].TSmallBlockType.BlockSize
        sub     ebx, BlockHeaderSize + 8
        // r14=pointer, P=NextUpBlockSize, rdx=NewSize, rbx=OldSize-8
@AdjustGetMemMoveFreeMem:
        // New allocated size is max(requestedsize, minimumupsize)
        cmp     rdx, P
        cmova   P, rdx
        push    rdx
        call    _GetMem
        pop     rdx
        test    rax, rax
        jz      @Done
        jmp     @MoveFreeMem // rax=New r14=P rbx=size-8
@GetMemMoveFreeMem:
        // reallocate copy and free: r14=P rdx=size
        mov     rbx, rdx
        mov     P, rdx // P is the proper first argument register
        call    _GetMem
        test    rax, rax
        jz      @Done
        test    r14, r14 // ReallocMem(nil,Size)=GetMem(Size)
        jz      @Done
        sub     rbx, 8
@MoveFreeMem:
        // copy and free: rax=New r14=P rbx=size-8
        push    rax
        {$ifdef FPCMM_ERMS}
        cmp     rbx, ErmsMinSize // startup cost of 0..255 bytes
        jae     @erms
        {$endif FPCMM_ERMS}
        lea     rcx, [r14 + rbx]
        lea     rdx, [rax + rbx]
        neg     rbx
        jns     @Last8
        align   16
@By16:  movaps  xmm0, oword ptr [rcx + rbx]
        movaps  oword ptr [rdx + rbx], xmm0
        add     rbx, 16
        js      @By16
@Last8: mov     rax, qword ptr [rcx + rbx]
        mov     qword ptr [rdx + rbx], rax
@DoFree:mov     P, r14
        call    _FreeMem
        pop     rax
        jmp     @Done
        {$ifdef FPCMM_ERMS}
@erms:  cld
        mov     rsi, r14
        mov     rdi, rax
        lea     rcx, [rbx + 8]
        rep movsb
        jmp     @DoFree
        {$endif FPCMM_ERMS}
@NotASmallBlock:
        // Is this a medium block or a large block?
        test    cl, IsFreeBlockFlag + IsLargeBlockFlag
        jnz     @PossibleLargeBlock
        // -------------- MEDIUM block -------------
        // rcx=CurrentSize+Flags, r14=P, rdx=RequestedSize, r10=TMediumBlockInfo
        lea     rsi, [rdx + rdx]
        {$ifdef FPCMM_MEDIUMPERTHREAD}
        mov     r10, r14
        and     r10, not MediumBlockAlignmentMask
        mov     r10, [r10 + TMediumBlockPoolHeader.MediumPerThreadBase]
        {$else}
        lea     r10, [rip + MediumBlockInfo]
        {$endif FPCMM_MEDIUMPERTHREAD}
        mov     rbx, rcx
        and     ecx, DropMediumAndLargeFlagsMask
        lea     rdi, [r14 + rcx]
        sub     ecx, BlockHeaderSize
        and     ebx, ExtractMediumAndLargeFlagsMask
        // Is it an upsize or a downsize?
        cmp     rdx, rcx
        ja      @MediumBlockUpsize
        // rcx=CurrentBlockSize-BlockHeaderSize, rbx=CurrentBlockFlags,
        // rdi=@NextBlock, r14=P, rdx=RequestedSize
        // Downsize reallocate and move data only if less than half the current size
        cmp     rsi, rcx
        jae     @NoResize
        // In-place downsize? Ensure not smaller than MinimumMediumBlockSize
        cmp     edx, MinimumMediumBlockSize - BlockHeaderSize
        jae     @MediumBlockInPlaceDownsize
        // Need to move to another Medium block pool, or into a Small block?
        cmp     edx, MediumInPlaceDownsizeLimit
        jb      @GetMemMoveFreeMem
        // No need to realloc: resize in-place (if not already at the minimum size)
        mov     edx, MinimumMediumBlockSize - BlockHeaderSize
        cmp     ecx, MinimumMediumBlockSize - BlockHeaderSize
        jna     @NoResize
@MediumBlockInPlaceDownsize:
        // Round up to the next medium block size
        lea     rsi, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
        and     rsi,  - MediumBlockGranularity
        add     rsi, MediumBlockSizeOffset
        // Get the size of the second split
        add     ecx, BlockHeaderSize
        sub     ecx, esi
        mov     ebx, ecx
        // Lock the medium blocks
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked1 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked1
        call    LockMediumBlocks
@MediumBlocksLocked1:
        mov     ecx, ebx
        // Reread the flags - may have changed before medium blocks could be locked
        mov     rbx, ExtractMediumAndLargeFlagsMask
        and     rbx, [r14 - BlockHeaderSize]
@DoMediumInPlaceDownsize:
        // Set the new size in header, and get rbx = second split size
        or      rbx, rsi
        mov     [r14 - BlockHeaderSize], rbx
        mov     ebx, ecx
        // If the next block is used, flag its previous block as free
        mov     rdx, [rdi - BlockHeaderSize]
        test    dl, IsFreeBlockFlag
        jnz     @MediumDownsizeNextBlockFree
        or      rdx, PreviousMediumBlockIsFreeFlag
        mov     [rdi - BlockHeaderSize], rdx
        jmp     @MediumDownsizeDoSplit
@MediumDownsizeNextBlockFree:
        // If the next block is free, combine both
        mov     rcx, rdi
        and     rdx, DropMediumAndLargeFlagsMask
        add     rbx, rdx
        add     rdi, rdx
        cmp     edx, MinimumMediumBlockSize
        jb      @MediumDownsizeDoSplit
        call    RemoveMediumFreeBlock // rcx=APMediumFreeBlock
@MediumDownsizeDoSplit:
        // Store the trailing size field and free part header
        mov     [rdi - 16], rbx
        lea     rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
        mov     [r14 + rsi - BlockHeaderSize], rcx
        // Bin this free block (if worth it)
        cmp     rbx, MinimumMediumBlockSize
        jb      @MediumBlockDownsizeDone
        lea     rcx, [r14 + rsi]
        mov     rdx, rbx
        call    InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
@MediumBlockDownsizeDone:
        // Unlock the medium blocks, and leave with the new pointer
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, r14
        jmp     @Done
@MediumBlockUpsize:
        // ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
        // rdi = @Next Block, r14 = P, rdx = Requested Size
        // Try to make in-place upsize
        mov     rax, [rdi - BlockHeaderSize]
        test    al, IsFreeBlockFlag
        jz      @CannotUpsizeMediumBlockInPlace
        // Get rax = available size, rsi = available size with the next block
        and     rax, DropMediumAndLargeFlagsMask
        lea     rsi, [rax + rcx]
        cmp     rdx, rsi
        ja      @CannotUpsizeMediumBlockInPlace
        // Grow into the next block
        mov     rbx, rcx
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked2 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked2
        mov     rsi, rdx
        call    LockMediumBlocks
        mov     rdx, rsi
@MediumBlocksLocked2:
        // Re-read info once locked, and ensure next block is still free
        mov     rcx, rbx
        mov     rbx, ExtractMediumAndLargeFlagsMask
        and     rbx, [r14 - BlockHeaderSize]
        mov     rax, [rdi - BlockHeaderSize]
        test    al, IsFreeBlockFlag
        jz      @NextMediumBlockChanged
        and     eax, DropMediumAndLargeFlagsMask
        lea     rsi, [rax + rcx]
        cmp     rdx, rsi
        ja      @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
        // Bin next free block (if worth it)
        cmp     eax, MinimumMediumBlockSize
        jb      @MediumInPlaceNoNextRemove
        push    rcx
        push    rdx
        mov     rcx, rdi
        call    RemoveMediumFreeBlock // rcx=APMediumFreeBlock
        pop     rdx
        pop     rcx
@MediumInPlaceNoNextRemove:
        // Medium blocks grow a minimum of 25% in in-place upsizes
        mov     eax, ecx
        shr     eax, 2
        add     eax, ecx
        // Get the maximum of the requested size and the minimum growth size
        xor     edi, edi
        sub     eax, edx
        adc     edi, -1
        and     eax, edi
        // Round up to the nearest block size granularity
        lea     rax, [rax + rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
        and     eax, -MediumBlockGranularity
        add     eax, MediumBlockSizeOffset
        // Calculate the size of the second split and check if it fits
        lea     rdx, [rsi + BlockHeaderSize]
        sub     edx, eax
        ja      @MediumInPlaceUpsizeSplit
        // Grab the whole block: Mark it as used in the next block, and adjust size
        and     qword ptr [r14 + rsi],  NOT PreviousMediumBlockIsFreeFlag
        add     rsi, BlockHeaderSize
        jmp     @MediumUpsizeInPlaceDone
@MediumInPlaceUpsizeSplit:
        // Store the size of the second split as the second last pointer
        mov     [r14 + rsi - BlockHeaderSize], rdx
        // Set the second split header
        lea     rdi, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [r14 + rax - BlockHeaderSize], rdi
        mov     rsi, rax
        cmp     edx, MinimumMediumBlockSize
        jb      @MediumUpsizeInPlaceDone
        lea     rcx, [r14 + rax]
        call    InsertMediumBlockIntoBin // rcx=P edx=blocksize r10=Info
@MediumUpsizeInPlaceDone:
        // No need to move data at upsize: set the size and flags for this block
        or      rsi, rbx
        mov     [r14 - BlockHeaderSize], rsi
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, r14
        jmp     @Done
@NextMediumBlockChanged:
        // The next block changed during lock: reallocate and move data
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
@CannotUpsizeMediumBlockInPlace:
        // rcx=OldSize-8, rdx=NewSize
        mov     rbx, rcx
        mov     eax, ecx
        shr     eax, 2
        lea     P, qword ptr [rcx + rax] // NextUpBlockSize = OldSize+25%
        jmp     @AdjustGetMemMoveFreeMem // P=BlockSize, rdx=NewSize, rbx=OldSize-8
@PossibleLargeBlock:
        // -------------- LARGE block -------------
        test    cl, IsFreeBlockFlag + IsMediumBlockFlag
        jnz     @Error
        {$ifdef MSWINDOWS}
        mov     rcx, r14
        {$else}
        mov     rdi, r14
        mov     rsi, rdx
        {$endif MSWINDOWS}
        call    ReallocateLargeBlock // with restored proper registers
        jmp     @Done
@Error: xor     eax, eax
@Done:  // restore registers and the stack frame before ret
        pop     rcx
        mov     qword ptr [rcx], rax // store new pointer in var P
@Quit:  pop     r14
        pop     rbx
        {$ifdef MSWINDOWS}
        pop     rsi
        pop     rdi
        {$endif MSWINDOWS}
end;

function _AllocMem(Size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        push    rbx
        // Compute rbx = size rounded down to the last pointer
        lea     rbx, [Size - 1]
        and     rbx,  - 8
        // Perform the memory allocation
        call    _GetMem
        // Could a block be allocated? rcx = 0 if yes, -1 if no
        cmp     rax, 1
        sbb     rcx, rcx
        // Point rdx to the last pointer
        lea     rdx, [rax + rbx]
        // Compute Size (1..8 doesn't need to enter the SSE2 loop)
        or      rbx, rcx
        jz      @LastQ
        // Large blocks from mmap/VirtualAlloc are already zero filled
        cmp     rbx, MaximumMediumBlockSize - BlockHeaderSize
        jae     @Done
        {$ifdef FPCMM_ERMS}
        cmp     rbx, ErmsMinSize // startup cost of 0..255 bytes
        jae     @erms
        {$endif FPCMM_ERMS}
        neg     rbx
        pxor    xmm0, xmm0
        align   16
@FillLoop: // non-temporal movntdq not needed with small/medium size
        movaps  oword ptr [rdx + rbx], xmm0
        add     rbx, 16
        js      @FillLoop
        // fill the last pointer
@LastQ: xor     rcx, rcx
        mov     qword ptr [rdx], rcx
        {$ifdef FPCMM_ERMS}
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ERMS has a startup cost, but "rep stosd" is fast enough on all CPUs
@erms:  mov     rcx, rbx
        push    rax
        {$ifdef MSWINDOWS}
        push    rdi
        {$endif MSWINDOWS}
        cld
        mov     rdi, rdx
        xor     eax, eax
        sub     rdi, rbx
        shr     ecx, 2
        mov     qword ptr [rdx], rax
        rep stosd
        {$ifdef MSWINDOWS}
        pop     rdi
        {$endif MSWINDOWS}
        pop     rax
        {$endif FPCMM_ERMS}
@Done:  // restore rbx register and the stack frame before ret
        pop     rbx
end;

function _MemSize(P: pointer): PtrUInt;
begin
  // AFAIK used only by fpc_AnsiStr_SetLength() in FPC RTL
  // also used by our static SQLite3 for its xSize() callback
  P := PPointer(PByte(P) - BlockHeaderSize)^;
  if (PtrUInt(P) and (IsMediumBlockFlag or IsLargeBlockFlag)) = 0 then
    result := PSmallBlockPoolHeader(PtrUInt(P) and DropSmallFlagsMask).
      BlockType.BlockSize - BlockHeaderSize
  else
  begin
    result := (PtrUInt(P) and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
    if (PtrUInt(P) and IsMediumBlockFlag) = 0 then
      dec(result, LargeBlockHeaderSize);
  end;
end;

function _FreeMemSize(P: pointer; size: PtrUInt): PtrInt;
begin
  // size = 0 needs to call _FreeMem() because GetMem(P,0) returned something
  result := _FreeMem(P); // P=nil will return 0
  // returns the chunk size - only used by heaptrc AFAIK
end;


{ ********* Information Gathering }

{$ifdef FPCMM_STANDALONE}

procedure Assert(flag: boolean);
begin
end;

{$else}

function _GetFPCHeapStatus: TFPCHeapStatus;
var
  mm: PMMStatus;
begin
  mm := @HeapStatus;
  {$ifdef FPCMM_DEBUG}
  result.MaxHeapSize := mm^.Medium.PeakBytes + mm^.Large.PeakBytes;
  {$else}
  result.MaxHeapSize := 0;
  {$endif FPCMM_DEBUG}
  result.MaxHeapUsed := result.MaxHeapSize;
  result.CurrHeapSize := mm^.Medium.CurrentBytes + mm^.Large.CurrentBytes;
  result.CurrHeapUsed := result.CurrHeapSize;
  result.CurrHeapFree := 0;
end;

function _GetHeapInfo: Utf8String;
begin
  // RetrieveMemoryManagerInfo from mormot.core.log expects RawUtf8 as result
  result := GetHeapStatus(' - fpcx64mm: ', 16, 16, {flags=}true, {sameline=}true);
end;

function _GetHeapStatus: THeapStatus;
begin
  // use this deprecated 32-bit structure to return hidden information
  FillChar(result{%H-}, sizeof(result), 0);
  PShortString(@result.TotalAddrSpace)^ := 'fpcx64mm'; // magic
  PPointer(@result.Unused)^ := @_GetHeapInfo;
end;

type
  // match both TSmallBlockStatus and TSmallBlockContention
  TRes = array[0..2] of PtrUInt;
  // details are allocated on the stack, not the heap
  TResArray = array[0..(NumSmallInfoBlock * 2) - 1] of TRes;

procedure QuickSortRes(var Res: TResArray; L, R, Level: PtrInt);
var
  I, J, P: PtrInt;
  pivot: PtrUInt;
  tmp: TRes;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := Res[P, Level]; // Level is 0..2
        while Res[I, Level] > pivot do
          inc(I);
        while Res[J, Level] < pivot do
          dec(J);
        if I <= J then
        begin
          tmp := Res[J];
          Res[J] := Res[I];
          Res[I] := tmp;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortRes(Res, L, J, Level);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortRes(Res, I, R, Level);
        R := J;
      end;
    until L >= R;
end;

procedure SetSmallBlockStatus(var res: TResArray; out small, tiny: cardinal);
var
  i, a: integer;
  p: PSmallBlockType;
  d: ^TSmallBlockStatus;
begin
  small := 0;
  tiny := 0;
  d := @res;
  p := @SmallBlockInfo;
  // gather TSmallBlockInfo.Small[] info
  for i := 1 to NumSmallBlockTypes do
  begin
    inc(small, ord(p^.GetmemCount <> 0));
    d^.Total := p^.GetmemCount;
    d^.Current := p^.GetmemCount - p^.FreememCount;
    d^.BlockSize := p^.BlockSize;
    inc(d);
    inc(p);
  end;
  // gather TSmallBlockInfo.Tiny[] info
  for a := 1 to NumTinyBlockArenas do
  begin
    d := @res; // aggregate counters
    for i := 1 to NumTinyBlockTypes do
    begin
      inc(tiny, ord(p^.GetmemCount <> 0));
      inc(d^.Total, p^.GetmemCount);
      inc(d^.Current, p^.GetmemCount - p^.FreememCount);
      inc(d);
      inc(p);
    end;
  end;
  assert(p = @SmallBlockInfo.GetmemLookup);
end;

function SortSmallBlockStatus(var res: TResArray; maxcount, orderby: PtrInt;
  count, bytes: PPtrUInt): PtrInt;
var
  i: PtrInt;
begin
  QuickSortRes(res, 0, NumSmallBlockTypes - 1, orderby);
  if count <> nil then
  begin
    count^ := 0;
    for i := 0 to NumSmallBlockTypes - 1 do
      inc(count^, res[i, orderby]);
  end;
  if bytes <> nil then
  begin
    bytes^ := 0;
    for i := 0 to NumSmallBlockTypes - 1 do
      inc(bytes^, res[i, orderby] * res[i, ord(obBlockSize)]);
  end;
  result := maxcount;
  if result > NumSmallBlockTypes then
    result := NumSmallBlockTypes;
  while (result > 0) and
        (res[result - 1, orderby] = 0) do
    dec(result);
end;

function SetSmallBlockContention(var res: TResArray; maxcount: integer): integer;
var
  i: integer;
  siz: cardinal;
  p: PCardinal;
  d: ^TSmallBlockContention;
begin
  result := 0;
  d := @res;
  p := @SmallBlockInfo.GetmemSleepCount;
  siz := 0;
  for i := 1 to length(SmallBlockInfo.GetmemSleepCount) do
  begin
    inc(siz, SmallBlockGranularity);
    if p^ <> 0 then
    begin
      d^.GetmemSleepCount := p^;
      d^.GetmemBlockSize := siz;
      d^.Reserved := 0;
      inc(d);
      inc(result);
    end;
    inc(p);
  end;
  if result = 0 then
    exit;
  QuickSortRes(res, 0, result - 1, 0); // sort by Level=0=GetmemSleepCount
  if result > maxcount then
    result := maxcount;
end;

var // use a pre-allocated buffer to avoid any heap usage during status output
  WrStrBuf: array[0 .. 1023] of AnsiChar; // typically less than 600 bytes
  WrStrPos: PtrInt;
  WrStrOnSameLine: boolean;

procedure W(const txt: ShortString);
var
  p, n: PtrInt;
begin
  n := ord(txt[0]);
  if n = 0 then
    exit;
  p := WrStrPos;
  inc(n, p);
  if n >= high(WrStrBuf) then
    exit; // paranoid
  Move(txt[1], WrStrBuf[p], ord(txt[0]));
  WrStrPos := n;
end;

const
  K_: array[0..4] of string[1] = (
    'P', 'T', 'G', 'M', 'K');

procedure K(const txt: ShortString; i: PtrUInt);
var
  j, n: PtrUInt;
  kk: PShortString;
  tmp: ShortString;
begin
  W(txt);
  kk := nil;
  n := 1 shl 50;
  for j := 0 to high(K_) do
    if i >= n then
    begin
      i := i div n;
      kk := @K_[j];
      break;
    end
    else
      n := n shr 10;
  str(i, tmp);
  W(tmp);
  if kk <> nil then
    W(kk^);
end;

procedure S(const txt: ShortString; i: PtrUInt);
var
  tmp: ShortString;
begin
  W(txt);
  str(i, tmp);
  W(tmp);
end;

procedure LF(const txt: ShortString = '');
begin
  if txt[0] <> #0 then
    W(txt);
  if WrStrOnSameLine then
    W(' ')
  else
    W({$ifdef OSWINDOWS} #13#10 {$else} #10 {$endif});
end;

procedure WriteHeapStatusDetail(const arena: TMMStatusArena;
  const name: ShortString);
begin
  K(name, arena.CurrentBytes);
  K('B/', arena.CumulativeBytes);
  W('B ');
  {$ifdef FPCMM_DEBUG}
  K('   peak=', arena.PeakBytes);
  K('B current=', arena.CumulativeAlloc - arena.CumulativeFree);
  K(' alloc=', arena.CumulativeAlloc);
  K(' free=', arena.CumulativeFree);
  {$endif FPCMM_DEBUG}
  K(' sleep=', arena.SleepCount);
  LF;
end;

function GetHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags, onsameline: boolean): PAnsiChar;
var
  res: TResArray; // no heap allocation involved
  i, n: PtrInt;
  t, b: PtrUInt;
  small, tiny: cardinal;
begin
  WrStrOnSameLine := onsameline;
  WrStrPos := 0;
  if context[0] <> #0 then
    LF(context);
  if compilationflags then
    LF(' Flags:' + FPCMM_FLAGS);
  with CurrentHeapStatus do
  begin
    K(' Small:  ', SmallBlocks);
    K('/', SmallBlocksSize);
    K('B  including tiny<=', SmallBlockSizes[NumTinyBlockTypes - 1]);
    S('B arenas=', NumTinyBlockArenas + 1);
    {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
    {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
    S(' pools=', length(SmallMediumBlockInfo));
    {$else}
    W(' fed from its own pool');
    {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
    {$else}
    W(' fed from Medium');
    {$endif FPCMM_SMALLNOTWITHMEDIUM}
    LF;
    WriteHeapStatusDetail(Medium, ' Medium: ');
    WriteHeapStatusDetail(Large,  ' Large:  ');
    if SleepCount <> 0 then
    begin
      K(' Total Sleep: count=', SleepCount);
      {$ifdef FPCMM_SLEEPTSC} K(' rdtsc=', SleepCycles); {$endif}
      LF;
    end;
    if SmallGetmemSleepCount <> 0 then
    begin
      K(' Small GetMem Sleep: count=', SmallGetmemSleepCount);
      LF;
    end;
  end;
  if (smallblockcontentioncount > 0) and
     (CurrentHeapStatus.SmallGetmemSleepCount <> 0) then
  begin
    n := SetSmallBlockContention(res, smallblockcontentioncount);
    for i := 0 to n - 1 do
      with TSmallBlockContention(res[i]) do
      begin
        S(' ', GetmemBlockSize);
        K('=' , GetmemSleepCount);
        if (i and 7 = 7) or
           (i = n - 1) then
          LF;
      end;
  end;
  if smallblockstatuscount > 0 then
  begin
    SetSmallBlockStatus(res, small, tiny);
    n := SortSmallBlockStatus(res, smallblockstatuscount, ord(obTotal), @t, @b) - 1;
    K(' Small Blocks since beginning: ', t);
    K('/', b);
    K('B (as small=', small);
    S('/', NumSmallBlockTypes);
    K(' tiny=', tiny);
    S('/', NumTinyBlockArenas * NumTinyBlockTypes);
    LF(')');
    for i := 0 to n do
      with TSmallBlockStatus(res[i]) do
      begin
        S('  ', BlockSize);
        K('=', Total);
        if (i and 7 = 7) or
           (i = n) then
          LF;
      end;
    n := SortSmallBlockStatus(res, smallblockstatuscount, ord(obCurrent), @t, @b) - 1;
    K(' Small Blocks current: ', t);
    K('/', b);
    LF('B');
    for i := 0 to n do
      with TSmallBlockStatus(res[i]) do
      begin
        S('  ', BlockSize);
        K('=', Current);
        if (i and 7 = 7) or
           (i = n) then
          LF;
      end;
  end;
  LF;
  WrStrBuf[WrStrPos] := #0; // makes PAnsiChar
  result := @WrStrBuf;
end;

procedure WriteHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags: boolean);
begin
  GetHeapStatus(context,  smallblockstatuscount, smallblockcontentioncount,
    compilationflags, {onsameline=}false);
  {$ifdef MSWINDOWS} // write all text at once
  {$I-}
  write(PAnsiChar(@WrStrBuf));
  ioresult;
  {$I+}
  {$else}
  fpwrite(StdOutputHandle, @WrStrBuf, WrStrPos); // POSIX
  {$endif MSWINDOWS}
end;

function GetSmallBlockStatus(maxcount: integer; orderby: TSmallBlockOrderBy;
  count, bytes: PPtrUInt; small, tiny: PCardinal): TSmallBlockStatusDynArray;
var
  res: TResArray;
  sm, ti: cardinal;
begin
  assert(SizeOf(TRes) = SizeOf(TSmallBlockStatus));
  result := nil;
  if maxcount <= 0 then
    exit;
  SetSmallBlockStatus(res, sm, ti);
  if small <> nil then
    small^ := sm;
  if tiny <> nil then
    tiny^ := ti;
  maxcount := SortSmallBlockStatus(res, maxcount, ord(orderby), count, bytes);
  if maxcount = 0 then
    exit;
  SetLength(result, maxcount);
  Move(res[0], result[0], maxcount * SizeOf(res[0]));
end;

function GetSmallBlockContention(maxcount: integer): TSmallBlockContentionDynArray;
var
  n: integer;
  res: TResArray;
begin
  result := nil;
  if maxcount <= 0 then
    exit;
  n := SetSmallBlockContention(res, maxcount);
  if n = 0 then
    exit;
  SetLength(result, n);
  Move(res[0], result[0], n * SizeOf(res[0]));
end;

{$endif FPCMM_STANDALONE}

function CurrentHeapStatus: TMMStatus;
var
  i: PtrInt;
  small: PtrUInt;
  p: PSmallBlockType;
begin
  result := HeapStatus;
  small := 0;
  for i := 0 to high(SmallBlockInfo.GetmemSleepCount) do
    inc(small, SmallBlockInfo.GetmemSleepCount[i]);
  result.SmallGetmemSleepCount := small;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    small := p^.GetmemCount - p^.FreememCount;
    if small <> 0 then
    begin
      inc(result.SmallBlocks, small);
      inc(result.SmallBlocksSize, small * p^.BlockSize);
    end;
    inc(p);
  end;
end;


{ ********* Initialization and Finalization }

procedure InitializeMediumPool(var Info: TMediumBlockInfo);
var
  i: PtrInt;
  medium: PMediumFreeBlock;
begin
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  Info.IsMultiThreadPtr := @IsMultiThread;
  {$endif FPCMM_ASSUMEMULTITHREAD}
  Info.PoolsCircularList.PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
  Info.PoolsCircularList.NextMediumBlockPoolHeader := @Info.PoolsCircularList;
  for i := 0 to MediumBlockBinCount - 1 do
  begin
    medium := @Info.Bins[i];
    medium.PreviousFreeBlock := medium;
    medium.NextFreeBlock := medium;
  end;
  {$ifdef FPCMM_MEDIUMPREFETCH}
  Info.Prefetch := OsAllocMedium(MediumBlockPoolSizeMem);
  {$endif FPCMM_MEDIUMPREFETCH}
end;

procedure InitializeMemoryManager;
var
  small: PSmallBlockType;
  a, i, min, poolsize, num, perpool, size, start, next: PtrInt;
begin
  {$ifdef MSWINDOWS}
  // check once if we could use VirtualAlloc2()
  VirtualAlloc2 := GetProcAddress(GetModuleHandleW('KernelBase'), 'VirtualAlloc2');
  {$endif MSWINDOWS}
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  MediumBlockInfoLookup[0] := @MediumBlockInfo;
  for i := 1 to high(MediumBlockInfoLookup) do
    MediumBlockInfoLookup[i] := @MediumBlockInfoExtra[i];
  {$endif FPCMM_MEDIUMPERTHREAD}
  InitializeMediumPool(MediumBlockInfo);
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  for i := 1 to high(MediumBlockInfoExtra) do
    InitializeMediumPool(MediumBlockInfoExtra[i]);
  {$endif FPCMM_MEDIUMPERTHREAD}
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
  for i := 0 to high(SmallMediumBlockInfo) do
    InitializeMediumPool(SmallMediumBlockInfo[i]);
  {$endif FPCMM_SMALLNOTWITHMEDIUM}
  SmallBlockInfo.IsMultiThreadPtr := @IsMultiThread; // call GOT if needed
  small := @SmallBlockInfo;
  assert(SizeOf(small^) = 1 shl SmallBlockTypePO2);  // exactly 64 bytes
  assert(length(SmallBlockInfo.GetmemSleepCount) =
    length(SmallBlockInfo.GetmemLookup));
  for a := 0 to NumTinyBlockArenas do
    for i := 0 to NumSmallBlockTypes - 1 do
    begin
      if (i = NumTinyBlockTypes) and
         (a > 0) then
        break;
      size := SmallBlockSizes[i];
      assert(size and 15 = 0);
      small^.BlockSize := size;
      small^.PreviousPartiallyFreePool := pointer(small);
      small^.NextPartiallyFreePool := pointer(small);
      small^.MaxSequentialFeedBlockAddress := pointer(0);
      small^.NextSequentialFeedBlockAddress := pointer(1);
      min := ((size * MinimumSmallBlocksPerPool +
         (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
         and -MediumBlockGranularity) + MediumBlockSizeOffset;
      if min < MinimumMediumBlockSize then
        min := MinimumMediumBlockSize;
      num := (min + (- MinimumMediumBlockSize +
        MediumBlockBinsPerGroup * MediumBlockGranularity div 2)) div
        (MediumBlockBinsPerGroup * MediumBlockGranularity);
      if num > 7 then
        num := 7;
      small^.AllowedGroupsForBlockPoolBitmap := byte(byte(-1) shl num);
      small^.MinimumBlockPoolSize := MinimumMediumBlockSize +
        num * (MediumBlockBinsPerGroup * MediumBlockGranularity);
      poolsize := ((size * TargetSmallBlocksPerPool +
        (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
        and -MediumBlockGranularity) + MediumBlockSizeOffset;
      if poolsize < OptimalSmallBlockPoolSizeLowerLimit then
        poolsize := OptimalSmallBlockPoolSizeLowerLimit;
      if poolsize > OptimalSmallBlockPoolSizeUpperLimit then
        poolsize := OptimalSmallBlockPoolSizeUpperLimit;
      perpool := (poolsize - SmallBlockPoolHeaderSize) div size;
      small^.OptimalBlockPoolSize := ((perpool * size +
         (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
          and -MediumBlockGranularity) + MediumBlockSizeOffset;
      inc(small);
    end;
  assert(small = @SmallBlockInfo.GetmemLookup);
  start := 0;
  with SmallBlockInfo do
    for i := 0 to NumSmallBlockTypes - 1 do
    begin
      next := PtrUInt(SmallBlockSizes[i]) div SmallBlockGranularity;
      while start < next do
      begin
        GetmemLookup[start] := i;
        inc(start);
      end;
    end;
  {$ifdef FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
  num := 0;
  for i := 0 to high(SmallBlockInfo.SmallMediumBlockInfo) do
  begin
    SmallBlockInfo.SmallMediumBlockInfo[i] := @SmallMediumBlockInfo[num];
    if num = high(SmallMediumBlockInfo) then
      num := 0
    else if i = NumSmallBlockTypes - 1 then
      dec(num) // last Small[] slot is unused: skip for better distribution
    else
      inc(num);
    //if SmallBlockInfo.Small[i].BlockSize = 16 then write(num:4);
  end;
  {$endif FPCMM_MULTIPLESMALLNOTWITHMEDIUM}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{$I-} // no console output error check in write/writeln below

{$ifdef FPCMM_REPORTMEMORYLEAKS}

{$define FPCMM_FULLCLEANUP} // leaks are tracked in FreeAllMemory

var
  MemoryLeakReported: boolean;

procedure StartReport;
begin
  if MemoryLeakReported then
    exit;
  writeln {$ifndef MSWINDOWS} (#27'[1;31m') {$endif}; // lightred posix console
  WriteHeapStatus('WARNING! THIS PROGRAM LEAKS MEMORY!'#13#10'Memory Status:');
  writeln('Leaks Identified:' {$ifndef MSWINDOWS} + #27'[1;37m' {$endif});
  MemoryLeakReported := true;
end;

{$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
var
  ObjectLeaksCount, ObjectLeaksRaiseCount: integer;

{$ifdef MSWINDOWS}
  LastMemInfo: TMemInfo; // simple cache

function SeemsRealPointer(p: pointer): boolean;
var
  meminfo: TMemInfo;
begin
  result := false;
  if PtrUInt(p) <= 65535 then
    exit; // first 64KB is not a valid pointer by definition
  if (LastMemInfo.State <> 0) and
     (PtrUInt(p) - LastMemInfo.BaseAddress < LastMemInfo.RegionSize) then
    result := true // quick check against last valid memory region
  else
  begin
    // VirtualQuery API is slow but better than raising an exception
    // see https://stackoverflow.com/a/37547837/458259
    FillChar(meminfo, SizeOf(meminfo), 0);
    result := (VirtualQuery(p, @meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
              (meminfo.State = MEM_COMMIT) and
              (PtrUInt(p) - meminfo.BaseAddress < meminfo.RegionSize) and
              (meminfo.Protect and PAGE_VALID <> 0) and
              (meminfo.Protect and PAGE_GUARD = 0);
    if result then
      LastMemInfo := meminfo;
  end;
end;
{$else}
function SeemsRealPointer(p: pointer): boolean;
begin
  // let the GPF happen silently in the kernel
  result := (PtrUInt(p) > 65535) and
            (fpaccess(p, F_OK) <> 0) and
            (fpgeterrno <> ESysEFAULT);
end;
{$endif MSWINDOWS}

{$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}

function SeemsClassName(P: PAnsiChar): boolean;
var
  l: PtrInt;
begin
  result := false;
  l := ord(P[0]);
  if (l = 0) or
     not (P[1] in ['A' .. 'z']) then
    exit;
  repeat
    if P[l] <= ' ' then
      exit;
    dec(l);
  until l = 0;
  result := true;
end;

procedure MediumMemoryLeakReport(
  var Info: TMediumBlockInfo; p: PMediumBlockPoolHeader);
var
  block: PByte;
  header, size: PtrUInt;
  {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  first, last: PByte;
  vmt: PAnsiChar;
  instancesize, blocksize: PtrInt;
  classname: PShortString;
  {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
begin
  if (Info.SequentialFeedBytesLeft = 0) or
     (PtrUInt(Info.LastSequentiallyFed) < PtrUInt(p)) or
     (PtrUInt(Info.LastSequentiallyFed) > PtrUInt(p) + MediumBlockPoolSize) then
    block := Pointer(PByte(p) + MediumBlockPoolHeaderSize)
  else if Info.SequentialFeedBytesLeft <>
            MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      block := Info.LastSequentiallyFed
    else
      exit;
  repeat
    header := PPtrUInt(block - BlockHeaderSize)^;
    size := header and DropMediumAndLargeFlagsMask;
    if size = 0 then
      exit;
    if header and IsFreeBlockFlag = 0 then
      if header and IsSmallBlockPoolInUseFlag <> 0 then
      begin
        {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
        if PSmallBlockPoolHeader(block).BlocksInUse > 0 then // some leaks
        begin
          blocksize := PSmallBlockPoolHeader(block).BlockType.BlockSize;
          first := PByte(block) + SmallBlockPoolHeaderSize;
          with PSmallBlockPoolHeader(block).BlockType^ do
            if (CurrentSequentialFeedPool <> pointer(block)) or
               (PtrUInt(NextSequentialFeedBlockAddress) >
                PtrUInt(MaxSequentialFeedBlockAddress)) then
              last := PByte(block) + (PPtrUInt(PByte(block) - BlockHeaderSize)^
                and DropMediumAndLargeFlagsMask) - BlockSize
            else
              last := Pointer(PByte(NextSequentialFeedBlockAddress) - 1);
          while (first <= last) and
                (ObjectLeaksRaiseCount < 64) do
          begin
            if ((PPtrUInt(first - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
            begin
              vmt := PPointer(first)^; // _FreeMem() ensured vmt=nil/$b10dle55
              if (vmt <> nil) and
                 ((PtrUInt(vmt) and (SizeOf(Pointer) - 1)) = 0) and
                 {$ifdef FPCMM_REPORTMEMORYLEAKS}
                 (PtrUInt(vmt) <> REPORTMEMORYLEAK_FREEDHEXSPEAK) and
                 // FreeMem marked freed blocks with BLOODLESS hexspeak magic
                 {$endif FPCMM_REPORTMEMORYLEAKS}
                 SeemsRealPointer(vmt) then
              try
                // try to access the TObject VMT
                instancesize := PPtrInt(vmt + vmtInstanceSize)^;
                if (instancesize >= sizeof(vmt)) and
                   (instancesize <= blocksize) then
                begin
                  classname := PPointer(vmt + vmtClassName)^;
                  if SeemsRealPointer(classname) and
                     SeemsClassName(pointer(classname)) then
                  begin
                     StartReport;
                     writeln(' probable ', classname^, ' leak (', instancesize,
                       '/', blocksize, ' bytes) at $', HexStr(first));
                     inc(ObjectLeaksCount);
                  end;
                end;
              except
                // intercept and ignore any GPF - SeemsRealPointer() not enough
                inc(ObjectLeaksRaiseCount);
                {$ifdef MSWINDOWS}
                LastMemInfo.State := 0; // reset VirtualQuery() cache
                {$endif MSWINDOWS}
              end;
            end;
            inc(first, blocksize);
          end;
        end;
        {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
      end
      else
      begin
        StartReport;
        writeln(' medium block leak of ', size, ' bytes');
      end;
    inc(block, size);
  until false;
end;

{$else}

{$undef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}

{$endif FPCMM_REPORTMEMORYLEAKS}

procedure FreeMediumPool(var Info: TMediumBlockInfo);
var
  medium, nextmedium: PMediumBlockPoolHeader;
  bin: PMediumFreeBlock;
  i: PtrInt;
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  list, next: PPointer;
  {$endif FPCMM_REPORTMEMORYLEAKS}
begin
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  list := Info.LastFree;
  {$endif FPCMM_REPORTMEMORYLEAKS}
  Info.LastFree := nil;
  // All blocks in this list belong to pools released just below. A pending
  // small-block pool has the same flag value as a large block, so the generic
  // dispatcher would wrongly send it to FreeLargeBlock.
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  while list <> nil do
  begin
    next := list^;
    PPtrUInt(PByte(list) - BlockHeaderSize)^ :=
      PPtrUInt(PByte(list) - BlockHeaderSize)^ or IsFreeBlockFlag;
    list := next;
  end;
  {$endif FPCMM_REPORTMEMORYLEAKS}
  medium := Info.PoolsCircularList.NextMediumBlockPoolHeader;
  while medium <> @Info.PoolsCircularList do
  begin
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    MediumMemoryLeakReport(Info, medium);
    {$endif FPCMM_REPORTMEMORYLEAKS}
    nextmedium := medium.NextMediumBlockPoolHeader;
    FreeMedium(medium, Info);
    medium := nextmedium;
  end;
  Info.PoolsCircularList.PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
  Info.PoolsCircularList.NextMediumBlockPoolHeader := @Info.PoolsCircularList;
  for i := 0 to MediumBlockBinCount - 1 do
  begin
    bin := @Info.Bins[i];
    bin.PreviousFreeBlock := bin;
    bin.NextFreeBlock := bin;
  end;
  Info.BinGroupBitmap := 0;
  Info.SequentialFeedBytesLeft := 0;
  for i := 0 to MediumBlockBinGroupCount - 1 do
    Info.BinBitmaps[i] := 0;
  {$ifdef FPCMM_MEDIUMPREFETCH}
  if Info.Prefetch <> nil then
    OsFreeMedium(Info.Prefetch, MediumBlockPoolSizeMem);
  {$endif FPCMM_MEDIUMPREFETCH}
end;

procedure FreeAllMemory;
var
  large, nextlarge: PLargeBlockHeader;
  p: PSmallBlockType;
  i, size: PtrUInt;
  list, next: PPointer;
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  leak, leaks: PtrUInt;
  {$endif FPCMM_REPORTMEMORYLEAKS}
begin
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  leaks := 0;
  {$endif FPCMM_REPORTMEMORYLEAKS}
  p := @SmallBlockInfo;
  for i := 0 to high(SmallBlockInfo.SmallLastFree) do
  begin
    list := SmallBlockInfo.SmallLastFree[i];
    SmallBlockInfo.SmallLastFree[i] := nil;
    p^.LastFreeCount := 0;
    while list <> nil do
    begin
      next := list^;
      _FreeMem(list); // not a leak, just an unexpected context
      list := next;
    end;
    inc(p);
  end;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    p^.PreviousPartiallyFreePool := pointer(p);
    p^.NextPartiallyFreePool := pointer(p);
    p^.NextSequentialFeedBlockAddress := pointer(1);
    p^.MaxSequentialFeedBlockAddress := nil;
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    leak := p^.GetmemCount - p^.FreememCount;
    if leak <> 0 then
    begin
      StartReport;
      inc(leaks, leak);
      writeln(' small block leak x', leak, ' of size=', p^.BlockSize,
        '  (GetMem=', p^.GetmemCount, ' FreeMem=', p^.FreememCount, ')');
    end;
    {$endif FPCMM_REPORTMEMORYLEAKS}
    inc(p);
  end;
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  if leaks <> 0 then
    writeln(' Total small block leaks = ', leaks);
  {$endif FPCMM_REPORTMEMORYLEAKS}
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
  for i := 0 to high(SmallMediumBlockInfo) do
    FreeMediumPool(SmallMediumBlockInfo[i]);
  {$endif FPCMM_SMALLNOTWITHMEDIUM}
  {$ifdef FPCMM_MEDIUMPERTHREAD}
  for i := 1 to high(MediumBlockInfoExtra) do
    FreeMediumPool(MediumBlockInfoExtra[i]);
  {$endif FPCMM_MEDIUMPERTHREAD}
  FreeMediumPool(MediumBlockInfo);
  {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  if ObjectLeaksCount <> 0 then
    writeln(' Total objects leaks = ', ObjectLeaksCount);
  {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  large := LargeBlocksCircularList.NextLargeBlockHeader;
  while large <> @LargeBlocksCircularList do
  begin
    size := large.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    StartReport;
    writeln(' large block leak of ', size, ' bytes');
    {$endif FPCMM_REPORTMEMORYLEAKS}
    nextlarge := large.NextLargeBlockHeader;
    FreeLarge(large, size);
    large := nextlarge;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{$I+}

{$ifndef FPCMM_STANDALONE}

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
  InitializeMemoryManager;
  GetMemoryManager(OldMM);
  SetMemoryManager(NewMM);

finalization
  {$ifdef FPCMM_FULLCLEANUP}
  SetMemoryManager(OldMM);
  FreeAllMemory;
  {$endif FPCMM_FULLCLEANUP}

{$endif FPCMM_STANDALONE}

{$endif FPCX64MM_AVAILABLE}

end.

