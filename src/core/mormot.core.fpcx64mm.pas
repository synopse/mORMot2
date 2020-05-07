/// Fast Memory Manager for FPC x86_64
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fpcx64mm;

{
  *****************************************************************************

    A Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
    - based on FastMM4 proven algorithms by Pierre le Riche
    - targetting Windows and Linux multi-threaded Services
    - only for FPC on the x86_64 target - use the original heap on Delphi or ARM
    - code has been reduced to the only necessary featureset for production
    - huge asm refactoring for cross-platform, compactness and efficiency
    - detailed statistics gathering (also about leaks and threads contention)
    - mremap() makes large block ReallocMem a breeze on Linux :)

    Usage: include this unit as the very first in your FPC project uses clause

    Why another Memory Manager on FPC?
    - The built-in heap.inc is well written and cross-platform and cross-CPU,
      but its threadvar arena for small blocks tends to consume a lot of memory
      on multi-threaded servers, and has suboptimal allocation performance
    - C memory managers (glibc, Intel TBB, jemalloc) have a very high RAM
      consumption (especially Intel TBB) and panic/SIGKILL on any GPF
    - Pascal alternatives (FastMM4,ScaleMM2,BrainMM) are Windows+Delphi specific
    - It was so fun deeping into SSE2 x86_64 assembly and Pierre's insight
    - Resulting code is still easy to understand and maintain

    IMPORTANT NOTICE: only tested on-site - feedback is (very) welcome!

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}


{$ifdef FPC_CPUX64}

// if set, will report any potential memory leak at shutdown
// - then use FPC heaptrc or valgrid for further investigation
// - additional fine-grained counters and sleep timing will also be
// included to CurrentHeapStatus/WriteHeapStatus
{$define FPCMM_DEBUG}


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
    ThreadContention: PtrInt;
  end;

  /// heap information as returned by CurrentHeapStatus
  TMMStatus = record
    /// contain blocks up to 256KB (small and medium blocks)
    Medium: TMMStatusArena;
    /// large blocks > 256KB which are directly handled by the Operating System
    Large: TMMStatusArena;
    {$ifdef FPCMM_DEBUG}
    /// how many times Getmem() did block for a small block
    // - see also GetSmallBlockContention()
    SmallGetmemThreadContention: PtrInt;
    /// how many times Freemem() did block for a small block
    // - see also GetSmallBlockContention()
    SmallFreememThreadContention: PtrInt;
    {$endif FPCMM_DEBUG}
  end;
  PMMStatus = ^TMMStatus;

  /// one GetSmallBlockContention item thread contention report
  // - a single GetmemThreadContentionSmallBlockSize or
  // FreememThreadContentionSmallBlockSize non 0 field is set
  TFPCMMSmallBlockContention = packed record
    /// the small block size on which Getmem() has been blocked - or 0
    GetmemThreadContentionSmallBlockSize: word;
    /// the small block size on which Freemem() has been blocked - or 0
    FreememThreadContentionSmallBlockSize: word;
    /// how many times a small block access has been waiting for unlock
    ThreadContention: cardinal;
  end;

  /// small blocks detailed information as returned GetSmallBlockContention
  TFPCMMSmallBlockContentionDynArray = array of TFPCMMSmallBlockContention;


/// retrieve high-level statistics about the current memory manager state
// - see also GetSmallBlockContention for detailed small blocks information
function CurrentHeapStatus: TMMStatus;

/// get all small blocks which suffered from blocking during multi-thread
// - results are sorted by contention occurence
function GetSmallBlockContention(GetMemTotal: PPtrInt = nil;
  FreeMemTotal: PPtrInt = nil): TFPCMMSmallBlockContentionDynArray;

/// convenient debugging function into the console
procedure WriteHeapStatus(const context: shortstring = '';
  includesmallblockcontention: integer = 0);

{$endif FPC_CPUX64}



implementation


{$ifdef FPC_CPUX64}

{ ********* Operating System Specific API Calls }

{$ifdef MSWINDOWS}

const
  kernel32 = 'kernel32.dll';

  MEM_COMMIT = $1000;
  MEM_RESERVE  = $2000;
  MEM_RELEASE = $8000;
  MEM_FREE = $10000;
  MEM_TOP_DOWN = $100000;

  PAGE_READWRITE = 4;

function VirtualAlloc(lpAddress: Pointer;
   dwSize: PtrUInt; flAllocationType, flProtect: Cardinal): Pointer; stdcall;
  external kernel32 name 'VirtualAlloc';
function VirtualFree(lpAddress: Pointer; dwSize: PtrUInt;
   dwFreeType: Cardinal): LongBool; stdcall;
  external kernel32 name 'VirtualFree';
procedure SwitchToThread; stdcall;
  external kernel32 name 'SwitchToThread';
procedure SleepMS(dwMilliseconds: Cardinal); stdcall;
  external kernel32 name 'Sleep';

procedure SleepInitial;
begin
  SwitchToThread;
end;

procedure SleepNext;
begin
  SleepMS(1);
end;

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
end;

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer;
begin
  Error(reOutOfMemory); // not available on this platform
  result := nil;
end;

procedure Free(ptr: pointer; Size: PtrInt); inline;
begin
  VirtualFree(ptr, 0, MEM_RELEASE);
end;

{$else}

uses
  BaseUnix;

const
  clib = 'c';

function mmap(addr: pointer; len: size_t; prot: longint; flags: longint;
   fd:longint; offset: PtrInt): pointer; cdecl;
  external clib name 'mmap';
function munmap(addr: pointer; len: size_t): longint; cdecl;
  external clib name 'munmap';
function nanosleep(requested_time, remaining: Ptimespec): longint; cdecl;
  external clib name 'nanosleep';

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  result := mmap(nil, Size, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  result := mmap(nil, Size, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

procedure Free(ptr: pointer; Size: PtrInt); inline;
begin
  Size := munmap(ptr, Size);
  //assert(Size = 0);
end;

{$ifdef LINUXNOTBSD}

function mremap(addr: pointer; old_len, new_len: size_t; may_move: longint): pointer; cdecl;
  external clib name 'mremap';

const
  MREMAP_MAYMOVE = 1;
  CLOCK_MONOTONIC = 1;

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer; inline;
begin
  result := mremap(ptr, OldSize, NewSize, MREMAP_MAYMOVE);
  //assert(result <> pointer(-1));
end;

{$else}

const
  {$ifdef OPENBSD}
  CLOCK_MONOTONIC = 3;
  {$else}
  CLOCK_MONOTONIC = 4;
  {$endif OPENBSD}

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer;
begin
  Error(reOutOfMemory); // not available on this platform
  result := nil;
end;

{$endif LINUXNOTBSD}

procedure NSleep(nsec: PtrInt);
var
  t: Ttimespec;
begin
  t.tv_sec := 0;
  t.tv_nsec := nsec;
  nanosleep(@t, nil);
end;

{$ifdef FPCMM_DEBUG}

{$ifdef DARWIN}

function QueryPerformanceMicroSeconds: Int64; inline;
begin
  result := 0;
end;

{$else}

function clock_gettime(clk_id: integer; tp: ptimespec): integer; cdecl;
  external clib name 'clock_gettime';

function QueryPerformanceMicroSeconds: PtrUInt; inline;
var
  r : TTimeSpec;
begin
  if clock_gettime(CLOCK_MONOTONIC, @r) = 0 then
    result := PtrUInt(r.tv_nsec) div 1000 + PtrUInt(r.tv_sec) * 1000000
  else
    result := 0;
end;

{$endif DARWIN}

var
  SleepingCount, SleepingTime: PtrUInt;

procedure SleepSetTime(start: QWord); nostackframe; assembler;
asm
  push start
  call QueryPerformanceMicroSeconds
  pop rcx
  sub rax, rcx
lock xadd qword ptr[rip + SleepingTime], rax
lock inc qword ptr[rip + SleepingCount]
end;


procedure SleepInitial;
var
  start: QWord;
begin
  start := QueryPerformanceMicroSeconds;
  NSleep(10000); // 10us is around timer resolution on modern HW
  SleepSetTime(start);
end;

procedure SleepNext;
var
  start: QWord;
begin
  start := QueryPerformanceMicroSeconds;
  NSleep(50000); // 50us was empirically measured
  SleepSetTime(start);
end;

{$else}

procedure SleepInitial;
begin
  NSleep(10000); // 10us is around timer resolution on modern HW
end;

procedure SleepNext;
begin
  NSleep(50000); // 50us was empirically measured
end;

{$endif FPCMM_DEBUG}

{$endif MSWINDOWS}


{ ********* Some Assembly Helpers }

procedure NotifyAlloc(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
     mov  rax, Size
lock xadd qword ptr[Arena].TMMStatusArena.CurrentBytes, rax
lock xadd qword ptr[Arena].TMMStatusArena.CumulativeBytes, Size
     {$ifdef FPCMM_DEBUG}
lock inc  qword ptr[Arena].TMMStatusArena.CumulativeAlloc
     mov  rax, qword ptr[Arena].TMMStatusArena.CurrentBytes
     cmp  rax, qword ptr[Arena].TMMStatusArena.PeakBytes
     jbe  @s
     mov  qword ptr[Arena].TMMStatusArena.PeakBytes, rax
@s:  {$endif FPCMM_DEBUG}
end;

procedure NotifyFree(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
     neg Size
lock xadd qword ptr[Arena].TMMStatusArena.CurrentBytes, Size
     {$ifdef FPCMM_DEBUG}
lock inc  qword ptr[Arena].TMMStatusArena.CumulativeFree
     {$endif FPCMM_DEBUG}
end;


// Move## are called from asm with src=rcx dst=rdx len=r8 even on POSIX ABI

{$WARN 7122 off : Check size of memory operand }

procedure Move8; nostackframe; assembler;
asm
      mov rax, [rcx]
      mov [rdx], rax
end;

procedure Move24; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      mov rax, [rcx + 16]
      movaps oword ptr [rdx], xmm0
      mov [rdx + 16], rax
end;

procedure Move40; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      mov rax, [rcx + 32]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      mov [rdx + 32], rax
end;

procedure Move56; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      mov rax, [rcx + 48]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      mov [rdx + 48], rax
end;

procedure MoveX16LP; nostackframe; assembler;
asm
      sub r8, 8
      add rcx, r8
      add rdx, r8
      neg r8
      jns @z
      align 16
@s:   movaps xmm0, oword ptr [rcx + r8]
      movaps oword ptr [rdx + r8], xmm0
      add r8, 16
      js @s
@z:   mov rax, qword ptr [rcx + r8]
      mov qword ptr [rdx + r8], rax
end;

// called from ReallocateLargeBlock with regular parameters

procedure MoveLarge(src, dst: pointer; cnt: PtrInt); nostackframe; assembler;
asm
      sub cnt, 8
      add src, cnt
      add dst, cnt
      neg cnt
      jns @z
      align 16
@s:   movaps xmm0, oword ptr [src + cnt]
      movntdq oword ptr [dst + cnt], xmm0 // non-temporal loop
      add cnt, 16
      js @s
      sfence
@z:   mov rax, qword ptr [src + cnt]
      mov qword ptr [dst + cnt], rax
end;



{ ********* Constants and Data Structures Definitions }

{$WARN 3175 off : Some fields coming before "$1" were not initialized }

const
  NumSmallBlockTypes = 46 + 9; // 9 smallest sizes are tripled to avoid spinning
  MaximumSmallBlockSize = 2608;
  SmallBlockSizes: array[0..NumSmallBlockTypes - 1] of word = (
   16, 16, 16, 32, 32, 32, 48, 48, 48, 64, 64, 64, 80, 80,
   96, 112, 128, 144, 160, 176, 192, 208, 224, 240,
   256, 272, 288, 304, 320, 352, 384, 416, 448, 480, 528, 576, 624, 672,
   736, 800, 880, 960, 1056, 1152, 1264, 1376, 1504, 1648, 1808, 1984,
   2176, 2384, MaximumSmallBlockSize, MaximumSmallBlockSize, MaximumSmallBlockSize);
  SmallBlockGranularity = 8;
  TargetSmallBlocksPerPool = 48;
  MinimumSmallBlocksPerPool = 12;

  MediumBlockPoolSizeMem = 20 * 64 * 1024;
  MediumBlockPoolSize = MediumBlockPoolSizeMem - 16;
  MediumBlockSizeOffset = 48;
  MinimumMediumBlockSize = 11 * 256 + MediumBlockSizeOffset;
  MediumBlockBinsPerGroup = 32;
  MediumBlockBinGroupCount = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  MediumBlockGranularity = 256;
  MaximumMediumBlockSize =
    MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  OptimalSmallBlockPoolSizeLowerLimit =
    29 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit =
    64 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  MaximumSmallBlockPoolSize =
    OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  LargeBlockGranularity = 65536;

  IsFreeBlockFlag = 1;
  IsMediumBlockFlag = 2;
  IsSmallBlockPoolInUseFlag = 4;
  IsLargeBlockFlag = 4;
  PreviousMediumBlockIsFreeFlag = 8;
  LargeBlockIsSegmented = 8;
  DropSmallFlagsMask = -8;
  ExtractSmallFlagsMask = 7;
  DropMediumAndLargeFlagsMask = -16;
  ExtractMediumAndLargeFlagsMask = 15;

  SpinLockCount = 5000;
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;
  MediumInPlaceDownsizeLimit = MinimumMediumBlockSize div 4;

type
  TMoveProc = procedure; // with rcx/rdx/r8 ABI convention

  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;
  TSmallBlockType = record
    BlockTypeLocked: Boolean;
    AllowedGroupsForBlockPoolBitmap: Byte;
    BlockSize: Word;
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    NextSequentialFeedBlockAddress: Pointer;
    MaxSequentialFeedBlockAddress: Pointer;
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
    UpsizeMoveProcedure: TMoveProc;
    {$ifdef CPU64}
    GetmemThreadContention: integer;
    FreememThreadContention: integer;
    {$endif}
  end;
  PSmallBlockType = ^TSmallBlockType;

  TSmallBlockPoolHeader = record
    BlockType: PSmallBlockType;
    {$ifdef CPU32}
    Padding32Bits: cardinal;
    {$endif}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    FirstFreeBlock: Pointer;
    BlocksInUse: Cardinal;
    SmallBlockPoolSignature: Cardinal;
    FirstBlockPoolPointerAndFlags: PtrUInt;
  end;

  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = record
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    Reserved1: PtrUInt;
    FirstMediumBlockSizeAndFlags: PtrUInt;
  end;

  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    UserAllocatedSize: PtrUInt;
    BlockSizeAndFlags: PtrUInt;
  end;

const
  BlockHeaderSize = SizeOf(Pointer);
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);

type
  TSmallBlockInfo = packed record
    Types: array[0..NumSmallBlockTypes - 1] of TSmallBlockType;
    AllocSizeIndX4: array[0..
      (MaximumSmallBlockSize - 1) div SmallBlockGranularity] of byte;
  end;

  TMediumBlockInfo = record
    Locked: Boolean;
    PoolsCircularList: TMediumBlockPoolHeader;
    LastSequentiallyFed: Pointer;
    SequentialFeedBytesLeft: Cardinal;
    BinGroupBitmap: Cardinal;
    BinBitmaps: array[0..MediumBlockBinGroupCount - 1] of Cardinal;
    Bins: array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
  end;

var
  SmallBlockInfo: TSmallBlockInfo;
  MediumBlockInfo: TMediumBlockInfo;

  LargeBlocksLocked: Boolean;
  LargeBlocksCircularList: TLargeBlockHeader;

  HeapStatus: TMMStatus;


{ ********* Shared Routines }

{ note: pause opcode latency was 10 cycles, but is 140 cycles since SkylakeX
        see http://tiny.cc/010ioz
  -> we use pause before SleepInitial API call when spinning locks }

procedure LockMediumBlocks; nostackframe; assembler;
asm
     mov  rdx, SpinLockCount
     lea  rcx, [rip + MediumBlockInfo.Locked]
@sp: pause
     mov  eax, $100
     dec  rdx
     jz   @sl
     cmp  [rcx], ah // don't flush the CPU cache if Locked still true
     je   @sp
lock cmpxchg byte ptr [rcx], ah
     je   @ok
     jmp  @sp
@sl: push rsi // preserve POSIX ABI registers
     push rdi
     call SleepInitial
@s:  mov  eax, $100
lock cmpxchg byte ptr [rip + MediumBlockInfo.Locked], ah
     je   @o
     call SleepNext
     jmp  @s
@o:  pop  rdi
     pop  rsi
     lea  r10, [rip + MediumBlockInfo] // as expected by caller asm
     lea  rax, [rip + HeapStatus] // simple inc within lock
     inc  qword ptr [rax].TMMStatus.Medium.ThreadContention
@ok:
end;

procedure InsertMediumBlockIntoBin; nostackframe; assembler;
asm
  {rcx=MediumFreeBlock edx=MediumBlockSize r10=MediumBlockInfo - even on POSIX}
  mov rax, rcx
  {Get the bin number for this block size}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  mov r9, rdx
  {Get the bin address in rcx}
  shl edx, 4
  lea rcx, [r10 + rdx + TMediumBlockInfo.Bins]
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov rdx, TMediumFreeBlock[rcx].NextFreeBlock
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rcx
  mov TMediumFreeBlock[rax].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rax
  mov TMediumFreeBlock[rcx].NextFreeBlock, rax
  {Was this bin empty?}
  cmp rdx, rcx
  jne @Done
  {Get the bin number in ecx}
  mov rcx, r9
  {Get the group number in edx}
  mov rdx, r9
  shr edx, 5
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  or dword ptr [r8 + rdx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure RemoveMediumFreeBlock; nostackframe; assembler;
asm
  {rcx=MediumFreeBlock r10=MediumBlockInfo - even on POSIX}
  {Get the current previous and next blocks}
  mov rdx, TMediumFreeBlock[rcx].PreviousFreeBlock
  mov rcx, TMediumFreeBlock[rcx].NextFreeBlock
  {Remove this block from the linked list}
  mov TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
  mov TMediumFreeBlock[rdx].NextFreeBlock, rcx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin}
  cmp rcx, rdx
  jne @Done
  {Get the bin number for this block size in rcx}
  lea r8, [r10 + TMediumBlockInfo.Bins]
  sub rcx, r8
  mov edx, ecx
  shr ecx, 4
  {Get the group number in edx}
  shr edx, 9
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  and dword ptr [r8 + rdx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure BinMediumSequentialFeedRemainder; nostackframe; assembler;
asm
  {r10=MediumBlockInfo - even on POSIX}
  mov eax, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  test eax, eax
  jz @Done
  {Get a pointer to the last sequentially allocated medium block}
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  {Is the block that was last fed sequentially free?}
  test byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  {Point eax to the start of the remainder}
  sub rax, rdx
@BinTheRemainder:
  {Status: rax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rax - BlockHeaderSize], rcx
  {Store the trailing size marker}
  mov [rax + rdx - 2 * BlockHeaderSize], rdx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jb @Done
  mov rcx, rax
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  jmp @Done
@LastBlockFedIsFree:
  {Drop the flags}
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  mov rcx, rax
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  {Re-read rax and rdx}
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  {Point rax to the start of the remainder}
  sub rax, rcx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
@Done:
end;

procedure FreeMedium(ptr: pointer);
begin
  Free(ptr, MediumBlockPoolSizeMem);
  NotifyFree(HeapStatus.Medium, MediumBlockPoolSizeMem);
end;

function AllocNewSequentialFeedMediumPool(blocksize: Cardinal): Pointer;
var
  old: PMediumBlockPoolHeader;
  new: Pointer;
begin
  BinMediumSequentialFeedRemainder;
  new := AllocMedium(MediumBlockPoolSizeMem);
  with MediumblockInfo do
  if new <> nil then
  begin
    old := PoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(new).PreviousMediumBlockPoolHeader := @PoolsCircularList;
   PoolsCircularList.NextMediumBlockPoolHeader := new;
    PMediumBlockPoolHeader(new).NextMediumBlockPoolHeader := old;
    old.PreviousMediumBlockPoolHeader := new;
    PPtrUInt(PByte(new) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    SequentialFeedBytesLeft :=
      (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - blocksize;
    result := Pointer(PByte(new) + MediumBlockPoolSize - blocksize);
    LastSequentiallyFed := result;
    PPtrUInt(PByte(result) - BlockHeaderSize)^ := blocksize or IsMediumBlockFlag;
    NotifyAlloc(HeapStatus.Medium, MediumBlockPoolSizeMem);
  end
  else
  begin
    SequentialFeedBytesLeft := 0;
    result := nil;
  end;
end;

procedure LockLargeBlocks; nostackframe; assembler;
asm
     mov  eax, $100
lock cmpxchg byte ptr [rip + LargeBlocksLocked], ah
     je   @none
     pause
     mov eax, $100
lock cmpxchg byte ptr [rip + LargeBlocksLocked], ah
     je   @ok
     call SleepInitial
@s:  mov  eax, $100
lock cmpxchg byte ptr [rip + LargeBlocksLocked], ah
     je   @ok
     call SleepNext
     jmp @s
@ok: lea rax, [rip + HeapStatus] // simple inc within lock
     inc qword ptr [rax].TMMStatus.Large.ThreadContention
@none:
end;

function AllocateLargeBlockFrom(size: PtrUInt;
   existing: pointer; oldsize: PtrUInt): Pointer;
var
  blocksize: PtrUInt;
  old: PLargeBlockHeader;
begin
  blocksize := (size + LargeBlockHeaderSize +
    LargeBlockGranularity - 1 + BlockHeaderSize) and -LargeBlockGranularity;
  if existing = nil then
    result := AllocLarge(blocksize)
  else
    result := ReallocLarge(existing, oldsize, blocksize);
  if result <> nil then
  begin
    NotifyAlloc(HeapStatus.Large, blocksize);
    if existing <> nil then
      NotifyFree(HeapStatus.Large, oldsize);
    PLargeBlockHeader(result).UserAllocatedSize := size;
    PLargeBlockHeader(result).BlockSizeAndFlags := blocksize or IsLargeBlockFlag;
    LockLargeBlocks;
    old := LargeBlocksCircularList.NextLargeBlockHeader;
    PLargeBlockHeader(result).PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := result;
    PLargeBlockHeader(result).NextLargeBlockHeader := old;
    old.PreviousLargeBlockHeader := result;
    LargeBlocksLocked := False;
    inc(PByte(result), LargeBlockHeaderSize);
  end;
end;

function AllocateLargeBlock(size: PtrUInt): Pointer;
begin
  result := AllocateLargeBlockFrom(size, nil, 0);
end;

function FreeLargeBlock(p: Pointer): Integer;
var
  prev, next: PLargeBlockHeader;
  size: PtrInt;
begin
  p := Pointer(PByte(p) - LargeBlockHeaderSize);
  // remove from current chain list
  LockLargeBlocks;
  prev := PLargeBlockHeader(p).PreviousLargeBlockHeader;
  next := PLargeBlockHeader(p).NextLargeBlockHeader;
  next.PreviousLargeBlockHeader := prev;
  prev.NextLargeBlockHeader := next;
  LargeBlocksLocked := False;
  // release memory to system
  size := DropMediumAndLargeFlagsMask and PLargeBlockHeader(p).BlockSizeAndFlags;
  NotifyFree(HeapStatus.Large, size);
  Free(p, size);
  result := 0; // assume success
end;

function _GetMem(size: PtrInt): Pointer; forward;
function _FreeMem(P: Pointer): PtrInt;   forward;

function ReallocateLargeBlock(p: Pointer; size: PtrUInt): Pointer;
var
  oldavail, olduser, minup, new: PtrUInt;
  header, prev, next: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  oldavail := (DropMediumAndLargeFlagsMask and header.BlockSizeAndFlags) -
    (LargeBlockHeaderSize + BlockHeaderSize);
  if size > oldavail then
  begin
    // size-up with 1/8 overhead for future increase
    minup := oldavail + (oldavail shr 3);
    if size < minup then
      new := minup
    else
      new := size;
    {$ifdef LINUXNOTBSD}
    // remove from current chain list
    LockLargeBlocks;
    prev := header.PreviousLargeBlockHeader;
    next := header.NextLargeBlockHeader;
    next.PreviousLargeBlockHeader := prev;
    prev.NextLargeBlockHeader := next;
    LargeBlocksLocked := False;
    // let the Linux Kernel mremap() the memory using its TLB magic
    size := DropMediumAndLargeFlagsMask and header.BlockSizeAndFlags;
    result := AllocateLargeBlockFrom(new, header, size);
    {$else}
    // no mremap(): reallocate a new block, copy the existing data, free old
    result := _GetMem(new);
    if result <> nil then
    begin
      if new > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(result) - LargeBlockHeaderSize).UserAllocatedSize := size;
      olduser := PLargeBlockHeader(PByte(p) - LargeBlockHeaderSize).UserAllocatedSize;
      MoveLarge(p, result, olduser);
      _FreeMem(p);
    end;
    {$endif LINUXNOTBSD}
  end
  else
  // size-down, or small size-up within current buffer
  if size >= (oldavail shr 1) then
  begin
    result := p;
    PLargeBlockHeader(PByte(p) - LargeBlockHeaderSize).UserAllocatedSize := size;
  end
  else
  begin
    result := _GetMem(size);
    if result <> nil then
    begin
      if size > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(p) - LargeBlockHeaderSize).UserAllocatedSize := size;
      MoveLarge(p, result, size);
      _FreeMem(p);
    end;
  end;
end;


{ ********* Main Memory Manager Functions }

function _GetMem(size: PtrInt): Pointer; nostackframe; assembler;
asm
  {$ifndef MSWINDOWS}
  mov rcx, size
  {$else}
  push rsi
  push rdi
  {$endif MSWINDOWS}
  push rbx
  {Since most allocations are for small blocks, determine the small block type}
  lea rdx, [size + BlockHeaderSize - 1]
  shr rdx, 3 // div SmallBlockGranularity
  {Preload the address of small block structures}
  lea rbx, [rip + SmallBlockInfo]
  {Is it a small block?}
  cmp rcx, (MaximumSmallBlockSize - BlockHeaderSize)
  ja @NotASmallBlock
  {Get the small block type pointer in rbx}
  movzx ecx, byte ptr [rbx + rdx + TSmallBlockInfo.AllocSizeIndX4]
  shl ecx, 4
  add rbx, rcx
  {Grab the block type}
@LockBlockTypeLoop:
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try up to two sizes larger}
  add rbx, SizeOf(TSmallBlockType)
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  add rbx, SizeOf(TSmallBlockType)
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub rbx, 2 * SizeOf(TSmallBlockType)
lock inc dword ptr [rbx].TSmallBlockType.GetmemThreadContention
  call SleepInitial
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  call SleepNext
  jmp @LockBlockTypeLoop
@GotLockOnSmallBlockType:
  {Find the next free block: Get the first pool with free blocks in rdx}
  mov rdx, [rbx].TSmallBlockType.NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if rdx = rbx)}
  mov rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
  {Get the drop flags mask in rcx so long}
  mov rcx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp rdx, rbx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  {Get the new first free block}
  and rcx, [rax - BlockHeaderSize]
  {Set the new first free block}
  mov [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov rdx, [rbx].TSmallBlockType.CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  add rcx, rax
  {Can another block fit?}
  cmp rax, [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  {Store the next sequential feed block address}
  mov [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rcx
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
  mov [rbx].TSmallBlockType.NextPartiallyFreePool, rcx
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@AllocateSmallBlockPool:
  {Access shared information about Medium blocks storage}
  lea rcx, [rip + MediumBlockInfo]
  mov eax, $100
  mov r10, rcx
lock cmpxchg byte ptr [rcx + TMediumBlockInfo.Locked], ah
  je @MediumLocked1
  call LockMediumBlocks
@MediumLocked1:
  {Are there any available blocks of a suitable size?}
  movsx esi, [rbx].TSmallBlockType.AllowedGroupsForBlockPoolBitmap
  and esi, [r10 + TMediumBlockInfo.BinGroupBitmap]
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  lea r9, [rax * 4]
  mov rcx, [r8 + r9]
  bsf ecx, ecx
  lea rcx, [rcx + r9 * 8]
  {Get a pointer to the bin in edi}
  lea rsi, [rcx * 8] // SizeOf(TMediumBlockBin) = 16
  lea rdi, [r10 + TMediumBlockInfo.Bins + rsi * 2]
  {Get the free block in rsi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rdx, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rdx
  jne @MediumBinNotEmpty
  {r8 = @MediumBlockBinBitmaps, eax = bin group number,
   r9 = bin group number * 4, ecx = bin number, edi = @bin, esi = free block,
   ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and [r8 + r9], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, [rbx].TSmallBlockType.OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea rcx, [rsi + rdi]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last qword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  jmp @GotMediumBlock
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, [rbx].TSmallBlockType.MinimumBlockPoolSize
  mov edi, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov rsi, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, [rbx].TSmallBlockType.OptimalBlockPoolSize
  lea rdx, [rcx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
@NotMuchSpace:
  sub rsi, rdi
  {Update the sequential feed parameters}
  sub [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], edi
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rsi
  {Get the block pointer}
  jmp @GotMediumBlock
  {Align branch target}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx size, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
  push size // use "size" variable = first argument in current ABI call
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  pop rdi  // restore edi=blocksize and r10=MediumBlockInfo
  lea r10, [rip + MediumBlockInfo]
  mov rsi, rax
  test rax, rax
  jnz @GotMediumBlock // rsi=freeblock rbx=blocktype edi=blocksize
  mov [r10 + TMediumBlockInfo.Locked], al
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  jmp @Done
@UseWholeBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Set the size and flags for this block}
  lea rcx, [rdi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [rsi - BlockHeaderSize], rcx
  {Unlock medium blocks}
  xor eax, eax
  mov [r10 + TMediumBlockInfo.Locked], al
  {Set up the block pool}
  mov TSmallBlockPoolHeader[rsi].BlockType, rbx
  mov TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
  mov TSmallBlockPoolHeader[rsi].BlocksInUse, 1
  {Set it up for sequential block serving}
  mov [rbx].TSmallBlockType.CurrentSequentialFeedPool, rsi
  {Return the pointer to the first block}
  lea rax, [rsi + SmallBlockPoolHeaderSize]
  {Compute the next and last block addresses}
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  lea rdx, [rax + rcx]
  mov [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rdx
  add rdi, rsi
  sub rdi, rcx
  mov [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rdi
  {Unlock the small block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  {Set the small block header}
  mov [rax - BlockHeaderSize], rsi
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
{-------------------Medium block allocation-------------------}
@NotASmallBlock:
  {Access shared information about Medium blocks storage}
  lea r10, [rip + MediumBlockInfo]
  {Do we need a Large block?}
  cmp rcx, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.}
  lea rbx, [rcx + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  mov rcx, r10
  and ebx, -MediumBlockGranularity
  add ebx, MediumBlockSizeOffset
  mov eax, $100
lock cmpxchg byte ptr [rcx + TMediumBlockInfo.Locked], ah
  je   @MediumLocked2
  call LockMediumBlocks
@MediumLocked2:
  {Get the bin number in ecx and the group number in edx}
  lea rdx, [rbx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  and eax, [r8 + rdx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -32
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, [r10 + TMediumBlockInfo.BinGroupBitmap]
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, [r8 + rdx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
@TrySequentialFeedMedium:
  mov ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  {Get the block address}
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  sub rax, rbx
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rax
  {Store the remaining bytes}
  mov [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], ecx
  {Set the flags for the block}
  or rbx, IsMediumBlockFlag
  mov [rax - BlockHeaderSize], rbx
  mov byte ptr [r10 + TMediumBlockInfo.Locked], false
  jmp @Done
@AllocateNewSequentialFeedForMedium:
  mov size, rbx // 'size' variable is the first argument register in ABI call
  call AllocNewSequentialFeedMediumPool
  mov byte [rip + MediumBlockInfo.Locked], false // r10 has been overwritten
  jmp @Done
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  {Get a pointer to the bin in edi}
  lea rax, [rcx + rcx]
  lea rdi, [r10 + TMediumBlockInfo.Bins + rax * 8]
  {Get the free block in esi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rax, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rax
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rax
  jne @MediumBinNotEmptyForMedium
  {edx = bin group number, ecx = bin number, rdi = @bin, rsi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr [r10 + TMediumBlockInfo.BinGroupBitmap], edx
@MediumBinNotEmptyForMedium:
  {rsi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea rcx, [rsi + rbx]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last dword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  jmp @GotMediumBlockForMedium
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea rcx, [rbx + IsMediumBlockFlag]
  mov [rsi - BlockHeaderSize], rcx
  {Unlock medium blocks}
  mov byte ptr[r10 + TMediumBlockInfo.Locked], false
  mov rax, rsi
  jmp @Done
{-------------------Large block allocation-------------------}
@IsALargeBlockRequest:
  xor rax, rax
  test rcx, rcx
  js @Done
  {Size is still in the rcx/rdi first param register}
  call AllocateLargeBlock
@Done:
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
end;

function _FreeMem(P: Pointer): PtrInt; nostackframe; assembler;
asm
  {$ifdef MSWINDOWS}
  push rsi
  {$else}
  mov rcx, P
  {$endif MSWINDOWS}
  push rbx
  {Get the block header in rdx}
  mov rdx, [P - BlockHeaderSize]
  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotSmallBlockInUse
  {Get the small block type in rbx and try to grab it}
  mov rbx, [rdx].TSmallBlockPoolHeader.BlockType
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Spin to grab the block type}
  mov r8, SpinLockCount
@SpinLockBlockType:
  pause
  mov eax, $100
  dec r8
  jz @LockBlockTypeSleep
  cmp [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @SpinLockBlockType
  lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  jmp @SpinLockBlockType
@LockBlockTypeSleep:
  {Couldn't grab the block type - sleep and try again}
  lock inc dword ptr [rbx].TSmallBlockType.FreeMemThreadContention
  push rcx
  call SleepInitial
  pop rcx
@LockBlockTypeLoop:
  mov rdx, [rcx - BlockHeaderSize]
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  push rcx
  call SleepNext
  pop rcx
  jmp @LockBlockTypeLoop
@GotLockOnSmallBlockType:
  {Current state: rdx = @SmallBlockPoolHeader, rcx = APointer, rbx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  {Get the old first free block}
  mov rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test rax, rax
  {Store this as the new first free block}
  mov [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
  {Store the previous first free block as the block header}
  lea rax, [rax + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  pop rbx
  {$ifdef MSWINDOWS}
  pop rsi
  {$endif MSWINDOWS}
  ret
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov rcx, [rbx].TSmallBlockType.NextPartiallyFreePool
  mov [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
  mov [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool, rcx
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rdx
  mov [rbx].TSmallBlockType.NextPartiallyFreePool, rdx
  {All ok}
  xor eax, eax
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  pop rbx
  {$ifdef MSWINDOWS}
  pop rsi
  {$endif MSWINDOWS}
  ret
@PoolIsNowEmpty:
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test rax, rax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov rax, [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool
  mov rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rax
  {Zero out eax}
  xor eax, eax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp [rbx].TSmallBlockType.CurrentSequentialFeedPool, rdx
  jne @NotSequentialFeedPool
@IsSequentialFeedPool:
  mov [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rax
@NotSequentialFeedPool:
  {Unlock the block type}
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  {Release this pool}
  mov rcx, rdx
  mov rdx, [rdx - BlockHeaderSize]
  jmp @FreeMediumBlock
  {---------------------Medium blocks------------------------------}
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
@FreeMediumBlock:
  {Access shared information about Medium blocks storage}
  lea r10, [rip + MediumBlockInfo]
  {Drop the flags}
  and rdx, DropMediumAndLargeFlagsMask
  {Free the medium block pointed to by eax, header in edx, bl = IsMultiThread}
  {Block size in rbx}
  mov rbx, rdx
  {Pointer in rsi}
  mov rsi, rcx
  mov rcx, r10
  mov eax, $100
lock cmpxchg byte ptr [rcx + TMediumBlockInfo.Locked], ah
  je   @MediumBlocksLocked
  call LockMediumBlocks
@MediumBlocksLocked:
  {Get the next block size and flags in rcx}
  mov rcx, [rsi + rbx - BlockHeaderSize]
  {Can we combine this block with the next free block?}
  test qword ptr [rsi + rbx - BlockHeaderSize], IsFreeBlockFlag
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or rcx, PreviousMediumBlockIsFreeFlag
  mov [rsi + rbx - BlockHeaderSize], rcx
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [rsi - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi - BlockHeaderSize], rax
  {Store the trailing size marker}
  mov [rsi + rbx - 2 * BlockHeaderSize], rbx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov rcx, rsi
  mov rdx, rbx
  {Insert into bin}
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  {All OK}
  xor eax, eax
  {Unlock medium blocks}
  mov [r10 + TMediumBlockInfo.Locked], al
  jmp @Done
@NextBlockIsFree:
  {Get the next block address in rax}
  lea rax, [rsi + rbx]
  {Increase the size of this block}
  and rcx, DropMediumAndLargeFlagsMask
  add rbx, rcx
  {Was the block binned?}
  cmp rcx, MinimumMediumBlockSize
  jb @NextBlockChecked
  mov rcx, rax
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  jmp @NextBlockChecked
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov rcx, [rsi - 2 * BlockHeaderSize]
  {Include the previous block}
  sub rsi, rcx
  {Set the new block size}
  add rbx, rcx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov rcx, rsi
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  jmp @PreviousBlockChecked
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  cmp dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub rsi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov rax, TMediumBlockPoolHeader[rsi].PreviousMediumBlockPoolHeader
  mov rdx, TMediumBlockPoolHeader[rsi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
  mov TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
  {Unlock medium blocks}
  mov [r10 + TMediumBlockInfo.Locked], false
  {Free the medium block pool}
  mov P, rsi
  call FreeMedium
  xor eax, eax // assume success
  jmp @Done
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea rbx, [rsi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  mov dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
  {Set the last sequentially fed block}
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rbx
  {Success}
  xor eax, eax
  {Unlock medium blocks}
  mov [r10 + TMediumBlockInfo.Locked], al
  jmp @Done
@NotASmallOrMediumBlock:
  {Attempt to free an already free block?}
  mov eax, -1
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Done
  call FreeLargeBlock // P is still in rcx/rdi first param register
@Done:
  pop rbx
  {$ifdef MSWINDOWS}
  pop rsi
  {$endif MSWINDOWS}
end;

// warning: FPC signature is not the same than Delphi: requires "var P"
function _ReallocMem(var P: Pointer; Size: PtrInt): Pointer; nostackframe; assembler;
asm
  {On entry: rcx = var P; rdx = Size}
  mov rdx, Size
  {$ifdef MSWINDOWS}
  push rdi
  {$endif MSWINDOWS}
  push rbx
  push r14
  push r15
  {Save var P into stack for @Done}
  push P
  {Get the original pointer in r14}
  mov r14, qword ptr[P]
  {ReallocMem(nil,Size)=GetMem(Size)}
  test r14, r14
  jz @PlainGetMem
  {Get the block header}
  mov rcx, [r14 - BlockHeaderSize]
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotASmallBlock
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in rbx}
  mov rbx, [rcx].TSmallBlockPoolHeader.BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  sub ecx, BlockHeaderSize
  {Is it an upsize or a downsize?}
  cmp rcx, rdx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea rbx, [rdx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  {In-place downsize - keep the original pointer}
  mov rax, r14
  pop P
  pop r15
  pop r14
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  {$endif MSWINDOWS}
  ret
@PlainGetMem:
  mov P, rdx // P is the proper first argument register
  call _GetMem
  jmp @Done
@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov rbx, rdx
  {Allocate a smaller block}
  mov P, rdx // P is the proper first argument register
  call _GetMem
  {Allocated OK?}
  test rax, rax
  jz @Done
  push rax
  {Move data across}
  mov r8, rbx
  mov rdx, rax
  mov rcx, r14
  call MoveX16LP
  {Free the original pointer}
  mov P, r14
  call _FreeMem
  {Return the pointer}
  pop rax
  jmp @Done
@SmallUpsize:
  {State: r14=Pointer, rdx=NewSize, rcx=CurrentBlockSize, rbx=CurrentBlockType}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea P, qword ptr[rcx + rcx + SmallBlockUpsizeAdder]
  {New allocated size is the maximum of the requested size and the minimum upsize}
  xor rax, rax
  sub P, rdx
  adc rax, -1
  and P, rax
  add P, rdx
  mov r15, rdx
  {Allocate the new block}
  call _GetMem
  mov rdx, r15
  {Allocated OK?}
  test rax, rax
  jz @Done
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp rdx, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [rax - 2 * BlockHeaderSize], rdx
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx r8d, [rbx].TSmallBlockType.BlockSize
  sub r8d, BlockHeaderSize
  mov r15, rax
  {Move to the new block}
  mov rdx, rax
  mov rcx, r14
  call [rbx].TSmallBlockType.UpsizeMoveProcedure
  {Free the old pointer}
  mov P, r14
  call _FreeMem
  {Done}
  mov rax, r15
  jmp @Done
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Access shared information about Medium blocks storage}
  lea r10, [rip + MediumBlockInfo]
  {Status: rcx = Current Block Size + Flags, r14 = APointer,
   rdx = Requested Size, r10 = MediumBlockInfo}
  mov rbx, rcx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Get a pointer to the next block in rdi}
  lea rdi, [r14 + rcx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask
  {Is it an upsize or a downsize?}
  cmp rdx, rcx
  ja @MediumBlockUpsize
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, r14 = APointer, rdx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea r15, [rdx + rdx]
  cmp r15, rcx
  jb @MediumMustDownsize
@MediumNoResize:
  mov rax, r14
  jmp @Done
@MediumMustDownsize:
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If the
  requested size is less than the threshold value (currently a quarter of the
  minimum medium block size), move the data to a small block, otherwise shrink
  the medium block to the minimum allowable medium block size.}
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  {The request is for a size smaller than the minimum medium block size, but
   not small enough to justify moving data: Reduce the block size to the
   minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  {Is it already at the minimum medium block size?}
  cmp ecx, edx
  jna @MediumNoResize
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea r15, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and r15, -MediumBlockGranularity
  add r15, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, r15d
  mov ebx, ecx
  {Lock the medium blocks}
  mov rcx, r10
  mov eax, $100
lock cmpxchg byte ptr [rcx + TMediumBlockInfo.Locked], ah
  je   @MediumBlocksLocked1
  call LockMediumBlocks
@MediumBlocksLocked1:
  mov ecx, ebx
  {Reread the flags - may have changed before medium blocks could be locked}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [r14 - BlockHeaderSize]
@DoMediumInPlaceDownsize:
  {Set the new size}
  or rbx, r15
  mov [r14 - BlockHeaderSize], rbx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov rdx, [rdi - BlockHeaderSize]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or rdx, PreviousMediumBlockIsFreeFlag
  mov [rdi - BlockHeaderSize], rdx
  jmp @MediumDownsizeDoSplit
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov rcx, rdi
  and rdx, DropMediumAndLargeFlagsMask
  add rbx, rdx
  add rdi, rdx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock // rcx=APMediumFreeBlock
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [rdi - 2 * BlockHeaderSize], rbx
  {Store the free part's header}
  lea rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [r14 + r15 - BlockHeaderSize], rcx
  {Bin this free block}
  cmp rbx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea rcx, [r14 + r15]
  mov rdx, rbx
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumBlockDownsizeDone:
  {Unlock the medium blocks}
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
  {result = old pointer}
  mov rax, r14
  jmp @Done
@MediumDownsizeRealloc:
  {Save the requested size}
  push rdx
  {Allocate the new block}
  mov P, rdx
  call _GetMem
  pop r8
  test rax, rax
  jz @Done
  push rax
  {Move the data across}
  mov rdx, rax
  mov rcx, r14
  call MoveX16LP
  mov P, r14
  call _FreeMem
  {Return the result}
  pop rax
  jmp @Done
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, r14 = APointer, rdx = Requested Size}
  {Can we do an in-place upsize?}
  mov rax, [rdi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and rax, DropMediumAndLargeFlagsMask
  {r15 = total available size including the next block (excluding the header)}
  lea r15, [rax + rcx]
  {Can the block fit?}
  cmp rdx, r15
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
  mov rbx, rcx
  {Lock the medium blocks.}
  mov rcx, r10
  mov eax, $100
lock cmpxchg byte ptr [rcx + TMediumBlockInfo.Locked], ah
  je   @MediumBlocksLocked2
  mov r15, rdx
  call LockMediumBlocks
  mov rdx, r15
@MediumBlocksLocked2:
  mov rcx, rbx
  {Re-read the info for this block (since it may have changed before the medium
   blocks could be locked)}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [r14 - BlockHeaderSize]
  {Re-read the info for the next block}
  mov rax, [rdi - BlockHeaderSize]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea r15, [rax + rcx]
  {Can the block still fit?}
  cmp rdx, r15
  ja @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  push rcx
  push rdx
  mov rcx, rdi
  call RemoveMediumFreeBlock // rcx=APMediumFreeBlock
  pop rdx
  pop rcx
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea rax, [rax + rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, -MediumBlockGranularity
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea rdx, [r15 + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and qword ptr [r14 + r15], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add r15, BlockHeaderSize
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [r14 + r15 - BlockHeaderSize], rdx
  {Set the second split header}
  lea rdi, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [r14 + rax - BlockHeaderSize], rdi
  mov r15, rax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  lea rcx, [r14 + rax]
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or r15, rbx
  mov [r14 - BlockHeaderSize], r15
  {Unlock the medium blocks}
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
  {result = old pointer}
  mov rax, r14
  jmp @Done
@NextMediumBlockChanged:
  {The next medium block changed while the medium blocks were being locked}
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor rdi, rdi
  sub rax, rdx
  adc rdi, -1
  and rax, rdi
  add rax, rdx
  {Save the size to allocate}
  mov r15, rax
  {Save the size to move across}
  mov edi, ecx
  {Save the requested sizes}
  push rdx
  push rdi
  {Get the block}
  mov P, rax
  call _GetMem
  pop rdi
  pop rdx
  {Success?}
  test eax, eax
  jz @Done
  {If it's a Large block - store the actual user requested size}
  cmp r15, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [rax - 2 * BlockHeaderSize], rdx
@MediumUpsizeNotLarge:
  {Save the result}
  push rax
  {Move the data across}
  mov rdx, rax
  mov rcx, r14
  mov r8, rdi
  call MoveX16LP
  {Free the old block}
  mov P, r14
  call _FreeMem
  {Restore the result}
  pop rax
  jmp @Done
@Error:
  xor eax, eax
  jmp @Done
@PossibleLargeBlock:
  {-----------------------Large block------------------------------}
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Error
  {$ifdef MSWINDOWS}
  mov rcx, r14
  {$else}
  mov rdi, r14
  mov rsi, rdx
  {$endif MSWINDOWS}
  call ReallocateLargeBlock // with restored proper registers
@Done:
  pop rcx
  mov qword ptr[rcx], rax // store new pointer in var P
  pop r15
  pop r14
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  {$endif MSWINDOWS}
end;

function _AllocMem(Size: PtrInt): Pointer; nostackframe; assembler;
asm
  push rbx
  {Get size rounded down to the previous multiple of SizeOf(Pointer) into ebx}
  lea rbx, [Size - 1]
  and rbx, -8
  {Get the block}
  call _GetMem
  {Could a block be allocated? rcx = 0 if yes, -1 if no}
  cmp rax, 1
  sbb rcx, rcx
  {Point rdx to the last dword}
  lea rdx, [rax + rbx]
  {rbx = -1 if no block could be allocated, otherwise size rounded down
   to previous multiple of 8. If rbx = 0 then the block size is 1..8 bytes and
   the SSE2 based clearing loop should not be used (since it clears 16 bytes per
   iteration).}
  or rbx, rcx
  jz @ClearLastQWord
  {Large blocks are already zero filled}
  cmp rbx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done
  {Make the counter negative based}
  neg rbx
  {Load zero into xmm0}
  pxor xmm0, xmm0
  {Clear groups of 16 bytes. Block sizes are 8 less than a multiple of 16}
  align 16
@FillLoop:
  movaps oword ptr [rdx + rbx], xmm0 // non-temporal movntdq not needed (256KB)
  add rbx, 16
  js @FillLoop
  {Clear the last 8 bytes}
@ClearLastQWord:
  xor rcx, rcx
  mov qword ptr [rdx], rcx
@Done:
  pop rbx
end;

function _MemSize(P: Pointer): PtrUInt;
begin
  // AFAIK used only by fpc_AnsiStr_SetLength() in RTL
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

function _FreeMemSize(P: pointer; size: PtrInt): PtrInt;
begin
  // should return the chunk size - only used by heaptrc
  if size <> 0 then
  begin
    result := _MemSize(P);
    _FreeMem(p);
  end
  else
    result := 0;
end;


{ ********* Information Gathering }

function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function K(i: PtrUInt): shortstring;
var
  tmp: string[1];
begin
  if i >= 1 shl 40 then
  begin
    i := i shr 40;
    tmp := 'T';
  end
  else
  if i >= 1 shl 30 then
  begin
    i := i shr 30;
    tmp := 'G';
  end
  else
  if i >= 1 shl 20 then
  begin
    i := i shr 20;
    tmp := 'M';
  end
  else
  if i >= 1 shl 10 then
  begin
    i := i shr 10;
    tmp := 'K';
  end
  else
    tmp := '';
  str(i, result);
  result := result + tmp;
end;

{$I-}

procedure WriteHeapStatusDetail(const arena: TMMStatusArena;
  const name: shortstring);
begin
  write(name, K(arena.CurrentBytes):4,
    'B/', K(arena.CumulativeBytes), 'B ');
  {$ifdef FPCMM_DEBUG}
  write('   peak=', K(arena.PeakBytes),
    'B current=', K(arena.CumulativeAlloc - arena.CumulativeFree),
    ' alloc=', K(arena.CumulativeAlloc), ' free=', K(arena.CumulativeFree));
  {$endif FPCMM_DEBUG}
  writeln(' sleep=', K(arena.ThreadContention));
end;

procedure WriteHeapStatus(const context: shortstring;
  includesmallblockcontention: integer);
var
  small: TFPCMMSmallBlockContentionDynArray;
  i, G, F: PtrInt;
begin
  if context <> '' then
    writeln(context);
  with HeapStatus do
  begin
    WriteHeapStatusDetail(Medium, ' Medium: ');
    WriteHeapStatusDetail(Large,  ' Large:  ');
    {$ifdef FPCMM_DEBUG}
    writeln(' Sleep:  count=', SleepingCount, ' microsec=', SleepingTime);
    {$endif FPCMM_DEBUG}
  end;
  if includesmallblockcontention <= 0 then
    exit;
  small := GetSmallBlockContention(@G, @F);
  if small <> nil then
  begin
    writeln(' Small Waits:   Getmem=', K(G), ' Freemem=', K(F));
    for i := 0 to high(small) do
      with small[i] do
      begin
        if GetmemThreadContentionSmallBlockSize <> 0 then
          write(' Getmem(', GetmemThreadContentionSmallBlockSize)
        else
          write(' Freemem(', FreememThreadContentionSmallBlockSize);
        write(')=' , ThreadContention);
        if i = includesmallblockcontention then
          exit;
      end;
    writeln;
  end;
end;

{$I+}

function CurrentHeapStatus: TMMStatus;
{$ifdef FPCMM_DEBUG}
var
  i: integer;
  p: PSmallBlockType;
  {$endif FPCMM_DEBUG}
begin
  result := HeapStatus;
  {$ifdef FPCMM_DEBUG}
  p := @SmallBlockInfo.Types;
  for i := 1 to NumSmallBlockTypes do
  begin
    inc(result.SmallGetmemThreadContention,
      p^.GetmemThreadContention);
    inc(result.SmallFreememThreadContention,
      p^.FreememThreadContention);
    inc(p);
  end;
  {$endif FPCMM_DEBUG}
end;

procedure QuickSortRes(const Res: TFPCMMSmallBlockContentionDynArray; L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: cardinal;
  tmp: TFPCMMSmallBlockContention;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := Res[P].ThreadContention;
        while Res[I].ThreadContention > pivot do
          inc(I);
        while Res[J].ThreadContention < pivot do
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
      begin // use recursion only for smaller range
        if L < J then
          QuickSortRes(Res, L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortRes(Res, I, R);
        R := J;
      end;
    until L >= R;
end;

function GetSmallBlockContention(GetMemTotal,
  FreeMemTotal: PPtrInt): TFPCMMSmallBlockContentionDynArray;
var
  i, n: integer;
  p: PSmallBlockType;
  d: ^TFPCMMSmallBlockContention;
begin
  if GetMemTotal <> nil then
    GetMemTotal^ := 0;
  if FreeMemTotal <> nil then
    FreeMemTotal^ := 0;
  n := 0;
  p := @SmallBlockInfo.Types;
  for i := 1 to NumSmallBlockTypes do
  begin
    if p^.GetmemThreadContention <> 0 then
      inc(n);
    if p^.FreememThreadContention <> 0 then
      inc(n);
    inc(p);
  end;
  SetLength(result, n);
  if result = nil then
    exit;
  d := pointer(result);
  p := @SmallBlockInfo.Types;
  for i := 1 to NumSmallBlockTypes do
  begin
    if p^.GetmemThreadContention <> 0 then
    begin
      d^.ThreadContention := p^.GetmemThreadContention;
      if GetMemTotal <> nil then
        inc(GetMemTotal^, p^.GetmemThreadContention);
      d^.GetmemThreadContentionSmallBlockSize := p^.BlockSize;
      inc(d);
    end;
    if p^.FreememThreadContention <> 0 then
    begin
      d^.ThreadContention := p^.FreememThreadContention;
      if FreeMemTotal <> nil then
        inc(FreeMemTotal^, p^.FreeMemThreadContention);
      d^.FreememThreadContentionSmallBlockSize := p^.BlockSize;
      inc(d);
    end;
    inc(p);
  end;
  QuickSortRes(result, 0, n - 1);
end;


{ ********* Initialization and Finalization }

const
  NewMM: TMemoryManager = (
    NeedLock: false;
    GetMem: @_Getmem;
    FreeMem: @_FreeMem;
    FreememSize: @_FreememSize;
    AllocMem: @_AllocMem;
    ReallocMem: @_ReAllocMem;
    MemSize: @_MemSize;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @_GetHeapStatus;
    GetFPCHeapStatus: @_GetFPCHeapStatus);

var
  OldMM: TMemoryManager;

procedure BuildBlockTypeLookupTable;
var
  i, start, next: PtrUInt;
  ndx: Byte;
begin
  start := 0;
  with SmallBlockInfo do
    for i := 0 to High(Types) do
    begin
      next := Types[i].BlockSize;
      if next and 15 = 0 then
      begin
        next := next div SmallBlockGranularity;
        ndx := i * 4;
        while start < next do
        begin
          AllocSizeIndX4[start] := ndx;
          inc(start);
        end;
        start := next;
      end;
    end;
end;

procedure InitializeMemoryManager;
var
  p: PSmallBlockType;
  i, min, poolsize, num, perpool, size: PtrInt;
  medium: PMediumFreeBlock;
begin
  p := @SmallBlockInfo.Types;
  assert(SizeOf(p^) = 64); // as expected by the asm above
  for i := 0 to NumSmallBlockTypes - 1 do
  begin
    size := SmallBlockSizes[i];
    p^.BlockSize := size;
    case size of
      16:
        p^.UpsizeMoveProcedure := Move8;
      32:
        p^.UpsizeMoveProcedure := Move24;
      48:
        p^.UpsizeMoveProcedure := Move40;
      64:
        p^.UpsizeMoveProcedure := Move56;
    else
      p^.UpsizeMoveProcedure := MoveX16LP;
    end;
    p^.PreviousPartiallyFreePool := pointer(p);
    p^.NextPartiallyFreePool := pointer(p);
    p^.MaxSequentialFeedBlockAddress := Pointer(0);
    p^.NextSequentialFeedBlockAddress := Pointer(1);
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
    p^.AllowedGroupsForBlockPoolBitmap := Byte(Byte(-1) shl num);
    p^.MinimumBlockPoolSize := MinimumMediumBlockSize +
      num * (MediumBlockBinsPerGroup * MediumBlockGranularity);
    poolsize := ((size * TargetSmallBlocksPerPool +
      (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    if poolsize < OptimalSmallBlockPoolSizeLowerLimit then
      poolsize := OptimalSmallBlockPoolSizeLowerLimit;
    if poolsize > OptimalSmallBlockPoolSizeUpperLimit then
      poolsize := OptimalSmallBlockPoolSizeUpperLimit;
    perpool := (poolsize - SmallBlockPoolHeaderSize) div size;
    p^.OptimalBlockPoolSize := ((perpool * size +
       (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
        and -MediumBlockGranularity) + MediumBlockSizeOffset;
    inc(p);
  end;
  BuildBlockTypeLookupTable;
  with MediumBlockInfo do
  begin
    PoolsCircularList.PreviousMediumBlockPoolHeader := @PoolsCircularList;
    PoolsCircularList.NextMediumBlockPoolHeader := @PoolsCircularList;
    for i := 0 to High(Bins) do
    begin
      medium := @Bins[i];
      medium.PreviousFreeBlock := medium;
      medium.NextFreeBlock := medium;
    end;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

procedure FreeAllMemory;
var
  head, nexthead: PMediumBlockPoolHeader;
  freeblock: PMediumFreeBlock;
  large, nextlarge: PLargeBlockHeader;
  p: PSmallBlockType;
  i, size: PtrInt;
begin
  with MediumBlockInfo do
  begin
    head := PoolsCircularList.NextMediumBlockPoolHeader;
    while head <> @PoolsCircularList do
    begin
      nexthead := head.NextMediumBlockPoolHeader;
      FreeMedium(head);
      head := nexthead;
    end;
    p := @SmallBlockInfo.Types;
    for i := 1 to NumSmallBlockTypes do
    begin
      p^.PreviousPartiallyFreePool := pointer(p);
      p^.NextPartiallyFreePool := pointer(p);
      p^.NextSequentialFeedBlockAddress := Pointer(1);
      p^.MaxSequentialFeedBlockAddress := nil;
      inc(p);
    end;
    PoolsCircularList.PreviousMediumBlockPoolHeader := @PoolsCircularList;
    PoolsCircularList.NextMediumBlockPoolHeader := @PoolsCircularList;
    for i := 0 to High(Bins) do
    begin
      freeblock := @Bins[i];
      freeblock.PreviousFreeBlock := freeblock;
      freeblock.NextFreeBlock := freeblock;
    end;
    BinGroupBitmap := 0;
    SequentialFeedBytesLeft := 0;
    FillChar(BinBitmaps, SizeOf(BinBitmaps), 0);
  end;
  large := LargeBlocksCircularList.NextLargeBlockHeader;
  while large <> @LargeBlocksCircularList do
  begin
    nextlarge := large.NextLargeBlockHeader;
    size := large.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    NotifyFree(HeapStatus.Large, size);
    Free(large, size);
    large := nextlarge;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
  {$ifdef FPCMM_DEBUG}
  if (HeapStatus.Large.CurrentBytes <> 0) or (HeapStatus.Medium.CurrentBytes <> 0) then
    WriteHeapStatus('mormot.core.fpcx64mm: memory leak detected at shutdown');
  {$endif FPCMM_DEBUG}
end;


initialization
  InitializeMemoryManager;
  GetMemoryManager(OldMM);
  SetMemoryManager(NewMM);

finalization
  SetMemoryManager(OldMM);
  FreeAllMemory;

{$endif FPC_CPUX64}

end.

