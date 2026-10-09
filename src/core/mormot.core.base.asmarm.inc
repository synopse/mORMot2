{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   AARCH64 and ARMv7 assembly used by mormot.core.base.pas
}

{
  Locked*() implementation notes:
  - FPC RTL InterlockedXXX() are plain LDXR/STXR loops on AARCH64, or
    LDREX/STREX loops on ARMv7, with no memory barrier, whereas Locked*() are
    expected to be full barriers, as the LOCK prefix on Intel - otherwise
    TLightLock/TRWLock & co don't protect data
  - on AARCH64, we use LDXR/STLXR loops followed by DMB ISH, which is how the
    Linux kernel implements its "fully ordered" atomics on arm64 - about twice
    faster than surrounding the RTL calls with two DMB ISH
  - ARMv7 has no store-release STLEX, so we surround LDREX/STREX loops with two
    DMB ISH, as the Linux kernel does on arm - about a third faster than
    surrounding the RTL calls with two ReadWriteBarrier calls
  - this ARMv7 asm requires CPUARM_HAS_DMB: older ARM CPUs use the RTL calls
    with ReadWriteBarrier
  - a failed compare-and-swap is a full barrier too, as LOCK CMPXCHG
  - "dmb #11" is "dmb ish" - FPC 3.2 assembler does not know the ish operand
}

{$ifdef FPC_CPUAARCH64}

procedure LockedInc32(int32: PInteger); assembler; nostackframe;
asm
.LLockedInc32Loop:
  ldxr  w1, [x0]
  add   w1, w1, #1
  stlxr w2, w1, [x0]
  cbnz  w2, .LLockedInc32Loop
  dmb   #11
end;

procedure LockedDec32(int32: PInteger); assembler; nostackframe;
asm
.LLockedDec32Loop:
  ldxr  w1, [x0]
  sub   w1, w1, #1
  stlxr w2, w1, [x0]
  cbnz  w2, .LLockedDec32Loop
  dmb   #11
end;

procedure LockedInc64(int64: PInt64); assembler; nostackframe;
asm
.LLockedInc64Loop:
  ldxr  x1, [x0]
  add   x1, x1, #1
  stlxr w2, x1, [x0]
  cbnz  w2, .LLockedInc64Loop
  dmb   #11
end;

function LockedExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt): boolean;
  assembler; nostackframe;
asm
.LLockedExcLoop:
  ldxr  x3, [x0]
  cmp   x3, x2
  b.ne  .LLockedExcFail
  stlxr w4, x1, [x0]
  cbnz  w4, .LLockedExcLoop
  dmb   #11
  mov   w0, #1
  ret
.LLockedExcFail:
  clrex
  dmb   #11
  mov   w0, #0
end;

function LockedExc32(var Target: cardinal; NewValue, Comperand: cardinal): boolean;
  assembler; nostackframe;
asm
.LLockedExc32Loop:
  ldxr  w3, [x0]
  cmp   w3, w2
  b.ne  .LLockedExc32Fail
  stlxr w4, w1, [x0]
  cbnz  w4, .LLockedExc32Loop
  dmb   #11
  mov   w0, #1
  ret
.LLockedExc32Fail:
  clrex
  dmb   #11
  mov   w0, #0
end;

procedure LockedAdd32(var Target: cardinal; Increment: cardinal);
  assembler; nostackframe;
asm
.LLockedAdd32Loop:
  ldxr  w2, [x0]
  add   w2, w2, w1
  stlxr w3, w2, [x0]
  cbnz  w3, .LLockedAdd32Loop
  dmb   #11
end;

procedure LockedAdd(var Target: PtrUInt; Increment: PtrUInt);
  assembler; nostackframe;
asm
.LLockedAddLoop:
  ldxr  x2, [x0]
  add   x2, x2, x1
  stlxr w3, x2, [x0]
  cbnz  w3, .LLockedAddLoop
  dmb   #11
end;

procedure LockedDec(var Target: PtrUInt; Decrement: PtrUInt);
  assembler; nostackframe;
asm
.LLockedDecLoop:
  ldxr  x2, [x0]
  sub   x2, x2, x1
  stlxr w3, x2, [x0]
  cbnz  w3, .LLockedDecLoop
  dmb   #11
end;

function InterlockedIncrement(var I: integer): integer;
  assembler; nostackframe;
asm
.LInterlockedIncLoop:
  ldxr  w1, [x0]
  add   w1, w1, #1
  stlxr w2, w1, [x0]
  cbnz  w2, .LInterlockedIncLoop
  dmb   #11
  mov   w0, w1            // return new value
end;

function InterlockedDecrement(var I: integer): integer;
  assembler; nostackframe;
asm
.LInterlockedDecLoop:
  ldxr  w1, [x0]
  sub   w1, w1, #1
  stlxr w2, w1, [x0]
  cbnz  w2, .LInterlockedDecLoop
  dmb   #11
  mov   w0, w1            // return new value
end;

{$else} // ARMv7 with CPUARM_HAS_DMB

procedure LockedInc32(int32: PInteger); assembler; nostackframe;
asm
  dmb   #11
.LLockedInc32Loop:
  ldrex r1, [r0]
  add   r1, r1, #1
  strex r2, r1, [r0]
  cmp   r2, #0
  bne   .LLockedInc32Loop
  dmb   #11
end;

procedure LockedDec32(int32: PInteger); assembler; nostackframe;
asm
  dmb   #11
.LLockedDec32Loop:
  ldrex r1, [r0]
  sub   r1, r1, #1
  strex r2, r1, [r0]
  cmp   r2, #0
  bne   .LLockedDec32Loop
  dmb   #11
end;

function InterlockedIncrement(var I: integer): integer;
  assembler; nostackframe;
asm
  dmb   #11
.LInterlockedIncLoop:
  ldrex r1, [r0]
  add   r1, r1, #1
  strex r2, r1, [r0]
  cmp   r2, #0
  bne   .LInterlockedIncLoop
  dmb   #11
  mov   r0, r1            // return new value
end;

function InterlockedDecrement(var I: integer): integer;
  assembler; nostackframe;
asm
  dmb   #11
.LInterlockedDecLoop:
  ldrex r1, [r0]
  sub   r1, r1, #1
  strex r2, r1, [r0]
  cmp   r2, #0
  bne   .LInterlockedDecLoop
  dmb   #11
  mov   r0, r1            // return new value
end;

procedure LockedInc64(int64: PInt64);
begin
  // LDREXD/STREXD would require an 8-byte aligned int64^: two atomic steps
  with PInt64Rec(int64)^ do
    if InterlockedIncrement(PInteger(@Lo)^) = 0 then
      LockedInc32(@Hi); // collission is highly unprobable
end;

function LockedExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt): boolean;
  assembler; nostackframe;
asm
  dmb   #11
.LLockedExcLoop:
  ldrex r3, [r0]
  cmp   r3, r2
  bne   .LLockedExcFail
  strex r12, r1, [r0]
  cmp   r12, #0
  bne   .LLockedExcLoop
  dmb   #11
  mov   r0, #1
  bx    lr
.LLockedExcFail:
  clrex
  dmb   #11
  mov   r0, #0
end;

function LockedExc32(var Target: cardinal; NewValue, Comperand: cardinal): boolean;
  assembler; nostackframe;
asm
  dmb   #11
.LLockedExc32Loop:
  ldrex r3, [r0]
  cmp   r3, r2
  bne   .LLockedExc32Fail
  strex r12, r1, [r0]
  cmp   r12, #0
  bne   .LLockedExc32Loop
  dmb   #11
  mov   r0, #1
  bx    lr
.LLockedExc32Fail:
  clrex
  dmb   #11
  mov   r0, #0
end;

procedure LockedAdd32(var Target: cardinal; Increment: cardinal);
  assembler; nostackframe;
asm
  dmb   #11
.LLockedAdd32Loop:
  ldrex r2, [r0]
  add   r2, r2, r1
  strex r3, r2, [r0]
  cmp   r3, #0
  bne   .LLockedAdd32Loop
  dmb   #11
end;

procedure LockedAdd(var Target: PtrUInt; Increment: PtrUInt);
  assembler; nostackframe;
asm
  dmb   #11
.LLockedAddLoop:
  ldrex r2, [r0]
  add   r2, r2, r1
  strex r3, r2, [r0]
  cmp   r3, #0
  bne   .LLockedAddLoop
  dmb   #11
end;

procedure LockedDec(var Target: PtrUInt; Decrement: PtrUInt);
  assembler; nostackframe;
asm
  dmb   #11
.LLockedDecLoop:
  ldrex r2, [r0]
  sub   r2, r2, r1
  strex r3, r2, [r0]
  cmp   r3, #0
  bne   .LLockedDecLoop
  dmb   #11
end;

{$endif FPC_CPUAARCH64}


