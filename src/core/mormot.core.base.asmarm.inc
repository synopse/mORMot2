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

procedure mul64x64(const left, right: QWord; out product: THash128Rec);
  assembler; nostackframe;
asm
  // x0 = left  x1 = right  x2 = @product
  mul   x3, x0, x1
  umulh x4, x0, x1
  stp   x3, x4, [x2]
end;

procedure _mult128(constref l, r: THash128Rec; out product: THash256Rec);
  assembler; nostackframe;
asm
  // x0 = @l, x1 = @r, x2 = @product
  ldp   x3, x4, [x0]       // l0, l1
  ldp   x5, x6, [x1]       // r0, r1
  mul   x7, x3, x5        // low(l0*r0)
  umulh x8, x3, x5        // high(l0*r0)
  str   x7, [x2]
  mul   x9, x4, x5        // low(l1*r0)
  umulh x10, x4, x5       // high(l1*r0)
  adds  x8, x8, x9
  adc   x10, x10, xzr     // high(l1*r0) <= 2^64-2, so no overflow here
  mul   x9, x3, x6        // low(l0*r1)
  umulh x11, x3, x6       // high(l0*r1)
  adds  x8, x8, x9
  adcs  x10, x10, x11
  adc   x12, xzr, xzr     // carry to highest 64-bit limb
  str   x8, [x2, #8]
  mul   x13, x4, x6       // low(l1*r1)
  umulh x14, x4, x6       // high(l1*r1)
  adds  x13, x13, x10
  adc   x14, x14, x12
  stp   x13, x14, [x2, #16]
end;

procedure _mult256(out Output: THash512Rec; const Left, Right: THash256Rec);
  assembler; nostackframe;
asm
  // x0 = @Output, x1 = @Left, x2 = @Right
  // a0..a3 = x3..x6, b0..b3 = x7..x10
  ldp   x3, x4, [x1]
  ldp   x5, x6, [x1, #16]
  ldp   x7, x8, [x2]
  ldp   x9, x10, [x2, #16]
  // x11: low accumulator, x12: high, x13: overflow
  // x14/x15: low/high halves of the current 64x64 product
  // Column 0: a0*b0
  mul   x14, x3, x7
  umulh x11, x3, x7
  str   x14, [x0]
  mov   x12, xzr
  mov   x13, xzr
  // Column 1
  mul   x14, x4, x7   // a1*b0
  umulh x15, x4, x7
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x3, x8   // a0*b1
  umulh x15, x3, x8
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  str   x11, [x0, #8]
  mov   x11, x12
  mov   x12, x13
  mov   x13, xzr
  // Column 2
  mul   x14, x5, x7   // a2*b0
  umulh x15, x5, x7
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x4, x8   // a1*b1
  umulh x15, x4, x8
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x3, x9   // a0*b2
  umulh x15, x3, x9
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  str   x11, [x0, #16]
  mov   x11, x12
  mov   x12, x13
  mov   x13, xzr
  // Column 3
  mul   x14, x6, x7   // a3*b0
  umulh x15, x6, x7
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x5, x8   // a2*b1
  umulh x15, x5, x8
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x4, x9   // a1*b2
  umulh x15, x4, x9
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x3, x10   // a0*b3
  umulh x15, x3, x10
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  str   x11, [x0, #24]
  mov   x11, x12
  mov   x12, x13
  mov   x13, xzr
  // Column 4
  mul   x14, x6, x8   // a3*b1
  umulh x15, x6, x8
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x5, x9   // a2*b2
  umulh x15, x5, x9
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x4, x10   // a1*b3
  umulh x15, x4, x10
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  str   x11, [x0, #32]
  mov   x11, x12
  mov   x12, x13
  mov   x13, xzr
  // Column 5
  mul   x14, x6, x9   // a3*b2
  umulh x15, x6, x9
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  mul   x14, x5, x10   // a2*b3
  umulh x15, x5, x10
  adds  x11, x11, x14
  adcs  x12, x12, x15
  adc   x13, x13, xzr
  str   x11, [x0, #40]
  mov   x11, x12
  mov   x12, x13
  mov   x13, xzr
  // Column 6
  mul   x14, x6, x10   // a3*b3
  umulh x15, x6, x10
  adds  x11, x11, x14
  adc   x12, x12, x15
  str   x11, [x0, #48]
  str   x12, [x0, #56]
end;

// Output := Left + Right (mod 2^256); return carry (0 or 1)
function _add256(out Output: THash256Rec; const Left, Right: THash256Rec): PtrUInt;
  assembler; nostackframe;
asm
  // x0 = @Output, x1 = @Left, x2 = @Right
  ldp   x3, x4, [x1]
  ldp   x5, x6, [x1, #16]
  ldp   x7, x8, [x2]
  ldp   x9, x10, [x2, #16]
  adds  x3, x3, x7
  adcs  x4, x4, x8
  adcs  x5, x5, x9
  adcs  x6, x6, x10
  stp   x3, x4, [x0]
  stp   x5, x6, [x0, #16]
  adc   x0, xzr, xzr
end;

// Value := Value + Added (mod 2^256); return carry (0 or 1)
function _inc256(var Value: THash256Rec; const Added: THash256Rec): PtrUInt;
  assembler; nostackframe;
asm
  // x0 = @Value, x1 = @Added
  ldp   x2, x3, [x0]
  ldp   x4, x5, [x0, #16]
  ldp   x6, x7, [x1]
  ldp   x8, x9, [x1, #16]
  adds  x2, x2, x6
  adcs  x3, x3, x7
  adcs  x4, x4, x8
  adcs  x5, x5, x9
  stp   x2, x3, [x0]
  stp   x4, x5, [x0, #16]
  adc   x0, xzr, xzr
end;

// Value := Value - Subs (mod 2^256); return borrow (0 or 1)
function _dec256(var Value: THash256Rec; const Subs: THash256Rec): PtrUInt;
  assembler; nostackframe;
asm
  // x0 = @Value, x1 = @Subs
  ldp   x2, x3, [x0]
  ldp   x4, x5, [x0, #16]
  ldp   x6, x7, [x1]
  ldp   x8, x9, [x1, #16]
  subs  x2, x2, x6
  sbcs  x3, x3, x7
  sbcs  x4, x4, x8
  sbcs  x5, x5, x9
  stp   x2, x3, [x0]
  stp   x4, x5, [x0, #16]
  sbc  x0, xzr, xzr
  and  x0, x0, #1
end;

// Output := Left - Right (mod 2^256); return borrow (0 or 1)
function _sub256(out Output: THash256Rec; const Left, Right: THash256Rec): PtrUInt;
  assembler; nostackframe;
asm
  // x0 = @Output, x1 = @Left, x2 = @Right
  ldp   x3, x4, [x1]
  ldp   x5, x6, [x1, #16]
  ldp   x7, x8, [x2]
  ldp   x9, x10, [x2, #16]
  subs  x3, x3, x7
  sbcs  x4, x4, x8
  sbcs  x5, x5, x9
  sbcs  x6, x6, x10
  stp   x3, x4, [x0]
  stp   x5, x6, [x0, #16]
  sbc  x0, xzr, xzr
  and  x0, x0, #1
end;

// V := V >> 1; low bit discarded, high bit cleared
procedure _rshift1(var V: THash256Rec);
  assembler; nostackframe;
asm
  // x0 = @V
  ldp   x1, x2, [x0]
  ldp   x3, x4, [x0, #16]
  extr  x1, x2, x1, #1
  extr  x2, x3, x2, #1
  extr  x3, x4, x3, #1
  lsr   x4, x4, #1
  stp   x1, x2, [x0]
  stp   x3, x4, [x0, #16]
end;

// V := V << 1; return old bit 255 (0 or 1)
function _lshift1(var V: THash256Rec): PtrUInt;
  assembler; nostackframe;
asm
  // x0 = @V
  ldp   x1, x2, [x0]
  ldp   x3, x4, [x0, #16]
  lsr   x5, x4, #63
  extr  x4, x4, x3, #63
  extr  x3, x3, x2, #63
  extr  x2, x2, x1, #63
  lsl   x1, x1, #1
  stp   x1, x2, [x0]
  stp   x3, x4, [x0, #16]
  mov   x0, x5
end;

function _cmp256(const Left, Right: THash256Rec): integer;
  assembler; nostackframe;
asm
  // branchless constant time comparison of x0 = @Left vs x1 = @Right
  ldp   x2, x3, [x0]
  ldp   x4, x5, [x0, #16]
  ldp   x6, x7, [x1]
  ldp   x8, x9, [x1, #16]
  // compute borrow(Left - Right)
  subs  xzr, x2, x6
  sbcs  xzr, x3, x7
  sbcs  xzr, x4, x8
  sbcs  xzr, x5, x9
  sbc   x10, xzr, xzr  // -1 when Left < Right
  // compute borrow(Right - Left)
  subs  xzr, x6, x2
  sbcs  xzr, x7, x3
  sbcs  xzr, x8, x4
  sbcs  xzr, x9, x5
  sbc   x0, xzr, xzr   // -1 when Right < Left
  sub   x0, x10, x0    // -1, 0, or 1
end;


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

procedure mul64x64(constref left, right: QWord; out product: THash128Rec);
  assembler; nostackframe;
asm
  // r0 = @left  r1 = @right  r2 = @product
  push  {r4-r7}
  // Load both 64-bit operands
  ldr   r3, [r0, #4]     // left.H
  ldr   r0, [r0]         // left.L
  ldr   r12, [r1, #4]    // right.H
  ldr   r1, [r1]         // right.L
  // Low x Low
  umull r4, r5, r0, r1
  // Low x High
  umull r6, r7, r0, r12
  adds  r5, r5, r6
  adc   r7, r7, #0
  // High x Low
  umull r0, r6, r3, r1
  adds  r5, r5, r0
  adcs  r7, r7, r6
  // High x High
  umull r0, r1, r3, r12
  adc   r1, r1, #0
  adds  r0, r0, r7
  adc   r1, r1, #0
  // Store 128-bit product
  str   r4, [r2]
  str   r5, [r2, #4]
  str   r0, [r2, #8]
  str   r1, [r2, #12]
  pop   {r4-r7}
end;

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


