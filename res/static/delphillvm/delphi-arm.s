// interface stubs for the Delphi LLVM 32-bit ARM compiler (Android armeabi-v7a)
//   armfakestub   = TInterfacedObjectFake methods (SOA client side)
//   armcallmethod = CallMethod() for RawExecute (SOA server side, callbacks)
//   armfakethunks = the VMT entry points, one per method index
// counterpart of delphi-aarch64.s, used by mormot.core.interfaces.pas when
// DELPHI_ARM32_STUBS is defined
//
// build with the clang of the NDK shipped with the RAD Studio Android SDK
// (see compile.sh)
//
// Delphi follows AAPCS with VFP hard-float: R0..R3 and D0..D7 carry the
// parameters, but a by-ref result (string, dynarray, record...) is passed in
// R0 and Self in R1 - the opposite of FPC, see DELPHI_RESULT_FIRST
//
// armfakestub is entered from one of the thunks below, with
//   R12 = method index, R0..R3 / D0..D7 / stack = parameters
// it builds a TFakeCallStack (ABIA32 layout) at SP+4, i.e. 132 bytes below
// the caller stack arguments:
//   +0   ParamRegs   R0..R3
//   +16  FPRegs      D0..D7
//   +80  MethodIndex (R12)
//   +84  Frame, +88 Ret, +92 DummyStack[0..9] (R11/LR saved at +124/+128)
//   +132 Stack       = caller stack arguments
// then calls fakecall(Instance = R0, Stack), i.e. the exported
// TInterfacedObjectFakeRaw.FakeCall redirection, which returns the ordinal
// result in R0:R1 and stores any float result into FPRegs[D0]
//
// NOTE: the .fnstart/.save/.setfp directives are mandatory: Delphi uses the
// ARM EHABI unwinder, so an exception raised within FakeCall must be able to
// unwind through this frame
// - everything is ARM (not Thumb) code: the VMT entries are then plain even
// addresses, and the linker adds the interworking to/from the Delphi code

    .syntax unified
    .arm
    .fpu vfpv3-d16

    .text
    .p2align 2
    .global armfakestub
    .type armfakestub, %function

armfakestub:
    .fnstart
    push {r11, lr}
    .save {r11, lr}
    mov r11, sp
    .setfp r11, sp
    sub sp, sp, #128                // SP = entry - 136, still 8-byte aligned

    str r12, [sp, #84]              // MethodIndex at +80
    add r12, sp, #4                 // R12 = TFakeCallStack
    stm r12, {r0-r3}
    add r1, r12, #16
    vstm r1, {d0-d7}

    mov r1, r12                     // R0 = Instance, R1 = Stack
    bl  fakecall

    // FakeCall stored any float result into FPRegs[D0]
    vldr d0, [sp, #20]

    mov sp, r11
    pop {r11, pc}
    .fnend
    .size armfakestub, .-armfakestub


// the VMT entry points, one per method index - compiled in instead of JITted,
// as for aarch64 (and Android may forbid rwx pages in the future)
// - the count has to match MAX_METHOD_COUNT of mormot.core.interfaces (a
//   {$message fatal} there guards the other direction)
// - entry i is armfakethunks + i * 8: method index into R12, then on to
//   armfakestub (b reaches +-32 MB, and both live in the same section)
//
// armfakethunkbase returns the address of that table, as a function - Delphi
// wraps an "external ... name" procedure in a thunk of its own, so taking
// @armfakethunks in Pascal yields the wrapper, not the table
// - it sits right before the table: ADR only encodes small offsets in ARM mode

    .p2align 2
    .global armfakethunkbase
    .type armfakethunkbase, %function

armfakethunkbase:
    .fnstart
    adr r0, .Lfakethunks            // PC + 8 = the table itself
    bx  lr
    .fnend
    .size armfakethunkbase, .-armfakethunkbase

    .global armfakethunks
    .type armfakethunks, %function

armfakethunks:
.Lfakethunks:
    .fnstart
    .cantunwind                     // reached by b, never a return address
    .set fakethunkindex, 0
    .rept 128
    mov r12, #fakethunkindex
    b   armfakestub
    .set fakethunkindex, fakethunkindex + 1
    .endr
    .fnend
    .size armfakethunks, .-armfakethunks


// armcallmethod(var Args: TCallMethodArgs) - used by RawExecute() to call an
// interface method implementation with the Delphi ARM calling convention
// TCallMethodArgs layout (ABIA32, default $A8 alignment):
//   +0   StackSize   number of 4-byte stack slots
//   +4   StackAddr   first slot to be copied onto the stack
//   +8   method      code address to call
//   +12  ParamRegs   R0..R3
//   +32  FPRegs      D0..D7 (8-byte aligned)
//   +96  res64       ordinal or float result
//   +104 resKind     TInterfaceMethodValueType (1 byte)
// imvDouble = 8, imvDateTime = 9 are returned in D0 - anything else in R0:R1

    .p2align 2
    .global armcallmethod
    .type armcallmethod, %function

armcallmethod:
    .fnstart
    push {r4, r5, r11, lr}
    .save {r4, r5, r11, lr}
    add r11, sp, #8
    .setfp r11, sp, #8
    mov r4, r0

    // reserve and fill the stack arguments, keeping SP 8-byte aligned
    ldr r0, [r4, #0]
    lsl r1, r0, #2
    add r1, r1, #7
    bic r1, r1, #7
    sub sp, sp, r1
    cmp r0, #0
    beq .Lload_regs
    mov r1, sp
    ldr r2, [r4, #4]
.Lstack_loop:
    ldr r3, [r2], #4
    str r3, [r1], #4
    subs r0, r0, #1
    bne .Lstack_loop

.Lload_regs:
    add r5, r4, #32
    vldm r5, {d0-d7}
    ldr r5, [r4, #8]
    ldr r0, [r4, #12]
    ldr r1, [r4, #16]
    ldr r2, [r4, #20]
    ldr r3, [r4, #24]
    blx r5

    // store the result
    str r0, [r4, #96]
    str r1, [r4, #100]
    ldrb r2, [r4, #104]
    cmp r2, #8
    beq .Lfloat_result
    cmp r2, #9
    bne .Ldone
.Lfloat_result:
    vstr d0, [r4, #96]
.Ldone:
    sub sp, r11, #8
    pop {r4, r5, r11, pc}
    .fnend
    .size armcallmethod, .-armcallmethod

    .section .note.GNU-stack,"",%progbits
