// interface stubs for the Delphi LLVM aarch64 compilers (Android and iOS):
//   aarch64fakestub   = TInterfacedObjectFake methods (SOA client side)
//   aarch64callmethod = CallMethod() for RawExecute (SOA server side, callbacks)
//   aarch64fakethunks = the VMT entry points, one per method index
// counterpart of delphi-linux-x64.s, used by mormot.core.interfaces.pas
// when DELPHI_AARCH64_RESULT_X8 is defined
//
// build with the clang of the NDK shipped with the RAD Studio Android SDK
// (see compile.sh - the same source yields ELF for Android, Mach-O for iOS)
//
// aarch64fakestub is entered from one of the thunks below, with
//   X16 = method index, X0 = interface instance,
//   X1..X7 / D0..D7 / stack = parameters, X8 = by-ref result (AAPCS64)
// it builds a TFakeCallStack (ABIA64 layout) on the stack:
//   +0   ParamRegs   X0..X7
//   +64  FPRegs      D0..D7
//   +128 MethodIndex (X16)
//   +136 Frame       = X8, the indirect result pointer used by Delphi
//   +144 Ret         = saved X29
//   +152 DummyStack  = saved X30
//   +160 Stack       = caller stack arguments
// then calls fakecall(Instance = X0, Stack = SP), i.e. the exported
// TInterfacedObjectFakeRaw.FakeCall redirection, which returns the ordinal
// result in X0 and stores any float result into FPRegs[D0]
//
// NOTE: the .cfi_* directives are mandatory: Delphi uses DWARF exceptions, so
// an exception raised within FakeCall must be able to unwind through this frame


#if defined(__MACH__)
  // Mach-O prefixes every symbol with an underscore - as does the Delphi iOS
  // compiler - knows no ELF symbol attributes, and starts local labels with L
  #define SYM(name) _##name
  #define FUNC_TYPE(name)
  #define FUNC_SIZE(name)
  #define LOCAL(name) L##name
#else
  #define SYM(name) name
  #define FUNC_TYPE(name) .type name, %function
  #define FUNC_SIZE(name) .size name, .-name
  #define LOCAL(name) .L##name
#endif

    .text
    .p2align 2
    .global SYM(aarch64fakestub)
    FUNC_TYPE(SYM(aarch64fakestub))

SYM(aarch64fakestub):
    .cfi_startproc
    stp x29, x30, [sp, #-16]!
    .cfi_def_cfa_offset 16
    .cfi_offset x29, -16
    .cfi_offset x30, -8
    mov x29, sp
    .cfi_def_cfa x29, 16
    sub sp, sp, #144

    stp x0, x1, [sp, #0]
    stp x2, x3, [sp, #16]
    stp x4, x5, [sp, #32]
    stp x6, x7, [sp, #48]
    stp d0, d1, [sp, #64]
    stp d2, d3, [sp, #80]
    stp d4, d5, [sp, #96]
    stp d6, d7, [sp, #112]
    stp x16, x8, [sp, #128]

    mov x1, sp
    bl  SYM(fakecall)

    // FakeCall stored any float result into FPRegs[D0]
    ldr d0, [sp, #64]

    mov sp, x29
    ldp x29, x30, [sp], #16
    .cfi_def_cfa sp, 0
    .cfi_restore x29
    .cfi_restore x30
    ret
    .cfi_endproc
    FUNC_SIZE(SYM(aarch64fakestub))


// the VMT entry points, one per method index - the Pascal side used to JIT these
// two instructions per index into an anonymous rwx page, which iOS kills on sight
// ("KERN_PROTECTION_FAILURE ... CODESIGNING, Invalid Page"): a normal app may not
// run code it wrote itself. Compiled in, they cost 8 bytes each.
// - the count has to match MAX_METHOD_COUNT of mormot.core.interfaces (a
//   {$message fatal} there guards the other direction)
// - entry i is aarch64fakethunks + i * 8, and does what the JITted stub did:
//   method index into X16, then on to aarch64fakestub (b reaches +-128 MB, and
//   both live in the same section)

    .p2align 2
    .global SYM(aarch64fakethunks)
    FUNC_TYPE(SYM(aarch64fakethunks))

SYM(aarch64fakethunks):
    .set fakethunkindex, 0
    .rept 128
    movz x16, #fakethunkindex
    b    SYM(aarch64fakestub)
    .set fakethunkindex, fakethunkindex + 1
    .endr
    FUNC_SIZE(SYM(aarch64fakethunks))

// the address of that table, as a function - Delphi wraps an "external ... name"
// procedure in a thunk of its own, so taking @aarch64fakethunks in Pascal yields
// the wrapper, not the table (which had the VMT point into unrelated code)

    .p2align 2
    .global SYM(aarch64fakethunkbase)
    FUNC_TYPE(SYM(aarch64fakethunkbase))

SYM(aarch64fakethunkbase):
    adr x0, SYM(aarch64fakethunks)   // +-1 MB, and the table is right above
    ret
    FUNC_SIZE(SYM(aarch64fakethunkbase))


// aarch64callmethod(var Args: TCallMethodArgs) - used by RawExecute() to call
// an interface method implementation with the Delphi aarch64 calling convention
// TCallMethodArgs layout (ABIA64 + DELPHI_AARCH64_RESULT_X8):
//   +0   StackSize   number of 8-byte stack slots (always even)
//   +8   StackAddr   first slot to be copied onto the stack
//   +16  method      code address to call
//   +24  ParamRegs   X0..X7
//   +88  FPRegs      D0..D7
//   +152 res64       ordinal or float result
//   +160 resKind     TInterfaceMethodValueType (1 byte)
//   +168 ResultX8    by-ref result pointer, loaded into X8 (AAPCS64)
// imvDouble = 8, imvDateTime = 9 are returned in D0 - anything else in X0

    .global SYM(aarch64callmethod)
    FUNC_TYPE(SYM(aarch64callmethod))

SYM(aarch64callmethod):
    .cfi_startproc
    stp x29, x30, [sp, #-16]!
    .cfi_def_cfa_offset 16
    .cfi_offset x29, -16
    .cfi_offset x30, -8
    mov x29, sp
    .cfi_def_cfa x29, 16
    stp x19, x20, [sp, #-16]!
    .cfi_offset x19, -32
    .cfi_offset x20, -24
    mov x19, x0

    // reserve and fill the stack arguments (StackSize is even -> 16-aligned)
    ldr x2, [x19, #0]
    lsl x3, x2, #3
    sub sp, sp, x3
    cbz x2, LOCAL(load_regs)
    mov x3, sp
    ldr x4, [x19, #8]
LOCAL(stack_loop):
    ldp x5, x6, [x4], #16
    stp x5, x6, [x3], #16
    subs x2, x2, #2
    b.hi LOCAL(stack_loop)

LOCAL(load_regs):
    ldp x0, x1, [x19, #24]
    ldp x2, x3, [x19, #40]
    ldp x4, x5, [x19, #56]
    ldp x6, x7, [x19, #72]
    ldp d0, d1, [x19, #88]
    ldp d2, d3, [x19, #104]
    ldp d4, d5, [x19, #120]
    ldp d6, d7, [x19, #136]
    ldr x8, [x19, #168]
    ldr x16, [x19, #16]
    blr x16

    // store the result
    str x0, [x19, #152]
    ldrb w9, [x19, #160]
    cmp w9, #8
    b.eq LOCAL(float_result)
    cmp w9, #9
    b.ne LOCAL(done)
LOCAL(float_result):
    str d0, [x19, #152]
LOCAL(done):
    sub sp, x29, #16
    ldp x19, x20, [sp], #16
    .cfi_restore x19
    .cfi_restore x20
    ldp x29, x30, [sp], #16
    .cfi_def_cfa sp, 0
    .cfi_restore x29
    .cfi_restore x30
    ret
    .cfi_endproc
    FUNC_SIZE(SYM(aarch64callmethod))
