{ Delphi Win64 decimal conversion. Byte loads and volatile registers only.
  Eighteen digits fit without overflow checks; further digits are accumulated
  while the signed mantissa fits, following the portable precision rules.
  Special syntax, partial error results and out-of-range scales use the portable
  implementation. No floating-point operation precedes a fallback. }
const
  NumberSignMask: UInt64 = $8000000000000000;
  NumberScale: double = 1.3407807929942597e154; // 2^512
  NumberInvScale: double = 7.458340731200207e-155; // 2^-512
  NumberMaxScaled: double = 1.3407807929942596e154; // MaxDouble * 2^-512

function GetExtended(P: PUtf8Char; out err: integer): TSynExtended;
asm .noframe
        movq    xmm2, rcx // original input for the general path
        test    rcx, rcx
        jz      @slow
        xor     r11d, r11d
        cmp     byte ptr [rcx], '-'
        jne     @positive
        inc     rcx
        inc     r11d
@positive:
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @slow
        lea     r8, [rcx + 18] // up to 18 digits need no overflow check
        xor     r9d, r9d
        .align 16
@integer:
        cmp     rcx, r8
        je      @moreInteger
@integerDigit:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @integer
        mov     r10, rcx
        cmp     eax, -2 // '.' minus '0'
        jne     @finish
        inc     rcx
        mov     r10, rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
        .align 16
@fraction:
        cmp     rcx, r8 // last permitted byte; two digits fit while rcx < r8
        jae     @lastFraction
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
@fractionOne:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @fraction
@finish:
        sub     r10, rcx
        cmp     eax, -48
        jne     @exponent
        jmp     @checkScale
@value:
        lea     r8, [rip + POW10]
        movsd   xmm0, qword ptr [r8 + r10 * 8 + 31 * 8] // POW10[frac]
        pxor    xmm1, xmm1
        cvtsi2sd xmm1, r9
@multiply:
        mulsd   xmm0, xmm1
@applySign:
        test    r11b, 1
        jz      @store
        movq    xmm3, qword ptr [rip + NumberSignMask]
        xorpd   xmm0, xmm3
@store:
        mov     dword ptr [rdx], 0
        ret
@lastFraction:
        ja      @moreFraction
        jmp     @fractionOne
@moreInteger:
        movq    xmm4, rax
        mov     rax, 922337203685477580
        cmp     r9, rax
        movq    rax, xmm4
        ja      @discardInteger
        jb      @retainInteger
        cmp     eax, 7
        ja      @discardInteger
@retainInteger:
        inc     r8
        jmp     @integerDigit
@discardInteger:
        xor     r10d, r10d
@discardIntegerLoop:
        inc     r10
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardIntegerLoop
        cmp     eax, -48
        je      @checkScale
        jmp     @exponent
@moreFraction:
        movq    xmm4, rax
        mov     rax, 922337203685477580
        cmp     r9, rax
        movq    rax, xmm4
        ja      @discardFraction
        jb      @retainFraction
        cmp     eax, 7
        ja      @discardFraction
@retainFraction:
        inc     r8
        jmp     @fractionOne
@discardFraction:
        sub     r10, rcx
@discardFractionLoop:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardFractionLoop
        cmp     eax, -48
        je      @checkScale
        jmp     @exponent
@exponent:
        or      eax, 32
        cmp     eax, 53 // 'e' or 'E', after subtracting '0' and setting bit 5
        jne     @slow
        inc     rcx
        movzx   eax, byte ptr [rcx]
        cmp     eax, '+'
        je      @exponentSign
        cmp     eax, '-'
        jne     @exponentFirst
        or      r11d, 2
@exponentSign:
        inc     rcx
        movzx   eax, byte ptr [rcx]
@exponentFirst:
        sub     eax, 48
        cmp     eax, 9
        ja      @slow
        mov     r8d, eax
        jmp     @exponentNext
@exponentLoop:
        cmp     r8d, 1000000 // the portable path validates huge exponents
        ja      @slow
        lea     r8, [r8 + r8 * 4]
        lea     r8, [rax + r8 * 2]
@exponentNext:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @exponentLoop
        cmp     eax, -48
        jne     @slow
        test    r11b, 2
        jz      @exponentPositive
        sub     r10, r8
        jmp     @checkScale
@exponentPositive:
        add     r10, r8
@checkScale:
        lea     rax, [r10 + 31]
        cmp     rax, 62
        jbe     @value
        test    r9, r9
        jnz     @largeScale
        xor     r10d, r10d
        jmp     @value
@largeScale:
        cmp     r10, -342
        jl      @slow
        cmp     r10, 308
        jg      @slow
        pxor    xmm1, xmm1
        cvtsi2sd xmm1, r9
        lea     r8, [rip + POW10]
        test    r10, r10
        js      @negativeScale
        cmp     r10, 290
        jge     @scaledPositive
        mov     rax, r10
        and     eax, 31
        shr     r10, 5
        movsd   xmm0, qword ptr [r8 + r10 * 8 + 65 * 8]
        mulsd   xmm0, qword ptr [r8 + rax * 8 + 31 * 8]
        jmp     @multiply
@negativeScale:
        cmp     r10, -324
        jg      @negativePower
        add     r10, 160
        mulsd   xmm1, qword ptr [r8 + 81 * 8] // d64 * POW10[50] = d64 * 1E-160
@negativePower:
        neg     r10
        mov     rax, r10
        and     eax, 31
        shr     r10, 5
        movsd   xmm0, qword ptr [r8 + r10 * 8 + 76 * 8]
        divsd   xmm0, qword ptr [r8 + rax * 8 + 31 * 8]
        jmp     @multiply
@scaledPositive:
        mov     rax, r10
        and     eax, 31
        shr     r10, 5
        movsd   xmm0, qword ptr [r8 + r10 * 8 + 65 * 8]
        mulsd   xmm0, qword ptr [r8 + rax * 8 + 31 * 8]
        mulsd   xmm0, qword ptr [rip + NumberInvScale]
        mulsd   xmm0, xmm1
        comisd  xmm0, qword ptr [rip + NumberMaxScaled]
        ja      @rangeError
        mulsd   xmm0, qword ptr [rip + NumberScale]
        jmp     @applySign
@rangeError:
        movsd   xmm0, xmm1 // upstream returns the unscaled mantissa with err=1
        test    r11b, 1
        jz      @rangeErrorStore
        movq    xmm3, qword ptr [rip + NumberSignMask]
        xorpd   xmm0, xmm3
@rangeErrorStore:
        mov     dword ptr [rdx], 1
        ret
@slow:
        movq    rcx, xmm2
        jmp     GetExtendedPascal
end;
