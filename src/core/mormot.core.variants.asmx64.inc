{ Delphi Win64 JSON number conversion. Preserve the portable Integer/Int64/
  Currency/Double classification and negative-mantissa floating-point rounding.
  Huge exponents use the portable implementation.
  Byte loads and volatile registers only; a fallback restores the arguments. }

function GetNumericVariantFromJson(Json: PUtf8Char; var Value: TVarData;
  AllowVarDouble: boolean): PUtf8Char;
asm .noframe
        movq    xmm2, rcx
        movzx   r8d, r8b
        shl     r8d, 3 // flags: negative=1, negative exponent=2, double=8, int64=16, discarded=32
        xor     r10d, r10d
        test    rcx, rcx
        jz      @invalid
        movzx   eax, byte ptr [rcx]
        cmp     eax, '-'
        jne     @first
        inc     rcx
        or      r8d, 1
        movzx   eax, byte ptr [rcx]
@first:
        lea     r11, [rcx + 18]
        sub     eax, 48
        cmp     eax, 9
        ja      @invalid
        mov     r9d, eax
        test    eax, eax
        jnz     @integerNext
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @invalid // JSON does not permit integer leading zeroes
        cmp     eax, -48
        je      @zero
        jmp     @integerEnd
        .align 16
@integer:
        cmp     rcx, r11
        jae     @lastInteger
@integerDigit:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
@integerNext:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @integer
@integerEnd:
        cmp     eax, -2
        jne     @finish
        inc     rcx
        inc     r11 // the dot does not consume a digit
        mov     r10, rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @invalid
        test    r9, r9
        jnz     @fraction
        test    eax, eax
        jnz     @fraction
@fractionZero:
        inc     rcx
        inc     r11 // skipped fractional zeroes affect scale, not digit capacity
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        jz      @fractionZero
        cmp     eax, -48
        je      @zero
        cmp     eax, 9
        ja      @fractionEnd
        .align 16
@fraction:
        cmp     rcx, r11
        jae     @lastFraction
@fractionDigit:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @fraction
@fractionEnd:
        sub     r10, rcx
@finish:
        cmp     eax, -2
        je      @invalid // a second dot
        sub     r11, rcx
        cmp     r11, 8 // 18 - retained digits; 10 or more means Int64
        jg      @suffix
        or      r8d, 16
@suffix:
        or      eax, 32
        cmp     eax, 53 // 'e'/'E', after subtracting '0'
        je      @exponent
@parsed:
        test    r9, r9
        jle     @nonPositive
        test    r8b, 32 // discarded digits exclude exact integer/currency results
        jnz     @double
        test    r10, r10
        jnz     @currency
        test    r8b, 1
        jz      @integerType
        neg     r9
@integerType:
        mov     eax, varInteger
        test    r8b, 16
        jz      @store
        mov     eax, varInt64
@store:
        mov     word ptr [rdx], ax
        mov     qword ptr [rdx + 8], r9
        mov     rax, rcx
        ret
@zero:
        mov     word ptr [rdx], varInteger
        mov     qword ptr [rdx + 8], r9
        mov     rax, rcx
        ret
@nonPositive:
        jz      @zero
        // The only negative magnitude is abs(Low(Int64)); its Double conversion
        // already has the negative mantissa required by the portable code.
        test    r8b, 32
        jnz     @double
        test    r8b, 1
        jz      @double
        mov     eax, varInt64
        test    r10, r10
        jz      @store
        cmp     r10, -4
        jne     @double
        mov     eax, varCurrency
        jmp     @store
@currency:
        lea     rax, [r10 + 4]
        cmp     rax, 3
        ja      @double
        lea     r11, [rip + CURRENCY_FACTOR]
        movsxd  rax, dword ptr [r11 + rax * 4] // negative factor
        imul    rax, r9
        jo      @double
        test    r8b, 1
        jnz     @currencyValue
        neg     rax
@currencyValue:
        mov     r9, rax
        mov     eax, varCurrency
        jmp     @store
@double:
        test    r8b, 8
        jz      @invalid
        lea     rax, [r10 + 31]
        cmp     rax, 62
        ja      @largeScale
        lea     r11, [rip + POW10]
        movsd   xmm0, qword ptr [r11 + rax * 8]
@multiply:
        neg     r9 // match portable conversion and rounding before sign application
        pxor    xmm1, xmm1
        cvtsi2sd xmm1, r9
        mulsd   xmm0, xmm1
        test    r8b, 1
        jnz     @storeDouble
        mov     rax, $8000000000000000
        movq    xmm1, rax
        xorpd   xmm0, xmm1
@storeDouble:
        mov     word ptr [rdx], varDouble
        movsd   qword ptr [rdx + 8], xmm0
        mov     rax, rcx
        ret
@exponent:
        movq    xmm4, r11 // retain the digit count for the large-scale bound
        inc     rcx
        movzx   eax, byte ptr [rcx]
        cmp     eax, '+'
        je      @exponentSign
        cmp     eax, '-'
        jne     @exponentFirst
        or      r8d, 2
@exponentSign:
        inc     rcx
        movzx   eax, byte ptr [rcx]
@exponentFirst:
        sub     eax, 48
        cmp     eax, 9
        ja      @invalid
        mov     r11d, eax
        jmp     @exponentNext
@exponentLoop:
        cmp     r11d, 1000000
        ja      @slow
        lea     r11, [r11 + r11 * 4]
        lea     r11, [rax + r11 * 2]
@exponentNext:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @exponentLoop
        test    r8b, 2
        jz      @positiveExponent
        sub     r10, r11
        movq    r11, xmm4
        jmp     @parsed
@positiveExponent:
        add     r10, r11
        movq    r11, xmm4
        jmp     @parsed
@lastInteger:
        ja      @discardInteger
        movq    xmm5, rax
        mov     rax, 922337203685477580
        cmp     r9, rax
        movq    rax, xmm5
        ja      @overflowInteger
        jb      @integerDigit
        cmp     eax, 8
        jbe     @integerDigit
@overflowInteger:
        dec     r11 // portable scanner charges an extra digit for signed overflow
@discardInteger:
        or      r8d, 32
@discardIntegerLoop:
        inc     r10
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardIntegerLoop
        jmp     @finish
@lastFraction:
        ja      @discardFraction
        movq    xmm5, rax
        mov     rax, 922337203685477580
        cmp     r9, rax
        movq    rax, xmm5
        ja      @overflowFraction
        jb      @fractionDigit
        cmp     eax, 8
        jbe     @fractionDigit
@overflowFraction:
        dec     r11
@discardFraction:
        or      r8d, 32
        sub     r10, rcx
@discardFractionLoop:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardFractionLoop
        jmp     @finish
@largeScale:
        cmp     r10, -324
        jle     @invalid
        test    r10, r10
        js      @negativeScale
        lea     rax, [r11 + 291] // portable bound: remaining digits + 290
        cmp     r10, rax
        jge     @invalid
        mov     rax, r10
        and     eax, 31
        shr     r10, 5
        lea     r11, [rip + POW10]
        movsd   xmm0, qword ptr [r11 + r10 * 8 + 65 * 8]
        mulsd   xmm0, qword ptr [r11 + rax * 8 + 31 * 8]
        jmp     @multiply
@negativeScale:
        neg     r10
        mov     rax, r10
        and     eax, 31
        shr     r10, 5
        lea     r11, [rip + POW10]
        movsd   xmm0, qword ptr [r11 + r10 * 8 + 76 * 8]
        divsd   xmm0, qword ptr [r11 + rax * 8 + 31 * 8]
        jmp     @multiply
@invalid:
        xor     eax, eax
        ret
@slow:
        movq    rcx, xmm2
        shr     r8d, 3
        and     r8d, 1
        jmp     GetNumericVariantFromJsonPascal
end;
