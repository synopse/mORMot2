{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86_64 assembly used by mormot.core.crypto.pas
}

{$ifdef FPC}
  // disabled some FPC paranoid warnings
  {$WARN 7102 off : Use of +offset(%ebp) for parameters invalid here }
  {$WARN 7119 off : Exported/global symbols should be accessed via the GOT }
  {$WARN 7121 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits]" }
  {$WARN 7122 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits + $4 byte offset]" }
  {$WARN 7123 off : Check "$1: offset of memory operand is negative "$2 byte" }
{$endif FPC}

{$ifdef ASMX64}

procedure aesencryptasm(const ctxt: TAesContext; bi, bo: PWA4);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // input: rcx/rdi=TAesContext, rdx/rsi=source, r8/rdx=dest
        .noframe
{$endif}
        // rolled optimized encryption asm version by A. Bouchez
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        push    rbp
        {$ifdef WIN64ABI}
        push    rdi
        push    rsi
        mov     r15, r8
        mov     r12, rcx
        {$else}
        mov     r15, rdx
        mov     rdx, rsi
        mov     r12, rdi
        {$endif WIN64ABI}
        movzx   r13, byte ptr [r12].TAesContext.Rounds
        mov     eax, dword ptr [rdx]
        mov     ebx, dword ptr [rdx + 4H]
        mov     ecx, dword ptr [rdx + 8H]
        mov     edx, dword ptr [rdx + 0CH]
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12 + 4H]
        xor     ecx, dword ptr [r12 + 8H]
        xor     edx, dword ptr [r12 + 0CH]
        sub     r13, 1
        add     r12, 16
        lea     r14, [rip + Te0]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@round: mov     esi, eax
        mov     edi, edx
        movzx   r8d, al
        movzx   r9d, cl
        movzx   r10d, bl
        mov     r8d, dword ptr [r14 + r8*4]
        mov     r9d, dword ptr [r14 + r9*4]
        mov     r10d, dword ptr [r14 + r10*4]
        shr     esi, 16
        shr     edi, 16
        movzx   ebp, bh
        xor     r8d, dword ptr [r14 + rbp*4 + 400H]
        movzx   ebp, dh
        xor     r9d, dword ptr [r14 + rbp*4 + 400H]
        movzx   ebp, ch
        xor     r10d, dword ptr [r14 + rbp*4 + 400H]
        shr     ebx, 16
        shr     ecx, 16
        movzx   ebp, dl
        mov     edx, dword ptr [r14 + rbp*4]
        movzx   ebp, cl
        xor     r8d, dword ptr [r14 + rbp*4 + 800H]
        movzx   ebp, sil
        xor     r9d, dword ptr [r14 + rbp*4 + 800H]
        movzx   r11, dil
        movzx   eax, ah
        shr     edi, 8
        movzx   ebp, bh
        shr     esi, 8
        xor     r10d, dword ptr [r14 + r11*4 + 800H]
        xor     edx, dword ptr [r14 + rax*4 + 400H]
        xor     r8d, dword ptr [r14 + rdi*4 + 0C00H]
        xor     r9d, dword ptr [r14 + rbp*4 + 0C00H]
        xor     r10d, dword ptr [r14 + rsi*4 + 0C00H]
        movzx   ebp, bl
        xor     edx, dword ptr [r14 + rbp*4 + 800H]
        mov     rbx, r10
        mov     rax, r8
        movzx   ebp, ch
        xor     edx, dword ptr [r14 + rbp*4 + 0C00H]
        mov     rcx, r9
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12 + 4H]
        xor     ecx, dword ptr [r12 + 8H]
        xor     edx, dword ptr [r12 + 0CH]
        add     r12, 16
        sub     r13, 1
        jnz     @round
        lea     r9, [rip + SBox]
        movzx   r8, al
        movzx   r14, byte ptr [r9 + r8]
        movzx   edi, bh
        movzx   r8, byte ptr [r9 + rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rcx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 24
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12]
        mov     dword ptr [r15], r14d
        movzx   r8, bl
        movzx   r14, byte ptr [r9 + r8]
        movzx   edi, ch
        movzx   r8, byte ptr [r9 + rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 24
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12 + 4H]
        mov     dword ptr [r15 + 4H], r14d
        movzx   r8, cl
        movzx   r14, byte ptr [r9 + r8]
        movzx   edi, dh
        movzx   r8, byte ptr [r9 + rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rbx
        shr     r11, 24
        movzx   r8, byte ptr [r9 + r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12 + 8H]
        mov     dword ptr [r15 + 8H], r14d
        and     rdx, 0FFH
        movzx   r14, byte ptr [r9 + rdx]
        movzx   eax, ah
        movzx   r8, byte ptr [r9 + rax]
        shl     r8d, 8
        xor     r14d, r8d
        shr     rbx, 16
        and     rbx, 0FFH
        movzx   r8, byte ptr [r9 + rbx]
        shl     r8d, 16
        xor     r14d, r8d
        shr     rcx, 24
        movzx   r8, byte ptr [r9 + rcx]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12 + 0CH]
        mov     dword ptr [r15 + 0CH], r14d
        {$ifdef WIN64ABI}
        pop     rsi
        pop     rdi
        {$endif WIN64ABI}
        pop     rbp
        pop     rbx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
end;


// optimized unrolled version from Intel's sha256_sse4.asm
//  Original code is released as Copyright (c) 2012, Intel Corporation
var
  K256AlignedStore: RawByteString;
  K256Aligned: pointer; // movaps + paddd do expect 16 bytes alignment

const
  STACK_SIZE = 32 {$ifndef SYSVABI} + 7 * 16 {$endif};

procedure sha256_sse4(var input_data; var digest; num_blks: PtrUInt);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // rcx=input_data rdx=digest r8=num_blks (Linux: rdi,rsi,rdx)
        .noframe
{$endif FPC}
        push    rbx
        {$ifdef SYSVABI}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
        {$else}
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif SYSVABI}
        push    rbp
        push    r13
        push    r14
        push    r15
        sub     rsp, STACK_SIZE
        {$ifndef SYSVABI}
        movaps  [rsp + 20H], xmm6    // manual .PUSHNV for FPC compatibility
        movaps  [rsp + 30H], xmm7
        movaps  [rsp + 40H], xmm8
        movaps  [rsp + 50H], xmm9
        movaps  [rsp + 60H], xmm10
        movaps  [rsp + 70H], xmm11
        movaps  [rsp + 80H], xmm12
        {$endif SYSVABI}
        shl     r8, 6
        je      @done
        add     r8, rcx
        mov     [rsp], r8
        mov     eax, [rdx]
        mov     ebx, [rdx + 4H]
        mov     edi, [rdx + 8H]
        mov     esi, [rdx + 0CH]
        mov     r8d, [rdx + 10H]
        mov     r9d, [rdx + 14H]
        mov     r10d, [rdx + 18H]
        mov     r11d, [rdx + 1CH]
        movaps  xmm12, [rip + @flip]
        movaps  xmm10, [rip + @00BA]
        movaps  xmm11, [rip + @DC00]
@loop0: mov     rbp, [rip + K256Aligned]
        movups  xmm4, [rcx]
        pshufb  xmm4, xmm12
        movups  xmm5, [rcx + 10h]
        pshufb  xmm5, xmm12
        movups  xmm6, [rcx + 20h]
        pshufb  xmm6, xmm12
        movups  xmm7, [rcx + 30h]
        pshufb  xmm7, xmm12
        mov     [rsp + 8h], rcx
        mov     rcx, 3
@loop1: movaps  xmm9, [rbp]
        paddd   xmm9, xmm4
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm7
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        palignr xmm0, xmm6, 04h
        ror     r14d, 9
        xor     r13d, r8d
        mov     r15d, r9d
        ror     r13d, 5
        movaps  xmm1, xmm5
        xor     r14d, eax
        xor     r15d, r10d
        paddd   xmm0, xmm4
        xor     r13d, r8d
        and     r15d, r8d
        ror     r14d, 11
        palignr xmm1, xmm4, 04h
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        pslld   xmm1, 25
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        psrld   xmm2, 7
        and     r13d, ebx
        add     r11d, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     r11d, r13d
        movaps  xmm2, xmm3
        mov     r13d, esi
        mov     r14d, r11d
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, esi
        mov     r15d, r8d
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, esi
        and     r15d, esi
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, r11d
        xor     r15d, r9d
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        pxor    xmm1, xmm8
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        pshufd  xmm2, xmm7, 0fah
        and     r13d, eax
        add     r10d, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     r10d, r13d
        movaps  xmm3, xmm2
        mov     r13d, edi
        mov     r14d, r10d
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, r8d
        psrlq   xmm3, 19
        xor     r13d, edi
        and     r15d, edi
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, r10d
        xor     r15d, r8d
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        pshufb  xmm8, xmm10
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        paddd   xmm0, xmm8
        and     r13d, r11d
        add     r9d, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     r9d, r13d
        movaps  xmm3, xmm2
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        movaps  xmm4, xmm2
        ror     r14d, 9
        xor     r13d, ebx
        mov     r15d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, r9d
        xor     r15d, esi
        psrlq   xmm3, 19
        xor     r13d, ebx
        and     r15d, ebx
        ror     r14d, 11
        psrld   xmm4, 10
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm4, xmm2
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        pshufb  xmm4, xmm11
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        paddd   xmm4, xmm0
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        movaps  xmm9, [rbp + 10h]
        paddd   xmm9, xmm5
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm4
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        palignr xmm0, xmm7, 04h
        ror     r14d, 9
        xor     r13d, eax
        mov     r15d, ebx
        ror     r13d, 5
        movaps  xmm1, xmm6
        xor     r14d, r8d
        xor     r15d, edi
        paddd   xmm0, xmm5
        xor     r13d, eax
        and     r15d, eax
        ror     r14d, 11
        palignr xmm1, xmm5, 04h
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        pslld   xmm1, 25
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        psrld   xmm2, 7
        and     r13d, r9d
        add     esi, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     esi, r13d
        movaps  xmm2, xmm3
        mov     r13d, r11d
        mov     r14d, esi
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, r11d
        mov     r15d, eax
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, r11d
        and     r15d, r11d
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, esi
        xor     r15d, ebx
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        pxor    xmm1, xmm8
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        pshufd  xmm2, xmm4, 0fah
        and     r13d, r8d
        add     edi, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     edi, r13d
        movaps  xmm3, xmm2
        mov     r13d, r10d
        mov     r14d, edi
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, eax
        psrlq   xmm3, 19
        xor     r13d, r10d
        and     r15d, r10d
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, edi
        xor     r15d, eax
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        pshufb  xmm8, xmm10
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        paddd   xmm0, xmm8
        and     r13d, esi
        add     ebx, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     ebx, r13d
        movaps  xmm3, xmm2
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        movaps  xmm5, xmm2
        ror     r14d, 9
        xor     r13d, r9d
        mov     r15d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, ebx
        xor     r15d, r11d
        psrlq   xmm3, 19
        xor     r13d, r9d
        and     r15d, r9d
        ror     r14d, 11
        psrld   xmm5, 10
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm5, xmm2
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        pshufb  xmm5, xmm11
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        paddd   xmm5, xmm0
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        movaps  xmm9, [rbp + 20h]
        paddd   xmm9, xmm6
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm5
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        palignr xmm0, xmm4, 04h
        ror     r14d, 9
        xor     r13d, r8d
        mov     r15d, r9d
        ror     r13d, 5
        movaps  xmm1, xmm7
        xor     r14d, eax
        xor     r15d, r10d
        paddd   xmm0, xmm6
        xor     r13d, r8d
        and     r15d, r8d
        ror     r14d, 11
        palignr xmm1, xmm6, 04h
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        pslld   xmm1, 25
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        psrld   xmm2, 7
        and     r13d, ebx
        add     r11d, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     r11d, r13d
        movaps  xmm2, xmm3
        mov     r13d, esi
        mov     r14d, r11d
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, esi
        mov     r15d, r8d
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, esi
        and     r15d, esi
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, r11d
        xor     r15d, r9d
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        pxor    xmm1, xmm8
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        pshufd  xmm2, xmm5, 0fah
        and     r13d, eax
        add     r10d, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     r10d, r13d
        movaps  xmm3, xmm2
        mov     r13d, edi
        mov     r14d, r10d
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, r8d
        psrlq   xmm3, 19
        xor     r13d, edi
        and     r15d, edi
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, r10d
        xor     r15d, r8d
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        pshufb  xmm8, xmm10
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        paddd   xmm0, xmm8
        and     r13d, r11d
        add     r9d, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     r9d, r13d
        movaps  xmm3, xmm2
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        movaps  xmm6, xmm2
        ror     r14d, 9
        xor     r13d, ebx
        mov     r15d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, r9d
        xor     r15d, esi
        psrlq   xmm3, 19
        xor     r13d, ebx
        and     r15d, ebx
        ror     r14d, 11
        psrld   xmm6, 10
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm6, xmm2
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        pshufb  xmm6, xmm11
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        paddd   xmm6, xmm0
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        movaps  xmm9, [rbp + 30h]
        paddd   xmm9, xmm7
        movaps  [rsp + 10h], xmm9
        add     rbp, 64
        movaps  xmm0, xmm6
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        palignr xmm0, xmm5, 04h
        ror     r14d, 9
        xor     r13d, eax
        mov     r15d, ebx
        ror     r13d, 5
        movaps  xmm1, xmm4
        xor     r14d, r8d
        xor     r15d, edi
        paddd   xmm0, xmm7
        xor     r13d, eax
        and     r15d, eax
        ror     r14d, 11
        palignr xmm1, xmm7, 04h
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        pslld   xmm1, 25
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        psrld   xmm2, 7
        and     r13d, r9d
        add     esi, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     esi, r13d
        movaps  xmm2, xmm3
        mov     r13d, r11d
        mov     r14d, esi
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, r11d
        mov     r15d, eax
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, r11d
        and     r15d, r11d
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, esi
        xor     r15d, ebx
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        pxor    xmm1, xmm8
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        pshufd  xmm2, xmm6, 0fah
        and     r13d, r8d
        add     edi, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     edi, r13d
        movaps  xmm3, xmm2
        mov     r13d, r10d
        mov     r14d, edi
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, eax
        psrlq   xmm3, 19
        xor     r13d, r10d
        and     r15d, r10d
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, edi
        xor     r15d, eax
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        pshufb  xmm8, xmm10
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        paddd   xmm0, xmm8
        and     r13d, esi
        add     ebx, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     ebx, r13d
        movaps  xmm3, xmm2
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        movaps  xmm7, xmm2
        ror     r14d, 9
        xor     r13d, r9d
        mov     r15d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, ebx
        xor     r15d, r11d
        psrlq   xmm3, 19
        xor     r13d, r9d
        and     r15d, r9d
        ror     r14d, 11
        psrld   xmm7, 10
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm7, xmm2
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        pshufb  xmm7, xmm11
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        paddd   xmm7, xmm0
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        sub     rcx, 1
        jne     @loop1
        mov     rcx, 2
@loop2: paddd   xmm4, [rbp]
        movaps  [rsp + 10h], xmm4
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        xor     r13d, r8d
        ror     r14d, 9
        mov     r15d, r9d
        xor     r14d, eax
        ror     r13d, 5
        xor     r15d, r10d
        xor     r13d, r8d
        ror     r14d, 11
        and     r15d, r8d
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 10h]
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        and     r13d, ebx
        add     r11d, r14d
        or      r13d, r15d
        add     r11d, r13d
        mov     r13d, esi
        ror     r13d, 14
        mov     r14d, r11d
        xor     r13d, esi
        ror     r14d, 9
        mov     r15d, r8d
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        xor     r13d, esi
        ror     r14d, 11
        and     r15d, esi
        xor     r14d, r11d
        ror     r13d, 6
        xor     r15d, r9d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 14h]
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        and     r13d, eax
        add     r10d, r14d
        or      r13d, r15d
        add     r10d, r13d
        mov     r13d, edi
        ror     r13d, 14
        mov     r14d, r10d
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        xor     r15d, r8d
        xor     r13d, edi
        ror     r14d, 11
        and     r15d, edi
        xor     r14d, r10d
        ror     r13d, 6
        xor     r15d, r8d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        and     r13d, r11d
        add     r9d, r14d
        or      r13d, r15d
        add     r9d, r13d
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        xor     r13d, ebx
        ror     r14d, 9
        mov     r15d, edi
        xor     r14d, r9d
        ror     r13d, 5
        xor     r15d, esi
        xor     r13d, ebx
        ror     r14d, 11
        and     r15d, ebx
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 1ch]
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        paddd   xmm5, [rbp + 10h]
        movaps  [rsp + 10h], xmm5
        add     rbp, 32
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        xor     r13d, eax
        ror     r14d, 9
        mov     r15d, ebx
        xor     r14d, r8d
        ror     r13d, 5
        xor     r15d, edi
        xor     r13d, eax
        ror     r14d, 11
        and     r15d, eax
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 10h]
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        and     r13d, r9d
        add     esi, r14d
        or      r13d, r15d
        add     esi, r13d
        mov     r13d, r11d
        ror     r13d, 14
        mov     r14d, esi
        xor     r13d, r11d
        ror     r14d, 9
        mov     r15d, eax
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        xor     r13d, r11d
        ror     r14d, 11
        and     r15d, r11d
        xor     r14d, esi
        ror     r13d, 6
        xor     r15d, ebx
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 14h]
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        and     r13d, r8d
        add     edi, r14d
        or      r13d, r15d
        add     edi, r13d
        mov     r13d, r10d
        ror     r13d, 14
        mov     r14d, edi
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        xor     r15d, eax
        xor     r13d, r10d
        ror     r14d, 11
        and     r15d, r10d
        xor     r14d, edi
        ror     r13d, 6
        xor     r15d, eax
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        and     r13d, esi
        add     ebx, r14d
        or      r13d, r15d
        add     ebx, r13d
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        xor     r13d, r9d
        ror     r14d, 9
        mov     r15d, r10d
        xor     r14d, ebx
        ror     r13d, 5
        xor     r15d, r11d
        xor     r13d, r9d
        ror     r14d, 11
        and     r15d, r9d
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 1ch]
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        movaps  xmm4, xmm6
        movaps  xmm5, xmm7
        dec     rcx
        jne     @loop2
        add     eax, [rdx]
        mov     [rdx], eax
        add     ebx, [rdx + 4H]
        add     edi, [rdx + 8H]
        add     esi, [rdx + 0CH]
        add     r8d, [rdx + 10H]
        add     r9d, [rdx + 14H]
        add     r10d, [rdx + 18H]
        add     r11d, [rdx + 1CH]
        mov     [rdx + 4H], ebx
        mov     [rdx + 8H], edi
        mov     [rdx + 0CH], esi
        mov     [rdx + 10H], r8d
        mov     [rdx + 14H], r9d
        mov     [rdx + 18H], r10d
        mov     [rdx + 1CH], r11d
        mov     rcx, [rsp + 8H]
        add     rcx, 64
        cmp     rcx, [rsp]
        jne     @loop0
@done: {$ifndef SYSVABI}
        movaps  xmm6, [rsp + 20H]
        movaps  xmm7, [rsp + 30H]
        movaps  xmm8, [rsp + 40H]
        movaps  xmm9, [rsp + 50H]
        movaps  xmm10, [rsp + 60H]
        movaps  xmm11, [rsp + 70H]
        movaps  xmm12, [rsp + 80H]
        {$endif SYSVABI}
        add     rsp, STACK_SIZE
        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        {$ifndef SYSVABI}
        pop     rdi
        pop     rsi
        {$endif SYSVABI}
        pop     rbx
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@flip:  dq      $0405060700010203
        dq      $0C0D0E0F08090A0B
@00BA:  dq      $0B0A090803020100
        dq      $FFFFFFFFFFFFFFFF
@DC00:  dq      $FFFFFFFFFFFFFFFF
        dq      $0B0A090803020100
end;

// Synopse's x64 asm, optimized for both in+out-order pipelined CPUs
procedure KeccakPermutationKernel(B, A, C: Pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifndef WIN64ABI}  // input: rcx=B, rdx=A, r8=C (Linux: rdi,rsi,rdx)
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64ABI}
        push    rbx
        push    r12
        push    r13
        push    r14
        add     rdx, 128
        add     rcx, 128
        // theta
        mov     r10, [rdx - 128]
        mov     r11, [rdx - 120]
        mov     r12, [rdx - 112]
        mov     r13, [rdx - 104]
        mov     r14, [rdx - 96]
        xor     r10, [rdx - 88]
        xor     r11, [rdx - 80]
        xor     r12, [rdx - 72]
        xor     r13, [rdx - 64]
        xor     r14, [rdx - 56]
        xor     r10, [rdx - 48]
        xor     r11, [rdx - 40]
        xor     r12, [rdx - 32]
        xor     r13, [rdx - 24]
        xor     r14, [rdx - 16]
        xor     r10, [rdx - 8]
        xor     r11, [rdx]
        xor     r12, [rdx + 8]
        xor     r13, [rdx + 16]
        xor     r14, [rdx + 24]
        xor     r10, [rdx + 32]
        xor     r11, [rdx + 40]
        xor     r12, [rdx + 48]
        xor     r13, [rdx + 56]
        xor     r14, [rdx + 64]
        mov     [r8], r10
        mov     [r8 + 8], r11
        mov     [r8 + 16], r12
        mov     [r8 + 24], r13
        mov     [r8 + 32], r14
        rol     r10, 1
        rol     r11, 1
        rol     r12, 1
        rol     r13, 1
        rol     r14, 1
        xor     r10, [r8 + 24]
        xor     r11, [r8 + 32]
        xor     r12, [r8]
        xor     r13, [r8 + 8]
        xor     r14, [r8 + 16]
        // rho pi
        mov     rax, [rdx - 128]
        mov     r8, [rdx - 80]
        mov     r9, [rdx - 32]
        mov     rbx, [rdx + 16]
        xor     rax, r11
        xor     r8, r12
        xor     r9, r13
        xor     rbx, r14
        rol     r8, 44
        rol     r9, 43
        rol     rbx, 21
        mov     [rcx - 128], rax
        mov     [rcx - 120], r8
        mov     [rcx - 112], r9
        mov     [rcx - 104], rbx
        mov     rax, [rdx + 64]
        mov     r8, [rdx - 104]
        mov     r9, [rdx - 56]
        mov     rbx, [rdx - 48]
        xor     rax, r10
        xor     r8, r14
        xor     r9, r10
        xor     rbx, r11
        rol     rax, 14
        rol     r8, 28
        rol     r9, 20
        rol     rbx, 3
        mov     [rcx - 96], rax
        mov     [rcx - 88], r8
        mov     [rcx - 80], r9
        mov     [rcx - 72], rbx
        mov     rax, [rdx]
        mov     r8, [rdx + 48]
        mov     r9, [rdx - 120]
        mov     rbx, [rdx - 72]
        xor     rax, r12
        xor     r8, r13
        xor     r9, r12
        xor     rbx, r13
        rol     rax, 45
        rol     r8, 61
        rol     r9, 1
        rol     rbx, 6
        mov     [rcx - 64], rax
        mov     [rcx - 56], r8
        mov     [rcx - 48], r9
        mov     [rcx - 40], rbx
        mov     rax, [rdx - 24]
        mov     r8, [rdx + 24]
        mov     r9, [rdx + 32]
        mov     rbx, [rdx - 96]
        xor     rax, r14
        xor     r8, r10
        xor     r9, r11
        xor     rbx, r10
        rol     rax, 25
        rol     r8, 8
        rol     r9, 18
        rol     rbx, 27
        mov     [rcx - 32], rax
        mov     [rcx - 24], r8
        mov     [rcx - 16], r9
        mov     [rcx - 8], rbx
        mov     rax, [rdx - 88]
        mov     r8, [rdx - 40]
        mov     r9, [rdx + 8]
        mov     rbx, [rdx + 56]
        xor     rax, r11
        xor     r8, r12
        xor     r9, r13
        xor     rbx, r14
        rol     rax, 36
        rol     r8, 10
        rol     r9, 15
        rol     rbx, 56
        mov     [rcx], rax
        mov     [rcx + 8], r8
        mov     [rcx + 16], r9
        mov     [rcx + 24], rbx
        mov     rax, [rdx - 112]
        mov     r8, [rdx - 64]
        mov     r9, [rdx - 16]
        mov     rbx, [rdx - 8]
        xor     rax, r13
        xor     r8, r14
        xor     r9, r10
        mov     r10, [rdx + 40]
        xor     rbx, r11
        rol     rax, 62
        rol     r8, 55
        xor     r10, r12
        rol     r9, 39
        rol     rbx, 41
        mov     [rcx + 32], rax
        mov     [rcx + 40], r8
        rol     r10, 2
        mov     [rcx + 48], r9
        mov     [rcx + 56], rbx
        mov     [rcx + 64], r10
        // chi
        mov     rax, [rcx - 120]
        mov     r8, [rcx - 112]
        mov     r9, [rcx - 104]
        mov     r10, [rcx - 96]
        mov     r11, [rcx - 128]
        mov     r12, [rcx - 80]
        mov     r13, [rcx - 72]
        mov     r14, [rcx - 64]
        mov     rbx, [rcx - 56]
        not     rax
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        not     r13
        not     r14
        not     rbx
        and     rax, [rcx - 112]
        and     r8, [rcx - 104]
        and     r9, [rcx - 96]
        and     r10, [rcx - 128]
        and     r11, [rcx - 120]
        and     r12, [rcx - 72]
        and     r13, [rcx - 64]
        and     r14, [rcx - 56]
        and     rbx, [rcx - 88]
        xor     rax, [rcx - 128]
        xor     r8, [rcx - 120]
        xor     r9, [rcx - 112]
        xor     r10, [rcx - 104]
        xor     r11, [rcx - 96]
        xor     r12, [rcx - 88]
        xor     r13, [rcx - 80]
        xor     r14, [rcx - 72]
        xor     rbx, [rcx - 64]
        mov     [rdx - 128], rax
        mov     [rdx - 120], r8
        mov     [rdx - 112], r9
        mov     [rdx - 104], r10
        mov     [rdx - 96], r11
        mov     [rdx - 88], r12
        mov     [rdx - 80], r13
        mov     [rdx - 72], r14
        mov     [rdx - 64], rbx
        mov     rax, [rcx - 88]
        mov     rbx, [rcx - 40]
        mov     r8, [rcx - 32]
        mov     r9, [rcx - 24]
        mov     r10, [rcx - 16]
        mov     r11, [rcx - 48]
        mov     r12, [rcx]
        mov     r13, [rcx + 8]
        mov     r14, [rcx + 16]
        not     rax
        not     rbx
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        not     r13
        not     r14
        and     rax, [rcx - 80]
        and     rbx, [rcx - 32]
        and     r8, [rcx - 24]
        and     r9, [rcx - 16]
        and     r10, [rcx - 48]
        and     r11, [rcx - 40]
        and     r12, [rcx + 8]
        and     r13, [rcx + 16]
        and     r14, [rcx + 24]
        xor     rax, [rcx - 56]
        xor     rbx, [rcx - 48]
        xor     r8, [rcx - 40]
        xor     r9, [rcx - 32]
        xor     r10, [rcx - 24]
        xor     r11, [rcx - 16]
        xor     r12, [rcx - 8]
        xor     r13, [rcx]
        xor     r14, [rcx + 8]
        mov     [rdx - 56], rax
        mov     [rdx - 48], rbx
        mov     [rdx - 40], r8
        mov     [rdx - 32], r9
        mov     [rdx - 24], r10
        mov     [rdx - 16], r11
        mov     [rdx - 8], r12
        mov     [rdx], r13
        mov     [rdx + 8], r14
        mov     rax, [rcx + 24]
        mov     rbx, [rcx - 8]
        mov     r8, [rcx + 40]
        mov     r9, [rcx + 48]
        mov     r10, [rcx + 56]
        mov     r11, [rcx + 64]
        mov     r12, [rcx + 32]
        not     rax
        not     rbx
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        and     rax, [rcx - 8]
        and     rbx, [rcx]
        and     r8, [rcx + 48]
        and     r9, [rcx + 56]
        and     r10, [rcx + 64]
        and     r11, [rcx + 32]
        and     r12, [rcx + 40]
        xor     rax, [rcx + 16]
        xor     rbx, [rcx + 24]
        xor     r8, [rcx + 32]
        xor     r9, [rcx + 40]
        xor     r10, [rcx + 48]
        xor     r11, [rcx + 56]
        xor     r12, [rcx + 64]
        mov     [rdx + 16], rax
        mov     [rdx + 24], rbx
        mov     [rdx + 32], r8
        mov     [rdx + 40], r9
        mov     [rdx + 48], r10
        mov     [rdx + 56], r11
        mov     [rdx + 64], r12
        pop     r14
        pop     r13
        pop     r12
        pop     rbx
end;

{$endif ASMX64}


procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // W=rcx Buf=rdx
  .noframe
{$endif}
        {$ifndef WIN64ABI}
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64ABI}
        mov     rax, rcx
        push    rsi
        push    rdi
        push    rbx
        mov     rsi, rax
        // part 1: W[i]:= RB(TW32Buf(Buf)[i])
        mov     eax, [rdx]
        mov     ebx, [rdx + 4]
        bswap   eax
        bswap   ebx
        mov     [rsi], eax
        mov     [rsi + 4], ebx
        mov     eax, [rdx + 8]
        mov     ebx, [rdx + 12]
        bswap   eax
        bswap   ebx
        mov     [rsi + 8], eax
        mov     [rsi + 12], ebx
        mov     eax, [rdx + 16]
        mov     ebx, [rdx + 20]
        bswap   eax
        bswap   ebx
        mov     [rsi + 16], eax
        mov     [rsi + 20], ebx
        mov     eax, [rdx + 24]
        mov     ebx, [rdx + 28]
        bswap   eax
        bswap   ebx
        mov     [rsi + 24], eax
        mov     [rsi + 28], ebx
        mov     eax, [rdx + 32]
        mov     ebx, [rdx + 36]
        bswap   eax
        bswap   ebx
        mov     [rsi + 32], eax
        mov     [rsi + 36], ebx
        mov     eax, [rdx + 40]
        mov     ebx, [rdx + 44]
        bswap   eax
        bswap   ebx
        mov     [rsi + 40], eax
        mov     [rsi + 44], ebx
        mov     eax, [rdx + 48]
        mov     ebx, [rdx + 52]
        bswap   eax
        bswap   ebx
        mov     [rsi + 48], eax
        mov     [rsi + 52], ebx
        mov     eax, [rdx + 56]
        mov     ebx, [rdx + 60]
        bswap   eax
        bswap   ebx
        mov     [rsi + 56], eax
        mov     [rsi + 60], ebx
        lea     rsi, [rsi + 64]
        // part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16])
        mov     ecx, 48
@@2:    mov     eax, [rsi - 2 * 4]    // W[i-2]
        mov     edi, [rsi - 7 * 4]    // W[i-7]
        mov     edx, eax
        mov     ebx, eax          // Sig1: RR17 xor RR19 xor SRx,10
        ror     eax, 17
        ror     edx, 19
        shr     ebx, 10
        xor     eax, edx
        xor     eax, ebx
        add     edi, eax
        mov     eax, [rsi - 15 * 4]   // W[i-15]
        mov     ebx, eax          // Sig0: RR7 xor RR18 xor SR3
        mov     edx, eax
        ror     eax, 7
        ror     edx, 18
        shr     ebx, 3
        xor     eax, edx
        xor     eax, ebx
        add     eax, edi
        add     eax, [rsi - 16 * 4]   // W[i-16]
        mov     [rsi], eax
        add     rsi, 4
        dec     ecx
        jnz     @@2
        pop     rbx
        pop     rdi
        pop     rsi
end;

procedure bswap256(s, d: PIntegerArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr [s]
        mov     r8d, dword ptr [s + 4]
        mov     r9d, dword ptr [s + 8]
        mov     r10d, dword ptr [s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr [d], eax
        mov     dword ptr [d + 4], r8d
        mov     dword ptr [d + 8], r9d
        mov     dword ptr [d + 12], r10d
        mov     eax, dword ptr [s + 16]
        mov     r8d, dword ptr [s + 20]
        mov     r9d, dword ptr [s + 24]
        mov     r10d, dword ptr [s + 28]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr [d + 16], eax
        mov     dword ptr [d + 20], r8d
        mov     dword ptr [d + 24], r9d
        mov     dword ptr [d + 28], r10d
end;

procedure bswap160(s, d: PIntegerArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr [s]
        mov     r8d, dword ptr [s + 4]
        mov     r9d, dword ptr [s + 8]
        mov     r10d, dword ptr [s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr [d], eax
        mov     dword ptr [d + 4], r8d
        mov     dword ptr [d + 8], r9d
        mov     dword ptr [d + 12], r10d
        mov     eax, dword ptr [s + 16]
        bswap   eax
        mov     dword ptr [d + 16], eax
end;

// see http://nicst.de/crc.pdf

function gf2_multiply(x, y, m, bits: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     rax, x
        and     rax, 1
        cmovne  rax, y
@s:     mov     r10, rax
        mov     r11, m
        shr     r10, 1
        xor     r11, r10
        test    al, 1
        mov     rax, r10
        cmovne  rax, r11
        shr     x, 1
        mov     r10, rax
        xor     r10, y
        {$ifdef WIN64ABI}
        test    cl, 1
        {$else}
        test    dil, 1
        {$endif WIN64ABI}
        cmovne  rax, r10
        dec     bits
        jne     @s
end;

procedure MD5Transform(var buf: TMd5Buf; const in_: TMd5In);
// see https://synopse.info/forum/viewtopic.php?id=4369 for asm numbers
{
 MD5_Transform-x64
 MD5 transform routine optimized for x64 processors
 Copyright 2018 Ritlabs, SRL
 The 64-bit version is written by Maxim Masiutin <max@ritlabs.com>

 The main advantage of this 64-bit version is that it loads 64 bytes of hashed
 message into 8 64-bit registers (RBP, R8, R9, R10, R11, R12, R13, R14) at the
 beginning, to avoid excessive memory load operations througout the routine.

 MD5_Transform-x64 is released under a dual license, and you may choose to use
 it under either the Mozilla Public License 2.0 (MPL 2.1, available from
 https://www.mozilla.org/en-US/MPL/2.0/) or the GNU Lesser General Public
 License Version 3, dated 29 June 2007 (LGPL 3, available from
 https://www.gnu.org/licenses/lgpl.html).

 MD5_Transform-x64 is based on Peter Sawatzki's code.
 Taken from https://github.com/maximmasiutin/MD5_Transform-x64
}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifndef WIN64ABI} // W=rcx Buf=rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64ABI}
        push    rbx
        push    rsi
        push    rdi
        push    rbp
        push    r12
        push    r13
        push    r14
        mov     r14, rdx
        mov     rsi, rcx
        push    rsi
        mov     eax, dword ptr [rsi]
        mov     ebx, dword ptr [rsi + 4H]
        mov     ecx, dword ptr [rsi + 8H]
        mov     edx, dword ptr [rsi + 0CH]
        mov     rbp, qword ptr [r14]
        add     eax, -680876936
        add     eax, ebp
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     rbp, 32
        add     edx, -389564586
        add     edx, ebp
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r8, qword ptr [r14 + 8H]
        add     ecx, 606105819
        add     ecx, r8d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r8, 32
        add     ebx, -1044525330
        add     ebx, r8d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r9, qword ptr [r14 + 10H]
        add     eax, -176418897
        add     eax, r9d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r9, 32
        add     edx, 1200080426
        add     edx, r9d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r10, qword ptr [r14 + 18H]
        add     ecx, -1473231341
        add     ecx, r10d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r10, 32
        add     ebx, -45705983
        add     ebx, r10d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r11, qword ptr [r14 + 20H]
        add     eax, 1770035416
        add     eax, r11d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r11, 32
        add     edx, -1958414417
        add     edx, r11d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r12, qword ptr [r14 + 28H]
        add     ecx, -42063
        add     ecx, r12d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r12, 32
        add     ebx, -1990404162
        add     ebx, r12d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r13, qword ptr [r14 + 30H]
        add     eax, 1804603682
        add     eax, r13d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r13, 32
        add     edx, -40341101
        add     edx, r13d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r14, qword ptr [r14 + 38H]
        add     ecx, -1502002290
        add     ecx, r14d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r14, 32
        add     ebx, 1236535329
        add     ebx, r14d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, -165796510
        add     eax, ebp
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r10, 32
        add     edx, -1069501632
        add     edx, r10d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, 643717713
        add     ecx, r12d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     rbp, 32
        add     ebx, -373897302
        add     ebx, ebp
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, -701558691
        add     eax, r9d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r12, 32
        add     edx, 38016083
        add     edx, r12d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, -660478335
        add     ecx, r14d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r9, 32
        add     ebx, -405537848
        add     ebx, r9d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, 568446438
        add     eax, r11d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r14, 32
        add     edx, -1019803690
        add     edx, r14d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, -187363961
        add     ecx, r8d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r11, 32
        add     ebx, 1163531501
        add     ebx, r11d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, -1444681467
        add     eax, r13d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r8, 32
        add     edx, -51403784
        add     edx, r8d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        ror     r10, 32
        add     ecx, 1735328473
        add     ecx, r10d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r13, 32
        add     ebx, -1926607734
        add     ebx, r13d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        ror     r9, 32
        add     eax, -378558
        add     eax, r9d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, -2022574463
        add     edx, r11d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r12, 32
        add     ecx, 1839030562
        add     ecx, r12d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, -35309556
        add     ebx, r14d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     rbp, 32
        add     eax, -1530992060
        add     eax, ebp
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     r9, 32
        add     edx, 1272893353
        add     edx, r9d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, -155497632
        add     ecx, r10d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r12, 32
        add     ebx, -1094730640
        add     ebx, r12d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     r13, 32
        add     eax, 681279174
        add     eax, r13d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     rbp, 32
        add     edx, -358537222
        add     edx, ebp
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r8, 32
        add     ecx, -722521979
        add     ecx, r8d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r10, 32
        add     ebx, 76029189
        add     ebx, r10d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     r11, 32
        add     eax, -640364487
        add     eax, r11d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     r13, 32
        add     edx, -421815835
        add     edx, r13d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r14, 32
        add     ecx, 530742520
        add     ecx, r14d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r8, 32
        add     ebx, -995338651
        add     ebx, r8d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, -198630844
        add     eax, ebp
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r10, 32
        add     edx, 1126891415
        add     edx, r10d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r14, 32
        add     ecx, -1416354905
        add     ecx, r14d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r9, 32
        add     ebx, -57434055
        add     ebx, r9d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, 1700485571
        add     eax, r13d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r8, 32
        add     edx, -1894986606
        add     edx, r8d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, -1051523
        add     ecx, r12d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     rbp, 32
        add     ebx, -2054922799
        add     ebx, ebp
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        ror     r11, 32
        add     eax, 1873313359
        add     eax, r11d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r14, 32
        add     edx, -30611744
        add     edx, r14d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r10, 32
        add     ecx, -1560198380
        add     ecx, r10d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r13, 32
        add     ebx, 1309151649
        add     ebx, r13d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        ror     r9, 32
        add     eax, -145523070
        add     eax, r9d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r12, 32
        add     edx, -1120210379
        add     edx, r12d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r8, 32
        add     ecx, 718787259
        add     ecx, r8d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r11, 32
        add     ebx, -343485551
        add     ebx, r11d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        pop     rsi
        add     dword ptr [rsi], eax
        add     dword ptr [rsi + 4H], ebx
        add     dword ptr [rsi + 8H], ecx
        add     dword ptr [rsi + 0CH], edx
        pop     r14
        pop     r13
        pop     r12
        pop     rbp
        pop     rdi
        pop     rsi
        pop     rbx
end;

{$ifdef USEAESNI}

procedure aesniencrypt128(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 0]
        movups  xmm1, dqword ptr [ctxt + 16 * 1]
        movups  xmm2, dqword ptr [ctxt + 16 * 2]
        movups  xmm3, dqword ptr [ctxt + 16 * 3]
        movups  xmm4, dqword ptr [ctxt + 16 * 4]
        movups  xmm5, dqword ptr [ctxt + 16 * 5]
        movups  xmm6, dqword ptr [ctxt + 16 * 6]
        movups  xmm8, dqword ptr [ctxt + 16 * 7]
        movups  xmm9, dqword ptr [ctxt + 16 * 8]
        movups  xmm10, dqword ptr [ctxt + 16 * 9]
        movups  xmm11, dqword ptr [ctxt + 16 * 10]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesniencrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 0]
        movups  xmm1, dqword ptr [ctxt + 16 * 1]
        movups  xmm2, dqword ptr [ctxt + 16 * 2]
        movups  xmm3, dqword ptr [ctxt + 16 * 3]
        movups  xmm4, dqword ptr [ctxt + 16 * 4]
        movups  xmm5, dqword ptr [ctxt + 16 * 5]
        movups  xmm6, dqword ptr [ctxt + 16 * 6]
        movups  xmm8, dqword ptr [ctxt + 16 * 7]
        movups  xmm9, dqword ptr [ctxt + 16 * 8]
        movups  xmm10, dqword ptr [ctxt + 16 * 9]
        movups  xmm11, dqword ptr [ctxt + 16 * 10]
        movups  xmm12, dqword ptr [ctxt + 16 * 11]
        movups  xmm13, dqword ptr [ctxt + 16 * 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenclast xmm7, xmm13
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesniencrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 0]
        movups  xmm1, dqword ptr [ctxt + 16 * 1]
        movups  xmm2, dqword ptr [ctxt + 16 * 2]
        movups  xmm3, dqword ptr [ctxt + 16 * 3]
        movups  xmm4, dqword ptr [ctxt + 16 * 4]
        movups  xmm5, dqword ptr [ctxt + 16 * 5]
        movups  xmm6, dqword ptr [ctxt + 16 * 6]
        movups  xmm8, dqword ptr [ctxt + 16 * 7]
        movups  xmm9, dqword ptr [ctxt + 16 * 8]
        movups  xmm10, dqword ptr [ctxt + 16 * 9]
        movups  xmm11, dqword ptr [ctxt + 16 * 10]
        movups  xmm12, dqword ptr [ctxt + 16 * 11]
        movups  xmm13, dqword ptr [ctxt + 16 * 12]
        movups  xmm14, dqword ptr [ctxt + 16 * 13]
        movups  xmm15, dqword ptr [ctxt + 16 * 14]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt128(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 10]
        movups  xmm1, dqword ptr [ctxt + 16 * 9]
        movups  xmm2, dqword ptr [ctxt + 16 * 8]
        movups  xmm3, dqword ptr [ctxt + 16 * 7]
        movups  xmm4, dqword ptr [ctxt + 16 * 6]
        movups  xmm5, dqword ptr [ctxt + 16 * 5]
        movups  xmm6, dqword ptr [ctxt + 16 * 4]
        movups  xmm8, dqword ptr [ctxt + 16 * 3]
        movups  xmm9, dqword ptr [ctxt + 16 * 2]
        movups  xmm10, dqword ptr [ctxt + 16 * 1]
        movups  xmm11, dqword ptr [ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdeclast xmm7, xmm11
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 12]
        movups  xmm1, dqword ptr [ctxt + 16 * 11]
        movups  xmm2, dqword ptr [ctxt + 16 * 10]
        movups  xmm3, dqword ptr [ctxt + 16 * 9]
        movups  xmm4, dqword ptr [ctxt + 16 * 8]
        movups  xmm5, dqword ptr [ctxt + 16 * 7]
        movups  xmm6, dqword ptr [ctxt + 16 * 6]
        movups  xmm8, dqword ptr [ctxt + 16 * 5]
        movups  xmm9, dqword ptr [ctxt + 16 * 4]
        movups  xmm10, dqword ptr [ctxt + 16 * 3]
        movups  xmm11, dqword ptr [ctxt + 16 * 2]
        movups  xmm12, dqword ptr [ctxt + 16 * 1]
        movups  xmm13, dqword ptr [ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdec  xmm7, xmm11
        aesdec  xmm7, xmm12
        aesdeclast xmm7, xmm13
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr [source]
        movups  xmm0, dqword ptr [ctxt + 16 * 14]
        movups  xmm1, dqword ptr [ctxt + 16 * 13]
        movups  xmm2, dqword ptr [ctxt + 16 * 12]
        movups  xmm3, dqword ptr [ctxt + 16 * 11]
        movups  xmm4, dqword ptr [ctxt + 16 * 10]
        movups  xmm5, dqword ptr [ctxt + 16 * 9]
        movups  xmm6, dqword ptr [ctxt + 16 * 8]
        movups  xmm8, dqword ptr [ctxt + 16 * 7]
        movups  xmm9, dqword ptr [ctxt + 16 * 6]
        movups  xmm10, dqword ptr [ctxt + 16 * 5]
        movups  xmm11, dqword ptr [ctxt + 16 * 4]
        movups  xmm12, dqword ptr [ctxt + 16 * 3]
        movups  xmm13, dqword ptr [ctxt + 16 * 2]
        movups  xmm14, dqword ptr [ctxt + 16 * 1]
        movups  xmm15, dqword ptr [ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdec  xmm7, xmm11
        aesdec  xmm7, xmm12
        aesdec  xmm7, xmm13
        aesdec  xmm7, xmm14
        aesdeclast xmm7, xmm15
        movups  dqword ptr [dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure ShiftAesNi(KeySize: cardinal; pk: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, keysize
        movups  xmm1, dqword ptr [pk]
        movaps  xmm5, dqword ptr [rip + @mask]
        cmp     al, 128
        je      @128
        cmp     al, 192
        je      @e // 192 bits is very complicated -> skip by now (128+256)
@256:   movups  xmm3, dqword ptr [pk + 16]
        add     pk, 32
        aeskeygenassist xmm2, xmm3, 1
        call    @exp256
        aeskeygenassist xmm2, xmm3, 2
        call    @exp256
        aeskeygenassist xmm2, xmm3, 4
        call    @exp256
        aeskeygenassist xmm2, xmm3, 8
        call    @exp256
        aeskeygenassist xmm2, xmm3, $10
        call    @exp256
        aeskeygenassist xmm2, xmm3, $20
        call    @exp256
        aeskeygenassist xmm2, xmm3, $40
        pshufd  xmm2, xmm2, $FF
        movups  xmm4, xmm1
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  dqword ptr [pk], xmm1
        jmp     @e
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@mask:  dd      $ffffffff
        dd      $03020100
        dd      $07060504
        dd      $0b0a0908
@exp256:pshufd  xmm2, xmm2, $ff
        movups  xmm4, xmm1
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  dqword ptr [pk], xmm1
        add     pk, $10
        aeskeygenassist xmm4, xmm1, 0
        pshufd  xmm2, xmm4, $AA
        movups  xmm4, xmm3
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pxor    xmm3, xmm2
        movups  dqword ptr [pk], xmm3
        add     pk, $10
@e:     ret
@exp128:pshufd  xmm2, xmm2, $FF
        movups  xmm3, xmm1
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pxor    xmm1, xmm2
        movups  dqword ptr [pk], xmm1
        add     pk, $10
        ret
@128:   add     pk, 16
        aeskeygenassist xmm2, xmm1, 1
        call    @exp128
        aeskeygenassist xmm2, xmm1, 2
        call    @exp128
        aeskeygenassist xmm2, xmm1, 4
        call    @exp128
        aeskeygenassist xmm2, xmm1, 8
        call    @exp128
        aeskeygenassist xmm2, xmm1, $10
        call    @exp128
        aeskeygenassist xmm2, xmm1, $20
        call    @exp128
        aeskeygenassist xmm2, xmm1, $40
        call    @exp128
        aeskeygenassist xmm2, xmm1, $80
        call    @exp128
        aeskeygenassist xmm2, xmm1, $1b
        call    @exp128
        aeskeygenassist xmm2, xmm1, $36
        call    @exp128
end;

procedure MakeDecrKeyAesNi(Rounds: integer; RK: Pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, Rounds
        sub     eax, 9
        movups  xmm0, dqword ptr [RK + $10]
        movups  xmm1, dqword ptr [RK + $20]
        movups  xmm2, dqword ptr [RK + $30]
        movups  xmm3, dqword ptr [RK + $40]
        movups  xmm4, dqword ptr [RK + $50]
        movups  xmm5, dqword ptr [RK + $60]
        movups  xmm6, dqword ptr [RK + $70]
        movups  xmm7, dqword ptr [RK + $80]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        aesimc  xmm6, xmm6
        aesimc  xmm7, xmm7
        movups  dqword ptr [RK + $10], xmm0
        movups  dqword ptr [RK + $20], xmm1
        movups  dqword ptr [RK + $30], xmm2
        movups  dqword ptr [RK + $40], xmm3
        movups  dqword ptr [RK + $50], xmm4
        movups  dqword ptr [RK + $60], xmm5
        movups  dqword ptr [RK + $70], xmm6
        movups  dqword ptr [RK + $80], xmm7
        lea     RK, [RK + $90]
@loop:  movups  xmm0, dqword ptr [RK]
        aesimc  xmm0, xmm0
        movups  dqword ptr [RK], xmm0
        add     RK, 16
        dec     eax
        jnz     @loop
end;

procedure AesNiEncryptOfb128(iv, aes, source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     rax, blockcount  // on Win64 ABI, blockcount is not a register
        test    eax, eax
        jz      @z
        movups  xmm7, dqword ptr [iv]  // xmm7 = IV
        movups  xmm0, dqword ptr [aes + 16 * 0]
        movups  xmm1, dqword ptr [aes + 16 * 1]
        movups  xmm2, dqword ptr [aes + 16 * 2]
        movups  xmm3, dqword ptr [aes + 16 * 3]
        movups  xmm4, dqword ptr [aes + 16 * 4]
        movups  xmm5, dqword ptr [aes + 16 * 5]
        movups  xmm6, dqword ptr [aes + 16 * 6]
        movups  xmm8, dqword ptr [aes + 16 * 7]
        movups  xmm9, dqword ptr [aes + 16 * 8]
        movups  xmm10, dqword ptr [aes + 16 * 9]
        movups  xmm11, dqword ptr [aes + 16 * 10]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [source]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        add     source, 16
        add     dest, 16
        dec     eax
        jnz     @s
        movups  dqword ptr [iv], xmm7
@z:
end;

procedure AesNiEncryptOfb256(iv, aes, source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     rax, blockcount
        test    eax, eax
        jz      @z
        movups  xmm7, dqword ptr [iv]  // xmm7 = IV
        movups  xmm0, dqword ptr [aes + 16 * 0]
        movups  xmm1, dqword ptr [aes + 16 * 1]
        movups  xmm2, dqword ptr [aes + 16 * 2]
        movups  xmm3, dqword ptr [aes + 16 * 3]
        movups  xmm4, dqword ptr [aes + 16 * 4]
        movups  xmm5, dqword ptr [aes + 16 * 5]
        movups  xmm6, dqword ptr [aes + 16 * 6]
        movups  xmm8, dqword ptr [aes + 16 * 7]
        movups  xmm9, dqword ptr [aes + 16 * 8]
        movups  xmm10, dqword ptr [aes + 16 * 9]
        movups  xmm11, dqword ptr [aes + 16 * 10]
        movups  xmm12, dqword ptr [aes + 16 * 11]
        movups  xmm13, dqword ptr [aes + 16 * 12]
        movups  xmm14, dqword ptr [aes + 16 * 13]
        add     aes,  16 * 14  // aes = last key
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [aes]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr [source]
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor fIV
        add     source, 16
        add     dest, 16
        dec     eax
        jnz     @s
        movups  dqword ptr [iv], xmm7
@z:
end;

// AES-CTR with 8x AES-NI interleave factor over a 32-bit counter
procedure AesNiEncryptCtrNist32(
  src, dest: pointer; blocks: PtrUInt; ctxt, iv: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        mov     rax, qword ptr [rsp + $28]  // not passed as register
        push    rsi
        push    rdi
        mov     rdi, src    // rcx
        mov     rsi, dest   // rdx
        mov     rdx, blocks // r8
        mov     rcx, ctxt   // r9
        mov     r8, rax     // iv on stack
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=blocks, rcx=ctxt, r8=iv
      	cmp    rdx, $1
      	jne    @big
        // handle single block without allocating any stack frame
      	movups xmm2, dqword ptr [r8]
      	movups xmm3, dqword ptr [rdi]
      	mov   al, byte ptr [rcx].TAesContext.Rounds
      	movups xmm0, dqword ptr [rcx]
      	movups xmm1, dqword ptr [rcx + $10]
      	lea    rcx, [rcx + $20]
        dec    al
      	xorps  xmm2, xmm0
@sml:   aesenc xmm2, xmm1
      	movups xmm1, dqword ptr [rcx]
      	add    rcx, $10
      	dec    al
      	jne    @sml
      	aesenclast xmm2, xmm1
      	pxor   xmm0, xmm0
      	pxor   xmm1, xmm1
      	xorps  xmm2, xmm3
      	pxor   xmm3, xmm3
      	movups dqword ptr [rsi], xmm2
      	xorps  xmm2, xmm2
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
      	ret
        // optimized AES - CTR process
@big:   lea    r11, [rsp]
        push   rbp
        sub    rsp, $80
        and    rsp, $fffffffffffffff0
        // precompute the next 8 counters on the stack
        movdqu xmm2, dqword ptr [r8]
        movdqu xmm0, dqword ptr [rcx]
        mov    r8d, dword ptr [r8 + $c] // get 32-bit counter
        pxor   xmm2, xmm0
        mov    ebp, dword ptr [rcx + $c]
        movdqa dqword ptr [rsp], xmm2
        bswap  r8d
        movdqa xmm3, xmm2
        movdqa xmm4, xmm2
        movdqa xmm5, xmm2
        movdqa dqword ptr [rsp + $40], xmm2
        movdqa dqword ptr [rsp + $50], xmm2
        movdqa dqword ptr [rsp + $60], xmm2
        mov    r10, rdx
        movdqa dqword ptr [rsp + $70], xmm2
        lea    rax, [r8 + $1]
        lea    rdx, [r8 + $2]
        bswap  eax
        bswap  edx
        xor    eax, ebp
        xor    edx, ebp
        pinsrd xmm3, eax, $3
        lea    rax, [r8 + $3]
        movdqa dqword ptr [rsp + $10], xmm3
        pinsrd xmm4, edx, $3
        bswap  eax
        mov    rdx, r10
        lea    r10, [r8 + $4]
        movdqa dqword ptr [rsp + $20], xmm4
        xor    eax, ebp
        bswap  r10d
        pinsrd xmm5, eax, $3
        movzx  eax, byte ptr [rcx].TAesContext.Rounds
        xor    r10d, ebp
        movdqa dqword ptr [rsp + $30], xmm5
        lea    r9, [r8 + $5]
        mov    dword ptr [rsp + $4c], r10d
        bswap  r9d
        lea    r10, [r8 + $6]
        dec    al // code below doesn't include the last aesenclast round
        xor    r9d, ebp
        bswap  r10d
        mov    dword ptr [rsp + $5c], r9d
        xor    r10d, ebp
        lea    r9, [r8 + $7]
        mov    dword ptr [rsp + $6c], r10d
        bswap  r9d
        xor    r9d, ebp
        mov    dword ptr [rsp + $7c], r9d
        // start interleaved process
        movups xmm1, dqword ptr [rcx + $10]
        movdqa xmm6, dqword ptr [rsp + $40]
        movdqa xmm7, dqword ptr [rsp + $50]
        cmp    rdx, $8
        jb     @tail
        sub    rdx, $6
        lea    rcx, [rcx + $80]
        sub    rdx, $2
        // main loop, processing 8 interleaved CTR per iteration
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop8: add    r8d, $8 // our 32-bit CTR
        movdqa xmm8, dqword ptr [rsp + $60]
        aesenc xmm2, xmm1
        mov    r9d, r8d
        movdqa xmm9, dqword ptr [rsp + $70]
        aesenc xmm3, xmm1
        bswap  r9d
        movups xmm0, dqword ptr [rcx - $60]
        aesenc xmm4, xmm1
        xor    r9d, ebp
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $c], r9d
        lea    r9, [r8 + $1]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $50]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $1c], r9d
        lea    r9, [r8 + $2]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx - $40]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $2c], r9d
        lea    r9, [r8 + $3]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $30]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $3c], r9d
        lea    r9, [r8 + $4]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx - $20]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $4c], r9d
        lea    r9, [r8 + $5]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $10]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $5c], r9d
        lea    r9, [r8 + $6]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $6c], r9d
        lea    r9, [r8 + $7]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $10]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        xor    r9d, ebp
        movdqu xmm10, dqword ptr [rdi]
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $7c], r9d
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $20]
        cmp    al, 11
        jb     @edone      // 128-bit AES
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $30]
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $40]
        je     @edone       // 192-bit AES
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $50]
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $60] // 256-bit AES
        // encrypt 8*128 - bit blocks from src into dest
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@edone: movdqu xmm11, dqword ptr [rdi + $10]
        pxor   xmm10, xmm0
        movdqu xmm12, dqword ptr [rdi + $20]
        pxor   xmm11, xmm0
        movdqu xmm13, dqword ptr [rdi + $30]
        pxor   xmm12, xmm0
        movdqu xmm14, dqword ptr [rdi + $40]
        pxor   xmm13, xmm0
        movdqu xmm15, dqword ptr [rdi + $50]
        pxor   xmm14, xmm0
        pxor   xmm15, xmm0
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movdqu xmm1, dqword ptr [rdi + $60]
        lea    rdi, [rdi + $80]
        aesenclast xmm2, xmm10
        pxor   xmm1, xmm0
        movdqu xmm10, dqword ptr [rdi - $10]
        aesenclast xmm3, xmm11
        pxor   xmm10, xmm0
        movdqa xmm11, dqword ptr [rsp]
        aesenclast xmm4, xmm12
        aesenclast xmm5, xmm13
        movdqa xmm12, dqword ptr [rsp + $10]
        movdqa xmm13, dqword ptr [rsp + $20]
        aesenclast xmm6, xmm14
        aesenclast xmm7, xmm15
        movdqa xmm14, dqword ptr [rsp + $30]
        movdqa xmm15, dqword ptr [rsp + $40]
        aesenclast xmm8, xmm1
        movdqa xmm0, dqword ptr [rsp + $50]
        movups xmm1, dqword ptr [rcx - $70]
        aesenclast xmm9, xmm10
        movups dqword ptr [rsi], xmm2
        movdqa xmm2, xmm11
        movups dqword ptr [rsi + $10], xmm3
        movdqa xmm3, xmm12
        movups dqword ptr [rsi + $20], xmm4
        movdqa xmm4, xmm13
        movups dqword ptr [rsi + $30], xmm5
        movdqa xmm5, xmm14
        movups dqword ptr [rsi + $40], xmm6
        movdqa xmm6, xmm15
        movups dqword ptr [rsi + $50], xmm7
        movdqa xmm7, xmm0
        movups dqword ptr [rsi + $60], xmm8
        movups dqword ptr [rsi + $70], xmm9
        lea    rsi, [rsi + $80]
        sub    rdx, $8
        jae    @loop8
        add    rdx, $8
        je     @done
        lea    rcx, [rcx - $80]
        // finalize the process with the 1..7 trailing blocks
@tail:  lea    rcx, [rcx + $10]
        cmp    rdx, $4
        jb     @loop3
        je     @loop4
        shl    eax, $4
        movdqa xmm8, dqword ptr [rsp + $60]
        pxor   xmm9, xmm9
        movups xmm0, dqword ptr [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        lea    rcx, [rcx + rax + $10]
        neg    rax
        aesenc xmm4, xmm1
        add    rax, $10
        movups xmm10, dqword ptr [rdi]
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        movups xmm11, dqword ptr [rdi + $10]
        movups xmm12, dqword ptr [rdi + $20]
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        jmp    @sub8l
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@sub8:  aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
@sub8l: movups xmm1, dqword ptr [rcx + rax]
        add    rax, $20
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + rax - $10]
        jne    @sub8
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        aesenclast xmm2, xmm0
        aesenclast xmm3, xmm0
        aesenclast xmm4, xmm0
        aesenclast xmm5, xmm0
        aesenclast xmm6, xmm0
        aesenclast xmm7, xmm0
        aesenclast xmm8, xmm0
        aesenclast xmm9, xmm0
        movdqu xmm13, dqword ptr [rdi + $30]
        pxor   xmm2, xmm10
        movdqu xmm10, dqword ptr [rdi + $40]
        pxor   xmm3, xmm11
        movdqu dqword ptr [rsi], xmm2
        pxor   xmm4, xmm12
        movdqu dqword ptr [rsi + $10], xmm3
        pxor   xmm5, xmm13
        movdqu dqword ptr [rsi + $20], xmm4
        pxor   xmm6, xmm10
        movdqu dqword ptr [rsi + $30], xmm5
        movdqu dqword ptr [rsi + $40], xmm6
        cmp    rdx, $6
        jb     @done
        movups xmm11, dqword ptr [rdi + $50]
        xorps  xmm7, xmm11
        movups dqword ptr [rsi + $50], xmm7
        je     @done
        movups xmm12, dqword ptr [rdi + $60]
        xorps  xmm8, xmm12
        movups dqword ptr [rsi + $60], xmm8
        jmp    @done
        // trailing 4 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop4: aesenc xmm2, xmm1
        lea    rcx, [rcx + $10]
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        movups xmm1, dqword ptr [rcx]
        dec    al
        jne    @loop4
        aesenclast xmm2, xmm1
        aesenclast xmm3, xmm1
        movups xmm10, dqword ptr [rdi]
        movups xmm11, dqword ptr [rdi + $10]
        aesenclast xmm4, xmm1
        aesenclast xmm5, xmm1
        movups xmm12, dqword ptr [rdi + $20]
        movups xmm13, dqword ptr [rdi + $30]
        xorps  xmm2, xmm10
        movups dqword ptr [rsi], xmm2
        xorps  xmm3, xmm11
        movups dqword ptr [rsi + $10], xmm3
        pxor   xmm4, xmm12
        movdqu dqword ptr [rsi + $20], xmm4
        pxor   xmm5, xmm13
        movdqu dqword ptr [rsi + $30], xmm5
        jmp    @done
        // 1..3 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop3: lea    rcx, [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        movups xmm1, dqword ptr [rcx]
        dec    al
        jne    @loop3
        aesenclast xmm2, xmm1
        aesenclast xmm3, xmm1
        aesenclast xmm4, xmm1
        movups xmm10, dqword ptr [rdi]
        xorps  xmm2, xmm10
        movups dqword ptr [rsi], xmm2 // 1 trailing block
        cmp    rdx, $2
        jb     @done
        movups xmm11, dqword ptr [rdi + $10]
        xorps  xmm3, xmm11
        movups dqword ptr [rsi + $10], xmm3 // 2 trailing blocks
        je     @done
        movups xmm12, dqword ptr [rdi + $20]
        xorps  xmm4, xmm12
        movups dqword ptr [rsi + $20], xmm4 // 3 trailing blocks
        // restore stack
@done:  mov    rbp, qword ptr [r11 - $8]
      	lea    rsp, [r11]
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

procedure CtrNistCarry12(ctr: PAesBlock); // not worth inlining
var
  n: PtrUInt;
  carry: cardinal;
begin
  n := 12;
  carry := 1;
  repeat
    dec(n);
    inc(carry, ctr[n]);
    ctr[n] := byte(carry);
    carry := carry shr 8;
  until (carry = 0) or
        (n = 0);
end;

// AesNiEncryptCtrNist32() expects the CTR in lowest 32-bit to never overflow
procedure AesNiEncryptCtrNist(src, dest: PByte; len: cardinal;
  ctxt, iv: PHash128Rec); inline;
var
  ctr, blocks: cardinal;
begin
  ctr := bswap32(iv.c3);
  repeat
    blocks := len shr AesBlockShift;
    inc(ctr, blocks);
    if ctr < blocks then
    begin
      // 32-bit counter overflow -> will loop until all processed
      dec(blocks, ctr);
      ctr := 0;
    end;
    AesNiEncryptCtrNist32(src, dest, blocks, ctxt, iv); // 32-bit CTR asm
    iv.c3 := bswap32(ctr);
    if ctr = 0 then
      CtrNistCarry12(@iv.b); // propagate carry
    blocks := blocks shl AesBlockShift;
    inc(src, blocks);
    inc(dest, blocks);
    dec(len, blocks);
  until len = 0; // caller ensured len and 15 = 0
end;

// AES-CTR and 256-bit crc32c with 8x AES-NI interleave over a 32-bit counter
procedure AesNiEncryptCtrCrc32(
  src, dest: pointer; blocks: PtrUInt; ctxt: TAesCtrCrc);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rdi, src    // rcx
        mov     rsi, dest   // rdx
        mov     rdx, blocks // r8
        mov     rcx, ctxt   // r9
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=blocks, rcx=TAesCtrCrc
        push   r12
        push   r13
        push   r14
        push   r15
        push   rbx
        lea    rbx, [rcx].TAesCtrCrc.fMac  // rbx=plain rbx+16=encrypted
      	cmp    rdx, $1
      	jne    @big
        // handle single block without allocating any stack frame
      	movups xmm2, dqword ptr [rcx].TAesCtrCrc.fIV
        lea    rcx, [rcx].TAesCtrCrc.fAes
      	movups xmm3, dqword ptr [rdi]
        mov    r10d, 16
        call   @crc
      	mov    al, byte ptr [rcx].TAesContext.Rounds
      	movups xmm0, dqword ptr [rcx]
      	movups xmm1, dqword ptr [rcx + $10]
      	lea    rcx, [rcx + $20]
        dec    al
      	xorps  xmm2, xmm0
@sml:   aesenc xmm2, xmm1
      	movups xmm1, dqword ptr [rcx]
      	add    rcx, $10
      	dec    al
      	jne    @sml
      	aesenclast xmm2, xmm1
      	pxor   xmm0, xmm0
      	pxor   xmm1, xmm1
      	xorps  xmm2, xmm3
      	pxor   xmm3, xmm3
      	movups dqword ptr [rsi], xmm2
      	xorps  xmm2, xmm2
        lea    rbx, [rbx + TAesMac256.encrypted]
        mov    rdi, rsi
        mov    r10d, 16
        call   @crc
        jmp    @exit
        // inlined crcblocksse42() crc=rbx data=rdi bytes=r10
        {$ifdef FPC} align 8 {$else} .align 8 {$endif}
@crc:   lea    r9, [rdi + r10]
        neg    r10
        mov    r12d, dword ptr [rbx]
        mov    r13d, dword ptr [rbx + 4]
        mov    r14d, dword ptr [rbx + 8]
        mov    r15d, dword ptr [rbx + 12]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@crcl:  crc32  r12d, dword ptr [r9 + r10]
        crc32  r13d, dword ptr [r9 + r10 + 4]
        crc32  r14d, dword ptr [r9 + r10 + 8]
        crc32  r15d, dword ptr [r9 + r10 + 12]
        add    r10, 16
        jnz    @crcl
        mov    dword ptr [rbx], r12d
        mov    dword ptr [rbx + 4], r13d
        mov    dword ptr [rbx + 8], r14d
        mov    dword ptr [rbx + 12], r15d
        ret
        // optimized AES - CTR process
@big:   lea    r11, [rsp]
        push   rbp
        sub    rsp, $80
        and    rsp, $fffffffffffffff0
        // precompute the next 8 counters on the stack
        movdqu xmm2, dqword ptr [rcx].TAesCtrCrc.fIV
        mov    r8d, dword ptr [rcx + $c].TAesCtrCrc.fIV // get 32-bit counter
        lea    rcx, [rcx].TAesCtrCrc.fAes
        movdqu xmm0, dqword ptr [rcx]
        pxor   xmm2, xmm0
        mov    ebp, dword ptr [rcx + $c]
        movdqa dqword ptr [rsp], xmm2
        bswap  r8d
        movdqa xmm3, xmm2
        movdqa xmm4, xmm2
        movdqa xmm5, xmm2
        movdqa dqword ptr [rsp + $40], xmm2
        movdqa dqword ptr [rsp + $50], xmm2
        movdqa dqword ptr [rsp + $60], xmm2
        mov    r10, rdx
        movdqa dqword ptr [rsp + $70], xmm2
        lea    rax, [r8 + $1]
        lea    rdx, [r8 + $2]
        bswap  eax
        bswap  edx
        xor    eax, ebp
        xor    edx, ebp
        pinsrd xmm3, eax, $3
        lea    rax, [r8 + $3]
        movdqa dqword ptr [rsp + $10], xmm3
        pinsrd xmm4, edx, $3
        bswap  eax
        mov    rdx, r10
        lea    r10, [r8 + $4]
        movdqa dqword ptr [rsp + $20], xmm4
        xor    eax, ebp
        bswap  r10d
        pinsrd xmm5, eax, $3
        movzx  eax, byte ptr [rcx].TAesContext.Rounds
        xor    r10d, ebp
        movdqa dqword ptr [rsp + $30], xmm5
        lea    r9, [r8 + $5]
        mov    dword ptr [rsp + $4c], r10d
        bswap  r9d
        lea    r10, [r8 + $6]
        dec    al // code below doesn't include the last aesenclast round
        xor    r9d, ebp
        bswap  r10d
        mov    dword ptr [rsp + $5c], r9d
        xor    r10d, ebp
        lea    r9, [r8 + $7]
        mov    dword ptr [rsp + $6c], r10d
        bswap  r9d
        xor    r9d, ebp
        mov    dword ptr [rsp + $7c], r9d
        // start interleaved process
        movups xmm1, dqword ptr [rcx + $10]
        movdqa xmm6, dqword ptr [rsp + $40]
        movdqa xmm7, dqword ptr [rsp + $50]
        cmp    rdx, $8
        jb     @tail
        sub    rdx, $6
        lea    rcx, [rcx + $80]
        sub    rdx, $2
        // main loop, processing 8 interleaved CTR per iteration
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop8: add    r8d, $8 // our 32-bit CTR
        movdqa xmm8, dqword ptr [rsp + $60]
        aesenc xmm2, xmm1
        mov    r9d, r8d
        movdqa xmm9, dqword ptr [rsp + $70]
        aesenc xmm3, xmm1
        bswap  r9d
        movups xmm0, dqword ptr [rcx - $60]
        aesenc xmm4, xmm1
        xor    r9d, ebp
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $c], r9d
        lea    r9, [r8 + $1]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $50]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $1c], r9d
        lea    r9, [r8 + $2]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx - $40]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $2c], r9d
        lea    r9, [r8 + $3]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $30]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $3c], r9d
        lea    r9, [r8 + $4]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx - $20]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $4c], r9d
        lea    r9, [r8 + $5]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx - $10]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        xor    r9d, ebp
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $5c], r9d
        lea    r9, [r8 + $6]
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx]
        bswap  r9d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        xor    r9d, ebp
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        mov    dword ptr [rsp + $6c], r9d
        lea    r9, [r8 + $7]
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $10]
        bswap  r9d
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        xor    r9d, ebp
        movdqu xmm10, dqword ptr [rdi]
        aesenc xmm5, xmm0
        mov    dword ptr [rsp + $7c], r9d
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $20]
        cmp    al, 11
        jb     @edone      // 128-bit AES
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $30]
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $40]
        je     @edone       // 192-bit AES
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movups xmm1, dqword ptr [rcx + $50]
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + $60] // 256-bit AES
        // encrypt 8*128 - bit blocks from src into dest
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@edone: mov    r12d, dword ptr [rbx]
        mov    r13d, dword ptr [rbx + 4]
        mov    r14d, dword ptr [rbx + 8]
        mov    r15d, dword ptr [rbx + 12]
        crc32  r12d, dword ptr [rdi]
        crc32  r13d, dword ptr [rdi + $04]
        crc32  r14d, dword ptr [rdi + $08]
        crc32  r15d, dword ptr [rdi + $0c]
        crc32  r12d, dword ptr [rdi + $10]
        crc32  r13d, dword ptr [rdi + $14]
        crc32  r14d, dword ptr [rdi + $18]
        crc32  r15d, dword ptr [rdi + $1c]
        crc32  r12d, dword ptr [rdi + $20]
        crc32  r13d, dword ptr [rdi + $24]
        crc32  r14d, dword ptr [rdi + $28]
        crc32  r15d, dword ptr [rdi + $2c]
        crc32  r12d, dword ptr [rdi + $30]
        crc32  r13d, dword ptr [rdi + $34]
        crc32  r14d, dword ptr [rdi + $38]
        crc32  r15d, dword ptr [rdi + $3c]
        crc32  r12d, dword ptr [rdi + $40]
        crc32  r13d, dword ptr [rdi + $44]
        crc32  r14d, dword ptr [rdi + $48]
        crc32  r15d, dword ptr [rdi + $4c]
        crc32  r12d, dword ptr [rdi + $50]
        crc32  r13d, dword ptr [rdi + $54]
        crc32  r14d, dword ptr [rdi + $58]
        crc32  r15d, dword ptr [rdi + $5c]
        crc32  r12d, dword ptr [rdi + $60]
        crc32  r13d, dword ptr [rdi + $64]
        crc32  r14d, dword ptr [rdi + $68]
        crc32  r15d, dword ptr [rdi + $6c]
        crc32  r12d, dword ptr [rdi + $70]
        crc32  r13d, dword ptr [rdi + $74]
        crc32  r14d, dword ptr [rdi + $78]
        crc32  r15d, dword ptr [rdi + $7c]
        movdqu xmm11, dqword ptr [rdi + $10]
        pxor   xmm10, xmm0
        movdqu xmm12, dqword ptr [rdi + $20]
        pxor   xmm11, xmm0
        movdqu xmm13, dqword ptr [rdi + $30]
        pxor   xmm12, xmm0
        movdqu xmm14, dqword ptr [rdi + $40]
        pxor   xmm13, xmm0
        movdqu xmm15, dqword ptr [rdi + $50]
        pxor   xmm14, xmm0
        pxor   xmm15, xmm0
        mov    dword ptr [rbx], r12d
        mov    dword ptr [rbx + 4], r13d
        mov    dword ptr [rbx + 8], r14d
        mov    dword ptr [rbx + 12], r15d
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        movdqu xmm1, dqword ptr [rdi + $60]
        lea    rdi, [rdi + $80]
        aesenclast xmm2, xmm10
        pxor   xmm1, xmm0
        movdqu xmm10, dqword ptr [rdi - $10]
        aesenclast xmm3, xmm11
        pxor   xmm10, xmm0
        movdqa xmm11, dqword ptr [rsp]
        aesenclast xmm4, xmm12
        aesenclast xmm5, xmm13
        movdqa xmm12, dqword ptr [rsp + $10]
        movdqa xmm13, dqword ptr [rsp + $20]
        aesenclast xmm6, xmm14
        aesenclast xmm7, xmm15
        movdqa xmm14, dqword ptr [rsp + $30]
        movdqa xmm15, dqword ptr [rsp + $40]
        aesenclast xmm8, xmm1
        movdqa xmm0, dqword ptr [rsp + $50]
        movups xmm1, dqword ptr [rcx - $70]
        aesenclast xmm9, xmm10
        movups dqword ptr [rsi], xmm2
        movdqa xmm2, xmm11
        movups dqword ptr [rsi + $10], xmm3
        movdqa xmm3, xmm12
        movups dqword ptr [rsi + $20], xmm4
        movdqa xmm4, xmm13
        movups dqword ptr [rsi + $30], xmm5
        movdqa xmm5, xmm14
        movups dqword ptr [rsi + $40], xmm6
        movdqa xmm6, xmm15
        movups dqword ptr [rsi + $50], xmm7
        movdqa xmm7, xmm0
        movups dqword ptr [rsi + $60], xmm8
        movups dqword ptr [rsi + $70], xmm9
        mov    r12d, dword ptr [rbx].TAesMac256.encrypted
        mov    r13d, dword ptr [rbx + 4].TAesMac256.encrypted
        mov    r14d, dword ptr [rbx + 8].TAesMac256.encrypted
        mov    r15d, dword ptr [rbx + 12].TAesMac256.encrypted
        crc32  r12d, dword ptr [rsi]
        crc32  r13d, dword ptr [rsi + $04]
        crc32  r14d, dword ptr [rsi + $08]
        crc32  r15d, dword ptr [rsi + $0c]
        crc32  r12d, dword ptr [rsi + $10]
        crc32  r13d, dword ptr [rsi + $14]
        crc32  r14d, dword ptr [rsi + $18]
        crc32  r15d, dword ptr [rsi + $1c]
        crc32  r12d, dword ptr [rsi + $20]
        crc32  r13d, dword ptr [rsi + $24]
        crc32  r14d, dword ptr [rsi + $28]
        crc32  r15d, dword ptr [rsi + $2c]
        crc32  r12d, dword ptr [rsi + $30]
        crc32  r13d, dword ptr [rsi + $34]
        crc32  r14d, dword ptr [rsi + $38]
        crc32  r15d, dword ptr [rsi + $3c]
        crc32  r12d, dword ptr [rsi + $40]
        crc32  r13d, dword ptr [rsi + $44]
        crc32  r14d, dword ptr [rsi + $48]
        crc32  r15d, dword ptr [rsi + $4c]
        crc32  r12d, dword ptr [rsi + $50]
        crc32  r13d, dword ptr [rsi + $54]
        crc32  r14d, dword ptr [rsi + $58]
        crc32  r15d, dword ptr [rsi + $5c]
        crc32  r12d, dword ptr [rsi + $60]
        crc32  r13d, dword ptr [rsi + $64]
        crc32  r14d, dword ptr [rsi + $68]
        crc32  r15d, dword ptr [rsi + $6c]
        crc32  r12d, dword ptr [rsi + $70]
        crc32  r13d, dword ptr [rsi + $74]
        crc32  r14d, dword ptr [rsi + $78]
        crc32  r15d, dword ptr [rsi + $7c]
        lea    rsi, [rsi + $80]
        mov    dword ptr [rbx].TAesMac256.encrypted, r12d
        mov    dword ptr [rbx + 4].TAesMac256.encrypted, r13d
        mov    dword ptr [rbx + 8].TAesMac256.encrypted, r14d
        mov    dword ptr [rbx + 12].TAesMac256.encrypted, r15d
        sub    rdx, $8
        jae    @loop8
        add    rdx, $8
        je     @done
        lea    rcx, [rcx - $80]
        // finalize the process with the edx=1..7 trailing blocks
@tail:  lea    rcx, [rcx + $10]
        mov    r10d, edx
        shl    r10d, 4
        call   @crc
        cmp    rdx, $4
        jb     @loop3
        je     @loop4
        // trailing 5..7 interleaved AES blocks
        shl    eax, $4
        movdqa xmm8, dqword ptr [rsp + $60]
        pxor   xmm9, xmm9
        movups xmm0, dqword ptr [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        lea    rcx, [rcx + rax + $10]
        neg    rax
        aesenc xmm4, xmm1
        add    rax, $10
        movups xmm10, dqword ptr [rdi]
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        movups xmm11, dqword ptr [rdi + $10]
        movups xmm12, dqword ptr [rdi + $20]
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        jmp    @sub8l
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@sub8:  aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
@sub8l: movups xmm1, dqword ptr [rcx + rax]
        add    rax, $20
        aesenc xmm2, xmm0
        aesenc xmm3, xmm0
        aesenc xmm4, xmm0
        aesenc xmm5, xmm0
        aesenc xmm6, xmm0
        aesenc xmm7, xmm0
        aesenc xmm8, xmm0
        aesenc xmm9, xmm0
        movups xmm0, dqword ptr [rcx + rax - $10]
        jne    @sub8
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        aesenc xmm6, xmm1
        aesenc xmm7, xmm1
        aesenc xmm8, xmm1
        aesenc xmm9, xmm1
        aesenclast xmm2, xmm0
        aesenclast xmm3, xmm0
        aesenclast xmm4, xmm0
        aesenclast xmm5, xmm0
        aesenclast xmm6, xmm0
        aesenclast xmm7, xmm0
        aesenclast xmm8, xmm0
        aesenclast xmm9, xmm0
        movdqu xmm13, dqword ptr [rdi + $30]
        pxor   xmm2, xmm10
        movdqu xmm10, dqword ptr [rdi + $40]
        pxor   xmm3, xmm11
        movdqu dqword ptr [rsi], xmm2
        pxor   xmm4, xmm12
        movdqu dqword ptr [rsi + $10], xmm3
        pxor   xmm5, xmm13
        movdqu dqword ptr [rsi + $20], xmm4
        pxor   xmm6, xmm10
        movdqu dqword ptr [rsi + $30], xmm5
        movdqu dqword ptr [rsi + $40], xmm6
        cmp    rdx, $6
        jb     @donec
        movups xmm11, dqword ptr [rdi + $50]
        xorps  xmm7, xmm11
        movups dqword ptr [rsi + $50], xmm7
        je     @donec
        movups xmm12, dqword ptr [rdi + $60]
        xorps  xmm8, xmm12
        movups dqword ptr [rsi + $60], xmm8
        jmp    @donec
        // trailing 4 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop4: aesenc xmm2, xmm1
        lea    rcx, [rcx + $10]
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        aesenc xmm5, xmm1
        movups xmm1, dqword ptr [rcx]
        dec    al
        jne    @loop4
        aesenclast xmm2, xmm1
        aesenclast xmm3, xmm1
        movups xmm10, dqword ptr [rdi]
        movups xmm11, dqword ptr [rdi + $10]
        aesenclast xmm4, xmm1
        aesenclast xmm5, xmm1
        movups xmm12, dqword ptr [rdi + $20]
        movups xmm13, dqword ptr [rdi + $30]
        xorps  xmm2, xmm10
        movups dqword ptr [rsi], xmm2
        xorps  xmm3, xmm11
        movups dqword ptr [rsi + $10], xmm3
        pxor   xmm4, xmm12
        movdqu dqword ptr [rsi + $20], xmm4
        pxor   xmm5, xmm13
        movdqu dqword ptr [rsi + $30], xmm5
        jmp    @donec
        // 1..3 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop3: lea    rcx, [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        movups xmm1, dqword ptr [rcx]
        dec    al
        jne    @loop3
        aesenclast xmm2, xmm1
        aesenclast xmm3, xmm1
        aesenclast xmm4, xmm1
        movups xmm10, dqword ptr [rdi]
        xorps  xmm2, xmm10
        movups dqword ptr [rsi], xmm2 // 1 trailing block
        cmp    rdx, $2
        jb     @donec
        movups xmm11, dqword ptr [rdi + $10]
        xorps  xmm3, xmm11
        movups dqword ptr [rsi + $10], xmm3 // 2 trailing blocks
        je     @donec
        movups xmm12, dqword ptr [rdi + $20]
        xorps  xmm4, xmm12
        movups dqword ptr [rsi + $20], xmm4 // 3 trailing blocks
@donec: lea    rbx, [rbx + TAesMac256.encrypted]
        mov    rdi, rsi
        mov    r10d, edx
        shl    r10d, 4
        call   @crc
        // restore stack
@done:  mov    rbp, qword ptr [r11 - $8]
      	lea    rsp, [r11]
@exit:  pop    rbx
        pop    r15
        pop    r14
        pop    r13
        pop    r12
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

// AesNiEncryptCtrCrc32() expects the CTR in lowest 32-bit to never overflow
procedure AesNiEncryptCtrCrc(src, dest: PByte; len: cardinal;
  ctxt: TAesCtrCrc); inline;
var
  ctr, blocks: cardinal;
begin
  ctr := bswap32(PCardinal(@ctxt.fIV[12])^);
  repeat
    blocks := len shr AesBlockShift;
    inc(ctr, blocks);
    if ctr < blocks then
    begin
      // 32-bit counter overflow -> will loop until all processed
      dec(blocks, ctr);
      ctr := 0;
    end;
    AesNiEncryptCtrCrc32(src, dest, blocks, ctxt); // 32-bit CTR asm
    PCardinal(@ctxt.fIV[12])^ := bswap32(ctr);
    if ctr = 0 then
      CtrNistCarry12(@ctxt.fIV); // propagate carry
    blocks := blocks shl AesBlockShift;
    inc(src, blocks);
    inc(dest, blocks);
    dec(len, blocks);
  until len = 0; // caller ensured len and 15 = 0
end;

procedure AesNiEncryptCfb128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0, dqword ptr [aes + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [aes + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [aes + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [aes + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [aes + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [aes + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [aes + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [aes + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [aes + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [aes + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [aes + 16 * 10].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [src]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm15              // fIV := fOut
        add     src, 16
        add     dest, 16
        dec     blocks
        jnz     @s
        movups  dqword ptr [aes].TAesCfbCrc.fIV, xmm7
@z:
end;

procedure AesNiDecryptCfb128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0, dqword ptr [aes + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [aes + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [aes + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [aes + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [aes + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [aes + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [aes + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [aes + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [aes + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [aes + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [aes + 16 * 10].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [src]
        movups  xmm14, xmm15
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm14               // fIV := fIn
        add     src, 16
        add     dest, 16
        dec     blocks
        jnz     @s
        movups  dqword ptr [aes].TAesCfbCrc.fIV, xmm7
@z:
end;

procedure AesNiEncryptCfb256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // use (mostly) 128-bit registers within the loop
        movups  xmm0, dqword ptr [aes + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [aes + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [aes + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [aes + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [aes + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [aes + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [aes + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [aes + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [aes + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [aes + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [aes + 16 * 10].TAesCfbCrc.fAes
        movups  xmm12, dqword ptr [aes + 16 * 11].TAesCfbCrc.fAes
        movups  xmm13, dqword ptr [aes + 16 * 12].TAesCfbCrc.fAes
        movups  xmm14, dqword ptr [aes + 16 * 13].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [aes + 16 * 14].TAesCfbCrc.fAes
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr [src]
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm15              // fIV := fOut
        add     src, 16
        add     dest, 16
        dec     blocks
        jnz     @s
        movups  dqword ptr [aes].TAesCfbCrc.fIV, xmm7
@z:
end;

procedure AesNiDecryptCfb256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0, dqword ptr [aes + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [aes + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [aes + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [aes + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [aes + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [aes + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [aes + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [aes + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [aes + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [aes + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [aes + 16 * 10].TAesCfbCrc.fAes
        movups  xmm12, dqword ptr [aes + 16 * 11].TAesCfbCrc.fAes
        movups  xmm13, dqword ptr [aes + 16 * 12].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm14, dqword ptr [aes + 16 * 13].TAesCfbCrc.fAes
        movups  xmm15, dqword ptr [aes + 16 * 14].TAesCfbCrc.fAes
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr [src]
        movups  xmm14, xmm15
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm14               // fIV := fIn
        add     src, 16
        add     dest, 16
        dec     blocks
        jnz     @s
        movups  dqword ptr [aes].TAesCfbCrc.fIV, xmm7
@z:
end;

procedure AesNiEncryptCfbCrc128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and 128-bit registers within the loop
        mov     eax, dword ptr [rdx + 0].TAesCfbCrc.fMac.plain
        mov     ebx, dword ptr [rdx + 4].TAesCfbCrc.fMac.plain
        mov     r8d, dword ptr [rdx + 8].TAesCfbCrc.fMac.plain
        mov     r9d, dword ptr [rdx + 12].TAesCfbCrc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted
        movups  xmm0, dqword ptr [rdx + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [rdx + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [rdx + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [rdx + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [rdx + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [rdx + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [rdx + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [rdx + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [rdx + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [rdx + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [rdx + 16 * 10].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [rdi]
        crc32   eax, dword ptr [rdi + 0]
        crc32   ebx, dword ptr [rdi + 4]
        crc32   r8d, dword ptr [rdi + 8]
        crc32   r9d, dword ptr [rdi + 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [rsi], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm15              // fIV := fOut
        crc32   r10d, dword ptr [rsi + 0]
        crc32   r11d, dword ptr [rsi + 4]
        crc32   r12d, dword ptr [rsi + 8]
        crc32   r13d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        dec     ecx
        jnz     @s
        movups  dqword ptr [rdx].TAesCfbCrc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfbCrc128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and 128-bit registers within the loop
        mov     eax, dword ptr [rdx + 0].TAesCfbCrc.fMac.plain
        mov     ebx, dword ptr [rdx + 4].TAesCfbCrc.fMac.plain
        mov     r8d, dword ptr [rdx + 8].TAesCfbCrc.fMac.plain
        mov     r9d, dword ptr [rdx + 12].TAesCfbCrc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted
        movups  xmm0, dqword ptr [rdx + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [rdx + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [rdx + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [rdx + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [rdx + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [rdx + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [rdx + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [rdx + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [rdx + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [rdx + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [rdx + 16 * 10].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [rdi]
        movups  xmm14, xmm15
        crc32   r10d, dword ptr [rdi + 0]
        crc32   r11d, dword ptr [rdi + 4]
        crc32   r12d, dword ptr [rdi + 8]
        crc32   r13d, dword ptr [rdi + 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [rsi], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm14              // fIV := fIn
        crc32   eax, dword ptr [rsi + 0]
        crc32   ebx, dword ptr [rsi + 4]
        crc32   r8d, dword ptr [rsi + 8]
        crc32   r9d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        dec     ecx
        jnz     @s
        movups  dqword ptr [rdx].TAesCfbCrc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

procedure AesNiEncryptCfbCrc256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and (mostly) 128-bit registers within the loop
        mov     eax, dword ptr [rdx + 0].TAesCfbCrc.fMac.plain
        mov     ebx, dword ptr [rdx + 4].TAesCfbCrc.fMac.plain
        mov     r8d, dword ptr [rdx + 8].TAesCfbCrc.fMac.plain
        mov     r9d, dword ptr [rdx + 12].TAesCfbCrc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted
        movups  xmm0, dqword ptr [rdx + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [rdx + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [rdx + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [rdx + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [rdx + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [rdx + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [rdx + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [rdx + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [rdx + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [rdx + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [rdx + 16 * 10].TAesCfbCrc.fAes
        movups  xmm12, dqword ptr [rdx + 16 * 11].TAesCfbCrc.fAes
        movups  xmm13, dqword ptr [rdx + 16 * 12].TAesCfbCrc.fAes
        movups  xmm14, dqword ptr [rdx + 16 * 13].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [rdx + 16 * 14].TAesCfbCrc.fAes
        crc32   eax, dword ptr [rdi + 0]
        crc32   ebx, dword ptr [rdi + 4]
        crc32   r8d, dword ptr [rdi + 8]
        crc32   r9d, dword ptr [rdi + 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr [rdi]
        pxor    xmm15, xmm7
        movups  dqword ptr [rsi], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm15              // fIV := fOut
        crc32   r10d, dword ptr [rsi + 0]
        crc32   r11d, dword ptr [rsi + 4]
        crc32   r12d, dword ptr [rsi + 8]
        crc32   r13d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        dec     ecx
        jnz     @s
        movups  dqword ptr [rdx].TAesCfbCrc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfbCrc256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
        {$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and (mostly) 128-bit registers within the loop
        mov     eax, dword ptr [rdx + 0].TAesCfbCrc.fMac.plain
        mov     ebx, dword ptr [rdx + 4].TAesCfbCrc.fMac.plain
        mov     r8d, dword ptr [rdx + 8].TAesCfbCrc.fMac.plain
        mov     r9d, dword ptr [rdx + 12].TAesCfbCrc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted
        movups  xmm0, dqword ptr [rdx + 16 * 0].TAesCfbCrc.fAes
        movups  xmm1, dqword ptr [rdx + 16 * 1].TAesCfbCrc.fAes
        movups  xmm2, dqword ptr [rdx + 16 * 2].TAesCfbCrc.fAes
        movups  xmm3, dqword ptr [rdx + 16 * 3].TAesCfbCrc.fAes
        movups  xmm4, dqword ptr [rdx + 16 * 4].TAesCfbCrc.fAes
        movups  xmm5, dqword ptr [rdx + 16 * 5].TAesCfbCrc.fAes
        movups  xmm6, dqword ptr [rdx + 16 * 6].TAesCfbCrc.fAes
        movups  xmm8, dqword ptr [rdx + 16 * 7].TAesCfbCrc.fAes
        movups  xmm9, dqword ptr [rdx + 16 * 8].TAesCfbCrc.fAes
        movups  xmm10, dqword ptr [rdx + 16 * 9].TAesCfbCrc.fAes
        movups  xmm11, dqword ptr [rdx + 16 * 10].TAesCfbCrc.fAes
        movups  xmm12, dqword ptr [rdx + 16 * 11].TAesCfbCrc.fAes
        movups  xmm13, dqword ptr [rdx + 16 * 12].TAesCfbCrc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfbCrc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm14, dqword ptr [rdx + 16 * 13].TAesCfbCrc.fAes
        movups  xmm15, dqword ptr [rdx + 16 * 14].TAesCfbCrc.fAes
        crc32   r10d, dword ptr [rdi + 0]
        crc32   r11d, dword ptr [rdi + 4]
        crc32   r12d, dword ptr [rdi + 8]
        crc32   r13d, dword ptr [rdi + 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr [rdi]
        movups  xmm14, xmm15
        pxor    xmm15, xmm7
        movups  dqword ptr [rsi], xmm15  // fOut := fIn xor IV
        movups  xmm7, xmm14              // fIV := fIn
        crc32   eax, dword ptr [rsi + 0]
        crc32   ebx, dword ptr [rsi + 4]
        crc32   r8d, dword ptr [rsi + 8]
        crc32   r9d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        dec     ecx
        jnz     @s
        movups  dqword ptr [rdx].TAesCfbCrc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfbCrc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfbCrc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfbCrc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfbCrc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

{$ifdef USEGCMAVX}

// prepare the GMAC process for gcmavx_data() and gcmavx_end()
procedure GcmAvxInit(ptab, ks: pointer; rounds: cardinal);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=ptab, rdx/rsi=ks, r8/rdx=kslen
        movdqa  xmm15, dqword ptr [rip + @bswapMask]
        movdqa  xmm14, dqword ptr [rip + @gcmPoly]
        movdqu  xmm0, dqword ptr [ks]
        movdqu  xmm11, dqword ptr [ks + 10H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 20H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 30H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 40H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 50H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 60H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 70H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 80H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 90H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 0A0H]
        cmp     rounds, 12
        jc      @last
        // end of AES-128
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 0B0H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 0C0H]
        jz      @last
        // end of AES-192
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 0D0H]
        aesenc  xmm0, xmm11
        movdqu  xmm11, dqword ptr [ks + 0E0H]
        // end of AES-256
@last:  aesenclast xmm0, xmm11
        pshufb  xmm0, xmm15
        pshufd  xmm11, xmm0, 0FFH
        movdqu  xmm12, xmm0
        psrad   xmm11, 31
        pand    xmm11, xmm14
        psrld   xmm12, 31
        pslldq  xmm12, 4
        pslld   xmm0, 1
        pxor    xmm0, xmm11
        pxor    xmm0, xmm12
        movdqu  dqword ptr [ptab + 0E0H], xmm0
        pshufd  xmm1, xmm0, 4EH
        pxor    xmm1, xmm0
        movdqu  dqword ptr [ptab + 0F0H], xmm1
        movdqu  xmm2, xmm0
        movdqu  xmm3, xmm1
        mov     al, 7
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@init:  movdqu  xmm11, xmm2
        movdqu  xmm12, xmm2
        movdqu  xmm13, xmm3
        // pclmulqdq xmm11, xmm0, 00H     @ 66 44: 0F 3A 44. D8, 00
        // pclmulqdq xmm12, xmm0, 11H     @ 66 44: 0F 3A 44. E0, 11
        // pclmulqdq xmm13, xmm1, 00H     @ 66 44: 0F 3A 44. E9, 00
        db $66, $44, $0F, $3A, $44, $D8, $00
        db $66, $44, $0F, $3A, $44, $E0, $11
        db $66, $44, $0F, $3A, $44, $E9, $00
        pxor    xmm13, xmm11
        pxor    xmm13, xmm12
        movdqu  xmm4, xmm13
        pslldq  xmm4, 8
        psrldq  xmm13, 8
        pxor    xmm11, xmm4
        pxor    xmm12, xmm13
        movdqu  xmm2, xmm14
        // pclmulqdq xmm2, xmm11, 01H        @ 66 41: 0F 3A 44. D3, 01
        db $66, $41, $0F, $3A, $44, $D3, $01
        pshufd  xmm11, xmm11, 4EH
        pxor    xmm11, xmm2
        movdqu  xmm2, xmm14
        // pclmulqdq xmm2, xmm11, 01H        @ 66 41: 0F 3A 44. D3, 01
        db $66, $41, $0F, $3A, $44, $D3, $01
        pshufd  xmm11, xmm11, 4EH
        pxor    xmm2, xmm11
        pxor    xmm2, xmm12
        movdqu  dqword ptr [ptab + 0C0H], xmm2
        pshufd  xmm3, xmm2, 4EH
        pxor    xmm3, xmm2
        movdqu  dqword ptr [ptab + 0D0H], xmm3
        lea     ptab, [ptab - 20H]
        dec     al
        jne     @init
        ret
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
end;

// compute GMAC with 8x interleaved pclmulqdq opcode
procedure GcmAvxAuth(ptab, data: pointer; datalen: PtrInt; hash: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rdi=ptab, rsi=data, rdx=datalen, rcx=hash
        movdqu  xmm8, dqword ptr [hash]
        movdqa  xmm15, dqword ptr [rip + @bswapMask]
        movdqa  xmm14, dqword ptr [rip + @gcmPoly]
        test    datalen, datalen
        jz      @done
        cmp     datalen, 128
        jc      @by1
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by8:   sub     datalen, 128
        movdqu  xmm0, dqword ptr [data]
        movdqu  xmm1, dqword ptr [data + 10H]
        movdqu  xmm2, dqword ptr [data + 20H]
        movdqu  xmm3, dqword ptr [data + 30H]
        movdqu  xmm4, dqword ptr [data + 40H]
        movdqu  xmm5, dqword ptr [data + 50H]
        movdqu  xmm6, dqword ptr [data + 60H]
        movdqu  xmm7, dqword ptr [data + 70H]
        lea     data, [data + 80H]
        pshufb  xmm0, xmm15
        pshufb  xmm1, xmm15
        pshufb  xmm2, xmm15
        pshufb  xmm3, xmm15
        pshufb  xmm4, xmm15
        pshufb  xmm5, xmm15
        pshufb  xmm6, xmm15
        pshufb  xmm7, xmm15
        pxor    xmm0, xmm8
        movdqu  xmm8, dqword ptr [ptab]
        movdqu  xmm10, dqword ptr [ptab + 10H]
        movdqu  xmm9, xmm8
        pshufd  xmm12, xmm0, 4EH
        pxor    xmm12, xmm0
        // pclmulqdq xmm8, xmm0, 00H         66 44: 0F 3A 44. C0, 00
        // pclmulqdq xmm9, xmm0, 11H         66 44: 0F 3A 44. C8, 11
        // pclmulqdq xmm10, xmm12, 00H       66 45: 0F 3A 44. D4, 00
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        db $66, $45, $0F, $3A, $44, $D4, $00
        movdqu  xmm12, dqword ptr [ptab + 20H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm1, 00H        66 44: 0F 3A 44. E1, 00
        db $66, $44, $0F, $3A, $44, $E1, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm1, 11H        66 44: 0F 3A 44. E9, 11
        db $66, $44, $0F, $3A, $44, $E9, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm1, 4EH
        pxor    xmm1, xmm12
        movdqu  xmm12, dqword ptr [ptab + 30H]
        // pclmulqdq xmm12, xmm1, 00H        66 44: 0F 3A 44. E1, 00
        db $66, $44, $0F, $3A, $44, $E1, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 40H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm2, 00H        66 44: 0F 3A 44. E2, 00
        db $66, $44, $0F, $3A, $44, $E2, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm2, 11H        66 44: 0F 3A 44. EA, 11
        db $66, $44, $0F, $3A, $44, $EA, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm2, 4EH
        pxor    xmm2, xmm12
        movdqu  xmm12, dqword ptr [ptab + 50H]
        // pclmulqdq xmm12, xmm2, 00H        66 44: 0F 3A 44. E2, 00
        db $66, $44, $0F, $3A, $44, $E2, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 60H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm3, 00H        66 44: 0F 3A 44. E3, 00
        db $66, $44, $0F, $3A, $44, $E3, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm3, 11H        66 44: 0F 3A 44. EB, 11
        db $66, $44, $0F, $3A, $44, $EB, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm3, 4EH
        pxor    xmm3, xmm12
        movdqu  xmm12, dqword ptr [ptab + 70H]
        // pclmulqdq xmm12, xmm3, 00H        66 44: 0F 3A 44. E3, 00
        db $66, $44, $0F, $3A, $44, $E3, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 80H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm4, 00H        66 44: 0F 3A 44. E4, 00
        db $66, $44, $0F, $3A, $44, $E4, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm4, 11H        66 44: 0F 3A 44. EC, 11
        db $66, $44, $0F, $3A, $44, $EC, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm4, 4EH
        pxor    xmm4, xmm12
        movdqu  xmm12, dqword ptr [ptab + 90H]
        // pclmulqdq xmm12, xmm4, 00H        66 44: 0F 3A 44. E4, 00
        db $66, $44, $0F, $3A, $44, $E4, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0A0H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm5, 00H        66 44: 0F 3A 44. E5, 00
        db $66, $44, $0F, $3A, $44, $E5, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm5, 11H        66 44: 0F 3A 44. ED, 11
        db $66, $44, $0F, $3A, $44, $ED, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm5, 4EH
        pxor    xmm5, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0B0H]
        // pclmulqdq xmm12, xmm5, 00H        66 44: 0F 3A 44. E5, 00
        db $66, $44, $0F, $3A, $44, $E5, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0C0H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm6, 00H        66 44: 0F 3A 44. E6, 00
        db $66, $44, $0F, $3A, $44, $E6, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm6, 11H        66 44: 0F 3A 44. EE, 11
        db $66, $44, $0F, $3A, $44, $EE, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm6, 4EH
        pxor    xmm6, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0D0H]
        // pclmulqdq xmm12, xmm6, 00H        66 44: 0F 3A 44. E6, 00
        db $66, $44, $0F, $3A, $44, $E6, $00
        pxor    xmm10, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0E0H]
        movdqu  xmm13, xmm12
        // pclmulqdq xmm12, xmm7, 00H        66 44: 0F 3A 44. E7, 00
        db $66, $44, $0F, $3A, $44, $E7, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm7, 11H        66 44: 0F 3A 44. EF, 11
        db $66, $44, $0F, $3A, $44, $EF, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm7, 4EH
        pxor    xmm7, xmm12
        movdqu  xmm12, dqword ptr [ptab + 0F0H]
        // pclmulqdq xmm12, xmm7, 00H        66 44: 0F 3A 44. E7, 00
        db $66, $44, $0F, $3A, $44, $E7, $00
        pxor    xmm10, xmm12
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movdqu  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        pxor    xmm8, xmm9
        cmp     datalen, 128
        jnc     @by8
@by1:   movdqu  xmm12, dqword ptr [ptab + 0E0H]
        movdqu  xmm13, dqword ptr [ptab + 0F0H]
        cmp     datalen, 16
        jc      @sml
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@next:  sub     datalen, 16
        movdqu  xmm0, dqword ptr [data]
@s:     pshufb  xmm0, xmm15
        pxor    xmm0, xmm8
        movdqu  xmm8, xmm12
        movdqu  xmm10, xmm13
        movdqu  xmm9, xmm12
        pshufd  xmm11, xmm0, 4EH
        pxor    xmm11, xmm0
        // pclmulqdq xmm8, xmm0, 00H         66 44: 0F 3A 44. C0, 00
        // pclmulqdq xmm9, xmm0, 11H         66 44: 0F 3A 44. C8, 11
        // pclmulqdq xmm10, xmm11, 00H       66 45: 0F 3A 44. D3, 00
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        db $66, $45, $0F, $3A, $44, $D3, $00
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movdqu  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        pxor    xmm8, xmm9
        lea     data, [data + 10H]
        cmp     datalen, 16
        jnc     @next
@sml:   test    datalen, datalen
        jz      @done
        pxor    xmm0, xmm0
        lea     data, [data + datalen - 1]
        {$ifdef FPC} align 8 {$else} .align 8 {$endif}
@ins:   pslldq  xmm0, 1
        pinsrb  xmm0, byte ptr [data], 00H
        dec     data
        dec     datalen
        jnz     @ins
        jmp     @s
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
@done:  movdqu  dqword ptr [hash], xmm8
end;

procedure GcmAvxGetTag(ptab, mask, hash: pointer; plen, dlen: PtrInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rdi=ptab, rsi=mask, rdx=hash, rcx=plen, r8=dlen
        {$ifdef WIN64ABI}
        mov     rax, qword ptr [rsp + $28]  // dlen not passed as register
        push    rsi
        push    rdi
        mov     rdi, ptab    // rcx
        mov     rsi, mask    // rdx
        mov     rdx, hash    // r8
        mov     rcx, plen    // r9
        {$else}
        mov     rax, r8
        {$endif WIN64ABI}
        shl     rcx, 3
        movdqu  xmm8, dqword ptr [rdx]
        movdqu  xmm13, dqword ptr [rsi]
        movdqa  xmm15, dqword ptr [rip + @bswapMask]
        movdqa  xmm14, dqword ptr [rip + @gcmPoly]
        shl     rax, 3
        movq    xmm0, rcx
        pinsrq  xmm0, rax, 1
        pxor    xmm0, xmm8
        movdqu  xmm8, dqword ptr [rdi + 0E0H]
        movdqu  xmm10, dqword ptr [rdi + 0F0H]
        movdqu  xmm9, xmm8
        // pclmulqdq xmm8, xmm0, 00H         66 44: 0F 3A 44. C0, 00
        // pclmulqdq xmm9, xmm0, 11H         66 44: 0F 3A 44. C8, 11
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        pshufd  xmm11, xmm0, 4EH
        pxor    xmm11, xmm0
        // pclmulqdq xmm10, xmm11, 00H       66 45: 0F 3A 44. D3, 00
        db $66, $45, $0F, $3A, $44, $D3, $00
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movdqu  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movdqu  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H        66 45: 0F 3A 44. D8, 01
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        pxor    xmm8, xmm9
        pshufb  xmm8, xmm15
        pxor    xmm8, xmm13
        movdqu  dqword ptr [rdx], xmm8
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
        ret
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
end;

{$endif USEGCMAVX}

// compute a := a * b in GF(2^128) using pclmulqdq on WestMere CPUs
// - three times faster than the pascal version using lookup tables
procedure gf_mul_pclmulqdq(a, b: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm0, dqword ptr [a]
        movups  xmm1, dqword ptr [b]
        movdqa  xmm10, dqword ptr [rip + @swap]
        pshufb  xmm0, xmm10
        pshufb  xmm1, xmm10
        movdqa  xmm5, xmm0
        movdqa  xmm4, xmm0
        movdqa  xmm2, xmm0
        // pclmulqdq xmm0, xmm1, 16
        db $66, $0f, $3a, $44, $c1, $10
        // pclmulqdq xmm5, xmm1, 17
        db $66, $0f, $3a, $44, $e9, $11
        movdqa  xmm3, xmm5
        // pclmulqdq xmm4, xmm1, 0
        db $66, $0f, $3a, $44, $e1, $00
        // pclmulqdq xmm2, xmm1, 1
        db $66, $0f, $3a, $44, $d1, $01
        pslldq  xmm3, 8
        pxor    xmm0, xmm2
        movdqa  xmm2, xmm4
        pxor    xmm3, xmm0
        pslldq  xmm2, 8
        punpckhqdq xmm3, xmm5
        movdqa  xmm1, xmm3
        pslldq  xmm0, 8
        pxor    xmm0, xmm4
        pslldq  xmm1, 8
        punpckhqdq xmm2, xmm0
        movdqa  xmm4, xmm2
        movdqa  xmm7, xmm1
        pslldq  xmm4, 8
        movdqa  xmm1, xmm2
        psrlq   xmm4, 63
        psrldq  xmm1, 8
        psllq   xmm3, 1
        movdqa  xmm6, xmm1
        psllq   xmm2, 1
        movdqa  xmm1, xmm3
        por     xmm2, xmm4
        movdqa  xmm3, xmm2
        psrlq   xmm7, 63
        pslldq  xmm3, 8
        por     xmm1, xmm7
        movdqa  xmm4, xmm3
        psrlq   xmm6, 63
        movdqa  xmm0, xmm3
        psllq   xmm4, 63
        por     xmm1, xmm6
        psllq   xmm3, 57
        psllq   xmm0, 62
        pxor    xmm2, xmm3
        pxor    xmm0, xmm4
        pxor    xmm2, xmm0
        movdqa  xmm3, xmm2
        movdqa  xmm7, xmm2
        psrldq  xmm3, 8
        movdqa  xmm0, xmm2
        movdqa  xmm6, xmm2
        movdqa  xmm4, xmm3
        psrlq   xmm7, 1
        movdqa  xmm5, xmm3
        psllq   xmm4, 63
        psllq   xmm3, 57
        por     xmm4, xmm7
        psrlq   xmm0, 7
        por     xmm0, xmm3
        psllq   xmm5, 62
        pxor    xmm1, xmm0
        movdqa  xmm0, xmm4
        psrlq   xmm6, 2
        por     xmm5, xmm6
        pxor    xmm0, xmm5
        pxor    xmm0, xmm1
        pxor    xmm0, xmm2
        pshufb  xmm0, xmm10
        movups  dqword ptr [a], xmm0
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@swap:  db 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
end;

{$endif USEAESNI}


{$ifdef SHA512_X64}

// optimized asm using SSE4 instructions for x64 64-bit

{$ifdef OSWINDOWS}
  {$L ..\..\static\delphi\sha512-x64sse4.obj}
{$else}
  {$L ..\..\static\x86_64-linux\sha512-x64sse4.o}
{$endif OSWINDOWS}

procedure sha512_sse4(data, hash: pointer; blocks: Int64);
  {$ifdef FPC}cdecl;{$endif} external;

{$endif SHA512_X64}


{$ifdef CRC32C_X64}

  { ISCSI CRC 32 Implementation with crc32 and pclmulqdq Instruction
    Copyright(c) 2011-2015 Intel Corporation All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of Intel Corporation nor the names of its
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICESLOSS OF USE,
   DATA, OR PROFITSOR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. }

{$ifdef OSWINDOWS}
  {$L ..\..\static\delphi\crc32c64.obj}
{$else}
  {$ifdef OSLINUX}
  {$L ..\..\static\x86_64-linux\crc32c64.o}
  {$else}
  {$L crc32c64.o}
  {$endif OSLINUX}
{$endif OSWINDOWS}

// defined in mormot.core.crypto.pas, not in mormot.core.base, to avoid
// .o/.obj dependencies for most basic executables (for which mormot.core.base
// crc32c x86_64 asm is alredy fast enough)
function crc32_iscsi_01(buf: PAnsiChar; len: PtrUInt; crc: cardinal): cardinal;
  {$ifdef FPC}cdecl;{$endif} external;

function crc32c_sse42_aesni(crc: PtrUInt; buf: PAnsiChar; len: PtrUInt): cardinal;
{$ifdef FPC}nostackframe; assembler; asm{$else}asm .noframe {$endif}
        mov     rax, crc
        mov     rcx, len
        not     eax
        test    buf, buf
        jz      @0
        cmp     len, 64
        jae     @big
        // it is faster to use a direct 8-bytes loop for blocks < 64 bytes
        shr     len, 3
        jz      @2
        {$ifdef FPC}
        align   16
        // hash 8 bytes per loop
@s:     crc32   rax, qword [buf]
        {$else}
        .align  16
        // circumvent Delphi inline asm compiler bug
@s:     db $F2, $48, $0F, $38, $F1, $02
        {$endif FPC}
        add     buf, 8
        dec     len
        jnz     @s
@2:     test    cl, 4
        jz      @3
        crc32   eax, dword ptr [buf]
        add     buf, 4
@3:     test    cl, 2
        jz      @1
        crc32   eax, word ptr [buf]
        add     buf, 2
@1:     test    cl, 1
        jz      @0
        crc32   eax, byte ptr [buf]
@0:     not     eax
        ret
@big:   // our  call: rcx/rdi=crc rdx/rsi=buf r8/rdx=len
        // iscsi_01:  rcx/rdi=buf rdx/rsi=len r8/rdx=crc
        mov     crc, buf
        mov     buf, len
        mov     len, rax
        call    crc32_iscsi_01
        not     eax
end;

{$endif CRC32C_X64}

