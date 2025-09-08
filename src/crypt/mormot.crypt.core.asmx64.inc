{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86_64 assembly used by mormot.crypt.core.pas
}

{$ifdef FPC}
  // disabled some FPC paranoid warnings
  {$WARN 7102 off : Use of +offset(%ebp) for parameters invalid here }
  {$WARN 7105 off : Use of -offset(%esp), access may cause a crash or value may be lost}
  {$WARN 7121 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits]" }
  {$WARN 7122 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits + $4 byte offset]" }
  {$WARN 7123 off : Check "$1: offset of memory operand is negative "$2 byte" }
{$endif FPC}

{$ifdef ASMX64}

procedure AesEncryptAsm(const ctxt: TAesContext; bi, bo: PBlock128);
{$ifdef FPC} nostackframe; assembler; asm {$else}
asm     // input: rcx/rdi=TAesContext, rdx/rsi=source, r8/rdx=dest
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
        mov     r15,  r8
        mov     r12,  rcx
        {$else}
        mov     r15,  rdx
        mov     rdx,  rsi
        mov     r12,  rdi
        {$endif WIN64ABI}
        movzx   r13,  byte ptr [r12].TAesContext.Rounds
        mov     eax,  dword ptr [rdx]
        mov     ebx,  dword ptr [rdx + 4H]
        mov     ecx,  dword ptr [rdx + 8H]
        mov     edx,  dword ptr [rdx + 0CH]
        xor     eax,  dword ptr [r12]
        xor     ebx,  dword ptr [r12 + 4H]
        xor     ecx,  dword ptr [r12 + 8H]
        xor     edx,  dword ptr [r12 + 0CH]
        sub     r13,  1
        add     r12,  16
        lea     r14,  [rip + Te0]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@round: mov     esi,  eax
        mov     edi,  edx
        movzx   r8d,  al
        movzx   r9d,  cl
        movzx   r10d, bl
        mov     r8d,  dword ptr [r14 + r8 * 4]
        mov     r9d,  dword ptr [r14 + r9 * 4]
        mov     r10d, dword ptr [r14 + r10 * 4]
        shr     esi,  16
        shr     edi,  16
        movzx   ebp,  bh
        xor     r8d,  dword ptr [r14 + rbp * 4 + 400H]
        movzx   ebp,  dh
        xor     r9d,  dword ptr [r14 + rbp * 4 + 400H]
        movzx   ebp,  ch
        xor     r10d, dword ptr [r14 + rbp * 4 + 400H]
        shr     ebx,  16
        shr     ecx,  16
        movzx   ebp,  dl
        mov     edx,  dword ptr [r14 + rbp * 4]
        movzx   ebp,  cl
        xor     r8d,  dword ptr [r14 + rbp * 4 + 800H]
        movzx   ebp,  sil
        xor     r9d,  dword ptr [r14 + rbp * 4 + 800H]
        movzx   r11,  dil
        movzx   eax,  ah
        shr     edi,  8
        movzx   ebp,  bh
        shr     esi,  8
        xor     r10d, dword ptr [r14 + r11 * 4 + 800H]
        xor     edx,  dword ptr [r14 + rax * 4 + 400H]
        xor     r8d,  dword ptr [r14 + rdi * 4 + 0C00H]
        xor     r9d,  dword ptr [r14 + rbp * 4 + 0C00H]
        xor     r10d, dword ptr [r14 + rsi * 4 + 0C00H]
        movzx   ebp,  bl
        xor     edx,  dword ptr [r14 + rbp * 4 + 800H]
        mov     rbx,  r10
        mov     rax,  r8
        movzx   ebp,  ch
        xor     edx,  dword ptr [r14 + rbp * 4 + 0C00H]
        mov     rcx,  r9
        xor     eax,  dword ptr [r12]
        xor     ebx,  dword ptr [r12 + 4H]
        xor     ecx,  dword ptr [r12 + 8H]
        xor     edx,  dword ptr [r12 + 0CH]
        add     r12,  16
        sub     r13,  1
        jnz     @round
        lea     r9,   [rip + SBox]
        movzx   r8,   al
        movzx   r14,  byte ptr [r9 + r8]
        movzx   edi,  bh
        movzx   r8,   byte ptr [r9 + rdi]
        shl     r8d,  8
        xor     r14d, r8d
        mov     r11,  rcx
        shr     r11,  16
        and     r11,  0FFH
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  16
        xor     r14d, r8d
        mov     r11,  rdx
        shr     r11,  24
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12]
        mov     dword ptr [r15], r14d
        movzx   r8,   bl
        movzx   r14,  byte ptr [r9 + r8]
        movzx   edi,  ch
        movzx   r8,   byte ptr [r9 + rdi]
        shl     r8d,  8
        xor     r14d, r8d
        mov     r11,  rdx
        shr     r11,  16
        and     r11,  0FFH
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  16
        xor     r14d, r8d
        mov     r11,  rax
        shr     r11,  24
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12 + 4H]
        mov     dword ptr [r15 + 4H], r14d
        movzx   r8,   cl
        movzx   r14,  byte ptr [r9 + r8]
        movzx   edi,  dh
        movzx   r8,   byte ptr [r9 + rdi]
        shl     r8d,  8
        xor     r14d, r8d
        mov     r11,  rax
        shr     r11,  16
        and     r11,  0FFH
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  16
        xor     r14d, r8d
        mov     r11,  rbx
        shr     r11,  24
        movzx   r8,   byte ptr [r9 + r11]
        shl     r8d,  24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12 + 8H]
        mov     dword ptr [r15 + 8H], r14d
        and     rdx,  0FFH
        movzx   r14,  byte ptr [r9 + rdx]
        movzx   eax,  ah
        movzx   r8,   byte ptr [r9 + rax]
        shl     r8d,  8
        xor     r14d, r8d
        shr     rbx,  16
        and     rbx,  0FFH
        movzx   r8,   byte ptr [r9 + rbx]
        shl     r8d,  16
        xor     r14d, r8d
        shr     rcx,  24
        movzx   r8,   byte ptr [r9 + rcx]
        shl     r8d,  24
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

// optimized unrolled version from Intel's Sha256Sse4.asm
//  Original code is released as Copyright (c) 2012, Intel Corporation
var
  K256Aligned: PBlock2048; // movaps + paddd do expect 16 bytes alignment

const
  STACK_SIZE = 32 {$ifndef SYSVABI} + 7 * 16 {$endif};

procedure Sha256Sse4(var input_data; var digest; num_blks: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else}
asm     // rcx=input_data rdx=digest r8=num_blks (Linux: rdi,rsi,rdx)
        .noframe
{$endif FPC}
        push    rbx
        {$ifdef WIN64ABI}
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$else}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
        {$endif WIN64ABI}
        push    rbp
        push    r13
        push    r14
        push    r15
        sub     rsp, STACK_SIZE
        {$ifdef WIN64ABI}
        movaps  [rsp + 20H], xmm6    // manual .PUSHNV for FPC compatibility
        movaps  [rsp + 30H], xmm7
        movaps  [rsp + 40H], xmm8
        movaps  [rsp + 50H], xmm9
        movaps  [rsp + 60H], xmm10
        movaps  [rsp + 70H], xmm11
        movaps  [rsp + 80H], xmm12
        {$endif WIN64ABI}
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
        sub     rcx, 1
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
@done: {$ifdef WIN64ABI}
        movaps  xmm6, [rsp + 20H]
        movaps  xmm7, [rsp + 30H]
        movaps  xmm8, [rsp + 40H]
        movaps  xmm9, [rsp + 50H]
        movaps  xmm10, [rsp + 60H]
        movaps  xmm11, [rsp + 70H]
        movaps  xmm12, [rsp + 80H]
        {$endif WIN64ABI}
        add     rsp, STACK_SIZE
        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
        pop     rbx
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@flip:  dq      $0405060700010203
        dq      $0c0d0e0f08090a0b
@00BA:  dq      $0b0a090803020100
        dq      $ffffffffffffffff
@DC00:  dq      $ffffffffffffffff
        dq      $0b0a090803020100
end;

// optimized SHA-1 and SHA-256 unrolled asm using Intel SHA HW opcodes
// - those opcodes are not available in FPC/Delphi asm, so are manually encoded

procedure Sha1ni(var input_data; var digest; num_bytes: cardinal);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
{$else} {$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        test    num_bytes, num_bytes
        jz      @0
        movups  xmm5, dqword ptr [digest]
        pxor    xmm1, xmm1
        pinsrd  xmm1, dword ptr [digest + 16], 3
        pshufd  xmm0, xmm5, $1B
        movaps  xmm6, xmm1
        movaps  xmm4, xmm0
        cmp     num_bytes, 63
        jbe     @2
        lea     eax, qword ptr [num_bytes - 64]
        movaps  xmm5, dqword ptr [rip + @3]
        shr     eax, 6
        add     eax, 1
        shl     rax, 6
        add     rax, input_data
@1:     movups  xmm1, dqword ptr [input_data]
        movaps  xmm0, xmm6
        movaps  xmm2, xmm4
        movaps  xmm3, xmm4
        add     input_data, 64
        pshufb  xmm1, xmm5
        paddd   xmm0, xmm1
        // sha1rnds4 xmm2, xmm0, 0
        db $0f, $3a, $cc, $d0, $00 // the sha1* opcodes are encoded by hand
        movups  xmm0, dqword ptr [input_data - 48]
        movaps  xmm8, xmm2
        pshufb  xmm0, xmm5
        // sha1nexte xmm3, xmm0
        db $0f, $38, $c8, $d8
        // sha1msg1 xmm1, xmm0
        db $0f, $38, $c9, $c8
        // sha1rnds4 xmm8, xmm3, 0
        db $44, $0f, $3a, $cc, $c3, $00
        movups  xmm3, dqword ptr [input_data - 32]
        movaps  xmm7, xmm8
        movaps  xmm9, xmm8
        pshufb  xmm3, xmm5
        // sha1nexte xmm2, xmm3
        db $0f, $38, $c8, $d3
        pxor    xmm1, xmm3
        // sha1msg1 xmm0, xmm3
        db $0f, $38, $c9, $c3
        // sha1rnds4 xmm7, xmm2, 0
        db $0f, $3a, $cc, $fa, $00
        movups  xmm2, dqword ptr [input_data - 16]
        movaps  xmm8, xmm7
        pshufb  xmm2, xmm5
        // sha1msg2 xmm1, xmm2
        db $0f, $38, $ca, $ca
        // sha1nexte xmm9, xmm2
        db $44, $0f, $38, $c8, $ca
        // sha1msg1 xmm3, xmm2
        db $0f, $38, $c9, $da
        // sha1rnds4 xmm8, xmm9, 0
        db $45, $0f, $3a, $cc, $c1, $00
        // sha1nexte xmm7, xmm1
        db $0f, $38, $c8, $f9
        pxor    xmm0, xmm2
        movaps  xmm9, xmm7
        movaps  xmm7, xmm8
        // sha1msg2 xmm0, xmm1
        db $0f, $38, $ca, $c1
        // sha1rnds4 xmm7, xmm9, 0
        db $41, $0f, $3a, $cc, $f9, $00
        pxor    xmm3, xmm1
        movaps  xmm9, xmm8
        // sha1msg2 xmm3, xmm0
        db $0f, $38, $ca, $d8
        // sha1nexte xmm9, xmm0
        db $44, $0f, $38, $c8, $c8
        movaps  xmm8, xmm7
        // sha1rnds4 xmm8, xmm9, 1
        db $45, $0f, $3a, $cc, $c1, $01
        // sha1nexte xmm7, xmm3
        db $0f, $38, $c8, $fb
        // sha1msg1 xmm2, xmm1
        db $0f, $38, $c9, $d1
        movaps  xmm9, xmm7
        // sha1msg1 xmm1, xmm0
        db $0f, $38, $c9, $c8
        pxor    xmm2, xmm0
        movaps  xmm7, xmm8
        // sha1msg2 xmm2, xmm3
        db $0f, $38, $ca, $d3
        pxor    xmm1, xmm3
        // sha1rnds4 xmm7, xmm9, 1
        db $41, $0f, $3a, $cc, $f9, $01
        movaps  xmm9, xmm8
        // sha1msg2 xmm1, xmm2
        db $0f, $38, $ca, $ca
        // sha1nexte xmm9, xmm2
        db $44, $0f, $38, $c8, $ca
        movaps  xmm8, xmm7
        // sha1msg1 xmm0, xmm3
        db $0f, $38, $c9, $c3
        // sha1rnds4 xmm8, xmm9, 1
        db $45, $0f, $3a, $cc, $c1, $01
        // sha1nexte xmm7, xmm1
        db $0f, $38, $c8, $f9
        // sha1msg1 xmm3, xmm2
        db $0f, $38, $c9, $da
        movaps  xmm9, xmm7
        pxor    xmm0, xmm2
        movaps  xmm7, xmm8
        // sha1msg2 xmm0, xmm1
        db $0f, $38, $ca, $c1
        // sha1rnds4 xmm7, xmm9, 1
        db $41, $0f, $3a, $cc, $f9, $01
        pxor    xmm3, xmm1
        movaps  xmm9, xmm8
        // sha1msg2 xmm3, xmm0
        db $0f, $38, $ca, $d8
        movaps  xmm8, xmm7
        // sha1nexte xmm9, xmm0
        db $44, $0f, $38, $c8, $c8
        // sha1nexte xmm7, xmm3
        db $0f, $38, $c8, $fb
        // sha1msg1 xmm2, xmm1
        db $0f, $38, $c9, $d1
        // sha1rnds4 xmm8, xmm9, 1
        db $45, $0f, $3a, $cc, $c1, $01
        // sha1msg1 xmm1, xmm0
        db $0f, $38, $c9, $c8
        movaps  xmm9, xmm7
        pxor    xmm2, xmm0
        movaps  xmm7, xmm8
        pxor    xmm1, xmm3
        // sha1msg2 xmm2, xmm3
        db $0f, $38, $ca, $d3
        // sha1rnds4 xmm7, xmm9, 2
        db $41, $0f, $3a, $cc, $f9, $02
        movaps  xmm9, xmm8
        // sha1msg2 xmm1, xmm2
        db $0f, $38, $ca, $ca
        // sha1nexte xmm9, xmm2
        db $44, $0f, $38, $c8, $ca
        movaps  xmm8, xmm7
        // sha1rnds4 xmm8, xmm9, 2
        db $45, $0f, $3a, $cc, $c1, $02
        // sha1nexte xmm7, xmm1
        db $0f, $38, $c8, $f9
        // sha1msg1 xmm0, xmm3
        db $0f, $38, $c9, $c3
        movaps  xmm9, xmm7
        // sha1msg1 xmm3, xmm2
        db $0f, $38, $c9, $da
        pxor    xmm0, xmm2
        movaps  xmm7, xmm8
        // sha1msg2 xmm0, xmm1
        db $0f, $38, $ca, $c1
        pxor    xmm3, xmm1
        // sha1rnds4 xmm7, xmm9, 2
        db $41, $0f, $3a, $cc, $f9, $02
        movaps  xmm9, xmm8
        // sha1msg2 xmm3, xmm0
        db $0f, $38, $ca, $d8
        // sha1nexte xmm9, xmm0
        db $44, $0f, $38, $c8, $c8
        movaps  xmm8, xmm7
        // sha1msg1 xmm2, xmm1
        db $0f, $38, $c9, $d1
        // sha1rnds4 xmm8, xmm9, 2
        db $45, $0f, $3a, $cc, $c1, $02
        // sha1nexte xmm7, xmm3
        db $0f, $38, $c8, $fb
        // sha1msg1 xmm1, xmm0
        db $0f, $38, $c9, $c8
        movaps  xmm9, xmm7
        pxor    xmm2, xmm0
        movaps  xmm7, xmm8
        // sha1msg2 xmm2, xmm3
        db $0f, $38, $ca, $d3
        // sha1rnds4 xmm7, xmm9, 2
        db $41, $0f, $3a, $cc, $f9, $02
        pxor    xmm1, xmm3
        movaps  xmm9, xmm8
        // sha1msg2 xmm1, xmm2
        db $0f, $38, $ca, $ca
        movaps  xmm8, xmm7
        // sha1nexte xmm9, xmm2
        db $44, $0f, $38, $c8, $ca
        // sha1nexte xmm7, xmm1
        db $0f, $38, $c8, $f9
        // sha1msg1 xmm0, xmm3
        db $0f, $38, $c9, $c3
        // sha1rnds4 xmm8, xmm9, 3
        db $45, $0f, $3a, $cc, $c1, $03
        pxor    xmm0, xmm2
        movaps  xmm9, xmm7
        movaps  xmm7, xmm8
        // sha1msg2 xmm0, xmm1
        db $0f, $38, $ca, $c1
        // sha1msg1 xmm3, xmm2
        db $0f, $38, $c9, $da
        // sha1rnds4 xmm7, xmm9, 3
        db $41, $0f, $3a, $cc, $f9, $03
        // sha1msg1 xmm2, xmm1
        db $0f, $38, $c9, $d1
        // sha1nexte xmm8, xmm0
        db $44, $0f, $38, $c8, $c0
        pxor    xmm1, xmm3
        movaps  xmm3, xmm7
        // sha1msg2 xmm1, xmm0
        db $0f, $38, $ca, $c8
        // sha1rnds4 xmm3, xmm8, 3
        db $41, $0f, $3a, $cc, $d8, $03
        pxor    xmm0, xmm2
        // sha1msg2 xmm0, xmm1
        db $0f, $38, $ca, $c1
        // sha1nexte xmm7, xmm1
        db $0f, $38, $c8, $f9
        movaps  xmm1, xmm3
        // sha1rnds4 xmm1, xmm7, 3
        db $0f, $3a, $cc, $cf, $03
        // sha1nexte xmm3, xmm0
        db $0f, $38, $c8, $d8
        movaps  xmm0, xmm1
        // sha1nexte xmm1, xmm6
        db $0f, $38, $c8, $ce
        // sha1rnds4 xmm0, xmm3, 3
        db $0f, $3a, $cc, $c3, $03
        movaps  xmm6, xmm1
        paddd   xmm0, xmm4
        movaps  xmm4, xmm0
        cmp     input_data, rax
        jne     @1
@2:     pshufd  xmm0, xmm0, 1bh
        pextrd  dword ptr [digest + 16], xmm1, 3
        movups  dqword ptr [digest], xmm0
@0:     {$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        jmp     @end
        {$else}
        ret
        {$endif WIN64ABI}
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@3:     dq $08090a0b0c0d0e0f
        dq $0001020304050607
{$ifdef WIN64ABI}
@end:
{$endif WIN64ABI}
end;

procedure Sha256ni(var input_data; var digest; num_blks: cardinal);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else} {$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        test    num_blks, num_blks
        jz      @0
        movups  xmm7, dqword ptr [digest]
        pshufd  xmm0, xmm7, 0B1H
        movups  xmm7, dqword ptr [digest + 16]
        movaps  xmm6, xmm0
        pshufd  xmm7, xmm7, 1BH
        palignr xmm6, xmm7, 08H
        pblendw xmm7, xmm0, 0F0H
        movaps  xmm8,  dqword ptr [rip + @c0]
        movaps  xmm13, dqword ptr [rip + @c1]
        movaps  xmm12, dqword ptr [rip + @c2]
        movaps  xmm11, dqword ptr [rip + @c3]
        movaps  xmm10, dqword ptr [rip + @c4]
        movaps  xmm9,  dqword ptr [rip + @c5]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@1:     movups  xmm5, dqword ptr [input_data]
        movaps  xmm1, xmm7
        movaps  xmm2, xmm6
        add     input_data, 64
        movaps  xmm0, xmm13
        movups  xmm14, dqword ptr [input_data - 48]
        movups  xmm3,  dqword ptr [input_data - 16]
        pshufb  xmm5, xmm8
        movups  xmm4,  dqword ptr [input_data - 32]
        paddd   xmm0, xmm5
        pshufb  xmm14, xmm8
        pshufb  xmm3, xmm8
        db $0f, $38, $cb, $ce // the sha256* opcodes are encoded by hand
        // sha256rnds2 xmm1, xmm6, xmm0
        pshufd  xmm0, xmm0, $0e
        pshufb  xmm4, xmm8
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm12
        movaps  xmm15, xmm3
        paddd   xmm0, xmm14
        palignr xmm15, xmm4, $04
        db $41, $0f, $38, $cc, $ee
        // sha256msg1 xmm5, xmm14
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        pshufd  xmm0, xmm0, $0e
        paddd   xmm5, xmm15
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm11
        db $0f, $38, $cd, $eb
        // sha256msg2 xmm5, xmm3
        paddd   xmm0, xmm4
        movaps  xmm15, xmm5
        db $44, $0f, $38, $cc, $f4
        // sha256msg1 xmm14, xmm4
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cc, $e3
        // sha256msg1 xmm4, xmm3
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm10
        paddd   xmm0, xmm3
        palignr xmm15, xmm3, $04
        db $0f, $38, $cc, $dd
        // sha256msg1 xmm3, xmm5
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        pshufd  xmm0, xmm0, $0e
        paddd   xmm14, xmm15
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm5
        db $44, $0f, $38, $cd, $f5
        // sha256msg2 xmm14, xmm5
        paddd   xmm0, xmm9
        movaps  xmm15, xmm14
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm5, $04
        movaps  xmm0, dqword ptr [rip + @c6]
        db $41, $0f, $38, $cc, $ee
        // sha256msg1 xmm5, xmm14
        paddd   xmm4, xmm15
        db $41, $0f, $38, $cd, $e6
        // sha256msg2 xmm4, xmm14
        paddd   xmm0, xmm14
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm4
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm14, $04
        movaps  xmm0, dqword ptr [rip + @c7]
        db $44, $0f, $38, $cc, $f4
        // sha256msg1 xmm14, xmm4
        paddd   xmm3, xmm15
        db $0f, $38, $cd, $dc
        // sha256msg2 xmm3, xmm4
        paddd   xmm0, xmm4
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm3
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm4, $04
        movaps  xmm0, dqword ptr [rip + @c8]
        db $0f, $38, $cc, $e3
        // sha256msg1 xmm4, xmm3
        paddd   xmm5, xmm15
        db $0f, $38, $cd, $eb
        // sha256msg2 xmm5, xmm3
        paddd   xmm0, xmm3
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm5
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm3, $04
        movaps  xmm0, dqword ptr [rip + @c9]
        db $0f, $38, $cc, $dd
        // sha256msg1 xmm3, xmm5
        paddd   xmm14, xmm15
        db $44, $0f, $38, $cd, $f5
        // sha256msg2 xmm14, xmm5
        paddd   xmm0, xmm5
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm14
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm5, $04
        movaps  xmm0, dqword ptr [rip + @c10]
        db $41, $0f, $38, $cc, $ee
        // sha256msg1 xmm5, xmm14
        paddd   xmm4, xmm15
        db $41, $0f, $38, $cd, $e6
        // sha256msg2 xmm4, xmm14
        paddd   xmm0, xmm14
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm4
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm14, $04
        movaps  xmm0, dqword ptr [rip + @c11]
        db $44, $0f, $38, $cc, $f4
        // sha256msg1 xmm14, xmm4
        paddd   xmm3, xmm15
        db $0f, $38, $cd, $dc
        // sha256msg2 xmm3, xmm4
        paddd   xmm0, xmm4
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm3
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm4, $04
        movaps  xmm0, dqword ptr [rip + @c12]
        db $0f, $38, $cc, $e3
        // sha256msg1 xmm4, xmm3
        paddd   xmm5, xmm15
        db $0f, $38, $cd, $eb
        // sha256msg2 xmm5, xmm3
        paddd   xmm0, xmm3
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm5
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm3, $04
        movaps  xmm0, dqword ptr [rip + @c13]
        db $0f, $38, $cc, $dd
        // sha256msg1 xmm3, xmm5
        paddd   xmm14, xmm15
        db $44, $0f, $38, $cd, $f5
        // sha256msg2 xmm14, xmm5
        paddd   xmm0, xmm5
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm15, xmm14
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        palignr xmm15, xmm5, $04
        movaps  xmm0, dqword ptr [rip + @c14]
        movaps  xmm5, dqword ptr [rip + @c15]
        paddd   xmm4, xmm15
        db $41, $0f, $38, $cd, $e6
        // sha256msg2 xmm4, xmm14
        paddd   xmm0, xmm14
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        paddd   xmm5, xmm4
        pshufd  xmm0, xmm0, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm5
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        movaps  xmm0, xmm4
        palignr xmm0, xmm14, $04
        paddd   xmm3, xmm0
        pshufd  xmm0, xmm5, $0e
        db $0f, $38, $cd, $dc
        // sha256msg2 xmm3, xmm4
        paddd   xmm3, dqword ptr [rip + @c16]
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        movaps  xmm0, xmm3
        db $0f, $38, $cb, $ca
        // sha256rnds2 xmm1, xmm2, xmm0
        pshufd  xmm0, xmm3, $0e
        db $0f, $38, $cb, $d1
        // sha256rnds2 xmm2, xmm1, xmm0
        paddd   xmm7, xmm1
        paddd   xmm6, xmm2
        sub     num_blks, 1
        jne     @1
@2:     pshufd  xmm6, xmm6, $1b
        pshufd  xmm7, xmm7, $b1
        movaps  xmm0, xmm6
        pblendw xmm0, xmm7, $f0
        palignr xmm7, xmm6, $08
        movups  dqword ptr [digest], xmm0
        movups  dqword ptr [digest + 16], xmm7
@0:     {$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        jmp     @end
        {$else}
        ret
        {$endif WIN64ABI}
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@c0:    dq $0405060700010203
        dq $0c0d0e0f08090a0b
@c1:    dq $71374491428a2f98
        dq $e9b5dba5b5c0fbcf
@c2:    dq $59f111f13956c25b
        dq $ab1c5ed5923f82a4
@c3:    dq $12835b01d807aa98
        dq $550c7dc3243185be
@c4:    dq $80deb1fe72be5d74
        dq $c19bf1749bdc06a7
@c5:    dq $efbe4786e49b69c1
        dq $240ca1cc0fc19dc6
@c6:    dq $4a7484aa2de92c6f
        dq $76f988da5cb0a9dc
@c7:    dq $a831c66d983e5152
        dq $bf597fc7b00327c8
@c8:    dq $d5a79147c6e00bf3
        dq $1429296706ca6351
@c9:    dq $2e1b213827b70a85
        dq $53380d134d2c6dfc
@c10:   dq $766a0abb650a7354
        dq $92722c8581c2c92e
@c11:   dq $a81a664ba2bfe8a1
        dq $c76c51a3c24b8b70
@c12:   dq $d6990624d192e819
        dq $106aa070f40e3585
@c13:   dq $1e376c0819a4c116
        dq $34b0bcb52748774c
@c14:   dq $4ed8aa4a391c0cb3
        dq $682e6ff35b9cca4f
@c15:   dq $78a5636f748f82ee
        dq $8cc7020884c87814
@c16:   dd $90befffa, $a4506ceb
        dd $bef9a3f7, $c67178f2
{$ifdef WIN64ABI}
@end:
{$endif WIN64ABI}
end;

// Synopse's x64 asm, optimized for both in+out-order pipelined CPUs
procedure KeccakPermutationKernel(B, A, C: pointer);
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

{$ifdef ASMX64AVXNOCONST}

// our AVX2 version is at least x2 faster than KeccakPermutationKernel() asm
// inspired by Andy Polyakov's keccak1600-avx2.pl from the CRYPTOGAMS project
procedure KeccakPermutationAvx2(A: PQWordArray);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15 and rdi
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        push    rdi
{$else} {$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // + 60H ajustments to keep asm opcodes offsets within -$7f..+$7f range
        lea     r8,  qword ptr [rip + @rhotates_left + 60H]
        lea     r9,  qword ptr [rip + @rhotates_right + 60H]
        lea     rax, qword ptr [rip + @iotas]
        lea     rdi, qword ptr [A + 60H]
        mov     ecx, 24 // cKeccakNumberOfRounds
        vzeroupper
        //vpbroadcastq ymm0, qword ptr [rdi - 60H]
        db $C4, $E2, $7D, $59, $47, $A0
        vmovdqu ymm1, yword ptr [rdi - 58H]
        vmovdqu ymm2, yword ptr [rdi - 38H]
        vmovdqu ymm3, yword ptr [rdi - 18H]
        vmovdqu ymm4, yword ptr [rdi + 8H]
        vmovdqu ymm5, yword ptr [rdi + 28H]
        vmovdqu ymm6, yword ptr [rdi + 48H]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@round: // Theta
        vpshufd  ymm13, ymm2, 4EH
        vpxor    ymm12, ymm5, ymm3
        vpxor    ymm9, ymm4, ymm6
        vpxor    ymm12, ymm12, ymm1
        vpxor    ymm12, ymm12, ymm9
        vpermq   ymm11, ymm12, 93H
        vpxor    ymm13, ymm13, ymm2
        vpermq   ymm7, ymm13, 4EH
        vpsrlq   ymm8, ymm12, 63
        vpaddq   ymm9, ymm12, ymm12
        vpor     ymm8, ymm8, ymm9
        vpermq   ymm15, ymm8, 39H
        vpxor    ymm14, ymm8, ymm11
        vpermq   ymm14, ymm14, 00H
        vpxor    ymm13, ymm13, ymm0
        vpxor    ymm13, ymm13, ymm7
        vpsrlq   ymm7, ymm13, 63
        vpaddq   ymm8, ymm13, ymm13
        vpor     ymm8, ymm8, ymm7
        vpxor    ymm2, ymm2, ymm14
        vpxor    ymm0, ymm0, ymm14
        vpblendd ymm15, ymm15, ymm8, 0C0H
        vpblendd ymm11, ymm11, ymm13, 03H
        vpxor    ymm15, ymm15, ymm11
        // Rho + Pi + pre-Chi shuffle
        vpsllvq  ymm10, ymm2, yword ptr [r8 - 60H]
        vpsrlvq  ymm2, ymm2, yword ptr [r9 - 60H]
        vpor     ymm2, ymm2, ymm10
        vpxor    ymm3, ymm3, ymm15
        vpsllvq  ymm11, ymm3, yword ptr [r8 - 20H]
        vpsrlvq  ymm3, ymm3, yword ptr [r9 - 20H]
        vpor     ymm3, ymm3, ymm11
        vpxor    ymm4, ymm4, ymm15
        vpsllvq  ymm12, ymm4, yword ptr [r8]
        vpsrlvq  ymm4, ymm4, yword ptr [r9]
        vpor     ymm4, ymm4, ymm12
        vpxor    ymm5, ymm5, ymm15
        vpsllvq  ymm13, ymm5, yword ptr [r8 + 20H]
        vpsrlvq  ymm5, ymm5, yword ptr [r9 + 20H]
        vpor     ymm5, ymm5, ymm13
        vpxor    ymm6, ymm6, ymm15
        vpermq   ymm10, ymm2, 8DH
        vpermq   ymm11, ymm3, 8DH
        vpsllvq  ymm14, ymm6, yword ptr [r8 + 40H]
        vpsrlvq  ymm8, ymm6, yword ptr [r9 + 40H]
        vpor     ymm8, ymm8, ymm14
        vpxor    ymm1, ymm1, ymm15
        vpermq   ymm12, ymm4, 1BH
        vpermq   ymm13, ymm5, 72H
        vpsllvq  ymm15, ymm1, yword ptr [r8 - 40H]
        vpsrlvq  ymm9, ymm1, yword ptr [r9 - 40H]
        vpor     ymm9, ymm9, ymm15
        // Chi
        vpsrldq  ymm14, ymm8, 8
        vpandn   ymm7, ymm8, ymm14
        vpblendd ymm3, ymm9, ymm13, 0CH
        vpblendd ymm15, ymm11, ymm9, 0CH
        vpblendd ymm5, ymm10, ymm11, 0CH
        vpblendd ymm14, ymm9, ymm10, 0CH
        vpblendd ymm3, ymm3, ymm11, 30H
        vpblendd ymm15, ymm15, ymm12, 30H
        vpblendd ymm5, ymm5, ymm9, 30H
        vpblendd ymm14, ymm14, ymm13, 30H
        vpblendd ymm3, ymm3, ymm12, 0C0H
        vpblendd ymm15, ymm15, ymm13, 0C0H
        vpblendd ymm5, ymm5, ymm13, 0C0H
        vpblendd ymm14, ymm14, ymm11, 0C0H
        vpandn   ymm3, ymm3, ymm15
        vpandn   ymm5, ymm5, ymm14
        vpblendd ymm6, ymm12, ymm9, 0CH
        vpblendd ymm15, ymm10, ymm12, 0CH
        vpxor    ymm3, ymm3, ymm10
        vpblendd ymm6, ymm6, ymm10, 30H
        vpblendd ymm15, ymm15, ymm11, 30H
        vpxor    ymm5, ymm5, ymm12
        vpblendd ymm6, ymm6, ymm11, 0C0H
        vpblendd ymm15, ymm15, ymm9, 0C0H
        vpandn   ymm6, ymm6, ymm15
        vpxor    ymm6, ymm6, ymm13
        vpermq   ymm4, ymm8, 1EH
        vpblendd ymm15, ymm4, ymm0, 30H
        vpermq   ymm1, ymm8, 39H
        vpblendd ymm1, ymm1, ymm0, 0C0H
        vpandn   ymm1, ymm1, ymm15
        vpblendd ymm2, ymm11, ymm12, 0CH
        vpblendd ymm14, ymm13, ymm11, 0CH
        vpblendd ymm2, ymm2, ymm13, 30H
        vpblendd ymm14, ymm14, ymm10, 30H
        vpblendd ymm2, ymm2, ymm10, 0C0H
        vpblendd ymm14, ymm14, ymm12, 0C0H
        vpandn   ymm2, ymm2, ymm14
        vpxor    ymm2, ymm2, ymm9
        vpermq   ymm7, ymm7, 00H
        vpermq   ymm3, ymm3, 1BH
        vpermq   ymm5, ymm5, 8DH
        vpermq   ymm6, ymm6, 72H
        vpblendd ymm4, ymm13, ymm10, 0CH
        vpblendd ymm14, ymm12, ymm13, 0CH
        vpblendd ymm4, ymm4, ymm12, 30H
        vpblendd ymm14, ymm14, ymm9, 30H
        vpblendd ymm4, ymm4, ymm9, 0C0H
        vpblendd ymm14, ymm14, ymm10, 0C0H
        vpandn   ymm4, ymm4, ymm14
        vpxor    ymm0, ymm0, ymm7
        vpxor    ymm1, ymm1, ymm8
        vpxor    ymm4, ymm4, ymm11
        // Iota
        vpxor   ymm0, ymm0, yword ptr [rax]
        add     rax, 32
        dec     ecx
        jnz     @round
        vmovq   qword ptr [rdi - 60H], xmm0
        vmovdqu yword ptr [rdi - 58H], ymm1
        vmovdqu yword ptr [rdi - 38H], ymm2
        vmovdqu yword ptr [rdi - 18H], ymm3
        vmovdqu yword ptr [rdi + 8H], ymm4
        vmovdqu yword ptr [rdi + 28H], ymm5
        vmovdqu yword ptr [rdi + 48H], ymm6
        vzeroupper
        {$ifdef WIN64ABI}
        jmp @w64end
        {$else}
        ret
        {$endif WIN64ABI}
        {$ifdef FPC} align 32 {$else} .align 16 {$endif}
        // note: on AVX2, constant alignment seems not required by above opcodes
@rhotates_left:
        dq     3,   18,    36,    41   // [2][0] [4][0] [1][0] [3][0]
        dq     1,   62,    28,    27   // [0][1] [0][2] [0][3] [0][4]
        dq    45,    6,    56,    39   // [3][1] [1][2] [4][3] [2][4]
        dq    10,   61,    55,     8   // [2][1] [4][2] [1][3] [3][4]
        dq     2,   15,    25,    20   // [4][1] [3][2] [2][3] [1][4]
        dq    44,   43,    21,    14   // [1][1] [2][2] [3][3] [4][4]
@rhotates_right:
        dq    64 - 3,  64 - 18,  64 - 36,  64 - 41
        dq    64 - 1,  64 - 62,  64 - 28,  64 - 27
        dq    64 - 45, 64 - 6,   64 - 56,  64 - 39
        dq    64 - 10, 64 - 61,  64 - 55,  64 - 8
        dq    64 - 2,  64 - 15,  64 - 25,  64 - 20
        dq    64 - 44, 64 - 43,  64 - 21,  64 - 14
@iotas:
   dq $0000000000000001, $0000000000000001, $0000000000000001, $0000000000000001
   dq $0000000000008082, $0000000000008082, $0000000000008082, $0000000000008082
   dq $800000000000808a, $800000000000808a, $800000000000808a, $800000000000808a
   dq $8000000080008000, $8000000080008000, $8000000080008000, $8000000080008000
   dq $000000000000808b, $000000000000808b, $000000000000808b, $000000000000808b
   dq $0000000080000001, $0000000080000001, $0000000080000001, $0000000080000001
   dq $8000000080008081, $8000000080008081, $8000000080008081, $8000000080008081
   dq $8000000000008009, $8000000000008009, $8000000000008009, $8000000000008009
   dq $000000000000008a, $000000000000008a, $000000000000008a, $000000000000008a
   dq $0000000000000088, $0000000000000088, $0000000000000088, $0000000000000088
   dq $0000000080008009, $0000000080008009, $0000000080008009, $0000000080008009
   dq $000000008000000a, $000000008000000a, $000000008000000a, $000000008000000a
   dq $000000008000808b, $000000008000808b, $000000008000808b, $000000008000808b
   dq $800000000000008b, $800000000000008b, $800000000000008b, $800000000000008b
   dq $8000000000008089, $8000000000008089, $8000000000008089, $8000000000008089
   dq $8000000000008003, $8000000000008003, $8000000000008003, $8000000000008003
   dq $8000000000008002, $8000000000008002, $8000000000008002, $8000000000008002
   dq $8000000000000080, $8000000000000080, $8000000000000080, $8000000000000080
   dq $000000000000800a, $000000000000800a, $000000000000800a, $000000000000800a
   dq $800000008000000a, $800000008000000a, $800000008000000a, $800000008000000a
   dq $8000000080008081, $8000000080008081, $8000000080008081, $8000000080008081
   dq $8000000000008080, $8000000000008080, $8000000000008080, $8000000000008080
   dq $0000000080000001, $0000000080000001, $0000000080000001, $0000000080000001
   dq $8000000080008008, $8000000080008008, $8000000080008008, $8000000080008008
        {$ifdef WIN64ABI}
@w64end:pop     rdi
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

{$endif ASMX64AVXNOCONST}

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
        sub     bits, 1
        jne     @s
end;

procedure bswap256(s, d: PIntegerArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax,  dword ptr [s]
        mov     r8d,  dword ptr [s + 4]
        mov     r9d,  dword ptr [s + 8]
        mov     r10d, dword ptr [s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr [d], eax
        mov     dword ptr [d + 4], r8d
        mov     dword ptr [d + 8], r9d
        mov     dword ptr [d + 12], r10d
        mov     eax,  dword ptr [s + 16]
        mov     r8d,  dword ptr [s + 20]
        mov     r9d,  dword ptr [s + 24]
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
        mov     r11d, dword ptr [s + 16]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        bswap   r11d
        mov     dword ptr [d], eax
        mov     dword ptr [d + 4], r8d
        mov     dword ptr [d + 8], r9d
        mov     dword ptr [d + 12], r10d
        mov     dword ptr [d + 16], r11d
end;

function _add256(out Output: THash256Rec; const Left, Right: THash256Rec): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Output rdx/rsi=Left r8/rdx=Right
        xor    eax,  eax
        mov    r9,   qword ptr [Left]
        mov    r10,  qword ptr [Left + 8]
        mov    r11,  qword ptr [Left + 16]
        mov    Left, qword ptr [Left + 24]
        add    r9,   qword ptr [Right]
        adc    r10,  qword ptr [Right + 8]
        adc    r11,  qword ptr [Right + 16]
        adc    Left, qword ptr [Right + 24]
        mov    qword ptr [Output], r9
        mov    qword ptr [Output + 8], r10
        mov    qword ptr [Output + 16], r11
        mov    qword ptr [Output + 24], Left
        adc    eax,  eax
end;

function _inc256(var Value: THash256Rec; const Added: THash256Rec): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Added
        xor    eax, eax
        mov    r8,  qword ptr [Added]
        mov    r9 , qword ptr [Added + 8]
        mov    r10, qword ptr [Added + 16]
        mov    r11, qword ptr [Added + 24]
        add    qword ptr [Value], r8
        adc    qword ptr [Value + 8], r9
        adc    qword ptr [Value + 16], r10
        adc    qword ptr [Value + 24], r11
        adc    eax, eax
end;

function _dec256(var Value: THash256Rec; const Subs: THash256Rec): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Subs
        xor    eax, eax
        mov    r8,  qword ptr [Subs]
        mov    r9 , qword ptr [Subs + 8]
        mov    r10, qword ptr [Subs + 16]
        mov    r11, qword ptr [Subs + 24]
        sub    qword ptr [Value], r8
        sbb    qword ptr [Value + 8], r9
        sbb    qword ptr [Value + 16], r10
        sbb    qword ptr [Value + 24], r11
        adc    eax, eax
end;

function _sub256(out Output: THash256Rec; const Left, Right: THash256Rec): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Output rdx/rsi=Left r8/rdx=Right
        xor    eax,  eax
        mov    r9,   qword ptr [Left]
        mov    r10,  qword ptr [Left + 8]
        mov    r11,  qword ptr [Left + 16]
        mov    Left, qword ptr [Left + 24]
        sub    r9,   qword ptr [Right]
        sbb    r10,  qword ptr [Right + 8]
        sbb    r11,  qword ptr [Right + 16]
        sbb    Left, qword ptr [Right + 24]
        mov    qword ptr [Output], r9
        mov    qword ptr [Output + 8], r10
        mov    qword ptr [Output + 16], r11
        mov    qword ptr [Output+ 24], Left
        adc    eax,  eax
end;

procedure _rshift1(var V: THash256Rec);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     rax, qword ptr [V + 3 * 8]
        mov     rdx, qword ptr [V + 2 * 8]
        mov     r9,  qword ptr [V + 1 * 8]
        mov     r10, qword ptr [V + 0 * 8]
        mov     r8,  rdx
        mov     r11, r9
        shrd    rdx, rax, 1     // 2 3
        shr     rax, 1          // 3
        shrd    r9, r8, 1       // 1 2
        shrd    r10, r11, 1     // 0 1
        mov     qword ptr [V + 3 * 8], rax
        mov     qword ptr [V + 2 * 8], rdx
        mov     qword ptr [V + 1 * 8], r9
        mov     qword ptr [V + 0 * 8], r10
end;

{$ifdef FPC} // constref to force passing by reference (as Win64/Delphi does)
procedure _mult128(constref l, r: THash128Rec; out product: THash256Rec);
  nostackframe; assembler; asm
{$else}
procedure _mult128(const l, r: THash128Rec; out product: THash256Rec);
asm
        .noframe
{$endif FPC}
        // rcx/rdi=l rdx/rsi=r r8/rdx=product
        {$ifdef WIN64ABI}
        // -> preserve rdx=r, which is used as result by mul opcode
        push   rsi
        mov    rsi, r
        {$else}
        mov    r8, product
        {$endif WIN64ABI}
        // t1.V := l.L * r.L;
        xor    r9, r9
        mov    r10, qword ptr [rsi].THash128Rec.L
        mov    rax, qword ptr [l].THash128Rec.L
        mul    r10
        mov    qword ptr [r8].THash256Rec.L.L, rax
        mov    r11, rdx
        // t2.V := l.H * r.L + t1.H;
        mov    rax, qword ptr [l].THash128Rec.H
        mul    r10
        add    r11, rax
        adc    rdx, r9
        mov    r10, rdx
        // t3.V := l.L * r.H + t2.L;
        mov    rax, qword ptr [l].THash128Rec.L
        mul    qword ptr [rsi].THash128Rec.H
        add    rax, r11
        adc    rdx, r9
        mov    qword ptr [r8].THash256Rec.L.H, rax
        mov    r11, rdx
        // product.H := l.H * r.H + t2.H + t3.H;
        mov    rax, qword ptr [l].THash128Rec.H
        mul    qword ptr [rsi].THash128Rec.H
        add    rax, r10
        adc    rdx, r9
        add    rax, r11
        adc    rdx, r9
        mov    qword ptr [r8].THash256Rec.H.L, rax
        mov    qword ptr [r8].THash256Rec.H.H, rdx
        // product.L := t3.V shl 64 or t1.L;
        {$ifdef WIN64ABI}
        pop    rsi
        {$endif WIN64ABI}
end;

procedure _mult256(out Output: THash512Rec; const Left, Right: THash256Rec);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Output rdx/rsi=Left r8/rdx=Right
        {$ifdef WIN64ABI}
        push    rdi
        push    rsi
        mov     rdi, Output
        {$endif WIN64ABI}
        push    r15
        push    r14
        push    r13
        push    r12
        mov     r12, qword ptr [Right]
        mov     r13, qword ptr [Right + 8H]
        mov     r14, qword ptr [Right + 10H]
        mov     r15, qword ptr [Right + 18H]
        mov     r8,  qword ptr [Left]
        mov     r9,  qword ptr [Left + 8H]
        mov     r10, qword ptr [Left + 10H]
        mov     r11, qword ptr [Left + 18H]
        xor     rsi, rsi
        mov     rax, r8
        mul     r12
        mov     qword ptr [rdi], rax
        mov     rcx, rdx
        mov     rax, r9
        mul     r12
        add     rax, rcx
        adc     rdx, rsi
        mov     qword ptr [rdi + 8H], rax
        mov     rcx, rdx
        mov     rax, r10
        mul     r12
        add     rax, rcx
        adc     rdx, rsi
        mov     qword ptr [rdi + 10H], rax
        mov     rcx, rdx
        mov     rax, r11
        mul     r12
        add     rax, rcx
        adc     rdx, rsi
        mov     qword ptr [rdi + 18H], rax
        mov     qword ptr [rdi + 20H], rdx
        mov     rax, r8
        mul     r13
        add     qword ptr [rdi + 8H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r9
        mul     r13
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 10H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r10
        mul     r13
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 18H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r11
        mul     r13
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 20H], rax
        adc     rdx, rsi
        mov     qword ptr [rdi + 28H], rdx
        mov     rax, r8
        mul     r14
        add     qword ptr [rdi + 10H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r9
        mul     r14
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 18H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r10
        mul     r14
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 20H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r11
        mul     r14
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 28H], rax
        adc     rdx, rsi
        mov     qword ptr [rdi + 30H], rdx
        mov     rax, r8
        mul     r15
        add     qword ptr [rdi + 18H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r9
        mul     r15
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 20H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r10
        mul     r15
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 28H], rax
        adc     rdx, rsi
        mov     rcx, rdx
        mov     rax, r11
        mul     r15
        add     rax, rcx
        adc     rdx, rsi
        add     qword ptr [rdi + 30H], rax
        adc     rdx, rsi
        mov     qword ptr [rdi + 38H], rdx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
        {$ifdef WIN64ABI}
        pop     rsi
        pop     rdi
        {$endif WIN64ABI}
end;

procedure _inc64(var Value: THash128Rec; var Added: QWord);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Added
        mov    rax, qword ptr [Added]
        xor    rdx, rdx
        add    qword ptr [Value], rax
        adc    qword ptr [Value + 8], rdx
end;

procedure _inc128(var Value: THash256Rec; var Added: THash128Rec);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Added
        mov    rax, qword ptr [Added]
        mov    rdx, qword ptr [Added + 8]
        xor    r8, r8
        add    qword ptr [Value], rax
        adc    qword ptr [Value + 8], rdx
        adc    qword ptr [Value + 16], r8
        adc    qword ptr [Value + 24], r8
end;

// use CF flag to propagate 64-bit additions carry
function _xasmadd(Value, Adds: pointer; Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Adds r8/rdx=Carry
        xor    eax, eax
        mov    r9, qword ptr [Adds]
        add    r9, Carry
        mov    r8 , qword ptr [Adds + 8]
        mov    r10, qword ptr [Adds + 8 * 2]
        mov    r11, qword ptr [Adds + 8 * 3]
        add    qword ptr [Value], r9
        adc    qword ptr [Value + 8], r8
        adc    qword ptr [Value + 8 * 2], r10
        adc    qword ptr [Value + 8 * 3], r11
        mov    r9,  qword ptr [Adds + 8 * 4]
        mov    r8 , qword ptr [Adds + 8 * 5]
        mov    r10, qword ptr [Adds + 8 * 6]
        mov    r11, qword ptr [Adds + 8 * 7]
        adc    qword ptr [Value + 8 * 4], r9
        adc    qword ptr [Value + 8 * 5], r8
        adc    qword ptr [Value + 8 * 6], r10
        adc    qword ptr [Value + 8 * 7], r11
        mov    r9,  qword ptr [Adds + 8 * 8]
        mov    r8 , qword ptr [Adds + 8 * 9]
        mov    r10, qword ptr [Adds + 8 * 10]
        mov    r11, qword ptr [Adds + 8 * 11]
        adc    qword ptr [Value + 8 * 8], r9
        adc    qword ptr [Value + 8 * 9], r8
        adc    qword ptr [Value + 8 * 10], r10
        adc    qword ptr [Value + 8 * 11], r11
        mov    r9,  qword ptr [Adds + 8 * 12]
        mov    r8 , qword ptr [Adds + 8 * 13]
        mov    r10, qword ptr [Adds + 8 * 14]
        mov    r11, qword ptr [Adds + 8 * 15]
        adc    qword ptr [Value + 8 * 12], r9
        adc    qword ptr [Value + 8 * 13], r8
        adc    qword ptr [Value + 8 * 14], r10
        adc    qword ptr [Value + 8 * 15], r11
        adc    eax, eax // return current carry as 0/1
end;

// use CF flag to propagate 64-bit substractions carry
function _xasmsub(Value, Subs: pointer; Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Subs r8/rdx=Carry
        xor    eax, eax
        mov    r9,  qword ptr [Subs]
        add    r9, Carry
        mov    r8 , qword ptr [Subs + 8]
        mov    r10, qword ptr [Subs + 8 * 2]
        mov    r11, qword ptr [Subs + 8 * 3]
        sub    qword ptr [Value], r9
        sbb    qword ptr [Value + 8], r8
        sbb    qword ptr [Value + 8 * 2], r10
        sbb    qword ptr [Value + 8 * 3], r11
        mov    r9,  qword ptr [Subs + 8 * 4]
        mov    r8 , qword ptr [Subs + 8 * 5]
        mov    r10, qword ptr [Subs + 8 * 6]
        mov    r11, qword ptr [Subs + 8 * 7]
        sbb    qword ptr [Value + 8 * 4], r9
        sbb    qword ptr [Value + 8 * 5], r8
        sbb    qword ptr [Value + 8 * 6], r10
        sbb    qword ptr [Value + 8 * 7], r11
        mov    r9,  qword ptr [Subs + 8 * 8]
        mov    r8 , qword ptr [Subs + 8 * 9]
        mov    r10, qword ptr [Subs + 8 * 10]
        mov    r11, qword ptr [Subs + 8 * 11]
        sbb    qword ptr [Value + 8 * 8], r9
        sbb    qword ptr [Value + 8 * 9], r8
        sbb    qword ptr [Value + 8 * 10], r10
        sbb    qword ptr [Value + 8 * 11], r11
        mov    r9,  qword ptr [Subs + 8 * 12]
        mov    r8 , qword ptr [Subs + 8 * 13]
        mov    r10, qword ptr [Subs + 8 * 14]
        mov    r11, qword ptr [Subs + 8 * 15]
        sbb    qword ptr [Value + 8 * 12], r9
        sbb    qword ptr [Value + 8 * 13], r8
        sbb    qword ptr [Value + 8 * 14], r10
        sbb    qword ptr [Value + 8 * 15], r11
        adc    eax, eax // return current carry as 0/1
end;

// use "mul" opcode to compute 64-bit * 64-bit into 128-bit
function _xasmmul(Src, Dst: pointer; Factor, Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Src rdx/rsi=Dst r8/rdx=Factor r9/rcx=Carry
        {$ifdef WIN64ABI}
        push   rsi
        push   rdi
        mov    rsi, Dst
        mov    rdi, Src
        mov    rcx, Carry
        {$endif WIN64ABI}
        xor    r10, r10
        mov    r11, Factor
        mov    rax, qword ptr [rdi]
        mul    r11         // rax:rdx = [Src] * Factor
        add    rax, rcx
        adc    rdx, r10    // rax:rdx = ([Src] * Factor) + Carry
        mov    qword ptr [rsi], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 1]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 1], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 2]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 2], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 3]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 3], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 4]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 4], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 5]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 5], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 6]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 6], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 7]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 7], rax
        mov    rax, rdx
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

// use "div" opcode to divide 128-bit into 64-bit * 64-bit result
function _xasmdiv(Value: pointer; Factor, Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Factor r8/rdx=Carry
        {$ifdef WIN64ABI}
        push   rsi
        push   rdi
        mov    rdi, Value
        mov    rsi, Factor
        mov    rdx, Carry
        {$endif WIN64ABI}
        mov    rax, qword ptr [rdi + 8 * 15]
        div    rsi // rax = rdx:rax div rsi (rdx = modulo for next div)
        mov    qword ptr [rdi + 8 * 15], rax
        mov    rax, qword ptr [rdi + 8 * 14]
        div    rsi
        mov    qword ptr [rdi + 8 * 14], rax
        mov    rax, qword ptr [rdi + 8 * 13]
        div    rsi
        mov    qword ptr [rdi + 8 * 13], rax
        mov    rax, qword ptr [rdi + 8 * 12]
        div    rsi
        mov    qword ptr [rdi + 8 * 12], rax
        mov    rax, qword ptr [rdi + 8 * 11]
        div    rsi
        mov    qword ptr [rdi + 8 * 11], rax
        mov    rax, qword ptr [rdi + 8 * 10]
        div    rsi
        mov    qword ptr [rdi + 8 * 10], rax
        mov    rax, qword ptr [rdi + 8 * 9]
        div    rsi
        mov    qword ptr [rdi + 8 * 9], rax
        mov    rax, qword ptr [rdi + 8 * 8]
        div    rsi
        mov    qword ptr [rdi + 8 * 8], rax
        mov    rax, qword ptr [rdi + 8 * 7]
        div    rsi
        mov    qword ptr [rdi + 8 * 7], rax
        mov    rax, qword ptr [rdi + 8 * 6]
        div    rsi
        mov    qword ptr [rdi + 8 * 6], rax
        mov    rax, qword ptr [rdi + 8 * 5]
        div    rsi
        mov    qword ptr [rdi + 8 * 5], rax
        mov    rax, qword ptr [rdi + 8 * 4]
        div    rsi
        mov    qword ptr [rdi + 8 * 4], rax
        mov    rax, qword ptr [rdi + 8 * 3]
        div    rsi
        mov    qword ptr [rdi + 8 * 3], rax
        mov    rax, qword ptr [rdi + 8 * 2]
        div    rsi
        mov    qword ptr [rdi + 8 * 2], rax
        mov    rax, qword ptr [rdi + 8 * 1]
        div    rsi
        mov    qword ptr [rdi + 8 * 1], rax
        mov    rax, qword ptr [rdi]
        div    rsi
        mov    qword ptr [rdi], rax
        mov    rax, rdx // return carry/modulo
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

// use "div" opcode to divide 128-bit into 64-bit * 64-bit result
function _xasmmod(Value: pointer; Factor, Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Value rdx/rsi=Factor r8/rdx=Carry
        {$ifdef WIN64ABI}
        push   rsi
        push   rdi
        mov    rdi, Value
        mov    rsi, Factor
        mov    rdx, Carry
        {$endif WIN64ABI}
        mov    rax, qword ptr [rdi + 8 * 15]
        div    rsi // rax = rdx:rax div rsi (rdx = modulo for next div)
        mov    rax, qword ptr [rdi + 8 * 14]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 13]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 12]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 11]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 10]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 9]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 8]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 7]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 6]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 5]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 4]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 3]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 2]
        div    rsi
        mov    rax, qword ptr [rdi + 8 * 1]
        div    rsi
        mov    rax, qword ptr [rdi]
        div    rsi
        mov    rax, rdx // return carry/modulo
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

// use "mul" opcode to compute 64-bit * 64-bit into 128-bit
function _xasmmuladd(Src, Dst: pointer; Factor, Carry: PtrUInt): PtrUInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=Src rdx/rsi=Dst r8/rdx=Factor r9/rcx=Carry
        {$ifdef WIN64ABI}
        push   rsi
        push   rdi
        mov    rsi, Dst
        mov    rdi, Src
        mov    rcx, Carry
        {$endif WIN64ABI}
        xor    r10, r10
        mov    r11, Factor
        mov    rax, qword ptr [rdi]
        mul    r11         // rax:rdx = [Src] * Factor
        add    rax, rcx
        adc    rdx, r10   // rax:rdx = [Src] * Factor + Carry
        add    qword ptr [rsi], rax
        adc    rdx, r10   // [Dst]:rdx = [Src] * Factor + [Dst] + Carry
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 1]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 1], rax
        adc    rdx, r10
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 2]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 2], rax
        adc    rdx, r10
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 3]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 3], rax
        adc    rdx, r10
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 4]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 4], rax
        adc    rdx, r10
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 5]
        mul    r11
        add    rax, qword ptr [rsi + 8 * 5]
        adc    rdx, r10
        add    rax, rcx
        adc    rdx, r10
        mov    qword ptr [rsi + 8 * 5], rax
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 6]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 6], rax
        adc    rdx, r10
        mov    rcx, rdx
        mov    rax, qword ptr [rdi + 8 * 7]
        mul    r11
        add    rax, rcx
        adc    rdx, r10
        add    qword ptr [rsi + 8 * 7], rax
        adc    rdx, r10
        mov    rax, rdx
        {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

procedure Xor512(dst, src: PPtrIntArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        movups  xmm0, dqword ptr [dst]
        movups  xmm1, dqword ptr [dst + 16]
        movups  xmm2, dqword ptr [dst + 32]
        movups  xmm3, dqword ptr [dst + 48]
        movups  xmm4, dqword ptr [src] // xorps xmm,[mem] requires alignment
        movups  xmm5, dqword ptr [src + 16]
        {$ifdef WIN64ABI} // Win64 requires to preserve xmm6/xmm7
        xorps   xmm0, xmm4
        xorps   xmm1, xmm5
        movups  xmm4, dqword ptr [src + 32]
        movups  xmm5, dqword ptr [src + 48]
        xorps   xmm2, xmm4
        xorps   xmm3, xmm5
        {$else}
        movups  xmm6, dqword ptr [src + 32]
        movups  xmm7, dqword ptr [src + 48]
        xorps   xmm0, xmm4
        xorps   xmm1, xmm5
        xorps   xmm2, xmm6
        xorps   xmm3, xmm7
        {$endif WIN64ABI}
        movups  dqword ptr [dst], xmm0
        movups  dqword ptr [dst + 16], xmm1
        movups  dqword ptr [dst + 32], xmm2
        movups  dqword ptr [dst + 48], xmm3
end;

procedure Move512(dst, src: PPtrIntArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        movups  xmm0, dqword ptr [src]
        movups  xmm1, dqword ptr [src + 16]
        movups  xmm2, dqword ptr [src + 32]
        movups  xmm3, dqword ptr [src + 48]
        movups  dqword ptr [dst], xmm0
        movups  dqword ptr [dst + 16], xmm1
        movups  dqword ptr [dst + 32], xmm2
        movups  dqword ptr [dst + 48], xmm3
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

procedure Sha1ExpandMessageBlocks(W: PCardinalArray; n: cardinal);
//    semi-unrolled optimized asm version by A. Bouchez
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
      // rcx=W rdx=n (Linux: rdi=W rsi=n)
@s:   mov     eax,  dword ptr [W]
      mov     r8d,  dword ptr [W + 4]
      mov     r9d,  dword ptr [W + 8]
      mov     r10d, dword ptr [W + 12]
      xor     eax,  r9d
      xor     r8d,  r10d
      xor     r9d,  dword ptr [W + $10]
      xor     r10d, dword ptr [W + $14]
      xor     eax,  dword ptr [W + $20]
      xor     r8d,  dword ptr [W + $20 + 4]
      xor     r9d,  dword ptr [W + $20 + 8]
      xor     r10d, dword ptr [W + $20 + 12]
      xor     eax,  dword ptr [W + $34]
      xor     r8d,  dword ptr [W + $34 + 4]
      xor     r9d,  dword ptr [W + $34 + 8]
      rol     eax, 1
      rol     r8d, 1
      rol     r9d, 1
      xor     r10d, eax
      mov     dword ptr [W + $40], eax
      mov     dword ptr [W + $40 + 4], r8d
      mov     dword ptr [W + $40 + 8], r9d
      rol     r10d, 1
      sub     n, 4
      mov     dword ptr [W + $40 + 12], r10d
      lea     W, [W + 16]
      jnz     @s
end;

{$ifdef USEAESNI}

procedure AesNiEncryptXmm0; // rax=TAesContext+$70 xmm0=input/output
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
      movups  xmm1,  dqword ptr [rax + $00 - $70]
      movups  xmm2,  dqword ptr [rax + $10 - $70]
      movups  xmm3,  dqword ptr [rax + $20 - $70]
      movups  xmm4,  dqword ptr [rax + $30 - $70]
      movups  xmm5,  dqword ptr [rax + $40 - $70]
      pxor    xmm0,  xmm1
      movups  xmm1,  dqword ptr [rax + $50 - $70]
      aesenc  xmm0,  xmm2
      aesenc  xmm0,  xmm3
      aesenc  xmm0,  xmm4
      aesenc  xmm0,  xmm5
      aesenc  xmm0,  xmm1
      movups  xmm1,  dqword ptr [rax + $60 - $70]
      movups  xmm2,  dqword ptr [rax + $70 - $70]
      movups  xmm3,  dqword ptr [rax + $80 - $70]
      movups  xmm4,  dqword ptr [rax + $90 - $70]
      movups  xmm5,  dqword ptr [rax + $a0 - $70]
      aesenc  xmm0,  xmm1
      aesenc  xmm0,  xmm2
      aesenc  xmm0,  xmm3
      aesenc  xmm0,  xmm4
      cmp     byte ptr [rax - $70].TAesContext.Rounds, 12
      je      @192
      ja      @256
      aesenclast xmm0, xmm5
      ret
@192: movups  xmm1,  dqword ptr [rax + $b0 - $70]
      movups  xmm2,  dqword ptr [rax + $c0 - $70]
      aesenc  xmm0,  xmm5
      aesenc  xmm0,  xmm1
      aesenclast xmm0, xmm2
      ret
@256: movups  xmm1,  dqword ptr [rax + $b0 - $70]
      movups  xmm2,  dqword ptr [rax + $c0 - $70]
      movups  xmm3,  dqword ptr [rax + $d0 - $70]
      movups  xmm4,  dqword ptr [rax + $e0 - $70]
      aesenc  xmm0,  xmm5
      aesenc  xmm0,  xmm1
      aesenc  xmm0,  xmm2
      aesenc  xmm0,  xmm3
      aesenclast xmm0, xmm4
end;

procedure AesNiEncrypt128(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $00 - $70]
        movups  xmm1,  dqword ptr [ctxt + $10 - $70]
        movups  xmm2,  dqword ptr [ctxt + $20 - $70]
        movups  xmm3,  dqword ptr [ctxt + $30 - $70]
        movups  xmm4,  dqword ptr [ctxt + $40 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $50 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $60 - $70]
        movups  xmm2,  dqword ptr [ctxt + $70 - $70]
        movups  xmm3,  dqword ptr [ctxt + $80 - $70]
        movups  xmm4,  dqword ptr [ctxt + $90 - $70]
        movups  xmm5,  dqword ptr [ctxt + $a0 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenclast xmm0, xmm5
        {$else}
        movups  xmm1,  dqword ptr [ctxt + $00 - $70]
        movups  xmm2,  dqword ptr [ctxt + $10 - $70]
        movups  xmm3,  dqword ptr [ctxt + $20 - $70]
        movups  xmm4,  dqword ptr [ctxt + $30 - $70]
        movups  xmm5,  dqword ptr [ctxt + $40 - $70]
        movups  xmm6,  dqword ptr [ctxt + $50 - $70]
        movups  xmm7,  dqword ptr [ctxt + $60 - $70]
        movups  xmm8,  dqword ptr [ctxt + $70 - $70]
        movups  xmm9,  dqword ptr [ctxt + $80 - $70]
        movups  xmm10, dqword ptr [ctxt + $90 - $70]
        movups  xmm11, dqword ptr [ctxt + $a0 - $70]
        pxor    xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm6
        aesenc  xmm0,  xmm7
        aesenc  xmm0,  xmm8
        aesenc  xmm0,  xmm9
        aesenc  xmm0,  xmm10
        aesenclast xmm0, xmm11
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
end;

procedure AesNiEncrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $00 - $70]
        movups  xmm1,  dqword ptr [ctxt + $10 - $70]
        movups  xmm2,  dqword ptr [ctxt + $20 - $70]
        movups  xmm3,  dqword ptr [ctxt + $30 - $70]
        movups  xmm4,  dqword ptr [ctxt + $40 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $50 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $60 - $70]
        movups  xmm2,  dqword ptr [ctxt + $70 - $70]
        movups  xmm3,  dqword ptr [ctxt + $80 - $70]
        movups  xmm4,  dqword ptr [ctxt + $90 - $70]
        movups  xmm5,  dqword ptr [ctxt + $a0 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $c0 - $70]
        aesenc  xmm0,  xmm1
        aesenclast xmm0, xmm2
        {$else}
        movups  xmm0,  dqword ptr [source]
        movups  xmm1,  dqword ptr [ctxt + $00 - $70]
        movups  xmm2,  dqword ptr [ctxt + $10 - $70]
        movups  xmm3,  dqword ptr [ctxt + $20 - $70]
        movups  xmm4,  dqword ptr [ctxt + $30 - $70]
        movups  xmm5,  dqword ptr [ctxt + $40 - $70]
        movups  xmm6,  dqword ptr [ctxt + $50 - $70]
        movups  xmm7,  dqword ptr [ctxt + $60 - $70]
        movups  xmm8,  dqword ptr [ctxt + $70 - $70]
        movups  xmm9,  dqword ptr [ctxt + $80 - $70]
        movups  xmm10, dqword ptr [ctxt + $90 - $70]
        movups  xmm11, dqword ptr [ctxt + $a0 - $70]
        movups  xmm12, dqword ptr [ctxt + $b0 - $70]
        movups  xmm13, dqword ptr [ctxt + $c0 - $70]
        pxor    xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm6
        aesenc  xmm0,  xmm7
        aesenc  xmm0,  xmm8
        aesenc  xmm0,  xmm9
        aesenc  xmm0,  xmm10
        aesenc  xmm0,  xmm11
        aesenc  xmm0,  xmm12
        aesenclast xmm0, xmm13
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
end;

procedure AesNiEncrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $00 - $70]
        movups  xmm1,  dqword ptr [ctxt + $10 - $70]
        movups  xmm2,  dqword ptr [ctxt + $20 - $70]
        movups  xmm3,  dqword ptr [ctxt + $30 - $70]
        movups  xmm4,  dqword ptr [ctxt + $40 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $50 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $60 - $70]
        movups  xmm2,  dqword ptr [ctxt + $70 - $70]
        movups  xmm3,  dqword ptr [ctxt + $80 - $70]
        movups  xmm4,  dqword ptr [ctxt + $90 - $70]
        movups  xmm5,  dqword ptr [ctxt + $a0 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $c0 - $70]
        movups  xmm3,  dqword ptr [ctxt + $d0 - $70]
        movups  xmm4,  dqword ptr [ctxt + $e0 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenclast xmm0, xmm4
        {$else}
        movups  xmm1,  dqword ptr [ctxt + $00 - $70]
        movups  xmm2,  dqword ptr [ctxt + $10 - $70]
        movups  xmm3,  dqword ptr [ctxt + $20 - $70]
        movups  xmm4,  dqword ptr [ctxt + $30 - $70]
        movups  xmm5,  dqword ptr [ctxt + $40 - $70]
        movups  xmm6,  dqword ptr [ctxt + $50 - $70]
        movups  xmm7,  dqword ptr [ctxt + $60 - $70]
        movups  xmm8,  dqword ptr [ctxt + $70 - $70]
        movups  xmm9,  dqword ptr [ctxt + $80 - $70]
        movups  xmm10, dqword ptr [ctxt + $90 - $70]
        movups  xmm11, dqword ptr [ctxt + $a0 - $70]
        movups  xmm12, dqword ptr [ctxt + $b0 - $70]
        movups  xmm13, dqword ptr [ctxt + $c0 - $70]
        movups  xmm14, dqword ptr [ctxt + $d0 - $70]
        movups  xmm15, dqword ptr [ctxt + $e0 - $70]
        pxor    xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm6
        aesenc  xmm0,  xmm7
        aesenc  xmm0,  xmm8
        aesenc  xmm0,  xmm9
        aesenc  xmm0,  xmm10
        aesenc  xmm0,  xmm11
        aesenc  xmm0,  xmm12
        aesenc  xmm0,  xmm13
        aesenc  xmm0,  xmm14
        aesenclast xmm0, xmm15
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
end;

procedure AesNiDecrypt128(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $a0 - $70]
        movups  xmm1,  dqword ptr [ctxt + $90 - $70]
        movups  xmm2,  dqword ptr [ctxt + $80 - $70]
        movups  xmm3,  dqword ptr [ctxt + $70 - $70]
        movups  xmm4,  dqword ptr [ctxt + $60 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $50 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $40 - $70]
        movups  xmm2,  dqword ptr [ctxt + $30 - $70]
        movups  xmm3,  dqword ptr [ctxt + $20 - $70]
        movups  xmm4,  dqword ptr [ctxt + $10 - $70]
        movups  xmm5,  dqword ptr [ctxt + $00 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdeclast xmm0, xmm5
        {$else}
        movups  xmm1,  dqword ptr [ctxt + $a0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $90 - $70]
        movups  xmm3,  dqword ptr [ctxt + $80 - $70]
        movups  xmm4,  dqword ptr [ctxt + $70 - $70]
        movups  xmm5,  dqword ptr [ctxt + $60 - $70]
        movups  xmm6,  dqword ptr [ctxt + $50 - $70]
        movups  xmm7,  dqword ptr [ctxt + $40 - $70]
        movups  xmm8,  dqword ptr [ctxt + $30 - $70]
        movups  xmm9,  dqword ptr [ctxt + $20 - $70]
        movups  xmm10, dqword ptr [ctxt + $10 - $70]
        movups  xmm11, dqword ptr [ctxt + $00 - $70]
        pxor    xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        aesdec  xmm0,  xmm6
        aesdec  xmm0,  xmm7
        aesdec  xmm0,  xmm8
        aesdec  xmm0,  xmm9
        aesdec  xmm0,  xmm10
        aesdeclast xmm0, xmm11
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
end;

procedure AesNiDecrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $c0 - $70]
        movups  xmm1,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $a0 - $70]
        movups  xmm3,  dqword ptr [ctxt + $90 - $70]
        movups  xmm4,  dqword ptr [ctxt + $80 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $70 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $60 - $70]
        movups  xmm2,  dqword ptr [ctxt + $50 - $70]
        movups  xmm3,  dqword ptr [ctxt + $40 - $70]
        movups  xmm4,  dqword ptr [ctxt + $30 - $70]
        movups  xmm5,  dqword ptr [ctxt + $20 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $10 - $70]
        movups  xmm2,  dqword ptr [ctxt + $00 - $70]
        aesdec  xmm0,  xmm1
        aesdeclast xmm0, xmm2
        {$else}
        movups  xmm1,  dqword ptr [ctxt + $c0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm3,  dqword ptr [ctxt + $a0 - $70]
        movups  xmm4,  dqword ptr [ctxt + $90 - $70]
        movups  xmm5,  dqword ptr [ctxt + $80 - $70]
        movups  xmm6,  dqword ptr [ctxt + $70 - $70]
        movups  xmm7,  dqword ptr [ctxt + $60 - $70]
        movups  xmm8,  dqword ptr [ctxt + $50 - $70]
        movups  xmm9,  dqword ptr [ctxt + $40 - $70]
        movups  xmm10, dqword ptr [ctxt + $30 - $70]
        movups  xmm11, dqword ptr [ctxt + $20 - $70]
        movups  xmm12, dqword ptr [ctxt + $10 - $70]
        movups  xmm13, dqword ptr [ctxt + $00 - $70]
        pxor    xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        aesdec  xmm0,  xmm6
        aesdec  xmm0,  xmm7
        aesdec  xmm0,  xmm8
        aesdec  xmm0,  xmm9
        aesdec  xmm0,  xmm10
        aesdec  xmm0,  xmm11
        aesdec  xmm0,  xmm12
        aesdeclast xmm0, xmm13
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
end;

procedure AesNiDecrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     ctxt, [ctxt + $70] // for smaller code gen of [ctx + -$70..+$70]
        movups  xmm0,  dqword ptr [source]
        {$ifdef WIN64ABI} // should preserve XMM6-XMM15 on Win64
        movups  xmm5,  dqword ptr [ctxt + $e0 - $70]
        movups  xmm1,  dqword ptr [ctxt + $d0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $c0 - $70]
        movups  xmm3,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm4,  dqword ptr [ctxt + $a0 - $70]
        pxor    xmm0,  xmm5
        movups  xmm5,  dqword ptr [ctxt + $90 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $80 - $70]
        movups  xmm2,  dqword ptr [ctxt + $70 - $70]
        movups  xmm3,  dqword ptr [ctxt + $60 - $70]
        movups  xmm4,  dqword ptr [ctxt + $50 - $70]
        movups  xmm5,  dqword ptr [ctxt + $40 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        movups  xmm1,  dqword ptr [ctxt + $30 - $70]
        movups  xmm2,  dqword ptr [ctxt + $20 - $70]
        movups  xmm3,  dqword ptr [ctxt + $10 - $70]
        movups  xmm4,  dqword ptr [ctxt + $00 - $70]
        aesdec  xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdeclast xmm0, xmm4
        {$else}
        movups  xmm1,  dqword ptr [ctxt + $e0 - $70]
        movups  xmm2,  dqword ptr [ctxt + $d0 - $70]
        movups  xmm3,  dqword ptr [ctxt + $c0 - $70]
        movups  xmm4,  dqword ptr [ctxt + $b0 - $70]
        movups  xmm5,  dqword ptr [ctxt + $a0 - $70]
        movups  xmm6,  dqword ptr [ctxt + $90 - $70]
        movups  xmm7,  dqword ptr [ctxt + $80 - $70]
        movups  xmm8,  dqword ptr [ctxt + $70 - $70]
        movups  xmm9,  dqword ptr [ctxt + $60 - $70]
        movups  xmm10, dqword ptr [ctxt + $50 - $70]
        movups  xmm11, dqword ptr [ctxt + $40 - $70]
        movups  xmm12, dqword ptr [ctxt + $30 - $70]
        movups  xmm13, dqword ptr [ctxt + $20 - $70]
        movups  xmm14, dqword ptr [ctxt + $10 - $70]
        movups  xmm15, dqword ptr [ctxt + $00 - $70]
        pxor    xmm0,  xmm1
        aesdec  xmm0,  xmm2
        aesdec  xmm0,  xmm3
        aesdec  xmm0,  xmm4
        aesdec  xmm0,  xmm5
        aesdec  xmm0,  xmm6
        aesdec  xmm0,  xmm7
        aesdec  xmm0,  xmm8
        aesdec  xmm0,  xmm9
        aesdec  xmm0,  xmm10
        aesdec  xmm0,  xmm11
        aesdec  xmm0,  xmm12
        aesdec  xmm0,  xmm13
        aesdec  xmm0,  xmm14
        aesdeclast xmm0, xmm15
        {$endif WIN64ABI}
        movups  dqword ptr [dest], xmm0
        pxor    xmm0,  xmm0 // for safety
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
@e:     ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@mask:  dd      $ffffffff
        dd      $03020100
        dd      $07060504
        dd      $0b0a0908
@128:   aeskeygenassist xmm2, xmm1, 1
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
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@exp128:lea     pk, [pk + 16]
        pshufd  xmm2, xmm2, $FF
        movups  xmm3, xmm1
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pxor    xmm1, xmm2
        movups  dqword ptr [pk], xmm1
        ret
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
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
        add     pk, 16
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
        add     pk, 16
end;

procedure MakeDecrKeyAesNi(Rounds: integer; RK: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        lea     RK, [RK + $70]
        movups  xmm0, dqword ptr [RK + $10 - $70]
        movups  xmm1, dqword ptr [RK + $20 - $70]
        movups  xmm2, dqword ptr [RK + $30 - $70]
        movups  xmm3, dqword ptr [RK + $40 - $70]
        movups  xmm4, dqword ptr [RK + $50 - $70]
        movups  xmm5, dqword ptr [RK + $60 - $70]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        movups  dqword ptr [RK + $10 - $70], xmm0
        movups  dqword ptr [RK + $20 - $70], xmm1
        movups  dqword ptr [RK + $30 - $70], xmm2
        movups  dqword ptr [RK + $40 - $70], xmm3
        movups  dqword ptr [RK + $50 - $70], xmm4
        movups  dqword ptr [RK + $60 - $70], xmm5
        movups  xmm0, dqword ptr [RK + $70 - $70]
        movups  xmm1, dqword ptr [RK + $80 - $70]
        movups  xmm2, dqword ptr [RK + $90 - $70]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        movups  dqword ptr [RK + $70 - $70], xmm0
        movups  dqword ptr [RK + $80 - $70], xmm1
        movups  dqword ptr [RK + $90 - $70], xmm2
        cmp     Rounds, 10
        je      @done
        movups  xmm0, dqword ptr [RK + $a0 - $70]
        movups  xmm1, dqword ptr [RK + $b0 - $70]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        movups  dqword ptr [RK + $a0 - $70], xmm0
        movups  dqword ptr [RK + $b0 - $70], xmm1
        cmp     Rounds, 12
        je      @done
        movups  xmm0, dqword ptr [RK + $c0 - $70]
        movups  xmm1, dqword ptr [RK + $d0 - $70]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        movups  dqword ptr [RK + $c0 - $70], xmm0
        movups  dqword ptr [RK + $d0 - $70], xmm1
@done:
end;

procedure AesNiEncryptOfb128(iv, aes, source, dest: pointer; blockcount: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        mov     rax,  blockcount  // on Win64 ABI, blockcount is not a register
        test    eax,  eax
        jz      @z
        movups  xmm7,  dqword ptr [iv]  // xmm7 = IV
        movups  xmm0,  dqword ptr [aes + $00]
        movups  xmm1,  dqword ptr [aes + $10]
        movups  xmm2,  dqword ptr [aes + $20]
        movups  xmm3,  dqword ptr [aes + $30]
        movups  xmm4,  dqword ptr [aes + $40]
        movups  xmm5,  dqword ptr [aes + $50]
        movups  xmm6,  dqword ptr [aes + $60]
        movups  xmm8,  dqword ptr [aes + $70]
        movups  xmm9,  dqword ptr [aes + $80]
        movups  xmm10, dqword ptr [aes + $90]
        movups  xmm11, dqword ptr [aes + $a0]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [source]
        pxor    xmm7,  xmm0
        aesenc  xmm7,  xmm1
        aesenc  xmm7,  xmm2
        aesenc  xmm7,  xmm3
        aesenc  xmm7,  xmm4
        aesenc  xmm7,  xmm5
        aesenc  xmm7,  xmm6
        aesenc  xmm7,  xmm8
        aesenc  xmm7,  xmm9
        aesenc  xmm7,  xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr [dest], xmm15  // fOut := fIn xor IV
        add     source, 16
        add     dest, 16
        sub     eax, 1
        jnz     @s
        movups  dqword ptr [iv], xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,   dqword ptr [bak6]
        movups  xmm7,   dqword ptr [bak7]
        movups  xmm8,   dqword ptr [bak8]
        movups  xmm9,   dqword ptr [bak9]
        movups  xmm10,  dqword ptr [bak10]
        movups  xmm11,  dqword ptr [bak11]
        movups  xmm15,  dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiEncryptOfb256(iv, aes, source, dest: pointer; blockcount: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        mov     rax, blockcount
        test    eax, eax
        jz      @z
        movups  xmm7,  dqword ptr [iv]  // xmm7 = IV
        movups  xmm0,  dqword ptr [aes + $00]
        movups  xmm1,  dqword ptr [aes + $10]
        movups  xmm2,  dqword ptr [aes + $20]
        movups  xmm3,  dqword ptr [aes + $30]
        movups  xmm4,  dqword ptr [aes + $40]
        movups  xmm5,  dqword ptr [aes + $50]
        movups  xmm6,  dqword ptr [aes + $60]
        movups  xmm8,  dqword ptr [aes + $70]
        movups  xmm9,  dqword ptr [aes + $80]
        movups  xmm10, dqword ptr [aes + $90]
        movups  xmm11, dqword ptr [aes + $a0]
        movups  xmm12, dqword ptr [aes + $b0]
        movups  xmm13, dqword ptr [aes + $c0]
        movups  xmm14, dqword ptr [aes + $d0]
        lea     aes,    qword ptr [aes + $e0]  // aes = last key
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
        sub     eax, 1
        jnz     @s
        movups  dqword ptr [iv], xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;


{ AES-CTR alternative code }

{$define USEAESNICTRORIGINAL} // our single procedure seems the fastest (yet)
{$ifdef USEAESNICTRNEW}
  {$undef USEAESNICTRORIGINAL}
{$endif USEAESNICTRNEW}

{ Some x86_64 numbers on a Core i5 15400, running on Debian 12:

1) all our code is faster than OpenSSL- much faster in fact - we observed that
   the OpenSSL asm is clearly circumvoluted and the benefit of using perl or
   macros for the code is not obvious: more complex, and not worth it
2) our naive/simple x4 approach - which did wonders on i386 - is a bit slower
   on 86_64 for no obvious reason - perhaps newer CPUs may benefit from it
3) we also tried an x8 unrolled version to leverage the additional xmm8-xmm15
  registers available on the platform - but it was actually slower than our x4 code
4) "simple" x4 and x8 code has been less tested than original AesNiEncryptCtrNist32
-> we continue to use our original AesNiEncryptCtrNist32, and keep our naive/simple
   x4 and x8 code as disabled reference, and may retry with future CPUs

 AesNiEncryptCtrNist32By1 only
 500 mormot aes-128-ctr 8 in 22us i.e. 21.6M/s, aver. 44ns, 173.4 MB/s
 500 mormot aes-256-ctr 8 in 23us i.e. 20.7M/s, aver. 46ns, 165.8 MB/s
 500 mormot aes-128-ctr 50 in 24us i.e. 19.8M/s, aver. 48ns, 0.9 GB/s
 500 mormot aes-256-ctr 50 in 25us i.e. 19M/s, aver. 50ns, 0.9 GB/s
 500 mormot aes-128-ctr 100 in 25us i.e. 19M/s, aver. 50ns, 1.8 GB/s
 500 mormot aes-256-ctr 100 in 27us i.e. 17.6M/s, aver. 54ns, 1.7 GB/s
 500 mormot aes-128-ctr 1000 in 93us i.e. 5.1M/s, aver. 186ns, 5 GB/s
 500 mormot aes-256-ctr 1000 in 104us i.e. 4.5M/s, aver. 208ns, 4.4 GB/s
 500 mormot aes-128-ctr 10000 in 532us i.e. 0.9M/s, aver. 1.06us, 8.7 GB/s
 500 mormot aes-256-ctr 10000 in 716us i.e. 681.9K/s, aver. 1.43us, 6.5 GB/s

 AesNiEncryptCtrNist32By4 only
 500 mormot aes-128-ctr 8 in 22us i.e. 21.6M/s, aver. 44ns, 173.4 MB/s
 500 mormot aes-256-ctr 8 in 23us i.e. 20.7M/s, aver. 46ns, 165.8 MB/s
 500 mormot aes-128-ctr 50 in 23us i.e. 20.7M/s, aver. 46ns, 1 GB/s
 500 mormot aes-256-ctr 50 in 24us i.e. 19.8M/s, aver. 48ns, 0.9 GB/s
 500 mormot aes-128-ctr 100 in 25us i.e. 19M/s, aver. 50ns, 1.8 GB/s
 500 mormot aes-256-ctr 100 in 26us i.e. 18.3M/s, aver. 52ns, 1.7 GB/s
 500 mormot aes-128-ctr 1000 in 78us i.e. 6.1M/s, aver. 156ns, 5.9 GB/s
 500 mormot aes-256-ctr 1000 in 93us i.e. 5.1M/s, aver. 186ns, 5 GB/s
 500 mormot aes-128-ctr 10000 in 453us i.e. 1M/s, aver. 906ns, 10.2 GB/s
 500 mormot aes-256-ctr 10000 in 604us i.e. 808.4K/s, aver. 1.20us, 7.7 GB/s

 AesNiEncryptCtrNist32By8 only
 500 mormot aes-128-ctr 8 in 22us i.e. 21.6M/s, aver. 44ns, 173.4 MB/s
 500 mormot aes-256-ctr 8 in 23us i.e. 20.7M/s, aver. 46ns, 165.8 MB/s
 500 mormot aes-128-ctr 50 in 24us i.e. 19.8M/s, aver. 48ns, 0.9 GB/s
 500 mormot aes-256-ctr 50 in 25us i.e. 19M/s, aver. 50ns, 0.9 GB/s
 500 mormot aes-128-ctr 100 in 26us i.e. 18.3M/s, aver. 52ns, 1.7 GB/s
 500 mormot aes-256-ctr 100 in 28us i.e. 17M/s, aver. 56ns, 1.6 GB/s
 500 mormot aes-128-ctr 1000 in 84us i.e. 5.6M/s, aver. 168ns, 5.5 GB/s
 500 mormot aes-256-ctr 1000 in 107us i.e. 4.4M/s, aver. 214ns, 4.3 GB/s
 500 mormot aes-128-ctr 10000 in 512us i.e. 0.9M/s, aver. 1.02us, 9 GB/s
 500 mormot aes-256-ctr 10000 in 757us i.e. 645K/s, aver. 1.51us, 6.1 GB/s

 AesNiEncryptCtrNist32By8 if blocks >= 64
 500 mormot aes-128-ctr 8 in 22us i.e. 21.6M/s, aver. 44ns, 173.4 MB/s
 500 mormot aes-256-ctr 8 in 23us i.e. 20.7M/s, aver. 46ns, 165.8 MB/s
 500 mormot aes-128-ctr 50 in 23us i.e. 20.7M/s, aver. 46ns, 1 GB/s
 500 mormot aes-256-ctr 50 in 25us i.e. 19M/s, aver. 50ns, 0.9 GB/s
 500 mormot aes-128-ctr 100 in 25us i.e. 19M/s, aver. 50ns, 1.8 GB/s
 500 mormot aes-256-ctr 100 in 26us i.e. 18.3M/s, aver. 52ns, 1.7 GB/s
 500 mormot aes-128-ctr 1000 in 78us i.e. 6.1M/s, aver. 156ns, 5.9 GB/s
 500 mormot aes-256-ctr 1000 in 93us i.e. 5.1M/s, aver. 186ns, 5 GB/s
 500 mormot aes-128-ctr 10000 in 510us i.e. 0.9M/s, aver. 1.02us, 9.1 GB/s
 500 mormot aes-256-ctr 10000 in 762us i.e. 640.7K/s, aver. 1.52us, 6.1 GB/s

 Original AesNiEncryptCtrNist32()
 500 mormot aes-128-ctr 8 in 22us i.e. 21.6M/s, aver. 44ns, 173.4 MB/s
 500 mormot aes-256-ctr 8 in 23us i.e. 20.7M/s, aver. 46ns, 165.8 MB/s
 500 mormot aes-128-ctr 50 in 24us i.e. 19.8M/s, aver. 48ns, 0.9 GB/s
 500 mormot aes-256-ctr 50 in 24us i.e. 19.8M/s, aver. 48ns, 0.9 GB/s
 500 mormot aes-128-ctr 100 in 27us i.e. 17.6M/s, aver. 54ns, 1.7 GB/s
 500 mormot aes-256-ctr 100 in 28us i.e. 17M/s, aver. 56ns, 1.6 GB/s
 500 mormot aes-128-ctr 1000 in 76us i.e. 6.2M/s, aver. 152ns, 6.1 GB/s
 500 mormot aes-256-ctr 1000 in 88us i.e. 5.4M/s, aver. 176ns, 5.2 GB/s
 500 mormot aes-128-ctr 10000 in 438us i.e. 1M/s, aver. 876ns, 10.6 GB/s
 500 mormot aes-256-ctr 10000 in 557us i.e. 876.6K/s, aver. 1.11us, 8.3 GB/s

 OpenSSL 3.0.16
 500 openssl aes-128-ctr 8 in 143us i.e. 3.3M/s, aver. 286ns, 26.6 MB/s
 500 openssl aes-256-ctr 8 in 140us i.e. 3.4M/s, aver. 280ns, 27.2 MB/s
 500 openssl aes-128-ctr 50 in 147us i.e. 3.2M/s, aver. 294ns, 162.1 MB/s
 500 openssl aes-256-ctr 50 in 147us i.e. 3.2M/s, aver. 294ns, 162.1 MB/s
 500 openssl aes-128-ctr 100 in 146us i.e. 3.2M/s, aver. 292ns, 326.6 MB/s
 500 openssl aes-256-ctr 100 in 147us i.e. 3.2M/s, aver. 294ns, 324.3 MB/s
 500 openssl aes-128-ctr 1000 in 189us i.e. 2.5M/s, aver. 378ns, 2.4 GB/s
 500 openssl aes-256-ctr 1000 in 199us i.e. 2.4M/s, aver. 398ns, 2.3 GB/s
 500 openssl aes-128-ctr 10000 in 526us i.e. 0.9M/s, aver. 1.05us, 8.8 GB/s
 500 openssl aes-256-ctr 10000 in 652us i.e. 748.9K/s, aver. 1.30us, 7.1 GB/s

}

{$ifdef USEAESNICTRORIGINAL}

// AES-CTR with 8x AES-NI interleave factor over a 32-bit counter
procedure AesNiEncryptCtrNist32(
  src, dest: pointer; blocks: PtrUInt; ctxt, iv: pointer);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        push    rsi
        push    rdi
        mov     rdi, src    // rcx
        mov     rsi, dest   // rdx
        mov     rdx, blocks // r8
        mov     rcx, ctxt   // r9
        mov     r8, qword ptr [iv]  // not passed as register
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=blocks, rcx=ctxt, r8=iv
      	cmp    rdx, $1
      	jne    @big
        // handle single block without allocating any stack frame
        mov   al, byte ptr [rcx].TAesContext.Rounds
      	movups xmm2, dqword ptr [r8]
      	movups xmm0, dqword ptr [rcx]
      	movups xmm3, dqword ptr [rcx + $10]
      	movups xmm4, dqword ptr [rcx + $20]
      	movups xmm5, dqword ptr [rcx + $30]
      	movups xmm1, dqword ptr [rcx + $40]
      	lea    rcx, [rcx + $50]
        sub    al, 4
      	xorps  xmm2, xmm0
        aesenc xmm2, xmm3
      	movups xmm3, dqword ptr [rdi]
        aesenc xmm2, xmm4
        aesenc xmm2, xmm5
@sml:   aesenc xmm2, xmm1
      	movups xmm1, dqword ptr [rcx]
      	add    rcx, $10
      	sub    al, 1
      	jne    @sml
      	aesenclast xmm2, xmm1
      	pxor   xmm0, xmm0
      	pxor   xmm1, xmm1
      	xorps  xmm2, xmm3
      	pxor   xmm3, xmm3
      	movups dqword ptr [rsi], xmm2
      	xorps  xmm2, xmm2
        {$ifdef WIN64ABI}
        jmp    @exit
        {$else}
        ret
        {$endif WIN64ABI}
        // optimized AES-CTR process
@big:   {$ifdef WIN64ABI}
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        {$endif WIN64ABI}
        lea    r11, [rsp]
        push   rbp
        sub    rsp, $80
        and    rsp, $fffffffffffffff0
        // precompute the next 8 counters on the stack
        movups xmm2, dqword ptr [r8]
        movups xmm0, dqword ptr [rcx]
        mov    r8d, dword ptr [r8 + $c] // get 32-bit counter
        pxor   xmm2, xmm0
        mov    ebp, dword ptr [rcx + $c]
        movaps dqword ptr [rsp], xmm2
        bswap  r8d
        movaps xmm3, xmm2
        movaps xmm4, xmm2
        movaps xmm5, xmm2
        movaps dqword ptr [rsp + $40], xmm2
        movaps dqword ptr [rsp + $50], xmm2
        movaps dqword ptr [rsp + $60], xmm2
        mov    r10, rdx
        movaps dqword ptr [rsp + $70], xmm2
        lea    rax, [r8 + $1]
        lea    rdx, [r8 + $2]
        bswap  eax
        bswap  edx
        xor    eax, ebp
        xor    edx, ebp
        pinsrd xmm3, eax, $3
        lea    rax, [r8 + $3]
        movaps dqword ptr [rsp + $10], xmm3
        pinsrd xmm4, edx, $3
        bswap  eax
        mov    rdx, r10
        lea    r10, [r8 + $4]
        movaps dqword ptr [rsp + $20], xmm4
        xor    eax, ebp
        bswap  r10d
        pinsrd xmm5, eax, $3
        movzx  eax, byte ptr [rcx].TAesContext.Rounds
        xor    r10d, ebp
        movaps dqword ptr [rsp + $30], xmm5
        lea    r9, [r8 + $5]
        mov    dword ptr [rsp + $4c], r10d
        bswap  r9d
        lea    r10, [r8 + $6]
        sub    al, 1 // code below doesn't include the last aesenclast round
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
        movaps xmm6, dqword ptr [rsp + $40]
        movaps xmm7, dqword ptr [rsp + $50]
        cmp    rdx, $8
        jb     @tail
        sub    rdx, $6
        lea    rcx, [rcx + $80]
        sub    rdx, $2
        // main loop, processing 8 interleaved CTR per iteration
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop8: add    r8d, $8 // our 32-bit CTR
        movaps xmm8, dqword ptr [rsp + $60]
        aesenc xmm2, xmm1
        mov    r9d, r8d
        movaps xmm9, dqword ptr [rsp + $70]
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
        movups xmm10, dqword ptr [rdi]
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
        // encrypt 8 * 128-bit blocks (128 bytes) from src into dest
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@edone: movups xmm11, dqword ptr [rdi + $10]
        pxor   xmm10, xmm0
        movups xmm12, dqword ptr [rdi + $20]
        pxor   xmm11, xmm0
        movups xmm13, dqword ptr [rdi + $30]
        pxor   xmm12, xmm0
        movups xmm14, dqword ptr [rdi + $40]
        pxor   xmm13, xmm0
        movups xmm15, dqword ptr [rdi + $50]
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
        movups xmm1, dqword ptr [rdi + $60]
        lea    rdi, [rdi + $80]
        aesenclast xmm2, xmm10
        pxor   xmm1, xmm0
        movups xmm10, dqword ptr [rdi - $10]
        aesenclast xmm3, xmm11
        pxor   xmm10, xmm0
        movaps xmm11, dqword ptr [rsp]
        aesenclast xmm4, xmm12
        aesenclast xmm5, xmm13
        movaps xmm12, dqword ptr [rsp + $10]
        movaps xmm13, dqword ptr [rsp + $20]
        aesenclast xmm6, xmm14
        aesenclast xmm7, xmm15
        movaps xmm14, dqword ptr [rsp + $30]
        movaps xmm15, dqword ptr [rsp + $40]
        aesenclast xmm8, xmm1
        movaps xmm0, dqword ptr [rsp + $50]
        movups xmm1, dqword ptr [rcx - $70]
        aesenclast xmm9, xmm10
        movups dqword ptr [rsi], xmm2
        movaps xmm2, xmm11
        movups dqword ptr [rsi + $10], xmm3
        movaps xmm3, xmm12
        movups dqword ptr [rsi + $20], xmm4
        movaps xmm4, xmm13
        movups dqword ptr [rsi + $30], xmm5
        movaps xmm5, xmm14
        movups dqword ptr [rsi + $40], xmm6
        movaps xmm6, xmm15
        movups dqword ptr [rsi + $50], xmm7
        movaps xmm7, xmm0
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
        movaps xmm8, dqword ptr [rsp + $60]
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
        movups xmm13, dqword ptr [rdi + $30]
        pxor   xmm2, xmm10
        movups xmm10, dqword ptr [rdi + $40]
        pxor   xmm3, xmm11
        movups dqword ptr [rsi], xmm2
        pxor   xmm4, xmm12
        movups dqword ptr [rsi + $10], xmm3
        pxor   xmm5, xmm13
        movups dqword ptr [rsi + $20], xmm4
        pxor   xmm6, xmm10
        movups dqword ptr [rsi + $30], xmm5
        movups dqword ptr [rsi + $40], xmm6
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
        sub    al, 1
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
        movups dqword ptr [rsi + $20], xmm4
        pxor   xmm5, xmm13
        movups dqword ptr [rsi + $30], xmm5
        jmp    @done
        // 1..3 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop3: lea    rcx, [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        movups xmm1, dqword ptr [rcx]
        sub    al, 1
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
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
@exit:  pop    rdi
        pop    rsi
        {$endif WIN64ABI}
end;

{$else} // our naive/simple code following our i386 x4 pattern - extended to x8

{$define USEAESNICTRBY4}      // x4 seems to give the best results among simple

type
  TAesNiCtr = record // shared stack structure to make Win64 code simpler
    count: PtrUInt;
    src, dst, ctxt, iv: pointer;
    {$ifdef WIN64ABI} // WIN64 ABI should preserve rsi/rdi and XMM6-XMM15
    bakrdi, bakrsi: pointer;
    bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
    {$endif WIN64ABI}
  end;

procedure AesNiEncryptCtrNist32Small;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rax=ctxt+$70 rcx=ctr r8=params r9=iv r10=src r11=dst edx=count
        // inlined AesNiEncryptXmm0 - don't use XMM6-XMM15 so is WIN64 friendly
        movups  xmm0, [r9]
        bswap   ecx
        pinsrd  xmm0, ecx, 3  // SSE 4.1 instruction
        movups  xmm1,  dqword ptr [rax + $00 - $70]
        movups  xmm2,  dqword ptr [rax + $10 - $70]
        movups  xmm3,  dqword ptr [rax + $20 - $70]
        movups  xmm4,  dqword ptr [rax + $30 - $70]
        movups  xmm5,  dqword ptr [rax + $40 - $70]
        pxor    xmm0,  xmm1
        movups  xmm1,  dqword ptr [rax + $50 - $70]
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm1
        movups  xmm1,  dqword ptr [rax + $60 - $70]
        movups  xmm2,  dqword ptr [rax + $70 - $70]
        movups  xmm3,  dqword ptr [rax + $80 - $70]
        movups  xmm4,  dqword ptr [rax + $90 - $70]
        movups  xmm5,  dqword ptr [rax + $a0 - $70]
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenc  xmm0,  xmm4
        cmp     byte ptr [rax - $70].TAesContext.Rounds, 12
        je      @192
        ja      @256
        aesenclast xmm0, xmm5
@write: movups  xmm1, [r10]
        lea     r10, qword ptr [r10 + $10]
        bswap   ecx
        pxor    xmm0, xmm1
        lea     rcx, qword ptr [rcx + 1]
        movups  [r11], xmm0
        lea     r11, qword ptr [r11 + $10]
        dec     edx
        jnz     AesNiEncryptCtrNist32Small
        ret
  @192: movups  xmm1,  dqword ptr [rax + $b0 - $70]
        movups  xmm2,  dqword ptr [rax + $c0 - $70]
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm1
        aesenclast xmm0, xmm2
        jmp     @write
  @256: movups  xmm1,  dqword ptr [rax + $b0 - $70]
        movups  xmm2,  dqword ptr [rax + $c0 - $70]
        movups  xmm3,  dqword ptr [rax + $d0 - $70]
        movups  xmm4,  dqword ptr [rax + $e0 - $70]
        aesenc  xmm0,  xmm5
        aesenc  xmm0,  xmm1
        aesenc  xmm0,  xmm2
        aesenc  xmm0,  xmm3
        aesenclast xmm0, xmm4
        jmp     @write
end;

procedure AesNiEncryptCtrNist32By1(var ctx: TAesNiCtr);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=ctx
        mov     rax, qword ptr [ctx].TAesNiCtr.ctxt
        mov     edx, dword ptr [ctx].TAesNiCtr.count
        mov     r8,  ctx
        mov     r9,  qword ptr [ctx].TAesNiCtr.iv
        mov     r10, qword ptr [ctx].TAesNiCtr.src
        mov     r11, qword ptr [ctx].TAesNiCtr.dst
        lea     rax, [rax + $70] // for smaller code gen of [ctx + -$70..+$70]
        // prepare ecx = 32-bit little endian ctr
        mov     ecx, dword ptr [r9].THash128Rec.c3
        bswap   ecx
        jmp     AesNiEncryptCtrNist32Small
end;

{$ifdef USEAESNICTRBY4}

procedure AesNiEncryptCtrNist32Fast(var ctx: TAesNiCtr);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=ctx
        mov     rax, [ctx].TAesNiCtr.ctxt
        mov     r8,  ctx
        mov     r9,  [ctx].TAesNiCtr.iv
        mov     r10, [ctx].TAesNiCtr.src
        mov     r11, [ctx].TAesNiCtr.dst
        lea     rax, [rax + $70] // for smaller code gen of [ctx + -$70..+$70]
        // prepare ecx = 32-bit little endian ctr
        mov     ecx, dword ptr [r9].THash128Rec.c3
        bswap   ecx
        // handle 0..3 blocks alignment
        mov     edx, dword ptr [r8].TAesNiCtr.count // caller ensured count <> 0
        and     edx, 3
        jz      @start
        sub     dword ptr [r8].TAesNiCtr.count, edx
        jz      AesNiEncryptCtrNist32Small
        call    AesNiEncryptCtrNist32Small
        {$ifdef WIN64ABI}
        // Windows x64 calling convention expects to preserve rsi/rdi XMM6-XMM15
@start: mov     qword  ptr [r8].TAesNiCtr.bakrdi, rdi
        mov     qword  ptr [r8].TAesNiCtr.bakrsi, rsi
        movups  dqword ptr [r8].TAesNiCtr.bak6, xmm6
        movups  dqword ptr [r8].TAesNiCtr.bak7, xmm7
        {$ifdef FPC} align 16 {$endif}
        {$else}
        {$ifdef FPC} align 16 {$endif}
@start: {$endif WIN64ABI}
@by4:   // prepare xmm0-xmm3 as iv0-iv3
        movups  xmm0, [r9]
        movaps  xmm1, xmm0
        movaps  xmm2, xmm0
        movaps  xmm3, xmm0
        lea     rdx, [rcx + 1]
        lea     rsi, [rcx + 2]
        lea     rdi, [rcx + 3]
        bswap   ecx
        bswap   edx
        bswap   esi
        bswap   edi
        pinsrd  xmm0, ecx, 3  // SSE 4.1 instruction
        pinsrd  xmm1, edx, 3
        pinsrd  xmm2, esi, 3
        pinsrd  xmm3, edi, 3
        bswap   ecx
        lea     rcx, [rcx + 4]
        // 4x interleaved AES process
        mov     dl, byte ptr [rax - $70].TAesContext.Rounds
        movups  xmm4, [rax + $00 - $70]
        movups  xmm5, [rax + $10 - $70]
        movups  xmm6, [rax + $20 - $70]
        movups  xmm7, [rax + $30 - $70]
        pxor    xmm0, xmm4
        pxor    xmm1, xmm4
        pxor    xmm2, xmm4
        pxor    xmm3, xmm4
        aesenc  xmm0, xmm5
        aesenc  xmm1, xmm5
        aesenc  xmm2, xmm5
        aesenc  xmm3, xmm5
        aesenc  xmm0, xmm6
        aesenc  xmm1, xmm6
        aesenc  xmm2, xmm6
        aesenc  xmm3, xmm6
        aesenc  xmm0, xmm7
        aesenc  xmm1, xmm7
        aesenc  xmm2, xmm7
        aesenc  xmm3, xmm7
        movups  xmm4, [rax + $40 - $70]
        movups  xmm5, [rax + $50 - $70]
        movups  xmm6, [rax + $60 - $70]
        movups  xmm7, [rax + $70 - $70]
        aesenc  xmm0, xmm4
        aesenc  xmm1, xmm4
        aesenc  xmm2, xmm4
        aesenc  xmm3, xmm4
        aesenc  xmm0, xmm5
        aesenc  xmm1, xmm5
        aesenc  xmm2, xmm5
        aesenc  xmm3, xmm5
        aesenc  xmm0, xmm6
        aesenc  xmm1, xmm6
        aesenc  xmm2, xmm6
        aesenc  xmm3, xmm6
        aesenc  xmm0, xmm7
        aesenc  xmm1, xmm7
        aesenc  xmm2, xmm7
        aesenc  xmm3, xmm7
        movups  xmm4, [rax + $80 - $70]
        movups  xmm5, [rax + $90 - $70]
        movups  xmm6, [rax + $a0 - $70]
        movups  xmm7, [rax + $b0 - $70]
        aesenc  xmm0, xmm4
        aesenc  xmm1, xmm4
        aesenc  xmm2, xmm4
        aesenc  xmm3, xmm4
        aesenc  xmm0, xmm5
        aesenc  xmm1, xmm5
        aesenc  xmm2, xmm5
        aesenc  xmm3, xmm5
        cmp     dl, 10
        ja      @192
@last:  aesenclast xmm0, xmm6
        aesenclast xmm1, xmm6
        aesenclast xmm2, xmm6
        aesenclast xmm3, xmm6
@write: // dst := src xor AES(iv)
        movups  xmm4, [r10 + $00]
        movups  xmm5, [r10 + $10]
        movups  xmm6, [r10 + $20]
        movups  xmm7, [r10 + $30]
        lea     r10, qword ptr [r10 + $40]
        pxor    xmm4, xmm0
        pxor    xmm5, xmm1
        pxor    xmm6, xmm2
        pxor    xmm7, xmm3
        movups  [r11 + $00], xmm4
        movups  [r11 + $10], xmm5
        movups  [r11 + $20], xmm6
        movups  [r11 + $30], xmm7
        lea     r11, qword [r11 + $40]
        sub     dword ptr [r8].TAesNiCtr.count, 4 // 4x blocks per loop
        jnz     @by4
        {$ifdef WIN64ABI}
        mov     rdi,  qword  ptr [r8].TAesNiCtr.bakrdi
        mov     rsi,  qword  ptr [r8].TAesNiCtr.bakrsi
        movups  xmm6, dqword ptr [r8].TAesNiCtr.bak6
        movups  xmm7, dqword ptr [r8].TAesNiCtr.bak7
        {$endif WIN64ABI}
        ret
@192:   movups  xmm4, [rax + $c0 - $70]
        movups  xmm5, [rax + $d0 - $70]
        aesenc  xmm0, xmm6
        aesenc  xmm1, xmm6
        aesenc  xmm2, xmm6
        aesenc  xmm3, xmm6
        aesenc  xmm0, xmm7
        aesenc  xmm1, xmm7
        aesenc  xmm2, xmm7
        aesenc  xmm3, xmm7
        cmp     dl, 12
        ja      @256
        aesenclast xmm0, xmm4
        aesenclast xmm1, xmm4
        aesenclast xmm2, xmm4
        aesenclast xmm3, xmm4
        jmp     @write
@256:   movups  xmm6, [rax + $e0 - $70]
        aesenc  xmm0, xmm4
        aesenc  xmm1, xmm4
        aesenc  xmm2, xmm4
        aesenc  xmm3, xmm4
        aesenc  xmm0, xmm5
        aesenc  xmm1, xmm5
        aesenc  xmm2, xmm5
        aesenc  xmm3, xmm5
        jmp     @last
end;

{$else}

// AES-CTR with 8x AES-NI interleave factor over a 32-bit counter - not faster
procedure AesNiEncryptCtrNist32Fast(var ctx: TAesNiCtr);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=ctx
        mov     rax, [ctx].TAesNiCtr.ctxt
        mov     r8,  ctx
        mov     r9,  [ctx].TAesNiCtr.iv
        mov     r10, [ctx].TAesNiCtr.src
        mov     r11, [ctx].TAesNiCtr.dst
        lea     rax, [rax + $70] // for smaller code gen of [ctx + -$70..+$70]
        // prepare ecx = 32-bit little endian ctr
        mov     ecx, dword ptr [r9].THash128Rec.c3
        bswap   ecx
        // handle 0..7 blocks alignment
        mov     edx, dword ptr [r8].TAesNiCtr.count // caller ensured count <> 0
        and     edx, 7
        jz      @start
        sub     dword ptr [r8].TAesNiCtr.count, edx
        jz      AesNiEncryptCtrNist32Small // no register save on Win64 for 1..7
        call    AesNiEncryptCtrNist32Small
        {$ifdef WIN64ABI}
        // Windows x64 calling convention expects to preserve rsi/rdi XMM6-XMM15
@start: mov     qword  ptr [r8].TAesNiCtr.bakrdi, rdi
        mov     qword  ptr [r8].TAesNiCtr.bakrsi, rsi
        movups  dqword ptr [r8].TAesNiCtr.bak6,  xmm6
        movups  dqword ptr [r8].TAesNiCtr.bak7,  xmm7
        movups  dqword ptr [r8].TAesNiCtr.bak8,  xmm8
        movups  dqword ptr [r8].TAesNiCtr.bak9,  xmm9
        movups  dqword ptr [r8].TAesNiCtr.bak10, xmm10
        movups  dqword ptr [r8].TAesNiCtr.bak11, xmm11
        movups  dqword ptr [r8].TAesNiCtr.bak12, xmm12
        movups  dqword ptr [r8].TAesNiCtr.bak13, xmm13
        movups  dqword ptr [r8].TAesNiCtr.bak14, xmm14
        movups  dqword ptr [r8].TAesNiCtr.bak15, xmm15
        {$ifdef FPC} align 16 {$endif}
        {$else}
        {$ifdef FPC} align 16 {$endif}
@start: {$endif WIN64ABI}
@by8:   // prepare xmm0-xmm7 as iv0-iv7
        movups  xmm0, [r9]
        movaps  xmm1, xmm0
        movaps  xmm2, xmm0
        movaps  xmm3, xmm0
        movups  xmm4, xmm0
        movaps  xmm5, xmm0
        movaps  xmm6, xmm0
        movaps  xmm7, xmm0
        lea     rdx, [rcx + 1]
        lea     rsi, [rcx + 2]
        lea     rdi, [rcx + 3]
        bswap   ecx
        bswap   edx
        bswap   esi
        bswap   edi
        pinsrd  xmm0, ecx, 3  // SSE 4.1 instruction
        pinsrd  xmm1, edx, 3
        pinsrd  xmm2, esi, 3
        pinsrd  xmm3, edi, 3
        bswap   ecx
        lea     rdx, [rcx + 5]
        lea     rsi, [rcx + 6]
        lea     rdi, [rcx + 7]
        lea     rcx, [rcx + 4]
        bswap   ecx
        bswap   edx
        bswap   esi
        bswap   edi
        pinsrd  xmm4, ecx, 3
        pinsrd  xmm5, edx, 3
        pinsrd  xmm6, esi, 3
        pinsrd  xmm7, edi, 3
        bswap   ecx
        lea     rcx, [rcx + 4]
        // 8x interleaved AES process
        mov     dl, byte ptr [rax - $70].TAesContext.Rounds
        movups  xmm8,  [rax + $00 - $70]
        movups  xmm9,  [rax + $10 - $70]
        movups  xmm10, [rax + $20 - $70]
        movups  xmm11, [rax + $30 - $70]
        movups  xmm12, [rax + $40 - $70]
        movups  xmm13, [rax + $50 - $70]
        movups  xmm14, [rax + $60 - $70]
        movups  xmm15, [rax + $70 - $70]
        pxor    xmm0, xmm8
        pxor    xmm1, xmm8
        pxor    xmm2, xmm8
        pxor    xmm3, xmm8
        pxor    xmm4, xmm8
        pxor    xmm5, xmm8
        pxor    xmm6, xmm8
        pxor    xmm7, xmm8
        aesenc  xmm0, xmm9
        aesenc  xmm1, xmm9
        aesenc  xmm2, xmm9
        aesenc  xmm3, xmm9
        aesenc  xmm4, xmm9
        aesenc  xmm5, xmm9
        aesenc  xmm6, xmm9
        aesenc  xmm7, xmm9
        aesenc  xmm0, xmm10
        aesenc  xmm1, xmm10
        aesenc  xmm2, xmm10
        aesenc  xmm3, xmm10
        aesenc  xmm4, xmm10
        aesenc  xmm5, xmm10
        aesenc  xmm6, xmm10
        aesenc  xmm7, xmm10
        aesenc  xmm0, xmm11
        aesenc  xmm1, xmm11
        aesenc  xmm2, xmm11
        aesenc  xmm3, xmm11
        aesenc  xmm4, xmm11
        aesenc  xmm5, xmm11
        aesenc  xmm6, xmm11
        aesenc  xmm7, xmm11
        aesenc  xmm0, xmm12
        aesenc  xmm1, xmm12
        aesenc  xmm2, xmm12
        aesenc  xmm3, xmm12
        aesenc  xmm4, xmm12
        aesenc  xmm5, xmm12
        aesenc  xmm6, xmm12
        aesenc  xmm7, xmm12
        aesenc  xmm0, xmm13
        aesenc  xmm1, xmm13
        aesenc  xmm2, xmm13
        aesenc  xmm3, xmm13
        aesenc  xmm4, xmm13
        aesenc  xmm5, xmm13
        aesenc  xmm6, xmm13
        aesenc  xmm7, xmm13
        aesenc  xmm0, xmm14
        aesenc  xmm1, xmm14
        aesenc  xmm2, xmm14
        aesenc  xmm3, xmm14
        aesenc  xmm4, xmm14
        aesenc  xmm5, xmm14
        aesenc  xmm6, xmm14
        aesenc  xmm7, xmm14
        aesenc  xmm0, xmm15
        aesenc  xmm1, xmm15
        aesenc  xmm2, xmm15
        aesenc  xmm3, xmm15
        aesenc  xmm4, xmm15
        aesenc  xmm5, xmm15
        aesenc  xmm6, xmm15
        aesenc  xmm7, xmm15
        movups  xmm8,  [rax + $80 - $70]
        movups  xmm9,  [rax + $90 - $70]
        movups  xmm10, [rax + $a0 - $70]
        movups  xmm11, [rax + $b0 - $70]
        aesenc  xmm0, xmm8
        aesenc  xmm1, xmm8
        aesenc  xmm2, xmm8
        aesenc  xmm3, xmm8
        aesenc  xmm4, xmm8
        aesenc  xmm5, xmm8
        aesenc  xmm6, xmm8
        aesenc  xmm7, xmm8
        aesenc  xmm0, xmm9
        aesenc  xmm1, xmm9
        aesenc  xmm2, xmm9
        aesenc  xmm3, xmm9
        aesenc  xmm4, xmm9
        aesenc  xmm5, xmm9
        aesenc  xmm6, xmm9
        aesenc  xmm7, xmm9
        cmp     dl, 10
        ja      @192
@last:  aesenclast xmm0, xmm10
        aesenclast xmm1, xmm10
        aesenclast xmm2, xmm10
        aesenclast xmm3, xmm10
        aesenclast xmm4, xmm10
        aesenclast xmm5, xmm10
        aesenclast xmm6, xmm10
        aesenclast xmm7, xmm10
@write: // dst := src xor AES(iv)
        movups  xmm8,  [r10 + $00]
        movups  xmm9,  [r10 + $10]
        movups  xmm10, [r10 + $20]
        movups  xmm11, [r10 + $30]
        movups  xmm12, [r10 + $40]
        movups  xmm13, [r10 + $50]
        movups  xmm14, [r10 + $60]
        movups  xmm15, [r10 + $70]
        lea     r10, qword ptr [r10 + $80]
        pxor    xmm8,  xmm0
        pxor    xmm9,  xmm1
        pxor    xmm10, xmm2
        pxor    xmm11, xmm3
        pxor    xmm12, xmm4
        pxor    xmm13, xmm5
        pxor    xmm14, xmm6
        pxor    xmm15, xmm7
        movups  [r11 + $00], xmm8
        movups  [r11 + $10], xmm9
        movups  [r11 + $20], xmm10
        movups  [r11 + $30], xmm11
        movups  [r11 + $40], xmm12
        movups  [r11 + $50], xmm13
        movups  [r11 + $60], xmm14
        movups  [r11 + $70], xmm15
        lea     r11, qword [r11 + $80]
        sub     dword ptr [r8].TAesNiCtr.count, 8 // 8x blocks per loop
        jnz     @by8
@done:  {$ifdef WIN64ABI}
        mov     rdi,   qword  ptr [r8].TAesNiCtr.bakrdi
        mov     rsi,   qword  ptr [r8].TAesNiCtr.bakrsi
        movups  xmm6,  dqword ptr [r8].TAesNiCtr.bak6
        movups  xmm7,  dqword ptr [r8].TAesNiCtr.bak7
        movups  xmm8,  dqword ptr [r8].TAesNiCtr.bak8
        movups  xmm9,  dqword ptr [r8].TAesNiCtr.bak9
        movups  xmm10, dqword ptr [r8].TAesNiCtr.bak10
        movups  xmm11, dqword ptr [r8].TAesNiCtr.bak11
        movups  xmm12, dqword ptr [r8].TAesNiCtr.bak12
        movups  xmm13, dqword ptr [r8].TAesNiCtr.bak13
        movups  xmm14, dqword ptr [r8].TAesNiCtr.bak14
        movups  xmm15, dqword ptr [r8].TAesNiCtr.bak15
        {$endif WIN64ABI}
        ret
@192:   movups  xmm8, [rax + $c0 - $70]
        movups  xmm9, [rax + $d0 - $70]
        aesenc  xmm0, xmm10
        aesenc  xmm1, xmm10
        aesenc  xmm2, xmm10
        aesenc  xmm3, xmm10
        aesenc  xmm4, xmm10
        aesenc  xmm5, xmm10
        aesenc  xmm6, xmm10
        aesenc  xmm7, xmm10
        aesenc  xmm0, xmm11
        aesenc  xmm1, xmm11
        aesenc  xmm2, xmm11
        aesenc  xmm3, xmm11
        aesenc  xmm4, xmm11
        aesenc  xmm5, xmm11
        aesenc  xmm6, xmm11
        aesenc  xmm7, xmm11
        cmp     dl, 12
        ja      @256
        aesenclast xmm0, xmm8
        aesenclast xmm1, xmm8
        aesenclast xmm2, xmm8
        aesenclast xmm3, xmm8
        aesenclast xmm4, xmm8
        aesenclast xmm5, xmm8
        aesenclast xmm6, xmm8
        aesenclast xmm7, xmm8
        jmp     @write
@256:   movups  xmm10, [rax + $e0 - $70]
        aesenc  xmm0, xmm8
        aesenc  xmm1, xmm8
        aesenc  xmm2, xmm8
        aesenc  xmm3, xmm8
        aesenc  xmm4, xmm8
        aesenc  xmm5, xmm8
        aesenc  xmm6, xmm8
        aesenc  xmm7, xmm8
        aesenc  xmm0, xmm9
        aesenc  xmm1, xmm9
        aesenc  xmm2, xmm9
        aesenc  xmm3, xmm9
        aesenc  xmm4, xmm9
        aesenc  xmm5, xmm9
        aesenc  xmm6, xmm9
        aesenc  xmm7, xmm9
        jmp     @last
end;

{$endif USEAESNICTRBY4}

procedure AesNiEncryptCtrNist32(src, dst: pointer; blocks: PtrUInt;
  ctxt: PAesContext; iv: PHash128Rec); inline;
var
  params: TAesNiCtr;
begin
  if blocks = 0 then
    exit;
  params.src   := src;
  params.dst   := dst;
  params.ctxt  := ctxt;
  params.iv    := iv;
  params.count := blocks;
  AesNiEncryptCtrNist32Fast(params); // x4 gives the best results on average
end;

{$endif USEAESNICTRORIGINAL}

// AES-CTR and 256-bit crc32c with 8x AES-NI interleave over a 32-bit counter
procedure AesNiEncryptCtrCrc32(
  src, dest: pointer; blocks: PtrUInt; ctxt: TAesCtc);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        push    rsi
        push    rdi
        mov     rdi, src    // rcx
        mov     rsi, dest   // rdx
        mov     rdx, blocks // r8
        mov     rcx, ctxt   // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=blocks, rcx=TAesCtc
        push   r12
        push   r13
        push   r14
        push   r15
        push   rbx
        lea    rbx, [rcx].TAesCtc.fMac  // rbx=plain rbx+16=encrypted
      	cmp    rdx, $1
      	jne    @big
        // handle single block without allocating any stack frame
      	movups xmm2, dqword ptr [rcx].TAesCtc.fIV
        lea    rcx, [rcx].TAesCtc.fAes
        mov    r10d, 16
        call   @crc // MAC plain block
      	mov    al, byte ptr [rcx].TAesContext.Rounds
      	movups xmm0, dqword ptr [rcx]
      	movups xmm3, dqword ptr [rcx + $10]
      	movups xmm4, dqword ptr [rcx + $20]
      	movups xmm5, dqword ptr [rcx + $30]
      	movups xmm1, dqword ptr [rcx + $40]
      	lea    rcx, [rcx + $50]
        sub    al, 4
      	xorps  xmm2, xmm0
        aesenc xmm2, xmm3
      	movups xmm3, dqword ptr [rdi]
        aesenc xmm2, xmm4
        aesenc xmm2, xmm5
@sml:   aesenc xmm2, xmm1
      	movups xmm1, dqword ptr [rcx]
      	add    rcx, $10
      	sub    al, 1
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
        call   @crc // MAC encrypted block
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
        // optimized AES-CTR process
@big:   {$ifdef WIN64ABI}
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        {$endif WIN64ABI}
        lea    r11, [rsp]
        push   rbp
        sub    rsp, $80
        and    rsp, $fffffffffffffff0
        // precompute the next 8 counters on the stack
        movups xmm2, dqword ptr [rcx].TAesCtc.fIV
        mov    r8d, dword ptr [rcx + 12].TAesCtc.fIV // get 32-bit counter
        lea    rcx, [rcx].TAesCtc.fAes
        movups xmm0, dqword ptr [rcx]
        pxor   xmm2, xmm0
        mov    ebp, dword ptr [rcx + $c]
        movaps dqword ptr [rsp], xmm2
        bswap  r8d
        movaps xmm3, xmm2
        movaps xmm4, xmm2
        movaps xmm5, xmm2
        movaps dqword ptr [rsp + $40], xmm2
        movaps dqword ptr [rsp + $50], xmm2
        movaps dqword ptr [rsp + $60], xmm2
        mov    r10, rdx
        movaps dqword ptr [rsp + $70], xmm2
        lea    rax, [r8 + $1]
        lea    rdx, [r8 + $2]
        bswap  eax
        bswap  edx
        xor    eax, ebp
        xor    edx, ebp
        pinsrd xmm3, eax, $3
        lea    rax, [r8 + $3]
        movaps dqword ptr [rsp + $10], xmm3
        pinsrd xmm4, edx, $3
        bswap  eax
        mov    rdx, r10
        lea    r10, [r8 + $4]
        movaps dqword ptr [rsp + $20], xmm4
        xor    eax, ebp
        bswap  r10d
        pinsrd xmm5, eax, $3
        movzx  eax, byte ptr [rcx].TAesContext.Rounds
        xor    r10d, ebp
        movaps dqword ptr [rsp + $30], xmm5
        lea    r9, [r8 + $5]
        mov    dword ptr [rsp + $4c], r10d
        bswap  r9d
        lea    r10, [r8 + $6]
        sub    al, 1 // code below doesn't include the last aesenclast round
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
        movaps xmm6, dqword ptr [rsp + $40]
        movaps xmm7, dqword ptr [rsp + $50]
        cmp    rdx, $8
        jb     @tail
        sub    rdx, $6
        lea    rcx, [rcx + $80]
        sub    rdx, $2
        // main loop, processing 8 interleaved CTR per iteration
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop8: add    r8d, $8 // our 32-bit CTR
        movaps xmm8, dqword ptr [rsp + $60]
        aesenc xmm2, xmm1
        mov    r9d, r8d
        movaps xmm9, dqword ptr [rsp + $70]
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
        movups xmm10, dqword ptr [rdi]
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
        // encrypt + MAC 8 * 128-bit blocks (128 bytes) from src into dest
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@edone: // MAC plain (and input prefetch)
        mov    r12d, dword ptr [rbx]
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
        // encrypt
        movups xmm11, dqword ptr [rdi + $10]
        pxor   xmm10, xmm0
        movups xmm12, dqword ptr [rdi + $20]
        pxor   xmm11, xmm0
        movups xmm13, dqword ptr [rdi + $30]
        pxor   xmm12, xmm0
        movups xmm14, dqword ptr [rdi + $40]
        pxor   xmm13, xmm0
        movups xmm15, dqword ptr [rdi + $50]
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
        movups xmm1, dqword ptr [rdi + $60]
        lea    rdi, [rdi + $80]
        aesenclast xmm2, xmm10
        pxor   xmm1, xmm0
        movups xmm10, dqword ptr [rdi - $10]
        aesenclast xmm3, xmm11
        pxor   xmm10, xmm0
        movaps xmm11, dqword ptr [rsp]
        aesenclast xmm4, xmm12
        aesenclast xmm5, xmm13
        movaps xmm12, dqword ptr [rsp + $10]
        movaps xmm13, dqword ptr [rsp + $20]
        aesenclast xmm6, xmm14
        aesenclast xmm7, xmm15
        movaps xmm14, dqword ptr [rsp + $30]
        movaps xmm15, dqword ptr [rsp + $40]
        aesenclast xmm8, xmm1
        movaps xmm0, dqword ptr [rsp + $50]
        movups xmm1, dqword ptr [rcx - $70]
        aesenclast xmm9, xmm10
        movups dqword ptr [rsi], xmm2
        movaps xmm2, xmm11
        movups dqword ptr [rsi + $10], xmm3
        movaps xmm3, xmm12
        movups dqword ptr [rsi + $20], xmm4
        movaps xmm4, xmm13
        movups dqword ptr [rsi + $30], xmm5
        movaps xmm5, xmm14
        movups dqword ptr [rsi + $40], xmm6
        movaps xmm6, xmm15
        movups dqword ptr [rsi + $50], xmm7
        movaps xmm7, xmm0
        movups dqword ptr [rsi + $60], xmm8
        movups dqword ptr [rsi + $70], xmm9
        // MAC encrypted
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
        call   @crc // MAC plain
        cmp    rdx, $4
        jb     @loop3
        je     @loop4
        // trailing 5..7 interleaved AES blocks
        shl    eax, $4
        movaps xmm8, dqword ptr [rsp + $60]
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
        movups xmm13, dqword ptr [rdi + $30]
        pxor   xmm2, xmm10
        movups xmm10, dqword ptr [rdi + $40]
        pxor   xmm3, xmm11
        movups dqword ptr [rsi], xmm2
        pxor   xmm4, xmm12
        movups dqword ptr [rsi + $10], xmm3
        pxor   xmm5, xmm13
        movups dqword ptr [rsi + $20], xmm4
        pxor   xmm6, xmm10
        movups dqword ptr [rsi + $30], xmm5
        movups dqword ptr [rsi + $40], xmm6
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
        sub    al, 1
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
        movups dqword ptr [rsi + $20], xmm4
        pxor   xmm5, xmm13
        movups dqword ptr [rsi + $30], xmm5
        jmp    @donec
        // 1..3 interleaved AES blocks
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop3: lea    rcx, [rcx + $10]
        aesenc xmm2, xmm1
        aesenc xmm3, xmm1
        aesenc xmm4, xmm1
        movups xmm1, dqword ptr [rcx]
        sub    al, 1
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
        call   @crc // MAC encrypted
        // restore stack
@done:  mov    rbp, qword ptr [r11 - $8]
      	lea    rsp, [r11]
        {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
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

procedure CtrNistCarry12(ctr: PAesBlock); forward; // not worth inlining

// AesNiEncryptCtrCrc32() expects the CTR in lowest 32-bit to never overflow
procedure AesNiEncryptCtrCrc(src, dest: PByte; len: cardinal;
  ctxt: TAesCtc); inline;
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
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0,  dqword ptr [aes + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [aes + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [aes + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [aes + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [aes + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [aes + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [aes + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [aes + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [aes + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [aes + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [aes + $a0].TAesCfc.fAes
        movups  xmm7,  dqword ptr [aes].TAesCfc.fIV  // xmm7 = IV
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
        movups  xmm7, xmm15               // fIV := fOut
        add     src, 16
        add     dest, 16
        sub     blocks, 1
        jnz     @s
        movups  dqword ptr [aes].TAesCfc.fIV, xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfb128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0,  dqword ptr [aes + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [aes + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [aes + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [aes + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [aes + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [aes + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [aes + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [aes + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [aes + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [aes + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [aes + $a0].TAesCfc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfc.fIV  // xmm7 = IV
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
        sub     blocks, 1
        jnz     @s
        movups  dqword ptr [aes].TAesCfc.fIV, xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiEncryptCfb256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // use (mostly) 128-bit registers within the loop
        movups  xmm0,  dqword ptr [aes + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [aes + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [aes + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [aes + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [aes + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [aes + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [aes + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [aes + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [aes + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [aes + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [aes + $a0].TAesCfc.fAes
        movups  xmm12, dqword ptr [aes + $b0].TAesCfc.fAes
        movups  xmm13, dqword ptr [aes + $c0].TAesCfc.fAes
        movups  xmm14, dqword ptr [aes + $d0].TAesCfc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [aes + $e0].TAesCfc.fAes
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
        sub     blocks, 1
        jnz     @s
        movups  dqword ptr [aes].TAesCfc.fIV, xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfb256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=src, rdx/rsi=dest, r8/rdx=aes, r9/rcx=blocks
        test    blocks, blocks
        jz      @z
        // only use 128-bit registers within the loop
        movups  xmm0,  dqword ptr [aes + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [aes + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [aes + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [aes + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [aes + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [aes + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [aes + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [aes + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [aes + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [aes + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [aes + $a0].TAesCfc.fAes
        movups  xmm12, dqword ptr [aes + $b0].TAesCfc.fAes
        movups  xmm13, dqword ptr [aes + $c0].TAesCfc.fAes
        movups  xmm7, dqword ptr [aes].TAesCfc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm14, dqword ptr [aes + $d0].TAesCfc.fAes
        movups  xmm15, dqword ptr [aes + $e0].TAesCfc.fAes
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
        sub     blocks, 1
        jnz     @s
        movups  dqword ptr [aes].TAesCfc.fIV, xmm7
@z:     {$ifdef WIN64ABI}
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiEncryptCfbCrc128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak15], xmm15
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and 128-bit registers within the loop
        mov     eax,  dword ptr [rdx + 0].TAesCfc.fMac.plain
        mov     ebx,  dword ptr [rdx + 4].TAesCfc.fMac.plain
        mov     r8d,  dword ptr [rdx + 8].TAesCfc.fMac.plain
        mov     r9d,  dword ptr [rdx + 12].TAesCfc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfc.fMac.encrypted
        movups  xmm0,  dqword ptr [rdx + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [rdx + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [rdx + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [rdx + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [rdx + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [rdx + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [rdx + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [rdx + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [rdx + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [rdx + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [rdx + $a0].TAesCfc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfc.fIV  // xmm7 = IV
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
        sub     ecx, 1
        jnz     @s
        movups  dqword ptr [rdx].TAesCfc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfbCrc128(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and 128-bit registers within the loop
        mov     eax,  dword ptr [rdx + 0].TAesCfc.fMac.plain
        mov     ebx,  dword ptr [rdx + 4].TAesCfc.fMac.plain
        mov     r8d,  dword ptr [rdx + 8].TAesCfc.fMac.plain
        mov     r9d,  dword ptr [rdx + 12].TAesCfc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfc.fMac.encrypted
        movups  xmm0,  dqword ptr [rdx + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [rdx + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [rdx + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [rdx + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [rdx + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [rdx + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [rdx + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [rdx + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [rdx + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [rdx + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [rdx + $a0].TAesCfc.fAes
        movups  xmm7,  dqword ptr [rdx].TAesCfc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [rdi]
        movups  xmm14, xmm15
        crc32   r10d, dword ptr [rdi + 0] // fMac.encrypted
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
        crc32   eax, dword ptr [rsi + 0] // fMac.plain
        crc32   ebx, dword ptr [rsi + 4]
        crc32   r8d, dword ptr [rsi + 8]
        crc32   r9d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        sub     ecx, 1
        jnz     @s
        movups  dqword ptr [rdx].TAesCfc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiEncryptCfbCrc256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6],  xmm6
        movups  dqword ptr [bak7],  xmm7
        movups  dqword ptr [bak8],  xmm8
        movups  dqword ptr [bak9],  xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and (mostly) 128-bit registers within the loop
        mov     eax,  dword ptr [rdx + 0].TAesCfc.fMac.plain
        mov     ebx,  dword ptr [rdx + 4].TAesCfc.fMac.plain
        mov     r8d,  dword ptr [rdx + 8].TAesCfc.fMac.plain
        mov     r9d,  dword ptr [rdx + 12].TAesCfc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfc.fMac.encrypted
        movups  xmm0,  dqword ptr [rdx + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [rdx + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [rdx + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [rdx + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [rdx + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [rdx + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [rdx + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [rdx + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [rdx + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [rdx + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [rdx + $a0].TAesCfc.fAes
        movups  xmm12, dqword ptr [rdx + $b0].TAesCfc.fAes
        movups  xmm13, dqword ptr [rdx + $c0].TAesCfc.fAes
        movups  xmm14, dqword ptr [rdx + $d0].TAesCfc.fAes
        movups  xmm7,  dqword ptr [rdx].TAesCfc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr [rdx + $e0].TAesCfc.fAes
        crc32   eax, dword ptr [rdi + 0]  // fMac.plain
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
        movups  dqword ptr [rsi], xmm15    // fOut := fIn xor IV
        movups  xmm7, xmm15                // fIV := fOut
        crc32   r10d, dword ptr [rsi + 0]  // fMac.encrypted
        crc32   r11d, dword ptr [rsi + 4]
        crc32   r12d, dword ptr [rsi + 8]
        crc32   r13d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        sub     ecx, 1
        jnz     @s
        movups  dqword ptr [rdx].TAesCfc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure AesNiDecryptCfbCrc256(src, dest, aes: pointer; blocks: PtrUInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        push    rsi
        push    rdi
        mov     rdi, src     // rcx
        mov     rsi, dest    // rdx
        mov     rdx, aes     // r8
        mov     rcx, blocks  // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=src, rsi=dest, rdx=aes, rcx=blocks
        test    ecx, ecx
        jz      @z
        push    r12
        push    r13
        push    rbx
        // only use 32-bit and (mostly) 128-bit registers within the loop
        mov     eax,  dword ptr [rdx + 0].TAesCfc.fMac.plain
        mov     ebx,  dword ptr [rdx + 4].TAesCfc.fMac.plain
        mov     r8d,  dword ptr [rdx + 8].TAesCfc.fMac.plain
        mov     r9d,  dword ptr [rdx + 12].TAesCfc.fMac.plain
        mov     r10d, dword ptr [rdx + 0].TAesCfc.fMac.encrypted
        mov     r11d, dword ptr [rdx + 4].TAesCfc.fMac.encrypted
        mov     r12d, dword ptr [rdx + 8].TAesCfc.fMac.encrypted
        mov     r13d, dword ptr [rdx + 12].TAesCfc.fMac.encrypted
        movups  xmm0,  dqword ptr [rdx + $00].TAesCfc.fAes
        movups  xmm1,  dqword ptr [rdx + $10].TAesCfc.fAes
        movups  xmm2,  dqword ptr [rdx + $20].TAesCfc.fAes
        movups  xmm3,  dqword ptr [rdx + $30].TAesCfc.fAes
        movups  xmm4,  dqword ptr [rdx + $40].TAesCfc.fAes
        movups  xmm5,  dqword ptr [rdx + $50].TAesCfc.fAes
        movups  xmm6,  dqword ptr [rdx + $60].TAesCfc.fAes
        movups  xmm8,  dqword ptr [rdx + $70].TAesCfc.fAes
        movups  xmm9,  dqword ptr [rdx + $80].TAesCfc.fAes
        movups  xmm10, dqword ptr [rdx + $90].TAesCfc.fAes
        movups  xmm11, dqword ptr [rdx + $a0].TAesCfc.fAes
        movups  xmm12, dqword ptr [rdx + $b0].TAesCfc.fAes
        movups  xmm13, dqword ptr [rdx + $c0].TAesCfc.fAes
        movups  xmm7, dqword ptr [rdx].TAesCfc.fIV  // xmm7 = IV
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm14, dqword ptr [rdx + $d0].TAesCfc.fAes
        movups  xmm15, dqword ptr [rdx + $e0].TAesCfc.fAes
        crc32   r10d, dword ptr [rdi + 0]   // fMac.encrypted
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
        movups  dqword ptr [rsi], xmm15   // fOut := fIn xor IV
        movups  xmm7, xmm14               // fIV := fIn
        crc32   eax, dword ptr [rsi + 0]  // fMac.plain
        crc32   ebx, dword ptr [rsi + 4]
        crc32   r8d, dword ptr [rsi + 8]
        crc32   r9d, dword ptr [rsi + 12]
        add     rdi, 16
        add     rsi, 16
        sub     ecx, 1
        jnz     @s
        movups  dqword ptr [rdx].TAesCfc.fIV, xmm7
        mov     dword ptr [rdx + 0].TAesCfc.fMac.plain, eax
        mov     dword ptr [rdx + 4].TAesCfc.fMac.plain, ebx
        mov     dword ptr [rdx + 8].TAesCfc.fMac.plain, r8d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.plain, r9d
        mov     dword ptr [rdx + 0].TAesCfc.fMac.encrypted, r10d
        mov     dword ptr [rdx + 4].TAesCfc.fMac.encrypted, r11d
        mov     dword ptr [rdx + 8].TAesCfc.fMac.encrypted, r12d
        mov     dword ptr [rdx + 12].TAesCfc.fMac.encrypted, r13d
        pop     rbx
        pop     r13
        pop     r12
@z:     {$ifdef WIN64ABI}
        pop    rdi
        pop    rsi
        movups  xmm6,  dqword ptr [bak6]
        movups  xmm7,  dqword ptr [bak7]
        movups  xmm8,  dqword ptr [bak8]
        movups  xmm9,  dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

{$ifdef USEGCMAVX}

// prepare the GMAC process for GcmAvxAuth() and GcmAvxGetTag()
procedure GcmAvxInit(ptab, ks: pointer; rounds: cardinal);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=ptab, rdx/rsi=ks, r8/rdx=kslen
        movaps  xmm15, dqword ptr [rip + @bswapMask]
        movaps  xmm14, dqword ptr [rip + @gcmPoly]
        movups  xmm0, dqword ptr [ks]
        movups  xmm11, dqword ptr [ks + 10H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 20H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 30H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 40H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 50H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 60H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 70H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 80H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 90H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 0A0H]
        cmp     rounds, 12
        jc      @last
        // end of AES-128
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 0B0H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 0C0H]
        jz      @last
        // end of AES-192
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 0D0H]
        aesenc  xmm0, xmm11
        movups  xmm11, dqword ptr [ks + 0E0H]
        // end of AES-256
@last:  aesenclast xmm0, xmm11
        pshufb  xmm0, xmm15
        pshufd  xmm11, xmm0, 0FFH
        movups  xmm12, xmm0
        psrad   xmm11, 31
        pand    xmm11, xmm14
        psrld   xmm12, 31
        pslldq  xmm12, 4
        pslld   xmm0, 1
        pxor    xmm0, xmm11
        pxor    xmm0, xmm12
        movups  dqword ptr [ptab + 0E0H], xmm0
        pshufd  xmm1, xmm0, 4EH
        pxor    xmm1, xmm0
        movups  dqword ptr [ptab + 0F0H], xmm1
        movups  xmm2, xmm0
        movups  xmm3, xmm1
        mov     al, 7
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@init:  movups  xmm11, xmm2
        movups  xmm12, xmm2
        movups  xmm13, xmm3
        // pclmulqdq xmm11, xmm0, 00H
        // pclmulqdq xmm12, xmm0, 11H
        // pclmulqdq xmm13, xmm1, 00H
        db $66, $44, $0F, $3A, $44, $D8, $00
        db $66, $44, $0F, $3A, $44, $E0, $11
        db $66, $44, $0F, $3A, $44, $E9, $00
        pxor    xmm13, xmm11
        pxor    xmm13, xmm12
        movups  xmm4, xmm13
        pslldq  xmm4, 8
        psrldq  xmm13, 8
        pxor    xmm11, xmm4
        pxor    xmm12, xmm13
        movups  xmm2, xmm14
        // pclmulqdq xmm2, xmm11, 01H
        db $66, $41, $0F, $3A, $44, $D3, $01
        pshufd  xmm11, xmm11, 4EH
        pxor    xmm11, xmm2
        movups  xmm2, xmm14
        // pclmulqdq xmm2, xmm11, 01H
        db $66, $41, $0F, $3A, $44, $D3, $01
        pshufd  xmm11, xmm11, 4EH
        pxor    xmm2, xmm11
        pxor    xmm2, xmm12
        movups  dqword ptr [ptab + 0C0H], xmm2
        pshufd  xmm3, xmm2, 4EH
        pxor    xmm3, xmm2
        movups  dqword ptr [ptab + 0D0H], xmm3
        lea     ptab, [ptab - 20H]
        sub     al, 1
        jne     @init
        {$ifdef WIN64ABI}
        jmp     @exit
        {$else}
        ret
        {$endif WIN64ABI}
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
        {$ifdef WIN64ABI}
@exit:  movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

// compute GMAC with 8x interleaved pclmulqdq opcode
procedure GcmAvxAuth(ptab, data: pointer; datalen: PtrInt; hash: pointer);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rdi=ptab, rsi=data, rdx=datalen, rcx=hash
        movups  xmm8, dqword ptr [hash]
        movaps  xmm15, dqword ptr [rip + @bswapMask]
        movaps  xmm14, dqword ptr [rip + @gcmPoly]
        test    datalen, datalen
        jz      @done
        cmp     datalen, 128
        jc      @by1
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by8:   sub     datalen, 128
        movups  xmm0, dqword ptr [data]
        movups  xmm1, dqword ptr [data + 10H]
        movups  xmm2, dqword ptr [data + 20H]
        movups  xmm3, dqword ptr [data + 30H]
        movups  xmm4, dqword ptr [data + 40H]
        movups  xmm5, dqword ptr [data + 50H]
        movups  xmm6, dqword ptr [data + 60H]
        movups  xmm7, dqword ptr [data + 70H]
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
        movups  xmm8, dqword ptr [ptab]
        movups  xmm10, dqword ptr [ptab + 10H]
        movups  xmm9, xmm8
        pshufd  xmm12, xmm0, 4EH
        pxor    xmm12, xmm0
        // pclmulqdq xmm8, xmm0, 00H
        // pclmulqdq xmm9, xmm0, 11H
        // pclmulqdq xmm10, xmm12, 00H
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        db $66, $45, $0F, $3A, $44, $D4, $00
        movups  xmm12, dqword ptr [ptab + 20H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm1, 00H
        db $66, $44, $0F, $3A, $44, $E1, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm1, 11H
        db $66, $44, $0F, $3A, $44, $E9, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm1, 4EH
        pxor    xmm1, xmm12
        movups  xmm12, dqword ptr [ptab + 30H]
        // pclmulqdq xmm12, xmm1, 00H
        db $66, $44, $0F, $3A, $44, $E1, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 40H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm2, 00H
        db $66, $44, $0F, $3A, $44, $E2, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm2, 11H
        db $66, $44, $0F, $3A, $44, $EA, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm2, 4EH
        pxor    xmm2, xmm12
        movups  xmm12, dqword ptr [ptab + 50H]
        // pclmulqdq xmm12, xmm2, 00H
        db $66, $44, $0F, $3A, $44, $E2, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 60H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm3, 00H
        db $66, $44, $0F, $3A, $44, $E3, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm3, 11H
        db $66, $44, $0F, $3A, $44, $EB, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm3, 4EH
        pxor    xmm3, xmm12
        movups  xmm12, dqword ptr [ptab + 70H]
        // pclmulqdq xmm12, xmm3, 00H
        db $66, $44, $0F, $3A, $44, $E3, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 80H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm4, 00H
        db $66, $44, $0F, $3A, $44, $E4, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm4, 11H
        db $66, $44, $0F, $3A, $44, $EC, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm4, 4EH
        pxor    xmm4, xmm12
        movups  xmm12, dqword ptr [ptab + 90H]
        // pclmulqdq xmm12, xmm4, 00H
        db $66, $44, $0F, $3A, $44, $E4, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 0A0H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm5, 00H
        db $66, $44, $0F, $3A, $44, $E5, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm5, 11H
        db $66, $44, $0F, $3A, $44, $ED, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm5, 4EH
        pxor    xmm5, xmm12
        movups  xmm12, dqword ptr [ptab + 0B0H]
        // pclmulqdq xmm12, xmm5, 00H
        db $66, $44, $0F, $3A, $44, $E5, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 0C0H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm6, 00H
        db $66, $44, $0F, $3A, $44, $E6, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm6, 11H
        db $66, $44, $0F, $3A, $44, $EE, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm6, 4EH
        pxor    xmm6, xmm12
        movups  xmm12, dqword ptr [ptab + 0D0H]
        // pclmulqdq xmm12, xmm6, 00H
        db $66, $44, $0F, $3A, $44, $E6, $00
        pxor    xmm10, xmm12
        movups  xmm12, dqword ptr [ptab + 0E0H]
        movups  xmm13, xmm12
        // pclmulqdq xmm12, xmm7, 00H
        db $66, $44, $0F, $3A, $44, $E7, $00
        pxor    xmm8, xmm12
        // pclmulqdq xmm13, xmm7, 11H
        db $66, $44, $0F, $3A, $44, $EF, $11
        pxor    xmm9, xmm13
        pshufd  xmm12, xmm7, 4EH
        pxor    xmm7, xmm12
        movups  xmm12, dqword ptr [ptab + 0F0H]
        // pclmulqdq xmm12, xmm7, 00H
        db $66, $44, $0F, $3A, $44, $E7, $00
        pxor    xmm10, xmm12
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movups  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movups  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movups  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        pxor    xmm8, xmm9
        cmp     datalen, 128
        jnc     @by8
@by1:   movups  xmm12, dqword ptr [ptab + 0E0H]
        movups  xmm13, dqword ptr [ptab + 0F0H]
        cmp     datalen, 16
        jc      @sml
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@next:  sub     datalen, 16
        movups  xmm0, dqword ptr [data]
@s:     pshufb  xmm0, xmm15
        pxor    xmm0, xmm8
        movups  xmm8, xmm12
        movups  xmm10, xmm13
        movups  xmm9, xmm12
        pshufd  xmm11, xmm0, 4EH
        pxor    xmm11, xmm0
        // pclmulqdq xmm8, xmm0, 00H
        // pclmulqdq xmm9, xmm0, 11H
        // pclmulqdq xmm10, xmm11, 00H
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        db $66, $45, $0F, $3A, $44, $D3, $00
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movups  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movups  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movups  xmm11, xmm14
        // pclmulqdq xmm11, xmm8, 01H
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
        sub     data, 1
        sub     datalen, 1
        jnz     @ins
        jmp     @s
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
@done:  movups  dqword ptr [hash], xmm8
        {$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure GcmAvxGetTag(ptab, mask, hash: pointer; plen, dlen: PtrInt);
{$ifdef WIN64ABI}
var
  bak8, bak9, bak10, bak11: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        mov     rax, qword ptr [dlen]  // dlen not passed as register
        push    rsi
        push    rdi
        mov     rdi, ptab    // rcx
        mov     rsi, mask    // rdx
        mov     rdx, hash    // r8
        mov     rcx, plen    // r9
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     rax, r8
{$endif WIN64ABI}
        // rdi=ptab, rsi=mask, rdx=hash, rcx=plen, r8=dlen
        shl     rcx, 3
        movups  xmm8, dqword ptr [rdx]
        movups  xmm1, dqword ptr [rsi]
        movaps  xmm2, dqword ptr [rip + @bswapMask]
        movaps  xmm3, dqword ptr [rip + @gcmPoly]
        shl     rax, 3
        movq    xmm0, rcx
        pinsrq  xmm0, rax, 1
        pxor    xmm0, xmm8
        movups  xmm8, dqword ptr [rdi + 0E0H]
        movups  xmm10, dqword ptr [rdi + 0F0H]
        movups  xmm9, xmm8
        // pclmulqdq xmm8, xmm0, 00H
        // pclmulqdq xmm9, xmm0, 11H
        db $66, $44, $0F, $3A, $44, $C0, $00
        db $66, $44, $0F, $3A, $44, $C8, $11
        pshufd  xmm11, xmm0, 4EH
        pxor    xmm11, xmm0
        // pclmulqdq xmm10, xmm11, 00H
        db $66, $45, $0F, $3A, $44, $D3, $00
        pxor    xmm10, xmm8
        pxor    xmm10, xmm9
        movups  xmm11, xmm10
        psrldq  xmm10, 8
        pslldq  xmm11, 8
        pxor    xmm9, xmm10
        pxor    xmm8, xmm11
        movups  xmm11, xmm3
        // pclmulqdq xmm11, xmm8, 01H
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        movups  xmm11, xmm3
        // pclmulqdq xmm11, xmm8, 01H
        db $66, $45, $0F, $3A, $44, $D8, $01
        pshufd  xmm8, xmm8, 4EH
        pxor    xmm8, xmm11
        pxor    xmm8, xmm9
        pshufb  xmm8, xmm2
        pxor    xmm8, xmm1
        movups  dqword ptr [rdx], xmm8
        {$ifdef WIN64ABI}
        jmp     @exit
        {$else}
        ret
        {$endif WIN64ABI}
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bswapMask:
        dq $08090A0B0C0D0E0F
        dq $0001020304050607
@gcmPoly:
        dq $0000000000000001
        dq $C200000000000000
        {$ifdef WIN64ABI}
@exit:  pop    rdi
        pop    rsi
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        {$endif WIN64ABI}
end;

{$endif USEGCMAVX}

// compute a := a * b in GF(2^128) using pclmulqdq on WestMere CPUs
// - three times faster than the pascal version using lookup tables
procedure gf_mul_pclmulqdq(a, b: pointer);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak10: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak10], xmm10
{$else}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        movups  xmm0, dqword ptr [a]
        movups  xmm1, dqword ptr [b]
        movaps  xmm10, dqword ptr [rip + @swap]
        pshufb  xmm0, xmm10
        pshufb  xmm1, xmm10
        movaps  xmm5, xmm0
        movaps  xmm4, xmm0
        movaps  xmm2, xmm0
        // pclmulqdq xmm0, xmm1, 16
        db $66, $0f, $3a, $44, $c1, $10
        // pclmulqdq xmm5, xmm1, 17
        db $66, $0f, $3a, $44, $e9, $11
        movaps  xmm3, xmm5
        // pclmulqdq xmm4, xmm1, 0
        db $66, $0f, $3a, $44, $e1, $00
        // pclmulqdq xmm2, xmm1, 1
        db $66, $0f, $3a, $44, $d1, $01
        pslldq  xmm3, 8
        pxor    xmm0, xmm2
        movaps  xmm2, xmm4
        pxor    xmm3, xmm0
        pslldq  xmm2, 8
        punpckhqdq xmm3, xmm5
        movaps  xmm1, xmm3
        pslldq  xmm0, 8
        pxor    xmm0, xmm4
        pslldq  xmm1, 8
        punpckhqdq xmm2, xmm0
        movaps  xmm4, xmm2
        movaps  xmm7, xmm1
        pslldq  xmm4, 8
        movaps  xmm1, xmm2
        psrlq   xmm4, 63
        psrldq  xmm1, 8
        psllq   xmm3, 1
        movaps  xmm6, xmm1
        psllq   xmm2, 1
        movaps  xmm1, xmm3
        por     xmm2, xmm4
        movaps  xmm3, xmm2
        psrlq   xmm7, 63
        pslldq  xmm3, 8
        por     xmm1, xmm7
        movaps  xmm4, xmm3
        psrlq   xmm6, 63
        movaps  xmm0, xmm3
        psllq   xmm4, 63
        por     xmm1, xmm6
        psllq   xmm3, 57
        psllq   xmm0, 62
        pxor    xmm2, xmm3
        pxor    xmm0, xmm4
        pxor    xmm2, xmm0
        movaps  xmm3, xmm2
        movaps  xmm7, xmm2
        psrldq  xmm3, 8
        movaps  xmm0, xmm2
        movaps  xmm6, xmm2
        movaps  xmm4, xmm3
        psrlq   xmm7, 1
        movaps  xmm5, xmm3
        psllq   xmm4, 63
        psllq   xmm3, 57
        por     xmm4, xmm7
        psrlq   xmm0, 7
        por     xmm0, xmm3
        psllq   xmm5, 62
        pxor    xmm1, xmm0
        movaps  xmm0, xmm4
        psrlq   xmm6, 2
        por     xmm5, xmm6
        pxor    xmm0, xmm5
        pxor    xmm0, xmm1
        pxor    xmm0, xmm2
        pshufb  xmm0, xmm10
        movups  dqword ptr [a], xmm0
        {$ifdef WIN64ABI}
        jmp     @exit
        {$else}
        ret
        {$endif WIN64ABI}
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@swap:  db 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
        {$ifdef WIN64ABI}
@exit:  movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm10, dqword ptr [bak10]
        {$endif WIN64ABI}
end;

{$endif USEAESNI}

{$ifdef USEAESNIHASH}

procedure _AesNiHashXmm0(seed: PtrUInt; data: pointer; len: PtrUInt); forward;

function _AesNiHash64(seed: QWord; data: pointer; len: PtrUInt): QWord;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=seed, rdx/rsi=data, r8/rdx=len
        movq    xmm0, seed
        call    _AesNiHashXmm0 // preserve rcx/rdi=seed flag
        movq    rax, xmm0 // made 3 AES permutations -> 64-bit truncate is fine
end;

procedure _AesNiHash128(hash: PHash128; data: pointer; len: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // rcx/rdi=seed&result, rdx/rsi=data, r8/rdx=len
        movups  xmm0, dqword ptr [hash]
        call    _AesNiHashXmm0
        movups  dqword ptr [hash], xmm0
end;

function _AesNiHash32(seed: cardinal; data: pointer; len: PtrUInt): cardinal;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // ecx/edi=seed, rdx/rsi=data, r8/rdx=len
        movd    xmm0, seed     // explicitly zeroing high bits for consistency
        jmp     _AesNiHashXmm0 // returns in both xmm0 and eax
        // _AesNiHashXmm0 makes 3 AES permutations -> 32-bit truncate is fine
end;

// 128-bit aeshash as implemented in GoLang runtime, using aesni + sse3 opcodes
// - simplified and rewritten to use a 128-bit seed and no sse4.2, and share the
// same algorithm with mormot.crypt.asmx86.inc - our initial translation of
// go-master/src/runtime/asm_amd64.s returned high 64-bit = 0 for len > 128
// with no actual performance benefit, and more complex code on the Win64 ABI
procedure _AesNiHashXmm0(seed: PtrUInt; data: pointer; len: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // xmm0=seed, rdx/rsi=data, r8/rdx=len, returns in xmm0 and eax
        test    len, len
        jz      @0
        mov     rax,  qword ptr [rip + AesNiHashKey] // aligned pxor xmm,[rax]
        movaps  xmm1, xmm0                           // save seed for @17up
        pxor    xmm0, dqword ptr [rax]               // xor with seed
        aesenc  xmm0, xmm0                           // scramble
        // 1..16 bytes have no branch
        lea     r11, [rip + @shifts] // (heap has header so no read before EOP)
        cmp     len, 16
        ja      @17up
        movups  xmm1, dqword ptr [data + len - 16] // no read after end of page
        je      @16
        shl     len, 4
        pshufb  xmm1, dqword ptr [r11 + len]  // shuffle data bits to lower xmm1
@16:    aesenc  xmm1, xmm0                    // scramble 3 times
        aesenc  xmm1, xmm1
        aesenc  xmm1, xmm1
        movaps  xmm0, xmm1
@0:     movd    eax,  xmm0  // truncate to 32-bit for _AesNiHash32
        ret
@17up:  cmp     len, 32
        ja      @33up
        // 17..32 bytes
        pxor    xmm1, dqword ptr [rax + 16]
        aesenc  xmm1, xmm1
        movups  xmm2, dqword ptr [data]
        movups  xmm3, dqword ptr [data + len - 16] // may overlap
        aesenc  xmm2, xmm0  // scramble 3 times
        aesenc  xmm3, xmm1
        aesenc  xmm2, xmm2
        aesenc  xmm3, xmm3
        aesenc  xmm2, xmm2
        aesenc  xmm3, xmm3
        pxor    xmm2, xmm3
        movaps  xmm0, xmm2
        movd    eax,  xmm2  // truncate to 32-bit for _AesNiHash32
        ret
@33up:  cmp     len, 64
        ja      @65up
        // 33..64 bytes
        movaps  xmm2, xmm1
        movaps  xmm3, xmm1
        pxor    xmm1, dqword ptr [rax + 16]
        pxor    xmm2, dqword ptr [rax + 32]
        pxor    xmm3, dqword ptr [rax + 48]
        aesenc  xmm1, xmm1
        aesenc  xmm2, xmm2
        aesenc  xmm3, xmm3
        movups  xmm4, dqword ptr [data]
        movups  xmm5, dqword ptr [data + 16]
        aesenc  xmm4, xmm0  // scramble 3 times
        aesenc  xmm5, xmm1
        movups  xmm0, dqword ptr [data + len - 32] // may overlap
        movups  xmm1, dqword ptr [data + len - 16]
        aesenc  xmm0, xmm2
        aesenc  xmm1, xmm3
        aesenc  xmm4, xmm4
        aesenc  xmm5, xmm5
        aesenc  xmm0, xmm0
        aesenc  xmm1, xmm1
        aesenc  xmm4, xmm4
        aesenc  xmm5, xmm5
        aesenc  xmm0, xmm0
        aesenc  xmm1, xmm1
        pxor    xmm0, xmm4  // combine result
        pxor    xmm5, xmm1
        pxor    xmm0, xmm5
        movd    eax,  xmm0  // truncate to 32-bit for _AesNiHash32
        ret
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
        // pshufb arguments to move data down from the high bytes of the register
        // to the low bytes of the register  -  index is how many bytes to move
@shifts:dq $0000000000000000, $0000000000000000
        dq $ffffffffffffff0f, $ffffffffffffffff
        dq $ffffffffffff0f0e, $ffffffffffffffff
        dq $ffffffffff0f0e0d, $ffffffffffffffff
        dq $ffffffff0f0e0d0c, $ffffffffffffffff
        dq $ffffff0f0e0d0c0b, $ffffffffffffffff
        dq $ffff0f0e0d0c0b0a, $ffffffffffffffff
        dq $ff0f0e0d0c0b0a09, $ffffffffffffffff
        dq $0f0e0d0c0b0a0908, $ffffffffffffffff
        dq $0e0d0c0b0a090807, $ffffffffffffff0f
        dq $0d0c0b0a09080706, $ffffffffffff0f0e
        dq $0c0b0a0908070605, $ffffffffff0f0e0d
        dq $0b0a090807060504, $ffffffff0f0e0d0c
        dq $0a09080706050403, $ffffff0f0e0d0c0b
        dq $0908070605040302, $ffff0f0e0d0c0b0a
        dq $0807060504030201, $ff0f0e0d0c0b0a09
@65up:  // 65 bytes and up
        {$ifdef WIN64ABI}
        // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [rsp - 16], xmm6
        movups  dqword ptr [rsp - 32], xmm7
        {$endif WIN64ABI}
        movaps  xmm2, xmm1
        movaps  xmm3, xmm1
        pxor    xmm1, dqword ptr [rax + 16]
        pxor    xmm2, dqword ptr [rax + 32]
        pxor    xmm3, dqword ptr [rax + 48]
        aesenc  xmm1, xmm1
        aesenc  xmm2, xmm2
        aesenc  xmm3, xmm3
        movups  xmm4, dqword ptr [data + len - 64] // may overlap
        movups  xmm5, dqword ptr [data + len - 48]
        movups  xmm6, dqword ptr [data + len - 32]
        movups  xmm7, dqword ptr [data + len - 16]
        aesenc  xmm4, xmm0
        aesenc  xmm5, xmm1
        aesenc  xmm6, xmm2
        aesenc  xmm7, xmm3
        dec     len
        shr     len, 6
        // process 64 bytes per iteration
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop:  movups  xmm0, dqword ptr [data]
        movups  xmm1, dqword ptr [data + 16]
        movups  xmm2, dqword ptr [data + 32]
        movups  xmm3, dqword ptr [data + 48]
        aesenc  xmm4, xmm0 // scramble 2 times
        aesenc  xmm5, xmm1
        aesenc  xmm6, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm4, xmm4
        aesenc  xmm5, xmm5
        aesenc  xmm6, xmm6
        aesenc  xmm7, xmm7
        add     data, 64
        dec     len
        jne     @loop
        aesenc  xmm4, xmm0  // scramble 3 times
        aesenc  xmm5, xmm1
        aesenc  xmm6, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm4, xmm4
        aesenc  xmm5, xmm5
        aesenc  xmm6, xmm6
        aesenc  xmm7, xmm7
        aesenc  xmm4, xmm4
        aesenc  xmm5, xmm5
        aesenc  xmm6, xmm6
        aesenc  xmm7, xmm7
        pxor    xmm4, xmm6  // combine result
        pxor    xmm5, xmm7
        {$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [rsp - 16]
        movups  xmm7, dqword ptr [rsp - 32]
        {$endif WIN64ABI}
        pxor    xmm4, xmm5
        movaps  xmm0, xmm4
        movd    eax,  xmm4   // truncate to 32-bit for _AesNiHash32
end;

{$endif USEAESNIHASH}


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

// defined in mormot.crypt.core.pas, not in mormot.core.base, to avoid
// .o/.obj dependencies for most basic executables (for which mormot.core.base
// crc32c x86_64 asm is already fast enough)
function crc32_iscsi_01(buf: PAnsiChar; len: PtrUInt; crc: cardinal): cardinal;
  {$ifdef FPC}cdecl;{$endif} external;

function crc32c_sse42_aesni(crc: PtrUInt; buf: PAnsiChar; len: PtrUInt): cardinal;
{$ifdef FPC} {$ifndef OSWINDOWS} nostackframe; {$endif} assembler; {$endif}
asm
        mov     rax, crc
        mov     rcx, len
        not     eax
        test    buf, buf
        jz      @0
        cmp     len, 64
        ja      @big
        // it is faster to use a direct 8-bytes loop for blocks <= 64 bytes
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
        sub     len, 1
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
        {$ifdef OSWINDOWS}
        jmp     @done
        {$else}
        ret
        {$endif OSWINDOWS}
@big:   // our  call: rcx/rdi=crc rdx/rsi=buf r8/rdx=len
        // iscsi_01:  rcx/rdi=buf rdx/rsi=len r8/rdx=crc
        mov     crc, buf
        mov     buf, len
        mov     len, rax
        call    crc32_iscsi_01
        not     eax
@done:  // on Win64, a stack frame is required when calling sub functions
end;

{$endif CRC32C_X64}


