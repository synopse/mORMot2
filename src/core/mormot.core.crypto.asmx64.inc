{
  This file is a part of the freeware Synopse mORMot framework 2,
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

procedure aesencryptasm(const ctxt: TAESContext; bi, bo: PWA4);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // input: rcx/rdi=TAESContext, rdx/rsi=source, r8/rdx=dest
        .noframe
{$endif} // rolled optimized encryption asm version by A. Bouchez
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        push    rbp
        {$ifdef win64}
        push    rdi
        push    rsi
        mov     r15, r8
        mov     r12, rcx
        {$else}
        mov     r15, rdx
        mov     rdx, rsi
        mov     r12, rdi
        {$endif win64}
        movzx   r13, byte ptr [r12].TAESContext.Rounds
        mov     eax, dword ptr [rdx]
        mov     ebx, dword ptr [rdx+4H]
        mov     ecx, dword ptr [rdx+8H]
        mov     edx, dword ptr [rdx+0CH]
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12+4H]
        xor     ecx, dword ptr [r12+8H]
        xor     edx, dword ptr [r12+0CH]
        sub     r13, 1
        add     r12, 16
        lea     r14, [rip+Te0]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@round: mov     esi, eax
        mov     edi, edx
        movzx   r8d, al
        movzx   r9d, cl
        movzx   r10d, bl
        mov     r8d, dword ptr [r14+r8*4]
        mov     r9d, dword ptr [r14+r9*4]
        mov     r10d, dword ptr [r14+r10*4]
        shr     esi, 16
        shr     edi, 16
        movzx   ebp, bh
        xor     r8d, dword ptr [r14+rbp*4+400H]
        movzx   ebp, dh
        xor     r9d, dword ptr [r14+rbp*4+400H]
        movzx   ebp, ch
        xor     r10d, dword ptr [r14+rbp*4+400H]
        shr     ebx, 16
        shr     ecx, 16
        movzx   ebp, dl
        mov     edx, dword ptr [r14+rbp*4]
        movzx   ebp, cl
        xor     r8d, dword ptr [r14+rbp*4+800H]
        movzx   ebp, sil
        xor     r9d, dword ptr [r14+rbp*4+800H]
        movzx   r11, dil
        movzx   eax, ah
        shr     edi, 8
        movzx   ebp, bh
        shr     esi, 8
        xor     r10d, dword ptr [r14+r11*4+800H]
        xor     edx, dword ptr [r14+rax*4+400H]
        xor     r8d, dword ptr [r14+rdi*4+0C00H]
        xor     r9d, dword ptr [r14+rbp*4+0C00H]
        xor     r10d, dword ptr [r14+rsi*4+0C00H]
        movzx   ebp, bl
        xor     edx, dword ptr [r14+rbp*4+800H]
        mov     rbx, r10
        mov     rax, r8
        movzx   ebp, ch
        xor     edx, dword ptr [r14+rbp*4+0C00H]
        mov     rcx, r9
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12+4H]
        xor     ecx, dword ptr [r12+8H]
        xor     edx, dword ptr [r12+0CH]
        add     r12, 16
        sub     r13, 1
        jnz     @round
        lea     r9, [rip+SBox]
        movzx   r8, al
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, bh
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rcx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12]
        mov     dword ptr [r15], r14d
        movzx   r8, bl
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, ch
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+4H]
        mov     dword ptr [r15+4H], r14d
        movzx   r8, cl
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, dh
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rbx
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+8H]
        mov     dword ptr [r15+8H], r14d
        and     rdx, 0FFH
        movzx   r14, byte ptr [r9+rdx]
        movzx   eax, ah
        movzx   r8, byte ptr [r9+rax]
        shl     r8d, 8
        xor     r14d, r8d
        shr     rbx, 16
        and     rbx, 0FFH
        movzx   r8, byte ptr [r9+rbx]
        shl     r8d, 16
        xor     r14d, r8d
        shr     rcx, 24
        movzx   r8, byte ptr [r9+rcx]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+0CH]
        mov     dword ptr [r15+0CH], r14d
        {$ifdef win64}
        pop     rsi
        pop     rdi
        {$endif win64}
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
  STACK_SIZE = 32{$ifndef LINUX}+7*16{$endif};

procedure sha256_sse4(var input_data; var digest; num_blks: PtrUInt);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // rcx=input_data rdx=digest r8=num_blks (Linux: rdi,rsi,rdx)
        .noframe
{$endif FPC}
        push    rbx
        {$ifdef LINUX}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
        {$else}
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif}
        push    rbp
        push    r13
        push    r14
        push    r15
        sub     rsp, STACK_SIZE
        {$ifndef LINUX}
        movaps  [rsp + 20H], xmm6    // manual .PUSHNV for FPC compatibility
        movaps  [rsp + 30H], xmm7
        movaps  [rsp + 40H], xmm8
        movaps  [rsp + 50H], xmm9
        movaps  [rsp + 60H], xmm10
        movaps  [rsp + 70H], xmm11
        movaps  [rsp + 80H], xmm12
        {$endif}
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
@done: {$ifndef LINUX}
        movaps  xmm6, [rsp + 20H]
        movaps  xmm7, [rsp + 30H]
        movaps  xmm8, [rsp + 40H]
        movaps  xmm9, [rsp + 50H]
        movaps  xmm10, [rsp + 60H]
        movaps  xmm11, [rsp + 70H]
        movaps  xmm12, [rsp + 80H]
        {$endif}
        add     rsp, STACK_SIZE
        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        {$ifndef LINUX}
        pop     rdi
        pop     rsi
        {$endif}
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
        {$ifndef win64}  // input: rcx=B, rdx=A, r8=C (Linux: rdi,rsi,rdx)
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif win64}
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
        {$ifndef win64}
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif win64}
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

procedure bswap256(s,d: PIntegerArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr[s]
        mov     r8d, dword ptr[s + 4]
        mov     r9d, dword ptr[s + 8]
        mov     r10d, dword ptr[s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d], eax
        mov     dword ptr[d + 4], r8d
        mov     dword ptr[d + 8], r9d
        mov     dword ptr[d + 12], r10d
        mov     eax, dword ptr[s + 16]
        mov     r8d, dword ptr[s + 20]
        mov     r9d, dword ptr[s + 24]
        mov     r10d, dword ptr[s + 28]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d + 16], eax
        mov     dword ptr[d + 20], r8d
        mov     dword ptr[d + 24], r9d
        mov     dword ptr[d + 28], r10d
end;

procedure bswap160(s,d: PIntegerArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr[s]
        mov     r8d, dword ptr[s + 4]
        mov     r9d, dword ptr[s + 8]
        mov     r10d, dword ptr[s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d], eax
        mov     dword ptr[d + 4], r8d
        mov     dword ptr[d + 8], r9d
        mov     dword ptr[d + 12], r10d
        mov     eax, dword ptr[s + 16]
        bswap   eax
        mov     dword ptr[d + 16], eax
end;

// see http://nicst.de/crc.pdf

function gf2_multiply(x,y,m,bits: PtrUInt): PtrUInt;
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
        {$ifdef win64}
        test    cl, 1
        {$else}
        test    dil, 1
        {$endif}
        cmovne  rax, r10
        dec     bits
        jne     @s
end;

procedure MD5Transform(var buf: TMD5Buf; const in_: TMD5In);
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
        {$ifndef win64} // W=rcx Buf=rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif win64}
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
        mov     ebx, dword ptr [rsi+4H]
        mov     ecx, dword ptr [rsi+8H]
        mov     edx, dword ptr [rsi+0CH]
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
        mov     r8, qword ptr [r14+8H]
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
        mov     r9, qword ptr [r14+10H]
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
        mov     r10, qword ptr [r14+18H]
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
        mov     r11, qword ptr [r14+20H]
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
        mov     r12, qword ptr [r14+28H]
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
        mov     r13, qword ptr [r14+30H]
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
        mov     r14, qword ptr [r14+38H]
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
        add     dword ptr [rsi+4H], ebx
        add     dword ptr [rsi+8H], ecx
        add     dword ptr [rsi+0CH], edx
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
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesniencrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
        movups  xmm12, dqword ptr[ctxt + 16 * 11]
        movups  xmm13, dqword ptr[ctxt + 16 * 12]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesniencrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
        movups  xmm12, dqword ptr[ctxt + 16 * 11]
        movups  xmm13, dqword ptr[ctxt + 16 * 12]
        movups  xmm14, dqword ptr[ctxt + 16 * 13]
        movups  xmm15, dqword ptr[ctxt + 16 * 14]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt128(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 10]
        movups  xmm1, dqword ptr[ctxt + 16 * 9]
        movups  xmm2, dqword ptr[ctxt + 16 * 8]
        movups  xmm3, dqword ptr[ctxt + 16 * 7]
        movups  xmm4, dqword ptr[ctxt + 16 * 6]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 4]
        movups  xmm8, dqword ptr[ctxt + 16 * 3]
        movups  xmm9, dqword ptr[ctxt + 16 * 2]
        movups  xmm10, dqword ptr[ctxt + 16 * 1]
        movups  xmm11, dqword ptr[ctxt + 16 * 0]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt192(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 12]
        movups  xmm1, dqword ptr[ctxt + 16 * 11]
        movups  xmm2, dqword ptr[ctxt + 16 * 10]
        movups  xmm3, dqword ptr[ctxt + 16 * 9]
        movups  xmm4, dqword ptr[ctxt + 16 * 8]
        movups  xmm5, dqword ptr[ctxt + 16 * 7]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 5]
        movups  xmm9, dqword ptr[ctxt + 16 * 4]
        movups  xmm10, dqword ptr[ctxt + 16 * 3]
        movups  xmm11, dqword ptr[ctxt + 16 * 2]
        movups  xmm12, dqword ptr[ctxt + 16 * 1]
        movups  xmm13, dqword ptr[ctxt + 16 * 0]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt256(const ctxt, source, dest);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 14]
        movups  xmm1, dqword ptr[ctxt + 16 * 13]
        movups  xmm2, dqword ptr[ctxt + 16 * 12]
        movups  xmm3, dqword ptr[ctxt + 16 * 11]
        movups  xmm4, dqword ptr[ctxt + 16 * 10]
        movups  xmm5, dqword ptr[ctxt + 16 * 9]
        movups  xmm6, dqword ptr[ctxt + 16 * 8]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 6]
        movups  xmm10, dqword ptr[ctxt + 16 * 5]
        movups  xmm11, dqword ptr[ctxt + 16 * 4]
        movups  xmm12, dqword ptr[ctxt + 16 * 3]
        movups  xmm13, dqword ptr[ctxt + 16 * 2]
        movups  xmm14, dqword ptr[ctxt + 16 * 1]
        movups  xmm15, dqword ptr[ctxt + 16 * 0]
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
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure ShiftAesNi(KeySize: cardinal; pk: pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, keysize
        movups  xmm1, dqword ptr[pk]
        movaps  xmm5, dqword ptr[rip + @mask]
        cmp     al, 128
        je      @128
        cmp     al, 192
        je      @e // 192 bits is very complicated -> skip by now (128+256)
@256:   movups  xmm3, dqword ptr[pk + 16]
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
        movups  dqword ptr[pk], xmm1
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
        movups  dqword ptr[pk], xmm1
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
        movups  dqword ptr[pk], xmm3
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
        movups  dqword ptr[pk], xmm1
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
        movups  xmm0, dqword ptr[RK + $10]
        movups  xmm1, dqword ptr[RK + $20]
        movups  xmm2, dqword ptr[RK + $30]
        movups  xmm3, dqword ptr[RK + $40]
        movups  xmm4, dqword ptr[RK + $50]
        movups  xmm5, dqword ptr[RK + $60]
        movups  xmm6, dqword ptr[RK + $70]
        movups  xmm7, dqword ptr[RK + $80]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        aesimc  xmm6, xmm6
        aesimc  xmm7, xmm7
        movups  dqword ptr[RK + $10], xmm0
        movups  dqword ptr[RK + $20], xmm1
        movups  dqword ptr[RK + $30], xmm2
        movups  dqword ptr[RK + $40], xmm3
        movups  dqword ptr[RK + $50], xmm4
        movups  dqword ptr[RK + $60], xmm5
        movups  dqword ptr[RK + $70], xmm6
        movups  dqword ptr[RK + $80], xmm7
        lea     RK, [RK + $90]
@loop:  movups  xmm0, dqword ptr[RK]
        aesimc  xmm0, xmm0
        movups  dqword ptr[RK], xmm0
        dec     eax
        lea     RK, [RK + 16]
        jnz     @loop
end;

procedure AesNiEncryptOFB_128(self: TAESOFB; source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        test    blockcount, blockcount
        jz      @z
        movups  xmm7, dqword ptr[self].TAESOFB.fIV  // xmm7 = fCV
        lea     rax,  qword ptr [self].TAESOFB.AES
        movups  xmm0, dqword ptr[rax + 16 * 0]
        movups  xmm1, dqword ptr[rax + 16 * 1]
        movups  xmm2, dqword ptr[rax + 16 * 2]
        movups  xmm3, dqword ptr[rax + 16 * 3]
        movups  xmm4, dqword ptr[rax + 16 * 4]
        movups  xmm5, dqword ptr[rax + 16 * 5]
        movups  xmm6, dqword ptr[rax + 16 * 6]
        movups  xmm8, dqword ptr[rax + 16 * 7]
        movups  xmm9, dqword ptr[rax + 16 * 8]
        movups  xmm10, dqword ptr[rax + 16 * 9]
        movups  xmm11, dqword ptr[rax + 16 * 10]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr[source]
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
        movups  dqword ptr[dest], xmm15  // fOut := fIn xor fCV
        add     source, 16
        add     dest, 16
        dec     blockcount
        jnz     @s
@z:
end;

procedure AesNiEncryptOFB_256(self: TAESOFB; source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        test    blockcount, blockcount
        jz      @z
        movups  xmm7, dqword ptr[self].TAESOFB.fIV  // xmm7 = fCV
        lea     rax,  qword ptr [self].TAESOFB.AES  // rax = TAES
        movups  xmm0, dqword ptr[rax + 16 * 0]
        movups  xmm1, dqword ptr[rax + 16 * 1]
        movups  xmm2, dqword ptr[rax + 16 * 2]
        movups  xmm3, dqword ptr[rax + 16 * 3]
        movups  xmm4, dqword ptr[rax + 16 * 4]
        movups  xmm5, dqword ptr[rax + 16 * 5]
        movups  xmm6, dqword ptr[rax + 16 * 6]
        movups  xmm8, dqword ptr[rax + 16 * 7]
        movups  xmm9, dqword ptr[rax + 16 * 8]
        movups  xmm10, dqword ptr[rax + 16 * 9]
        movups  xmm11, dqword ptr[rax + 16 * 10]
        movups  xmm12, dqword ptr[rax + 16 * 11]
        movups  xmm13, dqword ptr[rax + 16 * 12]
        movups  xmm14, dqword ptr[rax + 16 * 13]
        add     rax,  16 * 14  // rax = last key
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr[rax]
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
        movups  xmm15, dqword ptr[source]
        pxor    xmm15, xmm7
        movups  dqword ptr[dest], xmm15  // fOut := fIn xor fCV
        add     source, 16
        add     dest, 16
        dec     blockcount
        jnz     @s
@z:
end;

{$endif USEAESNI}


{$ifdef SHA512_X64}

// optimized asm using SSE4 instructions for x64 64-bit

{$ifdef MSWINDOWS}
  {$L ..\..\static\delphi\sha512-x64sse4.obj}
{$else}
  {$L sha512-x64sse4.o}
{$endif MSWINDOWS}

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

{$ifdef MSWINDOWS}
  {$L ..\..\static\delphi\crc32c64.obj}
{$else}
  {$L crc32c64.o}
{$endif MSWINDOWS}

// defined in mormot.core.crypto.pas, not in mormot.core.base, to avoid
// .o/.obj dependencies for most basic executables (for which mormot.core.base
// crc32c x86_64 asm is alredy fast enough
function crc32_iscsi_01(buf: PAnsiChar; len: PtrUInt; crc: cardinal): cardinal;
  {$ifdef FPC}cdecl;{$endif} external;

function crc32c_sse42_aesni(crc: PtrUInt; buf: PAnsiChar; len: PtrUInt): cardinal;
{$ifdef FPC}nostackframe; assembler; asm{$else}asm .noframe {$endif}
        mov     rax, crc
        mov     rcx, len
        not     eax
        test    buf, buf
        jz      @z
        cmp     len, 64
        jb      @sml
        // our  call: rcx/rdi=crc rdx/rsi=buf r8/rdx=len
        // iscsi_01:  rcx/rdi=buf rdx/rsi=len r8/rdx=crc
        mov     crc, buf
        mov     buf, len
        mov     len, rax
        call    crc32_iscsi_01
@z:     not     eax
        ret
@sml:   shr     len, 3
        jz      @2
{$ifdef FPC} align 16
@s:     crc32   rax, qword [buf] // hash 8 bytes per loop
{$else} @s:     db $F2,$48,$0F,$38,$F1,$02 // circumvent Delphi inline asm compiler bug
{$endif}add     buf, 8
        dec     len
        jnz     @s
@2:     test    cl, 4
        jz      @3
        crc32   eax, dword ptr[buf]
        add     buf, 4
@3:     test    cl, 2
        jz      @1
        crc32   eax, word ptr[buf]
        add     buf, 2
@1:     test    cl, 1
        jz      @0
        crc32   eax, byte ptr[buf]
@0:     not     eax
end;

{$endif CRC32C_X64}

