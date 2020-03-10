{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86_64 assembly used by mormot.core.base.pas
}

{$ifdef ASMX64}

const
  // non-temporal writes should bypass the cache when the size is bigger than
  // half the size of the largest level cache - we assume low 1MB cache here
  CPUCACHEX64 = 512*1024;

{
  regarding benchmark numbers from TTestLowLevelCommon.CustomRTL
  -> FillCharFast/MoveFast are faster, especially for small lengths (strings)
  -> Delphi RTL is slower than FPC's, and it doesn't support AVX assembly yet
  -> cpuERMS - of little benefit - is disabled, unless WITH_ERMS is defined
  http://blog.synopse.info/post/2020/02/17/New-move/fillchar-optimized-sse2/avx-asm-version
}

// these stand-alone functions will use CPUIDX64 global to adjust the algorithm
procedure MoveFast(const src; var dst; cnt: PtrInt);
{$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif} // rcx/rdi=src rdx/rsi=dst r8/rdx=cnt
        {$ifdef WIN64}
        mov     rax, r8
        {$else}
        mov     rax, rdx // rax=r8=cnt
        mov     r8, rdx
        {$endif}
        lea     r10, [rip+@jmptab]
        cmp     src, dst
        je      @equal
        cmp     cnt, 32
        ja      @lrg  // >32
        sub     rax, 8
        jg      @sml  // 9..32
        jmp     qword ptr[r10 + 64 + rax * 8]  // 0..8
@equal: ret
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@jmptab:dq      @exit, @01, @02, @03, @04, @05, @06, @07, @08
@sml:   mov     r8, qword ptr[src + rax] // last 8
        mov     r9, qword ptr[src]       // first 8
        cmp     al, 8
        jle     @sml16
        mov     r10, qword ptr[src + 8]  // second 8
        cmp     al, 16
        jle     @sml24
        mov     r11, qword ptr[src + 16] // third 8
        mov     qword ptr[dst + 16], r11 // third 8
@sml24: mov     qword ptr[dst + 8], r10  // second 8
@sml16: mov     qword ptr[dst], r9       // first 8
        mov     qword ptr[dst + rax], r8 // last 8 (may be overlapping)
        ret
@02:    movzx   eax, word ptr[src] // use small size moves as code alignment
        mov     word ptr[dst], ax
        ret
@04:    mov     eax, [src]
        mov     dword ptr[dst], eax
        ret
@08:    mov     rax, [src]
        mov     [dst], rax
@exit:  ret
@lrg:   jng     @exit   // cnt < 0
        cmp     src, dst
        ja      @lrgfwd
        sub     dst, rax
        cmp     src, dst
        lea     dst, [dst + rax]
        ja     @lrgbwd
@lrgfwd:{$ifdef WITH_ERMS}
        test    byte ptr[rip+CPUIDX64], 1 shl cpuERMS
        jz      @nofwe
        cmp     rax, 2048
        jb      @nofwe
        cld
@repmov:{$ifdef WIN64}
        push    rsi
        push    rdi
        mov     rsi, src
        mov     rdi, dst
        mov     rcx, r8
        rep movsb
        pop     rdi
        pop     rsi
        {$else}
        mov     rax, dst // dst=rsi and src=rdi -> rax to swap
        mov     rsi, src
        mov     rdi, rax
        mov     rcx, r8
        rep movsb
        {$endif}
        ret
@nofwe: {$endif WITH_ERMS}
        mov     r9, dst
        {$ifdef FPC} // no AVX asm on Delphi :(
        cmp     rax, 256  // vzeroupper penaly for cnt>255
        jb      @fsse2
        test    byte ptr[rip+CPUIDX64], 1 shl cpuAVX
        jnz     @fwdavx
        {$endif FPC}
@fsse2: movups  xmm2, oword ptr[src]  // first 16
        lea     src, [src + rax - 16]
        lea     rax, [rax + dst - 16]
        movups  xmm1, oword ptr[src]  // last 16
        mov     r10, rax
        neg     rax
        and     dst,  -16  // 16-byte aligned writes
        lea     rax, [rax + dst + 16]
        cmp     r8, CPUCACHEX64
        ja      @fwdnv  // bypass cache for cnt>512KB
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@fwd:   movups  xmm0, oword ptr[src + rax]  // regular loop
        movaps  [r10 + rax], xmm0
        add     rax, 16
        jl      @fwd
@fwdend:movups  [r10], xmm1 // last 16
        movups  [r9], xmm2  // first 16
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@fwdnv: movups  xmm0, oword ptr[src + rax]  // non-temporal loop
        movntdq [r10 + rax], xmm0
        add     rax, 16
        jl      @fwdnv
        sfence
        jmp     @fwdend
{$ifdef FPC}
@fwdavx:vmovups ymm2, oword ptr[src]  // first 32
        lea     src, [src + rax - 32]
        lea     rax, [rax + dst - 32]
        vmovups ymm1, oword ptr[src]  // last 32
        mov     r10, rax
        neg     rax
        and     dst,  -32  // 32-byte aligned writes
        lea     rax, [rax + dst + 32]
        cmp     r8, CPUCACHEX64
        ja      @favxn  // bypass cache for cnt>512KB
        align 16
@favxr: vmovups ymm0, oword ptr[src + rax]  // regular loop
        vmovaps [r10 + rax], ymm0
        add     rax, 32
        jl      @favxr
@favxe: vmovups [r10], ymm1 // last 32
        vmovups [r9], ymm2  // first 32
// https://software.intel.com/en-us/articles/avoiding-avx-sse-transition-penalties
        vzeroupper
        ret
        align 16
@favxn: vmovups ymm0, oword ptr[src + rax]  // non-temporal loop
        vmovntps [r10 + rax], ymm0
        add     rax, 32
        jl      @favxn
        sfence
        jmp     @favxe
{$endif FPC}
@lrgbwd:{$ifdef WITH_ERMS}  // backward move
        test    byte ptr[rip+CPUIDX64], 1 shl cpuERMS
        jz      @nobwe
        cmp     rax, 2048
        jb      @nobwe
        std
        lea     src, [src + rax - 1]
        lea     dst, [dst + rax - 1]
        jmp     @repmov
@nobwe: {$endif WITH_ERMS}
        {$ifdef FPC}
        cmp     rax, 256
        jb      @bsse2
        test    byte ptr[rip+CPUIDX64], 1 shl cpuAVX
        jnz      @bwdavx
        {$endif FPC}
@bsse2: sub     rax, 16
        mov     r9, rax
        movups  xmm2, oword ptr[src + rax]  // last 16
        movups  xmm1, oword ptr[src]        // first 16
        add     rax, dst
        and     rax, -16  // 16-byte aligned writes
        sub     rax, dst
        cmp     r8, CPUCACHEX64
        ja      @bwdnv    // bypass cache for cnt>512KB
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bwd:   movups  xmm0, oword ptr[src + rax]  // regular loop
        movaps  oword ptr[dst + rax], xmm0
        sub     rax, 16
        jg      @bwd
@bwdend:movups  oword ptr[dst], xmm1       // first 16
        movups  oword ptr[dst + r9], xmm2  // last 16
        ret
@01:    mov     al, byte ptr[src]
        mov     byte ptr[dst], al
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bwdnv: movups  xmm0, oword ptr[src + rax]  // non-temporal loop
        movntdq oword ptr[dst + rax], xmm0
        sub     rax, 16
        jg      @bwdnv
        sfence
        jmp     @bwdend
{$ifdef FPC}
@bwdavx:sub     rax, 32
        mov     r9, rax
        vmovups ymm2, oword ptr[src + rax]  // last 32
        vmovups ymm1, oword ptr[src]        // first 32
        add     rax, dst
        and     rax, -32  // 32-byte aligned writes
        sub     rax, dst
        cmp     r8, CPUCACHEX64
        ja      @bavxn    // bypass cache for cnt>512KB
        align 16
@bavxr: vmovups ymm0, oword ptr[src + rax]  // regular loop
        vmovaps oword ptr[dst + rax], ymm0
        sub     rax, 32
        jg      @bavxr
@bavxe: vmovups oword ptr[dst], ymm1       // first 32
        vmovups oword ptr[dst + r9], ymm2  // last 32
        vzeroupper
        ret
        align 16
@bavxn: vmovups ymm0, oword ptr[src + rax]  // non-temporal loop
        vmovntps oword ptr[dst + rax], ymm0
        sub     rax, 32
        jg      @bavxn
        sfence
        jmp     @bavxe
{$endif FPC}
@03:    movzx   eax, word ptr[src]
        mov     cl, byte ptr[src + 2]
        mov     word ptr[dst], ax
        mov     byte ptr[dst + 2], cl
        ret
@05:    mov     eax, dword ptr[src]
        mov     cl, byte ptr[src + 4]
        mov     dword ptr[dst], eax
        mov     byte ptr[dst + 4], cl
        ret
@06:    mov     eax, dword ptr[src]
        mov     cx, word ptr[src + 4]
        mov     dword ptr[dst], eax
        mov     word ptr[dst + 4], cx
        ret
@07:    mov     r8d, dword ptr[src]    // faster with no overlapping
        mov     ax, word ptr[src + 4]
        mov     cl, byte ptr[src + 6]
        mov     dword ptr[dst], r8d
        mov     word ptr[dst + 4], ax
        mov     byte ptr[dst + 6], cl
end;

procedure FillCharFast(var dst; cnt: PtrInt; value: byte);
{$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif} // rcx/rdi=dst rdx/rsi=cnt r8b/dl=val
        mov     r9, $0101010101010101
        lea     r10, [rip+@jmptab]
        {$ifdef WIN64}
        movzx   eax, r8b
        {$else}
        movzx   eax, dl
        mov     rdx, rsi // rdx=cnt
        {$endif}
        imul    rax, r9  // broadcast value into all bytes of rax (in 1 cycle)
        cmp     cnt, 32
        ja      @abv32  // >32
        sub     rdx, 8
        jg      @sml    // 9..32
        jmp     qword ptr[r10 + 64 + rdx*8] // small blocks
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@jmptab:dq @00, @01, @02, @03, @04, @05, @06, @07, @08
@sml:   cmp     dl, 8  // 9..32 bytes
        jle     @sml16
        cmp     dl, 16
        jle     @sml24
        mov     qword ptr[dst+16], rax
@sml24: mov     qword ptr[dst+8], rax
@sml16: mov     qword ptr[dst+rdx], rax // last 8 (may be overlapping)
@08:    mov     qword ptr[dst], rax
@00:    ret
@07:    mov     dword ptr[dst+3], eax
@03:    mov     word ptr[dst+1], ax
@01:    mov     byte ptr[dst], al
        ret
@06:    mov     dword ptr[dst+2], eax
@02:    mov     word ptr[dst], ax
        ret
@05:    mov     byte ptr[dst+4], al
@04:    mov     dword ptr[dst], eax
        ret
@abv32: movd    xmm0, eax
        lea     r8, [dst+cnt]  // r8 point to end
        pshufd  xmm0, xmm0, 0  // broadcast value into all bytes of xmm0
        mov     r10, rdx       // save rdx=cnt
        {$ifdef FPC} // Delphi doesn't support avx, and erms is slower
        cmp     rdx, 256
        jae     @abv256  // try erms or avx if cnt>255 (vzeroupper penaly)
        {$endif FPC}
@sse2:  movups  oword ptr[dst], xmm0  // first unaligned 16 bytes
        lea     rdx, [dst+rdx-1]
        and     rdx, -16
        add     dst, 16
        and     dst, -16 // dst is 16-bytes aligned
        sub     dst, rdx
        jnb     @last
        cmp     r10, CPUCACHEX64
        ja      @nv  // bypass cache for cnt>512KB
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@reg:   movaps  oword ptr[rdx+dst], xmm0  // regular loop
        add     dst, 16
        jnz     @reg
@last:  movups  oword ptr[r8-16], xmm0 // last unaligned 16 bytes
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@nv:    movntdq [rdx+dst], xmm0 // non-temporal loop
        add     dst, 16
        jnz     @nv
        sfence
        movups  oword ptr[r8-16], xmm0
        ret
{$ifdef FPC}
@abv256:{$ifdef WITH_ERMS}
        mov     r9b, byte ptr[rip+CPUIDX64]
        test    r9b, 1 shl cpuERMS
        jz      @noerms
        cmp     rdx, 2048  // ERMS is worth it for cnt>2KB
        jb      @noerms
        cmp     rdx, CPUCACHEX64  // non-temporal moves are still faster
        jae     @noerms
        cld
{$ifdef WIN64}
        mov     r8, rdi
        mov     rdi, dst
        mov     rcx, cnt
        rep     stosb
        mov     rdi, r8
{$else} mov     rcx, cnt
        rep stosb
{$endif}ret
@noerms:test    r9b, 1 shl cpuAVX
{$else} test    byte ptr[rip+CPUIDX64], 1 shl cpuAVX
        {$endif WITH_ERMS}
        jz      @sse2
        movups  oword ptr[dst], xmm0 // first unaligned 1..16 bytes
        add     dst, 16
        and     dst, -16
        movaps  oword ptr[dst], xmm0 // aligned 17..32 bytes
        vinsertf128 ymm0,ymm0,xmm0,1
        add     dst, 16
        and     dst, -32 // dst is 32-bytes aligned
        mov     rdx, r8
        and     rdx, -32
        sub     dst, rdx
        cmp     r10, CPUCACHEX64
        ja      @avxnv
        align 16
@avxreg:vmovaps ymmword ptr[rdx+dst], ymm0  // regular loop
        add     dst, 32
        jnz     @avxreg
@avxok: vmovups oword ptr[r8-32], ymm0  // last unaligned 32 bytes
        vzeroupper
        ret
        align 16
@avxnv: vmovntps oword ptr [rdx+dst], ymm0 // non-temporal loop
        add      dst, 32
        jnz      @avxnv
        sfence
        jmp      @avxok
{$endif FPC}
end;

function BufferLineLength(Text, TextEnd: PUTF8Char): PtrInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
{$ifdef MSWINDOWS} // Win64 ABI to System-V ABI
        push    rsi
        push    rdi
        mov     rdi, rcx
        mov     rsi, rdx
{$endif}mov     r8, rsi
        sub     r8, rdi // rdi=Text, rsi=TextEnd, r8=TextLen
        jz      @fail
        mov     ecx, edi
        movaps  xmm0, [rip + @for10]
        movaps  xmm1, [rip + @for13]
        and     rdi, -16 // check first aligned 16 bytes
        and     ecx, 15  // lower cl 4 bits indicate misalignment
        movaps  xmm2, [rdi]
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0
        pcmpeqb xmm3, xmm1
        por     xmm3, xmm2
        pmovmskb eax, xmm3
        shr     eax, cl  // shift out unaligned bytes
        test    eax, eax
        jz      @main
        bsf     eax, eax
        add     rax, rcx
        add     rax, rdi
        sub     rax, rsi
        jae     @fail   // don't exceed TextEnd
        add     rax, r8 // rax = TextFound - TextEnd + (TextEnd - Text) = offset
{$ifdef MSWINDOWS}
        pop     rdi
        pop     rsi
{$endif}ret
@main:  add     rdi, 16
        sub     rdi, rsi
        jae     @fail
        jmp     @by16
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@for10: dq      $0a0a0a0a0a0a0a0a
        dq      $0a0a0a0a0a0a0a0a
@for13: dq      $0d0d0d0d0d0d0d0d
        dq      $0d0d0d0d0d0d0d0d
@by16:  movaps  xmm2, [rdi + rsi] // check 16 bytes per loop
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0
        pcmpeqb xmm3, xmm1
        por     xmm3, xmm2
        pmovmskb eax, xmm3
        test    eax, eax
        jnz     @found
        add     rdi, 16
        jnc     @by16
@fail:  mov     rax, r8 // returns TextLen if no CR/LF found
{$ifdef MSWINDOWS}
        pop     rdi
        pop     rsi
{$endif}ret
@found: bsf     eax, eax
        add     rax, rdi
        jc      @fail
        add     rax, r8
{$ifdef MSWINDOWS}
        pop     rdi
        pop     rsi
{$endif}
end;

function crc32cfast(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifndef win64}
        mov     r8d, len
        {$endif}
        mov     eax, crc
        xor     ecx, ecx
        test    buf, buf // buf=rdx/rsi len=r8
        jz      @z
        neg     r8
        jz      @z
        not     eax
        lea     r9, [rip + crc32ctab]
        cmp     r8, -8
        jb      @head
@sml:   mov     cl, byte ptr[buf]
        inc     buf
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr[rcx * 4 + r9]
        inc     r8
        jnz     @sml
@0:     not     eax
@z:     ret
@head:  test    buf, 7
        jz      @align
        mov     cl, byte ptr[buf]
        inc     buf
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr[rcx * 4 + r9]
        inc     r8
        jnz     @head
        not     eax
        ret
@align: sub     buf, r8
        add     r8, 8
        jg      @done
        xor     r11, r11
@by8:   mov     r10d, eax
        mov     rcx, qword ptr[buf + r8 - 8]
        xor     r10d, ecx
        shr     rcx, 32
        mov     r11b, cl
        shr     ecx, 8
        mov     eax, dword ptr[r11 * 4 + r9 + 1024 * 3]
        mov     r11b, cl
        shr     ecx, 8
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 2]
        mov     r11b, cl
        shr     ecx, 8
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 1]
        mov     r11b, cl
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 0]
        mov     ecx, r10d
        mov     r11b, cl
        shr     ecx, 8
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 7]
        mov     r11b, cl
        shr     ecx, 8
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 6]
        mov     r11b, cl
        shr     ecx, 8
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 5]
        mov     r11b, cl
        xor     eax, dword ptr[r11 * 4 + r9 + 1024 * 4]
        add     r8, 8
        jle     @by8
@done:  sub     r8, 8
        jge     @e
@tail:  mov     cl, byte ptr[buf + r8]
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr[rcx * 4 + r9]
        inc     r8
        jnz     @tail
@e:     not     eax
end;

function Hash32(Data: PCardinalArray; Len: integer): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        xor     eax, eax
        xor     r9d, r9d
        test    Data, Data
        jz      @z
        {$ifdef win64}  // rcx/rdi=Data edx/esi=Len
        mov     r8, rdx
        shr     r8, 4
        {$else}
        mov     edx, esi
        shr     esi, 4
        {$endif}
        jz      @by4
@by16:  add     eax, dword ptr[Data]
        add     r9d, eax
        add     eax, dword ptr[Data+4]
        add     r9d, eax
        add     eax, dword ptr[Data+8]
        add     r9d, eax
        add     eax, dword ptr[Data+12]
        add     r9d, eax
        add     Data, 16
        {$ifdef win64}
        dec     r8d
        {$else}
        dec     esi
        {$endif}
        jnz     @by16
@by4:   mov     dh, dl
        and     dl, 15
        jz      @0
        shr     dl, 2
        jz      @rem
@4:     add     eax, dword ptr[Data]
        add     r9d, eax
        add     Data, 4
        dec     dl
        jnz     @4
@rem:   and     dh, 3
        jz      @0
        dec     dh
        jz      @1
        dec     dh
        jz      @2
        mov     ecx, dword ptr[Data]
        and     ecx, $ffffff
        jmp     @e
@2:     movzx   ecx, word ptr[Data]
        jmp     @e
@1:     movzx   ecx, byte ptr[Data]
@e:     add     eax, ecx
@0:     add     r9d, eax
        shl     r9d, 16
        xor     eax, r9d
@z:
end;

function xxHash32(crc: cardinal; P: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe{$endif}
        {$ifdef LINUX} // crc=rdi P=rsi len=rdx
        mov     r8, rdi
        mov     rcx, rsi
        {$else} // crc=r8 P=rcx len=rdx
        mov     r10, r8
        mov     r8, rcx
        mov     rcx, rdx
        mov     rdx, r10
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif}
        // P=r8 len=rcx crc=rdx
        push    r12
        push    rbx
        mov     r12d, -1640531535
        lea     r10, [rcx+rdx]
        lea     eax, [r8+165667B1H]
        cmp     rdx, 15
        jbe     @2
        lea     rsi, [r10-10H]
        lea     ebx, [r8+24234428H]
        lea     edi, [r8-7A143589H]
        lea     eax, [r8+61C8864FH]
@1:     imul    r9d, dword ptr [rcx], -2048144777
        add     rcx, 16
        imul    r11d, dword ptr [rcx-0CH], -2048144777
        add     ebx, r9d
        lea     r9d, [r11+rdi]
        rol     ebx, 13
        rol     r9d, 13
        imul    ebx, r12d
        imul    edi, r9d, -1640531535
        imul    r9d, dword ptr [rcx-8H], -2048144777
        add     r8d, r9d
        imul    r9d, dword ptr [rcx-4H], -2048144777
        rol     r8d, 13
        imul    r8d, r12d
        add     eax, r9d
        rol     eax, 13
        imul    eax, r12d
        cmp     rsi, rcx
        jnc     @1
        rol     edi, 7
        rol     ebx, 1
        rol     r8d, 12
        mov     r9d, edi
        ror     eax, 14
        add     r9d, ebx
        add     r8d, r9d
        add     eax, r8d
@2:     lea     r9, [rcx+4H]
        add     eax, edx
        cmp     r10, r9
        jc      @4
        mov     r8, r9
@3:     imul    edx, dword ptr [r8-4H], -1028477379
        add     r8, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, 668265263
        cmp     r10, r8
        jnc     @3
        lea     rdx, [r10-4H]
        sub     rdx, rcx
        mov     rcx, rdx
        and     rcx, 0FFFFFFFFFFFFFFFCH
        add     rcx, r9
@4:     cmp     r10, rcx
        jbe     @6
@5:     movzx   edx, byte ptr [rcx]
        add     rcx, 1
        imul    edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, r12d
        cmp     r10, rcx
        jnz     @5
@6:     mov     edx, eax
        shr     edx, 15
        xor     eax, edx
        imul    eax, -2048144777
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, -1028477379
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
        pop     rbx
        pop     r12
        {$ifndef LINUX}
        pop     rdi
        pop     rsi
        {$endif}
end;

function GetBitsCountPas(value: PtrInt): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        mov     rax, value
        mov     rdx, value
        shr     rax, 1
        mov     rcx, $5555555555555555
        mov     r8,  $3333333333333333
        mov     r10, $0f0f0f0f0f0f0f0f
        mov     r11, $0101010101010101
        and     rax, rcx
        sub     rdx, rax
        mov     rax, rdx
        shr     rdx, 2
        and     rax, r8
        and     rdx, r8
        add     rax, rdx
        mov     rdx, rax
        shr     rax, 4
        add     rax, rdx
        and     rax, r10
        imul    rax, r11
        shr     rax, 56
end;

procedure mul64x64(const left, right: QWord; out product: THash128Rec);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$ifdef WIN64}
        mov     rax, rcx
        mul     rdx // uses built-in 64-bit -> 128-bit multiplication
{$else} mov     r8, rdx
        mov     rax, rdi
        mul     rsi
{$endif}mov     qword ptr [r8], rax
        mov     qword ptr [r8+8], rdx
end;

function StrLenSSE2(S: pointer): PtrInt;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        // from GPL strlen64.asm by Agner Fog - www.agner.org/optimize
        {$ifdef win64}
        mov     rax, rcx             // get pointer to string from rcx
        mov     r8,  rcx             // copy pointer
        test    rcx, rcx
        {$else}
        mov     rax, rdi
        mov     ecx, edi
        test    rdi, rdi
        {$endif}
        jz      @null                // returns 0 if S=nil
        // rax=s,ecx=32-bit of s
        pxor    xmm0, xmm0           // set to zero
        and     ecx, 15              // lower 4 bits indicate misalignment
        and     rax, -16             // align pointer by 16
        // will never read outside a memory page boundary, so won't trigger GPF
        movaps  xmm1, [rax]          // read from nearest preceding boundary
        pcmpeqb xmm1, xmm0           // compare 16 bytes with zero
        pmovmskb edx, xmm1           // get one bit for each byte result
        shr     edx, cl              // shift out false bits
        shl     edx, cl              // shift back again
        bsf     edx, edx             // find first 1-bit
        jnz     @L2                  // found
        // Main loop, search 16 bytes at a time
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@L1:    add     rax, 10H             // increment pointer by 16
        movaps  xmm1, [rax]          // read 16 bytes aligned
        pcmpeqb xmm1, xmm0           // compare 16 bytes with zero
        pmovmskb edx, xmm1           // get one bit for each byte result
        bsf     edx, edx             // find first 1-bit
        // (moving the bsf out of the loop and using test here would be faster
        // for long strings on old processors, but we are assuming that most
        // strings are short, and newer processors have higher priority)
        jz      @L1                  // loop if not found
@L2:    // Zero-byte found. Compute string length
        {$ifdef win64}
        sub     rax, r8              // subtract start address
        {$else}
        sub     rax, rdi
        {$endif}
        add     rax, rdx             // add byte index
@null:
end;

function StrInt32(P: PAnsiChar; val: PtrInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifndef win64}
        mov     rcx, rdi
        mov     rdx, rsi
        {$endif win64}
        mov     r10, rdx
        sar     r10, 63         // r10=0 if val>=0 or -1 if val<0
        xor     rdx, r10
        sub     rdx, r10        // rdx=abs(val)
        cmp     rdx, 10
        jb      @3              // direct process of common val<10
        mov     rax, rdx
        lea     r8, [rip + TwoDigitLookup]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     lea     rcx, [rcx - 2]
        cmp     rax, 100
        jb      @2
        lea     r9, [rax * 2]
        shr     rax, 2
        mov     rdx, 2951479051793528259 // use power of two reciprocal to avoid division
        mul     rdx
        shr     rdx, 2
        mov     rax, rdx
        imul    rdx, -200
        lea     rdx, [rdx + r8]
        movzx   rdx, word ptr[rdx + r9]
        mov     [rcx], dx
        cmp     rax, 10
        jae     @s
@1:     or      al, '0'
        mov     byte ptr[rcx - 2], '-'
        mov     [rcx - 1], al
        lea     rax, [rcx + r10 - 1]       // includes '-' if val<0
        ret
@2:     movzx   eax, word ptr[r8 + rax * 2]
        mov     byte ptr[rcx - 1], '-'
        mov     [rcx], ax
        lea     rax, [rcx + r10]           // includes '-' if val<0
        ret
@3:     or      dl, '0'
        mov     byte ptr[rcx - 2], '-'
        mov     [rcx - 1], dl
        lea     rax, [rcx + r10 - 1]       // includes '-' if val<0
end;

function StrUInt32(P: PAnsiChar; val: PtrUInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifndef win64}
        mov     rcx, rdi
        mov     rdx, rsi
        {$endif win64}
        cmp     rdx, 10
        jb      @3           // direct process of common val<10
        mov     rax, rdx
        lea     r8, [rip + TwoDigitLookup]
@s:     lea     rcx, [rcx - 2]
        cmp     rax, 100
        jb      @2
        lea     r9, [rax * 2]
        shr     rax, 2
        mov     rdx, 2951479051793528259 // use power of two reciprocal to avoid division
        mul     rdx
        shr     rdx, 2
        mov     rax, rdx
        imul    rdx, -200
        add     rdx, r8
        movzx   rdx, word ptr[rdx + r9]
        mov     [rcx], dx
        cmp     rax, 10
        jae     @s
@1:     dec     rcx
        or      al, '0'
        mov     [rcx], al
@0:     mov     rax, rcx
        ret
@2:     movzx   eax, word ptr[r8 + rax * 2]
        mov     [rcx], ax
        mov     rax, rcx
        ret
@3:     lea     rax, [rcx - 1]
        or      dl, '0'
        mov     [rax], dl
end;

{$endif ASMX64}

// functions below are always available, even on DARWIN

function FastFindWordSorted(P: PWordArray; R: PtrInt; Value: Word): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef win64}
        push    rdi
        mov     rdi, P  // rdi=P
        {$endif}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     lea     rax, [r9 + R]
        shr     rax, 1
        lea     r10, qword ptr[rax - 1] // branchless loop
        lea     r11, qword ptr[rax + 1]
        movzx   ecx, word ptr[rdi + rax * 2]
        cmp     cx, Value
        je      @ok
        cmovg   R, r10
        cmovl   r9, r11
        cmp     r9, R
        jle     @s
@ko:    or      rax, -1
@ok:    {$ifdef win64}
        pop     rdi
        {$endif}
end;

function FastFindIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
        lea     rax, [r9 + R]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     shr     rax, 1
        lea     r10, qword ptr[rax - 1]  // efficient branchless binary search
        lea     r11, qword ptr[rax + 1]
        cmp     Value, dword ptr[P + rax * 4]
        je      @ok
        cmovl   R, r10
        cmovg   r9, r11
        lea     rax, [r9 + R]
        cmp     r9, R
        jle     @s
@ko:    or      rax, -1
@ok:
end;

function FastFindInt64Sorted(P: PInt64Array; R: PtrInt; const Value: Int64): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
        lea     rax, [r9 + R]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     shr     rax, 1
        lea     r10, qword ptr[rax - 1]  // efficient branchless binary search
        lea     r11, qword ptr[rax + 1]
        cmp     Value, qword ptr[P + rax * 8]
        je      @ok
        cmovl   R, r10
        cmovg   r9, r11
        lea     rax, [r9 + R]
        cmp     r9, R
        jle     @s
@ko:    or      rax, -1
@ok:
end;

function GetBitsCountSSE42(value: PtrInt): PtrInt;
{$ifdef FPC} assembler; nostackframe;
asm
        popcnt  rax, value
{$else} // oldest Delphi don't support this opcode
asm     .noframe
        {$ifdef win64} db $f3,$48,$0f,$B8,$c1
        {$else}        db $f3,$48,$0f,$B8,$c7 {$endif}
{$endif FPC}
end;

function crc32cBy4SSE42(crc, value: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, crc
        crc32   eax, value
end;

procedure crcblockSSE42(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax,  dword ptr[crc128] // we can't use two qword ptr here
        mov     r8d,  dword ptr[crc128 + 4]
        mov     r9d,  dword ptr[crc128 + 8]
        mov     r10d, dword ptr[crc128 + 12]
        crc32   eax,  dword ptr[data128]
        crc32   r8d,  dword ptr[data128 + 4]
        crc32   r9d,  dword ptr[data128 + 8]
        crc32   r10d, dword ptr[data128 + 12]
        mov     dword ptr[crc128],      eax
        mov     dword ptr[crc128 + 4],  r8d
        mov     dword ptr[crc128 + 8],  r9d
        mov     dword ptr[crc128 + 12], r10d
end;

procedure crcblockssse42(crc128, data128: PBlock128; count: integer);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        test    count, count
        jle     @z
        mov     rax, data128
        {$ifdef win64}
        mov     rdx, rcx
        mov     ecx, r8d
        {$else}
        mov     ecx, edx
        mov     rdx, rdi
        {$endif win64}
        mov     r8d,  dword ptr [rdx] // we can't use qword ptr here
        mov     r9d,  dword ptr [rdx + 4]
        mov     r10d, dword ptr [rdx + 8]
        mov     r11d, dword ptr [rdx + 12]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     crc32   r8d,  dword ptr [rax]
        crc32   r9d,  dword ptr [rax + 4]
        crc32   r10d, dword ptr [rax + 8]
        crc32   r11d, dword ptr [rax + 12]
        add     rax, 16
        dec     ecx
        jnz     @s
        mov     dword ptr [rdx], r8d
        mov     dword ptr [rdx + 4], r9d
        mov     dword ptr [rdx + 8], r10d
        mov     dword ptr [rdx + 12], r11d
@z:
end;

function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, crc
        not     eax
        test    len, len
        jz      @0
        test    buf, buf
        jz      @0
        jmp     @align
@7:     crc32   eax, byte ptr[buf]
        inc     buf
        dec     len
        jz      @0
@align: test    buf, 7
        jnz     @7
        mov     ecx, len
        shr     len, 3
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
        ret
{$ifdef FPC} align 16
@s:     crc32   rax, qword [buf] // hash 8 bytes per loop
{$else} .align 16
@s:     db $F2,$48,$0F,$38,$F1,$02 // circumvent Delphi inline asm compiler bug
{$endif}add     buf, 8
        dec     len
        jnz     @s
        jmp     @2
end;

function RdRand32: cardinal;
{$ifdef FPC}nostackframe; assembler; asm{$else} asm .noframe {$endif FPC}
  // rdrand eax: same opcodes for x86 and x64
  db $0f, $c7, $f0
  // returns in eax, ignore carry flag (eax=0 won't hurt)
end;

function Rdtsc: Int64;
{$ifdef FPC}nostackframe; assembler; asm{$else} asm .noframe {$endif FPC}
        rdtsc // returns the TSC in EDX:EAX
        shl     rdx, 32
        or      rax, rdx
end;

function IsXmmYmmOSEnabled: boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
// see https://software.intel.com/en-us/blogs/2011/04/14/is-avx-enabled
        xor     ecx, ecx  // specify control register XCR0 = XFEATURE_ENABLED_MASK
        db  $0f, $01, $d0 // XGETBV reads XCR0 into EDX:EAX
        and     eax, 6    // check OS has enabled both XMM (bit 1) and YMM (bit 2)
        cmp     al, 6
        sete    al
end;

procedure GetCPUID(Param: Cardinal; var Registers: TIntelRegisters);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, Param
        mov     r9, Registers
        mov     r10, rbx // preserve rbx
        xor     ebx, ebx
        xor     ecx, ecx
        xor     edx, edx
        cpuid
        mov     TIntelRegisters(r9).&eax, eax
        mov     TIntelRegisters(r9).&ebx, ebx
        mov     TIntelRegisters(r9).&ecx, ecx
        mov     TIntelRegisters(r9).&edx, edx
        mov     rbx, r10
end;

