{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Contains x86 32-bit assembly used by mormot.core.base
}

{$ifdef ASMX86}

// fast x87 versions by John O'Harrow - SSE2 are not faster
procedure FillcharFast(var dst; cnt: PtrInt; value: byte);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm     // eax=Dest edx=Count cl=Value
        mov     ch, cl                 // copy value into both bytes of cx
        cmp     edx, 32
        jl      @small
        mov     [eax], cx              // fill first 8 bytes
        mov     [eax + 2], cx
        mov     [eax + 4], cx
        mov     [eax + 6], cx
        sub     edx, 16
        fld     qword ptr[eax]
        fst     qword ptr[eax + edx]    // fill last 16 bytes
        fst     qword ptr[eax + edx + 8]
        mov     ecx, eax
        and     ecx, 7                 // 8-byte align writes
        sub     ecx, 8
        sub     eax, ecx
        add     edx, ecx
        add     eax, edx
        neg     edx
        {$ifdef FPC} align 8 {$endif}
@loop:  fst     qword ptr[eax + edx]    // fill 16 bytes per loop
        fst     qword ptr[eax + edx + 8]
        add     edx, 16
        jl      @loop
        ffree   st(0)
        fincstp
        ret
@small: test    edx, edx
        jle     @done
        mov     [eax + edx - 1], cl      // fill last byte
        and     edx, -2                  // no. of words to fill
        neg     edx
        lea     edx, [@fill + 60 + edx * 2]
        jmp     edx
        {$ifdef FPC} align 4 {$endif}
@fill:  mov     [eax + 28], cx
        mov     [eax + 26], cx
        mov     [eax + 24], cx
        mov     [eax + 22], cx
        mov     [eax + 20], cx
        mov     [eax + 18], cx
        mov     [eax + 16], cx
        mov     [eax + 14], cx
        mov     [eax + 12], cx
        mov     [eax + 10], cx
        mov     [eax + 8], cx
        mov     [eax + 6], cx
        mov     [eax + 4], cx
        mov     [eax + 2], cx
        mov     [eax], cx
        ret                         // for 4-bytes @fill alignment
@done:  db      $f3 // rep ret AMD trick here
end;

// fast x87 versions by John O'Harrow - SSE2 are not faster
procedure MoveFast(const src; var dst; cnt: PtrInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=source edx=dest ecx=count
        cmp     eax, edx
        jz      @exit                  // exit if source=dest
        cmp     ecx, 32
        ja      @lrg                   // count > 32 or count < 0
        sub     ecx, 8
        jg      @sml                   // 9..32 byte move
        jmp     dword ptr[@table + 32 + ecx * 4]   // 0..8 byte move
@sml:   fild    qword ptr[eax + ecx]   // load last 8
        fild    qword ptr[eax]         // load first 8
        cmp     ecx, 8
        jle     @sml16
        fild    qword ptr[eax + 8]     // load second 8
        cmp     ecx, 16
        jle     @sml24
        fild    qword ptr[eax + 16]    // load third 8
        fistp   qword ptr[edx + 16]    // save third 8
@sml24: fistp   qword ptr[edx + 8]     // save second 8
@sml16: fistp   qword ptr[edx]         // save first 8
        fistp   qword ptr[edx + ecx]   // save last 8
        ret
@exit:  rep     ret
        {$ifdef FPC} align 4 {$endif}
@table: dd      @exit, @m01, @m02, @m03, @m04, @m05, @m06, @m07, @m08
@lrgfwd:push    edx
        fild    qword ptr[eax]       // first 8
        lea     eax, [eax + ecx - 8]
        lea     ecx, [ecx + edx - 8]
        fild    qword ptr[eax]       // last 8
        push    ecx
        neg     ecx
        and     edx,  -8             // 8-byte align writes
        lea     ecx, [ecx + edx + 8]
        pop     edx
        {$ifdef FPC} align 8 {$endif}
@fwd:   fild    qword ptr[eax + ecx]
        fistp   qword ptr[edx + ecx]
        add     ecx, 8
        jl      @fwd
        fistp   qword ptr[edx]       // last 8
        pop     edx
        fistp   qword ptr[edx]       // first 8
        ret
@lrg:   jng     @exit                // count < 0
        cmp     eax, edx
        ja      @lrgfwd
        sub     edx, ecx
        cmp     eax, edx
        lea     edx, [edx + ecx]
        jna     @lrgfwd
        sub     ecx, 8               // backward move
        push    ecx
        fild    qword ptr[eax + ecx] // last 8
        fild    qword ptr[eax]       // first 8
        add     ecx, edx
        and     ecx, -8              // 8-byte align writes
        sub     ecx, edx
        {$ifdef FPC} align 8 {$endif}
@bwd:   fild    qword ptr[eax + ecx]
        fistp   qword ptr[edx + ecx]
        sub     ecx, 8
        jg      @bwd
        pop     ecx
        fistp   qword ptr[edx]       // first 8
        fistp   qword ptr[edx + ecx] // last 8
        ret
@m01:   movzx   ecx, byte ptr[eax]
        mov     [edx], cl
        ret
@m02:   movzx   ecx, word ptr[eax]
        mov     [edx], cx
        ret
@m03:   mov     cx, [eax]
        mov     al, [eax + 2]
        mov     [edx], cx
        mov     [edx + 2], al
        ret
@m04:   mov     ecx, [eax]
        mov     [edx], ecx
        ret
@m05:   mov     ecx, [eax]
        mov     al, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], al
        ret
@m06:   mov     ecx, [eax]
        mov     ax, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], ax
        ret
@m07:   mov     ecx, [eax]
        mov     eax, [eax + 3]
        mov     [edx], ecx
        mov     [edx + 3], eax
        ret
@m08:   mov     ecx, [eax]
        mov     eax, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], eax
end;

function CompareMem(P1, P2: Pointer; Length: PtrInt): Boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm     // eax=P1 edx=P2 ecx=Length
        cmp     eax, edx
        je      @0                 // P1=P2
        sub     ecx, 8
        jl      @small
        push    ebx
        mov     ebx, [eax]         // Compare First 4 Bytes
        cmp     ebx, [edx]
        jne     @setbig
        lea     ebx, [eax + ecx]   // Compare Last 8 Bytes
        add     edx, ecx
        mov     eax, [ebx]
        cmp     eax, [edx]
        jne     @setbig
        mov     eax, [ebx + 4]
        cmp     eax, [edx + 4]
        jne     @setbig
        sub     ecx, 4
        jle     @true              // All Bytes already Compared
        neg     ecx                // ecx=-(Length-12)
        add     ecx, ebx           // DWORD Align Reads
        and     ecx, -4
        sub     ecx, ebx
@loop:  mov     eax, [ebx + ecx]   // Compare 8 Bytes per Loop
        cmp     eax, [edx + ecx]
        jne     @setbig
        mov     eax, [ebx + ecx + 4]
        cmp     eax, [edx + ecx + 4]
        jne     @setbig
        add     ecx, 8
        jl      @loop
@true:  pop     ebx
@0:     mov     al, 1
        ret
@setbig:pop     ebx
        setz    al
        ret
@small: add     ecx, 8             // ecx=0..7
        jle     @0                 // Length <= 0
        neg     ecx                // ecx=-1..-7
        lea     ecx, [@1 + ecx * 8 + 8]   // each @#: block below = 8 bytes
        jmp     ecx
@7:     mov     cl, [eax + 6]
        cmp     cl, [edx + 6]
        jne     @setsml
@6:     mov     ch, [eax + 5]
        cmp     ch, [edx + 5]
        jne     @setsml
@5:     mov     cl, [eax + 4]
        cmp     cl, [edx + 4]
        jne     @setsml
@4:     mov     ch, [eax + 3]
        cmp     ch, [edx + 3]
        jne     @setsml
@3:     mov     cl, [eax + 2]
        cmp     cl, [edx + 2]
        jne     @setsml
@2:     mov     ch, [eax + 1]
        cmp     ch, [edx + 1]
        jne     @setsml
@1:     mov     al, [eax]
        cmp     al, [edx]
@setsml:setz    al
end;

function crc32cfast(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm  // adapted from Aleksandr Sharahov code and Maxim Masiutin remarks
        test    edx, edx
        jz      @z
        neg     ecx
        jz      @z
        not     eax
        push    ebx
        push    ebp
        lea     ebp, [crc32ctab]
@head:  test    dl, 3
        jz      @align
        movzx   ebx, byte ptr[edx]
        inc     edx
        xor     bl, al
        shr     eax, 8
        xor     eax, dword ptr[ebx * 4 + ebp]
        inc     ecx
        jnz     @head
        pop     ebp
        pop     ebx
        not     eax
@z:     ret
@align: sub     edx, ecx
        add     ecx, 8
        jg      @done
        push    esi
        push    edi
        mov     edi, edx
@by8:   mov     edx, eax
        mov     ebx, [edi + ecx - 4]
        xor     edx, [edi + ecx - 8]
        movzx   esi, bl
        mov     eax, dword ptr[esi * 4 + ebp + 1024 * 3]
        movzx   esi, bh
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 2]
        shr     ebx, 16
        movzx   esi, bl
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 1]
        movzx   esi, bh
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 0]
        movzx   esi, dl
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 7]
        movzx   esi, dh
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 6]
        shr     edx, 16
        movzx   esi, dl
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 5]
        movzx   esi, dh
        xor     eax, dword ptr[esi * 4 + ebp + 1024 * 4]
        add     ecx, 8
        jle     @by8
        mov     edx, edi
        pop     edi
        pop     esi
@done:  sub     ecx, 8
        jl      @tail
        pop     ebp
        pop     ebx
        not     eax
        ret
@tail:  movzx   ebx, byte[edx + ecx]
        xor     bl, al
        shr     eax, 8
        xor     eax, dword ptr[ebx * 4 + ebp]
        inc     ecx
        jnz     @tail
@e:     pop     ebp
        pop     ebx
        not     eax
end;

procedure crcblockfast(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // Delphi is not efficient about corresponding pascal code
        push    ebp
        push    edi
        push    esi
        mov     ebp, eax  // ebp=crc128 edi=data128
        mov     edi, edx
        mov     edx, dword ptr[eax]
        mov     ecx, dword ptr[eax + 4]
        xor     edx, dword ptr[edi]
        xor     ecx, dword ptr[edi + 4]
        movzx   esi, dl
        mov     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, dh
        shr     edx, 16
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, dl
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, dh
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 0]
        mov     edx, dword ptr[ebp + 8]
        xor     edx, dword ptr[edi + 8]
        mov     dword ptr[ebp], eax
        movzx   esi, cl
        mov     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, ch
        shr     ecx, 16
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, cl
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, ch
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr[ebp + 4], eax
        mov     ecx, dword ptr[ebp + 12]
        xor     ecx, dword ptr[edi + 12]
        movzx   esi, dl
        mov     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, dh
        shr     edx, 16
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, dl
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, dh
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr[ebp + 8], eax
        movzx   esi, cl
        mov     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, ch
        shr     ecx, 16
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, cl
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, ch
        xor     eax, dword ptr[esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr[ebp + 12], eax
        pop     esi
        pop     edi
        pop     ebp
end;

function Hash32(Data: PCardinalArray; Len: integer): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm  // eax=Data edx=Len
        push    esi
        push    edi
        mov     cl, dl
        mov     ch, dl
        xor     esi, esi
        xor     edi, edi
        test    eax, eax
        jz      @z
        shr     edx, 4
        jz      @by4
        nop
@by16:  add     esi, dword ptr[eax]
        add     edi, esi
        add     esi, dword ptr[eax+4]
        add     edi, esi
        add     esi, dword ptr[eax+8]
        add     edi, esi
        add     esi, dword ptr[eax+12]
        add     edi, esi
        add     eax, 16
        dec     edx
        jnz     @by16
@by4:   and     cl, 15
        jz      @0
        shr     cl, 2
        jz      @rem
@4:     add     esi, dword ptr[eax]
        add     edi, esi
        add     eax, 4
        dec     cl
        jnz     @4
@rem:   and     ch, 3
        jz      @0
        dec     ch
        jz      @1
        dec     ch
        jz      @2
        mov     eax, dword ptr[eax]
        and     eax, $ffffff
        jmp     @e
@2:     movzx   eax, word ptr[eax]
        jmp     @e
@1:     movzx   eax, byte ptr[eax]
@e:     add     esi, eax
@0:     add     edi, esi
        mov     eax, esi
        shl     edi, 16
        xor     eax, edi
@z:     pop     edi
        pop     esi
end;

function xxHash32(crc: cardinal; P: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        xchg    edx, ecx
        push    ebp
        push    edi
        lea     ebp, [ecx+edx]
        push    esi
        push    ebx
        sub     esp, 8
        mov     ebx, eax
        mov     dword ptr [esp], edx
        lea     eax, [ebx+165667B1H]
        cmp     edx, 15
        jbe     @2
        lea     eax, [ebp-10H]
        lea     edi, [ebx+24234428H]
        lea     esi, [ebx-7A143589H]
        mov     dword ptr [esp+4H], ebp
        mov     edx, eax
        lea     eax, [ebx+61C8864FH]
        mov     ebp, edx
@1:     mov     edx, dword ptr [ecx]
        imul    edx, -2048144777
        add     edi, edx
        rol     edi, 13
        imul    edi, -1640531535
        mov     edx, dword ptr [ecx+4]
        imul    edx, -2048144777
        add     esi, edx
        rol     esi, 13
        imul    esi, -1640531535
        mov     edx, dword ptr [ecx+8]
        imul    edx, -2048144777
        add     ebx, edx
        rol     ebx, 13
        imul    ebx, -1640531535
        mov     edx, dword ptr [ecx+12]
        lea     ecx, [ecx+16]
        imul    edx, -2048144777
        add     eax, edx
        rol     eax, 13
        imul    eax, -1640531535
        cmp     ebp, ecx
        jnc     @1
        rol     edi, 1
        rol     esi, 7
        rol     ebx, 12
        add     esi, edi
        mov     ebp, dword ptr [esp+4H]
        ror     eax, 14
        add     ebx, esi
        add     eax, ebx
@2:     lea     esi, [ecx+4H]
        add     eax, dword ptr [esp]
        cmp     ebp, esi
        jc      @4
        mov     ebx, esi
        nop
@3:     imul    edx, dword ptr [ebx-4H], -1028477379
        add     ebx, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, 668265263
        cmp     ebp, ebx
        jnc     @3
        lea     edx, [ebp-4H]
        sub     edx, ecx
        mov     ecx, edx
        and     ecx, 0FFFFFFFCH
        add     ecx, esi
@4:     cmp     ebp, ecx
        jbe     @6
@5:     movzx   edx, byte ptr [ecx]
        add     ecx, 1
        imul    edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, -1640531535
        cmp     ebp, ecx
        jnz     @5
        nop
@6:     mov     edx, eax
        add     esp, 8
        shr     edx, 15
        xor     eax, edx
        imul    eax, -2048144777
        pop     ebx
        pop     esi
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, -1028477379
        pop     edi
        pop     ebp
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
end;

function GetBitsCountPas(value: PtrInt): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // branchless Wilkes-Wheeler-Gill i386 asm implementation
        mov     edx, eax
        shr     eax, 1
        and     eax, $55555555
        sub     edx, eax
        mov     eax, edx
        shr     edx, 2
        and     eax, $33333333
        and     edx, $33333333
        add     eax, edx
        mov     edx, eax
        shr     eax, 4
        add     eax, edx
        and     eax, $0f0f0f0f
        mov     edx, eax
        shr     edx, 8
        add     eax, edx
        mov     edx, eax
        shr     edx, 16
        add     eax, edx
        and     eax, $3f
end;

function fnv32(crc: cardinal; buf: PAnsiChar; len: PtrInt): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc, edx=buf, ecx=len
        push    ebx
        test    edx, edx
        jz      @0
        neg     ecx
        jz      @0
        sub     edx, ecx
@1:     movzx   ebx, byte ptr[edx + ecx]
        xor     eax, ebx
        imul    eax, eax, 16777619
        inc     ecx
        jnz     @1
@0:     pop     ebx
end; // we tried an unrolled version, but it was slower on our Core i7!

function kr32(crc: cardinal; buf: PAnsiChar; len: PtrInt): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc, edx=buf, ecx=len
        test    ecx, ecx
        push    edi
        push    esi
        push    ebx
        push    ebp
        jz      @z
        cmp     ecx, 4
        jb      @s
@8:     mov     ebx, [edx] // unrolled version reading per dword
        add     edx, 4
        mov     esi, eax
        movzx   edi, bl
        movzx   ebp, bh
        shr     ebx, 16
        shl     eax, 5
        sub     eax, esi
        add     eax, edi
        mov     esi, eax
        shl     eax, 5
        sub     eax, esi
        lea     esi, [eax + ebp]
        add     eax, ebp
        movzx   edi, bl
        movzx   ebx, bh
        shl     eax, 5
        sub     eax, esi
        lea     ebp, [eax + edi]
        add     eax, edi
        shl     eax, 5
        sub     eax, ebp
        add     eax, ebx
        cmp     ecx, 8
        lea     ecx, [ecx - 4]
        jae     @8
        test    ecx, ecx
        jz      @z
@s:     mov     esi, eax
@1:     shl     eax, 5
        movzx   ebx, byte ptr[edx]
        inc     edx
        sub     eax, esi
        lea     esi, [eax + ebx]
        add     eax, ebx
        dec     ecx
        jnz     @1
@z:     pop     ebp
        pop     ebx
        pop     esi
        pop     edi
end;

procedure mul64x64(const left, right: QWord; out product: THash128Rec);
{$ifdef FPC}nostackframe; assembler; {$endif}
asm // adapted from FPC compiler output, which is much better than Delphi's here
        mov     ecx, eax
        mov     eax, dword ptr [ebp+8H]
        mul     dword ptr [ebp+10H]
        mov     dword ptr [ecx], eax
        mov     dword ptr [ebp-4H], edx
        mov     eax, dword ptr [ebp+8H]
        mul     dword ptr [ebp+14H]
        add     eax, dword ptr [ebp-4H]
        adc     edx, 0
        mov     dword ptr [ebp-10H], eax
        mov     dword ptr [ebp-0CH], edx
        mov     eax, dword ptr [ebp+0CH]
        mul     dword ptr [ebp+10H]
        add     eax, dword ptr [ebp-10H]
        adc     edx, 0
        mov     dword ptr [ecx+4H], eax
        mov     dword ptr [ebp-14H], edx
        mov     eax, dword ptr [ebp+0CH]
        mul     dword ptr [ebp+14H]
        add     eax, dword ptr [ebp-0CH]
        adc     edx, 0
        add     eax, dword ptr [ebp-14H]
        adc     edx, 0
        mov     dword ptr [ecx+8H], eax
        mov     dword ptr [ecx+0CH], edx
end;

function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    eax
        call    IntegerScan
        pop     edx
        test    eax, eax
        jnz     @e
        dec     eax // returns -1
        ret
@e:     sub     eax, edx
        shr     eax, 2
end;

function IntegerScan(P: PCardinalArray; Count: PtrInt; Value: cardinal): PCardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=P, edx=Count, Value=ecx
        test    eax, eax
        jz      @ok0 // avoid GPF
        cmp     edx, 8
        jb      @s2
        nop
        nop
        nop // @s1 loop align
@s1:    sub     edx, 8
        cmp     [eax], ecx
        je      @ok0
        cmp     [eax + 4], ecx
        je      @ok4
        cmp     [eax + 8], ecx
        je      @ok8
        cmp     [eax + 12], ecx
        je      @ok12
        cmp     [eax + 16], ecx
        je      @ok16
        cmp     [eax + 20], ecx
        je      @ok20
        cmp     [eax + 24], ecx
        je      @ok24
        cmp     [eax + 28], ecx
        je      @ok28
        add     eax, 32
        cmp     edx, 8
        jae     @s1
@s2:    test    edx, edx
        jz      @z
        cmp     [eax], ecx
        je      @ok0
        dec     edx
        jz      @z
        cmp     [eax + 4], ecx
        je      @ok4
        dec     edx
        jz      @z
        cmp     [eax + 8], ecx
        je      @ok8
        dec     edx
        jz      @z
        cmp     [eax + 12], ecx
        je      @ok12
        dec     edx
        jz      @z
        cmp     [eax + 16], ecx
        je      @ok16
        dec     edx
        jz      @z
        cmp     [eax + 20], ecx
        je      @ok20
        dec     edx
        jz      @z
        cmp     [eax + 24], ecx
        je      @ok24
@z:     xor     eax, eax // return nil if not found
        ret
@ok0:   rep     ret
@ok28:  add     eax, 28
        ret
@ok24:  add     eax, 24
        ret
@ok20:  add     eax, 20
        ret
@ok16:  add     eax, 16
        ret
@ok12:  add     eax, 12
        ret
@ok8:   add     eax, 8
        ret
@ok4:   add     eax, 4
end;

function IntegerScanExists(P: PCardinalArray; Count: PtrInt; Value: cardinal): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=P, edx=Count, Value=ecx
        test    eax, eax
        jz      @z // avoid GPF
        cmp     edx, 8
        jae     @s1
        jmp     dword ptr[edx * 4 + @Table]
@Table: dd      @z, @1, @2, @3, @4, @5, @6, @7
@s1:    // fast search by 8 integers (pipelined instructions)
        sub     edx, 8
        cmp     [eax], ecx
        je      @ok
        cmp     [eax + 4], ecx
        je      @ok
        cmp     [eax + 8], ecx
        je      @ok
        cmp     [eax + 12], ecx
        je      @ok
        cmp     [eax + 16], ecx
        je      @ok
        cmp     [eax + 20], ecx
        je      @ok
        cmp     [eax + 24], ecx
        je      @ok
        cmp     [eax + 28], ecx
        je      @ok
        add     eax, 32
        cmp     edx, 8
        jae     @s1
        jmp     dword ptr[edx * 4 + @Table]
@7:     cmp     [eax + 24], ecx
        je      @ok
@6:     cmp     [eax + 20], ecx
        je      @ok
@5:     cmp     [eax + 16], ecx
        je      @ok
@4:     cmp     [eax + 12], ecx
        je      @ok
@3:     cmp     [eax + 8], ecx
        je      @ok
@2:     cmp     [eax + 4], ecx
        je      @ok
@1:     cmp     [eax], ecx
        je      @ok
@z:     xor     eax, eax
        ret
@ok:    mov     al, 1
end;

{$endif ASMX86}

// functions below are always available, even on DARWIN

{$ifndef FPC_OR_UNICODE}

function CompareInt64(const A, B: Int64): integer;
asm // Delphi x86 compiler is not efficient at compiling Int64 comparisons
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
        jnb     @p
@n:     mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@nz:    jl      @n
@p:     mov     eax, 1
end;

function CompareQword(const A, B: QWord): integer;
asm // Delphi x86 compiler is not efficient, and oldest even incorrect
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
@nz:    jnb     @p
        mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

{$endif FPC_OR_UNICODE}

function GetBitsCountSSE42(value: PtrInt): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        {$ifdef FPC}
        popcnt  eax, eax
        {$else} // oldest Delphi don't support this opcode
        db      $f3,$0f,$B8,$c0
        {$endif}
end;

function crc32cBy4SSE42(crc, value: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc, edx=value
        {$ifdef FPC_OR_UNICODE}
        crc32   eax, edx
        {$else}
        db      $F2, $0F, $38, $F1, $C2
        {$endif}
end;

procedure crcblockSSE42(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc128, edx=data128
        mov     ecx, eax
        {$ifdef FPC_OR_UNICODE}
        mov     eax, dword ptr[ecx]
        crc32   eax, dword ptr[edx]
        mov     dword ptr[ecx], eax
        mov     eax, dword ptr[ecx + 4]
        crc32   eax, dword ptr[edx + 4]
        mov     dword ptr[ecx + 4], eax
        mov     eax, dword ptr[ecx + 8]
        crc32   eax, dword ptr[edx + 8]
        mov     dword ptr[ecx + 8], eax
        mov     eax, dword ptr[ecx + 12]
        crc32   eax, dword ptr[edx + 12]
        mov     dword ptr[ecx + 12], eax
        {$else}
        mov     eax, dword ptr[ecx]
        db      $F2, $0F, $38, $F1, $02
        mov     dword ptr[ecx], eax
        mov     eax, dword ptr[ecx + 4]
        db      $F2, $0F, $38, $F1, $42, $04
        mov     dword ptr[ecx + 4], eax
        mov     eax, dword ptr[ecx + 8]
        db      $F2, $0F, $38, $F1, $42, $08
        mov     dword ptr[ecx + 8], eax
        mov     eax, dword ptr[ecx + 12]
        db      $F2, $0F, $38, $F1, $42, $0C
        mov     dword ptr[ecx + 12], eax
        {$endif FPC_OR_UNICODE}
end;

procedure crcblockssse42(crc128, data128: PBlock128; count: integer);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc128 edx=data128 ecx=count
          push    ebx
          push    esi
          push    edi
          push    ebp
          test    count, count
          jle     @z
          mov     ebp, count
          mov     esi, crc128
          mov     edi, data128
          mov     eax, dword ptr[esi]
          mov     ebx, dword ptr[esi + 4]
          mov     ecx, dword ptr[esi + 8]
          mov     edx, dword ptr[esi + 12]
{$ifdef FPC} align 8
@s:       crc32   eax, dword ptr[edi]
          crc32   ebx, dword ptr[edi + 4]
          crc32   ecx, dword ptr[edi + 8]
          crc32   edx, dword ptr[edi + 12]
{$else}@s:db $F2, $0F, $38, $F1, $07
          db $F2, $0F, $38, $F1, $5F, $04
          db $F2, $0F, $38, $F1, $4F, $08
          db $F2, $0F, $38, $F1, $57, $0C
{$endif}  add     edi, 16
          dec     ebp
          jnz     @s
          mov     dword ptr[esi], eax
          mov     dword ptr[esi + 4], ebx
          mov     dword ptr[esi + 8], ecx
          mov     dword ptr[esi + 12], edx
@z:       pop     ebp
          pop     edi
          pop     esi
          pop     ebx
end;

function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=crc, edx=buf, ecx=len
        not     eax
        test    ecx, ecx
        jz      @0
        test    edx, edx
        jz      @0
        jmp     @align
        db      $8D, $0B4, $26, $00, $00, $00, $00 // manual @by8 align 16
@a:     {$ifdef FPC}
        crc32   eax, byte ptr[edx]
        {$else}
        db      $F2, $0F, $38, $F0, $02
        {$endif}
        inc     edx
        dec     ecx
        jz      @0
@align: test    dl, 3
        jnz     @a
        push    ecx
        shr     ecx, 3
        jnz     @by8
@rem:   pop     ecx
        test    cl, 4
        jz      @4
        {$ifdef FPC}
        crc32   eax, dword ptr[edx]
        {$else}
        db      $F2, $0F, $38, $F1, $02
        {$endif}
        add     edx, 4
@4:     test    cl, 2
        jz      @2
        {$ifdef FPC}
        crc32   eax, word ptr[edx]
        {$else}
        db      $66, $F2, $0F, $38, $F1, $02
        {$endif}
        add     edx, 2
@2:     test    cl, 1
        jz      @0
        {$ifdef FPC}
        crc32   eax, byte ptr[edx]
        {$else}
        db      $F2, $0F, $38, $F0, $02
        {$endif}
@0:     not     eax
        ret
@by8:   {$ifdef FPC}
        crc32   eax, dword ptr[edx]
        crc32   eax, dword ptr[edx + 4]
        {$else}
        db      $F2, $0F, $38, $F1, $02
        db      $F2, $0F, $38, $F1, $42, $04
        {$endif}
        add     edx, 8
        dec     ecx
        jnz     @by8
        jmp     @rem
end;

function RdRand32: cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
  // rdrand eax: same opcodes for x86 and x64
  db $0f, $c7, $f0
  // returns in eax, ignore carry flag (eax=0 won't hurt)
end;

function IsXmmYmmOSEnabled: boolean; assembler;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // see https://software.intel.com/en-us/blogs/2011/04/14/is-avx-enabled
        xor     ecx, ecx  // specify control register XCR0 = XFEATURE_ENABLED_MASK
        db  $0f, $01, $d0 // XGETBV reads XCR0 into EDX:EAX
        and     eax, 6    // check OS has enabled both XMM (bit 1) and YMM (bit 2)
        cmp     al, 6
        sete    al
end;

procedure GetCPUID(Param: Cardinal; var Registers: TIntelRegisters);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    esi
        push    edi
        mov     esi, edx
        mov     edi, eax
        pushfd
        pop     eax
        mov     edx, eax
        xor     eax, $200000
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax, edx
        jz      @nocpu
        push    ebx
        mov     eax, edi
        xor     ecx, ecx
        cpuid
        mov     TIntelRegisters(esi).&eax, eax
        mov     TIntelRegisters(esi).&ebx, ebx
        mov     TIntelRegisters(esi).&ecx, ecx
        mov     TIntelRegisters(esi).&edx, edx
        pop     ebx
@nocpu: pop     edi
        pop     esi
end;

