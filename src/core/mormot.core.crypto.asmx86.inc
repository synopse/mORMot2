{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86 32-bit assembly used by mormot.core.crypto.pas
}


{$ifdef FPC}
  // disabled some FPC paranoid warnings
  {$WARN 7102 off : Use of +offset(%ebp) for parameters invalid here }
  {$WARN 7104 off : Use of -offset(%ebp) is not recommended for local variable access }
{$endif FPC}


{$ifdef ASMX86}

// those functions use global variables, so are not PIC-compatible

procedure aesencryptasm(const ctxt: TAesContext; bi, bo: PWA4);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // rolled optimized encryption asm version by A. Bouchez
        push    ebx
        push    esi
        push    edi
        push    ebp
        add     esp,  - 24
        mov     [esp + 4], ecx
        mov     ecx, eax // ecx=pk
        movzx   eax, byte ptr[eax].TAesContext.Rounds
        dec     eax
        mov     [esp + 20], eax
        mov     ebx, [edx]
        xor     ebx, [ecx]
        mov     esi, [edx + 4]
        xor     esi, [ecx + 4]
        mov     eax, [edx + 8]
        xor     eax, [ecx + 8]
        mov     edx, [edx + 12]
        xor     edx, [ecx + 12]
        lea     ecx, [ecx + 16]
@1:     // pk=ecx s0=ebx s1=esi s2=eax s3=edx
        movzx   edi, bl
        mov     edi, dword ptr[4 * edi + Te0]
        movzx   ebp, si
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + Te1]
        mov     ebp, eax
        shr     ebp, $10
        and     ebp, $ff
        xor     edi, dword ptr[4 * ebp + Te2]
        mov     ebp, edx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Te3]
        mov     [esp + 8], edi
        mov     edi, esi
        and     edi, 255
        mov     edi, dword ptr[4 * edi + Te0]
        movzx   ebp, ax
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + Te1]
        mov     ebp, edx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + Te2]
        mov     ebp, ebx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Te3]
        mov     [esp + 12], edi
        movzx   edi, al
        mov     edi, dword ptr[4 * edi + Te0]
        movzx   ebp, dh
        xor     edi, dword ptr[4 * ebp + Te1]
        mov     ebp, ebx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + Te2]
        mov     ebp, esi
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Te3]
        mov     [esp + 16], edi
        and     edx, 255
        mov     edx, dword ptr[4 * edx + Te0]
        shr     ebx, $08
        and     ebx, 255
        xor     edx, dword ptr[4 * ebx + Te1]
        shr     esi, $10
        and     esi, 255
        xor     edx, dword ptr[4 * esi + Te2]
        shr     eax, $18
        xor     edx, dword ptr[4 * eax + Te3]
        mov     ebx, [ecx]
        xor     ebx, [esp + 8]
        mov     esi, [ecx + 4]
        xor     esi, [esp + 12]
        mov     eax, [ecx + 8]
        xor     eax, [esp + 16]
        xor     edx, [ecx + 12]
        lea     ecx, [ecx + 16]
        dec     byte ptr[esp + 20]
        jne     @1
        mov     ebp, ecx // ebp=pk
        movzx   ecx, bl
        mov     edi, esi
        movzx   ecx, byte ptr[ecx + SBox]
        shr     edi, $08
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, eax
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, edx
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp]
        mov     edi, [esp + 4]
        mov     [edi], ecx
        mov     ecx, esi
        and     ecx, 255
        movzx   ecx, byte ptr[ecx + SBox]
        movzx   edi, ah
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, edx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, ebx
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp + 4]
        mov     edi, [esp + 4]
        mov     [edi + 4], ecx
        mov     ecx, eax
        and     ecx, 255
        movzx   ecx, byte ptr[ecx + SBox]
        movzx   edi, dh
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, ebx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, esi
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp + 8]
        mov     edi, [esp + 4]
        mov     [edi + 8], ecx
        and     edx, 255
        movzx   edx, byte ptr[edx + SBox]
        shr     ebx, $08
        and     ebx, 255
        xor     ecx, ecx
        mov     cl, byte ptr[ebx + SBox]
        shl     ecx, $08
        xor     edx, ecx
        shr     esi, $10
        and     esi, 255
        xor     ecx, ecx
        mov     cl, byte ptr[esi + SBox]
        shl     ecx, $10
        xor     edx, ecx
        shr     eax, $18
        movzx   eax, byte ptr[eax + SBox]
        shl     eax, $18
        xor     edx, eax
        xor     edx, [ebp + 12]
        mov     eax, [esp + 4]
        mov     [eax + 12], edx
        add     esp, 24
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

procedure aesdecrypt386(const ctxt: TAesContext; bi, bo: PWA4);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        push    esi
        push    edi
        push    ebp
        add     esp,  - 20
        mov     [esp], ecx
        movzx   ecx, byte ptr[eax].taescontext.rounds
        lea     esi, [4 * ecx]
        lea     ecx, [ecx - 1]
        lea     eax, [eax + 4 * esi] // eax=@ctx.rk[ctx.rounds]=pk
        mov     [esp + 16], ecx      // [esp+16]=ctx.round
        mov     ebx, [edx]
        xor     ebx, [eax]
        mov     esi, [edx + 4]
        xor     esi, [eax + 4]
        mov     ecx, [edx + 8]
        xor     ecx, [eax + 8]
        mov     edx, [edx + 12]
        xor     edx, [eax + 12]
        lea     eax, [eax - 16]
@1:     // pk=eax s0=ebx s1=esi s2=ecx s3=edx
        movzx   edi, bl
        mov     edi, dword ptr[4 * edi + Td0]
        movzx   ebp, dh
        xor     edi, dword ptr[4 * ebp + Td1]
        mov     ebp, ecx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + Td2]
        mov     ebp, esi
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Td3]
        mov     [esp + 4], edi
        mov     edi, esi
        and     edi, 255
        mov     edi, dword ptr[4 * edi + Td0]
        movzx   ebp, bh
        xor     edi, dword ptr[4 * ebp + Td1]
        mov     ebp, edx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + Td2]
        mov     ebp, ecx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Td3]
        mov     [esp + 8], edi
        movzx   edi, cl
        mov     edi, dword ptr[4 * edi + Td0]
        movzx   ebp, si
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + Td1]
        mov     ebp, ebx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + Td2]
        mov     ebp, edx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + Td3]
        mov     [esp + 12], edi
        and     edx, 255
        mov     edx, dword ptr[4 * edx + Td0]
        movzx   ecx, ch
        xor     edx, dword ptr[4 * ecx + Td1]
        shr     esi, $10
        and     esi, 255
        xor     edx, dword ptr[4 * esi + Td2]
        shr     ebx, $18
        xor     edx, dword ptr[4 * ebx + Td3]
        xor     edx, [eax + 12]
        mov     ebx, [eax]
        xor     ebx, [esp + 4]
        mov     esi, [eax + 4]
        xor     esi, [esp + 8]
        mov     ecx, [eax + 8]
        xor     ecx, [esp + 12]
        lea     eax, [eax - 16]
        dec     byte ptr[esp + 16]
        jnz     @1
        mov     ebp, eax
        movzx   eax, bl
        movzx   eax, byte ptr[eax + InvSBox]
        movzx   edi, dh
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, ecx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, esi
        shr     edi, $18
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp]
        mov     edi, [esp]
        mov     [edi], eax
        mov     eax, esi
        and     eax, 255
        movzx   eax, byte ptr[eax + InvSBox]
        movzx   edi, bh
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, edx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, ecx
        shr     edi, $18
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp + 4]
        mov     edi, [esp]
        mov     [edi + 4], eax
        movzx   eax, cl
        movzx   eax, byte ptr[eax + InvSBox]
        movzx   edi, si
        shr     edi, $08
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, ebx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, edx
        shr     edi, $18
        movzx   edi, byte ptr[edi + InvSBox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp + 8]
        mov     edi, [esp]
        mov     [edi + 8], eax
        and     edx, 255
        movzx   eax, byte ptr[edx + InvSBox]
        shr     ecx, $08
        and     ecx, 255
        movzx   edx, byte ptr[ecx + InvSBox]
        shl     edx, $08
        xor     eax, edx
        shr     esi, $10
        and     esi, 255
        movzx   edx, byte ptr[esi + InvSBox]
        shl     edx, $10
        xor     eax, edx
        shr     ebx, $18
        movzx   edx, byte ptr[ebx + InvSBox]
        shl     edx, $18
        xor     eax, edx
        xor     eax, [ebp + 12]
        mov     [edi + 12], eax
        add     esp, 20
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

procedure Sha256Compressx86(HW: pointer);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        push    esi
        push    edi
        push    ebp
        xor     edi, edi // edi = i
        mov     ebp, eax // ebp = HW = TSHAHash followed by W[0..63]
        // rolled version faster than the unrolled one (good pipelining work :)
@s:     mov     eax, [ebp].TSHAHash.E
        mov     ecx, eax
        mov     edx, eax
        mov     ebx, eax // ebx=E
        ror     eax, 6
        ror     edx, 11
        ror     ecx, 25
        xor     eax, edx
        mov     edx, [ebp].TSHAHash.G
        xor     eax, ecx
        mov     ecx, [ebp].TSHAHash.H
        add     ecx, eax // T1=ecx
        mov     eax, [ebp].TSHAHash.F
        mov     [ebp].TSHAHash.H, edx
        mov     [ebp].TSHAHash.G, eax
        xor     eax, edx
        mov     [ebp].TSHAHash.F, ebx
        and     eax, ebx
        xor     eax, edx
        add     eax, dword ptr [K256 + edi * 4]
        add     eax, ecx
        mov     ecx, [ebp].TSHAHash.D
        add     eax, dword ptr [ebp + edi * 4 + 32] // 32 = SizeOf(TSHAHash)
        mov     ebx, [ebp].TSHAHash.A
        //  eax= T1 := ebp + Sum1(E) +(((F xor G) and E) xor G)+K256[i]+W[i];
        add     ecx, eax
        mov     esi, eax  // esi = T1
        mov     [ebp].TSHAHash.E, ecx // E := D + T1;
        mov     eax, ebx // Sum0(A)
        mov     edx, ebx
        ror     eax, 2
        mov     ecx, ebx
        ror     edx, 13
        ror     ecx, 22
        xor     eax, edx
        xor     eax, ecx // eax = Sum0(A)
        mov     ecx, [ebp].TSHAHash.B
        add     esi, eax
        mov     eax, ebx // ebx=A
        mov     edx, ebx // eax=edx=A
        or      eax, ecx
        and     eax, [ebp].TSHAHash.C   // eax = (A or B)and C
        and     edx, ecx
        or      eax, edx // eax = ((A or B)and C) or (A and B)
        inc     edi
        add     esi, eax  // esi= T1+T2
        mov     [ebp].TSHAHash.A, esi
        mov     eax, [ebp].TSHAHash.C // eax=C ecx=B ebx=A
        mov     [ebp].TSHAHash.B, ebx
        mov     [ebp].TSHAHash.C, ecx
        mov     [ebp].TSHAHash.D, eax
        cmp     edi, 64
        jnz     @s
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

{  MMX 32-bit assembler version based on optimized SHA-3 kernel by Eric Grange
   https://www.delphitools.info/2016/04/19/new-sha-3-permutation-kernel }

procedure KeccakPermutationKernel(B, A, C: Pointer);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        add     edx, 128
        add     eax, 128
        movq    mm1, [edx - 120]
        movq    mm4, [edx - 96]
        movq    mm3, [edx - 104]
        pxor    mm1, [edx - 80]
        movq    mm5, [edx + 16]
        pxor    mm1, [edx]
        movq    mm2, [edx - 112]
        pxor    mm1, [edx + 40]
        pxor    mm1, [edx - 40]
        movq    mm0, [edx - 128]
        movq    mm6, mm1
        pxor    mm4, [edx - 56]
        movq    [ecx + 8], mm1
        psrlq   mm6, 63
        pxor    mm4, [edx + 24]
        pxor    mm4, [edx + 64]
        pxor    mm4, [edx - 16]
        psllq   mm1, 1
        pxor    mm2, [edx + 48]
        por     mm1, mm6
        movq    mm6, [edx - 88]
        pxor    mm1, mm4
        pxor    mm2, [edx - 32]
        pxor    mm2, [edx - 72]
        pxor    mm6, mm1
        movq    mm7, mm6
        psrlq   mm7, 28
        psllq   mm6, 36
        por     mm6, mm7
        pxor    mm2, [edx + 8]
        movq    [eax], mm6
        movq    mm6, [edx + 32]
        movq    mm7, mm4
        psrlq   mm7, 63
        psllq   mm4, 1
        pxor    mm0, mm6
        por     mm4, mm7
        pxor    mm4, mm2
        pxor    mm5, mm4
        movq    mm7, mm5
        pxor    mm0, [edx - 8]
        psllq   mm5, 21
        psrlq   mm7, 43
        pxor    mm6, mm1
        por     mm5, mm7
        movq    [eax - 104], mm5
        movq    mm5, [edx - 48]
        pxor    mm0, mm5
        movq    mm7, mm6
        psrlq   mm7, 46
        psllq   mm6, 18
        por     mm6, mm7
        movq    [eax - 16], mm6
        movq    mm6, [edx + 56]
        pxor    mm5, mm1
        movq    mm7, mm5
        pxor    mm3, mm6
        psllq   mm5, 3
        psrlq   mm7, 61
        pxor    mm3, [edx + 16]
        pxor    mm3, [edx - 24]
        por     mm5, mm7
        pxor    mm6, mm4
        pxor    mm0, [edx - 88]
        movq    mm7, mm6
        psrlq   mm7, 8
        movq    [eax - 72], mm5
        movq    mm5, mm2
        psllq   mm2, 1
        psllq   mm6, 56
        psrlq   mm5, 63
        por     mm6, mm7
        por     mm2, mm5
        pxor    mm2, mm0
        movq    [eax + 24], mm6
        movq    mm5, [edx - 120]
        movq    mm6, mm0
        psllq   mm0, 1
        pxor    mm5, mm2
        pxor    mm3, [edx - 64]
        psrlq   mm6, 63
        por     mm0, mm6
        movq    mm6, [edx - 64]
        movq    mm7, mm5
        psllq   mm5, 1
        psrlq   mm7, 63
        pxor    mm6, mm4
        por     mm5, mm7
        pxor    mm0, mm3
        movq    mm7, mm6
        movq    [eax - 48], mm5
        movq    mm5, [edx]
        psllq   mm6, 55
        psrlq   mm7, 9
        por     mm6, mm7
        movq    [eax + 40], mm6
        movq    mm6, [edx - 40]
        pxor    mm5, mm2
        movq    mm7, mm5
        psllq   mm5, 45
        psrlq   mm7, 19
        pxor    mm6, mm2
        por     mm5, mm7
        movq    [eax - 64], mm5
        movq    mm5, [edx + 40]
        movq    mm7, mm6
        pxor    mm5, mm2
        psllq   mm6, 10
        psrlq   mm7, 54
        por     mm6, mm7
        movq    [eax + 8], mm6
        movq    mm6, [edx - 96]
        movq    mm7, mm3
        psrlq   mm7, 63
        psllq   mm3, 1
        por     mm3, mm7
        movq    mm7, mm5
        psllq   mm5, 2
        psrlq   mm7, 62
        por     mm5, mm7
        movq    [eax + 64], mm5
        movq    mm5, [edx + 24]
        pxor    mm6, mm0
        movq    mm7, mm6
        psrlq   mm7, 37
        psllq   mm6, 27
        por     mm6, mm7
        movq    [eax - 8], mm6
        pxor    mm5, mm0
        movq    mm6, [edx - 16]
        movq    mm7, mm5
        psllq   mm5, 8
        pxor    mm3, [ecx + 8]
        psrlq   mm7, 56
        pxor    mm6, mm0
        por     mm5, mm7
        movq    [eax - 24], mm5
        movq    mm7, mm6
        psllq   mm6, 39
        movq    mm5, [edx - 112]
        psrlq   mm7, 25
        por     mm6, mm7
        movq    [eax + 48], mm6
        movq    mm6, [edx - 24]
        pxor    mm5, mm3
        movq    mm7, mm5
        psrlq   mm7, 2
        psllq   mm5, 62
        por     mm5, mm7
        movq    [eax + 32], mm5
        movq    mm5, [edx - 104]
        pxor    mm6, mm4
        movq    mm7, mm6
        psrlq   mm7, 39
        psllq   mm6, 25
        por     mm6, mm7
        pxor    mm5, mm4
        movq    [eax - 32], mm6
        movq    mm6, [edx - 128]
        pxor    mm6, mm1
        movq    mm4, mm6
        movq    [eax - 128], mm6
        movq    mm4, mm6
        movq    mm6, [edx - 8]
        movq    mm7, mm5
        psrlq   mm7, 36
        psllq   mm5, 28
        pxor    mm6, mm1
        por     mm5, mm7
        movq    mm7, mm6
        psrlq   mm7, 23
        movq    mm1, mm5
        movq    [eax - 88], mm5
        movq    mm5, [edx - 56]
        pxor    mm5, mm0
        psllq   mm6, 41
        por     mm6, mm7
        movq    [eax + 56], mm6
        movq    mm6, [edx + 48]
        pxor    mm6, mm3
        movq    mm7, mm5
        psrlq   mm7, 44
        psllq   mm5, 20
        por     mm5, mm7
        movq    [eax - 80], mm5
        pandn   mm1, mm5
        movq    mm5, [edx - 32]
        movq    mm7, mm6
        psrlq   mm7, 3
        psllq   mm6, 61
        por     mm6, mm7
        pxor    mm1, mm6
        movq    [eax - 56], mm6
        movq    mm6, [edx + 8]
        movq    [edx - 56], mm1
        movq    mm1, [eax - 112]
        pxor    mm5, mm3
        movq    mm7, mm5
        psllq   mm5, 43
        psrlq   mm7, 21
        pxor    mm6, mm3
        por     mm5, mm7
        movq    mm1, mm5
        movq    mm5, [edx - 80]
        pxor    mm5, mm2
        movq    mm2, [eax - 104]
        movq    mm7, mm6
        psrlq   mm7, 49
        psllq   mm6, 15
        por     mm6, mm7
        movq    [eax + 16], mm6
        movq    mm6, [edx + 64]
        movq    [eax - 96], mm6
        movq    mm7, mm5
        psrlq   mm7, 20
        psllq   mm5, 44
        pxor    mm6, mm0
        por     mm5, mm7
        movq    mm7, mm6
        psrlq   mm7, 50
        psllq   mm6, 14
        por     mm6, mm7
        pandn   mm2, mm6
        movq    mm0, mm5
        pandn   mm0, mm1
        pxor    mm2, mm1
        pandn   mm1, [eax - 104]
        movq    [edx - 112], mm2
        pandn   mm4, mm5
        pxor    mm1, mm5
        movq    [eax - 120], mm5
        movq    mm2, [eax - 40]
        movq    [edx - 120], mm1
        movq    mm5, [edx - 72]
        movq    mm1, [eax - 64]
        pxor    mm4, mm6
        movq    [edx - 96], mm4
        pxor    mm5, mm3
        movq    mm4, [eax - 88]
        movq    mm7, mm5
        movq    mm3, mm6
        pxor    mm0, [eax - 128]
        movq    [edx - 128], mm0
        movq    mm6, [eax - 72]
        psllq   mm5, 6
        psrlq   mm7, 58
        movq    mm0, [eax - 56]
        por     mm5, mm7
        movq    mm2, mm5
        movq    mm5, [eax - 80]
        movq    mm7, mm1
        pandn   mm7, mm0
        pxor    mm7, mm6
        movq    [edx - 72], mm7
        movq    mm7, [eax - 72]
        pandn   mm6, mm1
        pxor    mm6, mm5
        pandn   mm0, mm4
        pandn   mm5, mm7
        movq    mm7, [eax]
        pxor    mm5, mm4
        movq    mm4, [eax - 24]
        movq    [edx - 80], mm6
        movq    mm6, [eax - 48]
        movq    [edx - 88], mm5
        movq    mm5, mm1
        movq    mm1, [eax - 16]
        pxor    mm0, mm5
        movq    mm5, mm1
        pandn   mm3, [eax - 128]
        pxor    mm3, [eax - 104]
        movq    [edx - 64], mm0
        movq    mm0, [eax + 8]
        movq    [edx - 104], mm3
        movq    mm3, [eax - 32]
        pandn   mm6, mm2
        pxor    mm6, mm5
        movq    [edx - 16], mm6
        movq    mm6, [eax + 56]
        pandn   mm3, mm4
        pxor    mm3, mm2
        movq    [edx - 40], mm3
        movq    mm3, [eax - 32]
        pandn   mm5, [eax - 48]
        pxor    mm5, mm4
        movq    [edx - 24], mm5
        pandn   mm7, mm0
        movq    mm5, [eax + 16]
        pandn   mm4, mm1
        pxor    mm4, mm3
        movq    [edx - 32], mm4
        movq    mm4, [eax + 40]
        movq    mm1, mm5
        movq    mm5, [eax + 48]
        pandn   mm5, mm6
        pxor    mm5, mm4
        pandn   mm2, mm3
        movq    mm3, [eax - 8]
        movq    [edx + 40], mm5
        movq    mm5, [eax + 24]
        pxor    mm7, mm3
        movq    [edx - 8], mm7
        movq    mm7, [eax + 64]
        pxor    mm2, [eax - 48]
        movq    [edx - 48], mm2
        movq    mm2, mm5
        pandn   mm2, mm3
        pxor    mm2, mm1
        movq    [edx + 16], mm2
        pandn   mm3, [eax]
        movq    mm2, mm5
        movq    mm5, [eax + 48]
        pandn   mm6, mm7
        pxor    mm6, mm5
        movq    [edx + 48], mm6
        pandn   mm1, mm2
        movq    mm6, [eax + 32]
        pxor    mm1, mm0
        pxor    mm3, mm2
        movq    [edx + 24], mm3
        pandn   mm0, [eax + 16]
        pxor    mm0, [eax]
        movq    mm3, mm4
        movq    [edx + 8], mm1
        movq    [edx], mm0
        movq    mm0, mm6
        movq    mm1, [eax + 56]
        pandn   mm4, mm5
        pxor    mm4, mm0
        pandn   mm0, mm3
        pxor    mm0, mm7
        movq    [edx + 32], mm4
        pandn   mm7, mm6
        pxor    mm7, mm1
        movq    [edx + 56], mm7
        movq    [edx + 64], mm0
end;

{$endif ASMX86}


procedure bswap256(s, d: PIntegerArray);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        mov     ecx, eax // ecx=s, edx=d
        mov     eax, [ecx]
        mov     ebx, [ecx + 4]
        bswap   eax
        bswap   ebx
        mov     [edx], eax
        mov     [edx + 4], ebx
        mov     eax, [ecx + 8]
        mov     ebx, [ecx + 12]
        bswap   eax
        bswap   ebx
        mov     [edx + 8], eax
        mov     [edx + 12], ebx
        mov     eax, [ecx + 16]
        mov     ebx, [ecx + 20]
        bswap   eax
        bswap   ebx
        mov     [edx + 16], eax
        mov     [edx + 20], ebx
        mov     eax, [ecx + 24]
        mov     ebx, [ecx + 28]
        bswap   eax
        bswap   ebx
        mov     [edx + 24], eax
        mov     [edx + 28], ebx
        pop     ebx
end;

procedure bswap160(s, d: PIntegerArray);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        mov     ecx, eax // ecx=s, edx=d
        mov     eax, [ecx]
        mov     ebx, [ecx + 4]
        bswap   eax
        bswap   ebx
        mov     [edx], eax
        mov     [edx + 4], ebx
        mov     eax, [ecx + 8]
        mov     ebx, [ecx + 12]
        bswap   eax
        bswap   ebx
        mov     [edx + 8], eax
        mov     [edx + 12], ebx
        mov     eax, [ecx + 16]
        bswap   eax
        mov     [edx + 16], eax
        pop     ebx
end;

function gf2_multiply(x, y, m: PtrUInt): PtrUInt;
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm     // eax=x edx=y ecx=m
        push    esi
        push    edi
        push    ebx
        push    ebp
        mov     ebp, 32
        mov     ebx, eax
        and     eax, 1
        cmovne  eax, edx
@s:     mov     esi, eax
        mov     edi, ecx
        shr     esi, 1
        xor     edi, esi
        test    al, 1
        mov     eax, esi
        cmovne  eax, edi
        shr     ebx, 1
        mov     esi, eax
        xor     esi, edx
        test    bl, 1
        cmovne  eax, esi
        dec     ebp
        jne     @s
        pop     ebp
        pop     ebx
        pop     edi
        pop     esi
end;

procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // W=eax Buf=edx
        push    esi
        push    edi
        push    ebx
        mov     esi, eax
        // part 1: W[i]:= RB(TW32Buf(Buf)[i])
        mov     eax, [edx]
        mov     ebx, [edx + 4]
        bswap   eax
        bswap   ebx
        mov     [esi], eax
        mov     [esi + 4], ebx
        mov     eax, [edx + 8]
        mov     ebx, [edx + 12]
        bswap   eax
        bswap   ebx
        mov     [esi + 8], eax
        mov     [esi + 12], ebx
        mov     eax, [edx + 16]
        mov     ebx, [edx + 20]
        bswap   eax
        bswap   ebx
        mov     [esi + 16], eax
        mov     [esi + 20], ebx
        mov     eax, [edx + 24]
        mov     ebx, [edx + 28]
        bswap   eax
        bswap   ebx
        mov     [esi + 24], eax
        mov     [esi + 28], ebx
        mov     eax, [edx + 32]
        mov     ebx, [edx + 36]
        bswap   eax
        bswap   ebx
        mov     [esi + 32], eax
        mov     [esi + 36], ebx
        mov     eax, [edx + 40]
        mov     ebx, [edx + 44]
        bswap   eax
        bswap   ebx
        mov     [esi + 40], eax
        mov     [esi + 44], ebx
        mov     eax, [edx + 48]
        mov     ebx, [edx + 52]
        bswap   eax
        bswap   ebx
        mov     [esi + 48], eax
        mov     [esi + 52], ebx
        mov     eax, [edx + 56]
        mov     ebx, [edx + 60]
        bswap   eax
        bswap   ebx
        mov     [esi + 56], eax
        mov     [esi + 60], ebx
        lea     esi, [esi + 64]
        // part2: w[i]:= lrot_1(w[i-3] xor w[i-8] xor w[i-14] xor w[i-16])
        mov     ecx, 48
@@2:    mov     eax, [esi - 2 * 4]    // w[i-2]
        mov     edi, [esi - 7 * 4]    // w[i-7]
        mov     edx, eax
        mov     ebx, eax              // sig1: rr17 xor rr19 xor srx,10
        ror     eax, 17
        ror     edx, 19
        shr     ebx, 10
        xor     eax, edx
        xor     eax, ebx
        add     edi, eax
        mov     eax, [esi - 15 * 4]   // w[i-15]
        mov     ebx, eax              // sig0: rr7 xor rr18 xor sr3
        mov     edx, eax
        ror     eax, 7
        ror     edx, 18
        shr     ebx, 3
        xor     eax, edx
        xor     eax, ebx
        add     eax, edi
        add     eax, [esi - 16 * 4]   // w[i-16]
        mov     [esi], eax
        add     esi, 4
        dec     ecx
        jnz     @@2
        pop     ebx
        pop     edi
        pop     esi
end;


{$ifdef USEAESNI}

procedure AesNiTrailer; // = TAesAbstractSyn.EncryptTrailer from AES-NI procs
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=TAesContext ecx=len xmm7=CV esi=BufIn edi=BufOut
    call   dword ptr [eax].TAesContext.AesNi32 // = AES.Encrypt(fCV,fCV)
    lea    edx, [eax].TAesContext.buf // used as temporary buffer
    movups [edx], xmm7
    cld
@s: lodsb
    xor    al, [edx] // = XorMemory(pointer(fOut),pointer(fIn),@fCV,len);
    inc    edx
    stosb
    dec    ecx
    jnz    @s
end;

procedure MakeDecrKeyAesNi(Rounds: integer; RK: Pointer);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=Rounds edx=RK
        sub     eax, 9
        movups  xmm0, [edx + $10]
        movups  xmm1, [edx + $20]
        movups  xmm2, [edx + $30]
        movups  xmm3, [edx + $40]
        movups  xmm4, [edx + $50]
        movups  xmm5, [edx + $60]
        movups  xmm6, [edx + $70]
        movups  xmm7, [edx + $80]
        {$ifdef HASAESNI}
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        aesimc  xmm6, xmm6
        aesimc  xmm7, xmm7
        {$else}
        db      $66, $0F, $38, $DB, $C0
        db      $66, $0F, $38, $DB, $C9
        db      $66, $0F, $38, $DB, $D2
        db      $66, $0F, $38, $DB, $DB
        db      $66, $0F, $38, $DB, $E4
        db      $66, $0F, $38, $DB, $ED
        db      $66, $0F, $38, $DB, $F6
        db      $66, $0F, $38, $DB, $FF
        {$endif}
        movups  [edx + $10], xmm0
        movups  [edx + $20], xmm1
        movups  [edx + $30], xmm2
        movups  [edx + $40], xmm3
        movups  [edx + $50], xmm4
        movups  [edx + $60], xmm5
        movups  [edx + $70], xmm6
        movups  [edx + $80], xmm7
        lea     edx, [edx + $90]
@loop:  movups  xmm0, [edx]
        db      $66, $0F, $38, $DB, $C0 // aesimc xmm0,xmm0
        movups  [edx], xmm0
        dec     eax
        lea     edx, [edx + 16]
        jnz     @loop
end;

procedure ShiftAesNi(KeySize: cardinal; pk: pointer);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=KeySize edx=pk
        movups  xmm1, [edx]
        movups  xmm5, dqword ptr[@mask]
        cmp     al, 128
        je      @128
        cmp     al, 192
        je      @e // 192 bits is very complicated -> skip by now (use 128+256)
@256:   movups  xmm3, [edx + 16]
        add     edx, 32
        db      $66, $0F, $3A, $DF, $D3, $01 // aeskeygenassist xmm2,xmm3,1
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $02 // aeskeygenassist xmm2,xmm3,2
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $04 // aeskeygenassist xmm2,xmm3,4
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $08 // aeskeygenassist xmm2,xmm3,8
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $10 // aeskeygenassist xmm2,xmm3,$10
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $20 // aeskeygenassist xmm2,xmm3,$20
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $40 // aeskeygenassist xmm2,xmm3,$40
        pshufd  xmm2, xmm2, $FF
        movups  xmm4, xmm1
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        jmp     @e
@mask:  dd      $ffffffff
        dd      $03020100
        dd      $07060504
        dd      $0b0a0908
@exp256:pshufd  xmm2, xmm2, $ff
        movups  xmm4, xmm1
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        add     edx, $10
        db      $66, $0F, $3A, $DF, $E1, $00 // aeskeygenassist xmm4,xmm1,0
        pshufd  xmm2, xmm4, $AA
        movups  xmm4, xmm3
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        pxor    xmm3, xmm2
        movups  [edx], xmm3
        add     edx, $10
        ret
@exp128:pshufd  xmm2, xmm2, $FF
        movups  xmm3, xmm1
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        add     edx, $10
        ret
@128:   add     edx, 16
        db      $66, $0F, $3A, $DF, $D1, $01 // aeskeygenassist xmm2,xmm1,1
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $02 // aeskeygenassist xmm2,xmm1,2
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $04 // aeskeygenassist xmm2,xmm1,4
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $08 // aeskeygenassist xmm2,xmm1,8
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $10 // aeskeygenassist xmm2,xmm1,$10
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $20 // aeskeygenassist xmm2,xmm1,$20
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $40 // aeskeygenassist xmm2,xmm1,$40
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $80 // aeskeygenassist xmm2,xmm1,$80
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $1b // aeskeygenassist xmm2,xmm1,$1b
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $36 // aeskeygenassist xmm2,xmm1,$36
        call    @exp128
@e:
end;

procedure AesNiEncryptXmm7_128;
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // input: eax=TAesContext, xmm7=data; output: eax=TAesContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenclast xmm7, xmm3
        {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DD, $FB
        {$endif}
end;

procedure aesniencrypt128(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_128
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure AesNiEncryptXmm7_192;
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // input: eax=TAesContext, xmm7=data; output: eax=TAesContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
        movups  xmm4, [eax + 16 * 11]
        movups  xmm5, [eax + 16 * 12]
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenclast xmm7, xmm5
        {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DD, $FD
        {$endif}
end;

procedure aesniencrypt192(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_192
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure AesNiEncryptXmm7_256;
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // input: eax=TAesContext, xmm7=data; output: eax=TAesContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
        movups  xmm4, [eax + 16 * 11]
        movups  xmm5, [eax + 16 * 12]
        movups  xmm6, [eax + 16 * 13]
        {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
        {$endif}
        movups  xmm1, [eax + 16 * 14]
        {$ifdef HASAESNI}
        aesenclast xmm7, xmm1
        {$else}
        db      $66, $0F, $38, $DD, $F9
        {$endif}
end;

procedure aesniencrypt256(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_256
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;

procedure aesnidecrypt128(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 10]
        movups  xmm1, [eax + 16 * 9]
        movups  xmm2, [eax + 16 * 8]
        movups  xmm3, [eax + 16 * 7]
        movups  xmm4, [eax + 16 * 6]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 4]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        {$endif}
        movups  xmm0, [eax + 16 * 3]
        movups  xmm1, [eax + 16 * 2]
        movups  xmm2, [eax + 16 * 1]
        movups  xmm3, [eax + 16 * 0]
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdeclast xmm7, xmm3
        {$else}
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DF, $FB
        {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;

procedure aesnidecrypt192(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 12]
        movups  xmm1, [eax + 16 * 11]
        movups  xmm2, [eax + 16 * 10]
        movups  xmm3, [eax + 16 * 9]
        movups  xmm4, [eax + 16 * 8]
        movups  xmm5, [eax + 16 * 7]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 5]
        movups  xmm1, [eax + 16 * 4]
        movups  xmm2, [eax + 16 * 3]
        movups  xmm3, [eax + 16 * 2]
        movups  xmm4, [eax + 16 * 1]
        movups  xmm5, [eax + 16 * 0]
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdeclast xmm7, xmm5
        {$else}
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DF, $FD
        {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;

procedure aesnidecrypt256(const ctxt, source, dest);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 14]
        movups  xmm1, [eax + 16 * 13]
        movups  xmm2, [eax + 16 * 12]
        movups  xmm3, [eax + 16 * 11]
        movups  xmm4, [eax + 16 * 10]
        movups  xmm5, [eax + 16 * 9]
        movups  xmm6, [eax + 16 * 8]
        pxor    xmm7, xmm0
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 6]
        movups  xmm2, [eax + 16 * 5]
        movups  xmm3, [eax + 16 * 4]
        movups  xmm4, [eax + 16 * 3]
        movups  xmm5, [eax + 16 * 2]
        movups  xmm6, [eax + 16 * 1]
        {$ifdef HASAESNI}
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        {$else}
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
        {$endif}
        movups  xmm0, [eax + 16 * 0]
        {$ifdef HASAESNI}
        aesdeclast xmm7, xmm0
        {$else}
        db      $66, $0F, $38, $DF, $F8
        {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;

{$ifdef HASAESNI} // oldest Delphi don't handle some opcode used below

// compute a := a * b in GF(2^128) using pclmulqdq on WestMere CPUs
// - two times faster than the pascal version using lookup tables
procedure gf_mul_pclmulqdq(a, b: pointer);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        movups  xmm0, dqword ptr[a]
        movups  xmm1, dqword ptr[b]
        movdqu  xmm2, dqword ptr[@swap]
        pshufb  xmm0, xmm2
        pshufb  xmm1, xmm2
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
        movdqu  xmm7, dqword ptr[@swap]
        por     xmm0, xmm3
        psllq   xmm5, 62
        pxor    xmm1, xmm0
        movdqa  xmm0, xmm4
        psrlq   xmm6, 2
        por     xmm5, xmm6
        pxor    xmm0, xmm5
        pxor    xmm0, xmm1
        pxor    xmm0, xmm2
        pshufb  xmm0, xmm7
        movups  dqword ptr[a], xmm0
        ret
@swap:  db 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
end;

{$endif HASAESNI}

{$endif USEAESNI}

function Adler32Asm(Adler: cardinal; p: pointer; Count: integer): cardinal;
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        push    esi
        push    edi
        mov     edi, eax
        shr     edi, 16
        movzx   ebx, ax
        push    ebp
        mov     esi, edx
        test    esi, esi
        mov     ebp, ecx
        jne     @31
        mov     eax, 1
        jmp     @32
@31:    test    ebp, ebp
        jbe     @34
@33:    cmp     ebp, 5552
        jae     @35
        mov     eax, ebp
        jmp     @36
@35:    mov     eax, 5552
@36:    sub     ebp, eax
        cmp     eax, 16
        jl      @38
        xor     edx, edx
        xor     ecx, ecx
@39:    sub     eax, 16
        mov     dl, [esi]
        mov     cl, [esi + 1]
        add     ebx, edx
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 2]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 3]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 4]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 5]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 6]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 7]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 8]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 9]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 10]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 11]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 12]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 13]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 14]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 15]
        add     edi, ebx
        add     ebx, ecx
        add     esi, 16
        lea     edi, [edi + ebx]
        cmp     eax, 16
        jge     @39
@38:    test    eax, eax
        je      @42
@43:    movzx   edx, byte ptr[esi]
        add     ebx, edx
        dec     eax
        lea     esi, [esi + 1]
        lea     edi, [edi + ebx]
        jg      @43
@42:    mov     ecx, 65521
        mov     eax, ebx
        xor     edx, edx
        div     ecx
        mov     ebx, edx
        mov     ecx, 65521
        mov     eax, edi
        xor     edx, edx
        div     ecx
        test    ebp, ebp
        mov     edi, edx
        ja      @33
@34:    mov     eax, edi
        shl     eax, 16
        or      eax, ebx
@32:    pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

{
   MD5_386.Asm   -  386 optimized helper routine for calculating
                    MD Message-Digest values
   written 2/2/94 by Peter Sawatzki
   Buchenhof 3, D58091 Hagen, Germany Fed Rep
   Peter@Sawatzki.de http://www.sawatzki.de

   original C Source was found in Dr. Dobbs Journal Sep 91
   MD5 algorithm from RSA Data Security, Inc.
   Taken from https://github.com/maximmasiutin/MD5_Transform-x64
}
procedure MD5Transform(var buf: TMd5Buf; const in_: TMd5In);
  {$ifdef FPC}nostackframe; assembler;{$endif}
// see https://synopse.info/forum/viewtopic.php?id=4369 for asm numbers
asm // eax=buf:TMd5Buf edx=in_:TMd5In
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebp, edx
        push    eax
        mov     edx, dword ptr [eax+0CH]
        mov     ecx, dword ptr [eax+8H]
        mov     ebx, dword ptr [eax+4H]
        mov     eax, dword ptr [eax]
        add     eax, dword ptr [ebp]
        add     eax, -680876936
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+4H]
        add     edx, -389564586
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+8H]
        add     ecx, 606105819
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+0CH]
        add     ebx, -1044525330
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+10H]
        add     eax, -176418897
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+14H]
        add     edx, 1200080426
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+18H]
        add     ecx, -1473231341
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+1CH]
        add     ebx, -45705983
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+20H]
        add     eax, 1770035416
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+24H]
        add     edx, -1958414417
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+28H]
        add     ecx, -42063
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+2CH]
        add     ebx, -1990404162
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+30H]
        add     eax, 1804603682
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+34H]
        add     edx, -40341101
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+38H]
        add     ecx, -1502002290
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+3CH]
        add     ebx, 1236535329
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+4H]
        add     eax, -165796510
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+18H]
        add     edx, -1069501632
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+2CH]
        add     ecx, 643717713
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp]
        add     ebx, -373897302
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+14H]
        add     eax, -701558691
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+28H]
        add     edx, 38016083
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+3CH]
        add     ecx, -660478335
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+10H]
        add     ebx, -405537848
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+24H]
        add     eax, 568446438
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+38H]
        add     edx, -1019803690
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+0CH]
        add     ecx, -187363961
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+20H]
        add     ebx, 1163531501
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+34H]
        add     eax, -1444681467
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+8H]
        add     edx, -51403784
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+1CH]
        add     ecx, 1735328473
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+30H]
        add     ebx, -1926607734
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+14H]
        add     eax, -378558
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+20H]
        add     edx, -2022574463
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+2CH]
        add     ecx, 1839030562
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+38H]
        add     ebx, -35309556
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+4H]
        add     eax, -1530992060
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+10H]
        add     edx, 1272893353
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+1CH]
        add     ecx, -155497632
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+28H]
        add     ebx, -1094730640
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+34H]
        add     eax, 681279174
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp]
        add     edx, -358537222
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+0CH]
        add     ecx, -722521979
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+18H]
        add     ebx, 76029189
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+24H]
        add     eax, -640364487
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+30H]
        add     edx, -421815835
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+3CH]
        add     ecx, 530742520
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+8H]
        add     ebx, -995338651
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp]
        add     eax, -198630844
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+1CH]
        add     edx, 1126891415
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+38H]
        add     ecx, -1416354905
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+14H]
        add     ebx, -57434055
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+30H]
        add     eax, 1700485571
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+0CH]
        add     edx, -1894986606
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+28H]
        add     ecx, -1051523
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+4H]
        add     ebx, -2054922799
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+20H]
        add     eax, 1873313359
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+3CH]
        add     edx, -30611744
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+18H]
        add     ecx, -1560198380
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+34H]
        add     ebx, 1309151649
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+10H]
        add     eax, -145523070
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+2CH]
        add     edx, -1120210379
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+8H]
        add     ecx, 718787259
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+24H]
        add     ebx, -343485551
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        pop     esi
        add     dword ptr [esi], eax
        add     dword ptr [esi+4H], ebx
        add     dword ptr [esi+8H], ecx
        add     dword ptr [esi+0CH], edx
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;


{$ifdef SHA512_X86} // optimized asm using SSE3 instructions for x86 32-bit

{$ifdef MSWINDOWS}
  {$ifdef FPC}
    {$L ..\..\static\i386-win32\sha512-x86.o}
  {$else}
    {$L ..\..\static\delphi\sha512-x86.obj}
  {$endif FPC}
{$else}
  {$L ..\..\static\i386-linux\sha512-x86.o}
{$endif MSWINDOWS}

{
  SHA-512 hash in x86 assembly
  Copyright (c) 2014 Project Nayuki. (MIT License)
  https://www.nayuki.io/page/fast-sha2-hashes-in-x86-assembly

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:
  - The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  - The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall the
  authors or copyright holders be liable for any claim, damages or other liability,
  whether in an action of contract, tort or otherwise, arising from, out of or
  in connection with the Software or the use or other dealings in the Software.
}
procedure sha512_compress(state: PQWord; block: PByteArray); cdecl; external;

{$endif SHA512_X86}

