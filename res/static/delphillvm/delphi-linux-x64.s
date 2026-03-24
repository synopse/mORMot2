.intel_syntax noprefix
.text

.global x64fakestub
.global x64callmethod

# redirect to TInterfacedObjectFakeRaw.FakeCall
.extern fakecall

x64fakestub:
    push rbp
    mov rbp, rsp
    sub rsp, 0x80

    mov [rbp-8], rax
    movlpd [rbp-0x48], xmm0
    movlpd [rbp-0x40], xmm1
    movlpd [rbp-0x38], xmm2
    movlpd [rbp-0x30], xmm3
    movlpd [rbp-0x28], xmm4
    movlpd [rbp-0x20], xmm5
    movlpd [rbp-0x18], xmm6
    movlpd [rbp-0x10], xmm7

    mov [rbp-0x50], r9
    mov [rbp-0x58], r8
    mov [rbp-0x60], rcx
    mov [rbp-0x68], rdx
    mov [rbp-0x70], rsi
    mov [rbp-0x78], rdi

    lea rsi, [rbp-0x78]

    call fakecall

    movsd xmm0, [rbp-0x48]

    mov rsp, rbp
    pop rbp
    ret

x64callmethod:
    push rbp
    push r12
    mov rbp, rsp
    sub rsp, 0x100
    and rsp, -16

    mov r12, rdi

    mov rcx, [r12]
    mov rdx, [r12+8]

    test ecx, ecx
    jz .Ldone_push

.Lpush_loop:
    push qword ptr [rdx]
    sub rdx, 8
    sub ecx, 1
    jnz .Lpush_loop

.Ldone_push:
    mov rdi, [r12+0x18]
    mov rsi, [r12+0x20]
    mov rdx, [r12+0x28]
    mov rcx, [r12+0x30]
    mov r8,  [r12+0x38]
    mov r9,  [r12+0x40]

    movsd xmm0, [r12+0x48]
    movsd xmm1, [r12+0x50]
    movsd xmm2, [r12+0x58]
    movsd xmm3, [r12+0x60]
    movsd xmm4, [r12+0x68]
    movsd xmm5, [r12+0x70]
    movsd xmm6, [r12+0x78]
    movsd xmm7, [r12+0x80]

    call qword ptr [r12+0x10]

    mov [r12+0x88], rax

    mov cl, byte ptr [r12+0x90]
    cmp cl, 8
    je .Lstore_xmm
    cmp cl, 9
    je .Lstore_xmm
    cmp cl, 10
    jne .Lend

.Lstore_xmm:
    movlpd [r12+0x88], xmm0

.Lend:
    mov rsp, rbp
    pop r12
    pop rbp
    ret
