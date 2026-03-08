default rel

section .data

section .bss
    alignb 4
    rt_bytes_written resd 1

section .text
    global runtime_init
    global rt_write
    global rt_print
    global rt_strlen
    global rt_streq
    global rt_memcpy
    global rt_exit
    extern GetStdHandle
    extern WriteFile
    extern ExitProcess

runtime_init:
    xor rax, rax
    ret

rt_write:
    push rbx
    push r12
    push r14

    mov r12, rsi
    mov rbx, rdx

    mov r14, rsp
    and rsp, -16
    sub rsp, 64

    mov rcx, rdi
    call GetStdHandle

    mov rcx, rax
    mov rdx, r12
    mov r8, rbx
    lea r9, [rel rt_bytes_written]
    mov dword [rel rt_bytes_written], 0
    mov qword [rsp + 32], 0
    call WriteFile

    mov rsp, r14
    pop r14
    pop r12
    pop rbx
    ret

rt_print:
    mov rdi, -11
    jmp rt_write

rt_strlen:
    xor rax, rax
.len_loop:
    cmp byte [rdi + rax], 0
    je .len_done
    inc rax
    jmp .len_loop
.len_done:
    ret

rt_streq:
    xor rax, rax
.cmp_loop:
    mov dl, [rdi]
    mov cl, [rsi]
    cmp dl, cl
    jne .cmp_not_equal
    test dl, dl
    je .cmp_equal
    inc rdi
    inc rsi
    jmp .cmp_loop
.cmp_equal:
    xor rax, rax
    ret
.cmp_not_equal:
    mov rax, 1
    ret

rt_memcpy:
    mov rax, rdi
    test rdx, rdx
    jz .copy_done
.copy_loop:
    mov r8b, [rsi]
    mov [rdi], r8b
    inc rsi
    inc rdi
    dec rdx
    jnz .copy_loop
.copy_done:
    ret

rt_exit:
    xor rcx, rcx
    jmp ExitProcess
