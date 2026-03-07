default rel

section .data

section .text
    global runtime_init
    global rt_write
    global rt_print
    global rt_strlen
    global rt_streq
    global rt_memcpy
    global rt_exit

runtime_init:
    xor rax, rax
    ret

rt_write:
    mov rax, 1
    syscall
    ret

rt_print:
    mov rdi, 1
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
    mov rax, 60
    syscall
    hlt
