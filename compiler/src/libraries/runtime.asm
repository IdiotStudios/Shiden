default rel

section .data
    runtime_msg db "Runtime helpers stub", 0xA
    runtime_msg_len equ $ - runtime_msg

section .text
    global runtime_init

runtime_init:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [runtime_msg]
    mov rdx, runtime_msg_len
    syscall
    ret
