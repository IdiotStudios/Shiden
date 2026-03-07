default rel

section .data
    lib_msg db "Libraries module stub", 0xA
    lib_msg_len equ $ - lib_msg

section .text
    global libraries_init

libraries_init:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [lib_msg]
    mov rdx, lib_msg_len
    syscall
    ret
