default rel

section .data
    fs_msg db "Filesystem helpers stub", 0xA
    fs_msg_len equ $ - fs_msg

section .text
    global filesystem_init

filesystem_init:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [fs_msg]
    mov rdx, fs_msg_len
    syscall
    ret
