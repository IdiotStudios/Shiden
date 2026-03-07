default rel

section .data
    net_msg db "Networking helpers stub", 0xA
    net_msg_len equ $ - net_msg

section .text
    global networking_init

networking_init:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [net_msg]
    mov rdx, net_msg_len
    syscall
    ret
