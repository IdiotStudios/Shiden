default rel

section .data
    net_msg db "Networking helpers stub", 0xA
    net_msg_len equ $ - net_msg

section .text
    global networking_init
    extern rt_print

networking_init:
    lea rsi, [net_msg]
    mov rdx, net_msg_len
    call rt_print
    ret
