default rel

section .data
    lib_msg db "Libraries module stub", 0xA
    lib_msg_len equ $ - lib_msg

section .text
    global libraries_init
    extern rt_print

libraries_init:
    lea rsi, [lib_msg]
    mov rdx, lib_msg_len
    call rt_print
    ret
