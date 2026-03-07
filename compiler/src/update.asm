default rel

section .data
    update_msg db "Update checker stub", 0xA
    update_msg_len equ $ - update_msg

section .text
    global update_check
    extern rt_print

update_check:
    lea rsi, [update_msg]
    mov rdx, update_msg_len
    call rt_print
    ret
