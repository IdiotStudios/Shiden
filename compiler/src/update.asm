default rel

section .data
    update_msg db "Update checker stub", 0xA
    update_msg_len equ $ - update_msg

section .text
    global update_check

update_check:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [update_msg]
    mov rdx, update_msg_len
    syscall
    ret
