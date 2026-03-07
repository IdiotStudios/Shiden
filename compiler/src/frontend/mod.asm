default rel

section .data
    parse_msg db "Frontend parser stub", 0xA
    parse_msg_len equ $ - parse_msg

section .text
    global frontend_parse

frontend_parse:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [parse_msg]
    mov rdx, parse_msg_len
    syscall
    ret
