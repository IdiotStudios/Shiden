default rel

section .data
    lexer_msg db "Syntax lexer stub", 0xA
    lexer_msg_len equ $ - lexer_msg

section .text
    global syntax_lex

syntax_lex:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [lexer_msg]
    mov rdx, lexer_msg_len
    syscall
    ret
