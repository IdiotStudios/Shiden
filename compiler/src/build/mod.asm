default rel

section .data
    build_msg db "Build module stub", 0xA
    build_msg_len equ $ - build_msg

section .text
    global build_compile

build_compile:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [build_msg]
    mov rdx, build_msg_len
    syscall
    ret
