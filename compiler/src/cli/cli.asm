default rel

section .data
    prompt db "Shiden Compiler CLI", 0xA
    prompt_len equ $ - prompt
    usage db "Usage: shiden [options]", 0xA
    usage_len equ $ - usage

section .bss

section .text
    global cli_main

cli_main:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [prompt]
    mov rdx, prompt_len
    syscall

    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [usage]
    mov rdx, usage_len
    syscall

    ret