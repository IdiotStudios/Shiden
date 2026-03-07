section .data

section .bss

section .text
    global _start

_start:
    pop rdi           ; argc
    mov rsi, rsp      ; argv
    call main
    mov rax, 60       ; exit
    xor rdi, rdi      ; status 0
    syscall

main:
    call cli_main
    mov rax, 60       ; exit
    xor rdi, rdi      ; status 0
    syscall

extern cli_main