section .data

section .bss

section .text
    global _start

_start:
    call main
    mov rax, 60    ; exit
    xor rdi, rdi   ; status 0
    syscall

main:
    call cli_main
    mov rax, 60    ; exit
    xor rdi, rdi   ; status 0
    syscall

extern cli_main