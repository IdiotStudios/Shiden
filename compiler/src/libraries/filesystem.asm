default rel

section .data
    fs_msg db "Filesystem helpers ready", 0xA
    fs_msg_len equ $ - fs_msg

section .bss
    fs_read_buffer resb 131072

section .text
    global filesystem_init
    global filesystem_read_file
    extern rt_print

filesystem_init:
    lea rsi, [fs_msg]
    mov rdx, fs_msg_len
    call rt_print
    ret

filesystem_read_file:
    push rbx
    mov rbx, rdi

    mov rax, 2
    mov rdi, rbx
    xor rsi, rsi
    xor rdx, rdx
    syscall
    test rax, rax
    js .read_fail

    mov r8, rax
    mov rax, 0
    mov rdi, r8
    lea rsi, [fs_read_buffer]
    mov rdx, 131071
    syscall
    test rax, rax
    js .close_fail

    mov r9, rax
    lea r10, [rel fs_read_buffer]
    mov byte [r10 + r9], 0

    mov rax, 3
    mov rdi, r8
    syscall

    lea rax, [rel fs_read_buffer]
    mov rdx, r9
    pop rbx
    ret

.close_fail:
    mov rax, 3
    mov rdi, r8
    syscall

.read_fail:
    xor rax, rax
    xor rdx, rdx
    pop rbx
    ret
