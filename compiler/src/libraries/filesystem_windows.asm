default rel

section .data
    fs_msg db "Filesystem helpers ready", 0xA
    fs_msg_len equ $ - fs_msg

section .bss
    alignb 8
    fs_read_buffer resb 131072
    alignb 4
    fs_bytes_read resd 1

section .text
    global filesystem_init
    global filesystem_read_file
    extern rt_print
    extern CreateFileA
    extern ReadFile
    extern CloseHandle

filesystem_init:
    lea rsi, [fs_msg]
    mov rdx, fs_msg_len
    call rt_print
    ret

filesystem_read_file:
    push rbx
    push r12
    push r13
    push r14
    mov r14, rsp
    and rsp, -16
    sub rsp, 64
    
    mov r12, rdi
    
    mov rcx, r12
    mov rdx, 0x80000000
    mov r8, 3
    xor r9, r9
    mov qword [rsp + 32], 3
    mov qword [rsp + 40], 0
    mov qword [rsp + 48], 0
    call CreateFileA
    
    cmp rax, -1
    je .read_fail
    
    mov r13, rax
    
    mov rcx, r13
    lea rdx, [rel fs_read_buffer]
    mov r8, 131071
    lea r9, [rel fs_bytes_read]
    mov dword [rel fs_bytes_read], 0
    mov qword [rsp + 32], 0
    call ReadFile
    
    test rax, rax
    jz .close_fail
    
    mov r9d, dword [rel fs_bytes_read]
    test r9d, r9d
    jz .close_fail
    
    lea r10, [rel fs_read_buffer]
    mov byte [r10 + r9], 0
    
    push r9
    mov rcx, r13
    call CloseHandle
    pop r9
    
    lea rax, [rel fs_read_buffer]
    mov rdx, r9
    mov rsp, r14
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.close_fail:
    mov rcx, r13
    call CloseHandle

.read_fail:
    xor rax, rax
    xor rdx, rdx
    mov rsp, r14
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
