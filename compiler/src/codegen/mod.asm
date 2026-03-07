default rel

section .data
    msg_codegen db "[info] Generating code...", 0xA
    msg_codegen_len equ $ - msg_codegen
    
section .bss
    code_buffer resb 65536
    code_ptr resq 1
    data_buffer resb 65536
    data_ptr resq 1
    data_offset resq 1
    string_count resq 1
    
section .text
    global codegen_init
    global codegen_emit_prologue
    global codegen_emit_epilogue
    global codegen_emit_println
    global codegen_add_string
    global codegen_get_code
    global codegen_get_code_size
    global codegen_get_data
    global codegen_get_data_size
    extern rt_print

codegen_init:
    lea rax, [code_buffer]
    mov [code_ptr], rax
    lea rax, [data_buffer]
    mov [data_ptr], rax
    mov qword [data_offset], 0x401000
    mov qword [string_count], 0
    
    lea rsi, [msg_codegen]
    mov rdx, msg_codegen_len
    call rt_print
    ret

codegen_emit_prologue:
    mov r10, [code_ptr]
    
    mov byte [r10], 0x55
    inc r10
    
    mov byte [r10], 0x48
    mov byte [r10+1], 0x89
    mov byte [r10+2], 0xE5
    add r10, 3
    
    mov [code_ptr], r10
    ret

codegen_emit_epilogue:
    mov r10, [code_ptr]
    
    mov byte [r10], 0x48
    mov byte [r10+1], 0x31
    mov byte [r10+2], 0xFF
    add r10, 3
    
    mov byte [r10], 0xB8
    mov dword [r10+1], 60
    add r10, 5
    
    mov byte [r10], 0x0F
    mov byte [r10+1], 0x05
    add r10, 2
    
    mov [code_ptr], r10
    ret

codegen_add_string:
    push rbx
    push r12
    push r13
    
    mov rbx, rdi
    mov r12, rsi
    
    mov r13, [data_ptr]
    
    mov rdi, r13
    mov rsi, rbx
    mov rdx, r12
    call .memcpy_inline
    
    mov rax, [data_offset]
    
    add r13, r12
    mov byte [r13], 0x0A
    inc r13
    mov [data_ptr], r13
    
    mov rcx, [data_offset]
    add rcx, r12
    inc rcx
    mov [data_offset], rcx
    
    mov rdx, r12
    inc rdx
    
    pop r13
    pop r12
    pop rbx
    ret

.memcpy_inline:
    test rdx, rdx
    jz .done
.loop:
    mov al, [rsi]
    mov [rdi], al
    inc rsi
    inc rdi
    dec rdx
    jnz .loop
.done:
    ret

codegen_emit_println:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, rsi
    mov r10, [code_ptr]

    ; mov eax, 1
    mov byte [r10], 0xB8
    mov dword [r10 + 1], 1
    add r10, 5

    ; mov edi, 1
    mov byte [r10], 0xBF
    mov dword [r10 + 1], 1
    add r10, 5

    ; lea rsi, [rip + 12]
    mov byte [r10], 0x48
    mov byte [r10 + 1], 0x8D
    mov byte [r10 + 2], 0x35
    mov dword [r10 + 3], 12
    add r10, 7

    ; mov edx, len + 1 (newline)
    mov byte [r10], 0xBA
    mov ebx, r13d
    inc ebx
    mov dword [r10 + 1], ebx
    add r10, 5

    ; syscall
    mov byte [r10], 0x0F
    mov byte [r10 + 1], 0x05
    add r10, 2

    ; jmp over inlined string bytes
    mov byte [r10], 0xE9
    mov ebx, r13d
    inc ebx
    mov dword [r10 + 1], ebx
    add r10, 5

    mov r14, r10
    xor rbx, rbx
.println_copy_loop:
    cmp rbx, r13
    jae .println_copy_done
    mov al, [r12 + rbx]
    mov [r14 + rbx], al
    inc rbx
    jmp .println_copy_loop
.println_copy_done:
    add r14, r13
    mov byte [r14], 0x0A
    inc r14

    mov [code_ptr], r14

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

codegen_get_code:
    lea rax, [code_buffer]
    ret

codegen_get_code_size:
    lea rax, [code_buffer]
    mov rbx, [code_ptr]
    sub rbx, rax
    mov rax, rbx
    ret

codegen_get_data:
    lea rax, [data_buffer]
    ret

codegen_get_data_size:
    lea rax, [data_buffer]
    mov rbx, [data_ptr]
    sub rbx, rax
    mov rax, rbx
    ret
