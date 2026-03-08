default rel

section .data

section .bss
    cmdline_buf resb 4096
    argv_buf resq 128

section .text
    global mainCRTStartup
    extern GetCommandLineA
    extern cli_main
    extern ExitProcess

mainCRTStartup:
    sub rsp, 32

    call GetCommandLineA
    mov rsi, rax
    lea rdi, [cmdline_buf]
    mov rcx, 4095
.copy_cmdline:
    test rcx, rcx
    jz .cmdline_done
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    dec rcx
    test al, al
    jnz .copy_cmdline
.cmdline_done:
    mov byte [rdi], 0

    lea rdi, [cmdline_buf]
    lea rsi, [argv_buf]
    call parse_argv_ansi

    mov rdi, rax
    lea rsi, [argv_buf]
    call cli_main

    xor rcx, rcx
    jmp ExitProcess

parse_argv_ansi:
    xor rax, rax

.skip_ws:
    mov dl, byte [rdi]
    test dl, dl
    jz .done
    cmp dl, ' '
    je .ws_advance
    cmp dl, 9
    je .ws_advance
    jmp .start_token

.ws_advance:
    inc rdi
    jmp .skip_ws

.start_token:
    cmp rax, 128
    jae .done
    mov qword [rsi + rax * 8], rdi
    inc rax

    cmp dl, '"'
    jne .normal_token
    inc rdi
    mov qword [rsi + (rax - 1) * 8], rdi

.quoted_loop:
    mov dl, byte [rdi]
    test dl, dl
    jz .done
    cmp dl, '"'
    je .quoted_end
    inc rdi
    jmp .quoted_loop

.quoted_end:
    mov byte [rdi], 0
    inc rdi
    jmp .skip_ws

.normal_token:
.normal_loop:
    mov dl, byte [rdi]
    test dl, dl
    jz .done
    cmp dl, ' '
    je .normal_end
    cmp dl, 9
    je .normal_end
    inc rdi
    jmp .normal_loop

.normal_end:
    mov byte [rdi], 0
    inc rdi
    jmp .skip_ws

.done:
    ret
