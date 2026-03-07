default rel

section .data
    banner db "Shiden Compiler v0.1.3-asm", 0xA
    banner_len equ $ - banner
    
    cmd_run db "run", 0
    cmd_check db "check", 0
    cmd_new db "new", 0
    cmd_init db "init", 0
    cmd_compile db "compile", 0
    cmd_help db "help", 0
    cmd_update db "update", 0
    
    msg_run db "[stub] Running Shiden code...", 0xA
    msg_run_len equ $ - msg_run
    msg_check db "[stub] Checking Shiden code...", 0xA
    msg_check_len equ $ - msg_check
    msg_new db "[stub] Creating new project...", 0xA
    msg_new_len equ $ - msg_new
    msg_init db "[stub] Initializing project...", 0xA
    msg_init_len equ $ - msg_init
    msg_compile db "[stub] Compiling project...", 0xA
    msg_compile_len equ $ - msg_compile
    msg_update db "[stub] Checking for updates...", 0xA
    msg_update_len equ $ - msg_update
    
    help_text db "Usage: shiden <command> [options]", 0xA, 0xA
    help_text2 db "Commands:", 0xA
    help_text3 db "  run       Compile and run (--debug for debug mode)", 0xA
    help_text4 db "  check     Validate syntax and check errors", 0xA
    help_text5 db "  new       Create a new project", 0xA
    help_text6 db "  init      Initialize project in current directory", 0xA
    help_text7 db "  compile   Compile the project", 0xA
    help_text8 db "  help      Show this help message", 0xA
    help_text9 db "  update    Check/install updates", 0xA
    help_len1 equ help_text2 - help_text
    help_len2 equ help_text3 - help_text2
    help_len3 equ help_text4 - help_text3
    help_len4 equ help_text5 - help_text4
    help_len5 equ help_text6 - help_text5
    help_len6 equ help_text7 - help_text6
    help_len7 equ help_text8 - help_text7
    help_len8 equ help_text9 - help_text8
    help_len9 equ $ - help_text9

section .bss

section .text
    global cli_main
    extern update_check
    extern frontend_parse
    extern syntax_lex
    extern build_compile

cli_main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    
    mov rbx, rdi      ; argc
    mov r12, rsi      ; argv
    
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    lea rsi, [banner]
    mov rdx, banner_len
    syscall
    
    cmp rbx, 1
    jle .show_help
    
    mov r13, [r12 + 8]    ; argv[1] - command
    
    lea rdi, [r13]
    lea rsi, [cmd_run]
    call strcmp
    test rax, rax
    jz .do_run
    
    lea rdi, [r13]
    lea rsi, [cmd_check]
    call strcmp
    test rax, rax
    jz .do_check
    
    lea rdi, [r13]
    lea rsi, [cmd_new]
    call strcmp
    test rax, rax
    jz .do_new
    
    lea rdi, [r13]
    lea rsi, [cmd_init]
    call strcmp
    test rax, rax
    jz .do_init
    
    lea rdi, [r13]
    lea rsi, [cmd_compile]
    call strcmp
    test rax, rax
    jz .do_compile
    
    lea rdi, [r13]
    lea rsi, [cmd_help]
    call strcmp
    test rax, rax
    jz .show_help
    
    lea rdi, [r13]
    lea rsi, [cmd_update]
    call strcmp
    test rax, rax
    jz .do_update

.show_help:
    lea rsi, [help_text]
    mov rdx, help_len1
    call print
    lea rsi, [help_text2]
    mov rdx, help_len2
    call print
    lea rsi, [help_text3]
    mov rdx, help_len3
    call print
    lea rsi, [help_text4]
    mov rdx, help_len4
    call print
    lea rsi, [help_text5]
    mov rdx, help_len5
    call print
    lea rsi, [help_text6]
    mov rdx, help_len6
    call print
    lea rsi, [help_text7]
    mov rdx, help_len7
    call print
    lea rsi, [help_text8]
    mov rdx, help_len8
    call print
    lea rsi, [help_text9]
    mov rdx, help_len9
    call print
    jmp .done

.do_run:
    lea rsi, [msg_run]
    mov rdx, msg_run_len
    call print
    call build_compile
    jmp .done

.do_check:
    lea rsi, [msg_check]
    mov rdx, msg_check_len
    call print
    call syntax_lex
    jmp .done

.do_new:
    lea rsi, [msg_new]
    mov rdx, msg_new_len
    call print
    jmp .done

.do_init:
    lea rsi, [msg_init]
    mov rdx, msg_init_len
    call print
    jmp .done

.do_compile:
    lea rsi, [msg_compile]
    mov rdx, msg_compile_len
    call print
    call build_compile
    jmp .done

.do_update:
    lea rsi, [msg_update]
    mov rdx, msg_update_len
    call print
    call update_check
    jmp .done

.done:
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

print:
    mov rax, 1            ; write
    mov rdi, 1            ; stdout
    syscall
    ret

strcmp:
    xor rax, rax
.loop:
    mov al, [rdi]
    mov cl, [rsi]
    cmp al, cl
    jne .not_equal
    test al, al
    jz .equal
    inc rdi
    inc rsi
    jmp .loop
.equal:
    xor rax, rax
    ret
.not_equal:
    mov rax, 1
    ret