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
    msg_check_ok db "[ok] Lex pass", 0xA
    msg_check_ok_len equ $ - msg_check_ok
    msg_parse_ok db "[ok] Parse pass", 0xA
    msg_parse_ok_len equ $ - msg_parse_ok
    msg_check_fail db "[error] Lex failed", 0xA
    msg_check_fail_len equ $ - msg_check_fail
    msg_parse_fail db "[error] Parse failed", 0xA
    msg_parse_fail_len equ $ - msg_parse_fail
    msg_check_read_fail db "[error] Could not read source file", 0xA
    msg_check_read_fail_len equ $ - msg_check_read_fail
    msg_new db "[stub] Creating new project...", 0xA
    msg_new_len equ $ - msg_new
    msg_init db "[stub] Initializing project...", 0xA
    msg_init_len equ $ - msg_init
    msg_compile db "[stub] Compiling project...", 0xA
    msg_compile_len equ $ - msg_compile
    msg_run_chdir_fail db "[error] Could not enter run directory", 0xA
    msg_run_chdir_fail_len equ $ - msg_run_chdir_fail
    msg_compile_chdir_fail db "[error] Could not enter compile directory", 0xA
    msg_compile_chdir_fail_len equ $ - msg_compile_chdir_fail
    msg_update db "[stub] Checking for updates...", 0xA
    msg_update_len equ $ - msg_update

    default_check_path db "examples/docs/src/main.sd", 0
    
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
    extern runtime_init
    extern rt_print
    extern rt_streq
    extern update_check
    extern frontend_parse
    extern frontend_parse_tokens
    extern syntax_lex
    extern syntax_lex_buffer
    extern build_compile
    extern build_run
    extern filesystem_read_file
    extern config_load_ini
    extern config_get_project_name
    extern config_get_targets
    extern config_get_opt_level
    extern config_get_debug

cli_main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    
    mov rbx, rdi      ; argc
    mov r12, rsi      ; argv

    call runtime_init
    
    lea rsi, [banner]
    mov rdx, banner_len
    call rt_print
    
    cmp rbx, 1
    jle .show_help
    
    mov r13, [r12 + 8]    ; argv[1] - command
    
    mov rdi, r13
    lea rsi, [cmd_run]
    call rt_streq
    test rax, rax
    jz .do_run
    
    mov rdi, r13
    lea rsi, [cmd_check]
    call rt_streq
    test rax, rax
    jz .do_check
    
    mov rdi, r13
    lea rsi, [cmd_new]
    call rt_streq
    test rax, rax
    jz .do_new
    
    mov rdi, r13
    lea rsi, [cmd_init]
    call rt_streq
    test rax, rax
    jz .do_init
    
    mov rdi, r13
    lea rsi, [cmd_compile]
    call rt_streq
    test rax, rax
    jz .do_compile
    
    mov rdi, r13
    lea rsi, [cmd_help]
    call rt_streq
    test rax, rax
    jz .show_help
    
    mov rdi, r13
    lea rsi, [cmd_update]
    call rt_streq
    test rax, rax
    jz .do_update

.show_help:
    lea rsi, [help_text]
    mov rdx, help_len1
    call rt_print
    lea rsi, [help_text2]
    mov rdx, help_len2
    call rt_print
    lea rsi, [help_text3]
    mov rdx, help_len3
    call rt_print
    lea rsi, [help_text4]
    mov rdx, help_len4
    call rt_print
    lea rsi, [help_text5]
    mov rdx, help_len5
    call rt_print
    lea rsi, [help_text6]
    mov rdx, help_len6
    call rt_print
    lea rsi, [help_text7]
    mov rdx, help_len7
    call rt_print
    lea rsi, [help_text8]
    mov rdx, help_len8
    call rt_print
    lea rsi, [help_text9]
    mov rdx, help_len9
    call rt_print
    jmp .done

.do_run:
    cmp rbx, 2
    jle .run_print

    mov rax, 80
    mov rdi, [r12 + 16]
    syscall
    test rax, rax
    js .run_chdir_fail

.run_print:
    lea rsi, [msg_run]
    mov rdx, msg_run_len
    call rt_print
    call build_compile
    call build_run
    jmp .done

.run_chdir_fail:
    lea rsi, [msg_run_chdir_fail]
    mov rdx, msg_run_chdir_fail_len
    call rt_print
    jmp .done

.do_check:
    lea rsi, [msg_check]
    mov rdx, msg_check_len
    call rt_print

    cmp rbx, 2
    jg .check_path_arg
    lea rdi, [default_check_path]
    jmp .check_read

.check_path_arg:
    mov rdi, [r12 + 16]

.check_read:
    call filesystem_read_file
    test rax, rax
    jz .check_read_fail

    mov rdi, rax
    mov rsi, rdx
    mov r13, rax
    call syntax_lex_buffer
    test rax, rax
    jnz .check_fail

    lea rsi, [msg_check_ok]
    mov rdx, msg_check_ok_len
    call rt_print

    call frontend_parse_tokens
    test rax, rax
    jnz .parse_fail

    lea rsi, [msg_parse_ok]
    mov rdx, msg_parse_ok_len
    call rt_print
    jmp .done

.check_read_fail:
    lea rsi, [msg_check_read_fail]
    mov rdx, msg_check_read_fail_len
    call rt_print
    jmp .done

.check_fail:
    lea rsi, [msg_check_fail]
    mov rdx, msg_check_fail_len
    call rt_print
    jmp .done

.parse_fail:
    lea rsi, [msg_parse_fail]
    mov rdx, msg_parse_fail_len
    call rt_print
    jmp .done

.do_new:
    lea rsi, [msg_new]
    mov rdx, msg_new_len
    call rt_print
    jmp .done

.do_init:
    lea rsi, [msg_init]
    mov rdx, msg_init_len
    call rt_print
    jmp .done

.do_compile:
    cmp rbx, 2
    jle .compile_print

    mov rax, 80
    mov rdi, [r12 + 16]
    syscall
    test rax, rax
    js .compile_chdir_fail

.compile_print:
    lea rsi, [msg_compile]
    mov rdx, msg_compile_len
    call rt_print
    call build_compile
    jmp .done

.compile_chdir_fail:
    lea rsi, [msg_compile_chdir_fail]
    mov rdx, msg_compile_chdir_fail_len
    call rt_print
    jmp .done

.do_update:
    lea rsi, [msg_update]
    mov rdx, msg_update_len
    call rt_print
    call update_check
    jmp .done

.done:
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret