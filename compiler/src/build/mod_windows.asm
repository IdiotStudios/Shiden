default rel

section .data
    msg_building db "[info] Building project...", 0xA
    msg_building_len equ $ - msg_building
    msg_targets db "[info] Targets: "
    msg_targets_len equ $ - msg_targets
    msg_target db "[info] Target: "
    msg_target_len equ $ - msg_target
    msg_no_targets db "[error] No targets in build.ini", 0xA
    msg_no_targets_len equ $ - msg_no_targets
    msg_nl db 0xA
    msg_nl_len equ $ - msg_nl
    msg_build_done db "[ok] Build complete", 0xA
    msg_build_done_len equ $ - msg_build_done
    msg_run_fail db "[error] Could not run built output", 0xA
    msg_run_fail_len equ $ - msg_run_fail
    msg_no_linux db "[error] No runnable windows target in build.ini", 0xA
    msg_no_linux_len equ $ - msg_no_linux
    build_root db "build", 0
    build_prefix db "build/", 0
    slash_str db "/", 0
    ext_exe db ".exe", 0
    default_name db "app", 0
    elf64_stub:
        db 0x7F, "ELF", 0x02, 0x01, 0x01, 0x00
        db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        dw 0x0002
        dw 0x003E
        dd 0x00000001
        dq 0x0000000000400078
        dq 0x0000000000000040
        dq 0x0000000000000000
        dd 0x00000000
        dw 0x0040
        dw 0x0038
        dw 0x0001
        dw 0x0000
        dw 0x0000
        dw 0x0000
        dd 0x00000001
        dd 0x00000005
        dq 0x0000000000000000
        dq 0x0000000000400000
        dq 0x0000000000400000
        dq elf64_stub_len
        dq elf64_stub_len
        dq 0x0000000000001000
        db 0xB8, 0x3C, 0x00, 0x00, 0x00
        db 0x31, 0xFF
        db 0x0F, 0x05
    elf64_stub_len equ $ - elf64_stub
    sh_header db "#!/usr/bin/env sh", 0xA
    sh_header_len equ $ - sh_header
    sh_echo_prefix db "echo ", 34, "Shiden program stub: "
    sh_echo_prefix_len equ $ - sh_echo_prefix
    sh_mid_linux db " (linux)", 34, 0xA
    sh_mid_linux_len equ $ - sh_mid_linux
    sh_mid_macos db " (macos)", 34, 0xA
    sh_mid_macos_len equ $ - sh_mid_macos
    sh_exit db "exit 0", 0xA
    sh_exit_len equ $ - sh_exit
    pe_stub db "MZ", 0x90, 0x00, 0x03
    pe_stub_len equ $ - pe_stub

section .bss
    path_dir resb 256
    path_file resb 320
    project_ptr resq 1
    project_len resq 1
    run_path resb 320
    run_argv resq 2
    run_ready resq 1
    emit_code_size resq 1
    write_count resd 1
    startup_info resb 104
    process_info resb 24

section .text
    global build_compile
    global build_run
    extern rt_print
    extern config_load_ini
    extern config_get_project_name
    extern config_get_targets
    extern compiler_compile_project
    extern codegen_get_code
    extern codegen_get_code_size
    extern codegen_get_data
    extern codegen_get_data_size
    extern CreateDirectoryA
    extern CreateFileA
    extern WriteFile
    extern CloseHandle
    extern CreateProcessA
    extern WaitForSingleObject

build_compile:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    lea rsi, [msg_building]
    mov rdx, msg_building_len
    call rt_print

    xor rax, rax
    mov [run_ready], rax

    call config_load_ini
    test rax, rax
    jnz .done
    
    call compiler_compile_project
    test rax, rax
    jnz .done

    call config_get_project_name
    test rsi, rsi
    jz .use_default_name
    test rdx, rdx
    jz .use_default_name
    mov [project_ptr], rsi
    mov [project_len], rdx
    jmp .have_name

.use_default_name:
    lea rsi, [default_name]
    mov [project_ptr], rsi
    mov qword [project_len], 3

.have_name:

    call config_get_targets
    test rsi, rsi
    jz .no_targets
    test rdx, rdx
    jz .no_targets

    mov r12, rsi
    mov r13, rdx

    lea rsi, [msg_targets]
    mov rdx, msg_targets_len
    call rt_print

    mov rsi, r12
    mov rdx, r13
    call rt_print

    lea rsi, [msg_nl]
    mov rdx, msg_nl_len
    call rt_print

    lea rdi, [build_root]
    call win_mkdir

    xor rbx, rbx

.target_loop:
    cmp rbx, r13
    jge .done

.skip_delim:
    cmp rbx, r13
    jge .done
    mov al, byte [r12 + rbx]
    cmp al, ','
    je .skip_delim_advance
    cmp al, ' '
    je .skip_delim_advance
    cmp al, 9
    je .skip_delim_advance
    cmp al, 10
    je .skip_delim_advance
    cmp al, 13
    je .skip_delim_advance
    jmp .token_start

.skip_delim_advance:
    inc rbx
    jmp .skip_delim

.token_start:
    mov r14, rbx

.scan_token:
    cmp rbx, r13
    jge .token_end
    mov al, byte [r12 + rbx]
    cmp al, ','
    je .token_end
    cmp al, ' '
    je .token_end
    cmp al, 9
    je .token_end
    cmp al, 10
    je .token_end
    cmp al, 13
    je .token_end
    inc rbx
    jmp .scan_token

.token_end:
    mov r15, rbx
    mov rax, r15
    sub rax, r14
    test rax, rax
    jz .target_loop

    lea rsi, [msg_target]
    mov rdx, msg_target_len
    call rt_print

    lea rsi, [r12 + r14]
    mov rdx, r15
    sub rdx, r14
    call rt_print

    lea rsi, [msg_nl]
    mov rdx, msg_nl_len
    call rt_print

    call .write_target_binary
    jmp .target_loop

.no_targets:
    lea rsi, [msg_no_targets]
    mov rdx, msg_no_targets_len
    call rt_print
    
.done:
    lea rsi, [msg_build_done]
    mov rdx, msg_build_done_len
    call rt_print

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.write_target_binary:
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push r8

    lea rdi, [path_dir]
    lea rsi, [build_prefix]
.copy_prefix_dir:
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    test al, al
    jnz .copy_prefix_dir
    dec rdi

    lea rsi, [r12 + r14]
    mov rcx, r15
    sub rcx, r14
.copy_target_dir:
    test rcx, rcx
    jz .finish_dir
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jmp .copy_target_dir

.finish_dir:
    mov byte [rdi], 0

    lea rdi, [path_dir]
    call win_mkdir

    lea rdi, [path_file]
    lea rsi, [path_dir]
.copy_dir_to_file:
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    test al, al
    jnz .copy_dir_to_file
    dec rdi

    lea rsi, [slash_str]
.copy_path_sep:
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    test al, al
    jnz .copy_path_sep
    dec rdi

    mov rsi, [project_ptr]
    mov rcx, [project_len]
.copy_project_name:
    test rcx, rcx
    jz .maybe_windows_ext
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jmp .copy_project_name

.maybe_windows_ext:
    mov rax, r15
    sub rax, r14
    cmp rax, 7
    jne .finalize_path
    cmp byte [r12 + r14], 'w'
    jne .finalize_path
    cmp byte [r12 + r14 + 1], 'i'
    jne .finalize_path
    cmp byte [r12 + r14 + 2], 'n'
    jne .finalize_path
    cmp byte [r12 + r14 + 3], 'd'
    jne .finalize_path
    cmp byte [r12 + r14 + 4], 'o'
    jne .finalize_path
    cmp byte [r12 + r14 + 5], 'w'
    jne .finalize_path
    cmp byte [r12 + r14 + 6], 's'
    jne .finalize_path

    lea rsi, [ext_exe]
.copy_ext:
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    test al, al
    jnz .copy_ext
    dec rdi

.finalize_path:
    mov byte [rdi], 0

    lea rdi, [path_file]
    call win_open_create
    test rax, rax
    js .artifact_ret

    mov r8, rax

    mov rax, r15
    sub rax, r14
    cmp rax, 5
    je .emit_linux_or_macos
    cmp rax, 7
    je .emit_windows
    jmp .emit_generic

.emit_linux_or_macos:
    cmp byte [r12 + r14], 'l'
    jne .check_macos
    cmp byte [r12 + r14 + 1], 'i'
    jne .check_macos
    cmp byte [r12 + r14 + 2], 'n'
    jne .check_macos
    cmp byte [r12 + r14 + 3], 'u'
    jne .check_macos
    cmp byte [r12 + r14 + 4], 'x'
    jne .check_macos

    call codegen_get_code_size
    mov [emit_code_size], rax
    
    call .emit_elf_header_dyn
    call .emit_program_headers_dyn
    call .emit_code_dyn
    call .emit_data_dyn

    jmp .close_file

.emit_elf_header_dyn:
    sub rsp, 64
    
    mov byte [rsp], 0x7F
    mov byte [rsp+1], 'E'
    mov byte [rsp+2], 'L'
    mov byte [rsp+3], 'F'
    mov byte [rsp+4], 0x02
    mov byte [rsp+5], 0x01
    mov byte [rsp+6], 0x01
    mov byte [rsp+7], 0x00
    mov qword [rsp+8], 0
    
    mov word [rsp+16], 0x0002
    mov word [rsp+18], 0x003E
    mov dword [rsp+20], 0x00000001
    mov qword [rsp+24], 0x0000000000400078
    mov qword [rsp+32], 0x0000000000000040
    mov qword [rsp+40], 0x0000000000000000
    mov dword [rsp+48], 0x00000000
    mov word [rsp+52], 0x0040
    mov word [rsp+54], 0x0038
    mov word [rsp+56], 0x0001
    mov word [rsp+58], 0x0000
    mov word [rsp+60], 0x0000
    mov word [rsp+62], 0x0000
    
    mov rdi, r8
    lea rsi, [rsp]
    mov rdx, 64
    call win_write
    
    add rsp, 64
    ret

.emit_program_headers_dyn:
    sub rsp, 56
    mov r9, [emit_code_size]

    mov dword [rsp], 0x00000001
    mov dword [rsp+4], 0x00000005
    mov qword [rsp+8], 0x0000000000000078
    mov qword [rsp+16], 0x0000000000400078
    mov qword [rsp+24], 0x0000000000400078
    mov qword [rsp+32], r9
    mov qword [rsp+40], r9
    mov qword [rsp+48], 0x0000000000001000

    mov rdi, r8
    lea rsi, [rsp]
    mov rdx, 56
    call win_write

    add rsp, 56
    ret

.emit_code_dyn:
    mov r9, [emit_code_size]
    test r9, r9
    jz .ecd_ret
    
    call codegen_get_code
    mov rsi, rax
    mov rdi, r8
    mov rdx, r9
    call win_write
.ecd_ret:
    ret

.emit_data_dyn:
.edd_ret:
    ret

.check_macos:
    cmp byte [r12 + r14], 'm'
    jne .emit_generic
    cmp byte [r12 + r14 + 1], 'a'
    jne .emit_generic
    cmp byte [r12 + r14 + 2], 'c'
    jne .emit_generic
    cmp byte [r12 + r14 + 3], 'o'
    jne .emit_generic
    cmp byte [r12 + r14 + 4], 's'
    jne .emit_generic

    mov rdi, r8
    lea rsi, [sh_header]
    mov rdx, sh_header_len
    call win_write

    mov rdi, r8
    lea rsi, [sh_echo_prefix]
    mov rdx, sh_echo_prefix_len
    call win_write

    mov rdi, r8
    mov rsi, [project_ptr]
    mov rdx, [project_len]
    call win_write

    mov rdi, r8
    lea rsi, [sh_mid_macos]
    mov rdx, sh_mid_macos_len
    call win_write

    mov rdi, r8
    lea rsi, [sh_exit]
    mov rdx, sh_exit_len
    call win_write
    jmp .close_file

.emit_windows:
    cmp byte [r12 + r14], 'w'
    jne .emit_generic
    cmp byte [r12 + r14 + 1], 'i'
    jne .emit_generic
    cmp byte [r12 + r14 + 2], 'n'
    jne .emit_generic
    cmp byte [r12 + r14 + 3], 'd'
    jne .emit_generic
    cmp byte [r12 + r14 + 4], 'o'
    jne .emit_generic
    cmp byte [r12 + r14 + 5], 'w'
    jne .emit_generic
    cmp byte [r12 + r14 + 6], 's'
    jne .emit_generic

    call codegen_get_code_size
    mov [emit_code_size], rax
    
    call .emit_pe32_binary

    lea rsi, [path_file]
    lea rdi, [run_path]
.copy_run_path_win:
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    test al, al
    jnz .copy_run_path_win
    mov qword [run_ready], 1
    jmp .close_file

.emit_pe32_binary:
    push r12
    push r13
    push r14
    sub rsp, 512
    
    mov r12, r8
    mov r13, [emit_code_size]
    
    lea r14, [rsp]
    mov rcx, 512
    xor rax, rax
.epb_clear:
    mov byte [r14 + rcx - 1], al
    dec rcx
    jnz .epb_clear
    
    mov byte [r14], 'M'
    mov byte [r14 + 1], 'Z'
    mov byte [r14 + 2], 0x90
    mov byte [r14 + 3], 0x00
    mov dword [r14 + 0x3C], 64
    
    mov rdi, r12
    mov rsi, r14
    mov rdx, 64
    call win_write
    
    lea r14, [rsp]
    mov rcx, 264
    xor rax, rax
.epb_clear2:
    mov byte [r14 + rcx - 1], al
    dec rcx
    jnz .epb_clear2
    
    mov byte [r14], 'P'
    mov byte [r14 + 1], 'E'
    mov byte [r14 + 2], 0
    mov byte [r14 + 3], 0
    
    mov word [r14 + 4], 0x8664
    mov word [r14 + 6], 1
    mov dword [r14 + 8], 0
    mov dword [r14 + 12], 0
    mov dword [r14 + 16], 0
    mov word [r14 + 20], 0x00F0
    mov word [r14 + 22], 0x022F
    
    mov word [r14 + 24], 0x020B
    mov byte [r14 + 26], 14
    mov byte [r14 + 27], 0
    
    mov eax, r13d
    mov [r14 + 28], eax
    mov dword [r14 + 32], 0
    mov dword [r14 + 36], 0
    mov dword [r14 + 40], 0x00001000
    mov dword [r14 + 44], 0x00001000
    
    mov dword [r14 + 48], 0x00400000
    mov dword [r14 + 52], 0
    
    mov dword [r14 + 56], 0x00001000
    mov dword [r14 + 60], 0x00000200
    
    mov word [r14 + 64], 6
    mov word [r14 + 66], 0
    mov word [r14 + 68], 0
    mov word [r14 + 70], 0
    mov word [r14 + 72], 6
    mov word [r14 + 74], 0
    mov dword [r14 + 76], 0
    
    mov eax, r13d
    add eax, 0x1000
    and eax, 0xFFFFF000
    cmp eax, r13d
    jge .epb_size_ok
    add eax, 0x1000
.epb_size_ok:
    add eax, 0x1000
    mov [r14 + 80], eax
    
    mov dword [r14 + 84], 0x00000200
    mov dword [r14 + 88], 0
    mov word [r14 + 92], 3
    mov word [r14 + 94], 0x0100
    
    mov qword [r14 + 96], 0x100000
    mov qword [r14 + 104], 0x100000
    mov qword [r14 + 112], 0x100000
    mov qword [r14 + 120], 0x10000
    mov dword [r14 + 128], 0x00001000
    mov dword [r14 + 132], 16
    
    mov rdi, r12
    mov rsi, r14
    mov rdx, 264
    call win_write
    
    lea r14, [rsp]
    mov rcx, 40
    xor rax, rax
.epb_clear3:
    mov byte [r14 + rcx - 1], al
    dec rcx
    jnz .epb_clear3
    
    mov byte [r14], '.'
    mov byte [r14 + 1], 't'
    mov byte [r14 + 2], 'e'
    mov byte [r14 + 3], 'x'
    mov byte [r14 + 4], 't'
    
    mov eax, r13d
    mov [r14 + 8], eax
    mov dword [r14 + 12], 0x00001000
    
    mov eax, r13d
    add eax, 511
    and eax, 0xFFFFFE00
    mov [r14 + 16], eax
    
    mov dword [r14 + 20], 0x00000200
    mov dword [r14 + 24], 0
    mov dword [r14 + 28], 0
    mov word [r14 + 32], 0
    mov word [r14 + 34], 0
    mov dword [r14 + 36], 0x60000020
    
    mov rdi, r12
    mov rsi, r14
    mov rdx, 40
    call win_write
    
    lea r14, [rsp]
    mov rcx, 144
    xor rax, rax
.epb_clear4:
    mov byte [r14 + rcx - 1], al
    dec rcx
    jnz .epb_clear4
    
    mov rdi, r12
    mov rsi, r14
    mov rdx, 144
    call win_write
    
    call codegen_get_code
    mov rsi, rax
    mov rdi, r12
    mov rdx, r13
    call win_write
    
    mov rax, r13
    add rax, 511
    and rax, 0xFFFFFE00
    sub rax, r13
    test rax, rax
    jz .epb_no_pad
    
    lea r14, [rsp]
    mov rcx, rax
    cmp rcx, 512
    jle .epb_pad_ok
    mov rcx, 512
.epb_pad_ok:
    xor rax, rax
.epb_clear_pad:
    mov byte [r14 + rcx - 1], al
    dec rcx
    jnz .epb_clear_pad
    
    mov rdi, r12
    mov rsi, r14
    mov rdx, rax
    call win_write
    
.epb_no_pad:
    add rsp, 512
    pop r14
    pop r13
    pop r12
    ret

.emit_generic:
    mov rdi, r8
    lea rsi, [sh_header]
    mov rdx, sh_header_len
    call win_write

    mov rdi, r8
    lea rsi, [sh_exit]
    mov rdx, sh_exit_len
    call win_write

.close_file:
    mov rdi, r8
    call win_close

.artifact_ret:
    pop r8
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    ret

build_run:
    cmp qword [run_ready], 1
    jne .run_no_linux

    lea rdi, [run_path]
    call win_exec_wait
    test rax, rax
    jz .run_ok

    lea rsi, [msg_run_fail]
    mov rdx, msg_run_fail_len
    call rt_print
    mov rax, -1
    ret

.run_ok:
    xor rax, rax
    ret

.run_no_linux:
    lea rsi, [msg_no_linux]
    mov rdx, msg_no_linux_len
    call rt_print
    mov rax, -1
    ret

win_mkdir:
    push r14
    mov r14, rsp
    and rsp, -16
    sub rsp, 48
    mov rcx, rdi
    xor rdx, rdx
    call CreateDirectoryA
    mov rsp, r14
    pop r14
    ret

win_open_create:
    push r14
    mov r14, rsp
    and rsp, -16
    sub rsp, 64
    mov rcx, rdi
    mov rdx, 0x40000000
    xor r8, r8
    xor r9, r9
    mov qword [rsp + 32], 2
    mov qword [rsp + 40], 0x80
    mov qword [rsp + 48], 0
    call CreateFileA
    cmp rax, -1
    je .woc_fail
    mov rsp, r14
    pop r14
    ret
.woc_fail:
    mov rax, -1
    mov rsp, r14
    pop r14
    ret

win_write:
    push r14
    mov r14, rsp
    and rsp, -16
    sub rsp, 48
    mov rcx, rdi
    mov r8, rdx
    mov rdx, rsi
    lea r9, [write_count]
    mov dword [write_count], 0
    mov qword [rsp + 32], 0
    call WriteFile
    mov rsp, r14
    pop r14
    ret

win_close:
    push r14
    mov r14, rsp
    and rsp, -16
    sub rsp, 48
    mov rcx, rdi
    call CloseHandle
    mov rsp, r14
    pop r14
    ret

win_exec_wait:
    push rbx
    push r14
    mov rbx, rdi
    mov r14, rsp
    and rsp, -16

    lea rdi, [startup_info]
    mov rcx, 104
    xor rax, rax
.wes_zero_start:
    mov byte [rdi], 0
    inc rdi
    dec rcx
    jnz .wes_zero_start

    lea rdi, [process_info]
    mov rcx, 24
.wes_zero_proc:
    mov byte [rdi], 0
    inc rdi
    dec rcx
    jnz .wes_zero_proc

    mov dword [startup_info], 104

    sub rsp, 96
    xor rcx, rcx
    mov rdx, rbx
    xor r8, r8
    xor r9, r9
    mov qword [rsp + 32], 0
    mov qword [rsp + 40], 0
    mov qword [rsp + 48], 0
    mov qword [rsp + 56], 0
    lea rax, [startup_info]
    mov qword [rsp + 64], rax
    lea rax, [process_info]
    mov qword [rsp + 72], rax
    call CreateProcessA
    test rax, rax
    jz .wes_fail

    mov rcx, [process_info]
    mov rdx, -1
    call WaitForSingleObject

    mov rcx, [process_info]
    call CloseHandle
    mov rcx, [process_info + 8]
    call CloseHandle

    mov rsp, r14
    xor rax, rax
    pop r14
    pop rbx
    ret

.wes_fail:
    mov rsp, r14
    mov rax, -1
    pop r14
    pop rbx
    ret
