default rel

section .data
    build_ini_path db "build.ini", 0
    msg_reading_ini db "[info] Reading build.ini...", 0xA
    msg_reading_ini_len equ $ - msg_reading_ini
    msg_parsed_ok db "[ok] Parsed build.ini", 0xA
    msg_parsed_ok_len equ $ - msg_parsed_ok
    msg_not_found db "[error] build.ini not found", 0xA
    msg_not_found_len equ $ - msg_not_found

section .bss
    cfg_project_name: resq 1
    cfg_project_name_len: resq 1
    cfg_targets: resq 1
    cfg_targets_len: resq 1
    cfg_opt_level: resq 1
    cfg_debug: resq 1
    cfg_name_buffer: resb 256
    cfg_targets_buffer: resb 256

section .text
    global config_load_ini
    global config_get_project_name
    global config_get_targets
    global config_get_opt_level
    global config_get_debug
    extern filesystem_read_file
    extern rt_print

config_load_ini:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    
    lea rsi, [msg_reading_ini]
    mov rdx, msg_reading_ini_len
    call rt_print
    
    lea rdi, [build_ini_path]
    call filesystem_read_file
    test rax, rax
    jz .not_found
    
    mov r12, rax
    mov r13, rdx

    xor rax, rax
    mov [cfg_project_name], rax
    mov [cfg_project_name_len], rax
    mov [cfg_targets], rax
    mov [cfg_targets_len], rax
    mov qword [cfg_opt_level], 3
    mov qword [cfg_debug], 0

    xor rcx, rcx

.search_name_loop:
    cmp rcx, r13
    jge .search_targets_start

    mov al, byte [r12 + rcx]
    inc rcx

    cmp al, 'n'
    jne .search_name_loop

    mov rax, r13
    sub rax, 4
    cmp rcx, rax
    jg .search_targets_start

    mov al, byte [r12 + rcx]
    cmp al, 'a'
    jne .search_name_loop
    mov al, byte [r12 + rcx + 1]
    cmp al, 'm'
    jne .search_name_loop
    mov al, byte [r12 + rcx + 2]
    cmp al, 'e'
    jne .search_name_loop

    add rcx, 3

.find_name_equals:
    cmp rcx, r13
    jge .search_targets_start
    mov al, byte [r12 + rcx]
    inc rcx
    cmp al, '='
    jne .find_name_equals

.skip_name_ws:
    cmp rcx, r13
    jge .search_targets_start
    mov al, byte [r12 + rcx]
    cmp al, ' '
    je .name_ws_char
    cmp al, 9
    je .name_ws_char
    jmp .extract_name
.name_ws_char:
    inc rcx
    jmp .skip_name_ws

.extract_name:
    lea r14, [r12 + rcx]

.find_name_end:
    cmp rcx, r13
    jge .name_end
    mov al, byte [r12 + rcx]
    cmp al, 10
    je .name_end
    cmp al, 13
    je .name_end
    inc rcx
    jmp .find_name_end

.name_end:
    mov rax, rcx
    mov rdx, r14
    sub rdx, r12
    sub rax, rdx
    
    lea rdi, [cfg_name_buffer]
    mov rsi, r14
    mov rcx, rax
    xor rbx, rbx
.copy_name:
    cmp rbx, rcx
    jae .name_copied
    mov dl, byte [rsi + rbx]
    mov byte [rdi + rbx], dl
    inc rbx
    jmp .copy_name
.name_copied:
    lea r14, [cfg_name_buffer]
    mov [cfg_project_name], r14
    mov [cfg_project_name_len], rax

.search_targets_start:
    xor rcx, rcx

.search_loop:
    cmp rcx, r13
    jge .parse_done

    mov al, byte [r12 + rcx]
    inc rcx

    cmp al, 't'
    jne .search_loop

    mov rax, r13
    sub rax, 6
    cmp rcx, rax
    jge .parse_done

    mov al, byte [r12 + rcx]
    cmp al, 'a'
    jne .search_loop
    mov al, byte [r12 + rcx + 1]
    cmp al, 'r'
    jne .search_loop
    mov al, byte [r12 + rcx + 2]
    cmp al, 'g'
    jne .search_loop
    mov al, byte [r12 + rcx + 3]
    cmp al, 'e'
    jne .search_loop
    mov al, byte [r12 + rcx + 4]
    cmp al, 't'
    jne .search_loop
    mov al, byte [r12 + rcx + 5]
    cmp al, 's'
    jne .search_loop

    add rcx, 6

.find_equals:
    cmp rcx, r13
    jge .parse_done
    mov al, byte [r12 + rcx]
    inc rcx
    cmp al, '='
    jne .find_equals

.skip_ws_after_eq:
    cmp rcx, r13
    jge .parse_done
    mov al, byte [r12 + rcx]
    cmp al, ' '
    je .ws_char
    cmp al, 9
    je .ws_char
    jmp .extract_value
.ws_char:
    inc rcx
    jmp .skip_ws_after_eq

.extract_value:
    lea r14, [r12 + rcx]

.find_value_end:
    cmp rcx, r13
    jge .value_end
    mov al, byte [r12 + rcx]
    cmp al, 10
    je .value_end
    cmp al, 13
    je .value_end
    inc rcx
    jmp .find_value_end

.value_end:
    mov rax, rcx
    mov rdx, r14
    sub rdx, r12
    sub rax, rdx
    
    lea rdi, [cfg_targets_buffer]
    mov rsi, r14
    push rcx
    mov rcx, rax
    xor rbx, rbx
.copy_targets:
    cmp rbx, rcx
    jae .targets_copied
    mov dl, byte [rsi + rbx]
    mov byte [rdi + rbx], dl
    inc rbx
    jmp .copy_targets
.targets_copied:
    pop rcx
    lea r14, [cfg_targets_buffer]
    mov [cfg_targets], r14
    mov [cfg_targets_len], rax
    
.parse_done:
    lea rsi, [msg_parsed_ok]
    mov rdx, msg_parsed_ok_len
    call rt_print
    
    xor rax, rax
    jmp .load_ret
    
.not_found:
    lea rsi, [msg_not_found]
    mov rdx, msg_not_found_len
    call rt_print
    
    mov rax, -1
    
.load_ret:
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

global config_get_project_name
config_get_project_name:
    mov rsi, [cfg_project_name]
    mov rdx, [cfg_project_name_len]
    ret

global config_get_targets
config_get_targets:
    mov rsi, [cfg_targets]
    mov rdx, [cfg_targets_len]
    ret

global config_get_opt_level
config_get_opt_level:
    mov rax, [cfg_opt_level]
    ret

global config_get_debug
config_get_debug:
    mov rax, [cfg_debug]
    ret
