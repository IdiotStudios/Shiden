default rel

section .data
    msg_reading_src db "[info] Reading source files...", 0xA
    msg_reading_src_len equ $ - msg_reading_src
    msg_parsing db "[info] Parsing...", 0xA
    msg_parsing_len equ $ - msg_parsing
    msg_compiling db "[info] Compiling to machine code...", 0xA
    msg_compiling_len equ $ - msg_compiling
    msg_read_fail db "[error] Failed to read src/main.sd", 0xA
    msg_read_fail_len equ $ - msg_read_fail
    msg_lex_fail db "[error] Lexing failed", 0xA
    msg_lex_fail_len equ $ - msg_lex_fail
    lit_true db "true"
    lit_true_len equ $ - lit_true
    lit_false db "false"
    lit_false_len equ $ - lit_false

    src_main_path db "src/main.sd", 0

section .bss
    source_buffer resb 1048576
    source_len resq 1
    
    ; Local variable storage (max 32 variables)
    local_count resq 1
    local_name_ptr resq 32
    local_name_len resq 32
    local_value_ptr resq 32
    local_value_len resq 32
    
    ; Temporary argument storage for println (max 8 args)
    arg_count resq 1
    arg_value_ptr resq 8
    arg_value_len resq 8
    
    ; String formatting buffer
    format_buffer resb 8192
    index_key_buffer resb 256
    key_pool resb 4096
    key_pool_pos resq 1
    value_pool resb 8192
    value_pool_pos resq 1
    if_skip_else_idx resq 1
    if_skip_close_idx resq 1
    loop_depth resq 1
    loop_kind resq 8
    loop_header_idx resq 8
    loop_body_idx resq 8
    loop_end_idx resq 8
    loop_break_flag resq 8
    for_var_ptr resq 8
    for_var_len resq 8
    for_first_tok resq 8
    for_count resq 8
    for_iter resq 8

section .text
    global compiler_compile_project
    extern rt_print
    extern filesystem_read_file
    extern syntax_lex_buffer
    extern syntax_token_count
    extern syntax_tokens_kind
    extern syntax_tokens_start
    extern syntax_tokens_len
    extern syntax_source_ptr
    extern syntax_source_len
    extern codegen_init
    extern codegen_emit_prologue
    extern codegen_emit_epilogue
    extern codegen_emit_println

compiler_compile_project:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    lea rsi, [msg_reading_src]
    mov rdx, msg_reading_src_len
    call rt_print

    lea rdi, [src_main_path]
    call filesystem_read_file
    test rax, rax
    jz .read_fail

    mov [source_len], rdx
    test rdx, rdx
    jz .read_fail

    mov rdi, rax
    mov rsi, rdx
    lea rax, [source_buffer]
    mov rbx, rax
    xor rcx, rcx
.copy_source:
    cmp rcx, rsi
    jae .copy_done
    mov al, byte [rdi + rcx]
    mov byte [rbx + rcx], al
    inc rcx
    jmp .copy_source
.copy_done:

    lea rsi, [msg_parsing]
    mov rdx, msg_parsing_len
    call rt_print

    lea rdi, [source_buffer]
    mov rsi, [source_len]
    call syntax_lex_buffer

    test rax, rax
    jnz .lex_fail

    lea rsi, [msg_compiling]
    mov rdx, msg_compiling_len
    call rt_print

    ; Initialize local variable count
    mov qword [local_count], 0
    mov qword [value_pool_pos], 0
    mov qword [key_pool_pos], 0
    mov qword [if_skip_else_idx], -1
    mov qword [if_skip_close_idx], -1
    mov qword [loop_depth], 0

    call codegen_init
    call codegen_emit_prologue

    mov r12, [syntax_token_count]
    test r12, r12
    jz .emit_epilogue

    lea r13, [syntax_tokens_kind]
    lea r14, [syntax_tokens_start]
    lea r15, [syntax_tokens_len]
    xor rbx, rbx

.token_loop:
    cmp rbx, r12
    jae .emit_epilogue

    mov eax, dword [r13 + rbx * 4]

    cmp eax, 1
    jne .dispatch_stmt
    mov qword [local_count], 0
    jmp .next_token

.dispatch_stmt:

    cmp eax, 3
    je .handle_let_stmt

    cmp eax, 5
    je .handle_return_stmt

    cmp eax, 6
    je .handle_if_stmt

    cmp eax, 7
    je .handle_while_stmt

    cmp eax, 8
    je .handle_else_stmt

    cmp eax, 9
    je .handle_break_stmt

    cmp eax, 10
    je .handle_continue_stmt

    cmp eax, 11
    je .handle_for_stmt

    cmp eax, 13
    jne .next_token
    jmp .handle_ident_stmt

.handle_let_stmt:
    push rbx

    inc rbx
    cmp rbx, r12
    jae .let_fail

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 4
    jne .let_expect_name

    inc rbx
    cmp rbx, r12
    jae .let_fail

.let_expect_name:
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 13
    jne .let_fail

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea r8, [source_buffer]
    add r8, rax
    mov r9, rdx

    inc rbx
    cmp rbx, r12
    jae .let_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 22
    jne .let_fail

    inc rbx
    cmp rbx, r12
    jae .let_fail

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 19
    je .let_array_literal

    mov rdi, rbx
    call evaluate_simple_expr
    test rax, rax
    jnz .let_value_ready

    mov rdi, rbx
    call resolve_simple_value
    test rax, rax
    jz .let_fail

.let_value_ready:

    mov rdi, r8
    mov rsi, r9
    mov r8, rax
    mov r9, rdx
    call local_set

    pop rbx
    jmp .next_token

.let_array_literal:
    push r8
    push r9
    xor r10, r10
    inc rbx

.let_array_loop:
    cmp rbx, r12
    jae .let_array_done

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 20
    je .let_array_done

    cmp eax, 16
    jne .let_array_next

    mov rax, r10
    call write_u64_to_pool
    test rax, rax
    jz .let_array_next

    mov r8, rax
    mov r9, rdx
    mov rdi, [rsp + 8]
    mov rsi, [rsp]
    call build_index_key_to_pool

    mov rdi, rax
    mov rsi, rdx
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea r8, [source_buffer + rax]
    mov r9, rdx
    call local_set

    inc r10

.let_array_next:
    inc rbx
    jmp .let_array_loop

.let_array_done:
    pop r9
    pop r8
    pop rbx
    jmp .next_token

.let_fail:
    pop rbx
    jmp .next_token

.handle_ident_stmt:
    push rbx

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea r8, [source_buffer]
    add r8, rax
    mov r9, rdx

    inc rbx
    cmp rbx, r12
    jae .ident_stmt_fail_simple

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 17
    je .ident_stmt_call
    cmp eax, 22
    je .handle_assign_stmt
    cmp eax, 19
    je .handle_index_assign
    jmp .ident_stmt_fail_simple

.ident_stmt_call:
    pop rbx
    jmp .handle_potential_call

.handle_assign_stmt:
    push r8
    push r9

    inc rbx
    cmp rbx, r12
    jae .assign_stmt_fail

    mov r10, rbx
    mov eax, dword [r13 + r10 * 4]
    cmp eax, 13
    jne .assign_try_simple

    mov rcx, r10
    inc rcx
    cmp rcx, r12
    jae .assign_try_simple
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 30
    jne .assign_try_simple

    mov rcx, r10
    add rcx, 2
    cmp rcx, r12
    jae .assign_try_simple
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .assign_try_simple

    mov rax, qword [r14 + r10 * 8]
    mov rdx, qword [r15 + r10 * 8]
    lea rdi, [source_buffer]
    add rdi, rax
    mov rsi, rdx
    call lookup_local
    test rax, rax
    jz .assign_try_simple

    push rcx
    mov rdi, rax
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .assign_parse_fail
    mov r11, rax

    pop rcx

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer]
    add rdi, rax
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .assign_try_simple

    add rax, r11
    call write_u64_to_pool
    test rax, rax
    jz .assign_stmt_fail

    pop rsi
    pop rdi
    mov r8, rax
    mov r9, rdx
    call local_set
    pop rbx
    jmp .next_token

.handle_index_assign:
    push r8
    push r9

    inc rbx
    cmp rbx, r12
    jae .index_assign_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 16
    jne .index_assign_fail

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea r8, [source_buffer + rax]
    mov r9, rdx
    mov rdi, [rsp + 8]
    mov rsi, [rsp]
    call build_index_key_to_pool
    mov r10, rax
    mov r11, rdx

    inc rbx
    cmp rbx, r12
    jae .index_assign_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 20
    jne .index_assign_fail

    inc rbx
    cmp rbx, r12
    jae .index_assign_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 22
    jne .index_assign_fail

    inc rbx
    cmp rbx, r12
    jae .index_assign_fail

    mov rdi, rbx
    call evaluate_simple_expr
    test rax, rax
    jnz .index_assign_store

    mov rdi, rbx
    call resolve_simple_value
    test rax, rax
    jz .index_assign_fail

.index_assign_store:
    mov r8, rax
    mov r9, rdx
    mov rdi, r10
    mov rsi, r11
    call local_set

    pop r9
    pop r8
    pop rbx
    jmp .next_token

.index_assign_fail:
    pop r9
    pop r8
    pop rbx
    jmp .next_token

.assign_parse_fail:
    pop rcx
    jmp .assign_try_simple

.assign_try_simple:
    mov rdi, r10
    call resolve_simple_value
    test rax, rax
    jz .assign_stmt_fail

    pop rsi
    pop rdi
    mov r8, rax
    mov r9, rdx
    call local_set
    pop rbx
    jmp .next_token

.assign_stmt_fail:
    pop r9
    pop r8
    pop rbx
    jmp .next_token

.ident_stmt_fail_simple:
    pop rbx
    jmp .next_token

.handle_for_stmt:
    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .for_open
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 35
    je .for_close

.for_open:
    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .next_token

    mov r8, qword [r14 + rcx * 8]
    mov r9, qword [r15 + rcx * 8]
    lea r8, [source_buffer + r8]

    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 12
    jne .next_token

    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 19
    jne .next_token

    mov r10, -1
    xor r11, r11
    inc rcx
.for_parse_items:
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 20
    je .for_items_done
    cmp eax, 16
    jne .for_items_next
    cmp r10, -1
    jne .for_have_first
    mov r10, rcx
.for_have_first:
    inc r11
.for_items_next:
    inc rcx
    jmp .for_parse_items

.for_items_done:
    mov rdi, rcx
    inc rdi
    call find_next_slash
    test rax, rax
    js .next_token
    mov rdx, rax

    mov rdi, rbx
    mov esi, 11
    call find_matching_block_end
    test rax, rax
    js .next_token
    mov rsi, rax

    test r11, r11
    jz .for_skip_all

    mov rcx, qword [loop_depth]
    cmp rcx, 8
    jae .next_token

    lea rax, [loop_kind]
    mov qword [rax + rcx * 8], 2
    lea rax, [loop_header_idx]
    mov [rax + rcx * 8], rbx
    lea rax, [loop_body_idx]
    mov rax, rdx
    inc rax
    mov [loop_body_idx + rcx * 8], rax
    lea rax, [loop_end_idx]
    mov [rax + rcx * 8], rsi
    lea rax, [loop_break_flag]
    mov qword [rax + rcx * 8], 0
    lea rax, [for_var_ptr]
    mov [rax + rcx * 8], r8
    lea rax, [for_var_len]
    mov [rax + rcx * 8], r9
    lea rax, [for_first_tok]
    mov [rax + rcx * 8], r10
    lea rax, [for_count]
    mov [rax + rcx * 8], r11
    lea rax, [for_iter]
    mov qword [rax + rcx * 8], 0
    inc qword [loop_depth]

    mov rax, qword [r14 + r10 * 8]
    mov rdx, qword [r15 + r10 * 8]
    lea rdi, [for_var_ptr + rcx * 8]
    mov rdi, [rdi]
    lea rsi, [for_var_len + rcx * 8]
    mov rsi, [rsi]
    lea r8, [source_buffer + rax]
    mov r9, rdx
    call local_set

    mov rbx, rdx
    mov rbx, qword [loop_body_idx + rcx * 8]
    dec rbx
    jmp .next_token

.for_skip_all:
    mov rbx, rsi
    inc rbx
    jmp .next_token

.for_close:
    mov rcx, qword [loop_depth]
    test rcx, rcx
    jz .next_token
    dec rcx
    cmp qword [loop_kind + rcx * 8], 2
    jne .next_token
    cmp qword [loop_end_idx + rcx * 8], rbx
    jne .next_token

    cmp qword [loop_break_flag + rcx * 8], 0
    jne .for_close_break

    mov rax, qword [for_iter + rcx * 8]
    inc rax
    mov qword [for_iter + rcx * 8], rax
    cmp rax, qword [for_count + rcx * 8]
    jae .for_close_done

    mov rdx, qword [for_first_tok + rcx * 8]
    lea rdx, [rdx + rax * 2]
    mov rax, qword [r14 + rdx * 8]
    mov r9, qword [r15 + rdx * 8]
    mov rdi, qword [for_var_ptr + rcx * 8]
    mov rsi, qword [for_var_len + rcx * 8]
    lea r8, [source_buffer + rax]
    call local_set

    mov rbx, qword [loop_body_idx + rcx * 8]
    dec rbx
    jmp .next_token

.for_close_break:
    mov qword [loop_break_flag + rcx * 8], 0

.for_close_done:
    dec qword [loop_depth]
    inc rbx
    jmp .next_token

.handle_return_stmt:
    ; For now, just skip return statements
    jmp .next_token


.handle_if_stmt:
    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 35
    je .next_token

    mov rdi, rbx
    call find_if_else_close
    mov r10, rax
    mov r11, rdx
    test r11, r11
    js .next_token

    mov rdi, rbx
    inc rdi
    call eval_condition_simple
    test rax, rax
    jz .if_false

    mov qword [if_skip_else_idx], -1
    mov qword [if_skip_close_idx], -1
    cmp r10, -1
    je .next_token
    mov [if_skip_else_idx], r10
    mov [if_skip_close_idx], r11
    jmp .next_token

.if_false:
    cmp r10, -1
    jne .if_false_to_else
    mov rbx, r11
    inc rbx
    jmp .next_token

.if_false_to_else:
    mov rbx, r10
    inc rbx
    jmp .next_token


.handle_else_stmt:
    cmp qword [if_skip_else_idx], rbx
    jne .next_token
    mov rax, qword [if_skip_close_idx]
    mov qword [if_skip_else_idx], -1
    mov qword [if_skip_close_idx], -1
    test rax, rax
    js .next_token
    mov rbx, rax
    inc rbx
    jmp .next_token


.handle_while_stmt:
    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 35
    je .while_close

    mov rdi, rbx
    mov esi, 7
    call find_matching_block_end
    test rax, rax
    js .next_token
    mov r10, rax

    mov rdi, rbx
    inc rdi
    call find_next_slash
    test rax, rax
    js .next_token
    mov r11, rax

    mov rdi, rbx
    inc rdi
    call eval_condition_simple
    test rax, rax
    jz .while_skip

    mov rcx, qword [loop_depth]
    cmp rcx, 8
    jae .next_token
    mov qword [loop_kind + rcx * 8], 1
    mov qword [loop_header_idx + rcx * 8], rbx
    mov rax, r11
    inc rax
    mov qword [loop_body_idx + rcx * 8], rax
    mov qword [loop_end_idx + rcx * 8], r10
    mov qword [loop_break_flag + rcx * 8], 0
    inc qword [loop_depth]

    mov rbx, r11
    jmp .next_token

.while_skip:
    mov rbx, r10
    inc rbx
    jmp .next_token

.while_close:
    mov rcx, qword [loop_depth]
    test rcx, rcx
    jz .next_token
    dec rcx
    cmp qword [loop_kind + rcx * 8], 1
    jne .next_token
    cmp qword [loop_end_idx + rcx * 8], rbx
    jne .next_token

    cmp qword [loop_break_flag + rcx * 8], 0
    jne .while_done

    mov rdi, qword [loop_header_idx + rcx * 8]
    inc rdi
    call eval_condition_simple
    test rax, rax
    jz .while_done

    mov rbx, qword [loop_body_idx + rcx * 8]
    dec rbx
    jmp .next_token

.while_done:
    mov qword [loop_break_flag + rcx * 8], 0
    dec qword [loop_depth]
    inc rbx
    jmp .next_token


.handle_break_stmt:
    mov rcx, qword [loop_depth]
    test rcx, rcx
    jz .next_token
    dec rcx
    mov qword [loop_break_flag + rcx * 8], 1
    mov rbx, qword [loop_end_idx + rcx * 8]
    dec rbx
    jmp .next_token


.handle_continue_stmt:
    mov rcx, qword [loop_depth]
    test rcx, rcx
    jz .next_token
    dec rcx
    mov rbx, qword [loop_end_idx + rcx * 8]
    dec rbx
    jmp .next_token


.handle_potential_call:
    ; Current: TOK_IDENT at rbx
    inc rbx
    cmp rbx, r12
    jae .emit_epilogue

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 17  ; TOK_LPAREN
    jne .next_token

    mov rcx, rbx
    dec rcx
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    cmp rdx, 4
    jne .call_not_push
    lea rsi, [source_buffer + rax]
    cmp byte [rsi], 'p'
    jne .call_not_push
    cmp byte [rsi + 1], 'u'
    jne .call_not_push
    cmp byte [rsi + 2], 's'
    jne .call_not_push
    cmp byte [rsi + 3], 'h'
    jne .call_not_push

    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .next_token

    mov rdi, qword [r14 + rcx * 8]
    mov rsi, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rdi]

    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 21
    jne .next_token

    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .next_token

    mov r8, qword [r14 + rcx * 8]
    mov r9, qword [r15 + rcx * 8]
    lea r8, [source_buffer + r8]

    inc rcx
    cmp rcx, r12
    jae .next_token
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 18
    jne .next_token

    call push_array_element
    mov rbx, rcx
    jmp .next_token

.call_not_push:

    inc rbx

    mov r8, rbx
    xor r9, r9

.find_args:
    cmp rbx, r12
    jae .check_ident_println
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 18
    je .check_ident_println
    cmp eax, 14
    je .found_string_arg
    inc rbx
    jmp .find_args

.found_string_arg:
    mov r9, rbx

.skip_to_close:
    cmp rbx, r12
    jae .check_ident_println
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 18
    je .check_ident_println
    inc rbx
    jmp .skip_to_close

.check_ident_println:
    mov rcx, r8
    sub rcx, 2
    test rcx, rcx
    js .next_token

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]

    cmp rdx, 7
    jne .next_token

    lea rsi, [source_buffer]
    add rsi, rax

    cmp byte [rsi], 'p'
    jne .next_token
    cmp byte [rsi + 1], 'r'
    jne .next_token
    cmp byte [rsi + 2], 'i'
    jne .next_token
    cmp byte [rsi + 3], 'n'
    jne .next_token
    cmp byte [rsi + 4], 't'
    jne .next_token
    cmp byte [rsi + 5], 'l'
    jne .next_token
    cmp byte [rsi + 6], 'n'
    jne .next_token

    test r9, r9
    jz .next_token

    ; Get the string token
    mov rax, qword [r14 + r9 * 8]
    mov rdx, qword [r15 + r9 * 8]

    lea rdi, [source_buffer]
    add rdi, rax
    inc rdi  ; skip opening quote
    mov rsi, rdx
    sub rsi, 2  ; remove both quotes

    test rsi, rsi
    js .next_token

    ; Collect all arguments after the string
    ; r9 = string token index, rbx = ) token index
    mov qword [arg_count], 0
    mov rcx, r9
    inc rcx  ; move past string token
    
.collect_args_loop:
    cmp rcx, rbx
    jae .emit_println_with_args
    
    ; Check for comma
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 21  ; TOK_COMMA
    jne .emit_println_with_args
    
    ; Move to next token (should be identifier)
    inc rcx
    cmp rcx, rbx
    jae .emit_println_with_args
    
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13  ; TOK_IDENT
    jne .emit_println_with_args

    mov r11, rcx

    mov rax, qword [r15 + r11 * 8]
    cmp rax, 3
    jne .collect_maybe_call
    mov rax, qword [r14 + r11 * 8]
    lea r8, [source_buffer + rax]
    cmp byte [r8], 'l'
    jne .collect_maybe_call
    cmp byte [r8 + 1], 'e'
    jne .collect_maybe_call
    cmp byte [r8 + 2], 'n'
    jne .collect_maybe_call

    mov rax, r11
    add rax, 3
    cmp rax, rbx
    ja .collect_maybe_call

    mov rax, r11
    add rax, 1
    mov edx, dword [r13 + rax * 4]
    cmp edx, 17
    jne .collect_maybe_call

    mov rax, r11
    add rax, 2
    mov edx, dword [r13 + rax * 4]
    cmp edx, 13
    jne .collect_maybe_call

    mov rax, r11
    add rax, 3
    mov edx, dword [r13 + rax * 4]
    cmp edx, 18
    jne .collect_maybe_call

    push rbx
    push rcx
    push rdi
    push rsi
    mov rdi, r11
    call builtin_len_function
    pop rsi
    pop rdi
    pop rcx
    pop rbx
    test rax, rax
    jz .collect_maybe_call
    mov rcx, r11
    add rcx, 3
    jmp .collect_arg_resolved

.collect_maybe_call:

    mov rax, r11
    inc rax
    cmp rax, rbx
    jae .lookup_maybe_index
    mov edx, dword [r13 + rax * 4]
    cmp edx, 17
    jne .lookup_maybe_index

    mov r10, rax
    mov r9, 1
.collect_call_scan:
    inc r10
    cmp r10, rbx
    jae .lookup_maybe_index
    mov edx, dword [r13 + r10 * 4]
    cmp edx, 17
    je .collect_call_open
    cmp edx, 18
    je .collect_call_close
    jmp .collect_call_scan

.collect_call_open:
    inc r9
    jmp .collect_call_scan

.collect_call_close:
    dec r9
    test r9, r9
    jnz .collect_call_scan
    jmp .lookup_maybe_index

.lookup_maybe_index:

    mov rax, rcx
    inc rax
    cmp rax, rbx
    jae .lookup_plain_arg
    mov edx, dword [r13 + rax * 4]
    cmp edx, 19
    jne .lookup_plain_arg

    mov rdx, rcx
    add rdx, 2
    cmp rdx, rbx
    jae .lookup_plain_arg
    mov eax, dword [r13 + rdx * 4]
    cmp eax, 16
    jne .lookup_plain_arg

    mov rax, rcx
    add rax, 3
    cmp rax, rbx
    jae .lookup_plain_arg
    mov edx, dword [r13 + rax * 4]
    cmp edx, 20
    jne .lookup_plain_arg

    push rbx
    push rcx
    push rdi
    push rsi

    mov rax, qword [r14 + r11 * 8]
    mov rdx, qword [r15 + r11 * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx

    mov rax, r11
    add rax, 2
    mov rax, qword [r14 + rax * 8]
    mov rdx, qword [r15 + r11 * 8 + 16]
    lea r8, [source_buffer + rax]
    mov r9, rdx
    call build_index_key

    mov rdi, rax
    mov rsi, rdx
    call lookup_local

    pop rsi
    pop rdi
    pop rcx
    pop rbx

    mov rcx, r11
    add rcx, 3
    jmp .collect_arg_resolved

.lookup_plain_arg:
    
    ; Look up this variable
    push rbx
    push rcx
    push rdi
    push rsi
    
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    
    lea rdi, [source_buffer]
    add rdi, rax
    mov rsi, rdx
    
    call lookup_local
    
    ; rax = value ptr, rdx = value len
    pop rsi
    pop rdi
    pop rcx
    pop rbx

.collect_arg_resolved:
    
    test rax, rax
    jz .collect_args_next
    
    ; Store this argument
    push rcx
    mov rcx, [arg_count]
    cmp rcx, 8
    jae .collect_args_skip
    
    lea r10, [arg_value_ptr]
    mov [r10 + rcx * 8], rax
    lea r10, [arg_value_len]
    mov [r10 + rcx * 8], rdx
    inc qword [arg_count]
    
.collect_args_skip:
    pop rcx
    
.collect_args_next:
    inc rcx  ; move to next token
    jmp .collect_args_loop

.emit_println_with_args:
    ; Now format the string with all collected arguments
    mov r10, [arg_count]
    test r10, r10
    jz .emit_println_no_args
    
    ; Format with all arguments
    push rbx
    call format_multiple_placeholders
    ; rax = formatted ptr, rdx = formatted len
    
    mov rdi, rax
    mov rsi, rdx
    call codegen_emit_println
    pop rbx
    jmp .next_token

.emit_println_no_args:
    ; No variable substitution - emit as-is
    call codegen_emit_println

.next_token:
    inc rbx
    jmp .token_loop

.emit_epilogue:
    call codegen_emit_epilogue

    xor rax, rax
    jmp .compile_done

.read_fail:
    lea rsi, [msg_read_fail]
    mov rdx, msg_read_fail_len
    call rt_print
    mov rax, 1
    jmp .compile_done

.lex_fail:
    lea rsi, [msg_lex_fail]
    mov rdx, msg_lex_fail_len
    call rt_print
    mov rax, 1
    jmp .compile_done

.compile_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; Helper: Look up local variable by name
; Input: rdi = name ptr, rsi = name len
; Output: rax = value ptr (or 0 if not found), rdx = value len
lookup_local:
    push rbx
    push rcx
    push r8
    push r9
    
    mov rcx, [local_count]
    test rcx, rcx
    jz .lookup_not_found
    dec rcx
.lookup_loop:
    cmp rcx, -1
    je .lookup_not_found
    
    ; Check if length matches
    lea r8, [local_name_len]
    mov r9, [r8 + rcx * 8]
    cmp r9, rsi
    jne .lookup_next
    
    ; Compare name bytes
    lea r8, [local_name_ptr]
    mov r8, [r8 + rcx * 8]
    
    push rdi
    push rsi
    push rcx
    xor rax, rax
.lookup_cmp_loop:
    cmp rax, rsi
    jae .lookup_cmp_match
    mov dl, byte [r8 + rax]
    cmp dl, byte [rdi + rax]
    jne .lookup_cmp_nomatch
    inc rax
    jmp .lookup_cmp_loop
    
.lookup_cmp_match:
    pop rcx
    pop rsi
    pop rdi
    
    ; Found! Return value
    lea r8, [local_value_ptr]
    mov rax, [r8 + rcx * 8]
    lea r8, [local_value_len]
    mov rdx, [r8 + rcx * 8]
    
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

.lookup_cmp_nomatch:
    pop rcx
    pop rsi
    pop rdi

.lookup_next:
    dec rcx
    jmp .lookup_loop

.lookup_not_found:
    xor rax, rax
    xor rdx, rdx
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

count_array_items:
    push rbx
    push rcx
    push r8
    push r9

    xor r8, r8
    xor rcx, rcx
    mov r9, qword [local_count]

.cai_loop:
    cmp rcx, r9
    jae .cai_done

    mov rax, qword [local_name_len + rcx * 8]
    cmp rax, rsi
    jb .cai_next

    mov rbx, qword [local_name_ptr + rcx * 8]
    xor rdx, rdx
.cai_cmp:
    cmp rdx, rsi
    jae .cai_prefix_ok
    mov al, byte [rbx + rdx]
    cmp al, byte [rdi + rdx]
    jne .cai_next
    inc rdx
    jmp .cai_cmp

.cai_prefix_ok:
    mov al, byte [rbx + rsi]
    cmp al, '['
    jne .cai_next
    inc r8

.cai_next:
    inc rcx
    jmp .cai_loop

.cai_done:
    mov rax, r8
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

push_array_element:
    push rbx
    push rcx
    push r10
    push r11
    push r12
    push r13

    mov r12, rdi
    mov r13, rsi
    mov r10, r8
    mov r11, r9

    call count_array_items
    call write_u64_to_pool
    test rax, rax
    jz .pae_done

    mov r8, rax
    mov r9, rdx
    mov rdi, r12
    mov rsi, r13
    call build_index_key_to_pool

    mov rdi, rax
    mov rsi, rdx
    mov r8, r10
    mov r9, r11
    call local_set

.pae_done:
    pop r13
    pop r12
    pop r11
    pop r10
    pop rcx
    pop rbx
    ret

build_index_key:
    push rbx
    push rcx
    push r10

    lea r10, [index_key_buffer]
    xor rcx, rcx

    xor rbx, rbx
.bik_copy_base:
    cmp rbx, rsi
    jae .bik_after_base
    mov al, byte [rdi + rbx]
    mov byte [r10 + rcx], al
    inc rbx
    inc rcx
    jmp .bik_copy_base

.bik_after_base:
    mov byte [r10 + rcx], '['
    inc rcx

    xor rbx, rbx
.bik_copy_idx:
    cmp rbx, r9
    jae .bik_after_idx
    mov al, byte [r8 + rbx]
    mov byte [r10 + rcx], al
    inc rbx
    inc rcx
    jmp .bik_copy_idx

.bik_after_idx:
    mov byte [r10 + rcx], ']'
    inc rcx

    lea rax, [index_key_buffer]
    mov rdx, rcx

    pop r10
    pop rcx
    pop rbx
    ret

build_index_key_to_pool:
    push rbx
    push rcx
    push r10

    mov r10, qword [key_pool_pos]
    xor rcx, rcx

    xor rbx, rbx
.bikp_copy_base:
    cmp rbx, rsi
    jae .bikp_after_base
    mov al, byte [rdi + rbx]
    mov byte [key_pool + r10 + rcx], al
    inc rbx
    inc rcx
    jmp .bikp_copy_base

.bikp_after_base:
    mov byte [key_pool + r10 + rcx], '['
    inc rcx

    xor rbx, rbx
.bikp_copy_idx:
    cmp rbx, r9
    jae .bikp_after_idx
    mov al, byte [r8 + rbx]
    mov byte [key_pool + r10 + rcx], al
    inc rbx
    inc rcx
    jmp .bikp_copy_idx

.bikp_after_idx:
    mov byte [key_pool + r10 + rcx], ']'
    inc rcx

    lea rax, [key_pool + r10]
    mov rdx, rcx
    add qword [key_pool_pos], rcx

    pop r10
    pop rcx
    pop rbx
    ret

find_next_slash:
    push rcx

    mov rcx, rdi
.fns_loop:
    cmp rcx, r12
    jae .fns_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 35
    je .fns_ok
    inc rcx
    jmp .fns_loop

.fns_ok:
    mov rax, rcx
    pop rcx
    ret

.fns_fail:
    mov rax, -1
    pop rcx
    ret

find_matching_block_end:
    push rbx
    push rcx
    push rdx

    mov ebx, esi
    mov rcx, rdi
    inc rcx
    xor rdx, rdx

.fmb_loop:
    cmp rcx, r12
    jae .fmb_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, ebx
    jne .fmb_next

    mov rax, rcx
    inc rax
    cmp rax, r12
    jae .fmb_open
    mov eax, dword [r13 + rax * 4]
    cmp eax, 35
    jne .fmb_open

    test rdx, rdx
    jz .fmb_found
    dec rdx
    jmp .fmb_next

.fmb_open:
    inc rdx

.fmb_next:
    inc rcx
    jmp .fmb_loop

.fmb_found:
    mov rax, rcx
    pop rdx
    pop rcx
    pop rbx
    ret

.fmb_fail:
    mov rax, -1
    pop rdx
    pop rcx
    pop rbx
    ret

find_if_else_close:
    push rbx
    push rcx
    push r8

    mov rcx, rdi
    inc rcx
    xor r8, r8
    mov rbx, -1
    mov rdx, -1

.fiec_loop:
    cmp rcx, r12
    jae .fiec_done
    mov eax, dword [r13 + rcx * 4]

    cmp eax, 6
    je .fiec_if_token
    cmp eax, 8
    je .fiec_else_token
    jmp .fiec_next

.fiec_if_token:
    mov rax, rcx
    inc rax
    cmp rax, r12
    jae .fiec_nested_open
    mov eax, dword [r13 + rax * 4]
    cmp eax, 35
    jne .fiec_nested_open
    test r8, r8
    jz .fiec_found_close
    dec r8
    jmp .fiec_next

.fiec_nested_open:
    inc r8
    jmp .fiec_next

.fiec_else_token:
    mov rax, rcx
    inc rax
    cmp rax, r12
    jae .fiec_next
    mov eax, dword [r13 + rax * 4]
    cmp eax, 35
    jne .fiec_next
    test r8, r8
    jnz .fiec_next
    cmp rbx, -1
    jne .fiec_next
    mov rbx, rcx
    jmp .fiec_next

.fiec_found_close:
    mov rdx, rcx
    jmp .fiec_done

.fiec_next:
    inc rcx
    jmp .fiec_loop

.fiec_done:
    mov rax, rbx
    pop r8
    pop rcx
    pop rbx
    ret

resolve_condition_value:
    push rbx

    mov rbx, rdi
    cmp rbx, r12
    jae .rcv_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 16
    je .rcv_number
    cmp eax, 13
    je .rcv_ident
    jmp .rcv_fail

.rcv_number:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .rcv_fail
    mov rdx, 1
    pop rbx
    ret

.rcv_ident:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx

    cmp rsi, 4
    jne .rcv_ident_check_false
    cmp byte [rdi], 't'
    jne .rcv_ident_check_false
    cmp byte [rdi + 1], 'r'
    jne .rcv_ident_check_false
    cmp byte [rdi + 2], 'u'
    jne .rcv_ident_check_false
    cmp byte [rdi + 3], 'e'
    jne .rcv_ident_check_false
    mov rax, 1
    mov rdx, 1
    pop rbx
    ret

.rcv_ident_check_false:
    cmp rsi, 5
    jne .rcv_ident_lookup
    cmp byte [rdi], 'f'
    jne .rcv_ident_lookup
    cmp byte [rdi + 1], 'a'
    jne .rcv_ident_lookup
    cmp byte [rdi + 2], 'l'
    jne .rcv_ident_lookup
    cmp byte [rdi + 3], 's'
    jne .rcv_ident_lookup
    cmp byte [rdi + 4], 'e'
    jne .rcv_ident_lookup
    xor rax, rax
    mov rdx, 1
    pop rbx
    ret

.rcv_ident_lookup:
    call lookup_local
    test rax, rax
    jz .rcv_fail
    mov rdi, rax
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jnz .rcv_ok

    cmp rsi, 4
    jne .rcv_check_false
    cmp byte [rdi], 't'
    jne .rcv_check_false
    cmp byte [rdi + 1], 'r'
    jne .rcv_check_false
    cmp byte [rdi + 2], 'u'
    jne .rcv_check_false
    cmp byte [rdi + 3], 'e'
    jne .rcv_check_false
    mov rax, 1
    mov rdx, 1
    pop rbx
    ret

.rcv_check_false:
    cmp rsi, 5
    jne .rcv_fail
    cmp byte [rdi], 'f'
    jne .rcv_fail
    cmp byte [rdi + 1], 'a'
    jne .rcv_fail
    cmp byte [rdi + 2], 'l'
    jne .rcv_fail
    cmp byte [rdi + 3], 's'
    jne .rcv_fail
    cmp byte [rdi + 4], 'e'
    jne .rcv_fail
    xor rax, rax
    mov rdx, 1
    pop rbx
    ret

.rcv_ok:
    mov rdx, 1
    pop rbx
    ret

.rcv_fail:
    xor rax, rax
    xor rdx, rdx
    pop rbx
    ret

eval_condition_simple:
    push rbx
    push rcx
    push r8
    push r9
    push r10

    mov rbx, rdi
    cmp rbx, r12
    jae .ecs_fail

    mov rdi, rbx
    call resolve_condition_value
    test rdx, rdx
    jz .ecs_fail
    mov r8, rax

    mov rcx, rbx
    inc rcx
    cmp rcx, r12
    jae .ecs_fail
    mov r10d, dword [r13 + rcx * 4]

    cmp r10d, 35
    je .ecs_unary

    cmp r10d, 28
    je .ecs_binary
    cmp r10d, 26
    je .ecs_binary
    cmp r10d, 23
    je .ecs_binary
    cmp r10d, 24
    je .ecs_binary
    jmp .ecs_fail

.ecs_binary:
    mov rdi, rbx
    add rdi, 2
    call resolve_condition_value
    test rdx, rdx
    jz .ecs_fail
    mov r9, rax

    cmp r10d, 28
    je .ecs_gt
    cmp r10d, 26
    je .ecs_lt
    cmp r10d, 23
    je .ecs_eq

    cmp r8, r9
    jne .ecs_true
    jmp .ecs_false

.ecs_gt:
    cmp r8, r9
    ja .ecs_true
    jmp .ecs_false

.ecs_lt:
    cmp r8, r9
    jb .ecs_true
    jmp .ecs_false

.ecs_eq:
    cmp r8, r9
    je .ecs_true
    jmp .ecs_false

.ecs_unary:
    test r8, r8
    jnz .ecs_true
    jmp .ecs_false

.ecs_true:
    mov rax, 1
    jmp .ecs_done

.ecs_false:
    xor rax, rax
    jmp .ecs_done

.ecs_fail:
    xor rax, rax

.ecs_done:
    pop r10
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

evaluate_simple_expr:
    push rbx
    push rcx
    push r8
    push r9
    push r10
    push r11

    mov rbx, rdi
    cmp rbx, [syntax_token_count]
    jae .eval_fail

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 17
    je .eval_group_mul
    cmp eax, 25
    je .eval_not_bool
    cmp eax, 16
    je .eval_num_bin
    cmp eax, 13
    je .eval_ident_entry
    jmp .eval_fail

.eval_group_mul:
    mov rcx, rbx
    add rcx, 7
    call .eval_delim_check
    test rax, rax
    jz .eval_fail

    mov rcx, rbx
    inc rcx
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .eval_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .eval_fail
    mov r8, rax

    mov rcx, rbx
    add rcx, 2
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 30
    jne .eval_fail

    mov rcx, rbx
    add rcx, 3
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .eval_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .eval_fail
    mov r9, rax

    mov rcx, rbx
    add rcx, 4
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 18
    jne .eval_fail

    mov rcx, rbx
    add rcx, 5
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 32
    jne .eval_fail

    mov rcx, rbx
    add rcx, 6
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .eval_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .eval_fail

    add r8, r9
    imul r8, rax
    mov rax, r8
    call write_u64_to_pool
    test rax, rax
    jz .eval_fail
    jmp .eval_ok

.eval_ident_entry:
    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .eval_bool_bin
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 17
    je .eval_check_builtin
    jmp .eval_bool_bin

.eval_check_builtin:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    
    cmp rsi, 3
    je .eval_check_len
    cmp rsi, 4
    je .eval_check_push
    
    jmp .eval_fn_call
    
.eval_check_len:
    cmp byte [rdi], 'l'
    jne .eval_fn_call
    cmp byte [rdi + 1], 'e'
    jne .eval_fn_call
    cmp byte [rdi + 2], 'n'
    jne .eval_fn_call
    
    jmp .eval_builtin_len
    
.eval_check_push:
    cmp byte [rdi], 'p'
    jne .eval_fn_call
    cmp byte [rdi + 1], 'u'
    jne .eval_fn_call
    cmp byte [rdi + 2], 's'
    jne .eval_fn_call
    cmp byte [rdi + 3], 'h'
    jne .eval_fn_call
    
    jmp .eval_fn_call

.eval_builtin_len:
    mov rdi, rbx
    call builtin_len_function
    test rax, rax
    jz .eval_fail
    jmp .eval_ok

.eval_fn_call:
    mov rdi, rbx
    call evaluate_function_call
    test rax, rax
    jz .eval_fail
    jmp .eval_ok

.eval_not_bool:
    mov rcx, rbx
    add rcx, 2
    call .eval_delim_check
    test rax, rax
    jz .eval_fail

    mov rcx, rbx
    inc rcx
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .eval_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_bool_ident
    test rdx, rdx
    jz .eval_fail

    cmp al, 0
    je .eval_true
    lea rax, [lit_false]
    mov rdx, lit_false_len
    jmp .eval_ok

.eval_num_bin:
    mov rcx, rbx
    add rcx, 3
    call .eval_delim_check
    test rax, rax
    jz .eval_fail

    mov rcx, rbx
    inc rcx
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 30
    je .eval_num_op_ok
    cmp eax, 32
    je .eval_num_op_ok
    cmp eax, 28
    je .eval_num_op_ok
    jmp .eval_fail

.eval_num_op_ok:
    mov r10d, eax

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .eval_fail
    mov r8, rax

    mov rcx, rbx
    add rcx, 2
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .eval_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .eval_fail
    mov r9, rax

    cmp r10d, 30
    je .eval_add
    cmp r10d, 32
    je .eval_mul
    jmp .eval_gt

.eval_add:
    mov rax, r8
    add rax, r9
    call write_u64_to_pool
    test rax, rax
    jz .eval_fail
    jmp .eval_ok

.eval_mul:
    mov rax, r8
    imul rax, r9
    call write_u64_to_pool
    test rax, rax
    jz .eval_fail
    jmp .eval_ok

.eval_gt:
    cmp r8, r9
    ja .eval_true
    jmp .eval_false

.eval_bool_bin:
    mov rcx, rbx
    add rcx, 3
    call .eval_delim_check
    test rax, rax
    jz .eval_fail

    mov rcx, rbx
    inc rcx
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 33
    je .eval_bool_op_ok
    cmp eax, 34
    je .eval_bool_op_ok
    jmp .eval_fail

.eval_bool_op_ok:
    mov r10d, eax

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_bool_ident
    test rdx, rdx
    jz .eval_fail
    mov r8b, al

    mov rcx, rbx
    add rcx, 2
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .eval_fail
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_bool_ident
    test rdx, rdx
    jz .eval_fail
    mov r9b, al

    cmp r10d, 33
    je .eval_and

    mov al, r8b
    or al, r9b
    test al, al
    jnz .eval_true
    jmp .eval_false

.eval_and:
    mov al, r8b
    and al, r9b
    test al, al
    jnz .eval_true
    jmp .eval_false

.eval_true:
    lea rax, [lit_true]
    mov rdx, lit_true_len
    jmp .eval_ok

.eval_false:
    lea rax, [lit_false]
    mov rdx, lit_false_len
    jmp .eval_ok

.eval_delim_check:
    cmp rcx, [syntax_token_count]
    jae .eval_delim_ok
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 36
    je .eval_delim_ok
    cmp eax, 35
    je .eval_delim_ok
    cmp eax, 18
    je .eval_delim_ok
    cmp eax, 21
    je .eval_delim_ok
    cmp eax, 20
    je .eval_delim_ok
    xor rax, rax
    ret

.eval_delim_ok:
    mov rax, 1
    ret

.eval_fail:
    xor rax, rax
    xor rdx, rdx

.eval_ok:
    pop r11
    pop r10
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

parse_bool_ident:
    cmp rsi, 4
    jne .parse_bool_false_check
    cmp byte [rdi], 't'
    jne .parse_bool_false_check
    cmp byte [rdi + 1], 'r'
    jne .parse_bool_false_check
    cmp byte [rdi + 2], 'u'
    jne .parse_bool_false_check
    cmp byte [rdi + 3], 'e'
    jne .parse_bool_false_check
    mov al, 1
    mov rdx, 1
    ret

.parse_bool_false_check:
    cmp rsi, 5
    jne .parse_bool_fail
    cmp byte [rdi], 'f'
    jne .parse_bool_fail
    cmp byte [rdi + 1], 'a'
    jne .parse_bool_fail
    cmp byte [rdi + 2], 'l'
    jne .parse_bool_fail
    cmp byte [rdi + 3], 's'
    jne .parse_bool_fail
    cmp byte [rdi + 4], 'e'
    jne .parse_bool_fail
    xor al, al
    mov rdx, 1
    ret

.parse_bool_fail:
    xor rax, rax
    xor rdx, rdx
    ret

evaluate_function_call:
    push rbx
    push rcx
    push r8
    push r9
    push r10
    push r11
    push r12

    mov rbx, rdi

    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea r10, [source_buffer + rax]
    mov r11, rdx

    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 17
    jne .fn_fail

    inc rcx
    xor r8, r8
    xor r9, r9
    xor r12, r12

    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 18
    je .fn_after_args

    cmp eax, 16
    jne .fn_fail
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .fn_fail
    mov r8, rax
    mov r12, 1

    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 21
    jne .fn_expect_rparen

    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .fn_fail
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .fn_fail
    mov r9, rax
    mov r12, 2

    inc rcx

.fn_expect_rparen:
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 18
    jne .fn_fail

.fn_after_args:
    mov rcx, rbx
    call find_function_def
    test rax, rax
    jz .fn_fail
    mov rbx, rax

    mov rcx, rbx
.fn_find_return:
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 1
    je .fn_fail
    cmp eax, 5
    je .fn_have_return
    inc rcx
    jmp .fn_find_return

.fn_have_return:
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]

    cmp eax, 16
    je .fn_return_number
    cmp eax, 13
    je .fn_return_ident_expr
    jmp .fn_fail

.fn_return_number:
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call parse_u64
    test rdx, rdx
    jz .fn_fail
    call write_u64_to_pool
    test rax, rax
    jz .fn_fail
    jmp .fn_ok

.fn_return_ident_expr:
    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call map_param_ab
    test rdx, rdx
    jz .fn_fail
    mov rbx, rax

    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 30
    jne .fn_fail

    inc rcx
    cmp rcx, [syntax_token_count]
    jae .fn_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .fn_fail

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call map_param_ab
    test rdx, rdx
    jz .fn_fail
    mov rcx, rax

    cmp rbx, 0
    jne .fn_left_is_p1
    mov rax, r8
    jmp .fn_have_left
.fn_left_is_p1:
    mov rax, r9
.fn_have_left:
    cmp rcx, 0
    jne .fn_right_is_p1
    mov rdx, r8
    jmp .fn_have_right
.fn_right_is_p1:
    mov rdx, r9
.fn_have_right:
    add rax, rdx
    call write_u64_to_pool
    test rax, rax
    jz .fn_fail
    jmp .fn_ok

.fn_fail:
    xor rax, rax
    xor rdx, rdx

.fn_ok:
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

find_function_def:
    push rbx
    push rcx
    push r8
    push r9

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea r8, [source_buffer + rax]
    mov r9, rdx

    xor rbx, rbx
.ffd_loop:
    cmp rbx, [syntax_token_count]
    jae .ffd_fail
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 1
    jne .ffd_next

    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .ffd_next
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 2
    jne .ffd_next

    inc rcx
    cmp rcx, [syntax_token_count]
    jae .ffd_next
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 13
    jne .ffd_next

    mov rax, qword [r14 + rcx * 8]
    mov rdx, qword [r15 + rcx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    mov rax, r8
    mov rdx, r9
    call compare_ptr_len
    test rax, rax
    jz .ffd_next

    mov rax, rcx
    jmp .ffd_ok

.ffd_next:
    inc rbx
    jmp .ffd_loop

.ffd_fail:
    xor rax, rax

.ffd_ok:
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

builtin_len_function:
    push rbx
    push r8
    push r9
    push r10
    
    mov rbx, rdi
    
    inc rbx
    cmp rbx, [syntax_token_count]
    jae .builtin_len_fail
    
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 17
    jne .builtin_len_fail
    
    inc rbx
    cmp rbx, [syntax_token_count]
    jae .builtin_len_fail
    
    mov eax, dword [r13 + rbx * 4]
    cmp eax, 13
    jne .builtin_len_fail
    
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx
    call count_array_items
    call write_u64_to_pool
    test rax, rax
    jz .builtin_len_fail
    jmp .builtin_len_ok
    
.builtin_len_ok:
    pop r10
    pop r9
    pop r8
    pop rbx
    ret
    
.builtin_len_fail:
    xor rax, rax
    pop r10
    pop r9
    pop r8
    pop rbx
    ret

map_param_ab:
    cmp rsi, 1
    jne .mp_fail
    cmp byte [rdi], 'a'
    je .mp_a
    cmp byte [rdi], 'b'
    je .mp_b
    jmp .mp_fail

.mp_a:
    xor rax, rax
    mov rdx, 1
    ret

.mp_b:
    mov rax, 1
    mov rdx, 1
    ret

.mp_fail:
    xor rax, rax
    xor rdx, rdx
    ret

compare_ptr_len:
    push rcx
    push r8

    cmp rsi, rdx
    jne .cpl_no
    xor rcx, rcx
.cpl_loop:
    cmp rcx, rsi
    jae .cpl_yes
    mov r8b, byte [rdi + rcx]
    cmp r8b, byte [rax + rcx]
    jne .cpl_no
    inc rcx
    jmp .cpl_loop

.cpl_yes:
    mov rax, 1
    jmp .cpl_done

.cpl_no:
    xor rax, rax

.cpl_done:
    pop r8
    pop rcx
    ret

resolve_simple_value:
    push rbx
    push rcx
    push r8
    push r9

    mov rbx, rdi
    cmp rbx, [syntax_token_count]
    jae .resolve_fail

    mov eax, dword [r13 + rbx * 4]
    cmp eax, 14
    je .resolve_string
    cmp eax, 16
    je .resolve_number
    cmp eax, 15
    je .resolve_char
    cmp eax, 13
    je .resolve_ident
    cmp eax, 31
    je .resolve_negative
    jmp .resolve_fail

.resolve_string:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rax, [source_buffer + rax + 1]
    sub rdx, 2
    jmp .resolve_ok

.resolve_char:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rax, [source_buffer + rax + 1]
    sub rdx, 2
    jmp .resolve_ok

.resolve_number:
    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .resolve_number_ok
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 36
    je .resolve_number_ok
    cmp eax, 35
    je .resolve_number_ok
    cmp eax, 18
    je .resolve_number_ok
    cmp eax, 21
    je .resolve_number_ok
    cmp eax, 20
    je .resolve_number_ok
    jmp .resolve_fail

.resolve_number_ok:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rax, [source_buffer + rax]
    jmp .resolve_ok

.resolve_ident:
    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .resolve_ident_ok
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 36
    je .resolve_ident_ok
    cmp eax, 35
    je .resolve_ident_ok
    cmp eax, 18
    je .resolve_ident_ok
    cmp eax, 21
    je .resolve_ident_ok
    cmp eax, 20
    je .resolve_ident_ok
    jmp .resolve_fail

.resolve_ident_ok:
    mov rax, qword [r14 + rbx * 8]
    mov rdx, qword [r15 + rbx * 8]
    lea rdi, [source_buffer + rax]
    mov rsi, rdx

    cmp rsi, 4
    jne .resolve_check_false
    cmp byte [rdi], 't'
    jne .resolve_check_false
    cmp byte [rdi + 1], 'r'
    jne .resolve_check_false
    cmp byte [rdi + 2], 'u'
    jne .resolve_check_false
    cmp byte [rdi + 3], 'e'
    jne .resolve_check_false
    lea rax, [lit_true]
    mov rdx, lit_true_len
    jmp .resolve_ok

.resolve_check_false:
    cmp rsi, 5
    jne .resolve_ident_lookup
    cmp byte [rdi], 'f'
    jne .resolve_ident_lookup
    cmp byte [rdi + 1], 'a'
    jne .resolve_ident_lookup
    cmp byte [rdi + 2], 'l'
    jne .resolve_ident_lookup
    cmp byte [rdi + 3], 's'
    jne .resolve_ident_lookup
    cmp byte [rdi + 4], 'e'
    jne .resolve_ident_lookup
    lea rax, [lit_false]
    mov rdx, lit_false_len
    jmp .resolve_ok

.resolve_ident_lookup:
    call lookup_local
    test rax, rax
    jz .resolve_fail
    jmp .resolve_ok

.resolve_negative:
    mov rcx, rbx
    inc rcx
    cmp rcx, [syntax_token_count]
    jae .resolve_fail
    mov eax, dword [r13 + rcx * 4]
    cmp eax, 16
    jne .resolve_fail

    mov r8, qword [r14 + rcx * 8]
    mov r9, qword [r15 + rcx * 8]
    mov rax, rcx
    inc rax
    cmp rax, [syntax_token_count]
    jae .resolve_fail
    mov eax, dword [r13 + rax * 4]
    cmp eax, 36
    je .resolve_negative_ok
    cmp eax, 35
    je .resolve_negative_ok
    cmp eax, 18
    je .resolve_negative_ok
    cmp eax, 21
    je .resolve_negative_ok
    jmp .resolve_fail

.resolve_negative_ok:
    mov rcx, [value_pool_pos]
    mov rax, rcx
    add rax, r9
    add rax, 1
    cmp rax, 8192
    ja .resolve_fail

    lea rax, [value_pool + rcx]
    mov byte [rax], '-'
    lea rdi, [source_buffer + r8]
    lea rsi, [rax + 1]
    xor r8, r8
.resolve_neg_copy:
    cmp r8, r9
    jae .resolve_neg_done
    mov dl, byte [rdi + r8]
    mov byte [rsi + r8], dl
    inc r8
    jmp .resolve_neg_copy

.resolve_neg_done:
    mov rdx, r9
    inc rdx
    add rcx, rdx
    mov [value_pool_pos], rcx
    jmp .resolve_ok

.resolve_fail:
    xor rax, rax
    xor rdx, rdx

.resolve_ok:
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

local_set:
    push rbx
    push rcx
    push r12
    push r10
    push r11

    mov r10, rdi
    mov r11, rsi
    mov rbx, [local_count]
    xor rcx, rcx

.local_set_find:
    cmp rcx, rbx
    jae .local_set_append

    lea rax, [local_name_len]
    mov rdx, [rax + rcx * 8]
    cmp rdx, r11
    jne .local_set_next

    lea rax, [local_name_ptr]
    mov rax, [rax + rcx * 8]
    xor rdx, rdx
.local_set_cmp:
    cmp rdx, r11
    jae .local_set_update
    mov r12b, byte [rax + rdx]
    cmp r12b, byte [r10 + rdx]
    jne .local_set_next
    inc rdx
    jmp .local_set_cmp

.local_set_update:
    lea rax, [local_value_ptr]
    mov [rax + rcx * 8], r8
    lea rax, [local_value_len]
    mov [rax + rcx * 8], r9
    jmp .local_set_done

.local_set_next:
    inc rcx
    jmp .local_set_find

.local_set_append:
    cmp rbx, 32
    jae .local_set_done
    lea rax, [local_name_ptr]
    mov [rax + rbx * 8], r10
    lea rax, [local_name_len]
    mov [rax + rbx * 8], r11
    lea rax, [local_value_ptr]
    mov [rax + rbx * 8], r8
    lea rax, [local_value_len]
    mov [rax + rbx * 8], r9
    inc qword [local_count]

.local_set_done:
    pop r11
    pop r10
    pop r12
    pop rcx
    pop rbx
    ret

parse_u64:
    push rbx
    push rcx

    xor rax, rax
    xor rcx, rcx
    test rsi, rsi
    jz .parse_u64_fail

.parse_u64_loop:
    cmp rcx, rsi
    jae .parse_u64_ok
    mov bl, byte [rdi + rcx]
    cmp bl, '0'
    jb .parse_u64_fail
    cmp bl, '9'
    ja .parse_u64_fail
    imul rax, rax, 10
    sub bl, '0'
    movzx rbx, bl
    add rax, rbx
    inc rcx
    jmp .parse_u64_loop

.parse_u64_ok:
    mov rdx, 1
    pop rcx
    pop rbx
    ret

.parse_u64_fail:
    xor rax, rax
    xor rdx, rdx
    pop rcx
    pop rbx
    ret

write_u64_to_pool:
    push rbx
    push rcx
    push r8
    push r9

    lea r8, [format_buffer]
    xor rcx, rcx
    mov rbx, rax
    cmp rbx, 0
    jne .write_u64_digits
    mov byte [r8], '0'
    mov rcx, 1
    jmp .write_u64_emit

.write_u64_digits:
    xor rdx, rdx
.write_u64_div:
    xor rdx, rdx
    mov rax, rbx
    mov r9, 10
    div r9
    add dl, '0'
    mov byte [r8 + rcx], dl
    inc rcx
    mov rbx, rax
    test rbx, rbx
    jnz .write_u64_div

.write_u64_emit:
    mov r9, [value_pool_pos]
    mov rdx, r9
    add rdx, rcx
    cmp rdx, 8192
    ja .write_u64_fail

    lea rax, [value_pool + r9]
    xor rdx, rdx
.write_u64_copy:
    cmp rdx, rcx
    jae .write_u64_done
    mov rbx, rcx
    sub rbx, rdx
    dec rbx
    mov bl, byte [r8 + rbx]
    mov byte [rax + rdx], bl
    inc rdx
    jmp .write_u64_copy

.write_u64_done:
    mov r8, [value_pool_pos]
    add r8, rcx
    mov [value_pool_pos], r8
    mov rdx, rcx
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

.write_u64_fail:
    xor rax, rax
    xor rdx, rdx
    pop r9
    pop r8
    pop rcx
    pop rbx
    ret

; Helper: Format string with single {} placeholder
; Input: rdi = template ptr, rsi = template len, 
;        r8 = value ptr, r9 = value len
; Output: rax = formatted string ptr, rdx = formatted string len
format_placeholder:
    push rbx
    push rcx
    push r10
    push r11
    
    ; Safety check: if value len > 200, something is wrong - don't substitute
    cmp r9, 200
    ja .no_placeholder_found
    
    ;  Find {} in template
    xor rbx, rbx
    xor rcx, rcx  ; position of {
.find_placeholder:
    cmp rbx, rsi
    jae .no_placeholder_found
    
    cmp byte [rdi + rbx], '{'
    jne .find_next
    
    ; Check if next char is }
    inc rbx
    cmp rbx, rsi
    jae .no_placeholder_found
    cmp byte [rdi + rbx], '}'
    je .found_placeholder
    dec rbx
    
.find_next:
    inc rbx
    jmp .find_placeholder

.found_placeholder:
    ; rcx = position before {
    mov rcx, rbx
    dec rcx
    
    ; Copy everything before {}
    lea r10, [format_buffer]
    xor r11, r11
    
    ; Copy prefix
    xor rax, rax
.copy_prefix:
    cmp rax, rcx
    jae .prefix_done
    mov dl, byte [rdi + rax]
    mov byte [r10 + r11], dl
    inc rax
    inc r11
    jmp .copy_prefix

.prefix_done:
    ; Copy value
    xor rax, rax
.copy_value:
    cmp rax, r9
    jae .value_done
    mov dl, byte [r8 + rax]
    mov byte [r10 + r11], dl
    inc rax
    inc r11
    jmp .copy_value

.value_done:
    ; Copy suffix (after })
    inc rbx  ; skip the }
    mov rax, rbx
.copy_suffix:
    cmp rax, rsi
    jae .format_done
    mov dl, byte [rdi + rax]
    mov byte [r10 + r11], dl
    inc rax
    inc r11
    jmp .copy_suffix

.format_done:
    lea rax, [format_buffer]
    mov rdx, r11
    
    pop r11
    pop r10
    pop rcx
    pop rbx
    ret

.no_placeholder_found:
    ; No placeholder - just return original
    mov rax, rdi
    mov rdx, rsi
    pop r11
    pop r10
    pop rcx
    pop rbx
    ret

; Helper: Format string with multiple {} placeholders
; Input: rdi = template ptr, rsi = template len
;        arg_count, arg_value_ptr[], arg_value_len[] arrays
; Output: rax = formatted string ptr, rdx = formatted string len
format_multiple_placeholders:
    push rbx
    push r12
    push r13
    push r14
    push r15
    
    ; r12 = current arg index
    ; r13 = output position in format_buffer
    ; r14 = input position in template
    ; r15 = arg_count
    
    xor r12, r12  ; arg index
    xor r13, r13  ; output pos
    xor r14, r14  ; input pos
    mov r15, [arg_count]
    
    lea rbx, [format_buffer]
    
.fmt_multi_loop:
    cmp r14, rsi  ; reached end of template?
    jae .fmt_multi_done
    
    ; Check if current char is {
    mov al, byte [rdi + r14]
    cmp al, '{'
    jne .fmt_multi_copy_char
    
    ; Check if next char is }
    mov rcx, r14
    inc rcx
    cmp rcx, rsi
    jae .fmt_multi_copy_char
    
    mov al, byte [rdi + rcx]
    cmp al, '}'
    jne .fmt_multi_copy_char
    
    ; Found {} - substitute with next argument
    cmp r12, r15  ; do we have more args?
    jae .fmt_multi_skip_placeholder
    
    ; Copy the argument value
    push rdi
    push rsi
    
    lea rax, [arg_value_ptr]
    mov rdi, [rax + r12 * 8]  ; arg value ptr
    lea rax, [arg_value_len]
    mov rsi, [rax + r12 * 8]  ; arg value len
    
    ; Copy value bytes
    xor rcx, rcx
.fmt_multi_copy_arg:
    cmp rcx, rsi
    jae .fmt_multi_arg_done
    mov al, byte [rdi + rcx]
    mov byte [rbx + r13], al
    inc rcx
    inc r13
    jmp .fmt_multi_copy_arg
    
.fmt_multi_arg_done:
    pop rsi
    pop rdi
    inc r12  ; move to next arg
    
    ; Skip past the {}
    inc r14
    inc r14
    jmp .fmt_multi_loop

.fmt_multi_skip_placeholder:
    ; No more args - leave {} as-is
    mov al, '{'
    mov byte [rbx + r13], al
    inc r13
    inc r14
    jmp .fmt_multi_loop
    
.fmt_multi_copy_char:
    ; Regular character - copy it
    mov al, byte [rdi + r14]
    mov byte [rbx + r13], al
    inc r13
    inc r14
    jmp .fmt_multi_loop

.fmt_multi_done:
    lea rax, [format_buffer]
    mov rdx, r13
    
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
