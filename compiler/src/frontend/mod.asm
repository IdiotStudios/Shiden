default rel

%define TOK_FN        1
%define TOK_NEW       2
%define TOK_LET       3
%define TOK_MUT       4
%define TOK_RETURN    5
%define TOK_IF        6
%define TOK_WHILE     7
%define TOK_ELSE      8
%define TOK_BREAK     9
%define TOK_CONTINUE  10
%define TOK_FOR       11
%define TOK_IN        12
%define TOK_IDENT     13
%define TOK_SLASH     35
%define TOK_LPAREN    17
%define TOK_RPAREN    18
%define TOK_COMMA     21
%define TOK_TYPE      36

section .data
    demo_src db "fn new main/", 0xA, "    println()" , "/" , 0xA, "fn/", 0xA
    demo_src_len equ $ - demo_src

section .text
    global frontend_parse
    global frontend_parse_buffer
    global frontend_parse_tokens
    extern syntax_lex_buffer
    extern syntax_token_count
    extern syntax_tokens_kind

frontend_parse:
    lea rdi, [demo_src]
    mov rsi, demo_src_len
    call frontend_parse_buffer
    ret

frontend_parse_buffer:
    call syntax_lex_buffer
    test rax, rax
    jnz fpb_parse_fail
    jmp frontend_parse_tokens

fpb_parse_fail:
    mov rax, 1
    ret

frontend_parse_tokens:
    push rbx
    push r12
    push r13
    push r14

    mov r12, [rel syntax_token_count]
    lea r10, [rel syntax_tokens_kind]
    xor r13, r13
    xor rbx, rbx
    xor rcx, rcx

fpt_parse_loop:
    cmp rcx, r12
    jae fpt_parse_done

    mov eax, [r10 + rcx * 4]
    cmp eax, TOK_FN
    je fpt_check_fn_type

    cmp eax, TOK_LET
    je fpt_stmt_let

    cmp eax, TOK_RETURN
    je fpt_stmt_return

    cmp eax, TOK_IF
    je fpt_stmt_if

    cmp eax, TOK_WHILE
    je fpt_stmt_while

    cmp eax, TOK_FOR
    je fpt_stmt_for

    cmp eax, TOK_BREAK
    je fpt_stmt_terminal

    cmp eax, TOK_CONTINUE
    je fpt_stmt_terminal

    cmp eax, TOK_IDENT
    je fpt_stmt_call_or_assign

    jmp fpt_advance_one

fpt_check_fn_type:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_NEW
    je fpt_open_fn
    cmp edx, TOK_SLASH
    je fpt_close_fn
    jmp fpt_parse_fail

fpt_open_fn:
    lea r8, [rcx + 2]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_IDENT
    jne fpt_parse_fail

    lea r8, [rcx + 3]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_LPAREN
    je fpt_parse_params
    cmp edx, TOK_SLASH
    je fpt_parse_fn_no_params

    jmp fpt_parse_fail

fpt_parse_fn_no_params:
    inc r13
    mov rbx, 1
    add rcx, 4
    jmp fpt_parse_loop

fpt_parse_params:
    lea r8, [rcx + 4]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_RPAREN
    je fpt_params_valid

    mov r14, rcx
    add r14, 4

fpt_params_loop:
    cmp r14, r12
    jae fpt_parse_fail
    mov edx, [r10 + r14 * 4]
    cmp edx, TOK_RPAREN
    je fpt_params_valid
    cmp edx, TOK_COMMA
    je fpt_params_comma
    cmp edx, TOK_IDENT
    je fpt_params_next
    jmp fpt_parse_fail

fpt_params_comma:
    add r14, 1
    jmp fpt_params_loop

fpt_params_next:
    add r14, 1
    jmp fpt_params_loop

fpt_params_valid:
    add r14, 1
    cmp r14, r12
    jae fpt_parse_fail
    mov edx, [r10 + r14 * 4]
    cmp edx, TOK_SLASH
    jne fpt_parse_fail
    inc r13
    mov rbx, 1
    mov rcx, r14
    inc rcx
    jmp fpt_parse_loop

fpt_close_fn:
    test r13, r13
    jz fpt_parse_fail
    dec r13
    add rcx, 2
    jmp fpt_parse_loop

fpt_stmt_let:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    
    cmp edx, TOK_MUT
    je fpt_let_mut
    cmp edx, TOK_IDENT
    je fpt_let_ok
    jmp fpt_parse_fail

fpt_let_mut:
    lea r8, [rcx + 2]
    cmp r8, r12
    jae fpt_parse_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_IDENT
    jne fpt_parse_fail
    add rcx, 3
    jmp fpt_let_skip

fpt_let_ok:
    add rcx, 2

fpt_let_skip:
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    jmp fpt_parse_loop

fpt_stmt_return:
    add rcx, 1
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    jmp fpt_parse_loop

fpt_stmt_if:
    add rcx, 1
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    call fpt_skip_if_block
    test rax, rax
    jnz fpt_parse_fail
    jmp fpt_parse_loop

fpt_stmt_while:
    add rcx, 1
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    call fpt_skip_while_block
    test rax, rax
    jnz fpt_parse_fail
    jmp fpt_parse_loop

fpt_stmt_for:
    add rcx, 1
    cmp rcx, r12
    jae fpt_parse_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_IDENT
    jne fpt_parse_fail
    add rcx, 1
    cmp rcx, r12
    jae fpt_parse_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_IN
    jne fpt_parse_fail
    add rcx, 1
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    call fpt_skip_for_block
    test rax, rax
    jnz fpt_parse_fail
    jmp fpt_parse_loop

fpt_stmt_terminal:
    add rcx, 1
    cmp rcx, r12
    jae fpt_parse_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_SLASH
    jne fpt_parse_fail
    add rcx, 1
    jmp fpt_parse_loop

fpt_stmt_call_or_assign:
    add rcx, 1
    cmp rcx, r12
    jae fpt_parse_fail
    call fpt_skip_to_term
    test rax, rax
    jnz fpt_parse_fail
    inc rcx
    jmp fpt_parse_loop

fpt_skip_if_block:
    cmp rcx, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_IF
    je fpt_skip_if_close
    cmp edx, TOK_ELSE
    je fpt_skip_else_block
    add rcx, 1
    jmp fpt_skip_if_block

fpt_skip_if_close:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_SLASH
    jne fpt_skip_if_block
    add rcx, 2
    xor rax, rax
    ret

fpt_skip_else_block:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_SLASH
    jne fpt_skip_if_block
    add rcx, 2
    jmp fpt_skip_if_block

fpt_skip_while_block:
    cmp rcx, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_WHILE
    je fpt_skip_while_close
    add rcx, 1
    jmp fpt_skip_while_block

fpt_skip_while_close:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_SLASH
    jne fpt_skip_while_block
    add rcx, 2
    xor rax, rax
    ret

fpt_skip_for_block:
    cmp rcx, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_FOR
    je fpt_skip_for_close
    add rcx, 1
    jmp fpt_skip_for_block

fpt_skip_for_close:
    lea r8, [rcx + 1]
    cmp r8, r12
    jae fpt_skip_block_fail
    mov edx, [r10 + r8 * 4]
    cmp edx, TOK_SLASH
    jne fpt_skip_for_block
    add rcx, 2
    xor rax, rax
    ret

fpt_skip_block_fail:
    mov rax, 1
    ret

fpt_skip_to_term:
    cmp rcx, r12
    jae fpt_skip_fail
    mov edx, [r10 + rcx * 4]
    cmp edx, TOK_SLASH
    je fpt_skip_found
    cmp edx, TOK_TYPE
    je fpt_skip_found
    add rcx, 1
    jmp fpt_skip_to_term

fpt_skip_found:
    xor rax, rax
    ret

fpt_skip_fail:
    mov rax, 1
    ret

fpt_advance_one:
    inc rcx
    jmp fpt_parse_loop

fpt_parse_done:
    test rbx, rbx
    jz fpt_parse_fail
    test r13, r13
    jnz fpt_parse_fail
    xor rax, rax
    mov rdx, r12
    jmp fpt_parse_return

fpt_parse_fail:
    mov rax, 1

fpt_parse_return:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
