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
%define TOK_STRING    14
%define TOK_CHAR      15
%define TOK_NUMBER    16
%define TOK_LPAREN    17
%define TOK_RPAREN    18
%define TOK_LBRACKET  19
%define TOK_RBRACKET  20
%define TOK_COMMA     21
%define TOK_EQUAL     22
%define TOK_EQEQ      23
%define TOK_NOTEQ     24
%define TOK_BANG      25
%define TOK_LESS      26
%define TOK_LESSEQ    27
%define TOK_GREATER   28
%define TOK_GREATEREQ 29
%define TOK_PLUS      30
%define TOK_MINUS     31
%define TOK_STAR      32
%define TOK_ANDAND    33
%define TOK_OROR      34
%define TOK_SLASH     35
%define TOK_TYPE      36
%define TOK_SLASHOP   37
%define MAX_TOKENS    8192

section .data
    demo_src db "fn new main/", 0xA, "    let n = 1/i64", 0xA, "fn/", 0xA
    demo_src_len equ $ - demo_src
    syntax_source_ptr dq 0
    syntax_source_len dq 0
    syntax_token_count dq 0

section .bss
    syntax_tokens_kind resd MAX_TOKENS
    syntax_tokens_start resq MAX_TOKENS
    syntax_tokens_len resq MAX_TOKENS

section .text
    global syntax_lex
    global syntax_lex_buffer
    global syntax_source_ptr
    global syntax_source_len
    global syntax_token_count
    global syntax_tokens_kind
    global syntax_tokens_start
    global syntax_tokens_len

syntax_lex:
    lea rdi, [demo_src]
    mov rsi, demo_src_len
    call syntax_lex_buffer
    ret

syntax_lex_buffer:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi
    mov r13, rsi
    xor r14, r14
    mov qword [rel syntax_source_ptr], r12
    mov qword [rel syntax_source_len], r13
    mov qword [rel syntax_token_count], 0

.next_token:
    cmp r14, r13
    jae .lex_ok

    mov rbx, r14

    mov dl, [r12 + r14]
    mov al, dl

    cmp al, ' '
    je .ws_advance
    cmp al, 9
    je .ws_advance
    cmp al, 10
    je .ws_advance
    cmp al, 13
    je .ws_advance

    mov al, dl
    call .is_ident_start
    test rax, rax
    jnz .lex_ident

    mov al, dl
    call .is_digit
    test rax, rax
    jnz .lex_number

    mov al, dl
    cmp al, '"'
    je .lex_string
    cmp al, 39
    je .lex_char

    cmp al, '('
    je .single_token
    cmp al, ')'
    je .single_token
    cmp al, '['
    je .single_token
    cmp al, ']'
    je .single_token
    cmp al, ','
    je .single_token
    cmp al, '+'
    je .single_token
    cmp al, '-'
    je .single_token
    cmp al, '*'
    je .single_token

    cmp al, '/'
    je .lex_slash
    cmp al, '='
    je .lex_eq
    cmp al, '!'
    je .lex_bang
    cmp al, '<'
    je .lex_lt
    cmp al, '>'
    je .lex_gt
    cmp al, '&'
    je .lex_and
    cmp al, '|'
    je .lex_or

    jmp .lex_error

.ws_advance:
    inc r14
    jmp .next_token

.single_token:
    cmp al, '('
    je .tok_lparen
    cmp al, ')'
    je .tok_rparen
    cmp al, '['
    je .tok_lbracket
    cmp al, ']'
    je .tok_rbracket
    cmp al, ','
    je .tok_comma
    cmp al, '+'
    je .tok_plus
    cmp al, '-'
    je .tok_minus
    mov edi, TOK_STAR
    jmp .emit_single

.tok_lparen:
    mov edi, TOK_LPAREN
    jmp .emit_single
.tok_rparen:
    mov edi, TOK_RPAREN
    jmp .emit_single
.tok_lbracket:
    mov edi, TOK_LBRACKET
    jmp .emit_single
.tok_rbracket:
    mov edi, TOK_RBRACKET
    jmp .emit_single
.tok_comma:
    mov edi, TOK_COMMA
    jmp .emit_single
.tok_plus:
    mov edi, TOK_PLUS
    jmp .emit_single
.tok_minus:
    mov edi, TOK_MINUS

.emit_single:
    inc r14
    mov rsi, rbx
    mov rdx, 1
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_ident:
    inc r14
.ident_loop:
    cmp r14, r13
    jae .ident_done
    mov al, [r12 + r14]
    call .is_ident_char
    test rax, rax
    jz .ident_done
    inc r14
    jmp .ident_loop
.ident_done:
    mov r10, r14
    sub r10, rbx
    lea rsi, [r12 + rbx]
    mov rdx, r10
    call .keyword_kind
    mov rsi, rbx
    mov rdx, r10
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_number:
    xor r11d, r11d
.num_loop:
    cmp r14, r13
    jae .num_done
    mov dl, [r12 + r14]
    mov al, dl
    call .is_digit
    test rax, rax
    jnz .num_digit
    mov al, dl
    cmp al, '.'
    jne .num_done
    test r11d, r11d
    jnz .num_done
    mov r11d, 1
    inc r14
    jmp .num_loop
.num_digit:
    inc r14
    jmp .num_loop
.num_done:
    mov edi, TOK_NUMBER
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_string:
    inc r14
.str_loop:
    cmp r14, r13
    jae .lex_error
    mov al, [r12 + r14]
    cmp al, '"'
    je .str_done
    cmp al, 92
    jne .str_advance
    inc r14
    cmp r14, r13
    jae .lex_error
.str_advance:
    inc r14
    jmp .str_loop
.str_done:
    inc r14
    mov edi, TOK_STRING
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_char:
    inc r14
    cmp r14, r13
    jae .lex_error

    mov al, [r12 + r14]
    cmp al, 92
    jne .char_body_done

    inc r14
    cmp r14, r13
    jae .lex_error

.char_body_done:
    inc r14
    cmp r14, r13
    jae .lex_error
    mov al, [r12 + r14]
    cmp al, 39
    jne .lex_error
    inc r14
    mov edi, TOK_CHAR
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_slash:
    inc r14
    cmp r14, r13
    jae .slash_token
    mov al, [r12 + r14]
    call .is_ident_start
    test rax, rax
    jz .slash_check_op

.slash_type_loop:
    cmp r14, r13
    jae .slash_type_done
    mov al, [r12 + r14]
    call .is_ident_char
    test rax, rax
    jz .slash_type_done
    inc r14
    jmp .slash_type_loop

.slash_type_done:
    mov edi, TOK_TYPE
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.slash_check_op:
    mov al, [r12 + r14]
    cmp al, ' '
    je .slash_token
    cmp al, 9
    je .slash_token
    cmp al, 10
    je .slash_token
    cmp al, 13
    je .slash_token
    mov edi, TOK_SLASHOP
    jmp .slash_emit

.slash_token:
    mov edi, TOK_SLASH

.slash_emit:
    mov rsi, rbx
    mov rdx, 1
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_eq:
    inc r14
    mov edi, TOK_EQUAL
    cmp r14, r13
    jae .eq_done
    mov al, [r12 + r14]
    cmp al, '='
    jne .eq_done
    inc r14
    mov edi, TOK_EQEQ
.eq_done:
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_bang:
    inc r14
    mov edi, TOK_BANG
    cmp r14, r13
    jae .bang_done
    mov al, [r12 + r14]
    cmp al, '='
    jne .bang_done
    inc r14
    mov edi, TOK_NOTEQ
.bang_done:
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_lt:
    inc r14
    mov edi, TOK_LESS
    cmp r14, r13
    jae .lt_done
    mov al, [r12 + r14]
    cmp al, '='
    jne .lt_done
    inc r14
    mov edi, TOK_LESSEQ
.lt_done:
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_gt:
    inc r14
    mov edi, TOK_GREATER
    cmp r14, r13
    jae .gt_done
    mov al, [r12 + r14]
    cmp al, '='
    jne .gt_done
    inc r14
    mov edi, TOK_GREATEREQ
.gt_done:
    mov rsi, rbx
    mov rdx, r14
    sub rdx, rbx
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_and:
    inc r14
    cmp r14, r13
    jae .lex_error
    mov al, [r12 + r14]
    cmp al, '&'
    jne .lex_error
    inc r14
    mov edi, TOK_ANDAND
    mov rsi, rbx
    mov rdx, 2
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_or:
    inc r14
    cmp r14, r13
    jae .lex_error
    mov al, [r12 + r14]
    cmp al, '|'
    jne .lex_error
    inc r14
    mov edi, TOK_OROR
    mov rsi, rbx
    mov rdx, 2
    call .emit_token
    test rax, rax
    jnz .lex_error
    jmp .next_token

.lex_ok:
    xor rax, rax
    mov rdx, [rel syntax_token_count]
    jmp .lex_return

.lex_error:
    mov rax, 1
    mov rdx, r14

.lex_return:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.emit_token:
    mov rcx, [rel syntax_token_count]
    cmp rcx, MAX_TOKENS
    jae .emit_fail
    lea r8, [rel syntax_tokens_kind]
    lea r9, [rel syntax_tokens_start]
    lea r10, [rel syntax_tokens_len]
    mov [r8 + rcx * 4], edi
    mov [r9 + rcx * 8], rsi
    mov [r10 + rcx * 8], rdx
    inc rcx
    mov [rel syntax_token_count], rcx
    xor rax, rax
    ret
.emit_fail:
    mov rax, 1
    ret

.keyword_kind:
    mov edi, TOK_IDENT

    cmp rdx, 2
    jne .kw_len2_if
    cmp byte [rsi], 'f'
    jne .kw_len2_if
    cmp byte [rsi + 1], 'n'
    jne .kw_len2_if
    mov edi, TOK_FN
    ret

.kw_len2_if:
    cmp rdx, 2
    jne .kw_len2_in
    cmp byte [rsi], 'i'
    jne .kw_len2_in
    cmp byte [rsi + 1], 'f'
    jne .kw_len2_in
    mov edi, TOK_IF
    ret

.kw_len2_in:
    cmp rdx, 2
    jne .kw_len3
    cmp byte [rsi], 'i'
    jne .kw_len3
    cmp byte [rsi + 1], 'n'
    jne .kw_len3
    mov edi, TOK_IN
    ret

.kw_len3:
    cmp rdx, 3
    jne .kw_len4
    cmp byte [rsi], 'n'
    jne .kw_let
    cmp byte [rsi + 1], 'e'
    jne .kw_let
    cmp byte [rsi + 2], 'w'
    jne .kw_let
    mov edi, TOK_NEW
    ret
.kw_let:
    cmp byte [rsi], 'l'
    jne .kw_mut
    cmp byte [rsi + 1], 'e'
    jne .kw_mut
    cmp byte [rsi + 2], 't'
    jne .kw_mut
    mov edi, TOK_LET
    ret
.kw_mut:
    cmp byte [rsi], 'm'
    jne .kw_for
    cmp byte [rsi + 1], 'u'
    jne .kw_for
    cmp byte [rsi + 2], 't'
    jne .kw_for
    mov edi, TOK_MUT
    ret
.kw_for:
    cmp byte [rsi], 'f'
    jne .kw_len4
    cmp byte [rsi + 1], 'o'
    jne .kw_len4
    cmp byte [rsi + 2], 'r'
    jne .kw_len4
    mov edi, TOK_FOR
    ret

.kw_len4:
    cmp rdx, 4
    jne .kw_len5
    cmp byte [rsi], 'e'
    jne .kw_else
    cmp byte [rsi + 1], 'l'
    jne .kw_else
    cmp byte [rsi + 2], 's'
    jne .kw_else
    cmp byte [rsi + 3], 'e'
    jne .kw_else
    mov edi, TOK_ELSE
    ret
.kw_else:
    jmp .kw_len5

.kw_len5:
    cmp rdx, 5
    jne .kw_len6
    cmp byte [rsi], 'w'
    jne .kw_break
    cmp byte [rsi + 1], 'h'
    jne .kw_break
    cmp byte [rsi + 2], 'i'
    jne .kw_break
    cmp byte [rsi + 3], 'l'
    jne .kw_break
    cmp byte [rsi + 4], 'e'
    jne .kw_break
    mov edi, TOK_WHILE
    ret
.kw_break:
    cmp byte [rsi], 'b'
    jne .kw_len6
    cmp byte [rsi + 1], 'r'
    jne .kw_len6
    cmp byte [rsi + 2], 'e'
    jne .kw_len6
    cmp byte [rsi + 3], 'a'
    jne .kw_len6
    cmp byte [rsi + 4], 'k'
    jne .kw_len6
    mov edi, TOK_BREAK
    ret

.kw_len6:
    cmp rdx, 6
    jne .kw_len8
    cmp byte [rsi], 'r'
    jne .kw_return_short
    cmp byte [rsi + 1], 'e'
    jne .kw_return_short
    cmp byte [rsi + 2], 't'
    jne .kw_return_short
    cmp byte [rsi + 3], 'u'
    jne .kw_return_short
    cmp byte [rsi + 4], 'r'
    jne .kw_return_short
    cmp byte [rsi + 5], 'n'
    jne .kw_return_short
    mov edi, TOK_RETURN
    ret
.kw_return_short:
    jmp .kw_len8

.kw_len8:
    cmp rdx, 8
    jne .kw_done
    cmp byte [rsi], 'c'
    jne .kw_done
    cmp byte [rsi + 1], 'o'
    jne .kw_done
    cmp byte [rsi + 2], 'n'
    jne .kw_done
    cmp byte [rsi + 3], 't'
    jne .kw_done
    cmp byte [rsi + 4], 'i'
    jne .kw_done
    cmp byte [rsi + 5], 'n'
    jne .kw_done
    cmp byte [rsi + 6], 'u'
    jne .kw_done
    cmp byte [rsi + 7], 'e'
    jne .kw_done
    mov edi, TOK_CONTINUE

.kw_done:
    ret

.is_ident_start:
    cmp al, '_'
    je .ident_yes
    cmp al, 'a'
    jb .ident_no
    cmp al, 'z'
    jbe .ident_yes
    cmp al, 'A'
    jb .ident_no
    cmp al, 'Z'
    jbe .ident_yes
.ident_no:
    xor rax, rax
    ret
.ident_yes:
    mov rax, 1
    ret

.is_ident_char:
    cmp al, '_'
    je .ident_char_yes
    cmp al, 'a'
    jb .ident_char_digit
    cmp al, 'z'
    jbe .ident_char_yes
    cmp al, 'A'
    jb .ident_char_digit
    cmp al, 'Z'
    jbe .ident_char_yes
.ident_char_digit:
    cmp al, '0'
    jb .ident_char_no
    cmp al, '9'
    jbe .ident_char_yes
.ident_char_no:
    xor rax, rax
    ret
.ident_char_yes:
    mov rax, 1
    ret

.is_digit:
    cmp al, '0'
    jb .digit_no
    cmp al, '9'
    jbe .digit_yes
.digit_no:
    xor rax, rax
    ret
.digit_yes:
    mov rax, 1
    ret
