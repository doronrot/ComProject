(define asm_symbol->string
  (lambda (global_var_table)
    (string-append
        "JUMP(L_symbol_string_clos); \n"
        "L_symbol_string_body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "MOV(R3, FPARG(2));\n" ;ADD OF SYMBOL
            "MOV(R0,INDD(R3, 1));\n" ;ADD OF REP STRING
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "L_symbol_string_clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_symbol_string_body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'simbol->string global_var_table)) "), R0);\n")))


(define asm_eq?
  (lambda (global_var_table)
    (string-append
        "JUMP(L_eq_clos); \n"
        "L_eq_body: \n"
            "PUSH (FP); \n"
            "MOV (FP, SP); \n"
            "MOV (R1, FPARG(2));\n" ;ADD OF PARAM 1
            "MOV (R2, FPARG(3));\n" ;ADD OF PARAM 2
            "CMP (R1, R2);\n" ;COMPARE ADD  (VOID, NIL, string, BOOLEAN, LIST, VECTOR, CLOSURE)
            "JUMP_EQ (L_eq_body_true);\n"
            "CMP (IND(R1), IND(R2));\n" ;COMPARE TYPE
            "JUMP_NE (L_eq_body_false);\n" ;if type not equal, params not equal
            ;;conds
            "CMP (IND(R1), IMM(T_CHAR));\n"
            "JUMP_EQ (L_eq_compare_by_one);\n"
            "CMP (IND(R1), IMM(T_INTEGER));\n"
            "JUMP_EQ (L_eq_compare_by_one);\n"
            ; "CMP (IND(R1), IMM(T_FRACTION));\n"
            ; "JUMP_EQ (L_eq_compare_by_one);\n"
            "CMP (IND(R1), IMM(T_SYMBOL));\n"
            "JUMP_EQ (L_eq_compare_by_one);\n"

            "JUMP (L_eq_body_false);\n"

            ;comp_by_one
            "L_eq_compare_by_one:\n"
            "CMP (INDD(R1, 1), INDD(R2, 1));\n"
            "JUMP_EQ (L_eq_body_true);\n"
            "JUMP (L_eq_body_false);\n"
            ;comp_by_one_and_two
            "L_eq_compare_by_one_two:\n"
            "CMP (INDD(R1, 1), INDD(R2, 1));\n"
            "JUMP_NE (L_eq_body_false);\n"
            "CMP (INDD(R1, 2), INDD(R2, 2));\n"
            "JUMP_NE (L_eq_body_false);\n"
            "JUMP (L_eq_body_true);\n"
            ;;true
            "L_eq_body_true:\n"
            "MOV (R0, IMM(SOB_TRUE));\n"
            "JUMP (L_eq_body_end);\n"
            ;;false
            "L_eq_body_false:\n"
            "MOV (R0, IMM(SOB_FALSE));\n"
            ;;done
            "L_eq_body_end:\n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "L_eq_clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_eq_body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'simbol->string global_var_table)) "), R0);\n")))


(define asm_string->symbol
  (lambda (global_var_table)
    (string-append
        "JUMP(L_string_symbol_clos); \n"
        "L_string_symbol_body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "MOV(R3, FPARG(2));\n" ;ADD OF STRING
            ;SEARCH_LOOP
            "MOV (R4, "(number->string string_table_start_add)");\n"    ;start symbol_list ADD
            "L_string_symbol_search_loop:\n"
            "CMP (R3, IND(R4));\n"
            "JUMP_EQ (L_clos_string_symbol_found);\n"
            "CMP (INDD(R4, 1), 0);\n"
            "JUMP_EQ (L_clos_string_symbol_not_found);\n"
            "MOV (R4, INDD(R4, 1));\n"

            ;handle results:
            "L_clos_string_symbol_not_found:\n"
            "PUSH (2);\n"                       ;for new string_link
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (IND(R0), R3);\n"
            "MOV (INDD (R0, 1), 0);\n"
            "MOV (INDD(R4, 1), 0);\n"           ;last link is no last anymore
            ;continue to make symbol

            "L_clos_string_symbol_found:\n"
            "PUSH (2);\n"                       ;for symbol
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (IND(R0), IMM(T_SYMBOL));\n"
            "MOV (INDD (R0, 1), R3);\n"

            "POP(FP); \n"
            "RETURN; \n\n"
        
        "L_string_symbol_clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_string_symbol_body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'simbol->string global_var_table)) "), R0);\n")))

