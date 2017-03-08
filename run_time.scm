;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RUN-TIME;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(define asm_car
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeCarClos); \n"
        "LcarBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_car_error_incorr_num_of_args);\n"
            "MOV(R1, FPARG(2)); \n"
            "CMP(INDD(R1, 0), IMM(T_PAIR)); \n"
            "JUMP_NE(L_car_error_incorr_type); \n"
            "MOV(R0,INDD(R1, 1)); \n"
            "L_car_error_incorr_type:\n"
            "L_car_error_incorr_num_of_args:\n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeCarClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LcarBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'car global_var_table)) "), R0);\n")))
     

(define asm_cdr
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeCdrClos); \n"
        "LcdrBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_cdr_error_incorr_num_of_args);\n"
            "MOV(R1, FPARG(2)); \n"
            "CMP(INDD(R1, 0), IMM(T_PAIR)); \n"
            "JUMP_NE(L_cdr_error_incorr_type); \n"
            "MOV(R0,INDD(R1, 2)); \n" ;index 2
            "L_cdr_error_incorr_type:\n"
            "L_cdr_error_incorr_num_of_args:\n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeCdrClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LcdrBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'cdr global_var_table)) "), R0);\n")))
     

(define asm_cons
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeConsClos); \n"
        "LconsBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n" ;two args needed for pair
            "JUMP_NE(L_cons_error_incorr_num_of_args);\n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0,0), IMM(T_PAIR));\n"
            "MOV(INDD(R0,1), FPARG(2)); \n"
            "MOV(INDD(R0,2), FPARG(3)); \n"
            "L_cons_error_incorr_num_of_args:\n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeConsClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"   ;env
            "MOV(INDD(R0, 2), LABEL(LconsBody));\n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'cons global_var_table)) "), R0);\n")))
     
;TODO - it handles only int and not fracs
(define asm_plus
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakePlusClos); \n"
        "LPlusBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "MOV(R1,FPARG(1));\n" ; R1: num params
            "MOV(R2,IMM(0));\n"   ; R2: curr param
            "MOV(R3,IMM(0));\n"   ; R3: acc
        "LPlusLoop: \n"
           "CMP (R2,R1);\n"
           "JUMP_EQ(LPlusEXIT); \n"
           "ADD(R3,INDD(FPARG(2+R2),1));\n"
           "ADD(R2,IMM(1));\n"
           "JUMP (LPlusLoop);\n"
        "LPlusEXIT: \n"
           "PUSH (R3);\n"
           "CALL (MAKE_SOB_INTEGER);\n"
           "DROP(1);\n"
           "POP(FP); \n"
           "RETURN; \n \n"
        
        "LmakePlusClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"   ;env
            "MOV(INDD(R0, 2), LABEL(LPlusBody));\n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'plus global_var_table)) "), R0);\n")))

;TODO - it handles only int and not fracs
(define asm_minus
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeMinusClos); \n"
        "LMinusBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP (FPARG(1),IMM(0));\n"
            "JUMP_EQ(LMinusZeroParams);\n"
            "MOV(R1,FPARG(1));\n" ; R1: num params
            "MOV(R2,IMM(1));\n"   ; R2: curr param
            "MOV(R3,INDD(FPARG(2),1));\n"   ; R3: acc (first param)
            "CMP (FPARG(1),IMM(1));\n"
            "JUMP_EQ(LMinusOneParam);\n"    
        "LMinusLoop: \n"
           "CMP (R2,R1);\n"
           "JUMP_EQ(LMinusEXIT); \n"
           "SUB(R3,INDD(FPARG(2+R2),1));\n"
           "ADD(R2,IMM(1));\n"
           "JUMP (LMinusLoop);\n"
        "LMinusZeroParams: \n"
            "MOV(R3,IMM(0));\n"
           "JUMP_EQ(LMinusEXIT); \n"
        "LMinusOneParam: \n"
            "MOV(R4,R3);\n"
            "ADD(R4,R4);\n"
            "SUB(R3,R4);\n "
        "LMinusEXIT: \n"
           "PUSH (R3);\n"
           "CALL (MAKE_SOB_INTEGER);\n"
           "DROP(1);\n"
           "POP(FP); \n"
           "RETURN; \n \n"

        "LmakeMinusClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"   ;env
            "MOV(INDD(R0, 2), LABEL(LMinusBody));\n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'minus global_var_table)) "), R0);\n")))


     
;TODO - it handles only int and not fracs
(define asm_multiply
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeMultiplyClos); \n"
        "LMultiplyBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "MOV(R1,FPARG(1));\n" ; R1: num params
            "MOV(R2,IMM(1));\n"   ; R2: curr param
            "MOV(R3,INDD(FPARG(2),1));\n"   ; R3: acc (first param)
        "LMultiplyLoop: \n"
           "CMP (R2,R1);\n"
           "JUMP_EQ(LMultiplyEXIT); \n"
           "MUL(R3,INDD(FPARG(2+R2),1));\n"
           "ADD(R2,IMM(1));\n"
           "JUMP (LMultiplyLoop);\n"
        "LMultiplyEXIT: \n"
           "PUSH (R3);\n"
           "CALL (MAKE_SOB_INTEGER);\n"
           "DROP(1);\n"
           "POP(FP); \n"
           "RETURN; \n \n"
        
        "LmakeMultiplyClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"   ;env
            "MOV(INDD(R0, 2), LABEL(LMultiplyBody));\n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'multiply global_var_table)) "), R0);\n")))

;TODO - it handles only int and not fracs
;       also doesn't handle 1 param -> 1/x
(define asm_div
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeDivClos); \n"
        "LDivBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP (FPARG(1),IMM(0));\n"
            "JUMP_EQ(Lerror_Div_zero_params);\n"    
            "MOV(R1,FPARG(1));\n" ; R1: num params
            "MOV(R2,IMM(1));\n"   ; R2: curr param
            "MOV(R3,INDD(FPARG(2),1));\n"   ; R3: acc (first param)
        "LDivLoop: \n"
           "CMP (R2,R1);\n"
           "JUMP_EQ(LDivEXIT); \n"
           "DIV(R3,INDD(FPARG(2+R2),1));\n"
           "ADD(R2,IMM(1));\n"
           "JUMP (LDivLoop);\n"
        "LDivEXIT: \n"
           "PUSH (R3);\n"
           "CALL (MAKE_SOB_INTEGER);\n"
           "DROP(1);\n"
        "Lerror_Div_zero_params: \n"
           "POP(FP); \n"
           "RETURN; \n \n"
        
        "LmakeDivClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"   ;env
            "MOV(INDD(R0, 2), LABEL(LDivBody));\n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'div global_var_table)) "), R0);\n")))


;TODO not variadic
(define asm_smallerThan
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeSmallerThanClos); \n"
        "LSmallerThanBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n"
            "JUMP_NE(LSmallerThan_EXIT);\n"  ;error agrs num
            "MOV(R1, INDD(FPARG(2),1)); \n"
            "MOV(R2, INDD(FPARG(3),1)); \n"
            "CMP(R1, R2); \n"
            "JUMP_LT(LSmallerThan); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(LSmallerThan_EXIT); \n"
        "LSmallerThan: \n"
            "MOV(R0,SOB_TRUE);\n"
        "LSmallerThan_EXIT: \n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeSmallerThanClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LSmallerThanBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'SmallerThan global_var_table)) "), R0);\n")))

;TODO not variadic
(define asm_GreaterThan
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeGreaterThanClos); \n"
        "LGreaterThanBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n"
            "JUMP_NE(LGreaterThan_End);\n"  ;error agrs num
            "MOV(R1, INDD(FPARG(2),1)); \n"
            "MOV(R2, INDD(FPARG(3),1)); \n"
            "CMP(R1, R2); \n"
            "JUMP_GT(LGreaterThan); \n"
        "LGreaterThan_FALSE: \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(LGreaterThan_End); \n"
        "LGreaterThan: \n"
            "MOV(R0,SOB_TRUE);\n"
        "LGreaterThan_End: \n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeGreaterThanClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LGreaterThanBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'greaterThan global_var_table)) "), R0);\n")))

;TODO not variadic
(define asm_shave
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeShaveClos); \n"
        "LShaveBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n"
            "JUMP_NE(LShave_End);\n"    ;error agrs num
            "MOV(R1, INDD(FPARG(2),1)); \n"
            "MOV(R2, INDD(FPARG(3),1)); \n"
            "CMP(R1, R2); \n"
            "JUMP_EQ(LShave); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(LShave_End); \n"
        "LShave: \n"
            "MOV(R0,SOB_TRUE);\n"
        "LShave_End: \n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeShaveClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LShaveBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'shave global_var_table)) "), R0);\n")))


(define asm_zero?
  (lambda (global_var_table)
    (string-append
        "JUMP(LmakeZeroClos); \n"
        "LZeroBody: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(LZero_End);\n"   ;incorr args num
            "CMP(INDD(R1, 1), IMM(0)); \n"
            "JUMP_EQ(LZero); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(LZero_End); \n"
        "LZero: \n"
            "MOV(R0,SOB_TRUE);\n"
        "LZero_End: \n"
            "POP(FP); \n"
            "RETURN; \n \n"
        
        "LmakeZeroClos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(LZeroBody)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'zero? global_var_table)) "), R0);\n")))
    


;TODO well syntaxed??
; make a cunk of scheme specials
; (define asm_not
;   (lambda (global_var_table)
;     (string-append
;         "JUMP(LmakeNotClos); \n"
;         "LNotBody: \n"
;             (code-gen pe major const_tab)
;             "PUSH(FP); \n"
;             "MOV(FP, SP); \n"
;             "CMP(R0,SOB_FALSE); \n"
;             "JUMP_EQ(LNot_retTrue); \n"
;             "MOV(R0,SOB_FALSE); \n"     ;else
;             "JUMP(LNot_End); \n"
;         "LNot_retTrue: \n"
;             "MOV(R0,SOB_TRUE);\n"
;         "LNot_End: \n"
;             "POP(FP); \n"
;             "RETURN; \n \n"
        
;         "LmakeNotClos: \n"
;             "PUSH(IMM(3)); \n"
;             "CALL(MALLOC); \n"
;             "DROP(1); \n"
;             "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
;             "MOV(INDD(R0, 1), IMM(12345678)); \n"
;             "MOV(INDD(R0, 2), LABEL(LNotBody)); \n"
;             "MOV(IND(" (number->string (fvar_get_address_by_name 'not global_var_table)) "), R0);\n")))
    
; (define asm_functions
;     not)


(define asm_boolean?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_boolean_Clos); \n"
        "L_boolean_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_boolean_End);\n"   ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_BOOL);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_isBoolean); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_boolean_End); \n"
        "L_isBoolean: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_boolean_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_boolean_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_boolean_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'boolean? global_var_table)) "), R0);\n")))
    

(define asm_char?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_char_Clos); \n"
        "L_char_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_char_End);\n"      ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_CHAR);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_isChar); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_char_End); \n"
        "L_isChar: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_char_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_char_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_char_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'char? global_var_table)) "), R0);\n")))
    

(define asm_integer?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_integer_Clos); \n"
        "L_integer_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_integer_End);\n"   ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_INTEGER);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_is_integer); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_integer_End); \n"
        "L_is_integer: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_integer_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_integer_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_integer_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'integer? global_var_table)) "), R0);\n")))
    

(define asm_pair?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_pair_Clos); \n"
        "L_pair_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_pair_End);\n"      ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_PAIR);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_is_pair); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_pair_End); \n"
        "L_is_pair: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_pair_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_pair_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_pair_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'pair? global_var_table)) "), R0);\n")))
    


(define asm_null?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_nil_Clos); \n"
        "L_nil_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_nil_End);\n"   ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_NIL);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_is_nil); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_nil_End); \n"
        "L_is_nil: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_nil_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_nil_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_nil_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'null? global_var_table)) "), R0);\n")))



(define asm_procedure?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_closure_Clos); \n"
        "L_closure_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_closure_End);\n"   ;incorr args num
            "PUSH(FPARG(2)); \n"
            "CALL(IS_SOB_CLOSURE);\n"
            "DROP(1);\n"
            "CMP(R0, IMM(1)); \n"
            "JUMP_EQ(L_is_closure); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_closure_End); \n"
        "L_is_closure: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_closure_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_closure_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_closure_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'procedure? global_var_table)) "), R0);\n")))


(define asm_string?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_string_Clos); \n"
        "L_string_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_string_End);\n"    ;incorr args num
            "MOV(R4,INDD(FPARG(2),0));\n"
            "CMP(R4, IMM(T_STRING)); \n"
            "JUMP_EQ(L_is_string); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_string_End); \n"
        "L_is_string: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_string_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_string_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_string_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'string? global_var_table)) "), R0);\n")))


(define asm_vector?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_vector_Clos); \n"
        "L_vector_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_vector_End);\n"    ;incorr args num
            "MOV(R4,INDD(FPARG(2),0));\n"
            "CMP(R4, IMM(T_VECTOR)); \n"
            "JUMP_EQ(L_is_vector); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_vector_End); \n"
        "L_is_vector: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_vector_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_vector_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_vector_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'vector? global_var_table)) "), R0);\n")))



(define asm_number?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_number_Clos); \n"
        "L_number_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_number_End);\n"    ;incorr args num
            "MOV(R4,INDD(FPARG(2),0));\n"   ;R4: type
            "CMP(R4, IMM(T_INTEGER)); \n"
            "JUMP_EQ(L_is_number); \n"
            "CMP(R4, IMM(T_FRACTION)); \n"  ;else if
            "JUMP_EQ(L_is_number); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_number_End); \n"
        "L_is_number: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_number_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_number_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_number_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'number? global_var_table)) "), R0);\n")))




(define asm_rational?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_rational_Clos); \n"
        "L_rational_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_rational_End);\n"      ;incorr args num
            "MOV(R4,INDD(FPARG(2),0));\n"   ;R4: type
            "CMP(R4, IMM(T_INTEGER)); \n"
            "JUMP_EQ(L_is_rational); \n"
            "CMP(R4, IMM(T_FRACTION)); \n"  ;else if
            "JUMP_EQ(L_is_rational); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_rational_End); \n"
        "L_is_rational: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_rational_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_rational_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_rational_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'rational? global_var_table)) "), R0);\n")))


; TODO do we have make_sob_symbol?
(define asm_symbol?
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_symbol_Clos); \n"
        "L_symbol_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_symbol_End);\n"    ;incorr args num
            "MOV(R4,INDD(FPARG(2),0));\n"
            "CMP(R4, IMM(T_SYMBOL)); \n"
            "JUMP_EQ(L_is_symbol); \n"
            "MOV(R0,SOB_FALSE); \n"     ;else
            "JUMP(L_symbol_End); \n"
        "L_is_symbol: \n"
            "MOV(R0,SOB_TRUE);\n"
        "L_symbol_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_symbol_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_symbol_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'symbol? global_var_table)) "), R0);\n")))

(define asm_string_length
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_string_length_Clos); \n"
        "L_string_length_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_string_length_End);\n"     ;incorr args num
            "MOV(R1, FPARG(2)); \n" 
            "CMP(INDD(R1,0),IMM(T_STRING)); \n" 
            "JUMP_NE(L_string_length_End);\n"   ;not a string
            "MOV(R2,INDD(R1,1));\n"
            "PUSH(R2);\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP (1);\n"     ;R0<_strlen (in shape of t_int)
        "L_string_length_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_string_length_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_string_length_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'string-length global_var_table)) "), R0);\n")))

(define asm_vector_length
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_vector_length_Clos); \n"
        "L_vector_length_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(1)); \n"
            "JUMP_NE(L_vector_length_EXIT);\n"     ;incorr args num
            "MOV(R1, FPARG(2)); \n" 
            "CMP(INDD(R1,0),IMM(T_VECTOR)); \n" 
            "JUMP_NE(L_vector_length_EXIT);\n"   ;not a vector
            "MOV(R2,INDD(FPARG(2),1));\n"
            "PUSH(R2);\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP (1);\n"     ;R0<_veclen (in shape of t_int)
        "L_vector_length_EXIT: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_vector_length_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_vector_length_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'vector-length global_var_table)) "), R0);\n")))

; assuming that ref is in str-len bounds
(define asm_string_ref
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_string_ref_Clos); \n"
        "L_string_ref_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n"
            "JUMP_NE(L_string_ref_End);\n"     ;incorr args num
            "MOV(R1, FPARG(2)); \n"     ;R1<-string
            "CMP(INDD(R1,0),IMM(T_STRING)); \n" 
            "JUMP_NE(L_string_ref_End);\n"   ;not a string type
            "MOV(R2,FPARG(3)); \n"  ;R2<-index 
            "MOV (R2, INDD(R2, 1));\n"
            "MOV(R3,INDD(R1,2+R2));\n"
            "PUSH(R3);\n"
            "CALL(MAKE_SOB_CHAR);\n"
            "DROP (1);\n"     ;R0<-str[i] (in shape of sob_char)
        "L_string_ref_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_string_ref_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_string_ref_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'string-ref global_var_table)) "), R0);\n")))

; assuming that ref is in vector-len bounds
(define asm_vector_ref
  (lambda (global_var_table)
    (string-append
        "JUMP(Lmake_vector_ref_Clos); \n"
        "L_vector_ref_Body: \n"
            "PUSH(FP); \n"
            "MOV(FP, SP); \n"
            "CMP(FPARG(1), IMM(2)); \n"
            "JUMP_NE(L_vector_ref_End);\n"     ;incorr args num
            "MOV(R1, FPARG(2)); \n"     ;R1<-vector
            "CMP(INDD(R1,0),IMM(T_VECTOR)); \n" 
            "JUMP_NE(L_vector_ref_End);\n"   ;not a vector type
            "MOV(R2,INDD(FPARG(3),1)); \n"  ;R2<-index 
            "MOV(R3,INDD(R1,2+R2));\n"  ;R3<-ref_to_Element
            "MOV(R0,R3);\n"
        "L_vector_ref_End: \n"
            "POP(FP); \n"
            "RETURN; \n\n"
        
        "Lmake_vector_ref_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_vector_ref_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'vector-ref global_var_table)) "), R0);\n")))

;sending params to make_sob_string: chars and length
(define asm_make_string
  (lambda (global_var_table)
    (string-append
        " JUMP(Lmake_make_string_Clos); \n"
        "L_make_string_Body: \n"
            " PUSH(FP); \n"
            " MOV(FP, SP); \n"
            " CMP(FPARG(1), IMM(1)); \n"
            " JUMP_LT(L_make_string_End);\n"     ;incorr args num
            " CMP(FPARG(1), IMM(2)); \n"
            " JUMP_GT(L_make_string_End);\n"     ;incorr args num
            " MOV(R3,INDD(FPARG(2),1));\n"  ;R3<-length
            " MOV(R4,R3);\n"    ;length to push before calling make_sob_string
            " MOV(R2,0);\n"   ;TODO------->\000
            " CMP(FPARG(1), IMM(2)); \n"
            " JUMP_NE(L_make_string_Loop);\n"     ;fill with \000's
            " MOV(R1, FPARG(3)); \n"    ;else: char was given
            " CMP(IND(R1),T_CHAR); \n" 
            " JUMP_NE(L_make_string_End);\n" ;not a char
            " MOV(R2, INDD(R1,1)); \n"   ;R2<-the specified char //overloading \000
        "L_make_string_Loop: \n"
            " CMP(R3,IMM(0));\n"
            " JUMP_EQ(L_make_Sob_String);\n"
            " PUSH(R2);\n"
            " SUB(R3,IMM(1));\n"
            " JUMP(L_make_string_Loop);\n"  
        "L_make_Sob_String: \n"
            " PUSH(R4);\n"
            " CALL(MAKE_SOB_STRING);\n"
            " DROP(1);\n" 
            " DROP (R4);\n" 
        "L_make_string_End: \n"
            " POP(FP); \n"
            " RETURN; \n\n"

        "Lmake_make_string_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_make_string_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'make-string global_var_table)) "), R0);\n")))



;sending params to make_sob_vector: some_type and length
(define asm_make_vector
  (lambda (global_var_table)
    (string-append
        " JUMP(Lmake_make_vector_Clos); \n"
        "L_make_vector_Body: \n"
            " PUSH(FP); \n"
            " MOV(FP, SP); \n"
            " CMP(FPARG(1), IMM(1)); \n"
            " JUMP_LT(L_make_vector_End);\n"     ;incorr args num
            " CMP(FPARG(1), IMM(2)); \n"
            " JUMP_GT(L_make_vector_End);\n"     ;incorr args num
            " MOV(R3,INDD(FPARG(2),1));\n"  ;R3<-length
            " MOV(R4,R3);\n"    ;length to push before calling make_sob_vector
            " MOV(R1,0);\n"   ;TODO------->\000
            " CMP(FPARG(1), IMM(2)); \n"
            " JUMP_NE(L_make_vector_Loop);\n"     ;fill with \000's
            " MOV(R1, FPARG(3)); \n"    ;R1<-some_type
        "L_make_vector_Loop: \n"
            " CMP(R3,IMM(0));\n"
            " JUMP_EQ(L_make_Sob_Vector);\n"
            " PUSH(R1);\n"
            " SUB(R3,IMM(1));\n"
            " JUMP(L_make_vector_Loop);\n"  
        "L_make_Sob_Vector: \n"
            " PUSH(R4);\n"
            " CALL(MAKE_SOB_VECTOR);\n"
            " DROP(1);\n"
            " DROP (R4);\n"
        "L_make_vector_End: \n"
            " POP(FP); \n"
            " RETURN; \n\n"

        "Lmake_make_vector_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_make_vector_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'make-vector global_var_table)) "), R0);\n")))



(define asm_vector
  (lambda (global_var_table)
    (string-append
        " JUMP(Lmake_vector_Clos); \n"
        "L_vector_Body: \n"
            " PUSH(FP); \n"
            " MOV(FP, SP); \n"
            " MOV(R3,FPARG(1));\n" ;num of args
            " MOV(R4,IMM(0));\n"    ;counter
        "L_vector_Loop: \n"
            " CMP(R4,R3);\n"
            " JUMP_EQ(L_make_Sob_Vector);\n"
            " MOV(R1, FPARG(2+R4));\n"
            " PUSH(R1);\n"
            " ADD(R4,IMM(1));\n"
            " JUMP(L_vector_Loop);\n" 
        "L_make_Sob_Vector: \n"
            " PUSH(R3);\n"
            " CALL(MAKE_SOB_VECTOR);\n"
            " DROP(1);\n" 
            " DROP(R3);\n"
        "L_vector_End: \n"
            " POP(FP); \n"
            " RETURN; \n\n"

        "Lmake_vector_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_vector_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'vector global_var_table)) "), R0);\n")))


(define asm_string_set
  (lambda (global_var_table)
    (string-append
        " JUMP(Lmake_string_set_Clos); \n"
        "L_string_set_Body: \n"
            " PUSH(FP); \n"
            " MOV(FP, SP); \n"
            " CMP(FPARG(1), IMM(3)); \n"
            " JUMP_NE(L_string_set_End);\n"     ;incorr args num
            " MOV(R2,FPARG(2));\n"       ;R2<-str
            " MOV(R3,INDD(FPARG(3),1));\n"  ;R3<-index to set
            " MOV(R4,INDD(FPARG(4),1));\n"  ;R4<-new char
            " MOV(INDD(R2,2+R3),R4);\n"     ;put the new char in the wanted index
            " MOV(R0, IMM(SOB_VOID));\n"
        "L_string_set_End: \n"
            " POP(FP); \n"
            " RETURN; \n\n"

        "Lmake_string_set_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_string_set_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'string-set! global_var_table)) "), R0);\n")))


(define asm_vector_set
  (lambda (global_var_table)
    (string-append
        " JUMP(Lmake_vector_set_Clos); \n"
        "L_vector_set_Body: \n"
            " PUSH(FP); \n"
            " MOV(FP, SP); \n"
            " CMP(FPARG(1), IMM(3)); \n"
            " JUMP_NE(L_vector_set_End);\n"     ;incorr args num
            " MOV(R2,FPARG(2));\n"       ;R2<-vector
            " MOV(R3,INDD(FPARG(3),1));\n"  ;R3<-index to set
            " MOV(R4,FPARG(4));\n"  ;R4<-new type
            " MOV(INDD(R2,2+R3),R4);\n"     ;put the new type in the wanted index
            " MOV(R0, IMM(SOB_VOID));\n"
        "L_vector_set_End: \n"
            " POP(FP); \n"
            " RETURN; \n\n"

        "Lmake_vector_set_Clos: \n"
            "PUSH(IMM(3)); \n"
            "CALL(MALLOC); \n"
            "DROP(1); \n"
            "MOV(INDD(R0, 0), IMM(T_CLOSURE)); \n"
            "MOV(INDD(R0, 1), IMM(12345678)); \n"
            "MOV(INDD(R0, 2), LABEL(L_vector_set_Body)); \n"
            "MOV(IND(" (number->string (fvar_get_address_by_name 'vector-set! global_var_table)) "), R0);\n")))


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
            "CMP (R1, R2);\n" ;COMPARE ADD  (VOID, NIL, string, BOOLEAN, pair (and list), VECTOR, CLOSURE)
            "JUMP_EQ (L_eq_body_true);\n"
            "CMP (IND(R1), IND(R2));\n" ;COMPARE TYPE
            "JUMP_NE (L_eq_body_false);\n" ;if type not equal, params not equal
            ;;conds
            "CMP (IND(R1), IMM(T_CHAR));\n"
            "JUMP_EQ (L_eq_compare_by_one);\n"
            "CMP (IND(R1), IMM(T_INTEGER));\n"
            "JUMP_EQ (L_eq_compare_by_one);\n"
            "CMP (IND(R1), IMM(T_FRACTION));\n"
            "JUMP_EQ (L_eq_compare_by_one_two);\n"
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
            "MOV(IND(" (number->string (fvar_get_address_by_name 'eq global_var_table)) "), R0);\n")))


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
            "JUMP (L_string_symbol_search_loop);\n"
            ;handle results:
            "L_clos_string_symbol_not_found:\n"
            "PUSH (2);\n"                       ;for new string_link
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (IND(R0), R3);\n"
            "MOV (INDD (R0, 1), 0);\n"
            "MOV (INDD(R4, 1), R0);\n"           ;last link is no last anymore
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
            "MOV(IND(" (number->string (fvar_get_address_by_name 'string->symbol global_var_table)) "), R0);\n")))

