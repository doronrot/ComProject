(load "compiler_hw3.scm")
(load "run_time.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE-GEN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define count 0)

(define code-gen
    (lambda (pe major const_tab global_tab)
        (cond 
              ((equal? pe `(const ,(void))) (code-gen-void))
              ((equal? pe `(const ())) (code-gen-nil))
              ((equal? pe `(const #f)) (code-gen-false))
              ((equal? pe `(const #t)) (code-gen-true))
              ((equal? pe 'cons) (asm_cons global_tab))
              ((equal? pe 'car) (asm_car global_tab))
              ((equal? pe 'cdr) (asm_cdr global_tab))
              ((equal? pe '+) (asm_plus global_tab))
              ((equal? pe '-) (asm_minus global_tab))
              ((equal? pe '*) (asm_multiply global_tab))
              ((equal? pe '/) (asm_div global_tab))
              ((equal? pe '<) (asm_smallerThan global_tab))
              ((equal? pe '>) (asm_GreaterThan global_tab))
              ((equal? pe '=) (asm_shave global_tab))
              ((equal? pe 'zero?) (asm_zero? global_tab))
              ((equal? pe 'boolean?) (asm_boolean? global_tab))
              ((equal? pe 'char?) (asm_char? global_tab))
              ((equal? pe 'integer?) (asm_integer? global_tab))
              ((equal? pe 'pair?) (asm_pair? global_tab))
              ((equal? pe 'procedure?) (asm_procedure? global_tab))
              ((equal? pe 'string?) (asm_string? global_tab))
              ((equal? pe 'vector?) (asm_vector? global_tab))
              ((equal? pe 'number?) (asm_number? global_tab))
              ((equal? pe 'rational?) (asm_rational? global_tab))
              ((equal? pe 'symbol?) (asm_symbol? global_tab))
              ((equal? pe 'null?) (asm_null? global_tab))
              ((equal? pe 'string-length) (asm_string_length global_tab))
              ((equal? pe 'vector-length) (asm_vector_length global_tab))
              ((pair? pe)
               ;TODO:,box
               (cond ((equal? (car pe) 'if3) (code-gen-if3 pe major const_tab global_tab))
                     ((equal? (car pe) 'seq) (code-gen-seq pe major const_tab global_tab))
                     ((equal? (car pe) 'or) (code-gen-or pe major const_tab global_tab))
                     ((equal? (car pe) 'const) (code-gen-const pe major const_tab))
                     ((equal? (car pe) 'applic) (code-gen-applic pe major const_tab global_tab))
                     ((equal? (car pe) 'tc-applic) (code-gen-tc-applic pe major const_tab global_tab))
                     ((equal? (car pe) 'lambda-simple) (code-gen-lambda-simple pe major const_tab global_tab))
                     ((equal? (car pe) 'lambda-opt) (code-gen-lambda-opt pe major const_tab global_tab))
                     ((equal? (car pe) 'lambda-var) (code-gen-lambda-var pe major const_tab global_tab))
                     ((equal? (car pe) 'pvar) (code-gen-pvar pe major))
                     ((equal? (car pe) 'bvar) (code-gen-bvar pe major))
                     ((equal? (car pe) 'fvar) (code-gen-fvar pe major global_tab))
                     ((and (equal? (car pe) 'set) (equal? (caadr pe) 'pvar)) (code-gen-set-pvar pe major const_tab global_tab))
                     ((and (equal? (car pe) 'set) (equal? (caadr pe) 'bvar)) (code-gen-set-bvar pe major const_tab global_tab))
                     ((and (equal? (car pe) 'set) (equal? (caadr pe) 'fvar)) (code-gen-set-fvar pe major const_tab global_tab))
                     ((and (equal? (car pe) 'def) (equal? (caadr pe) 'fvar)) (code-gen-set-fvar pe major const_tab global_tab))
                     ((and (equal? (car pe) 'box-get) (equal? (caadr pe) 'pvar)) (code-gen-box-get-pvar pe major))
                     ((and (equal? (car pe) 'box-get) (equal? (caadr pe) 'bvar)) (code-gen-box-get-bvar pe major))
                     ((and (equal? (car pe) 'box-set) (equal? (caadr pe) 'pvar)) (code-gen-box-set-pvar pe major const_tab global_tab))
                     ((and (equal? (car pe) 'box-set) (equal? (caadr pe) 'bvar)) (code-gen-box-set-bvar pe major const_tab global_tab))
                     (else "")))
              (else "")))) 
                
(define code-gen-void
    (lambda ()
        (string-append
            ; "CALL (MAKE_SOB_VOID);\n"
            "\n\n//----------VOID----------//\n\n"
            "MOV (R0, IMM(SOB_VOID));\n"
            ; "PUSH(R0);\n"
            ; "CALL(WRITE_SOB_VOID);\n"
            ; "POP(R0);\n"
        )))
        
(define code-gen-nil
    (lambda ()
        (string-append
            ; "CALL (MAKE_SOB_NIL);\n"
            "\n\n//----------NIL----------//\n\n"                   
            "MOV(R0, IMM(SOB_NIL));\n"
            ; "PUSH(R0);\n"
            ; "CALL(WRITE_SOB_NIL);\n"
            ; "POP(R0);\n"
        )))
        
(define code-gen-false
    (lambda ()
        (string-append
            ; "PUSH (IMM(0));\n"
            ; "CALL (MAKE_SOB_BOOL);\n"
            ; "DROP (1);\n"
            "\n\n//----------FALSE----------//\n\n"
            "MOV(R0, IMM(SOB_FALSE));\n"
            ; "PUSH(R0);\n"
            ; "CALL(WRITE_SOB_BOOL);\n"
            ; "POP(R0);\n"
        )))

(define code-gen-true
    (lambda ()
        (string-append
            ; "PUSH (IMM(1));\n"
            ; "CALL (MAKE_SOB_BOOL);\n"
            ; "DROP (1);\n"
            "\n\n//----------TRUE----------//\n\n"
            "MOV(R0, IMM(SOB_TRUE));\n"
            ; "PUSH(R0);\n"
            ; "CALL(WRITE_SOB_BOOL);\n"
            ; "POP(R0);\n"
        )))
        
(define code-gen-if3
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let ((test (cadr pe))
              (dit (caddr pe))
              (dif (cadddr pe))
              (count_str (number->string count)))
            (string-append   "\n\n//----------IF3----------//\n\n"
                            (code-gen test major const_tab global_tab)
                            "CMP (R0, IMM(SOB_FALSE));\n"
                            "JUMP_EQ (L_if3_else_"count_str");\n"
                            (code-gen dit major const_tab global_tab)
                            "JUMP (L_if3_exit_"count_str");\n"
                            "L_if3_else_"count_str":\n"
                            (code-gen dif major const_tab global_tab)
                            "L_if3_exit_"count_str":\n"))))
                            
(define code-gen-seq
    (lambda (pe major const_tab global_tab)
        (let ((seq_body (cadr pe)))
            (letrec ((run (lambda (lst)
                                (if (null? lst)
                                    ""
                                    (string-append (code-gen (car lst) major const_tab global_tab)
                                                   (run (cdr lst)))))))
                    (string-append "\n\n//----------SEQ----------//\n\n"
                                    (run seq_body))))))
                                    
(define code-gen-or
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let ((or_exps (cadr pe))
              (count_str (number->string count)))
            (letrec ((run (lambda (lst)
                                (if (equal? (length lst) 1)
                                    (string-append (code-gen (car lst) major const_tab global_tab)
                                                "L_or_exit_"count_str":\n"
                                                ; "PUSH (R0);\n"
                                                ; "CALL (WRITE_SOB);\n"
                                                ; "DROP (1);\n"
                                                )
                                    (string-append (code-gen (car lst) major const_tab global_tab)
                                                "CMP (R0, IMM(SOB_FALSE));\n"
                                                "JUMP_NE (L_or_exit_"count_str");\n"
                                                (run (cdr lst)))))))
                         (string-append "\n\n//----------OR----------//\n\n"
                                        (run or_exps))))))
                                        
(define code-gen-const
   (lambda (pe major const_tab)
        (let* ((address (search_element (cadr pe) const_tab))
                (address_str (number->string address)))
            (string-append
                "\n\n//----------CONST----------//\n\n"
                "MOV (R0, IMM("address_str"));\n")
            )))
                                    
(define code-gen-applic
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let* ((proc (cadr pe))
                (args (caddr pe))
                (args_count (length args))
                (args_count_str (number->string args_count))
                (count_str (number->string count)))
                    (letrec ((run (lambda (lst) 
                                     (if (null? lst)
                                         (string-append 
                                           "PUSH ("args_count_str");\n"
                                           (code-gen proc major const_tab global_tab)
                                           "CMP (INDD(R0, 0), IMM(T_CLOSURE));\n"
                                           "JUMP_NE (L_error_cannot_apply_non_clos_"count_str");\n"
                                           "PUSH (INDD(R0, 1));\n"
                                           "MOV (R4, INDD(R0, 2));\n"
                                           "CALLA (R4);\n"
                                           "DROP (1);\n"
                                           "POP (R1);\n"
                                           "DROP (R1);\n"
                                           "DROP (1);\n"                    ;;???
                                           "JUMP (L_applic_exit_"count_str");\n"
                                           "L_error_cannot_apply_non_clos_"count_str":\n"
                                           "L_applic_exit_"count_str":\n")
                                         (string-append 
                                           (code-gen (car lst) major const_tab global_tab)
                                           "PUSH (R0);\n"
                                           (run (cdr lst)))))))
                        (string-append
                                "\n\n//----------APPLIC----------//\n\n"
                                "PUSH (SOB_NIL);\n"
                                (run (reverse args)))))))

(define code-gen-tc-applic
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let* ((proc (cadr pe))
                (args (caddr pe))
                (args_count (length args))
                (args_count_str (number->string args_count))
                (count_str (number->string count)))
                    (letrec ((run (lambda (lst counter_up) 
                                     (if (null? lst)
                                         (string-append 
                                           "PUSH ("args_count_str");\n"
                                           (code-gen proc major const_tab global_tab)
                                           "CMP (INDD(R0, 0),IMM(T_CLOSURE));\n"
                                           "JUMP_NE (L_error_cannot_apply_non_clos_"count_str");\n"
                                           "PUSH (INDD(R0, 1));\n"
                                           "MOV (R2, FPARG(-1));\n" ;ret
                                           "PUSH (R2);\n"
                                           "MOV (R1, FPARG(-2));\n" ;old fp 1 - START FROM HERE
                                           "MOV (R7,  R1);"
                                           "MOV (R3, 0);\n"
                                           "L_tc_applic_loop_"count_str":\n"
                                           "CMP (R3, IMM("(number->string counter_up)"));\n" ;??
                                           "JUMP_EQ (L_tc_applic_loop_exit_"count_str");\n"
                                           "MOV (STACK(R7), LOCAL(R3));\n"
                                           "ADD (R3, IMM(1));\n"
                                           "ADD (R7, IMM(1));\n"
                                           "JUMP (L_tc_applic_loop_"count_str");\n"
                                           "L_tc_applic_loop_exit_"count_str":\n"
                                           "MOV (FP, R1);\n"
                                           "ADD (R3, FP);\n" ;??MAYBE THE NEW FP
                                           "MOV (SP, R3);\n"
                                           "JUMPA (INDD(R0, 2));\n"
                                           "L_error_cannot_apply_non_clos_"count_str":\n"
                                           "")

                                         (string-append 
                                           (code-gen (car lst) major const_tab global_tab)
                                           "PUSH (R0);\n"
                                           (run (cdr lst) (+ counter_up 1)))))))
                    (string-append
                                "\n\n//----------TC-APPLIC----------//\n\n"
                                (run (reverse args) 3))))))

(define code-gen-lambda-simple
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let* (
                (params (cadr pe))
                (num_params (length params))
                (num_params_str (number->string num_params))
                (body (caddr pe))
                (count_str (number->string count))
                (major_str (number->string major)))
            (string-append 
            "\n\n//----------LAMBDA-SIMPLE----------//\n\n"

            "\n\n//----------lambda-simple-build-env----------//\n"
            "MOV (R1, FPARG(0));\n" ;env
            "PUSH (IMM(1+"major_str"));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (R2, R0);\n"

            "\n\n//----------lambda-simple-build-env-shallow-copy----------//\n"
            (letrec ((shallow_copy 
                        (lambda (i j)
                            (let ((i_str (number->string i))
                                    (j_str (number->string j)))
                                (if (>= i major)
                                    ""
                                    (string-append 
                                        "MOV (R4, INDD(R1," i_str"));\n"
                                        "MOV (INDD(R2," j_str"), R4);\n"
                                        (shallow_copy (+ i 1) (+ j 1))))))))
                (shallow_copy 0 1))

            "\n\n//----------lambda-simple-build-env-from-stack----------//\n"
            "MOV (R3, FPARG(1));\n" ;number of argumets
            "PUSH (R3);\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R2, 0), R0);\n"
            "MOV (R6, 0);\n" ;i
            "MOV (R7, 2);\n" ;j
            "L_clos_loop_"count_str":\n"
            "CMP (R6, R3);\n"
            "JUMP_GE (L_clos_loop_end_"count_str");\n"
            "MOV (R4, (INDD(R2, 0)));\n"
            "MOV (R5, FPARG(R7));\n"
            "MOV (INDD(R4, R6), R5);\n"
            "ADD (R6, IMM(1));\n"
            "ADD (R7, IMM(1));\n"
            "JUMP (L_clos_loop_"count_str");\n"
            "L_clos_loop_end_"count_str":\n"

            "\n\n//----------lambda-simple-build-closure----------//\n"
            "PUSH (IMM(3));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R0, 0), IMM(T_CLOSURE));\n"
            "MOV (INDD(R0, 1), R2);\n"  ;ext. env          
            "MOV (INDD(R0, 2), LABEL(L_clos_body_"count_str"));\n"
            "JUMP (L_clos_exit_"count_str");\n"
            
            "\n\n//----------lambda-simple-body----------//\n"
            "L_clos_body_"count_str":\n"
            "PUSH (FP);\n"
            "MOV (FP,SP);\n"
            "CMP (FPARG(1), IMM("num_params_str"));\n"
            "JUMP_NE (L_error_lambda_args_count_"count_str");\n"
            (code-gen body (+ major 1) const_tab global_tab)
            "JUMP (L_clos_ok_"count_str");\n"
            "L_error_lambda_args_count_"count_str":\n"
            "L_clos_ok_"count_str":\n"
            "POP (FP);\n"
            "RETURN;\n"     ;return to caller
            "L_clos_exit_"count_str":\n"
            ))))

(define code-gen-lambda-opt
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let* (
                (must_params (cadr pe))
                (num_must_params (length must_params))
                (optional_params (+ num_must_params 1))
                (num_must_params_str (number->string num_must_params))
                (optional_params_str (number->string optional_params))
                (body (cadddr pe))
                (count_str (number->string count))
                (major_str (number->string major)))
            (string-append 
            "\n\n//----------LAMBDA-OPT----------//\n\n"
            "MOV (R1, FPARG(0));\n" ;env
            "PUSH (IMM(1+"major_str"));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (R2, R0);\n"
            (letrec ((shallow_copy 
                        (lambda (i j)
                            (let ((i_str (number->string i))
                                    (j_str (number->string j)))
                                (if (>= i major)
                                    ""
                                    (string-append 
                                        "MOV (R4, INDD(R1," i_str"));\n"
                                        "MOV (INDD(R2," j_str"), R4);\n"
                                        (shallow_copy (+ i 1) (+ j 1))))))))
                (shallow_copy 0 1))
            "MOV (R3, FPARG(1));\n" ;number of argumets
            "PUSH (R3);\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R2, 0), R0);\n"

            "MOV (R6, 0);\n" ;i
            "MOV (R7, 2);\n" ;j
            "L_clos_loop_"count_str":\n"
            "CMP (R6, R3);\n"
            "JUMP_GE (L_clos_loop_end_"count_str");\n"
            "MOV (R4, (INDD(R2, 0)));\n"
            "MOV (R5, FPARG(R7));\n"
            "MOV (INDD(R4, R6), R5);\n"
            "ADD (R6, IMM(1));\n"
            "ADD (R7, IMM(1));\n"
            "JUMP (L_clos_loop_"count_str");\n"
            "L_clos_loop_end_"count_str":\n"

            "PUSH (IMM(3));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R0, 0), IMM(T_CLOSURE));\n"
            "MOV (INDD(R0 ,1), R2);\n"  ;ext. env
            
            "MOV (INDD(R0, 2), LABEL(L_clos_body_"count_str"));\n"
            "JUMP (L_clos_exit_"count_str");\n"
            
            "L_clos_body_"count_str":\n"
            "PUSH (FP);\n"
            "MOV (FP,SP);\n"

            ;FIX STACK:
            "MOV (R1, SOB_NIL);\n"
            "MOV (R6, IMM(FPARG(1)));\n"
            "L_clos_fix_stack_loop_"count_str":\n"
            "CMP (R6, "num_must_params_str");\n"
            "JUMP_LE (L_clos_fix_stack_out_"count_str");\n"
            "PUSH (R1);\n"
            "PUSH (FPARG(R6+1));\n"
            "CALL (MAKE_SOB_PAIR);\n"
            "DROP (2);\n"
            "MOV (R1, R0);\n"
            "SUB (R6, IMM(1));\n"
            "JUMP (L_clos_fix_stack_loop_"count_str");\n"
            "L_clos_fix_stack_out_"count_str":\n"

            "MOV (FPARG(2+"num_must_params_str"),R1);\n"
            "MOV (FPARG(1)," optional_params_str");\n"

            "CMP (FPARG(1), IMM("optional_params_str"));\n"
            "JUMP_NE (L_error_lambda_args_count_"count_str");\n"
            (code-gen body (+ major 1) const_tab global_tab)
            "JUMP (L_clos_ok_"count_str");\n"
            "L_error_lambda_args_count_"count_str":\n"
            "L_clos_ok_"count_str":\n"
            "POP (FP);\n"
            "RETURN;\n"     ;return to caller
            "L_clos_exit_"count_str":\n"
            ))))
            
            
(define code-gen-lambda-var 
    (lambda (pe major const_tab global_tab)
        (set! count (+ count 1))
        (let* (
                (optional_params 1)
                (optional_params_str (number->string optional_params))
                (body (caddr pe))
                (count_str (number->string count))
                (major_str (number->string major)))
            (string-append 
            "\n\n//----------LAMBDA-VAR----------//\n\n"
            "MOV (R1, FPARG(0));\n" ;env
            "PUSH (IMM(1+"major_str"));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (R2, R0);\n"
            (letrec ((shallow_copy 
                        (lambda (i j)
                            (let ((i_str (number->string i))
                                    (j_str (number->string j)))
                                (if (>= i major)
                                    ""
                                    (string-append 
                                        "MOV (R4, INDD(R1," i_str"));\n"
                                        "MOV (INDD(R2," j_str"), R4);\n"
                                        (shallow_copy (+ i 1) (+ j 1))))))))
                (shallow_copy 0 1))
            "MOV (R3, FPARG(1));\n" ;number of argumets
            "PUSH (R3);\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R2, 0), R0);\n"

            "MOV (R6, 0);\n" ;i
            "MOV (R7, 2);\n" ;j
            "L_clos_loop_"count_str":\n"
            "CMP (R6, R3);\n"
            "JUMP_GE (L_clos_loop_end_"count_str");\n"
            "MOV (R4, (INDD(R2, 0)));\n"
            "MOV (R5, FPARG(R7));\n"
            "MOV (INDD(R4, R6), R5);\n"
            "ADD (R6, IMM(1));\n"
            "ADD (R7, IMM(1));\n"
            "JUMP (L_clos_loop_"count_str");\n"
            "L_clos_loop_end_"count_str":\n"

            "PUSH (IMM(3));\n"
            "CALL (MALLOC);\n"
            "DROP (1);\n"
            "MOV (INDD(R0, 0), IMM(T_CLOSURE));\n"
            "MOV (INDD(R0, 1), R2);\n"  ;ext. env
            
            "MOV (INDD(R0, 2),LABEL(L_clos_body_"count_str"));\n"
            "JUMP (L_clos_exit_"count_str");\n"
            
            "L_clos_body_"count_str":\n"
            "PUSH (FP);\n"
            "MOV (FP,SP);\n"

            ;FIX STACK:
            "MOV (R1, SOB_NIL);\n"
            "MOV (R6, IMM(FPARG(1)));\n"
            "L_clos_fix_stack_loop_"count_str":\n"
            "CMP (R6, 0);\n"
            "JUMP_LE (L_clos_fix_stack_out_"count_str");\n"
            "PUSH (R1);\n"
            "PUSH (FPARG(R6+1));\n"
            "CALL (MAKE_SOB_PAIR);\n"
            "DROP (2);\n"
            "MOV (R1, R0);\n"
            "SUB (R6, IMM(1));\n"
            "JUMP (L_clos_fix_stack_loop_"count_str");\n"
            "L_clos_fix_stack_out_"count_str":\n"

            "MOV (FPARG(2), R1);\n"
            "MOV (FPARG(1)," optional_params_str");\n"

            "CMP (FPARG(1), IMM("optional_params_str"));\n"
            "JUMP_NE (L_error_lambda_args_count_"count_str");\n"
            (code-gen body (+ major 1) const_tab global_tab)
            "JUMP (L_clos_ok_"count_str");\n"
            "L_error_lambda_args_count_"count_str":\n"
            "L_clos_ok_"count_str":\n"
            "POP (FP);\n"
            "RETURN;\n"     ;return to caller
            "L_clos_exit_"count_str":\n"
                    ))))
                    
(define code-gen-pvar
    (lambda (pe major)
        (let* ((minor (caddr pe)) 
                (minor_str (number->string minor)))
        (string-append
            "\n\n//----------PVAR----------//\n\n"
            "MOV (R0, FPARG(2+"minor_str"));\n" ;the minor's argument
            ))))
            
(define code-gen-bvar
    (lambda (pe major)
        (let* ((major (caddr pe))
                (minor (cadddr pe))
                (major_str (number->string major))
                (minor_str (number->string minor)))
        (string-append
            "\n\n//----------BVAR----------//\n\n"
            "MOV (R0, FPARG(0));\n" ;env
            "MOV (R0, INDD(R0," major_str"));\n"            
            "MOV (R0, INDD(R0," minor_str"));\n"))))

(define code-gen-fvar
    (lambda (pe major global_tab)
        (let* ((var_name (cadr pe))
               (address (fvar_get_address_by_name var_name global_tab)))
            (string-append
                "\n\n//----------FVAR----------//\n\n"
                "MOV (R0, IND("(number->string address)"));\n"))))


            
(define code-gen-set-pvar
    (lambda (pe major const_tab global_tab)
        (let* ((complete_var (cadr pe))
                (minor (caddr complete_var))
                (value (caddr pe)) 
                (minor_str (number->string minor)))
        (string-append
            "\n\n//----------SET-PVAR----------//\n\n"
            (code-gen value major const_tab global_tab)
            "MOV (FPARG(2+"minor_str"), R0);\n"
            "MOV (R0, SOB_VOID);\n" 
            ))))
            
(define code-gen-set-bvar
    (lambda (pe major const_tab global_tab)
        (let* ((complete_var (cadr pe))
                (minor (cadddr complete_var))
                (major (caddr complete_var))
                (value (caddr pe)) 
                (minor_str (number->string minor))
                (major_str (number->string major)))
        (string-append
            "\n\n//----------SET-BVAR----------//\n\n"
            (code-gen value major const_tab global_tab)
            "MOV (R1, FPARG(0));\n" ;env
            "MOV (R1, INDD(R1," major_str"));\n"            
            "MOV (INDD(R1," minor_str"), R0);\n"
            "MOV (R0, SOB_VOID);\n"
            ))))

(define code-gen-set-fvar
    (lambda (pe major const_tab global_tab)
        (let* ((complete_var (cadr pe))
               (var_name (cadr complete_var))
               (value (caddr pe))
               (address (fvar_get_address_by_name var_name global_tab)))
        (string-append
            "\n\n//----------SET/DEFINE-FVAR----------//\n\n"
            (code-gen value major const_tab global_tab)          
            "MOV (IND("(number->string address)"), R0);\n"
            "MOV (R0, SOB_VOID);\n"
            ))))

(define code-gen-box-get-pvar
    (lambda (pe major)
        (let* ((complete_var (cadr pe))
                (minor (caddr complete_var))
                (minor_str (number->string minor)))
        (string-append
            "\n\n//----------BOX-GET-PVAR----------//\n\n"
            "MOV (R0, FPARG(2+"minor_str"));\n"
            "MOV (R0, IND(R0));\n"
            ))))
            
(define code-gen-box-get-bvar
    (lambda (pe major)
               (let* ((complete_var (cadr pe))
                      (minor (cadddr complete_var))
                      (major (caddr complete_var))
                      (minor_str (number->string minor))
                      (major_str (number->string major)))
                (string-append
                    "\n\n//----------BOX-GET-BVAR----------//\n\n"
                    "MOV (R0, FPARG(0));\n" ;env
                    "MOV (R0, INDD(R0,"major_str"));\n"         
                    "MOV (R0, INDD(R0,"minor_str"));\n"
                    "MOV (R0, IND(R0));\n"
                    ))))
                    
(define code-gen-box-set-pvar
    (lambda (pe major const_tab global_tab)
        (let* ((complete_var (cadr pe))
                (minor (caddr complete_var))
                (value (caddr pe)) 
                (minor_str (number->string minor)))
        (string-append
            "\n\n//----------BOX-SET-PVAR----------//\n\n"
            (code-gen value major const_tab global_tab)
            "MOV (R1, FPARG(2+"minor_str"))"
            "MOV (IND(R1), R0);\n"
            "MOV (R0, SOB_VOID);\n"
            ))))
            
(define code-gen-box-set-bvar
    (lambda (pe major const_tab global_tab)
        (let* ((complete_var (cadr pe))
                (minor (cadddr complete_var))
                (major (caddr complete_var))
                (value (caddr pe)) 
                (minor_str (number->string minor))
                (major_str (number->string major)))
        (string-append
            "\n\n//----------BOX-SET-BVAR----------//\n\n"
            (code-gen value major const_tab global_tab)
            "MOV (R1, FPARG(0));\n" ;env
            "MOV (R1, INDD(R1,"major_str"));\n" 
            "MOV (R2, INDD(R1,"minor_str"));\n"         
            "MOV (IND(R2), R0);\n"
            "MOV (R0, SOB_VOID);\n"
            ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAIN-FLOW;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compile-scheme-file
    (lambda (scm_src_file asm_target_file)
        (let* ((scm_content (file->string scm_src_file))
               (match_and_remain (test-string <Sexpr> scm_content))
               (sexprs_list (create_sexprs_list (string-append "(begin " 
                                                                scm_content
                                                                ")")))
               (super_parsed_list (parsed_and_hw3 sexprs_list))
               (constant_table (build_constant_table super_parsed_list))
               (global_var_table (build_global_var_table super_parsed_list (find_next_available_address constant_table)))
               (list_table_of_rep_string 
                         (build_initial_table_list_string constant_table 
                                                 (find_next_available_address_after_global global_var_table 
                                                                                           constant_table)))
               (super_parsed_list_with_fvar_define (add_fvar_define super_parsed_list global_var_table))
               (asm_instructions_list (build_asm_insts_list super_parsed_list_with_fvar_define constant_table global_var_table))
               (asm_instructions_string (build_asm_insts_string asm_instructions_list))
               (asm_with_const_global_string_table (add_const_global_string_table constant_table 
                                                                                  global_var_table 
                                                                                  list_table_of_rep_string 
                                                                                  asm_instructions_string))
               (final_asm (add_prologue_epilgue asm_with_const_global_string_table)))
   (string->file final_asm asm_target_file))))
;super_parsed_list)))

(define build_asm_insts_list
    (lambda (super_parsed_list const_tab global_tab)
        (if (null? super_parsed_list)
            (list)
            (cons (code-gen (car super_parsed_list) 0 const_tab global_tab)
                  (build_asm_insts_list (cdr super_parsed_list) const_tab global_tab)))))

(define build_asm_insts_string
    (lambda (insts_list)
        (if (null? insts_list)
            ""
            (string-append (car insts_list) (build_asm_insts_string (cdr insts_list))))))

(define add_const_global_string_table 
    (lambda (constant_table global_var_table string_table asm_instructions_string)
        (string-append (build_asm_constant_table constant_table)
                       (build_asm_global_table global_var_table (find_next_available_address constant_table))
                       ;(build_asm_string_table string_table (find_next_available_address_after_global global_var_table constant_table))
                        asm_instructions_string)))

(define parsed_and_hw3 
    (lambda (sexprs_list)
        (if (null? sexprs_list)
            (list)
            (cons (annotate-tc
                     (pe->lex-pe
                       (box-set
                          (remove-applic-lambda-nil
                            (eliminate-nested-defines 
                                (parse (car sexprs_list)))))))
                   (parsed_and_hw3 (cdr sexprs_list))))))

(define create_sexprs_list
    (lambda (scm_content)
        (if (equal? scm_content "")
            (list)
            (let* ((match_and_remain (test-string <Sexpr> scm_content))
                   (match (cadar match_and_remain))
                   (remain (cadadr match_and_remain)))
                (cons match (create_sexprs_list remain))))))



(define file->string
    (lambda (in-file)
        (let ((in-port (open-input-file in-file)))
            (letrec ((run
                        (lambda ()
                            (let ((ch (read-char in-port)))
                                (if (eof-object? ch)
                                    (begin
                                        (close-input-port in-port)
                                        '())
                                    (cons ch (run)))))))

                (list->string (run))))))


(define string->file
    (lambda (string out-file)
        (let ((out-port (open-output-file out-file)))
            (begin (display string out-port)
                   (close-output-port out-port)))))


; /* change to 0 for no debug info to be printed: */
; #define DO_SHOW 1

(define add_prologue_epilgue
    (lambda (asm_insts_string)
        (string-append "
#include <stdio.h>
#include <stdlib.h>

#include \"cisc.h\"

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include \"debug_macros.h\"

int main()
{
START_MACHINE;

JUMP(CONTINUE);

#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"
#include \"scheme.lib\"

CONTINUE:

PUSH(FP);
MOV(FP, SP);

 #define SOB_VOID 1
 #define SOB_NIL 2
 #define SOB_FALSE 5
 #define SOB_TRUE 3

"
 asm_insts_string

"\n\n//----------PRINT----------//\n\n"

"
CMP(R0, SOB_VOID);
JUMP_EQ(DONT_PRINT);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
OUT(2,10);
DONT_PRINT:

POP(FP);

/*TODO - remove info - for debug*/
//INFO;

STOP_MACHINE;

return 0;
}"
                        )))

                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANT-TABLE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      
                        

(define build_constant_table
    (lambda (super_parsed_list)
        (remove-dups (build_const_table_for_each_sexpr super_parsed_list))))

(define remove-dups
    (lambda (lst)
        (letrec ((run (lambda (reverse_lst) 
                            (if (null? reverse_lst)
                                reverse_lst
                                (if (member (car reverse_lst) (cdr reverse_lst))
                                    (run (cdr reverse_lst))
                                    (cons (car reverse_lst) (run (cdr reverse_lst))))))))
            (reverse (run (reverse lst))))))

(define build_const_table_for_each_sexpr
    (lambda (super_parsed_list)
        (if (null? super_parsed_list)
            (list)
            (append (build_const_table_for_sexpr (car super_parsed_list))
                    (build_const_table_for_each_sexpr (cdr super_parsed_list))))))

(define build_const_table_for_sexpr
    (lambda (super_parsed_sexpr)
        (let* ((full_const_list (create_const_list super_parsed_sexpr))
               (const_list_no_dups (remove-dups full_const_list))
               (full_sub_const_list (create_sub_const_list const_list_no_dups))
               (sub_const_list_no_dups (remove-dups full_sub_const_list))
               (final_list (build_final_const_list sub_const_list_no_dups)))
            final_list)))

(define create_const_list
    (lambda (sp_sexpr)
        (cond ((or (null? sp_sexpr) (atom? sp_sexpr)) (list))
              ((and (equal? (car sp_sexpr) 'const)
                    (not (or    (equal? sp_sexpr `(const ,(void)))
                                (equal? sp_sexpr `(const ()))
                                (equal? sp_sexpr `(const #f))
                                (equal? sp_sexpr `(const #t))))) 
               (cdr sp_sexpr))
              (else (append (create_const_list (car sp_sexpr))
                            (create_const_list (cdr sp_sexpr)))))))

(define create_sub_const_list
    (lambda (const_list)
        (letrec ((run (lambda (element)
                        (cond ((pair? element)
                                `(,@(run (car element)) ,@(run (cdr element)) ,element))
                              ((vector? element)
                                `(,@(apply append (map run (vector->list element)))
                                  ,element))
                              ((symbol? element)
                                `(,(symbol->string element) ,element))
                              ((or (equal? #t element) (equal? #f element))
                                '())
                              (else `(,element))))))
            (remove_nil (flatten (map run const_list))))))

(define flatten
    (lambda (lst)
        (cond ((null? lst) lst)
              ((list? (car lst)) `(,@(car lst) ,@(flatten (cdr lst))))
              (else (cons (car lst) (flatten (cdr lst)))))))

(define remove_nil
    (lambda (lst)
        (if (null? lst)
            lst
            (if (equal? (car lst) '())
                (remove_nil (cdr lst))
                (cons (car lst) (remove_nil (cdr lst)))))))


(define build_final_const_list
    (lambda (sub_const_list_no_dups)
        (let* ((firsts (build_firsts))
               (rests (build_rest sub_const_list_no_dups firsts 7)))
            (cons `(1 ,(void) (T_VOID)) rests))))


(define build_firsts
    (lambda ()
        (list 
              `(2 () (T_NIL))
              `(3 #t (T_BOOL 1))
              `(5 #f (T_BOOL 0)))))

(define build_rest
    (lambda (sub_list acc_list next_available)
        (if (null? sub_list)
            acc_list
            (let* ((current_element (build_const_list_element (car sub_list) next_available acc_list))
                  (element_length (length (caddr current_element))))
                 (build_rest (cdr sub_list)
                            (append acc_list `(,current_element))
                            (+ next_available element_length))))))

(define build_const_list_element
    (lambda (element next_available acc_list)
        (cond ((and (number? element) (integer? element))
               `(,next_available ,element (T_INTEGER ,element)))
              ((and (number? element) (not (integer? element)))
               `(,next_available ,element (T_FRACTION ,element))) ;todo: change
              ((char? element)
                `(,next_available ,element (T_CHAR ,element)))
              ((string? element)
                (let ((list_of_chars (string->list element)))
                    `(,next_available ,element (T_STRING ,(string-length element) ,@list_of_chars))))
              ((vector? element)
                (let ((list_of_address_vec (letrec ((run (lambda (lst)
                                                            (if (null? lst)
                                                                (list)
                                                                (cons (search_element (car lst) acc_list)
                                                                      (run (cdr lst)))))))
                                                (run (vector->list element)))))
               `(,next_available ,element (T_VECTOR ,(vector-length element) ,@list_of_address_vec))))
              ((symbol? element)
                (let ((represent_string_address (search_element (symbol->string element) acc_list)))
                  `(,next_available ,element (T_SYMBOL ,represent_string_address))))
              ((pair? element)
               `(,next_available ,element (T_PAIR ,(search_element (car element) acc_list)
                                                  ,(search_element (cdr element) acc_list))))
              (else '()))))

(define search_element
    (lambda (element lst)
        (if (null? lst)
            0
            (let* ((current (car lst))
                   (current_value (cadr current)))
                (if (equal? current_value element)
                    (car current)
                    (search_element element (cdr lst)))))))
                    
(define build_asm_constant_table
    (lambda (constant_table)
        (let* ((last_element (car (reverse constant_table)))
               (address (car last_element))
               (represent (caddr last_element))
               (represent_length (length represent))
               (malloc_need (- (+ address represent_length) 1))
               (malloc_need_str (number->string malloc_need)))
            (string-append 
                "\n\n//----------CONST-TABLE----------//\n\n"
                "PUSH ("malloc_need_str");\n"
                "CALL (MALLOC);\n"
                "DROP (1);\n"
                (letrec ((run (lambda (lst)
                                    (if (null? lst)
                                        ""  
                                         (let* ((element (car lst))
                                                (address (car element))
                                                (rep_lst (caddr element)))
                                         
                                            (string-append (build_string_for_element_memory address rep_lst)
                                                           (run (cdr lst))))))))
                    (run constant_table)))
                
            )))

(define find_next_available_address
    (lambda (constant_table)
         (let* ((last_element (car (reverse constant_table)))
                (address (car last_element))
                (represent (caddr last_element))
                (represent_length (length represent)))
            (+ address represent_length))))


(define build_string_for_element_memory
    (lambda (address rep_lst)
        (letrec ((run (lambda (lst num)
                        (if (null? lst)
                            ""
                             (let ((string_rep (cond ((symbol? (car lst)) (symbol->string (car lst)))
                                                     ((number? (car lst)) (number->string (car lst)))
                                                     ((char? (car lst)) (number->string (char->integer (car lst))))
                                                     (else ""))))
                                (string-append
                                    "MOV (IND("(number->string num)"), "string_rep");\n"
                                    (run (cdr lst) (+ num 1))))))))
            (run rep_lst address))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL-TABLE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
            
(define build_global_var_table
    (lambda (super_parsed_list next_available_address)
        (remove-dups (build_global_table_for_each_sexpr super_parsed_list next_available_address))))


(define build_global_table_for_each_sexpr
    (lambda (super_parsed_list next_available_address)
        (if (null? super_parsed_list)
            (list)
            (append (build_global_table_for_sexpr (car super_parsed_list) next_available_address)
                    (build_global_table_for_each_sexpr (cdr super_parsed_list) next_available_address)))))

(define build_global_table_for_sexpr
    (lambda (super_parsed_sexpr next_available_address)
        (let* ((full_global_list (create_global_list super_parsed_sexpr))
               (global_list_no_dups (remove-dups full_global_list))
               (final_list (build_final_global_list global_list_no_dups next_available_address)))
            final_list)))

(define create_global_list
    (lambda (sp_sexpr)
        (cond ((or (null? sp_sexpr) (atom? sp_sexpr)) (list))
              ((equal? (car sp_sexpr) 'fvar) (cdr sp_sexpr))
              (else (append (create_global_list (car sp_sexpr))
                            (create_global_list (cdr sp_sexpr)))))))

(define build_final_global_list
    (lambda (global_list next_available_address)
        (if (null? global_list)
            (list)
            (cons `(,(car global_list) ,next_available_address)
                     (build_final_global_list (cdr global_list) (+ 1 next_available_address))))))


(define build_asm_global_table
    (lambda (global_table start_address)
        (if (null? global_table)
            "\n\n//----------GLOBAL-TABLE----------//\n\n"
            (let* ((last_element (car (reverse global_table)))
                   (address (cadr last_element))
                   (malloc_need (- (+ address 1) start_address))
                   (malloc_need_str (number->string malloc_need)))
                (string-append 
                    "\n\n//----------GLOBAL-TABLE----------//\n\n"
                    "PUSH ("malloc_need_str");\n"
                    "CALL (MALLOC);\n"
                    "DROP (1);\n"
                    ))
            )))

(define fvar_get_address_by_name
    (lambda (name global_table)
        (if (null? global_table)
            0
             (let* ((element (car global_table))
                    (element_name (car element)))
                (if (equal? name element_name)
                    (cadr element)
                    (fvar_get_address_by_name name (cdr global_table)))))))

(define add_fvar_define
    (lambda (super_parsed_list global_table)
        (letrec ((run (lambda (lst)
                        (if (null? lst)
                            lst
                            (let* ((element (car lst))
                                   (address (cadr element))
                                   (name (car element)))
                                (cons `(def (fvar ,name) ,name)
                                       (run (cdr lst))))))))
            (append (run global_table)
                  super_parsed_list))))

(define find_next_available_address_after_global
    (lambda (global_table const_tab)
         (if (null? global_table)
             (find_next_available_address const_tab)
             (let* ((last_element (car (reverse global_table)))
                    (address (cadr last_element)))
                (+ address 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REP-STRING-LIST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build_initial_table_list_string
    (lambda (const_tab next_available)
        (let ((string_list (build_initial_list_string const_tab)))
            (letrec ((run (lambda (lst next_add)
                            (if (null? lst)
                                lst
                                (cons `(,(car lst) ,next_add)
                                       (run (cdr lst) (+ next_add 1)))))))
                (run string_list next_available)))))

(define build_initial_list_string
    (lambda (const_tab)
        (if (null? const_tab)
            (list)
            (let* ((element (car const_tab))
                   (element_rep (caddr element))
                   (element_type (car element_rep)))
                (if (equal? element_type 'T_SYMBOL)
                    (cons (cadr element_rep) 
                          (build_initial_list_string (cdr const_tab)))
                    (build_initial_list_string (cdr const_tab)))))))

(define build_asm_string_table
    (lambda (string_table start_address)
        (if (null? string_table)
            "\n\n//----------STRING-TABLE----------//\n\n"
            (let* ((last_element (car (reverse string_table)))
                   (address (cadr last_element))
                   (malloc_need (- (+ address 1) start_address))
                   (malloc_need_str (number->string malloc_need)))
                (string-append 
                    "\n\n//----------STRING-TABLE----------//\n\n"
                    "PUSH ("malloc_need_str");\n"
                    "CALL (MALLOC);\n"
                    "DROP (1);\n"
                    (letrec ((run (lambda (lst)
                                        (if (null? lst)
                                            ""  
                                             (let* ((element (car lst))
                                                    (address (cadr element))
                                                    (string_add (car element)))
                                             
                                                (string-append "MOV (IND("(number->string address)"), "(number->string string_add)");\n"
                                                               (run (cdr lst))))))))
                        (run string_table)))
                    
                ))))


