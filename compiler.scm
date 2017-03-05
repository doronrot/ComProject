(load "compiler_hw3.scm")

;;;none-clear;;;
;starg (inlib/char&io/ in some files)
;addr(0) (in lib/system/malloc)


(define count 0)

(define code-gen
	(lambda (pe major const_tab)
		(cond 
			 ;void
			  ((equal? pe `(const ,(void)))
				(string-append
					; "CALL (MAKE_SOB_VOID);\n"
					"MOV(R0, IMM(SOB_VOID));\n"
					; "PUSH(R0);\n"
					; "CALL(WRITE_SOB_VOID);\n"
					; "POP(R0);\n"
					))
			  ;list
			   ((equal? pe `(const ()))
			   	 (string-append
				    ; "CALL (MAKE_SOB_NIL);\n"
				    "MOV(R0, IMM(SOB_NIL));\n"
	 				; "PUSH(R0);\n"
	 				; "CALL(WRITE_SOB_NIL);\n"
	 				; "POP(R0);\n"
	 				))
			   ;#f
			   ((equal? pe `(const #f))
			   	 (string-append
			   	 ; 	"PUSH (IMM(0));\n"
				    ; "CALL (MAKE_SOB_BOOL);\n"
				    ; "DROP (1);\n"
				    "MOV(R0, IMM(SOB_TRUE));\n"
	 				; "PUSH(R0);\n"
	 				; "CALL(WRITE_SOB_BOOL);\n"
	 				; "POP(R0);\n"
	 				))
			   ;#t
			   ((equal? pe `(const #t)) 
			  	 (string-append
			   	 ; 	"PUSH (IMM(1));\n"
				    ; "CALL (MAKE_SOB_BOOL);\n"
				    ; "DROP (1);\n"
				    "MOV(R0, IMM(SOB_FALSE));\n"
	 				; "PUSH(R0);\n"
	 				; "CALL(WRITE_SOB_BOOL);\n"
	 				; "POP(R0);\n"
	 				))
			   ;if
			  	((and (pair? pe) 
			  	    (equal? (car pe) 'if3))
			  	 (set! count (+ count 1))
			  	 (let ((test (cadr pe))
			  	 	   (dit (caddr pe))
			  	 	   (dif (cadddr pe))
			  	 	   (count_str (number->string count)))
			  	 	(string-append (code-gen test major const_tab)
			  	 				    "CMP (R0, IMM(SOB_FALSE));\n"
			  	 				    "JUMP_EQ (L_if3_else_"count_str");\n"
			  	 				    (code-gen dit major const_tab)
			  	 				    "JUMP (L_if3_exit_"count_str");\n"
			  	 				    "L_if3_else_"count_str":\n"
			  	 				    (code-gen dif major const_tab)
			  	 				    "L_if3_exit_"count_str":\n")))
			  ;seq
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'seq))
			   (let ((seq_body (cadr pe)))
			   	 (letrec ((run (lambda (lst)
			   	 					(if (null? lst)
			   	 						""
			   	 						(string-append (code-gen (car lst) major const_tab)
			   	 									   (run (cdr lst)))))))
			   	 	(run seq_body))))
			  	;or
			  ((and (pair? pe)
			  		(equal? (car pe) 'or))
			  	(set! count (+ count 1))
			  	(let ((or_exps (cadr pe))
			  		  (count_str (number->string count)))
			  		(letrec ((run (lambda (lst)
			  						 (if (equal? (length lst) 1)
			  						 	 (string-append (code-gen (car lst) major const_tab)
			  						 	 				"L_or_exit_"count_str":\n"
			  						 	 				; "PUSH (R0);\n"
			  						 	 				; "CALL (WRITE_SOB);\n"
			  						 	 				; "DROP (1);\n"
			  						 	 				)
			  						 	 (string-append (code-gen (car lst) major const_tab)
			  						 	 				"CMP(R0, IMM(SOB_FALSE));\n"
			  						 	 				"JUMP_NE(L_or_exit_"count_str");\n"
			  						 	 				(run (cdr lst)))))))
			  			 (run or_exps))))
			  ;applic
			  ((and (pair? pe) 
			  		(equal? (car pe) 'applic))
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
  						 				   (code-gen proc major const_tab)
  						 				   "CMP (INDD(R0, 0),IMM(T_CLOSURE));\n"
  						 				   "JUMP_NE (L_error_cannot_apply_non_clos_"count_str");\n"
  						 				   "PUSH (INDD(R0,1));\n"
  						 				   "MOV (R4, INDD(R0,2));\n"
  						 				   "CALLA (R4);\n"
  						 				   "DROP (1);\n"
  						 				   "POP (R1);\n"
  						 				   "DROP(R1);\n"
  						 				   "DROP (1);\n"
  						 				   "JUMP (L_applic_exit_"count_str");\n"
  						 				   "L_error_cannot_apply_non_clos_"count_str":\n"
  						 				   "L_applic_exit_"count_str":\n")
			  						 	 (string-append 
			  						 	   (code-gen (car lst) major const_tab)
  						 				   "PUSH (R0);\n"
  						 				   (run (cdr lst)))))))
			  			(string-append	
			  					"PUSH(SOB_NIL);\n"
								(run (reverse args))))))
			  ;lambda-simple
			  ((and (pair? pe) 
			  		(equal? (car pe) 'lambda-simple))
			  	(set! count (+ count 1))
			  	(let* (
			  		   (params (cadr pe))
			  		   (num_params (length params))
			  		   (num_params_str (number->string num_params))
			  		   (body (caddr pe))
			  		   (count_str (number->string count))
				  	   (major_str (number->string major)))
			  	  (string-append 
					"MOV (R1,FPARG(0));\n"	;env
					"PUSH (IMM(1+"major_str"));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (R2, R0);\n"
					(letrec ((shallow_copy 
								(lambda (i j)
								   (let ((i_str (number->string i))
								   		 (j_str (number->string j)))
									   (if (>= i major)
									   	   ""
									   	   (string-append 
									   	   	  "MOV (R4, INDD(R1,"i_str"));\n"
									   	   	  "MOV (INDD(R2,"j_str"), R4);\n"
									   	   	  (shallow_copy (+ i 1) (+ j 1))))))))
					   	(shallow_copy 0 1))
					"MOV(R3,FPARG(1));\n"	;number of argumets
					"PUSH(R3);\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R2,0), R0);\n"

					"MOV (R6, 0);\n" ;i
					"MOV (R7, 2);\n" ;j
					"L_clos_loop_"count_str":\n"
					"CMP (R6, R3);\n"
					"JUMP_GE (L_clos_loop_end_"count_str");\n"
					"MOV (R4, (INDD(R2,0)));\n"
					"MOV (R5, FPARG(R7));\n"
					"MOV (INDD(R4, R6), R5);\n"
					"ADD (R6, IMM(1));\n"
					"ADD (R7, IMM(1));\n"
					"JUMP (L_clos_loop_"count_str");\n"
					"L_clos_loop_end_"count_str":\n"

					"PUSH (IMM(3));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R0,0),IMM(T_CLOSURE));\n"
					"MOV (INDD(R0,1),R2);\n"	;ext. env
					
					"MOV (INDD(R0,2),LABEL(L_clos_body_"count_str"));\n"
					"JUMP (L_clos_exit_"count_str");\n"
					
					"L_clos_body_"count_str":\n"
					"PUSH (FP);\n"
					"MOV (FP,SP);\n"

					"CMP (FPARG(1),IMM("num_params_str"));\n"
					"JUMP_NE (L_error_lambda_args_count_"count_str");\n"
					(code-gen body (+ major 1) const_tab)
					"JUMP (L_clos_ok_"count_str");\n"
					"L_error_lambda_args_count_"count_str":\n"
					"L_clos_ok_"count_str":\n"
					"POP (FP);\n"
					"RETURN;\n"		;return to caller
					"L_clos_exit_"count_str":\n"
					)))
			  ;pvar
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'pvar))
			   (let* ((minor (caddr pe)) 
			   		  (minor_str (number->string minor)))
			   	(string-append
			   		"MOV (R0, FPARG(2+"minor_str"));\n" ;the minor's argument
			   		)))			  
			  ;bvar
			  ((and (pair? pe) 
			  		(equal? (car pe) 'bvar))
			   (let* ((major (caddr pe))
			   		  (minor (cadddr pe))
			   		  (major_str (number->string major))
			   		  (minor_str (number->string minor)))
			   	(string-append
			   		"MOV (R0, FPARG(0));\n" ;env
			   		"MOV (R0, INDD(R0,"major_str"));\n"	   		
			   		"MOV (R0, INDD(R0,"minor_str"));\n")))
			  ;set pvar
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'set)
			  	    (equal? (caadr pe) 'pvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (caddr complete_var))
			   		  (value (caddr pe)) 
			   		  (minor_str (number->string minor)))
			   	(string-append
			   		(code-gen value major const_tab)
			   		"MOV (FPARG(2+"minor_str"), R0);\n"
			   		"MOV (R0, SOB_VOID);\n" 
			   		)))
			  ;set bvar
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'set)
			  	    (equal? (caadr pe) 'bvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (cadddr complete_var))
			   		  (major (caddr complete_var))
			   		  (value (caddr pe)) 
			   		  (minor_str (number->string minor))
			   		  (major_str (number->string major)))
			   	(string-append
			   		(code-gen value major const_tab)
			   		"MOV (R1, FPARG(0));\n" ;env
			   		"MOV (R1, INDD(R1,"major_str"));\n"	   		
			   		"MOV (INDD(R1,"minor_str"), R0);\n"
			   		"MOV (R0, SOB_VOID);\n"
			   		)))
			  ;box-get pvar ;TODO - CHECK!! AFTER HANDLE WITH SEQ
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'box-get)
			  	    (equal? (caadr pe) 'pvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (caddr complete_var))
			   		  (minor_str (number->string minor)))
			   	(string-append
			   		"MOV (R0, FPARG(2+"minor_str"));\n"
			   		"MOV (R0, IND(R0));\n"
			   		)))
			  ;box-get bvar ;TODO - CHECK!! AFTER HANDLE WITH SEQ
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'box-get)
			  	    (equal? (caadr pe) 'bvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (cadddr complete_var))
			   		  (major (caddr complete_var))
			   		  (minor_str (number->string minor))
			   		  (major_str (number->string major)))
			   	(string-append
			   		"MOV (R0, FPARG(0));\n" ;env
			   		"MOV (R0, INDD(R0,"major_str"));\n"	   		
			   		"MOV (R0, INDD(R0,"minor_str"));\n"
			   		"MOV (R0, IND(R0));\n"
			   		)))
			  ;box-set pvar ;TODO - CHECK!! AFTER HANDLE WITH SEQ
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'box-set)
			  	    (equal? (caadr pe) 'pvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (caddr complete_var))
			   		  (value (caddr pe)) 
			   		  (minor_str (number->string minor)))
			   	(string-append
			   		(code-gen value major const_tab)
			   		"MOV (R1, FPARG(2+"minor_str"))"
			   		"MOV (IND(R1), R0);\n"
			   		"MOV (R0, IMM(T_VOID));\n" ;IMPORTANT TODO!! - AFTER CONST TABLE CHANGE TO SOB_VOID
			   		)))
			  ;box-set bvar ;TODO - CHECK!! AFTER HANDLE WITH SEQ
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'box-set)
			  	    (equal? (caadr pe) 'bvar))
			   (let* ((complete_var (cadr pe))
			   		  (minor (cadddr complete_var))
			   		  (major (caddr complete_var))
			   		  (value (caddr pe)) 
			   		  (minor_str (number->string minor))
			   		  (major_str (number->string major)))
			   	(string-append
			   		(code-gen value major const_tab)
			   		"MOV (R1, FPARG(0));\n" ;env
			   		"MOV (R1, INDD(R1,"major_str"));\n"	
			   		"MOV (R2, INDD(R1,"minor_str"));\n"   		
			   		"MOV (IND(R2), R0);\n"
			   		"MOV (R0, SOB_VOID);\n" ;IMPORTANT TODO!! - AFTER CONST TABLE CHANGE TO SOB_VOID
			   		)))
			  ;const
			  ((and (pair? pe)
			  	    (equal? (car pe) 'const))
			    (let* ((address (search_element (cadr pe) const_tab))
			    	   (address_str (number->string address)))
			    	(string-append
			  			"MOV (R0, IMM("address_str"));\n")
			  		))
			  ;lambda-opt
			  ((and (pair? pe) 
			  		(equal? (car pe) 'lambda-opt))
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
					"MOV (R1,FPARG(0));\n"	;env
					"PUSH (IMM(1+"major_str"));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (R2, R0);\n"
					(letrec ((shallow_copy 
								(lambda (i j)
								   (let ((i_str (number->string i))
								   		 (j_str (number->string j)))
									   (if (>= i major)
									   	   ""
									   	   (string-append 
									   	   	  "MOV (R4, INDD(R1,"i_str"));\n"
									   	   	  "MOV (INDD(R2,"j_str"), R4);\n"
									   	   	  (shallow_copy (+ i 1) (+ j 1))))))))
					   	(shallow_copy 0 1))
					"MOV(R3,FPARG(1));\n"	;number of argumets
					"PUSH(R3);\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R2,0), R0);\n"

					"MOV (R6, 0);\n" ;i
					"MOV (R7, 2);\n" ;j
					"L_clos_loop_"count_str":\n"
					"CMP (R6, R3);\n"
					"JUMP_GE (L_clos_loop_end_"count_str");\n"
					"MOV (R4, (INDD(R2,0)));\n"
					"MOV (R5, FPARG(R7));\n"
					"MOV (INDD(R4, R6), R5);\n"
					"ADD (R6, IMM(1));\n"
					"ADD (R7, IMM(1));\n"
					"JUMP (L_clos_loop_"count_str");\n"
					"L_clos_loop_end_"count_str":\n"

					"PUSH (IMM(3));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R0,0),IMM(T_CLOSURE));\n"
					"MOV (INDD(R0,1),R2);\n"	;ext. env
					
					"MOV (INDD(R0,2),LABEL(L_clos_body_"count_str"));\n"
					"JUMP (L_clos_exit_"count_str");\n"
					
					"L_clos_body_"count_str":\n"
					"PUSH (FP);\n"
					"MOV (FP,SP);\n"

					;FIX STACK:
					"MOV (R1, SOB_NIL);\n"
					"ADD (R6, IMM(FPARG(1)));\n"
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
			  	  	"MOV (FPARG(1),"optional_params_str");\n"

					"CMP (FPARG(1),IMM("optional_params_str"));\n"
					"JUMP_NE (L_error_lambda_args_count_"count_str");\n"
					(code-gen body (+ major 1) const_tab)
					"JUMP (L_clos_ok_"count_str");\n"
					"L_error_lambda_args_count_"count_str":\n"
					"L_clos_ok_"count_str":\n"
					"POP (FP);\n"
					"RETURN;\n"		;return to caller
					"L_clos_exit_"count_str":\n"
					)))
			  ;lambda-var
			  ((and (pair? pe) 
			  		(equal? (car pe) 'lambda-var))
			  	(set! count (+ count 1))
			  	(let* (
			  		   (optional_params 1)
			  		   (optional_params_str (number->string optional_params))
			  		   (body (caddr pe))
			  		   (count_str (number->string count))
				  	   (major_str (number->string major)))
			  	  (string-append 
					"MOV (R1,FPARG(0));\n"	;env
					"PUSH (IMM(1+"major_str"));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (R2, R0);\n"
					(letrec ((shallow_copy 
								(lambda (i j)
								   (let ((i_str (number->string i))
								   		 (j_str (number->string j)))
									   (if (>= i major)
									   	   ""
									   	   (string-append 
									   	   	  "MOV (R4, INDD(R1,"i_str"));\n"
									   	   	  "MOV (INDD(R2,"j_str"), R4);\n"
									   	   	  (shallow_copy (+ i 1) (+ j 1))))))))
					   	(shallow_copy 0 1))
					"MOV(R3,FPARG(1));\n"	;number of argumets
					"PUSH(R3);\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R2,0), R0);\n"

					"MOV (R6, 0);\n" ;i
					"MOV (R7, 2);\n" ;j
					"L_clos_loop_"count_str":\n"
					"CMP (R6, R3);\n"
					"JUMP_GE (L_clos_loop_end_"count_str");\n"
					"MOV (R4, (INDD(R2,0)));\n"
					"MOV (R5, FPARG(R7));\n"
					"MOV (INDD(R4, R6), R5);\n"
					"ADD (R6, IMM(1));\n"
					"ADD (R7, IMM(1));\n"
					"JUMP (L_clos_loop_"count_str");\n"
					"L_clos_loop_end_"count_str":\n"

					"PUSH (IMM(3));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R0,0),IMM(T_CLOSURE));\n"
					"MOV (INDD(R0,1),R2);\n"	;ext. env
					
					"MOV (INDD(R0,2),LABEL(L_clos_body_"count_str"));\n"
					"JUMP (L_clos_exit_"count_str");\n"
					
					"L_clos_body_"count_str":\n"
					"PUSH (FP);\n"
					"MOV (FP,SP);\n"

					;FIX STACK:
					"MOV (R1, SOB_NIL);\n"
					"ADD (R6, IMM(FPARG(1)));\n"
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

					"MOV (FPARG(2),R1);\n"
			  	  	"MOV (FPARG(1),"optional_params_str");\n"

					"CMP (FPARG(1),IMM("optional_params_str"));\n"
					"JUMP_NE (L_error_lambda_args_count_"count_str");\n"
					(code-gen body (+ major 1) const_tab)
					"JUMP (L_clos_ok_"count_str");\n"
					"L_error_lambda_args_count_"count_str":\n"
					"L_clos_ok_"count_str":\n"
					"POP (FP);\n"
					"RETURN;\n"		;return to caller
					"L_clos_exit_"count_str":\n"
					)))
			  ;tc-applic
			  ((and (pair? pe) 
			  		(equal? (car pe) 'tc-applic))
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
  						 				   (code-gen proc major const_tab)
  						 				   "CMP (INDD(R0, 0),IMM(T_CLOSURE));\n"
  						 				   "JUMP_NE (L_error_cannot_apply_non_clos_"count_str");\n"
										   "MOV (R1, FPARG(-2));\n" ;old fp 1 - START FROM HERE
										   "MOV (R2, FPARG(-1));\n" ;ret
										   "PUSH (R2);\n"	
										   "PUSH (R1);\n"
  						 				   ; "PUSH (INDD(R0,1));\n" ;env
  						 				   ; "PUSH (FPARG(-1));\n" ;old return address
  						 				   ; "MOV (R1, FPARG(-2));\n"
  						 				   ; ; ;new frame instead of new:
  						 				   ; "MOV (R2, FPARG(1));\n" ;num of params
  						 				   ; "MOV (R2, (R2+R1));\n" ;start of prev frame
  						 				   "MOV (R3, 0);\n"
  						 				   "L_tc_applic_loop_"count_str":\n"
  						 				   "CMP (R3, IMM("(number->string counter_up)"));\n"
  						 				   "JUMP_EQ (L_tc_applic_loop_exit_"count_str");\n"
  						 				   "MOV (STACK(R1), LOCAL(R3));\n"
  						 				   "ADD (R3, IMM(1));\n"
										   "ADD (R1, IMM(1));\n"
  						 				   "JUMP (L_tc_applic_loop_"count_str");\n"
  						 				   "L_tc_applic_loop_exit_"count_str":\n"
  						 				   "DROP ("(+ 2 (number->string counter_up))");\n"
  						 				   ; ;done
  						 				   "MOV (FP, R1);\n"
  						 				   "JUMPA (INDD(R0,2));\n"
  						 				   "L_error_cannot_apply_non_clos_"count_str":\n"
  						 				   "")

			  						 	 (string-append 
			  						 	   (code-gen (car lst) major const_tab)
  						 				   "PUSH (R0);\n"
  						 				   (run (cdr lst) (+ counter_up 1)))))))
			  		(run (reverse args) 4))))

			  ;else
			  (else "") 
			  	)))


(define compile-scheme-file
	(lambda (scm_src_file asm_target_file)
		(let* ((scm_content (file->string scm_src_file))
			   (match_and_remain (test-string <Sexpr> scm_content))
			   (sexprs_list (create_sexprs_list scm_content))
			   (super_parsed_list (parsed_and_hw3 sexprs_list))
			   (constant_table (build_constant_table super_parsed_list))
			   (global_var_table (build_global_var_table super_parsed_list))
			   (asm_instructions_list (build_asm_insts_list super_parsed_list constant_table))
			   (asm_instructions_string (build_asm_insts_string asm_instructions_list))
			   (asm_with_const_table (add_const_table constant_table asm_instructions_string))
			   (final_asm (add_prologue_epilgue asm_with_const_table)))
;			(string->file final_asm asm_target_file))))
super_parsed_list)))

;TODO - ONLY ONE S-EXP
(define build_asm_insts_list
	(lambda (super_parsed_list const_tab)
		(if (null? super_parsed_list)
			(list)
			(cons (add_r0_print (code-gen (car super_parsed_list) 0 const_tab))
				  (build_asm_insts_list (cdr super_parsed_list) const_tab)))))

;TODO - PROBABLY REMOVE
(define add_r0_print
	(lambda (asm_string)
		;(string-append asm_string "OUT(IMM(2), R0);\n ")
		asm_string))

(define build_asm_insts_string
	(lambda (insts_list)
		(if (null? insts_list)
			""
			(string-append (car insts_list) (build_asm_insts_string (cdr insts_list))))))

(define add_const_table 
	(lambda (constant_table asm_instructions_string)
		(string-append (build_asm_constant_table constant_table)
						asm_instructions_string)))

(define build_asm_constant_table
	(lambda (constant_table)
		(let* ((last_element (car (reverse constant_table)))
			   (address (car last_element))
			   (represent (caddr last_element))
		       (represent_length (length represent))
		       (malloc_need (+ address represent_length))
		       (malloc_need_str (number->string malloc_need)))
			(string-append 
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

/*TODO - should entered the constant_table*/

PUSH(FP);
MOV(FP, SP);

 #define SOB_VOID 1
 #define SOB_NIL 2
 #define SOB_FALSE 5
 #define SOB_TRUE 3

"
 asm_insts_string
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
			   (final_list (build_final_list sub_const_list_no_dups)))
		 	final_list)))

(define create_const_list
	(lambda (sp_sexpr)
		(cond ((or (null? sp_sexpr) (atom? sp_sexpr)) (list))
			  ((and (equal? (car sp_sexpr) 'const)
			  		(not (or 	(equal? sp_sexpr `(const ,(void)))
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


(define build_final_list
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
			  ; ((symbol? element)
			  ; 	`(,next_available ,element (T_SYMBOL ,element)))
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

;TODO
(define build_global_var_table
	(lambda (super_parsed_list)
		(list)))

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
