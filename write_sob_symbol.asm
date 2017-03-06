/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the character representation
 * of that object.

 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R4, FPARG(0));
  MOV(R5, INDD(R4, 1));

  MOV(R0, R5);
  MOV(R1, INDD(R0, 1));
  MOV(R2, R0);
  ADD(R2, IMM(2));

 L_SYM_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(L_SYM_EXIT);
  CMP(IND(R2), '\n');
  JUMP_EQ(L_SYM_NEWLINE);
  CMP(IND(R2), '\t');
  JUMP_EQ(L_SYM_TAB);
  CMP(IND(R2), '\f');
  JUMP_EQ(L_SYM_PAGE);
  CMP(IND(R2), '\r');
  JUMP_EQ(L_SYM_RETURN);
  CMP(IND(R2), '\\');
  JUMP_EQ(L_SYM_BACKSLASH);
  CMP(IND(R2), '\"');
  JUMP_EQ(L_SYM_DQUOTE);
  CMP(IND(R2), ' ');
  JUMP_LT(L_SYM_OCT_CHAR);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_DQUOTE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\"'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_BACKSLASH:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_RETURN:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('r'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_PAGE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('f'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_TAB:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);  
 L_SYM_NEWLINE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('n'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_SYM_LOOP_CONT);
 L_SYM_OCT_CHAR:
  MOV(R0, IND(R2));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  REM(R0, IMM(8));
  PUSH(R0);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
 L_SYM_LOOP_CONT:
  INCR(R2);
  DECR(R1);
  JUMP(L_SYM_LOOP); 
 L_SYM_EXIT:


  POP(FP);
  RETURN;
