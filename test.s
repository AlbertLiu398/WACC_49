.data 
      .word 43 
.L._errDivZero_str0: 
    .asciz "DivideByZeroError: divide or modulo by zero" 
      .word 4 
.L._prints_str0: 
    .asciz "%.*s" 
      .word 80 
.L._errOverflow_str0: 
    .asciz "OverflowError: the result is too small/large to store in a 4-byte signed-integer" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16] !
      stp x19, xzr, [sp, #-16] !
      mov fp, sp 
      mov x8, #18 
      mov x9, x8 
      mov x8, #0 
      cbz x8, _errDivZero 
      sdiv x10, x9, x8 
      mul x10, x10, x8 
      sub x8, x9, x10 
      b.VS _errOverflow 
      sxtw x8, w8 
      mov x19, x8 
      mov x0, #0 
      ldp x19, xzr, [sp], #16 
      ldp fp, lr, [sp], #16 
      ret 
.align 4 
_errDivZero: 
      adr x0, .L._errDivZero_str0 
      bl _prints 
      mov x0, #-1 
      bl _exit 
.align 4 
_errOverflow: 
      adr x0, .L._errOverflow_str0 
      bl _prints 
      mov x0, #-1 
      bl _exit 
.align 4 
_prints: 
      stp lr, xzr, [sp, #-16] !
      mov x2, x0 
      ldrsw x1, [x0, #-4] 
      adr x0, .L._prints_str0 
      bl printf 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp], #16 
      ret 
