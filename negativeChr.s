.data 
      .word 82 
.L._errOverflow_str0: 
      .asciz "OverflowError: the result is too small/large to store in a 4-byte signed-integer.
" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, xzr, [sp, #-16]!
      mov fp, sp 
      mov x8, #-1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x19, x8 
      mov x0, #0 
      ldp x19, xzr, [sp, #16]
      ldp fp, lr, [sp], #16
      ret 
.align 4 
_errOverflow: 
      adr x0, .L._errOverflow_str0 
      bl _prints 
      mov x0, #-1 
      bl _exit 
