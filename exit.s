.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, x20, [sp, #-48]!
      stp x21, x22, [sp, #16]
      stp x23, xzr, [sp, #32]
      mov fp, sp 
      mov x8, #1 
      mov x19, x8 
      mov x8, #1 
      mov x20, x8 
      mov x8, #1 
      mov x21, x8 
      mov x8, #1 
      mov x22, x8 
      mov x8, #6 
      mov x23, x8 
      mov x0, #0 
      ldp x21, x22, [sp, #16]
      ldp x23, xzr, [sp], #32
      ldp x19, x20, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
