.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      mov fp, sp 
      mov x0, #0 
      ldp fp, lr, [sp], #16
      ret 
inc: 
      stp fp, lr, [sp, #-16]!
      stp x19, xzr, [sp, #-16]!
      mov fp, [sp] 
      mov x8, #1 
      mov x0, x8 
      ldp x19, x20, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
