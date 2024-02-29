.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, xzr, [sp, #-16]!
      mov fp, sp 
      mov x8, #2147483646 
      mov x19, x8 
      mov x8, x19 
      mov x8, x19 
      add x8, x8, #1 
      mov x19, x8 
      mov x0, #0 
      ldp x19, xzr, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
