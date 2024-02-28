.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16], #0!
      stp x19, x20, [sp, #-16], #0!
      mov fp, [sp] 
      mov x8, #1 
      stp x8, xzr, [sp, #-16], #0!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _exit 
      mov x0, #0 
      ldp x19, x20, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
