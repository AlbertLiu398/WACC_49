.data 
      .word 2 
.L._printi_str0: 
      .asciz "%d" 
      .word 0 
.L._println_str0: 
      .asciz "" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, xzr, [sp, #-16]!
      mov fp, sp 
      mov x8, #99 
      mov x0, x8 
      bl wacc_f 
      mov x16, x0 
      mov x8, x16 
      mov x19, x8 
      mov x8, x19 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x0, #0 
      ldp x19, xzr, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
wacc_f: 
      stp fp, lr, [sp, #-16]!
      mov fp, sp 
      mov x8, x0 
      mov x0, x8 
      ldp fp, lr, [sp], #16
      ret 
.align 4 
_printi: 
      stp lr, xzr, [sp, #-16]!
      mov x1, x0 
      adr x0, .L._printi_str0 
      bl printf 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp], #16
      ret 
.align 4 
_println: 
      stp lr, xzr, [sp, #-16]!
      adr x0, .L._println_str0 
      bl puts 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp], #16
      ret 
