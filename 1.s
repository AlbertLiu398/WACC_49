.data 
      .word 4 
.L._printb_str1: 
      .asciz "true" 
      .word 4 
.L._printb_str2: 
      .asciz "%.*s" 
      .word 5 
.L._printb_str0: 
      .asciz "false" 
      .word 0 
.L._println_str0: 
      .asciz "" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, x20, [sp, #-16]!
      mov fp, sp 
      mov x8, #97 
      mov x19, x8 
      mov x8, #122 
      mov x20, x8 
      mov x8, x19 
      mov x9, x8 
      mov x8, x20 
      cmp x9, x8 
      cset x8, EQ 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, x19 
      mov x10, x8 
      mov x8, x20 
      cmp x10, x8 
      cset x8, NE 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, x19 
      mov x11, x8 
      mov x8, x20 
      cmp x11, x8 
      cset x8, LT 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, x19 
      mov x12, x8 
      mov x8, x20 
      cmp x12, x8 
      cset x8, LE 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, x19 
      mov x13, x8 
      mov x8, x20 
      cmp x13, x8 
      cset x8, GT 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, x19 
      mov x14, x8 
      mov x8, x20 
      cmp x14, x8 
      cset x8, GE 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x0, #0 
      ldp x19, x20, [sp], #16
      ldp fp, lr, [sp], #16
      ret 
.align 4 
_printb: 
      stp lr, xzr, [sp, #-16]!
      cmp x0, #0 
      b.NE .L_printb0 
      adr x2, .L._printb_str0 
      b .L_printb1 
.L_printb0: 
      adr x2, .L._printb_str1 
.L_printb1: 
      ldrsw x1, [x2, #-4] 
      adr x0, .L._printb_str2 
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
