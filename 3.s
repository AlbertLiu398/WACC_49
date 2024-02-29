.data 
      .word 0 
.L._println_str0: 
      .asciz "" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 5 
.L._printb_str1: 
      .asciz "false" 
      .word 8 
.L.str0: 
      .asciz "True is " 
      .word 4 
.L._printb_str0: 
      .asciz "true" 
      .word 9 
.L.str1: 
      .asciz "False is " 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      mov fp, sp 
      adrp x8, .L.str0 
      add x8, x8, :lo12:.L.str0 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, #1 
      mov x0, x8 
      bl _printb 
      bl _println 
      adrp x8, .L.str1 
      add x8, x8, :lo12:.L.str1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, #0 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x0, #0 
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
.align 4 
_prints: 
      stp lr, xzr, [sp, #-16]!
      mov x2, x0 
      ldrsw x1, [x0, #-4] 
      adr x0, .L._prints_str0 
      bl printf 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp], #16
      ret 
