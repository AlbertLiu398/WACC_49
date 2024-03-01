.data 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 7 
.L.str0: 
      .asciz "correct" 
      .word 9 
.L.str1: 
      .asciz "incorrect" 
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
      mov x8, #13 
      mov x19, x8 
      mov x8, x19 
      mov x9, x8 
      mov x8, #13 
      cmp x9, x8 
      cset x8, NE 
      b.NE .if_then_0 
      adrp x8, .L.str0 
      add x8, x8, :lo12:.L.str0 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      b .if_end_0 
.if_then_0: 
      adrp x8, .L.str1 
      add x8, x8, :lo12:.L.str1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
.if_end_0: 
      mov x0, #0 
      ldp x19, xzr, [sp], #16
      ldp fp, lr, [sp], #16
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
