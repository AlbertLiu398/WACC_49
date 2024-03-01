.data 
      .word 2 
.L._printi_str0: 
      .asciz "%d" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 80 
.L._errOverflow_str0: 
      .asciz "OverflowError: the result is too small/large to store in a 4-byte signed-integer" 
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
      mov x8, #-2000000000 
      mov x19, x8 
      mov x8, x19 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x8, x19 
      mov x8, x19 
      mov w9, w8 
      mov x10, #30517 
      movk x10, #37888, lsl #16 
      mov x8, x10 
      subs w8, w9, w8 
      b.VS _errOverflow 
      sxtw x8, w8
      mov x19, x8 
      mov x8, x19 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x0, #0 
      ldp x19, xzr, [sp], #16
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
.align 4 
_errOverflow: 
      adr x0, .L._errOverflow_str0 
      bl _prints 
      mov x0, #-1 
      bl _exit 
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
