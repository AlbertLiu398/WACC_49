.data 
      .word 22 
.L.str0: 
      .asciz "Should not print this." 
      .word 4 
.L..L._prints_0: 
      .asciz "%.*s" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      mov fp, sp 
      mov x8, #42 
      mov x0, x8 
      bl _exit 
      adrp x8, .L.str0 
      bl _prints 
      bl _println 
      mov x0, #0 
      ldp fp, lr, [sp], #16
      ret 
.align 4 
_prints: 
      stp lr, xzr, [sp, #-16]!
      mov x2, x0 
      LDRSW x1, [x0, #-4] 
      adr x0, .L._prints_1 
      bl printf 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp, #16]
      ret 
.align 4 
_println: 
      stp lr, xzr, [sp, #-16]!
      adr x0, .L._println_1 
      bl puts 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp, #16]
      ret 
