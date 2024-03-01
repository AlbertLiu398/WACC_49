.data 
      .word 25 
.L.str0: 
      .asciz "Please input an integer: " 
      .word 2 
.L._printi_str0: 
      .asciz "%d" 
      .word 34 
.L.str3: 
      .asciz "(enter Y for 'yes' and N for 'no')" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 39 
.L.str2: 
      .asciz "Do you want to continue entering input?" 
      .word 12 
.L.str1: 
      .asciz "echo input: " 
      .word 3 
.L.str4: 
      .asciz "r%c" 
      .word 3 
.L.str5: 
      .asciz "r%d" 
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
      mov x8, #89 
      mov x19, x8 
      mov x8, #0 
      mov x20, x8 
      b .w_condition_0 
.w_body_0: 
      adrp x8, .L.str0 
      add x8, x8, :lo12:.L.str0 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, x20 
      mov x0, x8 
      bl _readi 
      mov x16, x0 
      mov x8, x16 
      mov x20, x8 
      adrp x8, .L.str1 
      add x8, x8, :lo12:.L.str1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, x20 
      mov x0, x8 
      bl _printi 
      bl _println 
      adrp x8, .L.str2 
      add x8, x8, :lo12:.L.str2 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      adrp x8, .L.str3 
      add x8, x8, :lo12:.L.str3 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, x19 
      mov x0, x8 
      bl _readc 
      mov x16, x0 
      mov x8, x16 
      mov x19, x8 
.w_condition_0: 
      mov x8, x19 
      mov x9, x8 
      mov x8, #78 
      cmp x9, x8 
      cset x8, NE 
      b.NE .w_body_0 
      mov x0, #0 
      ldp x19, x20, [sp], #16
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
_readc: 
      stp x0, lr, [sp, #-16]!
      mov x1, sp 
      adr x0, .L._readc_str0 
      bl scanf 
      ldp x0, lr, [sp], #16
      ret 
_readi: 
      stp x0, lr, [sp, #-16]!
      mov x1, sp 
      adr x0, .L._readi_str0 
      bl scanf 
      ldp x0, lr, [sp], #16
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
