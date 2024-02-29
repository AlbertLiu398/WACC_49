.data 
      .word 0 
.L._println_str0: 
      .asciz "" 
      .word 2 
.L.str0: 
      .asciz "Hi" 
      .word 29 
.L.str9: 
      .asciz "They are not the same string." 
      .word 5 
.L.str1: 
      .asciz "Hello" 
      .word 16 
.L.str6: 
      .asciz "Now make s1 = s2" 
      .word 25 
.L.str10: 
      .asciz "They are the same string." 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 6 
.L.str7: 
      .asciz "s1 is " 
      .word 6 
.L.str8: 
      .asciz "s2 is " 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, x20, [sp, #-16]!
      mov fp, sp 
      adrp x8, .L.str0 
      add x8, x8, :lo12:.L.str0 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x19, x8 
      adrp x8, .L.str1 
      add x8, x8, :lo12:.L.str1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x20, x8 
      adrp x8, .L.str2 
      add x8, x8, :lo12:.L.str2 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, x19 
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
      mov x8, x20 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, x19 
      mov x9, x8 
      mov x8, x20 
      cmp x9, x8 
      cset x8, EQ 
      b.EQ .if_then_0 
      adrp x8, .L.str4 
      add x8, x8, :lo12:.L.str4 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      b .if_end_0 
.if_then_0: 
      adrp x8, .L.str5 
      add x8, x8, :lo12:.L.str5 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
.if_end_0: 
      adrp x8, .L.str6 
      add x8, x8, :lo12:.L.str6 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, x19 
      mov x8, x20 
      mov x19, x8 
      adrp x8, .L.str7 
      add x8, x8, :lo12:.L.str7 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, x19 
      mov x0, x8 
      bl _prints 
      bl _println 
      adrp x8, .L.str8 
      add x8, x8, :lo12:.L.str8 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, x20 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, x19 
      mov x10, x8 
      mov x8, x20 
      cmp x10, x8 
      cset x8, EQ 
      b.EQ .if_then_1 
      adrp x8, .L.str9 
      add x8, x8, :lo12:.L.str9 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      b .if_end_1 
.if_then_1: 
      adrp x8, .L.str10 
      add x8, x8, :lo12:.L.str10 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
.if_end_1: 
      mov x0, #0 
      ldp x19, x20, [sp], #16
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
