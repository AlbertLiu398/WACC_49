.data 
      .word 2 
.L._printc_str0: 
      .asciz "%c" 
      .word 2 
.L._printi_str0: 
      .asciz "%d" 
      .word 44 
.L._errNull_str0: 
      .asciz "fatal error: null pair dereferenced or freed" 
      .word 26 
.L._errOutOfMemory_str0: 
      .asciz "fatal error: out of memory" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 0 
.L._println_str0: 
      .asciz "" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, x20, [sp, #-32]!
      stp x21, xzr, [sp, #16]
      mov fp, sp 
      mov x0, #5 
      bl _malloc 
      mov x16, x0 
      mov x8, #10 
      str x8, [x16]
      mov x8, #97 
      str x8, [x16, #8]
      mov x8, x16 
      mov x19, x8 
      cbz x19, _errNull 
      mov x8, #98 
      str x8, [x19, #8]
      cbz x19, _errNull 
      ldr x8, [x19]
      mov x20, x8 
      cbz x19, _errNull 
      ldr x8, [x19, #8]
      mov x21, x8 
      mov x8, x21 
      mov x0, x8 
      bl _printc 
      bl _println 
      mov x8, x20 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x0, #0 
      ldp x21, xzr, [sp, #16]
      ldp x19, x20, [sp], #32
      ldp fp, lr, [sp], #16
      ret 
.align 4 
_printc: 
      stp lr, xzr, [sp, #-16]!
      mov x1, x0 
      adr x0, .L._printc_str0 
      bl printf 
      mov x0, #0 
      bl fflush 
      ldp lr, xzr, [sp], #16
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
_malloc: 
      stp lr, xzr, [sp, #-16]!
      bl malloc 
      cbz x0, _errOutOfMemory 
      ldp lr, xzr, [sp], #16
      ret 
.align 4 
_errOutOfMemory: 
      adr x0, .L._errOutOfMemory_str0 
      bl _prints 
      mov x0, #-1 
      bl _exit 
.align 4 
_errNull: 
      adr x0, .L._errNull_str0 
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
