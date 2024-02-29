.data 
      .word 27 
.L._errOutOfMemory_str0: 
      .asciz "fatal error: out of memory
" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
.align 4 
.text 
.global main 
main: 
      stp fp, lr, [sp, #-16]!
      stp x19, xzr, [sp, #-16]!
      mov fp, sp 
      mov x0, #16 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #3 
      str x8, [x16, #-4]
      mov x8, #1 
      str x8, [x16]
      mov x8, #2 
      str x8, [x16, #4]
      mov x8, #3 
      str x8, [x16, #8]
      mov x8, x16 
      mov x19, x8 
      mov x0, #0 
      ldp x19, xzr, [sp], #16
      ldp fp, lr, [sp], #16
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