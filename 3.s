.data 
      .word 4 
.L._printb_str1: 
      .asciz "true" 
      .word 3 
.L.str10: 
      .asciz "), " 
      .word 4 
.L._printb_str2: 
      .asciz "%.*s" 
      .word 2 
.L._printp_str0: 
      .asciz "%p" 
      .word 5 
.L._printb_str0: 
      .asciz "false" 
      .word 4 
.L._prints_str0: 
      .asciz "%.*s" 
      .word 2 
.L.str3: 
      .asciz "of" 
      .word 5 
.L.str2: 
      .asciz "array" 
      .word 2 
.L.str0: 
      .asciz ", " 
      .word 7 
.L.str4: 
      .asciz "strings" 
      .word 38 
.L._errOutOfBound_str0: 
      .asciz "fatal error: array index out of bounds" 
      .word 0 
.L._println_str0: 
      .asciz "" 
      .word 2 
.L._printc_str0: 
      .asciz "%c" 
      .word 16 
.L.str1: 
      .asciz "this is a string" 
      .word 2 
.L._printi_str0: 
      .asciz "%d" 
      .word 44 
.L._errNull_str0: 
      .asciz "fatal error: null pair dereferenced or freed" 
      .word 4 
.L.str11: 
      .asciz " = (" 
      .word 4 
.L.str9: 
      .asciz " = (" 
      .word 26 
.L._errOutOfMemory_str0: 
      .asciz "fatal error: out of memory" 
      .word 5 
.L.str6: 
      .asciz "] , [" 
      .word 3 
.L.str7: 
      .asciz "] )" 
      .word 3 
.L.str12: 
      .asciz ") ]" 
      .word 2 
.L.str8: 
      .asciz "[ " 
      .word 3 
.L.str5: 
      .asciz "( [" 
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
      mov x8, #5 
      mov x8, #120 
      mov x8, #1 
      adrp x8, .L.str1 
      add x8, x8, :lo12:.L.str1 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
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
      mov x0, #7 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #3 
      str x8, [x16, #-4]
      mov x8, #120 
      str x8, [x16]
      mov x8, #121 
      str x8, [x16, #1]
      mov x8, #122 
      str x8, [x16, #2]
      mov x8, x16 
      mov x0, #7 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #3 
      str x8, [x16, #-4]
      mov x8, #1 
      str x8, [x16]
      mov x8, #0 
      str x8, [x16, #1]
      mov x8, #1 
      str x8, [x16, #2]
      mov x8, x16 
      mov x0, #28 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #3 
      str x8, [x16, #-4]
      adrp x8, .L.str2 
      add x8, x8, :lo12:.L.str2 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      str x8, [x16]
      adrp x8, .L.str3 
      add x8, x8, :lo12:.L.str3 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      str x8, [x16, #8]
      adrp x8, .L.str4 
      add x8, x8, :lo12:.L.str4 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      str x8, [x16, #16]
      mov x8, x16 
      mov x0, #8 
      bl _malloc 
      mov x16, x0 
      mov x8, #1 
      str x8, [x16]
      mov x8, #2 
      str x8, [x16, #8]
      mov x8, x16 
      mov x0, #2 
      bl _malloc 
      mov x16, x0 
      mov x8, #97 
      str x8, [x16]
      mov x8, #1 
      str x8, [x16, #8]
      mov x8, x16 
      mov x0, #2 
      bl _malloc 
      mov x16, x0 
      mov x8, #98 
      str x8, [x16]
      mov x8, #0 
      str x8, [x16, #8]
      mov x8, x16 
      mov x0, #4 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #2 
      str x8, [x16, #-4]
      mov x8, xzr 
      str x8, [x16]
      mov x8, xzr 
      str x8, [x16]
      mov x8, x16 
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
      mov x0, #7 
      bl _malloc 
      mov x16, x0 
      add x16, x16, #4 
      mov x8, #3 
      str x8, [x16, #-4]
      mov x8, #97 
      str x8, [x16]
      mov x8, #98 
      str x8, [x16, #1]
      mov x8, #99 
      str x8, [x16, #2]
      mov x8, x16 
      mov x0, #0 
      bl _malloc 
      mov x16, x0 
      mov x8, xzr 
      str x8, [x16]
      mov x8, xzr 
      str x8, [x16, #8]
      mov x8, x16 
      cbz xzr, _errNull 
      ldr x8, [xzr]
      cbz xzr, _errNull 
      ldr x8, [xzr, #8]
      adrp x8, .L.str5 
      add x8, x8, :lo12:.L.str5 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printi 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printi 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #2 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printi 
      adrp x8, .L.str6 
      add x8, x8, :lo12:.L.str6 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printc 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printc 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #2 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printc 
      adrp x8, .L.str7 
      add x8, x8, :lo12:.L.str7 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      cbz xzr, _errNull 
      ldr x8, [xzr]
      cbz xzr, _errNull 
      ldr x8, [xzr, #8]
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      cbz xzr, _errNull 
      ldr x8, [xzr]
      cbz xzr, _errNull 
      ldr x8, [xzr, #8]
      adrp x8, .L.str8 
      add x8, x8, :lo12:.L.str8 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printp 
      adrp x8, .L.str9 
      add x8, x8, :lo12:.L.str9 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printc 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printb 
      adrp x8, .L.str10 
      add x8, x8, :lo12:.L.str10 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printp 
      adrp x8, .L.str11 
      add x8, x8, :lo12:.L.str11 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printc 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printb 
      adrp x8, .L.str12 
      add x8, x8, :lo12:.L.str12 
      stp x8, xzr, [sp, #-16]!
      ldp x8, xzr, [sp], #16
      mov x8, x8 
      mov x0, x8 
      bl _prints 
      bl _println 
      cbz xzr, _errNull 
      ldr x8, [xzr]
      cbz xzr, _errNull 
      ldr x8, [xzr, #8]
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, #2 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printb 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printb 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, #2 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, #0 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, #1 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, #2 
      mov w17, w8 
      mov x7, xzr 
      bl _arrLoad4 
      mov w8, w7 
      mov x8, x8 
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x8, xzr 
      mov x0, x8 
      bl _prints 
      bl _println 
      mov x8, xzr 
      mov x0, x8 
      bl _printb 
      bl _println 
      mov x8, xzr 
      mov x0, x8 
      bl _printc 
      bl _println 
      mov x8, xzr 
      mov x0, x8 
      bl _printi 
      bl _println 
      mov x0, #0 
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
_printp: 
      stp lr, xzr, [sp, #-16]!
      mov x1, x0 
      adr x0, .L._printp_str0 
      bl printf 
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
_errOutOfBounds: 
      adr x0, .L._errOutOfBound_str0 
      bl printf 
      mov x0, #0 
      bl fflush 
      mov x0, #-1 
      bl _exit 
_arrLoad4: 
      stp lr, xzr, [sp, #-16]!
      cmp x17, #0 
      csel x1, x17, x1, LT 
      b.LT _errOutOfBounds 
      ldrsw lr, [x7, #-4] 
      cmp x17, lr 
      csel x1, x17, x1, GE 
      b.GE _errOutOfBounds 
      ldrsw x7, [x7, x17, lsl #2] 
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
