.data
.align 4
.text
.global main
main:
    // push {fp, lr}
    stp fp, lr, [sp, #-16]!
    // push {x19, x20, x21}
    stp x19, x20, [sp, #-32]!
    stp x21, xzr, [sp, #16]
    mov fp, sp
    // Stack pointer unchanged, no stack allocated variables
    mov x8, #0
    mov x19, x8
    mov x8, #0
    mov x20, x8
    mov x8, #1
    mov x21, x8
    // Stack pointer unchanged, no stack allocated arguments
    cmp x19, #1
    b.ne .L0
    cmp x20, #1
.L0:
    cset x8, eq
    // push {x8}
    stp x8, xzr, [sp, #-16]!
    // pop {x8}
    ldp x8, xzr, [sp], #16
    cmp x8, #1
    b.eq .L1
    cmp x21, #1
.L1:
    cset x8, eq
    // push {x8}
    stp x8, xzr, [sp, #-16]!
    // pop {x8}
    ldp x8, xzr, [sp], #16
    mov x8, x8
    mov x0, x8
    // statement primitives do not return results (but will clobber r0/rax)
    bl _printb
    bl _println
    // Stack pointer unchanged, no stack allocated arguments
    cmp x19, #1
    b.ne .L2
    cmp x20, #1
    b.eq .L3
    cmp x21, #1
.L3:
    cset x8, eq
    // push {x8}
    stp x8, xzr, [sp, #-16]!
    // pop {x8}
    ldp x8, xzr, [sp], #16
    cmp x8, #1
.L2:
    cset x8, eq
    // push {x8}
    stp x8, xzr, [sp, #-16]!
    // pop {x8}
    ldp x8, xzr, [sp], #16
    mov x8, x8
    mov x0, x8
    // statement primitives do not return results (but will clobber r0/rax)
    bl _printb
    bl _println
    // Stack pointer unchanged, no stack allocated variables
    mov x0, #0
    // pop {x19, x20, x21}
    ldp x21, xzr, [sp, #16]
    ldp x19, x20, [sp], #32
    // pop {fp, lr}
    ldp fp, lr, [sp], #16
    ret

// length of .L._printb_str0
.word 5
.L._printb_str0:
    .asciz "false"
// length of .L._printb_str1
.word 4
.L._printb_str1:
    .asciz "true"
// length of .L._printb_str2
.word 4
.L._printb_str2:
    .asciz "%.*s"
.align 4
_printb:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    cmp w0, #0
    b.ne .L_printb0
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
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// length of .L._println_str0
.word 0
.L._println_str0:
    .asciz ""
.align 4
_println:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    adr x0, .L._println_str0
    bl puts
    mov x0, #0
    bl fflush
    // pop {lr}
    ldp lr, xzr, [sp], #16
    retx0, #0 
      b.NE .L_printb0 
      adr x2, .L._printb_str0 
.L_printb1: 
      ldrsw x1, [x2, #-4] 
      adr x2, .L._printb_str1 
      b .L_printb1 
.L_printb0: 
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
