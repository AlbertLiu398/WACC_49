.data
// length of .L.str0
.word 2
.L.str0:
.asciz "Hi"
// length of .L.str1
.word 5
.L.str1:
.asciz "Hello"
// length of .L.str2
.word 6
.L.str2:
.asciz "s1 is "
// length of .L.str3
.word 6
.L.str3:
.asciz "s2 is "
// length of .L.str4
.word 29
.L.str4:
.asciz "They are not the same string."
// length of .L.str5
.word 25
.L.str5:
.asciz "They are the same string."
// length of .L.str6
.word 16
.L.str6:
.asciz "Now make s1 = s2"
.align 4
.text
.global main
main:
	// push {fp, lr}
	stp fp, lr, [sp, #-16]!
	// push {x19, x20}
	stp x19, x20, [sp, #-16]!
	mov fp, sp
	// Stack pointer unchanged, no stack allocated variables
	adrp x8, .L.str0
	add x8, x8, :lo12:.L.str0
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x19, x8
	adrp x8, .L.str1
	add x8, x8, :lo12:.L.str1
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x20, x8
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str2
	add x8, x8, :lo12:.L.str2
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	// Stack pointer unchanged, no stack allocated arguments
	mov x8, x19
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str3
	add x8, x8, :lo12:.L.str3
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	// Stack pointer unchanged, no stack allocated arguments
	mov x8, x20
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	cmp x19, x20
	b.eq .L0
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str4
	add x8, x8, :lo12:.L.str4
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	b .L1
.L0:
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str5
	add x8, x8, :lo12:.L.str5
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
.L1:
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str6
	add x8, x8, :lo12:.L.str6
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	mov x8, x20
	mov x19, x8
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str2
	add x8, x8, :lo12:.L.str2
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	// Stack pointer unchanged, no stack allocated arguments
	mov x8, x19
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str3
	add x8, x8, :lo12:.L.str3
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	// Stack pointer unchanged, no stack allocated arguments
	mov x8, x20
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	cmp x19, x20
	b.eq .L2
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str4
	add x8, x8, :lo12:.L.str4
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	b .L3
.L2:
	// Stack pointer unchanged, no stack allocated arguments
	adrp x8, .L.str5
	add x8, x8, :lo12:.L.str5
	// push {x8}
	stp x8, xzr, [sp, #-16]!
	// pop {x8}
	ldp x8, xzr, [sp], #16
	mov x8, x8
	mov x0, x8
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
.L3:
	// Stack pointer unchanged, no stack allocated variables
	mov x0, #0
	// pop {x19, x20}
	ldp x19, x20, [sp], #16
	// pop {fp, lr}
	ldp fp, lr, [sp], #16
	ret

// length of .L._prints_str0
.word 4
.L._prints_str0:
.asciz "%.*s"
.align 4
_prints:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	mov x2, x0
	ldrsw x1, [x0, #-4]
	adr x0, .L._prints_str0
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
	ret
