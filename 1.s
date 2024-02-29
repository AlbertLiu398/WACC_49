.data 
.align 4
.text
.global main
main:
	// push {fp, lr}
	stp fp, lr, [sp, #-16]!
	// push {x19}
	stp x19, xzr, [sp, #-16]!
	mov fp, sp
	// Stack pointer unchanged, no stack allocated variables
	// 3 element array
	mov w0, #16
	bl _malloc
	mov x16, x0
	// array pointers are shifted forwards by 4 bytes (to account for size)
	add x16, x16, #4
	mov x8, #3
	str w8, [x16, #-4]
	mov x8, #1
	str w8, [x16, #0]
	mov x8, #2
	str w8, [x16, #4]
	mov x8, #3
	str w8, [x16, #8]
	mov x8, x16
	mov x19, x8
	// Stack pointer unchanged, no stack allocated variables
	mov x0, #0
	// pop {x19}
	ldp x19, xzr, [sp], #16
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

_malloc:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	bl malloc
	cbz x0, _errOutOfMemory
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret

// length of .L._errOutOfMemory_str0
.word 27
.L._errOutOfMemory_str0:
	.asciz "fatal error: out of memory\n"
.align 4
_errOutOfMemory:
	adr x0, .L._errOutOfMemory_str0
	bl _prints
	mov w0, #-1
	bl exit
