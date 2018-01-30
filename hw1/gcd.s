	.file	"gcd.c"
	.text
	.globl	gcdI
	.type	gcdI, @function
gcdI:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	jmp	.L2
.L4:
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jle	.L3
	movl	-8(%rbp), %eax
	subl	%eax, -4(%rbp)
	jmp	.L2
.L3:
	movl	-4(%rbp), %eax
	subl	%eax, -8(%rbp)
.L2:
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jne	.L4
	movl	-4(%rbp), %eax   # return result by storing in 
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret                      # exit from the function
	.cfi_endproc
.LFE0:
	.size	gcdI, .-gcdI
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.5) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
