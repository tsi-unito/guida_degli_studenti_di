.globl _start

.text

_start: 
	li a0, 12	# a
	li a1, 9	# b

	li s1, -1 	# result
	
	jal ra, mcm_function
	add s1, zero, a0
	
	beq zero, zero, exit
	
mcm_function:
	addi sp, sp, -8		# devo salvare il return address
	sd ra, 0(sp)
	
	addi sp, sp, -8		# devo salvare il return address
	sd s1, 0(sp)
	
	mul s1, a0, a1		# (a*b)
	jal ra, mcd_function
	
	div a0, s1, a0 		# (a*b) / MCD(a,b)
	
	ld s1, 0(sp)		# ri-carico lo stack pointier
	addi sp, sp, 8
	
	ld ra, 0(sp)		# ri-carico lo stack pointier
	addi sp, sp, 8
	

	jr ra

mcd_function:
	add t1, zero, a0
	add t2, zero, a1
	
	begin_while:
	bne t1, t2, do_while
		beq zero, zero, end_while
	do_while:
		bgt t1, t2, if_a_greater	# if (a > b)
			sub t2, t2, t1		#	b = b - a
			beq zero, zero, end_if
		if_a_greater:
			sub t1, t1, t2		#	a = a - b
		end_if:
		beq zero, zero, begin_while
	end_while:
		add a0, zero, t1
		jr ra
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
