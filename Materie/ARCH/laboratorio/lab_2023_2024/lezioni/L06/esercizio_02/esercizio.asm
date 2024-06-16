.globl _start

.text

_start:
	li a0, 80	# a
	li a1, 30	# b

	li s1, -1 	# result
	
	jal ra, mcd_function
	add s1, zero, a0
	
	beq zero, zero, exit
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
