.globl _start

.data
	num_1: 		.word 1
	num_2: 		.word 3
	num_3: 		.word 2
	num_max: 	.word 0
.text

_start:
	la a1, num_1
	lw t1, 0(a1)
	
	la a2, num_2
	lw t2, 0(a2)
	
	la a3, num_3
	lw t3, 0(a3)
	
	li t4, 0	# il max
	
	blt t1, t2, num_1_less_else		# if t1 > t2
		add t4, zero, t1		#	t4 = t1
		beq zero, zero, num_1_end_if
	num_1_less_else:
		add t4, zero, t2		# else t4 = t2
	num_1_end_if:
	
	bgt t4, t3, num_2_continue_else		# if t4 > t3
		add t4, zero, t3		#	 t4 = t3
	num_2_continue_else:			# 
		
	la a4, num_max				# salvo il valore
	sw t4, 0(a4)

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
