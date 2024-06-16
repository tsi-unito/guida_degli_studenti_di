.globl _start

.text

_start:

	li t0, 1
	li t1, 2
	li t2, 3
	
	li t3, 0
	
	blt t0, t1, else_t0_greater		# if (t0 >= t1)
		add t3, t3, t0			# 	t3 = t0
		beq x0, x0, end_if_t0_greater
	else_t0_greater:			# else
		add t3, t3, t1			#	t3 = t1
	end_if_t0_greater:
	
	# in t3 c'e' il maggiore tra t0 e t1. Stessa cosa tra t3 e t2
	
	bge t3, t2, end_if_t3_greater		# if (t3 < t2)
		li t3, 0			# 	t3 = t2;
		add t3, t3, t2
	end_if_t3_greater:

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
