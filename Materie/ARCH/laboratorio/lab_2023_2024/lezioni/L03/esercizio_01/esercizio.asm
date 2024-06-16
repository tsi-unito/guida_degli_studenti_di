.globl _start

.text

_start:
	# x = 2; y=1;
	li t0, 2
	li t1, 1
	
	addi t0, t0, -2 			# x = x-2
	add t0, t0, t1				# x = x + y

	blt t0, t1, IF_TRUE			# if (x < y)
		addi t1,t1,1 				# ELSE y = y - 1
		beq x0, x0, END_IF		
	IF_TRUE:				# se x < y
		addi t0, t0, 1				# TRUE x = x + 1
	END_IF:
    


exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall

