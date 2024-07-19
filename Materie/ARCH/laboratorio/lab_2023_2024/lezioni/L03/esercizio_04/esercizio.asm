.globl _start

.text

_start:

	li t1, 10			# N = 3 // i primi 3 quadrati perfetti
	li t2, 0			# i = 0 
	li t3, 0			# sum = 0
	li t4, 0			# current_quad = 0
	
	loop:
	bge t2, t1, endloop		# while (i < N)
		li t4, 0
		mul t4, t2, t2		#	current_quad = i * i
		add t3, t3, t4		#	sum = sum + current_quad
		addi t2, t2, 1
		beq zero, zero, loop
	endloop:
	
	

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
