.globl _start

.text

_start:
	li t0, 8			# N = 8
	li t1, 1			# R = 1
	li t2, 0			# A = 0
	li t3, 1			# B = 1
	
	loop:
	ble t0, x0, endloop		# while ( N > 0 )
		add t1, t2, t3		#	R = R + B
		li t2, 0
		add t2, x0, t3		#	A = B
		li t3, 0
		add t3, x0, t1		#	B = R
		addi t0, t0, -1		#	N = N - 1
		beq x0, x0, loop
		
	endloop:
	
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
