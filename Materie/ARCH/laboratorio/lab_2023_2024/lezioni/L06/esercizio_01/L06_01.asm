.globl _start

.data
	int_x: 		.word 8
	vet:		.byte 1, 3, 4, 5, 6, 7, 8, 9
	ris:		.word -1
.text

_start:
	la a1, int_x
	la a2, vet
	
	lw t1, 0(a1)
	lb t2, 0(a2)	# vet[0]
	
	li t3, 0	# contatore
	li t4, 0	# risultato
	li t6, 2
	
	begin_loop:
	blt t3, t1, do_loop
		beq zero, zero, exit_loop
	do_loop:
		
		rem t5, t2, t6
		
		bgt t5, zero, is_odd
			beq zero, zero is_odd_end_if
		is_odd:
			addi t4, t4, 1
		is_odd_end_if:	 
		
		addi a2, a2, 1
		lb t2, 0(a2)	# vet[i+1]
		addi t3, t3, 1	# i = i + 1
		beq zero, zero, begin_loop
	exit_loop:
	
	la a3, ris
	sw t4, 0(a3)
exit:
    	addi x17, x0, 10  # call number 10 = exit
    	ecall
