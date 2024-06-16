.globl _start

.data
	vet: 	.word 1, 2, 3, 4, 5, 6
	result: .word 0
.text

_start:
	la a1, vet	# carico il valore in memoria
	lw s1, 0(a1)	# di v[0]
	 
	li s2, 6	# dimensione array
	li s3, 0	# contatore

	li s0, 0	# accumulatore di somme
	
	loop:
	
	bge s3, s2, end_loop
		# then
		add s0, s0, s1		# sommo valore
		addi a1, a1, 4		# aumento l'indirizzo per passare al valore successivo
		lw s1, 0(a1)		# ricarico dentro a1 il valore
		addi s3, s3, 1		# aumento contatore
		beq zero, zero, loop
	
	end_loop:
	
	la s5, result
	sw s0, 0(s5)
    
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
