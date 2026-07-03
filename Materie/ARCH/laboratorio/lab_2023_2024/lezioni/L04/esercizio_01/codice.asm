.globl _start

.text

_start:

	# 37 DEC = 0001 0101
	
	li t0, 1 # carico l0    
	
	li t1, 8 # N: dimensione massima con cui iterare per 8 bit
	li t2, 1  # M: maschera inizializzata a 1
	li t3, 0  # I: contatore
	li t5, 0  # R: risultato
	
	do_loop:
	bge t3, t1, end_loop
		and t4, t0, t2 # applico la maschera in t4
		bgt t4, zero, greater_than_zero
			beq zero, zero end_if
		greater_than_zero: 
			addi t5, t5, 1 # aggiungo la somma
		end_if:
		addi t3, t3, 1 # aggiungo 1 alla somma
		slli t2, t2, 1 # faccio lo shift logico a sx
		beq x0, x0, do_loop # continua a ciclare
	end_loop:

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
