.globl _start

.data
	v: .word 1, 2, 3, 4
	dim: .word 4
.text

_start:
    la a0, v
    la a1, dim
    lw a1, 0(a1)
    jal ra, somma_ric
    
    beq zero, zero, exit
    
# Somma ricorsivamente gli elementi di un array
# param a0 - L'indirizzo del 1 elemento dell'array corrente
# param a1 - La dimensione dell'array corrente 
somma_ric:
	addi sp, sp, -16
	sd ra, 0(sp)
	sd s1, 8(sp)
	
	lw s1, 0(a0)
	addi a0, a0, 4
	addi a1, a1, -1
	
	bgt a1, zero, skip_recurse
		jal ra, somma_ric
	skip_recurse:
	
	
	
	addi s1, s1, 
	addi a0
	
	jr ra
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
