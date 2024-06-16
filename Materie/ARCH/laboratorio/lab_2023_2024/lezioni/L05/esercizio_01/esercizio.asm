.globl _start

.data 
	w1: .word 1
	w2: .word 9
	w3: .word 5
	w4: .word 13
	res: .word 0
.text

_start:
	# carico i valori
	la a1, w1	# carico l'indirizzo di w1 in a1
	lw t1, 0(a1)	# carico il valore dell'indirizzo in a1 dentro t1
	
	la a2, w2	# stessa cosa
	lw t2, 0(a2)
	
	la a3, w3
	lw t3, 0(a3)
	
	la a4, w4
	lw t4, 0(a4)
	
	
	
	add t5, t1, t2
	add t5, t5, t3
	add t5, t5, t4
	
	# ho la somma dei valori
	# devo dividere i numeri
	
	add s0, t5, zero
	li s1, 0	# il contatore di quante volte ci sta nella sottrazione (risultato)
	li s2, 4	# il numero di volte con cui vogliamo dividere
	
	# faccio un loop per simulare 
	# la divisione intera come sequenza di sottr.
	loop:
	blt s0, zero, end_loop		# vado avanti fin quando rimangono elementi
		# then
		sub s0, s0, s2 		# tolgo al totale il valore
		
		blt s0, zero, else_count
			addi s1, s1, 1		# aumento il contatore
		else_count:
		beq zero, zero, loop	# continuo il loop
	
	end_loop:
	
	la s4, res
	sw s1, 0(s4)
	
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
