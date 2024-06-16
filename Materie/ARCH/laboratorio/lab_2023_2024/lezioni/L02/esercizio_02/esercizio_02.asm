.globl _start

.text

_start:

	# CONSEGNA:
	# Si scriva un programma in che caricihi due interi pari su
	# x5, x6 e calcoli il valore della loro media aritmetica.
	# Il valore calcolato va scritto in x7.
	# 
	# Usare soltanto il set delle istruzioni di RV32I
	# (usare uno shift per fare la divisione per 2)

	# (1) Assegno i valori alle variabili
	li x5, 0x2
	li x6, 0x4

	# (2) Calcolo:
	add x7, x5, x6 # Faccio prima la somma
	srai x7, x7, 1 # Sto dividendo per 2


exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall

