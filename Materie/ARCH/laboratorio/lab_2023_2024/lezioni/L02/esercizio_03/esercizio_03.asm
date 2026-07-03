.globl _start

.text

_start:

    	# CONSEGNA:
    	# Scrivere le sequenze di istruzioni in RISC-V corrispondente ai seguenti
    	# frammenti di codice:
    	#
    	# (Frammento 1):
    	# x = x - y
    	# if (x < 0)
    	#	x = 0
    	# y = y -1
	#
	# (Frammento 2):
	# x = (x-2)+y
	# if(x<y)
	# 	x = x + 1
	# else
	# 	y = y + 1
	#
	# Supporre che x e y siano in t0, t1

	# Frammento 1:
	sub t0, t0, t1

	bge t0, x0, IF_ELSE
		add t0, t0, x0
	IF_ELSE:
	sub t1, t1, 1
	
	# Frammento 2:
	add t0, t0, t1
	addi t2, t2, 2
	sub t0, t0, t2
	
	blt t0, t1, IF_2_THEN
		addi t1, t1, 1
		beq x0, x0, IF_2_ENDIF
	:IF_2_THEN
		addi t0, t0, 1
		beq x0, x0, IF_2_ENDIF
	
	:IF_2_ENDIF



exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall

