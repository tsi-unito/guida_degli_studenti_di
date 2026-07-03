.globl _start

.data 
	src: .string "source"
	dst: .string "----------------"

.text

_start:
	la a1, src
	la a2, dst
	jal ra, strcpy
	beq zero, zero exit
strcpy:
	# parte di salvataggio dei dati
	addi sp, sp, -8
	sd ra, 0(sp)
	
	addi sp, sp, -8
	sd s1, 0(sp)
	
	addi sp, sp, -8
	sd a2, 0(sp)
	
	addi sp, sp, -8
	sd a1, 0(sp)
	
	# fine parte salvataggio dati
	jal ra, strlen			# s1 = strlen(a1)
	add s1, zero, a0
	add a1, a2, zero
			
	jal ra, strlen			# t1 = strlen(a2)
	add t1, zero, a0
	ld a1, 0(sp)
	addi sp, sp, 8
	
	ld a2, 0(sp)
	addi sp, sp, 8
	
	li t2, 0			# i = 0 (t2)
	begin_while_1:
					# while (i < s1)
	bge t2, s1, end_while_1
		lbu t3, 0(a1)		# t3 = src[i]
		sb t3, 0(a2)		# dst[i] = src[i] (t3)
		
		addi t2, t2, 1		# i++
		addi a1, a1, 1
		addi a2, a2, 1
		beq zero, zero, begin_while_1
	end_while_1:
	
	begin_while_2:
	
	bge t2, t1, end_while_2
		sb zero, 0(a2)
		addi t2, t2, 1
		addi a2, a2, 1
		beq zero, zero, begin_while_2
	end_while_2:
		
	# ricarica finale dati
	ld s1, 0(sp)
	addi sp, sp, 8
	
	ld ra, 0(sp)
	addi sp, sp, 8
	
strlen:	# in a1 il param
	lbu t1, 0(a1)
	li a0, 0
	do_while:
	beq t1, zero, end_while
		addi a0, a0, 1
		addi a1, a1, 1
		lbu t1, 0(a1)
		beq zero, zero, do_while
	end_while:
	jr ra

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
