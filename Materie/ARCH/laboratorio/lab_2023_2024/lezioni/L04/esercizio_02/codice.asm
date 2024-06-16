.globl _start

.text

_start:

    li a0, 10		# a
    li a1, 5		# b
    li a2, 0		# R
    
    
    li a3, 0					# i = 0
    first_loop:
    bge a3, a0, end_first_loop 			# !(i<a) ovvero i >= a
    	# siamo dentro il ciclo
    	li a4, 0				# j = 0
    	bge a4, a1, end_second_loop
    		add a2, a2, a2			# R = R*2
    		add a2, a2, a3			# R = R + i
    		add a2, a2, a4			# R = R + j
    		addi a4, a4, 1			# j++
    	end_second_loop:	# fine secondo for
    	addi a3, a3, 1
    	beq zero, zero, first_loop
    end_first_loop: # fine doppio ciclo annidato


exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
