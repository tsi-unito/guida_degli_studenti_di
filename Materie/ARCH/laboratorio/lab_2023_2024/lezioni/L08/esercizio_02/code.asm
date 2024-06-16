.globl _start

.data
    str: .string "my long stringm"
    char: .string "m"
.text

_start:
    la a1, str
    la a2, char
    lbu a3, 0(a2)

    jal ra, strchr_fun_last
    beq zero, zero, exit

strchr_fun_last:
    li a0, 0
    
    begin_while:
    lbu t1, 0(a1)		# char a = str[i]
    beq t1, zero, end_while
    	bne t1, a3, else_equals
    	    add a0, zero, a1
    	else_equals:
    	
    	addi a1, a1, 1
    	beq zero, zero, begin_while	

    end_while:
    	jr ra

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
