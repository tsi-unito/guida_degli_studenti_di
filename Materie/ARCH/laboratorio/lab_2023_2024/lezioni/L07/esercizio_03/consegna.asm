.globl _start

.data
	str1: .string "first"
	str2: .string "second"
.text

_start:
	la a1, str1
	la a2, str2
	
	jal ra, strcmp_fun
	beq zero, zero, exit

strcmp_fun:
	lb t1, 0(a1)
	lb t2, 0(a2)
	and t4, t1, t2
	begin_while:
	
	beq t4, t1, do_while
		beq zero, zero, exit_while
	do_while:
		addi a1, a1, 1
		addi a2, a2, 1
		
		lbu t1, 0(a1)
		
		lbu t2, 0(a2)
		and t4, t1, t2
		
		beq t1, zero, exit_while
		beq t2, zero, exit_while
	
		beq zero, zero, begin_while
	exit_while:
		beq t4, t1, if_equals
			addi a0, zero, 1
			beq zero, zero, end_if
			
			if_equals:
			addi a0, zero, 0
		end_if:
		jr ra
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
