.globl _start

.data
	src: .string "pippo"

.text

_start:
	la a0, src
	jal ra, str_len

	beq zero, zero, exit

str_len:
	lbu a1, 0(a0)
	li a3, 0
	
	begin_loop:
	
	bne a1, zero, do_loop
		beq zero, zero, end_loop
	do_loop:
		addi a0, a0, 1
		lbu a1, 0(a0)
		addi a3, a3, 1
	
		beq zero, zero, begin_loop
	end_loop:
	
	add a0, zero, a3 
	jr ra

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
