.globl _start

.data
    str: .string "my long string"
    char: .string "m"
.text

_start:
    la a1, str
    la a2, char
    lbu a3, 0(a2)

    jal ra, strchr_fun
    beq zero, zero, exit

strchr_fun:
    li a0, 0

    begin_while:
    lbu t1, 0(a1)

    beq t1, a3, end_while
        bne t1, zero, end_if
            li a0, 0
            beq zero, zero, end_while_not_found
        end_if:

        addi a1, a1, 1
        beq zero, zero, begin_while
    end_while:
    	add a0, zero, a1
    end_while_not_found:
        jr ra

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
