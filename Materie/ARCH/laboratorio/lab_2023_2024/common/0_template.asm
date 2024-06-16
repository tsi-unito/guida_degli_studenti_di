.globl _start

.text

_start:

    addi x20, x20, 4


exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
