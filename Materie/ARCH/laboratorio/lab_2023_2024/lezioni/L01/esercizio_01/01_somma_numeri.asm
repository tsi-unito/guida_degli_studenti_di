.globl _start

.text

_start:

    # Scrivere un programma per caricare le costanti 41, 43, 47 nei registri x5,x6 e x7 e
    # calcolare la loro somma.
    # Il risultato va scritto nel registro x28
    #
    # Eseguire il programma nel simulatore RARS. Eseguire un'istruzione alla volta e verificare i valori
    # dei registri
    
    # 1. Assegno le costanti
    addi x5, x5, 41
    addi x6, x6, 43
    addi x7, x7, 47
    
    # 2. Calcolo la somma in x28
    add x28, x5, x6
    add x28, x28, x7

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
