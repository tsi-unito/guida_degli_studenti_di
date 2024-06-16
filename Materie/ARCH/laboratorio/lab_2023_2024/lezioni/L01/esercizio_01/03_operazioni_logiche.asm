.globl _start

.text

_start:

    # Si supponga che i registri seguenti contengano i valori:
    # x5 = 0x00000000AAAAAAAA, x6 = 0x1234567812345678
    
    li x5, 0x00000000AAAAAAAA
    li x6, 0x1234567812345678
    
    # (1): Determinare il contenuto di x7 dopo l’esecuzione delle seguenti istruzioni:
    slli x7, x5, 4
    or x7, x7, x6
    
    # Supponendo che i registri x5 e x6 contengano i valori riportati sopra, determinare
    # il contenuto di x7 dopo l’esecuzione di:
    slli x7, x6, 4
    
    # Supponendo che i registri x5 e x6 contengano i valori riportati sopra, determinare
    # il contenuto di x7 dopo l’esecuzione di:

    srli x7, x5, 3
    andi x7, x7, 0xFF

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
