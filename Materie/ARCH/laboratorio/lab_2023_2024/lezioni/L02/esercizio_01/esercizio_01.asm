.globl _start

.text

_start:

	# CONSEGNA:
	# Determinare una sequenza di istruzioni risc-v che consenta di estrarre i bit da 
	# b11 a b16 dal registro x5 e li sostituisca ai bit da b26 a b31 del registro x6 senza
	# modificare gli altri bit del registro x5, x6.
	# 
	# CASO DI TEST: x5 = 0  e x6 = 0xffffffffffffffff

    	# (1) inizializzo x5
    	li x5, 0
    	li x6, 0xffffffffffffffff
    	
    	# (2) prendo i bit 11-26 usando una maschera di bit salvando in x7
    	# 	Sarebbero 0011 1111, che dovranno poi essere spostati
    	
    	li x7, 0x3f
    	slli x7, x7, 11
    	
    	and x7, x7, x5
   	
   	# Ora ho in x7 i bit richiesti da x5, devo andare a fare lo shift a sinistra e poi
   	# salvarli su x6
   	#
   	# Di quanto?
   	# Se ho da [11-16] e voglio da [26-31] devo spostare di 15 elementi 
   	slli x7, x7, 15
   	
   	# (2) Sovrascrivo: salvo usando x7 come maschera che fa il set dei dati
   	#or x6, x6, x7 => NO! Solo cosi non funziona per i casi
   	# 1 (x6) e 0(x7)
   	
   	# Bisogna quindi fare una AND, con una maschera che imposti a 0 solo i bit da 26-31
   	not x10, x7 # salvo una NOT
   	and x6, x6, x10
   	
   	or x6, x6, x7
   	

exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
