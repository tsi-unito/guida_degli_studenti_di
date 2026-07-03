#include <stdio.h>
#include <stdlib.h>

// Scrivere un programma che:
// 	- legge da input una sequenza di numeri interi terminata dal numero 0
// 	  che si assume non appartenga alla sequenza
// 	- Quando l'utente inserisce 0, il programma interrompe la lettura della
// 	  sequenza e stampa la media di tutti gli interi letti
// 	- Provare il programma per le sequenze
// 		- 4 8 10 2 0
// 		- 3 5 0
// 		- 0
// 	suggerimento: usare accumulatore che somma i numeri letti 
// 	e un contatore dei numeri per calcolare media

int main(){
	int numero;
	int somma = 0;
	int contatore = 0;
	double media = 0;

	do {
		printf("inserisci un numero nella sequenza: ");
		scanf("%d",&numero);

		if (numero != 0){
			somma = somma + numero;
			contatore++;
		}
	} while(numero != 0);

	if (media != 0){
		media = (double) somma / (double) contatore;
		printf("la media di tutti i numeri letti e': %f", media);
	}
	

	return 0;
}
