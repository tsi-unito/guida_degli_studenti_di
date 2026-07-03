#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define MAX_TENTATIVI 7

int main(void)
{
	// Genera un numero pseudo-casuale tra 0 (incluso) e 100 (escluso)
	srand(time(0));
	int numero = rand() % 100;
	int t;
	bool trovato = false;

	// VARIANTE 1:
	// Chiedi all'utente di indovinare il numero.
	// L'utente inserisce un intero t come tentativo
	// - Se t è il numero da indovinare, stampa un messaggio e termina
	// - Se t è maggiore del numero da indovinare, stampa un messaggio
	//   appropriato e procedi con un nuovo tentativo
	// - Procedi similmente se t è minore, con un diverso messaggio.

	printf("Inserisci il numero: ");
	while(scanf("%d",&t) && !trovato){
		if (t == numero){
			printf("\nNumero trovato!");
			trovato = true;
		} else if(t > numero){
			printf("\nIl numero scelto e' maggiore del numero da indovinare");
		} else {
			printf("\nIl numero scelto e' minore del numero da indovinare");
		}
	} 

	// VARIANTE 2:
	// memorizza in una variabile num_tentativi il numero
	// di tentativi che l'utente ha fatto. Quando il numero da indovinare
	// viene trovato, stampa a video il numero di tentativi effettuati.

	// VARIANTE 3:
	// Se l'utente supera MAX_TENTATIVI, termina il ciclo e stampa 
	// un messaggio di sconfitta, svelando anche il numero da indovinare.

}
