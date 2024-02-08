#include <stdio.h>


// Scrivere un programma che legge un numero n e stampa
// un quadrato di n * n caratteri tra '*', '\' e ':'
// seguendo questo pattern:
//
// Esempio con n = 6:
//
// \ : : : : :
// * \ : : : :
// * * \ : : :
// * * * \ : :
// * * * * \ :
//
// Suggerimento:
// Usare 2 cicli annidati, uno esterno per le righe ed uno piu' interno
// per i caratteri sulla stessa riga

int main(){
	int n;
	int divisore = 0;

	printf("Inserisci un numero: ");
	scanf("%d",&n);


	for ( int r = 0; r < n; r ++){
		// singola riga
		// stampa degli '*'
		for ( int c = 0; c < divisore; c++){
			printf("* ");
		}
		printf("\\ ");

		// stampa degli ':'
		for ( int c = divisore + 1; c < n; c++){
			printf(": ");
		}
		printf("\n");
		divisore++;
	}


	return 0;
}
