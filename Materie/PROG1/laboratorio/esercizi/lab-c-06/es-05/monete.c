#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// CONSEGNA:
// Scrivere un programma che chiede all'utente un numero naturale cent
// e successivamente stampa a video il piu' piccolo cambio in monete.
//
// I tagli di monente sono: 1,2,4,10,20,50
//
// Esempio: Se l'utente chiede in cambio di 95 cent, il programma scrivera:
// "1 da 50 cent"
// "2 da 20 cent"
// "1 da 5 cent"

#define NUM_TAGLI 6

void printCambio(int* pArr, int dim, int cent);

int main(){
	int tagli_monete[NUM_TAGLI] = { 1, 2, 5, 10, 20, 50 };

	int cent;
	printf("Inserisci il cambio in centesimi: ");
	scanf("%d",&cent);

	assert(cent >= 0);

	int* pArr = &tagli_monete[0];
	printCambio(pArr, NUM_TAGLI, cent);

	return 0;
}

void printCambio(int* pArr, int dim, int cent){
	for ( int* pI = pArr + dim-1; pI >= pArr ; pI-- ){
		// Iterare altre n volte per vedere quante volte puo' essere diviso
		int coinCounter = 0;

		while ( cent - *pI >= 0 ){
			coinCounter++;
			cent = cent - *pI;
		}

		if ( coinCounter > 0 ){
			printf("%d da %d cent\n", coinCounter, *pI);
		}
	}
}
