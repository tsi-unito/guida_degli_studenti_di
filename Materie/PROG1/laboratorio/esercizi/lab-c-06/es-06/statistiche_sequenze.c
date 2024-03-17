#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

// CONSEGNA:
// Scrivere un programma che legge una serie di sequenze di numeri interi positivi terminati da 0.
// Per ciascusa sequenza letta, il main deve stampare su una linea di testo i seguenti elementi:
//
// 	1. La somma degli elementi della sequenza
// 	2. La somma dei quadrati degli elementi della sequenza
// 	3. Il massimo degli elementi della sequenza
//
// Se la sequenza non ha elementi ( subito 0 ) il programma termina.
//

bool leggiSequenza(int* sommaElems, int* sommaQuad, int* maxElem);

int main(){

	bool leggiSeq = true;

	do {
		int sommaElems = 0;
		int sommaQuad = 0; 
		int maxElem = 0;

		leggiSeq = leggiSequenza(&sommaElems, &sommaQuad, &maxElem);

		if ( leggiSeq ){
			// Stampa dati
			printf("%d %d %d\n", sommaElems, sommaQuad, maxElem);
		}
	} while ( leggiSeq );

	return 0;
}


bool leggiSequenza(int* sommaElems, int* sommaQuad, int* maxElem){
	int currElem;
	bool hasRead = false;

	do {
		scanf("%d", &currElem);
		assert(currElem >= 0);

		if (currElem != 0){
			*sommaElems += currElem;
			*sommaQuad += currElem * currElem;
			*maxElem = currElem > *maxElem ? currElem : *maxElem;
			hasRead = true;
		}

	} while (currElem != 0);

	return hasRead;
}
