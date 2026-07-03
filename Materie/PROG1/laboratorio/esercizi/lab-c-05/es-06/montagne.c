#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


// CONSEGNA:
// Scrivere un programma in C che legge dallo standard input una sequenza di elementi nella forma:
// <nome_montagna> <altezza>
// dove
// -	il nome_montagna e' una stringa di caratteri non superiore a 50
// -	altezza e' un numero intero maggiore di 0.
//
// La sequenza termina quando non e' piu' possibile leggere elementi.
// Quando la sequenza termina, il programma stampa il nome della montagna piu' alta nella sequenza.

void copyArray(char arr1[], size_t size1, char arr2[], size_t size2);

int main(){
	size_t currMontagnaSize = 50;
	char currMontagna[currMontagnaSize];

	int currAltezza;

	int maxAltezza = -1;

	char maxMontagna[currMontagnaSize];

	while(scanf("%[^\0] %d",currMontagna, &currAltezza) == 2 && currMontagna[0] != '\n'){
		printf("Read: CurrMontagna = %s, currAltezza = %d\n", currMontagna, currAltezza);
		assert(currAltezza > 0);
		if (currAltezza > maxAltezza){
			maxAltezza = currAltezza;
			copyArray(currMontagna, currMontagnaSize, maxMontagna, currMontagnaSize);
		}
	}

	if (maxAltezza != -1){
		printf("%s", maxMontagna);
	}

	return 0;
}

void copyArray(char arr1[], size_t size1, char arr2[], size_t size2){
	assert(size2 >= size1);

	for (size_t i = 0; i < size1; i++){
		arr1[i] = arr2[i];
	}
}
