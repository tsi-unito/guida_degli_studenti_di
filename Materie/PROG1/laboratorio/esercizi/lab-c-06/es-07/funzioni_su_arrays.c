#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

#define ARR_LEN 20
// CONSEGNA:
// Scrivere una funzione leggi_array che legge un numero naturare N ( 0 <= N <= 20 ) seguito da N numeri interi,
// e scrive questi numeri in un array ( ricevuto da input ).
//
// Scrivere una funzione conta_univoci che, dato un array, calcola e ritorna il numero di elementi che compaiono 
// una sola volta all'interno dell'array.
//
// Scrivere una funzione stampa_elementi_ripetuti che prende in ingresso due array e stampa tutti gli elementi 
// del primo array che appaiono anche nel secondo array.
// Se lo stesso elemento appare piu' volte nel primo array, allora viene stampato piu' volte.
//
//
// Scrivere un programma che legge due array da standard input, stampa i risultati della funzione conta_univoci
// chiamato sia sul primo che sul secondo array e poi stampa, su di una nuova riga gli elementi ripetuti
// (usando stampa_elementi_ripetuti)

int leggi_array(int* pArr);
int conta_univoci(int* pArr, int dim);
void stampa_elementi_ripetuti(int* pArr1, int dim1, int* pArr2, int dim2);

int main(){
	int arr1[ARR_LEN];
	int arr2[ARR_LEN];

	int dim1 = leggi_array(arr1);
	int dim2 = leggi_array(arr2);

	printf("%d ", conta_univoci(arr1, dim1));
	printf("%d\n", conta_univoci(arr2, dim2));

	
	stampa_elementi_ripetuti(arr1, dim1, arr2, dim2);

	return 0;
}

int leggi_array(int* pArr){
	int dim;

	scanf("%d",&dim);
	assert(dim >= 0 && dim <= ARR_LEN);

	for (int* pI = pArr; pI <= pArr + dim -1; pI++){
		scanf("%d", pI);		
	}

	return dim;
}

int conta_univoci(int* pArr, int dim){
	int uniqueCounter= 0;

	for (int* pI = pArr; pI <= pArr + dim -1; pI++){
		int localCounter = 0;	
		for (int* pC = pArr; pC <= pArr + dim -1; pC++ ){
			if ( *pC == *pI ) {
				localCounter++;
			}
		}

		if (localCounter <= 1){
			uniqueCounter++;
		}
	}

	return uniqueCounter;
}

void stampa_elementi_ripetuti(int* pArr1, int dim1, int* pArr2, int dim2){
	for ( int* pI = pArr1; pI <= pArr1 + dim1 -1; pI++){
		bool hasBeenPrinted = false;
		for ( int* pC = pArr2; pC <= pArr2 + dim2 -1 && !hasBeenPrinted; pC++){
			if ( *pI == *pC ){
				printf("%d ", *pI);
				hasBeenPrinted = true;
			}
		}
	}
}
