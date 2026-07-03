#include <stdio.h>
#include <stdbool.h>

// Scrivere un programma che riceve dallo standard input (scanf) due numeri interi n ed m, e stampa un messaggio
// a seconda che valga una di queste condizioni:
//
// - Se n e' pari e maggiore di m, allora stampa "C1"
// - Se n e' pari e minore o uguale ad m, allora stampa "C2"
// - Se n e' dispari, procedere in questo modo:
// 	- se anche m e' dispari, stampare C3
// 	- se, invece, m non e' dispari ma piu' del doppio di n, allora stampa "C4"
// 	- in ogni altro caso, stampa "ALTRO"

bool printWhenNIsPair(bool hasPrinted, int n, int m);
void printWhenNIsOdd(int n, int m);

int main(){
	int n,m;
	bool hasPrinted = false;
	bool isNPair = false;

	scanf("%d %d", &n, &m);

	isNPair = n % 2 == 0;

	if ( isNPair ) {
		hasPrinted = printWhenNIsPair(hasPrinted,n,m);
	}	

	if ( !hasPrinted ) { // this means that n is not pair
		printWhenNIsOdd(n,m);
	}

	return 0;
}

bool printWhenNIsPair(bool hasPrinted, int n, int m){
	if ( !hasPrinted) {
		if ( n > m ) {
			printf("C1");
		} else {
			printf("C2");
		}
		hasPrinted = true;
	}

	return hasPrinted;
}

void printWhenNIsOdd(int n, int m){
	bool isMOdd = m % 2 != 0;

	if ( isMOdd ) {
		printf("C3");
	} else if ( !isMOdd && m > n * 2 ) {
		printf("C4");
	} else {
		printf("ALTRO");
	}
}
