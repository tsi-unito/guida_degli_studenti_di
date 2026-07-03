#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

// Srivere un programma che implementa i seguenti punti:
// 	- Dato un intero n letto da input con scanf(), si stampino a video tutti gli interi
//	  da 0 a n e poi da n a 0
//
//	  Domanda: quale ciclo e' piu' appropriato? : For loop
//	- Stampare a video tutti gli interi dispari da 0 a n
//	- Stampare il fattoriale di n come 1 * 2 * 3..
//

void es1(int n); // stampe
void es2(int n); // stampa numeri dispari
void es3(int n); // stampa fattoriale


int main(){
	int n;

	printf("inserisci un numero: ");
	scanf("%d",&n);

	es1(n);
	es2(n);
	es3(n);

	return 0;
}

void es1(int n){
	printf("\nesercizio 1.a stampa numeri da 0 a n :");

	for (int i = 0; i <= n; i++){
		printf(" %d",i);
	}

	printf("\nesercizio 1.b stampa numeri da n a 0 :");

	for (int i = n; i>=0; i--){
		printf(" %d", i);
	}	
}

void es2(int n){
	printf("\nesercizio 2 - stampa numeri dispari: ");

	for (int i = 1; i<=n; i = i + 2){
		printf(" %d", i);
	}
}

void es3(int n){
	int fattoriale = 1;
	printf("\nesercizio 3 - stampa fattoriale di %d: ", n);

	for (int i = 1; i <= n; i++){
		if ( i == n ) {
			printf(" %d", i);
		} else {
			printf(" %d *", i);
		}
		fattoriale = fattoriale * i;
	}

	printf(" = %d", fattoriale);
}
