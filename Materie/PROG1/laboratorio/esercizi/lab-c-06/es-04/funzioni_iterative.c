#include <stdio.h>
#include <math.h>
#include <assert.h>

// COMPLETARE: funzione esponenziale
// Dati una base b e un esponente e (come parametri), restituisce 
// il valore di b elevato ad e. Assumere e>=0.
int esponenziale(int base, int esponente) {
	assert(esponente >= 0);

	int exp = 1;

	for (int i = 0; i < esponente; i++){
		exp = exp *base;
	}

	return exp;
}

// COMPLETARE: prodotto_multipli
// Presi come parametri tre interi non-negativi n, m e q restituisce 
// il prodotto di tutti gli interi compresi tra n e m (estremi inclusi) 
// che sono multipli di q.
int prodotto_multipli(int n,int m, int q) {
	assert(n>=0);
	assert(m>=0);
	assert(q>=0);

	int prod = 1;
	for (int i = n; i <= m; i++){
		if ( i % q == 0){
			prod = prod * i;
		}
	}
	return prod;
}

// COMPLETARE: stampa_al_rovescio
// Preso un intero n>=0 stampa a video i valori da n a 0.
void stampa_al_rovescio(int n) {
	assert(n>=0);

	for (int i=n; i >=0; i--){
		printf("%d", i);
	}
}

// Non modificare il main.
int main(void) {
	puts("\nEsponenziale:");
	printf("esponenziale(3, 6) = %3d  [atteso:729]\n", esponenziale(3, 6));
	printf("esponenziale(5, 3) = %3d  [atteso:125]\n", esponenziale(5, 3));
	printf("esponenziale(2, 2) = %3d  [atteso:4]\n", esponenziale(2, 2));
	printf("esponenziale(1, 4) = %3d  [atteso:1]\n", esponenziale(1, 4));
	printf("esponenziale(0, 2) = %3d  [atteso:0]\n", esponenziale(0, 2));
	printf("esponenziale(0, 0) = %3d  [atteso:1]\n", esponenziale(0, 0));
	printf("esponenziale(3, 0) = %3d  [atteso:1]\n", esponenziale(3, 0));
	printf("esponenziale(5, 1) = %3d  [atteso:5]\n", esponenziale(5, 1));

	puts("\nProdotto multipli:");
	printf("prodotto_multipli(3, 6, 2)  = %3d   [atteso:24]\n", prodotto_multipli(3, 6, 2));
	printf("prodotto_multipli(1, 30, 10) = %3d  [atteso:6000]\n", prodotto_multipli(1, 30, 10));
	printf("prodotto_multipli(27, 42, 8) = %3d  [atteso:1280]\n", prodotto_multipli(27, 42, 8));

	puts("\nStampa al rovescio:");
	printf("stampa_al_rovescio(5):    [atteso: 5 4 3 2 1 0]\n");
	stampa_al_rovescio(5);
	printf("stampa_al_rovescio(0):    [atteso: 0]\n");
	stampa_al_rovescio(0);
}








