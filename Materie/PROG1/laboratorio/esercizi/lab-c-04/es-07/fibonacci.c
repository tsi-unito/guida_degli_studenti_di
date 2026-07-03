#include <stdio.h>
#include <assert.h>

// Scrivere un programma fibonacci.c che legge in input un numero k e stampa a 
// video i primi k numri della successione di fibonacci.
//
// Esempio con k = 10
// 0 1 1 2 3 5 8 13 21 34
//
// Per semplicita assumere che k >= 2
// Si ricorda che fibonacci parte da due numeri 0 ed 1 e ogni elemento 
// successo della sequenza e' ottenuto come somma dei 2 precedenti.
//
// Suggerimento:
// partendo da n=0 e m=1 e stamparli.
// Ad ogni iterazione si procede ad aggiornare le variabili:
// n' = m, m' = n + m
// e stampare m'
//
// Domanda" per fare questo aggiornamento serve una var. temporanea? Si


int main(){
	int k;
	int n = 0;
	int m = 1;

	printf("Inserisci un numero: ");
	scanf("%d",&k);
	
	assert(k >=2 );

	printf("%d ",n);

	for (int i = 1; i < k; i++){
		int tmp = n;
		printf("%d ",m);

		n = m;
		m = tmp + m;
	}

	return 0;
}
