#include <stdio.h>

// Scrivere un programma che prende in input un numero n e ne stampa le prime
// n righe del triangolo di floyd.
// Il triangolo di floyd e' costituito dai numeri naturali scritti in modo consecutivo
// per riempire le righe con 1,2,3.. valori.
//
// Ad esempio per n=5:
//
// 1
// 2 3
// 4 5 6
// 7 8 9 10
// 11 12 13 14 15

int main(){
	int n;
	int c = 1;

	printf("inserisci un numero: ");
	scanf("%d",&n);

	for (int i = 1; i <= n; i++){
		for ( int f = 0; f < i; f ++){
			printf("%d ", c);
			c++;
		}	
		printf("\n");
	}

	return 0;
}
