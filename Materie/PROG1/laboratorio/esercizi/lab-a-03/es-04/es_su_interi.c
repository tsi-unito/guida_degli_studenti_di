#include <stdio.h>
#include <stdbool.h>

// leggere dall'input 2 numeri: n ed m
// se n e' pari e n > m: stampa c1
// se n e' pari e n <= m: stampa c2
// se n e' dispari:
// 	- e m e' dispari: stampa c3
// 	- e m non e' dispari e m > 2n: stampa c4
// 	- altrimenti: stampa altro
int main(){
	int n,m;

	scanf("%d %d", &n, &m);


	if ( n % 2 == 0 ) { // n e' pari
		printf("%s", n > m ? "C1" : "C2" );	
		return 0;
	}

	// n e' dispari
	
	if ( m % 2 != 0 ) { // m e' dispari
		printf("%s","C3");
		return 0;
	}

	// n e' dispari ed m NON e' dispari
	
	if ( m > 2*n) {
		printf("C4");
		return 0;
	}

	printf("ALTRO");
	return 0;
}
