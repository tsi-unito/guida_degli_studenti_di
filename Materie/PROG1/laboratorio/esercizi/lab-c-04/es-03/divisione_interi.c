#include <stdio.h>
#include <math.h>

// Scrivere un programma che riceve dall'input un numero intero n e scrive in output
// tutti i divisori di n in ordine crescente.
//
// Esempio con input 12:
// 1 2 3 4 6 12

int main(){
	int n;

	scanf("%d", &n);

	for (int i = 1; i <= n; i++){
		if ( n % i == 0){ // il numero n e' divisibile da i
			printf("%d ", i);
		}	
	} 

	return 0;
}
