#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

int main(){
	int a, b;
	
	printf("Inserisci il primo numero: ");
	scanf("%d", &a);

	printf("\nInserisci il secondo numero: ");
	scanf("%d", &b);

	int somma = a+b;

	printf("\n(a+b) = %d", somma);
	printf("\n(a*b) = %d", a*b);
	printf("\n(a/b) = %d", a/b);

	double divReale = (double)a / (double) b;
	printf("\n(a/b) in R = %.5f", divReale);

	printf("\n(a/b) = %d con resto %d", a/b, a % b);
	return 0;
}
