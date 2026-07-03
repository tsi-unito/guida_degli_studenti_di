#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

// CONSEGNA:
// Scrivere un programma che determini se un num numero positivo 'n' e' primo
bool isPrimo(int n);


int main(){
	int n;

	printf("Inserisci un numero: ");

	scanf("%d",&n);

	if(isPrimo(n)) {
		printf("%d e' primo", n);
	} else {
		printf("%d non e' primo", n);
	}

	return 0;
}

bool isPrimo(int n){
	bool result = true;
	int i = 2;

	for(; i < floor(sqrt(n)) && result; i++){
		if ( n % i == 0) {
			result = false;
		}
	}
	

	return result;
}
