#include <stdio.h>
#include <stdlib.h>

int main(){
	printf("Inserisci un numero: ");

	int currNum;
	int somma = 0;
	int numElem = 0;

	while (scanf("%d",&currNum) == 1){
		somma += currNum;
		numElem++;
		printf("Inserisci un numero: ");
	}

	if (numElem > 0){
		double media = (double) somma / (double) numElem;
		printf("La media della serie di numeri inseriti e' %f", media);
	} else {
		printf("Non sono stati inseriti numeri");
	}
	

	return 0;
}
