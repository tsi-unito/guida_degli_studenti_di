#include <stdio.h>

int main(){
	int x1,x2,x3;

	printf("inserisci 3 numeri: ");
	scanf("%d %d %d",&x1, &x2, &x3);
	
	int scelta;
	do{
		printf("1. somma \n");
		printf("2. prodotto \n");
		printf("3. media \n");
		printf("0. esci\n scelta:");

		scanf("%d",&scelta);

		if (scelta < 0 || scelta >3) {
			printf("\nerrore: scelta non valida");
		}

		if (scelta == 1) {
			printf("\n%d", x1 + x2 + x3);
		}

		if (scelta == 2) {
			printf("\n%d", x1 * x2 * x3);
		}

		if (scelta == 3) {
			printf("\n%f", ( (float) x1 + x2 + x3) / 3);
		}
	}while( scelta != 0 );
	return 0;
}
