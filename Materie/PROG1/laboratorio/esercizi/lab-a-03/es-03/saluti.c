#include <stdio.h>
#include <stdlib.h>


int main(){
	int ora = 0;
	printf("inserisci l'ora: ");
	scanf("%d", &ora);

	if (ora>=6 && ora <= 11){
		printf("buongiorno");
	}

	if (ora >= 12 && ora <= 18) {
		printf("buon pomeriggio");
	}

	if (ora >= 19 && ora <= 22){
		printf("buona sera");
	}

	if (ora >= 23 && ora <= 5 ) {
		printf("buona notte");
	}
	return 0;
}
