#include <stdio.h>
#include <stdbool.h>

// Scrivere un programma che legge da input una sequenza di numeri interi terminata dallo 0
//
// Letta la sequenza, il programma procede a stampare i seguenti messaggi:
// "TUTTI_DISPARI"	-	se tutti i numeri della sequenza sono dispari
// "TUTTI_MULTIPLI_DI_3"	se tutti i numeri della sequenza sono multipli di 3
// "ESISTE_R7"			se almeno un numero da resto 7 se diviso per 10
// "ESISTE_100"			se esiste il numero 100 nella sequenza


int main(){
	int n;	
	bool dispari = true;
	bool multipliTre = true;
	bool esisteR7 = false;
	bool esisteR100 = false;

	while( scanf("%d",&n) && n != 0){
		if ( n == 100){
			esisteR100 = true;
		}

		if ( n % 2 == 0){
			dispari = false;
		}

		if ( n % 3 != 0){
			multipliTre = false;
		}	

		if ( n % 10 == 7 ){
			esisteR7 = true;
		}
	}

	if (dispari){
		printf("TUTTI_DISPARI ");
	}

	if (multipliTre){
		printf("TUTTI_MULTIPLI_3 ");
	}

	if (esisteR7){
		printf("ESISTE_R7 ");
	}

	if (esisteR100){
		printf("ESISTE_100");
	}

	return 0;
}
