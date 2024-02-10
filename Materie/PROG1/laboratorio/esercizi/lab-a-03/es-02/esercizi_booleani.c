#include <stdio.h>
#include <stdbool.h>

int main(void) {
    int a, b;
    printf("Inserisci due numeri: ");
    scanf("%d%d", &a, &b);

    // a) dati i due interi a e b, stampa 1 se il primo intero 
    //    è multiplo del secondo, 0 altrimenti;
	if ( a % b == 0 ) {
		printf("1");
	}else {
		printf("0");
	}
    // b) dato l'intero a (un voto), stampa "true" se a 
    //    è compreso fra 1 e 30 (inclusi), "false" altrimenti;

    bool b1 = a > 10;
    bool b2 = b <= 5;
   
	if (a >= 1 && a <= 30) {
		printf("true");
	}else {
		printf("false");
	}
    
    // c) dati i due booleani b1 e b2, stampa a video il booleano 
    //    "false" se b1 e b2 sono entrambi veri, "true" altrimenti;

	if (b1 == b2) {
		printf("true");
	}else {
		printf("false");
	}
    // d) dati i due interi a e b, stampa a video il massimo; se sono 
    //    uguali stampa a video "I due valori sono uguali".

	if (a == b){
		printf("i due valori sono uguali");
	}else if (a>b) {
		printf("%d",a);	
	}else {
		printf("%d",b);
	}

    int c = 20;
    // e) dati tre interi a, b e c, stampa a video il massimo 
    //    (si usi una variabile di supporto max); 

	int max = a;

	if (b > max){
		max = b;
	} else if (c > max) {
		max = c;
	}

    // f) dati tre interi a, b e c, stampa a video "ordinati" se questi sono 
    //    ordinati in modo crescente, altrimenti non stampare nulla.

	if ( a <= b && b <= c ) {
		printf("%d < %d < %d", a, b, c);
	}
}
