#include <stdio.h>

// Scrivere un programma che riceve dallo standard input (scanf) quattro numeri x1,x2,x3 e x4 e li riordini
// con opportune operazioni di scambio e alla fine li ristampi su un unica linea di testo dal piu' piccolo
// al piu' grande.
//
// Suggerimento: procedere con una variabile alla volta:
// 	- partire con x1 e ottenere x <= x2,x3,x4
// 	- seguire con x2 ed ottenere che x2 <= x3, x4
// 	- infine procedere con x3 <=x4
//

int getMin(int a, int b);
int getMax(int a, int b);

int main(){
 	// Leggi i dati dallo standard input usando scanf
 	int x1, x2, x3, x4;
    	scanf("%d%d%d%d", &x1, &x2, &x3, &x4);
    
    	// Scambia per riordinare le variabili, in modo che
    	//  x1 <= x2 <= x3 <= x4
 
	if ( x1 !

    	// Stampa le variabili ordinate
    	printf("%d %d %d %d\n", x1, x2, x3, x4);

	return 0;
}

int getMin(int a, int b){
	return a > b ? b : a;
}

int getMax(int a, int b){
	return a > b ? a : b;
}
