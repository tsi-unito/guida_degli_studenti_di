#include <stdio.h>

// funzione helper per avere il maggiore
int getMax(int a, int b){
	return a > b ? a : b;
}

// funzione helper per avere il minore
int getMin(int a, int b){
	return a > b ? b : a;
}

int main(void) {
    // Leggi i dati dallo standard input usando scanf
    int x1, x2, x3, x4;
    scanf("%d%d%d%d", &x1, &x2, &x3, &x4);
    int tempMax;
    int tempMin;
    
    // Scambia per riordinare le variabili, in modo che
    //  x1 <= x2 <= x3 <= x4
    
	tempMax = getMax(x1, x2);
	tempMin = getMin(x1, x2);

	x1 = tempMin;
	x2 = tempMax;
   
	tempMax = getMax(x1, x3);
	tempMin = getMin(x1, x3);
	x1 = tempMin;
	x3 = tempMax;
  	
	
	tempMax = getMax(x1, x4);
	tempMin = getMin(x1, x4);

	x1 = tempMin;
	x4 = tempMax;

	// x1 e' il minimo

	tempMax = getMax(x2, x3);
	tempMin = getMin(x2, x3);

	x2 = tempMin;
	x3 = tempMax;

	tempMax = getMax(x2, x4);
	tempMin = getMin(x2, x4);

	x2 = tempMin;
	x4 = tempMax;

	// x2 e' ordinato
	
	tempMax = getMax(x3, x4);
	tempMin = getMin(x3, x4);

	x3 = tempMin;
	x4 = tempMax;


	// x3 e' ordinato
	
    // Stampa le variabili ordinate
    printf("%d %d %d %d\n", x1, x2, x3, x4);
}

