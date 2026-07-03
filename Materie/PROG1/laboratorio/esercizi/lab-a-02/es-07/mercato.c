#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

int main(){
	const float prezzoOrecchiette = 1.50;
	const float prezzoKgPomodori  = 2.50;

	const int amountOrecchiette = 3;
	const float amountPomodori = 1.5;

	printf("Ecco quanto hai acquistato al supermercato: ");
	
	const double totale = (amountOrecchiette * prezzoOrecchiette ) + (amountPomodori * prezzoKgPomodori);

	printf("%.4f",totale);

	return 0;
}
