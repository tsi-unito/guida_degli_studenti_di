#include <stdio.h>
#include <stdlib.h>
#define NUM_RISP 20

/*
 * 	Scrivere un programma
	questionari.c che calcola le frequenze delle 5 possibili risposte,
	e ne stampa un sommario.
	
	Usare un array frequenze[] per
	accumulare le frequenze delle 5
	possibili risposte, prima di stamparle.
 */

void printStelle(int freq){
	printf("%d\t", freq);

	for (int i = 0; i < freq; i++){
		printf("#");
	}
	printf("\n");
}

void calcFreq(int arr[], int dim, int star){
	int media = 0;
	int somma = 0;

	for(int i=0; i < dim; i++){
		if (arr[i] == star){
			somma++;
		}
	} 	


	printf("%d\t", star);
	printStelle(somma);
	printf("\n");
}

void printArr(int arr[], int dim){
	for (int i = 0; i < dim; i++){
		printf("%d\t",arr[i]);
	}

	printf("\n");
}

int main(){
	int risposte[NUM_RISP] = {
		1, 2, 5, 4, 3, 5, 2, 1, 3, 1,
		4, 3, 3, 3, 2, 3, 3, 2, 2, 5
	};

	//printArr(risposte, NUM_RISP);

	calcFreq(risposte, NUM_RISP, 1);
	calcFreq(risposte, NUM_RISP, 2);
	calcFreq(risposte, NUM_RISP, 3);
	calcFreq(risposte, NUM_RISP, 4);
	calcFreq(risposte, NUM_RISP, 5);

	
	return 0;
}
