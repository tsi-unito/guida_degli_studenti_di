#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#define MAX_N 20

void fillArray(int[], int);
bool isEmpty(int);
int sommaSelettiva(int[], int);
int getMin(int [], int);

int main(){
	int dim;	

	//printf("inserisci il numero di elementi da inserire: ");
	scanf("%d",&dim);

	int arr[dim];
	fillArray(arr, dim);

	if (isEmpty(dim)){
		printf("Vuoto.");
	} else {
		printf("%d", sommaSelettiva(arr, dim));
	}

	return 0;
}

void fillArray(int arr[], int dim){
	for (int i = 0; i < dim; i++){
		scanf("%d",&arr[i]);
	}
}

bool isEmpty(int dim){
	return dim == 0;
}

int getMin(int arr[], int dim){
	int min = arr[0];	

	for (int i = 0; i < dim; i++){
		if ( arr[i] < min ){
			min = arr[i];
		}	
	}

	return min;
}

int sommaSelettiva(int arr[], int dim){
	int quad = getMin(arr, dim);
	quad = quad * quad;
	int somma = 0;

	for (int i = 0; i < dim; i++){
		if (arr[i] >= quad){
			somma = somma + arr[i];	
		//	printf("somma con quad = %d, %d",quad, somma);
		}	
	}

	return somma;
}
