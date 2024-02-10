#include <stdio.h>
#include <stdlib.h>


int count(int arr[], int dim, int n);

int main(){
	int arr[] = {5,4,3,5,6,7,8,98,9,9,7,5};

	int n;

	printf("inserisci un numero: ");
	scanf("%d",&n);

	int countArr = count(arr, 12, n); 

	printf("Le occorrenze del numero %d nell'array sono %d", n, countArr);

	return 0;
}

int count(int arr[], int dim, int n){
	int counter = 0;
	for (int i = 0; i < dim; i++){
		if (arr[i] == n){
			counter++;
		}
	}

	return counter;
}
