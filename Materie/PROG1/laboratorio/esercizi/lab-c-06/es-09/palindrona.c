#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

// CONSEGNA:
// Scrivere un programma che legge una singola parola dall'input con scanf() e determina
// tramite una funzione, se e' una parola palindroma ( invariata se letta da destra o sinistra ).
//
// Se e' palindroma, stampa "PALINDROMA" e termina. La parola in input e' garantita di avere al piu' 50
// caratteri.
//
// Se NON LO E' (palindroma), il programma deve copiare la stringa invertendo l'ordine dei caratteri
// e stampare la stringa original e la stringa capovolta.
// Per fare questo:
// 	- Scrivi una funzione capovolgi, che prende in input una stringa src ed un array di carateri
// 	  dst, e procede a copiare in senso inverso i caratteri della stringa src dentro dst.

#define MAX_LEN 51

bool is_palindroma(char str[]);
void capovolgi(char src[], char dest[]);
void print_str(char str[]);


int main(){
	char str[MAX_LEN];

	scanf("%50s",str);
	
	if(is_palindroma(str)){
		printf("PALINDROMA");
	} else {
		char cp[MAX_LEN];
		capovolgi(str, cp);
		print_str(str);
		printf(" ");
		print_str(cp);
	}
}


bool is_palindroma(char str[]){
	size_t i=0;
	size_t len=strlen(str);
	size_t j=len-1;

	bool result = true;


	for (; i < len && i <= j && str[i] == str[j]; i++,j--){
	}

	if (i <= j){
		result = false;
	}

	return result;
}

void capovolgi(char src[], char dest[]){
	for (size_t i = 0; i < strlen(src); i++){
		//printf("capovolgendo %zu-esimo elemento ('%c') nella posizione %zu di dest\n", i, src[i], strlen(src)-i-1);
		dest[strlen(src)-i-1] = src[i];
		//printf("dest[%zu] = %c\n", strlen(src)-i-1, dest[strlen(src)-i-1]);
	}

	dest[strlen(src)] = '\0';

}

void print_str(char str[]){
	for (size_t i = 0; i < strlen(str); i++){
		printf("%c", str[i]);
	}
}
