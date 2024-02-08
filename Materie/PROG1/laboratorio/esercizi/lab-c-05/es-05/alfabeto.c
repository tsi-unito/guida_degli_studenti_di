#include <stdio.h>
#include <stdlib.h>

// CONSEGNA:
// Scrivere un programma che crea un array di 27 caratteri in cui
// -	IL programma inizializza con un ciclo for i primi 26 caratteri
//	con le lettere progressive dell'alfabeto dalla 'A' fino alla 'Z'.
// -	Inserisce il terminatore della stringa
// -	Stampa la stringa ottenuta con printf()

int main(){
	int alfabeto_size = 27;
	char alfabeto [alfabeto_size];
	size_t asciiLetter = 65;

	for (size_t i=0; i < alfabeto_size-1; i++){
		alfabeto[i] = asciiLetter;
		asciiLetter++;
	}

	alfabeto[alfabeto_size] = '\0';

	printf("%s", alfabeto);
	return 0;
}
