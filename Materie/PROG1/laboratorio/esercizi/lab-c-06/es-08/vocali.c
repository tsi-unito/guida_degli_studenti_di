#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_LEN 20

// CONSEGNA:
// Scrivere un programma vocali.c che legge dallo standard input una parola
// ( usare %s con scanf() ) e svolge le seguenti operazioni:
// 	1. Stampa "1" se ci sono delle vocali, altrimenti stampa "0".
// 	   Per fare cio' scrivere le funzioni:
// 	   1.a	is_vowel: ritorna un valore booleano per determinare se un singolo carattere
// 	   		  e' una vocale. Considerando i caratteri AEIOUaeiou.
// 	   1.b 	esistono_vocali: prende in input una stringa e ritorna un booleano per indicare
// 	   			 se esistono vocali nella stringa.
// 	2. Stampa le sole vocali presenti nella perola in input tramite il metodo stampa_vocali.

bool is_vowel(char c);
bool esistono_vocali(char word[]);
void stampa_vocali(char word[]);


int main(){
	char str[MAX_LEN];
	printf("Inserisci una parola: ");
	scanf("%s", str);

	printf("Vocali: %c", esistono_vocali(str) ? '1' : '0');
	stampa_vocali(str);

}

bool is_vowel(char c){
	bool result = false;

	if (tolower(c) == 'a' || tolower(c) == 'e' || tolower(c) == 'i' || tolower(c) == 'o' || tolower(c) == 'u' ) {
		result = true;
	}

	return result;
}

bool esistono_vocali(char word[]){
	bool res = false;

	size_t i = 0;

	for (; i < strlen(word) && !is_vowel(word[i]); i++){	
	}

	if( i< strlen(word)){
		res = true;
	}

	return res;
}

void stampa_vocali(char word[]){
	if (esistono_vocali(word)){
		for ( size_t i = 0; i < strlen(word); i++){
			if (is_vowel(word[i])){
				printf("%c ", word[i]);
			}
		}
	}
}
