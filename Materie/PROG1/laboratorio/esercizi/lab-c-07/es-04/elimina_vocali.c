#include <stdio.h>
#include <stdbool.h>

// CONSEGNA:
// Scrivi un programma che legge una parola da standard input (lunghezza massima 50 caratteri) e chiama una funzione elimina_vocali,
// che si occupa di trasformare la stringa 
// eliminando tutti i caratteri che corrispondono alle lettere vocali.
// Stampare il risultato

// ritorna true se il carattere ch è una vocale
bool is_vowel(char ch) {
#   define NUM_VOWELS 10
    const char vowels[NUM_VOWELS] = { 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U' };
    bool is_v = false;
    for (size_t i=0; i<NUM_VOWELS && !is_v; i++) {
        if (ch == vowels[i])
            is_v = true;
    }
    return is_v;
}

void elimina_vocali(char* str, size_t len);
void stampa_str(char* str, size_t len);
#define MAX_L 50

int main(void) {
    // COMPLETARE
    	char str[MAX_L] = "";
	scanf("%49s", str);

	elimina_vocali(str, MAX_L);
	stampa_str(str, MAX_L);
}


void elimina_vocali(char* str, size_t len){
	char* pStr=str;
	char* pDest=str;
	
	for(;*pStr != '\0'; pStr++){
		if(!is_vowel(*pStr)){
			// Eliminare vocale.
			*pDest=*pStr;
			pDest++;
		}
	}
	*pDest='\0';
}

void stampa_str(char* str, size_t len){
	for(char* pStr; *pStr!='\0'; pStr++){
		printf("%c", *pStr);
	}
}
