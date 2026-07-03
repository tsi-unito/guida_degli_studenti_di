#include <stdio.h>
#include <stdbool.h>
// NON POTETE USARE <strings.h>

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// funzione ricorsiva che stampa la stringa pStr, 
// un carattere alla volta.
void stampa_strR(const char* pStr);

// funzione ricorsiva che stampa la stringa pStr, 
// un carattere alla volta, in ordine "rovesciato".
// Es. con pStr="ciao" viene stampato "oaic".
void stampa_rev_strR(const char* pStr);

// funziona ricorsiva che confronta due stringhe, un carattere alla volta,
// e ritorna true se le due stringhe sono uguali e della stessa lunghezza
bool str_ugualiR(const char* pStrA, const char* pStrB);

//------------------------------------------------------------------

int main(void)
{
    #define NUM_STR 7
    const char* stringhe[NUM_STR] = {
        "Bora Bora", "Barcellona", "Santorini", "Havana",
        "Chiang Mai", "Reykjavik", "Rio de Janeiro"
    };
    puts("Unit test per la stampa delle stringhe:");
    for (size_t i=0; i<NUM_STR; i++) {
        stampa_strR(stringhe[i]);
        printf(" | ");
        stampa_rev_strR(stringhe[i]);
        puts("");
    }

    puts("\nUnit test per confronto delle stringhe:");
    printf("%d [Atteso: 0]\n", str_ugualiR("", "Programma"));
    printf("%d [Atteso: 0]\n", str_ugualiR("Banana", "banana"));
    printf("%d [Atteso: 1]\n", str_ugualiR("ananas", "ananas"));
    printf("%d [Atteso: 0]\n", str_ugualiR("ananas", "mela"));
    printf("%d [Atteso: 0]\n", str_ugualiR("melagrana", "mela"));
    printf("%d [Atteso: 0]\n", str_ugualiR("mela", "melagrana"));
    printf("%d [Atteso: 1]\n", str_ugualiR("", ""));
}

//------------------------------------------------------------------

//COMPLETARE

//------------------------------------------------------------------

// funzione ricorsiva che stampa la stringa pStr, 
// un carattere alla volta.
void stampa_strR(const char* pStr){
	if(*pStr!='\0'){
		printf("%c",*pStr);
		stampa_strR(pStr+1);
	}
}

// funzione ricorsiva che stampa la stringa pStr, 
// un carattere alla volta, in ordine "rovesciato".
// Es. con pStr="ciao" viene stampato "oaic".
void stampa_rev_strR(const char* pStr){
	if(*pStr!='\0'){
		stampa_rev_strR(pStr+1);
		printf("%c", *pStr);
	}
}

// funziona ricorsiva che confronta due stringhe, un carattere alla volta,
// e ritorna true se le due stringhe sono uguali e della stessa lunghezza
bool str_ugualiR(const char* pStrA, const char* pStrB){
	if(*pStrA == '\0' || *pStrB=='\0'){
		return *pStrA == '\0' && *pStrB == '\0';
	}

	return *pStrA == *pStrB && str_ugualiR(pStrA+1, pStrB+1);
}


