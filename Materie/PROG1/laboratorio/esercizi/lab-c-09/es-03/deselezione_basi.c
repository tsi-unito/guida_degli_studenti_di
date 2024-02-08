#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Una delezione nel DNA è un tipo di mutazione genica che consiste nella perdita di uno o più nucleotidi in una sequenza di DNA.
// Scrivere un programma C che legge in input due stringhe (max: 100 caratteri) seq e del_seq. 
//
// Dopodichè il programma, usando solo funzioni ricorsive, stampa tutti i caratteri di seq, 
// eliminando tutte le occorrenze delle sottosequenze del_seq all'interno di seq e stampando al loro posto il carattere '-'.
//
// Esempio: dati seq="TACGCACGAT" e del_seq="ACG", il programma deve stampare "T-C-AT".
//
// Suggerimento: scrivere due funzioni ricorsive:
// - una funzione prefixR() che determina se una stringa è prefisso di un'altra stringa.
// - una funzione ricorsiva stampa_filtro_substrR() che, ad ogni passo, 
//   determina (usando prefixR) se dal carattere corrente inizia la stringa da non stampare. 
// 
// NOTA:
// Non si possono scrivere cicli for/while/do while.

#define MAX_STR 101

bool is_prefix_r(char* pSeq, char* pDelSeq);
void stampa_filtro_substr_r(char* pSeq, char* pDelSeq, char* pInitDelSeq);

int main(){
	char seq[MAX_STR];
	char del_seq[MAX_STR];

	scanf("%100s",seq);
	scanf("%100s",del_seq);

	stampa_filtro_substr_r(seq, del_seq, del_seq);
	
	//printf("%s", is_prefix_r(seq,del_seq)? "SI" : "NO");

}

bool is_prefix_r(char* pSeq, char* pDelSeq){
	if(*pSeq == '\0'){
		return *pDelSeq == '\0';
	}

	if(*pDelSeq == '\0'){
		return true;
	}

	return *pSeq == *pDelSeq && is_prefix_r(pSeq+1, pDelSeq+1);
}

void stampa_filtro_substr_r(char* pSeq, char* pDelSeq, char* pInitDelSeq){
	if(*pSeq == '\0' && *pDelSeq == '\0'){
		printf("-");
		return;
	}

	if(*pSeq == '\0'){
		return;
	}


	if(*pDelSeq == '\0'){
		printf("-");
		stampa_filtro_substr_r(pSeq, pInitDelSeq, pInitDelSeq);
		return;
	}

	if(!is_prefix_r(pSeq, pDelSeq)){
		printf("%c", *pSeq);
		stampa_filtro_substr_r(pSeq+1, pDelSeq, pInitDelSeq);
		return;
	} 

	stampa_filtro_substr_r(pSeq+1, pDelSeq+1, pInitDelSeq);
}
