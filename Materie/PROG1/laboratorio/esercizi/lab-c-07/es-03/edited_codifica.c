#include <stdio.h>
#include <ctype.h>
#include <string.h>

// converti tutti i caratteri della stringa str in maiuscolo
void convertToUppercase(char *pStr);

// converti i caratteri (in-place) seguendo questa codifica
//  - Le lettere maiuscole vengono trasformate seguendo queste regole:
//        - A diventa B
//        - B diventa C
//        - C diventa D
//         ...
//        - Y diventa Z
//        - Z diventa A
//  - tutti gli altri caratteri restano immodificati
void codifica(char *pStr);

// esegui la tarsformazione inversa rispetto alla funzione codifica
void decodifica(char *pStr);


int main(void) {
	// inzializza il char array
   char string[] = "<ctype> si usa per trasformare i char (anche all'esame)"; 
   printf("Stringa prima della trasformazione:\n %s\n", string);
   convertToUppercase(string);
   printf("Stringa dopo la trasformazione:\n %s\n", string);

   const size_t len = strlen(string);
   char code[len + 1];
   strncpy(code, string, len + 1);

   codifica(string);
   printf("Stringa dopo la codifica:\n %s\n", string);

   decodifica(string);
   printf("Stringa dopo la decodifica:\n %s\n", string);

   printf("Decodifica con successo? %d\n", 0==strcmp(string, code));
}


// implementazione di convertToUppercase
void convertToUppercase(char *pStr) {
	while (*pStr != '\0') {
		*pStr = toupper(*pStr);
		pStr++;
	}
}

// implementazione di codifica()
// COMPLETARE
void codifica(char *pStr){
	char* cStr = pStr;

	for(;*cStr != '\0'; cStr++){
		char curr = *cStr;

		if(isalpha(curr) && (curr != 'z' && curr!='Z')){
			*cStr=curr+1;
		} else if(isalpha(curr)){
			*cStr= curr == 'z' ? 'a' : 'Z';
		}
	}
}

// implementazione di decodifica()
// COMPLETARE
void decodifica(char *pStr){
	char* cStr = pStr;

	for(;*cStr != '\0'; cStr++){
		char curr = *cStr;

		if(isalpha(curr) && (curr != 'a' && curr!='A')){
			*cStr=curr-1;
		} else if(isalpha(curr)){
			*cStr= curr=='a'? 'z' : 'Z';
		}
	}
}
