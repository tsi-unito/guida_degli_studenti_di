#include <stdio.h>
#include <stdbool.h>
// NON includere <string.h> !

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// copia nell’intervallo [dstStart, dstStart+strLen) i caratteri 
// della stringa str1 seguiti dai caratteri della stringa str2. 
// Ricordarsi di aggiungere il terminatore alla fine.
// Se l’intervallo non ha spazio sufficiente, la copia termina 
// quando lo spazio nell’intervallo è esaurito. 
void my_concat(char* dstStart, const size_t strLen,
               const char* str1, const char* str2);

// Scrivere una funzione my_equal che prende in ingresso due 
// stringhe e restituisce true se sono identiche, carattere per carattere,
// altrimenti restituisce false.
bool my_equal(char* str1, char* str2);

// copia nell’intervallo [dstStart, dstStart+strLen) i caratteri della
// stringa str in modo tale che ogni carattere viene copiato N volte.
// Ricordarsi di aggiungere il terminatore alla fine.
// Se l’intervallo non ha spazio sufficiente, lo sdoppiamento termina 
// quando lo spazio nell’intervallo è esaurito. 
// Esempio: la stringa "aBc" con N=3 produce "aaaBBBccc" 
//          (assumendo sufficiente spazio)
void n_uplica_lettere(char* dstStart, const size_t strLen,
                      const char* str, const size_t N);

// restituisce la lunghezza della stringa str
size_t my_strlen(char* str);

// restituisce true se la stringa str inizia con il prefisso prefix,
// altrimenti false.
// NB. La stringa vuota "" è prefisso di ogni stringa.
bool startswith(char* str, char* prefix);

// restituisce true se la stringa str contiene la sottostringa sub,
// false altrimenti.
// size_t* left e size_t* right sono due puntatori a variabili che
// vengono usate per restituire:
//  - l'indice del primo carattere di str che fa parte della sottostringa sub e 
//  - l'indice del primo carattere di str che non fa parte della sottostringa sub.
// NB. La stringa vuota "" è contenuta in ogni stringa.
// NB. Consideriamo la prima occorrenza di sub in str.
bool contains(const char* str, const char* sub, 
              size_t* left, size_t* right);


//------------------------------------------------------------------

int main(void) 
{
    const char* s1 = "Prog";
    const char* s2 = "rammazione";

#   define NBUF1  20
#   define NBUF2  10
    char buffer1[NBUF1], buffer2[NBUF2];

    printf("Testing my_concat()\n");
    my_concat(buffer1, NBUF1, s1, s2);
    printf("%-20s   [Atteso: Programmazione]\n", buffer1);

    my_concat(buffer2, NBUF2, s1, s2);
    printf("%-20s   [Atteso: Programma]\n", buffer2);

    my_concat(buffer1, 0, s1, s2); // non deve rompersi, ne copiare nulla

    puts("");

    printf("Testing my_equal()\n");
    printf("%d [Atteso: 1]\n", my_equal(buffer2, "Programma"));
    printf("%d [Atteso: 0]\n", my_equal(buffer2, "banana"));
    printf("%d [Atteso: 1]\n", my_equal("ananas", "ananas"));
    printf("%d [Atteso: 0]\n", my_equal("ananas", "mela"));
    printf("%d [Atteso: 0]\n", my_equal("melagrana", "mela"));
    printf("%d [Atteso: 0]\n", my_equal("mela", "melagrana"));
    printf("%d [Atteso: 1]\n", my_equal("", ""));

    puts("");
    printf("Testing n_uplica_lettere()\n");
    n_uplica_lettere(buffer1, NBUF1, "aBc", 3);
    printf("%-20s   [Atteso: aaaBBBccc]\n", buffer1);

    n_uplica_lettere(buffer2, NBUF2, "XyZ", 4);
    printf("%-20s   [Atteso: XXXXyyyyZ]\n", buffer2);

    n_uplica_lettere(buffer2,NBUF2, "", 4);
    printf("%-20s   [Atteso: ]\n", buffer2);

    n_uplica_lettere(buffer2, NBUF2, "ciao", 0);
    printf("%-20s   [Atteso: ]\n", buffer2);

    n_uplica_lettere(buffer2, NBUF2, "mela", 2);
    printf("%-20s   [Atteso: mmeellaa]\n", buffer2);

    puts("");

    printf("Testing my_strlen()\n");
    printf("%2zu [Atteso: 0]\n", my_strlen(""));
    printf("%2zu [Atteso: 16]\n", my_strlen("programmazione-1"));
    printf("%2zu [Atteso: 6]\n", my_strlen("banana"));
    printf("%2zu [Atteso: 1]\n", my_strlen("a"));

    puts("");
    printf("Testing startswith()\n");
    printf("%d [Atteso: 1]\n", startswith("programmazione", "programmazione"));
    printf("%d [Atteso: 1]\n", startswith("programmazione", "programma"));
    printf("%d [Atteso: 0]\n", startswith("programma", "programmazione"));
    printf("%d [Atteso: 1]\n", startswith("", ""));
    printf("%d [Atteso: 1]\n", startswith("programmazione", ""));

    puts("");
    printf("Testing contains()\n");

    size_t left, right;

    bool res = contains("programmazione", "prog", &left, &right);
    printf("ret=%d    [%zu, %zu)  | Atteso: 1   [0, 4)\n", res, left, right);

    res = contains("stringhe", "programmazione", &left, &right);
    printf("ret=%d    [%zu, %zu)  | Atteso: 0   [0, 0)\n", res, left, right);

    res = contains("programmazione", "", &left, &right);
    printf("ret=%d    [%zu, %zu)  | Atteso: 1   [0, 0)\n", res, left, right);

    res = contains("", "", &left, &right);
    printf("ret=%d    [%zu, %zu)  | Atteso: 1   [0, 0)\n", res, left, right);

    res = contains("programmazione", "azione", &left, &right);
    printf("ret=%d    [%zu, %zu) |  Atteso: 1   [8  14)\n", res, left, right);
}


//------------------------------------------------------------------
// copia nell’intervallo [dstStart, dstStart+strLen) i caratteri 
// della stringa str1 seguiti dai caratteri della stringa str2. 
// Ricordarsi di aggiungere il terminatore alla fine.
// Se l’intervallo non ha spazio sufficiente, la copia termina 
// quando lo spazio nell’intervallo è esaurito.
void my_concat(char* dstStart, const size_t strLen,
               const char* str1, const char* str2)
{
	size_t i=0;

    	for(size_t j=0;str1[j] != '\0' && i<strLen-1; j++){
		dstStart[i] = str1[j];
		i++;
    	}
	
	
	for(size_t j=0;str2[j] != '\0' && i<strLen-1; j++){
		dstStart[i] = str2[j];
		i++;
    	}
	dstStart[i] = '\0';
}

//------------------------------------------------------------------
// Scrivere una funzione my_equal che prende in ingresso due 
// stringhe e restituisce true se sono identiche, carattere per carattere,
// altrimenti restituisce false.

bool my_equal(char* str1, char* str2)
{
    bool result = true;

    char* pStr1 = str1;
    char* pStr2 = str2;

    while (result && (*pStr1 != '\0' && *pStr2 != '\0')){
	if (*pStr1 != *pStr2){
		result = false;
	} 
	pStr1++;
	pStr2++;
    }
    if (*pStr1!=*pStr2){
	result = false;
    }
    
    return result;
}

//------------------------------------------------------------------
// copia nell’intervallo [dstStart, dstStart+strLen) i caratteri della
// stringa str in modo tale che ogni carattere viene copiato N volte.
// Ricordarsi di aggiungere il terminatore alla fine.
// Se l’intervallo non ha spazio sufficiente, lo sdoppiamento termina 
// quando lo spazio nell’intervallo è esaurito. 
// Esempio: la stringa "aBc" con N=3 produce "aaaBBBccc" 
//          (assumendo sufficiente spazio)

void n_uplica_lettere(char* dstStart, const size_t strLen,
                      const char* str, const size_t N)
{
	size_t j=0;
	for(size_t i=0; str[i]!='\0' && j<strLen;i++){
		for(size_t c=0;c<N && j<strLen;c++){
			dstStart[j]=str[i];
			j++;
		}
	}
	dstStart[j]='\0';
}

//------------------------------------------------------------------
// restituisce la lunghezza della stringa str

size_t my_strlen(char* str){
	size_t i = 0;
	
	for(char*pStr = str; *pStr!='\0'; pStr++){
		i++;
	}

    	return i;
}

//------------------------------------------------------------------
// restituisce true se la stringa str inizia con il prefisso prefix,
// altrimenti false.
// NB. La stringa vuota "" è prefisso di ogni stringa.
bool startswith(char* str, char* prefix){
    bool result = true;

    if(*prefix!='\0'){
	size_t i=0;
	char* pPrefix=prefix;
	for(; str[i]!='\0'&&*pPrefix!='\0' && result; pPrefix++){
		if(*pPrefix!=str[i]){
			result=false;
		} 
		i++;
	}

	if(str[i]=='\0' && *pPrefix!='\0'){
		result=false;
	}
    }

    return result;
}

//------------------------------------------------------------------
// restituisce true se la stringa str contiene la sottostringa sub,
// false altrimenti.
// size_t* left e size_t* right sono due puntatori a variabili che
// vengono usate per restituire:
//  - l'indice del primo carattere di str che fa parte della sottostringa sub e 
//  - l'indice del primo carattere di str che non fa parte della sottostringa sub.
// NB. La stringa vuota "" è contenuta in ogni stringa.
// NB. Consideriamo la prima occorrenza di sub in str.

bool contains(const char* str, const char* sub, 
              size_t* left, size_t* right)
{
    	bool result=false;
	*left=0;
	*right=0;
	
	size_t j=0;

	if(*sub!='\0'){
		size_t i=0;
		for(; str[i]!='\0' && !result && sub[j]!='\0'; i++){
			if(str[i]==sub[j]){
				if(j==0){
					*left=i;
				}
				j++;
			}else{
				j=0;
			}
		}	

		if(sub[j]=='\0'){
			result=true;
			*right=i;
		}
	}else{
		result = true;
	}

    	return result;
}
