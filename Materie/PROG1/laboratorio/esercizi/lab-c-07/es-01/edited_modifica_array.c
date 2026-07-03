#include <stdio.h>
#include <stdbool.h>

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

void stampa_array(const int* pA, const size_t lenA);

size_t copia_nonnegativi(const int* pA, const size_t lenA, 
                         int* pB, const size_t nmaxB);

size_t filtra_maggiori_di(int* pA, size_t lenA, int maggiori_di);

size_t duplica_pari(const int* pA, const size_t lenA, 
                    int* pB, const size_t nmaxB);

bool array_equal(const int* pA, const size_t lenA,
                 const int* pB, const size_t lenB);

//------------------------------------------------------------------

#define NBUF1  10
#define NBUF2  3
#define NBUF3  20

int main(void) {
    int arr1[NBUF1] = { -73, 10, 77, -10, -41, 0, -58, 41, 78, 97 };
    size_t len1 = NBUF1;

    stampa_array(arr1, len1);

    // copia gli elementi nonnegativi
    int arr2[NBUF1];
    size_t len2 = copia_nonnegativi(arr1, len1, arr2, NBUF1);
    puts("Copia non-negativi:");
    stampa_array(arr2, len2);
    puts("10 77 0 41 78 97    [atteso]\n");

    // copia gli elementi nonnegativi in un buffer più piccolo
    int arr3[NBUF2];
    size_t len3 = copia_nonnegativi(arr1, len1, arr3, NBUF2);
    puts("Copia non-negativi (con buffer di dimensione insufficiente):");
    stampa_array(arr3, len3);
    puts("10 77 0             [atteso]\n");

    // filtra gli elementi maggiori di 10
    len1 = filtra_maggiori_di(arr1, len1, 10);
    puts("Filtra da arr1 elementi maggori di 10:");
    stampa_array(arr1, len1);
    puts("77 41 78 97         [atteso]\n");

    int arr4[NBUF1] = { 63, 99, 56, 98, 59, 51, 12, 57, 12, 10 };
    const size_t len4 = NBUF1;

    stampa_array(arr4, len4);
    puts("");

    // duplica gli elementi pari di arr4
    int arr5[NBUF3];
    size_t len5 = duplica_pari(arr4, len4, arr5, NBUF3);
    puts("Duplicazione pari:");
    stampa_array(arr5, len5);
    puts("63 99 56 56 98 98 59 51 12 12 57 12 12 10 10    [atteso]\n");

    // duplica gli elementi pari di arr4 in un buffer di dimensione insufficiente
    int arr6[NBUF1];
    size_t len6 = duplica_pari(arr4, len4, arr6, NBUF1);
    puts("Duplicazione pari (con buffer di dimensione insufficiente):");
    stampa_array(arr6, len6);
    puts("63 99 56 56 98 98 59 51 12 12               [atteso]\n");


#   define LENRISP1  4
    int risp1[LENRISP1] = { 77, 41, 78, 97 };
    puts("Confronti tra array:");
    printf("%d  [atteso: 1]\n", array_equal(arr1, len1, risp1, LENRISP1));
    printf("%d  [atteso: 0]\n", array_equal(arr1, len1, arr2, len2));
    int arr7[LENRISP1] = { 77, 41, 0, 97 };
    printf("%d  [atteso: 0]\n", array_equal(arr1, len1, arr7, LENRISP1));
}

//------------------------------------------------------------------

// stampa i lenA elementi dell'array *pA
void stampa_array(const int* pA, const size_t lenA) 
{
    for (size_t i=0; i<lenA; i++) {
        printf("%d ", pA[i]);
    }
    printf("\n");
}

//------------------------------------------------------------------

// Copia nell'array pB (di dimensione massima nmaxB) tutti gli
// elementi di pA (array di lunghezza lenA) che sono non-negativi
// (ciòe' sono maggiori o uguali a 0).
// Ritorna il numero di elementi copiati in pB
size_t copia_nonnegativi(const int* pA, const size_t lenA, 
                         int* pB, const size_t nmaxB)
{
	size_t c=0;

   	for ( size_t i=0; i < lenA; i++){
		if ( pA[i] >=0 && c < nmaxB){
			pB[c] = pA[i];
			c++;
		}
  	}

	return c;
}

//------------------------------------------------------------------

// Rimuovi in-place dall'array pA (di dimensione lenA) tutti
// gli elementi che non sono maggiori del valore passato come argomento.
// (quindi mantieni sono gli elementi maggiori_di)
// Ritorna il numero di elementi rimasti dopo la riduzione.
size_t filtra_maggiori_di(int* pA, size_t lenA, int maggiori_di) 
{
    size_t afterIndex=0;

    for (size_t i=0; i<lenA; i++){
	if ( pA[i] > maggiori_di ){
		pA[afterIndex] = pA[i];
		afterIndex++;
	}
    }
    return afterIndex;
}

//------------------------------------------------------------------

// Copia tutti gli elementi da pA (dimensione: lenA) nel buffer
// pB (dimensione massima: nmaxB), copiando due volte di seguito
// gli elementi che sono pari
// Ritorna il numero di elementi inseriti nell'array pB
size_t duplica_pari(const int* pA, const size_t lenA, 
                    int* pB, const size_t nmaxB)
{
	size_t cB=0;
	for(size_t i=0; i<lenA && cB < nmaxB; i++){
		pB[cB] = pA[i];
		if(cB+1 < nmaxB && pA[i] % 2==0){
			pB[cB+1]=pA[i];
			cB++;
		}
		cB++;
	}
    
	return cB;
}

//------------------------------------------------------------------

// Ritorna true se i due array pA e pB sono uguali, cioè se
// hanno lo stesso numero di elementi e se tutti gli elementi 
// di pA (dimensione lenA) sono uguali ai corrispondenti elementi 
// di pB (dimensione pB).
// Ritorna false altrimenti
bool array_equal(const int* pA, const size_t lenA,
                 const int* pB, const size_t lenB)
{
	bool isEqual = lenA == lenB;

	if(isEqual){
		int i =0;
		for(; i<lenB && pA[i] == pB[i]; i++){
		}

		if ( i<lenB ){
			isEqual = false;
		}
	}
	
    return isEqual;
}

//------------------------------------------------------------------
