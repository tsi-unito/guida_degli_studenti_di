#include <stdio.h>
#include <stlib.h>


// CONSEGNA:
// Scrivere una funzione trasforma_array  che prende in input un array pA di dimensione lenA ed un buffer pB di dimensione massima nmaxB,
// che copia gli elementi di pA in pB seguendo queste regole:
// se l'elemento di pA è minore di 10 viene scartato;
// se l'elemento di pA da copiare è pari e multiplo di 4, allora viene copiato 4 volte in pB;
// se l'elemento di pA da copiare è pari e non è multiplo di 4, allora viene copiato 2 volte in pB;
// se l'elemento di pA da copiare è dispari e multiplo di 3, allora viene scartato;
// altrimenti viene copiato una volta in pB.
// La funzione trasforma_array deve ritornare il numero di elementi inseriti in pB.

// copia in pB (dimensione massima nmaxB) tutti gli elementi 
// dell'array pA (dimensione lenA) seguendo le regole.
// ritorna il numero di elementi copiati
size_t trasforma_array(const int* pA, const size_t lenA,
					   int *pB, const size_t nmaxB)
{
	// COMPLETARE
	
	size_t cB=0;
	for(int i=0; i<lenA && cB<nmaxB; i++){
		if( pA[i] >=10 && (pA[i]%2==0 || pA[i]%3!=0)){
			if(pA[i] % 4 ==0){
				// Copiare 4 volte
				for(int j=0; j<4 && cB<nmaxB ;cB++){
					pB[cB]=pA[i];
					j++;
				}
			}else if(pA[i] %2 ==0){
				// Copiare 2 volte
				for(int j=0; j<2 && cB<nmaxB ;cB++){
					pB[cB]=pA[i];
					j++;
				}
			}else{
				// Copiare 1 volta
				pB[cB] = pA[i];
				cB++;
			}
		}
	}

	return cB;

}

//------------------------------------------------------------------
// stampa gli elementi dell'array *pA (di dimensione lenA)
void stampa_array(const int* pA, const size_t lenA) 
{
	printf("len=%zu: [ ", lenA);
	for (size_t i=0; i<lenA; i++) {
		printf("%d ", pA[i]);
	}
	printf("]\n");
}

//------------------------------------------------------------------
// leggi da standard input un array, riempi il buffer pA 
// e ritorna il numero di elementi inseriti nel buffer
size_t leggi_array(int* pA, const size_t nmaxA) 
{
	size_t n, lenA=0;
	scanf("%zu", &n); // numero di elementi da leggere
	for (size_t i=0; i<n; i++) {
		int elem;
		scanf("%d", &elem);
		if (lenA < nmaxA) {
			pA[lenA] = elem;
			lenA++;
		}
	}
	return lenA;
}

//------------------------------------------------------------------
#define NMAX  10
int main(void) {
    int A[NMAX], B[NMAX]; 
    size_t nA = leggi_array(A, NMAX); 
    size_t nB = trasforma_array(A, nA, B, NMAX); 
    stampa_array(B, nB);
}

//------------------------------------------------------------------

