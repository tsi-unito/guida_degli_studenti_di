#include <stdio.h>
#include <stdbool.h>

//------------------------------------------------------------------
// Prototipi delle funzioni (solo gli involucri)
//------------------------------------------------------------------

// funzione ricorsiva che legge n valori da standard input con scanf e li memorizza nell'array a[]
void leggi_array(const size_t aLen, int a[]);

// funzione ricorsiva che calcola la somma dei valori dell'array a[]
int somma_array(const size_t aLen, int a[]);

// funzione ricorsiva che prende in ingresso un array (aLen,a[]) ed un valore val, e ritorna true se val compare tra gli elementi di a[].
bool esiste_val_in_array(const size_t aLen, const int a[], int val);

// funzione ricorsiva che prende in ingresso un array (aLen,a[]) ed un array (bLen,b[]), e ritorna true se ogni elemento di a[] compare nell'array b[]
bool sottoinsieme(const size_t aLen, const int a[], 
			      const size_t bLen, const int b[]);

//------------------------------------------------------------------

int main(void) {
	size_t aLen, bLen;

	scanf("%zu", &aLen);
	int a[aLen];
	leggi_array(aLen, a);

	scanf("%zu", &bLen);
	int b[bLen];
	leggi_array(bLen, b);

	printf("%d %d\n", somma_array(aLen, a), somma_array(bLen, b));

	printf("%d %d\n", 
		   sottoinsieme(aLen, a, bLen, b),
		   sottoinsieme(bLen, b, aLen, a));
}

//------------------------------------------------------------------





//------------------------------------------------------------------

// COMPLETARE LE FUNZIONI RIMANENTI

// funzione ricorsiva che legge n valori da standard input con scanf e li memorizza nell'array a[]
void leggi_arrayR(const size_t aLen, int a[], size_t i) {
	// COMPLETARE
	
	if(i == aLen){
		return;
	}

	scanf("%d",&a[i]);
	leggi_arrayR(aLen,a,i+1);
}

void leggi_array(const size_t aLen, int a[]) { 
	leggi_arrayR(aLen, a, 0);
}

// funzione ricorsiva che calcola la somma dei valori dell'array a[]
int somma_array(const size_t aLen, int a[]){
	if( aLen ==0 ){
		return 0;
	}

	int curr = a[aLen-1];

	return curr + somma_array(aLen-1, a);
}

// funzione ricorsiva che prende in ingresso un array (aLen,a[]) ed un valore val, e ritorna true se val compare tra gli elementi di a[].
bool esiste_val_in_array(const size_t aLen, const int a[], int val){
	if(aLen == 0){
		return false;
	}

	int curr = a[aLen-1];

	return val == curr || esiste_val_in_array(aLen-1, a, val);
}

// funzione ricorsiva che prende in ingresso un array (aLen,a[]) ed un array (bLen,b[]), e ritorna true se ogni elemento di a[] compare nell'array b[]
bool sottoinsieme(const size_t aLen, const int a[], 
			      const size_t bLen, const int b[]){

	if(aLen ==0){
		return true;
	}

	int currA = a[aLen-1];

	return esiste_val_in_array(bLen, b, currA) && sottoinsieme(aLen-1, a,bLen, b);
}

