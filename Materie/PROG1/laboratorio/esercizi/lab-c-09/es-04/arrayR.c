#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// capovolgi gli elementi dell'array a[]
void capovolgi_array(const size_t aLen, int a[]);

// ritorna true se tutti gli elementi dell'array sono pari
bool tutti_pari(const size_t aLen, const int a[]);

// ritorna la somma di tutti gli elementi dispari dell'array
// ritorna 0 se non ci sono elementi
int somma_elem_dispari(const size_t aLen, const int a[]);

// ritorna l'indice dell'elemento più grande dell'array
// se l'array è vuoto, ritornare -1.
size_t indice_elem_massimo(const size_t aLen, const int a[]);

// raddoppia il valore di tutti gli elementi dell'array a[]
void raddoppia_elem(const size_t aLen, int a[]);

// copia in b[] tutti gli elementi di a[] che sono uguali o maggiori di val
// fino alla capacita' massima di b[] passata per puntatore come *p_bLen.
// Al ritorno, il valore puntato da p_bLen viene aggiornato per riflettere
// la dimensione effettiva di b[] (gli elementi effettivamente copiati)
// SUGGERIMENTO: aggiungere due parametri alla funzione ricorsiva:
// - un indice di ricorsione i che scorre l'array a[]
// - un indice j che avanza ogni volta che viene copiato un elemento in b[]
void copia_maggiori_di(const size_t aLen, const int a[],
                       size_t* p_bLen, int b[], const size_t val, int i, int j);

// modifica in-place l'array a[] mantenendo solo gli elementi compresi
// compresi nell'intervallo chiuso [min, max]. Al ritorno, il valore puntato 
// da p_aLen viene aggiornato al numero di elementi mantenuti in a[].
void filtro_intervallo(size_t* p_aLen, int a[], 
                       const int min, const int max);

//------------------------------------------------------------------

void stampa_array(const size_t aLen, int a[]) {
    for (size_t i=0; i<aLen; i++) {
        printf("%d ", a[i]);
    }
    puts("");
}

//------------------------------------------------------------------


int main(void) {
    #define A1_LEN 9
    int a1[A1_LEN] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

    #define A2_LEN 5
    int a2[A2_LEN] = { 16, 8, 24, 22, 6 };

    #define A3_LEN 13
    int a3[A3_LEN] = { 61, 39, 10, 70, 3, 88, 35, 49, 71, 89, 90, 6, 73 };

    puts("Unit test per capovolgi_array:");
    stampa_array(A1_LEN, a1);
    capovolgi_array(A1_LEN, a1);
    stampa_array(A1_LEN, a1);

    puts("\nUnit test per capovolgi_array:");
    printf("tutti_pari(a1) = %d [atteso: 0]\n", tutti_pari(A1_LEN, a1));
    printf("tutti_pari(a2) = %d [atteso: 1]\n", tutti_pari(A2_LEN, a2));

    puts("\nUnit test per somma elementi dispari:");
    printf("somma_elem_dispari(a1) = %3d [atteso:  16]\n", somma_elem_dispari(A1_LEN, a1));
    printf("somma_elem_dispari(a2) = %3d [atteso:   0]\n", somma_elem_dispari(A2_LEN, a2));
    printf("somma_elem_dispari(a3) = %3d [atteso: 420]\n", somma_elem_dispari(A3_LEN, a3));

    #define MARI_LEN  10
    const char* nomi_mari[MARI_LEN] = {
        "Oceano Atlantico", "Golfo del Messico", "Mar Mediterraneo", 
        "Oceano Artico", "Mar dei Caraibi", "Mare di Bering", 
        "Mar del Giappone", "Oceano Indiano", "Oceano Pacifico", 
        "Mar cinese meridionale", 
    };
    const int profondita_media[MARI_LEN] = { 
        3926, 1486, 1429, 1205, 2647, 
        1547, 1350, 3963, 4028, 1652, 
    };

    puts("\nUnit test per indice_elem_massimo:");
    int a_empty[] = {};
    size_t i_max = indice_elem_massimo(0, a_empty);
    printf("indice_elem_massimo(a_empty) = %3d [atteso:  -1]\n", (int)i_max);

    i_max = indice_elem_massimo(MARI_LEN, profondita_media);
    printf("indice_elem_massimo(profondica_media) = %3d [atteso:  -1]\n", (int)i_max);
    printf("Maggior profondita' media: '%s' con %d metri.\n",
           nomi_mari[i_max], profondita_media[i_max]);

    puts("\nUnit test per raddoppio elementi:");
    raddoppia_elem(A2_LEN, a2);
    stampa_array(A2_LEN, a2);
    printf("32 16 48 44 12  [atteso]\n");

    puts("\nUnit test per copia elementi:");
    #define BUF1_LEN 10
    int buf1[BUF1_LEN];
    size_t buf1_len = 2;
    copia_maggiori_di(A2_LEN, a2, &buf1_len, buf1, 20);
    stampa_array(buf1_len, buf1);
    printf("32 48    [atteso]\n");

    buf1_len = 8;
    copia_maggiori_di(A2_LEN, a2, &buf1_len, buf1, 20);
    stampa_array(buf1_len, buf1);
    printf("32 48 44 [atteso]\n");

    puts("\nUnit test per filtro intervallo:");
    size_t a3Len = A3_LEN;
    filtro_intervallo(&a3Len, a3, 10, 71);
    stampa_array(a3Len, a3);
    printf("61 39 10 70 35 49 71  [atteso]\n");
}

//------------------------------------------------------------------

// COMPLETARE

//------------------------------------------------------------------


// capovolgi gli elementi dell'array a[]
void capovolgi_array(const size_t aLen, int a[]){
	if(aLen-1 == 0){
		return; // niente da fare
	}

	//printf("\t--------------------- LEN - %zu -------------------\n", aLen);
	//printf("\t--------------------- Array in esame: -------------------\n");
	//stampa_array(aLen, a);
	//printf("\t---------------------------------------------------\n\n");

	int temp = a[aLen-1];
	//printf("\t\ttemp = a[%zu-1]; temp = %d\n",aLen, temp);
	//printf("\t\ta[%zu-1] = %d\n",aLen, *a);
	a[aLen-1] = *a;
	//printf("\t\t*a (%d)= %d;\n",*a, temp);
	*a = temp;
	//printf("\t\t\t*a = %d\n",*a);
	//printf("\t\t\ta[%zu-1] = %d\n", aLen, a[aLen-1]);

	//printf("\t---------------------------------------------------\n\n");

	
	capovolgi_array(aLen-2, a+1);
}

// ritorna true se tutti gli elementi dell'array sono pari
bool tutti_pari(const size_t aLen, const int a[]){
	if(*a==a[aLen-1]){
		return a[aLen-1] % 2 ==0;
	}

	return (a[aLen-1] % 2 ==0) && tutti_pari(aLen, a+1);
}

// ritorna la somma di tutti gli elementi dispari dell'array
// ritorna 0 se non ci sono elementi
int somma_elem_dispari(const size_t aLen, const int a[]){
	if(aLen == 0){
		return 0;
	}

	int cur = a[aLen-1];
	return ((cur %2 !=0)? cur : 0) + somma_elem_dispari(aLen-1, a);
}

// ritorna l'indice dell'elemento più grande dell'array
// se l'array è vuoto, ritornare -1.
size_t indice_elem_massimo(const size_t aLen, const int a[]){
	if(aLen == 0){
		return -1;
	}

	int curr = a[aLen-1];

	int recurs = indice_elem_massimo(aLen-1, a);

	return curr > recurs ? aLen-1 : aLen-2;
}

// raddoppia il valore di tutti gli elementi dell'array a[]
void raddoppia_elem(const size_t aLen, int a[]){
	if(aLen == 0){
		return;
	}

	a[aLen-1] = a[aLen-1]*2;
	raddoppia_elem(aLen-1, a);
}

// copia in b[] tutti gli elementi di a[] che sono uguali o maggiori di val
// fino alla capacita' massima di b[] passata per puntatore come *p_bLen.
// Al ritorno, il valore puntato da p_bLen viene aggiornato per riflettere
// la dimensione effettiva di b[] (gli elementi effettivamente copiati)
// SUGGERIMENTO: aggiungere due parametri alla funzione ricorsiva:
// - un indice di ricorsione i che scorre l'array a[]
// - un indice j che avanza ogni volta che viene copiato un elemento in b[]
void copia_maggiori_di(const size_t aLen, const int a[],
                       size_t* p_bLen, int b[], const size_t val, int i, int j){

	if(aLen == 0){
		return;
	}

	int currA = a[aLen-1];

	if(a < val){
		// va avanti
		copia_maggiori_di(aLen+1, a, p_bLen, b,val);
	}

	// a>= val: va copiato
	
}

// modifica in-place l'array a[] mantenendo solo gli elementi compresi
// compresi nell'intervallo chiuso [min, max]. Al ritorno, il valore puntato 
// da p_aLen viene aggiornato al numero di elementi mantenuti in a[].
void filtro_intervallo(size_t* p_aLen, int a[], 
                       const int min, const int max){

}

