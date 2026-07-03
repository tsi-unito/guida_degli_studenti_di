#include <stdio.h>
#include <stdbool.h>

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// ritorna il massimo comun divisore (MCD) tra a e b usando 
// l'algoritmo di Euclide per l'MCD:
//   MCD(a, 0) = a
//   MCD(a, b) = MCD(b, a mod b)
int mcd_euclideR(int a, int b);

// somma tutti gli interi dispari tra 0 ed n (incluso)
int somma_dispariR(int n);

// ritorna il prodotto di tutti gli interi multipli di k
// partendo da n e scendendo fino a n_min (incluso)
// se non ci sono multipli di k nell'intervallo, allora ritorna 1.
int prodotto_multipli_in_intervalloR(int n_min, int n, int k);

// ritorna true se esiste un numero tra n_min ed n (estremi inclusi)
// che e' un divisore di k. Ritorna false altrimenti.
bool esiste_divisore_in_intervalloR(int n_min, int n, int k);

//------------------------------------------------------------------

int main(void) {
    // Unit test per algoritmo di Euclide
    puts("MCD con Euclide:");
    #define NUM 6
    const int a_vals[NUM]   = { 2, 8, 7, 12, 81, 2172 };
    const int b_vals[NUM]   = { 0, 4, 5,  7, 15, 3288 };
    const int mcd_vals[NUM] = { 2, 4, 1,  1,  3,   12 };
    for (size_t i=0; i<NUM; i++) {
        int mcd = mcd_euclideR(a_vals[i], b_vals[i]);
        printf("[%s] MCD(%d, %d) = %d\n",
               (mcd==mcd_vals[i] ? "Ok" : "NO"), 
               a_vals[i], b_vals[i], mcd);
    }

    // Unit test per somma dispari
    puts("\nSomma dispari:");
    const int somma_vals[NUM] = { 0, 4, 9, 16, 64, 2702736 };
    for (size_t i=0; i<NUM; i++) {
        int somma = somma_dispariR(b_vals[i]);
        printf("[%s] somma_dispari(%d) = %d\n", 
               (somma==somma_vals[i] ? "Ok" : "NO"), 
               b_vals[i], somma);
    }

    // Unit test per prodotto multipli
    puts("\nProdotto multipli:");
    const int n_vals[NUM]  = { 2, 7, 31, 15, 179, 2672 };
    const int k_vals[NUM]  = { 2, 4, 10,  1,  37,  231 };
    const int pm_vals[NUM] = { 2, 1, 6000, 32760, 16428, 5869710 };
    for (size_t i=0; i<NUM; i++) {
        int prod_mult = prodotto_multipli_in_intervalloR(a_vals[i], n_vals[i], k_vals[i]);
        printf("[%s] prodotto_multipli_in_intervallo(%d, %d, %d) = %d\n", 
               (prod_mult==pm_vals[i] ? "Ok" : "NO"),
               a_vals[i], n_vals[i], k_vals[i], prod_mult);
    }

    // Unit test per esiste divisore
    puts("\nEsiste divisore:");
    const int div_vals[NUM]  = { 2, 1, 25,  9,  242,  42 };
    const bool ed_vals[NUM] = { true, false, true, false, true, false };
    for (size_t i=0; i<NUM; i++) {
        bool esiste = esiste_divisore_in_intervalloR(a_vals[i], n_vals[i], div_vals[i]);
        printf("[%s] esiste_divisore_in_intervallo(%d, %d, %d) = %d\n", 
               (esiste==ed_vals[i] ? "Ok" : "NO"),
               a_vals[i], n_vals[i], div_vals[i], esiste);
    }   
}

//------------------------------------------------------------------

int mcd_euclideR(int a, int b) {
    // caso base: MCD(a, 0) = a
    if(b==0){
	return a;
    }

	return mcd_euclideR(b, a % b);
    // passo induttivo: MCD(a, b) = MCD(b, a mod b)
}

// somma tutti gli interi dispari tra 0 ed n (incluso)
int somma_dispariR(int n){
	if(n<=0){
		return 0;
	}

	return (n%2!=0 ? n : 0) + somma_dispariR(n-1);
}

// ritorna il prodotto di tutti gli interi multipli di k
// partendo da n e scendendo fino a n_min (incluso)
// se non ci sono multipli di k nell'intervallo, allora ritorna 1.
int prodotto_multipli_in_intervalloR(int n_min, int n, int k){
	if(n == n_min){
		return n_min % k == 0? n : 1;
	}

	return (n_min % k ==0? n : 1) * prodotto_multipli_in_intervalloR(n_min, n-1, k);
}

// ritorna true se esiste un numero tra n_min ed n (estremi inclusi)
// che e' un divisore di k. Ritorna false altrimenti.
bool esiste_divisore_in_intervalloR(int n_min, int n, int k){
	if (k==1){
		printf("n.min = %d | n = %d | k = %d\n", n_min, n, k);
	}
	if(n_min >= n){
		return k%n==0;
	}

	return k % n == 0 || esiste_divisore_in_intervalloR(n_min, n-1, k);
}

