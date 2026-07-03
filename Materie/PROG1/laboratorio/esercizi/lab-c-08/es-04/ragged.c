#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <limits.h>

#define ROWS  5
#define COLS  9

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// stampa la matrice ragged in ingresso
void stampa_matrice(const int mat[ROWS][COLS],
                    const size_t rags[ROWS]);

// ritorna la somma di tutti gli elementi sulla riga r della 
// matrice ragged mat.
int somma_righe(const int mat[ROWS][COLS],
                const size_t rags[ROWS],
                const size_t r);

// ritorna il numero di elementi inizializzati nella colonna c
// della matrice ragged in input.
size_t conta_elem_colonna(const int mat[ROWS][COLS],
                          const size_t rags[ROWS],
                          const size_t c);

// calcola il prodotto degli elementi inizializzati sulla colonna c
// della matrice ragged in input. Se la colonna è vuota, ritorna
// il valore 1.
int prod_colonna(const int mat[ROWS][COLS],
                 const size_t rags[ROWS],
                 const size_t c);

// ritorna il minimo elemento della colonna c.
// ritorna il massimo intero rappresentabile con un int 
// se la colonna non ha elementi.
int min_colonna(const int mat[ROWS][COLS],
                const size_t rags[ROWS],
                const size_t c);

// per ciascuna riga r della matrice ragged in ingresso,
// se esiste un numero primo lungo la riga r, allora sostituisci
// tutti gli elementi inizializzati della riga r con il valore val
void sostituisci_se_esiste_primo(int mat[ROWS][COLS],
                                 const size_t rags[ROWS],
                                 const int val);

bool is_primo(size_t num){
	bool result = num>=2;

	if (result){
		bool existsComposite = false;
		for(size_t i=2; i<floor(sqrt(num)) && !existsComposite; i++){
			existsComposite = num%i==0;
		}

		result = !existsComposite;
	}

	return result;
}


//------------------------------------------------------------------

int main(void) {
    // matrice con righe frastagliate (ragged)
    int mat[ROWS][COLS] = { 
        {4,4,6}, 
        {3,7,8,9,1},
        {},
        {0,1}, 
        {0,3,9,1,2,6,9,3} 
    };
    // lunghezza di ciascuna riga
    size_t rags[ROWS] = {
        3, 5, 0, 2, 8
    };

    // stampa la matrice
    stampa_matrice(mat, rags);
    puts("");

    // calcola la somma delle righe
    const int rispSomma[ROWS] = { 14, 28, 0, 1, 33 };
    for (size_t r=0; r<ROWS; r++) {
        printf("riga %zu: elementi=%zu  somma=%-2d  (atteso %-2d)\n", 
               r, rags[r], somma_righe(mat, rags, r), rispSomma[r]);
    }
    puts("");

    // calcola i prodotti ed i minimi di ciascuna colonna
    const int rispProd[COLS] = { 0, 84, 432, 9, 2, 6, 9, 3, 1 };
    const int rispMin[COLS] = { 0, 1, 6, 1, 1, 6, 9, 3, INT_MAX };
    for (size_t c=0; c<COLS; c++) {
        printf("colonna %zu: elementi=%zu  ", 
               c, conta_elem_colonna(mat, rags, c));

        printf("prodotto=%-3d  (atteso %-3d)  min=%-2d  (atteso %-2d)\n",
               prod_colonna(mat, rags, c), rispProd[c],
               min_colonna(mat, rags, c), rispMin[c]);
    }
    puts("");

    // sostituisci gli elementi di tutte le righe che 
    // contengono un numero primo
    sostituisci_se_esiste_primo(mat, rags, 100);
    stampa_matrice(mat, rags);
}

//------------------------------------------------------------------

// stampa la matrice ragged in ingresso
void stampa_matrice(const int mat[ROWS][COLS],
                    const size_t rags[ROWS])
{
    for (size_t r=0; r<ROWS; r++) {
        printf("[ ");
        for (size_t c=0; c<rags[r]; c++) {
            printf("%d ", mat[r][c]);
        }
        puts("]");
    }
}

//------------------------------------------------------------------

bool verifica_primalita(const int n) {
    bool primo = true;

    if (n < 2) {
        primo = false;
    }
    else if (n == 2) {
        primo = true;
    } 
    else if (n % 2 == 0) {
        primo = false;
    }
    else {
        int i = 3;
        while (i <= sqrt(n) && primo) {
            if (n % i == 0) {
                primo = false;
            }
            i += 2;
        }
    }

    return primo;
}

//------------------------------------------------------------------

// COMPLETARE

//------------------------------------------------------------------


// ritorna la somma di tutti gli elementi sulla riga r della 
// matrice ragged mat.
int somma_righe(const int mat[ROWS][COLS],
                const size_t rags[ROWS],
                const size_t r){
	
	int somma = 0;

	for(size_t c=0; c<rags[r]; c++){
		somma += mat[r][c];
	}

	return somma;
}

// ritorna il numero di elementi inizializzati nella colonna c
// della matrice ragged in input.
size_t conta_elem_colonna(const int mat[ROWS][COLS],
                          const size_t rags[ROWS],
                          const size_t c){
	
	size_t count = 0;

	for(size_t i=0; i<ROWS; i++){
		if ( c < rags[i]){
			count++;
		}
	}

	return count;
}

// calcola il prodotto degli elementi inizializzati sulla colonna c
// della matrice ragged in input. Se la colonna è vuota, ritorna
// il valore 1.
int prod_colonna(const int mat[ROWS][COLS],
                 const size_t rags[ROWS],
                 const size_t c){
	size_t res = 1;

	for(size_t i=0; i<ROWS; i++){
		if ( c < rags[i]){
			res = res*mat[i][c];
		}
	}

	return res;

}


// ritorna il minimo elemento della colonna c.
// ritorna il massimo intero rappresentabile con un int 
// se la colonna non ha elementi.
int min_colonna(const int mat[ROWS][COLS],
                const size_t rags[ROWS],
                const size_t c){

	size_t res = -1;
	bool hasElements = false;

	for(size_t i=0; i<ROWS; i++){
		if ( c < rags[i] && mat[i][c] < res){
			res = mat[i][c];
			hasElements = true;
		}
	}

	if (!hasElements){
		res = INT_MAX;
	}

	return res;
}

// per ciascuna riga r della matrice ragged in ingresso,
// se esiste un numero primo lungo la riga r, allora sostituisci
// tutti gli elementi inizializzati della riga r con il valore val
void sostituisci_se_esiste_primo(int mat[ROWS][COLS],
                                 const size_t rags[ROWS],
                                 const int val){
	for(size_t r=0; r<ROWS; r++){
		bool hasPrime=false;

		for(size_t c=0; c<COLS && c<rags[r] && !hasPrime; c++){
			hasPrime = verifica_primalita(mat[r][c]);
		}


		for(size_t c=0; c<COLS && c<rags[r] && hasPrime; c++){
			mat[r][c]=val;
		}
	}

}
