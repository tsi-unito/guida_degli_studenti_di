#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// CONSEGNA:
// Scrivere un programma che legge dall'input una matrice rettangolare
// e stampa in output tutte le coppie di indici r,c tali per cui
// tutti i numeri sulla colonna c risultano non negativi e pari.
//
// INPUT: le dimensioni rows e cols, seguite da rows* cols interi che rappresentano le righe della matrice
// OUTPUT: le coppie di indici che soddisfano i criteri indicati (una coppia per riga)

void leggi_matrice(const size_t rows, const size_t cols,
				   int mat[rows][cols]) 
{
	for (size_t r=0; r<rows; r++) {
		for (size_t c=0; c<cols; c++) {
			scanf("%d", &mat[r][c]);
		}
	}
}

void print_rows_cols_pairs_nonneg(const size_t rows, const size_t cols, int mat[rows][cols]){
	for(size_t r=0; r<rows ; r++){
		bool isRowOk = true;
		
		for(size_t c=0; c<cols && isRowOk; c++){
			isRowOk=mat[r][c] >= 0 && mat[r][c]%2==0;	
		}

		if (isRowOk){

			for(size_t c=0; c<cols; c++){
				bool isColOk = true;
				for(size_t r2=0; r2<rows && isColOk; r2++){
					isColOk=mat[r2][c] >= 0 && mat[r][c]%2==0;	
				}

				if(isColOk){
					printf("%zu %zu\n", r, c);
				}
			}
		}
	}
}

int main(void) {
	// leggi le dimensioni della matrice rettangolare
	size_t rows, cols;
	scanf("%zu %zu", &rows, &cols);

	// riserva la memoria per la matrice di dimensione rows * cols
	int mat[rows][cols];
	
	// leggi i dati
	leggi_matrice(rows, cols, mat);

	// COMPLETARE
	print_rows_cols_pairs_nonneg(rows, cols, mat);
}
