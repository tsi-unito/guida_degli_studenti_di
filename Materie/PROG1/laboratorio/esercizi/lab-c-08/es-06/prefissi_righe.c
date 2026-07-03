#include <stdio.h>
#include <stdbool.h>

void leggi_matrice_irregolare(const size_t rows, const size_t cols,
				              int mat[rows][cols], size_t rags[rows]) 
{
	for (size_t r=0; r<rows; r++) {
	    scanf("%zu", &rags[r]);
		for (size_t c=0; c<rags[r]; c++) {
			scanf("%d", &mat[r][c]);
		}
	}
}


void print_prefixes(const size_t rags1[], const size_t r1, const size_t c1, const size_t rags2[], const size_t r2, const size_t c2,
		int mat1[r1][c1], int mat2[r2][c2]);

int main(void) {
	// leggi la matrice irregolare A
	size_t rowsA, colsA;
	scanf("%zu %zu", &rowsA, &colsA);
	int matA[rowsA][colsA];
	size_t ragsA[rowsA];
	leggi_matrice_irregolare(rowsA, colsA, matA, ragsA);

	// leggi la matrice irregolare B
	size_t rowsB, colsB;
	scanf("%zu %zu", &rowsB, &colsB);
	int matB[rowsB][colsB];
	size_t ragsB[rowsB];
	leggi_matrice_irregolare(rowsB, colsB, matB, ragsB);

	// COMPLETARE
	print_prefixes(ragsA, rowsA, colsA, ragsB, rowsB, colsB, matA, matB);
}

void print_prefixes(const size_t rags1[], const size_t r1, const size_t c1, const size_t rags2[], const size_t r2, const size_t c2,
		int mat1[r1][c1], int mat2[r2][c2]){
	for(size_t r=0; r<r1; r++){
			
		for(size_t z=0; z<r2; z++){
			bool isPrefix = true;
			for(size_t c=0; c<c1 && c <rags1[r] && c <rags2[z] && isPrefix; c++){
				isPrefix = mat1[r][c]==mat2[z][c];
			}

			if(isPrefix){
				printf("%zu %zu\n", r, z);
			}
		}

		
	}
}
