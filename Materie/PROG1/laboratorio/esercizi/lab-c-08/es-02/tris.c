#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Scrivere un programma che legge in input una matrice 3x3 di caratteri.
// I caratteri possibili sono tre: 'X', 'O' e '-',
// che rappresentano rispettivamente i simboli dei due giocatori, e la casella vuota.
// La matrice 3x3 rappresenta la griglia del gioco del tris.
// Un giocatore vince il gioco del tris se occupa una riga, una colonna o una delle due diagonali con il proprio simbolo.
// 
// Il programma deve:
// 
// stampare 'X' se nella griglia in input vince il giocatore 'X';
// stampare 'O' se nella griglia in input vince il giocatore 'O';
// stampare '-' se non vince ne 'X' ne 'O'.
// È garantito che la griglia in input abbia al massimo un giocatore che vince.
// 
// SUGGERIMENTO: leggere i caratteri che formano la griglia di gioco 3x3 usando scanf
// con stringa di formato " %c" (spazio seguito da %c).
//Questa stringa di formato viene interpretato da scanf come: prima scarta tutti gli spazi 
// e poi leggi il prossimo carattere (diverso da uno spazio).

#define ROWS 3
#define COLS 3

char get_winner(size_t rows, size_t cols, char mat[rows][cols]);
bool has_won(size_t rows, size_t cols, char mat[rows][cols], char player);

int main(){
	char mat[ROWS][COLS] = { {'-', '-','-'}, {'-', '-','-'}, {'-', '-','-'} };


	for(size_t i=0; i<ROWS; i++){
		for(size_t j=0; j<COLS; j++){
			scanf(" %s",&mat[i][j]);
		}
	}

	printf("%c",get_winner(ROWS, COLS, mat));
}

bool has_won(size_t rows, size_t cols, char mat[rows][cols], char player){
	bool hasWon = false;

	// Verticals
	for(int j=0; j<COLS && !hasWon; j++){
		bool samePiece = true;
		for(int i=0; i<ROWS && samePiece; i++){
			samePiece = mat[i][j] == player;
		}

		hasWon = samePiece;
	}

	// Horizontals
	for(int i=0; i<ROWS && !hasWon; i++){
		bool samePiece = true;
		for(int j=0; j<ROWS && samePiece; j++){
			samePiece = mat[i][j] == player;
		}

		hasWon = samePiece;
	}

	// Diagonal 1
	
	if(!hasWon){
		bool samePieces = true;
		for(int c=0; c<ROWS && samePieces; c++){
			samePieces = mat[c][c] == player;
		}

		hasWon = samePieces;
	}
	
	// Diagonal 2
	
	if(!hasWon){
		bool samePieces = true;
		
		for(int i=0; i<ROWS && samePieces; i++){
			samePieces = mat[i][ROWS-1-i] == player;
			//for(int j=COLS-1; j>=0 && samePieces; j--){
			//	samePieces = mat[i][j] == player;
			//} 
		}

		hasWon = samePieces;
	}
	
	

	return hasWon;
}

char get_winner(size_t rows, size_t cols, char mat[rows][cols]){
	// checking for X
	
	char winner = '-';
	
	if (has_won(rows, cols, mat,'X')){
		winner = 'X';
	} else if (has_won(rows, cols, mat,'O')){
		winner = 'O';
	}
	
	return winner;
}
