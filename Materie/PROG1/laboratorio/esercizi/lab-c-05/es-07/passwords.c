#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

// CONSEGNA:
// Scrivere un programma che legge dallo standard input una stringa ( con massimo 20 caratteri ).
// In seguto il programma determina delle proprieta' e stampa un sommario finale.
//
// Le proprieta' seguenti sono:
// P1 = Esiste almeno 1 carattere che e' una cifra
// P2 = tutti i caratteri sono lettere
// P3 = ci sono almeno 2 lettere maiuscole
// P4 = ci sono almeno 2 cifre consecutive
// P5 = gli ultimi 2 caratteri non sono segni di punteggiatura
//
// Il sommario stampato deve avere forma <password> P1 P2 P3 P4 P5
// in cui P1..P5 sono valori 1 se e' soddisfatta oppure 0 se non e' soddisfatta.

size_t getLastUsedIndex(char arr[], size_t arrSize);
short p1(char arr[], size_t arrSize);
short p2(char arr[], size_t arrSize);
short p3(char arr[], size_t arrSize);
short p4(char arr[], size_t arrSize);
short p5(char arr[], size_t arrSize);

int main(){
	size_t arrSize = 20;
	char arr[arrSize] ;

	//scanf("%s", arr);
	fgets(arr, arrSize + 1, stdin); // +1 for the newline character

	size_t len = strlen(arr);
	if(len > 0 && arr[len - 1] == '\n') {
    		arr[len - 1] = '\0';
	}

	printf("%s %d %d %d %d %d", arr, p1(arr,arrSize), p2(arr,arrSize), p3(arr,arrSize), p4(arr, arrSize), p5(arr, arrSize));

	return 0;
}

size_t getLastUsedIndex(char arr[], size_t arrSize){

	return strlen(arr);
}

short p1(char arr[], size_t arrSize){
	short res = 0;
	for (size_t i=0; i < getLastUsedIndex(arr, arrSize) && res == 0; i++){
		if ( isdigit(arr[i]) ) {
			res = 1;
		}
	}

	return res;
}

short p2(char arr[], size_t arrSize){
	short res = 1;
	
	size_t i = 0;

	for ( ; i<getLastUsedIndex(arr, arrSize) && isalpha(arr[i]); i++){
	}

	if ( i < getLastUsedIndex(arr, arrSize) ) { // esiste 1 carattere non alfabetico
		res = 0;
	}

	return res;
}

short p3(char arr[], size_t arrSize){
	short upperCount = 0;

	for (size_t i = 0; i<getLastUsedIndex(arr, arrSize) && upperCount<2; i++){
		if(isupper(arr[i])) {
			upperCount++;
		}
	}

	return upperCount >= 2 ? 1 : 0;
}

short p4(char arr[], size_t arrSize){
	bool isFirstDigit = false;
	bool isSecondDigit = false;

	for (size_t i=0; i<getLastUsedIndex(arr, arrSize) && (!isSecondDigit); i++){
		if (isdigit(arr[i]) && !isFirstDigit){
			isFirstDigit = true;
		} else if (isdigit(arr[i]) ) {
			isSecondDigit = true;
		} else {
			isFirstDigit = false;
			isSecondDigit = false;
		}
	}

	return isFirstDigit && isSecondDigit ? 1 : 0;
}

short p5(char arr[], size_t arrSize){
	short res = 0;
	size_t posEnd = getLastUsedIndex(arr,arrSize);
	//printf("POS : %d. CH: '%c' STRKL: '%lu'",posEnd, arr[posEnd], strlen(arr));

	if (posEnd==1){
		res = 0;
	}else {
		res = ispunct(arr[posEnd-1]) && ispunct(arr[posEnd-2]) ? 1 :0;
	}

	return res;
}
