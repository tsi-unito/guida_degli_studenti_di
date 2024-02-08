#include <stdio.h>
#include <stdbool.h>

int main(void) {
	// Un singolo carattere è di tipo char
	char lettera = 'A';

	// Una stringa non e' altro che un array di elementi di tipo
	// char, ma con l'aggiunta che la sequenza dei caratteri
	// finisce con un terminatore: il carattere '\0'
	char testoA[] = "testo"; // il compilatore aggiunge per noi il terminatore

	// possiamo dichiarare la stringa come un'array invece che come
	// testo. Il risultato e' lo stesso (ma e' meno comodo).
	char testoB[] = { 't', 'e', 's', 't', 'o', '\0' };

	// stampiamo la stringa: usiamo lo specificatore di conversione %s
	printf("La stringa A e': %s\n", testoA);
	printf("La stringa B e': %s\n", testoB);

	// modifichiamo la stringa come array
	testoA[1] = 'E'; // cambia 'e' in 'E'
	testoA[4] = lettera; // cambia 'o' in 'A'
	testoA[3] = '\0'; // termina la stringa dopo 3 caratteri

	printf("La stringa A adesso e': %s\n", testoA);

	// leggi dall'input una sequenza di caratteri finchè non accade una
	// delle seguenti condizioni:
	//  - viene letto uno spazio, un ritorno a capo o il tab
	//  - vengono letto MAX_CH caratteri
	#define MAX_CH  20
	char testo_input[MAX_CH + 1]; // serve spazio per 20 caratteri + 1 per il terminatore

	// Provare a scrivere una parola singola, più parole, o una parola con più di 20 caratteri
	printf("\nInserisci una stringa di testo: ");
	scanf("%20s", testo_input);

	printf("Il testo letto da scanf e': %s\n\n", testo_input);

	// I caratteri sono in realtà numeri. 
	// Possiamo stamparne i singoli valori
	size_t pos = 0;
	bool end = false;
	while (!end) {
		// stampa tutti i caratteri, fino al terminatore (incluso)
		printf("  '%c' ha valore %d\n", testo_input[pos], testo_input[pos]);
		end = (testo_input[pos] == '\0');
		pos++;
	}

	// Le cifre e le lettere dell'alfabeto sono in sequenza.
	// Quindi possiamo fare operazioni semplici per passare
	// da numeri a cifre/lettere
	char cifra = '7';
	int num_cifra = cifra - '0'; // '0'-'0'==0,  '1'-'0'==1, etc.
	printf("La cifra '%c' ha valore ASCII %d, e rappresenta il numero %d.\n", 
		   cifra, cifra, num_cifra);

	for (size_t i=0; i<9; i++) {
		char cifra_i = '0' + i;
		printf("La cifra corrispondente al valore %zu e' il carattere '%c'.\n", i, cifra_i);
	}
}










