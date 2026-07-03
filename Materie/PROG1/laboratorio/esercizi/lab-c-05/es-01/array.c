#include <stdio.h>

#define N 5

int main(void) {
    int arr[N]; // array di 5 elementi interi

    // inizializza a 0 gli elementi dell'array arr[]
    for (size_t i = 0; i < N; i++) {
        arr[i] = 0; // imposta a 0 il valore dell'elemento i-esimo
    }

    // verifica che gli elementi di arr[] siano tutti a zero
    for (size_t i = 0; i < N; i++) {
        printf("arr[%zu] = %d\n", i, arr[i]);
    }
    puts("");

    // Array inizializzato con lista di inizializzazione (initializer list)
    int values[N] = { 23, 56, 12, -6, 31 };

    // Altro array pre-inizializzato.
    int values2[N] = { 100 }; // i rimanenti N-1 valori sono inizializzati a 0

    // Se viene fornita una lista di elementi di inizializzazione,
    // non serve specificare la dimensione
    int values3[] = { 0, 1, 2, 3 }; // values3 ha 4 elementi

    // stampa i primi 2 elementi di values3
    printf("%d %d\n", values3[0], values3[1]);

    // Assegna ad ogni elemento di arr la somma degli elementi di 
    // values e values2 nelle medesime posizioni
    for (size_t i = 0; i < N; i++) {
        arr[i] = values[i] + values2[i];
    }

    // stampa gli elementi di arr[] in forma tabulata
    puts("Indice  Valore");
    for (size_t i = 0; i < N; i++) {
        printf("%6zu %7d\n", i, arr[i]);
    }

    // chiedi all'utente una dimensione sz per un nuovo array
    size_t sz;
    printf("Scrivi la dimensione: ");
    scanf("%zu", &sz);

    // dichiara un array di dimensione sz (Variable-Length Array)
    int inseriti[sz];
    // chiedi all'utente di scrivere sz interi da memorizzare in un array
    for (size_t i = 0; i < sz; i++) {
        printf("Scrivi il valore %zu-esimo: ", i);
        scanf("%d", &inseriti[i]);
    }
    // ristampa gli elementi appena inseriti
    puts("\n\nIndice  Valore");
    for (size_t i = 0; i < sz; i++) {
        printf("%6zu %7d\n", i, inseriti[i]);
    }


    // ESERCIZIO: calcola e stampa la somma degli elementi dell'array inseriti[]
    
    int somma = 0;
    for (int i = 0; i < sz; i++){
	somma += inseriti[i];
    }

    printf("la somma dei numeri e' %d", somma);
}








