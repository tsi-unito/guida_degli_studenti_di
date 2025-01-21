#include <stdio.h>
#include <stdlib.h>
#define BUFFER_MAX_CHARACTERS 150
#define BUFFER_MAX_ARRAY 150

char **leggi_lista(FILE *file_in, int *nof_elements);
void stampa_lista(char **mio_ar, int n_elems);

int main() {
    FILE *file = fopen("./files/lista_nomi.txt", "r");
    int file_rows = 0;

    char **names = leggi_lista(file, &file_rows);
    stampa_lista(names, file_rows);

    return 0;
}

//
// PRE: il numero di righe del file corrisponde
//      esattamente al numero degli elementi
//
char **leggi_lista(FILE *file_in, int *nof_elements) {
    // alloco l'array con la dimensione massima predefinita
    *nof_elements = 0;
    char **names = (char **)malloc(sizeof(char *) * BUFFER_MAX_ARRAY);

    // leggo le righe e aggiungo i nomi
    while (!feof(file_in)) {
        char *row = (char *)malloc(sizeof(char) * BUFFER_MAX_CHARACTERS);
        fscanf(file_in, "%[^\n]\n", row);

        if (row != NULL) {
            names[*nof_elements] = row;
            (*nof_elements)++;
        }
    }

    // riduco le righe a quante effettivamente ne servono
    if (*nof_elements > 0) {
        names = (char **)realloc(names, sizeof(char *) * (*nof_elements));
    } else {
        free(names);
        names = NULL;
    }

    return names;
}

//
void stampa_lista(char **mio_ar, int n_elems) {
    for (int i = 0; i < n_elems; i++) {
        printf("%s\n", mio_ar[i]);
    }
}
