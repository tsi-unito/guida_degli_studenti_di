#include <stdio.h>

int main(int argc, char **args) {
    printf("[stampa_persone] initialized\n");
    if (argc <= 1) {
        fprintf(stderr,
                "Errore: il numero di parametri passati al programma deve "
                "essere >= 1\n");
        return 1;
    }

    printf("[stampa_persone] printing names\n");
    for (int i = 1; i < argc; i++) {
        printf("\t[stampa_persone] %d : '%s'\n", i, args[i]);
    }

    return 0;
}
