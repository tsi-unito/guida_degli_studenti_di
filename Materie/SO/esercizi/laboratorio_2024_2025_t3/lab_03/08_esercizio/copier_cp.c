#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROW_BUFFER 800

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr,
                "[copier] - Errore: Sono necessari i due path dei file da "
                "copiare\n");
        exit(1);
    }

    FILE *first_fp = fopen(argv[1], "r");
    FILE *second_fp = fopen(argv[2], "w");

    if (first_fp == NULL || second_fp == NULL) {
        fprintf(stderr,
                "[copier] - Errore: Uno dei due file non e' stato aperto "
                "correttamente!\n");
        exit(1);
    }

    char buffer[ROW_BUFFER];
    char *current_line = fgets(buffer, ROW_BUFFER, first_fp);

    while (current_line != NULL) {
        // printf("\t[copier] - ROW: %s\n", current_line);
        fprintf(second_fp, "%s", current_line);

        current_line = fgets(buffer, ROW_BUFFER, first_fp);
    }

    printf("[copier] - Contenuto copiato con successo!\n");
    return 0;
}
