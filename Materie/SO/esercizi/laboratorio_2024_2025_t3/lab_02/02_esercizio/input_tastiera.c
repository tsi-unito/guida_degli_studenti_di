#include <stdio.h>
#include <stdlib.h>

#define BUFFER_MAX_CHARACTERS 500

char *parse_nome(char *stringa_completa);

int main() {
    printf("Inserisci nome nel formato <nome, cognome>: ");
    char buffer[BUFFER_MAX_CHARACTERS];
    char *full_name = fgets(buffer, sizeof(buffer), stdin);

    if (full_name == NULL) {
        printf("Non e' stato letto nessun nome");
    } else {
        printf("Before: '%s'\n", full_name);
        char *name = parse_nome(full_name);
        printf("After: '%s'\n", name);
    }

    return 0;
}

char *parse_nome(char *stringa_completa) {
    // leggo il nome fino a quando non trovo la virgola
    char *name = (char *)malloc(sizeof(char *));
    sscanf(stringa_completa, "%[^,]", name);
    sscanf(name, "%[^ ]", name);

    return name;
}