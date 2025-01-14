#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_MAX_CHARACTERS 250

void reverse_print_rec(char *str, size_t index);

int main() {
    printf("Inserisci una stringa da invertire: ");
    char buffer[BUFFER_MAX_CHARACTERS];
    char *inserted_text = fgets(buffer, sizeof(buffer), stdin);

    if (inserted_text == NULL || strlen(inserted_text) <= 0) {
        printf("ERRORE: la stringa inserita e' vuota");
    } else {  // rimuovo il ritorno a capo
        inserted_text[strlen(inserted_text) - 1] = '\0';
    }

    reverse_print_rec(inserted_text, 0);
    printf("\n");

    return 0;
}

void reverse_print_rec(char *str, size_t index) {
    if (str[index] == '\0') {
        return;
    }

    reverse_print_rec(str, index + 1);

    char current = toupper(str[index]);
    printf("%c", current);
}