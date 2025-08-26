#include <ctype.h>
#include <stdio.h>
#include <string.h>

#define BUFFER_MAX_CHARACTERS 200

void recur_to_up(char *in_str);
int main() {
    char buffer[BUFFER_MAX_CHARACTERS];

    printf("Inserisci il testo da trasformare in maiusc: ");
    char *read_chars = fgets(buffer, sizeof(buffer), stdin);
    // rimozione spazio finale
    if (strlen(read_chars) > 0) {
        read_chars[strlen(read_chars) - 1] = '\0';
    }

    printf("BEFORE: '%s'\n", read_chars);
    recur_to_up(read_chars);
    printf("AFTER: '%s'\n", read_chars);

    return 0;
}

void recur_to_up(char *in_str) {
    if (*in_str == '\0') {
        return;
    }

    *in_str = toupper(*in_str);

    recur_to_up(in_str + sizeof(char));
}