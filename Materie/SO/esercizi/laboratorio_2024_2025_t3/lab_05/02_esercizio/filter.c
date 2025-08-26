#include <ctype.h>
#include <stdio.h>
#include <unistd.h>
#define BUFF_MAX 200

int main() {
    // printf("[INFO - filter.c] program started!\n");
    sleep(5);
    char buffer[BUFF_MAX];
    size_t buff_len = 0;
    for (int char_read = getc(stdin); char_read != EOF && buff_len < BUFF_MAX;
         char_read = getc(stdin)) {
#ifdef DEBUG
        printf("[DEBUG - filter.c]: wrote int = %d or in char = %c\n",
               char_read, (char)char_read);
#endif
        buffer[buff_len] = char_read;
        buff_len++;
    }
    printf("\n");

    for (size_t i = 0; i < buff_len; i++) {
        printf("%c", isupper(buffer[i]) ? buffer[i] : toupper(buffer[i]));
    }

    if (buff_len > 0) {
        printf("\n");
    }

    printf("[INFO - filter.c] program ended successfully!\n");
    return 0;
}
