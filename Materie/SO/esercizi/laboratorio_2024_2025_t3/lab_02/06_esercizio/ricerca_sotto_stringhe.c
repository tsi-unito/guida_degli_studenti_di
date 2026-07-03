#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_MAX_CHARS 200
#define BUFFER_MAX_ROWS 50

char **find_str(FILE *file, char *str, int *count_found_rows);

int main() {
    FILE *file = fopen("./files/complexity.txt", "r");

    char **rows_found = NULL;
    int count_rows_found = 0;
    char *str = "computational";

    if (file == NULL) {
        printf("ERRORE: Il file e' stato letto in maniera errata");
    } else {
        rows_found = find_str(file, str, &count_rows_found);
    }

    if (count_rows_found > 0) {
        printf("corrorrenza della stringa %s trovata in %d righe\n", str,
               count_rows_found);
        for (int i = 0; i < count_rows_found; i++) {
            printf("[%d] = '%s'\n", i, rows_found[i]);
        }
    } else {
        printf("Nessuna occorrenza della stringa %s e' stata trovata", str);
    }

    return 0;
}

char **find_str(FILE *file, char *str, int *count_found_rows) {
    if (file == NULL || str == NULL) {
        return NULL;
    }

    *count_found_rows = 0;
    char **found_rows = (char **)malloc(sizeof(char *) * BUFFER_MAX_ROWS);

    while (!feof(file)) {
        char buffer[BUFFER_MAX_CHARS];

        char *current_row = fgets(buffer, sizeof(buffer), file);
        // sostituzione \n con \0

        if (current_row != NULL && strlen(current_row) > 0) {
            current_row[strlen(current_row) - 1] = '\0';
        }

        if (current_row != NULL) {
            char *found_str = strstr(current_row, str);

            if (found_str != NULL) {  // stringa trovata
                char *mem_row =
                    (char *)malloc(sizeof(char) * strlen(current_row));
                strcpy(mem_row, current_row);
                found_rows[*count_found_rows] = mem_row;
                (*count_found_rows)++;
            }
        }
    }

    if (*count_found_rows > 0) {
        found_rows =
            (char **)realloc(found_rows, sizeof(char *) * (*count_found_rows));
    }

    return found_rows;
}
