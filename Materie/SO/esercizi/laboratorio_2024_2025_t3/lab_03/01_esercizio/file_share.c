#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUFFER_MAX_CHARS 200

int main() {
    char *shared_file_path = "./files/shared_file.txt";

    // apro la connessione al file:
    FILE *fp = fopen(shared_file_path, "r");

    if (fp == NULL) {
        fprintf(stderr,
                "Errore: non e' stato possibile aprire il file nel path %s\n",
                shared_file_path);
        return 1;
    }

    pid_t pid = fork();

    switch (pid) {
        // se =-1, allora e' andato in errore.
        case -1:
            fprintf(stderr,
                    "La fork del processo figlio e' andata in errore\n");
            exit(1);  // fine del processo
            break;
        // se =0, e' il figlio
        case 0:
            // il figlio modifica il file aperto
            // per modificarlo, ho bisogno di un nuovo puntatore a file
            FILE *children_fp = fopen(shared_file_path, "w");
            if (children_fp == NULL) {
                fprintf(stderr,
                        "Errore: il children non e' in grado di aprire il file "
                        "nel path '%s'",
                        shared_file_path);
                exit(1);
            }

            // poi scrivo qualcosa
            fprintf(children_fp,
                    "Children scrive a padre\nChildren scrive a padre passo.");

            // chiudo il file
            fclose(children_fp);
            exit(0);
            break;
        // se >0, allora e' il padre
        default:
            // il padre deve aspettare che il figlio finisca
            // conviene usare la wait() normale perche' c'e' solo un processo
            int children_stat = 1;
            pid_t wait_result = wait(&children_stat);

            if (wait_result == -1) {
                fprintf(stderr,
                        "La wait al processo figlio e' andata in errore!\n");
                exit(1);
            }

            if (children_stat != 0) {
                fprintf(stderr, "Il processo figlio e' andato in errore!");
                exit(1);
            }

            // una volta aspettato, deve andare a stampare a video il risultato
            // della scrittura del file
            while (!feof(fp)) {
                char current_row[BUFFER_MAX_CHARS];

                fgets(current_row, BUFFER_MAX_CHARS, fp);

                printf("%s", current_row);
            }
            break;
    }

    return 0;
}
