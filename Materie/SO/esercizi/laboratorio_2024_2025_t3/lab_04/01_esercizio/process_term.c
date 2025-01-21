#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/*
    CONSEGNA:
    Scrivere un programma in cui, al momento dell’esecuzione, il processo padre
    crea un processo figlio e lo uccide (quale segnale si usa? vedere la
    documentazione di signal sul manuale). Predisporre cicli di attesa e stampe
    in modo da potere verificare la correttezza del programma

*/

int main() {
    pid_t id_children = fork();

    switch (id_children) {
        case -1:  // error
            fprintf(stderr, "there was an error in the fork() operation.\n");
            break;
        case 0:  // children
            printf("[children]: waiting for signal.");
            while (1) {  // children must wait for signal
                pause();
            }
            break;
        default:  // father

            // parent tries to kill children
            printf("[parent]: killing children\n");
            short kill_res = kill(id_children, -23148);

            if (kill_res == 0) {
                printf(
                    "[parent]: the kill of the child process with id %u has "
                    "been "
                    "concluded successfully!\n",
                    id_children);
            } else {
                char *error = strerror(errno);
                fprintf(stderr,
                        "[parent]: the kill of the process has resulted in an "
                        "error!: '%s'\n",
                        error);
                return -1;
            }
            break;
    }
    printf("final print. Should be printed only once!\n");

    return 0;
}