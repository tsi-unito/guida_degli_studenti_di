#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

/**
 * Crea N processi ed aspetta che tutti terminino.
 * Per ogni processo che termina, il padre ne stampa il PID.
 * @param proc_amount - Il numero di processi da creare
 * */
void crea_proc(size_t proc_amount);

int main() {
    crea_proc(3);
    return 0;
}

void crea_proc(size_t proc_amount) {
    pid_t parent_pid = getpid();

    for (size_t i = 0; i < proc_amount; i++) {
        switch (fork()) {
            // errore di fork
            case -1:
                fprintf(stderr,
                        "\n\tIl comando fork(); e' andato in errore!\n");
                exit(1);
                break;
            // codice per il processo figlio
            case 0:
                pid_t current_pid = getpid();

                // mi prendo l'ultima cifra come resto della divisione per 10
                size_t last_digit = current_pid % 10;
                printf("\tCHILDREN PID[%d]: now waiting for %ld seconds\n",
                       current_pid, last_digit);

                // dormo per l'ultima cifra
                sleep(last_digit);

                // esco per evitare di creare 2^n processi
                exit(0);
                break;
            default:
                // il parent dovra' aspettare i children, ma non qui
                break;
        }
    }

    pid_t children_pid = wait(NULL);

    while (children_pid != -1) {
        printf("\tPARENT: Has waited for CHILDREN with PID [%d] to die\n",
               children_pid);
        children_pid = wait(NULL);
    }
}
