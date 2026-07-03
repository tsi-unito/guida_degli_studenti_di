#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

/**
 * Crea N processi quanta e' la dimensione di wait_times.
 * Per ogni processo i, aspetta il tempo in secondi di wait_times[i].
 * @param wait_times - L'array di tempo specificato
 * */
void children_wait(int *wait_times, int size);

int main(int argc, char **argv) {
    if (argc <= 1) {
        fprintf(stderr, "ERRORE: Almeno 1 parametro deve essere fornito!\n");
        return -1;
    }

    int wait_sec[argc - 1];

    printf("reading params...\n");
    for (int i = 0; i < argc - 1; i++) {
        wait_sec[i] = (int)*(argv[i + 1]) - 48;
        printf("\tparam %d = %d\n", i, wait_sec[i]);
    }

    children_wait(wait_sec, argc - 1);

    return 0;
}

void children_wait(int *wait_times, int size) {
    for (int i = 0; i < size; i++) {
        switch (fork()) {
            // errore
            case -1:
                fprintf(stderr, "ERRORE: la funzione fork(); e' fallita!\n");
                exit(1);
                break;
            // p. figlio
            case 0:
                printf("\tCHILDREN with PID[%d] will wait for %d seconds\n",
                       getpid(), wait_times[i]);
                sleep(wait_times[i]);
                exit(0);
                break;
            // p. padre
            default:
                // not here
                break;
        }
    }

    // il parent aspetta tutti gli altri
    pid_t term_pid = wait(NULL);

    while (term_pid != -1) {
        printf("\tPARENT: The process with id %d has done!\n", term_pid);
        term_pid = wait(NULL);
    }

    printf("\n\nPARENT has waited for children to complete!\n");
}
