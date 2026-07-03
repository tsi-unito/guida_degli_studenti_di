#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
    printf("[exec] - Forking\n");

    switch (fork()) {
        // errore
        case -1:
            fprintf(stderr,
                    "[exec] - Errore: il comando fork(); e' fallito!\n");
            exit(1);
            break;
        // figlio
        case 0:
            printf("[exec] CHILDREN - Initializing program..\n");
            execlp("./stampa_persone.out", "stampa_persone.out", "mario",
                   "luigi", "pippo", NULL);
            // se siamo qui sotto, vuol dire che e' fallito!
            fprintf(stderr,
                    "[exec] CHILDREN - Errore: il processo figlio ha fallito "
                    "ad avviare il prog.!\n");
            exit(1);
            break;
        // padre
        default:
            int children_stat = 0;
            pid_t child_pid = wait(&children_stat);
            if (child_pid <= 0) {
                fprintf(
                    stderr,
                    "[exec] PARENT - Errore: il comando wait(); e' fallito!");
                exit(1);
            }

            if (children_stat != 0) {
                fprintf(stderr,
                        "[exec] PARENT - Errore: il processo children con PID "
                        "%d ha "
                        "terminato in errore!\n",
                        child_pid);
            }

            printf("[exec] PARENT - children terminated successfully\n");
            break;
    }

    return 0;
}
