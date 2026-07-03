#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
    // creo figlio

    pid_t pid = fork();
    switch (pid) {
        // errore
        case -1:
            fprintf(stderr, "ERRORE: il comando fork(); e' andato in errore!");
            exit(1);
            break;
        // p. figlio
        case 0:
            exit(0);
            break;
        // p. padre
        default:
            printf("PARENT: now sleeping for 10s..\n");
            sleep(10);
            printf("PARENT: woke up, trying to kill son..");
            kill(pid, SIGKILL);

            // aspetto lo zombie
            int status = -1;
            wait(&status);
            printf("PARENT: waited for zombie process with stat = %d\n",
                   status);
            break;
    }

    return 0;
}
