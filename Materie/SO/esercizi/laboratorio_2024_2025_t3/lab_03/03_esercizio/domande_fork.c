#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

/**
 * (1) Quanti processi generiamo usando questo codice? => vedere sezione 1 del
 *codice sotto R: Ad ogni ciclo il numero di processi raddoppia quindi contando
 *il parent sarebbero 2^n (2) Quanti processi genera il seguente for? => vedere
 *SEZIONE 2 R: Stesso codice quasi, stesso numero di processi (3) Quanti
 *processi genera questo codice? => vedere SEZIONE 3 R: Il processo viene creato
 *ma ucciso subito, questo non gli da il tempo di crearne altri per cui il
 *numero rimane n quanti sono i cicli ovvero 3.
 * */

int main() {
    // SEZIONE 1

    pid_t parent_pid = getpid();
    pid_t *children_pids = (pid_t *)malloc(sizeof(pid_t) * ((pow(2, 3))-1));
    pid_t *pids = children_pids;

    // i = 0 => totale: 2
    // i = 1 => totale: 4
    // i = 2 => totale: 8
    for (int i = 0; i < 3; i++) {
        pid_t pid = fork();
        if (pid == 0) {
            printf("\tSEZIONE 1 => PID: %d\tParent PID: %d\n", getpid(),
                   getppid());
            *pids = getpid();
            pids++;
        }
    }

    if (getpid() == parent_pid) {
        while (wait(NULL) != -1) {
        }
    } else {
        exit(0);
    }

    printf("\nFINE SEZIONE 1\n\n");

    // SEZIONE 2
    // Idem come su perche' alla fine
    for (size_t i = 0; i < 3; i++) {
        pid_t pid = fork();
        if (pid == 0)
            printf("\tSEZIONE 2 => PID: %d\t sono un figlio\n", getpid());
        else
            printf("\tsono un padre\n");
    }

    if (getpid() == parent_pid) {
        while (wait(NULL) != -1) {
        }
    } else {
        exit(0);
    }

    printf("\nFINE SEZIONE 2\n\n");

    // SEZIONE 3
    for (size_t i = 0; i < 3; i++) {
        pid_t pid = fork();

        if (pid == 0) {
            printf("\tSEZIONE 3 => PID: %d\t sono un figlio\n", getpid());
            exit(0);
        } else {
            printf("\tSEZIONE 3 => PARENT\n");
        }
    }

    return 0;
}
