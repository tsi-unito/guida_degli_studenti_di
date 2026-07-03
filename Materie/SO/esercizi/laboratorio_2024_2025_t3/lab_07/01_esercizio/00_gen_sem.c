#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// imports needed for the semaphores
#include <sys/sem.h>

#include "./sem_data.h"

int main() {
    // creating semaphore
    int sem_id = semget(SEM_KEY, 1, IPC_CREAT | IPC_EXCL | SEM_PERMS);

    if (sem_id == -1) {
        fprintf(stderr, "[ERROR - %s]: Could not create the semaphore! '%s'\n",
                strerrorname_np(errno), strerror(errno));
        exit(EXIT_FAILURE);
    }
    printf("[INFO]: Successfully created a new semaphore! Key = %d, ID = %d\n",
           SEM_KEY, sem_id);
    // NB: Semaphores start by 0

    // setting inital value
    // semun is a custom defined union by the programmer
    union semun arg;  // union type for argument
    unsigned short values[1] = {0};
    arg.array = values;  // initial value

    // setting semun to 0 to target all semaphores
    int res_ctl = semctl(sem_id, 0, SETALL, arg);
    if (res_ctl == -1) {
        fprintf(stderr,
                "[ERROR - %s]: Could not set the inital value of the "
                "semaphore! '%s'\n",
                strerrorname_np(errno), strerror(errno));
        exit(EXIT_FAILURE);
    }

    // getting the value of the semaphore
    res_ctl = semctl(sem_id, 0, GETVAL, NULL);
    if (res_ctl == -1) {
        fprintf(
            stderr,
            "[ERROR - %s]: Could not get the value of the semaphore! '%s'\n",
            strerrorname_np(errno), strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO]: Successfully set the value of %d in the semaphore!\n",
           res_ctl);

    return 0;
}