#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// semaphore related imports
#include <sys/sem.h>

#include "./sem_data.h"

int main() {
    // connecting to an existing semaphore
    int sem_id = semget(SEM_KEY, 1, SEM_PERMS);
    if (sem_id == -1) {
        fprintf(
            stderr,
            "[ERROR - %s]: Could not connect to the semaphore with key = %d! "
            "'%s'\n",
            strerrorname_np(errno), SEM_KEY, strerror(errno));
        exit(EXIT_FAILURE);
    }
    printf("[INFO]: Successfully connected to a semaphore! Key = %d, ID = %d\n",
           SEM_KEY, sem_id);

    // closing the semaphore
    int res_ctl = semctl(sem_id, 0, IPC_RMID, NULL);
    if (res_ctl == -1) {
        fprintf(
            stderr,
            "[ERROR - %s]: Could not close the semaphore with key = %d and id "
            "= %d! '%s'\n",
            strerrorname_np(errno), SEM_KEY, sem_id, strerror(errno));
        exit(EXIT_FAILURE);
    }
    printf(
        "[INFO]: Successfully closed the semaphore with key = %d and id = "
        "%d!\n",
        SEM_KEY, sem_id);

    return 0;
}