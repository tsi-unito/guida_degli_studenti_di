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

    // signaling
    // 1. allocating sembuf pointier
    SemOp sem_op = (SemOp)malloc(sizeof(struct sembuf));
    if (sem_op == NULL) {
        fprintf(stderr,
                "[ERROR - %s]: Could not allocate memory for sem op. struct! "
                "'%s'\n",
                strerrorname_np(errno), strerror(errno));
        exit(EXIT_FAILURE);
    }

    // 2. setting operation details
    sem_op->sem_num = 0;  // targeting the first semaphore
    sem_op->sem_op = 1;   // adding 1 for signal
    sem_op->sem_flg = 0;  // no additional flags

    // 3. sending operation to semaphore
    int op_res = semop(sem_id, sem_op, 1);  // 1 = dimension of sem_op array

    if (op_res == -1) {
        fprintf(stderr, "[ERROR - %s]: Could not signal 1 in semaphore! '%s'\n",
                strerrorname_np(errno), strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO]: Successfully signaled 1 to a semaphore!\n");

    return 0;
}