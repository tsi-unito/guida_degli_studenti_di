#include "../include/semaphore.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sem.h>

/**
 * @brief Union structure used in the semctl operation
 *
 */
union semun {
    size_t val;
    struct semid_ds *buf;
    unsigned short *array;
};

/**
 * @brief Typedef to define more easily the pointier to a sembuf
 *
 */
typedef struct sembuf *SemOp;

// See *.h file for details
size_t sem_get(size_t sem_key, size_t val) {
    // allocating / connecting to the sem.
    int sem_id = semget(sem_key, 1, IPC_CREAT | SEM_PERMS);
    if (sem_id < 0) {
        fprintf(stderr, "[ERROR]: Could not create the semaphore group! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO] - Successfully created the semaphore!\n");

    // setting inital value
    union semun arg;
    arg.val = val;
    int res_ctl = semctl(sem_id, 0, SETVAL, arg);
    if (res_ctl == -1) {
        fprintf(
            stderr,
            "[ERROR]: Could not set the inital value of the semaphore! '%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }
    char buff_print[500];
    printf(
        "[INFO]: Successfully initialized the semaphore with the value "
        "assigned! Key = %ld, Id = %d, Value = %zu\n",
        sem_key, sem_id, val);

    return sem_id;
}

SemOp allocate_semop() {
    SemOp sem_op = (SemOp)malloc(sizeof(struct sembuf));
    if (sem_op == NULL) {
        fprintf(stderr, "[ERROR]: Could not allocate memory! '%s'\n",
                strerror(errno));
        return NULL;
    }

    return sem_op;
}

// See *.h file for details
void sem_signal(size_t sem_id) {
    SemOp sem_op = allocate_semop();
    if (sem_op == NULL) {
        return;
    }

    sem_op->sem_num = 0;
    sem_op->sem_op = 1;
    sem_op->sem_flg = 0;
    int res_op = semop(sem_id, sem_op, 1);
    if (res_op == -1) {
        fprintf(stderr, "[ERROR]: Could not perform a signal! '%s'\n",
                strerror(errno));
    }
    free(sem_op);
}

// See *.h file for details
void sem_wait(size_t sem_id) {
    SemOp sem_op = allocate_semop();
    if (sem_op == NULL) {
        return;
    }

    sem_op->sem_num = 0;
    sem_op->sem_op = -1;
    sem_op->sem_flg = 0;
    int res_op = semop(sem_id, sem_op, 1);
    if (res_op == -1) {
        fprintf(stderr, "[ERROR]: Could not perform a wait! '%s'\n",
                strerror(errno));
    }
    free(sem_op);
}

// See *.h file for details
void sem_del(size_t sem_id) {
    int res_del = semctl(sem_id, 1, IPC_RMID);
    if (res_del < 0) {
        fprintf(stderr, "[ERROR]: Could not delete the semaphore! '%s'\n",
                strerror(errno));
    }
}
