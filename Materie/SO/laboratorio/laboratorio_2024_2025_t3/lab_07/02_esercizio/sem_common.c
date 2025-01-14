#include "./sem_common.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void print_error(char *str) {
    if (str == NULL) {
        return;
    }

    fprintf(stderr, "[ERROR - PID: %d - %s]: %s '%s'\n", getpid(),
            strerrorname_np(errno), str, strerror(errno));
}

void print_info(char *str) {
    if (str == NULL) {
        return;
    }

    fprintf(stderr, "[INFO - PID: %d]: %s \n", getpid(), str);
}

SemOp allocate_semop() {
    SemOp sem_op = (SemOp)malloc(sizeof(struct sembuf));
    if (sem_op == NULL) {
        print_error("Could not allocate memory!");
        return NULL;
    }

    return sem_op;
}

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
        print_error("Could not perform a wait!");
    }
    free(sem_op);
}

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
        print_error("Could not perform a signal!");
    }
    free(sem_op);
}