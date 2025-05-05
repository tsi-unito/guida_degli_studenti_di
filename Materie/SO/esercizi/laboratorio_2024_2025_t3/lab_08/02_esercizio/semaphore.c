#include "./semaphore.h"

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

int sem_init(size_t sem_key, int value) {
    // allocating / connecting to the sem.
    int sem_id = semget(sem_key, 1, IPC_CREAT | SEM_PERMS);
    if (sem_id == -1) {
        print_error("Could not create the semaphore group!");
        return -1;
    }
    print_info("Successfully created the semaphore!");

    // setting inital value
    union semun arg;
    arg.val = value;
    int res_ctl = semctl(sem_id, 0, SETVAL, arg);
    if (res_ctl == -1) {
        print_error("Could not set the inital value of the semaphore!");
        return -1;
    }
    char buff_print[500];
    sprintf(
        buff_print,
        "Successfully initialized the semaphore with the value assigned! Key "
        "= %ld, Id = %d, Value = %d",
        sem_key, sem_id, value);
    print_info(buff_print);

    return sem_id;
}

int sem_connect(size_t sem_key) {
    // allocating / connecting to the sem.
    int sem_id = semget(sem_key, 1, SEM_PERMS);
    if (sem_id == -1) {
        print_error("Could not connect to the semaphore group!");
        return -1;
    }

    char buff_print[500];
    sprintf(buff_print,
            "Successfully connected to the semaphore with the value assigned! "
            "Key = %ld, Id = %d",
            sem_key, sem_id);
    print_info(buff_print);
    print_info("Successfully connected to the semaphore!");
    return sem_id;
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