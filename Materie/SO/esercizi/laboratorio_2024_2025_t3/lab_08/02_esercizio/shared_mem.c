#include "./shared_mem.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/shm.h>
#include <unistd.h>
#define SHM_PERM 0777

// see declaration
int shm_get(size_t shm_key, size_t size) {
    int res_get = shmget(SHM_KEY, sizeof(StrMessageItem), IPC_CREAT | SHM_PERM);
    if (res_get < 0) {
        fprintf(
            stderr,
            "[ERROR - PID: %d]: Could not allocate the shared memory! '%s'\n",
            getpid(), strerror(errno));
        exit(EXIT_FAILURE);
    }

    return res_get;
}

// see declaration
MessageItem shm_attach(size_t shm_id) {
    MessageItem item = (MessageItem)shmat(shm_id, NULL, 0);
    if (item == (void *)-1) {
        fprintf(stderr,
                "[ERROR - children]:  Could not get the shared memory struct! "
                "'%s'\n",
                strerror(errno));
    }
    return item;
}

// see declaration
void shm_detach(size_t shm_id, MessageItem msg) {
    int res_dt = shmdt(msg);
    if (res_dt < 0) {
        fprintf(stderr, "[ERROR]: Could not detach from shared memory! '%s'\n",
                strerror(errno));
    }
}

void shm_delete(size_t shm_id) {
    int del_res = shmctl(shm_id, IPC_RMID, 0);
}