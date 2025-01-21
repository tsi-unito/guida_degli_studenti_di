#include "../include/shared_mem.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/shm.h>

// see *.h file for desc
size_t shm_get(size_t shm_key) {
    ssize_t shm_id = shmget(SHM_KEY, sizeof(StrMessage), IPC_CREAT | SHM_PERM);

    if (shm_id < 0) {
        fprintf(
            stderr,
            "[ERROR]: Failed to allocate/connect to the shared memory! '%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }

    return shm_id;
}

// see *.h file for desc
Message shm_attach(size_t shm_id) {
    Message msg = (Message)shmat(shm_id, NULL, 0);

    if (msg == (void *)-1) {
        fprintf(stderr,
                "[ERROR]: Failed to attach to the shared memory! '%s'\n",
                strerror(errno));
        return NULL;
    }

    return msg;
}

// see *.h file for desc
void shm_detach(Message msg) {
    int res_dt = shmdt(msg);
    if (res_dt < 0) {
        fprintf(stderr,
                "[ERROR]: Failed to detach to the shared memory! '%s'\n",
                strerror(errno));
    }
}

// see *.h file for desc
void shm_del(size_t shm_id) {
    int res_del = shmctl(shm_id, IPC_RMID, NULL);

    if (res_del < 0) {
        fprintf(stderr, "[ERROR]: Failed to delete the shared memory! '%s'\n",
                strerror(errno));
    }
}
