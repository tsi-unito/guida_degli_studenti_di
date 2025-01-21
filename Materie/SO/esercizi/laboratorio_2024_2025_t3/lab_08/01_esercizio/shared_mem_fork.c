#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

// shared mem. includes and defines
#include <sys/shm.h>
#define SHM_KEY 42
#define SHM_PERM 0777

#define MSG_LEN 100
typedef struct {
    size_t counter;
    char message[MSG_LEN];
} StrMessageItem;

typedef StrMessageItem *MessageItem;

int main() {
    // connecting to shared memory
    int shm_id = shmget(SHM_KEY, sizeof(StrMessageItem), IPC_CREAT | SHM_PERM);
    if (shm_id < 0) {
        fprintf(stderr, "[ERROR]: Could not get the shared memory object!\n");
        exit(EXIT_FAILURE);
    }

    // attaching to segment
    MessageItem item = (MessageItem)shmat(shm_id, NULL, 0);
    if (item == NULL) {
        fprintf(stderr, "[ERROR]: Failed to attach to the shared memory!'%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }
    strcpy(item->message, "Hello from process master!\n");

    // detach
    int res_dt = shmdt(item);
    if (res_dt < 0) {
        fprintf(stderr, "[ERROR]: Could not detach from shared memory! '%s'\n",
                strerror(errno));
    }
    item = NULL;

    // forking
    switch (fork()) {
        case -1:  // error
            fprintf(stderr, "[ERROR]: Could not fork!'%s'\n", strerror(errno));
            exit(EXIT_FAILURE);
            break;
        case 0:  // children
            item = (MessageItem)shmat(shm_id, NULL, 0);
            if (item == (void *)-1) {
                fprintf(stderr,
                        "[ERROR - children]:  Could not get the shared memory "
                        "struct! '%s'\n",
                        strerror(errno));
                exit(EXIT_FAILURE);
            }
            printf("[INFO - children]: Successfully retrieved data!\n");
            printf("\tCounter: %zu\n", item->counter);
            printf("\tMessage: %s\n", item->message);

            // detaching
            res_dt = shmdt(item);
            if (res_dt < 0) {
                fprintf(stderr,
                        "[ERROR]: Could not detach from shared memory! '%s'\n",
                        strerror(errno));
            }
            item = NULL;
            exit(EXIT_SUCCESS);
            break;
        default:  // parent
            wait(NULL);
            // deleting shared memory segment
            int del_res = shmctl(shm_id, IPC_RMID, 0);
            break;
    }

    return 0;
}