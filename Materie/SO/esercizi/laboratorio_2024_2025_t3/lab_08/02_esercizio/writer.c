#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./semaphore.h"
#include "./shared_mem.h"
#define BUF_MAX_LEN 500

int main() {
    // reading content from stdin
    char buffer[BUF_MAX_LEN];
    print_info("Creating the semaphores!");
    int empty = sem_init(SEM_KEY, 1);
    int full = sem_init(SEM_KEY + 1, 0);
    print_info(
        "Initialized the sem. empty with 1 as initial value and full with "
        "0! Connecting to shared memory!");

    int shm_id = shm_get(SHM_KEY, sizeof(StrMessageItem));
    print_info("Connected to shared memory! starting sync reader/writer!");

    // reading
    int row_counter = 0;
    while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
        sem_wait(empty);

        buffer[strlen(buffer) - 1] = '\0';

        char buff_print[530];
        sprintf(buff_print, "Read from STDIN %d: '%s'", row_counter + 1,
                buffer);
        print_info(buff_print);

        // attach
        MessageItem msg = shm_attach(shm_id);
        if (msg != NULL) {
            strcpy(msg->row, buffer);
            shm_detach(shm_id, msg);
        } else {
            print_error("Could not attach! msg = NULL");
        }

        row_counter++;
        sem_signal(full);
    }

    return 0;
}