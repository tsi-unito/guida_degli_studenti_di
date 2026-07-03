#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "./semaphore.h"
#include "./shared_mem.h"
#define BUF_MAX_LEN 500

int main() {
    // connecting to semaphore
    print_info("Connecting to the semaphores!");
    int empty = sem_connect(SEM_KEY);
    int full = sem_connect(SEM_KEY + 1);
    print_info("Connection to semaphores completed!");

    bool should_exit = false;
    int row_counter = 0;
    char buffer[BUF_MAX_LEN];
    int shm_id = shm_get(SHM_KEY, sizeof(StrMessageItem));
    while (!should_exit && row_counter < 50) {
        sem_wait(full);
        print_info("inside full!");
        MessageItem msg = shm_attach(shm_id);
        if (msg != NULL) {
            char buff_print[BUF_MAX_LEN + 100];
            sprintf(buff_print, "Data [ROW %d]: '%s'", row_counter + 1,
                    msg->row);
            print_info(buff_print);
            row_counter++;
        } else {
            print_info("Finished rows!");
            should_exit = true;
        }
        sem_signal(empty);
    }

    return 0;
}