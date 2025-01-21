#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "./sem_common.h"
#define BUF_MAX_LEN 500

int main() {
    // connecting to semaphore
    print_info("Connecting to the semaphores!");
    int empty = sem_connect(SEM_KEY);
    int full = sem_connect(SEM_KEY + 1);
    print_info("Connection to semaphores completed!");

    // accessing shared memory
    // reading
    FILE *shared_mem = fopen("./shared_mem.data", "r");
    if (shared_mem == NULL) {
        print_error("Could not access shared memory!");
        exit(EXIT_FAILURE);
    }
    print_info("Connected to shared memory! starting sync reader/writer!");

    bool should_exit = false;
    int row_counter = 0;
    char buffer[BUF_MAX_LEN];
    while (!should_exit && row_counter < 50) {
        sem_wait(full);
        if (fgets(buffer, sizeof(buffer), shared_mem) != NULL) {
            char buff_print[BUF_MAX_LEN + 100];
            sprintf(buff_print, "Data [ROW %d]: '%s'", row_counter + 1, buffer);
            print_info(buff_print);
            row_counter++;
        } else {
            print_info("Finished rows!");
            should_exit = true;
        }
        sem_signal(empty);
    }

    fclose(shared_mem);

    return 0;
}