#include <stdio.h>
#include <stdlib.h>

#include "./sem_common.h"
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

    FILE *shared_mem = fopen("./shared_mem.data", "w");
    if (shared_mem == NULL) {
        print_error("Could not access shared memory!");
        exit(EXIT_FAILURE);
    }
    print_info("Connected to shared memory! starting sync reader/writer!");

    // reading
    int row_counter = 0;
    while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
        sem_wait(empty);
        char buff_print[530];
        sprintf(buff_print, "Printed the row %d: '%s'", row_counter + 1,
                buffer);
        print_info(buff_print);
        fprintf(shared_mem, "%s", buffer);
        fflush(shared_mem);
        row_counter++;
        sem_signal(full);
    }

    fclose(shared_mem);

    return 0;
}