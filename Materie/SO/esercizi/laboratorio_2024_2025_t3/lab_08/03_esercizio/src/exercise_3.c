#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include "../include/semaphore.h"
#include "../include/shared_mem.h"

#define ARG_POS_CHILDS 1
#define ARG_POS_CYCLES 2

/**
 * @brief Prints the usage of the application.
 *
 */
void print_usage();

/**
 * @brief Validates the input of the app.
 *
 * @param argc The argument count.
 * @param argv A NULL terminated list of arguments
 */
void validate_input(int argc, char **argv);

int main(int argc, char **argv) {
    validate_input(argc, argv);

    size_t childrens = (size_t)atoi(argv[ARG_POS_CHILDS]);
    size_t cycles = (size_t)atoi(argv[ARG_POS_CYCLES]);

    // allocating shared memory.
    size_t shm_id = shm_get(SHM_KEY);

    // allocating semaphorew
    size_t mutex = sem_get(SEM_KEY, 1);
    // size_t full = sem_get(SEM_KEY + 1, 0);
    // size_t empty = sem_get(SEM_KEY + 2, 1);

    pid_t parent_pid = getpid();

    for (size_t i = 0; i < childrens; i++) {
        if (getpid() == parent_pid) {
            switch (fork()) {
                case -1:  // error
                    fprintf(stderr, "[ERROR]: Could not fork! '%s'\n",
                            strerror(errno));
                    break;
                case 0:  // children
                    printf("[INFO - PID %d - CHLD]: Children initiaized!\n",
                           getpid());
                    break;
                default:  // parent
                    break;
            }
        }
    }

    // parent execution: consumer
    if (getpid() == parent_pid) {
        for (size_t i = 0; i < cycles; i++) {
            sleep(5);
            // sem_wait(empty);
            sem_wait(mutex);
            // read PID from shared mem.
            Message parent_msg = shm_attach(shm_id);
            if (parent_msg != NULL) {
                printf(
                    "[INFO - PRNT]: Got the %d as message from shared "
                    "memory!\n",
                    parent_msg->pid);
            }
            shm_detach(parent_msg);
            parent_msg = NULL;
            sem_signal(mutex);
            // sem_signal(full);
        }
    } else {
        for (size_t i = 0; i < cycles; i++) {
            pid_t child_pid = getpid();

            // sleeping a random time
            size_t time_to_wait = rand() % 10;  // rand [0 .. 19]
            printf("[INFO - PID %d - CHLD]: Now waiting for %zu seconds\n",
                   child_pid, time_to_wait);
            sleep(time_to_wait);

            printf(
                "[INFO - PID %d - CHLD]: Process activated! Entering critical "
                "section..\n",
                child_pid);
            // waiting in mutual exclusion the access for sh. mem.
            // sem_wait(full);
            sem_wait(mutex);

            Message child_msg = shm_attach(shm_id);
            if (child_msg != NULL) {
                child_msg->pid = getpid();
            }
            shm_detach(child_msg);
            child_msg = NULL;

            printf(
                "[INFO - PID %d - CHLD]: Slice finished! Exiting critical "
                "section..\n",
                child_pid);
            sem_signal(mutex);
            // sem_signal(empty);
        }
        exit(EXIT_SUCCESS);
    }

    // parent only execution.
    printf("[INFO - PRNT]: Work finished! Now waiting for childs!\n");
    while (wait(NULL) != -1);
    printf(
        "[INFO - PRNT]: All childs have finished! Now closing everything!\n");

    // removing shared memory
    shm_del(shm_id);
    sem_del(mutex);
    return 0;
}

// see prototype
void print_usage() {
    printf("Usage: ./exercise_3 <N> <M>\n");
    printf("\t<N>: The # of child processes to create.\n");
    printf("\t<M>: The amount of cycles on which to wait.\n");
}

// see prototype
void validate_input(int argc, char **argv) {
    if (argc == 2 && (strcmp(argv[1], "--h") || strcmp(argv[1], "-help"))) {
        print_usage();
        exit(EXIT_SUCCESS);
    }

    if (argc != 3) {
        fprintf(stderr, "[ERROR]: Invalid argument number!\n");
        print_usage();
        exit(EXIT_FAILURE);
    }

    if (atoi(argv[ARG_POS_CHILDS]) <= 0) {
        fprintf(stderr, "[ERROR]: Invalid <N> value! Inserted '%s'\n",
                argv[ARG_POS_CHILDS]);
        print_usage();
        exit(EXIT_FAILURE);
    }

    if (atoi(argv[ARG_POS_CYCLES]) <= 0) {
        fprintf(stderr, "[ERROR]: Invalid <M> value! Inserted '%s'\n",
                argv[ARG_POS_CYCLES]);
        print_usage();
        exit(EXIT_FAILURE);
    }
}
