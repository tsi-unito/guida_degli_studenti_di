#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "./sem_common.h"

int main() {
    // creating / getting semaphore
    int sem_id = semget(SEM_KEY, 1, IPC_CREAT | SEM_PERMS);
    if (sem_id == -1) {
        print_error("Could not create the semaphore!");
        exit(EXIT_FAILURE);
    }
    print_info("Successfully created a semaphore!");

    // setting inital value
    union semun arg;
    arg.val = 0;
    int res_ctl = semctl(sem_id, 0, SETVAL, arg);
    if (res_ctl == -1) {
        print_error("Could not set the inital value of semaphore!");
        exit(EXIT_FAILURE);
    }
    print_info(
        "Successfully setted the value 0 as inital value for the semaphore!");

    // forking
    switch (fork()) {
        case -1:  // error
            print_error("Could not fork!");
            break;
        case 0:  // children
            print_info("Waiting 5 seconds to signal!");
            sleep(5);
            sem_signal(sem_id);
            print_info("Signaled!");
            break;
        default:  // parent
            print_info("Starting wait..");
            sem_wait(sem_id);
            print_info("End wait!");
            wait(NULL);
            break;
    }
    return 0;
}