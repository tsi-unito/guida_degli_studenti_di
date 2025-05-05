#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "./sem_common.h"

int main() {
    // creating/connecting to the queue!
    print_info("Creating sem. and setting 1 as main value");
    int sem_id = sem_init(SEM_KEY, 1);

    // forking for critical section
    pid_t parent_pid = getpid();
    for (size_t i = 0; i < 3; i++) {
        if (getpid() == parent_pid) {  // parent has to fork
            switch (fork()) {
                case -1:  // error
                    print_error("Could not fork!");
                    exit(EXIT_FAILURE);
                    break;
                case 0:  // children
                    // entering critical section - lock
                    sem_wait(sem_id);
                    print_info("I entered critical section! Waiting 2 secs");
                    sleep(2);
                    print_info(
                        "Another text just to prove that processes are sync!");
                    sem_signal(sem_id);

                    exit(EXIT_SUCCESS);
                    break;
                default:  // parent has to continue
            }
        }
    }

    // wait for childrens
    while (wait(NULL) != -1);

    return 0;
}