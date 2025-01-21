#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUFF_SIZE 10

/**
 * @remarks Represents the monotone behaviour of the 3 children
 * @param pipe_fd - The pipe file descriptor.
 * */
void monotone_fork(int pipe_fd[]);

int main() {
    printf("[INFO - parent] program started!\n");

    int pipe_fd[2];

    // creating pipe
    if (pipe(pipe_fd) == -1) {
        fprintf(stderr, "[ERROR - parent]: could not create the pipe! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    pid_t parent_pid = getpid();

    // forking the 3 childrens
    for (short i = 0; i < 3; i++) {
        if (getpid() == parent_pid) {
            monotone_fork(pipe_fd);
        }
    }

    // closing write end
    if (close(pipe_fd[1]) == -1) {
        fprintf(stderr,
                "[ERROR - parent]: could not close the write end of the pipe! "
                "'%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }
    // waiting for childs to close read end
    char buff[BUFF_SIZE];
    ssize_t bytes_read = 0;
    do {
        bytes_read = read(pipe_fd[0], buff, BUFF_SIZE);

        if (bytes_read == -1) {
            fprintf(stderr,
                    "[ERROR - parent]: Could not read from pipe! '%s'\n",
                    strerror(errno));
            exit(EXIT_FAILURE);
        } else if (bytes_read == 0) {
            printf("[INFO - parent]: read EOF from pipe!\n");
        }
    } while (bytes_read > 0);

    printf("[INFO - parent]: all children have closed the read end!\n");
    // waiting for childs to terminate
    while (wait(NULL) != -1);

    printf("[INFO - parent]: childrens have finished!\n");

    printf("[INFO - parent] program ended!\n");
    return 0;
}

void monotone_fork(int pipe_fd[]) {
    pid_t pid_fork = fork();

    if (pid_fork == -1) {
        fprintf(stderr, "[ERROR - parent]: could not fork! '%s'\n",
                strerror(errno));
    }

    pid_t pid = getpid();

    if (pid_fork != 0) {
        return;
    }

    // children part from here onwards
    // closing read end
    if (close(pipe_fd[0]) == -1) {
        fprintf(
            stderr,
            "[ERROR - children %u]: could not close the read end pipe! '%s'\n",
            pid, strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf(
        "[INFO - children %u]: successfully closed read end! Now waiting 3 "
        "seconds..\n",
        pid);
    // sleeping sometime
    sleep(3);
    if (close(pipe_fd[1]) == -1) {
        fprintf(
            stderr,
            "[ERROR - children %u]: could not close the write end pipe! '%s'\n",
            pid, strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf(
        "[INFO - children %u]: successfully closed write end! Now "
        "terminating..\n",
        pid);
    exit(EXIT_SUCCESS);
}
