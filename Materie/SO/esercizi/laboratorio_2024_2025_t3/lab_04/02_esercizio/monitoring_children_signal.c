#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

void custom_terminal_handler(int signal) {
    printf("hello from custom signal handler! Signal received: %d: '%s'\n",
           signal, strsignal(signal));
}

int main() {
    pid_t child_pid = fork();
    char *error = strerror(errno);

    switch (child_pid) {
        case -1:  // error
            fprintf(
                stderr,
                "Error: could not create children process with fork();! '%s'\n",
                error);
            break;
        case 0:  // children
            printf("[children - PID: %d]: setting up the signal handler!\n",
                   getpid());
            // defining the signal disposition struct

            void (*new_handler)(int) = custom_terminal_handler;
            void (*old_handler)(int) = signal(SIGUSR1, new_handler);
            printf("[children]: custom signal set! try to send SIGUSR1!\n");
            pause();  // waiting for signal

            // resetting signal
            printf("[children]: resetting default signal.\n");
            signal(SIGUSR1, old_handler);

            printf("[children]: old signal set! waiting for it!\n");
            pause();

            exit(0);
            break;
        default:  // parent
            printf("[parent]: waiting for children!\n");
            // defining the signal disposition struct
            wait(NULL);
            printf("[parent]: waited for process termination!\n");
            break;
    }

    return 0;
}
