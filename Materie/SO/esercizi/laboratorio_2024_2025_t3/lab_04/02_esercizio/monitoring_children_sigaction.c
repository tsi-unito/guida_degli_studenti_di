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
            struct sigaction child_disposition;
            child_disposition.sa_handler =
                custom_terminal_handler;  // adding custom handler

            // defining the mask
            sigset_t mask;
            int mask_result = sigemptyset(&mask);
            if (mask_result != 0) {
                fprintf(stderr,
                        "Error: could not correctly initialize the mask for "
                        "the signal "
                        "handler! '%s'\n",
                        strerror(errno));
                wait(NULL);
                exit(1);
            }
            child_disposition.sa_mask = mask;

            // setting the mask
            struct sigaction old_disposition;
            sigaction(SIGUSR1, &child_disposition, &old_disposition);
            printf(
                "[children]: signal handler setted! try it! Now waiting for "
                "signal..\n");

            // waiting for signal
            pause();
            printf(
                "[children]: signal received! Now reverting to default and "
                "waiting "
                "again!\n");

            // resetting old disposition
            sigaction(SIGUSR1, &old_disposition, NULL);
            pause();  // waiting for new signal
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
