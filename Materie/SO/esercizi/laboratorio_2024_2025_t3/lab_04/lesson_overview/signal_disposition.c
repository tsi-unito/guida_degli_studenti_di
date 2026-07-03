#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

void custom_signal_handler(int signal) {
    printf("[sig. handler] - invoked!\n");
    printf("\t[sig. handler] SIG NAME: %s\n", sigabbrev_np(signal));

    // NB: per usare sigdescr_np va aggiunto _GNU_SOURCE come macro prima di
    // ogni include
    //     oppure va compilato con -D_GNU_SOURCE
    printf("\t[sig. handler] SIG DESC: %s\n", strsignal(signal));
}

int main() {
    // la signal disposition di un processo e' il modo in cui il processo stesso
    // gestisce i segnali in arrivo.
    // Puo' essere gestita con:
    // - signal(int signal, void (*disposition)(int)) => non tanto consigliata
    // perche'
    //   varia nel comportamento. Restituisce il puntatore alla disposizione
    //   precedente
    // - sigaction(int signal, *struct sigaction new, *struct sigaction old)
    //   usa una struct per la disposizione del segnale.

    short choice;
    int signal_to_handle = SIGUSR1;

    do {
        printf("process PID: %d\n", getpid());
        printf("signal disposition menu:\n");
        printf("\t1. use signal(int, void (*handler)(int));\n");
        printf(
            "\t2. use sigaction(int, *struct sigaction new, *struct sigaction "
            "old);\n");
        printf("\t0. exit\n");
        printf("insert your choice: ");
        scanf("%hd", &choice);
        printf("\n");

        switch (choice) {
            case 0:  // exit
                printf("quitting..\n");
                break;
            case 1:  // use signal();

                // setting the handler pointier;
                void (*new_handler)(int);
                new_handler = custom_signal_handler;

                void (*old_handler)(int) =
                    signal(signal_to_handle, new_handler);

                // signal(); returns SIG_ERR on error and populates it in errno
                if (old_handler == SIG_ERR) {
                    fprintf(stderr,
                            "ERROR: could not set the signal handler with "
                            "signal()!\n");
                    fprintf(stderr,
                            "\tError details: \n\t\tError name: %s\n\t\tError "
                            "desc: %s\n",
                            strerrorname_np(errno), strerror(errno));
                } else {
                    printf("signal handler for signal 'SIG%s': '%s' set!\n",
                           sigabbrev_np(signal_to_handle),
                           strsignal(signal_to_handle));
                    printf(
                        "try to send a signal with the system application kill "
                        "-SIG%s\nNow waiting..\n",
                        sigabbrev_np(signal_to_handle));
                    pause();
                }
                break;
            case 2:  // use sigaction();

                // setting up the new action
                struct sigaction new_action;
                struct sigaction old_action;
                new_action.sa_handler = custom_signal_handler;
                // setting the mask
                sigset_t new_mask;
                sigemptyset(&new_mask);
                new_action.sa_mask = new_mask;  // setting the mask
                new_action.sa_flags = 0;        // setting the flags
                int action_res =
                    sigaction(signal_to_handle, &new_action, &old_action);

                if (action_res == -1) {
                    fprintf(stderr,
                            "ERROR: could not set the signal handler with "
                            "signal()!\n");
                    fprintf(stderr,
                            "\tError details: \n\t\tError name: %s\n\t\tError "
                            "desc: %s\n",
                            strerrorname_np(errno), strerror(errno));
                } else {
                    printf("signal handler for signal 'SIG%s': '%s' set!\n",
                           sigabbrev_np(signal_to_handle),
                           strsignal(signal_to_handle));
                    printf(
                        "try to send a signal with the system application kill "
                        "-SIG%s\nNow waiting..\n",
                        sigabbrev_np(signal_to_handle));
                    pause();
                }
                break;
            default:  // illegal argument
                printf("you have inserted an invalid value: '%d'.\nTry again\n",
                       choice);
                break;
        }
    } while (choice != 0);

    return 0;
}
