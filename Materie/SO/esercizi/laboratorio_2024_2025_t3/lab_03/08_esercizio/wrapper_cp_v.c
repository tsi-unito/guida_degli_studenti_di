#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
    char *prog_name = "[wrapper]";
    printf("%s - starting..\n", prog_name);

    switch (fork()) {
        // errore:
        case -1:
            fprintf(stderr, "%s - the fork(); command returned an error!\n",
                    prog_name);
            break;
        // figlio:
        case 0:
            char *copier_path = "./copier_cp.out";
            char *copier_params[4] = {copier_path, "./files/source.txt",
                                      "./files/dest.txt", NULL};
            execv(copier_path, copier_params);
            fprintf(stderr, "%s CHILDREN - failed to execute the program %s!\n",
                    prog_name, copier_path);
            break;
        // padre:
        default:
            int children_stat = 1;
            pid_t children_pid = wait(&children_stat);

            if (children_pid <= 0) {
                fprintf(stderr,
                        "%s PARENT - the wait(); command returned an error!\n",
                        prog_name);
                exit(1);
            }

            if (children_stat == 1) {
                fprintf(stderr,
                        "%s PARENT - the children process terminated with an "
                        "error!\n",
                        prog_name);
                exit(1);
            }

            printf("%s PARENT - the children proc. closed gracefully\n",
                   prog_name);
            break;
    }

    return 0;
}
