#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFF_SIZE 200

int main() {
    printf("[INFO - main_pipe]: Started program!\n");

    FILE *pipe_fd = popen("./filter.out", "r");

    if (pipe_fd == NULL) {
        fprintf(stderr,
                "[ERROR - main_pipe]: Could not open pipe to communicate with "
                "filter.c! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    char buffer[BUFF_SIZE];
    char *line = NULL;
    while ((line = fgets(buffer, BUFF_SIZE, pipe_fd)) != NULL) {
        printf("[INFO - main_pipe]: Read line '%s'\n", line);
    }

    if (pclose(pipe_fd) == -1) {
        fprintf(stderr, "[ERROR - main_pipe]: Could not close the pipe! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO - main_pipe]: Finished program!\n");
    return 0;
}
