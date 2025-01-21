#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUFF_SIZE 200

void children_exec(int pipe_fd[], bool should_fork, char **args);
void parent_exec(int pipe_fd[], char *message);
void grandchildren_exec(int pipe_fd[], char *message);

int main(int argc, char **args) {
    // PIPE File descriptors
    int pipe_fd[2];

    char buff[BUFF_SIZE];

    if (argc < 2 || strcmp(args[1], "--help") == 0) {
        // print program invocation
        printf("[INFO] : Usage: ./app <message>\n");
        return 0;
    }

    int pipe_res = pipe(pipe_fd);  // 0 => success. -1 on error

    if (pipe_res == -1) {
        fprintf(stderr, "[ERROR]: PIPE Creation failed! '%s'\n",
                strerror(errno));
        exit(1);
    }
    printf("[INFO] : Successfully created PIPE!\n");

    // forking to have parent child comunication

    switch (fork()) {
        case -1:  // error
            fprintf(stderr, "[ERROR]: fork() failed! '%s'\n", strerror(errno));
            break;
        case 0:                                       // children
            children_exec(pipe_fd, argc == 3, args);  // reading
            exit(EXIT_SUCCESS);
            break;
        default:                            // parent
            parent_exec(pipe_fd, args[1]);  // writing
            wait(NULL);                     // waiting for children
            break;
    }

    return 0;
}

void grandchildren_exec(int pipe_fd[], char *message) {
    // closing write end
    if (close(pipe_fd[1]) == -1) {
        fprintf(
            stderr,
            "[ERROR - grand child]: Could not close write end for PIPE! '%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }

    bool should_exit = false;
    char buffer[BUFF_SIZE];

    while (!should_exit) {
        // readin
        ssize_t read_chars = read(pipe_fd[0], buffer, BUFF_SIZE);
        switch (read_chars) {
            case -1:  // error
                fprintf(
                    stderr,
                    "[ERROR - grand child]: could not read from PIPE! '%s'\n",
                    strerror(errno));
                exit(EXIT_FAILURE);
                break;
            case 0:  // EOF => should exit
                should_exit = true;
                break;
            default:  // read N chars => will print to console
                ssize_t write_chars = write(STDOUT_FILENO, buffer, read_chars);
                if (write_chars == -1) {
                    fprintf(stderr,
                            "[ERROR - grand child]: could not print to "
                            "console! '%s'\n",
                            strerror(errno));
                    exit(EXIT_FAILURE);
                }
                break;
        }
    }

    // adding termination char
    if (write(STDOUT_FILENO, "\n", 1) != 1) {
        fprintf(stderr,
                "[ERROR - grand child]: could not print new line! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    // closing read end
    if (close(pipe_fd[0]) == -1) {
        fprintf(stderr,
                "[ERROR - grand child]: could not close the read end! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO - grand child]: 2nd consumer terminated! will now exit..\n");
}

void children_exec(int pipe_fd[], bool should_fork, char **args) {
    bool should_exit = false;
    char buffer[BUFF_SIZE];

    // closing write end
    if (close(pipe_fd[1]) != 0) {
        fprintf(
            stderr,
            "[ERROR - children]: consumer could NOT close write end! '%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }

    while (!should_exit) {
        // reading from buffer using read();
        ssize_t read_lines = read(pipe_fd[0], buffer, BUFF_SIZE);

        switch (read_lines) {
            case -1:  // error
                fprintf(stderr,
                        "[ERROR - children]: read() from pipe failed! '%s'\n",
                        strerror(errno));
                exit(EXIT_FAILURE);
                break;
            case 0:  // EOF => must exit from infine reads
                should_exit = true;
                break;
            default:  // Read N lines => print them in the std out

                printf(
                    "[INFO - children] : successfully read %zu lines from "
                    "pipe! "
                    "writing them in STDOUT\n",
                    read_lines);
                ssize_t write_lines = write(STDOUT_FILENO, buffer, read_lines);
                if (write_lines < 0) {
                    fprintf(
                        stderr,
                        "[ERROR - children]: write() to STDOUT failed! '%s'\n",
                        strerror(errno));
                    exit(EXIT_FAILURE);
                }

                if (read_lines != write_lines) {
                    fprintf(stderr,
                            "[ERROR - children]: read and writen lines are "
                            "different! %zu "
                            "(R) != %zu (W)\n",
                            read_lines, write_lines);
                    exit(EXIT_FAILURE);
                }

                printf(
                    "[INFO - children] : successfully wrote %zu lines to "
                    "STDOUT!\n",
                    write_lines);
                break;
        }
    }

    // adding terminating char
    if (write(STDOUT_FILENO, "\n", 1) < 0) {
        fprintf(
            stderr,
            "[ERROR - children] : could not write termination char on STDOUT! "
            "'%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }

    // closing file descriptor for the read end.
    if (close(pipe_fd[0]) != 0) {
        fprintf(stderr,
                "[ERROR - children]: close() for pipe read file desc. failed! "
                "'%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    // will create a new child and pass it a new PIPE on which this process will
    // pass the 2 param
    if (should_fork) {
        printf(
            "[INFO - children]: will now print the 2nd message to another "
            "pipe!\n");
        int gchild_pipe_fd[2];
        if (pipe(gchild_pipe_fd) == -1) {
            fprintf(
                stderr,
                "[ERROR - children]: consumer could not open PIPE for grand "
                "children!\n '%s'\n",
                strerror(errno));
            exit(EXIT_FAILURE);
        }

        // fork to grand children
        switch (fork()) {
            case -1:  // error
                fprintf(stderr,
                        "[ERROR - children]: fork() failed to creade "
                        "grandchildren! '%s'\n",
                        strerror(errno));
                break;
            case 0:  // grandchildren => execute code and then terminate
                grandchildren_exec(gchild_pipe_fd, args[2]);
                exit(EXIT_SUCCESS);
                break;
            default:  // children => will write on pipe the message from command
                      // line
                // closing read end
                if (close(gchild_pipe_fd[0] == -1)) {
                    fprintf(stderr,
                            "[ERROR - children]: Could not close the file "
                            "descriptor for "
                            "2nd pipe! '%s'\n",
                            strerror(errno));
                    exit(EXIT_FAILURE);
                }

                // writing
                ssize_t write_lines =
                    write(gchild_pipe_fd[1], args[2], strlen(args[2]));
                if (write_lines < 0) {
                    fprintf(stderr,
                            "[ERROR - children]: Could not print 2nd message "
                            "in 2nd PIPE! "
                            "'%s'\n",
                            strerror(errno));
                    exit(EXIT_FAILURE);
                }

                // closing write end
                if (close(gchild_pipe_fd[1]) == -1) {
                    fprintf(stderr,
                            "[ERROR - children]: Could not close write end of "
                            "2nd PIPE! '%s'\n",
                            strerror(errno));
                    exit(EXIT_FAILURE);
                }

                printf(
                    "[INFO - children]: Successfully wrote %zu chars to the "
                    "2nd PIPE! "
                    "waiting for g.child\n",
                    write_lines);

                // will now wait for children to read
                wait(NULL);

                break;
        }
    }

    printf("[INFO - children]: consumer ended its work!\n");
}

void parent_exec(int pipe_fd[], char *message) {
    if (close(pipe_fd[0]) < 0) {  // producer has to close read end
        fprintf(
            stderr,
            "[ERROR - parent]: producer failed to close pipe read end! '%s'\n",
            strerror(errno));
        exit(EXIT_FAILURE);
    }

    // parent will write the string passed as argument
    ssize_t write_lines = write(pipe_fd[1], message, strlen(message));

    if (write_lines < 0) {
        fprintf(stderr,
                "[ERROR - parent]: producer failed to write in pipe! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO - parent]: consumer successfully wrote %zu lines!\n",
           write_lines);

    // closing the write end

    if (close(pipe_fd[1]) != 0) {
        fprintf(stderr,
                "[ERROR - parent]: producer could NOT close write end! '%s'\n",
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("[INFO - parent]: consumer ended! will now wait for children!\n");
}
