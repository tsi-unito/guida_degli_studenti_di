#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char **environ;

int main() {
    if (environ == NULL) {
        printf("ENVIRON e' vuota!\n");
    } else {
        char **env_variable = environ;

        while (*env_variable != NULL) {
            if (strstr(*env_variable, "LOGNAME") != NULL ||
                strstr(*env_variable, "HOME") ||
                strstr(*env_variable, "PATH")) {
                printf("%s\n", *env_variable);
            }
            env_variable++;
        }
    }

    return 0;
}
