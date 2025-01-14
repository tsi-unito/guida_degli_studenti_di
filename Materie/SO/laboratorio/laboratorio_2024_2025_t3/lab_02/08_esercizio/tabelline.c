#include <stdio.h>

int main() {
    for (int i = 1; i <= 10; i++) {
        for (int j = 1; j <= 10; j++) {
            printf("%6d", i * j);
        }
        printf("\n");
    }

    return 0;
}
