#include <stdio.h>
#include <string.h>

void print_decode(char *str);

int main() {
    char *encoded =
        "Kn\"eqtuq\"hqtpkueg\"wp)kpvtqfw|kqpg\"cn\"nkpiwciikq\"fk\"rtqitcooc|"
        "kqpg\"E\"g\"cnnc\"rtqitcooc|"
        "kqpg\"korgtcvkxc0\"Wp\"pwogtq\"eqpukuvgpvg\"fk\"qtg\"gb\"wvknk||"
        "cvq\"rgt\"uxqnigtg\"gugtekvc|kqpk\"kp\"ncdqtcvqtkq\"hkpcnk||"
        "cvg\"c\"rtgpfgtg\"eqphkfgp|c\"eqp\"kn\"nkpiwciikq\"vtcokvg\"nc\"tgcnk|"
        "|"
        "c|kqpg\"fk\"rtqitcook\"kp\"nkpiwciikq\"E0";
    printf("before: '%s'\n\n", encoded);
    printf("After: '");
    print_decode(encoded);
    printf("'\n");

    return 0;
}

void print_decode(char *str) {
    int i = 0;
    while (i < strlen(str)) {
        printf("%c", str[i] - 2);
        i++;
    }
}
