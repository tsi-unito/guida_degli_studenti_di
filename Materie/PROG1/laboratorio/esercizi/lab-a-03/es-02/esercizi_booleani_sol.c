#include <stdio.h>
#include <stdbool.h>

int main(void) {
    int a, b;
    printf("Inserisci due numeri: ");
    scanf("%d%d", &a, &b);

    // a) dati i due interi a e b, stampa 1 se il primo intero 
    //    è multiplo del secondo, 0 altrimenti;
    printf("a) %d\n", a % b == 0 ? 1 : 0);

    // b) dato l'intero a (un voto), stampa "true" se a 
    //    è compreso fra 1 e 30 (inclusi), "false" altrimenti;
    printf("b) %s\n", a >= 1 && a <= 30 ? "true" : "false");

    bool b1 = a > 10;
    bool b2 = b <= 5;
    // c) dati i due booleani b1 e b2, stampa a video il booleano 
    //    "false" se b1 e b2 sono entrambi veri, "true" altrimenti;
    printf("c) %s\n", b1 && b2 ? "false" : "true");

    // d) dati i due interi a e b, stampa a video il massimo; se sono 
    //    uguali stampa a video "I due valori sono uguali".
    printf("d) ");
    if (a > b) {
        printf("the maximum value is %d\n", a);
    } else if (b > a) {
        printf("the maximum value is %d\n", b);
    } else {
        printf("I due valori sono uguali\n");
    }

    int c = 20;
    // e) dati tre interi a, b e c, stampa a video il massimo 
    //    (si usi una variabile di supporto max); 
    int max = a;
    if (b > max) {
        max = b;
    }
    if (c > max) {
        max = c;
    }
    printf("e) the maximum value is %d\n", max);


    // f) dati tre interi a, b e c, stampa a video "ordinati" se questi sono 
    //    ordinati in modo crescente, altrimenti non stampare nulla.
    if (a <= b && b <= c) {
        printf("f) ordinati\n");
    }
}
