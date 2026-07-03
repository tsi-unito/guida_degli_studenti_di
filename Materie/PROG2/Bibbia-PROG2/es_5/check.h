#ifndef CHECK_H
#define CHECK_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

/**
 * @brief Dati una stringa s1 di lunghezza n1 >= 0 
 * e una stringa s2 di lunghezza n2 >= 0 !
 * verificha che le stringhe s2 e s3 siano uguali, dove s3 è la stringa
 * ottenuta considerando solo i caratteri in posizione pari di s1
 * 
 * NOTA BENE: Si assuma s1 != NULL e s2 != NULL 
 * ESEMPI:
 * (1) date "pArEeBvUaT" (di lunghezza 10) e "provateci" (di lunghezza 9)
 * restituisce 0.
 * (2) date "pArEoBvUaT" (di lunghezza 10) e "prova" (di lunghezza 5)
 * restituisce 1.
 * (3) date "pArEoBvUaTeci" (di lunghezza 13) e "prova" (di lunghezza 5)
 * restituisce 0.
 */
bool check(const char *s1, int n1, const char *s2, int n2);

// Funzioni ausiliarie per i test
static inline char* __extractEvenChars(const char *s1, int n1) {
    // Conta i caratteri in posizione pari
    int evenCount = (n1 + 1) / 2; // posizioni 0, 2, 4, 6, ...
    
    char *result = (char*)malloc((evenCount + 1) * sizeof(char));
    if (!result) return NULL;
    
    int j = 0;
    for (int i = 0; i < n1; i += 2) {
        result[j++] = s1[i];
    }
    result[j] = '\0';
    
    return result;
}

static inline void __printStringInfo(const char *s, int len, const char *name) {
    printf("         %s: \"%s\" (len=%d)\n", name, s, len);
}

static inline void __printEvenChars(const char *s1, int n1) {
    char *evenChars = __extractEvenChars(s1, n1);
    printf("         Even positions from s1: \"%s\"\n", evenChars);
    free(evenChars);
}

static inline int __countEvenPositions(int n1) {
    return (n1 + 1) / 2;
}

static inline bool __manualCheck(const char *s1, int n1, const char *s2, int n2) {
    // Verifica manuale per confronto con la funzione ricorsiva
    int evenCount = __countEvenPositions(n1);
    
    if (evenCount != n2) return false;
    
    int j = 0;
    for (int i = 0; i < n1; i += 2) {
        if (j >= n2 || s1[i] != s2[j]) {
            return false;
        }
        j++;
    }
    
    return j == n2;
}

#endif // CHECK_H
