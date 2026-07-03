#include "check.h"
#include <stdbool.h>

/**
 * IMPLEMENTARLA RICORSIVA
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
bool check(const char *s1, int n1, const char *s2, int n2){
  if ((n1+1)/2 != n2)
    return false;

  if (n2 == 0)
    return true;

  if (*s1 != *s2)
    return false;

  return check(s1+2, n1-2, s2+1, n2-1);
}
