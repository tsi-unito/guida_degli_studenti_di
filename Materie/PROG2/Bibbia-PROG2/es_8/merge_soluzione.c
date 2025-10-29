#include "merge.h"

/*
typedef struct node IntNode, *IntList;

struct node {
        int data;
        IntList next;
}
*/

/**
 * DA IMPLEMENTARE ITERATIVA
 * @brief Date due liste ordinate *IsPtr1 e *IsPtr2,
 * restituisce l'unione ordinata delle due
 * Non si può usare malloc, ma bisogna ordinare gli elementi delle liste
 * @return Primo elemento della lista riformattata
 * alla fine i param saranno entrambi NULL
 * Ad esempio:
 * date [1, 5, 9] e [0, 2, 4, 6, 8]
 * restituisce [0, 1, 2, 4, 5, 6, 8, 9]
 */
IntList merge(IntList *lsPtr1, IntList *lsPtr2) {
  if (!lsPtr1 || !lsPtr2)
    return NULL;

  IntList ls1 = *lsPtr1, ls2 = *lsPtr2;

  if (!ls1) {
    IntList result = ls2;
    *lsPtr1 = *lsPtr2 = NULL;
    return result;
  }
  if (!ls2) {
    IntList result = ls1;
    *lsPtr1 = *lsPtr2 = NULL;
    return result;
  }

  IntList result;
  if (ls1->data <= ls2->data) {
    result = ls1;
    ls1 = ls1->next;
  } else {
    result = ls2;
    ls2 = ls2->next;
  }

  IntList current = result;
  while (ls1 && ls2) {
    if (ls1->data <= ls2->data) {
      current->next = ls1;
      ls1 = ls1->next;
    } else {
      current->next = ls2;
      ls2 = ls2->next;
    }
    current = current->next;
  }

  if (ls1) {
    current->next = ls1;
  } else if (ls2) {
    current->next = ls2;
  }

  *lsPtr1 = *lsPtr2 = NULL;
  return result;
}
