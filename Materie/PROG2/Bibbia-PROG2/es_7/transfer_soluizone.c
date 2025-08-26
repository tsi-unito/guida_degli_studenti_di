#include "transfer.h"
#include <stdbool.h>

/*
typedef struct node IntNode, *IntList;

struct node {
        int     data;
        IntList next;
}
*/

/**
 * @brief restituisce se l'elemento x appartiene a ls
 */
bool match(IntList ls, int x) {
  if (!ls)
    return false;
  if (ls->data == x)
    return true;

  return match(ls->next, x);
}

/**
 * @brief implementa ricorsivamente la funzione transfer
 */
IntList _transfer(IntList* current, IntList ls, IntList* transferred) {
  if (!*current) return *transferred;
  IntList next = (*current)->next;

  if (match(ls, (*current)->data)){
    (*current)->next = NULL;

    if (!*transferred)
      *transferred = *current;
    else {
      IntList tail = *transferred;
      while (tail->next) {
          tail = tail->next;
      }
      tail->next = *current;
    }
    *current = next;
  } else
    current = &((*current)->next);

  return _transfer(current, ls, transferred);
}

/**
 * @brief Modifica *lsPtr1 togliendo da *lsPtr1 tutti i nodi che occorrono
 * in ls2, ﻿﻿e li restituisce in una nuova lista nello stesso ordine in cui
 * occorrono in *IsPtr1 (NON ALLOCA NUOVA MEMORIA).
 * ESEMPIO:
 * date [1, 2, 5, 3, 4, 5, 9, 8] e [7, 5, 2, 4]
 * modifica la prima lista in [1, 3, 9, 8] e restituisce [2, 5, 4, 5].
 */
IntList transfer(IntList *lsPtr1, IntList ls2) {
    if (!lsPtr1 || !*lsPtr1) return NULL; 
    if (!ls2) return NULL;  
    
    IntList transferred = NULL;
    
    _transfer(lsPtr1, ls2, &transferred);
    
    return transferred;
}
