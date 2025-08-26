#include "mixAlternate.h"

/*
Definiti nel .h:
typedef struct node IntNode, *IntList;
struct node {
    int data;
    IntList next;
};

*/

/**
 * @brief Restituisce la lista alternata dei nodi di *lsPtr1 e *lsPtr2,
 * togliendoli da *lsPtr1 e *lsPtr2, 
 * che alla fine conterranno entrambi NULL (non alloca nuova memoria). 
 * 
 * Ad es. date [1, 5, 9] e [0, 2, 4, 6, 8] restituisce
 * [1, 0, 5, 2, 9, 4, 6, 8].
 */
IntList mixAlternate(IntList *lsPtr1, IntList *lsPtr2) {
    if (!lsPtr1 || !lsPtr2) return NULL;
    if (!*lsPtr1) {
        IntList result = *lsPtr2;
        *lsPtr2 = NULL;
        return result;
    }
    if (!*lsPtr2) {
        IntList result = *lsPtr1;
        *lsPtr1 = NULL;
        return result;
    }

    IntList result =  *lsPtr1;
    IntList node   =   result;
    int     turn   =   1;
           *lsPtr1 = (*lsPtr1)->next;

    while (*lsPtr1 && *lsPtr2) {
        if (turn == 1) {
            node->next = *lsPtr2;
            *lsPtr2 = (*lsPtr2)->next;
            turn = 0;
        } else {
            node->next = *lsPtr1;
            *lsPtr1 = (*lsPtr1)->next;
            turn = 1;
        }
        node = node->next;
    }


    // se uno dei due e' null, allora aggiunge alla lista tutti gli elementi rimanenti dell'altro
    if (*lsPtr1) {
        node->next = *lsPtr1;
        *lsPtr1    = NULL;
    } else if (*lsPtr2) {
        node->next = *lsPtr2;
        *lsPtr2    = NULL;
    }
        
    
    return result;  
}
