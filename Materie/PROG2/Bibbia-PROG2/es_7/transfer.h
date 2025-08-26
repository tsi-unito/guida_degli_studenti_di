#ifndef TRANSFER_H
#define TRANSFER_H

#include <stdlib.h>
#include <stdio.h>

typedef struct node IntNode, *IntList;

struct node {
    int     data; 
    IntList next;
};

/**
 * @brief Modifica *lsPtr1 togliendo da *lsPtr1 tutti i nodi che occorrono
 * in ls2, e li restituisce in una nuova lista nello stesso ordine in cui
 * occorrono in *lsPtr1 (NON ALLOCA NUOVA MEMORIA).
 * ESEMPIO:
 * date [1, 2, 5, 3, 4, 5, 9, 8] e [7, 5, 2, 4]
 * modifica la prima lista in [1, 3, 9, 8] e restituisce [2, 5, 4, 5].
 */
IntList transfer(IntList *lsPtr1, IntList ls2);

// Funzioni di utilità per i test (implementate inline nel .h)

// Crea un nuovo nodo
static inline IntList __createNode(int data) {
    IntList node = (IntList)malloc(sizeof(IntNode));
    if (node) {
        node->data = data;
        node->next = NULL;
    }
    return node;
}

// Crea una lista da un array di valori
static inline IntList __createList(int *values, int size) {
    if (size <= 0) return NULL;
    
    IntList head = __createNode(values[0]);
    IntList current = head;
    
    for (int i = 1; i < size; i++) {
        current->next = __createNode(values[i]);
        current = current->next;
    }
    
    return head;
}

// Stampa una lista
static inline void __printList(IntList list, const char *name) {
    printf("%s: [", name);
    IntList current = list;
    while (current) {
        printf("%d", current->data);
        if (current->next) printf(", ");
        current = current->next;
    }
    printf("]\n");
}

// Libera la memoria di una lista
static inline void __freeList(IntList list) {
    while (list) {
        IntList temp = list;
        list = list->next;
        free(temp);
    }
}

// Calcola la lunghezza di una lista
static inline int __listLength(IntList list) {
    int count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

// Converte una lista in array
static inline int* __listToArray(IntList list, int *size) {
    *size = __listLength(list);
    if (*size == 0) return NULL;
    
    int *array = (int*)malloc(*size * sizeof(int));
    IntList current = list;
    for (int i = 0; i < *size; i++) {
        array[i] = current->data;
        current = current->next;
    }
    return array;
}

// Confronta due array
static inline int __compareArrays(int *arr1, int size1, int *arr2, int size2) {
    if (size1 != size2) return 0;
    for (int i = 0; i < size1; i++) {
        if (arr1[i] != arr2[i]) return 0;
    }
    return 1;
}

// Copia una lista (alloca nuova memoria per i test)
static inline IntList __copyList(IntList list) {
    if (!list) return NULL;
    
    IntList newHead = __createNode(list->data);
    IntList newCurrent = newHead;
    IntList original = list->next;
    
    while (original) {
        newCurrent->next = __createNode(original->data);
        newCurrent = newCurrent->next;
        original = original->next;
    }
    
    return newHead;
}

// Verifica se un elemento è presente in una lista
static inline int __isInList(IntList list, int value) {
    while (list) {
        if (list->data == value) return 1;
        list = list->next;
    }
    return 0;
}

#include <stdio.h>
#include <stdlib.h>

#endif // TRANSFER_H
