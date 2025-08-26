#ifndef MIXALTERNATE_H
#define MIXALTERNATE_H

#include <stdio.h>
#include <stdlib.h>

typedef struct node IntNode, *IntList;
struct node {
    int data;
    IntList next;
};

/**
 * @brief Restituisce la lista alternata dei nodi di *lsPtr1 e *lsPtr2,
 * togliendoli da *lsPtr1 e *lsPtr2, 
 * che alla fine conterranno entrambi NULL (non alloca nuova memoria). 
 * 
 * Ad es. date [1, 5, 9] e [0, 2, 4, 6, 8] restituisce
 * [1, 0, 5, 2, 9, 4, 6, 8].
 */
IntList mixAlternate(IntList *lsPtr1, IntList *lsPtr2);

// Funzioni di utilità per i test (implementate inline)
static inline IntList __createNode(int data) {
    IntList node = (IntList)malloc(sizeof(IntNode));
    if (node) {
        node->data = data;
        node->next = NULL;
    }
    return node;
}

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

static inline void __freeList(IntList list) {
    while (list) {
        IntList temp = list;
        list = list->next;
        free(temp);
    }
}

static inline int __listLength(IntList list) {
    int count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

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

static inline int __compareArrays(int *arr1, int size1, int *arr2, int size2) {
    if (size1 != size2) return 0;
    for (int i = 0; i < size1; i++) {
        if (arr1[i] != arr2[i]) return 0;
    }
    return 1;
}

#endif // MIXALTERNATE_H
