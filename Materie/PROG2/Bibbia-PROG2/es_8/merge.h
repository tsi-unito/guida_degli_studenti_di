#ifndef MERGE_H
#define MERGE_H

typedef struct node IntNode, *IntList;
struct node {
    int data;
    IntList next;
};

/**
 * @brief Date due liste ordinate *lsPtr1 e *lsPtr2, 
 * restituisce l'unione ordinata delle due
 * Non si può usare malloc, ma bisogna ordinare gli elementi delle liste
 * @return Primo elemento della lista riformattata 
 * alla fine i param saranno entrambi NULL
 * Ad esempio:
 * date [1, 5, 9] e [0, 2, 4, 6, 8] 
 * restituisce [0, 1, 2, 4, 5, 6, 8, 9]
 */
IntList merge(IntList *lsPtr1, IntList *lsPtr2);

// Funzioni di utilità per i test (implementate inline nel .h)

#include <stdio.h>
#include <stdlib.h>

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

static inline int __isListSorted(IntList list) {
    if (!list || !list->next) return 1;
    
    IntList current = list;
    while (current->next) {
        if (current->data > current->next->data) {
            return 0;
        }
        current = current->next;
    }
    return 1;
}

static inline IntList __copyList(IntList original) {
    if (!original) return NULL;
    
    IntList head = __createNode(original->data);
    IntList current = head;
    IntList orig_current = original->next;
    
    while (orig_current) {
        current->next = __createNode(orig_current->data);
        current = current->next;
        orig_current = orig_current->next;
    }
    
    return head;
}

#endif // MERGE_H
