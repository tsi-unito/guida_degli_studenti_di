#ifndef COUNT_H
#define COUNT_H

typedef struct treeNode CharTreeNode, *CharTree;
struct treeNode {
    CharTree left;
    char data;
    CharTree right;
};

/**
 * @brief Restituisce il numero di nodi dell'albero binario tree di
 * profondità compresa tra m e n
 * (si ricorda che il nodo radice ha profondità 0).
 * 
 * ESEMPIO: dato  
    R
   /\  
  F  Z
 /\
D  H
  /\
 G  L
   /\
  I  M
 * 
 * e gli interi -2 e 3 restituisce 7.
 */
int count(CharTree tree, int m, int n);

// Funzioni di utilità per i test (implementate inline)
#include <stdio.h>
#include <stdlib.h>

// Crea un nuovo nodo
static inline CharTree __createNode(char data) {
    CharTree node = (CharTree)malloc(sizeof(CharTreeNode));
    if (node) {
        node->data = data;
        node->left = NULL;
        node->right = NULL;
    }
    return node;
}

// Libera l'albero
static inline void __freeTree(CharTree tree) {
    if (tree) {
        __freeTree(tree->left);
        __freeTree(tree->right);
        free(tree);
    }
}

// Stampa l'albero (in-order traversal)
static inline void __printTreeInOrder(CharTree tree) {
    if (tree) {
        __printTreeInOrder(tree->left);
        printf("%c ", tree->data);
        __printTreeInOrder(tree->right);
    }
}

// Stampa l'albero con indentazione per mostrare la struttura
static inline void __printTreeStructure(CharTree tree, int depth, char prefix) {
    if (tree) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("%c%c (depth %d)\n", prefix, tree->data, depth);
        if (tree->left || tree->right) {
            if (tree->left) {
                __printTreeStructure(tree->left, depth + 1, 'L');
            } else {
                for (int i = 0; i <= depth; i++) printf("  ");
                printf("L- (null)\n");
            }
            if (tree->right) {
                __printTreeStructure(tree->right, depth + 1, 'R');
            } else {
                for (int i = 0; i <= depth; i++) printf("  ");
                printf("R- (null)\n");
            }
        }
    }
}

// Conta tutti i nodi (per verifica)
static inline int __countAllNodes(CharTree tree) {
    if (!tree) return 0;
    return 1 + __countAllNodes(tree->left) + __countAllNodes(tree->right);
}

// Calcola l'altezza dell'albero
static inline int __treeHeight(CharTree tree) {
    if (!tree) return -1;
    int leftHeight = __treeHeight(tree->left);
    int rightHeight = __treeHeight(tree->right);
    return 1 + (leftHeight > rightHeight ? leftHeight : rightHeight);
}

// Crea l'albero dell'esempio
static inline CharTree __createExampleTree() {
    /*
        R (depth 0)
       /\  
      F  Z (depth 1)
     /\
    D  H (depth 2)
      /\
     G  L (depth 3)
       /\
      I  M (depth 4)
    */
    CharTree root = __createNode('R');
    
    root->left = __createNode('F');
    root->right = __createNode('Z');
    
    root->left->left = __createNode('D');
    root->left->right = __createNode('H');
    
    root->left->right->left = __createNode('G');
    root->left->right->right = __createNode('L');
    
    root->left->right->right->left = __createNode('I');
    root->left->right->right->right = __createNode('M');
    
    return root;
}

// Crea un albero bilanciato semplice
static inline CharTree __createSimpleTree() {
    /*
       A (depth 0)
      /\
     B  C (depth 1)
    /\  /\
   D E F G (depth 2)
    */
    CharTree root = __createNode('A');
    
    root->left = __createNode('B');
    root->right = __createNode('C');
    
    root->left->left = __createNode('D');
    root->left->right = __createNode('E');
    root->right->left = __createNode('F');
    root->right->right = __createNode('G');
    
    return root;
}

// Crea un albero lineare (solo figli destri)
static inline CharTree __createLinearTree() {
    /*
    A (depth 0)
     \
      B (depth 1)
       \
        C (depth 2)
         \
          D (depth 3)
    */
    CharTree root = __createNode('A');
    root->right = __createNode('B');
    root->right->right = __createNode('C');
    root->right->right->right = __createNode('D');
    
    return root;
}

// Crea un albero con un solo nodo
static inline CharTree __createSingleNodeTree() {
    return __createNode('X');
}

#endif // COUNT_H
