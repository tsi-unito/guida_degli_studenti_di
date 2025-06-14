#ifndef SORT_H
#define SORT_H

typedef struct treeNode IntTreeNode, *IntTree;

struct treeNode {
    IntTree left;
    int data;
    IntTree right;
};

/**
 * @brief Trasforma un albero, agendo su ogni nodo con entrambi i rami,
 * scambiandoli se le loro radici non sono nell'ordine corretto 
 * (ovvero lo scambio avviene quando sinistra > destra).
 */
void sort(IntTree tree);

// Funzioni di utilità per i test (implementate inline)
#include <stdio.h>
#include <stdlib.h>

// Crea un nuovo nodo
static inline IntTree __createNode(int data) {
    IntTree node = (IntTree)malloc(sizeof(IntTreeNode));
    if (node) {
        node->data = data;
        node->left = NULL;
        node->right = NULL;
    }
    return node;
}

// Libera la memoria dell'albero
static inline void __freeTree(IntTree tree) {
    if (tree) {
        __freeTree(tree->left);
        __freeTree(tree->right);
        free(tree);
    }
}

// Stampa l'albero in ordine (inorder traversal)
static inline void __printInorder(IntTree tree) {
    if (tree) {
        __printInorder(tree->left);
        printf("%d ", tree->data);
        __printInorder(tree->right);
    }
}

// Stampa l'albero in preordine (preorder traversal)
static inline void __printPreorder(IntTree tree) {
    if (tree) {
        printf("%d ", tree->data);
        __printPreorder(tree->left);
        __printPreorder(tree->right);
    }
}

// Stampa l'albero con struttura visuale
static inline void __printTreeStructure(IntTree tree, int level, char prefix) {
    if (tree) {
        for (int i = 0; i < level; i++) printf("  ");
        printf("%c%d\n", prefix, tree->data);
        if (tree->left || tree->right) {
            if (tree->left) {
                __printTreeStructure(tree->left, level + 1, 'L');
            } else {
                for (int i = 0; i <= level; i++) printf("  ");
                printf("L(null)\n");
            }
            if (tree->right) {
                __printTreeStructure(tree->right, level + 1, 'R');
            } else {
                for (int i = 0; i <= level; i++) printf("  ");
                printf("R(null)\n");
            }
        }
    }
}

// Copia un albero
static inline IntTree __copyTree(IntTree tree) {
    if (!tree) return NULL;
    
    IntTree newNode = __createNode(tree->data);
    newNode->left = __copyTree(tree->left);
    newNode->right = __copyTree(tree->right);
    return newNode;
}

// Confronta due alberi per struttura e valori
static inline int __compareTrees(IntTree tree1, IntTree tree2) {
    if (!tree1 && !tree2) return 1;
    if (!tree1 || !tree2) return 0;
    
    return (tree1->data == tree2->data) &&
           __compareTrees(tree1->left, tree2->left) &&
           __compareTrees(tree1->right, tree2->right);
}

// Conta i nodi dell'albero
static inline int __countNodes(IntTree tree) {
    if (!tree) return 0;
    return 1 + __countNodes(tree->left) + __countNodes(tree->right);
}

// Verifica se un albero rispetta la proprietà di ordinamento locale
// (per ogni nodo, left->data <= data <= right->data se entrambi esistono)
static inline int __isLocallySorted(IntTree tree) {
    if (!tree) return 1;
    
    int valid = 1;
    
    // Controlla il nodo corrente
    if (tree->left && tree->right) {
        if (tree->left->data > tree->right->data) {
            valid = 0;
        }
    }
    
    // Controlla ricorsivamente i sottoalberi
    return valid && __isLocallySorted(tree->left) && __isLocallySorted(tree->right);
}

// Converte l'albero in array (preorder) per facilitare i test
static inline void __treeToArray(IntTree tree, int *array, int *index) {
    if (tree) {
        array[(*index)++] = tree->data;
        __treeToArray(tree->left, array, index);
        __treeToArray(tree->right, array, index);
    }
}

// Crea un albero da array (formato: valore, left_size, right_size)
static inline IntTree __createTestTree1() {
    // Albero:     5
    //           /   \
    //          8     3
    //         / \   / \
    //        2   7 1   6
    IntTree root = __createNode(5);
    root->left = __createNode(8);
    root->right = __createNode(3);
    root->left->left = __createNode(2);
    root->left->right = __createNode(7);
    root->right->left = __createNode(1);
    root->right->right = __createNode(6);
    return root;
}

static inline IntTree __createTestTree2() {
    // Albero:     10
    //           /    \
    //          15     5
    //         /      / \
    //        20     2   8
    IntTree root = __createNode(10);
    root->left = __createNode(15);
    root->right = __createNode(5);
    root->left->left = __createNode(20);
    root->right->left = __createNode(2);
    root->right->right = __createNode(8);
    return root;
}

static inline IntTree __createTestTree3() {
    // Albero semplice:  3
    //                  / \
    //                 7   1
    IntTree root = __createNode(3);
    root->left = __createNode(7);
    root->right = __createNode(1);
    return root;
}

static inline IntTree __createTestTree4() {
    // Albero già ordinato: 1
    //                     / \
    //                    2   3
    IntTree root = __createNode(1);
    root->left = __createNode(2);
    root->right = __createNode(3);
    return root;
}

static inline IntTree __createTestTree5() {
    // Albero solo radice
    IntTree root = __createNode(42);
    return root;
}

#endif // SORT_H
