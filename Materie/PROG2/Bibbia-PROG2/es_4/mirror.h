#ifndef MIRROR_H
#define MIRROR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct treeNode IntTreeNode, *IntTree;
struct treeNode {
    IntTree left;
    int data;
    IntTree right;
};

/**
 * @brief Trasforma un albero nella sua versione speculare.
 * @param tree: albero da specchiare
 *
 *    2           2
 *   /\    =>    /\
 *  1  5        5  1
 *     /\      /\
 *    4  9    9  4
 */
void mirror(IntTree tree);

// Funzioni ausiliarie per i test (implementate inline con prefisso __)
static inline IntTree __createNode(int data) {
    IntTree node = (IntTree)malloc(sizeof(IntTreeNode));
    if (node) {
        node->data = data;
        node->left = NULL;
        node->right = NULL;
    }
    return node;
}

static inline void __freeTree(IntTree tree) {
    if (tree) {
        __freeTree(tree->left);
        __freeTree(tree->right);
        free(tree);
    }
}

static inline void __printTreeInorder(IntTree tree) {
    if (tree) {
        __printTreeInorder(tree->left);
        printf("%d ", tree->data);
        __printTreeInorder(tree->right);
    }
}

static inline void __printTreePreorder(IntTree tree) {
    if (tree) {
        printf("%d ", tree->data);
        __printTreePreorder(tree->left);
        __printTreePreorder(tree->right);
    }
}

static inline void __printTreeStructure(IntTree tree, int level, char side) {
    if (tree) {
        __printTreeStructure(tree->right, level + 1, '/');
        for (int i = 0; i < level; i++) printf("    ");
        printf("%c%d\n", side, tree->data);
        __printTreeStructure(tree->left, level + 1, '\\');
    }
}

static inline IntTree __buildSampleTree1() {
    /*
        2
       / \
      1   5
         / \
        4   9
    */
    IntTree root = __createNode(2);
    root->left = __createNode(1);
    root->right = __createNode(5);
    root->right->left = __createNode(4);
    root->right->right = __createNode(9);
    return root;
}

static inline IntTree __buildSampleTree2() {
    /*
        10
       /  \
      5    15
     / \   / \
    3   7 12  20
   /         \
  1          25
    */
    IntTree root = __createNode(10);
    root->left = __createNode(5);
    root->right = __createNode(15);
    root->left->left = __createNode(3);
    root->left->right = __createNode(7);
    root->right->left = __createNode(12);
    root->right->right = __createNode(20);
    root->left->left->left = __createNode(1);
    root->right->right->right = __createNode(25);
    return root;
}

static inline IntTree __buildLinearTree() {
    /*
      1
       \
        2
         \
          3
           \
            4
    */
    IntTree root = __createNode(1);
    root->right = __createNode(2);
    root->right->right = __createNode(3);
    root->right->right->right = __createNode(4);
    return root;
}

static inline IntTree __buildCompleteTree() {
    /*
        1
       / \
      2   3
     / \ / \
    4  5 6  7
    */
    IntTree root = __createNode(1);
    root->left = __createNode(2);
    root->right = __createNode(3);
    root->left->left = __createNode(4);
    root->left->right = __createNode(5);
    root->right->left = __createNode(6);
    root->right->right = __createNode(7);
    return root;
}

static inline bool __compareTreesStructure(IntTree tree1, IntTree tree2) {
    if (!tree1 && !tree2) return true;
    if (!tree1 || !tree2) return false;
    
    return (tree1->data == tree2->data) &&
           __compareTreesStructure(tree1->left, tree2->left) &&
           __compareTreesStructure(tree1->right, tree2->right);
}

static inline IntTree __copyTree(IntTree tree) {
    if (!tree) return NULL;
    
    IntTree newNode = __createNode(tree->data);
    newNode->left = __copyTree(tree->left);
    newNode->right = __copyTree(tree->right);
    return newNode;
}

static inline void __getPreorderArray(IntTree tree, int *array, int *index) {
    if (tree) {
        array[(*index)++] = tree->data;
        __getPreorderArray(tree->left, array, index);
        __getPreorderArray(tree->right, array, index);
    }
}

static inline int __countNodes(IntTree tree) {
    if (!tree) return 0;
    return 1 + __countNodes(tree->left) + __countNodes(tree->right);
}

#endif // MIRROR_H
