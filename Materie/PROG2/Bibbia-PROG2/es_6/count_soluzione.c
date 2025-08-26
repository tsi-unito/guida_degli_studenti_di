
#include "count.h"

/*
typedef struct treeNode CharTreeNode, *CharTree;
struct treeNode {
        CharTree left;
        char data;
        CharTree right;
}
*/

int countInRange(CharTree node, int currentDepth, int m, int n) {
  if (!node)
    return 0;

  int nodeCount = 0;

  if (currentDepth >= m && currentDepth <= n) {
    nodeCount = 1;
  }

  if (currentDepth < n) {
    nodeCount += countInRange(node->left, currentDepth + 1, m, n);
    nodeCount += countInRange(node->right, currentDepth + 1, m, n);
  }

  return nodeCount;
}

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
int count(CharTree tree, int m, int n) {
  if (!tree) return 0;
  if (m < 0) m = 0;
  return countInRange(tree, 0, m, n);
}
