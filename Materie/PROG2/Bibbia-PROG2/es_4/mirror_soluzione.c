#include <stdlib.h>
#include "mirror.h"


/*
def della struttura nel .h

typedef struct treeNode IntTreeNode, *IntTree;
struct treeNode {
    IntTree left;
    int data;
    IntTree right;
};
*/

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
void mirror(IntTree tree) {
  if (!tree) return;

  if (!tree->left && !tree->right)
    return;
  
  IntTree tmp = tree->right;
  tree->right = tree->left;
  tree->left  = tmp;
  

  mirror(tree->left );
  mirror(tree->right);
}
