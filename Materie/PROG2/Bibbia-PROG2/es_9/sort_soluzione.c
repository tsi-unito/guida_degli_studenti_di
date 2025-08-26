
#include "sort.h"

/*

typedef struct treeNode Int TreeNode, *Int Tree;

struct treeNode {

Int Tree left;
	int data;
	IntTree right;
}
*/
/**
 * @brief Trasforma un albero, agendo su ogni nodo con entrambi i rami,
 * scambiandoli se le loro radici non sono nell'ordine corretto 
 * (ovvero lo scambio avviene quando sinistra > destra).
 */
void sort (IntTree tree) {
  if (!tree || (!tree->left && !tree->right)) return;

  if (!tree->left)
    tree->left = tree->right;
  
  if (tree->left && tree->right && tree->left->data > tree->right->data) {
    IntTree tmp = tree->left;
    tree->left = tree->right;
    tree->right = tmp;
  }


  sort(tree->left);
  sort(tree->right);
}
