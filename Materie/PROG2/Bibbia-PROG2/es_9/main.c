#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sort.h"

// Struttura per i test
typedef struct {
    IntTree (*createTree)(void);
    IntTree (*createExpected)(void);
    const char *description;
    const char *treeName;
} TestCase;

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, int passed) {
    printf("Test %2d: %-50s [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

// Funzione per creare l'albero atteso per il test 1
IntTree createExpectedTree1() {
    // Dopo sort:  5
    //           /   \
    //          3     8  (scambiati)
    //         / \   / \
    //        1   6 2   7 (scambiati)
    IntTree root = __createNode(5);
    root->left = __createNode(3);
    root->right = __createNode(8);
    root->left->left = __createNode(1);
    root->left->right = __createNode(6);
    root->right->left = __createNode(2);
    root->right->right = __createNode(7);
    return root;
}

// Funzione per creare l'albero atteso per il test 2
IntTree createExpectedTree2() {
    // Dopo sort:  10
    //           /    \
    //          5     15  (scambiati)
    //         /      /
    //        2      20   (2 e 8 scambiati)
    //        \
    //         8
    IntTree root = __createNode(10);
    root->left = __createNode(5);
    root->right = __createNode(15);
    root->left->left = __createNode(2);
    root->left->right = __createNode(8);
    root->right->left = __createNode(20);
    return root;
}

// Funzione per creare l'albero atteso per il test 3
IntTree createExpectedTree3() {
    // Dopo sort:  3
    //           /   \
    //          1     7  (scambiati)
    IntTree root = __createNode(3);
    root->left = __createNode(1);
    root->right = __createNode(7);
    return root;
}

// Funzioni wrapper per i test
IntTree createTree1() { return __createTestTree1(); }
IntTree createTree2() { return __createTestTree2(); }
IntTree createTree3() { return __createTestTree3(); }
IntTree createTree4() { return __createTestTree4(); }
IntTree createTree5() { return __createTestTree5(); }
IntTree createExpected4() { return __createTestTree4(); } // Già ordinato
IntTree createExpected5() { return __createTestTree5(); } // Solo radice
IntTree createNullTree() { return NULL; }

// Funzione per eseguire un singolo test
int runTest(TestCase *test) {
    printf("         Descrizione: %s\n", test->description);
    
    // Crea l'albero di test e quello atteso
    IntTree tree = test->createTree();
    IntTree expected = test->createExpected();
    
    printf("\n         Albero PRIMA del sort (%s):\n", test->treeName);
    if (tree) {
        __printTreeStructure(tree, 0, 'R');
    } else {
        printf("         (albero vuoto)\n");
    }
    
    printf("\n         Preorder PRIMA: ");
    __printPreorder(tree);
    printf("\n");
    
    // Esegui la funzione sort
    sort(tree);
    
    printf("\n         Albero DOPO il sort:\n");
    if (tree) {
        __printTreeStructure(tree, 0, 'R');
    } else {
        printf("         (albero vuoto)\n");
    }
    
    printf("\n         Preorder DOPO: ");
    __printPreorder(tree);
    printf("\n");
    
    printf("\n         Albero ATTESO:\n");
    if (expected) {
        __printTreeStructure(expected, 0, 'R');
    } else {
        printf("         (albero vuoto)\n");
    }
    
    printf("\n         Preorder ATTESO: ");
    __printPreorder(expected);
    printf("\n");
    
    // Verifica il risultato
    int passed = __compareTrees(tree, expected);
    
    // Verifica aggiuntiva: l'albero deve essere localmente ordinato
    int locallySorted = __isLocallySorted(tree);
    
    if (!locallySorted) {
        printf("         ERRORE: L'albero non rispetta l'ordinamento locale!\n");
        passed = 0;
    }
    
    printf("\n         Ordinamento locale: %s\n", locallySorted ? "✓" : "✗");
    printf("         Struttura corretta: %s\n", passed ? "✓" : "✗");
    
    // Cleanup
    __freeTree(tree);
    __freeTree(expected);
    
    return passed;
}

int main() {
    printf("=== TEST SUITE per sort ===\n\n");
    
    TestCase tests[] = {
        {createTree1, createExpectedTree1, 
         "Albero completo con scambi multipli", "Tree1"},
        
        {createTree2, createExpectedTree2, 
         "Albero con scambi a livelli diversi", "Tree2"},
        
        {createTree3, createExpectedTree3, 
         "Albero semplice con un scambio", "Tree3"},
        
        {createTree4, createExpected4, 
         "Albero già ordinato (nessun scambio)", "Tree4"},
        
        {createTree5, createExpected5, 
         "Albero con solo radice", "Tree5"},
        
        {createNullTree, createNullTree, 
         "Albero vuoto (NULL)", "NullTree"}
    };
    
    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;
    
    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        printf("========================================\n");
        int passed = runTest(&tests[i]);
        printTestResult(i + 1, tests[i].description, passed);
        
        if (passed) {
            passedTests++;
        }
        printf("\n");
    }
    
    // Risultato finale
    printf("==========================================\n");
    printf("RISULTATO FINALE: %d test passati su %d\n", passedTests, totalTests);
    printf("Percentuale di successo: %.1f%%\n", (float)passedTests / totalTests * 100);
    printf("==========================================\n");
    
    if (passedTests == totalTests) {
        printf("🎉 TUTTI I TEST SONO PASSATI! 🎉\n");
        return 0;
    } else {
        printf("❌ Alcuni test sono falliti.\n");
        return 1;
    }
}
