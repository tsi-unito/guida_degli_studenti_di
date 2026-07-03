#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "mirror.h"

// Struttura per i test
typedef struct {
    const char *description;
    IntTree (*buildTree)();
    int *expectedPreorder;
    int expectedSize;
} TestCase;

// Stampa risultato test
void printTestResult(int testNum, const char *description, bool passed) {
    printf("Test %2d: %-50s \t\t\t\t [%s]\n",
           testNum,
           description,
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

bool runTest(TestCase *test, int testNum) {
    printf("\n--- %s ---\n", test->description);

    // Costruisci l'albero
    IntTree tree = test->buildTree();

    // Esegui mirror
    mirror(tree);

    // Ottieni il preorder risultante
    int nodeCount = __countNodes(tree);
    int *actualPreorder = (int*)malloc(nodeCount * sizeof(int));
    int index = 0;
    __getPreorderArray(tree, actualPreorder, &index);

    // Mostra e confronta attuale vs atteso
    printf("Passa (preorder)  : [");
    for (int i = 0; i < nodeCount; i++) {
        printf("%d", actualPreorder[i]);
        if (i < nodeCount - 1) printf(" ");
    }
    printf("]\n");

    printf("Si aspetta        : [");
    for (int i = 0; i < test->expectedSize; i++) {
        printf("%d", test->expectedPreorder[i]);
        if (i < test->expectedSize - 1) printf(" ");
    }
    printf("]\n");

    bool passed = true;
    if (nodeCount != test->expectedSize) {
        passed = false;
        printf("Errore: Numero nodi! Attesi: %d, Ottenuti: %d\n", test->expectedSize, nodeCount);
    } else {
        for (int i = 0; i < test->expectedSize; i++) {
            if (actualPreorder[i] != test->expectedPreorder[i]) {
                passed = false;
                printf("Errore: Posizione %d! Atteso %d, Ottenuto %d\n",
                       i, test->expectedPreorder[i], actualPreorder[i]);
                break;
            }
        }
    }

    // Cleanup
    __freeTree(tree);
    free(actualPreorder);

    printTestResult(testNum, test->description, passed);
    return passed;
}

// Funzioni di creazione
IntTree buildSingleNode() {
    return __createNode(42);
}

IntTree buildEmptyTree() {
    return NULL;
}

int main() {
    printf("=== TEST SUITE mirror (senza struttura) ===\n");

    // Array di risultati attesi
    int expected1[] = {2, 5, 9, 4, 1};
    int expected2[] = {10, 15, 20, 25, 12, 5, 7, 3, 1};
    int expected3[] = {1, 2, 3, 4};
    int expected4[] = {1, 3, 7, 6, 2, 5, 4};
    int expected5[] = {42};

    TestCase tests[] = {
        {"Albero esempio (asimmetrico)", __buildSampleTree1, expected1, 5},
        {"Albero complesso (asimmetrico)", __buildSampleTree2, expected2, 9},
        {"Albero lineare (solo figli destri)", __buildLinearTree, expected3, 4},
        {"Albero completo (simmetrico)", __buildCompleteTree, expected4, 7},
        {"Singolo nodo", buildSingleNode, expected5, 1},
        {"Albero vuoto", buildEmptyTree, NULL, 0}
    };

    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;

    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        if (i == totalTests - 1) {
            // Test speciale per albero vuoto
            printf("\n--- %s ---\n", tests[i].description);
            mirror(NULL); // Non fa nulla
            printf("Passa (preorder)  : []\n");
            printf("Si aspetta        : []\n");
            printTestResult(i + 1, tests[i].description, true);
            passedTests++;
        } else {
            bool passed = runTest(&tests[i], i + 1);
            if (passed) {
                passedTests++;
            }
        }
    }

    // Test aggiuntivo: doppio mirror = originale
    printf("\n--- Test BONUS: Doppio mirror = originale ---\n");
    IntTree originalTree = __buildSampleTree1();
    IntTree copyTree = __copyTree(originalTree);

    mirror(copyTree);
    mirror(copyTree);

    bool bonusTest = __compareTreesStructure(originalTree, copyTree);
    printf("Doppio mirror %s\n", bonusTest ? "PASS" : "FAIL");
    if (bonusTest) {
        passedTests++;
    }
    int totalTestsWithBonus = totalTests + 1;

    __freeTree(originalTree);
    __freeTree(copyTree);

    // Report finale
    printf("\n=== RISULTATO FINALE ===\n");
    printf("Test passati: %d su %d\n", passedTests, totalTestsWithBonus);
    if (passedTests == totalTestsWithBonus) {
        printf("TUTTI I TEST PASSATI!\n");
        return 0;
    }
    printf("Alcuni test sono falliti...\n");
    return 1;
}
