#include <stdio.h>
#include <stdlib.h>
#include "count.h"

// Struttura per i test
typedef struct {
    CharTree (*createTree)();
    int m;
    int n;
    int expected;
    const char *description;
    const char *treeName;
} TestCase;

// Funzione per creare un albero NULL (definita fuori dal main)
CharTree __createNullTree() {
    return NULL;
}

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, int passed) {
    printf("Test %2d: %-60s [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

// Funzione per eseguire un singolo test
int runTest(TestCase *test) {
    // Crea l'albero
    CharTree tree = test->createTree();
    
    printf("         Albero: %s\n", test->treeName);
    printf("         Struttura:\n");
    if (tree) {
        __printTreeStructure(tree, 0, 'R');
    } else {
        printf("         (albero vuoto)\n");
    }
    
    printf("         Range profondità: [%d, %d]\n", test->m, test->n);
    printf("         Nodi totali: %d, Altezza: %d\n", 
           __countAllNodes(tree), __treeHeight(tree));
    
    // Esegui la funzione
    int result = count(tree, test->m, test->n);
    
    printf("         Risultato: %d, Atteso: %d\n", result, test->expected);
    
    int passed = (result == test->expected);
    
    // Cleanup
    __freeTree(tree);
    
    return passed;
}

int main() {
    printf("=== TEST SUITE per count ===\n\n");
    
    TestCase tests[] = {
        // Test con l'albero dell'esempio
        {__createExampleTree, -2, 3, 7, 
         "Esempio del problema: m=-2, n=3 (dovrebbe essere 7)", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 0, 2, 5, 
         "Albero esempio: profondità 0-2", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 1, 3, 6, 
         "Albero esempio: profondità 1-3", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 2, 4, 6, 
         "Albero esempio: profondità 2-4", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 0, 0, 1, 
         "Albero esempio: solo radice (profondità 0)", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 4, 4, 2, 
         "Albero esempio: solo profondità 4", 
         "Albero dell'esempio"},
        
        {__createExampleTree, 5, 10, 0, 
         "Albero esempio: profondità inesistenti (5-10)", 
         "Albero dell'esempio"},
        
        // Test con albero bilanciato
        {__createSimpleTree, 0, 2, 7, 
         "Albero bilanciato: tutte le profondità (0-2)", 
         "Albero bilanciato"},
        
        {__createSimpleTree, 1, 1, 2, 
         "Albero bilanciato: solo profondità 1", 
         "Albero bilanciato"},
        
        {__createSimpleTree, 2, 2, 4, 
         "Albero bilanciato: solo profondità 2", 
         "Albero bilanciato"},
        
        {__createSimpleTree, 0, 1, 3, 
         "Albero bilanciato: profondità 0-1", 
         "Albero bilanciato"},
        
        // Test con albero lineare
        {__createLinearTree, 0, 3, 4, 
         "Albero lineare: tutte le profondità (0-3)", 
         "Albero lineare"},
        
        {__createLinearTree, 1, 2, 2, 
         "Albero lineare: profondità 1-2", 
         "Albero lineare"},
        
        {__createLinearTree, 3, 3, 1, 
         "Albero lineare: solo profondità 3", 
         "Albero lineare"},
        
        {__createLinearTree, 4, 5, 0, 
         "Albero lineare: profondità inesistenti (4-5)", 
         "Albero lineare"},
        
        // Test con singolo nodo
        {__createSingleNodeTree, 0, 0, 1, 
         "Singolo nodo: profondità 0", 
         "Singolo nodo"},
        
        {__createSingleNodeTree, -1, 1, 1, 
         "Singolo nodo: range che include 0", 
         "Singolo nodo"},
        
        {__createSingleNodeTree, 1, 5, 0, 
         "Singolo nodo: profondità inesistenti (1-5)", 
         "Singolo nodo"},
        
        // Test con range negativi
        {__createExampleTree, -5, -1, 0, 
         "Albero esempio: range tutto negativo (-5, -1)", 
         "Albero dell'esempio"},
        
        {__createSimpleTree, -1, 1, 3, 
         "Albero bilanciato: range che inizia negativo (-1, 1)", 
         "Albero bilanciato"},
        
        // Test con range invertiti (m > n)
        {__createExampleTree, 3, 1, 0, 
         "Albero esempio: range invertito (m=3, n=1)", 
         "Albero dell'esempio"},
        
        // Test con albero NULL
        {__createNullTree, 0, 5, 0, 
         "Albero vuoto: qualsiasi range", 
         "Albero vuoto"}
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
