#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "transfer.h"

// Struttura per i test
typedef struct {
    int *list1_values;
    int list1_size;
    int *list2_values;
    int list2_size;
    int *expected_remaining_values;
    int expected_remaining_size;
    int *expected_transferred_values;
    int expected_transferred_size;
    const char *description;
} TestCase;

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, int passed) {
    printf("Test %2d: %-55s [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

// Funzione per eseguire un singolo test
int runTest(TestCase *test) {
    // Crea le liste
    IntList list1 = __createList(test->list1_values, test->list1_size);
    IntList list2 = __createList(test->list2_values, test->list2_size);
    IntList list1_original = __copyList(list1); // Per stampare l'input originale
    
    printf("         Input:  ");
    __printList(list1_original, "List1");
    printf("                 ");
    __printList(list2, "List2");
    
    // Esegui la funzione
    IntList transferred = transfer(&list1, list2);
    
    printf("         Output: ");
    __printList(list1, "Remaining");
    printf("                 ");
    __printList(transferred, "Transferred");
    
    // Converti i risultati in array per il confronto
    int remaining_size, transferred_size;
    int *remaining_array = __listToArray(list1, &remaining_size);
    int *transferred_array = __listToArray(transferred, &transferred_size);
    
    // Verifica i risultati
    int correct_remaining = __compareArrays(remaining_array, remaining_size, 
                                           test->expected_remaining_values, test->expected_remaining_size);
    int correct_transferred = __compareArrays(transferred_array, transferred_size, 
                                             test->expected_transferred_values, test->expected_transferred_size);
    
    printf("         Expected Remaining: [");
    for (int i = 0; i < test->expected_remaining_size; i++) {
        printf("%d", test->expected_remaining_values[i]);
        if (i < test->expected_remaining_size - 1) printf(", ");
    }
    printf("]\n");
    
    printf("         Expected Transferred: [");
    for (int i = 0; i < test->expected_transferred_size; i++) {
        printf("%d", test->expected_transferred_values[i]);
        if (i < test->expected_transferred_size - 1) printf(", ");
    }
    printf("]\n");
    
    int passed = correct_remaining && correct_transferred;
    
    if (!correct_remaining) {
        printf("         ERROR: La lista rimanente non corrisponde!\n");
    }
    if (!correct_transferred) {
        printf("         ERROR: La lista trasferita non corrisponde!\n");
    }
    
    // Cleanup
    __freeList(list1);
    __freeList(list2);
    __freeList(list1_original);
    __freeList(transferred);
    free(remaining_array);
    free(transferred_array);
    
    return passed;
}

int main() {
    printf("=== TEST SUITE per transfer ===\n\n");
    
    // Test cases
    int test1_list1[] = {1, 2, 5, 3, 4, 5, 9, 8};
    int test1_list2[] = {7, 5, 2, 4};
    int test1_remaining[] = {1, 3, 9, 8};
    int test1_transferred[] = {2, 5, 4, 5};
    
    int test2_list1[] = {1, 2, 3, 4, 5};
    int test2_list2[] = {2, 4};
    int test2_remaining[] = {1, 3, 5};
    int test2_transferred[] = {2, 4};
    
    int test3_list1[] = {10, 20, 30, 40};
    int test3_list2[] = {15, 25, 35};
    int test3_remaining[] = {10, 20, 30, 40};
    int test3_transferred[] = {};
    
    int test4_list1[] = {1, 1, 2, 2, 3, 3};
    int test4_list2[] = {1, 3};
    int test4_remaining[] = {2, 2};
    int test4_transferred[] = {1, 1, 3, 3};
    
    int test5_list1[] = {5, 4, 3, 2, 1};
    int test5_list2[] = {1, 2, 3, 4, 5};
    int test5_remaining[] = {};
    int test5_transferred[] = {5, 4, 3, 2, 1};
    
    int test6_list1[] = {7};
    int test6_list2[] = {7, 8, 9};
    int test6_remaining[] = {};
    int test6_transferred[] = {7};
    
    int test7_list1[] = {1, 3, 5, 7};
    int test7_list2[] = {2, 4, 6, 8};
    int test7_remaining[] = {1, 3, 5, 7};
    int test7_transferred[] = {};
    
    int test8_list1[] = {100, 200, 100, 300, 200};
    int test8_list2[] = {100, 300};
    int test8_remaining[] = {200, 200};
    int test8_transferred[] = {100, 100, 300};
    
    int test9_list1[] = {42};
    int test9_list2[] = {24};
    int test9_remaining[] = {42};
    int test9_transferred[] = {};
    
    TestCase tests[] = {
        {test1_list1, 8, test1_list2, 4, test1_remaining, 4, test1_transferred, 4,
         "Esempio base: [1,2,5,3,4,5,9,8] - [7,5,2,4]"},
        
        {test2_list1, 5, test2_list2, 2, test2_remaining, 3, test2_transferred, 2,
         "Caso semplice: [1,2,3,4,5] - [2,4]"},
        
        {test3_list1, 4, test3_list2, 3, test3_remaining, 4, test3_transferred, 0,
         "Nessun elemento da trasferire: [10,20,30,40] - [15,25,35]"},
        
        {test4_list1, 6, test4_list2, 2, test4_remaining, 2, test4_transferred, 4,
         "Elementi duplicati: [1,1,2,2,3,3] - [1,3]"},
        
        {test5_list1, 5, test5_list2, 5, test5_remaining, 0, test5_transferred, 5,
         "Tutti elementi trasferiti: [5,4,3,2,1] - [1,2,3,4,5]"},
        
        {test6_list1, 1, test6_list2, 3, test6_remaining, 0, test6_transferred, 1,
         "Lista con un elemento: [7] - [7,8,9]"},
        
        {test7_list1, 4, test7_list2, 4, test7_remaining, 4, test7_transferred, 0,
         "Nessuna intersezione: [1,3,5,7] - [2,4,6,8]"},
        
        {test8_list1, 5, test8_list2, 2, test8_remaining, 2, test8_transferred, 3,
         "Duplicati misti: [100,200,100,300,200] - [100,300]"},
        
        {test9_list1, 1, test9_list2, 1, test9_remaining, 1, test9_transferred, 0,
         "Singoli elementi diversi: [42] - [24]"},
        
        // Test con liste vuote
        {NULL, 0, test1_list2, 4, NULL, 0, NULL, 0,
         "Lista vuota: [] - [7,5,2,4]"},
        
        {test1_list1, 8, NULL, 0, test1_list1, 8, NULL, 0,
         "Lista di ricerca vuota: [1,2,5,3,4,5,9,8] - []"}
    };
    
    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;
    
    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        printf("--------------------------------------------------------\n");
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
