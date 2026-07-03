#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mixAlternate.h"

// Struttura per i test
typedef struct {
    int *list1_values;
    int list1_size;
    int *list2_values;
    int list2_size;
    int *expected_values;
    int expected_size;
    const char *description;
} TestCase;

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, int passed) {
    printf("Test %2d: %-50s \t\t\t [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

// Funzione per eseguire un singolo test
int runTest(TestCase *test) {
    // Crea le liste
    IntList list1 = __createList(test->list1_values, test->list1_size);
    IntList list2 = __createList(test->list2_values, test->list2_size);
    
    printf("         Input:  ");
    __printList(list1, "List1");
    printf("                 ");
    __printList(list2, "List2");
    
    // Esegui la funzione
    IntList result = mixAlternate(&list1, &list2);
    
    printf("         Output: ");
    __printList(result, "Result");
    printf("                 ");
    __printList(list1, "List1 (after)");
    printf("                 ");
    __printList(list2, "List2 (after)");
    
    // Verifica che le liste originali siano vuote
    int lists_empty = (list1 == NULL && list2 == NULL);
    
    // Converti il risultato in array per il confronto
    int result_size;
    int *result_array = __listToArray(result, &result_size);
    
    // Verifica il risultato
    int correct_result = __compareArrays(result_array, result_size, 
                                       test->expected_values, test->expected_size);
    
    printf("         Expected: [");
    for (int i = 0; i < test->expected_size; i++) {
        printf("%d", test->expected_values[i]);
        if (i < test->expected_size - 1) printf(", ");
    }
    printf("]\n");
    
    int passed = correct_result && lists_empty;
    
    if (!lists_empty) {
        printf("         ERROR: Le liste originali non sono vuote!\n");
    }
    if (!correct_result) {
        printf("         ERROR: Il risultato non corrisponde all'atteso!\n");
    }
    
    // Cleanup
    __freeList(result);
    free(result_array);
    
    return passed;
}

int main() {
    printf("=== TEST SUITE per mixAlternate ===\n");
    printf("Author: C4V4H | Date: 2025-06-08\n\n");
    
    // Test cases
    int test1_list1[] = {1, 5, 9};
    int test1_list2[] = {0, 2, 4, 6, 8};
    int test1_expected[] = {1, 0, 5, 2, 9, 4, 6, 8};
    
    int test2_list1[] = {1, 3, 5};
    int test2_list2[] = {2, 4};
    int test2_expected[] = {1, 2, 3, 4, 5};
    
    int test3_list1[] = {10};
    int test3_list2[] = {20, 30, 40};
    int test3_expected[] = {10, 20, 30, 40};
    
    int test4_list1[] = {1, 2, 3, 4};
    int test4_list2[] = {5};
    int test4_expected[] = {1, 5, 2, 3, 4};
    
    int test5_list1[] = {7};
    int test5_list2[] = {8};
    int test5_expected[] = {7, 8};
    
    int test6_list1[] = {1, 3, 5, 7, 9, 11};
    int test6_list2[] = {2, 4, 6, 8, 10};
    int test6_expected[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
    
    int test7_list1[] = {100, 200, 300, 400, 500};
    int test7_list2[] = {150, 250, 350};
    int test7_expected[] = {100, 150, 200, 250, 300, 350, 400, 500};
    
    TestCase tests[] = {
        {test1_list1, 3, test1_list2, 5, test1_expected, 8, 
         "Esempio base: [1,5,9] + [0,2,4,6,8]"},
        
        {test2_list1, 3, test2_list2, 2, test2_expected, 5, 
         "Prima lista più lunga: [1,3,5] + [2,4]"},
        
        {test3_list1, 1, test3_list2, 3, test3_expected, 4, 
         "Prima lista con un elemento: [10] + [20,30,40]"},
        
        {test4_list1, 4, test4_list2, 1, test4_expected, 5, 
         "Seconda lista con un elemento: [1,2,3,4] + [5]"},
        
        {test5_list1, 1, test5_list2, 1, test5_expected, 2, 
         "Entrambe con un elemento: [7] + [8]"},
        
        {test6_list1, 6, test6_list2, 5, test6_expected, 11, 
         "Liste lunghe: [1,3,5,7,9,11] + [2,4,6,8,10]"},
        
        {test7_list1, 5, test7_list2, 3, test7_expected, 8, 
         "Numeri grandi: [100,200,300,400,500] + [150,250,350]"},
        
        // Test con liste vuote
        {NULL, 0, test1_list2, 5, test1_list2, 5, 
         "Prima lista vuota: [] + [0,2,4,6,8]"},
        
        {test1_list1, 3, NULL, 0, test1_list1, 3, 
         "Seconda lista vuota: [1,5,9] + []"},
        
        {NULL, 0, NULL, 0, NULL, 0, 
         "Entrambe liste vuote: [] + []"}
    };
    
    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;
    
    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        printf("----------------------------------------\n");
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
