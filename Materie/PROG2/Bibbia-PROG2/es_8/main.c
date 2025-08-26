#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "merge.h"

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
    
    printf("         Input:  ");
    __printList(list1, "List1");
    printf("                 ");
    __printList(list2, "List2");
    
    // Verifica che le liste di input siano ordinate
    int list1_sorted = __isListSorted(list1);
    int list2_sorted = __isListSorted(list2);
    
    if (!list1_sorted || !list2_sorted) {
        printf("         ERROR: Le liste di input non sono ordinate!\n");
        __freeList(list1);
        __freeList(list2);
        return 0;
    }
    
    // Esegui la funzione
    IntList result = merge(&list1, &list2);
    
    printf("         Output: ");
    __printList(result, "Result");
    printf("                 ");
    __printList(list1, "List1 (after)");
    printf("                 ");
    __printList(list2, "List2 (after)");
    
    // Verifica che le liste originali siano vuote
    int lists_empty = (list1 == NULL && list2 == NULL);
    
    // Verifica che il risultato sia ordinato
    int result_sorted = __isListSorted(result);
    
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
    
    int passed = correct_result && lists_empty && result_sorted;
    
    if (!lists_empty) {
        printf("         ERROR: Le liste originali non sono vuote!\n");
    }
    if (!result_sorted) {
        printf("         ERROR: Il risultato non è ordinato!\n");
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
    printf("=== TEST SUITE per merge ===\n");
    printf("Creato da: C4V4H\n");
    printf("Data: 2025-06-08 17:16:12 UTC\n\n");
    
    // Test cases
    int test1_list1[] = {1, 5, 9};
    int test1_list2[] = {0, 2, 4, 6, 8};
    int test1_expected[] = {0, 1, 2, 4, 5, 6, 8, 9};
    
    int test2_list1[] = {1, 3, 5};
    int test2_list2[] = {2, 4, 6};
    int test2_expected[] = {1, 2, 3, 4, 5, 6};
    
    int test3_list1[] = {10};
    int test3_list2[] = {5, 15, 20};
    int test3_expected[] = {5, 10, 15, 20};
    
    int test4_list1[] = {1, 2, 3, 4};
    int test4_list2[] = {5};
    int test4_expected[] = {1, 2, 3, 4, 5};
    
    int test5_list1[] = {7};
    int test5_list2[] = {8};
    int test5_expected[] = {7, 8};
    
    int test6_list1[] = {1, 3, 5, 7, 9};
    int test6_list2[] = {2, 4, 6, 8, 10};
    int test6_expected[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    int test7_list1[] = {-5, -3, -1};
    int test7_list2[] = {-4, -2, 0};
    int test7_expected[] = {-5, -4, -3, -2, -1, 0};
    
    int test8_list1[] = {1, 1, 2, 3};
    int test8_list2[] = {1, 2, 2, 4};
    int test8_expected[] = {1, 1, 1, 2, 2, 2, 3, 4};
    
    int test9_list1[] = {100, 200, 300};
    int test9_list2[] = {50, 150, 250, 350, 400};
    int test9_expected[] = {50, 100, 150, 200, 250, 300, 350, 400};
    
    int test10_list1[] = {5, 10, 15, 20, 25, 30};
    int test10_list2[] = {1, 2, 3};
    int test10_expected[] = {1, 2, 3, 5, 10, 15, 20, 25, 30};
    
    TestCase tests[] = {
        {test1_list1, 3, test1_list2, 5, test1_expected, 8, 
         "Esempio base: [1,5,9] + [0,2,4,6,8]"},
        
        {test2_list1, 3, test2_list2, 3, test2_expected, 6, 
         "Liste uguali lunghezza: [1,3,5] + [2,4,6]"},
        
        {test3_list1, 1, test3_list2, 3, test3_expected, 4, 
         "Prima lista un elemento: [10] + [5,15,20]"},
        
        {test4_list1, 4, test4_list2, 1, test4_expected, 5, 
         "Seconda lista un elemento: [1,2,3,4] + [5]"},
        
        {test5_list1, 1, test5_list2, 1, test5_expected, 2, 
         "Entrambe un elemento: [7] + [8]"},
        
        {test6_list1, 5, test6_list2, 5, test6_expected, 10, 
         "Liste interlacciate: [1,3,5,7,9] + [2,4,6,8,10]"},
        
        {test7_list1, 3, test7_list2, 3, test7_expected, 6, 
         "Numeri negativi: [-5,-3,-1] + [-4,-2,0]"},
        
        {test8_list1, 4, test8_list2, 4, test8_expected, 8, 
         "Con duplicati: [1,1,2,3] + [1,2,2,4]"},
        
        {test9_list1, 3, test9_list2, 5, test9_expected, 8, 
         "Numeri grandi: [100,200,300] + [50,150,250,350,400]"},
        
        {test10_list1, 6, test10_list2, 3, test10_expected, 9, 
         "Prima lista più lunga: [5,10,15,20,25,30] + [1,2,3]"},
        
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
