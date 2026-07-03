#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"

// Struttura per i test
typedef struct {
    const char *s1;
    int n1;
    const char *s2;
    int n2;
    bool expected;
    const char *description;
} TestCase;

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, bool passed) {
    printf("Test %2d: %-55s \t\t\t\t\t [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

// Funzione per eseguire un singolo test
bool runTest(TestCase *test) {
    printf("         Input:\n");
    __printStringInfo(test->s1, test->n1, "s1");
    __printStringInfo(test->s2, test->n2, "s2");
    __printEvenChars(test->s1, test->n1);
    
    // Esegui la funzione ricorsiva
    bool result = check(test->s1, test->n1, test->s2, test->n2);
    
    // Verifica manuale per confronto
    bool manual_result = __manualCheck(test->s1, test->n1, test->s2, test->n2);
    
    printf("         Expected: %s, Got: %s, Manual check: %s\n",
           test->expected ? "true" : "false",
           result ? "true" : "false",
           manual_result ? "true" : "false");
    
    bool passed = (result == test->expected);
    
    if (!passed) {
        printf("         ERROR: Il risultato non corrisponde all'atteso!\n");
    }
    if (result != manual_result) {
        printf("         WARNING: Il risultato differisce dal controllo manuale!\n");
    }
    
    return passed;
}

int main() {
    printf("=== TEST SUITE per check (ricorsiva) ===\n");
    printf("Author: C4V4H | Date: 2025-06-08 13:31:32\n\n");
    
    // Test cases basati sugli esempi e casi aggiuntivi
    TestCase tests[] = {
        // Esempi dal problema
        {"pArEeBvUaT", 10, "provateci", 9, false, 
         "Esempio 1: 'pArEeBvUaT' vs 'provateci' -> false"},
        
        {"pArEoBvUaT", 10, "prova", 5, true, 
         "Esempio 2: 'pArEoBvUaT' vs 'prova' -> true"},
        
        {"pArEoBvUaTeci", 13, "prova", 5, false, 
         "Esempio 3: 'pArEoBvUaTeci' vs 'prova' -> false"},
        
        // Test base
        {"a", 1, "a", 1, true, 
         "Singolo carattere: 'a' vs 'a' -> true"},
        
        {"a", 1, "b", 1, false, 
         "Singolo carattere diverso: 'a' vs 'b' -> false"},
        
        {"ab", 2, "a", 1, true, 
         "Due caratteri: 'ab' vs 'a' -> true"},
        
        {"ab", 2, "b", 1, false, 
         "Due caratteri: 'ab' vs 'b' -> false"},
        
        {"abc", 3, "ac", 2, true, 
         "Tre caratteri: 'abc' vs 'ac' -> true"},
        
        {"abc", 3, "ab", 2, false, 
         "Tre caratteri: 'abc' vs 'ab' -> false"},
        
        // Test con stringhe vuote
        {"", 0, "", 0, true, 
         "Entrambe vuote: '' vs '' -> true"},
        
        {"a", 1, "", 0, false, 
         "Prima non vuota, seconda vuota: 'a' vs '' -> false"},
        
        {"", 0, "a", 1, false, 
         "Prima vuota, seconda non vuota: '' vs 'a' -> false"},
        
        // Test più complessi
        {"abcdef", 6, "ace", 3, true, 
         "Sei caratteri: 'abcdef' vs 'ace' -> true"},
        
        {"abcdef", 6, "acd", 3, false, 
         "Sei caratteri: 'abcdef' vs 'acd' -> false"},
        
        {"abcdefg", 7, "ace", 3, false, 
         "Sette caratteri: 'abcdefg' vs 'ace' -> false (lunghezza diversa)"},
        
        {"abcdefg", 7, "aceg", 4, true, 
         "Sette caratteri: 'abcdefg' vs 'aceg' -> true"},
        
        // Test con caratteri speciali
        {"a!b@c#", 6, "a!c", 3, false, 
         "Con simboli: 'a!b@c#' vs 'a!c' -> false"},
        
        {"123456", 6, "135", 3, true, 
         "Numeri: '123456' vs '135' -> true"},
        
        {"123456", 6, "136", 3, false, 
         "Numeri: '123456' vs '136' -> false"},
        
        // Test edge cases
        {"x", 1, "xy", 2, false, 
         "s2 più lunga: 'x' vs 'xy' -> false"},
        
        {"xyza", 4, "x", 1, false, 
         "s2 più corta del dovuto: 'xyza' vs 'x' -> false"},
        
        {"hello", 5, "hlo", 3, true, 
         "Parola reale: 'hello' vs 'hlo' -> true"},
        
        {"programming", 11, "pormig", 6, true, 
         "Parola lunga: 'programming' vs 'pormig' -> true"}
    };
    
    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;
    
    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        printf("----------------------------------------\n");
        bool passed = runTest(&tests[i]);
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
