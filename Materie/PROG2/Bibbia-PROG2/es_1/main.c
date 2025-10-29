#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "palindrome.h"  // Include il tuo header

// Struttura per i test
typedef struct {
    const char *string;
    int first;
    int last;
    bool expected;
    const char *description;
} TestCase;

// Funzione per stampare il risultato di un test
void printTestResult(int testNum, const char *description, bool passed) {
    printf("Test %2d: %-40s \t\t\t\t\t\t\t [%s]\n", 
           testNum, 
           description, 
           passed ? "\033[32mPASS\033[0m" : "\033[31mFAIL\033[0m");
}

int main() {
    printf("=== TEST SUITE per isPalindrome ===\n\n");
    
    // Array di test cases
    TestCase tests[] = {
        // Test base con palindromi semplici
        {"aba", 0, 2, true, "Palindromo semplice dispari (aba)"},
        {"abba", 0, 3, true, "Palindromo semplice pari (abba)"},
        {"abc", 0, 2, false, "Non palindromo (abc)"},
        
        // Test con singolo carattere
        {"a", 0, 0, true, "Singolo carattere"},
        {"hello", 1, 1, true, "Singolo carattere in mezzo"},
        
        // Test con due caratteri
        {"aa", 0, 1, true, "Due caratteri uguali"},
        {"ab", 0, 1, false, "Due caratteri diversi"},
        
        // Test con sottostringhe
        {"abcba", 0, 4, true, "Palindromo completo (abcba)"},
        {"abcba", 1, 3, true, "Sottostringa palindroma (bcb)"},
        {"abcdef", 2, 4, false, "Sottostringa non palindroma (cde)"},
        
        // Test con indici agli estremi
        {"racecar", 0, 6, true, "Palindromo classico (racecar)"},
        {"racecar", 1, 5, true, "Sottostringa palindroma (aceca)"},
        {"racecar", 2, 4, true, "Sottostringa palindroma (cec)"},
        
        // Test con stringhe più lunghe
        {"A man a plan a canal Panama", 0, 26, false, "Frase con spazi (non palindroma)"},
        {"abcdefedcba", 0, 10, true, "Palindromo lungo"},
        {"abcdefghijklmnopqrstuvwxyz", 0, 25, false, "Alfabeto (non palindromo)"},
        
        // Test edge cases
        {"ab", 1, 0, true, "Indici invertiti (range vuoto)"},
        {"hello", 2, 2, true, "Range di un solo carattere"},
        
        // Test con caratteri speciali
        {"12321", 0, 4, true, "Palindromo numerico"},
        {"a!b!a", 0, 4, true, "Palindromo con caratteri speciali"},
        {"a!b@a", 0, 4, false, "Non palindromo con caratteri speciali"},
        
        // Test con stringhe vuote o casi limite
        {"", 0, -1, true, "Range vuoto (first > last)"},
        {"x", 0, 0, true, "Un carattere solo"}
    };
    
    int totalTests = sizeof(tests) / sizeof(TestCase);
    int passedTests = 0;
    
    // Esecuzione dei test
    for (int i = 0; i < totalTests; i++) {
        bool result = isPalindrome(tests[i].string, tests[i].first, tests[i].last);
        bool passed = (result == tests[i].expected);
        
        printTestResult(i + 1, tests[i].string, passed);
        
        // Stampa dettagli del test
        printf("         String: \"%s\", Range: [%d, %d], Expected: %s, Got: %s\n",
               tests[i].string,
               tests[i].first,
               tests[i].last,
               tests[i].expected ? "true" : "false",
               result ? "true" : "false");
        
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
