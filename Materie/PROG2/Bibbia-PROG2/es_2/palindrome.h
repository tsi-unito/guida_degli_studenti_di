#ifndef PALINDROME_H
#define PALINDROME_H

#include <stdbool.h>

/**
 * @brief Verifica se una stringa è palindroma.
 * 
 * @param s    : una stringa, 
 * @param first: un indice in s
 * @param last : un indice in s
 * 
 * @return s nell'intervallo first..last è palindroma
 */
bool isPalindrome(const char *s, int first, int last);

#endif // PALINDROME_H
