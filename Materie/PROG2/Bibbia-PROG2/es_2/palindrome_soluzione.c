#include <stdio.h>
#include <stdbool.h>
#include <string.h>

/** 
 *
 * @brief Verifica se una stringa è palindroma.
 * 
 * @param s    : una stringa, 
 * @param first: un indice in s
 * @param last : un indice in s
 * 
 * @return s nell'intervallo first..last è palindroma
 ﻿*/
bool isPalindrome(
	const char *s, 
		    int   first, 
		    int   last
){
  // errore
  if (!s) return NULL;

  // caso base
  if (first >= last)
    return true;

  return s[first] == s[last] && isPalindrome(s, first+1, last-1);
}
