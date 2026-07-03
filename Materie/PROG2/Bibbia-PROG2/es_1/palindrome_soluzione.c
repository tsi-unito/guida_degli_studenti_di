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

  if (!s) return NULL;

  bool result = true;

  while (first <= last && result) {
    if (s[first] != s[last])
      result = false;
    else {
      first++;
      last --;
    }
  }

  return result;
}
