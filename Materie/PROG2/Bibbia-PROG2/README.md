# Bibbia-Prog2

Raccolta di esercizi divertenti di prog2 presa dal gruppo telegram e riorganizzata aggiungendo test (offered by GitHub Copilot) vari per ogni esecizio.

# Installation

```bash
git clone --filter=blob:none --no-checkout https://github.com/tsi-unito/guida_degli_studenti_di.git
cd guida_degli_studenti_di
git sparse-checkout init --cone
git sparse-checkout set Materie/PROG2
git checkout master
```

una volta clonata la repo, spostarsi nella directory di un esercizio, e implementare la funzione nel file .c.

per verificare l'esercizio eseguire:

```bash
make run

```

Have fun!


# Legenda esercizi



[1. Realizzare un implementazione ITERATIVA della seguente funzione:](es_1/palindrome.c)
```C
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
bool isPalindrome(const char *s, int first, int last);
```

[2. Realizzare un implementazione RICORSIVA della seguente funzione:](es_2/palindrome.c)
```C
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
bool isPalindrome(const char *s, int first, int last);
```


[3. Realizzare un implementazione ITERATIVA della seguente funzione:](es_3/mixAlternate.c)
``` C
typedef struct node IntNode, *IntList;
struct node {
  int data;
	IntList next;
}

/** 
 * @brief Restituisce la lista alternata dei nodi di *lsPtr1 e *lsPtr2,
 * togliendoli da *lsPtr1 e *lsPtr2, 
 * che alla fine conterranno entrambi NULL (non alloca nuova memoria). 
 * 
 * Ad es. date [1, 5, 9] e [0, 2, 4, 6, 8) restituisce
 * [1, 0, 5, 2, 9, 4, 6, 8].
 */
IntList mixAlternate(IntList *lsPtr1, IntList *lsPtr2);
```

[4. Realizzare un implementazione RICORSIVA della seguente funzione:](es_4/mirror.c)

```C
typedef struct treeNode IntTreeNode, *IntTree;

struct treeNode {
	IntTree left;
	int data;
	IntTree right;
};

/**
 * @brief Trasforma un albero nella sua versione speculare.
 * @param tree: albero da specchiare

	  2
	 /\ 
    1  5
       /\ 
      4  9
è:
	  2
	 /\ 
    5  1
   /\ 
  9  4
 */ 
vaid mirror(IntTree tree);
```

[5. Realizzare un implementazione RICORSIVA della seguente funzione:](es_5/check.c)

```C
/**
 * @brief Dati una stringa s1 di lunghezza n1 >= 0 
 * e una stringa s2 di lunghezza n2 >= 0 !
 * verificha che le stringhe s2 e s3 siano uguali, dove s3 è la stringa
 * ottenuta considerando solo i caratteri in posizione pari di s1
 * 
 * NOTA BENE: Si assuma s1 != NULL e s2 != NULL 
 * ESEMPI:
 * (1) date "pArEeBvUaT" (di lunghezza 10) e "provateci" (di lunghezza 9)
 * restituisce 0.
 * (2) date "pArEoBvUaT" (di lunghezza 10) e "prova" (di lunghezza 5)
 * restituisce 1.
 * (3) date "pArEoBvUaTeci" (di lunghezza 13) e "prova" (di lunghezza 5)
 * restituisce 0.
*/
bool check(const char *s1, int n1, const char *s2, int n2);
```

[6. Realizzare un implementazione RICORSIVA della seguente funzione:](es_6/count.c)
```C
typedef struct treeNode CharTreeNode, *CharTree;
struct treeNode {
	CharTree left;
	char data;
	CharTree right;
}

/**
 * @brief Restituisce il numero di nodi dell'albero binario tree di
 * profondità compresa tra m e n
 * (si ricorda che il nodo radice ha profondità 0).
 * 
 * ESEMPIO: dato  
    R
   /\  
  F  Z
 /\
D  H
  /\
 G  L
   /\
  I  M
 * 
 * e gli interi -2 e 3 restituisce 7.
 */
int count (CharTree tree, int m, int n);
```


[7. Realizzare un implementazione RICORSIVA della seguente funzione:](es_7/transfer.c)
```C
typedef struct node IntNode, *IntList;

struct node {
	int     data; 
	IntList next;
}

/**
 * @brief Modifica *lsPtr1 togliendo da *lsPtr1 tutti i nodi che occorrono
 * in ls2, ﻿﻿e li restituisce in una nuova lista nello stesso ordine in cui
 * occorrono in *IsPtr1 (NON ALLOCA NUOVA MEMORIA).
 * ESEMPIO:
 * date [1, 2, 5, 3, 4, 5, 9, 8] e [7, 5, 2, 4]
 * modifica la prima lista in [1, 3, 9, 8] e restituisce [2, 5, 4, 5].
 */
IntList transfer (IntList *lsPtr1, IntList ls2);

```

[8. Realizzare un implementazione ITERATIVA della seguente funzione:](es_8/merge.c)
```C
typedef struct node IntNode, *IntList;

struct node {
	int data;
	IntList next;
}

/**
 * @brief Date due liste ordinate *IsPtr1 e *IsPtr2, 
 * restituisce l'unione ordinata delle due
 * Non si può usare malloc, ma bisogna ordinare gli elementi delle liste
 * @return Primo elemento della lista riformattata 
 * alla fine i param saranno entrambi NULL
 * Ad esempio:
 * date [1, 5, 9] e [0, 2, 4, 6, 8] 
 * restituisce [0, 1, 2, 4, 5, 6, 8, 9]
 */
IntList merge(IntList *lsPtr1, IntList *lsPtr2);
```

[9. Realizzare un implementazione RICORSIVA della seguente funzione:](es_9/sort.c)
```C
typedef struct treeNode Int TreeNode, *Int Tree;

struct treeNode {

Int Tree left;
	int data;
	IntTree right;
}

/**
 * @brief Trasforma un albero, agendo su ogni nodo con entrambi i rami,
 * scambiandoli se le loro radici non sono nell'ordine corretto 
 * (ovvero lo scambio avviene quando sinistra > destra).
 */
void sort (IntTree tree);
```



