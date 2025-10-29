# Domande di teoria con soluzioni

1. Si considerino le seguenti dichiarazioni:

``` C
typedef struet _scheda Scheda, *Puntatore;

struct _scheda (
	int   numero;
	char* testo;
}	
int funzione(Scheda sc, Puntatore pu);
Scheda     x;
Scheda    *y;
Puntatore *z;
```

Quali delle seguenti invocazioni sano staticamente (ovvero per il compilatore) corrette e quali sono invece errate?

	1. funzione(x  ,  NULL);
	2. funzione(**z,  y);
	3. funzione(y  , *y);

soluzioni:
	1. VERO : x è di tipo Scheda e NULL può essere il valore di un ptr qualsiasi
	2. VERO : deserializzando due volte z si passa dal Ptr del ptr di una Scheda alla scheda puntata. y è va bene così
	3. FALSO: y è un ptr, quindi bisogna deserializzarlo prima di poterlo passare 

2. Date le seguenti definizioni:

``` C
enum tag_paese { UK, ITALIA };
typedef struct _indUK {
	int  number;
	char street[N1];
	int  floor;
	char pcode[N2];
	char town[N3];
} indUK;

enum tag_strada {VIA, PIAZZA, CORSO};

typedef struct _indITA (
		 int        civico;
	enum tag_strada strada;
		 char       nomestrada[N1]:
		 char       citta[N3];
} indITA;

struct {
	enum tag_paese  nazione;
	union {
		 indUK      address;
		 indITA     indirizzo;
	} n;
} X, X2, *PTRX;
```

indicare quali delle seguenti espressioni sono staticamente (**ovvero per il compilatore**) corrette e quali sono errate:
	1. `X.n.address.civico = 10;`
	2. `if (nazione == UK) strcpy(X.n.address.street, "Magellan Lane");`
	3. `PTRX = &X;`
	4. `PTRX = &(X-›nazione);`
	5. `PTRX->n.indirizzo.nomestrada[0] == 'z';`
	6. `PTRX->nomestrada;`

soluzioni:
	1. FALSE: Address non ha la var civico
	2. FALSE: nazione è un elemento di X. quindi si chiama con X.nazione
	3. VERO : il puntatore a X è un ptr a una struct
	4. FALSO: un ptr a nazione non ha lo stesso tipo di PTRX (perché nazione non è dello stesso tipo di X)
	5. VERO : il primo elemento di nomestrada è 'z' 
	6. FALSO: nomestrada non è un elemento della struct

3. Si considerino le seguenti dichiarazioni:

```C
typedef struct _oggetto Oggetto, *Puntatore;
struct _oggetto {
	char* descrizione;
	char  codice [10];
	int   peso;
};

int funzione(Oggetto og, Puntatore pu);
Oggetto   scatola[5];
Oggetto   *x;
Puntatore *z;
```
Quali delle seguenti invocazioni sono staticamente (ovvero per il compilatore) corrette e quali sono errate?

	1. funzione (x  , NULL);
	2. funzione (**z, z);
	3. funzione (*z , &(scatola[3]));

soluzioni:
	1. FALSE: x è un ptr
	2. FALSE: z è un ptr al ptr (`**z, *z` sarebbe stato corretto)
	3. FALSE: z va deserializzato 2 volte

4. Si consideri il codice riportato nel seguito e si risponda alle domande:

```C
char* arr = "ELEFANTE";

int funz(char *a, int n, char c) {
	if (a == NULL) return -1;
	if (n < 0)     return -2;
	int i = 0;
	int sum = 0;
	while ((i < 5) || (a[i] != '\0')) { 
		if (a[i] != c) {
			i   =   i + 2;
			sum = sum + 1;
		} else { 
			sum = sum - 1;
			i   =   i - 1;
		}
	}
	return sum;
}
```

	1. È possibile che qualche esecuzione di funz produca un segmentation fault
	
	2. La chiamata di funzione funz(arr, 5, 'A') restituisce (scegliere un valore numerico fra quelli proposti nel menu, oppure VERO se l'esecuzione può non terminare o produrre un segmentation fault)

	3. È possibile che qualche esecuzione funz non termini

soluzioni:
	1. VERO: se il primo carattere di arr è uguale da ch, perche' i diventa -1
	2. VERO: da seg fault perche' accede a `arr[10]`
	3. VERO: la i potrebbe oscillare all'infinito (per esempio chiamando `funz("EAA", 'A')`)


5. dato il frammento di codice:
```C
enum _tag_ristorante {ENGLISH, ITALIANO};
typedef struct_course {
	int   number;
	int   price;
	char  name[N1];
	char *description;
) Course;

enum tag_portata {ANTIPASTO, PRIMO, SECONDO}:

typedef struct_portata {
	int  prezzo;
	enum tag_portata tipo;
	char nomeportata[N1];
} Portata;

struct {
	enum tag_ristorante tipo;
	union {
		Course  co;
		Portata po;
	} piatto;
} X, X2, *PTRX;
```

indicare quali delle seguenti espressioni sono staticamente (ovvero per il compilatore) corrette e quali sono errate.

	1. X.piatto.po.nomeportata != X2.piatto.po.nomeportata;
	2. X.piatto.po.nomeportata =  X2.piatto.po.nomeportata;
	3. X.piatto.po.tag_portata =  "PRIMO";
	4. PTRX->piatto.co.number  = X.piatto.po.prezzo;
	5. PTRX = malloc(sizeof (Portata));
	6. X->piatto.co.name = PTRX.piatto.po.nomeportata;

soluzioni:
	1. VERO : paragone tra due puntatori.
	2. FALSO: non si può assegnare ad un array un ptr perché gli arr sono statici
	3. FALSO: non si può assegnare una string a un char*
	4. VERO : assegnazione tra int 
	5. FALSO: anche se è un void* (e quindi dovrebbe andare bene) mi pare dia comunque errore
	6. FALSO: X è una struct. quindi bisogna usare il . e il contrario per PTRX


6. Si consideri il codice riportato nel seguito e si risponda alle domande:
```C
int arr[] = {1, 4, -1, 5};

int funz (int *a, int n) {
	if (a == NULL) return -1;
	if (n < 0) return -2;
	
	int *ptr = a;
	int  i = 0;
	int  sum = 0;
	while (i < n-1){
		if (a[i]%2 == 0) { 
			i++; 
			sum = sum + *(ptr+i); 
		}
		else { 
			sum = sum + ptr[i+1];
			i++;
		}
	}
	return sum;

}
```

	1. la funzione può produrre un segmentation fault
	2. se non produce un segmentation fault, la funzione terminerà sempre
	3. La chiamata di funzione funz(arr,4) restituisce:

soluzioni:
	1. VERO: se si passa n > size di a 
	2. VERO: i aumenta sempre, quindi arrivera' ad essere >= n
	3. 8   : basta pensare a `*(ptr+i) = ptr[i] = a[i]`


7. Si considerino le seguenti dichiarazioni:

```C
typedef struct _scheda Scheda, *Puntatore;
struct _scheda {
	int numero;
	char *testo;
}
int funzione(Scheda sc, Puntatore pu);

Scheda     x;
Scheda    *y;
Puntatore *z;
```

Quali delle seguenti invocazioni sono staticamente (ovvero per il compilatore) corrette e quali sono invece errate?

	1. funzione(  *y , &(**z));
	2. funzione(   y , &y    );
	3. funzione((**z), &x    );

soluzioni:
	1. VERO : & e * di z si annullano, lasciando un ptr a Sheda
	2. FALSO: y è un ptr
	3. VERO : la doppia deserializzazione di z è di tipo Scheda e l'indirizzo di x è un puntatore a x

