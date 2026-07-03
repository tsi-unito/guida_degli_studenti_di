#include <stdio.h>
#include <math.h>

int main(void) {
	int x, y, z;

    printf("Introduci il primo numero (per es. 8):   ");
    scanf("%d", &x);
    printf("Introduci il secondo numero (per es. 2): ");
    scanf("%d", &y);
    printf("Introduci il terzo numero (per es. 3):   ");
    scanf("%d", &z);

    // divisione intera
    int media1 = (x + y + z) / 3;
    printf("int media1: (x+y+z)/3 = %d (divisione intera)\n", 
    	   media1);

    // divisione intera e cast a double
    double media2 = (x + y + z) / 3;
    printf(
        "double media2: (x+y+z)/3 = %lf (divisione intera e cast a double)\n",
    	media2
    );

    // divisione con virgola - risultato atteso!!
    double media3 = (x + y + z) / 3.0;
    printf("double media3: (x+y+z)/3.0 = %lf (divisione in virgola)\n", 
    	   media3);

    // ESERCIZIO: 
    // Consideriamo tre varianti per calcolare media3: 
    // double media3 = (x + y + z) / 3.0;
    // double media3 = (double)(x + y + z) / 3;
    // double media3 = (double)((x + y + z) / 3); 
    // Cosa fanno rispettivamente? 
    // Quali sono i tipi delle operazioni intermedie?
    // Dove avviene il cast da int a double nelle tre varianti?
    // 1. somma i 3 num e divide il loro risultato per 3.0
    // 2. somma i 3 num, casta il loro risultato in double e divide per 3
    // 3. somma i 3 num, divide per 3 e casta il risultato in double
    
    puts("");
    // arrotondiamo il valore di media3 ad intero
    printf("(int)media3 = %d\n", (int) media3);
    printf("(int)floor(media3) = %d\n", (int)floor(media3));
    printf("(int)round(media3) = %d\n", (int)round(media3));
    printf("(int)ceil(media3)  = %d\n", (int)ceil(media3));

    
    // ESERCIZIO: qual é la differenza tra i due modi di arrotondamento proposti?
    // Provare usando in input i valori 1,2,5
    
    
    printf("");
    // Problemi imprevisti dovuti a range di numeri finito,
    // che portano a rappresentazione binaria approssimata
    double f = 4.35;
    printf("4.35 * 100 = %d\n", round((int)(f * 100))); // provare round() prima del cast!
    
    
    // DOMANDA: spiegare per quale motivo il codice (int)(f * 100) non
    // produce il valore atteso (435).
}
