#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

//------------------------------------------------------------------
// Prototipi delle funzioni
//------------------------------------------------------------------

// Restituisce il valore minimo nell'array a[] attraverso una ricerca 
// lineare. Se l'array è vuoto, restituisce INT_MAX
// Usa la ricorsione controvariante (crescente):
int ricerca_minimo_con(const size_t aLen, const int a[]); // involucro
// Usa la ricorsione covariante (decrescente):
int ricerca_minimo_cov(const size_t aLen, const int a[]); // involucro
// Usa la ricorsione dicotomica (dimezzamento intervalli):
int ricerca_minimo_dic(const size_t aLen, const int a[]); // involucro

//------------------------------------------------------------------

// Sostituisce tutti i numeri dispari nell'array a[] con il loro doppio
// Usa la ricorsione controvariante (crescente):
void raddoppia_dispari_con(const size_t aLen, int a[]); // involucro
// Usa la ricorsione covariante (decrescente):
void raddoppia_dispari_cov(const size_t aLen, int a[]); // involucro
// Usa la ricorsione dicotomica (dimezzamento intervalli):
void raddoppia_dispari_dic(const size_t aLen, int a[]); // involucro

//------------------------------------------------------------------

// Restituisce true se nell'array a[] esistono valori maggiori o uguali a val.
// Usa la ricorsione controvariante (crescente):
bool esiste_maggiore_con(const size_t aLen, const int a[], const int val); // involucro
// Usa la ricorsione covariante (decrescente):
bool esiste_maggiore_cov(const size_t aLen, const int a[], const int val); // involucro
// Usa la ricorsione dicotomica (dimezzamento intervalli):
bool esiste_maggiore_dic(const size_t aLen, const int a[], const int val); // involucro

//------------------------------------------------------------------

// Restituisce il numero di occorrenze degli elementi nell'array a[] 
// che sono multipli di val.
// Usa la ricorsione controvariante (crescente):
size_t conta_multipli_con(const size_t aLen, const int a[], const int val); // involucro
// Usa la ricorsione covariante (decrescente):
size_t conta_multipli_cov(const size_t aLen, const int a[], const int val); // involucro
// Usa la ricorsione dicotomica (dimezzamento intervalli):
size_t conta_multipli_dic(const size_t aLen, const int a[], const int val); // involucro

//------------------------------------------------------------------

int main(void) {
    // matrice irregolare per il test
    #define ROWS 8
    #define COLS 15
    int mat[ROWS][COLS] = {
        {16,20,30},
        {18,3,30,22,18,71,82,6,98,57,35,35,84,19,5},
        {79,41,99,12,66},
        {63,56,79,48,28,62,15,78},
        {},
        {30,66,14,88,30,23,29,50},
        {50,45,72,55,10,9,71,59,95,85,16,42,37,32,70},
        {31,10,60,57,86,30,72,76,84}
    };
    size_t rags[ROWS] = {3,15,5,8,0,8,15,9};

    // Unit test per la ricerca del minimo
    puts("Ricerca del minimo:   controvariante  covariante  dicotomico");
    const int min_res[ROWS] = {16,3,12,15,INT_MAX,14,9,10};
    for (size_t r=0; r<ROWS; r++) {
        int m1 = ricerca_minimo_con(rags[r], mat[r]);
        int m2 = ricerca_minimo_cov(rags[r], mat[r]);
        int m3 = ricerca_minimo_dic(rags[r], mat[r]);
        printf("[%s] Array %zu: minimo  %-16d%-12d%-8d\n",
               (m1==min_res[r] && m2==m1 && m3==m1 ? "ok" : "NO"), r, m1, m2, m3);
    }
    puts("");

    // Unit test per il raddoppio dei dispari
    int mat_doppi[ROWS][COLS] = {
        {16,20,30},
        {18,6,30,22,18,142,82,6,98,114,70,70,84,38,10},
        {158,82,198,12,66},
        {126,56,158,48,28,62,30,78},
        {},
        {30,66,14,88,30,46,58,50},
        {50,90,72,110,10,18,142,118,190,170,16,42,74,32,70},
        {62,10,60,114,86,30,72,76,84}
    };
    puts("Raddoppio dispari:   controvariante  covariante  dicotomico");
    for (size_t r=0; r<ROWS; r++) {
        int a1[COLS], a2[COLS], a3[COLS];
        for (size_t i=0; i<rags[r]; i++)
            a1[i] = a2[i] = a3[i] = mat[r][i];

        raddoppia_dispari_con(rags[r], a1);
        raddoppia_dispari_cov(rags[r], a2);
        raddoppia_dispari_dic(rags[r], a3);

        bool tutti_con = true, tutti_cov = true, tutti_dic = true;
        for (size_t i=0; i<rags[r]; i++) {
            tutti_con = tutti_con && (a1[i]==mat_doppi[r][i]);
            tutti_cov = tutti_cov && (a2[i]==mat_doppi[r][i]);
            tutti_dic = tutti_dic && (a3[i]==mat_doppi[r][i]);
        }
        printf("[%s] Array %zu: doppi  %-16d%-12d%-8d\n",
               (tutti_con && tutti_cov && tutti_dic ? "ok" : "NO"), r, 
                tutti_con, tutti_cov, tutti_dic);

    } 
    puts("");
 
    // Unit test per esistenza maggiore
    puts("Esiste maggiore:   controvariante  covariante  dicotomico");
    const bool esiste_res[ROWS] = {false,true,true,false,false,true,true,true};
    for (size_t r=0; r<ROWS; r++) {
        bool e1 = esiste_maggiore_con(rags[r], mat[r], 80);
        bool e2 = esiste_maggiore_cov(rags[r], mat[r], 80);
        bool e3 = esiste_maggiore_dic(rags[r], mat[r], 80);
        printf("[%s] Array %zu: esiste?  %-16d%-12d%-8d\n",
               (e1==esiste_res[r] && e2==e1 && e3==e1 ? "ok" : "NO"), r, e1, e2, e3);
    }
    puts("");

    // Unit test per conteggio multipli
    puts("Conta multipli:   controvariante  covariante  dicotomico");
    const size_t cm_res[ROWS] = {2,4,0,1,0,3,7,3};
    for (size_t r=0; r<ROWS; r++) {
        size_t c1 = conta_multipli_con(rags[r], mat[r], 5);
        size_t c2 = conta_multipli_cov(rags[r], mat[r], 5);
        size_t c3 = conta_multipli_dic(rags[r], mat[r], 5);
        printf("[%s] Array %zu: conteggio?  %-16zu%-12zu%-8zu\n",
               (c1==cm_res[r] && c2==c1 && c3==c1 ? "ok" : "NO"), r, c1, c2, c3);
    }
    puts("");
}

//------------------------------------------------------------------
// Prototipi delle funzioni ricorsive (non involucro)
//------------------------------------------------------------------

int ricerca_minimo_conR(const int a[],
                        const size_t left, const size_t right);
int ricerca_minimo_covR(const int a[],
                        const size_t left, const size_t right);
int ricerca_minimo_dicR(const int a[],
                        const size_t left, const size_t right);

void raddoppia_dispari_conR(int a[],
                            const size_t left, const size_t right);
void raddoppia_dispari_covR(int a[],
                            const size_t left, const size_t right);
void raddoppia_dispari_dicR(int a[],
                            const size_t left, const size_t right);

bool esiste_maggiore_conR(const int a[], const int val,
                          const size_t left, const size_t right);
bool esiste_maggiore_covR(const int a[], const int val,
                          const size_t left, const size_t right);
bool esiste_maggiore_dicR(const int a[], const int val,
                          const size_t left, const size_t right);

size_t conta_multipli_conR(const int a[], const int val,
                           const size_t left, const size_t right);
size_t conta_multipli_covR(const int a[], const int val,
                           const size_t left, const size_t right);
size_t conta_multipli_dicR(const int a[], const int val,
                           const size_t left, const size_t right);

//------------------------------------------------------------------

int min(int a, int b) {
    return (a<b) ? a : b;
}

//------------------------------------------------------------------

// DA COMPLETARE