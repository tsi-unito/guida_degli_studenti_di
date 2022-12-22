---
title: "Basi di dati - Parte III - Esercizi - Architettura dei DBMS"
---

# Basi di dati - Parte III - Esercizi

## Gestione del buffer

### Strutture secondarie per l'organizzazione dei file

#### Esempio 1 - B-tree

- In ogni nodo si memorizzano, oltre alle chiavi di ricerca, anche i RID dei relativi record.
    - Si assume che la chiave di ricerca+RID occupi $40$ byte.
    - Se una pagina ha una dimensione di $4 \: KB$, ogni pagina può contenere $4 \: KB / 40 \: B \simeq 99$ chiavi di ricerca.
    - Considerando che un nodo occupa una pagina, si ha che $m = 100$.
    - Con soli tre livelli si può memorizzare $m^L = 100^3$ (un milione di chiavi).
    - Per accedere a una chiave, se ogni nodo è contenuto in una pagina diversa, si muovo al più $3$ pagine.
    - Nei sistemi DBMS, gli indici sopportano bene un carico del $70\%$ della capacità massima.
        - Con $3$ livelli si sopportano bene $700000$ chiavi di ricerca.

#### Esempio 2 - B+-tree

- Si immagina di usare un B+-tree per indicizzare le matricole degli studenti in una tabella `Studenti`.
    - Tabella che può essere memorizzata per esempio come heap.
    - I nodi interni memorizzano le matricole/chiavi di ricerca $k$.
    - Le foglie memorizzano tutte le matricole/chiavi di ricerca abbinate ai rispettivi RID.
        - $k^* = <K, RID>$.
    - Per la chiave di ricerca $35$ si avrà il RID che punta al record corrispondente allo studente con matricola $35$.

#### Esempio 3 - Ricerca puntuale nell'indice in un B+-tree

- Si considera la selezione $\sigma_{MATR=56}(studenti)$.
    - Il DBMS sa che esiste l'indice sul campo $MATR$ e usa il corrispettivo $B+-tree$.
    - Prende il valore $56$ e lo confronta con i vari nodi interni finché non arriva a una foglia.
    - Trova il valore nella foglia e utilizza il RID per accedere al record.
    - In tutto sono stati necessari $L + 1$ accessi.
- Nei sistemi informativi reali, con centinaia di migliaia di chiavi di ricerca, in genere gli indici hanno 2 o 3 livelli.

### Indici

#### Esempio 4 - Euristiche

- Si considera lo schema:
    - $STUDENTI(\underline{MATR} Nome, DataNascita, Genere, Indirizzo)$;
    - $ESAMI(\underline{MATR, Corso}, Voto, DataEsame)$;
- Valutazione delle euristiche:
    - Selettività dell'indice:
        - Un indice sulla chiave $MATR$ ha un'altissima selettività, quindi va bene.
        - Può avere senso mettere un indice sulle date di nascita.
        - Non ha senso mettere un indice sul genere (solo due valori possibili).
            - Il DBMS non userebbe mai questo indice.
    - Indici sulle chiavi:
        - Un indice su una chiave è sempre consigliato.
        - Analogamente si consiglia di definire indici su chiavi esterne, in quanto usate nei join.
        - Si considera $studenti \bowtie_{studenti.MATR = esami.MATR} esami$.
            - In $ESAMI$, $MATR$ non è chiave relazionale (la chiave è $MATR, CORSO$).
            - Ma si consiglia ugualmente di definire un indice.

-----

## Gestione della concorrenza

### Transazioni

#### Esempio 5

- Si valutano due operazioni bancarie che aggiornano i conti $A$ e $B$.
    - In $T_1$ si trasferisce una somma di $50$ dal conto $A$ al conto $B$;
    - In $T_2$ si trasferisce il $10\%$ del saldo del conto $A$ al conto $B$;

```
T1
read(A)
A := A - 50
write(A)
read(B)
B := B + 50
write(B)
```

```
T2
read(A)
temp := 0.1 * A
A := A - temp
write(A)
read(B)
B := B + temp
write(B)
```

- Si assume che tutti i vincoli della base di dati siano rispettati.
    - Si vede immediatamente che né $T_1$ né $T_2$ modificano la somma $A+B$.
        - Altrimenti si creerebbe o distruggerebbe denaro.
    - Per le due transazioni $A+B = 3000$ è un'invariante.
- Si suppone ora che le due transazioni entrino entrambe nel sistema e vengano eseguite in successione.
    - All'inizio $A$ contiene il valore $1000$ e $B$ il valore $2000$.
- Esecuzione delle transazioni $T_1 \: T_2$:
    - Prima dell'inizio:
        - $A = 1000$, $B = 2000$ e $A + B = 3000$.
    - La transazione $T_1$ toglie da $A$ il valore $50$ e lo aggiunge a $B$:
        - $A = 950$, $B = 2050$ e $A + B = 3000$.
    - La transazione $T_2$ toglie da $A$ toglie il $10\%$ dal nuovo saldo di $A$ e lo aggiunge a $B$:
        - $A = 855$, $B = 2145$ e $A + B = 3000$.
- Esecuzione delle transazioni $T_2 \: T_1$:
    - Prima dell'inizio:
        - $A = 1000$, $B = 2000$ e $A + B = 3000$.
    - La transazione $T_2$ toglie da $A$ toglie il $10\%$ dal nuovo saldo di $A$ e lo aggiunge a $B$:
        - $A = 900$, $B = 2100$ e $A + B = 3000$.
    - La transazione $T_1$ toglie da $A$ il valore $50$ e lo aggiunge a $B$:
        - $A = 850$, $B = 2150$ e $A + B = 3000$.
- Storie corrispondenti:
    - $T_1 \: T_2$: $r_1(A)$, $w_1(A)$, $r_1(B)$, $w_1(B)$, $r_2(A)$, $w_2(A)$, $r_2(B)$, $w_2(B)$;
    - $T_2 \: T_1$: $r_2(A)$, $w_2(A)$, $r_2(B)$, $w_2(B)$, $r_1(A)$, $w_1(A)$, $r_1(B)$, $w_1(B)$.

-----

## Gestione del ripristino

### File di log

#### Esempio 6

```
<T1, START>
<T2, START>
<T2, A, BS_0(A), AS_0(A)>
<T2, COMMIT>
<T1, A, BS_1(A), AS_1(A)>
<T3, START>
<T1, COMMIT>
<T3, A, BS_2(A), AS_2(A)>
```

- Glossario:
    - A: tupla generica;
    - BS: before state;
    - AS: after state.
- Si assume che dopo `<T3, A, BS_2(A), AS_2(A)>` il sistema va in crash.
- Si vuole ripristinare lo stato corretto del DB:
    - Le **transazioni attive** (quelle non terminate) sono $AT = \{T3\}$.
    - Per le transazioni attive bisogna esplorare il log all'indietro per annullarne gli effetti.
        - E per riportare la base di dati allo stato precedente delle modifiche.
    - UNDO considera quindi a ritroso le transazioni in $AT$ riportando gli oggetti al loro valore nel $BS$.

#### Esempio 7 - Checkpoint

```
<T1, START>
<T2, START>
<T2, A, BS_0(A), AS_0(A)>
<T2, COMMIT>
<T1, A, BS_1(A), AS_1(A)>
<T3, START>
| CP: | T1, p | T3, p | OK |
<T1, COMMIT>
<T3, A, BS_2(A), AS_2(A)>
```

- Utilizzo dei checkpoint:
    - $T1$ e $T3$ sono transazioni iniziate ma non terminate.
        - Quindi vengono aggiunte al record di checkpoint con i relativi puntatori.
    - $T2$ è iniziata e terminata.
        - Quindi non viene aggiunta al record.
    - Il record di log è reso persistente con una `FORCE LOG`.
    - Le modifiche di $T2$ vengono rese persistenti con una `FORCE` delle pagine.
    - Si aggiunge il flag `OK`.
    - Si esegue un nuovo `FORCE LOG`.

#### Esempio 8 - Ripristino con checkpoint

```
<T1, START>
<T2, COMMIT>
<T1, A, BS_1(A), AS_1(A)>
<T3, START>
| CP: | T1, p | T3, p | OK |
<T1, COMMIT>
<T4, START>
<T5, START>
<T4, A, BS_3(A), AS_3(A)>
<T4, COMMIT>
<T3, A, BS_4(A), AS_4(A)>
<T5, A, BS_5(A), AS_5(A)>
```

- Il ripristino con checkpoint considera l'ultimo checkpoint leggendo il file dal fondo.
    - Checkpoint che fornisce l'elenco delle transazioni attive in quel momento ($T1$ e $T3$).
    - Le transazioni che hanno raggiunto il commit prima del checkpoint ($T2$) non hanno bisogno di ripristino.
        - Si sta considerando un guasto soft.
- Al momento del ripristino si esegue:
    - `UNDO` delle transazioni non ancora terminate sia iniziate prima del checkpoint ($T3$) che dopo ($T5$).
    - `REDO` delle transazioni con il commit dopo il checkpoint sia iniziate prima del checkpoint ($T1$) che dopo ($T4$).
- Algoritmo di ripresa a caldo:
    - $AT = \{T3, T5\}$: insieme delle transazioni attive durante il crash;
    - $CT = \{T1, T4\}$: insieme delle transazioni con commit dopo il checkpoint.
    - Ripristino:
        - $UNDO(T3, T5)$;
        - $REDO(T1, T4)$.
