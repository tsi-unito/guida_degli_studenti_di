---
title: 'Intelligenza artificiale e laboratorio - Parte I - Answer Set Programming'
---

# Intelligenza artificiale e laboratorio - Parte I

## Answer Set Programming

- **Answer Set Programming** (ASP):
    - Non è più goal-directed (prove), si vuole invece creare **modelli stabili**.
        - Non è presente il concetto di dimostrare un obiettivo.
        - Non è quindi necessario rispettare le clausole di Horn.
    - Nato dalla *War of semantics*.
        - Dove si cercava di dare una semantica alla negazione per fallimento adottata dagli interpreti Prolog.
    - Utile per risolvere problemi combinatori (`eg` soddisfacimento di vincoli, planning).
    - L'ASP Solver è l'equivalente all'interprete Prolog.
        - Questo crea tutto quello che è **derivabile**, inferibile.

### Codice ASP

- Codice ASP:
    - Insieme finito di regole (clausole):
        - `a :- b1, b2, …, not c1, not c2, … cm`.
        - `a`, `bi` e `cj` sono letterali nella forma `p` o `-p`.
        - Si hanno sia negazioni per fallimento (`not`) che classiche (`-`, zucchero sintattico).
            - La negazione è più forte.
            - Quella per fallimento asserisce che ci si trova in assenza di informazione esplicita.
    - **Integrity constraint**:
        - Regole senza testa.
        - Utilizzato per **filtrare dei modelli**.
        - Modellano delle **situazioni che si vogliono escludere**.
        - `eg` `:- a1, a2, …, ak`, è inconsistente che `a1, a2, …, ak` siano tutti veri.

### ASP e Prolog

- ASP e Prolog:
    - ASP è **proposizionale** (no variabili), non del prim ordine come Prolog.
    - In ASP l'ordine dei letterali non ha alcuna importanza.
    - Prolog è goal-directed, ASP no.
    - Prolog implementa il CUT, ASP no (non è necessario).
    - La SLD-risoluzione del Prolog può portare a loop, mentre gli ASP solver non lo consente.
        - Non è presente nessun elemento operazionale.

### Semantica

- **Answer Set**: modello **minimale** (o stabile).
    - Un programma ASP senza negazioni per fallimento ha un **unico modello minimale** (answer set).
        - `eg` Con $p :- q$ e $q.$ l'answer set è $\{p, q\}$.
            - $:-$ è un'implicazione.
        - `eg` Con $p :- q, r$ e $q.$ l'answer set è $\{q\}$.
            - Si hanno $2^3 = 8$ combinazioni.
            - Anche $\{p, q, r\}$ è un modello, ma $\{q\}$ è minimale.
        - Un programma con sole implicazioni può essere soddisfacibile.
            - Il suo answer set è l'insieme vuoto.
    - Il modello minimo è l'insieme più piccolo di formule vere per soddisfare tutte le formule della KB.
    - Un modello con contraddizioni, non ha un answer set.
        - Non esiste un modello stabile che lo renda vero.
    - Esistono programmi che hanno un modello ma non hanno answer set.
        - `eg` $p :- not \: p, d.$ e $d.$:

#### Ridotto

- `def` **Ridotto**: $P^S$ di un programma $P$ rispetto ad un insieme di atomi $S$.
    - Presi due insiemi di atomi diversi, si hanno due ridotti diversi.
    - Procedimento:
        - Si rimuove ogni regola il cui corpo contiene $not \: L$, per $L \in S$.
        - Si rimuove tutti i $not \: L$ dai corpi delle restanti regole (si rimuove la negazione per fallimento).
    - $P^S$ non contiene atomi con negazione per fallimento (per costruzione).
        - Ha un unico answer set.
        - Se tale answer set coincide con $S$, allora $S$ è un answer set per $P$.
    - L'ASP Solver calcola tutti i possibili ridotti rispetto tutti i possibili sottoinsiemi delle lettere.
        - Si costruisce l'insieme potenza.
        - Per ogni insieme si costruisce il ridotto (anche con l'insieme vuoto).
        - Si verifica se il modello del ridotto coincide con il sottoinsieme di letterali scelto.
    - `ex` Calcolare il ridotto di un programma con pochi letterali.
