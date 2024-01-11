---
title: 'Intelligenza artificiale e laboratorio - Parte I - Introduzione alla programmazione logica'
---

# Intelligenza artificiale e laboratorio - Parte I

## Introduzione alla programmazione logica

### Introduzione all'intelligenza artificiale

>L'intelligenza artificiale è una disciplina appartenente all'informatica che studia i fondamenti teorici, le metodologie e le tecniche che consentono la progettazione di sistemi hardware e sistemi di programmi software capaci di fornire all’elaboratore elettronico prestazioni che, a un osservatore comune, sembrerebbero essere di pertinenza esclusiva dell’intelligenza umana [Somalvico].

- L'**intelligenza artificiale** è interessata ai **problemi intelligenti**.
    - Quei problemi per cui non esiste o non è noto un algoritmo di risoluzione.
- Sotto-aree di ricerca dell'intelligenza artificiale:
    - Rappresentazione della conoscenza e ragionamento;
    - Interpretazione/sintesi del linguaggio naturale;
    - Apprendimento automatico;
    - Pianificazione;
    - Robotica.

### Prolog

- Il `Prolog` è un linguaggio dichiarativo.
    - Si rappresentato con dei **fatti** un dominio di interesse.
    - Si rappresentano con delle **regole** delle possibili inferenze.
    - I fatti e le regole sono dette **clausole** (della risoluzione).
- Fatti e regole contengono:
    - Atomi: costanti, numeri;
    - Variabili;
    - Termini composti.
        - Ottenuti applicando un funtore a termini.

```prolog
% Fatti
gatto(tom).
gatto(fred).
tigre(mike).
graffia(fred).

% Regole
felino(X) :- gatto(X).
felino(X) :- tigre(X).
miagola(X) :- gatto(X), sveglio(X).
sveglio(X) :- graffia(X).
```

- Istruzioni per `swipl`:
    - Caricamento file: `['filename.pl'].` (*consult*).
    - Tracing: `trace.` e poi inserire il goal da analizzare.
        - `nodebug.` per uscire dal tracer.
- Predicati utili:
    - `var(X)`: verifica se è una variabile oppure no.
    - `ground(X)`: verifica se è istanziata oppure no.

```prolog
[trace]  ?- felino(mike).
   Call: (10) felino(mike) ? creep
   Call: (11) gatto(mike) ? creep
   Fail: (11) gatto(mike) ? creep
   Redo: (10) felino(mike) ? creep
   Call: (11) tigre(mike) ? creep
   Exit: (11) tigre(mike) ? creep
   Exit: (10) felino(mike) ? creep
true.
```

### Query

- `Prolog` cerca di risolvere il goal identificando anche solo una soluzione.
    - Il risultato immediato di una query è quindi unico.

```prolog
?- gatto(X).
Chi = tom .

?- gatto(X).
Chi = tom ; % Forza il backtracking
Chi = fred.
```

- Le query, quando non atomica, diventa una sequenza di goal.
    - Il sistema prova a risolvere la sequenza nell'ordine indicato.

```prolog
?- gatto(X), tigre(X).
false.

?- gatto(X), felino(X).
X = tom ;
X = fred ;
false. % Dopo aver forzato il backtracking, non trova soluzioni

tigre(X), felino(X).
X = mike. % Questo avviene per l'ordine nella dichiarazione iniziale
```

### Basi di conoscenza più complesse

#### Parentela

```prolog
% Fatti
genitore(anna, mario).
...
genitore(chiara, lia).

% Regole
nonno(X,Y) :- genitore(X, Z), genitore(Z, Y).

antenato(X, Y) :- genitore(X, Y).
antenato(X, Y) :- genitore(Z, Y), antenato(X, Z). % Regola ricorsiva
```

- Query possibili:

```prolog
?- nonno(X, lia).
?- nonno(X, _).
?- nonno(lia, X).
?- antenato(Chi, lia), antenato(anna, Chi).
```

- Si definiscono regole più complesse:
    - `X \== Y` va dichiarato dopo e non da subito.
    - `PrimoGenitore \== SecondoGenitore` va dichiarato dopo l'istanziazione di `SecondoGenitore`.

```prolog
% Entrambi i genitori in comune
fratelloGermano(X, Y) :-
    genitore(PrimoGenitore, X),
    genitore(PrimoGenitore, Y),
    X \== Y,
    genitore(SecondoGenitore, X),
    PrimoGenitore \== SecondoGenitore,
    genitore(SecondoGenitore, Y).

% Un solo genitore in comune
fratelloUnilaterale(X, Y) :-
    genitore(GenitoreComune, X),
    genitore(GenitoreComune, Y),
    X \== Y,
    genitore(GenitoreX, X),
    GenitoreX \== GenitoreComune,
    genitore(GenitoreY, Y),
    GenitoreY \== GenitoreComune,
    GenitoreX \== GenitoreY.
```

### Liste

- La **lista** è la struttura dati principale del `Prolog`.
    - Può contenere vari elementi, differenti e anche composti (anche liste).
    - Una lista è caratterizzata da una testa e da una coda.

```prolog
% Somma degli elementi di una lista
somma([], 0). % Fatto
somma([Head | Tail], R) :- somma(Tail, N), R is Head+N.

?- somma([1, 2, 4], Ris).
Ris = 7.

?- somma([1, 2, 4], 15).
false.

% Prodotto degli elementi di una lista
prodotto([], 0).
prodotto([X], X).
prodotto([Head | Tail], R) :- prodotto(Tail, PT), R is Head * PT.

?- prodotto([1, 2, 3, 4], X), X<0.
false.
```

- Predicati utili per le liste:
    - `length(Lista, N)`;
    - `member(Elemento, Lista)`;
        - `member(Variabile, Lista)` restituisce gli elementi della lista.
    - `select(Elemento, Lista, NuovaLista)` se l'elemento appartiene questo viene estratto.

### Predicati extra-logici

- Predicati extra-logici:
    - Cut;
    - `assert` e `assertz`.
        - Inseriscono un asserzione (in testa o in coda) nel database.
    - `retractall`;
        - Tutti i fatti o le clausole nel database che unicificano con il parametro sono rimossi.
    - `findall`;
        - `eg` `findall(X, member(X, [1, 2, 3]), ListaRisultato)`.
