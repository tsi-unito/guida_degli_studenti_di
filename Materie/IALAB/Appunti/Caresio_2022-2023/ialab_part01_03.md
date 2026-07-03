---
title: 'Intelligenza artificiale e laboratorio - Parte I - Strategie di ricerca'
---

# Intelligenza artificiale e laboratorio - Parte I

## Strategie di ricerca

### Ricerca nello spazio degli stati

- **Ricerca nello spazio degli stati**:
    - Si tratta di un **problema di ricerca** definito da:
        - Stato iniziale;
        - Insieme delle azioni;
            - Azione: fa passare da uno stato a un altro.
        - Specifica degli obiettivi (goal);
        - Costo di ogni azione.
    - Non tutti i problemi sono formulabili con la ricerca nello spazio degli stati.
        - Ma alcuni hanno una naturale rappresentazione in questi termini.
    - **Spazio degli stati**:
        - L'insieme di tutti gli stati raggiungibili a partire da quello iniziale.
            - Definito implicitamente dallo stato iniziale più l'insieme delle azioni.
        - Cammino: sequenza di stati collegati da una sequenza di azioni.
        - Costo di un cammino: somma dei costi delle azioni che lo compongono.
            - In alcuni casi si può assumere che il costo delle azioni sia identico per tutti.
            - In questo caso il costo del cammino è la sua lunghezza.
    - **Soluzione** ad un problema: cammino dallo stato iniziale ad uno stato di goal.
    - **Soluzione ottima**: soluzione di costo minimo tra tutte le soluzioni.

### Rappresentazione

- Gli **stati** sono **rappresentati** come **termini**.
    - Dipendono dal problema da rappresentare (con fatti, liste, etc).
- Le **azioni** sono specificate tramite **precondizioni** ed **effetti**.
    - Precondizioni: in quali stati un'azione può essere eseguita.
    - `applicazione(AZ, S)`: l'azione `AZ` è eseguibile nello stato `S`.
    - `trasforma(AZ, S, NUOVO_S)`:
        - Se l'azione `AZ` è applicabile a `S`;
        - Lo stato `NUOVO_S` è il risultato dell'applicazione di `AZ` allo stato `S`.

#### Problema: cammini in un labirinto

- Si rappresenta lo stato iniziale del problema.
    - Si introducono due fatti per stabilire la dimensione della mappa.
    - Si introducono due fatti per stabilire la posizione iniziale e finale.
    - Gli ostacoli vengono rappresentati con tanti `occupata(pos(x, y))`.
- Si devono anche rappresentare le azioni.
    - Alcune azioni sono possibili solo in determinate condizioni.
    - Si introducono quattro clausole, corrispondente alle quattro azioni di spostamento.
    - `eg` `findall(Azione, applicabile(Azione, pos(5, 8)), ListAzioni)`.
- In questo caso lo spazio degli stati ha grandezza massima $width \times height$.

### Strategie non informate

- **Strategie non informate** (*blind*):
    - **Ricerca a profondità limitata**;
        - Si utilizza un parametro che vincola la profondità massima di espansione dei nodi.
        - Strategia **non completa**.
    - **Iterative deepening**;
        - Ripete la ricerca a profondità limitata, incrementando ad ogni passo il mite.
        - Strategia **ottima** nel caso di azioni di costo unitario.
    - **Ricerca in ampiezza**;
        - Coda di nodi.
        - Ad ogni passo:
            - La procedura espande il nodo in testa alla coda (`findall`) generando tutti i suoi successori;
            - Questi vengono aggiunti in fondo alla coda.
        - Maggiore utilizzo di spazio, ma **ottimalità della soluzione**.
        - La soluzione non è però determinabile dalle chiamate ricorsive (come in profondità).
            - Una soluzione è rappresentare ogni stato come una coppia con:
                - Il valore dello stato stesso;
                - Il percorso che ha permesso di raggiungere quello stato.
    - **Ricerca in ampiezza su grafi**:
        - Si considera la **lista chiusa** dei nodi già espansi.
        - Prima di espandere un nodo, si verifica che non sia chiuso.
        - Il nodo chiuso non viene ulteriormente espanso.

### Strategie informate

- **Strategia informate**:
    - Si associa un costo a ciascuna azione.
    - Si utilizza una **funzione euristica** $h(n)$.
        - $h(n)$: costo stimato del cammino più conveniente dal nodo $n$ ad uno stato finale.
        - L'euristica **focalizza** la ricerca.
        - Si tratta di una stima (in base al dominio), non è detto che il goal sia raggiungibile.
        - Si tratta in media di una **sottostima**, sempre minore del vero costo.
        - Se l'euristica è **ammissibile**, la soluzione è **ottima**.
            - In generale non è detto che la soluzione ottima sia indispensabile (`eg` tempo).
            - Se l'euristica ha un $\Delta$ (sovrastima) rispetto alla migliore, il $\Delta$ alla soluzione ottima è proporzionale.
    - Si definisce una funzione $g(n)$.
        - $g(n)$: costo del cammino trovato dal nodo iniziale a $n$.
        - Si definisce questa funzione proprio perché $h(n)$ è una stima.
        - Si tiene conto del passato, quanto si è speso per arrivare a quello stato.

#### Ricerca in profondità IDA-Star

- **Ricerca in profondità IDA-Star**:
    - Iterative deepening ma con soglia stimata ad ogni passo in base all'euristica.
        - Al primo passo la soglia è $h(si)$, dove $si$ è lo stato iniziale.
        - Ad ogni iterazione, la soglia è il minimo $f(n) = g(n) + h(n)$ per tutti i nodi $n$ che superavano la soglia al passo precedente (*backtracking*).
    - Usare `assert` per salvare $f(n)$ in caso di fallimento.

#### Ricerca in ampiezza A-Star

- **Ricerca in ampiezza A-Star**:
    - Ricerca in ampiezza su grafi che tiene conto della funzione euristica.
        - Ad ogni passo si estrae per l'espansione della coda il nodo con minimo valore di $f(n) = g(n) + h(n)$.
        - I nodi già espansi non vengono espansi.
