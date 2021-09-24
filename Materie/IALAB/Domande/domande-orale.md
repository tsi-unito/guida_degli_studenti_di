# IALAB - Domande Possibili

## Pozzato

- Prolog:
  - Definizione
  - Interprete Prolog
  - Risoluzione SLD (Albero SLD, Terminazione, Non-Determinismo)
  - La risoluzione SLD è corretta e completa per tutta la logica o solo per alcuni tipi di clausole?
  - Negazione per fallimento (Ragionamento Non-Monotono)
  - 3 Possibili esiti con negazione per fallimento
  - Il CUT
  - La presenza del cut cosa va a mettere a rischio tra completezza e correttezza?
  - Se prendo delle clausole che non sono di Horn, cosa si mette a rischio tra completezza e correttezza?
- Ricerca nello spazio degli stati:
  - Strategie non informate/informate
- ASP:
  - Definizione
  - Prolog vs Clingo
  - Negazione classica vs. per fallimento
  - Modello Minimale di Herbrand
  - Programma Ridotto
  - Calcolare il Ridotto di un programma ASP
  - Nel ridotto c'è un solo answer set?
  - Integrity Constraint

## Micalizio - Planning

- Planning Classico:
  - Definizione
  - STS (Definizione, STS-Grafo)
  - Problema di pianificazione P (+ Complessità PlanSAT, Bounded PlanSAT)
    - Come formalizzarlo
    - Assunzioni
    - Cosa si assume riguardo le osservazioni
- Planning nello spazio degli stati
  - Spazio di Ricerca
  - STRIPS
  - Progression & Regression
- STRIPS:
  - Definizione (Linear Planning + Means-End Analysis)
  - Stati, Relazioni, Plan Operators, Azioni, Applicabilità, Funzione $\gamma(s,a)$
  - Algoritmo (+ Vantaggi e Svantaggi)
  - Anomalia di Sussman
  - Oltre all'anomalia di Sussman, quali altri problemi ha STRIPS?
- Least-Commitment Planning:
  - Perchè è un pianificatore particolare? In quale spazio cerca?
  - State-Variable Representation (Stato, Applicabilità, $\gamma(s,a)$)
  - Proprietà degli oggetti
  - Principio Least-Commitment
  - Piano $<A,O,B,L>$
  - Flaws (Open Goals, Threats)
  - Correttezza e Completezza
- Graph Planning:
  - Grafo di Pianificazione. Cos'è? Com'è definito? A cosa serve?
  - Tempo di costruzione del grafo. Perchè è polinomiale?
  - Quando si smette di costruire il grafo di pianificazione?
  - Grafo dei piani (Livelli, Costruzione del grafo, Livella, Mutex, Complessità e Raggiungibilità del goal)
  - Euristiche per stimare il costo della congiunzione di letterali
  - graphplan (EXTRACT-SOLUTION, GP-SEARCH, EXPAND-GRAPH, tuple no-good, terminazione e dimostrazione)
  - A cosa serve la struttura del grafo?
  - Vincoli di mutua esclusione
  - Vincoli di mutua esclusione tra le azioni
- Euristiche nel Planning:
  - Tecniche di rilassamento
  - Euristiche non ammissibili per forward e backward search
  - Se non volessimo un'euristica ammissibile, cosa faremmo?
  - Euristiche ammissibili $h_1$, $h_2$ e $h_G$
  - Euristica FAF per la selezione dei Flaws
  - Come determinare la famiglia di euristiche

## Micalizio - Sistemi Esperti

- Costruzione (Inference Engine, KB, WM, Facilities, UI)
- Conoscenza di dominio / conoscenza di controllo
- Quando non utilizzare un S.E.
- CLIPS:
  - Fatti (ordinati, non-ordinati, manipolazione)
  - Regole (LHS, RHS, Binding)
  - Rifrazione
  - Agenda (Definizione, Ordinamento Regole)
  - Se abbiamo in agenda R1 e sotto R2 ed eseguiamo R1, R2 rimane in agenda?
  - Strategie di Conflict Resolution
  - Regole e connettivi logici
  - Moduli. A cosa servono?
  - Ciclo di funzionamento di CLIPS

## Torta

- Probabilità Condizionata
- Inferenza per enumerazione
- Normalizzazione
- Paradosso di Monty-Hall
- Indipendenza (e indipendenza condizionale)
- Paradosso di Simpson
- Regola di Bayes
- Reti Bayesiane:
  - Componenti
  - Significato delle frecce nelle reti bayesiane
  - Compattezza
  - Semantica (Globale, Locale, Markov Blanket)
  - Come si costruisce
  - Reti Bayesiane Ibride
  - Rete Bayesiana Naive, calcolo delle probabilità della causa dati gli effetti
  - Task di Inferenza (Simple/Conjunctive Queries, MPE, MAP)
  - Struttura (Chain, Divergent, Convergent, Path Bloccati)
  - D-Separazione
    - La variabile è d-separata dai discendenti nel momento in cui conosco i suoi padri? Ci sono altri cammini che collegano ai suoi discendenti?
  - Effetto Causale 
    - Operatore DO.
      - Come differisce dalla probabilità condizionata?
    - causal effect rule
- Inferenza Esatta:
  - per Enumerazione
  - per Variable Elimination
  - Complessità e problemi (scelta dell'ordinamento, width, treewidth)
- Inferenza Approssimata:
  - Idea: inferenza con stochastic sampling
  - Rejection Sampling (Algoritmo, Analisi e problemi)
    - Cosa vuol dire che è consistente?
  - Likelihood Weighting (Algoritmo, Weighted-Sample, Analisi e problemi)
  - Impatto dell'ordine delle variabili
- Modelli Temporali
  - Catene di Markov (first e second order, Sensor Markov Assumption)
  - Task di Inferenza su Modelli Temporali (Filtering, Prediction, Smoothing e algoritmo forward backward)
