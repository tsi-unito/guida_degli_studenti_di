# Micalizio

- [Micalizio](#micalizio)
  - [**Planning**](#planning)
    - [**Agente**](#agente)
      - [**Definizione**](#definizione)
      - [**Agente Intelligente**](#agente-intelligente)
      - [**Razionalità**](#razionalità)
      - [**Tipi di agenti**](#tipi-di-agenti)
    - [**Planning Classico**](#planning-classico)
      - [**Definizione**](#definizione-1)
      - [**STS** (Definizione, STS-Grafo)](#sts-definizione-sts-grafo)
        - [**State Transition System**](#state-transition-system)
        - [**Componenti dell'STS**](#componenti-dellsts)
        - [**STS-Grafo**](#sts-grafo)
    - [**Planning e Plan Execution**](#planning-e-plan-execution)
    - [**Assunzioni e Rilassamenti**](#assunzioni-e-rilassamenti)
    - [**Problema di pianificazione $\mathcal{P}$** (+ Complessità PlanSAT, Bounded PlanSAT)](#problema-di-pianificazione-mathcalp--complessità-plansat-bounded-plansat)
      - [**Definizione**](#definizione-2)
    - [**Proprietà di un buon algoritmo di pianificazione**](#proprietà-di-un-buon-algoritmo-di-pianificazione)
      - [**Soundness** (correttezza)](#soundness-correttezza)
      - [**Completeness** (completezza)](#completeness-completezza)
      - [**Ottimalità**](#ottimalità)
      - [**Complessità**](#complessità)
    - [**Algoritmi di Pianificazione**](#algoritmi-di-pianificazione)
    - [**STRIPS**](#strips)
      - [**Definizione** (Linear Planning + Means-End Analysis)](#definizione-linear-planning--means-end-analysis)
        - [**Linear Planning**](#linear-planning)
        - [**Means-End Analysis**](#means-end-analysis)
      - [**Stati**](#stati)
      - [**Tipi di Relazioni**](#tipi-di-relazioni)
      - [**Plan Operators**](#plan-operators)
      - [**Azione, Applicabilità e Funzione $\gamma(s,a)$**](#azione-applicabilità-e-funzione-gammasa)
      - [**Algoritmo** (+ Vantaggi e Svantaggi)](#algoritmo--vantaggi-e-svantaggi)
      - [**Anomalia di Sussman**](#anomalia-di-sussman)
      - [**Incompletezza di STRIPS**](#incompletezza-di-strips)
      - [**PDDL**](#pddl)
    - [**Progression e Regression**](#progression-e-regression)
      - [**Progression**](#progression)
      - [**Regression**](#regression)
      - [**Progression vs Regression**](#progression-vs-regression)
    - [**CSP**](#csp)
      - [**Come costruire un modello espresso da variabili di stato?**](#come-costruire-un-modello-espresso-da-variabili-di-stato)
      - [**Proprietà nei CSP**](#proprietà-nei-csp)
      - [**State-Variable Representation** (Stato, Applicabilità, $\gamma(s,a)$)](#state-variable-representation-stato-applicabilità-gammasa)
    - [**Least-Commitment Planning**](#least-commitment-planning)
      - [**Principio**](#principio)
      - [**Perchè è un pianificatore particolare? In quale spazio cerca?**](#perchè-è-un-pianificatore-particolare-in-quale-spazio-cerca)
      - [**Piano $\langle A,O,B,L\rangle$**](#piano-langle-aoblrangle)
      - [**Flaws** (Open Goals, Threats)](#flaws-open-goals-threats)
    - [**Correttezza e Completezza di PSP** (Plan-Space Planning)](#correttezza-e-completezza-di-psp-plan-space-planning)
      - [**Algoritmo PSP**](#algoritmo-psp)
    - [**Graph Planning**](#graph-planning)
      - [**Grafo di Pianificazione. Cos'è? Com'è definito? A cosa serve?**](#grafo-di-pianificazione-cosè-comè-definito-a-cosa-serve)
      - [**Grafo dei piani** (Livelli, Costruzione del grafo, Livella, Mutex, Complessità e Raggiungibilità del goal)](#grafo-dei-piani-livelli-costruzione-del-grafo-livella-mutex-complessità-e-raggiungibilità-del-goal)
      - [**Tempo di costruzione del grafo/Complessità. Perchè è polinomiale?**](#tempo-di-costruzione-del-grafocomplessità-perchè-è-polinomiale)
      - [**Euristiche per stimare il costo**](#euristiche-per-stimare-il-costo)
      - [**graphplan** (EXTRACT-SOLUTION, GP-SEARCH, EXPAND-GRAPH, tuple no-good, terminazione e dimostrazione)](#graphplan-extract-solution-gp-search-expand-graph-tuple-no-good-terminazione-e-dimostrazione)
    - [**Euristiche nel Planning**](#euristiche-nel-planning)
      - [**Info**](#info)
      - [**Tecniche di rilassamento**](#tecniche-di-rilassamento)
      - [**Euristiche non ammissibili**](#euristiche-non-ammissibili)
      - [**Euristiche ammissibili $h_1$, $h_2$ e $h_G$**](#euristiche-ammissibili-h_1-h_2-e-h_g)
      - [**Least Commitment Planning come ricerca AND/OR, Euristica FAF per la selezione dei Flaws**](#least-commitment-planning-come-ricerca-andor-euristica-faf-per-la-selezione-dei-flaws)
  - [**Sistemi Esperti**](#sistemi-esperti)
    - [**Proprietà**](#proprietà)
    - [**Costruzione** (Inference Engine, KB, WM, Facilities, UI)](#costruzione-inference-engine-kb-wm-facilities-ui)
    - [**Conoscenza di dominio / conoscenza di controllo**](#conoscenza-di-dominio--conoscenza-di-controllo)
    - [**Quando (non) utilizzare un Ssistema Esperto**](#quando-non-utilizzare-un-ssistema-esperto)
    - [**CLIPS**](#clips)
      - [**Regole (LHS, RHS, Binding)**](#regole-lhs-rhs-binding)
      - [**Rifrazione**](#rifrazione)
      - [**Agenda** (Definizione, Ordinamento Regole, Conflict resolution)](#agenda-definizione-ordinamento-regole-conflict-resolution)
      - [**Se abbiamo in agenda $R_1$ e sotto $R_2$ ed eseguiamo $R_1$, $R_2$ rimane in agenda?**](#se-abbiamo-in-agenda-r_1-e-sotto-r_2-ed-eseguiamo-r_1-r_2-rimane-in-agenda)
      - [**Strategie di Conflict Resolution**](#strategie-di-conflict-resolution)
      - [**Regole e connettivi logici**](#regole-e-connettivi-logici)
      - [**`Gensym` e `Gensym\*`**](#gensym-e-gensym)
      - [**Moduli. A cosa servono?**](#moduli-a-cosa-servono)

## **Planning**

### **Agente**

#### **Definizione**

Il modello ad agenti è un'**astrazione per modularizzare il codice** in

- **Agente**: Processo che persegue un certo obiettivo. Utilizza una _funzione agente_ per decidere la prossima azione date le percezioni finora ricevute
- **Ambiente**: dati e risorse computazionali usate dagli agenti

#### **Agente Intelligente**

Essendo un'astrazione consente di analizzare un problema in termini di alto livello (ci si concentra sui **concetti** e non sull'implementazione)  
Ha due aspetti fondamentali:  

- **Agency**: agisce in un ambiente che _percepisce_ con dei sensori e che _modifica_ con attuatori.
- **Autonomia**: con le proprie conoscenze dell'ambiente sa prendere decisioni; può anche saper modificare il proprio modello se l'ambiente cambia
- **Razionalità**: Si deve comportare correttamente, cioè nel miglior modo possibile date le informazioni sull'ambiente.

L'agente genera azioni $\rightarrow$ l'ambiente evolve grazie ad esse in una sequenza di stati. Se la sequenza è quella desiderata, l'agente ha conseguito il **goal**, comportandosi quindi in modo **razionale**.

#### **Razionalità**

Impone una misura di prestazione! Bisogna valutare la sequenza degli stati, e l'agente potrebbe pensare di star agendo bene (anche se questo potrebbe non essere il caso).

La razionalità dipende da:

- Come è definito il criterio di successo
- Conoscenza pregressa dell'ambiente da parte dell'agente (il **modello**)
- Azioni che l'agente può eseguire
- Percezioni ricevute finora (non tutto è osservabile!)

L'agente è razionale se **MASSIMIZZA** il risultato: se sceglie le azioni che massimizzano il risultato atteso della misura di prestazione.

#### **Tipi di agenti**

- **Reattivi semplici**: vedono lo stato attuale e decidono un'azione
- **Reattivi basati sul modello**: ho uno **stato interno** (sintesi delle percezioni finora ricevute) e ho un modello del mondo che mantiene lo stato aggiornato. Se ci sono ambiguità si ragiona in termini della migliore ipotesi corrente.
- **Basati su Obiettivi**: La scelta della prossima azione non dipende unicamente dalla situazione corrente, ma anche da cosa si vuole raggiungere (il goal). Bisogna ragionare sul futuro: il modello del mondo ci consente di prevedere le conseguenze delle azioni. In questa categoria ricade il planning classico.
- **Basati sull'Utilità**: se esistono percorsi alternativi, potrei voler scegliere quello migliore rispetto una specifica grandezza di interesse (costo, tempo, ecc.).
  
### **Planning Classico**

#### **Definizione**

E' un processo deliberativo che sceglie ed organizza le azioni in base all'effetto che ci si aspetta che queste producano. Abbiamo due tipi di planning, che spesso si completano a vicenda:

- **Domain-Dependent Planning**: usa rappresentazioni e tecniche adattate alle specifiche condizioni di un dominio di pianificazione
- **Domain-Independent Planning**: usa rappresentazioni e tecniche generiche, che non dipendono da un dominio specifico. Si basa sugli **aspetti comuni** presenti in tutti i domini di pianificazione. Ci consente di considerare il planning come un problema generale: molti problemi sono riconducibili ad esso.

#### **STS** (Definizione, STS-Grafo)

##### **State Transition System**

$\Sigma = (S,A,E,\gamma)$, dove:

- $S=\{s_1,s_2,...\}$ insieme finito, ricorsivamente enumerabile di stati
- $A=\{a_1,a_2,...\}$ insieme finito, ricorsivamente enumerabile di azioni
- $E=\{e_1,e_2,...\}$ insieme finito, ricorsivamente enumerabile di eventi
- $\gamma:S\times(A \cup E) \rightarrow 2^S$ è una relazione di transizione di stato:  
  Se $a \in A$ e $\gamma(s,a)\neq \emptyset$, allora $a$ è **applicabile** in $S$.  
  Applicare $a$ in $s$ causerà una transizione di stato del sistema da $s$ a $s'$, dove $s'\in\gamma(s,a)$

##### **Componenti dell'STS**

- L'**STS** descrive tutte le possibili evoluzioni di un sistema.  
- Un **Piano** traccia tutte le azioni necessarie per raggiungere un determinato obiettivo $G$ dato uno stato iniziale $I$. Si può pensare ad esso come un cammino da $I$ a $G$ nello spazio degli stati tracciato dall'STS.
- I **Goal** possono essere di diversa tipologia:
  - Uno **stato goal** $s_g$ o un sottoinsieme di possibili stati goal in $S_g$
  - Soddisfacimento di condizioni in tutta la sequenza di stati prodotta dalle azioni (carburante sempre > 0)
  - Ottimizzazione di funzioni di utilità (minimizzare costi, tempi, massimizzare profitti)
  - Vincoli sulle azioni che possono essere eseguite (azione a che deve essere seguita subito o entro un tempo limite da un'azione b).

##### **STS-Grafo**

Un STS $\Sigma =(S,A,E,\gamma)$ può essere rappresentato come un grafo direzionato $G=(N_g,E_g)$:

- $N_g=S$ è l'insieme dei nodi del grafo coincidente con l'insieme degli stati di $\Sigma$
- $E_g$ è l'insieme degli archi del grafo tale che esiste un arco $s\xrightarrow{u}s'$ (anche rappresentabile come $\langle s,u,s' \rangle$) da $s$ a $s'$ etichettato con $u \in A \cup E$ **se e solo se**:
  - $s,s'\in S$ (stato iniziale e finale fanno parte degli stati dell'STS) e
  - $s'=\gamma(s,u)$ (la transizione da $s$ a $s'$ mediante la funzione di transizione gamma $\gamma$ è ottenibile con l'azione/evento $u$).

### **Planning e Plan Execution**

- **Planner**: Data la descrizione di un STS $\Sigma$, lo stato iniziale e il goal, genera un piano che raggiunge il goal dallo stato iniziale.
- **Controller**: Dato un piano e lo stato corrente (funzione di osservabilità $\eta:S\rightarrow O$ (eta)), seleziona ed esegue un'azione del piano
- **STS $\Sigma$**: evolve in funzione delle azioni che vengono eseguite e degli eventi che possono accadere.

(Si assume che gli eventi non interferiscano con le azioni del Controller)

Abbiamo un problema: il mondo reale può essere diverso da come è descritto nel modello $\Sigma$.  
Un approccio più realistico è quello del **continual planning**: consente un loop chiuso di feedback tra planner e controller. In questo caso gli eventi che occorrono possono interferire con le azioni pianificate (guasti ad esempio).

### **Assunzioni e Rilassamenti**

1) **Dominio finito**: $\Sigma$ contiene un numero finito di stati.  
 Si rilassa per descrivere _azioni che producono nuovi oggetti nel mondo_ e per trattare fluenti numerici.  
 Andiamo ad intaccare la **decidibilità e la terminazione del pianificatore**.
2) **Completa osservabilità**: la funzione $\eta:S\rightarrow O$ è la funzione identità  
 Si rilassa per trattare stati in cui non tutto è osservabile o può essere conosciuto  
 Può accadere che $\eta(s) = \eta(s') = o$ con $s \neq s'$.  
 Le osservazioni sono **ambigue** perchè consistenti con più stati possibili. Può essere difficile determinare lo stato successore.
3) **Determinismo**: $\Sigma$ è deterministico, cioè per ogni $s \in S, u \in A \cup E$ si ha $|\gamma(s,u)|\leq1$ (da un'evento/azione si può andare in più stati alternativi).  
 Si può voler rilassare per pianificare azioni che possono avere risultati alternativi, ma il controller deve essere in grado di osservare il risultato reale di ogni azione e il piano potrebbe avere dei punti di controllo condizionali o iterativi.
4) **Dominio Statico**: $\Sigma$ è statico, ovvero $E=\empty$ e quindi l'STS è riducibile a $\Sigma=(S,A,\gamma)$
 Si rilassa perchè si può voler modellare domini con eventi non controllabili dall'esecutore.  
 Il mondo però diventa **non deterministico** dal punto di vista del pianificatore.
5) **Goal Semplici**: Vogliamo poter rilassare il vincolo per poter lavorare su stati e piani, con funzioni di utilità/costo, ottimalità.  
 Il planning però diventa più costoso computazionalmente.
6) **Piani Sequenziali**: un piano soluzione è una sequenza finita di azioni **linearmente ordinate**: solo una alla volta è possibile eseguire.  
 Lo si vuole rilassare per poter eseguire più azioni in parallelo e per non introdurre vincoli che non sono parte del dominio, ma si dovrà ragionare (e gestire) su strutture dati più complesse.
7) **Tempo Implicito**: Le azioni e gli eventi non hanno durata o, in altri termini, hanno tutti durata istantanea.  
 Si può voler trattare azioni durative, problemi di concorrenza e deadline.  
 Problemi: può essere difficile ragionare e rappresentare il tempo; gli effetti delle aizoni si sviluppano nel tempo: come si comporterà il controller?
8) **Offline Planning**: $\Sigma$ non cambia mentre il pianificatore sta inferendo un piano: bisognerà poter controllare che il piano corrente sia ancora valido, altrimenti bisogna ripianificare.
9) **Single Agent**: Abbiamo un solo pianificatore e controller/esecutore  
 Si può voler sfruttare meglio le risorse disponibili (più pianificatori), oppure simulare situazioni in cui _più esecutori sono presenti_ ma non sono sotto il controllo dello stesso pianificatore.  Bisognerà poter trattare le interazioni, la coordinazione, competizione, negoziazione.
10) **Closed World Assumption**: Quello che non è esplicitamente vero in uno stato è assunto falso.

### **Problema di pianificazione $\mathcal{P}$** (+ Complessità PlanSAT, Bounded PlanSAT)  

#### **Definizione**

Un problema di pianificazione classica $\mathcal{P}=(\Sigma,s_0,S_g)$ ha:

- $\Sigma=(S,A,\gamma)$, modello del dominio espresso come STS (State Transition System)
- $s_0 \in S$ è lo stato iniziale
- $S_g \subset S$ è l'insieme degli stati goal

Una soluzione $\pi$ ad un problema $\mathcal{P}$ è una sequenza totalmente ordinata di azioni istanziate (**ground**, cioè nelle quali non compaiono formule) $\pi = \langle a_1,...,a_n\rangle$ che danno origine ad una sequenza di transizioni di stato $\langle s_0,...,s_n \rangle$ tale che:

- $s_1=\gamma(s_0,a_1)$ (la prima azione applicata con la funzione di transizione gamma porta allo stato $s_1$)
- $\forall k: 2..n, s_k = \gamma(s_{k-1},a_k)$ (tutte le azioni successive in sequenza, applicate con la f.ne di transizione gamma allo stato precedente portino allo stato direttamente successivo)
- $s_n \in S_g$ (lo stato finale raggiunto dal piano faccia parte degli stati goal)

### **Proprietà di un buon algoritmo di pianificazione**

#### **Soundness** (correttezza)

Un pianificatore è **corretto** se tutte le soluzioni che trova sono piani corretti, ovvero realmente eseguibili dal controller. Questo significa che:  

- Tutti i goal sono soddisfatti
- Nessuna precondizione di azione non insoddisfatta (o mancante)
- Nessun vincolo ulteriore è violato (temporali ad esempio)
  
#### **Completeness** (completezza)

Un pianificatore è **completo** se trova una soluzione quando il problema è risolubile.  
Un pianificatore è **strettamente completo** se, nonostante esso faccia (ad esempio) pruning dell'albero di ricerca, non scarta alcuna soluzione possibile.

#### **Ottimalità**

Un pianificatore è **ottimo** se l'ordine con cui le soluzioni sono trovate è coerente con una qualche misura di qualità dei piani (lunghezza, costo complessivo)
  
#### **Complessità**

Si può dimostrare che il planning è un compito computazionalmente costoso ricorrendo a due problemi decisionali:

- **PlanSAT**: esiste un piano che risolve un problema di pianificazione?
- **Bounded PlanSAT**: esiste un piano di lunghezza $k$?

Per la pianificazione classica entrambi i problemi sono decidibili (la ricerca avviene in spazio finito), ma se estendiamo il linguaggio con simboli di funzione:

- Lo spazio di ricerca diventa infinito
- PlanSAT diventa **semidecidibile**:
- Esiste un algoritmo che termina quando la soluzione esiste MA
- Potrebbe non terminare quando la soluzione non esiste.
- Bounded PlanSAT **rimane decidibile**.

PlanSAT e Bounded PlanSAT sono $PSPACE \supset NP$.  
In molti casi pratici:

- Bounded PlanSAT è NP-Completo
- PlanSat è Polinomiale

Questo significa che trovare una soluzione è meno costoso che trovare una soluzione ottima.  
Può quindi cercare euristiche (magari) domain independent che guidino il pianificatore nella formulazione di una soluzione.

### **Algoritmi di Pianificazione**

Inizialmente potremmo risolvere il planning con algoritmi di ricerca (BSF, DSF, A*...)  
**MA** ci sono delle differenze!  
lo spazio di ricerca $\not \equiv$ spazio degli stati:

- Lo spazio degli stati è indotto dal transition system $\Sigma$
- Lo spazio di ricerca dipende dall'algoritmo di pianificazione (e non sempre cerca nello spazio degli stati, come nel caso del least-commitment planning).

Variabili:  

1) Direzione della ricerca:  
    - Progression: $s_i \rightarrow s_g$
    - Regression: $s_i \leftarrow s_g$
    - Bidirezionale $s_i \leftrightarrow s_g$
2) Rappresentazione dello spazio di ricerca
    - Spazio di ricerca **esplicito**: coincide con lo spazio degli stati del transition system che modella il dominio
    - Spazio di ricerca **simbolico**: ogni stato corrisponde ad insiemi di stati del dominio
3) Algoritmo di ricerca
    - Non Informato (DFS, BFS, Iterative Deepening...)
    - Ricerca Euristica Sistematica (greedy best first, A*, IDA*...)
    - Ricerca Euristica locale (hill climbing, simulated annealing, beam search...)
4) Controllo della ricerca
    - Euristiche per gli algoritmi informati
    - Tecniche di pruning (partial-order reduction, helpful actions pruning, symmetry elimination...)
  
### **STRIPS**

(STanford research Institute Problem Solver):

#### **Definizione** (Linear Planning + Means-End Analysis)

Introduce una rappresentazione esplicita degli **operatori di pianificazione**.  
Fornisce una operazionalizzazione delle nozioni di

- differenza tra stati
- subgoal
- applicazione di un operatore

E' in grado di gestire il frame problem.  
Si basa su due idee fondamentali:

##### **Linear Planning**

L'idea di base è di risolvere un goal alla volta, passando al successivo solo quando il precedente è stato raggiunto.  
L'algoritmo di planning mantiene uno **Stack dei Goal** (un goal per essere risolto può richiedere la risoluzione di sottogoal). Ne consegue che:

- Non c'è interleaving nel conseguimento dei goal
- La ricerca è efficiente se i goal non interferiscono troppo tra di loro
- E' soggetto all'Anomalia di Sussmann

##### **Means-End Analysis**

L'idea è di considerare solo gli aspetti rilevanti al problema (ricerca backward). Si ragiona su quali mezzi (_means_, operatori) sono disponibili e necessari per raggiungere il goal.  
Occorre stabilire quali differenze ci sono tra lo stato corrente e il goal (means-end analysis), trovare un operatore che riduca tale differenza e ripetere l'analisi sui sottogoal ottenuti per regressione attraverso l'operatore scelto.

#### **Stati**
  
Il linguaggio di STRIPS $\subset$ FOL: abbiamo un numero finito di simboli di predicati, simboli costanti e **non sono presenti** simboli di funzione o quantificatori.  
Uno stato in STRIPS è una congiunzione di **atomi ground** (privi di simboli di funzione).  
Un atomo ground $p$ **vale in uno stato** $s\ sse\ p \in s$.  
La semantica è logic-oriented: uno stato $s$ soddisfa una congiunzione di letterali $g$ $s \vDash g$ se:

- Ogni letterale positivo in $g$ occorre in $s$
- Ogni letterale negativo in $g$ **non** occorre in $s$

Mondo dei blocchi:
$$s=\{On(A,B),\\OnTable(C),\\OnTable(A),\\Block(A),\\Block(B),\\Block(C),\\Handempty\}$$

#### **Tipi di Relazioni**

- **Fluenti**  
  Predicati che rappresentano relazioni il cui valore di verità può cambiare da uno stato al successivo ($On$, $OnTable$)
- **Persistenti** (o state invariant)  
  Predicati il cui valore di verità non può cambiare ($Block$)

#### **Plan Operators**

Un operatore di pianificazione in STRIPS è una **TRIPLA**:  
$o:(name(o), precond(o), effects(o))$:

- $name(o)$ è un'espressione sintattica con forma $$n(x_1,...,x_k)$$ dove n è il nome dell'operatore e $x_1,...,x_k$ è una lista di variabili che compaiono in $o$
- $precond(o)$ è l'insieme dei letterali che rappresentano le condizioni dell'azione:
- $precond^+(o)$ denota i letterali positivi
- $precond^-(o)$ denota i letterali negativi
- $effects(o)$ è l'insieme di letterali che rappresentano gli effetti dell'azione.  
Nota: una variabile può comparire negli effetti solo se è anche menzionata nelle precondizioni (_variable bounded_)
- $effects^+(o)$ denota i letterali positivi (_add-list_)
- $effects^-(o)$ denota i letterali negativi (_delete-list_)

```STRIPS
Pick_Block(?b, ?c)  
Pre: Block(?b), Handempty, Clear(?b), On(?b, ?c), Block(?c)  
Eff: Holding(?b), Clear(?c), ¬Handempty, ¬On(?b, ?c)  

Put_Block(?b, ?c)  
Pre: Block(?b), Holding(?b), Clear(?c), Block(?c)  
Eff: ¬Holding(?b), ¬Clear(?c), Handempty, On(?b, ?c)
```

#### **Azione, Applicabilità e Funzione $\gamma(s,a)$**

Sia $s$ uno stato del mondo ($s \in S$ per un dato dominio $\Sigma$)  
Sia $a$ un'**Azione** (istanziazione di un plan operator $o$)  
Diremo che $a$ è **applicabile** in s se e solo se:

- $precond^+(a)\subseteq s$ (le precondizioni sono nello stato $s$)
- $precond^-(a)\cap s = \empty$ (le precondizioni che non devono essere in $s$ non lo sono effettivamente)

La funzione di transizione di stato $\gamma(s,a)$ è definita come:

- Se $a$ è applicabile in $s$:  
$\gamma(s,a) = (s \setminus effects^-(a)) \cup effects^+(a) $
- Altrimenti $\gamma(s,a)$ non è definita.

#### **Algoritmo** (+ Vantaggi e Svantaggi)

```?
STRIPS (initState, goals )
state = initState; plan = []; stack = []
Push goals on stack
Repeat until stack is empty
    ◦ If top of stack is a goal g satisfied in state, then pop stack
    (se in cima allo stack c'è un goal atomico/complesso soddisfatto, non devo fare nulla)
    ◦ Else if top of stack is a conjunctive goal g, then
    (se in cima allo stack c'è un goal complesso, semplificalo [linearizzazione])
        Select an ordering for the subgoals of g, and push them on stack 
    ◦ Else if top of stack is a simple goal sg, then
    (se in cima allo stack c'è un goal atomico, risolvilo [means-end])
        Choose an operator o whose effects + matches goal sg
        Replace goal sg with operator o
        Push the preconditions of o on stack
    ◦ Else if top of stack is an operator o, then
    (se c'è un operatore completamente istanziato [azione], eseguilo; modifica lo stato corrente e aggiungilo al piano in costruzione)
        state = apply(o,state)
        plan = [plan; o]
```  

L'algoritmo implementa una forma di linear planning.

- **Vantaggi**
  - Spazio di ricerca ridotto: i goal sono considerati uno alla volta
  - ideale quando i goal sono indipendenti tra di loro
  - è sound
- **Svantaggi**  
  - Il linear planning può produrre piani **subottimi** (se come qualità usiamo la lunghezza)  
    E' una conseguenza dell'interdipendenza tra i goal e di un possibile ordinamento sfavorevole di essi.  
    La combinazione di questi due fattori può dare origine all'anomalia di Sussmann.
  - Il linear planning è incompleto

#### **Anomalia di Sussman**

Uno degli svantaggi di STRIPS è che il linear planning può produrre piani subottimi (se come parametro di qualità ci basiamo sulla lunghezza).  
L'interdipendenza tra i goal e un possibile ordinamento sfavorevole può portare all'anomalia, che si verifica quando è necessario **disfare** parte dei goal già raggiunti per poter risolvere l'intero problema.

#### **Incompletezza di STRIPS**

Il linear planning non è completo: questa è una conseguenza della **non reversibilità** delle azioni in alcuni domini specifici (ad esempio la logistica e il trasporto di merci).

#### **PDDL**

Risolve alcuni problemi di STRIPS:

1) Potere espressivo, che permette di semplificare i predicati e allontanarsi dalla FOL
2) Proposizionalizzazione per alcune specifiche condizioni ed azioni
Alcune estensioni di PDDL:

- Conditional effects (alcuni effetti sono raggiungibili solo se vere alcune condizioni)
- Durative Actions
- Fluenti Numerici
- Quantificatori ($\exists, \forall$)

### **Progression e Regression**

#### **Progression**

Calcolo lo stato successione $s'$ di uno stato $s$ rispetto all'applicazione di un operatore $o$: $s' = \gamma(s,o)$  
_Soundness_: corretto: se la funzione termina con un piano come soluzione, quella è effettivamente una soluzione per il problema iniziale  
_Completeness_: completo: se esiste una soluzione, allora esisterà un percorso d'esecuzione che restituirà quella soluzione come piano.

#### **Regression**

Dato che il numero di azioni applicabili in uno stato è generalmente molto grande, anche il branching factor tende ad essere grande: la ricerca in avanti rischia di non essere praticabile dopo pochi passi.

#### **Progression vs Regression**

1) La ricerca in avanti si parte da un solo stato iniziale.  
  La ricerca in indietro comincia da un insieme di stati goal.
2) Quando si applica in avanti un operatore $o$ ad uno stato $s$ si genera un unico successore $s'$.  
  Quando da uno stato $s'$ vogliamo fare un passo all'indietro, scopriamo che ci possono essere _molteplici stati predecessori_.
3) Nella ricerca in avanti lo spazio di ricerca coincide con lo spazio degli stati (stati del dominio originati da $\Sigma$ generalmente)
  Nella ricerca all'indietro ogni stato dello spazio di ricerca corrisponde ad un insieme di stati del dominio.

La ricerca all'indietro ha il vantaggio di poter gestire più stati simultaneamente (belief states), portando ad uno spazio di ricerca più compatto.  
Inoltre, si basa su azioni **rilevanti**, cioè quelle che contribuiscono attivamente al goal. Otteniamo così un branching factor più ridotto, ma il fatto di dover mantenere un belief state può complicare il planner.

### **CSP**

(Constraint Satisfaction Problems)  
Formulare un problema di pianificazione come un problema di soddisfacimento di vincoli consente di ritardare alcune decisioni fino a quando non è strettamente necessario.

#### **Come costruire un modello espresso da variabili di stato?**

- Ambiente E che vogliamo modellare
- B è l'insieme degli oggetti rilevanti per il problema che si vuole risolvere. Deve astrarre i dettagli insignificanti.

#### **Proprietà nei CSP**

- **Rigid**  
  Sono proprietà invarianti: persistono in ogni possibile stato del sistema (relazioni di adiacenza: $adjacent = \{(d_1,d_2),(d_2,d_1),...\}$).  
  Nella FOL si sarebbe dovuto usare atomi ground ($adjacent(d_1,d_2)$)
- **Varying**  
  Possono cambiare con le transizioni di stato  
  Nella rappresentazione classica sono le relazioni fluenti che vengono aggiunte/tolte per effetto delle azioni (add e delete list).  
  Sono modellate come **variabili** a cui si può assegnare un valore.

#### **State-Variable Representation** (Stato, Applicabilità, $\gamma(s,a)$)

  FOL e State-Variable sono equivalenti: Stesso valore espressivo e riconducibili una all'altra.

- **Stato**  
  Uno stato è rappresentato come una funzione che mappa ogni variabile $x$ ad un valore in $Range(x)$

- **Applicabilità**  
  Un'azione $a$ è applicabile in $s$ se
  - per ogni costante positiva $l \in pre(a), l \in s$
  - per ogni costante negativa $l \in pre(a), l \not \in s$
  - per ogni test di uguaglianza $sv(t_1,...,t_k) = t_0$ in $pre(a)$, allora vale $s(sv(t_1,...,t_k)) = t_0$
  - per ogni test di disuguaglianza $sv(t_1,...,t_k) \neq t_0$ in $pre(a)$, allora vale $s(sv(t_1,...,t_k)) \neq t_0$

- **Transizione di stato**  
  Se $a$ è applicabile in $s$, allora $\gamma(s,a) = s'$ tale che
  $$\{(x,w)|x \leftarrow w \in eff(a)\} \cup \{(x,w) \in s|x\ non\ occorre\ in\ eff(a)\}$$
  dove $(x, w)$ è la coppia (variabile, valore assegnato alla variabile).

### **Least-Commitment Planning**

#### **Principio**

Fare scelte solo quando queste sono indispensabili per risolvere una parte del problema.  
Durante la ricerca bisogna non porre più vincoli di quelli strettamente necessari.  
E' possibile ritardare:

- Ordinamenti (non ordinare se non necessario)
- Bindings: non vincolare le variabili se non è necessario unificarle con costanti per conseguire i goal

#### **Perchè è un pianificatore particolare? In quale spazio cerca?**

E' particolare perchè fa scelte solo quando indispensabili per risolvere il problema (ritarda ordinamenti e bindings).  
Cerca nello **SPAZIO DEI PIANI PARZIALI**. Ogni nodo della ricerca è un piano parzialmente ordinato con flaws, che ad ogni passo sono rimosse attraverso raffinamenti incrementali.  
Se l'algoritmo termina con successo, il piano risultante è completamente istanziato (ma solo parzialmente ordinato).

#### **Piano $\langle A,O,B,L\rangle$**

Un piano è una tupla $\langle A,O,B,L\rangle$ dove

- $A$ insieme di azioni anche solo parzialmente istanziate
  - $a_0$/$a_{start}$ è un'azione senza precondizione e tale che $effects^+(a_0) \equiv s_0$
  - $a_\infin$/$a_{finish}$ è un'azione senza effetti e tale che $pre(a_\infin) \equiv s_{goal}$
- $O$ insieme di vincoli di ordinamento della forma $(a_i<a_j)$; è una relazione d'ordine solo _parzialmente_ definita
- $B$ insieme di bindings (vincoli della forma
  - $v_i = C$: a $v_i$ è assegnata la costante $C$
  - $v_i \neq C$: $v_i$ può assumere qualunque valore, tranne $C$
  - $v_i = v_j$: $v_i$ e $v_j$ assumono lo stesso valore
  - $v_i \neq v_j$: $v_i$ e $v_j$ devono essere distinte
- $L$ insieme di causal links della forma $a_i\xrightarrow{c}a_j$ ad indicare che c è un effetto dell'azione $a_i$ che è necessario (precondizione) per l'azione $a_j$.  

$c$ è un assegnamento di valore ad una variabile.

#### **Flaws** (Open Goals, Threats)  

- **Open Goals**  
  Una precondizione $p$ per un'azione $b$ è un **open goal** se non c'è un link causale a supporto di p, cioè manca $?\xrightarrow{p}b$ in $L$.  
  Bisognerà risolvere il difetto aggiungendo il link mancante:
  1) Trova un'azione $a$ (possibilmente già in $\pi$, o in alternativa da aggiungere ad essa) tale che:
      - $p$ può appartenere a $effect^+(a)$ (previa unificazione)
      - $a$ può precedere $b$ in $\pi$
  2) istanzia l'azione $a$ in modo che asserisca $p$ (istanziazione least commitment, solo lo stretto necessario)
  3) aggiungi un vincolo di precedenza $a \prec b$ in $O$
  4) aggiungi un causal link a $a \xrightarrow{p} b$ in $L$

- **Threats**  
  Dato un link causale $l:a\xrightarrow{p}b$, un'azione $c$ minaccia il link $l$ se:
  - $c$ può modificare il valore di verità di $p$ e può posizionarsi tra $a$ e $b$

  oppure

  - $c$ può produrre $p$: il link $l$ impone che sia $a$ a produrre $p$ per $b$ e non un'altra azione $c$.  
  Se anche $c$ può produrre $p$ per $b$, si tratta di un'opportunità da investigare in un altro ramo dello spazio di ricerca.  
  $c$ è anche detta **clobber**.

  Situazione:

  - $l:a\xrightarrow{p}b$, link causale in $L$
  - $c$ azione in $A$ clobber per $l$

  Soluzioni per risolvere le minacce:

  - **Promotion**: imporre il vincolo $c \prec a$
  - **Demotion**: imporre il vincolo $b \prec c$
  - **Separation**: imporre un vincolo di _non-codesignation_ in modo tale che l'effetto di $c$ non unifichi con $p$.  
  Esempio:  
  $p: pos(Block_1) = Table$ ; l'effetto di $c$ potrebbe essere $pos(x) = Hand$, allora il vincolo $x \neq Block_1$ deve essere aggiunto a $B$.

### **Correttezza e Completezza di PSP** (Plan-Space Planning)

Ritorna infatti un piano parzialmente ordinato $\pi$ tale che qualsiasi ordinamento totale delle sue azioni raggiunge il goal.  
Dove il dominio lo consenta, le azioni non strettamente sequenziali sono eseguibili in parallelo.

#### **Algoritmo PSP**

$$
\text{PSP}(\Sigma, \pi) \\\ \\
Flaws(\pi) \leftarrow OpenGoals(\pi) \cup Threats(\pi) \\
\textbf{if}\ \ (Flaws(\pi) = \empty)\ \textbf{then return}\ \pi \\
\text{arbitrarily select} f \in Flaws(\pi) \\
R \leftarrow \{\text{all feasible resolvers for } f\} \\
\textbf{if}\ R = \empty \ \text{\textbf{then return} failure} \\
\text{nondeterministically select } \rho \in R \\
\pi' \leftarrow \rho(\pi) \\
\textbf{return } \text{PSP}(\Sigma, \pi')
$$

Con gli argomenti inizializzati a

- $\Sigma$ è l'STS che modella il dominio
- $\pi' = ({a_0,a_\infin}, {a_0 \prec a_\infin}, \empty, \empty)$

### **Graph Planning**

#### **Grafo di Pianificazione. Cos'è? Com'è definito? A cosa serve?**

E' una struttura dati speciale, utile per

- Definire **euristiche domain-independent** (non dà garanzie sulla raggiungibilità di uno stato, ma fornisce una buona stima della distanza)
- Generare un piano (*graphplan*)

Il grafo astrae lo spazio di ricerca e può essere costruito con un algoritmo polinomiale.

E' un **grafo orientato, aciclico, organizzato a livelli**:

- Il livello iniziale $S_0$ contiene i letterali che valgono nello stato iniziale (**un nodo per ogni fluente**)
- Livello di azioni $A_0$ contiene le azioni ground che possono essere applicate ad $S_0$ (**un nodo per azione**)

In genere $S_i,A_i$ si alternano fino a che non si raggiunge una condizione di terminazione e sono tali che:

- $S_i$ contiene tutti i letterali che **potrebbero** valere al tempo $i$
- $A_i$ contiene tutte le azioni che **potrebbero** avere le precondizioni soddisfatte al tempo $i$

Il grafo di pianificazione (GP)

- traccia un solo sottoinsieme delle possibili interazioni negative, quindi un letterale che appare in uno stato $S_i$ per la prima volta potrebbe di fatto essere producibile solo in uno stato successivo ad $S_i$. $i$ è comunque una buona stima (ammissibile) per il letterale
- è costruito a partire da azioni ground, ma la proposizionalizzazione può generare un'esplosione di alternative. GP è comunque costruito in tempo polinomiale.
- ogni $S_i$ è un belief state: codifica più stati possibili e alternativi

#### **Grafo dei piani** (Livelli, Costruzione del grafo, Livella, Mutex, Complessità e Raggiungibilità del goal)

- **Letterali**
  Un letterale al tempo $i$ può essere inteso sia come precondizione di un'azione in $A_i$, sia come atomo persistente (non usato ad esempio); in questo secondo caso la persistenza è realizzata dall'operazione **no-op** (sempre eseguibile!)
- **Mutua esclusione**  
  In ogni livello $S_i$ possono essere presenti link di mutua esclusione tra letterali ($P$ e $\neg P$, o $Have(Cake)$ e $Eaten(Cake)$).  
  In ogni livello $A_i$ possono essere presenti link di mutua esclusione tra azioni.

  - **Tra due azioni** in un dato livello $A_i$  
    - Effetti inconsistenti: un'azione nega gli effetti dell'altra  
      Ad esempio in $A_0$ l'azione $Eat(Cake)$ è inconsistente con l'azione $Have(Cake)$ in quanto gli effetti dell'una sono negati dall'altra.
    - Interferenza: uno degli effetti di un'azione è la negazione della precondizione dell'altra.  
      $Eat(Cake)$ interferisce con $Have(Cake)$ perchè l'effetto di Eat nega le precondizioni di Have
    - Competizione delle Precondizioni: una delle precondizioni di un'azione è mutualmente esclusiva con le precondizioni dell'altra: $Bake(Cake)$ e $Eat(Cake)$ sono in mutex perchè competono entrambe per la precondizione $Have(Cake)$
  - **Tra due letterali** in un dato livello $S_i$  
    - Complementarity: se uno è la negazione dell'altro
    - Inconsistent Support: se ogni possibile coppia di azioni al livello $A_{i-1}$ che producono i due letterali sono mutuamente esclusivi
- **Livella**  
  Il Grafo dei Piani cresce monotonicamente, quindi prima o poi si livella: due stati consecutivi $S_i$ e $S_{i+1}$ sono identici
  - Ogni livello $A_i$ contiene tutte le azioni che potrebbero essere applicate al tempo $i$
  - Ogni livello $S_i$ contiene tutti i letterali che potrebbero valere al tempo $i$
  - I vincoli di mutua esclusione indicano quali letterali o quali azioni non possono esistere simultaneamente.
  
#### **Tempo di costruzione del grafo/Complessità. Perchè è polinomiale?**

  E' polinomiale nella dimensione del problema:  
  $l$: numero letterali  
  $a$: numero azioni  
  Ogni livello $S_i$ ha non più di $l$ nodi e $l^2$ link di mutex tra letterali  
  Ogni livello $A_i$ non ha più di $l+a$ nodi (inclusi i no-op) e $(a+l)^2$ link di mutex  
  Un grafo con $n$ livelli ha dimensione $\mathcal{O}(n(a+l)^2)$ e la stessa complessità per costruirlo

#### **Euristiche per stimare il costo**

  Se un letterale del goal non compare in nessun livello, allora il goal non è raggiungibile.  
  Ma, se anche tutti i letterali del goal compaiono in un qualche livello del GdP, non significa che esista una soluzione, ma che _forse_ il problema è risolvibile.

- **Costo di un singolo letterale**  
  - Profondità del livello in cui il letterale compare per la prima volta (ammissibile, ma imprecisa)
  - Lunghezza del piano serializzato estratto dal GdP
- **Costo di una congiunzione di letterali**  
  - **max-level**: prendere il massimo livello tra quelli in cui un letterale del goal compare per la prima volta (cioè il primo livello in cui compaiono _tutti i letterali del goal_, anche con vincoli mutex)
  - **somma dei livelli**: non è ammissibile perchè assume l'indipendenza dei letterali del goal
  - **livello di insieme**: profondità del livello in cui tutti i letterali del goal compaiono senza che alcuna coppia di essi sia in mutua esclusione (ammissibile, funziona bene quando i goal non sono indipendenti tra di loro; ignora però le dipendenze tra tre o più letterali)

#### **graphplan** (EXTRACT-SOLUTION, GP-SEARCH, EXPAND-GRAPH, tuple no-good, terminazione e dimostrazione)

Con Graphplan si torna ad una rappresentazione classica del problema di planning.
$$\text{Se esiste un piano valido, allora questo è un sottografo del piano di pianificazione}$$
Come si estrae una soluzione da un Grafo di Pianificazione efficientemente?

1) Espandi il grafo un livello alla volta finchè tutti gli atomi del goal non compaiono nell'$i$-esimo stato **senza che vi siano vincoli di mutua esclusione tra loro**
2) Invoca `EXTRACT-SOLUTION` per cercare un piano all'interno del grafo
    - Se `EXTRACT-SOLUTION` trova una soluzione termina con successo
    - Altrimenti, se non c'è ragione per proseguire perchè la soluzione non esiste certamente, allora termina con fallimento
    - Altrimenti `goto 3`
3) Espandi il grafo ancora di un livello e torna al passo 2

**Extract-Solution**  
`EXTRACT-SOLUTION`: è una ricerca backward search in un sottografo AND/OR del Grafo di Pianificazione:

- Rami OR: archi che dalle azioni ad un livello $A_{i-1}$ producono un letterale $p$ in un goal $g$ al livello $S_i$
- Rami AND: sono gli archi che dagli atomi ad un livello $S_i$ rappresentano le precondizioni per un'azione al livello $A_i$

**GP-Search**  
`EXTRACT-SOLUTION` sfrutta una funzione di supporto `GP-Search`, e si chiamano a vicenda ricorsivamente:

1) `EXTRACT-SOLUTION` invoca `GP-Search` su un livello di aizoni e su un sottogoal (regredito dal goal del problema)
2) `GP-Search` pianifica per il solo livello su cui è invocato (cerca azioni rilevanti per il sottogoal che non siano tra loro in mutua esclusione) e, se ha successo, invoca `EXTRACT-SOLUTION` sul livello di stato precedente.

Ci sono casi in cui questa ricerca potrebbe essere intrattabile, per cui si ricorre ad euristiche.  
Una possibile soluzione è un algoritmo greedy:

- Dato un insieme di letterali che compaiono nel goal
- Scegliere il letterale con il costo di livello più alto
- Soddisfare il letterale scegliendo l'azione con le precondizioni più "facili" (ad esempio la somma/massimo dei costi di livello delle precondizioni è minima)
  
**No-good**  
Ogni volta che `EXTRACT-SOLUTION` applicata a certi letterali $L$ e ad un livello $lev$ fallisce, la coppia $(L,lev)$viene aggiunta nell'insieme **no-good**, in modo che qualunque ricerca successiva applicata alla stessa coppia fallisca immediatamente.

**Terminazione**  
Fino a quando bisogna espandere il grafo dopo che si è livellato?

- Se `EXTRACT-SOLUTION` non trova una soluzione, deve esistere un sottoinsieme di atomi ground del goal che non sono raggiungibili (e sono quindi stati marcati come no-good)
- Allora, se è possibile che vi siano **meno** no-good al livello successivo, continuiamo ad espandere
- Altrimenti, quando sia il grafo, sia i no-good si sono livellati senza che una soluzinoe sia stata trovata, si può concludere con fallimento.  
  
**Dimostrazione**  
Bisogna dimostrare che:

- I no-goods devono livellarsi sempre
- Quando i no-good e il grafo sono livellati, allora si può concludere che la soluzione non esiste

Ci basiamo su proprietà monotone dei grafi di pianificazione:

- **i letterali crescono monotonicamente** (se compare in un livello un letterale, con i no-op sarà sempre presente)
- **le azioni crescono monotonicamente** (conseguenza della precedente proprietà)
- **le relazioni di mutex decrescono monotonicamente**: se due azioni (o letterali) sono in mutex in un livello, lo sono anche nei precedenti MA potrebbero non esserlo in un livello successivo
- **i no-goods decrescono monotonicamente**: se un sottoinsieme di atomi del goal non è raggiungibile in un dato livello di stato $S_i$, allora non lo è nemmeno in nessuno dei precedenti MA potrà diventarlo in un qualche livello futuro (conseguenza delle P precedenti)

1) Dato che le azioni e i letterali crescono monotonicamente, e dato che sono entrambi insiemi finiti, deve esistere un livello in cui l'insieme dei letterali è uguale a quello dei letterali del livello precedente
2) Poichè i mutex e no-good decrescono monotonicamente e poiché non possono esserci meno di zero mutex e no-good, deve esistere un livello con lo stesso numero di mutex e no-good del precedente
3) Una volta che il grafo ha raggiunto la situazione in cui sia i livelli che i no-good si sono livellati, se uno dei letterali del goal è mancante o in mutex con un altro letterale del goal, allora si può concludere che non esiste una soluzione

**Correttezza e completezza**  
Restituisce _fallimento_ sol oquando il problema di pianificazione non è risolubile, altrimenti restituisce una soluzione espressa come una **sequenza di insiemi di azioni**. L'algoritmo termina sempre, anche quando la soluzione non esiste.

**Partial-Order Planner**  
Azioni allo stesso livello (non legate da vincoli di mutua esclusione per costruzione) possono essere eseguite in parallelo.  
Qualunque linearizzazione delle azioni che rispetti i vincoli di ordinamento imposti dai livelli è una possibile soluzione "classica".

### **Euristiche nel Planning**

#### **Info**

Le euristiche cercano di focalizzare la ricerca sul goal guidando le scelte non deterministiche.  
Una funzione euristica stima la "bontà" di una scelta:

- **Progression**: la scelta è il "next state" $s$ stima come distanza da $s$ al goal $G$
- **Regression**: la scelta è il "next sub-goal" $sg$: stima come fistanza da $sg$ allo stato iniziale $l$

L'approccio è di definire un problema analogo all'originale, ma _rilassato_, cioè semplificato. Lo si risolve (ottimalmente se possibile) e si usa il costo della soluzione per il problema rilassato come euristica nel problema originale.  
Deve essere una funzione EFFICIENTE visto che verrà chiamata per ogni stato dello spazio di ricerca. Più la funzione è vicina a quella originale, più sarà affidabile.

#### **Tecniche di rilassamento**

- **Delete relaxation**: cancellazione degli effetti negativi delle azioni
- **Astrazione**: considerare un problema più piccolo, ignorare alcune differenze tra stati in modo che collassino in un unico (riducendo lo spazio di ricerca)
- **Landmark**: un insieme di azioni; almeno una di esse deve essere presente in ogni soluzione. Utile per safe pruning.

Si può:

- Risolvere in modo OTTIMO il problema rilassato
- Risolverlo in modo SUBOTTIMO
- STIMARE il costo della soluzione del rilassamento, senza risolverlo.

#### **Euristiche non ammissibili**

- **Forward Search**  
  $\gamma*(s,a)=s\cup effects^+(a)$  
  La distanza tra uno stato $s$ e una proposizione $p$ è data dalle seguenti relazioni:
  - $\Delta_0(s,p)=0$ se $p \in s$
  - $\Delta_0(s,p)=min_a\{1+\Delta_0(s, precond(a))|p \in effects^+(a)\}$
  - $\Delta_0(s,p)=\infin$ se $\forall a\in A, p \not \in effects^+(a)$

  La distanza tra uno stato $s$ e una congiunzione di proposizioni $g=p_1 \wedge...\wedge p_n$:
  - $\Delta_0(s,g)=0$ se $g \subseteq s$
  - $\Delta_0(s,g)=\sum_{p_i \in g} \Delta_0(s,p_i)$

  La funzione euristica può essere definita come $$h_0(s)=\Delta_0(s,g)$$
  La funzione $h_0$ non è ammissibile perchè è costosa, visto che richiede di risolvere un problema (seppur rilassato) per ogni stato visitato.

- **Backward Search**  
  Regressione per mezzo della $\gamma^{-1}$.  
  Dato il sottogoal $g$ attualmente in esame, esistono diverse azioni _rilevanti_  
  Scegliere l'azione che porta al sottogoal più vicino allo stato iniziale
  $$a_{best} \leftarrow argmin\{\Delta_0(s_0, \gamma^{-1}(g,a))\ |\ a\ \text{è rilevante (resolver) per } g\}$$
  L'euristica è sempre basata sulla distanza $\Delta_0$, quindi non è ammissibile.  
  Il vero vantaggio è che è possibile precompilare le distanze tra s_0 e ogni possibile proposizione, il calcolo a runtime è quindi semplificato

- Se non volessimo un'euristica ammissibile, cosa faremmo?

#### **Euristiche ammissibili $h_1$, $h_2$ e $h_G$**

  Consentono di ottenere **soluzioni ottime**  
  
- **Safe Pruning**: so che una soluzione ha al più costo $k$, posso scartare un nodo qualsiasi $u$ tale che $h(u) > k$ senza correre il rischio di perdere le soluzioni.
- $h_1(s,g)=\Delta_1(s,g)$  
  $\Delta_1(s,g)$ considera il costo di applicare un'azione $a$ per ottenere $p$, tale costo è dominato dal costo peggiore per soddisfare le precondizioni di $a$, il costo per $p$ con $a$ è quindi il costo di $a+1$.  
  Tra tutte le azioni che producono $p$ scelgo la più economica.  
  E' poco informativa: considera una sola proposizione $p$ del goal, e si può raffinare:
- $h_2(s,g)=\Delta_2(s,g)$  
  Considera la massima distanza di una coppia di proposizioni $p,q$ del goal.  
  $h_2$ è ammissibile e più informativa di $h_1$, ma è più costosa.
- $h_G$: si usa Graphplan come euristica  
  Bisogna costruire un grafo di pianificazione senza considerare gli effetti negativi e la mutua esclusione  
  Il costo di una proposizione $p$ è la profondità del livello in cui $p$ compare per la prima volta  
  Il costo di un goal è la profondità del livello in cui tutti gli atomi del goal compaiono tutti insieme per la prima volta  
  E' ancora ammissibile e:
  - è più facile da calcolare e precompilare
  - $h_G$ considera due livelli: due azioni allo stesso livello pesano 1

#### **Least Commitment Planning come ricerca AND/OR, Euristica FAF per la selezione dei Flaws**

Abbiamo due scelte non deterministiche da effettuare: il prossimo flaw e resolver.  
Un flaw rappresenta rami in AND (da risolvere tutti dal planner)  
Un resolver rappresenta rami OR (sufficiente un solo resolver per flaw)  
L'ordine in cui si selezionano i flaw è irrilevante per la "forma" della soluzione finale, ma è indispensabile nel determinare il numero dei nodi (la scelta di un flaw o un altro può produrre un albero con molti più nodi rendendo meno efficiente il calcolo della soluzione).

**FAF (Fewer Alternatives First)**  
Si preferisce il flaw col minor numero di resolvers.  
In questo modo si esplorano prima nicchie dello spazio di ricerca e si limitano i costi di un eventuale backtracking.  
E' più facile scoprire che un flaw non è risolubile.

## **Sistemi Esperti**

Un sistema esperto basato sulla conoscenza è un sistema in grado di risolvere problemi in un dominio limitato ma con prestazioni simili a quelle di un esperto umano del dominio stesso.  
Generalmente esamina un largo numero di possibilità e costruisce dinamicamente una soluzione.

### **Proprietà**

- Specificità (conoscenza di dominio)
- Rappresentazione esplicita della conoscenza
- Meccanismi di ragionamento
- Capacità di spiegazione
- Capacità di operare in domini poco strutturati

### **Costruzione** (Inference Engine, KB, WM, Facilities, UI)

- Knowledge-Base: mantiene la conoscenza dell'esperto come regole del tipo condizione-azione
- Working Memory: mantiene i fatti iniziali e quelli generati dalle inferenze
- Inference Engine
  - Pattern matching: confronta la parte if delle regole rispetto ai fatti della Working Memory.  
    Le regole che hanno la parte if soddisfatta sono dette attivabili e sono poste nell'agenda.
  - **Agenda**: elenco ordinato di regole attivabili, ordinate in base alla loro priorità o in base ad altre strategie di preferenza/conflict resolution
  - Execution: la regola in cima all'agenda è selezionata ed eseguita (firing)
- Explanation Facility: fornisce una giustificazione delle soluzioni (sequenza di regole attivate, _reasoning chain_)
- Knowledge Acquisition Facility: aiuta a integrare nuove regole e mantenerle
- User Interface: consente all'utente di interagire con il sistema esperto

### **Conoscenza di dominio / conoscenza di controllo**

Ogni sistema basato sulla conoscenza deve riuscire ad esprimere due tipi di conoscenza in modo separato e modulare:

- Conoscenza sul dominio dell'applicazione ("COSA")
- Conoscenza su COME utilizzare la conoscenza sul dominio per risolvere problemi (CONTROLLO)
  
### **Quando (non) utilizzare un Ssistema Esperto**

Utile per Interpretazione, diagnosi, monitoring, planning, scheduling, previsione, progettazione e configurazione.

Non bisogna usarli quando:

- Esistono algoritmi "tradizionali" efficienti per risolvere il problema considerato
- L'aspetto principale è la computazione e non la conoscenza
- La conoscenza non può essere modellata in modo efficiente
- L'utente finale è riluttante ad usare un sistema esperto data la criticità del task

Un sistema a regole ha la stessa potenza espressiva di una macchina di Turing, ma non sempre è la scelta migliore.

### **CLIPS**

- Fatti (ordinati, non-ordinati, manipolazione)  
  - Fatti (ordinati, non-ordinati, manipolazione)
- Fatti (ordinati, non-ordinati, manipolazione)  
  - "chunk of information" , elemento indivisibile di informazione
  - Sono definiti da un nome di relazione
  - Hanno zero o più "slot"
  
  **Fatti ordinati**
  (no slot)

  ```Lisp
  (person-name Franco L Verdi)
  (person-name Verdi Franco L)
  ```

  **Fatti non ordinati**
    (template)
  
    ```Lisp
    (deftemplate person "commento opzionale"
      (slot name)
      (slot age)
      (slot eye-color)
      (slot hair-color))
    ```

    (istanza)
  
    ```Lisp
    (person (name "Franco L. Verdi")
      (age 46)
      (eye-color brown)
      (hair-color brown))
    ```

  **Manipolazione di fatti**
  Varie interazioni possibili con la WM
  - Aggiungere: `(assert <fact>+)`
  - Rimuovere: `(retract <fact-index>+)`
  - Modificare: `(modify <fact-index> (<slot-name> <slot-value>)+ )`
  - Duplicare: `(duplicate <fact-index> (<slot-name> <slot-value>)+ )`

#### **Regole (LHS, RHS, Binding)**

  ```Lisp  
  (defrule <rule name> ["comment"]
    <patterns>* ; left-hand side (LHS)
                ; or antecedent of the rule
    =>
    <actions>*) ; right-hand side (RHS)
                ; or consequent of the rule
  ```

  Le variabili in un pattern (LHS) sono legate a valori di fatti nella WM. Il binding è locale ad ogni regola.

  ```Lisp
  (defrule find-blue-eyes
    (person (name ?name)(eye-color blue))
    =>
    (printout t ?name " has blue eyes." crlf))
  ```

#### **Rifrazione**

  E' un meccanismo alla base del pattern-matching che impedisce di attivare due volte una regola sugli stessi fatti.  
  (Ad esempio per la regola degli occhi blu, verrà attivata una sola volta per persona che la soddisfa)

#### **Agenda** (Definizione, Ordinamento Regole, Conflict resolution)

E' la lista di tutte le regole che hanno la parte sinistra soddisfatta (e non sono ancora state eseguite.)  
Ogni modulo ha la sua agenda; si può pensare che sia simile ad uno stack di esecuzione.  
La regola in cima è la prima ad essere eseguita.

**Ordinamento**  
La posizione di una _regola attiva_ nell'agenda è determinata così:

1) in base alla _salience_ (priorità)  
    Tutte le regole con salience più bassa sono sotto la regola corrente  
    Tutte le regole con salience più alta sono sopra la regola corrente
2) A parità di salience, si usa la strategis di risoluzione del conflitto (conflict resolution strategy)
3) Se una regola è attivata dagli stessi fatti della WM e 1) e 2) non sono in grado di decidere l'ordine, allora ha priorità l'ordine con cui sono definite nel sorgente.

#### **Se abbiamo in agenda $R_1$ e sotto $R_2$ ed eseguiamo $R_1$, $R_2$ rimane in agenda?**

Non per forza: se la Working Memory viene modificata da $R_1$ e le precondizioni di $R_2$ non sono più verificate, allora non rimarrà in agenda.

#### **Strategie di Conflict Resolution**

  Si usa il comando `(set-strategy <nome-strategia> )` per decidere quale usare

- **Depth**: regole attive più recenti in cima alle altre regole di pari salience (sono preferite regole attivate dai **fatti più recenti**)
- **Breadth**: regole attive più recenti dopo regole di pari salience (sono preferite regole attivate dai **fatti meno recenti**)
- **Simplicity e Complexity**
  - **Specificity**: è determinata dal numero di confronti e bindings effettuati nella parte sinistra della regola
  - Simplicity: regole attive più recenti sopra a regole con uguale o più alta specificità
  - Complexity: regole attive più recenti sopra a regole con uguale o più bassa specificità
- **LEX**: A parità di salience, le regole attivate sono ordinate così:
  - Ogni fatto è etichettato con un time tag per indicarne la recentezza
  - I pattern di ogni attivazione sono associati al time tag del fatto che li soddisfa
  - Un'attivazione con un pattern più recente è sistemata prima di una attivazione con pattern meno recenti
  
    ```?
    Ordine di definizione       Ordinamento con LEX
    rule6: f-1, f-4             rule6: f-4, f-1,
    rule5: f-1, f-2, f-3        rule5: f-3, f-2, f-1
    rule1: f-1, f-2, f-3        rule1: f-3, f-2, f-1
    rule2: f-3, f-1             rule2: f-3, f-1
    rule4: f-1, f-2             rule4: f-2, f-1
    rule3: f-2, f-1             rule3: f-2. f-1
    ```
  
- **MEA**:
  - Time tag come con LEX
  - Il primo time tag del pattern associato col primo pattern è usato per determinare la posizione della regola e si ordina in base a quello
  - In caso di conflitti, si passa al secondo time tag (non ordinato) e così via.

    ```?
    Ordine di definizione       Ordinamento con MEA
    rule6: f-1, f-4             rule2: f-3, f-1
    rule5: f-1, f-2, f-3        rule3: f-2, f-1
    rule1: f-1, f-2, f-3        rule6: f-1, f-4
    rule2: f-3, f-1             rule5: f-1, f-2, f-3
    rule4: f-1, f-2             rule1: f-1, f-2, f-3
    rule3: f-2, f-1             rule4: f-1, f-2
    ```
  
- **Random**: si aggiunge alle regole un valore casuale con cui sono poi ordinate in agenda

#### **Regole e connettivi logici**

Di default i pattern nell'antecedente delle regole sono considerati in congiunzione ($and$ impliciti), ma si può anche creare pattern più complessi (field constraints)

- **OR**: `(or (pattern1) (pattern2))`
- **NOT**: `(not (pattern))`
- **EXISTS**: `(exists (pattern))`
- **FORALL**: `(forall (pattern))`

#### **`Gensym` e `Gensym\*`**

Restituisce un nuovo simbolo con forma $genX$ dove $X$ è un intero incrementato automaticamente ad ogni invocazione della funzione. Il primo simbolo generato è $gen1$. I simboli generati non sono necessariamente univoci

`(gensym*)`  
Simile a gensym ma con la garanzia che il simbolo generato è univoco.

#### **Moduli. A cosa servono?**

Programmi che risolvono domini reali possono avere migliaia di regole.  
Non tutte codificano conoscenza di dominio (cioè modellano il problema che si vuole risolvere), ma alcune codificano **conoscenza di controllo** che regola il comportamento del programma.  
Può diventare problematico l'interleaving della conoscenza di controllo e di dominio durante la manutenzione della KB.

L'idea consiste nel separare la conoscenza di controllo da quella di dominio. Si assegna un control pattern ad ogni regola che indica in quale fase è applicabile. Si potrebbero usare metodi come la salience o sistemi introdotti artificialmente, ma potremmo voler passare da una fase all'altra in modo più flessibile: si usano i moduli.

`(defmodule <module-name> [<comment>])`

Le regole sono definite in un solo modulo, che quindi partizionano la KB e anche la WM (ognuno ha la propria), quindi di conseguenza anche l'agenda.

I moduli possono comunque scambiare informazioni tra di loro, quando cambia il focus (modulo attualmente attivo, gestito tramite stack).  
Un modulo ha il focus finchè

- ci sono regole da eseguire, dopodichè avviene una `pop` implicita
- una regola esegue il comando `(pop focus)`
- una regola aggiunge un ulteriore modulo in cima allo stack `(focus nuovo-modulo)`
