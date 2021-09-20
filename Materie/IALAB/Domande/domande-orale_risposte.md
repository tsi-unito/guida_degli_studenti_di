# IALAB - Domande Possibili

- [IALAB - Domande Possibili](#ialab---domande-possibili)
  - [Pozzato](#pozzato)
    - [Unificazione](#unificazione)
    - [Prolog](#prolog)
      - [**Risoluzione SLD** (Albero SLD, Terminazione, Non-Determinismo)](#risoluzione-sld-albero-sld-terminazione-non-determinismo)
    - [Ricerca nello spazio degli stati](#ricerca-nello-spazio-degli-stati)
    - [Strategie di ricerca](#strategie-di-ricerca)
      - [Strategie non informate](#strategie-non-informate)
      - [Strategie informate](#strategie-informate)
    - [Answer Set Programming](#answer-set-programming)
  - [Micalizio - Planning](#micalizio---planning)
    - [Agente](#agente)
    - [Planning Classico](#planning-classico)
      - [**Problema di pianificazione $\mathcal{P}$** (+ Complessità PlanSAT, Bounded PlanSAT)](#problema-di-pianificazione-mathcalp--complessità-plansat-bounded-plansat)
    - [**STRIPS**](#strips)
      - [Progression e Regression](#progression-e-regression)
    - [CSP](#csp)
      - [**State-Variable Representation** (Stato, Applicabilità, $\gamma(s,a)$)](#state-variable-representation-stato-applicabilità-gammasa)
      - [**Least-Commitment Planning**](#least-commitment-planning)
  - [Micalizio - Sistemi Esperti](#micalizio---sistemi-esperti)
  - [Torta](#torta)

## Pozzato

### Unificazione

- **Definizione**
  Due termini $t1$ e $t2$ sono unificabili se esiste una sostituzione $\sigma$ (**unificatore**) che li rende identici: $t1\sigma = t2\sigma$.
  $$
  t_1=f(x_1,h(x_1),x_2) \\
  t_2=f(g(x_3),x4,x3) \\
  \sigma=\{x_1/g(x_3),x_2/x_3,x_4/h(g(x_3))\}
  $$

- **Most General Unifier**  
  Una sostituzione $\theta$ è **più generale** di una soluzione $\sigma$ se esiste una sostituzione $\lambda$ tale che $\sigma=\theta\lambda$.  
  Si può dimostrare che, se due termini sono unificabili, esiste sempre un **unificatore più generale**unico a meno di ridenominazione delle variabili.

- **Algoritmo MGU**
  L'algoritmo MGU Martelli ha il compito di trovare un *MGU* $\sigma$ più generale:
  - Scegliere un'equazione della forma $t=x$ dove $t$ non è una variabile ma $x$ lo è, diventa $x=t$
  - Cancellare tutte le equazioni della forma $x=x$
  - Scegliere un'equazione della forma $t'=t''$, entrambi non variabili. Se sono diversi i simboli di funzione, fallisco, altrimenti facciamo la **Term Reduction**
  - Scegliere un'equazione della forma $x=t$ dove $x$ è una variabile che occorre da qualche altra parte e $t\neq x$. Se *x* occorre in *t*, fallimento. Altrimenti, faccio **Variable Elimination**

### Prolog

- **Definizione**  
  Prolog è un linguaggio di programmazione logica, cioè dove invece di programmare con uno dei normali paradigmi si enunciano clausole per un programma, che poi il motore di inferenza utilizzerà per cercare di dimostrare un goal.  
  La ricerca della soluzione avviene mediante backward chaining in profondità: si parte dal goal che si vuole derivare (scritto come una congiunzione di formule atomiche $G_1,G_2,...,G_n$) e si cerca di risolverlo, dimostrando o meno che il goal **segue logicamente** dal programma.

- **Applicabilità di una regola**  
  Una regola $A \coloneq B_1,B_2,...,B_m$ è applicabile a $G_i$ se:
  - le variabili vengono rinominate e
  - $A$ e $G_i$ unificano
  
- **Interprete Prolog**  
  L'interprete prolog cerca di dimostrare il Goal. La computazione ha successo se esiste una computazione che termina con successo.  
  L'interprete Prolog si comporta deterministicamente:
  - Le clausole vengono considerate nell'ordine in cui sono scritte nel programma
  - Viene fatto backtracking all'ultimo punto di scelta ogni volta che la computazione fallisce.

  In caso di successo, l'interprete restituisce una sostituzione per le variabili che compaiono nel goal.

#### **Risoluzione SLD** (Albero SLD, Terminazione, Non-Determinismo)  

- **Definizione**  
  Per arrivare ad un linguaggio di programmazione ci serve una strategia efficiente per risolvere il problema.  
  Risoluzione SLD: **Linear resolution with Selection function for Definite clauses**  
  - Si lavora con **clausole di Horn**: una disgiunzione di letterali in cui al massimo uno dei letterali è positivo.
  - **Strategia Linear Input**: ad ogni passo di risoluzione, una **variante** di una clausola è sempre scelta nella K (_teoria_/_programma_) di partenza, mentre l'altra è sempre il risolvente del passo precedente (goal, la negazione di F al primo passo, visto che si cerca di risolvere per negazione).  
  - La derivazione SLD per un goal $G_0$ da un insieme di clausole $K$ è:
    - Una sequenza di clausole goal $G_0,G_1,...,G_n$
    - Una sequenza di varianti di clausole di $K$ $C_1,C_2,...,C_n$
    - Una sequenza di MGU $\sigma_1,\sigma_2,...,\sigma_n$, tali che $G_{i+1}$ è derivato da $G_i$ e da $C_{i+1}$ attraverso la sostituzione $\sigma_{i+1}$.

- **Terminazione**
  - Ci possono essere tre tipi di derivazioni:
    - **Successo** se $G_n = \square$
    - **Fallimento finito** se non è possibile derivare da $G_n$ alcun risolvente e $G_n \neq \square$
    - **Fallimento infinito** se è sempre possibile derivare nuovi risolventi

- **Non Determinismo**  
  Due forme di non determinismo:
  1) Regola di calcolo per selezionare ad ogni passo l'atomo $B_i$ del goal da unificare con una clausola
  2) Scelta di quale clausola utilizzare ad ogni passo di risoluzione

  - **Regola di Calcolo**  
  Funzione che ha come dominio l'insieme dei goal e per ogni goal seleziona un suo atomo.  
  Non influenza la correttezza e completezza del metodo di prova.

- **Albero SLD**  
  Data una regola di calcolo, è possibile rappresentare tutte le derivazioni con un albero SLD:  
  - Nodo: goal
  - Radice: goal iniziale $G_0$
  - Ogni nodo $\leftarrow A_1,...,A_m,...,A_k$, dove $A_m$ è l'atomo selezionato dalla regola di calcolo, ha un figlio per ogni clausola $A \leftarrow B_1,...,B_k$ tale che $A$ e $A_m$ sono unificabili con il MGU $\sigma$.  
  Il nodo figlio è etichettato con il goal $\leftarrow [A_1,...,A_{m-1},B_1,...,B_k,A_{m+1},...,A_k]\sigma$.
  Il ramo dal padre al figlio è etichettato con $\sigma$ e con la clausola selezionata.

- **La risoluzione SLD è corretta e completa per tutta la logica o solo per alcuni tipi di clausole?**  
  _In generale_ **NON** è completa, ma con le clasuole di Horn SI.

- **Negazione per fallimento (Ragionamento Non-Monotono)**

- **3 Possibili esiti con negazione per fallimento**  
  1) Se la query negata è soddisfatta, la negazione fallisce
  2) Se la query negata non è soddisfatta, la negazione ha successo
  3) Se la query richiede delle informazioni che non sono presenti nella KB (in poche parole, non sappiamo/conosciamo qualche informazione), dato che Prolog si basa sulla Assunzione di Mondo Aperto, non possiamo decidere se un'informazione mancante sia mancante volontariamente o meno, per cui ha successo.

- Risoluzione SLD con Prolog  
  Una risoluzione in prolog **si basa sulla dimostrazione del goal mediante risoluzione SLD**.  
  Per rendere la strategia deterministica:
  - Si sceglie sempre il **sottogoal più a sinistra**
  - Le clausole sono considerate **nell'ordine in cui sono scritte** nel programma
  - La strategia di ricerca è **in profondità, con backtracking**.  
    **Non è tuttavia completa**: se una computazione che porterebbe al successo della risoluzione si trova a destra di un ramo infinito, l'interprete non lo riuscirà mai a raggiungere perchè continuerà ad entrare nel ramo infinito.

- **Il CUT**  
  Consente di modificare e controllare l'esecuzione dell'interprete Prolog.  
  Si tratta di un predicato che è **sempre vero**, che se eseguito blocca il backtracking.  
  L'interprete Prolog utilizza due stack (in realtà uno, ma si alternano i due blocchi):  
  1) **Stack di esecuzione**: contiene i predicati attualmente attivati e ancora in esecuzione
  2) **Stack di backtracking**: contiene i punti di scelta (choice points) che sono in valutazione (o devono ancora essere valutati)

  Quando una valutazione fallisce, si effettua backtracking e si prova a valutare un altro ramo non ancora esplorato. Se non ci sono più punti di scelta, si ha il fallimento del predicato attualmente in fase di valutazione.

  In pratica, il CUT **rende definitive le scelte effettuate nel corso della valutazione** da parte dell'interprete: elimina i choice point dallo stack di backtracking, alterando quindi il controllo del programma. Perdiamo in dichiaratività

- **La presenza del cut cosa va a mettere a rischio tra completezza e correttezza?**  
  Andando a tagliare alcuni rami dell'albero SLD, rimuovendo alcuni punti di backtracking, non assicuriamo più la **COMPLETEZZA**.

- Se prendo delle clausole che non sono di Horn, cosa si mette a rischio tra completezza e correttezza?
  
### Ricerca nello spazio degli stati

- **Problema di ricerca**:  
  - Stato iniziale
  - Insieme delle azioni (un'azione fa passare da uno stato ad un altro)
  - Obiettivi
  - Costo di ogni azione

- **Spazio degli stati**  
  E' composto da: $StatoIniziale$ $\cup$ $InsiemeAzioni$ e corrisponde all'insieme di tutti gli stati raggiungibili a partire da quello iniziale.

- **Cammino**  
  Sequenza di stati collegati da una sequenza di azioni.  
  Il suo **costo** è dato dalla somma dei singoli costi di tutte le azioni che lo compongono.

- **Soluzione**  
  La soluzione ad un problema è un **cammino** dallo stato iniziale ad uno stato goal.  
  La soluzione è **ottima** se è quella con il costo minimo tra tutte le soluzioni possibili.

- **In Prolog?**  
  Le azioni le rappresentiamo con delle clausole del linguaggio. Ognuna avrà delle precondizioni (stati in cui può essere eseguita) e degli effetti quando sono eseguite. (Applicabile e Trasforma)

### Strategie di ricerca

#### Strategie non informate

- **Ricerca in profondità**  
  Si espande sempre per primo il nodo più distante dalla radice dell'albero di ricerca.  
  Facilmente realizzabile in Prolog grazie al suo non-determinismo (chiamate ricorsive continue).  
  L'interprete Prolog se non trova una soluzione, fa backtracking, quindi realizza una **ricerca in profondità con backtracking**.

- **Ricerca a profondità Limitata**  
  Uguale alla ricerca in profondità, ma si usa un parametro che vincola la profondità massima oltre la quale i nodi non vengono espansi. La procedura che ne risulta **non è completa**.

- **Iterative Deepening** (non informata)  
  Ripete la ricerca a profondità limitata, incrementando ad ogni passo il limite.  
  Nel caso in cui le azioni del problema abbiano tutte costo unitario, la strategia è **ottima**.

- **Ricerca in ampiezza**  
  Si utilizza una coda di nodi.  
  Ad ogni passo la procedura espande il nodo in testa alla coda (usando una `findall`) generando tutti i suoi successori, che vengono aggiunti in fondo alla coda.  
  E' sicuro che venga individuata la soluzione ottima.  

  Se si lavora con **grafi**, si conserva una lista dei nodi già espansi, per non esplorarli nuovamente.

#### Strategie informate

  Si utilizza una funzione euristica $h(n)$, che corrisponde al costo stimato del cammino più conveniente dal nodo n ad uno stato finale.  
  Ogni azione ha anche associato un costo $g(n)$, che corrisponde al costo del cammino trovato dal nodo iniziale a n.

- **A\***  
  Ricerca in ampiezza su grafi, tiene conto della funzione euristica.  
  Ad ogni passo si estrae per l'espansione dalla coda il nodo con minimo valore di $f(n) = g(n)+h(n)$.  
  I nodi già esplorati/espansi non vengono ulteriormente toccati.

- **IDA\***  
  Coincide con l'_Iterative Deepening_, ma la soglia è stimata basandosi sull'euristica.  
  Alla prima esecuzione è $h(StatoIniziale)$.  
  Per ogni esecuzione successiva, la soglia è il minimo $f(n)$, scelto dall'insieme dei nodi che al termine dell'esecuzione precedente superavano la soglia.

### Answer Set Programming

- **Definizione**  
  Inizialmente era stato ideato per dare una semantica alla negazione per fallimento di Prolog.  
  Non si cercano più le prove, ma i modelli che devono essere **stabili**; non si fa più inferenza con backward chaining. E' particolarmente utile per risolvere problemi combinatori (con vincoli, pianificazione).

  Viene dichiarato un insieme finito di regole del tipo $a \coloneq b_1,b_2,...,b_n, not\ c_1, not\ c_2,...,not\ c_m$ Con $a, b_i, c_j$ letterali della forma $p$ o $\neg p$.

  - $\neg$ coincide con la negazione classica.  
  - $not$ è la negazione per fallimento
  
- **Prolog vs Clingo**
  - In ASP l'ordine dei letterali non ha alcuna importanza
  - Prolog è goal directed (backward chaining!!), ASP no
  - La SLD-riduzione di Prolog può portare a loop, mentre gli ASP solver non lo permettono
  - Prolog ha il CUT, ASP no.

- **Negazione classica vs. per fallimento**
  - **Classica**  
    $attraversa \coloneq \neg treno$ : più forte. _Si attraversa solo se si può derivare che il treno non è in arrivo._
  
  - **Per Fallimento**  
    $attraversa \coloneq not\ treno$ : _si può attraversare in assenza di informazione esplicita sul treno in arrivo._

- **Answer Set**  
  Si tratta del modello minimale. Un programma ASP privo di letterali $not\ p_i$ha un unico modello minimale che è il suo answer set.  
  Un answer set è una soluzione al problema.  
  Esempio 1:
  $$
  p \coloneq q. \\
  q. \\
  \ \\
  Answer\ set=\{p,q\}
  $$
  Esempio 2:
  $$
  p \coloneq q,r. \\
  q. \\
  \ \\
  Answer\ set=\{q\}
  $$

- **Programma Ridotto**  
  Per ottenere il ridotto $P^S$ di un programma $P$ rispetto ad un insieme di atomi $S$:
  1) Rimuovi ogni regola il cui corpo contiene $not\ L, \forall L \in S$;
  2) Rimuovi tutti i $not\ L$ dai corpi delle regole che sono rimaste.
  
  $P^S$ non contiene atomi con negazione per fallimento (per costruzione):
  - Ha un unico answer set
  - se tale answer set **coincide** con $S$, allora $S$ è un answer set per $P$.
  
  E' possibile che esistano programmi che hanno un modello ma non hanno answer set:
  $$
  p \coloneq not\ p, d. \\
  d.
  $$
  $\{p,d\}$ è un modello. I candidati sono $\{d\}$ e $\{p,d\}$.  
  1) Il ridotto $P^{\{d\}}$ risulta essere $$p \coloneq d. \\ d.$$ ma l'Answer Set risultante è $\{p,d\} \neq \{d\}$.
  2) Il ridotto $P^{\{p,d\}}$ risulta essere $d.$ ma l'Answer Set risultante è $\{d\} \neq \{p,d\}$.

- **Modello Minimale di Herbrand**
  Il modello minimale di ASP è minimo rispetto all'inclusione insiemistica verso il modello di Herbrand, che è un insieme di *atomi ground* tali che tutte le clausole del programma sono **entailed**, ovvero, se ho il body vero, allora anche la mia testa è nel modello.

- **Nel ridotto c'è un solo answer set?**  
  No, è possibile che da un ridotto risultino anche più answer set.  
  Gli ASP-solver trovano (se richiesto) tutte le soluzioni nel momento stesso in cui calcola gli answer set del programma.

- **Integrity Constraint**  
  $a$ (la testa della regola) è **opzionale**; senza abbiamo un _integrity constraint_, che invalida tutti i modelli dove tutte le $a_n$ sono verificate:
  $$\coloneq a_1, a_2,...,a_k$$
  (è inconsistente che $a_1, a_2,...,a_k$ siano tutti veri)

- **Aggregati**  
  Si tratta di costrutti che selezionano answer set contenenti almeno $n$ e al massimo $m$ elementi di un insieme. Avremo da $n$ a $m$ atomi $p$, i cui possibili valori sono presi da $q$:
  $$n\ \{p(X)\!:q(X)\}\ m.$$
  Esempio: _ogni variabile deve avere un solo tipo_:
  $$1\ \{ha\_tipo(V,T) \! : tipo(T)\}\ 1 \coloneq variabile(V).$$

- **Risoluzione del problema ASP**
  - Descrizione mediante regole degli answer set potenziali
  - Introduzione di vincoli (regole senza testa) che invalidino gli answer set non accettabili

## Micalizio - Planning

### Agente

- **Definizione**  
  Il modello ad agenti è un'**astrazione per modularizzare il codice** in
  - **Agente**: Processo che persegue un certo obiettivo. Utilizza una _funzione agente_ per decidere la prossima azione date le percezioni finora ricevute
  - **Ambiente**: dati e risorse computazionali usate dagli agenti
- **Agente Intelligente**  
  Essendo un'astrazione consente di analizzare un problema in termini di alto livello (ci si concentra sui **concetti** e non sull'implementazione)  
  Ha due aspetti fondamentali:  
  - **Agency**: agisce in un ambiente che _percepisce_ con dei sensori e che _modifica_ con attuatori.
  - **Autonomia**: con le proprie conoscenze dell'ambiente sa prendere decisioni; può anche saper modificare il proprio modello se l'ambiente cambia
  - **Razionalità**: Si deve comportare correttamente, cioè nel miglior modo possibile date le informazioni sull'ambiente.
  
  L'agente genera azioni $\rightarrow$ l'ambiente evolve grazie ad esse in una sequenza di stati. Se la sequenza è quella desiderata, l'agente ha conseguito il **goal**, comportandosi quindi in modo **razionale**.

- **Razionalità**  
  Impone una misura di prestazione! Bisogna valutare la sequenza degli stati, e l'agente potrebbe pensare di star agendo bene (anche se questo potrebbe non essere il caso).

  La razionalità dipende da:
  - Come è definito il criterio di successo
  - Conoscenza pregressa dell'ambiente da parte dell'agente (il **modello**)
  - Azioni che l'agente può eseguire
  - Percezioni ricevute finora (non tutto è osservabile!)
  
  L'agente è razionale se **MASSIMIZZA** il risultato: se sceglie le azioni che massimizzano il risultato atteso della misura di prestazione.

- **Tipi di agenti**  
  - **Reattivi semplici**: vedono lo stato attuale e decidono un'azione
  - **Reattivi basati sul modello**: ho uno **stato interno** (sintesi delle percezioni finora ricevute) e ho un modello del mondo che mantiene lo stato aggiornato. Se ci sono ambiguità si ragiona in termini della migliore ipotesi corrente.
  - **Basati su Obiettivi**: La scelta della prossima azione non dipende unicamente dalla situazione corrente, ma anche da cosa si vuole raggiungere (il goal). Bisogna ragionare sul futuro: il modello del mondo ci consente di prevedere le conseguenze delle azioni. In questa categoria ricade il planning classico.
  - **Basati sull'Utilità**: se esistono percorsi alternativi, potrei voler scegliere quello migliore rispetto una specifica grandezza di interesse (costo, tempo, ecc.).
  
### Planning Classico

- **Definizione**  
  E' un processo deliberativo che sceglie ed organizza le azioni in base all'effetto che ci si aspetta che queste producano. Abbiamo due tipi di planning, che spesso si completano a vicenda:
  - **Domain-Dependent Planning**: usa rappresentazioni e tecniche adattate alle specifiche condizioni di un dominio di pianificazione
  - **Domain-Independent Planning**: usa rappresentazioni e tecniche generiche, che non dipendono da un dominio specifico. Si basa sugli **aspetti comuni** presenti in tutti i domini di pianificazione. Ci consente di considerare il planning come un problema generale: molti problemi sono riconducibili ad esso.

- **STS (Definizione, STS-Grafo)**  
  - **State Transition System** $\Sigma = (S,A,E,\gamma)$, dove:
    - $S=\{s_1,s_2,...\}$ insieme finito, ricorsivamente enumerabile di stati
    - $A=\{a_1,a_2,...\}$ insieme finito, ricorsivamente enumerabile di azioni
    - $E=\{e_1,e_2,...\}$ insieme finito, ricorsivamente enumerabile di eventi
    - $\gamma:S\times(A \cup E) \rightarrow 2^S$ è una relazione di transizione di stato:  
      Se $a \in A$ e $\gamma(s,a)\neq \emptyset$, allora $a$ è **applicabile** in $S$.  
      Applicare $a$ in $s$ causerà una transizione di stato del sistema da $s$ a $s'$, dove $s'\in\gamma(s,a)$
  
  - **Componenti dell'STS**
    - L'**STS** descrive tutte le possibili evoluzioni di un sistema.  
    - Un **Piano** traccia tutte le azioni necessarie per raggiungere un determinato obiettivo $G$ dato uno stato iniziale $I$. Si può pensare ad esso come un cammino da $I$ a $G$ nello spazio degli stati tracciato dall'STS.
    - I **Goal** possono essere di diversa tipologia:
      - Uno **stato goal** $s_g$ o un sottoinsieme di possibili stati goal in $S_g$
      - Soddisfacimento di condizioni in tutta la sequenza di stati prodotta dalle azioni (carburante sempre > 0)
      - Ottimizzazione di funzioni di utilità (minimizzare costi, tempi, massimizzare profitti)
      - Vincoli sulle azioni che possono essere eseguite (azione a che deve essere seguita subito o entro un tempo limite da un'azione b).
  
  - **STS-Grafo**  
    Un STS $\Sigma =(S,A,E,\gamma)$ può essere rappresentato come un grafo direzionato $G=(N_g,E_g)$:
    - $N_g=S$ è l'insieme dei nodi del grafo coincidente con l'insieme degli stati di $\Sigma$
    - $E_g$ è l'insieme degli archi del grafo tale che esiste un arco $s\xrightarrow{u}s'$ (anche rappresentabile come $\langle s,u,s' \rangle$) da $s$ a $s'$ etichettato con $u \in A \cup E$ **se e solo se**:
      - $s,s'\in S$ (stato iniziale e finale fanno parte degli stati dell'STS) e
      - $s'=\gamma(s,u)$ (la transizione da $s$ a $s'$ mediante la funzione di transizione gamma $\gamma$ è ottenibile con l'azione/evento $u$).

- **Planning e Plan Execution**  
  - **Planner**: Data la descrizione di un STS $\Sigma$, lo stato iniziale e il goal, genera un piano che raggiunge il goal dallo stato iniziale.
  - **Controller**: Dato un piano e lo stato corrente (funzione di osservabilità $\eta:S\rightarrow O$ (eta)), seleziona ed esegue un'azione del piano
  - **STS $\Sigma$**: evolve in funzione delle azioni che vengono eseguite e degli eventi che possono accadere.
  
  (Si assume che gli eventi non interferiscano con le azioni del Controller)

  Abbiamo un problema: il mondo reale può essere diverso da come è descritto nel modello $\Sigma$.  
  Un approccio più realistico è quello del **continual planning**: consente un loop chiuso di feedback tra planner e controller. In questo caso gli eventi che occorrono possono interferire con le azioni pianificate (guasti ad esempio).

- **Assunzioni e Rilassamenti**  
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

#### **Problema di pianificazione $\mathcal{P}$** (+ Complessità PlanSAT, Bounded PlanSAT)  

- **Definizione**  
  Un problema di pianificazione classica $\mathcal{P}=(\Sigma,s_0,S_g)$ ha:
  - $\Sigma=(S,A,\gamma)$, modello del dominio espresso come STS (State Transition System)
  - $s_0 \in S$ è lo stato iniziale
  - $S_g \subset S$ è l'insieme degli stati goal
  
  Una soluzione $\pi$ ad un problema $\mathcal{P}$ è una sequenza totalmente ordinata di azioni istanziate (**ground**, cioè nelle quali non compaiono formule) $\pi = \langle a_1,...,a_n\rangle$ che danno origine ad una sequenza di transizioni di stato $\langle s_0,...,s_n \rangle$ tale che:
  - $s_1=\gamma(s_0,a_1)$ (la prima azione applicata con la funzione di transizione gamma porta allo stato $s_1$)
  - $\forall k: 2..n, s_k = \gamma(s_{k-1},a_k)$ (tutte le azioni successive in sequenza, applicate con la f.ne di transizione gamma allo stato precedente portino allo stato direttamente successivo)
  - $s_n \in S_g$ (lo stato finale raggiunto dal piano faccia parte degli stati goal)

- **Proprietà di un buon algoritmo di pianificazione**  
  - **Soundness** (correttezza)  
    Un pianificatore è **corretto** se tutte le soluzioni che trova sono piani corretti, ovvero realmente eseguibili dal controller. Questo significa che:  
    - Tutti i goal sono soddisfatti
    - Nessuna precondizione di azione non insoddisfatta (o mancante)
    - Nessun vincolo ulteriore è violato (temporali ad esempio)
  - **Completeness** (completezza)  
    Un pianificatore è **completo** se trova una soluzione quando il problema è risolubile.  
    Un pianificatore è **strettamente completo** se, nonostante esso faccia (ad esempio) pruning dell'albero di ricerca, non scarta alcuna soluzione possibile.
  - **Ottimalità**  
    Un pianificatore è **ottimo** se l'ordine con cui le soluzioni sono trovate è coerente con una qualche misura di qualità dei piani (lunghezza, costo complessivo)
  
- **Complessità**  
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

- Algoritmi di Pianificazione
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

- **Definizione** (Linear Planning + Means-End Analysis)
  Introduce una rappresentazione esplicita degli **operatori di pianificazione**.  
  Fornisce una operazionalizzazione delle nozioni di
  - differenza tra stati
  - subgoal
  - applicazione di un operatore

  E' in grado di gestire il frame problem.  
  Si basa su due idee fondamentali:
  - **Linear Planning**  
    L'idea di base è di risolvere un goal alla volta, passando al successivo solo quando il precedente è stato raggiunto.  
    L'algoritmo di planning mantiene uno **Stack dei Goal** (un goal per essere risolto può richiedere la risoluzione di sottogoal). Ne consegue che:
    - Non c'è interleaving nel conseguimento dei goal
    - La ricerca è efficiente se i goal non interferiscono troppo tra di loro
    - E' soggetto all'Anomalia di Sussmann
  - **Means-End Analysis**  
    L'idea è di considerare solo gli aspetti rilevanti al problema (ricerca backward). Si ragiona su quali mezzi (_means_, operatori) sono disponibili e necessari per raggiungere il goal.  
    Occorre stabilire quali differenze ci sono tra lo stato corrente e il goal (means-end analysis), trovare un operatore che riduca tale differenza e ripetere l'analisi sui sottogoal ottenuti per regressione attraverso l'operatore scelto.

- **Stati**  
  Il linguaggio di STRIPS $\subset$ FOL: abbiamo un numero finito di simboli di predicati, simboli costanti e **non sono presenti** simboli di funzione o quantificatori.  
  Uno stato in STRIPS è una congiunzione di **atomi ground** (privi di simboli di funzione).  
  Un atomo ground $p$ **vale in uno stato** $s\ sse\ p \in s$.  
  La semantica è logic-oriented: uno stato $s$ soddisfa una congiunzione di letterali $g$ $s \vDash g$ se:
  - Ogni letterale positivo in $g$ occorre in $s$
  - Ogni letterale negativo in $g$ **non** occorre in $s$

  Mondo dei blocchi:
  $$s=\{On(A,B),\\OnTable(C),\\OnTable(A),\\Block(A),\\Block(B),\\Block(C),\\Handempty\}$$

- **Relazioni**
  - **Fluenti**  
    Predicati che rappresentano relazioni il cui valore di verità può cambiare da uno stato al successivo ($On$, $OnTable$)
  - **Persistenti** (o state invariant)  
    Predicati il cui valore di verità non può cambiare ($Block$)
- **Plan Operators**  
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

- **Azione, Applicabilità e Funzione $\gamma(s,a)$**  
  Sia $s$ uno stato del mondo ($s \in S$ per un dato dominio $\Sigma$)  
  Sia $a$ un'**Azione** (istanziazione di un plan operator $o$)  
  Diremo che $a$ è **applicabile** in s se e solo se:
  - $precond^+(a)\subseteq s$ (le precondizioni sono nello stato $s$)
  - $precond^-(a)\cap s = \empty$ (le precondizioni che non devono essere in $s$ non lo sono effettivamente)

  La funzione di transizione di stato $\gamma(s,a)$ è definita come:
  - Se $a$ è applicabile in $s$:  
    $\gamma(s,a) = (s \setminus effects^-(a)) \cup effects^+(a) $
  - Altrimenti $\gamma(s,a)$ non è definita.

- **Algoritmo** (+ Vantaggi e Svantaggi)

  ```?
  STRIPS (initState, goals )
    state = initState; plan = []; stack = []
    Push goals on stack
    Repeat until stack is empty
      ◦ If top of stack is a goal g satisfied in state, then pop stack
        (se in cima allo stack c'è un goal atomico/complesso soddisfatto, non devo fare nulla)
      ◦ Else if top of stack is a conjunctive goal g , then
        (se in cima allo stack c'è un goal complesso, semplificalo [linearizzazione])
          Select an ordering for the subgoals of g , and push them on stack 
      ◦ Else if top of stack is a simple goal sg , then
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

- **Anomalia di Sussman**  
  Uno degli svantaggi di STRIPS è che il linear planning può produrre piani subottimi (se come parametro di qualità ci basiamo sulla lunghezza).  
  L'interdipendenza tra i goal e un possibile ordinamento sfavorevole può portare all'anomalia, che si verifica quando è necessario **disfare** parte dei goal già raggiunti per poter risolvere l'intero problema.

- **Incompletezza di STRIPS**  
  Il linear planning non è completo: questa è una conseguenza della **non reversibilità** delle azioni in alcuni domini specifici (ad esempio la logistica e il trasporto di merci).

- **PDDL**  
  Risolve alcuni problemi di STRIPS:
  1) Potere espressivo, che permette di semplificare i predicati e allontanarsi dalla FOL
  2) Proposizionalizzazione per alcune specifiche condizioni ed azioni
  Alcune estensioni di PDDL:
  - Conditional effects (alcuni effetti sono raggiungibili solo se vere alcune condizioni)
  - Durative Actions
  - Fluenti Numerici
  - Quantificatori ($\exists, \forall$)

#### Progression e Regression

- **Progression**  
  Calcolo lo stato successione $s'$ di uno stato $s$ rispetto all'applicazione di un operatore $o$: $s' = \gamma(s,o)$  
  _Soundness_: corretto: se la funzione termina con un piano come soluzione, quella è effettivamente una soluzione per il problema iniziale  
  _Completeness_: completo: se esiste una soluzione, allora esisterà un percorso d'esecuzione che restituirà quella soluzione come piano.

- **Regression**  
  Dato che il numero di azioni applicabili in uno stato è generalmente molto grande, anche il branching factor tende ad essere grande: la ricerca in avanti rischia di non essere praticabile dopo pochi passi.

- **Progression vs Regression**  
  1) La ricerca in avanti si parte da un solo stato iniziale.  
    La ricerca in indietro comincia da un insieme di stati goal.
  2) Quando si applica in avanti un operatore $o$ ad uno stato $s$ si genera un unico successore $s'$.  
    Quando da uno stato $s'$ vogliamo fare un passo all'indietro, scopriamo che ci possono essere _molteplici stati predecessori_.
  3) Nella ricerca in avanti lo spazio di ricerca coincide con lo spazio degli stati (stati del dominio originati da $\Sigma$ generalmente)
    Nella ricerca all'indietro ogni stato dello spazio di ricerca corrisponde ad un insieme di stati del dominio.

  La ricerca all'indietro ha il vantaggio di poter gestire più stati simultaneamente (belief states), portando ad uno spazio di ricerca più compatto.  
  Inoltre, si basa su azioni **rilevanti**, cioè quelle che contribuiscono attivamente al goal. Otteniamo così un branching factor più ridotto, ma il fatto di dover mantenere un belief state può complicare il planner.

### CSP

(Constraint Satisfaction Problems)  
Formulare un problema di pianificazione come un problema di soddisfacimento di vincoli consente di ritardare alcune decisioni fino a quando non è strettamente necessario.

- **Come costruire un modello espresso da variabili di stato?**  
  - Ambiente E che vogliamo modellare
  - B è l'insieme degli oggetti rilevanti per il problema che si vuole risolvere. Deve astrarre i dettagli insignificanti.

- **Proprietà nei CSP**
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

#### **Least-Commitment Planning**

- **Principio**
  Fare scelte solo quando queste sono indispensabili per risolvere una parte del problema.  
  Durante la ricerca bisogna non porre più vincoli di quelli strettamente necessari.  
  E' possibile ritardare:
  - Ordinamenti (non ordinare se non necessario)
  - Bindings: non vincolare le variabili se non è necessario unificarle con costanti per conseguire i goal

- **Perchè è un pianificatore particolare? In quale spazio cerca?**
  E' particolare perchè fa scelte solo quando indispensabili per risolvere il problema (ritarda ordinamenti e bindings).  
  Cerca nello **SPAZIO DEI PIANI PARZIALI**. Ogni nodo della ricerca è un piano parzialmente ordinato con flaws, che ad ogni passo sono rimosse attraverso raffinamenti incrementali.  
  Se l'algoritmo termina con successo, il piano risultante è completamente istanziato (ma solo parzialmente ordinato).

- **Piano $\langle A,O,B,L\rangle$**  
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

- **Flaws** (Open Goals, Threats)  
  - **Open Goals**  
    Una precondizione $p$ per un'azione $b$ è un **open goal** se non c'è un link causale a supporto di p, cioè manca $?\xrightarrow{p}b$ in $L$.  
    Bisognerà risolvere il difetto aggiungendo il link mancante:
    1) Trova un'azione $a$ (possibilmente già in $\pi$, o in alternativa da aggiungere ad essa) tale che:
        - $p$ può appartenere a $effect^+(a)$ (previa unificazione)
        - $a$ può precedere $b$ in $\pi$
    2) istanzia l'azione $a$ in modo che asserisca $p$ (istanziazione least commitment, solo lo stretto necessario)
    3) aggiungi un vincolo di precedenza $a \prec b$ in $O$
    4) aggiungi un causal link a $a \xrightarrow{p} b$ in $L$
- Proprietà degli oggetti
- Principio Least-Commitment
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
