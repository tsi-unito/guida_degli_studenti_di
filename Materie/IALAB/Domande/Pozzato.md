# Pozzato

- [Pozzato](#pozzato)
  - [**Unificazione**](#unificazione)
    - [**Definizione**](#definizione)
    - [**Most General Unifier**](#most-general-unifier)
      - [**Algoritmo MGU**](#algoritmo-mgu)
  - [**Prolog**](#prolog)
    - [**Info**](#info)
    - [**Applicabilità di una regola**](#applicabilità-di-una-regola)
    - [**Interprete Prolog**](#interprete-prolog)
    - [**Risoluzione SLD** (Albero SLD, Terminazione, Non-Determinismo)](#risoluzione-sld-albero-sld-terminazione-non-determinismo)
      - [**Intro**](#intro)
      - [**Terminazione**](#terminazione)
      - [**Non Determinismo**](#non-determinismo)
      - [**Regola di Calcolo**](#regola-di-calcolo)
      - [**Albero SLD**](#albero-sld)
      - [**La risoluzione SLD è corretta e completa per tutta la logica o solo per alcuni tipi di clausole?**](#la-risoluzione-sld-è-corretta-e-completa-per-tutta-la-logica-o-solo-per-alcuni-tipi-di-clausole)
      - [**Negazione per fallimento** (Ragionamento Non-Monotono)**](#negazione-per-fallimento-ragionamento-non-monotono)
      - [**3 Possibili esiti con negazione per fallimento**](#3-possibili-esiti-con-negazione-per-fallimento)
      - [**Risoluzione SLD con Prolog**](#risoluzione-sld-con-prolog)
      - [**Il CUT**](#il-cut)
      - [**La presenza del cut cosa va a mettere a rischio tra completezza e correttezza?**](#la-presenza-del-cut-cosa-va-a-mettere-a-rischio-tra-completezza-e-correttezza)
      - [**Se prendo delle clausole che non sono di Horn, cosa si mette a rischio tra completezza e correttezza?**](#se-prendo-delle-clausole-che-non-sono-di-horn-cosa-si-mette-a-rischio-tra-completezza-e-correttezza)
  - [**Ricerca nello spazio degli stati**](#ricerca-nello-spazio-degli-stati)
    - [**Problema di ricerca**](#problema-di-ricerca)
    - [**Spazio degli stati**](#spazio-degli-stati)
    - [**Cammino**](#cammino)
    - [**Soluzione**](#soluzione)
    - [**In Prolog?**](#in-prolog)
  - [**Strategie di ricerca**](#strategie-di-ricerca)
    - [**Strategie non informate**](#strategie-non-informate)
      - [**Ricerca in profondità**](#ricerca-in-profondità)
      - [**Ricerca a profondità Limitata**](#ricerca-a-profondità-limitata)
      - [**Iterative Deepening**](#iterative-deepening)
      - [**Ricerca in ampiezza**](#ricerca-in-ampiezza)
    - [**Strategie informate**](#strategie-informate)
      - [**A\***](#a)
      - [**IDA\***](#ida)
  - [**Answer Set Programming**](#answer-set-programming)
    - [**Definizione ASP**](#definizione-asp)
    - [**Prolog vs Clingo**](#prolog-vs-clingo)
    - [**Negazione classica vs. per fallimento**](#negazione-classica-vs-per-fallimento)
    - [**Answer Set**](#answer-set)
    - [**Programma Ridotto**](#programma-ridotto)
    - [**Modello Minimale di Herbrand**](#modello-minimale-di-herbrand)
    - [**Nel ridotto c'è un solo answer set?**](#nel-ridotto-cè-un-solo-answer-set)
    - [**Integrity Constraint**](#integrity-constraint)
    - [**Aggregati**](#aggregati)
    - [**Risoluzione del problema ASP**](#risoluzione-del-problema-asp)

## **Unificazione**

### **Definizione**

Due termini $t1$ e $t2$ sono unificabili se esiste una sostituzione $\sigma$ (**unificatore**) che li rende identici: $t1\sigma = t2\sigma$.
$$
t_1=f(x_1,h(x_1),x_2) \\
t_2=f(g(x_3),x4,x3) \\
\sigma=\{x_1/g(x_3),x_2/x_3,x_4/h(g(x_3))\}
$$

### **Most General Unifier**

Una sostituzione $\theta$ è **più generale** di una soluzione $\sigma$ se esiste una sostituzione $\lambda$ tale che $\sigma=\theta\lambda$.  
Si può dimostrare che, se due termini sono unificabili, esiste sempre un **unificatore più generale**unico a meno di ridenominazione delle variabili.

#### **Algoritmo MGU**

L'algoritmo MGU Martelli ha il compito di trovare un *MGU* $\sigma$ più generale:

- Scegliere un'equazione della forma $t=x$ dove $t$ non è una variabile ma $x$ lo è, diventa $x=t$
- Cancellare tutte le equazioni della forma $x=x$
- Scegliere un'equazione della forma $t'=t''$, entrambi non variabili. Se sono diversi i simboli di funzione, fallisco, altrimenti facciamo la **Term Reduction**
- Scegliere un'equazione della forma $x=t$ dove $x$ è una variabile che occorre da qualche altra parte e $t\neq x$. Se *x* occorre in *t*, fallimento. Altrimenti, faccio **Variable Elimination**

## **Prolog**

### **Info**

Prolog è un linguaggio di programmazione logica, cioè dove invece di programmare con uno dei normali paradigmi si enunciano clausole per un programma, che poi il motore di inferenza utilizzerà per cercare di dimostrare un goal.  
La ricerca della soluzione avviene mediante backward chaining in profondità: si parte dal goal che si vuole derivare (scritto come una congiunzione di formule atomiche $G_1,G_2,...,G_n$) e si cerca di risolverlo, dimostrando o meno che il goal **segue logicamente** dal programma.

### **Applicabilità di una regola**

Una regola $A \coloneq B_1,B_2,...,B_m$ è applicabile a $G_i$ se:

- le variabili vengono rinominate e
- $A$ e $G_i$ unificano

### **Interprete Prolog**

L'interprete prolog cerca di dimostrare il Goal. La computazione ha successo se esiste una computazione che termina con successo.  
L'interprete Prolog si comporta deterministicamente:

- Le clausole vengono considerate nell'ordine in cui sono scritte nel programma
- Viene fatto backtracking all'ultimo punto di scelta ogni volta che la computazione fallisce.

In caso di successo, l'interprete restituisce una sostituzione per le variabili che compaiono nel goal.

### **Risoluzione SLD** (Albero SLD, Terminazione, Non-Determinismo)  

#### **Intro**

Per arrivare ad un linguaggio di programmazione ci serve una strategia efficiente per risolvere il problema.  
Risoluzione SLD: **Linear resolution with Selection function for Definite clauses**  

- Si lavora con **clausole di Horn**: una disgiunzione di letterali in cui al massimo uno dei letterali è positivo.
- **Strategia Linear Input**: ad ogni passo di risoluzione, una **variante** di una clausola è sempre scelta nella K (_teoria_/_programma_) di partenza, mentre l'altra è sempre il risolvente del passo precedente (goal, la negazione di F al primo passo, visto che si cerca di risolvere per negazione).  
- La derivazione SLD per un goal $G_0$ da un insieme di clausole $K$ è:
- Una sequenza di clausole goal $G_0,G_1,...,G_n$
- Una sequenza di varianti di clausole di $K$ $C_1,C_2,...,C_n$
- Una sequenza di MGU $\sigma_1,\sigma_2,...,\sigma_n$, tali che $G_{i+1}$ è derivato da $G_i$ e da $C_{i+1}$ attraverso la sostituzione $\sigma_{i+1}$.

#### **Terminazione**

- Ci possono essere tre tipi di derivazioni:
  - **Successo** se $G_n = \square$
  - **Fallimento finito** se non è possibile derivare da $G_n$ alcun risolvente e $G_n \neq \square$
  - **Fallimento infinito** se è sempre possibile derivare nuovi risolventi

#### **Non Determinismo**

Due forme di non determinismo:

1) Regola di calcolo per selezionare ad ogni passo l'atomo $B_i$ del goal da unificare con una clausola
2) Scelta di quale clausola utilizzare ad ogni passo di risoluzione

#### **Regola di Calcolo**

Funzione che ha come dominio l'insieme dei goal e per ogni goal seleziona un suo atomo.  
Non influenza la correttezza e completezza del metodo di prova.

#### **Albero SLD**

Data una regola di calcolo, è possibile rappresentare tutte le derivazioni con un albero SLD:  

- Nodo: goal
- Radice: goal iniziale $G_0$
- Ogni nodo $\leftarrow A_1,...,A_m,...,A_k$, dove $A_m$ è l'atomo selezionato dalla regola di calcolo, ha un figlio per ogni clausola $A \leftarrow B_1,...,B_k$ tale che $A$ e $A_m$ sono unificabili con il MGU $\sigma$.  
Il nodo figlio è etichettato con il goal $\leftarrow [A_1,...,A_{m-1},B_1,...,B_k,A_{m+1},...,A_k]\sigma$.
Il ramo dal padre al figlio è etichettato con $\sigma$ e con la clausola selezionata.

#### **La risoluzione SLD è corretta e completa per tutta la logica o solo per alcuni tipi di clausole?**

_In generale_ **NON** è completa, ma con le clasuole di Horn SI.

#### **Negazione per fallimento** (Ragionamento Non-Monotono)**

E' una regola usata nella programmazione logica per derivare $not\ p$ (nel senso che p non è vero), dal risultante fallimento nel derivare $p$.  
$not\ p$ è diverso da $\neg p$, cioè la negazione logica di $p$.

#### **3 Possibili esiti con negazione per fallimento**

1) Se la query negata è soddisfatta, la negazione fallisce
2) Se la query negata non è soddisfatta, la negazione ha successo  
   Se la query richiede delle informazioni che non sono presenti nella KB (in poche parole, non sappiamo/conosciamo qualche informazione), dato che Prolog si basa sulla Assunzione di Mondo Aperto, non possiamo decidere se un'informazione mancante sia mancante volontariamente o meno, per cui ha successo.
3) Fallimento infinito (se è sempre possibile derivare nuovi risolventi)

#### **Risoluzione SLD con Prolog**

Una risoluzione in prolog **si basa sulla dimostrazione del goal mediante risoluzione SLD**.  
Per rendere la strategia deterministica:

- Si sceglie sempre il **sottogoal più a sinistra**
- Le clausole sono considerate **nell'ordine in cui sono scritte** nel programma
- La strategia di ricerca è **in profondità, con backtracking**.  
**Non è tuttavia completa**: se una computazione che porterebbe al successo della risoluzione si trova a destra di un ramo infinito, l'interprete non lo riuscirà mai a raggiungere perchè continuerà ad entrare nel ramo infinito.

#### **Il CUT**

Consente di modificare e controllare l'esecuzione dell'interprete Prolog.  
Si tratta di un predicato che è **sempre vero**, che se eseguito blocca il backtracking.  
L'interprete Prolog utilizza due stack (in realtà uno, ma si alternano i due blocchi):  

1) **Stack di esecuzione**: contiene i predicati attualmente attivati e ancora in esecuzione
2) **Stack di backtracking**: contiene i punti di scelta (choice points) che sono in valutazione (o devono ancora essere valutati)

Quando una valutazione fallisce, si effettua backtracking e si prova a valutare un altro ramo non ancora esplorato. Se non ci sono più punti di scelta, si ha il fallimento del predicato attualmente in fase di valutazione.

In pratica, il CUT **rende definitive le scelte effettuate nel corso della valutazione** da parte dell'interprete: elimina i choice point dallo stack di backtracking, alterando quindi il controllo del programma. Perdiamo in dichiaratività

#### **La presenza del cut cosa va a mettere a rischio tra completezza e correttezza?**

Andando a tagliare alcuni rami dell'albero SLD, rimuovendo alcuni punti di backtracking, non assicuriamo più la **COMPLETEZZA**.

#### **Se prendo delle clausole che non sono di Horn, cosa si mette a rischio tra completezza e correttezza?**

Completezza, perchè nel caso non possiamo risolvere parte delle clausole
  
## **Ricerca nello spazio degli stati**

### **Problema di ricerca**

- Stato iniziale
- Insieme delle azioni (un'azione fa passare da uno stato ad un altro)
- Obiettivi
- Costo di ogni azione

### **Spazio degli stati**

  E' composto da: $StatoIniziale$ $\cup$ $InsiemeAzioni$ e corrisponde all'insieme di tutti gli stati raggiungibili a partire da quello iniziale.

### **Cammino**

  Sequenza di stati collegati da una sequenza di azioni.  
  Il suo **costo** è dato dalla somma dei singoli costi di tutte le azioni che lo compongono.

### **Soluzione**

  La soluzione ad un problema è un **cammino** dallo stato iniziale ad uno stato goal.  
  La soluzione è **ottima** se è quella con il costo minimo tra tutte le soluzioni possibili.

### **In Prolog?**

  Le azioni le rappresentiamo con delle clausole del linguaggio. Ognuna avrà delle precondizioni (stati in cui può essere eseguita) e degli effetti quando sono eseguite. (Applicabile e Trasforma)

## **Strategie di ricerca**

### **Strategie non informate**

#### **Ricerca in profondità**

  Si espande sempre per primo il nodo più distante dalla radice dell'albero di ricerca.  
  Facilmente realizzabile in Prolog grazie al suo non-determinismo (chiamate ricorsive continue).  
  L'interprete Prolog se non trova una soluzione, fa backtracking, quindi realizza una **ricerca in profondità con backtracking**.

#### **Ricerca a profondità Limitata**

  Uguale alla ricerca in profondità, ma si usa un parametro che vincola la profondità massima oltre la quale i nodi non vengono espansi. La procedura che ne risulta **non è completa**.

#### **Iterative Deepening**

  Ripete la ricerca a profondità limitata, incrementando ad ogni passo il limite.  
  Nel caso in cui le azioni del problema abbiano tutte costo unitario, la strategia è **ottima**.

#### **Ricerca in ampiezza**

  Si utilizza una coda di nodi.  
  Ad ogni passo la procedura espande il nodo in testa alla coda (usando una `findall`) generando tutti i suoi successori, che vengono aggiunti in fondo alla coda.  
  E' sicuro che venga individuata la soluzione ottima.  

  Se si lavora con **grafi**, si conserva una lista dei nodi già espansi, per non esplorarli nuovamente.

### **Strategie informate**

  Si utilizza una funzione euristica $h(n)$, che corrisponde al costo stimato del cammino più conveniente dal nodo n ad uno stato finale.  
  Ogni azione ha anche associato un costo $g(n)$, che corrisponde al costo del cammino trovato dal nodo iniziale a n.

#### **A\***

  Ricerca in ampiezza su grafi, tiene conto della funzione euristica.  
  Ad ogni passo si estrae per l'espansione dalla coda il nodo con minimo valore di $f(n) = g(n)+h(n)$.  
  I nodi già esplorati/espansi non vengono ulteriormente toccati.

#### **IDA\***

  Coincide con l'_Iterative Deepening_, ma la soglia è stimata basandosi sull'euristica.  
  Alla prima esecuzione è $h(StatoIniziale)$.  
  Per ogni esecuzione successiva, la soglia è il minimo $f(n)$, scelto dall'insieme dei nodi che al termine dell'esecuzione precedente superavano la soglia.

## **Answer Set Programming**

### **Definizione ASP**

Inizialmente era stato ideato per dare una semantica alla negazione per fallimento di Prolog.  
Non si cercano più le prove, ma i modelli che devono essere **stabili**; non si fa più inferenza con backward chaining. E' particolarmente utile per risolvere problemi combinatori (con vincoli, pianificazione).

Viene dichiarato un insieme finito di regole del tipo $a \coloneq b_1,b_2,...,b_n, not\ c_1, not\ c_2,...,not\ c_m$ Con $a, b_i, c_j$ letterali della forma $p$ o $\neg p$.

- $\neg$ coincide con la negazione classica.  
- $not$ è la negazione per fallimento
  
### **Prolog vs Clingo**

- In ASP l'ordine dei letterali non ha alcuna importanza
- Prolog è goal directed (backward chaining!!), ASP no
- La SLD-riduzione di Prolog può portare a loop, mentre gli ASP solver non lo permettono
- Prolog ha il CUT, ASP no.

### **Negazione classica vs. per fallimento**

- **Classica**  
  $attraversa \coloneq \neg treno$ : più forte. _Si attraversa solo se si può derivare che il treno non è in arrivo._

- **Per Fallimento**  
  $attraversa \coloneq not\ treno$ : _si può attraversare in assenza di informazione esplicita sul treno in arrivo._

### **Answer Set**

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

### **Programma Ridotto**

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

### **Modello Minimale di Herbrand**

  Il modello minimale di ASP è minimo rispetto all'inclusione insiemistica verso il modello di Herbrand, che è un insieme di *atomi ground* tali che tutte le clausole del programma sono **entailed**, ovvero, se ho il body vero, allora anche la mia testa è nel modello.

### **Nel ridotto c'è un solo answer set?**

  No, è possibile che da un ridotto risultino anche più answer set.  
  Gli ASP-solver trovano (se richiesto) tutte le soluzioni nel momento stesso in cui calcola gli answer set del programma.

### **Integrity Constraint**

  $a$ (la testa della regola) è **opzionale**; senza abbiamo un _integrity constraint_, che invalida tutti i modelli dove tutte le $a_n$ sono verificate:
  $$\coloneq a_1, a_2,...,a_k$$
  (è inconsistente che $a_1, a_2,...,a_k$ siano tutti veri)

### **Aggregati**

  Si tratta di costrutti che selezionano answer set contenenti almeno $n$ e al massimo $m$ elementi di un insieme. Avremo da $n$ a $m$ atomi $p$, i cui possibili valori sono presi da $q$:
  $$n\ \{p(X)\!:q(X)\}\ m.$$
  Esempio: _ogni variabile deve avere un solo tipo_:
  $$1\ \{ha\_tipo(V,T) \! : tipo(T)\}\ 1 \coloneq variabile(V).$$

### **Risoluzione del problema ASP**

- Descrizione mediante regole degli answer set potenziali
- Introduzione di vincoli (regole senza testa) che invalidino gli answer set non accettabili
