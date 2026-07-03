---
title: 'Intelligenza artificiale e laboratorio - Parte II - CLIPS'
---

# Intelligenza artificiale e laboratorio - Parte II

## CLIPS

### Introduzione a CLIPS

- **CLIPS** è uno strumento per la creazione di sistemi esperti.
    - Ha un sistema di tipi primitivi:
        - float, integer, **symbol**, string, external address, instance name, instance address.

#### Fatti

- **Fatto**: elemento indivisibile di informazione.
    - Definiti da un nome di relazione.
    - Possono avere zero o più *slot* (i.e. *facet*).
        - Se non hanno slot il nome della relazione li identifica univocamente.
        - Due fatti sono distinti perché hanno una lista di simboli (ordinata) distinta.
        - Non è possibile definire lo stesso fatto più volte nella WM.
    - Costrutti:
        - `deftemplate`: per definire fatti strutturati (dati non ordinati).
        - `deffacts`: per definire il set iniziale di fatti.
    - Un fatto può essere ordinato o non ordinato:
        - **Fatto ordinato** (no slot).
            - Distinto da un nome di relazione e da una lista (eventualmente vuota) di simboli.
            - `eg` `(person-name Franco L Verdi)` e `(person-name Verdi L Franco)`.
        - **Fatto non ordinato** (definito tramite un **template**, un modello).
            - Non è necessario definire un valore per ogni slot del template.
            - L'ordine con cui si definiscono gli slot è irrilevante, non serve a identificare il fatto.
            - La dichiarazione di un fatto tramite un template può essere arricchita con **tipi**, range, default, cardinalità.
                - Il tipo di default è il supertipo `VALUE`.

```lisp
; esempio di template
(deftemplate person "commento opzionale"
    (slot name)
    (slot age)
    (slot eye-color)
    (slot hair-color))

; esempio di istanza di template
(person (name "Franco L. Verdi")
    (age 46)
    (eye-color brown)
    (hair-color brown))

; esempi di template con tipi, default, range, multislot
(deftemplate studente "a student record"
    (slot name (type STRING))
    (slot age (type INTEGER) (default 18) (range 0 ?VARIABLE))
    (slot gender (type SYMBOL) (alowed-symbols male female)))

(deftemplate volleball-team
    (slot name (type STRING))
    (multislot player (type STRING) (cardinality 6 6))
    (multislot player alternates (type STRING) (cardinality 0 2)))
```

- `deffacts` è usato unicamente per stabilire quali fatti sono veri all'avvio del motore inferenziale.
    - I fatti sono effettivamente asseriti in WM solo dopo aver dato il comando `(reset)`.
    - Per elencare i fatti presenti in memoria si usa `(facts)`.
- Manipolazione di fatti:
    - Aggiungere fatti: `(assert <fact>+)`.
    - Rimuovere fatti: `(retract <fact-index>+)`.
    - Modificare fatti: `(modify <fact-index> (<slot-name> <slot-value>)+ )` (indice viene incrementato).
    - Duplicare fatti: `(duplicate <fact-index> (<slot-name> <slot-value>)+ )`.
        - Si duplica specificando una variante.
    - Ispezionare la working memory:
        - `(facts)` stampa la lista di fatti.
        - `(watch facts)` mostra i cambiamenti che occorrono alla WM a seguito dell'esecuzione delle regole.
            - `(unwatch facts)` per disabilitare il watching sui fatti.

#### Regole

- Una **regola** è definita dal suo nome, da eventuali pattern e eventuali azioni.
    - Un **pattern** è un test tra ciò che viene indicato e qualche fatto in WM.
        - Per capire se una regola è applicabile, si applica un processo di pattern-matching.
        - Nei pattern in media si ricorre all'uso di **variabili**.
    - Le azioni sono quelle che agiscono sui fatti in WM (`assert`, `modify`, ecc).
    - Una regola può non avere l'**antecedente**.
        - Verrà attivata da `(initial-fact)`.

```lisp
(defrule <rule name> ["comment"]
    <patterns>* ; left-hand side (LHS)
                ; or antecedent of the rule
    =>
    <actions>*  ; right-hand side (RHS)
                ; or consequent of the rule
)
```

- `(agenda)` permette di visualizzare le regole applicabili.
    - Vengono indicati gli indici dei fatti per l'attivazione della regola.
        - L'**attivazione della regola** è l'insieme dei fatti che rendono vere le precondizioni.
        - È possibile tracciare le attivazioni con `(watch activations)`.
        - CLIPS preferisce le attivazioni basate su fatti più recenti.
    - In una agenda possono essere presenti più regole, attivabili da fatti diversi.
        - Con attivazioni quindi diverse.
- Per default i pattern nell'antecedente delle regole sono considerati in **congiunzione**.
    - È però possibile definire comportamenti differenti.
    - `(or (pattern1) (pattern2))`: soddisfatto se almeno uno dei due pattern unifica con i fatti della WM.
        - In base a determinate condizioni, potrebbe essere attivato più volte.
        - Bisogna quindi modellare correttamente il problema.
    - `(not (pattern))`: soddisfatto quando nessun fatto unifica con il pattern.
    - `(exists (pattern))`: soddisfatto per un unico fatto che unifica
    - `(forall (pattern))`: soddisfatto se pattern vale per tutti i fatti che unificano.

#### Variabili

- Le **variabili** sono nomi simbolici che cominciano con `?`.
    - CLIPS assegna a queste variabili un valore (tramite il pattern matching).
    - **Binding**:
        - Variabili in un pattern (LHS) sono legate a valori di fatti in WM.
        - Ogni occorrenza di una variabile in una regola ha lo stesso valore.
        - L'occorrenza più a sinistra (la prima) determina il valore.
        - Il binding è valido solo localmente ad una regola.
        - Le variabili sono usate anche come *handlers* per accedere ai fatti della WM.
            - `eg` `?age <- (age harry 17)`, `age` conterrà l'indice del fatto.
    - È possibili definire delle wildcards, variabili senza nome.
        - `?`: match con qualsiasi valore in un campo singolo (slot) di un fatto.
        - `$?`: match con zero o più valori in un multislot di un fatto.

```lisp
; esempi di regole con utilizzo di variabili
(defrule birthday-FLV
    ?person <- (person (name "Luigi") ; fatto non ordinato
        (age 46)
        (eye-color brown)
        (hair-color brown))
        (date-today April-13-02) ; fatto ordinato
    =>
        (printout t "Happy birthday, Luigi!" crlf)
        (modify ?person (age 47))
)

(defrule find-blue-eyes
    (person (name ?name) (eye-color blue))
    =>
    (printout t ?name " has blue eyes." ctrlf))
```

- **Rifrazione**: un meccanismo alla base del PM che impedisce di attivare due volte una regola sugli stessi fatti.
    - Permettendo di evitare loop infiniti.
    - Se un fatto viene modificato, una regola che lo coinvolge diventa di nuovo (o potenzialmente) applicabile.
- `(run)` permette di eseguire l'agenda.
    - Esegue finché sono presenti regole attivabili (rispettando la rifrazione).
    - È possibile inserire il comando `(halt)` nel conseguente di alcune regole per fermare il motore inferenziale.
- `(bind)` permettere di assegnare un valore a una **nuova variabile** nel conseguente di una regola.

#### Motore inferenziale di CLIPS

- Funzionamento **forward-chaining** del **motore inferenziale** di CLIPS:
    1. **Determina** tutte le regole attivabili dai fatti presenti in working memory.
    2. **Ordina** le regole attivabili in agenda in base al criterio di *conflict resolution* (e alla priorità delle regole stesse). 
    3. **Fire** della regola in cima all'agenda.
    4. Torna al primo punto.
- Osservazioni sul suo funzionamento:
    - CLIPS produce un risultato per mezzo di una **concatenazione di regole**.
        - La concatenazione stessa è una **spiegazione delle conclusioni**.
        - In quanto dimostra come si è giunti ad una certa conclusione.
    - CLIPS è un sistema **eterarchico**.
        - Non esiste una *regola main*.
        - Tutte le regole sono sullo stesso piano (sistema piatto).
    - L'**agenda è ridefinita ad ogni ciclo**.
        - La regola attivabile in un passo, può non esserlo nel passo successivo.

#### CLIPS e Prolog

- CLIPS e Prolog:
    - CLIPS non ha un concetto di goal e non implementa una **ricerca** in profondità come Prolog.
        - Il sistema di CLIPS è **piatto**.
        - Se il programmatore desidera una ricerca in profondità con back-tracking, deve implementarla.
    - Le regole vengono eseguite appena sono attivabili.
        - Non è possibile *sequenzializzare* le regole.
        - Ci si può basare sulla **conflict resolution** e sulla prioritizzazione delle regole.
            - Ma sono entrambi meccanismi molto deboli.
    - CLIPS implementa una sorta di **ricerca in ampiezza**.
        - Attiva tutte le regole attivabili.
        - Non è detto che sia la strategia migliore.
    - È possibile strutturare le regole con con un approccio **goal-oriented**.
    - CLIPS è preferibile quando si lavora con **conoscenza esplicita**.

### Pattern matching in CLIPS

- Il **pattern matching** permette di unificare gli antecedenti delle regole con dei fatti in WM.
    - **Field constraints**: vincoli definiti per il singolo campo usati per filtrare il PM.
        - And `&`, or `|`, not `~`,
        - Expression `:`, aggiunge l'espressione che segue come vincolo.
            - `eg` `(age ?age &: (> ?age 20))`.
        - In alternativa ai field contraints è possibile usare la clausola `(test boolean-function)`.
            - I FC sono utilizzati già in fase di PM, il loro utilizzo è preferibile.

#### Pattern matching e multislot

- Pattern matching in presenza di **multislot**:
    - I multislot sono alla base della definizione dei fatti ordinati.
        - Quando si definisce un fatto ordinato è come se ne stesse creando uno non ordinato con template primitivo.
        - È come si stesse ricorrendo alla struttura `(<nome-relazione> <multislot-valori>)`.
    - I multislot possono quindi essere usati sia per descrivere fatti ordinati che non ordinati.
        - Ed essere utilizzati per il PM e il binding di variabili su entrambi i tipi di fatti.
    - **Wildcard**: `?` per slot semplici, `$?` per i multislot.
        - `?` implica la presenza di esattamente un valore, mentre `$?` è annullabile.
        - Un fatto, in base alla sua struttura, può attivare più volte una regola.
            - `eg` `(data blue blue red)` con `(data $?prima blue $?dopo)`.
                - Si hanno due binding differenti, non scatta quindi la rifrazione.
- Funzioni predefinite per lavorare con i multislot:
    - `length`;
    - `member` e `delete-member`;
    - `insert` e `create`;
    - `first`, `rest` e `nth`;
    - `explode`.

### Conflict Resolution

- La posizione di una **regola attiva** dell'agenda è determinata come segue:
    1. In base alla **salience** (*priorità*).
        - Tutte le regole con salience più bassa sono sotto la regola corrente, e viceversa.
    2. A parità di salience, è la **conflict resolution strategy** a determinare la posizione.
    3. Se le precedenti due non ne determinano l'ordine, si segue l'ordine di definizione nel sorgente.
        - A volte si usa l'ordine con cui sono scritte, a volte l'ordine inverso.

#### Salience

- A ogni regola può essere associata una **salience**.
    - È l'ingegnere della conoscenza ad assegnare la priorità.
    - I valori di salience cadono nell'intervallo $[-10000, 10000]$.
    - Non è buona pratica definire la *salience* per stabilire un ordine.
        - È invece meglio definire *tre* o *quattro* **classi di priorità**.
        - E non per sequenzializzarle.
    - La salience si utilizza più per regole relative alla *conoscenza di controllo* che a quella *di dominio*.

#### Strategie di conflict resolution

- **Strategie di conflict resolution** adottate in CLIPS:
    - **Depth**: regole attive più recenti in cima alle altre regole di pari *salience*;
    - **Breadth**: regole attive più recenti dopo regole di pari *salience*;
    - **Simplicity**: regole attive più recenti sopra a regole con uguale o più alta *specificità*;
    - **Complexity**: regole attive più recenti sopra a regole con uguale o più bassa *specificità*;
    - LEX, MEA: da OPS5;
    - **Random**: alle nuove regole attive viene assegnato un valore casuale con cui sono poi ordinate in agenda.
- Possibile cambiare strategia con `(set-strategy <strategy-name>)`.
    - Il **risultato finale del programma** può cambiare fortemente in base alla strategia di CL adottata.
        - Un programma che porta a una soluzione con una strategia, potrebbe finire in loop con un'altra.
    - O anche, il programma può non comportarsi correttamente in tutte le strategie.
    - `set-strategy` può anche essere impostato nel conseguente di una regola.

##### Specificità

- **Specificity**: determinata dal numero di confronti e binding effettuati nella LHS della regola.
    - Il calcolo della specificità è abbastanza sofisticato.
        - Non dipende solo dal numero di pattern di una regola, ma anche di variabili, ecc, all'interno di questi pattern.
    - **Simplicity Strategy**: fra tutte le regole con la stessa *salience*, le nuove regole attivate sono posizionate al di sopra di tutte le attivazioni di regole con maggiore o uguale specificità.
    - **Complexity Strategy**: fra tutte le regole con la stessa *salience*, le nuove regole attivate sono posizionate al di sopra di tutte le attivazioni di regole con minore o uguale specificità.
        - Se una regola è soddisfatta ed è **più complicata** di un'altra, si può assumere che sia **supportata** da più fatti e condizioni.
        - Questa strategia è quindi preferita a quella della semplicità.

##### LEX e MEA

- **LEX** offre un criterio di **ordinamento lessicografico**.
    - A parità di *salience*, le regole attivate sono ordinate come segue: 
        - Ogni fatto all'interno di ogni attivazione è etichettato con un *time tag* per indicare la sua *recentezza*.
            - A parità di fatti, si va a valutare l'ordine di quelli successivi.
        - Le attivazioni sono ordinate per *recentezza*.
            - Dal fatto più recente a quello meno recente.
    - Si eseguono le regole con **attivazioni più recenti**.
- Anche la **MEA** offre un criterio di ordinamento lessicografico.
    - Le attivazioni sono confrontate rispetto alla *recentezza* del fatto che soddisfa il **primo pattern** delle regole.
        - Cioè le attivazioni sono ordinate come nel caso precedente.
        - A parità di *time tag* del primo fatto si confrontano i *time tag* dei fatti successivi.
    - Non c'è un ordinamento *ex-ante* dei fatti come avviene con LEX.
        - Le attivazioni rimangono nell'ordine con cui le ha scritte il programmatore.
    - Permette di utilizzare la **conoscenza di controllo**.
        - Definita attraverso fatti ordinati che **etichettano** le regole che risolvono un particolare aspetto del problema.
        - Permette di controllare **blocchi di regole**, e non singole regole.

### Rappresentazione della conoscenza

- Gli slot di un fatto non ordinato possono essere *single-value* o *multi-value*.
    - Ma ciascuno di questi valori deve essere `VALUE` (o un suo sotto-tipo).
    - Nascondendo dentro un multislot un pezzo di informazione strutturata, si ha una **minore informatività**.
        - Il pattern matching viene reso più complicato e possibilmente inaccurato.
    - In CLIPS non si può avere **template annidati**.
        - Si rappresentano quindi due o più template distingui, e li si *lega logicamente*.
        - Per legare logicamente più fatti occorre un **attributo** che li mette **in relazione**
            - Una sorta di identificatore come nel modello relazionale.
        - `(gensym)` e `(gensym*)` generano interi auto-incrementali.
            - Il secondo ne garantisce l'univocità.
            - Con `(default)` il valore è determinato alla prima asserzione del fatto.
            - Con `(default-dynamic)` il valore viene determinato a ogni sua asserzione.
- Per **rappresentare la storia** in un **mondo dinamico** è necessario arricchire la rappresentazione.
    - Anche in questo caso si può accomunare una serie di eventi con un attributo.
    - **Frame problem**: tutto ciò che non è cambiato per effetto di un'azione deve **persistere**.
        - È necessario **trattare esplicitamente** il frame problem.
        - Con regole che replicano i fatti da uno **stato** al successivo se questi **non sono cambiati**.

### Programmazione modulare

- Programmi in domini reali coinvolgono facilmente migliaia di regole.
    - Bisogna far attenzione a non unire *eccessivamente* conoscenza di dominio a conoscenza di controllo.
        - E a non unire conoscenza che riguarda aspetti differenti del dominio.
    - Essendo la conoscenza **parcellizata** nelle regole, è facile modellarla ma **difficile mantenerla** nel tempo.

#### Fasi e fatti di controllo

- Una soluzione per rendere mantenibile un sistema è quella di distinguere **fasi** e **fatti di controllo**.
    - Si separa la conoscenza di controllo da quella di dominio.
        - A ogni regola è assegnato un **control pattern** che indica in quale fase quella regola è attivabile.
        - Le regole di controllo sono poi definite per **trasferire il controllo** da una fase alla successiva.
    - Ogni regola che appartiene ad una fase specifica ha come primo pattern un **fatto di controllo**.
        - Con la MEA questo pone tutte le regole attive della fase più recente in cima all'agenda.
    - Si adotta la *salience* per **dare priorità** a determinate tipi di regole.
        - In un SE sono quattro le **classi di conoscenza** modellabili tramite regole (*salience* decrescente):
            - **Constraint rules**: applicano vincoli/*business rules* per impedire operazioni illegali;
            - **Expert rules**: descrivono e risolvono il problema.
            - **Query rules**: per ottenere informazioni dall'esterno;
            - **Control rules**: guidano l'esecuzione.
        - Le fasi relative a queste classi non sono sequenziali.
    - In certe situazioni si può voler passare da una fase all'altra in maniera più flessibile.
        - Utilizzando delle fasi, in una transizione si *aggira* la rifrazione.
            - A ogni transizione, tutte le regole disattivate della rifrazione vengono riattivate.
        - È necessario introdurre un modo per **partizionare WM e KB** in maniera più strutturata.

#### Moduli

- CLIPS usa i **moduli** per partizionare KB e WM.
    - Sintassi: `(defmodule <module-name> [<comment>])`.
    - In assenza di indicazioni, tutti i fatti, template e regole sono definite nel modulo `MAIN`.
        - Quando definito un modulo, di default nuove regole e template vengono assegnate a esso.
        - Per specificare il modulo di destinazione: `(defrule modulo::nome-regola ...)`
- Un modulo è un **sistema esperto a sé stante**.
    - I moduli partizionano non solo la KB, ma anche la WM e di conseguenza l'agenda.
    - Ogni modulo ha la **propria agenda** e **propria WM**.
        - Quando si passa da un modulo a un altro per tornare al primo, lo stato della WM/KB/agenda del primo rimane inalterata.
            - Non si *aggira* quindi la rifrazione.
        - Il modulo ha una *storia* a sé stante di attivazioni che non viene persa nel vari passaggi tra moduli.
    - Alcuni comandi utili per consulate WM/KB in presenza di moduli:
        - `(get-current-module)`
        - `(set-current-module module-name)`;
        - `(list-defrules)`: visualizza il contenuto del modulo corrente;
        - `(list-defrules module-name)`: visualizza le regole definite in `module-name`;
        - `(list-defrules *)`: visualizza le regole definite in tutti i moduli;
        - Il comportamento per `(facts)`, `(agenda)`, ecc, è simile.

##### Condivisione tra moduli

- **Condivisione tra moduli**:
    - I costrutti `(defrule)` (conoscenza di dominio) e `(deffacts)` (stato iniziale) sono strettamente privati.
        - I fatti ordinati non sono condivisibili, in quanto spesso rappresentano la conoscenza di controllo.
    - I costrutti `(deftemplate)` possono essere definiti in un modulo ed importati in altri.
        - Quando condiviso, tutti i fatti non ordinati istanza di quel template sono anch'essi condivisi.
        - Un modulo può esportare o importare tutti o solo una parte dei propri template.

##### Focus

- Le regole che sono prese in considerazione per l'esecuzione sono quelle del modulo che nel momento ha il **focus**.
    - Che non corrisponde all'ultimo modulo definito.
- CLIPS gestisce il passaggio da un modulo a un altro tramite uno **stack** chiamato proprio *focus*.
    - Contiene tutti i moduli con regole potenzialmente attivabili.
    - I moduli sullo stack vanno posizionati **esplicitamente**.
    - I comandi di `(reset)` e `(clear)` portano il focus sul modulo `MAIN`.
        - L'esecuzione del programma parte (e termina) sempre dal modulo `MAIN`.
    - `(focus module-name)` pone in cima allo stack *focus* il modulo indicato.
        - Tutte le regole definite in quel modulo saranno prese in esame ed eventualmente attivate.
        - È possibile specificare più moduli: `(focus mod1top mod2 mod3 ...)`.
    - `(list-focus-stack)` visualizza il contenuto dello stack, ma anche `(get-focus-stack)`.
        - Anche il *focus* ha il suo ha il suo `watch`: `(watch focus)`.
- Un modulo rimane in cima allo stack fino a quando:
    - Ci sono regole attive da eseguire (altrimenti, *pop* implicito);
    - Una regole esegue il comando di *pop* esplicito `(pop focus)` oppure `(return)`;
    - Una regola aggiunge un ulteriore modulo in cima allo stack, con `(focus)`.
- È possibile definire delle regole con **auto-focus**.
    - Generalmente si tratta di regole che fanno *reinforcement* di qualche **vincolo** (*constraint rules*).
    - Si attivano quando il suo antecedente è soddisfatto anche quando il suo modulo **non ha il focus**.
        - Utile per il riconoscimento di violazione di un vincolo.
- Si sostituisce i fatti di controllo con i moduli.
    - Ogni volta che una fase termina il *focus* torna al modulo `MAIN` (**coordinatore**).
    - Si hanno molte **meno regole** e più **facili da mantenere**.

### Strategie di ricerca in CLIPS

- Per implementare una **strategia di ricerca** in profondità occorre:
    - Mantenere un cammino aperto.
    - Per ogni stato lungo il cammino, mantenere l'insieme delle possibilità ancora da esplorare.
    - Saper fare **backtracking**.
- Si utilizzano le strutture di CLIPS per ottenere una **ricerca in profondità con backtracking**:
    - Per modellare un singolo stato si usa un id condiviso da tutti i fatti appartenenti a quello stato.
    - Per modellare il cammino aperto si usa la WM.
    - Per tracciare le alternative non ancora esplorate si usa l'agenda (stack).
    - Bisogna assicurarsi la **persistenza** tra una passaggio tra uno stato e uno nuovo.
    - Si tratta di **backtracking cronologico**.
        - Si ha un numero di soluzioni da esplorare altissimo.
    - È possibile implementare solo quando la strategia di CR di CLIPS è quella in profondità.

### Incertezza

- CLIPS non ha funzionalità built-in per la gestione del **ragionamento incerto**.
    - È possibile però incorporare incertezza usando una strategia simile a quella usata in `MYCIN`.
        - Dove le regole basate su condizioni forniva una percentuale di una data infezione.
        - Le regole potrebbero equivalere a probabilità condizionate del tipo $P(H \mid E_1 \cap \dots \cap E_n) = 0.X$.
            - Ma i **certainty factors** non equivalgono a probabilità.
        - `MYCIN` opera nel contesto medico.
            - I medici non ragionano in termini probabilistici, ma per gradi di *belief* e *disbelief*.
            - Questi gradi non sono necessariamente consistenti in termini probabilistici.
            - Non si assume infatti che $P(not \: H \mid E_1 \cap \dots \cap E_n) = 1 - 0.X$ (ipotesi complementare).
                - È vero che $E_1, \dots, E_n$ hanno una relazione causa-effetto con $H$.
                - Non è necessariamente vero che $E_1, \dots, E_n$ hanno una relazione causa-effetto con $not \: H$.
            - $E_1, \dots, E_n$ devono essere intese come **evidenze a favore** di $H$.
                - Senza però concludere nulla circa $not \: H$.
    - I CF permettono una forma di **ragionamento inesatto** quando forme di ragionamento (probabilistico) esatto sono impraticabili per **mancanza di conoscenza**.

#### Certainty factors

- **Certainty factors** [Carnap, 1950]:
    - Alternativa alle probabilità per catturare un grado di conferma (*degree of confirmation* o *epistemic probability*).
    - Un CF **conferma un'ipotesi** data una qualche **evidenza**.
    - In `MYCIN` un CF è stato definito come la differenza tra *belief* e *disbelief*.
        - $CF(H, E) = MB(H, E) - MD(H, E)$.
            - $CF$: il fattore di certezza nell'ipotesi $H$ data l'evidenza $E$;
            - $MB$: **misura di crescita della credenza** (*belief*) in $H$ dato $E$.
            - $MD$: **misura di riduzione della credenza** in $H$ data $E$.
        - Un risultato positivo indica che il grado di **credenza a favore** dell'ipotesi cresce $0.X$ con l'evidenza $E$.
            - Un risultato negativo indica che il grado di **credenza a sfavore** dell'ipotesi cresce $0.X$ con l'evidenza $E$.
  - `def` **Certainty factors**: il grado *netto* di credenza sulla verità di un'ipotesi basata su qualche evidenza.
    - È un valore nell'intervallo $[-1, 0, +1]$ (il sistema di riferimento può variare).
        - $1$: il fatto è noto essere vero per certo;
        - $0$: non ci sono abbastanza elementi per affermare che il fatto sia vero o falso (**incertezza massima**);
        - $-1$: il fatto è noto essere faslo.
    - Un CF combina quindi *belief* e *disbelief* in un unico valore.
        - Comporta almeno due vantaggi:
            - Possibile ragionare in **assenza di informazione completa**.
            - Ranking delle ipotesi *disbelief*.

#### Simulare MYCIN in CLIPS

- Simulare `MYCIN` in CLIPS:
    - `MYCIN` rappresenta fatti come una tripletta `object-attribute-value` (OAV).

```lisp
(defmodule OAV (export deftemplate oav))
(deftemplate OAV::oav
    (multislot object (type SYMBOL))
    (multislot attribute (type SYMBOL))
    (multislot value)
    (slot CF (type FLOAT) (range -1.0 +1.0)))
```

- `MYCIN` consente di derivare la stessa tripla OAV da path di ragionamento distinti.
    - E quindi **combinare i corrispondenti fattori di certezza**.
    - CLIPS per default non consente di avere fatti identici.
        - Due OAV con CF diversi sono ammessi, ma non due OAV con medesimo CF.
        - Si può disabilitare con `(set-fact-duplication TRUE)`.
    - Dati due OAV identici nei loro primi tre slot si ottiene un nuovo OAV con:
        - $NewCertainty = (CF_1 + CF_2) - (CF_1 * CF_2)$.
            - I fattori di certezza vengono *cumulati*, vengono **sintetizzati** in un unico valore.
        - Se entrambi negativi, $NewCertainty = (CF_1 + CF_2) + (CF_1 * CF_2)$.
        - Se di segno opposto, $NewCertainty = \frac{CF_1 + CF_2}{1 - \min \{|CF_1|, |CF_2|\}}$.
    - L'auto-focus garantisce che due OAV identici saranno uniti prima di essere utilizzati in altre regole.
- Una regola è attivata da fatti (LHS), ciascuno con il proprio CF.
    - La regola stessa potrebbe essere associata ad un CF.
        - In `MYCIN` ogni asserzione ha un CF *a priori*.
    - A seconda del conditional element usato nell'antecedente:
        - $CF(P1 \lor P2) = \max \{CF(P1), CF(P2)\}$.
        - $CF(P1 \land P2) = \min \{CF(P1), CF(P2)\}$.
        - $CF(\lnot P) = - CF(P)$.
    - `MYCIN` aggiunge un ulteriore vincolo:
        - Se il CF risultante è inferiore a $0.2$, la regola viene considera inapplicabile.
        - La regola è infatti troppo poco probabile.
