---
title: "Basi di dati - Parte II - Normalizzazione"
---

# Basi di Dati - Parte II

## Normalizzazione

- La **normalizzazione** consiste nella *decomposizione* di uno schema di relazione in modo da ottenere più schemi che rispettino una forma normale e minimizzino le anomalie.
- La normalizzazione è desiderabile in un sistema OLTP per evitare ridondanze e anomalie.
    - Mentre in un sistema OLAP (intensivo dal punto di vista di letture/interrogazioni) non è così desiderabile.
    - Le anomalie di aggiornamento, inserimento e cancellazione infatti si presentano di rado.
        - In quanto i dati non vengono modificati spesso.
    - Se fosse denormalizzato, le ridondanze permetterebbero di aumentare le performannce in lettura e interrogazione.
        - In quanto si eseguirebbero meno join.

### Decomposizione che conservano le dipendenze

- Si considera una relazione $r(A)$ e una sua decomposizione in $r_1(A_1) = \pi_{A_1}(r(A))$ e $r_2(A_2) = \pi_{A_2}(r(A))$.
    - Con $A_1 \subseteq A, A_2 \subseteq A$ e $A_1 \cup A_2 = A$.
-  La differenza insiemistica $r_1(A_1) \bowtie r_2(A_2) - r(A)$ consiste nell'insieme di **tuple spurie** ottenute dalla decomposizione.
    - Se la differenza è vuota si ottiene la definizione di **join senza perdita di informazione**.
- Dato uno schema di relazione $R(A)$ e due sottoinsiemi di attributi $A_1 \subseteq A, A_2 \subseteq A$, con$A_1 \cup A_2 = A$:
    - $\{R_1(A_1), R_2(A_2)\}$ è una **decomposizione senza perdita di informazione** sse **per ogni istanza** $r(A)$ di $R(A)$ vale:
        - $r(A) = r_1(A_1) \bowtie r_2(A_2)$ dove $r_1(A_1) = \pi_{A_1}(r(A))$ e $r_2(A_2) = \pi_{A_2}(r(A))$.
- `th` La decomposizione è **senza perdita di informazione** per ogni istanza che soddisfa le d.f. $F$ sse:
    - $A_1 \cap A_2$ è superchiave di $A_1$ **o** ($\lor$) $A_1 \cap A_2$ è superchiave di $A_2$.
    - Con $R(A)$ schema con d.f. $F$ decomposto in $\{R_1(A_1), R_2(A_2)\}$.
    - $A_1 \cap A_2$ sono gli attributi usati dal natural join per ricomporre $R$.
        - Se sono una superchiave individuano una sola tupla di $R_1$ (o di $R_2$).
    - Nella dimostrazione si vuole verificare che una qualunque tupla $t$ di $r_1(A_1) \bowtie r_2(A_2)$ è contenuta in $r(A)$.
- `th` La decomposizione è **senza perdita di informazione** per ogni istanza che soddisfa le d.f. $F$ sse:
    - $A_1 \subseteq (A_1 \cap A_2)_{F}^{+} \lor A_2 \subseteq (A_1 \cap A_2)_{F}^{+}$.
    - Si considera quindi la definizione di superchiave per chiusura.
    - Con $R(A)$ schema con d.f. $F$ decomposto in $\{R_1(A_1), R_2(A_2)\}$.

#### Restrizioni

- `def` **Restrizione**: $F_i = \{X \rightarrow Y \mid (X \rightarrow Y \in F^+) \land (X, Y \subseteq A_i)\}$
    - Restrizione $F_i$ di $F$ su $R_i$.
    - Definito dato uno schema $R(A)$ su cui valgono le d.f. $F$ e una decomposizione $R_i(A_i)$ di $R(A)$.
    - È una riscrizione delle d.f. $F$ in modo che a ogni relazione sia associata il proprio insieme di d.f. che discendono dalla relazione di partenza.
    - Si lavora su $F^+$ e non su $F$ per ottenere risultati corretti.
- `prop` La decomposizione $\{R_1, R_2\}$ **conserva le dipendenze** quando $F_1 \cup F_2 \vdash F$.
    - Data una relazione $R(A)$ con d.f. $F$, decomposta in $R_1(A_1)$ con la restrizione $F_1$ e $R_2(A_2)$ con la restrizione $F_2$.
    - Esistono casi in cui questa proprietà non vale.
        - Se una d.f. non deducibile andasse persa nel database potrebbe formarsi tuple spurie.
        - Le dipendenze perse nella decomposizione andrebbero ri-aggiunte come **vincoli globali** (che coinvolgono più relazioni).
        - Ma la verifica dei vincoli globali è poco praticabile perché costosa (un join per ogni tupla modificata).

### Le forme normali

- Le **forme normali** sono dei target per una buona progettazione di basi dati.
    - Sono volte alla minimizzazione delle ridondanze e delle anomalie.
    - A una forma normale può essere associato un **algoritmo di normalizzazione**.
- Esistono varie forme normali (con più o meno compromessi):
    - 1NF:  le relazioni hanno valori con domini semplici (non composti o con ripetizioni);
    - 2NF, **3NF**, **BCNF**: basate sulle dipendenze funzionali;
    - 4NF: introdotta sui vincoli di dipendenza funzionale multi-valore;
    - 5NF: basata su vincoli di decomposizione con join senza perdita di dipendenze;
    - DKNF: basata su un ragionamento su chiavi e domini.

#### Boyce-Codd Normal Form (BCNF)

- `def` **Boyce-Codd Normal Form**: la relazione $R(A)$ in $1NF$ è in **BCNF** sse per ogni $X \rightarrow Y \in F$ si verifica **almeno una** delle seguenti condizioni:
    1. $Y \subseteq X$ ($X \rightarrow Y$ dipendenza **riflessiva**);
    2. $X$ è **superchiave** di $R$.
    - Una relazione deve contenere un unico concetto ancorché identificato con due chiavi diverse.
    - Escludendo le d.f. triviali (riflessive) la BCNF ammette solo d.f che **dipendono da superchiavi**.
- La BCNF evita ridondanze (e quindi anomalie di aggiornamento) ma evita inoltre anomalie di inserimento e di cancellazione.
    - Ogni d.f. introduce, per definizione, una ripetizione.
    - Le ripetizioni sono evitabili se possono essere dedotte da altre informazioni.
    - Per questo si lavora solo su superchiavi.
- L'idea dietro l'algoritmo di normalizzazione è:
    1. Per ogni dipendenza $X \rightarrow Y$ in $R$ che vìola la BCNF;
    2. Si decompone $R$ definendo una nuova relazione su $R_X(XY)$ (che ha chiave $X$ ed è quindi BCNF);
    3. Si elimina $Y$ dalla relazione originaria (e quindi anche la d.f che non è BCNF).
- Esistono schemi che violano la BCNF e per cui non esiste alcuna decomposizione in BCNF che conservi le dipendenze.
    - Perciò l'applicazione della normalizzazione in BCNF **non è sempre praticabile**.

#### Terza forma normale (3NF)

- Rispetto alla BCNF, la **terza forma normale** (3NF) è meno restrittiva.
    - Ma **non elimina tutte le anomalie** (accettabile).
    - Ed è sempre possibile da raggiungere **conservando le dipendenze funzionali** (importante).
- `def` **Attributi primi**: gli attributi $Y \subseteq A$ sono detti **attributi primi** sse $Y \subseteq K$.
    - Dove $K$ è una chiave della relazione $R(A)$.
- `def` **Terza forma normale**: la relazione $R(A)$ in $1NF$ è in **3NF** sse per ogni $X \rightarrow Y \in F$ si verifica **almeno una** delle seguenti condizioni:
    1. $Y \subseteq X$ ($X \rightarrow Y$ è **riflessiva**);
    2. $X$ è **superchiave**;
    3. $Y$ sono **attributi primi**.
- Se una relazione $R(A)$ è in BCNF, allora è anche in 3FN.
    - L'inverso non è necessariamente vero.
- Una relazione in 3NF può avere **anomalie di inserimento e di cancellazione**.
    - Storicamente la BCNF è stata definita successivamente per rimediare alle anomalie della 3NF.
    - Solitamente si tratta di anomalie limitate e tollerate.

#### Insieme di copertura minimale

- `def` **Attributo estraneo**: un attributo in una d.f. in $F$ è **estraneo** sse si può rimuovere l'attributo dalla d.f. continuando ad avere un insieme di d.f equivalente.
    - Si può verificare se un attributo è estraneo calcolando la chiusura degli attributi:
        - L'attributo $B \in X$ è estraneo in $X \rightarrow Y$ sse $Y \in (X - B)^+_F$.
        - Dato un insieme di d.f. $F$ e una $X \rightarrow Y \in F$ (assumendo $Y$ sia un attributo).
- `def` **Dipendenza ridondante**: una dipendenza funzionale è **ridondante** in un insieme di d.f. $F$ sse si può rimuoverla da $F$ continuando ad avere un insieme di d.f. equivalente.
    - $X \rightarrow Y \in F$ è ridondante (assumendo $Y$ sia un attributo) sse $Y \in X^+_{F - \{X \rightarrow Y \}}$.
- `def` **Insieme di copertura minimale**: Un insieme di d.f. $F'$ è un **insieme di copertura minimale** rispetto a $F$ quando:
    1. $F' \equiv F$ (equivalente);
    2. In ogni $X \rightarrow Y\ \in F'$, $Y$ è un attributo singolo (*forma canonica*);
    3. Ogni $X \rightarrow Y\ \in F'$ è priva di attributi estranei;
    4. Ogni $X \rightarrow Y\ \in F'$ non è ridondante;
    - Il passo 1 specifica che $F'$ è **copertura** di $F$, le altre che $F'$ è **minimale**.
    - Il passo 2 non è un requisito necessario ma semplifica la trattazione.

##### Algoritmo per il calcolo di un insieme di copertura minimale

- Algoritmo per il calcolo di un insieme di copertura minimale:
    - $F' := F$;
    - *Passo 1*: **per ogni** d.f $X \rightarrow A_1 ... A_n \in F'$:
        - Sostituire in $F'$ la d.f. $X \rightarrow A_1 ... A_n$ con $X \rightarrow A_1, \dots, X \rightarrow A_n$.
    - *Passo 2*: **per ogni** d.f. $X \rightarrow A_i \in F'$, **per ogni** $B_j \in X$:
        - Se $A_i \in (X - B_j)^+_F$ allora cancellare $B_j$ da $X$ e aggiornare $F'$.
    - *Passo 3*: **per ogni** $X \rightarrow A_i \in F'$:
        - $F^* := F' - (X \rightarrow A_i)$;
        - Se $A_i \in X^+_{F^*}$ allora $F' := F^*$.
    - `return` $F'$.
- Commento all'algoritmo:
    - Il passo 1 porta le d.f in *forma canonica*;
    - Il passo 2 elimina gli **attributi estranei**;
    - Il passo 3 elimina le **dipendenze ridondanti**.
    - Gli attributi estranei vanno **obbligatoriamente** eliminati prima delle dipendenze ridondanti (i due passi non sono invertibili).
    - L'algoritmo genera coperture minimali **non uniche**, che sono però tra loro comunque equivalenti.
        - Considerare gli attributi e le d.f. in ordine diverso può portare a eliminare attributi estranei e d.f. ridondanti diversi.
    - La complessità dell'algoritmo è **polinomiale**.

### Normalizzazione in 3NF

- Algoritmo per la normalizzazione in 3NF, data una relazione $(R(A), F)$:
    1. Si calcola la copertura minimale $F'$ di $F$.
    2. **Per ogni** insieme di d.f. ${X \rightarrow A_1, \dots, X \rightarrow A_n} \subseteq F'$ che contiene tutte le d.f. che hanno a sinistra gli stessi attributi $X$:
        - Si crea la relazione $(R_X(XA_1 ... A_n), \{X \rightarrow A_1 ... A_n\})$.
    3. **Per ogni** coppia di relazioni $R_X(XY), R_{X'}(X'Y')$ in cui $XY \supseteq X'Y'$:
        - Si elimina la relazione $R_{X'}$ e aggiungi le d.f. di $R_{X'}$ a quelle di $R_X$.
    4. **Se** nessuna relazione contiene una chiave $K$ qualsiasi di $R(A)$:
        - Si trova $K$ tale che $K^+ = A$ e crea una nuova relazione $R_K(K)$.

#### Proprietà della normalizzazione 3NF

- La normalizzazione 3NF:
    - Nelle relazioni $R_i(XA_1 ... A_n)$ generate dalle d.f. $X \rightarrow A_i$, $X$ è chiave di $R_i$;
    - Genera relazioni in 3NF;
    - Ha complessità polinomiale;
    - Conserva le dipendenze, infatti si trova ogni dipendenza di $F'$ all'interno della relazione corrispondente;
    - Garantisce la decomposizione con join senza perdita.
- La normalizzazione 3NF spesso genera schemi BCNF (più forti).
    - L'unico passo che può generare schemi non BCNF è il passo 3.
    - Gli schemi prodotti dalla normalizzazione in 3NF che sono anche in BCNF sono quindi quelli in cui non sono state generate dipendenze di tipo 3.
    - Nelle d.f. di tipo 3 le anomalie persistono (in quelle di tipo BCNF no).
        - `eg` In $AGENZIE$ possono essere introdotte anomalie dalla d.f. $Direttore \rightarrow CittaAgenzia$.
- Conviene quindi normalizzare sempre in 3NF per via dei vantaggi rispetto alla normalizzazione in BCNF:
    - Conservazione delle dipendenze;
    - Complessità polinomiale.

### Entity-Relationship

- Nel modello Entity-Relationship vengono implicitamente rappresentate le dipendenze funzionali identificate dal progettista.
    - Si può quindi analizzare quali d.f. sono sottointese negli schemi concettuali.
- ER produce **relazioni BCNF**, che però non conservano le dipendenze.
    - Queste vengono quindi espresse come regole aziendali che nella traduzione in relazionale coinvolgono relazioni diverse.
    - Queste regole possono causare degradi in performance e si può valutare se rinunciare alla BCNF al posto della 3NF.
- Confrontando le dipendenze funzionali identificate dal progettista con quelle generabili dallo schema ER si può verificare la qualità di entrambi.

#### Costrutti del modello Entity-Relationsip

- `def` **Entità**: $R_E(\underline{I},A)$.
    - Entità $E$ con identificatore $I$ e attributo $A$.
    - Esprime la dipendenza funzionale $I \rightarrow A$.
- `def` **Associazione molti a molti**: $R_{E_1}(\underline{I_1}, A_1), R_{E_2}(\underline{I_2}, A_2), R_A(\underline{I_1,I_2},B)$.
    - Da $E_1$ si ha $I_1 \rightarrow A_1$ e da $E_2$ si ha $I_2 \rightarrow A_2$.
    - Da $A$ si ha che ogni occorrenza di $A$ (e quindi dell'attributo $B$) è individuata da una coppia di occorrenze di $E_1$ e di $E_2$.
        - Si ha quindi $I_1, I_2 \rightarrow B$.
- `def` **Associazione uno a molti**: $R_{E_1}(\underline{I_1},A_1,B,I_2), R_{E_2}(\underline{I_2}, A_2)$.
    - Da $E_1$ si ha $I_1 \rightarrow A_1$ e da $E_2$ si ha $I_2 \rightarrow A_2$.
    - Dall'associazione $A$ si ha che ogni occorrenza di $A$ (e quindi del suo attributo $B$) è individuata da una coppia di occorrenze di $E_1$ e di $E_2$.
        - Si ha quindi $I_1, I_2 \rightarrow B$.
    - A causa della cardinalità $(1, 1)$, ogni occorrenza di $E_1$ è associata tramite $A$ a un'unica occorrenza di $E_2$.
        - Si ha quindi $I_1 \rightarrow B, I_2$.
    - Si calcola l'insieme di copertura minimale e si raggruppa per antecedente.
        - Rimangono così le d.f. $I_1 \rightarrow A_1, B, I_2$ e $I_2 \rightarrow A_2$.
- `def` **Identificazione esterna**: $R_{E_1}(\underline{I_1},A_1), R_{E_2}(\underline{I_2},A_2), R_{E_3}(\underline{I_3,I_1,I_2},A_3)$.
    - Da $E_1$ si ha $I_1 \rightarrow A_1$ e da $E_2$ si ha $I_2 \rightarrow A_2$.
    - Da $E_3$ si ha che, considerando l'identificazione esterna, preso un determinato valore di $I_3$ abbinato a una coppia di occorrenze di $E_1$ e di $E_2$, si trova un determinato valore di $A_3$.
        - Si ha quindi $I_3, I_1, I_2 \rightarrow A_3$.
- **Generalizzazione**: non si considera esplicitamente le gerarchie perché si può assumere che vengano ristrutturate in uno dei costrutti dell'ER.
