---
title: "Basi di dati - Parte III - Gestione del buffer"
---

# Basi di dati - Parte III

## Gestione del buffer

### DBMS e memorizzazione

- La *persistenza* delle basi di dati richiede di memorizzare i dati in memoria secondaria.
    - E la loro *grandezza* che tale gestione sia sofisticata.
- Memoria Secondaria $\rightarrow$ Cache $\rightarrow$ Buffer DBMS.
    - Postgres usa un doppio buffer, quello del SO e uno interno.
- Tutte i dati del database sono **organizzati in pagine**.
    - Le dimensioni di queste dipendono dal sistema.
    - I **record** sono contenuti nelle pagine.
    - Se una transizione ha bisogno di lavorare su un determinato record (tupla), il gestore cerca la pagina contenente il record:
        - Quando riceve la richiesta di una pagina caricherà la pagina nel buffer stesso prendendola dalla periferica di storage se non è già presente nel buffer.
        - Tempi di trasferimento di pagine da storage a RAM $10^{-3}$, mentre in RAM $10^{-9}$.
        - Si valutano quindi soprattutto gli accessi in memoria secondaria (`eg` negli algoritmi per il calcolo del join).
- Il DBMS utilizza le funzionalità del file system per creare e eliminare file e per leggere e scrivere singoli blocchi o sequenze di blocchi contigui.

#### Blocking factor

- `def` **Blocking Factor**: numero di record in un blocco $\lfloor L_B / L_R \rfloor$.
    - $L_B$: dimensione di un blocco;
    - $L_R$: dimensione media di un record;
    - Se $L_B > L_R$ si può avere più record in un blocco, con spazio residuo utilizzabile per altre relazioni (record *spanned*) o non essere utilizzato (*unspanned*).

### Ottimizzazione fisica

- L'ottimizzatore fisico prende in input un albero di parsificazione modificato dall'ottimizzatore logico e corrispondente a un'interrogazione DML.
    - Si basa innanzitutto sui **metodi di accesso**, cioè deve scegliere come accedere ai dati.
    - I DBMS possono presentare più modalità di accesso ai dati che dipendono dai **metodi usati per organizzare le tuple nei file**.

### Strutture primarie per l'organizzazione dei file

- I record nei file possono essere organizzati come:
    - File di record **non ordinati** o a **heap** (struttura **seriale**);
    - File di record **ordinati** (struttura **sequenziale**);
    - Strutture ad accesso **calcolato** o a **hash**.

#### Record a heap

- I **record a heap** (struttura seriale, o *entry sequenced*, file non ordinato):
    - Le tuple vengono inserite nei blocchi nell'ordine presentato al sistema.
    - È **molto diffusa** nelle basi di dati relazioni, spesso associata a strutture **secondarie** di accesso come indici.
    - Gli **inserimenti** vengono effettuati in modo molto efficienti: in coda oppure al posto di record cancellati.
    - La **cancellazione** lascia spazio inutilizzato nei blocchi e richiede un compattamento periodico.
- Per quanto riguarda la **ricerca**, l'ottimizzatore fisico deve valutare **a priori** il costo dell'uso di questo metodo per confrontarlo con altri.
    - Senza strutture secondarie il DBMS non può fare altro che leggere le pagine una dopo l'altra.
- Una ricerca con insuccesso richiederà quindi di leggere tutte le pagine, mentre per una con successo in media richiederà di leggere metà delle pagine.
    - `def` **Costo medio di accesso seriale**: $1 \cdot \frac{1}{N} + 2 \cdot \frac{1}{N}  + ... + N \cdot \frac{1}{N} = \sum_{i=1}^{N} i \frac{1}{N} = \frac{1}{N} \cdot \frac{N(N+1)}{2} = \frac{N+1}{2}$.
        - Si assume una **distribuzione uniforme** in cui la tupla ha la **stessa probabilità** $\frac{1}{N}$ di trovarsi in qualsiasi pagina.

#### Struttura ordinata

- Se invece di un heap si opta per un'**organizzazione ordinata** in cui i record nei file sono ordinati secondo un attributo *chiave*:
    - L'**inserimento** è costoso perché richiede di spostare pagine (in media metà delle pagine).
    - La **cancellazione** si può attuare contrassegnando un record come cancellato e attuando una riorganizzazione periodica.
    - La **ricerca con insuccesso** sull'attributo *chiave* ha lo stesso costo della ricerca con successo perché non è necessario scorrere tutte le pagine: $\frac{N+1}{2}$.
        - Le ricerche dicotomiche spesso non sono sono possibili perché i record non sono memorizzati in blocchi consecutivi.

#### Criticità delle strutture primarie

- Gli accessi a heap e a struttura ordinata hanno costi che sono dell'ordine del **numero delle pagine** (lineare).
    - Nel migliore dei casi metà delle pagine.
    - Questa organizzazione è troppo costosa per grossi sistemi informativi.
    - Si possono associare alle strutture primarie delle **strutture secondarie** più complesse.

### Strutture secondarie per l'organizzazione dei file

#### B-tree

- Un **B-tree** è una generalizzazione degli alberi binari di ricerca (**bilanciati**) in cui ogni nodo può avere $m$ figli (branching factor):
    - Ogni sottoalbero di *sinistra* di una chiave $k$ ha chiavi di ricerca strettamente *inferiori* alla chiave $k$;
    - Ogni sottoalbero di *destra* ha chiavi *strettamente* superiori a $k$.
- Un B-tree con branching factor $m$ deve rispettare le seguenti proprietà:
    1. **Ogni nodo** ha al massimo $m-1$ chiavi;
    2. **Ogni nodo** a eccezione della radice è **almeno mezzo pieno** (almeno $\lceil \frac{m}{2} - 1 \rceil$ chiavi);
    3. Se il B-tree non è vuoto, la radice ha almeno una chiave;
    4. Tutte le foglie sono allo stesso livello;
    5. Un nodo interno che ha $k$ chiavi ha $k + 1$ figli.
- `def` **Numero di livelli di un B-tree** ($L$): $log_{m}(N+1) \leq L \leq \log_{\lceil \frac{m}{2} \rceil}(\frac{N+1}{2})+1$.
    - Approssimazione: $\log_m(N) \leq L \leq \log_m(N) + 1$.
        - Dato che $L \simeq \log_{m}N$ si ha che il numero di chiavi memorizzabili è $N \simeq m^L$.
    - Assumendo che l'albero abbia $N$ chiavi totali e branching factor $m$.

#### B+-tree

- Nei **B+-tree** nei nodi interni sono memorizzate solo le chiavi di ricerca.
    - Mentre nelle foglie vengono memorizzate le chiavi di ricerca+RID (*record identifier*).
    - Per tentare di tenere in memoria principali i nodi usati più spesso (quelli interni) li si rende più leggeri.
    - Ogni chiave dei nodi interni è ricopiata in una foglia.
    - Le proprietà dei B-tree rimangono invariate.
- Le foglie del B+-tree possono contenere come data entry $k$:
    - `<K, tupla>`: il data entry è il record stesso;
        - Il B+-tree viene usato come struttura di memorizzazione primaria.
    - `<K, RID>`: il data entry è puntatore al record nell'area primaria;
    - `<K, ListaDiRID>`: il data entry è una lista di puntatori a record che hanno la stessa chiave $k$.
        - Necessario quando si crea un indice su un attributo che non è chiave relazionale.
- Inoltre le foglie sono tutte collegate tra di loro in modo da poter accedere in modo efficiente a sequenze di chiavi.
    - Utile per l'utilizzo di range di valori.
- Con un *B+-tree* di $L$ livelli per una ricerca puntuale nell'indice sono necessari $L + 1$ accessi.
- Nei sistemi informativi reali in genere gli indici hanno due o tre livelli.

### Indici

- L'**indice** è una struttura separata rispetto alle strutture di memorizzazione primaria.
    - Si possono costruire diversi indici sulla stessa struttura di memorizzazione primaria.
- Un amministratore DB crea un indice quando il carico di lavoro in termini di interrogazioni è tale per cui il costo di accesso all'indice riduce notevolmente i tempi di risposta rispetto alla scansione sequenziale.
- Gli indici rendono **più efficienti le interrogazioni**, ma appesantiscono le modifiche.
    - Gli inserimenti e le cancellazioni modificano gli indici.
    - Le modifiche ai valori possono modificare gli indici.
    - Non ha quindi senso definire indici su attributi che vengono cambiati molto spesso.
- Caratterizzazione degli indici:
    - Indice **primario**: definito sullo stesso **attributo chiave relazionale** su cui sono **ordinati fisicamente** i record;
    - Indice **clusterizzato**: definito su un **attributo non chiave relazionale** (con possibili valori ripetuti) su cui i record sono **ordinati fisicamente**;
    - Indici **secondari**: indici su un **attributo qualunque** su cui i record **non sono ordinati fisicamente**;
- Una tabella ha **al più un indice primario o clusterizzato** (non entrambi) e zero o più indici secondari.

#### Euristiche per la scelta di indici

- Euristiche per la scelta di indici:
    - **Evitare gli indici su tabelle di poche pagine**;
        - Probabilmente contenibile interamente nel buffer.
    - **Evitare indici su attributi volatili**;
        - Se l'attributo viene modificato più di quanto letto, il mantenimento dell'indice non è giustificato.
    - **Evitare indici su chiavi poco selettive**;
        - Non definire indici su attributi con pochi valori distinti.
        - La selettività $f_s$ di un indice su un attributo è data dal rapporto tra il numero di pagine restituite dalla ricerca di un valore dell'attributo e il numero di pagine nella tabella.
        - Si suggerisce di definire un indice su un attributo se $f_s < 20\%$.
    - **Evitare indici su chiavi con valori sbilanciati**;
        - Con un grosso squilibrio nella distribuzione dei valori di una chiave gli indici sulla chiave saranno poco efficienti.
        - Gli indici funzionano bene quando la distribuzione è uniforme.
    - **Limitare il numero di indici**;
        - Gli indici selettivi favoriscono le interrogazioni ma la loro manutenzione è costosa.
        - Si consiglia quindi di limitarsi al massimo a 4/5 indici per relazioni corpose
    - **Definire indici su chiavi relazionali ed esterne**:
        - Un indice su una chiave è sempre consigliato, come definire indici su chiavi esterne perché verranno usati dai join.
    - **Gli indici velocizzano le scansioni ordinate**;
        - Un indice è utile per reperire i record secondo l'ordine della chiave indicizzata.
        - Nei B+-tree (ordinati per definizione) sarà necessario semplicemente scansionerà sequenzialmente le foglie.
    - **Conoscere a fondo il DBMS**.
        - È l'ottimizzatore del DBMS a scegliere se utilizzare o meno un indice e ogni DBMS ha una propria strategia.
