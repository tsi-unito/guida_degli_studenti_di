---
title: 'Basi di dati - Laboratorio'
---

# Basi di Dati - Laboratorio

- Il ciclo di vita di una base di dati consiste di varie fasi:
    - **Studio di fattibilità**: si stabiliscono le possibili soluzioni alternative e le priorità in relazione ai vincoli imposti (costo, tempo, qualità, funzionalità).
    - **Raccolta e analisi dei requisiti**: si individuano le *proprietà* e le *funzionalità* che il sistema informativo dovrà avere, e i requisiti software e hardware (stretta interazione con gli esperti di dominio).
    - **Progettazione**: si costruisce un modello formale e dettagliato del sistema. Comprende progettazione dei dati e progettazione delle applicazioni.
    - **Implementazione**: si realizza il sistema informativo seguendo le specifiche del progetto, costruendo e popolando la base di dati e scrivendo il codice dei programmi.
    - **Validazione e collaudo**: si verifica il corretto funzionamento e la qualità del sistema informativo.
    - **Funzionamento**: messa in *produzione* del sistema realizzato, con successive manutenzioni e revisioni.
- Modello 3-tier (dall'alto):
    1. **User interface**: codice dedicato alla gestione dell'interfaccia utente.
    2. **Business**: codice dedicato alla gestione della logica applicativa.
    3. **Data**: codice dedicato all'interazione con il DBMS.
- Progettazione:
    - (Requisiti);
    - Progettazione concettuale (schema concettuale);
    - Progettazione logica (schema Logico);
    - Progettazione fisica (schema fisico).

-----

## Modello Entity-Relationship

- Fornisce costrutti per descrivere le specifiche sulla *struttura dei dati* in modo semplice e comprensibile, con un formalismo grafico e in modo indipendente dal modello logico, che potrà essere scelto in seguito. Non modella il comportamento sul sistema (come UML) ma *modella i dati*.
    - **Entità**: (graficamente un rettangolo) rappresentano aspetti del mondo reale con esistenza *autonoma* ai fini dell'applicazione di interesse. Un'occorrenza di entità non si riduce ai valori che la identificano, ma ha esistenza indipendente, ed è questa la differenza rispetto al modello relazionale.
    - **Associazioni**: (graficamente un rombo con linee per le associazioni) rappresentano legami logici tra due o più entità. È possibile avere associazioni diverse che coinvolgono le stesse entità, avere associazioni che coinvolgono più di due entità, o un'associazione tra un'entità e se stessa.
    - **Attributi**: descrivono le proprietà di entità o associazioni. Ogni attributo è caratterizzato dal suo *dominio*, l'insieme dei valori ammissibili per l'attributo. Gli *attributi composti* raggruppano attributi di una medesima entità o associazione che presentano affinità nel loro significato o suo.

### Cardinalità

- **Cardinalità delle associazioni**: vengono specificate una cardinalità *minima* e *massima* per ciascuna entità che partecipa a un'associazione.
    - Data un'occorrenza di entità, la cardinalità descrive il numero di occorrenze dell'associazione a cui l'occorrenza di entità può partecipare.
    - Le associazioni vengono classificate a seconda delle **cardinalità massime** (*molti a molti*, *uno a molti*, *uno a uno*).
    - Cardinalità più comuni:
	- Cardinalità minima:
		- $0$: la partecipazione dell'entità relativa è opzionale.
		- $1$: la partecipazione dell'entità relativa è obbligatoria.
	- Cardinalità massima:
		- $1$: l'associazione può avere una solo occorrenza dell'entità.
		- $n$: l'associazione può avere un numero arbitrario di occorrenze dell'entità.
- **Cardinalità degli attributi**: descrive il numero minimo e massimo di valori dell'attributo associati a ogni occorrenza di entità o associazione:
	- Cardinalità *minima* $0$: attributo opzionale.
	- Cardinalità *massima* $1$: attributo obbligatorio.
	- Cardinalità *massima* $n$: attributo multivalore.
- **Identificatore delle entità**: permettono di identificare **univocamente** le occorrenze di un entità, e sono costituiti da attributi dell'entità (**identificatore interno**) o da attributi dell'entità e entità esterne attraverso associazioni (**identificatore esterno**).
- Osservazioni:
    - Ogni entità deve avere **almeno un identificatore**, ma può anche averne diversi (a differenza rispetto al modello relazionale).
    - Ogni attributo che fa parte di un identificatore ha cardinalità $(1, 1)$.
    - Un'identificazione esterna è possibile solo attraverso associazioni a cui l'entità da identificare partecipa con cardinalità $(1, 1)$.
    - Un'identificazione esterna può coinvolgere entità a loro volta identificate esternamente purché non vengano generati cicli.
    - Le associazioni non hanno identificatori, un'occorrenza di un'associazione si distingue dalle altre unicamente tramite le occorrenze delle entità che vi partecipano.

### Generalizzazione

- **Generalizzazione**: mettere in relazione una o più entità $E_1, E_2, ..., E_n$ con una entità $E$, che le comprende come casi particolari:
    - $E$ è una *generalizzazione* di $E_1, E_2, ..., E_n$
    - $E_1, E_2, ..., E_n$ sono *specializzazioni* di $E$
    - Ogni occorrenza di $E_1, E_2, ..., E_n$ è anche un'occorrenza di $E$, e ogni proprietà di $E$ (attributi, associazioni, altre generalizzazioni) è anche una proprietà di $E_1, E_2, ..., E_n$ per ereditarietà (non rappresentato esplicitamente).
- Generalizzazione **totale/parziale**: *totale* se ogni occorrenza dell'entità genitore è occorrenza di (almeno) una delle entità figlie, altrimenti è *parziale*
- Generalizzazione **esclusiva/sovrapposta**: *esclusiva* se ogni occorrenza dell'entità genitore è occorrenza di *al più una* delle entità figlie, altrimenti è *sovrapposta*
- Osservazioni:
    - Una generalizzazione sovrapposta può essere trasformata in una esclusiva aggiungendo entità figlie che rappresentano le *intersezioni*.
    - Possono esistere generalizzazioni a più livelli e multiple generalizzazioni allo stesso livello.
    - Un'entità può essere inclusa in più generalizzazioni, come genitore e/o come figlia.
    - Le generalizzazioni non possono avere cicli.
    - Se una generalizzazione ha solo un'entità figlia si parla di *sottoinsieme*.

### Documentazione associata agli schemi E-R

- Uno schema E-R non è quasi mai sufficiente a rappresentare tutti gli aspetti di una base di dati, ad esso vengono associati:
- Descrizione dei concetti:
    - Dizionario dei dati per le entità.
    - Dizionario dei dati per le associazioni.
- Vincoli non esprimibili in E-R (business rules):
    - Vincoli di integrità (*concetto* (non) deve *espressione*).
    - Vincoli di derivazione (*concetto* si ottiene *operazione*).

-----

## Progettazione concettuale

- La raccolta e analisi dei requisiti e la progettazione concettuale comprendono attività interconnesse di:
    - **Raccolta dei requisiti**: tramite interviste con utenti, documentazione o realizzazioni preesistenti. Scegliere il corretto livello di astrazione, standardizzare la struttura delle frasi ed evitarne di contorte, individuare sinonimi unificando i termini e rendendone esplicito il riferimento tra essi, infine costruire un glossario dei termini e riorganizzare le frasi per concetti. Raccogliere inoltre le possibili operazioni da effettuare sui dati.
    - **Analisi dei requisiti**.
    - **Costruzione del glossario**.
    - **Costruzione dello schema concettuale**.

### Progettazione concettuale

- **Entità**: se ha proprietà significative e descrive oggetti autonomi.
- **Attributo**: se è semplice e non ha proprietà.
- **Associazione**: se correla due o più concetti.
- **Generalizzazione**: se è un caso particolare di un altro.

#### Pattern di progettazione

- Pattern di progettazione:
    - Reificazione di attributo di entità.
    - Part-of.
    - Instance-of.
    - Reificazione di associazione binaria.
    - Reificazione di associazione ricorsiva.
    - Reificazione di attributo di associazione.
    - Caso particolare di entità.
    - Storicizzazione di un'entità.
    - Storicizzazione di un'associazione.
    - Evoluzione di concetto.
    - Reificazione di associazione ternaria.

#### Strategie di progetto

- Strategie di progetto:
    - **Top-down**: a partire dalle specifiche si individuano e specificano i concetti cardine creando la struttura dello schema e successivamente mediante trasformazioni lo si raffina descrivendo i vari concetti con maggiore dettaglio. Possibile però solo quando si possiede una visione globale di tutte le componenti.
    - **Bottom-up**: le specifiche sono suddivise in parti elementari, che vengono poi tradotte in semplici schemi concettuali e poi fusi fino a giungere a uno schema completo. Adatta a una progettazione di gruppi, ma l'integrazione di sistemi concettuali diversi può comportare difficoltà.
    - **Inside-out**: variante del BU, si individuano alcuni concetti importanti e poi da questi ci si muove a macchia d'olio, si rappresentano prima i concetti legati a quelli già definiti. È necessario continuamente riesaminare tutte le specifiche per individuare concetti non ancora rappresentati e descriverli nel dettaglio.
    - **Mista**: si individuano i concetti principali e si realizza uno *schema scheletro*: si organizza i concetti più importanti in un semplice schema concettuale, sulla base di questo si può decomporre per poi raffinare, espandere e integrare.

#### Qualità di uno schema concettuale

- Qualità di uno schema concettuale:
    - **Correttezza**: quando utilizza propriamente i costrutti messi a disposizione dal modello concettuale di riferimento, gli errori possono essere sintattici o semantici.
    - **Completezza**: quando modella tutte le specifiche.
    - **Leggibilità**: quando rappresenta i requisiti in modo naturale e facilmente comprensibile.
    - **Minimalità**: quando tutte le specifiche sono rappresentate una sola volta nello schema.

## Progettazione logica

- Lo schema concettuale viene *tradotto* in uno schema logico che rappresenti gli stessi dati in maniera corretta ed efficiente, ma la traduzione non è immediata, alcuni aspetti non sono infatti direttamente rappresentabili ed è necessario considerare le prestazioni.
    - È suddivisibile in due fasi:
        - **Ristrutturazione dello schema concettuale** (EER): lo schema EER viene nuovamente analizzato per evidenziare ed eliminare *inefficienze*.
        - **Traduzione verso il modello logico e ottimizzazioni**: lo schema EER viene algoritmicamente tradotto in relazionale, nella fase di traduzione è possibile applicare ottimizzazioni (come la normalizzazione).

### Ristrutturazione dello schema concettuale

- Indicatori di prestazione:
    - **Tempo**: numero di occorrenze di entità e di associazioni visitate per eseguire un'operazione sul DB.
    - **Spazio**: spazio di memoria necessario per rappresentare i dati.
    - Per poterli valutare è necessario conoscere:
        - Il **volume dei dati** (numero di occorrenze, dimensione degli attributi).
        - Le **caratteristiche delle operazioni** (operazione interattiva/batch, frequenza, entità associazioni coinvolte).
    - Così da poter ottenere quindi la **tabella dei volumi** (concetto, tipo, volume) e la **tabella delle operazioni** (operazione, descrizione, tipo, frequenza).

#### Procedura per la ristrutturazione

- Procedura per la ristrutturazione:
    1. **Analisi delle ridondanze**: si decide se eliminare o aggiungere ridondanze (informazione significativa ma derivabile) presenti nello schema. Si ottengono interrogazioni/letture semplificate e più efficienti, ma inserimenti/modifiche meno efficienti. Per ogni ridondanza:
        - Per ogni operazione significativa su cui la presenza/assenza della ridondanza può avere effetto:
            - Schema di operazione in presenza e assenza di ridondanza.
            - Tavola degli accessi in presenza e assenza di ridondanza.
        - Confronto in spazio e tempo tra presenza e assenza di ridondanza.
        - Scelta se introdurre o non introdurre la ridondanza con motivazione.
    2. **Eliminazione delle generalizzazioni**: tutte le generalizzazioni presenti vengono analizzate e sostituite con entità, associazioni e regole aziendali. Le tre possibili casistiche:
        1. Accorpamento dei figli nel genitore (utile se gli accessi non fanno distinzioni tra i figli).
        2. Accorpamento del genitore nei figli (possibile sse totale, utile se gli accessi alle entità figlie sono distinti).
        3. Sostituzione della generalizzazione con associazioni (utile se gli accessi ai figli sono separati dagli accessi al genitore).
    3. **Partizionamento/accorpamenti di entità e associazioni**: si decide se è opportuno partizionare o accorpare concetti dello schema in unico concetto. Gli accessi si riducono:
        - Separando attributi di uno stesso concetto ai quali si accede in operazioni diverse.
        - Accorpando attributi di concetti diversi a cui si accede con le medesime operazioni (tipicamente entità con associazioni 1 a 1).
    4. **Scelta degli identificatori principali**: si sceglie un identificatore per le entità che ne hanno più d'uno. Criteri:
        - Assenza di opzionalità.
        - Semplicità.
        - Utilizzo nelle operazioni più frequenti.
    5. **Eliminazione degli attributi multivalore**: gli attributi multivalore, non rappresentabili direttamente in relazionale, possono essere reificati in una nuova entità associata a quella originale;
    6. **Eliminazione degli attributi composti**: gli attributi composti, anche essi non rappresentabili direttamente in relazione, devono essere trasformati.

### Traduzione verso il modello relazionale

- Traduzione verso il modello relazionale:
    - Le **entità** diventano relazioni con gli stessi attributi delle entità.
    - Le **associazioni** diventano relazioni con attributi delle associazioni + gli identificatori delle entità coinvolte.
    - **Associazioni molti a molti**: la traduzione non riesce a tener conto delle cardinalità minime delle associazioni MaM (se non con `SQL CHECK`).
    - **Associazioni uno a molti**: la traduzione riesce a rappresentare efficacemente la cardinalità minima della partecipazione che ha 1 come cardinalità massima: 0 come valore nullo ammesso, 1 come valore nullo non ammesso.
    - **Entità con identificazione esterna**: l'identificazione esterna è sempre su un'associazione uno a molti o uno a uno.

-----

## SQL

### Gestione valori nulli

- `coalesce` accetta un elenco di parametri e restituisce il primo parametro non nullo.
    - O `NULL` se sono tutti nulli.
    - Utile per interpretare i valori nulli come $0$ (`eg`, `coealesce(status, 0)`).

### Raggruppamento

- `SQL-99` permette di avere di avere nella *target list*, al di fuori delle funzioni aggregate, attributi la cui chiave primaria è tra gli **attributi discriminanti**.
    - Supportato da postgres, MySql (recentemente) ma non Oracle.

### Query nidificate

- Esistono due tipi di **sottoquery**:
    - **Semplici**: o stratificate, è possibile valutare prima l’interrogazione più interna (una volta per tutte), poi, sulla base del suo risultato, valutare l’interrogazione più esterna.
    - **Correlate**: o incrociate, l’interrogazione più interna fa riferimento a una delle tabelle appartenenti all’interrogazione più esterna.
        - Per ciascuna riga candidata alla selezione nell’interrogazione più esterna, è necessario valutare nuovamente la sottoquery.
        - Una sottoquery nella clausola `FROM` non può essere correlata.
- Una sottoquery nella clausola `SELECT` deve restituire esattamente una riga con un valore.
    - Quasi sempre sarà una sottoquery correlata.

#### Quantificatori

- In caso di presenza di valori nulli, l'utilizzo del quantificatore `ALL` produrrà un risultato vuoto.
- L'operatore di uguaglianza non viene quasi mai usato con il quantificatore `ALL`.

#### Sottoquery correlate

- Nelle sottointerrogazioni correlate si può usare il costrutto `EXISTS` e `NOT EXISTS`:
    - `EXISTS`: la riga in esame nella query più esterna soddisfa il predicato `EXISTS` se la query annidata non restituisce l’insieme vuoto.
    - `NOT EXISTS`: la riga in esame nella query più esterna soddisfa il predicato `NOT EXISTS` se la query annidata restituisce l’insieme vuoto.

### Costrutti avanzati

#### Windows functions

- Le **window functions** sono simili alle funzioni aggregate, compiono calcoli su insiemi di righe
    - Mentre le funzioni aggregate fanno sì che ogni gruppo di righe diventi una sola riga di output, le window functions mantengono le righe separate.
    - `eg` Per ogni fornitura riportiamo la quantità totale fornita per ogni fornitore:
        - `SELECT *, sum(Qty) OVER (PARTITION BY SNum) FROM SP`.
