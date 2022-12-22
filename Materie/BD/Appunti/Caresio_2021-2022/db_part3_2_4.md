---
title: "Basi di dati - Parte III - Gestione del ripristino"
---

# Basi di dati - Parte III

## Gestione del ripristino

- Il **gestore del ripristino** deve garantire i dati a fronte di possibili **guasti** e **anomalie**.
- Guasti **soft** (comuni):
    - *Crash di sistema*: guasto hardware (no storage), software o di rete durante l'esecuzione di una transazione;
    - *Errori di programma o di sistema*: una transazione fallisce per un errore (`div by 0`), per un bug o perché l'utente la interrompe;
    - *Eccezioni gestite dalla transazione*: non sono errori perché gestiti dal programmatore.
    - *Rollback di transazioni forzate dal gestore della transazione e dal serializzatore*: violazione della serializzabilità o per uscire da un deadlock;
- Guasti **hard** (meno comuni).
    - *Guasti delle periferiche di storage*: possono avvenire anche durante operazioni di lettura o scrittura.
    - *Eventi catastrofici*: furti, incendi, allagamenti, etc.
- Il gestore del ripristino è la componente del DBMS che garantisce **atomicità** e **durabilità**:
    - **Atomicità**: se una transazione fallisce durante l'esecuzione il GdR annulla eventuali modifiche apportate al DB.
    - **Durabilità**: se una transazione è *committed* i suoi effetti devono essere conservati stabilmente in memoria secondaria.
- Il gestore del ripristino gestisce l'affidabilità a fronte di errori tranne nel caso di guasti hard.
- Idea di base nella gestione dei guasti:
    - **Annullare** le operazioni memorizzate su storage ma **non commited**;
    - **Ripristinare** le operazioni non memorizzate su storage che sono **committed**.

### File di log

- Il gestore del ripristino mantiene un **file di log**.
    - È un **file sequenziale append-only** su storage (rischio solo guasti hard), di cui viene periodicamente fatto il backup.
    - Rende possibili effettuare *undo* e *redo* delle operazioni delle transazioni.
    - Per ogni transazione si tiene traccia dei comandi `start transaction`, `commit transaction` e `abort transaction`.
    - Per ogni operazione (`insert`, `delete`, `update`) effettuata sul DB si tiene traccia della quadrupla:
        1. Identificatore della transazione;
        2. L'oggetto $X$ soggetto dell'operazione;
        3. **Before state**, lo stato precedente di $X$ (tranne nel caso di insert);
        4. **After state**, lo stato successivo di $X$ (tranne nel caso di delete).
    - Si memorizzano anche i `read` in caso di auditing.

#### Undo e Redo

- È quindi possibile usare il log per fare **undo** e **redo** di una **singola azione** (`insert`, `delete`, `update`):
    - `UNDO`: per `update` e `delete` $X = BS$, per `insert` si elimina $X$.
    - `REDO`: per `insert` e `update` $X = AS$, per `delete` si elimina $X$.
- Anche il processo di ripristino potrebbe fallire e quindi dover essere rieseguito in seguito.
    - In questo caso bisogna avere lo stesso risultato che si avrebbe se il processo non fosse mai fallito.
    - Le due operazioni devono essere **idempotenti**: $undo(undo(X))=undo(X)$ e $redo(redo(X))=redo(X)$.

#### Gestione del buffer in memoria

- Tutte le operazioni su un DB vengono effettuate sui buffer in **memoria principale** e poi scritte su **memoria secondaria**.
    - Compresi i file di log.
    - Questo ha **conseguenze sulla gestione del ripristino**, possono infatti avvenire crash prima della scrittura su storage.
- Si definiscono quindi due regole per decidere quando scrivere il log su storage:
    - **Write-Ahead log**: il **before state** dei record di log deve essere scritto **prima dei corrispondenti record** nel DB.
        - Permette `UNDO`.
    - **Commit-Precedenza**: l'**after state** dei record di log deve essere scritto **prima di effettuare il commit**.
        - permette `REDO`.
- Un guasto che si verifica prima del commit impone al gestore del ripristino l'undo delle azioni effettuate.
    - Un guasto che si verifica dopo il commit impone il redo delle azioni effettuate.

### Rollback

- Quando una transazione richiede il **commit** il DBMS sceglie in modo atomico e indivisibile tra abort e commit.
    - E scrive sul log in modo **sincrono il record di commit**.
- Una transazione può fallire quando:
    - Richiede un **rollback**;
    - Il DBMS impone una **abort**;
    - La transazione richiede un **commit** e almeno un vincolo non è soddisfatto.
-  In questi casi Il DBMS esegue un **rollback** della transazione:
    - Esegue un'azione `UNDO(T)` che annulla tutte le azioni precedentemente compiute dalla transazione $T$.
    - Memorizza nel log il record `<T, abort>` seguito dalla direttiva `FORCE LOG` per forzare la scrittura del log su storage.
- Il ripristino in seguito a un crash avviene (in generale) con due operazioni:
    - `UNDO(AT)`: **annullamento** delle transazioni non terminate per garantire le proprietà di atomicità.
        - Esplorando il log **a ritroso** riportando gli oggetti delle transazioni in $AT$ al loro valore nel **before state**.
        - Modificando nel buffer la pagina e poi forzando la scrittura su storage.
        - $AT$ è l'insieme delle transazioni attive (non terminate).
    - `REDO(CT)`: **ripristino** delle transazioni committed.
        - Esplorando il log in avanti riporta la base dati allo stato dell'ultima modifica.
        - Modificando nel buffer la pagina usando $X = AS(X)$ e poi forzando la scrittura su storage.
        - $CT$ è l'insieme delle transazioni che hanno raggiunto il commit.
-  Il **REDO** deve essere obbligatoriamente eseguito **dopo l'UNDO**.
- Il processo di ripristino può ignorare le transazioni abortite perché già persistenti in storage.

### Checkpoint

- Il **checkpoint** garantisce che il processo di ripristino possa considerare solo una **parte dei file di log** (utile per grossi file).
    - Periodicamente (10 o 15 minuti) avviene un processo di **checkpoint** che aggiunge un record al file di log:
        1. Si **sospendono tutte** le transazioni;
        2. Si costruisce il record di checkpoint contenente l'elenco delle transazioni che in quel momento sono **attive** col relativo **puntatore** alla posizione dello start nel file di log;
        3. Si esegue un `FORCE LOG`;
        4. Si esegue un `FORCE` delle pagine delle transazioni committed;
        5. Si aggiunge un flag **OK** nel record di checkpoint e si esegue un nuovo `FORCE LOG`;
        6. Si riavviano le transazioni sospese.
- Il **ripristino con checkpoint** considera l'ultimo checkpoint.
    - Infatti le transazioni che hanno raggiunto il commit **prima** del checkpoint non hanno bisogno di ripristino (con guasti soft).
- Algoritmo di **ripresa a caldo** (*hot restart*):
    1. Trova l'ultimo checkpoint;
    2. Recupera la lista $AT$ della transazioni ancora **attive durante il crash**;
    3. Recupera la lista $CT$ delle transazioni che hanno raggiunto il commit **dopo l'ultimo checkpoint**;
    4. `UNDO(AT)`
    5. `REDO(CT)`.

### Guasti hard

- I guasti hard richiedono l'utilizzo di memorie secondarie considerate **stabili**, che però non saranno mai esenti da guasti.
    - È quindi possibile utilizzare la **duplicazione** (su nastri magnetici, memorie offline, etc) per rendere il sistema più robusto.
- Si possono eseguire periodicamente delle copie complete (**dump**) della base di dati (dopo di che il log viene svuotato).
    - Essendo il file di log solitamente più piccolo del DB può essere memorizzato direttamente in memoria stabile.
- In fasi di carico basso del sistema si possono rendere persistenti le modifiche delle transazioni commited, annullare le altre e azzerare il log.
- Dopo un guasto hard si effettua una **ripresa a freddo** usando i backup:
    1. Si ripristinano i dati a partire dai backup (**restore**);
    2. Si legge **tutto il log** e si effettua il `REDO(CT)`;
    3. Si effettua la ripresa a caldo.
