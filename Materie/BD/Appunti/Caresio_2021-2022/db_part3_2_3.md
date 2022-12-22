---
title: "Basi di dati - Parte III - Gestione delle concorrenza"
---

# Basi di dati - Parte III

## Gestione della concorrenza

### Transazioni contemporanee

- Le transizioni ricevute **contemporaneamente** devono godere della proprietà di **isolamento**
- Per il DBMS l'ordine di esecuzione delle transizioni è **irrilevante**, a patto che lasci in stato di consistenza la base di dati.
    - Il DBMS infatti esegue le transizione nell'ordine che ritiene più opportuno.
    - Se per il sistema informativo fosse importante l'ordine di esecuzione delle due, attenderebbe il termine della prima.
        - Oppure le includerebbe in un'unica transazione (alcuni DBMS supportano transazioni annidate).

#### Serializzabilità

- L'esecuzione in parallelo di due transazioni richiede di **interfogliare** (*interleave*) le attività delle transazioni.
    - Non tutti gli interfogliamenti lasciano però il database in uno stato consistente.
    - **Storia**: specifico interfogliamento, è la sequenza di azioni eseguite dal DBMS a fronte delle richieste da parte delle transazioni.
    - **Criterio di serializzabilità**: una storia $S$ è corretta se è equivalente a una qualsiasi storia seriale delle transazioni coinvolte da $S$.
        - Con $n$ transizioni esistono $n!$ storie seriali.
- Un DBMS potrebbe scoprire che una storia non è serializzabile.
    - Si introducono quindi degli strumenti per evitare la non serializzabilità.
    - Si definisce un **protocollo** che garantisce la serializzabilità **a priori**.
        - Un insieme di regole che prevengono che venga generata una storia non serializzabile.

### Lock

- Si valuta quindi il metodo basato sui **lock** (il più diffuso).
- Il gestore della concorrenza dispone di comandi per richiedere l'autorizzazione a compiere azioni sull'oggetto $X$:
    - `LS(X)`: **lock shared** sull'oggetto $X$ (`eg` una tupla) da richiedere prima di una lettura;
    - `LX(X)`: **lock exclusive** sull'oggetto $X$ da richiedere prima di una scrittura;
    - `UN(X)`: **unlock** sull'oggetto $X$.

#### Lock shared ed exclusive

- **Lock shared**: transazioni diverse possono acquisire un lock condiviso sul medesimo oggetto.
    - Una transazione prima di effettuare la lettura deve avere acquisito almeno il lock shared sull'oggetto.
        - Acquisisce ovvero il permesso per leggere l'oggetto $X$.
    - Il DBMS può quindi concedere il lock shared sullo stesso oggetto contemporaneamente a più transazioni.
- **Lock exclusive**: richiede un accesso esclusivo all'oggetto $X$ da parte di una transazione.
    - La richiesta di lock exclusive è di norma fatta da una transazione per **modificare** un oggetto.
    - Quando una transazione ha acquisito l'autorizzazione esclusiva a scrivere l'oggetto, nessun altra transazione può acquisire lock sullo stesso oggetto.
        - Né lock shared né lock esclusivi.

#### Gestione del lock

- Quando il DBMS non può concedere il lock la transazione va in **wait**.
    - Il DBMS tiene traccia dei lock concessi con una **tabella dei lock**.
        - Per gestire la compresenza di più lock utilizza la **tabella di compatibilità**.
        - È possibile aggiornare il lock da LS a LX se una stessa stransazione ne fa richiesta ed è l'unica che possiede il LS.

| possesso / richiesta | LS      | LX   |
| -                    | -       | -    |
| LS                   | Concede | Nega |
| LX                   | Nega    | Nega |

#### Lock a due fasi

- Il lock non permette però di costruire storie serializzabili.
    - Per avere benefici dal meccanismo del lock occorre un passo ulteriore.
    - Si impone alle transazioni un vincolo nell'utilizzo degli unlock.
- **Lock a due fasi** (2PL): politica dove si ha una **fase di acquisizione dei lock** seguita da una **fase di rilascio dei lock**.
    - Quando una transazione inizia la fase di rilascio **non può più acquisirne**.
    - Si ha una fase crescente, dove i lock vengono acquisiti, e una decrescente, dove vengono rilasciati.
- Esistono storie serializzabili che non sono possibili con lock a due fasi.
    - Permette due fasi di interfogliamento consistenti nelle letture, ma i lock exclusive sono bloccanti.
    - In SQL è possibile abbassare il livello di isolamento tra le transazioni.
        - Si accettano possibili anomalie in cambio di maggiore parallelismo.

#### Granularità del lock

- Nei DBMS i lock disponibili sono disponibili su:
    - Tuple (soluzione più diffusa);
    - Attributi (più complessi, maggiore parallelismo);
    - Pagine (per i sistemi distribuiti, diminuiscono il parallelismo);
    - Interi file (per la gestione degli indici).
