---
title: "Basi di dati - Parte III - Basi di dati non relazionali"
---

# Basi di dati - Parte III

## Basi di dati non relazionali

### Scalabilità

- **Scalabilità verticale**: aumentare la potenza di calcolo di una singola macchina migliorando le varie componenti HW.
- **Scalabilità orizzontale**: aumentare la potenza di calcolo aggiungendo nuove macchine non necessariamente più potenti.
    - È mediamente più conveniente della scalabilità verticale.
- I database hanno una limitata scalabilità orizzontale.

### NoSQL

- Negli ultimi anni sono emersi approcci **NoSQL** (Not only SQL).
    - La maggior parte sono database distribuiti.
    - Con attenzione su dati semi-strutturati, alte prestazioni, disponibilità, replicazione e scalabilità.
    - Sono utilizzati per social media, marketing, mappe, email).
- È più corretto chiamare questi **approcci non relazionali**.

#### Vantaggi di NoSQL

- NoSql evita:
    - Il costo delle proprietà ACID (di cui non fornisce i vantaggi);
    - Il costo delle query SQL (di cui non fornisce la potenza);
    - L'onere della progettazione a priori dello schema (non offre correttezza dei dati), le transazioni (da gestire a livello applicativo).
- NoSql permette:
    - Cambiamenti facili e frequenti al DB;
    - Sviluppo veloce, gestione di grandi quantità di dati;
    - Database senza schema.
- NoSql è utile quando:
    - Serve un sistema distribuito e scalabile;
    - Il modello relazione è troppo restrittivo (schemi flessibili o dati con struttura complessa);
    - Le proprietà ACIDE non sono necessarie.
        - Gli aggiornamenti vengono *prima o poi* eseguiti.
        - Permette quindi letture inconsistenti.
- NoSql è adatto per:
    - Log di dati da fonti distribuite;
    - Dati temporanei (carrelli, wishlist, dati di sessione).
- NoSql non è adatto per:
    - Dati finanziari;
    - Dati aziendali critici.

#### Quando utilizzare NoSql

- Vantaggi di NoSQL:
    - Alte prestazioni;
    - Riduzione dei tempi di sviluppo;
        - Modello dati più vicino alle applicazioni (**impedance mismatch**).
    - Supporto alla scalabilità orizzontale e alta disponibilità.
- Svantaggi di NoSQL:
    - Nessun supporto per join e transazioni;
    - Grande ridondanza dei dati;
    - Mancanza di un linguaggio di query standard;
    - Mancanza di vincoli di integrità (gestiti a livello applicativo).

#### DBMS NoSQL

- I database NoSQL possono essere raggruppati secondo il modello di dati:
    - **Chiave-valore**: associa ogni chiave a un valore come per gli array associativi o hash table;
        - Utile per fare cache in memoria principale per applicazioni web.
        - `eg` Memcached, Voldemort, Redis, Riak, Amazon Dynamo.
    - **Documento**: un documento consiste in un ID associato a valori di vari tipi come per esempio hash;
        - Può contenre strutture annidate.
        - `eg` MongoDB, CouchDB.
    - **Colonnario**: gestisce enormi volumi di dati su server diversi (nodi);
        - Grande scalabilità orizzontale.
        - `eg` Hbase, Cassandra, Hypertable, Google BigTable.
    - **A grafo**: dati altamente interconnessi e nodi e archi con proprietà.
        - `eg` Neo4J.
- Non esiste una linguaggio di query comune come il SQL.

#### MongoDB

- In MongoDB:
    - I dati sono memorizzati sotto forma di **documenti**.
        - JSON utilizzato per l'I/O delle query, BSON per la memorizzazione interna.
    - I documenti non devono aderire ad uno schema standard, ma possono contenere qualsiasi campo;
    - Per le operazioni di ricerca, si recuperano i documenti basandosi sul valore di un determinato campo;
    - Il DB è scalabile orizzontalmente, supportando partizionamento (*sharding*) dei dati in sistemi distribuiti;
    - Esistono funzionalità per aggregazione e analisi dei dati.
- Confronto a DB relazionali:
    - Un database è formato da tante tabelle che contengono righe;
    - Un database è formato da tante collezioni che contengono documenti;
- Esempio di query in MongoDB:
    - `db.scorse.aggregate([ {$match: {voto:30}}, {$group: {_id:"matricola", count: {$sum: 1}}}])`.
        - Seleziona solo i documenti che hanno voto $30$;
        - Raggruppa i documenti dello stage precedente per matricola sommando il valore $1$ per ogni documento nel gruppo.
    - Analogo SQL:
        - `SELECT matricola, count(*) FROM scores WHERE voto = 30 GROUP BY matricola`.
