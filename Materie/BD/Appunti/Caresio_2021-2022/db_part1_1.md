---
title: "Basi di dati - Parte I - Il modello relazionale"
---

# Basi di dati - Parte I

## Il modello relazionale

### Modello dei dati

- Il **modello relazione** è il **modello dei dati** adottato dai database relazionali.
- In ogni base di dati esistono:
    - **Schema**: ne descrive la struttura, sostanzialmente invariante nel tempo;
        - Pertinente all'aspetto **intensionale**.
    - **Istanza**: i valori attuali, che possono cambiare anche molto rapidamente.
        - Pertinente all'aspetto **estensionale**.
- Esistono due tipi principali di modelli:
    - Modelli **concettuali**;
    - Modelli **logici**.
- Nelle basi di dati si lavora con la **Closed-World Assumption**.
    - Si assume che i fatti contenuti nella base di dati siano una descrizione **completa** del mondo di interesse.
    - I soli fatti veri nel mondo sono quelli presenti nella base di dati.

#### Modelli concettuali

- I **modelli concettuali** permettono di descrive la base di dati in modo astratto.
    - Non sono disponibili nei DBMS commerciali.
    - Permettono di rappresentare i dati in modo indipendenti da ogni sistema.
    - Sono utilizzati nelle fasi preliminari di progettazioni.
- Il più diffuso modello concettuale è l'**Entity-relationship**.

#### Modelli logici

- I **modelli logici** sono adottati nei DBMS esistenti per l'organizzazione dei dati.
    - Sono indipendenti dalle strutture fisiche.
- Esempi storici di modelli logici:
    - **Relazionale**;
    - Reticolare;
    - Gerarchico;
    - A oggetti;
    - Basato su XML;
    - NoSQL (document-based, colonnari, graph-based, RDF, etc).
- Un DBMS ha una specifica **rappresentazione interna** che differisce dallo schema logico.
    - Ma ogni DBMS espone verso l'utente lo stesso schema logico, quello relazionale.
    - Il **livello logico** è **indipendente** dal **livello fisico**.

### Modello relazionale

- Il **modello relazionale** impone ai dati una struttura rigida:
    - Le informazioni sono rappresentate per mezzo di **tuple**.
    - Nella tabella a ogni dominio si associa un nome unico, un **attributo**.
    - Solo alcuni formati di tuple sono ammessi: quelli che corrispondono agli schemi di relazione.
- Il modello relazionale non ammette **strutture nidificate**.
    - Si possono introdurre altre relazioni per rappresentare strutture nidificate.
- Il modello relazionale è basato su **valori**.

#### Tabelle e relazioni

- In una tabella che rappresenta una relazione:
    - L'ordinamento tra le righe è irrilevante;
    - L'ordinamento tra le colonne è irrilevante.
- A differenza delle relazioni matematiche, due tabelle con le stesse colonne ma ordinate differentemente rappresentano la **stessa relazione**.

#### Definizioni relative al modello relazione

- `def` **Schema di relazione**: $R(A_1, \dots, A_N)$ un nome $R$ con un insieme di attributi $A_1, \dots, A_n$.
    - `eg` $Studenti(Matricola, \: Nome, \: Cognome, \: DataNascita)$.
- `def` **Schema di base di dati**: $R = \{R_1(X_1), \dots, R_k(X_k)\}$, insieme di schemi di relazione.
- `def` **Tupla**: su un insieme di attributi $(A_1, \dots, A_n)$ associa a ciascun attributo $A_i$ un valore del dominio di $A_i$.
    - $t[A_i]$ denota il valore della tupla $t$ sull'attributo $A_i$.
- `def` **Istanza di relazione**: l'insieme $r$ di tuple su $X$ su uno schema di relazione $R(X)$ dove $X$ sono attributi.
- `def` **Istanza di base di dati**: l'insieme di relazioni $\{r_1, \dots, r_h\}$ (con $r_i$ relazione su $R_i$) su uno schema di base di dati $R$.

#### Valori nulli

- Il modello relazionale adotta il **valore nullo**.
    - Denota l'assenza di un valore del dominio (ma non è un valore del dominio).
    - Si devono imporre **restrizioni** sulla presenza di tali valori nulli.
    - Esistono tre casi differenti per adottarlo: valore *sconosciuto*, *inesistente*, *senza informazione*.
        - I DBMS relazionali non distinguono tra questi tre tipi di valore nullo.
- Per la corretta gestione dei valori nulli si introducono dei **vincoli**.

#### Vincoli

- **Vincolo di integrità**: proprietà che deve essere soddisfatta dalle istanze che rappresentano informazioni corrette per l'applicazione.
    - È un **predicato logico** che associa a ogni istanza un valore di verità.
    - Se un vincolo non risulta **vero**, il cambiamento viene rifiutato.
- Esistono varie tipologie di vincoli:
    - Vincoli **intrarelazionali**: sui valori (o di **dominio**), di tupla;
    - Vincoli **interrelazionali**.

##### Vincoli di integrità

- **Vincoli di tupla**: esprimono condizioni sui valori di ciascuna tupla, indipendentemente dalle altre.
    - `eg` `Lordo = Ritenute + Netto`.
- **Vincoli di dominio**: coinvolgono un solo attributo.
- **Vincoli di chiave**: la **superchiave** è un insieme di attributi usato per **identificare univocamente le tuple di una relazione**.
    - Una chiave (candidata) deve essere una **superchiave minimale**.
    - Una **chiave primaria** è una particolare chiave scelta come identificatore univoco di un insieme di tuple, e **non può assumere valori nulli**.
- **Vincoli di integrità referenziale**: garantisce la correttezza dei riferimenti tra tabelle.
    - In caso di violazione: rifiuto dell'operazione (standard), eliminazione in cascata o introduzione di valori nulli.
    - Ogni vincolo di integrità referenziale ha un *verso*.
    - Possono interessare più attributi (più stringenti).
