---
title: "Basi di dati - Parte I - Algebra relazionale"
---

# Basi di dati - Parte I - Algebra relazionale

## Algebra relazionale

- [Edgar Codd](https://w.wiki/4SSq) formalizza l'interrogazione utilizzando l'**algebra relazionale**.
    - Fornisce un fondamento teorico ai linguaggi di interrogazione delle basi di dati.
    - L'algebra è una costruzione procedurale dell'interrogazione (un elenco dei passi).
    - Ogni passo è definito astrattamente attraverso gli **operatori algebrici**.
- Per verificare che un espressione algebrica sia **sintatticamente corretta** bisogna verificare che gli operatori algebrici siano **coerenti con gli argomenti**.
    - È possibile scrivere qualsiasi combinazione di operatori, purché sintatticamente corretta.

### Operatori dell'algebra relazionale

- Ogni operatore dell'algebra riceve come **argomento una relazione** e **produce una relazione** (detta virtuale).
- Operatori di base:
    - Selezione: $\sigma_p$;
    - Proiezione: $\pi_a$;
    - Prodotto cartesiano: $\times$;
    - Unione: $\cup$;
    - Differenza: $-$;
    - Ridenominazione: $\rho_{B \leftarrow A}$.
- Operatori derivati:
    - Intersezione: $\cap$;
    - Join nelle sue varie forme (theta, equi, natural, etc): $\bowtie$.
    - Quoziente: $\div$.
- L'algebra relazione è **composizionale**.
    - È possibile scrivere qualsiasi combinazione di operatori.
    - Purché sia **sintatticamente corretta**, ovvero che gli operatori algebrici siano coerenti con gli argomenti.

#### Predicati

- Il **predicato** $p$ è un espressione booleana formata componendo predicati atomici di due tipi:
    - $A_i \: \varphi$ costante;
    - $A_i  \: \varphi  \: A_j$.
    - Dove:
        - $A_i$ e $A_j$ sono attributi in $A$;
        - $\varphi$ è un **operatore di confronto** nell'insieme $\{ =,\neq, <, \leq, >, \geq \}$.

#### Operatore di selezione

- Data una relazione $r(A)$, l'**operatore di selezione** $\sigma_p(r(A))$ produce come risultato una relazione con:
    - Schema: A;
    - Istanza: le tuple della relazione $r$ che soddisfano il predicato $p$.
- `def` **Cardinalità della selezione**: $0 \leq | \sigma_p(r(A)) | \leq | r(A) |$.

#### Operatore di proiezione

- Data una relazione $r(A)$ e un insieme di attributi $A_i, A_j, \dots A_k \in A$, l'**operatore di proiezione** $\pi_{A_{i},A_{j}, \dots, A_{k}}(r(A))$ produce come risultato una relazione con:
    - Schema: $\{A_i, A_j, \dots,  A_k \}$;
    - Istanza: tutte le tuple della relazione argomento, ma solo rispetto agli attributi $A_i, A_j, \dots,  A_k$.
- `def` **Cardinalità della proiezione**: $0 \leq | \pi_{A_{i},A_{j}, \dots ,A_{k}}(r(A))| \leq |r(A)|$.
    - Trattandosi di insiemi, non sono possibili ripetizioni, ecco perché $\leq$.
    - Mentre se gli attributi proiettati formano una **superchiave** allora $| \pi_{A_{i},A_{j}, \dots ,A_{k}}(r(A))| = |r(A)|$.

#### Operatori insiemistici

- Gli **operatori insiemistici** richiedono per definizione che gli schemi delle relazioni argomento siano identici.
    - Il risultato sulle relazioni argomento $r_1(A)$ e $r_2(A)$ è una relazione con:
        - Schema: lo stesso schema $A$ delle relazioni argomento.
        - Istanza:
            - Unione: $r_1 \cup r_2$ (unione delle tuple di $r_1$ e $r_2$);
            - Differenza: $r_1 - r_2$ (tuple contenute in $r_1$ ma non in $r_2$);
            - Intersezione: $r_1 \cap r_2$ (tuple di $r_1$ contenute anche in $r_2$).
- `def` **Cardinalità degli operatori insiemistici**:
    - Unione: $max \{|r_1(A)|, |r_2(A)| \} \leq |r_1(A) \cup r_2(A)| \leq |r_1(A)| + |r_2(A)|$;
    - Differenza: $0 \leq |r_1(A) - r_2(A)| \leq |r_1(A)|$;
    - Intersezione: $0 \leq |r_1(A) \cap r_2(A)| \leq \text{min} \{ |r_1(A)|, |r_2(A)| \}$.
- L'unione, contrariamente alla differenza, gode della **proprietà commutativa**.
- L'intersezione è un **operatore derivato** dall'operatore di differenza insiemistica, infatti $r_1(A) \cap r_2(A) := r_1(A) - (r_1(A) - r_2(A))$.

#### Operatore di ridenominazione

- Data una relazione $r(A)$ come argomento, l'**operatore di ridenominazione** cambia nome a una parte o a tutti gli attributi della relazione argomento.
    - Data una relazione $r(a)$ l'operatore di ridenominazione $\rho_{B_i,B_j, \dots ,B_k \leftarrow A_i,A_j, \dots ,A_k}(r)$ produce una **nuova** (non modifica l'originale) relazione virtuale $r'(A')$ con:
        - Schema: $A' = \{A_1, \dots ,B_i, \dots ,B_j, \dots ,B_k, \dots ,A_n \}$;
        - Istanza: $r' = r$ (stesse tuple).

##### Ridenominazione dell'intero schema

- Data una relazione $r$ con schema $R(A)$, $A=\{A_1, \dots, A_i, \dots, A_j, \dots, A_k, \dots, A_n\}$, la **ridenominazione dello schema** $\rho_{R'(B_i,B_j, \dots, B_k) \leftarrow R(A_i,A_j, \dots, A_k)}(R)$ produce una relazione virtuale con:
    - Schema: $R'(A_1, \dots, B_i, \dots, B_j, \dots, B_k, \dots, A_n)$ in cui gli attributi non ridenominati rimangono invariati.
- L'operatore di ridenominazione appesantisce la lettura delle espressioni algebriche.
    - Quando si hanno attributi con lo stesso nome in relazioni diverse si può disambiguare il nome dell'attributo specificando la relazione a cui appartiene utilizzando la **dot-notation**.

#### Prodotto cartesiano

- Date due relazioni $r_1(A)$ e $r_2(B)$ con $A \cap B = \emptyset$ (i due schemi non hanno attributi in comune) il **prodotto cartesiano** $r_1(A) \times r_2(B)$ produce come risultato una relazione $r'$ con:
    - Schema: $R'$ composto dall'unione degli schemi $A \cup B$;
    - Istanza: combinazione di tutte le tuple di $r_1(A)$ con tutte le tuple di $r_2(B)$.
- `def` **Cardinalità del prodotto cartesiano**: $0 \leq |r_1(A) \times r_2(B)| = |r_1(A)|\cdot|r_2(B)|$.
- Il prodotto cartesiano gode della **proprietà commutativa**, $r_1(A) \times r_2(B) = r_2(B) \times r_1(A)$.
    - Secondo Codd non è importante né ordine attributi né ordine tuple.
    - Ma è un operatore *tecnico*, utile nella definizione di altri importanti operatori.

#### Operatore di theta-join

- L'**operatore di theta-join** costruisce informazioni estratte da più relazioni.
    - $r_1(A) \bowtie_\theta r_2(B)$, è definito con:
        - Due relazioni $r_1(A)$ e $r_2(B)$ con $A \cap B = \emptyset$ (i due schemi non hanno attributi in comune, **disgiunti**);
        - Una condizione (predicato) $\theta$ di join (tipicamente una formula proposizionale con **confronti tra attributi** del tipo $A_i \: \varphi \: B_j$ o confronti tra **attributi e valori** $A_i \: \varphi \: costante$ dove $\{ =,\neq, <, \leq >, \geq \}$).
    - Il theta-join è un **operatore derivato** definito come $r_1(A) \bowtie_\theta r_2(B) := \sigma_\theta(r_1(A) \times r_2(B))$.
        - La sua cardinalità sarà quindi determinata da quella della selezione e da quella del prodotto cartesiano.
- La **cardinalità** di $r_1(A) \bowtie_\theta r_2(B)$ ha un intervallo ampio, si calcola considerando la cardinalità di $\sigma_\theta(r_1(A) \times r_2(B))$:
    - $0 \leq |\sigma_\theta(r_1(A) \times r_2(B))| \leq |r_1(A) \times r_2(B)|$.
    - Ma si sa che $|r_1(A) \times r_2(B)| = |r_1(A)|\cdot|r_2(B)|$.
    - `def` **Cardinalità del theta-join**: $0 \leq | r_1(A) \bowtie_\theta r_2(B)| \leq |r_1(A)|\cdot|r_2(B)|$.
- In algebra relazione non sono presenti **operatori di conteggio**.
    - Ma si può mettere in **theta-join** la stessa relazione (rinominando i termini) per ottenere il conteggio **per almeno uno**.

#### Operatore di equi-join

- L'operatore di **equi-join** è un caso particolare del theta-join in cui i confronti sono **solo uguaglianze**.
    - Date $r_1(A)$ e $r_2(B)$ la condizione di join $\theta_e$ è da intendersi come una **congiunzione di uguaglianze** del tipo $A_i = B_j$ o $A_i = \text{costante}$.
        - $r_1(A) \bowtie_{\theta_e} r_2(B)$;
        - $\theta_e = A_{i1} \land B_{j1} \land A_{i2} = B_{j2} \land  \dots  \land A_{in} = B_{jn}$.
- La **cardinalità** può essere distinta in base al numero di tuple di $r_2$ che si trovano per ogni tupla di $r_1$:
    - **Al più una** tupla in $r_2$: $0 \leq | r_1(A) \bowtie_{\theta_e} r_2(B)| \leq | r_1(A)|$;
    - **Esattamente una** tupla in $r_2$: $|r_1(A) \bowtie_{\theta_e} r_2(B)| = | r_1(A)|$.

#### Operatore di natural join

- Date due relazioni $r_1(A)$ e $r_2(B)$ con $A = X \cup Y$ e $B = X \cup Z$ ($X$, $Y$ e $Z$ disgiunti), il **natural join** è definito come:
    - $r_1(A) \bowtie r_2(B) := \pi_{X,Y,Z}(r_1 \bowtie_{\theta_e} \rho_{X' \leftarrow X}(r_2))$;
    - $\theta_e := (X_1 = X'_1) \land \dots \land (X_k = X'_k)$.
    - I vari schemi **non sono insiemi disgiunti**.
    - Il natural join mette quindi in uguaglianza **tutti gli attributi omonimi**.
        - Questo introduce delle criticità da valutare.
- Date due relazioni $r_1$ e $r_2$ definite sullo stesso schema, il loro natural join:
    - Corrisponde a un equi-join su tutti gli attributi, prendendo le tuple uguali su tutti gli attributi in $r_1$ e $r_2$.
    - $r_1(A) \: \bowtie \: r_2(A) = r_1(A) \cap r_2(A)$.

#### Quantificazione universale

- L'algebra relazionale non gestisce direttamente la **quantificazione universale**.
    - Ma ci si può ricondurre alla quantificazione esistenziale: $\forall t (p(t)) \equiv \lnot \exists t (\lnot p(t))$.
    - `eg` Un valore è maggiore o uguale a tutti i valori sse non esiste un valore di cui è minore.

#### Operatore quoziente o divisione

- Date due relazioni $r(A, B)$ e $s(B)$ con $A$ e $B$ disgiunti, si ottiene:
    - `def` **Operatore quoziente** (o divisione): $r(A, B) \div s(B) = \pi_A(r) - \pi_A((\pi_A(r) \times s) - r)$.
    - Produce una relazione $u(A)$ che contiene le tuple che in $r(A, B)$ compaiono in combinazione con **ogni tupla** in $s(B)$.
- Scomponendo la definizione dell'operatore quoziente:
    - $\pi_A(r) \times s$: ha schema $(A, B)$ e dà tutte le combinazioni possibili;
    - $(\pi_A(r) \times s) - r$: ricava le combinazioni *non* presenti in $r$;
    - $\pi_A(r) - \pi_A((\pi_A(r) \times s) - r)$: dà tutte le *altre* tuple di $r$, cioè quelle che compaiono in ogni combinazione.
- `def` **Cardinalità del quoziente**: $0 \leq | r(A,B) \div s(B)| \leq | \pi_a(r)| \leq |r|$.
- Non ha un corrispettivo diretto in SQL, ma si può esprimere usando sottoquery correlate.

#### Valore nullo e logica a tre valori

- Codd propose di usare una **logica a tre valore**, `False`, `True` e `Unknown` (U, Sconosciuto):
    1. $v_i \: \varphi \: v_j$ (assenza di valori nulli);
    2. $v_i \: \varphi \: NULL = U$;
    3. $NULL \: \varphi \: v_j = U$;
    4. $NULL \:  \varphi \: NULL = U$.

##### Operatori e logica a tre valori

| $\lor$ | $F$ | $U$ | $T$ |   | $\land$ | $F$ | $U$ | $T$ |   | $\lnot$ |     |
| -      | -   | -   | -   | - | -       | -   | -   | -   | - | -       | -   |
| $F$    | $F$ | $U$ | $T$ |   | $F$     | $F$ | $F$ | $F$ |   | $F$     | $T$ |
| $U$    | $U$ | $U$ | $T$ |   | $U$     | $F$ | $U$ | $U$ |   | $U$     | $U$ |
| $T$    | $T$ | $T$ | $T$ |   | $T$     | $F$ | $U$ | $T$ |   | $T$     | $F$ |

- Si può arimetizzare il calcolo assegnando $F=0$, $U=1$ e $T=2$:
    - $\land$: minimo tra i valori;
    - $\lor$: massimo tra i valori;
    - $\lnot p = 2 - p$.
- La selezione dà come risultato tutte le tuple per cui il valore di verità è $T$, escludendo quelle per cui è $F$ o $U$.
    - Per poter cercare una tupla che **ha o non ha un valore nullo** si scrive un predicato con le clausole:
        - $A_i$ `IS NULL` ($T$ se $A_i$ è `NULL`, $F$ altrimenti);
        - $A_i$ `IS NOT NULL` ($T$ se $A_i$ non è `NULL`, $F$ altrimenti).
    - Nessuna delle due clausole è mai valutata $U$.
- La logica a tre valori **vìola** alcuni enunciati della logica classica (non contraddizione, terzo escluso).

#### Inner Join e Outer Join

- I join visti precedentemente sono tutti **join interni**: fanno parte del risultato solo le combinazioni di tuple per cui la $\theta$ è vera.
    - In alcuni casi si vuole avere la garanzia di non tralasciare informazioni, includendo anche le tuple di una relazione anche quando non c'è un corrispettivo nell'altra relazione.
- Si introducono quindi i **join esterni** (molto usati nella pratica), di cui si distinguono tre tipi di **outer join**:
    - **Left join** ($\unicode{10197}$): quando una tupla della relazione di sinistra non fa join con nessuna tupla di destra, si inseriscono valori nulli in corrispondenza degli attributi della **seconda** relazione;
    - **Right join** ($\unicode{10198}$): quando una tupla della relazione di destra non fa join con nessuna tupla di sinistra, si inseriscono valori nulli in corrispondenza degli attributi della **prima** relazione;
    - **Full join** ($\unicode{10199}$): è l'unione del left e del right join.

### Gruppi di proprietà

#### Proprietà commutative

- `def` **Proprietà commutativa del prodotto cartesiano**:
    - $r(A) \times s(B) = s(B) \times r(A)$;
- `def` **Proprietà commutativa del theta-join**:
    - $r(A) \bowtie_{\theta} s(B) = s(B) \bowtie_{\theta} r(A)$.
    - Il theta-join è un prodotto cartesiano a cui si applica una selezione.

#### Proprietà associative

- `def` **Proprietà associativa del prodotto cartesiano**:
    - $(r(A) \times s(B)) \times u(C) = r(A) \times (s(B) \times u(C))$.
    - Permette di scrivere il prodotto cartesiano tra tre relazioni senza parentesi.
- `def` **Proprietà associativa (ristretta) del theta-join**:
    - $r(A) \bowtie_{\theta(A,B \cup C)}(s(B,C) \bowtie_{\theta(B \cup C, D \cup E)} u(D,E))$;
    - $= (r(A) \bowtie_{\theta(A,B \cup C)}s(B,C)) \bowtie_{\theta(B \cup C, D \cup E)} u(D,E)$.
    - Possibile solo se è possibile suddividere i predicati del theta-join come indicato.

#### Proprietà della selezione multipla

- `def` **Proprietà della selezione multipla**: 
    - $\sigma_{p \land q}(r(A)) = \sigma_p(\sigma_q(r(A)))$.

#### Proprietà della sostituzione di operatori

- `def` **Proprietà della sostituzione di operatori**:
    - $\sigma_{p \land q} (r(A)) = \sigma_p (r(A)) \cap \sigma_q(r(A)))$;
    - $\sigma_{p \lor q} (r(A)) = \sigma_p (r(A)) \cup \sigma_q(r(A)))$;
    - $\sigma_{p \land \lnot q} (r(A)) = \sigma_p (r(A)) - \sigma_q(r(A)))$.
- Al DBMS costano molto meno le forme a sinistra.

#### Proprietà distributive della selezione

- `def` **Proprietà distributiva rispetto alla proiezione**:
    - $\sigma_p(\pi_x(r(A))) = \pi_x(\sigma_p(r(A)))$.
    - La proiezione $\pi$ non deve eliminare attributi usati dalla selezione $\sigma$.
- `def` **Proprietà distributiva rispetto al prodotto cartesiano**:
    - Se $p$ coinvolge sia attributi di $A$ che attributi di $B$ non si possono applicare proprietà distributive.
    - Se invece $p$ coinvolge **solo attributi** contenuti nello schema di **una** delle due relazione:
        - $\sigma_p(r(A) \times s(B)) = \sigma_p(r(A)) \times s(B)$;
        - $\sigma_p(r(A) \times s(B)) = r(A) \times \sigma_p(s(B))$.
- `def` **Proprietà distributiva della selezione rispetto al join** (come sopra):
    - $\sigma_p(r(A) \bowtie_\theta s(B)) = \sigma_p(r(A)) \bowtie_\theta s(B)$;
    - $\sigma_p(r(A) \bowtie_\theta s(B)) = r(A) \bowtie_\theta \sigma_p(s(B))$.
- `def` **Proprietà distributiva della selezione rispetto all'unione**:
    - $\sigma_p(r(A) \cup s(A)) = \sigma_p(r(A)) \cup \sigma(s(A))$.
- `def` **Proprietà distributiva della selezione rispetto alla differenza**:
    - $\sigma_p(r(A) - s(A)) = \sigma_p(r(A)) - \sigma(s(A))$.
- `def` **Proprietà distributiva della selezione rispetto all'intersezione**:
    - $\sigma_p(r(A) \cap s(A)) = \sigma_p(r(A)) \cap \sigma(s(A))$.

#### Proprietà della proiezione multipla

- `def` **Proprietà della proiezione multipla**:
    - $\pi_X(\pi_{X,Y}(r(A))) = \pi_X(r(A))$ (con $X$, $Y$ sottoinsiemi di $A$).

#### Proprietà distributive della proiezione

- `def` **Proprietà distributiva della proiezione al prodotto cartesiano**:
    - $\pi_X(r(A) \times s(B)) = \pi_{X \cap A}(r(A)) \times \pi_{X \cap B}(s(B))$.
- `def` **Proprietà distributive della proiezione rispetto al join**:
    - $\pi_X(r(A) \bowtie_\theta s(B)) = \pi_{X \cap A}(r(A)) \bowtie_\theta \pi_{X \cap B}(s(B))$.
    - Gli attributi coinvolti in $\theta$ devono essere contenuti in $X \cap A$ e in $X \cap B$.
    - Se $X$ non comprende gli attributi usati da $\theta$, la proprietà non vale.
- `def` **Proprietà distributive della proiezione rispetto all'unione**:
    - $\pi_x(r(A) \cup s(A)) = \pi_x(r(A)) \cup \pi_x(s(A))$.
- Non valgono invece le proprietà distributive della proiezione rispetto alla differenza e all'intersezione.
