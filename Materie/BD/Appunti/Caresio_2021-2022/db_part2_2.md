---
title: "Basi di dati - Parte II - Dipendenze funzionali"
---

# Basi di Dati - Parte II

## Dipendenze funzionali

### Forma normale

- Una **forma normale** è una proprietà di una base di dati relazionale che ne garantisce la qualità.
    - Come la non presenza di ridondanze;
    - E il mantenimento della consistenza dopo aggiornamenti, cancellazioni e inserimenti di dati nella base.
- Una base di dati relazionale in forma non normale presenta delle criticità:
    - Criticità di *esprimibilità*: incapacità di inserire un'informazione **concettualmente significativa** proveniente dal sistema informativo.
    - Criticità di *efficienza*: autoesplicativo.
    - Queste criticità possono presentarsi in seguito a:
        - Anomalie di inserimento;
        - Anomalie di cancellazione;
        - Anomalie di aggiornamento;

### Dipendenza funzionale

- Data una relazione $r(A)$ e due sottoinsiemi $X$ e $Y$ di attributi di $A(X, Y \subseteq A)$, il **vincolo di dipendenza funzionale** $X \rightarrow Y$ è soddisfatto sse $\forall t_1, t_2 \in r(t_1[X]=t_2[X] \implies t_1[Y]=t_2[Y])$.
    - $X \rightarrow Y$ si deve interpretare come $X$ **determina** $Y$.
    - Prese due tuple della relazione $r$, se queste tuple coincidono sul valore dell'attributo $X$ devono coincidere anche sul valore del attributo $Y$.
-  Per ogni relazione $r$ si ha un **insieme di dipendenze funzionali** $F$.
    - Due basi di dati, una progettata con i vincoli $F'$ e un'altra con i vincoli $F''$, se $F'$ e $F''$ sono equivalenti, **evolvono** esattamente nello stesso modo.
    - Per verificare che siano equivalenti occorre dimostrare: $f'_1 \land f'_2 \Leftrightarrow f''_1 \land f''_2$.
- Le dipendenze funzionali vengono raccolte attraverso un'analisi attenta della realtà.

### Teoria di Armstrong

- Armstrong ha definito una teoria con la quale caratterizza la dipendenza funzionale ($\rightarrow$) senza dover ricorrere alla definizione di dipendenza funzionale stessa.
    - `def` **Assioma di riflessività**: se $Y \subseteq X$ allora $X \rightarrow Y$.
    - `def` **Assioma di unione**: se $X \rightarrow Y$ e $X \rightarrow Z$ allora $X \rightarrow YZ$ (dove $YZ = Y \cup Z$).
    - `def` **Assioma di transitività**: se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $X \rightarrow Z$.
    - Chiamati assiomi, sono più propriamente proprietà inferibili dalla definizione di dipendenza funzionale.
- La Teoria di Armstrong è **corretta** e **completa**.
    - *Correttezza*: se è possibile dedurre $X \rightarrow Y$ con gli assiomi di Armstrong lo si può fare anche con la definizione di d.f.;
    - *Completezza*: se è possibile dedurre $X \rightarrow Y$ con la definizione di d.f. lo si può fare anche con gli assiomi di Armstrong.
    - Quindi ragionare con i suoi assiomi è perfettamente **equivalente** a ragionare con le definizioni di dipendenza funzionale.
- Dagli assiomi di Armstrong è possibile ricavare regole aggiuntive:
    - `def` **Regola dell'espansione**: data una d.f. $X \rightarrow Y$ e un insieme di attributi $W$, allora $WX \rightarrow WY$.
    - `def` **Regola di decomposizione**: se $X \rightarrow YZ$ allora $X \rightarrow Y$ e $X \rightarrow Z$ (vale solo sul conseguente).
    - `def` **Regola di pseudo-transitività**: se $X \rightarrow Y$ e $WY \rightarrow Z$, allora $WX \rightarrow Z$.
    - `def` **Regola del prodotto**: data $X \rightarrow Y$ e $W \rightarrow Z$, allora $XW \rightarrow YZ$.

#### Equivalenza di insiemi di dipendenze funzionali

- Primo approccio al problema dell'equivalenza di insiemi di dipendenze funzionali:
    - Due insiemi di dipendenze funzionali sono equivalenti quando fanno **evolvere nello stesso modo** la base di dati.
    - Sono equivalenti sse l'insieme di dipendenze funzionali derivabili dal primo è uguale all'insieme di dipendenze funzionali derivabili dal secondo.
    - $F \equiv G$ ($F$ è equivalente a $G$) sse $F \vdash G$ e $G \vdash F$.
        - $F \vdash G$ ($G$ è deducibile da $F$): presa una qualsiasi dipendenza $g$ di $G$, $g$ è deducibile da $F$.
- Secondo approccio al problema dell'equivalenza:
    - Due insiemi di dipendenze funzionali sono equivalenti quando hanno la **stessa chiusura**.
        - La **chiusura di un insieme di dipendenze funzionali** $F$ è l'insieme (finito) $F^+$ di tutte le dipendenze funzionali derivabili da $F$.
        - Derivabili tramite l'applicazione degli assiomi di Armstrong.
        - Il calcolo di una chiusura richiede però tempo esponenziale sulla cardinalità dell'insieme in esame.
    - $F \equiv G$ ($F$ è equivalente a $G$) sse $F^+ = G^+$.
- Il calcolo dell'equivalenza è quindi molto costoso.
    - I due approcci sono però **equivalenti**.
    - $F \vdash G$ e $G \vdash F \iff F^+ = G^+$.

#### Chiusura di un insieme di attributi

- `def` **Chiusura di un insieme di attributi**: $X^+_F = \{A \mid X \rightarrow A \in F^+ \}$.
    - Definito dato un insieme di attributi $R$ su cui è definito l'insieme di d.f. $F$ e dato un sottoinsieme $X \subseteq R$.
    - Utile per un approccio più efficace alla verifica dell'equivalenza di insiemi di d.f..
- La chiusura di un insieme di dipendenze funzionali e la chiusura di un insieme di attributi sono **strettamente legati**.
    - Si può infatti ricondurre il problema di decidere se $X \rightarrow Y$ appartenga a $F^+$ controllando se $Y$ è in $X_F^+$.
    - Si verifica che $X \rightarrow Y \in F^+$ sse $Y \subseteq X^+_F$.

##### Algoritmo per il calcolo della chiusura di un insieme di attributi

- Algoritmo per il calcolo di $X^+_F$:
    1. $C = X$, $F' = F$;
    2. Per ogni d.f. $Y \rightarrow Z$ in $F'$ tale che $Y \subseteq C$;
        - $C := C \cup Z$;
        - $F' := F' - \{Y \rightarrow Z\}$.
    3. `return` $C$.
- Per far sì che gli attributi $Z$ vengano aggiunti a $C$ tutti gli antecedenti $Y$ devono essere in $C$.
    - E non solo una parte.
- L'algoritmo è *corretto* e *completo*.
    - *Correttezza*: a ogni passo vengono aggiunti in $C$ attributi che sarebbero ricavabili anche con gli assiomi di Armstrong;
    - *Completezza*: tutti gli attributi ricavabili dalla definizione sono anche ricavati dall'algoritmo.
    - Ha complessità **polinomiale** rispetto al numero di d.f. e al numero di attributi.
        - Il ciclo contiene unione e differenza tra insiemi di attributi, operazioni polinomiali nel numero di attributi.
        - E viene eseguito una volta per ogni dipendenza funzionale.

##### Terzo approccio al problema dell'equivalenza

- Terzo approccio al problema dell'equivalenza di insiemi di dipendenze funzionali:
    - Si controlla se **ogni** $X \rightarrow Y$ di $F$ è deducibile in $G$ verificando se $Y$ è nella chiusura di $X$ usando le d.f. di $G$.
        - Ovvero si verifica che $Y \subseteq X_G^+$.
    - Viceversa, si controlla se **ogni** $X \rightarrow Y$ di $G$ è deducibile in $F$ verificando se $Y$ è nella chiusura di $X$ usando le d.f. di $F$.
        - Ovvero si verifica che $Y \subseteq X_F^+$.
    - `def` $F \equiv G$ sse $\forall X \rightarrow Y \in F(X \rightarrow Y \in G^+)$ e $\forall X \rightarrow Y \in G(X \rightarrow Y \in F^+)$.
    - `def` $F \equiv G$ sse $\forall X \rightarrow Y \in F(Y \subseteq X^+_G)$ e $\forall X \rightarrow Y \in G(Y \subseteq X^+_F)$.
        - Soluzione migliore al problema dell'equivalenza perché sufficientemente efficiente.

#### Dipendenze funzionali e superchiavi

- `def` **Superchiave**: $\forall t_i, t_j \in r (t_i[K] = t_j[K]) \Rightarrow (t_i[A] = t_j[A])$.
    - Data una relazione $r(A)$ e l'insieme di attributi $K \subseteq A$.
    - La definizione è simile alla definizione di vincolo di dipendenza funzionale.
- `def` **Superchiave**: un insieme di attributi $K \subseteq A$ sse $A = K_F^+$.
    - Dato una schema di relazione $R(A)$ con un insieme di d.f. $F$.
- Una **chiave** (candidata) è una **superchiave minimale**.
    - Se si identificano due insiemi di d.f. $F$ e $G$ equivalenti, si devono avere le **stesse identiche** chiavi candidate.
        - Infatti $F$ e $G$ avranno la stessa chiusura.
    - Si può avere **più di una chiave candidata** per la stessa relazione e lo stesso insieme di dipendenze funzionali.
- Gli attributi coinvolti nelle dipendenze funzionali **non necessariamente** coprono tutti gli attributi della relazione.
    - Quindi gli attributi della relazione non coinvolti dalle d.f. **devono sempre** fare parte delle chiavi candidate congetturate.
        - Queste infatti non sono derivabili tramite le dipendenze funzionali.
- Una chiave primaria $K$ su una relazione $R(A)$ implica $K \rightarrow A$ anche quando non è dichiarata esplicitamente.
