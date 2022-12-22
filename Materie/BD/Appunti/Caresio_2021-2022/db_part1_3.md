---
title: "Basi di dati - Parte I - Ottimizzazione logica"
---

# Basi di dati - Parte I - Ottimizzazione logica

## Ottimizzazione logica

- L'ottimizzazione delle interrogazioni implica che queste risultino il più efficienti possibili.
    - Si tratterà l'ottimizzazione in termini di tempo (più che di spazio/memoria).
    - Si cerca di diminuire il numero di pagine lette dalla memoria secondaria.
- L'**ottimizzazione logica** è indipendente dalle strutture di memorizzazione.
    - Prende in input l'albero sintattico dell'interrogazione e lo trasforma sfruttando le **proprietà dell'algebra relazionale**.
    - Dove l'albero in uscita sarà sintatticamente diverso ma **semanticamente equivalente** all'interrogazione originale.
- L'euristica principale è quella di **ridurre la massa di tuple** concettualmente coinvolte dall'interrogazione.
    - L'applicazione delle proprietà distributive della selezione.
- L'albero sintattico ottimizzato viene inviato all'ottimizzatore fisico.
    - Questo cambia i nodi foglia con nodi che tengono conto delle strutture fisiche di accesso ai dati.
    - Cambia i nodi intermedi con specifici algoritmi per l'esecuzione degli operatori che sfruttino le strutture fisiche.

### Algoritmo di ottimizzazione logica

- **Algoritmo di ottimizzazione logica**: I predicati $p$ della selezione sono in forma congiuntiva (non è una limitazione, si può ricondurre qualsiasi predicato a una congiunzione di disgiunzioni):
    1. Decomporre gli AND: $\sigma_{p1 \land p2}(r) \rightarrow \sigma_{p1}(\sigma_{p2}(r))$.
    2. Trasferire le selezioni verso le foglie finché è possibile con le proprietà distributive della selezione;
    3. Trasferire le proiezioni verso le foglie finché è possibile con le proprietà distributive della proiezione;
    4. Ricondurre a un'unica selezione le selezioni multiple nello stesso nodo dell'albero: $\sigma_{p1}(\sigma_{p2}(r)) \rightarrow \sigma_{p1 \land p2}(r)$.
    5. Riconoscere le sequenze di join: $\sigma_\theta(r \times s) \rightarrow r \bowtie_\theta s$.
    6. Ricondurre a un'unica proiezione le proiezioni multiple: $\pi_X(\pi_{X \cup Y}(r)) \rightarrow \pi_X(r)$.
    7. Esaminare le varianti dell'albero sintattico dovute alle proprietà associative scegliendo la variante di costo minimo.

#### Commento all'algoritmo

- Passo 1:
    - La distribuzione della selezione è possibile solo se gli attributi coinvolti in $p$ sono contenuti in solo una relazione.
    - Quindi è difficile che venga applicata a una selezione con un **predicato relativo a più relazioni**.
    - Per questo si decompongono i vari AND affinché aumenti la possibilità di applicazione della proprietà.
- Passo 3:
    - Dopo il passo 2 è possibile che siano presenti selezioni multiple (in cascata).
    - L'ottimizzatore ricompone quindi insieme le selezioni multiple.
- Passo 5:
    - L'ottimizzatore trasforma quindi la selezione+prodotto (in cascata) in un theta-join.
    - I DBMS hanno inoltre algoritmi ottimizzati per eseguire (nella fase successiva) i join.
- Passo 7:
    - La valutazione degli alberi di costo minimo avviene tramite l'introduzione di **stime quantitative** delle varie alternative.
    - Il passo consiste principalmente nella ricerca di parentesizzazioni alternative meno costose di join multipli.
        - Ma le configurazioni possibili dovute alle proprietà associative crescono esponenzialmente.
        - Non si esplora quindi tutto lo spazio possibile.
    - L'euristica applicata è quella di anticipare il prima possibile i join tra relazioni con cardinalità bassa.
        - Si cerca il join a cardinalità minima dei sottoalberi e si esegue per primo e così via.

### Aspetti quantitativi delle interrogazioni

- Informazioni mantenute dal DBMS per ogni tabella $r$:
    - $CARD(r) = |r|$: cardinalità della relazione;
    - $SIZE(t)$: ampiezza della tupla in byte;
    - $VAL(A_i, r)$: numero di valori distinti che appaiono nella colonna $A_i$ all'interno della tabella $r$, $|\pi_{A_i}(r)|$.
        - Se $A_i$ è chiave allora $VAL(A_i, r) = CARD(r)$.
    - $MIN(A_i, r)$: valore minimo di $A_i$ contenuto in $r$;
    - $MAX(A_i, r)$: valore massimo di $A_i$ contenuto in $r$;
    - $NPAGE(r)$: numero di pagine occupate da $r$.
        - $NPAGE(r) = CARD(r) * fattoreDiBloccaggio$ (numero di tuple contenute in una pagina);
- Le stime vengono calcolate **senza eseguire la query**.

#### Stima del costo della selezione

- Stima del costo della selezione:
    - Data la selezione $\sigma_p(r)$ conoscendo l'intervallo di variabilità della selezione:
        - $\sigma_p(r)$: $0 \leq | \sigma_p(r) | \leq |r|$.
    - Si può modellare la cardinalità della selezione $\sigma_p(r)$ con un **fattore di selettività** $f_p$ per la cardinalità di $r$:
        - $|\sigma_p(r)| = f_p \cdot |r|$.
    - Il fattore di selettività $f_p$ è legato al solo predicato $p$ di selezione e varia tra $0$ e $1$.
        - Può essere interpretato come la probabilità che una tupla in $r$ soddisfi il predicato di selezione $p$;
        - Ovvero la stima della percentuale di tuple che soddisfano il predicato di selezione.
- I DBMS hanno a disposizione un formulario anche molto avanzato per il calcolo di $f_p$, per semplicità si assumerà:
    - **Distribuzione uniforme** dei valori all'interno delle varie colonne;
    - **Assenza di correlazione** tra attributi diversi (eventi indipendenti).
- Poiché si sta ricavando una stima si trascurano alcuni termini.

#### Stime per predicati atomici

- Stime di $f_p$ per predicati atomici:
    - $A_i = v$: $\frac{1}{\text{VAL}(A_i,r)}$;
    - $A_i \leq v$: $\frac{v - \text{MIN}(A_i,r)}{\text{MAX}(A_i,r) - \text{MIN}(A_i,r)}$;
    - $A_i \geq v$: $\frac{\text{MAX}(A_i,r) - v}{\text{MAX}(A_i,r) - \text{MIN}(A_i,r)}$;
    - $v_1 \leq A_i \leq v_2$: $\frac{v_2 - v_1}{\text{MAX}(A_i,r) - \text{MIN}(A_i,r)}$.
- Quando $v$, $v_1$ e $v_2$ non sono compresi tra $MIN(A_i, r)$ e $MAX(A_i, r)$ le stime devono essere corrette.
    - `eg` Nella prima formula $f_p = 0$.

#### Stime per i predicati composti

-  Stime per i predicati composti:
    - $p_1 \land p_2 \land ... \land p_n$: $f_{p_1} \cdot f_{p_2} \cdot ... \cdot f_{p_n}$;
    - $\lnot p$: $1 - f_p$.
    - $p_1 \lor p_2 \lor ... \lor p_n$: $1 - ((1 - f_{p_1}) \cdot (1 - f_{p_2}) \cdot ... \cdot (1 - f_{p_n}))$.
- L'OR si dimostra da AND e NOT con De Morgan.

#### Stima della cardinalità del (equi)join

- Stima della cardinalità del (equi)join:
    - $| r(A) \bowtie_{A_i = B_j} s(B) | = \min \{ \frac{1}{VAL(A_i, r)}, \frac{1}{VAL(B_j, s)}\} \cdot CARD(r) \cdot CARD(s)$.
- Stima meno precisa: $| r(A) \bowtie_{A_i = B_j} s(B) | =  \frac{1}{VAL(B_j, r)} \cdot CARD(s) \cdot CARD(r)$.
    - Valendo la proprietà commutativa del join, si può scambiare il primo operando con il secondo operando.
    - $| s(B) \bowtie_{B_j = A_i} r(A) | =  \frac{1}{VAL(A_i, r)} \cdot CARD(r) \cdot CARD(s)$.
    - Cambia il fattore di selettività: $\frac{1}{VAL(A_i, r)}$ o $\frac{1}{VAL(B_j, r)}$.
    - Si sceglie quindi il minore tra i due per avere una sovrastima migliore.

##### Stima della cardinalità di equi-join tra selezioni

- Stima della cardinalità del equi-join tra selezioni:
    - $| \sigma_{p1}(r(A)) \bowtie_{A_i = B_j} \sigma_{p2}(s(B)) | = \min \{ \frac{1}{VAL(A_i, \sigma_{p1}(r))}, \frac{1}{VAL(B_j, \sigma_{p2}(s))}\} \cdot CARD(\sigma_{p1}(r)) \cdot CARD(\sigma_{p2}(s))$.
    - Quando $A_i$ e $B_j$ non sono coinvolti nei predicati di selezione:
        - $VAL(A_i, \sigma_{p1}(r)) = \min \{VAL(A_i, r), CARD(\sigma_{p1}(r))\}$;
        - $VAL(B_j, \sigma_{p2}(s)) = \min \{VAL(B_j, s), CARD(\sigma_{p2}(s))\}$;
    - Non è generale, si base sull'assunzione che gli attributi siano indipendenti tra loro.
