---
title: "Basi di dati - Parte I - Esercizi - Ottimizzazione logica"
---

# Basi di dati - Parte I - Esercizi - Ottimizzazione logica

## Caso studio

- Elencare i pazienti di Torino ricoverati nel 2010 dal dottore 405.
- $\pi_L(\sigma_{p1 \land p2 \land p3}(pazienti \bowtie_{COD=PAZ} ricoveri) \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} medici))$.
    - $p1$: $Pazienti.Residenza = TO$;
    - $p2$: $Inizio=2010$;
    - $p3$: $MATR='405'$;

### Dizionario dati

- $CARD(pazienti) = 10^5$;
- $CARD(ricoveri) = 10^5$;
- $CARD(medici) = 100$;
- $CARD(reparti) = 10$;
- $VAL(Inizio, ricoveri) = 10$;
- $VAL(reparto, ricoveri) = 10$;
- $VAL(residenza, pazienti) = 100$;
- $VAL(PAZ, ricoveri) = 10^5$;
- $VAL(reparto, medici) = 10$.

### Stime su query non ottimizzata

- Calcolo fattore di selettività dei predicati atomici:
    - $f_{p1} = 1 / VAL(residenza, pazienti) = 1 / 100$;
    - $f_{p2} = 1 / VAL(Inizio, ricoveri) 1 / 10$;
    - $f_{p3} = 1 / VAL(MATR, Medici) = 1 / CARD(medici) = 1/100$.
        - $MATR$ è una chiave primaria, quindi $VAL(MATR, Medici) = CARD(medici)$.
- Calcolo della cardinalità del join $r1 = pazienti \bowtie_{COD=PAZ} ricoveri$:
    - $| pazienti \bowtie_{COD=PAZ} ricoveri| =$
    - $= \min\{a, b\}$
        - $a = \frac{1}{VAL(COD, pazienti)} \cdot CARD(pazienti) \cdot CARD(ricoveri)$;
        - $b = \frac{1}{VAL(PAZ, ricoveri)} \cdot CARD(pazienti) \cdot CARD(ricoveri)\}$;
        - $COD$ è chiave di pazienti quindi $VAL(COD, pazienti) = CARD(pazienti)$.
    - $= \min\{CARD(ricoveri), 1/VAL(PAZ, ricoveri) \cdot CARD(pazienti) \cdot CARD(ricoveri)\}$
    - $= \min\{10^5, 1 / 10^5 \cdot 10^5 \cdot 10^5\} = 10^5$.
- Calcolo della cardinalità del join $r2 = r1 \bowtie_{RICOVERI.Reparto = MEDICI.Reparto} medici$:
    - $| r1 \bowtie_{RICOVERI.Reparto = MEDICI.Reparto} medici| =$
    - $= \min\{1/VAL(Reparto,r1), 1/VAL(Reparto, meidici)\} \cdot CARD(r1) \cdot CARD(medici)$
    - $= \min\{1/10, 1/10\} \cdot 10^5 \cdot 10^2 = 1/10 \cdot 10^5 \cdot 10^2 = 10^6$.
- Calcolo della cardinalità della selezione $\sigma_{p1 \land p2 \land p3}$:
    - $r3 = |\sigma_{p1 \land p2 \land p3}| =$
    - $= f_{p1} \cdot f_{p2} \cdot f_{p3} \cdot CARD(r2)$
    - $= 1/100 \cdot 1/10 \cdot 1/100 \cdot 10^6 = 10$.
- Massa di tuple coinvolte:
    - $r1$: combina $10^5$ tuple di $medici$ e $10^5$ tuple di $ricoveri$ per restituire $10^5$ tuple;
    - $r2$: combina $10^5$ tuple di $r1$ e $10^2$ tuple di $medici$ per restituire $10^6$ tuple;
    - $r3$: valuta $10^6$ tuple di $r2$ per restituire $10$ tuple.
    - Sono quindi coinvolte $10^5 \cdot 10^5 + 10^5 \cdot 10^2 + 10^6$ tuple.
- Considerando il **valore dominante** $10^5 \cdot 10^5 = 10^{10}$, si muovono quindi $10^{10}$ tuple per avere approssimativamente $10$ tuple.

### Query ottimizzata (prima alternativa)

- Per le proprietà associative dei join si ottengono due alternative.
    - Si va a valutare la prima.
- $\pi_L((A \bowtie_{COD=PAZ} B) \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} C)$.
    - $A$: $\sigma_{p1}(pazienti)$;
    - $B$: $\sigma_{p2}(ricoveri)$;
    - $C$: $\sigma_{p3}(medici)$.

#### Stime su query ottimizata (prima alternativa)

- Calcolo delle cardinalità della selezione $r1' = \sigma_{PAZIENTI.Residenza='TO'}(pazienti)$:
    - Le tuple coinvonte dalla selezione sono esattamente le tuple della relazione $pazienti$ ($10^5$).
    - $| \sigma_{PAZIENTI.Residenza='TO'}(pazienti)| =$
    - $= f_{p1} \cdot CARD(pazienti) = 1/100 \cdot 10^5 = 10^3$.
- Calcolo delle cardinalità della selezione $r2' = \sigma_{Inizio=2010}(ricoveri)$:
    - Le tuple coinvonte dalla selezione sono esattamente le tuple della relazione $ricoveri$ ($10^5$).
    - $| \sigma_{Inizio=2010}(ricoveri)| =$
    - $= f_{p2} \cdot CARD(ricoveri) = 1/10 \cdot 10^5 = 10^4$.
- Calcolo della cardinalità del equi-join $r3' = r1' \bowtie_{COD=PAZ} r2'$:
    - Il join lavora su $10^3 \cdot 10^4 = 10^7$ tuple.
    - $| r1' \bowtie_{COD=PAZ} r2' | =$
    - $= \min\{1/VAL(COD, r1'), 1/VAL(PAZ, r2')\} \cdot CARD(r1') \cdot CARD(r2')$
        - Non si ha però $VAL(COD, r1')$ e $VAL(COD, r2')$.
        - $r1'$ e $r2'$ sono relazioni virtuali di cui il DBMS non ha statistiche.
        - $VAL(COD, r1') = \min\{VAL(COD, pazienti), CARD(r1')\} = 10^3$.
            - Non può essere maggiore di $VAL(COD, pazienti) = 10^5$ e $CARD(r1') =10^3$.
        - $VAL(PAZ, r2') = \min\{VAL(PAZ, ricoveri), CARD(r2')\} = 10^4$.
            - Non può essere maggiore di $VAL(PAZ, ricoveri) = 10^5$ e $CARD(r2') =10^4$.
    - $= \min\{1/10^3, 1/10^4\} \cdot 10^3 \cdot 10^4 = 1/10^4 \cdot 10^3 \cdot 10^4 = 10^3$.
- Calcolo della cardinalità della selezione $r4' = \sigma_{MATR='405'}(medici)$:
    - Le tuple coinvonte dalla selezione sono esattamente le tuple della relazione $medici$ ($10^2$).
    - $|\sigma_{MATR='405'}(medici)| = 1$.
        - $MATR$ è una chiave quindi produce una sola tupla.
- Calcolo della cardinalità dell'equi-join $r5' = r3' \bowtie_{RICOVERI.Reparto = MEDICI.Reparto} r4'$:
    - Il join lavora su $10^3 \cdot 1 = 10^3$ tuple.
    - Non è utile calcolare quante tuple produce perché dovrà produrre lo stesso numero di tuple dell'albero precedente.
        - In quanto sintatticamente equivalente a esso.
- Massa di tuple coinvolte:
    - $r3'$: combina $10^3$ tuple di $\sigma_{p1}(pazienti)$ e $10^4$ tuple di $\sigma_{p2}(ricoveri)$ per restituire $10^3$ tuple;
    - $r4'$: valuta $10^2$ tuple di medici per restituire $1$ tupla;
    - $r5'$: combina $10^3$ tuple di $r3'$ e $1$ tupla di $r4'$ per restituire $10$ tuple.
    - Sono quindi coinvolte $10^3 \cdot 10^4 + 10^2 + 10^3 \cdot 1$ tuple.
- Considerando il valore dominante $10^3 \cdot 10^4$ si muove **mille volte meno** della massa di tuple coinvolte senza ottimizzazione.

### Valutazione passo 7

- I join della query operano su tre selezioni:
    - $\sigma_{p1}(pazienti)$ elabora $10^5$ tuple e $|\sigma_{p1}| = 10^3$;
    - $\sigma_{p2}(ricoveri)$ elabora $10^5$ tuple e $|\sigma_{p2}| = 10^4$;
    - $\sigma_{p3}(medici)$ elabora $10^2$ tuple e $|\sigma_{p3}| = 1$.
- Quindi l'ottimizzatore cercherà di eseguire prima un join che coinvolge $\sigma_{p3}(medici)$.
    - Tra gli altri due sottoalberi quello con cardinalità minore è quello di $\sigma_{p2}(ricoveri)$ ($10^3)$;
    - Ma $medici$ non può entrare in join direttamente con $paziente$, quindi è costretto a scegliere il join con $\sigma_{p1}(pazienti)$.

### Query ottimizzata (seconda alternativa)

- Per le proprietà associative dei join si ottengono due alternative.
    - Si va a valutare la seconda.
- $\pi_L(A \bowtie_{COD=PAZ} (B \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} C))$.
    - $A$: $\sigma_{p1}(pazienti)$;
    - $B$: $\sigma_{p2}(ricoveri)$;
    - $C$: $\sigma_{p3}(medici)$.

#### Stime su query ottimizata (seconda alternativa)

- Calcolo della cardinalità del equi-join $r1'' = B \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} C$:
    - Il join lavora su $10^4 \cdot 1 = 10^4$ tuple.
    - $| B \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} C| =$
    - $= \min(1/VAL(Reparto, C), 1/VAL(Reparto, B)\} \cdot CARD(C) \cdot CARD(B)$
    - $= min \{1/1, 1/10 \} \cdot 1 \cdot 10^4 = 10^3$.
- Il join $r2'' = C \bowtie_{COD=PAZ} r1''$:
    - Il join lavora su $10^3 \cdot 10^3 = 10^6$ tuple.
- Massa di tuple coinvolte:
    - $r1''$: $10^4$;
    - $r2''$: $10^6$;
    - Le tre selezioni: $10^2 + 10^5 + 10^5$.
    - Sono quindi coinvolte $10^2 + 10^5 + 10^5 + 10^4 + 10^6$ tuple.
- Considerando il valore dominante $10^6$ si muove **diecimila volte meno** della massa di tuple coinvolte senza ottimizzazione.
    - Inferiore al costo di $10^7$ del primo albero alternativo.
