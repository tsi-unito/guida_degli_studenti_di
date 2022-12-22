---
title: "Basi di dati - Parte I - Esercizi - Algebra relazionale"
---

# Basi di dati - Parte I - Esercizi - Algebra relazionale

## Esempi introduttivi

### Operatore di selezione

- Selezione dei pazienti con residenza a Torino.
    - $\sigma_{Residenza='TO'}(pazienti)$.
- Selezione dei pazienti con residenza a Torino o a Vercelli.
    - $\sigma_{Residenza='TO' \lor Residenza='VC'}(pazienti)$.
- Selezione dei pazienti non residenti a Torino.
    - $\sigma_{Residenza \neq 'TO'}(pazienti)$.

### Operatore di proiezione

- Proiezione della relazione pazienti sull'attributo `COD` e sull'attributo `Cognome`.
    - $\pi_{COD, Cognome}(pazienti)$.

### Operatori insiemistici

- Elencare cognomi e nomi di tutte le persone coinvolte nel DB *Ricoveri ospedalieri*.
    - Le tue tabelle hanno attributi differenti, è necessaria quindi una proiezione.
    - $\pi_{Cognome, Nome}(pazienti) \cup \pi_{Cognome, Nome}(medici)$.
- Elencare le province di residenza dei medici in cui non risiede alcun paziente.
    - $\pi_{Residenza}(medici) - \pi_{Residenza}(pazienti)$.

### Operatore di ridenozione

- Elencare i medici che non sono primari.
    - $\pi_{MATR}(medici) - \rho_{MATR \leftarrow Primario} (\pi_{Primario}(reparti))$.
- $\rho_{UTENTI(CF, Provincia) \leftarrow PAZIENTI(COD, Residenza)}(pazienti)$.
    - `COD` viene rinominato a `CF`;
    - `Residenza` viene rinominato a `Provincia`;
    - L'intero schema `Pazienti` viene rinominata a `Utenti`.
    - Lo schema del risultato è `Utenti(CF, Cognome, Nome, Provincia, AnnoNascita)`.

### Operatore di theta-join

- Elencare tutte le informazioni sui pazienti ricoverati.
    - $ricoveri \: \bowtie_{(PAZ=COD)} \: pazienti$;
    - $\sigma_{PAZ=COD}(ricoveri \times pazienti)$.
    - Gli schemi `Pazienti` e `Ricoveri` sono disgiunti.
- Elencare le informazioni dei primari di ogni reparto.
    - $reparti \: \bowtie_{(Primario=MATR)} \: medici$.
- Elencare i reparti con le informazioni del primario solo nel caso in cui il primario afferisca al reparto che dirige.
    - $reparti \: \bowtie_{(Primario=MATR \: \land \: COD=Reparto)} \: medici$.
- Elencare i reparti abbinati ai dati dei rispettivi primari solo nel caso in cui il primario non vi afferisca.
    - $reparti \: \bowtie_{(Primario=MATR \: \land \: COD \neq Reparto)} \: medici$.
- Elencare i pazienti ricoverati in chirurgia.
    - $\sigma_{Nome-Rep='Chirurgia'}(reparti \: \bowtie_{COD=Reparto} \: ricoveri)$.
    - $\pi_{PAZ, Inizio}(\sigma_{Nome-Rep='Chirurgia'}(reparti \: \bowtie_{COD=Reparto} \: ricoveri))$.
    - $\pi_{PAZ, Inizio}(\sigma_{Nome-Rep='Chirurgia'}(reparti) \: \bowtie_{COD=Reparto} \: ricoveri)$.
        - Anticipando la selezione si hanno meno tuple che partecipano al theta-join.
- Elencare i medici che hanno avuto in cura il paziente Luigi Missoni.
    - $A = \sigma_{Cognome='Missoni \: \land \: Nome = 'Luigi'}(pazienti) \: \bowtie_{COD=PAZ} ricoveri$;
    - $B = (A) \: \bowtie_{Reparto=Rep} \: \rho_{CM, NM, RM, Rep \leftarrow Cognome, Nome, Residenza, Reparto}(medici)$;
    - $\pi_{COD, AnnoNascita, CM, NM}(B)$.
    - $\pi_{COD, AnnoNascita, MEDICI.Cognome, MEDICI.Nome}((A) \: \bowtie_{RICOVERI.Reparto = MEDICI.Reparto}(medici))$.
- Elencare i pazienti che hanno avuto due o più ricoveri.
    - $A = \rho_{PAZ1, Inizio1, Fine1, Reparto1 \leftarrow PAZ, Inizio, Fine, Reparto} (ricoveri)$;
    - $B = \rho_{PAZ2, Inizio2, Fine2, Reparto2 \leftarrow PAZ, Inizio, Fine, Reparto} (ricoveri)$;
    - $A \: \bowtie_{PAZ1 = PAZ2 \: \land \: Inizio1 < Inizio2} \: B$.
    - Si potrebbe rinominare l'intero schema e poi usare la dot-notation nel theta-join.

### Interrogazioni con negazione

- Elencare i pazienti non residenti a Torino.
    - Equivale a chiedere di elencare i pazienti con residenza diversa da Torino.
    - $\sigma_{Residenza \neq Torino}(pazienti)$.
- Elencare i medici non primari.
    - Si tratta di un'interrogazione con **negazione essenziale**.
    - Si definisce l'universo del discorso $U$ (closed-world assumption): $U = medici$.
    - Si risponde alla domanda in forma positiva $P$:
        - $\pi_{MATR, Cognome, Nome, Residenza, Reparto}(medici \: \bowtie_{MATR=Primario} \: reparti)$.
    - Si utilizza il complemento per ottenere il risultato: $R = U - P$.
- Elencare i pazienti non residenti in città in cui risiede qualche medico.
    - $U = pazienti$;
    - $P = \pi_{COD, PAZIENTI.Cognome, PAZIENTI.Nome, PAZIENTI.Residenza, AnnoNascita}(I)$;
        - $I = pazienti \: \bowtie_{PAZIENTI.Residenza=MEDICI.Residenza} \: medici$;
    - $R = U - P$.
- Elencare i reparti in cui non avvengono ricoveri.
    - $U = \pi_{COD, Nome-Rep}(reparti)$;
    - $P = \pi_{COD, Nome-Rep}(reparti \: \bowtie_{REPARTI.COD=RICOVERI.Reparto} \: ricoveri)$;
    - $R = U - P$.
- Elencare i pazienti di Torino mai curati dal primario 203.
    - $U = \sigma_{Residenza='Torino'}(pazienti)$;
    - $P = \pi_{PAZIENTI.COD, Cognome, Nome, Residenza, AnnoNascita}(I)$;
        - $I = pazienti \: \bowtie_{COD=PAZ} \: ricoveri \: \bowtie_{Reparto = REPARTI.COD} \: \sigma_{Primario='203'}(reparti)$.
    - $R = U - P$.
- Elencare i pazienti di Torino ricoverati ma mai presi in cura dal primario 203.
    - Interpretazione della query precedente con l'universo del discorso differente.
    - $U = \pi_{PAZIENTI.COD, Cognome, Nome, Residenza, AnnoNascita}(\sigma_{Residenza='Torino'}(pazienti) \: \bowtie_{COD = PAZ} \: ricoveri)$.
- Elencare i pazienti con un solo ricovero.
    - **Negazione implicita**.
    - Si elenca i pazienti ricoverati almeno una volta e che non hanno avuto due o più ricoveri.
    - $U = \pi_{PAZ}(ricoveri)$;
    - $P = \pi_{RiCOVERI1.PAZ}(C)$.
        - $A = \rho_{RICOVERI1 \leftarrow RICOVERI}(ricoveri)$;
        - $B = \rho_{RICOVERI2 \leftarrow RICOVERI}(ricoveri)$;
        - $C = A \: \bowtie_{RICOVERI1.PAZ = RICOVERI2.PAZ \: \land \: RICOVERI1.Inizio \neq RICOVERI2.Inizio} \: B$;
    - $R = U - P$.

#### Calcolare il massimo

- Si considera una relazione $r(A)$ composta da un unico attributi di tipo intero $NUM = \{3, 5, 2\}$.
    - Si intende calcolare il massimo con gli operatori finora trattati.
    - $U = \rho_{NUM1 \leftarrow NUM}(r)$.
    - $P = \pi_{NUM1}(\sigma_{NUM1<NUM2}(A))$ (i numeri minori di un altro numero).
        - $A = \rho_{NUM1 \leftarrow NUM}(r) \times \rho_{NUM2 \leftarrow NUM}(r)$;
    - $R = U - P$;

#### Quantificazione universale

- Elencare gli studenti che hanno superato **tutti** i corsi previsti dal piano di studi.
    - $U = \pi_{MATR}(esami)$;
    - $A = \pi_{MATR}(esami) \times PianoDiStudi$ (tutti gli esami che gli studenti dovrebbero sostenere);
    - $B = A - esami$ (elenco degli esami non superati);
    - $C = \pi_{MATR}(B)$ (elenco delle matricole che non hanno ancora superato qualche esame);
    - $R = U - C$.
    - Si ottiene una quantificazione universale attraverso l'utilizzo di due sottrazioni e un prodotto cartesiano.
        - Questa è infatti la definizione dell'**operatore quoziente**.
        - $R = Esami(Matr, Corso) \div P(Corso)$.

### Operatore quoziente

- Elencare i pazienti che sono stati ricoverati **in ogni reparto**.
    - Si applica il quoziente $r(a, B) \div s(B)$.
    - $r(a, B) = \pi_{PAZ, Reparto}(ricoveri)$;
    - $S(B) = \pi_{Reparto}(ricoveri)$.

### Proprietà degli operatori

#### Proprietà associativa ristretta del theta-join

- $r(A) \bowtie_{A=B} (s(B,C) \bowtie_{C=D} u(D, E)) = (r(A) \bowtie_{A=B} \: s(B,C)) \bowtie_{C=D} u(D, E)$.
- $r(A) \bowtie_{A=E} (s(B,C) \bowtie_{C=D} u(D, E)) \neq (r(A) \bowtie_{A=E} \: s(B,C)) \bowtie_{C=D} u(D, E)$.

-----

## Esercizi

### Base di dati "Ricoveri"

- Elencare i codici dei pazienti che non sono mai stati ricoverati.
    - $\pi_{COD}(pazienti) - \rho_{COD \leftarrow PAZ}(\pi_{PAZ}(ricoveri))$.
- Elencare le città in cui risiedono sia medici che pazienti.
    - $\pi_{Residenza}(medici) \cap \pi_{Residenza}(pazienti)$.
- Elencare cognome e nome del primario del reparto in cui è stato ricoverato il paziente Luigi Missoni.
    - $A = (\sigma_{Cognome='Missoni \: \land \: Nome = 'Luigi'}(pazienti) \: \bowtie_{PAZIENTI.COD=PAZ} ricoveri)$;
    - $B = (A \: \bowtie_{Reparto=REPARTI.COD} \: reparti) \: \bowtie_{Primario=MATR} \: medici)$;
    - $\pi_{MEDICI.Cognome, MEDICI.Nome} (B)$.
- Elencare cognome e nome dei pazienti ricoverati più di una volta.
    - $A = \rho_{RICOVERI1 \leftarrow RICOVERI}(ricoveri)$;
    - $B = \rho_{RICOVERI2 \leftarrow RICOVERI}(ricoveri)$;
    - $C = A \: \bowtie_{RICOVERI1.PAZ = RICOVERI2.PAZ \: \land \: RICOVERI1.Inizio < RICOVERI2.Inizio} \: B$;
    - $D = C \: \bowtie_{RICOVERI1.PAZ = PAZIENTI.COD} \: pazienti$.
    - $\pi_{Cognome, Nome}(D)$.
- Elencare cognome e nome dei pazienti residenti in città in cui risiede almeno un medico.
    - $A = pazienti \: \bowtie_{PAZIENTI.Residenza = MEDICI.Residenza} \: medici$;
    - $\pi_{PAZIENTI.Cognome, PAZIENTI.Nome}(A)$.

#### Cardinalità

- Quante tuple genera l'equi-join $ricoveri \: \bowtie_{PAZ = COD} \: pazienti$?
    - La condizione coinvolge la chiave primaria di pazienti ed esiste un vincolo di integrità referenziale tra loro.
    - Quindi per ogni tupla di ricoveri esiste una e una sola tupla di pazienti che rispetta la condizione.
    - $|ricoveri \: \bowtie_{PAZ = COD} \: pazienti| = |ricoveri|$.

### Base di dati "Impiegati"

- Elencare i capi che guadagnano meno di almeno uno dei loro subalterni.
    - $A = \rho_{CAPI \leftarrow IMPIEGATI}(impiegati) \: \bowtie_{CAPI.MATR = CAPO} \: organigramma$;
    - $B = (A) \: \bowtie_{IMPIEGATO = IMPIEGATI.MATR} \: impiegati$;
    - $C = \sigma_{CAPI.Stipendio < IMPIEGATI.Stipendio} (B)$;
    - $\pi_{CAPI.Cognome, CAPI.Nome}(C)$.

-----

## Esercitazione

### Esercizio 0

- Ricavare i pazienti che sono anche medici (stesso nome, cognome e residenza).
    - $\pi_{Nome, Cognome, Residenza}(pazienti \bowtie medici)$.
    - Nota bene che $\{Nome, Cognome, Residenza\}$ non è una superchiave in questo caso.

### Esercizio 0 bis

- Ricavare il nome e il cognome del paziente più anziano (anno nascita minimo).
    - $U = \pi_{AnnoNascita}(\rho_{(PAZIENTI1 \leftarrow PAZIENTI)} (pazienti))$;
    - $P = \pi_{PAZIENTI1.AnnoNascita}(P1 \bowtie_{PAZIENTI1.AnnoNascita>PAZIENTI2.AnnoNascita} P2)$.
        - $P1 = \rho_{(PAZIENTI1 \leftarrow PAZIENTI)}(pazienti)$;
        - $P2 = \rho_{(PAZIENTI2 \leftarrow PAZIENTI)}(pazienti)$.
    - $A = U - P$;
    - $R = \pi_{Nome, Cognome}(A \bowtie pazienti)$.

### Esercizio 0 ter

- Ricavare il nome e il cognome dei medici che hanno curato almeno due pazienti nati prima del 1960.
    - $P60$: pazienti nati prima del 1960;
        - $P60 = \sigma_{AnnoNascita<1960}(pazienti)$.
    - $PM$: pazienti nati prima del 1960 con i dati dei medici che li curano;
        - $A = P60 \bowtie_{PAZIENTI.COD=RICOVERI.PAZ} ricoveri \bowtie_{RICOVERI.Reparto=Medici.Reparto} medici$;
        - $PM = \pi_{PAZIENTI.COD, MEDICI.MATR, MEDICI.Nome, MEDICI.Cognome}(A)$.
    - $M$: medici che curano almeno due pazienti diversi nati prima del 1960;
        - $PM1 = \rho_{PAZIENTI1 \leftarrow, MEDICI1 \leftarrow MEDICI, RICOVERI1 \leftarrow RICOVERI}(PM)$;
        - $PM2 = \rho_{PAZIENTI2 \leftarrow, MEDICI2 \leftarrow MEDICI, RICOVERI2 \leftarrow RICOVERI}(PM)$;
        - $M = PM1 \bowtie_{MEDICI1.MATR = MEDICI2.MATR \land PAZIENTI1.COD \neq PAZIENTI2.COD} PM2$.
    - $R$: dati dei medici in $M$.
        - $R = \pi_{MEDICI1.Nome, MEDICI1.Cognome}(M)$.
    - In caso avesse richiesto **tre pazienti**:
        - Al posto di $\land \neq$ tra i vari pazienti si deve stabilire un ordine ($<$).

### Esercizio 1 - Quoziente

- Elencare i pazienti curati da ogni medico.
    - $r(PAZ, MATR)$: coppie pazienti e medici che li curano.
    - $s(MATR)$: tutti i medici.
    - $R$: pazienti in $r(PAZ, MATR)$ che sono accoppiati con ogni medico in $s(MATR)$.
        - $R = \pi_{PAZ, MATR} (ricoveri \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} reparto) \div \pi_{MATR}(medici)$.
        - Bisogna che le strutture sia sintatticamente consistenti affinché sia applicabile l'operatore quoziente.
        - $r(A, B) \div s(B)$.

### Esercizio 2 - Quoziente

- Elencare i medici che hanno curato tutti i pazienti ricoverati.
    - $r(MATR, PAZ)$: coppie medico e paziente curato.
    - $s(PAZ)$: tutti i pazienti ricoverati.
    - $R$: medici in $r$ che sono accoppiati con ogni paziente in $s$.
        - $R = \pi_{MATR, PAZ} (ricoveri \bowtie_{RICOVERI.Reparto=MEDICI.Reparto} reparto) \div \pi_{PAZ}(ricoveri)$.

### Esercizio 3

- Elencare gli studenti che hanno superato tutti gli esami del loro indirizzo.
    - Nonostante possa sempre utile l'utilizzo del quoziente si usa invece la negazione esistenziale.
    - $U$: studenti che hanno superato almeno un esame;
        - $U = \pi_{MATR}(esami)$.
    - $P$: studenti che non hanno superato qualche esame del loro indirizzo;
        - Non essendo disponibile la quantificazione universale si lavora per negazione.
        - $I$: per ogni studente tutti i corsi associati al suo indirizzo;
            - $I = studenti \bowtie_{STUDENTI.Indirizzo = OFFERTA.Indirizzo} offerta$.
        - $J$: gli esami non superati dagli studenti per ogni indirizzo;
            - $J = \pi_{MATR, Corso, STUDENTE.Indirizzo}(I) - esami$.
        - $P = \pi_{MATR}(J)$.
    - $R = U - P$.
- Non si può usare il quoziente perché uno studente non è associato a tutti i corsi di $offerta$.
    - Ma è accoppiato solo a quelli della suo indirizzo.

### Esercizio 4

- Elencare i nomi dei piloti che non hanno partecipato a gare nelle nazioni in cui si sono disputati almeno due gran premi.
    - $r1$: le nazioni con almeno due GP;
        - $GP1 = \rho_{GRANPREMIO1 \leftarrow GRANPREMIO}(GranPremio)$;
        - $GP2 = \rho_{GRANPREMIO2 \leftarrow GRANPREMIO}(GranPremio)$;
        - $r1 = \pi_{GP1.Nazione} (GP1 \bowtie_{GP1.Nazione = GP2.Nazione \land GP1.Anno < GP2.Anno} GP2)$;
            - L'utilizzo di $<$ (o di $\neq$ o $>$) è fondamentale rispetto a $=$.
    - $r2$: i piloti che hanno partecipato a gare nelle nazioni in $r2$;
        - $A = GRANPREMIO1.Nazione = GRANPREMIO.Nazione$;
        - $B = GRANPREMIO.Nazione = PIAZZAMENTO.Nazione$.
        - $C = B \land GRANPREMIO.Anno = PIAZZAMENTO.Anno$;
        - $r2 = \pi_{Pilota}(r1 \bowtie_{A} GranPremio \bowtie_{C} Piazzamento)$.
    - $r3$: i piloti che non sono in $r2$.
        - $r3 = \rho_{Pilota \leftarrow Nome}(\pi_{Nome}(Pilota)) - r2$.

### Esercizio 5

- Elencare i nomi dei giocatori che hanno sempre vinto contro giocatori della loro stessa squadra.
    - $r1$: le partite tra giocatori della stessa squadra.
        - $GI1 = \rho_{G1 \leftarrow GIOCATORE}(giocatore)$;
        - $GI2 = \rho_{G2 \leftarrow GIOCATORE}(giocatore)$;
        - $r1 = GI1 \bowtie_{G1.TessaraGiocatore = Partita.Giocatore1} Partita \bowtie_{G2.TessaraGiocatore = Partita.Giocatore2} GI2$.
    - $r2$: i giocatori perdenti nelle partite in $r1$.
        - $P2 = \pi_{Giocatore2}(\sigma_{Vincitore=Giocatore1}(r1))$;
        - $P1 = \pi_{Giocatore1}(\sigma_{Vincitore=Giocatore2}(r1))$;
        - $r2 =  \rho_{Giocatore \leftarrow Giocatore2}(P2) \cup \rho_{Giocatore \leftarrow Giocatore1}(P1)$.
    - $r3$: i nomi dei giocatori che hanno vinto almeno una partita in $r1$ e non sono in $r2$.
        - $U = \sigma_{Vincitore}(r1)$;
            - L'universo non è invece tutti i giocatori.
        - $r3 = \rho_{Giocatore \leftarrow Vicintore}(U) - r2$.
