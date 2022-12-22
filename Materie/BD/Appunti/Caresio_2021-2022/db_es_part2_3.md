---
title: "Basi di dati - Parte II - Esercizi - Normalizzazione"
---

# Basi di dati - Parte II - Esercizi

## Normalizzazione

### Esempi introduttivi

#### Esempio 1

- Si considera $S(Matr, NomeS, Voto, Corso, CodC, Titolare)$.
    - E la sua decomposizione in $S_1'(Matr, Nomes, Voto, CodC)$ e $S_2(Corso, Codc, Titolare)$.
    - Si intersecano gli attributi di $S_1'$ e $S_2$:
        - $\{Matr, Nomes, Voto, CodC\} \cap \{Corso, Codc, Titolare\} = \{CodC\}$.
        - $CodC$ è superchiave di $S_2$ perché $CodC \rightarrow Corso, Titolare \in F$.
            - È inoltre una chiave minimale.
    - La decomposizione è quindi senza perdita di informazione.

### Boyce-Codd Normal Form (BCNF)

#### Esempio 2

- La relazione $ESAMI$ con le d.f. $F$:
    - $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - $CF \rightarrow MATR$;
    - $IS \rightarrow CAP$;
    - $MATR, Co \rightarrow Vo, Lo, DE, CP, NP$;
    - $CP \rightarrow NP, Q$;
    - $Q \rightarrow TU$;
    - $CP \rightarrow Co$.
- Non è in $BCNF$:
    - La prima $d.f.$ non è riflessiva.
    - $MATR$ non è una superchiave (calcolato in precedenza).

#### Esempio 3

- Si considera un sottoinsieme dello schema:
    - STUDENTE(MATR, NS, CF, DN);
    - $F = \{MATR \rightarrow CF, NS, DN; \: CF \rightarrow MATR \}$.
- Analizzando la relazione:
    - Si trovano due chiavi candidate $MATR$ o $CF$.
    - Le dipendenze funzionali sono tutte del tipo $2$ (BCNF) perché sia $MATR$ che $CF$ sono superchiavi.
    - Quindi $STUDENTE$ è una relazione in $BCNF$.

#### Esempio 4

- Si considera lo schema:
    - CC(**Titolare, NConto**, NAgenzia, CittaAgenzia, Saldo);
    - $F = \{NConto \rightarrow NAgenzia, CittAgenzia, Saldo \}$.
- Analizzando la relazione:
    - $CC$ non è in $BCNF$ perché la d.f. non è riflessiva e $NConto$ non è superchiave.
    - Se si rimuove dallo schema l'attributo $Titolare$, $CC$ è in $BCNF$.

#### Esempio 5 - Limitazioni della BCNF

- Si considera lo schema:
    - DIR(Dirigente, **Progetto, Sede**);
    - $F = \{ Progetto, Sede \rightarrow Dirigente; Dirigente \rightarrow Sede \}$.
- Analizzando la relazione:
    - La relazione non è in $BCNF$: in $f_2$ $Dirigente$ non è superchiave.
- Si vuole decomporre la relazione in modo da avere delle sottorelazioni che siano in $BCNF$:
    - Però $Progetto, Sede \rightarrow Dirigente$ coinvolge tutti gli attributi.
    - E quindi nessuna decomposizione può conservare tale dipendenza funzionale.
    - Bisognerebbe aggiungere il vincolo decomposto come vincolo globale (costoso).

### Terza forma normale (3NF)

#### Esempio 6 - Attributi primi

- Si considera lo schema:
    - ESAMI(**MATR, Co**, Vo, CP);
    - $F = \{MATR, Co \rightarrow Vo, CP; CP \rightarrow Co \}$;
    - Con le chiavi candidate $\{MATR, Co\}$ e $\{MATR, CP\}$.
- Analizzando la relazione:
    - $Co$ è contenuto in una chiave delle relazione esami.
    - $Co$ è quindi un **attributo primo** per la relazione esami.
        - Non è una chiave o una superchiave ma un attributo primo.

#### Esempio 7

- Si considera lo schema:
    - ESAMI(**MATR, Co**, Vo, CP);
    - $F = \{MATR, Co \rightarrow Vo, CP; CP \rightarrow Co \}$;
    - Con le chiavi candidate $\{MATR, Co\}$ e $\{MATR, CP\}$.
- Analizzando la relazione:
    - $MATR, Co \rightarrow Vo, CP$ è una d.f. $BCNF$;
    - $CP \rightarrow Co$ è una d.f. $3NF$ ma non $BCNF$.
- La relazione è quindi in $3NF$.

#### Esempio 8 - 3NF e anomalie

- Una relazione in $3NF$ può avere anomalie di inserimento e di cancellazione.
- Si considera lo schema:
    - AGENZIE(**NAgenzia, CittaAgenzia**, Direttore);
    - $F_{AGENZIE} = \{NAgenzia, CittaAgenzia \rightarrow Direttore; Direttore \rightarrow CittaAgenzia \}$.
- Analizzando la relazione:
    - $AGENZIE$ non è in BCNF perché la seconda d.f. $Direttore$ non è superchiave.
    - Ma il conseguente della stesa d.f. ($CittaAgenzia$) è un attributo primo, quindi è in $3NF$.
- Presenza di anomalie a causa della d.f. $Direttore \rightarrow CittaAgenzia$:
    - Se si aggiunge un direttore si deve aggiungere anche un'agenzia corrispondente perché $CittaAgenzia$ è in chiave.
        - Anomalia di inserimento.
    - Se si cancella tutte le agenzie di un direttore, si cancella anche ogni traccia del direttore.
        - Anomalia di cancellazione.
    - Se si cambia $CittaAgenzia$ a un direttore, si deve aggiornare tutte le agenzie che dirige.
        - Anomalia di aggiornamento.
        - Ma i cambiamenti delle chiavi primarie sono rari e limitati.

### Insieme di copertura minimale

#### Esempio 9 - Attributi estranei

- Si considera il seguente insieme di dipendenze funzionali:
    - $F = \{ABCD \rightarrow E; B \rightarrow C \}$.
- Analizzando l'insieme:
    - $C$ si può ricavare da $B$ tramite le due dipendenze funzionali.
    - $C$ è quindi un **attributo estraneo**.
- $F$ diventa quindi $F = \{ABD \rightarrow E; B \rightarrow C \}$.
- Utilizzando la chiusura:
    - $C$ è estraneo nella prima d.f. perché $E \in \{ABD\}^+_F = \{ABCDE\}$.
    - $D$ non è estraneo nella prima d.f. perché $D \notin \{ABC\}^+_F = \{ABC\}$.

#### Esempio 10 - Dipendenze ridondanti

- Si considera il seguente insieme di dipendenze funzionali:
    - $F = \{A \rightarrow B; B \rightarrow C; A \rightarrow C\}$.
- Analizzando l'insieme:
    - $A \rightarrow C$ è ridondante perché $C \in {A}^+_{\{A \rightarrow B; B \rightarrow C\}} = \{ABC\}$.
    - $B \rightarrow C$ non è ridondante perché $C \notin {B}^+_{\{A \rightarrow B; A \rightarrow C\}} = \{B\}$.
- $F$ diventa quindi $F = \{A \rightarrow B; B \rightarrow C \}$.

#### Esempio 11 - Non unicità di un insieme di copertura minimale

- Si considera lo schema:
    - R(CF, MATR, SCU);
    - Con il seguente insieme di dipendenze funzionali:
        - $CF \rightarrow MATR$;
        - $CF \rightarrow SCU$;
        - $MATR \rightarrow CF$;
        - $MATR \rightarrow SCU$;
        - $SCU \rightarrow CF$;
        - $SCU \rightarrow MATR$.
- Si calcola l'insieme di copertura minimale:
    - Si elimina per transitività $MATR \rightarrow SCU$ e $SCU \rightarrow MATR$.
        - L'insieme ottenuto è una copertura minimale (non svolto).
    - Ma per transitività si potrebbero eliminare invece $CF \rightarrow MATR$ e $MATR \rightarrow CF$.
        - L'insieme ottenuto è anche in questo caso una copertura minimale equivalente al precedente (non svolto).
    - Si può ottenere un insieme di copertura minimale con il ciclo:
        - $CF \rightarrow SCU$;
        - $SCU \rightarrow MATR$.
        - $MATR \rightarrow CF$.
        - Questa è una copertura minimale più semplice ma equivalente agli altri casi (non svolto).

### Normalizzazione in 3NF

#### Esempio 12

- Si considera lo schema:
    - ESAMI(MATR, NS, DN, Co, Vo, DE, CP, NP);
    - $F = \{MATR, Co \rightarrow Vo, DE, CP; MATR \rightarrow NS, DN; CP \rightarrow NP\}$.

##### Applicazione dell'algoritmo di normalizzazione in 3NF

- *Passo 1*: si calcola la copertura minimale $F'$ di $F$.
    - Si scrive le d.f. con un solo attributo a destra:
        - $MATR \rightarrow NS$;
        - $MATR \rightarrow DN$;
        - $MATR, Co \rightarrow Vo$;
        - $MATR, Co \rightarrow DE$;
        - $MATR, Co \rightarrow CP$;
        - $CP \rightarrow NP$.
    - Non sono presenti attributi estranei né d.f. ridondanti, quindi $F$ è già minimale.
- *Passo 2*: decomposizione in sottorelazioni.
    - Da $\{MATR \rightarrow NS; MATR \rightarrow DN \}$ si ottiene:
        - $R_1: R_1(\underline{MATR}, NS, DN)$.
    - Da $\{MATR, Co \rightarrow Vo; MATR, Co \rightarrow DE; MATR, Co \rightarrow CP \}$ si ottiene:
        - $R_2: R_2(\underline{MATR, Co}, Vo, DE, CP)$.
    - Da $\{CP \rightarrow NP\}$ si ottiene:
        - $R_3: R_3(\underline{CP}, NP)$.
    - Tutte le d.f. iniziali sono rappresentate dalle chiavi primari di $R_1$, $R_2$ e $R_3$.
- *Passo 3*: si cercano relazioni i cui attributi sono presenti in altre relazioni.
    - Nessuna relazione è un sottoinsieme di un'altra.
- *Passo 4*: si cerca una relazione contente una chiave $K$ della relazione di partenza.
    - Considerando la chiave di $R_2$ si ha che $\{MATR, Co\}_F^+$ contiene tutti gli attributi di $ESAMI$.
    - Quindi è anche una chiave di $ESAMI$.
    - Non è quindi necessario aggiungere una nuova relazione che contenga $K$.
- La normalizzazione quindi termina con:
    - $R_1: R_1(\underline{MATR}, NS, DN)$;
    - $R_2: R_2(\underline{MATR, Co}, Vo, DE, CP)$;
    - $R_3: R_3(\underline{CP}, NP)$.
    - Non è necessario che si rappresentino le d.f. delle tre relazioni perché sono tutte rappresentate dalle chiavi primarie.
    - Si può constatare che tutte le d.f. sono del tipo $2$ e che quindi il nuovo schema è anche in $BCNF$.
        - Ogni relazione ha una dipendenza funzionale dove l'antecedente è una superchiave della relazione stessa.

#### Esempio 13

- Si considera lo schema:
    - CC(Titolare, NConto, NAgenzia, CittaAgenzia, Saldo)
    - $F = \{NConto \rightarrow NAgenzia; NConto \rightarrow CittaAgenzia; NConto \rightarrow Saldo \}$.

##### Applicazione dell'algoritmo di normalizzazione in 3NF

- *Passo 1*: si calcola la copertura minimale $F'$ di $F$.
    - $F$ è già una copertura minimale.
- *Passo 2*: decomposizione in sottorelazioni.
    - Da $\{NConto \rightarrow NAgenzia; NConto \rightarrow CittaAgenzia; NConto \rightarrow Saldo \}$ si ottiene:
        - $R_1: R_1(\underline{NConto}, NAgenzia, CittaAgenzia, Saldo)$.
    - Tutte le d.f. iniziali sono rappresentate dalle chiavi primari di $R_1$.
- *Passo 3*: si cercano relazioni i cui attributi sono presenti in altre relazioni.
    - Essendo presente una sola relazione, non si cercano sottoinsiemi.
- *Passo 4*: si cerca una relazione contente una chiave $K$ della relazione di partenza.
    - In $R_1$ non è presente $Titolare$ quindi sicuramente manca la chiave di $CC$.
    - Considerando $F$, la chiave di $CC$ è $(Titolare, NConto$).
    - Si aggiunge quindi la relazione:
        - $R_2(\underline{Titolare, NConto})$.
- La normalizzazione quindi termina con:
    - $R_1: R_1(\underline{NConto}, NAgenzia, CittaAgenzia, Saldo)$;
    - $R_2(\underline{Titolare, NConto})$.

#### Esempio 14

- Si considera lo schema:
    - CC(Titolare, NConto, NAgenzia, CittaAgenzia, Saldo, Direttore, Qualifica, Stipendio);
    - Con il seguente insieme di dipendenze funzionali $F'$ (già copertura minimale):
        - $NConto \rightarrow NAgenzia$;
        - $NConto \rightarrow CittaAgenzia$;
        - $NConto \rightarrow Saldo$;
        - $NAgenzia, CittaAgenzia \rightarrow Direttore$;
        - $Qualifica \rightarrow Stipendio$;
        - $Direttore \rightarrow CittaAgenzia$.

##### Applicazione dell'algoritmo di normalizzazione in 3NF

- *Passo 2*: decomposizione in sottorelazioni.
    - $R_1(\underline{NConto}, NAgenzia, CittaAgenzia, Saldo)$;
    - $R_2(\underline{NAgenzia, CittaAgenzia}, Direttore)$;
    - $R_3(\underline{Qualifica}, Stipendio)$;
    - $R_4(\underline{Direttore}, CittaAgenzia)$.
- *Passo 3*: si cercano relazioni i cui attributi sono presenti in altre relazioni.
    - $R_4$ è un sottoinsieme di $R_2$, si elimina quindi la prima.
    - E si aggiunge la d.f. $Direttore \rightarrow CittaAgenzia$ in $R_2$.
        - $R_2(\underline{NAgenzia, CittaAgenzia}, Direttore)$ con $Direttore \rightarrow CittaAgenzia$.
- *Passo 4*: si cerca una relazione contente una chiave $K$ della relazione di partenza.
    - Nessuna relazione contiene $Titolare$ quindi sicuramente manca una chiave per $CC$.
    - Si aggiunge quindi la relazione:
        - $R_5(\underline{Titolare, NConto, Qualifica})$.
        - Si può verificare che $(Titolare, NConto, Qualifica)$ è chiave di $CC$.
- La normalizzazione quindi termina con:
    - $R_1(\underline{NConto}, NAgenzia, CittaAgenzia, Saldo)$;
    - $R_2(\underline{NAgenzia, CittaAgenzia}, Direttore)$ con $Direttore \rightarrow CittaAgenzia$.
    - $R_3(\underline{Qualifica}, Stipendio)$;
    - $R_5(\underline{Titolare, NConto, Qualifica})$.
- Possono essere introdotte anomalie dalla d.f. $Direttore \rightarrow CittaAgenzia$.

### Entity-Relationship

#### Esempio 15 - Verifica di normalizzazione su entità

- Si considera lo schema ER:
    - *Entità*: $Prodotto(\underline{Codice}, NomeProdotto, NomeFornitore, PartitaIVA)$.
- Analizzando lo schema:
    - L'ER rappresenta la dipendenza funzionale:
        - $Codice \rightarrow NomeProdotto, PartitaIVA, NomeFornitore$.
    - Dall'analisi della realtà invece si ha che sono valide le dipendenze funzionali:
        - $Codice \rightarrow NomeProdotto$;
        - $PartitaIVA \rightarrow NomeFornitore$.
    - L'ER originale permette per esempio di avere due fornitori con la stessa partita IVA ma due nomi fornitori diversi.
        - Questo fa capire che usare una sola entità è errato e occorre seprare il prodotto dal fornitore.
- Il nuovo schema sarà composto quindi:
    - *Entità*: $Prodotto(\underline{Codice}, NomeProdotto)$, cardinalità $(1, 1)$;
    - *Entità*: $Fornitore(\underline{PartitaIVA}, NomeFornitore)$, cardinalità $(1, n)$;
    - *Associazione*: $Fornitura$ tra $Prodotto$ e $Fornitore$.

#### Esempio 16 - Verifica di normalizzazione su associazioni

- Si considera lo schema ER:
    - *Entità*: $Dipartimento$, cardinalità $(0, N)$;
    - *Entità*: $Studente$, cardinalità $(0, 1)$;
    - *Entità*: $CorsoDiLaurea$, cardinalità $(0, N)$;
    - *Entità*: $Relatore$, cardinalità $(0, N)$;
    - *Associazione*: $Tesi$, quaternaria tra le quattro entità.
- Analizzando lo schema:
    - Dall'analisi della realtà si ha che sono valide le dipendenze funzionali:
        - $Studente \rightarrow CorsoDiLaurea$;
        - $Studente \rightarrow Relatore$;
        - $Relatore \rightarrow Dipartimento$.
    - L'ER è coerente con le prime due dipendenze, ma non con la terza.
        - L'ER non impedisce di avere un relatore che segue tesi per dipartimenti diversi.
    - Si intuisce quindi che in questo caso l'associazione quaternaria è errata.
        - Si separa $Dipartimento$ in modo da rappresentare $Relatore \rightarrow Dipartimento$.
- Il nuovo schema sarà composto quindi:
    - *Entità*: $Dipartimento$, cardinalità $(0, N)$ per $Afferenza$.
    - *Entità*: $Studente$, cardinalità $(0, 1)$;
    - *Entità*: $CorsoDiLaurea$, cardinalità $(0, N)$;
    - *Entità*: $Relatore$, cardinalità $(0, N)$ per $Tesi$, $(1, 1)$ per $Afferenza$;
    - *Associazione*: $Tesi$, ternaria tra $Studente$, $CorsoDiLaurea$ e $Relatore$;
    - *Associazione*: $Afferenza$ tra $Relatore$ e $Dipartimento$.
 - Analizzando il nuovo schema:
     - Si può notare che i concetti nel conseguente delle prime due d.f. ($CorsoDiLaurea$ e $Relatore$) non sono così strettamente correlati.
     - Infatti uno studente è iscritto a un corso di laurea anche se non è ancora un tesista.
     - È quindi opportuno decomporre ulteriormente l'associazione $Tesi$.
 - Lo schema finale sarà composto quindi:
    - *Entità*: $Dipartimento$, cardinalità $(0, N)$ per $Afferenza$.
    - *Entità*: $Studente$, cardinalità $(0, 1)$ per $Tesi$, $(1, 1)$ per $Iscrizione$.
    - *Entità*: $CorsoDiLaurea$, cardinalità $(0, N)$;
    - *Entità*: $Relatore$, cardinalità $(0, N)$ per $Tesi$, $(1, 1)$ per $Afferenza$;
    - *Associazione*: $Iscrizione$ tra $Studente$ e $CorsoDiLaurea$;
    - *Associazione*: $Tesi$ tra $Studente$, e $Relatore$;
    - *Associazione*: $Afferenza$ tra $Relatore$ e $Dipartimento$.
 - Analizzando lo schema finale:
     - In questo modo si conservano tutte le dipendenze funzionali.
     - E inoltre si può rappresentare il vincolo (non rappresentato dalle d.f.) che uno studente è iscritto obbligatoriamente a un corso di laurea anche se non sta ancora svolgendo una tesi.

-----

### Esercitazione

#### Esercizio 1

- Si considera la relazione:
    - $R(A, B, C, D, E, F, G)$;
    - $F = \{A, B \rightarrow E, F; B \rightarrow G; A \rightarrow C,D,G; C \rightarrow B; E \rightarrow F \}$.
- Svolgere:
    - Trovare le chiavi della relazione $R$;
    - Calcolare un insieme di copertura minimale di $F$;
    - Dire se $R$ è in $3NF$ e se non lo è decomporla.

##### Svolgimento

- Trovare le chiavi della relazione $R$.
    - Si deve trovare uno o più attributi la cui chiusura copre tutto $R$.
    - $B^+ = \{B, G\}$.
        - $B$ non è una superchiave.
    - $A^+ = \{A, C, D, G, B, E, F\} = F$.
        - $A$ è una superchiave.
        - $A$ è inoltre una chiave (superchiave minimale).
    - $A$ non è mai presente nei conseguenti delle dipendenze funzionali.
        - Non è quindi deducibile da altre dipendenze funzionali.
        - Ogni chiave deve quindi contenere $A$.
        - Non ci sono quindi altre chiavi.
- Calcolare un insieme di copertura minimale di $F$.
    - Si porta in forma normale l'insieme di d.f. $F$:
        - $A, B \rightarrow E$;
        - $A, B \rightarrow F$;
        - $B \rightarrow G$;
        - $A \rightarrow C$;
        - $A \rightarrow D$;
        - $A \rightarrow G$;
        - $C \rightarrow B$;
        - $E \rightarrow F$.
    - Si eliminano gli attributi estranei:
        - Si valuta la d.f. $A, B \rightarrow E$:
            - $B$ è un attributo estraneo perché $A$ è una chiave e sicuramente da $A$ soltanto si può ricavare $E$.
            - La nuova d.f. è quindi $A \rightarrow E$.
        - Si valuta la d.f. $A, B \rightarrow F$:
            - $B$ è un attributo estraneo perché $A$ è una chiave e sicuramente da $A$ soltanto si può ricavare $F$.
            - La nuova d.f. è quindi $A \rightarrow F$.
        - Tutte le altre d.f. hanno un singolo attributo nell'antecedente.
            - Con un solo attributo non si può avere attributi estranei.
    - Si eliminano le d.f. ridondanti:
        - Si valuta la d.f. $A \rightarrow E$:
            - La si elimina ottenendo $F^* = F \setminus \{A \rightarrow E\}$.
            - Si calcola $A^+_{F^*}$ e si verifica se si riesce a ricavare $E$.
            - $A^+_{F^*} = \{A, F, C, D, G, B\}$.
                - Non è possibile ricavare $E$.
                - Inoltre $E$ non è nel conseguente di nessuna d.f., quindi non è possibile ricavarlo.
            - $A \rightarrow E$ non è quindi una d.f. ridondante.
        - Si valuta la d.f. $A \rightarrow F$:
            - La si elimina ottenendo $F^* = F \setminus \{A \rightarrow F\}$.
            - Si calcola $A^+_{F^*}$ e si verifica se si riesce a ricavare $F$.
            - $A^+_{F^*} = \{A, E, C, D, G, B, F\} = A$.
                - È possibile ricavare $F$.
            - $A \rightarrow F$ è quindi una d.f. ridondante e può essere eliminata.
        - Si valuta la d.f. $B \rightarrow G$:
            - La si elimina ottenendo $F^* = F \setminus \{B \rightarrow G\}$.
            - Si calcola $B^+_{F^*}$ e si verifica se si riesce a ricavare $G$.
            - $B^+_{F^*} = \{B\}$.
                - $B$ non è presente in nessun altro antecedente.
                - Non è possibile ricavare $G$.
            - $B \rightarrow G$ non è quindi una d.f. ridondante.
        - Si valuta la d.f. $A \rightarrow C$:
            - $C$ non è presente in nessun altro conseguente.
            - $A \rightarrow C$ è quindi l'unico modo per ricavarlo.
            - $A \rightarrow C$ non è quindi una d.f. ridondante.
        - Si valuta la d.f. $A \rightarrow D$:
            - $D$ non è presente in nessun altro conseguente.
            - $A \rightarrow D$ è quindi l'unico modo per ricavarlo.
            - $A \rightarrow D$ non è quindi una d.f. ridondante.
        - Si valuta la d.f. $A \rightarrow G$:
            - La si elimina ottenendo $F^* = F \setminus \{A \rightarrow G\}$.
            - Si calcola $A^+_{F^*}$ e si verifica se si riesce a ricavare $G$.
            - $A^+_{F^*} = \{A, E, C, D, B, F, G\} = A$.
                - È possibile ricavare $G$.
            - $A \rightarrow G$ è quindi una d.f. ridondante e può essere eliminata.
        - Si valuta la d.f. $C \rightarrow B$:
            - La si elimina ottenendo $F^* = F \setminus \{C \rightarrow B\}$.
            - Si calcola $C^+_{F^*}$ e si verifica se si riesce a ricavare $B$.
            - $C^+_{F^*} = \{C\}$.
                - Non è possibile ricavare $G$.
            - $C \rightarrow G$ non è quindi una d.f. ridondante.
         - Si valuta la d.f. $E \rightarrow F$:
            - La si elimina ottenendo $F^* = F \setminus \{E \rightarrow F\}$.
            - Si calcola $E^+_{F^*}$ e si verifica se si riesce a ricavare $E$.
            - $E^+_{F^*} = \{E\}$.
                - Non è possibile ricavare $F$.
            - $E \rightarrow F$ non è quindi una d.f. ridondante.
   - La copertura minimale è quindi:
       - $\{A \rightarrow E; B \rightarrow G; A \rightarrow C; A \rightarrow D; C \rightarrow B; E \rightarrow F \}$.
- Dire se $R$ è in $3NF$ e se non lo è decomporla.
    - Verificare che $R$ sia in $3NF$.
        - In $A \rightarrow E$, $A$ è una superchiave e rispetta la $3NF$.
        - In $B \rightarrow G$:
            - $G$ non è un sottoinsieme di $B$;
            - $G$ non è una superchiave.
            - $G$ non è un attributo primo (solo $A$ lo è).
        - Quindi $R$ non è in $3NF$.
    - Si procede quindi alla decomposizione in sottorelazioni:
        - $R_1(\underline{A}, E, C, D)$;
        - $R_2(\underline{B}, G)$;
        - $R_3(\underline{C}, B)$;
        - $R_4(\underline{E}, F)$.
        - Si valuta se qualche relazione è un sottoinsieme di altre (no).
        - $A$ (chiave relazione originale) è già contenuta in una sottorelazione quindi non è necessaria aggiungerla.
        - La decomposizione trovata è quindi corretta.
            - La decomposizione è inoltre in $BCNF$.

#### Esercizio 2

- Si considera la relazione:
    - SCONTRINO(Tessera, Codice, Negozio, DataVendita, Prezzo, Sconto, Costo);
    - Con il seguente insieme di dipendenze funzionali $F$:
        - $Codice, Negozio, DataVendita \rightarrow Prezzo$;
        - $Tessera \rightarrow Negozio$;
        - $Tessera, Negozio, Codice \rightarrow Sconto$;
        - $Prezzo, Sconto \rightarrow Costo$.
- Svolgere:
    - Determinare la o le chiavi di $SCONTRINO$;
    - Normalizzare la relazione in $3NF$;
    - Verficare se il risultato è in $BCNF$.

##### Svolgimento

- Determinare la o le chiavi di $SCONTRINO$.
    - Guardando i conseguenti si possono inferire informazioni su quali attributi possono essere la o le chiavi.
        - $\{Pr, Ne, Sc, Co\}$ compaiono nei conseguenti.
        - $\{Te, Cd, DV\}$ non compaiono nei conseguenti.
            - Quindi devono appartenere a ogni chiave.
    - Si calcola la chiusura su $\{Te, Cd, DV\}$:
        - $\{Te, Cd, DV\}^+ = \{Te, Cd, DV, Ne, Pr, Sc, Co\} = A$.
        - $\{Te, Cd, DV\}$ è una chiave di $R$ (l'unica).
- Calcolare un insieme di copertura minimale di $F$.
    - $F$ è già in forma canonica.
    - Si eliminano gli attributi estranei:
         - Si valuta la d.f. $Cd, Ne, DV \rightarrow Pr$:
             - $\{Ne, DV\}^+ = \{Ne, DV\}$ non contiene $Pr$, $Cd$ non è estraneo in questa dipendenza;
             - $\{Cd, DV\}^+ = \{Cd, DV\}$ non contiene $Pr$, $Ne$ non è estraneo in questa dipendenza;
             - $\{Cd, Ne\}^+ = \{Cd, Ne\}$ non contiene $Pr$, $DV$ non è estraneo in questa dipendenza.
         - Si valuta la d.f. $Te, Ne, Cd \rightarrow Sc$:
             - $\{Ne, Cd\}^+ = \{Ne, Cd\}$ non contiene $Sc$, $Te$ non è estraneo in questa dipendenza;
             - $\{Te, Cd\}^+ = \{Te, Cd, Ne, Sc\}$ contiene $Sc$, $Ne$ è estraneo in questa dipendenza;
             - $\{Te, Ne\}^+ = \{Te, Ne\}$ non contiene $Sc$, $Cd$ non è estraneo in questa dipendenza;
             - La nuova d.f. è quindi $Te, Cd \rightarrow Sc$.
        - Si valuta la d.f. $Pr, Sc \rightarrow Co$:
             - $\{Sc\}^+ = \{Sc\}$ non contiene $Co$, $Pr$ non è estraneo in questa dipendenza;
             - $\{Pr\}^+ = \{Pr\}$ non contiene $Co$, $Sc$ non è estraneo in questa dipendenza;
    - Si eliminano le d.f. ridondanti:
        - Si valuta la d.f. $Cd, Ne, DV \rightarrow Pr$:
            - La si elimina ottenendo $F^* = F \setminus \{Cd, Ne, DV \rightarrow Pr\}$.
            - Si calcola $\{Cd, Ne, Dv\}^+_{F^*}$ e si verifica se si riesce a ricavare $Pr$.
            - $\{Cd, Ne, Dv\}^+_{F^*} = \{Cd, Ne, Dv\}$.
                - Non è possibile ricavare $Pr$.
            - $\{Cd, Ne, DV \rightarrow Pr\}$ non è quindi una d.f. ridondante.
        - Valutando le altre d.f. si riscontra che nessuna delle d.f. è ridondante.
   - La copertura minimale è quindi:
       - $\{Cd, Ne, Dv \rightarrow Pr; Te \rightarrow Ne; Te, Cd \rightarrow Sc; Pr, Sc \rightarrow Co\}$.
- Si decompone in $3NF$:
    - $R_1(\underline{Cd, Ne, Dv}, Pr)$;
    - $R_2(\underline{Te}, Ne)$;
    - $R_3(\underline{Te, Cd}, Sc)$;
    - $R_4(\underline{Pr, Sc}, Co)$.
    - Si valuta se qualche relazione è un sottoinsieme di altre (no).
    - $\{Te, Cd, DV\}$ (chiave relazione originale) non è contenuta in nessuna sottorelazione quindi è necessaria aggiungerla.
        - $R_5(\underline{Te, Cd, DV})$.
    - La decomposizione è inoltre in $BCNF$.
