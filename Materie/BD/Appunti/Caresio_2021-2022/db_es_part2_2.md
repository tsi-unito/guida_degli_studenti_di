---
title: "Basi di dati - Parte II - Esercizi - Dipendenze funzionali"
---

# Basi di dati - Parte II - Esercizi

## Dipendenze funzionali

### Esempi introduttivi

- Trovare le dipendenze funzionali nella base di dati Esami.
    - $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - $CF \rightarrow MATR$;
    - $IS \rightarrow CAP$ (nell'ipotesi di indirizzo completo);
    - $MATR, Co \rightarrow Vo, Lo, DE, CP, NP$;
    - $CP \rightarrow NP, Q$;
    - $Q \rightarrow TU$.
    - Immaginando un vincolo per qui ogni docente è titolare di un solo corso:
        - $CP \rightarrow Co$ (dipendenza funzionale usata in seguito).

### Equivalenza di insiemi di dipendenze funzionali tramite definizione

#### Esempio 1

- Verifica di equivalenza attraverso definizione di dipendenza funzionale:
    - Primo insieme $F'$:
        - $f'_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - Secondo insieme $F''$:
        - $f''_1$: $MATR \rightarrow NS, IS, CAP$;
        - $f''_2$: $MATR \rightarrow CF,DN$.
    - $F'= \{f'_1\}$ e $F''=\{f''_1, f''_2 \}$ sono equivalenti?
        - Si applica la definizione di dipendenze funzionale e si dimostra che $f''_1 \land f''_2 \implies f'_1$.
    - Applicazione della definizione di dipendenza funzionale su $f''_1 \land f''_2 \implies f'_1$:
        - $f''_1$: $\forall t_1, t_2 \in r(t_1[MATR]=t_2[MATR] \implies t_1[NS, IS, CAP]=t_2[NS, IS, CAP])$;
        - $f''_2$: $\forall t_1, t_2 \in r(t_1[MATR]=t_2[MATR] \implies t_1[CF,DN]=t_2[CF,DN])$;
        - Applicando proprietà degli operatori logici si ha:
            - $\forall \dots \: (t_1[M]=t_2[M] \implies (t_1[NS, IS, CAP]=t_2[NS, IS, CAP] \land t_1[CF,DN]=t_2[CF,DN]))$;
            - Che è equivalente a $f'_1$.
    - Applicazione della definizione di dipendenza funzionale su $f'_1 \implies f''_1 \land f''_2$:
        - Se è vero $f'_1$ lo sarà sicuramente anche per un suo sottoinsieme.
        - Quindi il rispetto di $f_1'$ implica il rispetto di $f''_1$ e $f''_2$.

#### Esempio 2

- Verifica di equivalenza attraverso definizione di dipendenza funzionale:
    - Primo insieme $F'$:
        - $f'_1$: $MATR \rightarrow NS, IS, CF, DN$;
        - $f'_2$: $IS \rightarrow CAP$;
    - Secondo insieme $F''$:
        - $f''_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
        - $f''_2$: $IS \rightarrow CAP$.
    - Si verifica che $f'_1 \land f'_2 \iff f''_1 \land f''_2$.
        - Dato che $f'_2=f''_2$ è sufficiente verificare che $f'_1 \land f'_2 \implies f''_1$ e $f''_1 \land f''_2 \implies f'_1$.
    - Utilizzando la definizione di dipendenza funzionale e proprietà degli operatori logici si conclude che i due insiemi sono equivalenti.

### Teoria di Armstrong

#### Esempio 3 - Assioma di riflessività

- Se $Y \subseteq X$ allora $X \rightarrow Y$
    - Se si considera un insieme di attributi e ne si prende un sottoinsieme, sicuramente c'è una d.f. tra i due.
    - Primo insieme $F'$:
        - $f'_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - Secondo insieme $F''$:
        - $f''_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
        - $f''_2$: $CF,Co,Q \rightarrow Co,Q$.
    - Applicando l'assioma di riflessività si può ricavare $f''_1$ e $f''_2$ partendo da $f'_1$.
        - E si può quindi stabilire che $F'$ e $F''$ sono equivalenti.

#### Esempio 4 - Assioma di unione

- Se $X \rightarrow Y$ e $X \rightarrow Z$ allora $X \rightarrow YZ$ (dove $YZ = Y \cup Z$).
    - Primo insieme $F'$:
        - $f'_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - Secondo insieme $F''$:
        - $f''_1$: $MATR \rightarrow NS, IS, CAP$;
        - $f''_2$: $MATR \rightarrow CF,DN$.
    - Applicando l'assioma di unione si può ricavare $f_1'$ partendo da $f_1''$ e $f''_2$.
        - E si può quindi stabilire che $F'$ e $F''$ sono equivalenti.

#### Esempio 5 - Assioma di transitività

- Se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $X \rightarrow Z$.
    - Primo insieme $F'$:
        - $f'_1$: $MATR \rightarrow NS, IS, CF, DN$;
        - $f'_2$: $IS \rightarrow CAP$;
    - Secondo insieme $F''$:
        - $f''_1$: $MATR \rightarrow NS, IS, CAP, CF, DN$;
        - $f''_2$: $IS \rightarrow CAP$.
    - Applicando l'assioma di transitività si può ricavare $f_1''$ partendo da $f'_1$ e $f'_2$.
        - E si può quindi stabilire che $F'$ e $F''$ sono equivalenti.

#### Esempio 6 - Regole aggiuntive

- *Regola dell'espansione*: data una d.f. $X \rightarrow Y$ e un insieme di attributi $W$, allora $WX \rightarrow WY$.
    - Si considera la dipendenza funzionale $Q \rightarrow TU$ e l'attributo $CF$;
    - Allora si può ottenere $CF, Q \rightarrow CF, TU$.
- *Regola di decomposizione*: se $X \rightarrow YZ$ allora $X \rightarrow Y$$X \rightarrow Z$.
    - Si considera la dipendenza funzionale $MATR \rightarrow CF, DN$;
    - Allora si può ottenere $MATR \rightarrow CF$ e $MATR \rightarrow DN$.
- *Regola di pseudo-transitività*: se $X \rightarrow Y$ e $WY \rightarrow Z$, allora $WX \rightarrow Z$.
    - Si considerano le dipendenze funzionali $CF \rightarrow MATR$ e $Co, MATR \rightarrow CP$;
    - Allora si può ottenere $Co, CF \rightarrow CP$.
- *Regola del prodotto*: data $X \rightarrow Y$ e $W \rightarrow Z$, allora $XW \rightarrow YZ$.
    - Si considerano le dipendenze funzionali $CF \rightarrow MATR$ e $CP \rightarrow NP$;
    - Allora si può ottenere $CF, CP \rightarrow MATR, NP$.

### Chiusura di un insieme di attributi

#### Esempio 7

- Si considera $X = \{MATR \}$ e ne si calcola la chiusura $X_F^+$ seguendo la definizione.
    - Si valutano le dipendenze $F$:
        - $MATR \rightarrow NS, IS, CAP, CF, DN$;
        - $CF \rightarrow MATR$;
        - $IS \rightarrow CAP$;
        - $MATR, Co \rightarrow Vo, DE, CP, NP$;
        - $CP \rightarrow NP, Q$;
        - $Q \rightarrow TU$;
        - $CP \rightarrow Co$.
    - Calcolo di $\{MATR\}_F^+$:
        - Include $MATR$ stesso per l'assioma di riflessività.
        - Da $MATR \rightarrow NS, IS, CAP, CF, DN$ si deduce quindi che $NS, IS, CAP, CF, DN \in X^+$;
        - Le ulteriori dipendenze in $F$ non aggiungono ulteriori attributi.
        - $\{MATR\}_F^+ = \{ MATR, NS, IS, CAP, CF, DN  \}$.

#### Esempio 8 - Algoritmo per il calcolo della chiusura di un insieme di attributi

- Si vuole calcolare $\{CP\}^+$.
    - Si parte con $X = \{CP\}$.
    - $C = {CP}$ e $F' = F$;
    - Si cerca una d.f. in $F'$ con antecedente in $\{CP\}$.
        - $CP \rightarrow NP, Q$;
        - $C = \{CP, NP, Q\}$.
    - Si cerca una d.f. con antecedente in $\{CP, NP, Q\}$.
        - $Q \rightarrow TU$;
        - $C = \{CP, NP, Q, TU\}$.
    - Si cerca una d.f. con antecedente in $\{CP, NP, Q, TU\}$.
        - $CP \rightarrow Co$;
        - $C = \{CP, NP, Q, TU, Co\}$.
    - Si cerca una d.f. con antecedente in $\{CP, NP, Q, TU, Co\}$.
        - $MATR, Co \rightarrow Vo, DE, CP, NP$ non è valida perché tutti gli antecedenti devono essere presenti in $C$.
        - Perciò l'algoritmo termina.
    - $\{CP\}^+ = \{CP, NP, Q, TU, Co \}$.


#### Esempio 9

- Dato $F$ si verifica se la dipendenza $MATR \rightarrow CAP$ appartiene a $F^+$.
    - Si vuole sapere se è possibile derivare $MATR \rightarrow CAP$ in $F$.
    - Per far ciò si ricava $\{MATR\}_F^+$ e si controlla se $CAP \in \{MATR\}_F^+$.
    - Precedentemente si è calcolato $\{MATR\}_F^+ = \{ MATR, NS, IS, CAP, CF, DN  \}$.
    - Si può quindi concludere che $MATR \rightarrow CAP$ è una dipendenza valida nel sistema delle dipendenze $F$.
- Dato $F$ si verifica se la dipendenza $MATR \rightarrow Vo$ appartiene a $F^+$.
    - Applicando l'algoritmo di chiusura su $X = \{MATR\}$ si ottiene $\{MATR\}_F^+ = \{ MATR, NS, IS, CAP, CF, DN  \}$.
    - $Vo$ non è incluso in $X^+$ quindi $MATR \rightarrow Vo$ non è deducibile da $F$.

### Equivalenza di insiemi di dipendenze funzionali

#### Esempio 10

- Si valuta l'insieme di dipendenze $F$:
    - $MATR \rightarrow NS, IS, CAP, CF, DN$;
    - $CF \rightarrow MATR$;
    - $IS \rightarrow CAP$;
    - $MATR, Co \rightarrow Vo, DE, CP, NP$;
    - $CP \rightarrow NP, Q$;
    - $Q \rightarrow TU$;
    - $CP \rightarrow Co$.
- Si valuta l'insieme di dipendenze $G$:
    - $MATR \rightarrow NS, IS$;
    - $MATR \rightarrow DN, CF$;
    - $CF \rightarrow MATR$;
    - $IS \rightarrow CAP$;
    - $MATR, Co \rightarrow Vo, Lo, DE$;
    - $MATR, Co, Ns \rightarrow CP$;
    - $CP \rightarrow NP, Q$;
    - $Q \rightarrow TU$;
    - $CP \rightarrow Co$.
- Si controlla se ogni vincolo di $F$ può essere derivato in $G$.
    - Diversi vincoli sono uguali in $F$ e in $G$, questi banalmente apparterranno alla chiusura di entrambi.
    - Si verifica quindi le seguenti d.f. in $F$:
        - $MATR \rightarrow NS, IS, CAP, CF, DN$;
        - $MATR, Co \rightarrow Vo, Lo, DE, CP, NP$.
    - Si controlla se $MATR \rightarrow NS, IS, CAP, CF, DN$ in $F$ appartiene a $G^+$.
        - Ovvero si controlla se dato $X = \{MATR\}$ e $Y = \{NS, IS, CAP, CF, DN\}$ allora $X \rightarrow Y \in G^+$.
        - Si calcola $\{X\}_G^+$ tramite l'algoritmo: $\{X\}_G^+ = \{MATR, NS, IS, DN, CF, CAP\}$.
        - $Y \subseteq \{X\}_G^+$, quindi sì $MATR \rightarrow NS, IS, CAP, CF, DN$ in $F$ appartiene a $G^+$.
    - Si controlla se $MATR, Co \rightarrow Vo, Lo, DE, CP, NP$ in $F$ appartiene a $G^+$.
        - Ovvero si controlla se dato $X = \{MATR, Co\}$ e $Y = \{Vo, Lo, DE, CP, NP\}$ allora $X \rightarrow Y \in G^+$.
        - Si calcola $\{X\}_G^+$ tramite l'algoritmo: $\{X\}_G^+ = A$ (tutti gli attributi).
        - $Y \subseteq \{X\}_G^+$, quindi sì $MATR, Co \rightarrow Vo, Lo, DE, CP, NP$ in $F$ appartiene a $G^+$.
    - Si può quindi affermare che $F^+ \subseteq G^+$.
- Per confermare l'equivalenza $F \equiv G$ si deve infine se ogni vincolo di $G$ può essere derivato in $F$.
    - Si deve quindi poter affermare che $G^+ \subseteq F^+$.

### Dipendenze funzionali e superchiavi

#### Esempio 11

- Si valuta l'insieme di dipendenze $F$ visto in precedenza.
- $MATR$ è una superchiave della relazione $ESAMI$?
    - Si è calcolato in precedenza che $\{MATR\}_F^+ = \{ MATR, NS, IS, CAP, CF, DN  \}$.
    - $\{MATR\}_F^+ = \{ MATR, NS, IS, CAP, CF, DN  \} \neq A$ quindi $MATR$ non può essere superchiave.
- $\{MATR, Co, NS\}$ è una superchiave della relazione $ESAMI$?
    - Applicazione dell'algoritmo con $X = \{MATR, Co, NS\}$:
        - $C = \{ MATR, NS, NS, IS, CAP, CF, DN, Co\}$ (riflessività e calcoli pregressi);
        - $C = \{ MATR, NS, IS, CAP, CF, DN, Co, Vo, Lo, DE, CP\}$;
        - $C = \{ MATR, NS, IS, CAP, CF, DN, Co, Vo, Lo, DE, CP, NP, Q\}$;
        - $C = \{ MATR, NS, IS, CAP, CF, DN, Co, Vo, Lo, DE, CP, NP, Q, TU\}$.
    - $X^+ = A$ quindi $X \rightarrow A$.
    - Quindi $X = \{MATR, Co, NS\}$ è una superchiave.
- Si vuole calcolare se $\{MATR, Co, NS\}$ è una chiave (superchiave minimale).
    - Si valuta la chiusura di $\{Co, Ns\}$ ma la proprietà di superchiave non vale (non svolto).
    - Si valuta la chiusura di $\{MATR, Co\}$ e si verifica che è una superchiave (non svolto).
    - Si è calcolato che $MATR$ non è superchiave, quindi $\{Matr, Co\}$ è minimale ed è una chiave candidata.
- Possono esistere varie chiavi candidate per la stessa relazione e lo stesso insieme di dipendenze funzionali:
    - $\{MATR, Co\}$;
    - $\{CF, Co\}$;
    - $\{MATR, CP\}$;
    - $\{CF, CP\}$.
- Si verifica che $K = \{MATR, CP \}$.
    - La chiusura di $MATR$ dà $\{MATR, NS, IS, CAP, CF, DN\}$.
    - La chiusura di $CP$ dà $\{CP, NP, Q, Co\}$.
    - Si ottiene $\{MATR, Co\}$ come visto precedentemente.
    - $MATR$ e $CP$ non sono superchiavi quindi $K = \{Matr, CP\}$ è una chiave.

#### Esempio 12

- Si valuta l'insieme di dipendenze $F$ definite sulla base di dati *ContiCorrente*:
    - $NCC \rightarrow NA, CA$;
    - $NCC \rightarrow SCC, DM$;
    - $NA, CA \rightarrow DA$ (il direttore è l'unico direttore dell'agenzia);
    - $DA \rightarrow CA$ (un direttore non può dirigere agenzie in città diverse).
- Si vuole calcolare le chiavi candidate.
    - L'attributo $T$ non appare nel conseguente di nessuna dipendenza funzionale.
        - Infatti un stesso conto corrente può essere intestato a più persone (non si ha quindi $NCC \rightarrow T$).
        - È impossibile quindi che $T$ compaia nella chiusura di qualsiasi insieme di attributi $X$.
            - A meno che non faccia già parte di $X$.
        - Di conseguenza $T$ **deve fare parte** di ogni chiave candidata.
    - L'attributo $T$ non appare inoltre nell'antecedente di nessuna dipendenza funzionale.
        - La sua chiusura quindi comprende solo $T$ stesso.
        - Da solo non può essere quindi una superchiave.
- Una possibile chiave candidata potrebbe essere $K = \{T, NCC\}$ (non svolto).
