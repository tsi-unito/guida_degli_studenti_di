---
title: "Basi di dati - Parte III - Gestione delle transizioni"
---

# Basi di dati - Parte III

## Gestione delle transazioni

### Transazioni

- Una **transazione** è una sequenza di operazioni:
    - **Atomica** (*indivisibile*);
    - **Corretta** anche in presenza di **concorrenza**;
    - Con effetti **definitivi**.
- La **gestione delle transazioni** è una tecnica fondamentale per l'affidabilità di un DBMS.
- Il DBMS accetta le modifiche (`INSERT`, `UPDATE` e `DELETE`) solo se lo stato della base di dati successivo alla modifica risulta **corretto** rispetto ai **vincoli** (di chiave, di integrità referenziale, di dominio, etc).
- Una transazione è una **unità di programma**:
    - Ha un **begin transaction** che comunica al DBMS la richiesta di interazione con esso da parte dell'applicazione;
    - Il DBMS identifica l'inizio della transazione $T_i$ e la abbina in modo univoco con l'utente/applicazione che ne ha fatto richiesta;
    - Il DBMS, nell'ambito di $T_i$, riceve dei comandi DML in sequenza e li abbina alla transazione;
    - La terminazione dell'unità di programma è decisa dall'applicazione attraverso il comando **commit work** o **rollback work** (standardizzati dal SQL).

#### Commit work

- Il comando **commit work** chiede al DBMS di rendere effettive tutte le modifiche inviate dall'inizio della transazione $T_i$ sino al comando **commit work**.
- Il DBMS non verifica i vincoli istruzione per istruzione, ma solo alla ricezione del **commit work**.
    - Se i vincoli sono verificati, il DBMS dà corso alla variazione di stato complessiva della base di dati.
    - Se i vincoli non sono verificati, il DBMS annulla tutta la transazione, disfacendo le eventuali modifiche apportate alla base di dati, e invia un segnale di errore all'applicazione.
- Tutte le applicazioni ben ingegnerizzate devono sempre fare l'analisi degli errori inviati dal DBMS dopo la chiusura della transazione.

#### Rollback work

- Dopo aver eseguito parte della transazione l'applicazione può rendersi conto di non poter procedere alla computazione e chiedere all'applicazione di disfare tutti i comandi fin lì eseguiti
    - L'applicazione può cioè chiedere un **rollback**.

#### Proprietà delle transazioni

- Le transazioni per loro natura, devono essere brevi.
- I DBMS gestiscono le transazioni garantendo, per ogni transazione $T_i$, il soddisfacimento delle proprietà **ACID**:
    - **Atomicità**: una transazione o è eseguita **interamente** o non è eseguita **per niente**;
        - Non può lasciare la base di dati in uno stato intermedio (commit o abort/rollback).
        - Esito: `COMMIT` nel caso normale (e più frequente) o `ABORT` (causa un rollback).
    - **Consistenza**: l'esecuzione di una transazione non deve violare i vincoli della base di dati (integrità referenziale, unicità, applicativi, etc);
        - Se la transazione opera con uno stato della base di dati **corretto in ingresso**, deve garantire la **correttezza dello stato in uscita**.
        - È responsabilità (dei programmatori) della transazione mantenere la base di dati in uno stato corretto.
    - **Isolamento**: il DBMS garantisce che la **esecuzione concorrente di transazioni** sia lo stesso risultato sulla base di dati che si avrebbe se ognuna venisse eseguita da sola;
    - **Durabilità**: (o persistenza) tutte le modifiche andate a **buon fine** devono essere **persistenti**, anche in presenza di guasti.
- Queste proprietà non sono garantite da database non-relazionali (per ottenere migliori prestazioni).
