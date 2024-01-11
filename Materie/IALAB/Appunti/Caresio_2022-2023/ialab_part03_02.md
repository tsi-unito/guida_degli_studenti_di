---
title: 'Intelligenza artificiale e laboratorio - Parte III - Approcci alla cognizione'
---

# Intelligenza artificiale e laboratorio - Parte III

## Approcci alla cognizione

- **Paradigmi alla cognizione**:
    - Sistemi cognitivisti (spesso simbolici).
    - Sistemi ibridi;
    - Sistemi emergentisti.
        - Approcci connettivisti;
        - Approcci dinamici;
        - Approcci *enactive*.
            - Mettono insieme da un punto di vista tecnico approcci connettivisti e dinamici.

### Approccio cognitivista

- **Cognitivismo**: **cognition is a type of computation**.
    - Rappresentazioni:
        - Esplicite e simboliche;
        - Denotano (mapping 1 a 1) oggetti esterni;
        - Proiettano la realtà esterna nei meccanismi interni di *information processing* mentale.
        - Assunzione: accesso condiviso a tali rappresentazioni.
            - Assunzione (filosofica) di base dei sistemi simbolici.
            - Gli individui condividono una semantica comune (**stessa ontologia accessibile**) dello stesso concetto.
            - Questo permette di comunicare tra individui.

#### Physical Symbol Systems

- *Physical Symbol Systems* [Newell & Simon, 1975]:
    - *Physical Symbol Systems Hypothesis*:
        - Un PSS possiede le condizioni necessarie e sufficienti per azioni generali intelligenti;
        - Ogni sistema che esisbisce intelligenza generale è un PSS;
        - Un PSS è «a machine that produces through time an evolving collection of symbol structures».
    - La teoria computazionale adottata dagli autori per spiegare l'intelligenza di sistemi biologici e macchine.
    - Assunzioni dei PSS:
        - Dotati di **memoria** (contiene informazione simbolica);
        - Operano su **simboli**;
        - **Operazioni** (per manipolare simboli); 
        - **Interpretazioni** (permettono ai simboli di specificare operazioni).
    - I *symbol systems* possono essere istanziati su diverse implementazioni.
        - È una teoria generale dell'intelligenza, non è necessario focalizzarsi sull'*hardware*.
        - La PSSH si focalizza sul secondo livello della gerarchia di Marr.
    - L'assunto dell'operazione sui simboli è stata falsificata dalle neuroscienze per il caso del cervello:
        - A livello neuroscientifico non si hanno veri e propri simboli.
        - Ma piuttosto rappresentazioni interne che *funzionano* da simboli.
    - *Heuristic Search Hypothesis*:
        - Le soluzioni a problemi sono rappresentati come strutture simboliche.
        - Un PSS utilizza la sua intelligenza in attività di problem-solving tramite **meccanismi di ricerca**.
            - Per essere effetiva la ricerca deve essere efficiente.

#### Evoluzione del cognitivismo

- Evoluzione del cognitivismo:
    - Il cognitivismo è alla base della creazione di **risorse di conoscenza** (wikipedia, ecc).
    - Oggigiorno non si ha solo conoscenza manualmente aggiunta:
        - Machine learning;
        - Modellazione probabilistica;
        - Modelli migliori, logiche migliori, ecc.
- È legittimo chiedersi perché l'utilizzo di metodi simbolici sia utile nonostante non siano **grounded nelle neuroscienze**.
    - Possono dire qualcosa quali siano le euristiche adottate cognitivamente per risolvere gli stessi problemi.
    - Bisogna ovviamente valutare i sistemi con vincoli strutturalmente realistici.

### Approcci emergentisti

- **Approcci emergentisti**: la cognizione è un **processo emergente** che avviene tramite meccanismi di **auto-organizzazione**.
    - Non si hanno rappresentazioni esplicite.
    - Il sistema si costruisce continuamente.
    - Si opera in real-time.
    - Non si ha **una ontologia condivisa dell'ambiente** nonostante si sia nello stesso spazio.
    - **Co-determinazione**:
        - Un agente cognitivo è specificato dal suo ambiente.
        - Il sistema costruisce la sua realtà (mondo) come il risultato delle operazioni di interazione con l'ambiente.
            - I processi cognitivi determinano cosa è reale o significativo per l'agente.
        - Importanza di **aspetti percettivi**.
        - **Cognizione e percezione come co-dipendenti**.
    - La **cognizione è il complemento della percezione**.
        - Il core è la percezione, non la cognizione.
        - Importanza del corpo come elemento di interazione con l'ambiente.
        - Assunzione: i sistemi cognitivi sono **sistemi embodied**.
    - Importanza di funzioni come apprendimento.
    - Fenomeni chiave dei sistemi emergentisti: **auto-organizzazione**, **emergenza**.

#### Approcci dinamici ed enactive

- **Approcci dinamici**:
    - Un sistema dinamico è un **sistema aperto dissipativo non-lineare** e **non in equilibrio**.
        - *Sistema*: gran numero di componenti in interazione e gran numero di gradi di libertà;
        - *Dissipativo*: diffonde energia, lo spazio delle fasi decresce in volume nel tempo ($\to$ sotto-spazi preferenziali);
        - *Non-equilibrio*: incapace a mantenere struttura o funzione senza fonti esterne di energia, materiale, informazione (quindi, aperto);
        - *Non-linearità*: la dissipazione non è uniforme, piccoli numeri di gradi di libertà del sistema contrinuiscono al comportamento.
    - Si tratta di sistemi più astratti di quelli connessionisti ma meno di quelli simbolici.
    - Si possono rappresentare tramite equazioni differenziali.
- **Approcci enactive**:
    - Si tratta degli approcci emergentisti più **radicali**.
    - *Enattivisimo*: agente e ambiente si **formano insieme** (*arise together*).
    - Cinque elementi chiave del'enattivismo:
        - Autonomia (nessun controllo esterno);
        - Embodiment (corpo);
        - Emergenza (cognizione emerge dall'interazione);
        - Esperienza (interazioni passate modificano agente e ambiente);
        - *Sense-making* (capacità di auto-modifica del comportamento).
    - Fondamentale avere più possibile interazione con il mondo da parte dell'agente.
    - Ogni individuo ha una propria rappresentazione del mondo che finisce a combaciare con il mondo stesso.
        - Non si ha un'ontologia comune (come avveniva nel cognitivismo).
        - Problema di allineamento tra rappresentazioni differenti tra più agenti.

### Approcci ibridi

- **Approcci ibridi**: mettono insieme approcci di natura differenti.
    - `eg` simbolici con connettivisti, simboli con enactive, ecc.
    - Nessun paradigma da solo è in grado di replicare l'**abbondanza di funzioni cognitive** dell'intelligenza umana.
    - Si uniscono le abilità specifiche dei vari approcci.
    - Non è noto quale sia la combinazione ottimale (sempre ne esista una) per raggiungere questo obiettivo.
