---
title: 'Etica, Società e Privacy - Privacy - Parte III - Statistical Disclosure Control'
---

# Etica, Società e Privacy - Privacy - Parte III

## Statistical Disclosure Control

### Rilascio dei dati

- **Rilascio dei dati**:
    - Internet e dalle nuove tecnologie permettono nuove e migliori modalità di raccolta e rilascio dei dati.
        - Web usage data, social media data, smartphone, wearables, ecc.
    - Questi dati pongono rischi di privacy, ma possono essere anche utilizzati a fini nobili.
    - Nuovi e sofisticati metodi di analisi dei dati:
        - Machine learning, data mining, statistical inference, big data analytics, ecc.
    - **Statistical data disseminations**:
        - (Linked) open data, dataset per la riproducibilità di esperimenti, contest, policy e regolazioni.
        - I dati rilasciati possono essere usati per **inferire informazioni** non intese per il rilascio.
    - Una **disclosure** può avvenire sfruttando dati rilasciati con quelli presenti pubblicamente su altre piattaforme.
        - `eg` Utilizzando i social network e i social media.
        - Quando dei dati vengono rilasciati, il **disclosure risk** deve essere molto basso.

#### Tipi di dati

- **Tipi di dati**:
    - *Macrodati* e *microdati*:
        - **Macrodati**: dati rilasciati in forma tabellare in database statistici (passato).
        - **Microdati**: record specifici di singole persone (richiesti oggi).
            - Considerati più a rischio in relazione a breach.
    - **Respondents**: business, autorità, persone, ecc, le cui informazioni sono raccolte a fini statistici.
        - I problemi di privacy riguardano le persone, per le organizzazioni si hanno **problemi di confidenzialità**.
            - I primi sono normati dalla legge (*reato*, più grave), i secondi no.
            - Le informazioni confidenziali sono confidenziali perché è l'organizzazione che le definisce tali.
    - **Macrodati** (aggregati):
        - Tabelle **count/frequency**: ogni cella contiene il numero di *respondents* (*count*) o la loro percentuale (*frequency*) che ha lo stesso valore su tutti gli attributi dell'analisi associata alla tabella.
        - Tabelle **magnitude**: ogni cella contiene un valore aggregato di una quantità di interesse su tutti gli attributi dell'analisi associata alla tabella.
    - **Disclosure sui microdati**:
        - I maggiori requisiti che devono essere presi in considerazione sono:
            - Identity disclosure protection.
            - Attribute disclosure protection.
            - Inference channel.

#### Disclosure

- **Information disclosure**: divulgazione di informazioni non autorizzate.
    - Una *disclosure* riguarda un'appropriazione impropria di informazioni a un respondent.
    - Una *disclosure* può avvenire in tre casi:
        - **Identity disclosure**: un *respondent* è identificato tramite dati rilasciati.
        - **Attribute disclosure**: informazioni sensibili su un *respondent* rivelate tramite dati rilasciati.
        - **Inferential disclosure**: i dati rilasciati permettono di determinare valori di alcune caratteristiche di un *respondent* in maniera più accurata di quanto possibile altrimenti.
            - Questa disclosure determina le altre due utilizzando un **canale di inferenza statistico**.

##### Identity disclosure

- **Identity disclosure**:
    - Quando una terza parte (tipicamente non autorizzata) riesce a identificare un soggetto o un respondent tramite i dati rilasciati.
        - Rivelare che un individuo è un *respondent* o un soggetto di una collezione di dati può o no violare requisiti di confidenzialità.
        - Può essere però un problema da un punto di vista della privacy.
    - Identity disclosure nei **macrodati**:
        - Rivelare un'identità tipicamente non è un problema, trattandosi di dati aggregati.
        - A meno che un'identificazione porti al rilascio di informazioni confidenziali (attribute disclosure).
    - Identity disclosure nei **microdati**:
        - L'identificazione è considerata generalmente un problema.
        - L'identity disclosure tipicamente implica anche un attribute disclosure.

##### Attribute disclosure

- **Attribute disclosure**:
    - Quando informazioni confidenziali su un *respondent* sono rivelante e possono essere attribuite a esso.
        - Collegamento di una singola persona con informazioni che dovrebbero essere confidenziali.
    - Può accadere quando informazioni confidenziali sono rivelate con certezza o stimate accuratamente.

##### Inferential disclosure

- **Inferential disclosure**:
    - Quando informazioni possono essere inferite con alta confidenza da **proprietà statistiche** dei dati rilasciati.
    - Modalità più complessa delle altre.
    - Difficile considerare questo tipo di disclosure in quanto:
        - Se la disclosure è equivalente all'inferenza, i dati non potrebbero essere rilasciati.
        - Le inferenze sono progettate per predire comportamenti aggregati, non singoli attributi.
            - Sono quindi spesso cattivi predictors per valori di dati singoli. 

#### Restricted data e restricted access

- **Restricted data** e **restricted access**:
    - La scelta di metodi per la **statistical disclosure limitation** dipende dalla natura del *data product*.
    - Alcuni microdati includono identificativi espliciti (`eg` nome, numeri telefonici, email, SSN, ecc).
        - Rimuovere gli identificativi è il primo step per un rilascio di microdati.
        - Anche rimuovendo gli identificatori, non è detto che la privacy sia mantenuta.
    - Si può o **limitare l'accesso** o **limitare le informazioni** stesse.
        - *Restricted data*: restringere la quantità di informazioni nelle tabelle e nei microdata rilasciati.
        - *Restricted access*: imporre condizioni di accesso ai data products.
        - Queste due strategie possono essere combinate in vario modo.

### Statistical Disclosure Control

- **Statistical Disclosure Control**:
    - Insieme di metodi usati come parte di processi di anonimizzazione per limitare il rischio di re-identification e attribute disclosure attraverso la manipolazione dei dati.
    - **Disclosure control non è privacy**.
        - Tutti i processi che provano a valutare i rischi di disclosure sono primariamente **processi tecnici di confidenzialità**.
        - Processi che sono indirettamente e imperfettamente possono essere mappati su protezioni della privacy.
        - Applicabile a qualsiasi tipo di dato, non solo a quelli privati di persone fisiche.
    - Si ha sempre un **tradeoff tra rischio e utilità**.
        - Rilasciare i dati originali, non modificati, ha un'utilità molto alta ma anche un rischio di disclosure altissimo.
        - Si deve raggiungere un compromesso, considerando una *threshold* (difficilmente determinabile).
    - Procedura tipica di SDC:
        - Determinare quali dati sono da proteggere.
        - Applicare il metodo di SDC (+ scegliere uno o più parametri di scelta euristica).
        - Misurare l'utilità dei nuovi dati.
        - Misurare il rischio di disclosure con i nuovi dati.

#### Metodi di Statistical Disclosure Control

- **Metodi di Statistical Disclosure Control**:
    - Macrodati.
    - Microdati.
    - Output di query su database.
        - I dati non sono rilasciati in un file, ma attraverso delle interrogazioni.

##### Metodi di Statistical Disclosure Control per i macrodati

- **Metodi di Statistical Disclosure Control per i macrodati**:
    - Metodi **non-perturbativi**: non modificano i valori nelle celle.
        - *Cell suppression* (CS):
            - Nei dati tabulari, soppressione primaria e soppressione complementare (secondaria).
            - **Primary suppression**: rimozione di valori a rischio.
                - `eg` Se la cella fa riferimento a una piccola parte della popolazione, quella va soppressa.
                - La rimozione di questi valori non garantisce la protezione dai dati.
            - **Complementary (secondary) suppression**: rimozione addizionale di celle non-rischiose.
                - Così da garantire il livello di protezione desiderato per le celle rischiose.
                - Funzionali alle soppressioni primarie.
                - L'ideale è limitare questo tipo di soppressione.
    - Metodi **perturbativi**: modificano i valori nelle celle.
        - **Random rounding** (RR): non si adottano convenzioni di arrotondamento standard.
        - **Controlled rounding** (CR):
            - L'arrotondamento avviene su ogni cella, ma deve rispettare i totali marginali.
            - Si applicano algoritmi di programmazione lineare.
        - **Controlled tabular adjustment** (CTA):
            - I valori sensibili vengono sostituiti dai valori sicuri più vicini (in base alla semantica delle celle).
            - Piccoli aggiustamenti vengono fatti alle altre celle per ripristinare l'additività della tabella.
            - Anche in questo caso i marginali vengono preservati.

##### Metodi di Statistical Disclosure Control per i database

- **Metodi di Statistical Disclosure Control per i database**:
    - **Query perturbation**:
        - **Input**: distorsione applicata sui record del DB.
        - **Output**: distorsione applicata sul risultato della query.
    - **Query restriction**:
        - Rifiutare di rispondere a certe query.
            - Per esempio su query troppo stringenti, su aggregati di dati troppo piccoli.
        - Necessario tenere traccia delle query precedenti.

##### Metodi di Statistical Disclosure Control per i microdati

- **Metodi di Statistical Disclosure Control per i microdati**:
    - **Data masking**: si genera una versione modificata del database originale.
        - **Perturbative masking**:
            - **Noise addition**:
                - Si aggiunge a ogni record un vettore di rumore (applicabile a microdati numerici).
                - Media e correlazioni tra dati possono essere preservati (tramite vincoli).
            - **Microaggregation**:
                - Si partizionano i record in gruppi (basati su una similarità intra-gruppo).
                - Si pubblica il record medio di ogni gruppo.
            - **Data swapping**:
                - Valori di ogni attributo vengono *ranked* in ordine ascendente.
                - *Swap* di un valore con un altro valore *ranked* scelto casualmente in un range ristretto.
            - **Post randomization**:
                - Funziona su valori di tipo categoriale.
                - Attributi confidenziali sono cambiati basandosi su una matrice stocastica data. 
                - `eg` Markov Chain.
        - **Non-perturbative masking**:
            - **Sampling**:
                - Si pubblica un campione del dataset originale (rimuovendo ciò che è identificativo).
            - **Generalization**:
                - Attributi categoriali sono combinati in categorie meno specifiche.
                - Attributi numerici sono sostituiti da intervalli (anche standardizzati).
            - **Top/bottom coding**:
                - Valori sotto o sopra una soglia vengono settati al valore *top*/*bottom*.
            - **Local suppression**:
                - Solo alcuni valori individuali sono soppressi.
                - Si aumentano gli insiemi di record che hanno la stessa combinazione di valori su certi attributi.
    - **Data synthesis**: si elabora il database originale e se ne crea uno sintetico.
        - Il nuovo database, generato casualmente, preserva le proprietà pre-selezionate di quello originale.
        - **Obiettivo**: rilasciare dataset che mantengono la confidenzialità dei data subject.
        - **Metodi**:
            - **Fully synthetic data**: si sintetizza l'intero dataset.
            - **Partially synthetic data**: si sintetizzano solo le variabili sensibili.
        - **Problematiche**:
            - I dati sintetici devono fornire comunque analisi statistiche sensate.
            - I dati sintetici dipendono dal modello usato.
            - **Overfitting**: record sintetici troppo simili agli originali permettono re-identificazione. 

#### Utilità

- **Misure di utilità**:
    - Non è facile stabilirle perché non è chiaro cosa gli utenti vogliano fare con i dati.
        - `eg` Fare modelli di ML predittivi.
    - **Perdita d'informazione**: tenta di cogliere termini di teoria dell'informazione del cambiamento di informazione causato dal controllo del disclosure.
        - Si vuole calcolare la perdita d'informazione tra il dataset originale e quello perturbato/sintetico.
- **Information loss per i macrodati**:
    - *Cell suppression*: numero di valori modificati dalla soppressione secondaria.
    - *Rounding e tabular adjustment*: somma delle distanze tra celle vere e perturbate.
        - Possibilmente pesate dal costo della cella se non tutte le celle hanno pari importanza.
- **Information loss per i database**:
    - *Query perturbation*: la differenza tra risposta della query vera e di quella perturbata.
        - `eg` Caratterizzata come media e varianza del rumore aggiunto (idealmente media $0$ e varianza piccola).
    - *Query restriction*: numero di query rifiutate.
        - Non è una misura di teoria dell'informazione, ma da un'idea dell'informazione persa. 
- **Information loss per i microdati**:
    - **Data use-specific loss measures**: valutano la portata in cui il SDC influenza l'output di particolari analisi. 
    - **General information loss measures**:
        - Statistica base: media, covarianza, correlazione, ecc.
        - Score (`eg` *propensity score*).
        - Distanza tra i valori originali e quelli controllati usando misure (`eg` divergenza Jensen-Shannon, divergenza Kullback-Leibler, ecc).

### Analisi del rischio ex-ante

- Quelli utilizzati sono metodi per **analisi dei rischi ex-post**.
    - Secondo questi metodi, a priori non si può stimare la probabilità di identificazione dei data subject.
    - Il **rischio ex-ante** si basa sulla nozione di **modello di privacy**.
        - Una condizione, dipendente da uno o più parametri, che garantisce un **limite superiore** al rischio di re-identificazione.
        - `eg` In una data situazione, non ci si può permettere un rischio maggiore del $10\%$.
        - Nei metodi precedenti, la privacy era un effetto secondario della confidenzialità.
            - Nel caso ex-post, quando cambia il dataset va rieseguita l'analisi del rischio.
            - Nell'ex-ante, quando cambia il dataset dato l'algoritmo e il parametro, si ha quella garanzia di privacy.
    - Metodi ex-ante:
        - $k$-anonimity.
        - $l$-diversity.
        - $t$-closeness.
        - $\delta$-presence.
        - Differential privacy.
