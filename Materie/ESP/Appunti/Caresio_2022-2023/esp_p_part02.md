---
title: 'Etica, Società e Privacy - Privacy - Parte II - Privacy, big data e sistemi informativi'
---

# Etica, Società e Privacy - Privacy - Parte II

## Privacy e big data

### Privacy nella contemporaneità

- Privacy nella contemporaneità:
    - Nel 1996, il $24\%$ degli statunitensi affermava di aver esperito un'*invasione della privacy*.
        - Nel 1978, era il $19\%$.
    - Privacy e *convenienza* (trade off).
        - Negli anni Sessanta, alcuni affermavano che l'inquinamento era necessario per lo sviluppo.
        - È possibile avere tecnologie informatiche *convenienti* e che rispettino la privacy.
    - La privacy è relativa all'**possesso di sé**, **autonomia** e **integrità**.
        - **Privacy**: il diritto di essere lasciati soli.
        - Diritto esplicito alla privacy è dato in costituzioni, leggi e trattati internazionali.
        - Esempi di criticità: Facebook, scandali NSA, Cambridge Analytica.
    - **Abbondanza di dati**.
        - Record di carte di credito, dati sanitari, IoT devices, dati DNA, ecc.
    - *Legge delle conseguenze non desiderate*:
        - Una tecnologia viene inventata per uno scopo, ma poi usata per scopi diversi.
        - I laser sono usati nei lettori DVD.

>Quale dignità rimane ad una persona divenuta prigioniera di un passato interamente in mani altrui, di cui deve rassegnarsi ad essere espropriato? [Rodotà]

### L'era dei big data

- Con *big data* si riferisce all'acquisizione e all'analisi di gigantesche collezioni di dati.
    - **Datafication**:
        - Prendere un processo che era prima invisibile nei dati.
        - Molte organizzazioni sono dipendenti dai dati per operare correttamente o del tutto.
        - Molte compagnie oggi sono essenzialmente compagnie basate sui dati.
        - Dopo che un processo è stato convertito a dati, può essere **tracciato**, **monitorato** e **ottimizzato**.
        - Anche se non usati subito, i big data possono essere usati più avanti per scopi inattesi.
    - **Massive messy data**:
        - Le analisi Big data richiedono di collezione quantità massive di *messy data*. 
        - **Messy data**: dati in formato non uniforme (come nei DB) o annotati (semanticamente).
            - Innovazioni tecnologiche necessarie per manipolare e analizzare questi dati.
        - Massive amount: $400$ milioni di tweet in un giorno nel 2013.
    - **Pattern nascosti**:
        - Le analisi BD permettono di rivelare pattern che altrimenti sarebbero rimasti inosservati.
    - **Raccolta indiscriminata**:
        - Big data richiede tipicamente la collezione di dati di tipo diverso.
    - **Retenzione indefinita, utilizzi inattesi**:
        - Le informazioni sono mantenuto tipicamente per lunghi periodi di tempi per aver utilizzi inattesi.
    - **Perdita della privacy informativa**:
        - **Informational privacy**: l'abilità di determinare per noi stessi quali informazioni su di noi vengono raccolte e come vengono usate.
        - Tutti gli sviluppi precedenti non possono avvenire senza una perdita di controllo sui nostri dati.

#### Deanonimizzazione

- **Deanonimizzazione**:
    - La **deanonimizzazione** è una criticità anche quando ci si affida alla sicurezza dell'anonimizzazione.
    - `eg` Nel 2006, Netflix rilascia dati anonimi di rating per una competizione (migliorare raccomandazioni)
        - $100M$ rating privati da $500K$ utenti, `<user, movie, date of grade, grade>`
        - Il nome utente viene sostituito da un numero.
        - Rating e date incrociati con quelli pubblici di IMDB.
        - Rivelare utenti che votano (privatamente) film NSFW su Netflix.
        - Con 6-8 rating e date è possibile identificare univocamente un utente con il $90\%$ di probabilità.
    - Le grosse quantità di dati presenti online permettono l'**incrociamento**.
    - `eg` Nel 2006, AOL rilascia le parole di ricerca di $650K$ utenti di un periodo di 3 mesi.
        - User ID sostituiti da numeri casuali.
        - Ritiro dopo 3 giorni.

#### Data breach

- **Cause dei data breach**:
    - Credenziali compromesse ($19\%$).
    - Cloud service mal configurati ($19\%$).
    - Vulnerabilità software ($16\%$).
- **Costo dei data breach**:
    - Studio su 383 aziende di 12 paesi:
        - $4M il costo medio per un data breach.
        - $29\%$ di aumento dei costi di un data breach dal 2013.
        - $158 costo medio per perdita o record rubato.
        - $15\%$ di aumento pro capita dal 2023.
    - Studio su 24 aziende in Italia:
        - 2.35M€ il costo medio per un data breach.
        - $17.1\%$ di aumento dei costi di un data breach.
        - 112€ costo medio per perdita o record rubato.
        - $6.3\%$ di aumento pro capita dal 2023.
    - Costo pro capita per industria (decrescente):
        - Sanitari ($335), educativi, finanziari, […], retail, industriale, […], pubblico ($80).

-----

## Privacy e sistemi informativi

### Sistemi informativi

- **Sistema informativo**: sistemi organizzativi, formali, socioetecnici progettati per **raccogliere**, **processare**, **memorizzare** e **distribuire** le informazioni usate da un'organizzazione.
    - **Sistema informativo computerizzato**: sistema composto da persone e computer che processano e interpretano informazioni.
        - Insieme di componenti software e hardware che insieme raccolgono, memorizzano, processano e distribuiscono informazioni con l'obiettivo di supportare:
            - Le **attività** (automazione);
            - Le **decisioni** prese per governare i corpi delle organizzazioni (analisi, controllo, coordinamento, visualizzazione e reporting).
    - I sistemi informativi funzionano grazie allo scambio di dati e informazioni.
        - Questi dati e informazioni possono essere più o meno privati, ma quasi sempre confidenziali.
        - Possono essere modellati come un insieme di processi interconnessi in pipeline.
        - Esistono differenti tipi di dati (contenuti generati dagli utenti, email, transazioni, ecc).
        - I dati appartengono alle differenti entità (impiegati, clienti, fornitori, ecc).

#### Sistemi informativi e privacy

- **Sistemi informativi e privacy**:
    - La privacy può essere assicurata a più livelli:
        - Educando impiegati e manager.
        - Rafforzando misure di cybersecurity.
        - Usare VPN, DMZ, aggiornare i software, ecc.
- Un sistema informativo **GDPR-compliant** deve assicurare:
    - **Attribute-based authorization**:
        - Meccanismo di autorizzazione dichiarativo e completamente tracciabile di accesso ai dati.
    - **Anonymizing/pseudonimizing data**:
        - Meccanismi sicuri per la pseudoanonimizzazione o anonimizzazione delle informazioni.
    - **Traceability**:
        - Un registro di chi ha creato, modificato o cancellato le informazioni, quando e con che scopo.
    - **Data deletion**:
        - Meccanismi sicuri che assicurino il *diritto all'oblio* senza che modifichi l'affidabilità del sistema.
    - Condizioni che devono coesistere tutte insieme.

#### Controllo d'accesso

- **Access control techniques** for **attribute-based authorization**:
    - **Controllo degli accessi**: restringere gli accessi a risorse informatiche, specialmente in sistemi multi-utente.
    - Requisiti di privacy e sicurezza complicano la creazione di schemi efficienti di AC.
        - Sistemi molto complessi (molti attori di vario tipo) richiedono schemi di AC complessi.
        - Bisogna valutare anche le (possibili) situazioni di emergenza (`eg` sanitarie).

##### Role-based Access Control (RBAC)

- **Role-based Access Control** (RBAC):
    - Formalizzato nel 1992 dal *US National Institute of Standards and Technology*.
    - Adatto ad imprese che gestiscono fino a 500 impiegati.
    - Gestisce l'AC tramite i **ruoli** piuttosto che gli user ID degli impiegati.
        - I ruoli forniscono un livello extra di astrazione.
        - Funziona come una collezione di permessi o *entitlements*.
            - A cosa si può accedere, come ci si può accedervici (scrittura, lettura).
        - Ogni ruolo può essere assegnato ad uno o a centinaia di utenti (semplificazione).
    - Funzionamento:
        - Gli amministratori assegnano i permessi di accesso ai ruoli.
        - I ruoli possono essere assegnati agli utenti individuali.
            - Gli utenti possono avere uno o più ruoli (ognuno con differenti diritti di accesso).
        - Gli amministratori possono semplicemente aggiornare i ruoli o i permessi d'accesso.
            - Assegnando o rimuovendo gli utenti dai ruoli appropriati.
- Limiti del RBAC:
    - Configurazioni DI AC grossolane, predefinite e statiche.
    - Non fornisce meccanismi flessibili a cui i clienti/utenti o le istituzioni possano esprimere i requisiti.
    - Non cattura gli scopi per cui i dati sono rilasciati ai vari stakeholders.

##### Attribute-based Access Control (ABAC)

- Un controllo di accesso accurato deve tenere traccia di:
    - L'utente giusto;
    - Al giusto momento;
    - Alla giusta locazione;
    - In accordo con la compliance regolatoria.
- **Attribute-based Access Control** (ABAC):
    - Aggiungendo del **contesto**, le decisioni d'autorizzazione possono basarsi su:
        - Il ruolo dell'utente;
        - A chi o a cosa quell'utente è relativo;
        - A cosa l'utente deve accedere;
        - Da dove l'utente deve accedere; 
        - Quando l'utente deve accedere;
        - Come quell'utente sta accendo a quelle informazioni.
    - Permette di estendere i ruoli con attributi e policy.
        - Sapere il ruolo di un utente non è sufficiente.
    - Costruisce delle **politiche** (*policy*) sui singoli attributi usando il **linguaggio naturale**.
        - Costruendo policy facili da capire l'AC diventa molto più robusto. 
    - Al posto di ruoli, si definiscono attributi di **soggetto**, **oggetto** e **ambiente**.
        - Rilassa il bisogno di essere registrato al sistema per poter accedere alle risorse condivise.
        - *Subject Attributes*: ruolo, appartenenza a un gruppo, dipartimento, livello di management, certificazioni, ecc.
        - *Resource Attributes*: metadata, tag, ecc.
        - *Environment Attributes*: tempo, location, canale di comunicazione, protocollo, livello cifratura, client, ecc.
    - Le policy possono essere definite sia dall'amministratore che dal data subject (differenza sostanziale).
    - **Authorization Engine**:
        - Prende in input: 
            - Attributi di soggetto;
            - Attributi dell'ambiente;
            - Attributi di risorse e azioni;
            - Una policy.
        - E permette o nega l'accesso a una risorsa.
        - Funzionamento:
            - La richiesta d'accesso viene mandata a un *Policy Enforcement Point* (PEP).
            - La richiesta è inoltrata al *Policy Decision Point* (PDP).
                - Guarda la richiesta e recupera le policy applicabili, le valute e restituisce la decisione al PEP.
                - Si avvale deli attributi memorizzate nel *Policy Information Point* (PIP).
                - Le policy sono create e memorizzate dal *Policy Administration Point* (PAP).
        - Il linguaggio utilizzato dalle varie componenti è il **XACML**.
            - Il *eXtensible Access Control Markup Language*, uno schema XML.
            - Esistono varianti che utilizzano JSON.
        - Sono presenti *rule-combining algorithm* per gestire i casi in cui siano presenti risultati contrastanti tra le policy.

#### Pseudoanonimizzazione

- **Pseudoanonimizzazione**: procedura di **de-identificazione** dei dati.
    - Dove uno o più campi informativi in un data record sono sostituiti con identificativi anonimi.
    - Un singolo pseudonimo per campo o insieme di campi rende comunque possibili la data analysis e processing. 
    - Dati pseudoanonimi possono essere **ripristinati** dallo stato originario con l'**aggiunta di informazione**.
        - L'informazione aggiuntiva deve essere memorizzata in un luogo separato rispetto ai dati. 
        - Dati anonimi non possono essere mai ripristinati al loro stato originario.
- Due approcci alla pseudoanonimizzazione:
    - **Column encryption**: cifra le colonne di dati sensibili a lato server.
    - **Dynamic Data Masking**: limita l'esposizione di dati sensibili mascherandoli dinamicamente a utenti non privilegiati.
        - In fase di comunicazione server-client.
        - Si mascherano i valori anche parzialmente per esplicitarne la forma.
            - Il DBMS può fornire delle funzioni di masking di *default* che discriminano in base alla forma.
        - I dati sono memorizzati in chiaro, e mascherati durante la fase di query.
        - Bypassabile con tecniche di inferenze e bruteforce, non sufficiente.

##### Column encryption

- **Column encryption**: funzione progettata per proteggere dati sensibili nei database.
    - I dati vengono cifrati lato client, e le chiavi non vengono mai rivelate al DB engine.
    - **Divisione** tra chi **possiede** i dati (e li può vedere) e chi li **gestisce** (ma non può averci accesso).
    - Si considera il DB sicuro anche in fase di breach.
    - Bisogna specificare informazioni riguardo l'algoritmo e il tipo di chiavi.
        - Tipo di chiavi:
            - **Column encryption keys**: uate per cifrare i dati in una colonna cifrata.
            - **Column master keys**: uate per cifrare ona o più column encryption keys.
        - Il DBMS memorizza le configurazioni di cifratura per ogni colonna dei metadata del DB.
            - Le chiavi non sono mai memorizzate o cifrate in plaintext.
        - Il DBMS memorizza solo le CEK in versione cifrata e la location della CMK.
            - Le CMK sono memorizzate in un *key store* esterno e fidato.
- **Cifratura deterministica e randomizzata**:
    - I DBMS permettono certe query sui dati cifrati, in base al tipo di cifratura sulla colonna. 
    - **Cifratura deterministica**: genera gli stessi valori cifrati per ogni plain text dato. 
        - Permette *point lookups*, *equality join*, *grouping* e *indexing*.
        - Permette ad utenti non autorizzati di indovinare informazioni sui valori cifrati.
            - Analizzando pattern, soprattutto quando i valori possibili sono pochi (`eg` T/F, N/S/E/W).
    - **Cifratura randomizzata**: cifra i dati in una maniera meno predicibile.
        - Più sicura, ma non permette ricerca, *grouping*, *indexing* e *joining* sulle colonne cifrate.

#### Auditing

- **Auditing**: **processo di esaminazione e validazione** di documenti, dati, processi, sistemi.
    - Assicura i principi di **tracciabilità** delle operazioni all'interno di un SI.
        - Come richiesto dalla GDPR.
    - Definizioni:
        - *Audit log*: **documento** che contiene tutte le attività oggetto di autiding in ordine cronologico.
        - *Audit objectives*: insiemi di **regole aziendali**, **sistemi di controllo**, **government regulations** o **policy di sicurezza**.
        - *Auditor*: **persona** autorizzata all'*auditing*.
        - *Audit procedure*: insieme di **istruzioni** per il processo di *auditing*.
        - *Audit report*: **documento** che contiene tutte le rilevazioni del processo.
        - *Audit trail*: registro cronologico di **cambi nei documenti e nei dati**, **attività di sistema** e **eventi operativi**.
    - Tipologie di *auditing*:
        - *Auditing interno*: condotte dallo staff interno all'organizzazione soggetta.
        - *Auditing esterno*: condotte da una terza parte indipendente sull'operato dell'organizzazione soggetta.
    - Attività di *auditing*:
        - Identificare problemi di sicurezza che devono essere indirizzati.
        - Stabilire piani, policy e procedure per condurre *audit*.
        - Organizzare e condurre *audit* interni.
        - Assicurare che tutte le voci contrattuali siano rispettate dall'organizzazione soggetta.
        - Funger da *legame* tra la compagnia e il team di *autiding* esterno.
        - Fornire consulenza al dipartimento legale.
    - **Processo di auditing**: stabilisce che il sistema sia funzionante e soddisfi policy, regolazioni e leggi.
        - Eseguito dopo che il prodotto ha raggiunto lo stato di *produzione*.
        - Flusso:
            - Ciclo di vita di sviluppo del sistema.
            - *Auditing process*:
                - Capire gli obiettivi.
                - Review, verifica e validazione del sistema.
                - Documentare i risultati.

##### Data auditing

- **Data audit**: registro cronologico dei cambiamenti di dati memorizzati in un file di log o in una tabella di un DB.
    - **Database auditing**: registro cronologico delle attività di un DB.
    - Obiettivi dell'*auditing* (verifica della bontà):
        - Integrità dei dati.
        - Utenti dell'applicazione e ruoli, confidenzialità dei dati, controllo d'accesso.
        - Data changes, data structure changes.
        - Disponibilità del DB o dell'applicazione. 
        - Controllo dei cambiamenti.
        - *Auditing reports*.
    - Possibile sfruttare funzioni built-in nei DBMS o sviluppare meccanismi ad-hoc.
- Modelli di *autiding*:
    - Modello 1 (informazioni memorizzate):
        - **Stato dell'oggetto prima** che dell'esecuzione dell'azione.
            - Possono essere presenti oggetti (db, tabelle) non soggetti ad *auditing*.
            - DMBS più sofisticati permettono *auditing* più granulari. 
        - Descrizione dell'**azione** eseguita.
            - Possono essere presenti azioni non soggette ad *auditing*.
        - **Nome dell'utente** che ha eseguito l'azione.
            - Possono essere presenti utenti non soggetti ad *auditing*.
    - Modello 2 (informazioni memorizzate):
        - **Cambiamenti ai valori** di una colonna.
            - Meccanismi di purging e archiviazione per salvare spazio.
            - Ideale per auditing su colonna o due di una tabella.
    - Modello *historical auditing*:
        - Utilizzato quando un record dell'intera riga è richiesto.
        - Tipico delle applicazioni finanziarie.
