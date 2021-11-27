# MAADB <!-- omit in toc -->

- [Concetti base](#concetti-base)
  - [Database](#database)
  - [Data Model](#data-model)
  - [Schema](#schema)
  - [Qual è l'utilità dei vincoli?](#qual-è-lutilità-dei-vincoli)
  - [DBMS](#dbms)
    - [Indipendenza logica dei dati](#indipendenza-logica-dei-dati)
    - [Indipendenza fisica dei dati](#indipendenza-fisica-dei-dati)
- [Storia dei database](#storia-dei-database)
  - [Modello gerarchico](#modello-gerarchico)
  - [Modello reticolare](#modello-reticolare)
  - [Modello Relazionale](#modello-relazionale)
  - [Modello ad oggetti](#modello-ad-oggetti)
    - [Object Oriented](#object-oriented)
    - [Object Relational](#object-relational)
  - [DB semistrutturati](#db-semistrutturati)
  - [DB a Grafi](#db-a-grafi)
  - [Altri DB](#altri-db)
    - [Conclusioni sui generi di DB a grafo](#conclusioni-sui-generi-di-db-a-grafo)
- [Moduli e Architettura di un DBMS](#moduli-e-architettura-di-un-dbms)
  - [Disk Space Manager](#disk-space-manager)
  - [File and Access Methods](#file-and-access-methods)
  - [Buffer Manager](#buffer-manager)
  - [Recovery Manager](#recovery-manager)
  - [Transaction Manager](#transaction-manager)
  - [Lock Manager](#lock-manager)
  - [Proprietà delle Transazioni - ACID](#proprietà-delle-transazioni---acid)
  - [Moduli in relazione con l'Utente](#moduli-in-relazione-con-lutente)
- [Dischi e File](#dischi-e-file)
  - [Disco Magnetico](#disco-magnetico)
  - [Tipi di File](#tipi-di-file)
    - [Heap Files (panoramica)](#heap-files-panoramica)
    - [Sorted Files (panoramica)](#sorted-files-panoramica)
    - [Index Files (panoramica)](#index-files-panoramica)
  - [Heap Files](#heap-files)
    - [Record a lunghezza fissa e variabile](#record-a-lunghezza-fissa-e-variabile)
    - [Organizzazione IntraPage](#organizzazione-intrapage)
    - [Organizzazione InterPage](#organizzazione-interpage)
  - [Sorted Files](#sorted-files)
  - [Index Files](#index-files)
    - [Struttura di un file indice](#struttura-di-un-file-indice)
    - [ISAM](#isam)
    - [B+Tree](#btree)
    - [Prefix Key Compression](#prefix-key-compression)
    - [Bulk Loading](#bulk-loading)
    - [Indici Clustered e Unclustered](#indici-clustered-e-unclustered)
    - [Indici con chiavi di ricerca composite](#indici-con-chiavi-di-ricerca-composite)
  - [Indici basati su hash](#indici-basati-su-hash)
    - [Static Hashing](#static-hashing)
    - [Extendible Hashing](#extendible-hashing)
    - [Linear Hashing](#linear-hashing)
  - [Modello dei Costi](#modello-dei-costi)
- [Buffer Management](#buffer-management)

## Concetti base

### Database

E' una collezione di dati organizzati in modo tale da consentire lo svolgimento di certi task specifici:

- Interrogazione e reperimento di informazioni
- Analisi delle informazioni contenute nel Database
- La visualizzazione dei risultati

Un database deve essere facilmente interrogabile; ci deve essere un modo non ambiguo per visualizzare i risultati.

### Data Model

E' un formalismo rigoroso che consente di descrivere la struttura dei dati che sono contenuti nel database. Può essere Relazionale, Object Oriented, ecc.

### Schema

E' un particolare insieme di vincoli sui dati che sono descritti rispetto al formalismo dello specifico data model che è in uso.  
Se uso il modello relazionale (quindi con chiavi esterne, vincoli di integrità, ecc) uno schema mi informerà riguardo le tabelle, gli attributi che compongono i campi, ecc.

### Qual è l'utilità dei vincoli?

Permettono di semplificare gli aspetti organizzativi, di gestione e ottimizzazione. Consentono l'esistenza di algoritmi efficienti e scalabili.  
Nei Database relazionali i vincoli comportano una serie di problemi per memorizzare i record (spreco di spazio), ma ne semplifica il reperimento (agevolazione nell'accesso ai dati).  
Come contro, questi vincoli limitano potenzialmente l'utilizzo di un certo modello a certi ambiti specifici.

Abbiamo vincoli:

- **Fisici**: per la struttura dei dati: come li si organizza sui vari settori del disco
- **Concettuali**: UML, E-R. Si descrive quali sono i concetti che dovranno essere rappresentati ecome verranno rappresentati (un prodotto avrà il suo ID, un prezzo, una quantità...)
- **Logici**: relazionale, OO, OR.  
  Vengono espressi vincoli o considerazioni sul livello fisico: quali strutture per l'organizzazione saranno usate, come verranno effettuate le associazioni, le indicizzazioni, ecc.  
  Con il modello logico ci si avvicina al modello fisico. Il modello logico in parte colma il gap tra concettuale e fisico.  
  Esistono modelli Logici più vicini al livello concettuale (che quindi richiedono un maggiore sforzo per mapparlo al modello fisico) e altri più vicini a quello Fisico (non rappresentano al meglio i tutti costrutti che potremmo rappresentare per esempio con il modello OO).  
  Ad esempio, il modello Object Oriented è molto vicino al livello concettuale perché ha nativamente dei costrutti vicini al livello relazionale (gerarchie ed ereditarietà ad esempio).  
  Il modello relazionale invece è più vicino al livello fisico: il metodo di organizzazione per record è molto più vicino a quello fisico di memorizzazione dei dati su disco (il mapping è dunque semplificato).

### DBMS

Sono sistemi hardware e software per la gestione delle basi di dati. Permettono di svolgere operazioni di memorizzazione, interrogazione e manipolazione.

Con l'utilizzo di un DBMS possiamo raggiungere obiettivi altrimenti impossibili con il semplice uso di un file system:

- Indipendenza fisica e logica dei dati
- Semplicità d'uso
- Integrità e sicurezza dei dati
- Amministrazione uniforme dei dati
- Accesso concorrente
- Recovery

Un DBMS deve garantire l'indipendenza logica e fisica dei dati: garantire un livello di astrazione tale per cui, a fronte di variazioni di ciò che sta sotto, le applicazioni scritte dall'utente non debbano essere adattate.

#### Indipendenza logica dei dati

A fronte di un'eventuale modifica nella struttura logica dei dati (un nuovo attributo, cambia lo schema, ecc.) il DB deve consentire che ciò che era stato scritto e risultava funzionante continui a funzionare.

#### Indipendenza fisica dei dati

Bisogna risultare protetti da modifiche della struttura fisica dei dati (nuovo indice o rimozione, cambia il metodo di indicizzazione dei dati).

## Storia dei database

### Modello gerarchico

Si usa una gerarchia per i dati, come una struttura ad albero. Si usano puntatori bidirezionali da padre a figlio. La navigazione nel database era puramente procedurale.

### Modello reticolare

Si basa sempre sui puntatori, ma si perde il vincolo della struttura ad albero, passando invece a grafi.

Il modello descrive gli oggetti in termini di oggetti e relazioni tra essi. Se A punta a B, significa che esiste una relazione tra A e B.

Svantaggi (anche del modello gerarchico):

- **Distanza dal livello concettuale**: I due modelli si prestavano particolarmente bene per applicazioni di tipo procedurale, mentre per sistemi ospedalieri/bancari/universitari non lo erano molto.
- **Imprevedibilità del puntatore**: Quando si segue un puntatore, si sa che c'è, ma non sappiamo dove arriviamo. Non possiamo fare previsioni e questo danneggia le potenziali ottimizzazioni che si potrebbero effettuare.  
  Abbiamo bisogno di maggiore prevedibilità per permettere all'ottimizzatore di ottimizzare le query
- Era difficile scrivere query procedurali.

### Modello Relazionale

I dati sono organizzati in frammenti tra loro omogenei: quando si dichiara una tabella si decidono tutti i vincoli strutturali. Se creo una tabella "Studente", tutti gli studenti avranno le stesse proprietà.

Vantaggi:

- **Omogeneità e prevedibilità dei dati**: Se conosco i dettagli di una tabella (elementi, parametri, ecc.), posso stimare quante pagine dovrò leggere per portare in RAM tutte le informazioni necessarie.  
  Grazie alla prevedibilità è possibile ottimizzare la lettura su disco. Prevedibilità implica ottimizzabilità.
- **Assenza di puntatori**: visto che sono fonte di imprevedibilità, sono stati eliminati. Definire record che si trovano in un'altra tabella è possibile e si può fare tramite il concetto di chiave esterna. Si fa JOIN su di essa per ottenere le informazioni che ci servono. Possiamo sempre effettuare ottimizzazioni.
- **Linguaggio Dichiarativo (SQL)**: La possibilità di specificare in modo dichiarativo che cosa si vuole fare ha reso il modello molto popolare. All'utente la struttura fisica dei dati non interessa.
- **Algebra Relazionale**: Sono necessari pochissimi operatori dell'algebra relazionale per implementare le operazioni disponibili in SQL.  
  L'SQL è dichiarativo: specifico cosa voglio fare senza preoccuparmi di nessun dettaglio.  
  L'algebra relazionale è procedurale: è presente un concetto di sequenzialità nelle operazioni da eseguire. E' facile da ottimizzare perché abbiamo poche possibili alternative. Pur essendo pochi gli operatori, il problema dell'ottimizzazione è molto complesso (NP-completo ed esponenziale in funzione del numero di operatori)
- **Vicinanza al livello Fisico**: Le tabelle sono composte da multiple righe, memorizzabili facilmente su disco: i dati sono compattabili in modo adiacente.

Svantaggi:

- L'omogeneità dei dati può ledere applicazioni che non hanno bisogno di quest'aspetto (dati multimediali, gerarchie)
- I dati sono piatti e divisi: bisogna fare molti JOIN per recuperare interamente le informazioni che cerco.
- L'ereditarietà non è immediatamente rappresentabile
- Non è possibile effettuare match parziali

### Modello ad oggetti

Esistono

- Object Oriented
- Object Relational

In entrambi i casi il concetto fondamentale è quello di **oggetto**: sono modelli che descrivono oggetti e le relazioni che intercorrono tra di essi.

In entrambi vi è uno **schema ad oggetti** (che ricorda quello delle classi java ad esempio). E' possibile dichiarare strutture dati complesse a piacere e, nativamente, si dichiarano i metodi che possono operare su queste strutture dati.

Intrinsecamente si portano dietro i concetti di gerarchie ed ereditarietà.

#### Object Oriented

Sarebbe una forzatura avere una "base" relazionale. Gli OODB presentano un potere rappresentativo più elevato di quello del modello relazionale, e buona parte del business code finisce più in profondità nel DBMS, visto che non memorizza solo i dati, ma anche le funzioni eseguibili su di essi.

Sviluppato da accademici

#### Object Relational

Si lavora con oggetti, ma su una base ("core") relazionale.

I sostenitori dell'OR sostenevano che il modello relazionale aveva avuto troppo successo per essere abbandonato; era anche poco conveniente economicamente soppiantarlo.

Si è pensato che fosse conveniente realizzare un modello co-relazionale dove è possibile usare le feature ad oggetti e quelle relazionali (conveniente per ri-usare lavoro già svolto!).

Sviluppato da gente che risiedeva nel mondo del lavoro.

### DB semistrutturati

Con l'avvento di internet cambia la filosofia di accesso ai dati.

Prima ogni DB era un mondo a sè e non si considerava la possibilità di interfacce con altri sistemi esterni.

Con Internet non ci si scambia i dati direttamente nel "formato" del DB, in quanto bisognerebbe condividere lo stesso schema: si usano altri linguaggi di markup, come XML.

Nascono quindi le **basi di dati gerarchiche semistrutturate**.

Forniscono la possibilità di comunicare dati che non hanno una struttura definita rigidamente, e si possono ottenere anche informazioni riguardo la struttura stessa.

Si perde in prevedibilità, ma guadagno in espressività e flessibilità. Lo schema stesso è oggetto di interrogazione.

    Dimmi se è definito il campo INDIRIZZO e, se sì, qual è il suo valore. Inoltre dimmi com'è strutturato, se è un'unica stringa ad esempio.

I documenti XML sono autodescrittivi.

### DB a Grafi

La rappresentazione è la stessa dei reticolari, ma la differenza rispetto al passato è che il grafo utilizzato è semanticamente informativo. Le interrogazioni sono di tipo dichiarativo.

**Non è forzata la rappresentazione tramite un grafo!**

In realtà sono concetti messi in relazione tra di loro; la rappresentazione più comune o naturale è quella a grafo.

Abbiamo:

- **RDF (Resource Description Framework)**: è il core della rappresentazione della conoscenza all'interno del web semantico.  
  Consente di modellare il mondo (complesso a piacere) con triple: \<soggetto, predicato, complemento oggetto\>

  Tipicamente lo schema deve essere definito a priori (rigido!) quando lo si progetta, ma nel caso di RDF si tratta di un formalismo più "flessibile" grazie alla tripla, che rappresenta un fatto.

  In un grafo RDF, il soggetto è un nodo collegato tramite un arco, che ne rappresenta la relazione con un altro nodo.  
  Il tipo di arco (cioè la relazione) non è stata definita a priori, e potenzialmente il grafo può avere dimensioni considerevoli.

  Posso poi interrogare il grafo con delle query; molte di esse non corrispondono a quelle possibili con i DB relazionali.

  Si tratta di **query di raggiungibilità**, e per poterle effettuare si utilizzano linguaggi di query quali SPARQL.
- Social network.  
  Mentre con i DB RDF abbiamo tantissime relazioni, con i social network è il contrario: abbiamo moltissimi nodi e un numero limitato di tipi di relazione (follow, tweet, ecc.)  

  Potremmo utilizzare un DB relazionale, ma sarebbe costoso e con molte relazioni poco utili. Per le query di raggiungibilità dovrei effettuare tantissimi JOIN.

### Altri DB

- **DB Spaziali GIS (Geographic Information System)**: l'elemento di base è il concetto di location, come punto nello spazio, caratterizzato da coordinate puntuali.
  
  Questa rappresentazione ad-hoc esiste per permettere un certo tipo di query che non è disponibile negli altri DBMS.

  Un esempio sono le **query di range** o le query di **nearest neighbors**. Spesso si usa l'operatore rappresentante il **JOIN Spaziale**
- **Database documentali** per la memorizzazione di dati multimediali: si possono effettuare query che si basano sui dati (immagini, audio, video) e naturalmente si possono fare query specifiche ("trova foto simili")
- **Incertezza**: un elemento non rappresentabile con i DB relazionali. Possiamo pensare che quello che non dico sia falso o non lo so.
- **Confidenza**: associo dei pesi a ciascun arco.

#### Conclusioni sui generi di DB a grafo

La differenza focale tra i "nuovi" modelli a grafo e quelli vecchi è che quelli nuovi sono pensati per rappresentare le realtà che si prestano alla rappresentazione a grafo, mentre i modelli vecchi erano usati come forzatura per la tecnologia e i sistemi disponibili.

Naturalmente oggi si usa quello che più conviene, che sia a grafo, o relazionale.

## Moduli e Architettura di un DBMS

Al livello più basso dell'organizzazione del database abbiamo i seguenti moduli:

### Disk Space Manager

E' il modulo che gestisce lo spazio disco. Va a gestire la scrittura e la lettura dei dati sul disco stesso.

### File and Access Methods

Direttamente sopra al Disk Space Manager c'è il modulo per la gestione dei file. Si occupa di gestire lo spazio del disco; lo fa in base alla tipologia dei dati che si stanno memorizzando. Dati diversi hanno metodi di memorizzazione diversi.

Abbiamo tre tipi di file principali (ne esistono altri):

1) **Data file**: i dati veri e propri, inseriti e interrogati dalla base di dati. Sono anche chiamati _Heap Files_
2) **Indici**: sono definiti sui data files
3) **Catalogo di sistema**: all'interno vengono mantenuti i dati che descrivono gli altri files. Serve per ottimizzare: aiuta per gli indici, a sapere quanti predicati sono presenti, a scoprire la presenza di ridondanze (tutte informazioni utili per l'ottimizzazione delle query).

### Buffer Manager

Gestisce l'accesso ai file sul disco. Si passa attraverso questo manager: i dischi in genere sono molto grandi, mentre la memoria centrale è piccola a confronto.

In più, l'applicazione non accede direttamente ai dati sul disco, ma si interfaccia ad esso con il Buffer Manager.

Quando un'applicazione richiede una pagina di disco (in lettura/scrittura), non interagisce direttamente con esso, ma opera su una pagina caricata in memoria centrale/RAM (quindi è già nel buffer).

Si lavora in questo modo perché in genere le operazioni più costose sono quelle di accesso al disco, e quindi vogliamo limitarle il più possibile.

Esiste l'**ipotesi di località temporale**: ad un dato vi accederò un certo numero di volte: invece di fare quel numero di letture da disco, posso risparmiare tempo portando il dato nel buffer e poi leggendolo rapidamente da lì ogni volta che serve.

In ogni caso, la testina che legge il disco non porta mai un singolo record, ma l'intera pagina, che verrà quindi salvata nel buffer.

Abbiamo qualche complicazione aggiuntiva se per caso parliamo di scritture: quando una avviene, il dato è aggiornato nella memoria centrale, ma non sul disco. Abbiamo quindi un'inconsistenza che, nel caso in cui avvenga un crash/perdita di alimentazione, sarà persa.

Per questo motivo serve il modulo di Recovery Management, essenziale per poter tornare in una situazione di consistenza. Potremmo evitare di usare il buffer, ma questo distruggerebbe i guadagni in prestazioni che avevamo prima.

### Recovery Manager

Entra in azione quando si verifica un crash e bisogna ripristinare una situazione in cui ci sono dei dati inconsistenti. Un possibile esempio è quando viene persa l'alimentazione e i dati nel buffer della memoria centrale vengono persi.

Quando si verifica un crash, per poter recuperare la consistenza, il Recovery Manager ha bisogno delle informazioni delle operazioni/transazioni, di cui tiene traccia il Transaction Manager.

### Transaction Manager

E' sempre attivo: gestisce un file di log dove vengono salvate le informazioni necessarie per (eventualmente) disfare e rifare tutte le operazioni che sono state effettuate prima di un crash, per poter poi tornare ad una situazione di consistenza (dopo una fase di recovery).

Deve essere efficiente: dove possiamo salvare le informazioni parziali (quelle esclusivamente necessarie per il recovery)?

Anche il TM salva tutte le informazioni in memoria centrale, non rallentando il sistema, al prezzo di rischiare che tutto quello che si è fatto in caso di crash venga perso (davvero).

Il TM agisce passo passo assieme al lock manager.

### Lock Manager

Serve per gestire l'accesso concorrente ad uno stesso dato da due o più applicazioni. Quando più processi vogliono accedere ad uno stesso dato bisogna adempiere tecniche per far sì che il dato venga modificato atomicamente (interamente) prima da un processo e poi dall'altro.

Questi sono accessi in scrittura esclusivi: la gestione dei lock è molto delicata visto che possiamo giungere a situazioni di deadlock (attese circolari di risorse bloccate da processi diversi).

Lock molto rigidi possono rallentare il sistema e limitare la possibile parallelizzazione di attività che (in realtà) potenzialmente non generano deadlock.

### Proprietà delle Transazioni - ACID

- **Atomicità**: Tutte le operazioni di una transazione sono effettuate, oppure nessuna.
- **Consistenza**: Tutte le transazioni che operano in una situazione consistente del DB devono lasciare il DB in uno stato consistente.
- **Isolamento**: Ogni transazione deve essere eseguita in modo isolato e indipendente dalle altre transazioni. L'eventuale fallimento di una transazione non deve interferire con le altre transazioni in esecuzione.
- **Durabilità**: Un'operazione completata deve essere persistente.

In genere Atomicità e Durabilità sono affidate al Recovery Manager.

Isolamento e Consistenza invece sono affidate al Concurrency Control Manager (Transaction e Lock Manager).

### Moduli in relazione con l'Utente

- **Modulo Parser**: parsifica le query degli utenti e le traduce in espressioni dell'algebra relazionale.
- **Modulo Optimizer**: Sceglie tra le diverse espressioni dell'algebra relazionale quella che sembra essere la più opportuna (non si cerca l'ottimo perché complesso). Si usano anche delle euristiche.
- **Modulo Operator Evaluator**: Implementa gli operatori scelti dal modulo precedente.
- **Modulo Plan Executor**: Esegue il piano, cioè la sequenza di operazioni da svolgere in cascata, e a sua volta quindi la sequenza di singoli operatori scelti dall'operator evaluator.

## Dischi e File

Il DBMS immagazzina le informazioni in dischi che permettono la persistenza dei dati (è la cosiddetta "memoria secondaria"). I dati in RAM invece sono persi se si verifica un crash o si perde l'alimentazione.

Le due principali operazioni da considerare per la progettazione di un DBMS sono:

- Operazioni di **READ**, che trasferiscono i dati dal disco alla memoria principale/RAM;
- Operazioni di **WRITE**, che trasferiscono i dati dalla RAM al disco.

Entrambe le operazioni sono molto costose se confrontate con quelle che lavorano esclusivamente sulla memoria centrale: bisogna capire come risparmiare al massimo le interazioni col disco.

Migliaia di operazioni in RAM costano molto meno di una scrittura su disco.

### Disco Magnetico

Mentre con la RAM il tempo di accesso è sempre costante, su disco varia. Questo perché i dati sono letti fisicamente.

Bisognerà studiare l'ordinamento secondo cui posizionare le pagine sul disco: la loro posizione comporterà tempi di lettura diversi.

Il disco è composto da una serie di piatti magnetizzati in rotazione continua; la superficie registra una sequenza di bit, divisa in multiple tracce disposte a forma di cerchio concentrico.

Ogni parte del disco passa periodicamente sotto la testina di lettura, che ha un costo diviso in tre parti:

- **Seek Time**: il tempo richiesto affinché la testina raggiunga la traccia contenente il dato che si vuole leggere (varia tra 1 e 20ms circa). E' la fase più costosa: si vorrà principalmente minimizzare questa.  
  Il costo dipende da quanto si trova lontana la traccia target rispetto a quella che ho letto per ultima (e quindi dall'arco che la testina deve spazzare). Se la testina è sulla traccia target, il tempo di seek è nullo.
- **Rotational Delay**: il disco ruota sempre; bisogna aspettare che la parte di traccia che vogliamo leggere si trovi sotto la testina. Varia da 0 a 10ms.
- **Tempo di trasferimento**: è il tempo necessario all'effettivo trasferimento dei dati. In genere comporta la lettura di una "pagina" di dati intera (non di più, non di meno).

Bisogna organizzarsi in anticipo per le letture in modo da renderle efficienti: allo stesso modo i dati devono essere piazzati sul disco in modo "intelligente".

Questo è il concetto di "**Next Block**": idealmente se so che dopo una pagina me ne servirà un'altra, la vorrò posizionare subito consecutiva. E' preferibile che sia anche lo stesso cilindro.

Un'altra mossa vincente è quella di portare in anticipo le pagine necessarie in memoria centrale (pre-fetching).

### Tipi di File

#### Heap Files (panoramica)

Corrispondono a file in cui l'informazione è organizzata in modo non ordinato. Sono i più generali, generici e meno vincolati, semplici collezioni non ordinate di record.

Dal punto di vista dell'immagazzinamento sono i più semplici (costi di gestione) ma non avendo vincoli non è possibile effettuare ricerche specifiche, ma solo sequenziali (costo di utilizzo).

Supportano poco le operazioni di ricerca: per individuare qualcosa in un heap file bisognerebbe scansionarlo completamente.

#### Sorted Files (panoramica)

Sono file ordinati; soddisfano due proprietà:

1) **Ordinamento**: sono ordinati in base a qualche tipo di criterio (alfabetico ad esempio).  
  Per ogni relazione memorizzata su un file posso avere al massimo una versione sorted (averne di più significherebbe avere repliche del file, con problemi di allineamento).  
  Il criterio di ordinamento è definito con un'analisi dei costi (basati sulle operazioni che si intendono effettuare sulla relazione, in modo da massimizzare i benefici dell'ordinamento) ed è deciso al momento della creazione della relazione.
2) **Strategie di memorizzazione**: Quando dichiaro file _ordinato_ rispetto ad un certo criterio, le pagine consecutive sono memorizzate secondo il principio del _Next_.  
  Esempio:
      Ordino le pagine in modo contiguo in base alla data di nascita.  
      Giulio nato il 1° Marzo sarà vicino a Giovanni, nato lo stesso giorno.
  Si memorizzano i file in modo contiguo perché ottengo un vantaggio in fase di interrogazione, sia se le query sono ad accesso diretto in maniera puntuale ("_Dimmi gli studenti iscritti all'esame X_"), sia se le query riguardano un range ("_Studenti iscritti dal 1° Gennaio al 5 Marzo_").  
  Il costo del mantenimento dei file ordinati è dato dal mantenimento dell'ordinamento scelto in caso di aggiornamenti.  
  Il costo può essere rischiosamente elevato se si aggiorna molto spesso!  

  Quando inserisco un record in un file ordinato devo inserirlo nella posizione corretta in funzione degli attributi definiti per il criterio di ordinamento.

Quindi i sorted files portano alcuni vantaggi in termini di query, ma di contro devo mantenere questa struttura ordinata (con i costi derivanti).

#### Index Files (panoramica)

Servono come sovrastruttura per i file non ordinati (o per i file ordinati).

Si comunica all'indice il valore della chiave corrispondente ai valori di cui si è interessati e si accede all'indice (rispetto al quale i record sono organizzate in modo efficiente).

Quando un indice individua la chiave ricercata, restituisce il puntatore specifico.

La struttura ad indice più utilizzata è quella del bi-albero.

Gli **Hash Files** sono un particolare tipo di index file. L'indicizzazione basata su hash è molto efficace per le query di uguaglianza, ma non per quelle di range: facilita solo il raggiungimento di un valore e non effettua alcun ordinamento.

### Heap Files

Sono una collezione di record non ordinati, memorizzati in pagine con totale libertà, senza alcun requisito di contiguità. Sono i più facili da gestire per le memorizzazioni.

- **Organizzazione Intrapage**: vedere come i record sono organizzati all'interno della pagina
- **Organizzazione InterPage**: Come devo organizzare tra loro le pagine diverse all'interno del file

Il file sarà una collezione di pagine, ciascuna delle quali è una collezione di record della relazione che sto considerando.

#### Record a lunghezza fissa e variabile

L'organizzazione intrapage può essere di tipo diverso in base ai vincoli che si impongono sulla struttura del record.

Posso avere record a lunghezza fissa o variabile:

- **Record a lunghezza fissa**: Si sa fin dall'inizio quanti byte corrisponderanno al singolo record. La relazione è composta da un determinato numero di campi con dimensioni specifiche.  
  E' molto utile nel modello relazionale: basterà sommare le lunghezze degli altri campi per individuare l'inizio di un specifico record.  
  Abbiamo vantaggi in termini di prevedibilità, ma lo schema sarà molto rigido.  
  Nota: le informazioni riguardanti la lunghezza fissa dei campi + specificata nel **catalogo di sistema**.
- **Record a lunghezza variabile**: Non definisco a priori la lunghezza dei singoli record, ma la definisco per ciascun campo in maniera a sé stante. Esistono due approcci:
  1) Un primo approccio prevede che si dedichi un campop all'inizio del record chiamato **field count**, per determinare il numero di campi informativi presenti nel record stesso.  
    Quando si supera una sequenza speciale di bit, si capisce di aver superato la fine del precedente record.  
    Il vantaggio è che non si spreca spazio a causa di una sovrastima delle lunghezze. Fra gli svantaggi è importante l'impossibilità di accedere direttamente ad un campo, oltre alla possibilità che il separatore sia molto lungo (riducendo quindi i guadagni!)
  2) Il secondo approccio prevede la presenza di puntatori nella prima parte del record: se il record contiene n campi, i primi n+1 elementi saranno puntatori; ciascuno punterà all'inizio del campo corrispondente (l'ultimo puntatore punta alla fine del record complessivo).  
    Vantaggio: potenzialmente spazio risparmiato.  
    Svantaggio: una volta allocato il puntatore, si stabilisce anche il valore massimo che può assumere (rappresentazione variabile, ma con un limite).

Nel caso di aggiornamenti, questi comportano sia variazioni sulle lunghezze dei campi e degli spostamenti (shifting) dei dati.

Non è un problema particolarmente pesante, visto che sono operazioni in genere completamente svolte in RAM.

#### Organizzazione IntraPage

**Record ID (RID) = \<page_id, slot #\>**.

Bisogna fare distinzione tra record a lunghezza fissa o variabile.

Nel caso della lunghezza fissa, possiamo organizzarli in due modi:

- **Packed**: La memorizzazione dei record è **contigua e compatta**: uno dopo l'altro sono archiviati.  
  E' presente un **campo contatore** che tiene traccia di quanti record attivi sono presenti nella pagina.  
  Posso calcolare la posizione del prossimo record moltiplicando il contatore per la lunghezza (fissa!) del tipo record. E' uno heap, per cui ogni nuovo elemento è salvato nel primo slot libero.  

  **Svantaggio**: abbiamo problemi quando cancelliamo un record e dobbiamo tenere tutto compatto. Di per sé non è troppo costoso, visto che svolgo l'operazione di ricompattamento in RAM, ma altri costi potrebbero derivare dalla presenza di indici che devono anch'essi essere aggiornati.

  L'operazione quindi è **poco costosa per lo shifting**, ma **molto costosa per l'aggiornamento degli indici** potenzialmente presenti (ogni spostamento potrebbe comportare l'aggiornamento di numerosi indici).
- **Unpacked**: Non è presente un contatore: abbandono il requisito di mantenere i record compatti; utilizzo una sequenza di bit (bitmap) che dicono quale slot è occupato (1) o no (0).  
  Quando uno slot è liberato per una cancellazione si mette a 0 il bit corrispondente allo slot. Se inserisco un record, lo slot viene marcato con un 1 nella bitmap.  

  La pagina viene frammentata con le cancellazioni, ma eventuali indici presenti sulla relazione non risentono delle modifiche.

La gestione dei record a lunghezza variabile aggiunge la complessità della lunghezza non fissa. Non si può dare per scontato che ciascun record che arrivi in scrittura possa essere scritto nello spazio di un record cancellato. Con i file heap ogni inserimento può essere fatto in uno spazio libero, purché lo spazio libero sia sufficiente per **contenere il record per intero**.

La strategia è basata sul concetto di **directory**, che contiene i puntatori ai record di interesse. Quando dovrò trovare il terzo record della relazione presente nella pagina, accederò al terzo elemento della directory e seguo il puntatore che mi dirige all'inizio dell'elemento stesso.

La directory non contiene informazioni sulla lunghezza del record, dal momento che è il record stesso a contenerla.

Quando devo cancellare un record ho due opzioni con questa strategia:

- Rimuovere semplicemente il record e lasciare lo spazio vuoto, frammentando le pagine (e rischiando che porzioni della frammentazione non vengano usate)
- Compattare tutto (visto che lavoro in RAM). Non ho più il problema degli indici visto che lavoriamo solo su dati e non sulla struttura verso cui puntano gli indici, che contiene solo dei puntatori da aggiornare.

#### Organizzazione InterPage

Usiamo sempre la struttura più generica, cioè un file di heap, che ha il minor costo di mantenimento delle proprietà (non devo mantenere un ordinamento).

Una prima soluzione per l'organizzazione delle pagine all'interno dei file potrebbe essere con un puntatore da uno slot al precedente e al successivo, ma saremmo limitati dall'accesso sequenziale (caso peggiore l'ultimo slot). In più, questa soluzione non ci fornisce informazioni sulla posizione dello spazio libero.

Una seconda soluzione per gestire le aggiunte: ogni volta vado nell'ultima pagina. Se c'è spazio lo uso, altrimenti creo una nuova pagina.

Ho il vantaggio che le operazioni di inserimento sono meno costose, ma rischio di rendere potenzialmente più numerose le pagine dedicate all'intero file (e anche portare ad uno spreco di spazio con la frammentazione dovuta alle cancellazioni).

Avere tante pagine in un file vuol dire leggerne molte di più di quando si legge/scandisce un file, un'operazione costosa. Il costo risparmiato negli inserimenti è quindi perso durante le ricerche.

Si usano due strategie:

1) **Implemento i file come una lista**: usiamo due tipi di pagine:
   - Pagine completamente piene
   - Pagine in cui è presente dello spazio libero
  
    Il file ha un header che contiene le informazioni necessarie per poter accedere alla lista delle pagine completamente piene o a quella con pagine in parte libere.

    Se una pagina viene saturata in seguito a degli inserimenti, allora viene spostata nell'altra lista.

    Ogni pagina contiene due puntatori oltre ai dati.

    Questa soluzione è comoda se abbiamo record a lunghezza fissa: non dobbiamo scandire tutta la lista per trovare uno spazio vuoto durante un inserimento.

    Se i record sono a lunghezza variabile, potenzialmente una pagina può stare solo in alcune posizioni, per cui dobbiamo scansionare sequenzialmente la lista. Possiamo risolvere il problema creando un secondo livello di directory.
2) **Utilizzare una directory**: Il file è visto come una sequenza di pagine alle quali si accede da una directory, che contiene due informazioni:
    - Puntatore alla pagina
    - Spazio libero nella pagina
    Ciascuna pagina della directory contiene dati relativi ad un elevatissimo numero di pagine (dimensione della pagina / (dimensione del puntatore + numero con cui rappresento lo spazio libero))

    Quando devo inserire un record di una certa dimensione, si cerca nella directory la prima pagina con spazio libero a sufficienza per contenere il nuovo record.

    La directory fa risparmiare tempo quando dobbiamo inserire e anche quando dobbiamo leggere: (leggo sequenzialmente la directory invece di dover leggere tutte le pagine linkate tra di loro).

    Gli accessi sono sempre due: (directory + pagina). Nel caso in cui serva una nuova pagina sono tre.

### Sorted Files

I file di tipo Heap non sono ordinati e quindi non dobbiamo preoccuparci di dove inserire i nuovi elementi: appena troviamo spazio li inseriamo.

Questo vantaggio organizzativo ha però un costo: qualsiasi operazione di ricerca di un'informazione (senza l'uso di indici) comporta l'intera scansione del file.

Se il file è ordinato posso utilizzare delle strategie di ricerche più efficienti (in particolare se l'ordine è in funzione del dato che sto cercando).

Un sorted file si può decidere che non abbia spazi intermedi. In questo caso possiamo considerarlo come un grande array su cui effettuiamo una ricerca binaria. Questo velocizza la ricerca, con un costo più elevato dovuto alla manutenzione dell'ordinamento.

Ricordiamo che il tipo di file è dichiarato alla creazione di esso (e del DB).

Un'altra osservazione sulla convenienza: se anche non dovessimo utilizzare la ricerca binaria, ma effettuarne una sequenziale, nel momento in cui troviamo un valore più grande di quello cercato possiamo fermarci in quanto siamo certi che non sarà presente. Di nuovo, risparmiamo in tempo di accesso.

**Vantaggi: possibile ricerca binaria o sequenziale rapida.**

E' costoso mantenere il file ordinato, specialmente se si sceglie di mantenere una memorizzazione contigua, dove l'inserimento di un nuovo record comporta lo shift in avanti di tutti gli elementi che lo seguono.

### Index Files

Ne esistono due tipi:

- Gerarchici (B-Tree), molto adatti sia a query di uguaglianza sia di range
- Hash (Hash File), adatti solo alle query di uguaglianza.

Distinzione: per **chiave di ricerca** si intende il valore dell'attributo rispetto a cui si effettua la ricerca. Non è da confondere con il concetto di "chiave primaria della relazione", un sottoinsieme di attributi della relazione che consente di identificare in modo univoco i record della relazione.

Ogni relazione può avere più chiavi candidate e tra di esse se ne sceglie una come principale.

Nel contesto delle query, la chiave di ricerca è l'attributo che deve assumere lo specifico valore (studenti con nome "Mario" -> la chiave è il nome). Può trattarsi di una ricerca per valore specifico, oppure per range. Si definiscono gli indici appositamente per accelerare gli accessi ai record che si usano più spesso (in genere).

---

Possiamo salvare tre tipi di informazioni negli indici:

1) Il **record** della relazione con la chiave imposta.  
   
   Vantaggi: presuppone l'organizzazione ordinata di tutti i record di tutta la relazione. Visto che l'ordinamento della relazione può essere effettuato rispetto ad un singolo criterio, possiamo mantenere l'ordine solo per un indice.

   Svantaggi: l'indice è molto appesantito dal contenimento dell'intero record.
2) Nel file sono contenute esclusivamente le informazioni riguardo la chiave di ricerca dell'indice: si usa una coppia **\<Chiave, Indirizzo al record\>** per ciascun record presente nella relazione che si sta indicizzando.

   Vantaggio: le informazioni hanno una lunghezza fissa e si possono gestire come i fixed records.

   Svantaggio: abbiamo tante occorrenze delle chiavi.
3) Invece di usare tante coppie \<Chiave, RID\>, si usa un'associazione **\<Chiave, lista di indirizzi\>**. Se si hanno N studenti con lo stesso cognome, con la precedente soluzione avremo N coppie, mentre con questa sarà presente una lista di RID. I record quindi hanno una lunghezza variabile.
   
   Vantaggio: risparmio spazio

   Svantaggio: la lista degli indirizzi è a lunghezza variabile e bisogna gestirla.

#### Struttura di un file indice

L'idea è di creare una struttura dati ausiliaria grazie alla quale si riesca a trovare l'elemento che si vuole raggiungere con la ricerca in modo diretto/guidato, risparmiando rispetto alla scansione sequenziale.

Un primo puntatore punta all'inizio del data file, mentre i successivi puntano ai record specifici.

Perché dovremmo usare un file separato per l'indice?

- L'indice può essere ordinato anche se il file che indicizza non lo è (con tutti i vantaggi che ne comporta)
- Posso creare il file di indice in maniera contigua e mantenerlo tale, anche se l'originale non lo è
- Posso effettuare ricerca binaria sull'indice
- Gli indici sono più piccoli visto ceh riguardano solo le colonne che interessano: ho meno pagine da portare in memoria quando li uso.

Svantaggi:

- Spazio: per quanto piccolo, ne occupiamo di più
- L'indice va mantenuto aggiornato quando si modificano i dati
- Vi è un salto di lettura ulteriore quando devo passare dall'indice al dato.

Il file costruito come struttura intermedia può però essere ancora potenzialmente lungo: se si suppone che la chiave di ricerca sia circa 1/10 (come ampiezza) rispetto al record globale, se si ha una relazione che occupa 1mln di pagine, l'indice ne occuperà 100k (comunque molte!)

E' conveniente quindi costruire delle sovrastrutture che "inglobino" degli intervalli di chiavi. Questo procedimento può essere ripetuto a più livelli:

- Al livello più basso ho le pagine del disco, con n chiavi non ordinate. Il tipo di organizzazione in cui non vi è ordinamento e contiguità del datafile è l'"unclustered".
  
  Non vi è alcun requisito di ordinamento/raggruppamento dei record logicamente vicini nelle pagine dei dati (mentre le chiavi nell'indice sono logicamente e fisicamente vicine).
- Una prima struttura ausiliaria a n chiavi (stesso numero del livello base) ordinate e con un puntatore
- Una struttura ausiliaria con una chiave per ogni range di valori.

Questa tecnica è quella dell'ISAM: **Index Sequential Access Method**.

#### ISAM

Questo genere di indice si basa su una ricerca ad albero.

Si può concludere che un dato non è presente nel database alla fine del cammino di un ramo.

**Il costo di accesso agli indici è logaritmico.**

Dopo un inserimento per bilanciare l'albero si identifica la pagina in cui il dato dovrebbe essere inserito (primary page); se non ci sta si genera una pagina di overflow che estende quella precedentemente individuata (overflow page).  
**Le pagine di overflow portano al deterioramento** della struttura dell'indice, per cui questo tipo di indice è molto efficace con realtà molto statiche (pochi inserimenti e cancellazioni), dove invece è semplice da gestire e molto efficace.

Quante operazioni di I/O mi servono per raggiungere il dato che voglio? **Ne servono tante quante la profondità dell'albero + 1** (la pagina che contiene effettivamente il dato; gli altri sono "salti" di livello).

Quindi, meno profondo è l'albero, e più efficiente è il suo uso.

La profondità può essere stimata come $log_k(n)$, dove k è il massimo numero di puntatori che ogni nodo ha (**fattore di ramificazione**), mentre n è il numero delle foglie.

In particolare, k è uguale al rapporto tra la dimensione di una pagina e la dimensione di una chiave di ricerca. Dunque è bene avere chiavi di ricerca piccole in modo da avere un k più grande.

Ogni accesso al dato comporta il caricamento dell'intera pagina in memoria principale. Inserimenti e cancellazioni non vanno a modificare la struttura interna dell'indice:

- Gli inserimenti si effettuano al livello delle foglie, nei file di dati, se c'è spazio.
- Le cancellazioni cancellano dal file di dati ma la corrispondente chiave, se presente nell'indice, vi resta.

Il file di indice è statico e può cambiare solo se si nota che la struttura sta degenerando e il costo sta diventando eccessivo (potenzialmente per la presenza di troppe pagine di overflow).

Il file di index è sorted, contiguo e, per ogni relazione, ci può essere al massimo 1 ISAM: ogni relazione può essere ordinata rispetto ad un unico criterio di ordinamento (ma questo vincolo è meno stringente con i B+Tree)

#### B+Tree

E' più utilizzato rispetto ad ISAM perché più realistico. Continua ad avere i dati nelle foglie, ma non sono pià memorizzati in modo ordinato e contiguo: si usano dei puntatori (in avanti e indietro) per passare da una foglia all'altra. Viene meno il vincolo di avere un unico criterio di indicizzazione.

I nodi interni non sono statici e soprattutto non sono completamente riempiti. La struttura dell'albero rimane bilanciata, ma viene aggiornata a fronte di aggiornamenti che rendono necessari degli aggiustamenti. **Non avremo quindi overflow.**

I nodi sono tenuti pieni tra il 50 e il 100%; l'occupazione media delle pagine è del 70%. Questo comporta un minor sfruttamento dello spazio.

Un minor riempimento comporta un minor fanout, e quindi una profondità dell'albero maggiore. Tuttavia, con questa organizzazione, possiamo passare dalla radice alle foglie con l'attraversamento dell'albero e una volta arrivati alla foglia si ha sicuramente il dato (data l'assenza di pagine di overflow).

Gli algoritmi utilizzati per aggiornare l'indice cercano di avere l'albero sufficientemente alto in modo da aumentare il fanout, per rischiare il meno possibile di trovarsi in un nodo interno e avere poco margine per ridirigere il traffico e raggiungere l'informazione realmente cercata.

Con un'occupazione di questo genere il parametro d (ordine dell'albero) va da 1d a 2d (con riempimento al 50% e 100%). L'ordine del B-albero è il numero minimo di chiavi contenute in un singolo nodo.

Come l'ISAM, supporta le query di uguaglianza e range molto efficientemente. La dimensione del nodo è pari alla pagina di memoria.

Il motivo per cui teniamo tanti spazi vuoti è per ridurre le operazioni di ristrutturazione (molto costose!) a discapito dello spazio.

L'albero non sarà mai sbilanciato. Solo la radice potrà avere un riempimento < 50%.

Quando avviene uno split di un nodo, si cercherà di suddividere il contenuto equamente tra la vecchia e la nuova pagina per:

1) Ridurre il rischio di dover effettuare un altro split in cui vengano eseguiti nuovi elementi
2) Per evitare fanout eccessivamente bassi che determinerebbero inefficienze durante la ricerca.

Importante: i B+Tree contengono solo i puntatori ai dati, mentre i B-tree contengono anche i dati.  
Il fanout è legato al numero massimo di coppie chiave-puntatore che si possono memorizzare all'interno della pagina.

Il B+Tree non si aspetta che il file che indicizza sia ordinato, per cui è suo compito ordinarlo a livello delle foglie.

#### Prefix Key Compression

E' una soluzione di compromesso nel caso in cui si ha la necessità di avere chiavi molto grandi (ad esempio un indirizzo cittadino), dal momento che la natura del dominio giustifica delle interrogazioni su una chiave di quel genere.

Si sarebbe potuto usare una funzione di hash ma, per risparmiare tempo dell'hashing, si usa la tecnica di compressione del prefisso.

Questa tecnica prevede che, qualora le stringhe siano troppo lunghe, anziché inserire a livello di chiave l'intera stringa, si può inserire soltanto una porzione minima sufficiente per discriminare e dirigere il traffico.

Queste compressioni potrebbero aumentare il fanout.

Implicazioni:

- Il fatto che il prefisso sia discriminante (e quindi sufficiente per rappresentare le chiavi) è vero in un certo momento, ma potrebbe non esserlo più se si aggiungono altre chiavi.
- Il vantaggio dell'appiattimento dell'albero lo si paga con una gestione della variabilità del record a livello di singolo nodo (che non permette più il concetto di un determinato grado per i nodi).
- Le procedure di inserimento e cancellazione sono pesanti: bisogna assicurarsi che le sotto-stringhe delle chiavi continuino a discriminare e, nel caso peggiore, bisogna aggiornare anch'esse (per evitare backtracking che renderebbe inutile l'indice).

#### Bulk Loading

Quando si crea da zero un indice su una relazione si effettua l'operazione di bulk-loading: si caricano in maniera completa, compatta ed efficiente le informazioni all'interno dell'albero.

Se possiamo ordinare completamente la relazione (quindi quando non è stata ancora ordinata rispetto ad altri criteri da un ISAM ad esempio), la ordiniamo completamente in modo che le pagine ordinate corrispondano alle pagine delle foglie dell'indice.

Se non lo si può fare (già presente un ISAM su un altro criterio), si effettua un ordinamento su una copia delle chiavi-puntatori su cui si sta costruendo l'indice.

E' una tecnica costosa, ma esistono comunque delle tecniche efficienti.

Quando i record ordinati vengono inseriti (una pagina alla volta) nella struttura, ciascun inserimento ha impatto solo sul sottoalbero più a destra che ho già inserito nella struttura che sta crescendo.

Quindi, prima si effettua l'ordine delle foglie e poi si costruiscono i nodi interni, sempre con questo effetto solo a destra, con un costo contenuto (le operazioni avvengono solo a destra perché i dati sono stati ordinati in partenza).

Con il bulk loading riusciamo a seguire i fattori di riempimento indicati inizialmente dal progettista.

Le foglie possono essere memorizzate sequenzialmente o tramite link (se si hanno già altri ordinamenti vincolanti). Quando si lavora con record a lunghezza variabile è difficile tenere pagine contigue, e non si ha la garanzia sulla cardinalità delle chiavi.

Vantaggi:

- Non effettuo split e riorganizzazioni dell'albero
- Posso controllare il fanout dei vari nodi.

#### Indici Clustered e Unclustered

Classificazione degli indici:

- **Indice primario**: definito sulla chiave primaria
- **Indice secondario**: definito su altre chiavi non primarie

Clustered vs Unclustered index: ad esempio la struttura ISAM è clustered, cioè che impone l'ordinamento; le unclustered non lo impongono.

In particolare:

- Un indice è **clustered** se l'ordinamento presente al livello delle foglie dell'index file è coerente con l'ordinamento dei record dei dati nel file dei dati. Questo vuol dire che il file su cui l'indice è costruito è ordinato (o "quasi" ordinato, visto che l'ISAM ha le pagine di overflow).
  
  Tipicamente succede che i puntatori che si incontrano scandendo le foglie ordinatamente (per definizione sono ordinate) sono a loro volta ordinati: **facendo una scansione non torno mai a leggere quello che ho già letto**.
- Con un indice **unclustered**, se si vuole effettuare una scansione sequenziale ed ordinata del file, lo si può fare partendo dall'indice, ma non si ha la garanzia di leggere ciascuna pagina del file di dati una volta sola (potrei dover tornare a leggere una pagina parzialmente letta per continuare con i record nell'ordine corretto).
  
  Questo può accadere perché potenzialmente l'ordinamento del file di partenza segue un'altro attributo (nome, cognome, indirizzo, cell, ecc): in pratica, **non vi è coerenza tra l'ordinamento delle chiavi e delle foglie**.

Lo svantaggio della versione unclustered è proprio la lettura più volte di una pagina. Inoltre, il livello delle foglie deve contenere un puntatore per ogni record, mentre nella versione clustered si può avere un solo puntatore per pagina.

Naturalmente, per la versione clustered uno degli svantaggi è che può funzionare solo per data file ordinati. 1 solo indice può essere clustered dal momento che un data file può avere un solo ordinamento: quindi tutti gli altri indici dovranno essere unclustered.

Esiste un modo per avere due indici clustered: utilizzare due copie dei dati. Questo è estremamente dispendioso a livello di spazio e di tempo.

E' buona norma quindi indicizzare con l'indice clustered la colonna che è più spesso usata per query di range.

#### Indici con chiavi di ricerca composite

Nel caso in cui effettuiamo ricerche con più attributi come criterio, possiamo creare degli **indici con chiavi di ricerca composite**.

Ho alcune soluzioni:

1) Uso prima un indice a chiave semplice e poi l'altro, effettuando intersezioni
2) Filtro sul valore specifico o sul range e poi filtro di nuovo sui dati rimanenti (rischio che non funzioni se per caso tutti i valori soddisfano la prima query)
3) Si rivela potenzialmente utile l'indicizzazione su chiave composta.  

Quando si realizza una chiave composta, prima si ordina il primo attributo della chiave, e poi si ordina nuovamente sul secondo attributo.

### Indici basati su hash

Una funzione di hashing è una funzione che ricevendo in input un qualche oggetto (ad esempio il valore dell'attributo che indicizziamo) lo mappa in un numero (spesso un intero) che corrisponderà all'indirizzo della pagina con tutti i record con quella specifica chiave.

Esistono due approcci:

1) Faccio hashing direttamente sui dati: quindi ottengo la posizione dell'oggetto nel disco (pagina)
2) Uso un indice a directory: Calcolo l'hash; vado direttamente alla pagina dove sono memorizzati tutti i puntatori. Da lì verifico la chiave e salto all'indirizzo dove si trova il dato su disco.  
  
  In questa maniera riesco ad avere file di hashing che occupano meno spazio, ma dovendo fare almeno un salto in più.

**Hash-based-indexes**: una struttura ad indice alternativa. L'indice è una collezione di bucket. Un **bucket** è una primary page più un certo numero di overflow pages.

Vantaggio: le query di uguaglianza sono molto più veloci rispetto agli indici gerarchici

Svantaggi:

- Non è possibile fare query di range
- Se ci sono collisioni si creano pagine di overflow quando le pagine si riempono

#### Static Hashing

Con l'hashing vogliamo avere la garanzia che chiavi distinte vengano mappate in valori di indice differenti.

Quando avviene una **collisione** significa che due chiavi restituiscono lo stesso hash.

Se n è il numero di elementi in cui si vogliono distribuire le chiavi, si ricorre all'operazione di modulo. Questo significa che otterremo sempre un valore nell'intervallo [0, n-1].

Idealmente i valori sono ben distribuiti (preferibilmente grazie alla funzione di hash). Sfortunatamente questa ipotesi di buona distribuzione non è assicurata, specialmente nei casi in cui possono essere presenti duplicati. In quel caso si estende la pagina con una pagina di overflow (e ripetendo il processo se anche quella dovesse essere piena).

Quando effettuo una query calcolo l'hash ed individuo il record. Spesso è un accesso rapido (1 solo), ma con catene di overflow lunghe questo non si verifica (e otteniamo un pesante costo di seek).

L'utilizzazione delle pagine deve essere comunque abbastanza alta: non possiamo averle quasi tutte vuote per evitare l'overflow! Dovrei portare in memoria molte pagine per dei file che voglio leggere sequenzialmente.

Serve un giusto bilanciamento tra:

1) contenere la lunghezza delle catene di overflow
2) buona utilizzazione delle pagine

Quale può essere una dimensione opportuna per la tabella di hash da utilizzare per ottimizzare gli accessi dei record di file, data la dimensione dei file di dati?

- Troppo larga: numero elevato di pagine sotto-utilizzate; scansione del file costosa
- Troppo piccola: numero eccessivo di collisioni e lunghe catene di overflow.

Si parte con un numero n di partizioni e si comincia a popolare i bucket relativi a questi n scelti. Quando il numero di collisioni diventa eccessivo, si raddoppia o triplica la dimensione dell'array.

#### Extendible Hashing

#### Linear Hashing

### Modello dei Costi

## Buffer Management
