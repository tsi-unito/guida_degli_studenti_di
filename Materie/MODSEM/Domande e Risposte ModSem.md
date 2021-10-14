# DOMANDE

1) **Come è strutturata tripla RDF**  
Resource Description Framework.  
Il modello è basato sui grafi. I NODI sono entità e gli ARCHI sono le relazioni tra esse.
Le triple sono della forma <Soggetto, Predicato, Oggetto>

2) **Come vengono rappresentati i blank nodes in turtle, e cosa succede ad essi quando il file viene caricato su una LDP e viene creato il knowledge graph**  
I blank nodes in Turtle sono identificati da una coppia di parentesi quadre.  
I predicati associati ad un blank node sono scritti al suo interno.  
Quando vengono caricati in una LDP gli viene associato un nome e l'estensione della LDP, ma vengono mantenuti come blank nodes.

3) Assioma di sottoclasse comporta relazione tra due class expression, che nel caso più semplice possono essere due class;

4) **Relazioni ISA nelle reti semantiche**  
Servono per indicare il tipo di un individuo, collegandolo con la relazione al concetto generale a cui appartiene.  
In alternativa, indicano anche che una classe è sottoclasse.

5) **Cosa s'intende con rete semantica proposizionale**  
A differenza delle reti semantiche tradizionali, in quelle proposizionali i NODI possono rappresentare oltre a concetti (GENERALI o INDIVIDUALI), anche proposizioni (ad esempio SNEPS funziona così).  
Il concetto principale in questo caso è quello di proposizione.  
Con degli archi che si dipartono da una proposizione, possiamo rappresentare ad esempio l'appartenenza (member) di un individuo ad una classe.  
Gli archi sono sempre uscenti dalla proposizione ed entranti nei target (soggetti ed oggetti)

6) **Cosa s'intende per T-Box e A-Box**  
La T-BOX (terminology box) è tutto l'insieme dei termini che rappresentano la parte concettuale di un'ontologia.  
L'A-BOX invece (assertion box) è l'insieme delle asserzioni effettuate sugli individui.

7) **Cos'è una classe definita**  
Una classe definita è un particolare tipo di classe che viene definito come equivalente ad un insieme di restrizioni.  
Le restrizioni sono le condizioni necessarie e sufficienti per l'appartenenza alla classe.  
Riducendo l'insieme degli individui a solo quelli che soddisfano le restrizioni, possiamo effettuare alcune forme di ragionamento automatico.

8) **Cos'è una class expression**  
Le class expressions identificano insiemi di individui grazie alla definizione di condizioni che devono essere soddisfatte dalle proprietà degli individui. Gli individui che le soddisfano si dicono ISTANZE delle rispettive class expression.  
Sono costruibili utilizzando CONNETTIVI booleani, QUANTIFICATORI, RESTRIZIONI numeriche, ENUMERAZIONI di individui.  
Esistono anche le Property Expression che permettono di definire PROPRIETA'

9) **Cos'è un named graph**  
Si tratta di un grafo di una rete semantica a cui è stato dato un nome. In Turtle si usano i prefissi.  
In questo modo è possibile utilizzare più reti provenienti da fonti differenti.

10) **Cos'è il default graph**  
Il default graph è una rete semantica a cui non è stato assegnato alcun nome.

11) **Cosa s'intende per quadrupla**
La quadrupla è un'estensione della tripla <Soggetto, Predicato, Oggetto> a cui è aggiunto un quarto valore che identifica a quale grafo appartiene la tripla.

12) **Struttura base di query sparql e cosa restituisce**  
Sono simili alle query di SQL; prima devono essere specificati i prefissi delle ontologie che si utilizzeranno indicando gli IRI.  
Si possono specificare i dataset, poi si fa una SELECT specificando i dati che ci interessa ottenere.  
Successivamente, si specificano nel WHERE le condizioni (sotto forma di triple che formano un pattern corrispondente ad un insieme di grafi) che devono essere soddisfatte dagli elementi che saranno restituiti come risultato, e infine si possono aggiungere  dei modificatori della query per ottenere ordinamenti diversi (ad esempio).  
Il RISULTATO della query è dato da tutte le occorrenze delle variabili contenute nella clausola WHERE nei grafi individuati.

13) **Cos'è Neon**  
E' una metodologia orientata agli aspetti COLLABORATIVI nello sviluppo e nel mantenimento di RETI di ontologie.  
E' un insieme di 9 scenari, associati a specifiche attività e documenti.  
4 fasi: Initiation, Design, Implementation, Maintenance (ed è un ciclo chiuso che ritorna a Design)

14) **Quali sono le metaproprietà in OntoClean**  
Le proprietà, che forniscono informazioni su come caratterizzare le classi, sono:
    - IDENTITA': Proprietà che permette di identificare un tipo di oggetti. Triangolo -> lunghezza dei lati.  
      [SORTAL => identificabili allo stesso modo].  
      Viene ereditata dalle sottoclassi.

    - UNITA': proprietà di un tipo di oggetto di essere unitario.  
      E' ereditata dalle sottoclassi (ma non vale per la proprietà contraria)

    - RIGIDITA': Proprietà di un tipo di oggetto che non è soggetta a cambiamenti: (Persona non cambia, studente lo è temporaneamente).  
    Solo l'ANTIRIGIDITA' viene ereditata.

    - DIPENDENZA: Proprietà di un tipo di oggetti di dipendere da un altro per la propria definizione. Studente dipende da scuola.

15) **Cos'è Provenance**  
E' un pattern utilizzabile per rappresentare agenti, attività e ruoli.  
Un agente può essere una persona, un concetto o un oggetto inanimato.  
I ruoli descrivono la relazione tra un'entità e un'attività: ad esempio come l'attività ha utilizzato o generato l'entità.

16) **Cosa s'intende per vocabolario RDF ed ontologia OWL**  
    I vocabolari RDF sono insiemi di elementi scelti per il loro utilizzo molto frequente, e servono per unificare ontologie diverse in modo che siano interoperabili. Così non è necessario inventare ogni volta nuovi termini per informazioni che così sono facili da reperire e comprendere.  
    Dublin Core è uno schema di metadati e serve per reperire risorse (Titolo, Creatori, Tema...)  
    Europeana usa DC.  
    FOAF è un altro genere di vocabolario e serve per dare informazioni sulle persone.  
    Schema.org è utilizzato per mantenere dati strutturati riguardo al web.  
    OWL invece è un LINGUAGGIO per creare ontologie per il Web Semantico, con un significato definito FORMALMENTE.

17) **Quali sono i linguaggi dello stack del web semantico**  
RDF: serve per descrivere risorse (documenti, persone, oggetti...)  
RDFS: permette di descrivere RELAZIONI TRA RISORSE  
OWL: permette di descrivere ONTOLOGIE computazionali. Con esso si possono descrivere relazioni complesse.

18) **Differenze tra RDF e RDFS**  
RDF: serve per descrivere risorse (documenti, persone, oggetti...)  
RDFS: permette di descrivere RELAZIONI TRA RISORSE

19) **Cosa sono le logiche descrittive**  
Sono un sottoinsieme della logica dei predicati, orientato alla classificazione e caratterizzato da proprietà desiderabili sul piano computazionale (decidibilità e trattabilità ad esempio)

20) **OWL vs OWL2 vs RDF**  
RDF serve per descrivere risorse (documenti, persone, oggetti).  
OWL è un LINGUAGGIO che permette di descrivere ontologie computazionali.  
OWL2 è un linguaggio per creare ontologie con un significato DEFINITO FORMALMENTE

21) **Elementi di un'ontologia OWL**  
Classi, Proprietà, Individui, Letterali.  
Alternativamente  
Entità, Assiomi ed Espressioni.  
Le entità possono essere INDIVIDUI, CLASSI, PROPRIETA' (object property, data property)  
Gli assiomi possono essere sulle classi, dichiarazioni, ecc.

22) **Cos'è DOLCE e la differenza tra enduranti e perduranti**  
    DOLCE è un'ontologia orientata alla congnizione e al linguaggio.  
    La definizione chiave è quella di PERDURANTE ed ENDURANTE:
    - Evento => Perdurante (ha natura temporale)
    - Partecipante => Endurante (non ha natura temporale)

23) **Perchè può capitare che la gerarchia di classi inferita sia diversa da quella asserita?**  
Può succedere perchè potenzialmente può essere inferito dal reasoner che alcune classi siano completamente sussunte da altre classi.  
In altre parole: l'insieme degli Individui che apparterrebbero ad una classe è totalmente incluso anche in quello di un'altra classe.

24) **Cos'è una linked data platform?**  
E' una piattaforma per l'accesso ai linked data che usa il paradigma REST.  
E' un servizio WEB basato sulle funzionalità di HTTP per leggere e scrivere i Linked Data.
