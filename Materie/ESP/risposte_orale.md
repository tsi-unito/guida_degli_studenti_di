# Risposte orale - Federico Torrielli

## Privacy by default e privacy by design

Privacy by default significa che in ogni sistema dove deve essere garantita la privacy,
essa deve essere messa sempre "al massimo". Ovvero, quando proposta una scelta, il valore
di default deve essere quello privacy-respecting.

Privacy by design è un concetto introdotto nel 2012 da Ann Cavoukian e consiste nell'integrale
in maniera fondamentale, già dal principio di ogni sistema o applicazione, il concetto della
privacy. Esso comprende 7 principi fondamentali tra cui:

* Proattività e non reattività (prevenire è meglio che curare)
* Privacy come impostazione di default
* Privacy nel design del sistema
* Somma positiva, e non zero (sistemi win-win)
* Visibilità e trasparenza
* Creazione di sistemi user-centric

## Novità del GDPR

Il GDPR integra diverse novità rispetto alla direttiva europea per la privacy.

* Pseudonimizzazione
* Responsabilità e responsabilizzazione

Pseudonimizzazione come column encryption oppure dynamic data masking

Responsabilità come nuovi ruoli (titolare del trattamento dei dati, responsabile del trattamento eDPO), trasparenza ed auditing

## Responsability vs accountability

* Responsability (responsabilità): il compito di rispondere e completare controlli
* Accountability (responsabilizzazione): l'abilità o il dovere di riportare eventi, controlli e problemi --> Si applica dopo che un evento è capitato

## Data breach, cosa fare

In caso di data breach, a seconda della gravità del fatto, il titolare del trattamento dei dati
deve informare le autorità entro 72 ore. Se l'impatto è negativo sugli utenti, essi dovranno essere informati.

Nel caso contrario, ci sono sanzioni: avvertimento scritto, audit periodici, 10 mln di euro o 2% del fatturato oppure 20 mln di euro o 4% del fatturato in caso di gravi violazioni

## Regole di trasferimento dati UE-USA

* Safe harbor privacy principles (2000 - 2015)
* USA-EU privacy shield

## RBAC vs ABAC

* RBAC: autorizzazione basata sui ruoli (ok per piccole aziende)
* ABAC: autorizzazione basata sugli attributi (migliore, controllo a grana fine)

## Auditing

Processo di validazione ed esame di documenti, processi, dati, procedure e sistemi.

Il processo produce un log di audit, ed esso può essere fatto in modo interno (la stessa azienda)
oppure in modo esterno (un'azienda dedicata all'auditing)

Processo: Planning --> Definizione obiettivi --> Review, verifica e validazione --> Report e documentazione

## K-Anonimity

Framework che attraverso la generalizzazione e la soppressione ha il fine di nascondere un'individuo in una tabella tra altri k-1 individui, e previene da il linking attack con una confidenza > 1/k.

Concetti fondamentali: quasi-identifier, gerarchia di generalizzazione del dominio e del valore

Limiti della K-Anonimity:

* Homogeneity attack: Potrebbe accadere che tutte le tuple con lo stesso valore QI in una tabella hanno lo stesso valore nell'attributo sensibile.
* Background knowledge attack che può portare ad una positive o negative disclosure

## Scelta del K

Il problema della K-Anonimity è scegliere un k adatto alla singola situazione per evitare di generalizzare troppo: la perdita di informazioni è proporzionale alla grandezza del k.
Non esiste un k per ogni problema, ma i seguenti ambienti sono caratterizzanti della scelta:

* Grandezza del dataset
* Variabilità del dominio
* Numero di attributi per record
* Presenza di QI molto simili o meno

## Problemi principali della K-anonimity

* Non è applicabile a dataset enormi (curse of dimensionality)
* Spesso è complessa (vd. Incognito) e computazionalmente problematica
* Introduce del lavoro aggiuntivo che deve essere fatto dalla persona, ovvero distinguere QI da attributi sensibili (non è banale, se sono molto vicini!)
* Non conta la mancanza di diversità su una tabella (vd. l-diversity)

## Algoritmi della k-anonimity

![Algoritmo di Samarati](https://i.imgur.com/OR17U62.png)
![Algoritmo Incognito](https://i.imgur.com/OE434jg.png)
![Algoritmo Mondrian 1](https://i.imgur.com/IHpi6qb.png)
![Algoritmo Mondrian 2](https://i.imgur.com/sKfnWSE.png)
![Algoritmo TopDown](https://i.imgur.com/ed7TiDR.png)

## L-diversity

* Classe di equivalenza: L'insieme di tutte le tuple nella tabella generalizzata i cui valori non sensibili della tabella PT generalizzano in q\*
* Lack of diversity: Quando il numero di tuple riguardanti un'attributo sensibile è molto minore di qualsiasi altro nella tabella (ad es. ci sono 100 tuple, 98 con diabette di tipo 1 e 2 con diabete di tipo 2)
* L-diversity: quando un blocco q\* ha l>2 diversi valori sensibili in modo che i valori + frequenti nel q\* block abbiano tutti la stessa frequenza
* Limiti: soggetto a attacchi basati sulla distribuzione dei valori interna ai blocchi (skewness attack e similarity attack), questi possono essere risolti grazie alla t-closeness

![Attacchi alla l-diversity](https://i.imgur.com/ETqLxhe.png)

## T-closeness

* Un blocco q\* ha t-closeness se la distanza tra la distribuzione di un attributo sensibile nella classe e nella tabella non supera una certa soglia *t*
* Una tabella si dice t-close se tutte le sue classi di equivalenza hanno t-closeness
* T-closeness è un parametro che consente di calibrare il trade-off privacy-utility

La t-closeness utilizza la Earth Mover Distance (EMD) invece che la distanza variazionale oppure la KL-distance perchè dobbiamo tenere conto le distanze di tipo semantico.
La EMD misura la quantità di lavoro necessaria per trasformare una distribuzione in un'altra spostando la massa di distribuzione tra loro.

Problemi t-closeness:

* Soffre ancora del background knowledge attack
* Ogni attributo è un quasi-identifier nel giusto contesto e questo non possiamo controllarlo facilmente

## delta-presence

Tenta di risolvere i problemi dati dalla membership disclosure impostando due parametri delta-min e delta-max tra la probabilità che un attaccante riesca a capire che una persona è all'interno della tabella privata dalla tabella generale.

![Formula e monotonicità della delta-presence](https://i.imgur.com/t2letKx.png)

## Private Distributed Mining

Effettuare un mining locale sulle fonti di dati, ed utilizzare un aggregatore per mettere insieme i risultati ottenuti in un unico risultato.
I dati poi vengono nascosti o generalizzati grazie ai seguenti metodi:

1. Data Obfuscation: obiettivo di nascondere l'informazione protetta --> modificando in maniera random i dati, scambiando valori tra record...
2. Summarization: rendere disponibili solo i *riassunti* dei dati
3. Data Separation: far rimanere i dati in chiaro in locale e i dati in remoto parziali e/o cifrati, eventualmente condividerli solo con una terza parte fidata

## Data obfuscation

I dati originali vengono sporcati andando a modificare la sua distribuzione associata, così facendo si hanno dei nuovi valori che proteggono gli originali. 
Il processo consiste nel: dato originale --> randomizer --> dato sporco --> ricostruzione della distribuzione originale --> algoritmo di classificazione --> modello

Per ricostruire la distribuzione originale ricorriamo all'**original distribution reconstruction**: avendo valori distorti il nostro problema è quello di stimare la funzione
di densità Fx dati i valori distorti wi e la funzione di densità Fy. Molto spesso utilizziamo una stima bayesiana per le funzioni di densità, che fa una stima probabilistica
della funzione originale, rendendo questa stima quando più vicina all'originale possibile.

Utilizziamo il **bootstrapping method** (che utilizza il resampling del dato in input e fa inferenza sul dato dopo il resampling, ovvero resampled == sample)
riuscendo effettivamente a stimare la funzione con l'utilizzo della appena citata con buona confidenza. Unico problema è la complessità dell'operatore,
che include un integrale al denominatore, quindi si tratta di una **stima costosa**. Ci fermiamo utilizzando il **test del chi-quadro** effettivamente verificando
che i dati siano distribuiti in maniera similare.

Costruiamo **alberi decisionali** per classificare dataset: Fase di crescita (dati partizionati per attributo, best split, e ripetiamo su ciascun nodo se i punti non sono della
stessa classe) e fase di Pruning (generalizzazione per evitare *overfitting*, rimozione del rumore statistico)

*Come vengono ricostruite le distribuzioni?*

* Global: ricostruiamo per ogni attributo una volta all'inizio, costruiamo l'albero di decisione utilizzando i dati ricostruiti
* ByClass: prima dividiamo i dati d'addestramento, ricostruiamo ogni classe separatamente, costruiamo l'albero di decisione usando i dati ricostruiti
* Local: dividiamo i dati d'addestramento, ricostruiamo ogni classe separatamente, ricostruiamo su ciascun nodo mentre costruiamo l'albero

Dopo la ricostruzione utilizziamo la **privacy metric** che ci dice con una confidenza c che i dati sono compresi tra x1 e x2.

## Data Separation

Utilizziamo la **Secure Multiparty Computation** che a sua volta utilizza il protocollo di **Oblivious Transfer**: si tratta di un modello semi-onesto (tiene traccia
dei suoi calcoli intermedi), solo a due parti trusted, calcola un'approssimazione ID3-delta al posto del classificatore reale, ovvero una delta approssimazione di
ID3, dove delta ha implicazioni sull'efficienza. *Funziona solo su attributi categorici*

## ID3-delta

1. L'albero viene costruito dalla sola radice, a cui vengono assegnate tutte le istanze di addestramento
2. Scegliamo un attributo guardando l'entropia, che deve essere minimizzata
3. Creiamo tanti nodi quanti sono i possibili valori dell'attributo scelto
4. Ricorsivamente si usano i nodi come nuove radici, e così via

**Privacy preserving ID3**: abbiamo 3 parametri (R set di attributi, C attributo di classe, T set di transazioni, ovvero tuple descritte dall'insieme R)
![ID3](https://i.imgur.com/F1ljCdb.png)

## Differential Privacy

Paradigma che garantisce che vengano raggiunte le stesse conclusioni da una query indipendentemente dalla presenza o meno dell'individuo nel database.

Immagina di avere due database praticamente identici, uno con la tua informazione e uno senza.
La differential privacy assicura che la probabilità che una query statistica produca un risultato
è la stessa sia sul primo che sul secondo. Il fatto che tu ci sia o meno non produce un effetto
significativo sull'output di questa query.

Un esempio molto semplice di applicazione è l'algoritmo randomizzato, che associa ad ogni persona in un set una probabilità random.

![Risposta randomizzata, esempio](https://i.imgur.com/li4GaQl.png)

* **epsilon differential privacy**: la probabilità che la risposta ottenuta dal meccanismo provenga da D1 diviso la probabilità che
  provenga da D2 è minore o uguale a e^epsilon. Devo trovare epsilon in modo che il rapporto sia 1. Se l'algoritmo rispetta la eps-dp
  allora la distanza tra le due distribuzioni sarà al massimo epsilon.
* **epsilon-delta differential privacy**: per ogni coppia di database vicini D1,D2 è estremamente improbabile che il valore osservato
  dal meccanismo D1 sia molto (più o meno) probabile che venga generato quando il database è D1 piuttosto a quando il database è D2.
  delta è, in questo caso, il grado di incertezza introdotto.

![epsilon-delta](https://i.imgur.com/7XUHnl1.png)

La differential privacy *non garantisce*:

1. Che i segreti rimangano segreti, in generale
2. Che delle conclusioni possano riflettere informazioni statistiche sulla popolazione
3. Che ciò che è pubblicamente osservabile (tu fumi), venga nascosto

Definizioni:

* **Sensitivity** di una funzione: calcola quanto vale la differenza di una certa funzione su un DB quando calcolo questa fuznione su database
  che differiscono di una sola tupla. Il massimo di questa variazione è la nostra sensitivity DeltaF. Questo valore rappresenta un upper-bound
  su quanto perturbare un input per preservare la privacy.
* **Meccanismo di Laplace**: Andiamo a sporcare una funzione f(D) con delle Y che sono variabili random tratte dalla laplaciana Lap(DeltaF/epsilon).
  Il meccanismo è applicabile solo su query numeriche e solamente quando sporcare la funzione non porta ad una riduzione drastica dell'utilità.
* **Meccanismo esponenziale**: Obiettivo di far ritornare una risposta precisa *senza* sporcare l'ouput. L'idea è di selezionare il miglior elemento
  da un set preservando però la differential privacy. Il miglior elemento è selezionato grazie ad una funzione di utilità U. Il meccanismo provvede
  a mantenere la DP andando ad *approssimare* il massimo dello score dell'elemento. In altre parole alle volte è possibile che non sia veramente il
  massimo quello che viene ritornato. L'output dell'esponenziale ha la proprietà di essere sempre membro del set di output possibili, e questo
  è fondamentale quando sporcare una distribuzione comporterebbe all'inutilità del dataset (calendario di un evento ad esempio).

### Sensitivity

* Globale: quella solita (non va bene in casi come quello della mediana)
* Locale: quanto varia al massimo una funzione se la calcolo su due sub-set del dataset

Meccanismi di LS:

1. Propose test release: usare un valore compreso tra la local sensitivity e la global sensitivity soddisfando la DP senza rivelare LS
2. Privately bounding LS: calcolare una stima privata di LS senza dare LS stesso
3. Smooth Sensitivity: utilizzare un nuovo parametro di sensitivity per creare un framework chiamato *Sample and Aggregate*. 
   Ovvero suddividere un dato x in k sotto-campioni e calcolare f su ogni sub-valore e aggregarlo dopo con una funzione (es: mediana)
   utilizzando la DP solo in questo passaggio.

## Esempi differential privacy

### Query di conteggio

Esempio: quanti nel DB sono femmine?

* Sensitivity: 1
* Privacy epsilon-differenziale ottenuta con rumore: Lap(1/epsilon)
* Errore previsto: 1/epsilon

Per conteggi multipli m/epsilon invece che 1

### Query di istogramma

Esempio: quanti nel DB ricadono nella categoria X?

* Sensitivity: 1
* Privacy epsilon-differenziale ottenuta con rumore: Lap(1/epsilon)

### Query di media

* Sensitivity: [beta-alpha]/n
* Privacy epsilon-differenziale ottenuta con rumore: Lap([beta-alpha]/n * epsilon)

### Classificatore Lineare

Trovare un vettore w che separa positivi e negativi, e trovare i parametri giusti in modo che
il vettore separi bene le due classi. Utilizziamo l'empirical risk minimization per ottenere un
risultato privato sfruttando esempi di un classificatore lineare.
