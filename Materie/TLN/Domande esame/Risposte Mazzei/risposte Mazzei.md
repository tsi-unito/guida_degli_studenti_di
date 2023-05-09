### Domande Mazzei
>
> Premessa: alcune domande potrebbero non essere più attuali perchè il programma e le esercitazioni cambia di anno in anno
> Le domande sono state fornite da marcoscale98 mentre le risposte da Gabriele Naretto aka SpectralWall

Pesca da un file di domande, quindi escono sempre le stesse ed eventualmente fa dei collegamenti.

### Livello Lessicale

#### Differenza fra HMM (Hidden Markov Model) e MEMM (Maximum-entropy Markov model): spiegazione dei pro e dei contro, differenze a livello di probabilità utilizzate

- **HMM**: modello generativo che usa le regole di bayes. Consideriamo il problema del Pos tagging come un problema di Sequence Labeling, in pratica sceglieremo il Pos più probabile fra quelli possibili. L'algoritmo si divide in due parti:
        1. **Fase di Modelling**: l'obiettivo è data la sequenza di tag $t^{n}_{1} $ e una sequenza di parole osservabili $ w^{n}_{1} $ trovare il tag più probabile in base alle parole e i loro tag. Per esprimerlo in modo più matematico la probabilità ecco la formula: $ t^n_1 = argmax P(t^n_1|w^n_1) $ che semplificheremo come $ t^n_1 = argmax P(w^n_1|t^n_1)P(t^n_1 ) $ ovvero la probabilità dell parola dato il tag diviso la probabilità del tag. In Questa ultima formula $P(w^n_1|t^n_1)$ rappresenta la _likehood_ che dice quale può essere il valore del mio output in base al valore degli osservabili, mentre $P(t^n_1)$ rappresenta la probabilità a priori di quel tag, la probabilità a priori potrà essere riscritta come $P(t_i|t_1)$ ovvero la probabilità del tag i dato il tag i-1. Quindi per riassumere si definisce la probabilità dei vari tag, per farlo si fa un approssimazione (detta di markov) che dice che la probabilità di un tag dipende dal tag che la preceduto e che una parola dipende solo dal tag che l'ha emessa.
        2. **Fase di learning**: l'obbiettivo di questa fase è quello di andare a rendere utilizzabili le formule della fase precedente, per farlo si dovrà andare a prendere un corpus annotato e contare le varie frequenze.
        Per la _probabilità a priori_ $P(t_i|t_1)$ si va a contare la frequenza dei in cui appare il tag $t_i$ preceduto dal tag $t_{-1}$ dividendo poi il totale per il numero di volte in cui appare il tag $t_{-1}$. per la likehood $P(w_i|t_i)$ conto quante volte la parola $w_i$ appare con il tag $t_i$ e divido il totale per il numero di volte in cui appare il tag $t_i$.
        3. **Fase di Decoding**: volendo la probabilità massima dovremo provare tutte le possibili combinazioni di sequenze di tag associate a una frase ma questo causa una esplosione a livello computazionale. Ma per risolvere questo problema ci ricordiamo che gli HMM hanno una memoria di 1 ovvero solo sul tag precedente.
        La soluzione sarà quindi l'algoritmo di **Viterbi** che sfrutta la programmazione dinamica. l'idea è quella di creare un array in cui abbiamo sulle colonne il nostro input, mentre sulle righe i possibili stati. Scorreremo la nostra matrice da sinistra a destra riempiendola con il path più probabile, questo grazie alla formula:    $$v_j(j) = max v_{t-1}(i) a_{ij}b_j(o_t) $$ dove $v_{t-1}(i)$ rappresenta la probabilità del path precedente al momento precedete, $a_{ij}$ è la probabilità transizione dal precedente stato $q_i$ allo stato corrente $q_j$ e $b_j(o_t)$ è la probabilità della likehood dell'osservazione $o_t$ dato lo stato $j$. Riempiendo sempre con i paths più probabili avremo la frase annotata con tutti i pos più probabili.
        (se siete un po confusi guardate [Algoritmo di Viterbi](https://www.youtube.com/watch?v=6JVqutwtzmo>)).
        Ma quali sono i **problemi degli HHM** ?. Alla fine dei conti l'algoritmo usa i bi-grammi, se noi volessimo usare tri-grammi svilupperemmo problemi di _Sparseness_ ovvero tri-grammi che non compaiono nel corpus e _Complessità delle parole_ ovvero la complessità di trattare lettere parole uguali ma con piccole differenze (come le maiuscole)
- **MEMM**: modello discriminativo che usa le regole di bayes. Nelle HMM il ragionamento era "se ti dico che è un nome, qual è la probabilità che sia paolo ?" Ovvero lo stato condizionava l'apparizione di una parola. Mentre nelle MEMM le parole condizionano direttamente lo stato. Abbiamo quindi una probabilità lessicale non una probabilità di verosimiglianza. Andremo inoltre a lavorare su features lessicali, alcune più importanti e altre meno, andremo quindi a pesarle.

  1. **Fase di Modelling**: iniziamo creando dei features template, ovvero scegliamo delle features booleane che ci possono aiutare a individuare il tag migliore. Un esempio di features boolean è "la parola finisce con -oso ?"se la risposta è si probabilmente siamo davanti a un aggettivo. Quindi sono delle regole che scattano per filtrare i possibili pos.
  2. **Fase di Learning**: vogliamo ora massimizzare i pesi delle differenti features che abbiamo scelto in modo da scegliere la giusta etichetta per ogni parola.
  3. **Fase di Decoding**: si utilizza anche in questo caso la tecnica di viterbi che modifica leggermente la probabilità da calcolare come la probabilità di un certo tag dato l'osservabile e i valori dello stato precedente.
  - Ma quali **Problemi hanno le MEMM ?**. Anche in questo caso è presente la _Sparseness_, ma se una parola è sconosciuta si possono usare diversi approcci. Posso usare la tecnica di smoothing ipotizzando che sia un nome, do alla parola sconosciuta la probabilità di tutti gli altri tag, posso usare corpus esterni oppure addotto delle euristiche come le probabilità la distribuzione di probabilità dei vari tag.
- **HMM vs MEMM**: nella fase di Modelling vince MEMM grazie alla possibilità di considerate tutte le features booleane mentre nelle HMM di guarda solo la parola precedente. Nella fase di Learning vince HMM grazie alla sua semplicità, mentre nella fase di Decoding siamo pari poiché entrambi usano l'approcci di Viterbi.

### Livello Sintattico

#### Problema dell'espressività delle lingue naturali, mi parli di Chomsky, della sua gerarchia e delle grammatiche Mildly Context Sensitive

Per parlare di Chomsky bisogna prima introdurre il livello sintattico, il livello sintattico cerca di spiegare le relazioni sintattiche tra le parole. Ovvero quelle relazioni che vogliono mettere in evidenza delle relazioni o strutture che esistono nella lingua, un esempio è che il soggetto e il verbo per quanto lontani nella frase sono collegati.
Quello che si chiede _Chomsky_ è quanto sia complesso il linguaggio naturale e quali strutture caratterizzano una frase (struttura).
la sintassi di una frase riesce a condizionare la semantica, poiché si occupa della formazione delle frasi, dell'ordine delle parole e della formazione dei sintagmi (per sintagmi intendiamo un gruppo di parole che hanno caratteristiche di tipo sintattico).
La sintassi si divide in:

- **_Competence_** : la grammatica in generale, il "sapere" una grammatica, l'insieme delle proprietà intrinseche della lingua stessa. La capacità intrinseca di collegare il significato con il suono stabilendo regole linguistiche. Qua si collocano le <span style="color:red"> _Grammatiche formali_ </span>.
- _**Performance**_: la capacità di ascoltare e comprendere le semantiche del discorso. L'applicazione della grammatica per generare frasi. Qua si collocano gli _algoritmi di parsing_

Arriviamo ora a Chomsky e la sua gerarchia. Chomsky sostiene che nella lingua bisogna concentrarsi più sulla competence (ignorando la grammatica), per questo ci si concentra sulla Linguistic theory ovvero la  <span style="color:red"> grammatica formale </span>. L'idea è usare la grammatica formale per descrivere il linguaggio naturale.
Chomsky inoltre definisce la grammatica generativa, che è composta da:
  
- un alfabeto di simboli terminali (le parole della frase)
- un alfabeto di simboli non terminali (A,C,B, ecc..)
- un simbolo di start (s)
- una serie di regole di produzione (da A vado in BC)

L'idea è che si genera una frase a partire dalla grammatica, ma il punto fondamentale è capire quali regole di produzione servono per farlo.
Quindi per riassumere il quesito principale di Chomsky era capire quanto fosse complesso il linguaggio naturale e le strutture generate dalle grammatiche, ed ha stilato questa gerarchia.
Un prima domanda che si è fatto Chomsky è se il linguaggio naturale fosse lineare, ma lui stesso riesce a dimostrare che il lnguaggio naturale non possa essere prodotto da grammatiche lineari. Quindi si chiede se fossero _Context free_ o _Context Sensitive_. Un certo _Shieber_ dimostro grazie a una frase in tedesco che il linguaggio naturale non era Context free.
Allora Chomsky penso che le grammatiche erano Context Sensitive, per context sensitive intendiamo che la grammatica dipende dal contesto in cui si trova, queste grammatiche sono più potenti di quelle libere dal contesto.
Alla fine però le grammatiche non erano context sensitive ma erano Midly Context Sensitive. Questo stato permette diverse proprietà:
  
  1. Non essere context free
  2. tutte le lingue naturali mostrano solo due tipi di dipendenze incrociate (Nested e cross-serial)
  3. non dobbiamo superare il tempo polinomiale nell’analisi
  4. tutte le frasi hanno la proprietà di crescita lineare

Dallle Midly Context Sensitive sono nate diverse grammatiche:
  
- **Tree adjoining grammars**: utilizza una struttura ad albero e sfrutta delle particolari regole di manipolazione. In particolare la regola di sostituzione che mi permette di cambiare un nodo foglia con un albero
  ![tree adjoin add](foto/tree%20adjoin%20sostituzione.png)
  e la regola di adjoining che mi permette di innestare un albero in un altro albero.
  ![tree adjoin add](foto/tree%20adjoin%20join.png)
  Queste due manipolazioni mi permettono di aumentare di molto il potere generativo dell'albero.
- **CCG (Combinatory Categorial Grammars)**: Grammatiche bottom up (mentre le grammatiche generative sono top down). Si parte dalle foglie ovvero le parole. A ogni parola associo una categoria e la singola parola cerca altre categorie per combinarsi fino a costruire il tutto. Un esempio è il verbo amare, che non sarà altro che un elemento che sta cercando qualcosa alla sua destra (soggetto) e qualcosa alla sua sinistra (complemento oggetto).
- _Linear Indexed Grammars_
- _Head Grammars_

#### Cos'è una grammatica CCG (Combinatory categorial grammar)? Mi parli un po' di questo paradigma

#### Anatomia di un parser

- Un parser è composto da 3 elementi fondamentali:

  1. **La grammatica** usata dal parser (cioè la competence CF,TAG,CCG)
  2. **L'algoritmo** ovvero la performance. Inoltre l'algoritmo ci pone a diverse scelte da affrontare, come la strategia di ricerca (top-down, bottom-up, left to right, ecc..) e l'organizzazione della memoria (back-tracking, dynamic programming, ecc ...).
  3. **L'oracolo** ovvero l'euristica che mi indica quale scelta devo fare, questo oracolo può essere probabilistico, rule-based, ecc...

- A lezione abbiamo visto due principali tecniche di ricerca la top down e la bottom up. Il top down parte da dalla radice S e scendo fino alle foglie, questa strategia ha pero diversi svantaggi, come il generare molti albero che non corrispondono alla soluzione ottima. Un altro problema è la ricorsione a sinistra (tipica delle lingue naturali) che fa riempire la memoria con una struttura dati sempre più complessa e spesso può rischiare il loop. L'approccio bottom up invece è l'opposto ovvero parto dalle foglie cercando di costruire sopra di me l'albero, il problema di questo approccio è la possibilità di generare alberi che non saranno davvero utilizzabili. Oltre ai difetti di entrambi gli approcci abbiamo anche due fenomeni relativi all'ambiguità sintattica. Il "**PP Attachment o Attachment Ambiguity**" nasce dall'incertezza di allegare una frase o una clausola a una parte della frase. Di solito accade quando una frase ha più di due locuzioni preposizionali. A lezione il professore faceva l'esempio del "cane che insegue i bambini in bicicletta", la frase risulta ambiguità perché sembra che il cane guidi la bicicletta. Il secondo problema di ambiguità è "**Coordination ambiguity**" e nasce dalle frasi come "si può mangiare e bere qualcosa" oppure "si può mangiare e pagare qualcosa". Questa coordinazione tra due verbi causa un esplosione combinatoria, l'esempio della lesione consisteva nella frase "Alcune persone trovano ispirazione nel cucinare le loro famiglie e i loro cani. Altre trovano ispirazione nel cucinare, nelle loro famiglie e nei loro cani". Per risolvere questi problemi ci torna in aiuto il concetto della programmazione dinamica (come per l'algoritmo di viterbi), vogliamo quindi utilizzare sotto strutture ottimali in modo da evitare di ricalcolare ove ci siano sotto problemi sovrapponibili. Il tutto memorizzando le combinazioni più comuni. Andiamo quindi a barattare spazio con tempo. Un esempio di programmazione dinamica nei parsing è l'algoritmo CYK.

#### CKY: spiegazione, a cosa serve, simulazione su carta

- **Algoritmo CKY (o CKY a detta del prof)** (Cocke Kasamy Younger): algoritmo di parsing che calcola tutti i possibili alberi di parsing in tempo $O(n^3)$. Il maggiore e che il caso peggiore e le caso medio coincidono. Per funzionare l'algoritmo deve avere una grammatica in CNF (Chomsky Normal Form); quindi non deve esistere una regola $A \rightarrow \phi$ dove $\phi$ è uno spazio vuoto. L'algoritmo utilizza come gia detto la programmazione dinamica con euristiche rule-based (Oracolo ruled based). L'idea dietro l'algoritmo e che se abbiamo due regole $A \rightarrow BC$ e $ D \rightarrow BC$ gli stessi sotto alberi usati per A saranno utilizzabili per D, a patto di memorizzarli. Ma quindi quali sono gli step di questo algoritmo ?.
Bene la risposta è dipende, in pratica a lezione il prof lo spiega in un modo, se cercate su internet gli esempi che trovate sono fatti con grammatiche più semplici, quindi per tagliare la testa al toro metto il link di un video che fa lo stesso esempio che fa il prof a lezione, con tanto di spiegazione e simulazione. [Algoritmo CYK](https://youtu.be/O-x3krZ3A-Q).

  Io proverò comunque a dare una spiegazione del funzionamento dell'algoritmo (almeno quello visto a lezione). L'algoritmo presentato a lezione cerca di mappare le regole su un array bidimensionale. nella regola A $\rightarrow$ BC, la A andrà da _i to j_, se questo accade allora ci sarà un k in cui avremo _i<k<j_. Avendo questa proprietà B va da _i to k_ e C va da _k to j_.
  Vediamo quindi come funziona l'algoritmo:
  - _for j from 1 to length(word) do_ : facciamo un ciclo che va da 1 fino al numero di parole. <span style="color:red"> Questo ciclo scorre le colonne </span>.
    - _for all {A|A $\rightarrow$ word[j] $\subset$ grammar}_: Per tutte le parole in posizione j che vengono generate da una grammatica
      - _table[j-1,j] $\leftarrow$ tabel[j-1,j] $\cup$ A_ : allora aggiungiamo la regola A alla tabella nella posizione j-1,j. <span style="color:red"> Questa operazione riempie le celle inferiori </span>.
    - _for i $\leftarrow$ from j-2 down to 0 do_ : questo ciclo scorre andando da j-2 fino a 0. Teoricamente <span style="color:blue"> Questa operazione riempie la riga i nella colonna j</span>.
      - _for k $\leftarrow$ i+1 to j-1 do_ : questo ciclo rappresenta il k (ovvero il mezzo tra B e C) e ci permette di <span style="color:green"> luppare su tutte le possibili posizione di split tra i e j </span>.
        - _for all {A | A $\rightarrow$ BC $\cup$ grammar and B $\cup$ table[i, k] and C $\cup$ table[k, j]}_ : in questo punto facciamo un loop su ogni elemento della grammatica in cui esiste la regola A $\rightarrow$ BC, in cui B è compreso nei punti della tabella [i,k] e C è compreso nei punti della tabella [k,j]
          - _table[i,j] table[i,j] $\subset$ A_ : E per ognuna di queste regole mi salvo A nella cella [i,j].

  Quindi dicendolo a parole mie: eseguiremo il ciclo più esterno per il numero di parole nella frase, il primo sotto ciclo partirà a ogni parola e riempirà le celle più al fondo della matrice con le regole che derivano quelle parole. Il ciclo di i e k partiranno solo all'ultimo ciclo di j, dopo che avremmo assegnato in tutte le celle inferiori un pos. I cicli di i e k ci permettono di risalire la matrice andando a riempirle con le regole che generano i pos delle parole, fino a risalire alla cima dell'albero.

- Come sappiamo CYK genera tutti i possibili alberi di parsing, ma questo ci obbliga a dover scegliere noi quale sia quello più corretto. Per questo ci viene in aiuto una variante di CYK, in cui l'oracolo non sarà più rule based ma **probabilistico**, inoltre utilizzerà i beam. Questa variante si basa sull'ipotesi che la probabilità di un albero è il prodotto delle regole usate nella sua derivazione, che ci porta alla formula $P(T,S) = \prod_{node\in T} (P(rule(n))$. Quindi andremo a prendere ogni volta la regola più probabile, cosi da avere alla fine l'albero più probabile.
  
#### Mi parli degli algoritmi di parsing delle grammatiche a dipendenze

- **CKY Probabilistico**: per vedere CYK e CYK probabilistico vedere la domanda precedente.

- **Parsing Parziale**: per utilizzare questo parsing dobbiamo fare un approssimazione e dire che il linguaggio naturale non e CF. Il concetto fondamentale di questo Parser e il Chunck ovvero una frase semplice non ricorsiva. Come base sintattica quindi non avremo la ricorsione, questo ci porta ad avere un processo di parsing molto più veloce. Una domanda che può venire in mente è "Che differenza c'è tra un sintagma e un chunck ?". Ebbene il sintagma è ricorsivo e può contenere al suo intero altre strutture. Il chunck non ha ricorsività, ma per rappresentare un concetto deve avere più chunck. Questo parser avrà inoltre un approccio bottom-up, userà la programmazione dinamica e il suo oracolo sarà rules based. Un modo per utilizzare questi algoritmi di parsing e il BIO tagging.

- **Parsing a dipendenze**: Una serie di parsing che si basano sulla dipendenza delle parole da altre. Per dirla in modo più formale "_La sintassi a dipendenze postula che la struttura sintattica consiste di elementi lessicali connessi da relazioni binarie asimmetriche (graficamente frecce) chiamate dipendenze_". Le dipendenze sono normalmente tipate con il nome si una relazione grammaticale (subject, prepositional object, apposition, etc.).Le dipendenze connettono una head (governor, superior, regent) con un dipendente (modifier, inferior, subordinate) Solitamente un albero (grafo connesso, aciclico, singlehead). Vediamo un esempio:
![Parsing a dipendeze](foto/parsing%20a%20dipendenze.png)
Abbiamo parlato di head e di dipendente, ma come riconoscerli ?.

  - Head determina la categoria sintattica di una Costruzione
  - Head può sostituire la Costruzione
  - Head è obbligatorio mentre il Dependent può essere opzionale
  - Head seleziona il Dependent e determina quando questo è obbligatorio
  - La forma del Dependent dipende dall'Head (agreement)
  - La posizione nella frase del Dependet è specificabile con riferimento al'Head
  - Head determina la categoria semantica di una Costruzione

  Vediamo ora una lista di algoritmi di parsing a dipendenze:
  
  1. Programmazione dinamica: Algoritmo simile a quello di CKY ma il problema era che erano troppo complessi, fino a che Eisner (1996) scopri un algoritmo con complessità $O(n^3)$ . Per usare tale algoritmo l’idea è quella di trasformare un albero a dipendenze in un albero simile a quello a costituenti. Questo algoritmo però è troppo lento.
  2. **Algoritmo a Grafo**: consideriamo un grafo che rappresenta tutte le possibili relazioni tra due elementi/nodi, supponiamo di riuscire a dare un peso a queste relazioni, questo ci porta a usare li minimum expanding tree per trovare il minimo grafo di copertura. Per trovare questi pesi utilizzo un corpus di riferimento che mi permettono di scegliere un peso in base alla probabilità delle parole. Utilizzo l'algoritmo di MST. Infine posso usare dei classificatori per categorizzare gli archi. Ci sono algoritmi nuovi per usare i pesi (vedere il libro di Jourawsky se non cambia)
  3. Parsing a costituenti e conversione: parto da un albero a costituenti qualsiasi, cerco di convertire tale albero in un albero a dipendenze sulla base di una conoscenza di tipo linguistico che mi dice appunto l’equivalenza tra i costituenti e le dipendenze. In questo modo cerco di usare la linguistica come un euristica. Per fare questa conversione uso delle “tabelle di percolazioni”.
  4. Parsing deterministico: Faccio scelte greedy per la creazione di dipendenze tra parole, guidate da machine learning classifiers
  5. Soddisfazione di vincoli: Vengono eliminate tutte le possibili dipendenze che non soddisfano a certi vincoli (hard)

- **MALT**: Un dei parser più importanti. Usa grammatiche a dipendenza, algoritmi bottom-up, una memory organizzation depth-first (senza backtracking grazie al fatto che è probabilistico) e abbiamo appunto un Oracolo probabilistico.
Si dice deterministico per il fatto che data una qualsiasi sequenza di parole si avrà sempre una soluzione ovvero un albero in uscita, a differenza del CKY, in cui il parser può fermarsi non appena si rende conto che non è derivabile. L'algoritmo funziona come una sorta di automa, gli stati di questo automa vengono definiti dal contenuto di 3 strutture:

  - Un _Input buffer_ che contiene le parole rimanenti da analizzare
  - Uno _Stack_ per le parole attualmente analizzate.
  - Un _Dependecy relations_ contenente tutte le dipendenze create fino a ora.

  Quindi in un qualunque momento potremo trovarci in uno stato che è la diversa combinazione delle 3 strutture. Ci sono due stati fondamentali che dobbiamo conoscere:
  
  - Lo _stato iniziale_, che è composto da Stack vuoto (in verità contiene il root), input buffer pieno (ovvero contiene tutte le frasi) e lista di Dependency relations vuota.
  - Lo _stato finale_, che è composto da Stack vuoto, Input buffer vuoto e lista di Dependency relations piena (contiene tutte le dipendenze create fino a ora).

  Spiegate le strutture dati e i principali stati parliamo del Funzionamento del parser. Il parser scorre la frase da sinistra verso destra spostando gli items dal Input Buffer allo Stack. A ogni passo analizzo i 2 elementi in testa allo stack el'oracolo fa una decisione sulla transizione da applicare. Le transizioni corrispondono alle azioni intuitive che uno dovrebbe fare per creare un albero a dipendenze leggendo le parole da sinistra a destra. Queste transizioni sono di 3 tipi:

  - _Shift_: prende la prossima parola dalla lista delle parole in input e la pusha sullo stack (rimuovendola dalla lista delle parole in input). Un esempio è passare da $[[root]], [I, booked, morning, flight], [()]$ allo stato $[[root], I], [booked,morning,flight], [()]$.
  - _Left_: questa operazione fa due cose
    1. crea una dipendenza $(a,b)$ tra la prossima parola della lista $(a)$ e la parola in cima allo stack $(b)$. $\rightarrow$ $[ [root, I], [booked, a, morning, flight], [()]]$
    2. Rimuove la parola in cima allo stack (effettua un pop sullo stack). $\rightarrow$ $[[root], [booked, morning, flight], (booked,I)]$
  - _Right_: questa operazione fra 3 cose:
    1. crea una dipendenza $(b,a)$ tra la prossima parola della lista $(a)$ e la parola in cima allo stack $(b)$. $\rightarrow$ $[ [root, I], [booked, a, morning, flight], [()]]$
    2. Rimuove la parola dalla lista in input. $\rightarrow$ $[[root], [booked, morning, flight], (booked,I)]$
    3. Mette la parola b sulla in cima alla lista in input rimuovendola dallo stack. $\rightarrow$ $[[root, booked], [flight], [()]]$ diventa $[[root], [booked], [(booked, flight)]]$
    **Nota:** in pratica il rigth prende la parola in cima alla lista in input crea una dipendenza con la parola in cima allo stack, dopodiché toglie la parola in input e rimette la parola dello stack nella lista in input.
    Vediamo un esempio di esecuzione: ![malt esecuzione](foto/malt%20esecuzione.png)

  Questo parsing segue un algoritmo molto semplice, parte dallo stato iniziale, per ogni stato in cui si trova chiede al'oracolo cosa fare fino al raggiungimento dello stato finale.
    Questo Parser ha due principali problemi:
    1. Non dice il tipo di dipendenze che ci sono tra parole. Per risolvere questo problema si potrebbero modificare le operazioni, trasformandole in Left_subj, Right_subj, Left_obj, Right_obj, Shift. Questo ci permetterebbe di creare delle dipendenze tipate. Vediamo un esempio $[[root]], [I, booked, morning, flight], [()]$ dopo un lft-subj diventa $[[root], I], [booked,morning,flight], [(subj(booked),I)]$.
    2. Il secondo difetto è dovuto a uno dei punti principali del parser, ovvero come fa l'oracolo a sapere quale scelta è la migliore ? (anche perché l'algoritmo è letteralmente tutti nell'oracolo). Ebbene la soluzione è la costruzione di un modello di machine learning che decide la transizione da eseguire. Per fare questa operazione abbiamo bisogno di diverse cose:

        - In primis bisogna capire quali sono le features linguisticamente significative. Un esempio potrebbe essere che se sul top dello stack c’è un articolo e la prossima parola da prendere è un nome, allora dovrò un Left
        - Bisogna inoltre costruire il dataset di addestramento attraverso delle dependecy treebank. Inoltre attraverso il reverse engineering vado a trovare le regole che portano la frase nell'albero.
        - Infine ho bisogno di usare un buon training algorithm per il modello di machine learning, questo in modo da massimizzare lo score del modello per tutte le configurazioni nel training set.

- TUP - Turin university Parser (Parser a regole per dipendenze a vincoli): Un parser che sfrutta una grammatica a dipendenze (vincoli), con algoritmo bottom up, memory organizattion depth-first ed euristica rule based.
L’idea era di dividere il sistema di parsing in 3 fasi:

  1. Chuncking: la frase in ingresso viene suddivisa in chunck.
  2. Coordination: rimuovo l’ambiguità delle congiunzioni con delle regole
  3. Verb sub categorization: ovvero regole verbali basate su una tassonomia di classi per la sotto categorizzazione

  Questo parser è molto veloce.

- Valutazione: Come valutiamo le soluzioni dei diversi alberi di parsing ?. Si usano precisione e recall con il concetto di gold tree (ovvero l'albero corretto).
  
  - Precision: Quale percentuale di subtree del system tree sono anche nel gold tree?. Ovvero ci chiediamo "Quanto di quello prodotto dai nostri paerser è effetivamente giusto ?"
  - Recall: Quale percentuale di subtree del golden tree sono anche nel system tree?. Ovvero ci chiediamo "Quanto di quello che è giusto è stato prodotto dal nostro parser ?"  

  Queste due metriche insieme sono dette parsival. Inoltre per le valutazioni possiamo anche controllare F-score, Unlabelled vs. Labelled e l'accuracy.

#### Mi parli dell'ambiguità sintattica (PP attachment e Coordination, con esempi)

Esistono due principali fenomeni dell'ambiguità sintattica e sono dovuti al fatto che vengono generati diversi alberi sintattici. I due fenomeni sono:
  
- **Attachment Ambiguity o PP Attachment**: L'ambiguità deriva dal fatto che il soggetto può essere posizionato in punti diversi dell'albero perché può essere collocato vicino al sostantivo oppure vicino al verbo. Potremmo definirlo formalmente come "_il caso quando un costituente della frase può essere attaccato a più parti della frase_". L'esempio fatto a lezione è "_One morning I shot an elephant in my pajamas. How he got into my pajamas I don’t know._" dove l'ambiguità sta nel fatto che non si capisce se l'elefante indossasse il pigiama o se il soggetto indossi un pigiama che ha dei disegni di elefanti. Questo problema si nota molto a livello dell'albero, poiché generiamo due alberi diversi. ![Esempio Lezione](foto/Ambiguit%C3%A0.png)
Un altro esempio fatto a lezione è la vignetta "_Potresti mettere il guinzaglio al tuo cane? Sta inseguendo i bambini in bicicletta_" da cui arriva la risposta "_Il mio cane non ha la bicicletta_". L'ambiguità è la stessa del primo esempio.

- **Coordination Ambiguity** si vanno a cordinare due verbi o due nomi e si aumenta la complessità nel capire a quale dei due ci riferiamo. Potremmo definirlo come "_quando un modificatore potrebbe essere attaccato a un'intera congiunzione o solo a un singolo componente della congiunzione_". Un esempio di questa ambiguità è la frase "_Si può mangiare e bere qualcosa_" oppure "_Si può mangiare e pagare qualcosa_". Il problema di queste due frasi è che non si capisce se vogliamo "mangiare qualcosa" o "bere qualcosa" o entrambe.

Dalle slide si può vedere che queste ambiguità possono portare a un grande aumento della complessità e per mitigarle possiamo usare la programmazione dinamica.

### $\lambda$ /lambda Calcolo (Semantica Composizionale)

#### A cosa serve il lambda calcolo?

Come siamo arrivati al lambda calcolo e a cosa serve ?. Prima di tutto il discorso è venuto fuori quando abbiamo iniziato a parlare del livello semantico. Parlando della semantica ci chiediamo non più chi è il complemento oggetto di una frase o qual è il verbo, ma ci chiediamo chi compie la frase e sopratutto come rappresentarlo.
Una prima idea per rappresentare la semantica è stata la logica del prim'ordine (FOL), questa logica rappresenta bene la logica dei verbi. Quindi per l'esempio "_Paolo Ama Francesca_" avremo "(S (NP Paolo) (VP (V Ama) (NP Francesca)))" a livello sintattico che potremo rappresentare come "love(Paolo,Francesca)". Questo procedimento è quello che chiamiamo Computantional Semantics è il suo procedimento è il seguente:

  1. Parsificare la frase per ottenere l'albero sintattico (costituenti)
  2. Cercare la semantica di ogni parola nel lessico
  3. Costruire la semantica per ogni sintagma (questo approccio e bottom up e syntax driven $\rightarrow$ "rule to rule transaltion")

  A questi argomenti si uscire il concetto di composizionalità di Frege che dice che "il significato del tutto è determinato dal significato delle parti e dalla maniera in cui sono combinate".
  Quindi per riassumere noi prendiamo la frase ci costruiamo l'albero e salendo dalle foglie verso l'alto capiamo come assegnare le costanti alla formula $love(?,?)$. Ma qua sorge un problema, ovvero come rappresentiamo i pezzi di formula come $love(x,francesca)$ oppure costrutti più complessi come "tutti amano francesca" che in FOl sarà $\forall x(love(x,francesca)))$.Ed è proprio in questo contesto che iniziamo a parlare di Lambda calcolo. $\lambda$ è un operatore che ci consente di rendere dinamico il FOL (che è statico) infatti ci consente di indicare/segnare/legare (il professore usa il termine **bind**) le variabili che non hanno ancora un argomento (quindi variabili libere), cioè l'informazione mancante. Per continuare il nostro esempio avremo quindi $\lambda x.love(x,francesca)) @ paolo$. Per passare dalla versione $\lambda$ alla versione FOL dovremo effettuare un passaggio detto **Beta reduction**, gli step per eseguire questa riduzione sono i seguenti:

- Partiamo dalla versione base $\rightarrow$ $\lambda x.love(x,francesca)) @ paolo$

  1. Eliminare il $\lambda$ $\rightarrow$ $love(x,francesca)(paolo)$
  2. Rimuoviamo l'argomento $\rightarrow$ $love(x,francesca)$
  3. Rimpiazziamo le occorenze della vartiabile legata dal $\lambda$ con l'argomento in tutta la formula $\rightarrow$ $love(paolo,francesca)$
  bisogna ricordarsi che quando usiamo i $\lambda$ l'ordine della frase deve essere preciso.

  **Nota:** il prof chiede l'esempio visto a lezione con la frase "Paolo ama Francesca" e la sua traduzione in lambda calcolo. Quindi guardate le slide.

Questo utilizzo di $\lambda$ + FOL è anche chiamato **Semantica di Montague** e ci permette di rappresentare parole e sintagmi mancanti. Si basa sull'idea che non esiste una grande differenza teoretica tra il linguaggio naturale ed il linguaggio logico. Possiamo quindi usare principi matematici e regole logiche per analizzare qualsiasi lingua. Inoltre si basa sul principio di _composizionalità_. La composizionalità ci permette di definire le regole per specificare quale sia la funzione e quale l'argomento (queste regole sono grammatiche a cui è stata aggiunta un informazione in più, sono anche dette augmented CFG). Un esempio di regola di composizione è il seguente "$ VP:f(a) \rightarrow V:f $ $NP: a$" che indica che quando ho un tag VP il suo figlio sinistro sarà V e il suo figlio destro sarà NP. Un altra regola è "$S:f(a)\rightarrow NP:a$     $VP:f$", questa regola indica che S ha  come figlio destro NP e come figlio sinistro VP

- Andiamo anche a rappresentare ogni tempo verbale con tre tempi
    1. **U** Utterance (quando viene detta la frase)
    2. **R** Reference time
    3. **E** Time of the Event

  Vedere le slide per l'immagine con tutti vari tempi verbali.

#### Come rappresentare articoli e sostantivi nel lambda calcolo?

- **Come gestire gli articoli** nel $\lambda$ calcolo ?

  Gestire gli articoli è molto più complesso di gestire nomi e verbi. Questo è dovuto al fatto che nomi e verbi sono vocaboli "di contenuti" e dunque si spiegano da soli indipendentemente dal contesto. Questo invece non si può dire per gli articoli che hanno una semantica più complicata. Vediamo una frase con l'articolo "Un" in particolare l'esempio _"Un uomo ama francesca"_. Partiamo dal fatto che "un uomo" significa che "c'è un uomo" e non "un uomo", per risolvere questo esempio dobbiamo fare reverse engineering quindi partire dalla frase finale.
  1. Un uomo ama francesca
  2. $ \exist z(man(z) \wedge love(z,francesca)) $
  3. $ \exist z(\lambda y.man(y)(z) \wedge \lambda x.love(x,francesca)(z)) $
  Quella al punto 3 è la frase in $\lambda$
  Ma vediamo come aggiungerci il predicato "un"
  4. $ \lambda Q.\exist z ((\lambda y.man(y)(z) \wedge Q(z))) (\lambda x.love(x,francesca)(z)) $
  5. $ (\lambda P.\lambda Q \exist z(P(z) \wedge Q(z)))(\lambda x.love(x,francesca)) (\lambda y.man(y))$
  In particolare questa stringa corrisponde a Un =  $\lambda P.\lambda Q \exist z(P(z) \wedge Q(z))$. In pratica stiamo dicendo che se c'è l'articolo "un" allora ci sarà un NP alla destra, quindi stiamo  già prevedendo un predicato binario in sostanza.
  Abbiamo inoltre le seguenti regole di composizione
  1. $ VP:f(a) \rightarrow V:f $ $NP: a$
  2. $S:f(a)\rightarrow NP:f$     $VP:a$

  La Struttura dell'albero sarà quindi la seguente:
  ![Albero di "Un uomo ama francesca"](foto/un%20uomo%20ama%20francesca.png)
  Possiamo vedere che le regole di composizionalità seguono questo albero.

- Ma adesso sorge un problema, Ovvero la **gestione dei nomi propri**. Se provassimo a produrre l'albero di "Paolo ama francesca" non rispetteremmo le regole di composizione. Questo perché paolo è un predicato che arriva da sinistra dell'albero ma si applica come un argomento destro . La regola che fa scattare questo problema è $S:f(a)\rightarrow NP:f$ $VP:a$ sull'esempio $(paolo) @ (\lambda x.love(x, francesca))$. Per modellare questo procedimento faremo type-raising (mi permette di invertire un val  ), cambiando il tipo "paolo" in nuovo tipo. Di base andiamo a usare ancora una volta l'espressione $\lambda$ ma su paolo. Trasformando la frase $(paolo) @ (\lambda x.love(x, francesca))$ $\rightarrow$ $\lambda P.P(paolo) @ (\lambda x.love(x, francesca)) $, che verrò trasformata in $love(paolo,francesca)$ dopo la beta reduction.
**Nota:** Questo procedimento andrà fatto anche per francesca.

- Vediamo ora le **gestione dei verbi transitivi**, il problema e che vogliamo ora gestire il verbo "ama Francesca". Il modo in cui abbiamo gestiti ama $ \lambda y.\lambda x.love(x, y)$ e il modo in cui abbiamo gestito il nome proprio francesca $\lambda P.P(francesca)$ non posso essere fusi insieme nell'albero (non ho capito perchè, dalle slide abbiamo solo una X rossa). Andiamo quindi a rifare type-raising e otteniamo cosi $\lambda R.\lambda x.R(\lambda y.love(x, y))$.
![Albero di "Ama francesca"](foto/ama%20francesca.png)

- Un altro punto da gestire sono le frasi con **coordinazione di nomi**. Un esempio è la frase $run(Paolo) \wedge run(Francesca)$, per gestire questa frase dovremo aggiungere al lessico l'istruzione $\lambda$ apposita che indica "e". Come nella frase "Paolo e Francesca corrono", questa frase sarà uguale a $ \lambda x.\lambda y.\lambda R.(x(R) ∧ y(R)) $
![Albero di "Paolo e Francesca corrono"](foto/paolo%20e%20francesca%20corrono.png)

- Vediamo ora gli **esistenziali** come "Ogni uomo ama una donna", quindi dobbiamo provare a gestire l'operatore $\forall$. Questa formulazione in $\lambda$ corrisponde a $\lambda Q\lambda P(∀x (Q(x) → P(x)))$

- Infine ci sono gli **avverbi**, come la frase "Paolo ama Francesca dolcemente". Il problema degli avverbi che si tratta i logica del second ordine $sweetly(love(P,F))$, inoltre il prof si pone la domanda di avere avverbi annidati come $love(P,F,sweetly)$. La soluzione sarà reificare l'evento, ovvero definire una variabile che identifica l'evento (in stile Davidson), in logica la rappresentiamo come $\exist e$ $love(e,P,F) \wedge sweetly(e)$. Inoltre possiamo generalizzare anche gli argomenti (neo-davidsonian style) facendoli diventare $∃e$ $love(e) ∧ agent(e,P) ∧ patient(e,F) \wedge sweetly(e)$.
**Nota:** Un eserzio richiede di sviluppare la derivazione (ovvero albero più lambda-Fol) di questa frase.

Quindi alla fine avremo le seguenti regole

  1. Nomi comuni uomo $\lambda x.man(x)$
  2. Nomi propri Paolo $\lambda P.P(Paolo)$
  3. Verbi intransitivi corre $\lambda x.run(x)$
  4. Verbi transitivi ama $\lambda R.\lambda x.R(\lambda y.love(x, y))$
  5. Articoli un $\lambda P.\lambda Q.∃z(P(z) \wedge Q(z))$
  6. Congiunzioni e $\lambda x.\lambda y.\lambda R.(x(R) \wedge y(R))$
  7. Esistenziali ogni $\lambda Q\lambda P(∀x (Q(x) → P(x)))$
  8. Con le solite due regole di composizione, una per la composizione del verbo $ VP:f(a) \rightarrow V:f $ $NP: a$  e una per la composizione di S $S:f(a)\rightarrow NP:f$     $VP:a$
  
Inoltre il professore segnala i punti chiave che sono

 1. extra $\lambda$ per gli NP
 2. astrazione sui predicati
 3. inversione di controllo: NP_subj come funzione e VP come argomento

#### Semantica formale e composizionale con esercizio (montague)

#### semantica di montague (fol + lambda calcolo) e la sintassi a dipendenze

#### Tipi di ambiguità

Qual'è la differenza fra ambiguità sintattica e ambiguità semantica ?

- Ambiguità sintattica: essendo il livello sintattico l'ambiguità è nella struttura stessa della frase e non a livello di significato. Un esempio è la frase "_Mangio la pizza con le acciughe_", in questo esempio per ogni albero generato avremo un valore univoco in FOL. Possiamo dire quindi che l'ambiguità sintattica avviene quando vengono generati differenti alberi sintattici ma questa non da problemi a livello semantico.
- Ambiguità semantica: quando parliamo di semantica intendiamo il non capire il significato della frase, un esempio è la frase "_Tutti gli uomini amano una donna_" in questo caso viene generato un solo albero sintattico ma i significato in FOL possono essere 2:
  1. $\forall x(man(x) \rightarrow \exist(woman(y) \wedge love(x,y))) $ ovvero che per ogni uomo esiste una donna, e l'uomo ama quella donna.
  2. $ \exist woman(y)\forall (man(x) \rightarrow love(x,y)) $ ovvero che esiste una donna che è amata da tutti gli uomini.

  Quindi quando parliamo di ambiguità semantica intendiamo che abbiamo un solo albero ma che ha più significati in FOL.

### NLG

#### Nell'NLG, quali sono le differenze tra i task di _Referencing Expression_ e _Lessicalizzazione_?

Come per le altre domande partiamo dall'inizio, analizzando una a una tutte le fasi di natural language generation.

  1. **Content determination**: questa prima fase vuole rispondere alla domanda "cosa devo dire ?". I messaggi provengono da una struttura dati, sono aggregazioni di dati che possono essere espressi linguisticamente, con una parola o un sintagma e sono basati su entità, concetti e relazioni sul dominio. Quello che il prof vuole intendere è che e che tramite una struttura dati che mi permette di creare le frasi di cui ho bisogno. A lezione fa l'esempio della struttura "DEPARTURETIM (CALEDONIANEXPRESS, 1000)" che indica che $\rightarrow$ "The Caledonian Express leaves at 10am"
  2. **Discourse planning**: Un testo non è solo una collezione casuale di frasi ma c'è una struttura che relaziona le frasi. Esistono quindi delle relazioni tra i messaggi che mandiamo. Abbiamo due tipi di relazioni:

      - Conceptual grouping (Relazione concettuale): Le due frasi sono affiancate poiché parlano dello stesso argomento, ma  non c'è un legame tra loro. Un esempio è la frase "il prossimo treno è il treno A e parte alle 10"
      - Rhetorical relations (Relazioni Retoriche): Ovvero che una delle frasi e un elaborazione dell'altra. Un esempio è la frase "Il prossimo treno è il Caledonian Express è parte da Abernati alle 10"
  3. **Sentence aggregation**: Non si possono semplicemente sputare fuori delle frasi una dopo l'altra, lo scopo di questo livello è quello di aggregare e combinare le frasi in modo che sembrino frasi più naturali ma anche più complesse. Il risultato è un sentence specification o un sentence plan. Questo livello può creare due tipi di frasi:
      - Senza Aggregazione: un esempio è "Il prossimo treno è il Caledonian Express" seguito dalla frase "lascia Abernati alle 10" (quindi due frasi che si susseguono)
      - Con Aggregazione: un esempio è "il prossimo treno lascerà la stazione alle 10am, si tratta del Calendonian Express" (quindi una frase più complessa)
  4. **Lexicalisation**: La lessicalizzazione determina quali parole usare per esprimere i concetti e le relazioni del dominio (classi -> Nomi Comuni,Verbi,...), inoltre controlla quali relazioni sintattiche usare per le parole. Un esempio sta nell'adattare per esempio la frase "Departure Time", devo esprimerla come  “partire” o “decollare” ?. In aeroporto si direbbe decollare, ma a una stazione dei treni si usa partire. Inoltre si occupa di decidere se utilizzare frasi attive o passive come "il cane morde l'uomo" oppure "l'uomo e morso dal cane".
  5. **Referring expression generation**: Generare le “referrinhg expressions” determina quali parole usare per esprimere le entità del dominio in una maniera comprensibile all'utente. Quindi dobbiamo iniziare a trattare i nomi propri. Deve anche andare a bilanciare fluenza e ambiguità. Un esempio è la frase "Alessandro Mazzei" e "il professore che tiene il corso di lezioni di TLN" si riferiscono allo stessa persona. Invece di ripetere il soggetto potremmo utilizzare dei pronomi per rendere la frase più naturale.
  6. **Syntactic and morphological realisation**: Questo livello è estremamente importante e si occupa di gestire le regole morfologie e sintattiche. Ogni lingua ha una morfologia (come si formano le parole) e una sintassi (come si formano le frasi), questo livello quindi dato come input l'albero sintattico a dipendenze genera la corretta sequenza morfologica rispettando la grammatica della lingua naturale selezionata.
  Un esempio di **regola morfologica** è "per formare il passato di un verbo aggiungi “ed”" che porta a $\rightarrow$ "walk + ed = walked". Un esempio do **regola sintattica** è "il soggetto va prima del verbo" che porta a $\rightarrow$ John walked.
  7. **Orthographic realization**: Questo ultimo livello si occupa della parte "estetica" delle frasi generate. In particolare le lettere maiuscole, la punteggiatura ma anche la formattazione del font. Di base si usano le **regole ortografiche**, degli esempi di regole ortografiche sono $\rightarrow$ "Le frasi cominciano con una lettera grande" oppure "Le frasi dichiarative finiscono con un punto" o anche "Se l'ultima parola è un'abbreviazione con un punto allora non va aggiunto il punto finale"

#### Nell'architettura finale dell'NGL, perché abbiamo visto che è divisa in 3 fasi?

Le 7 fasi del NGL formano una pipeline di generazione.
Le tre fasi sono:

- **Text Planning**:
  
  - Content determination
  - Discourse planning
- **Sentence Planning**:

  - Sentence aggregation
  - Lexicalisation
  - Referring expression generation
- **Linguistic Realization**:

  - Syntax + morphology
  - Orthographic realization

Ma perché queste fasi sono strutturate così ?. Principalmente per due motivi, quando genero un testo si usano due fonti di conoscenza. La prima è la **fonte del dominio**, mentre le seconda è la **fonte del linguaggio**. Ma vediamo meglio quali di queste fonti vengono utilizzate nelle tre fasi.
Nel _Text Planning_ è importante il dominio che andiamo a utilizzare e la conoscenza che questo possiede, in questa fase non ci importa la lingua.
Nel _Sentence Planning_ è importante sia il dominio che la lingua, perché per capire che parole utilizzare bisogna tenere conto della lingua, ma comunque non bisogna dimenticarsi del dominio.
Nel _Linguistic Realization_ è importante solo la lingua, perché in questa fase si va a generare il testo vero e proprio e quindi tutte le regole sintattiche, ortografiche e morfologiche, che appunto sono intrinsecamente legate alla lingua.  

### Dialogue Systems and ChatBotsFile

#### Piccolo ripasso sui Dialog system

Partiamo dicendo che un dialogo è un attività collaborativa. Un attività poiché ha un goal sociale e comunicativo, soggetta a un costo da minimizzare e composta da un sequenza temporale di eventi (ovvero i turni di dialogo). Collaborativa perché si coordinano coloro che ne fanno parte, richiede comprensione comune e perché rende le interazioni più efficienti. Vediamo quali sono le 4 chiavi del dialogo:
  
  1. I partecipanti del dialogo seguono dei turni, questi turni vengono sanciti da diversi segnali, come il silenzio (cambio turno) i segni di esitazione, le intonazioni  oppure il linguaggio del corpo.
  2. Questi turni sono strutturati in unità comunicative chiamate "_dialogue act o speech act_" , questi atti di dialogo possono essere Assertivi, imperativi, espressivi o dichiarativi
  3. L'interpretazione di questi dialog act e subordinata al contesto della conversazione. Ci 3 aspetti situazioni che caratterizzano un contesto, sono le _frasi che non sono frasi_ $\rightarrow$ ES: alla domanda "quando parti" rispondo "domani". Abbiamo poi le _frasi che implicano_ $\rightarrow$ ES: alla domanda "domani vieni a cena" la risposta "sono molto indaffarato". Infine abbiamo le _frasi anaforiche_ $\rightarrow$ ES: alla domanda "lui ha freddo ?" Devo capire di chi parlo.
  4. I partecipanti della conversazione continuano a mandarsi dei segnali in modo da far intendere se stanno capendo o no la conversazione.

Il prossimo argomento sono i Chat Bot e Dialog System, uno dei primi chatbot che ha trattato è Eliza (1966). La prima versione era solamente rule based, ovvero si andavano a riconoscere dei pattern nelle frasi in input e si trasformava parte della frase per dare una risposta. L'algoritmo ha più o meno il seguente funzionamento, cerca nella frase la parola chiave con ranking più alto e applica una regola per trasformare la frase in una risposta inserendo quella parola. Eliza usa anche un meccanismo di memoria dove se nella prossima frase non matcho nulla, allora uso una delle frasi (nel ranking) che prima era una potenziale candidata ma non l’avevo usata. Usa anche diversi trick, come le frasi di backup.
Abbiamo poi Parry che è una sorta di evoluzione di Eliza ma evoluto, utilizza un mental model per simulare risposte emotive. C'è da segnalare che è il primo chatbot ha passare il test di turing.
Si parla poi dei  chatbot Corpus based, che hanno principalmente due strategie. La prima ovviamente recupera dai corpus le risposte, mentre la seconda genera il testo usando dei language model oppure delle reti neurali. Infine vediamo una lista di pro e contro dei chatbot:

- Pro:

  - Il prof scrive letteralmente "fun" nelle slide
  - Ottimo per applicativi semplici e scriptati

- Contro

  - Non capiscono effettivamente cosa l'utente dice
  - Dare l''apparenza di capire il discorso è molto difficile
  - I chatbot rule based sono lunghi da implementare

Ci viene quindi spiegato che lo step successivo è quello di integrare un le abilità dei chatbot dentro i sistemi di dialogo.
I dialog sistem sono leggermente più evoluti, poiché usano una vera e propria architettura, vediamo quali sono gli elementi di questa architettura:

- _Speech Recognition_: riconosce la voce dell'utente e la trasforma in testo (di base si usano delle librerie)
- _Language understanding_: anche detta NLU, il task più complesso dell'architettura. Tendenzialmente la sua risoluzione si può riassumere nell'approccio classico e quello moderno.
  - Quello classico lavora sul significato delle parole e segue quindi tutta la pipeline vista fino a ora: Meaning = Syntactic $\rightarrow$ Semantic $\rightarrow$ Pragmatic. Quindi nell'approccio classico alla frase "paolo ama francesca" otterremo $love(Paolo,Francesca)$
  - L'approccio moderno è leggermente diverse e si basa sull sistema a **Frame**. Questo sistema usa un oggetto Frame che è composto da diversi slot, ogni slot avrà un type/tipo che indica il tipo di oggetto che dovrà risiedere al suo interno. I Frame tendono quindi a definire un contesto a cui si associano una o più domande, a volte questo viene chiamato domain ontology. Un esempio visto a lezione è il sistema GUS (che prenota viaggi aerei), che usa una serie di regole condizione azione. Quando a GUS scatta una regola associata alla destinazione del viaggio lui salva la meta nel frame, lo stesso vale per il giorno della partenza. GUS ha diversi frame per diversi contesti, come il frame per "prenotare una macchina o un hotel" oppure de frame dedicati alle tratte aeree. Il Procedimento che GUS segue è il seguente:
      1. Capire il dominio "Prenotare un volo ?" "Programmare una sveglia ?"
      2. Capire l'intento. "Trova film" "Mostra Voli"
      3. Riempire lo slot.
  
    Quali sono i pro e i contro di queste architetture a frame ?. I pro e che sono modelli molto precisi soprattutto e la domanda è precisa. I Contro sono che creare le regole è un processo lungo e faticoso, inoltre soffrono di problemi di recall.
    Segnalo inoltre che esiste un approccio ancora più moderno che usa i sistemi cloud e modelli di machine learning.
- _Dialog manager_: il dialog manager e colui che appunto gestisce/manage la conversazione. è composto dal _Dialog Context Model_ e dal _Dialog Control_.
  - Il Dialog Context Model si occupa di tenere traccia delle informazioni rilevanti del dialogo, come quello che è stato detto e ovviamente permette di condividere queste informazioni con il sistema e l'utente. Inoltre questa componente usa delle strutture dati che contengono lo stato del dialogo. Queste strutture possono essere:
    - Un numero di stati finiti
    - Slot tipati
    - Formule in FOL
  - Il Dialog Control invece si occupa di gestire la conversazione, ovvero decidendo cosa dire, cercare delucidazioni su quello che l'utente dice. Questa componente utilizza degli _Algoritmi per governare lo stato del dialogo_, vediamo alcuni esempi di questi algoritmi :
    - Graph based Dialog Controll: utilizzabile solo per interazioni semplici, ha un problema relativo numero di nodi e transizioni che crescono molto velocemente, sono però algoritmi centrali per la lingua parlata
    - Frame based Dialog Control: utilizzabile per interazioni più complesse, è basato sui frame visti sopra.
    - Plan or Logic Dialog Control
    - Statistical Model Dialog Control
- _Response Generation_: questa parte corrisponde all'architettura di generazione del testo vista sopra.
- _Text to Speech_: Sempre tramite delle librerie è possibile generare voce a partire dal testo scritto.

#### Confronti la semantica che ha prodotto nell'esercitazione con la semantica di Siri (Frame and Slot)
