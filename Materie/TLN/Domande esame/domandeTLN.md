# Domande TLN

**Parte comune sia a Mazzei che Radicioni**: una cosa positiva è che non ti assalgono se sbagli. Se gli dici una risposta
sbagliata, loro ti dicono chiaramente "No, ha sbagliato, sa perché? No? Ok, allora glielo rispiego io.". Tutto ciò però
senza darti la sensazione che ti stiano "asfaltando", nuovamente è una chiaccherata. Inoltre, non abbiate paura di
dirgli che non vi ricordate le cose, a Radiconi uno studente gli ha detto "scusi prof, non mi viene proprio in mente
questa risposta" e lui gli ha fatto un'altra domanda senza nessun problema.
Ovvio che se sbagli tutte le domande non passi l'esame. Inoltre emntrambi danno molto peso alla trattazzione dei
progetti (Relazine + codice in Mazzei, e discussione + codice in Radicioni).

## Mazzei
L'orale di Mazzei dura dai 20 ai 30 minuti. Lui si legge la tua relazione e il tuo codice per conto suo, e nella prima
metà dell'orale ti espone i suoi dubbi sul tuo progetto, del tipo "Non ho capito questa cosa, me la rispiega?". Come
dicono tutti, comunque è una chiaccherata molto amichevole, non ti mette a disagio. Nella seconda parte di orale invece
ti fa 2-3 domande di teoria, alle quali vuole risposte specifiche, e scende anche abbastanza nel dettaglio.

### Domande Mazzei
> Premessa: alcune domande potrebbero non essere più attuali perchè il programma e le esercitazioni cambia di anno in anno

Pesca da un file di domande, quindi escono sempre le stesse ed eventualmente fa dei collegamenti. 

- Mi parli degli algoritmi di parsing delle grammatiche a dipendenze.
  - CKY Probabilistico, Algoritmo basato su grafo MST, MALT, caratteristiche, problemi, numero di stati generati...
- Confronti la semantica che ha prodotto nell'esercitazione con la semantica di Siri (Frame and Slot)
- A cosa serve il lambda calcolo?
- CKY: spiegazione, a cosa serve, simulazione su carta.
- Come rappresentare articoli e sostantivi nel lambda calcolo?
- Mi partli dell'ambiguità sintattica (PP attachment e Coordination, con esempi)
  - Qual'è il vantaggio dell'ambiguità? Mi permette di fare **frasi molto compatte**.
- Nell'NLG, quali sono le differenze tra i task di _Referencing Expression_ e _Lessicalizzazione_?
- Problema dell'espressività delle lingue naturali, mi parli di Chomsky, della sua gerarchia e delle grammatiche
Mildly Context Sensitive
- Differenza fra HMM e MEMM: spiegazione dei pro e dei contro, differenze a livello di probabilità utilizzate. 
- Cos'è una grammatica CCG? Mi parli un po' di questo paradigma.
- Nell'architettura finale dell'NGL, perchè abbiamo visto he è divisa in 3 fasi? Me ne parli.
- Anatomia di un parser
- Semantica formale e composizionale con esercizio (montaque) e tipi ambiguità con esempi
- semantica di montague (fol + lambda calcolo) e la sintassi a dipendenze

## Radicioni

L'orale di Radicioni è completamente diverso da quello di Mazzei. Dura dai 45 ai 60 minuti, all'interno dei quali ti
chiede prima di discutere un'esercitazione (o una a piacere oppure una che sceglie lui), e poi di rispondere a delle
domande teoriche, articolate come un discorso. Mi spiego meglio. Io sono partito dall'esercitazione di Framenet, e una
volta discussa, mi ha chiesto di dirgli come funziona framenet, come sono rappresentati i concetti lì dentro, e poi di
confrontarlo con Conceptnet e poi di confrontare Conceptnet con Wordnet. Quindi mi ha fatto fare un discorso sulla
rappresentazione delle conoscenza a partire da Framenet, per poi spostarsi verso Conceptnet e Wordnet.
Come per Mazzei, anche Radicioni scende abbastanza nel dettaglio nelle domande.

Spesso fa domande di ragionamento tipo: "Useresti questo tool per fare questa cosa? Quali sarebbero i vantaggi/svantaggi rispetto ad usare un altro tool?", "Di fatto, in questa coppia di termini, cosa significano l'uno per l'altro? cosa significa questo valore di similarità?"

Un consiglio generale: non guardate le sue espressioni facciali. Anche se state dicendo tutto giusto vi lancerà delle
occhiatacce molto spaesate che vi faranno pensare di stare sbagliando tutto. Non abbiate timore e non guardatelo.

### Domande Radicioni

- Inizia sempre da un'esercitazione. O a scelta sua o a scelta dello studente
- Spiegazione dell'esercitazione e tool usato (Wordnet, Framenet ecc.)
- Nel testo dell'esercitazione vi ho chiesto di produrre un output particolare, discutiamolo un momento, cosa c'è
dentro a questo file?
- Wordnet: cos'è un synset? (e poi annessa discussione su Wordnet)
- Wordnet è uno strumento per una annotazione funzionale? Nel senso, per i task svolti nelle esercitazioni, non
sarebbe meglio qualcos'altro?
- Come si usa Framenet in un contesto applicativo?
- Se io le dico "spazi concettuali" gli viene in mente qualcosa? Cosa sono? Dove li abbiamo visti? (lo studente non se
lo ricordava ed è passato oltre) -- DOLCE
- Mi definisca in modo sintetico cos'è Verbnet.
- Partenza dall'esercitazione sulla summarization: Come valutare un riassunto?
- Cos'è Nasari?
- Nasari è una perfetta copia vettoriale di Babelnet? _No, Babelnet è più ricco, (iponimi, iperonimi...) è quasi un'ontologia, mentre Nasari è una rappresentazione degli oggetti_.
- Cos'ha BabelNet in più di Nasari? _I verbi_.
- Cos'è un endurante? Un perdurante?
- Com'è strutturato FrameNet? A cosa serve?
- Com'è strutturato VerbNet? A cosa serve?
- Quali sono le differenze fra FrameNet, VerbNet e ConceptNet?
- Che differenza c'è in Babelnet tra Named Entities e Concetti? (trabocchetto, in un nodo Babelnet possono essere
presenti entrambi)
- Mi parli della teoria dei prototipi.
- Cosa le viene in mente se le dico "ruolo semantico" e "funzione sinattica"? _Verbnet_. Perfetto, mi parli di Verbnet allora.
- Verbnet: cos'è l'alternation?
- eventuali problemi dell'alta granularità di FrameNet. Differenze con quella di WordNet
- Come fare disambiguazione con FrameNet
- Cos'è l'approccio moltiplicativo -- DOLCE
- Cos'è la semantica procedurale
- Cosa sono gli spazi concettuali
- Differenza tra teoria dei prototipi e teoria degli esemplari

## Di Caro
L'orale dura circa 45 minuti. I primi 35 ti lascia parlare liberamente su tutte le esercitazioni una per una (non sempre, a me ne ha chiesta solo 1). Ti interrompe raramente, giusto per chiedere:
- Interpretami questi risultati.
- Perché hai fatto questa particolare scelta?
- Hai testato qualche altra tecnica?
Tutto molto discorsivo.
Per ottenere un ottimo voto, pretende un'ottima esposizione brillante.

### Domande Di Caro
- Il triangolo semiotico
- Word sense disambiguation vs Word Sense Induction
- Differenza fra Text Mining e Semantica Distribuzionale (spoiler: il primo non usa semantica, solo statistica)
- Quali tipi di granularità esistono ed elencare alcuni task ad essi associati
- esercizio 1.5 (spiegazione concetti + spiegazione codice + collegamento con genus differentia)
- esercizio WSI
- cos'è il topic modelling
- configurazioni matriciali
- semantica documentale e text tiling
- LSA, cos'è il concetto latente
- knowledge graph
- Teoria di Hanks