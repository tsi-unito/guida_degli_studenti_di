---
title: 'Intelligenza artificiale e laboratorio - Parte III - Spiegazione e valutazione'
---

# Intelligenza artificiale e laboratorio - Parte III

## Spiegazione e valutazione

### Tipi di spiegazione

- La disciplina che più si è occupata di **potere esplicativo** è la filosofia della scienza.
    - Classificazione dei **tipi di spiegazione** (derivata dalla filosofia della scienza):
        - Teleologica;
        - Meccanicista;
        - Evoluzionistica;
        - Nomologico-deduttiva;
        - Funzionalista;
        - Causale.
- Elementi della spiegazione:
    - **Explanandum**: ciò che si vuole andare a spiegare;
    - **Explanans**: ciò che spiega l'explandandum.
- **Buone spiegazioni**:
    - **SNS**: explanans presenta condizioni necessarie e sufficienti rispetto all'explanandum ($\iff$, ottimo)
    - **SN**: explanans presenta condizioni necessarie rispetto all'explanandum.
    - **SS**: explanans presenta condizioni sufficienti rispetto all'explanandum.
    - Un una buona spiegazione l'explanans esprime le cause per l'explanandum e/o dovrebbe permettere di prevederlo.
- Nelle scienze cognitive si cerca di avere una **visione pluralistica**:
    - Più spiegazioni per fenomeno $F$ possono coesistere.
    - Questi tipi di spiegazione rispondono tutti a diversi tipi (dimensioni) sul perché dell'emergenza di un fenomeno.

#### Spiegazione teleologica

- **Spiegazione teleologica**: spiegare perché un fenomeno $F$ ha luogo vuol dire evidenziare il fine che l'esibizione di $F$ contribuisce a raggiungere.
    - Si spiega un fenomeno attribuendo una finalità.
    - Spesso sono gli umani ad attribuire scopi a sistemi *animati senza intenzione* (reattivi) o a sistemi inanimati.
    - **Intentional stance** [Dennett]: attribuire intenzionalità al comportamento/output prodotto da un sistema.
    - *Agents as Intentional Systems*: entità il cui comportamento può essere predetto attribuendo credenze, desideri e acume razionale.
    - Può essere un buon punto di partenza, ma non permette di definire *perché* un fenomeno si è manifestato.
        - Se si ragiona *solo* a questo livello, non si ha una buona spiegazione.

#### Spiegazione meccanicista e causale

- **Spiegazione meccanicista**: si cerca di spiegare i meccanismi che causano un fenomeno $F$.
    - Non c'è riferimento a nessuno scopo.
    - I meccanismi possono essere divisi in parte in un'ottica di spiegazione complessa.
    - Le spiegazioni meccanicistiche sono quelle più preziose per la creazione di sistemi biologicamente ispirati.
    - Le **spiegazioni causali** sono le spiegazioni meccanicistiche per eccellenza.
        - Le spiegazioni causali sono quelle **ottime**, ma i meccanismi causali sono spesso difficili da trovare.
        - Si tratta quindi spesso di meccanismi causali di carattere probabilistico.
    - `!` Nelle scienze cognitive ci si focalizza sulle **spiegazioni meccanicistiche e/o causali**.
        - Ma Difficilmente si sa come stabilire spiegazioni causali.

#### Spiegazione evoluzionistica

- **Spiegazione evoluzionistica**: si cerca di spiegare perché membri di una certa classe $X$ posseggono le capacità per produrre un certo fenomeno $F$.
    - Fa riferimento alla storia evolutiva.
    - Si tratta di un'altra dimensione (differente dalle precedenti) delle spiegazioni.
        - Nelle prime due si cerca di spiegare il perché tramite i **fini** e i **meccanismi**.

#### Spiegazione funzionalista

- **Spiegazione funzionalista**: spiegazione dell'output di un sistema sulla base delle componenti che lo caratterizzano.
    - Si tratta di spiegazioni con un certo livello di **circolarità**.
    - Non si spiegano i meccanismi o le cause, utile quando non si sa molto altro.

>Computational models that embody functional explanations explain the capacities of a system in terms of its sub-capacities. But this explanation is given **by the assumptions embodied in the model**, not by the computations performed by the model on the grounds of the assumptions [Piccini, 2007].

#### Spiegazione nomologico-deduttiva

- **Spiegazione nomologico-deduttiva**:
    - Criteri per una buona spiegazione:
        - R1: explanandum derivabile logicamente dall'explanans;
        - R2: explanans deve contenere almeno una legge di carattere generale;
        - R3: explanans controllabile empiricamente;
        - R4: explanans vero.
    - Esiste una variazione dove in R2 la legge è di tipo probabilistico (modello statistico induttivo).
    - Si tratta di condizioni molto restrittive.
        - Esistono buone spiegazioni che non sono ND.
        - Si hanno spesso **inference to best explanation** (IBE), simili alle abduzioni.

### Casi studio di spiegabilità

#### Cognitive Psychology for Deep Neural Networks

- *Cognitive Psychology for Deep Neural Networks: A Shape Bias Case Study* [Ritter et al, 2017]:
    - Si cerca di applicare la psicologica cognitiva dello sviluppo alle deep neural networks (DNN).
    - **Gavagai** (a là Quine): *indeterminacy of reference*.
        - *Whole-bias*: si assegna *gavagai* agli oggetti che hanno sia quella forma che quel colore.
        - *Taxonomic bias*: si assegna *gavagai* solo a oggetti di quel colore;
        - *Shape bias*: si assegna *gavagai* solo a oggetti di quella forma;
        - Negli esseri umani prevale lo shape bias.
            - La forma è l'**attributo discriminante** nell'**assegnazione di etichette categoriali**.
    - Viene misurato lo shape bias ai sistemi di classificazione in studio.
        - Viene presentato un oggetto *probe*.
        - Vengono presentati al sistemi due immagini di oggetti etichettati simili (colore e dimensione) al probe.
        - Il classificatore etichetta il probe con l'etichetta dell'oggetto con stessa forma (ma non stesso colore).
            - Le reti neurali in analisi hanno quindi un **shape bias** (primo claim).
        - Questi risultati dimostrano le capacità degli strumenti della psicologia cognitiva per esporre proprietà computazionali nascoste dei DNN.
            - Mentre allo stesso tempo fornisce un modello computazione dell'apprendimento umano del mondo (secondo claim).
    - Il secondo claim è evidentemente **falso**.
        - `!` Si attribuisce all'output di un sistema funzionalista delle spiegazioni (di tipo meccanicistico) afferenti all'ambito strutturalista.
        - Si confonde performance *human-level* con meccanismi *human-like*.
    - In mancanza di strumenti metodologici (di base) si possono commettere gravi errori.
- Problemi fondamentali inerenti allo studio:
    - Le **architetture neurali** scelte (artificiali e quella del bambino) sono **completamente differenti**.
    - Lo **shape bias è controverso** ance nella letteratura psicolinguistica.
    - Le **rappresentazioni interne** alle due reti sono **completamente differenti**.
- L'utilizzo di linguaggi propri ad altri ambiti (come per la psicologia cognitiva) deve essere utilizzato cautamente.
    - Bisogna distinguere il *marketing della scienza* dai fatti scientifici.

#### Using cognitive psychology to understand GPT-3

- *Using cognitive psychology to understand GPT-3* [Binz & Schulz, 2022]:
    - *Performance-match* su task noti.
    - Analisi human-level ma non human-like.
        - Mancano le componenti strutturaliste.
    - Si ha un confronto solo sui casi di successo (e abbozzatti quelli di fallimento).
    - Non si fa riferimento a nessun altra misura psicometrica (`eg` tempo).
    - La componente *to understand* è problematica.
    - Si tratta di uno studio *linguaggio-centrico*.
        - Ma il linguaggio è solo una dimensione di analisi.
    - I test di natura funzionalista hanno senso, ma vanno strutturati bene.
        - Senza sapere quale sia il training set e il test set, l'analisi non è sensata.

### Valutazione

- Sillogismo alla base dei test di
    - Assunti alla base:
        1. Un'entità è intelligente se è capace di mostrare certi repertori comportamentali $X$.
        2. Un computer può essere programmato per mostrare questi repertori $X$.
        3. Quindi un computer può essere considerato intelligente.
    - Obiezioni tipiche:
        - Obiezione behavioristico: la premessa $1$ è opinabile.
            - Solo perché qualcosa mostra certi repertori $X$ non significa sia intelligenti.
            - Magari si comporta semplicemente come tale.
        - La premessa $1$ è opinabile.
        - Dubita che si possano programmare macchine per fare ciò.

#### Turing Test

- **Turing Test** (TT):
    - Basato sul *gioco dell'imitazione*.
    - Storicamente rilevante ma *fallato*.
        - Obiezione behavioristica applicabile.
        - Test **non generale** dell'intelligenza, relativo solo alla **competenza linguistica** (linguaggio-centrico).
        - Ignora completamente la componente di *embodiment*.
    - Passare il test di Turing non implica che l'entità sia intelligente.
        - Alcuni sfidano questa opinione.
        - La semplice dimostrazione di questo comportamento può essere simbolo di intelligenza.
            - Si avrà un'intelligenza differente da quella umana.
    - Interpretato storicamente in maniere differenti:
        - Una definizione generale di intelligenza (no);
        - Un criterio operativo per attribuire o no intelligenza a un sistema;
        - Un criterio di adeguatezza per programmi di AI;
        - Un criterio di adeguatezza per modelli computazionali simulativi di processi cognitivi (no).
    - Criterio insufficiente per stabilire intelligenza human-like.

#### Importanza del linguaggio

- Il **linguaggio** è una competenza più importante di altre.
    - In quanto influisce in maniera decisiva nello svolgimento di **capacità non linguistiche**.
    - *Modularity and Development: the case of spatial reorientation* [Hermer & Spelke, 1995]:
        - Navigazione spaziale e linguaggio.
        - Si inserisce un topo in una scatola.
        - In un angolo della scatola è presente del cibo.
        - Il topo viene fatto girare al centro della scatola.
            - In questa fase il cibo è visibile al topo.
        - Dopo lo spin, il topo va in modo equiprobabile lungo la diagonale dove si trova il cibo.
            - Metà delle volte il topo va verso il cibo, la metà nella direzione opposta e poi tornano indietro.
        - Si sostituiscono i topi con bambini in età prelinguisti.
            - Il cibo viene sostituito con un giocattolo.
            - Come i topi, vanno in maniera equiprobabile lungo questa diagonale.
        - Si sostituiscono bambini con adulti.
            - Si ottiene lo stesso risultato.
        - La parete dell'angolo dove si trova il premio viene dipinta di blu.
            - Le persone adulte vanno il 100% delle volte verso il premio.
            - Per topi e bambini prelinguistici si ottengono gli stessi risultati di prima.
        - La capacità di verbalizzare (simbolicamente) permette un miglior orientamento.
            - La competenza linguistica gioca un ruolo preponderante anche in un task spaziale.
    - Risulta quindi giustificata l'importanza data al linguaggio nelle facoltà cognitive.
        - Questo negli umani, ma non nelle macchine.

#### Variazioni del e sul Turing Test

- **Total Turing Test** (TTT):
    - Si valutano degli agenti *embodied*.
    - Si prende in considerazione qualsiasi tipo di input e di output.
    - Anche oggigiorno, alcune componenti di *embodiment* non riescono a essere emulate.
- **Super-simplified Turing Test**:
    - Non c'è barriera tra macchina e interrogatore.
    - L'interrogatore dirà poi quanto è stata convincente l'interazione.
    - Problematiche:
        - Si hanno due distinzioni di persone: chi richiede troppo, chi troppo poco.
        - Ancora vincolato a bias noti.
- **Winograd Schema** challenge:
    - Piccole domande binarie che qualsiasi parlante inglese dovrebbe saper rispondere.
    - Necessario **disambiguare pronomi in base al commonsense** (**disambiguazione anaforica**).
        - `eg` «I poured water from the bottle into the cup until it was full. What was full?»
    - Schema in quanto le domande seguono tutte le stessa struttura.
    - Le domanda richiedono il pensiero e non possono essere risposte anche con accesso a un grande corpo.
        - I soggetti sono autocontenuti.
    - Differenze rispetto al TT:
        - Non è presente la soggettività del TT.
        - Non è facilmente ingannabile (ELIZA).
        - **Facile da valutare**.
    - Limiti:
        - Ma è nuovamente un test di tipo *comportamentale*, non generale.
            - Si valutano solo gli input e gli output.
        - Essendo binario, con risposta casuale si ha il $50\%$ di possibilità di indovinare.
    - Permette però di valutare modelli NLP.
        - Esistono già modelli che superano questo sistema.
        - Sono state introdotte quindi versioni più articolate.
- **12 criteri di Newell**:
    - Da considerarsi più come dei desiderata.
