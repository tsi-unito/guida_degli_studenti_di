---
title: 'Intelligenza artificiale e laboratorio - Parte II - Incertezza'
---

# Intelligenza artificiale e laboratorio - Parte II

## Incertezza

### Approcci logici e incertezza

- Gli approcci logici visti precedentemente si basano su **conoscenza completa e certa**.
    - In presenza di queste due caratteristiche la logica permette di derivare **soluzioni corrette**.
    - Nei casi reali però non è quasi mai questa la situazione.
    - `eg` Un modo per affrontare l'incertezza in CLIPS sono i *certainty factors*.
        - Non sono però misure qualitative non radicate su una teoria delle probabilità.
    - **Problematiche** relative all'**incertezza**:
        - Osservabilità parziale;
        - Osservazioni imprecise;
        - Incertezza sul risultato delle azioni;
        - Difficoltà nel modellare un sistema nel suo complesso.
    - Un approccio puramente logico: 
        - Rischia di prendere **decisioni errate**;
        - O porta a conclusioni **troppo deboli** per il *decision making*.
    - La conseguenza logica esprime sempre delle **conclusioni forti**.
        - Le relazioni però non sono sempre conseguenze logiche.
        - `!` La presenza di una **causa** non è sempre traducibile come una conseguenza logica.
- Problemi della logica e del suo utilizzo nei casi reali.
    - *Pigrizia*: elencare insieme completo di premesse/conseguenze richiede troppo lavoro;
        - Le regole diventano difficili da comprendere e utilizzare.
    - *Ignoranza teorica*: non si conosce tutto;
    - *Ignoranza pratica*: anche se si conosce tutto in generale, si potrebbe avere lacune sul caso specifico.
        - Mancano sufficienti osservazioni.

### Probabilità

- La soluzione è quella di utilizzare la **probabilità**.
    - Essa fornisce infatti uno strumento per **riassumere l'incertezza** che deriva da pigrizia e ignoranza.
    - Assegnare una probabilità a una formula corrisponde al **grado di credenza** nella verità della formula.
        - `eg` Probabilità dell $80\%$ non vuol dire vera al $80\%$ (si tratta sempre di proposizioni o vero o false).
        - Non si tratta di un grado associato alla verità (come in *fuzzy logic*), ma alla **credenza**.
        - Rispetto ai CF, le probabilità rispettano determinati assiomi.
    - Le credenze dipendono dalle **prove**.
        - Tutti gli enunciati indicheranno sempre le prove alla base degli assegnamenti di probabilità.
    - Due casi di probabilità:
        - **Probabilità a priori**: in assenza di prove;
        - **Probabilità a posteriori (condizionata)**: dopo che si hanno le osservazioni.
- Notazione per l'utilizzo della probabilità:
    - Una *variabile casuale* si riferisce a una parte del mondo il cui stato è inizialmente sconosciuto.
        - Può avere dominio continuo o discreto.
        - $X$ è una variabile casuale discreta.
        - Con lettere
    - Le probabilità saranno sempre associate a proposizioni.
        - `eg` Nella proposizione $X = val$, $X$ è la variabile casuale discreta e $val$ è un valore del suo dominio.
        - `eg` In $Weather = \langle sunny, rain, cloudy, snow \rangle$ il dominio è visto come una tupla ordinata.

#### Decisioni razionali

- Per prendere **decisioni razionali** un agente deve avere **preferenze sugli esiti** delle azioni.
    - La **teoria dell'utilità** attribuisce a ogni esito un **grado di utilità** per l'agente.
        - E permette all'agente di ragionare su esiti alternativi.
    - L'**agente razionale** compie azioni che **massimizzano l'utilità attesa**.
        - `!` **Teoria delle decisioni** $=$ teoria delle probabilità + teoria dell'utilità.
        - Una decisione razionale è un **compromesso** tra una probabilità di successo di un'azione e la sua utilità.

#### Assiomi della probabilità

- `prop` **Assiomi delle probabilità** (o di **Kolmogorov**): 
    - Per qualsiasi proposizione $a$ e $b$:
        1. $0 \leq P(A) \leq 1$;
        2. $P(true) = 1$ (evento certo) e $P(false) = 0$ (evento impossibile);
        3. $P(a \lor b) = P(a) + P(b) - p(a \land b)$.
    - L'utilizzo degli assiomi nella rappresentazione di *gradi di belief* come probabilità garantisce razionalità e **consistenza** delle decisioni.
    - I CF sono un esempio di gradi di belief che non soddisfano gli assiomi.
- `ex` **Evento atomico**: specifica **completa** dello stato del mondo.
    - È un **assegnamento** di valori a **tutte le variabili casuali**.
    - Eventi atomici differenti sono **mutuamente esclusivi**.
    - L'insieme di tutti gli eventi atomici descrive la verità o falsità di qualsiasi proposizione (semplice o complessa).
    - Ogni proposizione è logicamente equivalente alla disgiunzione di tutti gli eventi atomici che ne implicano la verità.
    - `!` In generale, $P(a) = \sum_{e_i \in \boldsymbol{e}(a)} P(e_i)$.
        - Dove $\boldsymbol{e}(a)$ è l'insieme degli eventi atomici dove $a$ è vera.
        - Non si conosce $P(a)$, ma la si può calcolare sulle probabilità degli eventi atomici (facili da calcolare).
        - `eg` Con due dadi, $P(D_1 = 1) = P(D_1 = 1, D_2 = 1) + P(D_1 = 1, D_2 = 2) + \dots = = \frac{6}{36} = \frac{1}{6}$.
        - Tutte le formule che si vedranno successivamente sono **elaborazioni** sull'idea della somma delle probabilità degli eventi atomici.

#### Probabilità a priori

- **Probabilità a priori**: il grado di credenza in una proposizione in **assenza di ogni altra informazione**.
    - **Distribuzione di probabilità (a priori)**: un vettore di valori di probabilità.
        - Uno per ogni valore del dominio della variabile casuale considerata.
            - `eg` $P(Weather) = \langle 0.7, 0.02, 0.08, 0.2 \rangle$.
        - I valori sono normalizzati in modo tale che la loro somma sia $1$.
    - **Distribuzione di probabilità congiunta**: una matrice che assegna un valore di probabilità ad ogni possibile combinazione di valori delle variabili considerate.
        - **Distribuzione di probabilità congiunta completa**: se costruita su tutte le variabili del dominio.
            - Situazione ideale.
            - Ha una crescita esponenziale sul numero di variabili, difficilmente costruibile.
- Quando si acquisisce nuova conoscenza, non si possono più usare le probabilità a priori.

#### Probabilità condizionate

- `def` **Probabilità condizionata**: $P(a \mid b) = \frac{P(a \land b)}{P(b)}$ (con $P(b) \neq 0$).
    - Probabilità di $a$ quando **tutto ciò che è noto** è $b$.
        - Intuizione insiemistica: quanto è probabile l'intersezione tra $A$ e $B$ valutando solo l'insieme $B$.
    - `def` **Regola del prodotto**: $P(A \land b) = P(a \mid b) P(b) = P(b \mid a) P(a)$.
    - `def` **Distribuzione di probabilità condizionate**: $\boldsymbol{P}(X, Y) = \boldsymbol{P}(X \mid Y) \boldsymbol{P}(Y)$.
        - Definita per ogni possibile coppia di valori $X = x_i$ e $Y = y_j$.
        - Non è un prodotto tra matrici, ma tra gli elementi.
            - Si tratta di sistemi di equazioni.
    - Le probabilità condizionate non sono *implicazioni logiche con incertezza*.
        - `eg` $P(a \mid b) = .8$ non deve essere interpretata come $P(b \to a) = .8$.
        - Le probabilità condizionate **non sono monotone** come le implicazioni logiche.
            - Se si ha altra informazione la probabilità condizionata di $a$ può cambiare significativamente.
            - `eg` $P(a \mid b \land c) \neq P(a \mid b)$.
                - Invece nella FOL (monotona), $b \to a \implies b \land c \to a$.

#### Inferenze con distribuzioni congiunte complete

- **Inferenza probabilistica**: calcolo delle probabilità a posteriori di proposizioni poste come interrogazioni (query) partendo dalle prove osservate.
    - Si è visto che $P(a) = \sum_{e_i \in \boldsymbol{e}(a)} P(e_i)$, dove $\boldsymbol{e}(a)$ è l'insieme degli eventi atomici dove $a$ è vera.
    - **Probabilità marginale**: la somma di tutte le $P$ di eventi atomici dove un assegnamento di valore è stabilito.
        - Somma per riga o per colonna (scritta *ai margini* in una rappresentazione tabellare.
        - Probabilità marginale $\equiv$ probabilità a priori.
            - Date le $P$ (esaustive) degli eventi atomici, si può calcolare la $P$ a priori di ogni singolo evento.
- `def` **Distribuzione di probabilità a priori delle variabili in $\boldsymbol{Y}$**: $\boldsymbol{P}(\boldsymbol{Y}) = \sum_{\boldsymbol{Z}} P(\boldsymbol{Y}, \boldsymbol{z})$.
    - Dove $\boldsymbol{Y}$ e $\boldsymbol{Z}$ sono insiemi di variabili casuali.
    - $\boldsymbol{P}(\boldsymbol{Y})$ è la distribuzione di probabilità dell'insieme di variabili casuali $\boldsymbol{Y}$.
    - $\boldsymbol{z}$ è un vettore di valori alle variabili in $\boldsymbol{Y}$.
    - Si fissano le variabili di $\boldsymbol{Y}$ e si fanno *ruotare* tutti le altre variabili.
        - Valutando tutte le possibili combinazioni.
    - `def` **Regola di condizionamento**: $\boldsymbol{P}(\boldsymbol{Y}) = \sum_{\boldsymbol{Z}} \boldsymbol{P}(\boldsymbol{Y} \mid \boldsymbol{z}) \boldsymbol(P)(\boldsymbol{z})$.
- **Normalizzazione**:
    - Nei calcoli delle distribuzioni si divide per una dividendo comune tutte le probabilità relative.
        - La **costante di normalizzazione** garantisce che la somma delle varie probabilità sia $1$.
        - `eg` In $\boldsymbol{P}(Carie \mid toothache) = \langle P(carie \mid toothache), P(\lnot carie \mid toothache) \rangle$:
            - Entrambe le probabilità si calcolano moltiplicando per $\frac{1}{P(toothache)}$.
            - $\boldsymbol{P}(Carie \mid toothache) = \alpha \boldsymbol{P}(Carie, toothache)$ dove $\alpha = \frac{1}{P(toothache)}$.
        - A volte il dividendo della costante non è noto.
            - Si può calcolare **a posteriori** dopo aver calcolato la distribuzione.
- `def` **Procedura di inferenza generale**: $\boldsymbol{P}(X \mid \boldsymbol{e}) = \alpha \boldsymbol{P}(X, \boldsymbol{e}) = \alpha \sum_{\boldsymbol{y}} \boldsymbol{P}(X, \boldsymbol{e}, \boldsymbol{y})$.
    - $\boldsymbol{E}$: variabili di evidenza;
        - $\boldsymbol{e}$ è un array di assegnamenti precisi a un sottoinsieme di variabili del mondo esclusa $X$.
    - $\boldsymbol{Y}$: variabili nascoste;
    - $\boldsymbol{X}$: variabili di query;
    - Nella sommatoria, $\boldsymbol{y}$ vengono fatte *ruotare*.
        - Per permettere di poter attribuire un valore di probabilità a ogni possibile valore di $X$.
        - È una generalizzazione della marginalizzazione.
    - Questa procedura ha due svantaggi:
        - È computazionalmente costosa, per $n$ variabili booleane si ha $O(2^n)$.
        - Costruire la **distribuzione congiunta completa** non è sempre possibile.
            - Soprattutto in casi reali con centinaia o migliaia di variabili.

### Bayes

- `def` **Indipendenza**: $a$ e $b$ sse $P(a \mid b) = P(a)$ o $P(b \mid a) = P(b)$ o $P(a \land b) = P(a)P(b)$.
    - `eg` $P(Toothache, Catch, Cavity, Weather) = P(Toothache, Catch, Cavity)P(Weather)$.
        - Si passa da $32$ a $12$ righe nella distribuzione congiunta completa.
    - L'**indipendenza assoluta** è però rara nei domini reali.
        - Servono modi più generale (*domain-independent*) per sfruttare l'indipendenza.

#### Regola di Bayes

- `def` **Regola di Bayes**: $P(b \mid a) = \frac{P(a \mid b) P(b)}{P(a)}$.
    - Derivata dalla regola del prodotto: $P(a \land b) = P(a \mid b)P(b) = P(b \mid a)P(a)$.
    - `def` **Regola di Bayes sulle distribuzioni**: $\boldsymbol{P}(Y \mid X) = \frac{\boldsymbol{P}(X \mid Y) \boldsymbol{P}(Y)}{\boldsymbol{P}(X)}$.
        - $\boldsymbol{P}$ è l'intero sistema di equazioni definite su ogni possibile coppia di valori per $X$ e $Y$.
    - `def` **Regola di Bayes e condizionamento all'evidenza**: $\boldsymbol{P}(Y \mid X, \boldsymbol{e}) = \frac{\boldsymbol{P}(X \mid Y, \boldsymbol{e}) \boldsymbol{P}(Y \mid \boldsymbol{e})}{\boldsymbol{P}(X \mid \boldsymbol{e})}$.
        - $\boldsymbol{e}$ è un vettore di valori alle variabili di prova.

#### Indipendenza condizionale

- Nei casi reali spesso si ha a disposizione informazione probabilistica nella forma $P(effetto \mid causa)$.
    - Ma spesso si vuole calcolare quanto probabile sia la causa osservando certi effetti, $P(causa \mid effetto)$.
    - Per poter applicare Bayes si ha bisogno di aggiungere delle asserzioni di **indipendenza**.
- `def` **Indipendenza condizionale**: di $A$ e $B$ sse $P(A \mid B, C) = P(A \mid C)$.
    - Si fattorizza la probabilità iniziale in fattori indipendenti.
        - Riducendo il problema in **sotto-problemi**.
    - `!` Per $n$ cause condizionalmente indipendenti dato $X$, la dimensione della rappresentazione **cresce** come $O(n)$ anziché $O(2^n)$.
- `def` **Modello di Bayes ingenuo**: $\boldsymbol{P}(Causa, Eff_1, \dots, Eff_n) = \boldsymbol{P}(Causa) \prod_i \boldsymbol{P}(Eff_i \mid Causa)$.
    - Noto anche come **classificatore** Bayesiano naive.
    - L'ingenuità sta nell'assumere che gli $n$ effetti siano tra loro indipendenti (improbabile nel mondo reale).
    - È un buon compromesso tra facilità di calcolabilità e accuratezza.
- `def` **Indipendenza condizionale generalizzata**: di $X$ e $Y$ data $Z$ vale sse $\boldsymbol{P}(X, Y \mid Z) = \boldsymbol{P}(X \mid Z) \boldsymbol{P}(Y \mid Z)$.
    - Se l'indipendenza condizionale vale, si ha:
        - $\boldsymbol{P}(X \mid Y, Z) = \boldsymbol{P}(X \mid Z)$;
        - $\boldsymbol{P}(Y \mid X, Z) = \boldsymbol{P}(Y \mid Z)$.

#### Reti bayesiane

- Un **belief networks** è una rappresentazione grafica di **asserzioni di indipendenze condizionali**.
    - Fornisce una **specifica concisa** di qualsiasi **distribuzione congiunta completa**.
        - La topologia della rete specifica le condizioni di indipendenza.
    - È un **grafo**:
        - Un nodo per ogni variabile casuale (discreta o continua).
        - È un **DAG**: archi diretti e privo di cicli.
        - Un arco da $X$ a $Y$ implica che $X$ è un genitore di $Y$.
            - $X$ ha un'**influenza diretta** su $Y$.
        - Ogni nodo $X_i$ è associata ad una *distribuzione condizionale* di $X_i$ dati i suoi genitori.
            - $\boldsymbol{P}(X_i \mid Parents(X_i))$ quantifica gli effetti dei genitori sul nodo.
            - Questa distribuzione è espressa come *tabella di probabilità condizionale* (CPT).
                - In generale la CPT di una variabile (booleana) con $k$ genitori booleani contiene $2^k$ righe.
                - Una per ogni possibile combinazione di valori dei genitori.
            - Un'osservazione si ripercuote sui valori delle CPT di tutta la rete.
        - Le **cause** sono nodi senza genitori, mentre le **evidenze** sono nodi senza figli.
    - L'utilizzo della probabilità catturano un'infinita di situazioni non note.
        - Le probabilità vanno intese come **credenze associate a proposizioni**.
- Compattezza:
    - Una CPT per una variabile booleana $X_i$ con $k$ genitori booleani ha $2^k$ righe per tutte le possibili combinazioni di valori dei genitori.
    - Se ogni variabile ha non più di $k$ genitori la rete completa richiede $O(n \cdot 2^k)$ valori.
    - Dato che in genere $k \ll n$, il numero di valori necessari cresce linearmente in $n$.
        - Contro $O(2^n)$ della distribuzione completa congiunta.

##### Semantica delle reti bayesiane

- **Semantica** stabilisce come la struttura (sintassi) della BN corrisponde ad una distribuzione delle variabili.
    - Semantica globale: $\boldsymbol{P}(X_1, \dots, X_n) = \prod_{i=1}^n \boldsymbol{P}(X_i \mid Parents(X_i))$.
        - Definisce la distribuzione completa congiunta come il prodotto delle distribuzioni condizionali locali.
    - Semantica locale:
        - Ogni nodo è condizionalmente indipendente dai suoi **non**-discendenti dati i suoi genitori.
        - Proprietà sfruttata dagli algoritmi di inferenza.
        - **Markov blanket**: genitori, discendenti e genitori dei discendenti.
            - Ogni nodo è condizionalmente indipendente da tutti gli altri nodi di una rete data la sua coperta di Markov.
    - Teorema: semantica globale $\equiv$ semantica locale.
        - La semantica locale applicata a tutti i nodi della rete ricostruisce la semantica globale.

##### Costruzione di una rete bayesiana

- Per costruire una rete si deve fare attenzione all'ordine con cui i nodi sono aggiunti.
    - `def` **Chain rule**: $\boldsymbol{P}(X_1, \dots X_n) = \prod_{i = 1}^n \boldsymbol{P}(X_i \mid X_{i-1}, \dots, X_1)$.
        - Riscrittura ricorsiva della regola del prodotto.
        - Una probabilità congiunta viene riscritta come un prodotto di probabilità condizionate, etc.
            - Meccanismo di condizionamento ripetuto più e più volte.
        - La chain rule è equivalente alla semantica globale delle BN purché $Parents(X_i) \subseteq \{X_1, \dots, X_{i-1}\}$.
            - Cioè vale che $\boldsymbol{P}(X_i \mid X_{i-1}, \dots, X_1) = \boldsymbol{P}(X_i \mid Parents(X_i))$.
    - Una rete rispetta la semantica globale delle BN solo se per ogni nodo $X_i$ esso è condizionalmente indipendente da ogni altro suo predecessore nell'ordine dei nodi, dati i suoi genitori. 
- Strategia di costruzione di una rete bayesiana basata sulla chain rule:
    - Si sceglie un ordinamento delle variabili $X_1, \dots, X_n$.
    - Per $i = 1$ a $n$:
        - Si aggiunge $X_i$ alla rete.
        - Si selezionano dei parenti da $X_1, \dots X_{i-1}$ per cui $\boldsymbol{P}(X_i \mid Parents(X_i)) = \boldsymbol{P}(X_i \mid X_1, \dots, X_{i-1})$.
            - Si valutano i nodi da cui il nodo attuale dipende veramente.
    - Bisogna però non solo avere una BN che rispetti la semantica globale, ma per cui le CPT siano **facilmente calcolabili**.

##### Inferenze con reti bayesiane

- Data una rete definita su un insieme di variabili $U$ occorre distinguere tra:
    - Variabili di **evidenza** $\boldsymbol{E}$;
    - Variabili di **query** $\boldsymbol{X}$;
    - Variabili **nascoste** $\boldsymbol{Y} = \boldsymbol{U} - \boldsymbol{E} - \boldsymbol{X}$.
    - Dalla distribuzione congiunta completa si può rispondere a qualsiasi query con la procedura di inferenza generale.
        - Essendo una BN una rappresentazione compatta di una DCC si può *navigare* lungo la rete.
- Inferenze con reti bayesiane:
    - Una soluzione naive (basata sulla semantica globale) per inferenze ha complessità $O(n2^n)$.
        - Con soluzioni migliori, si può arrivare a $O(2^n)$.
    - I CF, a differenza delle BN, sono molto più facili da utilizzare per i calcoli inferenziali.
        - Nonostante manchino di una formalizzazione precisa.
    - Due tipi di inferenze da perseguire sulle BN:
        - `def` **Most Probable Explanation**: $MPE(\boldsymbol{e}) = argmax_{\boldsymbol{x}} \boldsymbol{P(x, e)}$.
            - Qual'è l'istanza più probabile di tutte le variabili di dominio $\boldsymbol{X}$ data l'evidenza $\boldsymbol{e}$.
        - `def` **Maximum a Posteriori Probability**: $MAP(\boldsymbol{e}) = argmax_{\boldsymbol{x}} \boldsymbol{P(m, e)}$.
            - Qual è l'istanza $m$ più probabile per un sottoinsieme $M \subseteq X$ di variabili di query, data l'evidenza $e$.
