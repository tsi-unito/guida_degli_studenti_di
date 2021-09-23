# Torta

- [Torta](#torta)
  - [**Intro**](#intro)
  - [**Notazione**](#notazione)
  - [**Probabilità a Priori**](#probabilità-a-priori)
  - [**Probabilità per variabili continue**](#probabilità-per-variabili-continue)
  - [**Funzione di densità Gaussiana**](#funzione-di-densità-gaussiana)
  - [**Probabilità Condizionata / a posteriori**](#probabilità-condizionata--a-posteriori)
  - [**Inferenza per enumerazione e Normalizzazione**](#inferenza-per-enumerazione-e-normalizzazione)
  - [**Indipendenza (e indipendenza condizionale)**](#indipendenza-e-indipendenza-condizionale)
  - [Regola di Bayes](#regola-di-bayes)
  - [**Paradosso di Monty-Hall (e BN)**](#paradosso-di-monty-hall-e-bn)
  - [**Paradosso di Simpson (e BN)**](#paradosso-di-simpson-e-bn)
  - [**Reti Bayesiane**](#reti-bayesiane)
    - [**Componenti**](#componenti)
    - [**Significato delle frecce nelle reti bayesiane**](#significato-delle-frecce-nelle-reti-bayesiane)
    - [**Compattezza**](#compattezza)
    - [**Semantica** (Globale, Locale, Markov Blanket)](#semantica-globale-locale-markov-blanket)
      - [**Globale**](#globale)
      - [**Locale**](#locale)
      - [**Coperta di Markov**](#coperta-di-markov)
    - [**Come si costruisce**](#come-si-costruisce)
    - [**Reti Bayesiane Ibride**](#reti-bayesiane-ibride)
    - [**Rete Bayesiana Naive, calcolo delle probabilità della causa dati gli effetti**](#rete-bayesiana-naive-calcolo-delle-probabilità-della-causa-dati-gli-effetti)
    - [**Task di Inferenza** (Simple/Conjunctive Queries, MPE, MAP)](#task-di-inferenza-simpleconjunctive-queries-mpe-map)
      - [**Simple queries**](#simple-queries)
      - [**Conjunctive Queries**](#conjunctive-queries)
      - [**Most Probable Explanation**](#most-probable-explanation)
      - [**Maximum a Posteriori Probability**](#maximum-a-posteriori-probability)
    - [**Inferenza Esatta**](#inferenza-esatta)
    - [**Struttura** (Chain, Divergent, Convergent, Path Bloccati)](#struttura-chain-divergent-convergent-path-bloccati)
    - [**D-Separazione**](#d-separazione)
    - [**Effetto Causale**](#effetto-causale)
      - [**Operatore DO**](#operatore-do)
      - [**Causal effect rule**](#causal-effect-rule)
    - [**Inferenza Approssimata**](#inferenza-approssimata)
      - [**Rejection Sampling** (Algoritmo, Analisi e problemi)](#rejection-sampling-algoritmo-analisi-e-problemi)
      - [**Likelihood Weighting** (Algoritmo, Weighted-Sample, Analisi e problemi)](#likelihood-weighting-algoritmo-weighted-sample-analisi-e-problemi)
    - [**Modelli Temporali**](#modelli-temporali)
      - [**Catene di Markov** (first e second order, Sensor Markov Assumption)](#catene-di-markov-first-e-second-order-sensor-markov-assumption)
      - [**Task di Inferenza su Modelli Temporali** (Filtering, Prediction, Smoothing e algoritmo forward backward)](#task-di-inferenza-su-modelli-temporali-filtering-prediction-smoothing-e-algoritmo-forward-backward)

## **Intro**

La probabilità è utile per modellare quando non conosciamo tutti i fatti rilevanti per determinare l'esito di un evento. Possiamo essere più "pigri" perchè non dobbiamo enumerare ogni particolare. Ne esistono due tipi:

- Soggettiva: è la mia credenza nella conclusione basata sulla conoscenza personale.
- Oggettiva: si suppone che non si tratti du un'opinione o una credenza, ma che sia un fatto oggettivo, riscontrabile.  
Si possono ad esempio contare le frequenze, anche se non sempre è possibile fare grandi quantità di misurazioni.

Se la conoscenza bayesiana cambia, allora cambierà anche il risultato.  
Si può unire la Probability Theory con la Utility Theory per ottenere la **Decision Theory**, che permette di effettuare la scelta più razionale o ottimale (probabilità + preferibilità).

## **Notazione**

Consideriamo come esempio un dado a 6 facce.  
$\Omega$ è lo spazio campionario, e ogni suo elemento $\omega$ è un mondo possibile (_campione/evento atomico_)  
$0 \leq P(\omega)\leq 1$: abbiamo una P per owni $\omega$, quindi dovrà risultare $\sum_{\omega}P(\omega)=1$e quindi $P(1)=...=P(6)=\frac{1}{6}$.

$P(a=true) = P(a)$ e $P(a=false) = P(\neg a)$  
$P(a \wedge b) = P(a) \cap P(b)$  
$P(A \vee b) \equiv (\neg a \wedge b) \vee (a \wedge \neg b) \vee (a \wedge b)$  

Le proposizioni possono essere  

- Booleane
- Discrete (Weather = rain)
- Continue (Temperature < 22°)

## **Probabilità a Priori**

E' la credenza a priori data una qualche evidenza  
Con una tabella si può rappresentare tutti i possibili assegnamenti per certe combinazioni

## **Probabilità per variabili continue**

Data una funzione che definisce una certa densità cumulativa distribuita. $P(X=x)$ è la densità uniforme nel range definito.  
All'esterno il valore è 0. L'integrale della funzione varrà 1 se da $-\infty$ a $+\infty$, quindi il rettangolo della funzione ha area 1.

## **Funzione di densità Gaussiana**

La media ci dice in base a cosa è centrata. La STDDEV ci dice che + grande è la curva, tanto più è allargata.

## **Probabilità Condizionata / a posteriori**

$P(cavity\ |\ toothache) = 0.8$ significa che dobbiamo andare a prendere in $\Omega$ solo i mondi possibili in cui $toothache$ è $true$.  
Nel caso di una distribuzione condizionata, la notazione è $P(Cavity | Toothache)$ ed è una matrice 2x2.  
Se si aggiunge nuova conoscenza, potremmo modificare il risultato della probabilità, ma ci sono anche casi in cui certe informazioni non influiscono.  
$$
P(a\ |\ b) = \frac{P(a \wedge b)}{P(b)}\ \ \text{se } P(b) \neq 0
$$
Inoltre
$$P(a \wedge b) = P(a, b) = P(a\ |\ b) \cdot P(b) = P(B\ |\ a) \cdot P(a)$$

## **Inferenza per enumerazione e Normalizzazione**

Con una rappresentazione _tabellare_ della distribuzione congiunta con tutte le probabilità possiamo calcolare qualunque proposizione.  
Se chiedo $P(Cavity\ |\ Toothache)$, posso evitare di calcolare il denominatore convertendolo in un generico $\alpha$.  
Una volta calcolata la distribuzione, la si può normalizzare.  
Attenzione: se ci sono variabili hidden della nostra CPT che compongono alcuni dei valori della probabilità che stiamo cercando di calcolare, dovremo recuperarle tutte e assegnare tutti i possibili valori.

Abbiamo dei problemi:

1) Complessità temporale: $\mathcal{O}(d^n)$ (con $n$ = numero variabili e $d$ = arietà più grande)
2) Complessità temporale per la distribuzione congiunta: $\mathcal{O}(d^n)$
3) E' difficile inserire tutti i valori necessari!
  
## **Indipendenza (e indipendenza condizionale)**

Due variabili aleatorie sono **indipendenti** sse
$$
P(A\ |\ B) = P(A)\ o\ P(B\ |\ A)=P(B)\\
o\\
P(A,B) = P(A) \cdot P(B)
$$
Esempio:
$$
P(Toothache, Catch, Cavity, Weather) = P(Toothache,Catch,Cavity) \cdot P(Weather)
$$

Così facendo abbiamo ridotto il numero di entry da 32 a 12.  
Usare la **chain rule** per semplificare le dipendenze condizionali riduce la lunghezza della rappresentazione della distribuzione congiunta da esponenziale ($2^{n}-1$) a lineare (somma). L'indipendenza condizionale è **fondamentale** e **robusta**.
  
## Regola di Bayes

Ricordiamo la Product rule
$$
P(a\wedge b)=P(a\ |\ b)P(b)=P(b\ |\ a)P(a)
$$

La regola di bayes è utile per valutare la probabilità diagnostica dalla Probabilità causale e dice
$$
P(a\ |\ b) = \frac{P(b\ |\ a)P(a)}{P(b)} = \alpha \cdot P(b\ |\ a)P(a)
$$
Versione estesa:
$$
P(Y\ |\ X,Z) = \frac{P(X\ |\ Y,z)P(Y\ |\ Z)}{P(X\ |\ Z)}
$$

## **Paradosso di Monty-Hall (e BN)**

Nel gioco vengono mostrate al concorrente tre porte chiuse; dietro ad una si trova un'automobile, mentre ciascuna delle altre due nasconde una capra.  
Il giocatore può scegliere una delle tre porte, vincendo il premio corrispondente.  
Dopo che il giocatore ha selezionato una porta, ma non l'ha ancora aperta, il conduttore dello show – che conosce ciò che si trova dietro ogni porta – apre una delle altre due, rivelando una delle due capre, e offre al giocatore la possibilità di cambiare la propria scelta iniziale, passando all'unica porta restante; cambiare la porta migliora le chance del giocatore di vincere l'automobile, portandole da **1/3 a 2/3**.  
![Monty Hall](montyHall.jpg)

## **Paradosso di Simpson (e BN)**  

Il paradosso di Simpson indica una situazione in cui una relazione tra due fenomeni appare modificata, o perfino invertita, dai dati in possesso a causa di altri fenomeni non presi in considerazione nell'analisi (variabili nascoste).  
![BN](simpson_bn.png)
![Data](simpson_data.png)

$$
P(R=T\ |\ do(D=T)) = sum_S P(R=T\ |\ D=T,S=U)\ P(U) + P(R=T\ |\ D=T,S=D)\ P(S=D) =\\
0,93 * 357/700 + 0,73 * 343/700 = 0,832 = 83,2\%\\
\ \\
P(R=T\ |\ do(D=F)) = sum_S P(R=T|D=F,S=U) P(U) + P(R=T|D=F,S=D)P(S=D) = \\
0,87 * 357/700   +  0,69 * 343/700 = 0,7818 = 78,18\% \\
\ \\
P(R=T|D=T) = 78\%\ \text{(come da tabella) ma NON è}\ P(R=T | do(D=T))=83,2\%\\
\ \\
P(R=T|D=F) = 83\% \text{(come da tabella) ma NON è}\ P(R=T | do(D=F))=78,18\%
$$

## **Reti Bayesiane**

Notazione grafica per modellare delle asserzioni di (in)dipendenza condizionale. E' anche uno strumento utile a spiegare le distribuzioni congiunte complete
  
### **Componenti**

- DAG (Directed Acyclic Graph)
- CPT (uno per nodo, dove ogni nodo è una variabile aleatoria).  
Esprime $P(x_i | Parents(x_i))$
  
### **Significato delle frecce nelle reti bayesiane**  

Esprimono rapporti causali nella rete.

### **Compattezza**  

Per un nodo $X_i$ con $k$ parents, abbiamo una cpt grande $2^k$  
La rete ha un limite superiore di $O(n \cdot 2^k)$ ($n$ nodi e $2^k$ cpt)

### **Semantica** (Globale, Locale, Markov Blanket)

#### **Globale**

La distribuzione congiunta è il prodotto delle distribuzioni condizionali locali $P(X_1...X_n) = \prod^{n}_{i=1} P(X_i | Parents(X_i))$

#### **Locale**

Equivalente a quella globale (per teorema): ogni nodo è condizionalmente indipendente dai discendenti, dati i $Parents$

#### **Coperta di Markov**

Ogni nodo è condizionalmente indipendente dai nodi che sono al di fuori della sua coperta di markov. La CdM sono tutti i nodi corrispondenti ai padri, i figli e padri dei figli.
  
### **Come si costruisce**

1) Scegliamo un ordine delle variabili (preferibilmente causale, per evitare reti "deboli", cioè meno compatte, con le quali è più difficile fare inferenza e decidere l'indipendenza condizionale)
2) Per ogni variabile aleatoria aggiungiamo un nodo alla BN
3) Selezioniamo i suoi Parents, in modo che garantiscano la semantica globale.

### **Reti Bayesiane Ibride**

Sono reti con variabili aleatorie nel continuo, oltre che nel discreto.  
Visto che non è possibile assegnare una CPT a variabili aleatorie nel continuo, e quindi fare in modo che i padri e i figli si influenzino a vicenda, bisogna trovare un modo:

1) Discretizzando: non va bene perchè perdiamo valori di probabilità (perdiamo in precisione)
2) Se abbiamo dei padri continui, creiamo una funzione a scalino (sigmoide) per approssimare l'infinità del continuo
3) Se abbiamo padri discreti, bisogna avere una distribuzione diversa per ogni valore nel discreto

### **Rete Bayesiana Naive, calcolo delle probabilità della causa dati gli effetti**

### **Task di Inferenza** (Simple/Conjunctive Queries, MPE, MAP)

#### **Simple queries**

Probabilità a posteriori di **una** variabile data l'evidenza (cioè gli assegnamenti delle altre variabili specificate)

#### **Conjunctive Queries**

Probabilità a posteriori di **più** variabili data l'evidenza.

#### **Most Probable Explanation**

$MPE(e)=argmax_xP(x,e)$ (si cerca l'istanziazione più probabile di tutte le variabili $X$ data l'evidenza $e$) (Cerca la combinazione migliore tra i valori ammissibili delle variabili che fanno parte di $X$, andando a massimizzare la probabilità)

#### **Maximum a Posteriori Probability**

Istanziazione più probabile di un sottoinsieme di variabili $M \subseteq X$ data l'evidenza $e$. $MAP(e)=argmax_mP(m,e)$

### **Inferenza Esatta**

Sono entrambi temporalmente esponenziali rispetto alla treewidth della BN (per ordinamenti non buoni)

- **Per Enumerazione**: è una sumout delle variabili hidden (che non fanno parte dell'evidenza fornita)
- **Per Variable Elimination**: è come l'enumerazione, ma con la programmazione dinamica salviamo i fattori durante la computazione (bottom-up).  
  L'idea è che continuiamo ad eliminare variabili una alla volta finchè non giungiamo all'estrazione della soluzione. Per eliminare una variabile facciamo la sumout
- **Complessità e problemi** (scelta dell'ordinamento, width, treewidth)
  - **Enumerazione**
    - Nel caso di una catena, $X_1 \rightarrow X_2 \rightarrow ... \rightarrow X_n$ e una simple query $\mathcal{P}(X_1|X_n)$, con l'enumerazione ho una complessità di $\mathcal{O}(2^n)$ (questo perchè dobbiamo ripetere la costruzione di fattori ogni volta che facciamo una somma)
  - **Variable Elimination**
    - Con una catena, O(n). La width è 2 per la catena e quindi rimaniamo lineari.
    - Con una query del tipo $\mathcal{P}(X_1|Y_n)$ l'inferenza per enumerazione produce la $P(X_1)*\sum_{Y_1}*\sum_{Y_2}*...*\sum_{Y_{n-1}}*\phi$, dove $\phi$ è uin fattore con $n-1$ variabili, quindi anche qui abbiamo una complessità di $\mathcal{O}(2^n)$.
  - **Inferenza Esatta** (Ordinamento)  
    Scegliere un ordinamento migliore è un problema NP-Hard  
    Le BN si dividono in
    - Polytree: NP-Hard perchè Lineare in n ma esponenziale nel numero dei padri del nodo $\mathcal{O}(d^k*n)$. (Se a un polytree toglimo un arco non abbiamo cicli)
    - Multiconnected: #P-Concrete, equivalente a contare modelli 3-Sat (problema al minimo NP-Hard e al massimo P-Concrete)
  - **Treewidth**: minima width tra tutti gli ordinamenti di eliminazione (width del miglior ordinamento di eliminazione), dove la width è il numero di variabili nel fattore più grande.

### **Struttura** (Chain, Divergent, Convergent, Path Bloccati)

- Chain: sequenza $X_1 \rightarrow X_2 \rightarrow ... \rightarrow X_n$
- Divergent: fork $X_1 \leftarrow X_2 \rightarrow X_3$
- Convergent: collider $X_1 \rightarrow X_2 \leftarrow X_3$
- Path Bloccato: tra X e Y sono bloccati se c'è una sottostruttura bloccata
  - Chain: quando X2 è bloccato
  - Fork: Quando X2 è bloccato
  - Collider: $(X_2 \cup Descendants(X_2)) \cap \mathcal{Z} = \empty$. ($\mathcal{Z}$ è un insieme bloccato) Nel collider tutto tranne $X_2$ può essere bloccato.

### **D-Separazione**

$X$ e $Y$ sono D-Separati da $\mathcal{Z}$ sse tutti i path sono bloccati da $\mathcal{Z}$  
Se due variabili sono D-separate, allora sono condizionalmente indipendenti.
  
### **Effetto Causale**

Le BN sono molto utili per fare inferenza sulle varabili (indipendentemente dalla causalità); lo scopo di moltissime analisi statistiche è di tipo causale (ad esempio giustificare un intervento)  

#### **Operatore DO**

Rivela l'effetto causale di $X$ su $Y$: togliamo tutti gli archi entranti in $X$ quando blocchiamo $do(X = x)$

- Come differisce dalla probabilità condizionata?  
  La P condizionata rivela la correlazione, mentre la do rivela l'effetto causale

#### **Causal effect rule**

$P(Y=y|do(X=x)) = \sum_ZP(Y=y | X=x, PA = Z) * P(PA = Z)$ (PA = Padri)

### **Inferenza Approssimata**

Esiste perchè quella esatta è esponenziale da calcolare rispetto alla treewidth della BN, e in alcune reti non esistono ordinamenti con width piccole. L'inferenza esatta non è quindi sempre applicabile. Si vuole quindi scambiare un po' di precisione per più efficienza.  
Si basa sullo stochastic sampling (metodo Montecarlo): simuliamo una serie di estrazioni casuali dalla distribuzione originale calcolando a forza bruta una stima di probabilità (basata sulla frequenza dei campioni estratti).  
L'obiettivo è dimostrare che questa stima di $\hat{P} $ tendendo all'infinito, converge a quella esatta.  
Usiamo una rete Bayesiana come **generatore** di esempi (campioni) in avanti, cioè in modo stocastico.  
La vera probabilità a priori di $P(t,f,...) = \text{produttoria delle combinaizoni nelle CPT}$

#### **Rejection Sampling** (Algoritmo, Analisi e problemi)

L'idea è di eliminare i campioni generati che non sono consistenti con l'evidenza. Per farlo, abbiamo un prior sample (un assegnamento delle variabili della BN) e verifichiamo che questo sia consistente con l'evidenza.

$$
\hat{P}(Rain\ |\ Sprinkler = true)\\
\text{100 campioni. 27 con}\ Sprinkler = true\\
\text{Di questi 27, 8 sono con}\ Rain = true \text{e i restanti 19 con}\ Rain = false\\
\hat{P}(Rain\ |\ Sprinkler=true)=N(\langle 8,19 \rangle)
$$

- Cosa vuol dire che è consistente?
  Vuol dire che in una query del tipo $P(Rain | GrassWet = yes)$, i sample con $GrassWet = no$ verranno scartati.  
  Precisazione: più sample estraiamo e più il calcolo diventa preciso.
  $$
  \hat{P}(X | e) = \alpha N_PS(X, e) = \frac{NPS(x,e)}{NPS(e)} \\
  \simeq \frac{P(x,e)}{P(e)} \\
  = P(X | e)
  $$
  NPS = numero degli elementi in accordo con $e$  
  Notiamo anche un problema: diventa molto costoso fare questo rifiuto se $P(e)$ è molto piccolo (perchè con l'aumentare delle variabili di evidenza la sua probabilità diminuisce)

#### **Likelihood Weighting** (Algoritmo, Weighted-Sample, Analisi e problemi)

E' un esempio di **Importance sampling**: genera solo eventi che sono consistenti con l'evidence $e$.  
Invece di generare tutti i campioni dalla distribuzione a priori, cerchiamo di generarli dalla distribuzione che è già vicina a quella da stimare.
Ad ogni iterazione chiamiamo WeightedSample, che ci restituirà il peso e l'evento, che aggiungeremo ad un vettore di conti pesati su X.  
Il peso ci dice quanto dobbiamo credere alla valutazione.

- **Weighted Sample**  
    Il peso iniziale parte da 1 e

  - Se è una variabile di evidenza, viene moltiplicato per la probabilità congiunta ($P(x_i | Parents(X_i))$)
  - Se non è una variabile di evidenza, facciamo prior sampling (selezione random).

  Diamo quindi più importanza all'evidenza che ha un peso maggiore e automaticamente ignoreremo quello che invece ha un peso minore.

  $$
  S_{ws}(Z,e) = \prod^{l}_{i=1}P(z_i | Parents(Z_i))
  $$

  S_ws sono tutti i campioni della Likelyhood Weighting generati con la prior sampling  
  Dove Z sono le variabili NON di evidenza (hidden)  
  Mentre la weight è

  $$
  W(Z,e) = \prod^{m}_{i=1}P(e_i | Parents(E_i))
  $$

  W è il peso di tutte le variabili di evidenza  
  Dove $E$ è l'insieme delle variabili di evidenza.  
  Dunque, l'algoritmo totale produce

  $$
  S_{ws}(Z,e) \cdot W(Z,e) = P(Z,e)
  $$

  - **Problema**: la performance degrada (di meno della rejection sampling) all'aumentare dell'evidenza perchè pochi sample hanno tutto il peso
  - **Problema 2**: l'ordine dell'evidenza impatta fortemente sulla significatività del risultato
    - Se l'evidenza riguarda le prime variabili, allora i campioni saranno più probabili data l'evidenza (più significativi)
    - Se l'evidenza riguarda le ultime variabili, allora i campioni sono generati secondo le Probabilità a priori (indesiderato), e quindi sono meno significativi

    Se l'evidenza è in cima andiamo a tagliare tanti rami, altrimenti i campioni scendendo sono generati con la P a priori, ma è più complessa la ricerca. E' preferibile che l'evidenza riguardi le prime variabili e che la BN abbia una morfologia conveniente.

### **Modelli Temporali**

  E' una BN a cui aggiungiamo l'indicatore del tempo (discreto), cioè che noi frazioniamo nel tempo: $X_{a:b}=X_a...X_b$. Un modello "standard" è quello basato sulle catene di markov

#### **Catene di Markov** (first e second order, Sensor Markov Assumption)  

Nel costruire la BN assumiamo che la variabile X al tempo t dipenda soltanto da un insieme finito di variabili ai tempi $k < t$

- **First order**: $P(X_t | X_{0:t-1}) = P(X_t | X_{t-1})$
- **Second order**: $P(X_t | X_{0:t-1}) = P(X_t | X_{t-1}, X_{t-2})$
- **Sensor Markov Assumpion**: I modelli temporali sono processi stazionari, ovvero assumiamo che le loro CPT non cambino nel tempo.  
  $P(E_t | X_{0:t}, E_{0:t-1}) = P(E_t | X_t)$

Una catena di Markov di primo ordine spesso non rispetta la realtà e risulta approssimativa (Sprinkler: non possiamo basarci solo sul comportamento dello sprinkler di alcuni giorni prima, o su informazioni non complete). Per risolvere questo problema possiamo:

1) Aumentare l'ordine della catena
2) Arricchire la rete di stati con informazioni (sprinkler -+-> temperatura e pressione)
  
#### **Task di Inferenza su Modelli Temporali** (Filtering, Prediction, Smoothing e algoritmo forward backward)

- **Filtering**: $P(X_t | e_{1:t})$: la distribuzione a posteriori dello stato più recente. Calcoliamo il belief state data tutta l'evidenza ricavata  
  > Umbrella: _calcolare la probabilità di rain oggi, date tutte le osservazioni passate_
- **Prediction**: $P(X_{t+k} | e_{1:t})$: la distribuzione a posteriori dello stato futuro al tempo $t+k$ con $k > 0$
  > Calcolare la probabilità di rain con giorni d'anticipo
- **Smoothing**: $P(X_k | e_{1:t})$ con $0\leq k<t$: calcoliamo la distribuzione a posteriori di uno stato passato, date anche le osservazioni future (fino al presente). Ci fornisce un'approssimazione migliore perchè abbiamo potuto ottenere più evidenza.
- **Most Likely Explanation**: $argmax_{x_{1:t}}P(x_{1:t} | e_{1:t})$: vogliamo trovare la sequenza di stati che hanno generato più probabilmente queste osservazioni
  > 3 ombrelli e 1 no: ha piovuto 3 giorni e uno no.
