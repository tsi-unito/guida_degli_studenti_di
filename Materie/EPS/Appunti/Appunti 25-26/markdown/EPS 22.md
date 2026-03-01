Lezione del 4 novembre 2025
Appunti di Alessandro Salerno (con immagini dagli appunti delal Prof.ssa L. Andreis)

## Istogramma
L'asse X è composto da intervalli di $\mathbb{R}$ ed è presente una colonna per ognuno (detto _bin_). L'altezza di ogni oclonna è proporzionale alla realizzazione della variabile aleatoria in corrispondenza dell'intervallo. 
$$ \textrm{frequenza dell'intervallo } [a, b] = \textrm{numero osservazioni in } [a, b] $$
$$ \textrm{frequenza relativa dell'intervallo } [a, b] = \frac{\textrm{numero di osservazioni nell'intervallo } [a, b]}{\textrm{numero osservazioni totali}}$$
L'istogramma è una stima della funzione di distribuzione / di massa (se continua / discreta). 

## Box plot
Serve a rappresentare i quantili.
istogramma![[Pasted image 20251128120434.png]]

Nella scatola c'è il 50% dei dati con una linea sul quantile 50, mentre nei "baffi" ci sono il 25% minore e maggiore. Gli outliers sono esterni. 

## Statistica univariata per dati qualitativi (categorie)
Quando si trattano caratteristiche qualitative, non è possibile stabilire un valore numerico da utilizzare nei calcoli. Per questo, si utilizzano variabili contatrici che associano ad ogni qualità il numero di occorrenze nel dataset. 

## Bar chart
Esattamente come l'istogramma, ma per caratteristiche qualitative.  
Sull'asse X sono quindi presenti fattori qualitativi (rappresentati dalle loro etichette) anziché intervalli di $\mathbb{R}$ al contrario di quanto avviene per gli istogrammi. 

## Dot chart
Il peggior tipo di grafico che possa mai esistere, soprattutto se per qualche disgrazia nasci ipovedente. Serve principalemtne a descrivere dati discreti e non ordinati in modo più pulito rispetto a un line chart tra essi. Per il resto ptorebbe benissimo esplodere ed il mondo ne sarebbe asai più bello, per citare un grande poeta. 

## Pie chart (grafico a torta)
Assumo che tu sappia cosa sia, volevo solo dire che è stato trattato. 

## SCatter plot
Prese due variabi aleatorie $X, Y$, rappresentiamo tutti i possibili vettori aleatori $(x, y)$ sul grafico come punti.  Da questo grafico è possibile notare se la relazione $X, Y$ sembra seguire uan dipendeza:
![[Pasted image 20251128145301.png]]

## Covarianza
Siano $X, Y$ variabili aleatorie e $(X, Y)$ vettore aleatorio. Allora definiamo:
$$  \operatorname{cov}(X, Y) = \mathbb{E}\left[\left(X - \mathbb{E}(X)\right) \left(Y - \mathbb{E}(Y)\right) \right] $$
### Proprietà
- Se $X, Y$ indipendenti, allora $\operatorname{cov}(X, Y) = 0$
- Se $\mathbb{E}(X) = \mathbb{E}(Y) = 0$, allora $\operatorname{cov}(X, Y) = \mathbb{E}(XY)$
- Se tra $X, Y$ esiste una relazione lineare diretta, allora $\operatorname{cov}(X, Y))$ è (molto) positiva
- Se tra $X, Y$ esiste una relazione lineare inversa, allora $\operatorname{cov}(X, Y))$ è (molto) negativa

## Correlazione
Versione normalizzata della covarianza.
$$ \operatorname{corr}(X, Y) = \frac{\operatorname{cov}(X, Y)}{\sqrt{\operatorname{var}(X)\operatorname{var}(Y)}} \in [-1, 1]$$
Se $\operatorname{corr}(X, Y) \simeq 0$ allora $X, Y$ **possono** essere indipendenti, oppure avere una correlazione non lineare. 

_Ci sono altri dettagli che non capiamo ma sembrano marginali_
