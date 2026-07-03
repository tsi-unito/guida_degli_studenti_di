Lezione del 7 novembre 2025
Appunti di Alessandro Salerno

## Correlazione campionaria di Pearson
La correlazione campionaria di Pearson misura quanto due variabili quantitative sono linearmente associate.
$$ \operatorname{pearson}(X, Y) = \frac{1}{n - 1}\sum_{i = 1}^n{\left(\frac{x_i - \overline{x}}{s_X}\right)\left(\frac{y_i - \overline{y}}{s_Y}\right)} \in [-1, 1] $$

## Correlazione campionaria di Spearman
Serve a determinare se esiste una correlazione anche non lineare tra variabili aleatorie, ossia se per $X, Y$ variabili aleatorie, valori maggiori di $X$ appaiono simultaneamente a valori maggiori di $Y$. 

Sia $\operatorname{rk}{x_i}$ la posizione della realizzazione $x_i$ nel vettore ordinato (crescente) delle realizzazioni della variabile aleatoria $X$ e  $\operatorname{rk}{y_i}$ per la variabile aleatoria $Y$ e siano $\operatorname{rk}{X}$ e $\operatorname{rk}{Y}$ le variabili aleatorie la cui immagine è composta rispettivamente dagli $\operatorname{rk}{x_i}$ e $\operatorname{rk}{y_i}$ Definiamo il coefficiente di correlazione di Spearman come:
$$ \operatorname{spearman}(X, Y) = \operatorname{corr}(\operatorname{rk}{X}, \operatorname{rk}{Y})$$

## Correlazione qualitativa
Date due variabili aleatorie qualitative $X, Y$, per calcolarne la correlazione possiamo utilizzare un'estensioen della tabella di frequenza in forma matriciale: nella cella $x_i, y_i$ si trova il numero di realizzazioni in cui è presente quella combinazione. 

È poi possibile visualizzare questa matrice come Mosaic Plot. 

## Statistica inferenziale
Ogni volta che si campiona una variable su una popolazione, si ottengono valori leggermente deversi (variabilità campionaria). La statistica inferenziale si occupa quindi di determinare valori non misurati partendo da quelli misurati. 

Campionare una popolazione può essere visto in due modi: una variabile aleatoria singola che restituisce le misurazioni, oppure l'insieme di realizzazioni di più variabili aleatorie diverse indipendenti tra loro e identicamente distribuite. Noi consideriamo questa seconda opzione: $(X_1, X_2, ..., X_N)$ vettore di variabili aleatorie indipendenti e identicamente distribuite (VVAID). 

## Statistica inferenziale parametrica
Data una variabile aleatoria, si vuole risalire ai suoi parametri, ossia i valori "passati" alla distribuzione della variabile.

Visto che varianza, media e _la proporzione_ sono spesso funzioni dei parametri, queste sono dette sovraparametri e spesso considereremo questi anziché i parametri.

### Proporzione
Sia $X$ una variabile aleatoria, allora
$$ p = \operatorname{prop}(X, [a, b]) = \mathbb{P}(a \leq X \leq b) $$
Consideriamo quindi:
$$ Y = \begin{cases}
1 \textrm{ se } x_i \in [a, b] \\
0 \textrm{ altrimenti }
\end{cases} \sim \mathbb{Be}(p) $$
Calcolare la proporzione di una variabile aleatoria $X$, quindi, equivale a calcolare il parametro $p$ della variabile $Y$ così descritta.

## Concetti teorici e pratici
Sia $X$ una variabile aleatoria di su cui si vuole effettuare uno studio parametrico. Allora: 
$$\mathbb{E}(X) = \overline{x} = \frac1n\sum_{i = 1}^n{x_i} $$
Possiamo vedere questo anche come:
$$ \mathbb{E}(X) = \overline{X} = \frac1n\sum_{i = 1}^n{X_i} $$

Trattando ogni misurazione come una variabile aleatoria..
$$ \operatorname{var}(X) = s^2 = \frac{1}{n - 1}\sum_{i = 1}^n{(x_i - \overline{x})^2}$$
Possiamo quindi costruire uno stimatore corrispondente come variabile aleatoria:
$$  \operatorname{var}(X) = S^2 = \frac{1}{n - 1}\sum_{i = 1}^n{(X_i - \overline{X})^2} $$

Per la proporzione, abbiamo:
$$ \hat{p} = \frac{\textrm{numero di osservazioni con successo}}{n} $$
Ed è ovviamente possibile stabilire uno stimatore anche per essa:
$$ \hat{P} = \frac1n\sum_{i = 1}^n{Y_i} \textrm{ con } Y_i \sim \mathbb{Be}(p) \to \hat{P} = \overline{y} $$
Per proprietà dele Bernoulli.
