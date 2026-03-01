Lezione del 11 novembre 2025
Appunti di Alessandro Salerno

## Stime puntuali
Permettono di stimare media, varianza, proporzioni utilizzando valori campionari. 

## Stimatori
Sono stime puntuali ma applicate alle variabili aleatorie, visto che $x$ è una realizzazione e $X$ è una variabile aleatoria.
$$  \overline{X} = \frac{1}{n}\sum_{i = 1}^n{X_i} $$
E otteniamo che che:
$$ \mathbb{E}(\overline{X}) = \mathbb{E}(X) $$
Come dimostrato nella lezione precedente.
Se uno stimatore ha valore atteso uguale alla quantità che vuole stimare, lo stimatore si dice *corretto*. 

> [!NOTE]
> Gli stimatori introdotti finora sono tutti corretti.


## Osservazione
La varianza della somma di due variabili aleatorie è banalmente la somma delle varianze solo se le due variabili aleatorie sono indipendenti.  Generalizzabile a $n$ variabili aleatorie. Ciò intuitivamente implica che:
$$ \operatorname{var}(\overline{X}) = \frac{\operatorname{var}(X)}{n} $$
Dove $n$ è la dimensione del campione. 

## Legge dei grandi numeri v2
Sia $X_1, X_2, ...$ una collezione infinita di variabili aleatorie indipendenti e identicamente distribuite con la stessa legge della variabile aleatoria $X$. Allora: 
$$ \lim_{n \to +\infty}{\left(\overline{X}_n = \frac{1}{n}\sum_{i = 1}^n{X_i}\right)} = \mathbb{E}(X) $$
Ossia la variabil aleatoria media campionaria si avvicina sempre di più al NUMERO valore atteso di $X$. All'infinito teorico ipotetico, la variabile aleatoria diventa numero essa stessa. 

Questo vale anche per gli altri sovraparametri:
$$ \lim_{n \to +\infty}{\left(S^2_n = \sum_{i = 1}^n{(X_i - \overline{X})^2}\right)} = \operatorname{var}(X) $$
$$ \lim_{n \to +\infty}{\left(\hat{P} = \frac{|\{i \ | \ x_i \in [a, b]\}|}{n} =   \frac{\textrm{numero di osservazioni con successo}}{n} \right)} =  \mathbb{P}(a \leq X \leq b) $$

## Teorema del limite centrale 
Sia $X_1, ..., X_n$ campione aleatorio (variabili aleatorie indipendenti e identicamente distribuite) di legge $X$, allora 
$$ \forall x \in \mathbb{R}, \ \ \lim_{n \to + \infty}{\mathbb{P}\left(\frac{\overline{X_n} - \mathbb{E}(X)}{\sqrt{\frac{\operatorname{var}(X)}{n}}} \leq x \right)} = \mathbb{P}(Z \leq q) \textrm{ con } Z \sim N(0, 1) $$


> [!NOTE]
> Se $\overline{X}$ fosse una variabile aleatoria normale, allora
> $$ \frac{\overline{X } - \mathbb{E}(X)}{\sqrt{\frac{\operatorname{var}(X)}{n}}} \sim N(0, 1)$$
> 

Da cui concludiamo che $\overline{X}_n$ per $n$ sufficientemente grande, assomiglia ad una normale $N\left(\mathbb{E}(X), \frac{\operatorname{var}(X)}{n}\right)$. 

Il teorema del limite centrale vale anche per le proporzioni:
$$ \hat{P}_n = \frac1n\sum_{i = 1}^nY_i = \overline{Y}_n $$
Per $n$ grande:
$$ \hat{P}_n \approx N\left(p, \frac{p(1 -p)}{n}\right) $$

Tuttavia non vale per la varianza campionaria in quanto somma di quadrati. Se $(X_1, X_2, ..., X_n)$ campione casuale di legge $X \sim N(\mu, \sigma^2)$, allora
$$ \frac{S^2_n(n - 1)}{\sigma^2} \sim X^2(n - 1) $$
Si tratta di una variabile aleatoria $t_2$ assomiglia a una normale per valori molto grandi. 

## Metodi per controllare la variabilità campionaria
Ci sono due metodi:
- Intervalli di confidenza
- Test d'ipotesi
