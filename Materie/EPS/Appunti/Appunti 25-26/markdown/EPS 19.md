Lezione del 30 ottobre 2025
Appunti di Alessandro Salerno

## Variabile aleatoria Gaussiana/normale
Siano $\mu \in \mathbb{R}$, $\sigma^2 \in \mathbb{R}+$, $X$ si dice variabile aleatoria di media $\mu$ e varianza $\sigma^2$ se la densità:
$$ f_X(t) = \frac{1}{\sqrt{2\pi\sigma^2}}e^{-\frac{(t - \mu)^2}{2\sigma^2}} $$
E si dice:
$$ X \sim N(\mu, \sigma^2) $$

Sia quindi $X$ una variabile aleatoria continua tale che $X \sim N(\mu, \sigma^2)$, allora vale:
$$ \mathbb{P}(X \leq \mu) = \mathbb{P}(X \in (-\infty, \mu]) = \int_{-\infty}^{\mu}{f_X(t) \ dt} = \frac12 $$

E si dice _normale standard_ se $X$ è normale di media 0 e varianza 1. 
		
Per VA normali, $\forall a, b \in \mathbb{R}$ allora 
$$ Y = aX +b $$
È una variabile aleatoria normale con valre atteso $a\mu +b$ e varianza $a^2\sigma^2$. 

## Funzione di distribuzione cumulata
 Sia $X$ una variabile aleatoria (discreta o continua, non ci interessa). Si dice funzione di distribuzione cumulata e si indica con CDF la funzione definita come :
 $$ F_X \: \ \mathbb{R} \to \mathbb{R} $$
$$ x \to F_X(x) = \mathbb{P}(X \leq x) $$
Se $X$ discreta e PMF $P_X$ nota, allora:
$$ F_X(x) = \sum_{k \in Im(X), k \leq x}P_X(k) $$
Se invece è continua e PDF $f_X$ nota, allora:
$$ F_X(x) = \int_{-\infty}^x{f_X(t) \ dt} $$

### Proprietà per $X$ discreta
- $\lim_{x \to -\infty}{F_X(x)} = 0$
- $\lim_{x \to +\infty}{F_X(x)} = 1$
- $F_X$ è costante a tratti
- $F_X$ è continua da destra
- $F_X$ è non decrescente

### Proprietà per $X$ continua
- $\lim_{x \to -\infty}{F_X(x)} = 0$
- $\lim_{x \to +\infty}{F_X(x)} = 1$
- $F_X$ è continua
- $F_X$ è non decrescente
 
 
