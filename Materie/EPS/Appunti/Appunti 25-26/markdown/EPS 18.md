Lezione del 28 ottobre 2025
Appunti di Alessandro Salerno (immagini dagli appunti della Prof.ssa L. Andreis)

## Variabili aleatorie (assolutamente) continue
Sia $X$ una variabile aleatoria continua, si dice _densità della variabile aleatoria $X$_ la funzione:
$$ f_X \ : \ \mathbb{R} \to \mathbb{R} $$
Tale che:
$$ \forall A  \subseteq \mathbb{R} \ \ \mathbb{P}(X \in A) = \int_A{f_X(t) \ dt} $$
![[Pasted image 20251028155601.png]]

### Proprietà della funzione densità
1. Il famoso integrale su tutta la retta deve fare per forza 1
2. La funzione è sempre positiva

## Valore atteso, varianza e momenti di variabile aleatoria continua
Sia $X$ una variabile aleatoria assolutamente continua con densità di probabilità $f_X$, il valore atteso (media) di $X$ si definisce:
$$ \mathbb{E}(X) = \int_{-\infty}^{+\infty}{f_X(t)t \ dt} $$
$$ \mathbb{V}ar(X) = \mathbb{E}[(X - \mathbb{E}(X))^2] = \int_{-\infty}^{+\infty}{f_X(t)(t -\mathbb{E}(X))^2 \ dt} $$
Per qualsiasi $k$, si definisce:
$$ \mathbb{E}[X^k] = \int_{-\infty}^{+\infty}{t^kf_X(t) \ dt} $$
Momento $k$-esimo.

La funzione indicatrice $i(f, a, b)$ indica se $f$ non è zero sull'intervallo $[a, b]$. 

## Variabili note
Fissiamo un qualsiasi intervallo $[a, b]$ della retta. Definiamo 
- $X \sim \mathbb{Unif}[a, b]$ se la sua densità $f_X$ è costante sull'intervallo $[a, b]$. Il valore fisso è $c$ per cui $c(b - a) = 1$.  Questo segue banalmente dall'area del rettangolo calcolata mediante l'integrale di una funzione con valore costante tentendo in ocnsiderazione che la probabilità massima è $\leq 1$. 
- $X \sim \mathbb{Exp}(\lambda > 0)$ se la sua densità è $f_X(t) = \lambda e^{-\lambda t}$ Presenta la proprietà di assenza di memoria, ossia $\mathbb{P}(X > s + t \ | \ X > t) = \mathbb{P}(X > s)$ per due valori reali $s, t$. Utile per modellare la vita di apparecchiature (per esempio elettroniche: è più probabile che duri un anno appena comprato che non che duri un anno quando ne ha già 10)
 
