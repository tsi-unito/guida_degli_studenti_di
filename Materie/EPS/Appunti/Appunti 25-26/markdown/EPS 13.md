Lezione del 10 ottobre 2025
Appunti di Alessandro Salerno

## Variabile del francese (Poisson) 
È una variabile aleatoria che conta il numero di eventi di interesse che occorrono in una certa fissata finestra temporale detta finestra di osservazione. 

$$ X \sim \mathbb{Po}(\lambda) \textrm{ con } \lambda > 0 $$
$$ Im(X) = \{0, 1, 2, ... \} $$
Per $k \in Im(X)$, si ha:
$$ P_X(k) = \mathbb{P}(X = k) = \frac{\lambda^k}{k!}e^{-\lambda} $$

### Proprietà particolare
Sia $X$ una variabile di Poisson con parametro $\lambda$ ed una finestra di osservazione fissata. Allora la variabile aleatoria $Y$ che conta il numero di eventi nella finestra di osservazione modificata $kF$ è ancora una variabile aleatoria di Poisson ma con parametro $k\lambda$. 

## Variabile aleatoria ipergeometrica
Un urna contiene $N$ elementi, di cui  $C$ elementi con una caratteristica e $N - C$ senza. La variabile aleatoria $X$ conta il numero di successi, dove _successo_ è definito come l'ottenere un elemento con caratteristica in un sottoesperimento.
$$ P_X(k) = \mathbb{P}(X = k) = \mathbb{P}(\{\omega \in \Omega \ | \ X(\omega) = k\}) = \frac{\binom{C}{k} \binom{N - C}{n - k} }{ \binom{N}{n} } $$
Con $n$ numero di estrazioni e $k \in Im(X)$.
