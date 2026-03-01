Lezione del 13 novembre 2025
Appunti di Alessandro Salerno

## Intervallo di confidenza
Un intervallo di confidenza restituisce due cose:
- Un intervallo $[a, b]$
- Un livello di confidenza  $\alpha \in [0, 1]$ desiderabile piccolo

### Per proporzioni
In questo caso la variabile aleatoria ha solo due risultati possibili: il campione estratto ha una caratteristica oppure non ce l'ha. Questo le rende logicamente equivalenti all'estrazione di un campione di Bernoulli. 
$$ \hat{P} = \frac{\textrm{numero di successi}}{n} = \frac1n\sum_{i = 1}^nX_i \textrm{  visto che } X \sim \mathbb{Be}(p) $$
Per il Teorema del Limite Centrale:
$$ \operatorname{var}(X) = p(1 -p) $$
$$ \mathbb{E}(X) = p $$
$$ \hat{P}_n \simeq N\left(p, \frac{p(1 - p)}{n}\right) $$
Vogliamo quindi trovare $A_n$ e $B_n$ tali che:
$$ \mathbb{P}(A_n \leq p \leq B_n) = 1 -\alpha $$

### Metodo della quantità pivotale
Dall'enunciato originale del Teorema del Limite Centrale, sappiamo che applicando il processo di standardizzazione si ottiene:
$$ \operatorname{standard}(\hat{P}_n) = \frac{\hat{P}_n - \mathbb{E}(\hat{P}_n)}{\sqrt{\operatorname{var}(\hat{P}_n)}} = \frac{\hat{P}_n -p}{\sqrt{\frac{p(1 - p)}{n}}} \simeq N(0, 1) $$
Dal momento in cui stiamo cercando $p$ (ricordiamo che la statistica parametrica cerca i delle variabili aleatorie), sostituiamo i riferimenti a $p$ con il suo stimatore:
$$ \operatorname{approxstd}(\hat{P}_n) = \frac{\hat{P}_n -p}{\sqrt{\frac{\hat{P}_n(1 - \hat{P}_n)}{n}}} \simeq N(0, 1) $$

Riusciamo quindi a trovare due numeri $q_1, q_2$ tali che:
$$ \mathbb{P}\left(q_1 \leq \operatorname{approxstd}(\hat{P}_n) \leq q_2 \right) = 1- a $$
Ossia data $Z \sim N(0, 1)$:
$$ \mathbb{P}(Z \leq q_1) = \frac\alpha2 $$
Che corrisponde perfettamente alla definizione di quantile. Basta quindi calcolare il quantile di rodine $\frac\alpha2$ per la normale standard per ottenere $q_1$, quindi in R:
$$ q_1 = \operatorname{qnorm}\left(\frac\alpha2\right) $$
Per trovare $q_2$, invece, sfruttando la proprietà di simmetria della normale standard, possiamo dire:
$$ q_2 = -q_1 $$
Successivamente, attraverso una serie molto lunga di trasformaizoni banali, otteniamo che:
$$ \mathbb{P}\left(\hat{P}_n - Z_{1 - \frac\alpha2}\sqrt{\frac{\hat{P}_n(1 - \hat{P}_n)}{n}} \leq p \leq \hat{P}_n - Z_{\frac\alpha2}\sqrt{\frac{\hat{P}_n(1 - \hat{P}_n)}{n}} \right) = \mathbb{P}(A_n \leq p \leq B_n) = 1 - \alpha $$
Per la proprietà di simmetria già menzionata, questo è equivalente a:
$$ z = Z_{1 - \frac\alpha2}\sqrt{\frac{\hat{P}_n(1 - \hat{P}_n)}{n}} $$
$$ \mathbb{P}\left(\hat{P}_n -z \leq p \leq \hat{P}_n + z\right) = 1 - \alpha $$
Da questo si intuisce facilmente che l'errore $z$ è non-lineramente inversamente proporzionale al numero di osservazioni $n$. Su R si può usare il comando:
```
binom.test(numero di successi, numero di prove, conf.level = livello di confidenza)
```
