Lezione del 14 novembre 2025
Appunti di Alessandro Salerno

## Metodo della quantità pivotale per la media
Esattamente come nel caso delle proporzioni, sappiamo che:
$$ \frac{\overline{X}_n - \mathbb{E}(X)}{\sqrt{\frac{\operatorname{var}(X)}{n}}} \simeq N(0, 1) $$
Vogliamo nuovamente trovare:
$$ \mathbb{P}\left(q_1 \leq \frac{\overline{X}_n - \mathbb{E}(X)}{\sqrt{\frac{\operatorname{var}(X)}{n}}} \leq q_2 \right) = 1 - \alpha $$
Nuovamente, quindi presa $Z \sim N(0, 1)$ possiamo dire
$$ q_2 = Z_{1 - \frac\alpha2}$$
$$ q_1 = -q_2 $$
$$ \mathbb{P}\left(\overline{X}_n - Z_{1 - \frac\alpha2} \sqrt{\frac{\operatorname{var}(X)}{n}} \leq \mathbb{E}(X) \leq \overline{X}_n +Z_{1 - \frac\alpha2}\sqrt{\frac{\operatorname{var}(X)}{n}} \right) = \mathbb{P}\left(A_n \leq \mathbb{E}(X) \leq B_n \right) = 1 -\alpha $$

> [!IMPORTANT]
> Tuttavia, presupponiamo la conoscenza della varianza, che è un altro parametro teorico della legge $X$ a noi ignoto. Se $\operatorname{var}(X)$ non viene fornito nei dati del problema, bisogna sostituirla con $S^2$ (la varianza campionaria). Però questo cambia la distribuzione:
> $$ \frac{\overline{X}_n - \mathbb{E}(X)}{\sqrt{\frac{S^2}{n}}} \simeq \mathbb{t}(n - 1) $$
> Ossia la distribuzione $\mathbb{t}$ di Student con $n - 1$ gradi di libertà, molto simile, ma non uguale, alla normale. Questo vuol dire che nel metodo della quantità pivotale, è necessario sostituire $\operatorname{qnorm}$ con $\operatorname{qt}\left(n - 1, \frac\alpha2\right)$. Si ottiene quindi:
> $$  \mathbb{P}\left(\overline{X}_n - T_{n - 1, 1 - \frac\alpha2} \sqrt{\frac{s^2}{n}} \leq \mathbb{E}(X) \leq \overline{X}_n +T_{n - 1, 1 - \frac\alpha2}\sqrt{\frac{s^2}{n}} \right) = \mathbb{P}\left(A_n \leq \mathbb{E}(X) \leq B_n \right) = 1 -\alpha  $$
> 

> [!NOTE]
> Per $n$ sufficientemente grande, la distribuzione $\mathbb{t}$ di Student assomiglia sempre più ad una normale standard.

> [!NOTE]
> Se $X \sim N(\mu, \sigma^2)$ allora:
> $$ \frac{\overline{X}_n - \mathbb{E}(X)}{\sqrt{\frac{S^2}{n}}} \simeq \mathbb{t}(n - 1) $$
> Anche per $n$ piccola.

Su R, possiamo usare il comando `t.test`  anziché `binom.test`. 
