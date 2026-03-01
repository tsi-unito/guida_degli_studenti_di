Lezione del 17 novembre 2025
Appunti di Alessandro Salerno

> [!WARNING]
> Questo documento si concentra principalmente su riportare gli aspetti operativi dei concetti menzionati. La lezione in presenza tratta anche gli aspetti teorici.

## Metodo della quantità pivotale per la varianza
Possiamo usare la varianza campionaria come stimatore:
$$ S^2_n = \frac{1}{n - 1}\sum_{i = 1}^n{\left(X_i - \overline{X}_n\right)^2} $$
Tuttavia, come visto nella lezione 24, per la varianza campionaria non vale il Teorema del Limite Centrale, ma se $X \sim N(\mu, \sigma^2)$, allora:
$$  \frac{S^2_n(n - 1)}{\sigma^2} \sim \chi^2(n - 1) $$
Vogliamo sempre trovare $q_1, q_2$ tali che:
$$ \mathbb{P}\left(q_1 \leq  \frac{S^2_n(n - 1)}{\sigma^2} \leq q_2 \right) = 1 - \alpha $$
Usando però la legge $\chi^2$. 

### Distribuzione di $\chi^2$
Trattandosi di un rapporto tra due quadrati (come illustrato sopra), la variabile non è mai negativa. Su R:
```r
asse_y = dchisq(asse_x, df = <gradi di libertà>)
````
Questo vuol dire che non possiamo sfruttare la proprietà di smmetria vista per gli altri parametri.

### Determinare $q_1, q_2$
```r
q1 = qchisq(<alpha> / 2, n -1)
q2 = qchisq(1 - (<alpha> / 2), n - 1)
```

### Espllicitare rispetto $\operatorname{var}(X)$
```r
install.package("DescTools")
library(DescTools)
v = varCI(<valori>, conf.level = 1 - <alpha>)

install.package("LearningStats")
library(LearningStats)
v = variance.CI(<valori>, conf.level = 1 - <alpha>)
```

## Intervalli di confidenza per la differenza tra medie
Immaginiamo di voler confrontare due medie (es: pazienti che prendono un farmaco vs gruppo di controllo), vogliamo stabilire se la differenza nella media campionaria è causata da un errore statistico oppure se le due medie sono veramente diversi. In questo caso, è necessario trovare un intervallo di confidenza per la DIFFERENZA delle medie. 

Per esempio, per escludere che due medie siano uguali, basta osservare se lo zero è incluso nell'intervallo di confidenza per la loro differenza.

Presi due campioni $X^{SI}, X^{NO}$ (il primo con una caratteristica ed il secondo senza) con taglia $n$ ed $m$ rispettivamente (anche per $n \neq m$), allora per la differenza:
$$ \mathbb{E}(X^{SI}) - \mathbb{E}(X^{NO}) \sim N\left(\mathbb{E}(X^{SI}) - \mathbb{E}(X^{NO}), \frac{\operatorname{var}(X^{SI})}{n} + \frac{\operatorname{var}(X^{NO})}{m}\right)$$
Vale il Teorema del Limite Centrale.

> [!IMPORTANT]
> Le due popolazioni sono indipendenti. Se non lo sono, è necessario utilizzare un altro comando. Nel caso in cui i dati non sono indipendenti (es: prima e dopo) bisogna utilizzare il modificatore `paired` per `t.test`.