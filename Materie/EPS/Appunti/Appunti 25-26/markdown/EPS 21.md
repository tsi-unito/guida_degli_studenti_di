 Lezione del 3 novembre 2025
 Appunti di Alessandro Salerno (con immagini dagli appunti della Prof.ssa L. Andreis)
 
## Media campionaria (media empirica)
E il valore centrale delle mie osservazioni :
$$\overline{x} = \frac{1}{n}\sum_{i = 1}^{n}{x_i}$$
È influenzata da eventuali valori agli estremi.

## Mediana
È quel valore tale per cui 50% delle misurazioni risultano inferiori a $m$ e il 50% risultano superiori a $m$.

### Osservazioni sulla mediana
Se la media campionaria ($\overline{x}$) è :
$$\overline{x} < m$$
allora, vuol dire che ci sono valori estremi a sinistra (piccoli).
Invece, se :
$$\overline{x} > m$$
allora, vuol dire che ci sono valori estremi a destra (grandi).
Ancora, se :
$$\overline{x} \simeq m$$
allora non abbiamo dati estremali, i dati sono distribuiti in maniera abbastanza simmetrica intorno a m.

## Quantili (percentile)
Sia $\alpha \in [0, 100]$, $Q_\alpha$ è quel valore tale che a sinistra di $Q_\alpha$ ci sta il $\alpha$% delle osservazioni.

> [!NOTE] Quartili
> I quartili sono quantili noti, ovvero : 25, 50, 75 (rispettivamento primo, secondo e terzo quartile). 

## Varianza campionaria (indici di dispersione)
$$s^2 = \frac{1}{n-1}\sum_{i = 1}^{n}{(x_i-\overline{x})^2} $$

> [!NOTE]
> Dalla definizione di varianza segue che non è possibile comparare i suoi valori con la media ed è quindi necessario utilizzare la deviazione standard ($s = \sqrt{s^2}$).

## Range (indice di dispersione 2)
È l'intervallo più piccolo che può contenere tutti i dati.
$$ \textrm{range} = [\min{x_i}, \max{x_i}]$$

## Coefficiente di variazione
$$ \textrm{cv} = \frac{s}{\overline{x}}$$
Osserviamo che se $X \sim \mathbb{Exp}(\lambda)$, allora:
$$ \mathbb{E}(X) = \frac1\lambda $$
$$ \operatorname{var} X = \frac1{\lambda^2} $$
$$ s = \sqrt{\operatorname{var} X} = \frac1\lambda $$
$$ \textrm{cv}(X) = \frac{\frac1\lambda}{\frac1\lambda} = 1 $$
Da cui segue che:
- Se $\textrm{cv}(Y) < 1$, allora $Y$ varia "meno" di $X$
- - Se $\textrm{cv}(Y) = 1$, allora $Y$ varia "ugualmetne" ad $X$
- - Se $\textrm{cv}(Y) > 1$, allora $Y$ varia "più" di $X$

## Skewness (Indice di forma)
Serve a misurare la non simmetria dei dati.
$$ z_i = \frac{x_i - \overline{x}}{s} $$
$$ \textrm{indice di skewness} = \frac1n\sum_{i = 1}^n{z_i^3} $$
![[Pasted image 20251128113200.png]]
_I grafici e le affermaizoni si riferiscono alla variabile aleatoria $X$ sottostante_