Lezione del 19 novembre 2025
Appunti di Alessandro Salerno

## Test d'ipotesi sulla media
Fissata $\mu_0 \in \mathbb{R}$, si può provare a smentire $h_0 \ : \ \mathbb{E}(X) = \mu_0$ sostenendo $h_1 \ : \ \mathbb{E}(X) = \mu_1$ con $\mu_1$ diverso in qualche modo da $\mu_0$. 

## Statistica test per la media
$$   t = \frac{\overline{X}_n -\mu}{\sqrt{\frac{S^2}{n}}} \simeq \mathbb{t}(n - 1) \textrm{ per } n \textrm{ grande oppure se } X \sim N(\sigma^2, \mu) $$
Sia quindi $T \sim \mathbb{t}(n - 1)$ allora definiamo:
1. Se $h_1 \ : \ \mathbb{E} \geq \mu_0$, allora
$$ p = \mathbb{P}(T \geq t) = 1 - \operatorname{pt}(t, n-1) $$
2. Se $h_1 \ : \ \mathbb{E} \leq \mu_0$, allora
$$  p = \mathbb{P}(T \leq t) = \operatorname{pt}(t, n-1)  $$
3. Altrimenti, per ipotesi bilatera
$$ p = \mathbb{P}(|T| \leq |t|) = ?   $$

L'ipotesi nulla è smentita per $p \leq \alpha$. 