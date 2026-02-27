Lezione del 18 novembre 2025
Appunti di Alessandro Salerno

## Test d'ipotesi
 Con un test d'ipotesi si vuole verificare se un'affermazione fatta sui parametri sia vera o falsa.
È possibile fare test d'ipotesi su proporzioni, medie e differenza tra medie. 
Nei test d'ipotesi si ragiona su due ipotesi:
- Ipotesi nulla $h_0$ (Normalmente contiene qualcosa che si vuole smentire con i dati)
- Ipotesi alternativa $h_1$
Le due ipotesi non possono intersecarsi, quindi non possono essere vere contemporaneamente. 
In casi reali, $h_0$ normalmente contiene _lo stato dell'arte_ ossia qualcosa che è assunto essere vero che si vuole dimostrare falso (es: un nuovo farmaco si assume non funzioni e si vuole dimostrare che invece funziona). 
Il test d'ipotesi ci da due tipi di risposte:
- Ragionevole rifiutare $h_0$
- Non rifiuto $h_0$ (potrebbe essere vera come potrebbe non esserlo)
Le risposte vengono date ad un certo _livello di significatiità_, come nei test di confidenza si ha un _livello di confidenza_. 

## Relazione tra significatività  e ipotesi
La significatività è l'errore massimo nel caso in cui il risultato non corrispondesse alla realt. Gli errori possibili sono:
- Prima specie: non rifiuto $h_0$ quando è falsa in verità
- Seconda specie: rifiuto $h_0$ quando è vera

> [!IMPORTANT]
> È più grave commettere un errore di prima specie, in quanto questo no altera lo stato dell'arte.

In notazione:
- $\alpha$ è la probabilità di comettere un errore di prima specie
- $\beta$ è la probabilità di commettere un errore di seconda specie
È desiderabile minimizzare $\alpha$. 

## Statistica test per le proporzioni
Sia $\hat{P}_n$ la proporzione campionaria e $P$ la proporzione teorica, allora:
$$  t = \frac{\hat{P}_n -P}{\sqrt{\frac{P(1 - P)}{n}}} \simeq N(0, 1) \textrm{ per } n \textrm{ grande} $$
Normalmente $h_0$ fornisce in qualche modo il valore di $P$ (assunto vero). Se $h_0$ fosse vera, ciaspetteremmo $t \simeq 0$. Presa $X \sim N(0, 1)$ calcoliamo quindi:
$$ p = \mathbb{P}(X \geq t)  = 1 - \operatorname{pnorm}(t) $$
L'ipotesi $h_0$ è rifiutata per $p \leq \alpha$. 

## Ruolo di $h_1$
$h_0$ pone sempre $P$ ad un valore. In base a questo:
- Se $h_1$ ipotizza $P$ maggiore, allora ci si "insospettisce" se si ottiene una statistica test più grande (valore improbabile sotto $h_0$, ma probabile sotto $h_1$). Inversamente, se si ottiene un valore inferiore, allora questo NON confuta $h_0$
- Se $h_1$ ipotizza $P$ minore, esattamente il contrario
- Ipotesi bilatera: $h_1$ ipotizza che $P$ sia diverso, allora ci si "insospettice"in entrambi i casi

