Lezione del 20 ottobre 2025
Appunti di Alessandro Salerno (immagini da appunti del Prof. F. Polito)

![[Pasted image 20251020162232.png]]

## Osservazione
Dalla PMF congiunta si può derivare le PMF marginali che la compongono. Infatti:
$$ P_X(x) = \mathbb{P}(X = x) = \mathbb{P}(\{ \omega \in \Omega \ | \ X(\omega) = x \})$$
$$ = \mathbb{P}(\{X = x\} \cap \Omega) $$
$$ = \mathbb{P}(\{X = x\}\cap \cup_y\{Y = y\}) $$
$$ = \mathbb{P}\left(\cup_y\left(\{X = x\} \cap \{Y = y\}\right)\right) $$
per via della disgiunzione:
$$ = \sum_y{\mathbb{P}(X = x, Y = y)} = \sum_y{\mathbb{P}_{(X, Y)}{(x, y)} } $$
Analogamente per $P_Y$. 

## Indipendenza di variabili aleatorie
Le variabili aleatorie discrete $X,Y$ sono indipendenti se gli eventi $X = x$ e $Y = y$ sono indipendenti $\forall x\in Im(X)\forall y\in Im(Y)$. Quindi se $X,Y$ sono indipendenti, la funzione congiunta è data da:
$$ P_{(X, Y)} = \mathbb{P}(X = x, Y = y) $$
$$ = \mathbb{P}(\{X = x\} \cap \{Y = y\}) $$
$$ = \mathbb{P}(\{X = x\}) \mathbb{P}(\{Y = y\}) $$
$$ = P_X(x)P_Y(y) $$

> [!NOTE]
> In questo caso è quindi possibile ricostruire la congiunta dalle marginali. 

## Proposizione
Se $X,Y$ sono indipendenti, allora:
1. $\mathbb{E}(XY) = \mathbb{E}(X)\mathbb{E}(Y)$
2. $\mathbb{V}ar(X + Y) = \mathbb{V}ar(X) + \mathbb{V}ar(Y)$

Inoltre, la conoscenza di $X$ ed $Y$ separatamente equivale alla conoscenza di $(X, Y)$. 

## Definizione
$X_1, X_2$ sono indipendenti se $\{X_1 = x\}$ e $\{X_2 = y\}$ per ogni $x \in Im(X_1), y \in Im(X_2)$ sono indipendenti.
		


