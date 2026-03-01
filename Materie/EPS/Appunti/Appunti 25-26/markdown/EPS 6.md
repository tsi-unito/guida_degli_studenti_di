Lezione del 25 settembre 2025
Appunti di Alessandro Salerno

## Regola della moltiplicazione 
Siano $A_1, A_2, A_3, ..., A_N$ eventi. Si intende valutare la probabilità $\mathbb{P}(A_1 \cap A_2 \cap A_3 \cap ... \cap A_N)$ . 
In qanto eventi, tutti gli $A$ intersecati restituiscono altri elementi dell'insieme delle parti. Dal caso base deriva che:
$$ \mathbb{P}((A_1 \cap A_2) \cap A_3) = \mathbb{P}(A_3 \ | \ A_1 \cap A_2)\mathbb{P}(A_2 \cap A_2) $$
$$ = \mathbb{P}(A_3 \ | \ A_1 \cap A_2)\mathbb{P}(A_2 \ | \ A_1)\mathbb{P}(A_1) $$
Generalizzando, quindi, per $N$ eventi:
$$ \mathbb{P}(A_1 \cap A_2 \cap A_3 \cap ... \cap A_N) = \prod_{i = 1}^N{\mathbb{P}\left(A_i \ | \ \cap_{j = 1}^{i -1}{A_j}\right)} $$

## Formula delle probabilità totali 
Siano $A_,1 A_2, A_3, ..., A_N$ eventi disgiunti e la cui unione compone una partizione di $\Omega$. Siano, inoltre $B$ un evento. Si ha:
$$ \mathbb{P}(B) = \sum_{i = 1}^N{\mathbb{P}(B \ | \ A_1)\mathbb{P}(A_i)} $$

## Teorema di Bayes
Sia $(A_i)_{i = 1}^N$ una partizione di $\Omega$ e $B$ un evento. Si ha:
$$ \mathbb{P}(A_i \ | \ B) = \frac{\mathbb{P}(B \ | \ A_i)\mathbb{P}(A_i)}{\sum_{j = 1}^N{\mathbb{P}(B \ | \ A_j)\mathbb{P}(A_j)}} $$


