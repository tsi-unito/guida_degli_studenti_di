Lezione del 29 settembre 2025
Appunti di Alessandro Salerno

## Indipendenza tra eventi
Sia $(\Omega, p(\Omega), \mathbb{P})$, Siano $A, B$ eventi, si dicono indipendenti se vale l'identità:
$$ \mathbb{P}(A \ | \ B) = \mathbb{P}(A) $$
Alternativamente, osservando la definizione di probabilità condizionata, definita anche come:
$$ \mathbb{P}(A \cap B) = \mathbb{P}(A)\mathbb{P}(B) $$

### osservazione 1
Questo è equivalente a dire
$$  \mathbb{P}(B \ | \ A) = \mathbb{P}(B)  $$

### Osservazione 2
Siano $A, B$ eventi incompatibili, ossia tlai che $A \cap B = \emptyset$ e $\mathbb{P}(A), \mathbb{P}(B) > 0$, dalle definizioni precedenti segue che $A, B$ **NON** sono indipendenti poiché:
$$ A \cap B = \emptyset \to \mathbb{P}(A \cap B) = 0 \neq \mathbb{P}(A)\mathbb{P}(B) > 0 $$

Nonostante l'apparente contraddizione, è possibile comprendere la dipendenza tra eventi disgiunti ocn il seguente esempio: se $A$ accade, è noto che $A^C$ non è accaduto e viceversa. Cioè vuol dire che $A$ e $A^C$ sono dipendenti.

### Proposizione
Siano $A, B$ indipendenti. Allora, lo sono anche:
- $A$ e $B^C$ 
- $A^C$ e $B$
- $A^C$ e $B^C$

## Costruzione di funzioni di probabilità su prodotti cartesiani
Siano $A, B$ insiemi, il loro prodotto cartesiano è definito come:
$$ A \times B = \{(a, b) \ \forall a \in A \ \  \forall b \in B\} $$
Siano $\mathbb{P}^1, \mathbb{P}^2, ..., \mathbb{P}^N$ funzioni di probabilità non uniformi e siano $i, j, ...$ esiti di ogni esperimento. Allora vale:
$$ \mathbb{P}(\{i, j, ...\}) = \mathbb{P}^1({i})\mathbb{P}(\{j\})\mathbb{P}^N(\{...\}) $$
La nuova funzione deve essere coerente con quelle che la compongono. Ossia la probabilità di un evento data la sua $\mathbb{P}^N$ e del sottoevento corrispondente in $\mathbb{P}$ devono coincidere. Le $\mathbb{P}^N$ si chiamano dunque _leggi marginali_ mentre $\mathbb{P}$ si dice _legge congiunta_. Avendo leggi marginali uniformi si ottiene una misura prodotto uniforme. 


