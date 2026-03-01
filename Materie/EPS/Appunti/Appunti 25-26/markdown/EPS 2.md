Lezione del 18 settembre 2025
Appunti di Alessandro Salerno

## Operazioni estese sugli insiemi
Sia $A_1, A_2, A_3, ...$ una collezione di insiemi potenzialmente infinita, è possibile definire:
$$ \cup_{i = 1}^{+\infty}{A_i} $$
Ossia l'insieme definito come:
$$  \cup_{i = 1}^{+\infty}{A_i}  = \{x \ | \ \exists x \ : \ x \in A_i\} $$

È possibile definire lo stesso per l'intersezione: 
$$  \cap_{i = 1}^{+\infty}{A_i} = \{x \ | \ x \in A_i \forall i\} $$

## Partizioni di insiemi 
Sia $S$ un insieme, si dice _partizione di $S$_, una collezione di insiemi $(A_i)_1^{+\infty}$ un oggetto con le seguenti proprietà: 
1. Gli insiemi di questa collezione sono disgiunti due-a-due, ossia $\forall i \neq j \ \ S_i, S_j \ \ S_i \cap S_j = \emptyset$ 
2. L'unione di tutti gli insiemi nella collezione è l'insieme $S$ stesso

### Nota
In questi appunti è usata la notazione con unioni ed intersezioni ad infinito, anche per numeri finiti di insiemi. Questo è possibile perché per una quantità finita $N$, tutti gli insiemi $A_{N + 1}$ fino ad infinito sono _vuoti_ e pertanto, non contribuiscono all'operazione. 

## Leggi di De Morgen 
Presi comunque due insiemi $A, B$, è noto che:
$$ \left(A \cup B\right)^C = A^C \cap B^C $$
$$  \left(A \cap B\right)^C = A^C \cup B^C  $$
È anche possibile applicare queste leggi all'infinito:
Sia $(A_i)_1^{+\infty}$  una collezione di insiemi:
$$  \cap_{i = 0}^{+\infty}{(A_i)}^C =  \cup_{i = 0}^{+\infty}{\left(A_i^C\right)} $$
$$   \cup_{i = 0}^{+\infty}{(A_i)}^C =  \cap_{i = 0}^{+\infty}{\left(A_i^C\right)}  $$

## Modello probabilistico 
Un modello probabilistico è un oggetto matematico che fornisce la rappresentazione astratta dell'esperimento probabilistico. Esso è composto da:
1. Uno spazio campionario $\Omega$ ossia l'insieme che contiene i possibili esiti dell'esperimento probabilistico 
2. Una legge di probabilità $\mathbb{P}$, ossia una funzione $\mathbb{P} \ : \ p(\Omega) \to \mathbb{R}$ ossia una funzione dall'insieme di tutti i possibili sottoinsiemi di $\Omega$ ai numeri reali

**Esempio IMPORTANTE:** la probabilità che esca la faccia 3 di un dado lanciato, è data da $\mathbb{P}(\{3\})$ dove $\{3\}$ è il sottoinsieme di $\Omega$ che contiene solo la faccia 3 ed il valore restituito è un numero reale compreso tra 0 ed 1. Il motivo per cui si usa $\{3\}$ anziché $3$ è che _ci si può anche hciedere quale sia la probabilità che esca la faccia 1 OPPURE la 2_, ossia $\mathbb{P}(\{1, 2\})$. Chiaramente tutto questo assume che l'insieme passato faccia parte del dominio, ossia dell'insieme delle parti.

$\mathbb{P}$ per essere uan funzione di probabilità, deve avere le seguenti proprietà: 
1. $\forall A \in p(\Omega) \ \ \mathbb{P}(A) \geq 0$
2. $\mathbb{P}(\Omega) = 1$
3. Se $A, B \in p(\Omega)$ e $A \cap B = \emptyset$, allora $\mathbb{P}(A \cup B) = \mathbb{P}(A) +\mathbb{P}(B)$, ossia $\mathbb{P}$ è additiva
4. Se $(A_i)_1^{+\infty}$ è uan collezione di insiemi potenzialmente infinita di $p(\Omega)$ **disgiunti**, allora $\mathbb{P}\left(\cup_{i = 0}^{\infty}{A_i}\right) = \sum_{i = 1}^{+\infty}{\mathbb{P}(A_i)}$ ossia $\mathbb{P}$ è _additiva enumerabile_ (proprietà: $\sigma$-additività)
