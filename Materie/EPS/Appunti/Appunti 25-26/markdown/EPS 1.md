Lezione del 16 settembre 2025
Appunti di Alessandro Salerno

## Probabilità
La probabilità utilizza strumenti scheumorfici per rappresentanza, ma studia fenomeni non facilmente prevedibili.

Immaginiamo di appostarci fuori da un ufficio postale prima dell'apertura e avviare un cronometro di cui campioniamo il valore ogni volta che entra un cliente. I tempi registrati sono casuali nello stesso senso in cui lo sono le facce di un dado o di una moneta, ma non in quanto _discreti_, ma in quanto distinti seppur continui. L'aleatorietà è data dalla parziale assenza di informazioni: se avessimo la possibilità di monitorare le azioni notturne del primo cliente, potremmo prevedere con ragionevole accuratezza l'orario di arrivo.

## Insieme
Un insieme è una collezione di oggetti detti _elementi dell'insieme_. 
Gli insiemi sono solitamente indicati ocn lettere latine maiuscole. Per insiemi n elementi definiti, la notazione prevede l'uso delle parentesi graffe come nell'esempio:
$$
S = \{x_1, x_2, x_3\} $$
È importante notare che gli insiemi hanno ordine non definito, dunque l'ordine in cui gli elementi appaiono nella notazione di cui sopra non è indicativo dell'"ordine" degli elementi nell'insieme. Pertanto, l'insieme in esempio è equivalente all'insieme:
$$ S = \{x_2, x_1, x_3\} $$

Gli insiemi possono essere anche descritti con una notazione condizionale:
$$ S = \{x \ | \ x \text{ ha la proprità } p\} $$

Un insieme $S$ si dice numerabile quando ad ogni elemento è possibile associare un numero naturale. Il termine è spesso usato per indicare insiemi _infiniti_ numerabili. Inversamente, un insieme si dice _non numerabile_ quando questo non è possibile (e.g., la retta reale).

### Operazioni su insiemi
Dati due insiemi $T, S$, si può dire:
$$ T \subset S $$
Ossia $T$ è incluso in $S$, se $T$ è un sottoinsieme di $S$. Ossia, per ogni elemento $t \in T$, $t \in S$. 
Esistono anche rappresentazioni grafiche di questi concetti, particolarmente in riferimento ai Diagrammi di Ven. 

Sia $A$ un insieme, il _complementare di $A$_ è l'insieme $A^C$ definito come:
$$ A^C = \{x \ | \ x \not\in A\} $$
Ciò presuppone l'esistenza di un insieme ambiente $E$ tale che $A \subseteq E$ rispetto a cui viene effettuato il complementare di $A$. Il complemento rispetto ad $E$, quindi, è anche scritto come $E - A$ oppure $E \setminus A$.

Siano $A, B$ due insiemi, è possibile definire un insieme:
$$ A \cup B = \{x \ | \ x \in A \lor x \in B\} $$
$$  A \cap B = \{x \ | \ x \in A \land x \in B\}  $$

Due insiemi si dicono disgiunti se $A \cap B = \emptyset$. 

## Spazio campionario 
Gli spazi campionari sono spesso indicati con lettere greche maiuscole, solitamente $\Omega$. 
Lo spazio campionario $\Omega$ è l'insieme che contiene tutti i possibili esiti di un esperimento probabilistico. 
Un esperimento probabilistico è l'osservazione di un fenomeno con caratteristiche fuori dal nostro controllo, come quelli descritti nella prima parte. 
L'etichetta assegnata agli elementi dell'insieme campionario non è rilevante ai fini del problema. 