Di Alessandro Salerno (Gennaio 2025)

## INDICE
- Quick tips
	- [[#Calcolo combinatorio]]
		- [[#Rotazioni]]
		- [[#Anagrammi]]
		- [[#Numero di permutazioni dato il tipo]]
		- [[#Numero di sottoinsimi]]
		- [[#Leggere i binomi]]
		- [[#Disposizioni con ripetizioni, stringhe]]
		- [[#Disposizioni semplici]]
		- [[#Combinazioni con ripetizione]]
	- [[#Permutazioni
		- [[#Periodo dato il tipo]]
		- [[#Parità]]
	- [[#Euclide, Bézout e congruenze]]
		- [[#Euclide]]
		- [[#Risolvere Bézout con metodo classico]]
		- [[#Congruenze]]
		- [[#Congruenze con esponenti pazzi (disperata)]]
	- [[#Insiemistica]]
		- [[#Parti e partizioni]]
- Problemi
	- Funzioni
		- [[#(Luglio 2023) Problema 1]] GRUPPO PRODOTTO CICLICO, KERNEL, IMMAGINE, BIETTIVITÀ
	- Permutazioni
		- [[#(Gennaio 2023) Problema 2]] DIMOSTRAZIONE SOTTOGRUPPI
		- [[#(Giugno 2023) Problema 2]] OMOMORFISMI, SOTTOGRUPPI CICLICI, POTENZE
		- [[#(Settembre 2023) Problema 1]] SOTTOGRUPPI, POTENZE
	- Calcolo combinatorio
		- [[#(Giugno 2023) Problema 1]] **BITMAP**, SOTTOINSIEMI
		- [[#(Luglio 2024) Problema 1]] **RIPETIZIONI ESATTE**, STRINGHE, DISPOSIZIONI
		- [[#(Settembre 2023) Problema 2]] **ADIACENZE**, BINOMI, INSIEMI
		- [[#(Giugno 2024) Problema 1]] ????
		- [[#(Gennaio 2023) Problema 1]] NUMERO DI PERMUTAZIONI DATO IL TIPO
		- [[#(Gennaio 2023) Problema 2]] CHOOSE, CHOOSE PER LE DONNE
	- Elucide - Bézout
		- [[#(Gennaio 2023) Problema 2]] INVERSI

## QUICK TIPS
### Calcolo combinatorio

![[Pasted image 20250113225154.png]]
#### Rotazioni
La usi quando hai $n$ elementi da ordinare, l'ordine conta ma gli inversi sono uguali ai dritti. Tipo, se hai un cerchio o un quadrato (alla matematica non fotte della geometria) e vuoi metterci attorno dei bambini, chiaramente l'ordine inverso è uguale all'oridne iniziale. Quindi, la formula per la rotazioen è data da:
$$ \frac{n!}{n} $$

#### Anagrammi
Se hai una parola di lunghezza $l$, che include lettere ripetute $a_1, a_2, a_n$ ed ognuna di queste è ripetuta $r(a_1), r(a_2), r(a_n)$ volte, allora il numero di anagrammi è dato da:
$$ \frac{l!}{r(a_1)! \cdot r(a_2)! \cdot r(a_n)!} $$
_Ho messo due lettere di base, ma ne basta una._

#### Numero di permutazioni dato il tipo
Sappiamo che il numero di $k$-cicli in $S_n$ è dato da:
$$ \binom{n}{k}(k -1)! $$
Sappiamo, quindi, che dato un tipo $(l_1, l_2, l_n)$ tutti distinti, il numero di permutazioni con quel tipo è dato da:
$$ \binom{n}{l_1}(l_1 -1)! \cdot \binom{n -l_1}{l_2}(l_2 -1)! \cdot \binom{n - l_1 -l_2 \ ...}{l_n}(l_n - 1)! $$
È, però, possibile che un tipo contenga lunghezze ripetute.
Sia $r(l)$ il numero di ripetizioni di una certa lunghezza, allora la formula più generale sarebbe:
$$ \frac{1}{r(l)!}\prod_{i = 1}^{r(l)}{\binom{n - il}{l}(l -1)!} $$
Quando un tipo è composto da più ripetizioni di più lunghezze diverse (esempio 4, 4, 3, 3, 3), si applica più volte questa formula per ogni lunghezza (con il suo numero di ripetizioni) e si moltiplicano i risultati.
$$ \prod_{j = 1}^{n(l_1, l_2, l_n)}{\left(\frac{1}{r(l)!}\prod_{i = 1}^{r(l)}{\binom{n - il}{l}(l -1)!}\right)} $$
### Numero di sottoinsimi
Nota la cardinalità $n = |S|$ si un qualche insieme $S$, sappiamo che il numero dei suoi sottoinsimi è $2^n$. 

Sia $n$ la cardinalità di un insieme, il numero di sottoinsiemi di cardinalità $k$ è dato da:
$$ \binom{n}{k} $$

#### Leggere i binomi
$$ \binom{x}{y} = \frac{x!}{y!(x - y)!} $$

#### Disposizioni con ripetizioni, stringhe
Data una lunghezza $n$ ed un alfabeto con $k$ simboli, il numero totale di stringhe/disposizioni con ripetizioni è dato da $k^n$.

#### Disposizioni semplici
Sia $n$ il numero di elementi da disporre e $k$ il numero di posizioni disponibili.
$$  D_{n,k} = \frac{n!}{(n -k)!}  $$

#### Combinazioni con ripetizione
**NOTA:** le combinazioni semplici sono solo i binomi.

Sia $n$ il numero totale di elementi e $k$ il numero di elementi da sceglere. Definiamo la combinazione con ripetizione come:
$$ \frac{(n + k -1)!}{k!(n -1)!}$$

### Permutazioni

#### Periodo dato il tipo
Dato il tipo di una permutazione, il periodo di quest'ultima è il massimo comune multiplo degli elementi del tipo.
$$ per(\sigma) = mcm(l_1, l_2, l_n) $$

#### Parità
Un $k$-ciclo è pari se si può scrivere in un numero pari di trasposizioni ed è dispari se si può scrivere con un numero dispari di trasposizioni. Si ssa che questo equivale a dire che un $k$-ciclo è pari se $k$ è dispari ed è dispari se $k$ è pari.

Se la permutazione è scritta da più cicli disgiunti, la parità è data dalla somma delle parità. Se una lunghezza $l_i$ è pari, si somma 1, altrimenti si somma 0. La parità aritmetica del risultato è la parità della permutazione. 

### Euclide, Bézout e congruenze
#### Euclide
1. MCD(a, b) e diciamo che X è il numero più grande ed Y il più iccolo (tra a e b)
2. Scriviamo X come Y per qualcosa + un resto
3. Shiftiamo tutto a sinistra di uno (senza riportare il fattore di scala), ossia diciamo `X = Y, Y = R`
4. Ripeti 2 fino a quando `R == 0`
![[Pasted image 20250114103407.png]]

#### Risolvere Bézout con metodo classico
1. Riscrivo l'ultimo resto non nullo come una sottrazione prendendo al contrario gli elementi di Euclide
2. Guardo il resto precedente (che sicuramente ocmpare nella nuova scrittura) e lo sostittuisco con a sua scrittura come sottrazione id Euclide
3. Espando eventuali parentesi
4. Osservo se esistono RESTI ripetuti e raggruppo con quelli
5. Ripetere dal punto 2
![[Pasted image 20250114103039.png]]

#### Congruenze
$$ ax  \equiv b \mod n$$
Con $a, b, n \in Z$ e $n \geq 2$.

1. Controllare se esiston soluzioni, ossia se $MCD(a, n)|b$
2. Dividiamo tutto per $MCD(a, n)$ ottenendo $a', b', n'$
3. Trovare un inverso di $a' = \frac{a}{MCD(a, n)}$ ad intuito o con Bézout (faccio $MCD(a', n')$ usando Euclide e scrivo quello con Bézout ottenendo $a'X +n'Y = 1 = d$ e so che l'inverso è il numero $X$)
4. Riscrivo la congruenza come $Xa'x \equiv Xb' \mod n'$
5. Visto che $Xa' = 1$, possiamo riscrivere come $x \equiv Xb' \mod n'$
6. Portiamo le soluzioni in $\mod n$ dicendo $x \equiv Xb' +zn' \mod n$ con $z \in Z$ 

#### Congruenze con esponenti pazzi (disperata)
se tu hai 
5x^12345 = 3 mod 7 per esempio
       la prima cosa da fare è semplificarti l'esponente
       sai che se x sia coprimo con 7
       posso dire x^phi(7) = 1 mod 7
       ovvero x^6 = 1 mod 7
       quindi x^12345 diventa x^(12345 mod 6) = x^3
e ora sostituisci la semplificazione che hai trovato nell'equazione originale
5 x^3 = 3 mod 7 in maniera quasi analoga alle congruenze lineari

moltiplichi 3 per l'inverso di 5 
per ottenere x^3 = qualcosa mod 7

e poi qua ti devi fare tutta la tabella

(occhio, devi verificare che x sia coprimo con 7, 7 è primo quindi è facile, ma altrimenti devi semplificarti tutto dividendo per l'mcm analogamente alle congruenze linerai)

### Insiemistica

#### Parti e partizioni
Le parti di un insieme sono tutti i suoi sottoinsimi.
Mentre le partizioni sono tutti i suoi sottoinsimi DISGIUNTI e non vuoti. Per formare una partizione di un insieme, è necessario che l'unione dei sottoinsimi disgiunti sia uguale all'insieme vuoto.

## (Gennaio 2023) Problema 1
$$ \sigma = (1 \ 3 \ 5 \ 6)(1 \ 2)(3 \ 6 \ 8) $$
$$ \sigma = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\
2 & 3 & 1 & 4 & 6 & 8 & 7 & 5
\end{bmatrix} $$

Scrviaimo la permutazione come prodotto di cicli disgiutni:
$$ (1 \ 2 \ 3)(4)(5 \ 6 \ 8)(7) = (1 \ 2 \ 3)(5 \ 6 \ 8) $$

Scriviamo $\sigma^{-1}$ ribaltando la matrice di $\sigma$:
$$ \sigma^{-1} = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\
3 & 1 & 2 & 4 & 8 & 5 & 7 & 6
\end{bmatrix} $$
Scriviamola come prodotto di cicli disgiunti:
$$ \sigma^{-1} = (1 \ 3 \ 2)(4)(5 \ 8 \ 6)(7) = (1 \ 3 \ 2)(5 \ 8 \ 6) = (3 \ 2 \ 1)(5 \ 8 \ 6) $$

Scriviamo $\sigma^2$ in forma matriciale applicando due volte $\sigma$:
$$ \sigma^2 = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\
3 & 1 & 2 & 4 & 8 & 5 & 7 & 6
\end{bmatrix} $$
Scriviamo poi $\sigma^2$ come prodotto di cicli disgiunti:
$$ \sigma^2 = (1 \ 3 \ 2)(4)(5 \ 8 \ 6)(7) = (1 \ 3 \ 2)(5 \ 8 \ 6) = (3 \ 2 \ 1)(5 \ 8 \ 6) = \sigma^{-1} $$
Da questo deduciamo che:
$$ \sigma^1 = \sigma $$
$$ \sigma^2 = \sigma^{-1} $$
$$ \sigma^3 = \sigma^2 \circ \sigma^{1} = \sigma^{2 + 1} = \sigma^{-1} \circ \sigma^{1} = \sigma^{-1 +1} = \sigma^0 = id $$
Questo, assieme al minimo comune multiplo dei cicli disgiunti, conferma che il periodo di $\sigma$ è 3.

### Punto 2
Usando la formula vista a lezione, sappiamo che il numero di $k$-cicli in $S_n$ (che chiameremo $m(k, n)$) è descritto da:
$$ m(k, n) = \binom{n}{k}(k - 1)! $$
In questo caso, il tipo di $\sigma$ è $(3, 3)$, quindi $\sigma$ si può espriemre come prodotto di due 3-cicli DISGIUNTI. Il numero di 3-cicli in $S_8$ è quindi descritto da:
$$ m(3, 8) = \binom{8}{3} \cdot 2 $$
Non è possibile, però, applicare questa formula nuovamente per trovare il nuovamente il numero di 3-cicli in $S_8$ perché altrimenti si otterrebbero sovrapposizioni (in questo caso i due sottoinsiemi sarebbero uguali). Per ovviare a questo, troviamo il numero di $k$-cicli in $S_{n - k}$ ossia il numero di 3 cicli in $S_5$. 
$$ m(3, 5) = \binom{5}{3} \cdot 2 $$
Il totale, quindi è:
$$ 4 \cdot \binom{8}{3} \cdot \binom{5}{3} $$
È possibile, però, che due prodotti distinti di cicli disgiunti con al medesima lunghezza abbiano lo stesso risultato, quindi occorre dividere per due, ottenendo:
$$  2 \cdot \binom{8}{3} \cdot \binom{5}{3}  $$

### Punto 3
L'insieme $H$ non è un sottogruppo perhcé non contiene l'inverso di $\sigma$. Per avere un sottogruppo è necessario che:
- L'elemento neturo del gruppo sia contenuto anche nel sottogrupo
- Il sottogruppo contenga tutti gli inversi dei suoi elementi
- Il sottogruppo sia chiuso rispetto all'operazione del gruppo

Sia $G = \{id, \sigma, \sigma^{-1}\}$ , è facile affermare che $G$ è un sottogruppo si $S_8$ ed anche che è il più piccolo sottogruppo di $S_8$ che contiene $H$. Questo perché $G$ ha sia l'elemento neutro (l'identità), che $\sigma$, che l'inverso di $\sigma$ ossia $\sigma^{-1}$. È noto che l'inverso dell'identità sia l'identità stessa, che la composizione $\sigma \circ \sigma^{-1}$ sia l'identità e che la composizione di qualsiasi permutazione con l'identità sia la permutazione stessa, quindi è dimostrato che $G$ sia un sottogruppo. 
## (Gennaio 2023) Problema 2

### Punto 1
I possibili insiemi di 50 nuovi dipendenti sono descritti da:
$$ N = \binom{150}{50} $$

### Punto 2
Il numero di modi con cui si potrebbero scegelre 4 dipendenti a caso come rappresentanti è descritto da:
$$ \binom{50}{4} $$
Se vogliamo, invece, garantire la presenza di una donna, basta escludere tutti i modi di sceglere solo 4 maschi, quindi è pari a:
$$ \binom{50}{4} - \binom{23}{4} $$

### Punto 3
Ci sono 50 laureati e 57 lauree, quindi 7 persone hanno 2 leauree.

## (Giugno 2023) Problema 1
### Punto 1
$T$ è un sottoinsieme di $S$. I sottoinsiemi di $S$ contenenti $T$ INTERAMENTE sono $2^{10 - 4} = 2^6$ mentre il numero di sottoinsimi totale di $S$ è $2^{10}$, quindi ci sono $2^{10} - 2^6$ sottoinsimi di $S$ che non contengono $T$.

## (Giugno 2023) Problema 2

$$ \sigma = (4 \ 3 \ 5)(6 \ 1 \ 3)(1 \ 2 \ 5 \ 7 \ 9)(8 \ 4) $$
$$ \sigma = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 \\
2 & 4 & 6 & 8 & 7 & 1 & 9 & 3 & 5
\end{bmatrix} $$
$$ \sigma = (1 \ 2 \ 4 \ 8 \ 3 \ 6)(5 \ 7 \ 9) $$
La permutazione ha tipo (6, 3).
Il periodo di $\sigma$ è dato dall'mcm tra i numeri del tipo, quindi è 6. 
La parità della permutazione è la parità della somma dei numeri del tipo, quindi è dispari? 

### Punto 2
Esiste un intero $k \geq 0$ tale che $\sigma^k(1) = 8$, infatti con $k = 3$ si ha $\sigma^3(1) = 8$. 
Non esiste, invece, un intero $h \geq 0$ $\sigma^h(1) = 7$ perché $7$ non è incluso nello stesso cico in cui è incluso 7. 

### Punto 3
$\left<\sigma\right>$ è un gruppo rispetto alla composizione. $Z_{15}$ è un gruppo rispetto alla somma. La funzione ha gruppi sia come dominio che come codominio, quindi si può procedere.

Per essere un omomorfismo:
$$ f(a \circ b) = f(a) + f(b) $$
$$ a, b \in \left<\sigma\right> $$
Ma visto che $\left<\sigma\right>$ è composto solo da potenze, di $\sigma$, possiamo riscrivere come:
$$ f(\sigma^a \circ \sigma^b) = f(\sigma^a) + f(\sigma^b) $$
$$ f(\sigma^{a + b}) = f(\sigma^a) +f(\sigma^b) $$
Considerato che $f(\sigma^t) = 5t$, dobbiamo verificare che:
$$ f(\sigma^{a + b}) = (a + b)5 $$
Quindi procediamo:
$$ f(\sigma^{a + b}) = (a)5 +b(5) $$
$$ f(\sigma^{a + b}) = (a + b)5 $$
Dunque $f$ è un omomorfismo. 
Dimostriamo ora la buona definizione:
$$ \pi = \sigma^x $$
$$ \tau = \sigma^y $$
Con:
$$ y \equiv x \mod 6 $$
Ossia:
$$ y = x + 6n $$
$$ n \in Z $$
Allora:
$$ f(\pi) = f(\tau) $$

Riscriviamo come:
$$ f(\sigma^x) = f(\sigma^{x + 6n}) $$
$$ 5x = (x +6n)5 $$
$$ 5x =5x +30n $$
Che, trattandosi di congruenze tra classi di resto, sarebbe meglio scritto come:
$$ \overline{5}x \equiv \overline{5}x +30n \mod 15 $$
Qualsiasi sia $n$, $30n$ sarà un multiplo di $30$, ossia anche un multiplo di $15$ e, pertanto, la congruenza varrà. Dunque, la funzione è ben definita.



## (Luglio 2023) Problema 1

$$ f \ : \ Z_{30} \to Z_6 \times Z_{15} $$
$$ [a]_{30} \to ([a]_6, [a]_{15}) $$

### Punto 1
Dimostrare che la funzione sia ben definita equivale a chiedersi che:
$$ f(a) = f(a + 30n) = ([a]_6, [a]_{15}) $$
$$ n \in Z $$
$$ a \in Z_{30} $$

$$ f(a + 30n) = ([a + 30n]_6, [a + 30n]_15) $$
Sommare un multiplo di 30 ad $a$ equivale a prendere un altro rappresentante della medesima classe $a$ visto che $a \in Z_{30}$. Questo accade similarmente per $Z_6$ e $Z_{15}$ visto che $30n = (5 \cdot 6)n = (2 \cdot 15)n$, quindi i rappresentanti rimangono gli stessi e la funzione è ben definita. 

Per dimostrare che si un omomorfismo, basta chiedersi se:
$$ f(a + b) = f(a) + f(b) $$
$$ a, b \in Z_{30} $$
Ossia:
$$ f(a + b) = ([a]_6, [a]_{15}) \star ([b]_6, [b]_{15}) $$
L'operazione $\star$ in questo caso somma il primo elemento delle due tuple e il seocndo elemento delle due tuple visto che sia $Z_6$ che $Z_{15}$ sono gruppi rispetto alla somma.

$$ f(a + b) = ([a]_6 + [b]_{6}, [a]_{15} + [b]_{15}) $$
$$ = (]a + b]_6, [a + b]_{15}) $$

Dunque $f$ è un omomorfismo.

### Punto 2
Calcoliamo $\ker f$. Sappiamo che l'elemento neutro dell'immagine è:
$$ (0, 0) $$
Ossia:
$$ ([0]_6, [0]_{15}) $$
Visto che 15 on è un multiplo di 6, non è possibile trovare un $n \neq [0]_{30} \in Z_{30}$ tale che $f(n) = ([0]_6, [0]_{15})$ . Quindi $\ker f = \{[0]_{30}\}$.  Di conseguenza la funzione è iniettiva.

Però la funzione non è suriettiva perché è inietiva ma la cardinalità del dominio è inferiore a quella del codominio.

### Punto 3
Il gruppo prodotto $Z_6 \times Z_{15}$ non è ciclico perché 6 e 15 hanno MCD diverso da 1. Invece il sottogruppo dato dall'immagine della funzione lo è ed ha generatore $([1]_6, [1]_{15})$.

## (Settembre 2023) Problema 1


$$ \sigma = (1 \ 3)(2 \ 6)(3 \ 5 \ 8)(6 \ 2) $$
$$ \sigma = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\
3 & 2 & 5 & 4 & 8 & 6 & 7 & 1
\end{bmatrix} $$
$$ \sigma = (1 \ 3 \ 5 \ 8 )  $$
La permutazione $\sigma$ è dispari perché la sua lunghezza è pari. Il suo tipo è (4) e di ocnseguenza il suo periodo è 4.
$$ \sigma^{2} = (1 \ 3 \ 5 \ 8 )(1 \ 3 \ 5 \ 8 ) $$
$$ \sigma^2 = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\
5 & 2 & 8 & 4 & 1 & 6 & 7 & 3
\end{bmatrix} $$
$$ \sigma^2 = (1 \ 5)(3 \ 8) $$
Quindi $\sigma^2$ ha tipo (2, 2), dunqueh a periodo 2 ed è pari.

### PUnto 2
Non esiste potenza di $\sigma$ per cui $\sigma^h(1) = 6$ visto che, per quante composizioni si possano fare di $\sigma$ con sé stessa,  non si otterrà mai un ciclo $(1 \ 6 \ ...)$. 

### Punto 3
Il sottogruppo generato da $\sigma$ è composto da potenze (composizioni ripetute) di $\sigma$. Quindi, bisogna semplicemente controllare che esistano potenze di $\sigma$ tra le opzioni fornite.

Escludiamo $\tau_1$ perché necessita dell'identità per tuti i numeri tranne 3, 5, 8. Ma, per esempio, l'unica potenza (intesa come rappresentante dei suoi multipli) di $\sigma$ per cui si ha 1 identità è $\sigma^4(1) = 1$, ma $\sigma^4(8) = 8 \neq 3$.

Escludiamo $\tau_2$ perché qualsiasi potenza di $\sigma$ ($\sigma^k$) ha $\sigma^k(2) = 2$ e $\sigma^k(6) = 6$. 

$\tau_3$ Invece combacia perfettametne con $\sigma^3$, dunque è incluso nel gruppo generato da $\sigma$. 

## (Settembre 2023) Problema 2


### Punto 1
La fila di pioli ha 8 spazi liberi e se ne vogliono occupare 3, quindi è sufficiente sceglere 3 elementi da un insieme di cardihnalità 8:
$$ \binom{8}{3} = \frac{8!}{3!(8 - 3)!} = \frac{8!}{3! \cdot 5!} = 56 $$

### Punto 2
Considerando la presenza di 8 pioli, possiamo rappresentare i posti occupati con una $X$ e quelli liberi con una $Y$:
$$ Y \ Y \ Y \ Y \ Y \ Y \ Y \ Y $$
I modi per avere due ombrelloni adiacenti su 8 pioli sono del tipo:
$$  X \ X \ X \ Y \ Y \ Y \ Y \ Y  $$
$$   Y \ X \ X \ X \ Y \ Y \ Y \ Y   $$
...
Ne deriva che i modi per posizionare due ombrelloni adiacenti sono 6 e quindi iil numero di modi per posizionare 3 ombrelloni senza avere adiacenze a due a due è 56 - 6 = 50.
### Punto 3
Sia $B$ l'insieme dei bambini, sappiamo che $|B| = 57$.
Sappiamo che esistono due sottoinsiemi di $B$, ossia $S$  (bambino con secchiello) e $P$ (bambini con paletta). Sappiamo che $B = S \cup P$. Sappiamo che $|S \cap P| = 10$ e $|P - S| = 22$. È quindi noto che $|P| = |P \cup (S \cap P)| = |B| - |S| = 22 + 10 = 32$. Ne deriva che $|S| = |B| -|P| = 57 - 32 = 25$.   

## (Gennaio 2024) Problema 1
### Domanda (a)
$\sigma$ si scrive in forma matriciale come:
$$ \sigma =  \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 \\
2 & 5 & 4 & 3 & 7 & 6 & 9 & 8 & 1 & 10
\end{bmatrix} $$
Con questa matrice possiamo identificare la scrittura di $\sigma$ come prodotto di cicli disgiunti:
$$ \sigma = (1 \ 2 \ 5 \ 7 \ 9)(3 \ 4) $$

_I cicli identità come (8) non son stati riportati_

Il tipo di $\sigma$ è $(5, 2)$ ed il suo periodo è $mcm(5, 2)$ quindi $per(\sigma) = 10$. 

### Domanda (b)
Sappiamo che il periodo della permutazione è 10, quindi $\sigma^{5272} = \sigma^{5272 \mod 10} = \sigma^2$. 
$$ \sigma^2 = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 \\
5 & 7 & 3 & 4 & 9 & 6 & 1 & 8 & 2 & 10
\end{bmatrix} $$
$\pi$, quindi, deve dunque essere:
$$ \pi = \begin{bmatrix}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 \\
9 & 7 & 3 & 4 & 1 & 6 & 2 & 8 & 5 & 10
\end{bmatrix} $$

### Domanda (c)
Per essere un sottogruppo di $S_{10}$, è necessario che:
1. L'elemento neutro di $S_{10}$ sia anche in $G$
2. $G$ contenga gli inversi di tutti i suoi elementi
3. $G$ sia chiuso rispetto all'operazione di $S_{10}$ (la composizione)

La dimostrazione del punto 1 è immediata: l'elemento neutro di $S_{10}$ è la permutazione identità ed è noto che:
$$ id \circ \sigma = \sigma \circ id = \sigma $$
Quindi $id \in G$. 

Supponiamo ora che $\tau \in G$. Allora:
$$ \tau \circ \sigma = \sigma \circ \tau $$
Moltiplichiamo per $\tau^{-1}$ a sinistra:
$$ \tau^{-1} \circ \tau \circ \sigma = \tau^{-1} \circ \sigma \circ \tau $$
Visto che $\tau^{-1} \circ \tau = \tau^0 = id$, sappiamo che:
$$ \sigma = \tau^{-1} \circ \sigma \circ \tau $$
Ora moltiplichiamo per $\tau^{-1}$ a destra:
$$ \sigma \circ \tau^{-1} = \tau^{-1} \circ \sigma \circ \tau \circ \tau^{-1} $$
Semplifichiamo:
$$ \sigma \circ \tau^{-1} = \tau^{-1} \circ \sigma $$
Quindi $\tau^{-1} \in G$, ossia l'inverso di ogni permutazione $\tau \in G$ è esso stesso in $G$, ciò dimostra anche il punto 2.

Supponiamo ora che $\tau, \pi \in G$. Bisogna dimostrare che:
$$ \tau \circ \pi \in G $$
Ossia che:
$$ (\tau \circ \pi) \circ \sigma = \sigma \circ (\tau \circ \pi) $$
Ma questo è dimostrato automaticamente dalla associatività della composizione.

Dunque, $G$ è un sottogruppo di $S_{10}$.
Tre elementi distinti di questo sottogruppo sono:
- L'identità $id$
- La permutazione $(3 \ 4)$
- L'inverso della precedente, la permutazione $(4 \ 3)$

## (Gennaio 2024) Problema 2
### Domanda (a)
$$ MCD(1147, 1000) $$
$$ MCD(1000, 147) $$
$$ MCD(147, 118) $$
$$ MCD(118, 29) $$
$$ MCD(29, 2) $$
$$ MCD(14, 1) $$
$$ = 1 $$


### Domanda (b)
$$ [1146]_5 = [1]_5 $$
$$ [1000]_{1147}^{-1} = [148]_{1147} $$
$$ [5^{10} - 7]_5 = [3]_5 $$


## (Giugno 2024) Problema 1

### Punto 1
Il numero di ordinamenti di ogni gruppo deve essere moltiplicato agli altri piuttosto che sommato.
Inoltre, sappiamo che in ogni fila sono presenti due gruppi interi. Un gruppo è formato da $n$ membri tutti distinti ed i suoi ordinamenti non ammettono ripetizioni (un membro del coro non pùo essere contemporaneamente in due posti diversi). Sappiamo anche che tutti i membri di un gruppo sono presenti al coro, quindi usiamo le permutaizoni semplici congiunte e diciamo che il numero totale di disposizioni è descritto da:
$$ 12! \cdot 10! \cdot 6! \cdot 8! $$

### Punto 2
Per ogni "sottogruppo" di membri del coro si scelgono il numero di esecutori richiesto e per ottenere il numero totale si combinano questi usando la moltiplicazione.
$$ \binom{12}{2} \cdot \binom{10}{2} \cdot \binom{6}{3} \cdot \binom{8}{2} $$
## (Luglio 2024) Problema 1
$X$ contiene i numeri da 0 a 9 (inclusi)
$Y$ contiene le lettere maiuscole da $A$ ad $F$ (incluse).

### Punto 1
L'alfabeto $Z$ contiene tutti i simboli dell'insisme $X$ e dell'insieme $Y$.Sappiamo che $X \cap Y = \emptyset$,  pertanto sappiamo che $|Z| = |X| + |Y| = 10 +6 = 16$. Consideriamo 16 come il numero  possibili possibile di scelte per il simbolo da mettere in una certa posizione della stringa, possiamo quindi affermare che assumendo 6 come lunghezza, esistono $16^6 = 16.777.216$ strnghe.

Le stringhe che contengono solo simboli distinti sono ottenute con uan disposizione (tutti i modi di disporre 16 simboli distinti su 6 posizioni):
$$ D_{16,6} = \frac{16!}{(16 -6)!} = \frac{16!}{10!} 
$$

Ne deriva che il numero di stringhe che contengono almeno un carattere ripetuto è la differenza tra i due risultati:
$$ 10^6 -\frac{16!}{10!} $$

### Punto 2
Essendo $Z$ formato dall'unione di $X$ ed $Y$, sappiamo che il numero di stringhe in $Z$ che contengono almeno un simbolo di $X$ è dato dal numero totale di stringhe in $Z$ - il numero di stringhe contenenti solo simboli di $Y$ (ossia il numero totale di stringhe in $Y$). $|Y| = 6$, quindi il numero di stringhe di lunghezza 6 in $Y$ è $6^6$ ed il numero di stringhe in $Z$ che contengono almeno un simbolo di $X$ è $16^6 -6^6$. 

Possiamo ottenere il numero di stringhe senza elementi ripetuti in $Z$ che contengono almeno un elemento di $X$ sottraendo al risultato del punto precedente il numero di stringhe senza ripetizioni in $Y$:
$$ D_{6,6} = 6! $$
Quindi:
$$ D_{P16,6} -D_{6,6} = \frac{16!}{10!} -6! $$

### Punto 3
Una stringa di 6 simboli ha anche 6 posizioni. Se si vuole ottenere una stringa con due ripetizioni, è necessario sceglere dove mettere i due elementi ripetuti, ossia $\binom{6}{2}$ e moltiplicare questo valore per il numero di elementi che possono stare in una data posizione (16). Ora è necessario assegnare i valori alle 4 posizioni rimanenti, sapendo che non possiamo ripetere ulteriormente il valore scelto in precedenza, deduciamo che rimangono 15 valori e 4 spazi su cui NON voglia vogliamo ripetizioni. Quindi il totale è dato combinando con la moltiplicazione questi tre risultati:
$$ 16\binom{6}{2}D_{15, 4} = 16\binom{6}{2}\frac{15!}{11!} $$

Se vogliamo una ripetizione di 2 simboli di $X$ e 2 simboli di $Y$ basta sceglere nuovamente una posizione per i primi $\binom{6}{2}$ ed una posizione per i secondi $\binom{4}{2}$, moltiplicare per le rispettiva cardinalità e sceglere tra i 14 simboli rimanenti per le 2 posizioni vacanti.
$$ 10\binom{6}{2}6\binom{6}{2}D_{14,2} = 10\binom{6}{2}6\binom{4}{2}\frac{14!}{12!} $$
