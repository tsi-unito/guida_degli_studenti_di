Lezione del 7 ottobre 2025
Appunti di Alessandro Salerno

## Variabili aleatorie discrete note
Consideriamo un esperimento probabilistico con esito dicotomico, ossia esistono solo due "etichette" che per convenzione prendono il nome di _successo_ e _fallimento_. 
$$ \Omega = \{s, f\} $$
$$ X \ : \ \Omega \to \mathbb{R} $$
$$ X(s) = 1 $$
$$ X(f) = 0 $$
Ne deriva quindi che in questo tipo di variabili aleatorie l'l'immegine è:
$$ Im(X) = \{0, 1\} $$
Ed è nota come _variabile di Bernoulli_. 
Introduciamo un parametro $p$ (probabilità di successo) con:
$$ P_X(0) = 1 - p $$
$$ P_X(1) = p $$
Le variabili di Bernoulli in notazione si scrivono come:
$$ X \ \sim \ \mathbb{Be}(p) $$
$$ P_X(x_i)=p^{x_i}(1-p)^{1 - x_i} $$
$$ x_i \in Im(X) $$

### Variabili aleatorie binomiali
Esperimento probabilistico dicotomico (detto anche _prova Bernoulliana_) ripetuto $n \in \mathbb{N}$ in modo identico e indipendente. 
$$ X \sim \mathbb{Bi}(n, p) $$
Con $p$ la probabilità di successo.
$$ Im(X) = \{0, 1, ..., n\} $$
$$ P_X(k) = \binom{n}{k}p^k(1 - p)^{n - k} $$
Con $k \in Im(X)$. 

L'immagine di queste variabili aleatorie è compostda da )...N con N numero di prove dicotomiche. 

## Variabili aleatorie geoemtriche
Prove ripetute bernoulliana indipendenti e identiche che cessano al primo successo.
$$ X(\omega) = |\omega| $$
$$ \omega = \{\omega_1, \omega_2, ..., \omega_n\} \subseteq \Omega $$

E diciamo:
$$ X \sim \mathbb{Ge}(p) $$
$$ Im(X) = [1, +\infty) $$
$$ \forall k \in Im(X) \ \ P_X(k) = \mathbb{P}(X = k) = \mathbb{P}(X^{-1}(\{k\})) = (1 -p)^{k - 1}p $$
Con $k \in Im(X)$. 