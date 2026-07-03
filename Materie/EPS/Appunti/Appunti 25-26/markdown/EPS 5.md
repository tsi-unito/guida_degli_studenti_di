Lezione del 24 settembre 2025
Appunti di Alessandro Salerno

## Probabilità condizionata
Si vogliono calcolare delle probabilità che tengano conto di informazioni parziali nell'esperimento. 

**Esempio:** si lanciano due dadi, qual è la probabilità che la somma dei valori delle 2 facce che esocno sia 9? Assumendo che il primo lancio ha dato 6? 

### Definizione
Dato $(\Omega, p(\omega), \mathbb{P})$ e $B$ evento. Definiziamo _probabilità condizionata rispetto a $B$_ e indichiamo con $\mathbb{P}(... \ | \ B)$ come la seguente funzione:
$$ \mathbb{P}(... \ | \ B) \: \ p(\Omega) \to \mathbb{R} = \mathbb{P}(A \ | \ B) = \frac{\mathbb{P}(A \cap B)}{\mathbb{P}(B)} $$
Con denominatore non nullo (ossia $B$ si è verificato). Alternativametne:
$$ \mathbb{P}(A \ | \ B) = \mathbb{P}_B(A) = \mathbb{Q}(A) $$
Questa notazione sottolinea particolarmente bene il fatto che assumere $B$ come già verificato equivae a cambiare la funzione probabilistica. 
Da cioè deriva anche che:
$$ \mathbb{P}(A \cap B) = \mathbb{P}_B(A) \cdot \mathbb{P}(B) = \mathbb{P}_A(B) \cdot \mathbb{P}(A) \textrm{ per } |A| \neq 0 $$


