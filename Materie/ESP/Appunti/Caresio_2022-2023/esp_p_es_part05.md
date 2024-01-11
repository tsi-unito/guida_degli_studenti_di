---
title: 'Etica, Società e Privacy - Privacy - Parte V - Esercizi'
---
# Etica, Società e Privacy - Privacy - Parte V - Esercizi

## Differenziazione

### Esempio 1 - Meccanismo di Laplace

- Conteggio dei presenti a una lezione (**scalare**):
    - Dataset $D$: numero di presenti a una lezione.
    - Si vuole calcolare $n$ (numero di studenti presenti) in maniera DP usando il meccanismo di Laplace.
    - Si scegli un privacy budget $\varepsilon = 1$.
    - Si deve calcolare la sensibilità globale della funzione (conteggio).
        - Aggiungendo e rimuovendo un record il numero totale di studenti varia solo di $1$.
        - Quindi $GS = 1$.
    - Risultato: $\tilde{n} = n + Lap(1)$, dove $1 = GS_1(q)/\varepsilon$.
- Conteggio dei presenti a una lezione (**vettore**):
    - Dataset $D$: numero di presenti a una lezione.
    - La query $q(D) = (n, n_c)$ restituisce il numero di studenti presenti e il numero di studenti fisicamente in classe.
    - Si vuole calcolare $n$ (numero di studenti presenti) in maniera DP usando il meccanismo di Laplace.
    - Si scegli un privacy budget $\varepsilon = 1$.
    - Sensibilità globale: $GS_1(q) = \max_{D \sim D'} \|q(D)-q(D')\|_1 = \max_{D\sim D'}(|n - n'| + |n_c - n'_c|)$.
        - Caso $1$: lo studente extra è in classe, allora $(|n - n'| + |n_c - n'_c|) = |n - (n+1)| + |n_c - (n_c + 1)| = 2$.
        - Caso $2$: lo studente extra è online, allora $(|n - n'| + |n_c - n'_c|) = |n - (n+1)| + 0 = 1$.
        - $GS_1(q) = 2$.
    - Risultato: $(\tilde{n}, \tilde{n}_c) = (n + Lap(\frac{2}{\varepsilon}), n_c + Lap(\frac{2}{\varepsilon}))$.
- Conteggio dei presenti a una lezione (**vettore con applicazione teorema di composizione**):
    - Dataset $D$: numero di presenti a una lezione.
    - $q_n(D) = n$ e $q_c(D) = n_c$.
    - $GS(q_n) = 1$ (si aggiunge o toglie $1$) e $GS(q_c) = 1$.
    - $\tilde{n} = n + Lap(\frac{1}{\varepsilon})$ e $\tilde{n}_c = n_c + Lap(\frac{1}{\varepsilon})$.
    - $q(D) = (q_n(D), q_c(D))$ che preserva la $2\varepsilon$-DP (teorema di composizione).
        - Peggiora il livello di privacy rispetto al caso precedente.
        - Se si vuole rispettare la $\varepsilon$-DP bisogna cambiare il parametro di privacy $\varepsilon$.
        - Si sa che il livello totale di privacy sarà la somma dei livelli dei due meccanismi.
            - Si può quindi dimezzare il livello di privacy dei due singoli meccanismi, usando $\frac{\varepsilon}{2}$.
    - Risultato: $\tilde{n} = n + Lap(\frac{1}{\varepsilon/2}) = n + Lap(\frac{2}{\varepsilon})$ e $\tilde{n}_c = n_c + Lap(\frac{1}{\varepsilon/2}) = n + Lap(\frac{2}{\varepsilon})$.
        - In linea con quanto visto sopra.
- Conteggio dei presenti a una lezione (**funzione alternativa con teorema di composizione in parallelo**):
    - Si ha $f(D) = (n_c, n_o)$, che si divide in $f_c(D) = n_c$ e $f_o(D) = n_0$.
    - $GS(f_c) = 1$ e $GS(f_o) = 1$.
    - Si assume che i primi $c$ record del dataset siano gli studenti in classe, e i restanti $o$ quelli online.
        - Si è creata una partizione del dataset.
        - $f_c$ e $f_o$ sono applicati **in parallelo a sezioni distinte** del dataset.
    - Per il teorema di composizione in parallelo, non si rispetta la $2\varepsilon$-DP ma la $\varepsilon$-DP.
        - Si è raddoppiato il livello di privacy, o dimezzato il rumore inserito.
    - Non si ha margine di manovra quando si calcola la forma della Laplaciana.
        - Ma si può decidere **dove applicare il rumore**, come formulare il problema, quale funzione considerare.

### Esempio 2 - $(\varepsilon, \delta)$-differential privacy

- Si valuta $q(D) =$ numero di record di $D$.
    - Meccanismo $1$: 
        - Si estrae a caso il numero $x \in (0, 1)$, poi si ritorna:
            - $M(D) = q(D) + Lap(\frac{GS}{\varepsilon})$ se $x \geq \delta$.
            - $M(D) = D$ (intero dataset) se $x < \delta$.
        - Questo meccanismo, nonostante la pericolosità, **rispetta** la $(\varepsilon, \delta)$-DP.
    - Meccanismo $2$: 
        - Si estrae a caso il numero $x \in (0, 1)$, poi si ritorna:
            - $M(D) = q(D) + Lap(\frac{GS}{\varepsilon})$ se $x \geq \delta$.
            - $M(D) = q(D)$ se $x < \delta$.
        - Questo meccanismo **rispetta** la $(\varepsilon, \delta)$-DP.
    - La scelta di $\delta$ deve essere quindi guidata da:
        - $\delta$ deve essere il più piccolo possibile.
            - In caso di fallimento possono manifestarsi conseguenze molto gravi.
        - La maniera in cui il meccanismo funziona è cruciale nella scelta di $\delta$.
            - Se il meccanismo è molto pericoloso, $\delta$ deve essere prossimo allo zero.
        - Consiglio: scegliere $\delta$ inversamente proporzionale almeno al quadrato del numero di record.
