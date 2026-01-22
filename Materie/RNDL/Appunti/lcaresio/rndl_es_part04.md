---
title: 'Neural networks and Deep learning - Parte IV - Esercizi'
---

# Neural networks and Deep learning - Parte IV - Esercizi

## Recurrent neural networks

### Esempio 1 - Long Short Term Memory

- LSTM forget memory:
    - Let $\sigma = 1$ for arguments $> 0$, $0$ otherwise.
    - Let's $W_f = [1, 1, 1, -10; 1, 1, 2, -10]$ and $b_f = 0$.
    - $[h_{t-1}, x_t] = [1, 1, 0, 1]$ and suppose $x_t = [0, 1]$ codifies ".", the dot character.
    - Then $f_t = [0, 0]$. 
        - Intuitively (and informally) erases everything from $C_{t-1}$.
    - Since weights are learned via BP, gates are not as interpretable.
