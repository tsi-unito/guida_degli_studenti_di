
| Gradi | Radianti        | $\sin$                | $\cos$                | $\tan$                | $\cot$                |
| ----- | --------------- | --------------------- | --------------------- | --------------------- | --------------------- |
| 0     | $0$             | $0$                   | $1$                   | $0$                   | -                     |
| 30    | $\frac\pi6$     | $\frac12$             | $\frac{\sqrt{3}}{2}$  | $\frac{1}{\sqrt{3}}$  | $\sqrt{3}$            |
| 45    | $\frac\pi4$     | $\frac{\sqrt{2}}{2}$  | $\frac{\sqrt{2}}{2}$  | $1$                   | $1$                   |
| 60    | $\frac\pi3$     | $\frac{\sqrt{3}}{2}$  | $\frac12$             | $\sqrt{3}$            | $\frac{1}{\sqrt{3}}$  |
| 90    | $\frac\pi2$     | $1$                   | $0$                   | -                     | $0$                   |
| 120   | $\frac23\pi$    | $\frac{\sqrt{3}}{2}$  | $-\frac12$            | $-\sqrt{3}$           | $-\frac{1}{\sqrt{3}}$ |
| 135   | $\frac34\pi$    | $\frac{\sqrt{2}}{2}$  | $-\frac{\sqrt{2}}{2}$ | $-1$                  | $-1$                  |
| 150   | $\frac56\pi$    | $\frac12$             | $-\frac{\sqrt{3}}{2}$ | $-\frac{1}{\sqrt{3}}$ | $-\sqrt{3}$           |
| 180   | $\pi$           | 0                     | $-1$                  | $0$                   | -                     |
| 210   | $\frac76\pi$    | $-\frac12$            | $-\frac{\sqrt{3}}{2}$ | $\frac{1}{\sqrt{3}}$  | $\sqrt{3}$            |
| 225   | $\frac54\pi$    | $-\frac{\sqrt{2}}{2}$ | $-\frac{\sqrt{2}}{2}$ | $1$                   | $1$                   |
| 240   | $\frac43\pi$    | $-\frac{\sqrt{3}}{2}$ | $-\frac12$            | $\sqrt{3}$            | $\frac{1}{\sqrt{3}}$  |
| 270   | $\frac32\pi$    | $-1$                  | $0$                   | -                     | $0$                   |
| 300   | $\frac53\pi$    | $-\frac{\sqrt{3}}{2}$ | $\frac12$             | $-\sqrt{3}$           | $-\frac{1}{\sqrt{3}}$ |
| 315   | $\frac74\pi$    | $-\frac{\sqrt{2}}{2}$ | $\frac{\sqrt{2}}{2}$  | $-1$                  | $-1$                  |
| 330   | $\frac{11}6\pi$ | $-\frac12$            | $\frac{\sqrt{3}}{2}$  | $-\frac{1}{\sqrt{3}}$ | $-\sqrt{3}$           |
| 360   | $2\pi$          | $0$                   | $1$                   | $0$                   | -                     |
Molt: $(a +ib)(c +id) = (ac -bd) +(ad +bc)i$       Cnj: $\overline{z} = a -ib$     Molt-Cnj: $z \cdot \overline{z} = ||z||^2$ 
$i^0 = 1$    $i^1 = i$   $i^2 = -1$   $i^3 = -i$   $i^4 = i^0 = 1$   Prop: $\overline{z + w} = \overline{z} +\overline{w}$    $\overline{z \cdot w} = \overline{z} \cdot \overline{w}$ 
$z = re^{i\theta} = r(\cos{\theta} + \sin{\theta})$   $r = ||z||$   $a = x = r\cos{\theta}$   $b = y = r\sin{\theta}$   $\theta = \arctan{\frac{x}{y}}$  Prendere $\theta$ in base ai segni di $x = a, y = b$ sul piano cartesiano. Radici:  $z^n = z_0$  $r = \sqrt[n]{r_0}$   $\theta_k = \frac{\theta_0}{n} +\frac{2k\pi}{n}$ con $k \in [0, n)$. Quindi: $\sqrt[n]{z_0}_k = re^{i\theta_k}$.  Noto che: $\frac{x}{i} = -xi$. 

Spazio vettoriale: $v + w = w + v$, $(v + w) +u = v +(w + u)$, $v + 0 = v$, $v +(-v) = 0$, $\lambda(v + w) = \lambda v + \lambda w$, $(\lambda u)v =  \lambda uv$, $1 \cdot v = v$. Sottospazio $W \subset V$: $v_0 \in V$ origine $v_0 \in W$,  $v, v' \in W \to v + v' \in W$, $\lambda \in \mathbb{K} \to \lambda v \in W$.  

Prodotto vettoria;e in $\mathbb{R}^3$: $u = v \times w = \begin{bmatrix} v_2w_3 - v_3w_2 \\ v_3w_1 - v_1w_3 \\ v_1w_2 - v_2w_1 \end{bmatrix}$ . $u$ è ortogonale a $v, w$. $u = 0$ Se $v,w$ sono linearmente dipendenti. $(v | w| u)$ è base di $\mathbb{R}^3$. Vale $||u||^2 + \left<v, w\right>^2 = ||v||² \cdot ||w||^2$. Vale $||u||$ è l'area del parallelogramma.  

Molt matrici $M(m, n)$: $(AB)_{i,j} = \sum_{k = 1}^n{A_{i,k} \cdot B_{k, j}}$ . Per ottenere $(AB)_{i,j}$ si moltiplica la riga $i$ di $A$ con la colonna $j$ di $B$.  Prop: Assoc $A(BC) = (AB)C$, Dist $A(B + C) = AB +AC$, $\lambda(AB) = (\lambda A)B = A(\lambda B)$. 

Mosse gauss e det: $\lambda \cdot r \to \lambda \det(A)$, scambio $-\det(A)$, $\lambda A \to \lambda^n \cdot \det(A)$, somma non varia. Binet: $\det(AB) = \det(A) \cdot \det(B)$.

Inversa: $(cof)_{i,j} = (-1)^{i + j} \cdot \det(A_{i,j})$. $A^{-1} = \frac{1}{\det(A)} \cdot \ ^t(cof)$. Se $\det = 0$ allora non è né biett/invt.  Controllo $(cof)$: somma di prodotto euclideo tra due righe è $\det(A)$. 
Inversa 2x2: $A = \begin{bmatrix}a & b\\c & d\end{bmatrix}$, $A^{-1} = \frac{1}{ad -bc}\begin{bmatrix}d & -b\\-c & a\end{bmatrix}$. 

Teorema di Rouché-Capelli: 0 soluzioni se $rk(A|B) > rk(A)$, 1 soluzione se $rk(A) = n$, infinite se $rk(A) < n$. Dimensione spazio soluzioni: $\dim = n - rk(A)$. 

Applicazioni lineari: $f(v + w) = f(v) +f(w)$, $f(\lambda v) = \lambda f(v)$. Isomorfismo se biettiva. Endomorfismo: $f \ : \ V \to V$.  $L_A(v)$ è una FUNZIONE data da $A \cdot v$. 
Iniettiva se $\dim \ker f = 0$ cioè $\ker f$ contiene un solo vettore. Suriettiva se $\dim V \geq \dim Im \ f = \dim W$. $\dim im \ f$ è $Span$ dei vettori di $[f]$. $\dim(V) = \dim(\ker) + \dim(im)$. 

$A$ ~ $B$  (simili) solo se esiste $M$ per cui $A = M^{-1} \cdot B \cdot M$. Se simili: $rk(A) = rk(B)$ $\det(A) = \det(B)$, $A$ è invertibile se e solo se $B$ lo è. Invece, $A, B$ congruenti se $A = \ ^tM \cdot B \cdot M$. 

Diagonalizzabilità: se $[f]$ su $\mathbb{R}$ è simmetrica, allora è diagonalizzabile. Stesso se è su $\mathbb{C}$ ed è Hermitiana. Altrimenti, se ci sono tutti autovalori distinti è diagonalizzabile. Altrimenti, se tutte le $m^a = m^g$ è diagonalizzabile. Se matrice non su $\mathbb{C}$, allora controllare somma $m^a$ uguale grado.

Diagonalizzazione: $A = P \cdot D \cdot P^{-1}$, $D = P^{-1} \cdot D \cdot P$ dove $D$ contiene gli autovalori sulla diagonale, $P$ gli autovettori nello stesso ordine ed $A$ è la matrice originale. 

Prodotto scalare: $\left<v, w\right>$ con Prop: $\left<v + v', w\right> = \left<v, w\right> + \left<v', w\right>$, $\left<\lambda v, w\right> = \lambda \left<v, w\right>$, $\left<v, \right> = \left<w, v\right>$, $\left<v, w + w'\right> = \left<v, w\right> + \left<v, w'\right>$, $\left<v, \lambda w\right> = \lambda \left<v, w\right>$, $\left<v, 0\right> = 0$. Se $\left<v, w\right> = 0$ allora $v, w$ ortogonali. È degenere se $\left<v \neq 0, w\right> = 0 \ \forall w$. Si dice definito positivo se $\left<v, w\right> \geq 0 \ \forall v, w$. $g_S(v, w)$ Prodotto scalare con matrice $S$ dove $g_S(v, w) = \ ^tv \cdot S \cdot w$. Matrice associata $[g]_B$ nella base $B = \{v_1, v_n\}$ è dato da $([g]_B)_{i,j} = g_S(v_i, v_j)$.  $S$ può essere scritta in forma quadratica dove $ax^2_i$ è la posizione $i$-esima sulla diagonale e vale $a$, mentre $S_{i,j} = S_{j,i} = \frac{1}{2}bx_ix_j$.  È noto che $||v|| = \sqrt{g(v, v)}$. FARE ATTENZIONE AL TIPO DI PRODOTTO (Non sempre Euclideo).

Prodotto Hermitiano: simile allo scalare per matrici complesse. Prop: $\left<v + v', w\right> = \left<v, w\right> +\left<v', w\right>$, $\left<\lambda v, w\right> = \lambda\left<v, w\right>$, $\left<v, w\right> = \overline{\left<w, v\right>}$, $\left<v, w + w'\right> = \left<v, w\right> + \left<v, w'\right>$, $\left<v, \lambda w\right> = \overline{\lambda}\left<v, w\right>$, $\left<v, 0\right> = \left<0, v\right> = 0$, $\left<v, v\right> \in \mathbb{R}$. $H$ matrice Hermitiana, può descrivere un prodotto Hermitiano $g_H(v, w) = \ ^tv \cdot H \cdot \overline{w}$.  Prodotto Hermitiano Euclideo: $\left<v, w\right> = \ ^tv \cdot \overline{w}$. 

Rotazione: funzione $L_A \ : \ \mathbb{R}^2 \to \mathbb{R}^2$ con $A = \begin{bmatrix}\cos{\theta}&-\sin{\theta}\\\sin{\theta}&\cos{\theta}\end{bmatrix}$. Riflessione con $A = \begin{bmatrix}\cos{\theta}&\sin{\theta}\\\sin{\theta}&-\cos{\theta}\end{bmatrix}$. 

Autoagiunto: endomorfismo $T \ : \ V \to V$ con base ortonormale di $V$ se $\left<T(v), w\right> = \left<v, T(w)\right>$. Anche se $[T]^B_B$ Hermitiana in $\mathbb{C}$ o simmetrica in $\mathbb{R}$. Casi notevoli: $L_A \ : \ \mathbb{R}^n \to \mathbb{R}^n$ con $A$ simmetrica e $L_A \ : \ \mathbb{C}^n \to \mathbb{C}^n$ con $A$ Hermitiana. 

Angolo: $\cos \theta = \frac{\left<v, w\right>}{||v|| \cdot ||w||}$, $\theta = \arccos{\cos \theta}$. 

Proiezione ortogonale su vettore: $w, v$ vettori: $P_w(v) = \frac{\left<v, w\right>}{\left<w, w\right>}w$. Sia $W$ uno spazio vettoriale con base ortogonale con $\dim W = n$, allora $P_W(v) = \sum_{i = 1}^n{\frac{\left<v, w_i\right>}{\left<w_i, w_i\right>}}w_i$.   Se base ortonormale, niente denominatore. Se base non ortogonale, ortogonalizzare.

Gramm Schmidt: 
1. $w_1' = w_1$
2. $w_2' = w_2 - P_{w_1}(w_2)$
3. $w_n' = w_{m + 1}' = w_n - P_{\{w_1, ..., w_m\}}(w_n)$ 

#### Intersezioni
- Piano-Piano (Cartesiani): Sistema
- Piano-Piano (Parametrici): Trasformare in cartesiani -> parametrici
- Piano-Retta (Cartesiani): Sistema
- Retta-Retta (Cartesiani): Sistema
- Retta ($P + Span(v)$) - Retta ($Q + Span(w)$) (Parametriche): Porre uguali, $P +tv = Q +sw$, poi isolare: $tv +s(-w) = Q - P$, trasformare in sistema $\begin{bmatrix}v & -w & | & Q - P\end{bmatrix}$ 
- Piano (Cartesiano) - Retta (Parametrica $P + Span(v)$): Sostituire $x, y, z$ in equazione del piano $(P_0 +v_0t) +(P_1 +v_1t) +(P_3 +v_3t)$ 

#### Angoli
- Piano - Retta ($P +Span(v)$): $\phi$ angolo tra la retta ed il normale. $\theta$ angolo tra piano e retta dato da $\theta = \frac\pi2 -\phi$. Con $\phi = \arccos{\left|\frac{\left<v, n\right>}{||v|| \cdot ||n||}\right|}$ con $n$ normale del piano
- Retta ($P + Span(v)$) - Retta ($Q + Span(w)$): $\theta = \arccos{\left| \frac{\left<v, w\right>}{||v|| \cdot ||w||} \right|}$ 
- Piano - Piano: angolo trai normali
#### Distanze
- Punto-Punto: $d(P, Q) = ||Q - P||$ 
- Punto $P$ - Retta $Q + Span(v)$: $d(P, r) = \frac{||v \times (P -Q)||}{||v||}$ 
- Retta $r = P_0 +tv$ - Retta $r'= P_1 +sw$ (Parallele - se direzioni linearmente dipendenti): $d(r, r') = d(P_0, r')$ 
- Retta $r = P_0 +tv$ - Retta $r'= P_1 +sw$ (Sghembe): $\frac{|\det{(v|w|P_1 - P_0)}|}{||v \times w||}$ 
- Punto $P_0 = \begin{bmatrix}x_0\\ y_0\\ z_0\end{bmatrix}$ - Piano $\pi = ax +by +cz -d = 0$:$d(P_0, \pi) = \frac{|ax_0 +by_0 +cz_0 -d|}{\sqrt{a^2 +b^2 +c^2}}$ 

Sottospazio invariante: Consideriamo un endomorfismo $T \ : \ V \to V$, un sottospazio $U$ di $V$ è invariante rispetto a $T$ se $T(U) \subset U$. Il sottospaizo generato da un autovettore è $T$-invariante. Prop: lo è anche il complemento ortogonale. 

Teorema spettrale: $T$ con matrice associata Hermitiana/simmetrica su $\mathbb{C}$ o $\mathbb{R}$, allora $T$ autoaggiunto se e solo se tutti gli autovalori sono reali ed esite una base di autovettori ortonormali. 

Base: $\det \neq 0$
Generatori: $rk = \dim$
Linearmente indipendenti: $\dim \ker = n -rk = 0$
