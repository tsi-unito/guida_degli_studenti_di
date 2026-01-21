---
title: 'Neural networks and Deep learning  - Parte I - Esercizi'
---

# Reti neurali e Deep learning - Parte I - Esercizi

## Mathematical foundations

### Esempio 1 - Linear transformation

- **Linear transformation**:
    - $det(A) = a_1 \cdot a_4 - a_2 \cdot a_3 = 2 * 2 - 4 * 1 = 0$.
    - How does it affects the vector spaced composed by $V_1 = [1, 0]$ and $V_2 = [0, 1]$?
        - $(A \cdot V_1)_{1, 1} = 2$
        - $(A \cdot V_1)_{2, 1} = 1$
        - $(A \cdot V_2)_{1, 1} = 4$.
        - $(A \cdot V_2)_{2, 1} = 2$
        - Both vectors lies on the same line (a dimension is lost).

### Esempio 2 - Properties of derivatives

- Properties of derivaties:
    - Power rule: $(x^4)' = 4x^3$.
    - Linearity: $(3 \sin(x) + x^2)' = 3(\sin(x))' + (x^2) = 3 \cos(x) + 2x$.
    - Chain rule: $(\sin(x^2))' = \cos(x^2)(x^2)' 2 \cos(x^2) x$.
    - Product rule: $(x^2 x^3)' = 2x(x^3) + x^2 (3x^2) = 5x^4 = (x^5)'$.
    - Quotient rule: $(\frac{x^5}{x^2})' = \frac{5x^4 (x^2) - x^5 (2x)}{x^4} = \frac{3x^6}{x^4} = 3x^2 = (x^3)'$.

### Esempio 3 - Partial derivatives

- Evaluate the derivative of $z$ w.r.t. $t$:
    - Where $(x, y) = (t^2, t)$ and $z = f(x, y) = x^2 y^2$.
    - Then $\frac{dz}{dt} = \frac{\partial z}{\partial x} \frac{dx}{dt} + \frac{\partial z}{\partial y} \frac{dy}{dt} = 2xy^2 \cdot 2t + 2x^2 t \cdot 1 = 4t^5 + 2t^5 = 6t^5$.
    - The same results could have been obtained simply evaluating $\frac{d}{dt}t^6 = 6t^5$.
        - By noticing that $f(x, y) = (x(t))^2 \cdot (y(t))^2 = (t^2)^2 \cdot (t)^2 = t^4 t^2 = t^6$.

### Esempio 4 - Discrete probability distributions

- **Discrete probability distributions**:
    - A random variable $\text{x}$ taking $k$ possible values $x_1, \dots, x_k$ is considered.
    - The PMF $P(\text{x}) = \frac{1}{k}$ (**uniform probability**) is a valid PMF if:
        - It is defined over all possible states of $\text{x}$.
        - $\forall x \in \text{x}: 0 \leq P(x) = \frac{1}{k} \leq 1$.
        - $P(\boldsymbol{\Omega}) = \sum_{x \in \{x_1, \dots, x_k\}} P(x) = \sum_{x \in \{x_1, \dots, x_k\}} \frac{1}{k} = 1$.
    - Thanks to other properties it can be evaluated:
        - $P(\{x_1, x_2, x_3\} \cup \{x_4, x_5\}) = P(\{x_1, x_2, x_3\}) + P(\{x_4, x_5\}) = \frac{3}{k} + \frac{2}{k} = \frac{5}{k}$.
        - $P(\boldsymbol{\Omega} \setminus \{x_1, x_2\}) = 1 - \frac{2}{k} = \frac{k-2}{k} = P(\{x_3, \dots, x_k\})$.

### Esempio 5 - Continuous probability distributions

- **Continuous probability distributions**:
    - Consider a uniform distribution on an interval of the real numbers: $\text{x} \sim U(a, b)$.
        - Where $a$ and $b$ are the endpoints of the interval, with $b > a$.
    - The PDF $u(x; a, b)$ of the uniform distribution is:
        - $\frac{1}{b-a}$ if $x \in [a, b]$.
        - $0$, otherwise.
    - The PDF $u(x; a, b)$ is a valid PDF if:
        - It is defined over all possible states of $\text{x}$.
        - $\forall x \in \text{x}: p(x) \geq 0$.
        - $\int_{-\infty}^{\infty}u(x; a, b) \: dx = \int_a^b \frac{1}{b-a} dx = \frac{1}{b-a} \int_a^b 1 \: dx = \frac{1}{b-a} \cdot (x |_a^b) = 1$.
            - $(x |_a^b) = (b - a)$.
