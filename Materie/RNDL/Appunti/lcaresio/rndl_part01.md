---
title: 'Neural networks and Deep learning - Parte I - Introduction and Mathematical foundations'
---

# Neural networks and Deep learning - Parte I

## Introduction

### Introduction to Machine Learning

#### Supervised learning

- **Supervised learning**:
    - Where an algorithm learns from **labeled data**.
    - More formally:
        - Given a training set $\mathcal{X} = \{(\textbf{x}_i, y_i)\}_{i \in |N|}$.
        - Learn a function $g(\textbf{x}; \mathbb{\theta})$ that approximates the *true* function $f(\textbf{x})$ that generated the data.
            - $g(\circ)$ is the model, $\theta$ are its parameters.
    - $g$ is assumed to **generalize** to unseen data.

#### Tasks

- **Tasks**:
    - Tasks in ML specify the nature of the problem to be solved.
        - Classification, regression, clustering, association rules extraction, etc.
    - **Classification**:
        - A **supervised learning** task.
        - *Goal*: build a model to **classify** instances of an underlying concept using given **labels**.
        - `eg` Spam detection, sentiment analysis, object detection, LLM, etc.
    - **Regression**:
        - $y \in \mathbb{R}$ wile in classification $y$ is a categorical value.
        - `eg` stock price prediction, weather forecasting, image generation, etc.

#### Induction

> **Induction**: inference of a generalized conclusion from particular instances.

- **Induction**:
    - ML algorithms **optimizes** the parameters, $\theta$, in such a way a **loss function** $L$ is minimized.
        - $\theta^\star = \text{arg min}_{\theta} L(g(\textbf{X}; \theta), \textbf{y})$.
        - This assumes that the **induction** process hold in the applicative settings and generalizes to **unseen data**.
    - **Induction** is precisely what every algorithms aims to achieve when learning a classification function.
    - **No Free Lunch Theorem**: «all algorithms are equivalent, on average, by any of the following measures of risk».
        - Or, «all models are wrong, but some models are useful».
        - A learning algorithms is only as good as its **inductive bias**. 
            - How well the inductive bias of the algorithms fits the problem at hand must be considered.
    - `eg` Examples of inductive bias:
        - A **linear classifier** assumes that:
            - Examples lie in a metric space.
            - The underlying concept is linear in nature.
        - A **convolutional neural network** assumes that:
            - The underlying concept is translation invariant.
            - The input is an image (a 2D grid of pixels where adjacent pixels are correlated).
        - A **recurrent neural network** assumes that:
            - The underlying concept is sequential in nature.

#### Training a neural network

- **Training a neural network**:
    - NN are a incredible flexible family of models that can be used to approximate any function.
        - This flexibility comes at a cost: they depend on a vast number of **hyperparameters**.
            - Like the number of layers, neurons in each layer, learning rate, optimizer, etc.
            - These hyperparameters need to be set correctly in order to achieve good performance.
        - It is then important to understand how to **set these hyperparameters correctly**.
            - And how to validate the quality of the inferred model **without overestimating its performance**.
    - Problem statement:
        - Define the correct procedure to:
            - Find the best possible way to **set the hyperparameters**.
            - Get an **accurate estimation of the generalization error** at the end of the process.

#### Errors

##### Generalization error

- `def` **Generalization errors**: $R = \mathbb{E}_{(\textbf{x}, y) \sim p^\star}[L(y, g(\textbf{x}, \theta))]$.
    - The error that the acquired classifier $g$ commit, on average, on examples drawn from the same distribution used for sampling the examples in the training set.
    - $p^\star$ is the true distribution of the data.
    - $L$ is a loss function used to measure how bad the error is when $y$ is predicted as $g(\textbf{x}, \theta)$.
    - Problems:
        - No actual access to $p^\star$.
        - Even with access to $p^\star$, it's not possible to compute an average on an infinite number of samples.

##### Loss functions

- **Loss functions**:
    - `def` **$0-1$ loss**: $L(y, y') = \mathcal{I}_{y \neq y'} = 1$ if $y \neq y'$, $0$ otherwise.
    - `def` **Quadratic loss**: $L(y, y') = (y-y')^2$.
    - But many others are possible (`eg` hinge, exponential, logistic, etc).

##### Empirical error

- `def` **Empirical error**: $\hat{R}_T = \frac{1}{|T|} \sum_{(\textbf{x}, y) \in T} L(y, g(\textbf{x}; \theta))$.
    - It should be evident that (in general) $R$ cannot be computed.
        - To overcome this problem in most cases in $R$ is approximated with the empirical error.
    - $T$ is a finite sample drawn from $p^\star$.

##### Training and test errors

- **Training and test errors**:
    - **Training error** ($\hat{R}_{Tr}$): when $T$ is the **training set** ($T = Tr$).
        - Since it's implicitly (sometimes explicitly) optimized by the learning algorithm, it has an **optimistic bias**.
    - **Test error** ($\hat{R}_{Te}$): when $T$ is the **test set** ($T = Te$).
        - It serves as an unbiased estimator of the generalization error $R$.
        - It may exhibit a **pessimistic bias** because retraining the model on the entire dataset is likely to produce a model with a lower error.

#### Overfitting

- **Overfitting**:
    - When $\hat{R}_{Te} - \hat{R}_{Tr} > 0$, the algorithm is said to be **overfitting**.
    - Overfitting is common problem for learning algorithms.
        - And it is usually necessary to counter it by some method.
    - But minimizing the generalization error on the test set is a sort of tuning.
        - This makes the test set error an **optimistic estimator** of the generalization error.
        - And therefore **overfitting the test set**.
    - To overcome this problem, an extra set, the **validation set**, is needed.
        - Used to solely evaluate model performance under a given choice of hyperparameters.
        - Once the hyperparameters are set, $Te$ is used to asses the final model's quality.
        - When data is scarce, choices have been made and the final quality is fine, retrain the classifier on $Tr + Va$.
            - When the test set is no longer needed, it can be merged to the training set.
    - Selecting best hyperparameters for a model (?):
        - Both validation and test sets are **finite samples**, measuring the error on them will introduce some noise in the estimation of the generalization error.
            - $\hat{R}_{Te} = R + \epsilon_{Te}$ and $\hat{R}_{Val} = R + \epsilon_{Val}$.
        - The model is chose according to the hyperparameters that minimize the error on the validation set.
        - Determine if at the end of the process $\hat{R}_{Te}$ and $\hat{R}_{Val}$ are **unbiased estimators of the GE**.
            - Which is, $\mathbb{E}[\hat{R}_{Te}] = \mathbb{E}[\hat{R}_{Val}] = R$.
            - The distribution of Test errors is uniform and has mean $\mu_0$. 
            - The distribution of Validation errors is skewed to the left, resulting in an under-estimation.
    - Training, validation and test sets:
        - **Training set**: used during learning.
        - **Validation set**: used to assess the quality of the current choice of hyperparameters.
        - **Test set**: used to asses the quality of the final classifier.

-----

## Mathematical foundations

### Matrices

- **Matrices**:
    - In ANN, vectors and matrices are everywhere.
        - Inputs are vectors, weights are matrices.
        - Vectors and matrices must be multiplied to calculate network output.
    - A **scalar** is just a single number $x$.
    - A **vector** is an array of numbers.
    - A **matrix** is a $2D$ array of numbers.
        - Indices: $i$ for the row and $j$ for the column.
    - A **tensor** is an array with more than two dimensions.
        - Indices: $i$ for the batch (slice), $j$ for the row and $k$ for the column.
    - The **transpose** $\textbf{A}^T$ of a matrix $\textbf{A}$ is a mirror image, $(\textbf{A}^T)_{i, j} = \textbf{A}_{j, i}$.
    - If matrices have the same shape, they can be **added**, $C_{i, j} = A_{i, j} + B_{i, j}$. 
        - A scalar can be added or multiplied to, $D_{i, j} = a \cdot B_{i, j} + c$.
    - **Matrix multiplication**: if $A$ has shape $m \times n$ and has shape $B$ $n \times p$, $\textbf{A} \cdot \textbf{B}$.
        - $\textbf{A} \cdot \textbf{B} \neq \textbf{B} \cdot \textbf{A}$.
    - **Vector multiplication**: $\textbf{x} \cdot \textbf{y} = \textbf{x}^T \textbf{y}$.

#### Geometric interpretation

- **Geometric interpretation**:
    - Vector can be represented geometrically.
        - $\textbf{x} = [x_1, x_2]$ will be a vector originating at $(0, 0)$ and arriving at $(x_1, x_2)$.
    - `def` **Euclidean norm** (or $L^2$ norm): $\| \textbf{x} \| = \sqrt{x_1^2 + x_2^2} = \sqrt{x \cdot x}$.
        - The euclidean distance from the origin to the point identified by $\textbf{x}$.
        - `def` **Euclidean norm**: $\| \textbf{x} \| = \sqrt{\sum_i x_i^2} = \sqrt{x \cdot x}$.
        - The squared euclidean norm is also used.
    - `def` **$L^1$ norm**: $\| \textbf{x} \|_1 = \sum_i |x_i|$.
    - `def` **Dot product**: $\textbf{x} \cdot \textbf{y} = \| \textbf{x} \| \| \textbf{y} \| \cos(\theta)$.

#### Linear transformation

- **Linear transformation**:
    - Matrices are a tool to **linearly transform** vectors (and vector space).
    - **Determinant** of a squared matrix, $det(\textbf{A})$, is a function mapping matrices to real scalar.
        - $det(\textbf{A})$ gives a measure of how multiplication by the matrix expands or contract space.
            - If $| \det(\textbf{A})| = 1$ the volume remains unchanged.
            - If $\det(\textbf{A}) = 0$ the space is contracted along at least one dimension.
        - For $\textbf{A}$ with dimension $2$, $det(A) = a_1 \cdot a_4 - a_2 \cdot a_3$.

#### Special matrices

- **Special matrices**:
    - **Identity matrix**: it does not change any vector when multiplied the matrix by the vector.
    - **Inverse matrix** ($A^{-1}$): the matrix such as $A^{-1} A = I_n$.
        - Matrices for which $A^{-1}$ exists are **invertible** (only squared matrices).
        - `prop` $\textbf{A}$ is invertible iff $det(\textbf{A}) \neq 0$.
        - When $\textbf{A}^{-1}$ exists, there are several algorithms for finding it.
    - **Symmetric matrix**: any matrix equal to its transpose ($\textbf{A} = \textbf{A}^T$).
    - **Unit vector**: a vector with unit norm ($\| \textbf{x} \|_2 = 1$).
    - **Orthogonal vectors**: $\textbf{x}$ and $\textbf{y}$ to each other if $\textbf{x}^T \textbf{y} = 0$.
        - Vectors orthogonal to each other and with unit norm are **orthonormal**.
    - **Orthogonal matrix**: a square matrix whose rows are mutually orthonormal and whose columns are mutually orthonormal.
        - `prop` For orthogonal matrices $\textbf{A}^T \textbf{A} = \textbf{AA}^T = \textbf{I}$.
            - Hence $\textbf{A}^{-1} = \textbf{A}^T$.

### Calculus

#### Derivatives

- **Derivatives**:
    - The slope of the tangent line to $f$ at point $x$ (denoted as $f'(x)$ or $\frac{df}{dx}(x)$).
    - It specifies how to scale a small change in input in order to obtain the corresponding change in output.
        - $f(x + \epsilon) \approx f(x) + \epsilon f'(x)$.
    - The secant line becomes the tangent line when $\epsilon \to 0$, $f'(x) = \lim_{h \to 0} \frac{f(x + \epsilon) - f(x)}{\epsilon}$.
    - `prop` Properties of derivatives:
        - Linearity: $(\alpha f(x) + \beta g(x))' \equiv \alpha f'(x) + \beta g'(x)$.
        - **Chain rule**: $(f(g(x)))' \equiv f'(g(x))g'(x)$.
        - Product rule: $(g(x)h(x))' \equiv g'(x)h(x) + g(x)h'(x)$.
        - Quotient rule: $(\frac{f(x)}{g(x)})' \equiv \frac{f(x)'g(x) - f(x)g'(x)}{(g(x))^2}$.
        - Power rule: $(x^r)' \equiv rx^{r-1}$.

#### Integrals

- **Integrals**:
    - The integral value of $f$ between $a$ and $b$ is the area under $f$ in the given region.
        - When the function is below $0$, the area contributes negatively.
    - `def` **Fundamental Theorem of Calculus**:
        - If $f$ admits an antiderivative $F$ (if it exists $F$ such that $F'(x) = f(x)$) then:
            - $\int f(x) \: dx = F(x) + C$.
            - $\int_a^b f(x) \: dx = F(x) |_a^b = F(b) - F(a)$.
    - Approximation integral area with rectangles: $\int_a^b f(x) \: dx \approx \sum_i f(x) \: dx$. 
    - `prop` Properties of integrals:
        - Linearity: $\int \alpha f(x) + \beta g(x) \: dx \equiv \alpha \int f(x) \: dx + \beta \int g(x) \: dx$.
        - Constant rule: $\int k \: dx \equiv kx + C$.
        - Power rule: $\int x^n \: dx \equiv \frac{x^{n+1}}{n+1} + C$ ($n \neq -1$).
        - Log rule: $\int \frac{1}{x} \: dx \equiv \ln(|x|) + C$.
        - Exponential rule: $\int a^{kx} \: dx \equiv \frac{a^{kx}}{k \ln a}$ ($a > 0$, $a \neq 1$).
        - Sine rule: $\int \sin(x) \: dx \equiv - \cos(x) + C$.
        - Cosine rule: $\int \cos(x) \: dx \equiv \sin(x) + C$.
        - Derivatives can be easily computed automatically, but for antiderivatives this is not true.
            - But those properties can be applied.

#### Partial derivatives

- **Partial derivatives**:
    - A function $y = f(x_1, \dots, x_n) = f(\textbf{x})$ is given, where $y \in \mathbb{R}$ and $\textbf{x} \in \mathbb{R}^n$.
    - $\frac{\partial}{\partial x_j}f(\textbf{x})$ measures how $f$ changes as only the $x_j$ variable increases at point $\textbf{x}$.
        - $\frac{\partial}{\partial x_j} f(\textbf{x}) = \lim_{h \to 0} \frac{f(\textbf{x} + h \hat{i}_j) - f(\textbf{x})}{h} = \lim_{h \to 0} \frac{f(x_1, \dots, x_j + h, \dots, x_n) - f(x_1, \dots, x_n)}{h}$.
            - $h$ multiplied by the versor $\hat{i}_j$ is a *nudge* in the direction given by the current versor.
    - `def` **Gradient** (of $f$): $\nabla_x f$ (or $\nabla f$) $= [\frac{\partial f}{\partial x_1}, \dots, \frac{\partial f}{\partial x_n}]^T$.
        - The vector collecting all partial derivatives.
        - When $\nabla f(\textbf{x}_0) = \textbf{0}$, the tangent plane is horizontal.
            - The function doesn't grow in any direction along that point.
    - `prop` **Chain rule for multivariate calculus**: $\frac{dz}{dt} = \frac{\partial z}{\partial x} \frac{dx}{dt} + \frac{\partial z}{\partial y} \frac{dy}{dt}$.
        - $z = f(x, y)$ is assumed and let $x, y$ depend on an additional variable $t$ ($z$ can be seen as $f(x(t), y(t))$).
        - `prop` **Chain rule for multivariate calculus**: $\frac{df}{dt} = \sum_{i=1}^n \frac{\partial f}{\partial x_i} \frac{dx_i}{dt}$.
            - More in general for $f \colon \mathbb{R}^n \to \mathbb{R}$ when $x_1, \dots, x_n$ depend on a variable $t$.

#### Directional derivatives

- `def` **Directional derivative**: $D_\textbf{u} f(\textbf{x}) = \lim_{h \to 0} \frac{f(\textbf{x} + h \textbf{u}) - f(\textbf{x})}{h}$.
    - DD of $f$ at $\textbf{x}$ in $\textbf{u}$ direction: the rate of change in the direction given by the unit vector $\textbf{u}$.
    - The derivative of $f(\textbf{x} + \alpha \textbf{u})$ w.r.t. $\alpha$ evaluated at $\alpha = 0$.
    - Using the chain rule the expression for $D_\textbf{u} f(\textbf{x})$ can be easily computed.
        - $D_\textbf{u} f(\textbf{x}) = \frac{d}{d \alpha} f(\textbf{x} + \alpha \textbf{u}) |_{\alpha = 0} = \sum_{i=1}^n \frac{\partial f(\textbf{x} + \alpha \textbf{u})}{\partial x_i} |_{\alpha = 0} \frac{dx_i}{d \alpha} = \nabla f(\textbf{x}) \cdot \textbf{u} = \textbf{u}^T \nabla f(\textbf{x})$.

#### Gradient and optimization

- **Gradient and optimization**:
    - Goal: to find the direction in which the function increases the most.
        - To find $\textbf{u}$ such that $\nabla_\textbf{u} f$ is largest.
    - $\max_{\textbf{u}, \textbf{u}^T \textbf{u}=1} D_\textbf{u} f(\textbf{x}) = \max_{\textbf{u}, \textbf{u}^T \textbf{u}=1} \textbf{u}^T \nabla f(\textbf{x}) = \max_{\textbf{u}, \textbf{u}^T \textbf{u}=1} |\textbf{u}| | \nabla f(\textbf{x}) | \cos(\theta)$.
        - $\textbf{u}^T \textbf{u}=1$ is used to ensure that the $\textbf{u}$ is still a unit vector.
            - $\| \textbf{u} \|_2 = \sqrt{\sum_i u_i^2} = \sqrt{u \cdot u} = \textbf{u}^T \textbf{u}$.
        - $|\textbf{u}| = 1$ and since $\nabla f(\textbf{x})$ doesn't depend on $\textbf{u}$.
            - Therefore, only to find $\textbf{u}$ that maximises $\cos \theta$ is needed.
        - Therefore the maximum is attained when $\textbf{u}$ is in the same direction as $\nabla f(\textbf{x})$.
    - `!` The **gradient** points in the direction in which $f$ **increases the most**.

#### Jacobian Matrix

- `def` **Jacobian matrix**: $\textbf{J}_{i, j} = \frac{\partial}{\partial x_j}f(\textbf{x})_i$ or $\textbf{J} = [\nabla[f(\textbf{x})_i)^T]_{i=1}^m$.
    - A multi-valued, multi-variable function $f \colon \mathbb{R}^n \to \mathbb{R}^m$, $f(\textbf{x}) = [f(\textbf{x}_1), \dots, f(\textbf{x}_n)]$ is considered.
    - The Jacobian matrix $\textbf{J} \in \mathbb{R}^{m \times n}$ contains the partial derivative of all $f(\textbf{x})_i$ for all variables $x_j$.
        - With $1 \leq i \leq m$ and $1 \leq j \leq n$.

#### Hessian Matrix

- **Hessian Matrix**:
    - A **second derivatives** is a derivative of a derivative.
        - Specify how the first derivative will change as the input is varied (it measures **curvature**).
        - With $f \colon \mathbb{R}^n \to \mathbb{R}$, $n^2$ second derivatives can be computed as $\frac{\partial^2}{\partial x_i \partial x_j} f(\textbf{x})$.
            - There is a second derivative for each partial derivative.
    - `def` **Hessian matrix** ($H(f)$): $H(f) = \textbf{J}(\nabla f)$.
        - It contains all these partial derivatives.
    - `prop` Properties of Hessian matrices:
        - The Hessian Matrix is **symmetric** where the second partial derivates are continuous (Schwarz's Theorem).
            - Anywhere that the second partial derivatives are continues, differential operators are commutative.
            - Therefore their order can be swapped: $\frac{\partial^2}{\partial x_i \partial x_j} f(\textbf{x}) = \frac{\partial^2}{\partial x_j \partial x_i} f(\textbf{x})$.
        - When $\nabla f(\textbf{x}_0) = \textbf{0}$, the Hessian helps detect **minimum**, **maximum**, or **saddle points**.
            - **Maximum**: if the Hessian is negative definite (i.e. all eigenvalues are $<0$).
            - **Minimum**: if the Hessian is positive definite (i.e. all eigenvalues are $>0$).
            - If the Hessian is neither positive nor negative definite (at least one zero eigenvalue exists):
                - **Saddle point**: if there is at least one positive eigenvalue and one negative.
                - **Inconclusive test**: otherwise.

### Probability theory

- **Probability theory**:
    - Can be seen as the extension of logic to deal with uncertainty.
        - Logic provides a set of formal rules for determining what proposition are implied to be true.
            - Given the assumption that some other set of propositions is true or false.
        - PT provides a set of formal rules for determining the likelihood of a proposition being true.
            - Given the likelihood of other propositions.

#### Probability distributions

- **Probability distributions**:
    - **Random variable**: a variable that can take on different values randomly.
        - Random variables may be **discrete** or **continuous**.
        - The set of all possible values taken by a random variable $\text{x}$ is denoted $\boldsymbol{\Omega}_\text{x}$.

##### Discrete probability distributions

- **Discrete probability distribution**:
    - Described using a **probability mass function** (PMF, $P(\textbf{x}$)):
        - It maps from a state of a random variable to the probability of that random variable taking on that state.
        - $x \sim P$: the random variable $\text{x}$ follows the $P(\text{x})$ distribution.
    - `prop` Properties of a PMF (not to be confused with axioms of probabilities):
        - To be on a PMF on a random variable $\text{x}$, a function $P$ must:
            - The domain of $P$ must be the set of all possible states of $\text{x}$.
            - $\forall x \in \text{x} \colon 0 \leq P(x) \leq 1$.
            - $\sum_{x \in \text{x}} P(x) = 1$.
        - With $S$, $S_1$ and $S_2$ being sets of possible outcomes:
            - $P(S) = \sum_{x \in S} P(x)$ ($P(S)$ is a shorthand for $P(\text{x} \in S)$).
            - $P(S_1 \cup S_2) = P(S_1) + P(S_2) - P(S_1 \cap S_2)$.
            - $P(\boldsymbol{\Omega} \setminus S) = 1 - P(S)$.

##### Continuous probability distributions

- **Continuous probability distributions**:
    - When working with continuous random variables, a **probability density function** (PDF, $p(\textbf{x})$) is used.
    - `prop` Properties of a PDF:
        - To be on a PDF on a random variable $\text{x}$, a function $p$ must:
            - The domain of $p$ must be the set of all possible states of $\textbf{x}$.
            - $\forall x \in \text{x} \colon p(x) \geq 0$ ($p(x) \leq 1$ is not required).
            - $\int p(x) \: dx = 1$.

##### Marginal probability

- **Marginal probability**:
    - The probability distribution over a subset of the set of variables.
    - Marginal probabilities are computed by summing over all values of other variables.
    - `eg` With discrete variables $\text{x}$ and $\text{y}$ with a joint distribution $P(\text{x}, \text{y})$:
        - The marginal distribution $P(x)$ is $\forall x \in \text{x} \colon P(x) = \sum_y P(\text{x} = x, \text{y} = y)$.
    - `eg` With continuous variables $\text{x}$ and $\text{y}$ with a joint distribution $p(\text{x}, \text{y})$:
        - The marginal distribution $p(x)$ is $\int p(x, y) \: dy$.

#### Conditional probability

- **Conditional probability**: $P(\text{y} = y \mid \text{x} = x) = \frac{P(\text{y} = y, \text{x} = x)}{P(\text{x} = x)}$.
    - In many cases, it's useful to have the probability of some event given that some other has happened.
    - `def` **Chain rule of Conditional probabilities**: $P(\text{x}^{(1)}, \dots, \text{x}^{(n)}) = P(\text{x}^{(1)}) \prod_{i=2}^n P(\text{x}^{(i)} \mid \text{x}^{(1)}, \dots, \text{x}^{(i-1)})$.
        - Any joint probability distribution over many random variables may be decomposed.
            - Into products of conditional distribution over only one variable.
    - `def` **Bayes rule**: $P(\text{x} \mid \text{y}) = \frac{P(\text{y} \mid \text{x})}{P(\text{y})}$.
        - Used to find where $P(\text{x} \mid \text{y})$ where $P(\text{y} \mid \text{x})$ is known.

##### Independence

- **Independence**:
    - **Independent variables** ($\text{x} \perp \text{y}$): $\text{x}$ and $\text{y}$ if their probability distribution can be expressed as a product of two factors, one involving only $\text{x}$ and one involving only $\text{y}$.
        - $\forall x \in \text{x}, y \in \text{y} \colon p(\text{x} = x, \text{y} = y) = p(\text{x} = x)p(\text{y} = y)$.
        - $\text{x} \perp \text{y} \iff \forall x \in \text{x}, y \in \text{y}: p(x \mid y) = p(x) \land p(y \mid x) = p(y)$.
    - **Conditional independent variables** ($\text{x} \perp \text{y} \mid \text{z}$, given a random variable $\text{z}$): $\text{x}$ and $\text{y}$ if the conditional probability over $\text{x}$ and $\text{y}$ factorizes in this way for every value of $\text{z}$.
        - $\forall x \in \text{x}, y \in \text{y}, z \in \text{z} \colon p(\text{x} = x, \text{y} = y \mid \text{z} = z) = p(\text{x} = x \mid \text{z} = z)p(\text{y} = y \mid \text{z} = z)$.
        - $\text{x}$ and $\text{y}$ may not be independent per se, but they may be come it when another value is known.

#### Core statistical measures

- **Core statistical measures**:
    - **Expectation** ($\mu$):
        - The expected value of $f(x)$ with respect to $P(x)$ is the mean value that $f$ takes on where $x$ is drawn from $P$.
        - Discrete variables: $\mathbb{E}_{x \sim P}[f(x)] = \sum_x P(x) f(x)$.
        - Continuous variables: $\mathbb{E}_{x \sim p}[f(x)] = \int p(x) f(x) \: dx$.
        - `prop` **Linearity of the expectation operator**: $\mathbb{E}[\alpha f(x) + \beta g(x)] = \alpha \mathbb{E}[f(x)] + \beta \mathbb{E}[g(x)]$.
    - `def` **Variance** ($\sigma^2$): $Var[f(\text{x})] = \mathbb{E}[(f(\text{x}) - \mathbb{E}[f(\text{x})])^2] = \mathbb{E}[f(\text{x})^2] - \mathbb{E}[f(\text{x})]^2$.
        - It gives a measure of how much the values of a function of $\text{x}$ vary as a different values of $\text{x}$ from its probability distribution is sampled.
    - **Standard deviation** ($\sigma$): the square root of the variance.
        - For a normally distributed variable, about $95\%$ of the points fall into the range $\mu \pm 2 \sigma$.

##### Covariance

- `def` **Covariance**: $Cov(\text{x}, \text{y}) = \mathbb{E}_{x, y \sim P(\text{x}, \text{y})}[(x - \mathbb{E}[\text{x}])(y - \mathbb{E}[\text{y}])] = \mathbb{E}_{x, y \sim P(\text{x}, \text{y})}[(x - \mu_{\text{x}})(y - \mu_{\text{y}})]$.
    - It gives a sense of how much two random variables are related to each other.
    - It is affected by the scale of variables.
    - `def` **Correlation**: $Corr(x, y) = \frac{Cov(x, y)}{\sigma_x \sigma_y}$.
        - Adjust the scale of each variable, ensuring that the relation between the variables is measured without being influenced by their individual magnitude.
        - A correlation close to $\pm 1$ indicates a strong relationship between the variables.
        - A correlation close to $0$ suggests that the variables may be independent.
    - **Covariance** and **dependence** are related, but are in fact **distinct** concepts.
        - Two variables that are independent have zero covariance.
        - Two variables that have non-zero covariance are dependent.
        - Two variables can have zero covariance and be dependent nonetheless.

#### Common probability distribution

##### Bernoulli distribution

- **Bernoulli distribution**: a distribution over a single **binary** random variable.
    - Controlled by a single parameter $\phi \in [0, 1]$, which gives the probability of $\text{x} = 1$.
    - `prop` Properties of Bernoulli distributions:
        - $P(\text{x} = 1) = \phi$ and $P(\text{x} = 0) = 1 - \phi$.
        - $P(\text{x} = x) = \phi^x (1 - \phi)^{1-x}$.
        - $\mathbb{E}[\text{x}] = \phi$ and $Var(\text{x}) = \phi (1 - \phi)$.
- **Multinulli distribution** (or categorical): a distribution over a single discrete variable with $k$ (finite) different states.
    - It is parametrized by a vector $\textbf{p} \in [0, 1]^k$, with $\textbf{1}^T \textbf{p} = 1$.
        - Where $p_i$ gives the probability of the $i$-th state.
    - Often used to refer to distributions over **categories** of objects.
        - The state $1$ having numerical value $1$, etc, is not assumed.
        - usually no expectation or variance is computed.

##### Binomial distribution

- **Binomial distribution**:
    - It gives the probability of observing a given number of success in a **repeated Bernoulli experiment**.
    - It is parametrized by:
        - $p$: the probability of success of the Bernoulli experiment.
        - $N$: the number of total repetitions of the Bernoulli experiment.
    - If $\text{x} \sim Bi(p, N)$ then:
        - $P(\text{x} = k) = \binom{N}{k} p^k (1 - p)^{N-k}$ ($\binom{N}{k} = \frac{N!}{k! (N-k)!}$).
        - $\mathbb{E}[\text{x}] = Np$ and $Var[\text{x}] = Np(1-p)$.

##### Gaussian distribution

- **Gaussian distribution** (or **normal distribution**):
    - Most commonly used distribution over real numbers.
    - It is parametrized by mean $\mu$ and variance $\sigma^2$.
    - If $\text{x} \sim \mathcal{N}(\mu, \sigma^2)$ then:
        - $p(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} \exp(- \frac{(x - \mu)^2}{2 \sigma^2})$.
            - $\frac{1}{\sqrt{2 \pi \sigma^2}}$ to ensure that its integral will be $1$.
            - It goes to $0$ exponentially fast as $x$ goes far from $\mu$.
    - Many distribution in the real world are truly close to being normal distribution.
        - `th` **Central limit theorem of PT**:
            - Let $X_1, \dots, X_n$ be a sequence of iid random variable with finite mean $\mu$ and variance $\sigma^2$.
            - Then the distribution of $S = \sum_{i=1}^n X_i$ approaches $\mathcal{N}(n \mu, n \sigma^2)$ as $n$ approaches infinity.
            - The average $\frac{1}{n}S$ also approaches $\mathcal{N}(n \mu, n \sigma^2)$ as $n$ approaches infinity.
    - Out of all possible probability distribution with the same mean and variance, $\mathcal{N}$ **encodes the maximum amount of uncertainty**.
        - It is the distribution having the **maximum entropy**.
        - With an unknown phenomenon, to assume that it behaves as $\mathcal{N}$ takes the least amount of assumptions.
    - $\mathcal{N}$ generalizes to $\mathbb{R}^n$: $\mathcal{N}(\textbf{x}, \boldsymbol{\mu}, \boldsymbol{\Sigma}) = \frac{1}{\sqrt{(2 \pi)^n \text{det}(\boldsymbol{\Sigma})}} \exp(- \frac{1}{2} (\textbf{x} - \boldsymbol{\mu})^T \boldsymbol{\Sigma}^{-1}(\textbf{x} - \boldsymbol{\mu}))$.
        - $\boldsymbol{\mu}$: a vector denoting the mean of the distribution.
        - $\boldsymbol{\Sigma}$: the covariance matrix of the distribution.
            - Covariance matrices are **symmetric** and **positive semi-definite**.
            - Their main diagonal contains variances.
            - If $\boldsymbol{\Sigma} = \boldsymbol{\sigma}^2 \textbf{I}$ the variance is the same in every direction: the distribution is said to be **isotropic**.

##### Exponential and Laplace distributions

- **Exponential and Laplace distributions**:
    - `def` **Exponential distribution**: $p(x; \lambda) = \lambda \exp(-\lambda x)$, $x \geq 0$.
        - In DL, a distribution with a sharp point at $x = 0$ is needed. 
    - `def` **Laplace distribution**: $\text{Laplace}(x; \mu, \gamma) = \frac{1}{2 \gamma} \exp(- \frac{|x - \mu|}{\gamma})$.
        - Closely related to the exponential one.
        - It allows to place a sharp peak of probability mass at an arbitrary point $\mu$.

##### Mixtures of distributions

- **Mixtures of distributions**:
    - It's common to define probability distributions by combining other simpler distributions.
    - One common way is to construct a **mixture distribution**: $P(\text{x}) = \sum_i P(\text{c} = i) P(\text{x} \mid \text{c} = i)$.
        - Where $p(c)$ is a multinulli distribution over the components of the mixture.
    - **Latent variables**:
        - Random variables that **cannot be observed directly**.
            - The component identity variable $c$ of the mixture model provides an example.
        - Latent variables may be related to $\text{x}$ through the joint distribution.
            - Then $P(\text{x}, \text{c}) = P(\text{x} \mid \text{c}) P(\text{c})$.

#### Useful properties of common functions

- **Useful properties of common functions**:
    - `def` **Sigmoid**: $\sigma(x) = \frac{1}{1 + \exp(-x)}$.
        - Often used to produce the $\phi$ paramter of a Bernoulli distribution.
        - It **saturates** for large (negative/positive) values of $x$.
    - `def` **Softplus**: $\zeta(x) = \log(1 + exp(x))$.
        - A smoothed version of $x^+ = \max(0, x)$.

#### Continuous random variables

##### Measure theory

- **Measure theory**:
    - A proper formal understanding of Continuous random variables and PDF requires **measure theory**.
        - Without it, one might encounter paradoxical situation.
            - `eg` To construct two set $S_1$ and $S_2$ where $S_1 \cap S_2 = \emptyset$ such that $p(x \in S_1) + p(x \in S_2) > 1$.
            - These paradoxes usually involve constructing very exotic sets, but the possibility exists.
        - One of MT key contributions is that it provides a framework to characterize sets where probabilities can be consistently computed, thus avoiding paradoxes.
    - **Negligibly small** set of points:
        - It has **measure zero** (`eg` a line in $\mathbb{R}^2$ has measure zero).
        - Any **union of countably many sets** having measure zero has measure zero.
            - So the set of all rational numbers has measure zero.
            - A property that **holds almost anywhere**: 
                - When a property holds throughout all of space except for points in a set of measure zero.

##### Continuous variables that are deterministic functions of one another 

- **Continuous variables that are deterministic functions of one another**:
    - Two random variables $\textbf{x}$ and $\textbf{y}$, such that $\textbf{y} = g(\textbf{x})$, are given.
        - Where $g$ is an invertible, continuous differentiable transformation.
        - Since $g$ is invertible, $\textbf{x} = g^{-1}(\textbf{y})$.
            - By inverting the transformation, the probability in the initial space should be computable.
        - Unfortunately: $p_{\text{y}}(\textbf{y}) \neq p_{\text{x}} (g^{-1}(\textbf{y}))$.
            - Since the **transformation changes the space**.
    - This approach fails to account for the **distortion of space** introduced by $g$.
        - The probability of $x$ lying in an infinitesimally small region with volume $\delta x$ is given by $p(x) \delta x$.
        - Since $g$ can expand or contract space, the infinitesimal volume surround $x$ in $x$ space may have different volume in $y$ space.
    - To correct the problem the following property must be preserved: $|p_y(g(x))dy| = |p_x(x) dx|$.
        - Which yields $p_y(g(x))|dy| = p_x(x)|dx| \implies p_x(x) = p_y(g(x)) |\frac{d}{dx}g(x)|$.
        - Or equivalently, $p_y(y)|dy| = p_x(g^{-1}(y))|dx| \implies p_y(y) = p_x(g^{-1}(y)) |\frac{d}{dy}g^{-1}(y)|$.
    - In **higher dimension** $g \colon \mathbb{R}^m \to \mathbb{R}^n$ the derivative generalizes to the Jacobian matrix.
        - And the absolute value to the absolute value of the determinant.
        - $P_x(\textbf{x}) = p_y(g(\textbf{x})) |det(J)|$.

#### Graphical models

- **Graphical models**:
    - Often probability distribution can be split into many factors.
        - Like when a random variable influences another, and the latter influences another one, etc.
        - `eg` $P(a, b, c) = p(a) p(b \mid a) p(c \mid b)$ (the second expression is way simpler).
            - With $10$ values for each variable, $10 + 10^2 + 10^2 = 210$ instead of $10^3 = 1000$.
        - These factorizations can greatly reduce the number of parameters needed to describe the distribution.
    - Factorization over distributions can be visually described using **graphs** (directed or undirected).

##### Directed models

- **Directed models**: use graphs with directed edges.
    - They represent factorizations into conditional probabilities distributions.
    - One factor for every random variable $x_i$.
    - An edge from $a$ to $b$ represent the dependency $P(b \mid a)$.
    - `def` **Directed model factorization**: $p(\textbf{x}) = \prod_i p(x_i \mid P_{a_{\mathcal{G}}}(\text{x}_i))$.
        - Where $P_{a_{\mathcal{G}}}(\text{x}_i)$ is the set of parents of $\text{x}_i$.
        - It consists of the conditional distribution of $\text{x}_i$ given its parameters.

##### Undirected models

- **Undirected models**: use graphs with undirected edges.
    - Variables influence each others.
    - They represent factorizations using a set of functions.
        - Not necessary, nor common that they are probabilistic distributions.
    - One factor $\phi^{(i)}$ per clique $\mathcal{C}^{(i)}$ in the graph.
    - `def` **Undirected models factorization**: $p(\textbf{x}) = \frac{1}{Z} \prod_i \phi^{(i)} (\mathcal{C}^{(i)})$.
        - Where $Z = \sum_{x \in \textbf{x}} \prod_i \phi^{(i)} (\mathcal{C}^{(i)})$ (very problematic to compute).
        - $i$ ranges between all the cliques (fully-connected graph subsets) in the graph.
        - The normalized product of all factors.

### Information Theory

- **Information Theory**:
    - Revolves around quantifying how much information is present in a signal.
    - Useful applications of IT for DL:
        - To characterize probability distributions.
        - To quantify similarity between probability distributions.
            - `eg` Minimize the distance to a goal distribution (the one that generate $Tr$ data).
    - **Quantifying information** (main intuition behind IT):
        - The quantity of information carried by a message depends on **how likely it is**.
            - Learning that an **unlikely event** has occurred is **more informative**.
        - The **less probable** an even is, the more surprising it is.
            - The more surprising it is the **more information** it yields.
            - An event with probability $100\%$ is **perfectly unsurprising and yields no information**.
        - **Independent events** should have **additive information**.

#### Shannon's Entropy Measure

- `def` **Shannon's Entropy Measure**: $H(\text{x}) = \mathbb{E}_{x \sim P}[I(x)] = - \sum_{x} P(x) \log P(x)$.
    - It captures the average amount of *information* across all possible outcomes of a random variable.
    - $\sum$ if the probability is discrete, otherwise $\int$.
    - `def` **Self-information**: $I(x) = - \log P(x)$.
        - Used to quantify the uncertainty of an event.
        - $- \log$ used to guarantee that the more surprising an event is the **more information** it yields.
    - `th` **Shannon's Source Coding Theorem**: $H(x)$ provides a lower bound for the average length of codewords in an optimal encoding of the possible values of $\text{x}$.
        - An optimal encoding for messages (with different probabilities) is needed.
        - Ideal: if a message is very frequent, a smaller code is assign to it.
            - Since it's very frequent, a smaller code is better to minimize communication.
    - The entropy of a random variable $\text{x} \sim \text{Bernoulli}(\phi)$ as $\phi$ varies from $0$ to $1$.
        - $0$ nats (**natural unit of information**) at $\phi = 0$ and $\phi = 1$, $0.7$ nats at $\phi = 0.5$ (upward parabola).
        - The quantity of information send with a message with $\phi = 0$ or $\phi = 1$ is $0$ (no information gained).
        - The entropy of a distribution is maximized for a uniform distribution.

#### Kullback-Leibler divergence

- `def` **Kullback-Leibler divergence**: $D_{KL} (P \| Q) = \mathbb{E}_{x \sim P} [ \log \frac{P(x)}{Q(x)} ] = \mathbb{E}_{x \sim P} [\log P(x) - \log Q(x)]$.
    - $P(\text{x})$ and $Q(\text{x})$ are two separate distribution over the same random variable $\text{x}$.
    - The KL is used to measure how much different are these distributions.
        - If two distributions are the same, there is no information loss $\implies D_{KL} = 0$.
        - If two distributions are very dissimilar, there will be a substantial information loss.
    - In the case of discrete variables:
        - It is the **extra amount of information** needed to send a message containing symbols drawn from $P$.
        - When it is used a code that was designed to minimize the length of message drawn from $Q$.
    - `prop` Properties of the KL divergence:
        - KL divergence is always **non-negative**.
        - KL divergence is $0 \iff P$ and $Q$ are the same distribution.
            - Or are equal *almost anywhere* in the case of continuous variables.
        - KL **is not a distance**.
            - A distance should also be symmetric and satisfy the triangle inequality.
            - The fact that KL is not symmetric has important consequences when the distance between $P$ and $Q$ has to be minimized.
            - Most of the times, minimizing $D_{KL}(p \| q)$ yields different output than minimizing $D_{KL}(q \| p)$.
    - `eg` An optimization problem is used to approximate a bimodal distribution with a unimodal gaussian distribution.
        - By optimizing $\min_q [D_{KL} (q \| p) = \min_q [\mathbb{E}_{x \sim q} [ \log \frac{q(x)}{p(x)} ]$:
            - $p$ is given and can't be modified, instead $q$ is *tuned* to approximate the target distribution.
            - The result is a narrow distribution that approximate only one of the twos modes.
        - By optimizing $\min_q [D_{KL} (p \| q) = \min_q [\mathbb{E}_{x \sim p} [ \log \frac{p(x)}{q(x)} ]$:
            - The result is a shallow distribution that approximate both of the two modes, but in a more inaccurate way.
        - It is possible to approximate by composing the best parts of both results.
        - By just chaning the order of KL argument, there are two very different solution to the optimization problem.

#### Cross-Entropy

- `def` **Cross-Entropy**: $H(P, Q) = H(P) + D_{KL}(P \| Q) = - \mathbb{E}_{x \sim P}[\log Q(x)]$.
    - The average number of bits needed to encode message for code $Q$ with a code designed for $P$.
    - Similar to $D_{KL} (P \| Q) = \mathbb{E}_{x \sim P} [\log P(x) - \log Q(x)]$.
        - Similar expression, but it lacks the term $\log P(x)$.
        - $D_{KL}$ measures the expected **extra** number of bits.
            - While $H$ measures the total number of bits.
    - Minimizing $H(P, Q)$ wrt $Q$ is the same as minimizing $D_{KL}(P \| Q)$.
        - $\min_q D_{KL} (P \| Q) = \min_q \mathbb{E}_{x \sim P} [\log P(x)] - \mathbb{E}_{x \sim P} [\log Q(x)]$.
        - Since minimization is wrt $Q$, the first element term doesn't contribute to anything.
        - The resulting term is exactly the cross-entropy.
