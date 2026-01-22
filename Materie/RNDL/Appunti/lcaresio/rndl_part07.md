---
title: 'Neural networks and Deep learning - Parte VII - Generative Adversarial Networks'
---

# Neural networks and Deep learning - Parte VII

## Generative Adversarial Networks

### Generative models

- **Generative models**:
    - A systems that:
        - Take a training set of samples drawn from a distribution $p_{data}$.
        - Learn to represent and estimate of that distribution.
    - To learn $p(\textbf{x})$ is way more difficult that to learn $p(y \mid \textbf{x})$ (discriminative models).
    - The distribution $p_{model}$ can be estimated explicitly.
        - Or the model can only give the possibility to draw samples from it.
        - GANs are usually used to draw examples even if they can be designed to do both.
    - Advantages of studying generative models:
        - The ability to **represent high-dimensionality** is important in many domains.
        - GM can be incorporated in (model based) **reinforcement learning**.
            - The GM model can be *queried* by the RL system to validate assumptions.
        - They can be used as the **basis of Semi Supervised Learning** systems, etc.

#### Maximum likelihood models

- **Maximum likelihood models**:
    - GANs are compared to **maximum likelihood models** (a type of generative models).
    - `def` **Maximum Likelihood Optimization**: $\theta^{\star} = arg \: max_{\theta} \: p_{model}(\{ \textbf{x}^{(i)} \}_{i=1}^m; \theta)$.
        - Parameters $\theta$ that maximize the likelihood of the training data given the model.
        - Almost always that optimization is made in **log-space**.
            - $\theta^{\star} = arg \: max_{\theta} \: p_{model}(\{ \textbf{x}^{(i)} \}_{i=1}^m; \theta) \approx arg \: max_{\theta} \: \mathbb{E}_{\textbf{x} \sim p_{data}} [\log p_{model}(\textbf{x}; \theta)]$.
        - **Maximizing** the ML w.r.t. $\theta$ is the same as **minimizing** the KL divergence of $p_{data}$ and $p_{model}$.
            - $KL(p_{data} \| p_{model}) = \dots = \mathbb{E}_{\textbf{x} \sim p_{data}} [\log p_{data}(\textbf{x})] - \mathbb{E}_{\textbf{x} \sim p_{data}} [\log p_{model}(\textbf{x}; \theta)]$.
                - $\mathbb{E}_{\textbf{x} \sim p_{data}} [\log p_{data}(\textbf{x})]$ is the log-likelihood of data.
                - $\mathbb{E}_{\textbf{x} \sim p_{data}} [\log p_{model}(\textbf{x}; \theta)]$ is the log-likelihood of the model.
            - Since the minimization is w.r.t. to $\theta$, only the second element must be maximized.
    - Maximum likelihood models can be subdivided into ones with *explicit density* and ones with *implicit density*.
        - Explicit density can be subdivided into tractable density and approximate.

##### Explicit Density models with tractable density

- **Explicit Density models with tractable density**:
    - The distribution is fully modelled.
    - **Fully Visible Belief Nets**:
        - Strong assumption: the distribution is tractable.
        - Chain rule of probability to factor the probability of model $\textbf{x}$ into a product of $1D$ probabilities.
            - $p_{model}(\textbf{x}) = \prod_{i=1}^n p_{model} (x_i \mid x_1, \dots, x_{i-1})$.
        - They are the basis of sophisticated generative models.
        - *Problem*: sample must be generated one entry at a time $\to$ generating a new sample is $O(n)$.
    - **Nonlinear Independent Component Analysis**:
        - A **simple distribution** over $\textbf{z}$ coupled with a **non-linear transformation** $g$ that warps space in complicated ways can yield a **complicated distribution** over $\textbf{x}$.
        - If there is a vector of latent variables $\textbf{z}$ and a continuous differentiable, invertible transformation $g$ such that $g(\textbf{z})$ yields a sample from the model in $\textbf{x}$ space, then:
            - $p_{\textbf{x}}(x) = p_{\textbf{z}}(g^{-1}(x)) | \det (\frac{\partial g^{-1}(x)}{\partial \textbf{x}})| = p_{\textbf{z}}(g^{-1}(x)) | \det (\textbf{J}_{\textbf{x}} (g^{-1}(x)))|$.
        - The density $p_{\textbf{x}}$ is tractable if the density of $p_{\textbf{z}}$ is tractable and the Jacobian of $g^{-1}$ is tractable.
        - *Problem*: these models impose constaints on the choice of $g$.
            - The invertibility restriction impose that the number dimensions of $\textbf{x}$ is equals to the ones of $\textbf{z}$.

##### Explicit Density models with approximate density

- **Explicit Density models with approximate density**:
    - The distribution is approximated.
    - **Variational approximations**:
        - Use a deterministic approximation to overcome the problems of having to deal with an intractable distribution.
        - The main idea is to define and maximize a lower bound on the intractable distribution.
            - $\mathcal{L}(\textbf{x}; \theta) \leq \log p_{model}(\textbf{x}; \theta)$.
            - This is the kind of approach taken by **variational autoencoder**.
        - Very often the approximation is based on multivariate gaussians.
            - VAE learns make several approximation to the true likelihood using Gaussian distribution.
        - *Drawback*:
            - When the approximation is too crude even with a perfect optimization and infinite data the gap between $\mathcal{L}$ and the true likelihood can make the results poor.
            - VAE often obtain very good likelihood, but produce lower quality samples (w.r.t. GANs).
            - If compared to FVBNs, VAE are hard to train (but GANs are even harder to train).
    - **Markov Chain Approximations**:
        - Use some form of stochastic approximation.
        - Goal: to find a way to efficiently draw samples from $p_{model}(\textbf{x})$ when the distribution is intractable.
        - **Markov Chain** method (*MC Monte Carlo*) draw examples by repeatedly sampling from simpler distributions.
            - According to a transition operator: $\textbf{x}' \sim q(\textbf{x}' \mid \textbf{x})$.
        - Convergence:
            - By repeating updating $\textbf{x}$ according to the transition operator $q$, MC methods can sometimes guarantee that $\textbf{x}$ will eventually converge to sampling from $p_{model}(\textbf{x})$.
        - *Challenge*: **slow convergence**.
            - *Learning phase*: drawing of examples via MCMC can be too costly.
            - *Inference phase*: inefficient if compared to GANs.

##### Implicit Density models

- **Implicit Density models**:
    - Some models model the distribution only implicitly.
    - **Generative stochastic models**:
        - Learns a Markov transition operator that allows to draw from the implicit model.
        - *Problem*:
            - By approximating examples using a MC, these models have the same problems as MCMC.

### Generative Adversarial Networks

- **Generative Adversarial Networks** (GANs):
    - GANs model the distribution only implicitly.
        - The distribution is implicit but can be used to generates sample.
        - Offer a single step sample generation method.
    - Features:
        - GANs can generate samples in parallel.
        - The generator function has very few restrictions.
        - No costly Markov chain approximations.
        - No variational bound is needed.
        - Subjectively GANS are regarded to produce better samples.

#### GANs functionality

- **GANs functionality**:
    - The idea is to set up a game between two players.
        - **Generator** ($G$): create samples intended to come from the same distribution as the training data.
        - **Discriminator** ($D$): examines samples to determine whether they are real or false.
    - Definitions:
        - $G$ and $D$ are two differentiable functions (i.e. two NNs).
        - $G$ takes an input $\textbf{z}$, is defined in terms of parameters $\theta_G$ and outputs a value $\tilde{x}$ from same space of $\textbf{x}$.
        - $D$ takes an input $\textbf{x}$, is defined in terms of parameters $\theta_D$ and outputs a value $y \in \{False, Real\}$.
    - Assumption: the discriminator tries to predict the probability that the input is real (regression).
        - $D(\textbf{x}) \approx 1$ if the discriminator believes that the $P(y = Real \mid \textbf{x})$ is high.
    - **Cost functions**: $L_G(\theta_G, \theta_D)$ and $L_D(\theta_G, \theta_D)$.
        - `!` They are both defined with respect to both $\theta_G$ and $\theta_D$.
    - Each player want to minimize its cost function but can only do so by **acting on its own parameters**.
        - The optimization can be seen as a zero-sum game between two players.
            - Not always true, depending on the definition of $L_G$ and $L_D$.
            - The solution of a zero-sum is a **Nash equilibrium**.
                - A point in the $(\theta_G, \theta_D)$ space such that the point is local minimum of $L_D$ w.r.t. $\theta_D$ and a local minimum of $L_G$ w.r.t. $\theta_G$.
            - Nowadays, constraints for having a zero-sum game are abandoned for easier training.

#### GANs training

- **GANs training**:
    - The training process consists of simultaneous SGD.
        - Sample two minibatches (one from real data, one from generated data). 
        - Evaluate the two losses and update $\theta_D$ using the gradients from $L_D$ and $\theta_G$ using the ones from $L_G$.
    - Sometimes more steps are performed on the discriminator before going back to the generator.

##### Discriminator's Cost function

- `def` **Discriminator's Cost function**: $L_D(\theta_D, \theta_G) = - \frac{1}{2} \mathbb{E}_{\textbf{x} \sim p_{data}} [\log D(\textbf{x})] - \frac{1}{2} \mathbb{E}_{\textbf{z}} [\log (1 - D(G(\textbf{z})))]$.
    - The usual cross-entropy used when minimizing a binary classifier with sigmoid output units.
    - Initially, all variants of GANs used this exact cost for the discriminator.
        - While, costs for the generator changes from model to model.
    - $\frac{1}{2}$ is used since usually $50\%$ of examples are real and $50\%$ examples are not.
        - But since it is an optimization problem, constants are not impactful.
    - The distribution of $\textbf{z}$ is usually a Gaussian distribution.

##### Generator's Cost function

- **Generator's Cost function**:
    - `def` **Generator's Cost function (zero-sum game)**: $L_G = - L_D$.
        - Each time the discriminator lower its loss, the generator is penalized by the same amount.
        - The players are just trying to minimize/maximize (**minmax**) the same function.
            - $\theta_{G^\star}, \theta_{D^\star} = arg \: min_{\theta_G} \: max_{\theta_D} V(\theta_D, \theta_G)$, with $V(\theta_D, \theta_G) = - L_D(\theta_D, \theta_G)$.
            - A **saddle (or minmax) point** is searched in the loss surface.
                - The same loss surface is evaluated, but from two different perspectives.
                - At a relative minimum along one axial direction and at a relative maximum along the crossing axis. 
        - Theoretical advantages of this setup:
            - This game corresponds to minimizing the **Jensen-Shannon divergence** between the data and the model distribution.
            - `def` **Jensen-Shannon divergece**: $JSD(P \| Q) = \frac{1}{2} KL(P \| M) + \frac{1}{2} KL(Q \| M)$.
                - Where $M$ is the median distribution $M = \frac{1}{2} (P + Q)$.
                - The $JSD$ is a symmetrical type of $KL$.
            - The game **converge to equilibrium** if both players' policies can be updated in **function space**.
                - This is not possible in practice though.
        - When the **discriminator minimizes** the cross-entropy, the generator **maximizes** it.
            - The gradient of the loss **vanishes** when the discriminator rejects the generator examples with high confidence.
    - `def` **Generator's Cost function (non-saturating game)**: $L_G = \frac{1}{2} \mathbb{E}_{\textbf{z}} \log D(G(\textbf{z})$.
        - The generator minimize a cross-entropy term tailored to its view of the problem.
            - The generator maximizes the probability that the discriminator is mistaken.
        - It lacks the theoretical properties of the minmax loss, but it doesn't suffer from gradient vanishing.
            - The generator loss has been chosen **heuristically** (not theoretically sound, it *just works*).
            - The game is no longer a zero-sum one.

#### Why do GANs work

- **Why do GANs work**:
    - GANs performances were attributed to the minimization of $JSD$ instead of $KL$.
        - $KL$ is not symmetric and MLE minimizes $KL(P_{data} \| p_{model})$.
        - Minimizing the $JSD$ is more akin to minimizing $KL(p_{model} \| p_{data})$.
    - More recent results suggest that $JSD$ doesn't explain why GANs make sharper samples.
        - Using ML to optimize GANs doesn't show problems in selecting a small number of modes and generating sharp images.
            - By selecting a single mode (corresponding to an image) the result won't be the average of several modes.
                - Averaging several modes results in a *blurrier* image.
        - GANs often choose to generate from very few modes.
            - Fewer than the number allowed by the model capacity.
            - **Model collapse problem**: generating always the same image.
            - Reverse $KL$ would select as many modes as allowed by the model.
    - This suggests GANs choose to generate a small number of modes due to a **defect in the training procedure**.
        - Rather than due to the divergence they aim to minimize.
        - The reason why this happens is still not clear.
            - Maybe it **makes different approximations** than other models.
            - Maybe it **optimizes a different family of functions**.

#### Deep Convolutional GANs

- **Deep Convolutional GANs**:
    - Nowadays, most GANS are loosely based on DCGANs.
    - Main insights for this architecture are:
        - **Batch normalization**:
            - Most layers for both $D$ and $G$, batches for $G$ and $D$ are normalized separately.
            - The last layer of $G$ and the first layer of $D$ are not batch normalized.
        - **No pooling nor unpooling layers**:
            - When $G$ needs to increase the spatial dimension of the representation it used *transposed convolution* (*deconvolution*) with a stride greater than $1$.
        - **Adam instead of SGD with momentum**.

### Wasserstein GANs

- **Wasserstein GANs**:
    - GANs are very difficult to train.
        - The game between $G$ and $D$ doesn't converge easily.
    - `def` **Wasserstein distance** (*earth-mover distance*): $W(p, q) = \inf_{\gamma \in \Pi(p, q)} \mathbb{E}_{(x, y) \sim \gamma} \|x - y \|$.
        - A distance between two probability distributions $p$ and $q$.
        - $\Pi(P, Q)$: the set of all joint distribution $\gamma(x, y)$ whose marginals are respectively $p$ and $q$.
        - $(x, y) \sim \gamma$: $(x, y)$ distributes according to $\gamma$.
        - Interpretable as the **minimum amount of *work*** required to transform the probability mass $p$ to $q$.
    - $W$ has better properties than other distances and divergences used for GANs.
        - It allows **learning to convergence** in many situations when other measures fail.
        - But the infimum in the definition of $W$ is **intractable**.

#### Lipschitz continuity

- **Lipschitz continuity**:
    - A *solution* to the intractability of $W$.
    - **WGANs**: the family of functions considered $\{f_{\theta_D}(x)\}$ is **assumed to be Lipschitz continuous**.
        - `def` **Lipschitz continuous function**: $f$ if $\exists c \colon \|f(x) - f(y) \| \leq c \| x - y \|$ for all $x$ and $y$ in $dom(f)$.
            - The smaller the constant, the smoother the function.
            - The function cannot change too much in a small region of the domain.
    - Under this assumption:
        - **Discriminator training**: maximize $W$ by maximizing $\mathbb{E}_{\textbf{x} \sim p_{data}} [D_{\theta_D}(\textbf{x})] - \mathbb{E}_{\textbf{z}} [D_{\theta_D}(G_{\theta_G}(\textbf{z}))]$.
            - $\mathbb{E}_{\textbf{x} \sim p_{data}} [D_{\theta_D}(\textbf{x})]$: the average score for real examples.
            - $\mathbb{E}_{\textbf{z}} [D_{\theta_D}(G_{\theta_G}(\textbf{z}))]$: the average score for fake examples.
        - **Generator training**: minimize $L_G(\theta_G, \theta_D) = - \mathbb{E}_{\textbf{z}} [D_{\theta_D}(G_{\theta_G}(\textbf{z}))]$.
        - Those quantities are easy to compute (therefore $W$ **intractability is avoided**).
    - Everything can be trained by BP with a small trick to ensure that the learned function is Lipschitz continuous.
        - Every time $\theta_D$ parameters are updated, they are **clipped** in $[-c, c]$ (where $c$ is a user-defined constant).
            - `eg` With $[-5, 5]$, both $14$ and $16$ are clipped to $5$, while $-4.2$ is left as it is (no normalization).
    - Another difference is the activation function of the last layer of $D$ is **linear** (instead of sigmoid).
        - $D$ is no longer meant to model a probability distribution (there $D$ is often referred as the **critic**).

#### WGANs advantages

- **WGANs advantages**:
    - The loss is **meaningful** and **correlates** with $G$ **convergence** and sample quality.
        - Other losses do not correlate with sample quality.
        - In these cases, with a smaller loss the sample quality is still low or even worse than before.
    - The discriminator $f$ is usually trained near optimality.
        - Way more epochs are assigned to the discriminator.
            - When $D$ is properly trained, then $G$ is trained further (for a small amount of epochs).
        - It can be shown that the loss is an **estimate** of $W$ **given a factor** (determined by the constant $c$).
        - Empirical evidence shows that this correlates with the **quality of generated samples**.
        - The discriminator **should** be trained till optimality.
            - When the critic is trained to completion, it simply provides a loss to the generator that can be trained as any other NN.
    - Empirical evidence shows that WGANs are **much more robust** than GANs when one varies the generator.
