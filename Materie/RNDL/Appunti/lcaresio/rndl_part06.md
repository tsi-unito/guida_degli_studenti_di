---
title: 'Neural networks and Deep learning - Parte VI - Autoencoders'
---

# Neural networks and Deep learning - Parte VI

## Autoencoders

### Autoencoders

- **Autoencoders**:
    - Unsupervised learning tools used to **improve supervised networks**.
        - `eg` Image colorization, increase resolution, image inpainting, machine translation, etc. 
    - A NN trained to attempt to copy its input to its output via a representation $\textbf{h}$ built by its hidden layers.
        - Encoder: $\textbf{h} = f(\textbf{x})$.
        - Decoder: $\textbf{r} = g(\textbf{h})$.
        - Not expected to faithfully copy every input to its output.
            - Instead, they are forced to prioritize which aspects of the input should be preserved.
    - AE are a classic example of **self-supervised learning**, where the **supervised signal** is the input itself.
    - Traditionally used for dimensionality reduction or feature learning.
        - Today they are used for **generative models** due to connections established with **latent variable models**.
            - **Latent variable model**:
                - The model has both observed $\textbf{x}$ and latent variables $\textbf{h}$.
                - The goal is to learn the distribution $p(\textbf{x}, \textbf{h})$.
                - Similar to what happens in representation learning.
    - Goal:
        - Only copying the input to the output is not useful.
        - Instead, training the AE should result in $\textbf{h}$ taking on useful properties.
        - Approach: force $\textbf{h}$ to have a smaller dimension than $\textbf{x}$.

#### Dimensionality reduction

- **Dimensionality reduction**:
    - DR has been the first motivation to study autoencoders.
    - Learning AE that map input to lower dimensional output.
        - Many tasks can be more easily solved, and models of smaller spaces consume less memory and runtime.
        - It often places semantically related examples nearby, helping **generalization**.
    - **Semantic hashing**:
        - Store all database entires in a hash table mapping binary code vectors to entry.
        - The hash table allows retrieval of similar elements (they share the same binary code).
            - Swap single bits in the code to search for slightly less similar elements.
        - To produce these binary codes, one typically use sigmoid units in the final layer and train them to saturation.
            - By injecting additive noise just before the sigmoid.

#### Undercomplete Autoencoders 

- **Undercomplete Autoencoders**:
    - An autoencoder whose code dimension is less than the input dimension ($len(\textbf{x}) \gg len(\textbf{h})$).
    - The learning process for UAE is usually the minimization of a loss function, $L(\textbf{x}, g(f(\textbf{x}))$.
        - Where $L$ is a loss function penalizing $g(f(\textbf{x}))$ for being dissimilar from $\textbf{x}$ (`eg` MSE).
        - When the decoder is linear and $L$ is MSE, a UAE learn to span the same subspace as PCA.
            - PCA finds the mapping $f(\textbf{x}) = \text{arg min}_{\textbf{h}} \| \textbf{x} - g(\textbf{h}) \|_2$, imposing $g$ as a linear model.
            - When $L = \| \cdot \|_2$ and the reconstruction layer is modeled by linear units, the two approaches can solve the same problem.
        - Data to be reconstructed is assumed to lives on a linear manifold.
            - The best approach must be determined and why.
                - PCA is easier so when its requirements are granted.
                - An AE is better with a highly non-linear.

### Regularized Autoencoders

- **Regularized Autoencoders**:
    - The difference between $len(\textbf{x})$ and $len(\textbf{h})$ determines how much the AE is forced to learn only the *most relevant* part to the input.
        - If the size of $\textbf{h}$ is too big, the AE could simply learn the identity function.
    - RAE uses a loss function that encourages the model to have other properties besides I/O copying ability.
    - Properties exploited for regularization:
        - Sparsity of the representation $\to$ **Sparse autoencoders**.
        - Robustness to noise or missing inputs $\to$ **Denoising autoencoders**.
        - Smallness of the derivative $\to$ **Contractive autoencoders**.

#### Sparse Autoencoders

- **Sparse Autoencoders**:
    - The training criterion involves a sparsity penalty $\Omega(\textbf{h})$ on the code layer $\textbf{h}$, plus the reconstruction error.
        - $\Omega(\textbf{h}) = \lambda \| \textbf{h} \|_1 = \lambda \sum_i | h_i |$.
    - `def` **Sparse Autoencoders Loss**: $L(\textbf{x}, g(f(\textbf{x}))) + \Omega(\textbf{h})$.

##### Generative models

- **Generative models**:
    - Training a SAE, by minimizing reconstruction error plus a sparsity penalty, is approximately equivalent to maximizing the model's likelihood.
        - $\min L(\textbf{x}, g(f(\textbf{x}))) + \Omega(\textbf{h}) \approx \min - \log p_{model}(\textbf{x})$.
        - Then **sparsity penalty is not just a regularizer**.
            - It naturally arises from **modeling the joint distribution** over data and latent variables when a **Laplace prior** is assumed on the latent factors.
        - Adopting a probabilistic view reveals that **SAE are effectively learning a generative model**.
    - **Generative models**: a model that learns the joint distribution $p(\textbf{x}, \textbf{y})$ of the data.
        - After the model is trained, it can generate new data points of a given class by sampling from it.
        - Sometimes $\textbf{y}$ is not an observable variable.
            - $\textbf{y}$ is then called a **latent variable** and usually denoted by $\textbf{h}$.
        - GM differ from a **discriminative model**.
            - Since DM learn to approximate the conditional probability $p(\textbf{y} \mid \textbf{x})$.
            - A DM cannot therefore generate new data points.
    - Given a GM with visible variable $\textbf{x}$ and latent variables $\textbf{h}$.
        - $p_{model}(\textbf{h}, \textbf{x}) = p_{model}(\textbf{h}) \: p_{model}(\textbf{x} \mid \textbf{y})$.
        - The **likelihood** of $\textbf{x}$ given the model can be computed as the marginalization of the latent variables $\textbf{h}$.
            - **Likelihood**: $p_{model}(\textbf{x}) = \sum_{\textbf{h}} p_{model} (\textbf{h}, \textbf{x})$.
            - **Log-Likelihood**: $\log p_{model}(\textbf{x}) = \log \sum_{\textbf{h}} p_{model}(\textbf{h}, \textbf{x})$.
        - AE can be seen as approximating this sum with a **point estimate for just one highly likely value** for $\textbf{h}$.
            - By taking the logarithm of $p_{model}(\textbf{x})$ the log-likelihood of $\textbf{x}$ under the model is obtained.
                - $\log p_{model}(\textbf{x}) = \log \sum_{\textbf{x}} p_{model}(\textbf{h}, \textbf{x})$.
            - Given an $\tilde{\textbf{h}}$ generate by the decoder:
                - $\log p_{mod}(\textbf{x}) = \log \sum_{\textbf{h}} p_{mod} (\textbf{h}, \textbf{x}) \approx \log p_{mod} (\tilde{\textbf{h}}, \textbf{x}) = \log p_{mod} (\tilde{\textbf{h}}) + \log p_{mod} (\textbf{x} \mid \tilde{\textbf{h}})$.

###### Loss decomposition

- **Loss decomposition**:
    - The Loss can be divided in a **reconstruction loss** ($L(\textbf{x}, g(f(\textbf{x})))$) and a **sparsity loss** ($\Omega(\textbf{h})$).
    - **Reconstruction loss**:
        - By minimizing the reconstruction loss, the AE is learning to approximate the log-likelihood of the data. 
            - By minimizing it, the AE is learning to approximate the conditional distribution $p_{model}(\textbf{x} \mid \textbf{h})$.
            - It is trying to find the $\textbf{x}$ that is the best reconstruction given $\textbf{h}$.
                - That is, solving $\text{arg max}_{\textbf{x}} p_{model} (\textbf{x} \mid \textbf{h}) = \text{arg min}_{\textbf{x}} - \log p_{model} (\textbf{x} \mid \textbf{h})$.
                    - A minimization problem is preferred for optimization.
                    - While logarithms are preferred to avoid numerical issues thanks to their additivity.
    - **Sparsity loss**:
        - By setting $\Omega(\textbf{h}) = \lambda \sum_i | h_i |$ minimizing the sparsity term correspond to maximizing the log-likelihood of the $p(\textbf{h})$ term assuming a Laplace prior over each component of $\textbf{h}$ independently.
            - Using the $L_1$ norm of $\textbf{h}$ is known to **encourage sparsity**.
            - Since $h_i$ are independent, $p(\textbf{h}) = \prod P_{model}(h_i)$.
                - By taking the logarithms of it, $\prod$ turns into a $\sum$.
            - If $p_{mod}(h_i) = \frac{\lambda}{2} e^{- \lambda |h_i|}$ then $- \log p_{mod}(\textbf{h}) = \sum_i (\lambda | h_i | - \log \frac{\lambda}{2}) = \Omega(\textbf{h}) + \text{const}$.
                - $\lambda$ can be treated as an hyper-parameter.
                - $\text{const}$, depending only on $\lambda$, is not optimized during learning and can be ignored.

##### Training a SAE

- **Training a SAE**:
    - Training the network correspond to minimize the **negative log-likelihood of the data under the model**.
        - Obtained by the results related to the loss decomposition.
        - `def` **Sparse Autoencoder Training**: $\text{arg min}_{\theta} [- \log p_{model}(\textbf{x})] \approx \text{arg min} [- \log p(\textbf{x} \mid \tilde{\textbf{h}}) - \log p(\tilde{\textbf{h}})] \approx \text{arg min}_{\theta} [L(\textbf{x}, g(f(\textbf{x}))) + \Omega(\textbf{h})]$.
        - The sparsity penalty then is not a regularization term.
            - It's just the consequence of modeling the joint distribution taking into account the latent variables and assuming them to have a Laplace prior.
    - This view provides different motivations for training a SAE:
        - It is a way of approximately training a generative model (`eg` to generate new examples).
        - Learnt features are useful since they describe the latent variables that explain the input.

##### Generating new data with SAE

- **Generating new data with SAE**:
    - In principle, a learned model can be used to generate new data.
        - By sampling from the prior $p_{model}(\textbf{h})$ and then reconstructing $\textbf{x}$ using the decoder $g(\textbf{h})$.
    - During the training process, the decoder has only been trained to reconstruct data points close to $Tr$ examples.
        - In practice, by naively sampling from prior, it is likely to get $\textbf{h}$ values very far from the ones seen during training.
            - Yielding very poor samples.
    - SAE can be used to **generate new data points**, but one should **calibrate the sampling process**.
        - To ensure that the sampled $\textbf{h}$ values are close to the ones seen during training.

#### Denoising Autoencoders

- **Denoising Autoencoders**:
    - More in general, one can view both the encoder and the decoder as modeling some distribution. 
        - $p_{encoder}(\textbf{h} \mid \textbf{x}) = p_{model} (\textbf{h} \mid \textbf{x})$.
        - $p_{decoder}(\textbf{x} \mid \textbf{h}) = p_{model} (\textbf{x} \mid \textbf{h})$.
        - In general, the E and D distribution are not necessarily conditional distributions compatible with a unique joint distribution $p_{model}(\textbf{x}, \textbf{h})$.
            - Training E and D as a **denoising autoencoder** will tend to make them compatible asymptotically.
                - With enough capacity and examples.
                - DAE guarantees that those $p$ will be compatible, when previously it wasn't guaranteed.
    - Rather than adding a penalty $\Omega$ to the cost function, they changes the reconstruction error of the CF.
    - `def` **Denoising Autoencoders Loss**: $L(\textbf{x}, g(f(\tilde{\textbf{x}}))$.
        - Where $\tilde{\textbf{x}}$ is a copy of $\textbf{x}$ that has been corrupted by some form of noise.
        - DAE must therefore undo this corruption rather than simply copying their input.
    - It has been shown that denoising training forces $f$ and $g$ to implicitly learn the structure of $p_{data}(\textbf{x})$.

##### DAE Stochastic view

- **DAE Stochastic view**:
    - A corruption process $C(\tilde{\textbf{x}} \mid \textbf{x})$ that produces corrupted samples is introduced.
    - The AE then learn a **reconstruction distribution**.
        - `def` **Reconstruction distribution**: $p_{reconst}(\textbf{x} \mid \tilde{\textbf{x}}) = p_{decoder}(\textbf{x} \mid \textbf{h}) = g(\textbf{h})$ and $\textbf{h} = f(\tilde{\textbf{x}})$.
    - Typically, GD minimization on the negative log likelihood $- \log p_{decoder}(\textbf{x} \mid \textbf{h})$ can be performed.
        - As long as the encoder is deterministic, the whole AE can be **trained end-to-end using SGD**.
        - The DAE can be seen as performing SGD on $-\mathbb{E}_{\textbf{x} \sim p_{data}(\textbf{x})} \mathbb{E}_{\tilde{\textbf{x}} \sim C(\tilde{\textbf{x}} \mid \textbf{x})} \log p_{decoder} (\textbf{x} \mid \textbf{h} = f(\tilde{\textbf{x}}))$.

##### Manifolds

- **Manifolds**:
    - Many ML algorithms exploit the idea that data concentrates around **low-dimensional manifolds**.
        - Or a small set of such manifolds.
        - AE take this idea further and aim to learn the structure of the manifold.
    - DAE can be seen as **approximating a vector field**.
        - The corruption process moves the examples away from the lower dimensional manifold.
        - The AE is **learning to project back** these examples to the manifold.
            - The corrupted example is projected to the manifold, obtaining $\textbf{h}$.
            - $h$ can then be used to obtain the original example.
        - To assume that the lower dimensional manifold is *smooth* has been proved empirically right.
        - The DAE learns a **vector field that provides the fastest path to the manifold**.
    - **Manifold tangent planes**:
        - An important characterization of a manifold is the set of its tangent planes.
        - At a point $\textbf{x}$ on a $d$-dimensional manifold:
            - The tangent plane is given by $d$ basis vectors that span the local directions of variation allowed on the manifold.
        - These local directions specify how one can **change $\textbf{x}$ infinitesimally while staying on the manifold**.
    - AE and manifolds:
        - If the data generating distribution concentrates near a low-dimensional manifold, this yields representation that implicitly capture a local coordinate system for this manifold.
        - Only the variations tangent to the manifold around $\textbf{x}$ need to correspond to changes in $\textbf{h} = f(\textbf{x})$.
        - Hence the encoder learns a mapping from the input space $\textbf{x}$ to a representation space.
            - A mapping that is **only sensitive to changes along the manifold directions**.
            - But that is **insensitive to changes orthogonal to the manifold**.

#### Contractive Autoencoders

- **Contractive Autoencoders**: 
    - `def` **Contractive Autoencoders Loss**: $L(\textbf{x}, g(f(\textbf{x}))) + \lambda \sum_i \| \nabla_{\textbf{x}} h_i \|^2$.
        - As in SAE, optimize a regularized objective $L(\textbf{x}, g(f(\textbf{x}))) + \Omega(\textbf{h}, \textbf{x})$, but with a different form of $\Omega$.
        - This force the model to learn a function that does not change much when $\textbf{x}$ changes only slightly.
        - With a very high $\lambda$, a constant plane is obtained by minimizing the derivative.
    - As a side effect of the contractive penalty, the encoder learn to map a **neighborhood of inputs** onto a **smaller neighborhood of outputs**.
        - Without any other competing force (or with a very high $\lambda$) the penalty would drive $f$ to be learnt as a constant function.
            - Which maps the whole input space onto a single point.
    - CAE is **contractive only locally**.
        - If $\textbf{x}$ and $\textbf{x}'$ are different enough, they can be mapped to point very far apart than the original points.
    - In the limit of small Gaussian input noise, the **denoising reconstruction** error is equivalent to a **contractive penalty** on the **reconstruction function**.
        - DAE make the **reconstruction function** to resist to small errors in the input (decoder part).
        - CAE make the **feature extraction function** to resist small perturbations of the input (encoder part).

##### Learning manifolds

- **Learning manifolds**:
    - Regularized AE learns manifolds by balancing two opposing forces.
        - In case of CAE, these two forces are reconstruction error and the contractive penalty $\Omega(\textbf{h})$.
    - The compromise between these two forces yields an AE whose derivatives $\frac{\partial f(\textbf{x})}{\partial \textbf{x}}$ are mostly tiny.
        - Only a **small number of hidden units** (corresponding to a small number of directions in the input) may have **significant derivatives**.
    - Directions $\textbf{x}$ with large $\textbf{Jx}$ rapidly change $\textbf{h}$.
        - **Such directions are penalized during learning** (due to contractive penalty).
    - So the ones *surviving* are those really necessary to model the data.
        - And hence they are likely to to be directions which **approximate the tangent planes of the manifold**.
            - By loosing those directions, the input cannot be reconstructed, so those are kept.

### Variational Autoencoders

- **Variational Autoencoders**:
    - An AE whose **encodings distribution is regularised** during the training.
        - In order to ensure that its latent space has **good properties allowing to generate** some new data.
        - The regularization method is based on *variational inference*, an inference method popular in statistics.
    - AE are not good generative models.
        - To do generation:
            - After training the encoder is discarded.
            - A random $\textbf{h}$ is picked and served to the decoder.
            - The decoder will then generate an output.
        - While this simple schema is intuitive, it does not work well in practice.
    - In seen AE, encoding networks are not regularized.
        - The semantic *areas* are concentrated and well-spaced.
        - When an example (`eg` the randomized one) that is not nearby any of those area, will return a meaningless output.
        - Encoding networks need to be **regularized**.
            - Larger semantic *areas* all nearby between each other.
    - To impose this kind of regularization, images are **encoded into a distribution**.
        - During training, the decoder is trained to decode points sampled from these distributions.

#### Probabilistic model

- **Probabilistic model**:
    - A probabilistic model where a latent variable $\textbf{h}$ is sampled from a distribution $p(\textbf{h})$.
        - And then $\textbf{x}$ is sampled from $p(\textbf{x} \mid \textbf{h})$.
    - It is also assumed:
        - $p(\textbf{h}) \sim N(\textbf{0}, \textbf{I})$.
        - $p(\textbf{x} \mid \textbf{h}) \sim N(g(\textbf{h}), c \textbf{I})$ (centered around the reconstruction, isotropic distribution).
    - Even under these assumptions, the inference problem is **intractable** due to the normalization factor in the denominator.
        - $p(\textbf{h} \mid \textbf{x}) = \frac{p(\textbf{x} \mid \textbf{h}) p(\textbf{x})}{p(\textbf{x})} = \frac{p(\textbf{x} \mid \textbf{h}) p(\textbf{x})}{\int p(\textbf{x} \mid \textbf{u}) p(\textbf{u}) \: d \textbf{u}}$.
            - $p(\textbf{x})$ is unknown, it's therefore approximated with an integral (that is also unknown, intractable).
        - Variational inference is a technique allowing to solve this problem.
            - By **approximating the hard to compute probability with a simpler one**.
            - Specifically, a distribution $q_{\textbf{x}}(\textbf{h}) \sim N(f_1(\textbf{x}), f_2(\textbf{x})) \approx p(\textbf{h} \mid \textbf{x})$ is used.
                - $p(\textbf{h} \mid \textbf{x})$ is approximated using a Gaussian.
    - *Goal*: to find $f_1$ and $f_2$ allowing the best possible approximation of $p(\textbf{h} \mid \textbf{x})$.
        - $\text{arg min}_{f_1, f_2} \: KL(q_\textbf{x}(\textbf{h}) \: \| \: p(\textbf{h} \mid \textbf{x})) = \dots = \text{arg max}_{f_1, f_2} E_{\textbf{h} \sim q_{\textbf{x}}} [ - \frac{\| \textbf{x} - g(\textbf{h}) \|^2}{2c}] - KL (q_{\textbf{x}}(\textbf{h}) \: \| \: p(\textbf{h}))$.
    - *Goal*: to find the decoder function $g$ that maximises the likelihood of $p(\textbf{x} \mid \textbf{h})$ under assumption $\textbf{h} \sim q_{\textbf{x}}(\textbf{h})$.
        - `def` **VAE optimization problem**: $\text{arg min}_{f_1, f_2, g} E_{\textbf{h} \sim q_{\textbf{x}}} [ \frac{\| \textbf{x} - g(\textbf{h}) \|^2}{2c}] + KL (q_{\textbf{x}}(\textbf{h}) \: \| \: p(\textbf{h}))$.
            - With $q_{\textbf{x}}(\textbf{h}) \sim \mathcal{N}(f_1(\textbf{x}), f_2(\textbf{x})) \equiv \mathcal{N}(\mu_{\textbf{x}}, \sigma_{\textbf{x}})$ and $p(\textbf{h}) \sim \mathcal{N}(\textbf{0}, \textbf{I})$.
            - It has the form a **regularized network** (with a reconstruction term and a regularization term).
            - Easily solvable via GD.

##### Reparametrization trick

- **Reparametrization trick**:
    - While optimization is easily done via GD, the extraction of a point in the embedding space is **not differentiable**.
        - Randomly picking a point from a distribution is not a differentiable operation.
    - Sampling from $N(\mu_{\textbf{x}}, \sigma_{\textbf{x}})$ is equivalent to sampling $\zeta \sim N(\textbf{0}, \textbf{I})$ and then $\textbf{h} = \sigma_{\textbf{x}} + \zeta \mu_\textbf{x}$.
        - Points are rescaled and shifted (standard translation from standard Gaussian to other normal distributions).
        - A point is not extracted from $N(\mu_{\textbf{x}}, \sigma_{\textbf{x}})$, which depends on $\mu_{\textbf{x}}$ an $\sigma_{\textbf{x}}$, which are part of the network.
            - But is extracted from $N(\textbf{0}, \textbf{I})$, which contains only constant terms.
            - Then $\textbf{h}$ is computed as described.
    - During BP, $\mu_{\textbf{x}}$ and $\sigma_{\textbf{x}}$ are then easily reachable from $\textbf{h}$.
        - Derivatives to $\zeta$ (where sampling is performed) are not needed.
        - Optimization is only needed for part of the NN which construct $\mu_{\textbf{x}}$ and $\sigma_{\textbf{x}}$.

#### VAE Loss

- `def` **VAE Loss**: $C \| \textbf{x} - g (\textbf{h}) \|^2 + KL(N(\mu_\textbf{x}, \sigma_\textbf{x}) \: \| \: N(\textbf{0}, \textbf{I}))$.
    - It can be shown that $KL(N(\mu_\textbf{x}, \sigma_\textbf{x}) \: \| \| N(\textbf{0}, \textbf{I})) = \frac{1}{2} [ -1 - \log(\sigma_\textbf{x}^2) + \sigma_\textbf{x}^2 + \mu_\textbf{x}^2]$.
        - Where $\mu_\textbf{x} = f_1(\textbf{x})$ and $\sigma_\textbf{x} = f_2(\textbf{x})$.
