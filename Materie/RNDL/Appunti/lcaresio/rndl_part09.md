---
title: 'Neural networks and Deep learning - Parte IX - Transformers'
---

# Neural networks and Deep learning - Parte IX

## Transformers

### Introduction to transformers

- **Introduction to transformers**:
    - The main idea behind LLM is:
        - To have a **large model**.
        - Trained on a **large dataset**.
        - To **predict the next token in a sequence**.
    - They unlocked **efficient parallel processing** and the ability to **model long-range dependencies**.
    - Before Transformers, the main tools were RNNs and LSTMs.
        - These models have some limitations.
            - They are **sequential** and cannot be easily parallelized.
            - They have difficulties in capturing **long-range dependencies**.

#### Foundation models

>**Scaling hypothesis**: performance improves smoothly as we increase the **model** size, **dataset** size, and amount of **compute** used for training. For optimal performance **all three factors must be scaled up in tandem**.

- **Foundation models**:
    - Large models that are trained on a wide range of tasks.
        - And can be **fine-tuned** for specific tasks.
    - Transformers make them possible because of the following reasons:
        - They are **scalable** and can be trained on very large datasets.
            - Exploiting **parallelism** and distributed computing.
        - They can be trained in a **self-supervised** way (no need for labeled data).
        - The **scaling hypothesis** asserts that:
            - Simply by increasing the scale of the model (as measured by the number of parameters)
            - And training on a commensurately large dataset.
            - Significant improvements can be achieved, even with no architectural changes.
        - Performance has a power-law relationship with each of the three scale factors.

### Attention

- **Attention**:
    - A mechanism that allows a model to focus on different parts of the input when making predictions.
    - Originally introduced as an enhancement to RNNs for machine translation.
        - Later showed that significantly better results can be achieved using attention mechanism alone.
            - **Eliminating the recurrence mechanism** completely.
    - A transformer can be viewed as a way to build a **richer form of embedding**.
        - In which a given vector is mapped to a location that depends on the other vectors in the sequence.
        - While previous NN architectures (once trained) are fixed on word orders.
    - Why attention work better than recurrence:
        - Attention layers let **every token interact with every other token in one step**.
            - Whereas recurrent models process tokens **one at a time**.
        - This remove the sequential bottleneck, enabling **massive parallelization**.
            - And helping capture **long-range dependencies** more effectively.

#### Transformer processing

- **Transformer processing**:
    - Input data to a transformer is a sequence of vector $[x_n^T]_{n \in [N]}$ of dimensionality $D$.
        - Each vector is called a **token** (`eg` a word in a sentence, a patch within an image, etc).
        - Tokens are collected in a matrix $\textbf{X} \in \mathbb{R}^{N \times D}$, which is the input to the transformer.
    - The fundamental building block of a transformer is the **transformer layer**.
        - A function that takes $\textbf{X} \in \mathbb{R}^{N \times D}$ as input and produces a matrix $\tilde{\textbf{X}} \in \mathbb{R}^{N \times D}$ as output.
            - $\tilde{\textbf{X}} = \text{TransformerLayer}[\textbf{X}]$.
        - The transformer layer is composed by two blocks: the **attention** block and the **transform** block.

#### Attention weights

- **Attention weights**:
    - Assume to want to compute new embeddings $\textbf{y}_1, \dots, \textbf{y}_N$ for tokens $\textbf{x}_1, \dots, \textbf{x}_N$.
        - In such a way that the embedding for $\textbf{y}_n$ depends on the embeddings of all other token.
            - Instead of moving information *left-to-right* (as in RNNs), everything is connected.
        - $\textbf{y}_n = \sum_{m=1}^N a_{nm} \textbf{x}_m$ (where $a_{nm}$ are the **attention weights**).
    - Requirements for attention weights:
        - Capture the similarity between the tokens $\textbf{x}_n$ and $\textbf{x}_m$.
        - $a_{nm} \geq 0$.
        - $\sum_{m=1}^N a_{nm} = 1$ (to form a distribution).
    - `def` **Dot-product Self-attention**: $a_{nm} = \frac{\exp(\textbf{x}_n^T \textbf{x}_m)}{\sum_{m'=1}^N \exp(\textbf{x}_n^T \textbf{x}_m)}$.
        - **Similarity** is computed via **dot-product** (common method).
        - It computes the $a$ between the tokens in the same sequence using dot-product.
        - It uses **softmax** to form a distribution.
            - It takes the exponential and then normalize the result to sum to one.
        - With unrelated $\textbf{x}_n$ and $\textbf{x}_m$ (i.e. orthogonal), the attention weights will be $\approx 0$. 
    - `def` **Dot-product Self-attention** (matrix version): $\textbf{Y} = Softmax[\textbf{XX}^T]\textbf{X}$.
        - Fast way to compute all the attention weights at once.

#### Self-attention

- **Self-attention**:
    - **Query, key and value**:
        - **Query**: the *user* request to get the value.
        - **Key**: the information that the system uses to accompany the value and that should be match with query.
        - **Value**: the information that should be returned when the query match the key.
    - **Soft-attention**:
        - Continuous variables are used to measure the degree of match between the query and the keys.
        - These variables are used to weight the influence of the value vectors on the output.
    - Question: given an embedding of a token $\textbf{x}_n$, to which information should this token attend to compute its new embedding and how much.
        - $\textbf{x}_n$ can be used to produce a **query**, and each $\textbf{x}_m$ in the sequence can be seen as giving rise to:
            - A **key** than can be matched against the **query** $\textbf{x}_n$, to get a sense of how similar they are.
            - A **value** that can be used to compute how much influence token $\textbf{x}_m$ should have on the new embedding of $\textbf{x}_n$.
    - `def` **Self-attention** (matrix version): $\textbf{Y} = Softmax[\textbf{XX}^T]\textbf{X} = Softmax[\textbf{QK}^T]\textbf{V}$.
        - **Value**: the token $\textbf{x}_n$ that will be used to compute output tokens.
        - **Key**: the token $\textbf{x}_n$ for value $\textbf{x}_n$.
        - **Query**: the token $\textbf{x}_n$ that will be used to compute the attention weights for output $\textbf{y}_n$.
        - Self-attention since the **same sequence** is used to determine **all three components**.
        - $\textbf{Q}$, $\textbf{K}$ and $\textbf{V}$ are actually three different linear projections of $\textbf{X}$.
            - These projections allow the model to learn **separate similarity spaces** for queries, keys and values.

##### Trainable parameters

- **Trainable parameters**:
    - The given formula is **deterministic** and doesn't depend on the parameters model (not trainable).
    - Each feature with token $\textbf{x}_n$ contributes equally to the attention weights.
        - Whereas the flexibility to focus more on some features than on others is desirable. 
    - **Trainable parameters** are introduced to compute the attention weights, $\tilde{\textbf{X}} = \textbf{XU}$.
        - Where $\textbf{U} \in \mathbb{R}^{D \times D}$ is a matrix of trainable parameters.
            - Analogous to a layer in a standard NN.
        - Therefore the formula for new embeddings is $\textbf{Y} = Softmax[\textbf{XUU}^T \textbf{X}^T]\textbf{XU}$.
            - But the matrix $\textbf{XUU}^T \textbf{X}^T$ is **symmetric** (while significant asymmetries should be supported).
                - `eg` *Chisel* should be strongly associated with *tool* (since every chisel is a tool). 
                    - But *tool* should only be weakly associated with *chisel* (not every tool is a chisel).
        - Also, the **same matrix** $\textbf{U}$ is used to define both value vectors and attention coefficients (not ideal).
            - Therefore different separate matrices are defined for queries, keys and value.
    - **Trainable parameters**:
        - $\textbf{Q} = \textbf{XW}^{(q)}$.
            - With dimensionality $D \times D_k$ (where $D_k$ is the dimensionality of the **key** vectors).
        - $\textbf{K} = \textbf{XW}^{(k)}$.
            - With dimensionality $D \times D_k$, so that the dot product with **query** vectors $\textbf{QK}^T$ is well defined.
        - $\textbf{V} = \textbf{XW}^{(v)}$.
            - With dimensionality $D \times D_v$ (where $D_v$ is the dimensionality of the output **value** vectors).
    - `def` **Self-attention**: $\textbf{Y} = Softmax[\textbf{QK}^T]\textbf{V}$.

##### Scaled self-attention

- **Scaled self-attention**:
    - The gradient of the softmax function can become exponentially small for inputs of high magnitude.
        - Which can lead to **vanishing gradients** during training.
    - The values of the dot product can be **scaled** by a factor $\sqrt{D}$.
        - Where $D$ is the dimensionality of the key vectors.
    - `def` **Scaled self-attention**: $\textbf{Y} = Attention(\textbf{Q}, \textbf{K}, \textbf{V}) = Softmax[\frac{\textbf{QK}^T}{\sqrt{D_k}}]\textbf{V}$.
    - **Choice of the scaling factor** $\sqrt{D_k}$:
        - $\textbf{q}$ and $\textbf{k}$ vectors are assumed to be independent and have zero mean and unit variance.
        - $D_k$ is then exactly the **standard deviation** of each one of the dot products in $\textbf{QK}^T$.
        - The standard deviation of $\textbf{q}^T \textbf{k}$ matters since:
            - **Softmax saturates for large-magnitude inputs**.
                - Very large or very small logits entering the softmax leads to **vanishing gradients**.
            - The **typical magnitude** of $\textbf{q}^T \textbf{k}$ **grows as** $O(\sqrt{D_k})$.
                - Since $\text{std}(\textbf{q}^T \textbf{k}) = O(\sqrt{D_k})$, increasing the dimensionality of key/query vectors makes the dot-product logits naturally grown in magnitude.
                - Larger logits make the softmax more likely to saturate, causing the above gradient issue.

#### Multi-head self-attention

- **Multi-head self-attention**:
    - Used to handle when there might be **multiple patterns of attention** that are relevant at the same time.
        - `eg` IN NL, some patterns might be relevant to tense whereas other might be relevant to subject-verb agreement.
    - Given $H$ heads indexed by $h \in [H]$, for each $h$:
        - $\textbf{H}_h = Attention(\textbf{Q}_h, \textbf{K}_h, \textbf{V}_h)$.
            - Where $\textbf{Q}_h = \textbf{XW}_h^{(q)}$, $\textbf{k}_h = \textbf{XW}_h^{(k)}$ and $\textbf{V}_h = \textbf{XW}_h^{(v)}$.
        - The heads are first concatenated and then multipled by a matrix $\textbf{W}^{(o)}$.
    - `def` **Multi-head self-attention**: $\textbf{Y}(\textbf{X}) = \text{Concat}[\textbf{H}_1, \dots, \textbf{H}_H] \textbf{W}^{(o)}$.
        - Dimensionality:
            - $\textbf{Y}(\textbf{X})$: $N \times D$.
            - $\text{Concat}[\textbf{H}_1, \dots, \textbf{H}_H]$: $N \times HD_v$.
            - $\textbf{W}^{(o)}$: $HD_v \times D$.
        - $\textbf{W}^{(o)}$ is both used:
            - To **reduce dimensionality** (to have $N \times D$).
            - To **learn to best combine attention heads to produce a useful output**.
        - Typically $D_v = \frac{D}{H}$ so that the concatenated matrix has the same dimensionality as the input matrix.
            - So that $N \times D$ for the concat, and $D \times D$ for $\textbf{W}^{(o)}$.
            - In this case $\textbf{W}^{(o)}$ is not needed for dimensionality reduction, but for the other reason.

### Transform block

- **Trasnform block**:
    - A few additional improvements can be made to self-attention to make it more expressive and easier to train.
    - To improve training efficiency:
        - A **residual connection** around self-attention is added.
        - Followed or proceeded by **layer normalization**.
        - $\textbf{Z} = \text{LayerNorm}[\textbf{Y}(\textbf{X}) + \textbf{X}]$ or $\textbf{Z} = \textbf{Y}(\text{LayerNorm}[\textbf{X}]) + \textbf{X}$.
    - The output is then passed through a non-linear NN with $D$ input units and $D$ output units (MLP).
        - This can be a two-layer feedforward NN with ReLU activations.
        - $\tilde{\textbf{X}} = \text{LayerNorm}[\text{MLP}[\textbf{Z}] + \textbf{Z}]$ or $\tilde{\textbf{X}} = \text{MLP}[\text{LayerNorm}[\textbf{Z}]] + \textbf{Z}$.

### Positional encoding

- **Positional encoding**:
    - **Permutation equivariant of self-attention**:
        - Consider $\textbf{X} \in \mathbb{R}^{N \times d}$ a sequence of token embeddings and $\textbf{P}$ a permutation matrix  of the rows of $\textbf{X}$.
            - $\textbf{P}$ is a square binary matrix obtained by permuting the rows of an identity matrix.
            - $\textbf{P} \textbf{X}$ (left multiplication) $\to$ permuting the rows $i$ and $j$ of $\textbf{X}$ iff $P_{ij} = 1$.
            - $\textbf{P}^T = \textbf{P}^{-1} \implies \textbf{P}^T \textbf{P} = \textbf{P} \textbf{P}^T = \textbf{I}$.
        - The main matrix operation in the transformer are $\textbf{XX}^T \textbf{X}$.
            - Note that $\textbf{PX}(\textbf{PX})^T \textbf{PX} = \textbf{PXX}^T \textbf{P}^T \textbf{PX} = \textbf{P}(\textbf{XX}^T \textbf{X})$.
            - $\implies$ **Self-attention is equivariant** to the permutations of the input tokens.
        - This is a problem, since **order** of tokens is usually important (`eg` in text sentences).
    - The goal is not to change the transformer architecture.
        - Instead, to **encode the position of the tokens in the input sequence**.
        - Idea: to encode the position $n$ of the $n$-th tokens as an additional input vector $\textbf{r}_n$.
            - And combine it with the token vector $\textbf{x}_n$ before passing it to the transformer.

#### Appending or adding positional information

- **Appending or adding positional information**:
    - **Appending** the positional encoding to the token vector is not desirable.
        - Since the dimensionality of the input space and subsequent layers increase.
    - The positional encoding can be simply **added** to the token vector.
        - This can seems dangerous since it induces a corruption or loss of information.
        - But two randomly chosen uncorrelated vectors tend to be nearly **orthogonal in high-dimensional spaces**.
            - Allowing the network to process them separately.
        - The residual connection allows the positional information to not get lost too.
    - Due to linear processing in the transformer, **a concatenated representation exhibits properties to an additive one**.

#### Ideal and bad positional encodings

- **Ideal and bad positional encodings**:
    - An ideal encoding should:
        - Be **unique** for each position.
        - Be **bounded** (each element of the encoding representation should have a **finite range**).
        - Generalize to **sequences of arbitrary length**.
        - Have a consistent way to express **relative positions**.
    - `eg` Bad positional encodings:
        - **One-hot encoding**:
            - It's unique and bounded.
            - But it doesn't generalize to sequences of arbitrary length.
            - And not make it easy to reason about relative positions.
        - **Assigning an integer to each position**:
            - It's unique, but not bounded.
            - It may start to corrupt the vector significantly as the sequence length increases.
        - **Assigning a real number in $[0, 1]$ to each position**:
            - It's bounded, but not unique since it depends on the length of the sequence.

#### Sinusoidal positional encoding

- **Sinusoidal positional encoding**:
    - There are many approach to define positional encodings.
        - One of the most popular is the **sinusoidal positional encoding**.
    - `def` **Sinusoidal positional encoding**: $\textbf{r}_n = [\sin(w_1 \cdot n), \: \cos(w_1 \cdot n), \dots, \sin(w_{D/2} \cdot n), \cos(w_{D/2} \cdot n)]$.
        - Where $n$ is the token position, $w_i = \frac{1}{10000^{2i / D}}$ (frequency) and $D$ the size of the representation.
    - `prop` This encoding makes it easy to reason about **relative positions**.
        - Two reasoning sustains this assertion:
            - The dot product between two positional encoding $\textbf{r}_n$ and $\textbf{r}_m$ depends only on $n-m$.
                - And not on the absolute positions $n$ and $m$.
            - Encoding of $n+m$ can be expressed as a **linear combination of the encodings** of $n$ and $m$.
                - It is always possible to find $\textbf{M}$ that depends only on $k$, such that $\textbf{r}_{n+k} = \textbf{Mr}_n$.

### Applications of Transformers

- **Applications of Transformers**:
    - Three modes:
        - **Prediction** (**encoders**)
        - **Generation** (**decoders**)
        - **Translation** or **seq2seq** (**encoder-decoders**).

#### Tokenization

- **Tokenization**:
    - Tokens are generally small groups of characters.
        - They might include words in their entirety.
        - Created in a pre-processing step that convert a string of words and punctuation into a string of tokens.
    - Tokenization can be applied to various types of data (images, etc).
    - **Byte pair encoding**:
        - One of the most common tokenization method.
        - It initially treats the set of unique characters as $1$-character-long $n$-grams (the initial tokens). 
        - Then, successively, the **most frequent pair of adjacent tokens** is merged into a new, longer $n$-gram.
            - All instances of the pair are replaced by this new token.
        - This is repeated until a vocabulary of prescribed size is obtained.
    - Rule of thumb: one token generally corresponds to $\sim 4$ character of text for common English.
        - This translates to roughly $\frac{3}{4}$ of a word (`eg` $100$ tokens $\approx 75$ words).

##### Detokenization

- **Detokenization**:
    - Modern tokenizer must take *special care* to ensure that text can be reconstructed **exactly**.
        - With a naive concatenation, the original spacing may be lost.
        - A proper **detokenization** step is therefore required to recover the original text.
    - Modern LLM tokenizers **encode** whether a **space precedes each token directly inside the token itself**.
        - During tokenization a special prefix character is added whenever a token begins a new word.
        - Detokenization simply consists of joining the tokens **exactly as the tokenizer produced them**.
            - After which the special space-prefix markers are converted back into normal spaces.

#### Decoder transformers

- **Decoder transformers** (*generation*):
    - Goal: to use a transformer architecture to construct a **autoregressive model**.
        - `def` **Autoregressive model**: $p(\textbf{x}_1, \dots, \textbf{x}_N) = \prod_{n=1}^N p(\textbf{x}_n \mid \textbf{x}_1, \dots, \textbf{x}_{n-1})$.
            - But in the attention weights, everything is connected, is not sequential.
        - $p(\textbf{x}_n \mid \textbf{x}_1, \dots, \textbf{x}_{n-1})$ are expressed by a transformer network learned from data.
    - The architecture consists of a **stack of transformers**:
        - That a sequence of tokens.
        - And produce a sequence $\tilde{\textbf{x}}_1, \dots, \tilde{\textbf{x}}_n$ of dimensionality $D$ as output.
        - Then a linear transformation followed by a softmax to compute the **distribution** over the $K$ output tokens.
        - $\textbf{Y} = \text{Softmax}[\tilde{\textbf{X}}{\textbf{W}}^{(p)}]$.

##### Masking

- **Masking**:
    - Decoder models generate text by sampling from $p(\textbf{x}_n \mid \textbf{x}_1, \dots, \textbf{x}_{n-1})$ at each step $n$.
        - Older predictions are **used as input** to the model to generate the next token.
    - With other NN architecture for processing sequences it would be nice to **process entire sequences at once**.
        - But the model would be able to see the future tokens (making the task trivial).
    - The attention mechanism gives a way to **mask out the future tokens** when computing the attention weights.
        - Thus allowing parallel processing of whole sequence.
        - The idea consists of two steps:
            - Add a special token to the input sequence that represents the start of the sequence.
            - Mask out (set to zero) the attention weights for the future tokens.
        - The attention weights are computed as $\textbf{QK}^T$ but with attention set to $0$ for **future tokens**.
            - A **mask matrix** $\textbf{M}$ has $- \infty$ in the upper triangular part.
            - $\textbf{Y} = \text{Softmax}[\frac{QK^T}{\sqrt{D_k}} \circ \textbf{M}] \textbf{V}$.
            - Therefore $\textbf{y}_i$ will depends only on $\textbf{x}_1$ to $\textbf{x}_{i-1}$.

##### Padding

- **Padding**:
    - To allow processing multiple sequences at once, it is desirable to collect them in a sequence.
        - But this would require that all sequences have the same length.
    - To solve this issue, sequences are **padded** to the same length.
        - Using a special token to represent the padding.
    - A **mask matrix** $\textbf{M}$ that has $-\infty$ in the positions of the **padding tokens** is used.
        - So that the attention weights for the padding tokens are zero.

##### Sampling strategies

- **Sampling strategies**:
    - Once the distribution $p(\textbf{x}_n \mid \textbf{x}_1, \dots, \textbf{x}_{n-1})$ is computed, the next token can be sampled from it.
    - The **greedy strategy** simply chooses the token with the highest probability.
    - To find the most probable sequence, the joint distribution over all tokens must be maximized.
        - $p(\textbf{y}_1, \dots, \textbf{y}_N) = \prod_{n=1}^N p(\textbf{y}_n \mid \textbf{y}_1, \dots, \textbf{y}_{n-1})$
        - The higher probability sequences can be generated using a **beam search** strategy.
            - Which keeps track of the $k$ most probable sequences at each step.
    - However, the most likely sequences are not necessarily the most human-like sequences.
        - By sampling on the full distribution, sequences that are nonsensical or grammatically incorrect can be generated.
        - This arises from the typically very large size of the token dictionary.
            - Which has **long tail of tokens** with very **low probability**.
    - **Top-$K$ sampling** mitigates this issue by sampling from a **truncated distribution**.
        - A distribution that includes the top-$K$ most probable tokens.
    -  **Top-$p$ sampling** (**nucleus sampling**) samples from a truncated distribution that:
        - Includes the smallest set of tokens whose cumulative probability exceeds a threshold $p$.
    - `def` **Temperature scaling**: $y_i = \frac{\exp(a_i / T)}{\sum_j \exp(a_j / T)}$.
        - A soft version of Top-$K$ sampling.
        - Scales the pre-activation values by a temperature parameter $T$ before applying the softmax function.
        - When:
            - $T \to 0$ the distribution becomes more peaked around the most probable tokens.
            - $T = 1$ the distribution is the same as the original softmax distribution.
            - $T \to \infty$ the distribution becomes uniform across all states.

#### Encoder transformers

- **Encoder transformers** (*prediction*):
    - Used to process sequences of tokens and produce contextual embeddings that can be used to produce a **fixed-size representation of the sequence**.
    - **Masked language model**:
        - The $\langle class \rangle$ is put at the beginning of the input sequence.
            - During pretraining, **class** token **is not itself predicted**, since only masked tokens contribute directly to the loss.
            - But it still participates fully in the transformer layers.
            - It attends to all other tokens (and vice versa), so it representation becomes a **contextual summary of the whole sequence**.
            - It is never masked during pretraining. 
        - A randomly chosen subset (`eg` $15\%$) of the tokens are replaced by the **mask token**.
        - The model is then trained to predict the original tokens from the masked tokens.
            - **Self-supervised learning task**.
    - For sequence classification task, the **class token** is used to produce a fixed-size presentation.
        - Which is then passed through a linear layer to produce the output.
    - For token classification tasks, the output of the transformer is passed through a linear layer (or more complex classifier) for each token to produce the output.
        - The output of the class token is ignored.

#### Encoder-Decoder transformers

- **Encoder-Decoder transformers** (*seq2seq*):
    - To perform translation, the transformer architecture uses a **cross-attention mechanism**.
        - It allows the **decoder to mix information from the encoder** with the information generated so far.
        - The output of the $E$ $\textbf{Z}$ is fed with entire input sequence in the **multi-head cross attention** in $D$.
            - $\textbf{Z}$ will be the representation of the **class token**.
    - At training time, the input of $D$ will be the *translated sequence*.
        - It is therefore a **supervised learning task**.
