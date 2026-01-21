---
title: 'Neural networks and Deep learning - Parte X - Graph neural networks'
---

# Neural networks and Deep learning - Parte X

## Graph neural networks

### Graph neural networks

- **Graph neural networks**:
    - NNs tailored to process **graphs** (their architecture are not *graph-like*).
    - Graphs are a general language for **describing entities with relations/interactions**.
        - `eg` Social networks, biological networks, chemical compounds, etc.

#### Graphs

- **Graphs**:
    - `def` **Graph**: $G = (V, E)$, a tuple of a set of **nodes** and a set of **edges**.
        - Each edge is a pair of two vertices.
    - **Adjacency matrix**:
        - An undirected graph $G$ with $n$ nodes is assumed.
        - A (binary) **adjacency matrix** $A^G$:
            - A square matrix of size $n \times n$.
            - Each entry $\textbf{A}_{ij}^G$ is $1$ it there is an edge between the node $i$ and $j$.
    - Setup:
        - A graph $G = (V, E)$ is given.
        - $V$ is the set of nodes and $E$ is the set of edges.
        - $\textbf{A}$ is the adjacency matrix.
        - $N(v)$ is the set of neighbors of node $v \in V$.
        - Each node $v$ has an associated **feature vector** $\textbf{x}_v \in \mathbb{R}^d$:
            - If not, $\textbf{x}_v = 1$ can be set.
            - Or an indicator vector to each node (one-hot encoding) can be assigned.

##### Tasks involving graphs

- **Tasks involving graphs**:
    - **Node-level** prediction (`eg` drug-drug interaction).
        - The training can be performed on a single graph.
        - The test is performed on *unlabelled* nodes.
    - **Link-level** prediction (`eg` recommendation).
        - The training can be performed on a single graph.
        - The model can learn patterns and relationships that indicate preferences.
        - The model can use interactions and node features to predict the likelihood of preferences.
    - **Graph-level** prediction (`eg` time of arrival).

### Graphs and Machine learning

- **Graphs and Machine learning**:
    - The traditional method to handle graphs using ML:
        - Extract features from the graph (`eg` node degree, centrality, subgraph patterns, etc).
        - Use these features as input to a ML model (`eg` SVM, NN, RF, etc).
    - **Graph representation learning**:
        - DNN have changed the way data is learnt and represented.
        - Feature engineering is replaced by **learning representation**:
            - Learn a function that maps a graph to a vector representation.
            - Use the learned representation for downstream tasks (`eg` node classification, etc).

#### Node embeddings

- **Node embeddings**:
    - Early approach in GNN.
    - Goal: define $ENC()$ so that $sim(u, v) \approx \textbf{z}_u^T \textbf{z}_v$.
        - Where $sim$ is a similarity measure between nodes $u$ and $v$.
        - A simple way to get graph embeddings is to aggregate (`eg` average) the node embeddings.

##### Encoder-decoder framework

- **Encoder-decoder framework**:
    - **Encoder**: $\textbf{z}_v = ENC(v)$.
        - A function that maps nodes to **vector representations**.
    - **Similarity**: $sim(v, u)$.
        - A function that measures the similarity between two nodes in the **graph space** (**domain knowledge**).
        - **Random walk**: a sequence of steps where at each step, a node is chosen randomly from the neighboring nodes of the current node.
            - **Random Walk similarity** ($RW$): a node similarity based on visiting node $u$ on a random walk starting from node $v$.
            - $RW$ is a flexible stochastic definition that incorporates both **local and high-order neighborhood information**.
            - `eg` Expected commute time between two nodes, etc.
    - **Decoder** $y_{vu} = DEC(\textbf{z}_v, \textbf{z}_u)$.
        - A function that compute the similarity between two nodes in the **embedding space**.
        - Usually, a function of the dot product between embeddings of two nodes.
            - $y_{vu} = DEC(\textbf{z}_v, \textbf{u}) = f(\textbf{z}_v^T \textbf{z}_u)$.
    - Once fixed $DEC$, the goal is to learn the function $ENC$ such that the similairty in the embedding space $y_{vu}$ **approximates the similarity in the graph space**.

##### Learning

- **Learning**:
    - *Goal*: to learn the parametrized function $ENC_{\theta}$ such that $\textbf{z}_v^T \textbf{z}_y = ENC_{\theta}(v)^T ENC_{\theta}(u)$.
        - $\approx$ probability that $u$ and $v$ co-occur on a random walk over the graph.
    - `def` **Node embeddings loss (for RW)**: $\text{arg min}_{\theta} \sum_{v \in V} \sum_{u \in N_r(v)} - \log(\frac{\exp(\textbf{z}_v^T \textbf{z}_u)}{\sum_{w \in V} \exp(\textbf{z}_v^T \textbf{z}_w)})$.
        - A heuristic that tries to approximate the above probability.
        - Where $N_R(v)$ is the set of neighbors of $v$ according to the random-walk strategy $R$.
        - This expression is **easily optimized using SGD**.
        - Derivation:
            - $- \log(\frac{\exp(\textbf{z}_v^T \textbf{z}_u)}{\sum_{w \in V} \exp(\textbf{z}_v^T \textbf{z}_w)}) = \dots = C_v \textbf{z}_v^T \textbf{z}_w$.
            - $v$ is assumed fixed, the expression is minimized when $\textbf{z}_v^T \textbf{z}_w$ is maximized.
            - Which is, when the similarity between $v$ and $u$ is maximized.
            - Since $u$ is drawn from $v$ neighbors, the **similarity** between $v$ and its neighbors is **effectively maximized**.
        - $f$ is assumed to be a monotonic function (`eg` a normalization of the dot product).
        - This loss is **quadratic**, in practice **negative sampling optimization** is used.
            - The denominator becomes $\sum_{w \notin N_R(v)} \exp(\textbf{z}_v^T \textbf{z}_w)$.

#### Graph embeddings

- **Graph embeddings**:
    - The node embeddings can be aggregated into a graph embedding $\textbf{z}_G$.
        - Using different strategies to obtain a graph embedding.
        - The most common strategies are:
            - Sum/average of the node embeddings.
            - Create a super-node that represents the entire graph.
    - Early attempts tried to directly learn the embeddings of the nodes.
        - Instead of learning a function that maps the graph to a vector representation.
        - Idea:
            - Learn the embedding $\textbf{z}_v$ for all nodes $v \in V$.
            - Use the embeddings for downstream tasks.
    - Downstream tasks:
        - **Node clustering**: cluster points $\{ \textbf{z}_u\}$ in the embedding space.
        - **Node prediction**: predict the label of a node $u$ based on $\textbf{z}_u$.
        - **Link prediction**: predict the existence of edge (u, v) based on $\{ \textbf{z}_u, \textbf{z}_v\}$.
            - Combining the embeddings: concatenate, Hadamard, sum/avg, distance, etc.
        - **Graph classification**: predict the label of the entire graph based on $\textbf{z}_G$ (hardest task).

#### Limitations of earlier approaches

- **Limitations of earlier approaches**:
    - Why not use the predefined similarity (which node embeddings try to reproduce) on graph nodes.
        - Those similarity are usually:
            - Implicit or expensive to compute.
            - Not suitable as ML inputs.
            - Tied to a fixed graph.
            - Hard to generalize or scale.
        - Node embeddings act as a **low-dimensional**, **learnable surrogate** for graph similarity.
    - Limitations of earlier approaches:
        - **Transductive method**.
            - Cannot obtain embeddings for nodes not in the training set.
                - Therefore cannot be applied to new graphs.
            - Inductive methods: a model is built and used to predict on new data.
        - Cannot capture structural similarity.
        - Cannot utilize node, edge and graph features.

### Modern approaches to Graph neural network

- **Modern approaches to Graph neural network**:
    - Assumptions:
        - Arbitrary size and complex topological structure (no spatial locality like grids).
        - No fixed node ordering or reference point.
        - Often dynamic (graph changes over time) and have multimodal features.
    - Using an adjacency matrix with extra information for features is problematic.
        - By swapping the labelling of two nodes, the input matrix changes (not desirable).
        - Main issues:
            - **Sensitive to node ordering**.
            - Not applicable to graphs of different sizes (the input layer of the DNN wouldn't match).

#### Invariance and equivariance

- **Invariance and equivariance**:
    - **Permutation invariance**:
        - Graph does not have a canonical order of the nodes.
        - Graph and node **representations should be the same** for different ordering.
    - **Permutation matrix**:
        - A square binary matrix that has:
            - Exactly one entry of $1$ in each row and each column.
            - $0$ elsewhere.
        - Multiplication:
            - If multiplied on the left of a matrix, it permutes the rows of the matrix.
            - If multiplied on the right of a matrix, it permutes the columns of the matrix.
        - The structure of the corresponding graph is the same, but the nodes are permuted.
            - The roles played by the nodes is no more the same, since their connectivity is different.
        - To restore the correct connectivity, the adjacency matrix must be permuted as well.
            - The permutation matrix is applied to swap the rows and columns of the adjacency matrix.
                - $\textbf{PAP}^T$ is computed.
            - Then the graph is the same as the original one, but with a different labelling.
    - `def` **Permutation invariance**: the graph function $f$ if $f(\textbf{A}, \textbf{X}) = f(\textbf{PAP}^T, \textbf{PX})$.
        - For any graph function $f: \mathbb{R}^{n \times n} \times \mathbb{R}^{n \times m} \to \mathbb{R}^d$ and for any permutation matrix $\textbf{P}$.
            - $\mathbb{R}^{n \times n} \times \mathbb{R}^{n \times m}$, $\mathbb{R}^{n \times n}$ is for the adjacency matrix, $\mathbb{R}^{n \times m}$ for the feature matrix.
            - $n$ is the number of nodes, $m$ the number of features of a node.
    - `def` **Permutation equivariance**: the node function $f$ if $\textbf{P} f(\textbf{A}, \textbf{X}) = f(\textbf{PAP}^T, \textbf{PX})$.
        - For any node function $f: \mathbb{R}^{n \times n} \times \mathbb{R}^{n \times m} \to \mathbb{R}^{n \times d}$ and for any permutation matrix $\textbf{P}$.
            - $n$ is the number of nodes, $m$ the number of features of a node.
        - The output is permuted as is the input (it changes but it changes in the same manner).
    - `eg` Permutation invariant and equivariant functions:
        - $f(\textbf{A}, \textbf{X}) = 1^T \textbf{X} = \sum_i \textbf{x}_i$ is permutation-invariant.
            - Proof: $f(\textbf{PAP}^T, \textbf{PX}) = \textbf{1}^T \textbf{PX} = \sum_i x_i = f(\textbf{A}, \textbf{X})$.
        - $f(\textbf{A}, \textbf{X}) = \textbf{X}$ is permutation-equivariant.
            - Proof: $f(\textbf{PAP}^T, \textbf{PX}) = \textbf{PX} = \textbf{P}f(\textbf{A}, \textbf{X})$.
        - $f(\textbf{A}, \textbf{X}) = \textbf{AX}$ is permutation-equivariant.
            - Proof: $f(\textbf{PAP}^T, \textbf{PX}) = \textbf{PAP}^T \textbf{PX} = \textbf{PAX} = \textbf{P} f(\textbf{A}, \textbf{X})$.
        - **A MLP is neither permutation-invariant nor permutation-equivariant**.

#### GNN design principles

- **GNN design principles**:
    - Desiderata:
        - Ensure **permutation invariance** and **equivariance**.
            - By requiring PI and PE, the type of computation that the NN can performed is hugely restricted.
        - Use **layers** as reusable, permutation-equivariant units.
            - Stacking layers will not break permutation invariance.
            - **Node-level predictions** will be automatically permutation-invariant.
        - Design layers as **flexible differentiable functions**.
        - Enable scalability with **parameter sharing** (to enable scalability to big data).
    - A GNN is a **optimizable transformation** on all attributes of the graph (nodes, edges, global-context) **that preservers graph symmetries** (permutation invariances).
    - *Goal*: design graph neural networks using permutation invariant/equivariant transformations and passing and aggregating information from neighbors.

### Message Passing Framework

- **Message Passing Framework** (MPF):
    - Neighbour aggregation: generate node embeddings based on local network neighborhoods.
        - The neighborhood can be enlarged by apply the same produces for several steps.
        - NA can be described as a computational graph.
            - Computations can be done iteratively in steps (with parameter $k$ for the neighbor depth). 
            - Key distinctions are in how different approaches aggregate information across the layers.
    - The MPF is a general way to define the update rule for each node.
        - A single GNN layer is considered.
    - MPF consists of two main steps:
        - **Message computation**:
            - Each node $v$ sends a message to its neighbors.
            - Based on its own features and the features of its neighbors.
        - **Message aggregation**:
            - Each node $v$ aggregates messages received from its neighbors.
            - And updates its own features.
    - For node $v$, $\textbf{x}_v = \textbf{h}_v^{(0)} \to \textbf{h}_v^{(1)} \to \dots \to \textbf{h}_v^{(L)}$.

#### Message computation

- `def` **Message computation**: $m_u^{(l)} = MSG^{(l)}(\textbf{h}_u^{(l)})$.
    - The message computed at level $v$ by the node $u$.
    - $MSG^{(l)}$ is usually a (simple, `eg` one-layer) NN that:
        - Takes as input the features of the node $u$ at level $l-1$.
        - Returns a message $\textbf{m}_u^{(l)}$.
        - `eg` $\textbf{m}_u^{(l)} = f_{\textbf{W}^{(l)}} (\textbf{h}_u^{(l-1)}) = \text{ReLU}(\textbf{W}^{(l)} \textbf{h}_u^{(l-1)})$.
    - `prop` Properties of message computation:
        - The **parameters** of the message computation function $MSG^{(l)}$ are **shared across the nodes**.
        - The function is **permutation-equivariant**.
            - Because the same function is applied to all nodes in the graph independently.
        - Both properties are crucial to ensure that the GNN can be applied to graphs of different sizes.

#### Message aggregation

- `def` **Message aggregation**: $\textbf{h}_v^{(l)} = \text{Agg}^{(l)}(\{\textbf{m}_u^{(l)}, \forall u \in N(v)\})$.
    - Node $v$ will aggregate the messages from its neighbors.
        - The aggregation function must be **permutation-invariant** (`eg` sum, mean, max).
        - Potential parameters associated with $\text{Agg}^{(l)}$ are shared across the nodes.
    - Issue: information from node $v$ itself could **get lost**.
        - Right now, the computation of $\textbf{h}_v^{(l)}$ does not **directly** depend on $\textbf{h}_v^{(l-1)}$.
        - Possible fix (not all GNN fix this):
            - Message: $\textbf{m}_v^{(l)} = \textbf{B}^{(l)} \textbf{h}_v^{(l-1)}$.
                - Compute message from node $v$ itself.
            - Aggregation: $\textbf{h}_v^{(l)} = \text{Agg}^{(l)}(\{\textbf{m}_u^{(l)}, \forall u \in N(v)\}, \textbf{m}_v^{(l)})$.
                - Aggregates the message from node $v$ itself after aggregating from neighbors.
                - `eg` Sum aggregation: $\textbf{h}_v^{(l)} = \textbf{m}_v^{(l)} + \sum_{u \in N(v)} \textbf{m}_u^{(l)}$.

### Graph Convolutional Networks

- **Graph Convolutional Networks**:
    - A type of NN designed to operate on graph-structured data.
        - It generalizes the concept of **convolution** from traditional grid data to graph data. 
    - It **aggregates feature information** from a node's **local neighborhood**.
        - Each layer updates the feature representation of each node.
            - By combining its own features with the features of its neighbors.
        - It does so by **multiplying** the **adjacency matrix** $\textbf{A}$ ($n \times n$) with the **feature matrix** $\textbf{H}^{(l)}$ ($n \times m$).
            - For each node $u$ and feature $f$ the value $(\textbf{AH})_{uf} = \sum_{k \in N(u)} A_{uk} H_{kf}$ contains:
                - The sum of $f$ of all the nodes in the graph, weighted by edges connecting $u$ to those nodes.
                - Not connected nodes won't contribute to the sum via multiplication via $\textbf{A}$ (zero values).
            - With weights $0$ or $1$, $(\textbf{AH})_{uf} = \sum_{k \in N(u)} A_{uk} H_{kf} \to \sum_{k \in N(u)} \textbf{H}_k = \sum_{k \in N(u)} \textbf{h}_k$.

#### Normalization

- **Normalization**:
    - To **avoid numerical instability**, the adjacency matrix $\textbf{A}$ can be **normalized**.
        - By dividing each row by the sum of its elements.
    - `def` **Normalized adjacency matrix**: $\tilde{\textbf{A}} = \textbf{D}^{-1} \textbf{A}$.
        - $\tilde{A}_{uv} = \frac{A_{uv}}{deg(u)} = \frac{A_{uv}}{|N(u)|}$.
        - $\textbf{D}$: the diagonal degree matrix, $D_{uu} = \sum_v A_{uv}$.
    - `def` **Symmetric normalization**: $\tilde{\textbf{A}} = \textbf{D}^{-1/2} \textbf{AD}^{-1/2}$.
        - $\tilde{A}_{uv} = \frac{A_{uv}}{\sqrt{D_{uu}D_{vv}}} = \frac{A_{uv}}{\sqrt{deg(u)deg(v)}} = \frac{A_{uv}}{\sqrt{|N(u)||N(v)|}}$.
        - $\textbf{D}^{-1/2}$: the **diagonal matrix** with the inverse of the square root of the diagonal elements of $\textbf{D}$.
        - Enhancement over the previous solution, standard nowadays for GCN.

#### GCN layers

- **GCN layers**:
    - `def` **GCN layer**: $\textbf{H}^{(l+1)} = \sigma(\tilde{\textbf{D}}^{-1/2} \tilde{\textbf{A}} \tilde{\textbf{D}}^{-1/2} \textbf{H}^{(l)} \textbf{W}^{(l)})$.
        - $\tilde{\textbf{A}} = \textbf{A} + \textbf{I}$: adjacency matrix with added **self-loops**.
            - Self-loops are used to store information from the node itself (desiderata in message aggregation).
        - $\tilde{\textbf{D}}$: diagonal degree matrix of $\tilde{\textbf{A}}$, $\tilde{D}_{ii} = \sum_j \tilde{A}_{ij}$.
        - $\tilde{\textbf{D}}^{-1/2} \tilde{\textbf{A}} \tilde{\textbf{D}}^{-1/2}$: normalized $\tilde{\textbf{A}}$.
        - $\textbf{W}^{(l)}$: learnable weight matrix.
        - $\sigma$: activation function (`eg` ReLU).
    - Benefits:
        - **Locality**: leveraging local graph structure makes them effective for tasks like node classification and link prediction.
        - **Scalability**: by stacking multiple GCN layers, information from larger neighborhoods can be captured.
    - Everything can be described with the **MPF**:
        - $\textbf{h}_u^{(l)} = \sigma(\sum_{v \in N(u) \cup \{u\}} \textbf{W}^{(l)^T} \frac{\textbf{h}_v^{(l-1)}}{\sqrt{|N(v)||N(u)|}})$.
        - $\sum_{v \in N(u) \cup \{u\}}$ performs **aggregation**, while $\textbf{W}^{(l)^T} \frac{\textbf{h}_v^{(l-1)}}{\sqrt{|N(v)||N(u)|}}$ performs **message computation**.

### Training a GNN

- **Training a GNN**:
    - `def` **Last layer node embeddings**: $\textbf{z}_v = \textbf{h}_v^L = ENC_{\theta}(\textbf{A}, \textbf{x}_v)$.
        - Given a GNN with $L$ layers and a GNN model $ENC_\theta$ (`eg` GCN, GraphSAGE, GAT).
    - On top of the GNN, **further layers** (`eg` fully connected layers) are defined to perform **downstream tasks**.
        - $\textbf{y}_v = DEC_{\theta'}(\textbf{z}_v)$.
    - To train the GNN, a **loss function** is defined and optimized via gradient-based methods.

#### Supervised learning with GNNs

- **Supervised learning with GNNs**:
    - A **node-level task** is assumed.
    - The goal is to minimize the following generic loss function: $\min_{\theta, \theta'} \sum_v \mathcal{L}(\textbf{y}_v, DEC_{\theta'}(ENC_{\theta}(\textbf{A}, \textbf{x}_v))$.
        - This loss can be optimized using gradient-based methods.
    - The examples are the **nodes of the graph**.
        - To estimate the generalization error, a separate test set of nodes is kept. 
        - The model is trained on the remaining nodes.

#### Model parameters

- **Model parameters**:
    - In a GNN model the parameters are:
        - The parameters of the message computation and aggregation function.
        - The parameters for the downstream task (`eg` fully connected layers).
    - In **Graph convolutional networks** the parameters are:
        - The weight matrices $\textbf{W}^{(l)}$.
        - The parameters for solving the downstream task. 

#### Stacking GNN layers

- **Stacking GNN layers**:
    - In practice, multiple GNN layers can be stacked to **enhance model expressivity**.
        - But GNNs suffer from over-smoothing problem.
    - **Over-smoothing problem**: all the node embeddings converge to the **same value**.
        - It's not possible to capture the structural properties of the graph.
        - The model is not able to distinguish between different nodes.
    - Stacking multiple layers corresponds **enlarging the receptive field of a node**.
        - The set of nodes that influence the node's embedding.
        - Shared neighbors quickly grows when the number of hops.
            - Since the node embeddings is determined by its receptive field.
                - If two nodes have highly-overlapped receptive fields, then their **embeddings are highly similar**.
        - Stack **many** GNN layers $\implies$ nodes will have **highly overlapped** receptive fields.
            - $\implies$ node embeddings will be **highly similar** $\implies$ suffer from the **over-smoothing problem**.

##### Enhancing GNN expressivity

- **Enhancing GNN expressivity** (main strategies):
    - **Increase the expressive power within each GNN layer**.
        - Message computation and aggregation become more complex.
    - **Add layers that do not pass messages**.
        - Add more layers before and after the GNN layers.
    - **Skip connections**.
        - Node embeddings in earlier GNN layers can sometimes better differentiate nodes.
        - Shortcuts are therefore added in the GNN.
        - Usually skip connection is implemented by summing the previous embeddings to the current ones.
