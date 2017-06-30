
# Manual

## Concepts

We define the standard form problem as:

```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

At the moment all functions are described compactly with lists, vectors, and matrices. NLP is a special case discussed later. An objective function ``f_0`` can be affine or quadratic. The constraint functions ``f_i`` can be variablewise, affine, or quadratic (to be defined).

## Duals

So far, we define a convention for duals for conic representable problems. We do not define behavior for duals involving quadratic constraints.
We take the convention that duals on lower bounds (`GreaterThan`) should be nonnegative, duals on upper bounds (`LessThan`) should be nonpositive, and duals on closed convex cones should belong to the dual cone.

For minimization problems in conic form, we can define the primal  as:

```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & b_0^Tx
\\
& \;\;\text{s.t.} & A_ix + b_i & \in \mathcal{C}_i & \forall i
\end{align}
```
and the dual as:

```math
\begin{align}
& \max_{y_i \forall i} & -\sum_i b_i^T y_i
\\
& \;\;\text{s.t.} & b_0 - \sum_i A_i^T y_i &= 0
\\
& & y_i \in \mathcal{C}_i^* && \forall i
\end{align}
```

``a^Tx + b \ge c`` should be interpreted (for the purposes of duals) as ``a^Tx + b - c \in \mathbb{R}_+``, and similarly ``a^Tx + b \le c`` should be interpreted (for the purposes of duals) as ``a^Tx + b - c \in \mathbb{R}_-``. Variablewise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of ``A_i``.
