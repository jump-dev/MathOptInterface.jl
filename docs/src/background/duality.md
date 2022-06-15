```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Duality

Conic duality is the starting point for MOI's duality conventions. When all
functions are affine (or coordinate projections), and all constraint sets are
closed convex cones, the model may be called a conic optimization problem.

For a minimization problem in geometric conic form, the primal is:
```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```
and the dual is a maximization problem in standard conic form:
```math
\begin{align}
& \max_{y_1, \ldots, y_m} & -\sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```
where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i^*`` is
its dual cone.

For a maximization problem in geometric conic form, the primal is:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```
and the dual is a minimization problem in standard conic form:
```math
\begin{align}
& \min_{y_1, \ldots, y_m} & \sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 + \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```

A linear inequality constraint ``a^T x + b \ge c`` is equivalent to
``a^T x + b - c \in \mathbb{R}_+``, and ``a^T x + b \le c`` is equivalent to
``a^T x + b - c \in \mathbb{R}_-``. Variable-wise constraints are affine
constraints with the appropriate identity mapping in place of ``A_i``.

For the special case of minimization LPs, the MOI primal form can be stated as:
```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```

By applying the stated transformations to conic form, taking the dual, and
transforming back into linear inequality form, one obtains the following dual:
```math
\begin{align}
& \max_{y_1,y_2,y_3} & b_1^Ty_1 + b_2^Ty_2 + b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

For maximization LPs, the MOI primal form can be stated as:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```
and similarly, the dual is:
```math
\begin{align}
& \min_{y_1,y_2,y_3} & -b_1^Ty_1 - b_2^Ty_2 - b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = -a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

!!! warning
    For the LP case, the signs of the feasible dual variables depend only on the
    sense of the corresponding primal inequality and not on the objective sense.

## Duality and scalar product

The scalar product is different from the canonical one for the sets
[`PositiveSemidefiniteConeTriangle`](@ref), [`LogDetConeTriangle`](@ref),
[`RootDetConeTriangle`](@ref).

If the set ``C_i`` of the section [Duality](@ref) is one of these three cones,
then the rows of the matrix ``A_i`` corresponding to off-diagonal entries are
twice the value of the `coefficients` field in the [`VectorAffineFunction`](@ref)
for the corresponding rows. See [`PositiveSemidefiniteConeTriangle`](@ref) for
details.

## Dual for problems with quadratic functions

### Quadratic Programs (QPs)

For quadratic programs with only affine conic constraints,
```math
\begin{align*}
& \min_{x \in \mathbb{R}^n} & \frac{1}{2}x^TQ_0x + a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m.
\end{align*}
```
with cones ``\mathcal{C}_i \subseteq \mathbb{R}^{m_i}`` for ``i = 1, \ldots, m``, consider the Lagrangian function
```math
L(x, y) = \frac{1}{2}x^TQ_0x + a_0^T x + b_0 - \sum_{i = 1}^m y_i^T (A_i x + b_i).
```
Let ``z(y)`` denote ``\sum_{i = 1}^m A_i^T y_i - a_0``, the Lagrangian can be rewritten as
```math
L(x, y) = \frac{1}{2}{x}^TQ_0x - z(y)^T x + b_0 - \sum_{i = 1}^m y_i^T b_i.
```

The condition ``\nabla_x L(x, y) = 0`` gives
```math
0 = \nabla_x L(x, y) = Q_0x + a_0 - \sum_{i = 1}^m y_i^T b_i
```
which gives ``Q_0x = z(y)``.
This allows to obtain that
```math
\min_{x \in \mathbb{R}^n} L(x, y) = -\frac{1}{2}x^TQ_0x + b_0 - \sum_{i = 1}^m y_i^T b_i
```
so the dual problem is
```math
\max_{y_i \in \mathcal{C}_i^*} \min_{x \in \mathbb{R}^n} -\frac{1}{2}x^TQ_0x + b_0 - \sum_{i = 1}^m y_i^T b_i.
```
If ``Q_0`` is invertible, we have ``x = Q_0^{-1}z(y)`` hence
```math
\min_{x \in \mathbb{R}^n} L(x, y) = -\frac{1}{2}z(y)^TQ_0^{-1}z(y) + b_0 - \sum_{i = 1}^m y_i^T b_i
```
so the dual problem is
```math
\max_{y_i \in \mathcal{C}_i^*} -\frac{1}{2}z(y)^TQ_0^{-1}z(y) + b_0 - \sum_{i = 1}^m y_i^T b_i.
```

### Quadratically Constrained Quadratic Programs (QCQPs)

Given a problem with both quadratic function and quadratic objectives:
```math
\begin{align*}
& \min_{x \in \mathbb{R}^n} & \frac{1}{2}x^TQ_0x + a_0^T x + b_0
\\
& \;\;\text{s.t.} & \frac{1}{2}x^TQ_ix + a_i^T x + b_i & \in \mathcal{C}_i & i = 1 \ldots m.
\end{align*}
```
with cones ``\mathcal{C}_i \subseteq \mathbb{R}`` for ``i = 1 \ldots m``, consider the Lagrangian function
```math
L(x, y) = \frac{1}{2}x^TQ_0x + a_0^T x + b_0 - \sum_{i = 1}^m y_i (\frac{1}{2}x^TQ_ix + a_i^T x + b_i)
```
A pair of primal-dual variables $(x^\star, y^\star)$ is optimal if
* ``x^\star`` is a minimizer of
  ```math
  \min_{x \in \mathbb{R}^n} L(x, y^\star).
  ```
  That is,
  ```math
  0 = \nabla_x L(x, y^\star) = Q_0x + a_0 - \sum_{i = 1}^m y_i^\star (Q_ix + a_i).
  ```
* and ``y^\star`` is a maximizer of
  ```math
  \max_{y_i \in \mathcal{C}_i^*} L(x^\star, y).
  ```
  That is, for all ``i = 1, \ldots, m``, ``\frac{1}{2}x^TQ_ix + a_i^T x + b_i``
  is either zero or in the
  [normal cone](https://en.wikipedia.org/wiki/Normal_cone) of
  ``\mathcal{C}_i^*`` at ``y^\star``. For instance, if ``\mathcal{C}_i`` is
  ``\{ z \in \mathbb{R} : z \le 0 \}``, this means that if
  ``\frac{1}{2}x^TQ_ix + a_i^T x + b_i`` is nonzero at ``x^\star`` then
  ``y_i^\star = 0``. This is the classical complementary slackness condition.

If ``\mathcal{C}_i`` is a vector set, the discussion remains valid with
``y_i(\frac{1}{2}x^TQ_ix + a_i^T x + b_i)`` replaced with the scalar product
between ``y_i`` and the vector of scalar-valued quadratic functions.

## Dual for square semi-definite matrices

The set [`PositiveSemidefiniteConeTriangle`](@ref) is a self-dual. That is,
querying [`ConstraintDual`](@ref) of a [`PositiveSemidefiniteConeTriangle`](@ref)
constraint returns a vector that is itself a member of
[`PositiveSemidefiniteConeTriangle`](@ref).

However, the dual of [`PositiveSemidefiniteConeSquare`](@ref) is not so straight
forward. This section explains the duality convention we use, and how it is
derived.

!!! info "tl;dr"
    If you have a [`PositiveSemidefiniteConeSquare`](@ref) constraint, the
    result matrix ``A`` from [`ConstraintDual`](@ref) is not positive
    semidefinite. However, ``A + A^\top`` is positive semidefinite.

Let ``\mathcal{S}_+`` be the cone of symmetric semidefinite matrices in
the ``\frac{n(n+1)}{2}`` dimensional space of symmetric ``\mathbb{R}^{n \times n}``
matrices. That is, ``\mathcal{S}_+`` is the set [`PositiveSemidefiniteConeTriangle`](@ref).
It is well known that ``\mathcal{S}_+`` is a self-dual proper cone.

Let ``\mathcal{P}_+`` be the cone of symmetric semidefinite matrices in
the ``n^2`` dimensional space of ``\mathbb{R}^{n \times n}`` matrices. That is
``\mathcal{P}_+`` is the set [`PositiveSemidefiniteConeSquare`](@ref).

In addition, let ``\mathcal{D}_+`` be the cone of matrices ``A`` such that
``A+A^\top \in \mathcal{P}_+``.

``\mathcal{P}_+`` is not proper because it is not solid (it is not ``n^2``
dimensional), so it is not necessarily true that ``\mathcal{P}_+^{**} = \mathcal{P}_+``.

However, this _is_ the case, because we will show that ``\mathcal{P}_+^{*} = \mathcal{D}_+``
and ``\mathcal{D}_+^{*} = \mathcal{P}_+``.

First, let us see why ``\mathcal{P}_+^{*} = \mathcal{D}_+``.

If ``B`` is symmetric, then
```math
\langle A,B \rangle = \langle A^\top, B^\top \rangle = \langle A^\top, B\rangle
```
so
```math
2\langle A, B \rangle = \langle A, B \rangle + \langle A^\top, B \rangle = \langle A + A^\top , B \rangle.
```
Therefore, ``\langle A,B\rangle \ge 0`` for all ``B \in \mathcal{P}_+`` if and only if
``\langle A+A^\top,B\rangle \ge 0`` for all ``B \in \mathcal{P}_+``. Since ``A+A^\top`` is
symmetric, and we know that ``\mathcal{S}_+`` is self-dual, we have shown that
``\mathcal{P}_+^{*}`` is the set of matrices ``A`` such that
``A+A^\top \in \mathcal{P}_+``.

Second, let us see why ``\mathcal{D}_+^{*} = \mathcal{P}_+``.

Since ``A \in \mathcal{D}_+`` implies that ``A^\top \in \mathcal{D}_+``,
``B \in \mathcal{D}_+^{*}`` means that ``\langle A+A^\top,B\rangle \ge 0``
for all ``A \in \mathcal{D}_+``, and hence ``B \in \\mathcal{P}_+``.

To see why it should be symmetric, simply notice that if ``B_{i,j} < B_{j,i}``,
then ``\langle A,B\rangle`` can be made arbitrarily small by setting
``A_{i,j} = A_{i,j} + s`` and ``A_{j,i} = A_{j,i} - s``, with ``s`` arbitrarily
large, and ``A`` stays in ``\mathcal{D}_+`` because ``A+A^\top`` does not
change.

Typically, the primal/dual pair for semidefinite programs is presented as:
```math
\begin{align}
       \min & \langle C, X \rangle \\
\text{s.t.} \;\; & \langle A_k, X\rangle = b_k \forall k \\
            & X                     \in \mathcal{S}_+
\end{align}
```
with the dual
```math
\begin{align}
       \max & \sum_k b_k y_k \\
\text{s.t.} \;\; & C - \sum A_k y_k \in \mathcal{S}_+
\end{align}
```

If we allow ``A_k`` to be non-symmetric, we should instead use:
```math
\begin{align}
       \min & \langle C, X \rangle \\
\text{s.t.} \;\; & \langle A_k, X\rangle = b_k \forall k \\
            & X                     \in \mathcal{D}_+
\end{align}
```
with the dual
```math
\begin{align}
       \max & \sum b_k y_k \\
\text{s.t.} \;\; & C - \sum A_k y_k \in \mathcal{P}_+
\end{align}
```

This is implemented as:
```math
\begin{align}
       \min & \langle C,   Z \rangle + \langle C - C^\top, S \rangle \\
\text{s.t.} \;\; & \langle A_k, Z \rangle + \langle A_k - A_k^\top, S \rangle = b_k \forall k \\
            & Z \in \mathcal{S}_+
\end{align}
```
with the dual
```math
\begin{align}
       \max & \sum b_k y_k \\
\text{s.t.} \;\; & C+C^\top - \sum (A_k+A_k^\top) y_k \in \mathcal{S}_+ \\
            & C-C^\top - \sum(A_k-A_k^\top) y_k = 0
\end{align}
```
and we recover ``Z = X + X^\top``.
