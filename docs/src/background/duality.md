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
