
# Manual

## Concepts

The standard form problem is:

```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

where:
* objective function ``f_0`` is affine or quadratic
* constraint functions ``f_i`` is variable-wise, affine, or quadratic
* constraint sets ``\mathcal{S}_i`` are pre-defined real scalar or vector sets

Currently, all functions are described compactly with lists, vectors, and matrices.
The function types are:
* variable-wise: ``x_j``, a (scalar) variable
* affine: ``A_i x + b_i``, where ``A_i`` is a matrix and ``b_i`` is a vector
* quadratic: ``q_i(x) + A_i x + b_i``, where ``q_i(x)`` is a scalar quadratic expression of the form``\frac{1}{2} x^T Q_{i,k} x`` (for objective or constraints), or a vector of such quadratic expressions (for constraints only)

This API defines some commonly-used sets, but is extensible to other sets recognized by the solver.

## Duals

Currently, a convention for duals is not defined for problems with non-conic sets ``\mathcal{S}_i`` or quadratic functions ``f_0, f_i``. Note that bound constraints are supported by re-interpretation in terms of the nonnegative or nonpositive cones. An affine constraint ``a^T x + b \ge c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_+``, and similarly ``a^T x + b \le c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_-``. Variable-wise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of ``A_i``.

For such conic form minimization problems, the primal is:

```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

and the dual is:

```math
\begin{align}
& \max_{y_1, \ldots, y_m} & -\sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^T y_i \in {0}^n
\\
& & y_i & \in \mathcal{S}_i^* & i = 1 \ldots m
\end{align}
```

where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i`` is its dual cone.

Note:
* lower bounds have nonnegative duals
* upper bounds have nonpositive duals
* closed convex cones have duals belonging to the corresponding dual cones
