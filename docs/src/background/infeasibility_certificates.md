```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Infeasibility certificates

When given a conic problem that is infeasible or unbounded, some solvers can
produce a certificate of infeasibility. This page explains what a certificate of
infeasibility is, and the related conventions that MathOptInterface adopts.

## Conic duality

MathOptInterface uses conic duality to define infeasibility certificates. A full
explanation is given in the section [Duality](@ref), but here is a brief
overview.

### Minimization problems

For a minimization problem in geometric conic form, the primal is:
```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^\top x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m,
\end{align}
```
and the dual is a maximization problem in standard conic form:
```math
\begin{align}
& \max_{y_1, \ldots, y_m} & -\sum_{i=1}^m b_i^\top y_i + b_0
\\
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^\top y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m,
\end{align}
```
where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i^*`` is
its dual cone.

### Maximization problems

For a maximization problem in geometric conic form, the primal is:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^\top x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m,
\end{align}
```
and the dual is a minimization problem in standard conic form:
```math
\begin{align}
& \min_{y_1, \ldots, y_m} & \sum_{i=1}^m b_i^\top y_i + b_0
\\
& \;\;\text{s.t.} & a_0 + \sum_{i=1}^m A_i^\top y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m.
\end{align}
```

## Unbounded problems

A problem is unbounded if and only if:
 1. there exists a feasible primal solution
 2. the dual is infeasible.

A feasible primal solution—if one exists—can be obtained by setting
[`ObjectiveSense`](@ref) to `FEASIBILITY_SENSE` before optimizing. Therefore,
most solvers terminate after they prove the dual is infeasible via a certificate
of dual infeasibility, but _before_ they have found a feasible primal solution.
This is also the reason that MathOptInterface defines the `DUAL_INFEASIBLE`
status instead of `UNBOUNDED`.

A certificate of dual infeasibility is an improving ray of the primal problem.
That is, there exists some vector ``d`` such that for all ``\eta > 0``:
```math
A_i (x + \eta d) + b_i \in \mathcal{C}_i,\ \ i = 1 \ldots m,
```
and (for minimization problems):
```math
a_0^\top (x + \eta d) + b_0 < a_0^\top x + b_0,
```
for any feasible point ``x``. The latter simplifies to ``a_0^\top d < 0``. For
maximization problems, the inequality is reversed, so that ``a_0^\top d > 0``.

If the solver has found a certificate of dual infeasibility:

 * [`TerminationStatus`](@ref) must be `DUAL_INFEASIBLE`
 * [`PrimalStatus`](@ref) must be `INFEASIBILITY_CERTIFICATE`
 * [`VariablePrimal`](@ref) must be the corresponding value of ``d``
 * [`ConstraintPrimal`](@ref) must be the corresponding value of ``A_i d``
 * [`ObjectiveValue`](@ref) must be the value ``a_0^\top d``. Note that this is
   the value of the objective function at `d`, ignoring the constant `b_0`.

!!! note
    The choice of whether to scale the ray ``d`` to have magnitude `1` is left
    to the solver.

## Infeasible problems

A certificate of primal infeasibility is an improving ray of the dual problem.
However, because infeasibility is independent of the objective function, we
first homogenize the primal problem by removing its objective.

For a minimization problem, a dual improving ray is some vector ``d`` such that
for all ``\eta > 0``:
```math
\begin{align}
-\sum_{i=1}^m A_i^\top (y_i + \eta d_i) & = 0 \\
(y_i + \eta d_i) & \in \mathcal{C}_i^* & i = 1 \ldots m,
\end{align}
```
and:
```math
-\sum_{i=1}^m b_i^\top (y_i + \eta d_i) > -\sum_{i=1}^m b_i^\top y_i,
```
for any feasible dual solution ``y``. The latter simplifies to
``-\sum_{i=1}^m b_i^\top d_i > 0``. For a maximization problem, the inequality
is ``\sum_{i=1}^m b_i^\top d_i < 0``. (Note that these are the same inequality,
modulo a `-` sign.)

If the solver has found a certificate of primal infeasibility:

 * [`TerminationStatus`](@ref) must be `INFEASIBLE`
 * [`DualStatus`](@ref) must be `INFEASIBILITY_CERTIFICATE`
 * [`ConstraintDual`](@ref) must be the corresponding value of ``d``
 * [`DualObjectiveValue`](@ref) must be the value
   ``-\sum_{i=1}^m b_i^\top d_i`` for minimization problems and
   ``\sum_{i=1}^m b_i^\top d_i`` for maximization problems.

!!! note
    The choice of whether to scale the ray ``d`` to have magnitude `1` is left
    to the solver.

### Infeasibility certificates of variable bounds

Many linear solvers (e.g., Gurobi) do not provide explicit access to the primal
infeasibility certificate of a variable bound. However, given a set of linear
constraints:
```math
\begin{align}
l_A \le A x \le u_A \\
l_x \le x \le u_x,
\end{align}
```
the primal certificate of the variable bounds can be computed using the primal
certificate associated with the affine constraints, ``d``. (Note that ``d`` will
have one element for each row of the ``A`` matrix, and that some or all of the
elements in the vectors ``l_A`` and ``u_A`` may be ``\pm \infty``. If both
``l_A`` and ``u_A`` are finite for some row, the corresponding element in ``d`
 must be `0`.)

Given ``d``, compute ``\bar{d} = d^\top A``. If the bound is finite, a
certificate for the lower variable bound of ``x_i`` is ``\max\{\bar{d}_i, 0\}``,
and a certificate for the upper variable bound is ``\min\{\bar{d}_i, 0\}``.
