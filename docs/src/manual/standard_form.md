```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Standard form problem

MathOptInterface represents optimization problems in the standard form:
```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```
where:
* the functions ``f_0, f_1, \ldots, f_m`` are specified by
  [`AbstractFunction`](@ref) objects
* the sets ``\mathcal{S}_1, \ldots, \mathcal{S}_m`` are specified by
  [`AbstractSet`](@ref) objects

!!! tip
    For more information on this standard form, read [our paper](https://arxiv.org/abs/2002.03447).

MOI defines some commonly used functions and sets, but the interface is
extensible to other sets recognized by the solver.

## Functions

The function types implemented in MathOptInterface.jl are:

| Function         | Description |
| :--------------- | :---------- |
| [`VariableIndex`](@ref) | ``x_j``, the projection onto a single coordinate defined by a variable index ``j``. |
| [`VectorOfVariables`](@ref) | The projection onto multiple coordinates (that is, extracting a sub-vector). |
| [`ScalarAffineFunction`](@ref) | ``a^T x + b``, where ``a`` is a vector and ``b`` scalar. |
| [`ScalarNonlinearFunction`](@ref) | ``f(x)``, where ``f`` is a nonlinear function. |
| [`VectorAffineFunction`](@ref) | ``A x + b``, where ``A`` is a matrix and ``b`` is a vector. |
| [`ScalarQuadraticFunction`](@ref) | ``\frac{1}{2} x^T Q x + a^T x + b``, where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant. |
| [`VectorQuadraticFunction`](@ref) | A vector of scalar-valued quadratic functions. |
| [`VectorNonlinearFunction`](@ref) | ``f(x)``, where ``f`` is a vector-valued nonlinear function. |

Extensions for nonlinear programming are present but not yet well documented.

## One-dimensional sets

The one-dimensional set types implemented in MathOptInterface.jl are:

| Set                                                            | Description                            |
| :------------------------------------------------------------- | :------------------------------------- |
| [`LessThan(u)`](@ref MathOptInterface.LessThan)                | ``(-\infty, u]``                       |
| [`GreaterThan(l)`](@ref MathOptInterface.GreaterThan)          | ``[l, \infty)``                        |
| [`EqualTo(v)`](@ref MathOptInterface.GreaterThan)              | ``\{v\}``                              |
| [`Interval(l, u)`](@ref MathOptInterface.Interval)             | ``[l, u]``                             |
| [`Integer()`](@ref MathOptInterface.Integer)                   | ``\mathbb{Z}``                         |
| [`ZeroOne()`](@ref MathOptInterface.ZeroOne)                   | ``\{ 0, 1 \}``                         |
| [`Semicontinuous(l, u)`](@ref MathOptInterface.Semicontinuous) | ``\{ 0\} \cup [l, u]``                 |
| [`Semiinteger(l, u)`](@ref MathOptInterface.Semiinteger)       | ``\{ 0\} \cup \{l,l+1,\ldots,u-1,u\}`` |

## Vector cones

The vector-valued set types implemented in MathOptInterface.jl are:

| Set                                                                   | Description |
| :---------------------------------------------------------------------| :---------- |
| [`Reals(d)`](@ref MathOptInterface.Reals)                             | ``\mathbb{R}^{d}`` |
| [`Zeros(d)`](@ref MathOptInterface.Zeros)                             | ``0^{d}`` |
| [`Nonnegatives(d)`](@ref MathOptInterface.Nonnegatives)               | ``\{ x \in \mathbb{R}^{d} : x \ge 0 \}`` |
| [`Nonpositives(d)`](@ref MathOptInterface.Nonpositives)               | ``\{ x \in \mathbb{R}^{d} : x \le 0 \}`` |
| [`SecondOrderCone(d)`](@ref MathOptInterface.SecondOrderCone)         | ``\{ (t,x) \in \mathbb{R}^{d} : t \ge \lVert x \rVert_2 \}`` |
| [`RotatedSecondOrderCone(d)`](@ref MathOptInterface.RotatedSecondOrderCone) | ``\{ (t,u,x) \in \mathbb{R}^{d} : 2tu \ge \lVert x \rVert_2^2, t \ge 0,u \ge 0 \}`` |
| [`ExponentialCone()`](@ref MathOptInterface.ExponentialCone)          | ``\{ (x,y,z) \in \mathbb{R}^3 : y \exp (x/y) \le z, y > 0 \}`` |
| [`DualExponentialCone()`](@ref MathOptInterface.DualExponentialCone)  | ``\{ (u,v,w) \in \mathbb{R}^3 : -u \exp (v/u) \le \exp(1) w, u < 0 \}`` |
| [`GeometricMeanCone(d)`](@ref MathOptInterface.GeometricMeanCone)     | ``\{ (t,x) \in \mathbb{R}^{1+n} : x \ge 0, t \le \sqrt[n]{x_1 x_2 \cdots x_n} \}`` where ``n`` is ``d - 1`` |
| [`PowerCone(α)`](@ref MathOptInterface.PowerCone)                     | ``\{ (x,y,z) \in \mathbb{R}^3 : x^{\alpha} y^{1-\alpha} \ge \|z\|, x \ge 0,y \ge 0 \}`` |
| [`DualPowerCone(α)`](@ref MathOptInterface.DualPowerCone)             | ``\{ (u,v,w) \in \mathbb{R}^3 : \left(\frac{u}{\alpha}\right)^{\alpha}\left(\frac{v}{1-\alpha}\right)^{1-\alpha} \ge \|w\|, u,v \ge 0 \}`` |
| [`NormOneCone(d)`](@ref MathOptInterface.NormOneCone)                 | ``\{ (t,x) \in \mathbb{R}^{d} : t \ge \sum_i \lvert x_i \rvert \}`` |
| [`NormInfinityCone(d)`](@ref MathOptInterface.NormInfinityCone)       | ``\{ (t,x) \in \mathbb{R}^{d} : t \ge \max_i \lvert x_i \rvert \}`` |
| [`RelativeEntropyCone(d)`](@ref MathOptInterface.RelativeEntropyCone) | ``\{ (u, v, w) \in \mathbb{R}^{d} : u \ge \sum_i w_i \log (\frac{w_i}{v_i}), v_i \ge 0, w_i \ge 0 \}`` |
| [`HyperRectangle(l, u)`](@ref MathOptInterface.HyperRectangle)        | ``\{x \in \bar{\mathbb{R}}^d: x_i \in [l_i, u_i] \forall i=1,\ldots,d\}`` |
| [`NormCone(p, d)`](@ref MathOptInterface.NormCone)       | ``\{ (t,x) \in \mathbb{R}^{d} : t \ge \left(\sum\limits_i \lvert x_i \rvert^p\right)^{\frac{1}{p}} \}`` |
| [`VectorNonlinearOracle`](@ref MathOptInterface.VectorNonlinearOracle)| ``\{x \in \mathbb{R}^{dimension}: l \le f(x) \le u \}`` |

## Matrix cones

The matrix-valued set types implemented in MathOptInterface.jl are:

| Set              | Description |
| :--------------- | :----------- |
| [`RootDetConeTriangle(d)`](@ref MathOptInterface.RootDetConeTriangle) | ``\{ (t,X) \in \mathbb{R}^{1+d(1+d)/2} : t \le \det(X)^{1/d}, X \mbox{ is the upper triangle of a PSD matrix} \}`` |
| [`RootDetConeSquare(d)`](@ref MathOptInterface.RootDetConeSquare)     | ``\{ (t,X) \in \mathbb{R}^{1+d^2} : t \le \det(X)^{1/d}, X \mbox{ is a PSD matrix} \}`` |
| [`PositiveSemidefiniteConeTriangle(d)`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle) | ``\{ X \in \mathbb{R}^{d(d+1)/2} : X \mbox{ is the upper triangle of a PSD matrix} \}`` |
| [`PositiveSemidefiniteConeSquare(d)`](@ref MathOptInterface.PositiveSemidefiniteConeSquare) | ``\{ X \in \mathbb{R}^{d^2} : X \mbox{ is a PSD matrix} \}`` |
| [`LogDetConeTriangle(d)`](@ref MathOptInterface.LogDetConeTriangle)   | ``\{ (t,u,X) \in \mathbb{R}^{2+d(1+d)/2} : t \le u\log(\det(X/u)), X \mbox{ is the upper triangle of a PSD matrix}, u > 0  \}`` |
| [`LogDetConeSquare(d)`](@ref MathOptInterface.LogDetConeSquare)       | ``\{ (t,u,X) \in \mathbb{R}^{2+d^2} : t \le u \log(\det(X/u)), X \mbox{ is a PSD matrix}, u > 0 \}`` |
| [`NormSpectralCone(r, c)`](@ref MathOptInterface.NormSpectralCone)    | ``\{ (t, X) \in \mathbb{R}^{1 + r \times c} : t \ge \sigma_1(X), X \mbox{ is a } r\times c\mbox{ matrix} \}``
| [`NormNuclearCone(r, c)`](@ref MathOptInterface.NormNuclearCone)      | ``\{ (t, X) \in \mathbb{R}^{1 + r \times c} : t \ge \sum_i \sigma_i(X), X \mbox{ is a } r\times c\mbox{ matrix} \}`` |
| [`HermitianPositiveSemidefiniteConeTriangle(d)`](@ref MathOptInterface.HermitianPositiveSemidefiniteConeTriangle) | The cone of Hermitian positive semidefinite matrices, with
`side_dimension` rows and columns. |
| [`Scaled(S)`](@ref MathOptInterface.Scaled) | The set `S` scaled so that [`Utilities.set_dot`](@ref MathOptInterface.Utilities.set_dot) corresponds to `LinearAlgebra.dot` |

Some of these cones can take two forms: `XXXConeTriangle` and `XXXConeSquare`.

In `XXXConeTriangle` sets, the matrix is assumed to be symmetric, and the
elements are provided by a vector, in which the entries of the upper-right
triangular part of the matrix are given column by column (or equivalently, the
entries of the lower-left triangular part are given row by row).

In `XXXConeSquare` sets, the entries of the matrix are given column by column
(or equivalently, row by row), and the matrix is constrained to be symmetric. As
an example, given a 2-by-2 matrix of variables `X` and a one-dimensional
variable `t`, we can specify a root-det constraint as
`[t, X11, X12, X22] ∈ RootDetConeTriangle` or
`[t, X11, X12, X21, X22] ∈ RootDetConeSquare`.

We provide both forms to enable flexibility for solvers who may natively support
one or the other. Transformations between `XXXConeTriangle` and `XXXConeSquare`
are handled by bridges, which removes the chance of conversion mistakes by users
or solver developers.

## Multi-dimensional sets with combinatorial structure

Other sets are vector-valued, with a particular combinatorial structure. Read
their docstrings for more information on how to interpret them.

| Set                        | Description |
| :------------------------- | :---------- |
| [`SOS1`](@ref)             | A Special Ordered Set (SOS) of Type I |
| [`SOS2`](@ref)             | A Special Ordered Set (SOS) of Type II |
| [`Indicator`](@ref)        | A set to specify an indicator constraint |
| [`Complements`](@ref)      | A set to specify a mixed complementarity constraint |
| [`AllDifferent`](@ref)     | The `all_different` global constraint |
| [`BinPacking`](@ref)       | The `bin_packing` global constraint |
| [`Circuit`](@ref)          | The `circuit` global constraint |
| [`CountAtLeast`](@ref)     | The `at_least` global constraint |
| [`CountBelongs`](@ref)     | The `nvalue` global constraint |
| [`CountDistinct`](@ref)    | The `distinct` global constraint |
| [`CountGreaterThan`](@ref) | The `count_gt` global constraint |
| [`Cumulative`](@ref)       | The `cumulative` global constraint |
| [`Path`](@ref)             | The `path` global constraint |
| [`Table`](@ref)            | The `table` global constraint |
