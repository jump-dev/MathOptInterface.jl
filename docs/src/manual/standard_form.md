```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
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
    For more information on this standard form, read [our paper](https://arxiv.org/pdf/2002.03447.pdf).

MOI defines some commonly used functions and sets, but the interface is
extensible to other sets recognized by the solver.

## Functions

The function types implemented in MathOptInterface.jl are:

* [`VariableIndex`](@ref): ``x_j``, i.e., projection onto a single coordinate
  defined by a variable index ``j``.
* [`VectorOfVariables`](@ref): projection onto multiple coordinates (i.e.,
  extracting a subvector).
* [`ScalarAffineFunction`](@ref): ``a^T x + b``, where ``a`` is a vector and
  ``b`` scalar.
* [`VectorAffineFunction`](@ref): ``A x + b``, where ``A`` is a matrix and
  ``b`` is a vector.
* [`ScalarQuadraticFunction`](@ref): ``\frac{1}{2} x^T Q x + a^T x + b``,
  where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant.
* [`VectorQuadraticFunction`](@ref): a vector of scalar-valued quadratic
  functions.

Extensions for nonlinear programming are present but not yet well documented.

## One-dimensional sets

The one-dimensional set types implemented in MathOptInterface.jl are:

* [`LessThan(upper)`](@ref MathOptInterface.LessThan):
  ``\{ x \in \mathbb{R} : x \le \mbox{upper} \}``
* [`GreaterThan(lower)`](@ref MathOptInterface.GreaterThan):
  ``\{ x \in \mathbb{R} : x \ge \mbox{lower} \}``
* [`EqualTo(value)`](@ref MathOptInterface.GreaterThan):
  ``\{ x \in \mathbb{R} : x = \mbox{value} \}``
* [`Interval(lower, upper)`](@ref MathOptInterface.Interval):
  ``\{ x \in \mathbb{R} : x \in [\mbox{lower},\mbox{upper}] \}``
* [`Integer()`](@ref MathOptInterface.Integer): ``\mathbb{Z}``
* [`ZeroOne()`](@ref MathOptInterface.ZeroOne): ``\{ 0, 1 \}``
* [`Semicontinuous(lower,upper)`](@ref MathOptInterface.Semicontinuous):
  ``\{ 0\} \cup [\mbox{lower},\mbox{upper}]``
* [`Semiinteger(lower,upper)`](@ref MathOptInterface.Semiinteger):
  ``\{ 0\} \cup \{\mbox{lower},\mbox{lower}+1,\ldots,\mbox{upper}-1,\mbox{upper}\}``

## Vector cones

The vector-valued set types implemented in MathOptInterface.jl are:

* [`Reals(dimension)`](@ref MathOptInterface.Reals):
  ``\mathbb{R}^\mbox{dimension}``
* [`Zeros(dimension)`](@ref MathOptInterface.Zeros): ``0^\mbox{dimension}``
* [`Nonnegatives(dimension)`](@ref MathOptInterface.Nonnegatives):
  ``\{ x \in \mathbb{R}^\mbox{dimension} : x \ge 0 \}``
* [`Nonpositives(dimension)`](@ref MathOptInterface.Nonpositives):
  ``\{ x \in \mathbb{R}^\mbox{dimension} : x \le 0 \}``
* [`SecondOrderCone(dimension)`](@ref MathOptInterface.SecondOrderCone):
  ``\{ (t,x) \in \mathbb{R}^\mbox{dimension} : t \ge \lVert x \rVert_2 \}``
* [`RotatedSecondOrderCone(dimension)`](@ref MathOptInterface.RotatedSecondOrderCone):
  ``\{ (t,u,x) \in \mathbb{R}^\mbox{dimension} : 2tu \ge \lVert x \rVert_2^2, t,u \ge 0 \}``
* [`ExponentialCone()`](@ref MathOptInterface.ExponentialCone):
  ``\{ (x,y,z) \in \mathbb{R}^3 : y \exp (x/y) \le z, y > 0 \}``
* [`DualExponentialCone()`](@ref MathOptInterface.DualExponentialCone):
  ``\{ (u,v,w) \in \mathbb{R}^3 : -u \exp (v/u) \le \exp(1) w, u < 0 \}``
* [`GeometricMeanCone(dimension)`](@ref MathOptInterface.GeometricMeanCone):
  ``\{ (t,x) \in \mathbb{R}^{n+1} : x \ge 0, t \le \sqrt[n]{x_1 x_2 \cdots x_n} \}``
  where ``n`` is ``\mbox{dimension} - 1``
* [`PowerCone(exponent)`](@ref MathOptInterface.PowerCone):
  ``\{ (x,y,z) \in \mathbb{R}^3 : x^\mbox{exponent} y^{1-\mbox{exponent}} \ge |z|, x,y \ge 0 \}``
* [`DualPowerCone(exponent)`](@ref MathOptInterface.DualPowerCone):
  ``\{ (u,v,w) \in \mathbb{R}^3 : \frac{u}{\mbox{exponent}}^\mbox{exponent}\frac{v}{1-\mbox{exponent}}^{1-\mbox{exponent}} \ge |w|, u,v \ge 0 \}``
* [`NormOneCone(dimension)`](@ref MathOptInterface.NormOneCone): ``\{ (t,x) \in \mathbb{R}^\mbox{dimension} : t \ge \lVert x \rVert_1 \}`` where ``\lVert x \rVert_1 = \sum_i \lvert x_i \rvert``
* [`NormInfinityCone(dimension)`](@ref MathOptInterface.NormInfinityCone):
  ``\{ (t,x) \in \mathbb{R}^\mbox{dimension} : t \ge \lVert x \rVert_\infty \}`` where ``\lVert x \rVert_\infty = \max_i \lvert x_i \rvert``.
* [`RelativeEntropyCone(dimension)`](@ref MathOptInterface.RelativeEntropyCone):
  ``\{ (u, v, w) \in \mathbb{R}^\mbox{dimension} : u \ge \sum_i w_i \log (\frac{w_i}{v_i}), v_i \ge 0, w_i \ge 0 \}``

## Matrix cones

The matrix-valued set types implemented in MathOptInterface.jl are:

* [`RootDetConeTriangle(dimension)`](@ref MathOptInterface.RootDetConeTriangle):
  ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}(1+\mbox{dimension})/2} : t \le \det(X)^{1/\mbox{dimension}}, X \mbox{ is the upper triangle of a PSD matrix} \}``
* [`RootDetConeSquare(dimension)`](@ref MathOptInterface.RootDetConeSquare):
  ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}^2} : t \le \det(X)^{1/\mbox{dimension}}, X \mbox{ is a PSD matrix} \}``

* [`PositiveSemidefiniteConeTriangle(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle):
  ``\{ X \in \mathbb{R}^{\mbox{dimension}(\mbox{dimension}+1)/2} : X \mbox{ is the upper triangle of a PSD matrix} \}``
* [`PositiveSemidefiniteConeSquare(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeSquare):
  ``\{ X \in \mathbb{R}^{\mbox{dimension}^2} : X \mbox{ is a PSD matrix} \}``

* [`LogDetConeTriangle(dimension)`](@ref MathOptInterface.LogDetConeTriangle):
  ``\{ (t,u,X) \in \mathbb{R}^{2+\mbox{dimension}(1+\mbox{dimension})/2} : t \le u\log(\det(X/u)), X \mbox{ is the upper triangle of a PSD matrix}, u > 0 \}``
* [`LogDetConeSquare(dimension)`](@ref MathOptInterface.LogDetConeSquare):
  ``\{ (t,u,X) \in \mathbb{R}^{2+\mbox{dimension}^2} : t \le u \log(\det(X/u)), X \mbox{ is a PSD matrix}, u > 0 \}``

* [`NormSpectralCone(row_dim, column_dim)`](@ref MathOptInterface.NormSpectralCone):
  ``\{ (t, X) \in \mathbb{R}^{1 + \mbox{row_dim} \times \mbox{column_dim}} : t \ge \sigma_1(X), X \mbox{ is a matrix with row_dim rows and column_dim columns} \}``
* [`NormNuclearCone(row_dim, column_dim)`](@ref MathOptInterface.NormNuclearCone):
  ``\{ (t, X) \in \mathbb{R}^{1 + \mbox{row_dim} \times \mbox{column_dim}} : t \ge \sum_i \sigma_i(X), X \mbox{ is a matrix with row_dim rows and column_dim columns} \}``

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

* [`SOS1(weights)`](@ref MathOptInterface.SOS1):
  A special ordered set of Type I.
* [`SOS2(weights)`](@ref MathOptInterface.SOS2):
  A special ordered set of Type II.
* [`Indicator(set)`](@ref MathOptInterface.Indicator):
  A set to specify indicator constraints.
* [`Complements(dimension)`](@ref MathOptInterface.Complements):
  A set for mixed complementarity constraints.
