# Sets

"""
    AbstractSet

Abstract supertype for set objects used to encode constraints.
"""
abstract type AbstractSet end

"""
    AbstractScalarSet

Abstract supertype for subsets of ``\\mathbb{R}``.
"""
abstract type AbstractScalarSet <: AbstractSet end

"""
    AbstractVectorSet

Abstract supertype for subsets of ``\\mathbb{R}^n`` for some ``n``.
"""
abstract type AbstractVectorSet <: AbstractSet end

"""
    dimension(s::AbstractVectorSet)

Return the underlying dimension (number of vector components) in the set `s`, i.e.,
``n`` if the set is a subset of ``\\mathbb{R}^n``.
"""
dimension(s::AbstractVectorSet) = s.dimension # .dimension field is conventional, overwrite this method if not applicable

"""
    Reals(dimension)

The set ``\\mathbb{R}^{dimension}`` (containing all points) of dimension `dimension`.
"""
struct Reals <: AbstractVectorSet
    dimension::Int
end

"""
    Zeros(dimension)

The set ``\\{ 0 \\}^{dimension}`` (containing only the origin) of dimension `dimension`.
"""
struct Zeros <: AbstractVectorSet
    dimension::Int
end

"""
    Nonnegatives(dimension)

The nonnegative orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\ge 0 \\}`` of dimension `dimension`.
"""
struct Nonnegatives <: AbstractVectorSet
    dimension::Int
end

"""
    Nonpositives(dimension)

The nonpositive orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\le 0 \\}`` of dimension `dimension`.
"""
struct Nonpositives <: AbstractVectorSet
    dimension::Int
end

"""
    GreaterThan{T <: Real}(lower::T)

The set ``[lower,\\infty) \\subseteq \\mathbb{R}``.
"""
struct GreaterThan{T <: Real} <: AbstractScalarSet
    lower::T
end

"""
    LessThan{T <: Real}(upper::T)

The set ``(-\\infty,upper] \\subseteq \\mathbb{R}``.
"""
struct LessThan{T <: Real} <: AbstractScalarSet
    upper::T
end

"""
    EqualTo{T <: Number}(value::T)

The set containing the single point ``x \\in \\mathbb{R}`` where ``x`` is given by `value`.
"""
struct EqualTo{T <: Number} <: AbstractScalarSet
    value::T
end

"""
    Interval{T <: Real}(lower::T,upper::T)

The interval ``[lower, upper] \\subseteq \\mathbb{R}``.
If `lower` or `upper` is `-Inf` or `Inf`, respectively, the set is interpreted as a one-sided interval.

    Interval(s::GreaterThan{<:AbstractFloat})

Construct a (right-unbounded) `Interval` equivalent to the given [`GreaterThan`](@ref) set.

    Interval(s::LessThan{<:AbstractFloat})

Construct a (left-unbounded) `Interval` equivalent to the given [`LessThan`](@ref) set.

    Interval(s::EqualTo{<:Real})

Construct a (degenerate) `Interval` equivalent to the given [`EqualTo`](@ref) set.
"""
struct Interval{T <: Real} <: AbstractScalarSet
    lower::T
    upper::T
end
Interval(s::GreaterThan{<:AbstractFloat}) = Interval(s.lower, typemax(s.lower))
Interval(s::LessThan{<:AbstractFloat}) = Interval(typemin(s.upper), s.upper)
Interval(s::EqualTo{<:Real}) = Interval(s.value, s.value)

"""
    SecondOrderCone(dimension)

The second-order cone (or Lorenz cone) ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge || x ||_2 \\}`` of dimension `dimension`.
"""
struct SecondOrderCone <: AbstractVectorSet
    dimension::Int
end

"""
    RotatedSecondOrderCone(dimension)

The rotated second-order cone ``\\{ (t,u,x) \\in \\mathbb{R}^{dimension} : 2tu \\ge || x ||_2^2, t,u \\ge 0 \\}`` of dimension `dimension`.
"""
struct RotatedSecondOrderCone <: AbstractVectorSet
    dimension::Int
end

"""
    GeometricMeanCone(dimension)

The geometric mean cone ``\\{ (t,x) \\in \\mathbb{R}^{n+1} : x \\ge 0, t \\le \\sqrt[n]{x_1 x_2 \\cdots x_n} \\}`` of dimension `dimension```{}=n+1``.
"""
struct GeometricMeanCone <: AbstractVectorSet
    dimension::Int
end

"""
    ExponentialCone()

The 3-dimensional exponential cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : y \\exp (x/y) \\le z, y > 0 \\}``.
"""
struct ExponentialCone <: AbstractVectorSet end

"""
    DualExponentialCone()

The 3-dimensional dual exponential cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : -u \\exp (v/u) \\le \\exp(1) w, u < 0 \\}``.
"""
struct DualExponentialCone <: AbstractVectorSet end

"""
    PowerCone{T <: Real}(exponent::T)

The 3-dimensional power cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : x^{exponent} y^{1-exponent} >= |z|, x \\ge 0, y \\ge 0 \\}`` with parameter `exponent`.
"""
struct PowerCone{T <: Real} <: AbstractVectorSet
    exponent::T
end

"""
    DualPowerCone{T <: Real}(exponent::T)

The 3-dimensional power cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : (\\frac{u}{exponent})^{exponent} (\\frac{v}{1-exponent})^{1-exponent} \\ge |w|, u \\ge 0, v \\ge 0 \\}`` with parameter `exponent`.
"""
struct DualPowerCone{T <: Real} <: AbstractVectorSet
    exponent::T
end

dimension(s::Union{ExponentialCone, DualExponentialCone, PowerCone, DualPowerCone}) = 3

"""
    PositiveSemidefiniteConeTriangle(dimension)

The (vectorized) cone of symmetric positive semidefinite matrices, with side length `dimension` and with off-diagonals unscaled.
The entries of the upper triangular part of the matrix are given column by column (or equivalently, the entries of the lower triangular part are given row by row).
An ``dimension \\times dimension`` matrix has ``dimension(dimension+1)/2`` lower-triangular elements, so for the vectorized cone of dimension ``n``, the corresponding symmetric matrix has side dimension ``\\sqrt{1/4 + 2 n} - 1/2`` elements.

### Examples

The matrix
```math
\\begin{bmatrix}
  1 & 2 & 4\\\\
  2 & 3 & 5\\\\
  4 & 5 & 6
\\end{bmatrix}
```
corresponds to ``(1, 2, 3, 4, 5, 6)`` for `PositiveSemidefiniteConeTriangle(3)`

### Note

Two packed storage formats exist for symmetric matrices, the respective orders of the entries are:
- upper triangular column by column (or lower triangular row by row);
- lower triangular column by column (or upper triangular row by row).

The advantage of the first format is the mapping between the `(i, j)` matrix indices and the `k` index of the vectorized form. It is simpler and does not depend on the dimension of the matrix.
Indeed,
- the entry of matrix indices `(i, j)` has vectorized index `k = div((j-1)*j, 2) + i` if ``i \\leq j`` and `k = div((i-1)*i, 2) + j` if ``j \\leq i``;
- and the entry with vectorized index `k` has matrix indices `i = isqrt(2k)` and `j = k - div((i-1)*i, 2)` or `j = isqrt(2k)` and `i = k - div((j-1)*j, 2)`.

### Duality note
The scalar product for the symmetric matrix in its vectorized form is the sum of the pairwise product of the diagonal entries plus twice the sum of the pairwise product of the upper diagonal entries; see [p. 634, 1].
This has important consequence for duality.
Consider for example the following problem
```math
\\begin{align*}
    & \\max_{x \\in \\mathbb{R}} & x
    \\\\
    & \\;\\;\\text{s.t.} &
    (1, -x, 1) & \\in \\text{PositiveSemidefiniteConeTriangle}(2).
\\end{align*}
```
The dual is the following problem
```math
\\begin{align*}
    & \\min_{x \\in \\mathbb{R}^3} & y_1 + y_3
    \\\\
    & \\;\\;\\text{s.t.} & 2y_2 & = 1\\\\
    & & y & \\in \\text{PositiveSemidefiniteConeTriangle}(2).
\\end{align*}
```
Why do we use ``2y_2`` in the dual constraint instead of ``y_2`` ?
The reason is that ``2y_2`` is the scalar product between ``y`` and the symmetric matrix whose vectorized form is ``(0, 1, 0)``. Indeed, with our modified scalar products we have
```math
\\langle
(0, 1, 0),
(y_1, y_2, y_3)
\\rangle
=
\\mathrm{trace}
\\begin{pmatrix}
  0 & 1\\\\
  1 & 0
\\end{pmatrix}
\\begin{pmatrix}
  y_1 & y_2\\\\
  y_2 & y_3
\\end{pmatrix}
= 2y_2.
```

### References

[1] Boyd, S. and Vandenberghe, L.. *Convex optimization*. Cambridge university press, 2004.
"""
struct PositiveSemidefiniteConeTriangle <: AbstractVectorSet
    dimension::Int
end

dimension(s::PositiveSemidefiniteConeTriangle) = div(s.dimension * (s.dimension + 1), 2)

"""
    PositiveSemidefiniteConeSquare(dimension)

The cone of symmetric positive semidefinite matrices, with side length `dimension`.
The entries of the matrix are given column by column (or equivalently, row by row).
The matrix is both constrained to be symmetric and to be positive semidefinite.
That is, if the functions in entries ``(i, j)`` and ``(j, i)`` are different, then a constraint will be added to make sure that the entries are equal.

### Examples

Constraining the matrix
```math
\\begin{bmatrix}
  1 & -y\\\\
  -z & 0\\\\
\\end{bmatrix}
```
to be symmetric positive semidefinite can be achieved by constraining the vector ``(1, -z, -y, 0)`` (or ``(1, -y, -z, 0)``)
to belong to the `PositiveSemidefiniteConeSquare(2)`.
It both constrains ``y = z`` and ``(1, -y, 0)`` (or ``(1, -z, 0)``) to be in `PositiveSemidefiniteConeTriangle(2)`.
"""
struct PositiveSemidefiniteConeSquare <: AbstractVectorSet
    dimension::Int
end

dimension(s::PositiveSemidefiniteConeSquare) = s.dimension^2

"""
    LogDetConeTriangle(dimension)

The Log-Determinant cone ``\\{ (t, X) \\in \\mathbb{R}^{1 + d(d+1)/2} : t \\le \\log(\\det(X)) \\}`` where the matrix `X` is represented in the same symmetric packed format as in the `PositiveSemidefiniteConeTriangle`.
The argument `dimension` is the side dimension of the matrix `X`, i.e., its number of rows or columns.
"""
struct LogDetConeTriangle <: AbstractVectorSet
    dimension::Int
end

"""
    LogDetConeSquare(dimension)

The Log-Determinant cone ``\\{ (t, X) \\in \\mathbb{R}^{1 + d^2} : t \\le \\log(\\det(X)), X \\text{ symmetric} \\}`` where the matrix `X` is represented in the same format as in the `PositiveSemidefiniteConeSquare`.
Similarly to `PositiveSemidefiniteConeSquare`, constraints are added to ensures that `X` is symmetric.
The argument `dimension` is the side dimension of the matrix `X`, i.e., its number of rows or columns.
"""
struct LogDetConeSquare <: AbstractVectorSet
    dimension::Int
end

"""
    RootDetConeTriangle(dimension)

The Root-Determinant cone ``\\{ (t, X) \\in \\mathbb{R}^{1 + d(d+1)/2} : t \\le \\det(X)^{1/d} \\}`` where the matrix `X` is represented in the same symmetric packed format as in the `PositiveSemidefiniteConeTriangle`.
The argument `dimension` is the side dimension of the matrix `X`, i.e., its number of rows or columns.
"""
struct RootDetConeTriangle <: AbstractVectorSet
    dimension::Int
end

"""
    RootDetConeSquare(dimension)

The Root-Determinant cone ``\\{ (t, X) \\in \\mathbb{R}^{1 + d^2} : t \\le \\det(X)^{1/d}, X \\text{ symmetric} \\}`` where the matrix `X` is represented in the same format as in the `PositiveSemidefiniteConeSquare`.
Similarly to `PositiveSemidefiniteConeSquare`, constraints are added to ensures that `X` is symmetric.
The argument `dimension` is the side dimension of the matrix `X`, i.e., its number of rows or columns.
"""
struct RootDetConeSquare <: AbstractVectorSet
    dimension::Int
end

dimension(s::Union{LogDetConeTriangle, RootDetConeTriangle}) = 1 + div(s.dimension * (s.dimension + 1), 2)
dimension(s::Union{LogDetConeSquare, RootDetConeSquare}) = 1 + s.dimension^2

"""
    Integer()

The set of integers ``\\mathbb{Z}``.
"""
struct Integer <: AbstractScalarSet end

"""
    ZeroOne()

The set ``\\{ 0, 1 \\}``.
"""
struct ZeroOne <: AbstractScalarSet end

"""
    Semicontinuous{T <: Real}(lower::T,upper::T)

The set ``\\{0\\} \\cup [lower,upper]``.
"""
struct Semicontinuous{T <: Real} <: AbstractScalarSet
    lower::T
    upper::T
end

"""
    Semiinteger{T <: Real}(lower::T,upper::T)

The set ``\\{0\\} \\cup \\{lower,lower+1,\\ldots,upper-1,upper\\}``.
"""
struct Semiinteger{T <: Real} <: AbstractScalarSet
    lower::T
    upper::T
end

"""
    SOS1{T <: Real}(weights::Vector{T})

The set corresponding to the special ordered set (SOS) constraint of type 1.
Of the variables in the set, at most one can be nonzero.
The `weights` induce an ordering of the variables; as such, they should be unique values.
The *k*th element in the set corresponds to the *k*th weight in `weights`.
See [here](http://lpsolve.sourceforge.net/5.5/SOS.htm) for a description of SOS constraints and their potential uses.
"""
struct SOS1{T <: Real} <: AbstractVectorSet
    weights::Vector{T}
end

"""
    SOS2{T <: Real}(weights::Vector{T})

The set corresponding to the special ordered set (SOS) constraint of type 2.
Of the variables in the set, at most two can be nonzero, and if two are nonzero, they must be adjacent in the ordering of the set.
The `weights` induce an ordering of the variables; as such, they should be unique values.
The *k*th element in the set corresponds to the *k*th weight in `weights`.
See [here](http://lpsolve.sourceforge.net/5.5/SOS.htm) for a description of SOS constraints and their potential uses.
"""
struct SOS2{T <: Real} <: AbstractVectorSet
    weights::Vector{T}
end

dimension(s::Union{SOS1, SOS2}) = length(s.weights)
