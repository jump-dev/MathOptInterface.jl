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
dimension(s::AbstractVectorSet) = s.dim # .dim field is conventional, overwrite this method if not applicable

"""
    Reals(dim)

The set ``\\mathbb{R}^{dim}`` (containing all points) of dimension `dim`.
"""
struct Reals <: AbstractVectorSet
    dim::Int
end

"""
    Zeros(dim)

The set ``\\{ 0 \\}^{dim}`` (containing only the origin) of dimension `dim`.
"""
struct Zeros <: AbstractVectorSet
    dim::Int
end

"""
    Nonnegatives(dim)

The nonnegative orthant ``\\{ x \\in \\mathbb{R}^{dim} : x \\ge 0 \\}`` of dimension `dim`.
"""
struct Nonnegatives <: AbstractVectorSet
    dim::Int
end

"""
    Nonpositives(dim)

The nonpositive orthant ``\\{ x \\in \\mathbb{R}^{dim} : x \\le 0 \\}`` of dimension `dim`.
"""
struct Nonpositives <: AbstractVectorSet
    dim::Int
end

"""
    GreaterThan(lower)

The set ``[lower,\\infty) \\subseteq \\mathbb{R}``.
"""
struct GreaterThan{T <: Real} <: AbstractScalarSet
    lower::T
end

"""
    LessThan(upper)

The set ``(-\\infty,upper] \\subseteq \\mathbb{R}``.
"""
struct LessThan{T <: Real} <: AbstractScalarSet
    upper::T
end

"""
    EqualTo(value)

The set containing the single point ``x \\in \\mathbb{R}`` where ``x`` is given by `value`.
"""
struct EqualTo{T <: Real} <: AbstractScalarSet
    value::T
end

"""
    Interval(lower,upper)

The interval ``[lower, upper] \\subseteq \\mathbb{R}``.
If `lower` or `upper` is `-Inf` or `Inf`, respectively, the set is interpreted as a one-sided interval.
"""
struct Interval{T <: Real} <: AbstractScalarSet
    lower::T
    upper::T
end

"""
    SecondOrderCone(dim)

The second-order cone (or Lorenz cone) ``\\{ (t,x) \\in \\mathbb{R}^{dim} : t \\ge || x ||_2 \\}`` of dimension `dim`.
"""
struct SecondOrderCone <: AbstractVectorSet
    dim::Int
end

"""
    RotatedSecondOrderCone(dim)

The rotated second-order cone ``\\{ (t,u,x) \\mathbb{R}^{dim} : 2tu \\ge || x ||_2^2, t,u \\ge 0 \\}`` of dimension `dim`.
"""
struct RotatedSecondOrderCone <: AbstractVectorSet
    dim::Int
end

"""
    ExponentialCone()

The 3-dimensional exponential cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : y \\exp (x/y) \\le z, y > 0 \\}``.
"""
struct ExponentialCone <: AbstractVectorSet
end

"""
    DualExponentialCone()

The 3-dimensional dual exponential cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : -u \\exp (v/u) \\le \\exp(1) w, u < 0 \\}``.
"""
struct DualExponentialCone <: AbstractVectorSet
end

"""
    PowerCone(a)

The 3-dimensional power cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : x^{a} y^{1-a} >= |z|, x \\ge 0, y \\ge 0 \\}`` with parameter `a`.
"""
struct PowerCone{T <: Real} <: AbstractVectorSet
    a::T
end

"""
    DualPowerCone(a)

The 3-dimensional power cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : (\\frac{u}{a})^a (\\frac{v}/{1-a})^{1-a} >= |w|, u \\ge 0, v \\ge 0 \\}`` with parameter `a`.
"""
struct DualPowerCone{T <: Real} <: AbstractVectorSet
    a::T
end

dimension(s::Union{ExponentialCone, DualExponentialCone, PowerCone, DualPowerCone}) = 3

"""
    PositiveSemidefiniteConeTriangle(dim)

The (vectorized) cone of symmetric positive semidefinite matrices, with off-diagonals unscaled.
The entries of the upper triangular part of the matrix are given row by row (or equivalently, the entries of the lower triangular part are given column by column).
An ``n \\times n`` matrix has ``n(n+1)/2`` lower-triangular elements, so for the vectorized cone of dimension `dim`, the corresponding symmetric matrix has side dimension ``\\sqrt (1/4 + 2 dim) - 1/2`` elements.
The scalar product is the sum of the pairwise product of the diagonal entries plus twice the sum of the pairwise product of the upper diagonal entries.

### Examples

The matrix
```math
\\begin{bmatrix}
  1 & 2 & 3\\\\
  2 & 4 & 5\\\\
  3 & 5 & 6
\\end{bmatrix}
```
corresponds to ``(1, 2, 3, 4, 5, 6)`` for `PositiveSemidefiniteConeTriangle`
"""
struct PositiveSemidefiniteConeTriangle <: AbstractVectorSet
    dim::Int
end

"""
    PositiveSemidefiniteConeScaled(dim)

The (vectorized) cone of symmetric positive semidefinite matrices, with off-diagonals scaled.
The entries of the upper triangular part of the matrix are given row by row (or equivalently, the entries of the lower triangular part are given column by column).
An ``n \\times n`` matrix has ``n(n+1)/2`` lower-triangular elements, so for the vectorized cone of dimension `dim`, the corresponding symmetric matrix has side dimension ``\\sqrt (1/4 + 2 dim) - 1/2`` elements.
The off-diagonal entries of the matrices of both the cone and its dual are scaled by ``\\sqrt{2}`` and the scalar product is simply the sum of the pairwise product of the entries.

### Examples

The matrix
```math
\\begin{bmatrix}
  1 & 2 & 3\\\\
  2 & 4 & 5\\\\
  3 & 5 & 6
\\end{bmatrix}
```
and to ``(1, 2\\sqrt{2}, 3\\sqrt{2}, 4, 5\\sqrt{2}, 6)`` for `PositiveSemidefiniteConeScaled`.
"""
struct PositiveSemidefiniteConeScaled <: AbstractVectorSet
    dim::Int
end

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
    Semicontinuous(l,u)

The set ``\\{0\\} \\cup [l,u]``.
"""
struct SemiContinuous{T <: Real} <: AbstractScalarSet
    l::T
    u::T
end

"""
    Semiinteger(l,u)

The set ``\\{0\\} \\cup \\{l,l+1,\\ldots,u-1,u\\}``.
"""
struct SemiInteger{T <: Real} <: AbstractScalarSet
    l::T
    u::T
end

"""
    SOS1(weights)

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
    SOS2(weights)

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
