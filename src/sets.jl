#=
    Sets defined by MathOptFormat. These are map closely to those defined in
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/sets.jl
    In most cases, the doc-strings are copied verbatim
=#

"""
    equalto(value)

The set containing the single point `value ∈ R`.

### Examples

    {
        "head": "EqualTo",
        "value": 3.0
    }
"""
equalto(value) = Object("head" => "EqualTo", "value"=> value)

"""
    lessthan(value)

The set `(-∞, value] ⊆ R`.

### Examples

    {
        "head": "LessThan",
        "value": 3.0
    }
"""
lessthan(value) = Object("head" => "LessThan", "value"=> value)

"""
    greaterthan(value)

The set `[value, ∞) ⊆ R`.

### Examples

    {
        "head": "GreaterThan",
        "value": 3.0
    }
"""
greaterthan(value) = Object("head" => "GreaterThan", "value"=> value)

"""
    interval(lower, upper)

The set `[lower, upper] ⊆ R`.

### Examples

    {
        "head": "Interval",
        "lower": 1.0,
        "upper": 2.0
    }
"""
interval(lower, upper) = Object("head" => "Interval", "lower" => lower, "upper" => upper)

"""
    integer()

The set of integers `Z`.

### Examples

    {
        "head": "Integer"
    }
"""
integer() = Object("head" => "Integer")

"""
    zeroone()

The set `{0, 1}`.

### Examples

    {
        "head": "ZeroOne"
    }
"""
zeroone() = Object("head" => "ZeroOne")

"""
    reals(dim)

The set `R^{dim}`.

### Examples

    {
        "head": "Reals",
        "dimension": 1
    }
"""
reals(dim::Int) = Object("head" => "Reals", "dimension" => dim)

"""
    zeros(dim)

The set `{0}^{dim}`.

### Examples

    {
        "head": "Zeros",
        "dimension": 1
    }
"""
zeros(dim::Int) = Object("head" => "Zeros", "dimension" => dim)

"""
    nonnegatives(dim)

The set `{x ∈ R^{dim}: x ≤ 0}`.

### Examples

    {
        "head": "Nonnegatives",
        "dimension": 1
    }
"""
nonnegatives(dim::Int) = Object("head" => "Nonnegatives", "dimension" => dim)

"""
    nonpositives(dim)

The set `{x ∈ R^{dim}: x ≥ 0}`.

### Examples

    {
        "head": "Nonpositives",
        "dimension": 1
    }
"""
nonpositives(dim::Int) = Object("head" => "Nonpositives", "dimension" => dim)

"""
    semicontinuous(lower, upper)

The set `{0} ∪ [lower, upper]`.

### Examples

    {
        "head": "Semicontinuous",
        "lower": 1,
        "upper": 2
    }
"""
semicontinuous(lower, upper) = Object("head" => "Semicontinuous", "lower" => lower, "upper" => upper)

"""
    semiinteger(lower, upper)

The set `{0} ∪ [lower, lower+1, ..., upper-1, upper]`.

### Examples

    {
        "head": "Semiinteger",
        "lower": 1,
        "upper": 2
    }
"""
semiinteger(lower, upper) = Object("head" => "Semiinteger", "lower" => lower, "upper" => upper)

"""
    sos1(weights)

The set corresponding to the special ordered set (SOS) constraint of type 1. Of
the variables in the set, at most one can be nonzero. The weights induce an
ordering of the variables; as such, they should be unique values. The kth
element in the set corresponds to the kth weight in weights

### Examples

    {
        "head": "SOSI",
        "lower": [1.0, 2.0, 3.0]
    }
"""
sos1(weights::Vector) = Object("head" => "SOSI", "weights" => weights)

"""
    sos2(weights)

The set corresponding to the special ordered set (SOS) constraint of type 2. Of
the variables in the set, at most two can be nonzero, and if two are nonzero,
they must be adjacent in the ordering of the set. The weights induce an ordering
of the variables; as such, they should be unique values. The kth element in the
set corresponds to the kth weight in weights

### Examples

    {
        "head": "SOSII",
        "lower": [1.0, 2.0, 3.0]
    }
"""
sos2(weights::Vector) = Object("head" => "SOSII", "weights" => weights)

"""
    secondordercone(dim)

The second-order cone (or Lorenz cone) ``\\{ (t,x) \\in \\mathbb{R}^{dim} : t \\ge || x ||_2 \\}`` of dimension `dim`.
"""
secondordercone(dim) = Object("head" => "SecondOrderCone", "dimension" => dim)

"""
    rotatedsecondordercone(dim)

The rotated second-order cone ``\\{ (t,u,x) \\mathbb{R}^{dim} : 2tu \\ge || x ||_2^2, t,u \\ge 0 \\}`` of dimension `dim`.
"""
rotatedsecondordercone(dim) = Object("head" => "RotatedSecondOrderCone", "dimension" => dim)

"""
    exponentialcone()

The 3-dimensional exponential cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : y \\exp (x/y) \\le z, y > 0 \\}``.
"""
exponentialcone() = Object("head" => "ExponentialCone")

"""
    dualexponentialcone()
The 3-dimensional dual exponential cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : -u \\exp (v/u) \\le \\exp(1) w, u < 0 \\}``.
"""
dualexponentialcone() = Object("head" => "DualExponentialCone")

"""
    powercone{T <: Real}(a::T)
The 3-dimensional power cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : x^{a} y^{1-a} >= |z|, x \\ge 0, y \\ge 0 \\}`` with parameter `a`.
"""
powercone(a) = Object("head" => "PowerCone", "a" => a)

"""
    dualpowercone{T <: Real}(a::T)
The 3-dimensional power cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : (\\frac{u}{a})^a (\\frac{v}/{1-a})^{1-a} >= |w|, u \\ge 0, v \\ge 0 \\}`` with parameter `a`.
"""
dualpowercone(a) = Object("head" => "DualPowerCone", "a" => a)

"""
    positivesemidefiniteconetriangle(dim)

The (vectorized) cone of symmetric positive semidefinite matrices, with
off-diagonals unscaled. The entries of the upper triangular part of the matrix
are given row by row (or equivalently, the entries of the lower triangular part
are given column by column).

An ``n \\times n`` matrix has ``n(n+1)/2`` lower-triangular elements, so for the
vectorized cone of dimension `dim`, the corresponding symmetric matrix has side
dimension ``\\sqrt (1/4 + 2 dim) - 1/2`` elements.

The scalar product is the sum of the pairwise product of the diagonal entries
plus twice the sum of the pairwise product of the upper diagonal entries.

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
positivesemidefiniteconetriangle(dim) = Object("head" => "PositiveSemidefiniteConeTriangle", "dimension" => dim)

"""
    positivesemidefiniteconescaled(dim)

The (vectorized) cone of symmetric positive semidefinite matrices, with
off-diagonals scaled. The entries of the upper triangular part of the matrix are
given row by row (or equivalently, the entries of the lower triangular part are
given column by column).

An ``n \\times n`` matrix has ``n(n+1)/2`` lower-triangular elements, so for the
vectorized cone of dimension `dim`, the corresponding symmetric matrix has side
dimension ``\\sqrt (1/4 + 2 dim) - 1/2`` elements. The off-diagonal entries of
the matrices of both the cone and its dual are scaled by ``\\sqrt{2}`` and the
scalar product is simply the sum of the pairwise product of the entries.

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
positivesemidefiniteconescaled(dim) = Object("head" => "PositiveSemidefiniteConeScaled", "dimension" => dim)
