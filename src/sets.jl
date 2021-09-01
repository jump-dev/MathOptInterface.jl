# Sets

# Note: When adding a new set, also add it to Utilities.Model.

"""
    AbstractSet

Abstract supertype for set objects used to encode constraints. A set object
should not contain any [`VariableIndex`](@ref) or [`ConstraintIndex`](@ref)
as the set is passed unmodifed during [`copy_to`](@ref).
"""
abstract type AbstractSet end

"""
    dimension(s::AbstractSet)

Return the [`output_dimension`](@ref) that an [`AbstractFunction`](@ref) should have to be used with the set `s`.

### Examples

```julia-repl
julia> dimension(Reals(4))
4

julia> dimension(LessThan(3.0))
1

julia> dimension(PositiveSemidefiniteConeTriangle(2))
3
```

"""
function dimension end

"""
    dual_set(s::AbstractSet)

Return the dual set of `s`, that is the dual cone of the set. This follows the
definition of duality discussed in [Duality](@ref).

See [Dual cone](https://en.wikipedia.org/wiki/Dual_cone_and_polar_cone) for more
information.

If the dual cone is not defined it returns an error.

### Examples

```jldocstest
julia> dual_set(Reals(4))
Zeros(4)

julia> dual_set(SecondOrderCone(5))
SecondOrderCone(5)

julia> dual_set(ExponentialCone())
DualExponentialCone()
```
"""
function dual_set end

dual_set(s::AbstractSet) = error("Dual of $s is not implemented.")

"""
    dual_set_type(S::Type{<:AbstractSet})

Return the type of dual set of sets of type `S`, as returned by
[`dual_set`](@ref). If the dual cone is not defined it returns an error.

### Examples

```jldocstest
julia> dual_set_type(Reals)
Zeros

julia> dual_set_type(SecondOrderCone)
SecondOrderCone

julia> dual_set_type(ExponentialCone)
DualExponentialCone
```
"""
function dual_set_type end

function dual_set_type(S::Type{<:AbstractSet})
    return error("Dual type of $S is not implemented.")
end

"""
    AbstractScalarSet

Abstract supertype for subsets of ``\\mathbb{R}``.
"""
abstract type AbstractScalarSet <: AbstractSet end

Base.broadcastable(set::AbstractScalarSet) = Ref(set)

dimension(s::AbstractScalarSet) = 1

"""
    AbstractVectorSet

Abstract supertype for subsets of ``\\mathbb{R}^n`` for some ``n``.
"""
abstract type AbstractVectorSet <: AbstractSet end

dimension(s::AbstractVectorSet) = s.dimension # .dimension field is conventional, overwrite this method if not applicable

"""
    Reals(dimension)

The set ``\\mathbb{R}^{dimension}`` (containing all points) of dimension
`dimension`.
"""
struct Reals <: AbstractVectorSet
    dimension::Int
    function Reals(dimension::Base.Integer)
        if !(dimension >= 0)
            throw(
                DimensionMismatch(
                    "Dimension of Reals must be >= 0, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::Reals) = Zeros(dimension(s))
dual_set_type(::Type{Reals}) = Zeros

"""
    Zeros(dimension)

The set ``\\{ 0 \\}^{dimension}`` (containing only the origin) of dimension
`dimension`.
"""
struct Zeros <: AbstractVectorSet
    dimension::Int
    function Zeros(dimension::Base.Integer)
        if !(dimension >= 0)
            throw(
                DimensionMismatch(
                    "Dimension of Zeros must be >= 0, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::Zeros) = Reals(dimension(s))
dual_set_type(::Type{Zeros}) = Reals

"""
    Nonnegatives(dimension)

The nonnegative orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\ge 0 \\}`` of
dimension `dimension`.
"""
struct Nonnegatives <: AbstractVectorSet
    dimension::Int
    function Nonnegatives(dimension::Base.Integer)
        if !(dimension >= 0)
            throw(
                DimensionMismatch(
                    "Dimension of Nonnegatives must be >= 0, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::Nonnegatives) = copy(s)
dual_set_type(::Type{Nonnegatives}) = Nonnegatives

"""
    Nonpositives(dimension)

The nonpositive orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\le 0 \\}`` of
dimension `dimension`.
"""
struct Nonpositives <: AbstractVectorSet
    dimension::Int
    function Nonpositives(dimension::Base.Integer)
        if !(dimension >= 0)
            throw(
                DimensionMismatch(
                    "Dimension of Nonpositives must be >= 0, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::Nonpositives) = copy(s)
dual_set_type(::Type{Nonpositives}) = Nonpositives

"""
    GreaterThan{T <: Real}(lower::T)

The set ``[lower,\\infty) \\subseteq \\mathbb{R}``.
"""
struct GreaterThan{T<:Real} <: AbstractScalarSet
    lower::T
end

"""
    LessThan{T <: Real}(upper::T)

The set ``(-\\infty,upper] \\subseteq \\mathbb{R}``.
"""
struct LessThan{T<:Real} <: AbstractScalarSet
    upper::T
end

"""
    EqualTo{T <: Number}(value::T)

The set containing the single point ``x \\in \\mathbb{R}`` where ``x`` is given by `value`.
"""
struct EqualTo{T<:Number} <: AbstractScalarSet
    value::T
end

function Base.:(==)(
    set1::S,
    set2::S,
) where {S<:Union{GreaterThan,LessThan,EqualTo}}
    return constant(set1) == constant(set2)
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
struct Interval{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end
Interval(s::GreaterThan{<:AbstractFloat}) = Interval(s.lower, typemax(s.lower))
Interval(s::LessThan{<:AbstractFloat}) = Interval(typemin(s.upper), s.upper)
Interval(s::EqualTo{<:Real}) = Interval(s.value, s.value)
Interval(s::Interval) = s

"""
    constant(s::Union{EqualTo, GreaterThan, LessThan})

Returns the constant of the set.
"""
constant(s::EqualTo) = s.value
constant(s::GreaterThan) = s.lower
constant(s::LessThan) = s.upper

"""
    NormInfinityCone(dimension)

The ``\\ell_\\infty``-norm cone ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_\\infty = \\max_i \\lvert x_i \\rvert \\}`` of dimension `dimension`.
"""
struct NormInfinityCone <: AbstractVectorSet
    dimension::Int
    function NormInfinityCone(dimension::Base.Integer)
        if !(dimension >= 1)
            throw(
                DimensionMismatch(
                    "Dimension of NormInfinityCone must be >= 1, not " *
                    "$(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::NormInfinityCone) = NormOneCone(dimension(s))
dual_set_type(::Type{NormInfinityCone}) = NormOneCone

"""
    NormOneCone(dimension)

The ``\\ell_1``-norm cone ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_1 = \\sum_i \\lvert x_i \\rvert \\}`` of dimension `dimension`.
"""
struct NormOneCone <: AbstractVectorSet
    dimension::Int
    function NormOneCone(dimension::Base.Integer)
        if !(dimension >= 1)
            throw(
                DimensionMismatch(
                    "Dimension of NormOneCone must be >= 1, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::NormOneCone) = NormInfinityCone(dimension(s))
dual_set_type(::Type{NormOneCone}) = NormInfinityCone

"""
    SecondOrderCone(dimension)

The second-order cone (or Lorenz cone or ``\\ell_2``-norm cone) ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_2 \\}`` of dimension `dimension`.
"""
struct SecondOrderCone <: AbstractVectorSet
    dimension::Int
    function SecondOrderCone(dimension::Base.Integer)
        if !(dimension >= 1)
            throw(
                DimensionMismatch(
                    "Dimension of SecondOrderCone must be >= 1, not " *
                    "$(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::SecondOrderCone) = copy(s)
dual_set_type(::Type{SecondOrderCone}) = SecondOrderCone

"""
    RotatedSecondOrderCone(dimension)

The rotated second-order cone ``\\{ (t,u,x) \\in \\mathbb{R}^{dimension} : 2tu \\ge \\lVert x \\rVert_2^2, t,u \\ge 0 \\}`` of dimension `dimension`.
"""
struct RotatedSecondOrderCone <: AbstractVectorSet
    dimension::Int
    function RotatedSecondOrderCone(dimension::Base.Integer)
        if !(dimension >= 2)
            throw(
                DimensionMismatch(
                    "Dimension of RotatedSecondOrderCone must be >= 2, not " *
                    "$(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

dual_set(s::RotatedSecondOrderCone) = copy(s)
dual_set_type(::Type{RotatedSecondOrderCone}) = RotatedSecondOrderCone

"""
    GeometricMeanCone(dimension)

The geometric mean cone
``\\{ (t,x) \\in \\mathbb{R}^{n+1} : x \\ge 0, t \\le \\sqrt[n]{x_1 x_2 \\cdots x_n} \\}``,
where `dimension = n + 1 >= 2`.

## Duality note

The dual of the geometric mean cone is
``\\{ (u, v) \\in \\mathbb{R}^{n+1} : u \\le 0, v \\ge 0, -u \\le n \\sqrt[n]{\\prod_i v_i} \\}``,
where `dimension = n + 1 >= 2`.
"""
struct GeometricMeanCone <: AbstractVectorSet
    dimension::Int
    function GeometricMeanCone(dimension::Base.Integer)
        if !(dimension >= 2)
            throw(
                DimensionMismatch(
                    "Dimension of GeometricMeanCone must be >= 2, not " *
                    "$(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

"""
    ExponentialCone()

The 3-dimensional exponential cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : y \\exp (x/y) \\le z, y > 0 \\}``.
"""
struct ExponentialCone <: AbstractVectorSet end

dual_set(s::ExponentialCone) = DualExponentialCone()
dual_set_type(::Type{ExponentialCone}) = DualExponentialCone

"""
    DualExponentialCone()

The 3-dimensional dual exponential cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : -u \\exp (v/u) \\le \\exp(1) w, u < 0 \\}``.
"""
struct DualExponentialCone <: AbstractVectorSet end

dual_set(s::DualExponentialCone) = ExponentialCone()
dual_set_type(::Type{DualExponentialCone}) = ExponentialCone

"""
    PowerCone{T <: Real}(exponent::T)

The 3-dimensional power cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : x^{exponent} y^{1-exponent} \\ge |z|, x \\ge 0, y \\ge 0 \\}`` with parameter `exponent`.
"""
struct PowerCone{T<:Real} <: AbstractVectorSet
    exponent::T
end

dual_set(s::PowerCone{T}) where {T<:Real} = DualPowerCone{T}(s.exponent)
dual_set_type(::Type{PowerCone{T}}) where {T<:Real} = DualPowerCone{T}

"""
    DualPowerCone{T <: Real}(exponent::T)

The 3-dimensional power cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : (\\frac{u}{exponent})^{exponent} (\\frac{v}{1-exponent})^{1-exponent} \\ge |w|, u \\ge 0, v \\ge 0 \\}`` with parameter `exponent`.
"""
struct DualPowerCone{T<:Real} <: AbstractVectorSet
    exponent::T
end

dual_set(s::DualPowerCone{T}) where {T<:Real} = PowerCone{T}(s.exponent)
dual_set_type(::Type{DualPowerCone{T}}) where {T<:Real} = PowerCone{T}

function dimension(
    s::Union{ExponentialCone,DualExponentialCone,PowerCone,DualPowerCone},
)
    return 3
end

function Base.:(==)(set1::S, set2::S) where {S<:Union{PowerCone,DualPowerCone}}
    return set1.exponent == set2.exponent
end

"""
    RelativeEntropyCone(dimension)

The relative entropy cone
``\\{ (u, v, w) \\in \\mathbb{R}^{1+2n} : u \\ge \\sum_{i=1}^n w_i \\log(\\frac{w_i}{v_i}), v_i \\ge 0, w_i \\ge 0 \\}``,
where `dimension = 2n + 1 >= 1`.

### Duality note

The dual of the relative entropy cone is
``\\{ (u, v, w) \\in \\mathbb{R}^{1+2n} : \\forall i, w_i \\ge u (\\log (\\frac{u}{v_i}) - 1), v_i \\ge 0, u > 0 \\}`` of dimension `dimension```{}=2n+1``.
"""
struct RelativeEntropyCone <: AbstractVectorSet
    dimension::Int
    function RelativeEntropyCone(dimension::Base.Integer)
        if !(dimension >= 1 && isodd(dimension))
            throw(
                DimensionMismatch(
                    "Dimension of RelativeEntropyCone must be an odd integer " *
                    ">= 1, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

"""
    NormSpectralCone(row_dim, column_dim)

The epigraph of the matrix spectral norm (maximum singular value function)
``\\{ (t, X) \\in \\mathbb{R}^{1 + row_dim \\times column_dim} : t \\ge \\sigma_1(X) \\}``,
where ``\\sigma_i`` is the ``i``th singular value of the matrix ``X`` of row
dimension `row_dim` and column dimension `column_dim`.

The matrix X is vectorized by stacking the columns, matching the behavior of
Julia's `vec` function.
"""
struct NormSpectralCone <: AbstractVectorSet
    row_dim::Int
    column_dim::Int
    function NormSpectralCone(row_dim::Base.Integer, column_dim::Base.Integer)
        if !(row_dim >= 0 && column_dim >= 0)
            throw(
                DimensionMismatch(
                    "Dimensions of NormSpectralCone must be >= 0, not " *
                    "($(row_dim), $(column_dim)).",
                ),
            )
        end
        return new(row_dim, column_dim)
    end
end

dual_set(s::NormSpectralCone) = NormNuclearCone(s.row_dim, s.column_dim)
dual_set_type(::Type{NormSpectralCone}) = NormNuclearCone

"""
    NormNuclearCone(row_dim, column_dim)

The epigraph of the matrix nuclear norm (sum of singular values function)
``\\{ (t, X) \\in \\mathbb{R}^{1 + row_dim \\times column_dim} : t \\ge \\sum_i \\sigma_i(X) \\}``,
where ``\\sigma_i`` is the ``i``th singular value of the matrix ``X`` of row
dimension `row_dim` and column dimension `column_dim`.

The matrix X is vectorized by stacking the columns, matching the behavior of
Julia's `vec` function.
"""
struct NormNuclearCone <: AbstractVectorSet
    row_dim::Int
    column_dim::Int
    function NormNuclearCone(row_dim::Base.Integer, column_dim::Base.Integer)
        if !(row_dim >= 0 && column_dim >= 0)
            throw(
                DimensionMismatch(
                    "Dimension of NormNuclearCone must be >= 0, not " *
                    "($(row_dim), $(column_dim)).",
                ),
            )
        end
        return new(row_dim, column_dim)
    end
end

dual_set(s::NormNuclearCone) = NormSpectralCone(s.row_dim, s.column_dim)
dual_set_type(::Type{NormNuclearCone}) = NormSpectralCone

function dimension(s::Union{NormSpectralCone,NormNuclearCone})
    return 1 + s.row_dim * s.column_dim
end

"""
    abstract type AbstractSymmetricMatrixSetTriangle <: AbstractVectorSet end

Abstract supertype for subsets of the (vectorized) cone of symmetric matrices,
with [`side_dimension`](@ref) rows and columns. The entries of the upper-right
triangular part of the matrix are given column by column (or equivalently, the
entries of the lower-left triangular part are given row by row). A vectorized
cone of [`dimension`](@ref) ``n`` corresponds to a square matrix with side
dimension ``\\sqrt{1/4 + 2 n} - 1/2``. (Because a ``d \\times d`` matrix has
``d(d + 1) / 2`` elements in the upper or lower triangle.)

### Examples

The matrix
```math
\\begin{bmatrix}
  1 & 2 & 4\\\\
  2 & 3 & 5\\\\
  4 & 5 & 6
\\end{bmatrix}
```
has [`side_dimension`](@ref) 3 and vectorization ``(1, 2, 3, 4, 5, 6)``.

### Note

Two packed storage formats exist for symmetric matrices, the respective orders
of the entries are:
- upper triangular column by column (or lower triangular row by row);
- lower triangular column by column (or upper triangular row by row).

The advantage of the first format is the mapping between the `(i, j)` matrix
indices and the `k` index of the vectorized form. It is simpler and does not
depend on the side dimension of the matrix.
Indeed,
- the entry of matrix indices `(i, j)` has vectorized index
  `k = div((j - 1) * j, 2) + i` if ``i \\leq j`` and
  `k = div((i - 1) * i, 2) + j` if ``j \\leq i``;
- and the entry with vectorized index `k` has matrix indices
  `i = div(1 + isqrt(8k - 7), 2)` and `j = k - div((i - 1) * i, 2)` or
  `j = div(1 + isqrt(8k - 7), 2)` and `i = k - div((j - 1) * j, 2)`.

### Duality note

The scalar product for the symmetric matrix in its vectorized form is the sum of
the pairwise product of the diagonal entries plus twice the sum of the pairwise
product of the upper diagonal entries; see [p. 634, 1].
This has important consequence for duality.

Consider for example the following problem
([`PositiveSemidefiniteConeTriangle`](@ref) is a subtype of
[`AbstractSymmetricMatrixSetTriangle`](@ref))
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
The reason is that ``2y_2`` is the scalar product between ``y`` and the symmetric
matrix whose vectorized form is ``(0, 1, 0)``. Indeed, with our modified scalar
products we have
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
abstract type AbstractSymmetricMatrixSetTriangle <: AbstractVectorSet end

function dimension(set::AbstractSymmetricMatrixSetTriangle)
    d = side_dimension(set)
    return div(d * (d + 1), 2)
end

"""
    abstract type AbstractSymmetricMatrixSetSquare <: AbstractVectorSet end

Abstract supertype for subsets of the (vectorized) cone of symmetric matrices,
with [`side_dimension`](@ref) rows and columns. The entries of the matrix are
given column by column (or equivalently, row by row). The matrix is both
constrained to be symmetric and to have its [`triangular_form`](@ref) belong
to the corresponding set. That is, if the functions in entries ``(i, j)`` and
``(j, i)`` are different, then a constraint will be added to make sure that the
entries are equal.

### Examples

[`PositiveSemidefiniteConeSquare`](@ref) is a subtype of
[`AbstractSymmetricMatrixSetSquare`](@ref) and constraining the matrix
```math
\\begin{bmatrix}
  1 & -y\\\\
  -z & 0\\\\
\\end{bmatrix}
```
to be symmetric positive semidefinite can be achieved by constraining the vector
``(1, -z, -y, 0)`` (or ``(1, -y, -z, 0)``) to belong to the
`PositiveSemidefiniteConeSquare(2)`. It both constrains ``y = z`` and
``(1, -y, 0)`` (or ``(1, -z, 0)``) to be in
`PositiveSemidefiniteConeTriangle(2)`, since
`triangular_form(PositiveSemidefiniteConeSquare)` is
`PositiveSemidefiniteConeTriangle`.
"""
abstract type AbstractSymmetricMatrixSetSquare <: AbstractVectorSet end

dimension(set::AbstractSymmetricMatrixSetSquare) = side_dimension(set)^2

"""
    side_dimension(set::Union{AbstractSymmetricMatrixSetTriangle,
                              AbstractSymmetricMatrixSetSquare})

Side dimension of the matrices in `set`. By convention, it should be stored in
the `side_dimension` field but if it is not the case for a subtype of
[`AbstractSymmetricMatrixSetTriangle`](@ref), the method should be implemented
for this subtype.
"""
function side_dimension(
    set::Union{
        AbstractSymmetricMatrixSetTriangle,
        AbstractSymmetricMatrixSetSquare,
    },
)
    return set.side_dimension
end

"""
    triangular_form(S::Type{<:AbstractSymmetricMatrixSetSquare})
    triangular_form(set::AbstractSymmetricMatrixSetSquare)

Return the [`AbstractSymmetricMatrixSetTriangle`](@ref) corresponding to the
vectorization of the upper triangular part of matrices in the
[`AbstractSymmetricMatrixSetSquare`](@ref) set.
"""
function triangular_form end
function triangular_form(set::AbstractSymmetricMatrixSetSquare)
    return triangular_form(typeof(set))(side_dimension(set))
end

"""
    PositiveSemidefiniteConeTriangle(side_dimension) <: AbstractSymmetricMatrixSetTriangle

The (vectorized) cone of symmetric positive semidefinite matrices, with
`side_dimension` rows and columns.

See [`AbstractSymmetricMatrixSetTriangle`](@ref) for more details on the
vectorized form.
"""
struct PositiveSemidefiniteConeTriangle <: AbstractSymmetricMatrixSetTriangle
    side_dimension::Int
    function PositiveSemidefiniteConeTriangle(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of PositiveSemidefiniteConeTriangle must " *
                    "be >= 0, not $(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

dual_set(s::PositiveSemidefiniteConeTriangle) = copy(s)
function dual_set_type(::Type{PositiveSemidefiniteConeTriangle})
    return PositiveSemidefiniteConeTriangle
end

"""
    PositiveSemidefiniteConeSquare(side_dimension) <: AbstractSymmetricMatrixSetSquare

The cone of symmetric positive semidefinite matrices, with side length
`side_dimension`.

See [`AbstractSymmetricMatrixSetSquare`](@ref) for more details on the
vectorized form.

The entries of the matrix are given column by column (or equivalently, row by
row).

The matrix is both constrained to be symmetric and to be positive semidefinite.
That is, if the functions in entries ``(i, j)`` and ``(j, i)`` are different,
then a constraint will be added to make sure that the entries are equal.

### Examples

Constraining the matrix
```math
\\begin{bmatrix}
  1 & -y\\\\
  -z & 0\\\\
\\end{bmatrix}
```
to be symmetric positive semidefinite can be achieved by constraining the vector
``(1, -z, -y, 0)`` (or ``(1, -y, -z, 0)``) to belong to the
`PositiveSemidefiniteConeSquare(2)`.

It both constrains ``y = z`` and ``(1, -y, 0)`` (or ``(1, -z, 0)``) to be in
`PositiveSemidefiniteConeTriangle(2)`.
"""
struct PositiveSemidefiniteConeSquare <: AbstractSymmetricMatrixSetSquare
    side_dimension::Int
    function PositiveSemidefiniteConeSquare(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of PositiveSemidefiniteConeSquare must " *
                    "be >= 0, not $(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

function _dual_set_square_error()
    return error(
        """Dual of `PositiveSemidefiniteConeSquare` is not defined in MathOptInterface.
           For more details see the comments in `src/Bridges/Constraint/square.jl`.""",
    )
end
function dual_set(::PositiveSemidefiniteConeSquare)
    return _dual_set_square_error()
end
function dual_set_type(::Type{PositiveSemidefiniteConeSquare})
    return _dual_set_square_error()
end

function triangular_form(::Type{PositiveSemidefiniteConeSquare})
    return PositiveSemidefiniteConeTriangle
end

"""
    LogDetConeTriangle(side_dimension)

The log-determinant cone
``\\{ (t, u, X) \\in \\mathbb{R}^{2 + d(d+1)/2} : t \\le u \\log(\\det(X/u)), u > 0 \\}``,
where the matrix `X` is represented in the same symmetric packed format as in
the `PositiveSemidefiniteConeTriangle`.

The argument `side_dimension` is the side dimension of the matrix `X`, i.e., its
number of rows or columns.
"""
struct LogDetConeTriangle <: AbstractVectorSet
    side_dimension::Int
    function LogDetConeTriangle(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of LogDetConeTriangle must be >= 0, not " *
                    "$(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

function dimension(s::LogDetConeTriangle)
    return 2 + div(s.side_dimension * (s.side_dimension + 1), 2)
end

"""
    LogDetConeSquare(side_dimension)

The log-determinant cone
``\\{ (t, u, X) \\in \\mathbb{R}^{2 + d^2} : t \\le u \\log(\\det(X/u)), X \\text{ symmetric}, u > 0 \\}``,
where the matrix `X` is represented in the same format as in the
[`PositiveSemidefiniteConeSquare`](@ref).

Similarly to [`PositiveSemidefiniteConeSquare`](@ref), constraints are added to
ensure that `X` is symmetric.

The argument `side_dimension` is the side dimension of the matrix `X`, i.e., its
number of rows or columns.
"""
struct LogDetConeSquare <: AbstractVectorSet
    side_dimension::Int
    function LogDetConeSquare(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of LogDetConeSquare must be >= 0, not " *
                    "$(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

dimension(s::LogDetConeSquare) = 2 + s.side_dimension^2

"""
    RootDetConeTriangle(side_dimension)

The root-determinant cone
``\\{ (t, X) \\in \\mathbb{R}^{1 + d(d+1)/2} : t \\le \\det(X)^{1/d} \\}``,
where the matrix `X` is represented in the same symmetric packed format as in
the [`PositiveSemidefiniteConeTriangle`](@ref).

The argument `side_dimension` is the side dimension of the matrix `X`, i.e., its
number of rows or columns.
"""
struct RootDetConeTriangle <: AbstractVectorSet
    side_dimension::Int
    function RootDetConeTriangle(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of RootDetConeTriangle must be >= 0, not " *
                    "$(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

function dimension(s::RootDetConeTriangle)
    return 1 + div(s.side_dimension * (s.side_dimension + 1), 2)
end

"""
    RootDetConeSquare(side_dimension)

The root-determinant cone
``\\{ (t, X) \\in \\mathbb{R}^{1 + d^2} : t \\le \\det(X)^{1/d}, X \\text{ symmetric} \\}``,
where the matrix `X` is represented in the same format as
[`PositiveSemidefiniteConeSquare`](@ref).

Similarly to [`PositiveSemidefiniteConeSquare`](@ref), constraints are added to
ensure that `X` is symmetric.

The argument `side_dimension` is the side dimension of the matrix `X`, i.e., its
number of rows or columns.
"""
struct RootDetConeSquare <: AbstractVectorSet
    side_dimension::Int
    function RootDetConeSquare(side_dimension::Base.Integer)
        if !(side_dimension >= 0)
            throw(
                DimensionMismatch(
                    "Side dimension of RootDetConeSquare must be >= 0, not " *
                    "$(side_dimension).",
                ),
            )
        end
        return new(side_dimension)
    end
end

dimension(s::RootDetConeSquare) = 1 + s.side_dimension^2

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
struct Semicontinuous{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end

"""
    Semiinteger{T <: Real}(lower::T,upper::T)

The set ``\\{0\\} \\cup \\{lower,lower+1,\\ldots,upper-1,upper\\}``.
"""
struct Semiinteger{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end

function Base.:(==)(
    set1::S,
    set2::S,
) where {S<:Union{Interval,Semicontinuous,Semiinteger}}
    return set1.lower == set2.lower && set1.upper == set2.upper
end

"""
    SOS1{T <: Real}(weights::Vector{T})

The set corresponding to the special ordered set (SOS) constraint of type 1.
Of the variables in the set, at most one can be nonzero.
The `weights` induce an ordering of the variables; as such, they should be unique values.
The *k*th element in the set corresponds to the *k*th weight in `weights`.
See [here](http://lpsolve.sourceforge.net/5.5/SOS.htm) for a description of SOS constraints and their potential uses.
"""
struct SOS1{T<:Real} <: AbstractVectorSet
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
struct SOS2{T<:Real} <: AbstractVectorSet
    weights::Vector{T}
end

Base.:(==)(a::T, b::T) where {T<:Union{SOS1,SOS2}} = a.weights == b.weights

dimension(s::Union{SOS1,SOS2}) = length(s.weights)

"""
	ActivationCondition

Activation condition for an indicator constraint.

The enum value is used as first type parameter of `Indicator{A,S}`.
"""
@enum ActivationCondition begin
    ACTIVATE_ON_ZERO
    ACTIVATE_ON_ONE
end

"""
    Indicator{A<:ActivationCondition,S<:AbstractScalarSet}(set::S)

The set corresponding to an indicator constraint.

When `A` is `ACTIVATE_ON_ZERO`, this means:
``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R}^n : y = 0 \\implies x \\in set\\}``

When `A` is `ACTIVATE_ON_ONE`, this means:
``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R}^n : y = 1 \\implies x \\in set\\}``

## Notes

Most solvers expect that the first row of the function is interpretable as a
variable index `x_i` (e.g., `1.0 * x + 0.0`). An error will be thrown if this is
not the case.

## Example

The constraint
``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R}^2 : y = 1 \\implies x_1 + x_2 \\leq 9 \\}``
is defined as
```julia
f = MOI.VectorAffineFunction(
    [
        MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x1)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
    ],
    [0.0, 0.0],
)
s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
MOI.add_constraint(model, f, s)
```
"""
struct Indicator{A,S<:AbstractScalarSet} <: AbstractVectorSet
    set::S
    Indicator{A}(set::S) where {A,S<:AbstractScalarSet} = new{A,S}(set)
end

dimension(::Indicator) = 2

function Base.copy(set::Indicator{A,S}) where {A,S}
    return Indicator{A}(copy(set.set))
end

function Base.:(==)(set1::Indicator{A,S}, set2::Indicator{A,S}) where {A,S}
    return set1.set == set2.set
end

"""
    Complements(dimension::Base.Integer)

The set corresponding to a mixed complementarity constraint.

Complementarity constraints should be specified with an
[`AbstractVectorFunction`](@ref)-in-`Complements(dimension)` constraint.

The dimension of the vector-valued function `F` must be `dimension`. This
defines a complementarity constraint between the scalar function `F[i]` and the
variable in `F[i + dimension/2]`. Thus, `F[i + dimension/2]` must be
interpretable as a single variable `x_i` (e.g., `1.0 * x + 0.0`), and
`dimension` must be even.

The mixed complementarity problem consists of finding `x_i` in the interval
`[lb, ub]` (i.e., in the set `Interval(lb, ub)`), such that the following holds:

  1. `F_i(x) == 0` if `lb_i < x_i < ub_i`
  2. `F_i(x) >= 0` if `lb_i == x_i`
  3. `F_i(x) <= 0` if `x_i == ub_i`

Classically, the bounding set for `x_i` is `Interval(0, Inf)`, which recovers:
`0 <= F_i(x) ⟂ x_i >= 0`, where the `⟂` operator implies `F_i(x) * x_i = 0`.

### Examples

The problem:

    x -in- Interval(-1, 1)
    [-4 * x - 3, x] -in- Complements(2)

defines the mixed complementarity problem where the following holds:

  1. `-4 * x - 3 == 0` if `-1 < x < 1`
  2. `-4 * x - 3 >= 0` if `x == -1`
  3. `-4 * x - 3 <= 0` if `x == 1`

There are three solutions:

  1. `x = -3/4` with `F(x) = 0`
  2. `x = -1` with `F(x) = 1`
  3. `x = 1` with `F(x) = -7`

The function `F` can also be defined in terms of single variables. For example,
the problem:

    [x_3, x_4] -in- Nonnegatives(2)
    [x_1, x_2, x_3, x_4] -in- Complements(4)

defines the complementarity problem where `0 <= x_1 ⟂ x_3 >= 0` and
`0 <= x_2 ⟂ x_4 >= 0`.
"""
struct Complements <: AbstractVectorSet
    dimension::Int
    function Complements(dimension::Base.Integer)
        if !(dimension >= 0 && iseven(dimension))
            throw(
                DimensionMismatch(
                    "Dimension of Complements must be even, not $(dimension).",
                ),
            )
        end
        return new(dimension)
    end
end

# isbits types, nothing to copy
function Base.copy(
    set::Union{
        Reals,
        Zeros,
        Nonnegatives,
        Nonpositives,
        GreaterThan,
        LessThan,
        EqualTo,
        Interval,
        NormInfinityCone,
        NormOneCone,
        SecondOrderCone,
        RotatedSecondOrderCone,
        GeometricMeanCone,
        ExponentialCone,
        DualExponentialCone,
        PowerCone,
        DualPowerCone,
        RelativeEntropyCone,
        NormSpectralCone,
        NormNuclearCone,
        PositiveSemidefiniteConeTriangle,
        PositiveSemidefiniteConeSquare,
        LogDetConeTriangle,
        LogDetConeSquare,
        RootDetConeTriangle,
        RootDetConeSquare,
        Complements,
        Integer,
        ZeroOne,
        Semicontinuous,
        Semiinteger,
    },
)
    return set
end
Base.copy(set::S) where {S<:Union{SOS1,SOS2}} = S(copy(set.weights))

"""
    supports_dimension_update(S::Type{<:MOI.AbstractVectorSet})

Return a `Bool` indicating whether the elimination of any dimension of
`n`-dimensional sets of type `S` give an `n-1`-dimensional set `S`.
By default, this function returns `false` so it should only be implemented
for sets that supports dimension update.

For instance, `supports_dimension_update(MOI.Nonnegatives}` is `true` because
the elimination of any dimension of the `n`-dimensional nonnegative orthant
gives the `n-1`-dimensional nonnegative orthant. However
`supports_dimension_update(MOI.ExponentialCone}` is `false`.
"""
function supports_dimension_update(::Type{<:AbstractVectorSet})
    return false
end
function supports_dimension_update(
    ::Type{<:Union{Reals,Zeros,Nonnegatives,Nonpositives}},
)
    return true
end

"""
    update_dimension(s::AbstractVectorSet, new_dim)

Returns a set with the dimension modified to `new_dim`.
"""
function update_dimension end
function update_dimension(
    set::Union{Reals,Zeros,Nonnegatives,Nonpositives},
    new_dim,
)
    return typeof(set)(new_dim)
end
