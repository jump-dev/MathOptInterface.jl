# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    shift_constant(set::MOI.AbstractScalarSet, offset)

Returns a new scalar set `new_set` such that `func`-in-`set` is equivalent to
`func + offset`-in-`new_set`.

Only define this function if it makes sense to!

Use [`supports_shift_constant`](@ref) to check if the set supports shifting:
```Julia
if supports_shift_constant(typeof(old_set))
    new_set = shift_constant(old_set, offset)
    f.constant = 0
    add_constraint(model, f, new_set)
else
    add_constraint(model, f, old_set)
end
```

See also [`supports_shift_constant`](@ref).

## Examples

The call `shift_constant(MOI.Interval(-2, 3), 1)` is equal to
`MOI.Interval(-1, 4)`.
"""
function shift_constant end

"""
    supports_shift_constant(::Type{S}) where {S<:MOI.AbstractSet}

Return `true` if [`shift_constant`](@ref) is defined for set `S`.

See also [`shift_constant`](@ref).
"""
supports_shift_constant(::Type{S}) where {S<:MOI.AbstractSet} = false

function shift_constant(set::MOI.LessThan, offset)
    return MOI.LessThan(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.LessThan}) = true

function shift_constant(set::MOI.GreaterThan, offset)
    return MOI.GreaterThan(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.GreaterThan}) = true

function shift_constant(set::MOI.EqualTo, offset)
    return MOI.EqualTo(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.EqualTo}) = true

function shift_constant(set::MOI.Interval, offset)
    return MOI.Interval(set.lower + offset, set.upper + offset)
end
supports_shift_constant(::Type{<:MOI.Interval}) = true

function shift_constant(set::MOI.Parameter, offset)
    return MOI.Parameter(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.Parameter}) = true

"""
    ScalarLinearSet{T}

The union of scalar-valued linear sets with element type `T`.

This is used in the vectorize and scalarize bridges.

See also: [`VectorLinearSet`](@ref).
"""
const ScalarLinearSet{T} =
    Union{MOI.EqualTo{T},MOI.LessThan{T},MOI.GreaterThan{T}}

"""
    VectorLinearSet

The union of vector-valued linear cones.

This is used in the vectorize and scalarize bridges.

See also: [`ScalarLinearSet`](@ref).
"""
const VectorLinearSet = Union{MOI.Zeros,MOI.Nonnegatives,MOI.Nonpositives}

"""
    vector_set_type(::Type{S}) where {S}

A utility function to map scalar sets `S` to their vector equivalents.

This is used in the vectorize and scalarize bridges.

See also: [`scalar_set_type`](@ref).
"""
vector_set_type(::Type{<:MOI.EqualTo}) = MOI.Zeros
vector_set_type(::Type{<:MOI.LessThan}) = MOI.Nonpositives
vector_set_type(::Type{<:MOI.GreaterThan}) = MOI.Nonnegatives

"""
    scalar_set_type(::Type{S}, ::Type{T}) where {S,T}

A utility function to map vector sets `S` to their scalar equivalents with
element type `T`.

This is used in the vectorize and scalarize bridges.

See also: [`vector_set_type`](@ref).
"""
scalar_set_type(::Type{<:MOI.Zeros}, T::Type) = MOI.EqualTo{T}
scalar_set_type(::Type{<:MOI.Nonpositives}, T::Type) = MOI.LessThan{T}
scalar_set_type(::Type{<:MOI.Nonnegatives}, T::Type) = MOI.GreaterThan{T}

"""
    is_diagonal_vectorized_index(index::Base.Integer)

Return whether `index` is the index of a diagonal element in a
[`MOI.AbstractSymmetricMatrixSetTriangle`](@ref) set.
"""
function is_diagonal_vectorized_index(index::Base.Integer)
    # See https://en.wikipedia.org/wiki/Triangular_number#Triangular_roots_and_tests_for_triangular_numbers
    perfect_square = 1 + 8index
    return isqrt(perfect_square)^2 == perfect_square
end

"""
    side_dimension_for_vectorized_dimension(n::Integer)

Return the dimension `d` such that
`MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(d))` is `n`.
"""
function side_dimension_for_vectorized_dimension(n::Base.Integer)
    # We have `d*(d+1)/2 = n` so
    # `d² + d - 2n = 0` hence `d = (-1 ± √(1 + 8d)) / 2`
    # The integer `√(1 + 8d)` is odd and `√(1 + 8d) - 1` is even.
    # We can drop the `- 1` as `div` already discards it.
    return div(isqrt(1 + 8n), 2)
end

"""
    trimap(row::Integer, column::Integer)

Convert between the row and column indices of a matrix, to the linear index of
the corresponding element in the triangular representation.

This is most useful when mapping between `ConeSquare` and `ConeTriangle` sets,
e.g., as part of an [`MOI.AbstractSymmetricMatrixSetTriangle`](@ref) set.
"""
function trimap(row::Integer, column::Integer)
    if row < column
        return trimap(column, row)
    end
    return div((row - 1) * row, 2) + column
end

struct ZeroVector{T} <: AbstractVector{T}
    n::Int
end

Base.eltype(::Type{ZeroVector{T}}) where {T} = T

Base.length(v::ZeroVector) = v.n

Base.size(v::ZeroVector) = (v.n,)

function Base.getindex(v::ZeroVector{T}, i::Integer) where {T}
    return zero(T)
end

struct CanonicalVector{T} <: AbstractVector{T}
    index::Int
    n::Int
end

Base.eltype(::Type{CanonicalVector{T}}) where {T} = T

Base.length(v::CanonicalVector) = v.n

Base.size(v::CanonicalVector) = (v.n,)

function Base.getindex(v::CanonicalVector{T}, i::Integer) where {T}
    return convert(T, i == v.index)
end

function Base.view(v::CanonicalVector{T}, I::AbstractUnitRange) where {T}
    if v.index in I
        return CanonicalVector{T}(v.index - first(I) + 1, length(I))
    else
        return ZeroVector{T}(length(I))
    end
end

# This is much faster than the default implementation that goes
# through all entries even if only one is nonzero.
function LinearAlgebra.dot(
    x::CanonicalVector{T},
    y::CanonicalVector{T},
) where {T}
    return convert(T, x.index == y.index)
end

function triangle_dot(
    x::CanonicalVector{T},
    y::CanonicalVector{T},
    ::Int,
    offset::Int,
) where {T}
    if x.index != y.index || x.index <= offset
        return zero(T)
    elseif is_diagonal_vectorized_index(x.index - offset)
        return one(T)
    else
        return 2one(T)
    end
end

function _set_dot(i::Integer, s::MOI.AbstractVectorSet, T::Type)
    vec = CanonicalVector{T}(i, MOI.dimension(s))
    return set_dot(vec, vec, s)
end

function _set_dot(::Integer, ::MOI.AbstractScalarSet, T::Type)
    return one(T)
end

"""
    struct SetDotScalingVector{T,S<:MOI.AbstractSet} <: AbstractVector{T}
        set::S
        len::Int
    end

Vector `s` of scaling for the entries of the vectorized form of
a vector `x` in `set` and `y` in `MOI.dual_set(set)` such that
`MOI.Utilities.set_dot(x, y) = LinearAlgebra.dot(s .* x, s .* y)`.

```
julia> import MathOptInterface as MOI

julia> model = MOI.Utilities.Model{Float64}()
MOIU.Model{Float64}

julia> x = MOI.add_variables(model, 3);

julia> func = MOI.VectorOfVariables(x)
┌                    ┐
│MOI.VariableIndex(1)│
│MOI.VariableIndex(2)│
│MOI.VariableIndex(3)│
└                    ┘

julia> set = MOI.PositiveSemidefiniteConeTriangle(2)
MathOptInterface.PositiveSemidefiniteConeTriangle(2)

julia> MOI.add_constraint(model, func, MOI.Scaled(set))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Scaled{MathOptInterface.PositiveSemidefiniteConeTriangle}}(1)

julia> a = MOI.Utilities.SetDotScalingVector{Float64}(set)
3-element MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}:
 ⋮

julia> a = MOI.Utilities.SetDotScalingVector{Float64}(set)
3-element MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}:
 1.0
 1.4142135623730951
 1.0

julia> using LinearAlgebra

julia> Diagonal(a) * func
ERROR: MethodError: no method matching length(::MathOptInterface.VectorOfVariables)

Closest candidates are:
  length(::Union{Base.KeySet, Base.ValueIterator})
   @ Base abstractdict.jl:58
  length(::Union{Adjoint{T, S}, Transpose{T, S}} where {T, S})
   @ LinearAlgebra ~/.julia/juliaup/julia-1.9.2+0.x64.linux.gnu/share/julia/stdlib/v1.9/LinearAlgebra/src/adjtrans.jl:295
  length(::Union{SparseArrays.FixedSparseVector{Tv, Ti}, SparseArrays.SparseVector{Tv, Ti}} where {Tv, Ti})
   @ SparseArrays ~/.julia/juliaup/julia-1.9.2+0.x64.linux.gnu/share/julia/stdlib/v1.9/SparseArrays/src/sparsevector.jl:95
  ...

Stacktrace:
 [1] _similar_shape(itr::MathOptInterface.VectorOfVariables, #unused#::Base.HasLength)
   @ Base ./array.jl:658
 [2] _collect(cont::UnitRange{Int64}, itr::MathOptInterface.VectorOfVariables, #unused#::Base.HasEltype, isz::Base.HasLength)
   @ Base ./array.jl:713
 [3] collect(itr::MathOptInterface.VectorOfVariables)
   @ Base ./array.jl:707
 [4] broadcastable(x::MathOptInterface.VectorOfVariables)
   @ Base.Broadcast ./broadcast.jl:717
 [5] broadcasted
   @ ./broadcast.jl:1315 [inlined]
 [6] *(A::Diagonal{Float64, MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}}, α::MathOptInterface.VectorOfVariables)
   @ MutableArithmetics ~/.julia/packages/MutableArithmetics/h0wjj/src/dispatch.jl:649
 [7] top-level scope
   @ REPL[11]:1

julia> MOI.Utilities(Float64, *, Diagonal(a), func)
ERROR: MethodError: objects of type Module are not callable
Stacktrace:
 [1] top-level scope
   @ REPL[12]:1

julia> MOI.Utilities.operate(Float64, *, Diagonal(a), func)
ERROR: MethodError: no method matching operate(::Type{Float64}, ::typeof(*), ::Diagonal{Float64, MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}}, ::MathOptInterface.VectorOfVariables)

Closest candidates are:
  operate(::typeof(vcat), ::Type{T}, ::Union{MathOptInterface.VariableIndex, AbstractVector{T}, MathOptInterface.ScalarAffineFunction{T}, MathOptInterface.VectorAffineFunction{T}, MathOptInterface.VectorOfVariables, T}...) where T
   @ MathOptInterface ~/.julia/dev/MathOptInterface/src/Utilities/operate.jl:794
  operate(::typeof(vcat), ::Type{T}, ::Union{MathOptInterface.VariableIndex, AbstractVector{T}, MathOptInterface.ScalarAffineFunction{T}, MathOptInterface.ScalarQuadraticFunction{T}, MathOptInterface.VectorAffineFunction{T}, MathOptInterface.VectorOfVariables, MathOptInterface.VectorQuadraticFunction{T}, T}...) where T
   @ MathOptInterface ~/.julia/dev/MathOptInterface/src/Utilities/operate.jl:814
  operate(::typeof(+), ::Type{T}, ::Any, ::Any, ::Any, ::Any...) where T
   @ MathOptInterface ~/.julia/dev/MathOptInterface/src/Utilities/operate.jl:284
  ...

Stacktrace:
 [1] top-level scope
   @ REPL[13]:1

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), func)
┌                                             ┐
│0.0 + 1.0 MOI.VariableIndex(1)               │
│0.0 + 1.4142135623730951 MOI.VariableIndex(2)│
│0.0 + 1.0 MOI.VariableIndex(3)               │
└                                             ┘

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), ones(3))
3-element Vector{Float64}:
 1.0
 1.4142135623730951
 1.0
```
"""
struct SetDotScalingVector{T,S<:MOI.AbstractSet} <: AbstractVector{T}
    set::S
end

function SetDotScalingVector{T}(s::MOI.AbstractSet) where {T}
    return SetDotScalingVector{T,typeof(s)}(s)
end

function Base.getindex(s::SetDotScalingVector{T}, i::Base.Integer) where {T}
    return sqrt(_set_dot(i, s.set, T))
end

Base.size(x::SetDotScalingVector) = (MOI.dimension(x.set),)

function symmetric_matrix_scaling_vector(::Type{T}, n) where {T}
    d = side_dimension_for_vectorized_dimension(n)
    set = MOI.PositiveSemidefiniteConeTriangle(d)
    return SetDotScalingVector{T}(set)
end

function symmetric_matrix_inverse_scaling_vector(::Type{T}, n) where {T}
    return lazy_map(T, inv, symmetric_matrix_scaling_vector(T, n))
end

"""
    struct SymmetricMatrixScalingVector{T} <: AbstractVector{T}
        no_scaling::T
        scaling::T
        len::Int
    end

Vector of scaling for the entries of the vectorized form of
a symmetric matrix. The values `no_scaling` and `scaling`
are stored in the `struct` to avoid creating a new one for each entry.

!!! warning
    This type is deprecated, use `SetDotScalingVector` instead.
"""
struct SymmetricMatrixScalingVector{T} <: AbstractVector{T}
    scaling::T
    no_scaling::T
    len::Int
end

function SymmetricMatrixScalingVector{T}(scaling::T, len::Int) where {T}
    return SymmetricMatrixScalingVector{T}(scaling, one(T), len)
end

function Base.getindex(s::SymmetricMatrixScalingVector, i::Base.Integer)
    if is_diagonal_vectorized_index(i)
        return s.no_scaling
    else
        return s.scaling
    end
end

Base.size(x::SymmetricMatrixScalingVector) = (x.len,)

similar_type(::Type{<:MOI.LessThan}, ::Type{T}) where {T} = MOI.LessThan{T}

function similar_type(::Type{<:MOI.GreaterThan}, ::Type{T}) where {T}
    return MOI.GreaterThan{T}
end

similar_type(::Type{<:MOI.EqualTo}, ::Type{T}) where {T} = MOI.EqualTo{T}

similar_type(::Type{<:MOI.Interval}, ::Type{T}) where {T} = MOI.Interval{T}

function convert_approx(::Type{MOI.LessThan{T}}, set::MOI.LessThan) where {T}
    return MOI.LessThan{T}(set.upper)
end

function convert_approx(
    ::Type{MOI.GreaterThan{T}},
    set::MOI.GreaterThan,
) where {T}
    return MOI.GreaterThan{T}(set.lower)
end

function convert_approx(::Type{MOI.EqualTo{T}}, set::MOI.EqualTo) where {T}
    return MOI.EqualTo{T}(set.value)
end

function convert_approx(::Type{MOI.Interval{T}}, set::MOI.Interval) where {T}
    return MOI.Interval{T}(set.lower, set.upper)
end
