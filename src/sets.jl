# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Note: When adding a new set, also add it to Utilities.Model.

"""
    AbstractSet

Abstract supertype for set objects used to encode constraints.

## Required methods

For sets of type `S` with `isbitstype(S) == false`, you must implement:

 * `Base.copy(set::S)`
 * `Base.:(==)(x::S, y::S)`

Subtypes of `AbstractSet` such as [`AbstractScalarSet`](@ref) and
[`AbstractVectorSet`](@ref) may prescribe additional required methods.

## Optional methods

You may optionally implement:

 * [`dual_set`](@ref)
 * [`dual_set_type`](@ref)

## Note for developers

When creating a new set, the set struct must not contain any
[`VariableIndex`](@ref) or [`ConstraintIndex`](@ref) objects.
"""
abstract type AbstractSet end

function Base.copy(set::S) where {S<:AbstractSet}
    if isbitstype(S)
        return set
    end
    return error("Base.copy(::$S) is not implemented for this set.")
end

"""
    dimension(set::AbstractSet)

Return the [`output_dimension`](@ref) that an [`AbstractFunction`](@ref) should
have to be used with the set `set`.

## Example

```jldoctest
julia> MOI.dimension(MOI.Reals(4))
4

julia> MOI.dimension(MOI.LessThan(3.0))
1

julia> MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(2))
3
```
"""
function dimension end

"""
    dual_set(set::AbstractSet)

Return the dual set of `set`, that is the dual cone of the set. This follows the
definition of duality discussed in [Duality](@ref).

See [Dual cone](https://en.wikipedia.org/wiki/Dual_cone_and_polar_cone) for more
information.

If the dual cone is not defined it returns an error.

## Example

```jldoctest
julia> MOI.dual_set(MOI.Reals(4))
MathOptInterface.Zeros(4)

julia> MOI.dual_set(MOI.SecondOrderCone(5))
MathOptInterface.SecondOrderCone(5)

julia> MOI.dual_set(MOI.ExponentialCone())
MathOptInterface.DualExponentialCone()
```
"""
dual_set(set::AbstractSet) = error("Dual of $set is not implemented.")

"""
    dual_set_type(S::Type{<:AbstractSet})

Return the type of dual set of sets of type `S`, as returned by
[`dual_set`](@ref). If the dual cone is not defined it returns an error.

## Example

```jldoctest
julia> MOI.dual_set_type(MOI.Reals)
MathOptInterface.Zeros

julia> MOI.dual_set_type(MOI.SecondOrderCone)
MathOptInterface.SecondOrderCone

julia> MOI.dual_set_type(MOI.ExponentialCone)
MathOptInterface.DualExponentialCone
```
"""
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

## Required methods

All `AbstractVectorSet`s of type `S` must implement:

 * [`dimension`](@ref), unless the dimension is stored in the `set.dimension`
   field
 * [`Utilities.set_dot`](@ref), unless the dot product between two vectors in
   the set is equivalent to `LinearAlgebra.dot`.
"""
abstract type AbstractVectorSet <: AbstractSet end

dimension(s::AbstractVectorSet) = s.dimension

"""
    GreaterThan{T<:Real}(lower::T)

The set ``[lower, \\infty) \\subseteq \\mathbb{R}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.GreaterThan{Float64}}(1)
```
"""
struct GreaterThan{T<:Real} <: AbstractScalarSet
    lower::T
end

Base.:(==)(a::GreaterThan{T}, b::GreaterThan{T}) where {T} = a.lower == b.lower

"""
    LessThan{T<:Real}(upper::T)

The set ``(-\\infty, upper] \\subseteq \\mathbb{R}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.LessThan(2.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.LessThan{Float64}}(1)
```
"""
struct LessThan{T<:Real} <: AbstractScalarSet
    upper::T
end

Base.:(==)(a::LessThan{T}, b::LessThan{T}) where {T} = a.upper == b.upper

"""
    EqualTo{T<:Number}(value::T)

The set containing the single point ``\\{value\\} \\subseteq \\mathbb{R}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.EqualTo(2.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.EqualTo{Float64}}(1)
```
"""
struct EqualTo{T<:Number} <: AbstractScalarSet
    value::T
end

Base.:(==)(a::EqualTo{T}, b::EqualTo{T}) where {T} = a.value == b.value

"""
    Parameter{T<:Number}(value::T)

The set containing the single point ``\\{value\\} \\subseteq \\mathbb{R}``.

The `Parameter` set is conceptually similar to the [`EqualTo`](@ref) set, except
that a variable constrained to the `Parameter` set cannot have other constraints
added to it, and the `Parameter` set can never be deleted. Thus, solvers are
free to treat the variable as a constant, and they need not add it as a decision
variable to the model.

Because of this behavior, you must add parameters using [`add_constrained_variable`](@ref),
and solvers should declare [`supports_add_constrained_variable`](@ref) and not
[`supports_constraint`](@ref) for the `Parameter` set.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> p, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.5))
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Parameter{Float64}}(1))

julia> MOI.set(model, MOI.ConstraintSet(), ci, MOI.Parameter(3.0))

julia> MOI.get(model, MOI.ConstraintSet(), ci)
MathOptInterface.Parameter{Float64}(3.0)
```
"""
struct Parameter{T<:Number} <: AbstractScalarSet
    value::T
end

Base.:(==)(a::Parameter{T}, b::Parameter{T}) where {T} = a.value == b.value

"""
    Interval{T<:Real}(lower::T, upper::T)

The interval ``[lower, upper] \\subseteq \\mathbb{R} \\cup \\{-\\infty, +\\infty\\}``.

If `lower` or `upper` is `-Inf` or `Inf`, respectively, the set is interpreted
as a one-sided interval.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.Interval(1.0, 2.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(1)
```
"""
struct Interval{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end

function Interval(lower::Real, upper::Real)
    return Interval(promote(lower, upper)...)
end

function Base.:(==)(a::Interval{T}, b::Interval{T}) where {T}
    return a.lower == b.lower && a.upper == b.upper
end

"""
    Interval(set::GreaterThan{<:AbstractFloat})
    Interval(set::LessThan{<:AbstractFloat})
    Interval(set::EqualTo{<:Real})
    Interval(set::Interval{<:Real})

Construct an interval set from the set `set`, assuming any missing bounds are
`typemin(T)` or `typemax(T)`.

## Example

```jldoctest
julia> MOI.Interval(MOI.GreaterThan(1.0))
MathOptInterface.Interval{Float64}(1.0, Inf)

julia> MOI.Interval(MOI.LessThan(2.5))
MathOptInterface.Interval{Float64}(-Inf, 2.5)

julia> MOI.Interval(MOI.EqualTo(3))
MathOptInterface.Interval{Int64}(3, 3)

julia> MOI.Interval(MOI.Interval(5, 6))
MathOptInterface.Interval{Int64}(5, 6)
```
"""
Interval(s::GreaterThan{<:AbstractFloat}) = Interval(s.lower, typemax(s.lower))
Interval(s::LessThan{<:AbstractFloat}) = Interval(typemin(s.upper), s.upper)
Interval(s::EqualTo{<:Real}) = Interval(s.value, s.value)
Interval(s::Interval) = s

"""
    Integer()

The set of integers, ``\\mathbb{Z}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.Integer())
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1)
```
"""
struct Integer <: AbstractScalarSet end

"""
    ZeroOne()

The set ``\\{0, 1\\}``.

Variables belonging to the `ZeroOne` set are also known as "binary" variables.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.ZeroOne())
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(1)
```
"""
struct ZeroOne <: AbstractScalarSet end

"""
    Semicontinuous{T<:Real}(lower::T, upper::T)

The set ``\\{0\\} \\cup [lower, upper]``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.Semicontinuous(2.0, 3.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Semicontinuous{Float64}}(1)
```
"""
struct Semicontinuous{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end

function Semicontinuous(lower::Real, upper::Real)
    return Semicontinuous(promote(lower, upper)...)
end

function Base.:(==)(a::Semicontinuous{T}, b::Semicontinuous{T}) where {T}
    return a.lower == b.lower && a.upper == b.upper
end

"""
    Semiinteger{T<:Real}(lower::T, upper::T)

The set ``\\{0\\} \\cup \\{lower, lower+1, \\ldots, upper-1, upper\\}``.

Note that if `lower` and `upper` are not equivalent to an integer, then the
solver may throw an error, or it may round up `lower` and round down `upper` to
the nearest integers.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.add_constraint(model, x, MOI.Semiinteger(2.0, 3.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Semiinteger{Float64}}(1)
```
"""
struct Semiinteger{T<:Real} <: AbstractScalarSet
    lower::T
    upper::T
end

function Semiinteger(lower::Real, upper::Real)
    return Semiinteger(promote(lower, upper)...)
end

function Base.:(==)(a::Semiinteger{T}, b::Semiinteger{T}) where {T}
    return a.lower == b.lower && a.upper == b.upper
end

"""
    Reals(dimension::Int)

The set ``\\mathbb{R}^{dimension}`` (containing all points) of non-negative
dimension `dimension`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Reals(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Reals}(1)
```
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
    Zeros(dimension::Int)

The set ``\\{ 0 \\}^{dimension}`` (containing only the origin) of non-negative
dimension `dimension`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Zeros(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Zeros}(1)
```
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
    Nonnegatives(dimension::Int)

The nonnegative orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\ge 0 \\}`` of
non-negative dimension `dimension`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Nonnegatives}(1)
```
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
    Nonpositives(dimension::Int)

The nonpositive orthant ``\\{ x \\in \\mathbb{R}^{dimension} : x \\le 0 \\}`` of
non-negative dimension `dimension`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonpositives(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Nonpositives}(1)
```
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
    NormInfinityCone(dimension::Int)

The ``\\ell_\\infty``-norm cone ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_\\infty = \\max_i \\lvert x_i \\rvert \\}``
of dimension `dimension`.

The `dimension` must be at least `1`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables([t; x]), MOI.NormInfinityCone(4))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.NormInfinityCone}(1)
```
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
    NormOneCone(dimension::Int)

The ``\\ell_1``-norm cone ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_1 = \\sum_i \\lvert x_i \\rvert \\}``
of dimension `dimension`.

The `dimension` must be at least `1`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables([t; x]), MOI.NormOneCone(4))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.NormOneCone}(1)
```
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
    SecondOrderCone(dimension::Int)

The second-order cone (or Lorenz cone or ``\\ell_2``-norm cone)
``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\lVert x \\rVert_2 \\}`` of
dimension `dimension`.

The `dimension` must be at least `1`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables([t; x]), MOI.SecondOrderCone(4))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.SecondOrderCone}(1)
```
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
    RotatedSecondOrderCone(dimension::Int)

The rotated second-order cone ``\\{ (t,u,x) \\in \\mathbb{R}^{dimension} : 2tu \\ge \\lVert x \\rVert_2^2, t,u \\ge 0 \\}``
of dimension `dimension`.

The `dimension` must be at least `2`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> u = MOI.add_variable(model)
MOI.VariableIndex(2)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; u; x]),
           MOI.RotatedSecondOrderCone(5),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.RotatedSecondOrderCone}(1)
```
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
    NormCone(p::Float64, dimension::Int)

The ``\\ell_p``-norm cone ``\\{ (t,x) \\in \\mathbb{R}^{dimension} : t \\ge \\left(\\sum\\limits_i |x_i|^p\\right)^{\\frac{1}{p}} \\}``
of dimension `dimension`.

The `dimension` must be at least `1`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables([t; x]), MOI.NormCone(3, 4))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.NormCone}(1)
```
"""
struct NormCone <: AbstractVectorSet
    p::Float64
    dimension::Int

    function NormCone(p::Real, dimension::Base.Integer)
        if !(dimension >= 1)
            throw(
                DimensionMismatch(
                    "Dimension of NormCone must be >= 1, not " *
                    "$(dimension).",
                ),
            )
        elseif !(p >= 1)
            throw(ArgumentError("The p- argument to `NormCone` must be `>= 1`"))
        end
        return new(convert(Float64, p), dimension)
    end
end

NormCone(set::NormOneCone) = NormCone(1, set.dimension)
NormCone(set::SecondOrderCone) = NormCone(2, set.dimension)
NormCone(set::NormInfinityCone) = NormCone(Inf, set.dimension)

"""
    GeometricMeanCone(dimension::Int)

The geometric mean cone
``\\{ (t,x) \\in \\mathbb{R}^{n+1} : x \\ge 0, t \\le \\sqrt[n]{x_1 x_2 \\cdots x_n} \\}``,
where `dimension = n + 1 >= 2`.

## Duality note

The dual of the geometric mean cone is
``\\{ (u, v) \\in \\mathbb{R}^{n+1} : u \\le 0, v \\ge 0, -u \\le n \\sqrt[n]{\\prod_i v_i} \\}``,
where `dimension = n + 1 >= 2`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; x]),
           MOI.GeometricMeanCone(4),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.GeometricMeanCone}(1)
```
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

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.ExponentialCone())
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.ExponentialCone}(1)
```
"""
struct ExponentialCone <: AbstractVectorSet end

dual_set(::ExponentialCone) = DualExponentialCone()
dual_set_type(::Type{ExponentialCone}) = DualExponentialCone

dimension(::ExponentialCone) = 3

"""
    DualExponentialCone()

The 3-dimensional dual exponential cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : -u \\exp (v/u) \\le \\exp(1) w, u < 0 \\}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.DualExponentialCone())
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.DualExponentialCone}(1)
```
"""
struct DualExponentialCone <: AbstractVectorSet end

dual_set(::DualExponentialCone) = ExponentialCone()
dual_set_type(::Type{DualExponentialCone}) = ExponentialCone

dimension(::DualExponentialCone) = 3

"""
    PowerCone{T<:Real}(exponent::T)

The 3-dimensional power cone ``\\{ (x,y,z) \\in \\mathbb{R}^3 : x^{exponent} y^{1-exponent} \\ge |z|, x \\ge 0, y \\ge 0 \\}``
with parameter `exponent`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.PowerCone(0.5))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.PowerCone{Float64}}(1)
```
"""
struct PowerCone{T<:Real} <: AbstractVectorSet
    exponent::T
end

dual_set(s::PowerCone{T}) where {T<:Real} = DualPowerCone{T}(s.exponent)
dual_set_type(::Type{PowerCone{T}}) where {T<:Real} = DualPowerCone{T}

dimension(::PowerCone) = 3

function Base.:(==)(x::PowerCone{T}, y::PowerCone{T}) where {T}
    return x.exponent == y.exponent
end

"""
    DualPowerCone{T<:Real}(exponent::T)

The 3-dimensional power cone ``\\{ (u,v,w) \\in \\mathbb{R}^3 : (\\frac{u}{exponent})^{exponent} (\\frac{v}{1-exponent})^{1-exponent} \\ge |w|, u \\ge 0, v \\ge 0 \\}``
with parameter `exponent`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.DualPowerCone(0.5))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.DualPowerCone{Float64}}(1)
```
"""
struct DualPowerCone{T<:Real} <: AbstractVectorSet
    exponent::T
end

dual_set(s::DualPowerCone{T}) where {T<:Real} = PowerCone{T}(s.exponent)
dual_set_type(::Type{DualPowerCone{T}}) where {T<:Real} = PowerCone{T}

dimension(::DualPowerCone) = 3

function Base.:(==)(x::DualPowerCone{T}, y::DualPowerCone{T}) where {T}
    return x.exponent == y.exponent
end

"""
    RelativeEntropyCone(dimension::Int)

The relative entropy cone
``\\{ (u, v, w) \\in \\mathbb{R}^{1+2n} : u \\ge \\sum_{i=1}^n w_i \\log(\\frac{w_i}{v_i}), v_i \\ge 0, w_i \\ge 0 \\}``,
where `dimension = 2n + 1 >= 1`.

## Duality note

The dual of the relative entropy cone is
``\\{ (u, v, w) \\in \\mathbb{R}^{1+2n} : \\forall i, w_i \\ge u (\\log (\\frac{u}{v_i}) - 1), v_i \\ge 0, u > 0 \\}``
of dimension `dimension```{}=2n+1``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> u = MOI.add_variable(model);

julia> v = MOI.add_variables(model, 3);

julia> w = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([u; v; w]),
           MOI.RelativeEntropyCone(7),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.RelativeEntropyCone}(1)
```
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
    NormSpectralCone(row_dim::Int, column_dim::Int)

The epigraph of the matrix spectral norm (maximum singular value function)
``\\{ (t, X) \\in \\mathbb{R}^{1 + row_dim \\times column_dim} : t \\ge \\sigma_1(X) \\}``,
where ``\\sigma_i`` is the ``i``th singular value of the matrix ``X`` of
non-negative row dimension `row_dim` and column dimension `column_dim`.

The matrix X is vectorized by stacking the columns, matching the behavior of
Julia's `vec` function.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = reshape(MOI.add_variables(model, 6), 2, 3)
2×3 Matrix{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)  MOI.VariableIndex(4)  MOI.VariableIndex(6)
 MOI.VariableIndex(3)  MOI.VariableIndex(5)  MOI.VariableIndex(7)

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; vec(X)]),
           MOI.NormSpectralCone(2, 3),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.NormSpectralCone}(1)
```
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

dimension(s::NormSpectralCone) = 1 + s.row_dim * s.column_dim

"""
    NormNuclearCone(row_dim::Int, column_dim::Int)

The epigraph of the matrix nuclear norm (sum of singular values function)
``\\{ (t, X) \\in \\mathbb{R}^{1 + row_dim \\times column_dim} : t \\ge \\sum_i \\sigma_i(X) \\}``,
where ``\\sigma_i`` is the ``i``th singular value of the matrix ``X`` of
non-negative row dimension `row_dim` and column dimension `column_dim`.

The matrix X is vectorized by stacking the columns, matching the behavior of
Julia's `vec` function.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = reshape(MOI.add_variables(model, 6), 2, 3)
2×3 Matrix{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)  MOI.VariableIndex(4)  MOI.VariableIndex(6)
 MOI.VariableIndex(3)  MOI.VariableIndex(5)  MOI.VariableIndex(7)

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; vec(X)]),
           MOI.NormNuclearCone(2, 3),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.NormNuclearCone}(1)
```
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

dimension(s::NormNuclearCone) = 1 + s.row_dim * s.column_dim

"""
    abstract type AbstractSymmetricMatrixSetTriangle <: AbstractVectorSet end

Abstract supertype for subsets of the (vectorized) cone of symmetric matrices,
with [`side_dimension`](@ref) rows and columns. The entries of the upper-right
triangular part of the matrix are given column by column (or equivalently, the
entries of the lower-left triangular part are given row by row). A vectorized
cone of [`dimension`](@ref) ``n`` corresponds to a square matrix with side
dimension ``\\sqrt{1/4 + 2 n} - 1/2``. (Because a ``d \\times d`` matrix has
``d(d + 1) / 2`` elements in the upper or lower triangle.)

## Example

The matrix
```math
\\begin{bmatrix}
  1 & 2 & 4\\\\
  2 & 3 & 5\\\\
  4 & 5 & 6
\\end{bmatrix}
```
has [`side_dimension`](@ref) 3 and vectorization ``(1, 2, 3, 4, 5, 6)``.

## Note

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

## Duality note

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

## References

[1] Boyd, S. and Vandenberghe, L.. *Convex optimization*. Cambridge university
    press, 2004.
"""
abstract type AbstractSymmetricMatrixSetTriangle <: AbstractVectorSet end

is_set_dot_scaled(::Type{<:AbstractSymmetricMatrixSetTriangle}) = true

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

## Example

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
    PositiveSemidefiniteConeTriangle(side_dimension::Int) <: AbstractSymmetricMatrixSetTriangle

The (vectorized) cone of symmetric positive semidefinite matrices, with
non-negative `side_dimension` rows and columns.

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
    PositiveSemidefiniteConeSquare(side_dimension::Int) <: AbstractSymmetricMatrixSetSquare

The cone of symmetric positive semidefinite matrices, with non-negative side
length `side_dimension`.

See [`AbstractSymmetricMatrixSetSquare`](@ref) for more details on the
vectorized form.

The entries of the matrix are given column by column (or equivalently, row by
row).

The matrix is both constrained to be symmetric and to be positive semidefinite.
That is, if the functions in entries ``(i, j)`` and ``(j, i)`` are different,
then a constraint will be added to make sure that the entries are equal.

## Example

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

function dual_set(::PositiveSemidefiniteConeSquare)
    return error(
        "Dual of `PositiveSemidefiniteConeSquare` is not defined in " *
        "MathOptInterface.\nFor more details see the comments in " *
        "`src/Bridges/Constraint/bridges/square.jl`.",
    )
end

function dual_set_type(::Type{PositiveSemidefiniteConeSquare})
    return error(
        "Dual of `PositiveSemidefiniteConeSquare` is not defined in " *
        "MathOptInterface.\nFor more details see the comments in " *
        "`src/Bridges/Constraint/bridges/square.jl`.",
    )
end

function triangular_form(::Type{PositiveSemidefiniteConeSquare})
    return PositiveSemidefiniteConeTriangle
end

"""
    HermitianPositiveSemidefiniteConeTriangle(side_dimension::Int) <: AbstractVectorSet

The (vectorized) cone of Hermitian positive semidefinite matrices, with
non-negative `side_dimension` rows and columns.

Becaue the matrix is Hermitian, the diagonal elements are real, and the
complex-valued lower triangular entries are obtained as the conjugate of
corresponding upper triangular entries.

## Vectorization format

The vectorized form starts with real part of the entries of the upper triangular
part of the matrix, given column by column as explained in
[`AbstractSymmetricMatrixSetSquare`](@ref).

It is then followed by the imaginary part of the off-diagonal entries of the
upper triangular part, also given column by column.

For example, the matrix
```math
\\begin{bmatrix}
  1 & 2 + 7im & 4 + 8im\\\\
  2 - 7im & 3 & 5 + 9im\\\\
  4 - 8im & 5 - 9im & 6
\\end{bmatrix}
```
has [`side_dimension`](@ref) 3 and is represented as the vector
``[1, 2, 3, 4, 5, 6, 7, 8, 9]``.
"""
struct HermitianPositiveSemidefiniteConeTriangle <: AbstractVectorSet
    side_dimension::Int
end

function dimension(set::HermitianPositiveSemidefiniteConeTriangle)
    real_nnz = div(set.side_dimension * (set.side_dimension + 1), 2)
    imag_nnz = div((set.side_dimension - 1) * set.side_dimension, 2)
    return real_nnz + imag_nnz
end

dual_set(set::HermitianPositiveSemidefiniteConeTriangle) = set

function dual_set_type(::Type{HermitianPositiveSemidefiniteConeTriangle})
    return HermitianPositiveSemidefiniteConeTriangle
end

"""
    side_dimension(
        set::Union{
            AbstractSymmetricMatrixSetTriangle,
            AbstractSymmetricMatrixSetSquare,
            HermitianPositiveSemidefiniteConeTriangle,
        },
    )

Side dimension of the matrices in `set`.

## Convention

By convention, the side dimension should be stored in the `side_dimension`
field. If this is not the case for a subtype of [`AbstractSymmetricMatrixSetTriangle`](@ref),
or [`AbstractSymmetricMatrixSetSquare`](@ref) you must implement this method.
"""
function side_dimension(
    set::Union{
        AbstractSymmetricMatrixSetTriangle,
        AbstractSymmetricMatrixSetSquare,
        HermitianPositiveSemidefiniteConeTriangle,
    },
)
    return set.side_dimension
end

"""
    LogDetConeTriangle(side_dimension::Int)

The log-determinant cone
``\\{ (t, u, X) \\in \\mathbb{R}^{2 + d(d+1)/2} : t \\le u \\log(\\det(X/u)), u > 0 \\}``,
where the matrix `X` is represented in the same symmetric packed format as in
the `PositiveSemidefiniteConeTriangle`.

The non-negative argument `side_dimension` is the side dimension of the matrix
`X`, that is, its number of rows or columns.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; X]),
           MOI.LogDetConeTriangle(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.LogDetConeTriangle}(1)
```
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
    LogDetConeSquare(side_dimension::Int)

The log-determinant cone
``\\{ (t, u, X) \\in \\mathbb{R}^{2 + d^2} : t \\le u \\log(\\det(X/u)), X \\text{ symmetric}, u > 0 \\}``,
where the matrix `X` is represented in the same format as in the
[`PositiveSemidefiniteConeSquare`](@ref).

Similarly to [`PositiveSemidefiniteConeSquare`](@ref), constraints are added to
ensure that `X` is symmetric.

The non-negative argument `side_dimension` is the side dimension of the matrix
`X`, that is, its number of rows or columns.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = reshape(MOI.add_variables(model, 4), 2, 2)
2×2 Matrix{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)  MOI.VariableIndex(4)
 MOI.VariableIndex(3)  MOI.VariableIndex(5)

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; vec(X)]),
           MOI.LogDetConeSquare(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.LogDetConeSquare}(1)
```
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

side_dimension(s::LogDetConeSquare) = s.side_dimension

triangular_form(::Type{LogDetConeSquare}) = LogDetConeTriangle

triangular_form(set::LogDetConeSquare) = LogDetConeTriangle(set.side_dimension)

"""
    RootDetConeTriangle(side_dimension::Int)

The root-determinant cone
``\\{ (t, X) \\in \\mathbb{R}^{1 + d(d+1)/2} : t \\le \\det(X)^{1/d} \\}``,
where the matrix `X` is represented in the same symmetric packed format as in
the [`PositiveSemidefiniteConeTriangle`](@ref).

The non-negative argument `side_dimension` is the side dimension of the matrix
`X`, that is, its number of rows or columns.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; X]),
           MOI.RootDetConeTriangle(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.RootDetConeTriangle}(1)
```
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
    RootDetConeSquare(side_dimension::Int)

The root-determinant cone
``\\{ (t, X) \\in \\mathbb{R}^{1 + d^2} : t \\le \\det(X)^{1/d}, X \\text{ symmetric} \\}``,
where the matrix `X` is represented in the same format as
[`PositiveSemidefiniteConeSquare`](@ref).

Similarly to [`PositiveSemidefiniteConeSquare`](@ref), constraints are added to
ensure that `X` is symmetric.

The non-negative argument `side_dimension` is the side dimension of the matrix
`X`, that is, its number of rows or columns.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> t = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> X = reshape(MOI.add_variables(model, 4), 2, 2)
2×2 Matrix{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)  MOI.VariableIndex(4)
 MOI.VariableIndex(3)  MOI.VariableIndex(5)

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables([t; vec(X)]),
           MOI.RootDetConeSquare(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.RootDetConeSquare}(1)
```
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

side_dimension(s::RootDetConeSquare) = s.side_dimension

triangular_form(::Type{RootDetConeSquare}) = RootDetConeTriangle

function triangular_form(set::RootDetConeSquare)
    return RootDetConeTriangle(set.side_dimension)
end

"""
    is_set_dot_scaled(::Type{<:AbstractVectorFunction})

Return whether [`Utilities.set_dot(x, y)`](@ref Utilities.set_dot) is equivalent
to `x' * Diagonal(s) * y` for some scaling vector `s`. This scaling vector `s`
can be obtained with [`Utilities.SetDotScalingVector`](@ref).

!!! note
    For such sets `S`, we `Diagonal(s) * S` is [`MOI.Scaled(S)`](@ref Scaled).
    This linear relationship between the two sets allows briding between them
    with [`Bridges.Constraint.SetDotScalingBridge`](@ref) and
    [`Bridges.Constraint.SetDotInverseScalingBridge`](@ref). This scaling vector
    `s` is also used by Dualization.jl to compute the dual.
"""
is_set_dot_scaled(::Type{<:AbstractVectorSet}) = false

function is_set_dot_scaled(
    ::Type{
        <:Union{
            AbstractSymmetricMatrixSetTriangle,
            HermitianPositiveSemidefiniteConeTriangle,
            LogDetConeTriangle,
            RootDetConeTriangle,
        },
    },
)
    return true
end

"""
    struct Scaled{S<:AbstractVectorSet} <: AbstractVectorSet
        set::S
    end

Given a vector ``a \\in \\mathbb{R}^d`` and a `set` representing the set
``\\mathcal{S} \\in \\mathbb{R}^d`` such that [`Utilities.set_dot`](@ref) for
``x \\in \\mathcal{S}`` and ``y \\in \\mathcal{S}^*`` is
```math
\\sum_{i=1}^d a_i x_i y_i
```
the set `Scaled(set)` is defined as
```math
\\{ (\\sqrt{a_1} x_1, \\sqrt{a_2} x_2, \\ldots, \\sqrt{a_d} x_d) : x \\in S \\}
```

## Example

This can be used to scale a vector of numbers
```jldoctest scaling
julia> set = MOI.PositiveSemidefiniteConeTriangle(2)
MathOptInterface.PositiveSemidefiniteConeTriangle(2)

julia> a = MOI.Utilities.SetDotScalingVector{Float64}(set)
3-element MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}:
 1.0
 1.4142135623730951
 1.0

julia> using LinearAlgebra

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), ones(3))
3-element Vector{Float64}:
 1.0
 1.4142135623730951
 1.0
```

It can be also used to scale a vector of function
```jldoctest scaling
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> func = MOI.VectorOfVariables(x)
┌                    ┐
│MOI.VariableIndex(1)│
│MOI.VariableIndex(2)│
│MOI.VariableIndex(3)│
└                    ┘

julia> set = MOI.PositiveSemidefiniteConeTriangle(2)
MathOptInterface.PositiveSemidefiniteConeTriangle(2)

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), func)
┌                                             ┐
│0.0 + 1.0 MOI.VariableIndex(1)               │
│0.0 + 1.4142135623730951 MOI.VariableIndex(2)│
│0.0 + 1.0 MOI.VariableIndex(3)               │
└                                             ┘
```
"""
struct Scaled{S<:AbstractVectorSet} <: AbstractVectorSet
    set::S
end

dimension(s::Scaled) = dimension(s.set)

side_dimension(s::Scaled) = side_dimension(s.set)

dual_set(s::Scaled) = Scaled(dual_set(s.set))

dual_set_type(::Type{Scaled{S}}) where {S} = Scaled{dual_set_type(S)}

"""
    const ScaledPositiveSemidefiniteConeTriangle = Scaled{PositiveSemidefiniteConeTriangle}

The [`Scaled`](@ref) (vectorized) cone of symmetric positive semidefinite matrices, with
non-negative `side_dimension` rows and columns.

Compared to the [`PositiveSemidefiniteConeTriangle`](@ref), the off-diagonal
entries are scaled by `√2`. Thanks to this scaling, [`Utilities.set_dot`](@ref)
is the simply the sum of the pairwise product, while for
[`PositiveSemidefiniteConeTriangle`](@ref), the pairwise product additionally
have to be multiplied by `2`.

!!! note
    Using `MOI.ScaledPositiveSemidefiniteConeTriangle(d)` is deprecated, use
    `MOI.Scaled(MOI.PositiveSemidefiniteConeTriangle(d))`.
"""
const ScaledPositiveSemidefiniteConeTriangle =
    Scaled{PositiveSemidefiniteConeTriangle}

# TODO remove in MOI v2
function Scaled{PositiveSemidefiniteConeTriangle}(side_dimension::Int)
    return Scaled(PositiveSemidefiniteConeTriangle(side_dimension))
end

# TODO remove in MOI v2
function Base.getproperty(
    set::Scaled{PositiveSemidefiniteConeTriangle},
    f::Symbol,
)
    if f == :side_dimension
        return getproperty(getfield(set, :set), f)
    else
        @assert f == :set
        return getfield(set, f)
    end
end

"""
    SOS1{T<:Real}(weights::Vector{T})

The set corresponding to the Special Ordered Set (SOS) constraint of Type I.

Of the variables in the set, at most one can be nonzero.

The `weights` induce an ordering of the variables such that the *k*th element in
the set corresponds to the *k*th weight in `weights`. Solvers may use these
weights to improve the efficiency of the solution process, but the ordering does
not change the set of feasible solutions.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables(x),
           MOI.SOS1([1.0, 3.0, 2.5]),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.SOS1{Float64}}(1)
```
"""
struct SOS1{T<:Real} <: AbstractVectorSet
    weights::Vector{T}
end

dimension(set::SOS1) = length(set.weights)

Base.copy(set::SOS1{T}) where {T} = SOS1{T}(copy(set.weights))

Base.:(==)(a::SOS1{T}, b::SOS1{T}) where {T} = a.weights == b.weights

"""
    SOS2{T<:Real}(weights::Vector{T})

The set corresponding to the Special Ordered Set (SOS) constraint of Type II.

The `weights` induce an ordering of the variables such that the *k*th element in
the set corresponds to the *k*th weight in `weights`. Therefore, the weights
must be unique.

Of the variables in the set, at most two can be nonzero, and if two are nonzero,
they must be adjacent in the ordering of the set.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables(x),
           MOI.SOS2([1.0, 3.0, 2.5]),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.SOS2{Float64}}(1)
```
"""
struct SOS2{T<:Real} <: AbstractVectorSet
    weights::Vector{T}
end

dimension(set::SOS2) = length(set.weights)

Base.copy(set::SOS2{T}) where {T} = SOS2{T}(copy(set.weights))

Base.:(==)(a::SOS2{T}, b::SOS2{T}) where {T} = a.weights == b.weights

@_documented_enum(
    """
        ActivationCondition

    Activation condition for an indicator constraint.

    The enum value is used as first type parameter of `Indicator{A,S}`.
    """,
    ActivationCondition,
    """
    The indicator constraint holds when the binary variable is zero.
    """,
    ACTIVATE_ON_ZERO,
    """
    The indicator constraint holds when the binary variable is one.
    """,
    ACTIVATE_ON_ONE,
)

"""
    Indicator{ACTIVATE_ON_ZERO}(set::AbstractScalarSet)
    Indicator{ACTIVATE_ON_ONE}(set::AbstractScalarSet)

The set corresponding to an indicator constraint.

The type parameter must be an [`ActivationCondition`](@ref).

When the type parameter is [`ACTIVATE_ON_ZERO`](@ref), this means:

``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R} : y = 0 \\implies x \\in set\\}``

When the type parameter is [`ACTIVATE_ON_ONE`](@ref), this means:

``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R} : y = 1 \\implies x \\in set\\}``

## Notes

Most solvers expect that the first row of the function is interpretable as a
[`VariableIndex`](@ref) (for example, `1.0 * x + 0.0`), and that the variable is
constrained to the [`ZeroOne`](@ref) set. An error will be thrown if this is not
the case.

## Example

The constraint
``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R}^2 : y = 1 \\implies x_1 + x_2 \\leq 9 \\}``
is defined as
```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 2);

julia> y, _ = MOI.add_constrained_variable(model, MOI.ZeroOne());

julia> f = MOI.Utilities.vectorize([y, 1.0 * x[1] + 1.0 * x[2]])
┌                                                         ┐
│0.0 + 1.0 MOI.VariableIndex(3)                           │
│0.0 + 1.0 MOI.VariableIndex(1) + 1.0 MOI.VariableIndex(2)│
└                                                         ┘

julia> s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
MathOptInterface.Indicator{MathOptInterface.ACTIVATE_ON_ONE, MathOptInterface.LessThan{Float64}}(MathOptInterface.LessThan{Float64}(9.0))

julia> MOI.add_constraint(model, f, s)
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Indicator{MathOptInterface.ACTIVATE_ON_ONE, MathOptInterface.LessThan{Float64}}}(1)
```

The constraint
``\\{(y, x) \\in \\{0, 1\\} \\times \\mathbb{R} : y = 0 \\implies x = 0 \\}``
is defined as
```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> y, _ = MOI.add_constrained_variable(model, MOI.ZeroOne());

julia> f = MOI.VectorOfVariables([y, x]);

julia> s = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.EqualTo(0.0))
MathOptInterface.Indicator{MathOptInterface.ACTIVATE_ON_ZERO, MathOptInterface.EqualTo{Float64}}(MathOptInterface.EqualTo{Float64}(0.0))

julia> MOI.add_constraint(model, f, s)
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Indicator{MathOptInterface.ACTIVATE_ON_ZERO, MathOptInterface.EqualTo{Float64}}}(1)
```
"""
struct Indicator{A,S<:AbstractScalarSet} <: AbstractVectorSet
    set::S

    function Indicator{ACTIVATE_ON_ONE}(set::S) where {S<:AbstractScalarSet}
        return new{ACTIVATE_ON_ONE,S}(set)
    end
    function Indicator{ACTIVATE_ON_ZERO}(set::S) where {S<:AbstractScalarSet}
        return new{ACTIVATE_ON_ZERO,S}(set)
    end
end

dimension(::Indicator) = 2

Base.copy(set::Indicator{A}) where {A} = Indicator{A}(copy(set.set))

Base.:(==)(a::Indicator{A,S}, b::Indicator{A,S}) where {A,S} = a.set == b.set

"""
    Complements(dimension::Base.Integer)

The set corresponding to a mixed complementarity constraint.

Complementarity constraints should be specified with an
[`AbstractVectorFunction`](@ref)-in-`Complements(dimension)` constraint.

The dimension of the vector-valued function `F` must be `dimension`. This
defines a complementarity constraint between the scalar function `F[i]` and the
variable in `F[i + dimension/2]`. Thus, `F[i + dimension/2]` must be
interpretable as a single variable `x_i` (for example, `1.0 * x + 0.0`), and
`dimension` must be even.

The mixed complementarity problem consists of finding `x_i` in the interval
`[lb, ub]` (that is, in the set `Interval(lb, ub)`), such that the following holds:

  1. `F_i(x) == 0` if `lb_i < x_i < ub_i`
  2. `F_i(x) >= 0` if `lb_i == x_i`
  3. `F_i(x) <= 0` if `x_i == ub_i`

Classically, the bounding set for `x_i` is `Interval(0, Inf)`, which recovers:
`0 <= F_i(x) ⟂ x_i >= 0`, where the `⟂` operator implies `F_i(x) * x_i = 0`.

## Example

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

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x, _ = MOI.add_constrained_variable(model, MOI.Interval(-1.0, 1.0));

julia> MOI.add_constraint(
            model,
            MOI.Utilities.vectorize([-4.0 * x - 3.0, x]),
            MOI.Complements(2),
        )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Complements}(1)
```

The function `F` can also be defined in terms of single variables. For example,
the problem:

    [x_3, x_4] -in- Nonnegatives(2)
    [x_1, x_2, x_3, x_4] -in- Complements(4)

defines the complementarity problem where `0 <= x_1 ⟂ x_3 >= 0` and
`0 <= x_2 ⟂ x_4 >= 0`.

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 4);

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x[3:4]), MOI.Nonnegatives(2))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Nonnegatives}(1)

julia> MOI.add_constraint(
            model,
            MOI.VectorOfVariables(x),
            MOI.Complements(4),
        )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Complements}(1)
```
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

"""
    AllDifferent(dimension::Int)

The set ``\\{x \\in \\mathbb{Z}^{d}\\}`` such that no two elements in ``x`` take
the same value and `dimension = d`.

## Also known as

This constraint is called `all_different` in MiniZinc, and is sometimes also
called `distinct`.

## Example

To enforce `x[1] != x[2]` AND `x[1] != x[3]` AND `x[2] != x[3]`:

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.AllDifferent}(1)
```
"""
struct AllDifferent <: AbstractVectorSet
    dimension::Int
    function AllDifferent(dimension::Base.Integer)
        if dimension < 0
            throw(DimensionMismatch("Dimension of AllDifferent must be >= 0."))
        end
        return new(dimension)
    end
end

"""
    BinPacking(c::T, w::Vector{T}) where {T}

The set ``\\{x \\in \\mathbb{Z}^d\\}`` where `d = length(w)`, such that each
item `i` in `1:d` of weight `w[i]` is put into bin `x[i]`, and the total weight
of each bin does not exceed `c`.

There are additional assumptions that the capacity, `c`, and the weights, `w`,
must all be non-negative.

The bin numbers depend on the bounds of `x`, so they may be something other than
the integers `1:d`.

## Also known as

This constraint is called `bin_packing` in MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> bins = MOI.add_variables(model, 5)
5-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)
 MOI.VariableIndex(4)
 MOI.VariableIndex(5)

julia> weights = Float64[1, 1, 2, 2, 3]
5-element Vector{Float64}:
 1.0
 1.0
 2.0
 2.0
 3.0

julia> MOI.add_constraint.(model, bins, MOI.Integer())
5-element Vector{MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}}:
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(2)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(3)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(4)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(5)

julia> MOI.add_constraint.(model, bins, MOI.Interval(4.0, 6.0))
5-element Vector{MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}}:
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(1)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(2)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(3)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(4)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}}(5)

julia> MOI.add_constraint(model, MOI.VectorOfVariables(bins), MOI.BinPacking(3.0, weights))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.BinPacking{Float64}}(1)
```
"""
struct BinPacking{T} <: AbstractVectorSet
    capacity::T
    weights::Vector{T}
    function BinPacking(capacity::T, weights::Vector{T}) where {T}
        if capacity < zero(T)
            throw(DomainError(capacity, "capacity must be non-negative"))
        end
        if any(w -> w < zero(T), weights)
            throw(DomainError(weights, "weights must be non-negative"))
        end
        return new{T}(capacity, weights)
    end
end

dimension(set::BinPacking) = length(set.weights)

function Base.copy(set::BinPacking{T}) where {T}
    return BinPacking(set.capacity, copy(set.weights))
end

function Base.:(==)(x::BinPacking{T}, y::BinPacking{T}) where {T}
    return x.capacity == y.capacity && x.weights == y.weights
end

"""
    Circuit(dimension::Int)

The set ``\\{x \\in \\{1..d\\}^d\\}`` that constraints ``x`` to be a circuit,
such that ``x_i = j`` means that ``j`` is the successor of ``i``, and
`dimension = d`.

Graphs with multiple independent circuits, such as `[2, 1, 3]` and
`[2, 1, 4, 3]`, are not valid.

## Also known as

This constraint is called `circuit` in MiniZinc, and it is equivalent to forming
a (potentially sub-optimal) tour in the travelling salesperson problem.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Circuit(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Circuit}(1)
```
"""
struct Circuit <: AbstractVectorSet
    dimension::Int
    function Circuit(dimension::Base.Integer)
        if dimension < 0
            throw(DimensionMismatch("Dimension of Circuit must be >= 0."))
        end
        return new(dimension)
    end
end

"""
    CountAtLeast(n::Int, d::Vector{Int}, set::Set{Int})

The set ``\\{x \\in \\mathbb{Z}^{d_1 + d_2 + \\ldots d_N}\\}``, where `x` is
partitioned into `N` subsets (``\\{x_1,  \\ldots, x_{d_1}\\}``,
``\\{x_{d_1 + 1},  \\ldots, x_{d_1 + d_2}\\}`` and so on), and at least ``n``
elements of each subset take one of the values in `set`.

## Also known as

This constraint is called `at_least` in MiniZinc.

## Example

To ensure that `3` appears at least once in each of the subsets `{a, b}` and
`{b, c}`:

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> a, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1))

julia> b, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(2), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(2))

julia> c, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(3), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(3))

julia> x, d, set = [a, b, b, c], [2, 2], [3]
(MathOptInterface.VariableIndex[MOI.VariableIndex(1), MOI.VariableIndex(2), MOI.VariableIndex(2), MOI.VariableIndex(3)], [2, 2], [3])

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountAtLeast(1, d, Set(set)))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.CountAtLeast}(1)
```
"""
struct CountAtLeast <: AbstractVectorSet
    n::Int
    partitions::Vector{Int}
    set::Set{Int}
    function CountAtLeast(
        n::Base.Integer,
        partitions::Vector{Int},
        set::Set{Int},
    )
        if any(p <= 0 for p in partitions)
            throw(DimensionMismatch("Invalid partition dimension."))
        end
        return new(n, partitions, set)
    end
end

dimension(s::CountAtLeast) = sum(s.partitions)

function Base.copy(set::CountAtLeast)
    return CountAtLeast(set.n, copy(set.partitions), copy(set.set))
end

function Base.:(==)(x::CountAtLeast, y::CountAtLeast)
    return x.n == y.n && x.partitions == y.partitions && x.set == y.set
end

"""
    CountBelongs(dimenson::Int, set::Set{Int})

The set ``\\{(n, x) \\in \\mathbb{Z}^{1+d}\\}``, such that `n` elements of the
vector `x` take on of the values in `set` and `dimension = 1 + d`.

## Also known as

This constraint is called `among` by MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> n, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1))

julia> x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)
 MOI.VariableIndex(4)

julia> set = Set([3, 4, 5])
Set{Int64} with 3 elements:
  5
  4
  3

julia> MOI.add_constraint(model, MOI.VectorOfVariables([n; x]), MOI.CountBelongs(4, set))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.CountBelongs}(1)
```
"""
struct CountBelongs <: AbstractVectorSet
    dimension::Int
    set::Set{Int}
    function CountBelongs(dimension::Base.Integer, set::Set{Int})
        if dimension < 1
            throw(DimensionMismatch("Dimension of CountBelongs must be >= 1."))
        end
        return new(dimension, set)
    end
end

function Base.:(==)(x::CountBelongs, y::CountBelongs)
    return x.dimension == y.dimension && x.set == y.set
end

Base.copy(set::CountBelongs) = CountBelongs(set.dimension, copy(set.set))

"""
    CountDistinct(dimension::Int)

The set ``\\{(n, x) \\in \\mathbb{Z}^{1+d}\\}``, such that the number of distinct
values in `x` is `n` and `dimension = 1 + d`.

## Also known as

This constraint is called `nvalues` in MiniZinc.

## Example

To model:

 * if `n == 1`, then `x[1] == x[2] == x[3]`
 * if `n == 2`, then
    * `x[1] == x[2] != x[3]` or
    * `x[1] != x[2] == x[3]` or
    * `x[1] == x[3] != x[2]`
 * if `n == 3`, then `x[1] != x[2]`, `x[2] != x[3]` and `x[3] != x[1]`

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> n, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1))

julia> x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)
 MOI.VariableIndex(4)

julia> MOI.add_constraint(model, MOI.VectorOfVariables(vcat(n, x)), MOI.CountDistinct(4))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.CountDistinct}(1)
```

## Relationship to AllDifferent

When the first element is `d`, `CountDistinct` is equivalent to an
[`AllDifferent`](@ref) constraint.
"""
struct CountDistinct <: AbstractVectorSet
    dimension::Int
    function CountDistinct(dimension::Base.Integer)
        if dimension < 1
            throw(DimensionMismatch("Dimension of CountDistinct must be >= 1."))
        end
        return new(dimension)
    end
end

"""
    CountGreaterThan(dimension::Int)

The set ``\\{(c, y, x) \\in \\mathbb{Z}^{1+1+d}\\}``, such that `c` is strictly
greater than the number of occurences of `y` in `x` and `dimension = 1 + 1 + d`.

## Also known as

This constraint is called `count_gt` in MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> c, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1))

julia> y, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(2), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(2))

julia> x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(3)
 MOI.VariableIndex(4)
 MOI.VariableIndex(5)

julia> MOI.add_constraint(model, MOI.VectorOfVariables([c; y; x]), MOI.CountGreaterThan(5))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.CountGreaterThan}(1)
```
"""
struct CountGreaterThan <: AbstractVectorSet
    dimension::Int
    function CountGreaterThan(dimension::Base.Integer)
        if dimension < 2
            throw(
                DimensionMismatch(
                    "Dimension of CountGreaterThan must be >= 2.",
                ),
            )
        end
        return new(dimension)
    end
end

"""
    Cumulative(dimension::Int)

The set ``\\{(s, d, r, b) \\in \\mathbb{Z}^{3n+1}\\}``, representing the
`cumulative` global constraint, where
`n == length(s) == length(r) == length(b)` and `dimension = 3n + 1`.

`Cumulative` requires that a set of tasks given by start times ``s``, durations
``d``, and resource requirements ``r``, never requires more than the global
resource bound ``b`` at any one time.

## Also known as

This constraint is called `cumulative` in MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> d = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(4)
 MOI.VariableIndex(5)
 MOI.VariableIndex(6)

julia> r = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(7)
 MOI.VariableIndex(8)
 MOI.VariableIndex(9)

julia> b, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(10), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(10))

julia> MOI.add_constraint(model, MOI.VectorOfVariables([s; d; r; b]), MOI.Cumulative(10))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Cumulative}(1)
```
"""
struct Cumulative <: AbstractVectorSet
    dimension::Int
    function Cumulative(dimension::Base.Integer)
        if dimension < 1
            throw(DimensionMismatch("Dimension of Cumulative must be >= 1."))
        end
        return new(dimension)
    end
end

"""
    Path(from::Vector{Int}, to::Vector{Int})

Given a graph comprised of a set of nodes `1..N` and a set of arcs `1..E`
represented by an edge from node `from[i]` to node `to[i]`, `Path` constrains
the set
``(s, t, ns, es) \\in (1..N)\\times(1..E)\\times\\{0,1\\}^N\\times\\{0,1\\}^E``,
to form subgraph that is a path from node `s` to node `t`, where node `n` is in
the path if `ns[n]` is `1`, and edge `e` is in the path if `es[e]` is `1`.

The path must be acyclic, and it must traverse all nodes `n` for which `ns[n]`
is `1`, and all edges `e` for which `es[e]` is `1`.

## Also known as

This constraint is called `path` in MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> N, E = 4, 5
(4, 5)

julia> from = [1, 1, 2, 2, 3]
5-element Vector{Int64}:
 1
 1
 2
 2
 3

julia> to = [2, 3, 3, 4, 4]
5-element Vector{Int64}:
 2
 3
 3
 4
 4

julia> s, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(1))

julia> t, _ = MOI.add_constrained_variable(model, MOI.Integer())
(MOI.VariableIndex(2), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.Integer}(2))

julia> ns = MOI.add_variables(model, N)
4-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(3)
 MOI.VariableIndex(4)
 MOI.VariableIndex(5)
 MOI.VariableIndex(6)

julia> MOI.add_constraint.(model, ns, MOI.ZeroOne())
4-element Vector{MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}}:
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(3)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(4)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(5)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(6)

julia> es = MOI.add_variables(model, E)
5-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(7)
 MOI.VariableIndex(8)
 MOI.VariableIndex(9)
 MOI.VariableIndex(10)
 MOI.VariableIndex(11)

julia> MOI.add_constraint.(model, es, MOI.ZeroOne())
5-element Vector{MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}}:
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(7)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(8)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(9)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(10)
 MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(11)

julia> MOI.add_constraint(model, MOI.VectorOfVariables([s; t; ns; es]), MOI.Path(from, to))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Path}(1)
```
"""
struct Path <: AbstractVectorSet
    N::Int
    E::Int
    from::Vector{Int}
    to::Vector{Int}
    function Path(from::Vector{Int}, to::Vector{Int})
        @assert length(from) == length(to)
        E = length(from)
        N = max(maximum(from), maximum(to))
        return new(N, E, from, to)
    end
end

dimension(set::Path) = 2 + set.N + set.E

Base.copy(set::Path) = Path(copy(set.from), copy(set.to))

function Base.:(==)(x::Path, y::Path)
    return x.N == y.N && x.E == y.E && x.from == y.from && x.to == y.to
end

"""
    Table(table::Matrix{T}) where {T}

The set ``\\{x \\in \\mathbb{R}^d\\}`` where `d = size(table, 2)`, such that `x`
belongs to one row of `table`. That is, there exists some `j` in
`1:size(table, 1)`, such that `x[i] = table[j, i]` for all `i=1:size(table, 2)`.

## Also known as

This constraint is called `table` in MiniZinc.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3)
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> table = Float64[1 1 0; 0 1 1; 1 0 1; 1 1 1]
4×3 Matrix{Float64}:
 1.0  1.0  0.0
 0.0  1.0  1.0
 1.0  0.0  1.0
 1.0  1.0  1.0

julia> MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Table(table))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Table{Float64}}(1)
```
"""
struct Table{T} <: AbstractVectorSet
    table::Matrix{T}
end

dimension(set::Table) = size(set.table, 2)

Base.copy(set::Table) = Table(copy(set.table))

Base.:(==)(x::Table{T}, y::Table{T}) where {T} = x.table == y.table

"""
    HyperRectangle(lower::Vector{T}, upper::Vector{T}) where {T}

The set ``\\{x \\in \\bar{\\mathbb{R}}^d: x_i \\in [lower_i, upper_i] \\forall i=1,\\ldots,d\\}``.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3)
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> MOI.add_constraint(
           model,
           MOI.VectorOfVariables(x),
           MOI.HyperRectangle(zeros(3), ones(3)),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.HyperRectangle{Float64}}(1)
```
"""
struct HyperRectangle{T} <: AbstractVectorSet
    lower::Vector{T}
    upper::Vector{T}
    function HyperRectangle(lower::Vector{T}, upper::Vector{T}) where {T}
        l, u = length(lower), length(upper)
        if l != u
            throw(
                ArgumentError(
                    "length of lower (=$l) and upper (=$u) bounds must match.",
                ),
            )
        end
        return new{T}(lower, upper)
    end
end

dimension(set::HyperRectangle) = length(set.lower)

function Base.copy(set::HyperRectangle)
    return HyperRectangle(copy(set.lower), copy(set.upper))
end

function Base.:(==)(x::HyperRectangle{T}, y::HyperRectangle{T}) where {T}
    return x.lower == y.lower && x.upper == y.upper
end

"""
    Reified(set::AbstractSet)

The constraint ``[z; f(x)] \\in Reified(S)`` ensures that ``f(x) \\in S`` if and
only if ``z == 1``, where ``z \\in \\{0, 1\\}``.

```jldoctest
julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}());

julia> z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
(MOI.VariableIndex(1), MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(1))

julia> x = MOI.add_variable(model)
MOI.VariableIndex(2)

julia> MOI.add_constraint(
           model,
           MOI.Utilities.vectorize([z, 2.0 * x]),
           MOI.Reified(MOI.GreaterThan(1.0)),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Reified{MathOptInterface.GreaterThan{Float64}}}(1)
```
"""
struct Reified{S<:AbstractSet} <: AbstractVectorSet
    set::S
end

dimension(s::Reified) = 1 + dimension(s.set)

Base.copy(s::Reified) = Reified(copy(s.set))

function Base.:(==)(x::Reified{S}, y::Reified{S}) where {S}
    return x.set == y.set
end

"""
    VectorNonlinearOracle(;
        dimension::Int,
        l::Vector{Float64},
        u::Vector{Float64},
        eval_f::Function,
        jacobian_structure::Vector{Tuple{Int,Int}},
        eval_jacobian::Function,
        hessian_lagrangian_structure::Vector{Tuple{Int,Int}} = Tuple{Int,Int}[],
        eval_hessian_lagrangian::Union{Nothing,Function} = nothing,
    ) <: AbstractVectorSet

The set:
```math
S = \\{x \\in \\mathbb{R}^{dimension}: l \\le f(x) \\le u\\}
```
where ``f`` is defined by the vectors `l` and `u`, and the callback oracles
`eval_f`, `eval_jacobian`, and `eval_hessian_lagrangian`.

## f

The `eval_f` function must have the signature
```julia
eval_f(ret::AbstractVector, x::AbstractVector)::Nothing
```
which fills ``f(x)`` into the dense vector `ret`.

## Jacobian

The `eval_jacobian` function must have the signature
```julia
eval_jacobian(ret::AbstractVector, x::AbstractVector)::Nothing
```
which fills the sparse Jacobian ``\\nabla f(x)`` into `ret`.

The one-indexed sparsity structure must be provided in the `jacobian_structure`
argument and it must be independent of the point ``x``.

`jacobian_structure` is not required to be sorted and it may contain duplicates,
in which case the solver will combine the corresponding non-zero values in `ret`
by adding them together.

## Hessian

The `eval_hessian_lagrangian` function is optional.

If `eval_hessian_lagrangian === nothing`, Ipopt will use a Hessian approximation
instead of the exact Hessian.

If `eval_hessian_lagrangian` is a function, it must have the signature
```julia
eval_hessian_lagrangian(
    ret::AbstractVector,
    x::AbstractVector,
    μ::AbstractVector,
)::Nothing
```
which fills the sparse Hessian of the Lagrangian ``\\sum \\mu_i \\nabla^2 f_i(x)``
into `ret`.

The one-indexed sparsity structure must be provided in the
`hessian_lagrangian_structure` argument and it must be independent of the point
``x``.

`hessian_lagrangian_structure` is not required to be sorted and it may contain
duplicates, in which case the solver will combine the corresponding non-zero
values in `ret` by adding them together.

Any mix of lower and upper-triangular indices is valid. Elements `(i, j)` and
`(j, i)`, if both present, are treated as duplicates.

## Example

To model the set:
```math
\\begin{aligned}
0 &\\le x^2                    & \\le 1 \\\\
0 &\\le y^2 + x \\cdot z^3 - w & \\le 0
\\end{aligned}
```
do
```jldoctest
julia> import MathOptInterface as MOI

julia> set = MOI.VectorNonlinearOracle(;
           dimension = 4,
           l = [0.0, 0.0],
           u = [1.0, 0.0],
           eval_f = (ret, x) -> begin
               ret[1] = x[2]^2
               ret[2] = x[3]^2 + x[2] * x[4]^3 - x[1]
               return
           end,
           jacobian_structure = [(1, 2), (2, 1), (2, 2), (2, 3), (2, 4)],
           eval_jacobian = (ret, x) -> begin
               ret[1] = 2.0 * x[2]
               ret[2] = -1.0
               ret[3] = x[4]^3
               ret[4] = 2.0 * x[3]
               ret[5] = 3.0 * x[2] * x[4]^2
               return
           end,
           hessian_lagrangian_structure = [(2, 2), (3, 3), (4, 4), (2, 4)],
           eval_hessian_lagrangian = (ret, x, u) -> begin
               ret[1] = 2.0 * u[1]
               ret[2] = 2.0 * u[2]
               ret[3] = 6.0 * x[4] * u[2]
               ret[4] = 3.0 * x[4]^2 * u[2]
               return
           end,
       );

julia> set
VectorNonlinearOracle{Float64}(;
    dimension = 4,
    l = [0.0, 0.0],
    u = [1.0, 0.0],
    ...,
)
```
"""
struct VectorNonlinearOracle{T} <: AbstractVectorSet
    input_dimension::Int
    output_dimension::Int
    l::Vector{T}
    u::Vector{T}
    eval_f::Function
    jacobian_structure::Vector{Tuple{Int,Int}}
    eval_jacobian::Function
    hessian_lagrangian_structure::Vector{Tuple{Int,Int}}
    eval_hessian_lagrangian::Union{Nothing,Function}

    function VectorNonlinearOracle(;
        dimension::Int,
        l::Vector{T},
        u::Vector{T},
        eval_f::Function,
        jacobian_structure::Vector{Tuple{Int,Int}},
        eval_jacobian::Function,
        # The hessian_lagrangian is optional.
        hessian_lagrangian_structure::Vector{Tuple{Int,Int}} = Tuple{Int,Int}[],
        eval_hessian_lagrangian::Union{Nothing,Function} = nothing,
    ) where {T}
        if length(l) != length(u)
            throw(DimensionMismatch())
        end
        return new{T}(
            dimension,
            length(l),
            l,
            u,
            eval_f,
            jacobian_structure,
            eval_jacobian,
            hessian_lagrangian_structure,
            eval_hessian_lagrangian,
        )
    end
end

dimension(s::VectorNonlinearOracle) = s.input_dimension

function Base.copy(s::VectorNonlinearOracle)
    return VectorNonlinearOracle(;
        dimension = s.input_dimension,
        l = copy(s.l),
        u = copy(s.u),
        eval_f = s.eval_f,
        jacobian_structure = copy(s.jacobian_structure),
        eval_jacobian = s.eval_jacobian,
        hessian_lagrangian_structure = copy(s.hessian_lagrangian_structure),
        eval_hessian_lagrangian = s.eval_hessian_lagrangian,
    )
end

function Base.:(==)(
    x::VectorNonlinearOracle{T},
    y::VectorNonlinearOracle{T},
) where {T}
    return x.input_dimension == y.input_dimension &&
           x.output_dimension == y.output_dimension &&
           x.l == y.l &&
           x.u == y.u &&
           x.eval_f == y.eval_f &&
           x.jacobian_structure == y.jacobian_structure &&
           x.eval_jacobian == y.eval_jacobian &&
           x.hessian_lagrangian_structure == y.hessian_lagrangian_structure &&
           x.eval_hessian_lagrangian == y.eval_hessian_lagrangian
end

function Base.show(io::IO, s::VectorNonlinearOracle{T}) where {T}
    println(io, "VectorNonlinearOracle{$T}(;")
    println(io, "    dimension = ", s.input_dimension, ",")
    println(io, "    l = ", s.l, ",")
    println(io, "    u = ", s.u, ",")
    println(io, "    ...,")
    print(io, ")")
    return
end

# TODO(odow): these are not necessarily isbits. They may not be safe to return
# without copying if the number is BigFloat, for example.
function Base.copy(
    set::Union{
        GreaterThan,
        LessThan,
        EqualTo,
        Parameter,
        Interval,
        Semicontinuous,
        Semiinteger,
        PowerCone,
        DualPowerCone,
    },
)
    return set
end

"""
    supports_dimension_update(S::Type{<:MOI.AbstractVectorSet})

Return a `Bool` indicating whether the elimination of any dimension of
`n`-dimensional sets of type `S` give an `n-1`-dimensional set `S`.
By default, this function returns `false` so it should only be implemented
for sets that supports dimension update.

For instance, `supports_dimension_update(MOI.Nonnegatives)` is `true` because
the elimination of any dimension of the `n`-dimensional nonnegative orthant
gives the `n-1`-dimensional nonnegative orthant. However
`supports_dimension_update(MOI.ExponentialCone)` is `false`.
"""
supports_dimension_update(::Type{<:AbstractVectorSet}) = false

supports_dimension_update(::Type{Reals}) = true
supports_dimension_update(::Type{Zeros}) = true
supports_dimension_update(::Type{Nonnegatives}) = true
supports_dimension_update(::Type{Nonpositives}) = true

"""
    update_dimension(s::AbstractVectorSet, new_dim::Int)

Returns a set with the dimension modified to `new_dim`.
"""
function update_dimension end

update_dimension(::Reals, new_dim::Int) = Reals(new_dim)
update_dimension(::Zeros, new_dim::Int) = Zeros(new_dim)
update_dimension(::Nonnegatives, new_dim::Int) = Nonnegatives(new_dim)
update_dimension(::Nonpositives, new_dim::Int) = Nonpositives(new_dim)

"""
    constant(set::Union{EqualTo,GreaterThan,LessThan,Parameter})

Returns the constant term of the set `set`.

## Example

```jldoctest
julia> MOI.constant(MOI.GreaterThan(1.0))
1.0

julia> MOI.constant(MOI.LessThan(2.5))
2.5

julia> MOI.constant(MOI.EqualTo(3))
3

julia> MOI.constant(MOI.Parameter(4.5))
4.5
```
"""
constant(s::EqualTo) = s.value
constant(s::GreaterThan) = s.lower
constant(s::LessThan) = s.upper
constant(s::Parameter) = s.value
