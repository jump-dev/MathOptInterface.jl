"""
    abstract type ProductOfSets{T} end

Represents a cartesian product of sets of given types.
"""
abstract type ProductOfSets{T} end

set_index(::ProductOfSets, ::Type{S}) where {S<:MOI.AbstractSet} = nothing

function _sets_code(esc_name, T, type_def, set_types...)
    code = Expr(:block, type_def)
    esc_types = esc.(set_types)
    for (i, esc_type) in enumerate(esc_types)
        push!(
            code.args,
            :(
                function $MOIU.set_index(
                    ::$esc_name{$(T)},
                    ::Type{$(esc_type)},
                ) where {$T}
                    return $i
                end
            ),
        )
    end
    push!(
        code.args,
        :($MOIU.set_types(::$esc_name{$T}) where {$T} = [$(esc_types...)]),
    )
    return code
end

"""
    abstract type MixOfScalarSets{T} <: ProductOfSets{T} end

Product of scalar sets in the order the constraints are added, mixing the
constraints of different types.

Use [`@mix_of_scalar_sets`](@ref) to generate a new subtype.
"""
abstract type MixOfScalarSets{T} <: ProductOfSets{T} end

"""
    @mix_of_scalar_sets(name, set_types...)

Generate a new [`MixOfScalarSets`](@ref) subtype.

## Example

```julia
@mix_of_scalar_sets(
    MixedIntegerLinearProgramSets,
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.EqualTo{T},
    MOI.Integer,
)
```
"""
macro mix_of_scalar_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(struct $(esc_name){$(T)} <: $(MOIU).MixOfScalarSets{$(T)}
        """
        `set_ids[i]` maps the row `i` to the corresponding set type.
        """
        set_ids::Vector{Int}

        $(esc_name){$(T)}() where {$(T)} = new(Int[])
    end)
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::MixOfScalarSets) = isempty(sets.set_ids)

MOI.empty!(sets::MixOfScalarSets) = empty!(sets.set_ids)

MOI.dimension(sets::MixOfScalarSets) = length(sets.set_ids)

rows(::MixOfScalarSets, ci::MOI.ConstraintIndex) = ci.value

function add_set(sets::MixOfScalarSets, i)
    push!(sets.set_ids, i)
    return length(sets.set_ids)
end

final_touch(::MixOfScalarSets) = nothing

function MOI.get(
    sets::MixOfScalarSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    present = Set(sets.set_ids)
    return Tuple{Type,Type}[
        (_affine_function_type(T, S), S) for
        S in set_types(sets) if set_index(sets, S) in present
    ]
end

function MOI.get(
    sets::MixOfScalarSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    i = set_index(sets, S)
    return count(isequal(i), sets.set_ids)
end

function MOI.get(
    sets::MixOfScalarSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    i = set_index(sets, S)
    return MOI.ConstraintIndex{F,S}[
        MOI.ConstraintIndex{F,S}(ci) for
        (ci, set_type) in enumerate(sets.set_ids) if set_type == i
    ]
end

function MOI.is_valid(
    sets::MixOfScalarSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    i = set_index(sets, S)
    if i === nothing
        return false
    end
    return ci.value in eachindex(sets.set_ids) && sets.set_ids[ci.value] == i
end

"""
    abstract type OrderedProductOfSets{T} <: ProductOfSets{T} end

Product of sets in the order the constraints are added, grouping the
constraints of the same types contiguously.

Use [`@product_of_sets`](@ref) to generate new subtypes.
"""
abstract type OrderedProductOfSets{T} <: ProductOfSets{T} end

"""
    @product_of_sets(name, set_types...)

Generate a new [`OrderedProductOfSets`](@ref) subtype.

## Example

```julia
@product_of_sets(
    LinearOrthants,
    MOI.Zeros,
    MOI.Nonnegatives,
    MOI.Nonpositives,
    MOI.ZeroOne,
)
```
"""
macro product_of_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(
        mutable struct $(esc_name){$(T)} <: $(MOIU).OrderedProductOfSets{$(T)}
            """
            During the copy, this counts the number of rows corresponding to
            each set. At the end of copy, `final_touch` is called, which
            converts this list into a cumulative ordering.
            """
            num_rows::Vector{Int}

            """
            A dictionary which maps the `set_index` and `offset` of a set to the
            dimension, i.e., `dimension[(set_index,offset)] → dim`.
            """
            dimension::Dict{Tuple{Int,Int},Int}

            """
            A sanity bit to check that we don't call functions out-of-order.
            """
            final_touch::Bool

            function $(esc_name){$(T)}() where {$(T)}
                return new(
                    zeros(Int, $(length(set_types))),
                    Dict{Tuple{Int,Int},Int}(),
                    false,
                )
            end
        end
    )
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::OrderedProductOfSets) = all(iszero, sets.num_rows)

function MOI.empty!(sets::OrderedProductOfSets)
    fill!(sets.num_rows, 0)
    empty!(sets.dimension)
    sets.final_touch = false
    return
end

function MOI.dimension(sets::OrderedProductOfSets)
    @assert sets.final_touch
    return sets.num_rows[end]
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)
    return (i == 1 ? 0 : sets.num_rows[i-1]) + ci.value
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)
    offset = i == 1 ? 0 : sets.num_rows[i-1]
    return (offset + ci.value - 1) .+ (1:sets.dimension[(i, ci.value)])
end

function add_set(sets::OrderedProductOfSets, i)
    @assert !sets.final_touch
    sets.num_rows[i] += 1
    return sets.num_rows[i]
end

function add_set(sets::OrderedProductOfSets, i, dim)
    @assert !sets.final_touch
    ci = sets.num_rows[i] + 1
    sets.dimension[(i, ci)] = dim
    sets.num_rows[i] += dim
    return ci
end

function final_touch(sets::OrderedProductOfSets)
    @assert !sets.final_touch
    for i in 2:length(sets.num_rows)
        sets.num_rows[i] += sets.num_rows[i-1]
    end
    sets.final_touch = true
    return
end

"""
    num_rows(sets::OrderedProductOfSets, ::Type{S}) where {S}

Return the number of rows corresponding to a set of type `S`. That is, it is
the sum of the dimensions of the sets of type `S`.
"""
function num_rows(sets::OrderedProductOfSets, ::Type{S}) where {S}
    i = set_index(sets, S)
    if !sets.final_touch || i == 1
        return sets.num_rows[i]
    end
    return sets.num_rows[i] - sets.num_rows[i-1]
end

function MOI.get(
    sets::OrderedProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    return Tuple{Type,Type}[
        (_affine_function_type(T, S), S) for
        S in set_types(sets) if num_rows(sets, S) > 0
    ]
end

struct _UnevenIterator
    i::Int
    start::Int
    stop::Int
    dimension::Dict{Tuple{Int,Int},Int}
end

Base.IteratorSize(::_UnevenIterator) = Base.SizeUnknown()

function Base.iterate(it::_UnevenIterator, cur = it.start)
    if cur > it.stop
        return nothing
    end
    return (cur, cur + it.dimension[(it.i, cur)])
end

function Base.in(x::Int64, it::_UnevenIterator)
    return it.start <= x <= it.stop && haskey(it.dimension, (it.i, x))
end

function _range_iterator(
    ::OrderedProductOfSets{T},
    ::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return start:stop
end

function _range_iterator(
    sets::OrderedProductOfSets{T},
    i::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.VectorAffineFunction{T}},
) where {T}
    return _UnevenIterator(i, start, stop, sets.dimension)
end

function _range_iterator(
    sets::OrderedProductOfSets{T},
    ::Type{F},
    ::Type{S},
) where {T,F,S}
    i = set_index(sets, S)
    if i === nothing || F != _affine_function_type(T, S)
        return
    end
    return _range_iterator(sets, i, 1, num_rows(sets, S), F)
end

_length(::Nothing) = 0
_length(r::UnitRange) = length(r)
_length(r::_UnevenIterator) = count(_ -> true, r)

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    r = _range_iterator(sets, F, S)
    return _length(r)
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    rows = _range_iterator(sets, F, S)
    if rows === nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(rows)
end

function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    r = _range_iterator(sets, F, S)
    return r !== nothing && ci.value in r
end
