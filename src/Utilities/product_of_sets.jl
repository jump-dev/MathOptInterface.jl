"""
    abstract type ProductOfSets{T} end

Product of sets in the order the constraints are added, grouping the
constraints of the same types contiguously.

Use [`@product_of_sets`](@ref) to generate new subtypes.
"""
abstract type ProductOfSets{T} end

set_index(::ProductOfSets, ::Type{<:MOI.AbstractSet}) = nothing

"""
    @product_of_sets(name, set_types...)

Generate a new [`ProductOfSets`](@ref) subtype.

If the set is typed, use `SetType{T}`.

## Example

```julia
@product_of_sets(
    LinearOrthants,
    MOI.Zeros,
    MOI.Nonnegatives,
    MOI.Nonpositives,
    MOI.EqualTo{T},
)
```
"""
macro product_of_sets(name, set_types...)
    esc_name = esc(name)
    esc_set_types = esc.(set_types)
    T = esc(:T)
    code = quote
        mutable struct $(esc_name){$(T)} <: $(MOIU).ProductOfSets{$(T)}
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

        function $(MOIU).set_types(::$(esc_name){$(T)}) where {$(T)}
            return [$(esc_set_types...)]
        end
    end
    for (i, type) in enumerate(esc_set_types)
        push!(
            code.args,
            :(
                function $MOIU.set_index(
                    ::$esc_name{$(T)},
                    ::Type{$(type)},
                ) where {$T}
                    return $(i)
                end
            ),
        )
    end
    return code
end

MOI.is_empty(sets::ProductOfSets) = all(iszero, sets.num_rows)

function MOI.empty!(sets::ProductOfSets)
    fill!(sets.num_rows, 0)
    empty!(sets.dimension)
    sets.final_touch = false
    return
end

function MOI.dimension(sets::ProductOfSets)
    @assert sets.final_touch
    return sets.num_rows[end]
end

function rows(
    sets::ProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)
    return (i == 1 ? 0 : sets.num_rows[i-1]) + ci.value
end

function rows(
    sets::ProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)
    offset = i == 1 ? 0 : sets.num_rows[i-1]
    return (offset + ci.value - 1) .+ (1:sets.dimension[(i, ci.value)])
end

function add_set(sets::ProductOfSets, i)
    @assert !sets.final_touch
    sets.num_rows[i] += 1
    return sets.num_rows[i]
end

function add_set(sets::ProductOfSets, i, dim)
    @assert !sets.final_touch
    ci = sets.num_rows[i] + 1
    sets.dimension[(i, ci)] = dim
    sets.num_rows[i] += dim
    return ci
end

function final_touch(sets::ProductOfSets)
    @assert !sets.final_touch
    for i in 2:length(sets.num_rows)
        sets.num_rows[i] += sets.num_rows[i-1]
    end
    sets.final_touch = true
    return
end

function _num_rows(sets::ProductOfSets, ::Type{S}) where {S}
    i = set_index(sets, S)
    if !sets.final_touch || i == 1
        return sets.num_rows[i]
    end
    return sets.num_rows[i] - sets.num_rows[i-1]
end

function MOI.get(
    sets::ProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    return Tuple{DataType,DataType}[
        (_affine_function_type(T, S), S) for
        S in set_types(sets) if _num_rows(sets, S) > 0
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
    ::ProductOfSets{T},
    ::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return start:stop
end

function _range_iterator(
    sets::ProductOfSets{T},
    i::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.VectorAffineFunction{T}},
) where {T}
    return _UnevenIterator(i, start, stop, sets.dimension)
end

function _range_iterator(
    sets::ProductOfSets{T},
    ::Type{F},
    ::Type{S},
) where {T,F,S}
    i = set_index(sets, S)
    if i === nothing || F != _affine_function_type(T, S)
        return
    end
    return _range_iterator(sets, i, 1, _num_rows(sets, S), F)
end

_length(::Nothing) = 0
_length(r::UnitRange) = length(r)
_length(r::_UnevenIterator) = count(_ -> true, r)

function MOI.get(
    sets::ProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    r = _range_iterator(sets, F, S)
    return _length(r)
end

function MOI.get(
    sets::ProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    rows = _range_iterator(sets, F, S)
    if rows === nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(rows)
end

function MOI.is_valid(
    sets::ProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    r = _range_iterator(sets, F, S)
    return r !== nothing && ci.value in r
end
