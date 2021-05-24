"""
    abstract type ProductOfSets{T} end

Represents a cartesian product of sets of given types.
"""
abstract type ProductOfSets{T} end

"""
    set_index(sets::ProductOfSets, ::Type{S}) where {S<:MOI.AbstractSet}

Return an integer corresponding to the index of the set type in the list given
by [`set_types`](@ref). If this set is not part of the list then it returns
`nothing`.
"""
set_index(::ProductOfSets, ::Type{S}) where {S<:MOI.AbstractSet} = nothing

"""
    set_types(sets::ProductOfSets)

Return the list of the types of the sets allowed in the cartesian product.
"""
function set_types end

"""
    add_set(sets::OrderedProductOfSets, i)

Add a scalar set of type index `i`.

    add_set(sets::OrderedProductOfSets, i, dim)

Add a vector set of type index `i` and dimension `dim`.

Both method return a unique id of the set that
can be used to reference this set.
"""
function add_set end

"""
    indices(sets::OrderedProductOfSets, ci::MOI.ConstraintIndex)

Return the indices in `1:MOI.dimension(sets)` corresponding to the set of id
`ci.value`. For scalar sets, this return an integer and for vector sets, this
return an `UnitRange`.
"""
function indices end

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

If the sets are all scalar, use this type instead of [`OrderedProductOfSets`](@ref)
because it is a more efficient implementation.
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

indices(::MixOfScalarSets, ci::MOI.ConstraintIndex) = ci.value

function add_set(sets::MixOfScalarSets, i)
    push!(sets.set_ids, i)
    return length(sets.set_ids)
end

function MOI.get(
    sets::MixOfScalarSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    present = Set(sets.set_ids)
    return Tuple{DataType,DataType}[
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
        MOI.ConstraintIndex{F,S}(j) for
        j in eachindex(sets.set_ids) if sets.set_ids[j] == i
    ]
end

function MOI.is_valid(
    sets::MixOfScalarSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    i = set_index(sets, S)
    return i !== nothing &&
           ci.value in eachindex(sets.set_ids) &&
           sets.set_ids[ci.value] == i
end

"""
    abstract type OrderedProductOfSets{T} <: ProductOfSets{T} end

Product of sets in the order the constraints are added, grouping the
constraints of the same types contiguously.

Use [`@product_of_sets`](@ref) to generate new subtypes.

If the sets are all scalar, use [`MixOfScalarSets`](@ref) because it is a more
efficient implementation.
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
        struct $(esc_name){$(T)} <: $(MOIU).OrderedProductOfSets{$(T)}
            """
            The number of rows that each set takes.
            """
            num_rows::Vector{Int}

            """
            A dictionary which maps the `set_index` and `offset` of a set to the
            dimension, i.e., `dimension[(set_index,offset)] → dim`.
            """
            dimension::Dict{Tuple{Int,Int},Int}

            function $(esc_name){$(T)}() where {$(T)}
                return new(
                    zeros(Int, $(length(set_types))),
                    Dict{Tuple{Int,Int},Int}(),
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
    return
end

MOI.dimension(sets::OrderedProductOfSets) = sum(sets.num_rows)

function indices(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    offset = 0
    for j in 1:(i-1)
        offset += sets.num_rows[j]
    end
    return offset + ci.value + 1
end

function indices(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    offset = ci.value + 1
    for j in 1:(i-1)
        offset += sets.num_rows[j]
    end
    return offset:(offset+sets.dimension[(i, ci.value)]-1)
end

function add_set(sets::OrderedProductOfSets, i)
    sets.num_rows[i] += 1
    return sets.num_rows[i] - 1
end

function add_set(sets::OrderedProductOfSets, i, dim)
    ci = sets.num_rows[i]
    sets.dimension[(i, ci)] = dim
    sets.num_rows[i] += dim
    return ci
end

function _num_indices(sets::OrderedProductOfSets, ::Type{S}) where {S}
    i = set_index(sets, S)
    return sets.num_rows[i]
end

function MOI.get(
    sets::OrderedProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    return Tuple{DataType,DataType}[
        (_affine_function_type(T, S), S) for
        S in set_types(sets) if !iszero(_num_indices(sets, S))
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
    if cur >= it.stop
        return nothing
    else
        return (cur, cur + it.dimension[(it.i, cur)])
    end
end

function Base.in(x, it::_UnevenIterator)
    return x in it.start:(it.stop-1) && haskey(it.dimension, (it.i, x))
end

function _range_iterator(
    ::OrderedProductOfSets{T},
    ::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return start:(stop-1)
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

function _range(
    sets::OrderedProductOfSets{T},
    ::Type{F},
    ::Type{S},
) where {T,F,S}
    i = set_index(sets, S)
    if F != _affine_function_type(T, S) || i === nothing
        return nothing
    else
        return _range_iterator(sets, i, 0, sets.num_rows[i], F)
    end
end

_length(r::UnitRange) = length(r)
_length(r::_UnevenIterator) = count(_ -> true, r)

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    r = _range(sets, F, S)
    if r === nothing
        return 0
    else
        return _length(r)
    end
end

function _empty(
    ::OrderedProductOfSets{T},
    ::Type{<:MOI.ScalarAffineFunction},
) where {T}
    return 1:0
end

function _empty(
    sets::OrderedProductOfSets{T},
    ::Type{<:MOI.VectorAffineFunction},
) where {T}
    return _UnevenIterator(1, 1, 0, sets.dimension)
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    rows = _range(sets, F, S)
    if rows === nothing
        # Empty iterator
        rows = _empty(sets, F)
    end
    return MOI.ConstraintIndex{F,S}.(rows)
end

function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    r = _range(sets, F, S)
    return r !== nothing && ci.value in r
end
