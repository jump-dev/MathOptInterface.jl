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
set_index(sets::ProductOfSets, ::Type{S}) where {S<:MOI.AbstractSet} = nothing

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
        method = push!(
            code.args,
            :(
                function $MOIU.set_index(
                    ::$esc_name{$T},
                    ::Type{$esc_type},
                ) where {$T}
                    return $i
                end
            ),
        )
    end
    push!(code.args, :(function $MOIU.set_types(::$esc_name{$T}) where {$T}
        return [$(esc_types...)]
    end))
    return code
end

"""
    abstract type MixOfScalarSets{T} <: ProductOfSets{T} end

Product of scalar sets in the order the constraints are added, mixing the
constraints of different types.
"""
abstract type MixOfScalarSets{T} <: ProductOfSets{T} end
macro mix_of_scalar_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(struct $esc_name{$T} <: $MOIU.MixOfScalarSets{$T}
        set_ids::Vector{Int}
        function $esc_name{$T}() where {$T}
            return new(Int[])
        end
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
    return LazyMap{MOI.ConstraintIndex{F,S}}(
        j -> MOI.ConstraintIndex{F,S}(j),
        Base.Iterators.Filter(
            j -> sets.set_ids[j] == i,
            eachindex(sets.set_ids),
        ),
    )
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
"""
abstract type OrderedProductOfSets{T} <: ProductOfSets{T} end
macro product_of_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(
        struct $esc_name{$T} <: $MOIU.OrderedProductOfSets{$T}
            num_rows::Vector{Int}
            dimension::Dict{Tuple{Int,Int},Int}
            function $esc_name{$T}() where {$T}
                return new(zeros(Int, $(1 + length(set_types))), Dict{Int,Int}())
            end
        end
    )
    return _sets_code(esc_name, T, type_def, set_types...)
end

"""
    abstract type OrderedProductOfScalarSets{T} <: OrderedProductOfSets{T} end

Same as [`OrderedProductOfSets`](@ref) except that all types are scalar sets,
which allows a more efficient implementation.
"""
abstract type OrderedProductOfScalarSets{T} <: OrderedProductOfSets{T} end
macro product_of_scalar_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(struct $esc_name{$T} <: $MOIU.OrderedProductOfScalarSets{$T}
        num_rows::Vector{Int}
        function $esc_name{$T}() where {$T}
            return new(zeros(Int, $(1 + length(set_types))))
        end
    end)
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::OrderedProductOfSets) = all(iszero, sets.num_rows)
function MOI.empty!(sets::OrderedProductOfSets)
    fill!(sets.num_rows, 0)
    empty!(sets.dimension)
    return
end
function MOI.empty!(sets::OrderedProductOfScalarSets)
    fill!(sets.num_rows, 0)
    return
end

function MOI.dimension(sets::OrderedProductOfSets)
    for i in 3:length(sets.num_rows)
        sets.num_rows[i] += sets.num_rows[i-1]
    end
    return sets.num_rows[end]
end
function indices(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    return sets.num_rows[i] + ci.value + 1
end
function indices(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    return (sets.num_rows[i] + ci.value) .+ (1:sets.dimension[(i, ci.value)])
end
function add_set(sets::OrderedProductOfSets, i)
    offset = sets.num_rows[i+1]
    sets.num_rows[i+1] = offset + 1
    return offset
end
function add_set(sets::OrderedProductOfSets, i, dim)
    offset = sets.num_rows[i+1]
    sets.num_rows[i+1] = offset + dim
    sets.dimension[(i, offset)] = dim
    return offset
end
function _num_indices(sets::OrderedProductOfSets, ::Type{S}) where {S}
    i = set_index(sets, S)
    return sets.num_rows[i+1] - sets.num_rows[i]
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
struct UnevenIterator
    i::Int
    start::Int
    stop::Int
    dimension::Dict{Tuple{Int,Int},Int}
end
Base.IteratorSize(::UnevenIterator) = Base.SizeUnknown()
function Base.iterate(it::UnevenIterator, cur = it.start)
    if cur >= it.stop
        return nothing
    else
        return (cur, cur + it.dimension[(it.i, cur)])
    end
end
function Base.in(x, it::UnevenIterator)
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
    return UnevenIterator(i, start, stop, sets.dimension)
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
        return _range_iterator(
            sets,
            i,
            0,
            sets.num_rows[i+1] - sets.num_rows[i],
            F,
        )
    end
end
_length(r::UnitRange) = length(r)
_length(r::UnevenIterator) = count(_ -> true, r)
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
    return UnevenIterator(1, 1, 0, sets.dimension)
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
    return LazyMap{MOI.ConstraintIndex{F,S}}(
        i -> MOI.ConstraintIndex{F,S}(i),
        rows,
    )
end
function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    r = _range(sets, F, S)
    return r !== nothing && ci.value in r
end
