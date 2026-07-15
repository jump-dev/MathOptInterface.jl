# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
                function $MOI.Utilities.set_index(
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
        :(function $MOI.Utilities.set_types(::$esc_name{$T}) where {$T}
            return [$(esc_types...)]
        end),
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

```jldoctest
julia> MOI.Utilities.@mix_of_scalar_sets(
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
    type_def =
        :(struct $(esc_name){$(T)} <: $MOI.Utilities.MixOfScalarSets{$(T)}
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

function add_set(sets::MixOfScalarSets, i::Int)::Int64
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

```jldoctest
julia> MOI.Utilities.@product_of_sets(
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
        mutable struct $(esc_name){$(T)} <:
                       $MOI.Utilities.OrderedProductOfSets{$(T)}
            """
            `rows[i][j]` corresponds to constraint `j` of set type `i`.

            The value depends on `final_touch`:
             * Before `final_touch`, these are `1:dimension` of the constraint
             * After `final_touch`, these are the 1-indexed rows of the full
               constraint matrix
            """
            rows::Vector{Vector{UnitRange{Int}}}

            """
            A sanity bit to check that we don't call functions out-of-order.
            """
            final_touch::Bool

            function $(esc_name){$(T)}() where {$(T)}
                n = $(length(set_types))
                return new([UnitRange{Int}[] for _ in 1:n], false)
            end
        end
    )
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::OrderedProductOfSets) = all(isempty, sets.rows)

function MOI.empty!(sets::OrderedProductOfSets)
    map(empty!, sets.rows)
    sets.final_touch = false
    return
end

function MOI.dimension(sets::OrderedProductOfSets)::Int
    @assert sets.final_touch
    for i in reverse(eachindex(sets.rows))
        if !isempty(sets.rows[i])
            return last(sets.rows[i][end])
        end
    end
    return 0  # All rows were empty.
end

# A backwards-compatible method to ensure callers of `sets.num_rows` still
# works.
function Base.getproperty(sets::OrderedProductOfSets, key::Symbol)
    if key == :num_rows
        if sets.final_touch
            return cumsum(num_rows(sets, S) for S in set_types(sets))
        end
        return Int[num_rows(sets, S) for S in set_types(sets)]
    end
    return getfield(sets, key)
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
)::Int where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)::Int
    return only(sets.rows[i][ci.value])
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
)::UnitRange{Int} where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)::Int
    return sets.rows[i][ci.value]
end

function add_set(sets::OrderedProductOfSets, i::Int, dim::Int = 1)::Int64
    @assert !sets.final_touch
    push!(sets.rows[i], 1:dim)
    return length(sets.rows[i])
end

function final_touch(sets::OrderedProductOfSets)::Nothing
    @assert !sets.final_touch
    offset = 0
    for (i, rows) in enumerate(sets.rows)
        for (j, row) in enumerate(rows)
            rows[j] = offset .+ row
            offset += length(row)
        end
    end
    sets.final_touch = true
    return
end

"""
    num_rows(sets::OrderedProductOfSets, ::Type{S}) where {S}

Return the number of rows corresponding to a set of type `S`. That is, it is
the sum of the dimensions of the sets of type `S`.
"""
function num_rows(sets::OrderedProductOfSets, ::Type{S})::Int where {S}
    i = set_index(sets, S)::Int
    rows = sets.rows[i]
    if isempty(rows)
        return 0
    elseif sets.final_touch
        return max(0, last(rows[end]) - first(rows[1]) + 1)
    else
        return mapreduce(length, +, rows)
    end
end

function MOI.get(
    sets::OrderedProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
)::Vector{Tuple{Type,Type}} where {T}
    return Tuple{Type,Type}[
        (_affine_function_type(T, S), S) for
        (i, S) in enumerate(set_types(sets)) if !isempty(sets.rows[i])
    ]
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return 0
    end
    return length(sets.rows[i])
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
)::Vector{MOI.ConstraintIndex{F,S}} where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(1:length(sets.rows[i]))
end

function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
)::Bool where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return false
    end
    return 1 <= ci.value <= length(sets.rows[i])
end
