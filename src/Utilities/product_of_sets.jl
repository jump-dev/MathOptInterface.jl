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
            `dimension[i][j]` is the dimension of `ConstraintIndex().value` `j`
            of set type `i`.
            """
            dimension::Vector{Vector{Int}}

            """
            `offset[i][j]` is the 0-indexed row offset of constraint `j` of set
            type `i`. The rows are therefore `offset[i][j] + 1:dimension[i][j]`.

            The `offset` vector gets created during `final_touch`.
            """
            offset::Vector{Vector{Int}}

            """
            The total number of rows in the sets.

            This value gets set during `final_touch`.
            """
            num_rows::Int

            """
            A sanity bit to check that we don't call functions out-of-order.
            """
            final_touch::Bool

            function $(esc_name){$(T)}() where {$(T)}
                n = $(length(set_types))
                return new([Int[] for _ in 1:n], Vector{Int}[], 0, false)
            end
        end
    )
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::OrderedProductOfSets) = all(isempty, sets.dimension)

function MOI.empty!(sets::OrderedProductOfSets)
    map(empty!, sets.dimension)
    empty!(sets.offset)
    sets.final_touch = false
    return
end

function MOI.dimension(sets::OrderedProductOfSets)
    @assert sets.final_touch
    return sets.num_rows
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)::Int
    return sets.offset[i][ci.value] + 1
end

function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    @assert sets.final_touch
    i = set_index(sets, S)::Int
    return sets.offset[i][ci.value] .+ (1:sets.dimension[i][ci.value])
end

function add_set(sets::OrderedProductOfSets, i, dim = 1)
    @assert !sets.final_touch
    push!(sets.dimension[i], dim)
    return length(sets.dimension[i])
end

function final_touch(sets::OrderedProductOfSets)
    @assert !sets.final_touch
    offset = 0
    for (i, dimension) in enumerate(sets.dimension)
        offsets = Int[]
        for d in dimension
            push!(offsets, offset)
            offset += d
            sets.num_rows += d
        end
        push!(sets.offset, offsets)
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
    i = set_index(sets, S)::Int
    return sum(sets.dimension[i])
end

function MOI.get(
    sets::OrderedProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    return Tuple{Type,Type}[
        (_affine_function_type(T, S), S) for
        (i, S) in enumerate(set_types(sets)) if !isempty(sets.dimension[i])
    ]
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return 0
    end
    return length(sets.dimension[i])
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(1:length(sets.dimension[i]))
end

function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    i = set_index(sets, S)::Union{Nothing,Int}
    if i == nothing
        return false
    end
    return 1 <= ci.value <= length(sets.dimension[i])
end
