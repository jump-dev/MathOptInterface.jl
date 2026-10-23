# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Destinations are tagged so an inner index cannot alias a bridge slot.
struct InnerIndex
    value::Int64
end

struct BridgeSlot
    value::Int64
end

struct IndexMapping
    entries::Vector{Union{Nothing,InnerIndex,BridgeSlot}}
    reverse::Dict{Union{InnerIndex,BridgeSlot},Int64}
end

function IndexMapping()
    return IndexMapping(
        Union{Nothing,InnerIndex,BridgeSlot}[],
        Dict{Union{InnerIndex,BridgeSlot},Int64}(),
    )
end

"""
    IndexSpace()

An implicit identity prefix followed by a dense table of allocated outer
indices. The table is created only when the index spaces first diverge.
Entries identify inner indices or bridge slots; `nothing` reserves an index
or marks a deleted passthrough index. Allocated indices are never reused.
"""
mutable struct IndexSpace
    last_identity::Int64
    mapping::Union{Nothing,IndexMapping}
end

IndexSpace() = IndexSpace(0, nothing)

function activate!(space::IndexSpace)
    if space.mapping === nothing
        space.mapping = IndexMapping()
    end
    return
end

function allocate!(space::IndexSpace, destination; minimum::Int = 1)
    activate!(space)
    mapping = something(space.mapping)
    while space.last_identity + length(mapping.entries) + 1 < minimum
        push!(mapping.entries, nothing)
    end
    push!(mapping.entries, destination)
    value = space.last_identity + length(mapping.entries)
    if destination !== nothing
        mapping.reverse[destination] = value
    end
    return value
end

function destination(space::IndexSpace, value::Int64)
    if space.mapping === nothing
        return nothing
    end
    entries = something(space.mapping).entries
    i = value - space.last_identity
    return 1 <= i <= length(entries) ? entries[i] : nothing
end

function outer_to_inner(space::IndexSpace, value::Int64, default)
    if value <= space.last_identity
        return value
    end
    entry = destination(space, value)
    return entry isa InnerIndex ? entry.value : default
end

function inner_to_outer(space::IndexSpace, value::Int64, default)
    if value <= space.last_identity
        return value
    elseif space.mapping === nothing
        return default
    end
    return get(something(space.mapping).reverse, InnerIndex(value), default)
end

function remove_mapping!(space::IndexSpace, value::Int64)
    entry = destination(space, value)
    if entry isa InnerIndex
        mapping = something(space.mapping)
        delete!(mapping.reverse, entry)
        mapping.entries[value-space.last_identity] = nothing
    end
    return
end

"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged variables and the bridge that bridged the variable.

The user and inner model share indices until the first variable bridge is
added. Afterwards `variable_space` allocates both passthrough and bridged
variables, mapping the latter to slots in the dense bridge storage below.
`constraint_spaces` provides the same allocation and translation for each
`VectorOfVariables` constraint type whose namespaces diverge. Scalar variable
constraints derive their indices from the variable space.

Before activation, only the largest identity index is recorded. The inner
model must therefore be empty when wrapped and modified through the bridge
optimizer only.
"""
mutable struct Map <: AbstractDict{MOI.VariableIndex,AbstractBridge}
    # `slot` ->  `0`: the variable at `slot` was added with `add_constrained_variable`.
    # `slot` -> `-j`: the variable at `slot` was the first variable of
    #              `add_constrained_variables` with a
    #              `ConstraintIndex{MOI.VectorOfVariables}(j)` (note: `j > 0`).
    # `slot` ->  `j`: the variable at `slot` was the `j`th variable of
    #             `add_constrained_variables`.
    info::Vector{Int64}
    # `slot` ->  `-1`: the variable at `slot` was deleted.
    # `slot` ->  `0`: the variable at `slot` was added with `add_constrained_variable`.
    # `slot` ->  `j`: the variable at `slot` is the `j`th variable of a constrained
    #               vector of variables, taking deletion into account.
    index_in_vector::Vector{Int64}
    # `slot` -> `bridge`: the variable at `slot` was bridged by `bridge`.
    bridges::Vector{Union{Nothing,AbstractBridge}}
    sets::Vector{Union{Nothing,Type}}
    # If `nothing`, it cannot be computed because some bridges does not support it
    unbridged_function::Union{
        Nothing,
        Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}},
    }
    # Bridge that created this bridge, 0 if it is no bridge.
    parent_index::Vector{Int64}
    # Current bridge, 0 otherwise.
    current_context::Int64
    # Context of constraint bridged by constraint bridges
    constraint_context::Dict{MOI.ConstraintIndex,Int64}
    # `(ci::ConstraintIndex{MOI.VectorOfVariables}).value` ->
    # the `slot` of the first variable
    # and `0` if it is the index of a constraint bridge
    vector_of_variables_map::Vector{Int64}
    # `(ci::ConstraintIndex{MOI.VectorOfVariables}).value` ->
    # the dimension of the set
    vector_of_variables_length::Vector{Int64}
    # Same as in `MOI.Utilities.VariablesContainer`
    set_mask::Vector{UInt16}
    variable_space::IndexSpace
    constraint_spaces::Dict{Tuple{DataType,DataType},IndexSpace}
end

function Map()
    return Map(
        Int64[],
        Int64[],
        Union{Nothing,AbstractBridge}[],
        Union{Nothing,Type}[],
        Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}}(),
        Int64[],
        0,
        Dict{MOI.ConstraintIndex,Int64}(),
        Int64[],
        Int64[],
        UInt16[],
        IndexSpace(),
        Dict{Tuple{DataType,DataType},IndexSpace}(),
    )
end

# Implementation of `AbstractDict` interface.

Base.isempty(map::Map) = all(bridge -> bridge === nothing, map.bridges)

function Base.empty!(map::Map)
    empty!(map.info)
    empty!(map.index_in_vector)
    empty!(map.bridges)
    empty!(map.sets)
    if map.unbridged_function === nothing
        map.unbridged_function =
            Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}}()
    else
        empty!(something(map.unbridged_function))
    end
    empty!(map.parent_index)
    map.current_context = 0
    empty!(map.constraint_context)
    empty!(map.vector_of_variables_map)
    empty!(map.vector_of_variables_length)
    empty!(map.set_mask)
    map.variable_space = IndexSpace()
    empty!(map.constraint_spaces)
    return map
end

function is_constraint_mapping_active(
    map::Map,
    ::Type{F},
    ::Type{S},
) where {F,S}
    space = get(map.constraint_spaces, (F, S), nothing)
    return space !== nothing && space.mapping !== nothing
end

function record_identity!(map::Map, vi::MOI.VariableIndex)
    space = map.variable_space
    space.last_identity = max(space.last_identity, vi.value)
    return
end

function constraint_space(map::Map, ::Type{F}, ::Type{S}) where {F,S}
    return get!(IndexSpace, map.constraint_spaces, (F, S))
end

function record_identity!(map::Map, ci::MOI.ConstraintIndex{F,S}) where {F,S}
    space = constraint_space(map, F, S)
    space.last_identity = max(space.last_identity, ci.value)
    return
end

index_space(map::Map, ::MOI.VariableIndex) = map.variable_space
function index_space(map::Map, ::MOI.ConstraintIndex{F,S}) where {F,S}
    return constraint_space(map, F, S)
end

function outer_to_inner(map::Map, idx::MOI.Index)
    inner = outer_to_inner(map, idx, nothing)
    if inner === nothing
        throw(MOI.InvalidIndex(idx))
    end
    return inner
end

function outer_to_inner(map::Map, idx::I, default) where {I<:MOI.Index}
    value = outer_to_inner(index_space(map, idx), idx.value, nothing)
    return value === nothing ? default : I(value)
end

function inner_to_outer(map::Map, idx::MOI.Index)
    outer = inner_to_outer(map, idx, nothing)
    if outer === nothing
        throw(MOI.InvalidIndex(idx))
    end
    return outer
end

function inner_to_outer(map::Map, idx::I, default) where {I<:MOI.Index}
    value = inner_to_outer(index_space(map, idx), idx.value, nothing)
    return value === nothing ? default : I(value)
end

function activate_variable_mapping!(map::Map)
    activate!(map.variable_space)
    return
end

function activate_constraint_mapping!(
    map::Map,
    ::Type{F},
    ::Type{S},
) where {F,S}
    activate!(constraint_space(map, F, S))
    return
end

function allocate_variable!(map::Map, inner::MOI.VariableIndex)
    return MOI.VariableIndex(
        allocate!(map.variable_space, InnerIndex(inner.value)),
    )
end

function allocate_constraint!(
    map::Map,
    ::Type{F},
    ::Type{S},
    inner::Union{Nothing,MOI.ConstraintIndex{F,S}} = nothing;
    minimum::Int = 1,
) where {F,S}
    entry = inner === nothing ? nothing : InnerIndex(inner.value)
    return MOI.ConstraintIndex{F,S}(
        allocate!(constraint_space(map, F, S), entry; minimum),
    )
end

function is_bridged_variable(map::Map, vi::MOI.VariableIndex)
    return destination(map.variable_space, vi.value) isa BridgeSlot
end

function _slot(map::Map, vi::MOI.VariableIndex)
    entry = destination(map.variable_space, vi.value)
    return entry isa BridgeSlot ? entry.value : 0
end

function _variable(map::Map, slot::Integer)
    mapping = something(map.variable_space.mapping)
    return MOI.VariableIndex(mapping.reverse[BridgeSlot(slot)])
end

function bridge_index(map::Map, vi::MOI.VariableIndex)
    slot = _slot(map, vi)
    index = map.info[slot]
    if index ≤ 0
        return slot
    else
        return slot - index + 1
    end
end

function Base.haskey(map::Map, vi::MOI.VariableIndex)
    slot = _slot(map, vi)
    return slot != 0 &&
           map.bridges[bridge_index(map, vi)] !== nothing &&
           map.index_in_vector[slot] != -1
end

function Base.getindex(map::Map, vi::MOI.VariableIndex)
    return map.bridges[bridge_index(map, vi)]
end

function Base.delete!(map::Map, vi::MOI.VariableIndex)
    slot = _slot(map, vi)
    if iszero(map.info[slot])
        # Delete scalar variable
        index = bridge_index(map, vi)
        map.bridges[index] = nothing
        map.sets[index] = nothing
    elseif has_keys(map, [vi])
        # Delete whole vector
        delete!(map, [vi])
    else
        # Delete variable in vector and resize vector
        map.vector_of_variables_length[-map.info[bridge_index(map, vi)]] -= 1
        for s in slot:length(map.index_in_vector)
            if map.index_in_vector[s] == -1
                continue
            elseif bridge_index(map, vi) != bridge_index(map, _variable(map, s))
                break
            end
            map.index_in_vector[s] -= 1
        end
    end
    map.set_mask[slot] = MOI.Utilities._DELETED_VARIABLE
    map.index_in_vector[slot] = -1
    return map
end

function Base.delete!(map::Map, vis::Vector{MOI.VariableIndex})
    if !has_keys(map, vis)
        throw(
            ArgumentError(
                "`$vis` is not a valid key vector as returned by `add_keys_for_bridge`.",
            ),
        )
    end
    for vi in vis
        slot = _slot(map, vi)
        map.set_mask[slot] = MOI.Utilities._DELETED_VARIABLE
        map.index_in_vector[slot] = -1
    end
    map.bridges[bridge_index(map, first(vis))] = nothing
    map.sets[bridge_index(map, first(vis))] = nothing
    return map
end

function Base.keys(map::Map)
    return Base.Iterators.Filter(
        vi -> haskey(map, vi),
        MOI.Utilities.lazy_map(
            MOI.VariableIndex,
            slot -> _variable(map, slot),
            eachindex(map.bridges),
        ),
    )
end

Base.length(map::Map) = count(bridge -> bridge !== nothing, map.bridges)

function number_of_variables(map::Map)
    num = 0
    for slot in eachindex(map.bridges)
        if map.bridges[slot] !== nothing
            if iszero(map.info[slot])
                num += 1
            else
                num += length_of_vector_of_variables(map, _variable(map, slot))
            end
        end
    end
    return num
end

function Base.values(map::Map)
    # We don't use `filter` as it would compute the resulting array which
    # is not necessary if the caller just wants to iterater over `values`.
    return Base.Iterators.Filter(bridge -> bridge !== nothing, map.bridges)
end

function Base.iterate(map::Map, state = 1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return nothing
    else
        return _variable(map, state) => map.bridges[state], state + 1
    end
end

# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    constrained_set(map::Map, vi::MOI.VariableIndex)

Return the set type in which the bridged variable `vi` was added when it was
bridged.
"""
function constrained_set(map::Map, vi::MOI.VariableIndex)
    return map.sets[bridge_index(map, vi)]
end

"""
    number_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the number of bridged variables in `S`. Note that if `S` is a vector set,
bridging a vector of `n` variables only counts as 1.
"""
function number_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    return count(isequal(S), map.sets)
end

"""
    first_variable(::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex})

Return the `MOI.VariableIndex` of the `MOI.ConstraintFunction` of `ci`.
"""
function first_variable(::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex})
    return MOI.VariableIndex(ci.value)
end

"""
    first_variable(::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex})

Return the first `MOI.VariableIndex` of the `MOI.ConstraintFunction` of `ci`.
"""
function first_variable(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
)
    return _variable(map, map.vector_of_variables_map[ci.value])
end

function constraint(map::Map, vi::MOI.VariableIndex)
    S = constrained_set(map, vi)::Type{<:MOI.AbstractSet}
    F = MOI.Utilities.variable_function_type(S)
    index = bridge_index(map, vi)
    info = map.info[index]
    if iszero(info)
        # Scalar: by MOI convention `ci.value == vi.value`.
        return MOI.ConstraintIndex{F,S}(_variable(map, index).value)
    else
        # Vector: `info == -ci.value`.
        return MOI.ConstraintIndex{F,S}(-info)
    end
end

function MOI.is_valid(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    if !(ci.value in eachindex(map.vector_of_variables_map))
        return false
    end
    index = map.vector_of_variables_map[ci.value]
    return index in eachindex(map.bridges) &&
           !isnothing(map.bridges[index]) &&
           map.sets[index] === S
end

function MOI.is_valid(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    index = _slot(map, MOI.VariableIndex(ci.value))
    return index in eachindex(map.bridges) &&
           !isnothing(map.bridges[index]) &&
           map.sets[index] === S
end

"""
    MOI.add_constraint(map::Map, vi::MOI.VariableIndex, set::MOI.AbstractScalarSet)

Record that a constraint `vi`-in-`set` is added and throws if a lower or upper bound
is set by this constraint and such bound has already been set for `vi`.
"""
function MOI.add_constraint(::Map, ::MOI.VariableIndex, ::MOI.AbstractScalarSet)
    # Nothing to do as this is not recognized as setting a lower or upper bound
    return
end

# We cannot use `SUPPORTED_VARIABLE_SCALAR_SETS` because
# `Integer` and `ZeroOne` do not define `T` and we need `T`
# for `_throw_if_lower_bound_set`.
const _BOUNDED_VARIABLE_SCALAR_SETS{T} = Union{
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
    MOI.Semicontinuous{T},
    MOI.Semiinteger{T},
    MOI.Parameter{T},
}

function MOI.add_constraint(
    map::Map,
    vi::MOI.VariableIndex,
    ::S,
) where {T,S<:_BOUNDED_VARIABLE_SCALAR_SETS{T}}
    flag = MOI.Utilities._single_variable_flag(S)
    index = _slot(map, vi)
    mask = map.set_mask[index]
    MOI.Utilities._throw_if_lower_bound_set(vi, S, mask, T)
    MOI.Utilities._throw_if_upper_bound_set(vi, S, mask, T)
    map.set_mask[index] = mask | flag
    return
end

"""
    delete(map::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex,<:MOI.AbstractScalarSet})

Record that the constraint `vi`-in-`S` is deleted.
"""
function MOI.delete(
    ::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,<:MOI.AbstractScalarSet},
)
    # Nothing to do as this is not recognized as setting a lower or upper bound
    return
end

function MOI.delete(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {T,S<:_BOUNDED_VARIABLE_SCALAR_SETS{T}}
    flag = MOI.Utilities._single_variable_flag(S)
    map.set_mask[_slot(map, MOI.VariableIndex(ci.value))] &= ~flag
    return
end

"""
    constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the list of constraints corresponding to bridged variables in `S`.
"""
function constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    F = MOI.Utilities.variable_function_type(S)
    return MOI.ConstraintIndex{F,S}[
        constraint(map, _variable(map, i)) for
        i in eachindex(map.sets) if map.sets[i] == S
    ]
end

"""
    list_of_constraint_types(map::Map)

Return a list of all the different types `(F, S)` of `F`-in-`S` constraints in
`map`.
"""
function list_of_constraint_types(map::Map)
    list = Set{Tuple{Type,Type}}()
    for i in eachindex(map.bridges)
        if map.bridges[i] === nothing
            continue
        end
        S = map.sets[i]
        if S === nothing || S == MOI.Reals
            continue
        end
        push!(list, (MOI.Utilities.variable_function_type(S), S))
    end
    return list
end

"""
    has_keys(map::Map, vis::Vector{MOI.VariableIndex})::Bool

Return a `Bool` indicating whether `vis` was returned by
[`add_keys_for_bridge`](@ref) and has not been deleted yet.
"""
function has_keys(map::Map, vis::Vector{MOI.VariableIndex})
    return isempty(vis) || (
        length_of_vector_of_variables(map, first(vis)) == length(vis) &&
        all(
            vi -> bridge_index(map, vi) == bridge_index(map, first(vis)),
            vis,
        ) &&
        all(vi -> haskey(map, vi), vis) &&
        # Members of a vector have strictly ascending outer values (they are
        # consecutive at creation; a subset stays ascending after deletions).
        all(i -> vis[i].value > vis[i-1].value, 2:length(vis))
    )
end

"""
    length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)

If `vi` was bridged in a scalar set, it returns 0. Otherwise, it
returns the dimension of the set.
"""
function length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    info = map.info[bridge_index(map, vi)]
    if iszero(info)
        return 0
    else
        return map.vector_of_variables_length[-info]
    end
end

"""
    index_in_vector_of_variables(
        map::Map,
        vi::MOI.VariableIndex,
    )::MOI.Bridges.IndexInVector

Return the index of `vi` in the vector of variables in which it was bridged.
"""
function index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    return MOI.Bridges.IndexInVector(map.index_in_vector[_slot(map, vi)])
end

"""
    has_bridges(map::Map)::Bool

Return a `Bool` indicating whether any bridge was added yet. Note that it
returns `false` even if all bridges were deleted while `isempty` would return
`true`. It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used
by [`MOI.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut
operations in case variable bridges are not used.
"""
has_bridges(map::Map) = !isempty(map.info)

"""
    add_key_for_bridge(map::Map, bridge_fun::Function,
                       set::MOI.AbstractScalarSet)

Create a new variable index `vi`, store the mapping `vi => bridge` and
associate `vi` to `typeof(set)`. It returns a tuple with `vi` and the
constraint index
`MOI.ConstraintIndex{MOI.VariableIndex, typeof(set)}(vi.value)`.
"""
function add_key_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::MOI.AbstractScalarSet,
)
    push!(map.parent_index, map.current_context)
    bridge_index = Int64(length(map.parent_index))
    push!(map.info, 0)
    push!(map.index_in_vector, 0)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    push!(map.set_mask, 0x0000)
    value = allocate!(map.variable_space, BridgeSlot(bridge_index))
    variable = MOI.VariableIndex(value)
    map.bridges[bridge_index] = call_in_context(map, bridge_index, bridge_fun)
    if map.unbridged_function !== nothing
        mappings = unbridged_map(something(map.bridges[bridge_index]), variable)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    something(map.unbridged_function),
                    mapping.first => (bridge_index, mapping.second),
                )
            end
        end
    end
    MOI.add_constraint(map, variable, set)
    return variable, MOI.ConstraintIndex{MOI.VariableIndex,typeof(set)}(value)
end

"""
    function add_keys_for_bridge(
        map::Map,
        bridge_fun::Function,
        set::MOI.AbstractVectorSet;
        constraint_index::Union{Nothing,Int64} = nothing,
    )

Create vector of variable indices `variables`, stores the mapping
`vi => bridge` for each `vi ∈ variables` and associate `variables` to
`typeof(set)`. It returns a tuple with `variables` and a constraint index
`ci::MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}`. If
`constraint_index` is given, then `ci.value == constraint_index`.
"""
function add_keys_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::S;
    constraint_index::Union{Nothing,Int64} = nothing,
) where {S<:MOI.AbstractVectorSet}
    if iszero(MOI.dimension(set))
        return MOI.VariableIndex[],
        MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(0)
    end
    push!(map.parent_index, map.current_context)
    bridge_index = Int64(length(map.parent_index))
    # The optimizer supplies an index from the shared constraint allocator.
    if constraint_index === nothing
        constraint_index = length(map.vector_of_variables_map) + 1
    end
    @assert constraint_index > length(map.vector_of_variables_map)
    while length(map.vector_of_variables_map) + 1 < constraint_index
        push!(map.vector_of_variables_map, 0)
        push!(map.vector_of_variables_length, 0)
    end
    push!(map.vector_of_variables_map, bridge_index)
    push!(map.vector_of_variables_length, MOI.dimension(set))
    constraint_index = length(map.vector_of_variables_map)
    push!(map.info, -constraint_index)
    push!(map.index_in_vector, 1)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    push!(map.set_mask, 0x0000)
    allocate!(map.variable_space, BridgeSlot(bridge_index))
    for i in 2:MOI.dimension(set)
        push!(map.parent_index, 0)
        push!(map.info, i)
        push!(map.index_in_vector, i)
        push!(map.bridges, nothing)
        push!(map.sets, nothing)
        push!(map.set_mask, 0x0000)
        allocate!(map.variable_space, BridgeSlot(bridge_index + i - 1))
    end
    map.bridges[bridge_index] = call_in_context(map, bridge_index, bridge_fun)
    variables = MOI.VariableIndex[
        _variable(map, bridge_index - 1 + i) for i in 1:MOI.dimension(set)
    ]
    if map.unbridged_function !== nothing
        mappings =
            unbridged_map(something(map.bridges[bridge_index]), variables)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    something(map.unbridged_function),
                    mapping.first => (bridge_index, mapping.second),
                )
            end
        end
    end
    return variables,
    MOI.ConstraintIndex{MOI.VectorOfVariables,S}(constraint_index)
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex})

Return `vi` where `vi` is the bridged variable
corresponding to `ci`.
"""
function function_for(::Map, ci::MOI.ConstraintIndex{MOI.VariableIndex})
    return MOI.VariableIndex(ci.value)
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})

Return `MOI.VectorOfVariables(vis)` where `vis` is the vector of bridged
variables corresponding to `ci`.
"""
function function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})
    first_slot = map.vector_of_variables_map[ci.value]
    variables = MOI.VariableIndex[]
    for slot in first_slot:length(map.bridges)
        if map.index_in_vector[slot] == -1
            continue
        elseif bridge_index(map, _variable(map, slot)) == first_slot
            push!(variables, _variable(map, slot))
        else
            break
        end
    end
    return MOI.VectorOfVariables(variables)
end

"""
    throw_if_cannot_unbridge(map::Map)

Throw an error if some bridged variables do not have any reverse mapping.
"""
function throw_if_cannot_unbridge(map::Map)
    if map.unbridged_function === nothing
        err = MOI.GetAttributeNotAllowed(
            MOI.ConstraintFunction(),
            "Cannot unbridge function because some variables are bridged by " *
            "variable bridges that do not support reverse mapping, for " *
            "example, `ZerosBridge`.",
        )
        throw(err)
    end
end

"""
    unbridged_function(map::Map, vi::MOI.VariableIndex)

Return the expression of `vi` in terms of bridged variables.
"""
function unbridged_function(map::Map, vi::MOI.VariableIndex)
    throw_if_cannot_unbridge(map)
    context_func = get(something(map.unbridged_function), vi, nothing)
    if context_func === nothing
        return nothing
    end
    bridge_index, func = context_func
    # If the bridge bridging `vi` has index `bridge_index` or directly or
    # indirectly created this bridge then we don't unbridge the variable.
    context = map.current_context
    while !iszero(context)
        if bridge_index == context
            return nothing
        end
        context = map.parent_index[context]
    end
    return func
end

"""
    call_in_context(map::Map, bridge_index::Int64, f::Function)

Call function `f` in the context of the variable bridge of index `bridge_index`.
That is, the variable indices bridged by this bridge or the bridges that
created it will not be unbridged in [`unbridged_function`](@ref).
"""
function call_in_context(map::Map, bridge_index::Int64, f::Function)
    # This is a shortcut that is used in particular in the common case where
    # no variable bridge is used.
    if iszero(bridge_index) && iszero(map.current_context)
        return f()
    end
    previous_context = map.current_context
    map.current_context = bridge_index
    output = nothing
    try
        output = f()
    finally
        map.current_context = previous_context
    end
    return output
end

"""
    call_in_context(map::Map, vi::MOI.VariableIndex, f::Function)

Shortcut for `call_in_context(map, bridge_index, () -> f(bridge))` where
`vi` is bridged by `bridge` with index `bridge_index`.
"""
function call_in_context(map::Map, vi::MOI.VariableIndex, f::Function)
    idx = bridge_index(map, vi)
    return call_in_context(map, idx, () -> f(map.bridges[idx]))
end

"""
    call_in_context(map::Map, ci::MOI.ConstraintIndex, f::Function)

Shortcut for `call_in_context(map, bridge_index, f)` where `bridge_index` is the
variable bridge that created `ci` (directly or indirectly) or 0 otherwise.
"""
function call_in_context(map::Map, ci::MOI.ConstraintIndex, f::Function)
    return call_in_context(map, get(map.constraint_context, ci, Int64(0)), f)
end

"""
    register_context(map::Map, ci::MOI.ConstraintIndex)

Register the current context as the variable bridge that created `ci` (directly
or indirectly) or 0 otherwise.
"""
function register_context(map::Map, ci::MOI.ConstraintIndex)
    if !iszero(map.current_context)
        # By only storing non-zero values, we avoid any dictionary access for
        # constraint not created (directly or indirectly) by variable bridges.
        # This ensures that there is no performance hit of the bridge layer when
        # no variable bridge is used.
        map.constraint_context[ci] = map.current_context
    end
    return
end

"""
    EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MOI.Bridges.Constraint.SingleBridgeOptimizer`](@ref) as it does
not bridge any variable.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex,AbstractBridge} end

Base.show(::IO, ::EmptyMap) = nothing

Base.isempty(::EmptyMap) = true

function Base.empty!(::EmptyMap) end

Base.length(::EmptyMap) = 0

Base.keys(::EmptyMap) = MOI.Utilities.EmptyVector{MOI.VariableIndex}()

Base.values(::EmptyMap) = MOI.Utilities.EmptyVector{AbstractBridge}()

Base.iterate(::EmptyMap) = nothing

has_bridges(::EmptyMap) = false

number_of_variables(::EmptyMap) = 0

number_with_set(::EmptyMap, ::Type{<:MOI.AbstractSet}) = 0

function constraints_with_set(::EmptyMap, S::Type{<:MOI.AbstractSet})
    return MOI.ConstraintIndex{MOI.Utilities.variable_function_type(S),S}[]
end

register_context(::EmptyMap, ::MOI.ConstraintIndex) = nothing

call_in_context(::EmptyMap, ::MOI.ConstraintIndex, f::Function) = f()

MOI.is_valid(::EmptyMap, ::MOI.ConstraintIndex) = false
