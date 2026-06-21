# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged variables and the bridge that bridged the variable.

## Outer / inner index spaces

The user-facing ("outer") `VariableIndex` and `ConstraintIndex` namespaces are
independent from the ones used by `b.model` ("inner"). When no variable bridge
has ever been added to this `Map` the two namespaces coincide (identity
mapping) and no translation is performed. As soon as the first variable bridge
is added, [`activate_variable_mapping!`](@ref) is called: every existing
inner variable is copied into [`outer_to_inner`](@ref) and
[`inner_to_outer`](@ref) as an identity entry, and from that point on the
two namespaces drift apart: bridged variables get a fresh outer index with no
inner counterpart, non-bridged variables get a fresh outer index recorded
alongside their inner index.

Constraint indices follow the same rule per `(F, S)` pair. The first force-
bridged `CI{VariableIndex, S}` or `CI{VectorOfVariables, S}` triggers
[`activate_constraint_mapping!`](@ref) for that `(F, S)`: all existing inner
`CI{F, S}` are copied in as identity entries; afterwards the outer and inner
`CI{F, S}` namespaces are independent.

## Internal slot indexing

Bridged variables are identified by a **positive** outer `VariableIndex.value`
allocated from the shared outer counter (see [`next_outer_variable!`](@ref)).
Internally, the per-bridge data is stored in dense `Vector`s indexed by a
1-based `slot`. [`variable_to_slot`](@ref) maps an outer value to its slot and
[`slot_to_variable`](@ref) is the inverse (parallel to `bridges`). A bridged
variable is **outer-only**: it has no entry in `outer_to_inner`/`inner_to_outer`.
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
    # outer `VariableIndex.value` of a bridged variable -> its `slot`.
    variable_to_slot::Dict{Int64,Int64}
    # `slot` -> outer `VariableIndex.value` (inverse of `variable_to_slot`,
    # parallel to `bridges`).
    slot_to_variable::Vector{Int64}
    # Outer (user-facing) -> inner (`b.model`) translation. Empty until the
    # first variable bridge is added, at which point existing inner variables
    # are added as identity. After activation, every variable in the outer
    # namespace has an entry here (bridged ones map to the sentinel `0`).
    outer_to_inner::MOI.Utilities.IndexMap
    # Reverse of `outer_to_inner` for the entries that have an inner
    # counterpart (i.e., bridged outer-only entries are absent).
    inner_to_outer::MOI.Utilities.IndexMap
    # Next available outer `VariableIndex.value` once variable mapping has
    # been activated; `0` until then.
    next_outer_variable::Int64
    # Per-`(F, S)` next available outer `ConstraintIndex{F, S}.value`. A
    # missing entry means that `(F, S)` is in identity mode; presence means
    # constraint mapping has been activated for `(F, S)`.
    next_outer_constraint::Dict{Tuple{DataType,DataType},Int64}
end

function Map()
    return Map(
        Int64[],
        Int64[],
        Union{Nothing,AbstractBridge}[],
        Union{Nothing,Type}[],
        Dict{MOI.VariableIndex,MOI.AbstractScalarFunction}(),
        Int64[],
        0,
        Dict{MOI.ConstraintIndex,Int64}(),
        Int64[],
        Int64[],
        UInt16[],
        Dict{Int64,Int64}(),
        Int64[],
        MOI.Utilities.IndexMap(),
        MOI.Utilities.IndexMap(),
        0,
        Dict{Tuple{DataType,DataType},Int64}(),
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
    empty!(map.variable_to_slot)
    empty!(map.slot_to_variable)
    map.outer_to_inner = MOI.Utilities.IndexMap()
    map.inner_to_outer = MOI.Utilities.IndexMap()
    map.next_outer_variable = 0
    empty!(map.next_outer_constraint)
    return map
end

"""
    is_constraint_mapping_active(map::Map, ::Type{F}, ::Type{S})::Bool

Return `true` once at least one `CI{F, S}` has been force-bridged at this
layer (and hence the outer/inner translation for `(F, S)` has been
materialized).

Note that this is strictly stronger than [`has_bridges`](@ref): the latter
is `true` as soon as any variable bridge exists, while constraint mapping
only activates for the specific `(F, S)` pairs whose outer and inner
namespaces have diverged because a `VariableIndex`/`VectorOfVariables`
constraint was force-bridged. Regular `F`-in-`S` constraints are bridged
per *type*, never per *instance*, so their namespaces never diverge and
this stays `false` for them.
"""
function is_constraint_mapping_active(
    map::Map,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return haskey(map.next_outer_constraint, (F, S))
end

"""
    activate_variable_mapping!(map::Map, model::MOI.ModelLike)

Materialize identity mappings for every variable currently in `model`, so
that subsequent outer-only or inner-only allocations can extend the two
namespaces independently. No-op if the mapping is already active.

`model` is the inner model that this `Map` translates against (typically
`b.model` of the enclosing `AbstractBridgeOptimizer`).
"""
function activate_variable_mapping!(map::Map, model::MOI.ModelLike)
    # `has_bridges` flips to `true` only inside `add_key_for_bridge` (which
    # pushes to `map.info`), and that always runs *after* this function in
    # `add_constrained_variable`. So on the first variable bridge this is
    # still `false` and we populate; any nested re-entry (a variable bridge
    # that itself adds constrained variables) sees `true` and is a no-op.
    if has_bridges(map)
        return
    end
    max_value = Int64(0)
    for inner_vi in MOI.get(model, MOI.ListOfVariableIndices())
        map.outer_to_inner[inner_vi] = inner_vi
        map.inner_to_outer[inner_vi] = inner_vi
        if inner_vi.value > max_value
            max_value = inner_vi.value
        end
    end
    map.next_outer_variable = max_value + 1
    return
end

"""
    activate_constraint_mapping!(
        map::Map,
        model::MOI.ModelLike,
        ::Type{F},
        ::Type{S},
    )

Materialize identity mappings for every `CI{F, S}` currently in `model`.
Called when the first `CI{F, S}` is force-bridged at this layer. No-op if
already active for `(F, S)`.
"""
function activate_constraint_mapping!(
    map::Map,
    model::MOI.ModelLike,
    ::Type{F},
    ::Type{S},
) where {F,S}
    if is_constraint_mapping_active(map, F, S)
        return
    end
    max_value = Int64(0)
    for inner_ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        map.outer_to_inner[inner_ci] = inner_ci
        map.inner_to_outer[inner_ci] = inner_ci
        if inner_ci.value > max_value
            max_value = inner_ci.value
        end
    end
    map.next_outer_constraint[(F, S)] = max_value + 1
    return
end

"""
    next_outer_variable!(map::Map)::Int64

Return a fresh `Int64` value to use as a `VariableIndex.value` in the outer
namespace and advance the internal counter.
"""
function next_outer_variable!(map::Map)
    @assert has_bridges(map)
    value = map.next_outer_variable
    map.next_outer_variable = value + 1
    return value
end

"""
    next_outer_constraint!(map::Map, ::Type{F}, ::Type{S})::Int64

Return a fresh `Int64` value to use as a `ConstraintIndex{F, S}.value` in
the outer namespace and advance the internal `(F, S)` counter.
"""
function next_outer_constraint!(
    map::Map,
    ::Type{F},
    ::Type{S},
) where {F,S}
    @assert is_constraint_mapping_active(map, F, S)
    value = map.next_outer_constraint[(F, S)]
    map.next_outer_constraint[(F, S)] = value + 1
    return value
end

"""
    is_bridged_variable(map, vi::MOI.VariableIndex)::Bool

Return `true` if `vi` is (or was) a variable bridged by `map`. Unlike
[`haskey`](@ref), this stays `true` after the variable is deleted (the
outer index is never reused), matching the role of the former
`vi.value < 0` test.
"""
is_bridged_variable(map::Map, vi::MOI.VariableIndex) =
    haskey(map.variable_to_slot, vi.value)

# Internal `slot` of the bridged variable `vi`, i.e. the index into the dense
# per-bridge `Vector`s. Errors if `vi` is not a bridged variable.
_slot(map::Map, vi::MOI.VariableIndex) = map.variable_to_slot[vi.value]

# Outer `VariableIndex` stored at `slot`.
_variable(map::Map, slot::Integer) = MOI.VariableIndex(map.slot_to_variable[slot])

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
    slot = get(map.variable_to_slot, vi.value, 0)
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
        return MOI.ConstraintIndex{F,S}(map.slot_to_variable[index])
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
    index = get(map.variable_to_slot, ci.value, 0)
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
        all(i -> _slot(map, vis[i]) == _slot(map, vis[i-1]) + 1, 2:length(vis))
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
    # Allocate the positive outer index for the new bridged variable and
    # record the outer <-> slot bijection. Must be done after pushing to
    # `map.info` so that `has_bridges(map)` is `true`.
    value = next_outer_variable!(map)
    push!(map.slot_to_variable, value)
    map.variable_to_slot[value] = bridge_index
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
        set::MOI.AbstractVectorSet,
        is_available::Function,
    )

Create vector of variable indices `variables`, stores the mapping
`vi => bridge` for each `vi ∈ variables` and associate `variables` to
`typeof(set)`. It returns a tuple with `variables` and a constraint index
`ci::MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}` such that
`is_available(ci)`.
"""
function add_keys_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::S,
    is_available::Function,
) where {S<:MOI.AbstractVectorSet}
    if iszero(MOI.dimension(set))
        return MOI.VariableIndex[],
        MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(0)
    end
    push!(map.parent_index, map.current_context)
    bridge_index = Int64(length(map.parent_index))
    F = MOI.VectorOfVariables
    # Allocate a positive `CI{VectorOfVariables, S}` value for this vector of
    # constrained variables, skipping (with placeholders) the values that
    # `is_available` reports as already taken (by constraint bridges or by
    # the inner model). `vector_of_variables_map` stays dense, indexed by the
    # (positive) `ci.value`.
    while !is_available(
        MOI.ConstraintIndex{F,S}(length(map.vector_of_variables_map) + 1),
    )
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
    value = next_outer_variable!(map)
    push!(map.slot_to_variable, value)
    map.variable_to_slot[value] = bridge_index
    for i in 2:MOI.dimension(set)
        push!(map.parent_index, 0)
        push!(map.info, i)
        push!(map.index_in_vector, i)
        push!(map.bridges, nothing)
        push!(map.sets, nothing)
        push!(map.set_mask, 0x0000)
        value_i = next_outer_variable!(map)
        push!(map.slot_to_variable, value_i)
        map.variable_to_slot[value_i] = bridge_index + i - 1
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
    return variables, MOI.ConstraintIndex{F,S}(constraint_index)
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

Base.haskey(::EmptyMap, ::MOI.VariableIndex) = false

is_bridged_variable(::EmptyMap, ::MOI.VariableIndex) = false
