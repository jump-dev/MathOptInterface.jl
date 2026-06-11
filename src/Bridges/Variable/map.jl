# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged variables and the bridge that bridged the variable.

# Internal structure

`vi.value`s of bridged variables are allocated via
[`MOI.Bridges.reserve_variable_index`](@ref) so that they don't collide
with the inner model's namespace. They are not contiguous integers.

Internally, the `Map` allocates 1-based `slot` indices in insertion order
and uses them to index `Vector`-backed data (`bridges`, `info`,
`index_in_vector`, `sets`, `parent_index`, `set_mask`,
`slot_to_variable`). The Dict `variable_to_slot` maps the user-facing
`vi.value` to the internal `slot`.

For a vector of bridged variables of dimension `n`, the n variables get
consecutive `slot`s (the first slot is where the bridge is stored). The
`vi.value`s do not need to be consecutive.

The semantics of `info[slot]`:

  * `0`: `slot_to_variable[slot]` is a scalar variable added with
    `add_constrained_variable`.
  * `-x` with `x > 0`: `slot_to_variable[slot]` is the first variable of a
    vector added with `add_constrained_variables`, whose
    `ConstraintIndex{MOI.VectorOfVariables, S}.value` is `x`.
  * `k > 0`: `slot_to_variable[slot]` is the `k`-th variable of a vector
    (whose first-variable `slot` is `slot - k + 1`).

The semantics of `index_in_vector[slot]`:

  * `-1`: variable was deleted
  * `0`: scalar variable
  * `j > 0`: `j`-th variable of a vector (taking deletion into account)
"""
mutable struct Map <: AbstractDict{MOI.VariableIndex,AbstractBridge}
    # Forward mapping: user-facing vi.value -> internal slot
    variable_to_slot::Dict{Int64,Int64}
    # Reverse mapping: internal slot -> user-facing vi.value
    slot_to_variable::Vector{Int64}
    # (S, ConstraintIndex{VectorOfVariables, S}.value) -> first slot of that vector
    # Keyed per-S because reservation from the inner model can return the same
    # `.value` for different `S` types (each `(F, S)` has its own namespace).
    vov_to_slot::Dict{Tuple{Type,Int64},Int64}
    # (S, ci.value) -> dimension of the set
    vov_length::Dict{Tuple{Type,Int64},Int64}
    # See docstring above
    info::Vector{Int64}
    index_in_vector::Vector{Int64}
    bridges::Vector{Union{Nothing,AbstractBridge}}
    sets::Vector{Union{Nothing,Type}}
    # If `nothing`, it cannot be computed because some bridges does not support it
    unbridged_function::Union{
        Nothing,
        Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}},
    }
    # Parent context (slot) of the bridge that created this slot, 0 if root
    parent_index::Vector{Int64}
    # Current bridge, 0 otherwise.
    current_context::Int64
    # Context of constraint bridged by constraint bridges
    constraint_context::Dict{MOI.ConstraintIndex,Int64}
    # Same as in `MOI.Utilities.VariablesContainer`
    set_mask::Vector{UInt16}
end

function Map()
    return Map(
        Dict{Int64,Int64}(),
        Int64[],
        Dict{Tuple{Type,Int64},Int64}(),
        Dict{Tuple{Type,Int64},Int64}(),
        Int64[],
        Int64[],
        Union{Nothing,AbstractBridge}[],
        Union{Nothing,Type}[],
        Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}}(),
        Int64[],
        0,
        Dict{MOI.ConstraintIndex,Int64}(),
        UInt16[],
    )
end

# Implementation of `AbstractDict` interface.

Base.isempty(map::Map) = all(bridge -> bridge === nothing, map.bridges)

function Base.empty!(map::Map)
    empty!(map.variable_to_slot)
    empty!(map.slot_to_variable)
    empty!(map.vov_to_slot)
    empty!(map.vov_length)
    empty!(map.info)
    empty!(map.index_in_vector)
    empty!(map.bridges)
    empty!(map.sets)
    empty!(map.parent_index)
    map.current_context = 0
    empty!(map.constraint_context)
    if map.unbridged_function === nothing
        map.unbridged_function =
            Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}}()
    else
        empty!(something(map.unbridged_function))
    end
    empty!(map.set_mask)
    return map
end

"""
    bridge_index(map::Map, vi::MOI.VariableIndex)

Return the internal slot that stores the bridge for the variable bridge that
created `vi` (the first slot of the vector if `vi` is in a vector).
"""
function bridge_index(map::Map, vi::MOI.VariableIndex)
    slot = map.variable_to_slot[vi.value]
    return _first_slot(map, slot)
end

# Given any slot, return the first-slot of the (possibly vector) bridge that owns it
function _first_slot(map::Map, slot::Integer)
    info = map.info[slot]
    if info <= 0
        return Int64(slot)
    else
        return Int64(slot) - info + 1
    end
end

function Base.haskey(map::Map, vi::MOI.VariableIndex)
    slot = get(map.variable_to_slot, vi.value, 0)
    if slot == 0
        return false
    end
    first_s = _first_slot(map, slot)
    return map.bridges[first_s] !== nothing && map.index_in_vector[slot] != -1
end

function Base.getindex(map::Map, vi::MOI.VariableIndex)
    return map.bridges[bridge_index(map, vi)]
end

function Base.delete!(map::Map, vi::MOI.VariableIndex)
    slot = map.variable_to_slot[vi.value]
    first_s = _first_slot(map, slot)
    info_first = map.info[first_s]
    if iszero(info_first)
        # Delete scalar variable
        map.bridges[first_s] = nothing
        map.sets[first_s] = nothing
    elseif has_keys(map, [vi])
        # Delete whole vector
        delete!(map, [vi])
    else
        # Delete a single variable in the vector and shift positions
        S = map.sets[first_s]::Type
        vov_ci_value = -info_first
        map.vov_length[(S, vov_ci_value)] -= 1
        # Walk through subsequent slots in the same vector and decrement positions
        for s in slot:length(map.index_in_vector)
            if map.index_in_vector[s] == -1
                continue
            end
            if _first_slot(map, s) != first_s
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
        slot = map.variable_to_slot[vi.value]
        map.set_mask[slot] = MOI.Utilities._DELETED_VARIABLE
        map.index_in_vector[slot] = -1
    end
    first_s = bridge_index(map, vis[1])
    map.bridges[first_s] = nothing
    map.sets[first_s] = nothing
    return map
end

function Base.keys(map::Map)
    return Base.Iterators.Filter(
        vi -> haskey(map, vi),
        MOI.Utilities.lazy_map(
            MOI.VariableIndex,
            i -> MOI.VariableIndex(map.slot_to_variable[i]),
            eachindex(map.bridges),
        ),
    )
end

Base.length(map::Map) = count(bridge -> bridge !== nothing, map.bridges)

function number_of_variables(map::Map)
    num = 0
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            if iszero(map.info[i])
                num += 1
            else
                vi = MOI.VariableIndex(map.slot_to_variable[i])
                num += length_of_vector_of_variables(map, vi)
            end
        end
    end
    return num
end

function Base.values(map::Map)
    # We don't use `filter` as it would compute the resulting array which
    # is not necessary if the caller just wants to iterate over `values`.
    return Base.Iterators.Filter(bridge -> bridge !== nothing, map.bridges)
end

function Base.iterate(map::Map, state = 1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return nothing
    else
        vi = MOI.VariableIndex(map.slot_to_variable[state])
        return vi => map.bridges[state], state + 1
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
    first_variable(::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})

Return the first `MOI.VariableIndex` of the `MOI.ConstraintFunction` of `ci`.
"""
function first_variable(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    return MOI.VariableIndex(map.slot_to_variable[map.vov_to_slot[(S, ci.value)]])
end

function constraint(map::Map, vi::MOI.VariableIndex)
    S = constrained_set(map, vi)::Type{<:MOI.AbstractSet}
    F = MOI.Utilities.variable_function_type(S)
    first_s = bridge_index(map, vi)
    info_first = map.info[first_s]
    if iszero(info_first)
        # Scalar: ci.value == vi.value (by MOI convention)
        return MOI.ConstraintIndex{F,S}(map.slot_to_variable[first_s])
    else
        # Vector: ci.value stored as -info_first
        return MOI.ConstraintIndex{F,S}(-info_first)
    end
end

function MOI.is_valid(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    first_slot = get(map.vov_to_slot, (S, ci.value), 0)
    first_slot == 0 && return false
    return !isnothing(map.bridges[first_slot]) && map.sets[first_slot] === S
end

function MOI.is_valid(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    slot = get(map.variable_to_slot, ci.value, 0)
    slot == 0 && return false
    return !isnothing(map.bridges[slot]) && map.sets[slot] === S
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
    slot = map.variable_to_slot[vi.value]
    mask = map.set_mask[slot]
    MOI.Utilities._throw_if_lower_bound_set(vi, S, mask, T)
    MOI.Utilities._throw_if_upper_bound_set(vi, S, mask, T)
    map.set_mask[slot] = mask | flag
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
    slot = map.variable_to_slot[ci.value]
    map.set_mask[slot] &= ~flag
    return
end

"""
    constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the list of constraints corresponding to bridged variables in `S`.
"""
function constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    F = MOI.Utilities.variable_function_type(S)
    return MOI.ConstraintIndex{F,S}[
        constraint(map, MOI.VariableIndex(map.slot_to_variable[i])) for
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
    if isempty(vis)
        return true
    end
    head = vis[1]
    if !haskey(map, head)
        return false
    end
    n = length_of_vector_of_variables(map, head)
    if n != length(vis)
        return false
    end
    head_slot = bridge_index(map, head)
    for (k, vi) in enumerate(vis)
        slot = get(map.variable_to_slot, vi.value, 0)
        if slot != head_slot + k - 1
            return false
        end
        if !haskey(map, vi)
            return false
        end
    end
    return true
end

"""
    length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)

If `vi` was bridged in a scalar set, it returns 0. Otherwise, it
returns the dimension of the set.
"""
function length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    first_s = bridge_index(map, vi)
    info = map.info[first_s]
    if iszero(info)
        return 0
    else
        S = map.sets[first_s]::Type
        return map.vov_length[(S, -info)]
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
    slot = map.variable_to_slot[vi.value]
    return MOI.Bridges.IndexInVector(map.index_in_vector[slot])
end

"""
    has_bridges(map::Map)::Bool

Return a `Bool` indicating whether any bridge was added yet. Note that it
returns `false` even if all bridges were deleted while `isempty` would return
`true`. It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used
by [`MOI.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut
operations in case variable bridges are not used.
"""
has_bridges(map::Map) = !isempty(map.bridges)

"""
    add_key_for_bridge(
        map::Map,
        bridge_fun::Function,
        set::MOI.AbstractScalarSet,
        variable::MOI.VariableIndex,
    )

Register `variable` (whose `.value` must already be reserved from the inner
model via [`MOI.Bridges.reserve_variable_index`](@ref)) as a bridged variable
in `map`, store the mapping `variable => bridge_fun()` and associate
`variable` to `typeof(set)`. Returns a tuple with `variable` and the
constraint index
`MOI.ConstraintIndex{MOI.VariableIndex, typeof(set)}(variable.value)`.
"""
function add_key_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::MOI.AbstractScalarSet,
    variable::MOI.VariableIndex,
)
    push!(map.parent_index, map.current_context)
    slot = Int64(length(map.parent_index))
    push!(map.info, 0)
    push!(map.index_in_vector, 0)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    push!(map.set_mask, 0x0000)
    push!(map.slot_to_variable, variable.value)
    map.variable_to_slot[variable.value] = slot
    map.bridges[slot] = call_in_context(map, slot, bridge_fun)
    if map.unbridged_function !== nothing
        mappings = unbridged_map(something(map.bridges[slot]), variable)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    something(map.unbridged_function),
                    mapping.first => (slot, mapping.second),
                )
            end
        end
    end
    MOI.add_constraint(map, variable, set)
    return variable, MOI.ConstraintIndex{MOI.VariableIndex,typeof(set)}(variable.value)
end

"""
    add_keys_for_bridge(
        map::Map,
        bridge_fun::Function,
        set::MOI.AbstractVectorSet,
        variables::Vector{MOI.VariableIndex},
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    )

Register `variables` (whose `.value`s must already be reserved from the inner
model via [`MOI.Bridges.reserve_variable_index`](@ref)) as bridged variables
in `map`, store the mapping `vi => bridge_fun()` for each `vi ∈ variables`
and associate them to `typeof(set)`. `ci` must also have been reserved via
[`MOI.Bridges.reserve_constraint_index`](@ref).
"""
function add_keys_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::S,
    variables::Vector{MOI.VariableIndex},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S<:MOI.AbstractVectorSet}
    if isempty(variables)
        return variables, ci
    end
    dim = length(variables)
    @assert dim == MOI.dimension(set)
    # If `ci.value` is 0, the caller signals that no reservation was made
    # (the inner model could not produce a colliding CI{VOV, S}). Allocate
    # the next available local positive value for this `S`.
    ci_value = if iszero(ci.value)
        _next_local_vov_value(map, S)
    else
        ci.value
    end
    push!(map.parent_index, map.current_context)
    first_slot = Int64(length(map.parent_index))
    map.vov_to_slot[(S, ci_value)] = first_slot
    map.vov_length[(S, ci_value)] = dim
    push!(map.info, -ci_value)
    push!(map.index_in_vector, 1)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    push!(map.set_mask, 0x0000)
    push!(map.slot_to_variable, variables[1].value)
    map.variable_to_slot[variables[1].value] = first_slot
    for i in 2:dim
        push!(map.parent_index, 0)
        push!(map.info, i)
        push!(map.index_in_vector, i)
        push!(map.bridges, nothing)
        push!(map.sets, nothing)
        push!(map.set_mask, 0x0000)
        push!(map.slot_to_variable, variables[i].value)
        map.variable_to_slot[variables[i].value] = first_slot + i - 1
    end
    map.bridges[first_slot] = call_in_context(map, first_slot, bridge_fun)
    if map.unbridged_function !== nothing
        mappings = unbridged_map(something(map.bridges[first_slot]), variables)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    something(map.unbridged_function),
                    mapping.first => (first_slot, mapping.second),
                )
            end
        end
    end
    return variables, MOI.ConstraintIndex{MOI.VectorOfVariables,S}(ci_value)
end

"""
    _next_local_vov_value(map::Map, S::Type)

Return the next positive integer that, combined with `S`, is not already a
key in `map.vov_to_slot`. Used by [`add_keys_for_bridge`](@ref) when the
caller did not reserve a `CI{VOV, S}` value because the inner model
cannot produce a colliding one.
"""
function _next_local_vov_value(map::Map, S::Type)
    val = 1
    while haskey(map.vov_to_slot, (S, val))
        val += 1
    end
    return val
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
function function_for(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    first_slot = map.vov_to_slot[(S, ci.value)]
    dim = map.vov_length[(S, ci.value)]
    variables = MOI.VariableIndex[]
    for s in first_slot:(first_slot + dim - 1)
        if s > length(map.index_in_vector) || map.index_in_vector[s] == -1
            continue
        end
        if _first_slot(map, s) != first_slot
            break
        end
        push!(variables, MOI.VariableIndex(map.slot_to_variable[s]))
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
    bridge_slot, func = context_func
    # If the bridge bridging `vi` has slot `bridge_slot` or directly or
    # indirectly created this bridge then we don't unbridge the variable.
    context = map.current_context
    while !iszero(context)
        if bridge_slot == context
            return nothing
        end
        context = map.parent_index[context]
    end
    return func
end

"""
    call_in_context(map::Map, bridge_slot::Int64, f::Function)

Call function `f` in the context of the variable bridge of slot
`bridge_slot`. That is, the variable indices bridged by this bridge or the
bridges that created it will not be unbridged in
[`unbridged_function`](@ref).
"""
function call_in_context(map::Map, bridge_slot::Int64, f::Function)
    # This is a shortcut that is used in particular in the common case where
    # no variable bridge is used.
    if iszero(bridge_slot) && iszero(map.current_context)
        return f()
    end
    previous_context = map.current_context
    map.current_context = bridge_slot
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

Shortcut for `call_in_context(map, bridge_slot, () -> f(bridge))` where
`vi` is bridged by `bridge` with slot `bridge_slot`.
"""
function call_in_context(map::Map, vi::MOI.VariableIndex, f::Function)
    slot = bridge_index(map, vi)
    return call_in_context(map, slot, () -> f(map.bridges[slot]))
end

"""
    call_in_context(map::Map, ci::MOI.ConstraintIndex, f::Function)

Shortcut for `call_in_context(map, bridge_slot, f)` where `bridge_slot` is the
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
