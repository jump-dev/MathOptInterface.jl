# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Map <: AbstractDict{MOI.VariableIndex,AbstractBridge}

Mapping between bridged constraints and the bridge that bridged the constraint.
"""
struct Map <: AbstractDict{MOI.ConstraintIndex,AbstractBridge}
    # Constraint Index of bridged constraint -> Bridge.
    # It is set to `nothing` when the constraint is deleted.
    bridges::Vector{Union{Nothing,AbstractBridge}}
    # Constraint Index of bridged constraint -> Constraint type.
    constraint_types::Vector{Tuple{Type,Type}}
    # The order of the keys is used in `keys_of_type` which is used by
    # `ListOfConstraintIndices`. Therefore they need to be in the order
    # of creation so we need `OrderedDict` and not `Dict`.
    # For `VariableIndex` constraints: (variable, set type) -> bridge
    single_variable_constraints::OrderedDict{Tuple{Int64,Type},AbstractBridge}
    # The bridges that need a final touch, mapped to the "creation order" of the
    # constraint they bridge, that is, the order in which `bridge_constraint` was
    # called (see `_reserve_final_touch_order`). `final_touch` is called on them
    # in increasing creation order, so that a bridge gets its final touch before
    # the bridges of the constraints it added. See `final_touch(::Map, ...)`.
    needs_final_touch::OrderedDict{AbstractBridge,Int}
    # A counter incremented once per bridged constraint, used to assign the
    # creation order stored in `needs_final_touch`.
    final_touch_counter::Base.RefValue{Int}

    function Map()
        return new(
            Union{Nothing,AbstractBridge}[],
            Tuple{Type,Type}[],
            OrderedDict{Tuple{Int64,Type},AbstractBridge}(),
            OrderedDict{AbstractBridge,Int}(),
            Ref(0),
        )
    end
end

# Implementation of `AbstractDict` interface.

function Base.isempty(map::Map)
    return all(bridge -> bridge === nothing, map.bridges) &&
           isempty(map.single_variable_constraints)
end

function Base.empty!(map::Map)
    empty!(map.bridges)
    empty!(map.constraint_types)
    empty!(map.single_variable_constraints)
    empty!(map.needs_final_touch)
    map.final_touch_counter[] = 0
    return map
end

function Base.length(map::Map)
    return count(bridge -> bridge !== nothing, map.bridges) +
           length(map.single_variable_constraints)
end

_index(ci::MOI.ConstraintIndex) = ci.value

_index(ci::MOI.ConstraintIndex{MOI.VectorOfVariables}) = -ci.value

function Base.haskey(map::Map, ci::MOI.ConstraintIndex{F,S}) where {F,S}
    return 1 <= _index(ci) <= length(map.bridges) &&
           map.bridges[_index(ci)] !== nothing &&
           (F, S) == map.constraint_types[_index(ci)]
end

Base.getindex(map::Map, ci::MOI.ConstraintIndex) = map.bridges[_index(ci)]

function Base.haskey(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    return haskey(map.single_variable_constraints, (ci.value, S))
end

function Base.getindex(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    return map.single_variable_constraints[(ci.value, S)]
end

function Base.delete!(map::Map, ci::MOI.ConstraintIndex)
    _unregister_for_final_touch(map, map.bridges[_index(ci)]::AbstractBridge)
    map.bridges[_index(ci)] = nothing
    return map
end

function Base.delete!(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    bridge = map.single_variable_constraints[(ci.value, S)]
    _unregister_for_final_touch(map, bridge)
    delete!(map.single_variable_constraints, (ci.value, S))
    return map
end

function Base.values(map::Map)
    return Base.Iterators.flatten((
        # See comment in `values(::Variable.Map)`.
        Base.Iterators.Filter(bridge -> bridge !== nothing, map.bridges),
        values(map.single_variable_constraints),
    ))
end

# Implementation of iterate: it should combine non-`VariableIndex` constraints
# and `VariableIndex` constraints.
function _iterate_sv(
    map::Map,
    elem_state = iterate(map.single_variable_constraints),
)
    if elem_state === nothing
        return nothing
    end
    (i, S), bridge = elem_state[1]
    ci = MOI.ConstraintIndex{MOI.VariableIndex,S}(i)
    return ci => bridge, (2, elem_state[2])
end

_index(index, F, S) = MOI.ConstraintIndex{F,S}(index)

function _index(index, F::Type{MOI.VectorOfVariables}, S)
    return MOI.ConstraintIndex{F,S}(-index)
end

function _iterate(map::Map, state = 1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return _iterate_sv(map)
    end
    F, S = map.constraint_types[state]
    return _index(state, F, S) => map.bridges[state], (1, state + 1)
end

Base.iterate(map::Map) = _iterate(map)

function Base.iterate(map::Map, state)
    if state[1] == 1
        return _iterate(map, state[2])
    end
    return _iterate_sv(map, iterate(map.single_variable_constraints, state[2]))
end

# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    number_of_type(map::Map, C::Type{<:MOI.ConstraintIndex})

Return the number of keys of type `C` in `map`.
"""
function number_of_type(map::Map, ::Type{MOI.ConstraintIndex{F,S}}) where {F,S}
    return count(i -> haskey(map, _index(i, F, S)), eachindex(map.bridges))
end

function number_of_type(
    map::Map,
    ::Type{MOI.ConstraintIndex{MOI.VariableIndex,S}},
) where {S}
    ret = 0
    for (_, key) in keys(map.single_variable_constraints)
        ret += Int(key == S)
    end
    return ret
end

"""
    keys_of_type(map::Map, C::Type{<:MOI.ConstraintIndex})

Return a list of all the keys of type `C` in `map` in order in which they
were created with `add_key_for_bridge`.
"""
function keys_of_type(map::Map, C::Type{MOI.ConstraintIndex{F,S}}) where {F,S}
    return Base.Iterators.Filter(
        ci -> haskey(map, ci),
        MOI.Utilities.lazy_map(C, i -> _index(i, F, S), eachindex(map.bridges)),
    )
end

function keys_of_type(
    map::Map,
    C::Type{MOI.ConstraintIndex{MOI.VariableIndex,S}},
) where {S}
    return MOI.Utilities.lazy_map(
        C,
        key -> C(key[1]),
        Base.Iterators.Filter(
            key -> key[2] == S,
            keys(map.single_variable_constraints),
        ),
    )
end

"""
    list_of_key_types(map::Map)

Return a list of all the different concrete type of keys in `map`.
"""
function list_of_key_types(map::Map)
    list = Set{Tuple{Type,Type}}()
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            push!(list, map.constraint_types[i])
        end
    end
    for (_, key) in keys(map.single_variable_constraints)
        push!(list, (MOI.VariableIndex, key))
    end
    return list
end

"""
    variable_constraints(map::Map, vi::MOI.VariableIndex)

Return the list of all keys corresponding to [`MOI.VariableIndex`](@ref)
constraints on the variable `vi`.
"""
function variable_constraints(map::Map, vi::MOI.VariableIndex)
    cis = MOI.ConstraintIndex{MOI.VariableIndex}[]
    for key in keys(map.single_variable_constraints)
        if key[1] == vi.value
            push!(cis, MOI.ConstraintIndex{MOI.VariableIndex,key[2]}(vi.value))
        end
    end
    return cis
end

"""
    vector_of_variables_constraints(map::Map)

Return the list of all keys that correspond to
[`MOI.VectorOfVariables`](@ref) constraints.
"""
function vector_of_variables_constraints(map::Map)
    return MOI.Utilities.lazy_map(
        MOI.ConstraintIndex{MOI.VectorOfVariables},
        i -> MOI.ConstraintIndex{map.constraint_types[i]...}(-i),
        Base.Iterators.Filter(
            i ->
                map.bridges[i] !== nothing &&
                map.constraint_types[i][1] == MOI.VectorOfVariables,
            eachindex(map.bridges),
        ),
    )
end

"""
    has_bridges(map::Map)::Bool

Return a `Bool` indicating whether any bridge was added yet.

Note that it returns `false` even if all bridges were deleted while `isempty`
would return `true`.

It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used by
[`MOI.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut operations in case
variable bridges are not used.
"""
function has_bridges(map::Map)
    return !isempty(map.bridges) || !isempty(map.single_variable_constraints)
end

"""
    add_key_for_bridge(
        map::Map,
        bridge::AbstractBridge,
        func::MOI.AbstractFunction,
        set::MOI.AbstractSet,
        is_available::Function,
    )

Return a new constraint index `ci` and store the mapping `ci => bridge`.
If `func isa MOI.VectorOfVariables` then `ci` is such that `is_available(ci)`,
otherwise, `is_available` is ignored because there can only be a clash of
indices between variable and constraint bridge mapping for `MOI.VectorOfVariables`.
"""
function add_key_for_bridge end

_ensure_available(::Map, ::Type, ::Type, ::Function) = nothing
function _ensure_available(
    map::Map,
    F::Type{MOI.VectorOfVariables},
    ::Type{S},
    is_available::Function,
) where {S}
    while !is_available(MOI.ConstraintIndex{F,S}(-length(map.bridges) - 1))
        push!(map.bridges, nothing)
        push!(map.constraint_types, (F, S))
    end
    return
end

function add_key_for_bridge(
    map::Map,
    bridge::AbstractBridge,
    ::F,
    ::S,
    is_available::Function;
    final_touch_order::Int = _reserve_final_touch_order(map),
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    _register_for_final_touch(map, bridge, final_touch_order)
    _ensure_available(map, F, S, is_available)
    push!(map.bridges, bridge)
    push!(map.constraint_types, (F, S))
    return _index(length(map.bridges), F, S)
end

function add_key_for_bridge(
    map::Map,
    bridge::AbstractBridge,
    func::MOI.VariableIndex,
    ::S,
    ::Function;
    final_touch_order::Int = _reserve_final_touch_order(map),
) where {S<:MOI.AbstractScalarSet}
    _register_for_final_touch(map, bridge, final_touch_order)
    map.single_variable_constraints[(func.value, S)] = bridge
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(func.value)
end

"""
    _reserve_final_touch_order(map::Map)

Return the next "creation order" number. It must be called at the start of
`add_bridged_constraint`, that is, *before* `bridge_constraint`, so that a bridge
is given a smaller number than the bridges of the constraints it creates. The
number is passed to `add_key_for_bridge` as the `final_touch_order` keyword so
that `final_touch` is called on the bridges in that order.
"""
function _reserve_final_touch_order(map::Map)
    map.final_touch_counter[] += 1
    return map.final_touch_counter[]
end

function _register_for_final_touch(map::Map, bridge::AbstractBridge, order::Int)
    if MOI.Bridges.needs_final_touch(bridge)
        map.needs_final_touch[bridge] = order
    end
    return
end

function _unregister_for_final_touch(map::Map, bridge::AbstractBridge)
    if MOI.Bridges.needs_final_touch(bridge)
        delete!(map.needs_final_touch, bridge)
    end
    return
end

# Return the bridges that need a final touch, sorted by increasing creation
# order (the order in which their `bridge_constraint` was called).
function _sorted_needs_final_touch(map::Map)
    bridges = collect(keys(map.needs_final_touch))
    sort!(bridges; by = bridge -> map.needs_final_touch[bridge])
    return bridges
end

function MOI.Bridges.final_touch(map::Map, model::MOI.ModelLike)
    # `MOI.Bridges.final_touch(bridge, model)` may add constraints that are
    # bridged by bridges that themselves need a final touch. These new bridges
    # are added to `map.needs_final_touch` and must also get their `final_touch`
    # called (see issue #1980), after the bridge that created them: since they
    # are created later, they have a larger creation order, so they are sorted
    # after the current position. We cannot iterate over `map.needs_final_touch`
    # directly because it may be modified during the iteration, so we snapshot
    # it into a sorted vector and refresh the snapshot whenever new bridges are
    # added. The already-touched prefix keeps a smaller creation order, so it is
    # left unchanged by the refresh and `i` stays valid.
    bridges = _sorted_needs_final_touch(map)
    i = 1
    while i <= length(bridges)
        MOI.Bridges.final_touch(bridges[i], model)
        if length(bridges) < length(map.needs_final_touch)
            bridges = _sorted_needs_final_touch(map)
        end
        i += 1
    end
    return
end

"""
    EmptyMap <: AbstractDict{MOI.ConstraintIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MOI.Bridges.Variable.SingleBridgeOptimizer`](@ref) as it does
not bridge any constraint.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex,AbstractBridge} end

Base.show(::IO, ::EmptyMap) = nothing

Base.isempty(::EmptyMap) = true

Base.empty!(::EmptyMap) = nothing

Base.keys(::EmptyMap) = MOI.Utilities.EmptyVector{MOI.VariableIndex}()

Base.values(::EmptyMap) = MOI.Utilities.EmptyVector{AbstractBridge}()

has_bridges(::EmptyMap) = false

number_of_type(::EmptyMap, ::Type{<:MOI.ConstraintIndex}) = 0

MOI.Bridges.final_touch(::EmptyMap, ::MOI.ModelLike) = nothing
