"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

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
end

function Map()
    return Map(
        Union{Nothing,AbstractBridge}[],
        Tuple{Type,Type}[],
        OrderedDict{Tuple{Int64,Type},AbstractBridge}(),
    )
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
    return map
end

function Base.length(map::Map)
    return count(bridge -> bridge !== nothing, map.bridges) +
           length(map.single_variable_constraints)
end
_index(ci::MOI.ConstraintIndex) = ci.value
_index(ci::MOI.ConstraintIndex{MOI.VectorOfVariables}) = -ci.value
function Base.haskey(map::Map, ci::MOI.ConstraintIndex{F,S}) where {F,S}
    return 1 ≤ _index(ci) ≤ length(map.bridges) &&
           map.bridges[_index(ci)] !== nothing &&
           (F, S) == map.constraint_types[_index(ci)]
end

function Base.getindex(map::Map, ci::MOI.ConstraintIndex)
    return map.bridges[_index(ci)]
end

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
    map.bridges[_index(ci)] = nothing
    return map
end

function Base.delete!(
    map::Map,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
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

# Implementation of iterate: it should combine non-`VariableIndex` constraints and
# `VariableIndex` constraints.
function _iterate_sv(
    map::Map,
    elem_state = iterate(map.single_variable_constraints),
)
    if elem_state === nothing
        return nothing
    else
        i, S = elem_state[1].first
        bridge = elem_state[1].second
        ci = MOI.ConstraintIndex{MOI.VariableIndex,S}(i)
        return ci => bridge, (2, elem_state[2])
    end
end

function _index(index, F, S)
    return MOI.ConstraintIndex{F,S}(index)
end

function _index(index, F::Type{MOI.VectorOfVariables}, S)
    return MOI.ConstraintIndex{F,S}(-index)
end

function _iterate(map::Map, state = 1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return _iterate_sv(map)
    else
        F, S = map.constraint_types[state]
        return _index(state, F, S) => map.bridges[state], (1, state + 1)
    end
end
Base.iterate(map::Map) = _iterate(map)
function Base.iterate(map::Map, state)
    if state[1] == 1
        return _iterate(map, state[2])
    else
        return _iterate_sv(
            map,
            iterate(map.single_variable_constraints, state[2]),
        )
    end
end

# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    number_of_type(map::Map, C::Type{<:MOI.ConstraintIndex})

Return the number of keys of type `C` in `map`.
"""
function number_of_type end

"""
    keys_of_type(map::Map, C::Type{<:MOI.ConstraintIndex})

Return a list of all the keys of type `C` in `map` in order order in which they
were created with `add_key_for_bridge`.
"""
function keys_of_type end

function number_of_type(map::Map, ::Type{MOI.ConstraintIndex{F,S}}) where {F,S}
    return count(i -> haskey(map, _index(i, F, S)), eachindex(map.bridges))
end

function keys_of_type(map::Map, C::Type{MOI.ConstraintIndex{F,S}}) where {F,S}
    return Base.Iterators.Filter(
        ci -> haskey(map, ci),
        MOIU.LazyMap{C}(i -> _index(i, F, S), eachindex(map.bridges)),
    )
end

function number_of_type(
    map::Map,
    C::Type{MOI.ConstraintIndex{MOI.VariableIndex,S}},
) where {S}
    return count(key -> key[2] == S, keys(map.single_variable_constraints))
end

function keys_of_type(
    map::Map,
    C::Type{MOI.ConstraintIndex{MOI.VariableIndex,S}},
) where {S}
    return MOIU.LazyMap{C}(
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
    for key in keys(map.single_variable_constraints)
        push!(list, (MOI.VariableIndex, key[2]))
    end
    return list
end

"""
    variable_constraints(map::Map, vi::MOI.VariableIndex)

Return the list of all keys corresponding to [`MathOptInterface.VariableIndex`](@ref)
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
[`MathOptInterface.VectorOfVariables`](@ref) constraints.
"""
function vector_of_variables_constraints(map::Map)
    return MOIU.LazyMap{MOI.ConstraintIndex{MOI.VectorOfVariables}}(
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

Return a `Bool` indicating whether any bridge was added yet. Note that it
returns `false` even if all bridges were deleted while `isempty` would return
`true`. It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used
by [`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut
operations in case variable bridges are not used.
"""
function has_bridges(map::Map)
    return !isempty(map.bridges) || !isempty(map.single_variable_constraints)
end

"""
    add_key_for_bridge(map::Map, bridge::AbstractBridge,
                       func::MOI.AbstractFunction, set::MOI.AbstractSet)

Return a new constraint index `ci` and stores the mapping `ci => bridge`.
"""
function add_key_for_bridge end

function add_key_for_bridge(
    map::Map,
    bridge::AbstractBridge,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    push!(map.bridges, bridge)
    push!(map.constraint_types, (typeof(func), typeof(set)))
    return _index(length(map.bridges), typeof(func), typeof(set))
end

function add_key_for_bridge(
    map::Map,
    bridge::AbstractBridge,
    func::MOI.VariableIndex,
    set::MOI.AbstractScalarSet,
)
    map.single_variable_constraints[(func.value, typeof(set))] = bridge
    return MOI.ConstraintIndex{MOI.VariableIndex,typeof(set)}(
        func.value,
    )
end

"""
    EmptyMap <: AbstractDict{MOI.ConstraintIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MathOptInterface.Bridges.Variable.SingleBridgeOptimizer`](@ref) as it does
not bridge any constraint.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex,AbstractBridge} end
Base.isempty(::EmptyMap) = true
function Base.empty!(::EmptyMap) end
Base.keys(::EmptyMap) = MOIU.EmptyVector{MOI.VariableIndex}()
Base.values(::EmptyMap) = MOIU.EmptyVector{AbstractBridge}()
has_bridges(::EmptyMap) = false
number_of_type(::EmptyMap, ::Type{<:MOI.ConstraintIndex}) = 0
