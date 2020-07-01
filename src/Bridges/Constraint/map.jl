"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged constraints and the bridge that bridged the constraint.
"""
struct Map <: AbstractDict{MOI.ConstraintIndex, AbstractBridge}
    # Constraint Index of bridged constraint -> Bridge.
    # It is set to `nothing` when the constraint is deleted.
    bridges::Vector{Union{Nothing, AbstractBridge}}
    # Constraint Index of bridged constraint -> Constraint type.
    constraint_types::Vector{Tuple{DataType, DataType}}
    # The order of the keys is used in `keys_of_type` which is used by
    # `ListOfConstraintIndices`. Therefore they need to be in the order
    # of creation so we need `OrderedDict` and not `Dict`.
    # For `SingleVariable` constraints: (variable, set type) -> bridge
    single_variable_constraints::OrderedDict{Tuple{Int64, DataType}, AbstractBridge}
    # For `VectorVariable` constraints: (variable, set type) -> bridge
    vector_of_variables_constraints::OrderedDict{Tuple{Int64, DataType}, AbstractBridge}
end
function Map()
    return Map(Union{Nothing, AbstractBridge}[],
               Tuple{DataType, DataType}[],
               OrderedDict{Tuple{Int64, DataType}, AbstractBridge}(),
               OrderedDict{Tuple{Int64, DataType}, AbstractBridge}())
end

# Implementation of `AbstractDict` interface.

function Base.isempty(map::Map)
    return all(bridge -> bridge === nothing, map.bridges) &&
        isempty(map.single_variable_constraints) &&
        isempty(map.vector_of_variables_constraints)
end
function Base.empty!(map::Map)
    empty!(map.bridges)
    empty!(map.constraint_types)
    empty!(map.single_variable_constraints)
    empty!(map.vector_of_variables_constraints)
    return map
end
function Base.length(map::Map)
    return count(bridge -> bridge !== nothing, map.bridges) +
        length(map.single_variable_constraints) +
        length(map.vector_of_variables_constraints)
end
function Base.haskey(map::Map, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    return 1 ≤ ci.value ≤ length(map.bridges) &&
        map.bridges[ci.value] !== nothing &&
        (F, S) == map.constraint_types[ci.value]
end
function Base.getindex(map::Map,
                       ci::MOI.ConstraintIndex)
    return map.bridges[ci.value]
end
function Base.haskey(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable, S}) where S
    return haskey(map.single_variable_constraints, (ci.value, S))
end
function Base.getindex(map::Map,
                       ci::MOI.ConstraintIndex{MOI.SingleVariable, S}) where S
    return map.single_variable_constraints[(ci.value, S)]
end
function Base.haskey(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables, S}) where S
    return haskey(map.vector_of_variables_constraints, (ci.value, S))
end
function Base.getindex(map::Map,
                       ci::MOI.ConstraintIndex{MOI.VectorOfVariables, S}) where S
    return map.vector_of_variables_constraints[(ci.value, S)]
end
function Base.delete!(map::Map, ci::MOI.ConstraintIndex)
    map.bridges[ci.value] = nothing
    return map
end
function Base.delete!(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable, S}) where S
    delete!(map.single_variable_constraints, (ci.value, S))
    return map
end
function Base.delete!(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables, S}) where S
    delete!(map.vector_of_variables_constraints, (ci.value, S))
    return map
end
function Base.values(map::Map)
    return Base.Iterators.flatten((
        # See comment in `values(::Variable.Map)`.
        Base.Iterators.Filter(bridge -> bridge !== nothing, map.bridges),
        values(map.single_variable_constraints),
        values(map.vector_of_variables_constraints)
    ))
end

# Implementation of iterate: it should combine non-variablewise constraints,
# `SingleVariable` constraints and `VectorOfVariables` constraints.
function _iterate_vov(map::Map, elem_state=iterate(map.vector_of_variables_constraints))
    if elem_state === nothing
        return nothing
    else
        i, S = elem_state[1].first
        bridge = elem_state[1].second
        ci = MOI.ConstraintIndex{MOI.VectorOfVariables, S}(i)
        return ci => bridge, (3, elem_state[2])
    end
end
function _iterate_sv(map::Map, elem_state=iterate(map.single_variable_constraints))
    if elem_state === nothing
        return _iterate_vov(map)
    else
        i, S = elem_state[1].first
        bridge = elem_state[1].second
        ci = MOI.ConstraintIndex{MOI.SingleVariable, S}(i)
        return ci => bridge, (2, elem_state[2])
    end
end
function _iterate(map::Map, state=1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return _iterate_sv(map)
    else
        F, S = map.constraint_types[state]
        return MOI.ConstraintIndex{F, S}(state) => map.bridges[state], (1, state + 1)
    end
end
Base.iterate(map::Map) = _iterate(map)
function Base.iterate(map::Map, state)
    if state[1] == 1
        return _iterate(map, state[2])
    elseif state[1] == 2
        return _iterate_sv(map, iterate(map.single_variable_constraints, state[2]))
    else
        return _iterate_vov(map, iterate(map.vector_of_variables_constraints, state[2]))
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

function number_of_type(map::Map, C::Type{MOI.ConstraintIndex{F, S}}) where {F, S}
    return count(i -> haskey(map, C(i)), eachindex(map.bridges))
end
function keys_of_type(map::Map, C::Type{MOI.ConstraintIndex{F, S}}) where {F, S}
    return Base.Iterators.Filter(
        ci -> haskey(map, ci),
        MOIU.LazyMap{C}(
            i -> C(i), eachindex(map.bridges)))
end
function number_of_type(map::Map, C::Type{MOI.ConstraintIndex{MOI.SingleVariable, S}}) where S
    return count(key -> key[2] == S, keys(map.single_variable_constraints))
end
function keys_of_type(map::Map, C::Type{MOI.ConstraintIndex{MOI.SingleVariable, S}}) where S
    return MOIU.LazyMap{C}(
        key -> C(key[1]),
        Base.Iterators.Filter(key -> key[2] == S, keys(map.single_variable_constraints))
    )
end
function number_of_type(map::Map, C::Type{MOI.ConstraintIndex{MOI.VectorOfVariables, S}}) where S
    return count(key -> key[2] == S, keys(map.vector_of_variables_constraints))
end
function keys_of_type(map::Map, C::Type{MOI.ConstraintIndex{MOI.VectorOfVariables, S}}) where S
    return MOIU.LazyMap{C}(
        key -> C(key[1]),
        Base.Iterators.Filter(key -> key[2] == S, keys(map.vector_of_variables_constraints))
    )
end

"""
    list_of_key_types(map::Map)

Return a list of all the different concrete type of keys in `map`.
"""
function list_of_key_types(map::Map)
    list = Set{Tuple{DataType, DataType}}()
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            push!(list, map.constraint_types[i])
        end
    end
    for key in keys(map.single_variable_constraints)
        push!(list, (MOI.SingleVariable, key[2]))
    end
    for key in keys(map.vector_of_variables_constraints)
        push!(list, (MOI.VectorOfVariables, key[2]))
    end
    return list
end

"""
    variable_constraints(map::Map, vi::MOI.VariableIndex)

Return the list of all keys corresponding to [`MathOptInterface.SingleVariable`](@ref)
constraints on the variable `vi`.
"""
function variable_constraints(map::Map, vi::MOI.VariableIndex)
    cis = MOI.ConstraintIndex{MOI.SingleVariable}[]
    for key in keys(map.single_variable_constraints)
        if key[1] == vi.value
            push!(cis, MOI.ConstraintIndex{MOI.SingleVariable, key[2]}(vi.value))
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
        key -> MOI.ConstraintIndex{MOI.VectorOfVariables, key[2]}(key[1]),
        keys(map.vector_of_variables_constraints)
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
    return !isempty(map.bridges) ||
        !isempty(map.single_variable_constraints) ||
        !isempty(map.vector_of_variables_constraints)
end



"""
    add_key_for_bridge(map::Map, bridge::AbstractBridge,
                       func::MOI.AbstractFunction, set::MOI.AbstractSet)

Return a new constraint index `ci` and stores the mapping `ci => bridge`.
"""
function add_key_for_bridge end

function add_key_for_bridge(map::Map, bridge::AbstractBridge,
                            func::MOI.AbstractFunction, set::MOI.AbstractSet)
    push!(map.bridges, bridge)
    push!(map.constraint_types, (typeof(func), typeof(set)))
    return MOI.ConstraintIndex{typeof(func), typeof(set)}(length(map.bridges))
end
function add_key_for_bridge(map::Map, bridge::AbstractBridge,
                            func::MOI.SingleVariable, set::MOI.AbstractScalarSet)
    map.single_variable_constraints[(func.variable.value, typeof(set))] = bridge
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(func.variable.value)
end
function add_key_for_bridge(map::Map, bridge::AbstractBridge,
                            func::MOI.VectorOfVariables, set::MOI.AbstractVectorSet)
    index = first(func.variables).value
    map.vector_of_variables_constraints[(index, typeof(set))] = bridge
    return MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(index)
end

"""
    EmptyMap <: AbstractDict{MOI.ConstraintIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MathOptInterface.Bridges.Variable.SingleBridgeOptimizer`](@ref) as it does
not bridge any constraint.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge} end
Base.isempty(::EmptyMap) = true
function Base.empty!(::EmptyMap) end
Base.keys(::EmptyMap) = MOIU.EmptyVector{MOI.VariableIndex}()
Base.values(::EmptyMap) = MOIU.EmptyVector{AbstractBridge}()
has_bridges(::EmptyMap) = false
number_of_type(::EmptyMap, ::Type{<:MOI.ConstraintIndex}) = 0
