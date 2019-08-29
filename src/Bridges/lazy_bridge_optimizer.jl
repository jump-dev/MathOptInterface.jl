"""
    LazyBridgeOptimizer{OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `LazyBridgeOptimizer` combines several bridges, which are added using the [`add_bridge`](@ref) function.
Whenever a constraint is added, it only attempts to bridge it if it is not supported by the internal model (hence its name `Lazy`).
When bridging a constraint, it selects the minimal number of bridges needed.
For instance, a constraint `F`-in-`S` can be bridged into a constraint `F1`-in-`S1` (supported by the internal model) using bridge 1 or
bridged into a constraint `F2`-in-`S2` (unsupported by the internal model) using bridge 2 which can then be
bridged into a constraint `F3`-in-`S3` (supported by the internal model) using bridge 3,
it will choose bridge 1 as it allows to bridge `F`-in-`S` using only one bridge instead of two if it uses bridge 2 and 3.
"""
mutable struct LazyBridgeOptimizer{OT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    # Internal model
    model::OT
    # Bridged variables
    variable_map::Variable.Map
    var_to_name::Dict{MOI.VariableIndex, String}
    name_to_var::Union{Dict{String, MOI.VariableIndex}, Nothing}
    # Bridged constraints
    constraint_map::Constraint.Map
    con_to_name::Dict{MOI.ConstraintIndex, String}
    name_to_con::Union{Dict{String, MOI.ConstraintIndex}, Nothing}
    # Bridged objective
    objective_map::Objective.Map
    # Bellman-Ford
    # List of types of available bridges
    variable_bridge_types::Vector{Any}
    # (S,) -> Number of bridges that need to be used for constrained variables in `S`
    variable_dist::Dict{Tuple{DataType}, Int}
    # (S,) -> Bridge to be used for constrained variables in `S`
    variable_best::Dict{Tuple{DataType}, DataType}
    # List of types of available bridges
    constraint_bridge_types::Vector{Any}
    # (F, S) -> Number of bridges that need to be used for an `F`-in-`S` constraint
    constraint_dist::Dict{Tuple{DataType, DataType}, Int}
    # (F, S) -> Bridge to be used for an `F`-in-`S` constraint
    constraint_best::Dict{Tuple{DataType, DataType}, DataType}
    # List of types of available bridges
    objective_bridge_types::Vector{Any}
    # (F, S) -> Number of bridges that need to be used for an `F`-in-`S` constraint
    objective_dist::Dict{Tuple{DataType}, Int}
    # (F, S) -> Bridge to be used for an `F`-in-`S` constraint
    objective_best::Dict{Tuple{DataType}, DataType}
end
function LazyBridgeOptimizer(model::MOI.ModelLike)
    return LazyBridgeOptimizer{typeof(model)}(
        model,
        Variable.Map(), Dict{MOI.VariableIndex, String}(), nothing,
        Constraint.Map(), Dict{MOI.ConstraintIndex, String}(), nothing,
        Objective.Map(),
        Any[], Dict{Tuple{DataType}, Int}(),
        Dict{Tuple{DataType}, DataType}(),
        Any[], Dict{Tuple{DataType, DataType}, Int}(),
        Dict{Tuple{DataType, DataType}, DataType}(),
        Any[], Dict{Tuple{DataType}, Int}(),
        Dict{Tuple{DataType}, DataType}())
end

function Variable.bridges(bridge::LazyBridgeOptimizer)
    return bridge.variable_map
end
function Constraint.bridges(bridge::LazyBridgeOptimizer)
    return bridge.constraint_map
end
function Objective.bridges(b::LazyBridgeOptimizer)
    return b.objective_map
end

function _dist(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    F = MOIU.variable_function_type(S)
    if MOI.supports_constraint(b.model, F, S)
        return 0
    else
        return min(get(b.variable_dist, (S,), typemax(Int)),
                   get(b.constraint_dist, (F, S), typemax(Int) - 1) + 1)
    end
end

function _dist(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if MOI.supports_constraint(b.model, F, S)
        return 0
    else
        return get(b.constraint_dist, (F, S), typemax(Int))
    end
end

function _dist(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    if MOI.supports(b.model, MOI.ObjectiveFunction{F}())
        return 0
    else
        return get(b.objective_dist, (F,), typemax(Int))
    end
end

function _added_dist(b::LazyBridgeOptimizer, args...)
    dist = mapreduce(C -> _dist(b, C[1]), +,
                     added_constrained_variable_types(args...), init = 0)
    dist += mapreduce(C -> _dist(b, C[1], C[2]), +,
                      added_constraint_types(args...), init = 0)
    return dist
end
function added_dist(b::LazyBridgeOptimizer,
                    BT::Type{<:Union{Variable.AbstractBridge,
                                     Constraint.AbstractBridge}},
                    args...)
    return _added_dist(b, BT, args...)
end
function added_dist(b::LazyBridgeOptimizer,
                    BT::Type{<:Objective.AbstractBridge},
                    args...)
    F = set_objective_function_type(BT, args...)
    return _added_dist(b, BT, args...) + _dist(b, F)
end

function _supports_added_no_update(b::LazyBridgeOptimizer, args...)
    return all(C -> supports_no_update(b, C[1]), added_constrained_variable_types(args...)) &&
        all(C -> supports_no_update(b, C[1], C[2]), added_constraint_types(args...))
end
function supports_added_no_update(
    b::LazyBridgeOptimizer,
    BT::Type{<:Union{Variable.AbstractBridge,
                     Constraint.AbstractBridge}},
    args...
)
    return _supports_added_no_update(b, BT, args...)
end
function supports_added_no_update(
    b::LazyBridgeOptimizer,
    BT::Type{<:Objective.AbstractBridge},
    args...
)
    F = set_objective_function_type(BT, args...)
    return _supports_added_no_update(b, BT, args...) &&
        supports_no_update(b, F)
end


# Update `b.variable_dist`, `b.constraint_dist` `b.variable_best` and
# `b.constraint_best` for constrained variable types in `variables` and
# constraint types in `constraints`.
function update_dist!(b::LazyBridgeOptimizer, variables, constraints, objectives)
    # Bellman-Ford algorithm
    changed = true # Has b.constraint_dist changed in the last iteration ?
    while changed
        changed = false
        for BT in b.variable_bridge_types
            for (S,) in variables
                if Variable.supports_constrained_variable(BT, S) &&
                    supports_added_no_update(b, BT, S)
                    # Number of bridges needed using BT
                    dist = 1 + added_dist(b, BT, S)
                    # Is it better that what can currently be done ?
                    if dist < _dist(b, S)
                        b.variable_dist[(S,)] = dist
                        b.variable_best[(S,)] = Variable.concrete_bridge_type(BT, S)
                        changed = true
                    end
                end
            end
        end
        for BT in b.constraint_bridge_types
            for (F, S) in constraints
                if MOI.supports_constraint(BT, F, S) &&
                    supports_added_no_update(b, BT, F, S)
                    # Number of bridges needed using BT
                    dist = 1 + added_dist(b, BT, F, S)
                    # Is it better that what can currently be done ?
                    if dist < _dist(b, F, S)
                        b.constraint_dist[(F, S)] = dist
                        b.constraint_best[(F, S)] = Constraint.concrete_bridge_type(BT, F, S)
                        changed = true
                    end
                end
            end
        end
        for BT in b.objective_bridge_types
            for (F,) in objectives
                if Objective.supports_objective_function(BT, F) &&
                    supports_added_no_update(b, BT, F)
                    # Number of bridges needed using BT
                    dist = 1 + added_dist(b, BT, F)
                    # Is it better that what can currently be done ?
                    if dist < _dist(b, F)
                        b.objective_dist[(F,)] = dist
                        b.objective_best[(F,)] = Objective.concrete_bridge_type(BT, F)
                        changed = true
                    end
                end
            end
        end
    end
end

function fill_required!(required_variables::Set{Tuple{DataType}},
                        required_constraints::Set{Tuple{DataType, DataType}},
                        required_objectives::Set{Tuple{DataType}},
                        b::LazyBridgeOptimizer,
                        BT::Type{<:AbstractBridge})
    for C in added_constrained_variable_types(BT)
        fill_required!(required_variables, required_constraints,
                       required_objectives, b, C[1])
        F = MOIU.variable_function_type(C[1])
        fill_required!(required_variables, required_constraints,
                       required_objectives, b, F, C[1])
    end
    for C in added_constraint_types(BT)
        fill_required!(required_variables, required_constraints,
                       required_objectives, b, C[1], C[2])
    end
    if BT <: Objective.AbstractBridge
        fill_required!(required_variables, required_constraints, required_objectives, b,
                       set_objective_function_type(BT))
    end
end

function fill_required!(required_variables::Set{Tuple{DataType}},
                        required_constraints::Set{Tuple{DataType, DataType}},
                        required_objectives::Set{Tuple{DataType}},
                        b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    if supports_no_update(b, S)
        return # The constraint is supported
    end
    if (S,) in required_variables
        return # The requirements for these constrained variables have already been added or are being added
    end
    # The constrained variables are not supported yet, add in
    # `required_variables` the required variables types to bridge it.
    push!(required_variables, (S,))
    for BT in b.variable_bridge_types
        if Variable.supports_constrained_variable(BT, S)
            fill_required!(required_variables, required_constraints,
                           required_objectives, b,
                           Variable.concrete_bridge_type(BT, S))
        end
    end
end

function fill_required!(required_variables::Set{Tuple{DataType}},
                        required_constraints::Set{Tuple{DataType, DataType}},
                        required_objectives::Set{Tuple{DataType}},
                        b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
                        S::Type{<:MOI.AbstractSet})
    if supports_no_update(b, F, S)
        return # The constraint is supported
    end
    if (F, S) in required_constraints
        return # The requirements for this constraint have already been added or are being added
    end
    # The constraint is not supported yet, add
    # * in `required_variables` the required constrained variables types and
    # * in `required_constraints` the required constraint types
    # to bridge it.
    push!(required_constraints, (F, S))
    for BT in b.constraint_bridge_types
        if MOI.supports_constraint(BT, F, S)
            fill_required!(required_variables, required_constraints,
                           required_objectives, b,
                           Constraint.concrete_bridge_type(BT, F, S))
        end
    end
end

function fill_required!(required_variables::Set{Tuple{DataType}},
                        required_constraints::Set{Tuple{DataType, DataType}},
                        required_objectives::Set{Tuple{DataType}},
                        b::LazyBridgeOptimizer,
                        F::Type{<:MOI.AbstractScalarFunction})
    if supports_no_update(b, F)
        return # The objective is supported
    end
    if (F,) in required_objectives
        return # The requirements for this objective have already been added or are being added
    end
    # The objective is not supported yet, add
    # * in `required_variables` the required constrained variables types,
    # * in `required_constraints` the required constraint types and
    # * in `required_objectives` the required objective types
    # to bridge it.
    push!(required_objectives, (F,))
    for BT in b.objective_bridge_types
        if Objective.supports_objective_function(BT, F)
            fill_required!(required_variables, required_constraints,
                           required_objectives, b,
                           Objective.concrete_bridge_type(BT, F))
        end
    end
end

function required(b::LazyBridgeOptimizer, types::Tuple)
    required_variables = Set{Tuple{DataType}}()
    required_constraints = Set{Tuple{DataType, DataType}}()
    required_objectives = Set{Tuple{DataType}}()
    fill_required!(required_variables, required_constraints,
                   required_objectives, b, types...)
    return required_variables, required_constraints, required_objectives
end

# Compute dist[(F, S)], dist[(S,)] and best[(F, S)], best[(S,)]
function update!(b::LazyBridgeOptimizer, types::Tuple)
    update_dist!(b, required(b, types)...)
end

# After `add_bridge(b, BT)`, some constrained variables `(S,)` in
# `keys(b.variable_best)` or constraints `(F, S)` in `keys(b.constraint_best)`
# or `(F,)` in `keys(b.objective_best)` may be bridged
# with less bridges than `b.variable_dist[(S,)]`,
# `b.constraint_dist[(F, S)]` or `b.objective_dist[(F,)]` using `BT`.
function _update_key_dists!(b)
    # TODO we should call `fill_required!`.
    update_dist!(b, keys(b.variable_best), keys(b.constraint_best), keys(b.objective_best))
end

"""
    add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Variable.AbstractBridge})

Enable the use of the variable bridges of type `BT` by `b`.
"""
function add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Variable.AbstractBridge})
    push!(b.variable_bridge_types, BT)
    update_dist!(b, keys(b.variable_best), keys(b.constraint_best), keys(b.objective_best))
end

"""
    add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Constraint.AbstractBridge})

Enable the use of the constraint bridges of type `BT` by `b`.
"""
function add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Constraint.AbstractBridge})
    push!(b.constraint_bridge_types, BT)
    update_dist!(b, keys(b.variable_best), keys(b.constraint_best), keys(b.objective_best))
end

"""
    add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Objective.AbstractBridge})

Enable the use of the objective bridges of type `BT` by `b`.
"""
function add_bridge(b::LazyBridgeOptimizer, BT::Type{<:Objective.AbstractBridge})
    push!(b.objective_bridge_types, BT)
    update_dist!(b, keys(b.variable_best), keys(b.constraint_best), keys(b.objective_best))
end

function _remove_bridge(bridge_types::Vector, BT::Type)
    i = findfirst(isequal(BT), bridge_types)
    if i === nothing
        error("Cannot remove bridge `BT` as it was never added or was already",
              " removed.")
    else
        deleteat!(bridge_types, i)
    end
    return
end
function _reset_dist(b::LazyBridgeOptimizer)
    empty!(b.variable_dist)
    empty!(b.variable_best)
    empty!(b.constraint_dist)
    empty!(b.constraint_best)
end

"""
    remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:Variable.AbstractBridge})

Disable the use of the variable bridges of type `BT` by `b`.
"""
function remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:Variable.AbstractBridge})
    _remove_bridge(b.variable_bridge_types, BT)
    _reset_dist(b)
end

"""
    remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:Constraint.AbstractBridge})

Disable the use of the constraint bridges of type `BT` by `b`.
"""
function remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:Constraint.AbstractBridge})
    _remove_bridge(b.constraint_bridge_types, BT)
    _reset_dist(b)
end


# It only bridges when the constraint is not supporting, hence the name "Lazy"
function is_bridged(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return !MOI.supports_constraint(b.model, MOIU.variable_function_type(S), S)
end
function is_bridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    return !MOI.supports_constraint(b.model, F, S)
end
function is_bridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    return !MOI.supports(b.model, MOI.ObjectiveFunction{F}())
end
# Same as supports_constraint but do not trigger `update!`. This is
# used inside `update!`.
function supports_no_update(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    F = MOIU.variable_function_type(S)
    return MOI.supports_constraint(b.model, MOIU.variable_function_type(S), S) ||
        (S,) in keys(b.variable_best) || (F, S) in keys(b.constraint_best)
end
function supports_no_update(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    return MOI.supports_constraint(b.model, F, S) || (F, S) in keys(b.constraint_best)
end
function supports_no_update(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    return MOI.supports(b.model, MOI.ObjectiveFunction{F}()) || (F,) in keys(b.objective_best)
end

supports_constraint_bridges(::LazyBridgeOptimizer) = true
function supports_bridging_constrained_variable(
    b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet}
)
    update!(b, (S,))
    return (S,) in keys(b.variable_best)
end
function supports_bridging_constraint(
    b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet}
)
    update!(b, (F, S))
    return (F, S) in keys(b.constraint_best)
end
function supports_bridging_objective_function(
    b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction}
)
    update!(b, (F,))
    return (F,) in keys(b.objective_best)
end
function bridge_type(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    update!(b, (S,))
    result = get(b.variable_best, (S,), nothing)
    if result === nothing
        throw(MOI.UnsupportedConstraint{MOIU.variable_function_type(S), S}())
    end
    return result
end
function bridge_type(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    update!(b, (F, S))
    result = get(b.constraint_best, (F, S), nothing)
    if result === nothing
        throw(MOI.UnsupportedConstraint{F, S}())
    end
    return result
end
function bridge_type(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    update!(b, (F,))
    result = get(b.objective_best, (F,), nothing)
    if result === nothing
        throw(MOI.UnsupportedAttribute(MOI.ObjectiveFunction{F}()))
    end
    return result
end
