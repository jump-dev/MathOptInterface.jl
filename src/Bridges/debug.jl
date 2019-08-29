function debug_variable(variable_to_index, BT, args...)
    for C in added_constrained_variable_types(BT, args...)
        i = get(variable_to_index, C, nothing)
        if i != nothing
            println("  [", i, "] constrained variables in `", C[1], "` are not supported")
        end
    end
end
function debug_constraint(constraint_to_index, BT, args...)
    for C in added_constraint_types(BT, args...)
        i = get(constraint_to_index, C, nothing)
        if i != nothing
            println("  (", i, ") `", C[1], "`-in-`", C[2], "` constraints not supported")
        end
    end
end
function debug_objective(objective_to_index, BT, args...)
    F = set_objective_function_type(BT, args...)
    i = get(objective_to_index, (F,), nothing)
    if i != nothing
        println("  |", i, "| objective function of type `", F, "` not supported")
    end
end
function debug(b::LazyBridgeOptimizer, variable_to_index::Dict,
               constraint_to_index::Dict, objective_to_index::Dict)
    for (v, i) in variable_to_index
        S = v[1]
        println("[", i, "] constraint variables in `", S, "` not supported because:")
        for BT in b.variable_bridge_types
            if Variable.supports_constrained_variable(BT, S)
                println("  Cannot use `$BT` because:")
                debug_variable(variable_to_index, BT, S)
                debug_constraint(constraint_to_index, BT, S)
            end
        end
    end
    for (c, i) in constraint_to_index
        F, S = c
        println("(", i, ") `", F, "`-in-`", S, "` constraints not supported because:")
        for BT in b.constraint_bridge_types
            if MOI.supports_constraint(BT, F, S)
                println("  Cannot use `$BT` because:")
                debug_variable(variable_to_index, BT, F, S)
                debug_constraint(constraint_to_index, BT, F, S)
            end
        end
    end
    for (o, i) in objective_to_index
        F = o[1]
        println("|", i, "| objective function of type `", F, "` not supported because:")
        for BT in b.objective_bridge_types
            if Objective.supports_objective_function(BT, F)
                println("  Cannot use `$BT` because:")
                debug_variable(variable_to_index, BT, F)
                debug_constraint(constraint_to_index, BT, F)
                debug_objective(objective_to_index, BT, F)
            end
        end
    end
end
function debug(b::LazyBridgeOptimizer, variables::Set, constraints::Set, objectives::Set)
    variable_to_index = Dict{Tuple{DataType}, Int}()
    for (i, key) in enumerate(variables)
        variable_to_index[key] = i
    end
    constraint_to_index = Dict{Tuple{DataType, DataType}, Int}()
    for (i, key) in enumerate(constraints)
        constraint_to_index[key] = i
    end
    objective_to_index = Dict{Tuple{DataType}, Int}()
    for (i, key) in enumerate(objectives)
        objective_to_index[key] = i
    end
    debug(b, variable_to_index, constraint_to_index, objective_to_index)
end
function debug(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
               S::Type{<:MOI.AbstractSet})
    if MOI.supports_constraint(b, F, S)
        return
    end
    debug(b, required(b, (F, S))...)
end
function debug(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    if supports_bridging_objective_function(b, F)
        return
    end
    debug(b, required(b, (F,))...)
end
