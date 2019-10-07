function debug_variable(io::IO, variable_to_index, BT, args...)
    for C in added_constrained_variable_types(BT, args...)
        i = get(variable_to_index, C, nothing)
        if i != nothing
            MOIU.print_with_acronym(io, "  [$i] constrained variables in `$(C[1])` are not supported\n")
        end
    end
end
function debug_constraint(io::IO, constraint_to_index, BT, args...)
    for C in added_constraint_types(BT, args...)
        i = get(constraint_to_index, C, nothing)
        if i != nothing
            MOIU.print_with_acronym(io, "  ($i) `$(C[1])`-in-`$(C[2])` constraints not supported\n")
        end
    end
end
function debug_objective(io::IO, objective_to_index, BT, args...)
    F = set_objective_function_type(BT, args...)
    i = get(objective_to_index, (F,), nothing)
    if i != nothing
        MOIU.print_with_acronym(io, "  |$i| objective function of type `$F` not supported\n")
    end
end
function debug(b::LazyBridgeOptimizer, variable_to_index::OrderedDict,
               constraint_to_index::OrderedDict, objective_to_index::OrderedDict;
               io = Base.stdout)
    for (v, i) in variable_to_index
        S = v[1]
        MOIU.print_with_acronym(io, "[$i] constraint variables in `$S` not supported because")
        no_bridge = true
        for BT in b.variable_bridge_types
            if Variable.supports_constrained_variable(BT, S)
                if no_bridge
                    println(io, ":")
                    no_bridge = false
                end
                MOIU.print_with_acronym(io, "  Cannot use `$(Variable.concrete_bridge_type(BT, S))` because:\n")
                debug_variable(io, variable_to_index, BT, S)
                debug_constraint(io, constraint_to_index, BT, S)
            end
        end
        if no_bridge
            println(io, " no added bridge supports bridging it.")
        end
    end
    for (c, i) in constraint_to_index
        F, S = c
        MOIU.print_with_acronym(io, "($i) `$F`-in-`$S` constraints not supported because")
        no_bridge = true
        for BT in b.constraint_bridge_types
            if MOI.supports_constraint(BT, F, S)
                if no_bridge
                    println(io, ":")
                    no_bridge = false
                end
                MOIU.print_with_acronym(io, "  Cannot use `$(Constraint.concrete_bridge_type(BT, F, S))` because:\n")
                debug_variable(io, variable_to_index, BT, F, S)
                debug_constraint(io, constraint_to_index, BT, F, S)
            end
        end
        if no_bridge
            println(io, " no added bridge supports bridging it.")
        end
    end
    for (o, i) in objective_to_index
        F = o[1]
        MOIU.print_with_acronym(io, "|$i| objective function of type `$F` not supported because")
        no_bridge = true
        for BT in b.objective_bridge_types
            if Objective.supports_objective_function(BT, F)
                if no_bridge
                    println(io, ":")
                    no_bridge = false
                end
                MOIU.print_with_acronym(io, "  Cannot use `$(Objective.concrete_bridge_type(BT, F))` because:\n")
                debug_variable(io, variable_to_index, BT, F)
                debug_constraint(io, constraint_to_index, BT, F)
                debug_objective(io, objective_to_index, BT, F)
            end
        end
        if no_bridge
            println(io, " no added bridge supports bridging it.")
        end
    end
end
function debug(b::LazyBridgeOptimizer, variables::OrderedSet, constraints::OrderedSet, objectives::OrderedSet; kws...)
    variable_to_index = OrderedDict{Tuple{DataType}, Int}()
    for (i, key) in enumerate(variables)
        variable_to_index[key] = i
    end
    constraint_to_index = OrderedDict{Tuple{DataType, DataType}, Int}()
    for (i, key) in enumerate(constraints)
        constraint_to_index[key] = i
    end
    objective_to_index = OrderedDict{Tuple{DataType}, Int}()
    for (i, key) in enumerate(objectives)
        objective_to_index[key] = i
    end
    debug(b, variable_to_index, constraint_to_index, objective_to_index; kws...)
end
function debug(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
               S::Type{<:MOI.AbstractSet}; kws...)
    if MOI.supports_constraint(b, F, S)
        return
    end
    debug(b, required(b, (F, S))...; kws...)
end
function debug_supports_constraint(
    b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet}; kws...)
    debug(b, F, S; kws...)
end
function debug(b::LazyBridgeOptimizer,
               F::Type{<:MOI.AbstractScalarFunction}; kws...)
    if supports_bridging_objective_function(b, F)
        return
    end
    debug(b, required(b, (F,))...; kws...)
end
function debug_supports(
    b::LazyBridgeOptimizer, ::MOI.ObjectiveFunction{F}; kws...) where F
    debug(b, F; kws...)
end
