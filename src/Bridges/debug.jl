function debug_variable(io::IO, variable_to_index, BT, args...)
    for C in added_constrained_variable_types(BT, args...)
        i = get(variable_to_index, C, nothing)
        if i != nothing
            MOIU.print_with_acronym(io, "  [$i] constrained variables in `$(C[1])` are not supported\n")
        end
    end
end
function _print_unsupported_constraint(io::IO, constraint_to_index, C)
    i = get(constraint_to_index, C, nothing)
    if i != nothing
        MOIU.print_with_acronym(io, "  ($i) `$(C[1])`-in-`$(C[2])` constraints are not supported\n")
    end
end
function debug_constraint(io::IO, constraint_to_index, BT, args...)
    for C in added_constraint_types(BT, args...)
        _print_unsupported_constraint(io, constraint_to_index, C)
    end
end
function debug_objective(io::IO, objective_to_index, BT, args...)
    F = set_objective_function_type(BT, args...)
    i = get(objective_to_index, (F,), nothing)
    if i != nothing
        MOIU.print_with_acronym(io, "  |$i| objective function of type `$F` is not supported\n")
    end
end
function debug(b::LazyBridgeOptimizer, variable_to_index::OrderedDict,
               constraint_to_index::OrderedDict, objective_to_index::OrderedDict;
               io::IO = Base.stdout)
    for (v, i) in variable_to_index
        S = v[1]
        MOIU.print_with_acronym(io, "[$i] constraint variables in `$S` are not supported because")
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
        F = MOIU.variable_function_type(S)
        println(io, "  Cannot add free variables and then constrain them because:")
        _print_unsupported_constraint(io, constraint_to_index, (F, S))
    end
    for (c, i) in constraint_to_index
        F, S = c
        MOIU.print_with_acronym(io, "($i) `$F`-in-`$S` constraints are not supported because")
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
        MOIU.print_with_acronym(io, "|$i| objective function of type `$F` is not supported because")
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
               S::Type{<:MOI.AbstractSet}; io::IO = Base.stdout)
    if MOI.supports_constraint(b, F, S)
        if F == MOIU.variable_function_type(S)
            # This may be thanks to a variable bridge so `F`-in-`S` constraints
            # are maybe not supported but constraint variables in `S` are
            # definitely supported.
            MOIU.print_with_acronym(io, "Constrained variables in `$S` are supported.\n")
        else
            MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints are supported.\n")
        end
    else
        message = " are not supported and cannot be bridged into supported" *
            " constrained variables and constraints. See details below:"
        if F == MOIU.variable_function_type(S)
            MOIU.print_with_acronym(io, "Constrained variables in `$S`")
            println(io, message)
            required_tuples = required(b, (S,))
            fill_required!(required_tuples..., b, F, S)
            debug(b, required_tuples...; io = io)
        else
            MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints")
            println(io, message)
            debug(b, required(b, (F, S))...; io = io)
        end
    end
end

"""
    debug_supports_constraint(
        b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet}; io::IO = Base.stdout)

Prints to `io` explanations for the value of [`MOI.supports_constraint`](@ref)
with the same arguments.
"""
function debug_supports_constraint(
    b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet}; kws...)
    debug(b, F, S; kws...)
end
function debug(b::LazyBridgeOptimizer,
               F::Type{<:MOI.AbstractScalarFunction}; io::IO = Base.stdout)
    MOIU.print_with_acronym(io, "Objective function of type `$F` is")
    if supports_bridging_objective_function(b, F)
        MOIU.print_with_acronym(io, " supported.\n")
    else
        MOIU.print_with_acronym(io, " not supported and cannot be bridged" *
            " into a supported objective function by adding only supported" *
            " constrained variables and constraints. See details below:\n")
        debug(b, required(b, (F,))...; io = io)
    end
end

"""
    debug_supports(b::LazyBridgeOptimizer, ::MOI.ObjectiveFunction{F}; io::IO = Base.stdout) where F

Prints to `io` explanations for the value of [`MOI.supports`](@ref) with the
same arguments.
"""
function debug_supports(
    b::LazyBridgeOptimizer, ::MOI.ObjectiveFunction{F}; kws...) where F
    debug(b, F; kws...)
end
