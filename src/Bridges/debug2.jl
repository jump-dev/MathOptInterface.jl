import Printf

function _print_supported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return Printf.printstyled(io, s; bold = false, color = :green)
end

function _print_unsupported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return print(io, s)
end

function _print_constraint_tree(io, b, F, S, offset)
    if !MOI.Bridges.is_bridged(b, F, S)
        print(io, offset, " * ")
        _print_supported(io, "Constraint $F-in-$S\n")
        return
    end
    for (ci, bridge) in b.constraint_map
        if !(ci isa MOI.ConstraintIndex{F,S})
            continue
        end
        print(io, offset, " * ")
        _print_unsupported(io, "Constraint: $F-in-$S\n")
        print(io, offset, " |  bridged by: ")
        MOI.Utilities.print_with_acronym(io, "$(typeof(bridge))\n")
        for (FF, SS) in MOI.Bridges.added_constraint_types(typeof(bridge))
            _print_constraint_tree(io, b, FF, SS, offset * " |   ")
        end
        return
    end
end

function _print_objective_tree(io, b, F, offset)
    if !MOI.Bridges.is_bridged(b, F)
        print(io, offset, " * ")
        _print_supported(io, "Objective $F\n")
        return
    end
    print(io, offset, " * ")
    _print_unsupported(io, "Objective: $F\n")
    bridge = b.objective_map[MOI.ObjectiveFunction{F}()]
    print(io, offset, " |  bridged by: ")
    MOI.Utilities.print_with_acronym(io, "$(typeof(bridge))\n")
    new_f = MOI.Bridges.set_objective_function_type(typeof(bridge))
    _print_objective_tree(io, b, new_f, offset * " |   ")
    for (f, s) in MOI.Bridges.added_constraint_types(typeof(bridge))
        _print_constraint_tree(io, b, f, s, offset * " |   ")
    end
    return
end

function print_graph(b::MOI.Bridges.LazyBridgeOptimizer, io = stdout)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    _print_objective_tree(io, b, F, "")
    for (F, S) in MOI.get(b, MOI.ListOfConstraintTypesPresent())
        @show F, S
        _print_constraint_tree(io, b, F, S, "")
    end
    return
end

print_graph(model)
