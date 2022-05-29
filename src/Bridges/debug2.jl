# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    print_active_bridges([io::IO=stdout,] b::MOI.Bridges.LazyBridgeOptimizer)

Print the set of bridges that are active in the model `b`.
"""
function print_active_bridges(io::IO, b::MOI.Bridges.LazyBridgeOptimizer)
    F = MOI.get(b, MOI.ObjectiveFunctionType())
    _print_objective_tree(io, b, F, "")
    for (F, S) in MOI.get(b, MOI.ListOfConstraintTypesPresent())
        _print_constraint_tree(io, b, F, S, "")
    end
    return
end

function print_active_bridges(b::MOI.Bridges.LazyBridgeOptimizer)
    return print_active_bridges(stdout, b)
end

function _print_supported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return Printf.printstyled(io, s; bold = false, color = :green)
end

function _print_unsupported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return Printf.printstyled(io, s; bold = false, color = :red)
end

function _print_bridge(io, b, bridge::BT, offset) where {BT}
    new_offset = offset * " |  "
    for (F, S) in MOI.Bridges.added_constraint_types(BT)
        _print_constraint_tree(io, b, F, S, new_offset)
    end
    for (S,) in MOI.Bridges.added_constrained_variable_types(BT)
        _print_variable_tree(io, b, S, new_offset)
    end
    for x in MOI.get(bridge, MOI.ListOfVariableIndices())
        if MOI.Bridges.is_bridged(b, x)
            _print_variable(io, b, MOI.Reals, x, new_offset)
        end
    end
    return
end

function _print_objective_tree(io, b, F, offset)
    if !MOI.Bridges.is_bridged(b, F)
        print(io, offset, " * ")
        _print_supported(io, "Supported objective: $F\n")
        return
    end
    print(io, offset, " * ")
    _print_unsupported(io, "Unsupported objective: $F\n")
    bridge = b.objective_map[MOI.ObjectiveFunction{F}()]
    println(io, offset, " |  bridged by:")
    print(io, offset, " |   ")
    BT = typeof(bridge)
    MOI.Utilities.print_with_acronym(io, "$(BT)\n")
    println(io, offset, " |  introduces:")
    # Only objective bridges can create new objective trees.
    new_f = MOI.Bridges.set_objective_function_type(BT)
    _print_objective_tree(io, b, new_f, offset * " |  ")
    _print_bridge(io, b, bridge, offset)
    return
end

function _print_constraint_tree(io, b, F, S, offset)
    if !MOI.Bridges.is_bridged(b, F, S)
        # This constraint is natively supported.
        print(io, offset, " * ")
        _print_supported(io, "Supported constraint: $F-in-$S\n")
        return
    end
    for (ci, bridge) in b.constraint_map
        # Loop through bridged constraints to see if any F,S are bridged
        if ci isa MOI.ConstraintIndex{F,S}
            # The exact `ci` doesn't matter, only the type.
            print(io, offset, " * ")
            _print_unsupported(io, "Unsupported constraint: $F-in-$S\n")
            BT = typeof(bridge)
            println(io, offset, " |  bridged by:")
            print(io, offset, " |   ")
            MOI.Utilities.print_with_acronym(io, "$(BT)\n")
            println(io, offset, " |  introduces:")
            _print_bridge(io, b, bridge, offset)
            return
        end
    end
    # If we get here, (F, S) isn't bridged by a constraint bridge.
    if MOI.get(b, MOI.NumberOfConstraints{F,S}()) > 0
        _print_variable_tree(io, b, S, offset)
    end
    return
end

function _print_variable(io, b, S, x, offset)
    if !haskey(b.variable_map, x)
        print(io, offset, " * ")
        _print_supported(io, "Supported variable: $S\n")
        return
    end
    print(io, offset, " * ")
    _print_unsupported(io, "Unsupported variable: $S\n")
    bridge = b.variable_map[x]
    println(io, offset, " |  bridged by:")
    print(io, offset, " |    ")
    MOI.Utilities.print_with_acronym(io, "$(typeof(bridge))\n")
    println(io, offset, " |  introduces:")
    _print_bridge(io, b, bridge, offset)
    return
end

function _print_variable_tree(io, b, S::Type{<:MOI.AbstractVectorSet}, offset)
    if !MOI.Bridges.is_bridged(b, S)
        print(io, offset, " * ")
        _print_supported(io, "Supported variable: $S\n")
        return
    end
    for ci in MOI.get(b, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}())
        @assert MOI.Bridges.is_bridged(b, MOI.VectorOfVariables, S)
        f = MOI.get(b, MOI.ConstraintFunction(), ci)
        for x in f.variables
            if haskey(b.variable_map, x)
                _print_variable(io, b, S, x, offset)
                return
            end
        end
        return
    end
    return
end
