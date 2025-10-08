# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    VectorSlackBridge{T,F,G}

`VectorSlackBridge` implements the following reformulations:

 * ``\\min\\{f(x)\\}`` into ``\\min\\{y\\;|\\; y - f(x) \\in \\mathbb{R}_+ \\}``
 * ``\\max\\{f(x)\\}`` into ``\\max\\{y\\;|\\; f(x) - y \\in \\mathbb{R}_+ \\}``

where `F` is the type of `f(x) - y`, `G` is the type of `f(x)`, and `T` is the
coefficient type of `f(x)`.

## Source node

`VectorSlackBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`VectorSlackBridge` creates:

 * One variable node: [`MOI.VectorOfVariables`](@ref) in [`MOI.Reals`](@ref)
 * One objective node: [`MOI.ObjectiveFunction{MOI.VectorOfVariables}`](@ref)
 * One constraint node: `F`-in-[`MOI.Nonnegatives`](@ref)

!!! warning
    When using this bridge, changing the optimization sense is not supported.
    Set the sense to `MOI.FEASIBILITY_SENSE` first to delete the bridge, then
    set [`MOI.ObjectiveSense`](@ref) and re-add the objective.
"""
struct VectorSlackBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractVectorFunction,
} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    slacked_objectives::Vector{Int}
    constraint::MOI.ConstraintIndex{F,MOI.Nonnegatives}
end

const VectorSlack{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorSlackBridge{T},OT}

function bridge_objective(
    ::Type{VectorSlackBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
) where {T,F,G<:MOI.AbstractVectorFunction}
    variables, slacked_objectives = MOI.VariableIndex[], Int[]
    funcs = MOI.Utilities.eachscalar(func)
    for (i, fi) in enumerate(funcs)
        try
            push!(variables, convert(MOI.VariableIndex, fi))
        catch
            push!(variables, MOI.add_variable(model))
            push!(slacked_objectives, i)
        end
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.VectorOfVariables}(),
        MOI.VectorOfVariables(variables),
    )
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        msg = "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when using `MOI.Bridges.Objective.VectorSlackBridge`."
        throw(MOI.SetAttributeNotAllowed(MOI.ObjectiveFunction{G}(), msg))
    end
    slacks = MOI.VectorOfVariables(variables[slacked_objectives])
    f = if sense == MOI.MIN_SENSE
        MOI.Utilities.operate(-, T, slacks, funcs[slacked_objectives])
    else
        @assert sense == MOI.MAX_SENSE
        MOI.Utilities.operate(-, T, funcs[slacked_objectives], slacks)
    end
    set = MOI.Nonnegatives(length(slacked_objectives))
    ci = MOI.add_constraint(model, f, set)
    return VectorSlackBridge{T,F,G}(variables, slacked_objectives, ci)
end

function supports_objective_function(
    ::Type{<:VectorSlackBridge{T}},
    ::Type{MOI.VectorOfVariables},
) where {T}
    return false
end

function supports_objective_function(
    ::Type{<:VectorSlackBridge{T}},
    ::Type{F},
) where {T,F<:MOI.AbstractVectorFunction}
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:VectorSlackBridge},
)
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:VectorSlackBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.Nonnegatives)]
end

function MOI.Bridges.set_objective_function_type(::Type{<:VectorSlackBridge})
    return MOI.VectorOfVariables
end

function concrete_bridge_type(
    ::Type{<:VectorSlackBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
) where {T}
    F = MOI.Utilities.promote_operation(-, T, G, MOI.VectorOfVariables)
    return VectorSlackBridge{T,F,G}
end

function MOI.get(bridge::VectorSlackBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.slacked_objectives)
end

function MOI.get(bridge::VectorSlackBridge, ::MOI.ListOfVariableIndices)
    return bridge.variables[bridge.slacked_objectives]
end

function MOI.get(
    bridge::VectorSlackBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Nonnegatives},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::VectorSlackBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.Nonnegatives},
) where {T,F}
    return [bridge.constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::VectorSlackBridge)
    MOI.delete(model, bridge.constraint)
    MOI.delete(model, MOI.get(bridge, MOI.ListOfVariableIndices()))
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr_g::MOI.Bridges.ObjectiveFunctionValue{G},
    bridge::VectorSlackBridge{T,F,G},
) where {T,F,G}
    N = attr_g.result_index
    attr_f = MOI.Bridges.ObjectiveFunctionValue{MOI.VectorOfVariables}(N)
    objective_value = MOI.get(model, attr_f)
    con_primal = MOI.get(
        model,
        MOI.ConstraintPrimal(attr_g.result_index),
        bridge.constraint,
    )
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.MIN_SENSE
        # con_primal = objective_value - f(x)
        for (i, con_p) in zip(bridge.slacked_objectives, con_primal)
            objective_value[i] -= con_p
        end
    else
        @assert sense == MOI.MAX_SENSE
        # con_primal = f(x) - objective_value
        for (i, con_p) in zip(bridge.slacked_objectives, con_primal)
            objective_value[i] += con_p
        end
    end
    return objective_value
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{G},
    bridge::VectorSlackBridge{T,F,G},
) where {T,F,G<:MOI.AbstractVectorFunction}
    f = MOI.VectorOfVariables(bridge.variables)

    con_f = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
        # con_f = y - func so we need to negate it. Nothing to do in the
        # MAX_SENSE case.
        con_f = MOI.Utilities.operate(-, T, con_f)
    end
    con_fs = MOI.Utilities.eachscalar(con_f)
    f_map = Dict(i => fi for (i, fi) in zip(bridge.slacked_objectives, con_fs))
    args = [get(f_map, i, x) for (i, x) in enumerate(bridge.variables)]
    g = MOI.Utilities.operate(vcat, T, args...)
    slacks = bridge.variables[bridge.slacked_objectives]
    g = MOI.Utilities.remove_variable(g, slacks)
    return MOI.Utilities.convert_approx(G, g)
end

function MOI.modify(
    ::MOI.ModelLike,
    ::VectorSlackBridge{T},
    change::MOI.VectorConstantChange{T},
) where {T}
    return throw(MOI.ModifyObjectiveNotAllowed(change))
end
