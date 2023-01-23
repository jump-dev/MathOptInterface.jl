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
    slack::MOI.VectorOfVariables
    constraint::MOI.ConstraintIndex{F,MOI.Nonnegatives}
end

const VectorSlack{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorSlackBridge{T},OT}

function bridge_objective(
    ::Type{VectorSlackBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
) where {T,F,G<:MOI.AbstractVectorFunction}
    dim = MOI.output_dimension(func)
    slack = MOI.VectorOfVariables(MOI.add_variables(model, dim))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VectorOfVariables}(), slack)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        error(
            "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when",
            " using `MOI.Bridges.Objective.VectorSlackBridge`.",
        )
    end
    f = if sense == MOI.MIN_SENSE
        MOI.Utilities.operate(-, T, slack, func)
    elseif sense == MOI.MAX_SENSE
        MOI.Utilities.operate(-, T, func, slack)
    end
    set = MOI.Nonnegatives(dim)
    ci = MOI.add_constraint(model, f, set)
    return VectorSlackBridge{T,F,G}(slack, ci)
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
    return Tuple{Type}[]
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
    return MOI.output_dimension(bridge.slack)
end

function MOI.get(bridge::VectorSlackBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.slack.variables)
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
    MOI.delete(model, bridge.slack.variables)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr_g::MOI.Bridges.ObjectiveFunctionValue{G},
    bridge::VectorSlackBridge{T,F,G},
) where {T,F,G}
    N = attr_g.result_index
    attr_f = MOI.Bridges.ObjectiveFunctionValue{MOI.VectorOfVariables}(N)
    y_val = MOI.get(model, attr_f)
    con_primal = MOI.get(model, MOI.ConstraintPrimal(), bridge.constraint)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.MIN_SENSE
        # con_primal = y - func => func = y - con_primal
        return y_val - con_primal
    else
        @assert sense == MOI.MAX_SENSE
        # con_primal = func - y => func = con_primal + y
        return con_primal + y_val
    end
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{G},
    bridge::VectorSlackBridge{T,F,G},
) where {T,F,G<:MOI.AbstractVectorFunction}
    con_f = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
        # con_f = y - func so we need to negate it. Nothing to do in the
        # MAX_SENSE case.
        con_f = MOI.Utilities.operate(-, T, con_f)
    end
    g = MOI.Utilities.remove_variable(con_f, bridge.slack.variables)
    return MOI.Utilities.convert_approx(G, g)
end
