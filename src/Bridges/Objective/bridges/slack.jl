# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SlackBridge{T,F,G}

`SlackBridge` implements the following reformulations:

 * ``\\min\\{f(x)\\}`` into ``\\min\\{y\\;|\\; f(x) - y \\le 0\\}``
 * ``\\max\\{f(x)\\}`` into ``\\max\\{y\\;|\\; f(x) - y \\ge 0\\}``

where `F` is the type of `f(x) - y`, `G` is the type of `f(x)`, and `T` is the
coefficient type of `f(x)`.

## Source node

`SlackBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`SlackBridge` creates:

 * One variable node: [`MOI.VariableIndex`](@ref) in [`MOI.Reals`](@ref)
 * One objective node: [`MOI.ObjectiveFunction{MOI.VariableIndex}`](@ref)
 * One constraint node, that depends on the [`MOI.ObjectiveSense`](@ref):
   * `F`-in-[`MOI.LessThan`](@ref) if `MIN_SENSE`
   * `F`-in-[`MOI.GreaterThan`](@ref) if `MAX_SENSE`

!!! warning
    When using this bridge, changing the optimization sense is not supported.
    Set the sense to `MOI.FEASIBILITY_SENSE` first to delete the bridge, then
    set [`MOI.ObjectiveSense`](@ref) and re-add the objective.
"""
struct SlackBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    G<:MOI.AbstractScalarFunction,
} <: AbstractBridge
    slack::MOI.VariableIndex
    constraint::Union{
        MOI.ConstraintIndex{F,MOI.LessThan{T}},
        MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
    }
    constant::T
end

const Slack{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SlackBridge{T},OT}

function bridge_objective(
    ::Type{SlackBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
) where {T,F,G<:MOI.AbstractScalarFunction}
    slack = MOI.add_variable(model)
    f = MOI.Utilities.operate(-, T, func, slack)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
        set = MOI.LessThan(zero(T))
    elseif MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        set = MOI.GreaterThan(zero(T))
    else
        error(
            "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when",
            " using `MOI.Bridges.Objective.SlackBridge`.",
        )
    end
    constraint = MOI.Utilities.normalize_and_add_constraint(model, f, set)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), slack)
    return SlackBridge{T,F,G}(slack, constraint, MOI.constant(f))
end

function supports_objective_function(
    ::Type{<:SlackBridge{T}},
    ::Type{MOI.VariableIndex},
) where {T}
    return false
end

function supports_objective_function(
    ::Type{<:SlackBridge{T}},
    ::Type{F},
) where {T,F<:MOI.AbstractScalarFunction}
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:SlackBridge})
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SlackBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.GreaterThan{T}), (F, MOI.LessThan{T})]
end

function MOI.Bridges.set_objective_function_type(::Type{<:SlackBridge})
    return MOI.VariableIndex
end

function concrete_bridge_type(
    ::Type{<:SlackBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
) where {T}
    F = MOI.Utilities.promote_operation(-, T, G, MOI.VariableIndex)
    return SlackBridge{T,F,G}
end

# Attributes, Bridge acting as a model
MOI.get(::SlackBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(bridge::SlackBridge, ::MOI.ListOfVariableIndices)
    return [bridge.slack]
end

function MOI.get(
    bridge::SlackBridge{T,F},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {T,F,S<:Union{MOI.GreaterThan{T},MOI.LessThan{T}}}
    return bridge.constraint isa MOI.ConstraintIndex{F,S} ? 1 : 0
end

function MOI.get(
    bridge::SlackBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S<:Union{MOI.GreaterThan{T},MOI.LessThan{T}}}
    if bridge.constraint isa MOI.ConstraintIndex{F,S}
        return [bridge.constraint]
    else
        return MOI.ConstraintIndex{F,S}[]
    end
end

function MOI.delete(model::MOI.ModelLike, bridge::SlackBridge)
    MOI.delete(model, bridge.constraint)
    MOI.delete(model, bridge.slack)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionValue{G},
    bridge::SlackBridge{T,F,G},
) where {T,F,G}
    slack = MOI.get(
        model,
        MOI.Bridges.ObjectiveFunctionValue{MOI.VariableIndex}(
            attr.result_index,
        ),
    )
    # There may be a gap between the value of the original objective `g` and the
    # value of `bridge.slack`. Since `bridge.constraint` is `g - slack`, we can
    # get the value of the original objective by summing the value of `slack`
    # with the `ConstraintPrimal` of the constraint.
    obj_slack_constant =
        MOI.get(model, MOI.ConstraintPrimal(), bridge.constraint)
    return obj_slack_constant + slack + bridge.constant
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{G},
    bridge::SlackBridge{T,F,G},
) where {T,F,G<:MOI.AbstractScalarFunction}
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    f = if !iszero(bridge.constant)
        MOI.Utilities.operate(+, T, func, bridge.constant)
    else
        func
    end
    g = MOI.Utilities.remove_variable(f, bridge.slack)
    return MOI.Utilities.convert_approx(G, g)
end
