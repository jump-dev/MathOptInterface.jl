abstract type AbstractSlackBridge{T,VF,ZS,F,S} <: AbstractBridge end

function MOIB.added_constrained_variable_types(
    ::Type{<:AbstractSlackBridge{T,VF,ZS,F,S}},
) where {T,VF,ZS,F,S}
    return [(S,)]
end

function MOIB.added_constraint_types(
    ::Type{<:AbstractSlackBridge{T,VF,ZS,F}},
) where {T,VF,ZS,F}
    return [(F, ZS)]
end

function MOI.get(
    ::AbstractSlackBridge{T,VF,ZS,F},
    ::MOI.NumberOfConstraints{F,ZS},
) where {T,VF,ZS,F}
    return Int64(1)
end

function MOI.get(
    ::AbstractSlackBridge{T,VF,ZS,F,S},
    ::MOI.NumberOfConstraints{VF,S},
) where {T,VF,ZS,F,S}
    return Int64(1)
end

function MOI.get(
    bridge::AbstractSlackBridge{T,VF,ZS,F},
    ::MOI.ListOfConstraintIndices{F,ZS},
) where {T,VF,ZS,F}
    return [bridge.equality]
end

function MOI.get(
    bridge::AbstractSlackBridge{T,VF,ZS,F,S},
    ::MOI.ListOfConstraintIndices{VF,S},
) where {T,VF,ZS,F,S}
    return [bridge.slack_in_set]
end

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::AbstractSlackBridge)
    MOI.delete(model, bridge.equality)
    MOI.delete(model, bridge.slack)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:AbstractSlackBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::AbstractSlackBridge,
)
    # due to equality, slack should have the same value as original affine function
    return MOI.get(model, attr, bridge.slack_in_set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::AbstractSlackBridge,
    value,
)
    if bridge isa ScalarSlackBridge
        MOI.set(model, MOI.VariablePrimalStart(), bridge.slack, value)
    else
        MOI.set.(model, MOI.VariablePrimalStart(), bridge.slack, value)
    end
    MOI.set(model, attr, bridge.slack_in_set, value)
    MOI.set(model, attr, bridge.equality, zero(value))
    return
end

function MOI.get(
    model::MOI.ModelLike,
    a::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::AbstractSlackBridge,
)
    # The dual constraint on slack (since it is free) is
    # -dual_slack_in_set + dual_equality = 0 so the two duals are
    # equal and we can return either one of them.
    return MOI.get(model, a, bridge.slack_in_set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::AbstractSlackBridge,
    value,
)
    # As the slack appears `+slack` in `slack_in_set` and `-slack` in equality,
    # giving `value` to both will cancel it out in the Lagrangian.
    # Giving `value` to `bridge.equality` will put the function in the
    # lagrangian as expected.
    MOI.set(model, attr, bridge.slack_in_set, value)
    MOI.set(model, attr, bridge.equality, value)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::AbstractSlackBridge,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model, bridge.equality, change)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::AbstractSlackBridge{T,VF,ZS,F,S},
    change::S,
) where {T,VF,ZS,F,S}
    MOI.set(model, MOI.ConstraintSet(), bridge.slack_in_set, change)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::AbstractSlackBridge,
)
    return MOIU.remove_variable(
        MOI.get(model, attr, bridge.equality),
        bridge.slack,
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::AbstractSlackBridge,
)
    return MOI.get(model, attr, bridge.slack_in_set)
end

# scalar version

"""
    ScalarSlackBridge{T, F, S}

The `ScalarSlackBridge` converts a constraint `G`-in-`S` where `G` is a function different
from `SingleVariable` into the constraints `F`-in-`EqualTo{T}` and `SingleVariable`-in-`S`.
`F` is the result of subtracting a `SingleVariable` from `G`.
Typically `G` is the same as `F`, but that is not mandatory.
"""
struct ScalarSlackBridge{T,F,S} <:
       AbstractSlackBridge{T,MOI.SingleVariable,MOI.EqualTo{T},F,S}
    slack::MOI.VariableIndex
    slack_in_set::CI{MOI.SingleVariable,S}
    equality::CI{F,MOI.EqualTo{T}}
end

function bridge_constraint(
    ::Type{ScalarSlackBridge{T,F,S}},
    model,
    f::MOI.AbstractScalarFunction,
    s::S,
) where {T,F,S}
    slack, slack_in_set = MOI.add_constrained_variable(model, s)
    new_f = MOIU.operate(-, T, f, MOI.SingleVariable(slack))
    equality = MOI.add_constraint(model, new_f, MOI.EqualTo(zero(T)))
    return ScalarSlackBridge{T,F,S}(slack, slack_in_set, equality)
end

# start allowing everything (scalar)
function MOI.supports_constraint(
    ::Type{ScalarSlackBridge{T}},
    F::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return MOIU.is_coefficient_type(F, T)
end
# then restrict (careful with ambiguity)
function MOI.supports_constraint(
    ::Type{ScalarSlackBridge{T}},
    ::Type{<:MOI.SingleVariable},
    ::Type{<:MOI.EqualTo},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Type{ScalarSlackBridge{T}},
    ::Type{<:MOI.SingleVariable},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Type{ScalarSlackBridge{T}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:MOI.EqualTo},
) where {T}
    return false
end

function concrete_bridge_type(
    ::Type{<:ScalarSlackBridge{T}},
    F::Type{<:MOI.AbstractScalarFunction},
    S::Type{<:MOI.AbstractScalarSet},
) where {T}
    F2 = MOIU.promote_operation(-, T, F, MOI.SingleVariable)
    return ScalarSlackBridge{T,F2,S}
end

# Attributes, Bridge acting as a model
MOI.get(b::ScalarSlackBridge, ::MOI.NumberOfVariables) = Int64(1)
MOI.get(b::ScalarSlackBridge, ::MOI.ListOfVariableIndices) = [b.slack]

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintBasisStatus,
    bridge::ScalarSlackBridge{T,F,S},
) where {T,F,S<:MOI.Interval}
    return MOI.get(
        model,
        MOI.VariableBasisStatus(),
        MOI.VariableIndex(bridge.slack_in_set.value),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintBasisStatus,
    bridge::ScalarSlackBridge,
)
    status = MOI.get(
        model,
        MOI.VariableBasisStatus(),
        MOI.VariableIndex(bridge.slack_in_set.value),
    )
    if status == MOI.NONBASIC_AT_LOWER || status == MOI.NONBASIC_AT_UPPER
        return MOI.NONBASIC
    end
    return status
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ScalarSlackBridge{T,F,S},
    func::F,
) where {T,F,S}
    new_func = MOIU.operate(-, T, func, MOI.SingleVariable(bridge.slack))
    MOI.set(model, MOI.ConstraintFunction(), bridge.equality, new_func)
    return
end

# vector version

"""
    VectorSlackBridge{T, F, S}

The `VectorSlackBridge` converts a constraint `G`-in-`S` where `G` is a function different
from `VectorOfVariables` into the constraints `F`in-`Zeros` and `VectorOfVariables`-in-`S`.
`F` is the result of subtracting a `VectorOfVariables` from `G`.
Tipically `G` is the same as `F`, but that is not mandatory.
"""
struct VectorSlackBridge{T,F,S} <:
       AbstractSlackBridge{T,MOI.VectorOfVariables,MOI.Zeros,F,S}
    slack::Vector{MOI.VariableIndex}
    slack_in_set::CI{MOI.VectorOfVariables,S}
    equality::CI{F,MOI.Zeros}
end

function bridge_constraint(
    ::Type{VectorSlackBridge{T,F,S}},
    model,
    f::MOI.AbstractVectorFunction,
    s::S,
) where {T,F,S}
    d = MOI.dimension(s)
    slack, slack_in_set = MOI.add_constrained_variables(model, s)
    new_f = MOIU.operate(-, T, f, MOI.VectorOfVariables(slack))
    equality = MOI.add_constraint(model, new_f, MOI.Zeros(d))
    return VectorSlackBridge{T,F,S}(slack, slack_in_set, equality)
end

function MOI.supports_constraint(
    ::Type{VectorSlackBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.AbstractVectorSet},
) where {T}
    return MOIU.is_coefficient_type(F, T)
end

function MOI.supports_constraint(
    ::Type{VectorSlackBridge{T}},
    ::Type{<:MOI.VectorOfVariables},
    ::Type{<:MOI.Zeros},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Type{VectorSlackBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.Zeros},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Type{VectorSlackBridge{T}},
    ::Type{<:MOI.VectorOfVariables},
    ::Type{<:MOI.AbstractVectorSet},
) where {T}
    return false
end

function concrete_bridge_type(
    ::Type{<:VectorSlackBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    S::Type{<:MOI.AbstractVectorSet},
) where {T}
    F2 = MOIU.promote_operation(-, T, F, MOI.VectorOfVariables)
    return VectorSlackBridge{T,F2,S}
end

# Attributes, Bridge acting as a model
MOI.get(b::VectorSlackBridge, ::MOI.NumberOfVariables) = Int64(length(b.slack))

MOI.get(b::VectorSlackBridge, ::MOI.ListOfVariableIndices) = copy(b.slack)

# Attributes, Bridge acting as a constraint
function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::VectorSlackBridge{T,F,S},
    func::F,
) where {T,F,S}
    new_func = MOIU.operate(
        -,
        T,
        func,
        MOI.VectorAffineFunction{T}(MOI.VectorOfVariables(bridge.slack)),
    )
    MOI.set(model, MOI.ConstraintFunction(), bridge.equality, new_func)
    return
end
