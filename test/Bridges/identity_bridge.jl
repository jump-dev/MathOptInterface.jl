# Dummy bridges used for testing
module IdentityBridges

import MathOptInterface
const MOI = MathOptInterface
const MOIB = MOI.Bridges
const MOIBV = MOIB.Variable
const MOIBC = MOIB.Constraint

const F{T} = MOI.ScalarAffineFunction{T}
const S{T} = MOI.EqualTo{T}

struct VariableBridge{T} <: MOIBV.AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.SingleVariable,S{T}}
end

function MOIBV.bridge_constrained_variable(
    ::Type{VariableBridge{T}},
    model::MOI.ModelLike,
    set::S{T},
) where {T}
    variable, constraint = MOI.add_constrained_variable(model, set)
    return VariableBridge{T}(variable, constraint)
end

function MOIBV.supports_constrained_variable(
    ::Type{VariableBridge{T}},
    ::Type{S{T}},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(
    ::Type{VariableBridge{T}},
) where {T}
    return [(S{T},)]
end

function MOIB.added_constraint_types(::Type{<:VariableBridge})
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
MOI.get(bridge::VariableBridge, ::MOI.NumberOfVariables) = 1
MOI.get(bridge::VariableBridge, ::MOI.ListOfVariableIndices) = [bridge.variable]

function MOI.get(
    bridge::VariableBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone},
)
    return 1
end

function MOI.get(
    bridge::VariableBridge,
    ::MOI.ListOfConstraintIndices{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    },
)
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::VariableBridge)
    MOI.delete(model, bridge.variable)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::VariableBridge{T},
) where {T}
    return MOI.get(model, attr, bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintDual},
    bridge::VariableBridge,
)
    return MOI.get(model, attr, bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimal,
    bridge::VariableBridge,
)
    return MOI.get(model, attr, bridge.variable)
end

function MOIB.bridged_function(bridge::VariableBridge{T}) where {T}
    return MOI.SingleVariable(bridge.variable)
end

function unbridged_map(
    bridge::VariableBridge{T},
    vi::MOI.VariableIndex,
) where {T}
    func = convert(F{T}, MOI.SingleVariable(vi))
    return (bridge.variable => func,)
end

struct ConstraintBridge{T} <: MOIBC.SetMapBridge{T,S{T},S{T},F{T},F{T}}
    constraint::MOI.ConstraintIndex{F{T},S{T}}
end

MOIBC.map_set(::Type{<:ConstraintBridge}, set::S) = set
MOIBC.inverse_map_set(::Type{<:ConstraintBridge}, set::S) = set
MOIBC.map_function(::Type{<:ConstraintBridge}, func) = func
MOIBC.inverse_map_function(::Type{<:ConstraintBridge}, func) = func
MOIBC.adjoint_map_function(::Type{<:ConstraintBridge}, func) = func
MOIBC.inverse_adjoint_map_function(::Type{<:ConstraintBridge}, func) = func

end
