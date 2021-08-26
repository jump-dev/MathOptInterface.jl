# Dummy bridges used for testing
module IdentityBridges

import MathOptInterface
const MOI = MathOptInterface
const MOIB = MOI.Bridges

const F{T} = MOI.ScalarAffineFunction{T}
const S{T} = MOI.EqualTo{T}

struct VariableBridge{T} <: MOIB.Variable.SetMapBridge{T,S{T},S{T}}
    variable::MOI.VariableIndex
    constraint::MOI.ConstraintIndex{MOI.VariableIndex,S{T}}
end

struct ConstraintBridge{T} <:
       MOIB.Constraint.SetMapBridge{T,S{T},S{T},F{T},F{T}}
    constraint::MOI.ConstraintIndex{F{T},S{T}}
end

const IdentityBridge{T} = Union{VariableBridge{T},ConstraintBridge{T}}

MOIB.map_set(::Type{<:IdentityBridge}, set::S) = set
MOIB.inverse_map_set(::Type{<:IdentityBridge}, set::S) = set
MOIB.map_function(::Type{<:IdentityBridge}, func) = func
MOIB.inverse_map_function(::Type{<:IdentityBridge}, func) = func
MOIB.adjoint_map_function(::Type{<:IdentityBridge}, func) = func
MOIB.inverse_adjoint_map_function(::Type{<:IdentityBridge}, func) = func

struct ObjectiveBridge{T} <: MOIB.Objective.AbstractBridge end

function MOIB.Objective.bridge_objective(
    ::Type{ObjectiveBridge{T}},
    model::MOI.ModelLike,
    func::F{T},
) where {T}
    MOI.set(model, MOI.ObjectiveFunction{F}(), func)
    return ObjectiveBridge{T}()
end

function MOIB.Objective.supports_objective_function(
    ::Type{ObjectiveBridge{T}},
    ::Type{F{T}},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:ObjectiveBridge})
    return Tuple{DataType}[]
end

function MOIB.added_constraint_types(::Type{<:ObjectiveBridge})
    return Tuple{DataType,DataType}[]
end

function MOIB.set_objective_function_type(::Type{ObjectiveBridge{T}}) where {T}
    return F{T}
end

# Attributes, Bridge acting as a model
function MOI.get(::ObjectiveBridge, ::MOI.NumberOfVariables)
    return 0
end

function MOI.get(::ObjectiveBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

# No variables or constraints are created in this bridge so there is nothing to
# delete.
MOI.delete(model::MOI.ModelLike, bridge::ObjectiveBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::ObjectiveBridge,
    ::MOI.OptimizationSense,
)
    # `ObjectiveBridge` is sense agnostic, therefore, we don't need to change
    # anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOIB.ObjectiveFunctionValue{F{T}},MOI.ObjectiveFunction{F{T}}},
    ::ObjectiveBridge{T},
) where {T}
    return MOI.get(model, attr)
end

end
