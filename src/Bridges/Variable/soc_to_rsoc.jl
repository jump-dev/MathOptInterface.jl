"""
    SOCtoRSOCBridge{T} <: Bridges.Variable.AbstractBridge

Same transformation as [`MOI.Bridges.Constraint.SOCtoRSOCBridge`](@ref).
"""
struct SOCtoRSOCBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    }
end

function bridge_constrained_variable(
    ::Type{SOCtoRSOCBridge{T}},
    model::MOI.ModelLike,
    set::MOI.SecondOrderCone,
) where {T}
    variables, constraint = MOI.add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone(MOI.dimension(set)),
    )
    return SOCtoRSOCBridge{T}(variables, constraint)
end

function supports_constrained_variable(
    ::Type{<:SOCtoRSOCBridge},
    ::Type{MOI.SecondOrderCone},
)
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:SOCtoRSOCBridge})
    return [(MOI.RotatedSecondOrderCone,)]
end

function MOIB.added_constraint_types(::Type{<:SOCtoRSOCBridge})
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::SOCtoRSOCBridge, ::MOI.NumberOfVariables)
    return length(bridge.variables)
end

function MOI.get(bridge::SOCtoRSOCBridge, ::MOI.ListOfVariableIndices)
    return bridge.variables
end

function MOI.get(
    bridge::SOCtoRSOCBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone},
)
    return 1
end

function MOI.get(
    bridge::SOCtoRSOCBridge,
    ::MOI.ListOfConstraintIndices{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    },
)
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::SOCtoRSOCBridge)
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SOCtoRSOCBridge{T},
) where {T}
    return MOI.SecondOrderCone(length(bridge.variables))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintDual},
    bridge::SOCtoRSOCBridge,
)
    return rotate_result(model, attr, bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimal,
    bridge::SOCtoRSOCBridge,
    i::IndexInVector,
)
    return rotate_result(model, attr, bridge.variables, i)
end

function MOIB.bridged_function(
    bridge::SOCtoRSOCBridge{T},
    i::IndexInVector,
) where {T}
    return rotate_bridged_function(T, bridge.variables, i)
end

function unbridged_map(
    bridge::SOCtoRSOCBridge{T},
    vis::Vector{MOI.VariableIndex},
) where {T}
    return rotate_unbridged_map(T, bridge.variables, vis)
end
