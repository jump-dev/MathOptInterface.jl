"""
    SOCtoRSOCBridge{T} <: Bridges.Variable.SetMapBridge{T,MOI.RotatedSecondOrderCone,MOI.SecondOrderCone}

Same transformation as [`MOI.Bridges.Constraint.SOCtoRSOCBridge`](@ref).
"""
struct SOCtoRSOCBridge{T} <:
       SetMapBridge{T,MOI.RotatedSecondOrderCone,MOI.SecondOrderCone}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    }
end

"""
    RSOCtoSOCBridge{T} <: Bridges.Variable.SetMapBridge{T,MOI.SecondOrderCone,MOI.RotatedSecondOrderCone}

Same transformation as [`MOI.Bridges.Constraint.RSOCtoSOCBridge`](@ref).
"""
struct RSOCtoSOCBridge{T} <:
       SetMapBridge{T,MOI.SecondOrderCone,MOI.RotatedSecondOrderCone}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}
end
