function rotate_result(
    model,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintDual},
    ci::MOI.ConstraintIndex,
)
    x = MOI.get(model, attr, ci)
    T = eltype(x)
    s2 = √T(2)
    return [x[1] / s2 + x[2] / s2; x[1] / s2 - x[2] / s2; x[3:end]]
end

function rotate_result(
    model,
    attr::MOI.VariablePrimal,
    variables,
    i::MOIB.IndexInVector,
)
    if i.value == 1 || i.value == 2
        t, u = MOI.get(model, attr, variables[1:2])
        T = typeof(t)
        s2 = √T(2)
        if i.value == 1
            return t / s2 + u / s2
        else
            return t / s2 - u / s2
        end
    else
        return MOI.get(model, attr, variables[i.value])
    end
end

function rotate_bridged_function(T::Type, variables, i::MOIB.IndexInVector)
    s2 = √T(2)
    if i.value == 1 || i.value == 2
        t = MOIU.operate(/, T, MOI.SingleVariable(variables[1]), s2)
        u = MOIU.operate(/, T, MOI.SingleVariable(variables[2]), s2)
        if i.value == 1
            return MOIU.operate!(+, T, t, u)
        else
            return MOIU.operate!(-, T, t, u)
        end
    else
        return convert(
            MOI.ScalarAffineFunction{T},
            MOI.SingleVariable(variables[i.value]),
        )
    end
end

function rotate_unbridged_map(
    T::Type,
    inner_variables::Vector{MOI.VariableIndex},
    bridged_variables::Vector{MOI.VariableIndex},
)
    F = MOI.ScalarAffineFunction{T}
    umap = Pair{MOI.VariableIndex,F}[]
    s2 = √T(2)
    t = MOIU.operate(/, T, MOI.SingleVariable(bridged_variables[1]), s2)
    u = MOIU.operate(/, T, MOI.SingleVariable(bridged_variables[2]), s2)
    push!(umap, inner_variables[1] => MOIU.operate(+, T, t, u))
    push!(umap, inner_variables[2] => MOIU.operate(-, T, t, u))
    for i in 3:length(bridged_variables)
        func = convert(F, MOI.SingleVariable(bridged_variables[i]))
        push!(umap, inner_variables[i] => func)
    end
    return umap
end

"""
    RSOCtoSOCBridge{T} <: Bridges.Variable.AbstractBridge

Same transformation as [`MOI.Bridges.Constraint.RSOCtoSOCBridge`](@ref).
"""
struct RSOCtoSOCBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}
end

function bridge_constrained_variable(
    ::Type{RSOCtoSOCBridge{T}},
    model::MOI.ModelLike,
    set::MOI.RotatedSecondOrderCone,
) where {T}
    variables, constraint = MOI.add_constrained_variables(
        model,
        MOI.SecondOrderCone(MOI.dimension(set)),
    )
    return RSOCtoSOCBridge{T}(variables, constraint)
end

function supports_constrained_variable(
    ::Type{<:RSOCtoSOCBridge},
    ::Type{MOI.RotatedSecondOrderCone},
)
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:RSOCtoSOCBridge})
    return [(MOI.SecondOrderCone,)]
end

function MOIB.added_constraint_types(::Type{<:RSOCtoSOCBridge})
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::RSOCtoSOCBridge, ::MOI.NumberOfVariables)
    return Int64(length(bridge.variables))
end

function MOI.get(bridge::RSOCtoSOCBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    ::RSOCtoSOCBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    return Int64(1)
end

function MOI.get(
    bridge::RSOCtoSOCBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::RSOCtoSOCBridge)
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::RSOCtoSOCBridge{T},
) where {T}
    return MOI.RotatedSecondOrderCone(length(bridge.variables))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintDual},
    bridge::RSOCtoSOCBridge,
)
    return rotate_result(model, attr, bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimal,
    bridge::RSOCtoSOCBridge,
    i::MOIB.IndexInVector,
)
    return rotate_result(model, attr, bridge.variables, i)
end

function MOIB.bridged_function(
    bridge::RSOCtoSOCBridge{T},
    i::MOIB.IndexInVector,
) where {T}
    return rotate_bridged_function(T, bridge.variables, i)
end

function unbridged_map(
    bridge::RSOCtoSOCBridge{T},
    vis::Vector{MOI.VariableIndex},
) where {T}
    return rotate_unbridged_map(T, bridge.variables, vis)
end
