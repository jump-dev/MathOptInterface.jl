function rotate_result(
    model, attr::Union{MOI.ConstraintPrimal, MOI.ConstraintDual},
    ci::MOI.ConstraintIndex)
    x = MOI.get(model, attr, ci)
    s2 = √2
    return [x[1]/s2 + x[2]/s2; x[1]/s2 - x[2]/s2; x[3:end]]
end

"""
    SOCtoRSOCBridge{T} <: Bridges.Variable.AbstractBridge

Same transformation as [`MOI.Bridges.Constraint.SOCRBridge`](@ref).
"""
struct SOCtoRSOCBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.RotatedSecondOrderCone}
end
function bridge_constrained_variable(
    ::Type{SOCtoRSOCBridge{T}}, model::MOI.ModelLike,
    set::MOI.SecondOrderCone) where T
    variables, constraint = MOI.add_constrained_variables(
        model, MOI.RotatedSecondOrderCone(MOI.dimension(set)))
    return SOCtoRSOCBridge{T}(variables, constraint)
end

function supports_constrained_variable(
    ::Type{<:SOCtoRSOCBridge}, ::Type{MOI.SecondOrderCone})
    return true
end
function MOIB.added_constrained_variable_types(::Type{<:SOCtoRSOCBridge})
    return [(MOI.RotatedSecondOrderCone,)]
end
function MOIB.added_constraint_types(::Type{<:SOCtoRSOCBridge})
    return Tuple{DataType, DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::SOCtoRSOCBridge, ::MOI.NumberOfVariables)
    return length(bridge.variables)
end
function MOI.get(bridge::SOCtoRSOCBridge, ::MOI.ListOfVariableIndices)
    return bridge.variables
end
function MOI.get(bridge::SOCtoRSOCBridge,
                 ::MOI.NumberOfConstraints{MOI.VectorOfVariables,
                                           MOI.RotatedSecondOrderCone})
    return 1
end
function MOI.get(bridge::SOCtoRSOCBridge,
                 ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,
                                               MOI.RotatedSecondOrderCone})
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::SOCtoRSOCBridge)
    MOI.delete(model, bridge.variables)
end

# Attributes, Bridge acting as a constraint

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::SOCtoRSOCBridge{T}) where T
    return MOI.SecondOrderCone(length(bridge.variables))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal, MOI.ConstraintDual},
    bridge::SOCtoRSOCBridge)
    return rotate_result(model, attr, bridge.constraint)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.VariablePrimal,
                 bridge::SOCtoRSOCBridge{T}, i::IndexInVector) where T
    if i.value == 1 || i.value == 2
        t, u = MOI.get(model, attr, bridge.variables[1:2])
        s2 = convert(T, √2)
        if i.value == 1
            return t/s2 + u/s2
        else
            return t/s2 - u/s2
        end
    else
        return MOI.get(model, attr, bridge.variables[i.value])
    end
end

function MOIB.bridged_function(bridge::SOCtoRSOCBridge{T}, i::IndexInVector) where T
    s2 = convert(T, √2)
    if i.value == 1 || i.value == 2
        t = MOIU.operate(/, T, MOI.SingleVariable(bridge.variables[1]), s2)
        u = MOIU.operate(/, T, MOI.SingleVariable(bridge.variables[2]), s2)
        if i.value == 1
            return MOIU.operate!(+, T, t, u)
        else
            return MOIU.operate!(-, T, t, u)
        end
    else
        return convert(MOI.ScalarAffineFunction{T}, MOI.SingleVariable(bridge.variables[i.value]))
    end
end
function unbridged_map(bridge::SOCtoRSOCBridge{T}, vis::Vector{MOI.VariableIndex}) where T
    umap = Pair{MOI.VariableIndex, MOI.ScalarAffineFunction{T}}[]
    s2 = convert(T, √2)
    t = MOIU.operate(/, T, MOI.SingleVariable(vis[1]), s2)
    u = MOIU.operate(/, T, MOI.SingleVariable(vis[2]), s2)
    push!(umap, bridge.variables[1] => MOIU.operate(+, T, t, u))
    push!(umap, bridge.variables[2] => MOIU.operate(-, T, t, u))
    for i in 3:length(vis)
        func = convert(MOI.ScalarAffineFunction{T}, MOI.SingleVariable(vis[i]))
        push!(umap, bridge.variables[i] => func)
    end
    return umap
end
