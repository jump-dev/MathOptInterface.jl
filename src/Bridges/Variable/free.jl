"""
    FreeBridge{T} <: Bridges.Variable.AbstractBridge

Transforms constrained variables in [`MathOptInterface.Reals`](@ref)
to the sum of constrained variables in [`MathOptInterface.Nonnegatives`](@ref)
and constrained variables in [`MathOptInterface.Nonpositives`](@ref).
"""
struct FreeBridge{T} <: AbstractBridge
    nonneg_variables::Vector{MOI.VariableIndex}
    nonneg_constraint::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Nonnegatives}
    nonpos_variables::Vector{MOI.VariableIndex}
    nonpos_constraint::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Nonpositives}
end
function bridge_constrained_variable(::Type{FreeBridge{T}},
                                     model::MOI.ModelLike,
                                     set::MOI.Reals) where T
    nonneg_variables, nonneg_constraint = MOI.add_constrained_variables(
        model, MOI.Nonnegatives(MOI.dimension(set)))
    nonpos_variables, nonpos_constraint = MOI.add_constrained_variables(
        model, MOI.Nonpositives(MOI.dimension(set)))
    return FreeBridge{T}(nonneg_variables, nonneg_constraint,
                         nonpos_variables, nonpos_constraint)
end

function supports_constrained_variable(
    ::Type{<:FreeBridge}, ::Type{MOI.Reals})
    return true
end
function MOIB.added_constrained_variable_types(::Type{<:FreeBridge})
    return [(MOI.Nonnegatives,), (MOI.Nonpositives,)]
end
function MOIB.added_constraint_types(::Type{FreeBridge{T}}) where T
    return Tuple{DataType, DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::FreeBridge, ::MOI.NumberOfVariables)
    return length(bridge.nonneg_variables) + length(bridge.nonpos_variables)
end
function MOI.get(bridge::FreeBridge, ::MOI.ListOfVariableIndices)
    return vcat(bridge.nonneg_variables, bridge.nonpos_variables)
end
function MOI.get(bridge::FreeBridge,
                 ::MOI.NumberOfConstraints{MOI.VectorOfVariables,
                                           MOI.Nonnegatives})
    return 1
end
function MOI.get(bridge::FreeBridge,
                 ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,
                                               MOI.Nonnegatives})
    return [bridge.nonneg_constraint]
end
function MOI.get(bridge::FreeBridge,
                 ::MOI.NumberOfConstraints{MOI.VectorOfVariables,
                                           MOI.Nonpositives})
    return 1
end
function MOI.get(bridge::FreeBridge,
                 ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,
                                               MOI.Nonpositives})
    return [bridge.nonpos_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::FreeBridge)
    MOI.delete(model, bridge.nonneg_variables)
    MOI.delete(model, bridge.nonpos_variables)
end

function MOI.delete(model::MOI.ModelLike, bridge::FreeBridge, i::IndexInVector)
    MOI.delete(model, bridge.nonneg_variables[i.value])
    deleteat!(bridge.nonneg_variables, i.value)
    MOI.delete(model, bridge.nonpos_variables[i.value])
    deleteat!(bridge.nonpos_variables, i.value)
end


# Attributes, Bridge acting as a constraint

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::FreeBridge{T}) where T
    return MOI.get(model, attr, bridge.nonneg_constraint) +
        MOI.get(model, attr, bridge.nonpos_constraint)
end
# The transformation is x_free = [I I] * [x_nonneg; x_nonpos]
# so the transformation of the dual is
# [y_nonneg; y_nonpos] = [I; I] * y_free
# that is
# y_nonneg = y_nonpos = y_free
# We can therefore take either of them, let's take y_nonneg.
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::FreeBridge{T}) where T
    return MOI.get(model, attr, bridge.nonneg_constraint)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.VariablePrimal,
                 bridge::FreeBridge{T}, i::IndexInVector) where T
    return MOI.get(model, attr, bridge.nonneg_variables[i.value]) +
        MOI.get(model, attr, bridge.nonpos_variables[i.value])
end

function MOIB.bridged_function(bridge::FreeBridge{T}, i::IndexInVector) where T
    return MOIU.operate(+, T, MOI.SingleVariable(bridge.nonneg_variables[i.value]),
                        MOI.SingleVariable(bridge.nonpos_variables[i.value]))
end
# x_free has been replaced by x_nonneg + x_nonpos.
# To undo it we replace x_nonneg by x_free and x_nonpos by 0.
function unbridged_map(bridge::FreeBridge{T}, vi::MOI.VariableIndex,
                       i::IndexInVector) where T
    sv = MOI.SingleVariable(vi)
    func = convert(MOI.ScalarAffineFunction{T}, sv)
    return bridge.nonneg_variables[i.value] => func,
        bridge.nonpos_variables[i.value] => zero(MOI.ScalarAffineFunction{T})
end

function MOI.set(model::MOI.ModelLike, attr::MOI.VariablePrimalStart,
                 bridge::FreeBridge, value, i::IndexInVector)
    if value < 0
        nonneg = zero(value)
        nonpos = value
    else
        nonneg = value
        nonpos = zero(value)
    end
    MOI.set(model, attr, bridge.nonneg_variables[i.value], nonneg)
    MOI.set(model, attr, bridge.nonpos_variables[i.value], nonpos)
end
