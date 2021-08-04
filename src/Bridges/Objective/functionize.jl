"""
    FunctionizeBridge{T}

The `FunctionizeBridge` converts a `SingleVariable` objective into a
`ScalarAffineFunction{T}` objective.
"""
struct FunctionizeBridge{T} <: AbstractBridge end

function bridge_objective(
    ::Type{FunctionizeBridge{T}},
    model::MOI.ModelLike,
    func::MOI.SingleVariable,
) where {T}
    F = MOI.ScalarAffineFunction{T}
    MOI.set(model, MOI.ObjectiveFunction{F}(), convert(F, func))
    return FunctionizeBridge{T}()
end

function supports_objective_function(
    ::Type{<:FunctionizeBridge},
    ::Type{MOI.SingleVariable},
)
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:FunctionizeBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(::Type{<:FunctionizeBridge})
    return Tuple{Type,Type}[]
end

function MOIB.set_objective_function_type(
    ::Type{FunctionizeBridge{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

# Attributes, Bridge acting as a model
function MOI.get(::FunctionizeBridge, ::MOI.NumberOfVariables)
    return Int64(0)
end

function MOI.get(::FunctionizeBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

# No variables or constraints are created in this bridge so there is nothing to
# delete.
MOI.delete(model::MOI.ModelLike, bridge::FunctionizeBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::FunctionizeBridge,
    ::MOI.OptimizationSense,
)
    # `FunctionizeBridge` is sense agnostic, therefore, we don't need to change
    # anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOIB.ObjectiveFunctionValue{MOI.SingleVariable},
    ::FunctionizeBridge{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    return MOI.get(model, MOIB.ObjectiveFunctionValue{F}(attr.result_index))
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{MOI.SingleVariable},
    ::FunctionizeBridge{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    return convert(MOI.SingleVariable, func)
end
