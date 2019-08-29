"""
    FunctionizeBridge{T}

The `FunctionizeBridge` converts a `SingleVariable` objective into a
`ScalarAffineFunction{T}` objective.
"""
struct FunctionizeBridge{T} <: AbstractBridge end
function bridge_objective(::Type{FunctionizeBridge{T}}, model::MOI.ModelLike,
                           func::MOI.SingleVariable) where T
    F = MOI.ScalarAffineFunction{T}
    MOI.set(model, MOI.ObjectiveFunction{F}(), convert(F, func))
    return FunctionizeBridge{T}()
end

function supports_objective_function(
    ::Type{<:FunctionizeBridge}, ::Type{MOI.SingleVariable})
    return true
end
MOIB.added_constrained_variable_types(::Type{<:FunctionizeBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:FunctionizeBridge})
    return Tuple{DataType, DataType}[]
end
function MOIB.set_objective_function_type(::Type{FunctionizeBridge{T}}) where T
    return MOI.ScalarAffineFunction{T}
end

function MOI.delete(model::MOI.ModelLike, bridge::FunctionizeBridge) end

function MOI.set(::MOI.ModelLike, ::MOI.ObjectiveSense,
                 ::FunctionizeBridge, ::MOI.OptimizationSense)
end
function MOI.get(model::MOI.ModelLike,
                 attr::MOIB.ObjectiveFunctionValue{MOI.SingleVariable},
                 bridge::FunctionizeBridge{T}) where T
    F = MOI.ScalarAffineFunction{T}
    return MOI.get(model, MOIB.ObjectiveFunctionValue{F}())
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ObjectiveFunction{MOI.SingleVariable},
                 bridge::FunctionizeBridge{T}) where T
    F = MOI.ScalarAffineFunction{T}
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    return convert(MOI.SingleVariable, func)
end
