"""
    VectorizeBridge{T, S}

Transforms a constrained variable in `scalar_set_type(S, T)` where
`S <: VectorLinearSet` into a constrained vector of one variable in `S`. For
instance, `VectorizeBridge{Float64, MOI.Nonnegatives}` transforms a constrained
variable in `MOI.GreaterThan{Float64}` into a constrained vector of one
variable in `MOI.Nonnegatives`.
"""
mutable struct VectorizeBridge{T, S} <: AbstractBridge
    variable::MOI.VariableIndex
    vector_constraint::MOI.ConstraintIndex{MOI.VectorOfVariables, S}
    set_constant::T # constant in scalar set
end
function bridge_constrained_variable(
    ::Type{VectorizeBridge{T, S}},
    model::MOI.ModelLike, set::MOIU.ScalarLinearSet{T}) where {T, S}
    set_constant = MOI.constant(set)
    variables, vector_constraint = MOI.add_constrained_variables(model, S(1))
    return VectorizeBridge{T, S}(variables[1], vector_constraint, set_constant)
end

function supports_constrained_variable(
    ::Type{VectorizeBridge{T}}, ::Type{<:MOIU.ScalarLinearSet{T}}) where T
    return true
end
function MOIB.added_constrained_variable_types(::Type{VectorizeBridge{T, S}}) where {T, S}
    return [(S,)]
end
function MOIB.added_constraint_types(::Type{<:VectorizeBridge})
    return Tuple{DataType, DataType}[]
end
function concrete_bridge_type(::Type{<:VectorizeBridge{T}},
                              S::Type{<:MOIU.ScalarLinearSet{T}}) where T
    return VectorizeBridge{T, MOIU.vector_set_type(S)}
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::VectorizeBridge, ::MOI.NumberOfVariables)
    return 1
end
function MOI.get(bridge::VectorizeBridge, ::MOI.ListOfVariableIndices)
    return [bridge.variable]
end
function MOI.get(::VectorizeBridge{T, S},
                 ::MOI.NumberOfConstraints{MOI.VectorOfVariables, S}) where {T, S}
    return 1
end
function MOI.get(bridge::VectorizeBridge{T, S},
                 ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables, S}) where {T, S}
    return [bridge.vector_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    MOI.delete(model, bridge.variable)
end

# Attributes, Bridge acting as a constraint

function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::VectorizeBridge{T, S}) where {T, S}
    return MOIU.scalar_set_type(S, T)(bridge.set_constant)
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::VectorizeBridge, new_set::MOIU.ScalarLinearSet)
    # This would require modifing any constraint which uses the bridged
    # variable.
    throw(MOI.SetAttributeNotAllowed(attr,
        "The variable `$(bridge.variable)` is bridged by the `VectorizeBridge`."))
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::VectorizeBridge)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    y = x[1]
    if !MOIU.is_ray(MOI.get(model, MOI.PrimalStatus(attr.result_index)))
       # If it is an infeasibility certificate, it is a ray and satisfies the
       # homogenized problem, see https://github.com/JuliaOpt/MathOptInterface.jl/issues/433
       # Otherwise, we need to add the set constant since the ConstraintPrimal
       # is defined as the value of the function and the set_constant was
       # removed from the original function
       y += bridge.set_constant
   end
   return y
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::VectorizeBridge)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    return x[1]
end

function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.VariablePrimal, MOI.VariablePrimalStart},
                 bridge::VectorizeBridge)
    value = MOI.get(model, attr, bridge.variable)
    if !(attr isa MOI.VariablePrimal &&
         MOIU.is_ray(MOI.get(model, MOI.PrimalStatus(attr.result_index))))
        value += bridge.set_constant
    end
    return value
end
function MOI.supports(model::MOI.ModelLike, attr::MOI.VariablePrimalStart,
                      ::Type{<:VectorizeBridge})
    return MOI.supports(model, attr, MOI.VariableIndex)
end
function MOI.set(model::MOI.ModelLike, attr::MOI.VariablePrimalStart,
                 bridge::VectorizeBridge, value)
    MOI.set(model, attr, bridge.variable, value - bridge.set_constant)
end

function MOIB.bridged_function(bridge::VectorizeBridge{T}) where T
    func = MOI.SingleVariable(bridge.variable)
    return MOIU.operate(+, T, func, bridge.set_constant)
end
function unbridged_map(bridge::VectorizeBridge{T}, vi::MOI.VariableIndex) where T
    func = MOIU.operate(-, T, MOI.SingleVariable(vi),
                        bridge.set_constant)
    return (bridge.variable => func,)
end
