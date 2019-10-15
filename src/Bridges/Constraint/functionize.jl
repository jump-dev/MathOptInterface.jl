# scalar version

"""
    ScalarFunctionizeBridge{T, S}

The `ScalarFunctionizeBridge` converts a constraint `SingleVariable`-in-`S`
into the constraint `ScalarAffineFunction{T}`-in-`S`.
"""
struct ScalarFunctionizeBridge{T, S} <: AbstractFunctionConversionBridge
    constraint::CI{MOI.ScalarAffineFunction{T}, S}
end
function bridge_constraint(::Type{ScalarFunctionizeBridge{T, S}}, model,
                           f::MOI.SingleVariable, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(f), s)
    return ScalarFunctionizeBridge{T, S}(constraint)
end

# start allowing everything (scalar)
MOI.supports_constraint(::Type{ScalarFunctionizeBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = true
MOIB.added_constrained_variable_types(::Type{<:ScalarFunctionizeBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{ScalarFunctionizeBridge{T, S}}) where {T, S}
    return [(MOI.ScalarAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:ScalarFunctionizeBridge{T}},
                              ::Type{MOI.SingleVariable},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    return ScalarFunctionizeBridge{T, S}
end

# Attributes, Bridge acting as a model
MOI.get(b::ScalarFunctionizeBridge{T, S}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::ScalarFunctionizeBridge{T, S}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, c::ScalarFunctionizeBridge)
    MOI.delete(model, c.constraint)
    return
end

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::ScalarFunctionizeBridge{T}, f::MOI.SingleVariable) where {T}
    MOI.set(model, MOI.ConstraintFunction(), c.constraint, MOI.ScalarAffineFunction{T}(f))
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::ScalarFunctionizeBridge)
    f = MOIU.canonical(MOI.get(model, attr, b.constraint))
    return convert(MOI.SingleVariable, f)
end

# vector version

"""
    VectorFunctionizeBridge{T, S}

The `VectorFunctionizeBridge` converts a constraint `VectorOfVariables`-in-`S`
into the constraint `VectorAffineFunction{T}`-in-`S`.
"""
mutable struct VectorFunctionizeBridge{T, S} <: AbstractFunctionConversionBridge
    constraint::CI{MOI.VectorAffineFunction{T}, S}
end
function bridge_constraint(::Type{VectorFunctionizeBridge{T, S}}, model,
                           f::MOI.VectorOfVariables, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.VectorAffineFunction{T}(f), s)
    return VectorFunctionizeBridge{T, S}(constraint)
end

MOI.supports_constraint(::Type{VectorFunctionizeBridge{T}},
                        ::Type{MOI.VectorOfVariables},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = true
MOIB.added_constrained_variable_types(::Type{<:VectorFunctionizeBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{VectorFunctionizeBridge{T, S}}) where {T, S}
    return [(MOI.VectorAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:VectorFunctionizeBridge{T}},
                              ::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOI.AbstractVectorSet}) where T
    return VectorFunctionizeBridge{T, S}
end

# Attributes, Bridge acting as a model
MOI.get(b::VectorFunctionizeBridge{T, S},
        ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::VectorFunctionizeBridge{T, S},
        ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::VectorFunctionizeBridge)
    MOI.delete(model, bridge.constraint)
    return
end
function MOI.delete(model::MOI.ModelLike, bridge::VectorFunctionizeBridge,
                    i::IndexInVector)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOIU.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.constraint)
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
end

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 bridge::VectorFunctionizeBridge{T},
                 func::MOI.VectorOfVariables) where {T}
    MOI.set(model, MOI.ConstraintFunction(), bridge.constraint,
            MOI.VectorAffineFunction{T}(func))
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::VectorFunctionizeBridge)
    f = MOI.get(model, attr, b.constraint)
    return MOIU.convert_approx(MOI.VectorOfVariables, f)
end
