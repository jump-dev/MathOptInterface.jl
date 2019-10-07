"""
    ScalarizeBridge{T, F, S}

Transforms a constraint `AbstractVectorFunction`-in-`vector_set_type(S)` where
`S <: LPCone{T}` to `F`-in-`S`.
"""
mutable struct ScalarizeBridge{T, F, S} <: AbstractBridge
    scalar_constraints::Vector{CI{F, S}}
    constants::Vector{T}
end
function bridge_constraint(::Type{ScalarizeBridge{T, F, S}},
                           model::MOI.ModelLike,
                           f::MOI.AbstractVectorFunction,
                           set::MOIU.VectorLinearSet) where {T, F, S}
    dimension = MOI.output_dimension(f)
    constants = MOI.constant(f, T)
    new_f = MOIU.scalarize(f, true)
    constraints = Vector{CI{F, S}}(undef, dimension)
    for i in 1:dimension
        constraints[i] = MOI.add_constraint(model, new_f[i], S(-constants[i]))
    end
    return ScalarizeBridge{T, F, S}(constraints, constants)
end

function MOI.supports_constraint(::Type{ScalarizeBridge{T}},
                                 ::Type{<:MOI.AbstractVectorFunction},
                                 ::Type{<:MOIU.VectorLinearSet}) where T
    return true
end
MOIB.added_constrained_variable_types(::Type{<:ScalarizeBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{ScalarizeBridge{T, F, S}}) where {T, F, S}
    return [(F, S)]
end
function concrete_bridge_type(::Type{<:ScalarizeBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOIU.VectorLinearSet}) where T
    return ScalarizeBridge{T, MOIU.scalar_type(F), MOIU.scalar_set_type(S, T)}
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::ScalarizeBridge{T, F, S},
                 ::MOI.NumberOfConstraints{F, S}) where {T, F, S}
    return length(bridge.scalar_constraints)
end
function MOI.get(bridge::ScalarizeBridge{T, F, S},
                 ::MOI.ListOfConstraintIndices{F, S}) where {T, F, S}
    return bridge.scalar_constraints
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::ScalarizeBridge)
    for ci in bridge.scalar_constraints
        MOI.delete(model, ci)
    end
end
function MOI.delete(model::MOI.ModelLike, bridge::ScalarizeBridge,
                    i::IndexInVector)
    MOI.delete(model, bridge.scalar_constraints[i.value])
    deleteat!(bridge.scalar_constraints, i.value)
    deleteat!(bridge.constants, i.value)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::ScalarizeBridge{T}) where T
    func = MOIU.vectorize(MOI.get.(model, attr, bridge.scalar_constraints))
    if !(func isa MOI.VectorOfVariables)
        # `func` is in terms of bridged variables here while in
        # `bridge_constraint` it was in terms of the solver variables so
        # `MOI.constant(set)` might be different than `bridge.constants[i]`.
        for i in eachindex(bridge.scalar_constraints)
            set = MOI.get(model, MOI.ConstraintSet(), bridge.scalar_constraints[i])
            func = MOIU.operate_output_index!(-, T, i, func, MOI.constant(set))
        end
    end
    return func
end
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::ScalarizeBridge{T, F, S}) where {T, F, S}
    return MOIU.vector_set_type(S)(length(bridge.scalar_constraints))
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::ScalarizeBridge)
    return MOI.get.(model, attr, bridge.scalar_constraints) .+ bridge.constants
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::ScalarizeBridge)
    return MOI.get.(model, attr, bridge.scalar_constraints)
end
function MOI.modify(model::MOI.ModelLike, bridge::ScalarizeBridge{T,F,S},
    change::MOI.VectorConstantChange{T}) where {T,F,S}
    bridge.constants = change.new_constant
    MOI.set.(model, MOI.ConstraintSet(), bridge.scalar_constraints,
             S.(-change.new_constant))
end
function MOI.modify(model::MOI.ModelLike, bridge::ScalarizeBridge,
                    change::MOI.MultirowChange{T}) where T
    for (index, value) in change.new_coefficients
        MOI.modify(model, bridge.scalar_constraints[index],
               MOI.ScalarCoefficientChange{T}(change.variable, value))
    end
    nothing
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T}, func) where T
    old_constants = bridge.constants
    bridge.constants = MOI.constant(func, T)
    new_func = MOIU.scalarize(func, true)
    MOI.set.(model, MOI.ConstraintFunction(), bridge.scalar_constraints,
             new_func)
    for i in eachindex(bridge.constants)
        if bridge.constants[i] != old_constants[i]
            MOI.set(model, MOI.ConstraintSet(), bridge.scalar_constraints,
                     S(-bridge.constants[i]))
        end
    end
end

# TODO implement transform
