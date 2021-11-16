"""
    ScalarizeBridge{T, F, S}

Transforms a constraint `AbstractVectorFunction`-in-`vector_set_type(S)` where
`S <: LPCone{T}` to `F`-in-`S`.
"""
mutable struct ScalarizeBridge{T,F,S} <: AbstractBridge
    scalar_constraints::Vector{MOI.ConstraintIndex{F,S}}
    constants::Vector{T}
end

function bridge_constraint(
    ::Type{ScalarizeBridge{T,F,S}},
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
    set::MOI.Utilities.VectorLinearSet,
) where {T,F,S}
    dimension = MOI.output_dimension(f)
    constants = MOI.constant(f, T)
    new_f = MOI.Utilities.scalarize(f, true)
    constraints = Vector{MOI.ConstraintIndex{F,S}}(undef, dimension)
    for i in 1:dimension
        constraints[i] = MOI.add_constraint(model, new_f[i], S(-constants[i]))
    end
    return ScalarizeBridge{T,F,S}(constraints, constants)
end

function MOI.supports_constraint(
    ::Type{ScalarizeBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    # If `F` is `MOI.VectorAffineFunction{Complex{Float64}}`, `S` is `MOI.Zeros`
    # and `T` is `Float64`, it would create a set `MOI.EqualTo{Float64}` which
    # is incorrect hence we say we only support it if the coefficient type of
    # `F` is `T`.
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:ScalarizeBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ScalarizeBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function concrete_bridge_type(
    ::Type{<:ScalarizeBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    S::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    return ScalarizeBridge{
        T,
        MOI.Utilities.scalar_type(F),
        MOI.Utilities.scalar_set_type(S, T),
    }
end

function MOI.get(
    bridge::ScalarizeBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {T,F,S}
    return length(bridge.scalar_constraints)
end

function MOI.get(
    bridge::ScalarizeBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return copy(bridge.scalar_constraints)
end

function MOI.delete(model::MOI.ModelLike, bridge::ScalarizeBridge)
    for ci in bridge.scalar_constraints
        MOI.delete(model, ci)
    end
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge,
    i::MOI.Bridges.IndexInVector,
)
    MOI.delete(model, bridge.scalar_constraints[i.value])
    deleteat!(bridge.scalar_constraints, i.value)
    deleteat!(bridge.constants, i.value)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T},
) where {T}
    func = MOI.Utilities.vectorize(
        MOI.get.(model, attr, bridge.scalar_constraints),
    )
    if !(func isa MOI.VectorOfVariables)
        # `func` is in terms of bridged variables here while in
        # `bridge_constraint` it was in terms of the solver variables so
        # `MOI.constant(set)` might be different than `bridge.constants[i]`.
        for i in eachindex(bridge.scalar_constraints)
            set = MOI.get(
                model,
                MOI.ConstraintSet(),
                bridge.scalar_constraints[i],
            )
            func = MOI.Utilities.operate_output_index!(
                -,
                T,
                i,
                func,
                MOI.constant(set),
            )
        end
    end
    return func
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ScalarizeBridge{T,F,S},
) where {T,F,S}
    return MOI.Utilities.vector_set_type(S)(length(bridge.scalar_constraints))
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{ScalarizeBridge{T,F,S}},
) where {T,F,S}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::ScalarizeBridge,
)
    return MOI.get.(model, attr, bridge.scalar_constraints) .+ bridge.constants
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
)
    values = MOI.get.(model, attr, bridge.scalar_constraints)
    if any(value -> value === nothing, values)
        return
    end
    return values .+ bridge.constants
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
    ::Nothing,
)
    for ci in bridge.scalar_constraints
        MOI.set(model, attr, ci, nothing)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
    value,
)
    MOI.set.(model, attr, bridge.scalar_constraints, value .- bridge.constants)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::ScalarizeBridge,
)
    return MOI.get.(model, attr, bridge.scalar_constraints)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::ScalarizeBridge,
)
    values = MOI.get.(model, attr, bridge.scalar_constraints)
    if any(value -> value === nothing, values)
        return
    end
    return values
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::ScalarizeBridge,
    value,
)
    MOI.set.(model, attr, bridge.scalar_constraints, value)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge{T,F,S},
    change::MOI.VectorConstantChange{T},
) where {T,F,S}
    bridge.constants = change.new_constant
    MOI.set.(
        model,
        MOI.ConstraintSet(),
        bridge.scalar_constraints,
        S.(-change.new_constant),
    )
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge,
    change::MOI.MultirowChange{T},
) where {T}
    for (index, value) in change.new_coefficients
        MOI.modify(
            model,
            bridge.scalar_constraints[index],
            MOI.ScalarCoefficientChange{T}(change.variable, value),
        )
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T,F,S},
    func,
) where {T,F,S}
    old_constants = bridge.constants
    bridge.constants = MOI.constant(func, T)
    new_func = MOI.Utilities.scalarize(func, true)
    MOI.set.(
        model,
        MOI.ConstraintFunction(),
        bridge.scalar_constraints,
        new_func,
    )
    for i in eachindex(bridge.constants)
        if bridge.constants[i] != old_constants[i]
            MOI.set(
                model,
                MOI.ConstraintSet(),
                bridge.scalar_constraints[i],
                S(-bridge.constants[i]),
            )
        end
    end
    return
end

# TODO implement transform
