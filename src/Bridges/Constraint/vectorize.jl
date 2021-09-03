"""
    VectorizeBridge{T,F,S,G}

Transforms a constraint `G`-in-`scalar_set_type(S, T)` where
`S <: VectorLinearSet` to `F`-in-`S`.

## Examples

The constraint `VariableIndex`-in-`LessThan{Float64}` becomes
`VectorAffineFunction{Float64}`-in-`Nonpositives`, where `T = Float64`,
`F = VectorAffineFunction{Float64}`, `S = Nonpositives`, and
`G = VariableIndex`.
"""
mutable struct VectorizeBridge{T,F,S,G} <: AbstractBridge
    vector_constraint::MOI.ConstraintIndex{F,S}
    set_constant::T # constant in scalar set
end

function bridge_constraint(
    ::Type{VectorizeBridge{T,F,S,G}},
    model::MOI.ModelLike,
    scalar_f::G,
    set::MOI.Utilities.ScalarLinearSet{T},
) where {T,F,S,G}
    scalar_const = MOI.constant(scalar_f, T)
    if !iszero(scalar_const)
        throw(MOI.ScalarFunctionConstantNotZero{T,G,typeof(set)}(scalar_const))
    end
    vector_f = convert(F, scalar_f)
    set_const = MOI.constant(set)
    MOI.Utilities.operate_output_index!(-, T, 1, vector_f, set_const)
    vector_constraint = MOI.add_constraint(model, vector_f, S(1))
    return VectorizeBridge{T,F,S,G}(vector_constraint, set_const)
end

function MOI.supports_constraint(
    ::Type{VectorizeBridge{T}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:VectorizeBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{<:VectorizeBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function concrete_bridge_type(
    ::Type{<:VectorizeBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    S::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T}
    H = MOI.Utilities.promote_operation(-, T, G, T)
    F = MOI.Utilities.promote_operation(vcat, T, H)
    return VectorizeBridge{T,F,MOI.Utilities.vector_set_type(S),G}
end

function MOI.get(
    ::VectorizeBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {T,F,S}
    return 1
end

function MOI.get(
    bridge::VectorizeBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return [bridge.vector_constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    MOI.delete(model, bridge.vector_constraint)
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{VectorizeBridge{T,F,S,G}},
) where {T,F,S,G}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    return x[1] + bridge.set_constant
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    if MOI.Utilities.is_ray(MOI.get(model, MOI.PrimalStatus(attr.result_index)))
        # If it is an infeasibility certificate, it is a ray and satisfies the
        # homogenized problem, see https://github.com/jump-dev/MathOptInterface.jl/issues/433
        return x[1]
    else
        # Otherwise, we need to add the set constant since the ConstraintPrimal
        # is defined as the value of the function and the set_constant was
        # removed from the original function
        return x[1] + bridge.set_constant
    end
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::VectorizeBridge,
    value,
)
    MOI.set(
        model,
        attr,
        bridge.vector_constraint,
        [value - bridge.set_constant],
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    return x[1]
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::VectorizeBridge,
    value,
)
    MOI.set(model, attr, bridge.vector_constraint, [value])
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::VectorizeBridge,
    change::MOI.ScalarCoefficientChange,
)
    MOI.modify(
        model,
        bridge.vector_constraint,
        MOI.MultirowChange(change.variable, [(1, change.new_coefficient)]),
    )
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::VectorizeBridge,
    new_set::MOI.Utilities.ScalarLinearSet,
)
    bridge.set_constant = MOI.constant(new_set)
    MOI.modify(
        model,
        bridge.vector_constraint,
        MOI.VectorConstantChange([-bridge.set_constant]),
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::VectorizeBridge{T,F,S,G},
) where {T,F,S,G}
    f = MOI.Utilities.scalarize(
        MOI.get(model, attr, bridge.vector_constraint),
        true,
    )
    @assert length(f) == 1
    return convert(G, f[1])
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::VectorizeBridge{T,F,S},
) where {T,F,S}
    return MOI.Utilities.scalar_set_type(S, T)(bridge.set_constant)
end
