"""
    IndicatorSOS1Bridge{T, BC <: MOI.AbstractScalarSet}

The `IndicatorSOS1Bridge` replaces an indicator constraint of the following form:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) \\leq b`` with a SOS1 constraint:
``z \\in \\mathbb{B}, w \\leq 0, f(x) + w \\leq b, SOS1(w, z)``.
`GreaterThan` constraints are handled in a symmetric way:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) \\geq b`` is reformulated as:
``z \\in \\mathbb{B}, w \\geq 0, f(x) + w \\geq b, SOS1(w, z)``.
Other scalar sets are handled without a bound constraint:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) == b`` is reformulated as:
``z \\in \\mathbb{B}, w \\text{ free}, f(x) + w == b, SOS1(w, z)``.

If `BC !<: Union{LessThan, GreaterThan}`, `bound_constraint_index` is `nothing`.
"""
struct IndicatorSOS1Bridge{
    T,
    BC<:MOI.AbstractScalarSet,
    MaybeBC<:Union{MOI.ConstraintIndex{MOI.SingleVariable,BC},Nothing},
} <: AbstractBridge
    w_variable::MOI.VariableIndex
    z_variable::MOI.VariableIndex
    affine_func::MOI.ScalarAffineFunction{T}
    bound_constraint_index::MaybeBC
    sos_constraint_index::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SOS1{T}}
    linear_constraint_index::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},BC}
end

function bridge_constraint(
    ::Type{IndicatorSOS1Bridge{T,BC,MaybeBC}},
    model::MOI.ModelLike,
    f::MOI.VectorAffineFunction{T},
    s::MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,BC},
) where {T<:Real,BC,MaybeBC}
    f_scalars = MOIU.eachscalar(f)
    (w, bound_constraint) = _add_bound_constraint!(model, BC)
    z = convert(MOI.SingleVariable, f_scalars[1]).variable
    sos_vector = MOI.VectorOfVariables([w, z])
    sos_constraint =
        MOI.add_constraint(model, sos_vector, MOI.SOS1{T}([0.4, 0.6]))
    affine_func = f_scalars[2]
    affine_expr = MOIU.operate(+, T, affine_func, MOI.SingleVariable(w))
    linear_constraint = MOI.add_constraint(model, affine_expr, s.set)
    return IndicatorSOS1Bridge{T,BC,MaybeBC}(
        w,
        z,
        affine_func,
        bound_constraint,
        sos_constraint,
        linear_constraint,
    )
end

function _add_bound_constraint!(
    model::MOI.ModelLike,
    ::Type{BC},
) where {T<:Real,BC<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}}
    return MOI.add_constrained_variable(model, BC(zero(T)))
end

function _add_bound_constraint!(
    model::MOI.ModelLike,
    ::Type{<:MOI.AbstractScalarSet},
)
    return (MOI.add_variable(model), nothing)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorSOS1Bridge},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,<:MOI.AbstractScalarSet}},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    b::IndicatorSOS1Bridge,
)
    return MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(
        MOI.get(model, attr, b.linear_constraint_index),
    )
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    b::IndicatorSOS1Bridge{T},
) where {T}
    z = b.z_variable
    terms = [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(one(T), z))]
    for affine_term in b.affine_func.terms
        push!(terms, MOI.VectorAffineTerm(2, affine_term))
    end
    return MOI.VectorAffineFunction(terms, [zero(T), b.affine_func.constant])
end

function MOI.delete(model::MOI.ModelLike, bridge::IndicatorSOS1Bridge)
    if bridge.bound_constraint_index !== nothing
        MOI.delete(model, bridge.bound_constraint_index)
    end
    MOI.delete(model, bridge.sos_constraint_index)
    MOI.delete(model, bridge.linear_constraint_index)
    MOI.delete(model, bridge.w_variable)
    return
end

function MOIB.added_constrained_variable_types(
    ::Type{<:IndicatorSOS1Bridge{T,BC}},
) where {T,BC<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}}
    return [(BC,)]
end

function MOIB.added_constrained_variable_types(
    ::Type{<:IndicatorSOS1Bridge{T,BC}},
) where {T,BC}
    return []
end

function MOIB.added_constraint_types(
    ::Type{<:IndicatorSOS1Bridge{T,BC}},
) where {T,BC<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}}
    return [
        (MOI.VectorOfVariables, MOI.SOS1{T}),
        (MOI.ScalarAffineFunction{T}, BC),
    ]
end

function MOIB.added_constraint_types(
    ::Type{<:IndicatorSOS1Bridge{T,S}},
) where {T,S<:MOI.AbstractScalarSet}
    return [
        (MOI.VectorOfVariables, MOI.SOS1{T}),
        (MOI.ScalarAffineFunction{T}, S),
    ]
end

function concrete_bridge_type(
    ::Type{<:IndicatorSOS1Bridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,S}},
) where {T,S<:Union{MOI.LessThan,MOI.GreaterThan}}
    return IndicatorSOS1Bridge{T,S,MOI.ConstraintIndex{MOI.SingleVariable,S}}
end

function concrete_bridge_type(
    ::Type{<:IndicatorSOS1Bridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,S}},
) where {T,S<:MOI.AbstractScalarSet}
    return IndicatorSOS1Bridge{T,S,Nothing}
end

# Attributes, Bridge acting as a model

function MOI.get(::IndicatorSOS1Bridge, ::MOI.NumberOfVariables)
    return Int64(1)
end

function MOI.get(b::IndicatorSOS1Bridge, ::MOI.ListOfVariableIndices)
    return [b.w_variable]
end

function MOI.get(
    ::IndicatorSOS1Bridge{T,BC,Nothing},
    ::MOI.NumberOfConstraints{MOI.SingleVariable,BC},
) where {T,BC}
    return Int64(0)
end

function MOI.get(
    ::IndicatorSOS1Bridge{T,BC,CI},
    ::MOI.NumberOfConstraints{MOI.SingleVariable,BC},
) where {T,BC,CI<:MOI.ConstraintIndex{MOI.SingleVariable,BC}}
    return Int64(1)
end

function MOI.get(
    ::IndicatorSOS1Bridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,<:MOI.SOS1},
)
    return Int64(1)
end

function MOI.get(
    ::IndicatorSOS1Bridge{T,BC},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},BC},
) where {T,BC,CI<:MOI.ConstraintIndex{MOI.SingleVariable,BC}}
    return Int64(1)
end

function MOI.get(
    b::IndicatorSOS1Bridge{T,BC,CI},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable,BC},
) where {T,BC,CI<:MOI.ConstraintIndex}
    return [b.bound_constraint_index]
end

function MOI.get(
    ::IndicatorSOS1Bridge{T,BC,Nothing},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable,BC},
) where {T,BC}
    return MOI.ConstraintIndex{MOI.SingleVariable,BC}[]
end

function MOI.get(
    b::IndicatorSOS1Bridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,<:MOI.SOS1},
) where {T}
    return [b.sos_constraint_index]
end

function MOI.get(
    b::IndicatorSOS1Bridge{T,BC},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},BC},
) where {T,BC}
    return [b.linear_constraint_index]
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:IndicatorSOS1Bridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::IndicatorSOS1Bridge,
)
    zvalue =
        MOI.get(model, MOI.VariablePrimal(attr.result_index), bridge.z_variable)
    wvalue =
        MOI.get(model, MOI.VariablePrimal(attr.result_index), bridge.w_variable)
    lin_primal_start = MOI.get(model, attr, bridge.linear_constraint_index)
    return [zvalue, lin_primal_start - wvalue]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IndicatorSOS1Bridge,
)
    zstart = MOI.get(model, MOI.VariablePrimalStart(), bridge.z_variable)
    wstart = MOI.get(model, MOI.VariablePrimalStart(), bridge.w_variable)
    lin_primal_start = MOI.get(model, attr, bridge.linear_constraint_index)
    return [zstart, lin_primal_start - wstart]
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IndicatorSOS1Bridge{T},
    value,
) where {T}
    zvalue = value[1]
    lin_start = value[2]
    MOI.set(model, MOI.VariablePrimalStart(), bridge.z_variable, zvalue)
    wstart = MOI.get(model, MOI.VariablePrimalStart(), bridge.w_variable)
    wstart = wstart === nothing ? zero(T) : wstart
    MOI.set(model, attr, bridge.linear_constraint_index, lin_start + wstart)
    return
end
