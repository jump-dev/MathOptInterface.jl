"""
    IndicatorSOS1Bridge{T,S<:MOI.AbstractScalarSet}

The `IndicatorSOS1Bridge` replaces an indicator constraint of the following
form:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) \\in S`` with a SOS1 constraint:
``z \\in \\mathbb{B}, slack \\text{ free}, f(x) + slack \\in S, SOS1(slack, z)``.
"""
struct IndicatorSOS1Bridge{T,S<:MOI.AbstractScalarSet} <: AbstractBridge
    slack::MOI.VariableIndex
    z::MOI.VariableIndex
    sos_index::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SOS1{T}}
    affine_index::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}
end

function bridge_constraint(
    ::Type{IndicatorSOS1Bridge{T,S}},
    model::MOI.ModelLike,
    f::MOI.VectorAffineFunction{T},
    s::MOI.Indicator{MOI.ACTIVATE_ON_ONE,S},
) where {T<:Real,S}
    f_scalars = MOI.Utilities.eachscalar(f)
    z = convert(MOI.VariableIndex, f_scalars[1])
    slack = MOI.add_variable(model)
    sos_index = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([slack, z]),
        MOI.SOS1{T}([0.4, 0.6]),  # This weight vector is arbitrary!
    )
    new_f = MOI.Utilities.operate(+, T, f_scalars[2], slack)
    affine_index = MOI.add_constraint(model, new_f, s.set)
    return IndicatorSOS1Bridge{T,S}(slack, z, sos_index, affine_index)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorSOS1Bridge},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.Indicator{MOI.ACTIVATE_ON_ONE,<:MOI.AbstractScalarSet}},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    b::IndicatorSOS1Bridge,
)
    set = MOI.get(model, attr, b.affine_index)
    return MOI.Indicator{MOI.ACTIVATE_ON_ONE}(set)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    b::IndicatorSOS1Bridge{T},
) where {T}
    f = MOI.get(model, attr, b.affine_index)
    terms = MOI.VectorAffineTerm{T}[
        MOI.VectorAffineTerm(2, t) for t in f.terms if t.variable != b.slack
    ]
    push!(terms, MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(one(T), b.z)))
    return MOI.VectorAffineFunction(terms, [zero(T), f.constant])
end

function MOI.delete(model::MOI.ModelLike, bridge::IndicatorSOS1Bridge)
    MOI.delete(model, bridge.sos_index)
    MOI.delete(model, bridge.affine_index)
    MOI.delete(model, bridge.slack)
    return
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IndicatorSOS1Bridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:IndicatorSOS1Bridge{T,S}},
) where {T,S}
    return Tuple{Type,Type}[
        (MOI.VectorOfVariables, MOI.SOS1{T}),
        (MOI.ScalarAffineFunction{T}, S),
    ]
end

function concrete_bridge_type(
    ::Type{<:IndicatorSOS1Bridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Indicator{MOI.ACTIVATE_ON_ONE,S}},
) where {T,S}
    return IndicatorSOS1Bridge{T,S}
end

MOI.get(::IndicatorSOS1Bridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(b::IndicatorSOS1Bridge, ::MOI.ListOfVariableIndices)
    return [b.slack]
end

function MOI.get(
    ::IndicatorSOS1Bridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,<:MOI.SOS1},
)::Int64
    return 1
end

function MOI.get(
    ::IndicatorSOS1Bridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    b::IndicatorSOS1Bridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,<:MOI.SOS1},
) where {T}
    return [b.sos_index]
end

function MOI.get(
    b::IndicatorSOS1Bridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    return [b.affine_index]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::IndicatorSOS1Bridge,
)
    z = MOI.get(model, MOI.VariablePrimal(attr.result_index), bridge.z)
    w = MOI.get(model, MOI.VariablePrimal(attr.result_index), bridge.slack)
    f = MOI.get(model, attr, bridge.affine_index)
    return [z, f - w]
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    ::Type{IndicatorSOS1Bridge{T,S}},
) where {T,S}
    ci = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}
    return MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex) &&
           MOI.supports(model, attr, ci)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IndicatorSOS1Bridge,
)
    z = MOI.get(model, MOI.VariablePrimalStart(), bridge.z)
    w = MOI.get(model, MOI.VariablePrimalStart(), bridge.slack)
    f = MOI.get(model, attr, bridge.affine_index)
    return [z, f - w]
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IndicatorSOS1Bridge{T},
    value::AbstractVector,
) where {T}
    @assert length(value) == 2
    MOI.set(model, MOI.VariablePrimalStart(), bridge.z, value[1])
    w = something(
        MOI.get(model, MOI.VariablePrimalStart(), bridge.slack),
        zero(T),
    )
    MOI.set(model, attr, bridge.affine_index, value[2] + w)
    return
end
