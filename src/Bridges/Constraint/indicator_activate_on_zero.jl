"""
    IndicatorActiveOnFalseBridge{T}

The `IndicatorActiveOnFalseBridge` replaces an indicator constraint activated
on 0 with a variable ``z_0`` with the constraint activated on 1, with a variable ``z_1``.
It stores the added `variable` and added constraints:
- ``z_1 \\in \\mathbb{B}`` in `zero_one_cons`
- ``z_0 + z_1 == 1`` in `` in `disjunction_cons`
- The added `ACTIVATE_ON_ONE` indicator constraint in `indicator_cons_index`.
"""
struct IndicatorActiveOnFalseBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    S<:MOI.AbstractScalarSet,
} <: AbstractBridge
    variable::MOI.VariableIndex
    zero_one_cons::MOI.ConstraintIndex{MOI.SingleVariable,MOI.ZeroOne}
    disjunction_cons::MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    }
    indicator_cons_index::MOI.ConstraintIndex{
        F,
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,S},
    }
end

function bridge_constraint(
    ::Type{IndicatorActiveOnFalseBridge{T,F,S}},
    model::MOI.ModelLike,
    f::MOI.VectorAffineFunction{T},
    s::IS,
) where {
    S<:MOI.AbstractScalarSet,
    T<:Real,
    F,
    IS<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO,S},
}
    f_scalars = MOIU.eachscalar(f)
    z2, zo_cons = MOI.add_constrained_variable(model, MOI.ZeroOne())
    # z1 + z2 == 1
    z1_z2 = MOIU.operate(+, T, f_scalars[1], MOI.SingleVariable(z2))
    dcons = MOIU.normalize_and_add_constraint(model, z1_z2, MOI.EqualTo(one(T)))
    f2 = MOIU.operate(vcat, T, MOI.SingleVariable(z2), f_scalars[2])
    ci = MOI.add_constraint(
        model,
        f2,
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(s.set),
    )
    return IndicatorActiveOnFalseBridge{T,F,S}(z2, zo_cons, dcons, ci)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorActiveOnFalseBridge{T}},
    ::Type{<:MOI.VectorAffineFunction},
    ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(
    ::Type{<:IndicatorActiveOnFalseBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOIB.added_constraint_types(
    ::Type{IndicatorActiveOnFalseBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (F, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,S}),
    ]
end

function concrete_bridge_type(
    ::Type{<:IndicatorActiveOnFalseBridge{T}},
    ::Type{F},
    ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO,S}},
) where {T,F<:MOI.VectorAffineFunction,S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{T,F,S}
end

function concrete_bridge_type(
    ::Type{<:IndicatorActiveOnFalseBridge},
    ::Type{F},
    ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO,S}},
) where {F<:MOI.VectorAffineFunction,S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{Float64,F,S}
end
