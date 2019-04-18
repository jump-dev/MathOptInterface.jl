
"""
    IndicatorActiveOnFalseBridge{T}

The `IndicatorActiveOnFalseBridge` replaces an indicator constraint activated on 0
with the constraint activated on 1, creating an additional variable.
"""
struct IndicatorActiveOnFalseBridge{F <: MOI.AbstractScalarFunction, S <: MOI.AbstractScalarSet, T <: Real} <: AbstractBridge
    variable_index::MOI.VariableIndex
    zero_one_cons::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}
    disjunction_cons::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}
    indicator_cons_index::MOI.ConstraintIndex{F, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, S}}
end

function IndicatorActiveOnFalseBridge(model::MOI.ModelLike, f::MOI.ScalarAffineFunction{T}, s::IS) where {T <: Real, F, IS <: MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, S} where S <: MOI.AbstractScalarSet}
    z1 = f.terms[1].variable_index

    z2 = MOI.add_variable(model)
    zo_cons = MOI.add_constraint(model, MOI.SingleVariable(z2), MOI.ZeroOne())

    # z1 + z2 == 1
    dcons = MOI.add_constraint(model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), z1), MOI.ScalarAffineTerm(one(T), z2)], zero(T),
        ),
        MOI.EqualTo(one(T)),
    )

    ci = MOI.add_constraint(model, f, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(s.set))

    return IndicatorActiveOnFalseBridge{MOI.ScalarAffineFunction{T}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, IS}, T}(z2, zo_cons, dcons, ci)
end

function MOI.supports_constraint(::Type{<:IndicatorActiveOnFalseBridge},
                                 ::Type{<:MOI.AbstractScalarFunction},
                                 ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}})
    return true
end

function added_constraint_types(::Type{IndicatorActiveOnFalseBridge{F, S, T}}) where {F, S, T}
    return [(F, MOI.IndicatorConstraint{MOI.ACTIVATE_ON_ONE, S})]
end

function concrete_bridge_type(::Type{IndicatorActiveOnFalseBridge{F, S, T}},
                              ::Type{F},
                              ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, S}}) where {F, S, T}
    return IndicatorActiveOnFalseBridge{F, S, T}
end
