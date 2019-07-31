"""
    IndicatorActiveOnFalseBridge{T}

The `IndicatorActiveOnFalseBridge` replaces an indicator constraint activated
on 0 with a variable ``z_0`` with the constraint activated on 1, with a variable ``z_1``.
It stores the added `variable_index` and added constraints:
- ``z_1 \\in \\mathbb{B}`` in `zero_one_cons`
- ``z_0 + z_1 == 1`` in `` in `disjunction_cons`
- The added `ACTIVATE_ON_ONE` indicator constraint in `indicator_cons_index`.
"""
struct IndicatorActiveOnFalseBridge{T, F <: MOI.AbstractVectorFunction, S <: MOI.AbstractScalarSet} <: AbstractBridge
    variable_index::MOI.VariableIndex
    zero_one_cons::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}
    disjunction_cons::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}
    indicator_cons_index::MOI.ConstraintIndex{F, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, S}}
end

function bridge_constraint(::Type{IndicatorActiveOnFalseBridge{T,F,S}}, model::MOI.ModelLike, f::MOI.VectorAffineFunction{T}, s::IS) where {S <: MOI.AbstractScalarSet, T <: Real, F, IS <: MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, S}}
    z1 = f.terms[1].scalar_term.variable_index
    z2 = MOI.add_variable(model)
    zo_cons = MOI.add_constraint(model, MOI.SingleVariable(z2), MOI.ZeroOne())

    # z1 + z2 == 1
    dcons = MOI.add_constraint(model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), z1), MOI.ScalarAffineTerm(one(T), z2)], zero(T),
        ),
        MOI.EqualTo(one(T)),
    )
    vec_terms2 = [t for t in f.terms if t.output_index == 2]
    f2 = MOI.VectorAffineFunction(
        vcat(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2))],
            vec_terms2,
        ),
        f.constants
    )
    ci = MOI.add_constraint(model, f2, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(s.set))
    return IndicatorActiveOnFalseBridge{T,F,S}(z2, zo_cons, dcons, ci)
end

function MOI.supports_constraint(::Type{<:IndicatorActiveOnFalseBridge{T}},
                                 ::Type{<:MOI.VectorAffineFunction},
                                 ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}}) where {T}
    return true
end

function MOIB.added_constraint_types(::Type{IndicatorActiveOnFalseBridge{T, F, S}}) where {T, F, S}
    return [(F, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, S})]
end

function concrete_bridge_type(::Type{<:IndicatorActiveOnFalseBridge{T}},
                              ::Type{F},
                              ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, S}}) where {T, F<:MOI.VectorAffineFunction, S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{T, F, S}
end

function concrete_bridge_type(::Type{<:IndicatorActiveOnFalseBridge},
                              ::Type{F},
                              ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, S}}) where {F<:MOI.VectorAffineFunction, S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{Float64, F, S}
end
