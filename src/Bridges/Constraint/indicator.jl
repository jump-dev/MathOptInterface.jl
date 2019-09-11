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
    f_scalars = MOIU.eachscalar(f)
    z2, zo_cons = MOI.add_constrained_variable(model, MOI.ZeroOne())
    # z1 + z2 == 1
    z1_z2 = MOIU.operate(+, T, f_scalars[1], MOI.SingleVariable(z2))
    dcons = MOIU.normalize_and_add_constraint(model, z1_z2, MOI.EqualTo(one(T)))
    f2 = MOIU.operate(vcat, T, MOI.SingleVariable(z2), f_scalars[2])
    ci = MOI.add_constraint(model, f2, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(s.set))
    return IndicatorActiveOnFalseBridge{T,F,S}(z2, zo_cons, dcons, ci)
end

function MOI.supports_constraint(::Type{<:IndicatorActiveOnFalseBridge{T}},
                                 ::Type{<:MOI.VectorAffineFunction},
                                 ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}}) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:IndicatorActiveOnFalseBridge})
    return [(MOI.ZeroOne,)]
end
function MOIB.added_constraint_types(::Type{IndicatorActiveOnFalseBridge{T, F, S}}) where {T, F, S}
    return [(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
            (F, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, S})]
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

## Indicator constraint by SOS1 bridge

"""
    IndicatorSOS1Bridge{T, BC <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T}}}

The `IndicatorSOS1Bridge` replaces an indicator constraint of the following form:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) \\leq b`` with a SOS1 constraint:
``z \\in \\mathbb{B}, w \\leq 0, f(x) + w \\leq b, SOS1(w, z)``.
`GreaterThan` constraints are handled in a symmetric way:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) \\geq b`` is reformulated as:
``z \\in \\mathbb{B}, w \\geq 0, f(x) + w \\geq b, SOS1(w, z)``.
`EqualTo` constraints are handled without a bound constraint:
``z \\in \\mathbb{B}, z == 1 \\implies f(x) == b`` is reformulated as:
``z \\in \\mathbb{B}, w \\text{ free}, f(x) + w == b, SOS1(w, z)``.

If `BC <: EqualTo`, `bound_constraint_index` is `nothing`.
"""
struct IndicatorSOS1Bridge{T, BC <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T}}, MaybeBC <: Union{MOI.ConstraintIndex{MOI.SingleVariable, BC}, Nothing}} <: AbstractBridge
    w_variable_index::MOI.VariableIndex
    bound_constraint_index::MaybeBC
    sos_constraint_index::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SOS1{T}}
    linear_constraint_index::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, BC}
end

function bridge_constraint(::Type{IndicatorSOS1Bridge{T,BC,MaybeBC}}, model::MOI.ModelLike, f::MOI.VectorAffineFunction{T}, s::MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, BC}) where {T <: Real, BC, MaybeBC}
    f_scalars = MOIU.eachscalar(f)
    (w, bound_constraint) = _add_bound_constraint!(model, BC)
    z = f_scalars[1].terms[1].variable_index
    sos_vector = MOI.VectorOfVariables([w, z])
    sos_constraint = MOI.add_constraint(model, sos_vector, MOI.SOS1{T}([0.4, 0.6]))
    affine_func = copy(f_scalars[2])
    push!(
        affine_func.terms,
        MOI.ScalarAffineTerm{T}(one(T), w)
    )
    linear_constraint = MOI.add_constraint(model, affine_func, s.set)
    return IndicatorSOS1Bridge{T,BC,MaybeBC}(w, bound_constraint, sos_constraint, linear_constraint)
end

function _add_bound_constraint!(model::MOI.ModelLike, ::Type{BC}) where {T <: Real, BC <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}}}
    return MOI.add_constrained_variable(model, BC(zero(T)))
end

function _add_bound_constraint!(model::MOI.ModelLike, ::Type{<:MOI.EqualTo})
    return (MOI.add_variable(model), nothing)
end

function MOI.supports_constraint(::Type{<:IndicatorSOS1Bridge},
                                 ::Type{<:MOI.VectorAffineFunction},
                                 ::Type{<:MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, <: Union{MOI.LessThan, MOI.GreaterThan, MOI.EqualTo}}})
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:IndicatorSOS1Bridge{T, BC}}) where {T, BC <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}}}
    return [(MOI.ZeroOne,), (BC,)]
end

function MOIB.added_constrained_variable_types(::Type{<:IndicatorSOS1Bridge{T, <:MOI.EqualTo}}) where {T}
    return [(MOI.ZeroOne,)]
end

function MOIB.added_constraint_types(::Type{<:IndicatorSOS1Bridge{T, BC}}) where {T, BC <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}}}
    return [(MOI.SingleVariable, BC),
            (MOI.VectorOfVariables, MOI.SOS1{T}),
            (MOI.ScalarAffineFunction{T}, BC),
            ]
end

function MOIB.added_constraint_types(::Type{<:IndicatorSOS1Bridge{T, <:MOI.EqualTo}}) where {T}
    return [(MOI.VectorOfVariables, MOI.SOS1{T}),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
            ]
end

function concrete_bridge_type(::Type{<:IndicatorSOS1Bridge},
                              ::Type{<:MOI.VectorAffineFunction},
                              ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, S}}) where {T, S<:Union{MOI.LessThan{T}, MOI.GreaterThan{T}}}
    return IndicatorSOS1Bridge{T, S, MOI.ConstraintIndex{MOI.SingleVariable, S}}
end

function concrete_bridge_type(::Type{<:IndicatorSOS1Bridge},
                              ::Type{<:MOI.VectorAffineFunction},
                              ::Type{MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, MOI.EqualTo{T}}}) where {T}
    return IndicatorSOS1Bridge{T, MOI.EqualTo{T}, Nothing}
end
