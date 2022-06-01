# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IndicatorActiveOnFalseBridge{T,F,S} <: Bridges.Constraint.AbstractBridge

`IndicatorActiveOnFalseBridge` implements the following reformulation:

  * ``!z \\implies {f(x) \\in S}`` into ``y \\implies {f(x) \\in S}``,
    ``z + y == 1``, and ``y \\in \\{0, \\}``

## Source node

`IndicatorActiveOnFalseBridge` supports:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{MOI.ACTIVATE_ON_ZERO,S}`](@ref)

## Target nodes

`IndicatorActiveOnFalseBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{MOI.ACTIVATE_ON_ONE,S}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)

"""
struct IndicatorActiveOnFalseBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    S<:MOI.AbstractScalarSet,
} <: AbstractBridge
    variable::MOI.VariableIndex
    zero_one_cons::MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}
    disjunction_cons::MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    }
    indicator_cons_index::MOI.ConstraintIndex{
        F,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,S},
    }
end

const IndicatorActiveOnFalse{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IndicatorActiveOnFalseBridge{T},OT}

function bridge_constraint(
    ::Type{IndicatorActiveOnFalseBridge{T,F,S}},
    model::MOI.ModelLike,
    f::MOI.VectorAffineFunction{T},
    s::IS,
) where {
    S<:MOI.AbstractScalarSet,
    T<:Real,
    F,
    IS<:MOI.Indicator{MOI.ACTIVATE_ON_ZERO,S},
}
    f_scalars = MOI.Utilities.eachscalar(f)
    z2, zo_cons = MOI.add_constrained_variable(model, MOI.ZeroOne())
    # z1 + z2 == 1
    dcons = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate(+, T, f_scalars[1], z2),
        MOI.EqualTo(one(T)),
    )
    f2 = MOI.Utilities.operate(vcat, T, z2, f_scalars[2])
    ci =
        MOI.add_constraint(model, f2, MOI.Indicator{MOI.ACTIVATE_ON_ONE}(s.set))
    return IndicatorActiveOnFalseBridge{T,F,S}(z2, zo_cons, dcons, ci)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorActiveOnFalseBridge{T}},
    ::Type{<:MOI.VectorAffineFunction},
    ::Type{<:MOI.Indicator{MOI.ACTIVATE_ON_ZERO}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IndicatorActiveOnFalseBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{IndicatorActiveOnFalseBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (F, MOI.Indicator{MOI.ACTIVATE_ON_ONE,S}),
    ]
end

function concrete_bridge_type(
    ::Type{<:IndicatorActiveOnFalseBridge{T}},
    ::Type{F},
    ::Type{MOI.Indicator{MOI.ACTIVATE_ON_ZERO,S}},
) where {T,F<:MOI.VectorAffineFunction,S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{T,F,S}
end

function concrete_bridge_type(
    ::Type{<:IndicatorActiveOnFalseBridge},
    ::Type{F},
    ::Type{MOI.Indicator{MOI.ACTIVATE_ON_ZERO,S}},
) where {F<:MOI.VectorAffineFunction,S<:MOI.AbstractScalarSet}
    return IndicatorActiveOnFalseBridge{Float64,F,S}
end
