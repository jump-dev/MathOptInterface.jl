# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    NumberConversionBridge{T,F1,S1,F2,S2} <: Bridges.Constraint.AbstractBridge

`NumberConversionBridge` implements the following reformulation:

  * ``f1(x) \\in S1`` to ``f2(x) \\in S2``

where `f` and `S` are the same functional form, but differ in their coefficient
type.

## Source node

`NumberConversionBridge` supports:

  * `F1` in `S1`

## Target node

`NumberConversionBridge` creates:

  * `F2` in `S2`
"""
struct NumberConversionBridge{T,F1,S1,F2,S2} <: AbstractBridge
    constraint::MOI.ConstraintIndex{F2,S2}
end

const NumberConversion{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NumberConversionBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:NumberConversionBridge{T}},
    ::Type{F1},
    ::Type{S1},
) where {T,F1<:MOI.AbstractFunction,S1<:MOI.AbstractSet}
    F2 = MOI.Utilities.similar_type(F1, T)
    S2 = MOI.Utilities.similar_type(S1, T)
    return NumberConversionBridge{T,F1,S1,F2,S2}
end

function bridge_constraint(
    ::Type{NumberConversionBridge{T,F1,S1,F2,S2}},
    model::MOI.ModelLike,
    f::F1,
    set::S1,
) where {T,F1,S1,F2,S2}
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.convert_approx(F2, f),
        MOI.Utilities.convert_approx(S2, set),
    )
    return NumberConversionBridge{T,F1,S1,F2,S2}(ci)
end

function MOI.supports_constraint(
    ::Type{NumberConversionBridge{T}},
    ::Type{F1},
    ::Type{S1},
) where {T,F1<:MOI.AbstractFunction,S1<:MOI.AbstractSet}
    return F1 != MOI.Utilities.similar_type(F1, T) ||
           S1 != MOI.Utilities.similar_type(S1, T)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:NumberConversionBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    A::Type{NumberConversionBridge{T,F1,S1,F2,S2}},
) where {T,F1,S1,F2,S2}
    return Tuple{Type,Type}[(F2, S2)]
end

function MOI.get(
    bridge::NumberConversionBridge{T,F1,S1,F2,S2},
    ::MOI.NumberOfConstraints{F2,S2},
)::Int64 where {T,F1,S1,F2,S2}
    return 1
end

function MOI.get(
    bridge::NumberConversionBridge{T,F1,S1,F2,S2},
    ::MOI.ListOfConstraintIndices{F2,S2},
) where {T,F1,S1,F2,S2}
    return [bridge.constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::NumberConversionBridge)
    MOI.delete(model, bridge.constraint)
    return
end

MOI.get(b::NumberConversionBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(b::NumberConversionBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::NumberConversionBridge{T,F1,S1,F2,S2},
) where {T,F1,S1,F2,S2}
    f = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    return MOI.Utilities.convert_approx(F1, f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::NumberConversionBridge{T,F1,S1,F2,S2},
) where {T,F1,S1,F2,S2}
    s = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    return MOI.Utilities.convert_approx(S1, s)
end
