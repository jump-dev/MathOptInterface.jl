# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IndicatorSetMapBridge{T,A,S1,S2} <: Bridges.Constraint.AbstractBridge

`IndicatorSetMapBridgeBridge` implements the following reformulations:

  * ``z \\implies {f(x) \\ge l}`` into ``z \\implies {-f(x) \\le -l}``
  * ``z \\implies {f(x) \\le u}`` into ``z \\implies {-f(x) \\ge -u}``

## Source node

`IndicatorSetMapBridgeBridge` supports:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.Indicator{A,S1}`](@ref)

## Target nodes

`IndicatorSetMapBridgeBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.Indicator{A,S2}`](@ref)
"""
struct IndicatorSetMapBridgeBridge{T,B,S1,S2,A} <: AbstractBridge
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.Indicator{A,S2}}
end

function bridge_constraint(
    ::Type{IndicatorSetMapBridgeBridge{T,B,S1,S2,A}},
    model::MOI.ModelLike,
    func::MOI.VectorAffineFunction{T},
    s::MOI.Indicator{A,S1},
) where {T,B,S1,S2,A}
    f = MOI.Utilities.eachscalar(func)
    f2 = MOI.Bridges.map_function(B, f[2])
    g = MOI.Utilities.operate(vcat, T, f[1], f2)
    s2 = MOI.Bridges.map_set(B, s.set)
    ci = MOI.add_constraint(model, g, MOI.Indicator{A}(s2))
    return IndicatorSetMapBridgeBridge{T,B,S1,S2,A}(ci)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorSetMapBridgeBridge{T,B,S1,S2}},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.Indicator{A,S1}},
) where {T,B,S1,S2,A}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IndicatorSetMapBridgeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{IndicatorSetMapBridgeBridge{T,B,S1,S2,A}},
) where {T,B,S1,S2,A}
    return Tuple{Type,Type}[(MOI.VectorAffineFunction{T}, MOI.Indicator{A,S2})]
end

function concrete_bridge_type(
    ::Type{<:IndicatorSetMapBridgeBridge{T,B,S1,S2}},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.Indicator{A,S1}},
) where {T,B,S1,S2,A}
    return IndicatorSetMapBridgeBridge{T,B,S1,S2,A}
end

function MOI.delete(model::MOI.ModelLike, bridge::IndicatorSetMapBridgeBridge)
    MOI.delete(model, bridge.ci)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::IndicatorSetMapBridgeBridge{T,B,S1,S2,A},
) where {T,B,S1,S2,A}
    set = MOI.get(model, attr, bridge.ci)
    return MOI.Indicator{A}(MOI.Bridges.inverse_map_set(B, set.set))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::IndicatorSetMapBridgeBridge{T,B},
) where {T,B}
    func = MOI.get(model, attr, bridge.ci)
    f = MOI.Utilities.eachscalar(func)
    f2 = MOI.Bridges.inverse_map_function(B, f[2])
    return MOI.Utilities.operate(vcat, T, f[1], f2)
end

function MOI.get(
    ::IndicatorSetMapBridgeBridge{T,B,S1,S2,A},
    ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Indicator{A,S2}},
)::Int64 where {T,B,S1,S2,A}
    return 1
end

function MOI.get(
    bridge::IndicatorSetMapBridgeBridge{T,B,S1,S2,A},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.Indicator{A,S2},
    },
) where {T,B,S1,S2,A}
    return [bridge.ci]
end

"""
    IndicatorGreaterToLessThanBridge{T,A} <: Bridges.Constraint.AbstractBridge

`IndicatorGreaterToLessThanBridge` implements the following reformulation:

  * ``z \\implies {f(x) \\ge l}`` into ``z \\implies {-f(x) \\le -l}``

## Source node

`IndicatorGreaterToLessThanBridge` supports:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{A,MOI.GreaterThan{T}}`](@ref)

## Target nodes

`IndicatorGreaterToLessThanBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{A,MOI.LessThan{T}}`](@ref)
"""
const IndicatorGreaterToLessThanBridge{T,A} = IndicatorSetMapBridgeBridge{
    T,
    MOI.Bridges.Constraint.GreaterToLessBridge{
        T,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarAffineFunction{T},
    },
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    A,
}

const IndicatorGreaterToLessThan{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IndicatorGreaterToLessThanBridge{T},OT}

"""
    IndicatorLessToGreaterThanBridge{T,A} <: Bridges.Constraint.AbstractBridge

`IndicatorLessToGreaterThanBridge` implements the following reformulations:

  * ``z \\implies {f(x) \\le u}`` into ``z \\implies {-f(x) \\ge -u}``

## Source node

`IndicatorLessToGreaterThanBridge` supports:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{A,MOI.LessThan{T}}`](@ref)

## Target nodes

`IndicatorLessToGreaterThanBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in
    [`MOI.Indicator{A,MOI.GreaterThan{T}}`](@ref)
"""
const IndicatorLessToGreaterThanBridge{T,A} = IndicatorSetMapBridgeBridge{
    T,
    MOI.Bridges.Constraint.LessToGreaterBridge{
        T,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarAffineFunction{T},
    },
    MOI.LessThan{T},
    MOI.GreaterThan{T},
    A,
}

const IndicatorLessToGreaterThan{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IndicatorLessToGreaterThanBridge{T},OT}
