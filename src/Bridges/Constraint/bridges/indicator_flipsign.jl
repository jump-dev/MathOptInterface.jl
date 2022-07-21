# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IndicatorFlipSign{T,A,S1,S2} <: Bridges.Constraint.AbstractBridge

`IndicatorFlipSignBridge` implements the following reformulations:

  * ``z \\implies {f(x) \\ge l}`` into ``z \\implies {-f(x) \\le -l}``
  * ``z \\implies {f(x) \\le u}`` into ``z \\implies {-f(x) \\ge -u}``

## Source node

`IndicatorFlipSignBridge` supports:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.Indicator{A,S1}`](@ref)

## Target nodes

`IndicatorFlipSignBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.Indicator{A,S2}`](@ref)
"""
struct IndicatorFlipSignBridge{T,A,S1,S2} <: AbstractBridge
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.Indicator{A,S2}}
end

const IndicatorFlipSign{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IndicatorFlipSignBridge{T},OT}

_indicator_flip_set(s::MOI.LessThan) = MOI.GreaterThan(-s.upper)
_indicator_flip_set(s::MOI.GreaterThan) = MOI.LessThan(-s.lower)
_indicator_flip_set(::Type{MOI.LessThan{T}}) where {T} = MOI.GreaterThan{T}
_indicator_flip_set(::Type{MOI.GreaterThan{T}}) where {T} = MOI.LessThan{T}

function bridge_constraint(
    ::Type{IndicatorFlipSignBridge{T,A,S1,S2}},
    model::MOI.ModelLike,
    func::MOI.VectorAffineFunction{T},
    s::MOI.Indicator{A,S1},
) where {T,A,S1,S2}
    f = MOI.Utilities.eachscalar(func)
    g = MOI.Utilities.operate(vcat, T, f[1], MOI.Utilities.operate(-, T, f[2]))
    s2 = _indicator_flip_set(s.set)
    ci = MOI.add_constraint(model, g, MOI.Indicator{A}(s2))
    return IndicatorFlipSignBridge{T,A,S1,typeof(s2)}(ci)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorFlipSignBridge{T}},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.Indicator{A,S}},
) where {T,A,S<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IndicatorFlipSignBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{IndicatorFlipSignBridge{T,A,S1,S2}},
) where {T,A,S1,S2}
    return Tuple{Type,Type}[(MOI.VectorAffineFunction{T}, MOI.Indicator{A,S2})]
end

function concrete_bridge_type(
    ::Type{<:IndicatorFlipSignBridge{T}},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.Indicator{A,S1}},
) where {T,A,S1}
    S2 = _indicator_flip_set(S1)
    return IndicatorFlipSignBridge{T,A,S1,S2}
end

function MOI.delete(model::MOI.ModelLike, bridge::IndicatorFlipSignBridge)
    MOI.delete(model, bridge.ci)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::IndicatorFlipSignBridge{T,A,S1,S2},
) where {T,A,S1,S2}
    set = MOI.get(model, attr, bridge.ci)
    return MOI.Indicator{A}(_indicator_flip_set(set.set))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::IndicatorFlipSignBridge{T},
) where {T}
    func = MOI.get(model, attr, bridge.ci)
    f = MOI.Utilities.eachscalar(func)
    return MOI.Utilities.operate(
        vcat,
        T,
        f[1],
        MOI.Utilities.operate(-, T, f[2]),
    )
end

function MOI.get(
    ::IndicatorFlipSignBridge{T,A,S1,S2},
    ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Indicator{A,S2}},
)::Int64 where {T,A,S1,S2}
    return 1
end

function MOI.get(
    bridge::IndicatorFlipSignBridge{T,A,S1,S2},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.Indicator{A,S2},
    },
) where {T,A,S1,S2}
    return [bridge.ci]
end
