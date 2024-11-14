# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    InequalityToComplementsBridge{T,F,S,G} <: Bridges.Constraint.AbstractBridge

`InequalityToComplementsBridge` implements the following reformulations:

  * ``f(x) \\ge b`` into ``f(x) - b \\perp y \\ge 0``
  * ``f(x) \\le b`` into ``f(x) - b \\perp y \\le 0``
  * ``f(x) = b`` into ``f(x) - b \\perp y``

## Source node

`InequalityToComplementsBridge` supports:

  * `F` in [`MOI.GreaterThan{T}`](@ref)
  * `F` in [`MOI.LessThan{T}`](@ref)
  * `F` in [`MOI.EqualTo`](@ref)

## Target nodes

`InequalityToComplementsBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.GreaterThan{T}`](@ref)
  * `F` in [`MOI.Complements`](@ref)
"""
mutable struct InequalityToComplementsBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    S<:Union{MOI.GreaterThan{T},MOI.LessThan{T},MOI.EqualTo{T}},
    G<:MOI.AbstractVectorFunction,
} <: AbstractBridge
    y::MOI.VariableIndex
    set::S
    ci::MOI.ConstraintIndex{G,MOI.Complements}
end

const InequalityToComplements{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{InequalityToComplementsBridge{T},OT}

function _add_y_variable(model, ::MOI.GreaterThan{T}) where {T}
    return MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))[1]
end

function _add_y_variable(model, ::MOI.LessThan{T}) where {T}
    return MOI.add_constrained_variable(model, MOI.LessThan(zero(T)))[1]
end

_add_y_variable(model, ::MOI.EqualTo) = MOI.add_variable(model)

function bridge_constraint(
    ::Type{InequalityToComplementsBridge{T,F,S,G}},
    model::MOI.ModelLike,
    f::F,
    set::S,
) where {T,F,S<:Union{MOI.GreaterThan{T},MOI.LessThan{T},MOI.EqualTo{T}},G}
    y = _add_y_variable(model, set)
    f_set = MOI.Utilities.operate(-, T, f, MOI.constant(set))
    g = MOI.Utilities.operate(vcat, T, f_set, y)
    ci = MOI.add_constraint(model, g, MOI.Complements(2))
    return InequalityToComplementsBridge{T,F,S,G}(y, set, ci)
end

function MOI.supports_constraint(
    ::Type{<:InequalityToComplementsBridge{T}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:Union{MOI.GreaterThan{T},MOI.LessThan{T},MOI.EqualTo{T}}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{InequalityToComplementsBridge{T,F,S,G}},
) where {T,F,S<:Union{MOI.GreaterThan{T},MOI.LessThan{T}},G}
    return Tuple{Type}[(S,)]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{InequalityToComplementsBridge{T,F,MOI.EqualTo{T},G}},
) where {T,F,G}
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{InequalityToComplementsBridge{T,F,S,G}},
) where {T,F,S,G}
    return Tuple{Type,Type}[(G,MOI.Complements)]
end

function concrete_bridge_type(
    ::Type{<:InequalityToComplementsBridge},
    F::Type{<:MOI.AbstractScalarFunction},
    S::Type{<:Union{MOI.GreaterThan{T},MOI.LessThan{T},MOI.EqualTo{T}}},
) where {T}
    G = MOI.Utilities.promote_operation(vcat, T, F, MOI.VariableIndex)
    return InequalityToComplementsBridge{T,F,S,G}
end

function MOI.get(::InequalityToComplementsBridge, ::MOI.NumberOfVariables)
    return Int64(1)
end

function MOI.get(
    bridge::InequalityToComplementsBridge,
    ::MOI.ListOfVariableIndices,
)
    return [bridge.y]
end

function MOI.get(
    ::InequalityToComplementsBridge{T,F,S,G},
    ::MOI.NumberOfConstraints{G,MOI.Complements},
)::Int64 where {T,F,S,G}
    return 1
end

function MOI.get(
    bridge::InequalityToComplementsBridge{T,F,S,G},
    ::MOI.ListOfConstraintIndices{G,MOI.Complements},
) where {T,F,S,G}
    return [bridge.ci]
end

function MOI.delete(model::MOI.ModelLike, bridge::InequalityToComplementsBridge)
    MOI.delete(model, bridge.y)
    MOI.delete(model, bridge.ci)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::InequalityToComplementsBridge{T},
) where {T}
    g = MOI.get(model, attr, bridge.ci)
    f_set = first(MOI.Utilities.scalarize(g))
    return MOI.Utilities.operate(+, T, f_set, MOI.constant(bridge.set))
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::InequalityToComplementsBridge,
)
    return bridge.set
end
