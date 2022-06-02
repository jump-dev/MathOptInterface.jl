# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

abstract type _AbstractSOCtoNonConvexQuadBridge{T} <: AbstractBridge end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    b::_AbstractSOCtoNonConvexQuadBridge{T},
) where {T}
    return MOI.VectorOfVariables(b.vars)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:_AbstractSOCtoNonConvexQuadBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:_AbstractSOCtoNonConvexQuadBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarQuadraticFunction{T}, MOI.LessThan{T}),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
    ]
end

function MOI.get(
    ::_AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::_AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    },
) where {T}
    return [bridge.quad]
end

function MOI.get(
    bridge::_AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
)::Int64 where {T}
    return length(bridge.var_pos)
end

function MOI.get(
    bridge::_AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
) where {T}
    return copy(bridge.var_pos)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::_AbstractSOCtoNonConvexQuadBridge,
)
    MOI.delete(model, bridge.quad)
    MOI.delete.(model, bridge.var_pos)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::_AbstractSOCtoNonConvexQuadBridge,
)
    return MOI.get.(model, MOI.VariablePrimal(attr.result_index), bridge.vars)
end

"""
    SOCtoNonConvexQuadBridge{T} <: Bridges.Constraint.AbstractBridge

`SOCtoNonConvexQuadBridge` implements the following reformulations:

  * ``||x||_2 \\le t`` into ``\\sum x^2 - t^2 \\le 0`` and ``1t + 0 \\ge 0``

The [`MOI.ScalarAffineFunction`](@ref) ``1t + 0`` is used in case the variable
has other bound constraints.

!!! warning
    This transformation starts from a convex constraint and creates a
    non-convex constraint. Unless the solver has explicit support for detecting
    second-order cones in quadratic form, this may (wrongly) be interpreted by
    the solver as being non-convex. Therefore, this bridge is not added
    automatically by [`MOI.Bridges.full_bridge_optimizer`](@ref). Care is
    recommended when adding this bridge to a optimizer.

## Source node

`SOCtoNonConvexQuadBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in [`MOI.SecondOrderCone`](@ref)

## Target nodes

`SOCtoNonConvexQuadBridge` creates:

  * [`MOI.ScalarQuadraticFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)
"""
struct SOCtoNonConvexQuadBridge{T} <: _AbstractSOCtoNonConvexQuadBridge{T}
    quad::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}}
    var_pos::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
    }
    vars::Vector{MOI.VariableIndex}
end

const SOCtoNonConvexQuad{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoNonConvexQuadBridge{T},OT}

function bridge_constraint(
    ::Type{SOCtoNonConvexQuadBridge{T}},
    model,
    func::MOI.VectorOfVariables,
    set::MOI.SecondOrderCone,
) where {T}
    t, x = func.variables[1], func.variables[2:end]
    a_terms = MOI.ScalarAffineTerm{T}[]
    q_terms = MOI.ScalarQuadraticTerm.(T(2), x, x)
    push!(q_terms, MOI.ScalarQuadraticTerm(-T(2), t, t))
    fq = MOI.ScalarQuadraticFunction(q_terms, a_terms, zero(T))
    quad = MOI.add_constraint(model, fq, MOI.LessThan(zero(T)))
    fp = convert(MOI.ScalarAffineFunction{T}, t)
    var_pos = MOI.add_constraint(model, fp, MOI.GreaterThan(zero(T)))
    return SOCtoNonConvexQuadBridge(quad, [var_pos], func.variables)
end

function MOI.supports_constraint(
    ::Type{SOCtoNonConvexQuadBridge},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.SecondOrderCone},
)
    return true
end

function concrete_bridge_type(
    ::Type{SOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.SecondOrderCone},
) where {T}
    return SOCtoNonConvexQuadBridge{T}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    b::SOCtoNonConvexQuadBridge,
)
    return MOI.SecondOrderCone(length(b.vars))
end

"""
    RSOCtoNonConvexQuadBridge{T} <: Bridges.Constraint.AbstractBridge

`RSOCtoNonConvexQuadBridge` implements the following reformulations:

  * ``||x||_2 \\le t \\cdot u`` into ``\\sum x^2 - t\\cdot u \\le 0``,
    ``1t + 0 \\ge 0``, and ``1u + 0 \\ge 0``.

The [`MOI.ScalarAffineFunction`](@ref)s ``1t + 0`` and ``1u + 0`` are used
in case the variables have other bound constraints.

!!! warning
    This transformation starts from a convex constraint and creates a
    non-convex constraint. Unless the solver has explicit support for detecting
    rotated second-order cones in quadratic form, this may (wrongly) be
    interpreted by the solver as being non-convex. Therefore, this bridge is not
    added automatically by [`MOI.Bridges.full_bridge_optimizer`](@ref). Care is
    recommended when adding this bridge to a optimizer.

## Source node

`RSOCtoNonConvexQuadBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in [`MOI.RotatedSecondOrderCone`](@ref)

## Target nodes

`RSOCtoNonConvexQuadBridge` creates:

  * [`MOI.ScalarQuadraticFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)
"""
struct RSOCtoNonConvexQuadBridge{T} <: _AbstractSOCtoNonConvexQuadBridge{T}
    quad::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}}
    var_pos::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
    }
    vars::Vector{MOI.VariableIndex}
end

const RSOCtoNonConvexQuad{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoNonConvexQuadBridge{T},OT}

function bridge_constraint(
    ::Type{RSOCtoNonConvexQuadBridge{T}},
    model,
    func::MOI.VectorOfVariables,
    set::MOI.RotatedSecondOrderCone,
) where {T}
    t, u, x = func.variables[1], func.variables[2], func.variables[3:end]
    a_terms = MOI.ScalarAffineTerm{T}[]
    q_terms = MOI.ScalarQuadraticTerm.(T(2), x, x)
    push!(q_terms, MOI.ScalarQuadraticTerm(-T(2), t, u))
    fq = MOI.ScalarQuadraticFunction(q_terms, a_terms, zero(T))
    quad = MOI.add_constraint(model, fq, MOI.LessThan(zero(T)))
    fp1 = convert(MOI.ScalarAffineFunction{T}, t)
    var_pos1 = MOI.add_constraint(model, fp1, MOI.GreaterThan(zero(T)))
    fp2 = convert(MOI.ScalarAffineFunction{T}, u)
    var_pos2 = MOI.add_constraint(model, fp2, MOI.GreaterThan(zero(T)))
    return RSOCtoNonConvexQuadBridge(quad, [var_pos1, var_pos2], func.variables)
end

function MOI.supports_constraint(
    ::Type{RSOCtoNonConvexQuadBridge},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.RotatedSecondOrderCone},
)
    return true
end

function concrete_bridge_type(
    ::Type{RSOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    return RSOCtoNonConvexQuadBridge{T}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    b::RSOCtoNonConvexQuadBridge,
)
    return MOI.RotatedSecondOrderCone(length(b.vars))
end
