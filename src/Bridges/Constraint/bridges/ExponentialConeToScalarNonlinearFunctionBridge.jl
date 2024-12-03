# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ExponentialConeToScalarNonlinearFunctionBridge{T,F} <:
        Bridges.Constraint.AbstractBridge

`ExponentialConeToScalarNonlinearFunctionBridge` implements the following
reformulation:

  * ``(x, y, z) \\in \\textsf{ExponentialCone}()`` to
    ``y \\cdot exp(x / y)) - z \\le 0``, ``y \\ge 0``.

## Source node

`ExponentialConeToScalarNonlinearFunctionBridge` supports:

  * `F` in [`MOI.ExponentialCone`](@ref)

## Target nodes

`ExponentialConeToScalarNonlinearFunctionBridge` creates:

  * [`MOI.ScalarNonlinearFunction`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.ScalarAffineFunction`](@ref) in [`MOI.GreaterThan{T}`](@ref)
"""
mutable struct ExponentialConeToScalarNonlinearFunctionBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    ci::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,MOI.LessThan{T}}
    ci_y::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}
end

const ExponentialToScalarNonlinearFunction{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ExponentialConeToScalarNonlinearFunctionBridge{T},OT}

function bridge_constraint(
    ::Type{ExponentialConeToScalarNonlinearFunctionBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.ExponentialCone,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    x, y, z = MOI.Utilities.scalarize(f)
    g_x_div_y = MOI.ScalarNonlinearFunction(:/, Any[x, y])
    g_exp_x_div_y = MOI.ScalarNonlinearFunction(:exp, Any[g_x_div_y])
    g = MOI.ScalarNonlinearFunction(
        :-,
        Any[MOI.ScalarNonlinearFunction(:*, Any[y, g_exp_x_div_y]), z],
    )
    ci = MOI.add_constraint(model, g, MOI.LessThan(zero(T)))
    ci_y = MOI.add_constraint(model, one(T) * y, MOI.GreaterThan(zero(T)))
    return ExponentialConeToScalarNonlinearFunctionBridge{T,F}(f, ci, ci_y)
end

function MOI.supports_constraint(
    ::Type{<:ExponentialConeToScalarNonlinearFunctionBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.ExponentialCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{ExponentialConeToScalarNonlinearFunctionBridge{T,F}},
) where {T,F}
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ExponentialConeToScalarNonlinearFunctionBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[
        (MOI.ScalarNonlinearFunction, MOI.LessThan{T}),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:ExponentialConeToScalarNonlinearFunctionBridge{T}},
    ::Type{F},
    ::Type{MOI.ExponentialCone},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return ExponentialConeToScalarNonlinearFunctionBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ExponentialConeToScalarNonlinearFunctionBridge,
)
    return copy(bridge.f)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    ::ExponentialConeToScalarNonlinearFunctionBridge,
)
    return MOI.ExponentialCone()
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ExponentialConeToScalarNonlinearFunctionBridge,
)
    MOI.delete(model, bridge.ci)
    MOI.delete(model, bridge.ci_y)
    return
end

function MOI.get(
    ::ExponentialConeToScalarNonlinearFunctionBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return 0
end

function MOI.get(
    ::ExponentialConeToScalarNonlinearFunctionBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    return MOI.VariableIndex[]
end

function MOI.get(
    bridge::ExponentialConeToScalarNonlinearFunctionBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarNonlinearFunction,MOI.LessThan{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::ExponentialConeToScalarNonlinearFunctionBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarNonlinearFunction,MOI.LessThan{T}},
) where {T}
    return [bridge.ci]
end

function MOI.get(
    bridge::ExponentialConeToScalarNonlinearFunctionBridge{T},
    ::MOI.NumberOfConstraints{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::ExponentialConeToScalarNonlinearFunctionBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
) where {T}
    return [bridge.ci_y]
end
