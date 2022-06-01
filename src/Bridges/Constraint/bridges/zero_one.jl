# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ZeroOneBridge{T} <: Bridges.Constraint.AbstractBridge

`ZeroOneBridge` implements the following reformulation:

  * ``x \\in \\{0, 1\\}`` into ``x \\in \\mathbb{Z}``, ``1x \\in [0, 1]``.

## Source node

`ZeroOneBridge` supports:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)

## Target nodes

`ZeroOneBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.Integer`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.Interval{T}`](@ref)
"""
struct ZeroOneBridge{T} <: AbstractBridge
    interval_index::MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    }
    integer_index::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}
end

const ZeroOne{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{ZeroOneBridge{T},OT}

function bridge_constraint(
    ::Type{ZeroOneBridge{T}},
    model::MOI.ModelLike,
    x::MOI.VariableIndex,
    ::MOI.ZeroOne,
) where {T<:Real}
    f = convert(MOI.ScalarAffineFunction{T}, x)
    interval = MOI.add_constraint(model, f, MOI.Interval{T}(zero(T), one(T)))
    integer = MOI.add_constraint(model, x, MOI.Integer())
    return ZeroOneBridge{T}(interval, integer)
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:ZeroOneBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.Interval{T}),
        (MOI.VariableIndex, MOI.Integer),
    ]
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:ZeroOneBridge})
    return Tuple{Type}[]
end

function concrete_bridge_type(
    ::Type{<:ZeroOneBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
) where {T}
    return ZeroOneBridge{T}
end

function MOI.supports_constraint(
    ::Type{<:ZeroOneBridge},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
)
    return true
end

MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, ::ZeroOneBridge) = MOI.ZeroOne()

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::ZeroOneBridge,
)
    return MOI.get(model, attr, bridge.integer_index)
end

function MOI.delete(model::MOI.ModelLike, bridge::ZeroOneBridge)
    MOI.delete(model, bridge.interval_index)
    MOI.delete(model, bridge.integer_index)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal},
    bridge::ZeroOneBridge,
)
    return MOI.get(model, attr, bridge.integer_index)
end

function MOI.get(
    ::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.Interval{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    ::ZeroOneBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer},
)::Int64
    return 1
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.Interval{T}},
) where {T}
    return [bridge.interval_index]
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer},
)
    return [bridge.integer_index]
end
