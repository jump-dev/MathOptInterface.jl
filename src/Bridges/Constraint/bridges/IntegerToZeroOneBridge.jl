# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IntegerToZeroOneBridge{T} <: Bridges.Constraint.AbstractBridge

`IntegerToZeroOneBridge` implements the following reformulation:

  * ``x \\in \\mathbf{Z}`` into ``y_i \\in \\{0, 1\\}``,
    ``x == lb + \\sum 2^{i-1} y_i``.

## Source node

`IntegerToZeroOneBridge` supports:

  * `VariableIndex` in [`MOI.Integer`](@ref)

## Target nodes

`IntegerToZeroOneBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)

## Developer note

This bridge is implemented as a constraint bridge instead of a variable bridge
because we don't want to substitute the linear combination of `y` for every
instance of `x`. Doing so would be expensive and greatly reduce the sparsity of
the constraints.
"""
mutable struct IntegerToZeroOneBridge{T} <: AbstractBridge
    x::MOI.VariableIndex
    y::Vector{MOI.VariableIndex}
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}
    last_bounds::Union{Nothing,NTuple{2,T}}

    function IntegerToZeroOneBridge{T}(x::MOI.VariableIndex) where {T}
        return new{T}(
            x,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}(0),
            nothing,
        )
    end
end

const IntegerToZeroOne{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IntegerToZeroOneBridge{T},OT}

function bridge_constraint(
    ::Type{IntegerToZeroOneBridge{T}},
    ::MOI.ModelLike,
    x::MOI.VariableIndex,
    ::MOI.Integer,
) where {T}
    # !!! info
    #     Postpone creation until final_touch.
    return IntegerToZeroOneBridge{T}(x)
end

function MOI.supports_constraint(
    ::Type{IntegerToZeroOneBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Integer},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IntegerToZeroOneBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{IntegerToZeroOneBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})]
end

function concrete_bridge_type(
    ::Type{IntegerToZeroOneBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Integer},
) where {T}
    return IntegerToZeroOneBridge{T}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::IntegerToZeroOneBridge,
)
    return bridge.x
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, ::IntegerToZeroOneBridge)
    return MOI.Integer()
end

function MOI.delete(model::MOI.ModelLike, bridge::IntegerToZeroOneBridge)
    MOI.delete(model, bridge.ci)
    MOI.delete(model, bridge.y)
    return
end

function MOI.get(bridge::IntegerToZeroOneBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.y)
end

function MOI.get(bridge::IntegerToZeroOneBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.y)
end

function MOI.get(
    bridge::IntegerToZeroOneBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.y)
end

function MOI.get(
    bridge::IntegerToZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return map(bridge.y) do y
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(y.value)
    end
end

function MOI.get(
    bridge::IntegerToZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::IntegerToZeroOneBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return [bridge.ci]
end

MOI.Bridges.needs_final_touch(::IntegerToZeroOneBridge) = true

function MOI.Bridges.final_touch(
    bridge::IntegerToZeroOneBridge{T},
    model::MOI.ModelLike,
) where {T}
    ret = MOI.Utilities.get_bounds(model, T, bridge.x)
    if ret === bridge.last_bounds
        return nothing  # final_touch already called
    elseif ret[1] == typemin(T) || ret[2] == typemax(T)
        throw(MOI.Bridges.BridgeRequiresFiniteDomainError(bridge, bridge.x))
    end
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), bridge.x)], T(0))
    lb, ub = ceil(Int, ret[1]), floor(Int, ret[2])
    N = floor(Int, log2(ub - lb)) + 1
    for i in 1:N
        y, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
        push!(bridge.y, y)
        push!(f.terms, MOI.ScalarAffineTerm(-(T(2)^(i - 1)), y))
    end
    bridge.ci = MOI.add_constraint(model, f, MOI.EqualTo{T}(lb))
    bridge.last_bounds = ret
    return
end
