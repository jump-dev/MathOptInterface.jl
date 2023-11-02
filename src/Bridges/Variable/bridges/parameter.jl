# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ParameterToEqualToBridge{T} <: Bridges.Variable.AbstractBridge

`ParameterToEqualToBridge` implements the following reformulation:

* ``x \\in Parameter(v)`` into ``x == v``

## Source node

`ParameterToEqualToBridge` supports:

 * [`MOI.VariableIndex`](@ref) in [`MOI.Parameter`](@ref)

## Target nodes

`ParameterToEqualToBridge` creates:

 * One variable node: [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
struct ParameterToEqualToBridge{T} <: AbstractBridge
    x::MOI.VariableIndex
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}
end

const ParameterToEqualTo{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ParameterToEqualToBridge{T},OT}

function bridge_constrained_variable(
    ::Type{ParameterToEqualToBridge{T}},
    model::MOI.ModelLike,
    set::MOI.Parameter{T},
) where {T}
    x, ci = MOI.add_constrained_variable(model, MOI.EqualTo(set.value))
    return ParameterToEqualToBridge{T}(x, ci)
end

function supports_constrained_variable(
    ::Type{ParameterToEqualToBridge{T}},
    ::Type{MOI.Parameter{T}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{ParameterToEqualToBridge{T}},
) where {T}
    return Tuple{Type}[(MOI.EqualTo{T},)]
end

function MOI.Bridges.added_constraint_types(::Type{<:ParameterToEqualToBridge})
    return Tuple{Type,Type}[]
end

MOI.get(bridge::ParameterToEqualToBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(bridge::ParameterToEqualToBridge, ::MOI.ListOfVariableIndices)
    return [bridge.x]
end

function MOI.get(
    ::ParameterToEqualToBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::ParameterToEqualToBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    return [bridge.ci]
end

function MOI.delete(model::MOI.ModelLike, bridge::ParameterToEqualToBridge)
    MOI.delete(model, bridge.x)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ParameterToEqualToBridge{T},
) where {T}
    set = MOI.get(model, MOI.ConstraintSet(), bridge.ci)
    return MOI.Parameter(set.value)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintPrimal,MOI.ConstraintDual},
    bridge::ParameterToEqualToBridge,
)
    return MOI.get(model, attr, bridge.ci)
end

function _to_one(::Type{T}, x) where {T}
    return MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(one(T), x)], zero(T))
end

function MOI.Bridges.bridged_function(
    bridge::ParameterToEqualToBridge{T},
) where {T}
    return _to_one(T, bridge.x)
end

function unbridged_map(
    bridge::ParameterToEqualToBridge{T},
    x::MOI.VariableIndex,
) where {T}
    return [bridge.x => _to_one(T, x)]
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:ParameterToEqualToBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::ParameterToEqualToBridge,
)
    return MOI.get(model, attr, bridge.x)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::ParameterToEqualToBridge,
    value,
)
    MOI.set(model, attr, bridge.x, value)
    return
end
