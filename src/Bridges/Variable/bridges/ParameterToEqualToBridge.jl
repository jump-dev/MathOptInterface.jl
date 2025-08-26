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
struct ParameterToEqualToBridge{T} <:
       SetMapBridge{T,MOI.EqualTo{T},MOI.Parameter{T}}
    variable::MOI.VariableIndex
    constraint::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}
end

const ParameterToEqualTo{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ParameterToEqualToBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:ParameterToEqualToBridge{T}},
    set::MOI.EqualTo{T},
) where {T}
    return MOI.Parameter{T}(set.value)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:ParameterToEqualToBridge{T}},
    set::MOI.Parameter{T},
) where {T}
    return MOI.EqualTo{T}(set.value)
end

MOI.Bridges.map_function(::Type{<:ParameterToEqualToBridge}, f) = f

MOI.Bridges.inverse_map_function(::Type{<:ParameterToEqualToBridge}, f) = f

MOI.Bridges.adjoint_map_function(::Type{<:ParameterToEqualToBridge}, f) = f

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:ParameterToEqualToBridge},
    f,
)
    return f
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.VariableName,
    ::Type{<:ParameterToEqualToBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariableName,
    bridge::ParameterToEqualToBridge,
)
    return MOI.get(model, attr, bridge.variable)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariableName,
    bridge::ParameterToEqualToBridge,
    name::String,
)
    MOI.set(model, attr, bridge.variable, name)
    return
end
