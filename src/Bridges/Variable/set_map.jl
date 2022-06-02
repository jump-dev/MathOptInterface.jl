# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type SetMapBridge{T,S1,S2} <: AbstractBridge end

Consider two type of sets, `S1` and `S2`, and a linear mapping `A` such that
the image of a set of type `S1` under `A` is a set of type `S2`.

A `SetMapBridge{T,S1,S2}` is a bridge that substitutes constrained variables
in `S2` into the image through `A` of constrained variables in `S1`.

The linear map `A` is described by:

 * [`MathOptInterface.Bridges.map_set`](@ref)
 * [`MathOptInterface.Bridges.map_function`](@ref)

Implementing a method for these two functions is sufficient to bridge
constrained variables. However, in order for the getters and setters of
attributes such as dual solutions and starting values to work as well, a method
for the following functions must be implemented:

 * [`MathOptInterface.Bridges.inverse_map_set`](@ref)
 * [`MathOptInterface.Bridges.inverse_map_function`](@ref)
 * [`MathOptInterface.Bridges.adjoint_map_function`](@ref)
 * [`MathOptInterface.Bridges.inverse_adjoint_map_function`](@ref).

See the docstrings of each function to see which feature would be missing if it
was not implemented for a given bridge.
"""
abstract type SetMapBridge{T,S1,S2} <: AbstractBridge end

function _add_constrained_var(model, set::MOI.AbstractScalarSet)
    return MOI.add_constrained_variable(model, set)
end

function _add_constrained_var(model, set::MOI.AbstractVectorSet)
    return MOI.add_constrained_variables(model, set)
end

function bridge_constrained_variable(
    BT::Type{<:SetMapBridge{T,S1,S2}},
    model::MOI.ModelLike,
    set::S2,
) where {T,S1,S2}
    variables, constraint =
        _add_constrained_var(model, MOI.Bridges.inverse_map_set(BT, set))
    return BT(variables, constraint)
end

function supports_constrained_variable(
    ::Type{<:SetMapBridge{T,S1,S2}},
    ::Type{S2},
) where {T,S1,S2<:MOI.AbstractSet}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SetMapBridge{T,S1}},
) where {T,S1}
    return Tuple{Type}[(S1,)]
end

function MOI.Bridges.added_constraint_types(::Type{<:SetMapBridge})
    return Tuple{Type,Type}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::SetMapBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::SetMapBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    ::SetMapBridge{T,S1},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,S1},
)::Int64 where {T,S1<:MOI.AbstractScalarSet}
    return 1
end

function MOI.get(
    ::SetMapBridge{T,S1},
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S1},
)::Int64 where {T,S1<:MOI.AbstractVectorSet}
    return 1
end

function MOI.get(
    bridge::SetMapBridge{T,S1},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,S1},
) where {T,S1<:MOI.AbstractScalarSet}
    return [bridge.constraint]
end

function MOI.get(
    bridge::SetMapBridge{T,S1},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S1},
) where {T,S1<:MOI.AbstractVectorSet}
    return [bridge.constraint]
end

# References
function MOI.delete(
    model::MOI.ModelLike,
    bridge::SetMapBridge{T,S1,S2},
) where {T,S1,S2<:MOI.AbstractScalarSet}
    MOI.delete(model, bridge.variable)
    return
end
function MOI.delete(
    model::MOI.ModelLike,
    bridge::SetMapBridge{T,S1,S2},
) where {T,S1,S2<:MOI.AbstractVectorSet}
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return MOI.Bridges.map_set(typeof(bridge), set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge{T,S1},
    set::S1,
) where {T,S1}
    mapped = MOI.Bridges.inverse_map_set(typeof(bridge), set)
    MOI.set(model, attr, bridge.constraint, mapped)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return MOI.Bridges.map_function(typeof(bridge), value)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return MOI.Bridges.inverse_adjoint_map_function(typeof(bridge), value)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::SetMapBridge,
    i::MOI.Bridges.IndexInVector,
)
    value = MOI.get(model, attr, bridge.variables)
    return MOI.Bridges.map_function(typeof(bridge), value, i)
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:SetMapBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::SetMapBridge,
    value,
    i::MOI.Bridges.IndexInVector,
)
    bridged_value = MOI.Bridges.inverse_map_function(typeof(bridge), value)
    MOI.set(model, attr, bridge.variables[i.value], bridged_value)
    return
end

function MOI.Bridges.bridged_function(
    bridge::SetMapBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    func = MOI.Bridges.map_function(
        typeof(bridge),
        MOI.VectorOfVariables(bridge.variables),
        i,
    )
    return convert(MOI.ScalarAffineFunction{T}, func)
end

function unbridged_map(bridge::SetMapBridge{T}, vi::MOI.VariableIndex) where {T}
    F = MOI.ScalarAffineFunction{T}
    mapped = MOI.Bridges.inverse_map_function(typeof(bridge), vi)
    return Pair{MOI.VariableIndex,F}[bridge.variable=>mapped]
end

function unbridged_map(
    bridge::SetMapBridge{T},
    vis::Vector{MOI.VariableIndex},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    func = MOI.VectorOfVariables(vis)
    funcs = MOI.Bridges.inverse_map_function(typeof(bridge), func)
    scalars = MOI.UtilitiesU.eachscalar(funcs)
    return Pair{MOI.VariableIndex,F}[
        bridge.variables[i] => scalars[i] for i in eachindex(vis)
    ]
end
