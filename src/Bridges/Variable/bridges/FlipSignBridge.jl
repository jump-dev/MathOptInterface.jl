# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type FlipSignBridge{T,S1,S2} <: SetMapBridge{T,S2,S1} end

An abstract type that simplifies the creation of other bridges.
"""
abstract type FlipSignBridge{T,S1<:MOI.AbstractSet,S2<:MOI.AbstractSet} <:
              SetMapBridge{T,S2,S1} end

function MOI.Bridges.map_function(::Type{<:FlipSignBridge{T}}, func) where {T}
    return MOI.Utilities.operate(-, T, func)
end

# The map is an involution
function MOI.Bridges.inverse_map_function(BT::Type{<:FlipSignBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:FlipSignBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric involution
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:FlipSignBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    i::MOI.Bridges.IndexInVector,
)
    MOI.delete(model, bridge.variables[i.value])
    deleteat!(bridge.variables, i.value)
    return
end

"""
    NonposToNonnegBridge{T} <: Bridges.Variable.AbstractBridge

`NonposToNonnegBridge` implements the following reformulation:

* ``x \\in \\mathbb{R}_-`` into ``y \\in \\mathbb{R}_+`` with the substitution
  rule ``x = -y``,

where `T` is the coefficient type of `-y`.

## Source node

`NonposToNonnegBridge` supports:

* [`MOI.VectorOfVariables`](@ref) in [`MOI.Nonpositives`](@ref)

## Target nodes

`NonposToNonnegBridge` creates:

* One variable node: [`MOI.VectorOfVariables`](@ref) in [`MOI.Nonnegatives`](@ref),
"""
struct NonposToNonnegBridge{T} <:
       FlipSignBridge{T,MOI.Nonpositives,MOI.Nonnegatives}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
end

const NonposToNonneg{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonposToNonnegBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:NonposToNonnegBridge},
    set::MOI.Nonnegatives,
)
    return MOI.Nonpositives(set.dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:NonposToNonnegBridge},
    set::MOI.Nonpositives,
)
    return MOI.Nonnegatives(set.dimension)
end
