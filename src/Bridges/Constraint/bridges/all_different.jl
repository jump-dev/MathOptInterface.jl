# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    AllDifferentToCountDistinctBridge{T} <: Bridges.Constraint.AbstractBridge

`AllDifferentToCountDistinctBridge` implements the following reformulation:

  * ``x in AllDifferent(d)`` to ``[n, x] in CountDistinct(d+1)`` and ``n = d``

## Source node

`AllDifferentToCountDistinctBridge` supports:

  * [`MOI.VectorOfVariables`] in [`MOI.AllDifferent`](@ref)

## Target nodes

`AllDifferentToCountDistinctBridge` creates:

  * [`MOI.VectorOfVariables`](@ref) in [`MOI.CountDistinct`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
mutable struct AllDifferentToCountDistinctBridge{T} <: AbstractBridge
    f::MOI.VectorOfVariables
    y::MOI.VariableIndex
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.CountDistinct}
end

const AllDifferentToCountDistinct{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{AllDifferentToCountDistinctBridge{T},OT}

function bridge_constraint(
    ::Type{AllDifferentToCountDistinctBridge{T}},
    model::MOI.ModelLike,
    f::MOI.VectorOfVariables,
    s::MOI.AllDifferent,
) where {T}
    d = length(f.variables)
    y, _ = MOI.add_constrained_variable(model, MOI.EqualTo(T(d)))
    ci = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(vcat(y, f.variables)),
        MOI.CountDistinct(d + 1),
    )
    return AllDifferentToCountDistinctBridge{T}(f, y, ci)
end

function MOI.supports_constraint(
    ::Type{AllDifferentToCountDistinctBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.AllDifferent},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{AllDifferentToCountDistinctBridge{T}},
) where {T}
    return Tuple{Type}[(MOI.EqualTo{T},)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{AllDifferentToCountDistinctBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.VectorOfVariables, MOI.CountDistinct),]
end

function concrete_bridge_type(
    ::Type{<:AllDifferentToCountDistinctBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.AllDifferent},
) where {T}
    return AllDifferentToCountDistinctBridge{T}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::AllDifferentToCountDistinctBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::AllDifferentToCountDistinctBridge,
)
    return MOI.AllDifferent(length(bridge.f.variables))
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::AllDifferentToCountDistinctBridge,
)
    MOI.delete(model, bridge.ci)
    MOI.delete(model, bridge.y)
    return
end

function MOI.get(
    ::AllDifferentToCountDistinctBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge,
    ::MOI.ListOfVariableIndices,
)
    return [bridge.y]
end

function MOI.get(
    ::AllDifferentToCountDistinctBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}(bridge.y.value)
    return [ci]
end

function MOI.get(
    ::AllDifferentToCountDistinctBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.CountDistinct},
)::Int64
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.CountDistinct},
)
    return [bridge.ci]
end
