# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    AllDifferentToCountDistinctBridge{T,F} <: Bridges.Constraint.AbstractBridge

`AllDifferentToCountDistinctBridge` implements the following reformulations:

  * ``x \\in \\textsf{AllDifferent}(d)`` to ``(n, x) \\in \\textsf{CountDistinct}(1+d)``
    and ``n = d``
  * ``f(x) \\in \\textsf{AllDifferent}(d)`` to ``(d, f(x)) \\in \\textsf{CountDistinct}(1+d)``

## Source node

`AllDifferentToCountDistinctBridge` supports:

  * `F` in [`MOI.AllDifferent`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`AllDifferentToCountDistinctBridge` creates:

  * `F` in [`MOI.CountDistinct`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
mutable struct AllDifferentToCountDistinctBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    y::Union{Nothing,MOI.VariableIndex}
    ci::MOI.ConstraintIndex{F,MOI.CountDistinct}

    function AllDifferentToCountDistinctBridge{T}(
        f::MOI.VectorOfVariables,
        y::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.CountDistinct},
    ) where {T}
        return new{T,MOI.VectorOfVariables}(f, y, ci)
    end

    function AllDifferentToCountDistinctBridge{T}(
        f::MOI.VectorAffineFunction{T},
        ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.CountDistinct},
    ) where {T}
        return new{T,MOI.VectorAffineFunction{T}}(f, nothing, ci)
    end
end

const AllDifferentToCountDistinct{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{AllDifferentToCountDistinctBridge{T},OT}

function bridge_constraint(
    ::Type{AllDifferentToCountDistinctBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.AllDifferent,
) where {T,F<:MOI.VectorOfVariables}
    d = MOI.output_dimension(f)
    y, _ = MOI.add_constrained_variable(model, MOI.EqualTo(T(d)))
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, y, f),
        MOI.CountDistinct(d + 1),
    )
    return AllDifferentToCountDistinctBridge{T}(f, y, ci)
end

function bridge_constraint(
    ::Type{AllDifferentToCountDistinctBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.AllDifferent,
) where {T,F<:MOI.VectorAffineFunction{T}}
    d = MOI.output_dimension(f)
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, T(d), f),
        MOI.CountDistinct(d + 1),
    )
    return AllDifferentToCountDistinctBridge{T}(f, ci)
end

function MOI.supports_constraint(
    ::Type{<:AllDifferentToCountDistinctBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.AllDifferent},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{AllDifferentToCountDistinctBridge{T,MOI.VectorOfVariables}},
) where {T}
    return Tuple{Type}[(MOI.EqualTo{T},)]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{AllDifferentToCountDistinctBridge{T,MOI.VectorAffineFunction{T}}},
) where {T}
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{AllDifferentToCountDistinctBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.CountDistinct),]
end

function concrete_bridge_type(
    ::Type{<:AllDifferentToCountDistinctBridge{T}},
    ::Type{F},
    ::Type{MOI.AllDifferent},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return AllDifferentToCountDistinctBridge{T,F}
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
    return MOI.AllDifferent(MOI.output_dimension(bridge.f))
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::AllDifferentToCountDistinctBridge,
)
    MOI.delete(model, bridge.ci)
    if bridge.y !== nothing
        MOI.delete(model, bridge.y)
    end
    return
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge,
    ::MOI.NumberOfVariables,
)::Int64
    if bridge.y === nothing
        return 0
    end
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    if bridge.y === nothing
        return MOI.VariableIndex[]
    end
    return [bridge.y]
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    if bridge.y === nothing
        return 0
    end
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    if bridge.y === nothing
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}[]
    end
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}(bridge.y.value)
    return [ci]
end

function MOI.get(
    ::AllDifferentToCountDistinctBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.CountDistinct},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::AllDifferentToCountDistinctBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.CountDistinct},
) where {T,F}
    return [bridge.ci]
end
