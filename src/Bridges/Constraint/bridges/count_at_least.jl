# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CountAtLeastToCountBelongsBridge{T,F} <: Bridges.Constraint.AbstractBridge

`CountAtLeastToCountBelongsBridge` implements the following reformulation:

  * ``x \\in \\textsf{CountAtLeast}(n, d, set)`` to
    ``(n_i, x_{d_i}) \\in \\textsf{CountBelongs}(1+d)``
    and ``n_i \\ge n``

## Source node

`CountAtLeastToCountBelongsBridge` supports:

  * `F` in [`MOI.CountAtLeast`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`CountAtLeastToCountBelongsBridge` creates:

  * `F` in [`MOI.CountBelongs`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.GreaterThan{T}`](@ref)
"""
mutable struct CountAtLeastToCountBelongsBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    s::MOI.CountAtLeast
    variables::Vector{MOI.VariableIndex}
    ci::Vector{MOI.ConstraintIndex{F,MOI.CountBelongs}}
    # We need an explicit inner constructor to avoid the unbound type parameter
    # T (it doesn't appear in the fields).
    function CountAtLeastToCountBelongsBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
        s::MOI.CountAtLeast,
    ) where {T}
        return new{T,typeof(f)}(
            f,
            s,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{F,MOI.CountBelongs}[],
        )
    end
end

const CountAtLeastToCountBelongs{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{CountAtLeastToCountBelongsBridge{T},OT}

function bridge_constraint(
    ::Type{CountAtLeastToCountBelongsBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.CountAtLeast,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    x = collect(MOI.Utilities.eachscalar(f))
    bridge = CountAtLeastToCountBelongsBridge{T}(f, s)
    offset = 0
    for p in s.partitions
        indices = offset .+ (1:p)
        y, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(T(s.n)))
        push!(bridge.variables, y)
        ci = MOI.add_constraint(
            model,
            MOI.Utilities.operate(vcat, T, y, x[indices]...),
            MOI.CountBelongs(1 + p, s.set),
        )
        push!(bridge.ci, ci)
        offset += p
    end
    return bridge
end

function MOI.supports_constraint(
    ::Type{<:CountAtLeastToCountBelongsBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.CountAtLeast},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CountAtLeastToCountBelongsBridge{T}},
) where {T}
    return Tuple{Type}[(MOI.GreaterThan{T},)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{CountAtLeastToCountBelongsBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.CountBelongs)]
end

function concrete_bridge_type(
    ::Type{<:CountAtLeastToCountBelongsBridge{T}},
    ::Type{F},
    ::Type{MOI.CountAtLeast},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return CountAtLeastToCountBelongsBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::CountAtLeastToCountBelongsBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::CountAtLeastToCountBelongsBridge,
)
    return bridge.s
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::CountAtLeastToCountBelongsBridge,
)
    for ci in bridge.ci
        MOI.delete(model, ci)
    end
    for x in bridge.variables
        MOI.delete(model, x)
    end
    return
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    return copy(bridge.variables)
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.CountBelongs},
)::Int64 where {T,F}
    return length(bridge.ci)
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.CountBelongs},
) where {T,F}
    return copy(bridge.ci)
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}},
)::Int64 where {T}
    return length(bridge.variables)
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{T}},
) where {T}
    return [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{T}}(x.value) for
        x in bridge.variables
    ]
end
