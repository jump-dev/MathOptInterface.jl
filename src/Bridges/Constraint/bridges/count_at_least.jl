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
    and ``\\sum\\limits n_i \\ge n``

## Source node

`CountAtLeastToCountBelongsBridge` supports:

  * `F` in [`MOI.CountAtLeast`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`CountAtLeastToCountBelongsBridge` creates:

  * `F` in [`MOI.CountBelongs`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)
"""
mutable struct CountAtLeastToCountBelongsBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    s::MOI.CountAtLeast
    variables::Vector{MOI.VariableIndex}
    ci::Vector{MOI.ConstraintIndex{F,MOI.CountBelongs}}
    count::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}
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
    variables = MOI.VariableIndex[]
    cis = MOI.ConstraintIndex{F,MOI.CountBelongs}[]
    count_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
    offset = 0
    for p in s.partitions
        indices = offset .+ (1:p)
        y = MOI.add_variable(model)
        push!(variables, y)
        push!(count_f.terms, MOI.ScalarAffineTerm(one(T), y))
        ci = MOI.add_constraint(
            model,
            MOI.Utilities.operate(vcat, T, y, x[indices]...),
            MOI.CountBelongs(1 + p, s.set),
        )
        push!(cis, ci)
        offset += p
    end
    count = MOI.add_constraint(model, count_f, MOI.GreaterThan(T(s.n)))
    return CountAtLeastToCountBelongsBridge{T,F}(f, s, variables, cis, count)
end

function MOI.supports_constraint(
    ::Type{<:CountAtLeastToCountBelongsBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.CountAtLeast},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CountAtLeastToCountBelongsBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{CountAtLeastToCountBelongsBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[
        (F, MOI.CountBelongs),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
    ]
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
    MOI.delete(model, bridge.count)
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
    ::CountAtLeastToCountBelongsBridge{T,F},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::CountAtLeastToCountBelongsBridge{T,F},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
) where {T,F}
    return [bridge.count]
end
