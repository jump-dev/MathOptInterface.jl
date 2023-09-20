# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ReifiedAllDifferentToCountDistinctBridge{T,F} <:
    Bridges.Constraint.AbstractBridge

`ReifiedAllDifferentToCountDistinctBridge` implements the following
reformulations:

  * ``r \\iff x \\in \\textsf{AllDifferent}(d)`` to
    ``r \\iff (n, x) \\in \\textsf{CountDistinct}(1+d)`` and ``n = d``
  * ``r \\iff f(x) \\in \\textsf{AllDifferent}(d)`` to
    ``r \\iff (d, f(x)) \\in \\textsf{CountDistinct}(1+d)``

## Source node

`ReifiedAllDifferentToCountDistinctBridge` supports:

  * `F` in [`MOI.Reified{MOI.AllDifferent}`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`ReifiedAllDifferentToCountDistinctBridge` creates:

  * `F` in [`MOI.Reified{MOI.CountDistinct}`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
mutable struct ReifiedAllDifferentToCountDistinctBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    y::Union{Nothing,MOI.VariableIndex}
    ci::MOI.ConstraintIndex{F,MOI.Reified{MOI.CountDistinct}}

    function ReifiedAllDifferentToCountDistinctBridge{T}(
        f::MOI.VectorOfVariables,
        y::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{
            MOI.VectorOfVariables,
            MOI.Reified{MOI.CountDistinct},
        },
    ) where {T}
        return new{T,MOI.VectorOfVariables}(f, y, ci)
    end

    function ReifiedAllDifferentToCountDistinctBridge{T}(
        f::MOI.VectorAffineFunction{T},
        ci::MOI.ConstraintIndex{
            MOI.VectorAffineFunction{T},
            MOI.Reified{MOI.CountDistinct},
        },
    ) where {T}
        return new{T,MOI.VectorAffineFunction{T}}(f, nothing, ci)
    end
end

const ReifiedAllDifferentToCountDistinct{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ReifiedAllDifferentToCountDistinctBridge{T},OT}

function bridge_constraint(
    ::Type{ReifiedAllDifferentToCountDistinctBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Reified{MOI.AllDifferent},
) where {T,F<:MOI.VectorOfVariables}
    d = MOI.output_dimension(f)
    y, _ = MOI.add_constrained_variable(model, MOI.EqualTo(T(d - 1)))
    scalars = MOI.Utilities.eachscalar(f)
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, scalars[1], y, scalars[2:end]),
        MOI.Reified(MOI.CountDistinct(d)),
    )
    return ReifiedAllDifferentToCountDistinctBridge{T}(f, y, ci)
end

function bridge_constraint(
    ::Type{ReifiedAllDifferentToCountDistinctBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Reified{MOI.AllDifferent},
) where {T,F<:MOI.VectorAffineFunction{T}}
    d = MOI.output_dimension(f)
    scalars = MOI.Utilities.eachscalar(f)
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, scalars[1], T(d - 1), scalars[2:end]),
        MOI.Reified(MOI.CountDistinct(d)),
    )
    return ReifiedAllDifferentToCountDistinctBridge{T}(f, ci)
end

function MOI.supports_constraint(
    ::Type{<:ReifiedAllDifferentToCountDistinctBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.Reified{MOI.AllDifferent}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{ReifiedAllDifferentToCountDistinctBridge{T,MOI.VectorOfVariables}},
) where {T}
    return Tuple{Type}[(MOI.EqualTo{T},)]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{
        ReifiedAllDifferentToCountDistinctBridge{T,MOI.VectorAffineFunction{T}},
    },
) where {T}
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ReifiedAllDifferentToCountDistinctBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.Reified{MOI.CountDistinct}),]
end

function concrete_bridge_type(
    ::Type{<:ReifiedAllDifferentToCountDistinctBridge{T}},
    ::Type{F},
    ::Type{MOI.Reified{MOI.AllDifferent}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return ReifiedAllDifferentToCountDistinctBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ReifiedAllDifferentToCountDistinctBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ReifiedAllDifferentToCountDistinctBridge,
)
    return MOI.Reified(MOI.AllDifferent(MOI.output_dimension(bridge.f) - 1))
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ReifiedAllDifferentToCountDistinctBridge,
)
    MOI.delete(model, bridge.ci)
    if bridge.y !== nothing
        MOI.delete(model, bridge.y)
    end
    return
end

function MOI.get(
    bridge::ReifiedAllDifferentToCountDistinctBridge,
    ::MOI.NumberOfVariables,
)::Int64
    if bridge.y === nothing
        return 0
    end
    return 1
end

function MOI.get(
    bridge::ReifiedAllDifferentToCountDistinctBridge,
    ::MOI.ListOfVariableIndices,
)
    if bridge.y === nothing
        return MOI.VariableIndex[]
    end
    return MOI.VariableIndex[something(bridge.y)]
end

function MOI.get(
    bridge::ReifiedAllDifferentToCountDistinctBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    if bridge.y === nothing
        return 0
    end
    return 1
end

function MOI.get(
    bridge::ReifiedAllDifferentToCountDistinctBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    F, S = MOI.VariableIndex,MOI.EqualTo{T}
    ret = MOI.ConstraintIndex{F,S}[]
    if bridge.y === nothing
        push!(ret, MOI.ConstraintIndex{F,S}(something(bridge.y).value))
    end
    return ret
end

function MOI.get(
    ::ReifiedAllDifferentToCountDistinctBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Reified{MOI.CountDistinct}},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::ReifiedAllDifferentToCountDistinctBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.Reified{MOI.CountDistinct}},
) where {T,F}
    return [bridge.ci]
end
