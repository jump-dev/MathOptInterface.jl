# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ZerosBridge{T} <: Bridges.Variable.AbstractBridge

`ZerosBridge` implements the following reformulation:

* ``x \\in \\{0\\}`` into the substitution rule ``x = 0``,

where `T` is the coefficient type of `0`.

## Source node

`ZerosBridge` supports:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Zeros`](@ref)

## Target nodes

`ZerosBridge` does not create target nodes. It replaces all instances of `x`
with `0` via substitution. This means that no variables are created in the
underlying model.

## Caveats

The bridged variables are similar to parameters with zero values. Parameters
with non-zero values can be created with constrained variables in
[`MOI.EqualTo`](@ref) by combining a [`VectorizeBridge`](@ref) and this bridge.

However, functions modified by `ZerosBridge` cannot be unbridged. That is, for a
given function, we cannot determine if the bridged variables were used.

A related implication is that this bridge does not support
[`MOI.ConstraintDual`](@ref). However, if a [`MOI.Utilities.CachingOptimizer`](@ref)
is used, the dual can be determined by the bridged optimizer using
[`MOI.Utilities.get_fallback`](@ref) because the caching optimizer records the
unbridged function.
"""
struct ZerosBridge{T} <: AbstractBridge
    n::Int # Number of variables
end

const Zeros{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{ZerosBridge{T},OT}

function bridge_constrained_variable(
    ::Type{ZerosBridge{T}},
    model::MOI.ModelLike,
    set::MOI.Zeros,
) where {T}
    return ZerosBridge{T}(MOI.dimension(set))
end

function supports_constrained_variable(::Type{<:ZerosBridge}, ::Type{MOI.Zeros})
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:ZerosBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:ZerosBridge})
    return Tuple{Type,Type}[]
end

# Attributes, Bridge acting as a model
MOI.get(::ZerosBridge, ::MOI.NumberOfVariables)::Int64 = 0

MOI.get(::ZerosBridge, ::MOI.ListOfVariableIndices) = MOI.VariableIndex[]

# References
MOI.delete(::MOI.ModelLike, ::ZerosBridge) = nothing

# Attributes, Bridge acting as a constraint

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::ZerosBridge)
    return MOI.Zeros(bridge.n)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimal,
    bridge::ZerosBridge{T},
) where {T}
    return zeros(T, bridge.n)
end

function MOI.get(
    ::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ::ZerosBridge{T},
) where {T}
    msg =
        "Unable to query the dual of a variable bound that was reformulated " *
        "using `ZerosBridge`. This usually arises in conic solvers when a " *
        "variable is fixed to a value. As a work-around, instead of creating " *
        "a fixed variable using variable bounds like `p == 1`, add an affine " *
        "equality constraint like `1 * p == 1` (or `[1 * p - 1,] in Zeros(1)`)."
    return throw(MOI.GetAttributeNotAllowed(attr, msg))
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.VariablePrimal,
    ::ZerosBridge{T},
    ::MOI.Bridges.IndexInVector,
) where {T}
    return zero(T)
end

function MOI.Bridges.bridged_function(
    ::ZerosBridge{T},
    ::MOI.Bridges.IndexInVector,
) where {T}
    return zero(MOI.ScalarAffineFunction{T})
end

function unbridged_map(
    ::ZerosBridge,
    ::MOI.VariableIndex,
    ::MOI.Bridges.IndexInVector,
)
    # The transformation is not recoverable, so we explicitly return `nothing`
    # here. See the docstring of `unbridged_map` for details.
    return nothing
end
