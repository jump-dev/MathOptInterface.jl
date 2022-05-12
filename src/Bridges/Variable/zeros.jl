# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ZerosBridge{T} <: Bridges.Variable.AbstractBridge

Transforms constrained variables in [`MathOptInterface.Zeros`](@ref) to zeros,
which ends up creating no variables in the underlying model.

The bridged variables are therefore similar to parameters with zero values.
Parameters with non-zero value can be created with constrained variables in
[`MOI.EqualTo`](@ref) by combining a [`VectorizeBridge`](@ref) and this bridge.
The functions cannot be unbridged, given a function, we cannot determine, if
the bridged variables were used.

The dual values cannot be determined by the bridge but they can be determined
by the bridged optimizer using [`MathOptInterface.Utilities.get_fallback`](@ref)
if a `CachingOptimizer` is used (since `ConstraintFunction` cannot be got
as functions cannot be unbridged).
"""
struct ZerosBridge{T} <: AbstractBridge
    n::Int # Number of variables
end

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

function MOIB.added_constrained_variable_types(::Type{<:ZerosBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(::Type{<:ZerosBridge})
    return Tuple{Type,Type}[]
end

# Attributes, Bridge acting as a model
MOI.get(::ZerosBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(bridge::ZerosBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

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
    ::MOI.ConstraintDual,
    ::ZerosBridge{T},
) where {T}
    return error(
        "Unable to query the dual of a variable bound that was reformulated " *
        "using `ZerosBridge`. This usually arises in conic models when a " *
        "variable is fixed to a value. As a work-around, instead of creating " *
        "a fixed variable using variable bounds like `p == 1`, add an affine " *
        "equality constraint like `1 * p == 1` (or `[1 * p - 1,] in Zeros(1)`).",
    )
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.VariablePrimal,
    ::ZerosBridge{T},
    ::MOIB.IndexInVector,
) where {T}
    return zero(T)
end

function MOIB.bridged_function(::ZerosBridge{T}, ::MOIB.IndexInVector) where {T}
    return zero(MOI.ScalarAffineFunction{T})
end

function unbridged_map(::ZerosBridge, ::MOI.VariableIndex, ::MOIB.IndexInVector)
    return nothing
end
