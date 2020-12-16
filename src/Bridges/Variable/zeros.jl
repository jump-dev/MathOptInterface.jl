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
    return Tuple{DataType}[]
end
function MOIB.added_constraint_types(::Type{<:ZerosBridge})
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
MOI.get(bridge::ZerosBridge, ::MOI.NumberOfVariables) = 0
function MOI.get(bridge::ZerosBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

# References
function MOI.delete(::MOI.ModelLike, ::ZerosBridge) end

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
    ::MOI.VariablePrimal,
    ::ZerosBridge{T},
    ::IndexInVector,
) where {T}
    return zero(T)
end

function MOIB.bridged_function(::ZerosBridge{T}, ::IndexInVector) where {T}
    return zero(MOI.ScalarAffineFunction{T})
end
function unbridged_map(::ZerosBridge, ::MOI.VariableIndex, ::IndexInVector)
    return nothing
end
