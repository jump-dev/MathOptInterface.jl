# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitZerosBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SplitZerosBridge` implements the following reformulation:

  * ``f(x) \\in \\{0\\}^n`` into ``\\text{Re}(f(x)) \\in \\{0\\}^n`` and ``\\text{Im}(f(x)) \\in \\{0\\}^n``

## Source node

`SplitZerosBridge` supports:

  * `G` in [`MOI.Zeros`](@ref)

## Target nodes

`SplitZerosBridge` creates:

  * `F` in [`MOI.Zeros`](@ref)

where `F` is the type of the real/imaginary part of `G`.
"""
struct SplitZerosBridge{
    T,
    F<:MOI.Utilities.TypedLike{T},
    G<:MOI.Utilities.TypedLike{Complex{T}},
} <: AbstractBridge
    dimension::Int
    constraint::MOI.ConstraintIndex{F,MOI.Zeros}
    real_indices::Vector{Int}
    imag_indices::Vector{Int}
end

const SplitZeros{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitZerosBridge{T},OT}

function _nonzero_indices(func::MOI.AbstractVectorFunction)
    return [
        i for (i, scalar_func) in enumerate(MOI.Utilities.scalarize(func)) if
        !iszero(scalar_func)
    ]
end
function bridge_constraint(
    ::Type{SplitZerosBridge{T,F,G}},
    model::MOI.ModelLike,
    f::G,
    set::MOI.Zeros,
) where {T,F,G}
    real_part = real(f)
    imag_part = MOI.Utilities.operate(imag, T, f)
    real_indices = _nonzero_indices(real_part)
    imag_indices = _nonzero_indices(imag_part)
    func = MOI.Utilities.operate(
        vcat,
        T,
        MOI.Utilities.eachscalar(real_part)[real_indices],
        MOI.Utilities.eachscalar(imag_part)[imag_indices],
    )
    constraint = MOI.add_constraint(
        model,
        func,
        MOI.Zeros(length(real_indices) + length(imag_indices)),
    )
    return SplitZerosBridge{T,F,G}(
        MOI.dimension(set),
        constraint,
        real_indices,
        imag_indices,
    )
end

# We don't support `MOI.VectorOfVariables` as it would be a self-loop in the bridge graph
function MOI.supports_constraint(
    ::Type{SplitZerosBridge{T}},
    ::Type{<:MOI.Utilities.TypedLike{Complex{T}}},
    ::Type{MOI.Zeros},
) where {T}
    return true
end
function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitZerosBridge},
)
    return Tuple{DataType}[]
end
function MOI.Bridges.added_constraint_types(
    ::Type{SplitZerosBridge{T,F,G}},
) where {T,F,G}
    return Tuple{DataType,DataType}[(F, MOI.Zeros)]
end
function concrete_bridge_type(
    ::Type{<:SplitZerosBridge{T}},
    G::Type{<:MOI.Utilities.TypedLike},
    ::Type{MOI.Zeros},
) where {T}
    F = MA.promote_operation(imag, G)
    return SplitZerosBridge{T,F,G}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::SplitZerosBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Zeros},
) where {T,F}
    return 1
end
function MOI.get(
    bridge::SplitZerosBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.Zeros},
) where {T,F}
    return [bridge.constraint]
end

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::SplitZerosBridge)
    return MOI.delete(model, bridge.constraint)
end

# Attributes, Bridge acting as a constraint
function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitZerosBridge},
)
    return true
end
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{
        MOI.ConstraintPrimal,
        MOI.ConstraintPrimalStart,
        MOI.ConstraintDual,
        MOI.ConstraintDualStart,
    },
    bridge::SplitZerosBridge,
)
    values = MOI.get(model, attr, bridge.constraint)
    output = zeros(Complex{eltype(values)}, bridge.dimension)
    for (i, idx) in enumerate(bridge.real_indices)
        output[idx] = values[i]
    end
    for (i, idx) in enumerate(bridge.imag_indices)
        output[idx] = values[length(bridge.real_indices)+i] * im
    end
    return output
end
function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitZerosBridge{T},
    value,
) where {T}
    input = Vector{T}(
        undef,
        length(bridge.real_indices) + length(bridge.imag_indices),
    )
    for (i, idx) in enumerate(bridge.real_indices)
        input[i] = real(value[idx])
    end
    for (i, idx) in enumerate(bridge.imag_indices)
        input[length(bridge.real_indices)+i] = imag(value[idx])
    end
    return MOI.set(model, attr, bridge.constraint, input)
end
