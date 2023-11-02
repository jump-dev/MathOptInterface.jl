# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitComplexZerosBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SplitComplexZerosBridge` implements the following reformulation:

  * ``f(x) \\in \\{0\\}^n`` into ``\\text{Re}(f(x)) \\in \\{0\\}^n`` and
    ``\\text{Im}(f(x)) \\in \\{0\\}^n``

## Source node

`SplitComplexZerosBridge` supports:

  * `G` in [`MOI.Zeros`](@ref)

where `G` is a function with `Complex` coefficients.

## Target nodes

`SplitComplexZerosBridge` creates:

  * `F` in [`MOI.Zeros`](@ref)

where `F` is the type of the real/imaginary part of `G`.
"""
struct SplitComplexZerosBridge{
    T,
    F<:MOI.Utilities.TypedLike{T},
    G<:MOI.Utilities.TypedLike{Complex{T}},
} <: AbstractBridge
    dimension::Int
    constraint::MOI.ConstraintIndex{F,MOI.Zeros}
    real_indices::Vector{Int}
    imag_indices::Vector{Int}
end

const SplitComplexZeros{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitComplexZerosBridge{T},OT}

function _nonzero_indices(func::MOI.AbstractVectorFunction)
    return [
        i for (i, scalar_func) in enumerate(MOI.Utilities.scalarize(func)) if
        !iszero(scalar_func)
    ]
end

function bridge_constraint(
    ::Type{SplitComplexZerosBridge{T,F,G}},
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
    return SplitComplexZerosBridge{T,F,G}(
        MOI.dimension(set),
        constraint,
        real_indices,
        imag_indices,
    )
end

# We don't support `MOI.VectorOfVariables` as it would be a self-loop in the
# bridge graph
function MOI.supports_constraint(
    ::Type{<:SplitComplexZerosBridge{T}},
    ::Type{<:MOI.Utilities.TypedLike{Complex{T}}},
    ::Type{MOI.Zeros},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitComplexZerosBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{SplitComplexZerosBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[(F, MOI.Zeros)]
end

function concrete_bridge_type(
    ::Type{<:SplitComplexZerosBridge{T}},
    G::Type{<:MOI.Utilities.TypedLike},
    ::Type{MOI.Zeros},
) where {T}
    F = MA.promote_operation(imag, G)
    return SplitComplexZerosBridge{T,F,G}
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SplitComplexZerosBridge{T,F,G},
) where {T,F,G}
    g = MOI.Utilities.zero_with_output_dimension(G, bridge.dimension)
    f = MOI.get(model, attr, bridge.constraint)
    S = MOI.Utilities.scalar_type(G)
    for (f_i, g_i) in enumerate(bridge.real_indices)
        complex_f = convert(S, MOI.Utilities.eachscalar(f)[f_i])
        MOI.Utilities.operate_output_index!(+, Complex{T}, g_i, g, complex_f)
    end
    for (f_i, g_i) in enumerate(bridge.imag_indices)
        scalar = MOI.Utilities.eachscalar(f)[length(bridge.real_indices)+f_i]
        complex_f = im * scalar
        MOI.Utilities.operate_output_index!(+, Complex{T}, g_i, g, complex_f)
    end
    return g
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitComplexZerosBridge,
)
    return MOI.Zeros(bridge.dimension)
end

function MOI.get(
    ::SplitComplexZerosBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Zeros},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::SplitComplexZerosBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.Zeros},
) where {T,F}
    return [bridge.constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::SplitComplexZerosBridge)
    MOI.delete(model, bridge.constraint)
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitComplexZerosBridge{T,F}},
) where {T,F}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,MOI.Zeros})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{
        MOI.ConstraintPrimal,
        MOI.ConstraintPrimalStart,
        MOI.ConstraintDual,
        MOI.ConstraintDualStart,
    },
    bridge::SplitComplexZerosBridge,
)
    values = MOI.get(model, attr, bridge.constraint)
    if values === nothing
        return nothing
    end
    output = zeros(Complex{eltype(values)}, bridge.dimension)
    for (i, idx) in enumerate(bridge.real_indices)
        output[idx] = values[i]
    end
    for (i, idx) in enumerate(bridge.imag_indices)
        output[idx] += values[length(bridge.real_indices)+i] * im
    end
    return output
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitComplexZerosBridge{T},
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
    MOI.set(model, attr, bridge.constraint, input)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitComplexZerosBridge{T},
    ::Nothing,
) where {T}
    MOI.set(model, attr, bridge.constraint, nothing)
    return
end
