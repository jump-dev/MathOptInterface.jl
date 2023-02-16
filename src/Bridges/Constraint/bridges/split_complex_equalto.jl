# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitComplexEqualToBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SplitComplexEqualToBridge` implements the following reformulation:

  * ``f(x) + g(x) * im = a + b * im`` into ``f(x) = a`` and ``g(x) = b``

## Source node

`SplitComplexEqualToBridge` supports:

  * `G` in [`MOI.EqualTo{Complex{T}}`](@ref)

where `G` is a function with `Complex` coefficients.

## Target nodes

`SplitComplexEqualToBridge` creates:

  * `F` in [`MOI.EqualTo{T}`](@ref)

where `F` is the type of the real/imaginary part of `G`.
"""
struct SplitComplexEqualToBridge{
    T,
    F<:MOI.Utilities.TypedScalarLike{T},
    G<:MOI.Utilities.TypedScalarLike{Complex{T}},
} <: AbstractBridge
    real_constraint::Union{Nothing,MOI.ConstraintIndex{F,MOI.EqualTo{T}}}
    imag_constraint::Union{Nothing,MOI.ConstraintIndex{F,MOI.EqualTo{T}}}
end

const SplitComplexEqualTo{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitComplexEqualToBridge{T},OT}

function _add_constraint_if_nonzero(model, func, set)
    if iszero(func) && iszero(MOI.constant(set))
        return nothing
    end
    return MOI.add_constraint(model, func, set)
end

function bridge_constraint(
    ::Type{SplitComplexEqualToBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::MOI.EqualTo,
) where {T,F,G}
    real_func = real(func)
    imag_func = MOI.Utilities.operate(imag, T, func)
    real_set = MOI.EqualTo(real(MOI.constant(set)))
    imag_set = MOI.EqualTo(imag(MOI.constant(set)))
    real_ci = _add_constraint_if_nonzero(model, real_func, real_set)
    imag_ci = _add_constraint_if_nonzero(model, imag_func, imag_set)
    return SplitComplexEqualToBridge{T,F,G}(real_ci, imag_ci)
end

# We don't support `MOI.VariableIndex` as it would be a self-loop in the bridge
# graph.
function MOI.supports_constraint(
    ::Type{<:SplitComplexEqualToBridge{T}},
    ::Type{<:MOI.Utilities.TypedLike{Complex{T}}},
    ::Type{<:MOI.EqualTo},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitComplexEqualToBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{SplitComplexEqualToBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[(F, MOI.EqualTo{T})]
end

function concrete_bridge_type(
    ::Type{<:SplitComplexEqualToBridge{T}},
    G::Type{<:MOI.Utilities.TypedLike},
    ::Type{<:MOI.EqualTo},
) where {T}
    F = MA.promote_operation(imag, G)
    return SplitComplexEqualToBridge{T,F,G}
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SplitComplexEqualToBridge{T,F,G},
) where {T,F,G}
    g = zero(G)
    if bridge.real_constraint !== nothing
        f = MOI.get(model, attr, bridge.real_constraint)
        g = MOI.Utilities.operate!(+, Complex{T}, g, convert(G, f))
    end
    if bridge.imag_constraint !== nothing
        f = MOI.get(model, attr, bridge.imag_constraint)
        g = MOI.Utilities.operate!(+, Complex{T}, g, im * f)
    end
    return g
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitComplexEqualToBridge{T},
) where {T}
    rhs = zero(T) + zero(T) * im
    if bridge.real_constraint !== nothing
        set = MOI.get(model, MOI.ConstraintSet(), bridge.real_constraint)
        rhs += set.value
    end
    if bridge.imag_constraint !== nothing
        set = MOI.get(model, MOI.ConstraintSet(), bridge.imag_constraint)
        rhs += set.value * im
    end
    return MOI.EqualTo(rhs)
end

function MOI.get(
    bridge::SplitComplexEqualToBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.EqualTo{T}},
)::Int64 where {T,F}
    return Int64(bridge.real_constraint !== nothing) +
           Int64(bridge.imag_constraint !== nothing)
end

function MOI.get(
    bridge::SplitComplexEqualToBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.EqualTo{T}},
) where {T,F}
    list = MOI.ConstraintIndex{F,MOI.EqualTo{T}}[]
    if bridge.real_constraint !== nothing
        push!(list, bridge.real_constraint)
    end
    if bridge.imag_constraint !== nothing
        push!(list, bridge.imag_constraint)
    end
    return list
end

function MOI.delete(model::MOI.ModelLike, bridge::SplitComplexEqualToBridge)
    if bridge.real_constraint !== nothing
        MOI.delete(model, bridge.real_constraint)
    end
    if bridge.imag_constraint !== nothing
        MOI.delete(model, bridge.imag_constraint)
    end
    return
end

function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitComplexEqualToBridge},
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
    bridge::SplitComplexEqualToBridge{T},
) where {T}
    real_value, imag_value = zero(T), zero(T)
    if bridge.real_constraint !== nothing
        real_value = MOI.get(model, attr, bridge.real_constraint)
        if real_value === nothing
            return nothing
        end
    end
    if bridge.imag_constraint !== nothing
        imag_value = MOI.get(model, attr, bridge.imag_constraint)
        if imag_value === nothing
            return nothing
        end
    end
    return real_value + imag_value * im
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitComplexEqualToBridge{T},
    value,
) where {T}
    if bridge.real_constraint !== nothing
        MOI.set(model, attr, bridge.real_constraint, real(value))
    end
    if bridge.imag_constraint !== nothing
        MOI.set(model, attr, bridge.imag_constraint, imag(value))
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitComplexEqualToBridge,
    ::Nothing,
)
    if bridge.real_constraint !== nothing
        MOI.set(model, attr, bridge.real_constraint, nothing)
    end
    if bridge.imag_constraint !== nothing
        MOI.set(model, attr, bridge.imag_constraint, nothing)
    end
    return
end
