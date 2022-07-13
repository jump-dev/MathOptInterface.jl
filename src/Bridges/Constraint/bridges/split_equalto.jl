# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitEqualToBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SplitEqualToBridge` implements the following reformulation:

  * ``f(x) + g(x) * im = a + b * im`` into ``f(x) = a`` and ``g(x) = b``

## Source node

`SplitEqualToBridge` supports:

  * `G` in [`MOI.EqualTo{Complex{T}}`](@ref)

where `G` is a function with `Complex` coefficients.

## Target nodes

`SplitEqualToBridge` creates:

  * `F` in [`MOI.EqualTo{T}`](@ref)

where `F` is the type of the real/imaginary part of `G`.
"""
struct SplitEqualToBridge{
    T,
    F<:MOI.Utilities.TypedScalarLike{T},
    G<:MOI.Utilities.TypedScalarLike{Complex{T}},
} <: AbstractBridge
    real_constraint::Union{Nothing,MOI.ConstraintIndex{F,MOI.EqualTo{T}}}
    imag_constraint::Union{Nothing,MOI.ConstraintIndex{F,MOI.EqualTo{T}}}
end

const SplitEqualTo{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitEqualToBridge{T},OT}

function _add_constraint_if_nonzero(model, func, set)
    if iszero(func) && iszero(MOI.constant(set))
        return nothing
    else
        return MOI.add_constraint(model, func, set)
    end
end
function bridge_constraint(
    ::Type{SplitEqualToBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::MOI.EqualTo,
) where {T,F,G}
    real_func = real(func)
    imag_func = MOI.Utilities.operate(imag, T, func)
    real_set = MOI.EqualTo(real(MOI.constant(set)))
    imag_set = MOI.EqualTo(imag(MOI.constant(set)))
    real_constraint = _add_constraint_if_nonzero(model, real_func, real_set)
    imag_constraint = _add_constraint_if_nonzero(model, imag_func, imag_set)
    return SplitEqualToBridge{T,F,G}(real_constraint, imag_constraint)
end

# We don't support `MOI.VariableIndex` as it would be a self-loop in the bridge graph
function MOI.supports_constraint(
    ::Type{SplitEqualToBridge{T}},
    ::Type{<:MOI.Utilities.TypedLike{Complex{T}}},
    ::Type{<:MOI.EqualTo},
) where {T}
    return true
end
function MOI.Bridges.added_constrained_variable_types(::Type{<:SplitEqualToBridge})
    return Tuple{DataType}[]
end
function MOI.Bridges.added_constraint_types(
    ::Type{SplitEqualToBridge{T,F,G}},
) where {T,F,G}
    return Tuple{DataType,DataType}[(F, MOI.EqualTo{T})]
end
function concrete_bridge_type(
    ::Type{<:SplitEqualToBridge{T}},
    G::Type{<:MOI.Utilities.TypedLike},
    ::Type{<:MOI.EqualTo},
) where {T}
    F = MutableArithmetics.promote_operation(imag, G)
    return SplitEqualToBridge{T,F,G}
end

# Attributes, Bridge acting as a model
function MOI.get(
    bridge::SplitEqualToBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.EqualTo{T}},
) where {T,F}
    return !isnothing(bridge.real_constraint) +
           !isnothing(bridge.imag_constraint)
end
function MOI.get(
    bridge::SplitEqualToBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.EqualTo{T}},
) where {T,F}
    list = MOI.ConstraintIndex{F,MOI.EqualTo{T}}[]
    if !isnothing(bridge.real_constraint)
        push!(list, bridge.real_constraint)
    end
    if !isnothing(bridge.imag_constraint)
        push!(list, bridge.imag_constraint)
    end
    return list
end

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::SplitEqualToBridge)
    if !isnothing(bridge.real_constraint)
        MOI.delete(model, bridge.real_constraint)
    end
    if !isnothing(bridge.imag_constraint)
        MOI.delete(model, bridge.imag_constraint)
    end
end

# Attributes, Bridge acting as a constraint
function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitEqualToBridge},
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
    bridge::SplitEqualToBridge{T},
) where {T}
    if isnothing(bridge.real_constraint)
        real_value = zero(T)
    else
        real_value = MOI.get(model, attr, bridge.real_constraint)
    end
    if isnothing(bridge.imag_constraint)
        imag_value = zero(T)
    else
        imag_value = MOI.get(model, attr, bridge.imag_constraint)
    end
    return real_value + imag_value * im
end
function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitEqualToBridge{T},
    value,
) where {T}
    if !isnothing(bridge.real_constraint)
        MOI.set(model, attr, bridge.real_constraint, real(value))
    end
    if !isnothing(bridge.imag_constraint)
        MOI.set(model, attr, bridge.real_constraint, imag(value))
    end
    return
end
