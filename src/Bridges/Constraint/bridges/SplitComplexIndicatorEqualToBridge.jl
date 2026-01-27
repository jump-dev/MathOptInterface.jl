# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitComplexIndicatorEqualToBridge{T,F,G,A} <: Bridges.Constraint.AbstractBridge

`SplitComplexIndicatorEqualToBridge` implements the following reformulation:

  * ``z \\implies f(x) + g(x) * im = a + b * im`` into ``z \\implies f(x) = a``
    and ``z \\implies g(x) = b``

## Source node

`SplitComplexIndicatorEqualToBridge` supports:

  * `G` in [`MOI.Indicator{A,MOI.EqualTo{Complex{T}}`](@ref)

where `G` is a function with `Complex` coefficients.

## Target nodes

`SplitComplexIndicatorEqualToBridge` creates:

  * `F` in [`MOI.Indicator{A,MOI.EqualTo{T}}`](@ref)

where `F` is the type of the real/imaginary part of `G`.
"""
struct SplitComplexIndicatorEqualToBridge{
    T,
    F<:MOI.Utilities.TypedVectorLike{T},
    G<:MOI.Utilities.TypedVectorLike{Complex{T}},
    A,
} <: AbstractBridge
    real_constraint::Union{
        Nothing,
        MOI.ConstraintIndex{F,MOI.Indicator{A,MOI.EqualTo{T}}},
    }
    imag_constraint::Union{
        Nothing,
        MOI.ConstraintIndex{F,MOI.Indicator{A,MOI.EqualTo{T}}},
    }
end

const SplitComplexIndicatorEqualTo{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitComplexIndicatorEqualToBridge{T},OT}

function _add_constraint_if_nonzero(model, A, f, x, rhs::T) where {T}
    if iszero(f) && iszero(rhs)
        return nothing
    end
    g = MOI.Utilities.operate(vcat, T, x, f)
    return MOI.add_constraint(model, g, MOI.Indicator{A}(MOI.EqualTo(rhs)))
end

function bridge_constraint(
    ::Type{SplitComplexIndicatorEqualToBridge{T,F,G,A}},
    model::MOI.ModelLike,
    func::G,
    set::MOI.Indicator{A,MOI.EqualTo{Complex{T}}},
) where {T,F,G,A}
    @assert MOI.output_dimension(func) == 2
    scalars = MOI.Utilities.scalarize(func)
    x, f = convert(MOI.VariableIndex, scalars[1]), scalars[2]
    rhs = set.set.value
    real_ci = _add_constraint_if_nonzero(model, A, real(f), x, real(rhs))
    imag_f = MOI.Utilities.operate(imag, T, f)
    imag_ci = _add_constraint_if_nonzero(model, A, imag_f, x, imag(rhs))
    return SplitComplexIndicatorEqualToBridge{T,F,G,A}(real_ci, imag_ci)
end

# We don't support `MOI.VariableIndex` as it would be a self-loop in the bridge
# graph.
function MOI.supports_constraint(
    ::Type{<:SplitComplexIndicatorEqualToBridge{T}},
    ::Type{<:MOI.Utilities.TypedLike{Complex{T}}},
    ::Type{MOI.Indicator{A,MOI.EqualTo{Complex{T}}}},
) where {T,A}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitComplexIndicatorEqualToBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{SplitComplexIndicatorEqualToBridge{T,F,G,A}},
) where {T,F,G,A}
    return Tuple{Type,Type}[(F, MOI.Indicator{A,MOI.EqualTo{T}})]
end

function concrete_bridge_type(
    ::Type{<:SplitComplexIndicatorEqualToBridge{T}},
    G::Type{<:MOI.Utilities.TypedLike},
    ::Type{MOI.Indicator{A,MOI.EqualTo{Complex{T}}}},
) where {T,A}
    F = MA.promote_operation(imag, G)
    return SplitComplexIndicatorEqualToBridge{T,F,G,A}
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SplitComplexIndicatorEqualToBridge{T,F,G,A},
) where {T,F,G,A}
    H = MOI.Utilities.scalar_type(G)
    h = zero(H)
    x = nothing
    if bridge.real_constraint !== nothing
        f = MOI.get(model, attr, bridge.real_constraint)
        f_scalars = MOI.Utilities.scalarize(f)
        x = convert(MOI.VariableIndex, f_scalars[1])
        MOI.Utilities.operate!(+, Complex{T}, h, convert(H, f_scalars[2]))
    end
    if bridge.imag_constraint !== nothing
        f = MOI.get(model, attr, bridge.imag_constraint)
        f_scalars = MOI.Utilities.scalarize(f)
        x = convert(MOI.VariableIndex, f_scalars[1])
        MOI.Utilities.operate!(+, Complex{T}, h, im * f_scalars[2])
    end
    return MOI.Utilities.operate(vcat, Complex{T}, x, h)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitComplexIndicatorEqualToBridge{T,F,G,A},
) where {T,F,G,A}
    rhs = zero(T) + zero(T) * im
    if bridge.real_constraint !== nothing
        set = MOI.get(model, MOI.ConstraintSet(), bridge.real_constraint)
        rhs += set.set.value
    end
    if bridge.imag_constraint !== nothing
        set = MOI.get(model, MOI.ConstraintSet(), bridge.imag_constraint)
        rhs += set.set.value * im
    end
    return MOI.Indicator{A}(MOI.EqualTo(rhs))
end

function MOI.get(
    bridge::SplitComplexIndicatorEqualToBridge{T,F,G,A},
    ::MOI.NumberOfConstraints{F,MOI.Indicator{A,MOI.EqualTo{T}}},
)::Int64 where {T,F,G,A}
    return Int64(bridge.real_constraint !== nothing) +
           Int64(bridge.imag_constraint !== nothing)
end

function MOI.get(
    bridge::SplitComplexIndicatorEqualToBridge{T,F,G,A},
    ::MOI.ListOfConstraintIndices{F,MOI.Indicator{A,MOI.EqualTo{T}}},
) where {T,F,G,A}
    list = MOI.ConstraintIndex{F,MOI.Indicator{A,MOI.EqualTo{T}}}[]
    if bridge.real_constraint !== nothing
        push!(list, bridge.real_constraint)
    end
    if bridge.imag_constraint !== nothing
        push!(list, bridge.imag_constraint)
    end
    return list
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::SplitComplexIndicatorEqualToBridge,
)
    if bridge.real_constraint !== nothing
        MOI.delete(model, bridge.real_constraint)
    end
    if bridge.imag_constraint !== nothing
        MOI.delete(model, bridge.imag_constraint)
    end
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    ::Type{SplitComplexIndicatorEqualToBridge{T,F,G,A}},
) where {T,F,G,A}
    return MOI.supports(
        model,
        attr,
        MOI.ConstraintIndex{F,MOI.Indicator{A,MOI.EqualTo{T}}},
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SplitComplexIndicatorEqualToBridge{T},
) where {T}
    ret = zeros(Complex{T}, 2)
    if bridge.real_constraint !== nothing
        value = MOI.get(model, attr, bridge.real_constraint)
        if value == nothing
            return nothing
        end
        ret[1] = value[1]
        ret[2] += value[2]
    end
    if bridge.imag_constraint !== nothing
        value = MOI.get(model, attr, bridge.imag_constraint)
        if value == nothing
            return nothing
        end
        ret[1] = value[1]
        ret[2] += value[2] * im
    end
    return ret
end

_pass_nothing(f::Function, x::AbstractVector) = [x[1], f(x[2])]
_pass_nothing(::Any, ::Nothing) = nothing

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitComplexIndicatorEqualToBridge{T},
    value,
) where {T}
    if bridge.real_constraint !== nothing
        MOI.set(model, attr, bridge.real_constraint, _pass_nothing(real, value))
    end
    if bridge.imag_constraint !== nothing
        MOI.set(model, attr, bridge.imag_constraint, _pass_nothing(imag, value))
    end
    return
end
