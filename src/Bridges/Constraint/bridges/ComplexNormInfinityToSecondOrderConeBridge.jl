# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ComplexNormInfinityToSecondOrderConeBridge{T} <: Bridges.Constraint.AbstractBridge

`ComplexNormInfinityToSecondOrderConeBridge` implements the following
reformulation:

  * ``(t, x) \\in NormInfinity(1+d)`` into ``(t, real(x_i), imag(x_i)) \\in SecondOrderCone()``
    for all ``i``.

## Source node

`ComplexNormInfinityToSecondOrderConeBridge` supports:

  * [`MOI.VectorAffineFunction{Complex{T}}`](@ref) in [`MOI.NormInfinityCone`](@ref)

## Target nodes

`ComplexNormInfinityToSecondOrderConeBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.SecondOrderCone`](@ref)
"""
struct ComplexNormInfinityToSecondOrderConeBridge{T} <: AbstractBridge
    ci::Vector{
        MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.SecondOrderCone},
    }
end

const ComplexNormInfinityToSecondOrderCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ComplexNormInfinityToSecondOrderConeBridge{T},OT}

function bridge_constraint(
    ::Type{ComplexNormInfinityToSecondOrderConeBridge{T}},
    model::MOI.ModelLike,
    f::MOI.VectorAffineFunction{Complex{T}},
    s::MOI.NormInfinityCone,
) where {T}
    fi_s = MOI.Utilities.scalarize(f)
    if !iszero(imag(fi_s[1]))
        msg = "The epigraph variable `t` in `[t; x] in NormInfinityCone()` must be real. It is: $(fi_s[1])"
        throw(MOI.AddConstraintNotAllowed{typeof(f),typeof(s)}(msg))
    end
    t = real(fi_s[1])
    cis = MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.SecondOrderCone}[]
    for fi in fi_s[2:end]
        ci = MOI.add_constraint(
            model,
            MOI.Utilities.operate(vcat, T, t, real(fi), imag(fi)),
            MOI.SecondOrderCone(3),
        )
        push!(cis, ci)
    end
    return ComplexNormInfinityToSecondOrderConeBridge{T}(cis)
end

function MOI.supports_constraint(
    ::Type{<:ComplexNormInfinityToSecondOrderConeBridge{T}},
    ::Type{MOI.VectorAffineFunction{Complex{T}}},
    ::Type{MOI.NormInfinityCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:ComplexNormInfinityToSecondOrderConeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:ComplexNormInfinityToSecondOrderConeBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.VectorAffineFunction{T}, MOI.SecondOrderCone)]
end

function concrete_bridge_type(
    ::Type{<:ComplexNormInfinityToSecondOrderConeBridge{T}},
    ::Type{MOI.VectorAffineFunction{Complex{T}}},
    ::Type{MOI.NormInfinityCone},
) where {T}
    return ComplexNormInfinityToSecondOrderConeBridge{T}
end

function MOI.get(
    ::ComplexNormInfinityToSecondOrderConeBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return 0
end

function MOI.get(
    bridge::ComplexNormInfinityToSecondOrderConeBridge{T},
    ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.SecondOrderCone},
)::Int64 where {T}
    return length(bridge.ci)
end

function MOI.get(
    bridge::ComplexNormInfinityToSecondOrderConeBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    },
) where {T}
    return copy(bridge.ci)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ComplexNormInfinityToSecondOrderConeBridge,
)
    MOI.delete(model, bridge.ci)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ComplexNormInfinityToSecondOrderConeBridge{T},
) where {T}
    elements = MOI.ScalarAffineFunction{Complex{T}}[]
    for ci in bridge.ci
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        fi_s = MOI.Utilities.scalarize(f)
        if isempty(elements)
            push!(elements, fi_s[1])
        end
        fi = MOI.Utilities.operate(
            +,
            Complex{T},
            (one(T) + zero(T) * im) * fi_s[2],
            (zero(T) + one(T) * im) * fi_s[3],
        )
        push!(elements, fi)
    end
    return MOI.Utilities.operate(vcat, Complex{T}, elements...)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ComplexNormInfinityToSecondOrderConeBridge,
)
    return MOI.NormInfinityCone(1 + length(bridge.ci))
end
