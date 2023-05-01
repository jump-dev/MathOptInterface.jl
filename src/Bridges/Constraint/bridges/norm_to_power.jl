# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    NormToPowerBridge{T,F} <: Bridges.Constraint.AbstractBridge

`NormToPowerBridge` implements the following reformulation:

  * ``(t, x) \\in NormCone(p, 1+d)`` into ``(r_i, t, x_i) \\in PowerCone(1 / p)``
    for all ``i``, and ``\\sum\\limits_i r_i == t``.

## Source node

`NormToPowerBridge` supports:

  * `F` in [`MOI.NormCone`](@ref)

## Target nodes

`NormToPowerBridge` creates:

  * `F` in [`MOI.PowerCone{T}`](@ref)
  * [`MOI.ScalarAffineFunction`](@ref) in [`MOI.EqualTo`](@ref)
"""
struct NormToPowerBridge{T,F} <: AbstractBridge
    power::Vector{MOI.ConstraintIndex{F,MOI.PowerCone{T}}}
    r::Vector{MOI.VariableIndex}
    equal::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}
    set::MOI.NormCone
end

const NormToPower{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormToPowerBridge{T},OT}

function bridge_constraint(
    ::Type{NormToPowerBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.NormCone,
) where {T,F}
    d = MOI.dimension(s)
    fi_s = MOI.Utilities.eachscalar(f)
    r = MOI.add_variables(model, d - 1)
    power_ci = MOI.ConstraintIndex{F,MOI.PowerCone{T}}[
        MOI.add_constraint(
            model,
            MOI.Utilities.operate(vcat, T, r[i], fi_s[1], fi_s[i+1]),
            MOI.PowerCone(T(1 / s.p)),
        ) for i in 1:length(r)
    ]
    f = zero(MOI.ScalarAffineFunction{T})
    for ri in r
        f = MOI.Utilities.operate!(+, T, f, ri)
    end
    MOI.Utilities.operate!(-, T, f, fi_s[1])
    equal_ci = MOI.add_constraint(model, f, MOI.EqualTo(zero(T)))
    return NormToPowerBridge{T,F}(power_ci, r, equal_ci, s)
end

function MOI.supports_constraint(
    ::Type{<:NormToPowerBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:NormToPowerBridge},
)
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:NormToPowerBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[
        (F, MOI.PowerCone{T}),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:NormToPowerBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormCone},
) where {T}
    return NormToPowerBridge{T,F}
end

function MOI.get(bridge::NormToPowerBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.r)
end

function MOI.get(bridge::NormToPowerBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.r)
end

function MOI.get(
    bridge::NormToPowerBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.PowerCone{T}},
)::Int64 where {T,F}
    return length(bridge.power)
end

function MOI.get(
    bridge::NormToPowerBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.PowerCone{T}},
) where {T,F}
    return copy(bridge.power)
end

function MOI.get(
    bridge::NormToPowerBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::NormToPowerBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return [bridge.equal]
end

function MOI.delete(model::MOI.ModelLike, bridge::NormToPowerBridge)
    MOI.delete(model, bridge.power)
    MOI.delete(model, bridge.equal)
    MOI.delete(model, bridge.r)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::NormToPowerBridge{T,F},
) where {T,F}
    elements = MOI.Utilities.scalar_type(F)[]
    for ci in bridge.power
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        fi_s = MOI.Utilities.eachscalar(f)
        if isempty(elements)
            push!(elements, fi_s[2])
        end
        push!(elements, fi_s[3])
    end
    return MOI.Utilities.operate(vcat, T, elements...)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::NormToPowerBridge,
)
    return bridge.set
end
