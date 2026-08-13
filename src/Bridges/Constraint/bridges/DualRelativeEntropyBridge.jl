# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    DualRelativeEntropyBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`DualRelativeEntropyBridge` implements the following reformulation that converts a
[`MOI.DualRelativeEntropyCone`](@ref) into an [`MOI.DualExponentialCone`](@ref):

  * ``w_i \\ge u \\left(\\log \\left(\\frac{u}{v_i}\\right) - 1\\right)`` into
    ``(-u, w_i, v_i) \\in DualExponentialCone``.

## Source node

`DualRelativeEntropyBridge` supports:

  * `G` in [`MOI.DualRelativeEntropyCone`](@ref)

## Target nodes

`DualRelativeEntropyBridge` creates:

  * `F` in [`MOI.DualExponentialCone`](@ref)
"""
struct DualRelativeEntropyBridge{T,F,G} <: AbstractBridge
    exp_indices::Vector{MOI.ConstraintIndex{F,MOI.DualExponentialCone}}
end

const DualRelativeEntropy{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{DualRelativeEntropyBridge{T},OT}

function bridge_constraint(
    ::Type{DualRelativeEntropyBridge{T,F,G}},
    model::MOI.ModelLike,
    f::G,
    s::MOI.DualRelativeEntropyCone,
) where {T,F,G}
    f_scalars = MOI.Utilities.eachscalar(f)
    d = MOI.dimension(s)
    v_dim = div(d - 1, 2)
    exp_indices = MOI.ConstraintIndex{F,MOI.DualExponentialCone}[]
    for i in 1:v_dim
        fi = MOI.Utilities.operate(
            vcat,
            T,
            MOI.Utilities.operate(-, T, f_scalars[1]), # -u
            f_scalars[1+i+v_dim],                      # w_i
            f_scalars[1+i],                            # v_i
        )
        push!(
            exp_indices,
            MOI.add_constraint(model, fi, MOI.DualExponentialCone()),
        )
    end
    return DualRelativeEntropyBridge{T,F,G}(exp_indices)
end

function MOI.supports_constraint(
    ::Type{DualRelativeEntropyBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.DualRelativeEntropyCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:DualRelativeEntropyBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{DualRelativeEntropyBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[(F, MOI.DualExponentialCone)]
end

function concrete_bridge_type(
    ::Type{<:DualRelativeEntropyBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.DualRelativeEntropyCone},
) where {T}
    S = MOI.Utilities.scalar_type(G)
    U = MOI.Utilities.promote_operation(-, T, S, S)
    F = MOI.Utilities.promote_operation(vcat, T, U, S)
    return DualRelativeEntropyBridge{T,F,G}
end

function MOI.get(
    bridge::DualRelativeEntropyBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.DualExponentialCone},
)::Int64 where {T,F}
    return length(bridge.exp_indices)
end

function MOI.get(
    bridge::DualRelativeEntropyBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.DualExponentialCone},
) where {T,F}
    return copy(bridge.exp_indices)
end

function MOI.delete(model::MOI.ModelLike, bridge::DualRelativeEntropyBridge)
    for exp_index_i in bridge.exp_indices
        MOI.delete(model, exp_index_i)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::DualRelativeEntropyBridge{T,F,G},
) where {T,F,G}
    d = length(bridge.exp_indices)
    func = MOI.Utilities.zero_with_output_dimension(G, 1 + 2 * d)
    exp_1 = MOI.Utilities.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.exp_indices[begin]),
    )
    MOI.Utilities.operate_output_index!(-, T, 1, func, exp_1[1])
    for i in eachindex(bridge.exp_indices)
        exp_i = MOI.Utilities.eachscalar(
            MOI.get(model, MOI.ConstraintFunction(), bridge.exp_indices[i]),
        )
        MOI.Utilities.operate_output_index!(+, T, 1 + i, func, exp_i[3])
        MOI.Utilities.operate_output_index!(+, T, 1 + d + i, func, exp__i[2])
    end
    return MOI.Utilities.convert_approx(G, func)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::DualRelativeEntropyBridge,
)
    return MOI.DualRelativeEntropyCone(1 + 2 * length(bridge.exp_indices))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::DualRelativeEntropyBridge{T},
) where {T}
    d = length(bridge.exp_indices)
    primal = zeros(T, 1 + 2d)
    primal[1] = -MOI.get(model, attr, bridge.exp_indices[begin])[1]
    for i in eachindex(bridge.exp_indices)
        primal_i = MOI.get(model, attr, bridge.exp_indices[i])
        primal[1+i] = primal_i[3]
        primal[1+i+d] = primal_i[2]
    end
    return primal
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::DualRelativeEntropyBridge{T},
) where {T}
    d = length(bridge.exp_indices)
    dual = zeros(T, 1 + 2d)
    for (i, ci) in enumerate(bridge.exp_indices)
        dual_i = MOI.get(model, attr, ci)
        dual[1] -= dual_i[1]
        dual[1+i] = dual_i[3]
        dual[1+i+d] = dual_i[2]
    end
    return dual
end
