# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    RelativeEntropyBridge{T,F,G,H} <: Bridges.Constraint.AbstractBridge

`RelativeEntropyBridge` implements the following reformulation that converts a
[`MOI.RelativeEntropyCone`](@ref) into an [`MOI.ExponentialCone`](@ref):

  * ``u \\ge \\sum_{i=1}^n w_i \\log \\left(\\frac{w_i}{v_i}\\right)`` into
    ``y_i \\ge 0``, ``\\sum_{i=1}^n y_i``, and
    ``(-y_i, w_i, v_i) \\in ExponentialCone``.

## Source node

`RelativeEntropyBridge` supports:

  * `H` in [`MOI.RelativeEntropyCone`](@ref)

## Target nodes

`RelativeEntropyBridge` creates:

  * `F` in [`MOI.GreaterThan{T}`](@ref)
  * `G` in [`MOI.ExponentialCone`](@ref)
"""
struct RelativeEntropyBridge{T,F,G,H} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    ge_index::MOI.ConstraintIndex{F,MOI.GreaterThan{T}}
    exp_indices::Vector{MOI.ConstraintIndex{G,MOI.ExponentialCone}}
end

const RelativeEntropy{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RelativeEntropyBridge{T},OT}

function bridge_constraint(
    ::Type{RelativeEntropyBridge{T,F,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.RelativeEntropyCone,
) where {T,F,G,H}
    f_scalars = MOI.Utilities.eachscalar(f)
    d = MOI.dimension(s)
    v_dim = div(d - 1, 2)
    y = MOI.add_variables(model, v_dim)
    rhs = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), y), zero(T))
    ge_index = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate(-, T, f_scalars[1], rhs),
        MOI.GreaterThan(zero(T));
        allow_modify_function = true,
    )
    exp_indices = MOI.ConstraintIndex{G,MOI.ExponentialCone}[]
    for i in 1:v_dim
        fi = MOI.Utilities.operate(
            vcat,
            T,
            MOI.Utilities.operate(-, T, y[i]), # -y
            f_scalars[1+i+v_dim],              # u
            f_scalars[1+i],                    # v
        )
        push!(exp_indices, MOI.add_constraint(model, fi, MOI.ExponentialCone()))
    end
    return RelativeEntropyBridge{T,F,G,H}(y, ge_index, exp_indices)
end

function MOI.supports_constraint(
    ::Type{RelativeEntropyBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RelativeEntropyCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:RelativeEntropyBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{RelativeEntropyBridge{T,F,G,H}},
) where {T,F,G,H}
    return Tuple{Type,Type}[(F, MOI.GreaterThan{T}), (G, MOI.ExponentialCone)]
end

function concrete_bridge_type(
    ::Type{<:RelativeEntropyBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RelativeEntropyCone},
) where {T}
    S = MOI.Utilities.scalar_type(H)
    F = MOI.Utilities.promote_operation(-, T, S, S)
    Y = MOI.Utilities.promote_operation(-, T, MOI.VariableIndex)
    G = MOI.Utilities.promote_operation(vcat, T, Y, S)
    return RelativeEntropyBridge{T,F,G,H}
end

function MOI.get(bridge::RelativeEntropyBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.y)
end

function MOI.get(bridge::RelativeEntropyBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.y)
end

function MOI.get(
    ::RelativeEntropyBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.GreaterThan{T}},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::RelativeEntropyBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.ExponentialCone},
)::Int64 where {T,F,G}
    return length(bridge.y)
end

function MOI.get(
    bridge::RelativeEntropyBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.GreaterThan{T}},
) where {T,F}
    return [bridge.ge_index]
end

function MOI.get(
    bridge::RelativeEntropyBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.ExponentialCone},
) where {T,F,G}
    return copy(bridge.exp_indices)
end

function MOI.delete(model::MOI.ModelLike, bridge::RelativeEntropyBridge)
    for exp_index_i in bridge.exp_indices
        MOI.delete(model, exp_index_i)
    end
    MOI.delete(model, bridge.ge_index)
    MOI.delete(model, bridge.y)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::RelativeEntropyBridge{T,F,G,H},
) where {T,F,G,H}
    func = MOI.Utilities.zero_with_output_dimension(G, 1 + 2 * length(bridge.y))
    sum_y = MOI.get(model, MOI.ConstraintFunction(), bridge.ge_index)
    MOI.Utilities.operate_output_index!(+, T, 1, func, sum_y)
    w_start = 1 + length(bridge.y)
    for i in eachindex(bridge.y)
        exp_func_i = MOI.Utilities.eachscalar(
            MOI.get(model, MOI.ConstraintFunction(), bridge.exp_indices[i]),
        )
        MOI.Utilities.operate_output_index!(-, T, 1, func, exp_func_i[1])
        MOI.Utilities.operate_output_index!(+, T, 1 + i, func, exp_func_i[3])
        MOI.Utilities.operate_output_index!(
            +,
            T,
            w_start + i,
            func,
            exp_func_i[2],
        )
    end
    f = MOI.Utilities.remove_variable(func, bridge.y)
    return MOI.Utilities.convert_approx(H, f)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::RelativeEntropyBridge,
)
    return MOI.RelativeEntropyCone(1 + 2 * length(bridge.y))
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{RelativeEntropyBridge{T,F,G,H}},
) where {T,F,G,H}
    ci_1 = MOI.ConstraintIndex{F,MOI.GreaterThan{T}}
    ci_2 = MOI.ConstraintIndex{G,MOI.ExponentialCone}
    return MOI.supports(model, attr, ci_1) && MOI.supports(model, attr, ci_2)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::RelativeEntropyBridge{T},
) where {T}
    start = MOI.get(model, attr, bridge.ge_index)
    if start === nothing
        return nothing
    end
    primal = zeros(T, 1 + 2 * length(bridge.y))
    primal[1] = start
    for i in eachindex(bridge.y)
        primal_i = MOI.get(model, attr, bridge.exp_indices[i])
        primal[1] -= primal_i[1]
        primal[1+i] = primal_i[3]
        primal[1+i+length(bridge.y)] = primal_i[2]
    end
    return primal
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::RelativeEntropyBridge{T},
    value,
) where {T}
    v_dim = length(bridge.y)
    v = value[2:(v_dim+1)]
    w = value[(v_dim+2):end]
    y = [w_i * log(w_i / v_i) for (v_i, w_i) in zip(v, w)]
    MOI.set(model, attr, bridge.ge_index, value[1] - sum(y))
    for i in 1:v_dim
        start = [-y[i], w[i], v[i]]
        MOI.set(model, attr, bridge.exp_indices[i], start)
        MOI.set(model, MOI.VariablePrimalStart(), bridge.y[i], y[i])
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::RelativeEntropyBridge{T},
    ::Nothing,
) where {T}
    MOI.set(model, attr, bridge.ge_index, nothing)
    for i in 1:length(bridge.exp_indices)
        MOI.set(model, attr, bridge.exp_indices[i], nothing)
        MOI.set(model, MOI.VariablePrimalStart(), bridge.y[i], nothing)
    end
    return
end

# Given a is dual on u - sum(y) >= 0 and (b_i, c_i, d_i) is dual on
# (-y_i, w_i, v_i) in ExponentialCone, the dual on
# (u, v, w) in RelativeEntropyCone is (a, d_i, c_i).
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::RelativeEntropyBridge{T},
) where {T}
    value = MOI.get(model, attr, bridge.ge_index)
    if value === nothing
        return nothing
    end
    dual = zeros(T, 1 + 2 * length(bridge.y))
    dual[1] = value[1]
    for (i, ci) in enumerate(bridge.exp_indices)
        dual_i = MOI.get(model, attr, ci)
        dual[1+i] = dual_i[3]
        dual[1+i+length(bridge.y)] = dual_i[2]
    end
    return dual
end

# Given constraint dual start of (u, v, w), constraint dual on GreaterThan is u
# and on exponential cone constraint i is (r_i, w_i, v_i), but since y_i is
# free, its dual is 0, so we have -r_i + u == 0 hence r_i = u.
# Note: alternatively, we could use the Lambert W function to calculate
# r_i = exp(W(-w_i / (-v_i * e))) * (-v_i * e), but this is more complicated.
function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::RelativeEntropyBridge,
    value,
)
    MOI.set(model, attr, bridge.ge_index, value[1])
    for i in eachindex(bridge.y)
        start = [value[1], value[1+i+length(bridge.y)], value[1+i]]
        MOI.set(model, attr, bridge.exp_indices[i], start)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::RelativeEntropyBridge,
    ::Nothing,
)
    MOI.set(model, attr, bridge.ge_index, nothing)
    for ci in bridge.exp_indices
        MOI.set(model, attr, ci, nothing)
    end
    return
end
