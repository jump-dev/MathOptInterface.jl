# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    GeoMeantoRelEntrBridge{T,F,G,H} <: Bridges.Constraint.AbstractBridge

`GeoMeantoRelEntrBridge` implements the following reformulation:

  * ``(u, w) \\in GeometricMeanCone`` into
    ``(0, w, (u + y)\\mathbf{1})\\in RelativeEntropyCone`` and ``y \\ge 0``

## Source node

`GeoMeantoRelEntrBridge` supports:

  * `H` in [`MOI.GeometricMeanCone`](@ref)

## Target nodes

`GeoMeantoRelEntrBridge` creates:

  * `G` in [`MOI.RelativeEntropyCone`](@ref)
  * `F` in [`MOI.Nonnegatives`](@ref)

## Derivation

The derivation of the bridge is as follows:

```math
\\begin{aligned}
(u, w) \\in GeometricMeanCone \\iff & u \\le \\left(\\prod_{i=1}^n w_i\\right)^{1/n} \\\\
\\iff & 0 \\le u + y \\le \\left(\\prod_{i=1}^n w_i\\right)^{1/n}, y \\ge 0 \\\\
\\iff & 1 \\le \\frac{\\left(\\prod_{i=1}^n w_i\\right)^{1/n}}{u + y}, y \\ge 0 \\\\
\\iff & 1 \\le \\left(\\prod_{i=1}^n \\frac{w_i}{u + y}\\right)^{1/n}, y \\ge 0 \\\\
\\iff & 0 \\le \\sum_{i=1}^n \\log\\left(\\frac{w_i}{u + y}\\right), y \\ge 0 \\\\
\\iff & 0 \\ge \\sum_{i=1}^n \\log\\left(\\frac{u + y}{w_i}\\right), y \\ge 0 \\\\
\\iff & 0 \\ge \\sum_{i=1}^n (u + y) \\log\\left(\\frac{u + y}{w_i}\\right), y \\ge 0 \\\\
\\iff & (0, w, (u + y)\\mathbf{1}) \\in RelativeEntropyCone, y \\ge 0 \\\\
\\end{aligned}
```

This derivation assumes that ``u + y > 0``, which is enforced by the relative
entropy cone.
"""
struct GeoMeantoRelEntrBridge{T,F,G,H} <: AbstractBridge
    y::MOI.VariableIndex
    nn_index::MOI.ConstraintIndex{F,MOI.Nonnegatives} # for y >= 0
    relentr_index::MOI.ConstraintIndex{G,MOI.RelativeEntropyCone}
end

const GeoMeantoRelEntr{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GeoMeantoRelEntrBridge{T},OT}

function bridge_constraint(
    ::Type{GeoMeantoRelEntrBridge{T,F,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.GeometricMeanCone,
) where {T,F,G,H}
    f_scalars = MOI.Utilities.eachscalar(f)
    (y, nn_index) = MOI.add_constrained_variables(model, MOI.Nonnegatives(1))
    w_func = MOI.Utilities.vectorize(
        fill(
            MOI.Utilities.operate(+, T, f_scalars[1], y[1]),
            MOI.dimension(s) - 1,
        ),
    )
    relentr_func = MOI.Utilities.operate(
        vcat,
        T,
        zero(MOI.ScalarAffineFunction{Float64}),
        f_scalars[2:end],
        w_func,
    )
    relentr_index = MOI.add_constraint(
        model,
        relentr_func,
        MOI.RelativeEntropyCone(MOI.output_dimension(relentr_func)),
    )
    return GeoMeantoRelEntrBridge{T,F,G,H}(y[1], nn_index, relentr_index)
end

function MOI.supports_constraint(
    ::Type{<:GeoMeantoRelEntrBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:GeoMeantoRelEntrBridge},
)
    return Tuple{Type}[(MOI.Nonnegatives,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:GeoMeantoRelEntrBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[(G, MOI.RelativeEntropyCone)]
end

function concrete_bridge_type(
    ::Type{<:GeoMeantoRelEntrBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    F = MOI.VectorOfVariables
    S = MOI.Utilities.scalar_type(H)
    G = MOI.Utilities.promote_operation(
        vcat,
        T,
        T,
        S,
        MOI.Utilities.promote_operation(+, T, S, MOI.VariableIndex),
    )
    return GeoMeantoRelEntrBridge{T,F,G,H}
end

MOI.get(::GeoMeantoRelEntrBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(bridge::GeoMeantoRelEntrBridge, ::MOI.ListOfVariableIndices)
    return [bridge.y]
end

function MOI.get(
    ::GeoMeantoRelEntrBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Nonnegatives},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    ::GeoMeantoRelEntrBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.RelativeEntropyCone},
)::Int64 where {T,F,G}
    return 1
end

function MOI.get(
    bridge::GeoMeantoRelEntrBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.Nonnegatives},
) where {T,F}
    return [bridge.nn_index]
end

function MOI.get(
    bridge::GeoMeantoRelEntrBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.RelativeEntropyCone},
) where {T,F,G}
    return [bridge.relentr_index]
end

function MOI.delete(model::MOI.ModelLike, bridge::GeoMeantoRelEntrBridge)
    MOI.delete(model, bridge.relentr_index)
    MOI.delete(model, bridge.nn_index)
    MOI.delete(model, bridge.y)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::GeoMeantoRelEntrBridge{T,F,G,H},
) where {T,F,G,H}
    relentr_func = MOI.Utilities.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.relentr_index),
    )
    d = div(length(relentr_func) - 1, 2)
    u_func = MOI.Utilities.remove_variable(
        MOI.Utilities.operate(-, T, relentr_func[end], bridge.y),
        bridge.y,
    )
    w_func = relentr_func[2:(1+d)]
    return MOI.Utilities.convert_approx(
        H,
        MOI.Utilities.operate(vcat, T, u_func, w_func),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::GeoMeantoRelEntrBridge,
)
    return MOI.GeometricMeanCone(
        1 + div(
            MOI.dimension(
                MOI.get(model, MOI.ConstraintSet(), bridge.relentr_index),
            ) - 1,
            2,
        ),
    )
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{GeoMeantoRelEntrBridge{T,F,G,H}},
) where {T,F,G,H}
    ci_1 = MOI.ConstraintIndex{F,MOI.Nonnegatives}
    ci_2 = MOI.ConstraintIndex{G,MOI.RelativeEntropyCone}
    return MOI.supports(model, attr, ci_1) && MOI.supports(model, attr, ci_2)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::GeoMeantoRelEntrBridge,
)
    relentr_primal = MOI.get(model, attr, bridge.relentr_index)
    d = div(length(relentr_primal) - 1, 2)
    y_val = MOI.get(model, attr, bridge.nn_index)[1]
    u_val = sum(relentr_primal[(2+d):end]) / d - y_val
    return vcat(u_val, relentr_primal[2:(1+d)])
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::GeoMeantoRelEntrBridge,
    value,
)
    u_val = value[1]
    u_pos = -min(zero(u_val), u_val)
    d = length(value) - 1
    MOI.set(model, MOI.VariablePrimalStart(), bridge.y, u_pos)
    MOI.set(model, attr, bridge.nn_index, [u_pos])
    MOI.set(
        model,
        attr,
        bridge.relentr_index,
        vcat(0, value[2:end], fill(u_pos + u_val, d)),
    )
    return
end

# Given a is dual on y >= 0 and (b, c, d) is dual on RelativeEntropyCone
# constraint, dual on (u, w) in GeometricMeanCone is (-a, c). Note that
# sum(d) = -a, so we could instead use (sum(d), c).
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::GeoMeantoRelEntrBridge,
)
    u_dual = -MOI.get(model, attr, bridge.nn_index)[1]
    relentr_dual = MOI.get(model, attr, bridge.relentr_index)
    d = div(length(relentr_dual) - 1, 2)
    w_dual = relentr_dual[2:(d+1)]
    return vcat(u_dual, w_dual)
end

# Given GeometricMeanCone constraint dual start of (u, w), constraint dual on
# y >= 0 is -u and on RelativeEntropyCone constraint is
# (-u/n, w, u/n * (log.(w/geomean(w)) .+ 1)).
# Note log.(w/geomean(w)) = log.(w) .- sum(log, w) / n.
function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintDualStart,
    bridge::GeoMeantoRelEntrBridge,
    value,
)
    d = length(value) - 1
    u = value[1]
    w = value[2:end]
    MOI.set(model, MOI.ConstraintDualStart(), bridge.nn_index, [-u])
    relentr_dual =
        vcat(-u / d, value[2:end], u / d * (log.(w) .+ (1 - sum(log, w) / d)))
    MOI.set(
        model,
        MOI.ConstraintDualStart(),
        bridge.relentr_index,
        relentr_dual,
    )
    return
end
