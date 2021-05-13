"""
    GeoMeantoRelEntrBridge{T}

The `geometric mean cone` is representable with a relative entropy constraint
and a nonnegative auxiliary variable.

This is because ``u \\le \\prod_{i=1}^n w_i^{1/n}`` is equivalent to
``y \\ge 0`` and ``0 \\le u + y \\le \\prod_{i=1}^n w_i^{1/n}``,
and the latter inequality is equivalent to
``1 \\le \\prod_{i=1}^n (\\frac{w_i}{u + y})^{1/n}``, which is equivalent to
``0 \\le \\sum_{i=1}^n \\log (\\frac{w_i}{u + y})^{1/n}``, which is
equivalent to ``0 \\ge \\sum_{i=1}^n (u + y) \\log (\\frac{u + y}{w_i})``.

Thus ``(u, w) \\in GeometricMeanCone(1 + n)`` is representable as ``y \\ge 0``,
``(0, w, (u + y) e) \\in RelativeEntropyCone(1 + 2n)``, where ``e`` is a vector
of ones.
"""
struct GeoMeantoRelEntrBridge{T,F,G,H} <: AbstractBridge
    y::MOI.VariableIndex
    nn_index::CI{F,MOI.Nonnegatives} # for y >= 0
    relentr_index::CI{G,MOI.RelativeEntropyCone}
end

function bridge_constraint(
    ::Type{GeoMeantoRelEntrBridge{T,F,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.GeometricMeanCone,
) where {T,F,G,H}
    f_scalars = MOIU.eachscalar(f)
    (y, nn_index) = MOI.add_constrained_variables(model, MOI.Nonnegatives(1))
    w_func = MOIU.vectorize(
        fill(
            MOIU.operate(+, T, f_scalars[1], MOI.SingleVariable(y[1])),
            MOI.dimension(s) - 1,
        ),
    )
    relentr_func = MOIU.operate(
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

function MOIB.added_constrained_variable_types(::Type{<:GeoMeantoRelEntrBridge})
    return [(MOI.Nonnegatives,)]
end

function MOIB.added_constraint_types(
    ::Type{<:GeoMeantoRelEntrBridge{T,F,G}},
) where {T,F,G}
    return [(G, MOI.RelativeEntropyCone)]
end

function concrete_bridge_type(
    ::Type{<:GeoMeantoRelEntrBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    F = MOI.VectorOfVariables
    S = MOIU.scalar_type(H)
    G = MOIU.promote_operation(
        vcat,
        T,
        T,
        S,
        MOIU.promote_operation(+, T, S, MOI.SingleVariable),
    )
    return GeoMeantoRelEntrBridge{T,F,G,H}
end

# Attributes, Bridge acting as a model
MOI.get(bridge::GeoMeantoRelEntrBridge, ::MOI.NumberOfVariables) = 1

function MOI.get(bridge::GeoMeantoRelEntrBridge, ::MOI.ListOfVariableIndices)
    return [bridge.y]
end

function MOI.get(
    ::GeoMeantoRelEntrBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.Nonnegatives},
) where {T,F}
    return 1
end

function MOI.get(
    ::GeoMeantoRelEntrBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.RelativeEntropyCone},
) where {T,F,G}
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

# References
function MOI.delete(model::MOI.ModelLike, bridge::GeoMeantoRelEntrBridge)
    MOI.delete(model, bridge.relentr_index)
    MOI.delete(model, bridge.nn_index)
    MOI.delete(model, bridge.y)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::GeoMeantoRelEntrBridge{T,F,G,H},
) where {T,F,G,H}
    relentr_func = MOIU.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.relentr_index),
    )
    d = div(length(relentr_func) - 1, 2)
    u_func = MOIU.remove_variable(
        MOIU.operate(-, T, relentr_func[end], MOI.SingleVariable(bridge.y)),
        bridge.y,
    )
    w_func = relentr_func[2:(1+d)]
    return MOIU.convert_approx(H, MOIU.operate(vcat, T, u_func, w_func))
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
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:GeoMeantoRelEntrBridge},
)
    return true
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

# Given GeometricMeanCone constraint dual start of (u, w), constraint dual on y >= 0 is -u
# and on RelativeEntropyCone constraint is (-u/n, w, u/n * (log.(w/geomean(w)) .+ 1)).
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
