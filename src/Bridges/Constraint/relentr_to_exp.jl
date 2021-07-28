"""
    RelativeEntropyBridge{T}

The `RelativeEntropyCone` is representable with exponential cone and LP constraints, since
``u \\ge \\sum_{i=1}^n w_i \\log (\\frac{w_i}{v_i})`` if and only if there exists a vector
``y`` such that ``u \\ge \\sum_i y_i`` and ``y_i \\ge w_i \\log (\\frac{w_i}{v_i})`` or
equivalently ``v_i \\ge w_i \\exp (\\frac{-y_i}{w_i})`` or equivalently
``(-y_i, w_i, v_i) \\in ExponentialCone``, for all ``i``.
"""
struct RelativeEntropyBridge{T,F,G,H} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    ge_index::CI{F,MOI.GreaterThan{T}}
    exp_indices::Vector{CI{G,MOI.ExponentialCone}}
end

function bridge_constraint(
    ::Type{RelativeEntropyBridge{T,F,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.RelativeEntropyCone,
) where {T,F,G,H}
    f_scalars = MOIU.eachscalar(f)
    d = MOI.dimension(s)
    v_dim = div(d - 1, 2)
    y = MOI.add_variables(model, v_dim)
    rhs = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), y), zero(T))
    ge_index = MOIU.normalize_and_add_constraint(
        model,
        MOIU.operate(-, T, f_scalars[1], rhs),
        MOI.GreaterThan(zero(T)),
        allow_modify_function = true,
    )
    w_start = 1 + v_dim
    exp_funcs = [
        MOIU.operate(
            vcat,
            T,
            MOIU.operate(-, T, MOI.SingleVariable(y[i])),
            f_scalars[w_start+i],
            f_scalars[1+i],
        ) for i in 1:v_dim
    ]
    exp_indices = [
        MOI.add_constraint(model, exp_func_i, MOI.ExponentialCone()) for
        exp_func_i in exp_funcs
    ]
    return RelativeEntropyBridge{T,F,G,H}(y, ge_index, exp_indices)
end

function MOI.supports_constraint(
    ::Type{RelativeEntropyBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RelativeEntropyCone},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:RelativeEntropyBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{RelativeEntropyBridge{T,F,G,H}},
) where {T,F,G,H}
    return Tuple{Type,Type}[(F, MOI.GreaterThan{T}), (G, MOI.ExponentialCone)]
end

function concrete_bridge_type(
    ::Type{<:RelativeEntropyBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RelativeEntropyCone},
) where {T}
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(-, T, S, S)
    G = MOIU.promote_operation(
        vcat,
        T,
        MOIU.promote_operation(-, T, MOI.SingleVariable),
        S,
    )
    return RelativeEntropyBridge{T,F,G,H}
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::RelativeEntropyBridge, ::MOI.NumberOfVariables)
    return length(bridge.y)
end

function MOI.get(bridge::RelativeEntropyBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.y)
end

function MOI.get(
    bridge::RelativeEntropyBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.GreaterThan{T}},
) where {T,F}
    return 1
end

function MOI.get(
    bridge::RelativeEntropyBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.ExponentialCone},
) where {T,F,G}
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

# References
function MOI.delete(model::MOI.ModelLike, bridge::RelativeEntropyBridge)
    for exp_index_i in bridge.exp_indices
        MOI.delete(model, exp_index_i)
    end
    MOI.delete(model, bridge.ge_index)
    MOI.delete(model, bridge.y)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::RelativeEntropyBridge{T,F,G,H},
) where {T,F,G,H}
    func = MOIU.zero_with_output_dimension(G, 1 + 2 * length(bridge.y))
    MOIU.operate_output_index!(
        +,
        T,
        1,
        func,
        MOI.get(model, MOI.ConstraintFunction(), bridge.ge_index),
    )
    w_start = 1 + length(bridge.y)
    for i in eachindex(bridge.y)
        exp_func_i = MOIU.eachscalar(
            MOI.get(model, MOI.ConstraintFunction(), bridge.exp_indices[i]),
        )
        MOIU.operate_output_index!(-, T, 1, func, exp_func_i[1])
        MOIU.operate_output_index!(+, T, 1 + i, func, exp_func_i[3])
        MOIU.operate_output_index!(+, T, w_start + i, func, exp_func_i[2])
    end
    return MOIU.convert_approx(H, MOIU.remove_variable(func, bridge.y))
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::RelativeEntropyBridge,
)
    return MOI.RelativeEntropyCone(1 + 2 * length(bridge.y))
end

function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:RelativeEntropyBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::RelativeEntropyBridge{T},
) where {T}
    primal = zeros(T, 1 + 2 * length(bridge.y))
    primal[1] = MOI.get(model, attr, bridge.ge_index)
    w_start = 1 + length(bridge.y)
    for i in eachindex(bridge.y)
        exp_primal_i = MOI.get(model, attr, bridge.exp_indices[i])
        primal[1] -= exp_primal_i[1]
        primal[1+i] = exp_primal_i[3]
        primal[w_start+i] = exp_primal_i[2]
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
    v_value = value[2:(v_dim+1)]
    w_value = value[(v_dim+2):end]
    y_value = [w_i * log(w_i / v_i) for (v_i, w_i) in zip(v_value, w_value)]
    MOI.set(
        model,
        attr,
        bridge.ge_index,
        value[1] - reduce(+, y_value, init = zero(T)),
    )
    for i in 1:v_dim
        MOI.set(
            model,
            attr,
            bridge.exp_indices[i],
            [-y_value[i], w_value[i], v_value[i]],
        )
        MOI.set(model, MOI.VariablePrimalStart(), bridge.y[i], y_value[i])
    end
    return
end
# Given a is dual on u - sum(y) >= 0 and (b_i, c_i, d_i) is dual on (-y_i, w_i, v_i)
# in ExponentialCone, the dual on (u, v, w) in RelativeEntropyCone is (a, d_i, c_i).
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::RelativeEntropyBridge{T},
) where {T}
    dual = zeros(T, 1 + 2 * length(bridge.y))
    dual[1] = MOI.get(model, attr, bridge.ge_index)[1]
    w_start = 1 + length(bridge.y)
    for i in eachindex(bridge.y)
        exp_dual_i = MOI.get(model, attr, bridge.exp_indices[i])
        dual[1+i] = exp_dual_i[3]
        dual[w_start+i] = exp_dual_i[2]
    end
    return dual
end
# Given constraint dual start of (u, v, w), constraint dual on GreaterThan is u
# and on exponential cone constraint i is (r_i, w_i, v_i), but since y_i is free,
# its dual is 0, so we have -r_i + u == 0 hence r_i = u.
# Note: alternatively, we could use the Lambert W function to calculate
# r_i = exp(W(-w_i / (-v_i * e))) * (-v_i * e), but this is more complicated.
function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintDualStart,
    bridge::RelativeEntropyBridge,
    value,
)
    u = value[1]
    MOI.set(model, MOI.ConstraintDualStart(), bridge.ge_index, u)
    w_start = 1 + length(bridge.y)
    for i in eachindex(bridge.y)
        MOI.set(
            model,
            MOI.ConstraintDualStart(),
            bridge.exp_indices[i],
            [u, value[w_start+i], value[1+i]],
        )
    end
    return
end
