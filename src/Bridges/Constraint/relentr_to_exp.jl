"""
    RelativeEntropyBridge{T}

The `RelativeEntropyCone` is representable with exponential cone and LP constraints, since
``u \\ge \\sum_{i=1}^n w_i \\log (\\frac{w_i}{v_i})`` if and only if there exists a vector
``y`` such that ``u \\ge \\sum_i y_i`` and ``y_i \\ge w_i \\log (\\frac{w_i}{v_i})`` or
equivalently ``v_i \\ge w_i \\exp (\\frac{-y_i}{w_i})`` or equivalently
``(-y_i, w_i, v_i) \\in ExponentialCone``, for all ``i``.
"""
struct RelativeEntropyBridge{T, F, G, H} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    ge_index::CI{F, MOI.GreaterThan{T}}
    exp_indices::Vector{CI{G, MOI.ExponentialCone}}
end
function bridge_constraint(::Type{RelativeEntropyBridge{T, F, G, H}}, model::MOI.ModelLike, f::H, s::MOI.RelativeEntropyCone) where {T, F, G, H}
    f_scalars = MOIU.eachscalar(f)
    d = MOI.dimension(s)
    v_dim = div(d - 1, 2)
    y = MOI.add_variables(model, v_dim)
    ge_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate(sum, T, y)), MOI.GreaterThan(zero(T)), allow_modify_function=true)
    w_start = 1 + v_dim
    exp_funcs = [MOIU.operate(vcat, T, MOIU.operate(-, T, MOI.SingleVariable(y[i])), f_scalars[w_start + i], f_scalars[1 + i]) for i in 1:v_dim]
    exp_indices = [MOI.add_constraint(model, exp_func_i, MOI.ExponentialCone()) for exp_func_i in exp_funcs]
    return RelativeEntropyBridge{T, F, G, H}(y, ge_index, exp_indices)
end

MOI.supports_constraint(::Type{RelativeEntropyBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.RelativeEntropyCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:RelativeEntropyBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{RelativeEntropyBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.ExponentialCone)]
function concrete_bridge_type(::Type{<:RelativeEntropyBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.RelativeEntropyCone}) where T
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(-, T, S, S)
    G = MOIU.promote_operation(vcat, T, MOIU.promote_operation(-, T, MOI.SingleVariable), S)
    return RelativeEntropyBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::RelativeEntropyBridge, ::MOI.NumberOfVariables) = length(b.y)
MOI.get(b::RelativeEntropyBridge, ::MOI.ListOfVariableIndices) = b.y
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F, G, H} = 1
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.NumberOfConstraints{G, MOI.ExponentialCone}) where {T, F, G, H} = length(b.y)
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F, G, H} = [b.ge_index]
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{G, MOI.ExponentialCone}) where {T, F, G, H} = b.exp_indices

# References
function MOI.delete(model::MOI.ModelLike, c::RelativeEntropyBridge)
    for exp_index_i in c.exp_indices
        MOI.delete(model, exp_index_i)
    end
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.y)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::RelativeEntropyBridge{T, F, G, H}) where {T, F, G, H}
    func = MOIU.zero_with_output_dimension(G, 1 + 2 * length(c.y))
    MOIU.operate_output_index!(+, T, 1, func, MOI.get(model, MOI.ConstraintFunction(), c.ge_index))
    w_start = 1 + length(c.y)
    for i in eachindex(c.y)
        exp_func_i = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.exp_indices[i]))
        MOIU.operate_output_index!(-, T, 1, func, exp_func_i[1])
        MOIU.operate_output_index!(+, T, 1 + i, func, exp_func_i[3])
        MOIU.operate_output_index!(+, T, w_start + i, func, exp_func_i[2])
    end
    return MOIU.convert_approx(H, MOIU.remove_variable(func, c.y))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::RelativeEntropyBridge) = MOI.RelativeEntropyCone(1 + 2 * length(c.y))
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::RelativeEntropyBridge{T, F, G, H}) where {T, F, G, H}
    primal = zeros(T, 1 + 2 * length(c.y))
    primal[1] = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    w_start = 1 + length(c.y)
    for i in eachindex(c.y)
        exp_primal_i = MOI.get(model, MOI.ConstraintPrimal(), c.exp_indices[i])
        primal[1] -= exp_primal_i[1]
        primal[1 + i] = exp_primal_i[3]
        primal[w_start + i] = exp_primal_i[2]
    end
    return primal
end
# Given a is dual on u - sum(y) >= 0 and (b_i, c_i, d_i) is dual on (-y_i, w_i, v_i)
# in ExponentialCone, the dual on (u, v, w) in RelativeEntropyCone is (a, d_i, c_i).
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::RelativeEntropyBridge{T, F, G, H}) where {T, F, G, H}
    dual = zeros(T, 1 + 2 * length(c.y))
    dual[1] = MOI.get(model, MOI.ConstraintDual(), c.ge_index)[1]
    w_start = 1 + length(c.y)
    for i in eachindex(c.y)
        exp_dual_i = MOI.get(model, MOI.ConstraintDual(), c.exp_indices[i])
        dual[1 + i] = exp_dual_i[3]
        dual[w_start + i] = exp_dual_i[2]
    end
    return dual
end
