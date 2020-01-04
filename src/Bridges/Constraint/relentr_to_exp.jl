"""
    RelativeEntropyBridge{T}

The `RelativeEntropyCone` is representable with exponential cone and LP constraints, since
``u \\ge \\sum_{i=1}^n w_i \\log (\\frac{w_i}{v_i})`` if and only if there exists a vector
``y`` such that ``u \\ge \\sum_i y_i`` and ``y_i \\ge w_i \\log (\\frac{w_i}{v_i})`` or
equivalently ``v_i \\ge w_i \\exp (\\frac{-y_i}{w_i})``, for all ``i``.
"""
struct RelativeEntropyBridge{T, F, G, H} <: AbstractBridge
    v_dim::Int
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
    exp_indices = [MOI.add_constraint(model, [-y[i], f_scalars[1 + v_dim + i], f_scalars[1 + i]], MOI.ExponentialCone()) for i in 1:v_dim]
    return RelativeEntropyBridge{T, F, G, H}(v_dim, y, ge_index, exp_indices)
end

MOI.supports_constraint(::Type{RelativeEntropyBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.RelativeEntropyCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:RelativeEntropyBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{RelativeEntropyBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.ExponentialCone)]
function concrete_bridge_type(::Type{<:RelativeEntropyBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.RelativeEntropyCone}) where T
    # TODO
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(+, T, S, S)
    G = MOIU.promote_operation(+, T, H, H)
    return RelativeEntropyBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::RelativeEntropyBridge, ::MOI.NumberOfVariables) = length(b.y)
MOI.get(b::RelativeEntropyBridge, ::MOI.ListOfVariableIndices) = b.y
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F, G, H} = 1
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.NumberOfConstraints{G, MOI.ExponentialCone}) where {T, F, G, H} = b.v_dim
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F, G, H} = [b.ge_index]
MOI.get(b::RelativeEntropyBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{G, MOI.Nonnegatives}) where {T, F, G, H} = b.exp_indices

# References
function MOI.delete(model::MOI.ModelLike, c::RelativeEntropyBridge)
    MOI.delete(model, c.exp_indices)
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.y)
end

# # Attributes, Bridge acting as a constraint
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::RelativeEntropyBridge{T, F, G, H}) where {T, F, G, H}
#     ge_func = MOI.get(model, MOI.ConstraintFunction(), c.ge_index)
#     nn_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.nn_index))
#     t = MOIU.operate!(+, T, ge_func, MOIU.operate!(/, T, sum(nn_func), T(2)))
#     d = div(length(nn_func), 2)
#     x = MOIU.operate!(/, T, MOIU.operate!(-, T, nn_func[(d + 1):end], nn_func[1:d]), T(2))
#     return MOIU.convert_approx(H, MOIU.remove_variable(MOIU.operate(vcat, T, t, x), c.y))
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::RelativeEntropyBridge)
#     dim = 1 + div(MOI.dimension(MOI.get(model, MOI.ConstraintSet(), c.nn_index)), 2)
#     return MOI.RelativeEntropyCone(dim)
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::RelativeEntropyBridge)
#     ge_primal = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
#     nn_primal = MOI.get(model, MOI.ConstraintPrimal(), c.nn_index)
#     t = ge_primal + sum(nn_primal) / 2
#     d = length(c.y)
#     x = (nn_primal[(d + 1):end] - nn_primal[1:d]) / 2
#     return vcat(t, x)
# end
# # Given a_i is dual on y_i - x_i >= 0 and b_i is dual on y_i + x_i >= 0 and c is dual on t - sum(y) >= 0,
# # the dual on (t, x) in RelativeEntropyCone is (u, v) in NormInfinityCone, where
# # v_i = -a_i + b_i and u = c.
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::RelativeEntropyBridge)
#     t = MOI.get(model, MOI.ConstraintDual(), c.ge_index)
#     nn_dual = MOI.get(model, MOI.ConstraintDual(), c.nn_index)
#     d = div(length(nn_dual), 2)
#     x = (nn_dual[(d + 1):end] - nn_dual[1:d])
#     return vcat(t, x)
# end
