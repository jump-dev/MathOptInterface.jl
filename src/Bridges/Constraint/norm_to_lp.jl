"""
    NormInfinityBridge{T}

The `NormInfinityCone` is representable with LP constraints, since
``t \\ge \\max_i \\lvert x_i \\rvert`` if and only if
``t \\ge x_i`` and ``t \\ge -x_i`` for all ``i``.
"""
struct NormInfinityBridge{T, F, G} <: SetMapBridge{T, MOI.Nonnegatives, MOI.NormInfinityCone, F, G}
    constraint::CI{F, MOI.Nonnegatives}
end
function concrete_bridge_type(::Type{<:NormInfinityBridge{T}}, G::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormInfinityCone}) where T
    F = MOIU.promote_operation(+, T, G, G)
    return NormInfinityBridge{T, F, G}
end

function map_set(::Type{<:NormInfinityBridge}, set::MOI.NormInfinityCone)
    return MOI.Nonnegatives(2 * (MOI.dimension(set) - 1))
end
function inverse_map_set(::Type{<:NormInfinityBridge}, set::MOI.Nonnegatives)
    return MOI.NormInfinityCone(div(MOI.dimension(set), 2) + 1)
end

function map_function(::Type{<:NormInfinityBridge{T}}, func) where T
    scalars = MOIU.eachscalar(func)
    t = scalars[1]
    lb = scalars[2:end]
    ub = MOIU.operate(-, T, lb)
    f_new = MOIU.operate(vcat, T, ub, lb)
    for i in 1:(2 * (length(scalars) - 1))
        MOIU.operate_output_index!(+, T, i, f_new, t)
    end
    return f_new
end
function inverse_map_function(::Type{<:NormInfinityBridge{T}}, func) where T
    scalars = MOIU.eachscalar(func)
    t = MOIU.operate!(/, T, sum(scalars), T(length(scalars)))
    d = div(length(scalars), 2)
    x = MOIU.operate!(/, T, MOIU.operate!(-, T, scalars[(d + 1):end], scalars[1:d]), T(2))
    return MOIU.operate(vcat, T, t, x)
end
# Given a_i is dual on t - x_i >= 0 and b_i is dual on t + x_i >= 0,
# the dual on (t, x) in NormInfinityCone is (u, v) in NormOneCone, where
# v_i = -a_i + b_i and u = sum(a) + sum(b).
function adjoint_map_function(::Type{<:NormInfinityBridge}, func)
    scalars = MOIU.eachscalar(func)
    t = sum(scalars)
    d = div(length(scalars), 2)
    x = (scalars[(d + 1):end] - scalars[1:d])
    return vcat(t, x)
end
function inverse_adjoint_map_function(::Type{<:NormInfinityBridge{T}}, func::AbstractVector{T}) where T
    # This is used by `MOI.ConstraintDualStart`.
    # The result should belong to `MOI.Nonnegatives` and the sum of the elements should
    # be `t`.
    # Only one of dual of the upper and lower bound should be active so one of the two
    # duals is zero. We know which one since they should be nonnegative.
    # Then if `t = sum abs(x_i)`, we will indeed have only one of them being zero.
    t = func[1]
    y = func[2:end]
    lb = [x > 0 ?  x : zero(x) for x in y]
    ub = [x < 0 ? -x : zero(x) for x in y]
    x = [ub; lb]
    return x .+ (t - sum(x)) / length(x)
end

"""
    NormOneBridge{T}

The `NormOneCone` is representable with LP constraints, since
``t \\ge \\sum_i \\lvert x_i \\rvert`` if and only if there exists a vector y such that
``t \\ge \\sum_i y_i`` and ``y_i \\ge x_i``, ``y_i \\ge -x_i`` for all ``i``.
"""
struct NormOneBridge{T, F, G, H} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    ge_index::CI{F, MOI.GreaterThan{T}}
    nn_index::CI{G, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NormOneBridge{T, F, G, H}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormOneCone) where {T, F, G, H}
    f_scalars = MOIU.eachscalar(f)
    d = MOI.dimension(s)
    y = MOI.add_variables(model, d - 1)
    ge_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate(sum, T, y)), MOI.GreaterThan(zero(T)), allow_modify_function=true)
    lb = f_scalars[2:d]
    ub = MOIU.operate(-, T, lb)
    lb = MOIU.operate!(+, T, lb, MOI.VectorOfVariables(y))
    ub = MOIU.operate!(+, T, ub, MOI.VectorOfVariables(y))
    f_new = MOIU.operate(vcat, T, ub, lb)
    nn_index = MOI.add_constraint(model, f_new, MOI.Nonnegatives(2d - 2))
    return NormOneBridge{T, F, G, H}(y, ge_index, nn_index)
end

MOI.supports_constraint(::Type{NormOneBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormOneCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormOneBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormOneBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.Nonnegatives)]
function concrete_bridge_type(::Type{<:NormOneBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormOneCone}) where T
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(+, T, S, S)
    G = MOIU.promote_operation(+, T, H, H)
    return NormOneBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::NormOneBridge, ::MOI.NumberOfVariables) = length(b.y)
MOI.get(b::NormOneBridge, ::MOI.ListOfVariableIndices) = b.y
MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F, G, H} = 1
MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.NumberOfConstraints{G, MOI.Nonnegatives}) where {T, F, G, H} = 1
MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F, G, H} = [b.ge_index]
MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{G, MOI.Nonnegatives}) where {T, F, G, H} = [b.nn_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormOneBridge)
    MOI.delete(model, c.nn_index)
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.y)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormOneBridge{T, F, G, H}) where {T, F, G, H}
    ge_func = MOI.get(model, MOI.ConstraintFunction(), c.ge_index)
    nn_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.nn_index))
    t = MOIU.operate!(+, T, ge_func, MOIU.operate!(/, T, sum(nn_func), T(2)))
    d = div(length(nn_func), 2)
    x = MOIU.operate!(/, T, MOIU.operate!(-, T, nn_func[(d + 1):end], nn_func[1:d]), T(2))
    return MOIU.convert_approx(H, MOIU.remove_variable(MOIU.operate(vcat, T, t, x), c.y))
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormOneBridge)
    dim = 1 + div(MOI.dimension(MOI.get(model, MOI.ConstraintSet(), c.nn_index)), 2)
    return MOI.NormOneCone(dim)
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormOneBridge)
    ge_primal = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    nn_primal = MOI.get(model, MOI.ConstraintPrimal(), c.nn_index)
    t = ge_primal + sum(nn_primal) / 2
    d = length(c.y)
    x = (nn_primal[(d + 1):end] - nn_primal[1:d]) / 2
    return vcat(t, x)
end
# Given a_i is dual on y_i - x_i >= 0 and b_i is dual on y_i + x_i >= 0 and c is dual on t - sum(y) >= 0,
# the dual on (t, x) in NormOneCone is (u, v) in NormInfinityCone, where
# v_i = -a_i + b_i and u = c.
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormOneBridge)
    t = MOI.get(model, MOI.ConstraintDual(), c.ge_index)
    nn_dual = MOI.get(model, MOI.ConstraintDual(), c.nn_index)
    d = div(length(nn_dual), 2)
    x = (nn_dual[(d + 1):end] - nn_dual[1:d])
    return vcat(t, x)
end
