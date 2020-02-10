"""
    GeoMeantoExpBridge{T}
The `geometric mean cone` is representable with auxiliary variables, two linear inequality constraints, and multiple exponential cone constraints,
since ``u \\le \\prod_{i=1}^n w_i^{1/n}`` if and only if there exists a ``y`` and ``z_1, \\ldots, z_n`` such that
``u - y \\le 0``, ``\\sum(z) \\ge 0``, and ``(z_i, y, w_i) \\in ExponentialCone()``.
"""
struct GeoMeantoExpBridge{T, F, G, H, I} <: AbstractBridge
    y::MOI.VariableIndex
    z::Vector{MOI.VariableIndex}
    le_y_index::CI{F, MOI.LessThan{Float64}} # for u - y <= 0
    ge_z_index::CI{G, MOI.GreaterThan{Float64}} # for sum(z) >= 0
    exp_indices::Vector{CI{H, MOI.ExponentialCone}}
end
function bridge_constraint(::Type{GeoMeantoExpBridge{T, F, G, H, I}}, model::MOI.ModelLike, f::H, s::MOI.GeometricMeanCone) where {T, F, G, H, I}
    f_scalars = MOIU.eachscalar(f)
    w_dim = length(f_scalars) - 1
    y = MOI.add_variable(model)
    z = MOI.add_variables(model, w_dim)
    le_y_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOI.SingleVariable(y)), MOI.LessThan(zero(T)), allow_modify_function=true)
    ge_z_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(sum, T, z), MOI.GreaterThan(zero(T)), allow_modify_function=true)
    exp_funcs = [MOIU.operate(vcat, T, MOI.SingleVariable(z[i]), MOI.SingleVariable(y), f_scalars[1 + i]) for i in eachindex(z)]
    exp_indices = [MOI.add_constraint(model, exp_func_i, MOI.ExponentialCone()) for exp_func_i in exp_funcs]
    return GeoMeantoExpBridge{T, F, G, H, I}(y, z, le_y_index, ge_z_index, exp_indices)
end

MOI.supports_constraint(::Type{<:GeoMeantoExpBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.GeometricMeanCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:GeoMeantoExpBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{<:GeoMeantoExpBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.LessThan{Float64}), (G, MOI.GreaterThan{Float64}), (H, MOI.ExponentialCone)]
function concrete_bridge_type(::Type{<:GeoMeantoExpBridge{T}}, I::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.GeometricMeanCone}) where T
    S = MOIU.scalar_type(I)
    F = MOIU.promote_operation(-, T, S, MOI.SingleVariable)
    G = MOIU.promote_operation(+, T, MOI.SingleVariable, MOI.SingleVariable)
    H = MOIU.promote_operation(vcat, T, MOI.SingleVariable, MOI.SingleVariable, S)
    return GeoMeantoExpBridge{T, F, G, H, I}
end

# Attributes, Bridge acting as a model
MOI.get(bridge::GeoMeantoExpBridge, ::MOI.NumberOfVariables) = 1 + length(bridge.z)
MOI.get(bridge::GeoMeantoExpBridge, ::MOI.ListOfVariableIndices) = vcat(bridge.y, bridge.z)
MOI.get(bridge::GeoMeantoExpBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.LessThan{Float64}}) where {T, F} = 1
MOI.get(bridge::GeoMeantoExpBridge{T, F, G}, ::MOI.NumberOfConstraints{G, MOI.GreaterThan{Float64}}) where {T, F, G} = 1
MOI.get(bridge::GeoMeantoExpBridge{T, F, G, H}, ::MOI.NumberOfConstraints{H, MOI.ExponentialCone}) where {T, F, G, H} = length(bridge.z)
MOI.get(bridge::GeoMeantoExpBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.LessThan{Float64}}) where {T, F} = [bridge.le_y_index]
MOI.get(bridge::GeoMeantoExpBridge{T, F, G}, ::MOI.ListOfConstraintIndices{G, MOI.GreaterThan{Float64}}) where {T, F, G} = [bridge.ge_z_index]
MOI.get(bridge::GeoMeantoExpBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{H, MOI.ExponentialCone}) where {T, F, G, H} = bridge.exp_indices

# References
function MOI.delete(model::MOI.ModelLike, bridge::GeoMeantoExpBridge)
    MOI.delete.(model, bridge.exp_indices)
    MOI.delete(model, bridge.le_y_index)
    MOI.delete(model, bridge.ge_z_index)
    MOI.delete(model, bridge.y)
    MOI.delete.(model, bridge.z)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, bridge::GeoMeantoExpBridge{T, F, G, H, I}) where {T, F, G, H, I}
    u_func = MOIU.remove_variable(MOI.get(model, MOI.ConstraintFunction(), bridge.le_y_index), bridge.y)
    w_func = [MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), exp_index))[3] for exp_index in bridge.exp_indices]
    return MOIU.convert_approx(I, MOIU.operate(vcat, T, u_func, MOIU.vectorize(w_func)))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, bridge::GeoMeantoExpBridge) = MOI.GeometricMeanCone(1 + length(bridge.z))
MOI.supports(::MOI.ModelLike, ::Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart},
    ::Type{<:GeoMeantoExpBridge}) = true
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintPrimal, MOI.ConstraintPrimalStart}, bridge::GeoMeantoExpBridge)
    le_y_primal = MOI.get(model, attr, bridge.le_y_index)
    exp_primals = MOI.get.(model, attr, bridge.exp_indices)
    u_primal = le_y_primal + sum(exp_i_primal[2] for exp_i_primal in exp_primals) / length(exp_primals)
    w_primal = [exp_i_primal[3] for exp_i_primal in exp_primals]
    return vcat(u_primal, w_primal)
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart, bridge::GeoMeantoExpBridge, value)
    u_primal = value[1]
    y_primal = max(zero(u_primal), u_primal)
    MOI.set(model, MOI.VariablePrimalStart(), bridge.y, y_primal)
    if iszero(y_primal)
        z_primal = zeros(u_primal, length(bridge.z))
    else
        z_primal = [y_primal * log(value[i] / y_primal) for i in 2:length(value)]
    end
    MOI.set(model, MOI.VariablePrimalStart(), bridge.z, z_primal)
    MOI.set(model, attr, bridge.le_y_index, u_primal - y_primal)
    MOI.set(model, attr, bridge.ge_z_index, zero(u_primal))
    for i in eachindex(bridge.z)
        MOI.set(model, attr, bridge.exp_indices[i], vcat(z_primal[i], y_primal, value[1 + i]))
    end
    return
end
# (u, w) in GeoMean <=> exists y, z such that Au + Bw + Cy + Dz in (<=, >=, (exp)_i)
# so GeoMean* = [A'; B'] (<=, >=, exp1, ..., expn)*
# and 0 = [C'; D'] (<=, >=, exp1, ..., expn)*
# where
# A = [1, 0, (0, 0, 0)_i]
# B = [0, 0, (0, 0, I)]
# C = [-1, 0, (0, 1, 0)]
# D = [0, 1, (I, 0, 0)]
# so given dual z = (a, b, (c, d, e)_i), get
# -u = A' z = a => a = -u
# w_i = B' z = e_i => e_i = w_i
# 0 = C' z = -a + sum(d) => a = sum(d) = -u
# 0 = D' z = b + c_i => c_i = b
# Given a is dual on u - y <= 0, b is dual on sum(z) >= 0, and (c_i, d_i, e_i) is dual on ExponentialCone constraint i,
# dual on (u, w) in GeometricMeanCone is (-a, e).
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintDual, MOI.ConstraintDualStart}, bridge::GeoMeantoExpBridge)
    u_dual = -MOI.get(model, attr, bridge.le_y_index)
    w_dual = [MOI.get(model, attr, exp_index_i)[3] for exp_index_i in bridge.exp_indices]
    return vcat(u_dual, w_dual)
end
# Given constraint dual start of (u, w), constraint dual on LessThan constraint is a = -u,
# on GreaterThan constraint is b, and on exponential cone constraint i is (c_i = -b, d_i, e_i = w_i).
# We must have sum(d) = a = -u. We let b = c_i = 0, hence the dual on exponential cone constraint i is (0, -u / n, w_i), which is feasible for the DualExponentialCone.
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintDualStart, bridge::GeoMeantoExpBridge, value)
    MOI.set(model, MOI.ConstraintDualStart(), bridge.le_y_index, -value[1])
    MOI.set(model, MOI.ConstraintDualStart(), bridge.ge_z_index, zero(value[1]))
    b_value = -value[1] / length(bridge.z)
    for i in eachindex(bridge.z)
        MOI.set(model, MOI.ConstraintDualStart(), bridge.exp_indices[i], vcat(0, b_value, value[1 + i]))
    end
    return
end
