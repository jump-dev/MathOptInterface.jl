"""
    GeoMeantoRelEntrBridge{T}

The `geometric mean cone` is representable with a relative entropy constraint and a nonnegative auxiliary variable,
since ``u \\le \\prod_{i=1}^n w_i^{1/n}`` if and only if there exists a ``y \\ge 0`` such that
``0 \\ge \\sum_{i=1}^n (u + y) \\log (\\frac{u + y}{w_i})``, or equivalently
``(0, w, (u + y) e) \\in RelativeEntropyCone(1 + 2n)``, where ``e`` is a vector of ones.
"""
struct GeoMeantoRelEntrBridge{T, F, G, H} <: AbstractBridge
    y::MOI.VariableIndex
    ge_index::CI{F, MOI.GreaterThan{T}} # for y >= 0
    relentr_index::CI{G, MOI.RelativeEntropyCone}
end
function bridge_constraint(::Type{GeoMeantoRelEntrBridge{T, F, G, H}}, model::MOI.ModelLike, f::H, s::MOI.GeometricMeanCone) where {T, F, G, H}
    f_scalars = MOIU.eachscalar(f)
    y = MOI.add_variable(model)
    ge_index = MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    relentr_func = MOIU.operate(vcat, T, zero(MOI.ScalarAffineFunction{Float64}), f_scalars[2:end], fill(MOIU.operate(+, T, f_scalars[1], MOI.SingleVariable(y)), MOI.dimension(s) - 1)...)
    relentr_index = MOI.add_constraint(model, relentr_func, MOI.RelativeEntropyCone(MOI.output_dimension(relentr_func)))
    return GeoMeantoRelEntrBridge{T, F, G, H}(y, ge_index, relentr_index)
end

MOI.supports_constraint(::Type{GeoMeantoRelEntrBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.GeometricMeanCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:GeoMeantoRelEntrBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{GeoMeantoRelEntrBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.RelativeEntropyCone)]
function concrete_bridge_type(::Type{<:GeoMeantoRelEntrBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.GeometricMeanCone}) where T
    F = MOI.SingleVariable
    S = MOIU.scalar_type(H)
    G = MOIU.promote_operation(vcat, T, T, S, MOIU.promote_operation(+, T, S, MOI.SingleVariable))
    return GeoMeantoRelEntrBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(bridge::GeoMeantoRelEntrBridge, ::MOI.NumberOfVariables) = 1
MOI.get(bridge::GeoMeantoRelEntrBridge, ::MOI.ListOfVariableIndices) = [bridge.y]
MOI.get(bridge::GeoMeantoRelEntrBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F} = 1
MOI.get(bridge::GeoMeantoRelEntrBridge{T, F, G}, ::MOI.NumberOfConstraints{G, MOI.RelativeEntropyCone}) where {T, F, G} = 1
MOI.get(bridge::GeoMeantoRelEntrBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F} = [bridge.ge_index]
MOI.get(bridge::GeoMeantoRelEntrBridge{T, F, G}, ::MOI.ListOfConstraintIndices{G, MOI.RelativeEntropyCone}) where {T, F, G} = [bridge.relentr_index]

# References
function MOI.delete(model::MOI.ModelLike, bridge::GeoMeantoRelEntrBridge)
    MOI.delete(model, bridge.relentr_index)
    MOI.delete(model, bridge.ge_index)
    MOI.delete(model, bridge.y)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, bridge::GeoMeantoRelEntrBridge{T, F, G, H}) where {T, F, G, H}
    relentr_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), bridge.relentr_index))
    d = div(length(relentr_func) - 1, 2)
    u_func = MOIU.remove_variable(MOIU.operate(-, T, relentr_func[end], MOI.SingleVariable(bridge.y)), bridge.y)
    w_func = relentr_func[2:(1 + d)]
    return MOIU.convert_approx(H, MOIU.operate(vcat, T, u_func, w_func))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, bridge::GeoMeantoRelEntrBridge) = MOI.GeometricMeanCone(1 + div(MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge.relentr_index)) - 1, 2))
MOI.supports(::MOI.ModelLike, ::Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart}, ::Type{<:GeoMeantoRelEntrBridge}) = true
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintPrimal, MOI.ConstraintPrimalStart}, bridge::GeoMeantoRelEntrBridge)
    relentr_primal = MOI.get(model, attr, bridge.relentr_index)
    d = div(length(relentr_primal) - 1, 2)
    y_val = MOI.get(model, attr, bridge.ge_index)
    u_val = sum(relentr_primal[(2 + d):end]) / d - y_val
    return vcat(u_val, relentr_primal[2:(1 + d)])
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart, bridge::GeoMeantoRelEntrBridge, value)
    u_val = value[1]
    u_pos = -min(zero(u_val), u_val)
    d = length(value) - 1
    MOI.set(model, MOI.VariablePrimalStart(), bridge.y, u_pos)
    MOI.set(model, attr, bridge.ge_index, u_pos)
    MOI.set(model, attr, bridge.relentr_index, vcat(0, value[2:end], fill(u_pos + u_val, d)))
    return
end
# Given a is dual on y >= 0 and (b, c, d) is dual on RelativeEntropyCone constraint,
# dual on (u, w) in GeometricMeanCone is (-a, c)
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintDual, MOI.ConstraintDualStart}, bridge::GeoMeantoRelEntrBridge)
    u_dual = -MOI.get(model, attr, bridge.ge_index)[1]
    relentr_dual = MOI.get(model, attr, bridge.relentr_index)
    d = div(length(relentr_dual) - 1, 2)
    w_dual = relentr_dual[2:(d + 1)]
    return vcat(u_dual, w_dual)
end
# Given constraint dual start of (u, w), constraint dual on RelativeEntropyCone
# constraint is (1/d, -u/d * e, -w) and dual on y >= 0 constraint is -u.
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintDualStart, bridge::GeoMeantoRelEntrBridge, value)
    d = length(value) - 1
    u = value[1]
    MOI.set(model, MOI.ConstraintDualStart(), bridge.ge_index, -u)
    relentr_dual = vcat(one(u) / d, fill(-u / d, d), -value[2:end])
    MOI.set(model, MOI.ConstraintDualStart(), bridge.relentr_index, relentr_dual)
    return
end
