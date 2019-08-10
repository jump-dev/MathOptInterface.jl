"""
    NormInfinityBridge{T}

The `NormInfinityCone` is representable with LP constraints, since ``t \\ge \\max_i |x_i|`` if and only if ``t \\ge x_i`` and ``t \\ge -x_i`` for all ``i``.
"""
struct NormInfinityBridge{T} <: AbstractBridge
    nn_index::CI{<:Union{MOI.VectorAffineFunction{T}, MOI.VectorQuadraticFunction{T}}, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NormInfinityBridge{T}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormInfinityCone) where T
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    if t isa MOI.SingleVariable # TODO currently needed for operate_output_index!
        t = MOI.ScalarAffineFunction{T}(t)
    end
    d = MOI.dimension(s)
    lb = f_scalars[2:d]
    ub = MOIU.operate(-, T, lb)
    f_new = MOIU.operate(vcat, T, ub, lb)
    for i in 1:MOI.output_dimension(f_new)
        MOIU.operate_output_index!(+, T, i, f_new, t)
    end
    nn_index = MOI.add_constraint(model, f_new, MOI.Nonnegatives(MOI.output_dimension(f_new)))
    return NormInfinityBridge(nn_index)
end

MOI.supports_constraint(::Type{NormInfinityBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormInfinityCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormInfinityBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormInfinityBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormInfinityCone}) where T = [(MOI.VectorAffineFunction{T}, MOI.Nonnegatives),]
MOIB.added_constraint_types(::Type{NormInfinityBridge{T}}, ::Type{<:MOI.VectorQuadraticFunction{T}}, ::Type{MOI.NormInfinityCone}) where T = [(MOI.VectorQuadraticFunction{T}, MOI.Nonnegatives),]

# Attributes, Bridge acting as a model
MOI.get(b::NormInfinityBridge{T}, ::MOI.NumberOfConstraints{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, MOI.Nonnegatives}) where T = 1
MOI.get(b::NormInfinityBridge{T}, ::MOI.ListOfConstraintIndices{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, MOI.Nonnegatives}) where T = [b.nn_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormInfinityBridge)
    MOI.delete(model, c.nn_index)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormInfinityBridge)
    ge_value = MOI.get(model, MOI.ConstraintPrimal(), c.nn_index)
    t = sum(ge_value) / length(ge_value)
    d = div(length(ge_value), 2)
    x = (ge_value[(d + 1):end] - ge_value[1:d]) / 2
    return vcat(t, x)
end
# Given a_i is dual on t - x_i >= 0 and b_i is dual on t + x_i >= 0,
# the dual on (t, x) in NormInfinityCone is (u, v) in NormOneCone, where
# v_i = -a_i + b_i and u = sum(a) + sum(b).
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormInfinityBridge)
    ge_dual = MOI.get(model, MOI.ConstraintDual(), c.nn_index)
    t = sum(ge_dual)
    d = div(length(ge_dual), 2)
    x = (ge_dual[(d + 1):end] - ge_dual[1:d])
    return vcat(t, x)
end

"""
    NormOneBridge{T}

The `NormOneCone` is representable with LP constraints, since ``t \\ge \\sum_i |x_i|`` if and only if there exists a vector y such that ``t \\ge \\sum_i y_i`` and ``y_i \\ge x_i`` and ``y_i \\ge -x_i`` for all ``i``.
"""
struct NormOneBridge{T} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    ge_index::CI{<:Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}, MOI.GreaterThan{T}}
    nn_index::CI{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NormOneBridge{T}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormOneCone) where T
    if f isa MOI.VectorOfVariables # TODO currently needed for test to pass
        f = MOI.VectorAffineFunction{T}(f)
    end
    f_scalars = MOIU.eachscalar(f)
    d = MOI.dimension(s)
    y = MOI.add_variables(model, d - 1)
    ge_index = MOI.add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate(sum, T, y)), MOI.GreaterThan(zero(T)))
    lb = f_scalars[2:d]
    ub = MOIU.operate(-, T, lb)
    MOIU.operate!(+, T, lb, MOI.VectorOfVariables(y))
    MOIU.operate!(+, T, ub, MOI.VectorOfVariables(y))
    f_new = MOIU.operate(vcat, T, ub, lb)
    nn_index = MOI.add_constraint(model, f_new, MOI.Nonnegatives(2d - 2))
    return NormOneBridge(y, ge_index, nn_index)
end

MOI.supports_constraint(::Type{NormOneBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormOneCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormOneBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormOneBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormOneCone}) where T = [(MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}), (MOI.VectorAffineFunction{T}, MOI.Nonnegatives)]
MOIB.added_constraint_types(::Type{NormOneBridge{T}}, ::Type{<:MOI.VectorQuadraticFunction{T}}, ::Type{MOI.NormOneCone}) where T = [(MOI.ScalarQuadraticFunction{T}, MOI.GreaterThan{T}), (MOI.VectorQuadraticFunction{T}, MOI.Nonnegatives)]

# Attributes, Bridge acting as a model
MOI.get(b::NormOneBridge, ::MOI.NumberOfVariables) = length(b.y)
MOI.get(b::NormOneBridge, ::MOI.ListOfVariableIndices) = b.y
MOI.get(b::NormOneBridge{T}, ::MOI.NumberOfConstraints{<:Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}, MOI.GreaterThan{T}}) where T = 1
MOI.get(b::NormOneBridge{T}, ::MOI.NumberOfConstraints{<:Union{MOI.VectorAffineFunction{T}, MOI.VectorQuadraticFunction{T}}, MOI.Nonnegatives}) where T = 1
MOI.get(b::NormOneBridge{T}, ::MOI.ListOfConstraintIndices{<:Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}, MOI.GreaterThan{T}}) where T = [b.ge_index]
MOI.get(b::NormOneBridge{T}, ::MOI.ListOfConstraintIndices{<:Union{MOI.VectorAffineFunction{T}, MOI.VectorQuadraticFunction{T}}, MOI.Nonnegatives}) where T = [b.nn_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormOneBridge)
    MOI.delete(model, c.nn_index)
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.y)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormOneBridge)
    ge_value = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    nn_value = MOI.get(model, MOI.ConstraintPrimal(), c.nn_index)
    t = ge_value + sum(nn_value) / 2
    d = length(c.y)
    x = (nn_value[(d + 1):end] - nn_value[1:d]) / 2
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
