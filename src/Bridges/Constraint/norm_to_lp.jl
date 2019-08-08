"""
    NormInfinityBridge{T}

The `NormInfinityCone` is representable with LP constraints, since ``t \\ge \\max_i |x_i|`` if and only if ``t \\ge x_i`` and ``t \\ge -x_i`` for all ``i``.
"""
struct NormInfinityBridge{T} <: AbstractBridge
    ge_index::CI{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NormInfinityBridge{T}}, model, f::MOI.VectorOfVariables, s::MOI.NormInfinityCone) where T
    return bridge_constraint(NormInfinityBridge{T}, model, MOI.VectorAffineFunction{T}(f), s)
end
function bridge_constraint(::Type{NormInfinityBridge{T}}, model, f::MOI.VectorAffineFunction{T}, s::MOI.NormInfinityCone) where T
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    d = MOI.dimension(s)
    new_f = MOIU.vectorize(vcat([MOIU.operate(-, T, t, f_scalars[i]) for i in 2:d], [MOIU.operate(+, T, t, f_scalars[i]) for i in 2:d]))
    ge_index = MOI.add_constraint(model, new_f, MOI.Nonnegatives(2d - 2))
    return NormInfinityBridge(ge_index)
end

MOI.supports_constraint(::Type{NormInfinityBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormInfinityCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormInfinityBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormInfinityBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormInfinityCone}) where T = [(MOI.VectorAffineFunction{T}, MOI.Nonnegatives),]

# Attributes, Bridge acting as a model
MOI.get(b::NormInfinityBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}) where T = 1
MOI.get(b::NormInfinityBridge{T}, ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}) where T = [b.ge_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormInfinityBridge)
    MOI.delete(model, c.ge_index)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormInfinityBridge)
    ge_value = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    t = sum(ge_value) / length(ge_value)
    d = div(length(ge_value), 2)
    x = (ge_value[(d + 1):end] - ge_value[1:d]) / 2
    return vcat(t, x)
end

"""
    NormOneBridge{T}

The `NormOneCone` is representable with LP constraints, since ``t \\ge \\sum_i |x_i|`` if and only if there exists a vector y such that ``t = \\sum_i y_i`` and ``y_i \\ge x_i`` and ``y_i \\ge -x_i`` for all ``i``.
"""
struct NormOneBridge{T} <: AbstractBridge
    y::Vector{MOI.VariableIndex}
    eq_index::CI{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}
    ge_index::CI{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NormOneBridge{T}}, model, f::MOI.VectorOfVariables, s::MOI.NormOneCone) where T
    return bridge_constraint(NormOneBridge{T}, model, MOI.VectorAffineFunction{T}(f), s)
end
function bridge_constraint(::Type{NormOneBridge{T}}, model, f::MOI.VectorAffineFunction{T}, s::MOI.NormOneCone) where T
    f_scalars = MOIU.eachscalar(f)
    y = MOI.add_variables(model, MOI.dimension(s) - 1)
    eq_index = MOI.add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate(sum, T, y)), MOI.EqualTo(zero(T)))
    new_f = MOIU.vectorize(vcat([MOIU.operate(-, T, MOI.SingleVariable(y[i]), f_scalars[1 + i]) for i in eachindex(y)], [MOIU.operate(+, T, MOI.SingleVariable(y[i]), f_scalars[1 + i]) for i in eachindex(y)]))
    ge_index = MOI.add_constraint(model, new_f, MOI.Nonnegatives(2 * MOI.dimension(s) - 2))
    return NormOneBridge(y, eq_index, ge_index)
end

MOI.supports_constraint(::Type{NormOneBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormOneCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormOneBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormOneBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.NormOneCone}) where T = [(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}), (MOI.VectorAffineFunction{T}, MOI.Nonnegatives)]

# Attributes, Bridge acting as a model
MOI.get(b::NormOneBridge, ::MOI.NumberOfVariables) = length(b.y)
MOI.get(b::NormOneBridge, ::MOI.ListOfVariableIndices) = b.y
MOI.get(b::NormOneBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}) where T = 1
MOI.get(b::NormOneBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}) where T = 1
MOI.get(b::NormOneBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}) where T = [b.eq_index]
MOI.get(b::NormOneBridge{T}, ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, MOI.Nonnegatives}) where T = [b.ge_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormOneBridge)
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.eq_index)
    MOI.delete(model, c.y)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormOneBridge)
    eq_value = MOI.get(model, MOI.ConstraintPrimal(), c.eq_index)
    ge_value = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    t = eq_value + sum(ge_value) / 2
    d = length(c.y)
    x = (ge_value[(d + 1):end] - ge_value[1:d]) / 2
    return vcat(t, x)
end
