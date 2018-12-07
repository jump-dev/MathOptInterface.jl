"""
    SlackBridge{T}

The `SplackBridge` converts a constraint ``Function in Set`` where `F` is a function different
 from `SingleVariable` and to `VectorOfVariables` into the constraints ``F + F2 in Zeros`` of ``F + F2 in EqualTo{T}``
 and ``F2 in Set\``.
"""
struct ScalarSlackBridge{T, F<:MOI.AbstractScalarFunction, S<:MOI.AbstractScalarSet} <: AbstractBridge
    slack::MOI.VariableIndex
    slack_in_set::CI{MOI.SingleVariable, S}
    equality::CI{F, MOI.EqualTo{T}}
end
function ScalarSlackBridge{T, F, S}(model, f::F, s::S) where {T, F<:MOI.ScalarAffineFunction{T}, S<:MOI.AbstractScalarSet}
    slack = MOI.add_variable(model)
    new_f = copy(f)
    push!(new_f.terms, MOI.ScalarAffineTerm{T}(-one(T), slack))
    slack_in_set = MOI.add_constraint(model, SingleVariable(slack), s)
    equality = MOI.add_constraint(model, new_f, MOI.EqualTo(0.0))
    return ScalarSlackBridge{T, F, S}(slack, equality, slack_in_set)
end

MOI.supports_constraint(::Type{ScalarSlackBridge{T, F, S}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = true
MOI.supports_constraint(::Type{ScalarSlackBridge{T, F, S}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.GreaterThan{T}}) where T = true
MOI.supports_constraint(::Type{ScalarSlackBridge{T, F, S}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.LessThan{T}}) where T = true
function added_constraint_types(::Type{ScalarSlackBridge{T, F, S}}) where {T, F<:MOI.AbstractScalarFunction, S}
    return [(F, MOI.EqualTo{T}), (MOI.SingleVariable, S)]
end
function concrete_bridge_type(::Type{<:ScalarSlackBridge},
                              F::Type{<:MOI.AbstractScalarFunction},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    return ScalarSlackBridge{T, F, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{F, MOI.EqualTo{T}}) where {T, F<:MOI.ScalarAffineFunction{T}, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{MOI.SingleVariable, S}) where {T, F<:MOI.ScalarAffineFunction{T}, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{F, MOI.EqualTo{T}}) where {T, F<:MOI.ScalarAffineFunction{T}, S} = [b.equality]
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}) where {T, F<:MOI.ScalarAffineFunction{T}, S} = [b.slack_in_set]

# Indices
function MOI.delete(model::MOI.ModelLike, c::ScalarSlackBridge)
    MOI.delete(model, c.equality)
    MOI.delete(model, c.slack_in_set)
    MOI.delete(model, c.slack)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::ScalarSlackBridge)
    # ldue to equality, slack should have the same value as original affine function
    return MOI.get(model, attr, c.slack_in_set)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::ScalarSlackBridge)
    error("")
end

# Constraints
function MOI.modify(model::MOI.ModelLike, c::ScalarSlackBridge, change::MOI.AbstractFunctionModification)
    MOI.modify(model, c.equality, change)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                  c::ScalarSlackBridge{T, F, S}, func::F) where {T, F, S}
    new_func = copy(func)
    push!(new_func.terms, MOI.ScalarAffineTerm{T}(-one(T), c.slack))
    MOI.set(model, MOI.ConstraintFunction(), c.equality, new_func)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::ScalarSlackBridge, change::MOI.Interval)
    MOI.set(model, MOI.ConstraintSet(), c.slack_in_set, change)
end
