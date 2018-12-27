
const ScalarSlackSet{T} = Union{MOI.Interval{T}, MOI.GreaterThan{T}, MOI.LessThan{T}}
const NonSingleVariable{T} = Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}

"""
    ScalarSlackBridge{T, F, S}

The `ScalarSlackBridge` converts a constraint `F`-in-`S` where `F` is a function different
 from `SingleVariable` into the constraints ``F in EqualTo{T}`` and `SingleVariable`-in-`S`.
"""
struct ScalarSlackBridge{T, F<:NonSingleVariable{T}, S<:ScalarSlackSet{T}} <: AbstractBridge
    slack::MOI.VariableIndex
    slack_in_set::CI{MOI.SingleVariable, S}
    equality::CI{F, MOI.EqualTo{T}}
end
function ScalarSlackBridge{T, F, S}(model, f::F, s::S) where {T, F<:NonSingleVariable{T}, S<:ScalarSlackSet{T}}
    slack = MOI.add_variable(model)
    new_f = MOIU.operate(-, T, f, MOI.SingleVariable(slack))
    slack_in_set = MOI.add_constraint(model, MOI.SingleVariable(slack), s)
    equality = MOI.add_constraint(model, new_f, MOI.EqualTo(0.0))
    return ScalarSlackBridge{T, F, S}(slack, slack_in_set, equality)
end

MOI.supports_constraint(::Type{ScalarSlackBridge{T, F, S}}, ::Type{<:NonSingleVariable{T}}, ::Type{<:ScalarSlackSet{T}}) where {T, F, S} = true
function added_constraint_types(::Type{ScalarSlackBridge{T, F, S}}) where {T, F<:NonSingleVariable{T}, S}
    return [(F, MOI.EqualTo{T}), (MOI.SingleVariable, S)]
end
function concrete_bridge_type(::Type{<:ScalarSlackBridge},
                              F::Type{<:NonSingleVariable{T}},
                              S::Type{<:ScalarSlackSet{T}}) where T
    return ScalarSlackBridge{T, F, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{F, MOI.EqualTo{T}}) where {T, F<:NonSingleVariable{T}, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{MOI.SingleVariable, S}) where {T, F<:NonSingleVariable{T}, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{F, MOI.EqualTo{T}}) where {T, F<:NonSingleVariable{T}, S} = [b.equality]
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}) where {T, F<:NonSingleVariable{T}, S} = [b.slack_in_set]

# Indices
function MOI.delete(model::MOI.ModelLike, c::ScalarSlackBridge)
    MOI.delete(model, c.equality)
    MOI.delete(model, c.slack_in_set)
    MOI.delete(model, c.slack)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::ScalarSlackBridge)
    # due to equality, slack should have the same value as original affine function
    return MOI.get(model, attr, c.slack_in_set)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::ScalarSlackBridge)
    error("ScalarSlackBridge is not returning duals for now")
end

# Constraints
function MOI.modify(model::MOI.ModelLike, c::ScalarSlackBridge, change::MOI.AbstractFunctionModification)
    MOI.modify(model, c.equality, change)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                  c::ScalarSlackBridge{T, F, S}, func::F) where {T, F, S}
    new_func = MOIU.operate(-, T, func, MOI.SingleVariable(c.slack))
    MOI.set(model, MOI.ConstraintFunction(), c.equality, new_func)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::ScalarSlackBridge, change::MOI.Interval)
    MOI.set(model, MOI.ConstraintSet(), c.slack_in_set, change)
end
