# scalar version

"""
    ScalarSlackBridge{T, F, S}

The `ScalarSlackBridge` converts a constraint `G`-in-`S` where `G` is a function different
from `SingleVariable` into the constraints `F`-in-`EqualTo{T}` and `SingleVariable`-in-`S`.
`F` is the result of subtracting a `SingleVariable` from `G`.
Tipically `G` is the same as `F`, but that is not mandatory.
"""
struct ScalarSlackBridge{T, F, S} <: AbstractBridge
    slack::MOI.VariableIndex
    slack_in_set::CI{MOI.SingleVariable, S}
    equality::CI{F, MOI.EqualTo{T}}
end
function bridge_constraint(::Type{ScalarSlackBridge{T, F, S}}, model,
                           f::MOI.AbstractScalarFunction, s::S) where {T, F, S}
    slack = MOI.add_variable(model)
    new_f = MOIU.operate(-, T, f, MOI.SingleVariable(slack))
    slack_in_set = MOI.add_constraint(model, MOI.SingleVariable(slack), s)
    equality = MOI.add_constraint(model, new_f, MOI.EqualTo(0.0))
    return ScalarSlackBridge{T, F, S}(slack, slack_in_set, equality)
end

# start allowing everything (scalar)
MOI.supports_constraint(::Type{ScalarSlackBridge{T}},
                        ::Type{<:MOI.AbstractScalarFunction},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = true
# then restrict (careful with ambiguity)
MOI.supports_constraint(::Type{ScalarSlackBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.EqualTo}) where {T} = false
MOI.supports_constraint(::Type{ScalarSlackBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = false
MOI.supports_constraint(::Type{ScalarSlackBridge{T}},
                        ::Type{<:MOI.AbstractScalarFunction},
                        ::Type{<:MOI.EqualTo}) where {T} = false
function MOIB.added_constraint_types(::Type{ScalarSlackBridge{T, F, S}}) where {T, F, S}
    return [(F, MOI.EqualTo{T}), (MOI.SingleVariable, S)]
end
function concrete_bridge_type(::Type{<:ScalarSlackBridge{T}},
                              F::Type{<:MOI.AbstractScalarFunction},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    F2 = MOIU.promote_operation(-, T, F, MOI.SingleVariable)
    return ScalarSlackBridge{T, F2, S}
end

# Attributes, Bridge acting as a model
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfVariables) where {T, F, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{F, MOI.EqualTo{T}}) where {T, F, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{MOI.SingleVariable, S}) where {T, F, S} = 1
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{F, MOI.EqualTo{T}}) where {T, F, S} = [b.equality]
MOI.get(b::ScalarSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}) where {T, F, S} = [b.slack_in_set]

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
    # The dual constraint on slack (since it is free) is
    # -dual_slack_in_set + dual_equality = 0 so the two duals are
    # equal and we can return either one of them.
    return MOI.get(model, a, c.slack_in_set)
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintBasisStatus,  c::ScalarSlackBridge)
    MOI.get(model, MOI.ConstraintBasisStatus(), c.slack_in_set)
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

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::ScalarSlackBridge{T, F, S}, change::S) where {T, F, S}
    MOI.set(model, MOI.ConstraintSet(), c.slack_in_set, change)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::ScalarSlackBridge{T}) where T
    return MOIU.remove_variable(MOI.get(model, attr, b.equality), b.slack)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::ScalarSlackBridge)
    return MOI.get(model, attr, b.slack_in_set)
end

# vector version

"""
    VectorSlackBridge{T, F, S}

The `VectorSlackBridge` converts a constraint `G`-in-`S` where `G` is a function different
from `VectorOfVariables` into the constraints `F`in-`Zeros` and `VectorOfVariables`-in-`S`.
`F` is the result of subtracting a `VectorOfVariables` from `G`.
Tipically `G` is the same as `F`, but that is not mandatory.
"""
struct VectorSlackBridge{T, F, S} <: AbstractBridge
    slacks::Vector{MOI.VariableIndex}
    slacks_in_set::CI{MOI.VectorOfVariables, S}
    equality::CI{F, MOI.Zeros}
end
function bridge_constraint(::Type{VectorSlackBridge{T, F, S}}, model,
                           f::MOI.AbstractVectorFunction, s::S) where {T, F, S}
    d = MOI.dimension(s)
    slacks = MOI.add_variables(model, d)
    new_f = MOIU.operate(-, T, f, MOI.VectorOfVariables(slacks))
    slacks_in_set = MOI.add_constraint(model, MOI.VectorOfVariables(slacks), s)
    equality = MOI.add_constraint(model, new_f, MOI.Zeros(d))
    return VectorSlackBridge{T, F, S}(slacks, slacks_in_set, equality)
end

MOI.supports_constraint(::Type{VectorSlackBridge{T}},
                        ::Type{<:MOI.AbstractVectorFunction},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = true
MOI.supports_constraint(::Type{VectorSlackBridge{T}},
                        ::Type{<:MOI.VectorOfVariables},
                        ::Type{<:MOI.Zeros}) where {T} = false
MOI.supports_constraint(::Type{VectorSlackBridge{T}},
                        ::Type{<:MOI.AbstractVectorFunction},
                        ::Type{<:MOI.Zeros}) where {T} = false
MOI.supports_constraint(::Type{VectorSlackBridge{T}},
                        ::Type{<:MOI.VectorOfVariables},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = false
function MOIB.added_constraint_types(::Type{VectorSlackBridge{T, F, S}}) where {T, F<:MOI.AbstractVectorFunction, S}
    return [(F, MOI.Zeros), (MOI.VectorOfVariables, S)]
end
function concrete_bridge_type(::Type{<:VectorSlackBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOI.AbstractVectorSet}) where T
    F2 = MOIU.promote_operation(-, T, F, MOI.VectorOfVariables)
    return VectorSlackBridge{T, F2, S}
end

# Attributes, Bridge acting as a model
MOI.get(b::VectorSlackBridge{T, F, S}, ::MOI.NumberOfVariables) where {T, F, S} = length(b.slacks)
MOI.get(b::VectorSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{F, MOI.Zeros}) where {T, F, S} = 1
MOI.get(b::VectorSlackBridge{T, F, S}, ::MOI.NumberOfConstraints{MOI.VectorOfVariables, S}) where {T, F, S} = 1
MOI.get(b::VectorSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{F, MOI.Zeros}) where {T, F, S} = [b.equality]
MOI.get(b::VectorSlackBridge{T, F, S}, ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables, S}) where {T, F, S} = [b.slacks_in_set]

# Indices
function MOI.delete(model::MOI.ModelLike, c::VectorSlackBridge)
    MOI.delete(model, c.equality)
    MOI.delete(model, c.slacks_in_set)
    MOI.delete(model, c.slacks)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::VectorSlackBridge)
    # due to equality, slacks should have the same value as original affine function
    return MOI.get(model, attr, c.slacks_in_set)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::VectorSlackBridge)
    # The dual constraint on slack (since it is free) is
    # -dual_slack_in_set + dual_equality = 0 so the two duals are
    # equal and we can return either one of them.
    return MOI.get(model, a, c.slacks_in_set)
end

# Constraints
function MOI.modify(model::MOI.ModelLike, c::VectorSlackBridge, change::MOI.AbstractFunctionModification)
    MOI.modify(model, c.equality, change)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::VectorSlackBridge{T, F, S}, func::F) where {T, F, S}
    new_func = MOIU.operate(-, T, func, MOI.VectorAffineFunction{T}(MOI.VectorOfVariables(c.slacks)))
    MOI.set(model, MOI.ConstraintFunction(), c.equality, new_func)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::VectorSlackBridge{T,F,S}, change::S)  where {T, F, S}
    MOI.set(model, MOI.ConstraintSet(), c.slacks_in_set, change)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::VectorSlackBridge{T}) where T
    return MOIU.remove_variable(MOI.get(model, attr, b.equality), b.slacks)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::VectorSlackBridge)
    return MOI.get(model, attr, b.slacks_in_set)
end
