"""
    SplitIntervalBridge{T}

The `SplitIntervalBridge` splits a constraint ``l ≤ ⟨a, x⟩ + α ≤ u`` into the constraints ``⟨a, x⟩ + α ≥ l`` and ``⟨a, x⟩ + α ≤ u``.
"""
struct SplitIntervalBridge{T} <: AbstractBridge
    lower::CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}
    upper::CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
end
function SplitIntervalBridge{T}(model, f::MOI.ScalarAffineFunction{T}, s::MOI.Interval{T}) where T
    lower = MOI.addconstraint!(model, f, MOI.GreaterThan(s.lower))
    upper = MOI.addconstraint!(model, f, MOI.LessThan(s.upper))
    SplitIntervalBridge(lower, upper)
end

MOI.supportsconstraint(::Type{SplitIntervalBridge{T}}, ::Type{MOI.ScalarAffineFunction{T}}, ::Type{MOI.Interval{T}}) where T = true
addedconstrainttypes(::Type{SplitIntervalBridge{T}}, ::Type{MOI.ScalarAffineFunction{T}}, ::Type{MOI.Interval{T}}) where T = [(MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}), (MOI.ScalarAffineFunction{T}, MOI.LessThan{T})]

# Attributes, Bridge acting as an model
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T} = [b.lower]
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T} = [b.upper]

# Indices
function MOI.delete!(model::MOI.ModelLike, c::SplitIntervalBridge)
    MOI.delete!(model, c.lower)
    MOI.delete!(model, c.upper)
end

# Attributes, Bridge acting as a constraint
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, ::Type{SplitIntervalBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}})
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    MOI.get(model, MOI.ConstraintPrimal(), c.lower)
end
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, ::Type{SplitIntervalBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) &&
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}})
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    lowd = MOI.get(model, MOI.ConstraintDual(), c.lower) # Should be nonnegative
    uppd = MOI.get(model, MOI.ConstraintDual(), c.upper) # Should be nonpositive
    if lowd > -uppd
        lowd
    else
        uppd
    end
end

# Constraints
MOI.canmodify(model::MOI.ModelLike, c::SplitIntervalBridge, change) = true
function MOI.modify!(model::MOI.ModelLike, c::SplitIntervalBridge, change::MOI.AbstractFunctionModification)
    MOI.modify!(model, c.lower, change)
    MOI.modify!(model, c.upper, change)
end

MOI.canset(model::MOI.ModelLike, ::MOI.ConstraintFunction, ::Type{<:SplitIntervalBridge}) = true
function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::SplitIntervalBridge, func::MOI.ScalarAffineFunction)
    MOI.set!(model, MOI.ConstraintFunction(), c.lower, func)
    MOI.set!(model, MOI.ConstraintFunction(), c.upper, func)
end

MOI.canset(model::MOI.ModelLike, ::MOI.ConstraintSet, ::Type{<:SplitIntervalBridge}) = true
function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintSet, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.set!(model, MOI.ConstraintSet(), c.lower, MOI.GreaterThan(change.lower))
    MOI.set!(model, MOI.ConstraintSet(), c.upper, MOI.LessThan(change.upper))
end
