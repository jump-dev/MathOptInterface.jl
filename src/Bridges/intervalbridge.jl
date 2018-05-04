"""
    SplitIntervalBridge{T}

The `SplitIntervalBridge` splits a constraint ``l ≤ ⟨a, x⟩ + α ≤ u`` into the constraints ``⟨a, x⟩ + α ≥ l`` and ``⟨a, x⟩ + α ≤ u``.
"""
struct SplitIntervalBridge{T} <: AbstractBridge
    lower::CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}
    upper::CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
end
function SplitIntervalBridge{T}(optimizer, f::MOI.ScalarAffineFunction{T}, s::MOI.Interval{T}) where T
    lower = MOI.addconstraint!(optimizer, f, MOI.GreaterThan(s.lower))
    upper = MOI.addconstraint!(optimizer, f, MOI.LessThan(s.upper))
    SplitIntervalBridge(lower, upper)
end
# Attributes, Bridge acting as an optimizer
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T} = [b.lower]
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T} = [b.upper]

# Indices
function MOI.delete!(optimizer::MOI.AbstractOptimizer, c::SplitIntervalBridge)
    MOI.delete!(optimizer, c.lower)
    MOI.delete!(optimizer, c.upper)
end

# Attributes, Bridge acting as a constraint
function MOI.canget(optimizer::MOI.AbstractOptimizer, a::MOI.ConstraintPrimal, ::Type{SplitIntervalBridge{T}}) where T
    MOI.canget(optimizer, a, CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}})
end
function MOI.get(optimizer::MOI.AbstractOptimizer, a::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    MOI.get(optimizer, MOI.ConstraintPrimal(), c.lower)
end
function MOI.canget(optimizer::MOI.AbstractOptimizer, a::MOI.ConstraintDual, ::Type{SplitIntervalBridge{T}}) where T
    MOI.canget(optimizer, a, CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) &&
    MOI.canget(optimizer, a, CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}})
end
function MOI.get(optimizer::MOI.AbstractOptimizer, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    lowd = MOI.get(optimizer, MOI.ConstraintDual(), c.lower) # Should be nonnegative
    uppd = MOI.get(optimizer, MOI.ConstraintDual(), c.upper) # Should be nonpositive
    if lowd > -uppd
        lowd
    else
        uppd
    end
end

# Constraints
MOI.canmodifyconstraint(optimizer::MOI.AbstractOptimizer, c::SplitIntervalBridge, change) = true
function MOI.modifyconstraint!(optimizer::MOI.AbstractOptimizer, c::SplitIntervalBridge, change::Union{MOI.ScalarAffineFunction, MOI.AbstractFunctionModification})
    MOI.modifyconstraint!(optimizer, c.lower, change)
    MOI.modifyconstraint!(optimizer, c.upper, change)
end
function MOI.modifyconstraint!(optimizer::MOI.AbstractOptimizer, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.modifyconstraint!(optimizer, c.lower, MOI.GreaterThan(change.lower))
    MOI.modifyconstraint!(optimizer, c.upper, MOI.LessThan(change.upper))
end
