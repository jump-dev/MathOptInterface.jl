"""
    SplitIntervalBridge{T}

The `SplitIntervalBridge` splits a constraint ``l ≤ ⟨a, x⟩ + α ≤ u`` into the constraints ``⟨a, x⟩ + α ≥ l`` and ``⟨a, x⟩ + α ≤ u``.
"""
struct SplitIntervalBridge{T, F<:MOI.AbstractScalarFunction} <: AbstractBridge
    lower::CI{F, MOI.GreaterThan{T}}
    upper::CI{F, MOI.LessThan{T}}
end
function SplitIntervalBridge{T, F}(model, f::F, s::MOI.Interval{T}) where {T, F}
    lower = MOI.add_constraint(model, f, MOI.GreaterThan(s.lower))
    upper = MOI.add_constraint(model, f, MOI.LessThan(s.upper))
    return SplitIntervalBridge{T, F}(lower, upper)
end

MOI.supports_constraint(::Type{SplitIntervalBridge{T}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = true
function addedconstrainttypes(::Type{SplitIntervalBridge{T, F}}) where {T, F}
    return [(F, MOI.GreaterThan{T}), (F, MOI.LessThan{T})]
end
function concrete_bridge_type(::Type{<:SplitIntervalBridge},
                              F::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.Interval{T}}) where T
    return SplitIntervalBridge{T, F}
end

# Attributes, Bridge acting as an model
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.LessThan{T}}) where {T, F} = 1
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F} = 1
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F} = [b.lower]
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.LessThan{T}}) where {T, F} = [b.upper]

# Indices
function MOI.delete(model::MOI.ModelLike, c::SplitIntervalBridge)
    MOI.delete(model, c.lower)
    MOI.delete(model, c.upper)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    return MOI.get(model, attr, c.lower)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    # Should be nonnegative
    lower_dual = MOI.get(model, MOI.ConstraintDual(), c.lower)
    # Should be nonpositive
    upper_dual = MOI.get(model, MOI.ConstraintDual(), c.upper)
    return lower_dual > -upper_dual ? lower_dual : upper_dual
end

# Constraints
function MOI.modify(model::MOI.ModelLike, c::SplitIntervalBridge, change::MOI.AbstractFunctionModification)
    MOI.modify(model, c.lower, change)
    MOI.modify(model, c.upper, change)
end

MOI.supports(model::MOI.ModelLike, ::MOI.ConstraintFunction, ::Type{<:SplitIntervalBridge}) = true
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                  c::SplitIntervalBridge{T, F}, func::F) where {T, F}
    MOI.set(model, MOI.ConstraintFunction(), c.lower, func)
    MOI.set(model, MOI.ConstraintFunction(), c.upper, func)
end

MOI.supports(model::MOI.ModelLike, ::MOI.ConstraintSet, ::Type{<:SplitIntervalBridge}) = true
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.set(model, MOI.ConstraintSet(), c.lower, MOI.GreaterThan(change.lower))
    MOI.set(model, MOI.ConstraintSet(), c.upper, MOI.LessThan(change.upper))
end
