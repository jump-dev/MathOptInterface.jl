"""
    SplitIntervalBridge{T}

The `SplitIntervalBridge` splits a constraint ``l ≤ ⟨a, x⟩ + α ≤ u`` into the constraints ``⟨a, x⟩ + α ≥ l`` and ``⟨a, x⟩ + α ≤ u``.
"""
struct SplitIntervalBridge{T, F<:MOI.AbstractScalarFunction} <: AbstractBridge
    lower::CI{F, MOI.GreaterThan{T}}
    upper::CI{F, MOI.LessThan{T}}
end
function bridge_constraint(::Type{SplitIntervalBridge{T, F}}, model, f::F,
                           s::MOI.Interval{T}) where {T, F}
    lower = MOI.add_constraint(model, f, MOI.GreaterThan(s.lower))
    upper = MOI.add_constraint(model, f, MOI.LessThan(s.upper))
    return SplitIntervalBridge{T, F}(lower, upper)
end

MOI.supports_constraint(::Type{SplitIntervalBridge{T}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = true
MOIB.added_constrained_variable_types(::Type{<:SplitIntervalBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{SplitIntervalBridge{T, F}}) where {T, F}
    return [(F, MOI.GreaterThan{T}), (F, MOI.LessThan{T})]
end
function concrete_bridge_type(::Type{<:SplitIntervalBridge},
                              F::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.Interval{T}}) where T
    return SplitIntervalBridge{T, F}
end

# Attributes, Bridge acting as a model
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
function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart},
    ::Type{<:SplitIntervalBridge})

    return true
end
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintPrimal, MOI.ConstraintPrimalStart},
                 bridge::SplitIntervalBridge)
    # lower and upper should give the same value
    return MOI.get(model, attr, bridge.lower)
end
function MOI.set(model::MOI.ModelLike, a::MOI.ConstraintPrimalStart,
                 bridge::SplitIntervalBridge, value)
    MOI.set(model, a, bridge.lower, value)
    MOI.set(model, a, bridge.upper, value)
end
function MOI.get(model::MOI.ModelLike, attr::Union{MOI.ConstraintDual, MOI.ConstraintDualStart},
                 bridge::SplitIntervalBridge)
    # Should be nonnegative
    lower_dual = MOI.get(model, attr, bridge.lower)
    # Should be nonpositive
    upper_dual = MOI.get(model, attr, bridge.upper)
    return lower_dual > -upper_dual ? lower_dual : upper_dual
end
function MOI.set(model::MOI.ModelLike, a::MOI.ConstraintDualStart,
                 bridge::SplitIntervalBridge, value)
    if value < 0
        MOI.set(model, a, bridge.lower, 0.0)
        MOI.set(model, a, bridge.upper, value)
    else
        MOI.set(model, a, bridge.lower, value)
        MOI.set(model, a, bridge.upper, 0.0)
    end
end

function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintBasisStatus, c::SplitIntervalBridge)
    lower_stat = MOI.get(model, MOI.ConstraintBasisStatus(), c.lower)
    upper_stat = MOI.get(model, MOI.ConstraintBasisStatus(), c.upper)
    if lower_stat == MOI.NONBASIC_AT_LOWER
        @warn("GreaterThan constraints should not have basis status:" *
            " NONBASIC_AT_LOWER, instead use NONBASIC.")
    end
    if upper_stat == MOI.NONBASIC_AT_UPPER
        @warn("LessThan constraints should not have basis status:" *
            " NONBASIC_AT_UPPER, instead use NONBASIC.")
    end
    if lower_stat == MOI.NONBASIC
        return MOI.NONBASIC_AT_LOWER
    end
    if upper_stat == MOI.NONBASIC
        return MOI.NONBASIC_AT_UPPER
    end
    if lower_stat != upper_stat
        @warn("Basis status of lower (`$lower_stat`) and upper (`$upper_stat`) constraint are inconsistent," *
            " both should be basic or super basic.")
    end
    return lower_stat
end

# Constraints
function MOI.modify(model::MOI.ModelLike, c::SplitIntervalBridge, change::MOI.AbstractFunctionModification)
    MOI.modify(model, c.lower, change)
    MOI.modify(model, c.upper, change)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                  c::SplitIntervalBridge{T, F}, func::F) where {T, F}
    MOI.set(model, MOI.ConstraintFunction(), c.lower, func)
    MOI.set(model, MOI.ConstraintFunction(), c.upper, func)
end

function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.set(model, MOI.ConstraintSet(), c.lower, MOI.GreaterThan(change.lower))
    MOI.set(model, MOI.ConstraintSet(), c.upper, MOI.LessThan(change.upper))
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::SplitIntervalBridge)
    return MOI.get(model, attr, b.lower)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::SplitIntervalBridge)
    return MOI.Interval(MOI.get(model, attr, b.lower).lower,
                        MOI.get(model, attr, b.upper).upper)
end
