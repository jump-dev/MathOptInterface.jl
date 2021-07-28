_lower_set(set::MOI.Interval) = MOI.GreaterThan(set.lower)
_upper_set(set::MOI.Interval) = MOI.LessThan(set.upper)
_lower_set(set::MOI.EqualTo) = MOI.GreaterThan(set.value)
_upper_set(set::MOI.EqualTo) = MOI.LessThan(set.value)
_lower_set(set::MOI.Zeros) = MOI.Nonnegatives(set.dimension)
_upper_set(set::MOI.Zeros) = MOI.Nonpositives(set.dimension)

"""
    SplitIntervalBridge{T, F, S, LS, US}

The `SplitIntervalBridge` splits a `F`-in-`S` constraint into a `F`-in-`LS` and
a `F`-in-`US` constraint where we have either:
* `S = MOI.Interval{T}`, `LS = MOI.GreaterThan{T}` and `US = MOI.LessThan{T}`,
* `S = MOI.EqualTo{T}`, `LS = MOI.GreaterThan{T}` and `US = MOI.LessThan{T}`, or
* `S = MOI.Zeros`, `LS = MOI.Nonnegatives` and `US = MOI.Nonpositives`.

For instance, if `F` is `MOI.ScalarAffineFunction` and `S` is `MOI.Interval`,
it transforms the constraint ``l ≤ ⟨a, x⟩ + α ≤ u`` into the constraints
``⟨a, x⟩ + α ≥ l`` and ``⟨a, x⟩ + α ≤ u``.
"""
struct SplitIntervalBridge{
    T,
    F<:MOI.AbstractFunction,
    S<:MOI.AbstractSet,
    LS<:MOI.AbstractSet,
    US<:MOI.AbstractSet,
} <: AbstractBridge
    lower::CI{F,LS}
    upper::CI{F,US}
end

function bridge_constraint(
    ::Type{SplitIntervalBridge{T,F,S,LS,US}},
    model::MOI.ModelLike,
    f::F,
    set::S,
) where {T,F,S,LS,US}
    lower = MOI.add_constraint(model, f, _lower_set(set))
    upper = MOI.add_constraint(model, f, _upper_set(set))
    return SplitIntervalBridge{T,F,S,LS,US}(lower, upper)
end

function MOI.supports_constraint(
    ::Type{SplitIntervalBridge{T}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:Union{MOI.Interval{T},MOI.EqualTo{T}}},
) where {T}
    return true
end

function MOI.supports_constraint(
    ::Type{SplitIntervalBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Zeros},
) where {T}
    return MOIU.is_coefficient_type(F, T)
end

function MOIB.added_constrained_variable_types(::Type{<:SplitIntervalBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{SplitIntervalBridge{T,F,S,LS,US}},
) where {T,F,S,LS,US}
    return [(F, LS), (F, US)]
end

function concrete_bridge_type(
    ::Type{<:SplitIntervalBridge},
    F::Type{<:MOI.AbstractScalarFunction},
    S::Type{<:Union{MOI.Interval{T},MOI.EqualTo{T}}},
) where {T}
    return SplitIntervalBridge{T,F,S,MOI.GreaterThan{T},MOI.LessThan{T}}
end

function concrete_bridge_type(
    ::Type{<:SplitIntervalBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Zeros},
) where {T}
    return SplitIntervalBridge{T,F,MOI.Zeros,MOI.Nonnegatives,MOI.Nonpositives}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::SplitIntervalBridge{T,F,S,LS},
    ::MOI.NumberOfConstraints{F,LS},
) where {T,F,S,LS}
    return 1
end

function MOI.get(
    ::SplitIntervalBridge{T,F,S,LS,US},
    ::MOI.NumberOfConstraints{F,US},
) where {T,F,S,LS,US}
    return 1
end

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS},
    ::MOI.ListOfConstraintIndices{F,LS},
) where {T,F,S,LS}
    return [bridge.lower]
end

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS,US},
    ::MOI.ListOfConstraintIndices{F,US},
) where {T,F,S,LS,US}
    return [bridge.upper]
end

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::SplitIntervalBridge)
    MOI.delete(model, bridge.lower)
    MOI.delete(model, bridge.upper)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitIntervalBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SplitIntervalBridge,
)
    # lower and upper should give the same value
    return MOI.get(model, attr, bridge.lower)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitIntervalBridge,
    value,
)
    MOI.set(model, attr, bridge.lower, value)
    MOI.set(model, attr, bridge.upper, value)
    return
end
# The map is:
# x ∈ S <=> [1 1]' * x ∈ LS × US
# So the adjoint map is
# [1 1] * y ∈ S* <=> y ∈ (LS × US)*
# where [1 1] * y = y[1] + y[2]
# so we can just sum the dual values.
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SplitIntervalBridge,
)
    return MOI.get(model, attr, bridge.lower) +
           MOI.get(model, attr, bridge.upper)
end

function _split_dual_start(value)
    if value < 0
        return zero(value), value
    else
        return value, zero(value)
    end
end

function _split_dual_start(value::Vector)
    lower = similar(value)
    upper = similar(value)
    for i in eachindex(value)
        lower[i], upper[i] = _split_dual_start(value[i])
    end
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SplitIntervalBridge{T},
    value,
) where {T}
    lower, upper = _split_dual_start(value)
    MOI.set(model, attr, bridge.lower, lower)
    MOI.set(model, attr, bridge.upper, upper)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintBasisStatus,
    bridge::SplitIntervalBridge,
)
    lower_stat = MOI.get(model, MOI.ConstraintBasisStatus(), bridge.lower)
    upper_stat = MOI.get(model, MOI.ConstraintBasisStatus(), bridge.upper)
    if lower_stat == MOI.NONBASIC_AT_LOWER
        @warn(
            "GreaterThan constraints should not have basis status:" *
            " NONBASIC_AT_LOWER, instead use NONBASIC."
        )
    end
    if upper_stat == MOI.NONBASIC_AT_UPPER
        @warn(
            "LessThan constraints should not have basis status:" *
            " NONBASIC_AT_UPPER, instead use NONBASIC."
        )
    end
    if lower_stat == MOI.NONBASIC
        return MOI.NONBASIC_AT_LOWER
    end
    if upper_stat == MOI.NONBASIC
        return MOI.NONBASIC_AT_UPPER
    end
    if lower_stat != upper_stat
        @warn(
            "Basis status of lower (`$lower_stat`) and upper (`$upper_stat`) constraint are inconsistent," *
            " both should be basic or super basic."
        )
    end
    return lower_stat
end

# Constraints
function MOI.modify(
    model::MOI.ModelLike,
    bridge::SplitIntervalBridge,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model, bridge.lower, change)
    MOI.modify(model, bridge.upper, change)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::SplitIntervalBridge{T,F},
    func::F,
) where {T,F}
    MOI.set(model, MOI.ConstraintFunction(), bridge.lower, func)
    MOI.set(model, MOI.ConstraintFunction(), bridge.upper, func)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,S},
    change::S,
) where {T,F,S}
    MOI.set(model, MOI.ConstraintSet(), bridge.lower, _lower_set(change))
    MOI.set(model, MOI.ConstraintSet(), bridge.upper, _upper_set(change))
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SplitIntervalBridge,
)
    return MOI.get(model, attr, bridge.lower)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,MOI.Interval{T}},
) where {T,F}
    return MOI.Interval(
        MOI.get(model, attr, bridge.lower).lower,
        MOI.get(model, attr, bridge.upper).upper,
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,MOI.EqualTo{T}},
) where {T,F}
    return MOI.EqualTo(MOI.get(model, attr, bridge.lower).lower)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,MOI.Zeros},
) where {T,F}
    return MOI.Zeros(MOI.get(model, attr, bridge.lower).dimension)
end
