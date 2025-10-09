# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitIntervalBridge{T,F,S,LS,US} <: Bridges.Constraint.AbstractBridge

`SplitIntervalBridge` implements the following reformulations:

  * ``l \\le f(x) \\le u`` into ``f(x) \\ge l`` and ``f(x) \\le u``
  * ``f(x) = b`` into ``f(x) \\ge b`` and ``f(x) \\le b``
  * ``f(x) \\in \\{0\\}`` into ``f(x) \\in \\mathbb{R}_+`` and
    ``f(x) \\in \\mathbb{R}_-``

## Source node

`SplitIntervalBridge` supports:

  * `F` in [`MOI.Interval{T}`](@ref)
  * `F` in [`MOI.EqualTo{T}`](@ref)
  * `F` in [`MOI.Zeros`](@ref)

## Target nodes

`SplitIntervalBridge` creates:

  * `F` in [`MOI.LessThan{T}`](@ref)
  * `F` in [`MOI.GreaterThan{T}`](@ref)

or

  * `F` in [`MOI.Nonnegatives`](@ref)
  * `F` in [`MOI.Nonpositives`](@ref)


!!! note
    If `T<:AbstractFloat` and `S` is `MOI.Interval{T}` then no lower (resp.
    upper) bound constraint is created if the lower (resp. upper) bound is
    `typemin(T)` (resp. `typemax(T)`). Similarly, when
    [`MOI.ConstraintSet`](@ref) is set, a lower or upper bound
    constraint may be deleted or created accordingly.
"""
mutable struct SplitIntervalBridge{
    T,
    F<:MOI.AbstractFunction,
    S<:MOI.AbstractSet,
    LS<:MOI.AbstractSet,
    US<:MOI.AbstractSet,
} <: AbstractBridge
    lower::Union{Nothing,MOI.ConstraintIndex{F,LS}}
    upper::Union{Nothing,MOI.ConstraintIndex{F,US}}
    # To allow the user to do
    # ```jl
    # x = MOI.add_variable(model)
    # c = MOI.add_constraint(model, x, MOI.Interval(-Inf, Inf))
    # MOI.set(model, MOI.ConstraintSet(), c, MOI.Interval(0.0, Inf))
    # ```
    # we need to store the function to create the lower bound constraint.
    func::Union{Nothing,F}
end

const SplitInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitIntervalBridge{T},OT}

_lower_set(set::MOI.Interval) = MOI.GreaterThan(set.lower)
_upper_set(set::MOI.Interval) = MOI.LessThan(set.upper)

function _lower_set(
    set::MOI.Interval{T},
) where {T<:Union{AbstractFloat,Rational}}
    if set.lower == typemin(T)
        return nothing
    end
    return MOI.GreaterThan(set.lower)
end

function _upper_set(
    set::MOI.Interval{T},
) where {T<:Union{AbstractFloat,Rational}}
    if set.upper == typemax(T)
        return nothing
    end
    return MOI.LessThan(set.upper)
end

_lower_set(set::MOI.EqualTo) = MOI.GreaterThan(set.value)
_upper_set(set::MOI.EqualTo) = MOI.LessThan(set.value)

_lower_set(set::MOI.Zeros) = MOI.Nonnegatives(set.dimension)
_upper_set(set::MOI.Zeros) = MOI.Nonpositives(set.dimension)

function bridge_constraint(
    ::Type{SplitIntervalBridge{T,F,S,LS,US}},
    model::MOI.ModelLike,
    f::F,
    set::S,
) where {T,F,S,LS,US}
    lower, upper, func = nothing, nothing, nothing
    lower_set, upper_set = _lower_set(set), _upper_set(set)
    if lower_set !== nothing
        lower = MOI.add_constraint(model, f, lower_set)
    end
    if upper_set !== nothing
        upper = MOI.add_constraint(model, f, upper_set)
    end
    if lower === nothing && upper === nothing
        func = f
    end
    return SplitIntervalBridge{T,F,S,LS,US}(lower, upper, func)
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
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitIntervalBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{SplitIntervalBridge{T,F,S,LS,US}},
) where {T,F,S,LS,US}
    return Tuple{Type,Type}[(F, LS), (F, US)]
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

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS},
    ::MOI.NumberOfConstraints{F,LS},
)::Int64 where {T,F,S,LS}
    if bridge.lower === nothing
        return 0
    else
        return 1
    end
end

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS,US},
    ::MOI.NumberOfConstraints{F,US},
)::Int64 where {T,F,S,LS,US}
    if bridge.upper === nothing
        return 0
    else
        return 1
    end
end

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS},
    ::MOI.ListOfConstraintIndices{F,LS},
) where {T,F,S,LS}
    if bridge.lower === nothing
        return MOI.ConstraintIndex{F,LS}[]
    else
        return [bridge.lower]
    end
end

function MOI.get(
    bridge::SplitIntervalBridge{T,F,S,LS,US},
    ::MOI.ListOfConstraintIndices{F,US},
) where {T,F,S,LS,US}
    if bridge.upper === nothing
        return MOI.ConstraintIndex{F,US}[]
    else
        return [bridge.upper]
    end
end

function MOI.delete(model::MOI.ModelLike, bridge::SplitIntervalBridge)
    if bridge.lower !== nothing
        MOI.delete(model, bridge.lower)
    end
    if bridge.upper !== nothing
        MOI.delete(model, bridge.upper)
    end
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{SplitIntervalBridge{T,F,S,LS,US}},
) where {T,F,S,LS,US}
    ci_1 = MOI.ConstraintIndex{F,LS}
    ci_2 = MOI.ConstraintIndex{F,US}
    return MOI.supports(model, attr, ci_1) && MOI.supports(model, attr, ci_2)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SplitIntervalBridge,
)
    if bridge.lower !== nothing
        return MOI.get(model, attr, bridge.lower)
    end
    if bridge.upper !== nothing
        return MOI.get(model, attr, bridge.upper)
    end
    msg = "Cannot get `$attr` for a constraint in the interval `[-Inf, Inf]`."
    return throw(MOI.GetAttributeNotAllowed(attr, msg))
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitIntervalBridge,
    value,
)
    if bridge.lower !== nothing
        MOI.set(model, attr, bridge.lower, value)
    end
    if bridge.upper !== nothing
        MOI.set(model, attr, bridge.upper, value)
    end
    return
end

# The map is:
#   x ∈ S <=> [1 1]' * x ∈ LS × US
# So the adjoint map is
#   [1 1] * y ∈ S* <=> y ∈ (LS × US)*
# where
#   [1 1] * y = y[1] + y[2]
# so we can just sum the dual values.
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SplitIntervalBridge{T},
) where {T}
    if bridge.lower === nothing && bridge.upper === nothing
        return zero(T)
    elseif bridge.lower === nothing
        return MOI.get(model, attr, bridge.upper)
    elseif bridge.upper === nothing
        return MOI.get(model, attr, bridge.lower)
    else
        lower = MOI.get(model, attr, bridge.lower)
        if lower === nothing
            return nothing
        end
        return lower + MOI.get(model, attr, bridge.upper)
    end
end

_split_dual_start(::Nothing) = nothing, nothing

function _split_dual_start(value)
    if value < 0
        return zero(value), value
    else
        return value, zero(value)
    end
end

function _split_dual_start(value::Vector{T}) where {T}
    lower, upper = similar(value), similar(value)
    for i in eachindex(value)
        lower[i], upper[i] = _split_dual_start(value[i])
    end
    return lower, upper
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SplitIntervalBridge{T},
    value,
) where {T}
    if bridge.lower === nothing
        if bridge.upper !== nothing
            MOI.set(model, attr, bridge.upper, value)
        end
    else
        if bridge.upper === nothing
            MOI.set(model, attr, bridge.lower, value)
        else
            lower, upper = _split_dual_start(value)
            MOI.set(model, attr, bridge.lower, lower)
            MOI.set(model, attr, bridge.upper, upper)
        end
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintBasisStatus,
    bridge::SplitIntervalBridge,
)
    if bridge.upper !== nothing
        upper_stat = MOI.get(model, attr, bridge.upper)
        if upper_stat == MOI.NONBASIC
            return MOI.NONBASIC_AT_UPPER
        end
    end
    if bridge.lower === nothing
        if bridge.upper === nothing
            # The only case where the interval `[-∞, ∞]` is allowed is for
            # `VariableIndex` constraints but `ConstraintBasisStatus` is not
            # defined for `VariableIndex` constraints.
            msg = "Cannot get `$attr` for a constraint in the interval `[-Inf, Inf]`."
            throw(MOI.GetAttributeNotAllowed(attr, msg))
        end
        return upper_stat
    end
    lower_stat = MOI.get(model, attr, bridge.lower)
    if lower_stat == MOI.NONBASIC
        return MOI.NONBASIC_AT_LOWER
    end
    # Both statuses must be BASIC or SUPER_BASIC, so just return the lower.
    return lower_stat
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::SplitIntervalBridge,
    change::MOI.AbstractFunctionModification,
)
    if bridge.lower !== nothing
        MOI.modify(model, bridge.lower, change)
    end
    if bridge.upper !== nothing
        MOI.modify(model, bridge.upper, change)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::SplitIntervalBridge{T,F},
    func::F,
) where {T,F}
    if bridge.lower !== nothing
        MOI.set(model, MOI.ConstraintFunction(), bridge.lower, func)
    end
    if bridge.upper !== nothing
        MOI.set(model, MOI.ConstraintFunction(), bridge.upper, func)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,S},
    change::S,
) where {T,F,S}
    lower_set, upper_set = _lower_set(change), _upper_set(change)
    if lower_set === nothing && upper_set === nothing
        # The constraints are going to be deleted, we store the function before
        # it is lost.
        bridge.func = MOI.get(model, MOI.ConstraintFunction(), bridge)
    end
    if bridge.lower === nothing
        if lower_set !== nothing
            func = MOI.get(model, MOI.ConstraintFunction(), bridge)
            bridge.lower = MOI.add_constraint(model, func, lower_set)
        end
    else
        if lower_set === nothing
            MOI.delete(model, bridge.lower)
            bridge.lower = nothing
        else
            MOI.set(model, MOI.ConstraintSet(), bridge.lower, lower_set)
        end
    end
    if bridge.upper === nothing
        if upper_set !== nothing
            func = MOI.get(model, MOI.ConstraintFunction(), bridge)
            bridge.upper = MOI.add_constraint(model, func, upper_set)
        end
    else
        if upper_set === nothing
            MOI.delete(model, bridge.upper)
            bridge.upper = nothing
        else
            MOI.set(model, MOI.ConstraintSet(), bridge.upper, upper_set)
        end
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SplitIntervalBridge,
)
    if bridge.lower !== nothing
        return MOI.get(model, attr, bridge.lower)
    end
    if bridge.upper !== nothing
        return MOI.get(model, attr, bridge.upper)
    end
    return copy(bridge.func)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SplitIntervalBridge{T,F,MOI.Interval{T}},
) where {T,F}
    lower, upper = typemin(T), typemax(T)
    if bridge.lower !== nothing
        lower = MOI.get(model, attr, bridge.lower).lower
    end
    if bridge.upper !== nothing
        upper = MOI.get(model, attr, bridge.upper).upper
    end
    return MOI.Interval(lower, upper)
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
