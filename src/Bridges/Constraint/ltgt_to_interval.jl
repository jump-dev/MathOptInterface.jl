# The code here is mostly copied from the flip_sign.jl code for FlipSignBridge and GreaterToLessBridge

"""
    AbstractToIntervalBridge{T, S1, F}

Bridge a `F`-in-`Interval` constraint into an `F`-in-`Interval{T}` constraint where we have either:
* `S1 = MOI.GreaterThan{T}`
* `S1 = MOI.LessThan{T}`

The `F`-in-`Interval{T}` constraint is stored in the `constraint`
field by convention.
It is required that T be a AbstractFloat type because otherwise
typemin and typemax would either be not implemented (e.g. BigInt)
or would not give infinite value (e.g. Int).
"""
abstract type AbstractToIntervalBridge{
    T<:AbstractFloat, S1<:MOI.AbstractScalarSet,
    F<:MOI.AbstractScalarFunction} <: SetMapBridge{T, MOI.Interval{T}, S1, F, F} end

# The function map is the identity. It is also an involution, symmetric, and a symmetric involution.
map_function(::Type{<:AbstractToIntervalBridge{T}}, func) where {T} = func
inverse_map_function(::Type{<:AbstractToIntervalBridge}, func) = func
adjoint_map_function(::Type{<:AbstractToIntervalBridge}, func) = func
inverse_adjoint_map_function(::Type{<:AbstractToIntervalBridge}, func) = func

# FIXME are these modify functions necessary?
function MOI.modify(model::MOI.ModelLike, bridge::AbstractToIntervalBridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(model, bridge.constraint, change)
end
function MOI.modify(model::MOI.ModelLike, bridge::AbstractToIntervalBridge,
                    change::MOI.MultirowChange{T}) where T
    MOI.modify(model, bridge.constraint, change)
end

"""
    GreaterToIntervalBridge{T, F<:MOI.AbstractScalarFunction} <:
        AbstractToIntervalBridge{T, MOI.GreaterThan{T}, F}

Transforms a `F`-in-`GreaterThan{T}` constraint into an `F`-in-`Interval{T}`
constraint.
"""
struct GreaterToIntervalBridge{T, F<:MOI.AbstractScalarFunction} <:
    AbstractToIntervalBridge{T, MOI.GreaterThan{T}, F}
    constraint::CI{F, MOI.Interval{T}}
end
map_set(::Type{<:GreaterToIntervalBridge}, set::MOI.GreaterThan) = MOI.Interval(set.lower, typemax(set.lower))
inverse_map_set(::Type{<:GreaterToIntervalBridge}, set::MOI.Interval) = MOI.GreaterThan(set.lower)
function concrete_bridge_type(::Type{<:GreaterToIntervalBridge{T}},
                              F::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    return GreaterToIntervalBridge{T, F}
end

"""
    LessToIntervalBridge{T, F<:MOI.AbstractScalarFunction} <:
        AbstractToIntervalBridge{T, MOI.LessThan{T}, F}

Transforms a `F`-in-`LessThan{T}` constraint into an `F`-in-`Interval{T}`
constraint.
"""
struct LessToIntervalBridge{T, F<:MOI.AbstractScalarFunction} <:
    AbstractToIntervalBridge{T, MOI.LessThan{T}, F}
    constraint::CI{F, MOI.Interval{T}}
end
map_set(::Type{<:LessToIntervalBridge}, set::MOI.LessThan) = MOI.Interval(typemin(set.upper), set.upper)
inverse_map_set(::Type{<:LessToIntervalBridge}, set::MOI.Interval) = MOI.LessThan(set.upper)
function concrete_bridge_type(::Type{<:LessToIntervalBridge{T}},
                              F::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.LessThan{T}}) where T
    return LessToIntervalBridge{T, F}
end
