"""
    ChancetoSOCBridge{T, F, G}

The [`IndependentNormalChance`](@ref) is [`SecondOrderCone`](@ref) representable.
Indeed, consider the chance constraint
```math
\\Pr\\left[t + x^\\top \\xi \\le u \\right] \\geq p
```
Recall that ``t + x^\\top \\xi`` is normally distributed with mean
``\\tilde{\\mu} = t + x^\\top \\mu`` and variance
``\\tilde{\\sigma}^2 = \\sum x_i^2 \\sigma_i^2`.
Then the constraint is equivalent to ``\\Phi((u - \\tilde{\\mu}) / \\tilde{\\sigma}) \\ge p``.
where ``\\Phi`` is the standard normal cumulative distribution function.
Since ``\\Phi^{-1}`` is monotonically increasing, this is equivalent to
``(u - \\tilde{\\mu}) / \\tilde{\\sigma}) \\ge \\Phi^{-1}(p)`` which
can be rewritten into
```math
u - t - x^\\top \\mu \\ge \\sqrt{\\sum (\\Phi^{-1}(p) x_i \\sigma_i)^2}
```
as ``\\Phi^{-1}(p)`` is nonnegative (since ``p \\ge 0.5``).
This is a second-order cone constraint.
"""
struct ChancetoSOCBridge{T, F, S, G} <: SetMapBridge{T, MOI.SecondOrderCone, MOI.IndependentNormalChance{T, S}, F, G}
    constraint::CI{F, MOI.SecondOrderCone}
    set::IndependentNormalChance{T, S}
end
const LessOrGreater{T} = Union{MOI.GreaterThan{T}, MOI.LessThan{T}}
function _map_function(::Type{<:ChancetoSOCBridge{T, F, S}},
                       func, set::IndependentNormalChance,
                       homogenized::Bool) where {T, S<:LessOrGreater{T}, F}
    scalars = MOIU.eachscalar(func)
    a = MA.copy_if_mutable(scalars[1])
    if !homogenized
        a = MOIU.operate!(-, T, MOI.constant(set.set))
    end
    for i in eachindex(set.μ)
        a = MA.operate!(MA.add_mul, a, μ[i], scalars[1 + i])
    end
    if S == MOI.LessThan{T}
        a = MOIU.operate!(-, T, a)
    end
    output = Vector{typeof(a)}(undef, length(scalars))
    output[1] = a
    q = Distributions.quantile(Distributions.Normal(0, 1), set.probability)
    # If `q` is negative, we cannot move it inside the norm in the proof above.
    if q < 0
        error("The probability $(set.probability) is less than 0.5.")
    end
    for i in eachindex(set.σ)
        output[1 + i] = MOIU.operate(*, T, q * set.σ[i], scalars[1 + i])
    end
    return MOIU.vectorize(output)
end
function _inverse_map_function(::Type{<:ChancetoSOCBridge{T, F, S}},
                               func, set::IndependentNormalChance,
                               homogenized::Bool) where {T, S<:LessOrGreater{T}, F}
    scalars = MOIU.eachscalar(func)
    if S == MOI.LessThan{T}
        t = MOIU.operate(-, T, scalars[1])
    else
        t = MA.copy_if_mutable(scalars[1])
    end
    if !homogenized
        t = MOIU.operate!(+, T, MOI.constant(set.set))
    end
    output = Vector{typeof(t)}(undef, length(scalars))
    q = Distributions.quantile(Distributions.Normal(0, 1), set.probability)
    # If `q` is negative, we cannot move it inside the norm in the proof above.
    if q < 0
        error("The probability $(set.probability) is less than 0.5.")
    end
    for i in eachindex(set.σ)
        output[1 + i] = MOIU.operate(/, T, scalars[1 + i], q * set.σ[i])
    end
    for i in eachindex(set.μ)
        t = MA.operate!(MA.sub_mul, t, μ[i], output[1 + i])
    end
    output[1] = t
    return MOIU.vectorize(output)
end

function bridge_constraint(BT::Type{ChancetoSOCBridge{T, F, S, G}},
                           model::MOI.ModelLike, g::G,
                           set::IndependentNormalChance{T, S}) where {T, F, S, G}
    constraint = MOI.add_constraint(model, _map_function(BT, g, set, false),
                                    MOI.SecondOrderCone(MOI.dimension(set)))
    return ChancetoSOCBridge{T, F, S, G}(constraint, set)
end


function MOI.supports_constraint(
    ::Type{ChancetoSOCBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.IndependentNormalChance{T, S}}) where {T, S<:LessOrGreater{T}}
    return true
end
MOIB.added_constrained_variable_types(::Type{<:ChancetoSOCBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:ChancetoSOCBridge{T, F}}) where {T, F}
    return [(F, MOI.SecondOrderCone)]
end

function concrete_bridge_type(::Type{<:ChancetoSOCBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.IndependentNormalChance{T, S}}) where T
    S = MOIU.promote_operation(+, G, G)
    F = MOIU.promote_operation(*, T, S)
    return ChancetoSOCBridge{T, F, S, G}
end

# Attributes, Bridge acting as a model
function MOI.get(::ChancetoSOCBridge{T, F},
                 ::MOI.NumberOfConstraints{F, MOI.SecondOrderCone}) where {T, F}
    return 1
end
function MOI.get(bridge::ChancetoSOCBridge{T, F},
                 ::MOI.ListOfConstraintIndices{F, MOI.SecondOrderCone}) where {T, F}
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::ChancetoSOCBridge)
    MOI.delete(model, bridge.constraint)
end

# Attributes, Bridge acting as a constraint

function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart},
    ::Type{<:ChancetoSOCBridge})

    return true
end
function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.ConstraintPrimal, MOI.ConstraintPrimalStart},
                 bridge::ChancetoSOCBridge)
    return _inverse_map_function(
        typeof(bridge), MOI.get(model, attr, bridge.constraint),
        bridge.set,
        attr isa MOI.ConstraintPrimal &&
            MOIU.is_ray(MOI.get(model, MOI.PrimalStatus(attr.N))))
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart,
                 bridge::ChancetoSOCBridge, value)
    MOI.set(model, attr, bridge.constraint,
            _map_function(typeof(bridge), value, bridge.set, false))
end
function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.ConstraintDual, MOI.ConstraintDualStart},
                 bridge::ChancetoSOCBridge)
    return _adjoint_map_function(
        typeof(bridge), MOI.get(model, attr, bridge.constraint),
        bridge.set)
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintDualStart,
                 bridge::ChancetoSOCBridge, value)
    MOI.set(model, attr, bridge.constraint, _inverse_adjoint_map_function(
        typeof(bridge), MOI.get(model, attr, bridge.constraint),
        bridge.set))
end
function MOI.modify(model::MOI.ModelLike, bridge::SetMapBridge,
                    change::MOI.VectorConstantChange)
    # By linearity of the map, we can just change the constant
    constant = _map_function(typeof(bridge), change.new_constant, bridge.set, true)
    MOI.modify(model, bridge.constraint, MOI.VectorConstantChange(constant))
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::ChancetoSOCBridge)
    return bridge.set
end
