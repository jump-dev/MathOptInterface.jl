"""
    FlipSignBridge{S1, S2, F}

Bridge a `G`-in-`S1` constraint into an `F`-in-`S2` constraint by multiplying
the function by `-1` and taking the point reflection of the set across the
origin. The flipped `F`-in-`S` constraint is stored in the `flipped_constraint`
field by convention.
"""
abstract type FlipSignBridge{
    S1<:MOI.AbstractSet, S2<:MOI.AbstractSet,
    F<:MOI.AbstractFunction} <: AbstractBridge end

function MOI.supports_constraint(::Type{<:FlipSignBridge{S1}},
                                 ::Type{<:MOI.AbstractScalarFunction},
                                 ::Type{S1}) where {S1<:MOI.AbstractSet}
    return true
end
function added_constraint_types(
    ::Type{<:FlipSignBridge{S1, S2, F}}) where {S1, S2, F}
    return [(F, S2)]
end

# Attributes, Bridge acting as an model
function MOI.get(::FlipSignBridge{S1, S2, F},
                 ::MOI.NumberOfConstraints{F, S2}) where {S1, S2, F}
    return 1
end
function MOI.get(bridge::FlipSignBridge{S1, S2, F},
                 ::MOI.ListOfConstraintIndices{F, S2}) where {S1, S2, F}
    return [bridge.flipped_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::FlipSignBridge)
    MOI.delete(model, bridge.flipped_constraint)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.ConstraintPrimal, MOI.ConstraintDual},
                 bridge::FlipSignBridge)
    return -MOI.get(model, attr, bridge.flipped_constraint)
end

function MOI.modify(model::MOI.ModelLike, bridge::FlipSignBridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(
        model, bridge.flipped_constraint,
        MOI.ScalarCoefficientChange(change.variable, -change.new_coefficient))
end

function MOI.modify(model::MOI.ModelLike, bridge::FlipSignBridge,
                    change::MOI.MultirowChange{T}) where T
    new_coefficients = Tuple{Int64, T}[
        (index, -coef) for (index, coef) in change.new_coefficients]
    MOI.modify(model, bridge.flipped_constraint,
               MOI.MultirowChange(change.variable,
                                  new_coefficients))
end
function MOI.modify(model::MOI.ModelLike, bridge::FlipSignBridge,
                    change::MOI.VectorConstantChange)
    MOI.modify(model, bridge.flipped_constraint,
               MOI.VectorConstantChange(-change.new_constant))
end

"""
    GreaterToLessBridge{T, F<:MOI.AbstractScalarFunction} <:
        FlipSignBridge{MOI.GreaterThan{T}, MOI.LessThan{T}, F}

Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`LessThan{T}`
constraint.
"""
struct GreaterToLessBridge{T, F<:MOI.AbstractScalarFunction} <:
    FlipSignBridge{MOI.GreaterThan{T}, MOI.LessThan{T}, F}
    flipped_constraint::CI{F, MOI.LessThan{T}}
end
function GreaterToLessBridge{T, F}(model::MOI.ModelLike,
                                   g::MOI.AbstractScalarFunction,
                                   s::MOI.GreaterThan) where {T, F}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f, MOI.LessThan(-s.lower))
    return GreaterToLessBridge{T, F}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:GreaterToLessBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    F = MOIU.promote_operation(-, T, G)
    return GreaterToLessBridge{T, F}
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::GreaterToLessBridge, new_set::MOI.GreaterThan)
    MOI.set(model, attr, bridge.flipped_constraint,
            MOI.LessThan(-new_set.lower))
end

"""
    LessToGreaterBridge{T, F<:MOI.AbstractScalarFunction} <:
        FlipSignBridge{MOI.LessThan{T}, MOI.GreaterThan{T}, F}

Transforms a `G`-in-`LessThan{T}` constraint into an `F`-in-`GreaterThan{T}`
constraint.
"""
struct LessToGreaterBridge{T, F<:MOI.AbstractScalarFunction} <:
    FlipSignBridge{MOI.LessThan{T}, MOI.GreaterThan{T}, F}
    flipped_constraint::CI{F, MOI.GreaterThan{T}}
end
function LessToGreaterBridge{T, F}(model::MOI.ModelLike,
                                   g::MOI.AbstractScalarFunction,
                                   s::MOI.LessThan) where {T, F}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f, MOI.GreaterThan(-s.upper))
    return LessToGreaterBridge{T, F}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:LessToGreaterBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.LessThan{T}}) where T
    F = MOIU.promote_operation(-, T, G)
    return LessToGreaterBridge{T, F}
end
function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::LessToGreaterBridge, new_set::MOI.LessThan)
    MOI.set(model, attr, bridge.flipped_constraint,
            MOI.GreaterThan(-new_set.upper))
end

"""
    NonnegToNonposBridge{T, F<:MOI.AbstractVectorFunction}

Transforms a `G`-in-`Nonnegatives` constraint into a `F`-in-`Nonpositives`
constraint.
"""
struct NonnegToNonposBridge{T, F<:MOI.AbstractVectorFunction} <:
    FlipSignBridge{MOI.Nonnegatives, MOI.Nonpositives, F}
    flipped_constraint::CI{F, MOI.Nonpositives}
end
function NonnegToNonposBridge{T, F}(model::MOI.ModelLike,
                                    g::MOI.AbstractVectorFunction,
                                    s::MOI.Nonnegatives) where {T, F}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f,
                                            MOI.Nonpositives(s.dimension))
    return NonnegToNonposBridge{T, F}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:NonnegToNonposBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.Nonnegatives}) where T
    F = MOIU.promote_operation(-, T, G)
    return NonnegToNonposBridge{T, F}
end

"""
    NonposToNonnegBridge{T, F<:MOI.AbstractVectorFunction}

Transforms a `G`-in-`Nonpositives` constraint into a `F`-in-`Nonnegatives`
constraint.
"""
struct NonposToNonnegBridge{T, F<:MOI.AbstractVectorFunction} <:
    FlipSignBridge{MOI.Nonpositives, MOI.Nonnegatives, F}
    flipped_constraint::CI{F, MOI.Nonnegatives}
end
function NonposToNonnegBridge{T, F}(model::MOI.ModelLike,
                                    g::MOI.AbstractVectorFunction,
                                    s::MOI.Nonpositives) where {T, F}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f,
                                            MOI.Nonnegatives(s.dimension))
    return NonposToNonnegBridge{T, F}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:NonposToNonnegBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.Nonpositives}) where T
    F = MOIU.promote_operation(-, T, G)
    return NonposToNonnegBridge{T, F}
end
