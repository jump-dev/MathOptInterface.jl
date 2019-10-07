"""
    FlipSignBridge{T, S1, S2, F, G}

Bridge a `G`-in-`S1` constraint into an `F`-in-`S2` constraint by multiplying
the function by `-1` and taking the point reflection of the set across the
origin. The flipped `F`-in-`S` constraint is stored in the `flipped_constraint`
field by convention.
"""
abstract type FlipSignBridge{
    T, S1<:MOI.AbstractSet, S2<:MOI.AbstractSet,
    F<:MOI.AbstractFunction, G<:MOI.AbstractFunction} <: AbstractBridge end

function MOI.supports_constraint(
    ::Type{<:FlipSignBridge{T, S1}}, ::Type{<:MOI.AbstractScalarFunction},
    ::Type{S1}) where {T, S1<:MOI.AbstractScalarSet}
    return true
end
function MOI.supports_constraint(
    ::Type{<:FlipSignBridge{T, S1}}, ::Type{<:MOI.AbstractVectorFunction},
    ::Type{S1}) where {T, S1<:MOI.AbstractVectorSet}
    return true
end
MOIB.added_constrained_variable_types(::Type{<:FlipSignBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(
    ::Type{<:FlipSignBridge{T, S1, S2, F}}) where {T, S1, S2, F}
    return [(F, S2)]
end

# Attributes, Bridge acting as a model
function MOI.get(::FlipSignBridge{T, S1, S2, F},
                 ::MOI.NumberOfConstraints{F, S2}) where {T, S1, S2, F}
    return 1
end
function MOI.get(bridge::FlipSignBridge{T, S1, S2, F},
                 ::MOI.ListOfConstraintIndices{F, S2}) where {T, S1, S2, F}
    return [bridge.flipped_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::FlipSignBridge)
    MOI.delete(model, bridge.flipped_constraint)
end
function MOI.delete(model::MOI.ModelLike, bridge::FlipSignBridge, i::IndexInVector)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.flipped_constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOIU.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.flipped_constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.flipped_constraint)
    bridge.flipped_constraint = MOI.add_constraint(model, new_func, new_set)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.ConstraintPrimal, MOI.ConstraintDual},
                 bridge::FlipSignBridge)
    return -MOI.get(model, attr, bridge.flipped_constraint)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::FlipSignBridge{T, S1, S2, F, G}) where {T, S1, S2, F, G}
    func = MOIU.operate(-, T, MOI.get(model, attr, bridge.flipped_constraint))
    return MOIU.convert_approx(G, func)
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
    GreaterToLessBridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
        FlipSignBridge{T, MOI.GreaterThan{T}, MOI.LessThan{T}, F, G}

Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`LessThan{T}`
constraint.
"""
struct GreaterToLessBridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
    FlipSignBridge{T, MOI.GreaterThan{T}, MOI.LessThan{T}, F, G}
    flipped_constraint::CI{F, MOI.LessThan{T}}
end
function bridge_constraint(::Type{GreaterToLessBridge{T, F, G}},
                           model::MOI.ModelLike,
                           g::MOI.AbstractScalarFunction,
                           s::MOI.GreaterThan) where {T, F, G}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f, MOI.LessThan(-s.lower))
    return GreaterToLessBridge{T, F, G}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:GreaterToLessBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    F = MOIU.promote_operation(-, T, G)
    return GreaterToLessBridge{T, F, G}
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::GreaterToLessBridge, new_set::MOI.GreaterThan)
    MOI.set(model, attr, bridge.flipped_constraint,
            MOI.LessThan(-new_set.lower))
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::GreaterToLessBridge)
    set = MOI.get(model, attr, bridge.flipped_constraint)
    return MOI.GreaterThan(-set.upper)
end

"""
    LessToGreaterBridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
        FlipSignBridge{T, MOI.LessThan{T}, MOI.GreaterThan{T}, F, G}

Transforms a `G`-in-`LessThan{T}` constraint into an `F`-in-`GreaterThan{T}`
constraint.
"""
struct LessToGreaterBridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
    FlipSignBridge{T, MOI.LessThan{T}, MOI.GreaterThan{T}, F, G}
    flipped_constraint::CI{F, MOI.GreaterThan{T}}
end
function bridge_constraint(::Type{LessToGreaterBridge{T, F, G}},
                           model::MOI.ModelLike,
                           g::MOI.AbstractScalarFunction,
                           s::MOI.LessThan) where {T, F, G}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f, MOI.GreaterThan(-s.upper))
    return LessToGreaterBridge{T, F, G}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:LessToGreaterBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.LessThan{T}}) where T
    F = MOIU.promote_operation(-, T, G)
    return LessToGreaterBridge{T, F, G}
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::LessToGreaterBridge, new_set::MOI.LessThan)
    MOI.set(model, attr, bridge.flipped_constraint,
            MOI.GreaterThan(-new_set.upper))
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::LessToGreaterBridge)
    set = MOI.get(model, attr, bridge.flipped_constraint)
    return MOI.LessThan(-set.lower)
end

"""
    NonnegToNonposBridge{T, F<:MOI.AbstractVectorFunction, G<:MOI.AbstractVectorFunction} <:
        FlipSignBridge{T, MOI.Nonnegatives, MOI.Nonpositives, F, G}

Transforms a `G`-in-`Nonnegatives` constraint into a `F`-in-`Nonpositives`
constraint.
"""
mutable struct NonnegToNonposBridge{T, F<:MOI.AbstractVectorFunction, G<:MOI.AbstractVectorFunction} <:
    FlipSignBridge{T, MOI.Nonnegatives, MOI.Nonpositives, F, G}
    flipped_constraint::CI{F, MOI.Nonpositives}
end
function bridge_constraint(::Type{NonnegToNonposBridge{T, F, G}},
                           model::MOI.ModelLike,
                           g::MOI.AbstractVectorFunction,
                           s::MOI.Nonnegatives) where {T, F, G}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f,
                                            MOI.Nonpositives(s.dimension))
    return NonnegToNonposBridge{T, F, G}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:NonnegToNonposBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.Nonnegatives}) where T
    F = MOIU.promote_operation(-, T, G)
    return NonnegToNonposBridge{T, F, G}
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::NonnegToNonposBridge)
    set = MOI.get(model, attr, bridge.flipped_constraint)
    return MOI.Nonnegatives(MOI.dimension(set))
end

"""
    NonposToNonnegBridge{T, F<:MOI.AbstractVectorFunction, G<:MOI.AbstractVectorFunction} <:
        FlipSignBridge{T, MOI.Nonpositives, MOI.Nonnegatives, F, G}

Transforms a `G`-in-`Nonpositives` constraint into a `F`-in-`Nonnegatives`
constraint.
"""
mutable struct NonposToNonnegBridge{T, F<:MOI.AbstractVectorFunction, G<:MOI.AbstractVectorFunction} <:
    FlipSignBridge{T, MOI.Nonpositives, MOI.Nonnegatives, F, G}
    flipped_constraint::CI{F, MOI.Nonnegatives}
end
function bridge_constraint(::Type{NonposToNonnegBridge{T, F, G}},
                           model::MOI.ModelLike,
                           g::MOI.AbstractVectorFunction,
                           s::MOI.Nonpositives) where {T, F, G}
    f = MOIU.operate(-, T, g)
    flipped_constraint = MOI.add_constraint(model, f,
                                            MOI.Nonnegatives(s.dimension))
    return NonposToNonnegBridge{T, F, G}(flipped_constraint)
end
function concrete_bridge_type(::Type{<:NonposToNonnegBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.Nonpositives}) where T
    F = MOIU.promote_operation(-, T, G)
    return NonposToNonnegBridge{T, F, G}
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 bridge::NonposToNonnegBridge)
    set = MOI.get(model, attr, bridge.flipped_constraint)
    return MOI.Nonpositives(MOI.dimension(set))
end
