"""
    FlipSignBridge{T, S1, S2, F, G}

Bridge a `G`-in-`S1` constraint into an `F`-in-`S2` constraint by multiplying
the function by `-1` and taking the point reflection of the set across the
origin. The flipped `F`-in-`S` constraint is stored in the `constraint`
field by convention.
"""
abstract type FlipSignBridge{
    T,
    S1<:MOI.AbstractSet,
    S2<:MOI.AbstractSet,
    F<:MOI.AbstractFunction,
    G<:MOI.AbstractFunction,
} <: SetMapBridge{T,S2,S1,F,G} end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    i::MOIB.IndexInVector,
)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOIU.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.constraint)
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    change::MOI.ScalarCoefficientChange,
)
    MOI.modify(
        model,
        bridge.constraint,
        MOI.ScalarCoefficientChange(change.variable, -change.new_coefficient),
    )
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    change::MOI.MultirowChange{T},
) where {T}
    new_coefficients = Tuple{Int64,T}[
        (index, -coef) for (index, coef) in change.new_coefficients
    ]
    MOI.modify(
        model,
        bridge.constraint,
        MOI.MultirowChange(change.variable, new_coefficients),
    )
    return
end

"""
    GreaterToLessBridge{
        T,
        F<:MOI.AbstractScalarFunction,
        G<:MOI.AbstractScalarFunction
    } <: FlipSignBridge{T, MOI.GreaterThan{T}, MOI.LessThan{T}, F, G}

Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`LessThan{T}`
constraint.
"""
struct GreaterToLessBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    G<:MOI.AbstractScalarFunction,
} <: FlipSignBridge{T,MOI.GreaterThan{T},MOI.LessThan{T},F,G}
    constraint::CI{F,MOI.LessThan{T}}
end

function MOIB.map_set(::Type{<:GreaterToLessBridge}, set::MOI.GreaterThan)
    return MOI.LessThan(-set.lower)
end

function MOIB.inverse_map_set(::Type{<:GreaterToLessBridge}, set::MOI.LessThan)
    return MOI.GreaterThan(-set.upper)
end

function concrete_bridge_type(
    ::Type{<:GreaterToLessBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    F = MOIU.promote_operation(-, T, G)
    return GreaterToLessBridge{T,F,G}
end

"""
    LessToGreaterBridge{
        T,
        F<:MOI.AbstractScalarFunction,
        G<:MOI.AbstractScalarFunction
    } <: FlipSignBridge{T, MOI.LessThan{T}, MOI.GreaterThan{T}, F, G}

Transforms a `G`-in-`LessThan{T}` constraint into an `F`-in-`GreaterThan{T}`
constraint.
"""
struct LessToGreaterBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    G<:MOI.AbstractScalarFunction,
} <: FlipSignBridge{T,MOI.LessThan{T},MOI.GreaterThan{T},F,G}
    constraint::CI{F,MOI.GreaterThan{T}}
end

function MOIB.map_set(::Type{<:LessToGreaterBridge}, set::MOI.LessThan)
    return MOI.GreaterThan(-set.upper)
end

function MOIB.inverse_map_set(
    ::Type{<:LessToGreaterBridge},
    set::MOI.GreaterThan,
)
    return MOI.LessThan(-set.lower)
end

function concrete_bridge_type(
    ::Type{<:LessToGreaterBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    ::Type{MOI.LessThan{T}},
) where {T}
    F = MOIU.promote_operation(-, T, G)
    return LessToGreaterBridge{T,F,G}
end

"""
    NonnegToNonposBridge{
        T,
        F<:MOI.AbstractVectorFunction,
        G<:MOI.AbstractVectorFunction
    } <: FlipSignBridge{T, MOI.NonnegativeCone, MOI.Nonpositives, F, G}

Transforms a `G`-in-`NonnegativeCone` constraint into a `F`-in-`Nonpositives`
constraint.
"""
mutable struct NonnegToNonposBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractVectorFunction,
} <: FlipSignBridge{T,MOI.NonnegativeCone,MOI.Nonpositives,F,G}
    constraint::CI{F,MOI.Nonpositives}
end

function concrete_bridge_type(
    ::Type{<:NonnegToNonposBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NonnegativeCone},
) where {T}
    F = MOIU.promote_operation(-, T, G)
    return NonnegToNonposBridge{T,F,G}
end

"""
    NonposToNonnegBridge{
        T,
        F<:MOI.AbstractVectorFunction,
        G<:MOI.AbstractVectorFunction,
    } <: FlipSignBridge{T, MOI.Nonpositives, MOI.NonnegativeCone, F, G}

Transforms a `G`-in-`Nonpositives` constraint into a `F`-in-`NonnegativeCone`
constraint.
"""
mutable struct NonposToNonnegBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractVectorFunction,
} <: FlipSignBridge{T,MOI.Nonpositives,MOI.NonnegativeCone,F,G}
    constraint::CI{F,MOI.NonnegativeCone}
end

function MOIB.map_set(::Type{<:NonposToNonnegBridge}, set::MOI.Nonpositives)
    return MOI.NonnegativeCone(set.dimension)
end

function MOIB.inverse_map_set(
    ::Type{<:NonposToNonnegBridge},
    set::MOI.NonnegativeCone,
)
    return MOI.Nonpositives(set.dimension)
end

function concrete_bridge_type(
    ::Type{<:NonposToNonnegBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Nonpositives},
) where {T}
    F = MOIU.promote_operation(-, T, G)
    return NonposToNonnegBridge{T,F,G}
end
