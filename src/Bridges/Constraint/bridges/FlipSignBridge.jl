# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FlipSignBridge{T,S1,S2,F,G}

An abstract type that simplifies the creation of other bridges.
"""
abstract type FlipSignBridge{
    T,
    S1<:MOI.AbstractSet,
    S2<:MOI.AbstractSet,
    F<:MOI.AbstractFunction,
    G<:MOI.AbstractFunction,
} <: SetMapBridge{T,S2,S1,F,G} end

function MOI.Bridges.map_function(::Type{<:FlipSignBridge{T}}, func) where {T}
    return MOI.Utilities.operate(-, T, func)
end

# The map is an involution
function MOI.Bridges.inverse_map_function(BT::Type{<:FlipSignBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:FlipSignBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric involution
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:FlipSignBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    i::MOI.Bridges.IndexInVector,
)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOI.Utilities.eachscalar(func)[idx]
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
    GreaterToLessBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`GreaterToLessBridge` implements the following reformulation:

  * ``f(x) \\ge l`` into ``-f(x) \\le -l``

## Source node

`GreaterToLessBridge` supports:

  * `G` in [`MOI.GreaterThan{T}`](@ref)

## Target nodes

`GreaterToLessBridge` creates:

  * `F` in [`MOI.LessThan{T}`](@ref)
"""
struct GreaterToLessBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    G<:MOI.AbstractScalarFunction,
} <: FlipSignBridge{T,MOI.GreaterThan{T},MOI.LessThan{T},F,G}
    constraint::MOI.ConstraintIndex{F,MOI.LessThan{T}}
end

const GreaterToLess{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GreaterToLessBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:GreaterToLessBridge},
    set::MOI.GreaterThan,
)
    return MOI.LessThan(-set.lower)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:GreaterToLessBridge},
    set::MOI.LessThan,
)
    return MOI.GreaterThan(-set.upper)
end

function concrete_bridge_type(
    ::Type{<:GreaterToLessBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    F = MOI.Utilities.promote_operation(-, T, G)
    return GreaterToLessBridge{T,F,G}
end

"""
    LessToGreaterBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`LessToGreaterBridge` implements the following reformulation:

* ``f(x) \\le u`` into ``-f(x) \\ge -u``

## Source node

`LessToGreaterBridge` supports:

* `G` in [`MOI.LessThan{T}`](@ref)

## Target nodes

`LessToGreaterBridge` creates:

* `F` in [`MOI.GreaterThan{T}`](@ref)
"""
struct LessToGreaterBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    G<:MOI.AbstractScalarFunction,
} <: FlipSignBridge{T,MOI.LessThan{T},MOI.GreaterThan{T},F,G}
    constraint::MOI.ConstraintIndex{F,MOI.GreaterThan{T}}
end

const LessToGreater{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{LessToGreaterBridge{T},OT}

function MOI.Bridges.map_set(::Type{<:LessToGreaterBridge}, set::MOI.LessThan)
    return MOI.GreaterThan(-set.upper)
end

function MOI.Bridges.inverse_map_set(
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
    F = MOI.Utilities.promote_operation(-, T, G)
    return LessToGreaterBridge{T,F,G}
end

"""
    NonnegToNonposBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`NonnegToNonposBridge` implements the following reformulation:

  * ``f(x) \\in \\mathbb{R}_+`` into ``-f(x) \\in \\mathbb{R}_-``

## Source node

`NonnegToNonposBridge` supports:

  * `G` in [`MOI.Nonnegatives`](@ref)

## Target nodes

`NonnegToNonposBridge` creates:

  * `F` in [`MOI.Nonpositives`](@ref)
"""
mutable struct NonnegToNonposBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractVectorFunction,
} <: FlipSignBridge{T,MOI.Nonnegatives,MOI.Nonpositives,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.Nonpositives}
end

const NonnegToNonpos{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonnegToNonposBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:NonnegToNonposBridge},
    set::MOI.Nonnegatives,
)
    return MOI.Nonpositives(set.dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:NonnegToNonposBridge},
    set::MOI.Nonpositives,
)
    return MOI.Nonnegatives(set.dimension)
end

function concrete_bridge_type(
    ::Type{<:NonnegToNonposBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Nonnegatives},
) where {T}
    F = MOI.Utilities.promote_operation(-, T, G)
    return NonnegToNonposBridge{T,F,G}
end

"""
    NonposToNonnegBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`NonposToNonnegBridge` implements the following reformulation:

  * ``f(x) \\in \\mathbb{R}_-`` into ``-f(x) \\in \\mathbb{R}_+``

## Source node

`NonposToNonnegBridge` supports:

  * `G` in [`MOI.Nonpositives`](@ref)

## Target nodes

`NonposToNonnegBridge` creates:

  * `F` in [`MOI.Nonnegatives`](@ref)
"""
mutable struct NonposToNonnegBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractVectorFunction,
} <: FlipSignBridge{T,MOI.Nonpositives,MOI.Nonnegatives,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.Nonnegatives}
end

const NonposToNonneg{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonposToNonnegBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:NonposToNonnegBridge},
    set::MOI.Nonpositives,
)
    return MOI.Nonnegatives(set.dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:NonposToNonnegBridge},
    set::MOI.Nonnegatives,
)
    return MOI.Nonpositives(set.dimension)
end

function concrete_bridge_type(
    ::Type{<:NonposToNonnegBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Nonpositives},
) where {T}
    F = MOI.Utilities.promote_operation(-, T, G)
    return NonposToNonnegBridge{T,F,G}
end
