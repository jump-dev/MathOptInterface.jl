# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IntervalToHyperRectangleBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`IntervalToHyperRectangleBridge` implements the following reformulations:

  * ``l \\le g(x) \\le u`` into ``[g(x)] \\in `` `MOI.HyperRectangle([l], [u])`

where `T` is the coefficient type of `g(x)` and the type of `l` and `u`.

See also [`VectorizeBridge`](@ref) for equality and single-sided bound
constraints.

## Source node

`IntervalToHyperRectangleBridge` supports:

  * `G` in [`MOI.Interval{T}`](@ref)

## Target nodes

`IntervalToHyperRectangleBridge` creates:

  * `F` in [`MOI.HyperRectangle{T}`](@ref).
"""
mutable struct IntervalToHyperRectangleBridge{T,F,G} <: AbstractBridge
    vector_constraint::MOI.ConstraintIndex{F,MOI.HyperRectangle{T}}
end

const IntervalToHyperRectangle{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IntervalToHyperRectangleBridge{T},OT}

function bridge_constraint(
    ::Type{IntervalToHyperRectangleBridge{T,F,G}},
    model::MOI.ModelLike,
    scalar_f::G,
    set::MOI.Interval{T},
) where {T,F,G}
    MOI.throw_if_scalar_and_constant_not_zero(scalar_f, typeof(set))
    vector_f = MOI.Utilities.operate(vcat, T, scalar_f)
    rect = MOI.HyperRectangle([set.lower], [set.upper])
    vector_constraint = MOI.add_constraint(model, vector_f, rect)
    return IntervalToHyperRectangleBridge{T,F,G}(vector_constraint)
end

function MOI.supports_constraint(
    ::Type{IntervalToHyperRectangleBridge{T}},
    ::Type{F},
    ::Type{MOI.Interval{T}},
) where {T,F<:MOI.AbstractScalarFunction}
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IntervalToHyperRectangleBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:IntervalToHyperRectangleBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.HyperRectangle{T})]
end

function concrete_bridge_type(
    ::Type{<:IntervalToHyperRectangleBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    S::Type{MOI.Interval{T}},
) where {T}
    F = MOI.Utilities.promote_operation(vcat, T, G)
    return IntervalToHyperRectangleBridge{T,F,G}
end

function MOI.get(
    ::IntervalToHyperRectangleBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.HyperRectangle{T}},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::IntervalToHyperRectangleBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.HyperRectangle{T}},
) where {T,F}
    return [bridge.vector_constraint]
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::IntervalToHyperRectangleBridge,
)
    MOI.delete(model, bridge.vector_constraint)
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{IntervalToHyperRectangleBridge{T,F,G}},
) where {T,F,G}
    return MOI.supports(
        model,
        attr,
        MOI.ConstraintIndex{F,MOI.HyperRectangle{T}},
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::IntervalToHyperRectangleBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    if isnothing(x)
        return nothing
    end
    return only(x)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IntervalToHyperRectangleBridge,
    value,
)
    MOI.set(model, attr, bridge.vector_constraint, [value])
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::IntervalToHyperRectangleBridge,
    ::Nothing,
)
    MOI.set(model, attr, bridge.vector_constraint, nothing)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::IntervalToHyperRectangleBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    if isnothing(x)
        return nothing
    end
    return only(x)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::IntervalToHyperRectangleBridge,
    value,
)
    if isnothing(value)
        MOI.set(model, attr, bridge.vector_constraint, nothing)
    else
        MOI.set(model, attr, bridge.vector_constraint, [value])
    end
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::IntervalToHyperRectangleBridge,
    change::MOI.ScalarCoefficientChange,
)
    MOI.modify(
        model,
        bridge.vector_constraint,
        MOI.MultirowChange(change.variable, [(1, change.new_coefficient)]),
    )
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::IntervalToHyperRectangleBridge,
    new_set::MOI.Interval,
)
    MOI.set(
        model,
        attr,
        bridge.vector_constraint,
        MOI.HyperRectangle([new_set.lower], [new_set.upper]),
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::IntervalToHyperRectangleBridge{T,F,G},
) where {T,F,G}
    return convert(
        G,
        only(
            MOI.Utilities.scalarize(
                MOI.get(model, attr, bridge.vector_constraint),
            ),
        ),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::IntervalToHyperRectangleBridge,
)
    rect = MOI.get(model, MOI.ConstraintSet(), bridge.vector_constraint)
    return MOI.Interval(only(rect.lower), only(rect.upper))
end
