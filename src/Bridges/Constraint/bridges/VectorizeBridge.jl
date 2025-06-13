# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    VectorizeBridge{T,F,S,G} <: Bridges.Constraint.AbstractBridge

`VectorizeBridge` implements the following reformulations:

  * ``g(x) \\ge a`` into ``[g(x) - a] \\in \\mathbb{R}_+``
  * ``g(x) \\le a`` into ``[g(x) - a] \\in \\mathbb{R}_-``
  * ``g(x) == a`` into ``[g(x) - a] \\in \\{0\\}``

where `T` is the coefficient type of `g(x) - a`.

See also [`IntervalToHyperRectangleBridge`](@ref) for double-sided bound
constraints.

## Source node

`VectorizeBridge` supports:

  * `G` in [`MOI.GreaterThan{T}`](@ref)
  * `G` in [`MOI.LessThan{T}`](@ref)
  * `G` in [`MOI.EqualTo{T}`](@ref)

## Target nodes

`VectorizeBridge` creates:

  * `F` in `S`, where `S` is one of [`MOI.Nonnegatives`](@ref),
    [`MOI.Nonpositives`](@ref), [`MOI.Zeros`](@ref) depending on the type of the
    input set.
"""
mutable struct VectorizeBridge{T,F,S,G} <: AbstractBridge
    vector_constraint::MOI.ConstraintIndex{F,S}
    set_constant::T # constant in scalar set
end

const Vectorize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorizeBridge{T},OT}

function bridge_constraint(
    ::Type{VectorizeBridge{T,F,S,G}},
    model::MOI.ModelLike,
    scalar_f::G,
    set::MOI.Utilities.ScalarLinearSet{T},
) where {T,F,S,G}
    MOI.throw_if_scalar_and_constant_not_zero(scalar_f, typeof(set))
    set_const = MOI.constant(set)
    vector_f = MOI.Utilities.operate(
        vcat,
        T,
        MOI.Utilities.operate(-, T, scalar_f, set_const),
    )
    vector_constraint = MOI.add_constraint(model, vector_f, S(1))
    return VectorizeBridge{T,F,S,G}(vector_constraint, set_const)
end

function MOI.supports_constraint(
    ::Type{VectorizeBridge{T}},
    ::Type{F},
    ::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T,F<:MOI.AbstractScalarFunction}
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:VectorizeBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:VectorizeBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function concrete_bridge_type(
    ::Type{<:VectorizeBridge{T}},
    G::Type{<:MOI.AbstractScalarFunction},
    S::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T}
    H = MOI.Utilities.promote_operation(-, T, G, T)
    F = MOI.Utilities.promote_operation(vcat, T, H)
    return VectorizeBridge{T,F,MOI.Utilities.vector_set_type(S),G}
end

function MOI.get(
    ::VectorizeBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {T,F,S}
    return 1
end

function MOI.get(
    bridge::VectorizeBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return [bridge.vector_constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    MOI.delete(model, bridge.vector_constraint)
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{VectorizeBridge{T,F,S,G}},
) where {T,F,S,G}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    if x === nothing
        return nothing
    end
    return only(x) + bridge.set_constant
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    if MOI.Utilities.is_ray(MOI.get(model, MOI.PrimalStatus(attr.result_index)))
        # If it is an infeasibility certificate, it is a ray and satisfies the
        # homogenized problem, see https://github.com/jump-dev/MathOptInterface.jl/issues/433
        return only(x)
    else
        # Otherwise, we need to add the set constant since the ConstraintPrimal
        # is defined as the value of the function and the set_constant was
        # removed from the original function
        return only(x) + bridge.set_constant
    end
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::VectorizeBridge,
    value,
)
    MOI.set(
        model,
        attr,
        bridge.vector_constraint,
        [value - bridge.set_constant],
    )
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::VectorizeBridge,
    ::Nothing,
)
    MOI.set(model, attr, bridge.vector_constraint, nothing)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    if x === nothing
        return nothing
    end
    return only(x)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::VectorizeBridge,
    value,
)
    if value === nothing
        MOI.set(model, attr, bridge.vector_constraint, nothing)
    else
        MOI.set(model, attr, bridge.vector_constraint, [value])
    end
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::VectorizeBridge,
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
    ::MOI.ConstraintSet,
    bridge::VectorizeBridge,
    new_set::MOI.Utilities.ScalarLinearSet,
)
    bridge.set_constant = MOI.constant(new_set)
    MOI.modify(
        model,
        bridge.vector_constraint,
        MOI.VectorConstantChange([-bridge.set_constant]),
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::VectorizeBridge{T,F,S,G},
) where {T,F,S,G}
    f = MOI.Utilities.scalarize(
        MOI.get(model, attr, bridge.vector_constraint),
        true,
    )
    return convert(G, only(f))
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::VectorizeBridge{T,F,S},
) where {T,F,S}
    return MOI.Utilities.scalar_set_type(S, T)(bridge.set_constant)
end
