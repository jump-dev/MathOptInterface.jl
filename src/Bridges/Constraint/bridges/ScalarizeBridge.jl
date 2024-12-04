# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ScalarizeBridge{T,F,S}

`ScalarizeBridge` implements the following reformulations:

  * ``f(x) - a \\in \\mathbb{R}_+`` into ``f_i(x) \\ge a_i`` for all ``i``
  * ``f(x) - a \\in \\mathbb{R}_-`` into ``f_i(x) \\le a_i`` for all ``i``
  * ``f(x) - a \\in \\{0\\}`` into ``f_i(x) == a_i`` for all ``i``

## Source node

`ScalarizeBridge` supports:

  * `G` in [`MOI.Nonnegatives{T}`](@ref)
  * `G` in [`MOI.Nonpositives{T}`](@ref)
  * `G` in [`MOI.Zeros{T}`](@ref)

## Target nodes

`ScalarizeBridge` creates:

  * `F` in `S`, where `S` is one of [`MOI.GreaterThan{T}`](@ref),
    [`MOI.LessThan{T}`](@ref), and [`MOI.EqualTo{T}`](@ref), depending on the
    type of the input set.
"""
mutable struct ScalarizeBridge{T,F,S} <: AbstractBridge
    scalar_constraints::Vector{MOI.ConstraintIndex{F,S}}
    constants::Vector{T}
end

const Scalarize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ScalarizeBridge{T},OT}

function bridge_constraint(
    ::Type{ScalarizeBridge{T,F,S}},
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
    set::MOI.Utilities.VectorLinearSet,
) where {T,F,S}
    constraints = MOI.ConstraintIndex{F,S}[
        MOI.Utilities.normalize_and_add_constraint(model, fi, S(zero(T)))
        for fi in MOI.Utilities.eachscalar(f)
    ]
    return ScalarizeBridge{T,F,S}(constraints, MOI.constant(f, T))
end

function MOI.supports_constraint(
    ::Type{ScalarizeBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    S::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    # If `F` is `MOI.VectorAffineFunction{Complex{Float64}}`, `S` is `MOI.Zeros`
    # and `T` is `Float64`, it would create a set `MOI.EqualTo{Float64}` which
    # is incorrect hence we say we only support it if the coefficient type of
    # `F` is `T`.
    # If `S` is `MOI.Nonnegatives` or `MOI.Nonpositives` then `T` must be
    # `Real` as otherwise, `scalar_set_type` will fail in `concrete_bridge_type`.
    return MOI.Utilities.is_coefficient_type(F, T) &&
           (S === MOI.Zeros || (T <: Real))
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:ScalarizeBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ScalarizeBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function concrete_bridge_type(
    ::Type{<:ScalarizeBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    S::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    return ScalarizeBridge{
        T,
        MOI.Utilities.scalar_type(F),
        MOI.Utilities.scalar_set_type(S, T),
    }
end

function MOI.get(
    bridge::ScalarizeBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {T,F,S}
    return length(bridge.scalar_constraints)
end

function MOI.get(
    bridge::ScalarizeBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return copy(bridge.scalar_constraints)
end

function MOI.delete(model::MOI.ModelLike, bridge::ScalarizeBridge)
    for ci in bridge.scalar_constraints
        MOI.delete(model, ci)
    end
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge,
    i::MOI.Bridges.IndexInVector,
)
    MOI.delete(model, bridge.scalar_constraints[i.value])
    deleteat!(bridge.scalar_constraints, i.value)
    deleteat!(bridge.constants, i.value)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T},
) where {T}
    func = MOI.Utilities.vectorize(
        MOI.get.(model, attr, bridge.scalar_constraints),
    )
    if func isa MOI.VectorOfVariables
        return func
    end
    # `func` is in terms of bridged variables here while in `bridge_constraint`
    # it was in terms of the solver variables so `MOI.constant(set)` might be
    # different than `bridge.constants[i]`.
    for i in eachindex(bridge.scalar_constraints)
        set = MOI.get(model, MOI.ConstraintSet(), bridge.scalar_constraints[i])
        rhs = MOI.constant(set)
        if !iszero(rhs)
            func = MOI.Utilities.operate_output_index!(-, T, i, func, rhs)
        end
    end
    return func
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ScalarizeBridge{T,F,S},
) where {T,F,S}
    return MOI.Utilities.vector_set_type(S)(length(bridge.scalar_constraints))
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{ScalarizeBridge{T,F,S}},
) where {T,F,S}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::ScalarizeBridge,
)
    return MOI.get.(model, attr, bridge.scalar_constraints) .+ bridge.constants
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
)
    values = MOI.get.(model, attr, bridge.scalar_constraints)
    if any(isnothing, values)
        return
    end
    return values .+ bridge.constants
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
    ::Nothing,
)
    for ci in bridge.scalar_constraints
        MOI.set(model, attr, ci, nothing)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::ScalarizeBridge,
    value,
)
    MOI.set.(model, attr, bridge.scalar_constraints, value .- bridge.constants)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::ScalarizeBridge,
)
    return MOI.get.(model, attr, bridge.scalar_constraints)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::ScalarizeBridge,
)
    values = MOI.get.(model, attr, bridge.scalar_constraints)
    if any(isnothing, values)
        return
    end
    return values
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::ScalarizeBridge,
    value,
)
    MOI.set.(model, attr, bridge.scalar_constraints, value)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge{T,F,S},
    change::MOI.VectorConstantChange{T},
) where {T,F,S}
    bridge.constants = change.new_constant
    MOI.set.(
        model,
        MOI.ConstraintSet(),
        bridge.scalar_constraints,
        S.(-change.new_constant),
    )
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::ScalarizeBridge,
    change::MOI.MultirowChange{T},
) where {T}
    for (index, value) in change.new_coefficients
        MOI.modify(
            model,
            bridge.scalar_constraints[index],
            MOI.ScalarCoefficientChange{T}(change.variable, value),
        )
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T,F,S},
    func,
) where {T,F,S}
    bridge.constants = MOI.constant(func, T)
    for (i, fi) in enumerate(MOI.Utilities.eachscalar(func))
        ci = bridge.scalar_constraints[i]
        if !iszero(bridge.constants[i])
            fi = MOI.Utilities.operate!(-, T, fi, bridge.constants[i])
        end
        MOI.set(model, MOI.ConstraintFunction(), ci, fi)
        MOI.set(model, MOI.ConstraintSet(), ci, S(-bridge.constants[i]))
    end
    return
end

# TODO implement transform
