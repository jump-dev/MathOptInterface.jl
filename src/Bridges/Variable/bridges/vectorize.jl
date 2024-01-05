# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    VectorizeBridge{T,S} <: Bridges.Variable.AbstractBridge

`VectorizeBridge` implements the following reformulations:

 * ``x \\ge a`` into ``[y] \\in \\mathbb{R}_+`` with the substitution
   rule ``x = a + y``
 * ``x \\le a`` into ``[y] \\in \\mathbb{R}_-`` with the substitution
   rule ``x = a + y``
 * ``x == a`` into ``[y] \\in \\{0\\}`` with the substitution
   rule ``x = a + y``

where `T` is the coefficient type of `a + y`.

## Source node

`VectorizeBridge` supports:

 * [`MOI.VariableIndex`](@ref) in [`MOI.GreaterThan{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.LessThan{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)

## Target nodes

`VectorizeBridge` creates:

 * One variable node: [`MOI.VectorOfVariables`](@ref) in `S`, where `S` is one
   of [`MOI.Nonnegatives`](@ref), [`MOI.Nonpositives`](@ref),
   [`MOI.Zeros`](@ref) depending on the type of ``S``.
"""
mutable struct VectorizeBridge{T,S} <: AbstractBridge
    variable::MOI.VariableIndex
    vector_constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,S}
    set_constant::T # constant in scalar set
end

const Vectorize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorizeBridge{T},OT}

function bridge_constrained_variable(
    ::Type{VectorizeBridge{T,S}},
    model::MOI.ModelLike,
    set::MOI.Utilities.ScalarLinearSet{T},
) where {T,S}
    set_constant = MOI.constant(set)
    variables, vector_constraint = MOI.add_constrained_variables(model, S(1))
    return VectorizeBridge{T,S}(variables[1], vector_constraint, set_constant)
end

function supports_constrained_variable(
    ::Type{VectorizeBridge{T}},
    ::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{VectorizeBridge{T,S}},
) where {T,S}
    return Tuple{Type}[(S,)]
end

function MOI.Bridges.added_constraint_types(::Type{<:VectorizeBridge})
    return Tuple{Type,Type}[]
end

function concrete_bridge_type(
    ::Type{<:VectorizeBridge{T}},
    S::Type{<:MOI.Utilities.ScalarLinearSet{T}},
) where {T}
    return VectorizeBridge{T,MOI.Utilities.vector_set_type(S)}
end

# Attributes, Bridge acting as a model
MOI.get(::VectorizeBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(bridge::VectorizeBridge, ::MOI.ListOfVariableIndices)
    return [bridge.variable]
end

function MOI.get(
    ::VectorizeBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    bridge::VectorizeBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S},
) where {T,S}
    return [bridge.vector_constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    # It isn't safe to delete the variable because the constant may appear in
    # other parts of the model (like the objective, or right-hand side sets).     Therefore, we don't implement
    err = MOI.DeleteNotAllowed(
        bridge.variable,
        "Cannot delete variable because it is bridged by the `VectorizeBridge`.",
    )
    return throw(err)
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::VectorizeBridge{T,S},
) where {T,S}
    return MOI.Utilities.scalar_set_type(S, T)(bridge.set_constant)
end

function MOI.set(
    ::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::VectorizeBridge,
    ::MOI.Utilities.ScalarLinearSet,
)
    # This would require modifing any constraint which uses the bridged
    # variable.
    return throw(
        MOI.SetAttributeNotAllowed(
            attr,
            "The variable `$(bridge.variable)` is bridged by the " *
            "`VectorizeBridge`.",
        ),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::VectorizeBridge,
)
    return throw(
        MOI.GetAttributeNotAllowed(
            attr,
            "The variable `$(bridge.variable)` is bridged by the " *
            "`VectorizeBridge`.",
        ),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::VectorizeBridge,
)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    return x[1]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimal,
    bridge::VectorizeBridge,
)
    value = MOI.get(model, attr, bridge.variable)
    status = MOI.get(model, MOI.PrimalStatus(attr.result_index))
    if MOI.Utilities.is_ray(status)
        return value  # Return the homogenized value.
    end
    return value + bridge.set_constant
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:VectorizeBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::VectorizeBridge,
)
    start = MOI.get(model, attr, bridge.variable)
    if start === nothing
        return nothing
    end
    return start + bridge.set_constant
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::VectorizeBridge,
    value,
)
    if value === nothing
        MOI.set(model, attr, bridge.variable, value)
    else
        MOI.set(model, attr, bridge.variable, value - bridge.set_constant)
    end
    return
end

function MOI.Bridges.bridged_function(bridge::VectorizeBridge{T}) where {T}
    return MOI.Utilities.operate(+, T, bridge.variable, bridge.set_constant)
end

function unbridged_map(
    bridge::VectorizeBridge{T},
    vi::MOI.VariableIndex,
) where {T}
    func = MOI.Utilities.operate(-, T, vi, bridge.set_constant)
    return (bridge.variable => func,)
end
