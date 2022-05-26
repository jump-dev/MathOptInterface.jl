# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FreeBridge{T} <: Bridges.Variable.AbstractBridge

`FreeBridge` implements the following reformulation:

* ``x \\in \\mathbb{R}`` into ``y, z \\ge 0`` with the substitution
  rule ``x = y - z``,

where `T` is the coefficient type of `y - z`.

## Source node

`FreeBridge` supports:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Reals`](@ref)

## Target nodes

`FreeBridge` creates:

 * One variable node: [`MOI.VectorOfVariables`](@ref) in [`MOI.Nonnegatives`](@ref)
"""
struct FreeBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
end

const Free{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{FreeBridge{T},OT}

function bridge_constrained_variable(
    ::Type{FreeBridge{T}},
    model::MOI.ModelLike,
    set::MOI.Reals,
) where {T}
    variables, constraint = MOI.add_constrained_variables(
        model,
        MOI.Nonnegatives(2 * MOI.dimension(set)),
    )
    return FreeBridge{T}(variables, constraint)
end

supports_constrained_variable(::Type{<:FreeBridge}, ::Type{MOI.Reals}) = true

function MOI.Bridges.added_constrained_variable_types(::Type{<:FreeBridge})
    return Tuple{Type}[(MOI.Nonnegatives,)]
end

function MOI.Bridges.added_constraint_types(::Type{FreeBridge{T}}) where {T}
    return Tuple{Type,Type}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::FreeBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::FreeBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    ::FreeBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives},
)::Int64
    return 1
end

function MOI.get(
    bridge::FreeBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives},
)
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::FreeBridge)
    MOI.delete(model, bridge.variables)
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FreeBridge,
    i::MOI.Bridges.IndexInVector,
)
    n = div(length(bridge.variables), 2)
    MOI.delete(model, bridge.variables[i.value])
    MOI.delete(model, bridge.variables[n+i.value])
    deleteat!(bridge.variables, [i.value, n + i.value])
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::FreeBridge{T},
) where {T}
    n = div(length(bridge.variables), 2)
    primal = MOI.get(model, attr, bridge.constraint)
    return primal[1:n] - primal[n.+(1:n)]
end

# The transformation is x_free = [I -I] * x
# so the transformation of the dual is
# y = [I; -I] * y_free
# that is
# y[1:n] = -y[n .+ (1:n)] = y_free
# We can therefore compute `y_free` from either of them, let's take `y[1:n]`.
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::FreeBridge{T},
) where {T}
    n = div(length(bridge.variables), 2)
    return MOI.get(model, attr, bridge.constraint)[1:n]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::FreeBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    n = div(length(bridge.variables), 2)
    return MOI.get(model, attr, bridge.variables[i.value]) -
           MOI.get(model, attr, bridge.variables[n+i.value])
end

function MOI.Bridges.bridged_function(
    bridge::FreeBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    n = div(length(bridge.variables), 2)
    y = bridge.variables[i.value]
    z = bridge.variables[n+i.value]
    return MOI.Utilities.operate(-, T, y, z)
end

# x_free has been replaced by x[i] - x[n + i].
# To undo it we replace x[i] by x_free and x[n + i] by 0.
function unbridged_map(
    bridge::FreeBridge{T},
    vi::MOI.VariableIndex,
    i::MOI.Bridges.IndexInVector,
) where {T}
    n = div(length(bridge.variables), 2)
    y = bridge.variables[i.value] => convert(MOI.ScalarAffineFunction{T}, vi)
    z = bridge.variables[n+i.value] => zero(MOI.ScalarAffineFunction{T})
    return y, z
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:FreeBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::FreeBridge,
    value,
    i::MOI.Bridges.IndexInVector,
)
    n = div(length(bridge.variables), 2)
    MOI.set(model, attr, bridge.variables[i.value], max(zero(value), value))
    MOI.set(model, attr, bridge.variables[n+i.value], -min(zero(value), value))
    return
end
