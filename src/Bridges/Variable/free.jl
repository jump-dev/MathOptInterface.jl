"""
    FreeBridge{T} <: Bridges.Variable.AbstractBridge

Transforms constrained variables in [`MOI.Reals`](@ref) to the difference of
constrained variables in [`MOI.Nonnegatives`](@ref).
"""
struct FreeBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
end

function bridge_constrained_variable(
    ::Type{FreeBridge{T}},
    model::MOI.ModelLike,
    set::MOI.Reals,
) where {T}
    variables, constraint = MOI.add_constrained_variables(
        model,
        MOI.Nonnegatives(2MOI.dimension(set)),
    )
    return FreeBridge{T}(variables, constraint)
end

function supports_constrained_variable(::Type{<:FreeBridge}, ::Type{MOI.Reals})
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:FreeBridge})
    return [(MOI.Nonnegatives,)]
end

function MOIB.added_constraint_types(::Type{FreeBridge{T}}) where {T}
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::FreeBridge, ::MOI.NumberOfVariables)
    return length(bridge.variables)
end

function MOI.get(bridge::FreeBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    ::FreeBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives},
)
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

function MOI.delete(model::MOI.ModelLike, bridge::FreeBridge, i::IndexInVector)
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
    i::IndexInVector,
) where {T}
    n = div(length(bridge.variables), 2)
    return MOI.get(model, attr, bridge.variables[i.value]) -
           MOI.get(model, attr, bridge.variables[n+i.value])
end

function MOIB.bridged_function(
    bridge::FreeBridge{T},
    i::IndexInVector,
) where {T}
    n = div(length(bridge.variables), 2)
    return MOIU.operate(
        -,
        T,
        MOI.SingleVariable(bridge.variables[i.value]),
        MOI.SingleVariable(bridge.variables[n+i.value]),
    )
end

# x_free has been replaced by x[i] - x[n + i].
# To undo it we replace x[i] by x_free and x[n + i] by 0.
function unbridged_map(
    bridge::FreeBridge{T},
    vi::MOI.VariableIndex,
    i::IndexInVector,
) where {T}
    sv = MOI.SingleVariable(vi)
    # `unbridged_map` is required to return a `MOI.ScalarAffineFunction`.
    func = convert(MOI.ScalarAffineFunction{T}, sv)
    n = div(length(bridge.variables), 2)
    return bridge.variables[i.value] => func,
    bridge.variables[n+i.value] => zero(MOI.ScalarAffineFunction{T})
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
    i::IndexInVector,
)
    if value < 0
        nonneg = zero(value)
        nonpos = -value
    else
        nonneg = value
        nonpos = zero(value)
    end
    n = div(length(bridge.variables), 2)
    MOI.set(model, attr, bridge.variables[i.value], nonneg)
    MOI.set(model, attr, bridge.variables[n+i.value], nonpos)
    return
end
