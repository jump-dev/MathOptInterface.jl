"""
    RSOCtoPSDBridge{T} <: Bridges.Variable.AbstractBridge

Transforms constrained variables in
[`MathOptInterface.RotatedSecondOrderCone`](@ref)
to constrained variables in
[`MathOptInterface.PositiveSemidefiniteConeTriangle`](@ref).
"""
struct RSOCtoPSDBridge{T} <: AbstractBridge
    # `t` is `variables[1]`
    # `u` is `variables[2]/2`
    # `x` is `variables[[3, 5, 8, ...]]`
    variables::Vector{MOI.VariableIndex}
    psd::Union{
        MOI.ConstraintIndex{
            MOI.VectorOfVariables,
            MOI.PositiveSemidefiniteConeTriangle,
        },
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives},
    }
    off_diag::Vector{MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}}
    diag::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
end

function bridge_constrained_variable(
    ::Type{RSOCtoPSDBridge{T}},
    model::MOI.ModelLike,
    set::MOI.RotatedSecondOrderCone,
) where {T}
    off_diag = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}[]
    diag = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    if set.dimension <= 2
        variables, psd = MOI.add_constrained_variables(
            model,
            MOI.Nonnegatives(set.dimension),
        )
    else
        dim = set.dimension - 1
        variables, psd = MOI.add_constrained_variables(
            model,
            MOI.PositiveSemidefiniteConeTriangle(dim),
        )
        # This is `2 * u`
        u2 = variables[3]
        k = 3
        for j in 3:dim
            k += 1
            for i in 2:(j-1)
                k += 1
                push!(
                    off_diag,
                    MOI.add_constraint(
                        model,
                        variables[k],
                        MOI.EqualTo(zero(T)),
                    ),
                )
            end
            k += 1
            func = MOIU.operate(-, T, u2, variables[k])
            push!(diag, MOI.add_constraint(model, func, MOI.EqualTo(zero(T))))
        end
        @assert k == MOIU.trimap(dim, dim)
    end
    return RSOCtoPSDBridge{T}(variables, psd, off_diag, diag)
end

function supports_constrained_variable(
    ::Type{<:RSOCtoPSDBridge},
    ::Type{MOI.RotatedSecondOrderCone},
)
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:RSOCtoPSDBridge})
    return Tuple{Type}[
        (MOI.PositiveSemidefiniteConeTriangle,),
        (MOI.Nonnegatives,),
    ]
end

function MOIB.added_constraint_types(::Type{RSOCtoPSDBridge{T}}) where {T}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
    ]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::RSOCtoPSDBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::RSOCtoPSDBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::RSOCtoPSDBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S},
)::Int64 where {S<:Union{MOI.PositiveSemidefiniteConeTriangle,MOI.Nonnegatives}}
    if bridge.psd isa MOI.ConstraintIndex{MOI.VectorOfVariables,S}
        return 1
    else
        return 0
    end
end

function MOI.get(
    bridge::RSOCtoPSDBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S},
) where {S<:Union{MOI.PositiveSemidefiniteConeTriangle,MOI.Nonnegatives}}
    if bridge.psd isa MOI.ConstraintIndex{MOI.VectorOfVariables,S}
        return [bridge.psd]
    else
        return MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
    end
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.off_diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    return copy(bridge.off_diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.diag)
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::RSOCtoPSDBridge)
    for ci in bridge.diag
        MOI.delete(model, ci)
    end
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::RSOCtoPSDBridge)
    if bridge.psd isa
       MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
        dim = length(bridge.variables)
    else
        dim = length(bridge.diag) + 3
    end
    return MOI.RotatedSecondOrderCone(dim)
end

function _variable_map(bridge::RSOCtoPSDBridge, i::MOIB.IndexInVector)
    if bridge.psd isa
       MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
        return i.value
    elseif i.value == 1
        return 1
    elseif i.value == 2
        return 3
    else
        return MOIU.trimap(1, i.value - 1)
    end
end

function _variable(bridge::RSOCtoPSDBridge, i::MOIB.IndexInVector)
    return bridge.variables[_variable_map(bridge, i)]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::RSOCtoPSDBridge,
)
    values = MOI.get(model, attr, bridge.psd)
    n = MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge))
    mapped = [values[_variable_map(bridge, MOIB.IndexInVector(i))] for i in 1:n]
    if length(mapped) >= 2
        mapped[2] /= 2
    end
    return mapped
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::RSOCtoPSDBridge,
)
    dual = MOI.get(model, attr, bridge.psd)
    n = MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge))
    mapped = [dual[_variable_map(bridge, MOIB.IndexInVector(i))] for i in 1:n]
    for ci in bridge.diag
        mapped[2] += MOI.get(model, attr, ci)
    end
    for i in 2:length(mapped)
        # For `i = 2`, we multiply by 2 because it is `2u`.
        # For `i > 2`, we multiply by 2 because to account for the difference
        # of scalar product `MOIU.set_dot`.
        mapped[i] *= 2
    end
    return mapped
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimal,
    bridge::RSOCtoPSDBridge,
    i::MOIB.IndexInVector,
)
    value = MOI.get(model, attr, _variable(bridge, i))
    if i.value == 2
        return value / 2
    else
        return value
    end
end

function MOIB.bridged_function(
    bridge::RSOCtoPSDBridge{T},
    i::MOIB.IndexInVector,
) where {T}
    func = _variable(bridge, i)
    if i.value == 2
        return MOIU.operate(/, T, func, convert(T, 2))
    else
        return convert(MOI.ScalarAffineFunction{T}, func)
    end
end

function unbridged_map(
    bridge::RSOCtoPSDBridge{T},
    vi::MOI.VariableIndex,
    i::MOIB.IndexInVector,
) where {T}
    if i.value == 2
        func = MOIU.operate(*, T, convert(T, 2), vi)
    else
        func = convert(MOI.ScalarAffineFunction{T}, vi)
    end
    return (_variable(bridge, i) => func,)
end
