"""
    FlipSignBridge{T, S1, S2}

Bridge constrained variables in `S1` into constrained variables in `S2` by
multiplying the variables by `-1` and taking the point reflection of the set
across the origin. The flipped `MOI.VectorOfVariables`-in-`S` constraint is
stored in the `flipped_constraint` field by convention.
"""
abstract type FlipSignBridge{T,S1<:MOI.AbstractSet,S2<:MOI.AbstractSet} <:
              AbstractBridge end

function supports_constrained_variable(
    ::Type{<:FlipSignBridge{T,S1}},
    ::Type{S1},
) where {T,S1<:MOI.AbstractVectorSet}
    return true
end

function MOIB.added_constrained_variable_types(
    ::Type{<:FlipSignBridge{T,S1,S2}},
) where {T,S1,S2}
    return [(S2,)]
end

function MOIB.added_constraint_types(::Type{<:FlipSignBridge})
    return Tuple{DataType,DataType}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::FlipSignBridge, ::MOI.NumberOfVariables)
    return length(bridge.flipped_variables)
end

function MOI.get(bridge::FlipSignBridge, ::MOI.ListOfVariableIndices)
    return bridge.flipped_variables
end

function MOI.get(
    ::FlipSignBridge{T,S1,S2},
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S2},
) where {T,S1,S2<:MOI.AbstractVectorSet}
    return 1
end

function MOI.get(
    bridge::FlipSignBridge{T,S1,S2},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S2},
) where {T,S1,S2<:MOI.AbstractVectorSet}
    return [bridge.flipped_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::FlipSignBridge)
    MOI.delete(model, bridge.flipped_variables)
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FlipSignBridge,
    i::MOIB.IndexInVector,
)
    MOI.delete(model, bridge.flipped_variables[i.value])
    deleteat!(bridge.flipped_variables, i.value)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::FlipSignBridge{T,S1},
) where {T,S1<:MOI.AbstractVectorSet}
    return S1(length(bridge.flipped_variables))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintDual},
    bridge::FlipSignBridge,
)
    return -MOI.get(model, attr, bridge.flipped_constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::FlipSignBridge,
    i::MOIB.IndexInVector,
)
    return -MOI.get(model, attr, bridge.flipped_variables[i.value])
end

function MOIB.bridged_function(
    bridge::FlipSignBridge{T},
    i::MOIB.IndexInVector,
) where {T}
    func = MOI.SingleVariable(bridge.flipped_variables[i.value])
    return MOIU.operate(-, T, func)
end

function unbridged_map(
    bridge::FlipSignBridge{T},
    vi::MOI.VariableIndex,
    i::MOIB.IndexInVector,
) where {T}
    func = MOIU.operate(-, T, MOI.SingleVariable(vi))
    return (bridge.flipped_variables[i.value] => func,)
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:FlipSignBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::FlipSignBridge,
    value,
    i::MOIB.IndexInVector,
)
    MOI.set(model, attr, bridge.flipped_variables[i.value], -value)
    return
end

"""
    NonposToNonnegBridge{T} <:
        FlipSignBridge{T, MOI.Nonpositives, MOI.Nonnegatives}

Transforms constrained variables in `Nonpositives` into constrained variables in
`Nonnegatives`.
"""
struct NonposToNonnegBridge{T} <:
       FlipSignBridge{T,MOI.Nonpositives,MOI.Nonnegatives}
    flipped_variables::Vector{MOI.VariableIndex}
    flipped_constraint::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    }
end

function bridge_constrained_variable(
    ::Type{NonposToNonnegBridge{T}},
    model::MOI.ModelLike,
    set::MOI.Nonpositives,
) where {T}
    flipped_variables, flipped_constraint =
        MOI.add_constrained_variables(model, MOI.Nonnegatives(set.dimension))
    return NonposToNonnegBridge{T}(flipped_variables, flipped_constraint)
end
