"""
    FlipSignBridge{T, S1, S2}

Bridge constrained variables in `S1` into constrained variables in `S2` by
multiplying the variables by `-1` and taking the point reflection of the set
across the origin. The flipped `MOI.VectorOfVariables`-in-`S` constraint is
stored in the `flipped_constraint` field by convention.
"""
abstract type FlipSignBridge{T,S1<:MOI.AbstractSet,S2<:MOI.AbstractSet} <:
              SetMapBridge{T,S2,S1} end

"""
    NonposToNonnegBridge{T} <:
        FlipSignBridge{T, MOI.Nonpositives, MOI.Nonnegatives}

Transforms constrained variables in `Nonpositives` into constrained variables in
`Nonnegatives`.
"""
struct NonposToNonnegBridge{T} <:
       FlipSignBridge{T,MOI.Nonpositives,MOI.Nonnegatives}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::NonposToNonnegBridge,
    i::MOIB.IndexInVector,
)
    MOI.delete(model, bridge.variables[i.value])
    deleteat!(bridge.variables, i.value)
    return
end
