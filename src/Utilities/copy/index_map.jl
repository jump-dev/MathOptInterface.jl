# IndexMap is defined here because there is a boostrapping problem.
#  * IndexMap requires `Utilities.CleverDicts` and `Utilities.DoubleDicts`, so
#    if it were to be defined in MOI proper, it must be included after
#    Utilities.
#  * However, Utilities requires IndexMap, so it must be defined before
#    Utilities.jl is included.
# To work around this issue, we define `IndexMap` here.

struct IndexMap <: AbstractDict{MOI.Index,MOI.Index}
    var_map::CleverDicts.CleverDict{
        MOI.VariableIndex,
        MOI.VariableIndex,
        typeof(CleverDicts.key_to_index),
        typeof(CleverDicts.index_to_key),
    }
    con_map::DoubleDicts.IndexDoubleDict
end

"""
    IndexMap()

The dictionary-like object returned by [`MathOptInterface.copy_to`](@ref).
"""
function IndexMap()
    var_map = CleverDicts.CleverDict{MOI.VariableIndex,MOI.VariableIndex}()
    con_map = DoubleDicts.IndexDoubleDict()
    return IndexMap(var_map, con_map)
end

function _identity_constraints_map(
    model,
    map::MOIU.DoubleDicts.IndexDoubleDictInner{F,S},
) where {F,S}
    for c in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        map[c] = c
    end
    return
end

"""
    identity_index_map(model::MOI.ModelLike)

Return an [`IndexMap`](@ref) that maps all variable and constraint indices of
`model` to themselves.
"""
function identity_index_map(model::MOI.ModelLike)
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    map = IndexMap()
    for x in variables
        map[x] = x
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        _identity_constraints_map(model, map.con_map[F, S])
    end
    return map
end

Base.getindex(map::IndexMap, key::MOI.VariableIndex) = map.var_map[key]

function Base.getindex(map::IndexMap, key::MOI.ConstraintIndex{F,S}) where {F,S}
    return map.con_map[key]::MOI.ConstraintIndex{F,S}
end

function Base.setindex!(
    map::IndexMap,
    value::MOI.VariableIndex,
    key::MOI.VariableIndex,
)
    return map.var_map[key] = value
end

function Base.setindex!(
    map::IndexMap,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return map.con_map[key] = value
end

Base.delete!(map::IndexMap, x::MOI.VariableIndex) = delete!(map.var_map, x)

Base.delete!(map::IndexMap, c::MOI.ConstraintIndex) = delete!(map.con_map, c)

Base.haskey(map::IndexMap, c::MOI.ConstraintIndex) = haskey(map.con_map, c)

Base.haskey(map::IndexMap, x::MOI.VariableIndex) = haskey(map.var_map, x)

function Base.keys(map::IndexMap)
    return Iterators.flatten((keys(map.var_map), keys(map.con_map)))
end

Base.length(map::IndexMap) = length(map.var_map) + length(map.con_map)

function Base.iterate(map::IndexMap, args...)
    return iterate(Base.Iterators.flatten((map.var_map, map.con_map)), args...)
end
